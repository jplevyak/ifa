# CODEGEN_C — C Code Generation

A working reference for `ifa/codegen/cg.cc` — the default C backend.
After IFA + cloning + DCE + inlining, this walks the post-analysis IF1
state and emits a single `.c` file plus invokes the system C compiler.

Sister docs: [IR.md](IR.md) (the Sym/Var/PNode/Fun forms it consumes),
[OPTIMIZE.md](OPTIMIZE.md) (the `live` bits it respects),
[PRIMITIVES.md](PRIMITIVES.md) (the per-primitive emission paths),
[CFG_SSU.md](CFG_SSU.md) (the phi/phy nodes it inlines as MOVEs),
[CODEGEN_LLVM.md](CODEGEN_LLVM.md) when written (the alternative backend).

---

## 1. In one paragraph

`c_codegen_write_c(fa, main, filename)` opens `<filename>.c`, calls the
frontend's `c_codegen_pre_file` hook (which for pyc emits
`#include "pyc_c_runtime.h"` and accumulated `__pyc_insert_c_code__`
blobs), then emits type declarations, global declarations, function
bodies (one C function per live `Fun`), and finally a tiny `main()`
that calls `MEM_INIT()` and the analyzed program's entry closure.
Code generation is mostly a per-`PNode` switch on `Code_kind` /
`Prim::index`, recursively walking `cfg_succ` from each Fun's entry.
Branch elimination, phi/phy materialisation as MOVEs, label
emission, and the `extra_goto` synthesis happen during this walk.

The compiler-driver step (`c_codegen_compile`) shells out to
`make -f $system_dir/Makefile.cg CG_TARGET=...` which invokes the
system C compiler.

---

## 2. The public API (`cg.h`)

```c
void c_codegen_print_c(FILE *fp, FA *fa, Fun *main);   // emit to FILE*
void c_codegen_write_c(FA *fa, Fun *main, cchar *filename);  // open <filename>.c and emit
int  c_codegen_compile(cchar *filename);               // shell out to system cc
```

`pyc.cc:compile` calls `c_codegen_write_c` then `c_codegen_compile`.

---

## 3. Top-level emission (`c_codegen_print_c`, `cg.cc:914`)

In order:

```c
void c_codegen_print_c(FILE *fp, FA *fa, Fun *init) {
  Vec<Var *> globals;
  int index = 0;

  // 1. Frontend prologue (default: include c_runtime.h).
  if (!if1->callback->c_codegen_pre_file(fp))
    fprintf(fp, "#include \"c_runtime.h\"\n\n");

  // 2. Type declarations.
  if (build_type_strings(fp, fa, globals) < 0)
    fail("unable to generate C code: no unique typing");

  // 3. Global variable declarations.
  for (Var *v : globals) { ... assign v->cg_string, emit "T t /* name */ gN;" ... }

  // 4. Function bodies. init last.
  for (Fun *f : fa->funs)
    if (f != init && !f->is_external)
      write_c(fp, fa, f);
  write_c(fp, fa, init, &globals);  // init is special: receives globals to initialize

  // 5. C main() that calls init.
  fprintf(fp, "int main(...) { MEM_INIT(); %s(); return 0; }\n", init->cg_string);
}
```

### 3.1 Frontend prologue

`if1->callback->c_codegen_pre_file(fp)` — when true, the frontend has
emitted its own prologue (typically `#include`s) and `cg.cc` skips the
default `#include "c_runtime.h"`. pyc's `c_codegen_pre_file`
(`python_ifa_sym.cc:241`) emits every `__pyc_insert_c_code__` /
`__pyc_include_c_header__` blob accumulated during `build_if1`
(stored in `ctx.c_code`), then returns true. See
[PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md) §8.

### 3.2 Type declarations (`build_type_strings`, `cg.cc:783`)

Walks `if1->allsyms`, assigns each type Sym a unique `cg_string`,
emits forward declarations and full definitions for `Type_RECORD`,
`Type_FUN`, and the special-case `_CG_TUPLE_TO_LIST_FUN` macro for
homogeneous-tuple→list converters.

Returns -1 if a Sym can't be uniquely typed (post-clone, every type
should have one concrete representation; if not, codegen fails).

### 3.3 Global declarations

For each global Var:
- If it's an `is_fun`, reuse `v->sym->fun->cg_string`.
- If `nil`, `"NULL"`.
- If a numeric immediate, format via `sprint_imm`.
- If `constant` string, wrap in `_CG_String(...)`.
- If `is_symbol`, emit `_CG_Symbol(id, "name")`.
- If `is_fun`, skip (already handled).
- If a record type, allocate a fresh `gN` identifier and emit a typed
  declaration.

### 3.4 Function bodies

`write_c(fp, fa, f)` per live Fun. The init function (`__main__`)
gets the `globals` vector so it can emit the per-global initialisers
that bind function pointers to their definitions:

```c
g7 = my_func;   // for each global function pointer
```

### 3.5 The C `main`

A 3-line wrapper that calls `MEM_INIT()` (Boehm GC init) and the
init closure (typically `pyc_init` for pyc, `init_main` for V).

---

## 4. Per-Fun emission (`write_c`, `cg.cc:731`)

```c
static void write_c(FILE *fp, FA *fa, Fun *f, Vec<Var *> *globals = 0) {
  if (!f->live) return;
  write_c_fun_proto(fp, f);            // C-style prototype
  if (!f->entry) { fputs(" { }\n", fp); return; }
  fputs(" {\n", fp);

  // Local declarations: collect all Vars, assign cg_strings, group by type.
  ...assigning v->cg_string for live non-internal Vars...
  emit "  T t0, t1, t2;\n  U t3, t4;\n  ...\n"

  // Initialize globals (init only).
  if (globals) for (Var *v : *globals) if (...) fprintf(fp, "  %s = %s;\n", ...);

  // Initialize arguments.
  write_c_args(fp, f);

  // Walk PNodes from entry.
  rebuild_cfg_pred_index(f);
  Vec<PNode *> done;  done.set_add(f->entry);
  write_c_pnode(fp, fa, f, f->entry, done);
  fputs("}\n", fp);
}
```

### 4.1 Prototype (`write_c_fun_proto`, `cg.cc:28`)

Emits e.g.:

```c
T7 my_func(T3 t0, T4 t1, T5 t2)
```

Walks `Fun::positional_arg_positions`, looking up each formal Var via
`Fun::args[pos]`, skipping dead ones. The first arg is typically the
function-position Sym; subsequent args are the actual parameters.

The `type = 1` overload emits a function-pointer typedef-style form
(used by `_CG_FUN_TYPE` emission in the type declarations).

### 4.2 Local variable declarations (`cg.cc:740`)

For each Var collected by `collect_Vars(... FUN_COLLECT_VARS_NO_TVALS)`:
- Reset `cg_string = 0` for `is_local` and `is_fake` Vars.
- For each non-internal, non-fake, live Var without a `cg_string`,
  assign `t<index++>` and add to `defs`.

`defs` is then sorted by type id (so Vars of the same type are
declared together) and emitted as comma-separated lists per type.

This generates e.g.:

```c
  T3 t0, t1, t2;
  T7 t3;
```

### 4.3 Argument initialisation (`write_c_args`, `cg.cc:713`)

For each formal, emit `arg = <position>;` so the caller's positional
argument lands in the local Var the body expects. Use of MPositions
allows nested-pattern unpacking to be handled by `write_arg_position`.

---

## 5. PNode walk (`write_c_pnode`, `cg.cc:636`)

Recursive DFS over `cfg_succ`. `done` tracks visited PNodes; each is
visited at most once (its successors are deferred to subsequent
recursive calls).

Algorithm sketch:

```c
write_c_pnode(p):
  if (p->live && p->fa_live) switch (p->code->kind) {
    case Code_LABEL: emit "L<id>:;"
    case Code_MOVE:  emit "lhs = rhs;"
    case Code_SEND:
      if (prim) try write_c_prim   (returns 1 if handled)
      else write_send              (regular call)
    case Code_IF / Code_GOTO: handled below
  }

  switch (p->code->kind) {
    case Code_IF:
      if (rval is true_type)  do_phi/phy + recurse succ[0]
      else if (rval is false) do_phi/phy + recurse succ[1]
      else emit "if (cond) {...} else {...}" with both branches
    case Code_GOTO: do_phi/phy + emit "goto L<id>;"
    case Code_SEND with dead reply: emit "return 0;"
    default: do_phi_nodes for succ
  }

  extra_goto = single-succ, non-GOTO PNode (sets fallthrough behavior)
  for each succ in cfg_succ:
    if not visited: recurse  (clears extra_goto)
  if extra_goto && succ is a live LABEL: emit "goto L<id>;"
```

### 5.1 Phi/phy materialisation

`do_phi_nodes(fp, n, isucc)` emits MOVEs for the `cfg_succ[isucc]`'s
`phi` list, using the per-pred `cfg_pred_index` to pick the right
rval slot.

`do_phy_nodes(fp, n, isucc)` emits MOVEs for the current PNode's
`phy` list, using the successor index to pick the right lval slot.

These are emitted *before* the successor recursion so phi/phy
semantics line up with SSU.

### 5.2 The `extra_goto` invariant

A previous bug (project memory: "Critical IFA Code Generator Bug
Fixed") was that LABEL PNodes with one already-`done` cfg_succ needed
an explicit goto emitted. The fix:

```c
int extra_goto = n->cfg_succ.n == 1 && n->code->kind != Code_GOTO;
for (PNode *p : n->cfg_succ) if (done.set_add(p)) {
  write_c_pnode(fp, fa, f, p, done);
  extra_goto = 0;
}
if (extra_goto && n->cfg_succ[0]->live && n->cfg_succ[0]->fa_live) {
  if (n->cfg_succ[0]->code->kind == Code_LABEL)
    fprintf(fp, "  goto L%d;\n", n->cfg_succ[0]->code->label[0]->id);
}
```

The original code excluded `Code_LABEL` from the `extra_goto`
condition, breaking for-loop-with-elif cases. The fix here only
excludes `Code_GOTO` (which emits its own goto).

### 5.3 Branch elimination

`Code_IF` with a constant condition (the rval is `true_type` or
`false_type`'s singleton CS) elides the unused branch entirely. The
`else` branch's PNodes are still visited later if they're reachable
from elsewhere; if they're only reachable through this branch, they
stay marked dead and are skipped by the `if (p->live && p->fa_live)`
gate.

### 5.4 Dead PNode behaviour

Dead PNodes (`!p->live || !p->fa_live`) skip the per-kind code
emission but *still* emit phi/phy nodes for live successors (they're
join-point bookkeeping). The `Code_SEND` case has a special branch:
a dead `P_prim_reply` emits `return 0;` so the C compiler doesn't
complain about a non-void function falling off the end.

---

## 6. Send emission

### 6.1 `write_c_prim` (`cg.cc:183`) — table primitives

A 300-line switch on `Prim::index` covering every `P_prim_*`.
Highlights:

- `P_prim_reply` → `return <val>;`
- `P_prim_make` (tuple) → `_CG_prim_tuple(T, n); lhs->eK = ...`
- `P_prim_make` (list) → `_CG_prim_list(elem, n); list[K] = ...`
- `P_prim_period` (getter) → `lhs = (T)((O)rhs)->eK; /* name */`
- `P_prim_period` (closure-create) → `lhs = _CG_prim_closure(T); lhs->eK = ...`
- `P_prim_setter` → `((O)obj)->eK = (T)val;`
- `P_prim_apply` → currently `assert(!"unimplemented");`
- Numeric ops (`P_prim_add`, etc.) → `lhs = _CG_<name>(a, b);` (via the
  default `write_send` fallback when `write_c_prim` returns 0).

Return value:
- `return 1` → handled, skip the default `write_send`.
- `return 0` → fall through to default `write_send` which emits
  `lhs = _CG_<name>(...args...);`

### 6.2 Default `write_send` (`cg.cc:580`)

For non-primitive SENDs:

```c
if (n->prim) {
  // Generic primitive form
  emit "lhs = _CG_<prim_name>(rval2, rval3, ...);"
} else {
  Fun *target = get_target_fun(n, f);
  if (target) {
    emit "lhs = target_cg_string(arg0, arg1, ...);"
    // args from MPosition iteration with write_send_arg
  } else {
    emit "assert(!\"runtime error: matching function not found\");"
  }
}
```

`get_target_fun(n, f)` (`cg.cc:485`) looks up `f->calls.get(n)` and
returns the *single* call target if there's exactly one; otherwise
NULL. The post-cloning + post-DCE state should have monomorphised
most call sites; failures here usually mean a polymorphic site
survived clone (which is also a problem for the analysis correctness).

### 6.3 Argument emission (`write_send_arg`, `cg.cc:550`)

Walks the `target->positional_arg_positions`, looks up the actual at
each position via the call's rvals, emits with comma separation
between live args.

### 6.4 Closure handling

A SEND emitted with a closure-typed receiver (Sym whose `is_fun &&
type_kind == Type_FUN`) is a closure call. `is_closure_var(v)`
(`cg.cc:534`) checks this. The `simple_inlining` pass tries to
collapse closure-create+closure-call pairs into direct calls, so
runtime closures should be rare.

---

## 7. Helper functions

### 7.1 `c_type(v)` / `c_type(s)` (`cg.cc:18,23`)

Returns the Sym's `cg_string` if available, otherwise `_CG_void`.
Used everywhere a C type name is needed.

### 7.2 `c_rhs(v)` (`cg.cc:159`)

Returns the C-string form of a Var suitable for the right-hand side
of an assignment. Handles:
- Numeric immediates → format via `sprint_imm`.
- Strings → `_CG_String(...)`.
- Otherwise → `v->cg_string`.

### 7.3 `num_string(s)` (`cg.cc:106`)

Returns a C literal for a numeric constant Sym, including the right
suffix (`L`, `LL`, `U`, etc.) for the type.

### 7.4 `simple_move(fp, lhs, rhs)` (`cg.cc:494`)

Emits a single MOVE: `lhs = (T)rhs;` with the right cast if types
differ. Skipped if `lhs == rhs` or if the lhs is dead.

### 7.5 `destruct_prim(fp, l, r)` (`cg.cc:172`)

Special-case codegen for `P_prim_destruct` — splices a tuple/record
into multiple destination Vars.

---

## 8. Type strings (`build_type_strings`, `cg.cc:783`)

A 130-line function that walks `if1->allsyms` and assigns `cg_string`
to every type Sym. The naming convention:
- Numeric primitives → `_CG_int32`, `_CG_uint64`, etc.
- Records → `Tn` where `n` is the Sym's id.
- Pointers → `Tn *`.
- Closures → `Cn`.
- Functions (pointer-to-function types) → emitted via `_CG_FUN_TYPE(n,
  ret, args...)`.

The function also collects all live globals into the `globals` output
vector for later initialisation.

Emission order:
1. Forward declarations: `struct Tn;` for every record.
2. `typedef struct Tn *Tn;` for pointers.
3. Function pointer typedefs.
4. Full struct definitions: `struct Tn { T0 e0; T1 e1; ... };`.
5. `_CG_TUPLE_TO_LIST_FUN(id, n)` for tuples that get used as lists.

`homogeneous_tuple(s)` (`cg.cc:776`) — true if every field of a
record type has the same type. Triggers tuple-to-list conversion.

---

## 9. Compile driver (`c_codegen_compile`, `cg.cc:985`)

```c
int c_codegen_compile(cchar *filename) {
  char target[512], s[1024];
  strcpy(target, filename);
  *strrchr(target, '.') = 0;
  snprintf(s, sizeof(s),
           "make --no-print-directory -f %s/Makefile.cg "
           "CG_ROOT=%s CG_TARGET=%s CG_FILES=%s.c %s %s",
           system_dir, system_dir, target, filename,
           codegen_optimize ? "OPTIMIZE=1" : "",
           codegen_debug ? "DEBUG=1" : "");
  return system(s);
}
```

`Makefile.cg` (root: `/Makefile.cg`) drives the system C compiler
(`cc` by default) with the right include paths, optimization flags,
and links against `pyc_c_runtime`. `system_dir` is the install location
(controlled by `IFA_SYSTEM_DIRECTORY` / `PYC_SYSTEM_DIRECTORY`).

The `target` is `filename` with the extension stripped, so
`hello_world.py.c` produces a `hello_world` binary.

`codegen_optimize` / `codegen_debug` are global CLI flags (`-O` / `-g`
in `pyc.cc`).

---

## 10. Runtime conventions

Every emitted call into the runtime is prefixed `_CG_`. The runtime
header (pyc's `pyc_c_runtime.h`, V's `c_runtime.h`) defines:

- Type aliases: `_CG_int8`, `_CG_uint32`, `_CG_float64`,
  `_CG_String`, `_CG_list`, `_CG_void`, `_CG_Symbol`.
- Primitive operations: `_CG_add(a, b)`, `_CG_subtract(a, b)`,
  `_CG_period(o, sel)`, etc. — one per `Prim::name` for primitives
  that don't have inline codegen.
- Allocation: `_CG_prim_tuple(T, n)`, `_CG_prim_list(T, n)`,
  `_CG_prim_closure(T)`, `_CG_prim_make(T)`.
- Init: `MEM_INIT()` (Boehm GC).
- Format helpers: `_CG_format_string(...)`, `_CG_write(...)`,
  `_CG_writeln()`.

When you add a new primitive whose codegen falls into the default
`write_send`, you also need a `_CG_<name>` definition in the runtime
header. The pyc-specific helpers (`_CG_format_string`, `_CG_write`,
etc.) are emitted by the frontend's registered codegen callbacks
(see [PRIMITIVES.md](PRIMITIVES.md) §7), not by `cg.cc`'s switch.

---

## 11. Gotchas

### 11.1 `cg_string` must be set for everything live
A live Var with `cg_string == nullptr` will cause a `nullptr`
fprintf and crash. `write_c`'s declaration pass assigns `cg_string`
for live unnamed Vars, but if a downstream change introduces a new
live Var without going through that path, you get a SIGSEGV at
codegen time.

### 11.2 `fa_live` ≠ `live`
`Var::live` and `PNode::live` are post-DCE flags. `fa_live` is set
during analysis to indicate the PNode was reached. The `write_c_pnode`
gate is `if (p->live && p->fa_live)` — both must be true. A PNode
that's `live` (DCE-survived) but not `fa_live` (analysis never
reached) is treated as dead for emission. This is rare but possible
on edges of polymorphic dispatch.

### 11.3 Single-target call assumption
`write_send`'s non-primitive path uses `get_target_fun(n, f)` which
returns NULL for polymorphic call sites (>1 callee). The emitted
fallback is `assert(!"runtime error: matching function not found");`,
which means a polymorphic call at runtime would abort. Post-clone +
post-DCE, this shouldn't happen — but if you see this assertion at
runtime, cloning didn't fully monomorphise.

### 11.4 The `Makefile.cg` is hard-coded to `cc` (mostly)
`Makefile.cg` uses `CC ?= cc` style defaults. Override via environment.
The path is `$system_dir/Makefile.cg`, so installing to a non-standard
location requires `IFA_SYSTEM_DIRECTORY` / `PYC_SYSTEM_DIRECTORY`.

### 11.5 Phi/phy ordering relative to branch
`do_phy_nodes` runs *before* the successor recursion; `do_phi_nodes`
runs immediately *before* the goto/recurse to the successor. This is
how the phy "logically precedes" and phi "logically follows" semantics
map to sequential C: phy = "do these MOVEs before leaving the current
node," phi = "do these MOVEs as you arrive at the successor."

### 11.6 `rebuild_cfg_pred_index` is needed
`write_c` calls `rebuild_cfg_pred_index(f)` before walking PNodes.
`cfg_pred_index` is a per-PNode `Map<PNode*, int>` that lets phi
lookups find the right rval slot. The inliner and other passes can
invalidate it, so cg refreshes.

### 11.7 The `init` function gets the globals vector
Only when `write_c` is called with `globals != nullptr` does it emit
the `g7 = my_func;` initialisers. That's why `c_codegen_print_c`
emits non-init Funs first (without globals) and init last (with
globals).

### 11.8 `P_prim_apply` is unimplemented
`write_c_prim` for `P_prim_apply` is `assert(!"unimplemented")`.
Programs using continuation-passing-style apply (V has this; pyc
doesn't emit it) will fail. If you need it, the path is mostly
written but commented; it generates an indirect call through a
closure.

### 11.9 `MEM_INIT` is Boehm-GC-specific
The emitted `main` calls `MEM_INIT()`. This is a Boehm GC macro
(`GC_INIT`-equivalent). If you swap allocators, redefine the macro
in the runtime header.

### 11.10 No buffering control in `cg.cc`
`fopen` is followed by many `fputs` / `fprintf` calls. Writes are
line-buffered to a file (or unbuffered if `fp` is a pipe). If you
add custom emission via `system()` calls or external processes,
ensure `fflush` before `close`.

### 11.11 `escape_string` (used at `cg.cc:937`) is a separate helper
Defined in `common/misc.cc`. Properly escapes C string literals
(backslashes, quotes, control chars). If you handle constant strings
elsewhere, call this function to avoid producing invalid C.

### 11.12 `Type_SUM` inside a `P_prim_period`
The getter case (`P_prim_period`) handles `Type_SUM` receivers by
taking the first member (`obj = obj->has[0]`). This is correct only
if every member of the sum has the field at the same offset (which
clone's layout pass should ensure via `prim_period_offset`). If it
doesn't, the wrong field is read at runtime.

### 11.13 Tuple-to-list specialisation
Homogeneous tuples get a `_CG_TUPLE_TO_LIST_FUN(id, n)` macro
emission so they can be passed to functions expecting lists.
`clone.cc:tuple_able` marks the candidates; if your homogeneous tuple
isn't tuple_able, you get a build error in the runtime when something
tries to convert.

### 11.14 `is_external` Funs are skipped
`c_codegen_print_c`'s `if (f != init && !f->is_external)` filter
skips externally-implemented functions. Their declarations come from
the runtime header. Don't add bodies to those.

---

## 12. Symptom → start-here

| Symptom | Start here |
|---|---|
| "missing C type for var" | `build_type_strings` — verify the Sym got assigned a `cg_string` |
| "function emitted with empty body" | `f->entry == nullptr` (empty function); check the frontend produced a `Code` body |
| "runtime: matching function not found" | `get_target_fun` returned null → polymorphic call survived clone |
| "wrong field accessed" | `P_prim_period` switch — receivers in a Type_SUM share offsets via clone's layout pass |
| "compile-time C error 'undefined identifier'" | A Var's `cg_string` collided with a C keyword, or `escape_string` failed on a constant |
| "wrong goto target" | `write_c_pnode` `extra_goto` logic — see project memory bug fix |
| "for-loop elif crash" | The classic `extra_goto` bug; verify `cfg_succ[0]->code->kind == Code_LABEL` check is present |
| "MEM_INIT undefined" | Runtime header not included; check `c_codegen_pre_file` returned the right value |
| "wrong arg type in call" | `write_send_arg` — formal vs actual type mismatch (clone should have rewritten) |
| "missing local declaration" | `write_c`'s declaration pass skipped a Var (`is_internal`, `is_fake`, or `!live`) |
| "duplicate label" | `Code_LABEL` Codes share an id; check `if1_alloc_label` was used for both |
| "init globals not initialized" | `write_c(fp, fa, init, &globals)` not called for init; see top-level emission |

---

## 13. References

- `ifa/codegen/cg.cc` + `cg.h` — implementation + public API.
- `ifa/Makefile.cg` (in the install dir) — driver makefile.
- `pyc_c_runtime.h` / `pyc_c_runtime_plib.h` (pyc) — runtime
  definitions referenced by `_CG_*` macros.
- `ifa/common/c_runtime.h` (V) — V-language runtime.
- Sister docs: [IR.md](IR.md) (the IR being emitted),
  [OPTIMIZE.md](OPTIMIZE.md) (the `live`/`fa_live` flags consumed),
  [PRIMITIVES.md](PRIMITIVES.md) (registered cgfns),
  [CFG_SSU.md](CFG_SSU.md) (phi/phy → MOVE),
  [PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md) §8 (`c_codegen_pre_file`).
