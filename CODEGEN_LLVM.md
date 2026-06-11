# CODEGEN_LLVM — LLVM Backend

A working reference for `ifa/codegen/{llvm,llvm_codegen,llvm_primitives}.cc`
plus `llvm_internal.h`. This is the experimental LLVM IR backend,
selected by `-b` / `IFA_LLVM=1` / `PYC_LLVM=1`. The default backend is
the C emitter in [CODEGEN_C.md](CODEGEN_C.md).

Sister docs: [CODEGEN_C.md](CODEGEN_C.md) (default backend with the
same overall structure), [IR.md](IR.md), [PRIMITIVES.md](PRIMITIVES.md),
[OPTIMIZE.md](OPTIMIZE.md) (live bits the backend consumes).

---

## 1. In one paragraph

The LLVM backend mirrors the C backend's structure but emits LLVM IR
instead of C source. `llvm_codegen_write_ir(fa, main, filename)`
allocates a process-wide LLVM `Module` / `Builder` / `DIBuilder` (with
explicit reverse-order tear-down on re-entry, see §3 and §14.2), maps
IF1 `Sym`s to LLVM `Type`s (`getLLVMType` and its phase-4 helpers
`mapBuiltinOrNumeric` / `mapVectorType` / `mapStructType` /
`mapFunctionType` / `mapSumType` / `mapByTypeKind`), creates LLVM
`Function` declarations for every live `Fun` (`createFunction`, ~30
LOC after phase-4 decomposition into helper functions), then
translates each function body (`translateFunctionBody`, ~20 LOC after
phase-4 decomposition into a CFG-walk driver + per-Code_kind
helpers) by walking PNodes. Each PNode becomes a `BasicBlock` (or
part of one) terminated by a branch/ret. Phi/phy MOVEs are materialised
the same way as in the C backend, but using LLVM IR `phi` instructions
or simple stores. `llvm_codegen_compile` spawns
`clang -c -fPIC <file>.ll -o <file>.o` then
`clang <file>.o -o <file> -lm` via `posix_spawnp` (see §11). Debug
output goes through the `DEBUG_LOG(…)` macro (off unless built with
the right flag) — production builds are quiet.

Most of the cross-backend code (`c_type`, `num_string`,
`is_closure_var`, `get_target_fun_core`, type-string assignment,
process spawn) lives in `codegen_common.{h,cc}`; the LLVM backend
includes the header and calls those helpers directly.

---

## 2. Public API (`llvm.h`)

```c
void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main, cchar *input_filename);
void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *filename);
int  llvm_codegen_compile(cchar *filename);
```

`pyc.cc:compile` calls these when `codegen_llvm` is set:

```c
if (codegen_llvm) {
  llvm_codegen(pdb->fa, if1->top->fun, fn);     // wrapper for *_write_ir
  if (!codegen_jit && llvm_codegen_compile(fn)) fail("compilation failure");
}
```

`codegen_jit` (when set) skips the compile step — the IR stays in
memory and the (unimplemented) JIT path is supposed to execute it.
That path is currently a no-op shell.

---

## 3. Global state (`llvm_internal.h`)

The backend uses process-wide globals (set up by
`llvm_codegen_initialize`):

```c
extern std::unique_ptr<llvm::LLVMContext> TheContext;
extern std::unique_ptr<llvm::Module>      TheModule;
extern std::unique_ptr<llvm::IRBuilder<>> Builder;
extern std::unique_ptr<llvm::DIBuilder>   DBuilder;
extern llvm::DICompileUnit *CU;
extern llvm::DIFile *UnitFile;
extern Vec<Fun *> *all_funs_global;
extern std::map<Label *, llvm::BasicBlock *> label_to_bb_map;
extern std::map<…> string_constants_map;
extern std::map<Fun *, Vec<…>> reverse_call_graph;
```

`label_to_bb_map` is **per-function-translation**; it's cleared at
the start of each `translateFunctionBody` so labels don't leak
between functions.

`all_funs_global` holds the *transitive call closure* from `main`,
computed by `discover_all_reachable_functions` (`llvm.cc:164`). Used
as a fallback target lookup table when `f->calls.get(p)` returns
inconclusive results.

**Reset-on-init (phase 0.1 of CODEGEN_PLAN).**
`llvm_codegen_initialize` (`llvm.cc:45`) tears down all of the above
in reverse-dependency order — `DBuilder.reset()`, `Builder.reset()`,
`TheModule.reset()`, `TheContext.reset()`, then clears
`string_constants_map`, `label_to_bb_map`, and `reverse_call_graph` —
*before* allocating the new instances. Two reasons:
- Reverse order ensures every destructor runs while its dependencies
  are still alive (e.g. destroying `Module` after `Context` would
  dereference freed memory).
- The cleared maps hold pointers into the just-destroyed Context /
  Module; not clearing them would be a use-after-free on the next
  lookup.

This makes re-entrant `llvm_codegen_print_ir` calls safe. Phase 5
of CODEGEN_PLAN added a `Codegen` base class scaffold (in
`codegen_common.h`) so a future migration can move these globals
into a per-run instance; the actual mass-rewrite of access sites is
deferred — see CODEGEN_PLAN §8.

---

## 4. Top-level emission

### 4.1 `llvm_codegen_write_ir` (`llvm.cc:1321`)

Opens `<filename without extension>.ll`, calls
`llvm_codegen_print_ir`, closes.

### 4.2 `llvm_codegen_print_ir` (`llvm.cc:904`)

```c
void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main_fun, cchar *input_filename) {
  llvm_codegen_initialize(fa);          // create context/module/builder/dbuilder
  llvm_build_type_strings(fa);          // assign cg_strings, build LLVM type cache
  // ... DICompileUnit setup ...
  createGlobalVariables(fa);            // emit module-level globals
  discover_all_reachable_functions(fa, main_fun, all_funs);
  build_reverse_call_graph(fa);         // for constant-arg recovery
  all_funs_global = &all_funs;

  // First pass: declarations.
  for (Fun *f : all_funs) if (f->live) createFunction(f, TheModule.get());

  // Second pass: bodies.
  for (Fun *f : all_funs)
    if (f && f->live && f->llvm && !f->is_external && f->entry)
      translateFunctionBody(f);

  // Emit C-style main() that calls the IF1 main_fun.
  ...creates llvm_main, builds entry BB, CreateCall(main_fun->llvm), CreateRet(0)...

  // Finalize debug info.
  if (DBuilder) DBuilder->finalize();

  // Run llvm::verifyModule (warns on issues).
  // Print the module IR to fp.
  TheModule->print(...);
}
```

The two-pass approach (declarations, then bodies) is required because
function-call instructions need their target's `llvm::Function*` to
already exist.

### 4.3 `llvm_codegen_initialize(fa)` (`llvm.cc:45`)

Initializes LLVM target subsystems (`InitializeAllTargetInfos`,
`InitializeAllTargets`, `InitializeAllTargetMCs`,
`InitializeAllAsmParsers`, `InitializeAllAsmPrinters`), creates the
`LLVMContext` / `Module` / `IRBuilder` / `DIBuilder`, sets the target
triple from the host.

---

## 5. Type mapping (`llvm.cc`)

### 5.1 `mapNumericType(sym)` (`llvm.cc:114`)

Maps `Sym::num_kind` + `Sym::num_index` to the corresponding LLVM
primitive type (`getInt8Ty`, `getInt32Ty`, `getDoubleTy`, etc.).

### 5.2 `getLLVMType(sym)` (`llvm.cc:462`)

The general mapper. Caches results on `Sym::llvm_type`. Phase 4 of
CODEGEN_PLAN decomposed the formerly-monolithic implementation
(~230 LOC of nested switches) into a thin dispatcher plus per-shape
helpers:

- `mapBuiltinOrNumeric(sym)` (`llvm.cc:317`) — handles
  `sym_string` → `i8 *`, `sym_symbol` → `i64`, and the numeric path
  (delegating to `mapNumericType`).
- `mapVectorType(sym)` (`llvm.cc:334`) — vector types (struct with
  trailing flexible array of `element` type).
- `mapStructType(sym, unaliased)` (`llvm.cc:353`) — `Type_RECORD`
  layout. Allocates an opaque named StructType *first*, caches it on
  `llvm_type`, then sets the body — this is what breaks the
  infinite recursion for self-referential record types. Void or null
  field types get substituted with `i8` placeholders to keep
  LLVM's `StructLayout` happy (see Phase 3 fix history).
- `mapFunctionType(sym)` (`llvm.cc:381`) — `Type_FUN`. Returns an
  opaque pointer (`ptr`); under LLVM 15+ opaque-pointer mode every
  function value is just `ptr` regardless of signature. The actual
  signature lives on `Fun::llvm`'s `FunctionType` and is consulted at
  call sites via `CreateCall(target->llvm, args)`. `ret` and `has`
  are walked best-effort to warm the type cache for debug info, but
  lazily-populated slots are no longer fatal.
- `mapSumType(sym)` (`llvm.cc:412`) — `Type_SUM` "T | nil" collapses
  to the underlying non-nil type's pointer.
- `mapByTypeKind(sym, unaliased)` (`llvm.cc:429`) — the per-`Type_*`
  dispatch.

`getLLVMType` itself is now ~50 LOC: null check, function-symbol
recovery, cache lookup, `mapBuiltinOrNumeric` short-circuit,
`mapByTypeKind` dispatch, cache store. Function symbols
(`is_fun && type_kind == 0`) follow a three-step resolution:
recurse through `sym->type` if usable; else fall back to opaque
pointer if `sym->fun` is set (the analog of the C backend's
`s->fun->cg_structural_string` resolution); else fail. This is what
keeps method dispatch (`__iter__`, `__init__`, `__pyc_more__`, etc.)
from crashing — the symbol's Type_FUN slot is often lazily populated,
but `sym->fun` is set by the IFA analyzer and that's enough under
opaque pointers.

### 5.3 `getLLVMDIType(sym, di_file)` (`llvm.cc:517`)

Parallel mapper for debug-info types. Cached on
`Sym::llvm_type_di_cache`. Emits `DI*Type` records that show up in
the resulting binary's DWARF.

### 5.4 `llvm_build_type_strings(fa)` (`llvm.cc:887`)

Pre-pass that walks all live Syms and assigns names — both the
C-side `cg_string` (matching the C backend, for runtime helpers) and
the LLVM-side StructType names. Run before any actual translation.

---

## 6. Value mapping

### 6.1 `getLLVMValue(var, ifa_fun)` (`llvm.cc:1057`)

Returns the LLVM `Value*` for a Var in the context of `ifa_fun`.
- If `var->llvm_value` is set, return it (cached).
- If `var->sym->is_constant`, call `getLLVMConstant`.
- If `var` is a formal arg, look up in `ifa_fun->llvm`'s arg list.
- If `var` is global, look up via `TheModule->getGlobalVariable`.
- Otherwise, allocate an `alloca` in the function's entry block
  (Vars are stored in stack slots for the simple case; SSA-like
  passing happens implicitly via the LLVM verifier promoting allocas
  to registers).

### 6.2 `setLLVMValue(var, val, ifa_fun)` (`llvm.cc:1298`)

Stores `val` to `var`'s LLVM location. For SSA-style Vars, just
caches `var->llvm_value = val`; for memory-backed Vars, emits a
`store` instruction.

### 6.3 `getLLVMConstant(var)` (`llvm.cc:644`)

Builds an LLVM `Constant*` from an immediate Sym:
- Numeric → `ConstantInt::get` / `ConstantFP::get`.
- String → calls `getOrCreateLLVMStringConstant` which interns
  string globals.
- Symbol → constant integer (the symbol's id).

### 6.4 `createGlobalVariables(fa)` (`llvm.cc:766`)

Walks `if1->allsyms` for globals (`!is_local`, `!is_fun`,
non-constant), emits `llvm::GlobalVariable` declarations with the
appropriate initial values.

---

## 7. Function declaration (`createFunction`, `llvm_codegen.cc:204`)

Phase 4 of CODEGEN_PLAN decomposed this from ~290 LOC into a ~30-LOC
driver plus per-job helpers above it in the file. The driver:

1. Check `f->llvm` cache; if set, return it.
2. Compute return / argument LLVM types via the helpers
   (`compute_return_type`, `compute_arg_types`).
3. Build the `FunctionType`, call `Function::Create(...)` with
   `ExternalLinkage` and the Fun's `cg_string` as the symbol name.
4. Cache `f->llvm = llvm_func`.
5. Name arguments for IR readability (helper).
6. Optionally build the `DISubprogram` debug-info entry (helper at
   `llvm_codegen.cc:180`).
7. For external functions, return the declaration here — no entry
   block. Otherwise, emit the entry `BasicBlock` and stack-allocate
   each formal arg.

The helpers live above `createFunction` so the driver reads
top-down: each step is a named function call instead of an inline
block, which makes the per-Fun control flow easy to follow and the
"what does each phase do" mapping legible at a glance.

External functions (`f->is_external`) get declarations only; their
bodies come from the runtime library at link time.

---

## 8. Function body translation (`translateFunctionBody`,
`llvm_codegen.cc:382`)

Phase 4 of CODEGEN_PLAN decomposed this from ~250 LOC into a
~20-LOC driver that calls a sequence of named helpers — entry-BB
setup, var alloca, arg translation, CFG-walk worklist. The
worklist invokes `translatePNode` (see §8.1) per PNode and adds
successors as it goes. After the walk, untermimated blocks get
inserted-unreachable/ret terminators and `verifyFunction` runs.

### 8.1 `translatePNode` (`llvm_codegen.cc:545`)

Phase 4 of CODEGEN_PLAN decomposed the formerly ~200-LOC switch into
a thin dispatcher that defers each `Code_kind` to a helper:

- `translate_code_label` (`llvm_codegen.cc:404`) — emits/gets the
  BB and sets the insert point.
- `translate_code_move` (`llvm_codegen.cc:436`) — phi-precondition
  + `simple_move`.
- `translate_code_send` (`llvm_codegen.cc:499`) — primitive
  dispatch (`write_llvm_prim` → `write_send` fallback).
- `translate_code_if` (`llvm_codegen.cc:446`) — constant-branch
  elimination and `CreateCondBr`.
- `translate_code_goto` (`llvm_codegen.cc:420`) — phi nodes + `CreateBr`.

Same control flow as the C backend's `write_c_pnode`, but emitting
LLVM IR via `Builder->Create*`. The dispatcher (`translatePNode`)
itself is now ~10 LOC: null-check the code, dispatch by kind, done.

### 8.2 `getLLVMBasicBlock(label, current_llvm_fun)` (`llvm_codegen.cc:44`)

Look up or create the LLVM `BasicBlock*` for an IF1 `Label`. Caches
in both `label_to_bb_map` and `label->bb` (the union field —
`Label::code` overlaps with `Label::bb`).

### 8.3 phi/phy emission

`do_phi_nodes(n, isucc, ifa_fun)` and `do_phy_nodes(n, isucc,
ifa_fun)` — same semantics as the C backend, but using
`simple_move` which emits LLVM `store`/`load`/`select` rather than C
assignment. LLVM's own SSA optimisations + verifier promote the
allocas to registers / phi instructions later.

### 8.4 `simple_move(lhs, rhs, ifa_fun)` (`llvm_codegen.cc:507`)

```c
void simple_move(Var *lhs, Var *rhs, Fun *ifa_fun) {
  if (lhs == rhs || !lhs->live) return;
  llvm::Value *rhs_val = getLLVMValue(rhs, ifa_fun);
  setLLVMValue(lhs, rhs_val, ifa_fun);
}
```

Trivial when both Vars use SSA-style caching; for memory-backed ones,
emits a `load`/`store`.

---

## 9. Send emission

### 9.1 `write_llvm_prim` (`llvm_primitives.cc:181`)

A switch on `Prim::index` — parallel to the C backend's
`write_c_prim`. After phase 3 of CODEGEN_PLAN, the coverage is:

| Primitive | Emission |
|---|---|
| `P_prim_operator` / numeric ops | `CreateAdd` / `CreateFAdd` / `CreateSDiv` / `CreateFDiv` / `CreateMul` / `CreateMod` etc., with int/float dispatch from operand type |
| `P_prim_add` / `subtract` / `mult` / `div` / `mod` | as above; type-dispatched |
| `P_prim_less` / `lessorequal` / `greater` / `greaterorequal` / `equal` / `notequal` | `CreateICmp*` / `CreateFCmp*` + optional `CreateZExt` to i1 |
| `P_prim_and` / `or` / `xor` | `CreateAnd` / `CreateOr` / `CreateXor` (bitwise; logical-short-circuit is lowered earlier) |
| `P_prim_make` | tuple/list/record allocation via runtime call (`GC_malloc` + `CreateStore` initializers) |
| `P_prim_new` | runtime allocation; phase 3 added the spill-to-alloca pattern for the resulting pointer |
| `P_prim_clone` / `clone_vector` | structural copy via GC_malloc + memcpy |
| `P_prim_assign` | `CreateStore` (typed) |
| `P_prim_sizeof` / `sizeof_element` | LLVM `DataLayout::getTypeAllocSize` constant |
| `P_prim_index_object` / `set_index_object` | `CreateGEP` + `CreateLoad` / `CreateStore` |
| `P_prim_destruct` | per-field `CreateGEP` + `CreateLoad` per lval |
| `P_prim_len` | runtime-call lowered or GEP-to-length-field |
| `P_prim_period` (getter) | `CreateGEP` + `CreateLoad`; spill-to-alloca for AllocaInst/GlobalVariable receivers |
| `P_prim_setter` | `CreateGEP` + `CreateStore` |
| `P_prim_primitive` | metadata-marker, no codegen |
| `P_prim_reply` | `Builder->CreateRet(val)` |

See [PRIMITIVES.md §14](PRIMITIVES.md) for the cross-backend
coverage matrix and per-primitive pinpoint fixtures.

Return value: `1` if handled, `0` to fall through to `write_send`.

### 9.2 `write_send(f, n)` (`llvm_primitives.cc:45`)

For non-primitive SENDs:

```c
void write_send(Fun *f, PNode *n) {
  Fun *target = get_target_fun(n, f);
  if (!target || !target->llvm) {
    // Emit assertion / abort runtime helper.
    return;
  }
  // Build LLVM args from n's rvals through MPosition walk.
  llvm::Value *call = Builder->CreateCall(target->llvm, args);
  // If n->lvals.n, setLLVMValue(n->lvals[0], call, f).
}
```

### 9.3 `get_target_fun(n, f)` (`llvm_primitives.cc:18`) — with fallbacks

Wraps `get_target_fun_core` (now in `codegen_common.cc`) with two
LLVM-specific fallback paths:
1. Call `get_target_fun_core(n, f)` (single target via
   `f->calls.get(n)`); if that returns non-null, use it.
2. If no entry: walk `n->rvals[0]`'s Sym, look it up in
   `all_funs_global` by Sym pointer.
3. Last resort: walk `all_funs_global` looking for any Fun whose
   `sym->name` matches (string compare).

The fallback exists because clone's call-graph rebuild can leave
some sites without a definitive `calls` entry in less-tested
configurations. The C backend treats this as fatal (its
`get_target_fun` wrapper at `cg.cc:438` calls `fail(...)`); the
LLVM backend tries harder.

If all fallbacks fail, returns NULL and the call emits an
abort-runtime call (the equivalent of C's
`assert(!"runtime error: matching function not found");`).

The structural bug — that `discover_all_reachable_functions`
doesn't always populate `all_funs` via the `top->calls` walk — is
filed in CODEGEN_PLAN §8 / AUDIT §1 #6 and deferred to the §5
Codegen migration.

---

## 10. Constant propagation: `recover_constant_arg` (`llvm.cc:245`)

Heuristic for inlining-equivalent constant propagation. When a call
site passes a constant Var to a callee whose formal expects a runtime
value, this helper:
1. Walks `reverse_call_graph` (built by `build_reverse_call_graph`,
   `llvm.cc:228`) to find every call site of the function.
2. If every call site passes the same constant, use that constant
   instead of the formal's load.

This isn't fully wired up in all paths — it's referenced by some
primitive emission code but the main `getLLVMValue` doesn't go
through it. Treat as experimental.

---

## 11. The compile driver (`llvm_codegen_compile`, `llvm.cc:1342`)

```c
int llvm_codegen_compile(cchar *input_filename) {
  // Derive <name>.ll, <name>.o, <name> paths from input_filename.
  // Each snprintf uses FILENAME_MAX and fails on truncation.
  char ll_file[FILENAME_MAX], obj_file[FILENAME_MAX], exe_file[FILENAME_MAX];
  // ... bounded snprintf, fail-on-overflow ...

  // Step 1: clang -c -fPIC <ll> -o <obj>
  {
    char *argv[] = {"clang", "-c", "-fPIC", ll_file, "-o", obj_file, nullptr};
    int res = codegen_spawn("clang", argv);
    if (res != 0) fail("llvm_codegen_compile: clang -c failed for %s (exit=%d)", ll_file, res);
  }

  // Step 2: clang <obj> -o <exe> -lm
  {
    char *argv[] = {"clang", obj_file, "-o", exe_file, "-lm", nullptr};
    int res = codegen_spawn("clang", argv);
    if (res != 0) fail("llvm_codegen_compile: linking failed for %s (exit=%d)", obj_file, res);
  }
  return 0;
}
```

Phase 4 of CODEGEN_PLAN replaced the pre-existing `system(cmd)`
shell invocations with `codegen_spawn(file, argv)` (from
`codegen_common.cc`), which calls `posix_spawnp`. Same benefits as
the C backend's compile path (§9 of CODEGEN_C.md): no shell quoting,
length-checked path buffers, no chance of shell-metachar surprises
in user-supplied filenames.

So the LLVM backend's compile path is:
1. Emit `.ll`.
2. `clang -c -fPIC file.ll -o file.o`.
3. `clang file.o -o file -lm`.

Note: this does NOT link in the pyc runtime (`-lpyc_c_runtime` or
similar). Programs that need runtime helpers (anything using
`_CG_*` calls) will fail to link as-is. Either:
- The LLVM backend currently restricts to pure-numeric programs, or
- A separate `-l` argument needs to be added (and the runtime
  rebuilt as a shared library).

Status: the LLVM backend is a work-in-progress; the C backend is the
reliable production path.

---

## 12. The CLI flags

From `pyc.cc:45`:

```
-b / --llvm    PYC_LLVM=1      Use LLVM backend (USE_LLVM build only)
-j / --jit     PYC_JIT=1       JIT execute (skips disk compile)
```

Conditional on `#ifdef USE_LLVM` which is set by the build system if
LLVM was detected. Without USE_LLVM the flags don't exist.

---

## 13. The stale `LLVM.md`

`ifa/LLVM.md` documents an **IR serialization format** referencing
`ir_serialize.cc` / `ir_deserialize.cc`. **Those files do not exist
in this tree.** The plan in
[`DOCUMENTATION_PLAN.md`](../DOCUMENTATION_PLAN.md) is to either
delete or rewrite `LLVM.md`. Until then, treat it as historical
documentation for a feature that's no longer present.

The current LLVM pipeline:
- IF1 lives in memory only (no `.ir` file).
- `llvm_codegen_write_ir` writes `.ll` (LLVM textual IR).
- `clang` compiles `.ll` to `.o` directly.

If a binary IR serialization format ever returns, that's what
`LLVM.md` should document.

---

## 14. Gotchas

### 14.1 Debug logging gated by `DEBUG_LOG`
Phase 4 of CODEGEN_PLAN wrapped the formerly-unconditional
`fprintf(stderr, "DEBUG: …")` calls in the `DEBUG_LOG(fmt, …)` macro
(defined in `codegen_common.h` — expands to nothing in release
builds). Only a handful of raw `fprintf(stderr, …)` remain for true
warnings/errors. Production builds are quiet.

### 14.2 Module / Context globals
`TheContext`, `TheModule`, `Builder`, `DBuilder` are process-wide
unique_ptrs. `llvm_codegen_initialize` tears them down in
reverse-dependency order *before* allocating new ones, and clears
the pointer-into-Context maps (`string_constants_map`,
`label_to_bb_map`, `reverse_call_graph`). This makes re-entrant
codegen runs safe — see §3 (Reset-on-init). Phase 5 added the
`Codegen` base class scaffold for a future migration of these into
per-run instance state; the actual wholesale move is deferred.

### 14.3 `label_to_bb_map` must clear per-function
The map is module-level, but each `translateFunctionBody` clears it
at start. If you refactor and forget to clear, labels from a previous
function leak into the next.

### 14.4 `getLLVMType` recursion
Record types can contain pointers to themselves (recursive types).
The cache (`Sym::llvm_type`) must be set to the opaque struct
*before* recursing into fields, or the recursion infinite-loops.
Current code does this; if you add a new type-kind branch, do the
same.

### 14.5 Runtime helpers aren't linked by default
`llvm_codegen_compile` runs `clang file.o -o file -lm`. The `-lm` is
for math functions. The pyc runtime (`pyc_c_runtime.{c,h}`) is NOT
linked. Programs using runtime functions (allocations,
`_CG_format_string`, etc.) won't link as-is. The C backend invokes
`Makefile.cg` which handles this; LLVM doesn't have an equivalent.

### 14.6 JIT path is unimplemented
`codegen_jit` skips `llvm_codegen_compile` but does NOT invoke a JIT
engine. The intent was `llvm::ExecutionEngine` integration; the code
isn't there. Don't expect `-j` to work.

### 14.7 Debug info incomplete
`DBuilder` is set up but per-function debug info (line tables,
scopes) is partial. Compiling with `-g` may emit warnings or empty
debug info. The intent is full DWARF support; we're partway.

### 14.8 Polymorphic call sites
`get_target_fun`'s permissive fallback path (Sym-pointer match, then
Sym-name match) handles cases where `clone` didn't fully monomorphise.
The C backend treats this as a fatal codegen error; the LLVM backend
silently picks "some" matching function. If you see weird runtime
behaviour, set a breakpoint on the fallback paths.

### 14.9 `Function::ExternalLinkage` everywhere
Every emitted function uses `ExternalLinkage`. That's harmless for
linking but prevents LLVM from doing whole-module dead-code
elimination across function boundaries (every function is treated as
potentially referenced from outside). If you care about post-clang
size, change non-main functions to `InternalLinkage`.

### 14.10 No equivalent of `c_codegen_pre_file`
The C backend lets the frontend inject prologue lines via
`if1->callback->c_codegen_pre_file(fp)`. The LLVM backend has no
equivalent hook; pyc's `__pyc_insert_c_code__` blobs are silently
ignored when the LLVM backend is used. Programs that depend on them
will fail.

### 14.11 alloca-in-entry-block convention
LLVM optimisation passes promote allocas in the function's entry block
to registers (mem2reg pass). Allocas elsewhere stay as memory. The
backend emits allocas in the entry BB on purpose. If you add allocas
later, do them via a separate IRBuilder positioned at the entry
block's first instruction.

### 14.12 The `f->llvm` cache invalidation
`f->llvm` is set once by `createFunction` and never cleared. If you
re-run codegen in the same process, the cached pointer references
the *first* module's Function — which is destroyed when the new
module is allocated. Either reset all `f->llvm` to NULL between
invocations or accept single-shot semantics. (The reset hook in
`llvm_codegen_initialize` clears the module-level maps but not the
per-`Fun` `f->llvm` field; that's a known gap, filed in CODEGEN_PLAN
for the §5 wholesale Codegen migration.)

---

## 15. Symptom → start-here

| Symptom | Start here |
|---|---|
| "linker error: undefined reference to _CG_*" | Runtime not linked by `llvm_codegen_compile`; needs `-lpyc_c_runtime` added |
| "LLVM verification failed" | `verifyModule` output from `llvm_codegen_print_ir`; the IR has a malformed instruction or BB |
| "wrong target function picked" | `get_target_fun` fallback used the name-match path; check `f->calls` has the right entry |
| "missing debug info" | `DBuilder` paths in `translateFunctionBody` and `createFunction`; not all locations emit `DILocation` |
| "infinite recursion in getLLVMType" | Recursive struct type without opaque-cache shortcut; see §14.4 |
| "stale `f->llvm` after rerun" | Reset all `f->llvm` to NULL between invocations |
| "label across functions wrong" | `label_to_bb_map` not cleared in `translateFunctionBody` |
| "JIT does nothing" | The JIT path is unimplemented; only AOT compile works |
| "module flag mismatch" | Check `DEBUG_METADATA_VERSION` and target triple settings |
| "type mismatch in CreateAdd" | `mapNumericType` is producing the wrong width; check `num_index` |

---

## 16. References

- `ifa/codegen/llvm.cc` — driver, type mapping, value mapping, globals.
- `ifa/codegen/llvm_codegen.cc` — function-body translation, PNode walk.
- `ifa/codegen/llvm_primitives.cc` — `write_send`, `write_llvm_prim`.
- `ifa/codegen/llvm_internal.h` — shared header.
- `ifa/codegen/llvm.h` — public API.
- LLVM C++ API docs: <https://llvm.org/docs/ProgrammersManual.html>.
- Sister docs: [CODEGEN_C.md](CODEGEN_C.md) (parallel structure, more
  mature), [IR.md](IR.md), [PRIMITIVES.md](PRIMITIVES.md),
  [OPTIMIZE.md](OPTIMIZE.md).
- Stale: [LLVM.md](LLVM.md) — describes an IR serialization format
  whose source files no longer exist in the tree. See
  [DOCUMENTATION_PLAN.md](../DOCUMENTATION_PLAN.md) for the
  rewrite/delete plan.
