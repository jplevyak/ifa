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
allocates a process-wide LLVM `Module` / `Builder` / `DIBuilder`,
maps IF1 `Sym`s to LLVM `Type`s (`getLLVMType`), creates LLVM
`Function` declarations for every live `Fun` (`createFunction`), then
translates each function body (`translateFunctionBody`) by walking
PNodes. Each PNode becomes a `BasicBlock` (or part of one) terminated
by a branch/ret. Phi/phy MOVEs are materialised the same way as in
the C backend, but using LLVM IR `phi` instructions or simple stores.
`llvm_codegen_compile` shells out to `clang -c -fPIC <file>.ll -o
<file>.o` then `clang <file>.o -o <file> -lm`. The implementation
currently has heavy `DEBUG` output and a handful of fallback heuristics
for symbol resolution that suggest it's less mature than the C backend.

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
```

`label_to_bb_map` is **per-function-translation**; it's cleared at
the start of each `translateFunctionBody` so labels don't leak
between functions.

`all_funs_global` holds the *transitive call closure* from `main`,
computed by `discover_all_reachable_functions` (`llvm.cc:168`). Used
as a fallback target lookup table when `f->calls.get(p)` returns
inconclusive results.

---

## 4. Top-level emission

### 4.1 `llvm_codegen_write_ir` (`llvm.cc:1509`)

Opens `<filename without extension>.ll`, calls
`llvm_codegen_print_ir`, closes.

### 4.2 `llvm_codegen_print_ir` (`llvm.cc:1091`)

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

### 4.3 `llvm_codegen_initialize(fa)` (`llvm.cc:49`)

Initializes LLVM target subsystems (`InitializeAllTargetInfos`,
`InitializeAllTargets`, `InitializeAllTargetMCs`,
`InitializeAllAsmParsers`, `InitializeAllAsmPrinters`), creates the
`LLVMContext` / `Module` / `IRBuilder` / `DIBuilder`, sets the target
triple from the host.

---

## 5. Type mapping (`llvm.cc`)

### 5.1 `mapNumericType(sym)` (`llvm.cc:118`)

Maps `Sym::num_kind` + `Sym::num_index` to the corresponding LLVM
primitive type (`getInt8Ty`, `getInt32Ty`, `getDoubleTy`, etc.).

### 5.2 `getLLVMType(sym)` (`llvm.cc:318`)

The general mapper. Caches results on `Sym::llvm_type`. Handles:
- Numeric primitives → via `mapNumericType`.
- `sym_string` → `i8 *`.
- `sym_symbol` → `i64`.
- `Type_RECORD` → `StructType` (recursive; named struct with field
  types from `Sym::has`).
- `Type_FUN` (closure types) → `StructType` with function pointer +
  captured-var fields.
- `Type_REF` → pointer.
- Lists, vectors → pointers to opaque runtime structs.

Recursive types are handled by allocating an opaque named struct first,
recording it in `llvm_type`, then setting the body.

### 5.3 `getLLVMDIType(sym, di_file)` (`llvm.cc:605`)

Parallel mapper for debug-info types. Cached on
`Sym::llvm_type_di_cache`. Emits `DI*Type` records that show up in
the resulting binary's DWARF.

### 5.4 `llvm_build_type_strings(fa)` (`llvm.cc:1027`)

Pre-pass that walks all live Syms and assigns names — both the
C-side `cg_string` (matching the C backend, for runtime helpers) and
the LLVM-side StructType names. Run before any actual translation.

---

## 6. Value mapping

### 6.1 `getLLVMValue(var, ifa_fun)` (`llvm.cc:1243`)

Returns the LLVM `Value*` for a Var in the context of `ifa_fun`.
- If `var->llvm_value` is set, return it (cached).
- If `var->sym->is_constant`, call `getLLVMConstant`.
- If `var` is a formal arg, look up in `ifa_fun->llvm`'s arg list.
- If `var` is global, look up via `TheModule->getGlobalVariable`.
- Otherwise, allocate an `alloca` in the function's entry block
  (Vars are stored in stack slots for the simple case; SSA-like
  passing happens implicitly via the LLVM verifier promoting allocas
  to registers).

### 6.2 `setLLVMValue(var, val, ifa_fun)` (`llvm.cc:1486`)

Stores `val` to `var`'s LLVM location. For SSA-style Vars, just
caches `var->llvm_value = val`; for memory-backed Vars, emits a
`store` instruction.

### 6.3 `getLLVMConstant(var)` (`llvm.cc:732`)

Builds an LLVM `Constant*` from an immediate Sym:
- Numeric → `ConstantInt::get` / `ConstantFP::get`.
- String → calls `getOrCreateLLVMStringConstant` which interns
  string globals.
- Symbol → constant integer (the symbol's id).

### 6.4 `createGlobalVariables(fa)` (`llvm.cc:855`)

Walks `if1->allsyms` for globals (`!is_local`, `!is_fun`,
non-constant), emits `llvm::GlobalVariable` declarations with the
appropriate initial values.

---

## 7. Function declaration (`createFunction`, `llvm_codegen.cc:31`)

Per Fun:
1. Check `f->llvm` cache; if set, return it.
2. Determine return type: `f->rets[0]->type` mapped via `getLLVMType`.
3. Build the argument type vector by walking `f->positional_arg_positions`
   and mapping each formal's type.
4. Build the `FunctionType`, call `Function::Create(...)` with
   `ExternalLinkage` and the Fun's `cg_string` as the symbol name.
5. Cache `f->llvm = llvm_fun`.
6. Set arg names (for IR readability).
7. (Optional) Build debug info for the function declaration.

External functions (`f->is_external`) get declarations only; their
bodies come from the runtime library at link time.

---

## 8. Function body translation (`translateFunctionBody`,
`llvm_codegen.cc:324`)

The 300-line body of this function:

```c
void translateFunctionBody(Fun *ifa_fun) {
  llvm::Function *llvm_fun = ifa_fun->llvm;
  label_to_bb_map.clear();              // per-function-call reset

  // Create entry BB. Builder->SetInsertPoint(entry).
  // Stack-allocate Var slots for non-constant Vars.
  // Translate args: store each formal arg's LLVM value into its alloca.

  // Walk PNodes in CFG order from f->entry.
  Vec<PNode *> done;
  done.set_add(f->entry);
  translatePNode(f->entry, ifa_fun);    // recurses through cfg_succ

  // For any BB without a terminator, insert unreachable or ret.

  // verifyFunction(*llvm_fun) for sanity.
}
```

### 8.1 `translatePNode` (`llvm_codegen.cc:668`)

The per-PNode switch — exactly the same structure as the C backend's
`write_c_pnode` but emitting LLVM IR via `Builder->Create*`:

```c
void translatePNode(PNode *pn, Fun *ifa_fun) {
  if (pn->live && pn->fa_live) switch (pn->code->kind) {
    case Code_LABEL:
      // Emit BB (or get-or-create), Builder->SetInsertPoint(bb).
    case Code_MOVE:
      // For each lval/rval pair, simple_move.
    case Code_SEND:
      if (pn->prim) if (write_llvm_prim(ifa_fun, pn)) break;
      write_send(ifa_fun, pn);
    case Code_IF:    // emit CreateCondBr or CreateBr (constant case)
    case Code_GOTO:  // emit CreateBr
  }
  // do_phi_nodes / do_phy_nodes for successors
  // Recurse into cfg_succ, similar to C backend's extra_goto logic
}
```

### 8.2 `getLLVMBasicBlock(label, current_llvm_fun)` (`llvm_codegen.cc:12`)

Look up or create the LLVM `BasicBlock*` for an IF1 `Label`. Caches
in both `label_to_bb_map` and `label->bb` (the union field —
`Label::code` overlaps with `Label::bb`).

### 8.3 phi/phy emission

`do_phi_nodes(n, isucc, ifa_fun)` and `do_phy_nodes(n, isucc,
ifa_fun)` — same semantics as the C backend, but using
`simple_move` which emits LLVM `store`/`load`/`select` rather than C
assignment. LLVM's own SSA optimisations + verifier promote the
allocas to registers / phi instructions later.

### 8.4 `simple_move(lhs, rhs, ifa_fun)` (`llvm_codegen.cc:630`)

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

### 9.1 `write_llvm_prim` (`llvm_primitives.cc:204`)

A switch on `Prim::index` — parallel to the C backend's
`write_c_prim`:
- `P_prim_add` → `Builder->CreateAdd(a, b)` (or `CreateFAdd` for
  floats).
- `P_prim_subtract` → `CreateSub` / `CreateFSub`.
- `P_prim_mult` → `CreateMul` / `CreateFMul`.
- `P_prim_div` → `CreateSDiv` / `CreateFDiv`.
- Comparisons (`P_prim_less` etc.) → `CreateICmpSLT` /
  `CreateFCmpOLT` then optional `CreateZExt` to i1.
- Bitwise: `CreateAnd`, `CreateOr`, `CreateXor`.
- Logical: synthesised via branches.
- `P_prim_reply` → `Builder->CreateRet(val)`.
- `P_prim_make` (tuple/list) → calls runtime helpers via
  `Builder->CreateCall(runtime_fn, args)`.
- `P_prim_period` (getter) → `CreateGEP` + `CreateLoad`.
- `P_prim_setter` → `CreateGEP` + `CreateStore`.

Return value: `1` if handled, `0` to fall through to `write_send`.

### 9.2 `write_send(f, n)` (`llvm_primitives.cc:58`)

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

### 9.3 `get_target_fun(n, f)` (`llvm_primitives.cc:10`) — with fallbacks

The version here is more permissive than the C backend's:
1. Try `f->calls.get(n)` (single target).
2. If no entry: walk `n->rvals[0]`'s Sym, look it up in
   `all_funs_global` by Sym pointer.
3. Last resort: walk `all_funs_global` looking for any Fun whose
   `sym->name` matches (string compare).

The fallback exists because clone's call-graph rebuild can leave
some sites without a definitive `calls` entry in less-tested
configurations. The C backend treats this as fatal; the LLVM backend
tries harder.

If all fallbacks fail, returns NULL and the call emits an
abort-runtime call (the equivalent of C's
`assert(!"runtime error: matching function not found");`).

---

## 10. Constant propagation: `recover_constant_arg` (`llvm.cc:249`)

Heuristic for inlining-equivalent constant propagation. When a call
site passes a constant Var to a callee whose formal expects a runtime
value, this helper:
1. Walks `reverse_call_graph` (built by `build_reverse_call_graph`,
   `llvm.cc:232`) to find every call site of the function.
2. If every call site passes the same constant, use that constant
   instead of the formal's load.

This isn't fully wired up in all paths — it's referenced by some
primitive emission code but the main `getLLVMValue` doesn't go
through it. Treat as experimental.

---

## 11. The compile driver (`llvm_codegen_compile`, `llvm.cc:1530`)

```c
int llvm_codegen_compile(cchar *input_filename) {
  // input_filename → ll_file (".ll") and obj_file (".o")
  snprintf(cmd, sizeof(cmd), "clang -c -fPIC %s -o %s", ll_file, obj_file);
  int res = system(cmd);
  if (res != 0) fail("LLVM IR compilation failed...");

  // Link
  snprintf(cmd, sizeof(cmd), "clang %s -o %s -lm", obj_file, exe_file);
  res = system(cmd);
  if (res != 0) fail("Linking failed...");
  return 0;
}
```

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

### 14.1 Heavy debug logging
The LLVM backend has `fprintf(stderr, "DEBUG: ...")` calls
throughout. They're unconditional in the current source. This is noise
for production builds; either wrap them in `#ifdef DEBUG_LLVM` or
remove. (The C backend has the same temptation but cleaner gates.)

### 14.2 Module / Context globals
`TheContext`, `TheModule`, `Builder`, `DBuilder` are process-wide
unique_ptrs. A second `llvm_codegen_write_ir` invocation will reset
them (via `llvm_codegen_initialize`), which destroys all previously
emitted state. Don't try to emit two programs from one process unless
each finishes before the next begins.

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
invocations or accept single-shot semantics.

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
