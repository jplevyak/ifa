# ifa/codegen — Pre-modification Audit

A snapshot of the state of the code in `ifa/codegen/` (≈4,376 lines
across `cg.cc`, `cg.h`, `llvm.cc`, `llvm.h`, `llvm_codegen.cc`,
`llvm_internal.h`, `llvm_primitives.cc`, `make_prims.cc`), written
as a working brief for the next person (human or LLM) about to
touch this directory.

This is paired with [CODEGEN_C.md](../CODEGEN_C.md) and
[CODEGEN_LLVM.md](../CODEGEN_LLVM.md), which describe *what each
backend does*. Read those first if you need orientation. This
document is about *the shape of the code* — what to fix, what not
to fix, and what to be aware of when refactoring.

> Reading order: §1 for the headline issues; jump to whichever
> later section matches the work in front of you.

Filed June 2026.

---

## Subsystem map

| File | LOC | Purpose | Maturity |
|---|---|---|---|
| `cg.h` | 11 | Public C-backend API (3 fns) | stable |
| `cg.cc` | 1003 | C backend (sole file) | mature, load-bearing |
| `llvm.h` | 10 | Public LLVM-backend API (3 fns, parallel to cg.h) | stable |
| `llvm_internal.h` | 93 | Shared internal header for the LLVM trio | OK |
| `llvm.cc` | 1575 | LLVM type/value/constant + driver | partially complete |
| `llvm_codegen.cc` | 867 | `createFunction` + `translateFunctionBody` + `translatePNode` | partially complete |
| `llvm_primitives.cc` | 642 | `write_send` + `write_llvm_prim` | **major gaps** |
| `make_prims.cc` | 175 | Build-time generator of `prim_data.cc/.h` from `prim_data.dat` | misfiled |

Two backends, two parallel public APIs. Documentation exists
(`CODEGEN_C.md` 579 lines, `CODEGEN_LLVM.md` 558 lines) and is
substantive.

---

## 1. Headline issues — in order of likely impact

| # | Issue | Where | Why it matters now |
|---|-------|-------|---|
| 1 | **LLVM backend structurally incomplete** — 9 of ~14 primitives unimplemented (`setter`, `apply`, `index_object`, `set_index_object`, `new`, `assign`, `len`, `clone`, `clone_vector`, `sizeof`, `sizeof_element`, `destruct`, registered prims). | `llvm_primitives.cc` `write_llvm_prim` switch | Any pyc program with fields/indexing/allocation/iteration silently lacks codegen on this backend. The gap is invisible — no error, just missing code. |
| 2 | **Three `static` maps in the LLVM backend leak across compilations.** `llvm_codegen_initialize` resets `TheContext`/`TheModule`/`Builder`/`DBuilder`, but `string_constants_map` (llvm.cc:691, function-static, never cleared) and `label_to_bb_map` (llvm_codegen.cc:10, file-level, only per-function clear) hold dangling pointers from a freed Context on the next invocation. | `llvm.cc:691`, `llvm_codegen.cc:10` | Same shape as issue 002's destructor-ordering bug; latent use-after-free on multi-fixture / multi-compile runs. |
| 3 | **`02_call.ir.codegen-c.expected` golden contradicts its own design intent.** Fixture comment says "the C backend should emit `%add` as a separate function and a direct call from `__main__`"; golden shows only `__main__` with `return 0;` — `add` is elided. The LLVM equivalent fixture DOES show `define internal void @add0()`. | `ifa/tests/ir/codegen-c/02_call.ir.codegen-c.expected` | The test passes against contradictory expectations; whatever regression it was meant to catch is invisible. |
| 4 | **Unconditional `fprintf(stderr, ...)` in the LLVM backend** — 25 in `llvm.cc`, 14 in `llvm_codegen.cc`, several in `llvm_primitives.cc` — pollutes stderr on every codegen run and isn't gated on `ifa_debug` like `DEBUG_LOG` is. | `codegen/llvm*.cc` | Goldens that diff stderr (or test runners that fail on non-empty stderr) hit noise from successful runs. |
| 5 | **Test fixture coverage is thin.** `codegen-c` has 2 fixtures, `codegen-llvm` has 4. Neither covers records-with-methods, control flow, strings, lists/dicts, recursion, virtual calls, closures, runtime-error paths, or primitives beyond trivial. The C backend's real coverage comes entirely from end-to-end `test_pyc` (74 Python programs) — integration coverage that surfaces breaks at the wrong abstraction level. | `ifa/tests/ir/codegen-c/`, `ifa/tests/ir/codegen-llvm/` | Codegen regressions get caught (or not) only at the integration level. |
| 6 | **The "on-demand create function" fallback in `write_send`** (llvm_primitives.cc:79-114) saves Builder state, recursively translates a missed callee, then restores. The comment says "This should not happen if call graph discovery worked correctly" — but the safety net is wired in anyway, masking real call-graph bugs. | `llvm_primitives.cc:79-114` | Either `discover_all_reachable_functions` is incomplete (then fix it) or the fallback is dead (then remove it). Keeping both hides which is true. |
| 7 | **Duplicated code between backends.** `num_string` (cg.cc:106-157 ≈ llvm.cc:961-1011), `is_closure_var` (cg.cc:542-545 ≈ llvm.cc:90-93), `c_type`, the `build_type_strings` structural pass — all near-identical between cg.cc and llvm.cc with no shared module. | both backends | Any convention change to `_CG_*` naming requires editing both. Will worsen as primitives get filled in on the LLVM side. |
| 8 | **`make_prims.cc` is misfiled.** It's a standalone build-time tool that emits `prim_data.cc/.h` from `prim_data.dat`. Has its own `main()`, uses raw `malloc`/`FILE*`, doesn't link with the IFA library. | `codegen/make_prims.cc` | Makes the directory look like four codegen backends when it's actually three plus a build tool. |
| 9 | **C backend has buffer-overflow risk** in `c_codegen_write_c` and `c_codegen_compile` — fixed `char fn[512]` / `char target[512]` buffers manipulated with `strcpy`/`strcat`, no bounds checking. `c_codegen_compile` also uses `system()` with no quoting on `filename`. | `cg.cc:984-1003` | Long or space-containing paths break in non-obvious ways. |
| 10 | **`build_type_strings` returns 0 unconditionally; caller checks `< 0`.** | `cg.cc:919, 926` | Dead branch. Documented in CODEGEN_C.md but never cleaned up. |

---

## 2. What's good (don't break this)

**C backend** (`cg.cc`):
- Clean 3-function public surface. Top-level driver `c_codegen_print_c`
  is linear and reads top-to-bottom.
- Primitive dispatch is a flat switch over `prim->index`; every case
  is locally complete and self-contained — easy to extend.
- `simple_move` is THE one shared move-emission helper, used uniformly
  by `Code_MOVE`, `phi`, and `phy`. Issue 011's recent comment
  captures the non-obvious "lvalue carries val's type" semantics
  inline.
- Frontend integration via `if1->callback->c_codegen_pre_file(fp)`
  keeps the backend frontend-agnostic.
- 74-program end-to-end coverage through `test_pyc`.

**LLVM backend infrastructure:**
- `llvm_internal.h` documents the inter-file API and gives a clean
  shared seam.
- `DEBUG_LOG` macro gated on `ifa_debug` is the right pattern (use
  it consistently — see headline #4).
- The destructor-ordering fix in `llvm_codegen_initialize`
  (llvm.cc:46-54) is exactly the kind of subtle invariant that
  needs to be in code, not just in commit messages. Issue 002's
  resolution is well-documented.
- Reverse-call-graph for constant-recovery is a thoughtful
  optimization (and is correctly `.clear()`'d on every build).
- Debug-info generation (DI types/variables/parameters/subprograms,
  `dbg.declare`/`dbg.value`) is properly wired.

---

## 3. The LLVM-backend primitive gap

Of ~14 primitive operations the C backend handles in `write_c_prim`
(cg.cc:183-491), the LLVM backend handles ~5:

| Primitive | C backend (`cg.cc`) | LLVM backend (`llvm_primitives.cc`) |
|---|---|---|
| `reply` | ✓ | ✓ |
| `make` (tuple/list) | ✓ | ✓ (tuple only) |
| `period` (getter) | ✓ | ✓ (no closure-creation case) |
| `setter` | ✓ | ✗ |
| `apply` | ✓ (asserted unimplemented) | ✗ |
| `index_object` | ✓ | ✗ |
| `set_index_object` | ✓ | ✗ |
| `new` | ✓ | ✗ |
| `assign` | ✓ | ✗ |
| `len` | ✓ | ✗ |
| `clone`, `clone_vector` | ✓ | ✗ |
| `sizeof`, `sizeof_element` | ✓ | ✗ |
| `destruct` | ✓ | ✗ |
| Arithmetic / comparison / logical | ✓ | ✓ (one switch in llvm_primitives.cc:255-354) |
| `primitive` named (print/println + registered table) | ✓ (RegisteredPrim->cgfn) | ✓ (print/println hardcoded only) |

The `P_prim_operator` case in `write_llvm_prim` (llvm_primitives.cc:230-253)
"hijacks" a hard-coded `"Output: %d\n"` printf for an operator —
this looks like a debugging placeholder, not a real implementation.
Treat it as dead/WIP code.

The LLVM backend has NO equivalent of the `prim_get(name)->cgfn`
dispatch (cg.cc:468-470) — registered runtime primitives have no
LLVM hook at all.

---

## 4. State-lifetime risk in the LLVM backend

The destructor-ordering fix from issue 002 (commit `3cfef44`)
handled `TheContext`/`TheModule`/`Builder`/`DBuilder`/`CU`/`UnitFile`
correctly. Three other static / file-level maps weren't part of
that fix:

```cpp
// llvm.cc:148 — cleared each call by build_reverse_call_graph()
static std::map<Fun *, std::vector<PNode *>> reverse_call_graph;

// llvm.cc:691 — function-static; NEVER cleared
static std::map<std::string, llvm::Constant *> string_constants_map;

// llvm_codegen.cc:10 — file-level; clear()'d per-function inside
// translateFunctionBody, but persists between top-level
// llvm_codegen_print_ir calls
std::map<Label *, llvm::BasicBlock *> label_to_bb_map;
```

`string_constants_map` is the worst — it caches `llvm::Constant *`
keyed by string content across calls. On the second invocation of
`llvm_codegen_print_ir`, every cache hit returns a pointer into a
Context that was destroyed in `llvm_codegen_initialize`'s reset
block. Same use-after-free shape as the issue-002 bug.

**Fix shape**: add an explicit reset for both in
`llvm_codegen_initialize`. Better yet, restructure so they're
not file-scope state at all (move to a context object passed
through).

---

## 5. Test coverage gaps

`ifa/tests/ir/codegen-c/` (2 fixtures):

- `01_baseline.ir` — empty `__main__`
- `02_call.ir` — see headline #3 (the golden is wrong)

`ifa/tests/ir/codegen-llvm/` (4 fixtures):

- `01_baseline.ir`, `02_call.ir`, `03_record_with_field.ir`,
  `04_two_records.ir`

Not covered by any pinpoint fixture (only by end-to-end `test_pyc`):
- Setter (`P_prim_setter`) — exactly the path we just fixed in
  issue 011. The Option A change had no pinpoint regression test;
  we relied on rerunning `test_pyc` for confidence.
- Index/set-index on lists and records
- Polymorphic call sites
- Control flow (`Code_IF` with non-constant condition)
- Closures and closure creation
- String operations
- Runtime-error variants (`fruntime_errors` true/false branches)
- Tuple destructuring
- Cross-type promotion / coercion

When any of these breaks, the first signal is a `test_pyc` end-to-end
diff. That makes it the wrong abstraction level for debugging — you
get a Python output mismatch instead of an "this codegen primitive
emitted wrong C" arrow.

---

## 6. Duplicated code between backends

| What | C backend | LLVM backend | Same shape? |
|---|---|---|---|
| `num_string` | cg.cc:106-157 | llvm.cc:961-1011 | Yes — identical mapping logic |
| `is_closure_var` | cg.cc:542-545 | llvm.cc:90-93 | Yes — identical 3-line implementation |
| `c_type(Var*)` / `c_type(Sym*)` | cg.cc:18-26 | (only Var* form in cg.cc) | Used in both, defined only once but heavily |
| `get_target_fun` | cg.cc:493-499 | llvm_primitives.cc:10-56 | Similar shape; LLVM has fallback search by name |
| `build_type_strings` (type cg_string assignment + symbol walk) | cg.cc:791-920 | llvm.cc:1013-1075 (`llvm_build_type_strings`) | Same structure, different output formats |

A small `codegen_common.{h,cc}` would extract these and provide a
single point of edit when type-naming conventions change. The
duplication is small now; it'll grow with every primitive added
to the LLVM side.

---

## 7. Per-file rough edges (cg.cc)

- **Line 12**: duplicate `#include "pattern.h"`.
- **Line 156**: `num_string`'s final `return 0` after an unreachable
  switch — `assert(!"case")` was hit but reachability isn't visible
  to the C compiler. Should be `__builtin_unreachable()` or
  `fail("…")` for consistency with the rest of cg.cc.
- **Line 531**: `Fun *f = sym->type->fun;` inside `write_c_fun_arg`
  **shadows the parameter `Fun *f`**. The outer `f` is unused after
  the shadow, but it makes the function harder to read.
- **Lines 984-1003**: `c_codegen_write_c` and `c_codegen_compile`
  manipulate `char fn[512]` / `char target[512]` with
  `strcpy`/`strcat` — no bounds checking. Use `snprintf` consistently.
- **`c_codegen_compile`**: uses `system()` with no quoting on
  `filename`. Compiling `tests/foo bar.py` produces an ambiguous
  command line.
- **`write_c_pnode`**: recursive over the CFG (line 711). Deep
  loops / very large functions risk stack overflow. Has been fine
  in practice but an iterative worklist would harden it.
- **`Lgetter_found:` / `Lsetter_found:` gotos** (lines 251, 290):
  work, but could be expressed as `else break;` after the
  symbol-match loop.

---

## 8. Per-file rough edges (llvm.cc)

- **1575 lines is too many**. The current split (llvm.cc / llvm_codegen.cc
  / llvm_primitives.cc) is logical at the function level — but llvm.cc
  itself does type translation + constant generation + global setup +
  the `llvm_codegen_print_ir` driver. Consider splitting type/value/
  constant code into a `llvm_types.cc`.
- **`getLLVMType`** (304-588): 280-line function, deeply nested. The
  `Type_SUM` handling (537-563) silently maps unknown sum types to
  `i8*` with a stderr warning — structurally important type, wrong
  representation.
- **Dead code**: `#if 0` block at lines 510-536 documents future
  Type_ARRAY/Type_VECTOR handling that never happened. Either
  implement or delete.
- **`Type_REF`** (491-506): computes `element_type` then ignores it
  and returns `PointerType::getUnqual` — the computation is dead
  and the surrounding comment is misleading.
- **`llvm_codegen_print_ir`** (1077-1228): calls `DBuilder->finalize()`
  twice (lines 1182, 1219). Probably a latent bug — finalize is
  meant to be called once. Remove one.
- **`llvm_codegen_compile`** (1516-1575): shells out to `clang -c -fPIC`
  via `system()`. No path quoting, no error reporting beyond
  return code. Same shape as the C backend's `system()` call.
- **25 unconditional `fprintf(stderr, ...)`** writes (Warning/ERROR
  prefixes) — convert to `DEBUG_LOG` or, for genuinely fatal cases,
  `fail(...)`.

---

## 9. Per-file rough edges (llvm_codegen.cc)

- **`createFunction`** (32-321): 290 lines, deeply nested. Split:
  return-type determination, arg-list construction, function
  creation, debug-info, ensure-terminators.
- **`translateFunctionBody`** (324-628): 304 lines. Locals-allocation
  loop (407-497) and worklist-translate loop (538-568) and
  ensure-terminators (571-626) are each candidates for extraction.
- **Lines 411-422**: unconditional `fprintf(stderr, …)` per-Var
  in the locals-allocation loop. Spammy. Should be `DEBUG_LOG`.
- **`translatePNode`** (668-867): 200 lines. The `Code_IF` case
  alone is 73 lines (774-847) with both constant-fold and dynamic
  branches inline. Worth extracting.
- **Duplicated "ensure all blocks have terminators" pass** in
  `createFunction` (296-317) and `translateFunctionBody` (571-626).
  Same logic, two copies — collapse to one helper.

---

## 10. Per-file rough edges (llvm_primitives.cc)

- **`get_target_fun`** (10-56): "Search by name" fallback (36-44)
  is risky — comment says so. If sym-ID match fails, falling back
  to name match can resolve to the wrong function in any sufficiently
  ambiguous program.
- **`write_send`** on-demand function creation (79-114): see
  headline #6.
- **`P_prim_operator`** (230-253): "hijacked for printf via
  `operator(a, ".", b)`" with hard-coded `"Output: %d\n"`. Either
  a debugging placeholder or genuinely broken behavior. Remove
  or implement.
- **Binary-op switch** (255-354): handles signed integer ops
  (`CreateSDiv`, `CreateSRem`); no unsigned variants. Fine for
  pyc's current types but a gap for future unsigned-arithmetic
  support.
- **`P_prim_make`** (355-428): uses `malloc()` for allocation
  (line 389). Should use GC allocator — the C backend uses
  `_CG_prim_tuple` runtime helper. Same `getOrInsertFunction`
  pattern works for `_CG_prim_*` helpers.
- **`P_prim_period`** (429-492): missing the closure-creation
  special case the C backend has (cg.cc:236-244).
- **`P_prim_primitive`** (493-588): hardcoded print/println only.
  No `RegisteredPrim` table dispatch.

---

## 11. `make_prims.cc` — misfiled, mediocre code

- 175 lines, standalone `main()`, reads `prim_data.dat`, writes
  `prim_data.cc` and `prim_data.h`. Build tool, not a codegen
  backend.
- Uses raw `malloc()` with `dupstr`/`catstr` helpers and never frees.
- No error handling on `fopen` results.
- The `get(p)` parser is OK but doesn't validate the data file
  format — any malformed line silently produces wrong output.

**Recommendation**: move to `ifa/tools/` or `ifa/build/`. The current
location creates the false impression that `codegen/` has four
backends.

---

## 12. Documentation status

- `CODEGEN_C.md` (579 lines) — substantive, accurate where it
  documents API.
- `CODEGEN_LLVM.md` (558 lines) — substantive but documents an
  *intended* LLVM backend more than the current state.
  Specifically, primitives that don't have implementations are
  still described as if they do.

Both docs are useful as starting reading; neither tells you that
the LLVM backend is missing 9 primitives.

---

## 13. Recommended next steps (in priority order)

1. **Fix the `string_constants_map` static-lifetime bug** in
   llvm.cc (headline #2). Move it into `llvm_codegen_initialize`'s
   reset block — same shape fix as issue 002. Low risk, high value.
2. **Bless `02_call.ir.codegen-c.expected` correctly** (headline #3).
   Either rebless to current truth (and update the fixture comment
   to match) or fix the C backend to emit `add` (which is what the
   comment expects).
3. **Triage the missing LLVM primitives** (headline #1). Either
   implement them or file an issue documenting which gaps are
   deliberate. The current state hides the gap.
4. **Add 4-6 codegen-c .ir fixtures** for paths the C backend handles
   uniquely: setter, getter, `index_object`, `set_index_object`,
   `clone`, sum types. End-to-end `test_pyc` is no substitute when
   codegen regresses.
5. **Extract `codegen_common.{h,cc}`** for `num_string`,
   `is_closure_var`, `c_type`, `get_target_fun`, and the
   symbol-table walk parts of `build_type_strings`.
6. **Move `make_prims.cc`** out of `codegen/` to a build/tools
   directory.
7. **Replace stray `fprintf(stderr, …)` with `DEBUG_LOG`** in the
   LLVM backend trio. Restore stderr cleanliness.
8. **Bounds-check `cg.cc`'s `char fn[512]` paths** with `snprintf`
   and properly escape `system()` arguments.

The C backend is mature and load-bearing — most issues there are
tidiness. The LLVM backend is genuinely incomplete and has real
lurking state-leaks; that's where focused work would pay the
largest dividend.

---

## See also

- [CODEGEN_C.md](../CODEGEN_C.md) — what the C backend does.
- [CODEGEN_LLVM.md](../CODEGEN_LLVM.md) — what the LLVM backend
  intends to do.
- [PRIMITIVES.md](../PRIMITIVES.md) — per-primitive contract that
  both backends should satisfy.
- [Issue 002](../issues/002-codegen-llvm-normalizer.md) — the
  destructor-ordering fix in `llvm_codegen_initialize`. Same
  shape as headline #2.
- [Issue 011](../issues/011-setter-codegen-vs-analyzer-mismatch.md)
  — the P_prim_setter analyzer/codegen alignment that recently
  landed.
- `ifa/analysis/AUDIT.md` — companion review of the analysis
  subsystem (same format).
