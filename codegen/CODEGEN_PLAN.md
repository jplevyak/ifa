# ifa/codegen — Plan to production quality

A phased plan to address the findings in
[AUDIT.md](AUDIT.md), bring both backends to full production
quality, and unify their organization to the extent it makes
sense.

Companion to [AUDIT.md](AUDIT.md) (what's wrong) and
[CODEGEN_C.md](../CODEGEN_C.md) /
[CODEGEN_LLVM.md](../CODEGEN_LLVM.md) (what each backend does).

> Reading order: skim §1 (principles) and §2 (the phase map),
> then jump to whichever phase is in front of you.

Convention: `[ ]` open · `[~]` in progress · `[x]` done ·
`[-]` won't fix (explain inline). Add the PR / commit hash on
close.

Filed June 2026.

---

## 1. Design principles

These shape every decision below.

1. **Two backends, one contract.** From IF1's perspective the
   backends are substitutable. Every primitive operation that
   the C backend emits must have an LLVM emission. Every type
   that maps to a `cg_string` must map to a `llvm::Type`. Where
   the contract is unwritten, write it in [PRIMITIVES.md](../PRIMITIVES.md)
   before implementing.

2. **Share infrastructure where it makes sense; diverge where
   the targets genuinely differ.** `num_string`, type-string
   tagging, `is_closure_var`, `get_target_fun`, the symbol-table
   walk in `build_type_strings` — these are the same logic in
   both backends and should live in a shared module.
   `simple_move` (string-emission vs LLVM Value flow), local
   allocation (declarations vs `AllocaInst`), control flow
   (gotos vs BasicBlocks) — these genuinely differ.

3. **Each phase ships independently.** No Big Bang rewrites.
   Each phase leaves the tree in a passing state (`make test`,
   `make test_llvm`, `./test_pyc`). Exit criteria are explicit.

4. **Test coverage at three levels.**
   - Pinpoint `.ir` fixtures (one primitive per golden).
   - Integration via `test_pyc` (real pyc programs end-to-end).
   - Parity between backends (same `.ir` runs through both,
     compare normalized output where comparable).

5. **Fail loudly at codegen-detection time, not at runtime.**
   `fprintf(stderr, "WARNING…")` followed by silent wrong output
   is a footgun. Either it's recoverable (then no stderr) or
   it's a bug (then `fail(...)`). Don't leave the third option.

---

## 2. Phase map

| Phase | Focus | Risk | Bounded effort | Exit criterion |
|---|---|---|---|---|
| 0 | Foundation — safety, dead code, file moves | Low | 1-2 PRs | Same test pass count as today; LLVM-backend state-leaks fixed. |
| 1 | Test infrastructure | Low | 1-2 PRs | ≥ 8 codegen-c fixtures, ≥ 10 codegen-llvm fixtures, primitive coverage matrix in PRIMITIVES.md. |
| 2 | Shared `codegen_common.{h,cc}` | Medium | 1 PR | Duplicated functions removed; both backends compile and pass tests against shared module. |
| 3 | Complete LLVM primitives | High | 4-8 PRs (1-2 per primitive) | All ~14 primitives that C backend handles also handled by LLVM; `IFA_LLVM=1 ./test_pyc` matches C-backend results. |
| 4 | Hardening and modernization | Medium | 2-3 PRs | Buffer safety in cg.cc; createFunction / translateFunctionBody / translatePNode each ≤ 100 lines; no unconditional `fprintf(stderr,…)` outside `DEBUG_LOG`/`fail`. |
| 5 | Unified architecture — codegen state object, primitive dispatch table | Medium | 2-3 PRs | A single `Codegen` context replaces file-scope globals; one primitive-dispatch interface used by both backends. |
| 6 | Production polish | Low | 1-2 PRs | CI gates `make test` + `test_pyc` + `IFA_LLVM=1 ./test_pyc` on each push; CODEGEN_C.md / CODEGEN_LLVM.md describe reality. |

Total: 12-21 PRs across 6 phases. No phase blocks any other
*phase* but items within a phase often have an internal order.

---

## 3. Phase 0 — Foundation

Low-risk batchable cleanup. Many of these can ship as one or two
PRs. Each fixes a specific AUDIT item.

### 0.1 LLVM backend static-state leaks ([AUDIT §1 #2](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

- [ ] **Move `string_constants_map` into `llvm_codegen_initialize`'s
  reset.** `llvm.cc:691` declares a function-static
  `std::map<std::string, llvm::Constant *>` that's never cleared.
  Across the next `llvm_codegen_print_ir` call its cached pointers
  reference a freed Context. Either lift it to a file-scope
  variable that `llvm_codegen_initialize` `.clear()`s, or
  (better) make it a member of a future `Codegen` context object
  (deferred to phase 5).
  **Verify**: 10× back-to-back `./ifa-test --phase codegen-llvm`
  runs are byte-identical and don't leak between fixtures.

- [ ] **Reset `label_to_bb_map` in `llvm_codegen_print_ir`.**
  `llvm_codegen.cc:10` declares the map at file scope.
  `translateFunctionBody` clears it per-function, but the
  top-level driver doesn't clear before the first function. Add
  `label_to_bb_map.clear()` early in `llvm_codegen_print_ir` —
  same shape as the other reset block.

- [ ] **Audit for other file-static maps** in the LLVM backend.
  `grep -n "static std::map\|static std::vector\|static std::set"`
  across the trio. Any that survives a `llvm_codegen_initialize`
  call needs to be reset or moved.

### 0.2 02_call.ir.codegen-c.expected golden ([AUDIT §1 #3](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

- [ ] **Decide which truth the golden should encode.** Two
  options:
  - **A.** Rebless to current truth (the C backend elides `add`
    because its result is unused). Update the fixture comment to
    explain why — "DCE removes `add` since `r` is unused; locked
    in to detect a regression where DCE stops firing here."
  - **B.** Modify the fixture so `add`'s result is used (e.g.,
    print `r`), forcing emission. Then rebless. This is the
    "stronger test" path and also brings the C fixture into
    parallel with the LLVM fixture (which DOES show
    `define internal void @add0()`).
  Recommend (B) — gets actual primitive coverage.

### 0.3 Move `make_prims.cc` out of `codegen/` ([AUDIT §1 #8](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

- [ ] **Move to `ifa/tools/make_prims.cc`** (create dir).
  Update Makefile to find it in the new location. Add a one-line
  comment at the new top of file explaining what it does
  ("Build-time tool: reads prim_data.dat, writes prim_data.cc
  and prim_data.h. Not part of the runtime codegen.").

### 0.4 Dead code removal

- [ ] **`cg.cc:12`**: delete duplicate `#include "pattern.h"`.
- [ ] **`cg.cc:919, 926`**: `build_type_strings` returns 0
  unconditionally; caller's `if (... < 0) fail(...)` is dead.
  Either change return type to `void` (and drop the check) or
  make `build_type_strings` actually return non-zero on failure
  (preferred — it has a `fail("setter not resolved")` path that
  should propagate). Pick one.
- [ ] **`cg.cc:156`**: `num_string` returns `0` after an
  unreachable switch — replace with `__builtin_unreachable()`
  or `fail("unhandled num_kind %d", s->num_kind)`. Same shape
  in `cg.cc`'s other switch defaults.
- [ ] **`llvm.cc:510-536`**: dead `#if 0` block documenting
  intended Type_ARRAY handling. Delete (the design idea is in
  the AUDIT now).
- [ ] **`llvm.cc:468-474`**: dead `#if 0` block for varargs.
  Same disposition.
- [ ] **`llvm.cc:491-506`** (`Type_REF`): computes
  `element_type` then ignores it. Either use it or delete the
  computation.

### 0.5 stderr → DEBUG_LOG in the LLVM backend ([AUDIT §1 #4](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

- [ ] **`llvm.cc`** (25 sites): every `fprintf(stderr, …)` that
  isn't followed by `fail(...)` should be `DEBUG_LOG(...)`.
  Special cases:
  - `getLLVMType`'s "ERROR: getLLVMType called with function
    symbol …" branch (lines 317-329) — this IS a real error.
    Convert to `fail(...)`.
  - The "WARNING: Function not found in module" path
    (line 1319) — investigate whether this is recoverable. If
    yes, `DEBUG_LOG`. If no, `fail`.
- [ ] **`llvm_codegen.cc`** (14 sites): same treatment.
  Particular focus on the locals-allocation loop spew at
  lines 411-422 — those are flat unconditional writes that
  should be `DEBUG_LOG` immediately.
- [ ] **`llvm_primitives.cc`**: same treatment.

### 0.6 Buffer-safety in cg.cc ([AUDIT §1 #9](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

- [ ] **`cg.cc:984-1003`**: replace `char fn[512]` /
  `char target[512]` + `strcpy`/`strcat` with `snprintf` and
  size checks. If snprintf truncates, `fail(...)`.
- [ ] **`c_codegen_compile`**: properly quote `filename` and
  `system_dir` arguments to the `system()` call (or move to
  `posix_spawn` with explicit argv to avoid shell entirely —
  preferred).

### 0.7 LLVM `DBuilder->finalize()` called twice

- [ ] **`llvm.cc:1182, 1219`**: `DBuilder->finalize()` is called
  at line 1182 and again at line 1219. Delete the first call
  (or the second — they're at function exit).

### Phase 0 exit criteria

- All AUDIT headline items #2, #3, #4, #8, #9 closed or in
  progress.
- `./test_pyc` pass count unchanged (74 / 1 expected fail / 0).
- `./ifa-test` clean across all phases.
- 10× consecutive `./ifa-test --phase codegen-llvm` runs are
  byte-identical (confirms 0.1).

---

## 4. Phase 1 — Test infrastructure

The thinnest part of the codegen subsystem. Without pinpoint
fixtures, every regression manifests at the wrong abstraction
level.

### 1.1 Add codegen-c pinpoint fixtures ([AUDIT §1 #5](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

Aim for one `.ir` fixture per primitive emission path in
`write_c_prim`. Each fixture should be ~10-20 lines of `.ir`,
exercise one primitive in isolation, and produce a stable
golden ≤ 50 lines.

- [ ] **`03_setter.ir`** — `(send @primitive @setter obj selector val)`.
  Exercises cg.cc:262-291. Golden locks in the issue-011 val-emit
  behavior.
- [ ] **`04_getter.ir`** — `(send @primitive @period obj selector)`
  on a record field. Exercises cg.cc:222-261.
- [ ] **`05_index_object.ir`** — `(send @primitive @index_object …)`
  on a list and on a record. Exercises cg.cc:320-352.
- [ ] **`06_set_index_object.ir`** — same on the setter side.
  Exercises cg.cc:353-378.
- [ ] **`07_clone.ir`** — `(send @primitive @clone proto)`.
  Exercises cg.cc:404-419.
- [ ] **`08_sum_type.ir`** — record + nil sum type. Exercises
  the `Type_SUM` cg-string collapse path.
- [ ] **`09_runtime_error.ir`** — fixture with
  `fruntime_errors=true` exercising the "matching function not
  found" assert emission.
- [ ] Update `02_call.ir` per phase 0.2 so it actually exercises
  the call site (option B from 0.2).

### 1.2 Mirror in codegen-llvm

For each codegen-c fixture added, create the corresponding
codegen-llvm fixture with the same `.ir` content and bless its
own golden. This produces a parity matrix where both backends'
behavior on the same input is locked in.

- [ ] `05_setter.ir.codegen-llvm.expected` (LLVM backend will
  initially fail — that's the input to phase 3).
- [ ] `06_getter.ir.codegen-llvm.expected`.
- [ ] (… one per phase-1.1 fixture …)

### 1.3 Primitive coverage matrix in `PRIMITIVES.md`

- [ ] **Add a table** to `PRIMITIVES.md` listing every primitive
  with three columns: "C backend status", "LLVM backend status",
  "pinpoint fixture". This is the single dashboard that tells
  you what's covered.

### 1.4 Optional: differential / fuzz tests

- [ ] **(Stretch)** Add a `differential_test` harness: generate
  small random `.ir` files, run through both backends, compile,
  execute, compare outputs. Catches semantic divergence the
  golden tests miss. Defer if scope explodes.

### Phase 1 exit criteria

- ≥ 8 codegen-c fixtures (was 2).
- ≥ 10 codegen-llvm fixtures (was 4).
- PRIMITIVES.md has the coverage matrix.
- All fixtures pass via `make test`.

---

## 5. Phase 2 — Shared `codegen_common.{h,cc}`

Extract the duplicated functionality between `cg.cc` and
`llvm.cc` into a single module. Both backends include the
header, link against the implementation.

### 2.1 Create the module

- [ ] **`ifa/codegen/codegen_common.h`** and **`codegen_common.cc`**.
  Add to `ifa/Makefile`.

### 2.2 Move duplicated functions ([AUDIT §1 #7](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

Each function is currently identical between backends. Move,
delete from `cg.cc` and `llvm.cc`, update includes.

- [ ] **`num_string(Sym *s)`** — IF1 numeric-type → name
  mapping. Currently in cg.cc:106-157 and llvm.cc:961-1011.
- [ ] **`is_closure_var(Var *v)`** — cg.cc:542-545 and
  llvm.cc:90-93.
- [ ] **`c_type(Var *)` / `c_type(Sym *)`** — cg.cc:18-26.
  Used only by C backend currently but the structure is shared.
- [ ] **`get_target_fun(PNode *n, Fun *f)`** — cg.cc:493-499
  and llvm_primitives.cc:10-56 (with the LLVM-specific name-match
  fallback isolated under `#ifdef CODEGEN_LLVM_FALLBACKS` or a
  parameter).
- [ ] **The type-string assignment pass** that walks `allsyms`
  and assigns `cg_string` to each Sym. The two implementations
  (cg.cc:791-920 and llvm.cc:1013-1075) have the same shape but
  emit different strings. Factor the walk into a shared
  template / callback structure.

### 2.3 Define the primitive emission contract

Backends differ in how they emit, but they all answer the same
question: "given a SEND PNode for primitive P, in function F,
emit code that writes lvals[0] = computation over rvals."

- [ ] **Document the contract in `PRIMITIVES.md`** — what
  `rvals[0..n]` mean for each prim, what `lvals[0]` should hold
  after emission, whether `lvals[0]->live` gating applies.
- [ ] **Add an interface header** (in `codegen_common.h`) for
  primitive emission — see phase 5 for the actual unified
  dispatch. For now, just the documentation.

### Phase 2 exit criteria

- `codegen_common.{h,cc}` exists, holds the moved functions.
- `cg.cc` and `llvm.cc` no longer contain duplicates.
- `make` clean; all tests pass.

---

## 6. Phase 3 — Complete LLVM primitives

The largest phase by raw work, but each primitive can ship
independently. Order matters: implement the most common
primitives first so `IFA_LLVM=1 ./test_pyc` unlocks coverage
incrementally.

### 3.1 Implementation order (rough — pick what unblocks `test_pyc` most)

- [ ] **`P_prim_setter`** — already specced by issue 011's
  Option A landing on the C side. LLVM equivalent: GEP +
  store + writeback to lvalue.
- [ ] **`P_prim_index_object`** — list/vector and record
  indexing. Exercise: any `a[i]` in test_pyc.
- [ ] **`P_prim_set_index_object`** — same on the writer side.
- [ ] **`P_prim_new`** — runtime `_CG_prim_new` equivalent.
  Use GC allocator hooks, not raw `malloc`.
- [ ] **`P_prim_assign`** — handles `assign` primitive. cg.cc:386-391.
- [ ] **`P_prim_len`** — string vs list / record dispatch.
- [ ] **`P_prim_clone`** / **`P_prim_clone_vector`** — runtime
  `_CG_prim_clone` equivalent.
- [ ] **`P_prim_sizeof`** / **`P_prim_sizeof_element`** — constant
  emission.
- [ ] **`P_prim_destruct`** — tuple destructuring.
- [ ] **`P_prim_apply`** — currently asserts unimplemented in
  cg.cc:294. Investigate whether LLVM needs it before fixing
  the C version.

### 3.2 Hook up RegisteredPrim dispatch

- [ ] **`P_prim_primitive`** (`llvm_primitives.cc:493-588`)
  currently hardcodes `print`/`println`. The C backend uses
  `prim_get(name)->cgfn(fp, n, f)` (cg.cc:467-482) to dispatch
  to a registered function-pointer table. The LLVM backend needs
  its own analog: `RegisteredPrim->llvm_cgfn(ifa_fun, n)`.
- [ ] **Add `llvm_cgfn` field to `RegisteredPrim`** struct.
- [ ] **Migrate existing C-side registered primitives** to also
  register LLVM emitters. Initially, fall through to a
  "unimplemented for LLVM backend" `fail` if unset.

### 3.3 P_prim_make using GC allocator

- [ ] **Replace `malloc` (llvm_primitives.cc:389)** with a call
  to the appropriate `_CG_prim_*` runtime helper. The C backend
  emits `_CG_prim_tuple(type, n)` (cg.cc:199); LLVM should
  `getOrInsertFunction` for the same symbol and call it.

### 3.4 Cleanup `P_prim_operator` placeholder

- [ ] **`llvm_primitives.cc:230-253`**: the "hijacked for printf"
  case with hardcoded `"Output: %d\n"` is a debugging artifact.
  Either it's now obsoleted by `P_prim_primitive` print/println,
  in which case delete; or it's load-bearing for some path, in
  which case document and rename.

### 3.5 Achieve `IFA_LLVM=1 ./test_pyc` parity

- [ ] **Run** `IFA_LLVM=1 ./test_pyc` and triage failures
  primitive-by-primitive. Each missing primitive is a failure
  this phase needs to close.
- [ ] **Lock in** the parity bar: as primitives ship,
  `IFA_LLVM=1 ./test_pyc` pass count climbs monotonically.

### Phase 3 exit criteria

- Every primitive in `write_c_prim`'s switch has an LLVM
  equivalent in `write_llvm_prim`.
- `IFA_LLVM=1 ./test_pyc` passes the same set of tests
  C-backend passes (or has a documented expected-fail per test
  for known LLVM-only gaps).
- Each phase-1.2 LLVM fixture passes.

---

## 7. Phase 4 — Hardening and modernization

After functionality is complete, harden the code.

### 4.1 Function decomposition

- [ ] **`createFunction`** (llvm_codegen.cc:32-321, 290 lines):
  split into `determine_return_type`, `build_arg_list`,
  `create_llvm_function`, `attach_debug_info`,
  `ensure_external_terminators`.
- [ ] **`translateFunctionBody`** (llvm_codegen.cc:324-628):
  split into `prepare_basic_blocks`, `allocate_locals`,
  `emit_parameter_debug_info`, `translate_pnodes_worklist`,
  `ensure_terminators` (and reuse `ensure_terminators` between
  `createFunction` and this — currently duplicated).
- [ ] **`translatePNode`** (llvm_codegen.cc:668-867): extract
  `Code_IF` and `Code_SEND` into their own functions.
- [ ] **`getLLVMType`** (llvm.cc:304-588): split by `type_kind`
  switch case into private helpers.

Exit criterion: no function in `codegen/llvm*.cc` longer than
100 lines (excluding switch tables).

### 4.2 Modern C++ idioms

- [ ] **Replace raw `char[]` paths in `cg.cc:984-1003`** with
  `std::string` + bounds-safe construction.
- [ ] **RAII for `FILE*`** in `c_codegen_write_c` (currently
  fine but a `std::ofstream` would be safer if it fits the
  callback contract).
- [ ] **`const`-correct** the cg.cc helpers (`c_type`, `c_rhs`)
  — they take pointers that aren't mutated.

### 4.3 Safer process invocation

- [ ] **`c_codegen_compile`** (cg.cc:993-1003) and
  **`llvm_codegen_compile`** (llvm.cc:1516-1575): replace
  `system()` with `posix_spawn` or `execvp` using explicit
  argv arrays. Eliminates shell injection from filenames.

### 4.4 Defensive code consolidation

- [ ] **"Ensure terminators" pass** is currently duplicated in
  `createFunction` (lines 296-317) and `translateFunctionBody`
  (lines 571-626). Extract into one helper.
- [ ] **"On-demand create function" fallback** in
  `write_send` (llvm_primitives.cc:79-114): see AUDIT §1 #6.
  Either prove `discover_all_reachable_functions` is complete
  (delete the fallback, treat misses as `fail`) or document
  why the fallback is structurally required and keep it.

### Phase 4 exit criteria

- No function in `codegen/` longer than 100 lines (excluding
  switch tables).
- No `system()` calls in production codegen paths.
- No duplicated "ensure terminators" / "ensure x" defensive
  blocks.

---

## 8. Phase 5 — Unified architecture

With both backends complete and clean, unify their internal
organization.

### 5.1 `Codegen` context object

- [ ] **Define `struct Codegen`** (or `class Codegen`) in
  `codegen_common.h` that owns:
  - The current `FA *`, `IF1 *`, target `Fun *` references.
  - Per-codegen-call state: type-string allocations, name
    counters.
  - Per-backend state via virtual functions or member objects
    (FILE* vs Builder, etc.).
- [ ] **Move file-scope LLVM globals** (`TheContext`, `TheModule`,
  `Builder`, `DBuilder`, `CU`, `UnitFile`, `all_funs_global`)
  into a `LLVMCodegen` subclass of `Codegen`.
- [ ] **Lifetime guarantee**: `Codegen` is created at the start
  of `c_codegen_print_c` / `llvm_codegen_print_ir` and destroyed
  at exit. Eliminates file-scope-state-leak entirely (the AUDIT
  headline #2 class of bug becomes structurally impossible).

### 5.2 Unified primitive dispatch

- [ ] **Define a `PrimEmitter` interface** in
  `codegen_common.h`:
  ```cpp
  struct PrimEmitter {
    virtual void emit_setter(Codegen&, PNode*) = 0;
    virtual void emit_getter(Codegen&, PNode*) = 0;
    // … one method per primitive …
  };
  ```
  (Or a table of function pointers keyed on `prim->index` — pick
  whichever the team prefers stylistically.)
- [ ] **Move `write_c_prim` switch into `CPrimEmitter`** —
  each case becomes a method.
- [ ] **Move `write_llvm_prim` switch into `LLVMPrimEmitter`**.
- [ ] **Driver code** (the per-Code_kind walk over PNodes)
  becomes shared.

This is a refactor, not a behavior change. Tests should be
byte-identical before/after.

### 5.3 Consistent error handling

- [ ] **Define `codegen_fail(Codegen&, cchar *fmt, ...)`** that
  knows about source location and emits a structured error
  pointing at the offending PNode/Var. Replace ad-hoc
  `fail("…")` calls with this.

### Phase 5 exit criteria

- No file-scope globals in LLVM backend (everything owned by a
  `Codegen` instance).
- One primitive-dispatch interface used by both backends.
- All tests pass.

---

## 9. Phase 6 — Production polish

The bow-tying phase. Land after phases 0-5 to know the polish
sticks.

### 9.1 CI gates

- [ ] **GitHub Actions / CI config** runs on every push:
  `make`, `make test`, `make test_llvm`, `./test_pyc`,
  `IFA_LLVM=1 ./test_pyc`, `make test_dparse`.
- [ ] **Optional**: a `--keep-build` CI mode that retains
  `tests/build/` artifacts for debugging failures.

### 9.2 Documentation alignment

- [ ] **`CODEGEN_C.md`** — review against current code. Currently
  documents the dead `< 0` branch (cg.cc:919). Fix any other
  drift.
- [ ] **`CODEGEN_LLVM.md`** — review. After phase 3, many
  primitives listed as "intended" will be "implemented" —
  update.
- [ ] **`PRIMITIVES.md`** — coverage matrix from phase 1.3
  reflects post-phase-3 state.

### 9.3 Performance pass

- [ ] **Profile** `c_codegen_print_c` and `llvm_codegen_print_ir`
  on the largest test programs. Suspected hot spots: the type-
  string allocation loop, the per-function PNode worklist.
- [ ] **Measure** `pyc` end-to-end compilation time on
  representative programs (sieve.py, dict_basic.py, the largest
  test_pyc fixture). Establish a baseline; aim for ≤ 10%
  regression on any single PR through this plan.

### 9.4 Backend selection cleanup

- [ ] **Document `IFA_LLVM=1`** in CODEGEN_LLVM.md and README.
- [ ] **Single source of truth** for which backend is active —
  currently `IFA_LLVM` env var + `pyc.cc` branches. Move
  decision to a clean `Codegen::factory()`.

### Phase 6 exit criteria

- CI green on push.
- Docs match reality.
- Performance baseline recorded; no regression introduced by
  the plan.

---

## 10. What's deliberately NOT in scope

Things that look like they could be in this plan but aren't:

- **A third backend** (WASM, bytecode, etc.). Wait until the
  two existing backends are at parity.
- **Cross-type method dispatch semantic check** (the missing
  Option C from issue 011). That's a frontend/analyzer concern;
  not codegen.
- **Major IF1 IR changes** (e.g., dropping the `Code_kind` enum
  in favor of subclasses). Out of scope; touch only the codegen
  *consumption* of IF1.
- **Runtime library work** (`pyc_c_runtime.h` and friends).
  Codegen interacts with the runtime ABI but doesn't own it.
  Runtime changes are tracked separately.
- **Frontend changes** for cross-type checks, reflective
  primitives, etc. Out of scope.

---

## 11. Ordering across PRs

Within each phase items can ship in any order. Between phases
the dependency graph is:

```
Phase 0 (foundation)
  └── Phase 1 (test infra)
        └── Phase 2 (shared module)
              ├── Phase 3 (LLVM primitives) — can run in parallel with Phase 4
              └── Phase 4 (hardening) — can run in parallel with Phase 3
                    └── Phase 5 (unification)
                          └── Phase 6 (polish)
```

Phases 3 and 4 are independent — different files, different
concerns. They can ship in parallel.

---

## 12. Estimated cost

Rough sizing per phase (PR count × hours-per-PR):

| Phase | PRs | Hours/PR | Total |
|---|---|---|---|
| 0 | 1-2 | 3-5 | 4-10 |
| 1 | 1-2 | 4-8 | 4-16 |
| 2 | 1 | 6-10 | 6-10 |
| 3 | 4-8 | 4-8 | 16-64 |
| 4 | 2-3 | 4-8 | 8-24 |
| 5 | 2-3 | 8-16 | 16-48 |
| 6 | 1-2 | 3-6 | 3-12 |
| **Total** | 12-21 | | **~57-184 hours** |

Order-of-magnitude. The wide range on phase 3 reflects unknown
LLVM-specific quirks per primitive. Phase 5 is the second-most
expensive — a structural refactor is harder than functional
additions.

If sizing matters: phase 0 alone (a single afternoon's work)
addresses the live latent bug (state leak) and most of the
nuisance items in AUDIT. Phase 1 makes the rest of the plan
verifiable. The minimum viable subset is phases 0 + 1.

---

## 13. Cross-references

- [AUDIT.md](AUDIT.md) — what's broken; this plan fixes it.
- [CODEGEN_C.md](../CODEGEN_C.md), [CODEGEN_LLVM.md](../CODEGEN_LLVM.md)
  — what each backend does today.
- [PRIMITIVES.md](../PRIMITIVES.md) — the per-primitive contract.
  Phase 1.3 / 2.3 / 3 all touch this.
- [Issue 011](../issues/011-setter-codegen-vs-analyzer-mismatch.md)
  — the analyzer/codegen alignment that landed June 2026.
- `ifa/analysis/AUDIT.md` and `ifa/analysis/CLEANUP.md` —
  precedent for the AUDIT + plan pattern.
