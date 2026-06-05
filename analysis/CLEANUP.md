# ifa/analysis — Cleanup checklist

Companion to [AUDIT.md](AUDIT.md). Each item links back to the
AUDIT section that explains *why*. Tiers are by **risk of
breaking the analysis**, not by effort. Check items off as
landed; delete the section once a tier is empty.

Convention: `[ ]` open · `[~]` in progress · `[x]` done · `[-]`
won't fix (explain inline). Add the PR/commit hash on close.

---

## Tier 0 — trivial, batchable into one PR

Low-effort fixes with no semantic risk. Safe to bundle. **Landed.**

- [x] **`mkdir(log_dir, 0xFFF)` → `mkdir(log_dir, 0755)`** in
  `ifalog.cc:10,13`. Same for `save_dir`.
  ([AUDIT §1 item 9](AUDIT.md#1-headline-issues--in-order-of-likely-impact))
- [x] **Replace `NULL` and obvious pointer-context `0` with
  `nullptr`** throughout the directory. Scope landed: all
  `NULL`; `Type *x = 0;` declarations; constructor member
  initializer lists for known pointer fields (`AVar`, `AEdge`,
  `CreationSet`, `EntrySet`, `Setters`, `ATypeViolation`,
  `ATypeFold`); `type_violation()` argument-`0` call sites.
  Out of scope (deferred to tier 1): `return 0;` (mixed int /
  pointer-returning), random arg-`0` at non-`type_violation`
  call sites. ([AUDIT §4.1](AUDIT.md#41-what-is-worth-modernizing))
- [x] **`enum class FAPassStage`** in `fa.h:328`. Prefix
  `FA_STAGE_` dropped (redundant under `enum class`); call sites
  updated in `fa.cc` and `ifa/testing/print_fa_converge.cc`
  (added explicit `(int)` casts at array-index sites).
  ([AUDIT §4.1](AUDIT.md#41-what-is-worth-modernizing))
- [x] **`enum class ATypeViolation_kind`** in `fa.h:229`.
  Prefix `ATypeViolation_` dropped; call sites updated in
  `fa.cc`, `ifa/if1/pattern.cc`, `python_ifa_sym.cc`.
  Switch in `show_violations` uses qualified names.
- [x] **`[[nodiscard]]`** on `extend_analysis`,
  `split_entry_set`, `split_edges`, `split_ess_for_type`,
  `split_ess_for_mark_type`, `split_for_setters`,
  `split_for_setters_of_setters`, `split_for_violations`,
  `split_with_type_marks`, `split_with_setter_marks`,
  `split_ess_setters`, `split_ess_setters_marks`, `split_css`,
  `update_setter`, `compute_setters`. Three legitimate
  ignored-return-value call sites flagged and silenced with
  explicit `(void)` casts (recursive `update_setter`,
  side-effect-only `compute_setters` in `extend_analysis`).
  ([AUDIT §4.1](AUDIT.md#41-what-is-worth-modernizing))

**Verification:** `make test` (all phases pass, 46 unit tests),
`make test_llvm`, `./test_pyc` (73 pass, 2 expected-fail, 0 fail).

---

## Tier 1 — low risk, mechanical

Self-contained changes; easy to review; no cross-file ripple.

- [x] **`int` → `bool` for two-valued predicates.** Audit each:
  `same_eq_classes`, `edge_nest_compatible_with_entry_set`,
  `sset_compatible`, `edge_sset_compatible_with_edge/_entry_set`,
  `edge_constant_compatible_with_entry_set`, `check_edge`,
  `is_fa_Var`, `is_return_value`, `is_call_result`,
  `mixed_basics`, `back_reaching`, `result_is_different`,
  `empty_type_minus_partial_applications`,
  `get_obj_index`. Skip the three-valued returns
  (`application`, `all_applications`, `entry_set_compatibility`,
  `edge_type_compatible_with_*` returning `-1`/`0`/`1`) — those
  need renaming + documentation instead.
  ([AUDIT §1 item 3](AUDIT.md#1-headline-issues--in-order-of-likely-impact),
  [§4.1](AUDIT.md#41-what-is-worth-modernizing))
- [x] **Centralize hash combiners** into a single
  `combine_hash(uintptr_t a, uintptr_t b)` helper, replacing the
  bespoke `(13 * …) + (100003 * …)` blobs in `PendingMapHash`,
  `ATypeViolationHashFuns`, `ATypeFoldChainHashFns` (all in
  `fa.h`). Mixer chosen: `(13 * a) + (100003 * b)` — matches the
  first two existing sites; `ATypeFoldChainHashFns` previously
  used `1009` for the first multiplier and now uses `13` to keep
  one mixer. Verified: `make test`, `make test_llvm`, and
  `./test_pyc` all unchanged (73 pass, 2 expected fails).
  ([AUDIT §1 item 4](AUDIT.md#1-headline-issues--in-order-of-likely-impact))
- [x] **Decide the fate of `cdb.cc` / `cdb.h`.** Deleted —
  `cdb.cc`, `cdb.h`, the `FA::cdb` member, the `class CDB` forward
  decl, the `check_es_db` stub in `fa.cc`, the
  `Fun::prof_id` / `prof_ess` / `es_info` fields, the
  `class CDB_EntrySet` forward decl in `fun.h`, and the
  `analysis/cdb.cc` entry in `ifa/Makefile`. Intent and revival
  plan captured in
  [../notes/001-compilation-database.md](../notes/001-compilation-database.md).
  ([AUDIT §1 item 7](AUDIT.md#1-headline-issues--in-order-of-likely-impact),
  [§7](AUDIT.md#7-code-thats-marked-dead-but-kept-on-purpose))
- [x] **Document the `#if 0` survivors.** All three blocks deleted
  along with the `SettersClasses` infrastructure that only existed
  to support them (`class SettersClasses`, `SettersClassesHashFns`,
  `cannonical_setters_classes`, `Setters::eq_classes`). Intent
  captured in
  [../notes/002-eager-splitting.md](../notes/002-eager-splitting.md).
  ([AUDIT §7](AUDIT.md#7-code-thats-marked-dead-but-kept-on-purpose))
- [x] **Wire `DEBUG_PRINT` into `log_tag` / `LOG_SPLITTING`.**
  The macro and all 8 call sites in `extend_analysis` rewritten to
  `log(LOG_SPLITTING, …)`, matching the rest of the splitter's
  logging. Behavioral change: stage-progress messages now gate on
  `-ls` (the splitter log channel) instead of `-d` (the global
  debug flag). Verified: `make test`, `make test_llvm`, and
  `./test_pyc` all clean (73 pass, 2 expected fails).
  ([AUDIT §1 item 5](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

---

## Tier 2 — scoped but needs care

Touches multiple files or has subtle correctness implications.
Land each as its own PR; run the full §8 checklist.

- [x] **Buffer-overflow-prone `strcat` / `strcpy` in `graph.cc`.**
  All bare `strcat` / `strcpy` sites converted to bounded
  `safe_strcat(buf, cap, src)` (new helper at the top of `graph.cc`)
  or to `snprintf`. `strcat_sym_node` and `strcat_pattern` gained
  explicit `size_t cap` parameters so callers pass `sizeof(buf)` at
  every use. `graph_start` and `graph_abstract_types` (the
  hand-rolled `pname += strlen` chain) rewritten to single
  `snprintf` / sequential `safe_strcat` calls. Verified by build +
  full test suite (graph code path is dormant from the live
  drivers — `fgraph` is parsed in `ifa/main.cc` but never consulted,
  and `pyc.cc`'s `fgraph` is never wired to args/env — but the fix
  stands on its own and keeps the door open for reviving the
  feature).
  ([AUDIT §1 item 10](AUDIT.md#1-headline-issues--in-order-of-likely-impact))
- [x] **Implement `P_prim_meta_apply` and `P_prim_cast`** —
  chose the "convert to structured `fail()`" path after
  investigation showed neither prim is emitted by any live
  frontend (V grammar has no cast syntax; pyc never references
  `prim_cast`; the `@meta_apply` send in `for1.v.code` is the
  symbol-named builtin, not the primitive). Asserts replaced with
  `fail(...)` calls that name the prim, report the PNode source
  position via `p->code->filename()` / `p->code->line()`, and
  point at the design note. The `#if 0` sketch in
  `P_prim_meta_apply` (which referenced a `meta_apply(Sym*, Sym*)`
  helper that doesn't exist) was removed; the snippet plus its
  missing dependencies and a roadmap for reviving either prim are
  preserved in
  [../notes/003-cast-and-meta-apply-prims.md](../notes/003-cast-and-meta-apply-prims.md).
  Kept the `Prim *prim_cast` / `prim_meta_apply` definitions and
  the `P_prim_*` macros — `clone.cc:234` and `llvm.cc:233` still
  reference the constants by name. Verified: `make test`,
  `make test_llvm`, `./ifa --test` (51 pass), `./test_pyc` (73
  pass / 2 expected fail) all clean.
  ([AUDIT §1 item 8](AUDIT.md#1-headline-issues--in-order-of-likely-impact))
- [x] **Add unit tests for the lattice ops.** New
  `ifa/testing/lattice_test.cc` registers 5 UnitTest functions
  (`test_type_union`, `test_type_intersection`, `test_type_diff`,
  `test_type_cannonicalize`, `test_lattice_cross_ops`) that stand
  up a minimal canonical-type world (just `bottom_type` and a
  handful of "boring" abstract types, no full FA pass) and verify
  the algebraic identities: identity / absorbing elements,
  idempotency, commutativity, associativity, distinct-disjoint
  behavior, the absorption law, and inclusion-exclusion. Result:
  `./ifa --test` now runs 51 tests (was 46), 0 failures.
  `type_num_fold` is deferred — it depends on `coerce_num` and the
  abstract-type tables for numeric symbols, which the minimal
  setup doesn't have; covered by the existing phase tests under
  `make test`. ([AUDIT §3.4](AUDIT.md#34-the-deeper-fix),
  [§8](AUDIT.md#8-things-to-verify-before-merging-an-fa-change))
- [x] **Make `IFA_PASS_LIMIT` overflow loud.** Reframed after
  reviewing the trip behavior: `type_violations` is never cleared
  on trip, so downstream consumers (e.g. pyc's `show_violations`)
  *do* see the leftover violations — the missing thing was the
  signal that they were holding a mid-iteration snapshot rather
  than a converged set. Landed:
  - New `FA::pass_limit` field (defaults to `IFA_PASS_LIMIT = 100`).
    The macro stays as the default constant; the field is what the
    splitter actually consults. Frontends can raise/lower per FA
    instance without recompiling.
  - New `FA::pass_limit_hit` flag, set true when the trip fires
    with `analyze_again` still set (i.e. the splitter wanted
    another pass and was cut off).
  - Trip site rewritten to emit `log(LOG_SPLITTING, "PASS LIMIT %d
    reached at pass %d, %d violations remain (mid-iteration)\n", …)`
    and set the flag. Behavior on natural convergence is unchanged.
  - Chose not to fail hard — the existing pyc tests include
    programs whose violations are correctly handled downstream
    (boxing fallbacks, frontend diagnostics), and aborting would
    break them. Frontends that want fail-fast behavior can check
    `fa->pass_limit_hit` and escalate.
  Verified: `make test`, `make test_llvm`, `./ifa --test`
  (51 pass), `./test_pyc` (73 pass / 2 expected fail) all clean.
  ([AUDIT §1 item 6](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

---

## Tier 3 — high risk, multi-PR

Don't attempt without a stable test harness (so: only after
[issue 009](../issues/009-fa-violations-nondeterminism.md) is
closed). Each tier-3 item is its own multi-week project.

- [ ] **Fix [issue 009](../issues/009-fa-violations-nondeterminism.md)
  (non-deterministic violation count).** Detailed 6-step plan in
  [issue 009 §Verification plan](../issues/009-fa-violations-nondeterminism.md#verification-plan)
  — recover the deleted `nested_iterator` fixture, instrument
  `type_violation`, walk the divergence point back to the
  offending iteration, `qsort_by_id` it, regression-test, file
  the deeper plib follow-up as a note. ~1 focused day if the
  AUDIT's hypothesis is right. Likely closes
  [issue 008](../issues/008-fa-crash-on-nested-iterator-shape.md)
  as a side effect. **Blocks every later tier-3 item** — until
  goldens are reliable, no large refactor can be validated.
  ([AUDIT §3](AUDIT.md#3-determinism--the-cause-of-issue-009),
  [§10](AUDIT.md#10-suggested-first-steps-for-issue-009))
- [ ] **Reentrancy step 1: sink worklists into `FA`.**
  `edge_worklist`, `send_worklist`, `es_worklist`,
  `entry_set_done`, `type_violations`, `fa_events_storage`.
  ~30 sites; each becomes `fa->...`.
  ([AUDIT §2.2](AUDIT.md#22-refactor-plan-when-ready))
- [ ] **Reentrancy step 2: sink hash-cons caches into a
  `TypeWorld` owned by `FA`.** `cannonical_atypes`,
  `cannonical_setters`, `cannonical_setters_classes`,
  `type_fold_cache`, `type_violation_hash`. Trickier because
  AType identity is shared everywhere.
- [ ] **Reentrancy step 3: sink canonical types.**
  `bottom_type`, `void_type`, `any_type`, …; move onto
  `TypeWorld`. Update `fa.h:437-451` `extern` block.
- [ ] **Reentrancy step 4: sink id counters.** `avar_id`,
  `aedge_id`, `creation_set_id`, `entry_set_id`. Trivial after
  steps 1-3; do last because everything else uses them.
- [ ] **Reentrancy step 5: remove global `fa` and `pdb`.** Thread
  `FA*` through call chains; add `EntrySet::fa` /
  `CreationSet::fa` back pointers if helpful.
  ([AUDIT §2.1](AUDIT.md#21-the-non-reentrant-set))
- [ ] **Move `graph.cc` globals into a config object.**
  `graph_fun[80]`, `graph_var[80]`, `graph_type`,
  `fgraph_frequencies`, `fgraph_constants` (`graph.h:13-17`).
  Naturally piggybacks on reentrancy step 5.
- [ ] **Fix `Vec::set_add_internal` pointer-bucket hashing in
  plib.** The deeper fix for [issue 009](../issues/009-fa-violations-nondeterminism.md):
  replace `(uintptr_t)c % n` with content-based hashing or
  chain by stable id. Cross-cutting change — affects the entire
  pyc tree, not just `ifa/analysis/`. File as a separate
  follow-on once the surface fix at the use sites stabilizes
  goldens. ([AUDIT §3.4](AUDIT.md#34-the-deeper-fix))

---

## Done

- **Tier 0 (5 items)** — landed as one batch. See tier 0 section
  above for what was in scope and what was deferred. Verified
  with full `make test` + `./test_pyc`.
- **Tier 1 (5 items)** — landed incrementally. Predicate-bool
  conversion; `combine_hash` centralization (single mixer used by
  `PendingMapHash` / `ATypeViolationHashFuns` /
  `ATypeFoldChainHashFns`); CDB removal (cdb sources + `FA::cdb`
  + `check_es_db` + `Fun::prof_id`/`prof_ess`/`es_info` deleted,
  intent in [../notes/001-compilation-database.md](../notes/001-compilation-database.md));
  three `#if 0` survivors + their `SettersClasses` support infra
  removed, intent in [../notes/002-eager-splitting.md](../notes/002-eager-splitting.md);
  `DEBUG_PRINT` macro + 8 call sites unified onto
  `log(LOG_SPLITTING, …)`. Verified with full `make test` +
  `make test_llvm` + `./test_pyc` (73 pass, 2 expected fails) at
  each step.
- **Tier 2 (4 items)** — landed incrementally. `graph.cc` buffer
  overflows fixed via a `safe_strcat(buf, cap, src)` helper +
  size params on `strcat_sym_node` / `strcat_pattern` (graph code
  path is currently dormant from the live drivers but the fix
  stands); `P_prim_cast` / `P_prim_meta_apply` asserts converted
  to structured `fail()` calls with PNode source position, intent
  in [../notes/003-cast-and-meta-apply-prims.md](../notes/003-cast-and-meta-apply-prims.md);
  5 new lattice-op unit tests in `ifa/testing/lattice_test.cc`
  (`./ifa --test` now runs 51, was 46); `IFA_PASS_LIMIT` made
  soft and configurable via `FA::pass_limit` + `FA::pass_limit_hit`
  flag instead of failing hard (frontends already iterate
  `type_violations` downstream, the missing signal was "this is a
  mid-iteration snapshot"). Verified at each step with the full
  test matrix.
