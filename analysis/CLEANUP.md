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

- [x] **Fix [issue 009](../issues/009-fa-violations-nondeterminism.md)
  (non-deterministic violation count).** **Closed June 2026.**
  Diagnosis was a surprise: the symptom was a **measurement bug**
  in the printer, not an iteration-order non-determinism as the
  AUDIT hypothesized. `type_violations` is a `Vec`-as-set; `.n`
  is the open-addressed table capacity, not the live element
  count (which is `.set_count()`). 9 of 17 fa-converge fixtures
  were silently mis-reporting — only `nested_iterator` happened
  to land near a probe-collision threshold often enough to
  *alternate* visibly. Landed:
  - Step 1: restored `nested_iterator` fixture + builder; added
    env-gated `IFA_PRINT_VIOLATIONS` to the printer; confirmed
    13/31 alternation reproduces in HEAD.
  - Step 2: added env-gated per-call trace in `type_violation()`
    (`IFA_DEBUG_VIOLATIONS=1`); diagnosed the `.n` vs
    `.set_count` measurement bug; scanned all fixtures.
  - Step 3: replaced `type_violations.n` with `.set_count()` at
    10 reporting sites in `fa.cc` (printer recorder, 7 splitter
    `viol0` snapshots, pass-limit trip log, return-code check).
  - Step 4: dropped the printer's env gate, re-blessed 14
    fa-converge goldens. Determinism restored — 20× nested_iterator
    runs all identical.
  - Step 5: regression test landed —
    `test_type_violation_dedup_invariance` in
    `ifa/testing/lattice_test.cc` exercises the order-invariance
    of `type_violations.set_count()` across distinct triples and
    confirms `kind` is part of the dedup key. New public helper
    `type_violations_count()` exposes the live count.
    `./ifa --test`: 52/0 (was 51).
  - Step 6: cross-cutting plib pointer-set hashing follow-on
    filed as
    [../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md).
    No code change — captures the AUDIT §3.4 concern, the
    iteration-order effect (latent, contained by ~17
    `qsort_by_id` sites in `fa.cc`), the ~244 set_add/set_in
    call-site surface area, and three migration shapes
    (sorted-view helper / content-based hashing / dedicated
    IdSet container).
  - **Unexpected side observation**: 40 multi-fixture runs
    probing for
    [issue 008](../issues/008-fa-crash-on-nested-iterator-shape.md)
    produced zero crashes (008 claimed 30-60% reproducibility).
    Not closing 008 unilaterally; flagged for separate
    investigation.
  ([AUDIT §3](AUDIT.md#3-determinism--the-cause-of-issue-009),
  [§10](AUDIT.md#10-suggested-first-steps-for-issue-009),
  [Issue 009 §Verification plan](../issues/009-fa-violations-nondeterminism.md#verification-plan)
  for the full trail)
- [x] **Reentrancy step 1: sink worklists into `FA`.** Done
  June 2026. Seven globals moved onto `class FA` as members:
  `edge_worklist`, `send_worklist`, `es_worklist`,
  `entry_set_done`, `type_violations`, `fa_events_storage`,
  `fa_events_enabled`. Touch sites in `fa.cc` were redirected
  via `fa->...` in free functions; references inside
  `FA::analyze` (member function) work as bare member access.
  The `fa_events_*()` free functions called by the test harness
  delegate via `pdb->fa` because they fire *before* `FA::analyze`
  sets the global `fa` pointer. `fa_reset()` no longer clears
  the moved members — FA destruction handles them.
  `FAPassEvent` got a forward declaration in `fa.h` since
  `Vec<FAPassEvent *>` is now used in the `FA` class body before
  its definition. Verified: `./ifa --test` (52/0), full
  `make test` (15 phases clean), `make test_llvm`, `./test_pyc`
  (73 pass / 2 expected fail), and 20 multi-fixture
  `fa-converge` runs with `nested_iterator` included (0 crashes
  — same as the post-009 baseline).
  ([AUDIT §2.2](AUDIT.md#22-refactor-plan-when-ready))
- [x] **Reentrancy step 2: sink hash-cons caches into a
  `TypeWorld` owned by `FA`.** Done June 2026. New `class
  TypeWorld : public gc` in `fa.h` (placed between the four
  hash-fn classes and `class FA`) holds the four remaining
  hash-cons caches: `cannonical_atypes`, `cannonical_setters`,
  `type_fold_cache`, `type_violation_hash`. (`cannonical_setters_classes`
  was already removed during tier-1 eager-splitting cleanup.)
  `FA` got a `TypeWorld type_world` member. The four
  file-statics in `fa.cc` and their `fa_reset()` clears are
  gone; the 7 reference sites were redirected via
  `fa->type_world.X`. AType identity remains meaningful only
  *within* one TypeWorld — today there's one TypeWorld per FA,
  so the per-process behavior is unchanged; future work can
  share a TypeWorld across multiple FAs if cross-instance
  AType identity is wanted. Verified: `./ifa --test` (52/0),
  full `make test` (15 phases clean), `make test_llvm`,
  `./test_pyc` (73 pass / 2 expected fail), and 20× multi-
  fixture `fa-converge` (0 crashes).
- [x] **Reentrancy step 3: sink canonical types.** Done June 2026.
  All 17 canonical AType pointers (`bottom_type`, `nil_type`,
  `unknown_type`, `void_type`, `top_type`, `any_type`, `bool_type`,
  `true_type`, `false_type`, `size_type`, `anyint_type`,
  `anynum_kind`, `symbol_type`, `string_type`, `tuple_type`,
  `anytype_type`, `function_type`) moved onto `TypeWorld` as
  members (default-init to `nullptr`; populated by `initialize()`
  at `FA::analyze` entry). The `extern` block in `fa.h` removed,
  including two stale entries (`fun_type`, `fun_symbol_type`) that
  were declared but never defined or used. The 17 file-static
  definitions and the 4 chained nulls in `fa_reset()` are gone.
  Reference rewrites: ~109 in `fa.cc` and 8 external (4 in
  `clone.cc`, 2 in `codegen/cg.cc`, 2 in `llvm.cc`) plus 30 in
  `testing/lattice_test.cc`. All driven by word-boundary sed
  passes so collisions with `sym_nil_type` / `sym_unknown_type` /
  `sym_void_type` were avoided. Verified: `./ifa --test` (52/0),
  full `make test` (15 phases clean), `make test_llvm`,
  `./test_pyc` (73 pass / 2 expected fail), 20× multi-fixture
  `fa-converge` (0 crashes).
- [x] **Reentrancy step 4: sink id counters.** Done June 2026.
  All four counters (`avar_id`, `aedge_id`, `creation_set_id`,
  `entry_set_id`) moved onto `FA` as `int` members defaulting to
  1. The 4 file-statics in `fa.cc` and the `fa_reset()` reset
  line are gone. Only 5 usage sites — all in object constructors
  — needed redirection to `fa->X_id++`. Constructors run inside
  `FA::analyze`'s call tree (where the global `fa` pointer is
  set), so the redirection is safe. As the CLEANUP item
  predicted, this was the cheapest reentrancy step to land.
  Verified: `./ifa --test` (52/0), full `make test` (15 phases
  clean), `make test_llvm`, `./test_pyc` (73 pass / 2 expected
  fail), 20× multi-fixture `fa-converge` (0 crashes).
- [-] **Reentrancy step 5: remove global `fa` and `pdb`.**
  Deferred June 2026 — no concrete multi-FA use case justifies
  the cost. Steps 1-4 already gave each FA instance ownership of
  its analysis state; what remains is signature-level
  independence from the singleton `FA *fa` / `PDB *pdb`. That
  would touch ~430 references across 10 files (~50 functions
  already accept `FA*`; ~380 implicit-global uses remain). The
  AUDIT explicitly warns "Don't try to do this in one PR."
  Surface-area inventory, migration shapes (thread `FA*` /
  back-pointers on IR objects / hybrid), and what it would
  unblock (true concurrent analyses, embedded IFA usage, the
  graph.cc globals cleanup below) are preserved in
  [../notes/005-singleton-fa-and-pdb.md](../notes/005-singleton-fa-and-pdb.md).
  ([AUDIT §2.1](AUDIT.md#21-the-non-reentrant-set))
- [-] **Move `graph.cc` globals into a config object.** Deferred
  alongside reentrancy step 5. The graph subsystem reaches the
  current FA via the global `::fa`; threading FA through
  `graph()` naturally drags the graph-specific globals
  (`graph_fun[80]`, `graph_var[80]`, `graph_type`,
  `fgraph_frequencies`, `fgraph_constants` from `graph.h:13-17`)
  into a config struct, so this work is gated on step 5. See
  [../notes/005-singleton-fa-and-pdb.md](../notes/005-singleton-fa-and-pdb.md).
- [x] **Fix `Vec::set_add_internal` pointer-bucket hashing in
  plib.** Done June 2026. Combined option A + option B from
  [../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md):
  added `PointerHash<C>` trait in `ifa/common/vec.h` with
  explicit specializations on `c->id` for the six id-bearing
  pointer types (`AVar`, `AEdge`, `EntrySet`, `CreationSet`,
  `Sym`, `Fun`); `set_add_internal` / `set_in_internal` now
  index via the trait. Also added a `sorted_view(Vec<C*>&)` free
  function in `analysis/fa.h` (option A) as the non-mutating
  alternative to the in-place `qsort_by_id` discipline; no call
  sites migrated in this PR (deferred — see issue 010). Result:
  `fa-converge` is byte-identical across 5+ runs of every
  fixture (including `nested_iterator`, the one issue 009
  surfaced). The remaining deferred work — the API rename
  (`Vec::n` → `Vec::capacity`, add `Vec::size`) and the
  migration of the 17 `qsort_by_id` call sites to `sorted_view`
  — is filed as [issue 010](../issues/010-vec-set-api-cleanup.md).
  Verified: `./ifa --test` (52/0), full `make test` (all
  phases clean), `make test_llvm`, `./test_pyc` (73 pass / 2
  expected fail), 5× determinism check on every fa-converge
  fixture. ([AUDIT §3.4](AUDIT.md#34-the-deeper-fix))

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
