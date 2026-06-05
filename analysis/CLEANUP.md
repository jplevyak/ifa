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
- [ ] **Wire `DEBUG_PRINT` into `log_tag` / `LOG_SPLITTING`.**
  `fa.cc:21` currently keys on `ifa_debug` and prints to stdout;
  the rest of the splitter logging goes through `log(LOG_SPLITTING, …)`.
  Pick one channel.
  ([AUDIT §1 item 5](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

---

## Tier 2 — scoped but needs care

Touches multiple files or has subtle correctness implications.
Land each as its own PR; run the full §8 checklist.

- [ ] **Buffer-overflow-prone `strcat` / `strcpy` in `graph.cc`.**
  Audit every fixed-size buffer: `hfn[512]`, `title[256]`,
  `id[80]`, `label[80]`, `name[256]`. Convert to bounded
  `snprintf` (mixed inconsistently today). Add a `safe_strcat`
  helper if it helps. All-internal inputs so likelihood is low,
  but a long symbol name silently corrupts the stack.
  ([AUDIT §1 item 10](AUDIT.md#1-headline-issues--in-order-of-likely-impact))
- [ ] **Implement `P_prim_meta_apply` and `P_prim_cast`** —
  currently `assert(!"implemented")` at `fa.cc:1672` and
  `fa.cc:1927`. The Python frontend can hit `P_prim_cast` in
  edge cases. If the path is genuinely unreachable from V and
  pyc, convert to a structured `fail()` with a diagnostic.
  ([AUDIT §1 item 8](AUDIT.md#1-headline-issues--in-order-of-likely-impact))
- [ ] **Add unit tests for the lattice ops** — pure functions
  with no `FA*` dependency once canonical types are constructed:
  `type_union`, `type_intersection`, `type_diff`,
  `type_cannonicalize`, `type_num_fold`. Highest-leverage
  testing work in the directory; unlocks future refactor
  safety. ([AUDIT §3.4](AUDIT.md#34-the-deeper-fix),
  [§8](AUDIT.md#8-things-to-verify-before-merging-an-fa-change))
- [ ] **Make `IFA_PASS_LIMIT` overflow loud.** `fa.h:11`,
  triggered at `fa.cc:3829`. Either raise the cap with
  justification or fail hard with a clear message on trip — silent
  acceptance of remaining violations is misleading.
  ([AUDIT §1 item 6](AUDIT.md#1-headline-issues--in-order-of-likely-impact))

---

## Tier 3 — high risk, multi-PR

Don't attempt without a stable test harness (so: only after
[issue 009](../issues/009-fa-violations-nondeterminism.md) is
closed). Each tier-3 item is its own multi-week project.

- [ ] **Fix [issue 009](../issues/009-fa-violations-nondeterminism.md)
  (non-deterministic violation count).** Follow the recipe in
  [AUDIT §10](AUDIT.md#10-suggested-first-steps-for-issue-009).
  Likely closes [issue 008](../issues/008-fa-crash-on-nested-iterator-shape.md)
  as a side effect. **Blocks every later tier-3 item** — until
  goldens are reliable, no large refactor can be validated.
  ([AUDIT §3](AUDIT.md#3-determinism--the-cause-of-issue-009))
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
