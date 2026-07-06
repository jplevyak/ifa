# Issue 032: Actionable findings from the 2026-07 fa.cc semantic survey

**Status:** open (tracking umbrella). Full analysis and rationale
in [../analysis/FA_SEMANTIC_SURVEY.md](../analysis/FA_SEMANTIC_SURVEY.md)
(labels B1–B5, S1–S9, P1–P4 refer to that document). This issue
tracks only the items concrete enough to land as individual fixes;
check them off here with the closing commit.

## Confirmed bugs

- [x] (`7142949`) **B1 — canonical hash overwrite** (`fa.cc:530`,
  `fa.cc:3726`): `h = ptr * prime` in a loop keeps only the last
  element; both hash-cons tables (`cannonical_atypes`,
  `cannonical_setters`) collide on last-element groups. Use
  `combine_hash` (fa.h:50). Land alone; expect golden churn from
  changed table iteration order, re-run flake checks (issue 021
  class) before/after.
- [x] (`55958ca`) **B2 — precision tables indexed by `num_kind`**
  (`fa.cc:417-423`, and the same in `num.cc:695,700`):
  `int_type_precision[]`/`float_type_precision[]` are
  `num_index`-indexed; using the kind enum makes int⊕float
  coercion return the float operand's width unconditionally and
  leaves the widening branches dead. Latent for pyc (float64
  only), wrong for f32 / V. Fix both sites together + V-side
  mixed-width test (`int64 + float32` must widen to float64 per
  `coerce_num`'s own comment).
- [x] (`1c22da6`) **B3 — range-for over growing Accum**
  (`build_setter_marks` fa.cc:3644/3646, `back_reaching`
  fa.cc:4081): truncated transitive closures (under-splitting) +
  realloc UB. Same class as the already-fixed `build_type_marks`
  one-hop cap (comment at fa.cc:3598); fix identically with
  index-based loops.
- [x] (`1c22da6`) **B4 — extend_analysis stage 4 never `clear_marks`**
  (fa.cc:4205-4223): mark contamination across confluences within
  the pass, and stale `mark_map`s surviving into the converged
  state when stages 4/5 find nothing. Add `clear_marks(acc)` per
  iteration. (Consider folding in the stage-4 restructure — P3 —
  while touching it: seed once, collect once, split once.)
- [x] (`1c22da6`) **B5 — `split_edges` null edge target on constant actuals**
  (fa.cc:3459-3460): `cs_es_map` keyed by constant-stripped
  `out->type` CSs but probed with the raw `out->v[0]`, which can
  be a constant CS → `ee->to = nullptr`. Reachable via
  `SPLIT_DYNAMIC` (`split_for_violations`). Probe
  `out->type->v[0]` or fall through to the general loop.

## Smaller hardening items (from the survey's S/P sections)

- [x] (`e39d8db`) **S1** — extract the shared propagation tail of
  `update_in` / `flow_var_type_permit` / `flow_var_permit_pred`
  (the permit variants currently omit the `is_if_arg` EntrySet
  resume).
- [x] (`e39d8db`) **S2** — assert or restructure the blanket
  `arg_of_send.add(result)` re-trigger registration
  (fa.cc:1707-1709) so snapshot-style prim transfers can't be
  emitted without a resume path; turn `record_arg`'s
  `assert(s->has.n == cs->vars.n)` into a type_violation.
- [x] (`e39d8db`) **S3** — invariants comment at `clear_avar` (what survives
  a pass and why) + debug assertion in `remove_unused_closures`
  that a closure CS consumed by a live call site has
  `closure_used` set. (Comment half landed; the assertion half
  needs a consumed-by backlink that doesn't exist yet -- tracked
  as the open item below.)
- [x] (`e39d8db`) **S5** — normalize `P_prim_coerce`'s operand indexing to
  the `o = rvals[0]==sym_primitive ? 2 : 1` convention (or prove
  only the 3-rval form is emitted).
- [x] (`e39d8db`) **P2** — clear or cap `AVar::match_cache` across passes.
- [x] Makefile: CORRECTED on investigation -- the main object
  groups already generate AND include `.d` files; the one real gap
  was `IFA_TEST_OBJS` missing from ifa's `-include` (ifa-test could
  go stale on header changes). Fixed. `make clean` after
  header-layout changes remains the safe habit per MEMORY.md.
- [x] `ifa-test` outside `ifa/`: CORRECTED -- it already exits 1;
  the false-green was the caller's grep pipeline. Message upgraded
  to say ERROR and point at the cwd requirement / --fixtures-root.

### Remaining open in this issue

- [ ] **S3 (assertion half)**: a debug check that a closure CS
  consumed by a live call site has `closure_used` set needs a
  consumed-by backlink from call sites to closure CSs that the
  data model doesn't carry today. Blocked on adding that link (or
  an equivalent post-convergence scan); the invariants comment
  landed.

## What this unblocks

Splitter correctness (B3/B4) directly affects FA precision on
setter-heavy OO code; B5 removes a latent crash class in the
violation-driven splitter; B1/P2 are the cheap wins for FA time on
larger programs; B2 matters before anyone relies on f32 or the V
numeric tower. The S-items convert three "you must remember"
protocols (re-trigger registration, pass-persistence, propagation
tails) into checked or single-sourced code — the 030 fixpoint bug
and this session's survey both trace to exactly those protocols.
