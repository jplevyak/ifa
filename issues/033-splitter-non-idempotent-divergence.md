# Issue 033: Splitting loop has no fixed point on some inputs (non-idempotent, order-dependent split decisions)

**Status:** open. Divergence is *mitigated* (stall guard, commit
`21dbdad4` on the pyc side of the tree) so affected programs
terminate quickly instead of timing out; the root cause — split
decisions that are not idempotent across passes — is untouched.
**Affects:** `ifa/analysis/fa.cc` — `extend_analysis()` and every
`split_*` stage it drives; `analyze_to_convergence()`'s outer loop
contract.
**Related:** [032-fa-survey-findings.md](032-fa-survey-findings.md)
(survey umbrella; B4/P3 fixed one instance of the same class inside
stage 4), closed
[009-fa-violations-nondeterminism.md](closed/009-fa-violations-nondeterminism.md)
and [021-v2-call-arg-swap.md](closed/021-v2-call-arg-swap.md) (the
"iteration-order dependence" family this belongs to),
[034-pygasus-update-display-assert.md](034-pygasus-update-display-assert.md)
(crash unmasked by the mitigation).

## Symptom

Five shedskin-corpus examples (fysphun, kmeanspp, pygasus, pylife,
stereo — pyc issue 025's "compile timeout" bucket) never finished
FA: `analyze_to_convergence()` looped with per-pass cost growing
superlinearly. `IFA_PASS_LIMIT` (100) exists but was unreachable in
wall time.

Measured trajectory on fysphun (`pyc -v`, with the ess/css/violation
counts added to the per-pass report in `21dbdad4`):

```
pass  7: 199 ess, 548 css,  84 violations   <- best ever reached
pass  8: 210 ess, 550 css,  99 violations   \
 ...          (plateau at 99)                | 8 passes, no improvement
pass 15: 224 ess, 560 css,  99 violations   /
pass 18: 367 ess, 578 css, 226 violations   \
pass 19: 497 ess, 578 css, 326 violations    |
pass 22: 850 ess, 578 css, 598 violations    |  oscillation: ess and
pass 23: 589 ess, 578 css, 397 violations    |  violations swing up
pass 24: 875 ess, 578 css, 617 violations    |  and down together
pass 26: 878 ess, 578 css, 620 violations    |
pass 27: 617 ess, 578 css, 419 violations   /
pass 26 cost 9.9s, pass 27 cost 17.9s (98%/77% in extend)
```

The state never exactly repeats (no simple cycle), it *oscillates
with drift* — so neither a pass cap nor exact-state cycle detection
catches it at useful cost.

## Root cause (as far as traced)

The outer loop `do { flow-to-fixpoint } while (extend_analysis()
|| reanalyze(...))` is only a fixed-point iteration if
`extend_analysis()`'s split decisions are **monotone and
idempotent**: a split made in pass k should stay made, and
re-presenting the same imprecision should not split again. Neither
holds:

1. **Split state is per-pass.** `clear_splits()` zeroes
   `EntrySet::split` / `CreationSet::split` every pass, and
   `clear_results()` throws away the flow state that justified the
   split. The next pass re-derives confluences from scratch on the
   *new* contour set and makes fresh decisions.

2. **Decisions are order-dependent.** Which confluence gets split
   first determines which ESs exist when the next confluence is
   examined; `split_ess_for_type`, the mark stages, and
   `split_for_violations` all iterate hash-ordered `Vec`-sets whose
   layout shifts as ess/css ids change (the issue 009/021 iteration-
   order family). Stage 4's pre-B4 shape — per-confluence mark
   seeding that leaked marks across iterations, fixed in the survey
   (032 B4/P3) — was one concrete instance; the fysphun trace shows
   the same disease at loop granularity: passes 22..27 REMOVE
   contours a previous pass added (ess drops from 850 to 589), then
   re-add different ones.

3. **`split_for_violations` splits blind.** Stage 5 splits on every
   violation imprecision each pass with `SPLIT_DYNAMIC`, whether or
   not the same violation was already split for in a previous pass
   and survived. When a violation is NOT resolvable by contour
   refinement (fysphun's residue is numeric-coercion-shaped, see
   §Starvation), stage 5 manufactures contours forever.

## Additional finding: the `||` short-circuit starves `reanalyze`

`while (extend_analysis() || if1->callback->reanalyze(...))` only
consults the frontend's reanalysis callback when the splitter
reports no work. On a diverging input the splitter ALWAYS reports
work, so the pyc numeric-coercion annotator — which existed
precisely to resolve fysphun's class of violations — never ran.
With the stall guard forcing the splitter to rest, the annotator
resolves **all 99 residual violations** and fysphun's FA converges
(0.94s total, was 300s+ timeout). Any future scheduling change
should preserve this fairness property or make it explicit
(e.g. run `reanalyze` between passes rather than only at splitter
exhaustion).

## Mitigation in place (not the fix)

`FA::stall_limit` (`IFA_STALL_LIMIT`, default 8): if the violation
count fails to improve on its best for that many consecutive
passes, `extend_analysis` sets `pass_limit_hit` and stops
splitting. Zero-violation passes (pure precision splitting) never
advance the counter, so converging programs are unaffected. This
guarantees wall-clock termination and, via the starvation fix-by-
accident above, actually *converges* some previously-diverging
inputs. It does NOT make splitting deterministic or idempotent.

## Proposed real fix

Make split decisions persistent and keyed, so re-running the
splitter is idempotent:

- Give each split a canonical key — e.g. (fun, dispatch position,
  type partition) for ES splits, (creation site, field, partition)
  for CS splits — interned like `cannonical_atypes`.
- Keep a cross-pass `Map<key, generation>`; a key that was already
  split for is never split again unless the partition it produced
  was itself refined (generation bump).
- `split_for_violations` additionally records which violations it
  split for; a violation that survives its own split verbatim is
  marked non-refinable and excluded from stage 5 thereafter.
- Iterate confluences in a canonical order (sorted by stable ids,
  the `compar_*` idiom used elsewhere) so the surviving decisions
  are order-independent.

With idempotent splits the contour set grows monotonically, the
outer loop is a genuine fixed-point iteration, and both
`IFA_PASS_LIMIT` and the stall guard become true safety nets
instead of load-bearing.

## Verification plan

- fysphun/kmeanspp/pylife under `-v`: pass count bounded, ess/css
  monotone nondecreasing, no oscillation, outcome unchanged or
  better vs the stall-guard baseline.
- Full suites both backends + ifa-test, plus the fa-converge
  fixtures (issue 003 sidecar) which lock per-pass stage events.
- Re-run the issue 009/021 flake checks: with canonical ordering,
  repeated runs must produce identical pass counts and violation
  sets.

## What it unblocks

- The remaining diverging inputs (kmeanspp, pylife residues).
- Trustworthy FA telemetry (pass counts become stable metrics).
- Removes the risk that stall_limit=8 clips a legitimately slow
  but converging program (none known today; the guard only fires
  on nonzero-violation plateaus).

---

# Detailed implementation design (the split-decision ledger)

Everything below is written to be implementable without prior
familiarity with the splitter. File/line references are as of
commit `21dbdad4`.

## D0. Background: how splitting actually works today

Read this first; the design hangs off these five mechanisms.

1. **Contour storage.** EntrySets live in the per-function list
   `Fun::ess` and are NEVER deleted. The global `fa->ess` is
   rebuilt every pass by `collect_results()` (fa.cc:3064) from
   `fa->entry_set_done` — it lists only the ESs *reached* this
   pass. That is why `fa->ess.n` can DROP between passes while
   total allocated contours only grow: an oscillation is the flow
   re-derivation reaching a different subset of `Fun::ess` each
   pass.

2. **Edge → ES routing.** When an `AEdge` needs a target,
   `make_entry_set` (fa.cc:1052) tries, in order:
   `check_split(e, ...)` (fa.cc:1031 — follows
   `e->from->split->out_edge_map` when the caller ES was itself
   split this pass), then `find_best_entry_sets` (scores every
   existing ES of the callee via `entry_set_compatibility`, which
   consults `EntrySet::filters` through `check_edge`), then
   creates a fresh ES.

3. **Split path A — filtered (already half-keyed).**
   `split_edges` (fa.cc:3546, the `SPLIT_DYNAMIC` route) builds a
   `Map<MPosition*, AType*> filters` per receiver CreationSet and
   calls `find_or_make_filtered_entry_set` (fa.cc:3536), which
   REUSES an existing ES of the function whose filters are
   compatible (`!some_disjunction`) and only otherwise allocates
   (recording the filters on the new ES). This path has a natural
   persistent key; its defect is only that nothing stops the same
   confluence from being re-collected and re-acted-on after
   `clear_results()`.

4. **Split path B — greedy grouping (the unkeyed one).**
   `split_entry_set` (fa.cc:3598) partitions an ES's incoming
   edges into pairwise-compatible groups
   (`edge_type_compatible_with_edge`), seeds each group from
   `do_edges[0]` (greedy — group shape depends on iteration
   order), and parks each group in a NEW bare ES via
   `make_entry_set(x, ..., es, e->to)`. The only record is
   `new_es->split = orig_es`, which `clear_splits()` erases at the
   start of the next `extend_analysis`. Nothing prevents the next
   pass from regrouping the same edges differently.

5. **CS splits.** `split_css` (fa.cc:~4130) groups a CreationSet's
   defs by setter-equivalence (`same_eq_classes(v->setters, ...)`)
   and moves each group into a `new CreationSet(cs)` with
   `new_cs->split = cs`. Same per-pass amnesia as path B. NOTE:
   setter objects are hash-consed in
   `type_world.cannonical_setters`, which `clear_results()`
   CLEARS — setter pointers are therefore unusable in any
   cross-pass key.

**Stable-identity inventory** (safe to key on across passes):
`Fun*`, `Sym*`, `Var*`, `PNode*`, `MPosition*` (globally interned,
pattern.h), and `AType*` (hash-consed in
`type_world.cannonical_atypes`, which `clear_results()` does NOT
clear — verify: fa.cc:3861 clears only `cannonical_setters`).
**Unstable** (never key on): `EntrySet*`/`CreationSet*` contents,
`AVar->out` object identity mid-pass, setters, anything cleared by
`clear_avar`/`clear_es`/`clear_cs`.

## D1. The ledger data structure

Add to `FA` (fa.h), next to the type_world:

```cpp
// Issue 033: persistent record of split decisions, so re-running
// the splitter over a re-derived flow state is idempotent.
// Key components are all interned/stable (see issue 033 D0):
// fun, the FAPassStage that made the split, the argument
// position driving it, and the canonical AType partition
// assigned to the product contour. Never cleared between passes
// (only in FA::initialize / a fresh analyze()).
struct SplitDecision {
  Fun *fun;
  int stage;          // FAPassStage of the split site
  MPosition *pos;     // confluence/dispatch position (interned)
  AType *partition;   // canonical (cannonical_atypes) filter type
  int pass_made;      // analysis_pass at record time (diagnostics)
  EntrySet *product;  // ES created/selected (nullptr for CS splits)
};
```

Hashing: all four key fields are pointers with stable identity.
Use the house position-sensitive accumulation idiom (see the AType
hash in fa.cc ~590: `h += (uint)(intptr_t)ptr *
open_hash_primes[i % 256]`, primes from common/vec.h:188) over the
four fields, into a `ChainHash` (the `cannonical_atypes` pattern,
fa.h:410) or a plain `Map<uintptr_t, Vec<SplitDecision*>>`;
collisions resolved by comparing the four fields. Add:

```cpp
  SplitDecision *ledger_find(Fun*, int stage, MPosition*, AType*);
  SplitDecision *ledger_add (Fun*, int stage, MPosition*, AType*, EntrySet*);
  int dup_split_attempts;   // per-pass diagnostic counter
```

Debug-assert on insert that `partition` is canonical:
`assert(partition == fa->type_world.cannonical_atypes.put(partition))`.

## D2. Stage A — record-only + observability (land first, no behavior change)

1. Thread `int stage` (the `FAPassStage` values already defined for
   the issue-003 sidecar) into `split_edges`, `split_entry_set`,
   and `find_or_make_filtered_entry_set` call sites.
2. In `find_or_make_filtered_entry_set`: for each (position,
   AType) pair in `filters` that differs from `orig_es->filters`,
   probe the ledger. On miss: `ledger_add`. On hit:
   `++fa->dup_split_attempts` (do NOT change behavior yet).
3. In `split_entry_set`, when a group lands in a new ES
   (`x->to != es` after `make_entry_set`): compute the group's
   partition (see D4) and record/probe the same way.
4. Print `dup_splits=%d` in the `-v` PASS line (fa.cc:4509)
   and reset the counter in `initialize_pass`.

Acceptance for stage A: suites unchanged (record-only);
`pyc -v fysphun.py` shows `dup_splits > 0` beginning around pass 8
(the plateau) — this validates the key shape against the real
divergence before any enforcement. If dup_splits stays 0 on
fysphun, the key is wrong — stop and re-derive (most likely the
partition ATypes differ across passes because constants haven't
been stripped; see D4 note).

## D3. Stage B — enforce on the filtered path (SPLIT_DYNAMIC)

In `split_edges` (fa.cc:3546), before building `cs_es_map`:

```cpp
// Probe: have we already split this fun at this position for
// this receiver partition? If every receiver CS's key hits, the
// contours already exist; routing is find_best_entry_sets' job.
bool all_known = true;
for (CreationSet *cs : av->out->type->sorted)
  if (!fa->ledger_find(es->fun, stage, p, make_AType(cs))) { all_known = false; break; }
if (all_known) { ++fa->dup_split_attempts; return 0; }
```

and record each miss when its ES is made. `make_AType(cs)`
(fa.cc:219, decl fa.h:577) returns the canonical singleton, so the
key matches across passes as long as the receiver CS set is the
same — which
is exactly the "no new information" case we want to stop acting
on. If the CS set grew (new CS from a CS split), its key misses
and the split proceeds: that is the legitimate-refinement case.

## D4. Stage C — put split_entry_set's groups on filters

This is the core change. In `split_entry_set`'s group loop
(fa.cc:3652), replace the bare
`make_entry_set(x, new_edges, es, e->to)` parking with:

1. Find the confluence position `p` for `av` in `es->args` (the
   same `form_MPositionAVar` lookup `split_edges` uses; hoist it).
2. For the group `these_edges`, compute the partition:
   `AType *part = bottom; for (x : these_edges) part =
   type_union(part, x->args.get(p)->out->type)`. Note the `->type`
   accessor: every canonical AType carries its CONSTANT-STRIPPED
   view (computed during canonicalization, fa.cc ~570-600), and
   both `split_edges` and the compatibility predicates already
   key off it. Using the raw `->out` instead would make partitions
   pass-unstable (constant CSs like "3" vs int64 re-derive
   differently under the constant cap) — this is the most likely
   silent mistake in the whole implementation; the stage-A
   dup-counter check on fysphun exists to catch exactly it.
3. Probe ledger with (es->fun, stage, p, part). Hit: do NOT make
   a new ES; instead route the group's edges to the hit's
   `product` ES via `set_entry_set` (it exists — product ESs are
   never deleted) and count `dup_split_attempts`. Miss:
   `filters = es->filters + (p -> part)`, call
   `find_or_make_filtered_entry_set(es, filters)`, route the
   group there, and `ledger_add(..., product)`.
4. Keep `new_es->split = orig_es` exactly as today (check_split
   and the intra-pass short-circuits depend on it).

The greedy group SEEDING stays, but becomes harmless: whatever
order groups form in, each group's (position, partition) key is
canonical, so a re-derived pass either reproduces the same keys
(all hit; zero new contours) or produces genuinely refined ones.

## D5. Stage D — CS splits

Key shape for `split_css`: (`cs->sym`, stage, position = nullptr,
partition = canonical AType of the compatible_set's VALUE types)
is NOT sufficient — two def-groups can share value types and
differ only by setters, and setters aren't stable. Use instead the
sorted def-Var identity signature: hash of the sorted
`v->var->sym->id` list of `compatible_set`, stored in a parallel
FA-level `Map<uintptr_t, int>` (advisory, CS-only ledger). On
re-derivation the same def partition hashes identically; a hit
skips the split. This is deliberately weaker (advisory only) —
if stage A observation shows CS splits are not a driver of the
fysphun-class divergence (expected: the trace shows css stops
growing at 578 while ess oscillates), stage D can be deferred
indefinitely.

## D6. Stage E — split_for_violations non-refinability

Add `Map<Var *, int> violation_split_attempts` to FA (persistent).
In `split_for_violations` (fa.cc:4369): for each imprecision AVar
`av`, bump `violation_split_attempts[av->var]`. If the count
exceeds 2 (two full split attempts for the same underlying Var
that both failed to remove its violation), drop it from
`imprecisions` before splitting and log
`LOG_SPLITTING "[nonrefinable] var %d"`. Key on `Var*` (stable),
not `AVar*` (contour pointer changes as ESs are superseded).
This directly kills the stage-5 "manufacture contours forever"
arm even before stages C/D land, and is ~15 lines — it can be
landed second, right after stage A, for early corpus relief.

## D7. Stage F — ordering audit (determinism hardening)

**Status: audit performed 2026-07-09; two real gaps found and
fixed, one original claim in this section corrected, one
hypothesized deeper bug investigated and refuted.** Every
`collect_*`/`split_*` function feeding `extend_analysis` was read
end to end and traced against its consumer(s) to determine whether
consumption is (a) order-independent (membership-only test, or a
final canonicalizing sort makes traversal order irrelevant) or (b)
order-sensitive (first-match short-circuit, e.g.
`split_entry_set`'s `if (es->split) return 0`, or a greedy seed
like `do_edges[0]`/`starter_set[0]`). Findings, corrected from the
original draft of this section:

- **`collect_type_confluences` — ORIGINAL CLAIM WAS WRONG.** It
  already ends with `confluences.set_to_vec(); qsort_by_id
  (confluences);` (fa.cc:3485-3486) — verified by direct
  `git blame` (present since Feb 2026, long before this session).
  No fix needed; correcting the record here since the design doc
  is meant to be trustworthy source material for whoever implements
  D1-D6.
- **`collect_violation_imprecisions` — CONFIRMED, FIXED.** Ended
  with a bare `imprecisions.set_to_vec();`, no sort. Its output
  feeds `split_ess_for_type`'s and `split_with_type_marks`'s
  straight `for (AVar *av : imprecisions)` loops, both of which
  call `split_entry_set` per element — order determines which
  `AVar` "drives" a given ES's split this pass (subsequent AVars
  hitting an already-split ES short-circuit via `es->split`).
  Fixed: `qsort_by_id(imprecisions);` added (fa.cc, in
  `collect_violation_imprecisions`).
- **`collect_cs_setter_confluences` — NEW FINDING, NOT IN THE
  ORIGINAL LIST, FIXED.** Every sibling collector in this file
  (`collect_type_confluences`, `collect_cs_marked_confluences`,
  `collect_es_marked_confluences`, `collect_setter_confluences`)
  sorts its output; this one didn't. Its sole consumer,
  `split_for_setters_of_setters`, feeds the result straight into
  `compute_setters(..., AKIND_SETTER)` — and that function is
  called every pass from BOTH `extend_analysis` stage 3 and stage
  4, so this was a live, frequently-exercised gap, not a
  theoretical one. Doubly notable: the enclosing function already
  carries a comment describing a PRIOR order-dependence bug fixed
  in this exact spot (issue 007/032 — confluences vector clobbered
  across stages) — this collector's missing sort is a sibling
  defect the 007/032 fix didn't happen to touch. Fixed:
  `qsort_by_id(setters_confluences);` added (fa.cc, in
  `collect_cs_setter_confluences`).
- **The `Accum<AVar*> avs` fed to `split_for_setters` (stages 3
  and 4) — ORIGINAL CLAIM WAS WRONG; investigated in depth,
  concluded NO FIX NEEDED.** The original draft asserted this
  needed sorting. Tracing it fully: `avs`'s insertion order comes
  from `update_setter`'s recursive backward propagation
  (fa.cc:3953), which is a **standard memoized reachability
  closure** — for a fixed setter `s`, the early-return
  `if (av->setters->in(s)) return 0` only prunes an AVar whose
  backward recursion has ALREADY completed via some other path (a
  visited-set check, not a value chosen among competing
  alternatives), so the final `av->setters` membership is
  provably independent of traversal order. Its companion,
  `recompute_eq_classes` (fa.cc:4010), has a general "reparition"
  code path that WOULD be order-sensitive for a multi-element
  input, but its one and only call site (`compute_setters`,
  fa.cc:4048) always passes singleton `Setters` objects, for which
  that path is unreachable (a singleton either already has a
  `setter_class` and the function no-ops, or doesn't and gets one
  assigned with no other element to reparition against). `avs`'s
  own traversal order therefore doesn't affect what gets computed;
  its only consumer, `collect_setter_confluences`, already
  produces sorted, membership-only output regardless. Left
  unchanged (a code comment could be added here to save the next
  reader this investigation, but is not required for correctness).
- `split_edges`'s `all_edges` (fa.cc:3549 `qsort_by_id`) and
  `cs_es_map` construction over `av->out->type->sorted` (sorted by
  AType canonicalization) — confirmed fine as originally claimed.
- `split_entry_set`'s `all_edges` (fa.cc:3609 `qsort_by_id`) —
  confirmed fine: the edge LIST is canonically ordered, so
  `do_edges[0]`'s greedy seed is at least deterministic run-to-run.
  Note this does NOT make the GROUPING outcome itself
  order-independent of split HISTORY — that's what D1-D4's ledger
  is for; D7 only guarantees the iteration order feeding the
  greedy seed is stable, not that the greedy algorithm's grouping
  choice is optimal or history-independent.
- `split_css`'s `starters` parameter — confirmed fine: it arrives
  pre-sorted from its caller (`split_for_setters` passes
  `collect_setter_confluences`'s already-sorted `setter_starters`),
  and `split_css`'s own internal `css` local is separately sorted
  (fa.cc:4131-4132) before its `starter_set[0]` greedy grouping —
  same caveat as `split_entry_set` above (stable order, not
  history-independent grouping).
- **`fa->ess` itself — CONFIRMED foot-gun, FIXED.** Rebuilt every
  pass by `collect_results()` (fa.cc:3064) from
  `fa->entry_set_done` in WORKLIST-COMPLETION order (not sorted) —
  unlike the sibling `fa->css`, which IS sorted two lines later in
  the same function (fa.cc:3083). Every CURRENT direct consumer of
  `fa->ess` (`collect_type_confluences`, `collect_cs_marked_
  confluences`, `collect_cs_setter_confluences`, `collect_var_
  type_violations`, `fa_coerce_numeric_confluences`,
  `fa_dump_types`) either canonicalizes its own output before
  return or performs an order-independent per-(ES,Var) test/action
  with no cross-element interaction, so this was NOT a live bug —
  but it was exactly the kind of foot-gun that produces one
  silently: a future consumer that assumed sorted order (as every
  sibling collector correctly does) or that performed a greedy/
  first-match pass directly over `fa->ess` would reintroduce this
  whole class of bug with no local signal that anything was wrong.
  Fixed: `qsort_by_id(fa->ess);` added in `collect_results`,
  immediately after the `fa->ess.add(es)` loop and its `fa->funs`
  sort, before `fa->css` is derived from `fa->ess` (so `fa->css`'s
  own construction now also walks `fa->ess` in canonical order,
  though its output was already independently sorted regardless).

This stage doesn't change what converges; it makes repeated runs
byte-identical so the issue-003 sidecar fixtures can lock pass
counts. **Verified**: three consecutive `pyc -v` runs of fysphun
produce IDENTICAL ess/css/violation counts on every pass (only
wall-clock timing numbers differ in a byte diff) — before this
fix, this had not been checked run-to-run; re-verified after
landing the `fa->ess` sort with the same result. Full corpus sweep
run three times across the three D7 fixes (the two collector
sorts, then again after the `fa->ess` sort): identical 22-compiled/
55-failed split each time, same member set (ant, astar, genetic,
mandelbrot, nbody, neural2 compile clean; bh, block, brainfuck,
chess, collatz, hq2x, life, mao, neural1, pisang, richards, rsync,
sudoku1, sudoku4, voronoi, webserver compile with warnings) — no
regression from any of the three fixes. The two examples that
showed timeout/warning changes across EARLIER sweep runs (oliva2,
stereo, from before this D7 work started) were independently
confirmed to fail IDENTICALLY at the pre-D7 commit (`e7b76a37`)
across four repeated runs each, ruling out the D7 fixes as the
cause — they are pre-existing, unrelated gaps (oliva2: a
primitive-argument type mismatch; stereo: a timeout still gated by
the stall guard from the mitigation, not touched by D7).

## D8. What NOT to change

- `clear_splits` / `EntrySet::split` / `check_split`: intra-pass
  routing machinery; the ledger complements it, does not replace
  it.
- `find_best_entry_sets` scoring: product ESs carry filters, so
  existing compatibility logic already prefers them.
- The stall guard: keep as a safety net. After stages A–E it
  should never fire; add `assert(!fa->pass_limit_hit)` to the
  fa-converge FIXTURES only (not production) to catch regressions.
- `reanalyze` scheduling: unchanged; with splitting idempotent the
  splitter reports no-work as soon as contours stabilize, and the
  annotator runs as designed.

## D9. Termination argument (for the reviewer)

With enforcement on, a split happens only under a ledger MISS.
Every key's partition either equals a previous pass's (hit — no
work) or is new. New keys require new canonical ATypes at the
position, which requires the CS universe or the flow state at the
confluence to have genuinely changed. CS growth is bounded by
`split_css`'s def partitions (each split strictly shrinks
`cs->defs`, well-founded), and per-(fun, pos) partitions are
subsets of the CS universe, so the key space is finite for a
finite CS universe. Total splits are therefore finite; the outer
loop terminates without the stall guard. (The stall guard stays
because D5 is advisory and because "finite" can still be "large".)

## D10. Change inventory & sizing

| File | Change | Est. |
|---|---|---|
| `analysis/fa.h` | SplitDecision, FA::ledger fields + 2 methods, dup counter | ~60 lines |
| `analysis/fa.cc` `find_or_make_filtered_entry_set` | probe/record | ~20 |
| `analysis/fa.cc` `split_edges` | all-known probe (D3) | ~15 |
| `analysis/fa.cc` `split_entry_set` | group partition + filters routing (D4) | ~50 |
| `analysis/fa.cc` `split_for_violations` | non-refinable marking (D6) | ~15 |
| `analysis/fa.cc` `split_css` | advisory signature map (D5, deferrable) | ~20 |
| `analysis/fa.cc` `initialize_pass` / PASS print | counter reset + report | ~5 |
| ordering audit (D7) | **DONE** (2026-07-09): `qsort_by_id` added in `collect_violation_imprecisions`, `collect_cs_setter_confluences`, and `collect_results` (`fa->ess`) | 3 lines landed |
| tests | fa-converge fixture + determinism double-run + corpus greps | — |

Land order: A (record-only) → D6 → B → C → E → (D5 if ever
needed). D7 landed out of order (audit-only, no behavior change
beyond removing two nondeterminism sources) since it was safe to
verify independently of the ledger machinery. Each lands with all
three suites (pyc C, pyc LLVM, ifa-test) plus the acceptance checks
below.

## D11. Acceptance checks per landing

- **A**: suites green; fysphun `-v` shows nonzero dup_splits on
  the plateau; dup_splits == 0 on 5 converging tests (e.g.
  fibheap_full, expr_evaluator, richards) — if converging tests
  show dups, the key is too coarse (would enforce wrongly later).
- **D6**: kmeanspp/pylife pass counts drop; no suite change.
- **B/C**: fysphun/kmeanspp/pylife: ess monotone nondecreasing
  across passes (grep the `-v` trail), stall guard no longer
  fires (`STALL LIMIT` absent from logs), outcomes unchanged or
  improved; full corpus sweep bucket counts not worse anywhere.
- **E**: two consecutive full runs of the ifa-test fixtures
  produce byte-identical sidecar event streams (issue 003
  machinery gives this for free — extend the harness to diff).
