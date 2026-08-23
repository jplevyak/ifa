# 113 — setter equivalence classing is a global, per-pass batch partition, so nothing incremental can be built on FA

**Status:** open, filed 2026-08-23. Extracted from
[111](111-FA-selective-invalidation-per-pass.md) M3, which is **blocked
on this** after seven attempts that all failed for this one reason.
**Affects:** `ifa/analysis/fa.cc` — `AVar::setter_class`,
`AVar::setters`, `Setters`, `TypeWorld::cannonical_setters`,
`compute_setters`, `recompute_eq_classes`, `split_eq_class`,
`same_eq_classes`, `sset_compatible`.

## The invariant, and why it is a problem

`AVar::setter_class` is "the smallest set of setter AVars which are
equivalent (same `->out`, equivalent `->setters`)" — a **partition over
all participating AVars**. It is established in exactly one place,
`recompute_eq_classes` (called from `compute_setters`), run in a
**batch over one pass's type confluences**. `same_eq_classes` then
asserts that every member of every live `Setters` set carries a class:

```c
for (AVar *av : *s) if (av) {
  assert(av->setter_class);
  sc1.set_add(av->setter_class);
}
```

With the current full per-pass reset this holds trivially: `clear_avar`
zeroes every `setters`/`setter_class`, so every set that exists was
BUILT this pass, and every member was therefore classed this pass. The
invariant is maintained by the reset, not by the machinery.

That makes the whole subsystem **incompatible with any form of state
carried across passes.** Three properties, each independently fatal:

1. **Membership is decided DURING a pass**, but any incremental scheme
   must decide what to preserve BEFORE it. A closure computed at the
   end of pass N cannot cover what enters a `Setters` set in pass N+1.
2. **Classing is GLOBAL.** Computing it from a subset gives a different
   partition. Measured: classing on demand from the two sets being
   compared makes `collatz` churn (18 passes and climbing where the
   batch converges in 6) and costs precision on a 4-line program
   (8 `mixed basic types` warnings against 1).
3. **Coverage widening does not help**, because the gap is not which
   AVars get classed — it is *when*, relative to set construction.

## Evidence: seven approaches, one cause

All from ifa/111 M3 (see that issue for the full trail):

| approach | outcome |
|---|---|
| Zero `setters`/`setter_class` on all AVars | assert — preserved AVars land in sets rebuilt this pass |
| Preserve both, plus the `cannonical_setters` interning table | assert — *cleared* AVars re-enter sets unclassed |
| `same_eq_classes` answers `false` for unclassed | no assert; refuses EntrySet merges, contours grow unbounded, no convergence |
| Pre-add every live `Setters` member to the closure | assert — membership is decided during the NEXT pass |
| Backward invalidation (clear whoever NAMES a cleared AVar) | assert |
| Run `compute_setters` over EVERY CS-contoured AVar | assert — `av#5979`, `setters=(nil)`, unclassed, as a MEMBER of another set |
| Lazy classing via `recompute_eq_classes` on demand | no assert; wrong partition, churn + precision loss (see 2 above) |

None of these failed because the invalidation closure was wrong.
ifa/111 M1 measured that closure at median 0%, p90 3%, max 16% of
AVars — the upside is real and is not what is blocked.

## Fix direction

Not chosen; this issue exists to hold the question rather than answer
it. Three shapes, roughly in increasing order of ambition:

1. **Make the invariant explicit and cheap to restore.** Give an
   unclassed AVar a well-defined lazy class that agrees with the batch
   result — which needs the batch's inputs, so it probably means
   keeping the confluence collection alive rather than recomputing
   from a pair.
2. **Make classing incremental**: maintain the partition under
   insertion/removal of a single AVar instead of recomputing per pass.
   This is standard union-find territory, and `split_eq_class` already
   does refinement — the missing half is merging.
3. **Make classing per-contour** so a `Setters` set cannot span a
   preserve/clear boundary at all. Cleanest for 111, but changes what
   the classes MEAN, so it needs its own precision measurement.

Any of these is FA redesign rather than an optimisation, and should be
justified on its own terms — not merely as a prerequisite for 111.

## Verification plan

- `same_eq_classes` no longer needs its assert, or the assert holds
  without a full per-pass reset.
- `IFA_SELECTIVE=1` (ifa/111) reaches convergence on `collatz` in the
  same pass count as `IFA_SELECTIVE=0`, and
  `ifa/tests/selective_diff.sh --corpus` reports zero divergence.
- Both suites unchanged; all five CI gates green.
- Precision unchanged corpus-wide (violation counts per program), since
  options 2 and 3 above can move it.

## What this unblocks

[111](111-FA-selective-invalidation-per-pass.md) directly — hq2x spends
48% of its FA time on four passes whose real closure is at most 6 AVars
out of ~100 000, and pyc loses to shedskin (31.0 s against 14.0 s) on
that program today.

More generally: **any** attempt to make FA incremental hits this first.
It is the reason the per-pass reset has to be total, which is the
premise ifa/111 set out to remove.

## Related

- [111](111-FA-selective-invalidation-per-pass.md) — blocked on this.
  Its M1 measurement, `foreach_avar`, the predicate-threaded
  `clear_results` and `ifa/tests/selective_diff.sh` all survive and are
  reusable here.
- [074](074-FA-cross-pass-oscillation-plan.md),
  [101](101-FA-first-time-forever-splitting.md) — convergence work that
  also lives in the splitter, and would interact with any reworking of
  the classes.
