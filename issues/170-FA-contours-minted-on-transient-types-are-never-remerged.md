# 170 — re-derive contour decisions at convergence: the merge of equal contours

**Status: open on steps 2 and 3.** Step 1 landed 2026-10-12. Rewritten
2026-11-03; the three instances, the stop-condition firings and the
measurements are in git (`git show 123ac81:ifa/issues/170-FA-contours-minted-on-transient-types-are-never-remerged.md`).

## The rule

A contour exists because something observed a distinction that required
it. A distinction observed on pass-0 imprecision or on a half-populated
contour is a statement about the analysis, not the program, and a contour
whose demand has evaporated at convergence is unjustified. Retiring one
costs nothing in soundness: every pass re-derives types from bottom, and
only the DECISION persists.

## Step 1, landed: `merge_equivalent_entry_sets` (`PYC_ESMERGE=1`)

Runs as the last split stage, on a pass where no stage acted. Two
contours of one function merge when every AVar has the same converged
`out` in both, every creation point maps to the same CreationSets, the
filters and display agree, no AVar is coerced, setter classes are the
same, no violation is outstanding, nothing either creates carries a live
demand, and no ledger decision touched either this pass. The younger's
in-edges move to the older. Measured: verdicts identical on all 77,
EntrySets −2.7 %, compile time −7 %, `voronoi`'s `int.__add__` back to one
`[int64][int64]` contour. Three stop-condition firings each named a demand
the first version misread (a split made as a means, coercion, the setter
criterion) and none was answered with hysteresis.

## Step 2, open: routing on a momentarily-empty contour

`entry_set_compatibility` scores an edge against candidates' formal types
as they are mid-pass; at the top of a pass every formal is bottom, so a
wrong contour scores "less compatible" and can win, and the `ret → result`
flow `analyze_edge` installed for the first binding outlives the
rebinding (instances 2 and 2b: msp_ss's `ord('q')` edge, `bool.__eq__`'s
per-constant formal under `--fa-inline=0`). A codegen guard now refuses
the mismatch, so a valid program is rejected rather than miscompiled.

Two fixes, either sufficient: a constant-keeping formal hard-rejects
(`-1`) a contour holding a different constant or the abstract type; or
rebinding an edge retracts the previous binding's return flow.
`reroute_uncovered_edges` (closed/178 fix 2) re-takes stale routes at
quiescence and may already cover msp_ss: **check that first**, on the
site, not the verdict.

## Step 3, blocked: `merge_equivalent_creation_sets` (`PYC_CSMERGE`, built, default 0)

The data-side counterpart: same-class CreationSets whose converged
members, element and arity agree, with no coercion, same setter classes,
no live demand, no ledger touch this pass. Census: 70 pairs on bh, 25 on
voronoi, mostly closures (excluded: `partial_application` needs one
creation point per closure CS).

Every merged pair is split straight back on the next pass, to the pass
cap, by the SETTER stage, which separates two creation points whenever the
writes into what they allocate differ at all. That is a split on a fact,
and it is [196](196-the-setter-stage-splits-on-a-fact.md). Adding a guard
against the re-split would be the hysteresis this issue forbids. **Do not
attempt step 3 before 196.**

## Stop condition

If a merge re-creates the same split on the next pass, the split has a
live demand this issue misread. Find what the demand is. Do not add
hysteresis.

## Verification

Step 2: msp_ss has no argument-vs-formal mismatch at the `ord('q')` site;
`tests/cross_type_eq.py` and `tests/minmax_3arg.py` pass under
`--fa-inline=0`. Step 3: `PYC_CSMERGE=1` on voronoi and bh converges
without the pass cap, output unchanged, `css` down; then it defaults on.
