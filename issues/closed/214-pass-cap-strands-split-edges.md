# 214 — a guard that stops the analysis after a split strands the split's edges

**Status:** fixed 2026-11-27. Found in langwb (Step 12 review follow-up):
MLite's `wordcount.mlite`, with both operands of `or` / `and` tested by an
`if`, printed a pointer-sized number for `len_l(ws)` instead of `15`. A
wrong answer, not a refusal.

## Symptom

`extend_analysis()` runs the split stages first. Only afterwards do the
stall guards (`stall_limit`, `nonimprove_limit`, the zero-violation stall)
and the pass cap (`pass_limit`) decide to stop, by zeroing `analyze_again`;
the outer loop in `analyze_to_convergence()` also stops on
`analysis_pass > pass_limit`. The splits of that last pass have already
rebound edges (`set_entry_set`, MINT) to new EntrySets, and those are never
analyzed: no argument types, no out edges, not in `fa->ess`, no `equiv`.

`clone()` then leaves such an EntrySet's `fun` pointing at the original
`Fun`, which becomes whichever clone of the callee took the last
equivalence class, and `clone_functions()` wires the caller's call site to
it. In the langwb case the edge `len_l -> fold_l(xs, 0, fn (n, _) => n +
1)` oscillated (MINT on odd passes, REUSE of the analyzed contour on even
ones) until the cap stopped it on a MINT pass at pass 101; the call was
generated as a call of the `fold_l` clone that `rev_l` uses, with `null`
for the `0`, and its result went through `ptrtoint`.

## Fix

When a guard or the cap stops the analysis in a pass whose split stages
split, `extend_analysis()` asks for one more pass (`settle_pass_pending`),
which the outer loop runs even past the cap. Splitting is already
suppressed for it (`pass_limit_hit`), and its `extend_analysis()` returns 0
at once. So the analysis never returns with edges to unanalyzed contours;
a guard that fires costs one extra pass.

## Test

`ifa-test --phase fa-converge` prints `unanalyzed-edges:` (call edges of
analyzed contours whose target is not an analyzed contour), 0 in every
golden. `tests/ir/fa-converge/06_pass_cap_settles.ir` is `03_cascade.ir`
with `;; pass-limit: 0` (a new fixture header `ifa-test` applies to
`FA::pass_limit`), where the cap stops the analysis in the pass that splits
`%fwd`: 1 unanalyzed edge before the fix, 0 after.

## Not fixed here

Why the langwb edge oscillates between minting a contour and reusing the
analyzed one is not established; the run still ends at the pass cap, now
with a consistent result.
