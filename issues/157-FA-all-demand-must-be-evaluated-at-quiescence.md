# ifa/157 — ALL demand must be evaluated at quiescence

**Status:** open. Architecture issue: when may a demand test run, and why do
the lower rungs of the ladder almost never get to? Rewritten 2026-09-28.
This file now also carries [148](closed/148-stage-5-starvation-root-caused.md)
(stage starvation, termination) and the residual of
[007](closed/007-FA-mark-type-stage-coverage.md) (SETTER_OF_SETTER is never
reached). The full record, including every probe's output, is in git:
`git show 3f36072b:ifa/issues/157-FA-all-demand-must-be-evaluated-at-quiescence.md`.

**Author's directive, 2026-09-15:** *"All demand should be done after
quiescence. All."*

## The rule

A demand is a property of the CONVERGED types: *this AVar holds a union that
something cannot proceed on*. It is asked once types reach a fixed point,
level-triggered, and re-asked every pass. It is never asked on the
transient event of a union forming.

## What is already true — read this before acting on the rule

**Types ARE converged when every split stage runs.**
`analyze_to_convergence` drains the edge, send and ES worklists to empty
before `extend_analysis` → `run_split_stages`. So "when" was never the
defect people assumed. A level-triggered detector was built
(`PYC_CONFLEVEL`), measured and deleted. It produced 3.6× the candidates
and one more split on `softrender`, and warnings went 39 → 206 on
`sudoku4`: the stop condition "the detector is not the blocker, the
partitioner is" fired exactly as written.

**The `!analyze_again` gate is NOT a quiescence gate.** `fa.cc` calls it
one throughout ("runs only on quiescence of all stages above"). What it
tests is *did a higher-priority stage act this pass*. It is a CASCADE
PRIORITY gate, and it starves every lower stage for as long as a higher
one has work. Measured: `PER_CS_RECEIVER` and `CSM_ELEMENT_CS` fire ZERO
times on every program measured. `split_css` runs 31× on `bh` and 0× on
`sudoku4`. `SETTER_OF_SETTER` is reached only when SETTER found nothing
that same pass (007). Stage 5 got 2 of 41 passes on `sudoku5`.

**Why stage 1 claims nearly every pass:** a separation propagates ONE
contour per pass. A filtered contour narrows its formal, and for that to
reach the next callee the callee needs its own filtered product, which
only the splitter makes, once per pass. So stage 1 does real, slow work on
~35 of 41 passes. That is not false progress and cannot be suppressed.
(`PYC_ROUTESTABLE=1`, now default, removed the part that WAS false: a
re-route of an unchanged group to the same home.)

## The four defects, and the plan for each

### 1. Rename the gate to what it tests

The misnomer is load-bearing. It is why "stages 6+ are starved" read as a
convergence problem across two issues. Rename it and its comments to a
cascade-priority gate. No behaviour change.

### 2. Give every stage route 4's shape: TOLD, not gated

`split_css_by_defs(int quiescent)` (route 4, `CS_DEF_PARTITION`) takes the
condition as a PARAMETER. It runs every pass, asking a weaker question
early and a stronger one late. That is why ifa/143's level-triggered
`csdemand` works at all. `PER_CS_RECEIVER`, `SETTER_OF_SETTER` and stage 5
(VIOLATION) should take the same shape.

**Not by lifting the gate.** That is `PYC_RECVFAN=2` (removed) and
`PYC_SETTERGATE=1/2` (on 146's delete list). With the gate lifted
`split_css` partitions every starter finely: +31% container CSs, sudoku4
fixed and sudoku5 broken. Each stage needs its DEMAND TEST written before
it gets the parameter: *"the answer has to be a reason this stage may act,
not permission to act without one."* `collect_setter_confluences` builds
`setter_starters` with no demand test at all (every container allocation
qualifies), which is why lifting its gate over-splits.

### 3. Route the ES-side demand to an actuator instead of dropping it

Stage 1's only actuator is "split an EntrySet on a formal". A confluence
on a non-formal rvalue is counted and dropped at `tc_skip_rval` (769 of
923 per pass on `softrender`). Measured with `IFA_DBG_RETCONF`: these are
unresolved DISPATCHES (callee returns disagree, from different Funs), all
class-based on the receiver, none on closures. Walked back through
union-carrying writers, 97% on `sudoku4` land on a CreationSet: route 4's
actuator. `PYC_RETDEMAND` (default 0) is that link. It delivers (on `sudoku4`,
closed/152's backtrack reaches go from 0 to 135) and changes nothing, because everything downstream
declines. 72% of the routed CSs have one creation point, and route 4
declines the rest with *"1 group: every creation point on the same assign
sets"*.

So the dispatch ambiguity is a SYMPTOM. On `sudoku4` 102 ambiguous sites
come from **5** distinct receiver unions, each a `{dict, bool, list,
tuple, set, int64, str}` union the program never makes. Fix the few merges
upstream, not the broadcast. This is [129](129-plan-demand-driven-creation-set-splitting.md)'s
open item (the shared-writer decline).

### 4. Terminate on a fixed point, not on "a stage split"

`analyze_again` means two things: *a stage split something* and *run
another pass*. They are not the same condition. The splitter runs after
the analysis in each pass, so a structural change is only observed a
pass later, and an apply that mints no contour still re-points edges
(sudoku5: 501 of 501 such applies moved edges). `CONVERGED=1` reports only
that the pass cap was not hit; `tuple_compare` "converges" four passes
before its types settle. `PYC_TYPEMOVE=1` sets `analyze_again` while any
live contour's formal types still move (pointer identity of hash-consed
ATypes). It stops for the right reason and costs 2 suite tests
(`container_scalar_union_add`, `set_ops_chained_mixed_elem_types`).
Finish it. This is a correctness property of the whole analysis, not a
splitter detail.

## Stop conditions, written before measuring

- If a stage given the route-4 shape fires on every pass forever, the gate
  was load-bearing for termination and the stage needs a demand test
  before it needs permission.
- For (3): if the routed demands land on CreationSets whose creation points
  all share one assign-set signature, the blocker is 129's writer split,
  not the link. That is today's state.
- For (4): if `TYPEMOVE` increases passes without changing any result, the
  moving types are a period-2 oscillation, which is
  [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)'s
  business and not a termination problem.

## Dead ends — each built, measured, deleted

| attempt | result |
| --- | --- |
| level-triggered stage 1 (`PYC_CONFLEVEL=1..3`) | 3.6× candidates, +1 split; sudoku4 39 → 206 warnings |
| split each creation point, let the pass re-derive (`PYC_CSFAN`) | 26 unsplittable `defs=1` contours; the shared `append` contour re-merges them |
| lift `CSM_ELEMENT_CS`'s gate | confluences byte-identical, errors 0 → 9 |
| demand-filtered setter starters (`PYC_SETTERGATE=3`) | worse than no filter: partitioning some containers while siblings stay merged is unstable |
| coarsest setter partition (`PYC_SETTERMIN`) | broke the setter-driven dict split in sudoku5 (15 vs 26 setter classes separate X and Y). The finest setter partition is the sanctioned CS criterion working, not an over-split |
| ES receiver split from an irrepresentable rvalue (`PYC_ESDEMAND`) | ~99% do not derive from a formal of their own contour; the rest self-blind |
| tuple homogeneity / slot-signature keys (`PYC_SPLITHOMO`) | too coarse / not a fixed point of its own decision (900 s timeout) |
| whole-path split in one pass (`PYC_ESPATH`) | right idea (tuple_compare 13 → 10 passes), wrong selector: any type test against a big union admits most of the program |

## Probes that stay (count, change nothing)

`IFA_DBG_CONFLEVEL`, `IFA_DBG_RETCONF[_V]`, `IFA_DBG_INCOMPAT`,
`IFA_DBG_SEED`, `IFA_DBG_CSDEFS`, `IFA_DBG_SCSS`, `IFA_DBG_ESDEMAND`,
`PYC_DBG_QUIESCE` (misnamed: it counts passes on which stage 1 found
nothing), `PYC_DBG_STAGEDELTA`, `IFA_DBG_REPARK`.

## Verification

Six CI gates. `IFA_DBG_INCOMPAT` baseline on `softrender` p=1: seen /
skip(rval) / dec / split(formal) = 923 / 769 / 90 / 66. Stage-firing
counts per program, which should go from 0 to >0 for `PER_CS_RECEIVER` and
`SETTER_OF_SETTER` on programs with a real demand at those rungs. A
same-binary corpus `check` A/B.
