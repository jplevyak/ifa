# 196 — the SETTER stage splits on a fact; the demand it needs is a counterfactual

**Status: open, design.** Filed 2026-11-03 as the survivor of
[closed/124](closed/124-FA-refuse-imprecise-inference.md) residual 2,
[closed/146](closed/146-remove-all-arbitrary-splitting.md)'s last
default-path violation, [closed/178](closed/178-FA-route4-declines-records-built-through-one-constructor.md)
(the writer split, no witness), [closed/185](closed/185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md)
phase A, and [closed/129](closed/129-plan-demand-driven-creation-set-splitting.md)'s
residual family. This is the one open design question in the splitter.
Everything it blocks is listed at the end.

## The defect

`SETTER` (stage 2, `split_for_setters` and `split_css`) nominates a
member or element whenever its writers disagree: "the setters of this
confluence differ" is a FACT about the program, not a demand (README
"Read first", rule 1). Measured on `go` (closed/124): `UCTNode.parent` is
`{None}` from `__init__` and `{UCTNode}` from `play`; the stage splits the
CreationSet by setter class, `go` ends with two `UCTNode` layouts where
shedskin has one, and nothing ever observed `{None, UCTNode}` and failed
to proceed. `PYC_NOSETTER=1` gives one layout and output identical to
CPython.

## Why a plain demand gate does not work (measured, do not repeat)

Four gate formulations were built on 2026-10-17 and reverted
(closed/124, "The full demand gate"): nominate a SETTER confluence only on
SETTER_OF_SETTER's demand test (`sos_type_conflict`), with and without its
content walk, with numeric mixes counted, with reach from every
irrepresentable AVar. Each either left `go` unchanged or lost suite tests
(`polymorphic_field_per_type`'s `{float64, C}`, `defaultdict`'s
`{list, int64}`, `decorator_recursion` with a wrong answer). The finding:
**the confluence SETTER must split is upstream of the union it prevents,
and that union does not exist yet on the pass where SETTER must act.** A
demand observed at quiescence cannot name it, because the merge has not
happened. So the demand is a counterfactual: *would merging these writers
make an irrepresentable union downstream?*

The same wall appears from the other side in
[170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
step 3: every merge of two converged-equal CreationSets is re-split by
SETTER on the next pass, to the pass cap.

## The shape of an answer

Route 4 (`split_css_by_defs`) already asks a counterfactual on converged
content: it partitions creation points by assign-set signature and splits
only when the partition has more than one class. Ask SETTER the same way:

1. For a nominated confluence, partition its writers by the type each
   would contribute to the merged member (converged `out` of each writer,
   constants stripped where the formal does not want them).
2. Split only when the partition has two or more classes that are
   **irrepresentable together** (`atype_irrepresentable` over the union of
   the classes; a nullable pointer `{None, T*}` is one class).
3. Otherwise decline, and record the decline so [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
   step 3 may merge.

Stop condition, from 170: if a merge re-creates the same split on the next
pass, a real demand was misread. Find it. Do not add hysteresis.

A second route, if 1-3 is not enough: a call-site split of the shared
writer with a re-merge at convergence (closed/185 phase A). Without the
re-merge it is 1-CFA by the back door and is forbidden; with it, it is the
"mechanism, not reason" case AGENTS.md's 2026-09-06 refinement allows.

Also here: `is-recursive-edge-low` in the Concert ancestor takes a
recursion LEVEL and retries a type split one level deeper when level 1
finds nothing (closed/195). ifa's `recgate` has no second level. Try it
only if 1-3 decline on a recursive confluence.

## Watch list (no current witness; do not build without one)

- **One creation point whose element takes two types** through paths that
  agree on argument types and call site (closed/129's residual family).
  Every past instance was a single wrong edge found with a probe
  (closed/152, closed/133's `__delitem__`, heapq, closed/139, closed/172,
  closed/175). When a program appears: `IFA_DBG_ELEMCONF`,
  `IFA_DBG_CSVARS`, `ELEMWRITER`, `IFA_DBG_CSDEFSPLIT`.
- **A receiver-aware route-4 key** (closed/178 fact 2): a store's receiver
  creation point is not on the content path, so two records built through
  one constructor reach the same assign sets. Six variants tried; each is
  either already separated or temporal (bh's shape, no split).
- Two shapes that look like this family and are not: one slot holding two
  types over time (temporal; the fix is in the program, PYC_CHANGES.md),
  and a branch that should fold per contour (a constant demand,
  [134](134-remove-the-frontend-forced-split-opt-in.md)).

**Do not restart from these** (each deleted with its measurement in
closed/146 or closed/157): `PYC_CSELEM`, `PYC_CSSITELESS`,
`PYC_CSRESPLIT`, `PYC_CSDEFREUSE`, `PYC_CSCALLSITE`, `PYC_CSMEMBER`,
`PYC_WALKCTX`, `PYC_CSPEEL2`, `PYC_CSFAN`, `PYC_SETTERMIN`,
`PYC_RETDEMAND`, `PYC_SOSDEMAND`, the level-triggered detector, a
compatibility rule for receiver identity, receiver-keyed method contours,
unconditional receiver filtering, a bounded wholesale fan.

## Blocked on this

- [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
  step 3, the CreationSet merge (`PYC_CSMERGE`, built, off).
- `go`'s one-layout `UCTNode`, and every corpus program whose clone count
  SETTER inflates (voronoi: two `list[int64]` to shedskin's one).
- `--refuse-imprecise` on `go` (6 sites, all this split).

## Verification

`go` compiles with one `UCTNode` layout and matches CPython; `make test`
green on both backends; same-binary corpus `check` A/B with no verdict
regression and `ess`/`css` down; then `PYC_CSMERGE=1` runs voronoi and bh
to a fixed point without hitting the pass cap.
