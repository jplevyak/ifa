# 129 — THE PLAN: demand-driven CreationSet splitting

**Status: open. The single integrated plan.** Rewritten 2026-09-28. It now
also carries [133](closed/133-split-a-container-on-its-element-type.md)
(the container separator). [128](closed/128-cs-identity-over-discriminates-vs-element-type.md),
[131](closed/131-demand-driven-constant-splitting.md),
[143](closed/143-shared-container-method-contours-refuse-cs-splits.md) and
[144](closed/144-route-4-fans-per-creation-point-instead-of-partitioning.md)
are closed into it. Constraints on it live in
[146](146-remove-all-arbitrary-splitting.md) (no arbitrary splits),
[157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) (when demand is
asked), [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
(taking decisions back) and [134](134-remove-the-frontend-forced-split-opt-in.md)
(constants). If one of those seems to carry a competing plan, this file
wins.

## The design

1. **Coarse identity by construction.** `PYC_CSDCPA1=2`: one CreationSet
   per sym, except `tuple`, whose arity and position are part of its type
   (132). This is not "reuse" (closed/128).
2. **Demand-driven separation.** Give precision back only where something
   observed a distinction and could not proceed. The demand is observed
   where the union is USED, almost never where the merge happened, so
   backtracking is part of the step, not an option (closed/152,
   `PYC_CSBACKTRACK=1`).
3. **A ledger**, so a re-derived separation re-attaches to the contour it
   first made (`ledger_find_cs`/`ledger_add_cs`,
   `find_or_make_filtered_entry_set`). The coarser the start, the more
   this matters.

The three-way distinction (AGENTS.md) governs every step: **assignment**
(which CS a value flows into) is decided by types, **identity** (which
creation point this is: an AVar, already ES × call site) may be as fine as
it likes, and **compatibility** (may two creation points share a CS) is
decided by demand.

## What has landed (all default-on)

| piece | issue | what it does |
| --- | --- | --- |
| `PYC_CSDCPA1=2` | 128/129 | start merged |
| arity in CS identity | [132](132-arity-is-representation-not-provenance.md) | a representation property is not provenance |
| `CS_DEF_PARTITION` (route 4), uncapped | 133, 142 | partition `cs->defs` by assign-set signature, as the last rung |
| `PYC_CSCONTENT=1` | 133/154 | build the flow graph over the channel that holds the content (element, or positional slots) |
| `PYC_CSBACKTRACK=1` | 152 | walk a `defs=1` demand back to the merged CS upstream |
| `PYC_ESBLOCK=1` | 133/146 | when a SHARED writer contour hides the partition ("1 group"), split that ES as a MEANS, bounded to two groups, container element writers only |
| `PYC_CSPARENTFIRST=1` | 074 | an ES split must not split, or re-merge, a CS |
| owner lift (`PYC_CSOWNER=1`), `PYC_RECGATE=1` | 074/168 | setter splitting through a container's containers; the recursive overlap gate |
| `PYC_NILSTORE=1`, `__delitem__`'s false `merge_in` removed, heapq's `[n-1:n] = []` | 133 | single false edges found with probes |

Corpus at `4b61e721` (`check`): container CS / shapes **2122 / 666 =
3.19** (4.41 before the flip), 24 compile failures, 20 programs matching
CPython. linalg, plcfrs, bh, sudoku2/3/5, chull, richards and pystone all
compile.

## What is open

### A. The shared-writer decline on `sudoku4`, and the ES demand link

`sudoku4`'s first error today is a missing builtin (`dict.copy`, see
[086](closed/086-list-and-dict-have-no-copy-method.md)). Re-measure it
after that is fixed, before attributing anything to this plan. The
standing mechanism question: route 4 declines *"1 group: every creation
point on the same assign sets"* when the creation points' paths share a
writer contour. `PYC_ESBLOCK` answers this for container element writers.
[157](157-FA-all-demand-must-be-evaluated-at-quiescence.md)'s ES-demand
link (`PYC_RETDEMAND`) routes ~100 demands per pass on sudoku4 to route 4
and lands on exactly this decline. **Next:** with `dict.copy` fixed,
re-run `IFA_DBG_CSDEFSPLIT` on sudoku4 and check whether the "1 group"
declines remain. If they do, find the shared writer by `IFA_DBG_ELEMCONF`
before building anything.

### B. A container with ONE creation point whose element takes two types

The residual family: the element receives two types through paths that
agree on argument types AND on call site, so neither CS partitioning
(nothing to partition) nor ES splitting (nothing to split on) can act.
**What has moved this family every time is finding the specific upstream
merge and fixing it** (closed/152's backtrack, `__delitem__`, heapq,
closed/139's arity hole, closed/172's global fold, closed/175's dead
loop). Each was a single wrong edge found with a probe, not a new
mechanism. Keep doing that. The confluence is a CONTOUR, not a program
point (AGENTS.md).

### C. Stage 5's fan and the lower rungs

See 146 (land `PYC_SPLITEDGES2`'s bound) and 157 (the route-4 shape for
`PER_CS_RECEIVER` / `SETTER_OF_SETTER` / VIOLATION).

### D. Constants

[134](134-remove-the-frontend-forced-split-opt-in.md): 9 forced-split
annotations remain.

### E. Performance

[111](111-FA-selective-invalidation-per-pass.md). Not a precondition for
anything here.

## Settled — do not re-litigate

- **A merge can be taken back.** `analyze_to_convergence` resets before
  every pass; only the DECISION (`av->cs_map`) persists. Wholesale
  re-deriving costs precision, never correctness.
- **Mint-time reuse keying cannot work.** The key is read when the element
  is unfilled by construction (402 of 795 such mints never acquire a
  shape). `cselem`/`csshape`/`csmold` all measure 0 hits.
- **`dcpa1`'s `tuple` exclusion stays.** Merging tuples costs ten corpus
  programs, and it makes sudoku5's comparison contour worse (22 → 32
  shapes).
- **The separator set is not "slot type at construction".** Recording the
  literal's slot types at the mint (`PYC_CSMEMBER=2`) measured WORSE: the
  construction operands are already polluted by the time route 4 runs, and
  a slot's type is inferred, not structural. Arity works because it is a
  COUNT. *"The solution must be demand, not at mint"* (author,
  2026-09-10).
- **The pyc suite is blind to this area.** Several changes here were
  suite-neutral while costing 3-10 corpus programs (the tuple merge,
  `PYC_ESPATH`). Always judge a change here by a same-binary corpus
  `check` A/B, per program (`compile_rc`, `run_rc`, `stdout_match`), never
  by totals.

## Do not restart from these

`PYC_CSELEM=3`, `PYC_CSSITELESS`, `PYC_CSRESPLIT`, `PYC_CSDEFREUSE`,
`PYC_CSCALLSITE` modes 1 and 3 (a fan), `PYC_WALKCTX` (matched call/return
on the backflow walk answers must-reach where route 4 asks may-reach; it
"fixed" a fixture by under-approximating), `PYC_CSPEEL2`, `PYC_CSFAN`,
`PYC_SETTERMIN`, the level-triggered detector. Each is recorded, with its
measurement, in 146 or 157.

## Verification, for any change here

Six CI gates on both backends; same-binary `./corpus_sweep.sh -m check`
A/B, compared per program; `ess`/`css` down, not up. A 1–2 program move
between two BUILDS is not attributable
([147](147-analysis-result-depends-on-the-binary-not-the-inputs.md)).
