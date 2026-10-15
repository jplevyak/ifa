# 129 — THE PLAN: demand-driven CreationSet splitting

**Status: open, on one residual family (below). The plan has landed.**
Condensed 2026-10-15; the full record, including the shared-writer
investigation now in 178, is in git:
`git show e92568f3:ifa/issues/129-plan-demand-driven-creation-set-splitting.md`.
It carries [133](closed/133-split-a-container-on-its-element-type.md);
[128](closed/128-cs-identity-over-discriminates-vs-element-type.md),
[131](closed/131-demand-driven-constant-splitting.md),
[143](closed/143-shared-container-method-contours-refuse-cs-splits.md) and
[144](closed/144-route-4-fans-per-creation-point-instead-of-partitioning.md)
are closed into it. If another issue seems to carry a competing plan, this
file wins.

## The design

1. **Coarse identity by construction.** `PYC_CSDCPA1=2`: one CreationSet
   per sym, except `tuple`, whose arity and position are part of its type
   (132). This is not "reuse" (closed/128).
2. **Demand-driven separation.** Give precision back only where something
   observed a distinction and could not proceed. The demand is observed
   where the union is USED, almost never where the merge happened, so
   backtracking is part of the step, not an option (closed/152).
3. **A ledger**, so a re-derived separation re-attaches to the contour it
   first made (`ledger_find_cs`/`ledger_add_cs`,
   `find_or_make_filtered_entry_set`).

The three-way distinction (AGENTS.md) governs every step: **assignment**
(which CS a value flows into) is decided by types, **identity** (which
creation point this is) may be as fine as it likes, and **compatibility**
(may two creation points share a CS) is decided by demand.

## Landed (all default-on)

| piece | issue | what it does |
| --- | --- | --- |
| `PYC_CSDCPA1=2` | 128/129 | start merged |
| arity in CS identity | [132](132-arity-is-representation-not-provenance.md) | a representation property is not provenance |
| `CS_DEF_PARTITION` (route 4), uncapped | 133, 142 | partition `cs->defs` by assign-set signature, as the last rung, on a demand only (146) |
| `PYC_CSCONTENT=1` | 133/154 | build the flow graph over the channel that holds the content (element, or positional slots) |
| `PYC_CSBACKTRACK=1` | 152 | walk a `defs=1` demand back to the merged CS upstream |
| `PYC_ESBLOCK=1` | 133/146 | when a shared writer contour hides the partition ("1 group"), split that ES as a MEANS, two groups, container element writers only |
| `PYC_CSPARENTFIRST=1` | 074 | an ES split must not split, or re-merge, a CS |
| owner lift (`PYC_CSOWNER=1`), `PYC_RECGATE=1` | 074/168 | setter splitting through a container's containers; the recursive overlap gate |
| `PYC_NILSTORE=1`, `__delitem__`'s false `merge_in` removed, heapq's `[n-1:n] = []` | 133 | single false edges found with probes |

**Corpus now** (`check__default__ea9a60bc+23598ae4`): 1 compile failure
(othello3, a compile timeout), 58 programs matching CPython, 1 differing
(othello2, int64 overflow against CPython's unbounded int), 18 unverifiable
(CPython itself exceeds the 120 s cap), 21 with warnings (almost all
int/float coercion). Container CS / shapes **2669 / 799 = 3.34**. At
`4b61e721`, when this plan was written, it was 24 compile failures, 20
matching, 2122 / 666 = 3.19; the ratio moved because the 23 programs that
now compile add containers. None of the remaining defects is a splitting
problem.

## Open: a container with ONE creation point whose element takes two types

The residual family: the element receives two types through paths that
agree on argument types AND on call site, so neither CS partitioning
(nothing to partition) nor ES splitting (nothing to split on) can act.
**What has moved this family every time is finding the specific upstream
merge and fixing it** (closed/152's backtrack, `__delitem__`, heapq,
closed/139's arity hole, closed/172's global fold, closed/175's dead
loop), each a single wrong edge found with a probe, not a new mechanism.
The confluence is a CONTOUR, not a program point (AGENTS.md).

**Regression coverage of the past instances** (2026-10-15; each probed by
removing its fix and checking that the test fails):

| instance | guarded by |
| --- | --- |
| closed/152, backtrack to the merged CreationSet | **corpus only: plcfrs.** chull no longer needs it. Same-binary `check -R` with `PYC_CSBACKTRACK=0` (`check__PYC_CSBACKTRACK_0__e92568f3+2afef579` vs `check__default__ea9a60bc+23598ae4`): plcfrs fails to compile (`min(chart[start])`: illegal argument `Terminal`), the other 76 unchanged. The backtrack there fans from one demanded CreationSet to 10+ upstream ones; no suite test reduces it yet, and `tests/plcfrs_grammar_tables_nonconvergence.py` passes without it |
| closed/139, an unknown-arity list joining a fixed-arity CreationSet | `tests/list_mul_joins_fixed_arity_literal.py` (fails under `PYC_ARITYSTRICT=0`), and `tests/builtins.py` |
| `__delitem__`'s false `merge_in` (closed/133) | **no output can catch it now.** With the old `__pyc_setslice__(..., [])` form restored, `tests/list_pop_insert.py` and a targeted reduction both still match CPython: route 4 separates the leaked element downstream. A regression would show only as extra contours |
| closed/172, global loads folded | `tests/containers_of_different_types_module_scope.py`, `tests/dicts_int_and_object_keys.py` |
| closed/175, a dead loop kept live by a shared compare | `tests/empty_list_loop_live_via_shared_compare.py` |

No corpus program is attributed to it as of 2026-10-15. When one appears:
`IFA_DBG_ELEMCONF` (which channels hold two classes, and whether their
writers are separable), `IFA_DBG_CSVARS` / `ELEMWRITER` (who writes into an
element), `IFA_DBG_CSDEFSPLIT` (what route 4 decided and why). Two shapes
that look like this family and are not:

- **One slot holding two types over time** (bh; `b.replace([...])` after
  construction in 178's 2026-10-15 notes). Temporal, not per-object: no
  split separates it.
- **A branch that should fold per contour and does not** (the
  `record_member_shared_constructor` case). A constant demand, fixed in
  134 by walking from content to its writers.

## Elsewhere

| was | now |
| --- | --- |
| A. the shared-writer decline ("1 group") and the ES demand link | [178](178-FA-route4-declines-records-built-through-one-constructor.md): no known witness; `PYC_RETDEMAND` deleted ([157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) item 3) |
| C. stage 5's fan and the lower rungs | [146](146-remove-all-arbitrary-splitting.md) (inventory empty), [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) |
| D. constants | [134](134-remove-the-frontend-forced-split-opt-in.md): 9 forced-split annotations |
| E. performance | [111](111-FA-selective-invalidation-per-pass.md), not a precondition |
| taking decisions back | [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) |

## Settled — do not re-litigate

- **A merge can be taken back.** `analyze_to_convergence` resets before
  every pass; only the DECISION (`av->cs_map`) persists. Wholesale
  re-deriving costs precision, never correctness.
- **Mint-time reuse keying cannot work.** The key is read when the element
  is unfilled by construction (402 of 795 such mints never acquire a
  shape). `cselem`/`csshape`/`csmold` all measured 0 hits; all deleted.
- **`dcpa1`'s `tuple` exclusion stays.** Merging tuples costs ten corpus
  programs, and it makes sudoku5's comparison contour worse (22 → 32
  shapes).
- **The separator set is not "slot type at construction".** Recording the
  literal's slot types at the mint measured WORSE: the construction
  operands are already polluted by the time route 4 runs, and a slot's
  type is inferred, not structural. Arity works because it is a COUNT.
  *"The solution must be demand, not at mint"* (author, 2026-09-10).
- **The pyc suite is blind to this area.** Several changes here were
  suite-neutral while costing 3-10 corpus programs. Judge a change here by
  a same-binary corpus `check` A/B, per program (`compile_rc`, `run_rc`,
  `stdout_match`), never by totals.

**Do not restart from these** (each deleted, with its measurement in 146
or 157): `PYC_CSELEM`, `PYC_CSSITELESS`, `PYC_CSRESPLIT`, `PYC_CSDEFREUSE`,
`PYC_CSCALLSITE`, `PYC_CSMEMBER`, `PYC_WALKCTX` (matched call/return on the
backflow walk answers must-reach where route 4 asks may-reach), `PYC_CSPEEL2`,
`PYC_CSFAN`, `PYC_SETTERMIN`, `PYC_RETDEMAND`, the level-triggered detector.

## Verification, for any change here

Six CI gates on both backends; same-binary `./corpus_sweep.sh -m check`
A/B, compared per program; `ess`/`css` down, not up. A 1–2 program move
between two BUILDS is not attributable
([147](147-analysis-result-depends-on-the-binary-not-the-inputs.md)).
