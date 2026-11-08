# 196 — the SETTER stage splits on a fact; the demand it needs is a counterfactual

**Status: open; the demand-free half landed 2026-11-08** (below). `go`'s
split is still there, and why is now measured. Filed 2026-11-03 as the survivor of
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

## 2026-11-08: the census, the gate that landed, and why `go` is not it

**What the stage is needed for.** With `PYC_NOSETTER=1`, 11 corpus programs
stop compiling (`check__PYC_NOSETTER_1__0b9265ba+c9077403`):
- bh, dijkstra2, mastermind2, plcfrs, pygasus, pylife and sudoku2 fail on
  irrepresentable unions or illegal argument types;
- doom fails on an untyped expression;
- quameon, sudoku4 and sunfish time out.

27 suite tests fail too, among them every generator test,
`defaultdict_eq`, `builtin_type_factory`, `match_map_star`,
`nil_union_prealloc`, and `decorator_recursion` (a wrong answer). `go` is
not on either list.

**What it splits.** A probe (temporary, removed) logged every SETTER split
over the 606 corpus and suite compiles: 720 splits in 135 programs. It
also logged the merged contour's demand, asked with SETTER_OF_SETTER's
tests (`sos_*`) on the members before the split. **120 had no demand at
all**: no irrepresentable member, no violation reach, no
dispatch-receiver member, nothing through the content.

**Landed: decline those** (`setter_contour_demanded`, `fa.cc`, both
SETTER actuators). It needed one new clause. `decorator_recursion`
segfaulted without it: `counted`'s wrapper closure holds
`f = {fact, down}`, and `f(n)` dispatches to two functions. That is a
receiver-less dispatch, which `sos_poly_recvs` never collected, so
`sos_poly_callees` now holds the called values of every send that reaches
two or more functions.

Measured:
- **Suite:** unchanged, 495/0 on both backends.
- **IR goldens:** one fixture changed, `iterator_missing_field.synth`
  (refused before and after; 5 → 3 EntrySets), re-blessed in its six
  phases.
- **Corpus:** a same-binary A/B (`check__PYC_SETTERDEMAND_1__0b9265ba+361322cd`
  against `check__default__0b9265ba+361322cd`) has every verdict
  identical. EntrySets and CreationSets fall in 11 programs and rise in
  none: solitaire 232 → 221 EntrySets, minilight 2473 → 2428 and
  tonyjpegdecoder 2059 → 1969 CreationSets. It is monotone, as 146's
  diagnostic requires.
- **As landed (default, no flag):** `check__default__0b9265ba+0048dc1a` is
  identical to that A/B's gated arm in every column, and
  `check__PYC_LLVM_1__0b9265ba+7dffc5df` has the same verdicts as
  `…4ab937b3+0205962a`.

**`go` is demanded, twice.** `UCTNode`'s split (pass 10) follows the
EntrySet splits of `UCTNode.__new__` (pass 8) and `__init__` (pass 9).
Its demanded members are:

| member | contours | clause |
| --- | --- | --- |
| `losses`, `pos`, `wins` | `int64` only | on the backward reach of a violation |
| `parent`, `bestchild` | `{None, UCTNode}` | receivers of a truthiness dispatch to two functions |
| `pos_child`, `unexplored` | one `list` each | content walk |

**What was tried for `go`, and what each cost.** Each step was a mode of a
temporary flag, measured on eight witnesses, the suite and `go`:

1. **Count a member only if it holds two or more contours.** A one-contour
   member is the same on both sides of any split.
   - Counting classes instead of CreationSets lost `defaultdict_eq`,
     `builtin_type_factory` and `match_map_star`, whose `_keys` holds four
     `list` contours. CreationSets are the right count.
   - Applying the rule to EntrySet splits too lost 19 suite tests,
     generators among them: an EntrySet split on a single-typed formal
     answers a demand in that formal's content.
   - On CreationSet splits only, the suite was clean, but `go` kept its
     split through the `parent` dispatch.
2. **Do not count a `{None, T*}` dispatch,** which codegen resolves with a
   null test. `go` kept its split through the content walk.
3. **Have the content walk skip the CreationSet being split.** Its own
   members were judged directly. Without the skip, `parent` → `UCTNode`
   re-counts the `int64` members' demand.

With all three, the suite loses only `nil_receiver_method_send`'s
`niltest=3` golden (it becomes 6, with the same output and `dynamic=0`).
`go` reaches the target: three `UCTNode` structs, as with the stage off,
and 1 `--refuse-imprecise` site instead of 6 (the remaining one is
`__str__` in `__pyc__.py`).

**But the corpus loses four programs**
(`check__PYC_SETTERDEMAND_6__0b9265ba+8bb88384`): dijkstra2, plcfrs,
pylife and sunfish fail as they do with the stage off. Bisected: rule 1
alone loses all four. Their demand reaches the CreationSet through a
one-contour member while the split partitions a different member. So a
per-member test is wrong, and the CreationSet-level form ("some member
demanded, and some member partitioned") keeps `go`'s split, because
`go`'s demand also arrives through one-contour members. That is the stop
condition: no local test on the merged contour separates `go` from these
four. It agrees with closed/124.

CreationSets fell in 21 programs under the full gate (loop 1327 → 1196,
voronoi2 1605 → 1494, bh 1227 → 1202), so there is more to win here than
`go`.

**What `go` needs, then.** Two questions, both about the violation-reach
clause:
- **Which violation is it?** `go`'s only diagnostic is the `maxscore`
  int/float coercion warning, which no `UCTNode` split can affect. Check
  whether `sos_viol_reach` is built from a violation the split cannot
  change. If so, the fix is to restrict the reach to violations whose
  union the partition would separate, and the counterfactual is asked of
  the violation, not the member.
- **Why does the reach pass through `losses`/`wins`?** Find out which
  writer's flow carries it.

The experiment's code is not in the tree. The modes are described above
and are reproducible from this text.

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
