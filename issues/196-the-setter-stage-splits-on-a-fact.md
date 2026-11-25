# 196 — the SETTER stage splits on a fact; the demand it needs is a counterfactual

**Status: open; the demand-free half landed 2026-11-08; re-analyzed 2026-11-25**
(options below; earlier draft at
`git show 194d92e1:ifa/issues/196-the-setter-stage-splits-on-a-fact.md`). `go`'s
split and `re`'s 42-pass zero-violation tail are still there, and why is now
measured. Filed 2026-11-03 as the survivor of
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

## 2026-11-24: a compile-time witness, pyc's `re` (issues/210)

The rewritten `pyc_lib/re.py` (a parser class, a program of op records, a
Match and a Pattern) is a clean, self-contained witness of the cost side.
One test using most of the module (`re_api.py`, since split into
`tests/re_find_sub.py`, `re_match_flags.py`, `re_syntax.py`) measured:

| | passes | ES at end | flow analysis | output |
| --- | --- | --- | --- | --- |
| default | 54 | 725 | 18.0 s | CPython's |
| `PYC_NOSETTER=1` | 12 | 570 | 3.0 s | CPython's |

Violations are zero from pass 12. Passes 12 to 55 are a tail of one to
thirteen splits each: `SETTER` / `SETTER_EQ_CLASS` on `_ReParser.__init__`'s
`self.names = {}` and `self.open_groups = []` (passes 14, 18, 30), then on
the `_ReParser` CreationSet (19, 31), `Pattern` (41) and `Match` (50,
`SETTER_OF_SETTER`), each followed by `TYPE_CONFL` splits of every method of
that class on `self` (`node`, `fail`, `peek`, `advance`, `parse_*`, ...).
Nothing downstream needed any of them: with the stage off the program types
and prints the same 41 lines. In the corpus, the new module raised compile
time for minilight from 26 s to 35 s and for sunfish from 53 s to 66 s
(sweep `check__default__c58b051a+a90a2485` against `b77498e0`, verdicts
unchanged).

What makes it a good test case: the merged members here (`names`,
`open_groups`, the parser and pattern objects) never become irrepresentable, so
any replacement below should decline every one of these splits and bring the
program to 12 passes, while keeping the 11 programs that need SETTER today. Reproduce:
`IFA_DBG_ESPERFUN=1 PYC_DBG_PHASES=1 ./pyc` on the three tests concatenated,
with and without `PYC_NOSETTER=1`; `--dump-json` gives every split with its
stage and trigger.

## 2026-11-25: why gating SETTER's output hit a structural wall

Every attempt above and in [closed/124](closed/124-FA-refuse-imprecise-inference.md)
kept SETTER's engine (`compute_setters` → `AVar::setters` → `same_eq_classes`)
and put a boolean gate (`setter_contour_demanded`) in front of
`split_ess_setters` and `split_css`. That strategy is exhausted for three
structural reasons:

1. **`setter_class` is `(Var, EntrySet)` writer provenance, not written type.**
   As measured in [closed/157](closed/157-FA-all-demand-must-be-evaluated-at-quiescence.md)
   (`fa.cc` `recompute_eq_classes`), `setter_class`es are singletons in
   practice (3,704 of 3,704 on `plcfrs`). Two writes of `int64` from two
   contours of a split method — or `{None}` in `__init__` and `{UCTNode}` in
   `play` — have distinct `Setters` sets. That drives `re`'s 42-pass
   zero-violation tail ([issues/210](../../issues/210-re-complete.md)): an
   EntrySet splits → its field or container writes become distinct `AVar*`
   singletons → `SETTER` splits the `CreationSet` → `TYPE_CONFLUENCE` splits
   every method of that class on `self` → their writes become new `AVar*`
   singletons. That is a contour split caused by an enclosing contour split,
   mediated through `Setters`.
2. **`AVar::setters` erases which member belongs to which demand.**
   `compute_setters` pours every confluent member's writers into one flat
   `av->setters` set on the container or formal. When `setter_contour_demanded`
   runs, it can test only an uncoupled conjunction: *some member has a demand*
   **and** *some member has differing writers*. In `go`, `losses` (`int64`) lies
   on `maxscore`'s unrelated int/float `sos_viol_reach` while `parent`
   (`{None, UCTNode}`) supplies the writer difference — locally identical to
   `dijkstra2`, `plcfrs`, `pylife` and `sunfish`, whose real demand arrives
   through a one-contour member while the split partitions another.
3. **Two competing data splitters disagree on contour identity.**
   pyc now has **stage 8** (`split_css_by_defs` / route 4, demand-backtracked,
   keyed on assigned `AType`s) and **stage 3** (`SETTER` / `SETTER_OF_SETTER`,
   seeded bottom-up from type confluences, keyed on writer `AVar*`s). Because
   stage 3 keys on writer identity,
   [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)'s
   `merge_equivalent_entry_sets` and `merge_equivalent_creation_sets` must carry
   `same_eq_classes` / `cs_setter_compatible` vetoes solely to keep SETTER from
   re-splitting converged-equal contours to the pass cap.

## What SETTER still uniquely provides (`PYC_NOSETTER=1`: 11 corpus, 27 suite)

Any replacement must account for the three capabilities stage 3 provides today
that stage 8 (`split_css_by_defs`) lacks:

1. **Splitting a shared factory or constructor `EntrySet` via caller return
   writes (`defs == 1`).** `edge_sset_compatible_with_edge` compares
   `e->rets[i]->setters`, splitting a factory when callers pass identical
   argument types (`[None] * n` in `list.__mul__` for `nil_union_prealloc`,
   `__new__`, `defaultdict`, generator frames) and write incompatible types
   into the returned object at the call sites. Stage 8 declines at `defs.n < 2`
   except for `CSOWNER` and numeric mixes (`csnumsplit`,
   [closed/156](closed/156-FA-split-int-from-float-coerce-last.md)).
2. **Splitting pass-through `EntrySet`s (`__init__`, mutating helpers) along
   `e->args`.** Stage 8's `ESBLOCK` splits only direct container-element writer
   contours when `defs.n > 1`.
3. **Demand kinds absent from stage 8's `demanded` set:**
   - **Callable-union dispatch** (`f = {fact, down}` in `decorator_recursion`,
     where `PYC_CSPARENTFIRST=1` merges `counted`'s closure record;
     `dispatch_named` inspects only untagged `rvals[1]` receivers of a single
     `Fun*`).
   - **Multi-hop member and container content demands** (`SETTER_OF_SETTER`,
     `defaultdict_eq`, `splitter_setter_of_setter`).

## Strategic options (2026-11-25)

### Option 1: retire `SETTER` and `SETTER_OF_SETTER`; fold their missing actuators into route 4 (`CS_DEF_PARTITION`)

Make `PYC_NOSETTER=1` the target state so pyc has one CreationSet splitter,
demand-driven and `AType`-keyed:

- **Generalize `csnumsplit`'s `defs == 1` climb (`fa.cc`) to any demanded
  channel.** Run `build_cs_flow_graph_over` with the call-, receiver- and
  heap-object-matched walk (`kCallMatchNumeric` / `heap_pts`,
  [closed/156](closed/156-FA-split-int-from-float-coerce-last.md)) over *only*
  the demanded channel(s), climb `defs[0]->contour` to the nearest choice
  point, and partition callers by which assigned `AType`s reach their returned
  or passed object — never by writer `AVar*` identity.
- **Widen `ESBLOCK`, `CSOWNER` and `CSBACKTRACK`**
  ([closed/152](closed/152-FA-backtrack-the-demand-to-the-merged-creation-set.md))
  to cover record-member channels as well as container elements.
- **Bring stage 8's `demanded` set to parity:** add callable-union dispatch
  (`sos_poly_callees`) and unresolved multi-`Fun` dispatch (excluding
  `{None, single-T*}` null tests and shared-ancestor `virtualvars`).
- **Payoff:** deletes `AVar::setters`, `setter_class`, `SETTER`,
  `SETTER_OF_SETTER`, and both setter-compatibility vetoes in
  [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md). Fixes
  `go`, `re` (54 → 12 passes) and `PYC_CSMERGE=1` by construction, against a
  closed worklist (27 suite tests, 11 corpus programs).
- **Risk / stop condition:** `quameon`, `sudoku4` and `sunfish` time out under
  `PYC_NOSETTER=1`. If triage shows they blow up in early passes because stage 8
  sits behind the cascade gate rather than because an actuator is missing,
  route 4's actuators either need stage 3's cascade priority or Option 2 is
  required.

### Option 2: demand-originated, `AType`-keyed stage 3 (invert `SETTER` per demand)

Keep stage 3's ladder position and its `e->args` / `e->rets` edge partitioner,
but replace fact-seeding, writer-`AVar*` singletons, and the mixed
`av->setters` bag:

1. **Seed backward from individual demands, never from
   `collect_type_confluences`.** Each demand carries its offending `AType` set
   `want` (as `CSBACKTRACK` does):
   - an irrepresentable member or element (`atype_irrepresentable`, numeric mix,
     `{None, scalar}`);
   - a `type_violation` backtracked *only* through channels that supply two or
     more incompatible parts of the violating union (excluding one-type
     bystanders like `go`'s `UCTNode.losses: int64` on `maxscore`'s int/float
     reach);
   - an unresolved multi-`Fun` dispatch *excluding* `{None, single-T*}`
     (null-testable, `dynamic=0`) and common-ancestor polymorphism.
2. **Key writer equivalence on written `AType` compatibility, not singleton
   `AVar*`.** Group a demanded channel's writers by constant-stripped `AType*`,
   with `{None}` riding in pointer classes unless paired with a scalar (as in
   `split_edges`). If all writers of that channel are compatible (`< 2`
   classes), stop.
3. **Propagate per demand (or per demanded channel) rather than unioning all
   members onto `av->setters`.** A CreationSet or EntrySet splits iff its
   creation points or call edges disagree on the incompatible writer classes of
   *that* demand.

- **Payoff:** breaks both `go`'s cross-member false positive (`losses` vs
  `parent`) and `re`'s 42-pass feedback loop (zero violations + `{None, T*}`
  excluded + same-`AType*` writers in split ESs collapse to one class), while
  preserving `dijkstra2`, `plcfrs`, `pylife` and `sunfish` (a demand inside a
  one-contour member propagates its two incompatible writer classes onto the
  outer receiver `x->container`). Lets
  [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) drop
  `cs_setter_compatible`.
- **Cost:** retains both stage 3 and stage 8 in `fa.cc`.
- **Stop condition:** if a program requires separating two writers of the
  *same* converged `AType*` because their types only diverge after the split
  (circular pass-0 fixed point, [closed/185](closed/185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md)
  phase A), pair with a demand-triggered call-site writer split and
  [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)'s
  convergence re-merge rather than restoring writer-`AVar*` identity.

### Option 3: `AType`-keyed `compute_setters` + scoped `sos_*` inside current `SETTER`

The minimum-surface probe of Option 2 inside the existing data structures:

1. In `compute_setters`, group writers by constant-stripped `AType*` (with
   `{None}` riding on single-pointer `AType`s; note `AType*` rather than `Sym*`
   so multi-CreationSet members like `defaultdict._keys`'s four `list` contours
   stay distinct), and skip seeding on confluences whose union is representable
   with fewer than two non-`None` CreationSets.
2. In `sos_build_demand`, ignore `{None, single-CS}` in `sos_poly_recvs` and
   restrict `sos_viol_reach` to AVars carrying two or more types of the
   violation's union.

- **Role:** fast probe to measure how much of `SETTER`'s over-splitting comes
  from writer-`AVar*` singletons and `{None, T*}` seeding alone, before
  building per-demand propagation.
- **Stop condition:** if two *real* multi-CS members on one class still cross-talk
  through the shared `av->setters` set, move to Option 2 or Option 1.

### Option 4: permissive split + LUB re-merge at quiescence ([170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)-driven)

Let upstream splits happen (including call-site splits on circular merges,
[closed/185](closed/185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md)
phase A), drop `cs_setter_compatible` from `merge_equivalent_creation_sets`,
and generalize [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
to merge CreationSets whose member-wise join (`X.m ⊔ Y.m`) is representable and
introduces no dispatch ambiguity.

- **Not standalone:** in `re` ([issues/210](../../issues/210-re-complete.md))
  the cost is 42 wasted passes before quiescence, which a post-hoc merge does
  not recover; in `go`, root (`parent = {None}`) and child
  (`parent = {UCTNode}`) are not `cs_state_equal`, and re-merging while stage 3
  still keys on writer `AVar*`s oscillates immediately (`170`'s stop
  condition).
- **Where it belongs:** follow-up step (`PYC_CSMERGE=1`) once Option 1 or
  Option 2 removes writer-`AVar*` identity from stage 3.

### Next step: two fast discriminating measurements

Before committing to Option 1 vs Option 2:

1. **Triage the `PYC_NOSETTER=1` failure set** (27 suite tests and the 3
   corpus timeouts `quameon`, `sudoku4`, `sunfish`): check how many collapse to
   `defs == 1` + `PYC_CSPARENTFIRST=1` (closures, generators, `[None] * n`),
   and whether the three timeouts come from a missing route-4 actuator or from
   stage 8's cascade position.
2. **Run Option 3's probe** on the fast witnesses (`go`, the three `re_*.py`
   tests concatenated, `decorator_recursion`, `defaultdict_eq`, `dijkstra2`,
   `plcfrs`, `pylife`) to see whether collapsing writer-`AVar*` singletons to
   `AType*` classes already brings `re` to ~12 passes and `go` to one
   `UCTNode` layout without losing the four corpus programs.

### Review of the options (2026-11-25)

**Target: Option 1. Option 2 is the fallback only with hard proof** (the
triage below) that route 4 cannot carry the demand. Reasons and corrections:

- **The counterfactual dissolves without SETTER.** The demand looked
  counterfactual only because SETTER acts before the union forms. Under
  `PYC_NOSETTER=1` the union DOES form (the 11 corpus failures are exactly
  those unions) and is then an ordinary quiescent demand, which CSBACKTRACK
  ([closed/152](closed/152-FA-backtrack-the-demand-to-the-merged-creation-set.md))
  exists to backtrack. That is "a merge can be taken back", and it is the
  case for Option 1. The title's diagnosis is superseded by it.
- **Option 2 is Option 1 at stage 3's priority.** Seeded from real demands
  and keyed on `AType`, stage 3 becomes a second backtracker beside route 4;
  structural reason 3 shrinks but remains.
- **The clause that admits `re`'s tail was never measured.** Those splits
  were measured (11-24) after the gate landed (11-08), with zero violations,
  so `sos_viol_reach` was empty and some other clause of
  `setter_contour_demanded` admitted them. Options 2 and 3 assume it is the
  `{None, T*}` receiver dispatch. Measure it first (see below).
- **`go`'s `maxscore` reach was a question on 11-08** ("Which violation is
  it?") and is stated as fact in structural reason 2. Confirm it.
- **Option 3's step 2 repeats 11-08's modes 2 and 3**, which hit that
  section's stop condition. Its new content is step 1, the `AType*`
  coarsening, and that is what the probe measures. Its reach is wider than
  SETTER: `sset_compatible` drives `split_entry_set`'s setter edge partition
  (`fsetters`) and both 170 vetoes.
- **`AType` coarsening is Option 2/3's central risk, not a corner case.**
  Two writers of the same merged CS compare equal, so a separation that only
  appears after a split is invisible: the 10-17 shape. It is the likeliest
  way to lose dijkstra2, plcfrs, pylife or sunfish. Option 2's claim to keep
  them is unmeasured.
- **"By construction" overclaims for Option 1.** `go` and `re` inherit the
  `PYC_NOSETTER=1` results only if the new route-4 demands (callable union,
  multi-`Fun` dispatch) do not fire on them. They join its stop condition.
- **`go`'s target is recorded inconsistently.** "The defect" says
  `PYC_NOSETTER=1` gives one layout; 11-08 says "three `UCTNode` structs, as
  with the stage off"; Verification asks for one layout. Reconcile.
- **`decorator_recursion`'s wrong answer is a separate soundness bug.** A
  two-callee dispatch (`f = {fact, down}`) must dispatch or refuse, never
  miscompile; SETTER's precision only hides it. Fix it under any option.
- Stage numbering: "stage 2" above is the zero-based `FAPassStage` index;
  "stage 3" / "stage 8" in the options are the one-based ladder comments.

Order: (0) the clause probe on `re` and `go`; (1) the `PYC_NOSETTER=1`
triage; (2) Option 3's coarsening probe, only if it still discriminates.

### Measurement 0: which clause admits the splits (2026-11-25)

A temporary probe (`PYC_DBG_SETTERWHY`, removed) logged, at every admission
by `setter_contour_demanded`, each demanded member and the exact clause.
For `poly_recv` it also logged the send's site and target functions, and
for `viol_reach` the violations whose backward reach holds the member. It
was run on `re` (the three `re_*.py` tests concatenated) and on `go`.
`viol_reach` is empty for `re`, and **every admission in both programs
comes from just two clauses**.

**`re`: all 1,593 admissions are `poly_recv`, from three `print` sends.**
None is irrepresentable, a violation, or a callee union. Most arrive
through the content walk.

| send | receivers in the member | what it is |
| --- | --- | --- |
| `re_all.py:69` `print(..., m.groups(), ...)` → 2 × `__str__` | `{str, None}` (1,251) | a real optional (`groups()` can hold `None`). It is in every part of any split, and a null test resolves it |
| `re_all.py:56` `print(re.compile(...), ...)` → 8 × `__str__` | `{Pattern, Pattern, Pattern, Pattern}` (k = 2 to 4) | four contours of ONE class, all dispatching to one `Pattern.__str__`. The eight targets come from the send's other receivers |
| `re_all.py:43` `m.lastindex` → 2 × `__pyc_get_lastindex__` | (24) | |

The second row is the feedback loop behind the tail. SETTER splits
`Pattern`, so a member now holds k `Pattern` contours. `sos_member_demanded`
counts *two or more receiver CreationSets of a multi-function send*, never
whether those CreationSets reach different functions. So the split's own
product is the next demand, k grows 2 → 3 → 4, and each step pays a round
of `TYPE_CONFLUENCE` method splits.

**`go`: `UCTNode`'s split (pass 10, stage SETTER) is admitted on two
clauses, both invariant under the split:**

| members | clause | the demand |
| --- | --- | --- |
| `parent`, `bestchild`; `pos_child` via content | `poly_recv(k=2, nil)` | `go.py:337` `if not child` → 6 × `__not__` on `{None, UCTNode}`. A real optional, null-testable |
| `losses`, `pos`, `wins`; `unexplored` via content | `viol_reach` | BOXING (kind 6) `{int64, float64}` violations: `count@score`, `__getitem__`, a tuple. They are the numeric mixes behind the `maxscore` coercion warning. Every member on their reach is a single `int64` |

So the 11-08 question "which violation is it?" is answered. It is the numeric
coercion family (`score`, not only `maxscore`), and no `UCTNode` partition
can change it. The `__new__`/`__init__` EntrySet admissions (passes 8, 9,
22–26) are the same `bestchild` and `pos_child` `{None, UCTNode}` dispatch,
reached through the content walk.

**What it means.** Each admitting demand is a union that every part of the
proposed split would still hold, or that the split itself created. That
gives the missing test a precise form: **a demand admits a split only if the
partition SEPARATES it**, meaning the parts receive different subsets of the
demanded union, or receivers that reach different targets. That is
Option 2's per-demand question, and Option 1 gets it for free, since
CSBACKTRACK partitions by what reaches the demand. It also explains 11-08's
stop condition: a per-member test on the merged contour cannot ask it.
Whether a partition separates a union depends on the partition, not on the
member.

Two local defects follow, separable from the strategy choice:

- `sos_poly_recvs` should require the member's receivers to reach **two or
  more distinct targets** of the send. A receiver set of one class is
  monomorphic at that send. This alone should break `re`'s `Pattern` loop.
  It does not touch `{str, None}` or `go`.
- `{None, T*}` dispatch and `viol_reach` through single-typed members are
  never separable by a CS split. They are the 11-08 modes 2 and 3, and that
  section's corpus losses (dijkstra2, plcfrs, pylife, sunfish) are still
  unexplained. Run the same probe on those four before any gate is rebuilt.

The probe's patch is reproducible from this text: about 160 lines in
`fa.cc` (`setter_contour_demanded`, `sos_build_demand`).

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
