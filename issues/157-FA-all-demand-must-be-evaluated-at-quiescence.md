# ifa/157 — ALL demand must be evaluated at quiescence

**Status:** in-progress, opened 2026-09-15.

**Author's directive, 2026-09-15:** *"All demand should be done after
quiescence. All."*

This is an architecture issue, not a bug report. It says WHEN a demand test
may run, and today essentially none of them run at that time — either because
they are edge-triggered and have gone blind by then, or because they are
gated on a quiescence that never arrives.

## The rule

A demand is a property of the CONVERGED types: *this AVar holds a union that
something cannot proceed on*. It is therefore asked **once types have reached
a fixed point**, **level-triggered**, **re-asked every pass**.

The contrast is an *edge-triggered* test, which asks *did a writer just bring
something new?* That fires while the union is FORMING, once, and then goes
blind — because once the union has settled nothing is new, and the demand
that is by then plainly visible is never reported again.

Both forms observe the same union. Only one of them observes it at a time
when the analysis can act on it.

## The measurement

**1. Stage 1's detector is edge-triggered.** `collect_type_confluence`
(`ifa/analysis/fa.cc:6060`-ish) flags an AVar only on the pass some writer
contributes a type it does not already have:

```c
if (x->out->type->n && type_diff(av->in->type, x->out->type) != bottom_type) {
  confluences.set_add(av); trigger = x; break;
}
```

Counting `IFA_DBG_CONFLUENCE` reports on `softrender`, restricted to formals
whose type is exactly `{int64, float64}` — the mix that whole investigation
was about:

| | count |
| --- | --- |
| `added=0` (the mix is present, nothing flagged) | **1184** |
| `added=1` (flagged) | 47 |

**96% of the time the union is sitting right there and no confluence is
raised.**

**2. Quiescence, which stages 6+ are gated on, is essentially never reached.**
Measured with `PYC_DBG_QUIESCE` (added at the `PER_CS_RECEIVER` gate in
`run_split_stages`, `fa.cc:12324`):

| program | passes reaching quiescence |
| --- | --- |
| `softrender` | **3 of 56** |
| `fysphun` | **3 of 18** |
| `sudoku4` | **0 of 19** |

Stage counts agree: `TYPE_CONFLUENCE` fires on nearly every pass (56 on
`softrender`, 19 on `sudoku4`), while `PER_CS_RECEIVER` and `CSM_ELEMENT_CS`
fire **never**.

**3. These two facts are the same fact.** An edge-triggered stage 1 keeps
finding transient work every pass, so `analyze_again` is never 0, so no stage
that waits for quiescence ever runs. The ordering is right on paper and
inoperative in practice.

## Why this is upstream of several open issues

- **ifa/156 (int/float)** — a formal holding `{int64, float64}` is invisible
  to an edge-triggered detector after the pass it formed on. No new demand
  SOURCE is needed for it; a different TIME is.
- **ifa/146's ESBLOCK trade** — `sudoku4` vs `softrender`, where the lever
  moves the failure rather than removing it. Both arms are demands being
  evaluated mid-formation.
- **ifa/148's stage-1 starvation** — "stage 1 claims ~35 of 41 passes and
  starves everything below it" is this issue's symptom stated per-stage.
- **ifa/045 / ifa/075** — `PER_CS_RECEIVER` and `CSM_ELEMENT_CS` are not
  disabled; they are unreachable.

## The one place the target architecture already exists

`split_css_by_defs(int quiescent)` — route 4, `CS_DEF_PARTITION` — takes
quiescence as a **parameter** rather than being gated out by it. It runs
every pass and *tightens* its criterion once quiescence is reached. That is
why ifa/143's level-triggered `csdemand` works at all, and it is the shape
every other stage should have:

> a stage is not "allowed to run only at quiescence"; it is TOLD whether the
> types it is looking at are converged, and asks a correspondingly strong or
> weak question.

## FIRST ATTEMPT, measured dead: level-triggering the detector

**2026-09-15.** The obvious repair — ask the LEVEL question alongside the
edge one, *does this AVar HOLD an irrepresentable union?*, on the converged
type — was built (`PYC_CONFLEVEL`) and measured. Three formulations:

| | predicate |
| --- | --- |
| `=1` | irrepresentable, excluding a pure numeric mix |
| `=2` | the same, INCLUDING one (ifa/156's int-vs-float case) |
| `=3` | irrepresentable AND something STUCK on it — a recorded type violation or an unresolved dispatch argument (`confluence_is_demanded`) |

**All three are dead.**

| `softrender` | confluences (p=1) | stage-1 `d_ess` (p=1) | warnings | quiescent |
| --- | --- | --- | --- | --- |
| off | 923 | 347 | 13 | 3 of 56 |
| `=2` | 3295 | **348** | 13 | 3 of 55 |
| `=3` | +20599 nominated | unchanged | 13 | 3 of 55 |

**3.6× the candidates and ONE more split.** And on `sudoku4` it is worse than
inert — warnings 39 → **206** (`=2`) / **54** (`=3`) — which is
[146](146-remove-all-arbitrary-splitting.md)'s non-monotone diagnostic. Per
CLAUDE.md the lever was DELETED rather than defaulted off. What survives is a
probe (`IFA_DBG_CONFLEVEL`), which counts and changes nothing.

### Why it cannot work — two measurements

**1. 83–87% of confluences never reach a partitioner.** Stage 1's only
actuator is *split an EntrySet on a FORMAL* (or on a return value). A union
observed on a non-formal rvalue — a local, a temporary — is counted and
dropped at `tc_skip_rval`. `IFA_DBG_INCOMPAT` on `softrender` p=1:

```
off  stage1 seen=923  skip(rval=769 lval=0 cs=64)  dec=90  split(formal=66)
=2   stage1 seen=3295 skip(rval=2861 lval=0 cs=280) dec=93 split(formal=67)
```

2372 extra candidates moved `dec` from 90 to 93. `skip(lval)` is **0** on
every pass of every program measured: a return value is never a confluence.

**2. In the rest there is nothing to partition** — every in-edge already
carries the union, so `etype == stype` and
`edge_type_compatible_with_entry_set` sees no disagreement. That is ifa/146's
self-blinding, and it is stop condition 1 below, hit exactly as written.

### And the framing was wrong: types were never stale

`analyze_to_convergence` drains the edge, send and ES worklists **to empty**
before calling `extend_analysis` → `run_split_stages`. So the types every
split stage reads are ALREADY at a fixed point, on every pass, today. The
edge-triggered detector is not reading stale types; it is reading converged
types and asking a question about their *history*.

**Which means the `!analyze_again` gate is not a quiescence gate.** It is
named one throughout `fa.cc` — "runs only on quiescence of all stages above",
"only on full quiescence of stages 1-5" — and what it actually tests is *did
a higher-priority stage act this pass*. Types are converged on both sides of
it. It is a CASCADE PRIORITY gate wearing a convergence gate's name, and it
starves every lower stage for as long as a higher one has work, which
([148](148-stage-5-starvation-root-caused.md)) is every pass, because a
separation propagates exactly one contour per pass.

So `PYC_DBG_QUIESCE`'s 3-of-56 and 0-of-19 are real but they do not mean what
the name says. They measure how often stage 1 ran out of work, not how often
the analysis converged — it converged every time.

## SECOND MEASUREMENT: a confluence does not come from nowhere

**Author, 2026-09-15:** *"We know shedskin resolves the types. The contours
are resolvable. So what is the missing mechanism? A confluence at a return
must be from different callee entry sets or functions. It does not come from
nowhere."*

Correct, and the first attempt's write-up stopped one question short. "769 of
923 confluences are non-formal rvalues" says where the demand is OBSERVED. It
says nothing about where the union came FROM — and a union at `r = f(x)` has
exactly one source, the callee contours that returned into it
(`flow_vars(ee->to->rets[i], ee->rets.v[i])`, fa.cc:4523).

`IFA_DBG_RETCONF` classifies every skipped rvalue by its writers.

### 1. They are unresolved DISPATCHES, not merges

| | `softrender` p=1 | `sudoku4` p=3 |
| --- | --- | --- |
| callee returns DISAGREE (`ret_differ`) | **363** | **102** |
| ...from different **Funs** | **363** | **102** |
| ...from different contours of ONE Fun | 0 | 0 |
| callee returns agree (`ret_same`) | 0 | 0 |
| one callee, already unioned (`ret_one`) | 0 | 0 |
| local merge, same contour | 54 | 18 |

**`funs == ess` in every single case.** Not one is a function split into
several contours; every one is several *different functions* reached from one
call site. `IFA_DBG_RETCONF_V` shows what they are:

```
[retconf] av=3004 var=(anon) in=cross ess=5 funs=5
    <- __iter__/es228: __list_iter__#1043
    <- __iter__/es166: __set_iter__#1484
    <- __iter__/es45:  __base_iter__#1017
    <- __iter__/es158: __dict_iter__#1422
    <- __iter__/es205: __tuple_iter__#1035
[retconf] av=3137 var=(anon) in=cross ess=5 funs=5
    <- __next__/es52:  str#8
    <- __next__/es183: bool int64 str None list dict tuple set ...
```

One `for x in seq` whose receiver unions list/tuple/set/dict. `__iter__`
fans to five iterator classes; the iterator is then a union, so `__next__`
fans too, and the loop variable becomes the whole world. Note the first
`__next__` contour returns exactly `str` — the precise answer is *right
there*, in a contour that already exists.

### 2. The thing that decides the dispatch is never a formal

In single-dispatch OOP one argument position picks the `Fun`. Classifying it:

| receiver is | `softrender` p=1 | `sudoku4` p=3 |
| --- | --- | --- |
| a **FORMAL** of the caller | 2 | 2 |
| a local rvalue | 307 | 50 |
| **CreationSet**-contoured (a field/element read) | 54 | 50 |

Stage 1's only actuator is "split an EntrySet on a formal". The value that
decides these dispatches is a formal **2 times out of 363**.

### 3. But the walk back always lands on an actuator

Backtracking each local receiver through writers that still CARRY a union —
ifa/152's walk, on the EntrySet side:

| walk terminates at | `softrender` p=1 | `sudoku4` p=3 |
| --- | --- | --- |
| a FORMAL (stage 1's actuator) | 15 (avg 5.3 hops) | 1 (avg 2.0) |
| a **CreationSet** (route 4's actuator) | 183 (avg 2.0 hops) | 49 (avg 4.7) |
| the JOIN — writers upstream are all single-typed | 109 | 0 |
| hit the 32-hop cap | **0** | **0** |

**Nothing is lost.** Every demand terminates somewhere nameable, in two to
five hops. Totalled with the direct receivers:

- `sudoku4`: 102 dispatch demands → **3 reach a formal, 99 reach a
  CreationSet.** 97% are route 4's job.
- `softrender`: 363 → 17 formal, 237 CreationSet, 109 at a join where several
  single-typed writers meet (the classic confluence, actionable on its own).

## THE MISSING MECHANISM

**There is no path from an EntrySet-contoured demand to route 4.**

Both halves exist. A CS-contoured confluence is handed to route 4 today —
`tc_skip_cs` adds it to `tc_cs_dropped` with the comment *"hand it to the
pass's last rung rather than dropping it"* — and ifa/152 built the backtrack
that finds the merged CreationSet upstream. An ES-contoured non-formal rvalue
gets `++tc_skip_rval` and a log line, and is dropped on the floor.

So the analysis observes ~99 demands per pass on `sudoku4` whose actuator is a
CreationSet split it already knows how to perform, and throws every one of
them away at the boundary between the two halves of a mechanism it has
already built.

That is CLAUDE.md's own rule, unimplemented on one side:

> The demand is observed where the union is USED, which is almost never where
> the merge happened. **Backtracking is therefore not optional**; a demand
> evaluated only at the point of observation cannot reach the merge.

### Why this is a demand and not a fact

- The demand is an **unresolved dispatch** — one call site, N callee `Fun`s,
  whose returns disagree. Something observed a distinction and could not
  proceed. Not "this type is a union".
- It would **not fire without the demand**: the walk runs only for a
  confluence whose callee returns actually differ.
- The handle is the receiver's **type** and the CreationSet partition, never
  where the value came from.
- It is what CLAUDE.md already specifies as `PYC_CPA`'s replacement, in
  `tests/splitter_cartesian_product.py`'s known-issue text: *"demand (an
  unresolved dispatch or an irrepresentable union) plus dispatch-aware
  filtering of the RECEIVER, which in single-dispatch OOP is the position
  that determines dispatch."*

## "Split the list comprehension" — it is already split

**Author:** *"So the key is splitting the list comprehension. That should be
easy, just unzipper it top down."*

Measured, and the unzipping has already happened. The merge is one level below
it, and it is not where the comprehension is.

### The dict comprehensions are already separate

`sudoku4` builds three dicts from three comprehensions whose values are a
`list`, a `set` and a `str`:

```python
units  = dict([(s, [u for u in unitlist if s in u]) for s in squares])
peers  = dict([(s, set([s2 for u in units[s] for s2 in u if s2 != s])) for s in squares])
values = dict([(s, digits) for s in squares])
```

`IFA_DBG_FUNES=__pyc_dict_from_iterable__` — **three contours, one per call
site, each taking a different accumulator list**:

```
es=75  args= [__pyc_dict_from_iterable__#479] [list#1850]
es=388 args= [__pyc_dict_from_iterable__#479] [list#1852]
es=389 args= [__pyc_dict_from_iterable__#479] [list#1856]
```

### The blocking contour is a different comprehension, and it is also split

`cs=1849` is the `SEPARABLE-UNRELATED` element confluence (`defs=6`, elements
`{str, set, list}`), and it is a product of an earlier route-4 split — group 1
of 12 from the start-merged `list` contour `cs=1010`. `IFA_DBG_CSDEFS=1849`:

```
CSDEFS cs=1849 sym=list defs=6
  def av=2847  in=cross  es=44   line=17     <- [a+b for a in A for b in B]
  def av=12852 in=cross  es=185  line=17
  def av=12947 in=cross  es=186  line=17
  def av=12975 in=cross  es=187  line=17
  def av=3643  in=__init__ es=77 line=2273   <- __pyc__ builtin
  def av=5769  in=split  es=120  line=738    <- __pyc__ builtin
```

**`cross` already has four EntrySets and four separate creation points.** The
comprehension is unzipped top-down exactly as proposed. All four creation
points, plus two from unrelated `__pyc__` builtins, still land in **one
CreationSet**.

### Why route 4 cannot separate them

Its partition key is one bit per assign set — *which assign sets does this
creation point flow into*. All six carry the identical signature:

```
[csdefsplit] cs=1010 def av=2847  -> cs=1849 (group 1/12 sig=0100000000010)
[csdefsplit] cs=1010 def av=12852 -> cs=1849 (group 1/12 sig=0100000000010)
[csdefsplit] cs=1010 def av=12947 -> cs=1849 (group 1/12 sig=0100000000010)
...
[csdefsplit] cs=1849 KEY sets=4 defs=6 groups=1 informative=1
[csdefsplit] cs=1849 defs=6 DECLINED (1 group: every creation point on the same assign sets)
```

**The key says where the container GOES. The demand is about what it HOLDS.**
Those are different questions, and the second one is never asked — which is
[133](133-split-a-container-on-its-element-type.md)'s title.

### And the contours that would name the partition already exist

`cs=1849`'s element channel writers, from `IFA_DBG_CSDEFS`:

```
  <- __setitem__/es=289 : str
  <- __setitem__/es=562 : list          <- already one contour per element type
  <- __setitem__/es=563 : set
  <- __setitem__/es=529 : list
  <- append/es=210      : str set list  <- these have absorbed the union
  <- append/es=560      : str set list
```

**`__setitem__` is already split per element type.** One contour writes only
`str`, another only `list`, another only `set`. The analysis has done the hard
part; the three of them write into one element channel because the list they
write into is one CreationSet.

So this is CLAUDE.md's rule with the pieces visible: *the confluence is a
CONTOUR, not a program point*. The writers are separable and separated. What is
missing is a partition of the CONTAINER, and the only key route 4 has is blind
to the element type that the demand is entirely about.

**So splitting the comprehension is not the lever.** The levers are, in order
of how close they are to the evidence:

1. **Give route 4 an element-type key** — partition creation points by the
   element types their downstream writers contribute, alongside the assign-set
   key. The `MEMBER-KEY` path (`m_informative`/`msig`) in `split_css_by_defs`
   is the place this plugs in; it already tries a second key and keeps it when
   it yields more groups. This is [133](133-split-a-container-on-its-element-type.md).
2. **Or partition by SETTER**, which is the same information reached from the
   other side: `__setitem__/es=562` and `es=563` are distinct setters and the
   setter machinery (`same_eq_classes`, `elemsetter_enabled`) already exists.
3. Note neither is an ES split. `cross` splitting further cannot help — all
   four of its contours produce `list[str]`, so no partition of ITS creation
   points separates `str` from `set` and `list`.

## WHICH dispatch is ambiguous? All of it is class-based, on the receiver

**Author:** *"Python has static dispatch, class based dispatch and
closure/function variables. Which is the source?"*

"N candidate Funs" is not yet a diagnosis — the three have different fixes.
The discriminator is structural and is in `find_visible_functions`
(`if1/pattern.cc:1477`). `args[0]` — MPosition 1, the first of
`positional_arg_positions` — is the CALLEE position:

| `args[0]` holds | resolved by | means |
| --- | --- | --- |
| one CS with `sym->fun` set | directly | **static** |
| several CSs with `sym->fun` set | `function_values` | **function variable / closure** |
| CSs with `sym->fun` UNSET (a name) | `visible_functions(sym)`, then the other positions narrow by type | **class-based** |

`IFA_DBG_RETCONF`'s `KIND` line classifies every ambiguous call that way. The
answer is not mixed:

| | `sudoku4` | `softrender` |
| --- | --- | --- |
| static | **0** | **0** |
| function variable / closure | **0** | **0** |
| class-based, narrowed by the **receiver** (position 2) | **102** | **363** |
| class-based, narrowed by a LATER position | **0** | **0** |
| classes in the receiver, mean | 6.7 | 5.9 → 2.0 |
| `related` (share a user-defined ancestor) | **0** | **0** |
| **DISTINCT receiver unions** | **5** | **9** |

**Three things fall out of that.**

**1. Closures and function variables contribute nothing.** Neither program
dispatches through a function value at an ambiguous site. Every one is
`x.m()` with a union receiver, which is Python's single dispatch — and
`class/arg=0` says pyc never resolved on a non-receiver argument either, so
there is no multi-method modelling artifact to chase.

**2. It is not polymorphism.** `related=0` everywhere: no receiver union
shares a user-defined ancestor, so `classes_are_related` says SPLIT, not
HOIST. And the union itself is the whole world —

```
[kind] recv of __iter__:     UNRELATED(split) classes: dict bool list tuple set int64 str
[kind] recv of __getitem__:  UNRELATED(split) classes: dict bool list tuple set int64 str
[kind] recv of __eq__:       UNRELATED(split) classes: dict bool list tuple set int64 str
```

Seven types, four containers and three scalars, in one variable. No `sudoku4`
variable holds that. By CLAUDE.md's directive this is a `{scalar, container}`
union pyc INVENTED, not a property of the program.

**3. And there are only FIVE of them.** `distinct_unions=5` accounts for all
102 ambiguous sites on `sudoku4`; 9 for softrender's 363. The ambiguity is not
distributed — it is a handful of merged contours broadcast through every
container operation in the program, because once a variable holds all seven
types, every `for`, every `[]`, every `==` on it becomes an unresolved
dispatch.

**So the dispatch ambiguity is a SYMPTOM, not the disease.** Routing these ~100
demands is treating the broadcast; the disease is the 5 merges upstream, which
is `sudoku4`'s `cs=1849` — the comprehension accumulator whose element unions
`{str, set, list}` from `dict([(s, …) for s in squares])`, already named as
[146](146-remove-all-arbitrary-splitting.md) E's blocker. Fixing 5 merges
fixes 102 dispatch sites.

That also explains why the link below delivers and buys nothing: it aims
correctly at the receiver, and the receiver's union has one creation point 72%
of the time, because it was merged somewhere else entirely.

## THE LINK, BUILT (`PYC_RETDEMAND`, default 0)

At `tc_skip_rval`, instead of dropping: if the callee returns disagree, find
the position that decides the dispatch, and hand the demand to whichever
actuator it lands on — `decide_entry_set_split` for a formal,
`tc_cs_dropped` (route 4) for a CreationSet, via ifa/152's walk when the
receiver is a local.

Guarded by `classes_are_related`, extracted from `report_elem_confluence`
where it had been sitting inside a diagnostic: classes sharing a
**user-defined** ancestor are legitimate polymorphism and must be HOISTED,
never split — splitting `richards`' four `Task` subclasses is what broke it.

**It delivers.** `sudoku4`, per pass: 3 demands routed to a formal, 37-52 to a
CreationSet, 321-465 declining (not dispatch confluences — field and element
reads, a different case).

**And it changes nothing, because everything downstream declines.** Three
guards, in order, each already known:

| | `sudoku4` |
| --- | --- |
| routed CreationSets with **one** creation point — nothing to partition | **72%** (72 of 99) |
| ifa/152's backtrack now REACHES upstream contours (`BTREACH`) | **0 → 135** |
| ...rejected by its "supplies none" guard | **123 of 135** |
| route 4's own decline, *"1 group: every creation point on the same assign sets"* | 31 → 43 |
| net new CreationSets, warnings, compile result | **0 / 0 / unchanged** |

Three things worth keeping from that:

1. **ifa/152's backtrack fires ZERO times on `sudoku4` today.** It is not
   declining — it is never reached, because the one-def candidates that would
   trigger it only exist if an ES-side demand is routed in. This link is what
   turns it on (0 → 135 reaches).
2. **The routed contour is often the wrong one.** 555 `closure` records per
   pass: the receiver of a dispatch frequently lives in a closure, and the
   record that STORES a value is not the contour that merged it. Handing over
   `a->contour` is right for a member/element channel (what `tc_skip_cs` does)
   and wrong here.
3. **The remaining blocker is ifa/146 E verbatim** — every creation point on
   the same assign sets. The link cannot pay off until route 4 can partition
   what it is handed.

So the mechanism is built and correct, and it is **blocked on two named open
issues**, not on itself. Default 0 until one of them moves; it is kept rather
than deleted because it is the only path from an EntrySet-side demand to
route 4, and the work on those guards needs it to exist.

## The fix, restated

The directive stands and is already satisfied in the one sense that was in
doubt: **demand IS evaluated on converged types.** The defect is the cascade.

1. **Stop calling it quiescence.** Rename the gate and its comments to what
   they test. The misnomer is load-bearing: it is why "stages 6+ are starved"
   read as a convergence problem for two issues running.

2. **Give every stage the route-4 shape.** `split_css_by_defs(int quiescent)`
   takes the condition as a PARAMETER and runs every pass, asking a weaker
   question early and a stronger one late. `PER_CS_RECEIVER` and
   `CSM_ELEMENT_CS` should do the same, so a stage with a genuine demand can
   act instead of never running — 0 firings across every program measured.

   **Not by lifting the gate.** That was `PYC_RECVFAN=2`, removed 2026-09-07,
   and its note states the rule: *"the answer has to be a reason this stage
   may act, not permission to act without one."* Each stage needs its demand
   test written before it gets the parameter.

3. **Route the ES-side demand to an actuator instead of dropping it** — the
   missing mechanism above, and the item with the measured headroom. At
   `tc_skip_rval`, when the callee returns disagree, walk back through
   union-carrying writers and hand the result to whichever actuator it lands
   on: `decide_entry_set_split` for a formal, `tc_cs_dropped` (route 4) for a
   CreationSet. Both already exist; only the link is missing. 100% of walks
   terminate, 2-5 hops, no cap hits.

   The `join` bucket (109 per pass on `softrender`, 0 on `sudoku4`) is a
   third case and needs its own answer: the union is minted where several
   single-typed writers meet, so there is no upstream contour to partition and
   the split has to be of the contour holding the join itself.

## Stop conditions, written before measuring

- ~~If level-triggered detection finds a demand at quiescence but **nothing to
  partition**, the detector is not the blocker and the issue moves to the
  partitioner.~~ **HIT, 2026-09-15.** 3.6× candidates, one extra split.
- ~~If making stage 1 level-triggered *increases* contour counts corpus-wide,
  it is failing ifa/146's non-monotone diagnostic.~~ **HIT.** `sudoku4`
  39 → 206 warnings. Lever deleted, negative result recorded above.
- If quiescence is reached but stages 6+ then fire on every pass forever, the
  gate was load-bearing for termination and the stages themselves need demand
  tests before they need permission.
- For step 3: if the skipped rvalues backtrack to formals whose in-edges
  already agree, stop — the actuator is not the limit either.

## Verification plan

- six CI gates (CLAUDE.md "Change acceptance")
- `IFA_DBG_CONFLEVEL` — of the AVars whose CONVERGED type is an
  irrepresentable union, how many does the edge-triggered test flag?
  Baseline: `softrender` flagged=11890 unflagged=**34878** (75% blind),
  `sudoku4` flagged=1327 unflagged=**12662** (90% blind). This generalizes
  ifa/156's 47-vs-1184 from numeric formals to every irrepresentable union —
  and the attempt above shows closing it is not sufficient.
- `IFA_DBG_INCOMPAT` — `seen` / `skip(rval)` / `dec` / `split(formal)`.
  Baseline `softrender` p=1: 923 / 769 / 90 / 66.
- corpus `-m check` A/B against `main`, both arms freshly built
  (`sweep-measures-the-binary-not-the-tree`)

## See also

- [156](156-FA-split-int-from-float-coerce-last.md) — where this was found;
  its "CORRECTION" section is the origin of this issue
- [146](146-remove-all-arbitrary-splitting.md) — the non-monotone diagnostic
  this must pass
- [148](148-stage-5-starvation-root-caused.md) — stage 1 starving what is
  below it, stated per-stage
- [143](143-shared-container-method-contours-refuse-cs-splits.md) — the
  level-triggered `csdemand` that already works
- [133](133-split-a-container-on-its-element-type.md) — `PYC_CONFDEMAND`, the
  probe whose predicate step 1 reuses
