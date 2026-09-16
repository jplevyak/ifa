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

## "So the CS has to split" — yes, and BOTH ways of forcing it are dead

Agreed, and that was tested two ways. Both levers were built, measured, and
**deleted** (ifa/146's non-monotone diagnostic). The results are the
deliverable.

### Attempt 1 — split coarser and let the analysis re-derive (`PYC_CSFAN`)

CLAUDE.md's corollary: *"a merge you cannot undo is not a reason to record
provenance. It is a reason to split coarser and let the analysis re-derive."*
Every pass re-derives from bottom, so give each creation point its own contour
and let the next pass attribute the element writes by flowing them.

| `sudoku4` | off | on |
| --- | --- | --- |
| warnings | 39 | **61** |
| errors | 0 | **1** |

It fires once, at p=1, on the start-merged parent (`defs=26`), and yields 26
contours of which **every one still carries the whole union** — turning one
splittable `defs=6` contour into 26 unsplittable `defs=1` ones, which is
ifa/133's residual family, manufactured.

**Why**, and this is the useful part. `append`'s contours, without the fan:

```
es=560 args= [append] [list#1849] [set#1400]     <- two contours,
es=561 args= [append] [list#1849] [str#8]           ONE receiver CS
```

Two method contours already separated by element type, writing into one
element channel because the receiver is one CreationSet. And *with* the fan:

```
es=630 args= [append] [list#1010 list#1656 list#1678 list#1679 list#1680 list#2392] [str#8]
```

**One method contour over SIX receiver CreationSets.** The shared container
method re-merges on the next pass exactly what the fan just separated.

### Attempt 2 — then split the container methods per receiver CS

That is `CSM_ELEMENT_CS` (ifa/075, `PYC_CSM=2`), and it is precisely
[143](143-shared-container-method-contours-refuse-cs-splits.md)'s title. It
**never runs** on `sudoku4` — the cascade gate this issue opened with:

```
PYC_CSM=2                   STAGES: TYPE_CONFL CS_DEF_PART          39 warnings
PYC_CSM=2 + gate lifted     STAGES: TYPE_CONFL CSM_ELEM_CS CS_DEF_PART   53 warnings, 9 errors
PYC_CSM=2 + gate + CSFAN    STAGES: TYPE_CONFL CSM_ELEM_CS CS_DEF_PART  268 warnings, 570 errors
```

**The prediction failed.** I expected the starved stage to be the enabler. With
the gate lifted the stage runs and the element confluences are
**byte-identical** — 6 confluences, 1 separable, 5 fused, in both arms — while
errors go 0 → 9. So splitting container methods per receiver CS does not
separate these channels, and the shared-receiver union is not the whole
re-merge.

*(The gate lift was an experiment, not a proposal. Lifting a quiescence gate so
a stage may act without a reason is `PYC_RECVFAN=2`, removed 2026-09-07.)*

### What stands

- **The CS does have to split.** The union is an element channel of a merged
  CreationSet and nothing downstream can repair it.
- **No key currently names the partition.** Route 4's says where the container
  FLOWS; the demand is about what it HOLDS.
- **And forcing the split without a key is self-defeating**, in a way that is
  now measured rather than argued: creation-point fanning manufactures
  `defs=1` contours, and the shared method contour re-merges them.

So the open question is narrower than when this section started, and it is not
"how do we make it split" but **what names the partition**. The evidence points
at the element writers — `__setitem__/es=289 → str`, `es=562 → list`,
`es=563 → set` are already distinct contours carrying exactly the distinction
the demand is about — and at getting that information into a key, which is
[133](133-split-a-container-on-its-element-type.md) and the `MEMBER-KEY` path
in `split_css_by_defs`. Attempt 2 shows that splitting the method contours is
not by itself that key.

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

~~**The key says where the container GOES. The demand is about what it
HOLDS.**~~ **Withdrawn** — see the shedskin comparison below.
`CSFlowGraph::keys` is a `Vec<AType *>`, one AType per assign set, exactly
shedskin's `assignsets.setdefault(merge_simple_types(types), …)`. pyc's assign
sets ARE type-keyed, so each signature bit already means *this creation point
reaches an assignment of type T*. The key is not blind to type. What is missing
is a rung BELOW it.

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

## "For CS it is by setters" — that mechanism exists and has never run

**Author:** *"Splitting must be by demand, for cs it is by setters. So no
arbitrary fans. Are the cheaper absences demand, principled and testable?"*

### The two shedskin absences, against that test

| | demand? | principled? | testable? | verdict |
| --- | --- | --- | --- | --- |
| rung 3 (`prt`) | **no** — a partition KEY, not a demand | yes (deduced types, no provenance) | yes | **provably useless** |
| `ifa_confluence_point` | **no** — a locator; "a creation point appears in two assign sets" is a FACT, the same shape as "this formal's type is a union" | yes | yes | not the mechanism |

`prt` is *dominated*: pyc's key is one bit per assign set; `prt`'s is the
flattened union of the types of the sets a csite lies on. Same bitstring ⇒ same
union, so its equivalence is coarser-or-equal and it can never produce more
groups than pyc already produces. It cannot split anything pyc does not.

So neither is the answer. But the criterion in the question is, and it points
at code that already exists.

### `split_css(setter_starters)` is the CS-by-setter split, and it is starved exactly where it is needed

**Scope correction.** It is not unreachable in general — measured at the
default, `split_css` runs 31 times on `bh`, 38 on `go`, 8 on `richards`, 4 on
`chess`. It runs **zero** times on `sudoku4`. The stage works on the programs
whose stage 1 converges and is starved on the ones whose stage 1 never does,
which is this issue's thesis stated per-program: the cascade gate starves the
lower stages precisely on the programs that need them.

`split_for_setters` ends in `split_css(setter_starters)` — partition a
CreationSet by setter equivalence classes. That IS "for cs it is by setters".
It sits behind **two** gates: the outer `!analyze_again` in `run_split_stages`,
and an inner `if (analyze_again) return 1` that preempts it whenever any
earlier stage acted this pass. `PYC_SETTERGATE=1` lifts only the outer one.

Measured on `sudoku4`:

| | `split_css` reached | preempted | warnings | compiles |
| --- | --- | --- | --- | --- |
| default | **0** | 0 (stage never runs) | 39 | no |
| `PYC_SETTERGATE=1` | **0** | 17 | 36 | yes |
| `PYC_SETTERGATE=2` (new, both gates) | **22** | 0 | **15** | yes |

`=1`'s compile fix comes from the *EntrySet*-side setter split
(`split_ess_setters`, 2 wins) — `split_css` still ran zero times. **The CS
criterion had never executed on this program at all.**

### Lifting the gate is not the fix — the gate was standing in for a demand test

`collect_setter_confluences` builds `setter_starters` with **no demand test**:
every AVar whose setters write a CreationSet it allocates, i.e. essentially
every container allocation in the program. So `split_css` on it is a wholesale
setter partition of everything, and `analyze_again` was throttling it rather
than qualifying it.

Corpus `-m check`, all three arms on `f13e1b2e`, fresh builds:

| | default | `=1` | `=2` |
| --- | --- | --- | --- |
| compile_fail | 3 | **2** | 3 |
| run_fail | 35 | 36 | 36 |
| stdout_differs | 25 | 25 | **24** |
| container CS / shapes | 2051/614 = 3.34 | 2110/617 = 3.42 | **2694/616 = 4.37** |

`=2` is **+31% container CreationSets**, and its two verdict changes trade
against each other: `sudoku4` compile-fail → compiles, `sudoku5` runs →
**compile-fail**. No program that matched CPython moved (same four in every
arm). Warnings: `pygasus` 52 → 2 and `sudoku4` 39 → 15, against `linalg`
33 → 137 and `plcfrs` 122 → 183. Non-monotone, so not a default.

### And the obvious repair is dead too

Keep the setter criterion, supply the missing reason: offer only starters that
allocate a CreationSet something cannot proceed on — `cs_elem_irrepresentable`,
route 4's own `csdemand` predicate, so the two rungs agree on what a demand is.
That is `PYC_SETTERGATE=3`, and it is **worse than no filter at all**:

| | gate=0 | gate=2 (no filter) | gate=3 (demand-filtered) |
| --- | --- | --- | --- |
| `sudoku4` | fails, 39w | compiles, 15w | fails, 36w |
| `sudoku5` | runs, 19w | fails, 20w | fails, 61w |
| `linalg` | runs, 33w | runs, 137w | **FAILS**, 182w |
| `plcfrs` | runs, 122w | runs, 183w | **FAILS**, 378w |

Partitioning only the demanded containers while their siblings stay merged is
*less* stable than partitioning all of them or none — the split CreationSets
and the unsplit ones disagree about the same element channel. Mode 3 removed.

### The modes, and which is sound

Two priority gates and one candidate-set switch, all independent:

| | axis | effect |
| --- | --- | --- |
| `PYC_SETTERGATE=0` | — | SETTER stage runs only when stages 1-2 found nothing this pass; `split_css` then runs. Starved to zero on `sudoku4`, fine on `bh`/`go`. |
| `=1` | outer gate off | stage runs every pass, but the inner `if (analyze_again) return 1` still preempts `split_css`. Only the ES-side `split_ess_setters` gains. |
| `=2` | both gates off | `split_css` runs every pass over all starters. |
| ~~`=3`~~ | + demand filter on candidates | **removed** — partial application is unstable. |
| `PYC_ELEMSETTER=1` | candidate set | adds CreationSets with an irrepresentable element and seeds their starters from `cs->defs`. Orthogonal to the gates. |

And one thing that is **not** a mode, which is where the problem is:
`split_css` always partitions to the **finest** grouping setter equivalence
induces — it peels `same_eq_classes` groups until none remain — with no
reference to whether any demand required that distinction.

Scored against *principled, general, minimal*:

| | principled (gated by demand, not by priority) | general (uniform) | minimal (only demanded contours) |
| --- | --- | --- | --- |
| `=0` | **no** — gated on "did another stage act", a scheduling artifact, not a property of the program | yes | no — finest partition where it runs |
| `=1` | no | yes | n/a — `split_css` never reached |
| `=2` | **yes** | **yes** | **no** — maximal partition, universally. +31% contours |
| `=3` | yes | **no** — and that is what broke it | no |

**`=2` is the most principled and most general, and the least minimal.** No
existing mode is sound, and the failure is on the axis the project cares most
about.

### The sound mode does not exist, and the measurements say what it is

Every pass, no priority gate (`=2`'s axis). Uniformly over all CreationSets
(`=2`'s and `=0`'s generality — `=3` measured that partial application is
unsound). But partitioning to the **COARSEST setter-induced grouping that
discharges the demand**, not the finest:

- **setters supply the vocabulary** — the handle naming the parts;
- **demand supplies the trigger AND the stopping criterion** — keep separating
  setter classes only while some group's content is still irrepresentable, and
  stop the moment every group is representable.

Minimal by construction: no contour is minted that no demand asked for.

This is CLAUDE.md's reason/mechanism refinement applied to **granularity**
rather than to existence, and that is the step every attempt in this issue has
missed. `=3` applied demand to *which containers to split*; the sound version
applies it to *how far to split each one*.

It also explains all four measurements as one story:

| | demand consulted? | result |
| --- | --- | --- |
| `=0` on `sudoku4` | never — stage starved | no contours, fails to compile |
| `=1` | never | ES-side split only |
| `=2` | never — setters consulted, demand not | maximal partition, +31% contours |
| `=3` | as a container filter, not a depth limit | partial, siblings disagree, unstable |

Implementation is cheap and local to `split_css`: compute the setter classes as
now, then start from ONE group and separate a class only while the group's
content channel is irrepresentable — or equivalently compute the finest
partition and greedily merge back while each merged group stays representable.
`n` is small. The verification is the same three-arm corpus comparison used
above, with the stop condition written first: **if the coarsest-that-discharges
partition still costs contours corpus-wide, the setter vocabulary is not
expressive enough to name this demand, and the answer is not in this rung.**

### BUILT (`PYC_SETTERMIN`) — and the sound-in-theory mode is dead in practice

The design above was implemented and measured. **It is principled, general and
provably minimal for its demand, and it makes things worse.** Removed.

The construction: a union has no representation exactly when it mixes a basic
type with a pointer-shaped one, or two distinct basic kinds. So a group is
representable exactly when its written types are **all pointer-shaped** or
**all one basic kind**, and the unique coarsest valid partition is *one group
for every pointer-shaped writer, one group per basic kind*. Computable in one
pass from `s->out` over each starter's setters — no search, no heuristic, and
provably the coarsest partition that discharges the demand.

It does exactly what it claims:

| | container_cs | ess | css | warnings | compiles |
| --- | --- | --- | --- | --- | --- |
| `plcfrs` default | 152 | 1242 | 2440 | 122 | yes |
| `plcfrs` `SETTERMIN=1` | **142** ↓ | 1555 ↑ | 2739 ↑ | **322** | **no** |
| `sudoku5` default | 59 | 615 | 1400 | 19 | yes |
| `sudoku5` `SETTERMIN=1` | **50** ↓ | 767 ↑ | 1782 ↑ | **62** | **no** |

**Container CreationSets go DOWN, which is the goal, and everything else gets
worse.** With both priority gates also lifted it helps exactly one program
(`linalg` 137 → 54) and costs the rest (`sudoku4` 15 → 36, `plcfrs` 183 → 290
and stops compiling).

#### RETRACTED: "the over-split is justified"

The first write-up of this measurement concluded that the finest setter
partition was *"a conservative over-approximation"* whose extra contours were
*"the price of over-approximating a conjunction that cannot be evaluated
locally"*, and scored `PYC_SETTERGATE=2`'s non-minimality as **justified**.

**That is a retreat and it is withdrawn.** It is the exact failure CLAUDE.md
names: *"the numbers get much worse and the change still passes; the new rule is
described as conservative or safe; the underlying disagreement is worked around
rather than explained."* It even used the word.

**Author, 2026-09-16:** *"The overriding principle is minimal contours, all
demand driven splits. If some random arbitrary split happens to cause a program
to compile then it was hiding another bug, which should be root caused rather
than accepting any arbitrary split."*

So the finest partition is not justified by `SETTERMIN` breaking `plcfrs` and
`sudoku5`. It was HIDING what `SETTERMIN` surfaces. `PYC_SETTERMIN` is kept at
default 0 **as the instrument that exposes the defect**, explicitly not as a
lever to be judged on its corpus numbers.

#### Root-causing what it surfaces — `sudoku5`

Under `SETTERMIN`, `sudoku5` gains **118 `has mixed basic types` errors**, which
is the very demand this partition is built to discharge. Tracing it:

- The key is **not** unstable across passes (`IFA_DBG_REPRKEY`: new=13, same=102,
  **changed=0**), so a decision recorded from moving types is not the cause.
- `IFA_DBG_ELEMCONF`: default has **0** element confluences, `SETTERMIN` has 3,
  the largest being `cs=1033 sym=list defs=9 classes={int64, str, tuple}`.
- `IFA_DBG_CSDEFS=1033`: its nine creation points are **the nine rows of the
  sudoku grid literal**, `sudoku5.py:77-85`, all `list[int64]`.

**Merging those nine is CORRECT.** They are nine identical `list[int64]`
literals; no demand distinguishes them, and CLAUDE.md's premise says they should
share one contour. The default arm separates them only as a side effect of
setter-equivalence over-splitting — which is precisely the arbitrary split the
directive forbids.

The union does not come from the creation points. `cs=1033`'s element channel
has exactly **one** writer:

```
<- av=4253 in=__setitem__ es=80 : int64 str tuple#1087 tuple#1144 tuple#1146 tuple#1148 tuple#1150
```

and `es=80` has exactly one in-edge, from `solve_sudoku`:

```
es=80 args= [__setitem__] [list#1033] [int64 str tuple...] [int64 str tuple...]
  <- edge=1114 from=solve_sudoku es=65
```

That is `grid[r][c] = n` (`sudoku5.py:33`). **Both the index and the value
formal hold `{int64, str, tuple x5}`** — and a list index that can be a `str` is
already wrong on its own, independent of any contour question.

Upstream is tuple destructuring:

```python
for (r, c, n) in solution:      # solution holds (r,c,n) 3-tuples
    grid[r][c] = n
c = min([(len(X[c]), c) for c in X])[1]   # 2-tuples (int, X-key)
X1 = [("rc", rc) for rc in product(...)]  # 2-tuples (str, tuple)
```

Several distinct tuple shapes reach one variable, so unpacking hands every
position the union of all of them. The five separate `tuple#…` contours in the
union show arity separation (ifa/132) is working; what fails is that a
*variable* ends up holding all five plus two scalars.

**So the hidden bug is upstream of the container partition entirely**, in how a
destructured tuple's positions are typed — not in `split_css`'s granularity.
The finest setter partition scatters the grid rows across contours so the
pollution lands on one of them and the rest stay clean, which is why the default
compiles.

#### What is established, and what is next

- The retreat is withdrawn; non-minimality is **not** justified.
- `SETTERMIN`'s partition is correct where it was blamed: merging nine identical
  `list[int64]` literals is the minimal demanded answer.
- The defect it exposes is real and independent: a list `__setitem__` whose
  **index** formal admits `str` and `tuple`.
- Next step is to root cause that — which tuple contour feeds `c` and `n` at
  `sudoku5.py:33`, and why `min([...])[1]` and the `(r,c,n)` unpack reach the
  same variable. **Stop condition:** if the positions turn out to be genuinely
  separate contours and the union is formed at a confluence with a demand that
  no test nominates, this rejoins the main thread of this issue rather than
  being a tuple bug.

### Where this leaves it

The directive is right and the mechanism is present; what is missing is a
qualification of `setter_starters` that is neither "everything" nor
"only the irrepresentable ones". Both extremes are now measured. The middle —
what makes a setter partition of THIS container safe when its siblings are not
partitioned — is the open question, and it is a sharper one than "how do we
make the CS split".

`PYC_SETTERGATE=2` is kept at default 0, as the only way to exercise the CS
criterion at all; its cost is the table above.

## How shedskin handles it

**Author:** *"How does shedskin handle it?"* CLAUDE.md treats shedskin as the
reference for MECHANISM, so this is read from
`/home/jplevyak/projects/shedskin/shedskin/infer.py`, not recalled.

### The data structure is the same one

`ifa_flow_graph` builds assign sets, backflow paths, creation points, csites
and emptycsites — and pyc's `CSFlowGraph` is a faithful port of it, field for
field. In particular shedskin keys assign sets BY TYPE:

```python
assignsets.setdefault(merge_simple_types(gx, types), []).append(target)
```

and so does pyc (`Vec<AType *> keys`). **That is why the correction above was
needed**: the signature bit already carries type information.

### The ladder has four rungs. pyc's has two.

`ifa_split_vars`, in order:

| | shedskin | pyc |
| --- | --- | --- |
| 1 | `ifa_split_no_confusion` — >1 type AND >1 assign set | route 1 |
| 2 | split at a confluence point (`ifa_determine_split`), formal args / class attrs only, `2 <= remaining < 10` | route 4's signature partition |
| 3 | partition csites by `frozenset(union of types along the csite's paths)` | **absent** |
| 4 | **"if all else fails, perform wholesale splitting"** | **absent — DECLINES** |

Rung 4 verbatim:

```python
# --- if all else fails, perform wholesale splitting
elif len(paths) > 1 and 1 < len(csites) < 10:
    for csite in csites[1:]:
        ifa_split_class(cl, dcpa, [csite], split)
    return split
```

**One contour per creation site — the fan.** It is shedskin's answer to
exactly the case `cs=1849` is in: a graph exists, several creation points, and
no earlier rung names a partition.

### pyc deleted a fan, but not this one

[146](146-remove-all-arbitrary-splitting.md) C removed `if (defpart >= 2 && !g)
goto Lfan` — the fan that fired when there was **no flow graph at all**. That
removal was right, and the replacement (giving non-container CreationSets a
content channel) was a real fix: `bh` 24 → 7 mints, same precision.

shedskin's rung 4 is a different position: the graph EXISTS, `len(paths) > 1`,
and the earlier rungs came up empty. **pyc has no rung there.** It prints
`DECLINED (1 group: every creation point on the same assign sets)` and stops.
That is where `cs=1849` dies, every pass, on every program in this family.

### And shedskin's fan is bounded three ways mine was not

This reframes `PYC_CSFAN`'s negative result above. The idea was not the
problem; the engineering was.

| | shedskin | `PYC_CSFAN` as built |
| --- | --- | --- |
| creation-point bound | `1 < len(csites) < 10` | none |
| requires ≥2 assign sets | `len(paths) > 1` | not checked |
| how many per round | ONE variable's csites, then `return split` | every candidate, in one pass |
| between splits | **full re-propagation** — `iterative_dataflow_analysis` loops `propagate(gx)` → `ifa(gx)` | next pass, after every other stage also acted |
| global cap | `MAXITERS = 30`, `CPA_LIMIT = 10` | — |

`cs=1849` has **6** creation points: inside shedskin's bound, it would fan.
`cs=1010` has **26**: outside it, shedskin would refuse. **`PYC_CSFAN` fired on
`cs=1010` at p=1** — the contour shedskin would have declined — and never
reached the one it would have split, because route 4 only produces `cs=1849` at
p=2. The measured 39 → 61 warnings is that, not a verdict on the rung.

### What this does and does not say

It does **not** say "add the fan back". CLAUDE.md's directive is that splitting
is only ever on demand, and 146 calls the fan *"the arbitrary mechanism ifa/146
exists to retire"*. Reinstating it unguarded would be that mechanism again.

What it does say, and it is a fact about the reference implementation:
**shedskin's ladder terminates in a bounded, last-resort wholesale fan, and the
programs pyc cannot type are the ones that reach it.** The two positions can be
reconciled or one of them is wrong, and that is the author's call. The material
difference worth weighing is that shedskin's fan is not a blind fan: it is
bounded to under ten creation points, requires at least two type-keyed assign
sets, splits one variable per round, and re-propagates the entire network
before deciding anything else — so a split that buys nothing is observed and
not compounded.

Two smaller mechanisms are also absent and are cheaper to evaluate:

- **Rung 3** (`prt`): partition csites by the flattened union of types along
  their paths. Strictly coarser than pyc's per-assign-set bitstring, so on its
  own it cannot separate what the bitstring cannot — but it is what shedskin
  tries before resorting to rung 4, and it is nearly free to add.
- **`ifa_confluence_point`** is level-triggered on converged types — *does some
  creation point of this node appear in more than one assign set's creation
  points* — which is the shape [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md)
  opens by asking for, implemented.

