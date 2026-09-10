# 143 — a shared container-method EntrySet re-fuses every CreationSet the splitter separates

**Status: open.** Root-caused 2026-09-07 on `bh`.

**Corrected 2026-09-09:** this used to say `bh` is *the only*
`shedskin_examples` program that runs correctly at the default and fails
under `PYC_CSDCPA1=2`. It is not the only one -- `richards` does too, and
`sudoku5` stops compiling. Attributing all seven flag-arm divergences
across four arms ([129](129-plan-demand-driven-creation-set-splitting.md))
gives **three** blockers, not one and not seven:

| program | default | under the flag | element union |
| --- | --- | --- | --- |
| `bh` | correct, 0 pyc warnings | aborts 134, getter not resolved | `{Body, str}` from three `__slots__` lists |
| `richards` | correct but the `TIME` line | **SIGSEGV**, an int64 dereferenced as a task pointer | `{int64, None, Packet}` from `[0]*n` merged with `[None]*n` |
| `sudoku5` | compiles, 24 warnings | 364 compile errors | `{list, tuple, int64, str}` |

The other four -- `kanoodle`, `plcfrs`, `quameon`, `sudoku3` -- already
fail at the default, so the flag costs nothing there.

**All three blockers are one defect**: a merged `list` CreationSet whose
element channel unions types the program keeps apart, and which the
splitter can no longer separate because the merge destroyed the
attribution ([133](133-split-a-container-on-its-element-type.md), and
[142](142-linalg-empty-list-collapse-is-a-fixed-point.md) for the fixed
point). They differ only in the CREATION ROUTE into the shared CS --
string-literal lists here, sequence multiplication in `richards`,
comprehensions and `append` in `sudoku5` -- and in whether the resulting
union is caught at compile time or miscompiled. `tests/` now carries one
reduced repro per route.

## Symptom

```
bh: bh.py.c:7520: Assertion `!"runtime error: getter not resolved"' failed.
```

preceded by 11 FA warnings of the form

```
bh.py:565:53: warning: illegal call argument type 'q' illegal: str
```

`q` comes from `for q in reversed(self.bodies)`, so `self.bodies`'s
element channel carries `str` alongside `Body`/`Cell`.

## Where the `str` comes from

`bh.py` has three `__slots__` declarations, and they are the program's
only `str` lists:

```python
class Random: __slots__ = ["seed"]
class Vec3:   __slots__ = ["d0", "d1", "d2"]
class HG:     __slots__ = ["pskip", "pos0", "phi0", "acc0"]
```

Proved by deletion: rewriting the three `__slots__` lines to `pass` takes
the program from **11 warnings and `run_rc=134`** to **0 warnings and
`run_rc=0` with CPython-matching output**, under the flag, changing
nothing else.

Under `PYC_CSDCPA1=2` every `list` starts on ONE CreationSet, so these
merge with `self.bodies` and `Cell.subp`. **That merge is correct** — it is
what "one CreationSet per sym" means. The defect is downstream.

## RETRACTED ROOT CAUSE, and the real one (2026-09-08)

**The section below is wrong and is kept only as the record.** It said
`list.append` and `list.__setitem__` each have ONE EntrySet program-wide,
concluding that CreationSet splitting could never help. That was
generalised from a single CreationSet's two backward edges (`nback=2` on
`cs=1180`) and is false: a full `IFA_DBG_ELEMSETTER` dump of the last pass
shows **`append` on `es=62, 219, 220, 457, 462`** and **`__setitem__` on
`es=66, 226, 227, 228, 229, 326, 327, 427, 440, 441, 459, 464`**. The
methods are heavily contoured, per receiver, exactly as ordinary
argument-type CPA should do — which is also what the author pointed out
about static dispatch, and what `tests/two_list_element_separation.py`
pins.

### What `bh` actually contains

`bh.py` has **no `.append(` calls at all**, and no list comprehensions
either. The complete inventory of list construction in the file:

| site | how built | content | arity |
| --- | --- | --- | --- |
| `__slots__ = ["seed"]` | `primitive make list` | `str` | **1 (static)** |
| `__slots__ = ["d0","d1","d2"]` | `primitive make list` | `str` | **3 (static)** |
| `__slots__ = ["pskip",…]` (4) | `primitive make list` | `str` | **4 (static)** |
| `self.subp = [None] * Cell.NSUB` | `__mul__` (a C call) | `Cell`/`Body`/`None` | **none (runtime)** |
| `self.bodies = [None] * nbody` | `__mul__` (a C call) | `Body`/`None` | **none (runtime)** |
| `self.bodies = []` | `primitive make list` | — | 0 |

**Corrected 2026-09-08, on the author's objection** — *"pyc supports
heterogeneous lists converted to tuples, so how can list literals use
append? doesn't that conflict?"* It does conflict, and the earlier claim
here that "a list LITERAL is lowered to `sym_append`" was wrong.
`python_ifa_build_if1.cc:1238` is `build_list_comp_pyda`, the list
COMPREHENSION accumulator; a literal goes through the `PY_list` case at
`:4160`, which emits `primitive make list` with the elements as POSITIONAL
arguments. That positional path is exactly what makes a heterogeneous
literal representable as a tuple-like record, so it could not have been
`append`.

So the `append` contours seen in `bh`'s element writers come from `__pyc__`
library code — e.g. `__pyc_tolist__`'s `r = []; for x in self: r.append(x)`
(`__pyc__/04_sequence.py:97`) — not from `bh` source. **Which library path
carries `str` into a node list's element channel is NOT yet established**,
and is the next thing to measure.

What the correction does NOT change is the shape of the union: a
static-arity string literal sharing a CreationSet with a runtime-length
node list, which the partial separation in the dumps fits (`cs=1549` a
clean `[str]`, `cs=1550` a clean `[Body]`, `cs=1606`/`1709`/`1710` still
`[str Body]`).

If anything it STRENGTHENS the arity suspicion below, because the two
families do not even use the same content channel. ifa/104's two channels:
a `make list` literal fills POSITIONAL `cs->vars`, while `__mul__` /
`__setitem__` fill the generic ELEMENT. A fixed-arity positional record and
a runtime-length element-channel list are different shapes in the one way
the type system can actually see, and the guard below discards precisely
that distinction.

### The suspect: the arity guard exempts a varying-length candidate

`creation_point`'s dcpa1 route, under the default `PYC_ARITYSTRICT=1`:

```c
if (!x->no_static_arity && x->static_arity != arity) continue;
```

A candidate `x` that has ALREADY lost its static arity is exempt from the
check entirely, so an arity-1/3/4 literal may join the runtime-length
`[None] * n` contour. The comment says why, and says it in the revealing
word:

> a candidate that has ALREADY lost its static arity is on list layout and
> reads its length at run time, so merging any arity into it **is
> representable**.

Representable is not compatible. Under the three-way rule in CLAUDE.md
representability is a NECESSARY condition for sharing a contour, never a
sufficient one — compatibility is decided by demand, and nothing here
demanded that a three-element string literal share a contour with a
tree-node array. Arity is the one type-shaped fact that separates these two
families (ifa/132's title is exactly "arity is representation, not
provenance"), and the guard discards it in the one direction that matters.

**REFUTED 2026-09-08.** The tightening

```c
if (x->no_static_arity ? (arity >= 0) : (x->static_arity != arity)) continue;
```

was implemented behind `PYC_ARITYSTRICT=2`, measured, and reverted. On `bh`
it is **byte-identical** to the default: same 11 `illegal: str` warnings,
same `ess=366 css=1134 container_cs=16`. It was reverted rather than left
in as a dead lever.

It is inert because the premise was also wrong. `IFA_DBG_CSVARS=list` shows
the three `__slots__` literals **already hold their own contours, with the
right arities and EMPTY element channels**:

```
cs=1191 vars=1 arity=1  elem=          ["seed"]
cs=1195 vars=3 arity=3  elem=(none)    ["d0","d1","d2"]
cs=1200 vars=4 arity=4  elem=(none)    ["pskip", ...]
```

So the union was never "a string literal sharing a CreationSet with a node
list". `creation_point` separates them correctly. Clean separated contours
exist for the element channel too — `cs=1588 arity=0 elem= str` and
`cs=1546 arity=0 elem= Body`.

## Where the union actually enters (2026-09-08)

`IFA_DBG_FUNES=append` is decisive. `append` has **four** contours,
correctly split by (receiver, value) exactly as argument-type CPA should:

```
es=62  args= [append] [list#1596] [str#8 Body#1306]    <- value ALREADY a union
es=211 args= [append] [list#1588] [str#8]              <- clean
es=212 args= [append] [list#1597] [tuple#1359]         <- clean
es=367 args= [append] [list#1630] [str#8 Body#1306]    <- value ALREADY a union
```

**The union pre-exists the call.** Two contours receive a value argument
that is already `{str, Body}`, so no argument-type split can separate them
— there is only one argument type to split on. This is ifa/142's fixed
point again, reached at the CALL rather than at the element.

So the defect is **upstream of the container entirely**, and every
container-side mechanism examined in this issue — method contours, arity,
the element channel — is downstream of it. That is why splitting the
CreationSet 8 ways left all 8 polluted: the contours were fine; the value
flowing in was already mixed.

The `r = []` / `r.append(self[k])` shape those contours belong to is
`list.__pyc_copy__` (`__pyc__/04_sequence.py:360`), a shared library
helper whose `self[k]` is the receiver's element. `bh` reaches it through
`from copy import copy` — `copy(p)`, `copy(b.new_acc)`, `copy(dvel)`.

## ROOT CAUSE, traced end to end (2026-09-08)

**1. The union forms at pass 0, and that is CORRECT.** `PYC_LOG=s`, line 44
of the splitting log — the first `str`-with-class confluence in the run:

```
[confluence] av 3797 x [ES/formal] str __pyc_None_type__ Body tuple Cell
```

`av 3797` is `append(self, x)`'s value formal. Under `PYC_CSDCPA1` every
list starts on one CreationSet, so every `append` targets it and the formal
sees every appended type. Start-merged working as designed.

**2. Splitting then works, partially.** `IFA_DBG_FUNES=append` ends with
FOUR contours, correctly keyed on (receiver, value):

```
es=62  [list#1596] [str#8 Body#1306]    <- union
es=211 [list#1588] [str#8]              <- clean
es=212 [list#1597] [tuple#1359]         <- clean
es=367 [list#1630] [str#8 Body#1306]    <- union
```

**3. The two survivors have a SINGLE receiver and a union value.** No
EntrySet split repairs that shape: two call sites write different types
into the SAME CreationSet, so splitting the method by value type gives two
contours sharing one receiver and the element channel still takes both.
**The receiver has to split.**

**4. But those receivers have ONE creation point.** `IFA_DBG_CSDEFSPLIT`:

```
p=3 cs=1596 sym=list DEMAND-ADDED defs=1
p=3 cs=1596 sym=list defs=1 DECLINED (single creation point)
```

**104 such declines.** ifa/144's demand-driven candidate path correctly
OFFERS them; route 4 partitions by creation point and there is only one.

**5. The mechanism for exactly this exists and is OFF BY DEFAULT.**
ifa/129's third clause — *"a CreationSet with ONE creation point and an
irrepresentable element... the separation has to come from splitting the
EntrySet that OWNS the creation point, so the site duplicates and gives the
next pass two defs"* — is gated on `cscallsite_enabled()`, and
`PYC_CSCALLSITE` **defaults to 0**.

**6. Turning it on does not fix `bh` either, and the reason is terminal.**
`=1` and `=2` both leave the 11 warnings and the abort (`ess` 366 →
384/379, `css` 1134 → 1149, so it does fire). `IFA_DBG_THIRD`: 253
attempts, 251 refusals.

| owning EntrySet | attempts | outcome |
| --- | --- | --- |
| `__mul__` | 102 | `splittable=0 why=no_groups` |
| `reversed` | 78 | `splittable=0 why=no_groups` |
| `__new__` | 71 | `splittable=0 why=no_groups` |
| `__mul__` | 2 | `splittable=3` (succeeded) |

`no_groups` means `decide_entry_set_split` could not partition the callers
at all: every in-edge is mutually compatible under
`edge_type_compatible_with_edge`, because they all pass the SAME union
type. **ifa/142's fixed point, one level up** — at the call rather than at
the element.

### The statement

The creation point that needs separating lives inside a library function
(`list.__mul__`, `reversed`, `__new__`) whose callers are
**indistinguishable by type**, because the value they pass has already
converged to the union. So the CreationSet cannot split (one creation
point), its owning EntrySet cannot split (one type-group of callers), and
every finer rung has already declined by construction.

No partition key reaches this. What is missing is a way to tell those
callers apart that is **not their argument type** — the call site, which is
what the third clause reaches for and what `PYC_CSCALLSITE=2`'s
demand-driven mode is meant to supply. Its `no_groups` refusal is the
precise thing to fix: the demand names the parts (`{str}` vs `{Body}`), but
`decide_entry_set_split` is asked to find them by type compatibility, which
cannot see them.

**This supersedes everything above in this issue.** The shared-method-
contour story, the arity story and the `__pyc_copy__` attribution were all
wrong — `list.__pyc_copy__` is not even reached in `bh` (`IFA_DBG_FUNES`
shows `__pyc_copy__` only on `Vec3` receivers).

## SUPERSEDED — Root cause: the element channel has ONE writer per method, program-wide

`IFA_DBG_ELEMTYPE=1 IFA_DBG_ELEMSETTER=1` on the merged CreationSet:

```
ELEM cs=1180 sym=list elem_av=3809 ntypes=4 nback=2
   <- av=3812 in_fun=append      es=62  container=3811
   <- av=3903 in_fun=__setitem__ es=66  container=3892
```

**Two backward edges for eight creation points.** `list.append` has a
single EntrySet (es=62) serving every `append` in the program, and
`list.__setitem__` likewise (es=66). Their value formals hold the union of
everything anyone ever appended, and they write that union into the
element channel of EVERY receiver CreationSet.

So CreationSet splitting cannot fix this program **at all**. Forcing route
4 to partition `cs=1180` fully — measured, `PYC_CSDEFSPLIT=2` yields
`cs=1546 … cs=1552`, one contour per creation point at pass 0 — leaves
**every one of the eight still irrepresentable**, because each still has
`append es=62` as its element writer. Separating the containers is
pointless while one method contour writes the union back into all of them.

This was predicted in the tree. `python_ifa_build_syms.cc:2850`, on the
`clone_methods_per_cs` track:

> the per-constant instance CSs this creates are only useful if the
> class's methods split per CS too — **otherwise shared method contours
> write through the union and widen every sibling's fields**

`list` is not on that track. `__list_iter__` and the set iterators are
(via `__pyc_clone_constants__` on a ctor param, `__pyc__/04_sequence.py`,
`__pyc__/08_set.py`); `list` itself never was, because at the default each
allocation site already had its own CreationSet and the shared method
contour did not matter.

## What this issue is NOT

- **Not the cap.** Fixed independently ([133](133-split-a-container-on-its-element-type.md)).
- **Not route 4's candidate gate.** Also fixed (below), and `bh` still fails.
- **Not boxing.** `{Body, str}` is a merge pyc invented; shedskin compiles
  `bh` (see CLAUDE.md, "Boxing is never the answer for a corpus program").
- **Not `__slots__` semantics.** pyc need not implement `__slots__` for
  this; the lists just have to stop sharing an element writer with the
  node lists.

## Two fixes that landed on the way (both demand-driven, both kept)

**1. Route 4's candidate set was gated on TYPE_CONFLUENCE.**
`split_css_by_defs` read `tc_cs_dropped` and nothing else, and that Vec is
populated in exactly one place — the `else` branch of stage 1's confluence
handler. [142](142-linalg-empty-list-collapse-is-a-fixed-point.md) proved
that is backwards for the case route 4 exists to answer: once the union
forms, every writer carries it, `etype == stype` on every edge, and
TYPE_CONFLUENCE has nothing to see. The coarsest rung was reachable only
through a finer rung's detection. Measured on `bh`: `cs=1180` was a
candidate in 4 of 30 passes, in each of which a finer ladder rung had
already fired that pass.

Fixed by `cs_elem_irrepresentable` — the demand observed ON the
CreationSet. `cs=1180` is now offered at pass 0. `PYC_CSDEMAND=0` restores
the old candidate set for attribution.

**2. The third clause's guard was blind to its own case.** ifa/129's
third clause (`defs == 1`, split the owning ES) tested `mixed_basics`,
which counts only BASIC types — so `{Body, str}` has one basic and reads
as UNIFORM. The clause never fired on the union it exists to separate.
Replaced with `elem_irrepresentable`, which flags a `{class-or-container,
scalar}` mix as well as `{int64, str}`, while deliberately NOT flagging
`nil_type` unions (`{None, Cell}` is representable, and `bh`'s `subp`
holds exactly that) or pure-numeric mixes (`coerce_annotate` resolves
those).

With both, the third clause now fires on `bh` and reports
`es=128 fun=reversed edges=1 splittable=0 why=no_groups` — a single-caller
EntrySet, so there is nothing to partition. Correct refusal: `reversed`
merely inherits the pollution.

## How shedskin handles it (measured 2026-09-07)

shedskin compiles this program, from a `bh.py` carrying **the same three
`__slots__` lines**, and it has **no `__slots__` handling anywhere in its
source** (`grep -rn __slots__ shedskin/*.py` matches only its own
`infer.py:181`). It simply infers the right types. Generated C++:

```cpp
static list<str *> *__slots__;   // x3, one per class
list<Node *> *subp;
list<Body *> *bodies;
```

Three list instantiations, three element types, cleanly separated. Against
pyc on the same program:

| | list contours | element types |
| --- | --- | --- |
| shedskin | **3** | 3 (`str*`, `Node*`, `Body*`) |
| pyc, default | 21 | 6 |
| pyc, flag arm | 16 | 7 (one is the bad `{Body, str}`) |

**pyc makes five to seven times as many list contours as shedskin and
still gets the wrong answer.** That is the finding: the contours are not
merely excessive, they are excessive in the wrong places while merging in
the one place that decides the outcome.

The excess half is root-caused separately in
[144](144-route-4-fans-per-creation-point-instead-of-partitioning.md):
route 4 answers a demand to separate by giving EVERY creation point its own
contour, and nothing re-joins the ones that converge. On `bh` that leaves
18 of 20 `Vec3` contours byte-identical across all 29 members, differing
only in which creation point made them.

### Why shedskin cannot hit this bug

Not because it starts unmerged — it does not. All shedskin lists begin at
`dcpa=0`, exactly like `PYC_CSDCPA1`, and its ifa ladder separates them
(that ladder is already ported here: ifa/133 routes 1 and 3).

The difference is one line of its call machinery. shedskin indexes every
function contour **first by the receiver's data contour**:

```python
func.cp[dcpa][c] = cpa = len(func.cp[dcpa])       # infer.py:1401
if dcpa not in func.cp or c not in func.cp[dcpa]: # infer.py:1314
    create_template(gx, func, dcpa, c, worklist)
```

`(func, dcpa, objtype)` is unpacked from the call's function type
(`infer.py:1279`), so `list.append` reached through data contour 1 and
through data contour 2 are **different templates with different `value`
formals**. A shared `append` that unions element types cannot exist, by
construction. When the ladder splits a data contour, that contour's
methods come with it automatically — which is why shedskin's finer rungs
are sufficient there and insufficient here.

Two properties worth keeping when copying this:

- **It is identity, not a fan.** `(func, dcpa)` is what that method contour
  IS, not a split triggered by the receiver splitting. Under the rule
  recorded in CLAUDE.md — assignment by types, identity as ES x call site,
  compatibility by demand — a container method's identity legitimately
  includes which data contour it operates on. "Identity may be as fine as
  it likes; that is not a split."
- **It is reachability-driven.** `create_template` fires only when a call
  with that `(dcpa, c)` actually occurs, so contours follow real calls
  rather than being fanned out per receiver speculatively. That is the
  distinction between this and the deleted `PYC_RECVFAN`.

Codegen then gets the separation for free: `list<str*>::append` and
`list<Node*>::append` are distinct C++ template instantiations, so no
representation question arises either.

## CORRECTION 2026-09-07: keying on the receiver contour is NOT required here

The author's objection, and it is right: *"keying on the receiver data
contour is a unique solution if there is a dynamic dispatch. if the
dispatch is static as it is here then it isn't required afaict."*

Measured, and it holds. `list.append` **already gets one contour per
argument-type combination by ordinary CPA**, receiver included — the
receiver is just an argument, and an `AVar`'s type IS a set of
CreationSets, so `edge_type_compatible_with_entry_set` already compares
them. A five-line program shows both halves:

```python
a = []; a.append(1)
b = []; b.append("x")
print(a[0], b[0])
```

```
default:  FUNES fun=append contours=2
            es=44 args=[append][list#983][int64]
            es=50 args=[append][list#985][str]     <- DIFFERENT receiver CSs
flag:     FUNES fun=append contours=2
            es=44 args=[append][list#983][int64]
            es=50 args=[append][list#983][str]     <- SAME receiver CS
```

`append` splits either way. In the default arm the two contours already
carry different receiver CreationSets — **without any `dcpa` dimension**,
purely because the `self` argument's type differs. So shedskin's
`func.cp[dcpa][c]` indexing is how shedskin spells it, not a mechanism pyc
is missing: pyc's `c` already subsumes shedskin's `dcpa` for a statically
dispatched call, because the receiver is in the cartesian product.

What differs in the flag arm is only that **the CreationSet did not
split** — both contours share `list#983`, so both write into one element
channel. The fix is therefore to make the CreationSet split; the method
contours follow on their own.

**So the "what the fix has to be" section below is wrong as written** and
is kept for the record rather than as a plan. Splitting container method
contours per receiver CS is not the missing mechanism. Where the receiver
contour would still be the only handle is DYNAMIC dispatch — several
classes reachable at one call site — which is not this case.

### What that leaves for `bh`

`bh` is not explained by the corrected story yet, and the earlier
measurement stands unexplained: forcing route 4 to give all eight of
`bh`'s creation points their own contour left every one of them still
carrying `{Body, str}`. If splitting the CreationSet were sufficient, that
should have cleaned them.

The likely reason is the fixed point one level up rather than any
contour rule: `q` comes from `reversed(self.bodies)`, whose element is
already `{Body, str}`, so every `append`/`__setitem__` call SITE passes the
union as its value argument. CPA cannot separate contours whose argument
types are already identical unions — the same `etype == stype` fixed point
ifa/142 describes, reached at the call rather than at the element. That
makes `bh` a question about breaking the cycle upstream, not about method
contour identity, and it is not yet root-caused.

## What the fix has to be (SUPERSEDED -- see the correction above)

`list`'s methods must be able to split per receiver CreationSet, **on
demand** — the demand being an irrepresentable element union on a receiver
whose writer is a shared method contour.

**Not by fanning.** `PYC_RECVFAN` did exactly this fan and was DELETED
2026-09-07 as arbitrary splitting (it fanned per receiver CS with no
demand test, partition size = receiver count). Its numbers are the warning:
on `bh`, `=2` left 1 warning, `=3` left **5** — more splitting, worse
result — and `=2` with a forced route 4 cleared the union but produced new
`unresolved call '__init__'` and `matching function not found` failures.
Splitting past what demand justifies multiplies contours until dispatch
cannot resolve them.

The shape wanted is the existing PER_CS_RECEIVER stage with a demand
predicate: act on a receiver position when some receiver CreationSet's
element channel is irrepresentable AND this EntrySet is one of its
writers. That is an ES split serving a CS separation, which is the
sanctioned direction (CLAUDE.md: "an EntrySet is split **so that** a
CreationSet split becomes possible").

Note the stage is also STARVED on `plcfrs`/`rdb`/`sudoku5`: TYPE_CONFLUENCE
fires every pass, so its `!analyze_again` gate never opens. That is a real
problem and its answer is a reason the stage may act, not permission to
act without one.

## Reproducer

```sh
cd shedskin_examples/bh
PYC_CSDCPA1=2 PYC_CSLADDER=3 ../../pyc -D ../.. bh.py 2>&1 | grep -c "illegal: str"   # 11
PYC_CSDCPA1=2 PYC_CSLADDER=3 IFA_DBG_ELEMTYPE=1 IFA_DBG_ELEMSETTER=1 \
  ../../pyc -D ../.. bh.py 2>&1 | grep -A3 "^ELEM cs=1180"                            # nback=2
```

`report_element_setters()` is reached only via `report_element_types()`,
so **both** `IFA_DBG_ELEMTYPE` and `IFA_DBG_ELEMSETTER` are required —
setting only the latter prints nothing, which reads as "no writers".

## Why `bh` does not split correctly -- traced to the partition, not the gate (2026-09-09)

Step 5 above says the mechanism for the single-def case exists and is off
by default (`PYC_CSCALLSITE`). Measured: **turning it on does not fix
`bh`**, and the reason closes step 5.

```
PYC_CSCALLSITE=0  compile=0 warns=11  ess=372 css=1136
PYC_CSCALLSITE=1  compile=0 warns=11  ess=375 css=1140
PYC_CSCALLSITE=2  compile=0 warns=11  ess=375 css=1140
```

**It cannot work here.** `IFA_DBG_THIRD` probes every single-def candidate
and reports the owning EntrySet's in-edges:

| | count |
| --- | --- |
| single-def candidates probed | 216 |
| **owning ES has exactly ONE in-edge** | **215** |
| `splittable=0 why=no_groups` | 207 |

One caller means one call site, and one call site cannot be partitioned.
So "split the EntrySet that owns the creation point, so the site duplicates
and gives the next pass two defs" is structurally inapplicable to 215 of
216 of `bh`'s demanded CreationSets, whatever the flag says.

### Where it actually stops: the partition is made, along the wrong line

The demand is detected -- 65 `list` CreationSets DEMAND-ADDED with
irrepresentable elements. And route 4 DOES partition the merged root:

```
p=0   cs=1180 def av=3013  -> cs=1546 (group 1/2 sig=1)
p=0   cs=1180 def av=4123  -> cs=1546 (group 1/2 sig=1)
p=25  cs=1180 def av=16622 -> cs=1631 (group 1/2 sig=1)
```

but the products still carry the union:

```
cs=1180 defs=8 | in=___init___ | in=__init__ | in=enumerate | in=_get_argv
               | in=reversed   elem= str#8 Body#1306 tuple#1359 Cell#1442
cs=1596 defs=1 | in=reversed   elem= str#8 Body#1306
cs=1631 defs=2 | in=reversed   elem= str#8 Body#1306
```

**So the split happens and does not separate the demanded distinction.**
Route 4 groups creation points by their assign-set signature; on `bh` that
signature yields 2 groups and neither corresponds to "str elements" vs
"Body elements". After the split each product has `defs=1` -- which is
where the 62 of 65 `DECLINED (single creation point)` come from -- and
there is nothing left to partition.

That is [133](133-split-a-container-on-its-element-type.md)'s "merging
destroys the attribution" in its sharpest form: by the time the demand is
observable on the CreationSet, every creation point carries the whole
union, so no signature computed from the current graph can name which
creation point contributed the `str`.

### The specific shape

The lists that carry `{str, Body}` are built inside **`reversed`** (a
`__pyc__` builtin), and its callers pass the SAME merged list CreationSet,
so `reversed`'s formal has ONE type and stage 1 sees no confluence on it.
The distinction is one level down -- in the ELEMENT of the argument, not in
the argument's type -- and no stage keys on that.

So `bh` needs a split keyed on a formal's ELEMENT type, not its type. That
is a different key from anything in the file today, and it is the concrete
thing this issue has been circling.

## The element demand now DOES reach setter splitting -- and bh still fails

Author: *"so the demand to split the elements must be turned into a demand
to split the cs, right? we discussed this before?"* Yes, and the missing
half was specific: there are TWO CreationSet-splitting mechanisms and the
element demand only ever reached one of them.

| mechanism | partitions by | fed from |
| --- | --- | --- |
| route 4, `split_css_by_defs` | assign-set signature (the CS flow graph) | the element demand (`cs_elem_irrepresentable`) -- ifa/144 |
| setter splitting, `split_css` | `same_eq_classes(v->setters, ...)` -- WHO WRITES | setter confluences only |

A `str` writer and a `Body` writer are different setters, so setter
splitting is the partition that can express `bh`'s distinction, and route 4's
is not. Measured before the change: of the 101 `list` CreationSets
`split_css` sees on `bh`, **98 have `starter_set=1 defs=1`**, and **cs=1180
-- the merged root, 8 defs, element `{str, Body, tuple, Cell}` -- appears
ZERO times**. The demand never arrived.

**Wired** (`PYC_ELEMSETTER`, default 0): `collect_cs_setter_confluences`
now offers a CreationSet's element unconditionally when
`cs_elem_irrepresentable(cs)`, rather than only when its setters already
disagree with a consumer's -- a test that presumes the separation it is
trying to create.

It works, and it is safe:

| | pyc suite | `bh` (flag) | `sudoku5` (flag) |
| --- | --- | --- | --- |
| `PYC_ELEMSETTER=0` | 313 / 0 | w=11, ess=372 css=1136 | 364 errors, ess=1104 |
| `PYC_ELEMSETTER=1` | **313 / 0** | w=11, ess=**369** css=**1133** | 364 errors, ess=1104 |

The demand reaches the rung and produces a split. **It does not fix `bh`.**

### Why -- the writers are themselves merged

Step 2 above has the answer, and this measurement confirms it end to end.
`append`'s surviving contours are

```
es=62  [list#1596] [str#8 Body#1306]   <- ONE receiver, union VALUE
es=367 [list#1630] [str#8 Body#1306]
```

so the element of `cs=1596` has **one setter carrying both types**, not two
setters carrying one each. Setter equivalence cannot partition what a
single setter merged. To get two setters you must split `append` by value
type, which step 3 already records as giving "two contours sharing one
receiver, and the element channel still takes both".

So `bh` is circular in its own terms: the receiver CreationSet cannot split
because its writers are merged, and its writers are merged because the
receiver CreationSet is one. Route 4 cannot break it (wrong partition key),
setter splitting cannot break it (one setter), and the single-def fallback
cannot break it (215 of 216 owning EntrySets have one in-edge).

**What would**: a split keyed on a formal's ELEMENT type -- `append(self,
x)` separated by the element type of `self`, not by the type of `self` --
which is the one key nothing in the file has. That is the same conclusion
the previous section reached from the `reversed` side, now reached from the
setter side as well.

`PYC_ELEMSETTER` kept at default 0. It is correct and suite-neutral, but it
buys nothing measurable yet, so it is apparatus rather than a landed
improvement.

## THE CONVERGENCE, located -- and the member IS the partition (2026-09-09)

Author: *"look, there is a convergence. that is the key. focus on that.
where is it. how can we move it to the instance variable or member?"*

**Where it is.** Only NINE distinct sites in the whole of `bh` mix `str`
with a class (`[confluence]` lines, `PYC_LOG=s`):

```
av 3797 x     [ES/formal]  str None Body tuple Cell    <- append(self, x)'s VALUE
av 3894 value [ES/formal]  str None Body tuple Cell    <- __setitem__'s value
av 4515 (anon)[CS/other]   str None Body tuple Cell    <- a MEMBER
av 7362 root  [CS/other]   str None Body tuple Cell    <- a MEMBER (Tree.root)
av 7641 x     [ES/formal]  str Body
```

The first is the cause; the rest are downstream of reading a polluted
element. And two of the nine are ALREADY on instance variables --
`[CS/other]` means the AVar's contour is a CreationSet, i.e. it is a
member -- which stage 1 detects and then DROPS (`tc_skip_cs`, "deferred to
CS_DEF_PARTITION").

**Moving it to the member works, and here is the measurement.** For the
merged root `cs=1180` (8 creation points, element
`{str, Body, tuple, Cell}`), walking each creation point's FORWARD closure
and recording which MEMBERS it reaches:

```
av=2847  __set_iter__._items
av=4123  Tree.bodies  closure.seq  closure.x  __list_iter__.thelist
av=1434  __dict_items_iter__._keys
av=1437  __dict_items_iter__._vals
av=1399  __dict_iter__._keys
av=7676  closure.r closure.tmp __list_iter__.thelist closure.x
av=7602  closure.r closure.tmp __list_iter__.thelist closure.x
av=3013  closure.result closure.tmp closure.args closure.x
```

**The creation points reach DIFFERENT members**, and `Tree.bodies` -- the
`Body` list -- is reached by exactly ONE of them. Partitioning `cs->defs`
by the SET OF MEMBERS each reaches yields about **7 groups from 8 defs**,
against route 4's **2**. The distinction the demand needs is right there,
and no rung looks at it.

**Why this is the right key, not provenance.** A member is part of a
class's declared structure (`Sym::has`) -- "which field holds this
container" is a structural fact about the program's TYPES, in the same
family as arity ([132](132-arity-is-representation-not-provenance.md)). It
is not "where did this value come from". And unlike a violation or a stuck
dispatch it is observable BEFORE the union forms, so it does not have the
circularity that defeated the violation-gated experiments in
[133](133-split-a-container-on-its-element-type.md).

**Why the existing rungs miss it.** Route 4's `build_cs_flow_graph` keys on
the CONTENT channel (`cs_content_avars` -- the element, or the positional
vars), i.e. on what is IN the container. The members are what the container
is IN. Those are different graphs, and only the second separates `bh`'s
lists: every one of these creation points has the same polluted content by
the time the demand is visible, which is why the content key collapses them
to 2 groups.

**So the concrete next step is a new partition key for route 4**: group a
CreationSet's creation points by the set of member AVars their forward
closure reaches. Everything needed is already in the graph -- this section's
numbers were produced by a 12-line probe (`IFA_DBG_MEMBER`) over
`av->forward`.

## The MEMBER key, implemented -- `sudoku5` FIXED on the flag arm (2026-09-09)

Built the key the previous section called for: group a CreationSet's
creation points by the set of MEMBERS their forward closure reaches, used
in `split_css_by_defs` whenever it names a FINER partition than the content
key. `PYC_CSMEMBER`, default 0.

**Two failed iterations first, and the second is the whole point.**

1. *Fallback only when the content key fails.* Never fired. Route 4's
   dominant decline is `defs.n < 2`, which short-circuits before any
   signature, and on `cs=1180` the content key does not fail -- it returns
   2 groups, both still `{str, Body}`. Changed to "use the member key
   whenever it is strictly finer".
2. *Counting every member.* Fired, and produced a FAN: `defs=8 -> 8
   groups`, `defs=4 -> 4 groups`, `defs=31 -> 20 groups`. `sudoku5` got
   WORSE (364 -> 375 errors). The probe output said why -- the members
   being counted were `closure.r`, `closure.tmp`, `__list_iter__.thelist`:
   internal plumbing, not user structure. A closure's slots are per-contour
   by construction (`creation_point` mints closures unique per site x
   contour), so counting them makes the signature nearly unique per
   creation point. **That is a fan wearing a type key, and `defs=8 -> 8
   groups` is exactly the ifa/144 signature I have been flagging all
   session.**

**Excluding closure-contoured members (structural test on `sym_closure`,
never on a name) is what makes it work:**

| | `sudoku5` (flag arm) | `bh` (flag arm) | pyc suite | default arm |
| --- | --- | --- | --- | --- |
| `PYC_CSMEMBER=0` | **364 errors**, ess=1104 css=2634 | w=11, ess=372 | 313 / 0 | baseline |
| `PYC_CSMEMBER=1` | **0 errors**, ess=**629** css=**1476** | w=11, ess=372 | 313 / 0 | **contour-identical** |

`sudoku5` now compiles on the flag arm with **24 warnings -- the same count
as the default arm -- runs `rc=0`, and prints output matching CPython but
for the `TIME` benchmark line.** ess falls 43%, css 44%.

Default arm measured contour-identical on `chess`, `richards`, `sudoku1`,
`bh`, `nbody`, `dijkstra`; suite 313 passed / 0 failed on both backends.

**Why this is a type-side key and not provenance.** A member is part of a
class's declared structure (`Sym::has`); "which field holds this container"
is a fact about the program's types, the same family as arity
([132](132-arity-is-representation-not-provenance.md)). It is keyed on
`Sym::id`. And it is observable BEFORE the element union forms, which is
what every violation-gated experiment in
[133](133-split-a-container-on-its-element-type.md) could not manage. The
closure exclusion is what keeps it a partition rather than a fan: closure
slots are per-contour by construction, so they carry no information about
which USER structure a container belongs to.

**`bh` is unchanged** and remains the last blocker -- its lists reach only
ONE user member (`Tree.bodies`) out of eight creation points, so there is
no member partition to make. Its problem is the one the previous sections
reach from both sides: a formal's ELEMENT type, which nothing keys on.

**Not landed on** (`PYC_CSMEMBER` defaults to 0): a corpus `check` sweep on
both arms is owed first, per this repo's rule for a splitter change.

## WHERE the confluence is in `bh` -- traced to a member (2026-09-10)

Followed it end to end with `IFA_DBG_AV` (dump an AVar and every backward
writer with the type it contributes) and `IFA_DBG_FUNES` (a function's
contours and their argument keys).

**1. The first confluence carrying `str` with a class** is line 44 of the
splitting log -- `append(self, x)`'s VALUE formal:

```
[av] 3797 var=x in=append  type= str#8 Body#1306 tuple#1359 Cell#1442
    <- av=3799  var=x in=append  contributes: str#8
    <- av=7831  var=x in=append  contributes: tuple#1359
    <- av=7968  var=x in=append  contributes: str#8 Body#1306 tuple#1359 Cell#1442
```

**2. The receiver split is already exhausted.** `append` has FIVE contours,
correctly keyed on (receiver CreationSet, value type):

```
es=211 [list#1546] [str]          <- clean
es=212 [list#1597] [tuple]        <- clean
es=62  [list#1596] [str Body]     <- union, SINGLE receiver
es=373 [list#1634] [str Body]     <- union, SINGLE receiver
es=377 [list#1631] [str Body]     <- union, SINGLE receiver
```

Three contours each have ONE receiver CreationSet taking both `str` and
`Body`. No further receiver split exists to make -- this is step 3 of the
2026-09-08 trace, now with the contour table behind it.

**3. Those three receivers are all created in `reversed`** (measured
earlier: `def var=(anon) in=reversed`).

**4. `reversed` cannot be split, because its formal's TYPE is identical in
every contour:**

```
FUNES fun=reversed contours=3
  es=128 args= [reversed#416] [list#1588 list#1606]
  es=371 args= [reversed#416] [list#1588 list#1606]
  es=372 args= [reversed#416] [list#1588 list#1606]
```

Three contours, one argument key. Stage 1 keys on the formal's type and
every contour has the same two-CreationSet union, so there is nothing for
it to separate.

**5. And `bh` only ever calls `reversed` on Body lists:**

```
573:        for b in reversed(self.bodies):
582:        for q in reversed(self.bodies):
625:        for b in reversed(bodies):
```

**So the `str` is already inside the member before `reversed` sees it.**

### The confluence is `Tree.bodies`

The member holds a union of TWO list CreationSets, `list#1588` and
`list#1606`, and one of them carries `str`. Everything downstream is a
consequence: `reversed`'s formal inherits the union, its result list takes
elements from both, and `append` into that result then has a single
receiver and a `{str, Body}` value.

That also explains why every rung tried so far fails, in one sentence
each. Route 4 partitions creation points -- the receivers already have one
each. Setter splitting partitions writers -- there is one writer carrying
both. The single-def fallback splits the owning ES -- it has one in-edge.
The MEMBER key partitions by which member a creation point reaches --
`Tree.bodies` is reached by exactly one of the eight. **Every one of them
is asking about the container. The distinction is in the member's TYPE: one
field holding two list CreationSets.**

### What that makes the fix

Splitting a MEMBER by the CreationSets its type spans -- `Tree.bodies` into
the `list#1588` part and the `list#1606` part -- is the shape nothing in
the file does. It is the same conclusion the `append` side and the
`reversed` side reached independently, stated on the object that actually
holds the union.

## THE PATH, end to end -- `bh` is `list_mul_scalar_object_separation` (2026-09-10)

Author: *"so the cs coming from the non union append is fed to reverse via
what path?"* Traced it with `IFA_DBG_CSELEM=<csid>` (dump a CreationSet's
element AVar and every backward writer with its contribution). The two
CreationSets `reversed`'s formal spans:

```
[cselem] cs=1588 defs=1 elem= Body#1306              <- CLEAN
    DEF av=4123 in=__init__
    <- av=13596 in=__setitem__  gives: Body#1306

[cselem] cs=1606 defs=1 elem= str#8 Body#1306        <- POLLUTED
    DEF av=4512 in=__mul__
    <- av=4516  in=__mul__      gives: str#8         <- the str enters HERE
    <- av=13596 in=__setitem__  gives: Body#1306
```

The `str` enters through **`list.__mul__`**, which is `P_prim_merge`, whose
`structural_assignment(..., merge=true)` pours the OPERAND's positional
slots into the RESULT's element:

```c
for (int i = cs->sym->has.n; i < cs->vars.n; i++) {
  flow_vars(cs->vars[i], tval);
  ... flow_vars(tval, get_element_avar(new_cs));   // operand slot -> result ELEMENT
}
```

And `bh` multiplies only `[None]`:

```
418:        self.subp   = [None] * Cell.NSUB
518:        self.bodies = [None] * nbody
```

**So the operand is `[None]` -- an ARITY-1 list literal. And so is
`Random.__slots__ = ["seed"]`.** CreationSet identity is (sym x arity)
(ifa/132), so those two literals share one CreationSet, its slot 0 unions
`{None, str}`, and `__mul__` pours that into `self.bodies`'s element.

### Proved by changing one arity

```
baseline                                        11 warnings
__slots__ = ["seed"]  ->  ["seed", "_pad"]       1 warning
```

Changing ONLY `Random.__slots__` from arity 1 to arity 2 -- so it no longer
shares the arity-1 literal contour -- takes `bh` from 11 pyc warnings to
zero (the remaining 1 is clang's `long`->`double` note). The other two
`__slots__` are arity 3 and arity 4 and were never involved. (Corpus file
restored; this was a probe, not an edit -- see
[PYC_CHANGES.md](../../shedskin_examples/PYC_CHANGES.md).)

### The full chain

1. `["seed"]` and `[None]` are both arity-1 `list` literals -> ONE
   CreationSet under `PYC_CSDCPA1`.
2. Its positional slot 0 unions `{str, None}`.
3. `[None] * nbody` -> `list.__mul__` -> `P_prim_merge` pours slot 0 into
   the result's ELEMENT -> `{str, None}`.
4. That result is `self.bodies` (cs=1606); `__setitem__` adds `Body` ->
   `{str, Body}`.
5. `reversed(self.bodies)` -- all three contours share the argument key
   `[list#1588 list#1606]`, so no split separates them.
6. `reversed`'s result list takes both, so `append` into it has a SINGLE
   receiver and a `{str, Body}` value -- the confluence at `av 3797`.
7. `str` reaches `rt` / `root`, the getter cannot resolve, the binary
   aborts.

### What this means

**`bh` is the same defect as `tests/list_mul_scalar_object_separation.py`**
-- the repro built for `richards`, `[0] * 4` against `[None] * 4`. Same
mechanism exactly, with `["seed"]` in place of `[0]`. The `nilstore` fix
happened to resolve `richards`'s instance (there the nil store had lost its
setter and could not become a `split_css` starter); `bh`'s instance has a
`str` on the other side, so that fix does not reach it.

So `bh` is NOT a separate problem from the flag arm's other blockers, and
it is not really about shared container-method contours at all -- those are
downstream. It is arity-1 list literals sharing a contour, which is
[133](133-split-a-container-on-its-element-type.md), and the test for it is
already in the tree.

### CORRECTION (2026-09-10): the richards repro no longer reproduces this

The section above ends "bh is the same defect as
`tests/list_mul_scalar_object_separation.py`". Same SHAPE, but not the same
instance, and the distinction matters because that test now **passes on
both arms**:

```
default                        diagnostics=0
PYC_CSDCPA1=2                  diagnostics=0
PYC_CSDCPA1=2 PYC_CSLADDER=3   diagnostics=0
PYC_CSDCPA1=2 PYC_NILSTORE=0   diagnostics=1     <- the fix that closed it
```

ifa/133's `nilstore` fix resolved it, so it is a regression guard for that
fix now, not a pin on `bh`. Its header said "fails under PYC_CSDCPA1=2" and
was stale; fixed.

`bh`'s instance survives because its far side is a `str` in an UNMULTIPLIED
literal (`__slots__ = ["seed"]`), not a nil store that had lost its setter.
**New minimal repro: `tests/arity1_literal_shares_contour.py`** -- 35 lines,
clean at the default, and under `PYC_CSDCPA1=2` emits `illegal call
argument type 'b' illegal: str`, which is `bh`'s exact signature.

## A faithful minimal repro for `bh` -- NOT YET (2026-09-10)

Author: *"so we need a minimal repo for bh"*. Agreed, and I do not have one.
What I have reproduces the signature and even the CreationSet shapes, but
not the blocking behaviour, and the gap is now located precisely enough to
be the next thing to chase.

**`bh`'s merge point, found.** `IFA_DBG_CSDUMP=-1` dumps every `list`
CreationSet with its positional slots and creation points:

```
[cs] id=1180 defs=4 vars=0 elem_var=1  | 4x ___init___
[cs] id=1191 defs=5 vars=1 var[0]=str#8
     | def in=__init__ | def in=___init___ | def in=__init__
     | def in=create_test_data | def in=__init__
```

`cs=1191` is the arity-1 literal contour holding BOTH
`__slots__ = ["seed"]` (in `___init___`, the class body) and four `[None]`
literals. Five creation points, so it is partitionable.

**A structurally identical repro does NOT reproduce the blockage.** Built
one with the same shape -- a `__slots__ = ["seed"]`, three classes each
doing `[None] * n`, and a module-level `[None] * n` -- and its contours come
out the same:

| | 4-def CS | 5-def literal CS |
| --- | --- | --- |
| repro | `defs=4`, 4x `___init___` | `defs=5 vars=1 var[0]=str#8` |
| `bh` | `defs=4`, 4x `___init___` | `defs=5 vars=1 var[0]=str#8` |

But with `PYC_VIOLCS=2 PYC_CSMEMBER=1` the repro goes to **0 diagnostics**
and `bh` stays at **10**. The trace says why:

```
repro:  cs=1036 def av=3220 -> cs=1081 (group 1/2)      <- SPLITS
        cs=1030 defs=5 MEMBER-KEY -> 5 groups           <- SPLITS
bh:     cs=1191  (no csdefsplit lines at all)           <- NEVER A CANDIDATE
```

**So the open question is sharp:** why does `bh`'s `cs=1191` never become a
route-4 candidate, when the structurally identical `cs=1036` in the repro
does? The backward demand walk (`PYC_VIOLCS=2`) reaches the repro's merge
and not `bh`'s, so something on `bh`'s path from the observed violation back
to the literal contour is not traversable by `av->backward` -- one hop more
than the repro has, or a primitive that copies content without leaving a
backward edge.

Ruled out along the way: it is not the number of creation points (both 5),
not the class-body `__slots__` shape (both have it), and not the nil-writer
gate in `collect_type_confluence` (`PYC_CONFNIL=1`, measured inert on `bh`).

`tests/arity1_literal_shares_contour.py` stays as the SIGNATURE guard, with
its header saying so. The faithful repro has to wait on the answer above.

## WHERE `bh` FAILS THE DEMAND -- stage 5 never runs, and the fix (2026-09-10)

Author: *"keep digging, find where bh fails the demand"*. Found it, and it
is upstream of everything the previous sections were probing.

**The demand is never consulted.** `IFA_DBG_VIOLWALK` produced no output on
`bh` at all -- because `collect_violation_imprecisions` is never called.
`IFA_DBG_STAGE5` says why:

```
[stage5] p=4  analyze_again=1 -> starved
[stage5] p=5  analyze_again=1 -> starved
   ... every pass ...
[stage5] p=39 analyze_again=1 -> starved
```

Stage 5 is starved on **all 40 passes**. The stages above it always claim
progress -- TYPE_CONFL 40 times, SETTER 18, SETTER_OF_SETTER 10,
MARK_SETTER 4 -- so under the first-stage-wins cascade the VIOLATION stage
never executes once. `bh`'s `illegal call argument type ... illegal: str`
is recorded and then never looked at.

**The obvious fix is the wrong one.** Lifting the quiescence gate
(`PYC_SIZEOF_VIOL=2`) does let stage 5 run, and with the demand-transfer
machinery it takes `bh` from 11 warnings to 1 -- but it costs **16 suite
tests** on its own. That is exactly the retreat CLAUDE.md names: answering
starvation with more splitting rather than with a demand test.

**The deferral does not need the STAGE.** All the demand has to do is name
a CreationSet, and route 4 (`split_css_by_defs`) runs on EVERY pass
regardless of quiescence. So walk each violation backward and offer every
CreationSet on the path directly to route 4 -- `PYC_VIOLCS=3`. No gate is
lifted and no stage ordering changes.

### Result: both remaining flag-arm blockers are fixed

With `PYC_VIOLCS=3 PYC_CSMEMBER=1`, on `PYC_CSDCPA1=2 PYC_CSLADDER=3`:

| | compile | pyc warnings | run | stdout |
| --- | --- | --- | --- | --- |
| `bh` | 0 | **0** | **rc=0** | **identical to CPython** |
| `sudoku5` | 0 | 24 (same as the default arm) | **rc=0** | identical but the `TIME` line |

and it is safe:

| | |
| --- | --- |
| pyc suite | **314 passed / 0 failed**, both backends, on and off |
| default arm contours | **identical** on `chess`, `richards`, `sudoku1`, `nbody`, `dijkstra` |
| six gates | green |

### Why all three pieces were needed, and why one was dropped

- **`PYC_VIOLCS=3`** -- the violation names the CreationSet where the union
  is OBSERVED; the one that MERGED is upstream, so the demand must be walked
  back. Alone: `bh` 11 -> 10 warnings.
- **`PYC_CSMEMBER=1`** -- the merged contour then declines with "flow graph
  covers none of the defs", because the content key asks what is IN the
  container; the member key asks what the container is IN. Alone: no effect
  on `bh`. Together: **0**.
- **`PYC_SIZEOF_VIOL=2`** -- needed only while the deferral lived inside
  stage 5. Once it is handed to route 4 directly it is unnecessary, which is
  fortunate, because it is the piece that broke 16 tests.

Both mechanisms remain at default 0. A corpus `check` sweep on both arms is
owed before flipping them, per this repo's rule for a splitter change.

## CORPUS SWEEPS for `PYC_VIOLCS=3 PYC_CSMEMBER=1` (2026-09-10)

Four arms from ONE binary (both mechanisms are env-gated), so the arms are
exactly comparable. `check` mode could not complete -- this shared host kept
killing the RUN phase for memory -- so the sweeps are `compile` mode, which
completes reliably in ~6 minutes, and the run side was taken by hand for
every program whose compile output changed.

### Compile, all four arms

| arm | compile_fail | with_warnings | container CS / shapes |
| --- | --- | --- | --- |
| default | 2 (`othello3`, `rdb`) | 43 | 2740 / 625 = 4.38 |
| default + mechanisms | 2 (same two) | 44 | 2816 / 625 = 4.51 |
| flag (`PYC_CSDCPA1=2 PYC_CSLADDER=3`) | **7** | 39 | 2091 / 626 = 3.34 |
| flag + mechanisms | **3** | 44 | 2505 / 624 = 4.01 |

**Flag-arm compile failures 7 -> 3**: `chull`, `plcfrs`, `quameon` and
`sudoku5` all start compiling. What remains is `othello3`, `rdb`,
`sudoku3` -- and the DEFAULT arm fails `othello3` and `rdb` too, so the flag
arm is **one program (`sudoku3`) away from compile parity with the
default**.

### Run side, every program whose flag-arm compile output changed

| program | flag BASE | flag + mechanisms | |
| --- | --- | --- | --- |
| `bh` | rc=134, output differs | **rc=0, output IDENTICAL to CPython** | **fixed** |
| `sudoku5` | compile-fail | **rc=0**, only the `TIME` line differs | **fixed** |
| `chull` | compile-fail | rc=139 | = its DEFAULT-arm behaviour |
| `plcfrs` | compile-fail | rc=134 | = its DEFAULT-arm behaviour |
| `quameon` | compile-fail | rc=134 | = its DEFAULT-arm behaviour |
| `linalg`, `mastermind2`, `sudoku4`, `sunfish`, `tarsalzp`, `tictactoe`, `webserver` | | unchanged | |

**No regressions.** Warning counts also fall sharply where they change:
`linalg` 112 -> 46, `plcfrs` 342 -> 162, `sudoku5` 249 -> 24, `sudoku4`
45 -> 21, `webserver` 14 -> 6, `tarsalzp` 231 -> 213. Three go the other
way and are small: `mastermind2` 42 -> 54, `sunfish` 51 -> 55, `tictactoe`
0 -> 6.

### Cost on the default arm

+76 container CreationSets (2740 -> 2816, +2.8%), and one program gains
warnings: `chull`, 0 -> 6. `chull` **segfaults at runtime on the default arm
either way** (`run=139` with and without), so those six warnings surface
diagnostics on a binary that was already failing silently -- ifa/102's case,
and an improvement in failure mode rather than a regression.

pyc suite is 314 passed / 0 failed on both backends with the mechanisms on
and off; six gates green.

### Where that leaves the flip

The flag arm now has **no known blocker**: `bh` and `sudoku5` are fixed,
and the three remaining compile failures are `othello3` and `rdb` (which
the default fails too) plus `sudoku3`. Container CS on the flag arm is 2505
against the default's 2740 -- still 8.6% fewer contours than the default,
with one more compile failure.

Both mechanisms remain at default 0. The recommendation is to flip them
WITH `PYC_CSDCPA1`, not before it: on the default arm alone they buy
nothing and cost 76 contours.

## `sudoku3` root-caused: bh's defect at arity 3 -- not yet fixed (2026-09-10)

`sudoku3` is the last flag-arm compile failure the default arm does not
share. Root cause found; a fix is not.

### The merge point

```
[cs] id=1022 sym=list defs=5 vars=3
     var[0]= int64#6  list#1023  tuple#2290
     var[1]= int64#6  list#1826  tuple#2291
     var[2]= int64#6  list#1827  tuple#2292
     | check_for_last_in_row_col_3x3 | __main__ | check_for_single_occurances
     | __main__ | __main__
```

An **arity-3 list literal contour with five creation points**, every slot
unioning `{int64, list, tuple}`. The source is one line:

```python
TRIPLETS = [[0,1,2],[3,4,5],[6,7,8]]
```

The OUTER list is `list` of arity 3. So are the three INNER lists. CS
identity is (sym x arity), so all four land on one contour, and slot 0 then
holds `int64` (from `[0,1,2]`) and `list` (from `TRIPLETS`) and `tuple`
(from a same-arity tuple literal elsewhere).

**This is exactly `bh`'s defect at a different arity** -- same-arity
literals sharing a contour -- and here it needs no `__slots__` or `__mul__`
at all: a nested literal whose outer and inner lists happen to agree on
length is enough.

### Why the mechanisms that fixed `bh` do not fix this

`cs=1022` declines **35 times** with *"flow graph covers none of the
defs"*. The MEMBER key was built for exactly that decline -- and it is
empty here: `TRIPLETS` is a MODULE-LEVEL GLOBAL, so its creation points
reach no instance variable at all and all five member signatures are the
same (empty) string. The key that saved `bh` depends on the container
reaching a member, and this one never does.

### The construction-time slot key -- tried, measured WORSE

The obvious next key, and the one ifa/133's closing question named: group
creation points by the TYPES THEY WERE BUILT WITH, read from the make
primitive's operands in each def's own contour. That is type-side,
legitimate under CLAUDE.md, and observable at construction rather than
after the union forms. Implemented as `PYC_CSMEMBER=2`.

It splits, and not as a fan -- `defs=5 -> 2 groups`, `defs=21 -> 6`,
`defs=17 -> 4`, `defs=2 -> 2`. But `sudoku3` gets **worse**: warnings
121 -> 147, errors 157 -> 165. And the tell is in the numbers: `cs=1022`
has THREE distinct slot shapes and the key finds only **two** groups --
because by the time route 4 runs, the construction OPERANDS have themselves
been polluted. The same lag, one level further in than where it was last
found.

Kept at `PYC_CSMEMBER=2` (opt-in; suite is 314/0 either way) as the record
of the attempt, and labelled measured-worse. `PYC_CSMEMBER=1` -- member key
only -- is the setting the `bh` and `sudoku5` results were measured with.

### What is actually needed

A key that separates same-arity literals **at the moment they are created**,
before any operand can be polluted. `creation_point` has the operand types
right there in `make_kind`, at the mint, and that is the one place in the
pipeline where `[0,1,2]` is unambiguously int-slotted. Doing it there is a
change to CS IDENTITY rather than to a splitting rung, which is a different
and larger kind of change than anything tried in this issue so far -- and
it is what ifa/132 did for arity.

## Where the demand was failing for `sudoku3` -- and the fix (2026-09-10)

Author: *"the solution must be demand not at mint. figure out where demand
is failing"*. Right on both counts. The mint-side key was the wrong
instinct, and the demand was failing in a specific, small place.

**First, a wrong turn worth recording.** I thought the demand test itself
was blind -- `cs_elem_irrepresentable` inspects only the generic ELEMENT
channel, and `cs=1022`'s content is entirely in its positional slots. Wired
the test to the slots as well (`PYC_CSSLOTDEMAND`) and it changed nothing,
because `cs=1022` was ALREADY a candidate: it arrives via confluence, and
the demand-add loop skips anything already in the candidate set
(`!css.set_in(cs)`), which is why it shows `DEMAND-ADDED` zero times. The
demand reaches the rung.

**Where it actually fails is one line further in.** `cs_content_avars`
decides which AVars the partition graph is built over:

```c
if (cs->sym->element && cs->sym->element->var && cs->added_element_var) {
  if (AVar *e = unique_AVar(cs->sym->element->var, cs)) out.add(e);
  return;                       // <- returns on the EXISTENCE of the element
}
for (AVar *v : cs->vars) if (v && v->out) out.add(v);
```

A container has TWO content channels (ifa/104), and HAVING an element
channel does not mean the content is IN it. A literal fills `cs->vars` and
leaves the element bottom -- deliberately, because that is what
`tuple_able()` tests. This returned on the mere existence of the element
var, so for `cs=1022` the graph was built over an EMPTY AVar, produced no
paths, and route 4 declined **35 times** with *"flow graph covers none of
the defs"*.

So the demand arrived, named the right CreationSet, and the rung looked in
the wrong channel. **Fall through to the positional vars when the element
carries nothing** (`PYC_CSCONTENT=1`):

```c
if (!cscontent_enabled() || (e && e->out && e->out->n)) return;
```

### Result: the flag arm reaches COMPILE PARITY with the default

| arm | compile_fail | with_warnings | container CS / shapes |
| --- | --- | --- | --- |
| default | 2 (`othello3`, `rdb`) | 43 | 2740 / 625 = 4.38 |
| default + all three | 2 (same) | 44 | 2826 / 625 = 4.52 |
| flag | **7** | 39 | 2091 / 626 = 3.34 |
| **flag + all three** | **2 (`othello3`, `rdb`)** | 45 | **2406 / 617 = 3.90** |

**The flag arm now fails to compile exactly the two programs the default
arm fails, with 12% FEWER container CreationSets than the default.**

`PYC_CSCONTENT=1` changes only three programs on the flag arm, and none
regresses at runtime:

| | | run |
| --- | --- | --- |
| `sudoku3` | compile-fail -> compiles, warns 121 -> 48 | 134, = its default-arm behaviour |
| `quameon` | warns 76 -> 69 | 134 both ways |
| `pygasus` | warns 3 -> 53 | 134 both ways -- extra warnings on an already-aborting binary |

Suite 314 passed / 0 failed on both backends with the mechanisms on and
off; six gates green.

### The three mechanisms, and what each is for

- **`PYC_VIOLCS=3`** -- a violation is a demand, and stage 5 is starved on
  every pass of both `bh` and `sudoku3`, so hand the demand to route 4
  directly (which runs unconditionally) rather than lifting a quiescence
  gate, which costs 16 suite tests.
- **`PYC_CSMEMBER=1`** -- partition a CreationSet by the MEMBERS its
  creation points reach, when the content graph names nothing. Fixes `bh`.
- **`PYC_CSCONTENT=1`** -- build that content graph over the positional
  slots when the element channel exists but is empty. Fixes `sudoku3`.

All three default 0.
