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
