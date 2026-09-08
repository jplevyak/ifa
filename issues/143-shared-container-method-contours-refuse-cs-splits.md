# 143 — a shared container-method EntrySet re-fuses every CreationSet the splitter separates

**Status: open.** Root-caused 2026-09-07 on `bh`, the only
`shedskin_examples` program that RUNS CORRECTLY at the default and aborts
under `PYC_CSDCPA1=2`.

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

## Root cause: the element channel has ONE writer per method, program-wide

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

## What the fix has to be

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
