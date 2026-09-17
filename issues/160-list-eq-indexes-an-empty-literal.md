# ifa/160 — `list.__eq__` indexes an empty-list literal

**Status:** open, root-caused 2026-09-17. `shedskin_examples/dijkstra2`'s only
4 errors.

## Repro — six lines

```python
def f(xs):
    p = []
    for x in xs:
        p = [x]
    return p == []
print(f([3.0]))
```

CPython prints `False`. pyc refuses with 4 errors:

```
expression has no type
unresolved call '__ne__'
illegal call argument type expression illegal: float64
  called from __pyc__.py:1400
```

## Mechanism

`__pyc__`'s `list.__eq__` is

```python
ll = __pyc_clone_constants__(len(l))
lself = __pyc_clone_constants__(len(self))
if lself != ll:
    return False
for i in range(lself):
    if l[i] != self[i]:      # <- __pyc__.py:1400
        return False
```

`p == []` binds `l` to the empty literal. FA cannot decide `lself != ll`
statically, so it analyses the loop body and indexes `l` — an empty container
whose element channel is bottom. `l[i]` is NOTYPE, and `!=` on it is an
unresolved call.

At runtime the read never happens: the length guard returns first. This is the
empty-container-element residual — the same shape
`tests/empty_container_elem.py` documents, where the conclusion was that the
honest fix is a clean codegen trap rather than seeding the element.

## What is and is not affected — measured

| expression | result |
| --- | --- |
| `p == []` | **4 errors** |
| `p != []` | **4 errors** |
| `p == [0.0]` | clean |
| `len(p) == 0` | clean |
| `not p` | clean |

And the loop matters:

| | result |
| --- | --- |
| `p = []; p = [1.0]; return p == []` (straight line) | clean |
| the same at module level | clean |
| the same with `p = [x]` inside a `for` | **4 errors** |

So it needs both the empty literal on the right of `==`/`!=` **and** a
loop-carried assignment to the left operand. Without the loop the two lists
unify and the element is typed; with it they do not.

## Why it matters beyond dijkstra2

`x == []` is an ordinary Python idiom and the failure is silent about its real
cause — the message points at `__pyc__.py:1400`, inside the builtin, not at the
user's comparison. Under the pre-[158](158-FA-every-type-violation-is-fatal.md)
severity it was four warnings and a compiled binary.

## Candidate fixes, none yet measured

1. **Do not report a violation for an element read from a provably-empty
   container**; let codegen emit the trap. This is
   `tests/empty_container_elem.py`'s own proposal and would close the whole
   family, not just `==`.
2. **Unify the two elements at `list.__eq__`** — comparing two lists is only
   meaningful if their elements are comparable, so an empty operand can adopt
   the other's element type. Narrower, and it leaves the general empty-read
   residual open.
3. Lower `x == []` to a length test in the frontend. Sound when `x` is
   statically a list, but it is a peephole on one idiom and leaves `l[i]`
   reachable from every other path into `__eq__`.

## (1) TRIED AND REVERTED, 2026-09-17 — and (3) is unsound

**Option 1, the codegen-trap route, does not close it.** Implemented as
`PYC_EMPTYREAD`: suppress the NOTYPE when the value is defined by a SEND one of
whose arguments resolves to containers that are ALL provably empty
(`cs->sym->element` present, `static_arity == 0`, `!no_static_arity`,
`!vars.n` — so a container that might be non-empty, or whose length varies at
run time, still reports).

It fires — `EMPTYREAD suppressed=4` on the repro — and the error count does not
move:

| | errors |
| --- | --- |
| off | 4 |
| on, suppressing NOTYPE at the read | 4 (`suppressed=4`) |
| on, also dropping violations whose subject IS the read (one hop) | 4 |

The four that remain are `expression has no type` x2,
`illegal call argument type ... float64`, and `unresolved call '__ne__'` — and
they are raised in **other contours**, so neither the read-site check nor a
one-hop filter on the violation's subject reaches them. Closing it this way
means chasing the same dead path through every contour it touches, in several
violation kinds. That is the symptom in several places, not the cause, so it
was reverted rather than extended.

**Option 3 is unsound and is withdrawn.** Lowering `x == []` to `len(x) == 0`
is only correct when `x` is statically a `list` — `{} == []` and `set() == []`
are `False` in CPython, not length tests — and the frontend does not know `x`'s
type. That is FA's job, and by the time FA knows, the lowering has happened.

## Where that leaves it

The obstacle is a **correlation** FA does not do: past `if lself != ll: return
False`, `lself == ll`, so when `l` is the empty literal (`ll` folds to 0) the
loop `range(lself)` has zero iterations. FA analyses the body anyway, indexes
`l`, and every violation downstream follows from that one unreachable read.

So the candidates that remain are about making the path actually dead, not
about hiding its diagnostics:

- **Propagate the guard.** After `lself != ll` returns, narrow `lself` to
  `ll`'s value on the fall-through edge. With `ll` a folded 0 the loop bound is
  0. This is ordinary comparison-narrowing (the `is_not_none_narrow` family)
  extended to integer equality against a constant, and it would close the whole
  shape rather than the `==` case.
- **Give `range(0)` an empty element**, so `for i in range(lself)` with a
  folded 0 yields no iterations and the body is unreachable. Narrower, and it
  depends on the first one to know the bound is 0.

Neither is a splitter question, which is why this issue is filed apart from
the ifa/157 thread.

## WHY the type is bottom, and what shedskin does

**Author, 2026-09-17: "Why in that mini example is any type bottom?"** Because
pyc's own arity-keyed identity puts the empty literal in a contour that is empty
by construction:

```
cs=987 sym=list vars=1 defs=2 arity=1   <- the [x] literals, element float
cs=991 sym=list vars=0 defs=6 arity=0   <- the [] literals: no slots, no element
```

`[]` and `[x]` have different arity, so [132](132-arity-is-representation-not-provenance.md)
keeps them apart. `cs=991` has no positional slots and nothing ever written to
its element channel, so `l[i]` can only be bottom. **It is not a failure to
infer — there is nothing there to infer**, and the read is statically out of
bounds.

**shedskin does not have this problem because its list type has no arity.**
Compiled on the same repro it emits, for both literals:

```cpp
p = (__ss_list<__ss_float>());
return ___bool(__eq(p, (__ss_list<__ss_float>())));
```

`list<__ss_float>` for the empty literal too — unified with `p`, length left to
run time. There is no arity-0 type to be empty.

## `PYC_EMPTYJOIN` — fixes the repro, not `dijkstra2`

ifa/132's own safety net already says what should happen when arities disagree:
`make_kind` sets `no_static_arity`, the CS drops to LIST layout and reads its
length at run time. The identity key prevents the two from ever meeting, so the
net never fires. `PYC_EMPTYJOIN=1` lets an arity-ZERO container join a CS of
another arity (tuples excluded — a 0-tuple is a distinct representation, which
is ifa/132's point) and lets the net do the rest.

| | result |
| --- | --- |
| the six-line repro, off | 4 errors |
| the six-line repro, on | **compiles, runs, prints `False` = CPython** |
| `dijkstra2`, off | 4 errors |
| `dijkstra2`, on | **4 errors, identical** |

It does change `dijkstra2`'s contours — 21 list CreationSets become 20, and 19
of them drop to list layout — but not its diagnostics. **So the repro is not
`dijkstra2`'s shape**, and the reduction has to be redone from the program
rather than from the idiom. `finalpath` is built by `paths[0][w] + revpath[1:]`,
a `list.__add__` result with UNKNOWN arity (-1), not a literal; that is the
difference from the repro and the next thing to reduce.

Left at default 0. Suite 312/0 with it off; it has had no corpus measurement,
which it needs before it could be anything else — merging arity-0 containers
broadly is a real precision change, not a free one.

## The real repro is dict-mediated — `PYC_EMPTYJOIN` does NOT fix it

The six-line repro above was not `dijkstra2`'s shape. Reducing from the program
instead of from the idiom gives seven lines,
`tests/empty_list_compare_via_dict.py`:

```python
def f(xs):
    d = {}
    p = []
    for x in xs:
        d[0] = [x]
        p = d[0]
    return p == []
```

The list reaches `p` through a **dict element channel**. That is the difference,
and it is what `dijkstra2` does (`paths[dir][w]`).

| | EMPTYJOIN=0 | EMPTYJOIN=1 |
| --- | --- | --- |
| `p = []; p = [x]` (no dict) | 4 errors | **0** |
| `[x] + p`, `[x][:]`, the 9-line slice/reverse chain | 4 errors | **0** |
| **via a dict** (`p = d[0]`, `d[0] + [x]`, `[x] + d[0]`, `list(d[0])`) | 4 errors | **4 errors** |
| `dijkstra2` | 4 errors | **4 errors** |

**And the contours say why the join is the wrong mechanism.** Under
EMPTYJOIN=1 on the dict repro:

```
cs=1035 vars=1 defs=6 no_arity=1 elem=           <- the six [] literals went HERE
cs=987  vars=1 defs=1 no_arity=1 elem= float64   <- the list they are compared against
```

Six empty literals merged into a contour whose element is **still bottom**,
while the one carrying `float64` sits beside it. The join picks whichever
creator is arity-compatible and comes first; it is not unification. shedskin's
is TYPE-driven — the empty literal's `T` comes from the context it is compared
with — and no amount of merging by arity reproduces that, because an empty
literal has no element for the merge to find.

So `PYC_EMPTYJOIN` was **removed**, and the account kept at
`creation_point`. Two mechanisms are now measured dead on this issue:
suppressing the violations (re-raised in other contours) and joining arity-0
containers (merges into an arbitrary contour).

What remains is still the same statement, now with a fixture pinning it: an
index into an **arity-0** contour is statically out of bounds, and the honest
fix is to treat that PNode as the dead code it is — not to give the read a
type, and not to hide its diagnostics after the fact.

## Does it generalize? Measured — the union case already works

**Author: "What if a 0-arity list mixes with a non-zero then under a len test
dereferences? Does your proposal generalize?"**

It does, and that case needs nothing — it already works today:

| | errors | pyc | CPython |
| --- | --- | --- | --- |
| union {arity-0, arity-1}, `if len(p) > 0: return p[0]` | 0 | `3.0` | `3.0` |
| union, unguarded `return p[0]` | 0 | `3.0` | `3.0` |
| **pure** arity-0, `if len(p) > 0: return p[0]` | 0 | `0.0` | `0.0` |
| pure arity-0, `p[0] != 1.0` (method dispatch on a bottom receiver) | 0 | `False` | `False` |
| pure arity-0, `for i in range(len(p)): if p[i] != 1.0` — `list.__eq__`'s own shape | 0 | `False` | `False` |

The proposal is per-CreationSet, so it composes the right way: for a UNION
receiver the arity-0 member contributes bottom, which unions away against the
other member's element, and the read is typed from the non-empty member. Only a
receiver that is *purely* arity-0 yields bottom overall. Nothing has to reason
about the `len` test at all.

### The failing case is narrower than "index into arity-0"

Even a pure arity-0 receiver is fine in user code (rows 3-5 above). The failure
needs the empty container to arrive as a **formal**, i.e. across a call into a
shared function that indexes it. `list.__eq__`'s shape written out in pure user
code, 13 lines, reproduces it exactly:

```python
def eq(s, l):
    if len(s) != len(l):
        return False
    for i in range(len(s)):
        if l[i] != s[i]:
            return False
    return True
def f(xs):
    p = []
    for x in xs:
        p = [x]
    return eq(p, [])
```

5 errors — `expression has no type` x3, `illegal call argument type ... float64`,
`unresolved call '__ne__'` — while the same loop with a literal operand
(`p[i] != 1.0`) is clean. The NOTYPE check requires `av->live_arg`, which is
what a formal's derived value has and a local's does not.

**So the blast radius of the fix is small**: not every index into an empty
container, but one reached through a formal bound to a purely arity-0 argument.
That is `list.__eq__`, `list.__ne__` and their relatives in `__pyc__`, and it is
why the idiom `x == []` is what surfaces it.
