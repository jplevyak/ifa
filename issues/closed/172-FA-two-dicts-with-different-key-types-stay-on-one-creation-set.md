# 172 — containers of different element types stay on one CreationSet at module scope

**Status: FIXED 2026-09-26.** Found while landing
[issues/118](../../../issues/closed/118-set-and-dict-are-linear-scans.md). It
is not caused by 118: the linear dict at `752544ed` fails identically.

## Symptom

```python
class B:
    pass
d = {}
d[3] = 1
b = B()
num = {}
num[b] = 2
print(d[3], num[b])
```

```
k1.py:7:32: error: illegal primitive argument type 'x' illegal: B
    num[b] = 2
```

The same at `752544ed` for:
- an int set and a str set in one program: 6 errors;
- an int set and a tuple set: 9 errors;
- an int set and a float set: compiles with NO diagnostics. The union is
  pure numeric, so coercion widens it to float, and LLVM then answers
  membership wrongly (286 hits on C, 1 on LLVM).

In short, it hits any program holding two containers of one kind with
different element types, which is most programs, **but only at module
scope**. The same eight lines inside a function compile and run.

## Root cause

**The setter walk never reached a module-level container's allocation
site.**

`split_css` partitions a CreationSet by the setters of its creation
points. It can only do that for creation points that have been recorded as
*starters*: AVars that carry a `cs_map` (an allocation site) and have
setters. Setters are recorded where a write happens
(`update_setter(x->container, x)`, on the AVar holding the container at
the write) and propagate BACKWARD along `av->backward` to the allocation
site.

At module scope `d` and `num` are globals. ifa/050 stage 1 resolves every
read of a module-level cell to the store that dominates it and applies it
with `update_gen`, a SNAPSHOT (deliberately: it is flow-sensitive, and a
write-only cell must stay unobservable to BOXING). A snapshot has no
backward edge. So the setters written by `d[3] = 1` inside `__setitem__`
climb to `__main__`'s read of `d` and stop. They never reach
`d = {}`'s allocation AVar:

```
                     setter AVars   with cs_map   starters   split_css
function (k4)             57              2            2          1   <- splits the dicts
module   (k1)             57              0            0          0
```

With no starter, the two dicts stay on one CreationSet. Their `_keys` lists
are then both stored into one merged `_keys` field, so the keys-list
contour cannot be partitioned either. It reports "1 group: every creation
point on the same assign sets", because the key difference (`int` vs `B`)
is observed there but its separation depends on the dict splitting first.

This is the same defect ifa/146 B found and fixed for ONE walk,
`build_cs_flow_graph`'s backflow. That fix taught that walk to cross a
folded load by asking `provably_constant_load` for the store's value. The
setter walk had the identical gap and was never given the crossing.

## Fix

`folded_load_source(a)`, factored out of ifa/146 B's inline code, returns
the stored value when `a` is a folded global-cell load. `update_setter`
now also continues to it when `a` has no backward edge, and
`build_cs_flow_graph` uses the same helper. Reachability crosses the fold;
type propagation is untouched (the snapshot stays a snapshot).

Measured on the repros, all at module scope, all now compiling and
matching CPython on both backends: int/object-keyed dicts, int/str sets,
int/tuple sets, int/float sets (LLVM now answers 286).
`tests/dicts_int_and_object_keys.py` (formerly this issue's known-issue
test) and `tests/containers_of_different_types_module_scope.py` pin it.

## What this exposed

An empty `dict([])` next to a populated `dict([...])` no longer shares its
CreationSet, which is correct. Its own contour then has a bottom key
type, and FA type-checks loops that never run over it
(`'pair' has no type`). That program failed at `752544ed` too. It is
[160](160-list-eq-indexes-an-empty-literal.md)'s family, and
`tests/dict_empty_next_to_populated.py` is its known-issue test.

## Negative results on the way (do not repeat)

- **Backtracking the demand into the container.** A forward walk from the
  demanded keys list to the container field it is stored in does reach the
  merged dict, but the dict's partition key (per field, by written type)
  sees both creation points on every field. Nomination was not the gap.
- **Partitioning the container by which assign set it owns.** It correctly
  finds the loads (`self._keys` in the two `__setitem__` contours) but
  cannot trace their receivers back to the allocation sites, for the same
  reason as above: the backflow stops at the folded global load. In a
  function it was not needed at all, since SETTER_OF_SETTER already splits
  the dicts. Both were reverted; the fix is one level lower.

Lesson: when a split works inside a function and not at module scope, look
for a walk that does not cross ifa/050's folded global loads before looking
at the partition keys.
