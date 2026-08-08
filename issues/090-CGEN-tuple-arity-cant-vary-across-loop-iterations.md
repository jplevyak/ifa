# 090 — a variable whose tuple arity (or None-vs-tuple shape) changes across loop iterations can't resolve a call site

**Status:** open, found 2026-08-08 while diagnosing
[issues/025](../../issues/025-shedskin-examples-coverage.md)'s TODO
list item 4 (sunfish's blocker — the doc's own "`sizeof_element of
non-container type` internal fail in `__add__`" claim is **stale**;
re-verified today, that specific fail no longer appears in sunfish's
compile output at all, superseded by this one and by
[ifa/issues/089](089-DISPATCH-closure-pyc-to-bool-no-candidate.md)-adjacent
gaps found along the way, see issues/025 for the full trace).

**Affects:** `ifa/codegen/cg.cc`'s `get_target_fun` (~line 953),
which hard-`fail`s the whole compile when a call site's receiver type
has no single resolvable target function. Root cause is upstream —
tuples are fixed-arity `Type_RECORD`s (one concrete type per arity,
per [ifa/issues/closed/069](closed/069-per-arity-tuple-types-scope.md)) — so
a single `Var` that needs to hold *different arities* (or `None` vs.
a tuple) across a loop's iterations has no single concrete type for
FA to settle on, and codegen has nothing to call through.

## Repro 1 — accumulating tuples via `+` in a loop

```python
t = ()
for i in range(3):
    t = t + (i, i+1)
print(t)
```
- CPython: `(0, 1, 1, 2, 2, 3)`.
- pyc: `fail: unable to resolve to a single function at call site` —
  a clean compile-time rejection, not a crash. Each iteration's `t +
  (i, i+1)` needs a *different* concrete tuple-arity type for `t`
  (0-tuple, then 2-tuple, then 4-tuple, then 6-tuple...) — impossible
  to give the loop-carried `t` one static type.

This is the mechanism behind sunfish.py:75's
`padrow = lambda row: (0,) + tuple(x+piece[k] for x in row) + (0,)`
combined with `sum((padrow(...) for i in range(8)), ())` (line 76) —
even after fixing `sum()`'s missing `start` parameter (this session,
see [issues/025](../../issues/025-shedskin-examples-coverage.md)'s
item 4), the accumulator inside `sum()`'s shared loop body still needs
to hold a growing tuple across iterations, hitting this exact wall
(confirmed: re-running sunfish.py post-`sum()`-fix, this is now the
*first* error reported, at line 75).

## Repro 2 — a loop-carried `None`-or-tuple variable

```python
def gen_moves():
    return [(1, 2), (3, 4)]

move = None
while move not in gen_moves():
    move = (1, 2)
print(move)
```
- CPython: `(1, 2)`.
- pyc: identical `fail: unable to resolve to a single function at
  call site`.

This is sunfish.py:448's `while move not in hist[-1].gen_moves():`
(`move = None` initially, reassigned to an actual move tuple inside
the loop) — a second, independent trigger of the same underlying
"receiver type can't settle on one concrete shape across the loop"
mechanism, this time via `None`/tuple rather than two different tuple
arities. Not confirmed whether this is `__contains__`/`__eq__`
dispatch specifically or something more general about `move`'s type;
not traced further.

## Why not root-caused/fixed further here

Both repros hit the exact same `fail()` site
(`get_target_fun`/`cg.cc:956`) via what looks like the same
underlying cause (a `Var` needing more than one concrete tuple-family
type across loop iterations), but this session's digging only
confirmed the *symptom* and its call site, not why FA lets these
programs reach codegen with an unresolved type instead of catching
it earlier (or whether catching it earlier is even possible without
a deeper representational change — [ifa/issues/closed/069](closed/069-per-arity-tuple-types-scope.md)'s
own history suggests per-arity tuple types are foundational and
unlikely to change lightly). This may be a genuine architectural
limit (Python's dynamic tuple-arity-changing-in-a-loop pattern is
fundamentally in tension with a compiler that gives every loop-carried
variable one static type) rather than a bug with a real fix — worth
someone with FA context making that call explicitly rather than this
session guessing.

## Verification plan once addressed (or ruled permanently out of scope)

- Both repros above should either compile and match CPython, or (if
  ruled architecturally infeasible) get a clean, documented,
  compile-time diagnostic explaining why, rather than the generic
  "unable to resolve to a single function at call site" message.
- `shedskin_examples/sunfish/sunfish.py` — re-run `pyc -r` and confirm
  whether this was the last blocker or whether (per this session's
  repeated pattern) another layer is underneath.

## What this unblocks

sunfish.py's `pst` (piece-square table) construction and its move-input
loop, both idiomatic, common shapes (flatten-via-`sum`, "no selection
yet" sentinel-then-loop). More generally, any program that grows a
tuple's arity across loop iterations or lets a loop-carried variable's
type span `None`/tuple.
