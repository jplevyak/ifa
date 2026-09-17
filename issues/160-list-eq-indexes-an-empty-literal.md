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

(1) is the one that matches what the repo has already concluded about this
family; it should be measured first, against the `empty_container_elem`
fixture and this one.
