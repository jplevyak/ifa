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
