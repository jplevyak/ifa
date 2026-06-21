# Issue 024: IFA doesn't narrow union types on `is None` comparisons

**Status:** open.
**Affects:** `ifa/analysis/fa.cc` (the splitter that decides
when to refine an EntrySet based on conditional predicates).
**Related:**
[`issues/004-is-operator-unimplemented.md`](../../issues/004-is-operator-unimplemented.md)
— the partial fix for `is` that exposed this gap.

## Symptom

After [issue 004](../../issues/004-is-operator-unimplemented.md)'s
partial fix lands the `is`/`is not` operators, two
follow-on patterns crash at codegen with a "matching
function not found" runtime assertion:

### Pattern 1 — polymorphic None-LHS dispatch

```python
a = None
b = None
c = 5
print(a is b)  # OK
print(a is c)  # crash
```

`__pyc_None_type__.__is__(self, x)` is dispatched once with
`x: None` and once with `x: int`.  IFA tries to specialize
__is__ over a polymorphic `x` and the inner dispatch on
`x.__null__()` fails because IFA doesn't decompose the
union {None, int} into per-branch dispatches.

### Pattern 2 — recursive-type narrowing

```python
class Node:
  def __init__(self, v):
    self.value = v
    self.next = None
def sum_list(node):
  if node is None:
    return 0
  return node.value + sum_list(node.next)
```

Compiles but crashes at runtime.  `node` has union type
{None, Node}.  IFA needs to narrow `node` to Node-only in
the False branch of `if node is None`, but it doesn't.
Then `node.value` (in the False branch) tries to dispatch
on Node|None and fails on the None side.

## Root cause

IFA's setter-splitting strategy refines EntrySets based on
predicates' type-correlated outcomes.  Looking at
`ifa/IFA.md` §6 (splitting), the existing mechanism handles
splits driven by direct comparison primitives (`==`, `<`,
etc.) but NOT by user-method dispatches like `__is__`.

When `node is None` lowers to `node.__is__(None)`, IFA sees
a regular method call returning a bool.  It doesn't know
the return value type-correlates with the receiver's narrow
type ("True iff `node`'s runtime type is None").  So no
narrowing happens.

Pattern 1 is a slightly different variant: the dispatch
itself fails because IFA's per-EntrySet specialization
can't handle a polymorphic `x` inside `__is__`'s body when
`__is__` has only one definition.

## Proposed fix sketches

### Option A — frontend rewrite

In `python_ifa_build_if1.cc:102`'s `PY_CMP_IS` handler,
detect when one operand is the None constant and emit a
direct `__null__()` call on the other operand instead of
going through `__is__`:

```cpp
case PY_CMP_IS:
  if (lhs_is_none_constant || rhs_is_none_constant) {
    // Emit non_None_side.__null__() directly.
    return make_symbol("__null__");
  }
  return make_symbol("__is__");
```

`__null__()` is type-correlated (only the None singleton
returns True), so the existing setter-splitter SHOULD be
able to narrow on its return value — assuming the splitter
recognizes `__null__` as a discriminator.  May need
explicit annotation.

### Option B — IFA-level support for `is` as a typed
primitive

Promote `is` from a user-method dispatch (`__is__`) to a
proper IF1 primitive (`prim_is_none` or similar) that the
splitter knows narrows.  Mirrors `prim_isinstance` which
already has this property (the splitter uses it).

This is cleaner but touches more code: `prim_data.dat`, the
frontend lowering, FA's splitter logic, and any code that
dispatches on primitives.

### Option C — generalize the splitter to union dispatch

Pattern 1 (polymorphic `__is__` with union `x`) would be
unblocked by making IFA's per-EntrySet specialization
decompose union-typed arguments into per-branch dispatches
recursively.  This is a broader change.

## Verification plan

When fixing:

1. The `print(a is b); print(a is c)` two-statement test
   compiles and prints `True\nFalse`.
2. The recursive linked-list pattern in
   [`issues/004`](../../issues/004-is-operator-unimplemented.md#whats-still-broken)
   compiles and runs.
3. `tests/recursive_alloc_basic.py` and the other recursive
   tests still pass.
4. Pyc suite stays at ≥ 85/0 on both backends.
5. A new `tests/recursive_list_is_none.py` exercises the
   pattern that fails today.

## What this unblocks

- The full linked-list / tree / fib-heap data structure
  test set that was the original motivation for issue 004.
- Any pyc program using `is None` for sentinel checks
  inside method bodies.
- A broader class of Python ports from CPython that rely on
  None-narrowing.

## Related

- `python_ifa_build_if1.cc:102` — `PY_CMP_IS` → `__is__`
  symbol mapping.
- `__pyc__/00_runtime.py` — where the partial fix landed.
- `ifa/IFA.md` §6 — setter-splitting docs.
- `ifa/if1/prim_data.h` — `prim_isinstance` (the existing
  primitive that the splitter already narrows on; a
  potential model for `prim_is_none`).
