# 118 — `{bool, None}` has no representation

**Status:** open, narrow. Rewritten 2026-09-28. The original title ("the
last two corpus compile failures: union-field width, and polymorphic
field offsets") is obsolete. `go`'s offset half was an inference
imprecision, fixed by 124's splitter fix. chess stopped producing the
union once its source returned `False` explicitly, and chess now matches
CPython (three codegen heap bugs were found and fixed on the way; see
git). History:
`git show 3f36072b:ifa/issues/118-union-field-representation-and-polymorphic-field-offset.md`.

## Fixture

`tests/bool_or_none_fallthrough.py` (`.known_issue`), 7 lines, reduced
from chess:

```python
def f(xs):
    for k in xs:
        if k:
            return False
    # falls off the end -> None
print(f([0, 1]))            # CPython: False
```

```
mismatched field members: bool(1) __pyc_None_type__(8)
fail: mismatched field sizes: class 'closure' field '<anon>' mixes 1- and 8-byte members
```

The loop matters. Without it, the implicit-None fall-through is folded
away.

## What the union is

A GENUINE CPython union: `f` returns `False` on one path and `None` on
another. Unlike AGENTS.md's corpus cases, nothing merged it, and no split
can separate it (one call, one result). So this is a REPRESENTATION
question, AGENTS.md's legitimate third category, and its answer belongs
behind `IFACallbacks`:

- `{None, T}` for a pointer-shaped `T` is a nullable pointer (closed/164).
- `{None, int64}` is refused. Every 64-bit pattern is a valid int, so a
  null test cannot tell `None` from `0` (`tests/none_int_field_zero.py`,
  issues/048).
- **`{None, bool}` has spare values.** A `bool` needs one bit, so a
  one-byte tri-state (`0` False, `1` True, `2` None) represents the union
  exactly, with no boxing, and `is None` is `== 2`. The same holds for any
  scalar whose value set leaves a spare code.

Widening the slot alone (`PYC_WIDEN_UNION_FIELD=1`, default off) was
measured: it gets past `determine_layouts` into 18 C errors, because the
member's C type comes out `void`. The representation has to be chosen and
applied at every read and write, not just sized.

## Fix

1. A representation callback: given `{nil, scalar S}`, return a sentinel
   encoding if `S` has spare codes (bool: yes; int64 / float64: no).
   Otherwise refuse, as today.
2. Codegen: the slot's C type is `S`'s. A `None` store writes the
   sentinel. `is None` / truthiness / `==` compare against it. Reading
   the value as `S` where the analysis proved non-None is plain.
3. Delete `PYC_WIDEN_UNION_FIELD`: it is a lever for the wrong mechanism.

## Verification

The fixture prints `False` on both backends. A variant that returns the
fall-through `None` prints `None`. `tests/none_int_field_zero.py` is
still refused.
