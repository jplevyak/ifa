# 109 — tuple slicing is unimplemented: `t[0:2]` aborts

**Status:** open, found 2026-08-18 while digging into `sunfish`'s crash.
Repro: `tests/tuple_arity_union_slice.py` (`.known_issue`) — and a much
smaller one below.

> **CORRECTION.** This issue was first filed as *"slicing a
> `{tuple(N), tuple(M)}` union aborts"*, blaming mixed arity. **That was
> wrong**, and the correction is the whole point of the issue:

## Two lines are enough

```python
t = (1, 2, 3, 4)
print(t[0:2])
```

| | result |
|---|---|
| CPython | `(1, 2)` |
| **pyc** | **SIGABRT** — `runtime error: list index type mismatch` |

No union. No dict. No differing arity. **A plain tuple, a constant
slice.**

## Cause

`__pyc_getslice__` is defined **only on `class list`** in
`__pyc__/04_sequence.py`. `class tuple` has `__getitem__`, `__setitem__`,
`__iter__`, `__len__`, `__contains__` — but no slice method — so `t[i:j]`
resolves to something that cannot index a tuple and the emitted guard
fires.

`list` slicing works fine (`[1,2,3,4][0:2]` → `[1, 2]`).

## What the mixed-arity theory got wrong

The original diagnosis came from `sunfish`, where the sliced value *does*
have unioned arity, and from a `DISPATCH FAIL` line showing two
`__pyc_getslice__` candidates. But the controls disprove it:

| variant | result |
|---|---|
| mixed-arity tuples in a dict, sliced | aborts |
| **same-arity** tuples in a dict, sliced | **also aborts** |
| **plain tuple, constant slice, no union at all** | **also aborts** |
| mixed-arity tuples, *iterated* and `len`-ed (no slice) | **works** |

Arity is not the variable; **slicing** is. The union in `sunfish` is
incidental, exactly as the tuples in
[104](closed/104-unify-list-and-tuple-in-analysis.md) turned out to be
incidental passengers in a degenerate type.

So [104](closed/104-unify-list-and-tuple-in-analysis.md)'s conclusion —
that mixed-arity tuples cause no corpus failures — **stands after all**.
The correction I made to it on the strength of this issue has itself been
retracted.

## Attempted fix, and why it is not one line

Adding a `tuple.__pyc_getslice__` mirroring `list`'s — on the reasoning
that `cg.cc` builds every tuple with `_CG_prim_tuple_list`, which sets a
real list header, so `_CG_list_getslice` should apply — **crashes the
compiler** (SIGSEGV during compilation). Reverted.

The real difficulty is the return type. `list.__pyc_getslice__` returns a
`list`; a tuple slice must return a **tuple**, and with a runtime range
its arity is unknown — which pyc's fixed-arity record tuples cannot
express. `sunfish`'s `table[i*8:i*8+8]` has a runtime start and a
constant length, so a constant-length special case would cover it, but
the general case wants the variable-length tuple representation
[104](closed/104-unify-list-and-tuple-in-analysis.md) prototyped.

## Verification plan

- `t = (1, 2, 3, 4); print(t[0:2])` prints `(1, 2)`.
- `tests/tuple_arity_union_slice.py` prints
  `(0, 1, 2, 0) (0, 5, 6, 0)`; delete its `.known_issue` tag.
- `sunfish` compiles **and runs**.
- `list` slicing is unaffected.
