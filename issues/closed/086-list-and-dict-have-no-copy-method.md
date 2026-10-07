# 086 — `list.copy()` / `dict.copy()` do not exist in the builtin library; the value is untyped

**Status: FIXED 2026-10-07** (see "Fixed" below); re-diagnosed 2026-09-28. Filed 2026-08-07 as "a
self-recursive function whose recursive call passes `arg.copy()` degrades
to NOTYPE". The recursion was never the cause.

## Symptom

```python
l = [1]
m = l.copy()          # error: 'm' has no type
d = {"a": 1}
e = d.copy()          # error: 'e' has no type
```

Both fail at HEAD (`3f36072b`) with no recursion at all. `__pyc__/`
defines `__pyc_copy__` (`04_sequence.py:520`, `00_runtime.py:54`, `:286`)
and `__deepcopy__`, but no public `copy` on `list` or `dict` (or `set`).
So `x.copy()` resolves to nothing, its result is bottom, and everything
downstream cascades. The original 2026-08 repro:

```python
def search(values, n):
    if n == 0: return values
    v2 = values.copy()
    return search(v2, n - 1)
print(search({'a': 1}, 3))   # CPython: {'a': 1}
```

used to compile with warnings and abort at run time. Since closed/158
(every violation fatal) it is a compile error: `'v2' has no type`.

## Why it matters

It is `sudoku4`'s first error (`sudoku4.py:40`,
`search(assign(values.copy(), s, d))`: `unresolved member 'copy' of class
'dict'`), and the Norvig-solver idiom that `sudoku2` also uses. Any
precision diagnosis of sudoku4 (see
[129](../129-plan-demand-driven-creation-set-splitting.md) A) made before this
is fixed measures a cascade, not the splitter.

## Fix

Add `copy` to `list`, `dict` and `set` in `__pyc__`, delegating to the
existing `__pyc_copy__` (shallow copy, same semantics as CPython). This is
a builtin-library change, not FA. If it lands, move this file to the
top-level `issues/` tree's convention or close it here with the commit.

## Then re-test the recursive shape

Once `copy` resolves, re-run the recursive repro above and sudoku4. A copy
fed back into its own source through recursion is the self-feeding family
that closed/074, closed/168 and closed/176 resolved for `deepcopy`. If the
shallow `copy` path shows contour growth or a fused element, it belongs
there (the owner lift / recursive gate), not in a new mechanism.

## Verification

`m = l.copy(); m.append(2); print(l, m)` prints `[1] [1, 2]`. `d.copy()`
likewise. The recursive repro prints `{'a': 1}`. `sudoku4` gets past line
40. Six gates green.

## Fixed (2026-10-07)

`list.copy`, `dict.copy` and `set.copy` exist (`__pyc__/04_sequence.py`,
`07_dict.py`, `08_set.py`), each a shallow copy. A new dict or set gets
fresh copies of `_keys`/`_vals`/`_index` (or `_items`/`_index`), and the
same element references.

Doing it exposed a second bug the plan above would have kept. dict and set
had no `__pyc_copy__` of their own, so `copy.copy` fell back to
`object.__pyc_copy__`, the copy primitive's one-level struct clone, which
shared the original's storage lists. Overwriting a key through
`copy.copy(d)` changed `d`, and `discard`/`add` on `copy.copy(s)` changed
`s`. Appending only looked right, because the original's own `_len` hid the
new slot. Both classes now define `__pyc_copy__` as `copy()`.

Verified: `tests/container_copy.py` covers each copy, `copy.copy`, the
overwrite case and the recursive repro above, and matches CPython on both
backends. sudoku4 compiles with no diagnostics; its 11,411 output lines
match CPython (bar `TIME`). No contour growth from the recursive shape.
Check sweep `a208ca8d+fd9a8c4c` against `4179d8cb+0f64aa0f`: sudoku4 is
the only status change, and no stdout changed.
