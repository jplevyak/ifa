# ifa/150 — `if x is not None:` never folds, so its dead branch is type-checked

**Found 2026-09-12** while ranking the corpus's failures by class.
[ifa/149](149-the-largest-diagnostic-class-reports-nothing.md) established
that `unresolved call` is the largest class — 34 of the 44 corpus programs
that warn, 166 warnings — and that every one names an operator dunder. This
is the single largest root cause inside it: **12 programs, 21 of the 166.**

## Two lines reproduce it

```python
print(min(2, 9))
print(min(3, 7, 1))
```

```
d.py:1:6: warning: illegal call argument type 'c' illegal: __pyc_None_type__
d.py:1:6: warning: illegal call argument type 'm' illegal: int64
d.py:1:6: warning: unresolved call '__lt__'
d.py:1:6: warning: expression has no type
```

Either line ALONE is clean. The failing send is `if c < m` inside
`__pyc__`'s `min`, on the **3-argument** path — reached from the
**2-argument** call, where `c` is exactly `None`. The guard is right there
in the source and did not prune:

```python
def min(a, b=None, c=None, key=None):
  ...
  if c is not None:      # <-- 2-arg contour: c is exactly None
    m = a
    if b < m: m = b
    if c < m: m = c      # <-- type-checked anyway, against None
```

## The guard form decides, not the guard

The same logic written as an early return is completely clean. This is the
whole bug in one pair of programs:

```python
def f(a, c=None):            def f(a, c=None):
    if c is None:                if not (c is None):
        return a                     return c + a
    else:                        return a
        return c + a
print(f(1)); print(f(2, 3))  print(f(1)); print(f(2, 3))
  -> 0 warnings                 -> 8 warnings, unresolved call '__add__'
```

`python_ifa_build_if1.cc` lowers both `x is None` and `x is not None`
through `prim_isinstance` against `sym_nil_type`, but the negative form
adds a `__not__` send:

```c
// x is/== None      ->  isinstance(x, __pyc_None_type__)
// x is not/!= None  ->  not isinstance(x, __pyc_None_type__)
```

`isinstance` folds to a single constant per contour — that is why the left
column works. The `__not__` in between threw the constant away.

## Why `__not__` lost it

`bool.__not__` MATERIALIZES a fresh bool through a branch:

```python
def __not__(self):
    if (self):
      return False
    else:
      return True
```

It is one function shared by every `__not__` in the program. In `min`'s
2-argument contour `isinstance` yields `{True}`, in the 3-argument contour
`{False}` — so the shared `__not__` contour sees `self` as `{True, False}`
and returns `{True, False}`. Neither caller's IF can prune, and the dead
branch is type-checked in both.

Nothing about the CONTOURS is wrong here: `min` itself splits correctly
(`IFA_DBG_FUNES` shows one contour with `c=int64` and one with `c=nil`).
The constant is destroyed one call deeper.

Two things that do NOT fix it, both measured:

- **Narrowing.** `is not None` narrowing exists and is applied
  (`narrowing_is_not_none_name`, `RP_IsNotNilType`), but it restricts the
  per-branch SSU view; it cannot help when the IF itself never prunes.
  The reason this shows up on `Optional[numeric]` and not `Optional[T]` is
  issue 060's carve-out: `nil` is kept in the `->type` projection only when
  the union also carries a `num_kind` scalar, so `{nil, N}` projects to
  `{N}` and dispatch succeeds regardless, while `{nil, int64}` keeps the
  `nil` and `__lt__` has no candidate for it.
- **Materializing the negation inline.** Writing the branch out by hand —
  `if c is None: t = False else: t = True` then `if t:` — fails identically
  (7 warnings): the join merges `{True, False}`. Any materialized negated
  bool is merged somewhere; that is the shape of the problem.

## Fix

`bool.__not__` takes `__pyc_clone_constants__(self)` — the device
`bool.__pyc_to_bool__` two methods above already uses, for exactly this
reason. With a per-constant contour each caller's constant survives, the IF
folds, and the dead branch is never type-checked.

### This is a mechanism, and the principled replacement is a primitive

Judged against [ifa/146](146-remove-all-arbitrary-splitting.md)'s two-question
test, a per-constant contour is not demand-driven: it would split whether or
not anything downstream asked. It is defensible here and no more — `bool`
has exactly two constants, so the partition is bounded at 2 and is not a fan
in [ifa/144](144-route-4-fans-per-creation-point-instead-of-partitioning.md)'s
sense (partition size = a count of things), and the identical device is
already sanctioned one method away.

The demand-driven replacement for the contour itself is
[151](151-split-an-entryset-on-a-constant-argument-on-demand.md): split an
EntrySet on a constant argument when, and only when, a violation is blocked
on it. When that lands, the `__pyc_clone_constants__` here can be removed
and this result must hold without it — that is 151's acceptance test.

The fix that needs no contour at all is a **foldable logical-not primitive**.
`not` of a known boolean is a pure lattice function; FA should evaluate it as
one instead of analysing a method body. `P_prim_not` is BITWISE (`~`), so
this is a new primitive: register it, fold it in FA's primitive switch
(`{True}`->`{False}`, `{False}`->`{True}`, else `bool`), emit `!` in both
backends, and use it in the `is not None` / `is not` / `not in` lowerings,
which provably negate a primitive-produced bool. That also picks up `not in`
(`othello`'s `x not in range(8)` reports `unresolved call '__not__'` today).
Until then this issue stays open on that replacement.

## Measured

Six gates green, 315/0 on both backends. `tests/minmax_3arg.py` — the
fixture that recorded this bug — goes from **32 warning lines to zero**, with
its runtime output unchanged and still matching `.exec.check`.

Per program, warnings before -> after (unresolved calls in parentheses):

| program | warnings | unresolved |
| --- | --- | --- |
| adatron | 8 -> **0** | 2 -> 0 |
| chaos | 4 -> **0** | 1 -> 0 |
| solitaire | 5 -> 1 | 1 -> 0 |
| amaze | 11 -> 7 | 1 -> 0 |
| neural1 | 22 -> 14 | 3 -> 1 (`__iter__` remains) |
| voronoi2 | 21 -> 17 | 1 -> 0 |
| sokoban | 17 -> 17 | 1 -> 1 (`__iter__`, unrelated) |

Corpus-wide (`compile__default__ae80a6ed+8aae3c6b` -> `+7a8b764e`),
**every delta is negative — no program got worse anywhere**:

```
                before   after
warning lines     1465    1382    (-83)
unresolved call    166     146    (-20)
has no type        756     737
typed illegal      528     484
programs warning    43      40
cs/shapes      2740/625  2744/624
```

Three programs go fully clean — **adatron 8->0, chaos 4->0, yopyra 16->0** —
and ten more improve (othello2 -8, softrender -8, mastermind2 -8, neural1 -8,
amaze -4, solitaire -4, voronoi2 -4, sudoku3 -4, tarsalzp -4, rubik -3).
The contour cost is flat: +4 CreationSets and one shape fewer across the
whole corpus, which is the bounded-at-2 partition showing up as noise.

A `check` sweep (`check__default__ae80a6ed+240f6fe9`) against the last one
on record (`ae16c44e+0e9deefa`, 2026-09-09) confirms the change is
warnings-only: joining the two per-program tables, **not one of the 77
programs changed `compile_rc`, `run_rc`, `cpy_rc` or `stdout_match`** --
`compile_fail=2 run_fail=38 stdout_differs=24` in both -- while
`with_warnings` falls 43 -> 40 and total warning lines 1947 -> 1382 (that
total also carries ifa/149's 482 suppressed duplicates).

`run_fail=38` is the corpus's standing state, not a regression: most of
those programs already crashed, which is why a `compile` sweep is never
sufficient evidence on its own.

## Still failing, same family

A nested guard with a defaulted parameter is not fixed:

```python
def f(a, c=None):
    if c is not None:
        if c < a:          # still `unresolved call '__lt__'`
            return c
    return a
print(f(1)); print(f(2, 3))
```

The same program with `c` as a required parameter (`f(1, None)`) IS clean, so
the remaining half involves the defaulted-parameter wrapper. Open.

## The rest of the class

The other root causes behind `unresolved call`, to work in order:

- `range` has no `__contains__` while nine other classes do, and
  `emit_in_pyda` dispatches `__contains__` directly with no fallback to the
  iterable protocol — the gap `__pyc_generator__.__contains__` and
  `__pyc_iterator__.__contains__` were each added to plug by hand
  (`othello`, `sudoku3`).
- `bool` lacks its int-subtype operators; `00_runtime.py` already documents
  why `__lt__`/`__gt__` were added and warns such a gap "cascaded into
  unrelated NOTYPE collapses". `__xor__` is the next instance (`quameon`,
  `softrender`).
- `__iter__` unresolved on a generator expression passed to `max`, and on
  `list(<dict>)` (`life`, `sokoban`, `neural1`, `doom`, `plcfrs`).
