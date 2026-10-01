# 175 — a loop over a never-written list is live, and its element is bottom

**Status: closed 2026-09-28** — fixed in `__pyc__/04_sequence.py` (see
*Fix*). Root-caused the same day while chasing why the ifa/074 owner lift
broke `shedskin_examples/chull`. The lift did not cause it. It split
contours correctly and exposed this pre-existing defect.

## Symptom

`tests/empty_list_loop_live_via_shared_compare.py` (known issue):

```python
def check(a):
    if a < 0: print("neg")
def empty():
    r = []
    return r
check(len(sys.argv) - 5)
L = [1]
L.extend(empty())
print(len(L))
```

```
error: 'x' has no type
error: expression has no type
error: illegal call argument type expression illegal: closure
```

CPython prints `neg` and `1`. Delete the `check(...)` call and pyc compiles
it. The `illegal: closure` line is a cascade: `self.append(x)` inside
`list.extend` is called with an untyped `x`. It is not a closure bug.

## Mechanism

1. `list.extend(other)` is `for x in other: self.append(x)`. The loop
   condition is `__list_iter__.__pyc_more__`, i.e.
   `self.position < len(self.thelist)`.
2. `r = []` is never written, so its element channel is bottom. In a loop
   over it, `position` is the constant 0 and `len` is the constant 0.
3. `0 < 0` goes through `int.__lt__`, and that contour is SHARED with every
   other `int < 0` in the program. `check`'s `a < 0` passes a non-constant
   `int64` as `self`, so the shared contour's `self` is `{int64, 0}` and it
   returns plain `bool`, not `False`.
4. The loop body is therefore live. `__next__` reads the bottom element,
   `x` is NOTYPE, and ifa/158 makes that fatal.

Without step 3, `0 < 0` folds to `False`, the body is dead, and nothing is
reported. That is why the small `tests/empty_container_elem`-style probes
pass: whether the program types depends on an unrelated comparison
elsewhere sharing the `__lt__` contour.

## How chull reaches it (ifa/074 owner lift, `PYC_CSOWNER=1`)

- The lift splits `Face` by its two `__new__` contours, for the demanded
  Edge `cs=1370`. This is a correct demand split.
- SETTER_OF_SETTER then splits `Face.InitEdges(self, fold=None)` by
  caller. The `fold=Face` contour prunes the `fold is None` branch, so it
  returns only the `newedges = []` literal (`list#1173`), which nothing
  writes.
- TYPE_CONFL gives `list.extend` a contour whose `other` is only
  `list#1173` (es=156, from `self.edges.extend(f1.InitEdges(f0))`, line
  284). Before the splits, lines 281 and 284 shared one `extend` contour
  with `other = {list#1173, list#1673}`, and `x` got `Edge` from the second
  list.
- Its `position < len(...)` shares `int.__lt__` es=560 with `seed`'s
  non-constant comparison. Steps 3 and 4 follow.

The `defs.n < 3` guard in `split_owner_of_demanded` (abe8b309) only hides
this by refusing the Face split. It should be removed when this is fixed.

## Fix

**The iterator tests emptiness with a comparison that keeps constants
apart.**

```python
  def __pyc_more__(self):          # __list_iter__, and __tuple_iter__ alike
    n = len(self.thelist)
    return n != 0 and self.position < n
```

`len` of a never-written list already folds to the constant 0. `int.__ne__`
wraps both operands in `__pyc_clone_constants__`, so `0 != 0` folds to
`False` in a contour of its own, however many other comparisons exist. The
`and` short-circuits, the loop body is dead, and the bottom element is
never read. This is the mechanism ifa/160 used: pyc already kills a
zero-trip loop whose bound folds to 0. The iterator's own test was just the
one comparison that could not fold.

`position` cannot be what carries the fact. Once the body is live,
`position += 1` (a shared `int.__iadd__`) widens it to `int64` for good. So
even demand-driven constant splitting (ifa/151 CONST_DEMAND) finds no
constant left to separate: both callers of `__lt__` pass `int64`. The loop
feeds itself, the same shape as ifa/171.

**Measured:**

- `make test` is green: 356 passed, 0 failed on both backends. The
  reproducer now passes and its `.known_issue` is removed.
- Corpus `-m check`, default arm, `9dfbf0fc+4690daca` →
  `cf0961f3+678941ba`: no verdict changes except plcfrs, which goes from a
  compile timeout to a fast compile error (the ifa/074 dict merge). `ess`
  rises by 1–2 on most programs, from the extra constant-cloned `__ne__`
  contours.
- `PYC_CSOWNER=1` with the ifa/074 `defs.n < 3` guard removed, `0536d85c`
  → `cf0961f3+678941ba`: no verdict changes. chull compiles, and its lift
  splits now apply (ess 416 → 488).

**Dead ends, measured:**

- Gating the continuation of every call on a non-bottom result, so that
  code after a call that yields no value is dead. It is sound in principle.
  But it needs codegen to terminate the dead tail: 12 tests failed with
  C functions falling off the end, e.g. `list.__str__` on `[]`. It also
  still leaves a raise-only exit, `__next__`'s return, typed bottom (the
  ifa/049 shape). That is three changes, not one.
- Gating only non-primitive calls: the `index_object` primitive read then
  still reports.

**Not covered:** iterators whose bound is a field rather than a folded
`len`: `__set_iter__`'s `_pos < _len`, `dict.__str__`'s
`while i < self._len`, `__str_iter__`'s `slen`. That is why
`tests/dict_empty_next_to_populated.py` stays KNOWN (ifa/160 family).

**Update 2026-10-01.** The same guard now covers `__range_iter__`
(`i0 != j`, where `i0` is the start, never written after `__init__`),
`__dict_iter__` and `__set_iter__` (`_len != 0 and ...`). A field-held
bound needs the guard rather than constant demand: the demand walk stops
at CreationSet fields, so it cannot reach `_len` the way it reaches
`n = len(self)`. Still uncovered: `dict.__str__`'s `while i < self._len`,
`__str_iter__`'s `slen`, and `__pyc_dict_from_iterable__`'s loop, so
`tests/dict_empty_next_to_populated.py` stays KNOWN.

## Side finding

`ifa_fa_inline` defaults to 1 (`ifa/common/fail.h:28`), even though the
comment at `fa.cc:13338` says "default off". So `analyze_to_convergence()`
runs a second time after mid-FA inlining, and that call's first iteration
skips `clear_results()` (`first_pass`). The final pass therefore starts
from the previous fixed point, not from bottom. This was measured on chull:
the loop condition in `extend` es=156 already carries its old type and
flow edge at the start of the post-inlining pass. It does not cause this
issue, because the pass before it already fails, but it contradicts
AGENTS.md's "every pass re-derives from bottom".

## Verification

1. The test prints `neg` and `1`, and its `.known_issue` flips to PASS.
2. With the `defs.n < 3` guard removed, `PYC_CSOWNER=1` chull compiles.
3. `tests/empty_container_elem.py` still reports its unguarded read.

## What this unblocks

The ifa/074 owner lift on chull without the guard, and so the lift as a
default. Any program where a split isolates a never-filled container
literal into its own contour is exposed to this.
