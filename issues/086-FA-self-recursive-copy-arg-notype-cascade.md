# 086 — a self-recursive function whose recursive call passes `arg.copy()` degrades entirely to NOTYPE and crashes at runtime

**Status:** open, found 2026-08-07 while diagnosing
[issues/025](../../issues/025-shedskin-examples-coverage.md)'s
`sudoku4` entry (TODO list item 9 — "never diagnosed beyond being
named in a bucket list"). `sudoku2` almost certainly hits the same
root cause (same Norvig-style recursive-solver-with-`.copy()`
structure, already grouped with sudoku4 in the doc's own "R3" bucket
guess, though not independently confirmed here).

**Affects:** core FA (self-recursive function type resolution) —
reproduces identically on both backends, confirmed backend-agnostic
(the crash happens inside the function body itself, a plain
`assert()`, not in either backend's own codegen-specific salvage
machinery).

## Repro (minimal, 3-line function body)

```python
def search(values):
    v2 = values.copy()
    return search(v2)

vals = {'a': 1}
print(search(vals))
```

Compiles with warnings (`illegal call argument type 'search' illegal:
search`, `expression has no type`, etc.) but produces a binary anyway
(exit 0, no diagnosed failure). Running it:

```
repro: repro.py.c:237: _CG_void_type _CG_f_9977_7(_CG_ps10580): Assertion `!"runtime error: getter not resolved"' failed.
```

A **terminating** (CPython-correct) version, confirming this isn't
about infinite recursion — matches CPython's actual output
(`{'a': 1}`) when it doesn't crash:

```python
def search(values, n):
    if n == 0:
        return values
    v2 = values.copy()
    return search(v2, n - 1)

vals = {'a': 1}
print(search(vals, 3))  # CPython: {'a': 1}; pyc: same assert, same crash
```

Confirmed **not container-specific**: substituting `[1, 2]`/`list` for
the dict reproduces identically (same warning count, same crash
shape). Confirmed **not about the `None`-guard/narrowing pattern** —
removing `if values is None: return None` / `if r: return r` (the
actual Norvig-solver shape sudoku4 uses) still reproduces with the
bare 3-line version above.

**Confirmed essential:** the recursive call must pass `.copy()`'s
*return value*, not the parameter directly. `search(values, n-1)` — no
`.copy()`, just passing `values` straight through — compiles with
**zero** warnings and runs correctly. It's specifically "recursive
argument is a method call on the (self-recursive) parameter" that
triggers it, not self-recursion or mutable-container arguments alone.

## Root cause (traced via generated C, not yet traced further into FA internals)

Generated C for the minimal repro's `search`:

```c
_CG_void_type _CG_f_9977_7/*search*/(_CG_ps10590 a1) {
  ...
  t1 = a1;
  t2 = t1;
  t3 = t2;
  assert(!"runtime error: getter not resolved");
  assert(!"runtime error: matching function not found");
  assert(!"runtime error: matching function not found");
  assert(!"runtime error: getter not resolved");
  assert(!"runtime error: matching function not found");
  if (t4) {
  return t0;
  } else {
  }
}
```

`search`'s own return type resolved to `_CG_void_type` (no value at
all — FA gave up entirely), and the `.copy()` call itself doesn't even
appear in the generated body (just three no-op `MOVE`s aliasing the
parameter) — codegen already knows this whole function body is dead
weight and just stacks unconditional salvage asserts. Since these run
unconditionally (no surrounding `if`), the function crashes on its
*first* call, every time, regardless of `n` or recursion depth — this
isn't a deep-recursion or fixed-point-divergence timeout, it's an
immediate, total failure to type the function body at all.

**Working hypothesis** (not confirmed by tracing FA itself, only by
the compiled-output symptom above): this looks like a genuine
self-recursion type-inference bootstrap problem. To type the
recursive call `search(v2)`, FA needs `v2`'s type — which comes from
`values.copy()` — which needs `values`'s type — which is `search`'s
own formal parameter type, the very thing currently being computed by
analyzing this call. When the recursive argument is the parameter
*itself* (no intervening method call), this cycle apparently resolves
fine (the zero-warning `search(values, n-1)` case above). Routing it
through `.copy()` — a call whose own return type is presumably
supposed to mirror the receiver's type — seems to break whatever
propagation makes the direct-pass-through case work, and the entire
function collapses to NOTYPE rather than converging on a fixed point
or degrading gracefully.

## Why this matters

This is a very common, idiomatic Python pattern — any recursive
algorithm that explores a mutable-state search tree by copying and
modifying local state before recursing (backtracking search, DFS over
partial assignments, etc.) uses exactly this shape. Both `sudoku2` and
`sudoku4` in `shedskin_examples/` hit it (classic Norvig sudoku
solver: `search(assign(values.copy(), s, d))`), and it's a plausible
root cause for other still-undiagnosed corpus examples with similar
backtracking-search structure. Unlike most of the corpus's other open
gaps (missing builtins, stdlib shims, dispatch precision), this isn't
a missing feature — it's a **compiler crash on valid, idiomatic
Python**, the same severity class as issue 081 (`int * bool`) before
it was fixed: no diagnostic tells the user what's wrong, and there's
no workaround short of restructuring the recursion to avoid `.copy()`
in the recursive argument position (not obvious to a user hitting
this cold).

## Verification plan

- Trace into FA itself (not just the generated-C symptom) to find
  where the self-recursive contour's fixed point actually fails —
  likely somewhere in the same contour-scheduling machinery several
  other open issues in this tracker already touch (`ifa/issues/057`'s
  non-convergence family, or the split/clone machinery `ifa/issues/033`
  and friends cover) — not established here which, if any, of those
  is the same underlying mechanism.
- `python3 repro.py` → `{'a': 1}` is the reference output for the
  terminating repro above; match that on both backends.
- Full `test_pyc.py` both backends — this is core FA, treat any new
  failure as a signal to narrow the fix, not special-case around it.
- Re-sweep `shedskin_examples/sudoku2` and `sudoku4` after a fix to
  confirm both actually solve puzzles (not just compile) and match
  CPython byte-for-byte.

## What this unblocks

`sudoku4` (confirmed) and very likely `sudoku2` (same structural
pattern, not independently verified) in the shedskin corpus, plus any
future program using the "copy mutable state, recurse, backtrack"
idiom — a common enough pattern that this is worth prioritizing over
narrower single-example gaps.
