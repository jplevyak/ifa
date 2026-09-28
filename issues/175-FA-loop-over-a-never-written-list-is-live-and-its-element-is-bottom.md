# 175 — a loop over a never-written list is live, and its element is bottom

**Status: open.** Root-caused 2026-09-28 while chasing why the ifa/074
owner lift broke `shedskin_examples/chull`. The lift did not cause it. It
split contours correctly and exposed this pre-existing defect.

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

## Fix directions (undecided)

- **An empty element channel is a type fact.** A container CreationSet
  whose element channel is bottom has never held an element. A loop over it
  cannot iterate, and a read from it cannot execute. Deriving "this loop is
  dead" from the element type decides it on what the type IS, instead of
  hoping `0 < 0` folds through a contour shared with unrelated callers.
  Check that every insertion path writes the element AVar before relying
  on it.
- **Or:** report a NOTYPE whose only source is a read of a bottom element
  channel as unreachable (ifa/043 option 1, a codegen trap) rather than as
  a type violation. This is weaker: it gives up the precision above, and
  `tests/empty_container_elem.py` (an unguarded `x[0]` on `[]`) must stay
  an error.
- Splitting `int.__lt__` on constant vs non-constant `self` would also
  make it pass, but that would only move the dependence on contour
  sharing somewhere else.

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
