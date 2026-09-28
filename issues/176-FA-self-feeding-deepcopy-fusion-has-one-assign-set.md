# 176 — a deep copy fed back into its own source fuses outer and inner copies, and the fusion hides the partition

**Status: open.** Split out of
[168](closed/168-FA-recursive-overlap-gate-refuses-its-own-split.md) on
2026-09-28, when 168's gate landed and fixed everything except this shape.

## Symptom

```python
import copy
M = [[1.0], [2.0]]
M = copy.deepcopy(M)
print(M[0][0])
```

```
error: illegal call argument type expression illegal: float64
```

CPython prints `1.0`. The recursive-function versions (`total(shrink(M))`,
linalg's `determinant(Minor(M, ...))`) type and converge. What they lack
is the copy flowing back into the SAME global the original came from.

## Mechanism

Measured with `IFA_DBG_FUNES=__deepcopy__`, `IFA_DBG_CSDEFSPLIT`, and
`IFA_DBG_FUNES=append`:

1. `list.__deepcopy__` has an outer contour (es=48, called from `deepcopy`)
   and an inner one (es=79, split from it). Under parent-first routing
   ([074](closed/074-FA-cross-pass-oscillation-plan.md) section 0), es=79's
   `r = []` joins es=48's CreationSet. So `cs=1078` holds both copies, and
   its element is `{float64, list#1078}`.
2. `M = copy.deepcopy(M)` puts the copy back into `M`, so es=48's `self` is
   `{original, list#1078}`. Its `self[k]` is then `{inner original, float64,
   list#1078}`, and es=79 sees the same union.
3. Both levels' `r.append(...)` therefore write `{float64, list#1078}`.
   `append` has one contour (es=71) with two in-edges carrying IDENTICAL
   types.
4. CS_DEF_PART sees the demand on `cs=1078`: its element is irrepresentable
   and it has 2 creation points in different contours. But the flow graph
   has ONE assign-set key, so every creation point gets the same signature:
   `DECLINED (1 group)`. `find_blocking_es` cannot help. With one key, no
   contour held terminal separates the two defs.
5. 168's gate does not fire. `cs=1078` has two live creation points, not
   one.

The fusion produces the evidence that keeps it fused. Each level writes the
union because the other level's copy is in the same CreationSet.

## Direction (not built)

The demand is real (an irrepresentable element) and the creation points
are distinguishable (two contours of one function). AGENTS.md's refinement
allows the handle when the demand decides WHETHER: split `cs=1078` by
creation point exactly when it is demanded, has several creation points,
and the assign-set partition is uninformative. That is route 4 limited to
this case.

**The measured risk:** ifa/157's `PYC_CSFAN` did this broadly and was
deleted. On `sudoku4` the fanned contours re-merged through a shared
container-method contour (`append` over six receiver CreationSets). Here,
after the split, `append`'s two edges would carry different receivers.
TYPE_CONFLUENCE should then separate them, and the writes would diverge
(outer: the inner copy; inner: `float64`). That has to be MEASURED, not
assumed.

**Stop condition:** if the split re-merges on the next pass (element still
`{float64, list}` on both halves), the partition is not the missing piece.
The receiver union at `append` is, and that is ifa/157's cascade.

## Verification

- The 4-line program prints `1.0`.
- `tests/deepcopy_recursive_nested_growth.py` and linalg keep passing.
- Corpus `-m check`: no regression, and sudoku4 must not regress. It is
  the program the broad version broke.
