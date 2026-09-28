# 176 — a deep copy fed back into its own source fuses outer and inner copies, and the fusion hides the partition

**Status: closed 2026-09-28** (see *RESOLVED*). Split out of
[168](168-FA-recursive-overlap-gate-refuses-its-own-split.md) on
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
   ([074](074-FA-cross-pass-oscillation-plan.md) section 0), es=79's
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

## Direction (built as below)

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

## RESOLVED 2026-09-28

Built as described, as `PYC_CSJOINSPLIT` (default 1), in
`split_css_by_defs`' one-group branch, after the owner lift and `esblock`
have declined. When a demanded CreationSet (irrepresentable element) has
several creation points that are ALL the same allocation site (one `Var`),
each in a different contour, split it by contour, one CreationSet per
contour. That is exactly the join parent-first makes. It is narrower than
ifa/157's deleted `PYC_CSFAN`, which fanned any demanded CS, including
unrelated sites (26 on sudoku4).

**The stop condition did not trigger.** The split fires twice on the
4-line program (p=2, then one level deeper at p=4). Nothing re-merges: the
split `append` edges carry different receivers, and the writes diverge.

| | before | after |
|---|---|---|
| 4-line program | 1 error | **0 errors**, prints `1.0`, `CONVERGED=1` at p=6 |
| `tests/deepcopy_recursive_nested_growth.py` | p=16, ess 120 | p=11, ess 115 |
| `linalg` | p=61, ess 789, 24 s compile | **p=22, ess 548, 12 s**, still matches CPython |

`make test` is green: 358/0 on both backends, with
`tests/deepcopy_reassign_self.py` added. Corpus `-m check`,
`check__default__4ab070d7+a0ef1f9f` → `check__default__4b61e721+d7af0afe`:
no verdict regresses, and **sudoku4 is identical** (10 errors, 10 s). The
only verdict move is tonyjpegdecoder, on the 120 s run cap. Container CSs
+26 corpus-wide (2096 → 2122) against ess −108. The split fires on demanded
CreationSets in sunfish (86 → 94), sudoku5 (55 → 59), solitaire (25 → 29),
rdb and plcfrs (+4 each), with every one of their verdicts unchanged.

