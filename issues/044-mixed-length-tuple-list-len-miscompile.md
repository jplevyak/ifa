# 044 — Mixed-length list literals in one container: len over the union miscompiles (silent wrong output)

**Status:** open, found 2026-07-15 while prototyping issue 043's
option 4 (the repro has nothing to do with empty containers).
**Affects:** `_CG_prim_tuple_list` concretization + `P_prim_len` over
a union of tuple-list CreationSets (`ifa/analysis/fa.cc` len
transfer, `ifa/codegen/cg.cc` tuple-list emission).
**Related:** issue 025's tuple-list soundness fix (the
sizeof-element-0 bug — same representation, different failure),
043 (where this was found).

## Symptom

```python
ls = [[3], [1, 2]]
print(ls)
```

pyc (both the compile and the run are diagnostic-free):

```
[[3, 0], [1, 2, 0]]
```

CPython: `[[3], [1, 2]]`. Two extra phantom elements, value 0 —
silent wrong output, no compile-time or runtime error.

## What's known

- Inner list literals with statically-known lengths concretize as
  `_CG_prim_tuple_list` fixed-field structs (no runtime length).
  `len()` on such a CS is a compile-time constant (its `vars.n`).
- When DIFFERENT-length tuple-lists flow into one container slot,
  reads yield the union; `list.__str__`'s loop bound `len(self)`
  over the union is no longer a per-CS constant. The printed output
  suggests both inner lists iterate to the MAX length (2 for `[3]`
  is wrong already; `[1, 2]` printing 3 elements suggests len
  resolved to 3 = 1 + 2? or the union'd size type produced garbage)
  and out-of-bounds slots read as 0.
- The `0` values are reads past the fixed struct's fields —
  uninitialized/adjacent memory that happens to be zero here; this
  could equally read garbage.

## Sensitivity note (2026-07-15, post issue-045 landing)

The wrong output's exact shape is contour-sensitive: after the
receiver-CS cloning work (045), the empty-plus-concrete variant
`ls = [[], [1, 2]]; print(ls)` shifted from `[[], [1, 2, 0]]` to
`[[0], [1, 2, 0]]` — still wrong, same mechanism (len over the union
of different-vars.n tuple-list CSs), but the empty inner list now
also prints a phantom element. Any change to how the inner lists'
CSs group across contours can move the symptom; don't treat a shift
in the garbage as progress or regression without checking the len
resolution itself.

## Repro variations to check when picking this up

- `[[3], [1, 2]]` printed (above) — wrong.
- Same but iterated (`for inner in ls: for x in inner: print(x)`) —
  check whether iteration uses the same broken len.
- Mixed lengths where one is append-grown (a real `_CG_list` with a
  runtime len) vs a literal — check which representation the union
  concretizes to.

## Possible directions (not investigated)

- FA: `P_prim_len` over a union of tuple-list CSs with differing
  `vars.n` must not produce a usable constant; it should force the
  union to concretize as a REAL `_CG_list` (runtime length) instead,
  or force a clone split by length class.
- Codegen: a union of different-length tuple-lists cannot share one
  fixed-field struct; today's emission appears to pick one layout
  and read through it for both.

## Impact

Any list-of-list-literals with differing inner lengths — a common
shape (lookup tables, adjacency lists) — silently produces wrong
values. Worth prioritizing above further issue 025 bucket work:
silent wrong output beats compile failures in badness.
