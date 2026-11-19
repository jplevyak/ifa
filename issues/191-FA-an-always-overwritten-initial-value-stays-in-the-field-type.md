# 191 — a placeholder that is always overwritten before any read: one row left

**Status: open on row 5.** Direction 2 landed 2026-10-17
(`ifa/analysis/overwrite.cc`) and fixed rows 2–4 and three issues/048
fixtures. Rewritten 2026-11-03; the census, the probe's hazard list and
the landing record are in git
(`git show 123ac81:ifa/issues/191-FA-an-always-overwritten-initial-value-stays-in-the-field-type.md`).

## What is fixed

A `None` store into a field whose type also holds a number is recorded in
a decision ledger when, at the end of a pass, EVERY contour of its
function walks `NEVER` (no read of the field, and no primitive that may
read fields, is reachable on any path, into callees and out to callers,
before a store overwrites it). A recorded store does not flow its value
into the field; a recorded store that later becomes observable fails the
compile naming it. `PYC_OVERWRITE=0` turns it off. Corpus: identical in
every column; the suite flipped six known issues. The nine hazard programs
(a read in `__init__`, in a called method, by the caller, through an
alias, in a loop, in an `except` after a raise, through `copy.copy`, a
generator yielding the object) are still refused, correctly, and two
fixtures pin both sides (`field_none_read_before_overwrite`,
`field_none_overwritten_two_objects`).

## What is open: row 5

```python
xs = [None] * n
for i in range(n): xs[i] = i * 2
print(xs[1])            # CPython: 2.   pyc: refused, {None, int64}
```

`tests/list_none_prealloc_scalar_overwritten.py` (`.known_issue`). This is
not a field store, so the walk above does not see it. It needs a
loop-coverage fact: `[None] * n` followed by a loop that stores every index
in `range(n)` before any read. Check whether closed/164's machinery (the
same idiom for pointer elements, where None is a null) has the coverage
half already.

## Rules that bound any fix

- **Never retract inside the fixed point.** Decide before it (a pre-FA
  IF1 fact) or after convergence as a ledgered decision re-checked every
  later pass. Retraction mid-pass is non-monotone (closed/057, closed/078).
- **Not a split.** The union is temporal (one object, None then int); no
  contour separates it.
- **Not a representation.** issues/048's sentinel is permissive-only; this
  issue asks for the CPython-exact answer, in every mode.

## Stop condition

If the re-run from bottom does not reach the same fixed point twice (the
decision oscillates), it is being taken on transient types
([170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)).
Stop; do not add a guard.

## Verification

The row-5 fixture flips to PASS on both backends; a variant that reads
`xs[0]` before the loop is still refused; corpus `check` unchanged.
