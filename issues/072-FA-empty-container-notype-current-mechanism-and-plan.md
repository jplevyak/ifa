# 072 — a never-written container's bottom element reaches live code

**Status:** open, not attempted. Rewritten 2026-09-28; re-checked
2026-10-17 (both fixtures below still fail). It owns two `.known_issue`
fixtures. The history (shedskin's backward pass, the negative seeding
prototype, the 043 family) is in git:
`git show 3f36072b:ifa/issues/072-FA-empty-container-notype-current-mechanism-and-plan.md`.

## The principle

A container that is allocated and never written has a BOTTOM element on
its contour. That is not missing information. It is a precise fact: there
are no elements. Every operation that would produce an element either
never runs (a loop over it iterates zero times) or raises (`x[0]` is an
`IndexError`). **Code reached only through such an element is dead**, and
a bottom value in dead code is not a violation. That is the same rule as
[049](closed/049-FA-raise-only-contour-notype.md): an exit that produces no value
must not be read as one.

**Seeding a default element is the wrong answer, and it was measured
2026-07-28:** a fixed default regresses the operations that already
handle a bottom element, and shedskin does not do it either (its
inference is write-driven).

## What already works

- Forward, write-driven inference types every valid shape: `if lst:
  lst[0]`, `for x in lst`, `sum(lst)`, a `len(x) > 0`-guarded read.
- closed/175: a `for` over a never-written list folds dead, because
  `__pyc_more__` tests `n != 0 and position < n`, and `len` of it folds
  to 0. (This depends on `int.__ne__`'s constant annotation,
  [134](134-remove-the-frontend-forced-split-opt-in.md).)
- closed/160: `list.__eq__` against an empty literal.

## Open fixtures

| fixture | shape |
| --- | --- |
| `tests/empty_container_elem.py` | `x = []; x[0]`: a mutual error (CPython raises `IndexError`). Today it is a NOTYPE refusal. |
| `tests/dict_empty_next_to_populated.py` | `dict([])` beside a populated `dict([(1, 2), (3, 4)])`. Once closed/172 separates their CSs, the empty one's code is type-checked as live over a bottom element: `dict.__str__`'s `while i < self._len` with `_len` 0, and the pairs loop over `[]`. Since issues/178 (`dict(x)` dispatches `x.__pyc_dict_into__(d)`) it is reported as `illegal call argument type ... int64` and `unresolved member '__getitem__' of class 'int64'`; before, as `'pair' has no type`. Measured 2026-10-17: `dict([])` alone, the populated `dict(...)` alone, and `{}` beside the populated `dict(...)` all compile and match CPython -- only the two `dict(...)` calls together fail. |

Its `.known_issue` text says "(was 160)"; it belongs here.
(`tests/empty_list_compare_via_dict.py`, a list read out of a dict
compared to `[]`, was a third fixture; it compiles and matches CPython on
both backends as of 2026-10-14, not bisected, and its `.known_issue` is
gone.)

Also in this family: **a METHOD DISPATCHED on the element** of a
never-written container inside the builtin `set`/`dict` (`item.__hash__()`
in `set.update`). A read-side codegen trap does not cover a dispatch. It
must be dead, not trapped.

## The fix

Make "this loop runs zero times" and "this index raises" facts that FA
DERIVES from a bottom element, instead of relying on each builtin's loop
test happening to fold:

1. **Loops:** the iterator's `__pyc_more__` over a container whose element
   AType is bottom folds to `False`. That is 175's mechanism, but keyed on
   the element being bottom rather than on `len` folding through one
   annotated comparison. `dict`'s `while i < self._len` needs the same
   fact through `_len`.
2. **Reads:** an `index_object` / element load whose container's element
   is bottom is an exceptional exit. Its result is not a value, so its
   uses are dead (049's rule), and codegen emits the raise.

The fact is stable across passes only once the container's writers have
converged. A container that is empty in pass 0 and written later must not
keep its loops dead. That makes this a decision taken at convergence, like
[170](170-FA-contours-minted-on-transient-types-are-never-remerged.md): a
dead edge is recomputed every pass, never only accumulated.

## Verification

The two fixtures flip to PASS (`empty_container_elem` as a clean
runtime `IndexError`, matching CPython). Nothing that types today
regresses. Corpus `check` neutral or better.
