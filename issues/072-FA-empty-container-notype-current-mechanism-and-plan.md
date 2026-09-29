# 072 — a never-written container's bottom element reaches live code

**Status:** open. Rewritten 2026-09-28. It owns three `.known_issue`
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
[049](049-FA-raise-only-contour-notype.md): an exit that produces no value
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
| `tests/dict_empty_next_to_populated.py` | `dict([])` beside a populated dict. Once closed/172 separates their CSs, the empty one's `for pair in pairs` and `while i < self._len` are type-checked as live over a bottom element. |
| `tests/empty_list_compare_via_dict.py` | a list read out of a dict compared to `[]`. `l[i]` on the empty literal is bottom. |

The last two point at closed/160 in their `.known_issue` text. They
belong here.

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

The three fixtures flip to PASS (`empty_container_elem` as a clean
runtime `IndexError`, matching CPython). Nothing that types today
regresses. Corpus `check` neutral or better.
