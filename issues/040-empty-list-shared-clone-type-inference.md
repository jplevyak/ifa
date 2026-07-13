# 040 — An empty list literal sharing a method clone with a non-empty, differently-element-typed list fails to type-check

**Status:** open, filed for triage (found while landing pyc's
issue 024, extended iterable unpacking — not investigated further,
not fixed).
**Affects:** FA type inference for `list` methods (`__str__`,
`__getitem__` confirmed; likely any method whose body indexes/
iterates `self`) when the SAME method clone serves both an empty
list literal and a non-empty list of some other, non-generic
element type.

## Symptom

```python
b = [2, 3]
print(b)

k = []
print(k)
```

Compiles with `expression has no type` errors pointing into
`__pyc__/04_sequence.py`'s `list.__getitem__` (`index_object`) and
`list.__str__` (`self[k].__repr__()`), then fails to build the
generated C.

Minimal-ish: needs BOTH a non-empty list of some concrete element
type AND an empty list literal `[]`, both reaching `print()` (or
presumably any other method needing to know the list's element
type) in the same program. Neither alone fails:

```python
k = []
print(k)          # fine alone -- prints "[]"
```

```python
b = [2, 3]
print(b)           # fine alone
```

only combined do they fail.

## Hypothesis (not investigated)

`list` is not a compile-time-generic class in pyc's type system --
element type is tracked per allocation/creation site via FA's
CreationSet machinery, not via a class-level type parameter. A
method like `list.__str__`/`list.__getitem__` presumably gets ONE
shared clone per receiver-type-equivalence-class rather than one
per distinct element type, and an empty list literal's element type
is (reasonably) unresolved/bottom at its own creation site. When FA
has to build ONE clone serving both a receiver with a concrete
element type and a receiver with an unresolved one, something about
unifying `self[i]`'s result type across both apparently fails
outright rather than falling back to (or just not needing) a valid
type for the never-actually-indexed empty case.

Not verified with a debugger or `PYC_DBG_DISPATCH`-style tracing --
this is a plausible-shaped guess based on the symptom, not a
confirmed root cause.

## How this was found

Landing issue 024 (extended iterable unpacking, `a, *b = [1, 2, 3]`)
built each star target's value via a runtime loop appending into a
freshly-constructed empty list. A test combining a star-unpack that
happens to consume zero elements (`j, *k = [1]`, where the star's
range is empty) with an EARLIER star-unpack that consumes a
non-empty, int-element range hit this exact failure. Isolating
further (see the symptom above) showed it's unrelated to unpacking
at all -- a plain empty-list literal reproduces it identically.
Issue 024's own tests avoid the combination (every star target in
`tests/star_unpack.py` ends up non-empty); the always-empty-star
case (`j, *k = [1]`) is verified working correctly in isolation only
(no committed test for it, since the moment it shares a program
with any other list of the same element type, it may hit this bug).

## Reproducer

```python
b = [2, 3]
print(b)

k = []
print(k)
```

Expected (CPython): `[2, 3]` then `[]`.

## Verification plan

1. Confirm the hypothesis with `PYC_DBG_DISPATCH`-style tracing or a
   debugger: which clone of `list.__getitem__`/`list.__str__` is
   shared between `b` and `k`, and what type does `self[i]` end up
   with in that clone.
2. Land a fix.
3. Add the reproducer above as a test; also un-skip issue 024's
   always-empty-star case as a real committed test once this no
   longer poisons a shared clone.

## What this unblocks

- Any program mixing an empty list literal with a non-empty list of
  some concrete element type reaching the same method clone --
  plausibly common (e.g. an accumulator initialized `[]` then later
  compared/printed alongside a literal list of the same nominal
  element type elsewhere in the program).
- Issue 024's always-empty extended-unpacking case
  (`j, *k = [1]`, `a, *b, c = [1, 2]`) as a fully-committed,
  unconditionally-safe test rather than one that only works in
  program-level isolation.
