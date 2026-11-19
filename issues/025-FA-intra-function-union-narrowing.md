# 025 — narrowing: branch correlation over a class union, and composed predicates

**Status: open.** Rewritten 2026-11-03; it carries closed/079's permissive
remedy (2026-10-16) and [closed/192](closed/192-review-analysis-and-issues-2026-11.md)
B3. History in git (`git show 123ac81:ifa/issues/025-FA-intra-function-union-narrowing.md`).

## What works

`is None` / `is not None` and `isinstance(a, Cls)` narrowing on the
per-branch SSU view (`Code_IF` → `flow_var_permit_pred`), through a user
helper too (`tests/recursive_list_is_none.py`, `tests/isinstance_union.py`).
Scalar unions are not a narrowing problem; they are refused (closed
issues/018).

## 1. Branch correlation (refusal of a valid program)

`tests/branch_correlation_class_union.py` (`.known_issue`):

```python
if flag: a = Dog()
else:    a = Cat()
if flag: print(a.bark())    # error: illegal call argument type ... Cat
else:    print(a.meow())
```

CPython runs it; since closed/158 made violations fatal, pyc refuses it.
The second test's arms imply the types the first test's arms assigned,
which a MUST/MAY dataflow cannot represent and one contour cannot split.

Options, in order:

1. **Tail duplication at the second test** (the precise fix): when a phi's
   operands are each defined under a condition that a later branch
   re-tests on the same SSU value, duplicate the join-to-test region per
   predecessor. A CFG transformation before FA; the lattice stays
   monotone.
2. **Permissive fallback** (closed/079's remedy): a union member with no
   candidate for the method becomes a tag test with a final `else`
   raising `AttributeError`, as CPython would on that path; `--strict`
   keeps the refusal. It runs the program but does not narrow.

Do not add a correlation-tracking lattice element: a relational domain
grafted onto a non-relational analysis.

## 2. Composed predicates (precision loss)

`flow_var_permit_pred` drops a second distinct predicate on the same AVar
("composition not implemented"), so `if x is not None and isinstance(x,
Foo):` narrows on the first test only. Sound, and the commonest
typed-Python idiom. Fix: a predicate list on `AVar`, evaluated in
`apply_restrict_pred` as a conjunction.

## Verification

The fixture flips KNOWN → PASS with no warnings; closed/079's probe
(`x = A() if c else B(); if c: print(x.m())`) prints `done` in permissive
mode and is refused under `--strict`; a fixture for the composed predicate
shows the narrowed type on both backends.
