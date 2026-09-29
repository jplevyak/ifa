# 025 — branch correlation is not a discriminator: correlated arms over a CLASS union are refused

**Status:** open. **Now a REFUSAL, not a warning** (re-verified
2026-09-28): since closed/158 made every violation fatal, the fixture
below no longer compiles, and CPython runs it.
Rewritten 2026-09-28; history in git:
`git show 3f36072b:ifa/issues/025-FA-intra-function-union-narrowing.md`.

## What works

- `is None` / `is not None` narrowing on a class-or-None union, end to
  end on both backends (`tests/recursive_list_is_none.py`). `Code_IF`
  applies `flow_var_type_permit` to the per-branch SSU view that
  `peel_wrapper_def` finds under the `__pyc_to_bool__` wrapper.
- `isinstance(a, Dog)` over a union of user classes returns the right
  bool (`tests/isinstance_union.py`). The frontend lowers a direct
  2-argument `isinstance` to a per-site `prim_isinstance` send, so no
  shared wrapper clone exists to mis-fold.
- A user helper `def is_end(n): return n is None` narrows too.
  Clone-time specialization carries it across the call.

The scalar-union cases originally filed here (`x` is `int` on one branch
and `str` on the other, then used) are not narrowing problems. A raw
scalar union has no representation, and they are refused
(closed issues/018).

## What is open

`tests/branch_correlation_class_union.py` (`.known_issue`):

```python
if flag: a = Dog()
else:    a = Cat()
if flag: print(a.bark())    # warning: illegal call argument type ... Cat
else:    print(a.meow())    # warning: illegal call argument type ... Dog
```

CPython runs it. pyc used to warn twice and then run correctly (classtag
dispatch resolves at run time). Since closed/158 the two violations are
fatal (`error: illegal call argument type expression illegal: Cat`, and
the same for `Dog`), so a valid program is refused.

**Mechanism needed:** when the same Var (`flag`) is tested as an earlier
branch condition, the second test's arms imply the types the first test's
arms assigned. That is path correlation. A MUST/MAY dataflow cannot
represent it, and neither can per-contour splitting, since there is one
contour.

## Principled options

1. **Tail duplication at the second test** (the classic fix): when a phi's
   operands are each defined under a condition that a later branch
   re-tests on the same SSU value, duplicate the join-to-test region per
   predecessor. Each copy then sees one operand. This is a CFG
   transformation before FA, so FA's lattice stays monotone.
2. **Permissive fallback:** a union member with no candidate for the
   method (`Cat` has no `bark`) becomes a runtime check that raises
   `AttributeError`, as CPython would if that path were ever taken.
   `--strict` keeps the refusal. This is the permissive/strict split of
   [165](165-none-reaching-an-operation-is-silently-accepted.md), and the
   check is the one [079](079-DISPATCH-single-candidate-dispatch-unchecked-cast.md)
   is missing. It makes the program run, but it does not narrow, so
   option 1 remains the precise fix.

Do not add a correlation-tracking lattice element. It is a relational
domain grafted onto a non-relational analysis.

## Verification

The fixture flips KNOWN → PASS (no warnings), and the two working
fixtures above keep passing.
