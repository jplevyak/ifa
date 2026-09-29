# 049 — a function whose only reached branch raises has a bottom return, and that is now a hard error

**Status:** open. Rewritten 2026-09-28; history (two reverted prototypes
and the sudoku2 dig) in git:
`git show 3f36072b:ifa/issues/049-FA-raise-only-contour-notype.md`.
**Severity went UP:** it used to be a spurious warning on a program that
ran correctly. Since closed/158 made every violation fatal, it refuses a
valid program.

## Repro (re-verified 2026-09-28)

```python
def risky(n):
    if n > 5:
        raise ValueError("too big")
    return n

print(risky(9))
```

```
r049.py:1: error: expression has no type
  called from r049.py:6
r049.py:6:57: error: expression has no type
```

No binary. CPython raises `ValueError: too big`. Add any call that reaches
the normal `return` (`print(risky(3))`) and the program types.

## Mechanism (pinned 2026-08-06)

`PY_raise_stmt` routes the raise to `goto_exc_target` without a move into
`fn->ret`. That is deliberate: the exceptional path must not add a type
arm to the return. When every contour of `risky` reaches only the raising
arm, `fn->ret` has no reaching definition, and it is bottom.

**The violating AVar is `risky`'s OWN return var**, made `live_arg` by
`risky`'s own unconditional `reply`, which reads `fn->ret` whichever edge
reached it. No caller's control flow is involved. So pruning the caller
(making its use of the result unreachable) changes nothing. Measured:
identical output with and without it.

## The principled fix

**The exceptional exit does not produce a value, so it must not read
one.** A function has two exits: the normal return reads `fn->ret`, and
the exceptional exit reads only the pending exception. The single
`reply` conflates them. Once the raise edge reaches an exit that does not
read `fn->ret`, a raise-only contour's `fn->ret` is not live, it is not a
violation, and nothing needs suppressing. This is the same rule
closed/175 applied to a dead loop: bottom in code no live path reads is
not an error.

Two things this rules out, both built and reverted in 2026-08:

- **A placeholder value on the raise edge** (an opaque `any`-typed move):
  it unions into sibling contours' real return types. `risky(2)` printed
  `<instance>`, and one shape crashed `dom.cc`. A value that must not
  exist cannot be approximated by one that does.
- **Suppressing the NOTYPE report** (`notype_violation_is_benign`): the
  AVar stays bottom, codegen inherits a type it cannot emit, and the repro
  went from compiling to failing in C. Suppression without removing the
  liveness is a retreat.

## Verification

- The repro compiles, runs, and prints CPython's error.
- `print(risky(3)); print(risky(9))` and the `try: risky(9) except
  ValueError: …` then `risky(2)` shapes still print the real values
  (`2`, not `<instance>`). These are the shapes that broke both
  prototypes.
- `tests/exception_propagation.py` passes. Add a fixture that calls a
  can-raise function ONLY with a raising argument.
- Six gates green.
