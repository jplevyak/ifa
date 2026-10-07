# 049 — a function whose only reached branch raises has a bottom return, and that is now a hard error

**Status: FIXED 2026-10-06** (see "The fix" below). Rewritten 2026-09-28;
history (two reverted prototypes and the sudoku2 dig) in git:
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

## The fix (2026-10-06)

The principled fix above, plus the caller-side half it implied. msp_ss
found that half: once `pyc_lib/serial.py` raises honestly, the errors are
on the CALLERS' variables (`hdr = self.serialport.read(1)`), in code after
a call that can only raise.

- **Frontend** (`python_ifa_build_if1.cc`). `goto_exc_target` marks a goto
  to the function's exit taken with an exception pending as `exc_exit`
  (`Code` bit; not for generators or coroutines). `emit_exc_check` marks
  its IF as `exc_check`.
- **The exceptional exit produces no value.** The walk does not follow an
  `exc_exit` goto: its path to the exit is made live, because codegen
  emits it, but it is not walked, so the exit's `reply` does not read
  `fn->ret`. Two new per-contour bits separate what `live_pnodes` had
  conflated. `returns` means the exit was reached along a normal path
  (`es_reaches_exit` now asks it). `raises` means an `exc_exit` was walked.
- **A call to a raise-only callee completes, exceptionally.** `gate_send`
  is now `send_gate_kind`:
  - GATE_NORETURN: the callee neither returns nor raises. Trap, as ifa/178.
  - GATE_RAISES: it raises and no exception check follows (builtin-module
    code does not check). The walk stops, the caller contour raises too,
    and codegen returns at once, so the caller's check sees the pending
    exception (`fa_noreturn_raises`, both backends).
  - not gated: it raises and IS checked.
- **At quiescence** (`settle_gated_liveness`). The exception check after a
  call that never returns keeps only its pending branch, so a handler stays
  reachable and the code using the result does not. `returns` and `raises`
  are re-derived from that walk, iterated to a fixed point. Contours
  reachable only from a send the cut removed are dropped from the pass's
  done set. During the walk the check is usually reached before the call's
  edge exists, so only the converged graph can answer it: the
  quiescence rule.
- **Consumers.** `clone.cc` builds `Fun::calls` only from sends the contour
  reaches; an edge walked before a cut had made DCE keep the dead send.
  The inliner's single-send and prim-chain matchers refuse a function that
  cannot return (`fun_can_return`, asked of FA, not of the reply, which DCE
  kills whenever a result is unused); inlining it would drop the raise.
  `cg.cc` traps in an exception check's pruned branch instead of emitting
  its dead body, which fell off the end of the function (C++ UB,
  `-Wreturn-type`).

Neither reverted prototype came back: no placeholder value is placed on
the raise edge, and no report is suppressed.

**Verified.** The repro prints `Unhandled exception: too big`. Both shapes
from the verification list print the real values (`3`, `2`):
`print(risky(3)); print(risky(9))`, and `try: risky(9) except ValueError`
then `risky(2)`. Regression tests: `tests/raise_only_uncaught.py` (the
repro) and `tests/raise_only_call_result.py`. The latter is a raise-only
function whose result is used two calls up, caught there, which is msp_ss's
shape. `make test`: 407 passed / 0 failed on both backends. Check sweep
`bb950ad3+70b8fb2a` against `7ef9bdfe+ea3bf1ef`:
- msp_ss and voronoi2 now compile (voronoi2's `_match_long_opt`, issues/025);
- no other status changes (sieve's NO is its timing lines);
- contour counts move by a few (mandelbrot2 +8 ess), and fall where dead
  code stopped being analysed (msp_ss 879 -> 560 ess, rubik 681 -> 629).

One mid-way state is worth knowing, because it regressed pygmy to a
`matching function not found` abort. Filtering `Fun::calls` by "the callee
contour survived the pass" dropped real calls: some live sends' edges point
at contours outside the final pass's done set. Filtering by the sending
node's liveness alone is the correct test.
