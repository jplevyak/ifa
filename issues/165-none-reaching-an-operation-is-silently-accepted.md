# 165 — a `None` that does arrive at an operation: two residuals

**Status: largely fixed 2026-09-28; open on two residuals.** Rewritten
2026-11-03; the symptom, root cause and the 164/165 distinction are in git
(`git show 123ac81:ifa/issues/165-none-reaching-an-operation-is-silently-accepted.md`).

## What is fixed

A `{None, T*}` receiver is tested where the call or attribute read is
emitted, on both backends (`nil_receiver_rval`, `nil_period_receiver`;
`_CG_none_receiver` prints CPython's message). `_CG_strcat` checks its
operands. A `{None, scalar}` receiver has no null to test and is refused
as BOXING. The inliner and DCE keep such a call and its receiver read
(closed/184). `tests/none_receiver_raises.py`.

## Residual 1: the check exits instead of raising

The emitted check reports and exits: it is CPython's *uncaught*
`TypeError`, and `except TypeError:` cannot catch it. The fix routes the
check through `__pyc_exc__`, which means arming `pyc_program_has_raise`
for every call that has such a check. This is the same change as
threading the pending exception along the propagation path
([issues/172](../../issues/172-stopiteration-value-reads-the-global-exception-slot.md),
closed/190 item 3), and should land with it.

## Residual 2: argument positions other than string concatenation

Only `_CG_strcat` checks an argument for null. A `{None, T*}` value in any
other primitive argument position is read as the zero value. Enumerate
the primitives that take pointer arguments (`PRIMITIVES.md`) and emit the
same test at each; measure first how many survive `RP_IsNotNilType`
narrowing, since most nullable unions in the corpus sit under an explicit
`is None` guard.

## Verification

The repro raises CPython's `TypeError` and is catchable; `tests/nil_union_prealloc.py`
still costs nothing observable; corpus: richards, life and sokoban (the
most nullable fields) do not regress from match to run-fail.
