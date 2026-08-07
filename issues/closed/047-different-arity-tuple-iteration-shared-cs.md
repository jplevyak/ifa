# 047 — Iterating two different-arity tuples in one program: shared __tuple_iter__ CS miscompiles (segfault)

**Status: CLOSED — FIXED (confirmed 2026-08-06, archived same day).**
Found 2026-07-16 during the genetic2 dig (pyc
issues/025); pre-existing at user level, NOT caused by the dynamic
`tuple()` intercept (which merely exercised it via
`tuple.__pyc_tolist__`'s original iteration loop — since switched
to an index loop to sidestep this).

Re-tested 2026-08-06 (prompted by a direct question: "different
tuples should split into different CSs — this should split the
iterator"): the 4-line repro below, plus a stress-tested expansion
(three arities, same-arity-different-element-type, tuples of varying
shape passed through one shared function) now compile and run
correctly on **both backends**, matching CPython. Confirmed via the
generated C that the fix matches the hypothesis exactly: each loop's
`tuple.__iter__()` call now resolves to its own CreationSet, whose
`__pyc_more__`/`__next__` clones are called **directly** rather than
through the shared prototype's `__pyc_tag`-dispatched method slot —
the mechanism section below (the segfault's root cause) no longer
applies. The vestigial shared-prototype `___init___` is still
emitted (presumably reachable from some other polymorphic-fallback
shape) but neither loop's runtime path goes through it anymore.

Confirmed **pre-existing at HEAD when this fix was verified** — not
caused by this session's issue-030 dispatch work: built at the
pre-session baseline commit (`5bec10ac`, via a disposable
`git worktree`) and the repro was already fixed there too. No commit
in the intervening window mentions "047" or "tuple_iter" directly, so
this reads as an incidental fix from the broader per-creation-site
CS-splitting work landed earlier in the session (the 026/045/066/073/
076–078 cluster) — never separately verified against this exact
repro until now. None of the three "Fix directions" below were
deliberately implemented as such.

New regression test: `tests/tuple_iter_mixed_arity.py` (`.exec.check`
verifies the full stress-tested output against CPython). Verified:
`test_pyc.py` 251/0 (was 250/0, new test included), `PYC_FLAGS=-b
test_pyc.py` 251/0, `tests/for_over_tuple.py` still passes on both
backends.

## Repro (4 lines, crashes on main)

```python
for x in (1, 2):
    print(x)
for y in (30, 40, 50):
    print(y)
```

SIGSEGV at runtime.

## Mechanism (from the generated C)

All tuple iterations share ONE `__tuple_iter__` CreationSet:

1. Its `thetuple` field is a union of differently-SHAPED tuple
   structs (2-tuple and 3-tuple have different layouts), emitted as
   `_CG_void e5;` with per-clone casts.
2. `tuple.__len__` constant-folds per arity
   (`__pyc_clone_constants__` on the len prim), so each
   `__pyc_more__` clone hard-codes ONE arity — e.g.
   `position < 3` — and the clone bound for the other loop runs the
   2-tuple to index 2.
3. The class-body `___init___` installs `__pyc_more__`/`__next__`
   into the GLOBAL PROTOTYPE's method-pointer slots (issue 029/030
   machinery); every iterator instance clones from that one
   prototype, so both loops get whichever clone was installed last
   — wrong-arity length checks + out-of-bounds
   `tuple::__getitem__` → garbage/segfault.

## What was tried

Marking `__tuple_iter__` `clone_methods_per_cs` (the issue-045
lever that works for `__list_iter__`/`range`) did NOT split the CS
here — the iterator instances materialize through the prototype
`___init___`/`_CG_prim_new` path in the creating scope rather than
per-`__new__`-contour `creation_point` calls, so the flag's
parent-reuse skip never applies. Backed out.

## Fix directions

- Per-creating-contour iterator CSs for the prototype-clone path
  (extend the 045 skip to prototype-based instantiation), or
- Per-arity `__tuple_iter__` method clones bound through the
  INSTANCE's own slots rather than the shared prototype's (the
  ___init___ writes `g0->e2/e3` — the prototype — instead of
  `self->e2/e3`), or
- Lower `for x in <tuple>` to an index loop in the frontend
  (mirrors what tuple.__pyc_tolist__ now does; tuples' fixed arity
  makes the iterator object pure overhead anyway).

## Verification

- [x] The 4-line repro prints 1 2 30 40 50 (confirmed 2026-08-06,
      both backends).
- [x] tests/for_over_tuple.py keeps passing.
- [x] A new test with two different-arity tuple loops in one file —
      `tests/tuple_iter_mixed_arity.py`, expanded to three arities,
      same-arity-different-type, and a shared function.
