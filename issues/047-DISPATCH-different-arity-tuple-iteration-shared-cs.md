# 047 — Iterating two different-arity tuples in one program: shared __tuple_iter__ CS miscompiles (segfault)

**Status:** open. Found 2026-07-16 during the genetic2 dig (pyc
issues/025); pre-existing at user level, NOT caused by the dynamic
`tuple()` intercept (which merely exercised it via
`tuple.__pyc_tolist__`'s original iteration loop — since switched
to an index loop to sidestep this).

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

- The 4-line repro prints 1 2 30 40 50.
- tests/for_over_tuple.py keeps passing.
- A new test with two different-arity tuple loops in one file.
