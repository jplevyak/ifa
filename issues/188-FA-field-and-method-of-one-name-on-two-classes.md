# 188 — a data field on one class and a method of the same name on another break a call through the merged contour

**Status:** open. Filed 2026-10-12, found while writing the test for
[177](closed/177-FA-narrowing-does-not-reach-a-module-global.md). Nothing is
fixed.

## Symptom

```python
class A:
    def __init__(self):
        self.f = 0


class B:
    def __init__(self):
        self.g = 0

    def f(self):
        return 'method'


xs = [A(), B()]
b = B()
print(xs[0].f, xs[1].g, b.f())
```

CPython prints `0 0 method`.

- **C backend:** compiles with no diagnostic, then aborts at runtime in
  `b.f()`: `Assertion '!"runtime error: matching function not found"'`.
- **LLVM backend:** fails to compile: `LLVM module verification failed:
  Function arguments must have first-class types! void %0`.

Without the list (`a = A(); b = B()`), both backends print `0 0 method`.
So the trigger is `A` and `B` meeting in one container, with `f` a data
field on one class and a method on the other.

## What is known

- No guard or narrowing is involved, and it happens with or without 177's
  load reuse (`PYC_GUARDREUSE=0`).
- `b` is a separate `B()` creation point. Under start-merged identity it
  shares `B`'s CreationSet with `xs[1]`, so whatever the `{A, B}` element
  reads of `f` do to that contour reaches `b.f()`.

## Next

Find which contour `b.f()`'s dispatch sees: `IFA_DBG_FUNES=f` and
`IFA_DBG_CSVARS=B` should show whether `B`'s `f` member slot was given A's
`int` field type, or whether the dispatch candidate set for `f` lost B's
method. Then decide whether the member-slot model lets one name be a field
on one class and a method on another (CPython does), and fix the analysis
rather than the dispatch.
