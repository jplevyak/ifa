# 188 — a data field on one class and a method of the same name on another break a call through the merged contour

**Status:** closed 2026-10-13. Filed 2026-10-12, found while writing the
test for [177](177-FA-narrowing-does-not-reach-a-module-global.md). Not a
member-slot problem: a number and an object in one value had no BOXING
check, so codegen stored the int as a pointer. Now refused at compile time;
see *Resolution*.

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

## Resolution (2026-10-13)

**The slot model was never wrong.** `IFA_DBG_CSVARS` shows `A` with `f:
int64` and `B` with `f: f#1166` (the method) and `g: int64`, identical
between a failing and a passing variant. `b.f()` is not the failing call at
all: `print(xs[0].f, xs[1].g)` alone aborts, with no `b`.

**What fails.** `xs[0]` reads the list's element, which holds `{A, B}`;
pyc does not track list positions. So `xs[0].f` is `{int64, closure}`:
A's int field, or B's method bound to its receiver. That union is genuine
for the analysis. `PYC_DBG_DISPATCH` names the abort: `print`'s
`__str__` has two candidates (`int64::__str__` and
`__pyc_any_type__::__str__`), and the argument is a C `int64`, so neither
branch of the tag dispatch can be taken.

**The defect is general, and worse elsewhere.** No BOXING check covered a
numeric scalar beside a pointer-shaped value. `mixed_basics` counts distinct
basic types, and an instance or closure is not one, although the rule
`fa_flags.cc` documents ("a basic type with a pointer-shaped one") says the
union has no representation. Codegen put such a union in a `_CG_any` and
stored the int AS a pointer:

```python
x = 0
if len(sys.argv) > 5:
    x = A()
print(x)
```

compiled with no diagnostic and printed **`A`**, where CPython prints `0`.
The dispatch read the class tag through the null pointer that `0` became,
and at `-O2` that undefined behaviour took the `A` branch. That is a silent
miscompile, not an abort. With a function in place of `A()`, the union was
refused only by a struct-layout check (`mixes 8- and 0-byte members`).

**Fix.** `scalar_with_pointer` (fa.cc) adds a BOXING violation, at both
of `collect_var_type_violations`' BOXING sites, for a union holding a
numeric basic (`num_kind`) and a value that is neither basic, `None` nor
void.
- `None` is excluded because {None, scalar} has its own rule
  (issues/048); a null test separates it except at a dispatch.
- `str` and symbols are "basic" to `to_basic_type`, so {int, str} stays with
  `mixed_basics` as before.
- The message says what the union is: `'x' holds both a number and an
  object, which has no representation: ( int64 A )`.

As a BOXING violation this is also a demand: a union pyc invented can be
backtracked and split, and a genuine one, like this issue's, is refused.
pyc does not box, so refusal is the CPython-or-error answer.

**Measured.**
- Corpus sweep `check__default__d79eb50b+0e1cb904` against `3eb03ec8+d0c8ef56`:
  every verdict, warning count and contour count is identical. No corpus
  program held a number beside an object. The two run timeouts of the
  earlier sweep, taken under outside load, pass here.
- `make test`: green, 445/0 on both backends before the new tests.
  `container_scalar_union_add`'s golden is re-blessed. It is a refusal test
  whose stated expectation is an error naming the union, and the new check
  names `{list, float64}` ahead of the old "illegal primitive argument"
  line.
- New refusal tests (`.check_fail` + `.check`): `scalar_object_union_refused.py`
  (the miscompile above) and `field_and_method_one_name.py` (this issue's
  repro). Both pass on both backends.

**Seen, not fixed.** `print(xs[1].f)` compiles, because the read's result
holds only the bound method, and prints `<instance>` where CPython prints
`<bound method B.f of <__main__.B object at ...>>`. That is a repr fidelity
gap for bound methods, unrelated to the union.
