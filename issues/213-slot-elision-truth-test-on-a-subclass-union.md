# 213 — slot elision: a truth test on `{None, Base, Sub}` breaks the layout contract

**Status:** open. Filed 2026-11-19, found porting `logging`
([issues/197](../../issues/197-logging.md)), whose logger chain is exactly
this shape. Present on the pushed HEAD `ce537989`. A refusal at codegen, not
a wrong answer: "object layout contract violated".

## Symptom

```python
class L:
    def __init__(self, level):
        self.level = level
        self.parent = None
class R(L):
    pass
root = R(3)
a = L(4)
a.parent = root
x = a
n = 0
while x:              # x is {None, L, R}
    n += x.level
    x = x.parent
print(n)              # CPython 7
```

```
error: object layout: 'L' is blind-cast to 'R' and read at e18, but member
width differs at e0 (_CG_void vs <placeholder>), in '__init__' (r5.py:2)
fail: object layout contract violated at 2 blind cast(s) -- see ifa/issues/123
```

`PYC_ELIDE_SLOTS=0` compiles it and prints 7. Without `None` in the union
(`{L, R}`), or without the subclass, it compiles.

## What is established

- `e0` is the `__bool__` method slot (`object.__bool__`, inherited).
- In the generated C, `R`'s instance struct keeps `e0` (`_CG_void e0; /*
  __bool__ */`) and `L`'s elides it. Both keep `e11`, `__pyc_to_bool__`.
- `R`'s prototype initializer (`R::___init___`) stores `object.__bool__`
  into `e0`; `L`'s does not. That is so even with elision off, after
  dead-store removal: the store survives on `R` only.
- `L.__init__` is one clone shared by `L` and `R` instances, which is a
  blind cast and needs the two layouts to agree up to the slot it reads.

So something counts `__bool__` as used on `R`'s CreationSet and not on
`L`'s, after a truth test whose receiver is `{None, L, R}`.
`object.__pyc_to_bool__` calls `self.__bool__()`. With None in the union
the truthiness dispatch has a None arm (`__pyc_None_type__.__bool__`), and
codegen gives the truthiness selectors special treatment
(`codegen_common.cc` around line 1910, `cg.cc` around 2613). One of those
is the likely place where the read is attributed to one class only.

## Not yet known

- Which read keeps `R`'s store alive (`IFA_DBG_SLOTUSE` lists the slot
  accounting; it was not read to the end).
- Why `R`'s prototype writes the inherited `__bool__` and `L`'s does not:
  the includes copy loop in `gen_class_pyda` (`python_ifa_build_syms.cc`)
  resolves each inherited member at the class in the MRO that defines it,
  and `R`'s MRO is `[R, L, object]`.

The rule from ifa/123 applies: the answer is not to make every clone of a
class agree, but to find why the two classes disagree on a slot neither
program-visibly uses.

## Workaround in place

`pyc_lib/logging.py` tests its logger chain and a handler's formatter with
`is not None`, which is the same answer for those classes. A user program
that tests a logger or handler for truth still hits this.

## Test

`tests/truth_test_subclass_union.py` with a `.known_issue` sidecar. It
flips to PASS when this is fixed.
