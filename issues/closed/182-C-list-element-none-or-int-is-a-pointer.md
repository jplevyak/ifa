# 182 — C backend: a `{None, int}` list element is stored as a pointer

**Status:** CLOSED 2026-10-03. Filed 2026-10-02, found fixing pisang's
LLVM build ([181](181-LLVM-unhandled-prim-emits-nothing.md)).

## Symptom

```python
def first_negative(xs):
    for x in xs:
        if x < 0:
            return x          # otherwise falls off: None
fixedt = [0] * 10
mods = [3]
mods.append(first_negative([4, -7, 2]))
for lit in mods:
    fixedt[abs(lit)] = int(lit > 0)
print(fixedt)
```

CPython prints `[0, 0, 0, 1, 0, 0, 0, 0, 0, 0]`. The LLVM backend now
matches it. The C backend emits `_CG_prim_list(_CG_void, 1)` and
`((_CG_void*)...)[0] = 3`, which does not compile: "incompatible integer to
pointer conversion assigning to '_CG_void'". shedskin_examples/pisang has
the same shape (`mods.append(unfixed(clause))`) and fails at run time on
the C backend with `list element type mismatch`.

## Root cause: three defects, all three needed

1. **Clone-made unions were not self-typed.** Every front-end type has
   `t->type == t` (`set_type_and_meta_type`, ast.cc). Codegen relies on
   that: `c_type(Sym*)` reads `->type`. The five `Type_SUM` construction
   sites in clone.cc left `->type` null, so any union the clone pass
   minted rendered as `_CG_void`. They now go through `new_sum_type()`.
2. **Element types were never collected.** `collect_types_and_globals`
   gathers every live Var's type and everything reachable through `has`,
   and it never followed a container's `element`. An element union that
   no Var happened to carry was never named, so even when self-typed it
   had no C type string. It now follows `element`.
3. **The element/field guards asked the wrong question.** The C index
   load/store paths refuse a scalar-vs-pointer store with
   `assert(!"runtime error: list element type mismatch")`, and they tested
   that with `num_kind != 0`. `None | int` has no num_kind but IS an int64
   in C, so `list.append` of an int into the now-correctly-typed list
   still tripped the assert (pisang's abort). `cg_repr_mismatch` compares
   the emitted C types instead.

With 1+2 the reduced repro compiles and matches CPython. pisang needs 3
as well.

**Regression caught by the sweep, fixed before landing.** Following
`element` (2) reached types whose `has` holds a null, meaning a vacant
slot. The `has` walk in `collect_types_and_globals` had never met one and
dereferenced it. chess and quameon segfaulted the compiler on both
backends. The walk now skips null members.

## Verification

`tests/none_or_int_arithmetic.py` passes on both backends; its
`.known_issue` is deleted. pisang runs on the C backend and matches
CPython (3.6 s; CPython takes 28.9 s).
