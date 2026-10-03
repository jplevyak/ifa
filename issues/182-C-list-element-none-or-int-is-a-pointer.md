# 182 — C backend: a `{None, int}` list element is stored as a pointer

**Status:** open. Filed 2026-10-02, found fixing pisang's LLVM build
([181](closed/181-LLVM-unhandled-prim-emits-nothing.md)).

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

## Cause, as far as measured

A scalar `None | T` has T's representation everywhere else in the C
backend (`assign_type_cg_strings_pass2` gives the 2-member nil SUM T's C
type), and since 181 the LLVM backend's too. A list ELEMENT of that type is
emitted as `_CG_void`, a pointer, so the list's storage and the scalar
values written into it disagree. The element-type path has not been
traced yet.

## Fix

Give a `None | T` element T's representation, as for a scalar. Find
where the list element C type is chosen, and why it does not see the
collapse.

## Verification

`tests/none_or_int_arithmetic.py` passes on the C backend (it carries a
`.known_issue` until then), and pisang runs on the C backend.
