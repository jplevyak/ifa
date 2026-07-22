# 061 — C backend: a list of tuples emits `(null)*` / incompatible-pointer element types when several distinct tuple record types coexist

**Status:** open, found 2026-07-22 while verifying the tuple-comparison
primitive work (issue 025 / tictactoe, `tuple.__lt__`/`__eq__` →
`P_prim_tuple_lt`/`P_prim_tuple_eq`). Filed rather than fixed: the fix
site is the C backend's list-literal / list backing-store element-type
naming, a hot path shared by all list codegen, and the LLVM backend does
*not* have the bug (it compiles and runs every repro below correctly), so
this is a C-backend-only type-naming gap, not a frontend/FA defect.
**Affects:** `ifa/codegen/cg.cc` list construction — the
`((elem_type*)(_CG_list_ptr(t)))[i] = ...` initialiser emission, where
`elem_type` is the C name of the list's element `Sym`.

## Symptom

When two or more **distinct** fixed-arity tuple record types appear in
one program *and* at least one list of tuples is `.sort()`ed, the C
backend emits either a literal `(null)` element type:

```
het.py.c:844:12: error: expected expression
  844 |   (((null)*)(_CG_list_ptr(t75)))[0] = t77;
```

or two structurally-distinct tuple structs get conflated:

```
sort_a.py.c:600:42: error: incompatible pointer types assigning to
  '_CG_ps9066' (aka 'struct _CG_s9066 *') from '_CG_ps9074'
```

Both are genuine pyc-produced C compile errors (the program "types",
`.c` is written, only `clang` rejects it), the same *class* of bug as
[056](056-degraded-index-type-raw-c-compile-error.md).

## Root cause (working theory)

`list.sort` is a single generic method that gets cloned/shared across
call sites; the shared list contour's element `Sym` ends up as a union
(or an unnamed merge) of the several concrete tuple record types in the
program. The C backend has no clean name for that merged element type,
so `c_type()` yields `null` (or it picks one arm's struct name and then
assigns a different arm's struct pointer into it → the
incompatible-pointer error). The LLVM backend routes list element
storage through its own type machinery and is unaffected.

This is orthogonal to tuple comparison — it reproduces at baseline
(before the `P_prim_tuple_*` work) with plain homogeneous tuples, and it
reproduces with **no** comparison at all as soon as a `.sort()` forces
the shared-list-contour merge. The tuple-comparison work merely made it
easier to hit, because heterogeneous / nested tuple sorts now type-check
far enough to reach codegen.

## Repro

C backend only (all of these compile+run correctly on `-b` / LLVM):

```python
# two distinct homogeneous tuple types, both sorted -> (null)*
a = [(3, 1), (1, 2)];        a.sort()
b = [(2, 9, 1), (1, 5, 5)];  b.sort()
```

```python
# only ONE sorted, a second distinct-typed tuple list present -> incompatible ptr
a = [(3, "c"), (1, "b")];        a.sort()
b = [(2, (1, 9)), (1, (5, 5))]   # not even sorted
```

A *single* tuple type sorted (any arity, nested, heterogeneous fields)
codegens fine on C — see `tests/tuple_compare.py`, which deliberately
sorts one nested-tuple list and does all its other checks as direct
comparisons to stay clear of this bug.

## What a fix would look like

Give the merged list-of-tuples element type a real C struct name (emit a
tagged-union or a common struct for the sorted-list contour), or split
the `list.sort` clone per concrete element type so each sorted list
keeps its monomorphic tuple struct. Either way, add the guard convention
from [056](056-degraded-index-type-raw-c-compile-error.md): never emit a
`(null)*` cast — if the element type has no C name, degrade to a runtime
error rather than malformed C. Keep the LLVM backend (which is correct)
as the behavioural oracle.

## What this unblocks

Heterogeneous/nested tuple sorting on the C backend in programs that mix
several tuple shapes (the general shedskin case), and any C program that
puts more than one distinct tuple type into sorted lists.
