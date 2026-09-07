# 140 — a `{None, scalar}` union needs an explicit cast at call arguments and field stores

**Status:** fixed 2026-09-07. Root cause of `richards`'s compile failure
under `PYC_CSDCPA1=2`, one of the two corpus programs in
[129](129-plan-demand-driven-creation-set-splitting.md)'s bill that had
never been triaged.

## Symptom

```
richards.py.c:4588:8: error: no matching function for call to '_CG_f_11481_126'
  note: no known conversion from '_CG_int64' to '_CG_nil_type' for 3rd argument
richards.py.c:3285:27: error: incompatible integer to pointer conversion
  3285 |   t4->e0 = /* None 101 */ g1;
```

Four errors, two shapes: a call argument and a record-field store.

## Root cause

Under the flag a `None` global is materialised — `_CG_int64 /* None 101 */
g1;` — where the default emits none at all. That declaration is *correct*:
[048](../../issues/048-none-int-field-pair-runtime-abort.md) records that
`{None, int64}` IS representable, stored as the scalar and round-tripped
bit-for-bit, and codegen already casts it that way at plain assignments:

```c
t1 = (_CG_nil_type)/* None 101 */ g1;
```

**It did not cast at two other emission sites.** `write_send_arg` casts
in one direction only —

```c
if (arg_is_voidish && !formal_is_voidish) fprintf(fp, "(%s)%s", formal_t, arg_cg);
else fputs(arg_cg, fp);
```

— so a `_CG_int64`-spelled `{None, int64}` actual passed to a
`_CG_nil_type` formal got no cast. The record-field store in the tuple
constructor had no cast at all.

`write_c_pnode`'s `arg_mismatch` guard did not catch it and should not
have: it tests `actual->type->num_kind`, and a union is `Type_SUM`, not a
numeric. That guard exists to refuse a BARE scalar going into a pointer
slot — genuinely unsound, as its own comment says — which is a different
case.

## Fix

`cg_is_nil_union` / `cg_needs_nil_union_cast` in `cg.cc`, used at both
sites. Deliberately narrow: it fires only for a union that ACTUALLY
CONTAINS nil, flowing into a voidish destination whose spelling differs.
A bare scalar into a pointer slot stays refused by the existing guard.

Factored rather than duplicated because this was already the second site;
a third would have drifted.

## Result

`richards` compiles under the flag. Its output still differs from CPython
— but **identically to the default arm**, which is
[issues/120](../../issues/120-richards-silent-wrong-answer.md), a
pre-existing wrong answer. Parity with the default is what the flip needs.

`make test` 311/0, LLVM backend 311/0.

## Also established: `othello3` is not a flag blocker

It fails with `FA flow analysis made no EntrySet progress for 120s` on
**both** arms, identically. It is one of the DEFAULT's own two compile
failures, and had been miscounted as part of the flag arm's gap.

## `softrender` — the same shape at a third site (fixed 2026-09-07)

```
softrender.py.c:2361:8: error: cast from pointer to smaller type '_CG_bool'
                               (aka 'unsigned char') loses information
```

```c
_CG_bool _CG_f_6601_7/*__coerce__*/(_CG_list a1) {
  t1 = _CG_prim_coerce(_CG_bool, t2);   // t2 is _CG_list
```

`_CG_prim_coerce(_t, _v)` expands to `((_t)_v)`, so a pointer narrowed to
`unsigned char` is a hard C error.

**A fix for this already existed and did not fire.** ifa/055 added a
`(uintptr_t)` route at exactly this site, and its comment even names
softrender — *"softrender's `__coerce__(_CG_any) -> _CG_bool`"*. But it
tested three literal spellings:

```c
bool v_ptr = vt && (!strcmp(vt, "_CG_any") || !strcmp(vt, "_CG_void") || !strcmp(vt, "_CG_nil_type"));
```

Under `PYC_CSDCPA1=2` the contour is `__coerce__(_CG_list)`, which is not
one of those names — so the guard missed it, for the same program it was
written for.

Broadened to *"not a scalar spelling, and 8 bytes wide"*, reusing
`cg_ctype_width`'s own table rather than adding a second list of names to
drift from. The meaning is unchanged and still correct: for a pointer
operand the coercion IS the null test, so `(bool)(uintptr_t)p` is `p != 0`
— exactly Python's truthiness for a container.

*Result:* `softrender` compiles under the flag, matching the default's
compile status. It still aborts at run time (`rc=134`) — **identically at
the default**, so that is a separate pre-existing failure, not a flag
regression. `make test` 311/0, LLVM backend 311/0, dparse green.

**The pattern worth noting:** three of the four sites fixed in this issue
were *name-based* type tests that missed a spelling the flag introduced.
That is CLAUDE.md's "never decide by name" rule applying to C type
spellings, not just Python identifiers.