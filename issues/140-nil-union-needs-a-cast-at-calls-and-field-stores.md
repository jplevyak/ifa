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
