# 166 — the LLVM backend segfaults on `len()` of a NULL list, which pyc produces routinely

**Status: FIXED** 2026-09-18 (`ifa/codegen/cg_emit_llvm.cc`
`emit_send_len`). Regression test `tests/list_mul_zero_null_list.py`.

**Related:** [../../issues/039](../../issues/039-list-mul-shared-element-type-cross-contamination.md)
(the other `[None] * n` defect), [164](164-nil-union-at-a-primitive-argument-is-a-nullable-pointer.md)
(whose regression test surfaced this).

## Symptom

```python
a = [1] * 0
print(len(a))
```

| backend | result |
| --- | --- |
| C (default) | `0` |
| LLVM (`-b`) | **segfault** |

Two lines, and nothing to do with `None` — `[None] * 0`, `["x"] * n` with
`n = 0`, and iterating such a list all crash the same way. Creating the
list is fine; the first *use* of it faults.

## Root cause

**NULL is a legal `_CG_list` and means EMPTY.** The C backend has always
said so:

```c
#define _CG_prim_len(_c, _l) ((_l) ? _CG_list_len(_l) : 0)
```

and pyc produces one routinely — `_CG_list_mult_internal` opens with
`if (!l) return 0;` for a zero repeat count, and `GC_MALLOC` zeroing
leaves a never-assigned list field NULL.

`emit_send_len` in the LLVM backend loaded the `len` header
unconditionally:

```
len_addr = gep i8, ptr %obj, -12
len32    = load i32, ptr %len_addr
```

so a NULL list dereferenced address `-12`. The C backend printed the
right answer only because its macro carries the guard; where the C
backend also constant-folds the `len` away, the divergence is invisible
until something defeats the fold.

## Fix

Guard the load the way the C macro does — `icmp` the pointer, branch, and
phi in `0` for the null edge. The optimiser folds the whole thing away
wherever the pointer is known non-null, so this costs nothing in the
common case.

The alternative — making `_CG_list_mult_internal` return a real empty
list instead of NULL — was rejected as the primary fix. NULL-as-empty is
part of pyc's list representation contract (that is *why* `_CG_prim_len`
guards it), and a never-assigned list field is NULL no matter what
`__mul__` does, so the backend has to honour the contract regardless.

## How it was found, and why it had not been

[164](164-nil-union-at-a-primitive-argument-is-a-nullable-pointer.md)'s
regression test calls `pad("")`, which reaches `[None] * 0`. Before 164
that program did not compile, so the codegen path was never exercised.

Attributed with `PYC_NILARG=0` (which restores the pre-164 raw-`out`
check): the two-line repro above compiles clean **with 164's guard
disabled** and still segfaults, so this predates 164 entirely and 164
only exposed it.

A verification gap of mine is worth recording with it: the LLVM gate was
run *before* those tests were added, so "six gates green" was reported
without the new tests having run under `-b`. Add a test, then re-run both
backends — not the other way round.

## Measured

Six CI gates green, suite 315 passed / 0 failed / 26 known on **both**
backends.
