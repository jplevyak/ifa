# 164 — a `{None, T}` union at a primitive ARGUMENT was rejected, though pyc already represents it

**Status: FIXED** 2026-09-17 (`ifa/analysis/fa.cc`,
`nil_member_is_representable`). Fixes `solitaire`; regression tests
`tests/nil_union_prealloc.py` (the idiom) and
`tests/nil_only_arg_rejected.py` (the half that must stay an error).

**Related:** [060](closed/060-none-branch-dropped-mixed-with-literal-bool-sequence.md)
(the settled model this restores consistency with),
[140](140-nil-union-needs-a-cast-at-calls-and-field-stores.md) (the
codegen half, already fixed), [165](165-none-reaching-an-operation-is-silently-accepted.md)
(the residual deviation this exposes, still open),
[../../issues/048](../../issues/048-none-int-field-pair-runtime-abort.md)
(the `{None, scalar}` case, deliberately NOT covered here).

## Symptom

```python
cipher = [None] * len(txt)          # preallocate
for n in range(len(txt)):
    cipher[n] = toChar(...)         # every slot overwritten
return "".join(cipher)
```

```
solitaire.py:119:204: error: illegal primitive argument type 'x' illegal: __pyc_None_type__
```

A correct CPython program, and the standard preallocation idiom. Under
ifa/158 (every type violation fatal) it is a hard compile failure.

## Root cause

`type_cannonicalize` already **strips `nil_type` from the AType `->type`
projection** whenever the rest of the union is pointer-shaped. That is
issue 060's settled decision, recorded in its closing note as
*"`Optional[pointer]` still single-clone (frontend-sanctioned merge
preserved)"* — for a pointer, `None` is a null pointer and unambiguous.
It is also exactly what shedskin emits for this program:

```cpp
list<str *> *cipher;
cipher = ((new list<str *>(1,NULL)))->__mul__(len(txt));
```

`list<str *>`, with `None` as `NULL`. **No union, and no representation
change required.**

Dispatch, narrowing and defaulted parameters all read `->type`, so none
of them ever sees the None. The primitive-argument check in
`add_send_edges_pnode` read the **raw `arg->out`**, which made it the one
consumer that rejected a member the rest of the compiler had already
agreed to represent.

**The tell is that the two operand positions of one operator disagreed:**

| | `None` in the… | result |
| --- | --- | --- |
| `a[1] + "y"` | receiver | compiled silently |
| `"".join(a)` → `r = r + x` | argument | **fatal** |

Same operator, same union, opposite verdicts. One of them had to be
wrong; both were (see 165 for the other).

## Why no split is the answer here

The union is **TEMPORAL** — the list holds `None` at t0 and `str` at t1,
in one object, from one creation point. That is CLAUDE.md's `bh` case:
no contour split can separate it, and none should be asked for. It is
also not boxing: both members are pointer-sized, and pyc has represented
the union since 060. The answer is the representation pyc already has.

## Fix

`nil_member_is_representable(arg, diff, legal)` suppresses the violation
only when all four hold:

1. the rejected part is `nil_type` **and nothing else**;
2. `arg->out->type` is non-empty — **a nullable pointer needs a pointee**,
   so `"" + None` stays an error;
3. `arg->out->type` contains no nil — i.e. canonicalization actually
   *stripped* it, which is what makes this the pointer case;
4. the non-nil part is itself legal for the primitive.

(3) is what keeps `{None, scalar}` out. 060 deliberately **keeps** nil in
`->type` when the union carries a `num_kind` scalar, because `None` and
`0` share a bit pattern under the unboxed representation, so the guard
declines and the diagnostic stands. `genetic2` is that case — an implicit
fall-through `return None` from `execute()` unioned with `int64` — and it
is issues/048's problem, not this one.

## Measured

- `solitaire`: 1 error → compiles, runs, prints `All tests passed.`,
  matching CPython (only the `TIME` line differs, which issues/163's
  variance filter handles).
- Six CI gates green; suite 312 passed / 0 failed / 26 known on both
  backends, unchanged from baseline.
- Corpus programs carrying a `__pyc_None_type__` diagnostic before the
  fix, as (None errors of total errors): `solitaire` 1/1, `genetic2` 3/3,
  `neural1` 1/14, `life` 5/18, `sokoban` 1/17, `plcfrs` 2/122,
  `rubik` 25/134.

  `solitaire` is the only one this unblocks. `genetic2` is the other
  program whose errors are ALL of this family, but they are the
  `{None, scalar}` kind that condition (3) deliberately excludes, so it
  still fails; the remaining five have unrelated errors besides.
