# 061 — C backend: a list of tuples emits `(null)*` / incompatible-pointer element types when several distinct tuple record types coexist

**Status:** open. Found 2026-07-22 while verifying the tuple-comparison
primitive work (issue 025 / tictactoe, `tuple.__lt__`/`__eq__` →
`P_prim_tuple_lt`/`P_prim_tuple_eq`). Filed rather than fixed: the fix
site is the C backend's list-literal / list backing-store element-type
naming, a hot path shared by all list codegen.

**Re-verified 2026-08-07** (independently, prompted by a direct
question — several other issues from this same investigative period
turned out to be incidentally fixed by unrelated commits, so this one
was re-checked rather than assumed still-accurate): **partially fixed
incidentally, root cause still live, and the LLVM claim below is now
wrong.**

- The two repros exactly as originally written **do now compile
  clean on both backends** — commit `c6b2d9bb` ("Fix codegen.",
  2026-07-29, the same incidental commit that also fixed issue 062)
  changed list-literal element-type naming from `cg_get_string(e)`
  to `c_type(e)` at the exact site this issue names
  (`((elem_type*)(_CG_list_ptr(t)))[i] = ...`), which is the class of
  fix this issue's own "What a fix would look like" section asked
  for.
- **But neither original repro ever prints anything** — they only
  reproduce a *compile*-time symptom, so "compiles clean" is not the
  same claim as "the bug is gone." Adding a single, completely
  ordinary `print()` reopens it immediately: `a = [(3,99),(1,22)];
  a.sort(); b = [(2,(1,9)),(1,(5,5))]; print(b)` — note **`a` is
  never printed** — still hits the *identical*
  `error: incompatible pointer types assigning to '_CG_ps...' from
  '_CG_ps...'` this issue quotes verbatim, at the identical
  `_CG_list_ptr(t)` construct site. Removing `a.sort()` makes it
  compile clean again, confirming the root cause below (the shared
  `.sort()` clone contaminating an unrelated list's element-type
  resolution) is unchanged. `print(a)` alone (the sorted list) is
  fine — it's specifically an unrelated, never-sorted list that
  breaks, which is what makes this a genuine cross-contamination bug
  rather than "sorting a heterogeneous list is unsupported."
- **The "LLVM backend does not have the bug" claim (below, and in
  "What a fix would look like") is no longer true and may never
  have been rechecked with a print() present.** With the same
  print()-added repro, LLVM now compiles clean but **silently prints
  wrong data** — `[(, ), (, )]` instead of `[(2, (1, 9)), (1, (5,
  5))]` for the unrelated list `b`. This is a regression in kind, not
  just degree: the original C-only symptom was a loud compile
  failure (safe, if inconvenient); LLVM's version is silent data
  corruption. Do **not** use LLVM as the "behavioural oracle" for
  this bug class going forward (see "What a fix would look like",
  also corrected below).

**Affects:** `ifa/codegen/cg.cc` list construction — the
`((elem_type*)(_CG_list_ptr(t)))[i] = ...` initialiser emission, where
`elem_type` is the C name of the list's element `Sym` — confirmed
still the exact site as of the 2026-08-07 re-verification. Also,
as of 2026-08-07, `cg_emit_llvm.cc`'s equivalent list-element storage
path (silent wrong-data corruption instead of a compile error).

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
[056](closed/056-CGEN-degraded-index-type-raw-c-compile-error.md).

## Root cause (working theory)

`list.sort` is a single generic method that gets cloned/shared across
call sites; the shared list contour's element `Sym` ends up as a union
(or an unnamed merge) of the several concrete tuple record types in the
program. The C backend has no clean name for that merged element type,
so `c_type()` yields `null` (or it picks one arm's struct name and then
assigns a different arm's struct pointer into it → the
incompatible-pointer error).

**Update, 2026-08-07:** the LLVM backend is not, in fact, unaffected —
see the re-verification note at the top. It goes through different
mechanics (no C struct name to fail to synthesize, so no compile-time
signal at all), but the same underlying merged-contour element type
produces silently-wrong stored/loaded tuple field values at runtime.
The two backends share the root cause (the `.sort()`-shared contour
merge); they differ only in how badly each one fails to cope with it.

This is orthogonal to tuple comparison — it reproduces at baseline
(before the `P_prim_tuple_*` work) with plain homogeneous tuples, and it
reproduces with **no** comparison at all as soon as a `.sort()` forces
the shared-list-contour merge. The tuple-comparison work merely made it
easier to hit, because heterogeneous / nested tuple sorts now type-check
far enough to reach codegen.

## Repro

The two repros below were the original filing's; **as literally
written (no `print()`) they no longer reproduce the bug** on either
backend as of 2026-08-07 — `c_type()` now names the merged element
type well enough to *compile*, but that's a false negative, not a fix:
neither repro consumes the merged element type at runtime, so the
vulnerable path is simply never exercised.

```python
# two distinct homogeneous tuple types, both sorted -- COMPILES CLEAN as of
# 2026-08-07 on both backends (false negative, nothing prints)
a = [(3, 1), (1, 2)];        a.sort()
b = [(2, 9, 1), (1, 5, 5)];  b.sort()
```

```python
# only ONE sorted, a second distinct-typed tuple list present -- COMPILES CLEAN
# as of 2026-08-07 on both backends (false negative, nothing prints)
a = [(3, "c"), (1, "b")];        a.sort()
b = [(2, (1, 9)), (1, (5, 5))]   # not even sorted
```

**Current, live repro (2026-08-07)** — add a `print()` of the
*unrelated, never-sorted* list to force the merged element type to
actually be consumed:

```python
a = [(3, 99), (1, 22)]
a.sort()
b = [(2, (1, 9)), (1, (5, 5))]
print(b)          # a itself doesn't even need to be printed
```

- **C backend:** still fails to compile with the exact
  `incompatible pointer types assigning to '_CG_ps...' from
  '_CG_ps...'` error quoted above at the identical
  `_CG_list_ptr(t)` construct site. Removing `a.sort()` makes it
  compile clean, confirming `.sort()` is the necessary trigger, not
  the print itself.
- **LLVM backend:** compiles and runs, but **silently prints wrong
  data**: `[(, ), (, )]` instead of the correct `[(2, (1, 9)), (1, (5,
  5))]`.

Note: a *single* heterogeneous tuple type (e.g. `(int, str)`) printed
after `.sort()` fails on **both** backends with a different, unrelated
error (C: `assert(!"runtime error: matching function not found")`;
LLVM: silent blank tuple fields) even with only one tuple type in the
whole program — this is a distinct, not-yet-filed bug (it doesn't
require the "several distinct tuple types" condition that defines
this issue) and should not be conflated with 061's root cause when
someone next stumbles on it.

A *single* tuple type sorted (any arity, nested, heterogeneous fields)
codegens fine on C — see `tests/tuple_compare.py`, which deliberately
sorts one nested-tuple list and does all its other checks as direct
comparisons to stay clear of this bug.

## What a fix would look like

Give the merged list-of-tuples element type a real C struct name (emit a
tagged-union or a common struct for the sorted-list contour), or split
the `list.sort` clone per concrete element type so each sorted list
keeps its monomorphic tuple struct. Either way, add the guard convention
from [056](closed/056-CGEN-degraded-index-type-raw-c-compile-error.md): never emit a
`(null)*` cast — if the element type has no C name, degrade to a runtime
error rather than malformed C.

**Update, 2026-08-07:** LLVM is no longer a valid "correct" oracle for
this bug — as of this date it silently corrupts the same merged-type
data instead of refusing to compile. A real fix needs to make the
*element type itself* well-defined for the shared contour on both
backends (split the clone, or give the merge a real tagged
representation); using either backend's current behavior as a
reference for "what correct looks like" would be wrong.

## What this unblocks

Heterogeneous/nested tuple sorting on the C backend in programs that mix
several tuple shapes (the general shedskin case), and any C program that
puts more than one distinct tuple type into sorted lists.
