# 051 — LLVM backend: nested list indexing crashes when the outer list's element type is a mixed Type_SUM

**Status: REOPENED 2026-08-20 — REGRESSED.** Was closed 2026-08-06 after
being verified fixed; it came back, and the closure notes below are what
made the return easy to miss.

## Regression, bisected

`tests/list_element_type_union.py` segfaults again under `-b` (rc=139,
no output at all — it dies before the first `print`). `git bisect` over
the 195 commits since the closure, with a probe that builds, compiles
under `-b`, runs, and compares to the recorded output:

    first bad commit: 8cc8434f
    "ifa: enable PYC_NOMARK by default -- provenance is not contour identity"

Confirmed directly on today's HEAD:

| | LLVM backend |
|---|---|
| `PYC_NOMARK=1` (the default since 8cc8434f) | **segfault** |
| `PYC_NOMARK=0` | `0 15 16 / 1 125 141 / 2 576 717` ✓ |

The C backend compiles the same FA output correctly under BOTH settings,
so this is not the mark change producing wrong types — it is an LLVM
codegen assumption that the coarser contour partitioning violates.

## Where it dies

Inside `__list_iter__.__next__`, inlined into `__main__`, on the
`for cur_state in states:` loop of the BFS:

```
=> mov (%rdx,%rcx,8),%rax     rdx = 0x8, rcx = 0
   mov %rax, cur_state
```

`__next__` loads the list data pointer from `thelist - 8` and gets `8`
— i.e. `thelist` (field 6 of the iterator) is not a list. Address
`0x8` is a null base plus the header offset.

## What has been RULED OUT

- **Not an arity/signature mismatch.** Every `call` in the emitted
  module matches its callee's definition, in both the good and bad
  builds (checked mechanically across the whole module).
- **Not the iterator's layout.** `%__list_iter__` is the same type in
  both builds; the constructor stores `thelist` at field 6 and
  `__next__` reads field 6 in both.
- **Not the `states` construction.** Identical IR in both builds —
  `_CG_prim_tuple_list_internal` then `_CG_to_list_runtime`.
- **Not `sizeof_element`** (the original 2026-07-18 filing's suspicion),
  and not the nested `affected_cubies[face][i]` read the original
  symptom pointed at: the crash is on the plain `states` iteration.

So a bogus VALUE reaches `thelist`, in a module whose IR is internally
consistent. The two builds differ in which contour clones exist (the
IR diff is ~1000 lines, mostly renumbering), so the next step is to
find which clone of `__iter__`/`__next__` the `states` loop actually
binds to under NOMARK and what it was handed.

## Lesson for the closure notes

The 2026-08-06 closure said "already resolved by subsequent codegen and
dispatch fixes" without identifying WHICH fix. Nothing then pinned the
behaviour except the test itself — and the test is `.exec.check`-verified
only on the C backend path in `make test-e2e`'s first phase. The LLVM
phase's failure sat in the second summary, which is easy not to read
(see the note in the test-status memory). **A closure that cannot name
the fix cannot tell you when it comes undone.**

### Original closure text (2026-08-06), kept for the record

#### Resolution Summary
Re-testing revealed that this bug was already resolved by subsequent codegen and dispatch fixes (such as augmented-assign to subscript handling and `sizeof_element` for non-record boxed unions).

`tests/list_element_type_union.py` now compiles and executes cleanly on both the C backend and the LLVM backend (`./pyc -b`), producing output matching CPython byte-for-byte (`0 15 16\n1 125 141\n2 576 717`). Added `tests/list_element_type_union.py.exec.check` to promote it from a compile-only test to a fully execution-verified test in the suite.

**Original filing follows.**

**Status:** open, found 2026-07-18 while fixing issue 025's rubik2
sizeof_element bug (see that entry's continuation in
[../../issues/025-shedskin-examples-coverage.md](../../../issues/025-shedskin-examples-coverage.md)).
**Affects:** `ifa/codegen/cg_emit_llvm.cc` (LLVM backend only — the
C backend, `ifa/codegen/cg.cc`, handles the same program correctly).
**Related:** the `P_prim_sizeof_element` fix in this same commit
(both `cg.cc` and `cg_emit_llvm.cc`), which is necessary but not
sufficient for this program under `-b`.

## Symptom

`tests/list_element_type_union.py` (compile-only in the suite for
exactly this reason) segfaults under the LLVM backend (`pyc -b`)
but runs correctly and matches CPython under the default C backend.

Isolated further: the crash happens on the very *first*
`cube_state.apply_move()` call — before the BFS/set-growth logic
that issue 025's sizeof_element bug actually lived in is reached at
all. `apply_move` indexes `affected_cubies[face][i]` — a nested
list index (list-of-lists) — right after `affected_cubies` (a
list-of-lists literal) and `next_states`/`states` (lists of
`cube_state` instances) have both been seen by FA as uses of the
one, program-wide-unified `list` class. That unification is what
makes `list`'s element type a `Type_SUM` of `{list, cube_state}` in
the first place (see issue 025's writeup); this issue is what goes
wrong afterward, specifically on the LLVM side.

## What's known

- The `P_prim_sizeof_element` fix (this commit, both backends) makes
  `list.append`'s resize call see the correct pointer-sized element
  slot (8 bytes) for this exact union — confirmed via a debug trace
  showing `uniform=true, common=8` on the LLVM side. That fix is
  real and correct, but doesn't resolve this crash.
- `sym_to_llvm_type` (`cg_emit_llvm.cc` ~line 200) maps any
  Type_SUM/Type_PRIMITIVE/other non-record/non-string/non-nil type
  to an opaque `ptr` — so both `list` and `cube_state` resolve to
  the same LLVM type, consistent with the sizeof fix.
- The crash therefore isn't in `sizeof_element` or in the element's
  *nominal* LLVM type — it's somewhere in how a nested nested-index
  read (`affected_cubies[face][i]`, i.e. `__getitem__` off a
  Type_SUM-element list producing another list, then `__getitem__`
  off *that*) is lowered, OR in how the `affected_cubies` list
  literal itself is constructed once its sibling lists elsewhere in
  the program force a Type_SUM element type onto the shared `list`
  class (`P_prim_make`'s "flat list" sub-shape, `cg_emit_llvm.cc`
  ~line 1549, is the likely place — it wasn't traced further).
- The C backend (`cg.cc`) does not have this problem — same source
  program, same FA-computed types, correct output.

## Repro

`tests/list_element_type_union.py` (already in the tree,
compile-only — no `.exec.check`, so `PYC_FLAGS=-b ./test_pyc.py`
stays green). To reproduce the crash directly:

```
./pyc -b -D . tests/list_element_type_union.py
./tests/list_element_type_union   # segfaults
```

Adding `print(...)` statements between `goal_state = cube_state(...)`
and the first `apply_move` call in a scratch copy shows output up
through cube construction, then nothing — the crash is inside (or
immediately preceding) the first `apply_move`/`affected_cubies[face][i]`
access.

## Possible directions (not investigated)

- Trace `P_prim_make`'s "flat list" sub-shape (`cg_emit_llvm.cc`
  ~1549-1650) for `affected_cubies`'s construction once its element
  type is forced to a Type_SUM by the unrelated `cube_state` lists
  elsewhere in the program — check whether it still assumes a
  concrete (non-union) element type when choosing element size/GEP
  stride for the literal's per-index stores.
- Trace `__getitem__` codegen for a list whose element type is
  Type_SUM — does it correctly re-derive the *specific* concrete
  type of the value being read (a `list`, to then index again) from
  an opaque `ptr` slot, or does it assume the element IS the type
  that happens to be used at that particular call site?
- Compare against `resolve_union_receiver` (`cg_emit_llvm.cc` ~511)
  — a similar-sounding "concrete component from a union" mechanism
  used for method dispatch; check whether list-element reads need
  (and lack) the same kind of resolution.

## What this unblocks

The LLVM backend (`-b`) matching the C backend's coverage on
programs that mix list-of-list and list-of-record usage under one
generic `list` class — currently the LLVM backend silently crashes
on a shape the C backend handles correctly, which is a coverage gap
for anyone treating `-b` as a drop-in alternative backend.
