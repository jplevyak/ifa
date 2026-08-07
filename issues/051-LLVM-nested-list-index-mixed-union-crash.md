# 051 — LLVM backend: nested list indexing crashes when the outer list's element type is a mixed Type_SUM

**Status:** open, found 2026-07-18 while fixing issue 025's rubik2
sizeof_element bug (see that entry's continuation in
[../../issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)).
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
