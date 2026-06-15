# Issue 019: v2 LLVM flat-list literals lack pyc's runtime list header

**Status:** open.
**Affects:** `ifa/codegen/cg_normalize_v2.cc` (`lower_send_alloc`)
and `ifa/codegen/cg_ir_v2_emit_llvm.cc`'s `CG2_ALLOC` emit
under `IFA_LLVM_V2=1`. Surfaced while investigating
`tests/list_concat.py` (`x = [1] + [2,3,4,5,6]`) after closing
[018-v2-loop-after-undef.md](018-v2-loop-after-undef.md).

## Symptom

`tests/list_concat.py`, `tests/list_or_concat.py`, and any
program that runs a list-literal through `list.__add__` /
`list.__mul__` / similar runtime helpers segfaults under
`IFA_LLVM_V2=1 -b`. The C backend is unaffected.

`_CG_list_add` (and friends, in `pyc_c_runtime.h`) reads pyc's
list header — a 16-byte struct at `ptr - 16` carrying
`total_len`, `len`, and an internal `ptr` field. The v2 LLVM
backend's `CG2_ALLOC` emits a raw `GC_malloc(size)` for a list
literal and stores element values starting at byte 0; nothing
allocates or initializes the header. So the runtime helper
reads garbage memory and crashes (or returns a bogus length
that's then used for downstream walks).

The two pyc literal-list shapes both hit this gap differently:

- **Tuple-list / multi-element struct** (`[2, 3, 4, 5, 6]`):
  the C backend goes through `_CG_prim_tuple_list_internal`
  which allocates `n * elem_size + 16` bytes and skips the
  header before returning. v2 emits a struct of the right
  field count, but the struct itself doesn't include the
  header — `_CG_list_add(list_ptr - 16 ...)` reads memory
  before the allocation.

- **Flat single-element list** (`[1]`): the C backend uses
  `_CG_prim_list` which is the same internal but typed for
  flat arrays. v2 emits `GC_malloc(8)` for the int64, with
  element stride GEP for the store (this commit's fix). No
  header, same crash.

The store sequence the v2 emit now produces for `[1]` is
correct in isolation (`getelementptr i64, ptr %v9, i64 0;
store i64 1, ptr %1`), but `_CG_list_add` still reads garbage
from `ptr - 16`.

## Suspected root cause

`cg_normalize_v2.cc`'s `lower_send_alloc` doesn't know about
pyc's list header convention. It treats every `P_prim_make`
SEND as "allocate a struct of the declared type" and emits one
CG2_ALLOC for that struct. The struct doesn't include the
16-byte header.

The C backend works because `_CG_prim_tuple_list` and friends
are runtime helpers that hide the header allocation behind a
macro/inline call — pyc emits one symbol call and the runtime
helper takes care of the header. The v2 emit could do the
same: route `P_prim_make` on list/tuple symbols through a
CG2_C_CALL to `_CG_prim_tuple_list_internal` (or a new typed
wrapper) and skip the inline alloc + field-store sequence
entirely.

## Verification plan

When fixing:

1. `tests/list_concat.py` (`[1] + [2,3,4,5,6]`) prints
   `[1, 2, 3, 4, 5, 6]` under `IFA_LLVM_V2=1 -b`.
2. `tests/list_or_concat.py` (uses `or` + list concat) passes
   on the same harness.
3. `nm <obj>` shows `_CG_prim_tuple_list_internal` (or
   whichever helper closes this) as an undefined reference
   that `libpyc_runtime.a` satisfies.
4. C-backend 74/0 unchanged; unit tests 105/0 unchanged.

## What this unblocks

- `tests/list_concat.py`, `tests/list_or_concat.py` and any
  test that exercises list runtime helpers (`__add__`,
  `__mul__`, slicing, etc.) move from segv/wrong-output to
  passing.
- Pyc-suite v2 LLVM ratchet should gain ~2-3 tests at the
  literal-list boundary.
- Aligns the v2 emit's list-allocation strategy with the
  established runtime contract used by `pyc_c_runtime.h`,
  removing the second source of pyc-list-layout knowledge
  (right now it lives in both `pyc_runtime.c` and the v2
  emit).
