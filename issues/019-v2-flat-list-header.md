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

## June 2026 attempt — diagnostics worth keeping

An attempted fix (reverted; commits 7f4c1c2 and earlier are
the baseline) added an out-of-line `_CG_prim_tuple_list_internal`
to `pyc_runtime.c` and routed `lower_send_alloc` for
list/tuple-symbol SENDs through a `CG2_C_CALL` to it. That
piece compiled and produced the right shape for the simple
case (`print([1, 2, 3, 4])` worked), but uncovered three
deeper entanglements that any future fix needs to handle
together:

1. **The C backend's two-stage allocation.** Looking at
   `cg.cc:140` (struct path), pyc allocates with
   `_CG_prim_tuple_list(_c, rvals.n - 2)` — that's
   `element_count + 1`, the over-alloc count. The struct
   it returns has `header.len = element_count + 1` (semantic
   length **plus one**). The conversion that fixes this lives
   in `_CG_TUPLE_TO_LIST_FUN(_s, _n)` (`pyc_c_runtime.h:216`),
   which `list.__add__` and other consumers call as
   `_CG_to_list(struct_ptr)` — it allocates a new list with
   `header.len = _n` (the **correct** count) and memcpys the
   struct's payload. v2 has no equivalent of `_CG_to_list`,
   so any v2 route through the runtime helper must either
   skip the over-alloc convention or replicate the conversion.

2. **`sizeof_element` returns sizeof(element), not
   sizeof(struct).** In the C output for `[1] + [2,3,4,5,6]`,
   `_CG_list_add(t2, t3, 8, 8)` — both size args are 8.
   That's because `_CG_prim_sizeof_element(_c, _l) sizeof(*_l)`
   for the struct-ptr `_l` of type `_CG_ps3343` is
   `sizeof(_CG_s3343) = 40` at the C level, but pyc's
   *analyzer* constant-folds the argument to a known semantic
   element size (8 for int64) before the call. The v2
   `CG2_SIZEOF_ELEMENT` emit, by contrast, computes from the
   CGv2Type at LLVM-emit time and gets 40 for the struct,
   which propagates into `_CG_list_add` and overruns the
   source buffer. Any attempt that updates the alloc shape
   has to coordinate with `CG2_SIZEOF_ELEMENT` so both sides
   see the same "element size" semantics. Falling back to
   `struct->fields[0]->type` in the emit works for
   homogeneous lists but is fragile (heterogeneous tuples
   would still mislead).

3. **dst's CGv2Type rewriting cascades.** When the FA leaves
   the dst's type as opaque `t_ptr` (which is the common case
   for flat single-element lists like `[1]`), `CG2_SIZEOF_ELEMENT`
   can't compute a stride at all. Attempting to "upgrade" the
   dst's CGv2Type in `lower_send_alloc` to a more specific
   `ptr_<elem>` breaks downstream FIELD_STORE call sites that
   were using the original struct shape — the GEP indices that
   pyc's analyzer generated for the struct's layout no longer
   match the new flat type. The two views (struct-layout-with-
   named-fields vs flat-array-of-elements) are mutually
   incompatible at the LLVM type level even though they
   produce identical byte offsets for homogeneous lists.

The structurally correct fix is probably one of:

- **(a)** Add a v2-side `_CG_to_list` equivalent that runs
  after each list/tuple `make` SEND in the same block. Keep
  the existing CG2_ALLOC + FIELD_STORE shape for the
  literal's construction; immediately follow it with a
  `CG2_C_CALL` to the conversion helper that re-emits the
  payload with the correct header.len. This is the closest
  match to what the C backend already does, including
  preserving the +1 convention's intent.

- **(b)** Skip the +1 convention entirely in v2 and route
  `make` through the runtime helper with `(sizeof(element),
  element_count)`. Requires CG2_SIZEOF_ELEMENT to agree on
  the element-size semantics across both paths, plus
  re-emitting per-element stores using INDEX_STORE
  exclusively (not FIELD_STORE) so the GEP stride matches
  what `_CG_list_add` will memcpy.

Either path is a substantial refactor that needs to be done
as one coherent commit, not incrementally — the partial
states leave the pyc-suite half-broken.
