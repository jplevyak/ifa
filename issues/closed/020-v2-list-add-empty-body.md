# Issue 020: v2 LLVM list.__add__ body is `ret ptr undef`

**Status:** **closed June 2026** — fix landed in commit
`7dc970b` ("ifa/codegen: issue 020 — list runtime helpers +
opaque-ptr fallbacks").  `list.__add__`'s v2 LLVM body now
contains the call to `_CG_list_add`; sibling methods
(`list.__mul__`, `list.__pyc_getslice__`,
`list.__pyc_setslice__`) emit their respective helpers; pyc
suite is 80/0 on both backends.

**Affects:** v2 LLVM lowering of pyc-Python library methods
that build `__pyc_c_call__` SENDs with a non-trivial type
marker as the return-type argument. Surfaced when the
issue 019 E.3 commit (`5a0ff11`) made the allocation side of
list literals sound, leaving `list.__add__`'s body as the
next bottleneck.
**Related:** [closed/019-v2-flat-list-header.md](closed/019-v2-flat-list-header.md)
(the alloc-side fix that exposed this gap).

## What landed (closes the issue)

The actual root cause turned out to be different from the
three suspected hypotheses below — the fn-name slot was being
read correctly; what was failing was downstream.  Three
coordinated changes in commit `7dc970b`:

1. **`CG2_SIZEOF_ELEMENT` opaque-ptr fallback**
   (`cg_ir_v2_emit_llvm.cc`). The previous emit bailed when
   the rval's CGv2Type was opaque (`t_ptr`), which is exactly
   how the FA leaves `self` / `l` formals in pyc library
   methods (they hold `_CG_any`-typed ptrs). The new emit
   walks the CGv2Type for a struct's first-field type to
   recover the element size, or returns a default
   `sizeof(int64) = 8` for fully-opaque ptrs.  Without a
   produced value, the downstream `CG2_C_CALL` to
   `_CG_list_add` was bailing at `resolve_value`-of-undef and
   the SEND silently disappeared — leaving the function body
   empty with `ret ptr undef`.
2. **`CG2_LEN` runtime path** (`cg_ir_v2_emit_llvm.cc`). The
   previous emit returned `ConstantInt 0` for non-string
   ptrs (placeholder from before `_CG_prim_len` existed).
   `for k in range(len(self))` inside `list.__str__` would
   iterate zero times.  Replaced with a `CG2_C_CALL` to
   `_CG_prim_len` that reads `header.len` from `ptr - 12`.
3. **`libpyc_runtime.a` exports**
   (`pyc_runtime.c:225-333`). `_CG_list_add`, `_CG_list_mult`,
   `_CG_list_resize`, `_CG_list_getslice`, `_CG_list_setslice`
   are now exported as plain externs taking int64 size args
   (matching v2's `CG2_SIZEOF_ELEMENT` which produces i64;
   diverges from `pyc_c_runtime.h`'s uint32 internals
   intentionally).

## Verification (June 2026)

- `_CG_f_1359_3` (list.__add__) body:
  `call ptr @_CG_list_add(ptr %self, ptr %l, i64 8, i64 8)`
  followed by `ret ptr %v` — no more `ret ptr undef`.
- `list.__mul__` body:
  `call ptr @_CG_list_mult(ptr %self, i64 N, i64 8)`.
- `list.__pyc_getslice__` body:
  `call ptr @_CG_list_getslice(ptr %self, i64 8, i64 %i, i64 %j, i64 %s)`.
- `list.__pyc_setslice__` body:
  `call ptr @_CG_list_setslice(ptr %self, i64 8, i64 lo, i64 hi, ptr %v)`.
- `tests/list_concat.py`, `tests/list_or_concat.py`,
  `tests/list_multiply.py`, `tests/list_comprehension.py`,
  `tests/list_slicing.py` — all PASS on v2 LLVM.
- pyc suite v2 LLVM: 80/0 (parity with C backend).

## Symptom

Under `IFA_LLVM_V2=1 -b`, the LLVM IR for `_CG_f_1359_3`
(pyc's specialization of `list.__add__`) is:

```llvm
define internal ptr @_CG_f_1359_3(ptr %self, ptr %l) {
entry:
  br label %L132
L132:
  ret ptr undef
}
```

No body inst. No call to `_CG_list_add`. The function returns
`undef` regardless of the argument shape.

The same shape probably affects sibling library methods that
use the same `__pyc_c_call__` idiom — `list.__mul__`,
`list.__pyc_getslice__`, `list.__pyc_setslice__`,
`list.append` — all in `__pyc__/04_sequence.py`. Need to
audit; not all will surface as test failures because some
have additional simplification paths.

The downstream consequence is that `tests/list_concat.py`,
`tests/list_or_concat.py`, and any program that uses list
operators segfaults in `list.__str__` (which gets called on
the undef result returned by `__add__`).

## Library shape

`__pyc__/04_sequence.py`'s `list.__add__`:

```python
def __add__(self, l):
    return __pyc_c_call__(__pyc_primitive__(__pyc_symbol__("merge_in"), self, l),
                          "_CG_list_add",
                          list, self,
                          int, l,
                          int, __pyc_primitive__(__pyc_symbol__("sizeof_element"), self),
                          int, __pyc_primitive__(__pyc_symbol__("sizeof_element"), l))
```

The first argument to `__pyc_c_call__` is a `__pyc_primitive__("merge_in", ...)` call. That's pyc's "type marker" idiom: the inner SEND's result has the abstract type the analyzer wants the c-call's return to take, but no actual code is meant to be emitted for the marker (`is_fake = 1` is set on it during build_if1, see
`python_ifa_build_if1.cc:308`).

The post-FA SEND has rvals:

```
rvals[0] = sym_primitive marker
rvals[1] = sym___pyc_c_call__ (the prim selector by name)
rvals[2] = merge_in result Var (the type marker, is_fake=1)
rvals[3] = "_CG_list_add" string constant
rvals[4..] = arg type/value pairs
```

(This is the layout `python_ifa_build_if1.cc:301-308`
produces.)

## Suspected root cause

`cg_normalize_v2.cc`'s `lower_send_c_call` reads
`pn->rvals[3]->sym->constant` for the C function name and
walks `rvals[5, 7, 9, ...]` for the value arguments. The
relevant early-return is:

```cpp
Var *name_var = pn->rvals[3];
if (!name_var || !name_var->sym || !name_var->sym->constant) {
  return false;
}
```

If `pn->rvals[3]->sym->constant` is null — e.g., because the
FA replaced the literal sym with a renamed sym that has no
constant — `lower_send_c_call` bails. The dispatcher's
fall-through then routes through the generic CG2_PRIM path,
which `dispatch_prim` silently drops because there's no emit
for "__pyc_c_call__".

There are several plausible reasons the rvals layout could
diverge from what the build-time code wrote:

1. **FA-side SSU rename.** Each pass through the optimizer
   can rename `rvals[3]` to an SSU sibling that loses the
   `sym->constant` linkage. The C backend handles this
   because its `c_call_codegen` reads `n->rvals[3]->sym->constant`
   *after* the same SSU renames, so either pyc's SSU is
   preserving the constant or there's a guarantee the FA
   doesn't rename Vars that point to constant syms.

2. **`compute_prim_arg_offset` underestimate.** With the
   `sym_primitive` marker fix from commit `fac4ed2`, the
   offset returns 2. So `rvals[3]` is the fn-name slot. But
   if the marker happens to be absent for this specific
   library method (because pyc's analyzer specialized away
   the outer `__pyc_primitive__` wrapper), the offset would
   shift and the fn name would be at a different index.

3. **`lvals.n == 0`.** When the result Var is dead-coded by
   the optimizer (the return value is only used inside the
   library code, never escapes), `pn->lvals.n` can be 0;
   `lower_send_c_call`'s `inst->lvals.add(dst)` then never
   fires, and the C_CALL emit's `lvals[0]->name` access
   produces a no-op result. (But `lvals.n == 0` doesn't
   explain an empty function body — the C_CALL inst would
   still be added.)

The first hypothesis is the most likely. Verify by adding a
debug print in `lower_send_c_call` (similar to the issue 018
investigation pattern) that dumps `pn->rvals[3]->sym`'s name
+ constant for any SEND that goes through it.

## Verification plan

When fixing:

1. The minimal repro
   (`x = [1] + [2,3,4,5,6]; print(x)`) prints
   `[1, 2, 3, 4, 5, 6]` under `IFA_LLVM_V2=1 -b`.
2. `_CG_f_1359_3`'s LLVM IR shows a call to `_CG_list_add`
   in its body.
3. `tests/list_concat.py`, `tests/list_or_concat.py` flip
   from FAIL to PASS in the v2 LLVM pyc-suite ratchet.
4. C-backend 74/0 unchanged.

## What this unblocks

- `tests/list_concat.py`, `tests/list_or_concat.py`,
  `tests/list_multiply.py` (and probably `list_comprehension.py`
  via the same pattern in `__iadd__`). Pyc-suite v2 LLVM
  should gain 2-4 tests.
- All four sub-commits of issue 019's plan
  (`CG_IR_019_DESIGN.md`) become live: the allocation side
  works (E.1-E.3) but the consumer side (this issue) was the
  bottleneck. Closing 020 makes the issue 019 work pay off.
- A library-side audit of how many `__pyc_c_call__`
  invocations use the type-marker idiom — every one of them
  is a candidate for the same gap.
