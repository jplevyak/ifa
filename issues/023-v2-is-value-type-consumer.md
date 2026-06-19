# Issue 023: v2 LLVM has no `is_value_type` consumer for Type_RECORD

**Status:** **partial (Stage 1 landed June 2026)**.

**Stage 1 done:**
`build_struct_type` now propagates `is_heap_aggregate = !s->is_value_type`
into the CGv2Type lattice (vectors always stay heap-allocated).
`CG2_ALLOC` emit (`cg_ir_v2_emit_llvm.cc:488`) now consults the
bit and emits a stack alloca in the function's entry block
when `!is_heap_aggregate` AND the alloc's lvalue doesn't escape
the current function.  Escape is detected conservatively by
scanning all instructions for non-target-position uses of the
lvalue (any rval other than position-0 of FIELD/INDEX
LOAD/STORE / LEN / SIZEOF_ELEMENT / CLONE-as-proto).

**Stage 2 remaining (the actual user-visible win):**
Today's pyc IR shape wraps every `__new__` constructor in a
helper that allocates a fresh struct and returns the ptr.  Any
call to `Point(x, y)` lands in the wrapper, which means the
alloc's lvalue is in the wrapper's `CG2_RET` rvals — escape
positive, fallback to GC_malloc.  The result: the alloca path
is wired and correct, but **dormant** on pyc's current
codegen: zero allocas fire for `@pyc_struct` instances in the
pyc test suite today.

To unlock Stage 2, the constructor wrappers need either
- (a) inlining at the call site (issue 022 territory — once
  iterative inlining lands, small `__new__` wrappers fold into
  the caller and the alloca site moves to the caller's frame
  where escape is determined by the caller's own uses), or
- (b) sret-style calling convention rework so the caller
  pre-alloca's the slot and passes its ptr as an out arg; the
  wrapper writes through the slot ptr instead of allocating.
  This is more invasive but doesn't depend on inlining.

**Affects:** `ifa/codegen/cg_normalize_v2.cc:build_struct_type`
(Stage 1 done), plus the cascade through `lower_send_alloc`,
the wrapper IR shape, and CG2_CALL's argument / return ABI
(Stage 2).
**Surfaced while:** closing
[`issues/015-pyc-pod-records-no-frontend-hook.md`](../../issues/closed/015-pyc-pod-records-no-frontend-hook.md)
— pyc's `@pyc_struct` decorator now sets `Sym::is_value_type`
on user RECORDs.  Stage 1 closes the gap between the bit and
the codegen layer.  Stage 2 makes the bit observable.
**Related:**
[`closed/014-llvm-construction-flow-to-slots.md`](closed/014-llvm-construction-flow-to-slots.md)
(v1 LLVM had a `getLLVMVarType` POD-record override that was
retired alongside v1 LLVM),
[`022-iterative-inlining.md`](022-iterative-inlining.md) (the
inliner-side dependency for Stage 2 path (a)).

## Symptom

`@pyc_struct class Point: ...` from
`tests/pyc_struct_basic.py` produces the same LLVM IR shape
as a plain `class Point: ...` would: `Point` instances are
`ptr` SSA values that point at a heap struct allocated via
`GC_malloc`. There's no observable code-generation difference
between value-type and reference-type RECORDs today.

For comparison, the IFA-level structural-type hierarchy
computation (`ifa/if1/ast.cc:338`) already special-cases
`Type_RECORD && is_value_type`, so the bit is meaningful at
the analysis layer; only the codegen layer ignores it.

## Suspected root cause

`cg_normalize_v2.cc:build_type` for `Type_RECORD` always wraps
the struct in `CG2T_PTR`:

```cpp
if (s->type_kind == Type_RECORD) {
  CGv2Type *struct_t = build_struct_type(c, s);
  CGv2Type *ptr_t = new CGv2Type();
  ...
  ptr_t->kind = CG2T_PTR;
  ptr_t->element = struct_t;
  c.sym_to_type.put(s, ptr_t);
  return ptr_t;
}
```

For `is_value_type`, this should return `struct_t` directly,
giving the type a CG2T_STRUCT (or new CG2T_VALUE_STRUCT) shape
that LLVM emits as a struct SSA value. The cascade then needs:

1. **`lower_send_alloc`** — `CG2_ALLOC` (heap) → either
   `CG2_ALLOCA` (stack) for locals, or skip-the-alloc-entirely
   for fresh SSA struct values. The `__init__` call needs a
   target slot regardless, so `CG2_ALLOCA` + GEP-based
   FIELD_STORE is the cleanest shape.
2. **`lower_send_period` / field access** — `CG2_FIELD_LOAD`
   on a struct SSA value (not a ptr) needs LLVM `extractvalue`
   instead of `getelementptr` + `load`. Same for `FIELD_STORE`
   → `insertvalue`. The simpler alternative is to alloca the
   struct upfront and keep using GEP+load/store; LLVM's
   `mem2reg` / SROA passes lower the alloca to SSA
   automatically.
3. **`CG2_CALL` arg/return ABI** — passing a struct by value
   to a function with a struct formal needs the LLVM type to
   match (`%struct.Point` not `ptr`). LLVM's calling convention
   handles by-value struct args (via `byval` attribute or
   in-register, depending on size); the emit just needs to
   produce the correct call instruction.
4. **`__new__` / `__init__` cooperation** — pyc emits
   constructor calls separately from the alloc. For value
   types, `__init__` writes through a slot the caller
   provides; the result is the slot's contents (loaded once
   __init__ returns). Easiest shape: alloca-and-init,
   matching today's heap-alloc-and-init pattern but with the
   slot living on the caller's stack.

## Verification plan

When fixing:

1. `tests/pyc_struct_basic.py` produces IR where `Point`'s
   constructor allocates via `alloca` (or returns an SSA
   struct) rather than `call ptr @GC_malloc`.
2. A new fixture `tests/pyc_struct_byval_call.py` confirms
   passing a `@pyc_struct` instance to a function works
   correctly (the callee reads the same field values the
   caller wrote).
3. Pyc-suite v2 LLVM ratchet rises by 0 (no regressions) and
   the `pyc_struct_basic` test continues to pass.
4. C-backend behavior unchanged (no `is_value_type` consumer
   on that side either; parallel work tracked separately).

## What this unblocks

- The promised `@pyc_struct` value-type semantics — stack-on-the
  -wire structs, no GC pressure for small POD captures.
- Closure-capture optimization for POD captures (the issue 015
  motivation).
- Tighter pyc-↔-C interop via `__pyc_c_call__` for struct args
  (today the helper has to receive `ptr` and dereference; with
  value semantics it can take the struct directly).
- A path to C-backend parity (issue 015's "C-backend parity
  question" follow-on — `cg_string` would honour
  `is_value_type` similarly).

## Related

- [`closed/014-llvm-construction-flow-to-slots.md`](closed/014-llvm-construction-flow-to-slots.md)
  — v1 LLVM had a `getLLVMVarType` POD-record override that
  this issue would restore in v2 form.
- `ifa/codegen/cg_normalize_v2.cc:163` (`build_type`) — the
  primary modification site.
- `ifa/codegen/cg_normalize_v2.cc:800` (`lower_send_alloc`)
  — the alloc-cascade site.
- `python_ifa_build_if1.cc` (`@pyc_struct` decorator handling)
  — the frontend feeder that puts `is_value_type` on the Sym.
