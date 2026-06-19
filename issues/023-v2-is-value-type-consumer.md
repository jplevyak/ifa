# Issue 023: v2 LLVM has no `is_value_type` consumer for Type_RECORD

**Status:** **partial (Stage 1 + Stage 2 (b) landed June 2026)**.
Stage 1: heap→stack promotion for non-escaping `@pyc_struct`
allocs (commit `a37d652`).  Stage 2 (b): sret-style calling
convention for value-type-returning functions.  Remaining work
is improving the per-function escape check (today it
conservatively bails on any cross-function pointer flow, which
keeps the heap fallback active for the common closure-bound
`self` pattern).

**Stage 1 done:**
`build_struct_type` now propagates `is_heap_aggregate = !s->is_value_type`
into the CGv2Type lattice (vectors always stay heap-allocated).
`CG2_ALLOC` emit (`cg_ir_v2_emit_llvm.cc:488`) now consults the
bit and emits a stack alloca in the function's entry block
when `!is_heap_aggregate` AND the alloc's lvalue doesn't escape
the current function.  Escape is detected conservatively by
`value_escapes_in_fun` — any rval other than position-0 of
FIELD/INDEX LOAD/STORE / LEN / SIZEOF_ELEMENT / CLONE-as-proto
counts as escape.

**Stage 2 (b) done — sret calling convention:**
When `CGv2Sig::ret` is CG2T_PTR-to-(value-type CG2T_STRUCT),
`build_fun_decl` now sets `is_sret=true` + `sret_struct=<elem>`
on the signature.  `to_llvm_fn_type` rewrites such signatures
to `void @f(ptr sret(struct), <user args...>)`; `declare_fun`
attaches the LLVM `StructRet(struct)` attribute to the implicit
first param.  Inside sret-returning bodies, the canonical
"alloc → fill → ret" pattern is routed: CG2_ALLOC / CG2_CLONE
of the sret target struct put_result through the sret slot
instead of allocating; CG2_RET emits `ret void` (with a fallback
memcpy if some other rval reaches the ret).  At call sites,
CG2_CALL detects `StructRet` on the callee's first param,
allocates the slot (alloca if the dst doesn't escape — same
`value_escapes_in_fun` check; GC_malloc otherwise), prepends it
to the args, and binds the dst CGv2Value to the slot ptr.

**Observable wins on `tests/pyc_struct_basic.py`:**
- The Point constructor wrapper (`_CG_f_3338_3`) now has **zero
  internal allocations** — it writes directly through `%sret`.
  Previously it did GC_malloc + memcpy(proto) + field stores +
  ret ptr.  Now: memcpy(proto → sret) + field stores → ret void.
- Caller alloca's the sret slot when the dst is local
  (e.g. `%v10 = alloca %Point.3465`) and GC_mallocs otherwise.
- Two sret-attributed function definitions; one alloca'd slot;
  remaining GC_mallocs are prototypes, closure structs, and
  Point slots whose dst escapes via global / closure capture.

**What's still on the table:**
The user-visible global-bound case (`p = Point(3, 4); print(p.x)`
at top level) still GC_mallocs the sret slot because `p` is
stored into the module global `@p`.  Same for any Point bound
into a closure-self slot — the escape check (correctly) treats
the closure's field-store as escape.  Loosening this requires
real escape analysis at the FA / CG_IR level, ideally
propagating across function calls.  That's a substantial
follow-on, but the sret ABI is now in place and ready to
benefit from any future loosening.

**Stage 2 (a) — inlining — independent value:**
Issue 022's iterative inliner is still worth landing, but no
longer for value-type purposes (Stage 2 (b) handles those
directly).  See [`022-iterative-inlining.md`](022-iterative-inlining.md);
its motivation is now strictly the FA-feedback / code-size
side of the ledger.

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
