# Issue 014: LLVM backend — heap-object construction doesn't store into the variable slot

**Status:** open.
**Affects:** `ifa/codegen/llvm.cc` (setLLVMValue), `ifa/codegen/llvm_codegen.cc`
(translate_code_move and the per-Code_kind handlers), the
`__new__` / `_CG_prim_new` LLVM emission paths. Surfaced while
landing the `getLLVMVarType` migration
(`ifa/codegen/CODEGEN_PLAN.md` references the partial fix in
commit 06bec4a).

## Symptom

After the getLLVMVarType migration (heap aggregates declared as
`ptr` slots in globals, locals, args, returns), the LLVM backend
emits the construction sequence for `y = A()` like this:

```llvm
%0 = call ptr @GC_malloc(i64 8), !dbg ...   ; allocate A instance
call void @_CG_f_2164_0(), !dbg ...         ; class-body init (no args)
call void @__new__219(), !dbg ...           ; __new__ — returns void, no args
%y.load = load ptr, ptr @y, align 8         ; @y is still `ptr null`
%9 = getelementptr %A, ptr %y.load, i32 0, i32 0
%10 = load i64, ptr %9, align 4             ; SEGV — y.load is null
```

The `GC_malloc` result is never connected to `@y`. The same
shape appears in `tuple_mixed_types.py` (the tuple Vars
`%local_var9` etc. that prints read from are never initialized
from `@a`).

## Why this matters

The full `getLLVMVarType` migration (Step 2 of the planned
multi-step fix) would have `P_prim_period`'s `obj_ptr` come from
`getLLVMValue(obj_var)` (which loads the slot to get the
ptr-typed value). But because the slot is *never written*, the
load returns the zeroinit `null` and codegen SEGVs. So Step 2
landed only as a TODO marker in `llvm_primitives.cc` (the
alloca/global "use directly" special-case stayed); it can drop
once this issue is fixed.

## Root cause (hypothesis)

Two distinct paths drop the result:

1. **`__new__` returns void.** `createFunction` decides the
   return type from `ifa_fun->rets[0]->type` — but the LLVM
   `__new__` is being declared with no return type instead of a
   pointer to the new instance. Either the IFA's `__new__`
   function genuinely has no return Var (in which case the
   instance pointer can't escape), or `determine_return_type`
   misses it.

2. **No MOVE PNode connects the constructor result to `y`.**
   The C backend constant-folds `y.x` to `2` and elides the
   construction entirely; the LLVM backend faithfully emits the
   IFA's PNode list which (apparently) has a SEND-without-lval
   for `__new__` and no MOVE PNode binding its result to `y`.

The C backend output for `class_attributes.py` confirms (1):
the C-side `__main__` doesn't reference `y` at all because the
analyzer figured out `y.x == 2` statically. So the LLVM backend
is generating more "literal" code than the C path needs, and
this construction gap shows.

## Proposed fix

Three options, ranked by scope:

**Option A** — Add a `P_prim_new` LLVM emitter that:
- Allocates via `GC_malloc(sizeof(struct))`.
- Stores the pointer into `lvals[0]`'s slot.
- Calls `__init__` / `__new__` with the new pointer as `self`.

This treats `_CG_prim_new` as a structural primitive the LLVM
backend recognizes, parallel to how the C backend's
`P_prim_new` emits `_CG_prim_new(<type>)` (which is a macro that
expands to `(T*)GC_malloc(sizeof(*T))`).

The current LLVM `P_prim_new` (`llvm_primitives.cc:373`) does
the GC_malloc + `setLLVMValue` already — what's missing is that
the SEND for "call `__new__(self)`" decomposes into
`P_prim_new` + a method call, and the LLVM backend may be
splitting the two in a way that loses the result.

**Option B** — Constant-fold harder in IFA so the LLVM backend
inherits the same simplification the C backend gets. The C
backend doesn't have to handle this case because `y.x = 2` is
folded upstream. Bringing the same optimization to bear before
LLVM codegen would let it skip the construction too.

**Option C** — Make `setLLVMValue` work harder when binding to
a slot whose declared type is `ptr` and the incoming val is
`ptr`: emit the store unconditionally instead of caching. The
existing AllocaInst + GlobalVariable cases already do this
(`llvm.cc:1336-1339`); the gap is when a MOVE PNode's lval is
neither an alloca-cached Var nor a global-cached Var, but
something the IFA produced via `set_add` etc.

## Verification plan

After the fix:
- `tuple_mixed_types.py` and `string_format.py` no longer SEGV.
- `class_attributes.py`, `dynamic_attr.py`, and the
  `method_alias_*` family pass end-to-end.
- The `P_prim_period` special-case in `llvm_primitives.cc`
  ("use alloca/global directly, skip the load") can drop in
  favor of just calling `getLLVMValue` — confirmed by
  `make test-ir` staying green.
- `make USE_LLVM=1 ./test_pyc` pass count rises measurably (the
  ~5 tests in the regressed cohort).

## What fixing this unblocks

- Step 2 of the getLLVMVarType migration (the
  `P_prim_period/setter/index_object` cleanup) can land cleanly.
- The "Call parameter type does not match" cluster that
  `tuple_mixed_types.py` newly exposed has its own follow-up
  bound to landing this fix.
- `CODEGEN_LLVM.md` §14.5 (runtime helpers aren't linked) is
  partly unblocked: with construction working, the next
  bottleneck moves to runtime-helper linking.

## Related

- `ifa/codegen/llvm.cc` `setLLVMValue` (commit 087075d added
  GlobalVariable handling).
- `ifa/codegen/llvm.cc` `getLLVMVarType` (commit 06bec4a — this
  session's Step 1).
- `ifa/codegen/llvm_primitives.cc` `P_prim_period` /
  `P_prim_setter` / `P_prim_index_object` — the special-case
  this issue's fix would let drop.
- `ifa/codegen/CODEGEN_PLAN.md` §3.5 — the LLVM-backend parity
  gap, which closing this issue closes by ~5 tests.
