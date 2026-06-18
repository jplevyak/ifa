# Issue 017: iterator construction passes `undef` self to `__new__`

**Status:** **closed June 2026 (obsolete — v1 LLVM retired).**
The "IF1 path" and "CG_IR path" both refer to v1 LLVM code
paths that were deleted with the v1 retirement (commits
`c4a9475` Stage 1, `41535cc` Stage 2 of issue 014).  v2 LLVM
goes through `cg_normalize_v2` + `cg_v2_emit_llvm_module`,
which constructs iterators via CG2_ALLOC + explicit FIELD_STORE
of the freshly-allocated pointer into the destination slot
(see `lower_send_period` and `lower_send_clone` in
`cg_normalize_v2.cc`).  The 80/0 v2 LLVM pass rate confirms the
for-loop iterator construction works; `PYC_STRICT_VERIFY=1`
would surface any new "undef self to call" regression as a
verification failure.
**Affects (historical):** v1 LLVM backend, both IF1 path
(`llvm_codegen.cc:translate_pn`, deleted) and CG_IR path
(`emit_cg.cc:emit_cg_inst`, deleted).

## Symptom

For-loop iterator construction in `__main__` emits IR like:

```llvm
%0 = call ptr @GC_malloc(i64 56)            ; allocate iterator
%1 = call ptr @_CG_f_1682_4(ptr undef), !dbg !167  ; __new__(undef)
store ptr %1, ptr %local_var11, align 8
```

The GC_malloc result `%0` is dropped on the floor. `__new__`
(`_CG_f_1682_4`) is then called with `ptr undef` instead of the
freshly-allocated pointer. The function `__new__` does some
setup (clone + __init__) but operates on `undef`, so reads of
the result later see `undef` chains.

Net effect on `for_range_from_zero.py` (`for x in range(10):
print(x)`):

- The iterator is constructed with undef self.
- `range::__init__(iter, 10)` runs but writes to undef.
- `range::__pyc_more__(iter)` reads i and j from the iterator's
  range struct slot — both come back as the struct's
  default-initialized values (i=0, j=0 from the class
  attributes).
- `i < j` = `0 < 0` = false. Loop body never executes.

Test produces empty output.

## Why the CG_IR path inherits this

Both the IF1 emitter (`translate_code_send` → write_llvm_prim
P_prim_new) and the CG_IR emitter (emit_cg_inst CG_ALLOC, with
back-translation fallback) end up routing through the same
per-prim emitter (`P_prim_new` in `llvm_primitives.cc:603`).
That emitter does:

```cpp
llvm::Value *struct_ptr = Builder->CreateCall(
    gcMallocFunc, llvm::ConstantInt::get(...);
setLLVMValue(res_var, struct_ptr, ifa_fun);
```

`setLLVMValue` is the SSA-style cache RENAME — it stores the
malloc pointer as `res_var`'s cached value. Subsequent reads
via `getLLVMValue(res_var)` return the cached pointer
directly.

**But** the `__new__` call's first argument is a DIFFERENT Var
than `res_var`. The `__new__` wrapper takes the result of
P_prim_clone (which is itself derived from the new instance),
but the caller has dropped that result somewhere along the
chain. The undef comes from a Var with no cached value.

The C backend works because cg.cc emits an explicit
`t1 = _CG_prim_new(...)` assignment that doesn't depend on the
SSA cache — the C variable directly holds the malloc pointer
and the next line passes it.

## Field-index resolution NOT the bug

Investigation during issue 016 partial close found the LLVM
struct type for `range` is CORRECT — `{ i64, i64, ptr, ptr,
i64, ptr, ptr }` matching the `has` order (s, i, __init__,
__pyc_more__, j, __next__, __iter__). `__pyc_more__` correctly
reads i (field 1) and j (field 4) and compares `slt i64`.
The IR is correct; the problem is that i and j are read as
their CLASS DEFAULTS (0 and 0) because the iterator itself
was never properly initialized — it was constructed with
undef self.

## Why the IF1 path "works" (infinite-loop instead of empty)

The baseline (IF1 path) for_range_from_zero prints 0…391M+
before timeout — i.e. the loop runs forever. That's because
the IF1 path's `__pyc_more__` somehow reads garbage from
uninitialized memory that happens to satisfy `i < j` (since
heap memory after GC_malloc isn't zeroed, depending on
allocation reuse, the comparison can return true).

The CG_IR path makes the bug more consistent: the slot is
zeroed via the alloca + memcpy path, so i and j are both 0,
and `0 < 0` = false. Loop terminates after zero iterations.

So the same underlying bug surfaces differently depending on
the codegen path. Neither version actually iterates correctly.

## Investigation findings (2026-06-13)

The bug is BACKEND-LOCALIZED to the Var-cache management in
the LLVM emitter, not in cg_normalize. Concrete findings:

**IF1 path emits the correct call.** Without the CG_IR swap,
`translate_pnodes_worklist` produces:

```llvm
%0 = call ptr @GC_malloc(i64 56), !dbg !150
%1 = call ptr @_CG_f_1682_4(ptr %0), !dbg !151   ; ← %0 passed correctly
```

**CG_IR path emits the broken call.** With the swap:

```llvm
%0 = call ptr @GC_malloc(i64 56)
%1 = call ptr @_CG_f_1682_4(ptr undef), !dbg !167  ; ← undef
```

**Root cause is the Var cache.** In the IF1 path,
`write_llvm_prim P_prim_new` (llvm_primitives.cc:603) does:

```cpp
llvm::Value *struct_ptr = Builder->CreateCall(gcMallocFunc, size);
setLLVMValue(res_var, struct_ptr, ifa_fun);   // ← updates Var cache
```

The downstream `_CG_f_1682_4` call (for __init__) resolves its
first arg via `getLLVMValue(arg_var)` which hits the cache and
returns the malloc pointer.

The CG_IR path's `emit_cg_inst CG_ALLOC` direct emission does
`Builder->CreateCall(gc_malloc...) + Builder->CreateStore(p,
slot_ptr)`. This writes the malloc result to the SLOT's
pointer (an AllocaInst or GlobalVariable) but does NOT update
the Var cache. Subsequent `getLLVMValue(arg_var)` reads return
`undef` (the cache is empty, the recovery path falls back to
undef when constant-recovery can't reconstruct the value).

**Why routing through write_llvm_prim doesn't help.** Two
attempted fixes both caused verifyModule to fail with
"Referring to an instruction in another function!":

1. Adding `setLLVMValue(res_var, p, source_fun)` after the
   direct CG_ALLOC emission — when the var's cache contains a
   GlobalVariable/AllocaInst, setLLVMValue emits CreateStore.
   The cross-function leak comes when the cached value belongs
   to a function whose Builder is no longer current.

2. Routing CG_ALLOC through write_llvm_prim P_prim_new (via
   inst->prim hint + fallback path) — same leak. The
   write_llvm_prim path's setLLVMValue + getLLVMValue cache
   thread across the FA's per-function dispatch in a way that
   the IF1 path's worklist BFS happens to avoid by accident.

**The structural resolution lives in getLLVMValue/setLLVMValue's
scope tracking** (llvm.cc:1207-1271). The existing
scope-mismatch detection clears cache entries when the cached
Instruction's parent function isn't `cg_get_llvm(ifa_fun)`. The
clear-and-recover path's recovery isn't reliable for the
malloc-then-init pattern; it ends up returning undef when the
Var's defining instruction was emitted in a different function
context.

A proper fix would either:
- Make the Var cache function-aware (key by Var + Fun rather
  than just Var), OR
- Move construction-flow value flow off the Var cache and onto
  CGSlot (the explicit slot model) — which is exactly what
  CG_IR is supposed to be moving toward, but requires
  eliminating the IF1-path Var cache dependency on the LLVM
  emitter's per-prim emitters first.

For this session, no further code change. The structural
insights are recorded; the deeper fix requires upstream work
that's out of the CG_IR plan's scope.

## Update 2026-06-13 — confirmed structural via CG_IR_v2 audit

The CG_IR_v2 audit (`ifa/codegen/CG_IR_v2.md`) traced this
issue's root cause to CG_IR's biggest architectural miss: the
per-Var llvm::Value cache is program-scoped, but llvm::Value
identity is function-scoped. Three surgical fixes were
attempted post-audit:

1. **cg_set_llvm_value directly after CG_ALLOC direct
   emission** (bypassing setLLVMValue's store branches).
   Result: verifyModule rejects with cross-function
   instruction leak — overwriting the program-scoped cache
   with a function-local value contaminates other functions'
   reads.

2. **Branched cg_set_llvm_value / CreateStore** that only
   writes the cache when no AllocaInst/GlobalVariable is
   already there, and only emits CreateStore when the
   existing handle's function matches the current Builder's
   function. Result: still verifyModule failure — the
   contamination happens before the guard can detect it.

3. **Routing CG_ALLOC through write_llvm_prim P_prim_new**
   via the back-translation fallback (with `inst->prim`
   populated). Result: same verifyModule failure — the IF1
   path's setLLVMValue inside write_llvm_prim hits the same
   contamination, but the IF1 path's worklist BFS happens
   to avoid the cross-function read order; the CG_IR
   per-CGBlock iteration order exposes the bug.

The structural resolution is a **per-CGFun value cache**
(`Map<Var *, llvm::Value *>` on each CGFun), with
emit_cg_inst's `resolve_value` and back-translation seam
threading the CGFun through to write_llvm_prim. That's the
CG_IR_v2 design.

**Status**: documented, not implemented. Deferred to a v2
landing that's properly scoped (not a one-session attempt).
Issue 017 stays open with the understanding that closure
requires the per-CGFun cache.

## Proposed fix

The construction-flow chain is:
```
y = range(10)
  → SEND #range (some __init__-dispatcher / class call)
    → SEND @primitive @new range → tmp = GC_malloc(56)
    → SEND #__init__ tmp 10
    → MOVE tmp y
```

The `MOVE tmp y` should bind `tmp` (the malloc result, cached
in tmp's slot) to `y` (the iterator local). But the IR shows
that `y` (passed as `ptr undef` to __new__) wasn't getting the
malloc pointer. Either:

- The MOVE is being DCE'd inappropriately, OR
- The MOVE is emitted but writes to the wrong slot, OR
- The __new__ wrapper takes a Var that was never bound to tmp.

Need to inspect the IF1 cfg dump for `range(10)` to see where
the breakage actually is. The fix likely lives in the
analyzer's clone-and-init pattern handling, not in the LLVM
emitter itself.

## Verification plan

After the fix:
- `for_range_from_zero.py` prints 0..9 (issue 016 cohort
  unlocks).
- `for_over_range.py`, `for_over_list.py`, `for_over_tuple.py`
  pass.
- LLVM pyc-suite count rises by ~5.
- IR for `_CG_f_105_X` (main) shows `store ptr %0, ptr <iter
  slot>` immediately after GC_malloc, before the __new__
  call, and __new__ is called with the iterator pointer
  instead of undef.

## What this unblocks

- Issue 016 fully closes (the SSU formal-arg binding is
  already correct; the remaining for-loop failures all stem
  from this construction-flow gap).
- The CG_IR + per-prim back-translation path becomes
  semantically equivalent to the C backend for iterator
  patterns.

## Related

- Issue 014 (LLVM construction flow to slots) — closed by
  Phase 2.4's unconditional Code_MOVE emission, but THIS bug
  is a deeper variant: the MOVE itself is correctly emitted,
  but the Var being moved-into isn't the one __new__ reads.
- Issue 016 (SSU formal-arg binding) — partial close; this
  issue is what blocks full closure.
- `ifa/codegen/llvm_primitives.cc:603` (P_prim_new) — the
  per-prim emitter; works correctly in isolation, but its
  `setLLVMValue(res_var, ...)` is on a different Var than
  __new__'s self parameter.
- `ifa/codegen/cg.cc` `P_prim_new` (line ~320) — the C
  backend's parallel; emits `lhs = _CG_prim_new(type)` which
  works because lhs is the same Var used downstream.
