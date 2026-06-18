# Issue 014: LLVM backend — heap-object construction doesn't store into the variable slot

**Status:** **closed June 2026 (obsolete — v1 LLVM retired).**
v2 LLVM passes the issue's `/tmp/construct3.py` reproducer
cleanly because it bypasses the v1 `translate_pn` gating
condition entirely (v2 emits CG2_C_CALL into the destination
slot directly via lower_send_clone / lower_send_alloc).  With
v1 LLVM removed (commits `c4a9475` Stage 1, `41535cc` Stage 2),
the gating site this issue's fix would have touched no longer
exists.  v2's `--strict-verify` mode catches the same class of
"undef self to constructor" bug as a verification failure (see
`pyc_runtime.c` / `cg_normalize_v2.cc`).
**Affects (historical):** `ifa/codegen/llvm.cc` (setLLVMValue,
deleted), `ifa/codegen/llvm_codegen.cc` (translate_code_move,
deleted), the `__new__` / `_CG_prim_new` LLVM emission paths.

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

## Root-cause investigation (June 2026)

Done as part of CG_IR plan Phase 0 §5.3 (IF1 construction-flow
walk).

**Outcome: (a)** — the Code_MOVE binding the constructor result
to the destination slot IS present in the IF1 stream. The C
backend faithfully emits it; the LLVM backend was dropping it
because of overly aggressive liveness gating.

**Reproducer.** `/tmp/construct3.py`:

```python
class A:
  def __init__(self):
    self.x = 2
  def hello(self):
    print(self.x)
y = A()
y.x = 5
y.hello()
```

The `hello()` call defeats the constant-folding path (cf. the
hypothesis in §"Root cause" above). The C backend's
`__main__` then emits:

```c
/* 2162 */ g2 = _CG_prim_new(_CG_ps3363);            // SEND @primitive @new
t1 = _CG_f_2163_1/*A::___init___*/(/* 2162 */ g2);   // SEND #__init__
t0 = _CG_f_3284_2/*__new__*/();                      // SEND %__new__
/* y 2188 */ g1 = t0;                                // *** MOVE %t0 %y ***
((_CG_ps3367)/* y 2188 */ g1)->e2 = (_CG_int64)5;    // setter
_CG_f_2177_0/*A::hello*/(/* y 2188 */ g1);           // hello()
```

The line `g1 = t0` IS a Code_MOVE PNode with `rvals[0] = %t0`
(the temp that received `__new__`'s reply) and `lvals[0] = %y`
(the module-level global). It is present in the IF1 — the C
backend's `translate_code_move` emits it as `g1 = t0`. The LLVM
backend was filtering it out because the gating condition in
`translate_pn` looked at `pn->live` without making MOVE
unconditional.

**Confirmation from the IF1 dump.** Running
`pyc -x 1 -v construct3.py` writes `if1.code`. The dump's
`(sym %y)` is declared but only writers/readers in the dump are
for the module-init path; the dump's `;; funs` section does not
include the top-level `__main__` because the writer in
`testing/write_ir.cc:235` iterates `p->allclosures` and the
program entry isn't materialized as an `allclosures` member at
that point. The C output is the authoritative window into what
the IF1 stream contains for `__main__`.

**Implication for the plan.**
- **Phase 2.4** ("emit all Code_MOVE unconditionally during
  cg_normalize"): sufficient and necessary. This is the patch
  that closes this issue.
- **Phase 2.5** (peephole inserting a `CG_STORE` when MOVE is
  missing): NOT needed for the construction-flow case. Keep it
  scoped to the SEND-with-immediate-lvalue cases (Code_SEND
  whose `lvals[0]` is a global slot) if any of those come up,
  but do not block on it for closing this issue.

**Why the SEGV happens despite the MOVE being present.** Before
the gating change, `translate_pn` in `llvm_codegen.cc`
short-circuited Code_MOVE nodes when `!pn->live || !pn->fa_live`
on the assumption that a "dead" MOVE was DCE'd. But the
construction-flow MOVE has `pn->fa_live` true and `pn->live`
false in some IF1 shapes (it's the lvalue's first definition
and the lvalue is module-scoped, so the SSU pass marks it
non-live within the local function while flow analysis keeps
fa_live true for the cross-function visibility). The fix is to
treat all Code_MOVE as emit-unconditionally during normalization
— which is exactly what the c-backend does (its MOVE handler
has no liveness gate at all).

See also: `ifa/LIVENESS.md` (§"The three gates" and §"Common
pitfalls — the `fa_live` is more accurate trap") which
documents the contract `cg_normalize::pn_should_emit(pn)` will
implement.

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
