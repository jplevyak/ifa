# Issue 016: LLVM backend — SSU formal-arg → renamed-local MOVEs are skipped

**Status:** partial fix landed (CG_IR_PLAN Phase 3.4 production
swap), full closure blocked on a separate field-index bug.

## Progress (2026-06-13)

CG_IR_PLAN Phase 3.4 production swap closes the structural
half of this issue:

- `range::__pyc_more__` now correctly emits `store ptr %self,
  ptr %self5` in the entry block (the SSU formal-arg binding).
- `__pyc_more__` correctly returns `ret i1 %0` (the comparison
  result) instead of `ret i1 undef`. Fixed by populating
  `CG_RET`'s rvals from `closer->rvals[3]` (the @reply SEND's
  return value) in cg_normalize.
- Entry blocks no longer degenerate to `unreachable`. Fixed by
  cg_normalize's implicit fall-through closer detection (any
  PNode whose cfg_succ leaves the block becomes a CG_BR
  closer) and emit_cg's CG_COND_BR fallback (when the
  condition Var can't be resolved, fall back to
  `ConstantInt::getTrue` instead of `unreachable`).
- LLVM pyc-suite: 38/37 (was 37/38 baseline) — 1 net
  improvement (`conditional.py` newly passes, others shifted
  failure modes).

## Blocked on: field-index resolution bug in mapStructType

The remaining failure for `for_range_from_zero.py` and the
for-loop cohort is NOT issue 016's SSU binding — that's now
correctly emitted. The actual blocker is in the LLVM struct
type layout for `range`:

```
;; IF1 sees range's has = (s_6 i_4 __iter__ __next__ j_3 __init__ __pyc_more__)
;; expected struct: 7 fields where field 1=i, field 4=j

%range = type { ptr, ptr, i64, ptr, ptr, i64, i64 }
;; actual: field 1 is ptr (should be i64), field 4 is ptr (should be i64),
;; field 2 is i64 (should be ptr __iter__), etc.
```

`__pyc_more__`'s IR ends up comparing field 6 with field 2
(both i64 in the struct) — but those should be METHOD
pointers, not integers. The actual `i` (field 1) and `j`
(field 4) are typed as `ptr` so the comparison silently uses
the wrong fields.

This is upstream of `cg_normalize` — it's in
`getLLVMType::mapStructType` or in how the type-string pass
populates `field_sym->type` for record-with-methods. Both the
IF1 path and the CG_IR path produce the same wrong struct
layout.

**Action**: file a follow-up issue for the field-index /
struct-layout bug; the CG_IR path can't fix it without an
upstream fix to mapStructType or to how the analyzer
populates field types on cloned record types.

## Original report (pre-fix):
**Affects:** `ifa/codegen/llvm_codegen.cc:translatePNode` (the
liveness gate), all functions that have SSU-renamed `self`
references — iterators in particular (`range::__pyc_more__`,
`__next__`, etc.).
**Surfaced while:** completing for-loop / iterator emission
(commit f5b6200 brought for-loop bodies back; this issue is the
next layer).

## Symptom

For-loops compile and run, but iteration never terminates. For
example `for i in range(3): print(i)` prints `0, 1, 2, 3, 4, 5,
…` forever instead of stopping at 2. Affected tests
(all `EXEC-TIMEOUT` or wrong-count failures):

- `for_range_from_zero.py` (prints 0…391M+ before timeout)
- `for_over_range.py`
- `for_over_list.py`
- `for_over_tuple.py`
- `break_continue.py` (incorrect iteration count)

## Why the loop never terminates

`range::__pyc_more__(self)` is supposed to return `self.i <
self.j`. The LLVM IR shows:

```llvm
define internal i1 @_CG_f_1714_1(ptr %self) !dbg !193 {
entry:
  %self5 = alloca ptr, align 8         ; SSU-renamed `self`, NEVER stored to
  ...
  br label %label_261
label_261:
  %1 = getelementptr %range, ptr %self5, i32 0, i32 4   ; %self5 = garbage
  %2 = load i64, ptr %1                  ; loads garbage
  ...
}
```

`%self5` is allocated for the SSU-renamed `self` Var, but the
formal argument `%self` is never stored into it. The GEP/load
chain reads uninitialized stack memory; the resulting i/j
comparison returns true effectively at random, and the loop
keeps spinning.

The C backend produces the equivalent statements explicitly:

```c
t11 = a1;     // SSU MOVE: formal arg → renamed local
t9 = t11;     // SSU MOVE: rename chain
t5 = t9->e5;  // read i (now well-defined)
t6 = t9->e1;  // read j
```

## Why the LLVM backend misses it

The IF1 emits these renamings as `Code_MOVE` PNodes. They're
marked `live=0` (DCE strips the SSU alias as unused — the rename
is "dead code" in the DCE sense) but `fa_live=1` (the FA needs
the binding for downstream Vars to have a value).

The C backend uses the same `live && fa_live` gate for per-kind
emission (`cg.cc:586`) but runs phi/phy materialization
**outside** that gate (`cg.cc:648`):

```cpp
if (n->live && n->fa_live) switch (n->code->kind) {
  case Code_MOVE: simple_move(...); break;
  ...
}
switch (n->code->kind) {            // <-- outside the live gate
  case Code_IF: ...
  case Code_GOTO: ...
  default: do_phi_nodes for succ;   // <-- always emits SSU bindings
}
```

`do_phi_nodes` calls `simple_move` for every phi entry on the
successor, regardless of whether the current PNode is live. So
the C backend's SSU MOVEs land either through the live-gated
`Code_MOVE` case or through the unconditional `do_phi_nodes`
side door.

The LLVM backend's `translatePNode` returns early on `!is_live`
and routes the per-Code_kind handlers to `translate_code_*`
helpers. `translate_code_if` and `translate_code_goto` do call
`do_phi_nodes` for their successors — but only when the PNode
itself is live. There's no unconditional phi/phy pass for
non-live PNodes.

## Proposed fix

Restructure `translatePNode` so phi/phy materialization runs
**unconditionally** (matching `cg.cc:648`), with only the per-kind
emission gated:

```cpp
void translatePNode(PNode *pn, Fun *ifa_fun) {
  if (!pn || !pn->code) return;
  // ...
  bool is_live = pn->live && pn->fa_live;
  if (is_live) {
    switch (pn->code->kind) {
      case Code_LABEL: translate_code_label(pn, ifa_fun); break;
      case Code_MOVE:  translate_code_move(pn, ifa_fun); break;
      case Code_SEND:  translate_code_send(pn, ifa_fun); break;
      case Code_IF:    translate_code_if(pn, ifa_fun);   break;
      case Code_GOTO:  translate_code_goto(pn, ifa_fun); break;
    }
  } else {
    // Out-of-gate work: phi/phy materialization for successors,
    // dead-reply special case for SENDs, label-block setup so
    // worklist successors find their basic block. Mirror the
    // unconditional second switch in cg.cc:604-650.
    handle_non_live_pnode(pn, ifa_fun);
  }
}
```

`handle_non_live_pnode` walks `pn->cfg_succ` and emits
`do_phi_nodes` / `do_phy_nodes` for each — same primitives the
live path uses, just without the per-kind emission.

The factoring is mostly mechanical (the existing
`translate_code_*` helpers already split into phi/phy + per-kind
emission internally; pulling the phi/phy half out is a
straight refactor). The risk is around `Code_LABEL` (which needs
its BB created either way so subsequent PNodes know where to
land) and `Code_IF` / `Code_GOTO` (which need their terminators
emitted unconditionally if the entry-into-this-block was real).

## Verification plan

After the fix:

- `for_range_from_zero.py`, `for_over_range.py`,
  `for_over_list.py`, `for_over_tuple.py` all pass (loop
  terminates at the expected count).
- `break_continue.py` produces the documented sequence.
- The LLVM IR for `range::__pyc_more__` shows a `store ptr
  %self, ptr %self5` early in entry, before the field GEPs.
- C backend remains 74/1/0/2; codegen-c and codegen-llvm
  fixtures unchanged.
- LLVM pyc-suite pass count rises by ~5 (the iterator-bound
  cohort).

## What fixing this unblocks

- For-loop / iterator parity with the C backend.
- The deferred Step 2 of the getLLVMVarType migration
  (`P_prim_period` / `setter` / `index_object` special-case
  removal) probably becomes safer to land — the construction
  flow from issue 014 and the SSU binding from this issue
  share the "slot needs a store the LLVM backend isn't
  emitting" failure mode, and fixing the phi/phy pass might
  cover both.

## Related

- `ifa/codegen/llvm_codegen.cc:translatePNode` — the early
  `!is_live` return.
- `ifa/codegen/cg.cc:586-650` — the C backend's two-stage
  emission (live-gated per-kind, ungated phi/phy).
- `ifa/issues/014-llvm-construction-flow-to-slots.md` — the
  related "slot not stored to" pattern for heap-aggregate
  globals.
- Commit f5b6200 — restored the strict `live && fa_live` gate
  that made this issue visible (it had previously been
  shadowed by the looser `fa_live`-only gate skipping the body
  PNodes entirely).
