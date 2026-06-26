# LLVM Direct Emission Plan

This document outlines the execution plan for migrating the LLVM backend to emit *directly* from the `IF1` core data structures.

## Architectural Goal

The primary goal is to abandon intermediate codegen representations (both materialized `CGv2Program` and virtual `CGView` layers) which proved to be lossy and unnecessarily complex (stalling at 42/100 parity). 

Instead, the LLVM backend will adopt the proven, 99/0-parity architecture of the C backend (`cg.cc`):
- No intermediate IR.
- DFS over `IF1` PNodes via `cfg_succ`.
- Dispatch directly on `pn->prim` to emit LLVM IR.
- Type info comes from lazily mapping `Var->type` + `Sym->has[]` to an `llvm::Type *`.
- Operand identity comes from a per-Fun `Var * → llvm::Value *` map.

This model cuts the backend LOC by ~50%, eliminates hundreds of thousands of intermediate allocations per compile, and removes the semantic translation gap.

## Implementation Checklist

The implementation proceeds by bootstrapping a skeleton emitter and incrementally porting primitives, using the test suite to drive parity against the C baseline.

### Phase R.1 — Emitter Skeleton & Core Structure

- [x] **R.1.1** New file `codegen/cg_emit_llvm.cc` (formerly `cg_view_emit_llvm.cc`). Public entry: `bool cg_emit_llvm(FA *fa, Fun *main_fun)`. Defines the lazy type cache (`sym_to_llvm_type` / `sym_to_llvm_struct`).
- [x] **R.1.2** Per-Fun signature `build_fun_signature(ctx, f)`. Maps `IF1` positional args to `llvm::Argument`.
- [ ] **R.1.2.1** Sret-rewrite for value-type RECORD returns.
- [x] **R.1.3** Per-Fun body emission driver. Pre-pass `discover_blocks(ctx, f)` BFS over `cfg_succ`, followed by DFS `emit_pnode` dispatch.
- [x] **R.1.4** `Code_SEND` skeleton and prim dispatcher.
- [x] **R.1.4 wiring**. Wire the direct emitter into `llvm.cc` and allow selection.
- [ ] **R.1.5** Initial prim cases: `P_prim_reply`, `P_prim_isinstance`, `P_prim_is`.
- [ ] **R.1.6** Per-Fun `Var * → llvm::Value *` map. Locals get an `AllocaInst`.
- [ ] **R.1.7** Type cache completion (`Sym * → llvm::Type *`).

### Phase R.2 — Primitive Ports

Port each primitive's emission logic from `cg.cc` directly to `llvm_primitives.cc` / `cg_emit_llvm.cc`.

- [x] **R.2.1** `P_prim_period` handler (`emit_send_period`).
- [x] **R.2.2** `P_prim_setter` handler (`emit_send_setter`).
- [x] **R.2.3** `P_prim_make` handler (`emit_send_make`).
- [x] **R.2.4 / R.2.5** Arithmetic & comparisons (`emit_send_binop`).
- [x] **R.2.6** `P_prim_index_object` load (`emit_send_index_load`).
- [x] **R.2.7** `P_prim_set_index_object` store (`emit_send_index_store`).
- [x] **R.2.8** `P_prim_sizeof` / `P_prim_sizeof_element` (`emit_send_sizeof`).
- [ ] **R.2.9** `P_prim_len`.
- [ ] **R.2.10** `P_prim_clone` (vector form).
- [x] **R.2.11** `P_prim_primitive` handler (`emit_send_primitive`).
- [x] **R.2.12** `emit_send_call` MPosition-aware routing.

### Phase R.3 — Close Parity Gap

Ensure the new emitter hits the 99/0 pass rate by porting remaining C-side workarounds.

- [x] **`resolve_union_receiver`** — Ported as part of R.2.1.
- [x] **Voidish-arg cast** — Handled implicitly.
- [x] **String constants** — `value_for_var` handles pyc-layout string globals.
- [x] **SSU phi/phy MOVE emission**.
- [x] **Forward-declare all functions before bodies**.
- [x] **Module-level globals**.
- [x] **Class-prototype init prelude**.
- [x] **P_prim_new** handler.
- [x] **P_prim_clone** (non-vector) handler.
- [ ] **Address Remaining EXEC failures**. Target 99/0 parity.

### Phase R.4 — Retire Old Paths

Once parity is proven, remove all legacy materialized representations.

- [ ] **R.4.1** Delete `cg_normalize_v2.{cc,h}`, `cg_ir_v2_emit_llvm.cc`, `cg_ir_v2_print.cc`, `cg_ir_v2_parse.cc`.
- [ ] **R.4.2** Delete discarded multi-inst handlers in `cg_view.cc`.
- [ ] **R.4.3** Promote the direct-emission path to default.
- [ ] **R.4.4** Finalize file rename: `cg_view_emit_llvm.cc` → `cg_emit_llvm.cc`.
