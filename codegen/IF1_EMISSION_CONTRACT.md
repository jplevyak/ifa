# IF1 Emission Contract

This document outlines the strict subset of the `IF1` (Intermediate Form 1) data structures that are guaranteed to be populated and stable by the time execution reaches the codegen backend phase. 

Any new backend (e.g., a direct LLVM emitter, Cranelift, or V8) should rely **only** on these fields to generate code. All other `IF1` fields are considered build-time or analysis-time machinery and may be discarded or invalid during emission.

## The Flattened Model

By the time codegen runs, the deep analysis models (`AVar`, `AType`, `lattice`) have completely collapsed into concrete types and values on `Sym` and `Var`. The backend NEVER needs to perform dataflow or type inference.

At emission time, an IF1 "value" is:
`(Var, Sym=Var->sym, Type=Var->type, Constant=Var->constant)`

## Load-Bearing Fields for Emission

The minimum information set the emitter genuinely needs and can assume is populated:

### Per Type (`Sym`)
* `id`, `name`: Identity and structural naming (e.g., for `struct _CG_sN`).
* `type_kind`: Distinguishes `RECORD`, `PRIMITIVE`, `FUN`, `REF`, `SUM`, etc.
* `num_kind`, `num_index`: For `PRIMITIVE` numeric types (int/float, bit width).
* `has`: For `RECORD` and closure `FUN` types, the ordered list of field `Sym`s.
* `element`: The element type for `REF` / vector / aggregate types.
* `alias`: For unwrapping type aliases.
* `size`, `alignment`: Basic memory layout hints (though target-specific layout often dominates).

### Per Function (`Fun`)
* `id`, `sym`: Function identity.
* `args`, `rets`: Formal arguments (mapped to `Var*`) and return type(s).
* `positional_arg_positions`: For correct argument ordering.
* `entry`, `blocks`: The entry block and ordered block list.
* `is_external`, `is_varargs`: Linkage and ABI flags.

### Per Block (`Code_LABEL` via `cfg_succ`)
* `id`: The label ID for diagnostic naming.
* `cfg_succ`, `cfg_pred`: Block connectivity (the CFG).
* **Predecessor MOVEs**: Phi lowering is implicit in the SSU `phi` and `phy` lists attached to edge boundary `PNode`s.

### Per Operation (`PNode` & `Code`)
* `kind`: Drives emission dispatch (`Code_LABEL`, `Code_MOVE`, `Code_SEND`, `Code_IF`, `Code_GOTO`).
* `live`, `fa_live`: Gates emission (if dead, skip).
* `lvals`, `rvals`: The `Vec<Var*>` def-use arrays.
* `prim`: The primitive dispatch key for `Code_SEND`.
* `creates`: Distinguishes closure construction (ALLOC + FIELD_STORE) vs period (FIELD_LOAD) for `P_prim_period`.
* **For If**: Condition `Var` + true/false target labels.
* **For Goto**: Target label.
* **For Move**: `lhs` / `rhs` `Var`s.

### Per Value (`Var`)
* `sym`: Back-pointer to identity.
* `type`: The fully resolved flow-analysis type.
* `def`: The single `PNode` that produces this value (SSA def site).
* `is_formal`, `is_internal`, `live`: Scope and liveness flags.
* `constant`: Constant payload (populated post-DCE) for compile-time constants.
* **Emission Cache**: The backend should maintain a function-scoped map from `Var*` to the backend's local value representation (e.g. `llvm::Value*`).

### Per Primitive (`Prim`)
* `index`: The universal dispatch key for `Code_SEND`.
* `cgfn` / `llvm_cgfn`: Registered function pointers for backend dispatch.

## What NOT to read

Do not rely on:
- `AVar`, `AType`, `CreationSet`: Analysis phase only.
- `dom`, `rdom`, `loop_node`: Unless recomputed, CFG structural analysis is stale post-transform.
- `Code`'s `rvals`/`lvals`: Use the flattened `PNode::rvals`/`lvals` which carry the resolved `Var*`.
