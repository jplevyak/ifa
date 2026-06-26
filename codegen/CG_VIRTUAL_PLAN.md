# CG_VIRTUAL_PLAN — Migrate LLVM backend from materialized CG IR to virtual views

## June 2026 architectural reset (after C.2 close-out)

The C.2 work hit a wall at 42/100 under `PYC_LLVM_VIEW=1` not because
the view layer is wrong but because **the materialized CGv2Program
itself is lossy** — it's at 92/7 against the C backend's 99/0.  Porting
each `lower_send_*` helper into the view path was reproducing the
materialized side's deficits, then forcing the view to reconstruct a
parallel intermediate (`view_translate_type`, `view_translate_struct`,
`sym_to_struct` cache) that ended up creating *different* CGv2Types
than the materialized side, surfacing as `Invalid indices for GEP
pointer type` verifier errors.

**The C backend (`cg.cc`) shows the right model.**  No intermediate IR.
`write_c_pnode` walks PNodes via cfg_succ DFS; `write_send` / `write_c_prim`
dispatch on `pn->prim` and emit C directly; type info comes from
`cg_get_string(var)` (the IF1 Var/Sym already carries a renderable
name) and `c_type(var)` (resolves a struct type name from the Sym).
Closure construction is three printfs (`_CG_prim_closure` + two field
stores).  Result: 99/0.

The LLVM backend should follow the same model: emit directly from IF1,
no CGv2.  Type info comes from `Var->type` + `Sym->has[]` walk at emit
time, mapped to an `llvm::Type *` lazily.  Operand identity comes from
a per-Fun `Var * → llvm::Value *` map.  Multi-inst sequences (closure
construction, list literals) are just multiple emit calls in one
`switch` case.

This collapses Phase C.3 / D / E into one move:

- **C.3-rewrite**: build `cg_view_emit_llvm.cc` that walks IF1 directly
  (mirroring `cg.cc` structurally) and emits LLVM IR per-PNode.  Type
  rendering: lazy `Sym * → llvm::Type *` cache.  No CGv2Program.
- **D-rewrite**: when the new emitter reaches 99/0, retire
  `cg_normalize_v2.{cc,h}`, `cg_ir_v2_emit_llvm.cc`,
  `cg_ir_v2_{print,parse}.cc`, and most of `cg_view.cc`
  (the multi-inst handlers + `view_translate_type` machinery were
  premised on the lossy intermediate).
- **E**: the parity gap to 99/0 closes inside the new emitter; the
  three known C-side fixes (`prim_is`, voidish-cast, union receiver)
  port straight from `cg.cc`'s code paths.

What survives from the C.2 work:
- `CGInstView::kind()` and the constant-elision check (still
  classification logic that's useful at the dispatch layer).
- B.4 block iteration (`view_build_fun_blocks`) — same shape as
  cg.cc's per-block DFS, just typed.
- B.5 phi/phy enumeration — same as cg.cc's `do_phi_nodes`,
  `do_phy_nodes`, just emits IR instead of C printfs.
- B.6 diff oracle (instruction-level) — still useful for the
  rewrite, just now comparing view-emitted IR text against
  materialized-emitted IR text directly.

What does NOT survive:
- `view_translate_type`, `view_translate_struct`,
  `view_translate_closure_struct`, `sym_to_struct` —
  these were reconstructing a CGv2 intermediate.  The new emitter
  reads `Sym->has[]` at emit time and builds `llvm::StructType`
  directly.
- `view_lower_pnode`'s multi-inst handlers
  (`view_try_lower_alloc`, `view_try_lower_call`,
  `view_try_lower_period_closure`) and the per-prim shape
  helpers (`view_shape_*`) — these consume CGv2Insts.  The new
  emitter has them as `case`s in one big switch that emits LLVM
  directly, like `write_c_prim`.
- `cg_v2_emit_llvm_module_view` — the "rebuild CGv2 prog from
  view then reuse materialized emit" model.  The new emitter
  goes straight to LLVM.

Sub-checklist for the rewrite is at the bottom of this doc.

---


## Goal

Replace `cg_normalize_v2.cc`'s materialized `CGv2Program`
+ `CGv2Inst`/`CGv2Block`/`CGv2Fun`/`CGv2Type`/`CGv2Value`
with **accessor-based views** over IF1 (FA's converged
state).  The LLVM emitter consumes views; no in-memory
CG IR objects are built.

The classification taxonomy (`CGv2Op` enum + the
contract in [CODEGEN_LLVM_CONTRACT.md](../CODEGEN_LLVM_CONTRACT.md))
**stays**.  Materializing the objects that hold the
classification doesn't.

## Success criteria

| Criterion | Today | After |
|-----------|------:|------:|
| C suite | 99/0 | 99/0 |
| LLVM suite | 92/7 | 99/0 (Phase E) |
| `cg_normalize_v2.cc` LOC | 2400 | 0 (file deleted) |
| Total LLVM-path LOC | ~3900 | ~1800-2200 |
| Codegen compile time (suite) | baseline | ≤ baseline (expect 10-30% faster from elided allocations) |
| Allocations per compile | hundreds of thousands | near zero (view objects are stack-only) |

## Non-goals

- Adding IR-to-IR passes.  None planned; virtual makes
  them harder anyway (they'd need to materialize their
  inputs).
- Serializable IR.  No `cg_view_parse` round-trip.
  Debugging gets `cg_view_print` for human inspection,
  not parseable text.
- IR-level unit tests with synthesized inputs.  Tests
  go through real FA, like the C backend does today.
- Touching the C backend.  C reads IF1 directly already;
  this plan doesn't move that.

## Architecture target

```
IF1 (Sym/Var/Fun/PNode/Code, FA-annotated)
  │
  ▼
CGView (header-only types: CGFunView, CGBlockView,
        CGInstView, CGValueView, CGTypeView)
  │  ↑ accessors (kind(), rvals(), lvals(), type(), ...)
  ▼
cg_emit_llvm.cc (consumes views, emits LLVM IR)
  │
  ▼
llvm.cc (scaffolding: init, main wrapper, verify, write)
  │
  ▼
LLVM IR → clang → executable
```

`CGv2Op` enum stays as the canonical instruction-kind
taxonomy.  Accessors return `CGv2Op` values.  Emit
dispatches on them the same way it does today.

## Migration phases

### Phase A — Define view types and accessor library (1 week)

**Deliverable**: `ifa/codegen/cg_view.h` + `cg_view.cc`.

Define the view header types:

```cpp
class CGFunView {
  Fun *fn_;
public:
  explicit CGFunView(Fun *fn) : fn_(fn) {}
  cchar *name() const;
  CGv2Type *return_type() const;  // or returns a Sym* + lazy translation
  Vec<CGValueView> formals() const;
  iterator<CGBlockView> blocks() const;
};

class CGBlockView {
  Fun *fn_;
  PNode *label_;   // the LABEL PNode that starts this block
public:
  iterator<CGInstView> insts() const;
  iterator<CGBlockView> successors() const;
  // ...
};

class CGInstView {
  PNode *pn_;
public:
  CGv2Op kind() const;       // classification from pn_->code->kind + pn_->prim
  Vec<CGValueView> rvals() const;
  Vec<CGValueView> lvals() const;
  CGv2Type *type_arg() const;
  cchar *prim_name() const;  // for CG2_PRIM / CG2_C_CALL
  // ...
};

class CGValueView {
  Var *v_;
public:
  CGv2Type *type() const;
  cchar *cg_string() const;
  bool is_constant() const;
  // ...
};
```

Implementation: each accessor walks IF1 once.  No
caching in Phase A — measure first, optimize later.

**Tests in Phase A**:
- Build the views over the existing suite's FA outputs.
- For each test program, run `cg_normalize_v2(fa)` and
  build views from the same `fa`.  Verify the
  classifications match.  If a CGv2Inst has
  `op == CG2_BINOP`, the corresponding `CGInstView`
  must return `CG2_BINOP` from `kind()`.
- This is the **diff oracle** for the entire migration:
  any mismatch is a bug in either the normalizer or the
  accessor.

**Verify**: full suite green (99/0 C + 92/7 LLVM,
unchanged).  No call sites changed.

### Phase B — Bridge: emit_inst accepts views OR materialized (1 week)

**Status (June 2026): B.1 landed.**  See "Phase B
sub-deliverables" below for the per-case checklist.



**Deliverable**: `cg_ir_v2_emit_llvm.cc` accepts both
representations via an adapter.

Strategy: introduce a thin interface that the emit
switch reads from:

```cpp
struct CGInstRef {
  CGv2Op op;
  CGv2Type *type_arg;
  Vec<CGValueRef> rvals;
  Vec<CGValueRef> lvals;
  // ...

  // Two factories:
  static CGInstRef from_v2(CGv2Inst *inst);  // existing path
  static CGInstRef from_view(CGInstView v);  // new path
};
```

Refactor `emit_inst(CGv2Inst *, ...)` to
`emit_inst(CGInstRef, ...)`.  Both factories produce
identical `CGInstRef`s for the same logical instruction.

**Tests in Phase B**:
- Compile each test program through BOTH paths (run
  materialized normalize+emit, then virtual emit) and
  diff the produced `.ll` files.  They must be
  bit-identical.
- Add a `PYC_LLVM_DIFF=1` mode that does this
  automatically and reports any divergence.

**Verify**: full suite green.  Both paths produce
identical LLVM IR.  Cross-check via diff tool.

#### Phase B sub-deliverables

The 24 emit cases get refactored to `CGInstRef`
incrementally.  Each is mechanical: change the
function signature, replace `inst->field` with
`ref.field` (with `rvals`/`lvals` accesses going
through the pointer indirection).  No semantic change.

- [x] **B.1** Bridge type + factories defined
      (`CGInstRef::from_v2`, stub `from_view`).
- [x] **B.1** Terminator path refactored:
      `emit_terminator(CGInstRef, CGv2Block *, EmitFunCtx&)`
      consumes the bridge; caller wraps via
      `CGInstRef::from_v2(b->terminator)`.  Suite green
      (LLVM 92/7 unchanged, C 99/0 unchanged).
- [x] **B.2** (signature pass): `emit_inst(CGInstRef,
      EmitFunCtx&)` lands as a thin shim — case bodies
      keep reading `inst->X` via a `ref.v2` shadow at
      the top of the function.  Both call sites (block
      body + phi-edge MOVE groups) wrap their
      `CGv2Inst*` argument via `CGInstRef::from_v2`.
      Suite green (LLVM 92/7 unchanged, C 99/0
      unchanged, unit 105/0).
- [x] **B.2a** (field-access pass): inside `emit_inst`
      cases, replace `inst->op` / `inst->rvals[i]` /
      `inst->type_arg` / etc. with `ref.op` /
      `ref.rvals->v[i]` / `ref.type_arg` so the
      function no longer depends on `ref.v2`.  Done via
      `sed -i 's/inst->/ref./'` across the case bodies
      plus three manual touch-ups (the switch dispatch
      on `ref.op`, a comment, and one `for (… : *ref.rvals)`
      range loop where the dereference must match the
      bridge's pointer field).  The early-return guard
      `if (!ref.rvals || !ref.lvals) return;` replaces
      the `ref.v2` shadow; for the materialized path
      `from_v2` always populates both, so production
      behavior is unchanged.  Suite green (LLVM 92/7
      unchanged, C 99/0 unchanged).  This is what
      unlocks `from_view` callers reaching the same
      code path — once Phase B.3 populates `rvals`/`lvals`
      on view-origin refs, they sail through the guard.
- [x] **B.3** Built the `CGv2Value`-shaped wrapper.
      `ViewBuildCtx` (declared in `cg_view.h`) carries a
      `Map<Var*, CGv2Value*>` and a `Map<Sym*, CGv2Type*>`
      keyed on a shared `CGv2Program*` (whose predefined
      `t_int64` / `t_ptr` / ... slots the view reuses).
      `view_translate_value(CGValueView, ViewBuildCtx&)`
      synthesizes a fresh CGv2Value per Var: name from
      Sym/cg_string, scope from is_constant / is_fun /
      is_symbol flags, `imm` populated via the new
      `view_build_immediate` (a stand-alone duplicate of
      `cg_normalize_v2.cc:build_immediate` — kept
      separate so Phase D removes the materialized side
      without breaking the view).  Type translation is
      handled by `view_translate_type`: predefined
      numerics short-circuit to `prog->t_*`; record/
      list/tuple/closure Syms map to `prog->t_ptr` for
      now (matches the materialized side's by-pointer
      convention and keeps the bridge usable without a
      full `build_type` reimplementation — full
      `CG2T_STRUCT` walks land when B.5's synthetic
      emit needs them).  `CGInstRef::from_view` now
      takes an optional `ViewBuildCtx *` parameter:
      when non-null it populates `rvals` / `lvals` with
      freshly-translated CGv2Value*'s (GC-allocated Vec
      per PNode).  Without a ctx the previous
      classification-only behavior is preserved — the
      emit_inst guard at `!ref.rvals || !ref.lvals`
      sends those refs through the early-return path.
      No callers wire up the ctx form yet; that's
      Phase B.5.  Suite green (LLVM 92/7 unchanged, C
      99/0 unchanged).
- [x] **B.4** Block-iterator on the view side.
      `view_build_fun_blocks(CGFunView, ViewBuildCtx&, CGv2Fun*=nullptr)`
      mirrors `cg_normalize_v2.cc:build_block_skeleton`:
      BFS from `Fun::entry` over `cfg_succ`, allocate one
      CGv2Block per Code_LABEL PNode discovered, name
      them "entry" / "L<label-id>" / "B<id>" identically
      to the materialized side.  Populates
      `ViewBuildCtx::pn_to_block`, `label_to_block`,
      `fun_blocks` (per-Fun cache of `Vec<CGBlockView>`),
      and `entry_to_closer` (the in-walk closer
      detection — a PNode is a closer iff any cfg_succ
      lands in a different CGv2Block, or it has no
      successors).  When an optional `CGv2Fun *cf` is
      passed, the freshly-allocated CGv2Blocks are also
      appended to `cf->blocks` so a co-existing
      materialized fn ends up with identical block
      ids/names — the side-by-side parity needed for the
      B.6 byte diff.  `CGBlockView::body_pnodes(vctx)`
      walks the same-block cfg_succ region excluding
      entry-LABEL and closer; `CGBlockView::closer_pnode(vctx)`
      reads from the cached `entry_to_closer`.
      `CGInstRef::from_view` was extended to consume
      these maps: GOTO target → `vctx.label_to_block.get(code->label[0])`,
      IF true/false → `label[0]` / `label[1]`.
      No callers wire up the block iteration yet; that
      lands in B.5 alongside synthetic-instruction
      emission.  Suite green (LLVM 92/7 unchanged, C
      99/0 unchanged).
- [x] **B.5** Synthetic-instruction emission from the
      view.  Three new functions in `cg_view.{h,cc}`:
      `view_enumerate_phi_moves(pred_closer, succ_entry,
      isucc, vctx)` walks `pred_closer->phy` and
      `succ_entry->phi`, emitting CG2_MOVE CGInstRefs
      that mirror `cg_normalize_v2.cc:materialize_phi_phy`
      (phy: `lvals[isucc] ← rvals[0]`; phi:
      `lvals[0] ← rvals[pred_idx]` using
      `cfg_pred_index.get(closer)`).
      `view_make_terminator(CGBlockView, vctx)` dispatches
      on the closer's Code kind to produce a CGInstRef
      with op ∈ {CG2_BR, CG2_COND_BR, CG2_RET,
      CG2_UNREACHABLE} — identical to
      `build_terminator`'s logic.  And
      `view_enumerate_fun_insts(CGFunView, vctx)` ties
      them together: per block, emit body PNodes (via
      `from_view`) → phi-edge MOVEs (for each multi-block
      cfg_succ) → terminator.  The diff oracle
      (`cg_view_diff_report`) now consumes this
      enumeration instead of walking flat PNodes — so the
      Phase A histogram-diff gap on the synthetic side
      closes:
      ```
      conditional.py:   BR 3/3, COND_BR 1/1, RET 3/3,
                        MOVE 8/8, ALLOC 9/9, BINOP/C_CALL match
      while_loop.py:    BR 3/3, COND_BR 1/1, RET 2/2,
                        BINOP 2/2, MOVE 9/8 (one phi edge case)
      ```
      The CGInstView::kind() body-classification gaps for
      Code_SEND (CALL vs FIELD_LOAD routing) remain — they
      were Phase A's "best-guess" classifications and
      depend on cg_normalize_v2's lower_send shape-
      detection.  Those don't block B.5's deliverable
      (synthetic instructions match); they're addressed
      in Phase C's view-driven emit when the dispatch is
      centralized in one place.  Suite green (LLVM 92/7
      unchanged, C 99/0 unchanged).
- [x] **B.6** `PYC_LLVM_DIFF=1` mode lands as an
      instruction-level diff oracle:
      `cg_view_diff_module(fa, prog, tag)` pair-walks the
      view's enumeration (from B.5's
      `view_enumerate_fun_insts`) against the materialized
      CGv2Insts flattened in emit-walk order (body → phi
      MOVEs per cross-block edge → terminator).  Each
      instruction is compared on `(op, sub_op, n_rvals,
      n_lvals)`; first per-Fun divergence reports both
      sides' signature with `r0=<name>` / `l0=<name>`
      operand hints for grep-ability across the suite.
      Wired up in `llvm_codegen_print_ir` after the
      materialized emit returns; output goes to stderr
      tagged with the input filename:
      ```
      [llvm-diff] tests/conditional.py fn=_CG_f_2195_1 first divergence @ inst 0:
      [llvm-diff]   v2:   MOVE      sub=0 r=1 l=1 r0=a l0=v
      [llvm-diff]   view: FIELD_LOAD sub=0 r=4 l=1 r0=__operator l0=v
      [llvm-diff] tests/conditional.py total divergences: 35
      ```
      Why instruction-level instead of byte-diff on
      `.ll`: a fully view-driven LLVM emit is Phase C's
      deliverable, and until C lands there's no second
      `.ll` to byte-diff against.  The instruction
      stream the diff oracle reads IS what Phase C's
      emit will iterate over, so a clean diff here is
      the equivalent invariant — once C's emit goes
      live, the harness can be promoted to literal
      `.ll` byte-diff without re-plumbing.  Phase D
      promotes a non-zero count to a hard error;
      during B/C divergences are informational.
      The current divergences across the suite are
      exactly the lower_send shape-detection gaps from
      B.5 (CALL vs FIELD_LOAD vs ALLOC routing) — they
      block C's first iteration but don't block B.6's
      deliverable, which is the harness itself.
      Suite green (LLVM 92/7 unchanged, C 99/0
      unchanged, ifa-test 79/1 unchanged).

### Phase C — View-based emit becomes primary (2 weeks)

**Status (June 2026): C.1 landed.**  See the C.1+
sub-deliverable checklist below.

**Deliverable**: `cg_emit_llvm.cc` (renamed or new file)
that walks views directly, without going through
`CGv2Program`.

Each existing emit case is rewritten to consume the
view:

```cpp
case CG2_BINOP: {
  // before:
  //   llvm::Value *lhs = lookup_value(inst->rvals[0]);
  //   llvm::Value *rhs = lookup_value(inst->rvals[1]);
  // after:
  llvm::Value *lhs = emit_value(inst.rvals()[0]);
  llvm::Value *rhs = emit_value(inst.rvals()[1]);
  // ... dispatch on inst.sub_op() unchanged
}
```

The materialized path is still callable for diff-
verification, but the production path is virtual.

**Tests in Phase C**:
- `PYC_LLVM_DIFF=1` continues running both and diffing.
- Default suite uses the virtual path.

**Verify**: full suite green via virtual path.
Materialized path still produces identical output for
diff.

#### Phase C sub-deliverables

- [x] **C.1** View-driven emit entry point:
      `cg_v2_emit_llvm_module_view(fa, prog)` in
      `cg_view.cc`.  Walks `fa->funs`, finds each Fun's
      CGv2Fun in prog (by `cg_string` name match),
      clears its bodies (`cf->blocks`, `cf->entry`), and
      rebuilds them from the view enumeration:
      `view_build_fun_blocks` reallocates blocks
      identically; per-block, `body_pnodes` produces
      body insts (via `cg_view_ref_to_v2inst` — a
      helper that materializes a CGInstRef back into a
      CGv2Inst); `view_enumerate_phi_moves` produces
      phi-edge MOVEs, placed in the successor block's
      `phi_by_pred` group keyed by the pred block;
      `view_make_terminator` produces the terminator.
      Then hands off to the existing
      `cg_v2_emit_llvm_module(prog)` — which now sees
      view-derived data and emits LLVM as before.
      Activated by `PYC_LLVM_VIEW=1`; default emit
      unchanged.  Today's status with this flag on:
      10/100 LLVM tests pass — the 90 failures are
      classification gaps in `CGInstView::kind()` that
      C.2 closes (the diff oracle from B.6 reports
      them per-test).  Default suites unchanged: C
      99/0, LLVM 92/7, ifa-test 79/1.
- [~] **C.2** Closing Code_SEND classification gaps in
      `CGInstView::kind()`.  Partial progress this
      session — `PYC_LLVM_VIEW=1` moved 10 → 11 (still
      far from the 92 target):
      - [x] **Constant-folded functional SEND elision.**
            Mirrors `lower_send`'s top-of-fn check at
            cg_normalize_v2.cc:1979: when the result is
            a single statically-known constant AND the
            prim is functional (`!prim->nonfunctional`)
            AND `lvals.n == 1` AND
            `get_constant(lvals[0])` returns non-null,
            `CGInstView::kind()` returns CG2_NOP — the
            view enumeration drops the instruction
            entirely, matching the materialized side.
            This is the highest-volume gap: hundreds of
            per-Fun specialization sends FA produces.
      - [x] **Operand identity for value translation.**
            `view_translate_value` now looks up
            constants/globals (via `prog->{constants,
            globals}`) and formals/locals (via
            `vctx.current_cf->{formals,locals}`) by Sym
            name BEFORE allocating a fresh CGv2Value.
            Without this, the LLVM emit's per-Fun
            `value_map` (keyed on CGv2Value*) couldn't
            resolve view-derived operands to their
            materialized formal/local objects —
            formals would never bind to their
            llvm::Argument and locals would diverge
            from their alloca slots.  Added a
            `current_cf` field on ViewBuildCtx that
            `cg_v2_emit_llvm_module_view` updates
            per-Fun.
      - [x] **`lower_send_prim` name-based dispatch
            (partial).**  `from_view` now post-processes
            P_prim_primitive / isinstance / is SENDs via
            `view_dispatch_send_prim` →
            `view_shape_isinstance_or_is` for
            CG2_BINOP-EQ vs null/rhs,
            `view_shape_c_call_pyc` for `__pyc_c_call__`
            (reads `pn->rvals[3]->sym->constant` as the
            C fn name, walks `(arg_type, arg_value)`
            pairs starting at rvals[5]), and a default
            `view_shape_default_named_prim` that emits
            `_CG_<name>(rvals[2..])` for unknown prims.
            Mirrors the structure of
            cg_normalize_v2.cc:lower_send_prim.  What's
            still missing here: `__pyc_to_str__`
            constant-MOVE shape and the secondary named-
            prim routings (`__pyc_format_string__`,
            `index_object`, `sizeof`, `len`).
      - [x] **`lower_send_period` shape detection
            (partial).**  `view_shape_period` ports
            `resolve_field_index_v2` and the simple
            FIELD_LOAD lowering; falls through to
            CG2_CALL when the obj's struct doesn't
            carry the named field.  Still missing: the
            closure-construction case (lv->type ==
            Type_FUN with pn->creates non-empty) that
            emits ALLOC + two FIELD_STOREs from one
            PNode.
      - [x] **`lower_send_setter` (partial).**
            `view_shape_setter` ports the
            CG2_FIELD_STORE shape (obj, val) with
            field_idx + type_arg from the obj's struct.
            Missing: the chained-assignment forward MOVE
            into lvals[0] (lower_send_setter's tail at
            cg_normalize_v2.cc:813).
      - [x] **BINOP operand trim.**
            `view_shape_binop` rewrites the operand
            list to `(rvals[n-3], rvals[n-1])` matching
            lower_send_binop's IF1 prim-call convention.
      - [x] **ALLOC type_arg + rvals trim** + **CAST /
            CLONE type_arg** in `from_view`'s post-pass.
            ALLOC contract requires empty rvals; the
            naive view passed through the `__primitive`
            marker.  type_arg derived from `lvals[0]`'s
            translated CGv2Type.
      - [ ] **`lower_send_alloc` multi-inst sequence.**
            List/tuple literals lower to a SIZEOF +
            CG2_C_CALL(`_CG_prim_tuple_list_internal`) +
            per-element INDEX_STOREs + a second
            CG2_C_CALL(`_CG_to_list_runtime`) for the
            struct-shape path.  The view's current
            one-PNode-one-CGInstRef model can't emit
            multi-inst sequences — needs a per-block
            enumeration extension similar to the
            phi-edge MOVE injection from B.5.
      - [ ] **`lower_send_call` MPosition-aware arg
            routing + closure unpacking.**  The
            materialized side walks
            `target->positional_arg_positions` and
            inserts per-formal FIELD_LOAD insts before
            the CALL when the receiver is a closure.
            View currently passes through all rvals
            verbatim.
      - [x] **`lower_send_index_load`** —
            `view_shape_index_load` ports the
            three-way dispatch: string-typed obj →
            CG2_C_CALL `_CG_char_from_string`,
            constant-index tuple element →
            CG2_FIELD_LOAD with the resolved field
            idx, otherwise CG2_INDEX_LOAD.
      - [x] **`lower_send_index_store`** —
            `view_shape_index_store` emits
            CG2_INDEX_STORE with (obj, idx, val).
      - [x] **`lower_send_sizeof`** —
            `view_shape_sizeof` emits CG2_SIZEOF with
            type_arg from rvals[o]->type.
      - [x] **`lower_send_sizeof_element`** —
            `view_shape_sizeof_element` emits
            CG2_SIZEOF_ELEMENT with the src as rval.
      - [x] **`lower_send_len`** — `view_shape_len`
            preferentially picks the string-typed rval
            (mirrors lower_send_len's loop at
            cg_normalize_v2.cc:1527), falls back to
            rvals[o].  Emits CG2_LEN.
      - [x] **`lower_send_clone` (non-vector path)**
            — `view_shape_clone` emits CG2_CLONE with
            type_arg from the dst's unwrapped struct;
            falls through to caller's default
            classification on shape mismatch.  The
            vector path (P_prim_clone_vector) needs
            SIZEOF + C_CALL multi-inst — left for the
            multi-inst refactor.
      - [x] **`lower_send_lnot`** —
            `view_shape_lnot` synthesizes a zero
            constant of the operand's type and emits
            CG2_BINOP EQ.
      - [x] **`lower_send_neg`** —
            `view_shape_neg` similar: synthesizes
            zero and emits CG2_BINOP SUB (`0 - x`).
      - [x] **`lower_send_coerce`** —
            `view_shape_coerce` reads (rvals[n-2])
            for the target type Sym, unwraps via
            `unalias_type`, emits CG2_CAST.
      - [ ] **`lower_send_format_string` /
            `lower_send_strcat` /
            `__pyc_to_str__` constant-MOVE.**
            Three smaller ports remaining.

      - [x] **Multi-inst infrastructure.**
            `view_lower_pnode(PNode, vctx, &out)`
            replaces the previous "one PNode → one
            CGInstRef" body walk in both
            `view_enumerate_fun_insts` and
            `cg_v2_emit_llvm_module_view`.  Multi-inst
            handlers (`view_try_lower_alloc_flat`,
            `view_try_lower_call`) try to claim a PNode
            and append N refs to `out`; on `false`
            fall through to the single-inst
            `from_view` path.
      - [x] **`lower_send_call` MPosition arg routing.**
            `view_try_lower_call` ports
            cg_normalize_v2.cc:1565: resolves callee
            via `vctx.current_fun->calls.get(pn)`
            (single-target only), allocates the fnref
            CGv2Value, walks
            `target->positional_arg_positions`
            computing `Position2int(p->pos[0]) - 1` as
            the rval index for each formal.  Falls
            back to pass-through `rvals[1..]` when
            MPosition produces nothing.  Closure-
            receiver case (rvals[0] is Type_FUN with
            has.n ≥ 2) deliberately skipped — needs
            the closure-FIELD_LOAD-unpack multi-inst
            sequence (deferred).  Added `current_fun`
            field to ViewBuildCtx scoped per-Fun by
            both walkers.
      - [x] **`lower_send_alloc` flat-list multi-inst
            (partial).**  `view_try_lower_alloc_flat`
            emits SIZEOF(elem_type) +
            CG2_C_CALL(`_CG_prim_tuple_list_internal`,
            size, n) for the flat-array list/tuple
            literal shape.  The struct-shape stage-1/
            stage-2 path (`_CG_to_list_runtime`) is
            still deferred.
      - [x] **`lower_send_call` closure unpacking.**
            `view_try_lower_call` now handles
            closure-receiver dispatch (lower_send_call
            lines 1618-1647): when rvals[0] is a
            closure (`is_closure_var(v0)` returns true)
            and its CGv2Type is a CG2T_PTR to
            CG2T_STRUCT, each formal that maps within
            the closure's field range gets a
            CG2_FIELD_LOAD inst emitted BEFORE the
            CALL.  Formals beyond the field count get
            their rval index shifted by `has.n - 1`
            (rvals[0] is the closure itself).  Formal
            filters mirror the materialized side
            (`!live`, `type->is_fun`, `pos.n > 1`
            skipped).
      - [x] **`lower_send_period` closure construction.**
            `view_try_lower_period_closure` ports
            cg_normalize_v2.cc:718-774: when
            lvals[0]->type is Type_FUN with
            `pn->creates` non-empty and `has.n ≥ 2`,
            emits CG2_ALLOC(closure_struct) +
            CG2_FIELD_STORE(0, selector) +
            CG2_FIELD_STORE(1, bound_self).  Currently
            firing rarely because Phase B.3's
            `view_translate_type` returns CG2T_OPAQUE
            for Type_FUN Syms — the proper
            CG2T_STRUCT translation that would let
            this fire across the suite is the next
            big unlock.
      - [ ] **Remaining single-inst ports:**
            `lower_send_format_string`,
            `lower_send_strcat`, `__pyc_to_str__`
            constant-MOVE.
      - [x] **`lower_send_alloc` full multi-inst port.**
            `view_try_lower_alloc` (replaces
            `view_try_lower_alloc_flat`) handles three
            sub-paths matching `cg_normalize_v2.cc:890+`:
            (1) flat list/tuple/vector — SIZEOF + C_CALL
            + per-element INDEX_STOREs; (2) struct-shape
            list/tuple — SIZEOF + C_CALL alloc to tmp +
            per-field FIELD_STOREs to tmp + SIZEOF +
            C_CALL(`_CG_to_list_runtime`) into dst; (3)
            regular class instantiation — bare CG2_ALLOC
            + per-field FIELD_STOREs from constructor
            args.  Constants/temps allocated GC-side,
            registered with prog->constants where the
            materialized side does.
      - [x] **`view_translate_type` full Type_FUN /
            Type_RECORD walk.**  Port of
            cg_normalize_v2.cc:build_type (line 168)
            and build_struct_type (line 104).  Handles
            predefined numerics, void, nil, symbol;
            string family routes to a typed char-ptr
            via prog->types `lookup("string")`;
            Type_RECORD builds the CG2T_STRUCT
            recursively then wraps in CG2T_PTR;
            Type_FUN closures (no resolved fun +
            non-empty has[]) build closure_<name>.<id>
            struct + wrap in PTR.  Field translation
            recursively calls view_translate_type;
            CG2T_VOID field types are substituted to
            t_ptr per the materialized convention.
            Identity-stability with the materialized
            translator achieved via name-match lookup
            in prog->types BEFORE allocating a fresh
            CGv2Type — when both paths run side-by-
            side (Phase B/C dual mode) the view-driven
            CGv2Insts reference the same CGv2Type
            pointers the materialized side built.
            Added `vctx.sym_to_struct` to ViewBuildCtx
            mirroring NormCtx::sym_to_struct.

      **Session progress:** `PYC_LLVM_VIEW=1` moved
      10 → 40 passing (+30 across multiple turns).
      Milestones:
      - 10 → 17: BINOP trim, lower_send_prim
        dispatch + named-prim fallback, lower_send_period,
        lower_send_setter, INDEX_LOAD/STORE, SIZEOF,
        LEN, CLONE, LNOT, NEG, COERCE single-inst ports.
      - 17 → 37: `lower_send_call` MPosition routing
        (the big lever).
      - 37 → 38: closure unpacking + construction.
      - 38 → 40: full `view_translate_type` with
        proper CG2T_PTR-to-CG2T_STRUCT for records
        and closures (unlocks tuple_unpack,
        tuple_unpack_multiprint, tuple_len,
        string_len, ring_selfcycle).
      - 40 → 42: full `lower_send_alloc` multi-inst
        port — adds the per-element INDEX_STORE /
        FIELD_STORE loop and the struct-shape stage-2
        `_CG_to_list_runtime` conversion (unlocks
        `print_multiple_args`, `multimethod_closure`,
        `closure_in_loop`, `lambda_closure` and
        similar).

      **Remaining gaps (~58 failing):** runtime
      `Invalid indices for GEP pointer type` errors on
      class-heavy tests suggest FIELD_LOAD/STORE
      type_arg derivation is sometimes still picking
      up an opaque ptr where the materialized side
      gets a struct.  Tracing these requires
      per-test diffing against materialized LLVM IR
      output (B.6 diff oracle reports instruction-
      level divergence but not the actual GEP type
      mismatch — that's an LLVM verifier message
      generated at emit time).  Plus remaining small
      ports: `lower_send_format_string`,
      `lower_send_strcat`, `__pyc_to_str__`.
      Drive each gap with B.6's diff oracle on a
      single test: fix the first-reported divergence,
      re-run.  Target: PYC_LLVM_VIEW=1 reaches 92/7
      (matches materialized baseline).  Estimated
      multi-session effort.  Default suites unchanged
      this session: C 99/0, LLVM 92/7, ifa-test 79/1.
- [ ] **C.3** Promote `PYC_LLVM_VIEW` to the default
      path.  Materialized path stays callable via
      `PYC_LLVM_MATERIALIZED=1` for diff verification.
      Verify suite still 92/7.
- [ ] **C.4** Promote B.6's diff oracle from
      instruction-level to literal `.ll` byte-diff:
      capture TheModule->print() to a stringstream for
      both runs, diff with `diff -u`, report the first
      differing line.  This is the safety net Phase D
      depends on.

### Phase D — Remove materialization (1 week)

**Deliverable**: delete `cg_normalize_v2.{cc,h}`,
delete `CGv2Inst`/`CGv2Block`/`CGv2Fun` data structures
(keep the `CGv2Op` enum + `CGv2Type` if needed for
LLVM type cache).

Remove the bridge `CGInstRef::from_v2` factory.  The
emitter only consumes views.

Update `llvm_codegen_print_ir` to call the virtual
pipeline directly: `emit_llvm_module_from_fa(fa)`.

**Tests in Phase D**:
- `PYC_LLVM_DIFF=1` mode is removed (no second path to
  diff against).
- Full suite must remain green.

**Verify**: suite still 92/7 (no parity changes yet),
~1500-2000 lines removed.

### Phase E — Close the C-parity gap on virtual (1-2 weeks)

Now that LLVM goes virtual, port the three remaining
C-side fixes:

1. `prim_is` codegen → add CG2_BINOP-EQ case for
   `P_prim_is` SENDs in the view-based emitter.
2. Voidish-arg cast at SENDs → in the view-based
   `emit_value` for call arguments, when the arg's
   declared type is `_CG_any`/`_CG_void`/`_CG_nil_type`
   but the formal expects typed, emit `bitcast`.
3. `resolve_union_receiver` for getter/setter →
   `CGInstView::receiver_type()` accessor picks a
   non-nil component of a SUM receiver.

**Verify**: LLVM suite reaches 99/0.  Both backends at
parity.

### Phase F (optional) — Reorganize files (0-1 week)

If after E the file layout feels stale:

- `cg_view.h` / `cg_view.cc` — accessor library.
- `cg_emit_llvm.cc` — view-based emitter (renamed from
  `cg_ir_v2_emit_llvm.cc`).
- `llvm.cc` — scaffolding only (init + main wrapper +
  verify + write).

Drop the `cg_ir_v2_*` prefix since "v2" is no longer
the active design — there's just "the LLVM backend."

## Risks and mitigations

| Risk | Mitigation |
|------|------------|
| Subtle behavior divergence between materialized and virtual paths during Phase B/C | `PYC_LLVM_DIFF=1` diffs `.ll` output bit-by-bit across all tests; any mismatch caught before merge |
| View accessor cost dominates (recomputation per access) | Phase A tests measure compile time vs baseline; if regression > 5%, add per-`PNode` caching in `CGInstView` |
| A CGv2 concept turns out not to map cleanly back to IF1 | Phase A's "classification match" test will reveal these.  For each mismatch, either: (a) the materializer's transformation is the real semantic — encode it as an accessor; or (b) IF1 has the info and v2 was redundantly transforming.  Either way, the answer surfaces during A. |
| Loss of textual round-trip (`cg_v2_print/parse`) | Replace with `cg_view_print(fa)` — a debug-only dump tool.  Round-trip parse is dropped (no users today). |
| Loss of unit-test mode that builds CGv2Inst directly | All v2 unit tests go through `cg_normalize_v2(fa)` already — they need a real FA.  No actual loss. |
| Regressions in v2's LLVM-type cache during translation | Move the type cache into the view (or keep `CGv2Type` as the type cache and drop only `CGv2Inst`/`Block`/`Fun`).  Type translation is genuinely worth caching across queries. |

## Measurement

Track these per phase:

```
LOC                              # wc -l on the codegen files
Memory per compile (peak RSS)    # /usr/bin/time -v ./pyc <test>
Wall time per compile (suite)    # ./test_pyc | tail -1
Suite results (C / LLVM)         # passed / failed / skipped
```

Baseline (today, before Phase A):
- `cg_normalize_v2.cc`: 2400 lines
- `cg_ir_v2_emit_llvm.cc`: ~1500 lines
- `cg_ir_v2.{h,cc}` + `cg_ir_v2_print.cc` + `cg_ir_v2_parse.cc`: ~1000 lines combined
- Total LLVM-side: ~5000 lines

Target (after Phase F):
- `cg_view.h` + `cg_view.cc`: ~600 lines (accessor
  definitions, classification logic).
- `cg_emit_llvm.cc`: ~1500 lines (mostly unchanged from
  v2 emit, plus inlined accessor calls).
- `llvm.cc`: ~404 lines (unchanged scaffolding).
- Total: ~2500 lines.  **~50% reduction.**

Compile time: expect 10-30% faster on the larger tests
(allocation elision, fewer dependent loads through the
mapping tables).

## Calendar estimate

| Phase | Wall-clock effort | Calendar (with normal interruptions) |
|-------|------------------:|--------------------------------------:|
| A     | 1 week | 1-2 weeks |
| B     | 1 week | 1-2 weeks |
| C     | 2 weeks | 2-3 weeks |
| D     | 1 week | 1 week |
| E     | 1-2 weeks | 2 weeks |
| F     | 0-1 week | 1 week |
| **Total** | **6-8 weeks** | **8-12 weeks** |

E and F are independent of A-D; you could ship at the
end of D and do E/F later.  D's end-state is "virtual
LLVM at the current 92/7 parity," which is shippable.

## Rollback strategy

Each phase produces a working compiler.  If a phase
goes badly:

- A: pure addition; revert by deleting `cg_view.{h,cc}`.
- B: bridge can be turned off by reverting the
  `CGInstRef` factory selection; falls back to
  materialized path.
- C: view-based emit can be disabled via env var
  (`PYC_LLVM_VIEW=0` forces materialized path).
- D: this is the irreversible step.  Don't take it
  until B and C have been green for > 2 weeks.

The bridge in Phase B doubles as an insurance policy.
The diff-test (`PYC_LLVM_DIFF=1`) is the safety net.

## What this plan does NOT do

- Doesn't touch the FA layer.  Views read FA's existing
  state.
- Doesn't change the C backend.  C already consumes IF1
  directly; it'd be a no-op move.
- Doesn't add new language features.  Phase E closes a
  known parity gap but doesn't extend behavior.
- Doesn't address `issue 029` (polymorphic dispatch).
  That's a separate codegen feature, orthogonal to
  this migration.  Easier to do after virtual since the
  emit dispatch is centralized.

## Why this is worth doing

The line-count reduction is real (~50%).  The
allocation reduction is real (near-zero per compile).
The conceptual unification is real (one source of
truth for program shape, the FA-annotated IF1).  And
the contract doc (Phase 1) becomes the spec instead of
the materialized types, which is a stronger contract
because it doesn't co-evolve with implementation
details.

The cost is the migration weeks themselves.  No
ongoing tax; the smaller codebase is the steady state.

---

*Authored after a survey of pyc's actual multi-backend
needs (zero today, hypothetical tomorrow) and an
analysis of what materialization buys when those needs
are hypothetical.  See the dialogue in
`CG_IR_PARITY_PLAN.md` "Status (June 2026)" section
for the framing.*

---

## Rewrite sub-checklist (post-architectural reset)

### Phase R.1 — emitter skeleton + simplest cases

- [x] **R.1.1** New file `codegen/cg_view_emit_llvm.cc` (215 lines).
      Public entry: `bool cg_view_emit_llvm(FA *fa, Fun *main_fun)`.
      Defines the type cache (`Sym * → llvm::Type *` and
      `Sym * → llvm::StructType *`) with the full
      `sym_to_llvm_type` / `sym_to_llvm_struct` recursion-safe
      builder — predefined numerics short-circuit, sym_void →
      `getVoidTy`, sym_nil_type / is_symbol / string → opaque ptr,
      Type_RECORD → opaque ptr (struct built via
      `sym_to_llvm_struct` named after `<sym_name>.<id>`),
      Type_FUN closure → struct + opaque ptr.  Added to ifa
      Makefile.  Builds clean, tests unaffected.  `emit_pnode`
      and `emit_fun` are R.1.3+ stubs.  Not yet wired through
      `llvm.cc` — env var `PYC_LLVM_VIEW2=1` is intentionally
      ungated until R.1.3 lands actual emission (otherwise a
      bare skeleton would fail `verifyModule`).
- [x] **R.1.2** Per-Fun signature `build_fun_signature(ctx, f)`.
      Iterates `f->positional_arg_positions` (the materialized
      translator's pattern), reads each formal via
      `f->args.get(p)`, skips dead and `is_fun` (closure-self)
      formals, collects `llvm::Type *` via the new
      `sym_to_llvm_type` cache.  Return type from
      `f->rets[0]->type`.  Creates `llvm::FunctionType` +
      `llvm::Function` in TheModule with ExternalLinkage,
      binds each `llvm::Argument` to its formal `Var *` via
      `ctx.var_map`, sets the LLVM arg name from
      `cg_get_string(v)`.  Reuses existing declarations by
      name (forward-declares from earlier emit don't get
      duplicated).  `emit_fun(ctx, f)` calls this and emits
      a placeholder `unreachable` entry block as a body
      stub — real body emission lands in R.1.3.  **Sret-
      rewrite deferred to R.1.2.1** (value-type RECORD
      returns; only affects @pyc_struct).  Defaults
      unchanged: C 99/0, LLVM 92/7, ifa-test 79/1.
- [ ] **R.1.2.1** Sret-rewrite for value-type RECORD returns:
      when `f->rets[0]->type` is a Type_RECORD whose Sym has
      `is_value_type=1` (and Fun isn't main / varargs), prepend
      an implicit `sret` ptr arg and rewrite the body's
      RET to a store-then-ret-void.  Mirrors the materialized
      sret detection in cg_normalize_v2.cc:462.
- [ ] **R.1.3** Per-Fun body: `write_c_pnode`-style DFS over
      `f->entry`'s `cfg_succ`, with a `done` set.  Inside each
      live PNode: dispatch on `Code_kind`.  Code_LABEL emits the
      LLVM basic-block boundary; Code_MOVE emits `Builder->CreateStore`
      / `CreateLoad` pair (or alloca slot bind); Code_IF emits
      `CreateCondBr`; Code_GOTO emits `CreateBr`; Code_SEND dispatches
      via `emit_send_*`.
- [ ] **R.1.4** Code_SEND skeleton: `is_const_folded_send` check
      first; then dispatch on `pn->prim->index` (mirror
      cg.cc:write_c_prim's switch).  Default to
      `emit_send_call` (`get_target_fun` resolves; emit
      `Builder->CreateCall`).
- [ ] **R.1.5** First three prim cases: `P_prim_reply` (CreateRet),
      `P_prim_isinstance`-vs-nil (`CreateICmp` against null),
      `P_prim_is` (`CreateICmp` of two ptrs).
- [ ] **R.1.6** Per-Fun `Var * → llvm::Value *` map.  Formals bind
      from `llvm::Argument`s; locals get an `AllocaInst` in the
      entry block on first lvalue write.
- [ ] **R.1.7** Type cache: `Sym * → llvm::Type *`.  Predefined
      numerics resolve to `Int1`/`Int8`/.../`Float`/`Double`/etc.
      `sym_void` → `getVoidTy`.  `sym_nil_type` → opaque ptr.
      `is_symbol` → opaque ptr.  Type_RECORD →
      `StructType::create("<name>.<id>", { field-types })`
      cached (visit `s->has[]` once, opaque-ptr fallback for
      void/recursive fields).  Type_FUN closure → struct.
      Type_FUN with resolved fun → opaque ptr.

### Phase R.2 — port the remaining prim cases

For each prim, port the cg.cc emit code to LLVM (replacing the
`fprintf` calls with `Builder->Create*` calls).  Each is its own
landing.  Target: `PYC_LLVM_VIEW2=1` reaches 99/0.

- [ ] **R.2.1** `P_prim_period` — both struct getter (GEP + Load)
      and closure-construction (alloca + 2 stores) cases.
- [ ] **R.2.2** `P_prim_setter` — struct setter (GEP + Store) and
      chained-assignment forward (extra Store to lvals[0]).
- [ ] **R.2.3** `P_prim_make` — tuple (`_CG_prim_tuple` call +
      e0/e1/... stores) and list (`_CG_prim_list` call + indexed
      stores).  Both variants live in cg.cc:208-236.
- [ ] **R.2.4** Arithmetic family (`P_prim_add` etc.) — direct
      `Builder->CreateAdd`/`Sub`/etc.; constant-fold path for
      pairs of `Sym->constant` operands.
- [ ] **R.2.5** Comparison family — `CreateICmp`/`CreateFCmp`.
- [ ] **R.2.6** `P_prim_index_object` — string detour (call
      `_CG_char_from_string`); constant-index tuple FIELD_LOAD;
      otherwise `CreateGEP` + `CreateLoad`.
- [ ] **R.2.7** `P_prim_set_index_object` — `CreateGEP` +
      `CreateStore`.
- [ ] **R.2.8** `P_prim_sizeof` / `P_prim_sizeof_element` —
      `DataLayout::getTypeAllocSize`.
- [ ] **R.2.9** `P_prim_len` — string detour (`_CG_string_len`);
      otherwise `_CG_list_len` etc. via the typed runtime call.
- [ ] **R.2.10** `P_prim_clone` (non-vector) — alloc + memcpy.
      Vector form: `_CG_prim_clone_vector_runtime` call.
- [ ] **R.2.11** `P_prim_primitive` — name-based dispatch via
      `rvals[1]->sym->name`: `__pyc_c_call__` → CreateCall to
      `rvals[3]->sym->constant`; `__pyc_format_string__` →
      CreateCall to `_CG_format_string`; `__pyc_to_str__` →
      constant string; default fallback → CreateCall to
      `_CG_<name>(rvals[2..])`.
- [ ] **R.2.12** Generic call path (`emit_send_call`) — MPosition
      arg routing, closure unpacking via FIELD_LOAD per formal,
      MPosition-shifted index for extra args.

### Phase R.3 — close C-parity (99/0)

Port `cg.cc`'s `resolve_union_receiver` (line 148) so polymorphic
getters pick a non-nil receiver component (issue 029 echo).  Plus
the voidish-arg cast in send-call args.  At this point the new
emitter is at 99/0 and `cg_normalize_v2` + `cg_ir_v2_emit_llvm.cc`
+ the C.2 multi-inst handlers can be retired.

### Phase R.4 — retire the old paths

- [ ] **R.4.1** Delete `cg_normalize_v2.{cc,h}`,
      `cg_ir_v2_emit_llvm.cc`, `cg_ir_v2_print.cc`,
      `cg_ir_v2_parse.cc`.
- [ ] **R.4.2** Delete most of `cg_view.cc` — keep only
      `CGInstView::kind`, `view_build_fun_blocks`, the diff
      oracle, and the value/type lookup helpers if the new
      emitter wants them.  Drop the multi-inst handlers and
      `cg_v2_emit_llvm_module_view`.
- [ ] **R.4.3** `PYC_LLVM_VIEW2=1` becomes default; the old
      `PYC_LLVM_VIEW=1` and the materialized path are
      removed.
- [ ] **R.4.4** File rename: `cg_view_emit_llvm.cc` →
      `cg_emit_llvm.cc`.
