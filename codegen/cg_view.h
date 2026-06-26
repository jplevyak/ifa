// SPDX-License-Identifier: BSD-3-Clause
//
// cg_view.h — Phase A of CG_VIRTUAL_PLAN.
//
// Accessor-based "virtual" view over FA's converged IF1
// state.  Mirrors the conceptual hierarchy of CGv2Program
// (program → fun → block → inst → value/type) but as
// header-only wrapper types that compute their fields on
// demand from IF1, without materializing CG IR objects.
//
// **Phase A scope (this commit):**
//   - View types declared.
//   - Structural accessors for the program walk
//     (program.funs() → fun.pnodes() → inst.kind()).
//   - `CGInstView::kind()` covers the major cases
//     (terminators, primitives that have a 1:1 IF1 shape).
//     Subtle classifications that depend on
//     cg_normalize_v2's lowering helpers (lower_send_period,
//     lower_send_alloc, etc) are stubbed and return CG2_NOP
//     — they'll fill in over the migration.
//   - A "diff oracle" function `cg_view_diff_report(fa, prog)`
//     computes per-kind histograms on the view side and the
//     materialized side, prints mismatches.  Gated by
//     `PYC_VIEW_DIFF=1` env var; on by default in
//     llvm_codegen_print_ir.
//
// Phase B layers a `CGInstRef` bridge so emit can consume
// either materialized or view.  Phase C makes view the
// primary path.  Phase D deletes the materialized form.
// See ifa/codegen/CG_VIRTUAL_PLAN.md.

#ifndef _cg_view_H_
#define _cg_view_H_

#include "fa.h"
#include "codegen/cg_ir_v2.h"  // for CGv2Op enum

class CGFunView;
class CGBlockView;
class CGInstView;
class CGValueView;
class CGProgramView;
class ViewBuildCtx;
struct CGInstRef;

// Wraps an IF1 Var.  Cheap to copy.
class CGValueView {
 public:
  Var *var;
  explicit CGValueView(Var *v = nullptr) : var(v) {}
  bool is_null() const { return var == nullptr; }
  bool is_constant() const { return var && var->sym && var->sym->is_constant; }
  Sym *type_sym() const { return var ? var->type : nullptr; }
  cchar *cg_string() const;
  cchar *name() const { return (var && var->sym) ? var->sym->name : nullptr; }
};

// Wraps an IF1 PNode.  `kind()` classifies it into a
// CGv2Op (the canonical instruction taxonomy from the
// retired-but-still-named "v2" CG IR).
class CGInstView {
 public:
  PNode *pn;
  explicit CGInstView(PNode *p = nullptr) : pn(p) {}
  bool is_null() const { return pn == nullptr; }
  CGv2Op kind() const;
  // For CG2_PRIM / CG2_C_CALL: returns the primitive's
  // name (e.g. "isinstance", "__pyc_c_call__").
  cchar *prim_name() const;
  // For CG2_BINOP: returns the sub-op (ADD/SUB/MUL/EQ/...).
  // CG2B_NONE if not a binop.
  CGv2BinSub binop_sub() const;
  // For CG2_FIELD_LOAD / CG2_FIELD_STORE: returns the
  // field-slot index in the receiver's struct.  -1 if not
  // a field access.
  int field_index() const;
  // Operand accessors.  rvals[0] is typically the
  // function (for SEND) or the receiver (for period); see
  // CODEGEN_LLVM_CONTRACT.md §2.3.
  int n_rvals() const { return pn ? pn->rvals.n : 0; }
  CGValueView rval(int i) const { return CGValueView(pn ? pn->rvals[i] : nullptr); }
  int n_lvals() const { return pn ? pn->lvals.n : 0; }
  CGValueView lval(int i) const { return CGValueView(pn ? pn->lvals[i] : nullptr); }
  // Liveness — codegen ignores dead PNodes.
  bool is_live() const { return pn && pn->live && pn->fa_live; }
};

// Wraps a sequence of PNodes from a block boundary to the
// next terminator.  Phase B.4: `entry` is the first PNode of
// the block (either `f->entry` for the entry block or a
// Code_LABEL PNode for every other block).  Body and closer
// accessors require a `ViewBuildCtx` whose `view_pn_to_block`
// map has been populated by `view_build_fun_blocks`.
class CGBlockView {
 public:
  PNode *entry;  // first PNode of the block
  explicit CGBlockView(PNode *p = nullptr) : entry(p) {}
  bool is_null() const { return entry == nullptr; }
  // Walk the body PNodes — every PNode from `entry`
  // (exclusive when `entry` is a LABEL) to the closer
  // (exclusive).  Followed by the closer, the next
  // call would land on the next block's entry.
  Vec<PNode *> body_pnodes(ViewBuildCtx &vctx) const;
  // The closer PNode is the last PNode in the block before
  // a CFG exit (cfg_succ.n == 0, or a successor in a
  // different block).  Phase A/B emit's terminator builder
  // reads from here.
  PNode *closer_pnode(ViewBuildCtx &vctx) const;
};

// Wraps a Fun.  Phase B.4: `blocks(vctx)` partitions the
// CFG at LABEL boundaries — same shape the materialized
// `build_block_skeleton` produces (entry block + one block
// per Code_LABEL PNode reachable via cfg_succ).
class CGFunView {
 public:
  Fun *fn;
  explicit CGFunView(Fun *f = nullptr) : fn(f) {}
  bool is_null() const { return fn == nullptr; }
  cchar *name() const { return (fn && fn->sym && fn->sym->name) ? fn->sym->name : "(anon)"; }
  bool is_live() const { return fn && fn->live; }
  // Discover the block-entry PNodes for this Fun.  Returns
  // an empty Vec for a null / dead Fun.  Result order: entry
  // block first, then LABEL blocks in CFG-BFS order (same
  // order the materialized `build_block_skeleton` emits).
  // Idempotent — `view_build_fun_blocks` caches into
  // `vctx`.
  Vec<CGBlockView> blocks(ViewBuildCtx &vctx) const;
};

// Top-level view of the program.  Wraps a FA pointer.
class CGProgramView {
 public:
  FA *fa;
  explicit CGProgramView(FA *f = nullptr) : fa(f) {}
};

// ---------------------------------------------------------------------------
// Diff oracle: classify every live PNode via the view, then
// classify the corresponding materialized side, and compare
// per-kind histograms.  Phase A's correctness check.
// ---------------------------------------------------------------------------
//
// Side-by-side per-instruction matching is left for Phase
// B (it needs a PNode * back-pointer on CGv2Inst).  For
// Phase A, histogram parity is sufficient evidence that
// the classification logic agrees on the program shape.
//
// Returns the number of histogram bins that disagree
// (0 = full match).  Prints details to stderr.
int cg_view_diff_report(FA *fa, CGv2Program *prog);

// ---------------------------------------------------------------------------
// Phase B.6: instruction-level diff oracle.
// ---------------------------------------------------------------------------
//
// `PYC_LLVM_DIFF=1` enables this in `llvm_codegen_print_ir`.
// Granularity: per-Fun, per-block, per-instruction.  For
// each materialized CGv2Inst the view enumeration produces
// a candidate CGInstRef; this oracle pair-walks them in
// emit-order and reports the first per-block divergence.
//
// Reports the test name (passed in by the caller), the Fun
// name, the block name, and the first differing instruction
// (with op + operand-count summary on each side).  Returns
// the total number of divergences found across the
// program (0 = full parity).
//
// What gets compared today (Phase B.6):
//   - op kind (CGv2Op): must match.
//   - sub_op (CGv2BinSub): must match for BINOPs.
//   - rvals/lvals count: must match.
//   - terminator block targets resolve to the same
//     CGv2Block id (when both sides see the same block
//     skeleton, which the side-effecting form of
//     view_build_fun_blocks ensures).
//
// What's deliberately NOT compared (yet — Phase C):
//   - operand identity (the view uses fresh CGv2Value
//     objects; pointer equality with materialized CGv2Value
//     would be impossible).  Operand *names* are compared
//     where they're set.
//   - Phase B.5's CGInstView::kind() best-guess gaps for
//     Code_SEND that the materialized side routes via
//     lower_send shape detection.  Phase C centralizes
//     this and the diff tightens automatically.
//
// `tag` is a short identifier (typically the test name,
// or "(anon)") prefixed to every line for grep-ability
// when running across the suite.
int cg_view_diff_module(FA *fa, CGv2Program *prog, cchar *tag);

// ---------------------------------------------------------------------------
// Phase C.1 — view-driven emit entry point.
// ---------------------------------------------------------------------------
//
// `cg_v2_emit_llvm_module_view(fa, prog)` is the dual of
// `cg_v2_emit_llvm_module(prog)`: it rebuilds prog's
// per-Fun bodies (CGv2Block::body, ::terminator,
// ::phi_by_pred) from the view's enumeration, then runs
// the existing LLVM emit machinery on the rebuilt prog.
// The materialized side's bodies (whatever
// cg_normalize_v2 produced) get OVERWRITTEN.
//
// This is the production path Phase C migrates to.  Phase
// C.1 (this commit) lands the entry point; Phase C.2+
// closes the CGInstView::kind() classification gaps so
// the view-driven output stays at suite baseline (92/7).
//
// Today (C.1): activated only under `PYC_LLVM_VIEW=1`.
// Tests with the gaps closed will round-trip cleanly;
// tests that hit a gap will diverge from the
// materialized output (B.6's diff oracle reports it).
// The default emit path is unchanged.
bool cg_v2_emit_llvm_module_view(FA *fa, CGv2Program *prog);

// Helper: materialize a CGInstRef back into a fresh
// CGv2Inst.  Used by `cg_v2_emit_llvm_module_view` to
// stuff view-derived data into prog's CGv2Block::body
// vectors so the existing emit_inst path sees them.
// GC-allocates the result; caller takes ownership by
// adding it to a Vec.
CGv2Inst *cg_view_ref_to_v2inst(const CGInstRef &r);

// ---------------------------------------------------------------------------
// ViewBuildCtx — Phase B.3 operand-translation cache.
// ---------------------------------------------------------------------------
//
// View-origin instructions need to produce CGv2Value-shaped
// operands so they can flow through the existing emit cases
// (which read `(*ref.rvals)[i]->type`, `->name`, etc.).  The
// cache lives outside `CGInstRef` so a single ViewBuildCtx
// spans an entire program walk — every Var maps to one
// CGv2Value across all instructions that mention it.
//
// Phase B.3 implements the bare wrapper: name, scope, imm,
// escapes.  The `type` field is populated for predefined
// numerics by short-circuit to `prog->t_*`; record/list/tuple
// types are stubbed to `prog->t_ptr` so downstream code that
// reads `->type` sees a usable pointer (the materialized side
// uses CG2T_PTR for records too).  Higher-fidelity type
// translation lands in B.5+ when synthetic-instruction emit
// goes live and the diff oracle drives parity at byte level.
//
// Coupling to the materialized side: `prog` is the same
// CGv2Program the materialized translator built.  The view
// reuses its predefined-type slots (`t_int64`, `t_ptr`, ...)
// but its own `Map<Var*, CGv2Value*>` — so view-origin
// CGv2Value objects are FRESH, parallel to materialized
// ones.  This keeps the two paths cleanly separated; B.6's
// diff oracle compares emitted text, not CGv2Value pointers.
class ViewBuildCtx {
 public:
  CGv2Program *prog;
  Map<Var *, CGv2Value *> var_to_value;
  Map<Sym *, CGv2Type *> sym_to_type;
  Map<Sym *, CGv2Type *> sym_to_struct;  // Type_RECORD underlying struct

  // Phase B.4: block discovery.  `pn_to_block` maps the
  // block-entry PNode (either `f->entry` or a Code_LABEL)
  // to its CGv2Block.  `label_to_block` maps Label* to the
  // same CGv2Block — needed for terminator targets where
  // the goto/if records the Label, not the LABEL PNode.
  // Both are populated by `view_build_fun_blocks`; lookups
  // outside that path return null.
  Map<PNode *, CGv2Block *> pn_to_block;
  Map<Label *, CGv2Block *> label_to_block;

  // Per-Fun visited / discovered sentinel — set after
  // `view_build_fun_blocks` runs once for a given Fun, so
  // repeat calls (e.g. from CGFunView::blocks() invoked
  // mid-emit) become O(1) cache hits.
  Map<Fun *, Vec<CGBlockView> *> fun_blocks;

  // Per-block closer cache.  `view_build_fun_blocks`
  // computes closers eagerly during the walk so
  // `CGBlockView::closer_pnode` and `body_pnodes` don't
  // re-traverse the CFG.  Keyed on the block-entry PNode.
  Map<PNode *, PNode *> entry_to_closer;

  // Phase C.2: the CGv2Fun being lowered, when known.
  // `view_translate_value` reads this to look up formals
  // and locals on the materialized cf before allocating a
  // fresh CGv2Value — keeps the emit's per-Fun value map
  // identity-stable across view and materialized paths.
  // `cg_v2_emit_llvm_module_view` updates this field
  // per-Fun as it walks; consumers outside that flow can
  // leave it null.
  CGv2Fun *current_cf;

  // Phase C.2 (lower_send_call port): the IF1 Fun whose
  // PNodes the lowering is currently walking.  Required
  // for the `caller->calls` lookup that resolves a
  // SEND PNode to a target Fun.  Set by
  // `view_enumerate_fun_insts` and
  // `cg_v2_emit_llvm_module_view` per-Fun.
  Fun *current_fun;

  explicit ViewBuildCtx(CGv2Program *p)
      : prog(p), current_cf(nullptr), current_fun(nullptr) {}
};

// Phase B.4: discover and materialize per-Fun blocks.  For
// each reachable block-entry PNode (Fun::entry plus every
// Code_LABEL reachable via cfg_succ), allocate a fresh
// CGv2Block, name it ("entry" / "L<id>" / "B<id>"), and
// register it in `vctx.pn_to_block` plus `vctx.label_to_block`.
// Idempotent: re-calling for the same Fun returns the
// cached `Vec<CGBlockView>` without re-walking.
//
// `cf` is optional — when non-null, the freshly-created
// CGv2Block objects are also appended to `cf->blocks` so a
// CGv2Fun that was started by the materialized translator
// gains identical block ids.  Passing nullptr is fine when
// the caller only needs the view-side maps.
Vec<CGBlockView> view_build_fun_blocks(CGFunView fv,
                                        ViewBuildCtx &vctx,
                                        CGv2Fun *cf = nullptr);

// ---------------------------------------------------------------------------
// Phase B.5: synthetic-instruction emission.
// ---------------------------------------------------------------------------
//
// Two flavors of instruction don't have a 1:1 PNode mapping
// on the view side:
//
//   1. SSU phi/phy resolution.  IF1 carries `phi` PNodes
//      hanging off a successor block's entry and `phy`
//      PNodes hanging off the predecessor's closer.  Each
//      encodes "MOVE on an edge".  `view_enumerate_phi_moves`
//      walks them and produces a CGv2_MOVE CGInstRef per
//      (pred, succ, phi-pnode) triple — matching
//      cg_normalize_v2.cc:materialize_phi_phy.
//
//   2. Block terminators.  The closer PNode encodes BR /
//      COND_BR / RET semantics but the materialized side
//      always allocates a separate CGv2Inst for the
//      terminator (even when the closer's own Code_kind
//      determines the op).  `view_make_terminator` builds
//      the CGInstRef without going through the closer's
//      `CGInstView::kind()` classification — the latter is
//      designed for body classification and conflates RET
//      with "Code_SEND with P_prim_reply", which is the
//      same shape but a different instruction in the
//      emit's eyes.
//
// `view_enumerate_fun_insts` returns every synthetic +
// real CGInstRef for the Fun in the order the materialized
// emit walks them: per block, body insts → phi/phy MOVEs
// (grouped by the pred edge) → terminator.  This is what
// closes the Phase A histogram-diff gap and what Phase C's
// view-driven emit will iterate over.

// Synthesize CGv2_MOVE CGInstRefs for the phi/phy edges
// landing on `succ_block` from `pred_block`.  Returns the
// full list (one CGInstRef per source PNode).
// Mirrors materialize_phi_phy's inner loop.
Vec<CGInstRef> view_enumerate_phi_moves(PNode *pred_closer,
                                         PNode *succ_entry,
                                         int isucc,
                                         ViewBuildCtx &vctx);

// Phase C.2 multi-inst refactor: lower a body PNode to
// 1..N CGInstRefs, appended to `out`.  Replaces the
// previous "one PNode → one CGInstRef::from_view" model
// for body iteration.  Caller (view_enumerate_fun_insts
// and cg_v2_emit_llvm_module_view) walks the returned
// slice in emit order.
//
// Multi-inst cases (each ports a `lower_send_*` from
// cg_normalize_v2.cc that emits multiple CGv2Insts per
// PNode):
//   - P_prim_make flat-list shape:
//     SIZEOF(elem) + C_CALL(_CG_prim_tuple_list_internal).
//   - Other multi-inst lowerings land here as they get
//     ported.
//
// Single-inst PNodes still flow through `from_view` — the
// multi-inst handler just appends one ref.
void view_lower_pnode(PNode *pn, ViewBuildCtx &vctx,
                       Vec<CGInstRef> &out);

// Synthesize the terminator CGInstRef for a block.  Op
// depends on the closer's Code kind:
//   Code_GOTO       → CG2_BR    (br_target from label[0])
//   Code_IF         → CG2_COND_BR (br_true / br_false +
//                                   rvals[0]=cond)
//   Code_SEND/reply → CG2_RET  (rvals[3]=return value)
//   no closer       → CG2_UNREACHABLE
//   else            → CG2_BR to the single successor
// Identical dispatch to cg_normalize_v2.cc:build_terminator.
CGInstRef view_make_terminator(CGBlockView bv, ViewBuildCtx &vctx);

// Aggregate enumeration: every CGInstRef for the Fun in
// emit-walk order.  Used by the Phase A diff oracle and by
// Phase C's view-driven emit loop.
Vec<CGInstRef> view_enumerate_fun_insts(CGFunView fv,
                                          ViewBuildCtx &vctx);

// Translate an IF1 Var-shaped CGValueView into a CGv2Value.
// Caches in `vctx.var_to_value` so repeated calls for the same
// Var return the same pointer.  Returns nullptr on a null view.
CGv2Value *view_translate_value(CGValueView v, ViewBuildCtx &vctx);

// Translate an IF1 type Sym into a CGv2Type.  Phase B.3:
// covers predefined numerics, void, nil, symbol, string; all
// other shapes return `prog->t_ptr` (matches v1/v2's
// by-pointer record convention).  Idempotent.
CGv2Type *view_translate_type(Sym *s, ViewBuildCtx &vctx);

// Populate a CGv2Immediate from an IF1 Immediate.  Stand-alone
// duplicate of cg_normalize_v2.cc:build_immediate so the view
// side has no dependency on the materialized translator's
// internals.  When Phase D removes the materialized side,
// this is the surviving implementation.
void view_build_immediate(const Immediate &src, CGv2Immediate &dst);

// ---------------------------------------------------------------------------
// CGInstRef — Phase B bridge.
// ---------------------------------------------------------------------------
//
// Field-level adapter that the LLVM emitter consumes
// instead of `CGv2Inst *` directly.  Two factories:
//
//   - `CGInstRef::from_v2(CGv2Inst *)`: production path,
//     drops in for existing materialized callers.  Zero
//     semantic change — just a field-by-field copy.
//
//   - `CGInstRef::from_view(CGInstView)`: emit-from-view
//     factory, partial in Phase B.1.  Synthesizes
//     fields for the shape categories the view fully
//     understands (terminators, simple SENDs with
//     1:1 IF1 mapping).  Returns a CGInstRef with
//     `op == CG2_NOP` for shapes the view doesn't yet
//     cover — caller treats as "skip" / "synthetic
//     instruction not yet emitted by view path."
//
// Phase B.1 (this commit): refactor `emit_terminator`
// to consume `CGInstRef`; both materialized and view
// flows exercise that path.  Other `emit_inst` cases
// remain `CGv2Inst *` and get migrated in B.2-B.6.
struct CGInstRef {
  CGv2Op op = CG2_NOP;
  CGv2BinSub sub_op = CG2B_NONE;

  // Block-target references.  In the materialized path
  // these point to CGv2Block.  In the view path, NULL
  // for now (block-iterator integration is Phase B
  // continuation).
  CGv2Block *br_target = nullptr;
  CGv2Block *br_true = nullptr;
  CGv2Block *br_false = nullptr;

  // Operand vectors — pointers into either materialized
  // CGv2Value lists or future view-side equivalents.
  // Phase B.1 holds CGv2Value* directly; B.3 generalizes.
  Vec<CGv2Value *> *rvals = nullptr;
  Vec<CGv2Value *> *lvals = nullptr;

  // Type / field metadata
  CGv2Type *type_arg = nullptr;
  int field_idx = 0;
  cchar *prim_name = nullptr;

  // Provenance — at most one of these is non-null.
  // Lets emitters that need extra context (e.g. for
  // diagnostics or as-yet-unconverted edge cases) reach
  // through to the source representation.
  CGv2Inst *v2 = nullptr;
  PNode *view_pn = nullptr;

  static CGInstRef from_v2(CGv2Inst *inst);

  // Phase B.3: the from_view factory now takes a
  // ViewBuildCtx so it can populate rvals/lvals via
  // view_translate_value.  Without a ctx, callers get
  // the previous "classification only" behavior (op,
  // sub_op, prim_name, field_idx populated; operands
  // null).  With a ctx, rvals/lvals point at fresh
  // Vec<CGv2Value*> filled from the view's PNode.
  static CGInstRef from_view(CGInstView v, ViewBuildCtx *vctx = nullptr);
};

#endif  // _cg_view_H_
