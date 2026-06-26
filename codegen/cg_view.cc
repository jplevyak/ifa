// SPDX-License-Identifier: BSD-3-Clause
//
// cg_view.cc — Phase A of CG_VIRTUAL_PLAN.
//
// Accessor implementations + the diff oracle.  See
// cg_view.h for the declarations and ifa/codegen/CG_VIRTUAL_PLAN.md
// for the migration phases.

#include "ifadefs.h"

#include "codegen/cg_view.h"

#include "codegen/cg_ir_v2.h"
#include "codegen/codegen_common.h"
#include "fa.h"
#include "fun.h"
#include "pattern.h"  // MPosition + Position2int macro
#include "pdb.h"
#include "pnode.h"
#include "prim.h"

#include <stdio.h>
#include <stdlib.h>

// ---------------------------------------------------------------------------
// CGValueView
// ---------------------------------------------------------------------------

cchar *CGValueView::cg_string() const {
  if (!var) return nullptr;
  if (cg_get_string(var)) return cg_get_string(var);
  if (var->sym && cg_get_string(var->sym)) return cg_get_string(var->sym);
  return nullptr;
}

// ---------------------------------------------------------------------------
// CGInstView::kind() — classify a PNode into CGv2Op.
//
// Mirrors the switch logic in cg_normalize_v2.cc's
// translate_code_send + lower_send_* dispatch chain.
// Phase A covers the unambiguous shapes:
//   - Code_GOTO    → CG2_BR
//   - Code_IF      → CG2_COND_BR
//   - Code_LABEL   → not an instruction; returns CG2_NOP
//   - Code_MOVE    → CG2_MOVE
//   - Code_SEND with arithmetic prim → CG2_BINOP
//   - Code_SEND with P_prim_reply    → CG2_RET (terminator)
//   - Code_SEND with P_prim_period   → CG2_FIELD_LOAD (preliminary;
//                                       cg_normalize_v2 may route to
//                                       CG2_ALLOC for struct-shape;
//                                       Phase A reports best guess)
//   - Code_SEND with P_prim_setter   → CG2_FIELD_STORE (similar caveat)
//   - Code_SEND with P_prim_new / make → CG2_ALLOC
//   - Code_SEND with P_prim_clone    → CG2_CLONE
//   - Code_SEND with P_prim_isinstance → CG2_BINOP (for vs sym_nil_type)
//                                      → CG2_CALL otherwise (Phase A:
//                                                            CG2_BINOP)
//   - Code_SEND with P_prim_is       → CG2_BINOP (EQ)
//   - Code_SEND with P_prim_primitive → CG2_C_CALL or CG2_PRIM
//                                       (Phase A: CG2_C_CALL)
//   - Code_SEND otherwise            → CG2_CALL
//
// Cases that depend on cg_normalize_v2's shape-detection
// (e.g. CG2_INDEX_LOAD vs CG2_FIELD_LOAD for a period on a
// list element) return the most common best-guess kind.
// The Phase A diff report will surface these as histogram
// disagreements and they get refined in Phase B.

static CGv2BinSub prim_index_to_binop(int idx) {
  switch (idx) {
    case P_prim_add:            return CG2B_ADD;
    case P_prim_subtract:       return CG2B_SUB;
    case P_prim_mult:           return CG2B_MUL;
    case P_prim_div:            return CG2B_DIV;
    case P_prim_mod:            return CG2B_MOD;
    case P_prim_lsh:            return CG2B_SHL;
    case P_prim_rsh:            return CG2B_SHR;
    case P_prim_less:           return CG2B_LT;
    case P_prim_lessorequal:    return CG2B_LE;
    case P_prim_greater:        return CG2B_GT;
    case P_prim_greaterorequal: return CG2B_GE;
    case P_prim_equal:          return CG2B_EQ;
    case P_prim_notequal:       return CG2B_NE;
    case P_prim_and:            return CG2B_AND;
    case P_prim_or:             return CG2B_OR;
    case P_prim_xor:            return CG2B_XOR;
    default:                    return CG2B_NONE;
  }
}

CGv2Op CGInstView::kind() const {
  if (!pn || !pn->code) return CG2_NOP;
  switch (pn->code->kind) {
    case Code_GOTO:   return CG2_BR;
    case Code_IF:     return CG2_COND_BR;
    case Code_LABEL:  return CG2_NOP;   // label is a block boundary, not an instruction
    case Code_MOVE:   return CG2_MOVE;
    case Code_SEND: {
      // Phase C.2: constant-folded functional SEND elision.
      // Mirrors cg_normalize_v2.cc:lower_send top-of-fn check:
      // when the result is a single statically-known constant
      // AND the prim is functional (no side effects), the
      // materialized side returns early — no CGv2Inst emitted.
      // Without this elision the view counts hundreds of extra
      // SENDs (mostly type-check and arithmetic-on-constants
      // primitives produced by FA's specialization) that the
      // emit path correctly drops.
      if (pn->prim && !pn->prim->nonfunctional &&
          pn->lvals.n == 1 && get_constant(pn->lvals.v[0])) {
        return CG2_NOP;
      }
      if (!pn->prim) return CG2_CALL;
      int idx = pn->prim->index;
      if (idx == P_prim_reply) return CG2_RET;
      if (prim_index_to_binop(idx) != CG2B_NONE) return CG2_BINOP;
      if (idx == P_prim_period) return CG2_FIELD_LOAD;
      if (idx == P_prim_setter) return CG2_FIELD_STORE;
      if (idx == P_prim_new || idx == P_prim_make) return CG2_ALLOC;
      if (idx == P_prim_clone || idx == P_prim_clone_vector) return CG2_CLONE;
      if (idx == P_prim_isinstance) return CG2_BINOP;  // vs sym_nil_type
      if (idx == P_prim_is) return CG2_BINOP;
      if (idx == P_prim_index_object) return CG2_INDEX_LOAD;
      if (idx == P_prim_set_index_object) return CG2_INDEX_STORE;
      if (idx == P_prim_sizeof) return CG2_SIZEOF;
      if (idx == P_prim_sizeof_element) return CG2_SIZEOF_ELEMENT;
      if (idx == P_prim_len) return CG2_LEN;
      if (idx == P_prim_primitive) return CG2_C_CALL;
      if (idx == P_prim_cast) return CG2_CAST;
      return CG2_CALL;
    }
    default: return CG2_NOP;
  }
}

cchar *CGInstView::prim_name() const {
  if (!pn || !pn->prim) return nullptr;
  return pn->prim->name;
}

CGv2BinSub CGInstView::binop_sub() const {
  if (!pn || !pn->prim) return CG2B_NONE;
  int idx = pn->prim->index;
  if (idx == P_prim_isinstance) return CG2B_EQ;
  if (idx == P_prim_is) return CG2B_EQ;
  return prim_index_to_binop(idx);
}

int CGInstView::field_index() const {
  // The field slot index is the position of the named field
  // in the receiver's struct.  cg_normalize_v2 computes this
  // in lower_send_period / lower_send_setter by scanning
  // recv_type->has[].  Phase A: return -1 (not yet computed
  // here); Phase B will inline the scan.
  return -1;
}

// ---------------------------------------------------------------------------
// Diff oracle: classify all live PNodes via the view, then
// classify the materialized CGv2Program, and compare
// per-kind histograms.
// ---------------------------------------------------------------------------

static cchar *op_name(int op) {
  switch (op) {
    case CG2_NOP: return "NOP";
    case CG2_MOVE: return "MOVE";
    case CG2_BINOP: return "BINOP";
    case CG2_CALL: return "CALL";
    case CG2_ALLOC: return "ALLOC";
    case CG2_FIELD_STORE: return "FIELD_STORE";
    case CG2_FIELD_LOAD: return "FIELD_LOAD";
    case CG2_INDEX_LOAD: return "INDEX_LOAD";
    case CG2_INDEX_STORE: return "INDEX_STORE";
    case CG2_LEN: return "LEN";
    case CG2_CAST: return "CAST";
    case CG2_SIZEOF: return "SIZEOF";
    case CG2_SIZEOF_ELEMENT: return "SIZEOF_ELEMENT";
    case CG2_CLONE: return "CLONE";
    case CG2_C_CALL: return "C_CALL";
    case CG2_PRIM: return "PRIM";
    case CG2_BR: return "BR";
    case CG2_COND_BR: return "COND_BR";
    case CG2_RET: return "RET";
    case CG2_UNREACHABLE: return "UNREACHABLE";
    default: return "?";
  }
}

static const int kNumOps = 24;  // generous bound; CG2_* ops < 24 today

int cg_view_diff_report(FA *fa, CGv2Program *prog) {
  if (!fa || !prog) return 0;
  int view_hist[kNumOps] = {0};
  int v2_hist[kNumOps] = {0};

  // View side: enumerate synthetic + real CGInstRefs per
  // Fun.  Phase B.5 closes the histogram-diff gap by
  // including phi/phy MOVEs and block terminators that
  // don't have a 1:1 PNode mapping.  A separate
  // ViewBuildCtx per call keeps the diff hermetic — its
  // block maps don't leak to subsequent compiles.
  ViewBuildCtx vctx(prog);
  for (Fun *f : fa->funs) {
    if (!f->live) continue;
    Vec<CGInstRef> insts = view_enumerate_fun_insts(CGFunView(f), vctx);
    for (int i = 0; i < insts.n; i++) {
      CGv2Op k = insts.v[i].op;
      if (k >= 0 && k < kNumOps) view_hist[k]++;
    }
  }

  // Materialized side: walk every CGv2Inst in every CGv2Fun.
  for (int fi = 0; fi < prog->funs.n; fi++) {
    CGv2Fun *cf = prog->funs[fi];
    if (!cf) continue;
    for (int bi = 0; bi < cf->blocks.n; bi++) {
      CGv2Block *blk = cf->blocks[bi];
      if (!blk) continue;
      for (int ii = 0; ii < blk->body.n; ii++) {
        CGv2Inst *inst = blk->body[ii];
        if (!inst) continue;
        int k = (int)inst->op;
        if (k >= 0 && k < kNumOps) v2_hist[k]++;
      }
      // Terminators count too.
      if (blk->terminator) {
        int k = (int)blk->terminator->op;
        if (k >= 0 && k < kNumOps) v2_hist[k]++;
      }
    }
  }

  // Compare and report.
  int mismatches = 0;
  fprintf(stderr, "[view-diff] kind         view  materialized  delta\n");
  for (int k = 0; k < kNumOps; k++) {
    if (view_hist[k] == 0 && v2_hist[k] == 0) continue;
    int delta = view_hist[k] - v2_hist[k];
    if (delta != 0) mismatches++;
    fprintf(stderr, "[view-diff]   %-14s %5d  %12d  %+d%s\n",
            op_name(k), view_hist[k], v2_hist[k], delta,
            delta != 0 ? "  MISMATCH" : "");
  }
  fprintf(stderr, "[view-diff] %d bin(s) differ\n", mismatches);
  return mismatches;
}

// ---------------------------------------------------------------------------
// CGInstRef factories — Phase B.1
// ---------------------------------------------------------------------------

CGInstRef CGInstRef::from_v2(CGv2Inst *inst) {
  CGInstRef r;
  if (!inst) return r;
  r.op = inst->op;
  r.sub_op = inst->sub_op;
  r.br_target = inst->br_target;
  r.br_true = inst->br_true;
  r.br_false = inst->br_false;
  r.rvals = &inst->rvals;
  r.lvals = &inst->lvals;
  r.type_arg = inst->type_arg;
  r.field_idx = inst->field_idx;
  r.prim_name = inst->prim_name;
  r.v2 = inst;
  r.view_pn = nullptr;
  return r;
}

// Phase C.2 helpers: per-op operand shaping.  The materialized
// `lower_send_*` family applies operand-index conventions that
// the view's naive "pass through all rvals/lvals" violates.
// These helpers reshape the view-translated operand vectors to
// match the materialized contract for each CGv2Op.

// BINOP from a prim SEND: materialized reads
// `rvals[n-3]` (lhs) and `rvals[n-1]` (rhs) per
// lower_send_binop (line 662-664).  The view passes
// through all rvals; this helper picks the right pair.
static void view_shape_binop(PNode *pn, ViewBuildCtx &vctx,
                              Vec<CGv2Value *> *rv) {
  if (!pn || !rv) return;
  if (pn->rvals.n < 4) return;  // shape mismatch — keep all (caller may fall through)
  Var *lhs = pn->rvals.v[pn->rvals.n - 3];
  Var *rhs = pn->rvals.v[pn->rvals.n - 1];
  rv->clear();
  if (CGv2Value *a = view_translate_value(CGValueView(lhs), vctx)) rv->add(a);
  if (CGv2Value *b = view_translate_value(CGValueView(rhs), vctx)) rv->add(b);
}

// Mirror cg_normalize_v2.cc:compute_prim_arg_offset.
static int view_prim_arg_offset(PNode *pn) {
  if (pn && pn->rvals.n > 0 && pn->rvals.v[0] &&
      pn->rvals.v[0]->sym == sym_primitive) {
    return 2;
  }
  return 1;
}

// P_prim_index_object — port of lower_send_index_load at
// cg_normalize_v2.cc:1227.  String indexing detours to
// `_CG_char_from_string`; constant-index tuple element
// detours to FIELD_LOAD; otherwise INDEX_LOAD.
static bool view_shape_index_load(PNode *pn, ViewBuildCtx &vctx,
                                    CGInstRef &r) {
  int o = view_prim_arg_offset(pn);
  if (pn->rvals.n < o + 2 || pn->lvals.n < 1) return false;
  CGv2Value *obj = view_translate_value(CGValueView(pn->rvals.v[o]), vctx);
  CGv2Value *idx = view_translate_value(CGValueView(pn->rvals.v[o + 1]), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!obj || !idx || !dst) return false;

  Sym *obj_ty = pn->rvals.v[o]->type;
  if (obj_ty &&
      (obj_ty == sym_string ||
       (sym_string && sym_string->specializers.set_in(obj_ty)))) {
    // String index: route to `_CG_char_from_string`.
    r.op = CG2_C_CALL;
    r.prim_name = "_CG_char_from_string";
    r.type_arg = dst->type ? dst->type :
                 (vctx.prog ? vctx.prog->t_ptr : nullptr);
    Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
    rv->add(obj); rv->add(idx);
    r.rvals = rv;
    Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
    lv->add(dst);
    r.lvals = lv;
    return true;
  }

  Var *idx_var = pn->rvals.v[o + 1];
  if (obj_ty && obj_ty->type_kind == Type_RECORD && obj_ty->has.n &&
      idx_var && idx_var->sym && idx_var->sym->constant) {
    int fi = (int)strtol(idx_var->sym->constant, nullptr, 10);
    if (fi >= 0 && fi < obj_ty->has.n) {
      r.op = CG2_FIELD_LOAD;
      r.field_idx = fi;
      r.type_arg = view_translate_type(obj_ty, vctx);
      Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
      rv->add(obj);
      r.rvals = rv;
      Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
      lv->add(dst);
      r.lvals = lv;
      return true;
    }
  }

  r.op = CG2_INDEX_LOAD;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(obj); rv->add(idx);
  r.rvals = rv;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// P_prim_set_index_object — port of lower_send_index_store.
static bool view_shape_index_store(PNode *pn, ViewBuildCtx &vctx,
                                     CGInstRef &r) {
  int o = view_prim_arg_offset(pn);
  if (pn->rvals.n < o + 3) return false;
  CGv2Value *obj = view_translate_value(CGValueView(pn->rvals.v[o]), vctx);
  CGv2Value *idx = view_translate_value(CGValueView(pn->rvals.v[o + 1]), vctx);
  CGv2Value *val = view_translate_value(CGValueView(pn->rvals.v[o + 2]), vctx);
  if (!obj || !idx || !val) return false;
  r.op = CG2_INDEX_STORE;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(obj); rv->add(idx); rv->add(val);
  r.rvals = rv;
  r.lvals = new Vec<CGv2Value *>;
  return true;
}

// P_prim_sizeof — sizeof(type at rvals[o]).
static bool view_shape_sizeof(PNode *pn, ViewBuildCtx &vctx,
                                CGInstRef &r) {
  int o = view_prim_arg_offset(pn);
  if (o >= pn->rvals.n || pn->lvals.n < 1) return false;
  Var *type_var = pn->rvals.v[o];
  if (!type_var || !type_var->type) return false;
  CGv2Type *t = view_translate_type(type_var->type, vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!t || !dst) return false;
  r.op = CG2_SIZEOF;
  r.type_arg = t;
  r.rvals = new Vec<CGv2Value *>;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// P_prim_sizeof_element — sizeof(element of rvals[o]).
static bool view_shape_sizeof_element(PNode *pn, ViewBuildCtx &vctx,
                                        CGInstRef &r) {
  int o = view_prim_arg_offset(pn);
  if (o >= pn->rvals.n || pn->lvals.n < 1) return false;
  CGv2Value *src = view_translate_value(CGValueView(pn->rvals.v[o]), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!src || !dst) return false;
  r.op = CG2_SIZEOF_ELEMENT;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(src);
  r.rvals = rv;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// P_prim_len — len(obj).  Preferentially picks the
// string-typed rval (mirrors lower_send_len at
// cg_normalize_v2.cc:1522).
static bool view_shape_len(PNode *pn, ViewBuildCtx &vctx,
                             CGInstRef &r) {
  if (pn->lvals.n < 1) return false;
  Var *obj_var = nullptr;
  for (int i = 1; i < pn->rvals.n; i++) {
    Var *v = pn->rvals.v[i];
    if (!v || !v->type) continue;
    if (v->type == sym_string ||
        (sym_string && sym_string->specializers.set_in(v->type))) {
      obj_var = v;
      break;
    }
  }
  if (!obj_var) {
    int o = view_prim_arg_offset(pn);
    if (o >= pn->rvals.n) return false;
    obj_var = pn->rvals.v[o];
  }
  CGv2Value *obj = view_translate_value(CGValueView(obj_var), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!obj || !dst) return false;
  r.op = CG2_LEN;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(obj);
  r.rvals = rv;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// P_prim_clone (non-vector) — port of lower_send_clone at
// cg_normalize_v2.cc:1169.  The vector path
// (P_prim_clone_vector) needs SIZEOF + C_CALL multi-inst
// — left for the multi-inst refactor.
static bool view_shape_clone(PNode *pn, ViewBuildCtx &vctx,
                               CGInstRef &r) {
  if (pn->lvals.n < 1) return false;
  if (pn->prim && pn->prim->index == P_prim_clone_vector) return false;
  int o = view_prim_arg_offset(pn);
  if (pn->rvals.n <= o) return false;
  CGv2Value *proto = view_translate_value(CGValueView(pn->rvals.v[o]), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!proto || !dst || !dst->type) return false;
  CGv2Type *t = dst->type;
  CGv2Type *struct_t =
      (t && t->kind == CG2T_PTR && t->element) ? t->element : t;
  if (!struct_t ||
      (struct_t->kind != CG2T_STRUCT &&
       struct_t->kind != CG2T_VECTOR)) {
    return false;
  }
  r.op = CG2_CLONE;
  r.type_arg = struct_t;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(proto);
  r.rvals = rv;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// P_prim_lnot — port of lower_send_lnot at
// cg_normalize_v2.cc:1446.  Emits CG2_BINOP EQ with a
// zero constant of the operand's type.
static bool view_shape_lnot(PNode *pn, ViewBuildCtx &vctx,
                              CGInstRef &r) {
  if (pn->rvals.n < 1 || pn->lvals.n < 1) return false;
  CGv2Value *src = view_translate_value(
      CGValueView(pn->rvals.v[pn->rvals.n - 1]), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!src || !dst || !src->type) return false;
  CGv2Value *zero = new CGv2Value();
  zero->name = "zero";
  zero->type = src->type;
  zero->scope = CG2V_CONSTANT;
  zero->imm.kind = (src->type->kind == CG2T_FLOAT)
                       ? CGv2Immediate::I_FLOAT
                       : CGv2Immediate::I_INT;
  if (src->type->kind == CG2T_FLOAT) zero->imm.v.f = 0.0;
  else zero->imm.v.i = 0;
  if (vctx.prog) vctx.prog->constants.add(zero);
  r.op = CG2_BINOP;
  r.sub_op = CG2B_EQ;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(src); rv->add(zero);
  r.rvals = rv;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// P_prim_minus (unary `-x`) — port of lower_send_neg at
// cg_normalize_v2.cc:1407.  Emits CG2_BINOP SUB with a
// fresh zero constant as lhs.
static bool view_shape_neg(PNode *pn, ViewBuildCtx &vctx,
                             CGInstRef &r) {
  if (pn->rvals.n < 1 || pn->lvals.n < 1) return false;
  CGv2Value *src = view_translate_value(
      CGValueView(pn->rvals.v[pn->rvals.n - 1]), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!src || !dst || !src->type) return false;
  CGv2Value *zero = new CGv2Value();
  zero->name = "zero";
  zero->type = src->type;
  zero->scope = CG2V_CONSTANT;
  zero->imm.kind = (src->type->kind == CG2T_FLOAT)
                       ? CGv2Immediate::I_FLOAT
                       : CGv2Immediate::I_INT;
  if (src->type->kind == CG2T_FLOAT) zero->imm.v.f = 0.0;
  else zero->imm.v.i = 0;
  if (vctx.prog) vctx.prog->constants.add(zero);
  r.op = CG2_BINOP;
  r.sub_op = CG2B_SUB;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(zero); rv->add(src);
  r.rvals = rv;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// P_prim_coerce — port of lower_send_coerce at
// cg_normalize_v2.cc:1479.  Reads (rvals[n-2]) for the
// target type Sym, (rvals[n-1]) for the source value.
static bool view_shape_coerce(PNode *pn, ViewBuildCtx &vctx,
                                CGInstRef &r) {
  if (pn->rvals.n < 2 || pn->lvals.n < 1) return false;
  Var *tgt_var = pn->rvals.v[pn->rvals.n - 2];
  Var *src_var = pn->rvals.v[pn->rvals.n - 1];
  if (!tgt_var || !tgt_var->sym || !src_var) return false;
  Sym *tgt_sym = tgt_var->sym->is_meta_type
                     ? tgt_var->sym->meta_type
                     : tgt_var->sym;
  tgt_sym = unalias_type(tgt_sym);
  if (!tgt_sym) return false;
  CGv2Type *dst_ty = view_translate_type(tgt_sym, vctx);
  CGv2Value *src = view_translate_value(CGValueView(src_var), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!dst_ty || !src || !dst) return false;
  r.op = CG2_CAST;
  r.type_arg = dst_ty;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  rv->add(src);
  r.rvals = rv;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  lv->add(dst);
  r.lvals = lv;
  return true;
}

// __pyc_c_call__(ret_type, fn_name, ret_type_sym, arg_ty,
// arg_val, ...) — pyc's generic FFI primitive.  Mirrors
// lower_send_c_call at cg_normalize_v2.cc:1702.
//   rvals[0] = sym_primitive marker
//   rvals[1] = "__pyc_c_call__" prim name
//   rvals[2] = return type Sym
//   rvals[3] = fn name (constant string Sym)
//   rvals[5,7,...] = arg values (every other, skipping arg-type Syms)
static bool view_shape_c_call_pyc(PNode *pn, ViewBuildCtx &vctx,
                                    CGInstRef &r) {
  if (pn->rvals.n < 4) return false;
  Var *name_var = pn->rvals.v[3];
  if (!name_var || !name_var->sym || !name_var->sym->constant) return false;
  r.op = CG2_C_CALL;
  r.prim_name = name_var->sym->constant;

  // type_arg from result type, falling back to rvals[2]'s
  // meta-type Sym (the explicit return-type arg).
  Vec<CGv2Value *> *new_lv = new Vec<CGv2Value *>;
  if (pn->lvals.n > 0 && pn->lvals.v[0]) {
    CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
    if (dst) {
      r.type_arg = dst->type;
      new_lv->add(dst);
    }
  }
  if (!r.type_arg && pn->rvals.n > 2 && pn->rvals.v[2] &&
      pn->rvals.v[2]->sym) {
    Sym *t = pn->rvals.v[2]->sym->is_meta_type
                 ? pn->rvals.v[2]->sym->meta_type
                 : pn->rvals.v[2]->sym;
    r.type_arg = view_translate_type(t, vctx);
  }
  if (!r.type_arg && vctx.prog) r.type_arg = vctx.prog->t_void;
  r.lvals = new_lv;

  // Args: every other rval starting at 5 (skip arg-type Syms).
  Vec<CGv2Value *> *new_rv = new Vec<CGv2Value *>;
  for (int i = 5; i < pn->rvals.n; i += 2) {
    Var *v = pn->rvals.v[i];
    if (!v) continue;
    if (CGv2Value *cv = view_translate_value(CGValueView(v), vctx))
      new_rv->add(cv);
  }
  r.rvals = new_rv;
  return true;
}

// Default named-prim fallback: route to `_CG_<name>`
// runtime helper.  Mirrors the tail of lower_send_prim
// at cg_normalize_v2.cc:1930.
//   rvals[0] = sym_primitive marker
//   rvals[1] = prim name
//   rvals[2..] = args
static void view_shape_default_named_prim(PNode *pn, cchar *name,
                                            ViewBuildCtx &vctx,
                                            CGInstRef &r) {
  r.op = CG2_C_CALL;
  char helper[256];
  snprintf(helper, sizeof(helper), "_CG_%s", name);
  r.prim_name = dupstr(helper);

  Vec<CGv2Value *> *new_lv = new Vec<CGv2Value *>;
  if (pn->lvals.n > 0 && pn->lvals.v[0]) {
    CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
    if (dst) {
      r.type_arg = dst->type;
      new_lv->add(dst);
    }
  }
  if (!r.type_arg && vctx.prog) r.type_arg = vctx.prog->t_void;
  r.lvals = new_lv;

  Vec<CGv2Value *> *new_rv = new Vec<CGv2Value *>;
  for (int i = 2; i < pn->rvals.n; i++) {
    Var *v = pn->rvals.v[i];
    if (!v) continue;
    if (CGv2Value *cv = view_translate_value(CGValueView(v), vctx))
      new_rv->add(cv);
  }
  r.rvals = new_rv;
}

// Mirror cg_normalize_v2.cc:resolve_field_index_v2 — port
// `lower_send_period`'s shape detection.  Returns the
// field-slot index, or -1 if the obj's type doesn't carry
// the named symbol (in which case lower_send_period falls
// through to CG2_CALL).
static int view_resolve_field_index(PNode *pn,
                                     int field_rval_idx,
                                     int obj_rval_idx) {
  if (!pn) return -1;
  if (field_rval_idx >= pn->rvals.n || obj_rval_idx >= pn->rvals.n)
    return -1;
  Var *field_var = pn->rvals.v[field_rval_idx];
  Var *obj_var = pn->rvals.v[obj_rval_idx];
  if (!field_var || !obj_var || !obj_var->type) return -1;
  Sym *obj = obj_var->type;
  if (obj->type_kind == Type_SUM && obj->has.n) obj = obj->has.v[0]->type;
  cchar *symbol = nullptr;
  if (field_var->sym && field_var->sym->is_symbol)
    symbol = field_var->sym->name;
  if (!symbol) {
    Vec<Sym *> symbols;
    symbol_info(field_var, symbols);
    if (symbols.n == 1) symbol = symbols.v[0]->name;
  }
  if (!symbol) return -1;
  for (int i = 0; i < obj->has.n; i++) {
    if (symbol == obj->has.v[i]->name) return i;
  }
  return -1;
}

// P_prim_period — port of lower_send_period at
// cg_normalize_v2.cc:711.  Returns true and reshapes
// `r` to a CG2_FIELD_LOAD when the obj's struct shape
// resolves the named field.  False → caller falls
// through to CG2_CALL (view leaves r alone, except for
// op which it has to flip).
static bool view_shape_period(PNode *pn, ViewBuildCtx &vctx,
                                CGInstRef &r) {
  int fi = view_resolve_field_index(pn, 3, 1);
  if (fi < 0 || pn->lvals.n < 1) {
    // Shape mismatch — fall through to CG2_CALL.  The
    // materialized side flows this PNode through
    // lower_send_call which produces a CG2_CALL with
    // rvals[0]=fn_ref, rvals[1..]=args.  Re-shaping that
    // here is involved; for now flip the op and let the
    // emit's CALL case do its best.  Closer parity is
    // future C.2 work.
    r.op = CG2_CALL;
    return false;
  }
  CGv2Value *obj = view_translate_value(CGValueView(pn->rvals.v[1]), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!obj || !dst) return false;
  r.op = CG2_FIELD_LOAD;
  r.field_idx = fi;
  r.type_arg = (obj->type && obj->type->kind == CG2T_PTR &&
                obj->type->element)
                   ? obj->type->element
                   : obj->type;
  Vec<CGv2Value *> *new_rv = new Vec<CGv2Value *>;
  new_rv->add(obj);
  r.rvals = new_rv;
  Vec<CGv2Value *> *new_lv = new Vec<CGv2Value *>;
  new_lv->add(dst);
  r.lvals = new_lv;
  return true;
}

// P_prim_setter — port of lower_send_setter at
// cg_normalize_v2.cc:795.  Reshapes `r` to a
// CG2_FIELD_STORE when the obj's struct shape resolves
// the named field.
static bool view_shape_setter(PNode *pn, ViewBuildCtx &vctx,
                                CGInstRef &r) {
  int fi = view_resolve_field_index(pn, 3, 1);
  if (fi < 0 || pn->rvals.n < 5) {
    r.op = CG2_CALL;
    return false;
  }
  CGv2Value *obj = view_translate_value(CGValueView(pn->rvals.v[1]), vctx);
  CGv2Value *val = view_translate_value(CGValueView(pn->rvals.v[4]), vctx);
  if (!obj || !val) return false;
  r.op = CG2_FIELD_STORE;
  r.field_idx = fi;
  r.type_arg = (obj->type && obj->type->kind == CG2T_PTR &&
                obj->type->element)
                   ? obj->type->element
                   : obj->type;
  Vec<CGv2Value *> *new_rv = new Vec<CGv2Value *>;
  new_rv->add(obj);
  new_rv->add(val);
  r.rvals = new_rv;
  // Note: lower_send_setter also forwards val into
  // lvals[0] via a separate CG2_MOVE inst (chained
  // assignment).  That second instruction would need
  // injection by the per-block enumeration; today we
  // omit it.  Chained-assignment behavior may not
  // match the materialized side exactly until C.2 adds
  // multi-inst emission from a single PNode.
  r.lvals = new Vec<CGv2Value *>;
  return true;
}

// "isinstance" / "is" with sym_nil_type → CG2_BINOP EQ
// vs NULL.  Mirrors lower_send_prim's first dispatch at
// cg_normalize_v2.cc:1812.  Returns true if it handled.
static bool view_shape_isinstance_or_is(PNode *pn, cchar *name,
                                          ViewBuildCtx &vctx,
                                          CGInstRef &r) {
  if (pn->rvals.n < 4 || pn->lvals.n < 1) return false;
  bool is_isinstance =
      strcmp(name, "isinstance") == 0 &&
      pn->rvals.v[3] && pn->rvals.v[3]->sym == sym_nil_type;
  bool is_is = strcmp(name, "is") == 0;
  if (!is_isinstance && !is_is) return false;

  Var *lhs = pn->rvals.v[2];
  Var *rhs = is_isinstance ? nullptr : pn->rvals.v[3];
  CGv2Value *a = view_translate_value(CGValueView(lhs), vctx);
  if (!a) return false;
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!dst) return false;

  CGv2Value *b = nullptr;
  if (is_isinstance) {
    // Synthesize a NULL constant of the lhs type.
    if (!a->type) return false;
    b = new CGv2Value();
    b->name = "null";
    b->type = a->type;
    b->scope = CG2V_CONSTANT;
    b->imm.kind = CGv2Immediate::I_NIL;
    b->imm.v.i = 0;
    if (vctx.prog) vctx.prog->constants.add(b);
  } else {
    b = view_translate_value(CGValueView(rhs), vctx);
    if (!b) return false;
  }

  r.op = CG2_BINOP;
  r.sub_op = CG2B_EQ;
  Vec<CGv2Value *> *new_rv = new Vec<CGv2Value *>;
  new_rv->add(a);
  new_rv->add(b);
  r.rvals = new_rv;
  Vec<CGv2Value *> *new_lv = new Vec<CGv2Value *>;
  new_lv->add(dst);
  r.lvals = new_lv;
  return true;
}

// Top-level dispatch for P_prim_primitive / isinstance / is
// SENDs.  Reads the prim name from rvals[1] and routes.
// Mirrors lower_send_prim's structure.
static void view_dispatch_send_prim(PNode *pn, ViewBuildCtx &vctx,
                                      CGInstRef &r) {
  if (pn->rvals.n < 2) return;
  Var *name_var = pn->rvals.v[1];
  if (!name_var || !name_var->sym) return;
  cchar *name = name_var->sym->name
                    ? name_var->sym->name
                    : name_var->sym->constant;
  if (!name) return;

  // Direct prim dispatch — isinstance/is with the specific
  // shapes; if not handled, fall through to the named-prim
  // / c_call routing below.
  if (view_shape_isinstance_or_is(pn, name, vctx, r)) return;
  if (strcmp(name, "__pyc_c_call__") == 0 &&
      view_shape_c_call_pyc(pn, vctx, r)) return;
  // Default: `_CG_<name>(args)` — the materialized side
  // emits this for every unrecognized prim.
  view_shape_default_named_prim(pn, name, vctx, r);
}

CGInstRef CGInstRef::from_view(CGInstView v, ViewBuildCtx *vctx) {
  // Phase B.1 → B.4 → C.2: classification fields are
  // populated for any non-null view.  When a ViewBuildCtx
  // is also provided (B.3), `rvals` / `lvals` are filled
  // with freshly-translated CGv2Value*'s from the view's
  // PNode.  When the ctx also carries populated block
  // maps (B.4), GOTO/IF targets resolve to CGv2Block*'s
  // via `label_to_block`.  C.2 layers per-op operand
  // shaping (BINOP rval trimming, prim-name dispatch
  // for C_CALL etc.) on top.  Without a ctx, operands
  // stay null — callers in that path will trip the
  // `if (!ref.rvals || !ref.lvals) return;` guard at
  // the top of emit_inst.
  CGInstRef r;
  if (v.is_null()) return r;
  r.op = v.kind();
  r.sub_op = v.binop_sub();
  r.prim_name = v.prim_name();
  r.field_idx = v.field_index();
  r.view_pn = v.pn;
  r.v2 = nullptr;

  if (vctx) {
    // Allocate the operand vectors GC-side so the caller
    // can hold them past the CGInstRef's frame.  Each
    // PNode gets its own pair — no shared / cached
    // backing.  Cheap (n_rvals + n_lvals small).
    Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
    Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
    for (int i = 0; i < v.n_rvals(); i++) {
      CGv2Value *cv = view_translate_value(v.rval(i), *vctx);
      if (cv) rv->add(cv);
    }
    for (int i = 0; i < v.n_lvals(); i++) {
      CGv2Value *cv = view_translate_value(v.lval(i), *vctx);
      if (cv) lv->add(cv);
    }
    r.rvals = rv;
    r.lvals = lv;

    // Phase C.2: per-op operand shaping.  Runs after the
    // naive pass-through so handlers can reshape rather
    // than build from scratch.  Each branch matches a
    // `lower_send_*` helper in cg_normalize_v2.cc.
    // Scoped narrowly to ops where the materialized
    // contract is clearly known — broader handling
    // (type_arg derivation for ALLOC / CLONE / CAST,
    // FIELD_LOAD/STORE shape detection) lands in
    // follow-up commits after the diff oracle confirms
    // them.
    PNode *pn = v.pn;
    if (pn && pn->code && pn->code->kind == Code_SEND && pn->prim) {
      int idx = pn->prim->index;
      if (r.op == CG2_BINOP) {
        // BINOP from arithmetic / comparison prim: trim
        // to (rvals[n-3], rvals[n-1]).
        view_shape_binop(pn, *vctx, rv);
      } else if (idx == P_prim_primitive ||
                 idx == P_prim_isinstance ||
                 idx == P_prim_is) {
        view_dispatch_send_prim(pn, *vctx, r);
      } else if (idx == P_prim_period) {
        view_shape_period(pn, *vctx, r);
      } else if (idx == P_prim_setter) {
        view_shape_setter(pn, *vctx, r);
      } else if (idx == P_prim_index_object) {
        view_shape_index_load(pn, *vctx, r);
      } else if (idx == P_prim_set_index_object) {
        view_shape_index_store(pn, *vctx, r);
      } else if (idx == P_prim_sizeof) {
        view_shape_sizeof(pn, *vctx, r);
      } else if (idx == P_prim_sizeof_element) {
        view_shape_sizeof_element(pn, *vctx, r);
      } else if (idx == P_prim_len) {
        view_shape_len(pn, *vctx, r);
      } else if (idx == P_prim_clone || idx == P_prim_clone_vector) {
        view_shape_clone(pn, *vctx, r);
      } else if (idx == P_prim_lnot) {
        view_shape_lnot(pn, *vctx, r);
      } else if (idx == P_prim_minus) {
        view_shape_neg(pn, *vctx, r);
      } else if (idx == P_prim_coerce) {
        view_shape_coerce(pn, *vctx, r);
      } else if (r.op == CG2_ALLOC || r.op == CG2_CLONE ||
                 r.op == CG2_CAST) {
        // type_arg = result's struct type.  Mirrors
        // lower_send_alloc / lower_send_clone setting
        // inst->type_arg from build_var(pn->lvals[0])->type.
        if (!r.type_arg && pn->lvals.n > 0 && pn->lvals.v[0] &&
            pn->lvals.v[0]->type) {
          r.type_arg = view_translate_type(pn->lvals.v[0]->type, *vctx);
        }
        // ALLOC contract: rvals=() — the materialized
        // lower_send_alloc emits an empty rvals.  View
        // currently passes through `__primitive` marker
        // and other Vars; trim to match.
        if (r.op == CG2_ALLOC) rv->clear();
      }
    }

    // Phase B.4: resolve branch targets via the ctx's
    // label_to_block.  Mirrors `build_terminator` in
    // cg_normalize_v2.cc — IF1's Code records label[0]
    // (goto target / true branch) and label[1] (false
    // branch); cfg_succ ordering is a less-reliable
    // fallback and is intentionally not used here.
    if (v.pn && v.pn->code) {
      Code *cd = v.pn->code;
      if (cd->kind == Code_GOTO) {
        if (cd->label[0]) {
          r.br_target = vctx->label_to_block.get(cd->label[0]);
        }
      } else if (cd->kind == Code_IF) {
        if (cd->label[0]) {
          r.br_true = vctx->label_to_block.get(cd->label[0]);
        }
        if (cd->label[1]) {
          r.br_false = vctx->label_to_block.get(cd->label[1]);
        }
      }
    }
  }
  return r;
}

// ---------------------------------------------------------------------------
// ViewBuildCtx-backed translation — Phase B.3
// ---------------------------------------------------------------------------

void view_build_immediate(const Immediate &src, CGv2Immediate &dst) {
  // Stand-alone duplicate of cg_normalize_v2.cc:build_immediate.
  // See the comment on `view_build_immediate` in cg_view.h for
  // why the view side carries its own copy.
  switch (src.const_kind) {
    case IF1_NUM_KIND_INT: {
      dst.kind = CGv2Immediate::I_INT;
      switch (src.num_index) {
        case IF1_INT_TYPE_1:  dst.v.i = src.v_bool ? 1 : 0;  break;
        case IF1_INT_TYPE_8:  dst.v.i = src.v_int8;          break;
        case IF1_INT_TYPE_16: dst.v.i = src.v_int16;         break;
        case IF1_INT_TYPE_32: dst.v.i = src.v_int32;         break;
        case IF1_INT_TYPE_64: dst.v.i = src.v_int64;         break;
        default:              dst.v.i = 0;                   break;
      }
      break;
    }
    case IF1_NUM_KIND_UINT: {
      if (src.num_index == IF1_INT_TYPE_1) {
        dst.kind = CGv2Immediate::I_BOOL;
        dst.v.b = src.v_bool;
        break;
      }
      dst.kind = CGv2Immediate::I_UINT;
      switch (src.num_index) {
        case IF1_INT_TYPE_8:  dst.v.u = src.v_uint8;  break;
        case IF1_INT_TYPE_16: dst.v.u = src.v_uint16; break;
        case IF1_INT_TYPE_32: dst.v.u = src.v_uint32; break;
        case IF1_INT_TYPE_64: dst.v.u = src.v_uint64; break;
        default:              dst.v.u = 0;            break;
      }
      break;
    }
    case IF1_NUM_KIND_FLOAT: {
      dst.kind = CGv2Immediate::I_FLOAT;
      switch (src.num_index) {
        case IF1_FLOAT_TYPE_32: dst.v.f = src.v_float32; break;
        case IF1_FLOAT_TYPE_64: dst.v.f = src.v_float64; break;
        default:                dst.v.f = 0.0;          break;
      }
      break;
    }
    case IF1_CONST_KIND_STRING:
      dst.kind = CGv2Immediate::I_STR;
      dst.str = src.v_string;
      break;
    case IF1_CONST_KIND_SYMBOL:
      dst.kind = CGv2Immediate::I_SYM;
      dst.str = src.v_string;
      break;
    default:
      dst.kind = CGv2Immediate::I_NONE;
      break;
  }
}

// Map a predefined numeric Sym to the program's canonical
// CGv2Type slot.  Mirrors cg_normalize_v2.cc:predef_numeric
// but reads from `prog->t_*` directly.  Returns nullptr for
// non-numeric Syms.
static CGv2Type *view_predef_numeric(Sym *s, CGv2Program *prog) {
  if (!s || !prog) return nullptr;
  switch (s->num_kind) {
    case IF1_NUM_KIND_UINT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:  return prog->t_bool;
        case IF1_INT_TYPE_8:  return prog->t_uint8;
        case IF1_INT_TYPE_16: return prog->t_uint16;
        case IF1_INT_TYPE_32: return prog->t_uint32;
        case IF1_INT_TYPE_64: return prog->t_uint64;
      }
      break;
    case IF1_NUM_KIND_INT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:  return prog->t_bool;
        case IF1_INT_TYPE_8:  return prog->t_int8;
        case IF1_INT_TYPE_16: return prog->t_int16;
        case IF1_INT_TYPE_32: return prog->t_int32;
        case IF1_INT_TYPE_64: return prog->t_int64;
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      switch (s->num_index) {
        case IF1_FLOAT_TYPE_32: return prog->t_float32;
        case IF1_FLOAT_TYPE_64: return prog->t_float64;
      }
      break;
    default:
      break;
  }
  return nullptr;
}

// Look up a type by name in prog->types.  Returns the
// existing CGv2Type if found, otherwise nullptr.  Used to
// share identity with materialized types built by
// cg_normalize_v2.  Cheap linear scan (types vector is
// small; the materialized side uses prog->lookup_type
// internally for the same purpose).
static CGv2Type *view_lookup_type_by_name(CGv2Program *prog,
                                             cchar *name) {
  if (!prog || !name) return nullptr;
  for (int i = 0; i < prog->types.n; i++) {
    CGv2Type *t = prog->types.v[i];
    if (t && t->name && strcmp(t->name, name) == 0) return t;
  }
  return nullptr;
}

// Forward decl — view_translate_type and
// view_translate_struct call each other recursively.
CGv2Type *view_translate_type(Sym *s, ViewBuildCtx &vctx);

// Phase C.2: port of cg_normalize_v2.cc:build_struct_type
// (line 104).  Resolves the CG2T_STRUCT (or CG2T_VECTOR)
// for a Type_RECORD Sym.  When the materialized translator
// has already built this struct (the common case in Phase
// B/C where both paths run), prefer the existing CGv2Type
// via prog->types name match — identity stability matters
// for the emit's LLVM type cache (keyed on CGv2Type*).
static CGv2Type *view_translate_struct(Sym *s, ViewBuildCtx &vctx) {
  if (!s || !vctx.prog) return nullptr;
  if (CGv2Type *cached = vctx.sym_to_struct.get(s)) return cached;

  // Match-by-name against prog->types — the materialized
  // build_struct_type names each as "<sym_name>.<sym_id>",
  // so the lookup is deterministic.
  cchar *raw_name = s->name ? s->name :
                    (s->cg_string ? s->cg_string : "anon");
  char buf[160];
  snprintf(buf, sizeof(buf), "%s.%d", raw_name, s->id);
  if (CGv2Type *existing = view_lookup_type_by_name(vctx.prog, buf)) {
    vctx.sym_to_struct.put(s, existing);
    return existing;
  }

  // Not in prog->types yet — build a fresh CGv2Type.  This
  // path runs when the materialized translator didn't
  // register the struct (rare in Phase B; e.g. when the
  // view-driven emit runs on a Fun whose body the
  // materialized side skipped).
  CGv2Type *t = new CGv2Type();
  t->id = 1000 + vctx.prog->types.n;
  t->name = dupstr(buf);
  t->kind = s->is_vector ? CG2T_VECTOR : CG2T_STRUCT;
  t->is_heap_aggregate = s->is_vector ? true : !s->is_value_type;
  vctx.sym_to_struct.put(s, t);     // register first (recursion guard)
  vctx.prog->types.add(t);
  int idx = 0;
  for (Sym *f : s->has) {
    CGv2TypeField *cf = new CGv2TypeField();
    cf->name = f->name;
    CGv2Type *ft = view_translate_type(f->type, vctx);
    // Substitute t_ptr for unsized field types (matches
    // cg_normalize_v2.cc:144).
    if (!ft || ft->kind == CG2T_VOID) ft = vctx.prog->t_ptr;
    cf->type = ft;
    cf->idx = idx++;
    t->fields.add(cf);
  }
  if (s->element) t->element = view_translate_type(s->element->type, vctx);
  return t;
}

// Closure-struct flavor of view_translate_struct.  The
// materialized side (cg_normalize_v2.cc:227) names these
// "closure_<sym>.<id>" instead of "<sym>.<id>".
static CGv2Type *view_translate_closure_struct(Sym *s, ViewBuildCtx &vctx) {
  if (!s || !vctx.prog) return nullptr;
  if (CGv2Type *cached = vctx.sym_to_struct.get(s)) return cached;
  cchar *raw_name = s->name ? s->name :
                    (s->cg_string ? s->cg_string : "closure");
  char buf[160];
  snprintf(buf, sizeof(buf), "closure_%s.%d", raw_name, s->id);
  if (CGv2Type *existing = view_lookup_type_by_name(vctx.prog, buf)) {
    vctx.sym_to_struct.put(s, existing);
    return existing;
  }
  CGv2Type *t = new CGv2Type();
  t->id = 1000 + vctx.prog->types.n;
  t->name = dupstr(buf);
  t->kind = CG2T_STRUCT;
  t->is_heap_aggregate = true;
  vctx.sym_to_struct.put(s, t);
  vctx.prog->types.add(t);
  int idx = 0;
  for (Sym *f : s->has) {
    CGv2TypeField *cf = new CGv2TypeField();
    cf->name = f->name;
    CGv2Type *ft = view_translate_type(f->type, vctx);
    if (!ft || ft->kind == CG2T_VOID) ft = vctx.prog->t_ptr;
    cf->type = ft;
    cf->idx = idx++;
    t->fields.add(cf);
  }
  return t;
}

CGv2Type *view_translate_type(Sym *s, ViewBuildCtx &vctx) {
  if (!s || !vctx.prog) return nullptr;
  if (CGv2Type *cached = vctx.sym_to_type.get(s)) return cached;

  CGv2Type *t = view_predef_numeric(s, vctx.prog);
  if (t) { vctx.sym_to_type.put(s, t); return t; }

  if (s == sym_void) {
    vctx.sym_to_type.put(s, vctx.prog->t_void);
    return vctx.prog->t_void;
  }
  if (s == sym_nil_type) {
    vctx.sym_to_type.put(s, vctx.prog->t_nil);
    return vctx.prog->t_nil;
  }
  if (s->is_symbol) {
    vctx.sym_to_type.put(s, vctx.prog->t_sym);
    return vctx.prog->t_sym;
  }

  // pyc string family: typed char-ptr.  The materialized
  // side caches a single "string" entry in prog->types via
  // get_string_type; reuse it for identity stability.
  if (s == sym_string ||
      (sym_string && sym_string->specializers.set_in(s))) {
    if (CGv2Type *existing = view_lookup_type_by_name(vctx.prog,
                                                        "string")) {
      vctx.sym_to_type.put(s, existing);
      return existing;
    }
    CGv2Type *str_t = new CGv2Type();
    str_t->id = 1000 + vctx.prog->types.n;
    str_t->name = "string";
    str_t->kind = CG2T_PTR;
    str_t->element = vctx.prog->t_int8;
    vctx.prog->types.add(str_t);
    vctx.sym_to_type.put(s, str_t);
    return str_t;
  }

  // Type_RECORD: pyc holds records by pointer.  Build the
  // struct, then wrap in CG2T_PTR.  Match-by-name against
  // prog->types preserves identity with the materialized
  // CGv2Type when both paths run.
  if (s->type_kind == Type_RECORD) {
    CGv2Type *struct_t = view_translate_struct(s, vctx);
    if (struct_t) {
      char buf[160];
      snprintf(buf, sizeof(buf), "ptr_%s",
               struct_t->name ? struct_t->name : "anon");
      if (CGv2Type *existing = view_lookup_type_by_name(vctx.prog, buf)) {
        vctx.sym_to_type.put(s, existing);
        return existing;
      }
      CGv2Type *ptr_t = new CGv2Type();
      ptr_t->id = 1000 + vctx.prog->types.n;
      ptr_t->name = dupstr(buf);
      ptr_t->kind = CG2T_PTR;
      ptr_t->element = struct_t;
      vctx.prog->types.add(ptr_t);
      vctx.sym_to_type.put(s, ptr_t);
      return ptr_t;
    }
  }

  // Closure: Type_FUN with no resolved fun + non-empty
  // has[] (mirrors is_closure_var).  Build the closure
  // struct, then wrap in CG2T_PTR.
  if (s->type_kind == Type_FUN && !s->fun && s->has.n) {
    CGv2Type *struct_t = view_translate_closure_struct(s, vctx);
    if (struct_t) {
      char buf[160];
      snprintf(buf, sizeof(buf), "ptr_%s", struct_t->name);
      if (CGv2Type *existing = view_lookup_type_by_name(vctx.prog, buf)) {
        vctx.sym_to_type.put(s, existing);
        return existing;
      }
      CGv2Type *ptr_t = new CGv2Type();
      ptr_t->id = 1000 + vctx.prog->types.n;
      ptr_t->name = dupstr(buf);
      ptr_t->kind = CG2T_PTR;
      ptr_t->element = struct_t;
      vctx.prog->types.add(ptr_t);
      vctx.sym_to_type.put(s, ptr_t);
      return ptr_t;
    }
  }

  // Type_FUN (resolved fun pointer), Type_REF,
  // Type_PRIMITIVE → opaque ptr.  Matches
  // cg_normalize_v2.cc:264.
  vctx.sym_to_type.put(s, vctx.prog->t_ptr);
  return vctx.prog->t_ptr;
}

CGv2Value *view_translate_value(CGValueView v, ViewBuildCtx &vctx) {
  if (v.is_null()) return nullptr;
  Var *var = v.var;
  if (CGv2Value *cached = vctx.var_to_value.get(var)) return cached;

  // Phase C.2: if the Var's Sym corresponds to a constant,
  // global, formal, or local the materialized translator
  // already placed in prog->{constants,globals} or
  // current_cf->{formals,locals}, reuse that object.  This
  // matters for view-driven emit: the LLVM emit pass caches
  // llvm::Value*'s keyed on CGv2Value*, so fresh objects
  // with the same Sym name would never resolve (formals
  // wouldn't find their bound llvm::Argument; locals
  // wouldn't find their alloca slot).  Identity-match by
  // name on the public vectors keeps the view-driven and
  // materialized worlds in sync without exposing the
  // private sym_to_value / var_to_value maps.
  //
  // Lookup order matches the materialized build_var:
  // current_cf's formals/locals (per-Fun) → prog constants
  // → prog globals → fresh local.
  if (var->sym) {
    cchar *sym_name = var->sym->name ? var->sym->name :
                       (var->cg_string ? var->cg_string : nullptr);
    cchar *vname = var->cg_string ? var->cg_string :
                   (var->sym->name ? var->sym->name : nullptr);
    auto match = [&](CGv2Value *cv) -> bool {
      if (!cv || !cv->name) return false;
      if (sym_name && strcmp(cv->name, sym_name) == 0) return true;
      if (vname && strcmp(cv->name, vname) == 0) return true;
      return false;
    };
    if (vctx.current_cf) {
      for (int i = 0; i < vctx.current_cf->formals.n; i++) {
        CGv2Value *cv = vctx.current_cf->formals.v[i];
        if (match(cv)) { vctx.var_to_value.put(var, cv); return cv; }
      }
      for (int i = 0; i < vctx.current_cf->locals.n; i++) {
        CGv2Value *cv = vctx.current_cf->locals.v[i];
        if (match(cv)) { vctx.var_to_value.put(var, cv); return cv; }
      }
    }
    if (vctx.prog) {
      for (int i = 0; i < vctx.prog->constants.n; i++) {
        CGv2Value *cv = vctx.prog->constants.v[i];
        if (match(cv)) { vctx.var_to_value.put(var, cv); return cv; }
      }
      for (int i = 0; i < vctx.prog->globals.n; i++) {
        CGv2Value *cv = vctx.prog->globals.v[i];
        if (match(cv)) { vctx.var_to_value.put(var, cv); return cv; }
      }
    }
  }


  CGv2Value *cv = new CGv2Value();
  // The id namespace here mirrors the materialized side's
  // LOCAL range (2000+) without colliding with global /
  // constant ids (which start at 1000).  Disambiguation
  // matters only if both paths' values are ever cross-
  // referenced in the same emit run — Phase B.6 diff is
  // text-level, so the id values themselves don't need
  // to match the materialized side bit-for-bit.
  cv->id = 3000 + vctx.var_to_value.n;
  cv->name = (var->sym && var->sym->name) ? var->sym->name :
             (var->cg_string ? var->cg_string : "v");
  cv->type = var->type ? view_translate_type(var->type, vctx)
                       : vctx.prog->t_ptr;

  if (var->sym && var->sym->is_constant) {
    cv->scope = CG2V_CONSTANT;
    view_build_immediate(var->sym->imm, cv->imm);
  } else if (var->sym && var->sym->is_fun && var->sym->fun) {
    cv->scope = CG2V_FUN_REF;
    cv->target_name = var->sym->fun->cg_string;
  } else if (var->sym && var->sym->is_symbol) {
    cv->scope = CG2V_SYMBOL;
  } else {
    cv->scope = CG2V_LOCAL;
  }
  // Phase B.3: conservative `escapes=true` (CGv2Value's
  // default).  Phase D-era will plumb IFA's per-AVar
  // escape lattice through the view once that becomes
  // the only path.
  vctx.var_to_value.put(var, cv);
  return cv;
}

// ---------------------------------------------------------------------------
// Block discovery — Phase B.4
// ---------------------------------------------------------------------------
//
// `view_build_fun_blocks` mirrors `build_block_skeleton` in
// cg_normalize_v2.cc: BFS from Fun::entry over cfg_succ,
// allocate one CGv2Block per LABEL PNode discovered, name it
// "entry" / "L<label-id>" / "B<id>".  Maps populated in the
// ctx are queryable by both `CGInstRef::from_view` (branch
// target lookup) and `CGFunView::blocks` (iteration order).
//
// Closer detection runs in the same walk: a PNode is a closer
// if any of its cfg_succ leaves the current block (lands on
// a LABEL) or it has no successors.  Recorded into
// `vctx.entry_to_closer` keyed by the block's entry PNode.

Vec<CGBlockView> view_build_fun_blocks(CGFunView fv,
                                        ViewBuildCtx &vctx,
                                        CGv2Fun *cf) {
  // Cache hit: re-running for the same Fun returns the
  // same Vec of CGBlockView's that the first call computed,
  // so emit_fun-side callers can ask repeatedly.
  if (!fv.fn || !fv.fn->entry) return Vec<CGBlockView>();
  if (Vec<CGBlockView> *cached = vctx.fun_blocks.get(fv.fn)) {
    return *cached;
  }

  Vec<CGBlockView> *out = new Vec<CGBlockView>;

  CGv2Block *entry_blk = new CGv2Block();
  entry_blk->id = 0;
  entry_blk->name = "entry";
  vctx.pn_to_block.put(fv.fn->entry, entry_blk);
  if (cf) {
    if (!cf->entry) cf->entry = entry_blk;
    cf->blocks.add(entry_blk);
  }
  out->add(CGBlockView(fv.fn->entry));

  int next_id = 1;
  Vec<PNode *> worklist;
  Vec<PNode *> seen;
  worklist.add(fv.fn->entry);
  seen.set_add(fv.fn->entry);
  while (worklist.n) {
    PNode *cur = worklist.pop();
    // Closer detection: every PNode whose cfg_succ leaves
    // the current block records itself as the closer for
    // its block's entry.  The entry-PNode of `cur`'s
    // block is the most recent LABEL ancestor; rather than
    // tracking that on the worklist, we lazily resolve
    // closers in a second pass below.  For now just
    // enqueue successors.
    for (PNode *succ : cur->cfg_succ) {
      if (!succ || !seen.set_add(succ)) continue;
      if (succ->code && succ->code->kind == Code_LABEL) {
        CGv2Block *b = new CGv2Block();
        b->id = next_id++;
        char buf[32];
        if (succ->code->label[0]) {
          snprintf(buf, sizeof(buf), "L%d", succ->code->label[0]->id);
        } else {
          snprintf(buf, sizeof(buf), "B%d", b->id);
        }
        b->name = dupstr(buf);
        vctx.pn_to_block.put(succ, b);
        if (succ->code->label[0]) {
          vctx.label_to_block.put(succ->code->label[0], b);
        }
        if (succ->code->label[1]) {
          vctx.label_to_block.put(succ->code->label[1], b);
        }
        if (cf) cf->blocks.add(b);
        out->add(CGBlockView(succ));
      }
      worklist.add(succ);
    }
  }

  // Second pass: closer detection.  For each block-entry
  // PNode, DFS forward through cfg_succ within the block
  // (i.e. stop when a successor lands on a different
  // CGv2Block) and record the last PNode visited as the
  // closer.  Linear in PNode count.
  for (int bi = 0; bi < out->n; bi++) {
    CGBlockView bv = (*out)[bi];
    if (!bv.entry) continue;
    CGv2Block *this_blk = vctx.pn_to_block.get(bv.entry);
    if (!this_blk) continue;
    PNode *closer = bv.entry;
    Vec<PNode *> stack;
    Vec<PNode *> visited2;
    stack.add(bv.entry);
    visited2.set_add(bv.entry);
    while (stack.n) {
      PNode *cur = stack.pop();
      bool is_closer = cur->cfg_succ.n == 0;
      for (PNode *s : cur->cfg_succ) {
        if (!s) continue;
        CGv2Block *sblk =
            (s->code && s->code->kind == Code_LABEL)
                ? vctx.pn_to_block.get(s)
                : this_blk;
        if (sblk != this_blk) { is_closer = true; }
        else if (visited2.set_add(s)) { stack.add(s); }
      }
      if (is_closer) closer = cur;
    }
    vctx.entry_to_closer.put(bv.entry, closer);
  }

  vctx.fun_blocks.put(fv.fn, out);
  return *out;
}

Vec<CGBlockView> CGFunView::blocks(ViewBuildCtx &vctx) const {
  return view_build_fun_blocks(*this, vctx);
}

PNode *CGBlockView::closer_pnode(ViewBuildCtx &vctx) const {
  if (!entry) return nullptr;
  return vctx.entry_to_closer.get(entry);
}

// Helper: synthesize a CGv2_MOVE CGInstRef from a (src, dst)
// pair of Vars.  view_pn carries the originating phi/phy
// PNode so consumers that want diagnostics can recover the
// source location.
static CGInstRef view_make_move(Var *src, Var *dst, PNode *origin,
                                  ViewBuildCtx &vctx) {
  CGInstRef r;
  r.op = CG2_MOVE;
  r.view_pn = origin;
  Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
  Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
  if (CGv2Value *s = view_translate_value(CGValueView(src), vctx)) rv->add(s);
  if (CGv2Value *d = view_translate_value(CGValueView(dst), vctx)) lv->add(d);
  r.rvals = rv;
  r.lvals = lv;
  return r;
}

Vec<CGInstRef> view_enumerate_phi_moves(PNode *pred_closer,
                                         PNode *succ_entry,
                                         int isucc,
                                         ViewBuildCtx &vctx) {
  Vec<CGInstRef> out;
  if (!pred_closer || !succ_entry) return out;

  // phy: lvals[isucc] ← rvals[0] for each phy on the closer.
  for (PNode *p : pred_closer->phy) {
    if (!p || p->lvals.n <= isucc || p->rvals.n < 1) continue;
    Var *src = p->rvals.v[0];
    Var *dst = p->lvals.v[isucc];
    if (!src || !dst) continue;
    out.add(view_make_move(src, dst, p, vctx));
  }

  // phi: each phi PNode on the successor has lvals[0] ←
  // rvals[pred_idx] where pred_idx is the closer's position
  // in succ's pred list.  cfg_pred_index is maintained by
  // FA; if a key is missing the default (0) is used by the
  // materialized side too — keeping parity here.
  int pred_idx = succ_entry->cfg_pred_index.get(pred_closer);
  for (PNode *p : succ_entry->phi) {
    if (!p || p->lvals.n < 1) continue;
    if (p->rvals.n <= pred_idx) continue;
    Var *src = p->rvals.v[pred_idx];
    Var *dst = p->lvals.v[0];
    if (!src || !dst) continue;
    out.add(view_make_move(src, dst, p, vctx));
  }
  return out;
}

CGInstRef view_make_terminator(CGBlockView bv, ViewBuildCtx &vctx) {
  CGInstRef r;
  PNode *closer = bv.closer_pnode(vctx);
  if (!closer || !closer->code) {
    r.op = CG2_UNREACHABLE;
    r.view_pn = closer;
    return r;
  }
  r.view_pn = closer;
  Code *cd = closer->code;

  // Collect unique successor blocks (mirrors build_terminator).
  CGv2Block *this_blk = vctx.pn_to_block.get(bv.entry);
  Vec<CGv2Block *> succ_blocks;
  for (PNode *s : closer->cfg_succ) {
    if (!s) continue;
    CGv2Block *sblk = (s->code && s->code->kind == Code_LABEL)
                          ? vctx.pn_to_block.get(s)
                          : nullptr;
    if (sblk && sblk != this_blk) succ_blocks.set_add(sblk);
  }

  switch (cd->kind) {
    case Code_GOTO:
      r.op = CG2_BR;
      r.br_target = cd->label[0]
                        ? vctx.label_to_block.get(cd->label[0])
                        : (succ_blocks.n > 0 ? succ_blocks.v[0] : nullptr);
      break;
    case Code_IF: {
      r.op = CG2_COND_BR;
      Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
      if (closer->rvals.n > 0) {
        if (CGv2Value *c = view_translate_value(
                CGValueView(closer->rvals.v[0]), vctx))
          rv->add(c);
      }
      r.rvals = rv;
      r.lvals = new Vec<CGv2Value *>;
      r.br_true = cd->label[0]
                      ? vctx.label_to_block.get(cd->label[0])
                      : (succ_blocks.n > 0 ? succ_blocks.v[0] : nullptr);
      r.br_false = cd->label[1]
                       ? vctx.label_to_block.get(cd->label[1])
                       : (succ_blocks.n > 1 ? succ_blocks.v[1] : nullptr);
      break;
    }
    case Code_SEND:
      if (closer->prim && closer->prim->index == P_prim_reply) {
        r.op = CG2_RET;
        Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
        if (closer->rvals.n >= 4) {
          if (CGv2Value *c = view_translate_value(
                  CGValueView(closer->rvals.v[3]), vctx))
            rv->add(c);
        }
        r.rvals = rv;
        r.lvals = new Vec<CGv2Value *>;
      } else if (succ_blocks.n == 0) {
        r.op = CG2_UNREACHABLE;
      } else {
        r.op = CG2_BR;
        r.br_target = succ_blocks.v[0];
      }
      break;
    default:
      if (succ_blocks.n == 0) {
        r.op = CG2_RET;
      } else {
        r.op = CG2_BR;
        r.br_target = succ_blocks.v[0];
      }
      break;
  }
  return r;
}

// Phase C.2 multi-inst port of lower_send_period's
// closure-construction case (cg_normalize_v2.cc:718-774).
// When `a.x` reads a function-typed field on an instance,
// the materialized side allocates a fresh closure struct
// and stores (selector, bound_self) into fields 0 and 1.
// This is what `lower_send_call`'s closure-unpack path
// consumes.
static bool view_try_lower_period_closure(PNode *pn,
                                             ViewBuildCtx &vctx,
                                             Vec<CGInstRef> &out) {
  if (!pn || !pn->code || pn->code->kind != Code_SEND) return false;
  if (!pn->prim || pn->prim->index != P_prim_period) return false;
  if (pn->lvals.n < 1 || pn->rvals.n < 4) return false;
  Var *lv = pn->lvals.v[0];
  if (!lv || !lv->type) return false;
  if (lv->type->type_kind != Type_FUN) return false;
  if (!pn->creates || pn->creates->n == 0) return false;
  if (lv->type->has.n < 2) return false;

  CGv2Type *closure_ptr = view_translate_type(lv->type, vctx);
  CGv2Type *closure_struct = (closure_ptr && closure_ptr->kind == CG2T_PTR)
                                 ? closure_ptr->element
                                 : nullptr;
  if (!closure_struct || closure_struct->kind != CG2T_STRUCT ||
      closure_struct->fields.n < 2) {
    return false;
  }

  CGv2Value *obj = view_translate_value(CGValueView(pn->rvals.v[1]), vctx);
  CGv2Value *dst = view_translate_value(CGValueView(lv), vctx);
  if (!obj || !dst) return false;

  // CG2_ALLOC for the closure struct.
  {
    CGInstRef alloc;
    alloc.op = CG2_ALLOC;
    alloc.type_arg = closure_struct;
    alloc.view_pn = pn;
    alloc.rvals = new Vec<CGv2Value *>;
    Vec<CGv2Value *> *lv2 = new Vec<CGv2Value *>;
    lv2->add(dst);
    alloc.lvals = lv2;
    out.add(alloc);
  }

  // CG2_FIELD_STORE field_idx=0, (dst, selector).
  // Selector is the symbol Var at rvals[3].
  if (pn->rvals.n > 3) {
    CGv2Value *sel = view_translate_value(CGValueView(pn->rvals.v[3]), vctx);
    if (sel) {
      CGInstRef st;
      st.op = CG2_FIELD_STORE;
      st.field_idx = 0;
      st.type_arg = closure_struct;
      st.view_pn = pn;
      Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
      rv->add(dst); rv->add(sel);
      st.rvals = rv;
      st.lvals = new Vec<CGv2Value *>;
      out.add(st);
    }
  }

  // CG2_FIELD_STORE field_idx=1, (dst, bound_self).
  {
    CGInstRef st;
    st.op = CG2_FIELD_STORE;
    st.field_idx = 1;
    st.type_arg = closure_struct;
    st.view_pn = pn;
    Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
    rv->add(dst); rv->add(obj);
    st.rvals = rv;
    st.lvals = new Vec<CGv2Value *>;
    out.add(st);
  }
  return true;
}

// Phase C.2 multi-inst port of lower_send_call's
// MPosition-aware arg routing (cg_normalize_v2.cc:1565).
// Resolves the call target via the caller's
// `caller->calls.get(pn)` (single-target only — multi-
// target falls through to the naive CALL).  Walks
// `target->positional_arg_positions` and for each formal
// computes the rval index via `Position2int(p->pos[0]) - 1`.
//
// Closure unpacking (when rvals[0] is a closure-typed
// receiver, lower_send_call inserts a FIELD_LOAD inst
// for each formal that maps into a closure-struct field)
// is omitted from this initial port — closure-using
// tests fall through to the naive classification until
// the closure handler lands.
static bool view_try_lower_call(PNode *pn, ViewBuildCtx &vctx,
                                  Vec<CGInstRef> &out) {
  if (!pn || !pn->code || pn->code->kind != Code_SEND) return false;
  if (!vctx.current_fun) return false;
  Vec<Fun *> *callees = vctx.current_fun->calls.get(pn);
  if (!callees || callees->n != 1) return false;
  Fun *target = callees->v[0];
  if (!target) return false;
  if (pn->lvals.n < 1) return false;

  // Find the target's CGv2Fun via prog->funs name match.
  if (!target->cg_string || !vctx.prog) return false;
  CGv2Fun *target_cf = nullptr;
  for (int i = 0; i < vctx.prog->funs.n; i++) {
    CGv2Fun *cf = vctx.prog->funs.v[i];
    if (cf && cf->name && strcmp(cf->name, target->cg_string) == 0) {
      target_cf = cf;
      break;
    }
  }
  if (!target_cf) return false;

  // Closure receiver handling: when rvals[0] is a closure
  // (Type_FUN with `has.n` ≥ 2), each formal that maps into
  // a closure-struct field gets a FIELD_LOAD inst emitted
  // BEFORE the CALL (lower_send_call lines 1618-1647).
  Var *v0 = pn->rvals.n > 0 ? pn->rvals.v[0] : nullptr;
  bool v0_is_closure = v0 && is_closure_var(v0);
  CGv2Value *closure = nullptr;
  CGv2Type *closure_struct = nullptr;
  if (v0_is_closure) {
    closure = view_translate_value(CGValueView(v0), vctx);
    if (closure && closure->type && closure->type->kind == CG2T_PTR &&
        closure->type->element &&
        closure->type->element->kind == CG2T_STRUCT) {
      closure_struct = closure->type->element;
    }
  }

  CGInstRef call;
  call.op = CG2_CALL;
  call.view_pn = pn;

  CGv2Value *fnref = new CGv2Value();
  fnref->name = target_cf->name;
  fnref->type = vctx.prog->t_fun_ptr;
  fnref->scope = CG2V_FUN_REF;
  fnref->target_name = target_cf->name;

  // Pre-CALL closure-unpack FIELD_LOAD insts go to
  // `unpack_refs`; the CALL's rvals go to `new_rv`.  We
  // append unpack_refs to `out` before the CALL so the
  // emit order matches lower_send_call's blk->body.add
  // sequence.
  Vec<CGInstRef> unpack_refs;
  Vec<CGv2Value *> *new_rv = new Vec<CGv2Value *>;
  new_rv->add(fnref);
  int before = new_rv->n;

  // MPosition arg routing with formal filters mirroring
  // cg_normalize_v2.cc:1612-1654.
  for (MPosition *p : target->positional_arg_positions) {
    Var *formal_arg = target->args.get(p);
    if (!formal_arg || !formal_arg->live) continue;
    if (formal_arg->type && formal_arg->type->is_fun) continue;
    if (p->pos.n > 1) continue;     // skip nested tuple fields
    if (p->pos.n == 0) continue;
    int i = (int)Position2int(p->pos.v[0]) - 1;

    if (v0_is_closure && v0->type) {
      if (i < v0->type->has.n) {
        // Unpack: FIELD_LOAD(closure, i) → fresh local → arg.
        if (closure && closure_struct && i >= 0 &&
            i < closure_struct->fields.n) {
          CGv2Value *arg = new CGv2Value();
          arg->name = "clo_arg";
          arg->type = closure_struct->fields.v[i]
                          ? closure_struct->fields.v[i]->type
                          : vctx.prog->t_ptr;
          arg->scope = CG2V_LOCAL;

          CGInstRef fl;
          fl.op = CG2_FIELD_LOAD;
          fl.field_idx = i;
          fl.type_arg = closure_struct;
          fl.view_pn = pn;
          Vec<CGv2Value *> *fl_rv = new Vec<CGv2Value *>;
          fl_rv->add(closure);
          fl.rvals = fl_rv;
          Vec<CGv2Value *> *fl_lv = new Vec<CGv2Value *>;
          fl_lv->add(arg);
          fl.lvals = fl_lv;
          unpack_refs.add(fl);

          new_rv->add(arg);
        }
        continue;
      } else {
        // Beyond the closure's field count — extra args come
        // from pn->rvals shifted by `has.n - 1` (rvals[0] is
        // the closure itself).
        i -= v0->type->has.n - 1;
      }
    }
    if (i < 0 || i >= pn->rvals.n) continue;
    Var *actual = pn->rvals.v[i];
    if (!actual) continue;
    CGv2Value *cv = view_translate_value(CGValueView(actual), vctx);
    if (cv) new_rv->add(cv);
  }
  // Fallback: pass-through rvals[1..] when MPosition routing
  // produced nothing.
  if (new_rv->n == before) {
    for (int i = 1; i < pn->rvals.n; i++) {
      CGv2Value *cv = view_translate_value(CGValueView(pn->rvals.v[i]), vctx);
      if (cv) new_rv->add(cv);
    }
  }
  call.rvals = new_rv;

  Vec<CGv2Value *> *new_lv = new Vec<CGv2Value *>;
  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (dst) new_lv->add(dst);
  call.lvals = new_lv;

  // Emit FIELD_LOAD unpack insts first, then the CALL.
  for (int i = 0; i < unpack_refs.n; i++) out.add(unpack_refs.v[i]);
  out.add(call);
  return true;
}

// Phase C.2 multi-inst: full port of lower_send_alloc
// (cg_normalize_v2.cc:890+).  Three sub-paths plus the
// per-element initializer loop and stage-2 conversion:
//   1. Flat list/tuple/vector (dst->type doesn't unwrap to
//      a struct): SIZEOF(elem) + C_CALL(_CG_prim_tuple_list_internal)
//      + per-element INDEX_STOREs.
//   2. Struct-shape list/tuple: SIZEOF + C_CALL alloc into
//      a tmp + per-field FIELD_STOREs into tmp + SIZEOF +
//      C_CALL(_CG_to_list_runtime) into dst.
//   3. Regular class instantiation: bare CG2_ALLOC + per-
//      field FIELD_STOREs for constructor args.
static bool view_try_lower_alloc(PNode *pn,
                                   ViewBuildCtx &vctx,
                                   Vec<CGInstRef> &out) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_make &&
      pn->prim->index != P_prim_new) return false;
  if (pn->lvals.n < 1 || !vctx.prog) return false;

  CGv2Value *dst = view_translate_value(CGValueView(pn->lvals.v[0]), vctx);
  if (!dst || !dst->type) return false;

  // unwrap_struct: if dst->type is CG2T_PTR, look at element.
  CGv2Type *struct_t = dst->type;
  if (struct_t->kind == CG2T_PTR && struct_t->element) {
    struct_t = struct_t->element;
  }

  bool is_list_or_tuple_or_vec = false;
  if (pn->prim->index == P_prim_make && pn->rvals.n >= 3 &&
      pn->rvals.v[2] && pn->rvals.v[2]->sym) {
    Sym *target = pn->rvals.v[2]->sym;
    is_list_or_tuple_or_vec =
        (sym_tuple && sym_tuple->specializers.set_in(target)) ||
        (sym_list && sym_list->specializers.set_in(target)) ||
        target->is_vector;
  }

  CGv2Value *struct_init_target = dst;
  bool routed_flat = false;
  bool routed_struct = false;

  // Sub-path 1: flat (no struct underneath).
  if (is_list_or_tuple_or_vec &&
      !(struct_t && struct_t->kind == CG2T_STRUCT)) {
    CGv2Type *elem_type = nullptr;
    if (pn->rvals.n > 3) {
      CGv2Value *fv =
          view_translate_value(CGValueView(pn->rvals.v[3]), vctx);
      if (fv && fv->type) elem_type = fv->type;
    }
    if (!elem_type) elem_type = vctx.prog->t_int64;

    CGv2Value *size_v = new CGv2Value();
    size_v->name = "elem_sz";
    size_v->type = vctx.prog->t_uint32;
    size_v->scope = CG2V_LOCAL;
    {
      CGInstRef sz;
      sz.op = CG2_SIZEOF;
      sz.type_arg = elem_type;
      sz.view_pn = pn;
      sz.rvals = new Vec<CGv2Value *>;
      Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
      lv->add(size_v);
      sz.lvals = lv;
      out.add(sz);
    }

    CGv2Value *n_v = new CGv2Value();
    n_v->name = "cnt";
    n_v->type = vctx.prog->t_uint32;
    n_v->scope = CG2V_CONSTANT;
    n_v->imm.kind = CGv2Immediate::I_INT;
    n_v->imm.v.i = pn->rvals.n - 3;
    vctx.prog->constants.add(n_v);

    {
      CGInstRef call;
      call.op = CG2_C_CALL;
      call.prim_name = "_CG_prim_tuple_list_internal";
      call.type_arg = dst->type;
      call.view_pn = pn;
      Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
      rv->add(size_v); rv->add(n_v);
      call.rvals = rv;
      Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
      lv->add(dst);
      call.lvals = lv;
      out.add(call);
    }
    routed_flat = true;
  }

  // Sub-path 2: struct-shape list/tuple.
  if (!routed_flat && is_list_or_tuple_or_vec &&
      struct_t && struct_t->kind == CG2T_STRUCT) {
    CGv2Value *size_v = new CGv2Value();
    size_v->name = "list_struct_sz";
    size_v->type = vctx.prog->t_uint32;
    size_v->scope = CG2V_LOCAL;
    {
      CGInstRef sz;
      sz.op = CG2_SIZEOF;
      sz.type_arg = struct_t;
      sz.view_pn = pn;
      sz.rvals = new Vec<CGv2Value *>;
      Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
      lv->add(size_v);
      sz.lvals = lv;
      out.add(sz);
    }

    CGv2Value *n_alloc = new CGv2Value();
    n_alloc->name = "alloc_cnt";
    n_alloc->type = vctx.prog->t_uint32;
    n_alloc->scope = CG2V_CONSTANT;
    n_alloc->imm.kind = CGv2Immediate::I_INT;
    n_alloc->imm.v.i = pn->rvals.n - 2;
    vctx.prog->constants.add(n_alloc);

    CGv2Value *tmp = new CGv2Value();
    tmp->name = "list_tmp";
    tmp->type = dst->type;
    tmp->scope = CG2V_LOCAL;
    {
      CGInstRef call;
      call.op = CG2_C_CALL;
      call.prim_name = "_CG_prim_tuple_list_internal";
      call.type_arg = dst->type;
      call.view_pn = pn;
      Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
      rv->add(size_v); rv->add(n_alloc);
      call.rvals = rv;
      Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
      lv->add(tmp);
      call.lvals = lv;
      out.add(call);
    }
    struct_init_target = tmp;
    routed_struct = true;
  }

  // Sub-path 3: bare ALLOC.
  if (!routed_flat && !routed_struct) {
    CGInstRef alloc;
    alloc.op = CG2_ALLOC;
    alloc.type_arg = struct_t;
    alloc.view_pn = pn;
    alloc.rvals = new Vec<CGv2Value *>;
    Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
    lv->add(dst);
    alloc.lvals = lv;
    out.add(alloc);
  }

  // Per-element initializers (P_prim_make only).
  if (pn->prim->index == P_prim_make) {
    if (struct_t && struct_t->kind == CG2T_STRUCT) {
      int n_fields = struct_t->fields.n;
      for (int i = 3; i < pn->rvals.n; i++) {
        int field_idx = i - 3;
        if (field_idx >= n_fields) break;
        Var *val_var = pn->rvals.v[i];
        if (!val_var) continue;
        CGv2Value *val = view_translate_value(CGValueView(val_var), vctx);
        if (!val) continue;
        CGInstRef st;
        st.op = CG2_FIELD_STORE;
        st.field_idx = field_idx;
        st.type_arg = struct_t;
        st.view_pn = pn;
        Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
        rv->add(struct_init_target); rv->add(val);
        st.rvals = rv;
        st.lvals = new Vec<CGv2Value *>;
        out.add(st);
      }
    } else if (routed_flat) {
      // Flat-array shape: INDEX_STORE with a constant idx.
      for (int i = 3; i < pn->rvals.n; i++) {
        Var *val_var = pn->rvals.v[i];
        if (!val_var) continue;
        CGv2Value *val = view_translate_value(CGValueView(val_var), vctx);
        if (!val) continue;
        CGv2Value *idx = new CGv2Value();
        idx->name = "litidx";
        idx->type = vctx.prog->t_int64;
        idx->scope = CG2V_CONSTANT;
        idx->imm.kind = CGv2Immediate::I_INT;
        idx->imm.v.i = i - 3;
        CGInstRef st;
        st.op = CG2_INDEX_STORE;
        st.type_arg = val->type;
        st.view_pn = pn;
        Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
        rv->add(dst); rv->add(idx); rv->add(val);
        st.rvals = rv;
        st.lvals = new Vec<CGv2Value *>;
        out.add(st);
      }
    }
  }

  // Stage 2 — struct-shape final conversion to runtime list.
  if (routed_struct) {
    CGv2Value *size2 = new CGv2Value();
    size2->name = "list_struct_sz2";
    size2->type = vctx.prog->t_uint32;
    size2->scope = CG2V_LOCAL;
    {
      CGInstRef sz;
      sz.op = CG2_SIZEOF;
      sz.type_arg = struct_t;
      sz.view_pn = pn;
      sz.rvals = new Vec<CGv2Value *>;
      Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
      lv->add(size2);
      sz.lvals = lv;
      out.add(sz);
    }

    CGv2Value *n_semantic = new CGv2Value();
    n_semantic->name = "sem_cnt";
    n_semantic->type = vctx.prog->t_uint32;
    n_semantic->scope = CG2V_CONSTANT;
    n_semantic->imm.kind = CGv2Immediate::I_INT;
    n_semantic->imm.v.i = pn->rvals.n - 3;
    vctx.prog->constants.add(n_semantic);

    CGInstRef conv;
    conv.op = CG2_C_CALL;
    conv.prim_name = "_CG_to_list_runtime";
    conv.type_arg = dst->type;
    conv.view_pn = pn;
    Vec<CGv2Value *> *rv = new Vec<CGv2Value *>;
    rv->add(struct_init_target); rv->add(size2); rv->add(n_semantic);
    conv.rvals = rv;
    Vec<CGv2Value *> *lv = new Vec<CGv2Value *>;
    lv->add(dst);
    conv.lvals = lv;
    out.add(conv);
  }
  return true;
}

void view_lower_pnode(PNode *pn, ViewBuildCtx &vctx,
                       Vec<CGInstRef> &out) {
  if (!pn) return;
  CGInstView iv(pn);
  CGv2Op k = iv.kind();
  if (k == CG2_NOP) return;

  // Multi-inst handlers — each tries to handle the PNode
  // and append 1..N CGInstRefs.  Returns true to claim
  // the PNode; false to fall through to the single-inst
  // path.
  if (view_try_lower_alloc(pn, vctx, out)) return;
  // Closure construction: prim_period reading a function-
  // typed field on an instance lowers to ALLOC + 2
  // FIELD_STOREs.  Must run BEFORE the single-inst
  // FIELD_LOAD path in from_view.
  if (view_try_lower_period_closure(pn, vctx, out)) return;
  // CG_CALL routing: when the view classifies as CG_CALL
  // (no prim or prim falls through), use MPosition-aware
  // arg routing via the FA's caller->calls map.
  if (k == CG2_CALL && view_try_lower_call(pn, vctx, out)) return;

  // Default: single-inst via from_view.
  out.add(CGInstRef::from_view(iv, &vctx));
}

Vec<CGInstRef> view_enumerate_fun_insts(CGFunView fv,
                                          ViewBuildCtx &vctx) {
  Vec<CGInstRef> out;
  if (fv.is_null()) return out;
  // C.2: scope the caller-Fun for lower_send_call's
  // `caller->calls` lookup.  Save/restore so nested
  // calls (none today, but defensive) compose cleanly.
  Fun *saved_fun = vctx.current_fun;
  vctx.current_fun = fv.fn;
  Vec<CGBlockView> blocks = view_build_fun_blocks(fv, vctx);

  for (int bi = 0; bi < blocks.n; bi++) {
    CGBlockView bv = blocks.v[bi];

    // Body: every non-NOP PNode in the block (excluding the
    // closer, which the terminator step covers).  C.2:
    // multi-inst handler may append 0, 1, or N refs per
    // PNode — `view_lower_pnode` handles the dispatch.
    Vec<PNode *> body = bv.body_pnodes(vctx);
    for (int pi = 0; pi < body.n; pi++) {
      PNode *pn = body.v[pi];
      if (!pn) continue;
      view_lower_pnode(pn, vctx, out);
    }

    // Phi-edge MOVEs: for each successor that lands in a
    // different block, emit the phi/phy MOVEs for this edge.
    PNode *closer = bv.closer_pnode(vctx);
    CGv2Block *this_blk = vctx.pn_to_block.get(bv.entry);
    if (closer && this_blk) {
      for (int isucc = 0; isucc < closer->cfg_succ.n; isucc++) {
        PNode *succ_pn = closer->cfg_succ.v[isucc];
        if (!succ_pn) continue;
        if (!succ_pn->code || succ_pn->code->kind != Code_LABEL) continue;
        CGv2Block *succ_blk = vctx.pn_to_block.get(succ_pn);
        if (!succ_blk || succ_blk == this_blk) continue;
        Vec<CGInstRef> moves =
            view_enumerate_phi_moves(closer, succ_pn, isucc, vctx);
        for (int mi = 0; mi < moves.n; mi++) out.add(moves.v[mi]);
      }
    }

    // Terminator.
    out.add(view_make_terminator(bv, vctx));
  }
  vctx.current_fun = saved_fun;
  return out;
}

// ---------------------------------------------------------------------------
// Phase B.6 — instruction-level diff oracle.
// ---------------------------------------------------------------------------

namespace {

// Concatenate the materialized CGv2Fun's emit-order
// instructions (body → phi-edge MOVEs → terminator) into a
// flat Vec of (CGv2Op, n_rvals, n_lvals) triples, matching
// the shape view_enumerate_fun_insts returns.
struct InstSig {
  CGv2Op op;
  CGv2BinSub sub_op;
  int n_rvals;
  int n_lvals;
  cchar *first_rval_name;   // for grep-ability; may be null
  cchar *first_lval_name;
};

InstSig sig_from_v2(CGv2Inst *inst) {
  InstSig s;
  s.op = inst ? inst->op : CG2_NOP;
  s.sub_op = inst ? inst->sub_op : CG2B_NONE;
  s.n_rvals = inst ? inst->rvals.n : 0;
  s.n_lvals = inst ? inst->lvals.n : 0;
  s.first_rval_name = (inst && inst->rvals.n > 0 && inst->rvals.v[0])
                          ? inst->rvals.v[0]->name : nullptr;
  s.first_lval_name = (inst && inst->lvals.n > 0 && inst->lvals.v[0])
                          ? inst->lvals.v[0]->name : nullptr;
  return s;
}

InstSig sig_from_ref(const CGInstRef &r) {
  InstSig s;
  s.op = r.op;
  s.sub_op = r.sub_op;
  s.n_rvals = r.rvals ? r.rvals->n : 0;
  s.n_lvals = r.lvals ? r.lvals->n : 0;
  s.first_rval_name = (r.rvals && r.rvals->n > 0 && r.rvals->v[0])
                          ? r.rvals->v[0]->name : nullptr;
  s.first_lval_name = (r.lvals && r.lvals->n > 0 && r.lvals->v[0])
                          ? r.lvals->v[0]->name : nullptr;
  return s;
}

bool sig_eq(const InstSig &a, const InstSig &b) {
  if (a.op != b.op) return false;
  if (a.sub_op != b.sub_op) return false;
  if (a.n_rvals != b.n_rvals) return false;
  if (a.n_lvals != b.n_lvals) return false;
  return true;
}

void print_sig(FILE *out, const char *prefix, const InstSig &s) {
  fprintf(out, "%s%-14s sub=%d r=%d l=%d", prefix, op_name(s.op),
          (int)s.sub_op, s.n_rvals, s.n_lvals);
  if (s.first_rval_name) fprintf(out, " r0=%s", s.first_rval_name);
  if (s.first_lval_name) fprintf(out, " l0=%s", s.first_lval_name);
  fprintf(out, "\n");
}

// Flatten a CGv2Fun into the emit-order InstSig stream that
// view_enumerate_fun_insts produces: per block, body → phi-
// edge MOVEs (for each successor that's a different block)
// → terminator.
void flatten_v2_fun(CGv2Fun *cf, Vec<InstSig> &out) {
  if (!cf) return;
  for (int bi = 0; bi < cf->blocks.n; bi++) {
    CGv2Block *b = cf->blocks.v[bi];
    if (!b) continue;
    for (int ii = 0; ii < b->body.n; ii++) {
      out.add(sig_from_v2(b->body.v[ii]));
    }
    // Phi-edge MOVEs: for each successor S in the fun, if S
    // declares a phi_by_pred group with pred == b, emit those
    // MOVEs here (matches emit_fun's ordering).
    for (int si = 0; si < cf->blocks.n; si++) {
      CGv2Block *s = cf->blocks.v[si];
      if (!s) continue;
      for (CGv2PhiGroup *g : s->phi_by_pred) {
        if (g->pred != b) continue;
        for (CGv2Inst *mv : g->moves) out.add(sig_from_v2(mv));
      }
    }
    if (b->terminator) out.add(sig_from_v2(b->terminator));
  }
}

}  // namespace

int cg_view_diff_module(FA *fa, CGv2Program *prog, cchar *tag) {
  if (!fa || !prog) return 0;
  int divergences = 0;
  // Build a CGv2Fun lookup keyed by IF1 Fun (the
  // materialized translator stashes the link as the cf's
  // name == f->cg_string; safer to walk both in the same
  // order).  Pyc's materialized side adds CGv2Funs in
  // fa->funs order, so positional pairing works here too,
  // but the explicit name-match keeps it robust.
  ViewBuildCtx vctx(prog);
  for (Fun *f : fa->funs) {
    if (!f || !f->live || !f->entry) continue;
    cchar *fname = f->cg_string ? f->cg_string :
                   (f->sym && f->sym->name ? f->sym->name : "(anon)");
    CGv2Fun *cf = nullptr;
    for (int i = 0; i < prog->funs.n; i++) {
      if (prog->funs.v[i] && prog->funs.v[i]->name &&
          strcmp(prog->funs.v[i]->name, fname) == 0) {
        cf = prog->funs.v[i];
        break;
      }
    }
    if (!cf) continue;

    Vec<InstSig> v2_sigs;
    flatten_v2_fun(cf, v2_sigs);
    Vec<CGInstRef> view_refs = view_enumerate_fun_insts(CGFunView(f), vctx);

    int common = v2_sigs.n < view_refs.n ? v2_sigs.n : view_refs.n;
    int local_div = 0;
    for (int i = 0; i < common; i++) {
      InstSig vs = sig_from_ref(view_refs.v[i]);
      if (!sig_eq(v2_sigs.v[i], vs)) {
        if (local_div == 0) {
          fprintf(stderr, "[llvm-diff] %s fn=%s first divergence @ inst %d:\n",
                  tag ? tag : "?", fname, i);
          print_sig(stderr, "[llvm-diff]   v2:   ", v2_sigs.v[i]);
          print_sig(stderr, "[llvm-diff]   view: ", vs);
        }
        local_div++;
        divergences++;
      }
    }
    if (v2_sigs.n != view_refs.n) {
      fprintf(stderr, "[llvm-diff] %s fn=%s inst-count differs: v2=%d view=%d\n",
              tag ? tag : "?", fname, v2_sigs.n, view_refs.n);
      divergences += (v2_sigs.n > view_refs.n ? v2_sigs.n - view_refs.n
                                              : view_refs.n - v2_sigs.n);
    }
  }
  fprintf(stderr, "[llvm-diff] %s total divergences: %d\n",
          tag ? tag : "?", divergences);
  return divergences;
}

// ---------------------------------------------------------------------------
// Phase C.1 — view-driven emit entry point.
// ---------------------------------------------------------------------------

CGv2Inst *cg_view_ref_to_v2inst(const CGInstRef &r) {
  CGv2Inst *inst = new CGv2Inst();
  inst->op = r.op;
  inst->sub_op = r.sub_op;
  inst->br_target = r.br_target;
  inst->br_true = r.br_true;
  inst->br_false = r.br_false;
  inst->type_arg = r.type_arg;
  inst->field_idx = r.field_idx;
  inst->prim_name = r.prim_name;
  if (r.rvals) {
    for (int i = 0; i < r.rvals->n; i++) inst->rvals.add(r.rvals->v[i]);
  }
  if (r.lvals) {
    for (int i = 0; i < r.lvals->n; i++) inst->lvals.add(r.lvals->v[i]);
  }
  return inst;
}

// Helper: look up the CGv2Fun for an IF1 Fun via cg_string
// name match.  Phase C.1: linear scan over prog->funs.  The
// materialized translator's `fun_to_fun` map would be faster
// but isn't exposed publicly; this is invoked once per Fun
// per compile, which is fine in absolute terms.
static CGv2Fun *find_cgv2fun_for(Fun *f, CGv2Program *prog) {
  if (!f || !prog) return nullptr;
  cchar *fname = f->cg_string ? f->cg_string :
                 (f->sym && f->sym->name ? f->sym->name : nullptr);
  if (!fname) return nullptr;
  for (int i = 0; i < prog->funs.n; i++) {
    CGv2Fun *cf = prog->funs.v[i];
    if (cf && cf->name && strcmp(cf->name, fname) == 0) return cf;
  }
  return nullptr;
}

// Helper: get-or-make a phi_by_pred group on `succ_blk`
// keyed by `pred_blk`.  Mirrors get_or_make_phi_group in
// cg_normalize_v2.cc.
static CGv2PhiGroup *vd_get_or_make_phi_group(CGv2Block *succ_blk,
                                                CGv2Block *pred_blk) {
  for (CGv2PhiGroup *g : succ_blk->phi_by_pred) {
    if (g->pred == pred_blk) return g;
  }
  CGv2PhiGroup *g = new CGv2PhiGroup();
  g->pred = pred_blk;
  succ_blk->phi_by_pred.add(g);
  return g;
}

// Forward declaration — the materialized emit lives in
// cg_ir_v2_emit_llvm.cc.
extern bool cg_v2_emit_llvm_module(CGv2Program *prog);

bool cg_v2_emit_llvm_module_view(FA *fa, CGv2Program *prog) {
  if (!fa || !prog) return false;

  ViewBuildCtx vctx(prog);
  for (Fun *f : fa->funs) {
    if (!f || !f->live || !f->entry) continue;
    CGv2Fun *cf = find_cgv2fun_for(f, prog);
    if (!cf) continue;

    // Replace cf's bodies with view-derived data.  cf
    // retains its signature, name, formals, sret config —
    // all set by cg_normalize_v2's build_fun_decl pass.
    cf->blocks.clear();
    cf->entry = nullptr;

    // Phase C.2: scope view_translate_value's lookup to
    // this Fun's formals/locals so view-derived inst
    // operands share identity with the materialized
    // formal/local CGv2Values the emit's value_map is
    // keyed on.  current_fun threads the IF1 Fun through
    // to lower_send_call's `caller->calls` lookup.
    vctx.current_cf = cf;
    vctx.current_fun = f;

    // Allocate fresh CGv2Blocks via the view (also
    // populates vctx.pn_to_block / label_to_block which
    // CGInstRef::from_view consumes for branch targets).
    Vec<CGBlockView> blocks = view_build_fun_blocks(CGFunView(f), vctx, cf);

    // Per-block: body PNodes → CGv2Inst, phi-edge MOVEs →
    // succ block's phi_by_pred, terminator → block->terminator.
    for (int bi = 0; bi < blocks.n; bi++) {
      CGBlockView bv = blocks.v[bi];
      CGv2Block *blk = vctx.pn_to_block.get(bv.entry);
      if (!blk) continue;

      // Body insts.  C.2: route through view_lower_pnode
      // so multi-inst PNodes (e.g. list-literal alloc)
      // emit their full sequence into blk->body.
      Vec<PNode *> body = bv.body_pnodes(vctx);
      for (int pi = 0; pi < body.n; pi++) {
        PNode *pn = body.v[pi];
        if (!pn) continue;
        Vec<CGInstRef> refs;
        view_lower_pnode(pn, vctx, refs);
        for (int ri = 0; ri < refs.n; ri++) {
          blk->body.add(cg_view_ref_to_v2inst(refs.v[ri]));
        }
      }

      // Phi-edge MOVEs on cross-block successor edges.
      PNode *closer = bv.closer_pnode(vctx);
      if (closer) {
        for (int isucc = 0; isucc < closer->cfg_succ.n; isucc++) {
          PNode *succ_pn = closer->cfg_succ.v[isucc];
          if (!succ_pn) continue;
          if (!succ_pn->code || succ_pn->code->kind != Code_LABEL) continue;
          CGv2Block *succ_blk = vctx.pn_to_block.get(succ_pn);
          if (!succ_blk || succ_blk == blk) continue;
          Vec<CGInstRef> moves =
              view_enumerate_phi_moves(closer, succ_pn, isucc, vctx);
          if (moves.n == 0) continue;
          CGv2PhiGroup *g = vd_get_or_make_phi_group(succ_blk, blk);
          for (int mi = 0; mi < moves.n; mi++) {
            g->moves.add(cg_view_ref_to_v2inst(moves.v[mi]));
          }
        }
      }

      // Terminator.
      CGInstRef term_ref = view_make_terminator(bv, vctx);
      blk->terminator = cg_view_ref_to_v2inst(term_ref);
    }
  }

  // Now hand off to the existing LLVM emit, which walks
  // prog's blocks / body / phi_by_pred / terminator —
  // all view-derived after the loop above.
  return cg_v2_emit_llvm_module(prog);
}

Vec<PNode *> CGBlockView::body_pnodes(ViewBuildCtx &vctx) const {
  // Walk PNodes from `entry` along cfg_succ as long as they
  // stay in the same block.  The closer (terminator-equivalent)
  // is excluded from the body — emit's terminator builder
  // consumes it separately.  Entry LABEL is also excluded
  // (it's a boundary marker, not an instruction).
  Vec<PNode *> out;
  if (!entry) return out;
  CGv2Block *this_blk = vctx.pn_to_block.get(entry);
  if (!this_blk) return out;
  PNode *closer = vctx.entry_to_closer.get(entry);

  Vec<PNode *> stack;
  Vec<PNode *> visited;
  stack.add(entry);
  visited.set_add(entry);
  while (stack.n) {
    PNode *cur = stack.pop();
    bool is_entry_label = (cur == entry && cur->code &&
                            cur->code->kind == Code_LABEL);
    if (!is_entry_label && cur != closer) out.add(cur);
    for (PNode *s : cur->cfg_succ) {
      if (!s) continue;
      CGv2Block *sblk =
          (s->code && s->code->kind == Code_LABEL)
              ? vctx.pn_to_block.get(s)
              : this_blk;
      if (sblk == this_blk && visited.set_add(s)) {
        stack.add(s);
      }
    }
  }
  return out;
}
