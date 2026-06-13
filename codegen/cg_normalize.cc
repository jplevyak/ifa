// SPDX-License-Identifier: BSD-3-Clause
//
// cg_normalize.cc — the IF1 → CG_IR normalization pass.
//
// Phase 2 of CG_IR_PLAN. The pass walks the post-FA / post-clone /
// post-DCE IF1 state and produces a `CGProgram` (CGTypes + CGSlots +
// CGFuns + CGBlocks + CGInsts) consumed by the C / LLVM emitters
// after Phases 3-4. Through Phase 2 the result is built and
// discarded; the cg-normalize test phase locks the shape via
// golden files.
//
// Subphases (per CG_IR_PLAN §7):
//   2.1 — types + globals      (build_cgtypes / build_cgslots)
//   2.2 — per-Fun skeleton     (build_cgfun_skeleton)
//   2.3 — per-PNode lowering   (lower_pnode dispatch)
//   2.4 — phi/phy material.    (materialize_phi_phy — unconditional)
//   2.5 — construction-flow    (skipped per Phase 0 §5.3 outcome (a))
//
// See [CG_IR_PLAN.md](CG_IR_PLAN.md) for the migration plan and
// [../CODE_GEN_IR.md](../CODE_GEN_IR.md) for the design rationale.

#include "ifadefs.h"

#include "cg_ir.h"
#include "code.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "num.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"
#include "var.h"

// ---------------------------------------------------------------------------
// 2.1 — Type table and slot building
// ---------------------------------------------------------------------------

static CGType *build_cgtype(Sym *s, CGProgram *p);

static void num_kind_to_cgkind(Sym *s, CGTypeKind &kind, int &bits) {
  bits = 0;
  switch (s->num_kind) {
    case IF1_NUM_KIND_UINT:
      kind = (s->num_index == IF1_INT_TYPE_1) ? CG_T_BOOL : CG_T_UINT;
      switch (s->num_index) {
        case IF1_INT_TYPE_1:  bits = 1;  break;
        case IF1_INT_TYPE_8:  bits = 8;  break;
        case IF1_INT_TYPE_16: bits = 16; break;
        case IF1_INT_TYPE_32: bits = 32; break;
        case IF1_INT_TYPE_64: bits = 64; break;
      }
      break;
    case IF1_NUM_KIND_INT:
      kind = (s->num_index == IF1_INT_TYPE_1) ? CG_T_BOOL : CG_T_INT;
      switch (s->num_index) {
        case IF1_INT_TYPE_1:  bits = 1;  break;
        case IF1_INT_TYPE_8:  bits = 8;  break;
        case IF1_INT_TYPE_16: bits = 16; break;
        case IF1_INT_TYPE_32: bits = 32; break;
        case IF1_INT_TYPE_64: bits = 64; break;
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      kind = CG_T_FLOAT;
      switch (s->num_index) {
        case IF1_FLOAT_TYPE_32:  bits = 32;  break;
        case IF1_FLOAT_TYPE_64:  bits = 64;  break;
        case IF1_FLOAT_TYPE_128: bits = 128; break;
      }
      break;
    default:
      kind = CG_T_VOID;
      break;
  }
}

static CGType *build_cgtype(Sym *s, CGProgram *p) {
  if (!s) return 0;
  CGType *existing = p->sym_to_type.get(s);
  if (existing) return existing;

  CGType *t = new CGType();
  t->source = s;
  t->name = s->cg_string;
  p->sym_to_type.put(s, t);
  p->types.add(t);

  if (s->num_kind) {
    num_kind_to_cgkind(s, t->kind, t->bits);
    return t;
  }
  if (s->is_symbol) {
    t->kind = CG_T_PTR;
    return t;
  }
  switch (s->type_kind) {
    case Type_FUN:
      t->kind = s->fun ? CG_T_FUN_PTR : CG_T_PTR;
      break;
    case Type_RECORD:
      t->kind = s->has.n ? CG_T_PTR : CG_T_VOID;
      for (Sym *f : s->has) {
        t->fields.add(build_cgtype(f->type, p));
        t->field_names.add(f->name);
      }
      if (s->element) t->element = build_cgtype(s->element->type, p);
      break;
    case Type_PRIMITIVE:
    case Type_REF:
    default:
      t->kind = CG_T_PTR;
      break;
  }
  return t;
}

static void build_cgtypes(FA *fa, CGProgram *p) {
  Vec<Sym *> typesyms;
  Vec<Var *> globals;
  collect_types_and_globals(fa, typesyms, globals);
  for (Sym *s : typesyms) build_cgtype(s, p);
}

static void build_cgslots(FA *fa, CGProgram *p) {
  Vec<Sym *> typesyms;
  Vec<Var *> globals;
  collect_types_and_globals(fa, typesyms, globals);
  for (Var *v : globals) {
    if (!v->sym) continue;
    if (p->sym_to_slot.get(v->sym)) continue;
    CGSlot *slot = new CGSlot();
    slot->source_sym = v->sym;
    slot->source_var = v;
    slot->type = v->type ? p->sym_to_type.get(v->type) : 0;
    slot->name = v->sym->name;
    slot->cg_name = v->cg_string;
    slot->id = v->sym->id;
    if (v->sym->is_constant) {
      slot->kind = CG_SLOT_CONSTANT;
      slot->imm = &v->sym->imm;
    } else {
      slot->kind = CG_SLOT_GLOBAL;
    }
    p->sym_to_slot.put(v->sym, slot);
    p->globals.add(slot);
  }
}

// ---------------------------------------------------------------------------
// Lowering context — per-CGFun state passed through 2.3 / 2.4.
// ---------------------------------------------------------------------------

struct LowerCtx {
  CGFun *cf;
  CGProgram *p;
  Map<Var *, CGSlot *> local_map;        // Var -> slot in this fun
  Map<PNode *, CGBlock *> pn_to_block;   // LABEL PNode -> block
  Map<PNode *, CGBlock *> pn_owning;     // any PNode -> block it lives in
};

static CGSlot *get_or_make_local_slot(Var *v, LowerCtx &lc) {
  if (!v) return 0;
  CGSlot *existing = lc.local_map.get(v);
  if (existing) return existing;
  if (v->sym) {
    CGSlot *g = lc.p->sym_to_slot.get(v->sym);
    if (g) { lc.local_map.put(v, g); return g; }
  }
  CGSlot *slot = new CGSlot();
  slot->kind = CG_SLOT_LOCAL;
  slot->source_var = v;
  slot->source_sym = v->sym;
  slot->type = v->type ? lc.p->sym_to_type.get(v->type) : 0;
  slot->name = v->sym ? v->sym->name : 0;
  slot->cg_name = v->cg_string;
  slot->id = v->sym ? v->sym->id : 0;
  lc.local_map.put(v, slot);
  lc.cf->locals.add(slot);
  return slot;
}

static CGValue *make_slot_value(CGSlot *s, CGProgram *) {
  CGValue *cv = new CGValue();
  cv->kind = CG_V_SLOT;
  cv->slot = s;
  cv->type = s ? s->type : 0;
  return cv;
}

static CGValue *rval_to_value(Var *v, LowerCtx &lc) {
  if (!v) return 0;
  if (v->sym && v->sym->is_constant) {
    CGSlot *cslot = lc.p->sym_to_slot.get(v->sym);
    if (!cslot) {
      cslot = new CGSlot();
      cslot->kind = CG_SLOT_CONSTANT;
      cslot->source_sym = v->sym;
      cslot->source_var = v;
      cslot->type = v->type ? lc.p->sym_to_type.get(v->type) : 0;
      cslot->name = v->sym->name;
      cslot->cg_name = v->cg_string;
      cslot->id = v->sym->id;
      cslot->imm = &v->sym->imm;
      lc.p->sym_to_slot.put(v->sym, cslot);
      lc.p->globals.add(cslot);
    }
    return make_slot_value(cslot, lc.p);
  }
  return make_slot_value(get_or_make_local_slot(v, lc), lc.p);
}

// ---------------------------------------------------------------------------
// 2.2 — Per-Fun skeleton + 2.3 — Per-PNode lowering
// ---------------------------------------------------------------------------

static CGInst *new_inst(CGOp op, PNode *src) {
  CGInst *i = new CGInst();
  i->op = op;
  i->source_pn = src;
  if (src && src->code) {
    i->src_line = src->code->source_line();
    i->src_file = src->code->pathname();
  }
  return i;
}

// Emit CG_STORE for one Code_MOVE PNode. Unconditional — see Phase 0
// §5.2 LIVENESS.md (`pn_should_emit` contract carve-out).
static void lower_move(PNode *pn, CGBlock *blk, LowerCtx &lc) {
  if (!pn || !pn->code || pn->code->lvals.n == 0 || pn->code->rvals.n == 0) return;
  Var *rv = pn->rvals.n ? pn->rvals[0] : 0;
  Var *lv = pn->lvals.n ? pn->lvals[0] : 0;
  if (!rv || !lv) return;
  CGInst *st = new_inst(CG_STORE, pn);
  st->slot = get_or_make_local_slot(lv, lc);
  st->rvals.add(rval_to_value(rv, lc));
  blk->body.add(st);
}

// Resolve a P_prim_period / P_prim_setter field name to its index in
// the object's `has` list. Mirrors cg.cc's symbol_info walk. Returns
// -1 if not resolvable (the call falls back to generic CG_CALL).
static int resolve_field_index(PNode *pn, int field_rval_idx, int obj_rval_idx) {
  if (field_rval_idx >= pn->rvals.n || obj_rval_idx >= pn->rvals.n) return -1;
  Var *field_var = pn->rvals[field_rval_idx];
  Var *obj_var = pn->rvals[obj_rval_idx];
  if (!field_var || !obj_var || !obj_var->type) return -1;
  Sym *obj = obj_var->type;
  if (obj->type_kind == Type_SUM && obj->has.n) obj = obj->has[0]->type;
  cchar *symbol = nullptr;
  if (field_var->sym && field_var->sym->is_symbol) symbol = field_var->sym->name;
  if (!symbol) {
    Vec<Sym *> symbols;
    symbol_info(field_var, symbols);
    if (symbols.n == 1) symbol = symbols[0]->name;
  }
  if (!symbol) return -1;
  for (int i = 0; i < obj->has.n; i++) {
    if (symbol == obj->has[i]->name) return i;
  }
  return -1;
}

// Emit a generic CG_CALL with prim hint preserved. The LLVM/C
// emitter still dispatches on `inst->prim->index` for primitives we
// don't structurally decompose at normalization time (arithmetic,
// comparison, registered cgfns, etc.).
static void emit_generic_call(PNode *pn, CGBlock *blk, LowerCtx &lc) {
  CGInst *call = new_inst(CG_CALL, pn);
  call->prim = pn->prim;
  for (Var *r : pn->rvals) call->rvals.add(rval_to_value(r, lc));
  if (pn->lvals.n) call->slot = get_or_make_local_slot(pn->lvals[0], lc);
  if (call->slot && call->slot->type) call->result_type = call->slot->type;
  blk->body.add(call);
}

// Emit per-CG_OP dispatch for Code_SEND. For structural primitives
// (period/setter/new/clone/reply/index_object) we emit a specific
// CG_OP so the LLVM emitter dispatches on shape rather than on
// `prim->index`. For everything else (arithmetic/comparison/
// registered cgfns) we fall back to CG_CALL with prim preserved.
//
// rvals convention (from cg.cc mirror):
//   P_prim_period:     rvals[1]=object, rvals[3]=field-name symbol
//   P_prim_setter:     rvals[1]=object, rvals[3]=field-name, rvals[4]=value
//   P_prim_new:        result is heap allocation typed by lvals[0]
//   P_prim_clone:      rvals[1]=prototype object
//   P_prim_reply:      rvals[3]=return value (terminator)
//   P_prim_index_object: rvals[1]=object, rvals[2]=index
static void lower_send(PNode *pn, CGBlock *blk, LowerCtx &lc) {
  if (!pn || !pn->code) return;
  if (!pn->prim) {
    emit_generic_call(pn, blk, lc);
    return;
  }
  int idx = pn->prim->index;

  // P_prim_reply: emit nothing here — the terminator emit in
  // build_cgfun's pass-3 picks this up as the block's CG_RET.
  if (idx == P_prim_reply) return;

  // P_prim_period: structural field load. rvals[3] is the field
  // selector, rvals[1] is the object. Resolve to a field index.
  if (idx == P_prim_period) {
    int fi = resolve_field_index(pn, /*field_rval_idx=*/3, /*obj_rval_idx=*/1);
    if (fi < 0) { emit_generic_call(pn, blk, lc); return; }
    CGInst *inst = new_inst(CG_LOAD_FIELD, pn);
    inst->rvals.add(rval_to_value(pn->rvals[1], lc));
    inst->field_idx = fi;
    if (pn->lvals.n) {
      inst->slot = get_or_make_local_slot(pn->lvals[0], lc);
      if (inst->slot) inst->result_type = inst->slot->type;
    }
    blk->body.add(inst);
    return;
  }

  // P_prim_setter: structural field store. rvals[1]=obj,
  // rvals[3]=field, rvals[4]=value. The setter also flows the
  // value back into lvals[0] (Python chained-assignment); we emit
  // an extra CG_STORE for the lval bind when live.
  if (idx == P_prim_setter) {
    int fi = resolve_field_index(pn, /*field_rval_idx=*/3, /*obj_rval_idx=*/1);
    if (fi < 0 || pn->rvals.n < 5) { emit_generic_call(pn, blk, lc); return; }
    CGInst *store = new_inst(CG_STORE_FIELD, pn);
    store->rvals.add(rval_to_value(pn->rvals[1], lc));  // object
    store->rvals.add(rval_to_value(pn->rvals[4], lc));  // value
    store->field_idx = fi;
    blk->body.add(store);
    // Forward the value into the lvalue (chained-assignment).
    if (pn->lvals.n && pn->lvals[0]->live) {
      CGInst *fwd = new_inst(CG_STORE, pn);
      fwd->slot = get_or_make_local_slot(pn->lvals[0], lc);
      fwd->rvals.add(rval_to_value(pn->rvals[4], lc));
      blk->body.add(fwd);
    }
    return;
  }

  // P_prim_new: heap allocation typed by lvals[0]. Bind into the
  // result slot.
  if (idx == P_prim_new) {
    if (!pn->lvals.n) { emit_generic_call(pn, blk, lc); return; }
    CGInst *inst = new_inst(CG_ALLOC, pn);
    inst->slot = get_or_make_local_slot(pn->lvals[0], lc);
    if (inst->slot) inst->result_type = inst->slot->type;
    blk->body.add(inst);
    return;
  }

  // P_prim_clone / P_prim_clone_vector: allocate-and-copy from
  // prototype. For now emit CG_ALLOC + a CG_CALL hint (the LLVM
  // emitter can switch on the prim to decide between GC_malloc +
  // memcpy and the C runtime's _CG_prim_clone helper).
  if (idx == P_prim_clone || idx == P_prim_clone_vector) {
    if (!pn->lvals.n) { emit_generic_call(pn, blk, lc); return; }
    CGInst *inst = new_inst(CG_ALLOC, pn);
    inst->slot = get_or_make_local_slot(pn->lvals[0], lc);
    inst->prim = pn->prim;  // hint: clone vs. plain new
    if (inst->slot) inst->result_type = inst->slot->type;
    blk->body.add(inst);
    return;
  }

  // P_prim_index_object: vector/list element load. rvals[1]=obj,
  // rvals[2]=index. The exact CG_OP depends on whether the index
  // is a compile-time constant; for now route through CG_LOAD_FIELD
  // with field_idx = -1 (sentinel meaning "use a CGValue index").
  if (idx == P_prim_index_object) {
    if (pn->rvals.n < 3) { emit_generic_call(pn, blk, lc); return; }
    CGInst *inst = new_inst(CG_LOAD_FIELD, pn);
    inst->rvals.add(rval_to_value(pn->rvals[1], lc));
    inst->rvals.add(rval_to_value(pn->rvals[2], lc));
    inst->field_idx = -1;  // dynamic index (CGValue in rvals[1])
    if (pn->lvals.n) {
      inst->slot = get_or_make_local_slot(pn->lvals[0], lc);
      if (inst->slot) inst->result_type = inst->slot->type;
    }
    blk->body.add(inst);
    return;
  }

  // Fallthrough: arithmetic / comparison / registered cgfn — keep
  // CG_CALL with prim hint preserved. The LLVM emitter dispatches
  // on prim->index via the existing per-prim helpers in
  // llvm_primitives.cc.
  emit_generic_call(pn, blk, lc);
}

// 2.4 — phi/phy materialization. Emit unconditionally; this is the
// structural fix for issue 016. phi follows the PNode (run on
// successors' entry); phy precedes the PNode (run on this block
// before the kind-specific emission). For now we treat both as
// CG_STORE in the owning block — refinement to predecessor-block
// placement for phi lands with the LLVM emitter (Phase 3) which
// is where the placement actually matters.
static void materialize_phi_phy(PNode *pn, CGBlock *blk, LowerCtx &lc) {
  for (PNode *pp : pn->phy) lower_move(pp, blk, lc);
}
static void materialize_phi(PNode *pn, CGBlock *blk, LowerCtx &lc) {
  for (PNode *pp : pn->phi) lower_move(pp, blk, lc);
}

// Emit a terminator for a block. Called once per block from the
// per-Fun lowering after all body instructions are placed. The
// terminator kind comes from the PNode that closed the block.
static void emit_terminator(PNode *closer, CGBlock *blk, LowerCtx &lc) {
  if (!closer || !closer->code) {
    blk->terminator = new_inst(CG_UNREACHABLE, closer);
    return;
  }
  Code *c = closer->code;
  switch (c->kind) {
    case Code_GOTO: {
      CGInst *t = new_inst(CG_BR, closer);
      blk->terminator = t;
      break;
    }
    case Code_IF: {
      CGInst *t = new_inst(CG_COND_BR, closer);
      if (closer->rvals.n) t->rvals.add(rval_to_value(closer->rvals[0], lc));
      blk->terminator = t;
      break;
    }
    case Code_SEND: {
      // @primitive @reply: treat as RET. Detection by checking the
      // first rval's name is approximate; the Phase 3 emitter
      // refines via Prim. For Phase 2 the test phase only checks
      // structural shape, so an UNREACHABLE here is acceptable.
      blk->terminator = new_inst(CG_RET, closer);
      break;
    }
    default:
      blk->terminator = new_inst(CG_UNREACHABLE, closer);
      break;
  }
}

// Walk a Fun's CFG: each LABEL PNode (and the entry PNode) starts a
// CGBlock; the block accumulates every PNode reachable until the
// next LABEL or terminator. Lower each PNode into its block.
static void build_cgfun(Fun *f, CGProgram *p) {
  if (!f || !f->live || !f->entry) return;
  if (p->fun_map.get(f)) return;

  CGFun *cf = new CGFun();
  cf->source_fun = f;
  cf->name = f->cg_string;
  cf->is_external = f->is_external;
  cf->is_main = (if1->top && if1->top->fun == f);
  if (f->rets.n == 1 && f->rets[0] && f->rets[0]->type)
    cf->return_type = p->sym_to_type.get(f->rets[0]->type);
  Vec<Var *> arg_vars;
  f->args.get_values(arg_vars);
  for (Var *a : arg_vars) {
    if (!a) continue;
    cf->arg_types.add(a->type ? p->sym_to_type.get(a->type) : 0);
  }
  p->fun_map.put(f, cf);
  p->funs.add(cf);
  if (cf->is_main) p->main_fun = cf;

  LowerCtx lc;
  lc.cf = cf;
  lc.p = p;

  // Pass 1: identify block boundaries — LABEL PNodes and the entry
  // PNode each get a CGBlock. Walk cfg_succ from entry as a BFS,
  // visiting each PNode once.
  CGBlock *entry_blk = new CGBlock();
  entry_blk->id = 0;
  entry_blk->label = "entry";
  entry_blk->source_pn = f->entry;
  cf->entry = entry_blk;
  cf->blocks.add(entry_blk);
  lc.pn_to_block.put(f->entry, entry_blk);

  int next_block_id = 1;
  Vec<PNode *> worklist;
  worklist.add(f->entry);
  Vec<PNode *> all_pnodes;
  all_pnodes.set_add(f->entry);
  while (worklist.n) {
    PNode *cur = worklist.pop();
    for (PNode *succ : cur->cfg_succ) {
      if (!succ) continue;
      if (!all_pnodes.set_add(succ)) continue;
      if (succ->code && succ->code->kind == Code_LABEL) {
        CGBlock *b = new CGBlock();
        b->id = next_block_id++;
        char buf[32];
        if (succ->code->label[0])
          snprintf(buf, sizeof(buf), "L%d", succ->code->label[0]->id);
        else
          snprintf(buf, sizeof(buf), "B%d", b->id);
        b->label = dupstr(buf);
        b->source_pn = succ;
        cf->blocks.add(b);
        lc.pn_to_block.put(succ, b);
      }
      worklist.add(succ);
    }
  }

  // Pass 2: assign each PNode to its owning block via DFS from
  // entry. Owning block = most recent LABEL on the path. Lower
  // each PNode into its block.
  Vec<PNode *> visited;
  Vec<PNode *> dfs;
  Vec<CGBlock *> dfs_blk;
  dfs.add(f->entry);
  dfs_blk.add(entry_blk);
  visited.set_add(f->entry);
  while (dfs.n) {
    PNode *cur = dfs.pop();
    CGBlock *blk = dfs_blk.pop();
    lc.pn_owning.put(cur, blk);

    Code *c = cur->code;
    if (c) {
      // phy runs before the kind-specific lowering on this block.
      materialize_phi_phy(cur, blk, lc);
      switch (c->kind) {
        case Code_LABEL:
          // No body instruction; block boundary.
          break;
        case Code_MOVE:
          lower_move(cur, blk, lc);
          break;
        case Code_SEND:
          lower_send(cur, blk, lc);
          break;
        case Code_IF:
        case Code_GOTO:
          // Terminator; handled by emit_terminator below for the
          // last PNode in the block.
          break;
        default:
          break;
      }
      // phi runs after the kind-specific lowering.
      materialize_phi(cur, blk, lc);
    }

    for (PNode *succ : cur->cfg_succ) {
      if (!succ) continue;
      if (!visited.set_add(succ)) continue;
      CGBlock *succ_blk = lc.pn_to_block.get(succ);
      dfs.add(succ);
      dfs_blk.add(succ_blk ? succ_blk : blk);
    }
  }

  // Pass 3: wire block CFG edges at the block granularity and
  // emit terminators. A block's terminator is the last PNode in
  // it that has a cfg_succ leaving the block.
  for (CGBlock *b : cf->blocks) {
    PNode *closer = 0;
    // Find the last PNode in this block whose cfg_succ goes
    // outside the block. We approximate by walking source_pn's
    // cfg_succ and looking for the highest-priority terminator
    // PNode whose owning block == b.
    // Cheap heuristic: take source_pn itself plus walk forward
    // by cfg_succ while staying in the same block.
    Vec<PNode *> seen;
    Vec<PNode *> walk;
    walk.add(b->source_pn);
    while (walk.n) {
      PNode *w = walk.pop();
      if (!seen.set_add(w)) continue;
      if (lc.pn_owning.get(w) != b) continue;
      if (w->code && (w->code->kind == Code_IF ||
                      w->code->kind == Code_GOTO ||
                      w->code->kind == Code_SEND)) {
        // A terminator candidate; SEND counts as a closer when its
        // cfg_succ leaves the block OR is empty (function-exit
        // returns, typically @primitive @reply, have no successor
        // and lose the block edge otherwise).
        bool leaves = w->cfg_succ.n == 0;
        for (PNode *s : w->cfg_succ) {
          if (lc.pn_owning.get(s) != b) { leaves = true; break; }
        }
        if (leaves || w->code->kind != Code_SEND) closer = w;
      }
      for (PNode *s : w->cfg_succ) walk.add(s);
    }
    emit_terminator(closer, b, lc);
    if (closer) {
      for (PNode *s : closer->cfg_succ) {
        CGBlock *sb = lc.pn_to_block.get(s);
        if (sb && sb != b) {
          b->succs.set_add(sb);
          sb->preds.set_add(b);
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// 2.5 — Construction-flow patch (skipped)
// ---------------------------------------------------------------------------
//
// Per Phase 0 §5.3 outcome (a): the construction-flow Code_MOVE
// exists in IF1; Phase 2.4's "emit every Code_MOVE unconditionally"
// suffices. Issue 014 closes via 2.4, not via a separate peephole.

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

CGProgram *cg_normalize(FA *fa) {
  CGProgram *p = new CGProgram();
  if (!fa) return p;

  // 2.1 — Types + globals.
  build_cgtypes(fa, p);
  build_cgslots(fa, p);

  // 2.2 + 2.3 + 2.4 — Per-Fun lowering with phi/phy material.
  for (Fun *f : fa->funs) build_cgfun(f, p);

  return p;
}
