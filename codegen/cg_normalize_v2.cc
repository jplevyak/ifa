// SPDX-License-Identifier: BSD-3-Clause
//
// cg_normalize_v2.cc — Phase B of the CG_IR_v2 migration.
//
// Translates IF1 (Sym/Var/Fun/PNode/Code) into a CGv2Program.
// This is the input side of the v2 LLVM emitter — once this
// produces a program with full fidelity to v1's cg_normalize,
// Phase B's pass criterion is met and the pyc-suite can run
// behind IFA_LLVM_V2=1.
//
// Landing cadence: one IF1 concept per commit, with a test
// per landing. Same pattern as the prim layer in Phase A.

#include "ifadefs.h"

#include "codegen/cg_normalize_v2.h"

#include "codegen/cg_ir_v2.h"
#include "code.h"
#include "fa.h"
#include "fun.h"
#include "num.h"
#include "pnode.h"
#include "prim.h"
#include "prim_data.h"
#include "sym.h"

namespace {

struct FunCtx;

// Per-translation context. Holds the CGv2Program being built
// plus the maps subsequent passes read to resolve IF1
// entities to CGv2 entities.
struct NormCtx {
  CGv2Program *p;
  Map<Sym *, CGv2Type *> sym_to_type;
  Map<Sym *, CGv2Value *> sym_to_value;   // globals + constants
  Map<Fun *, CGv2Fun *> fun_to_fun;        // IF1 Fun → CGv2Fun
  Map<Fun *, FunCtx *> fun_to_ctx;         // IF1 Fun → FunCtx

  explicit NormCtx(CGv2Program *prog) : p(prog) {}
};

// Per-Fun lowering context. Created at decl time so formals
// have a stable Var → CGv2Value mapping by body-build time.
struct FunCtx {
  CGv2Fun *cf;
  Map<PNode *, CGv2Block *> pn_to_block;
  Map<Var *, CGv2Value *> var_to_value;
};

// Map an IF1 numeric Sym (e.g. sym_int64, sym_uint8, sym_float32)
// to the matching predefined CGv2Type from the program.
// Returns nullptr if the Sym isn't a recognized numeric kind.
CGv2Type *predef_numeric(NormCtx &c, Sym *s) {
  switch (s->num_kind) {
    case IF1_NUM_KIND_UINT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:  return c.p->t_bool;
        case IF1_INT_TYPE_8:  return c.p->t_uint8;
        case IF1_INT_TYPE_16: return c.p->t_uint16;
        case IF1_INT_TYPE_32: return c.p->t_uint32;
        case IF1_INT_TYPE_64: return c.p->t_uint64;
      }
      break;
    case IF1_NUM_KIND_INT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:  return c.p->t_bool;
        case IF1_INT_TYPE_8:  return c.p->t_int8;
        case IF1_INT_TYPE_16: return c.p->t_int16;
        case IF1_INT_TYPE_32: return c.p->t_int32;
        case IF1_INT_TYPE_64: return c.p->t_int64;
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      switch (s->num_index) {
        case IF1_FLOAT_TYPE_32: return c.p->t_float32;
        case IF1_FLOAT_TYPE_64: return c.p->t_float64;
      }
      break;
    default:
      break;
  }
  return nullptr;
}

CGv2Type *build_type(NormCtx &c, Sym *s);

CGv2Type *build_struct_type(NormCtx &c, Sym *s) {
  CGv2Type *t = new CGv2Type();
  t->id = 1000 + c.p->types.n;
  t->name = s->name ? s->name : (s->cg_string ? s->cg_string : "anon");
  t->kind = CG2T_STRUCT;
  t->is_heap_aggregate = true;       // pyc's default for RECORD
  c.sym_to_type.put(s, t);            // register first (recursion guard)
  c.p->types.add(t);
  int idx = 0;
  for (Sym *f : s->has) {
    CGv2TypeField *cf = new CGv2TypeField();
    cf->name = f->name;
    cf->type = build_type(c, f->type);
    cf->idx = idx++;
    t->fields.add(cf);
  }
  if (s->element) t->element = build_type(c, s->element->type);
  return t;
}

CGv2Type *build_type(NormCtx &c, Sym *s) {
  if (!s) return nullptr;
  CGv2Type *cached = c.sym_to_type.get(s);
  if (cached) return cached;

  // Predefined numeric types short-circuit to the program's
  // canonical instance — no per-Sym duplicate created.
  if (s->num_kind) {
    CGv2Type *t = predef_numeric(c, s);
    if (t) { c.sym_to_type.put(s, t); return t; }
  }
  if (s == sym_void) {
    c.sym_to_type.put(s, c.p->t_void);
    return c.p->t_void;
  }
  if (s == sym_nil_type) {
    c.sym_to_type.put(s, c.p->t_nil);
    return c.p->t_nil;
  }
  if (s->is_symbol) {
    c.sym_to_type.put(s, c.p->t_sym);
    return c.p->t_sym;
  }
  if (s->type_kind == Type_RECORD) {
    return build_struct_type(c, s);
  }
  // Type_FUN, Type_REF, Type_PRIMITIVE → treat as opaque ptr.
  // Refined when their tests land.
  c.sym_to_type.put(s, c.p->t_ptr);
  return c.p->t_ptr;
}

void build_types(NormCtx &c, FA *fa) {
  Vec<Sym *> typesyms;
  Vec<Var *> globals;
  collect_types_and_globals(fa, typesyms, globals);
  for (Sym *s : typesyms) (void)build_type(c, s);
}

// Translate an IF1 Immediate (carried on Sym::imm for constant
// syms) into a CGv2Immediate. Best-effort across numeric kinds
// and strings; sets I_NONE on unrecognized kinds so the
// downstream emitter can fall back to undef.
void build_immediate(const Immediate &src, CGv2Immediate &dst) {
  switch (src.const_kind) {
    case IF1_NUM_KIND_INT: {
      dst.kind = CGv2Immediate::I_INT;
      // Imm carries a tagged width — coerce up to int64 for
      // the CGv2 immediate.
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
      // IF1_INT_TYPE_1 with UINT kind is bool semantically; we
      // model it as I_BOOL so the v2 emitter picks i1 properly.
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

// Translate a global IF1 Var → CGv2Value. Constants land in
// prog->constants with their imm populated; non-constants
// land in prog->globals.
CGv2Value *build_global(NormCtx &c, Var *v) {
  if (!v || !v->sym) return nullptr;
  if (CGv2Value *cached = c.sym_to_value.get(v->sym)) return cached;

  CGv2Value *cv = new CGv2Value();
  cv->id = 1000 + c.p->constants.n + c.p->globals.n;
  cv->name = v->sym->name ? v->sym->name : v->cg_string;
  cv->type = v->type ? build_type(c, v->type) : c.p->t_ptr;

  if (v->sym->is_constant) {
    cv->scope = CG2V_CONSTANT;
    build_immediate(v->sym->imm, cv->imm);
    c.p->constants.add(cv);
  } else {
    cv->scope = CG2V_GLOBAL;
    c.p->globals.add(cv);
  }
  c.sym_to_value.put(v->sym, cv);
  return cv;
}

void build_globals(NormCtx &c, FA *fa) {
  Vec<Sym *> typesyms;
  Vec<Var *> globals;
  collect_types_and_globals(fa, typesyms, globals);
  for (Var *v : globals) (void)build_global(c, v);
}

// Declare an IF1 Fun as a CGv2Fun — signature, formals, but
// no body. Body translation lands in B.5+. Mirrors the
// per-Fun head of v1's build_cgfun (cg_normalize.cc:472).
CGv2Fun *build_fun_decl(NormCtx &c, Fun *f) {
  if (!f || !f->live || !f->entry) return nullptr;
  if (CGv2Fun *cached = c.fun_to_fun.get(f)) return cached;

  CGv2Fun *cf = new CGv2Fun();
  cf->id = c.p->funs.n;
  cf->name = f->cg_string ? f->cg_string :
             (f->sym && f->sym->name ? f->sym->name : "fn");
  cf->is_external = f->is_external;
  cf->is_varargs = f->is_varargs;
  cf->is_main = (if1->top && if1->top->fun == f);

  cf->signature = new CGv2Sig();
  cf->signature->is_varargs = f->is_varargs;
  if (f->rets.n == 1 && f->rets[0] && f->rets[0]->type) {
    cf->signature->ret = build_type(c, f->rets[0]->type);
  } else {
    cf->signature->ret = c.p->t_void;
  }

  // Args come from a map keyed by MPosition; use the same
  // get_values helper v1 uses to extract them in order.
  // Each formal's Var is registered in the per-Fun FunCtx
  // so body translation resolves arg references cleanly.
  FunCtx *fc = new FunCtx();
  fc->cf = cf;

  Vec<Var *> arg_vars;
  f->args.get_values(arg_vars);
  for (Var *a : arg_vars) {
    if (!a) continue;
    CGv2Type *at = a->type ? build_type(c, a->type) : c.p->t_ptr;
    cf->signature->args.add(at);
    CGv2Value *formal = new CGv2Value();
    formal->id = 1000 + c.p->funs.n * 100 + cf->formals.n;
    formal->name = a->sym && a->sym->name ? a->sym->name :
                   (a->cg_string ? a->cg_string : "arg");
    formal->type = at;
    formal->scope = CG2V_FORMAL;
    cf->formals.add(formal);
    fc->var_to_value.put(a, formal);
  }

  c.fun_to_fun.put(f, cf);
  c.fun_to_ctx.put(f, fc);
  c.p->funs.add(cf);
  if (cf->is_main) c.p->main_fun = cf;
  return cf;
}

void build_funs(NormCtx &c, FA *fa) {
  for (Fun *f : fa->funs) (void)build_fun_decl(c, f);
}

// Pass 1 of build_fun_body — walk the CFG from the entry PNode
// in BFS order. Create a CGv2Block for the entry and one for
// each LABEL PNode. Subsequent passes (B.6+) place insts in
// the right block via pn_to_block.
void build_block_skeleton(NormCtx &c, Fun *f, FunCtx &fc) {
  (void)c;
  CGv2Block *entry_blk = new CGv2Block();
  entry_blk->id = 0;
  entry_blk->name = "entry";
  fc.cf->entry = entry_blk;
  fc.cf->blocks.add(entry_blk);
  fc.pn_to_block.put(f->entry, entry_blk);

  int next_id = 1;
  Vec<PNode *> worklist;
  Vec<PNode *> seen;
  worklist.add(f->entry);
  seen.set_add(f->entry);
  while (worklist.n) {
    PNode *cur = worklist.pop();
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
        fc.cf->blocks.add(b);
        fc.pn_to_block.put(succ, b);
      }
      worklist.add(succ);
    }
  }
}

// Resolve an IF1 Var → CGv2Value. Order:
//   per-Fn formals/locals (var_to_value) → globals/constants
//   (sym_to_value via the Var's Sym) → fresh local (created
//   on first use, similar to v1's get_or_make_local_slot).
CGv2Value *build_var(NormCtx &c, FunCtx &fc, Var *v) {
  if (!v) return nullptr;
  if (CGv2Value *cached = fc.var_to_value.get(v)) return cached;
  if (v->sym) {
    if (CGv2Value *g = c.sym_to_value.get(v->sym)) return g;
  }
  // Fresh local. Name from Var/Sym; type via build_type.
  CGv2Value *cv = new CGv2Value();
  cv->id = 2000 + fc.cf->id * 1000 + fc.cf->locals.n;
  cv->name = v->sym && v->sym->name ? v->sym->name :
             (v->cg_string ? v->cg_string : "v");
  cv->type = v->type ? build_type(c, v->type) : c.p->t_ptr;
  cv->scope = CG2V_LOCAL;
  fc.var_to_value.put(v, cv);
  fc.cf->locals.add(cv);
  return cv;
}

// Lower a Code_MOVE PNode into a CG2_MOVE inst placed in
// `blk`. Mirrors v1's lower_move (cg_normalize.cc:230).
void lower_move(NormCtx &c, FunCtx &fc, PNode *pn, CGv2Block *blk) {
  if (!pn || pn->rvals.n < 1 || pn->lvals.n < 1) return;
  CGv2Value *src = build_var(c, fc, pn->rvals[0]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!src || !dst) return;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_MOVE;
  inst->rvals.add(src);
  inst->lvals.add(dst);
  blk->body.add(inst);
}

// Map an IF1 arithmetic/comparison/bitwise prim index to a
// CG_BINOP sub-op. Returns CG2B_NONE for non-binop prims
// (caller falls through to other dispatch).
CGv2BinSub prim_to_binop(int idx) {
  switch (idx) {
    case P_prim_add:            return CG2B_ADD;
    case P_prim_subtract:       return CG2B_SUB;
    case P_prim_mult:           return CG2B_MUL;
    case P_prim_div:            return CG2B_DIV;
    case P_prim_mod:            return CG2B_MOD;
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

// Lower a Code_SEND with an arithmetic / comparison / bitwise
// prim into a CG2_BINOP. Mirrors v1's per-prim handling at
// llvm_primitives.cc:463 (P_prim_operator + family). Operand
// pattern: rvals[n-3] = lhs, rvals[n-1] = rhs, lvals[0] = result.
// Returns true if handled (caller stops dispatch).
bool lower_send_binop(NormCtx &c, FunCtx &fc, PNode *pn,
                       CGv2Block *blk, CGv2BinSub sub) {
  if (sub == CG2B_NONE) return false;
  if (pn->rvals.n < 4 || pn->lvals.n < 1) return false;
  CGv2Value *a = build_var(c, fc, pn->rvals[pn->rvals.n - 3]);
  CGv2Value *b = build_var(c, fc, pn->rvals[pn->rvals.n - 1]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!a || !b || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_BINOP;
  inst->sub_op = sub;
  inst->rvals.add(a);
  inst->rvals.add(b);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// Resolve the integer field index of a P_prim_period /
// P_prim_setter send. Mirrors v1's resolve_field_index
// (cg_normalize.cc:244): pn->rvals[obj_idx]->type carries
// the struct Sym; pn->rvals[field_idx] is the field-name
// symbol Var. Returns -1 on unresolvable shapes (dynamic
// dispatch — caller falls through to CG_CALL).
int resolve_field_index_v2(PNode *pn, int field_rval_idx,
                            int obj_rval_idx) {
  if (field_rval_idx >= pn->rvals.n || obj_rval_idx >= pn->rvals.n)
    return -1;
  Var *field_var = pn->rvals[field_rval_idx];
  Var *obj_var = pn->rvals[obj_rval_idx];
  if (!field_var || !obj_var || !obj_var->type) return -1;
  Sym *obj = obj_var->type;
  if (obj->type_kind == Type_SUM && obj->has.n) obj = obj->has[0]->type;
  cchar *symbol = nullptr;
  if (field_var->sym && field_var->sym->is_symbol)
    symbol = field_var->sym->name;
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

// P_prim_period — structural field load.
//   rvals[1] = object, rvals[3] = field selector.
//   lvals[0] = loaded value.
// Returns true if handled; false if shape doesn't resolve
// (dynamic dispatch, caller falls through to CG_CALL).
bool lower_send_period(NormCtx &c, FunCtx &fc, PNode *pn,
                        CGv2Block *blk) {
  int fi = resolve_field_index_v2(pn, 3, 1);
  if (fi < 0 || pn->lvals.n < 1) return false;
  CGv2Value *obj = build_var(c, fc, pn->rvals[1]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!obj || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_FIELD_LOAD;
  inst->field_idx = fi;
  inst->type_arg = obj->type;            // :struct hint
  inst->rvals.add(obj);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_setter — structural field store.
//   rvals[1]=obj, rvals[3]=field, rvals[4]=value.
// v1 also forwards the value into lvals[0] for chained
// assignment; we do the same via a CG2_MOVE.
bool lower_send_setter(NormCtx &c, FunCtx &fc, PNode *pn,
                        CGv2Block *blk) {
  int fi = resolve_field_index_v2(pn, 3, 1);
  if (fi < 0 || pn->rvals.n < 5) return false;
  CGv2Value *obj = build_var(c, fc, pn->rvals[1]);
  CGv2Value *val = build_var(c, fc, pn->rvals[4]);
  if (!obj || !val) return false;
  CGv2Inst *store = new CGv2Inst();
  store->op = CG2_FIELD_STORE;
  store->field_idx = fi;
  store->type_arg = obj->type;            // :struct hint
  store->rvals.add(obj);
  store->rvals.add(val);
  blk->body.add(store);
  // Chained-assignment forward (Python semantics).
  if (pn->lvals.n > 0) {
    CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
    if (dst) {
      CGv2Inst *fwd = new CGv2Inst();
      fwd->op = CG2_MOVE;
      fwd->rvals.add(val);
      fwd->lvals.add(dst);
      blk->body.add(fwd);
    }
  }
  return true;
}

// P_prim_new / P_prim_make — heap allocation. The result type
// comes from lvals[0]->type (the result-Var carries the type
// it will hold post-construction).
bool lower_send_alloc(NormCtx &c, FunCtx &fc, PNode *pn,
                      CGv2Block *blk) {
  if (pn->lvals.n < 1) return false;
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!dst || !dst->type) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_ALLOC;
  inst->type_arg = dst->type;
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_clone / P_prim_clone_vector — allocate-and-copy from
// prototype. rvals[1] = prototype object. Result type from
// lvals[0]->type, same as alloc.
bool lower_send_clone(NormCtx &c, FunCtx &fc, PNode *pn,
                      CGv2Block *blk) {
  if (pn->rvals.n < 2 || pn->lvals.n < 1) return false;
  CGv2Value *proto = build_var(c, fc, pn->rvals[1]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!proto || !dst || !dst->type) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_CLONE;
  inst->type_arg = dst->type;
  inst->rvals.add(proto);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_index_object — vector / list element load.
//   rvals[1] = obj, rvals[2] = index.
bool lower_send_index_load(NormCtx &c, FunCtx &fc, PNode *pn,
                            CGv2Block *blk) {
  if (pn->rvals.n < 3 || pn->lvals.n < 1) return false;
  CGv2Value *obj = build_var(c, fc, pn->rvals[1]);
  CGv2Value *idx = build_var(c, fc, pn->rvals[2]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!obj || !idx || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_INDEX_LOAD;
  inst->rvals.add(obj);
  inst->rvals.add(idx);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_set_index_object — vector / list element store.
//   rvals[1] = obj, rvals[2] = index, rvals[3] = value.
bool lower_send_index_store(NormCtx &c, FunCtx &fc, PNode *pn,
                             CGv2Block *blk) {
  if (pn->rvals.n < 4) return false;
  CGv2Value *obj = build_var(c, fc, pn->rvals[1]);
  CGv2Value *idx = build_var(c, fc, pn->rvals[2]);
  CGv2Value *val = build_var(c, fc, pn->rvals[3]);
  if (!obj || !idx || !val) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_INDEX_STORE;
  inst->rvals.add(obj);
  inst->rvals.add(idx);
  inst->rvals.add(val);
  blk->body.add(inst);
  return true;
}

// Lower a Code_SEND without a (recognized) prim into a
// CG2_CALL. The callee is resolved via the FA's
// caller->calls map: when it contains exactly one Fun for
// this PNode, we can statically dispatch. Multi-target or
// missing call edges fall through (B.8.4+ will refine for
// dynamic-dispatch sites).
//
// Mirrors v1's emit_generic_call (cg_normalize.cc:269) at
// the IR level + v1's write_send for callee resolution
// (llvm_primitives.cc:217-235) + codegen_common.cc:97's
// get_target_fun_core.
//
// Returns true if handled.
bool lower_send_call(NormCtx &c, FunCtx &fc, PNode *pn,
                      Fun *caller, CGv2Block *blk) {
  if (!caller) return false;
  Vec<Fun *> *callees = caller->calls.get(pn);
  if (!callees || callees->n != 1) return false;
  Fun *target = callees->v[0];
  if (!target) return false;
  CGv2Fun *target_cf = c.fun_to_fun.get(target);
  if (!target_cf || !target_cf->name) return false;

  // fun_ref CGv2Value for the call site. The v2 emit's
  // CG2_CALL case reads target_name to look up the
  // llvm::Function. Cheap to construct per-site; the v2
  // emitter shares the llvm::Function across sites by name.
  CGv2Value *fnref = new CGv2Value();
  fnref->id = 5000 + fc.cf->id * 100 + (int)blk->body.n;
  fnref->name = target_cf->name;
  fnref->type = c.p->t_fun_ptr;
  fnref->scope = CG2V_FUN_REF;
  fnref->target_name = target_cf->name;

  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_CALL;
  inst->rvals.add(fnref);

  // Args from pn->rvals. v1's emit_generic_call adds all
  // rvals to the CG_CALL preserving everything for the LLVM
  // backend's later MPosition-based remapping. For v2 we
  // forward rvals[1..] verbatim — the LLVM-call CreateCall
  // expects them in declaration order, which they are when
  // the IF1 emit is well-formed. (Mismatches surface as
  // verifyModule errors and inform a later refinement.)
  for (int i = 1; i < pn->rvals.n; i++) {
    CGv2Value *cv = build_var(c, fc, pn->rvals[i]);
    if (cv) inst->rvals.add(cv);
  }
  if (pn->lvals.n > 0) {
    CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
    if (dst) inst->lvals.add(dst);
  }
  blk->body.add(inst);
  return true;
}

// Lower a Code_SEND with P_prim_primitive (the named
// RegisteredPrim form) into a CG2_PRIM. rvals[1] carries the
// name Var (sym->name or sym->constant); rvals[2..] are the
// args. Mirrors v1's llvm_primitives.cc:1111 name extraction.
// Returns true if handled.
bool lower_send_prim(NormCtx &c, FunCtx &fc, PNode *pn,
                      CGv2Block *blk) {
  if (pn->rvals.n < 2) return false;
  Var *name_var = pn->rvals[1];
  if (!name_var || !name_var->sym) return false;
  cchar *name = name_var->sym->name;
  if (!name) name = name_var->sym->constant;
  if (!name) return false;

  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_PRIM;
  inst->prim_name = name;
  for (int i = 2; i < pn->rvals.n; i++) {
    CGv2Value *cv = build_var(c, fc, pn->rvals[i]);
    if (cv) inst->rvals.add(cv);
  }
  if (pn->lvals.n > 0) {
    CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
    if (dst) inst->lvals.add(dst);
  }
  blk->body.add(inst);
  return true;
}

// Lower a Code_SEND PNode. Dispatches on pn->prim, with a
// fall-through to lower_send_call (regular Fun-targeted call)
// for the no-prim or unrecognized-prim case.
//
// Currently supports:
//   P_prim_reply         — handled by build_terminator (B.8.1)
//   arithmetic family    — CG2_BINOP (B.8.2)
//   P_prim_primitive     — CG2_PRIM (B.8.3)
//   no prim / other      — CG2_CALL via lower_send_call (B.8.4)
//   P_prim_period/setter — CG_FIELD_LOAD/STORE (B.8.5)
//   P_prim_new/make      — CG_ALLOC (B.8.5)
//   P_prim_clone[_vector]— CG_CLONE (B.8.5)
//   P_prim_index_object  — CG_INDEX_LOAD (B.8.5)
//   P_prim_set_index_object — CG_INDEX_STORE (B.8.5)
void lower_send(NormCtx &c, FunCtx &fc, PNode *pn, Fun *caller,
                CGv2Block *blk) {
  if (!pn) return;
  if (pn->prim) {
    int idx = pn->prim->index;
    if (idx == P_prim_reply) return;     // terminator
    if (CGv2BinSub sub = prim_to_binop(idx); sub != CG2B_NONE) {
      lower_send_binop(c, fc, pn, blk, sub);
      return;
    }
    if (idx == P_prim_primitive) {
      lower_send_prim(c, fc, pn, blk);
      return;
    }
    // Aggregate ops. Each handler returns true if it can
    // structurally decompose the SEND; on false (dynamic
    // dispatch / shape mismatch) we fall through to CG_CALL
    // so the FA's call-edge resolution can take over.
    if (idx == P_prim_period &&
        lower_send_period(c, fc, pn, blk)) return;
    if (idx == P_prim_setter &&
        lower_send_setter(c, fc, pn, blk)) return;
    if ((idx == P_prim_new || idx == P_prim_make) &&
        lower_send_alloc(c, fc, pn, blk)) return;
    if ((idx == P_prim_clone || idx == P_prim_clone_vector) &&
        lower_send_clone(c, fc, pn, blk)) return;
    if (idx == P_prim_index_object &&
        lower_send_index_load(c, fc, pn, blk)) return;
    if (idx == P_prim_set_index_object &&
        lower_send_index_store(c, fc, pn, blk)) return;
  }
  lower_send_call(c, fc, pn, caller, blk);
}

// Pass 2 of build_fun_body — DFS from entry, tracking owning
// block (most recent LABEL ancestor on the path). For each
// PNode, dispatch on Code kind:
//   Code_LABEL → no body inst (boundary marker)
//   Code_MOVE  → CG2_MOVE
//   others     → defer to later landings
// Also detects each block's "closer" PNode — the last PNode
// before the block's cfg exits — and stashes it in
// `block_closer` for terminator emission.
void lower_body(NormCtx &c, Fun *f, FunCtx &fc,
                Map<CGv2Block *, PNode *> &block_closer) {
  if (!f->entry) return;
  Vec<PNode *> stack;
  Vec<CGv2Block *> stack_blk;
  Vec<PNode *> visited;
  stack.add(f->entry);
  stack_blk.add(fc.pn_to_block.get(f->entry));
  visited.set_add(f->entry);
  while (stack.n) {
    PNode *cur = stack.pop();
    CGv2Block *blk = stack_blk.pop();
    Code *cd = cur->code;
    if (cd && blk) {
      switch (cd->kind) {
        case Code_LABEL:
          break;
        case Code_MOVE:
          lower_move(c, fc, cur, blk);
          break;
        case Code_SEND:
          lower_send(c, fc, cur, f, blk);
          break;
        default:
          // Code_IF/GOTO closers are recognized by
          // build_terminator below.
          break;
      }
      // A PNode is a closer if any of its cfg_succ leaves
      // the current block (different CGv2Block) or it has
      // no cfg_succ at all.
      bool is_closer = cur->cfg_succ.n == 0;
      for (PNode *s : cur->cfg_succ) {
        if (!s) continue;
        CGv2Block *sblk = (s->code && s->code->kind == Code_LABEL)
                              ? fc.pn_to_block.get(s)
                              : blk;
        if (sblk != blk) { is_closer = true; break; }
      }
      if (is_closer) block_closer.put(blk, cur);
    }
    for (PNode *s : cur->cfg_succ) {
      if (!s || !visited.set_add(s)) continue;
      CGv2Block *succ_blk =
          (s->code && s->code->kind == Code_LABEL)
              ? fc.pn_to_block.get(s)
              : blk;
      stack.add(s);
      stack_blk.add(succ_blk);
    }
  }
}

// Translate a block's closer PNode into a CGv2Inst terminator.
// Mirrors v1's emit_terminator (cg_normalize.cc:425).
//
// Supported in this landing:
//   Code_GOTO → CG2_BR with br_target = succ block
//   Code_IF   → CG2_COND_BR with cond=rvals[0], targets from
//               cfg_succ[0] (true) / cfg_succ[1] (false)
//   no succs  → CG2_RET (void; rval translation lands when
//               Code_SEND reply handling does in B.8)
//   other     → CG2_BR to the single successor block (the
//               implicit fall-through case)
void build_terminator(NormCtx &c, FunCtx &fc, CGv2Block *blk,
                       PNode *closer) {
  if (blk->terminator) return;
  CGv2Inst *term = new CGv2Inst();
  if (!closer || !closer->code) {
    term->op = CG2_UNREACHABLE;
    blk->terminator = term;
    return;
  }
  Code *cd = closer->code;
  // Find which CGv2Blocks the closer's cfg_succ leads to,
  // unique, in source order.
  Vec<CGv2Block *> succ_blocks;
  for (PNode *s : closer->cfg_succ) {
    if (!s) continue;
    CGv2Block *sblk = (s->code && s->code->kind == Code_LABEL)
                          ? fc.pn_to_block.get(s)
                          : nullptr;
    if (sblk) succ_blocks.set_add(sblk);
  }
  switch (cd->kind) {
    case Code_GOTO:
      term->op = CG2_BR;
      term->br_target = succ_blocks.n > 0 ? succ_blocks[0] : nullptr;
      break;
    case Code_IF:
      term->op = CG2_COND_BR;
      if (closer->rvals.n > 0) {
        CGv2Value *cv = build_var(c, fc, closer->rvals[0]);
        if (cv) term->rvals.add(cv);
      }
      term->br_true  = succ_blocks.n > 0 ? succ_blocks[0] : nullptr;
      term->br_false = succ_blocks.n > 1 ? succ_blocks[1] : nullptr;
      break;
    case Code_SEND:
      // P_prim_reply: the IF1 "return" form. rvals[3] is the
      // value to return. Other SEND kinds land as body insts
      // in B.8.2+ — those wouldn't be closers anyway (a
      // returning fn always ends in @reply).
      if (closer->prim && closer->prim->index == P_prim_reply) {
        term->op = CG2_RET;
        if (closer->rvals.n >= 4) {
          CGv2Value *cv = build_var(c, fc, closer->rvals[3]);
          if (cv) term->rvals.add(cv);
        }
      } else if (succ_blocks.n == 0) {
        term->op = CG2_UNREACHABLE;
      } else {
        term->op = CG2_BR;
        term->br_target = succ_blocks[0];
      }
      break;
    default:
      if (succ_blocks.n == 0) {
        term->op = CG2_RET;            // void
      } else {
        term->op = CG2_BR;
        term->br_target = succ_blocks[0];
      }
      break;
  }
  blk->terminator = term;
}

// Per-Fun body build. Block skeleton (B.5) + body insts (B.6)
// + terminator translation (B.7). Any block without a closer
// after the walk gets an UNREACHABLE so verifyModule stays
// happy.
void build_fun_body(NormCtx &c, Fun *f, CGv2Fun *cf, FunCtx &fc) {
  if (!f || !cf || !f->entry) return;
  build_block_skeleton(c, f, fc);
  Map<CGv2Block *, PNode *> block_closer;
  lower_body(c, f, fc, block_closer);
  for (CGv2Block *b : cf->blocks) {
    build_terminator(c, fc, b, block_closer.get(b));
  }
}

void build_fun_bodies(NormCtx &c, FA *fa) {
  for (Fun *f : fa->funs) {
    CGv2Fun *cf = c.fun_to_fun.get(f);
    FunCtx *fc = c.fun_to_ctx.get(f);
    if (!cf || !fc || cf->is_external) continue;
    build_fun_body(c, f, cf, *fc);
  }
}

}  // namespace

CGv2Program *cg_normalize_v2(FA *fa) {
  CGv2Program *p = new CGv2Program();
  if (!fa) return p;

  NormCtx c(p);
  build_types(c, fa);
  build_globals(c, fa);
  build_funs(c, fa);
  build_fun_bodies(c, fa);

  return p;
}
