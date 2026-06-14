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
#include "fa.h"
#include "fun.h"
#include "num.h"
#include "sym.h"

namespace {

// Per-translation context. Holds the CGv2Program being built
// plus the maps subsequent passes read to resolve IF1
// entities to CGv2 entities.
struct NormCtx {
  CGv2Program *p;
  Map<Sym *, CGv2Type *> sym_to_type;
  Map<Sym *, CGv2Value *> sym_to_value;   // globals + constants
  Map<Fun *, CGv2Fun *> fun_to_fun;        // IF1 Fun → CGv2Fun

  explicit NormCtx(CGv2Program *prog) : p(prog) {}
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
  }

  c.fun_to_fun.put(f, cf);
  c.p->funs.add(cf);
  if (cf->is_main) c.p->main_fun = cf;
  return cf;
}

void build_funs(NormCtx &c, FA *fa) {
  for (Fun *f : fa->funs) (void)build_fun_decl(c, f);
}

}  // namespace

CGv2Program *cg_normalize_v2(FA *fa) {
  CGv2Program *p = new CGv2Program();
  if (!fa) return p;

  NormCtx c(p);
  build_types(c, fa);
  build_globals(c, fa);
  build_funs(c, fa);

  return p;
}
