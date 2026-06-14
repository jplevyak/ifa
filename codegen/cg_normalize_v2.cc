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
// plus the Sym → CGv2Type map used by subsequent passes to
// resolve type references on values.
struct NormCtx {
  CGv2Program *p;
  Map<Sym *, CGv2Type *> sym_to_type;

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

}  // namespace

CGv2Program *cg_normalize_v2(FA *fa) {
  CGv2Program *p = new CGv2Program();
  if (!fa) return p;

  NormCtx c(p);
  build_types(c, fa);

  return p;
}
