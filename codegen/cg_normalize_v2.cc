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
#include "codegen/codegen_common.h"
#include "builtin.h"
#include "code.h"
#include "fa.h"
#include "fun.h"
#include "num.h"
#include "pattern.h"
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
  // sym_to_type maps a Sym to its "value-shape" CGv2Type —
  // what to put on a Var/Value carrying that Sym's type.
  // For records this is a CG2T_PTR (pyc's by-pointer
  // convention); the underlying struct lives in
  // sym_to_struct.
  Map<Sym *, CGv2Type *> sym_to_type;
  Map<Sym *, CGv2Type *> sym_to_struct;   // CG2T_STRUCT underlying
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
  Map<Label *, CGv2Block *> label_to_block;  // for branch targets
  Map<Var *, CGv2Value *> var_to_value;
  Map<CGv2Block *, PNode *> block_closer;   // for phi/phy edges
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

// Get-or-make the CG2T_STRUCT entry for a Type_RECORD Sym.
// Cached in sym_to_struct (separate from sym_to_type which
// carries the ptr wrapper, since pyc holds records by pointer).
CGv2Type *build_struct_type(NormCtx &c, Sym *s) {
  if (CGv2Type *cached = c.sym_to_struct.get(s)) return cached;
  CGv2Type *t = new CGv2Type();
  t->id = 1000 + c.p->types.n;
  // Multiple list / tuple Syms can share the same `name`
  // (`"list"`, `"tuple"`) but have different field counts —
  // pyc specializes a fresh struct per specialization. The
  // LLVM emit's named-struct cache (`getTypeByName`) keys on
  // the name only, so two such CGv2Types of different shapes
  // collide and the second's body silently inherits the
  // first's. Append the Sym id to disambiguate (F.1.1).
  cchar *raw_name = s->name ? s->name :
                    (s->cg_string ? s->cg_string : "anon");
  char buf[160];
  snprintf(buf, sizeof(buf), "%s.%d", raw_name, s->id);
  t->name = dupstr(buf);
  // `@vector("s")` classes (e.g. bytearray) have a struct prefix
  // followed by a trailing flexible array of `s->element`; this is
  // its own type-lattice kind so CG2_INDEX_LOAD/STORE/SIZEOF_ELEMENT
  // can dispatch on it instead of a side-channel bool.
  t->kind = s->is_vector ? CG2T_VECTOR : CG2T_STRUCT;
  // pyc's default for RECORD is heap (GC_malloc).  @pyc_struct
  // (issue 015) sets Sym::is_value_type=1; lift that bit into
  // the CGv2Type so the CG2_ALLOC emit can route to alloca
  // (issue 023).  Vectors stay heap-allocated regardless —
  // their trailing-array layout has no upper size bound.
  t->is_heap_aggregate = s->is_vector ? true : !s->is_value_type;
  c.sym_to_struct.put(s, t);          // register first (recursion guard)
  c.p->types.add(t);
  int idx = 0;
  for (Sym *f : s->has) {
    CGv2TypeField *cf = new CGv2TypeField();
    cf->name = f->name;
    CGv2Type *ft = build_type(c, f->type);
    // Substitute t_ptr for unsized field types (void / nested
    // structs that can't be sized at this point). pyc records
    // often contain method-slot fields whose IF1 type is
    // sym_void; without the substitution the containing
    // struct becomes unsized and LLVM rejects GEPs through it.
    // Phase B.10.6.
    if (!ft || ft->kind == CG2T_VOID) ft = c.p->t_ptr;
    cf->type = ft;
    cf->idx = idx++;
    t->fields.add(cf);
  }
  if (s->element) t->element = build_type(c, s->element->type);
  return t;
}

// Get-or-make a typed-ptr CGv2Type for char* (string), so
// emit_prim_write's "%s" path matches and CG2_LEN dispatches
// to _CG_string_len. Cached by name on prog->types so all
// string-family Syms share one entry.
CGv2Type *get_string_type(NormCtx &c) {
  if (CGv2Type *cached = c.p->lookup_type("string")) return cached;
  CGv2Type *t = new CGv2Type();
  t->id = 1000 + c.p->types.n;
  t->name = "string";
  t->kind = CG2T_PTR;
  t->element = c.p->t_int8;
  c.p->types.add(t);
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
  // pyc string family: sym_string and its specializers all
  // become a typed char-ptr so emit_prim_write/len take the
  // string path. Phase B.10.3.
  if (s == sym_string ||
      (sym_string && sym_string->specializers.set_in(s))) {
    CGv2Type *t = get_string_type(c);
    c.sym_to_type.put(s, t);
    return t;
  }
  if (s->type_kind == Type_RECORD) {
    // pyc holds records by pointer (matches v1 build_cgtype:
    // CG_T_PTR for s->has.n > 0). Wrap the underlying struct
    // in a CG2T_PTR. Field ops follow ptr->element to find
    // the struct shape; to_llvm_type for CG2T_PTR returns
    // opaque ptr so LLVM values are correctly pointer-shaped.
    CGv2Type *struct_t = build_struct_type(c, s);
    CGv2Type *ptr_t = new CGv2Type();
    ptr_t->id = 1000 + c.p->types.n;
    char buf[128];
    snprintf(buf, sizeof(buf), "ptr_%s",
             struct_t->name ? struct_t->name : "anon");
    ptr_t->name = dupstr(buf);
    ptr_t->kind = CG2T_PTR;
    ptr_t->element = struct_t;
    c.sym_to_type.put(s, ptr_t);
    c.p->types.add(ptr_t);
    return ptr_t;
  }
  // Closure type — Type_FUN with no resolved fun and a non-empty
  // `has[]` (codegen_common.cc's is_closure_var test).  pyc's
  // closure layout is `{e0 = fn_ptr, e1 = bound_obj, e2..eN =
  // captured_locals}`.  Build it as a CG2T_PTR wrapping a
  // CG2T_STRUCT so lower_send_period can ALLOC/FIELD_STORE and
  // lower_send_call can FIELD_LOAD the unpack at call sites.
  // v1's equivalent: the `_CG_s<NNN>` struct + `_CG_prim_closure`
  // macro (cg.cc:177).
  if (s->type_kind == Type_FUN && !s->fun && s->has.n) {
    CGv2Type *struct_t = new CGv2Type();
    struct_t->id = 1000 + c.p->types.n;
    char sbuf[160];
    cchar *raw_name = s->name ? s->name :
                      (s->cg_string ? s->cg_string : "closure");
    snprintf(sbuf, sizeof(sbuf), "closure_%s.%d", raw_name, s->id);
    struct_t->name = dupstr(sbuf);
    struct_t->kind = CG2T_STRUCT;
    struct_t->is_heap_aggregate = true;
    c.p->types.add(struct_t);
    int idx = 0;
    for (Sym *f : s->has) {
      CGv2TypeField *cf = new CGv2TypeField();
      cf->name = f->name;
      CGv2Type *ft = build_type(c, f->type);
      // Function-pointer fields (the selector slot) collapse to
      // opaque ptr — there's no useful LLVM-typed signature
      // because the FA already resolves call targets statically.
      if (!ft || ft->kind == CG2T_VOID) ft = c.p->t_ptr;
      cf->type = ft;
      cf->idx = idx++;
      struct_t->fields.add(cf);
    }
    CGv2Type *ptr_t = new CGv2Type();
    ptr_t->id = 1000 + c.p->types.n;
    char pbuf[160];
    snprintf(pbuf, sizeof(pbuf), "ptr_%s", struct_t->name);
    ptr_t->name = dupstr(pbuf);
    ptr_t->kind = CG2T_PTR;
    ptr_t->element = struct_t;
    c.sym_to_type.put(s, ptr_t);
    c.p->types.add(ptr_t);
    return ptr_t;
  }
  // Type_FUN (function pointer), Type_REF, Type_PRIMITIVE →
  // opaque ptr.  Refined when their tests land.
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
  } else if (v->sym->is_fun && v->sym->fun) {
    // Function reference — track the IF1 Fun so emit-time can
    // resolve to the materialized llvm::Function (via the
    // CGv2Fun name lookup in the emit's global function map).
    // Without this, the global ends up as a generic ptr-typed
    // module variable storing null, and stores of "the lambda
    // function" (class-default method-slot init from the
    // ___init___ pyc synthesizes) write null instead of the
    // real function pointer.  F.4.8 lambda_closure fix.
    cv->scope = CG2V_FUN_REF;
    cv->target_name = v->sym->fun->cg_string;
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

  // Issue 023 Stage 2: sret-rewrite when the logical return is
  // a value-type RECORD.  Detection is structural on the
  // CGv2Type: CG2T_PTR whose element is a CG2T_STRUCT with
  // `is_heap_aggregate==false` (set by build_struct_type when
  // the underlying IF1 Sym had `is_value_type=1`).  Skip the
  // rewrite for main (it's called by the LLVM-level main wrapper
  // which expects the existing void/int return) and for varargs
  // (LLVM's sret + varargs interaction has caller-side rules
  // that aren't worth the complexity for now).
  CGv2Type *rt = cf->signature->ret;
  if (rt && rt->kind == CG2T_PTR && rt->element &&
      rt->element->kind == CG2T_STRUCT &&
      !rt->element->is_heap_aggregate &&
      !cf->is_main && !cf->is_varargs) {
    cf->signature->is_sret = true;
    cf->signature->sret_struct = rt->element;
  }

  // Args come from a map keyed by MPosition. Iterating
  // `f->args.get_values()` directly returns them in map
  // insertion order, which does NOT match the positional
  // ordering that `lower_send_call`'s MPosition routing uses
  // — when those two diverge, the LLVM signature's formals
  // end up in a different order than the rvals the caller
  // passes, and CG2_CALL emit inserts ptrtoint/inttoptr
  // coercions that look like type fixes but are actually
  // bridging a positional swap (issue 021).
  //
  // Mirror v1's `build_arg_list` (llvm_codegen.cc:107):
  // walk positions 1..N explicitly via `cannonicalize_mposition`
  // and look up the formal Var at each position. This keeps
  // signature ordering and call-site routing locked together.
  FunCtx *fc = new FunCtx();
  fc->cf = cf;

  MPosition p;
  p.push(1);
  for (int i = 0; i < f->sym->has.n; i++) {
    MPosition *cp = cannonicalize_mposition(p);
    p.inc();
    Var *a = f->args.get(cp);
    if (!a) continue;
    // Mirror v1's createFunction filter (llvm_codegen.cc:98).
    // Skip args that DCE killed (not in any live path) AND
    // function-typed formals (closures — dispatched at call
    // sites, not passed as direct parameters). Without these
    // filters the v2 LLVM signature has args v1's main
    // wrapper isn't passing, producing verifyModule
    // arg-count mismatches. Phase B.10.2.
    if (!a->live) continue;
    if (a->type && a->type->is_fun) continue;
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
        // Register the LABEL's first Label* so GOTO/IF
        // targets resolve via the same map v1 uses
        // (llvm_codegen.cc:getLLVMBasicBlock).
        if (succ->code->label[0]) {
          fc.label_to_block.put(succ->code->label[0], b);
        }
        if (succ->code->label[1]) {
          fc.label_to_block.put(succ->code->label[1], b);
        }
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
  // Closure construction: when `a.x` reads a function-typed
  // field, materialize a `{selector, bound_self, ...captured}`
  // struct.  Mirrors v1's `_CG_prim_closure` (cg.cc:177) which
  // allocates the closure struct and stores e0=selector,
  // e1=bound_obj.  Triggered when:
  //   - lvals[0] is Type_FUN (a closure-typed result, including
  //     plain method bindings)
  //   - pn->creates is non-empty (FA marked this SEND as
  //     allocating; absent for non-closure bindings)
  //
  // This replaces the earlier MOVE-bound-obj-into-dst shortcut
  // (F.4.8), which only worked when (a) the lambda had a single
  // resolved target and (b) there were no captured locals.  The
  // FIELD_LOAD-driven dispatch in lower_send_call (below)
  // unpacks each formal from its closure field.
  Var *lv = pn->lvals[0];
  if (lv && lv->type && lv->type->type_kind == Type_FUN &&
      pn->creates && pn->creates->n &&
      lv->type->has.n >= 2) {
    CGv2Type *closure_ptr = build_type(c, lv->type);
    CGv2Type *closure_struct = closure_ptr ? closure_ptr->element
                                            : nullptr;
    if (closure_struct &&
        closure_struct->kind == CG2T_STRUCT &&
        closure_struct->fields.n >= 2) {
      // Alloc the closure struct.
      CGv2Inst *alloc = new CGv2Inst();
      alloc->op = CG2_ALLOC;
      alloc->type_arg = closure_struct;
      alloc->lvals.add(dst);
      blk->body.add(alloc);
      // e0 = selector (the method/symbol var at rvals[3]).
      // FIELD_LOAD/STORE coercion at emit (CG2_CAST in v2's
      // CG2_C_CALL arg path) accepts ptr-or-int sources.
      if (pn->rvals.n > 3) {
        CGv2Value *sel = build_var(c, fc, pn->rvals[3]);
        if (sel) {
          CGv2Inst *st = new CGv2Inst();
          st->op = CG2_FIELD_STORE;
          st->field_idx = 0;
          st->type_arg = closure_struct;
          st->rvals.add(dst);
          st->rvals.add(sel);
          blk->body.add(st);
        }
      }
      // e1 = bound self.
      CGv2Inst *st = new CGv2Inst();
      st->op = CG2_FIELD_STORE;
      st->field_idx = 1;
      st->type_arg = closure_struct;
      st->rvals.add(dst);
      st->rvals.add(obj);
      blk->body.add(st);
      return true;
    }
  }
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_FIELD_LOAD;
  inst->field_idx = fi;
  // :struct hint = obj->type->element when obj is a ptr-to-
  // struct (the pyc record convention). Fall back to obj->type
  // for cases where the value already carries the struct type.
  inst->type_arg = (obj->type && obj->type->kind == CG2T_PTR &&
                     obj->type->element)
                        ? obj->type->element
                        : obj->type;
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
  // :struct hint = obj->type->element when obj is ptr-to-struct.
  store->type_arg = (obj->type && obj->type->kind == CG2T_PTR &&
                      obj->type->element)
                         ? obj->type->element
                         : obj->type;
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

// Unwrap a CGv2Type to its underlying CG2T_STRUCT when it's a
// CG2T_PTR wrapper. Used by ALLOC/CLONE/SIZEOF to get the
// actual struct's byte size, not the ptr's 8 bytes. B.10.6.
CGv2Type *unwrap_struct(CGv2Type *t) {
  if (t && t->kind == CG2T_PTR && t->element) return t->element;
  return t;
}

// Forward decl — definition at the bottom of the prim-handler
// block; needed by lower_send_index_{load,store} above it.
int compute_prim_arg_offset(PNode *pn);

// Mint an int32 constant CGv2Value. Used by E.2/E.3 to thread
// element-size + count constants into CG2_C_CALL invocations
// of `_CG_prim_tuple_list_internal` / `_CG_to_list_runtime`,
// which both take (uint32, uint32) signatures.
static CGv2Value *make_uint32_const(NormCtx &c, FunCtx &fc,
                                     int64_t v, cchar *name) {
  CGv2Value *cv = new CGv2Value();
  cv->id = 8000 + fc.cf->id * 1000 + (int)fc.cf->locals.n;
  cv->name = name;
  cv->type = c.p->t_uint32;
  cv->scope = CG2V_CONSTANT;
  cv->imm.kind = CGv2Immediate::I_INT;
  cv->imm.v.i = v;
  return cv;
}

// Discriminate a `make` SEND's target sym as a list/tuple/vector
// literal — same check `cg.cc:135-144` uses. The discriminator
// is whether `rvals[2]->sym` is in `sym_tuple` / `sym_list`'s
// specializers set, or marked `is_vector`. Returns false for
// regular class instantiations (which keep the existing bare
// CG2_ALLOC path).
static bool is_list_or_tuple_make_target(Sym *target) {
  if (!target) return false;
  if (sym_tuple && sym_tuple->specializers.set_in(target)) return true;
  if (sym_list && sym_list->specializers.set_in(target)) return true;
  if (target->is_vector) return true;
  return false;
}

// P_prim_new / P_prim_make — heap allocation. The result type
// comes from lvals[0]->type (the result-Var carries the type
// it will hold post-construction).
//
// For P_prim_make on tuple/list literals (`[1, 2, 3]`,
// `(a, b)`), rvals[3..] carry the element values that
// initialize fields e0, e1, e2 ... of the allocated struct.
// The C backend emits these as `t1->e0 = v1; t1->e1 = v2; ...`
// (`cg.cc:142`); v2 LLVM needs the same field-store sequence
// after the alloc, otherwise the freshly-allocated struct
// holds uninitialized memory.
//
// E.2 (issue 019): for the flat-array shape, the alloc itself
// now routes through pyc's `_CG_prim_tuple_list_internal`
// runtime helper so the 16-byte list header is carved out.
// Runtime helpers (`_CG_list_add`, `_CG_list_mult`,
// `_CG_prim_len`) read header.len/header.ptr at `dst - 16`;
// without the header they crashed (issue 019 ratchet table).
// The struct-shape path still uses bare CG2_ALLOC for now;
// E.3 lands the two-stage construction that gives the struct
// path the same runtime contract.
bool lower_send_alloc(NormCtx &c, FunCtx &fc, PNode *pn,
                      CGv2Block *blk) {
  if (pn->lvals.n < 1) return false;
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!dst || !dst->type) return false;
  CGv2Type *struct_t = unwrap_struct(dst->type);

  // E.2: route the flat-array list-literal shape through the
  // runtime helper. Conditions:
  //   - this is a `make` SEND (not `new` / `clone` etc.)
  //   - `rvals[2]->sym` is a list/tuple/vector spec
  //   - `unwrap_struct(dst->type)` is NOT a CG2T_STRUCT
  //     (i.e. the FA's typing keeps it as an opaque/flat ptr;
  //     struct-shape literals stay on the bare-alloc path for
  //     E.2 and pick up the two-stage construction in E.3)
  //
  // F.1.1: empty literal `[]` (rvals.n == 3) needs the runtime
  // header too — `_CG_list_add(empty_list, ...)` reads len/ptr
  // at `dst - 16` and crashes without it. Take the flat path
  // unconditionally for any list/tuple `make` whose dst isn't
  // a struct, defaulting the element type to int64 when
  // there's no first element to infer from.
  bool routed_flat = false;
  if (pn->prim && pn->prim->index == P_prim_make &&
      pn->rvals.n >= 3 &&
      pn->rvals[2] && pn->rvals[2]->sym &&
      is_list_or_tuple_make_target(pn->rvals[2]->sym) &&
      !(struct_t && struct_t->kind == CG2T_STRUCT)) {
    // Discover the element type from the first value's
    // CGv2Type. For `[1, 2, 3]` the FA tags rvals[3] as
    // int64, which is exactly the stride
    // `_CG_prim_tuple_list_internal` needs. For empty literals
    // (F.1.1) there's no first value; default to int64 so the
    // header carving still happens.
    CGv2Type *elem_type = nullptr;
    if (pn->rvals.n > 3) {
      CGv2Value *fv = build_var(c, fc, pn->rvals[3]);
      if (fv && fv->type) elem_type = fv->type;
    }
    if (!elem_type) elem_type = c.p->t_int64;

    if (elem_type) {
      // CG2_SIZEOF — compute element_size at LLVM-emit time
      // from the CGv2Type. Stays as i64 by emit convention;
      // CG2_C_CALL's arg coercion truncates to i32 at the
      // call site.
      CGv2Value *size_v = new CGv2Value();
      size_v->id = 9000 + fc.cf->id * 1000 + (int)fc.cf->locals.n;
      size_v->name = "elem_sz";
      size_v->type = c.p->t_int64;
      size_v->scope = CG2V_LOCAL;
      fc.cf->locals.add(size_v);
      CGv2Inst *sz_inst = new CGv2Inst();
      sz_inst->op = CG2_SIZEOF;
      sz_inst->type_arg = elem_type;
      sz_inst->lvals.add(size_v);
      blk->body.add(sz_inst);

      // CG2_C_CALL `_CG_prim_tuple_list_internal(size, n)`.
      // n is the element count (rvals.n - 3). Args are passed
      // as uint32 to match the runtime helper's signature.
      CGv2Value *n_v = make_uint32_const(c, fc,
                                          pn->rvals.n - 3, "cnt");
      CGv2Inst *call_inst = new CGv2Inst();
      call_inst->op = CG2_C_CALL;
      call_inst->prim_name = "_CG_prim_tuple_list_internal";
      call_inst->type_arg = dst->type;
      // Mark size_v as uint32 too so the call signature
      // matches; the emit's CG2_C_CALL coercion handles the
      // int64 → uint32 trunc.
      size_v->type = c.p->t_uint32;
      call_inst->rvals.add(size_v);
      call_inst->rvals.add(n_v);
      call_inst->lvals.add(dst);
      blk->body.add(call_inst);
      routed_flat = true;
    }
  }

  // E.3 (issue 019): for the struct-shape list-literal,
  // route through the two-stage construction the C backend
  // already uses (`cg.cc:140` for stage 1,
  // `pyc_c_runtime.h:_CG_TUPLE_TO_LIST_FUN` macro for stage 2).
  // Stage 1 over-allocates with `n = element_count + 1` (the
  // pyc +1 convention) into an intermediate temp; the FIELD_STOREs
  // below write the elements into that temp; stage 2 calls
  // `_CG_to_list_runtime` to produce the runtime-contract-shaped
  // list (16-byte header + `header.len = element_count`) into
  // dst. dst's CGv2Type is NOT touched — downstream
  // `CG2_FIELD_LOAD` / `CG2_SIZEOF_ELEMENT` / etc. see the same
  // type they always did (the June 2026 reverted attempt's
  // mistake was rewriting it).
  CGv2Value *struct_init_target = dst;  // FIELD_STORE writes here
  bool routed_struct = false;
  if (!routed_flat && pn->prim && pn->prim->index == P_prim_make &&
      pn->rvals.n >= 3 &&
      pn->rvals[2] && pn->rvals[2]->sym &&
      is_list_or_tuple_make_target(pn->rvals[2]->sym) &&
      struct_t && struct_t->kind == CG2T_STRUCT) {
    // Stage 1: SIZEOF(struct_t) → size_v.
    CGv2Value *size_v = new CGv2Value();
    size_v->id = 9500 + fc.cf->id * 1000 + (int)fc.cf->locals.n;
    size_v->name = "list_struct_sz";
    size_v->type = c.p->t_int64;
    size_v->scope = CG2V_LOCAL;
    fc.cf->locals.add(size_v);
    CGv2Inst *sz_inst = new CGv2Inst();
    sz_inst->op = CG2_SIZEOF;
    sz_inst->type_arg = struct_t;
    sz_inst->lvals.add(size_v);
    blk->body.add(sz_inst);

    // CG2_C_CALL — _CG_prim_tuple_list_internal(sizeof(struct),
    //                                            element_count+1)
    // → tmp. The +1 mirrors cg.cc:141's `rvals.n - 2` count.
    // tmp has the same CGv2Type as dst; the field stores
    // below GEP into it as a struct.
    size_v->type = c.p->t_uint32;
    CGv2Value *n_alloc = make_uint32_const(c, fc,
                                            pn->rvals.n - 2,
                                            "alloc_cnt");
    CGv2Value *tmp = new CGv2Value();
    tmp->id = 9700 + fc.cf->id * 1000 + (int)fc.cf->locals.n;
    tmp->name = "list_tmp";
    tmp->type = dst->type;
    tmp->scope = CG2V_LOCAL;
    fc.cf->locals.add(tmp);
    CGv2Inst *alloc_inst = new CGv2Inst();
    alloc_inst->op = CG2_C_CALL;
    alloc_inst->prim_name = "_CG_prim_tuple_list_internal";
    alloc_inst->type_arg = dst->type;
    alloc_inst->rvals.add(size_v);
    alloc_inst->rvals.add(n_alloc);
    alloc_inst->lvals.add(tmp);
    blk->body.add(alloc_inst);

    // FIELD_STORE writes below target the temp, not dst.
    struct_init_target = tmp;

    // Stage 2 (deferred): the conversion CG2_C_CALL into dst
    // lands after the FIELD_STORE block below. Stash the
    // pieces we need so the post-loop code can emit it.
    routed_struct = true;
  }

  if (!routed_flat && !routed_struct) {
    CGv2Inst *inst = new CGv2Inst();
    inst->op = CG2_ALLOC;
    inst->type_arg = struct_t;
    inst->lvals.add(dst);
    blk->body.add(inst);
  }

  // Per-element initializers for tuple/list literals. pyc's C
  // backend has two shapes (`cg.cc:140`-onwards):
  //
  //   - Tuple-list / multi-element list of mixed types →
  //     `_CG_prim_tuple_list` returning a struct whose fields
  //     are `e0`, `e1`, ... and named-field stores
  //     `t->e0 = v1; t->e1 = v2; ...`. CG2_FIELD_STORE matches.
  //
  //   - Flat list (homogeneous element type, including the
  //     single-element-literal case `[x]`) → `_CG_prim_list`
  //     returning a typed pointer, written via array indexing
  //     `((T*)t)[0] = v1; ((T*)t)[1] = v2; ...`.
  //     CG2_INDEX_STORE matches.
  //
  // The discriminator is whether `unwrap_struct(dst->type)`
  // gave us a CG2T_STRUCT: if yes, we have the tuple shape; if
  // no, we have a flat array and route through INDEX_STORE.
  // Without the flat-array arm, single-element `[1]` was
  // allocated but never initialized — `list_concat.py` segv'd
  // when `_CG_list_add` read garbage from it.
  if (pn->prim && pn->prim->index == P_prim_make) {
    if (struct_t && struct_t->kind == CG2T_STRUCT) {
      int n_fields = struct_t->fields.n;
      for (int i = 3; i < pn->rvals.n; i++) {
        int field_idx = i - 3;
        if (field_idx >= n_fields) break;       // over-alloc tail
        Var *val_var = pn->rvals[i];
        if (!val_var) continue;
        CGv2Value *val = build_var(c, fc, val_var);
        if (!val) continue;
        CGv2Inst *st = new CGv2Inst();
        st->op = CG2_FIELD_STORE;
        st->field_idx = field_idx;
        st->type_arg = struct_t;
        st->rvals.add(struct_init_target);
        st->rvals.add(val);
        blk->body.add(st);
      }
    } else {
      // Flat-array shape. INDEX_STORE walks a typed-ptr stride
      // (see CG2_INDEX_STORE emit), so the dst's ptr type
      // drives the addressing.
      for (int i = 3; i < pn->rvals.n; i++) {
        Var *val_var = pn->rvals[i];
        if (!val_var) continue;
        CGv2Value *val = build_var(c, fc, val_var);
        if (!val) continue;
        // Build a constant index value at i-3.
        CGv2Value *idx = new CGv2Value();
        idx->id = 7000 + fc.cf->id * 100 + (i - 3);
        idx->name = "litidx";
        idx->type = c.p->t_int64;
        idx->scope = CG2V_CONSTANT;
        idx->imm.kind = CGv2Immediate::I_INT;
        idx->imm.v.i = i - 3;
        CGv2Inst *st = new CGv2Inst();
        st->op = CG2_INDEX_STORE;
        // type_arg overrides the GEP element type when the
        // dst's CGv2Type is opaque (`t_ptr` for FA-unknown
        // ptrs — single-element list literals end up here).
        st->type_arg = val->type;
        st->rvals.add(dst);
        st->rvals.add(idx);
        st->rvals.add(val);
        blk->body.add(st);
      }
    }
  }

  // E.3 stage 2 — `_CG_to_list_runtime(struct_init_target,
  //                                     sizeof(struct), semantic_n)`
  // → dst. Runs after the field stores so the runtime sees
  // a fully populated struct. Recomputes SIZEOF locally
  // because the stage-1 size_v has already been retyped
  // (uint32 for the alloc call's signature) and reusing it
  // here would force another coercion.
  if (routed_struct) {
    CGv2Value *size2 = new CGv2Value();
    size2->id = 9800 + fc.cf->id * 1000 + (int)fc.cf->locals.n;
    size2->name = "list_struct_sz2";
    size2->type = c.p->t_uint32;
    size2->scope = CG2V_LOCAL;
    fc.cf->locals.add(size2);
    CGv2Inst *sz2_inst = new CGv2Inst();
    sz2_inst->op = CG2_SIZEOF;
    sz2_inst->type_arg = struct_t;
    sz2_inst->lvals.add(size2);
    blk->body.add(sz2_inst);

    CGv2Value *n_semantic = make_uint32_const(c, fc,
                                                pn->rvals.n - 3,
                                                "sem_cnt");
    CGv2Inst *conv_inst = new CGv2Inst();
    conv_inst->op = CG2_C_CALL;
    conv_inst->prim_name = "_CG_to_list_runtime";
    conv_inst->type_arg = dst->type;
    conv_inst->rvals.add(struct_init_target);
    conv_inst->rvals.add(size2);
    conv_inst->rvals.add(n_semantic);
    conv_inst->lvals.add(dst);
    blk->body.add(conv_inst);
  }

  return true;
}

// P_prim_clone / P_prim_clone_vector — allocate-and-copy from
// prototype.  The IF1 SEND from gen_class_pyda
// (python_ifa_build_syms.cc:614 / :621) is
// `(sym_primitive, sym_clone, proto)` → t for plain clone, or
// `(sym_primitive, sym_clone_vector, proto, vec_size)` → t for
// vector clone (`@vector("s")` classes like bytearray).
// `compute_prim_arg_offset` returns 2 when rvals[0] is
// sym_primitive, so proto is at rvals[o] and vec_size at
// rvals[o+1].  Reading rvals[1] would pick up the sym_clone
// marker, whose global is uninitialized — exactly the segfault
// the pre-F.3 comment warned about.
//
// F.4.5: clone_vector routes through the
// `_CG_prim_clone_vector_runtime(proto, sizeof_struct,
// vec_extra)` helper so `@vector("s")` instances get
// `sizeof(struct) + vec_extra` bytes allocated with the trailing
// data area zero-init'd.  Matches v1's `_CG_prim_clone_vector`
// macro semantics: `vec_extra` is the value passed by the
// frontend, which for bytearray equals the byte count (uint8
// element).
bool lower_send_clone(NormCtx &c, FunCtx &fc, PNode *pn,
                      CGv2Block *blk) {
  if (pn->lvals.n < 1) return false;
  int o = compute_prim_arg_offset(pn);
  if (pn->rvals.n <= o) return false;
  CGv2Value *proto = build_var(c, fc, pn->rvals[o]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!proto || !dst || !dst->type) return false;
  CGv2Type *struct_t = unwrap_struct(dst->type);
  if (!struct_t ||
      (struct_t->kind != CG2T_STRUCT &&
       struct_t->kind != CG2T_VECTOR)) return false;

  bool is_vec = pn->prim &&
                pn->prim->index == P_prim_clone_vector;
  if (is_vec && pn->rvals.n > o + 1) {
    CGv2Value *vec_extra = build_var(c, fc, pn->rvals[o + 1]);
    if (!vec_extra) return false;

    // Materialize sizeof(struct) as a fresh local from
    // CG2_SIZEOF so the call site has a real value.
    CGv2Value *sz = new CGv2Value();
    sz->id = 9300 + fc.cf->id * 1000 + (int)fc.cf->locals.n;
    sz->name = "vec_sz";
    sz->type = c.p->t_int64;
    sz->scope = CG2V_LOCAL;
    fc.cf->locals.add(sz);
    CGv2Inst *sz_inst = new CGv2Inst();
    sz_inst->op = CG2_SIZEOF;
    sz_inst->type_arg = struct_t;
    sz_inst->lvals.add(sz);
    blk->body.add(sz_inst);

    CGv2Inst *call = new CGv2Inst();
    call->op = CG2_C_CALL;
    call->prim_name = "_CG_prim_clone_vector_runtime";
    call->type_arg = dst->type;
    call->rvals.add(proto);
    call->rvals.add(sz);
    call->rvals.add(vec_extra);
    call->lvals.add(dst);
    blk->body.add(call);
    return true;
  }

  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_CLONE;
  inst->type_arg = struct_t;
  inst->rvals.add(proto);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_index_object — vector / list element load.
//   rvals[o] = obj, rvals[o+1] = index, where o accounts for
//   the optional `primitive` marker at rvals[0] (set when the
//   SEND came from __pyc_primitive__).
bool lower_send_index_load(NormCtx &c, FunCtx &fc, PNode *pn,
                            CGv2Block *blk) {
  int o = compute_prim_arg_offset(pn);
  if (pn->rvals.n < o + 2 || pn->lvals.n < 1) return false;
  CGv2Value *obj = build_var(c, fc, pn->rvals[o]);
  CGv2Value *idx = build_var(c, fc, pn->rvals[o + 1]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!obj || !idx || !dst) return false;
  // String indexing: `s[i]` returns a 1-char string, not a
  // raw byte.  v1 dispatches to `_CG_char_from_string(s, i)`
  // (cg.cc:271 — guarded by `sym_string->specializers.set_in(t)`).
  // Without this, v2 emits a raw i8 load and hands it to
  // printf("%s", ...) — segfault.
  Sym *obj_ty = pn->rvals[o]->type;
  if (obj_ty &&
      (obj_ty == sym_string ||
       (sym_string && sym_string->specializers.set_in(obj_ty)))) {
    CGv2Inst *call = new CGv2Inst();
    call->op = CG2_C_CALL;
    call->prim_name = "_CG_char_from_string";
    call->type_arg = dst->type ? dst->type : get_string_type(c);
    call->rvals.add(obj);
    call->rvals.add(idx);
    call->lvals.add(dst);
    blk->body.add(call);
    return true;
  }
  // Heterogeneous-tuple indexing with a constant index:
  // `t[N]` where t is a Type_RECORD-typed object goes through
  // CG2_FIELD_LOAD so the per-field type (not first-field
  // stride) drives the GEP.  v1 emits `obj->eN` for this case
  // (cg.cc:289).
  Var *idx_var = pn->rvals[o + 1];
  if (obj_ty && obj_ty->type_kind == Type_RECORD && obj_ty->has.n &&
      idx_var && idx_var->sym && idx_var->sym->constant) {
    int fi = (int)strtol(idx_var->sym->constant, nullptr, 10);
    if (fi >= 0 && fi < obj_ty->has.n) {
      CGv2Type *struct_t = build_struct_type(c, obj_ty);
      CGv2Inst *fl = new CGv2Inst();
      fl->op = CG2_FIELD_LOAD;
      fl->field_idx = fi;
      fl->type_arg = struct_t;
      fl->rvals.add(obj);
      fl->lvals.add(dst);
      blk->body.add(fl);
      return true;
    }
  }
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_INDEX_LOAD;
  inst->rvals.add(obj);
  inst->rvals.add(idx);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_set_index_object — vector / list element store.
//   rvals[o] = obj, rvals[o+1] = index, rvals[o+2] = value.
bool lower_send_index_store(NormCtx &c, FunCtx &fc, PNode *pn,
                             CGv2Block *blk) {
  int o = compute_prim_arg_offset(pn);
  if (pn->rvals.n < o + 3) return false;
  CGv2Value *obj = build_var(c, fc, pn->rvals[o]);
  CGv2Value *idx = build_var(c, fc, pn->rvals[o + 1]);
  CGv2Value *val = build_var(c, fc, pn->rvals[o + 2]);
  if (!obj || !idx || !val) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_INDEX_STORE;
  inst->rvals.add(obj);
  inst->rvals.add(idx);
  inst->rvals.add(val);
  blk->body.add(inst);
  return true;
}

// Compute the argument offset for prims that may be prefixed
// by an explicit `primitive` selector. Mirrors `cg.cc`'s
// `(n->rvals.v[0]->sym == sym_primitive) ? 2 : 1` — the
// pointer comparison is robust against frontend name choices
// (in pyc `sym_primitive->name` is "__primitive", not the
// "primitive" string v1's llvm_primitives.cc:425 happened to
// compare against). Without the right offset, structural
// lowerings like `lower_send_index_load` read self as the
// index and the prim sym as the obj.
int compute_prim_arg_offset(PNode *pn) {
  if (pn->rvals.n > 0 && pn->rvals[0] &&
      pn->rvals[0]->sym == sym_primitive) {
    return 2;
  }
  return 1;
}

// P_prim_sizeof — sizeof(T) where T is the type of rvals[o].
// Mirrors v1's llvm_primitives.cc:659.
bool lower_send_sizeof(NormCtx &c, FunCtx &fc, PNode *pn,
                       CGv2Block *blk) {
  int o = compute_prim_arg_offset(pn);
  if (o >= pn->rvals.n || pn->lvals.n < 1) return false;
  Var *type_var = pn->rvals[o];
  if (!type_var || !type_var->type) return false;
  CGv2Type *t = build_type(c, type_var->type);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!t || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_SIZEOF;
  inst->type_arg = t;
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_sizeof_element — sizeof(element type of rvals[o]).
// The v2 emit reads element off the rval ptr's CGv2Type;
// build_struct_type already populates Type::element from
// the Sym's element->type so the lookup resolves naturally.
bool lower_send_sizeof_element(NormCtx &c, FunCtx &fc, PNode *pn,
                                CGv2Block *blk) {
  int o = compute_prim_arg_offset(pn);
  if (o >= pn->rvals.n || pn->lvals.n < 1) return false;
  CGv2Value *src = build_var(c, fc, pn->rvals[o]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!src || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_SIZEOF_ELEMENT;
  inst->rvals.add(src);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_strcat — string `::` operator (`s = a + b` for str).
// The C backend expands to the `_CG_prim_strcat(a, "::", b)`
// macro which calls the `_CG_strcat` runtime helper. The v2
// LLVM backend has no inline emit for it, so without this
// handler the SEND was silently dropped (issue 018, "loop-after"
// symptom: post-loop string concats vanished and the return
// loaded an undef SSU rename of the accumulator).
//
// SEND rvals layout from the operator path:
//   rvals[0] = sym_operator marker
//   rvals[1] = lhs  (string)
//   rvals[2] = "::" operator symbol
//   rvals[3] = rhs  (string)
//   lvals[0] = result
// (Mirrors `llvm_primitives.cc:447`'s P_prim_add layout — `o=1`
// when rvals[0] is the operator marker, lhs at rvals[o],
// rhs at rvals[o+2].)
//
// Routes through CG2_C_CALL so libpyc_runtime.a's `_CG_strcat`
// satisfies the link (Phase D.3.5).
bool lower_send_strcat(NormCtx &c, FunCtx &fc, PNode *pn,
                        CGv2Block *blk) {
  int o = compute_prim_arg_offset(pn);
  if (pn->rvals.n < o + 3 || pn->lvals.n < 1) return false;
  CGv2Value *a = build_var(c, fc, pn->rvals[o]);
  CGv2Value *b = build_var(c, fc, pn->rvals[o + 2]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!a || !b || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_C_CALL;
  inst->prim_name = "_CG_strcat";
  inst->type_arg = dst->type ? dst->type : get_string_type(c);
  inst->rvals.add(a);
  inst->rvals.add(b);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_minus — unary `-x`.  IF1 layout from
// `__pyc_operator__("-", x)`:
//   rvals[0] = sym_operator marker
//   rvals[1] = "-" symbol
//   rvals[2] = operand
//   lvals[0] = result
// Emit as `0 - x` (CG2_BINOP SUB).  Without this, the SEND
// drops and `x = -x` becomes a no-op — bin(-1) then loops
// over the negative value's two-complement representation
// (all ones), producing a 64-digit binary string (F.4.7).
bool lower_send_neg(NormCtx &c, FunCtx &fc, PNode *pn,
                     CGv2Block *blk) {
  if (pn->rvals.n < 1 || pn->lvals.n < 1) return false;
  CGv2Value *src = build_var(c, fc, pn->rvals[pn->rvals.n - 1]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!src || !dst || !src->type) return false;
  CGv2Value *zero = new CGv2Value();
  zero->id = 9650 + fc.cf->id * 1000 + (int)c.p->constants.n;
  zero->name = "zero";
  zero->type = src->type;
  zero->scope = CG2V_CONSTANT;
  zero->imm.kind = (src->type->kind == CG2T_FLOAT)
                       ? CGv2Immediate::I_FLOAT
                       : CGv2Immediate::I_INT;
  if (src->type->kind == CG2T_FLOAT) zero->imm.v.f = 0.0;
  else zero->imm.v.i = 0;
  c.p->constants.add(zero);
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_BINOP;
  inst->sub_op = CG2B_SUB;
  inst->rvals.add(zero);
  inst->rvals.add(src);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_lnot — logical `not x`.  IF1 layout from pyc's
// `PY_bool_not` lowering (python_ifa_build_if1.cc:846) into
// `__pyc_operator__("!", x)`:
//   rvals[0] = sym_operator marker
//   rvals[1] = "!" symbol
//   rvals[2] = operand
//   lvals[0] = result (bool)
// Emit as `x == 0` (CG2_BINOP EQ against zero) so any numeric
// type works.  Without this, the SEND was being silently
// dropped (CG2_PRIM fall-through emits no LLVM IR) — every
// `if not x` in user code lost its condition, and the
// surrounding block's CFG fell off into `unreachable`.
bool lower_send_lnot(NormCtx &c, FunCtx &fc, PNode *pn,
                      CGv2Block *blk) {
  if (pn->rvals.n < 1 || pn->lvals.n < 1) return false;
  CGv2Value *src = build_var(c, fc, pn->rvals[pn->rvals.n - 1]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!src || !dst || !src->type) return false;
  // Build a zero constant of the same type as src.
  CGv2Value *zero = new CGv2Value();
  zero->id = 9600 + fc.cf->id * 1000 + (int)c.p->constants.n;
  zero->name = "zero";
  zero->type = src->type;
  zero->scope = CG2V_CONSTANT;
  zero->imm.kind = (src->type->kind == CG2T_FLOAT)
                       ? CGv2Immediate::I_FLOAT
                       : CGv2Immediate::I_INT;
  if (src->type->kind == CG2T_FLOAT) zero->imm.v.f = 0.0;
  else zero->imm.v.i = 0;
  c.p->constants.add(zero);
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_BINOP;
  inst->sub_op = CG2B_EQ;
  inst->rvals.add(src);
  inst->rvals.add(zero);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_coerce — `int(x)`, `float(x)`, etc.  v1 emits
// `_CG_prim_coerce(t, v)` which is a C-cast macro.  v2 emits
// CG2_CAST with the target type from `rvals[n-2]`'s Sym and
// source value from `rvals[n-1]`.  Mirrors the FA's transfer
// function at `fa.cc:1878`.
bool lower_send_coerce(NormCtx &c, FunCtx &fc, PNode *pn,
                        CGv2Block *blk) {
  if (pn->rvals.n < 2 || pn->lvals.n < 1) return false;
  Var *tgt_var = pn->rvals[pn->rvals.n - 2];
  Var *src_var = pn->rvals[pn->rvals.n - 1];
  if (!tgt_var || !tgt_var->sym || !src_var) return false;
  // The target Var holds the meta-type symbol; unwrap to its
  // concrete type before building the CGv2Type.
  Sym *tgt_sym = tgt_var->sym->is_meta_type
                     ? tgt_var->sym->meta_type
                     : tgt_var->sym;
  // Follow Type_ALIAS chains (pyc `int` is an alias for
  // sym_int64; `float` for sym_float64; etc.) — without this,
  // build_type falls into the Type_RECORD branch and returns
  // an opaque ptr instead of the i64/double the cast should
  // target.  Mirrors the FA's `unalias_type` call at
  // `analysis/fa.cc:1879`.
  tgt_sym = unalias_type(tgt_sym);
  if (!tgt_sym) return false;
  CGv2Type *dst_ty = build_type(c, tgt_sym);
  CGv2Value *src = build_var(c, fc, src_var);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!dst_ty || !src || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_CAST;
  inst->type_arg = dst_ty;
  inst->rvals.add(src);
  inst->lvals.add(dst);
  blk->body.add(inst);
  return true;
}

// P_prim_len — len(obj). v2 emit dispatches based on the
// obj's CGv2Type: string-like → strlen, other → constant 0.
//
// The IF1 SEND for len() can route the actual obj into
// different rvals positions depending on the call shape
// (direct len(s) vs @primitive @len s vs method dispatch).
// Walk the rvals from index 1 looking for the first one
// whose Sym suggests it's the real obj (i.e., type is
// sym_string-family rather than a function/method
// reference). Falls back to compute_prim_arg_offset's
// guess. Phase B.10.11.
bool lower_send_len(NormCtx &c, FunCtx &fc, PNode *pn,
                    CGv2Block *blk) {
  if (pn->lvals.n < 1) return false;
  // Scan for a string-typed rval first.
  Var *obj_var = nullptr;
  for (int i = 1; i < pn->rvals.n; i++) {
    Var *v = pn->rvals[i];
    if (!v || !v->type) continue;
    if (v->type == sym_string ||
        (sym_string && sym_string->specializers.set_in(v->type))) {
      obj_var = v;
      break;
    }
  }
  if (!obj_var) {
    int o = compute_prim_arg_offset(pn);
    if (o >= pn->rvals.n) return false;
    obj_var = pn->rvals[o];
  }
  CGv2Value *obj = build_var(c, fc, obj_var);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!obj || !dst) return false;
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_LEN;
  inst->rvals.add(obj);
  inst->lvals.add(dst);
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

  // MPosition-aware arg routing — mirror v1's write_send
  // (llvm_primitives.cc:281). Walk target->positional_arg_
  // positions in order; for each formal that survives the
  // live/non-fun filter, compute its rvals index via
  // Position2int(p->pos[0]) - 1.
  //
  // Closure dispatch: when rvals[0] is a closure (is_closure_var
  // — a Type_FUN sym with `has[]` populated), the lambda was
  // built with each formal mapped to a closure-struct field:
  // formal at IF1 position p reads closure->e_{p-1}.  Since v2
  // filters Type_FUN formals at the def site, the position-1
  // selector slot (e0) is never visited; subsequent positions
  // map directly: position 2 → e1, position 3 → e2, ...  So
  // `i = pos - 1` IS the closure field index for the unpack.
  // For formal positions past the closure's field count, the
  // extra args come from rvals (shifted by `has.n - 1` since
  // rvals[0] is the closure itself).
  //
  // Mirrors v1's write_send_arg (cg.cc:502 + write_c_fun_arg)
  // semantics minus the unused fn_ptr formal v2 already drops.
  Var *v0 = pn->rvals.n > 0 ? pn->rvals[0] : nullptr;
  int before = inst->rvals.n;
  for (MPosition *p : target->positional_arg_positions) {
    Var *formal_arg = target->args.get(p);
    if (!formal_arg || !formal_arg->live) continue;
    if (formal_arg->type && formal_arg->type->is_fun) continue;
    if (p->pos.n > 1) continue;     // skip nested tuple fields
    int i = (int)Position2int(p->pos[0]) - 1;
    if (v0 && is_closure_var(v0) && v0->type) {
      if (i < v0->type->has.n) {
        // Unpack: FIELD_LOAD(closure, i) → fresh local → arg.
        CGv2Value *closure = build_var(c, fc, v0);
        CGv2Type *cptr = closure ? closure->type : nullptr;
        CGv2Type *cst = (cptr && cptr->kind == CG2T_PTR)
                            ? cptr->element : nullptr;
        if (closure && cst && cst->kind == CG2T_STRUCT &&
            i >= 0 && i < cst->fields.n) {
          CGv2Value *arg = new CGv2Value();
          arg->id = 7000 + fc.cf->id * 1000 +
                    (int)fc.cf->locals.n;
          arg->name = "clo_arg";
          arg->type = cst->fields[i] ? cst->fields[i]->type
                                      : c.p->t_ptr;
          arg->scope = CG2V_LOCAL;
          fc.cf->locals.add(arg);
          CGv2Inst *fl = new CGv2Inst();
          fl->op = CG2_FIELD_LOAD;
          fl->field_idx = i;
          fl->type_arg = cst;
          fl->rvals.add(closure);
          fl->lvals.add(arg);
          blk->body.add(fl);
          inst->rvals.add(arg);
        }
        continue;
      } else {
        i -= v0->type->has.n - 1;
      }
    }
    if (i < 0 || i >= pn->rvals.n) continue;
    Var *actual = pn->rvals[i];
    if (!actual) continue;
    CGv2Value *cv = build_var(c, fc, actual);
    if (cv) inst->rvals.add(cv);
  }
  // Fallback to verbatim rvals[1..] when MPosition routing
  // produced nothing (target has no positional_arg_positions
  // populated, or every formal failed the filter). Better to
  // pass something than nothing — the defensive coercion in
  // v2 emit's CG2_CALL trims as needed.
  if (inst->rvals.n == before) {
    for (int i = 1; i < pn->rvals.n; i++) {
      CGv2Value *cv = build_var(c, fc, pn->rvals[i]);
      if (cv) inst->rvals.add(cv);
    }
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
// __pyc_c_call__(ret_type, "fn_name", arg_type, arg_value, ...)
// — pyc's generic FFI primitive. Lower to CG2_C_CALL. Mirrors
// v1's c_call_codegen (python_ifa_main.cc:56) at the IR level.
//
// SEND rval layout (set by python_ifa_build_if1.cc:301):
//   rvals[0] = sym_primitive marker
//   rvals[1] = __pyc_c_call__ sym (the prim name)
//   rvals[2] = ret type (FA-typed; meta_type)
//   rvals[3] = fn name (string constant)
//   rvals[4] = arg_type_1 (meta_type, ignored at emit)
//   rvals[5] = arg_value_1
//   rvals[6] = arg_type_2
//   rvals[7] = arg_value_2
//   ...
//
// The type meta-rvals exist for FA's benefit only — at codegen
// the actual LLVM arg type is read from each arg value's
// CGv2Type, mirroring CG2_C_CALL's emit contract. The return
// type comes from the lval's FA-inferred type (matching v1's
// transfer function); for void calls with no lval, we fall
// back to the meta_type chain off rvals[2].
//
// Phase D.3.
bool lower_send_c_call(NormCtx &c, FunCtx &fc, PNode *pn,
                        CGv2Block *blk) {
  if (pn->rvals.n < 4) return false;
  Var *name_var = pn->rvals[3];
  if (!name_var || !name_var->sym || !name_var->sym->constant) {
    return false;
  }
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_C_CALL;
  inst->prim_name = name_var->sym->constant;

  if (pn->lvals.n > 0 && pn->lvals[0]) {
    CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
    if (dst) {
      inst->type_arg = dst->type;
      inst->lvals.add(dst);
    }
  }
  if (!inst->type_arg) {
    Var *rt_var = pn->rvals[2];
    if (rt_var && rt_var->sym) {
      Sym *t = rt_var->sym->is_meta_type ? rt_var->sym->meta_type
                                          : rt_var->sym;
      inst->type_arg = build_type(c, t);
    }
  }
  if (!inst->type_arg) inst->type_arg = c.p->t_void;

  // Walk the (arg_type, arg_value) pairs and keep only values.
  for (int i = 5; i < pn->rvals.n; i += 2) {
    Var *v = pn->rvals[i];
    if (!v) continue;
    CGv2Value *cv = build_var(c, fc, v);
    if (cv) inst->rvals.add(cv);
  }
  blk->body.add(inst);
  return true;
}

// __pyc_format_string__ — pyc's `s % t` operator runtime.
// Mirrors v1's `format_string_codegen` (python_ifa_main.cc:80):
//
//   rvals[0] = sym_primitive marker
//   rvals[1] = __pyc_format_string__ sym (the prim name)
//   rvals[2] = format string
//   rvals[3] = value (tuple → unpack fields; scalar → pass through)
//   lvals[0] = result string
//
// Route to `_CG_format_string(fmt, ...)` via CG2_C_CALL,
// unpacking tuple fields into separate varargs.  Without this
// the SEND was being dropped (CG2_PRIM fall-through emits no
// LLVM IR), so `a = "foo %d" % (3,4)` left @a null and the
// program printed "(null)".  F.4 string_format fix.
bool lower_send_format_string(NormCtx &c, FunCtx &fc, PNode *pn,
                               CGv2Block *blk) {
  if (pn->rvals.n < 4 || pn->lvals.n < 1) return false;
  CGv2Value *fmt = build_var(c, fc, pn->rvals[2]);
  CGv2Value *val = build_var(c, fc, pn->rvals[3]);
  CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
  if (!fmt || !val || !dst) return false;
  CGv2Inst *call = new CGv2Inst();
  call->op = CG2_C_CALL;
  call->prim_name = "_CG_format_string";
  call->type_arg = dst->type ? dst->type : get_string_type(c);
  call->rvals.add(fmt);
  // If val is a tuple/struct, unpack each field via
  // CG2_FIELD_LOAD into a fresh local and append those locals
  // to the call's rvals.  Otherwise pass val directly.
  Sym *val_ty = pn->rvals[3]->type;
  if (val_ty && val_ty->type_kind == Type_RECORD && val_ty->has.n) {
    CGv2Type *struct_t = build_struct_type(c, val_ty);
    for (int i = 0; i < val_ty->has.n; i++) {
      if (i >= struct_t->fields.n || !struct_t->fields[i]) continue;
      CGv2Value *field = new CGv2Value();
      field->id = 9500 + fc.cf->id * 1000 + (int)fc.cf->locals.n;
      field->name = "fmt_arg";
      field->type = struct_t->fields[i]->type;
      field->scope = CG2V_LOCAL;
      fc.cf->locals.add(field);
      CGv2Inst *fl = new CGv2Inst();
      fl->op = CG2_FIELD_LOAD;
      fl->field_idx = i;
      fl->type_arg = struct_t;
      fl->rvals.add(val);
      fl->lvals.add(field);
      blk->body.add(fl);
      call->rvals.add(field);
    }
  } else {
    call->rvals.add(val);
  }
  call->lvals.add(dst);
  blk->body.add(call);
  return true;
}

bool lower_send_prim(NormCtx &c, FunCtx &fc, PNode *pn,
                      CGv2Block *blk) {
  if (pn->rvals.n < 2) return false;
  Var *name_var = pn->rvals[1];
  if (!name_var || !name_var->sym) return false;
  cchar *name = name_var->sym->name;
  if (!name) name = name_var->sym->constant;
  if (!name) return false;

  // __pyc_c_call__ — pyc's generic FFI primitive. Route to
  // CG2_C_CALL via lower_send_c_call (D.3).
  if (strcmp(name, "__pyc_c_call__") == 0 &&
      lower_send_c_call(c, fc, pn, blk)) {
    return true;
  }
  // __pyc_format_string__ — pyc's `s % t` runtime helper.
  if (strcmp(name, "__pyc_format_string__") == 0 &&
      lower_send_format_string(c, fc, pn, blk)) {
    return true;
  }
  // __pyc_to_str__ — pyc's class/instance repr.  v1's
  // `to_str_codegen` (python_ifa_main.cc:93) compile-time-
  // resolves to a constant string: `"<class 'X'>"` for a
  // meta-type with a name, `"<instance>"` otherwise.  The
  // operand is at rvals[2] (after the sym_primitive +
  // prim-name markers).  Without this handler, `print(str)`
  // dropped the SEND and printed nothing.  F.4.6.
  if (strcmp(name, "__pyc_to_str__") == 0 && pn->rvals.n >= 3 &&
      pn->lvals.n >= 1) {
    Var *val_var = pn->rvals[2];
    CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
    if (val_var && val_var->type && dst) {
      char buf[256];
      if (val_var->type->is_meta_type && val_var->type->name) {
        snprintf(buf, sizeof(buf), "<class '%s'>",
                 val_var->type->name);
      } else {
        snprintf(buf, sizeof(buf), "<instance>");
      }
      CGv2Value *kc = new CGv2Value();
      kc->id = 9700 + fc.cf->id * 1000 +
               (int)c.p->constants.n;
      kc->name = "class_repr";
      kc->type = get_string_type(c);
      kc->scope = CG2V_CONSTANT;
      kc->imm.kind = CGv2Immediate::I_STR;
      kc->imm.str = dupstr(buf);
      c.p->constants.add(kc);
      CGv2Inst *mv = new CGv2Inst();
      mv->op = CG2_MOVE;
      mv->rvals.add(kc);
      mv->lvals.add(dst);
      blk->body.add(mv);
      return true;
    }
  }
  // Named primitives that have structural lowerings — route to
  // the typed CG_OP rather than a generic CG2_PRIM, so the v2
  // LLVM emit produces real IR instead of dropping the SEND.
  // The dispatcher in lower_send already calls these handlers
  // for prim->index == P_prim_*; the same handlers also work
  // here because they read rvals via compute_prim_arg_offset.
  // This catches the case where the FA leaves pn->prim as the
  // outer P_prim_primitive and the inner primitive name lives
  // in rvals[1] (the __pyc_primitive__("name", ...) shape).
  if (strcmp(name, "index_object") == 0 &&
      lower_send_index_load(c, fc, pn, blk)) return true;
  if (strcmp(name, "set_index_object") == 0 &&
      lower_send_index_store(c, fc, pn, blk)) return true;
  if (strcmp(name, "sizeof") == 0 &&
      lower_send_sizeof(c, fc, pn, blk)) return true;
  if (strcmp(name, "sizeof_element") == 0 &&
      lower_send_sizeof_element(c, fc, pn, blk)) return true;
  if (strcmp(name, "len") == 0 &&
      lower_send_len(c, fc, pn, blk)) return true;
  // Default: route an unrecognized primitive to a `_CG_<name>`
  // runtime helper via CG2_C_CALL, mirroring v1's write_send
  // (cg.cc:536) which emits `_CG_%s(...)` and lets the C
  // toolchain resolve the symbol at link time.  The old
  // behavior — wrapping the SEND in a CG2_PRIM whose emit
  // dispatcher silently drops unknown names — converted prim
  // bugs into wrong-output / hang / segfault failures with no
  // diagnostic.  CG2_C_CALL turns the same bug into a loud
  // "undefined reference to _CG_<name>" link error, surfacing
  // the missing handler before any test runs.
  CGv2Inst *inst = new CGv2Inst();
  inst->op = CG2_C_CALL;
  char helper[256];
  snprintf(helper, sizeof(helper), "_CG_%s", name);
  inst->prim_name = dupstr(helper);
  if (pn->lvals.n > 0 && pn->lvals[0]) {
    CGv2Value *dst = build_var(c, fc, pn->lvals[0]);
    if (dst) {
      inst->type_arg = dst->type;
      inst->lvals.add(dst);
    }
  }
  if (!inst->type_arg) inst->type_arg = c.p->t_void;
  for (int i = 2; i < pn->rvals.n; i++) {
    CGv2Value *cv = build_var(c, fc, pn->rvals[i]);
    if (cv) inst->rvals.add(cv);
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
    // P_prim_clone: prefer a concrete Fun call when the
    // analyzer resolved one (constructors, factory methods,
    // etc.). v1's translate_code_send/write_send takes the
    // same path implicitly because P_prim_clone returns 0
    // when the target type isn't a struct, falling through
    // to write_send. v2's CG_CLONE+memcpy from the @clone
    // prototype only works when that global is initialized
    // — which it isn't in pyc's current runtime model. So
    // when there's a callable Fun, route there instead.
    // Phase B.10.7.
    if (idx == P_prim_clone || idx == P_prim_clone_vector) {
      // F.3.1: route through CG2_CLONE.  The IF1 SEND from
      // `gen_class_pyda` (`python_ifa_build_syms.cc:614`) is
      // `(sym_primitive, sym_clone, proto)` → t.  Its `proto`
      // is `cls->self`, the class's prototype global, which
      // gets initialized by the class's ___init___ at startup
      // (line 581 in the same builder).  Memcpy'ing from that
      // global into the new instance gives the class-default
      // field values — `class A: x = 2; y = A(); y.x` now
      // reads 2 instead of 0.
      //
      // The pre-F.3 comment warned that "@clone prototype
      // global isn't initialized" — but that was reading
      // `rvals[1]` (the sym_clone marker), not the actual
      // proto at `rvals[o]`.  See lower_send_clone's comment.
      //
      // Falls back to bare alloc on shape mismatch so other
      // clone-edge cases (non-struct types, missing proto)
      // stay on the existing path.
      if (lower_send_clone(c, fc, pn, blk)) return;
      if (lower_send_alloc(c, fc, pn, blk)) return;
    }
    if (idx == P_prim_index_object &&
        lower_send_index_load(c, fc, pn, blk)) return;
    if (idx == P_prim_set_index_object &&
        lower_send_index_store(c, fc, pn, blk)) return;
    if (idx == P_prim_sizeof &&
        lower_send_sizeof(c, fc, pn, blk)) return;
    if (idx == P_prim_sizeof_element &&
        lower_send_sizeof_element(c, fc, pn, blk)) return;
    if (idx == P_prim_len &&
        lower_send_len(c, fc, pn, blk)) return;
    if (idx == P_prim_strcat &&
        lower_send_strcat(c, fc, pn, blk)) return;
    if (idx == P_prim_coerce &&
        lower_send_coerce(c, fc, pn, blk)) return;
    if (idx == P_prim_lnot &&
        lower_send_lnot(c, fc, pn, blk)) return;
    if (idx == P_prim_minus &&
        lower_send_neg(c, fc, pn, blk)) return;
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
void lower_body(NormCtx &c, Fun *f, FunCtx &fc) {
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
      if (is_closer) fc.block_closer.put(blk, cur);
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
      // Use code->label[0] (the goto target) via label_to_block.
      // cfg_succ ordering doesn't reliably match v1's semantics
      // for branches — v1 uses pn->code->label[N] explicitly
      // (llvm_codegen.cc:translate_code_goto). Phase B.10.6.
      term->br_target = cd->label[0]
                            ? fc.label_to_block.get(cd->label[0])
                            : (succ_blocks.n > 0 ? succ_blocks[0] : nullptr);
      break;
    case Code_IF:
      term->op = CG2_COND_BR;
      if (closer->rvals.n > 0) {
        CGv2Value *cv = build_var(c, fc, closer->rvals[0]);
        if (cv) term->rvals.add(cv);
      }
      // label[0] = true branch, label[1] = false branch
      // (matches v1's translate_code_if and the IF1
      // convention). Falls back to cfg_succ ordering when
      // labels aren't set (rare).
      term->br_true = cd->label[0]
                          ? fc.label_to_block.get(cd->label[0])
                          : (succ_blocks.n > 0 ? succ_blocks[0] : nullptr);
      term->br_false = cd->label[1]
                           ? fc.label_to_block.get(cd->label[1])
                           : (succ_blocks.n > 1 ? succ_blocks[1] : nullptr);
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

// Get-or-create a phi-group on `succ_blk` for the edge from
// `pred_blk`. Two distinct phi/phy passes (or the same pass
// firing for multiple edges) on the same edge share one
// group so the v2 emit's per-pred alloca+store pass picks
// up all the MOVEs in source order.
CGv2PhiGroup *get_or_make_phi_group(CGv2Block *succ_blk,
                                     CGv2Block *pred_blk) {
  for (CGv2PhiGroup *g : succ_blk->phi_by_pred) {
    if (g->pred == pred_blk) return g;
  }
  CGv2PhiGroup *g = new CGv2PhiGroup();
  g->pred = pred_blk;
  succ_blk->phi_by_pred.add(g);
  return g;
}

// Pass 3 of build_fun_body — phi/phy materialization.
//
// IF1's phi and phy PNodes encode "MOVE on an edge":
//   phy at closer N, edge i: lvals[i] ← rvals[0]
//   phi at successor S, from pred N: lvals[0] ← rvals[pred_idx]
// where pred_idx is N's position in S's pred list (carried
// on cfg_pred_index).
//
// Both translate to CGv2_MOVE insts placed in the
// successor block's phi_by_pred group keyed by the
// predecessor block. The v2 emit's alloca/store-on-edge
// path takes over from there (Phase 4 :phi_by_pred lowering).
//
// Mirrors v1's do_phy_nodes / do_phi_nodes in
// llvm_codegen.cc:610-631, but encodes the moves into the
// IR rather than emitting them inline at LLVM-emit time.
void materialize_phi_phy(NormCtx &c, FunCtx &fc) {
  for (CGv2Block *pred_blk : fc.cf->blocks) {
    PNode *closer = fc.block_closer.get(pred_blk);
    if (!closer) continue;
    for (int isucc = 0; isucc < closer->cfg_succ.n; isucc++) {
      PNode *succ_pn = closer->cfg_succ[isucc];
      if (!succ_pn) continue;
      // Find the block this edge lands in. Only LABEL-headed
      // successors start a new block; intra-block edges aren't
      // candidates for phi MOVEs.
      CGv2Block *succ_blk = nullptr;
      if (succ_pn->code && succ_pn->code->kind == Code_LABEL) {
        succ_blk = fc.pn_to_block.get(succ_pn);
      }
      if (!succ_blk || succ_blk == pred_blk) continue;

      // phy: lvals[isucc] = rvals[0] for each phy on the
      // closer. The isucc index distinguishes which target
      // slot of the multi-lval phy gets bound on this edge.
      for (PNode *p : closer->phy) {
        if (!p || p->lvals.n <= isucc || p->rvals.n < 1) continue;
        CGv2Value *src = build_var(c, fc, p->rvals[0]);
        CGv2Value *dst = build_var(c, fc, p->lvals[isucc]);
        if (!src || !dst) continue;
        CGv2Inst *mv = new CGv2Inst();
        mv->op = CG2_MOVE;
        mv->rvals.add(src);
        mv->lvals.add(dst);
        get_or_make_phi_group(succ_blk, pred_blk)->moves.add(mv);
      }

      // phi: each phi PNode on the successor has lvals[0] ←
      // rvals[pred_idx] where pred_idx is the closer's position
      // in succ's pred list.  build_fun_body calls
      // rebuild_cfg_pred_index before us so the map is fresh.
      int pred_idx = succ_pn->cfg_pred_index.get(closer);
      for (PNode *p : succ_pn->phi) {
        if (!p || p->lvals.n < 1) continue;
        if (p->rvals.n <= pred_idx) continue;
        CGv2Value *src = build_var(c, fc, p->rvals[pred_idx]);
        CGv2Value *dst = build_var(c, fc, p->lvals[0]);
        if (!src || !dst) continue;
        CGv2Inst *mv = new CGv2Inst();
        mv->op = CG2_MOVE;
        mv->rvals.add(src);
        mv->lvals.add(dst);
        get_or_make_phi_group(succ_blk, pred_blk)->moves.add(mv);
      }
    }
  }
}

// Per-Fun body build. Block skeleton (B.5) + body insts (B.6)
// + terminators (B.7) + phi/phy materialization (B.8.7). Any
// block without a closer gets an UNREACHABLE terminator so
// verifyModule stays happy.
void build_fun_body(NormCtx &c, Fun *f, CGv2Fun *cf, FunCtx &fc) {
  if (!f || !cf || !f->entry) return;
  build_block_skeleton(c, f, fc);
  lower_body(c, f, fc);
  // SSU's cfg_pred_index is stale after clone/optimization
  // passes — the v1 C backend rebuilds it before walking
  // PNodes (cg.cc:718). Without this rebuild, the phi loop in
  // materialize_phi_phy below reads `cfg_pred_index.get(closer)`
  // for non-entry preds and gets 0 (the missing-key default),
  // causing loop-back edges to incorrectly resolve to the entry
  // pred's rvals[0] — the SSU accumulation bug.
  rebuild_cfg_pred_index(f);
  materialize_phi_phy(c, fc);
  for (CGv2Block *b : cf->blocks) {
    build_terminator(c, fc, b, fc.block_closer.get(b));
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

// Issue 023 Stage 3 — per-function arg-escape analysis.
//
// For each function, compute which formals "escape" the
// function body.  An arg `fa` escapes if any of its uses is
// non-benign: appearing in CG2_RET rvals, being the source of
// a MOVE into a CG2V_GLOBAL lvalue, appearing as the value
// position (rvals[1+]) of a FIELD/INDEX_STORE whose target ptr
// also escapes, or being passed to a CG2_CALL at an arg
// position whose callee marks that slot as escape.
//
// Benign uses (which do NOT cause escape): position 0 of
// FIELD/INDEX LOAD/STORE / LEN / SIZEOF_ELEMENT / CLONE (the
// pointer is read/written THROUGH, the pointer itself doesn't
// leave), or the value position of a FIELD/INDEX_STORE whose
// target is itself non-escaping (the value lives only as long
// as the target).
//
// Algorithm: track a per-function "non-escape" set of
// CGv2Values, initially populated with formals.  Iteratively
// prune any value whose uses include an escape.  Cross-function
// info (callee arg_escapes) participates in the per-iteration
// re-check.  Outer loop iterates over the call graph until
// arg_escapes stabilizes (bounded by total args).
namespace {

static bool is_ptr_target_op(CGv2Op op) {
  return op == CG2_FIELD_STORE || op == CG2_FIELD_LOAD ||
         op == CG2_INDEX_STORE || op == CG2_INDEX_LOAD ||
         op == CG2_LEN || op == CG2_SIZEOF_ELEMENT ||
         op == CG2_CLONE;
}

// Check whether `v` escapes the body of `cf`, given the
// current `non_escape` set within cf and the cross-function
// arg_escapes data already populated.
//
// `non_escape` is the set of CGv2Values already proved
// non-escaping in this function (formals + values whose uses
// have all been classified benign so far).  `v` itself need
// not be in the set; we're computing whether to add it.
static bool value_escapes(CGv2Fun *cf, CGv2Value *v,
                          Map<CGv2Value *, int> &non_escape,
                          CGv2Program *p) {
  for (CGv2Block *b : cf->blocks) {
    for (CGv2Inst *bi : b->body) {
      // MOVE: source escapes if dst is a global; otherwise
      // dst is a local alias — escape transitively if dst
      // escapes (i.e. dst not in non_escape).
      if (bi->op == CG2_MOVE && bi->rvals.n > 0 && bi->lvals.n > 0 &&
          bi->rvals[0] == v) {
        if (bi->lvals[0]->scope == CG2V_GLOBAL) return true;
        if (!non_escape.get(bi->lvals[0])) return true;
        continue;
      }
      bool target_op = is_ptr_target_op(bi->op);
      for (int i = 0; i < bi->rvals.n; i++) {
        if (bi->rvals[i] != v) continue;
        if (target_op && i == 0) continue;  // benign: ptr is the target
        // Value position of FIELD/INDEX_STORE: benign if the
        // target ptr is non-escaping.
        if ((bi->op == CG2_FIELD_STORE || bi->op == CG2_INDEX_STORE) &&
            i >= 1 && bi->rvals.n >= 1 && non_escape.get(bi->rvals[0])) {
          continue;
        }
        // CG2_CALL: benign if callee marks this slot as
        // non-escaping.  rvals[0] is the callee; rvals[1..]
        // are user args.  If callee has sret, its LLVM
        // arg-0 is the implicit slot — so the rvals[i] at
        // position 1 maps to the callee's formal index 0,
        // etc.
        if (bi->op == CG2_CALL && i >= 1 && bi->rvals[0] &&
            bi->rvals[0]->target_name) {
          CGv2Fun *callee = p->lookup_fun(bi->rvals[0]->target_name);
          if (callee && callee->arg_escapes.n > 0) {
            int formal_idx = i - 1;  // skip callee slot
            if (formal_idx < callee->arg_escapes.n &&
                !callee->arg_escapes[formal_idx]) {
              continue;
            }
          }
        }
        return true;
      }
    }
    // Block terminator escape check.
    if (b->terminator) {
      CGv2Inst *t = b->terminator;
      if (t->op == CG2_RET) {
        for (CGv2Value *rv : t->rvals)
          if (rv == v) return true;
      }
      // CG2_BR / CG2_COND_BR don't carry CGv2Values as rvals
      // in a way that would propagate `v`; skip.
    }
  }
  return false;
}

}  // namespace

static void compute_arg_escapes(CGv2Program *p) {
  if (!p) return;

  // Outer loop: re-run until no function's arg_escapes flips.
  // Each function's analysis can refine its own arg_escapes
  // (which then feeds back into callers' analyses on the next
  // iteration).
  bool changed = true;
  int outer_passes = 0;
  while (changed && outer_passes < 8) {
    changed = false;
    outer_passes++;
    for (CGv2Fun *cf : p->funs) {
      if (!cf || cf->is_external) continue;
      // Inner fixed-point per function: start with formals in
      // non_escape, iteratively prune.  Also pull in locals
      // that prove non-escaping so transitive uses (value
      // stored into a non-escaping struct, etc.) chain
      // correctly.
      Map<CGv2Value *, int> non_escape;
      for (CGv2Value *fv : cf->formals) non_escape.put(fv, 1);
      // Seed locals as candidates; we'll prune ones that
      // escape.
      Vec<CGv2Value *> candidates;
      candidates.copy(cf->formals);
      for (CGv2Value *lv : cf->locals) {
        if (lv) {
          non_escape.put(lv, 1);
          candidates.add(lv);
        }
      }
      bool inner_changed = true;
      while (inner_changed) {
        inner_changed = false;
        for (CGv2Value *v : candidates) {
          if (!non_escape.get(v)) continue;
          if (value_escapes(cf, v, non_escape, p)) {
            non_escape.put(v, 0);
            inner_changed = true;
          }
        }
      }
      // Project the final state onto cf->arg_escapes (for
      // cross-function lookup) and onto each value's
      // .escapes bit (for emit-time consumption).
      Vec<bool> new_escapes;
      for (CGv2Value *fv : cf->formals) {
        bool ne = non_escape.get(fv) ? true : false;
        new_escapes.add(!ne);
        fv->escapes = !ne;
      }
      for (CGv2Value *lv : cf->locals) {
        if (lv) lv->escapes = non_escape.get(lv) ? false : true;
      }
      if (cf->arg_escapes.n != new_escapes.n) {
        cf->arg_escapes.move(new_escapes);
        changed = true;
      } else {
        for (int i = 0; i < new_escapes.n; i++) {
          if (cf->arg_escapes[i] != new_escapes[i]) {
            cf->arg_escapes[i] = new_escapes[i];
            changed = true;
          }
        }
      }
    }
  }
}

CGv2Program *cg_normalize_v2(FA *fa) {
  CGv2Program *p = new CGv2Program();
  if (!fa) return p;

  NormCtx c(p);
  build_types(c, fa);
  build_globals(c, fa);
  build_funs(c, fa);
  build_fun_bodies(c, fa);

  // Escape annotation source.  Two paths:
  //   - IFA-integrated (Phase 1+, see ESCAPE_PLAN.md): when
  //     ifa_escape_in_fa is on, copy `f->arg_escapes` (set
  //     by IFA's escape pass) onto each CGv2Fun.  In Phase 1
  //     IFA leaves it empty, so the copy is a no-op and
  //     codegen still relies on the Stage 3 fallback.
  //   - Stage 3 (issue 023, today's default): scan each
  //     CGv2Fun body locally with cross-function lookups via
  //     CGv2Program::lookup_fun.
  // Both can run during the phased rollout (1-4); Stage 3
  // remains the production source-of-truth until Phase 5.
  //
  // Phase 2: IFA populates Fun::arg_escapes intra-procedurally,
  // but it's used only as a comparison signal — Stage 3
  // still computes the result that drives alloca/sret choice.
  // This keeps the IFA pass non-disruptive while the lattice
  // and transfer rules mature.
  if (ifa_escape_in_fa) {
    // Phase 2 readback: copy onto CGv2Fun for cross-fun
    // lookup ergonomics, but do NOT drive codegen — Stage 3
    // below will overwrite.  Phase 5 will flip this around.
    for (Fun *f : fa->funs) {
      CGv2Fun *cf = c.fun_to_fun.get(f);
      if (!cf || f->arg_escapes.n == 0) continue;
      cf->arg_escapes.clear();
      for (int i = 0; i < f->arg_escapes.n; i++)
        cf->arg_escapes.add(f->arg_escapes[i] != 0);
    }
  }
  // Issue 023 Stage 3: cross-function arg-escape annotation,
  // consumed by emit-time `value_escapes_in_fun` to unlock
  // alloca for ptrs passed only into read-only callees.
  // Production source of truth during Phases 1-4.
  compute_arg_escapes(p);

  return p;
}
