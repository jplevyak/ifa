// SPDX-License-Identifier: BSD-3-Clause
//
// cg_view_emit_llvm.cc — direct PNode → LLVM IR emitter.
//
// Phase R (CG_VIRTUAL_PLAN architectural reset).  Mirrors the
// structure of `codegen/cg.cc` (the C backend at 99/0) but
// emits LLVM IR instead of C text.  No CGv2Program, no
// CGv2Inst — type info comes from the IF1 `Var->type` Sym +
// `Sym->has[]` field walk at emit time; per-Fun
// `Var * → llvm::Value *` map binds operand identity.
//
// Public entry: `cg_view_emit_llvm(fa, main_fun)`.  Wired
// from `llvm.cc` via `PYC_LLVM_VIEW2=1`.
//
// Phase R sub-phases (see CG_VIRTUAL_PLAN.md):
//   R.1 — skeleton + simplest cases (MOVE, LABEL, RET, IF, GOTO).
//   R.2 — per-prim ports (period, setter, make, call, etc.).
//   R.3 — close C-parity to 99/0.
//   R.4 — retire cg_normalize_v2 + cg_ir_v2_emit_llvm.

#include "ifadefs.h"

#include "codegen/llvm_internal.h"
#include "codegen/codegen_common.h"
#include "fa.h"
#include "fun.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

namespace {

// -------------------------------------------------------------
// Type cache: Sym * → llvm::Type *.  Built lazily by visiting
// `s->has[]` once per struct; the recursion guard registers
// the partially-built llvm::StructType before walking fields
// so cycles resolve cleanly.
// -------------------------------------------------------------

Map<Sym *, llvm::Type *> g_sym_to_type;
Map<Sym *, llvm::StructType *> g_sym_to_struct;

llvm::Type *sym_to_llvm_type(Sym *s);

llvm::StructType *sym_to_llvm_struct(Sym *s) {
  if (!s) return nullptr;
  if (llvm::StructType *cached = g_sym_to_struct.get(s)) return cached;
  cchar *raw_name = s->name ? s->name :
                    (s->cg_string ? s->cg_string : "anon");
  char buf[160];
  snprintf(buf, sizeof(buf), "%s.%d", raw_name, s->id);
  llvm::StructType *st =
      llvm::StructType::getTypeByName(*TheContext, buf);
  if (!st) st = llvm::StructType::create(*TheContext, buf);
  g_sym_to_struct.put(s, st);
  if (st->isOpaque()) {
    std::vector<llvm::Type *> fields;
    for (Sym *f : s->has) {
      llvm::Type *ft = sym_to_llvm_type(f->type);
      // Substitute opaque ptr for unsized / void fields.
      if (!ft || ft->isVoidTy()) {
        ft = llvm::PointerType::getUnqual(*TheContext);
      }
      fields.push_back(ft);
    }
    st->setBody(fields, /*isPacked=*/false);
  }
  return st;
}

llvm::Type *sym_to_llvm_type(Sym *s) {
  if (!s) return llvm::Type::getVoidTy(*TheContext);
  if (llvm::Type *cached = g_sym_to_type.get(s)) return cached;
  llvm::Type *t = nullptr;

  // Predefined numerics.
  if (s->num_kind) {
    switch (s->num_kind) {
      case IF1_NUM_KIND_INT:
      case IF1_NUM_KIND_UINT:
        switch (s->num_index) {
          case IF1_INT_TYPE_1:
            t = llvm::Type::getInt1Ty(*TheContext); break;
          case IF1_INT_TYPE_8:
            t = llvm::Type::getInt8Ty(*TheContext); break;
          case IF1_INT_TYPE_16:
            t = llvm::Type::getInt16Ty(*TheContext); break;
          case IF1_INT_TYPE_32:
            t = llvm::Type::getInt32Ty(*TheContext); break;
          case IF1_INT_TYPE_64:
            t = llvm::Type::getInt64Ty(*TheContext); break;
        }
        break;
      case IF1_NUM_KIND_FLOAT:
        switch (s->num_index) {
          case IF1_FLOAT_TYPE_32:
            t = llvm::Type::getFloatTy(*TheContext); break;
          case IF1_FLOAT_TYPE_64:
            t = llvm::Type::getDoubleTy(*TheContext); break;
        }
        break;
    }
  }

  if (!t) {
    if (s == sym_void) {
      t = llvm::Type::getVoidTy(*TheContext);
    } else if (s == sym_nil_type || s->is_symbol) {
      t = llvm::PointerType::getUnqual(*TheContext);
    } else if (s == sym_string ||
               (sym_string && sym_string->specializers.set_in(s))) {
      // pyc strings are typed char-ptrs.
      t = llvm::PointerType::getUnqual(*TheContext);
    } else if (s->type_kind == Type_RECORD) {
      // pyc holds records by pointer.  The struct itself is
      // looked up via sym_to_llvm_struct; the value-shape
      // type is the wrapping ptr.
      (void)sym_to_llvm_struct(s);  // ensure struct is built
      t = llvm::PointerType::getUnqual(*TheContext);
    } else if (s->type_kind == Type_FUN && !s->fun && s->has.n) {
      // Closure: struct + ptr wrapper.
      (void)sym_to_llvm_struct(s);
      t = llvm::PointerType::getUnqual(*TheContext);
    } else {
      // Type_FUN with resolved fun, Type_REF, Type_PRIMITIVE,
      // and other Type_SUM cases → opaque ptr.
      t = llvm::PointerType::getUnqual(*TheContext);
    }
  }

  g_sym_to_type.put(s, t);
  return t;
}

// -------------------------------------------------------------
// Per-Fun emit context: Var * → llvm::Value *.
// -------------------------------------------------------------

struct EmitCtx {
  Fun *fn;
  llvm::Function *llvm_fn;
  Map<Var *, llvm::Value *> var_map;
  Map<PNode *, llvm::BasicBlock *> label_bb;
};

void emit_pnode(EmitCtx &ctx, PNode *pn, Vec<PNode *> &done);

// -------------------------------------------------------------
// Reset per-program state.  Called at the start of every
// `cg_view_emit_llvm` invocation so type caches don't leak
// across compiles.
// -------------------------------------------------------------

void reset_state() {
  g_sym_to_type.clear();
  g_sym_to_struct.clear();
}

// -------------------------------------------------------------
// Per-PNode emit dispatcher.  Mirrors cg.cc:write_c_pnode.
// Phase R.1: stub — only Code_LABEL / Code_MOVE / Code_GOTO /
// Code_IF / Code_SEND-reply implemented.  Everything else
// returns without emitting.
// -------------------------------------------------------------

void emit_pnode(EmitCtx &ctx, PNode *pn, Vec<PNode *> &done) {
  if (!pn || !pn->code) return;
  // Phase R.1 stub: walk successors so the DFS still
  // terminates, but emit nothing per-PNode yet.  Real
  // per-Code dispatch lands in R.1.3-R.1.5.
  for (PNode *s : pn->cfg_succ) {
    if (s && done.set_add(s)) emit_pnode(ctx, s, done);
  }
}

// -------------------------------------------------------------
// Per-Fun signature build.  Mirrors cg.cc:write_c_fun_proto
// (line 20) at the LLVM level.
//
// Iteration over positional formals follows the materialized
// translator's pattern (walk f->positional_arg_positions
// rather than f->sym->has), matching the MPosition-based
// lookup `f->args.get(p)`.  Skip dead and is_fun formals (the
// is_fun filter at the C level corresponds to v2's
// `formal_arg->type->is_fun` skip in lower_send_call's
// MPosition routing — the closure self formal isn't part of
// the LLVM signature).
//
// Sret-rewrite: when the result is a value-type RECORD
// (Sym::is_value_type), the materialized side passes the
// caller-provided buffer as an implicit first ptr arg.  R.1
// deferral — left as a TODO.  Most code uses heap-aggregate
// records (the pyc default), so this only affects @pyc_struct
// returns.
// -------------------------------------------------------------

llvm::Function *build_fun_signature(EmitCtx &ctx, Fun *f) {
  if (!f || !f->sym) return nullptr;
  cchar *name = cg_get_string(f);
  if (!name) name = "fn";

  // Reuse existing declaration if some earlier emit (or a
  // forward declare) already created the llvm::Function.
  if (llvm::Function *existing = TheModule->getFunction(name)) {
    ctx.llvm_fn = existing;
    return existing;
  }

  // Return type.  Materialized's sret-rewrite for value-type
  // RECORD returns is a future R.1.2.1 item; for now value-
  // type RECORDS get returned by value (let LLVM lower).
  llvm::Type *ret_ty = llvm::Type::getVoidTy(*TheContext);
  if (f->rets.n == 1 && f->rets.v[0] && f->rets.v[0]->type) {
    ret_ty = sym_to_llvm_type(f->rets.v[0]->type);
    if (!ret_ty) ret_ty = llvm::Type::getVoidTy(*TheContext);
  }

  // Collect param types from positional formals, skipping
  // dead and is_fun (closure-self) formals.  Order matches
  // cg.cc's iteration so the per-position arg routing in
  // emit_send_call lines up.
  std::vector<llvm::Type *> param_tys;
  Vec<Var *> formal_vars;
  for (MPosition *p : f->positional_arg_positions) {
    Var *v = f->args.get(p);
    if (!v || !v->live) continue;
    if (v->sym && v->sym->is_fun) continue;
    if (v->type && v->type->is_fun) continue;
    llvm::Type *pt = v->type ? sym_to_llvm_type(v->type)
                              : llvm::PointerType::getUnqual(*TheContext);
    if (!pt) pt = llvm::PointerType::getUnqual(*TheContext);
    param_tys.push_back(pt);
    formal_vars.add(v);
  }

  llvm::FunctionType *ft = llvm::FunctionType::get(
      ret_ty, param_tys, /*isVarArg=*/false);
  llvm::Function *llvm_fn = llvm::Function::Create(
      ft, llvm::Function::ExternalLinkage, name, TheModule.get());

  // Bind each llvm::Argument back to its Var via the per-Fun
  // var_map.  Read sites pick up the bound value through
  // ctx.var_map; cg.cc's equivalent is the cg_get_string ↔
  // arg name correspondence.
  int idx = 0;
  for (llvm::Argument &a : llvm_fn->args()) {
    if (idx >= formal_vars.n) break;
    Var *v = formal_vars.v[idx];
    cchar *vname = cg_get_string(v);
    if (vname) a.setName(vname);
    ctx.var_map.put(v, &a);
    idx++;
  }

  ctx.llvm_fn = llvm_fn;
  return llvm_fn;
}

// -------------------------------------------------------------
// Per-Fun emit.  Phase R.1.2 builds the signature.  Body
// emission lands in R.1.3.
// -------------------------------------------------------------

void emit_fun(EmitCtx &ctx, Fun *f) {
  if (!f) return;
  if (!build_fun_signature(ctx, f)) return;
  if (f->is_external) return;  // body is in a linked library.
  // R.1.3: body via emit_pnode DFS from f->entry.  For now,
  // emit a single "unreachable" entry block so the
  // declaration's verifier check stays happy when this path
  // is wired up.
  if (!ctx.llvm_fn) return;
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(*TheContext, "entry", ctx.llvm_fn);
  Builder->SetInsertPoint(entry);
  Builder->CreateUnreachable();
}

}  // namespace

// -------------------------------------------------------------
// Public entry.
// -------------------------------------------------------------

bool cg_view_emit_llvm(FA *fa, Fun *main_fun) {
  if (!fa || !TheModule) return false;
  reset_state();

  // Phase R.1 placeholder: emit nothing.  Calling this from
  // llvm.cc under PYC_LLVM_VIEW2=1 produces an empty module
  // which won't pass verifyModule — so the env var should
  // not be set until R.1.3+ lands actual emission.  This
  // skeleton's value is the type cache + the scaffolding
  // that the subsequent landings build on.
  (void)main_fun;

  for (Fun *f : fa->funs) {
    if (!f || !f->live) continue;
    EmitCtx ctx;
    ctx.fn = f;
    ctx.llvm_fn = nullptr;
    emit_fun(ctx, f);
  }
  return true;
}
