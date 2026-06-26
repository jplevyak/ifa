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
#include "pattern.h"  // MPosition + Position2int
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
#include "llvm/IR/Intrinsics.h"
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
Map<cchar *, llvm::GlobalVariable *> g_string_globals;
// Per-program globals: maps an IF1 Var (a module-level global)
// to its LLVM GlobalVariable.  Populated by Pass 0
// (`declare_globals`); read by emit_send and value_for_var
// to resolve global Var operands.
Map<Var *, llvm::GlobalVariable *> g_var_to_global;

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
  // Phi-target Vars get an alloca slot in the entry block so
  // cross-edge writes have a stable storage location.  Mirrors
  // cg_ir_v2_emit_llvm.cc's alloca pre-pass.  Without this,
  // SSA-bind alone can't carry values across loop back-edges
  // or if/else joins — every loop body just sees the initial
  // value forever.  When a Var has an entry in alloca_map,
  // value_for_var emits a load, put_result emits a store, and
  // emit_phi_moves writes via store.
  Map<Var *, llvm::AllocaInst *> alloca_map;
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
  g_string_globals.clear();
  g_var_to_global.clear();
}

// Pass 0: declare module-level globals.  Mirrors
// cg.cc:c_codegen_print_c lines 1014-1057 (which emits C
// `<type> g<N>;` declarations) and the materialized
// emitter's globals pre-declaration pass.
//
// `collect_types_and_globals` (fa.h:550) walks fa and
// populates the `globals` vector with module-level Vars.
// For each that's a live, non-constant, non-fun Var, we
// emit an `llvm::GlobalVariable` with private internal
// linkage and zero (null ptr / 0) initializer.
//
// Then the per-Fun emit reads these via
// `g_var_to_global` when value_for_var sees a global Var.
void declare_globals(FA *fa) {
  Vec<Sym *> typesyms;
  Vec<Var *> globals;
  collect_types_and_globals(fa, typesyms, globals);
  for (Var *v : globals) {
    if (!v || !v->live || !v->sym) continue;
    Sym *s = v->sym;
    // Constants don't need a global slot — they're inlined
    // via value_for_var.
    if (s->is_constant) continue;
    // Fun references aren't stored in globals at LLVM level.
    if (s->is_fun) continue;
    // Skip symbol Syms and nil — they're constants in disguise.
    if (s == sym_nil_type) continue;
    if (s->is_symbol) continue;
    if (!v->type) continue;
    llvm::Type *t = sym_to_llvm_type(v->type);
    if (!t) continue;
    cchar *name = cg_get_string(v);
    if (!name) name = (s->name ? s->name : "g");
    // If a global with this name already exists in the
    // module (declared by some earlier pass), reuse it.
    if (llvm::GlobalVariable *existing =
            TheModule->getNamedGlobal(name)) {
      g_var_to_global.put(v, existing);
      continue;
    }
    llvm::Constant *init = nullptr;
    if (t->isPointerTy()) {
      init = llvm::ConstantPointerNull::get(
          llvm::cast<llvm::PointerType>(t));
    } else if (t->isIntegerTy()) {
      init = llvm::ConstantInt::get(t, 0);
    } else if (t->isFloatingPointTy()) {
      init = llvm::ConstantFP::get(t, 0.0);
    } else {
      init = llvm::UndefValue::get(t);
    }
    llvm::GlobalVariable *gv = new llvm::GlobalVariable(
        *TheModule, t, /*isConstant=*/false,
        llvm::GlobalValue::InternalLinkage, init, name);
    g_var_to_global.put(v, gv);
  }
}

// (emit_main_prelude logic inlined into cg_view_emit_llvm
// below since it needs get_gc_malloc which is declared
// later in the file; cleaner than a forward decl.)

// -------------------------------------------------------------
// Operand lookup helpers — mirror cg.cc's `cg_get_string(v)`
// idiom.  Read sites consult `ctx.var_map`; for is_constant
// Syms with an Immediate, materialize the LLVM constant
// inline.  Write sites bind the produced llvm::Value via
// `put_result`.
// -------------------------------------------------------------

// Build (or fetch from the cache) a pyc-layout string
// global: `{ i64 len, [N x i8] body }` packed.  Returns
// a pointer to the first body byte (i.e. `gv + offset(len)`).
// Mirrors cg_ir_v2_emit_llvm.cc:261-307.
llvm::Value *materialize_pyc_string(cchar *raw) {
  if (!raw) raw = "";
  if (llvm::GlobalVariable *cached = g_string_globals.get(raw)) {
    llvm::StructType *sty =
        llvm::cast<llvm::StructType>(cached->getValueType());
    llvm::Type *i32_ty = llvm::Type::getInt32Ty(*TheContext);
    return Builder->CreateInBoundsGEP(
        sty, cached,
        { llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 1),
          llvm::ConstantInt::get(i32_ty, 0) });
  }
  std::string text = raw;
  if (text.size() >= 2 && text.front() == '"' && text.back() == '"') {
    text = text.substr(1, text.size() - 2);
  }
  llvm::Type *i64_ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::Constant *cdata =
      llvm::ConstantDataArray::getString(*TheContext, text);
  llvm::StructType *sty = llvm::StructType::get(
      *TheContext, { i64_ty, cdata->getType() },
      /*isPacked=*/true);
  llvm::Constant *init = llvm::ConstantStruct::get(
      sty,
      { llvm::ConstantInt::get(i64_ty, (uint64_t)text.size()),
        cdata });
  llvm::GlobalVariable *gv = new llvm::GlobalVariable(
      *TheModule, sty, /*isConstant=*/true,
      llvm::GlobalValue::PrivateLinkage, init, ".str.lit");
  g_string_globals.put(raw, gv);
  llvm::Type *i32_ty = llvm::Type::getInt32Ty(*TheContext);
  return Builder->CreateInBoundsGEP(
      sty, gv,
      { llvm::ConstantInt::get(i32_ty, 0),
        llvm::ConstantInt::get(i32_ty, 1),
        llvm::ConstantInt::get(i32_ty, 0) });
}

llvm::Value *value_for_var(EmitCtx &ctx, Var *v) {
  if (!v) return nullptr;
  // Phi-target Var: load from its alloca slot every time
  // (each read sees the current store, which lets loops
  // re-read updated state).
  if (llvm::AllocaInst *slot = ctx.alloca_map.get(v)) {
    cchar *name = cg_get_string(v);
    return Builder->CreateLoad(
        slot->getAllocatedType(), slot, name ? name : "");
  }
  // Global Var: ALWAYS load from the module-level
  // GlobalVariable.  Checking var_map first would return a
  // stale cached SSA value from a different block, causing
  // dominance violations.  Globals are unique values whose
  // current contents live in memory.
  if (llvm::GlobalVariable *gv = g_var_to_global.get(v)) {
    llvm::Type *t = sym_to_llvm_type(v->type);
    if (!t) t = llvm::PointerType::getUnqual(*TheContext);
    cchar *name = cg_get_string(v);
    llvm::Value *loaded = Builder->CreateLoad(
        t, gv, name ? name : "g");
    return loaded;
  }
  if (llvm::Value *cached = ctx.var_map.get(v)) return cached;
  // Constant Sym: materialize the LLVM constant directly.
  Sym *s = v->sym;
  if (s && s->is_constant && v->type) {
    llvm::Type *t = sym_to_llvm_type(v->type);
    if (!t) return nullptr;
    llvm::Value *cv = nullptr;
    switch (s->imm.const_kind) {
      case IF1_NUM_KIND_INT:
      case IF1_NUM_KIND_UINT:
        if (t->isIntegerTy())
          cv = llvm::ConstantInt::get(t, (uint64_t)s->imm.v_int64,
                                      s->imm.const_kind == IF1_NUM_KIND_INT);
        break;
      case IF1_NUM_KIND_FLOAT:
        if (t->isFloatingPointTy())
          cv = llvm::ConstantFP::get(t, s->imm.v_float64);
        break;
      case IF1_CONST_KIND_STRING:
        // String literal — emit as pyc-layout global and GEP
        // to the first char.  Same shape as cg_ir_v2_emit_llvm
        // produces so runtime helpers (`_CG_string_len` etc.)
        // can read the length prefix at offset -8.
        cv = materialize_pyc_string(s->imm.v_string);
        break;
      case IF1_CONST_KIND_SYMBOL:
        // Symbol constants — landed in the materialized side
        // via test against sym_to_value cache; for now opaque
        // null, downstream code that needs the symbol's
        // pointer-tagged value will run wrong on these but
        // not crash the emit.
        if (t->isPointerTy())
          cv = llvm::ConstantPointerNull::get(
              llvm::cast<llvm::PointerType>(t));
        break;
      default:
        break;
    }
    if (!cv && t->isPointerTy()) {
      cv = llvm::ConstantPointerNull::get(
          llvm::cast<llvm::PointerType>(t));
    }
    if (cv) ctx.var_map.put(v, cv);
    return cv;
  }
  return nullptr;
}

void put_result(EmitCtx &ctx, Var *v, llvm::Value *value) {
  if (!v || !value) return;
  // Phi-target Var: store to its alloca slot so subsequent
  // reads (including reads in a different block) see the
  // new value via load.
  if (llvm::AllocaInst *slot = ctx.alloca_map.get(v)) {
    llvm::Type *want = slot->getAllocatedType();
    if (value->getType() != want) {
      if (value->getType()->isPointerTy() && want->isPointerTy()) {
        // opaque-ptr no-op.
      } else if (value->getType()->isIntegerTy() && want->isIntegerTy()) {
        value = Builder->CreateSExtOrTrunc(value, want);
      } else if (value->getType()->isPointerTy() && want->isIntegerTy()) {
        value = Builder->CreatePtrToInt(value, want);
      } else if (value->getType()->isIntegerTy() && want->isPointerTy()) {
        value = Builder->CreateIntToPtr(value, want);
      }
    }
    Builder->CreateStore(value, slot);
    return;
  }
  ctx.var_map.put(v, value);
  if (value->getType()->isPointerTy() == false &&
      value->getType()->isVoidTy() == false) {
    cchar *vname = cg_get_string(v);
    if (vname && !value->hasName()) value->setName(vname);
  }
}

// -------------------------------------------------------------
// Get-or-create an extern declaration for a pyc runtime helper.
// `_CG_<name>` style — used by per-prim emitters that route to
// runtime calls (mirrors cg.cc's `_CG_%s` fprintf pattern).
// -------------------------------------------------------------

llvm::Function *get_runtime_helper(cchar *name, llvm::Type *ret_ty,
                                     llvm::ArrayRef<llvm::Type *> param_tys,
                                     bool is_va = false) {
  if (llvm::Function *existing = TheModule->getFunction(name)) return existing;
  llvm::FunctionType *ft = llvm::FunctionType::get(ret_ty, param_tys, is_va);
  return llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
                                 name, TheModule.get());
}

// -------------------------------------------------------------
// resolve_union_receiver — port of cg.cc:148.  When the obj's
// declared type is a Type_SUM (union), pick a concrete
// component that carries the named field.  This is the cg.cc
// trick that gets it to 99/0 against materialized's 92/7 —
// the materialized side doesn't carry this through CGv2Type
// so emit gets stuck on opaque-typed unions.
// -------------------------------------------------------------

Sym *resolve_union_receiver(Sym *obj, cchar *symbol) {
  if (!obj || obj->type_kind != Type_SUM) return obj;
  if (symbol) {
    for (Sym *component : obj->has) {
      if (!component || component == sym_nil_type) continue;
      for (Sym *field : component->has) {
        if (field && field->name == symbol) return component;
      }
    }
  }
  if (obj->has.n > 0) {
    for (Sym *component : obj->has) {
      if (component && component != sym_nil_type) return component;
    }
    return obj->has.v[0];
  }
  return obj;
}

// Get-or-create GC_malloc declaration.
llvm::Function *get_gc_malloc() {
  return get_runtime_helper(
      "GC_malloc",
      llvm::PointerType::getUnqual(*TheContext),
      { llvm::Type::getInt64Ty(*TheContext) });
}

// -------------------------------------------------------------
// Phase R.2.1: P_prim_period emit — struct getter and closure
// construction.
//
// Mirrors cg.cc:237-276 (write_c_prim's P_prim_period case)
// at LLVM level.
//
// Two sub-shapes:
//   1. Closure construction (lvals[0]->type is Type_FUN AND
//      pn->creates non-empty): allocate a closure struct via
//      GC_malloc(sizeof), then store selector at e0 and
//      bound-self at e1.
//   2. Struct getter: resolve the obj's concrete type through
//      `resolve_union_receiver`, find the field index by
//      symbol name in `obj->has`, GEP + Load.
//
// Returns true if it claimed; false → fall through to default.

bool emit_send_period(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_period) return false;
  if (pn->lvals.n < 1 || pn->rvals.n < 4) return false;

  // Resolve the field-selector symbol from rvals[3].
  cchar *symbol = nullptr;
  Vec<Sym *> symbols;
  symbol_info(pn->rvals.v[3], symbols);
  if (symbols.n == 1) symbol = symbols.v[0]->name;
  else if (pn->rvals.v[3]->sym && pn->rvals.v[3]->sym->is_symbol)
    symbol = pn->rvals.v[3]->sym->name;
  if (!symbol) return false;

  Var *dst_var = pn->lvals.v[0];
  if (!dst_var || !dst_var->type) return false;

  // Closure construction case.
  if (dst_var->type->type_kind == Type_FUN && pn->creates &&
      pn->creates->n > 0 && dst_var->type->has.n >= 2) {
    llvm::StructType *closure_struct = sym_to_llvm_struct(dst_var->type);
    if (!closure_struct || closure_struct->getNumElements() < 2) {
      return false;
    }
    const llvm::DataLayout &DL = TheModule->getDataLayout();
    uint64_t sz = DL.getTypeAllocSize(closure_struct);
    llvm::Value *sz_v =
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), sz);
    llvm::Value *dst = Builder->CreateCall(
        get_gc_malloc(), { sz_v },
        cg_get_string(dst_var) ? cg_get_string(dst_var) : "closure");
    put_result(ctx, dst_var, dst);

    // e0 = selector (rvals[3]).
    llvm::Value *sel = value_for_var(ctx, pn->rvals.v[3]);
    if (sel) {
      llvm::Value *e0 = Builder->CreateStructGEP(closure_struct, dst, 0);
      // Coerce selector to e0's field type (opaque-ptr-aware).
      llvm::Type *e0_ty = closure_struct->getElementType(0);
      if (sel->getType() != e0_ty) {
        if (sel->getType()->isPointerTy() && e0_ty->isPointerTy()) {
          // opaque ptrs, no cast.
        } else if (sel->getType()->isIntegerTy() && e0_ty->isPointerTy()) {
          sel = Builder->CreateIntToPtr(sel, e0_ty);
        } else if (sel->getType()->isPointerTy() && e0_ty->isIntegerTy()) {
          sel = Builder->CreatePtrToInt(sel, e0_ty);
        }
      }
      Builder->CreateStore(sel, e0);
    }
    // e1 = bound self (rvals[1]).
    llvm::Value *bound = value_for_var(ctx, pn->rvals.v[1]);
    if (bound) {
      llvm::Value *e1 = Builder->CreateStructGEP(closure_struct, dst, 1);
      llvm::Type *e1_ty = closure_struct->getElementType(1);
      if (bound->getType() != e1_ty) {
        if (bound->getType()->isPointerTy() && e1_ty->isPointerTy()) {
          // opaque ptrs.
        }
      }
      Builder->CreateStore(bound, e1);
    }
    return true;
  }

  // Struct getter case.  Resolve union → concrete component,
  // find field index by name, GEP + Load.
  Sym *obj_sym = pn->rvals.v[1]->type;
  obj_sym = resolve_union_receiver(obj_sym, symbol);
  if (!obj_sym) return false;
  int field_idx = -1;
  for (int i = 0; i < obj_sym->has.n; i++) {
    if (obj_sym->has.v[i] && symbol == obj_sym->has.v[i]->name) {
      field_idx = i;
      break;
    }
  }
  if (field_idx < 0) return false;

  llvm::StructType *obj_struct = sym_to_llvm_struct(obj_sym);
  if (!obj_struct || field_idx >= (int)obj_struct->getNumElements()) {
    return false;
  }
  llvm::Value *obj = value_for_var(ctx, pn->rvals.v[1]);
  if (!obj) return false;
  llvm::Value *gep =
      Builder->CreateStructGEP(obj_struct, obj, field_idx);
  llvm::Type *field_ty = obj_struct->getElementType(field_idx);
  llvm::Value *loaded = Builder->CreateLoad(
      field_ty, gep,
      cg_get_string(dst_var) ? cg_get_string(dst_var) : "");
  // Coerce to dst's expected type if it differs (e.g. dst is
  // a wider integer-typed local).
  llvm::Type *dst_ty = sym_to_llvm_type(dst_var->type);
  if (dst_ty && loaded->getType() != dst_ty) {
    if (loaded->getType()->isPointerTy() && dst_ty->isPointerTy()) {
      // opaque ptrs.
    } else if (loaded->getType()->isIntegerTy() && dst_ty->isIntegerTy()) {
      loaded = Builder->CreateSExtOrTrunc(loaded, dst_ty);
    } else if (loaded->getType()->isPointerTy() && dst_ty->isIntegerTy()) {
      loaded = Builder->CreatePtrToInt(loaded, dst_ty);
    } else if (loaded->getType()->isIntegerTy() && dst_ty->isPointerTy()) {
      loaded = Builder->CreateIntToPtr(loaded, dst_ty);
    }
  }
  put_result(ctx, dst_var, loaded);
  return true;
}

// -------------------------------------------------------------
// Phase R.2.2: P_prim_setter emit — struct setter.
//
// Mirrors cg.cc:277-317.  Resolves symbol from rvals[3],
// looks up the field index in the union-resolved obj type,
// emits GEP + Store, then optionally a value-forward Store
// to lvals[0] (chained-assignment semantics).
// -------------------------------------------------------------

bool emit_send_setter(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_setter) return false;
  if (pn->rvals.n < 5) return false;

  cchar *symbol = nullptr;
  if (pn->rvals.v[3] && pn->rvals.v[3]->sym &&
      pn->rvals.v[3]->sym->is_symbol)
    symbol = pn->rvals.v[3]->sym->name;
  if (!symbol) {
    Vec<Sym *> symbols;
    symbol_info(pn->rvals.v[3], symbols);
    if (symbols.n == 1) symbol = symbols.v[0]->name;
  }
  if (!symbol) return false;

  Sym *obj_sym = pn->rvals.v[1]->type;
  obj_sym = resolve_union_receiver(obj_sym, symbol);
  if (!obj_sym) return false;

  int field_idx = -1;
  for (int i = 0; i < obj_sym->has.n; i++) {
    if (obj_sym->has.v[i] && symbol == obj_sym->has.v[i]->name) {
      field_idx = i;
      break;
    }
  }
  if (field_idx < 0) return false;

  llvm::StructType *obj_struct = sym_to_llvm_struct(obj_sym);
  if (!obj_struct || field_idx >= (int)obj_struct->getNumElements())
    return false;

  llvm::Value *obj = value_for_var(ctx, pn->rvals.v[1]);
  llvm::Value *val = value_for_var(ctx, pn->rvals.v[4]);
  if (!obj || !val) return false;

  // Dead-field elision: skip the store when the field's
  // `has[i]->var` is non-live (issue 026 in cg.cc).
  bool field_live =
      obj_sym->has.v[field_idx]->type &&
      !(obj_sym->has.v[field_idx]->var &&
        !obj_sym->has.v[field_idx]->var->live);
  if (field_live) {
    llvm::Value *gep =
        Builder->CreateStructGEP(obj_struct, obj, field_idx);
    llvm::Type *field_ty = obj_struct->getElementType(field_idx);
    if (val->getType() != field_ty) {
      if (val->getType()->isIntegerTy() && field_ty->isPointerTy()) {
        val = Builder->CreateIntToPtr(val, field_ty);
      } else if (val->getType()->isPointerTy() && field_ty->isIntegerTy()) {
        val = Builder->CreatePtrToInt(val, field_ty);
      } else if (val->getType()->isIntegerTy() && field_ty->isIntegerTy()) {
        val = Builder->CreateSExtOrTrunc(val, field_ty);
      }
    }
    Builder->CreateStore(val, gep);
  }
  // Chained-assignment forward into lvals[0].
  if (pn->lvals.n > 0 && pn->lvals.v[0] && pn->lvals.v[0]->live) {
    put_result(ctx, pn->lvals.v[0], val);
  }
  return true;
}

// -------------------------------------------------------------
// Phase R.2.4 / R.2.5: arithmetic and comparison family.
//
// Direct LLVM ops (CreateAdd/Sub/.../CreateICmp/FCmp) keyed
// on the prim index.  The C backend punts to `_CG_<name>`
// runtime helpers for these, but LLVM emits the IR ops
// directly — that's the standard LLVM idiom and matches
// what the materialized v2 emitter does.
//
// Operand layout from IF1 prim-call convention:
//   rvals[n-3] = lhs (the `__operator` receiver if present)
//   rvals[n-1] = rhs
// Materialized's lower_send_binop reads at these positions
// (cg_normalize_v2.cc:662).
// -------------------------------------------------------------

bool emit_send_binop(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  int idx = pn->prim->index;
  if (pn->rvals.n < 4 || pn->lvals.n < 1) return false;
  Var *lhs_v = pn->rvals.v[pn->rvals.n - 3];
  Var *rhs_v = pn->rvals.v[pn->rvals.n - 1];
  Var *dst_v = pn->lvals.v[0];
  if (!lhs_v || !rhs_v || !dst_v) return false;
  llvm::Value *lhs = value_for_var(ctx, lhs_v);
  llvm::Value *rhs = value_for_var(ctx, rhs_v);
  if (!lhs || !rhs) return false;

  bool is_float = lhs->getType()->isFloatingPointTy();
  // Coerce rhs to lhs type (best-effort int↔int width match).
  if (lhs->getType() != rhs->getType()) {
    if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
      rhs = Builder->CreateSExtOrTrunc(rhs, lhs->getType());
    }
  }

  llvm::Value *res = nullptr;
  switch (idx) {
    case P_prim_add:
      res = is_float ? Builder->CreateFAdd(lhs, rhs)
                     : Builder->CreateAdd(lhs, rhs);
      break;
    case P_prim_subtract:
      res = is_float ? Builder->CreateFSub(lhs, rhs)
                     : Builder->CreateSub(lhs, rhs);
      break;
    case P_prim_mult:
      res = is_float ? Builder->CreateFMul(lhs, rhs)
                     : Builder->CreateMul(lhs, rhs);
      break;
    case P_prim_div:
      res = is_float ? Builder->CreateFDiv(lhs, rhs)
                     : Builder->CreateSDiv(lhs, rhs);
      break;
    case P_prim_mod:
      res = is_float ? Builder->CreateFRem(lhs, rhs)
                     : Builder->CreateSRem(lhs, rhs);
      break;
    case P_prim_lsh:  res = Builder->CreateShl(lhs, rhs);  break;
    case P_prim_rsh:  res = Builder->CreateAShr(lhs, rhs); break;
    case P_prim_and:  res = Builder->CreateAnd(lhs, rhs);  break;
    case P_prim_or:   res = Builder->CreateOr(lhs, rhs);   break;
    case P_prim_xor:  res = Builder->CreateXor(lhs, rhs);  break;
    // Comparison family.
    case P_prim_less:
      res = is_float ? Builder->CreateFCmpOLT(lhs, rhs)
                     : Builder->CreateICmpSLT(lhs, rhs);
      break;
    case P_prim_lessorequal:
      res = is_float ? Builder->CreateFCmpOLE(lhs, rhs)
                     : Builder->CreateICmpSLE(lhs, rhs);
      break;
    case P_prim_greater:
      res = is_float ? Builder->CreateFCmpOGT(lhs, rhs)
                     : Builder->CreateICmpSGT(lhs, rhs);
      break;
    case P_prim_greaterorequal:
      res = is_float ? Builder->CreateFCmpOGE(lhs, rhs)
                     : Builder->CreateICmpSGE(lhs, rhs);
      break;
    case P_prim_equal:
      res = is_float ? Builder->CreateFCmpOEQ(lhs, rhs)
                     : Builder->CreateICmpEQ(lhs, rhs);
      break;
    case P_prim_notequal:
      res = is_float ? Builder->CreateFCmpONE(lhs, rhs)
                     : Builder->CreateICmpNE(lhs, rhs);
      break;
    default:
      return false;
  }
  if (!res) return false;

  // Coerce to dst's expected LLVM type (widen booleans
  // back up to dst's integer width, etc.).
  llvm::Type *dst_ty = sym_to_llvm_type(dst_v->type);
  if (dst_ty && res->getType() != dst_ty) {
    if (res->getType()->isIntegerTy() && dst_ty->isIntegerTy()) {
      res = Builder->CreateZExtOrTrunc(res, dst_ty);
    }
  }
  put_result(ctx, dst_v, res);
  return true;
}

// -------------------------------------------------------------
// Phase R.2.8: P_prim_sizeof and P_prim_sizeof_element —
// compile-time constants via DataLayout.
// -------------------------------------------------------------

bool emit_send_sizeof(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_sizeof &&
      pn->prim->index != P_prim_sizeof_element) return false;
  if (pn->lvals.n < 1) return false;
  int o = (pn->rvals.n > 0 && pn->rvals.v[0] &&
           pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  if (o >= pn->rvals.n) return false;
  Sym *t_sym = pn->rvals.v[o]->type;
  if (!t_sym) return false;
  if (pn->prim->index == P_prim_sizeof_element) {
    if (!t_sym->element) return false;
    t_sym = t_sym->element->type;
  }
  if (!t_sym) return false;
  llvm::Type *t = sym_to_llvm_type(t_sym);
  if (!t) return false;
  uint64_t sz = TheModule->getDataLayout().getTypeAllocSize(t);
  llvm::Value *sz_v = llvm::ConstantInt::get(
      sym_to_llvm_type(pn->lvals.v[0]->type), sz);
  put_result(ctx, pn->lvals.v[0], sz_v);
  return true;
}

// -------------------------------------------------------------
// Phase R.2.6 / R.2.7: index load / store.  Mirrors
// cg.cc:345-403 at LLVM level.  Three sub-shapes per cg.cc:
//   - is_vector: `obj->v[idx]` — GEP through the vector
//     header to its trailing array.
//   - constant-index tuple (Type_RECORD with constant idx
//     Sym): FIELD_LOAD/STORE at that index.
//   - general flat: GEP into the typed-ptr stride.
// String detour (`_CG_char_from_string`) is for INDEX_LOAD
// on string-specializer types.
//
// R.2.6/R.2.7 stub: cover only the general flat case (the
// most common).  is_vector + constant-index-tuple + string
// detour are deferred to follow-ups — they're orthogonal
// once the fundamental GEP+Load/Store is in place.
// -------------------------------------------------------------

bool emit_send_index_load(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_index_object)
    return false;
  if (pn->lvals.n < 1) return false;
  int o = (pn->rvals.n > 0 && pn->rvals.v[0] &&
           pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  if (pn->rvals.n < o + 2) return false;
  llvm::Value *obj = value_for_var(ctx, pn->rvals.v[o]);
  llvm::Value *idx = value_for_var(ctx, pn->rvals.v[o + 1]);
  if (!obj || !idx) return false;
  Var *dst_v = pn->lvals.v[0];
  if (!dst_v || !dst_v->type) return false;
  llvm::Type *elem_ty = sym_to_llvm_type(dst_v->type);
  if (!elem_ty || elem_ty->isVoidTy())
    elem_ty = llvm::Type::getInt64Ty(*TheContext);

  // Coerce idx to i64 (LLVM GEP expects integer index).
  if (idx->getType()->isIntegerTy() &&
      !idx->getType()->isIntegerTy(64)) {
    idx = Builder->CreateSExtOrTrunc(
        idx, llvm::Type::getInt64Ty(*TheContext));
  }
  llvm::Value *gep = Builder->CreateGEP(elem_ty, obj, idx);
  llvm::Value *loaded = Builder->CreateLoad(
      elem_ty, gep,
      cg_get_string(dst_v) ? cg_get_string(dst_v) : "");
  put_result(ctx, dst_v, loaded);
  return true;
}

bool emit_send_index_store(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_set_index_object)
    return false;
  int o = (pn->rvals.n > 0 && pn->rvals.v[0] &&
           pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  if (pn->rvals.n < o + 3) return false;
  llvm::Value *obj = value_for_var(ctx, pn->rvals.v[o]);
  llvm::Value *idx = value_for_var(ctx, pn->rvals.v[o + 1]);
  llvm::Value *val =
      value_for_var(ctx, pn->rvals.v[pn->rvals.n - 1]);
  if (!obj || !idx || !val) return false;
  llvm::Type *elem_ty = val->getType();
  if (idx->getType()->isIntegerTy() &&
      !idx->getType()->isIntegerTy(64)) {
    idx = Builder->CreateSExtOrTrunc(
        idx, llvm::Type::getInt64Ty(*TheContext));
  }
  llvm::Value *gep = Builder->CreateGEP(elem_ty, obj, idx);
  Builder->CreateStore(val, gep);
  return true;
}

// -------------------------------------------------------------
// Phase R.3: P_prim_is / P_prim_isinstance — identity tests.
//
// cg.cc:686-698 emits these as direct C-level pointer
// equality:
//   prim_is(a, b) → (void *)a == (void *)b
//   prim_isinstance(x, nil_type) → x == NULL
// At LLVM level both become `CreateICmpEQ` on opaque ptrs.
// The IF1 SEND shape is `(__primitive, "is"/"isinstance",
// arg0, arg1)` with operands at rvals[2] and rvals[3].
static bool emit_send_is(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_is &&
      pn->prim->index != P_prim_isinstance) return false;
  if (pn->lvals.n < 1 || pn->rvals.n < 4) return false;
  Var *dst_var = pn->lvals.v[0];
  Var *lhs_var = pn->rvals.v[2];
  Var *rhs_var = pn->rvals.v[3];
  if (!dst_var || !lhs_var || !rhs_var) return false;
  llvm::Value *lhs = value_for_var(ctx, lhs_var);
  if (!lhs) return false;
  llvm::Value *rhs = nullptr;
  if (pn->prim->index == P_prim_isinstance &&
      rhs_var->sym == sym_nil_type) {
    // Compare against null.
    llvm::Type *t = lhs->getType();
    if (t->isPointerTy()) {
      rhs = llvm::ConstantPointerNull::get(
          llvm::cast<llvm::PointerType>(t));
    } else {
      rhs = llvm::ConstantInt::get(t, 0);
    }
  } else {
    rhs = value_for_var(ctx, rhs_var);
    if (!rhs) return false;
    // Coerce to lhs's type if needed.
    if (lhs->getType() != rhs->getType()) {
      if (lhs->getType()->isPointerTy() && rhs->getType()->isPointerTy()) {
        // opaque ptrs no-op.
      } else if (lhs->getType()->isPointerTy() && rhs->getType()->isIntegerTy()) {
        rhs = Builder->CreateIntToPtr(rhs, lhs->getType());
      } else if (lhs->getType()->isIntegerTy() && rhs->getType()->isPointerTy()) {
        rhs = Builder->CreatePtrToInt(rhs, lhs->getType());
      }
    }
  }
  llvm::Value *res = Builder->CreateICmpEQ(lhs, rhs,
      cg_get_string(dst_var) ? cg_get_string(dst_var) : "is");
  // Coerce to dst type (typically i1, but FA may widen).
  llvm::Type *dst_ty = sym_to_llvm_type(dst_var->type);
  if (dst_ty && res->getType() != dst_ty) {
    if (dst_ty->isIntegerTy()) {
      res = Builder->CreateZExtOrTrunc(res, dst_ty);
    }
  }
  put_result(ctx, dst_var, res);
  return true;
}

// -------------------------------------------------------------
// Phase R.3: P_prim_strcat — string `+`.  Mirrors
// cg_normalize_v2.cc:lower_send_strcat at the LLVM level.
// The SEND shape is `__operator + s1 s2`, with rvals[o] = s1
// and rvals[o+2] = s2 (rvals[o+1] is the '+' operator
// symbol, skipped).  Calls the linkable `_CG_strcat(a, b)`
// helper in pyc_runtime.c.
static bool emit_send_strcat(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_strcat) return false;
  if (pn->lvals.n < 1) return false;
  int o = (pn->rvals.n > 0 && pn->rvals.v[0] &&
           pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  if (pn->rvals.n < o + 3) return false;
  Var *a_var = pn->rvals.v[o];
  Var *b_var = pn->rvals.v[o + 2];
  Var *dst_var = pn->lvals.v[0];
  if (!a_var || !b_var || !dst_var) return false;
  llvm::Value *a = value_for_var(ctx, a_var);
  llvm::Value *b = value_for_var(ctx, b_var);
  if (!a || !b) return false;
  llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
  llvm::Function *fn = get_runtime_helper(
      "_CG_strcat", ptr_ty, { ptr_ty, ptr_ty });
  llvm::Value *res = Builder->CreateCall(fn, { a, b },
      cg_get_string(dst_var) ? cg_get_string(dst_var) : "concat");
  put_result(ctx, dst_var, res);
  return true;
}

// -------------------------------------------------------------
// Phase R.3: P_prim_len — port of cg.cc:417-427.  The C
// backend emits `_CG_string_len(s)` for strings and
// `_CG_prim_len(_, l)` for lists; both are macros that
// don't link.  We inline the equivalent operations:
//   - String: load i64 at (s - 8).
//   - List: load i64 at (l - SIZEOF_LIST_HEADER), i.e. at
//     (l - 16) since list header is 16 bytes (len + ptr).
static bool emit_send_len(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_len) return false;
  if (pn->lvals.n < 1) return false;
  Var *dst_var = pn->lvals.v[0];
  if (!dst_var) return false;
  int o = (pn->rvals.n > 0 && pn->rvals.v[0] &&
           pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  if (o >= pn->rvals.n) return false;
  Var *obj_var = pn->rvals.v[o];
  if (!obj_var || !obj_var->type) return false;
  llvm::Value *obj = value_for_var(ctx, obj_var);
  if (!obj) return false;

  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *i8 = llvm::Type::getInt8Ty(*TheContext);
  Sym *t = obj_var->type;
  bool is_string = (t == sym_string ||
                    (sym_string && sym_string->specializers.set_in(t)));
  // Header offset: -8 for string, -16 for list (sizeof
  // list header at the C runtime).
  int64_t offset = is_string ? -8 : -16;
  llvm::Value *off = llvm::ConstantInt::get(i64, offset);
  // GEP through i8 to do byte arithmetic.
  llvm::Value *header_ptr = Builder->CreateGEP(i8, obj, off);
  llvm::Value *len = Builder->CreateLoad(i64, header_ptr,
      cg_get_string(dst_var) ? cg_get_string(dst_var) : "len");
  // Coerce to dst type.
  llvm::Type *dst_ty = sym_to_llvm_type(dst_var->type);
  if (dst_ty && len->getType() != dst_ty) {
    if (dst_ty->isIntegerTy()) {
      len = Builder->CreateZExtOrTrunc(len, dst_ty);
    }
  }
  put_result(ctx, dst_var, len);
  return true;
}

// -------------------------------------------------------------
// Phase R.3: P_prim_clone / P_prim_clone_vector — clones a
// prototype struct.  Mirrors cg.cc:429-451 (write_c_prim's
// clone case) at the runtime level.  cg.cc emits the C
// macro `_CG_prim_clone_dst(dst_type, src)` which expands
// to `_CG_prim_primitive_clone_dst(src, sizeof(dst), sizeof(src))`.
// At LLVM level we compute the two sizes via DataLayout and
// call the real runtime function directly.
//
// Vector variant: `_CG_prim_primitive_clone_vector(c, size, v)`.
static bool emit_send_clone(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_clone &&
      pn->prim->index != P_prim_clone_vector) return false;
  if (pn->lvals.n < 1) return false;
  Var *dst_var = pn->lvals.v[0];
  if (!dst_var || !dst_var->type) return false;

  llvm::Type *ret_ty = sym_to_llvm_type(dst_var->type);
  if (!ret_ty) ret_ty = llvm::PointerType::getUnqual(*TheContext);

  int o = (pn->rvals.n > 0 && pn->rvals.v[0] &&
           pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  if (o >= pn->rvals.n) return false;
  Var *src_var = pn->rvals.v[o];
  if (!src_var || !src_var->type) return false;
  llvm::Value *src = value_for_var(ctx, src_var);
  if (!src) return false;

  // Compute size of dst's underlying struct.
  llvm::StructType *dst_struct =
      sym_to_llvm_struct(dst_var->type);
  llvm::StructType *src_struct =
      sym_to_llvm_struct(src_var->type);
  if (!dst_struct && src_struct) dst_struct = src_struct;
  if (!dst_struct) return false;
  if (!src_struct) src_struct = dst_struct;
  const llvm::DataLayout &DL = TheModule->getDataLayout();
  if (!dst_struct->isSized() || !src_struct->isSized()) return false;
  uint64_t dst_sz = DL.getTypeAllocSize(dst_struct);
  uint64_t src_sz = DL.getTypeAllocSize(src_struct);
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);

  if (pn->prim->index == P_prim_clone) {
    // Plain clone: GC_malloc(dst_size) + memcpy from src.
    // The materialized emit uses this approach (no runtime
    // call — the `_CG_prim_clone_dst` macro is static inline
    // in pyc_c_runtime.h, not linkable).
    llvm::Value *new_p = Builder->CreateCall(
        get_gc_malloc(), { llvm::ConstantInt::get(i64, dst_sz) },
        cg_get_string(dst_var) ? cg_get_string(dst_var) : "clone");
    // memcpy(new_p, src, min(dst_size, src_size)).
    uint64_t copy_sz = dst_sz < src_sz ? dst_sz : src_sz;
    llvm::Function *memcpy_fn = llvm::Intrinsic::getOrInsertDeclaration(
        TheModule.get(), llvm::Intrinsic::memcpy,
        { ptr_ty, ptr_ty, i64 });
    Builder->CreateCall(memcpy_fn,
        { new_p, src, llvm::ConstantInt::get(i64, copy_sz),
          llvm::ConstantInt::getFalse(*TheContext) });
    put_result(ctx, dst_var, new_p);
    return true;
  }

  // Vector form: cg.cc:444 emits `_CG_prim_clone_vector(c, v)`
  // which expands to `_CG_prim_primitive_clone_vector(c,
  // sizeof(*(c)), v)`.  At runtime this is the linkable
  // `_CG_prim_clone_vector_runtime(proto, sz, v_extra)` in
  // pyc_runtime.c.
  if (pn->rvals.n <= o + 1) return false;
  llvm::Value *v_extra = value_for_var(ctx, pn->rvals.v[o + 1]);
  if (!v_extra) return false;
  // Coerce v_extra to i64 if needed.
  if (v_extra->getType()->isIntegerTy() &&
      !v_extra->getType()->isIntegerTy(64)) {
    v_extra = Builder->CreateZExtOrTrunc(v_extra, i64);
  }
  llvm::Function *fn = get_runtime_helper(
      "_CG_prim_clone_vector_runtime", ptr_ty,
      { ptr_ty, i64, i64 });
  llvm::Value *res = Builder->CreateCall(
      fn, { src, llvm::ConstantInt::get(i64, src_sz), v_extra },
      cg_get_string(dst_var) ? cg_get_string(dst_var) : "clone_v");
  put_result(ctx, dst_var, res);
  return true;
}

// -------------------------------------------------------------
// Phase R.3: P_prim_new emit — class instantiation.
//
// Mirrors cg.cc:404-409 (write_c_prim's P_prim_new case).
// C-level emit is `lvals[0] = _CG_prim_new(<type>)`, which
// expands to `GC_MALLOC(sizeof(struct _CG_sNNN))` cast to the
// dst type.  At LLVM level we just call `GC_malloc(sizeof)`
// directly.
//
// Without this, class-prototype globals stay null and any
// `class A: x = 2`-style attribute store crashes — exactly
// the segfault pattern that dominates the VIEW2 EXEC failure
// list.
static bool emit_send_new(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_new) return false;
  if (pn->lvals.n < 1) return false;
  Var *dst_var = pn->lvals.v[0];
  if (!dst_var || !dst_var->type) return false;

  // dst's type is typically Type_RECORD with the struct as
  // the element of a ptr wrapper.  Find the underlying struct
  // for sizeof.
  Sym *t_sym = dst_var->type;
  llvm::StructType *struct_ty = nullptr;
  if (t_sym->type_kind == Type_RECORD) {
    struct_ty = sym_to_llvm_struct(t_sym);
  } else if (t_sym->element) {
    // Fallback: maybe type is wrapped in something else.
    struct_ty = sym_to_llvm_struct(t_sym->element->type);
  }
  if (!struct_ty) return false;
  const llvm::DataLayout &DL = TheModule->getDataLayout();
  if (!struct_ty->isSized()) return false;
  uint64_t sz = DL.getTypeAllocSize(struct_ty);

  llvm::Value *sz_v = llvm::ConstantInt::get(
      llvm::Type::getInt64Ty(*TheContext), sz);
  llvm::Value *result = Builder->CreateCall(
      get_gc_malloc(), { sz_v },
      cg_get_string(dst_var) ? cg_get_string(dst_var) : "new");
  put_result(ctx, dst_var, result);
  return true;
}

// -------------------------------------------------------------
// Phase R.2.3: P_prim_make emit — tuple and list literals.
//
// Mirrors cg.cc:208-236 (write_c_prim's P_prim_make case)
// adapted to LLVM IR.  The C backend uses macro-expanded
// `_CG_prim_tuple(<type>, n)` where the C preprocessor
// computes sizeof; at the LLVM level we use the pyc runtime's
// `_CG_prim_tuple_list_internal(size, n)` variant which takes
// the size as a runtime arg.  Both variants share the same
// underlying allocator in libpyc_runtime.a, so the LLVM-emitted
// calls link cleanly against C-compiled call sites.
//
// Three sub-shapes (mirroring cg.cc:209-235):
//   1. Tuple (rvals[2]->sym in sym_tuple->specializers):
//      `_CG_prim_tuple_list_internal(sizeof(<struct>), n)` →
//      dst, then per-field FIELD_STORE for rvals[3..].
//   2. List with Type_RECORD lvals[0]->type (struct-shape
//      list): same as tuple — same helper, same FIELD_STOREs.
//      cg.cc gotos Ltuple via `listish_tuple=true`.
//   3. Flat list (rvals[2]->sym in sym_list->specializers
//      or is_vector, and lvals[0]->type is NOT Type_RECORD):
//      `_CG_prim_list_internal(sizeof(<elem>), n)` → dst,
//      then per-index INDEX_STORE for rvals[3..].
//
// Returns true if it handled the PNode; false otherwise.  The
// R.1.4 dispatcher will pair this with the fall-through to
// emit_send_call.

bool emit_send_make(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_make) return false;
  if (pn->lvals.n < 1 || pn->rvals.n < 3) return false;
  if (!pn->rvals.v[2] || !pn->rvals.v[2]->sym) return false;

  Var *dst_var = pn->lvals.v[0];
  if (!dst_var || !dst_var->type) return false;

  Sym *target = pn->rvals.v[2]->sym;
  bool is_tuple = sym_tuple && sym_tuple->specializers.set_in(target);
  bool is_list_or_vec =
      (sym_list && sym_list->specializers.set_in(target)) ||
      target->is_vector;

  if (!is_tuple && !is_list_or_vec) return false;

  // Decide shape.  Type_RECORD lvals[0]->type means the list
  // is being held by struct (mixed-type tuple-list); else flat.
  Sym *dst_ty = dst_var->type;
  bool is_struct_shape =
      is_tuple ||
      (is_list_or_vec && dst_ty->type_kind == Type_RECORD);

  int n_elements = pn->rvals.n - 3;

  llvm::Type *i32 = llvm::Type::getInt32Ty(*TheContext);
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
  const llvm::DataLayout &DL = TheModule->getDataLayout();

  if (is_struct_shape) {
    // Stage 1: allocate via `_CG_prim_tuple_list_internal`.
    // Element type for the helper's size arg is the struct
    // itself (sizeof entire record, like cg.cc's
    // `_CG_prim_tuple(<type>, n)`).
    llvm::StructType *struct_ty = sym_to_llvm_struct(dst_ty);
    if (!struct_ty) return false;
    uint64_t sz_bytes = DL.getTypeAllocSize(struct_ty);
    llvm::Value *size_arg = llvm::ConstantInt::get(i32, sz_bytes);
    // pyc convention: n+1 for over-allocation when the lvals[0]
    // is a Type_RECORD-shaped list; tuples don't need the +1
    // semantically but cg.cc emits rvals.n - 2 in both cases,
    // which IS n+1 (n_elements + 1 marker).  Match the C
    // backend exactly.
    int alloc_count = pn->rvals.n - 2;
    llvm::Value *n_arg = llvm::ConstantInt::get(i32, alloc_count);
    llvm::Function *helper = get_runtime_helper(
        "_CG_prim_tuple_list_internal", ptr_ty, { i32, i32 });
    llvm::Value *alloc =
        Builder->CreateCall(helper, { size_arg, n_arg },
                             cg_get_string(dst_var)
                                 ? cg_get_string(dst_var) : "tmp");
    put_result(ctx, dst_var, alloc);

    // Per-field FIELD_STORE.  GEP into the struct slots
    // and CreateStore each rval.
    int n_fields = struct_ty->getNumElements();
    for (int i = 3; i < pn->rvals.n; i++) {
      int field_idx = i - 3;
      if (field_idx >= n_fields) break;
      Var *val_var = pn->rvals.v[i];
      if (!val_var) continue;
      llvm::Value *val = value_for_var(ctx, val_var);
      if (!val) continue;
      llvm::Value *gep = Builder->CreateStructGEP(
          struct_ty, alloc, field_idx);
      Builder->CreateStore(val, gep);
    }

    // Stage 2 conversion to runtime list shape — only for
    // the list-struct case, NOT plain tuples.  Mirrors
    // cg_normalize_v2's `_CG_to_list_runtime` lowering.
    // cg.cc skips this because at C level the runtime
    // macro handles the conversion inline; at LLVM level
    // we need an explicit helper call.
    if (is_list_or_vec) {
      llvm::Value *size2 = llvm::ConstantInt::get(i32, sz_bytes);
      llvm::Value *n_semantic =
          llvm::ConstantInt::get(i32, n_elements);
      llvm::Function *conv = get_runtime_helper(
          "_CG_to_list_runtime", ptr_ty,
          { ptr_ty, i32, i32 });
      llvm::Value *final = Builder->CreateCall(
          conv, { alloc, size2, n_semantic },
          cg_get_string(dst_var) ? cg_get_string(dst_var) : "dst");
      put_result(ctx, dst_var, final);
    }
    return true;
  }

  // Flat-list path.  Allocate via `_CG_prim_list_internal`
  // (the sized variant of cg.cc's `_CG_prim_list(<elem>, n)`).
  // Element type comes from dst's CGv2Type element if
  // available; else int64 (matches the materialized fallback
  // in lower_send_alloc).
  Sym *elem_sym = nullptr;
  if (dst_ty && dst_ty->element) elem_sym = dst_ty->element->type;
  llvm::Type *elem_ty = elem_sym ? sym_to_llvm_type(elem_sym) : i64;
  if (!elem_ty || elem_ty->isVoidTy()) elem_ty = i64;
  uint64_t elem_sz = DL.getTypeAllocSize(elem_ty);
  llvm::Value *size_arg = llvm::ConstantInt::get(i32, elem_sz);
  llvm::Value *n_arg = llvm::ConstantInt::get(i32, n_elements);
  llvm::Function *helper = get_runtime_helper(
      "_CG_prim_tuple_list_internal", ptr_ty, { i32, i32 });
  llvm::Value *alloc =
      Builder->CreateCall(helper, { size_arg, n_arg },
                           cg_get_string(dst_var)
                               ? cg_get_string(dst_var) : "list");
  put_result(ctx, dst_var, alloc);

  // Per-index INDEX_STORE: GEP into the typed pointer's
  // element stride and store each rval.
  for (int i = 3; i < pn->rvals.n; i++) {
    Var *val_var = pn->rvals.v[i];
    if (!val_var) continue;
    llvm::Value *val = value_for_var(ctx, val_var);
    if (!val) continue;
    llvm::Value *idx = llvm::ConstantInt::get(i64, i - 3);
    llvm::Value *gep = Builder->CreateGEP(elem_ty, alloc, idx);
    Builder->CreateStore(val, gep);
  }
  return true;
}

// -------------------------------------------------------------
// is_const_folded_send — port of cg.cc:768.  Skip SENDs whose
// single lval is a known constant: consumers inline the
// literal via value_for_var's is_constant path.
// -------------------------------------------------------------

bool is_const_folded_send(PNode *pn) {
  if (!pn || !pn->code || pn->code->kind != Code_SEND) return false;
  if (!pn->prim || pn->prim->nonfunctional) return false;
  if (pn->lvals.n != 1) return false;
  Var *lv = pn->lvals.v[0];
  if (!lv) return false;
  return get_constant(lv) != nullptr;
}

// -------------------------------------------------------------
// Code_MOVE emit.  Port of cg.cc:simple_move (line 537) at LLVM
// level — but in SSA form an LLVM "move" is just binding the
// rhs's value to the lhs in `ctx.var_map`.  No actual store/
// load unless the lhs needs an alloca (phi destination), which
// the materialized emitter handles via its alloca pre-pass.
// For R.1.3 we skip the alloca path — relying on SSA binding
// alone may miss phi-typed locals but covers the dominant case
// and matches what cg.cc emits at the C-source level (`a = b`
// is structurally just an alias when downstream reads).
// -------------------------------------------------------------

void emit_move(EmitCtx &ctx, PNode *pn) {
  if (!pn) return;
  int n = pn->lvals.n < pn->rvals.n ? pn->lvals.n : pn->rvals.n;
  for (int i = 0; i < n; i++) {
    Var *lhs = pn->lvals.v[i];
    Var *rhs = pn->rvals.v[i];
    if (!lhs || !rhs) continue;
    if (!lhs->live) continue;
    if (lhs->sym && lhs->sym->type_kind) continue;
    if (rhs->sym && rhs->sym->type_kind) continue;
    if (rhs->type == sym_void || lhs->type == sym_void) continue;
    if (lhs->type == sym_nil_type) continue;
    if (get_constant(lhs)) continue;
    // Alloca consolidation is handled exclusively by
    // discover_phi_targets's union-find pre-pass.  Doing
    // it at emit time would pollute alloca_map with
    // non-allocable Vars (constants, function refs) which
    // then load-from-uninitialized-slot instead of being
    // materialized.
    llvm::Value *src = value_for_var(ctx, rhs);
    if (!src) continue;
    // Global Var as lhs: emit a real store to the
    // module-level GlobalVariable.  Without this, stores
    // like `g = expr` are dropped and subsequent loads
    // return null.
    if (llvm::GlobalVariable *gv = g_var_to_global.get(lhs)) {
      llvm::Type *gv_ty = sym_to_llvm_type(lhs->type);
      if (gv_ty && src->getType() != gv_ty) {
        if (src->getType()->isPointerTy() && gv_ty->isPointerTy()) {
          // opaque-ptr no-op.
        } else if (src->getType()->isIntegerTy() && gv_ty->isIntegerTy()) {
          src = Builder->CreateSExtOrTrunc(src, gv_ty);
        }
      }
      Builder->CreateStore(src, gv);
    }
    put_result(ctx, lhs, src);
  }
}

// Forward decls for the SEND dispatcher.
bool emit_send_make(EmitCtx &ctx, PNode *pn);
bool emit_send_period(EmitCtx &ctx, PNode *pn);
bool emit_send_setter(EmitCtx &ctx, PNode *pn);
bool emit_send_binop(EmitCtx &ctx, PNode *pn);
bool emit_send_sizeof(EmitCtx &ctx, PNode *pn);
bool emit_send_index_load(EmitCtx &ctx, PNode *pn);
bool emit_send_index_store(EmitCtx &ctx, PNode *pn);
bool emit_send_primitive(EmitCtx &ctx, PNode *pn);
static bool emit_send_new(EmitCtx &ctx, PNode *pn);
static bool emit_send_clone(EmitCtx &ctx, PNode *pn);
static bool emit_send_len(EmitCtx &ctx, PNode *pn);
static bool emit_send_strcat(EmitCtx &ctx, PNode *pn);
static bool emit_send_is(EmitCtx &ctx, PNode *pn);
void emit_send(EmitCtx &ctx, PNode *pn);
void emit_send_call(EmitCtx &ctx, PNode *pn);

// -------------------------------------------------------------
// Code_SEND dispatcher.  Mirrors cg.cc:write_send (line 655) +
// write_c_prim (line 198) at LLVM level.
//
// Order:
//   1. is_const_folded_send → skip entirely.
//   2. P_prim_reply is handled as a terminator, not here.
//   3. If prim, route to per-prim handler.  Each handler
//      returns bool — true claims, false falls through.
//   4. Fall through to emit_send_call (regular fn call).
//
// Phase R.1.4: skeleton with three prim handlers wired
// (make from R.2.3, plus stub fallback for unrecognized
// prims that routes to `_CG_<name>` via the runtime
// helper).  R.2.1+ ports the remaining prim cases.
// -------------------------------------------------------------

// -------------------------------------------------------------
// Phase R.2.11: P_prim_primitive emit — name-based dispatch.
//
// Mirrors cg.cc:484-517.  The actual primitive name lives in
// `rvals[1]->sym->name` (or `->constant`); rvals[2..] are
// the args.  Default route: `_CG_<name>(args)`.  Specialized
// dispatch for `__pyc_c_call__` (FFI — name at rvals[3],
// arg pairs from rvals[5..]) is needed when the analyzer
// doesn't statically pick a target.
// -------------------------------------------------------------

bool emit_send_primitive(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_primitive) return false;
  if (pn->rvals.n < 2 || !pn->rvals.v[1] || !pn->rvals.v[1]->sym) return false;

  cchar *name = pn->rvals.v[1]->sym->name;
  if (!name) name = pn->rvals.v[1]->sym->constant;
  if (!name) return false;

  // __pyc_c_call__(ret_type, fn_name_const, (arg_type, arg_value)*)
  // rvals layout:
  //   [0] sym_primitive marker
  //   [1] "__pyc_c_call__" name
  //   [2] ret type sym
  //   [3] fn_name as constant-string Sym
  //   [4,5] (arg_type, arg_value) — args at odd positions
  if (strcmp(name, "__pyc_c_call__") == 0) {
    if (pn->rvals.n < 4) return false;
    Var *name_var = pn->rvals.v[3];
    if (!name_var || !name_var->sym || !name_var->sym->constant)
      return false;
    cchar *fn_name = name_var->sym->constant;

    llvm::Type *ret_ty = llvm::Type::getVoidTy(*TheContext);
    if (pn->lvals.n > 0 && pn->lvals.v[0] && pn->lvals.v[0]->type) {
      ret_ty = sym_to_llvm_type(pn->lvals.v[0]->type);
      if (!ret_ty) ret_ty = llvm::Type::getVoidTy(*TheContext);
    }

    std::vector<llvm::Type *> param_tys;
    std::vector<llvm::Value *> args;
    for (int i = 5; i < pn->rvals.n; i += 2) {
      Var *v = pn->rvals.v[i];
      if (!v) continue;
      llvm::Value *val = value_for_var(ctx, v);
      if (!val) return false;
      args.push_back(val);
      param_tys.push_back(val->getType());
    }
    llvm::Function *fn = get_runtime_helper(fn_name, ret_ty, param_tys);
    llvm::Value *res = Builder->CreateCall(fn, args);
    if (pn->lvals.n > 0 && pn->lvals.v[0] && !ret_ty->isVoidTy()) {
      put_result(ctx, pn->lvals.v[0], res);
    }
    return true;
  }

  // Default named-prim route: `_CG_<name>(rvals[2..])`.
  char helper[256];
  snprintf(helper, sizeof(helper), "_CG_%s", name);

  std::vector<llvm::Type *> param_tys;
  std::vector<llvm::Value *> args;
  for (int i = 2; i < pn->rvals.n; i++) {
    Var *v = pn->rvals.v[i];
    if (!v) continue;
    llvm::Value *val = value_for_var(ctx, v);
    if (!val) return false;
    args.push_back(val);
    param_tys.push_back(val->getType());
  }

  llvm::Type *ret_ty = llvm::Type::getVoidTy(*TheContext);
  if (pn->lvals.n > 0 && pn->lvals.v[0] && pn->lvals.v[0]->type) {
    ret_ty = sym_to_llvm_type(pn->lvals.v[0]->type);
    if (!ret_ty) ret_ty = llvm::Type::getVoidTy(*TheContext);
  }

  llvm::Function *fn = get_runtime_helper(helper, ret_ty, param_tys);
  llvm::Value *res = Builder->CreateCall(fn, args);
  if (pn->lvals.n > 0 && pn->lvals.v[0] && !ret_ty->isVoidTy()) {
    put_result(ctx, pn->lvals.v[0], res);
  }
  return true;
}

bool emit_send_default_prim(EmitCtx &ctx, PNode *pn) {
  // Port of write_c_prim's tail fallback (`_CG_<name>` route).
  // Used for any prim that doesn't have a structural handler.
  // Note: P_prim_primitive goes through emit_send_primitive,
  // not here — it has its own name-from-rvals[1] dispatch.
  if (!pn || !pn->prim || !pn->prim->name) return false;
  cchar *name = pn->prim->name;
  char helper[256];
  snprintf(helper, sizeof(helper), "_CG_%s", name);

  // Collect args.  cg.cc:706-715: `start = 2 if rvals[0]==
  // sym_primitive else 1`.
  int start = 1;
  if (pn->rvals.n > 0 && pn->rvals.v[0] &&
      pn->rvals.v[0]->sym == sym_primitive) start = 2;

  std::vector<llvm::Type *> param_tys;
  std::vector<llvm::Value *> args;
  for (int i = start; i < pn->rvals.n; i++) {
    Var *v = pn->rvals.v[i];
    if (!v) continue;
    llvm::Value *val = value_for_var(ctx, v);
    if (!val) return false;
    args.push_back(val);
    param_tys.push_back(val->getType());
  }

  llvm::Type *ret_ty = llvm::Type::getVoidTy(*TheContext);
  if (pn->lvals.n > 0 && pn->lvals.v[0] && pn->lvals.v[0]->type) {
    ret_ty = sym_to_llvm_type(pn->lvals.v[0]->type);
    if (!ret_ty) ret_ty = llvm::Type::getVoidTy(*TheContext);
  }

  llvm::Function *fn =
      get_runtime_helper(helper, ret_ty, param_tys);
  llvm::Value *res = Builder->CreateCall(fn, args);
  if (pn->lvals.n > 0 && pn->lvals.v[0] && !ret_ty->isVoidTy()) {
    put_result(ctx, pn->lvals.v[0], res);
  }
  return true;
}

void emit_send(EmitCtx &ctx, PNode *pn) {
  if (!pn) return;
  if (is_const_folded_send(pn)) return;
  if (pn->prim) {
    int idx = pn->prim->index;
    // P_prim_reply is the function-return terminator —
    // handled in emit_block_terminator, not here.
    if (idx == P_prim_reply) return;
    // Structural prim handlers (R.2.x landings).  Each
    // returns true if it claimed; false → fall through.
    // Arithmetic + comparison family — direct LLVM ops.
    if (emit_send_binop(ctx, pn)) return;
    if (emit_send_period(ctx, pn)) return;
    if (emit_send_setter(ctx, pn)) return;
    if (emit_send_new(ctx, pn)) return;
    if (emit_send_clone(ctx, pn)) return;
    if (emit_send_len(ctx, pn)) return;
    if (emit_send_strcat(ctx, pn)) return;
    if (emit_send_is(ctx, pn)) return;
    if (emit_send_make(ctx, pn)) return;
    if (emit_send_index_load(ctx, pn)) return;
    if (emit_send_index_store(ctx, pn)) return;
    if (emit_send_sizeof(ctx, pn)) return;
    // P_prim_primitive — name-based dispatch from rvals[1].
    if (emit_send_primitive(ctx, pn)) return;
    // Default fallback: `_CG_<name>(args)`.
    if (emit_send_default_prim(ctx, pn)) return;
    return;
  }
  // No prim — regular function call.
  emit_send_call(ctx, pn);
}

// -------------------------------------------------------------
// emit_send_call — regular function call.  R.2.12: full
// MPosition-aware arg routing + closure unpack FIELD_LOAD
// per formal.  Mirrors cg.cc:write_send (lines 717-735) +
// cg_normalize_v2.cc:lower_send_call (lines 1565-1672).
// -------------------------------------------------------------

void emit_send_call(EmitCtx &ctx, PNode *pn) {
  if (!pn || !ctx.fn) return;
  Vec<Fun *> *callees = ctx.fn->calls.get(pn);
  if (!callees || callees->n != 1) return;
  Fun *target = callees->v[0];
  if (!target || !target->cg_string) return;
  llvm::Function *target_fn =
      TheModule->getFunction(target->cg_string);
  if (!target_fn) return;

  // Closure detection: rvals[0] is a closure receiver when
  // its type is Type_FUN with has.n ≥ 2 (is_closure_var
  // predicate from codegen_common).  Unpack via FIELD_LOAD
  // per closure-struct field that maps to a formal.
  Var *v0 = pn->rvals.n > 0 ? pn->rvals.v[0] : nullptr;
  bool v0_is_closure = v0 && is_closure_var(v0);
  llvm::Value *closure = nullptr;
  llvm::StructType *closure_struct = nullptr;
  if (v0_is_closure) {
    closure = value_for_var(ctx, v0);
    closure_struct = sym_to_llvm_struct(v0->type);
  }

  std::vector<llvm::Value *> args;

  // MPosition arg routing.  Walk target->positional_arg_
  // positions in order; for each formal that passes the
  // live + non-fun filter, compute the rval index as
  // `Position2int(p->pos[0]) - 1`.  For closures, indices
  // within the closure's field range emit a FIELD_LOAD
  // unpack; beyond that, shift by `has.n - 1`.
  for (MPosition *p : target->positional_arg_positions) {
    Var *formal = target->args.get(p);
    if (!formal || !formal->live) continue;
    if (formal->type && formal->type->is_fun) continue;
    if (p->pos.n > 1) continue;
    if (p->pos.n == 0) continue;
    int i = (int)Position2int(p->pos.v[0]) - 1;

    if (v0_is_closure && v0->type) {
      if (i < v0->type->has.n) {
        // Unpack closure field i: GEP + Load.
        if (closure && closure_struct && i >= 0 &&
            i < (int)closure_struct->getNumElements()) {
          llvm::Value *gep =
              Builder->CreateStructGEP(closure_struct, closure, i);
          llvm::Type *field_ty =
              closure_struct->getElementType(i);
          llvm::Value *arg =
              Builder->CreateLoad(field_ty, gep, "clo_arg");
          args.push_back(arg);
        }
        continue;
      } else {
        i -= v0->type->has.n - 1;
      }
    }
    if (i < 0 || i >= pn->rvals.n) continue;
    Var *actual = pn->rvals.v[i];
    if (!actual) continue;
    llvm::Value *val = value_for_var(ctx, actual);
    if (!val) return;
    args.push_back(val);
  }

  // Coerce arg types to match target's declared param types
  // (defensive — opaque ptr casts no-op, int widths get
  // SExtOrTrunc).
  int ai = 0;
  for (llvm::Argument &fp : target_fn->args()) {
    if (ai >= (int)args.size()) break;
    llvm::Type *want = fp.getType();
    llvm::Value *have = args[ai];
    if (have->getType() != want) {
      if (have->getType()->isIntegerTy() && want->isIntegerTy()) {
        args[ai] = Builder->CreateSExtOrTrunc(have, want);
      } else if (have->getType()->isPointerTy() && want->isIntegerTy()) {
        args[ai] = Builder->CreatePtrToInt(have, want);
      } else if (have->getType()->isIntegerTy() && want->isPointerTy()) {
        args[ai] = Builder->CreateIntToPtr(have, want);
      }
    }
    ai++;
  }
  // Trim if we somehow produced more args than the target wants.
  if ((int)args.size() > (int)target_fn->arg_size()) {
    args.resize(target_fn->arg_size());
  }

  llvm::Value *res = Builder->CreateCall(target_fn, args);
  if (pn->lvals.n > 0 && pn->lvals.v[0]) {
    Var *dst = pn->lvals.v[0];
    if (!target_fn->getReturnType()->isVoidTy()) {
      put_result(ctx, dst, res);
    }
  }
}

// -------------------------------------------------------------
// Block-terminator emit.  Reads the closer PNode's Code kind
// and emits the LLVM terminator (CreateBr / CreateCondBr /
// CreateRet / CreateUnreachable).  Mirrors cg.cc's per-Code
// branch logic at write_c_pnode lines 797-843.
// -------------------------------------------------------------

void emit_block_terminator(EmitCtx &ctx, PNode *closer) {
  if (!closer || !closer->code) {
    Builder->CreateUnreachable();
    return;
  }
  Code *cd = closer->code;
  switch (cd->kind) {
    case Code_GOTO: {
      llvm::BasicBlock *succ_bb = nullptr;
      if (closer->cfg_succ.n > 0) {
        succ_bb = ctx.label_bb.get(closer->cfg_succ.v[0]);
      }
      if (succ_bb) Builder->CreateBr(succ_bb);
      else Builder->CreateUnreachable();
      break;
    }
    case Code_IF: {
      llvm::Value *cond = nullptr;
      if (closer->rvals.n > 0)
        cond = value_for_var(ctx, closer->rvals.v[0]);
      // Truncate to i1 if needed.
      if (cond && !cond->getType()->isIntegerTy(1)) {
        cond = Builder->CreateICmpNE(cond,
            llvm::Constant::getNullValue(cond->getType()));
      }
      llvm::BasicBlock *t_bb = nullptr, *f_bb = nullptr;
      if (closer->cfg_succ.n > 0) t_bb = ctx.label_bb.get(closer->cfg_succ.v[0]);
      if (closer->cfg_succ.n > 1) f_bb = ctx.label_bb.get(closer->cfg_succ.v[1]);
      if (cond && t_bb && f_bb) Builder->CreateCondBr(cond, t_bb, f_bb);
      else Builder->CreateUnreachable();
      break;
    }
    case Code_SEND: {
      if (closer->prim && closer->prim->index == P_prim_reply) {
        llvm::Type *ret_ty = ctx.llvm_fn->getReturnType();
        if (ret_ty->isVoidTy()) {
          Builder->CreateRetVoid();
        } else {
          // Non-void fn — try to read the return value from
          // rvals[3]; fall back to null if not available so
          // the verifier accepts the function (real semantics
          // can't be honored here, but it's better than
          // shipping a non-terminated BB).
          llvm::Value *rv = nullptr;
          if (closer->rvals.n >= 4) {
            rv = value_for_var(ctx, closer->rvals.v[3]);
          }
          if (rv) {
            if (rv->getType() != ret_ty) {
              if (rv->getType()->isPointerTy() && ret_ty->isPointerTy()) {
                // opaque ptrs.
              } else if (rv->getType()->isIntegerTy() &&
                         ret_ty->isIntegerTy()) {
                rv = Builder->CreateSExtOrTrunc(rv, ret_ty);
              } else if (rv->getType()->isPointerTy() &&
                         ret_ty->isIntegerTy()) {
                rv = Builder->CreatePtrToInt(rv, ret_ty);
              } else if (rv->getType()->isIntegerTy() &&
                         ret_ty->isPointerTy()) {
                rv = Builder->CreateIntToPtr(rv, ret_ty);
              }
            }
            Builder->CreateRet(rv);
          } else {
            Builder->CreateRet(llvm::Constant::getNullValue(ret_ty));
          }
        }
      } else if (closer->cfg_succ.n > 0) {
        // Non-reply SEND that's a block closer — fall
        // through to the next block.
        llvm::BasicBlock *succ_bb =
            ctx.label_bb.get(closer->cfg_succ.v[0]);
        if (succ_bb) Builder->CreateBr(succ_bb);
        else Builder->CreateUnreachable();
      } else {
        Builder->CreateUnreachable();
      }
      break;
    }
    default: {
      if (closer->cfg_succ.n > 0) {
        llvm::BasicBlock *succ_bb =
            ctx.label_bb.get(closer->cfg_succ.v[0]);
        if (succ_bb) Builder->CreateBr(succ_bb);
        else Builder->CreateUnreachable();
      } else if (ctx.llvm_fn->getReturnType()->isVoidTy()) {
        Builder->CreateRetVoid();
      } else {
        Builder->CreateUnreachable();
      }
      break;
    }
  }
}

// -------------------------------------------------------------
// Alloca pre-pass: every distinct phi-target Var gets an
// alloca slot in the entry block.  Mirrors the materialized
// emit's pre-pass at cg_ir_v2_emit_llvm.cc:1467-1488.
//
// Phi targets are `pn->phi[i]->lvals[0]` and `pn->phy[i]->
// lvals[*]` — Vars whose value flows across cfg edges via
// SSU MOVE PNodes.  Without alloca slots, SSA-bind alone
// can't carry their state across loop back-edges or join
// blocks: each block sees the initial value forever.
// -------------------------------------------------------------

// Union-find for Var equivalence classes.  Two Vars are
// equivalent when linked by a Code_MOVE or by an SSU
// phi/phy pair.  After building the classes we allocate
// one alloca per class containing a phi target.
namespace {
struct VarUF {
  Map<Var *, Var *> parent;
  Var *find(Var *v) {
    Var *p = parent.get(v);
    if (!p || p == v) {
      parent.put(v, v);
      return v;
    }
    Var *root = find(p);
    if (root != p) parent.put(v, root);
    return root;
  }
  void unite(Var *a, Var *b) {
    Var *ra = find(a);
    Var *rb = find(b);
    if (ra != rb) parent.put(ra, rb);
  }
};
}  // anonymous namespace

void discover_phi_targets(EmitCtx &ctx, Fun *f) {
  if (!f || !f->entry || !ctx.llvm_fn) return;
  // Pass A: walk every PNode reachable from f->entry,
  // collect phi/phy targets AND build a Var-equivalence
  // union-find by uniting:
  //   - Code_MOVE: lhs[i] ~ rhs[i]
  //   - phi PNode: lvals[0] ~ each rvals[i]
  //   - phy PNode: each lvals[i] ~ rvals[0]
  // The classes capture chains of SSU Vars representing the
  // same logical variable; allocating one alloca per class
  // makes loop-body reads load from the same slot the
  // back-edge writes to.
  Vec<PNode *> stack;
  Vec<PNode *> seen;
  stack.add(f->entry);
  seen.set_add(f->entry);
  VarUF uf;
  Vec<Var *> phi_targets;
  Vec<Var *> seen_targets;
  while (stack.n) {
    PNode *cur = stack.pop();
    auto allocable = [](Var *v) -> bool {
      if (!v || !v->sym) return false;
      if (v->sym->is_constant) return false;
      if (v->sym->is_fun) return false;
      if (v->sym->is_symbol) return false;
      if (g_var_to_global.get(v)) return false;
      return true;
    };
    if (cur->code && cur->code->kind == Code_MOVE) {
      int n = cur->lvals.n < cur->rvals.n ? cur->lvals.n : cur->rvals.n;
      for (int i = 0; i < n; i++) {
        Var *lhs = cur->lvals.v[i];
        Var *rhs = cur->rvals.v[i];
        if (lhs && rhs && lhs->live && allocable(lhs) && allocable(rhs))
          uf.unite(lhs, rhs);
      }
    }
    for (PNode *p : cur->phi) {
      if (!p || p->lvals.n < 1) continue;
      Var *lv = p->lvals.v[0];
      if (lv && lv->live && allocable(lv) && seen_targets.set_add(lv))
        phi_targets.add(lv);
      for (int i = 0; i < p->rvals.n; i++) {
        Var *rv = p->rvals.v[i];
        if (lv && rv && allocable(lv) && allocable(rv)) uf.unite(lv, rv);
      }
    }
    for (PNode *p : cur->phy) {
      if (!p || p->rvals.n < 1) continue;
      Var *rv = p->rvals.v[0];
      for (int i = 0; i < p->lvals.n; i++) {
        Var *lv = p->lvals.v[i];
        if (lv && lv->live && allocable(lv) && seen_targets.set_add(lv))
          phi_targets.add(lv);
        if (lv && rv && allocable(lv) && allocable(rv)) uf.unite(lv, rv);
      }
    }
    for (PNode *s : cur->cfg_succ) {
      if (s && seen.set_add(s)) stack.add(s);
    }
  }
  if (phi_targets.n == 0) return;

  // Pass B: allocate one alloca per phi-target equivalence
  // class (representative = uf.find()).  All Vars in the
  // class share the same slot via ctx.alloca_map.
  llvm::BasicBlock *entry_bb = ctx.label_bb.get(f->entry);
  if (!entry_bb) return;
  llvm::IRBuilder<>::InsertPointGuard guard(*Builder);
  if (entry_bb->empty()) {
    Builder->SetInsertPoint(entry_bb);
  } else {
    Builder->SetInsertPoint(&entry_bb->front());
  }
  Map<Var *, llvm::AllocaInst *> class_slot;  // root → alloca
  for (Var *v : phi_targets) {
    Var *root = uf.find(v);
    llvm::AllocaInst *slot = class_slot.get(root);
    if (!slot) {
      if (!v->type) continue;
      llvm::Type *t = sym_to_llvm_type(v->type);
      if (!t || t->isVoidTy()) continue;
      cchar *name = cg_get_string(v);
      slot = Builder->CreateAlloca(t, nullptr, name ? name : "");
      class_slot.put(root, slot);
    }
    ctx.alloca_map.put(v, slot);
  }
  // Pass C: also map every Var in any allocated class to
  // the shared slot.  Walk the union-find's parent map and
  // assign.
  Vec<Var *> all_vars;
  uf.parent.get_keys(all_vars);
  for (Var *v : all_vars) {
    if (ctx.alloca_map.get(v)) continue;
    Var *root = uf.find(v);
    if (llvm::AllocaInst *slot = class_slot.get(root)) {
      ctx.alloca_map.put(v, slot);
    }
  }
}

// -------------------------------------------------------------
// Pre-pass: allocate an llvm::BasicBlock for the Fun's entry
// PNode and for each Code_LABEL PNode reachable via cfg_succ.
// Registers in `ctx.label_bb`.  BFS — mirrors cg.cc's
// label-discovery via write_c_pnode's recursion + done-set.
// -------------------------------------------------------------

void discover_blocks(EmitCtx &ctx, Fun *f) {
  if (!f || !f->entry) return;
  llvm::BasicBlock *entry_bb =
      llvm::BasicBlock::Create(*TheContext, "entry", ctx.llvm_fn);
  ctx.label_bb.put(f->entry, entry_bb);

  Vec<PNode *> worklist;
  Vec<PNode *> seen;
  worklist.add(f->entry);
  seen.set_add(f->entry);
  while (worklist.n) {
    PNode *cur = worklist.pop();
    for (PNode *s : cur->cfg_succ) {
      if (!s || !seen.set_add(s)) continue;
      if (s->code && s->code->kind == Code_LABEL) {
        char buf[32];
        snprintf(buf, sizeof(buf), "L%d",
                 s->code->label[0] ? s->code->label[0]->id : (int)seen.n);
        llvm::BasicBlock *bb = llvm::BasicBlock::Create(
            *TheContext, buf, ctx.llvm_fn);
        ctx.label_bb.put(s, bb);
      }
      worklist.add(s);
    }
  }
}

// -------------------------------------------------------------
// SSU phi/phy MOVE emission.  Ports cg.cc:do_phy_nodes /
// do_phi_nodes (lines 739-751) to LLVM-SSA.  IF1's phi and
// phy PNodes encode "MOVE on an edge":
//   pn->phy[i]: at this PNode, lvals[isucc] ← rvals[0]
//   succ->phi[i]: on this edge, lvals[0] ← rvals[pred_idx]
// At LLVM-SSA level the MOVE is just a rebind via
// put_result (matches our emit_move treatment).  These get
// called from emit_pnode AFTER each PNode's main emit, on
// the edge that's being taken.  For IF: separately per
// branch; for GOTO/SEND: on isucc=0.
// -------------------------------------------------------------

void emit_phy_moves(EmitCtx &ctx, PNode *pn, int isucc) {
  if (!pn) return;
  for (PNode *p : pn->phy) {
    if (!p || p->lvals.n <= isucc || p->rvals.n < 1) continue;
    Var *lhs = p->lvals.v[isucc];
    Var *rhs = p->rvals.v[0];
    if (!lhs || !rhs || !lhs->live) continue;
    llvm::Value *src = value_for_var(ctx, rhs);
    if (src) put_result(ctx, lhs, src);
  }
}

void emit_phi_moves(EmitCtx &ctx, PNode *pn, int isucc) {
  if (!pn || pn->cfg_succ.n == 0) return;
  if (isucc < 0 || isucc >= pn->cfg_succ.n) return;
  PNode *succ = pn->cfg_succ.v[isucc];
  if (!succ || succ->phi.n == 0) return;
  int i = succ->cfg_pred_index.get(pn);
  for (PNode *p : succ->phi) {
    if (!p || p->lvals.n < 1 || p->rvals.n <= i) continue;
    Var *lhs = p->lvals.v[0];
    Var *rhs = p->rvals.v[i];
    if (!lhs || !rhs || !lhs->live) continue;
    llvm::Value *src = value_for_var(ctx, rhs);
    if (src) put_result(ctx, lhs, src);
  }
}

// -------------------------------------------------------------
// Per-PNode emit.  Mirrors cg.cc:write_c_pnode (line 777) — the
// body-emit loop.  Phase R.1.3+R.1.4: dispatch on Code_kind,
// handle MOVE / SEND / LABEL / GOTO / IF.  Block boundaries
// switch the IRBuilder's insert point; closer PNodes emit
// terminators via emit_block_terminator.
// -------------------------------------------------------------

void emit_pnode(EmitCtx &ctx, PNode *pn, Vec<PNode *> &done) {
  if (!pn || !pn->code) return;
  // LABEL: switch to the corresponding BB.  Previous block's
  // closer should have already emitted its terminator.
  if (pn->code->kind == Code_LABEL) {
    llvm::BasicBlock *bb = ctx.label_bb.get(pn);
    if (bb) Builder->SetInsertPoint(bb);
  }
  if (pn->live && pn->fa_live) {
    switch (pn->code->kind) {
      case Code_LABEL:
        break;  // boundary only
      case Code_MOVE:
        emit_move(ctx, pn);
        break;
      case Code_SEND:
        emit_send(ctx, pn);
        break;
      case Code_IF:
      case Code_GOTO:
        break;  // terminators handled below
      default:
        break;
    }
  }
  // Phi/phy edge MOVE emission — mirrors cg.cc:do_phy_nodes /
  // do_phi_nodes at the per-PNode tail switch.  Called AFTER
  // the main emit so phi/phy MOVEs see the just-computed
  // PNode result.  isucc selection mirrors cg.cc:
  //   - IF: per-branch (deferred to emit_block_terminator)
  //   - GOTO / SEND / MOVE / default: isucc=0
  if (pn->code->kind != Code_IF) {
    emit_phy_moves(ctx, pn, 0);
    emit_phi_moves(ctx, pn, 0);
  }
  // Determine if `pn` is a block closer — any cfg_succ that
  // leaves the current block, or no successors at all.
  bool is_closer = (pn->cfg_succ.n == 0);
  for (PNode *s : pn->cfg_succ) {
    if (!s) continue;
    if (s->code && s->code->kind == Code_LABEL) {
      is_closer = true;
      break;
    }
  }
  // Special case: IF / GOTO / SEND-reply are always closers.
  if (pn->code->kind == Code_IF || pn->code->kind == Code_GOTO ||
      (pn->code->kind == Code_SEND && pn->prim &&
       pn->prim->index == P_prim_reply)) {
    is_closer = true;
  }
  if (is_closer && Builder->GetInsertBlock() &&
      !Builder->GetInsertBlock()->getTerminator()) {
    emit_block_terminator(ctx, pn);
  }
  // Recurse to successors.
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
  if (!ctx.llvm_fn) return;

  // R.1.3: pre-pass to allocate llvm::BasicBlocks for the
  // entry and every Code_LABEL PNode.  This lets emit_pnode
  // resolve cross-block branch targets directly.
  discover_blocks(ctx, f);
  // R.3 follow-up: alloca slots for phi-target Vars so
  // cross-edge writes have stable storage.  Mirrors the
  // materialized emit's alloca pre-pass.
  discover_phi_targets(ctx, f);
  llvm::BasicBlock *entry_bb = ctx.label_bb.get(f->entry);
  if (!entry_bb) {
    // Defensive fallback if entry wasn't reachable (no PNodes).
    entry_bb =
        llvm::BasicBlock::Create(*TheContext, "entry", ctx.llvm_fn);
    Builder->SetInsertPoint(entry_bb);
    if (ctx.llvm_fn->getReturnType()->isVoidTy())
      Builder->CreateRetVoid();
    else
      Builder->CreateRet(
          llvm::Constant::getNullValue(ctx.llvm_fn->getReturnType()));
    return;
  }
  Builder->SetInsertPoint(entry_bb);

  // DFS emit from entry.  emit_pnode handles MOVE / SEND /
  // LABEL switching / terminator emission.
  Vec<PNode *> done;
  done.set_add(f->entry);
  emit_pnode(ctx, f->entry, done);

  // Any BB without a terminator (rare — should only happen
  // when the closer-detection missed a case) gets an
  // unreachable so verifyModule doesn't reject the function.
  for (llvm::BasicBlock &bb : *ctx.llvm_fn) {
    if (!bb.getTerminator()) {
      Builder->SetInsertPoint(&bb);
      if (ctx.llvm_fn->getReturnType()->isVoidTy())
        Builder->CreateRetVoid();
      else
        Builder->CreateUnreachable();
    }
  }
}

}  // namespace

// -------------------------------------------------------------
// Public entry.
// -------------------------------------------------------------

bool cg_view_emit_llvm(FA *fa, Fun *main_fun) {
  if (!fa || !TheModule) return false;
  reset_state();

  // Pass 0: declare module-level globals so per-Fun emit can
  // resolve global Var operands.  Without this, loads/stores
  // to class-prototype globals (`@A`, `@y` etc.) silently
  // resolve to null and pyc-generated class-attribute stores
  // segfault at runtime.
  declare_globals(fa);

  // Pass 1: declare all functions (signature only).  Without
  // this pre-pass, emit_send_call's `TheModule->getFunction`
  // lookup misses for any callee declared later in fa->funs
  // order, silently dropping the call.  This is the standard
  // LLVM forward-declare pattern.
  std::vector<EmitCtx *> ctxs;
  for (Fun *f : fa->funs) {
    if (!f || !f->live) continue;
    EmitCtx *ctx = new EmitCtx();
    ctx->fn = f;
    ctx->llvm_fn = nullptr;
    build_fun_signature(*ctx, f);
    ctxs.push_back(ctx);
  }

  // Pass 2: emit bodies.  Each ctx already has its
  // llvm_fn and per-formal var_map bindings from Pass 1.
  // For the main_fun, prepend a GC_malloc-and-store prelude
  // for every ptr-to-struct global — mirrors the
  // materialized emit's class-prototype init pass.
  for (EmitCtx *ctx : ctxs) {
    emit_fun(*ctx, ctx->fn);
  }

  // Now do a separate pass: for main_fun specifically, emit
  // the class-prototype init prelude at the start of its
  // entry block.  Done after Pass 2 so the LLVM Module has
  // all functions declared (`get_gc_malloc` and friends).
  // Splice via SetInsertPoint to entry's first non-PHI inst
  // — since LLVM doesn't have a "prepend" API, we use a
  // helper BB approach: create a new BB, set as entry, emit
  // prelude there, then Br to the old entry.
  if (main_fun) {
    for (EmitCtx *ctx : ctxs) {
      if (ctx->fn != main_fun) continue;
      if (!ctx->llvm_fn) break;
      llvm::BasicBlock *old_entry =
          &ctx->llvm_fn->getEntryBlock();
      llvm::BasicBlock *new_entry = llvm::BasicBlock::Create(
          *TheContext, "main_prelude", ctx->llvm_fn, old_entry);
      Builder->SetInsertPoint(new_entry);
      // Emit GC_malloc + store for every ptr-to-struct global.
      // Dedupe by GlobalVariable — multiple Vars can map to
      // the same GV (e.g. aliased class refs), and emitting
      // a fresh malloc per alias would clobber the slot.
      Vec<Var *> keys;
      g_var_to_global.get_keys(keys);
      Vec<llvm::GlobalVariable *> seen_gvs;
      for (Var *v : keys) {
        llvm::GlobalVariable *gv = g_var_to_global.get(v);
        if (!gv || !v || !v->type) continue;
        if (v->type->type_kind != Type_RECORD) continue;
        if (!seen_gvs.set_add(gv)) continue;
        llvm::StructType *struct_ty = sym_to_llvm_struct(v->type);
        if (!struct_ty || !struct_ty->isSized()) continue;
        uint64_t sz =
            TheModule->getDataLayout().getTypeAllocSize(struct_ty);
        llvm::Value *sz_v = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(*TheContext), sz);
        cchar *nm = cg_get_string(v);
        llvm::Value *p = Builder->CreateCall(
            get_gc_malloc(), { sz_v }, nm ? nm : "proto");
        Builder->CreateStore(p, gv);
      }
      Builder->CreateBr(old_entry);
      break;
    }
  }
  return true;
}
