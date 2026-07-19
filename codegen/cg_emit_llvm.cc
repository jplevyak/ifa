// SPDX-License-Identifier: BSD-3-Clause
//
// cg_emit_llvm.cc — direct PNode → LLVM IR emitter.
//
// Parallel to cg.cc (C-text backend) but emits LLVM IR directly.
// Type info comes from IF1 Var->type Sym and Sym->has[] fields at
// emit time; a per-Fun Var* → llvm::Value* map (EmitCtx::var_map)
// binds operands.  Public entry: cg_emit_llvm() called from llvm.cc.
//
// Key contracts:
//   • Phi/phy targets get alloca slots (discover_phi_targets) so
//     cross-edge writes have stable storage across loop back-edges.
//   • Globals are always loaded fresh (value_for_var bypasses
//     var_map for GlobalVariables to avoid dominance violations).
//   • cfg_pred_index is rebuilt per function (emit_fun) so
//     emit_phi_moves selects the correct predecessor rval.
//   • For Code_IF with two live branches, interceding "edge" blocks
//     (t_edge / f_edge) are inserted as critical-edge splits so
//     phi/phy moves execute only on the taken branch.

#include "ifadefs.h"

#include "codegen/llvm_internal.h"
#include "codegen/codegen_common.h"
#include "builtin.h"  // sym_bool, sym_int32, sym_string, etc.
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

static llvm::Function *get_intrinsic_decl(llvm::Intrinsic::ID id) {
#if LLVM_VERSION_MAJOR >= 19
  return llvm::Intrinsic::getOrInsertDeclaration(TheModule.get(), id);
#else
  return llvm::Intrinsic::getDeclaration(TheModule.get(), id);
#endif
}

static llvm::Function *get_intrinsic_decl(llvm::Intrinsic::ID id,
                                          llvm::ArrayRef<llvm::Type *> tys) {
#if LLVM_VERSION_MAJOR >= 19
  return llvm::Intrinsic::getOrInsertDeclaration(TheModule.get(), id, tys);
#else
  return llvm::Intrinsic::getDeclaration(TheModule.get(), id, tys);
#endif
}

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
    // ifa/issues/030: class records carry a classtag header at
    // slot 0 (mirrors the C backend's `__pyc_tag` member). All
    // has-index -> struct-slot translations must go through
    // llvm_fld() so tagged structs shift by one.
    if (cg_has_classtag(s))
      fields.push_back(llvm::PointerType::getUnqual(*TheContext));
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

// ifa/issues/030: translate a has-index into the LLVM struct slot
// (tagged class records have the classtag at slot 0).
static inline int llvm_fld(Sym *s, int i) { return cg_has_classtag(s) ? i + 1 : i; }

// ifa/issues/030: the per-class tag object. Only its ADDRESS
// matters (identity comparison at dispatch sites), so an internal
// i64 global per class name suffices; named to match the C
// backend's `_CG_type_<name>` for debuggability.
static llvm::GlobalVariable *get_classtag_global(cchar *name) {
  char buf[160];
  snprintf(buf, sizeof(buf), "_CG_type_%s", name);
  if (llvm::GlobalVariable *gv = TheModule->getNamedGlobal(buf)) return gv;
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  return new llvm::GlobalVariable(*TheModule, i64, /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
                                  llvm::ConstantInt::get(i64, 0), buf);
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
    if (s == sym_void || s == sym_void_type) {
      // sym_void_type marks FA-unreachable/no-value results (the C
      // backend's `_CG_void_type = void*` placeholder that's declared
      // but never read/written). Mapping it to a real pointer type
      // here let void-typed dead-branch Vars poison the shared
      // alloca-slot union-find in discover_phi_targets: a dead var
      // could claim a phi-class's slot as 'ptr' before a live,
      // genuinely-int64 member of the same class got a chance to,
      // corrupting that live var's storage type (LLVM verifier
      // failure on otherwise-plain int arithmetic reached through a
      // loop). LLVM's void type makes discover_phi_targets' existing
      // `t->isVoidTy()` skip actually fire for these.
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
  FA *fa;
  Fun *fn;
  llvm::Function *llvm_fn;
  Map<Var *, llvm::Value *> var_map;
  // Phi-target Vars get an alloca slot in the entry block so
  // cross-edge writes have a stable storage location.  Without
  // this, SSA-bind alone can't carry values across loop back-edges
  // or if/else joins — every loop body sees the initial value
  // forever.  When a Var has an entry in alloca_map,
  // value_for_var emits a load, put_result emits a store, and
  // emit_phi_moves writes via store.
  Map<Var *, llvm::AllocaInst *> alloca_map;
  Map<PNode *, llvm::BasicBlock *> label_bb;
  llvm::Value *coro_id = nullptr;
  llvm::Value *coro_hdl = nullptr;
  llvm::BasicBlock *coro_suspend_bb = nullptr;
  llvm::BasicBlock *coro_destroy_bb = nullptr;
};

// emit_phy_moves/emit_phi_moves are called by emit_block_terminator but defined later.
void emit_phy_moves(EmitCtx &ctx, PNode *pn, int isucc);
void emit_phi_moves(EmitCtx &ctx, PNode *pn, int isucc);

// Reset per-program state so type caches don't leak across compiles.

void reset_state() {
  g_sym_to_type.clear();
  g_sym_to_struct.clear();
  g_string_globals.clear();
  g_var_to_global.clear();
}

// Pass 0: declare module-level globals.  Parallel to cg.cc's
// `<type> gN;` declarations.  Walks live non-constant non-fun Vars;
// the materialized
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
    std::string final_name;
    if (name && name[0]) {
      final_name = name;
    } else if (s->name && s->name[0] && strchr(s->name, ' ') == nullptr) {
      final_name = s->name;
    } else {
      final_name = "g" + std::to_string(v->id);
    }
    // If a global with this name already exists in the
    // module (declared by some earlier pass), reuse it.
    if (llvm::GlobalVariable *existing =
            TheModule->getNamedGlobal(final_name)) {
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
        llvm::GlobalValue::InternalLinkage, init, final_name);
    g_var_to_global.put(v, gv);
  }
}

// (emit_main_prelude logic is inlined into cg_emit_llvm below
// since it needs get_gc_malloc which is declared later.)

// -------------------------------------------------------------
// Operand lookup helpers — mirror cg.cc's `cg_get_string(v)`
// idiom.  Read sites consult `ctx.var_map`; for is_constant
// Syms with an Immediate, materialize the LLVM constant
// inline.  Write sites bind the produced llvm::Value via
// `put_result`.
// -------------------------------------------------------------

// Build (or fetch from cache) a pyc-layout string global:
// `{ i64 len, [N x i8] body }` packed.  Returns a GEP to
// the first body byte.  Length is at offset -8 from that ptr.
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
  Sym *s = get_constant(v);
  if (!s) s = v->sym;
  if (s && s->is_constant && v->type) {
    llvm::Type *t = sym_to_llvm_type(v->type);
    if (!t) return nullptr;
    if (s->name && (!strcmp(s->name, "True") || !strcmp(s->name, "true"))) {
      if (t->isIntegerTy()) return llvm::ConstantInt::get(t, 1);
      if (t->isPointerTy()) {
        llvm::Constant *int_val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), 1);
        return llvm::ConstantExpr::getIntToPtr(int_val, t);
      }
      if (t->isFloatingPointTy()) return llvm::ConstantFP::get(t, 1.0);
    }
    if (s->name && (!strcmp(s->name, "False") || !strcmp(s->name, "false"))) {
      if (t->isIntegerTy()) return llvm::ConstantInt::get(t, 0);
      if (t->isPointerTy()) {
        llvm::Constant *int_val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), 0);
        return llvm::ConstantExpr::getIntToPtr(int_val, t);
      }
      if (t->isFloatingPointTy()) return llvm::ConstantFP::get(t, 0.0);
    }

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
        // String literal → pyc-layout global; GEP to first char.
        // Length prefix is at offset -8 from that pointer so
        // runtime helpers (_CG_string_len etc.) can read it.
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
  llvm::AllocaInst *slot = ctx.alloca_map.get(v);
  if (slot) {
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
  // Global Var: always also emit a real store so reads via
  // value_for_var (which loads from the GlobalVariable) see
  // the current value, not the initial null.
  if (llvm::GlobalVariable *gv = g_var_to_global.get(v)) {
    llvm::Type *gv_ty = sym_to_llvm_type(v->type);
    llvm::Value *sv = value;
    if (gv_ty && sv->getType() != gv_ty) {
      if (sv->getType()->isPointerTy() && gv_ty->isPointerTy()) {
        // opaque-ptr no-op
      } else if (sv->getType()->isIntegerTy() && gv_ty->isIntegerTy()) {
        sv = Builder->CreateSExtOrTrunc(sv, gv_ty);
      }
    }
    Builder->CreateStore(sv, gv);
    return;  // value_for_var always loads globals fresh; var_map bind would be dead
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

// Get-or-create the two per-function coroutine exit blocks shared by
// every suspend point in the function (the initial suspend right
// after coro.begin, plus every mid-body await/wait_read/wait_write):
//
// - suspend_ret_bb: reached whenever the coroutine genuinely
//   suspends, waiting to be resumed later (by the event loop, or
//   simply because it hasn't been resumed yet). This is NOT an end
//   of the coroutine -- it must just return the handle to the
//   caller, never call llvm.coro.end.
// - destroy_bb: reached on early cancellation (the coroutine is
//   destroyed before running to completion). Performs cleanup and
//   calls coro.end marked IsUnwind=true, so it isn't a second
//   "fallthrough" end -- the epilogue's own coro.end (emitted at
//   normal completion, see the P_prim_reply handling) is the
//   function's one true (IsUnwind=false) end. LLVM's coro-split pass
//   rejects more than one fallthrough coro.end per function.
void ensure_coro_suspend_destroy_bbs(EmitCtx &ctx) {
  if (ctx.coro_suspend_bb && ctx.coro_destroy_bb) return;
  llvm::BasicBlock *old = Builder->GetInsertBlock();

  if (!ctx.coro_suspend_bb) {
    ctx.coro_suspend_bb =
        llvm::BasicBlock::Create(*TheContext, "suspend_ret", ctx.llvm_fn);
    Builder->SetInsertPoint(ctx.coro_suspend_bb);
    Builder->CreateRet(ctx.coro_hdl);
  }

  if (!ctx.coro_destroy_bb) {
    ctx.coro_destroy_bb =
        llvm::BasicBlock::Create(*TheContext, "destroy", ctx.llvm_fn);
    Builder->SetInsertPoint(ctx.coro_destroy_bb);
    llvm::Function *coro_free_fn = get_intrinsic_decl(llvm::Intrinsic::coro_free);
    llvm::Value *mem = Builder->CreateCall(coro_free_fn, {ctx.coro_id, ctx.coro_hdl});
    llvm::Function *gc_free = get_runtime_helper(
        "GC_free", llvm::Type::getVoidTy(*TheContext),
        {llvm::PointerType::getUnqual(*TheContext)});
    Builder->CreateCall(gc_free, {mem});
    llvm::Function *coro_end_fn = get_intrinsic_decl(llvm::Intrinsic::coro_end);
    Builder->CreateCall(coro_end_fn,
                         {ctx.coro_hdl, Builder->getTrue(),
                          llvm::ConstantTokenNone::get(*TheContext)});
    Builder->CreateRet(ctx.coro_hdl);
  }

  if (old) Builder->SetInsertPoint(old);
}

// -------------------------------------------------------------
// P_prim_period: struct field getter and closure construction.
//
// Two sub-shapes:
//   1. Closure: lvals[0]->type is Type_FUN and pn->creates is
//      non-empty — GC_malloc a closure struct, store selector
//      at field 0 (e0) and bound-self at field 1 (e1).
//   2. Struct getter: resolve the obj's concrete type through
//      resolve_union_receiver, find the field index by symbol
//      name in obj->has, emit GEP + Load.
// -------------------------------------------------------------

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
    if (!closure_struct->isSized()) return false;
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
  if (!obj_sym) {
    return false;
  }
  int field_idx = -1;
  for (int i = 0; i < obj_sym->has.n; i++) {
    if (obj_sym->has.v[i] && symbol == obj_sym->has.v[i]->name) {
      field_idx = i;
      break;
    }
  }
  if (field_idx < 0) {
    return false;
  }

  llvm::StructType *obj_struct = sym_to_llvm_struct(obj_sym);
  int slot_idx = llvm_fld(obj_sym, field_idx);
  if (!obj_struct || slot_idx >= (int)obj_struct->getNumElements()) {
    return false;
  }
  llvm::Value *obj = value_for_var(ctx, pn->rvals.v[1]);
  if (!obj) {
    return false;
  }
  llvm::Value *gep =
      Builder->CreateStructGEP(obj_struct, obj, slot_idx);
  llvm::Type *field_ty = obj_struct->getElementType(slot_idx);
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
// P_prim_setter: struct field setter.
// Resolves symbol from rvals[3], looks up the field index in the
// union-resolved obj type, emits GEP + Store.  Optionally also
// stores into lvals[0] for chained-assignment semantics.
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
  int slot_idx = llvm_fld(obj_sym, field_idx);
  if (!obj_struct || slot_idx >= (int)obj_struct->getNumElements())
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
        Builder->CreateStructGEP(obj_struct, obj, slot_idx);
    llvm::Type *field_ty = obj_struct->getElementType(slot_idx);
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
// Arithmetic and comparison operators: direct LLVM IR ops keyed
// on prim index.  Unlike the C backend (which calls _CG_<name>
// runtime helpers), LLVM emits CreateAdd/Sub/ICmp/FCmp directly.
//
// Operand layout from IF1 prim-call convention:
//   rvals[n-3] = lhs (the __operator receiver if present)
//   rvals[n-1] = rhs
// Materialized's lower_send_binop reads at these positions
// (cg_normalize_v2.cc:662).
// -------------------------------------------------------------

// -------------------------------------------------------------
// emit_send_unaryop — P_prim_lnot and P_prim_minus.
// -------------------------------------------------------------

bool emit_send_unaryop(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  int op = pn->prim->index;
  if (op != P_prim_lnot && op != P_prim_minus) return false;

  // Return true (claimed) even when operands are missing — these are
  // dead/pruned nodes; falling through to emit_send_default_prim would
  // produce a spurious _CG_<name> call for an operation with no result.
  if (pn->rvals.n < 1 || pn->lvals.n < 1) return true;
  Var *src = pn->rvals.v[pn->rvals.n - 1];
  Var *dst = pn->lvals.v[0];
  if (!src || !dst) return true;

  llvm::Value *val = value_for_var(ctx, src);
  if (!val) return true;

  llvm::Value *res = nullptr;
  if (op == P_prim_lnot) {
    if (val->getType()->isIntegerTy()) {
      llvm::Value *zero = llvm::ConstantInt::get(val->getType(), 0);
      res = Builder->CreateICmpEQ(val, zero);
    } else if (val->getType()->isFloatTy() || val->getType()->isDoubleTy()) {
      llvm::Value *zero = llvm::ConstantFP::get(val->getType(), 0.0);
      res = Builder->CreateFCmpOEQ(val, zero);
    } else if (val->getType()->isPointerTy()) {
      llvm::Value *zero = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(val->getType()));
      res = Builder->CreateICmpEQ(val, zero);
    } else {
      res = Builder->CreateNot(val);
    }
  } else if (op == P_prim_minus) {
    if (val->getType()->isFloatTy() || val->getType()->isDoubleTy()) {
      res = Builder->CreateFNeg(val);
    } else if (val->getType()->isIntegerTy()) {
      res = Builder->CreateNeg(val);
    }
  }

  if (!res) {
    // Live unaryop with an unsupported operand type — not a dead node,
    // so silently returning true would leave dst uninitialized.
    cchar *src_ty_name = src->type ? cg_get_string(src->type) : "?";
    codegen_fail(pn, "emit_send_unaryop: op %d unsupported for operand type %s",
                 op, src_ty_name ? src_ty_name : "?");
  }

  // Coerce to dst's expected LLVM type
  llvm::Type *dst_ty = sym_to_llvm_type(dst->type);
  if (dst_ty && res->getType() != dst_ty) {
    if (res->getType()->isIntegerTy() && dst_ty->isIntegerTy()) {
      res = Builder->CreateZExtOrTrunc(res, dst_ty);
    } else if (res->getType()->isIntegerTy() && dst_ty->isFloatTy()) {
      res = Builder->CreateSIToFP(res, dst_ty);
    } else if (res->getType()->isFloatTy() && dst_ty->isIntegerTy()) {
      res = Builder->CreateFPToSI(res, dst_ty);
    }
  }
  put_result(ctx, dst, res);
  return true;
}

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
// P_prim_sizeof / P_prim_sizeof_element: compile-time sizes via DataLayout.
// -------------------------------------------------------------

bool emit_send_sizeof(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_sizeof &&
      pn->prim->index != P_prim_sizeof_element) return false;
  if (pn->lvals.n < 1) return false;
  int o = (pn->rvals.n > 0 && pn->rvals.v[0] &&
           pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  if (o >= pn->rvals.n) return false;
  Sym *outer = pn->rvals.v[o]->type;
  if (!outer) return false;
  Sym *t_sym = outer;
  llvm::Type *dst_ty = sym_to_llvm_type(pn->lvals.v[0]->type);
  if (!dst_ty) return false;
  if (pn->prim->index == P_prim_sizeof_element) {
    // Mirrors cg.cc: prefer outer->element->type; fall back to first
    // field of a Type_RECORD if the element type has no compile-time
    // size; else 0.
    if (outer->element) t_sym = outer->element->type;
    if (t_sym && !t_sym->size && outer->type_kind == Type_RECORD &&
        outer->has.n) {
      t_sym = outer->has.v[0]->type;
    }
    // Mirrors cg.cc's SUM-of-uniform-size case: a generic list whose
    // element type is the union of 2+ distinct types stores each
    // element in one slot sized to fit all of them -- boxed records
    // and other boxed containers (list, str, set, dict, ...) are all
    // one pointer_size; same-width scalars agree too. Emitting 0 made
    // list::append resize with element size 0, so the storage never
    // grew and reads returned null/corrupted at runtime with a clean
    // compile (issue 025 tuple-list soundness bug; recurred as
    // list-of-list vs list-of-record mixing in rubik2, since `list`
    // itself is Type_PRIMITIVE, not Type_RECORD -- the original
    // record-only check rejected a perfectly uniform pointer-sized
    // union).
    if (t_sym && !t_sym->size && t_sym->type_kind == Type_SUM && t_sym->has.n) {
      int common = 0;
      bool uniform = true;
      for (Sym *m : t_sym->has) if (m) {
        Sym *mt = m->type ? m->type : m;
        if (!mt->size) { uniform = false; break; }
        if (!common) common = mt->size;
        else if (common != mt->size) { uniform = false; break; }
      }
      if (uniform && common) {
        put_result(ctx, pn->lvals.v[0], llvm::ConstantInt::get(dst_ty, common));
        return true;
      }
    }
  }
  if (!t_sym) {
    put_result(ctx, pn->lvals.v[0],
               llvm::ConstantInt::get(dst_ty, 0));
    return true;
  }
  llvm::Type *t = sym_to_llvm_type(t_sym);
  if (!t || !t->isSized()) {
    put_result(ctx, pn->lvals.v[0],
               llvm::ConstantInt::get(dst_ty, 0));
    return true;
  }
  uint64_t sz = TheModule->getDataLayout().getTypeAllocSize(t);
  put_result(ctx, pn->lvals.v[0],
             llvm::ConstantInt::get(dst_ty, sz));
  return true;
}

// -------------------------------------------------------------
// P_prim_index_object: indexed load / store.
//   - String: call _CG_char_from_string(obj, idx) → ptr
//   - List (non-vector, non-RECORD): deref _CG_list_ptr (*(ptr*)(obj-8))
//     then GEP into that data buffer.
//   - Vector / RECORD: GEP directly into obj.
// -------------------------------------------------------------

// Return the data pointer for a list: *(ptr*)(l - 8) = _CG_list_ptr(l).
static llvm::Value *load_list_data_ptr(llvm::Value *obj) {
  llvm::Type *i8 = llvm::Type::getInt8Ty(*TheContext);
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
  llvm::Value *ptr_field = Builder->CreateGEP(
      i8, obj, llvm::ConstantInt::get(i64, -8), "list_ptr_addr");
  return Builder->CreateLoad(ptr_ty, ptr_field, "list_data");
}

// issues/025: plain (non-slice) indexing had no negative-index
// normalization at all on this backend either (mirrors the C
// backend's fix in cg.cc/pyc_c_runtime.h -- `a[-1]` read/wrote
// out-of-bounds memory instead of counting back from the end).
// String length lives at (s-8) as i64; list length at (l-12) as u32
// (same offsets emit_send_len already reads). idx64: the raw index,
// already sign/zero-extended to i64 by the caller.
static llvm::Value *emit_norm_idx(llvm::Value *obj, llvm::Value *idx64, bool is_string) {
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *i8 = llvm::Type::getInt8Ty(*TheContext);
  llvm::Value *len;
  if (is_string) {
    llvm::Value *off = llvm::ConstantInt::get(i64, -8);
    llvm::Value *header_ptr = Builder->CreateGEP(i8, obj, off);
    len = Builder->CreateLoad(i64, header_ptr, "str_len");
  } else {
    llvm::Type *i32 = llvm::Type::getInt32Ty(*TheContext);
    llvm::Value *off = llvm::ConstantInt::get(i64, -12);
    llvm::Value *len_addr = Builder->CreateGEP(i8, obj, off, "len_addr");
    llvm::Value *len32 = Builder->CreateLoad(i32, len_addr, "len32");
    len = Builder->CreateZExt(len32, i64, "list_len");
  }
  llvm::Value *is_neg = Builder->CreateICmpSLT(idx64, llvm::ConstantInt::get(i64, 0), "idx_neg");
  llvm::Value *normed = Builder->CreateAdd(idx64, len, "idx_normed");
  return Builder->CreateSelect(is_neg, normed, idx64, "idx");
}

// Same as emit_norm_idx, but for a Type_RECORD receiver (a tuple or
// a fixed-size tuple-list literal) where the "length" is the field
// count -- a compile-time constant (`len`), not something to read
// from memory. Deliberately NOT the runtime -12-offset list-header
// trick emit_norm_idx uses: a real tuple (_CG_prim_tuple) has no
// such header at all (only a fixed-size tuple-list literal,
// _CG_prim_tuple_list, happens to -- confirmed by testing: reading
// a real tuple's nonexistent header as if it were one gave nonsense
// results, not a crash). t->has.n is correct for both shapes since
// it's a static property of the record type either way.
static llvm::Value *emit_norm_idx_const_len(llvm::Value *idx64, int64_t len) {
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Value *is_neg = Builder->CreateICmpSLT(idx64, llvm::ConstantInt::get(i64, 0), "idx_neg");
  llvm::Value *normed = Builder->CreateAdd(idx64, llvm::ConstantInt::get(i64, len), "idx_normed");
  return Builder->CreateSelect(is_neg, normed, idx64, "idx");
}

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

  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  if (idx->getType()->isIntegerTy() && !idx->getType()->isIntegerTy(64))
    idx = Builder->CreateSExtOrTrunc(idx, i64);

  Sym *t = pn->rvals.v[o]->type;

  // String: _CG_char_from_string(obj, (int)idx) → ptr
  if (t && sym_string && sym_string->specializers.set_in(t)) {
    llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
    llvm::Type *i32 = llvm::Type::getInt32Ty(*TheContext);
    llvm::Value *idx_normed = emit_norm_idx(obj, idx, /*is_string=*/true);
    llvm::Value *idx32 = Builder->CreateTrunc(idx_normed, i32);
    llvm::FunctionCallee fn = TheModule->getOrInsertFunction(
        "_CG_char_from_string",
        llvm::FunctionType::get(ptr_ty, {ptr_ty, i32}, false));
    llvm::Value *result = Builder->CreateCall(fn, {obj, idx32});
    put_result(ctx, dst_v, result);
    return true;
  }

  // List (not vector, not RECORD): GEP through _CG_list_ptr(obj)
  if (t && !t->is_vector && t->type_kind != Type_RECORD) {
    llvm::Value *data = load_list_data_ptr(obj);
    llvm::Value *idx_normed = emit_norm_idx(obj, idx, /*is_string=*/false);
    llvm::Value *gep = Builder->CreateGEP(elem_ty, data, idx_normed);
    llvm::Value *loaded = Builder->CreateLoad(
        elem_ty, gep, cg_get_string(dst_v) ? cg_get_string(dst_v) : "");
    put_result(ctx, dst_v, loaded);
    return true;
  }

  // Vector / RECORD: direct GEP into obj. A non-vector Type_RECORD
  // reaching here is a real tuple or a fixed-size tuple-list literal
  // (`a = [1,2,3]`) -- t->has.n (field count) is its length either
  // way, a compile-time constant. A true @vector class (bytearray)
  // has no such static length -- its length is a runtime struct
  // field (`self.length`), not generically discoverable here, so
  // it's excluded (still-open gap, issues/025).
  llvm::Value *idx_use = idx;
  if (t && !t->is_vector) idx_use = emit_norm_idx_const_len(idx, t->has.n);
  llvm::Value *gep = Builder->CreateGEP(elem_ty, obj, idx_use);
  llvm::Value *loaded = Builder->CreateLoad(
      elem_ty, gep, cg_get_string(dst_v) ? cg_get_string(dst_v) : "");
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
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  if (idx->getType()->isIntegerTy() && !idx->getType()->isIntegerTy(64))
    idx = Builder->CreateSExtOrTrunc(idx, i64);

  Sym *t = pn->rvals.v[o]->type;

  // List: GEP through _CG_list_ptr(obj)
  if (t && !t->is_vector && t->type_kind != Type_RECORD &&
      !(sym_string && sym_string->specializers.set_in(t))) {
    llvm::Value *data = load_list_data_ptr(obj);
    llvm::Value *idx_normed = emit_norm_idx(obj, idx, /*is_string=*/false);
    llvm::Value *gep = Builder->CreateGEP(elem_ty, data, idx_normed);
    Builder->CreateStore(val, gep);
    return true;
  }

  // Vector / RECORD: same normalization (and the same @vector
  // exclusion) as emit_send_index_load's fallback above.
  llvm::Value *idx_use = idx;
  if (t && !t->is_vector) idx_use = emit_norm_idx_const_len(idx, t->has.n);
  llvm::Value *gep = Builder->CreateGEP(elem_ty, obj, idx_use);
  Builder->CreateStore(val, gep);
  return true;
}

// -------------------------------------------------------------
// P_prim_coerce: type conversion.
// rvals[n-2] = target-type Sym, rvals[n-1] = source value.
// Emits the appropriate SExtOrTrunc / ZExtOrTrunc / FPToSI /
// SIToFP / PtrToInt / IntToPtr based on src and dst LLVM types.
static bool emit_send_coerce(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim || pn->prim->index != P_prim_coerce) return false;
  if (pn->rvals.n < 2 || pn->lvals.n < 1) return false;
  Var *tgt_var = pn->rvals.v[pn->rvals.n - 2];
  Var *src_var = pn->rvals.v[pn->rvals.n - 1];
  Var *dst_var = pn->lvals.v[0];
  if (!tgt_var || !tgt_var->sym || !src_var || !dst_var) return false;
  Sym *tgt_sym = tgt_var->sym->is_meta_type
                     ? tgt_var->sym->meta_type
                     : tgt_var->sym;
  tgt_sym = unalias_type(tgt_sym);
  if (!tgt_sym) return false;
  llvm::Type *dst_ty = sym_to_llvm_type(tgt_sym);
  llvm::Value *src = value_for_var(ctx, src_var);
  if (!dst_ty || !src) return false;

  llvm::Type *src_ty = src->getType();
  llvm::Value *res = src;
  if (src_ty != dst_ty) {
    if (src_ty->isIntegerTy() && dst_ty->isIntegerTy()) {
      // Narrowing to i1: use icmp ne instead of trunc to preserve truthiness.
      // Trunc keeps only the LSB, so bool(10) → 0 → False (wrong).
      if (dst_ty->isIntegerTy(1)) {
        res = Builder->CreateICmpNE(src,
            llvm::ConstantInt::get(src_ty, 0));
      } else {
        res = Builder->CreateSExtOrTrunc(src, dst_ty);
      }
    } else if (src_ty->isIntegerTy() && dst_ty->isFloatingPointTy()) {
      res = Builder->CreateSIToFP(src, dst_ty);
    } else if (src_ty->isFloatingPointTy() && dst_ty->isIntegerTy()) {
      res = Builder->CreateFPToSI(src, dst_ty);
    } else if (src_ty->isFloatingPointTy() && dst_ty->isFloatingPointTy()) {
      if (src_ty->getPrimitiveSizeInBits() <
          dst_ty->getPrimitiveSizeInBits()) {
        res = Builder->CreateFPExt(src, dst_ty);
      } else {
        res = Builder->CreateFPTrunc(src, dst_ty);
      }
    } else if (src_ty->isPointerTy() && dst_ty->isIntegerTy()) {
      res = Builder->CreatePtrToInt(src, dst_ty);
    } else if (src_ty->isIntegerTy() && dst_ty->isPointerTy()) {
      res = Builder->CreateIntToPtr(src, dst_ty);
    }
  }
  put_result(ctx, dst_var, res);
  return true;
}

// -------------------------------------------------------------
// P_prim_is / P_prim_isinstance: pointer identity and None checks.
//   prim_is(a, b)           → icmp eq ptr a, b
//   prim_isinstance(x, nil) → icmp eq ptr x, null
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
  // issue 011: isinstance against a REAL class -- needed for
  // `except X as e:` matching against the pending-exception slot's
  // whole-program union type, which defeats FA's per-CS constant
  // folding. There's no runtime hierarchy to walk (a record's only
  // identity is its classtag pointer, compared by equality), so
  // build a compile-time disjunction over the class's implementors
  // (mirrors cg.cc's identical fix and fa.cc's OWN isinstance
  // constant-folding, which reads cs2->sym->meta_type->implementors
  // the same way) instead of falling through to the (wrong for this
  // case -- compares the OBJECT POINTER, not its classtag) generic
  // `lhs == rhs` path below.
  if (pn->prim->index == P_prim_isinstance && rhs_var->sym && rhs_var->sym != sym_nil_type &&
      rhs_var->sym->meta_type != sym_nil_type) {
    // cg.cc's identical fix: the second arg is usually ALREADY the
    // concrete class Sym (a raw sym_primitive send built directly at
    // the checking call site) -- use it as-is when it looks like a
    // real record type (type_kind Type_RECORD, has.n > 0).
    // ->meta_type is a DIFFERENT (meta-level, has.n==0) Sym whose own
    // implementors are empty here; it's only the right hop for a
    // value-vs-type-Sym split (build_isinstance_call's shared
    // isinstance() clone, where the formal itself carries no
    // concrete type_kind).
    Sym *cls = rhs_var->sym;
    Sym *cls_type = (cls->type_kind == Type_RECORD && cls->has.n) ? cls : (cls->meta_type ? cls->meta_type : cls);
    Vec<Sym *> concrete;
    for (Sym *impl : cls_type->implementors)
      if (impl && cg_has_classtag(impl)) concrete.add(impl);
    llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
    llvm::Value *res;
    if (!concrete.n) {
      res = llvm::ConstantInt::getFalse(*TheContext);
    } else {
      llvm::Value *tag = Builder->CreateLoad(ptr_ty, lhs, "classtag");
      res = llvm::ConstantInt::getFalse(*TheContext);
      for (Sym *impl : concrete) {
        llvm::Value *cmp = Builder->CreateICmpEQ(tag, get_classtag_global(impl->name), "isocmp");
        res = Builder->CreateOr(res, cmp);
      }
    }
    llvm::Type *dst_ty = sym_to_llvm_type(dst_var->type);
    if (dst_ty && res->getType() != dst_ty && dst_ty->isIntegerTy()) res = Builder->CreateZExtOrTrunc(res, dst_ty);
    put_result(ctx, dst_var, res);
    return true;
  }
  llvm::Value *rhs = nullptr;
  if (pn->prim->index == P_prim_isinstance && rhs_var->sym &&
      (rhs_var->sym == sym_nil_type || rhs_var->sym->meta_type == sym_nil_type)) {
    // issue 011: an exception-propagation check (emit_exc_check)
    // that ifa/optimize/exc_check_fold.cc proved can never fire has
    // ALREADY had its condition Var rewritten to FA's own canonical
    // true_type constant by the time codegen runs --
    // virtual_cg_is_const_folded_send's check upstream
    // (virtual_cg_emit_send) skips this whole send for that case, so
    // this function is never even reached for it; the downstream
    // Code_IF handling then elides the dead arm too. No
    // special-casing needed here -- just the ordinary null compare.
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
// P_prim_strcat: string concatenation via _CG_strcat(a, b).
// SEND shape: __operator + s1 s2; rvals[o]=s1, rvals[o+2]=s2.
// rvals[o+1] is the '+' operator symbol — skipped.
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
// P_prim_len: length of string or list.
//   - String: i64 at (s - 8)  (_CG_string layout: {i64 len, char data[]})
//   - List:   i32 at (l - 12) (_CG_list_struct: {u32 total @-16,
//             u32 len @-12, void* ptr @-8, char data[] @0})
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
  llvm::Value *len;
  if (is_string) {
    // String: i64 at offset -8.
    llvm::Value *off = llvm::ConstantInt::get(i64, -8);
    llvm::Value *header_ptr = Builder->CreateGEP(i8, obj, off);
    len = Builder->CreateLoad(i64, header_ptr,
        cg_get_string(dst_var) ? cg_get_string(dst_var) : "len");
  } else {
    // List: u32 `len` field at offset -12; zero-extend to i64.
    llvm::Type *i32 = llvm::Type::getInt32Ty(*TheContext);
    llvm::Value *off = llvm::ConstantInt::get(i64, -12);
    llvm::Value *len_addr = Builder->CreateGEP(i8, obj, off, "len_addr");
    llvm::Value *len32 = Builder->CreateLoad(i32, len_addr, "len32");
    len = Builder->CreateZExt(len32, i64, "len");
  }
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
// P_prim_clone / P_prim_clone_vector: copy a prototype struct.
//   clone: GC_malloc(dst_size) + memcpy(src, min(dst,src) bytes).
//     dst_size may exceed src_size for subclass destinations.
//   clone_vector: _CG_prim_clone_vector_runtime(proto, sz, n_extra).
static bool emit_send_clone(EmitCtx &ctx, PNode *pn) {
  if (!pn || !pn->prim) return false;
  if (pn->prim->index != P_prim_clone &&
      pn->prim->index != P_prim_clone_vector &&
      pn->prim->index != P_prim_copy) return false;
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

  // Scalar (or string) copy is identity — no struct to clone.
  // Mirrors cg.cc's P_prim_copy Type_RECORD check; reachable via
  // deepcopy's monomorphic int64 leaf contour (pyc issues/025 R1
  // item 5).
  if (pn->prim->index == P_prim_copy && dst_var->type->type_kind != Type_RECORD) {
    put_result(ctx, dst_var, src);
    return true;
  }

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

  if (pn->prim->index == P_prim_clone || pn->prim->index == P_prim_copy) {
    // Plain clone: GC_malloc(dst_size) + memcpy from src.
    // The materialized emit uses this approach (no runtime
    // call — the `_CG_prim_clone_dst` macro is static inline
    // in pyc_c_runtime.h, not linkable).
    llvm::Value *new_p = Builder->CreateCall(
        get_gc_malloc(), { llvm::ConstantInt::get(i64, dst_sz) },
        cg_get_string(dst_var) ? cg_get_string(dst_var) : "clone");
    // memcpy(new_p, src, min(dst_size, src_size)).
    uint64_t copy_sz = dst_sz < src_sz ? dst_sz : src_sz;
#if LLVM_VERSION_MAJOR >= 19
    llvm::Function *memcpy_fn = llvm::Intrinsic::getOrInsertDeclaration(
        TheModule.get(), llvm::Intrinsic::memcpy,
        { ptr_ty, ptr_ty, i64 });
#else
    llvm::Function *memcpy_fn = llvm::Intrinsic::getDeclaration(
        TheModule.get(), llvm::Intrinsic::memcpy,
        { ptr_ty, ptr_ty, i64 });
#endif
    Builder->CreateCall(memcpy_fn,
        { new_p, src, llvm::ConstantInt::get(i64, copy_sz),
          llvm::ConstantInt::getFalse(*TheContext) });
    // ifa/issues/029/030: populate method-pointer slots for
    // polymorphic dispatch (mirrors cg.cc's P_prim_clone
    // polymorphic method-pointer installation logic).
    if (pn->prim->index == P_prim_clone) {
    // emission from cg_new_to_val_map).
    if (Vec<PolymorphicSlot> *pslots = cg_new_to_val_map.get(ctx.fn)) {
      Sym *dt = dst_var->type;
      for (int si = 0; si < pslots->n; si++) {
        int slot = (*pslots)[si].slot;
        Fun *fun_val = (*pslots)[si].fun_val;
        if (!cg_field_live(dt, slot)) continue;
        if (!fun_val->cg_string) continue;
        llvm::Function *fv = TheModule->getFunction(fun_val->cg_string);
        if (!fv) continue;
        llvm::Value *gep = Builder->CreateStructGEP(dst_struct, new_p, llvm_fld(dt, slot));
        Builder->CreateStore(fv, gep);
      }
    }
    }
    put_result(ctx, dst_var, new_p);
    return true;
  }

  // Vector form: cg.cc:444 emits `_CG_prim_clone_vector(c, v)`
  // which expands to `_CG_prim_primitive_clone_vector(c,
  // sizeof(*(c)), v)` — the same symbol exported from
  // libpyc_runtime.a via extern inline in pyc_runtime.c.
  if (pn->rvals.n <= o + 1) return false;
  llvm::Value *v_extra = value_for_var(ctx, pn->rvals.v[o + 1]);
  if (!v_extra) return false;
  // Coerce v_extra to i64 if needed.
  if (v_extra->getType()->isIntegerTy() &&
      !v_extra->getType()->isIntegerTy(64)) {
    v_extra = Builder->CreateZExtOrTrunc(v_extra, i64);
  }
  llvm::Function *fn = get_runtime_helper(
      "_CG_prim_primitive_clone_vector", ptr_ty,
      { ptr_ty, i64, i64 });
  llvm::Value *res = Builder->CreateCall(
      fn, { src, llvm::ConstantInt::get(i64, src_sz), v_extra },
      cg_get_string(dst_var) ? cg_get_string(dst_var) : "clone_v");
  put_result(ctx, dst_var, res);
  return true;
}

// -------------------------------------------------------------
// P_prim_new: class instantiation — GC_malloc(sizeof(struct)).
// Without this, class-prototype globals stay null and attribute
// stores on class instances crash at runtime.
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
  // ifa/issues/030: stamp the classtag into the prototype; every
  // instance inherits it via emit_send_clone's memcpy.
  if (cg_has_classtag(t_sym)) {
    llvm::Value *tag_gep = Builder->CreateStructGEP(struct_ty, result, 0);
    Builder->CreateStore(get_classtag_global(t_sym->name), tag_gep);
  }
  put_result(ctx, dst_var, result);
  return true;
}

// -------------------------------------------------------------
// P_prim_make: tuple and list literals.
//
// The C backend uses _CG_prim_tuple(<type>, n) with a C
// preprocessor sizeof; at LLVM level we call the runtime's
// _CG_prim_tuple_list_internal(size, n) with a DataLayout size.
//
// Three sub-shapes:
//   1. Tuple (rvals[2]->sym in sym_tuple->specializers):
//      _CG_prim_tuple_list_internal(sizeof(struct), n) → dst,
//      then per-field GEP+Store for rvals[3..].
//   2. Struct-shape list (lvals[0]->type is Type_RECORD):
//      same as tuple path — same helper, same field stores.
//   3. Flat list (sym_list specializer, type NOT Type_RECORD):
//      _CG_prim_tuple_list_internal(sizeof(elem), n) → dst,
//      then per-index GEP+Store for rvals[3..].

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
    if (!struct_ty || !struct_ty->isSized()) return false;
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
  if (!elem_ty || elem_ty->isVoidTy() || !elem_ty->isSized()) elem_ty = i64;
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
// is_const_folded_send moved to codegen_common.cc
// -------------------------------------------------------------

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
    // Function value moved into a variable (issues/007 split
    // identity: `public_name = internal_fn`, or `f = g` for a
    // def'd g): materialize the function's address, matching the
    // C backend's representation and the value-identity dispatch
    // comparisons. Deliberately done HERE and not in
    // value_for_var: other fn-Sym reads (e.g. builtin-class
    // prototype attribute stores) historically no-op on null, and
    // materializing addresses there pollutes vector prototypes.
    if (!src && rhs->sym && rhs->sym->is_fun && rhs->sym->fun && rhs->sym->fun->cg_string)
      src = TheModule->getFunction(rhs->sym->fun->cg_string);
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

// emit_send_call is defined after LLVMEmitter (which calls it).
void emit_send_call(EmitCtx &ctx, PNode *pn);

// Code_SEND dispatcher — wraps virtual_cg_emit_send with LLVMEmitter.
// P_prim_reply is short-circuited in virtual_cg_emit_send before
// reaching any emitter hook; constant-folded SENDs are also skipped there.

// -------------------------------------------------------------
// P_prim_primitive: name-based runtime dispatch.
// rvals[1]->sym->name (or ->constant) is the primitive name.
// Default: call _CG_<name>(args).
// Special: __pyc_c_call__(ret_type, fn_name, (type, arg)*) for FFI.
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
    // A leading "::" (e.g. "::exit") is a C++ global-scope qualifier for
    // the C backend's generated C++ output (so it reaches the real libc
    // symbol rather than any shadowing name) -- meaningless as an LLVM/C
    // symbol name. Without stripping it, the LLVM backend declares and
    // calls a function literally named "::exit", which never links
    // against the real `exit` (issues/013 found this via `exit()`,
    // called from the frontend's new assert-fail lowering).
    if (fn_name[0] == ':' && fn_name[1] == ':') fn_name += 2;

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
    if (strcmp(fn_name, "__pyc_net_wait_read__") == 0 || strcmp(fn_name, "__pyc_net_wait_write__") == 0) {
      if (args.size() < 1) return false;
      llvm::Value *fd = args[0];
      
      llvm::Type *void_ty = llvm::Type::getVoidTy(*TheContext);
      std::vector<llvm::Type*> io_tys = {
        llvm::PointerType::getUnqual(*TheContext),
        fd->getType(),
        llvm::Type::getInt32Ty(*TheContext)
      };
      llvm::Function *reg_fn = get_runtime_helper("_CG_event_loop_register_io", void_ty, io_tys);
      
      int event = (strcmp(fn_name, "__pyc_net_wait_read__") == 0) ? 1 : 4;
      Builder->CreateCall(reg_fn, {ctx.coro_hdl, fd, Builder->getInt32(event)});

      llvm::Function *save_fn = get_intrinsic_decl(llvm::Intrinsic::coro_save);
        llvm::Value *save_res = Builder->CreateCall(save_fn, {ctx.coro_hdl});
        llvm::Function *suspend_fn = get_intrinsic_decl(llvm::Intrinsic::coro_suspend);
        llvm::Value *suspend_res = Builder->CreateCall(suspend_fn, {save_res, Builder->getFalse()});

      ensure_coro_suspend_destroy_bbs(ctx);

      llvm::BasicBlock *resume_bb = llvm::BasicBlock::Create(*TheContext, "resume", ctx.llvm_fn);

      llvm::SwitchInst *sw = Builder->CreateSwitch(suspend_res, ctx.coro_suspend_bb, 2);
      sw->addCase(Builder->getInt8(0), resume_bb);
      sw->addCase(Builder->getInt8(1), ctx.coro_destroy_bb);
      
      Builder->SetInsertPoint(resume_bb);

      if (pn->lvals.n > 0 && pn->lvals.v[0] && !ret_ty->isVoidTy()) {
        llvm::Value *zero = llvm::ConstantInt::get(ret_ty, 0);
        put_result(ctx, pn->lvals.v[0], zero);
      }
      return true;
    }

    llvm::Function *fn = get_runtime_helper(fn_name, ret_ty, param_tys);
    llvm::Value *res = Builder->CreateCall(fn, args);
    if (pn->lvals.n > 0 && pn->lvals.v[0] && !ret_ty->isVoidTy()) {
      put_result(ctx, pn->lvals.v[0], res);
    }
    return true;
  }

  // __pyc_format_string__(fmt, arg) → _CG_format_string(fmt, [fields...])
  // Mirrors format_string_codegen in python_ifa_main.cc: rvals[2]=fmt,
  // rvals[3]=arg (single value or struct/record tuple).
  if (strcmp(name, "__pyc_format_string__") == 0) {
    if (pn->rvals.n < 4) return false;
    Var *fmt_var = pn->rvals.v[2];
    Var *arg_var = pn->rvals.v[3];
    if (!fmt_var || !arg_var) return false;
    llvm::Value *fmt = value_for_var(ctx, fmt_var);
    if (!fmt) return false;
    llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
    // Collect args: fmt + expanded tuple fields (or single arg).
    std::vector<llvm::Value *> args;
    args.push_back(fmt);
    if (arg_var->type && arg_var->type->type_kind == Type_RECORD &&
        arg_var->type->has.n > 0) {
      llvm::StructType *rec_ty = sym_to_llvm_struct(arg_var->type);
      llvm::Value *rec = value_for_var(ctx, arg_var);
      if (!rec || !rec_ty) return false;
      for (int fi = 0; fi < (int)rec_ty->getNumElements(); fi++) {
        llvm::Type *ft = rec_ty->getElementType(fi);
        llvm::Value *gep = Builder->CreateStructGEP(rec_ty, rec, fi);
        args.push_back(Builder->CreateLoad(ft, gep));
      }
    } else {
      llvm::Value *av = value_for_var(ctx, arg_var);
      if (!av) return false;
      args.push_back(av);
    }
    // Declare as varargs: ptr (char *str, ...)
    llvm::FunctionType *ft = llvm::FunctionType::get(
        ptr_ty, {ptr_ty}, /*isVarArg=*/true);
    llvm::FunctionCallee fn = TheModule->getOrInsertFunction(
        "_CG_format_string", ft);
    llvm::Value *res = Builder->CreateCall(ft, fn.getCallee(), args);
    if (pn->lvals.n > 0 && pn->lvals.v[0])
      put_result(ctx, pn->lvals.v[0], res);
    return true;
  }

  if (strcmp(name, "__pyc_to_str__") == 0) {
    if (pn->rvals.n >= 3) {
      Var *v = pn->rvals.v[2];
      char buf[256];
      if (v && v->type && v->type->is_meta_type && v->type->name) {
        snprintf(buf, sizeof(buf), "<class '%s'>", v->type->name);
      } else {
        snprintf(buf, sizeof(buf), "<instance>");
      }
      llvm::Value *cv = materialize_pyc_string(dupstr(buf));
      if (pn->lvals.n > 0 && pn->lvals.v[0]) {
        put_result(ctx, pn->lvals.v[0], cv);
      }
      return true;
    }
  }

  // print / println: emit type-specific printf calls, mirroring
  // the C backend's cg_writeln.  Using a single typed _CG_println
  // declaration causes LLVM verification failures when arguments
  // are i1, double, or ptr (type mismatch against the first-seen
  // i32 declaration).
  if (strcmp(name, "print") == 0 || strcmp(name, "println") == 0) {
    bool do_nl = (strcmp(name, "println") == 0);
    llvm::Type *i32_ty = llvm::Type::getInt32Ty(*TheContext);
    llvm::Type *i64_ty = llvm::Type::getInt64Ty(*TheContext);
    llvm::Type *dbl_ty = llvm::Type::getDoubleTy(*TheContext);
    llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
    llvm::FunctionType *printf_ty =
        llvm::FunctionType::get(i32_ty, {ptr_ty}, /*isVarArg=*/true);
    llvm::FunctionCallee printf_fn =
        TheModule->getOrInsertFunction("printf", printf_ty);

    for (int i = 2; i < pn->rvals.n; i++) {
      Var *v = pn->rvals.v[i];
      if (!v) continue;
      bool last_nl = (i == pn->rvals.n - 1) && do_nl;
      Sym *t = v->type;

      // For string constants, materialise the char* directly from the
      // constant Sym so we bypass value_for_var returning ptr null
      // (which happens when FA leaves v->type unresolved for literals).
      Sym *cs = get_constant(v);
      bool is_str = (t == sym_string) ||
                    (sym_string && t && sym_string->specializers.set_in(t)) ||
                    (cs && cs->imm.const_kind == IF1_CONST_KIND_STRING);
      if (is_str) {
        llvm::Value *str_val = nullptr;
        if (cs && cs->imm.const_kind == IF1_CONST_KIND_STRING && cs->imm.v_string)
          str_val = materialize_pyc_string(cs->imm.v_string);
        if (!str_val) str_val = value_for_var(ctx, v);
        if (str_val) {
          cchar *fmt = last_nl ? "%s\n" : "%s";
          llvm::Value *fmt_v = Builder->CreateGlobalStringPtr(fmt, ".fmt");
          // str_val can be a compile-time or runtime null pointer when
          // FA leaves the Var's type unresolved (see comment above).
          // printf's "%s" NULL -> "(null)" behavior is a non-portable
          // glibc extension that LLVM's libcall simplification
          // (printf("%s\n", x) -> puts(x)) silently defeats at -O2 --
          // puts() has no such NULL handling and segfaults. Never pass
          // a possibly-null pointer to printf; substitute a literal
          // "(null)" string ourselves instead.
          llvm::Value *null_str =
              Builder->CreateGlobalStringPtr("(null)", ".nullstr");
          llvm::Value *is_null = Builder->CreateICmpEQ(
              str_val, llvm::ConstantPointerNull::get(
                           llvm::cast<llvm::PointerType>(str_val->getType())));
          llvm::Value *safe_str =
              Builder->CreateSelect(is_null, null_str, str_val);
          Builder->CreateCall(printf_ty, printf_fn.getCallee(), {fmt_v, safe_str});
        }
        continue;
      }

      llvm::Value *val = value_for_var(ctx, v);
      if (!val) return false;
      llvm::Type *vty = val->getType();

      if (vty->isIntegerTy(1)) {
        // Boolean: zero-extend to i32, print as unsigned.
        cchar *fmt = last_nl ? "%u\n" : "%u";
        llvm::Value *fmt_v = Builder->CreateGlobalStringPtr(fmt, ".fmt");
        llvm::Value *ext = Builder->CreateZExt(val, i32_ty, "zext");
        Builder->CreateCall(printf_ty, printf_fn.getCallee(), {fmt_v, ext});
      } else if (vty->isIntegerTy(64)) {
        bool is_signed = !(t == sym_uint64);
        cchar *fmt = last_nl ? (is_signed ? "%lld\n" : "%llu\n")
                             : (is_signed ? "%lld"   : "%llu");
        llvm::Value *fmt_v = Builder->CreateGlobalStringPtr(fmt, ".fmt");
        Builder->CreateCall(printf_ty, printf_fn.getCallee(), {fmt_v, val});
      } else if (vty->isIntegerTy()) {
        bool is_signed = !(t == sym_bool || t == sym_uint8 || t == sym_uint16 ||
                           t == sym_uint32);
        cchar *fmt = last_nl ? (is_signed ? "%d\n" : "%u\n")
                             : (is_signed ? "%d"   : "%u");
        llvm::Value *fmt_v = Builder->CreateGlobalStringPtr(fmt, ".fmt");
        llvm::Value *ext =
            is_signed ? Builder->CreateSExt(val, i32_ty, "sext")
                      : Builder->CreateZExt(val, i32_ty, "zext");
        Builder->CreateCall(printf_ty, printf_fn.getCallee(), {fmt_v, ext});
      } else if (vty->isFloatingPointTy()) {
        cchar *fmt = last_nl ? "%g\n" : "%g";
        llvm::Value *fmt_v = Builder->CreateGlobalStringPtr(fmt, ".fmt");
        llvm::Value *dv =
            vty->isDoubleTy() ? val
                              : Builder->CreateFPExt(val, dbl_ty, "fpext");
        Builder->CreateCall(printf_ty, printf_fn.getCallee(), {fmt_v, dv});
      } else {
        // Pointer or other opaque type: skip (unsupported).
        cchar *ph = last_nl ? "<unsupported>\n" : "<unsupported>";
        llvm::Value *fmt_v = Builder->CreateGlobalStringPtr(ph, ".fmt");
        Builder->CreateCall(printf_ty, printf_fn.getCallee(), {fmt_v});
      }
    }

    if (pn->rvals.n < 3 && do_nl) {
      // Empty println(): just a newline.
      llvm::Value *nl = Builder->CreateGlobalStringPtr("\n", ".nl");
      Builder->CreateCall(printf_ty, printf_fn.getCallee(), {nl});
    }

    if (pn->lvals.n > 0 && pn->lvals.v[0]) {
      llvm::Type *ret_ty = sym_to_llvm_type(pn->lvals.v[0]->type);
      if (ret_ty && !ret_ty->isVoidTy())
        put_result(ctx, pn->lvals.v[0], llvm::Constant::getNullValue(ret_ty));
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

class LLVMEmitter : public VirtualCGEmitter {
  EmitCtx &ctx;
 public:
  LLVMEmitter(EmitCtx &ctx) : ctx(ctx) {}
  void emit_move(PNode *pn) override { ::emit_move(ctx, pn); }
  bool emit_send_unaryop(PNode *pn) override { return ::emit_send_unaryop(ctx, pn); }
  bool emit_send_binop(PNode *pn) override { return ::emit_send_binop(ctx, pn); }
  bool emit_send_period(PNode *pn) override { return ::emit_send_period(ctx, pn); }
  bool emit_send_setter(PNode *pn) override { return ::emit_send_setter(ctx, pn); }
  bool emit_send_new(PNode *pn) override { return ::emit_send_new(ctx, pn); }
  bool emit_send_clone(PNode *pn) override { return ::emit_send_clone(ctx, pn); }
  bool emit_send_len(PNode *pn) override { return ::emit_send_len(ctx, pn); }
  bool emit_send_strcat(PNode *pn) override { return ::emit_send_strcat(ctx, pn); }
  bool emit_send_is(PNode *pn) override { return ::emit_send_is(ctx, pn); }
  bool emit_send_coerce(PNode *pn) override { return ::emit_send_coerce(ctx, pn); }
  bool emit_send_make(PNode *pn) override { return ::emit_send_make(ctx, pn); }
  bool emit_send_index_load(PNode *pn) override { return ::emit_send_index_load(ctx, pn); }
  bool emit_send_index_store(PNode *pn) override { return ::emit_send_index_store(ctx, pn); }
  bool emit_send_sizeof(PNode *pn) override { return ::emit_send_sizeof(ctx, pn); }
  bool emit_send_primitive(PNode *pn) override { return ::emit_send_primitive(ctx, pn); }
  bool emit_send_default_prim(PNode *pn) override { return ::emit_send_default_prim(ctx, pn); }
  void emit_send_call(PNode *pn) override { ::emit_send_call(ctx, pn); }
  
  bool emit_send_any_prim(PNode *pn) override {
    if (!pn || !pn->prim) return false;
    if (pn->prim->index == P_prim_id) {
      // id(x): address (pointers) or value bits (ints) as i64 --
      // mirrors write_c_prim's `(_CG_int64)(uintptr_t)x` (cg.cc).
      if (pn->lvals.n < 1 || pn->rvals.n < 3) return false;
      Var *dst_var = pn->lvals.v[0];
      llvm::Value *v = value_for_var(ctx, pn->rvals.v[2]);
      if (!dst_var || !v) return false;
      llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
      llvm::Value *res;
      if (v->getType()->isPointerTy())
        res = Builder->CreatePtrToInt(v, i64, "id");
      else if (v->getType()->isIntegerTy())
        res = Builder->CreateZExtOrTrunc(v, i64, "id");
      else if (v->getType()->isDoubleTy() || v->getType()->isFloatTy())
        res = Builder->CreateBitCast(
            v->getType()->isDoubleTy() ? v : Builder->CreateFPExt(v, llvm::Type::getDoubleTy(*TheContext)), i64,
            "id");
      else
        return false;
      put_result(ctx, dst_var, res);
      return true;
    }
    if (pn->prim->index == P_prim_await) {

      if (ctx.coro_hdl) {
        llvm::Function *save_fn = get_intrinsic_decl(llvm::Intrinsic::coro_save);
        llvm::Value *save_res = Builder->CreateCall(save_fn, {ctx.coro_hdl});
        llvm::Function *suspend_fn = get_intrinsic_decl(llvm::Intrinsic::coro_suspend);
        llvm::Value *suspend_res = Builder->CreateCall(suspend_fn, {save_res, Builder->getFalse()});
        
        // See ensure_coro_suspend_destroy_bbs: suspend_ret_bb must not
        // call llvm.coro.end (it isn't a real end, just "suspended,
        // resume me later"), and destroy_bb gets its own
        // non-fallthrough coro.end.
        ensure_coro_suspend_destroy_bbs(ctx);

        llvm::BasicBlock *resume_bb = llvm::BasicBlock::Create(*TheContext, "resume", ctx.llvm_fn);

        llvm::SwitchInst *sw = Builder->CreateSwitch(suspend_res, ctx.coro_suspend_bb, 2);
        sw->addCase(Builder->getInt8(0), resume_bb);
        sw->addCase(Builder->getInt8(1), ctx.coro_destroy_bb);
        
        Builder->SetInsertPoint(resume_bb);
      }
      return true;
    }
    return false;
  }
};

void emit_send(EmitCtx &ctx, PNode *pn) {
  LLVMEmitter emitter(ctx);
  virtual_cg_emit_send(&emitter, pn);
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
  if (!callees) return;
  if (callees->n > 1) {
    // ifa/issues/030 classtag dispatch (mirrors cg.cc's
    // emit_send_call polymorphic branch). Group candidates by
    // receiver class; branch on the instance's classtag (slot 0);
    // each class branch calls through THAT class's own stored
    // method pointer, so per-creation-site clones keep working.
    Vec<Sym *> classes;
    Vec<int> slots;
    Fun *nil_fn = nullptr;  // nil-receiver candidate (None method on a nil|record union)
    llvm::Value *recv = nullptr;
    // issue 026 pre-pass + classtag/nil-receiver resolution: shared
    // with the C backend (codegen_common.{h,cc}) -- previously
    // independently duplicated here and in cg.cc, independently
    // re-fixed for issue 026. Only this resolution algorithm was ever
    // identical between backends; the emission below (and the total
    // absence, here, of cg.cc's plain-function/untagged-direct
    // fallback for a candidate that resolves to neither route) is
    // backend-specific and unchanged by this refactor.
    Vec<cchar *> directly_owned;
    poly_dispatch_directly_owned(callees, directly_owned);
    bool ok = true;
    for (int fi = 0; fi < callees->n && ok; fi++) {
      Fun *fun_val = (*callees)[fi];
      if (!fun_val || !fun_val->sym || !fun_val->sym->name) { ok = false; break; }
      Vec<Sym *> rts;
      Vec<int> rt_slots;
      Vec<int> rt_ridxs;
      poly_dispatch_classtag_targets(fun_val, pn, directly_owned, rts, rt_slots, rt_ridxs);
      for (int ri = 0; ri < rts.n; ri++) {
        if (!recv && rt_ridxs[ri] >= 0 && rt_ridxs[ri] < pn->rvals.n && pn->rvals[rt_ridxs[ri]])
          recv = value_for_var(ctx, pn->rvals[rt_ridxs[ri]]);
      }
      if (!rts.n) {
        // Nil-receiver candidate: a None-class method reached
        // through a nil|record union (`if not self.field:` where the
        // field starts as None). None is a NULL pointer at runtime --
        // no classtag -- so it dispatches via a null test emitted
        // before the tag load (which also makes the tag load
        // null-safe).
        int nil_ridx = -1;
        bool is_nil_recv = poly_dispatch_is_nil_receiver(fun_val, pn, &nil_ridx);
        if (is_nil_recv && !recv && nil_ridx >= 0 && nil_ridx < pn->rvals.n && pn->rvals[nil_ridx])
          recv = value_for_var(ctx, pn->rvals[nil_ridx]);
        if (is_nil_recv && !nil_fn && fun_val->cg_string && TheModule->getFunction(fun_val->cg_string)) {
          nil_fn = fun_val;
          continue;
        }
      }
      if (!rts.n) { ok = false; break; }
      bool added_any = false;
      for (int ri = 0; ri < rts.n; ri++) {
        Sym *rt = rts[ri];
        if (!rt->name || rt->is_system_type || !cg_has_classtag(rt)) continue;
        added_any = true;
        bool found = false;
        for (int ci = 0; ci < classes.n; ci++)
          if (!strcmp(classes[ci]->name, rt->name)) { found = true; break; }
        if (!found) {
          classes.add(rt);
          slots.add(rt_slots[ri]);
        }
      }
      if (!added_any) { ok = false; break; }
    }
    Var *dst_var = pn->lvals.n ? pn->lvals.v[0] : nullptr;
    llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
    llvm::Type *res_ty = (dst_var && dst_var->type) ? sym_to_llvm_type(dst_var->type) : nullptr;
    if (res_ty && res_ty->isVoidTy()) res_ty = nullptr;
    llvm::Function *cur_fn = ctx.llvm_fn;
    if (ok && classes.n && recv) {
      llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(*TheContext, "poly.merge", cur_fn);
      llvm::AllocaInst *res_slot = nullptr;
      if (res_ty) {
        llvm::IRBuilder<> tmp(&cur_fn->getEntryBlock(), cur_fn->getEntryBlock().begin());
        res_slot = tmp.CreateAlloca(res_ty, nullptr, "poly.res");
      }
      if (nil_fn) {
        // Null test first: selects the None method and keeps the
        // classtag load below null-safe.
        llvm::Function *nlf = TheModule->getFunction(nil_fn->cg_string);
        llvm::BasicBlock *nil_bb = llvm::BasicBlock::Create(*TheContext, "poly.nil", cur_fn);
        llvm::BasicBlock *nonnull_bb = llvm::BasicBlock::Create(*TheContext, "poly.nonnull", cur_fn);
        llvm::Value *isnull = Builder->CreateICmpEQ(
            recv, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty)), "nilcmp");
        Builder->CreateCondBr(isnull, nil_bb, nonnull_bb);
        Builder->SetInsertPoint(nil_bb);
        // Route live non-fun formals from the call-site rvals
        // (None methods usually ignore self, so this is often empty).
        std::vector<llvm::Value *> nargs;
        llvm::FunctionType *nft = nlf->getFunctionType();
        unsigned nai = 0;
        MPosition nargp;
        nargp.push(1);
        for (int pi = 0; pi < nil_fn->sym->has.n + 2; pi++) {
          MPosition *cp = cannonicalize_mposition(nargp);
          nargp.inc();
          Var *av = nil_fn->args.get(cp);
          if (!av || !av->live) continue;
          if (av->type && av->type->is_fun) continue;
          if (nai >= nft->getNumParams()) break;
          int i = (int)Position2int(cp->pos[0]) - 1;
          if (i < 0 || i >= pn->rvals.n) break;
          llvm::Value *aval = value_for_var(ctx, pn->rvals[i]);
          if (!aval) break;
          llvm::Type *pt = nft->getParamType(nai);
          if (aval->getType() != pt) {
            if (aval->getType()->isIntegerTy() && pt->isIntegerTy())
              aval = Builder->CreateSExtOrTrunc(aval, pt);
            else if (aval->getType()->isPointerTy() && pt->isIntegerTy())
              aval = Builder->CreatePtrToInt(aval, pt);
            else if (aval->getType()->isIntegerTy() && pt->isPointerTy())
              aval = Builder->CreateIntToPtr(aval, pt);
          }
          nargs.push_back(aval);
          nai++;
        }
        if (nargs.size() == nft->getNumParams()) {
          llvm::Value *ncall = Builder->CreateCall(nlf, nargs);
          if (res_slot && !nlf->getReturnType()->isVoidTy()) {
            llvm::Value *nres = ncall;
            if (nres->getType() != res_ty) {
              if (nres->getType()->isIntegerTy() && res_ty->isIntegerTy())
                nres = Builder->CreateSExtOrTrunc(nres, res_ty);
              else if (nres->getType()->isPointerTy() && res_ty->isIntegerTy())
                nres = Builder->CreatePtrToInt(nres, res_ty);
              else if (nres->getType()->isIntegerTy() && res_ty->isPointerTy())
                nres = Builder->CreateIntToPtr(nres, res_ty);
            }
            if (nres->getType() == res_ty) Builder->CreateStore(nres, res_slot);
          }
        }
        Builder->CreateBr(merge_bb);
        Builder->SetInsertPoint(nonnull_bb);
      }
      // Load the instance's classtag (slot 0 of any tagged struct).
      llvm::Value *tag = Builder->CreateLoad(ptr_ty, recv, "classtag");
      for (int ci = 0; ci < classes.n; ci++) {
        llvm::BasicBlock *hit_bb = llvm::BasicBlock::Create(*TheContext, "poly.hit", cur_fn);
        llvm::BasicBlock *next_bb = llvm::BasicBlock::Create(*TheContext, "poly.next", cur_fn);
        llvm::Value *cmp = Builder->CreateICmpEQ(tag, get_classtag_global(classes[ci]->name), "tagcmp");
        Builder->CreateCondBr(cmp, hit_bb, next_bb);
        Builder->SetInsertPoint(hit_bb);
        llvm::StructType *st = sym_to_llvm_struct(classes[ci]);
        llvm::Value *slot_gep = Builder->CreateStructGEP(st, recv, llvm_fld(classes[ci], slots[ci]));
        llvm::Value *fnptr = Builder->CreateLoad(ptr_ty, slot_gep, "methodptr");
        llvm::FunctionType *fty =
            llvm::FunctionType::get(res_ty ? res_ty : llvm::Type::getVoidTy(*TheContext), {ptr_ty}, false);
        llvm::Value *callv = Builder->CreateCall(fty, fnptr, {recv});
        if (res_slot) Builder->CreateStore(callv, res_slot);
        Builder->CreateBr(merge_bb);
        Builder->SetInsertPoint(next_bb);
      }
      // Fallthrough (unknown tag): trap-free no-op, mirror the C
      // backend's assert by leaving the result zeroed.
      Builder->CreateBr(merge_bb);
      Builder->SetInsertPoint(merge_bb);
      if (dst_var && res_slot) {
        llvm::Value *res = Builder->CreateLoad(res_ty, res_slot, "poly.val");
        put_result(ctx, dst_var, res);
      }
      return;
    }
    // ifa/issues/030 "bare callable value" extension (issue 007's
    // shape; mirrors cg.cc's emit_send_call): no receiver object to
    // read a classtag from -- the callable ITSELF is the polymorphic
    // value. Raw function values are function addresses, so dispatch
    // by VALUE IDENTITY with a direct call per candidate.
    Var *v0 = pn->rvals.v[0];
    if (!v0 || !v0->sym || v0->sym->is_symbol) return;
    llvm::Value *fv0 = value_for_var(ctx, v0);
    // Issue 036: a callable that is a compile-time function
    // constant (sym->fun) has no runtime slot in the LLVM world
    // (fun-typed values are excluded from signatures and storage),
    // so value_for_var returns nothing and this send used to be
    // DROPPED — expr_evaluator's recursive calls vanished from the
    // clones whose callable resolved to a constant, leaving their
    // l/r slots uninitialized (returned 0). Use the function's own
    // address as the identity operand instead; the icmp chain below
    // then folds statically, mirroring the C backend's cg-string
    // comparison chain.
    if ((!fv0 || !fv0->getType()->isPointerTy()) && v0->sym->fun && v0->sym->fun->cg_string)
      fv0 = TheModule->getFunction(v0->sym->fun->cg_string);
    if (!fv0 || !fv0->getType()->isPointerTy()) return;
    std::vector<llvm::Function *> cands;
    for (int fi = 0; fi < callees->n; fi++) {
      Fun *fv = (*callees)[fi];
      if (!fv || !fv->cg_string) return;
      llvm::Function *lf = TheModule->getFunction(fv->cg_string);
      if (!lf) return;
      cands.push_back(lf);
    }
    llvm::BasicBlock *fmerge_bb = llvm::BasicBlock::Create(*TheContext, "fnid.merge", cur_fn);
    llvm::AllocaInst *fres_slot = nullptr;
    if (res_ty) {
      llvm::IRBuilder<> tmp(&cur_fn->getEntryBlock(), cur_fn->getEntryBlock().begin());
      fres_slot = tmp.CreateAlloca(res_ty, nullptr, "fnid.res");
    }
    for (int fi = 0; fi < callees->n; fi++) {
      Fun *fv = (*callees)[fi];
      llvm::Function *lf = cands[fi];
      llvm::BasicBlock *hit_bb = llvm::BasicBlock::Create(*TheContext, "fnid.hit", cur_fn);
      llvm::BasicBlock *next_bb = llvm::BasicBlock::Create(*TheContext, "fnid.next", cur_fn);
      llvm::Value *cmp = Builder->CreateICmpEQ(fv0, lf, "fncmp");
      Builder->CreateCondBr(cmp, hit_bb, next_bb);
      Builder->SetInsertPoint(hit_bb);
      // Route live formals from the call-site rvals, coercing to
      // the callee's declared param types.
      std::vector<llvm::Value *> args;
      llvm::FunctionType *lft = lf->getFunctionType();
      bool args_ok = true;
      unsigned ai = 0;
      MPosition argp;
      argp.push(1);
      for (int pi = 0; pi < fv->sym->has.n + 2 && args_ok; pi++) {
        MPosition *cp = cannonicalize_mposition(argp);
        argp.inc();
        Var *av = fv->args.get(cp);
        if (!av || !av->live) continue;
        // Fun-typed formals are excluded from LLVM signatures
        // (mirrors build_fun_signature / the single-target path).
        if (av->type && av->type->is_fun) continue;
        if (ai >= lft->getNumParams()) break;
        int i = (int)Position2int(cp->pos[0]) - 1;
        if (i < 0 || i >= pn->rvals.n) { args_ok = false; break; }
        llvm::Value *aval = value_for_var(ctx, pn->rvals[i]);
        if (!aval) { args_ok = false; break; }
        if (ai < lft->getNumParams()) {
          llvm::Type *pt = lft->getParamType(ai);
          if (aval->getType() != pt) {
            if (aval->getType()->isIntegerTy() && pt->isIntegerTy())
              aval = Builder->CreateSExtOrTrunc(aval, pt);
            else if (aval->getType()->isPointerTy() && pt->isIntegerTy())
              aval = Builder->CreatePtrToInt(aval, pt);
            else if (aval->getType()->isIntegerTy() && pt->isPointerTy())
              aval = Builder->CreateIntToPtr(aval, pt);
          }
        }
        args.push_back(aval);
        ai++;
      }
      if (args_ok) {
        llvm::Value *callv = Builder->CreateCall(lf, args);
        if (fres_slot && !callv->getType()->isVoidTy()) {
          llvm::Value *cv = callv;
          if (cv->getType() != res_ty) {
            if (cv->getType()->isIntegerTy() && res_ty->isIntegerTy())
              cv = Builder->CreateSExtOrTrunc(cv, res_ty);
            else if (cv->getType()->isPointerTy() && res_ty->isIntegerTy())
              cv = Builder->CreatePtrToInt(cv, res_ty);
            else if (cv->getType()->isIntegerTy() && res_ty->isPointerTy())
              cv = Builder->CreateIntToPtr(cv, res_ty);
          }
          Builder->CreateStore(cv, fres_slot);
        }
      }
      Builder->CreateBr(fmerge_bb);
      Builder->SetInsertPoint(next_bb);
    }
    Builder->CreateBr(fmerge_bb);
    Builder->SetInsertPoint(fmerge_bb);
    if (dst_var && fres_slot) {
      llvm::Value *res = Builder->CreateLoad(res_ty, fres_slot, "fnid.val");
      put_result(ctx, dst_var, res);
    }
    return;
  }
  Fun *target = callees->v[0];
  if (!target || !target->cg_string) return;
  llvm::Function *target_fn =
      TheModule->getFunction(target->cg_string);

  if (!target_fn) return;

  // Closure detection: rvals[0] is a closure receiver when
  // its type is Type_FUN with has.n ≥ 2 (closure_fun_type
  // from codegen_common; also looks through a nullable
  // SUM{nil_type, closure} — issues/002 Case B).  Unpack via
  // FIELD_LOAD per closure-struct field that maps to a formal.
  Var *v0 = pn->rvals.n > 0 ? pn->rvals.v[0] : nullptr;
  Sym *v0_closure_type = v0 ? closure_fun_type(v0) : nullptr;
  bool v0_is_closure = v0_closure_type != nullptr;
  llvm::Value *closure = nullptr;
  llvm::StructType *closure_struct = nullptr;
  if (v0_is_closure) {
    closure = value_for_var(ctx, v0);
    closure_struct = sym_to_llvm_struct(v0_closure_type);
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

    if (v0_is_closure) {
      if (i < v0_closure_type->has.n) {
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
        i -= v0_closure_type->has.n - 1;
      }
    }
    if (i < 0 || i >= pn->rvals.n) continue;
    Var *actual = pn->rvals.v[i];
    if (!actual) continue;
    llvm::Value *val = value_for_var(ctx, actual);
    if (!val) {
      return;
    }
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

// Given a Code_IF PNode, returns the single cfg_succ index (0 or 1)
// selected by a compile-time-constant condition -- the condition
// Var's ->sym is FA's own canonical true_type/false_type Sym, either
// from FA's ordinary constant folding or fed in by a pass like
// ifa/optimize/exc_check_fold.cc -- or -1 if the condition is a real
// runtime value (both successors are potentially live). Shared by
// discover_blocks (which must not allocate/walk into the dead arm at
// all) and emit_block_terminator (which must not branch into it),
// so the two can't drift out of sync.
static int const_if_successor(EmitCtx &ctx, PNode *n) {
  if (!n || n->rvals.n == 0) return -1;
  if (ctx.fa && ctx.fa->type_world.true_type && n->rvals.v[0]->sym == ctx.fa->type_world.true_type->v[0]->sym)
    return 0;
  if (ctx.fa && ctx.fa->type_world.false_type && n->rvals.v[0]->sym == ctx.fa->type_world.false_type->v[0]->sym)
    return 1;
  return -1;
}

// -------------------------------------------------------------
// Block-terminator emit: given a "closer" PNode (one whose cfg_succ
// exits the current block), emit the LLVM terminator.
//
// For Code_IF with both branches live, critical-edge split blocks
// (t_edge / f_edge) are inserted between the conditional and each
// target.  Without them, phi/phy moves intended for only one branch
// would execute in a block shared by both, corrupting the other branch.
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
      if (closer->live && closer->fa_live && closer->rvals.n > 0) {
        int only_succ = const_if_successor(ctx, closer);
        if (only_succ >= 0 && closer->cfg_succ.n > only_succ) {
          llvm::BasicBlock *bb = ctx.label_bb.get(closer->cfg_succ.v[only_succ]);
          if (bb) {
            emit_phy_moves(ctx, closer, only_succ);
            emit_phi_moves(ctx, closer, only_succ);
            Builder->CreateBr(bb);
            break;
          }
        }
      }
      llvm::Value *cond = nullptr;
      if (closer->rvals.n > 0)
        cond = value_for_var(ctx, closer->rvals.v[0]);
      // Truncate to i1 if needed.
      if (cond && !cond->getType()->isIntegerTy(1)) {
        cond = Builder->CreateICmpNE(cond,
            llvm::Constant::getNullValue(cond->getType()));
      }
      llvm::BasicBlock *t_bb = nullptr, *f_bb = nullptr;
      if (closer->cfg_succ.n > 0 && closer->cfg_succ.v[0]) {
        t_bb = ctx.label_bb.get(closer->cfg_succ.v[0]);
      }
      if (closer->cfg_succ.n > 1 && closer->cfg_succ.v[1]) {
        f_bb = ctx.label_bb.get(closer->cfg_succ.v[1]);
      }
      if (t_bb && f_bb) {
        if (cond) {
          llvm::BasicBlock *t_edge = llvm::BasicBlock::Create(*TheContext, "t_edge", ctx.llvm_fn);
          llvm::BasicBlock *f_edge = llvm::BasicBlock::Create(*TheContext, "f_edge", ctx.llvm_fn);
          Builder->CreateCondBr(cond, t_edge, f_edge);
          Builder->SetInsertPoint(t_edge);
          emit_phy_moves(ctx, closer, 0);
          emit_phi_moves(ctx, closer, 0);
          Builder->CreateBr(t_bb);
          Builder->SetInsertPoint(f_edge);
          emit_phy_moves(ctx, closer, 1);
          emit_phi_moves(ctx, closer, 1);
          Builder->CreateBr(f_bb);
        } else {
          Builder->CreateUnreachable();
        }
      } else if (t_bb && !f_bb) {
        emit_phy_moves(ctx, closer, 0);
        emit_phi_moves(ctx, closer, 0);
        Builder->CreateBr(t_bb);
      } else if (f_bb && !t_bb) {
        emit_phy_moves(ctx, closer, 1);
        emit_phi_moves(ctx, closer, 1);
        Builder->CreateBr(f_bb);
      } else {
        Builder->CreateUnreachable();
      }
      break;
    }
    case Code_SEND: {
      if (closer->prim && closer->prim->index == P_prim_reply) {
        if (ctx.fn->sym && ctx.fn->sym->is_async && ctx.coro_hdl && ctx.coro_id) {
          // Emitting coroutine epilogue
          llvm::Function *coro_free_fn = get_intrinsic_decl(llvm::Intrinsic::coro_free);
          llvm::Value *mem = Builder->CreateCall(coro_free_fn, {ctx.coro_id, ctx.coro_hdl});
          
          llvm::Function *free_fn = TheModule->getFunction("GC_free");
          if (!free_fn) {
            llvm::FunctionType *free_ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {llvm::PointerType::getUnqual(*TheContext)}, false);
            free_fn = llvm::Function::Create(free_ty, llvm::Function::ExternalLinkage, "GC_free", TheModule.get());
          }
          Builder->CreateCall(free_fn, {mem});
          
          llvm::Function *coro_end_fn = get_intrinsic_decl(llvm::Intrinsic::coro_end);
          Builder->CreateCall(coro_end_fn, {ctx.coro_hdl, Builder->getFalse(), llvm::ConstantTokenNone::get(*TheContext)});
          Builder->CreateRet(ctx.coro_hdl);
          break;
        }
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
// Alloca pre-pass: one alloca slot per phi-target Var.
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
  //
  // Known gap (not fixed, ifa/issues/011's dead-code-elimination
  // landing, 2026-07-18): unlike discover_blocks/emit_pnode/
  // emit_block_terminator, this walk has no const_if_successor
  // awareness -- it visits BOTH arms of a constant-condition Code_IF,
  // so a dead arm's phi/phy targets can get an alloca slot allocated
  // for them here even though nothing ever writes/reads it. Harmless
  // (LLVM's own -O2, which llvm_codegen_compile always runs
  // regardless of `-O`, drops unused allocas trivially) and left
  // alone deliberately -- fixing it would mean threading
  // const_if_successor through a FOURTH cfg_succ walk for a purely
  // cosmetic win, unlike the other three fixes which closed actual
  // correctness/dead-code gaps.
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
    // Code_MOVE: do NOT unite lhs~rhs here.  MOVE chains carry
    // values between SSU vars via register bindings (var_map);
    // uniting them through the union-find collapses distinct
    // function args that happen to flow to the same phi target
    // into one alloca slot, clobbering earlier args at entry.
    // phi/phy PNodes below already create all the sharing that
    // loops and joins actually need.
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
//
// A Code_IF PNode with a compile-time-constant condition (see
// const_if_successor) only enqueues its live successor -- matching
// emit_block_terminator's selection exactly, so the dead arm is
// never allocated a BasicBlock at all (previously it was: visible in
// a raw .ll dump as an orphaned "; No predecessors!" block, cleaned
// up only by the mandatory downstream `clang++ -O2` in
// llvm_codegen_compile, never by pyc's own emission).
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
    int only_succ = -1;
    if (cur->code && cur->code->kind == Code_IF && cur->live && cur->fa_live)
      only_succ = const_if_successor(ctx, cur);
    for (int si = 0; si < cur->cfg_succ.n; si++) {
      if (only_succ >= 0 && si != only_succ) continue;
      PNode *s = cur->cfg_succ.v[si];
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

// Per-PNode DFS emit.  Mirrors cg.cc:write_c_pnode structure.
// LABEL → switch IRBuilder insert point; MOVE/SEND → inline emit;
// IF/GOTO → emit_block_terminator.  A "closer" PNode (cfg_succ
// targets a Code_LABEL) always ends the current block.
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
  // Recurse to successors -- must agree with discover_blocks and
  // emit_block_terminator about which arm of a constant-condition
  // Code_IF is live, or this walk emits content for a PNode
  // discover_blocks never allocated a BasicBlock for (Builder stays
  // at the previous, already-terminated block, corrupting it: "LLVM
  // module verification failed: Terminator found in the middle of a
  // basic block").
  int only_succ = -1;
  if (pn->code->kind == Code_IF && pn->live && pn->fa_live) only_succ = const_if_successor(ctx, pn);
  for (int si = 0; si < pn->cfg_succ.n; si++) {
    if (only_succ >= 0 && si != only_succ) continue;
    PNode *s = pn->cfg_succ.v[si];
    if (s && done.set_add(s)) {
      emit_pnode(ctx, s, done);
    }
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
  if (f->sym && f->sym->is_async) {
    ret_ty = llvm::PointerType::getUnqual(*TheContext);
  }

  // Collect param types from positional formals, skipping
  // dead and is_fun (closure-self) formals.  Sub-positions
  // (pos.n > 1, i.e. pattern-match field extractions) are
  // excluded from the LLVM signature; they are materialised
  // by GEP + Load at function entry in emit_fun instead.
  // Order matches cg.cc's iteration so the per-position arg
  // routing in emit_send_call lines up.
  std::vector<llvm::Type *> param_tys;
  Vec<Var *> formal_vars;
  for (MPosition *p : f->positional_arg_positions) {
    if (p->pos.n > 1) continue;
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

  if (f->sym && f->sym->is_async) {
    llvm_fn->addFnAttr(llvm::Attribute::PresplitCoroutine);
  }

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

// Per-Fun emit: build signature, then body.
// -------------------------------------------------------------

void emit_fun(EmitCtx &ctx, Fun *f) {
  if (!f) return;
  if (!build_fun_signature(ctx, f)) return;
  if (f->is_external) return;  // body is in a linked library
  if (!ctx.llvm_fn) return;

  // Pre-pass 1: allocate one llvm::BasicBlock per Code_LABEL PNode
  // so emit_pnode can resolve branch targets without a second walk.
  discover_blocks(ctx, f);
  // Pre-pass 2: alloca slots for phi-target Vars (loop back-edges,
  // if/else joins) so cross-edge writes have stable storage.
  discover_phi_targets(ctx, f);
  llvm::BasicBlock *entry_bb = ctx.label_bb.get(f->entry);
  if (!entry_bb) {
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
  
  if (f->sym && f->sym->is_async) {
    llvm::Function *coro_id_fn = get_intrinsic_decl(llvm::Intrinsic::coro_id);
    ctx.coro_id = Builder->CreateCall(coro_id_fn, {
        Builder->getInt32(0),
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*TheContext)),
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*TheContext)),
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*TheContext))
    });

    llvm::Function *coro_size_fn = get_intrinsic_decl(llvm::Intrinsic::coro_size, {Builder->getInt64Ty()});
    llvm::Value *size = Builder->CreateCall(coro_size_fn, {});

    // Allocate frame using standard memory allocator (e.g., GC_malloc)
    llvm::Function *malloc_fn = TheModule->getFunction("GC_malloc");
    if (!malloc_fn) {
      llvm::FunctionType *malloc_ty = llvm::FunctionType::get(
          llvm::PointerType::getUnqual(*TheContext), {Builder->getInt64Ty()}, false);
      malloc_fn = llvm::Function::Create(malloc_ty, llvm::Function::ExternalLinkage, "GC_malloc", TheModule.get());
    }
    llvm::Value *alloc = Builder->CreateCall(malloc_fn, {size});

    llvm::Function *coro_begin_fn = get_intrinsic_decl(llvm::Intrinsic::coro_begin);
    ctx.coro_hdl = Builder->CreateCall(coro_begin_fn, {ctx.coro_id, alloc});

    // Initial suspend: calling an async function must build and
    // suspend its coroutine frame WITHOUT running any of the body --
    // matching Python's "calling an async def returns a suspended
    // coroutine object" semantics (mirrors the C backend's
    // `std::suspend_always initial_suspend()`, pyc_c_runtime.h).
    // Without this, the body below runs eagerly on the very call
    // that's supposed to just construct the coroutine, and only
    // genuinely suspends at its first real (I/O) await -- observed
    // as async functions with no I/O producing output immediately
    // on the initiating call, before anything ever resumes them.
    llvm::Function *save_fn = get_intrinsic_decl(llvm::Intrinsic::coro_save);
    llvm::Value *save_res = Builder->CreateCall(save_fn, {ctx.coro_hdl});
    llvm::Function *suspend_fn = get_intrinsic_decl(llvm::Intrinsic::coro_suspend);
    llvm::Value *suspend_res = Builder->CreateCall(suspend_fn, {save_res, Builder->getFalse()});

    ensure_coro_suspend_destroy_bbs(ctx);

    llvm::BasicBlock *coro_start_bb =
        llvm::BasicBlock::Create(*TheContext, "coro_start", ctx.llvm_fn);
    llvm::SwitchInst *sw = Builder->CreateSwitch(suspend_res, ctx.coro_suspend_bb, 2);
    sw->addCase(Builder->getInt8(0), coro_start_bb);
    sw->addCase(Builder->getInt8(1), ctx.coro_destroy_bb);

    // Everything from here on (arg storage, the body walk below) must
    // land in coro_start_bb, not entry_bb -- entry_bb now ends at the
    // switch above. f->entry's cfg_succ traversal reaches this same
    // code via ctx.label_bb, so redirect that mapping too: emit_pnode
    // only re-targets the builder for Code_LABEL kinds, but f->entry
    // may or may not be one, and nothing else should ever branch to
    // a function's own entry from inside the function, so remapping
    // it here is safe.
    Builder->SetInsertPoint(coro_start_bb);
    ctx.label_bb.put(f->entry, coro_start_bb);
  }

  // Store each argument into its alloca slot at function entry.
  // discover_phi_targets may assign an alloca to a Var that's also
  // a formal (e.g., a loop that modifies a parameter); without this
  // prestore the first loop iteration reads an uninitialized slot.
  Vec<Var *> bound_vars;
  ctx.var_map.get_keys(bound_vars);
  for (Var *v : bound_vars) {
    llvm::Value *arg_val = ctx.var_map.get(v);
    if (!arg_val || !llvm::isa<llvm::Argument>(arg_val)) continue;
    if (llvm::AllocaInst *slot = ctx.alloca_map.get(v)) {
      Builder->CreateStore(arg_val, slot);
    } else if (llvm::GlobalVariable *gv = g_var_to_global.get(v)) {
      Builder->CreateStore(arg_val, gv);
    }
  }

  // Pattern-arg extraction: for each sub-position formal (pos.n == 2),
  // GEP + Load the field from the parent struct pointer.  These vars
  // were excluded from the LLVM signature by build_fun_signature; they
  // must be materialised here so the function body can use them.
  for (MPosition *p : f->positional_arg_positions) {
    if (p->pos.n != 2) continue;
    Var *sub_var = f->args.get(p);
    if (!sub_var || !sub_var->live) continue;
    if (sub_var->type && sub_var->type->is_fun) continue;
    // Build the parent position (drop the last component).
    MPosition pp;
    pp.push((int)Position2int(p->pos.v[0]));
    MPosition *parent_pos = cannonicalize_mposition(pp);
    Var *parent_var = f->args.get(parent_pos);
    if (!parent_var || !parent_var->type) continue;
    llvm::Value *parent_val = ctx.var_map.get(parent_var);
    if (!parent_val) continue;
    llvm::StructType *struct_ty = sym_to_llvm_struct(parent_var->type);
    if (!struct_ty) continue;
    int field_idx = (int)Position2int(p->pos.v[1]) - 1;
    if (field_idx < 0 || field_idx >= (int)struct_ty->getNumElements()) continue;
    llvm::Value *gep =
        Builder->CreateStructGEP(struct_ty, parent_val, field_idx, "pat_gep");
    llvm::Type *field_ty = struct_ty->getElementType(field_idx);
    llvm::Value *loaded = Builder->CreateLoad(field_ty, gep, "pat");
    if (llvm::AllocaInst *slot = ctx.alloca_map.get(sub_var))
      Builder->CreateStore(loaded, slot);
    else
      ctx.var_map.put(sub_var, loaded);
  }

  // Rebuild cfg_pred_index so emit_phi_moves selects the correct
  // predecessor's rval (mirrors write_c_fun's call in cg.cc:909).
  rebuild_cfg_pred_index(f);

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
      Builder->CreateUnreachable();
    }
  }
}

}  // namespace

// -------------------------------------------------------------
// Public entry.
// -------------------------------------------------------------

bool cg_emit_llvm(FA *fa, Fun *main_fun) {
  if (!fa || !TheModule) return false;
  reset_state();

  // ifa/issues/029/030: creator-Fun -> method-slot registrations,
  // consumed by emit_send_clone (slot stores) and emit_send_call
  // (classtag dispatch). Shared with the C backend.
  cg_build_new_to_val_map(fa);

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
    ctx->fa = fa;
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
