// SPDX-License-Identifier: BSD-3-Clause
//
// cg_ir_v2_emit_llvm.cc — minimal LLVM emitter for CGv2Program.
// CG_IR_PLAN Phase 4 commit 2.
//
// First emit landing. Handles the test 01 shape (empty void
// function + CG_RET / CG_BR / CG_UNREACHABLE terminators). Each
// new test landing extends the per-op switch by exactly what
// it needs.
//
// Uses the existing file-scope LLVM Context/Module/Builder
// from llvm.cc (set up via llvm_codegen_initialize). This
// keeps the v2 emitter consistent with the rest of the LLVM
// codegen and avoids context-mismatch issues.

#include "ifadefs.h"

#include "codegen/cg_ir_v2.h"
#include "codegen/llvm_internal.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <memory>

extern std::unique_ptr<llvm::LLVMContext> TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::unique_ptr<llvm::IRBuilder<>> Builder;

namespace {

// CGv2Type → llvm::Type. v0: handles only the numeric / void
// kinds. STRUCT / FUN_PTR / etc. land with their test corpus
// expansions.
llvm::Type *to_llvm_type(CGv2Type *t) {
  if (!t) return llvm::Type::getVoidTy(*TheContext);
  switch (t->kind) {
    case CG2T_VOID:
      return llvm::Type::getVoidTy(*TheContext);
    case CG2T_BOOL:
      return llvm::Type::getInt1Ty(*TheContext);
    case CG2T_INT:
    case CG2T_UINT:
      switch (t->bits) {
        case 1:  return llvm::Type::getInt1Ty(*TheContext);
        case 8:  return llvm::Type::getInt8Ty(*TheContext);
        case 16: return llvm::Type::getInt16Ty(*TheContext);
        case 32: return llvm::Type::getInt32Ty(*TheContext);
        case 64: return llvm::Type::getInt64Ty(*TheContext);
        default: return nullptr;
      }
    case CG2T_FLOAT:
      switch (t->bits) {
        case 32: return llvm::Type::getFloatTy(*TheContext);
        case 64: return llvm::Type::getDoubleTy(*TheContext);
        default: return nullptr;
      }
    case CG2T_PTR:
    case CG2T_REF:
    case CG2T_FUN_PTR:
    case CG2T_SYMBOL:
    case CG2T_SUM:
      return llvm::PointerType::getUnqual(*TheContext);
    case CG2T_STRUCT:
      // Lands with test 08 (struct alloc + field access).
      return nullptr;
  }
  return nullptr;
}

llvm::FunctionType *to_llvm_fn_type(CGv2Sig *sig) {
  if (!sig) return nullptr;
  llvm::Type *ret = to_llvm_type(sig->ret);
  if (!ret) return nullptr;
  std::vector<llvm::Type *> args;
  for (CGv2Type *a : sig->args) {
    llvm::Type *at = to_llvm_type(a);
    if (!at) return nullptr;
    args.push_back(at);
  }
  return llvm::FunctionType::get(ret, args, sig->is_varargs);
}

// Per-CGFun emission state. Holds the LLVM function + block
// map. This is intentionally function-scoped — the
// per-CGFun value cache (issue 017 structural fix) lives here.
struct EmitFunCtx {
  CGv2Fun *cf;
  llvm::Function *llvm_fun;

  // Block map. Built before instruction emission so cross-
  // block terminator targets resolve.
  Map<CGv2Block *, llvm::BasicBlock *> blk_map;

  // Per-(CGv2Value*) cache. The CG_IR_v2 design's structural
  // issue-017 fix: value identity is function-scoped by
  // construction. v0 has no values to cache yet (test 01 is
  // empty); test 02+ populates this.
  Map<CGv2Value *, llvm::Value *> value_map;

  // Locals that are phi-by-pred destinations live in alloca
  // slots. Stored to from the predecessor's edge, loaded
  // fresh on each use site.
  Map<CGv2Value *, llvm::AllocaInst *> alloca_map;
};

// Resolve a CGv2Value to an llvm::Value usable in the current
// function. Constants materialize a ConstantInt/ConstantFP/etc.
// Locals, formals, and globals are looked up in the per-fun
// value_map (the issue-017 structural fix).
//
// Returns nullptr on unresolved value (caller emits undef).
llvm::Value *resolve_value(EmitFunCtx &ctx, CGv2Value *v) {
  if (!v) return nullptr;

  // Constants materialize fresh into the current function's
  // context every time. ConstantInt/ConstantFP are uniqued by
  // LLVM, so there's no cross-function leak risk.
  if (v->scope == CG2V_CONSTANT) {
    llvm::Type *t = to_llvm_type(v->type);
    if (!t) return nullptr;
    switch (v->imm.kind) {
      case CGv2Immediate::I_INT:
        if (auto *it = llvm::dyn_cast<llvm::IntegerType>(t))
          return llvm::ConstantInt::getSigned(it, v->imm.v.i);
        return nullptr;
      case CGv2Immediate::I_UINT:
        if (auto *it = llvm::dyn_cast<llvm::IntegerType>(t))
          return llvm::ConstantInt::get(it, v->imm.v.u, false);
        return nullptr;
      case CGv2Immediate::I_BOOL:
        return llvm::ConstantInt::get(
            llvm::Type::getInt1Ty(*TheContext),
            v->imm.v.b ? 1 : 0);
      case CGv2Immediate::I_FLOAT:
        return llvm::ConstantFP::get(t, v->imm.v.f);
      case CGv2Immediate::I_NIL:
        if (t->isPointerTy())
          return llvm::ConstantPointerNull::get(
              llvm::cast<llvm::PointerType>(t));
        return nullptr;
      case CGv2Immediate::I_UNDEF:
        return llvm::UndefValue::get(t);
      case CGv2Immediate::I_STR:
      case CGv2Immediate::I_SYM:
        // Land with their corresponding tests.
        return nullptr;
      case CGv2Immediate::I_NONE:
      default:
        return nullptr;
    }
  }

  // Phi-target locals live in an alloca slot. Each use emits
  // a fresh load. mem2reg/SROA will collapse redundant loads
  // back to register values during optimization.
  llvm::AllocaInst *slot = ctx.alloca_map.get(v);
  if (slot) {
    return Builder->CreateLoad(slot->getAllocatedType(), slot,
                                v->name ? v->name : "");
  }

  // Function-scoped lookup. Lands with formals/locals tests.
  return ctx.value_map.get(v);
}

// Store-to-slot if the dst is an alloca-backed local;
// otherwise alias through value_map. Used by both regular
// MOVE/BINOP body insts and phi-edge MOVEs.
void put_result(EmitFunCtx &ctx, CGv2Value *dst, llvm::Value *r) {
  if (!dst || !r) return;
  llvm::AllocaInst *slot = ctx.alloca_map.get(dst);
  if (slot) Builder->CreateStore(r, slot);
  else ctx.value_map.put(dst, r);
}

void emit_inst(CGv2Inst *inst, EmitFunCtx &ctx) {
  switch (inst->op) {
    case CG2_BINOP: {
      if (inst->rvals.n < 2 || inst->lvals.n < 1) return;
      llvm::Value *a = resolve_value(ctx, inst->rvals[0]);
      llvm::Value *b = resolve_value(ctx, inst->rvals[1]);
      if (!a || !b) return;
      llvm::Value *r = nullptr;
      cchar *out = inst->lvals[0]->name ? inst->lvals[0]->name : "";
      switch (inst->sub_op) {
        case CG2B_ADD:
          r = Builder->CreateAdd(a, b, out);
          break;
        case CG2B_LT:
          // v0 supports signed-integer LT only. Unsigned /
          // float / typed-bool paths land when their tests do.
          r = Builder->CreateICmpSLT(a, b, out);
          break;
        case CG2B_NONE:
          return;
      }
      put_result(ctx, inst->lvals[0], r);
      break;
    }
    case CG2_MOVE: {
      if (inst->rvals.n < 1 || inst->lvals.n < 1) return;
      llvm::Value *src = resolve_value(ctx, inst->rvals[0]);
      if (!src) return;
      put_result(ctx, inst->lvals[0], src);
      break;
    }
    case CG2_NOP:
    default:
      // Land per-test.
      break;
  }
}

void emit_block_skeleton(CGv2Block *b, EmitFunCtx &ctx) {
  cchar *name = b->name ? b->name : "blk";
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(*TheContext, name, ctx.llvm_fun);
  ctx.blk_map.put(b, bb);
}

void emit_terminator(CGv2Inst *term, CGv2Block *blk, EmitFunCtx &ctx) {
  if (!term) {
    Builder->CreateUnreachable();
    return;
  }
  switch (term->op) {
    case CG2_RET: {
      llvm::Type *ret_ty = ctx.llvm_fun->getReturnType();
      if (ret_ty->isVoidTy()) {
        Builder->CreateRetVoid();
      } else {
        llvm::Value *rv = nullptr;
        if (term->rvals.n > 0) rv = resolve_value(ctx, term->rvals[0]);
        if (!rv) rv = llvm::UndefValue::get(ret_ty);
        Builder->CreateRet(rv);
      }
      (void)blk;
      break;
    }
    case CG2_BR: {
      llvm::BasicBlock *target = nullptr;
      if (term->br_target) target = ctx.blk_map.get(term->br_target);
      if (!target) Builder->CreateUnreachable();
      else Builder->CreateBr(target);
      break;
    }
    case CG2_COND_BR: {
      llvm::BasicBlock *tb = term->br_true ? ctx.blk_map.get(term->br_true) : nullptr;
      llvm::BasicBlock *fb = term->br_false ? ctx.blk_map.get(term->br_false) : nullptr;
      llvm::Value *cond = term->rvals.n > 0 ? resolve_value(ctx, term->rvals[0]) : nullptr;
      if (!tb || !fb || !cond) Builder->CreateUnreachable();
      else Builder->CreateCondBr(cond, tb, fb);
      break;
    }
    case CG2_UNREACHABLE:
    default:
      Builder->CreateUnreachable();
      break;
  }
}

void emit_fun(CGv2Fun *cf) {
  if (cf->is_external) {
    // External: declaration only.
    llvm::FunctionType *ft = to_llvm_fn_type(cf->signature);
    if (!ft) return;
    (void)llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
                                  cf->name ? cf->name : "ext",
                                  TheModule.get());
    return;
  }

  // Internal: full declaration + body.
  llvm::FunctionType *ft = to_llvm_fn_type(cf->signature);
  if (!ft) return;

  EmitFunCtx ctx;
  ctx.cf = cf;
  ctx.llvm_fun = llvm::Function::Create(
      ft, llvm::Function::InternalLinkage,
      cf->name ? cf->name : "fn", TheModule.get());

  // Bind formals into the per-fun value map. The CGv2Fun's
  // formals vector is in signature order (parser enforces this
  // when :formals (...) is present, else falls back to value
  // decl order).
  {
    int i = 0;
    for (llvm::Argument &a : ctx.llvm_fun->args()) {
      if (i < cf->formals.n) {
        CGv2Value *v = cf->formals[i];
        if (v && v->name) a.setName(v->name);
        ctx.value_map.put(v, &a);
      }
      i++;
    }
  }

  // Pre-allocate basic blocks so cross-block branches resolve.
  for (CGv2Block *b : cf->blocks) emit_block_skeleton(b, ctx);

  // Alloca pre-pass: every distinct phi-target local gets a
  // slot in the entry block. Stores happen on the pred edge,
  // loads happen at each use site (resolve_value).
  if (cf->entry) {
    llvm::BasicBlock *entry_bb = ctx.blk_map.get(cf->entry);
    if (entry_bb) {
      Builder->SetInsertPoint(entry_bb);
      for (CGv2Block *b : cf->blocks) {
        for (CGv2PhiGroup *g : b->phi_by_pred) {
          for (CGv2Inst *mv : g->moves) {
            for (CGv2Value *lv : mv->lvals) {
              if (!lv || ctx.alloca_map.get(lv)) continue;
              llvm::Type *t = to_llvm_type(lv->type);
              if (!t) continue;
              llvm::AllocaInst *a = Builder->CreateAlloca(
                  t, nullptr, lv->name ? lv->name : "");
              ctx.alloca_map.put(lv, a);
            }
          }
        }
      }
    }
  }

  // Emit each block's body insts + edge MOVEs + terminator.
  for (CGv2Block *b : cf->blocks) {
    llvm::BasicBlock *bb = ctx.blk_map.get(b);
    if (!bb) continue;
    Builder->SetInsertPoint(bb);
    for (CGv2Inst *inst : b->body) emit_inst(inst, ctx);

    // Phi-edge MOVEs: for each successor S in the fun, if S
    // declares a phi_by_pred group with pred == b, emit those
    // MOVE stores here (before this block's terminator).
    for (CGv2Block *s : cf->blocks) {
      for (CGv2PhiGroup *g : s->phi_by_pred) {
        if (g->pred != b) continue;
        for (CGv2Inst *mv : g->moves) emit_inst(mv, ctx);
      }
    }

    emit_terminator(b->terminator, b, ctx);
  }
}

}  // namespace

// Public entry point. Walks the CGv2Program, creating LLVM
// functions in the current TheModule. Caller is responsible
// for initializing TheModule (e.g. via llvm_codegen_initialize).
//
// Returns true on success. On failure, the module may be
// partially populated; caller should run verifyModule.
bool cg_v2_emit_llvm_module(CGv2Program *prog) {
  if (!prog || !TheModule) return false;
  for (CGv2Fun *f : prog->funs) {
    if (!f) continue;
    emit_fun(f);
  }
  return true;
}
