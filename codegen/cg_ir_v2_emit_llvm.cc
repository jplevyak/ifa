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
};

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
        // Value resolution lands with test 02 (CGV_CONSTANT)
        // and test 03 (CGV_FORMAL). For v0 (test 01 only,
        // void), this branch isn't reached. Emit undef as
        // a safety net.
        Builder->CreateRet(llvm::UndefValue::get(ret_ty));
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

  // Pre-allocate basic blocks so cross-block branches resolve.
  for (CGv2Block *b : cf->blocks) emit_block_skeleton(b, ctx);

  // Emit each block's body insts + terminator.
  for (CGv2Block *b : cf->blocks) {
    llvm::BasicBlock *bb = ctx.blk_map.get(b);
    if (!bb) continue;
    Builder->SetInsertPoint(bb);
    // Body emission lands with test 02 (non-empty body).
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
