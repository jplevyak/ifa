// SPDX-License-Identifier: BSD-3-Clause
//
// emit_cg.cc — CG_IR_PLAN Phase 3.3 + 3.4 scaffold.
//
// Walks a CGProgram (produced by cg_normalize) and emits LLVM IR.
// Parallel to the existing `translateFunctionBody` / `translatePNode`
// path in `llvm_codegen.cc` — the production codegen still goes
// through the IF1-driven emitter; this file's `emit_llvm_module` is
// exercised by unit tests and the follow-up production swap (the
// "real" 3.4 wire-up).
//
// What lands in this file:
//   - `emit_llvm_module(CGProgram*)` — iterate funs, call emit_cgfun.
//   - `emit_cgfun(CGFun*)` — allocate AllocaInsts for local slots,
//     allocate BasicBlocks per CGBlock, emit each block's body +
//     terminator.
//   - `emit_cg_inst(CGInst*, EmitCtx&)` — per-CG_OP dispatch.
//     Handles CG_STORE, CG_NOP, and the four terminators
//     (BR / COND_BR / RET / UNREACHABLE).
//
// What's deferred to the production-swap PR:
//   - CG_LOAD_FIELD / CG_STORE_FIELD / CG_ALLOC structural ops
//     (need pyc-record struct-type resolution — coupled to
//     issue 015's `is_value_type` work).
//   - CG_CALL with prim hint back-translation to source PNode
//     (calls into the existing per-prim emitter in
//     llvm_primitives.cc).
//   - phi/phy materialization at predecessor-block placement.
//   - The print_ir wire-up that flips production from IF1 to
//     CG_IR (CG_IR_PLAN §8.1 step 3.4).
//
// Why the deferred swap is safe: the LLVM-suite ratchet requires
// ≥ 37 passes after every PR (CG_IR_PLAN §8.2). The deferred ops
// touch the prim emitters whose 1274 LOC of existing dispatch
// logic carries subtle invariants — porting them needs a
// dedicated session for the per-prim back-translation seam and
// the validation that keeps the suite ≥ 37.

#include "ifadefs.h"

#include "cg_ir.h"
#include "codegen_common.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "llvm_internal.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"
#include "var.h"

// LLVM globals defined in `llvm.cc`. Same convention as
// `llvm_codegen.cc` (the existing per-PNode emitter).
extern std::unique_ptr<llvm::LLVMContext> TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::unique_ptr<llvm::IRBuilder<>> Builder;

// Per-prim / generic-call emitters from the IF1 path. Used by
// Track 2 (CG_CALL back-translation) to reuse the ~1274 LOC of
// existing per-prim emission without porting it to CGInst.
extern int  write_llvm_prim(Fun *ifa_fun, PNode *n);
extern void write_send(Fun *ifa_fun, PNode *n);

// GC_malloc declaration helper. Same pattern as the per-prim
// emitter (see llvm_primitives.cc:624). Used by CG_ALLOC and
// CG_LOAD_FIELD's closure-create paths.
static llvm::FunctionCallee gc_malloc_fn() {
  return TheModule->getOrInsertFunction(
      "GC_malloc",
      llvm::FunctionType::get(llvm::PointerType::getUnqual(*TheContext),
                              llvm::IntegerType::getInt64Ty(*TheContext), false));
}

// Resolve a CGType to its underlying llvm::StructType (when
// available). pyc heap aggregates are CG_T_PTR at the type level
// with `source` preserving the original IF1 Sym; the StructType
// lives in getLLVMType's cache. Returns nullptr if no struct
// type is available — caller should fall back to back-translation.
static llvm::Type *struct_type_for(CGType *t) {
  if (!t || !t->source) return nullptr;
  llvm::Type *st = getLLVMType(t->source);
  if (!st || !st->isStructTy()) return nullptr;
  return st;
}

// ---------------------------------------------------------------------------
// Emission state
// ---------------------------------------------------------------------------

struct EmitCtx {
  CGProgram *prog;
  CGFun *cf;
  llvm::Function *llvm_fun;
  Map<CGBlock *, llvm::BasicBlock *> blk_map;
};

// Resolve a CGSlot's storage pointer (AllocaInst / GlobalVariable /
// Argument). Returns nullptr if the slot hasn't been materialized.
static llvm::Value *slot_pointer(CGSlot *s) {
  return s ? s->llvm_handle : nullptr;
}

// Resolve a CGValue to an llvm::Value at the current insert point.
// For CG_V_SLOT, emit a load of the slot's pointee. Other kinds
// return nullptr (the caller is responsible for skip-or-fail).
static llvm::Value *resolve_value(CGValue *cv) {
  if (!cv) return nullptr;
  switch (cv->kind) {
    case CG_V_NONE:
    case CG_V_INST:       return nullptr;
    case CG_V_SLOT: {
      llvm::Value *ptr = slot_pointer(cv->slot);
      if (!ptr) return nullptr;
      if (cv->slot && cv->slot->kind == CG_SLOT_FORMAL) return ptr;
      llvm::Type *ty = (cv->slot && cv->slot->type) ? cg_to_llvm_type(cv->slot->type)
                                                    : llvm::PointerType::getUnqual(*TheContext);
      if (!ty || ty->isVoidTy()) ty = llvm::PointerType::getUnqual(*TheContext);
      return Builder->CreateLoad(ty, ptr);
    }
    case CG_V_FUN:        return cv->fun ? cv->fun->llvm_handle : nullptr;
    case CG_V_IMMEDIATE:  return nullptr;  // Phase 4 work
  }
  return nullptr;
}

// Allocate AllocaInst storage for every local CGSlot used in this
// function, in the entry block. Globals (CG_SLOT_GLOBAL) and
// constants (CG_SLOT_CONSTANT) get GlobalVariables elsewhere;
// formals (CG_SLOT_FORMAL) get their Argument value wired by
// create_llvm_function_from_cgfun.
static void materialize_local_slots(EmitCtx &ctx) {
  llvm::BasicBlock *entry_bb = &ctx.llvm_fun->getEntryBlock();
  llvm::IRBuilder<> entry_builder(entry_bb, entry_bb->getFirstInsertionPt());
  for (CGSlot *s : ctx.cf->locals) {
    if (!s || s->llvm_handle) continue;
    if (s->kind != CG_SLOT_LOCAL) continue;
    llvm::Type *ty = s->type ? cg_to_llvm_type(s->type)
                             : llvm::PointerType::getUnqual(*TheContext);
    if (!ty || ty->isVoidTy()) ty = llvm::PointerType::getUnqual(*TheContext);
    s->llvm_handle = entry_builder.CreateAlloca(ty, nullptr, s->name ? s->name : "local");
  }
}

// Pre-allocate BasicBlocks for every CGBlock so cross-block
// terminator targets can resolve before instruction emission.
static void materialize_blocks(EmitCtx &ctx) {
  // The first CGBlock is the entry; the LLVM function already has
  // an entry block from create_llvm_function_from_cgfun. Bind it.
  if (ctx.cf->blocks.n == 0) return;
  CGBlock *entry = ctx.cf->blocks[0];
  ctx.blk_map.put(entry, &ctx.llvm_fun->getEntryBlock());
  for (int i = 1; i < ctx.cf->blocks.n; i++) {
    CGBlock *b = ctx.cf->blocks[i];
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(
        *TheContext, b->label ? b->label : "blk", ctx.llvm_fun);
    ctx.blk_map.put(b, bb);
  }
}

// ---------------------------------------------------------------------------
// Per-CG_OP emitter (Phase 3.3)
// ---------------------------------------------------------------------------

static void emit_cg_inst(CGInst *inst, EmitCtx &ctx);

// Terminator emission. The CGBlock passed in supplies succs for
// branches; CG_RET's value comes from the inst's rvals[0] (when
// present).
static void emit_terminator(CGInst *term, CGBlock *blk, EmitCtx &ctx) {
  if (!term) {
    Builder->CreateUnreachable();
    return;
  }
  switch (term->op) {
    case CG_BR: {
      llvm::BasicBlock *target = nullptr;
      if (blk->succs.n) target = ctx.blk_map.get(blk->succs[0]);
      if (!target) { Builder->CreateUnreachable(); return; }
      Builder->CreateBr(target);
      break;
    }
    case CG_COND_BR: {
      llvm::Value *cond = term->rvals.n ? resolve_value(term->rvals[0]) : nullptr;
      llvm::BasicBlock *tbb = blk->succs.n > 0 ? ctx.blk_map.get(blk->succs[0]) : nullptr;
      llvm::BasicBlock *fbb = blk->succs.n > 1 ? ctx.blk_map.get(blk->succs[1]) : nullptr;
      if (!cond || !tbb || !fbb) { Builder->CreateUnreachable(); return; }
      Builder->CreateCondBr(cond, tbb, fbb);
      break;
    }
    case CG_RET: {
      llvm::Type *ret_ty = ctx.llvm_fun->getReturnType();
      if (ret_ty->isVoidTy()) {
        Builder->CreateRetVoid();
      } else {
        llvm::Value *v = term->rvals.n ? resolve_value(term->rvals[0]) : nullptr;
        if (!v) v = llvm::UndefValue::get(ret_ty);
        Builder->CreateRet(v);
      }
      break;
    }
    case CG_UNREACHABLE:
    default:
      Builder->CreateUnreachable();
      break;
  }
}

static void emit_cg_inst(CGInst *inst, EmitCtx &ctx) {
  if (!inst) return;
  switch (inst->op) {
    case CG_NOP:
      break;

    case CG_STORE: {
      if (!inst->slot || inst->rvals.n == 0) return;
      llvm::Value *ptr = slot_pointer(inst->slot);
      if (!ptr) return;
      llvm::Value *val = resolve_value(inst->rvals[0]);
      if (!val) return;
      Builder->CreateStore(val, ptr);
      break;
    }

    case CG_LOAD:
    case CG_GEP_FIELD:
    case CG_CAST:
      // Unused in the current Phase 2.3 lowering output. Future
      // CG_OP additions land their direct emission here.
      break;

    case CG_ALLOC: {
      // Track 1 — direct emission. GC_malloc(sizeof(struct))
      // into the result slot. Falls back to back-translation
      // when the slot's type doesn't yield a struct type
      // (typically: when CGType::source is unset, e.g. for
      // synthesized CGPrograms without IF1 backing).
      if (!inst->slot) goto fallback;
      llvm::Type *st = struct_type_for(inst->slot->type);
      if (!st) goto fallback;
      uint64_t size = TheModule->getDataLayout().getTypeAllocSize(st);
      llvm::Value *p = Builder->CreateCall(
          gc_malloc_fn(),
          llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(*TheContext), size));
      llvm::Value *slot_ptr = slot_pointer(inst->slot);
      if (slot_ptr) Builder->CreateStore(p, slot_ptr);
      break;
    }

    case CG_LOAD_FIELD: {
      // Track 1 — direct emission. GEP into object's struct,
      // load field, store into result slot.
      if (inst->rvals.n == 0 || !inst->slot) goto fallback;
      if (inst->field_idx < 0) goto fallback;  // dynamic index — fallback
      CGValue *obj_cv = inst->rvals[0];
      if (!obj_cv || obj_cv->kind != CG_V_SLOT || !obj_cv->slot) goto fallback;
      llvm::Type *st = struct_type_for(obj_cv->slot->type);
      if (!st) goto fallback;
      llvm::Value *obj_ptr = resolve_value(obj_cv);
      if (!obj_ptr || !obj_ptr->getType()->isPointerTy()) goto fallback;
      llvm::Value *gep = Builder->CreateStructGEP(st, obj_ptr, inst->field_idx);
      // Field LLVM type from CGType.fields[field_idx], with PTR
      // fallback so an under-specified CGType doesn't fail emit.
      llvm::Type *field_lt = llvm::PointerType::getUnqual(*TheContext);
      if (inst->field_idx < obj_cv->slot->type->fields.n) {
        CGType *ft = obj_cv->slot->type->fields[inst->field_idx];
        if (ft) {
          llvm::Type *t = cg_to_llvm_type(ft);
          if (t && !t->isVoidTy()) field_lt = t;
        }
      }
      llvm::Value *loaded = Builder->CreateLoad(field_lt, gep);
      llvm::Value *slot_ptr = slot_pointer(inst->slot);
      if (slot_ptr) Builder->CreateStore(loaded, slot_ptr);
      break;
    }

    case CG_STORE_FIELD: {
      // Track 1 — direct emission. GEP into object's struct,
      // store value at field_idx.
      if (inst->rvals.n < 2) goto fallback;
      if (inst->field_idx < 0) goto fallback;
      CGValue *obj_cv = inst->rvals[0];
      if (!obj_cv || obj_cv->kind != CG_V_SLOT || !obj_cv->slot) goto fallback;
      llvm::Type *st = struct_type_for(obj_cv->slot->type);
      if (!st) goto fallback;
      llvm::Value *obj_ptr = resolve_value(obj_cv);
      if (!obj_ptr || !obj_ptr->getType()->isPointerTy()) goto fallback;
      llvm::Value *val = resolve_value(inst->rvals[1]);
      if (!val) goto fallback;
      llvm::Value *gep = Builder->CreateStructGEP(st, obj_ptr, inst->field_idx);
      Builder->CreateStore(val, gep);
      break;
    }

    case CG_CALL:
    case CG_PRIM_OP:
    case CG_PRIM_CGFN: {
    fallback:
      // Track 2 — CG_CALL back-translation. When the CGInst
      // carries source_pn and the owning CGFun's source_fun is
      // available, dispatch through the existing per-prim
      // emitter in `llvm_primitives.cc`. The ~1274 LOC of
      // primitive emission logic is preserved verbatim;
      // emit_cg_inst is the seam.
      //
      // Note: this code path is exercised only when the parallel
      // emitter runs standalone (the production path uses
      // translateFunctionBody, which double-emits if we also
      // back-translate). Track 4's print_ir swap deletes the IF1
      // path so the back-translation becomes the sole emitter.
      PNode *spn = inst->source_pn;
      Fun *sf = ctx.cf ? ctx.cf->source_fun : nullptr;
      if (!spn || !sf) break;
      if (inst->prim) {
        if (write_llvm_prim(sf, spn)) break;
      }
      write_send(sf, spn);
      break;
    }

    // Terminators handled by emit_terminator at block close.
    case CG_BR:
    case CG_COND_BR:
    case CG_RET:
    case CG_UNREACHABLE:
      break;
  }
}

// ---------------------------------------------------------------------------
// emit_cgfun — emit one CGFun's body
// ---------------------------------------------------------------------------

static void emit_cgfun(CGFun *cf, CGProgram *prog) {
  if (!cf || cf->is_external) return;
  if (!cf->llvm_handle) return;

  EmitCtx ctx;
  ctx.prog = prog;
  ctx.cf = cf;
  ctx.llvm_fun = cf->llvm_handle;

  materialize_blocks(ctx);
  materialize_local_slots(ctx);

  // Emit each block: set insert point, body insts, terminator.
  for (CGBlock *b : cf->blocks) {
    llvm::BasicBlock *bb = ctx.blk_map.get(b);
    if (!bb) continue;
    Builder->SetInsertPoint(bb);
    for (CGInst *inst : b->body) emit_cg_inst(inst, ctx);
    emit_terminator(b->terminator, b, ctx);
  }
}

// ---------------------------------------------------------------------------
// emit_llvm_module — Phase 3.3 entry point
//
// Materializes function declarations (via Phase 3.2's
// create_llvm_function_from_cgfun) and emits each non-external
// function's body. Globals (CGProgram::globals) are left to the
// production path's createGlobalVariables — Phase 4's POD-records
// work (issue 015) lands the CG_IR-side global materialization
// once heap-aggregate slot typing is settled.
// ---------------------------------------------------------------------------

void emit_llvm_module(CGProgram *prog) {
  if (!prog || !TheModule) return;
  // Pass 1: function declarations.
  for (CGFun *cf : prog->funs) {
    if (!cf) continue;
    create_llvm_function_from_cgfun(cf, TheModule.get());
  }
  // Pass 2: function bodies (non-external only).
  for (CGFun *cf : prog->funs) emit_cgfun(cf, prog);
}
