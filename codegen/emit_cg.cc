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
#include "code.h"
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

// Used by Track 3 (phi/phy placement) to emit a single MOVE in
// the predecessor block. Reuses the IF1 path's value cache so
// the resulting LLVM IR matches the existing translateFunctionBody
// pattern byte-for-byte.
extern void simple_move(Var *lhs, Var *rhs, Fun *ifa_fun);

// label_to_bb_map: populated by `prepare_basic_blocks` in
// `translateFunctionBody`. emit_cgfun_body (Phase 3.4 production
// path) looks up llvm::BasicBlock by Label so the same BBs from
// the IF1-side setup are reused. Defined in llvm_codegen.cc.
extern std::map<Label *, llvm::BasicBlock *> label_to_bb_map;
extern llvm::BasicBlock *getLLVMBasicBlock(Label *label, llvm::Function *current_llvm_fun);

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
// Argument). Falls back to the IF1-side Var::llvm_value cache
// populated by `allocate_locals` (the production path). Returns
// nullptr if no storage is materialized for the slot.
static llvm::Value *slot_pointer(CGSlot *s) {
  if (!s) return nullptr;
  if (s->llvm_handle) return s->llvm_handle;
  if (s->source_var) return cg_get_llvm_value(s->source_var);
  if (s->source_sym) return cg_get_llvm_value(s->source_sym);
  return nullptr;
}

// Resolve a CGValue to an llvm::Value at the current insert point.
// For CG_V_SLOT, emit a load of the slot's pointee — UNLESS the
// cached value is already a non-pointer (Argument / direct
// constant), in which case it's the value itself. Other kinds
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
      // Formals cached via allocate_locals's Var-keyed map come
      // through as Arguments — already the value, not a pointer.
      if (llvm::isa<llvm::Argument>(ptr)) return ptr;
      // Constants (e.g. global function pointers) return directly.
      if (!ptr->getType()->isPointerTy()) return ptr;
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
//
// Production path (Phase 3.4): allocate_locals already created
// allocas keyed on Var via cg_set_llvm_value. Skip CGSlots whose
// source_var has a cached value — re-allocating duplicates and
// causes lookups to diverge.
static void materialize_local_slots(EmitCtx &ctx) {
  llvm::BasicBlock *entry_bb = &ctx.llvm_fun->getEntryBlock();
  llvm::IRBuilder<> entry_builder(entry_bb, entry_bb->getFirstInsertionPt());
  for (CGSlot *s : ctx.cf->locals) {
    if (!s || s->llvm_handle) continue;
    if (s->kind != CG_SLOT_LOCAL) continue;
    // Skip if allocate_locals already gave this slot's source Var
    // backing storage (production path).
    if (s->source_var && cg_get_llvm_value(s->source_var)) continue;
    if (s->source_sym && cg_get_llvm_value(s->source_sym)) continue;
    llvm::Type *ty = s->type ? cg_to_llvm_type(s->type)
                             : llvm::PointerType::getUnqual(*TheContext);
    if (!ty || ty->isVoidTy()) ty = llvm::PointerType::getUnqual(*TheContext);
    s->llvm_handle = entry_builder.CreateAlloca(ty, nullptr, s->name ? s->name : "local");
  }
}

// Pre-allocate (or reuse) BasicBlocks for every CGBlock so
// cross-block terminator targets can resolve before instruction
// emission. The production path (Phase 3.4) reuses BBs created by
// `prepare_basic_blocks` via `label_to_bb_map`; the standalone
// test path creates fresh BBs.
static void materialize_blocks(EmitCtx &ctx) {
  if (ctx.cf->blocks.n == 0) return;
  for (int i = 0; i < ctx.cf->blocks.n; i++) {
    CGBlock *b = ctx.cf->blocks[i];
    llvm::BasicBlock *bb = nullptr;
    // Reuse the existing LLVM BasicBlock when the source PNode is
    // a LABEL whose `label_to_bb_map` entry was populated by the
    // IF1-side `prepare_basic_blocks`. This is how Phase 3.4's
    // swap stays compatible with allocate_locals / debug-info
    // emission that already ran in the LLVM function's entry block.
    if (b->source_pn && b->source_pn->code &&
        b->source_pn->code->kind == Code_LABEL &&
        b->source_pn->code->label[0]) {
      auto it = label_to_bb_map.find(b->source_pn->code->label[0]);
      if (it != label_to_bb_map.end()) bb = it->second;
    }
    if (!bb) {
      // First CGBlock (entry) maps to the LLVM function's
      // auto-created entry block; the rest get fresh BBs.
      if (i == 0) bb = &ctx.llvm_fun->getEntryBlock();
      else        bb = llvm::BasicBlock::Create(
                          *TheContext, b->label ? b->label : "blk", ctx.llvm_fun);
    }
    ctx.blk_map.put(b, bb);
  }
}

// ---------------------------------------------------------------------------
// Track 3 — phi/phy placement at predecessor blocks
// ---------------------------------------------------------------------------
//
// Phase 2.4's `materialize_phi_phy` / `materialize_phi` in
// `cg_normalize.cc` emit phi/phy MOVEs as CG_STOREs in the source
// PNode's block. That's correct for single-successor flows, but
// the LLVM SSA model wants phi MOVEs placed in the PREDECESSOR
// block (each predecessor stores into the phi'd slot before
// branching, so the successor reads the correct value).
//
// For the parallel emitter, we ignore the in-body phi/phy
// CG_STOREs (they're informational — they show in the
// cg-normalize golden) and instead walk `source_pn->phi` /
// `source_pn->phy` at terminator time, emitting MOVEs in the
// right blocks. This mirrors the IF1 emitter's
// `do_phi_nodes` / `do_phy_nodes` at translate_code_goto /
// translate_code_if call sites.

// Mirrors do_phy_nodes / do_phi_nodes from llvm_codegen.cc. For
// each phy MOVE p in spn->phy, emit `simple_move(p->lvals[isucc],
// p->rvals[0])`. For each phi MOVE pp in cfg_succ[isucc]->phi,
// emit `simple_move(pp->lvals[0], pp->rvals[pred_idx])`.
static void emit_phi_phy(PNode *spn, int isucc, EmitCtx &ctx) {
  if (!spn || !ctx.cf || !ctx.cf->source_fun) return;
  Fun *sf = ctx.cf->source_fun;
  for (PNode *p : spn->phy) {
    if (p->lvals.n > isucc && p->rvals.n > 0) {
      simple_move(p->lvals[isucc], p->rvals[0], sf);
    }
  }
  if (spn->cfg_succ.n > isucc) {
    PNode *succ = spn->cfg_succ[isucc];
    if (succ && succ->phi.n) {
      int pred_idx = succ->cfg_pred_index.get(spn);
      for (PNode *pp : succ->phi) {
        if (pp->rvals.n > pred_idx && pp->lvals.n > 0) {
          simple_move(pp->lvals[0], pp->rvals[pred_idx], sf);
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Per-CG_OP emitter (Phase 3.3)
// ---------------------------------------------------------------------------

static void emit_cg_inst(CGInst *inst, EmitCtx &ctx);

// Terminator emission. The CGBlock passed in supplies succs for
// branches; CG_RET's value comes from the inst's rvals[0] (when
// present). For CG_BR / CG_COND_BR, phi/phy MOVEs (from
// `source_pn->phi` and `source_pn->phy`) are emitted before
// the branch — placed in the current block for CG_BR, in
// per-successor intermediate blocks for CG_COND_BR. See Track 3
// notes above.
static void emit_terminator(CGInst *term, CGBlock *blk, EmitCtx &ctx) {
  if (!term) {
    Builder->CreateUnreachable();
    return;
  }
  PNode *spn = term->source_pn;
  switch (term->op) {
    case CG_BR: {
      llvm::BasicBlock *target = nullptr;
      if (blk->succs.n) target = ctx.blk_map.get(blk->succs[0]);
      if (!target) { Builder->CreateUnreachable(); return; }
      // phi/phy MOVEs go at end of current block, before the
      // unconditional branch.
      if (spn) emit_phi_phy(spn, 0, ctx);
      Builder->CreateBr(target);
      break;
    }
    case CG_COND_BR: {
      llvm::Value *cond = term->rvals.n ? resolve_value(term->rvals[0]) : nullptr;
      llvm::BasicBlock *tbb = blk->succs.n > 0 ? ctx.blk_map.get(blk->succs[0]) : nullptr;
      llvm::BasicBlock *fbb = blk->succs.n > 1 ? ctx.blk_map.get(blk->succs[1]) : nullptr;
      if (!cond || !tbb || !fbb) { Builder->CreateUnreachable(); return; }
      // Constant-folded condition: skip intermediate blocks, branch
      // directly. Matches translate_code_if's optimization.
      if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(cond)) {
        llvm::BasicBlock *target = ci->isOne() ? tbb : fbb;
        int isucc = ci->isOne() ? 0 : 1;
        if (spn) emit_phi_phy(spn, isucc, ctx);
        Builder->CreateBr(target);
        break;
      }
      // Dynamic condition: create per-branch intermediate blocks
      // and place phi/phy MOVEs in each before branching to the
      // real successor. Mirrors translate_code_if's pattern
      // (llvm_codegen.cc:562-578).
      llvm::BasicBlock *if_true_bb =
          llvm::BasicBlock::Create(*TheContext, "if.true", ctx.llvm_fun);
      llvm::BasicBlock *if_false_bb =
          llvm::BasicBlock::Create(*TheContext, "if.false", ctx.llvm_fun);
      Builder->CreateCondBr(cond, if_true_bb, if_false_bb);
      Builder->SetInsertPoint(if_true_bb);
      if (spn) emit_phi_phy(spn, 0, ctx);
      Builder->CreateBr(tbb);
      Builder->SetInsertPoint(if_false_bb);
      if (spn) emit_phi_phy(spn, 1, ctx);
      Builder->CreateBr(fbb);
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
  // Fix 4f — phi/phy double-emission avoidance. Phase 2.4 emits
  // phi/phy MOVEs as in-body CG_STOREs so they show in the
  // cg-normalize golden, but emit_terminator (Track 3) re-emits
  // the same MOVEs at the right per-successor blocks via
  // emit_phi_phy. Skip the in-body STOREs to prevent
  // double-emission. The marker bit is set by
  // materialize_phi/phy in cg_normalize.cc.
  if (inst->is_phi_phy) return;
  // Live gate: mirrors translatePNode's `pn->live && pn->fa_live`
  // check (llvm_codegen.cc:666). Non-live PNodes carry stale
  // refs that DCE expected to elide (e.g. P_prim_period on
  // methods that no longer exist on the post-DCE type), so
  // back-translation to write_llvm_prim/write_send must be
  // gated. Phi/phy materialization stays unconditional and
  // happens in emit_terminator.
  if (inst->source_pn && !(inst->source_pn->live && inst->source_pn->fa_live)) {
    return;
  }
  switch (inst->op) {
    case CG_NOP:
      break;

    case CG_STORE: {
      // Fix 4e — SSA-style cache rename. The IF1 path's
      // simple_move uses setLLVMValue, which is a cache rename
      // (not a real store): subsequent getLLVMValue reads return
      // the cached val without loading. emit_cg_inst's previous
      // raw CreateStore broke this — the store went to memory but
      // write_llvm_prim's later getLLVMValue reads returned stale
      // values. Repro: multi_assignment.py `a=b=c=1; print(a+b+c)`
      // printed 2 instead of 3.
      //
      // When the source PNode is available, delegate to
      // simple_move — exactly matching translate_code_move's
      // semantics. Direct CreateStore stays as the fallback for
      // synthesized CGPrograms (no source_pn).
      PNode *spn = inst->source_pn;
      Fun *sf = ctx.cf ? ctx.cf->source_fun : nullptr;
      if (spn && sf && spn->lvals.n >= 1 && spn->rvals.n >= 1) {
        simple_move(spn->lvals[0], spn->rvals[0], sf);
        break;
      }
      // Fallback: raw store, used for synthesized CGPrograms
      // (no IF1 backing, e.g. unit tests).
      if (!inst->slot || inst->rvals.n == 0) return;
      llvm::Value *ptr = slot_pointer(inst->slot);
      if (!ptr) return;
      if (llvm::isa<llvm::Argument>(ptr)) return;
      if (!ptr->getType()->isPointerTy()) return;
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

static void emit_cgfun_inner(CGFun *cf, CGProgram *prog,
                              llvm::Function *llvm_fun) {
  EmitCtx ctx;
  ctx.prog = prog;
  ctx.cf = cf;
  ctx.llvm_fun = llvm_fun;

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

static void emit_cgfun(CGFun *cf, CGProgram *prog) {
  if (!cf || cf->is_external) return;
  if (!cf->llvm_handle) return;
  emit_cgfun_inner(cf, prog, cf->llvm_handle);
}

// Phase 3.4 production-path entry. Called from
// `translateFunctionBody` after prepare_basic_blocks /
// allocate_locals / emit_parameter_debug_info have set up the
// IF1-side caches. The CGFun's llvm_handle isn't set on this
// path (createFunction populates `cg_get_llvm(source_fun)`
// instead), so we pass the function pointer explicitly.
void emit_cgfun_body(CGFun *cf, llvm::Function *llvm_fun) {
  if (!cf || !llvm_fun) return;
  // Wire CGFun::llvm_handle so cross-CGFun calls and recursive
  // dispatch can resolve back to the same Function.
  if (!cf->llvm_handle) cf->llvm_handle = llvm_fun;
  emit_cgfun_inner(cf, /*prog=*/nullptr, llvm_fun);
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
