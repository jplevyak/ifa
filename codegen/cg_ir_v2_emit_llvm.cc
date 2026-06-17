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
    case CG2T_OPAQUE:
    case CG2T_REF:
    case CG2T_FUN_PTR:
    case CG2T_SYMBOL:
    case CG2T_SUM:
      return llvm::PointerType::getUnqual(*TheContext);
    case CG2T_STRUCT:
    case CG2T_VECTOR: {
      // VECTOR's LLVM type is just the prefix struct — the
      // trailing flexible array is handled by INDEX_LOAD/STORE
      // advancing past `sizeof(struct)` before GEPing by element
      // stride.  Same body / named-struct caching as STRUCT.
      cchar *nm = t->name ? t->name : "anon_struct";
      llvm::StructType *st =
          llvm::StructType::getTypeByName(*TheContext, nm);
      if (st && !st->isOpaque()) return st;
      if (!st) st = llvm::StructType::create(*TheContext, nm);
      std::vector<llvm::Type *> fields;
      for (CGv2TypeField *f : t->fields) {
        llvm::Type *ft = to_llvm_type(f->type);
        if (!ft) return nullptr;
        fields.push_back(ft);
      }
      st->setBody(fields, /*isPacked=*/false);
      return st;
    }
  }
  return nullptr;
}

// Get-or-create a variadic printf declaration.
llvm::Function *get_printf() {
  llvm::Function *f = TheModule->getFunction("printf");
  if (f) return f;
  llvm::FunctionType *ft = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*TheContext),
      { llvm::PointerType::getUnqual(*TheContext) },
      /*isVarArg=*/true);
  return llvm::Function::Create(ft,
      llvm::Function::ExternalLinkage, "printf",
      TheModule.get());
}

// Get-or-create a private constant string global for a printf
// format. Names by the format text so two emit sites for the
// same format share one global. Returns a ptr to the first
// char.
llvm::Value *get_format_str(cchar *fmt, cchar *name_hint) {
  std::string gname = std::string(".str.") + name_hint;
  llvm::GlobalVariable *existing = TheModule->getNamedGlobal(gname);
  if (existing) {
    return Builder->CreateInBoundsGEP(
        existing->getValueType(), existing,
        { llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0),
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0) });
  }
  llvm::Constant *c = llvm::ConstantDataArray::getString(*TheContext, fmt);
  llvm::GlobalVariable *gv = new llvm::GlobalVariable(
      *TheModule, c->getType(), /*isConstant=*/true,
      llvm::GlobalValue::PrivateLinkage, c, gname);
  return Builder->CreateInBoundsGEP(
      c->getType(), gv,
      { llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0),
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0) });
}

// Get-or-create the GC_malloc(i64) -> ptr declaration.
llvm::Function *get_gc_malloc() {
  llvm::Function *f = TheModule->getFunction("GC_malloc");
  if (f) return f;
  llvm::FunctionType *ft = llvm::FunctionType::get(
      llvm::PointerType::getUnqual(*TheContext),
      { llvm::Type::getInt64Ty(*TheContext) },
      /*isVarArg=*/false);
  return llvm::Function::Create(ft,
      llvm::Function::ExternalLinkage, "GC_malloc",
      TheModule.get());
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

// Program-scope emit state. Cleared at the start of each
// cg_v2_emit_llvm_module call. Holds maps that genuinely
// need to span functions (globals + their tracked struct
// types) — kept separate from EmitFunCtx so per-fun caches
// stay local and the issue-017 structural fix holds.
struct EmitProgCtx {
  Map<CGv2Value *, llvm::GlobalVariable *> global_map;
  Map<CGv2Value *, CGv2Type *> global_ptr_struct;

  void clear() { global_map.clear(); global_ptr_struct.clear(); }
};

static EmitProgCtx g_prog_ctx;

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

  // ptr values that point to a known struct (currently
  // populated by CG2_ALLOC). FIELD_STORE / FIELD_LOAD look
  // up the struct here to compute GEPs. Future tests with
  // ptr formals/globals will populate this through other
  // routes.
  Map<CGv2Value *, CGv2Type *> ptr_struct;
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
      case CGv2Immediate::I_STR: {
        // Materialize as a private constant global with the pyc
        // string layout: [i64 length][N x i8 chars (with NUL
        // terminator)]. The returned pointer skips the 8-byte
        // length prefix so it can be handed to runtime helpers
        // (`_CG_string_len(s)` reads bytes -8..0; `_CG_strcat`
        // and friends rely on this layout).
        //
        // The imm.str carries the textual literal, with or
        // without surrounding quotes depending on the source:
        // cg_normalize_v2's build_immediate uses src.v_string
        // directly (no quotes). The textual-form parser
        // preserves quotes; we strip them so the runtime sees
        // exactly the program's literal bytes.
        cchar *s = v->imm.str ? v->imm.str : "";
        std::string text = s;
        if (text.size() >= 2 && text.front() == '"' &&
            text.back() == '"') {
          text = text.substr(1, text.size() - 2);
        }
        llvm::Type *i8_ty = llvm::Type::getInt8Ty(*TheContext);
        llvm::Type *i64_ty = llvm::Type::getInt64Ty(*TheContext);
        // getString adds a trailing NUL so the inner array is
        // (text.size() + 1) bytes.
        llvm::Constant *cdata =
            llvm::ConstantDataArray::getString(*TheContext, text);
        // Packed struct so the i8 array starts exactly at byte 8.
        llvm::StructType *sty = llvm::StructType::get(
            *TheContext, { i64_ty, cdata->getType() },
            /*isPacked=*/true);
        llvm::Constant *init = llvm::ConstantStruct::get(
            sty,
            { llvm::ConstantInt::get(i64_ty,
                                      (uint64_t)text.size()),
              cdata });
        llvm::GlobalVariable *gv = new llvm::GlobalVariable(
            *TheModule, sty, /*isConstant=*/true,
            llvm::GlobalValue::PrivateLinkage, init, ".str.lit");
        // GEP to &sty->field1[0] — skip the i64 prefix and land
        // at the first char.
        llvm::Type *i32_ty = llvm::Type::getInt32Ty(*TheContext);
        return Builder->CreateInBoundsGEP(
            sty, gv,
            { llvm::ConstantInt::get(i32_ty, 0),
              llvm::ConstantInt::get(i32_ty, 1),
              llvm::ConstantInt::get(i32_ty, 0) });
        (void)i8_ty;
      }
      case CGv2Immediate::I_SYM:
        // Land with the symbol type test.
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

  // Globals: each use loads from the @global slot. Like the
  // alloca case, mem2reg/SROA cleans this up where possible.
  if (v->scope == CG2V_GLOBAL) {
    llvm::GlobalVariable *gv = g_prog_ctx.global_map.get(v);
    if (gv) {
      return Builder->CreateLoad(gv->getValueType(), gv,
                                  v->name ? v->name : "");
    }
  }

  // Function references — used as values (e.g. method-slot
  // initialization in a class ___init___).  Resolves to the
  // LLVM Function pointer.  F.4.8 lambda_closure fix.
  if (v->scope == CG2V_FUN_REF && v->target_name) {
    llvm::Function *fn = TheModule->getFunction(v->target_name);
    if (fn) return fn;
  }

  // Function-scoped lookup. Lands with formals/locals tests.
  return ctx.value_map.get(v);
}

// Resolve the struct type a ptr value points to. Looks in
// the per-fun ptr_struct first, then falls back to the
// program-scope global_ptr_struct (populated by cross-fn
// MOVEs to globals).
CGv2Type *lookup_ptr_struct(EmitFunCtx &ctx, CGv2Value *v) {
  CGv2Type *st = ctx.ptr_struct.get(v);
  if (st) return st;
  return g_prog_ctx.global_ptr_struct.get(v);
}

// Store-to-slot if the dst is an alloca-backed local;
// otherwise alias through value_map. Used by both regular
// MOVE/BINOP body insts and phi-edge MOVEs.
void put_result(EmitFunCtx &ctx, CGv2Value *dst, llvm::Value *r) {
  if (!dst || !r) return;
  llvm::AllocaInst *slot = ctx.alloca_map.get(dst);
  if (slot) { Builder->CreateStore(r, slot); return; }
  if (dst->scope == CG2V_GLOBAL) {
    llvm::GlobalVariable *gv = g_prog_ctx.global_map.get(dst);
    if (gv) { Builder->CreateStore(r, gv); return; }
  }
  ctx.value_map.put(dst, r);
}

// D.7: emit_prim_to_string + get_snprintf retired. int.__str__
// and float.__str__ now resolve through libpyc_runtime.a's
// `_CG_str_from_int` / `_CG_str_from_float` via __pyc_c_call__
// (D.4/D.5). No call site of the `to_string` primitive remains
// in pyc-Python source, so the SEND path that fed this emit is
// dead. snprintf is no longer declared on demand here — if it
// resurfaces (e.g. for a future format primitive) it'll be
// declared by CG2_C_CALL the same way as any other libc fn.
//
// Recommendation 4 follow-up: emit_prim_write / writeln /
// dispatch_prim are gone too.  The frontend's `print(...)`
// lowering (python_ifa_build_if1.cc:346) already calls
// `__str__(arg)` before every `write(s)` SEND, so write's arg
// is always a string.  lower_send_prim's default routes both
// `write` and `writeln` through CG2_C_CALL to libpyc_runtime
// externs `_CG_write` / `_CG_writeln`, identical to every
// other library helper.  CG2_PRIM is now unreachable from the
// normalize pass; emit_inst keeps the case so the IR can still
// parse hand-written round-trip tests if needed.

// IndexLayout — distilled "what does GEP through this ptr look
// like?" decision shared by CG2_INDEX_LOAD, CG2_INDEX_STORE and
// CG2_SIZEOF_ELEMENT.  Folds the three workaround branches the
// restart.txt called out:
//
//   1. `@vector("s")` classes (CG2T_VECTOR) — advance the base
//      ptr by `vec_prefix_bytes = sizeof(struct prefix)` before
//      GEPing by element stride.
//   2. opaque ptrs (CG2T_OPAQUE) and "FA collapsed to a 0-field
//      struct" — read the list-header `data` pointer at offset
//      -8 and stride from the lval/rval CGv2Type (val_ty).
//   3. typed struct-as-array lists — stride by the first field's
//      type (pyc lists are laid out as `{e0,e1,...}` per
//      specialization).
//
// All three sites used to re-derive these answers independently;
// keeping them in one place is the recommendation 2 cleanup.
struct IndexLayout {
  bool use_header_indirection;  // load `(ptr_lty *)(p - 8)` first
  uint64_t vec_prefix_bytes;    // 0 unless CG2T_VECTOR
  llvm::Type *elem_lty;         // GEP stride type
};

IndexLayout compute_index_layout(CGv2Type *ptr_ty, CGv2Type *val_ty,
                                  CGv2Type *type_arg_override) {
  IndexLayout L{};
  L.use_header_indirection = false;
  L.vec_prefix_bytes = 0;
  L.elem_lty = nullptr;

  // 1. CG2_INDEX_STORE callers can supply an explicit override
  //    (lower_send_alloc does this when the dst's type is
  //    opaque).  That wins over everything below.
  if (type_arg_override) {
    L.elem_lty = to_llvm_type(type_arg_override);
    return L;
  }

  // 2. @vector struct → step past the prefix, then stride by
  //    the trailing element type.
  if (ptr_ty && ptr_ty->element &&
      ptr_ty->element->kind == CG2T_VECTOR) {
    llvm::Type *sty = to_llvm_type(ptr_ty->element);
    if (sty && sty->isSized()) {
      L.vec_prefix_bytes =
          TheModule->getDataLayout().getTypeAllocSize(sty);
      CGv2Type *ect = ptr_ty->element->element;
      llvm::Type *elt = ect ? to_llvm_type(ect)
                            : llvm::Type::getInt8Ty(*TheContext);
      if (!elt || !elt->isSized())
        elt = llvm::Type::getInt8Ty(*TheContext);
      L.elem_lty = elt;
      return L;
    }
  }

  // 3. No usable static element type → opaque-via-header-ptr
  //    path.  Catches:
  //      - CG2T_OPAQUE (FA-unknown, _CG_any)
  //      - missing ptr_ty entirely
  //      - 0-field struct (FA union of empty/non-empty list
  //        specializations collapsed to a zero-stride shape)
  bool opaque_kind = !ptr_ty || ptr_ty->kind == CG2T_OPAQUE ||
                     !ptr_ty->element ||
                     ptr_ty->element->kind == CG2T_OPAQUE;
  bool empty_struct = ptr_ty && ptr_ty->element &&
                      ptr_ty->element->kind == CG2T_STRUCT &&
                      ptr_ty->element->fields.n == 0;
  if (opaque_kind || empty_struct) {
    L.use_header_indirection = true;
    // Stride from the value's CGv2Type — the lval for LOAD, the
    // rval for STORE — so non-i64 element lists (i1/i8 booleans,
    // etc.) walk by the correct byte count.  Default i64.
    llvm::Type *elt = llvm::Type::getInt64Ty(*TheContext);
    if (val_ty) {
      llvm::Type *t = to_llvm_type(val_ty);
      if (t && t->isSized()) elt = t;
    }
    L.elem_lty = elt;
    return L;
  }

  // 4. Typed struct-as-array list: stride by the first field
  //    type.  Pyc specializes `{e0:i64, e1:i64, ...}` for
  //    `list[int64]` and walks dynamic indices over the field
  //    stride, not the full struct stride.
  CGv2Type *index_ty = ptr_ty->element;
  if (index_ty->kind == CG2T_STRUCT &&
      index_ty->fields.n > 0 && index_ty->fields[0] &&
      index_ty->fields[0]->type) {
    index_ty = index_ty->fields[0]->type;
  }
  L.elem_lty = to_llvm_type(index_ty);
  return L;
}

// Step `base` past a CG2T_VECTOR's prefix to the trailing data
// area.  No-op when `prefix_bytes == 0`.
llvm::Value *apply_vec_prefix(llvm::Value *base, uint64_t prefix_bytes) {
  if (!prefix_bytes) return base;
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  return Builder->CreateGEP(
      llvm::Type::getInt8Ty(*TheContext), base,
      llvm::ConstantInt::get(i64, prefix_bytes), "vec_data");
}

// Read the list-header `data` ptr at offset -8 from `p`.  Pyc's
// runtime list layout: `[ptr@-8][cap@-12][len@-16]<data...>`.
llvm::Value *apply_header_indirection(llvm::Value *p) {
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *ptr_lty = llvm::PointerType::getUnqual(*TheContext);
  llvm::Value *hdr_addr = Builder->CreateGEP(
      llvm::Type::getInt8Ty(*TheContext), p,
      llvm::ConstantInt::getSigned(i64, -8));
  return Builder->CreateLoad(ptr_lty, hdr_addr, "list_data");
}

void emit_inst(CGv2Inst *inst, EmitFunCtx &ctx) {
  switch (inst->op) {
    case CG2_ALLOC: {
      if (inst->lvals.n < 1 || !inst->type_arg) return;
      llvm::Type *sty = to_llvm_type(inst->type_arg);
      if (!sty || !sty->isSized()) {
        // Type isn't sized (opaque struct, void, or struct
        // with one of those as a field). LLVM's
        // getTypeAllocSize traps on unsized types — skip the
        // ALLOC entirely. Phase B.10 investigation: figure out
        // why cg_normalize_v2 produced a struct without a
        // computable size.
        DEBUG_LOG("CG2_ALLOC skipped: unsized type %s\n",
                  inst->type_arg->name ? inst->type_arg->name
                                       : "(anon)");
        return;
      }
      uint64_t size_bytes = TheModule->getDataLayout()
                                    .getTypeAllocSize(sty);
      llvm::Function *gc_malloc = get_gc_malloc();
      llvm::Value *size_v = llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(*TheContext), size_bytes);
      llvm::Value *p = Builder->CreateCall(gc_malloc, { size_v },
          inst->lvals[0]->name ? inst->lvals[0]->name : "p");
      put_result(ctx, inst->lvals[0], p);
      ctx.ptr_struct.put(inst->lvals[0], inst->type_arg);
      break;
    }
    case CG2_FIELD_STORE: {
      if (inst->rvals.n < 2) return;
      llvm::Value *p = resolve_value(ctx, inst->rvals[0]);
      llvm::Value *v = resolve_value(ctx, inst->rvals[1]);
      if (!p || !v) return;
      // Prefer the explicit :struct hint when present (test
      // 13's cross-fn ptr-formal pattern); else look up.
      CGv2Type *st = inst->type_arg ? inst->type_arg
                                     : lookup_ptr_struct(ctx, inst->rvals[0]);
      if (!st) return;
      llvm::Type *sty = to_llvm_type(st);
      if (!sty) return;
      llvm::Value *gep = Builder->CreateStructGEP(sty, p,
          (unsigned)inst->field_idx);
      Builder->CreateStore(v, gep);
      break;
    }
    case CG2_INDEX_LOAD: {
      if (inst->rvals.n < 2 || inst->lvals.n < 1) return;
      llvm::Value *p = resolve_value(ctx, inst->rvals[0]);
      llvm::Value *idx = resolve_value(ctx, inst->rvals[1]);
      if (!p || !idx) return;
      IndexLayout L = compute_index_layout(inst->rvals[0]->type,
                                            inst->lvals[0]->type,
                                            nullptr);
      if (!L.elem_lty) return;
      llvm::Value *base = L.use_header_indirection
                              ? apply_header_indirection(p)
                              : apply_vec_prefix(p, L.vec_prefix_bytes);
      llvm::Value *gep = Builder->CreateGEP(L.elem_lty, base, idx);
      cchar *out = inst->lvals[0]->name ? inst->lvals[0]->name : "";
      llvm::Value *loaded = Builder->CreateLoad(L.elem_lty, gep, out);
      put_result(ctx, inst->lvals[0], loaded);
      break;
    }
    case CG2_CAST: {
      // Auto-dispatched typecast. The frontend produces a
      // (src_value, dst_type) pair; the emitter picks the right
      // LLVM cast opcode (sext/zext/trunc/sitofp/fptosi/fpext/
      // fptrunc/ptrtoint/inttoptr/bitcast) from the (src_kind,
      // dst_kind, bits) tuple.
      if (inst->rvals.n < 1 || inst->lvals.n < 1) return;
      llvm::Value *src = resolve_value(ctx, inst->rvals[0]);
      if (!src) return;
      CGv2Type *src_ty = inst->rvals[0]->type;
      CGv2Type *dst_ty = inst->type_arg ? inst->type_arg
                                         : inst->lvals[0]->type;
      if (!src_ty || !dst_ty) return;
      llvm::Type *dst_llvm = to_llvm_type(dst_ty);
      if (!dst_llvm) return;
      cchar *out = inst->lvals[0]->name ? inst->lvals[0]->name : "";

      // Same kind+bits → no-op (just alias).
      if (src->getType() == dst_llvm) {
        put_result(ctx, inst->lvals[0], src);
        break;
      }

      auto is_int_like = [](CGv2TypeKind k) {
        return k == CG2T_INT || k == CG2T_UINT || k == CG2T_BOOL;
      };
      auto is_ptr_like = [](CGv2TypeKind k) {
        return k == CG2T_PTR || k == CG2T_OPAQUE ||
               k == CG2T_REF || k == CG2T_FUN_PTR;
      };
      auto bits_of = [](CGv2Type *t) {
        return t->kind == CG2T_BOOL ? 1 : t->bits;
      };

      llvm::Value *r = nullptr;
      // Classify by LLVM type, not CGv2 type kind.  Upstream
      // ops (notably CG2_INDEX_LOAD's element-type picker) can
      // produce an LLVM `ptr` when the CGv2Type says
      // `uint8`/`int8` — that happens in bytearray clones where
      // the FA can't narrow the element type.  Dispatching by
      // CGv2 kind alone would then call `CreateZExt(ptr, i64)`,
      // invalid IR.  LLVM-type dispatch makes the cast robust:
      // a ptr-valued source gets `ptrtoint` to any integer dst,
      // matching v1's `_CG_prim_coerce` macro semantics
      // (it's a plain C cast: `((dst_t)src)`).
      llvm::Type *src_llvm = src->getType();
      bool src_int = src_llvm->isIntegerTy();
      bool dst_int = is_int_like(dst_ty->kind) &&
                      dst_llvm->isIntegerTy();
      bool src_flt = src_llvm->isFloatingPointTy();
      bool dst_flt = dst_ty->kind == CG2T_FLOAT &&
                      dst_llvm->isFloatingPointTy();
      bool src_ptr = src_llvm->isPointerTy();
      bool dst_ptr = is_ptr_like(dst_ty->kind) &&
                      dst_llvm->isPointerTy();
      if (getenv("PYC_DEBUG_COERCE")) {
        fprintf(stderr, "  src_int=%d src_flt=%d src_ptr=%d "
                "dst_int=%d dst_flt=%d dst_ptr=%d "
                "dst_kind=%d\n",
                src_int, src_flt, src_ptr,
                dst_int, dst_flt, dst_ptr,
                (int)dst_ty->kind);
        fflush(stderr);
      }

      if (src_int && dst_ty->kind == CG2T_BOOL &&
          src_ty->kind != CG2T_BOOL) {
        // int → bool: Python semantics is `x != 0`, not trunc
        // (which would keep only the LSB and read bool(10) as
        // False).  LLVM emits `icmp ne x, 0`.
        llvm::Value *zero = llvm::ConstantInt::get(
            src->getType(), 0);
        r = Builder->CreateICmpNE(src, zero, out);
      } else if (src_flt && dst_ty->kind == CG2T_BOOL) {
        // float → bool: `x != 0.0`.
        llvm::Value *zero = llvm::ConstantFP::get(
            src->getType(), 0.0);
        r = Builder->CreateFCmpONE(src, zero, out);
      } else if (src_int && dst_int) {
        int sb = bits_of(src_ty);
        int db = bits_of(dst_ty);
        if (sb > db) {
          r = Builder->CreateTrunc(src, dst_llvm, out);
        } else if (sb < db) {
          // Unsigned extension if src is UINT or BOOL; signed
          // otherwise. The "is the source signed" property
          // lives on the src type — LLVM types alone are
          // signedness-agnostic.
          bool unsigned_src = src_ty->kind == CG2T_UINT ||
                              src_ty->kind == CG2T_BOOL;
          r = unsigned_src
                  ? Builder->CreateZExt(src, dst_llvm, out)
                  : Builder->CreateSExt(src, dst_llvm, out);
        } else {
          // Same width — signed/unsigned distinction is per-op,
          // not per-value. Bitwise no-op.
          r = src;
        }
      } else if (src_int && dst_flt) {
        r = (src_ty->kind == CG2T_UINT)
                ? Builder->CreateUIToFP(src, dst_llvm, out)
                : Builder->CreateSIToFP(src, dst_llvm, out);
      } else if (src_flt && dst_int) {
        r = (dst_ty->kind == CG2T_UINT)
                ? Builder->CreateFPToUI(src, dst_llvm, out)
                : Builder->CreateFPToSI(src, dst_llvm, out);
      } else if (src_flt && dst_flt) {
        if (src_ty->bits > dst_ty->bits) {
          r = Builder->CreateFPTrunc(src, dst_llvm, out);
        } else if (src_ty->bits < dst_ty->bits) {
          r = Builder->CreateFPExt(src, dst_llvm, out);
        } else {
          r = src;
        }
      } else if (src_ptr && dst_int) {
        r = Builder->CreatePtrToInt(src, dst_llvm, out);
      } else if (src_int && dst_ptr) {
        r = Builder->CreateIntToPtr(src, dst_llvm, out);
      } else if (src_ptr && dst_ptr) {
        // Opaque ptr → opaque ptr: bitwise no-op.
        r = src;
      }
      if (r) put_result(ctx, inst->lvals[0], r);
      break;
    }
    case CG2_CLONE: {
      // GC_malloc(sizeof(T)) + memcpy(new_p, proto_p, sz).
      // Mirrors v1's P_prim_clone in llvm_primitives.cc:684.
      // The new ptr aliases the same struct type — register
      // it in ctx.ptr_struct so subsequent field ops resolve.
      if (!inst->type_arg || inst->rvals.n < 1 || inst->lvals.n < 1)
        return;
      llvm::Type *sty = to_llvm_type(inst->type_arg);
      if (!sty || !sty->isSized()) {
        DEBUG_LOG("CG2_CLONE skipped: unsized type %s\n",
                  inst->type_arg->name ? inst->type_arg->name
                                       : "(anon)");
        return;
      }
      llvm::Value *proto = resolve_value(ctx, inst->rvals[0]);
      if (!proto) return;
      uint64_t sz = TheModule->getDataLayout().getTypeAllocSize(sty);
      llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
      llvm::Value *size_v = llvm::ConstantInt::get(i64, sz);
      cchar *out = inst->lvals[0]->name ? inst->lvals[0]->name : "clone";
      llvm::Value *new_p = Builder->CreateCall(get_gc_malloc(),
          { size_v }, out);
      Builder->CreateMemCpy(new_p, llvm::MaybeAlign(8),
                            proto, llvm::MaybeAlign(8), size_v);
      put_result(ctx, inst->lvals[0], new_p);
      ctx.ptr_struct.put(inst->lvals[0], inst->type_arg);
      break;
    }
    case CG2_SIZEOF: {
      if (!inst->type_arg || inst->lvals.n < 1) return;
      llvm::Type *t = to_llvm_type(inst->type_arg);
      if (!t || !t->isSized()) {
        DEBUG_LOG("CG2_SIZEOF skipped: unsized type %s\n",
                  inst->type_arg->name ? inst->type_arg->name
                                       : "(anon)");
        return;
      }
      uint64_t sz = TheModule->getDataLayout().getTypeAllocSize(t);
      llvm::Value *c = llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(*TheContext), sz);
      put_result(ctx, inst->lvals[0], c);
      break;
    }
    case CG2_SIZEOF_ELEMENT: {
      if (inst->rvals.n < 1 || inst->lvals.n < 1) return;
      // Element stride matches the INDEX_LOAD/STORE dispatch.
      // For FA-opaque ptrs and 0-field structs the runtime
      // helpers all assume i64 elements; for typed lists we use
      // the first-field type (homogeneous {e0,e1,...} shape).
      IndexLayout L = compute_index_layout(inst->rvals[0]->type,
                                            nullptr, nullptr);
      uint64_t sz = 8;
      if (L.elem_lty && L.elem_lty->isSized()) {
        uint64_t s = TheModule->getDataLayout().getTypeAllocSize(L.elem_lty);
        if (s > 0) sz = s;
      }
      llvm::Value *c = llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(*TheContext), sz);
      put_result(ctx, inst->lvals[0], c);
      break;
    }
    case CG2_LEN: {
      if (inst->rvals.n < 1 || inst->lvals.n < 1) return;
      llvm::Value *obj = resolve_value(ctx, inst->rvals[0]);
      if (!obj) return;
      llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
      llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
      // String-like (char* / ref to int8) → _CG_string_len.
      // Other ptr → _CG_prim_len(null_desc, obj). Mirrors v1's
      // P_prim_len dispatch in llvm_primitives.cc:853.
      CGv2Type *t = inst->rvals[0]->type;
      bool is_str = t && (t->kind == CG2T_PTR || t->kind == CG2T_REF) &&
                    t->element && t->element->kind == CG2T_INT &&
                    t->element->bits == 8;
      llvm::Value *result = nullptr;
      cchar *out = inst->lvals[0]->name ? inst->lvals[0]->name : "";
      // Coerce non-ptr args (immediate ints carrying a small payload)
      // to ptr the same way v1 does.
      if (!obj->getType()->isPointerTy()) {
        obj = obj->getType()->isIntegerTy()
                  ? Builder->CreateIntToPtr(obj, ptr_ty)
                  : llvm::ConstantPointerNull::get(
                        llvm::cast<llvm::PointerType>(ptr_ty));
      }
      if (is_str) {
        // Use libc strlen (always linkable) instead of
        // _CG_string_len (a pyc-runtime helper that the v2
        // LLVM toolchain doesn't link). Returns size_t which
        // is i64 on 64-bit. Phase B.10.11.
        llvm::Function *fn = TheModule->getFunction("strlen");
        if (!fn) {
          llvm::FunctionType *ft = llvm::FunctionType::get(i64,
              { ptr_ty }, false);
          fn = llvm::Function::Create(ft,
              llvm::Function::ExternalLinkage, "strlen",
              TheModule.get());
        }
        result = Builder->CreateCall(fn, { obj }, out);
      } else {
        // Issue 020: read header.len directly from pyc's list
        // layout. The list ptr points past a 16-byte header
        // whose `len` (uint32) sits at offset -12. Equivalent
        // to `_CG_prim_len(0, ptr)` in pyc_c_runtime.h's
        // C-backend macro form, without needing a runtime call.
        llvm::Type *i32 = llvm::Type::getInt32Ty(*TheContext);
        llvm::Value *len_addr = Builder->CreateGEP(
            llvm::Type::getInt8Ty(*TheContext), obj,
            llvm::ConstantInt::getSigned(i64, -12), "len_addr");
        llvm::Value *len32 = Builder->CreateLoad(i32, len_addr, "len32");
        result = Builder->CreateZExt(len32, i64, out);
      }
      put_result(ctx, inst->lvals[0], result);
      break;
    }
    case CG2_INDEX_STORE: {
      if (inst->rvals.n < 3) return;
      llvm::Value *p = resolve_value(ctx, inst->rvals[0]);
      llvm::Value *idx = resolve_value(ctx, inst->rvals[1]);
      llvm::Value *v = resolve_value(ctx, inst->rvals[2]);
      if (!p || !idx || !v) return;
      IndexLayout L = compute_index_layout(inst->rvals[0]->type,
                                            inst->rvals[2]->type,
                                            inst->type_arg);
      if (!L.elem_lty) return;
      llvm::Value *base = L.use_header_indirection
                              ? apply_header_indirection(p)
                              : apply_vec_prefix(p, L.vec_prefix_bytes);
      // Coerce the stored value to the element LLVM type when
      // it arrived wider (e.g. `coerce(uint8, val)` upstream
      // kept i64).  Mirrors CG2_CAST's LLVM-type dispatch.
      if (v->getType() != L.elem_lty &&
          v->getType()->isIntegerTy() &&
          L.elem_lty->isIntegerTy()) {
        v = Builder->CreateTruncOrBitCast(v, L.elem_lty);
      }
      llvm::Value *gep = Builder->CreateGEP(L.elem_lty, base, idx);
      Builder->CreateStore(v, gep);
      break;
    }
    case CG2_FIELD_LOAD: {
      if (inst->rvals.n < 1 || inst->lvals.n < 1) return;
      llvm::Value *p = resolve_value(ctx, inst->rvals[0]);
      if (!p) return;
      CGv2Type *st = inst->type_arg ? inst->type_arg
                                     : lookup_ptr_struct(ctx, inst->rvals[0]);
      if (!st) return;
      llvm::Type *sty = to_llvm_type(st);
      if (!sty) return;
      llvm::Value *gep = Builder->CreateStructGEP(sty, p,
          (unsigned)inst->field_idx);
      // Resolve field type to know what we're loading.
      if ((int)inst->field_idx < 0 ||
          inst->field_idx >= st->fields.n) return;
      llvm::Type *ft = to_llvm_type(st->fields[inst->field_idx]->type);
      if (!ft) return;
      llvm::Value *loaded = Builder->CreateLoad(ft, gep,
          inst->lvals[0]->name ? inst->lvals[0]->name : "");
      put_result(ctx, inst->lvals[0], loaded);
      break;
    }
    case CG2_C_CALL: {
      // Generic FFI primitive. Mirrors v1's c_call_codegen
      // (python_ifa_main.cc:56) at the LLVM emit level.
      //
      // The callee's C function name lives on inst->prim_name.
      // Its return type is inst->type_arg. Each arg is a
      // CGv2Value in inst->rvals; the arg's CGv2Type drives
      // the LLVM param type.
      //
      // The external function is declared on demand. If a
      // call site for the same name with a different
      // signature appears later, LLVM rejects the second
      // declaration (verifyModule catches the mismatch).
      if (!inst->prim_name || !inst->type_arg) return;
      llvm::Type *ret_ty = to_llvm_type(inst->type_arg);
      if (!ret_ty) return;
      std::vector<llvm::Type *> param_tys;
      std::vector<llvm::Value *> args;
      for (CGv2Value *v : inst->rvals) {
        if (!v) continue;
        llvm::Type *pt = to_llvm_type(v->type);
        if (!pt) return;
        llvm::Value *a = resolve_value(ctx, v);
        if (!a) return;
        // Coerce to the declared param type when LLVM's
        // already-cached argument type differs (e.g. an
        // integer-typed local widened during resolve_value).
        if (a->getType() != pt) {
          if (a->getType()->isIntegerTy() && pt->isIntegerTy()) {
            a = Builder->CreateSExtOrTrunc(a, pt);
          } else if (a->getType()->isPointerTy() && pt->isPointerTy()) {
            // Opaque ptrs in LLVM 22 — no-op.
          } else if (a->getType()->isPointerTy() && pt->isIntegerTy()) {
            a = Builder->CreatePtrToInt(a, pt);
          } else if (a->getType()->isIntegerTy() && pt->isPointerTy()) {
            a = Builder->CreateIntToPtr(a, pt);
          }
        }
        param_tys.push_back(pt);
        args.push_back(a);
      }
      llvm::Function *callee =
          TheModule->getFunction(inst->prim_name);
      if (!callee) {
        // Known-varargs C runtime helpers: declare with just
        // the first param's type fixed, the rest as varargs.
        // Without this, distinct call sites with different
        // arg counts trip "Incorrect number of arguments
        // passed to called function" at verifyModule.
        bool is_va = (strcmp(inst->prim_name,
                              "_CG_format_string") == 0);
        std::vector<llvm::Type *> decl_tys = param_tys;
        if (is_va && decl_tys.size() > 1) decl_tys.resize(1);
        llvm::FunctionType *ft = llvm::FunctionType::get(
            ret_ty, decl_tys, /*isVarArg=*/is_va);
        callee = llvm::Function::Create(ft,
            llvm::Function::ExternalLinkage,
            inst->prim_name, TheModule.get());
      }
      cchar *out = inst->lvals.n > 0 && inst->lvals[0]->name
                       ? inst->lvals[0]->name : "";
      llvm::Value *r = Builder->CreateCall(callee, args,
          ret_ty->isVoidTy() ? "" : out);
      if (inst->lvals.n > 0) put_result(ctx, inst->lvals[0], r);
      break;
    }
    case CG2_CALL: {
      if (inst->rvals.n < 1) return;
      // rvals[0] is the callee — a CGv2Value of scope FUN_REF
      // with a target_name. Resolve to llvm::Function by name.
      CGv2Value *callee_v = inst->rvals[0];
      if (!callee_v || callee_v->scope != CG2V_FUN_REF ||
          !callee_v->target_name) return;
      llvm::Function *callee =
          TheModule->getFunction(callee_v->target_name);
      if (!callee) return;
      llvm::FunctionType *ft = callee->getFunctionType();
      std::vector<llvm::Value *> args;
      for (int i = 1; i < inst->rvals.n; i++) {
        llvm::Value *a = resolve_value(ctx, inst->rvals[i]);
        if (a) args.push_back(a);
      }

      // Defensive arg coercion (Phase B.10.2). cg_normalize_v2's
      // lower_send_call forwards pn->rvals[1..] verbatim; the
      // IF1 SEND's rval layout doesn't always 1:1 match the
      // LLVM signature (closure/self/MPosition mapping). Until
      // cg_normalize_v2 grows MPosition-aware arg routing, the
      // emitter pads with undef and truncates extras to keep
      // verifyModule happy. Each coercion is a DEBUG_LOG so
      // the ratchet can see what's being papered over.
      size_t expected = ft->getNumParams();
      if (args.size() < expected) {
        DEBUG_LOG("CG2_CALL %s: padding %zu→%zu args with undef\n",
                  callee_v->target_name, args.size(), expected);
        while (args.size() < expected) {
          args.push_back(llvm::UndefValue::get(
              ft->getParamType((unsigned)args.size())));
        }
      } else if (args.size() > expected && !ft->isVarArg()) {
        DEBUG_LOG("CG2_CALL %s: truncating %zu→%zu args\n",
                  callee_v->target_name, args.size(), expected);
        args.resize(expected);
      }
      // Per-arg type coercion. Where we passed wrong-shaped
      // values, fall back to undef of the expected type so the
      // module verifies. Real type-aware coercion (zext/sext/
      // ptrtoint/inttoptr/bitcast) lands once cg_normalize_v2
      // routes MPositions correctly.
      for (size_t i = 0; i < expected; i++) {
        llvm::Type *want = ft->getParamType((unsigned)i);
        if (args[i]->getType() == want) continue;
        // Try a few cheap structural coercions before bailing
        // to undef.
        llvm::Type *got = args[i]->getType();
        if (got->isIntegerTy() && want->isIntegerTy()) {
          args[i] = Builder->CreateSExtOrTrunc(args[i], want);
        } else if (got->isPointerTy() && want->isPointerTy()) {
          // Opaque ptrs — same LLVM type; if we get here it's
          // a mis-shaped CGv2Value. Leave as-is.
        } else if (got->isPointerTy() && want->isIntegerTy()) {
          args[i] = Builder->CreatePtrToInt(args[i], want);
        } else if (got->isIntegerTy() && want->isPointerTy()) {
          args[i] = Builder->CreateIntToPtr(args[i], want);
        } else {
          DEBUG_LOG("CG2_CALL %s: arg %zu coerce-to-undef\n",
                    callee_v->target_name, i);
          args[i] = llvm::UndefValue::get(want);
        }
      }

      cchar *out = inst->lvals.n > 0 && inst->lvals[0]->name
                       ? inst->lvals[0]->name : "";
      llvm::Value *r = Builder->CreateCall(callee, args,
          ft->getReturnType()->isVoidTy() ? "" : out);
      if (inst->lvals.n > 0) put_result(ctx, inst->lvals[0], r);
      break;
    }
    case CG2_BINOP: {
      if (inst->rvals.n < 2 || inst->lvals.n < 1) return;
      llvm::Value *a = resolve_value(ctx, inst->rvals[0]);
      llvm::Value *b = resolve_value(ctx, inst->rvals[1]);
      if (!a || !b) return;
      llvm::Value *r = nullptr;
      cchar *out = inst->lvals[0]->name ? inst->lvals[0]->name : "";
      // Operand-type dispatch: the same textual sub-op
      // ("add"/"div"/"lt"/etc.) selects an integer-signed,
      // integer-unsigned, or floating-point LLVM op based on
      // the operand's CGv2Type. Mirrors v1's P_prim_operator
      // (which inspects t->type_kind to pick FAdd vs Add etc.)
      // without needing per-typed sub-ops.
      CGv2Type *t = inst->rvals[0]->type;
      bool is_flt = t && t->kind == CG2T_FLOAT;
      bool is_uns = t && (t->kind == CG2T_UINT || t->kind == CG2T_BOOL);
      switch (inst->sub_op) {
        case CG2B_ADD:
          r = is_flt ? Builder->CreateFAdd(a, b, out)
                     : Builder->CreateAdd(a, b, out);
          break;
        case CG2B_SUB:
          r = is_flt ? Builder->CreateFSub(a, b, out)
                     : Builder->CreateSub(a, b, out);
          break;
        case CG2B_MUL:
          r = is_flt ? Builder->CreateFMul(a, b, out)
                     : Builder->CreateMul(a, b, out);
          break;
        case CG2B_DIV:
          r = is_flt ? Builder->CreateFDiv(a, b, out)
            : is_uns ? Builder->CreateUDiv(a, b, out)
                     : Builder->CreateSDiv(a, b, out);
          break;
        case CG2B_MOD:
          r = is_flt ? Builder->CreateFRem(a, b, out)
            : is_uns ? Builder->CreateURem(a, b, out)
                     : Builder->CreateSRem(a, b, out);
          break;
        // Comparisons. FCmp uses ordered predicates (OLT/OEQ/...)
        // — false if either operand is NaN, matching Python's
        // total ordering semantics for `<` and `==`.
        case CG2B_LT:
          r = is_flt ? Builder->CreateFCmpOLT(a, b, out)
            : is_uns ? Builder->CreateICmpULT(a, b, out)
                     : Builder->CreateICmpSLT(a, b, out);
          break;
        case CG2B_LE:
          r = is_flt ? Builder->CreateFCmpOLE(a, b, out)
            : is_uns ? Builder->CreateICmpULE(a, b, out)
                     : Builder->CreateICmpSLE(a, b, out);
          break;
        case CG2B_GT:
          r = is_flt ? Builder->CreateFCmpOGT(a, b, out)
            : is_uns ? Builder->CreateICmpUGT(a, b, out)
                     : Builder->CreateICmpSGT(a, b, out);
          break;
        case CG2B_GE:
          r = is_flt ? Builder->CreateFCmpOGE(a, b, out)
            : is_uns ? Builder->CreateICmpUGE(a, b, out)
                     : Builder->CreateICmpSGE(a, b, out);
          break;
        case CG2B_EQ:
          // ICmpEQ is signedness-agnostic.
          r = is_flt ? Builder->CreateFCmpOEQ(a, b, out)
                     : Builder->CreateICmpEQ(a, b, out);
          break;
        case CG2B_NE:
          r = is_flt ? Builder->CreateFCmpONE(a, b, out)
                     : Builder->CreateICmpNE(a, b, out);
          break;
        // Bitwise / logical — integer-only (LLVM rejects them
        // on float types; the frontend must have typed correctly).
        case CG2B_AND:
          r = Builder->CreateAnd(a, b, out);
          break;
        case CG2B_OR:
          r = Builder->CreateOr(a, b, out);
          break;
        case CG2B_XOR:
          r = Builder->CreateXor(a, b, out);
          break;
        case CG2B_SHL:
          r = Builder->CreateShl(a, b, out);
          break;
        case CG2B_SHR:
          // AShr for signed, LShr for unsigned/bool. AShr
          // matches Python's `>>` semantics on signed; LShr
          // matches C's `>>` semantics on unsigned.
          r = is_uns ? Builder->CreateLShr(a, b, out)
                     : Builder->CreateAShr(a, b, out);
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
      CGv2Value *dst = inst->lvals[0];
      // Cross-fn construction-flow case: MOVE into a global
      // emits `store src, ptr @global` AND propagates the
      // ptr_struct annotation from src into the program-scope
      // map, so field ops in OTHER functions can find the
      // struct type of @global. This is THE issue-017 path.
      if (dst->scope == CG2V_GLOBAL) {
        llvm::GlobalVariable *gv = g_prog_ctx.global_map.get(dst);
        if (gv) Builder->CreateStore(src, gv);
        CGv2Type *st = lookup_ptr_struct(ctx, inst->rvals[0]);
        if (st) g_prog_ctx.global_ptr_struct.put(dst, st);
        break;
      }
      put_result(ctx, dst, src);
      // Propagate per-fun ptr_struct through the alias too.
      CGv2Type *st = ctx.ptr_struct.get(inst->rvals[0]);
      if (st) ctx.ptr_struct.put(dst, st);
      break;
    }
    case CG2_PRIM:
      // Unreachable from cg_normalize_v2 — lower_send_prim
      // routes everything through CG2_C_CALL / typed CG_OPs.
      // The case stays so round-trip parsing of hand-authored
      // IR (cg_ir_v2_parse.cc) doesn't trap; emit is a no-op.
      break;
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

// Declaration-only pass. Creates the llvm::Function and adds
// it to TheModule. Called for every fun before any body is
// emitted, so cross-fun CG_CALL sites resolve regardless of
// declaration order.
llvm::Function *declare_fun(CGv2Fun *cf) {
  llvm::FunctionType *ft = to_llvm_fn_type(cf->signature);
  if (!ft) return nullptr;
  llvm::GlobalValue::LinkageTypes linkage =
      cf->is_external ? llvm::Function::ExternalLinkage
                      : llvm::Function::InternalLinkage;
  return llvm::Function::Create(ft, linkage,
                                cf->name ? cf->name : "fn",
                                TheModule.get());
}

void emit_fun(CGv2Fun *cf) {
  if (cf->is_external) return;       // declared in pre-pass

  llvm::Function *llvm_fun =
      TheModule->getFunction(cf->name ? cf->name : "fn");
  if (!llvm_fun) return;

  EmitFunCtx ctx;
  ctx.cf = cf;
  ctx.llvm_fun = llvm_fun;

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

  // Class-prototype init prelude (Phase B.10.10.3).
  //
  // For main_fun only: walk every ptr-to-struct global and
  // allocate a fresh GC_malloc'd block at the start. Without
  // this, pyc-generated stores like `class A: x = 2` (which
  // compile to `store i64 2, GEP (load @g)`) deref a null @g
  // and segfault.
  //
  // v1 sidesteps this because its analyzer specializes class
  // attribute reads into constants — `print(A.x)` becomes
  // `print(2)` with no @g reference emitted at all. v2's
  // per-PNode walk preserves the field-store path, so the
  // global has to actually be allocated.
  //
  // Only ptr-to-struct globals get the GC_malloc; ptr-to-
  // primitive globals (which v2 also produces) stay null.
  if (cf->is_main && cf->entry) {
    llvm::BasicBlock *entry_bb = ctx.blk_map.get(cf->entry);
    if (entry_bb) {
      Builder->SetInsertPoint(entry_bb);
      Vec<CGv2Value *> gkeys;
      g_prog_ctx.global_map.get_keys(gkeys);
      for (CGv2Value *gv : gkeys) {
        llvm::GlobalVariable *llgv = g_prog_ctx.global_map.get(gv);
        if (!gv || !llgv || !gv->type) continue;
        // Class-proto globals: typed PTR whose element is either
        // a regular STRUCT or a VECTOR (`@vector("s")` like
        // bytearray — its proto is just the prefix struct; the
        // trailing data area is materialized per-clone by
        // _CG_prim_clone_vector_runtime).
        if (gv->type->kind != CG2T_PTR || !gv->type->element ||
            (gv->type->element->kind != CG2T_STRUCT &&
             gv->type->element->kind != CG2T_VECTOR)) continue;
        llvm::Type *sty = to_llvm_type(gv->type->element);
        if (!sty || !sty->isSized()) continue;
        uint64_t sz = TheModule->getDataLayout().getTypeAllocSize(sty);
        llvm::Value *size_v = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(*TheContext), sz);
        llvm::Value *p = Builder->CreateCall(get_gc_malloc(),
            { size_v }, gv->name ? gv->name : "proto");
        Builder->CreateStore(p, llgv);
      }
    }
  }

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
  // Reset program-scope emit state at the start of each call.
  // This is critical for issue-017 hygiene — leftover map
  // entries from a previous emit would cross fn boundaries.
  g_prog_ctx.clear();

  // Pass 1: declare globals as llvm::GlobalVariable.
  for (CGv2Value *gv : prog->globals) {
    if (!gv || gv->scope != CG2V_GLOBAL) continue;
    llvm::Type *t = to_llvm_type(gv->type);
    if (!t) continue;
    llvm::Constant *init = nullptr;
    if (t->isPointerTy()) {
      init = llvm::ConstantPointerNull::get(
          llvm::cast<llvm::PointerType>(t));
    } else {
      init = llvm::Constant::getNullValue(t);
    }
    llvm::GlobalVariable *llgv = new llvm::GlobalVariable(
        *TheModule, t, /*isConstant=*/false,
        llvm::GlobalValue::InternalLinkage, init,
        gv->name ? gv->name : "g");
    g_prog_ctx.global_map.put(gv, llgv);
  }

  // Pass 2: declare all funs up front so CG_CALL sites can
  // resolve callees by name regardless of source ordering.
  for (CGv2Fun *f : prog->funs) {
    if (f) declare_fun(f);
  }
  // Pass 3: emit bodies.
  for (CGv2Fun *f : prog->funs) {
    if (f) emit_fun(f);
  }
  return true;
}
