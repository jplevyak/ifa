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
    case CG2T_STRUCT: {
      // Look up by name first; if opaque (or absent), set its
      // body. Using named structs keeps llvm IR readable and
      // allows recursive types in later tests.
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

  // Globals: each use loads from the @global slot. Like the
  // alloca case, mem2reg/SROA cleans this up where possible.
  if (v->scope == CG2V_GLOBAL) {
    llvm::GlobalVariable *gv = g_prog_ctx.global_map.get(v);
    if (gv) {
      return Builder->CreateLoad(gv->getValueType(), gv,
                                  v->name ? v->name : "");
    }
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
  if (slot) Builder->CreateStore(r, slot);
  else ctx.value_map.put(dst, r);
}

// Per-prim emit functions. v0 ships write + writeln only
// (int64 args for write). Each subsequent IF1 prim adds one
// branch to dispatch_prim — same per-test cadence as Phase 4.

void emit_prim_write(EmitFunCtx &ctx, CGv2Inst *inst) {
  if (inst->rvals.n < 1) return;
  llvm::Value *arg = resolve_value(ctx, inst->rvals[0]);
  if (!arg) return;
  CGv2Value *argv = inst->rvals[0];
  if (!argv->type) return;

  // Type-dispatch the format string + LLVM cast. Mirrors v1's
  // pyc_llvm_write_cgfn (codegen/llvm_primitives.cc). The
  // dispatch matches what users actually print: ints (all
  // widths/signs), bools, floats, strings.
  cchar *fmt = nullptr;
  cchar *name_hint = "write_unknown";
  switch (argv->type->kind) {
    case CG2T_INT:
      switch (argv->type->bits) {
        case 8: case 16: case 32:
          fmt = "%d";  name_hint = "write_int";
          break;
        case 64:
          fmt = "%lld"; name_hint = "write_i64";
          arg = Builder->CreateSExtOrTrunc(arg,
              llvm::Type::getInt64Ty(*TheContext));
          break;
      }
      break;
    case CG2T_UINT:
      switch (argv->type->bits) {
        case 8: case 16: case 32:
          fmt = "%u"; name_hint = "write_uint";
          break;
        case 64:
          fmt = "%llu"; name_hint = "write_u64";
          arg = Builder->CreateZExtOrTrunc(arg,
              llvm::Type::getInt64Ty(*TheContext));
          break;
      }
      break;
    case CG2T_BOOL:
      // Print booleans as 0/1 like v1 does. (Python users
      // expect True/False but that's a frontend decision —
      // the prim layer just emits what it was asked to.)
      fmt = "%u"; name_hint = "write_bool";
      arg = Builder->CreateZExt(arg,
          llvm::Type::getInt32Ty(*TheContext));
      break;
    case CG2T_FLOAT:
      // %g matches Python's str(float) better than %f
      // (1.2 → "1.2" not "1.200000"). Same trade as v1.
      fmt = "%g"; name_hint = "write_float";
      if (arg->getType()->isFloatTy()) {
        arg = Builder->CreateFPExt(arg,
            llvm::Type::getDoubleTy(*TheContext));
      }
      break;
    case CG2T_PTR:
    case CG2T_REF:
      // Treat as string when the element is a small int (the
      // pyc string convention: char* = ptr to int8 element).
      // Generic ptr printing isn't user-meaningful; skip.
      if (argv->type->element &&
          argv->type->element->kind == CG2T_INT &&
          argv->type->element->bits == 8) {
        fmt = "%s"; name_hint = "write_str";
      }
      break;
    default:
      break;
  }
  if (!fmt) return;       // unsupported — emit nothing
  llvm::Value *fmt_ptr = get_format_str(fmt, name_hint);
  Builder->CreateCall(get_printf(), { fmt_ptr, arg });
}

void emit_prim_writeln(EmitFunCtx &ctx, CGv2Inst *inst) {
  (void)ctx; (void)inst;
  llvm::Value *fmt_ptr = get_format_str("\n", "writeln");
  Builder->CreateCall(get_printf(), { fmt_ptr });
}

// Get-or-create the snprintf(char*, i64, char*, ...) -> i32
// declaration. Used by to_string's numeric-to-string path.
llvm::Function *get_snprintf() {
  llvm::Function *f = TheModule->getFunction("snprintf");
  if (f) return f;
  llvm::Type *i32 = llvm::Type::getInt32Ty(*TheContext);
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *p = llvm::PointerType::getUnqual(*TheContext);
  llvm::FunctionType *ft = llvm::FunctionType::get(i32,
      { p, i64, p }, /*isVarArg=*/true);
  return llvm::Function::Create(ft,
      llvm::Function::ExternalLinkage, "snprintf",
      TheModule.get());
}

// to_string(v) → ptr (GC-managed null-terminated buffer).
// Numeric/bool → GC_malloc(64) + snprintf with type-dispatched
// format. String passthrough (the arg is already a ptr to
// chars; just propagate). Mirrors v1's
// pyc_llvm_to_string_cgfn in codegen/llvm_primitives.cc.
void emit_prim_to_string(EmitFunCtx &ctx, CGv2Inst *inst) {
  if (inst->rvals.n < 1 || inst->lvals.n < 1) return;
  llvm::Value *arg = resolve_value(ctx, inst->rvals[0]);
  if (!arg) return;
  CGv2Value *argv = inst->rvals[0];
  if (!argv->type) return;

  // String passthrough — char* in, char* out.
  if (argv->type->kind == CG2T_PTR && argv->type->element &&
      argv->type->element->kind == CG2T_INT &&
      argv->type->element->bits == 8) {
    put_result(ctx, inst->lvals[0], arg);
    return;
  }

  cchar *fmt = nullptr;
  cchar *name_hint = "tostr_unknown";
  switch (argv->type->kind) {
    case CG2T_INT:
      if (argv->type->bits == 64) {
        fmt = "%lld"; name_hint = "tostr_i64";
        arg = Builder->CreateSExtOrTrunc(arg,
            llvm::Type::getInt64Ty(*TheContext));
      } else {
        fmt = "%d"; name_hint = "tostr_int";
      }
      break;
    case CG2T_UINT:
      if (argv->type->bits == 64) {
        fmt = "%llu"; name_hint = "tostr_u64";
        arg = Builder->CreateZExtOrTrunc(arg,
            llvm::Type::getInt64Ty(*TheContext));
      } else {
        fmt = "%u"; name_hint = "tostr_uint";
      }
      break;
    case CG2T_BOOL:
      fmt = "%u"; name_hint = "tostr_bool";
      arg = Builder->CreateZExt(arg,
          llvm::Type::getInt32Ty(*TheContext));
      break;
    case CG2T_FLOAT:
      fmt = "%g"; name_hint = "tostr_float";
      if (arg->getType()->isFloatTy()) {
        arg = Builder->CreateFPExt(arg,
            llvm::Type::getDoubleTy(*TheContext));
      }
      break;
    default:
      return;
  }
  if (!fmt) return;

  // GC_malloc(64) — buffer for the snprintf result. 64 is the
  // v1 size; covers every numeric type comfortably.
  const uint64_t kBufSize = 64;
  llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
  cchar *out_name = inst->lvals[0]->name ? inst->lvals[0]->name : "s";
  llvm::Value *buf = Builder->CreateCall(get_gc_malloc(),
      { llvm::ConstantInt::get(i64, kBufSize) }, out_name);

  llvm::Value *fmt_ptr = get_format_str(fmt, name_hint);
  Builder->CreateCall(get_snprintf(),
      { buf, llvm::ConstantInt::get(i64, kBufSize), fmt_ptr, arg });

  put_result(ctx, inst->lvals[0], buf);
}

// Dispatch a CG2_PRIM by name to a per-prim emit function.
// Unknown prims are silently no-op'd in v0 — landings extend
// the switch one prim at a time.
void dispatch_prim(EmitFunCtx &ctx, CGv2Inst *inst) {
  if (!inst->prim_name) return;
  cchar *n = inst->prim_name;
  if (strcmp(n, "write") == 0) { emit_prim_write(ctx, inst); return; }
  if (strcmp(n, "writeln") == 0) { emit_prim_writeln(ctx, inst); return; }
  if (strcmp(n, "to_string") == 0) { emit_prim_to_string(ctx, inst); return; }
  // Unknown — leave for a later landing.
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
      CGv2Type *ptr_ty = inst->rvals[0]->type;
      if (!ptr_ty || !ptr_ty->element) return;
      llvm::Type *elem = to_llvm_type(ptr_ty->element);
      if (!elem) return;
      llvm::Value *gep = Builder->CreateGEP(elem, p, idx);
      cchar *out = inst->lvals[0]->name ? inst->lvals[0]->name : "";
      llvm::Value *loaded = Builder->CreateLoad(elem, gep, out);
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
        return k == CG2T_PTR || k == CG2T_REF || k == CG2T_FUN_PTR;
      };
      auto bits_of = [](CGv2Type *t) {
        return t->kind == CG2T_BOOL ? 1 : t->bits;
      };

      llvm::Value *r = nullptr;
      bool src_int = is_int_like(src_ty->kind);
      bool dst_int = is_int_like(dst_ty->kind);
      bool src_flt = src_ty->kind == CG2T_FLOAT;
      bool dst_flt = dst_ty->kind == CG2T_FLOAT;
      bool src_ptr = is_ptr_like(src_ty->kind);
      bool dst_ptr = is_ptr_like(dst_ty->kind);

      if (src_int && dst_int) {
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
      CGv2Type *ptr_ty = inst->rvals[0]->type;
      if (!ptr_ty || !ptr_ty->element) return;
      llvm::Type *elem = to_llvm_type(ptr_ty->element);
      if (!elem || !elem->isSized()) {
        DEBUG_LOG("CG2_SIZEOF_ELEMENT skipped: unsized element\n");
        return;
      }
      uint64_t sz = TheModule->getDataLayout().getTypeAllocSize(elem);
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
        llvm::Function *fn = TheModule->getFunction("_CG_string_len");
        if (!fn) {
          llvm::FunctionType *ft = llvm::FunctionType::get(i64,
              { ptr_ty }, false);
          fn = llvm::Function::Create(ft,
              llvm::Function::ExternalLinkage, "_CG_string_len",
              TheModule.get());
        }
        result = Builder->CreateCall(fn, { obj }, out);
      } else {
        llvm::Function *fn = TheModule->getFunction("_CG_prim_len");
        if (!fn) {
          llvm::FunctionType *ft = llvm::FunctionType::get(i64,
              { ptr_ty, ptr_ty }, false);
          fn = llvm::Function::Create(ft,
              llvm::Function::ExternalLinkage, "_CG_prim_len",
              TheModule.get());
        }
        // No type descriptor available in v2 (v1 reads it from
        // the IF1 args). Pass null; runtime fallback can read
        // obj's own size header.
        llvm::Value *desc = llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptr_ty));
        result = Builder->CreateCall(fn, { desc, obj }, out);
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
      CGv2Type *ptr_ty = inst->rvals[0]->type;
      if (!ptr_ty || !ptr_ty->element) return;
      llvm::Type *elem = to_llvm_type(ptr_ty->element);
      if (!elem) return;
      llvm::Value *gep = Builder->CreateGEP(elem, p, idx);
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
      std::vector<llvm::Value *> args;
      for (int i = 1; i < inst->rvals.n; i++) {
        llvm::Value *a = resolve_value(ctx, inst->rvals[i]);
        if (!a) return;
        args.push_back(a);
      }
      cchar *out = inst->lvals.n > 0 && inst->lvals[0]->name
                       ? inst->lvals[0]->name : "";
      llvm::Value *r = Builder->CreateCall(callee, args,
          callee->getReturnType()->isVoidTy() ? "" : out);
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
      dispatch_prim(ctx, inst);
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
