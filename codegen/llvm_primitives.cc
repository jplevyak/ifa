#include "llvm_internal.h"
#include "builtin.h"
#include "codegen_common.h"
#include "prim.h"
#include <ctype.h>

// ============================================================================
// Call Target Resolution
// ============================================================================

// LLVM-backend wrapper around get_target_fun_core (codegen_common.{h,cc}).
//
// When `f->calls` doesn't resolve the SEND uniquely, the LLVM backend
// has a riskier fallback that the C backend doesn't: search
// `all_funs_global` first by exact Sym pointer, then by Sym name.
// See AUDIT §1 #6 — the fallback masks a real call-graph-discovery
// gap and is slated for removal in phase 4 once the gap is closed.
Fun *get_target_fun(PNode *n, Fun *f) {
  if (Fun *core = get_target_fun_core(n, f)) return core;

  DEBUG_LOG("get_target_fun: f->calls didn't resolve PNode %p uniquely; trying fallback\n", n);
  if (n->rvals.n == 0) return nullptr;
  Var *called_var = n->rvals[0];
  if (!called_var || !called_var->sym || !all_funs_global) return nullptr;

  // First pass: exact Sym pointer match.
  for (Fun *fx : *all_funs_global) {
    if (fx && fx->sym && fx->sym == called_var->sym) {
      DEBUG_LOG("get_target_fun fallback: matched by sym pointer (%s)\n",
                fx->sym->name ? fx->sym->name : "unnamed");
      return fx;
    }
  }
  // Second pass: Sym-name match (risky if name collisions exist).
  for (Fun *fx : *all_funs_global) {
    if (fx && fx->sym && fx->sym->name && called_var->sym->name &&
        strcmp(fx->sym->name, called_var->sym->name) == 0) {
      DEBUG_LOG("get_target_fun fallback: matched by sym name (%s)\n", fx->sym->name);
      return fx;
    }
  }
  return nullptr;
}

void write_send(Fun *f, PNode *n) {
  if (n->prim) {
    // Primitives should be handled by caller or specific helper
    return;
  }

  Fun *target = get_target_fun(n, f);
  if (!target) {
    DEBUG_LOG("write_send: unable to resolve to a single function at call site in %s\n", f->sym->name);
    return;
  }

  if (!target->sym) {
    DEBUG_LOG("write_send: target function has null sym\n");
    return;
  }

  llvm::Function *callee = target->llvm;
  DEBUG_LOG("write_send target %s (Fun %p), llvm=%p\n", target->sym->name ? target->sym->name : "unnamed",
          (void *)target, (void *)callee);

  if (!callee) {
    // Target wasn't discovered by `discover_all_reachable_functions`
    // — `get_target_fun` reached it via the name-search fallback over
    // `all_funs_global`. This happens when `current->calls.get(n)`
    // returns null for some SEND PNode, which means the analyzer's
    // call-resolution map didn't record this edge but the LLVM-side
    // name lookup still found the function.
    //
    // We create the function and translate its body in-place, saving
    // and restoring the Builder insert point so the calling function's
    // translation isn't disrupted. See AUDIT §1 #6 — this fallback
    // masks a real call-graph-discovery gap, slated for closure in
    // phase 5's unified `Codegen` context object.
    callee = createFunction(target, TheModule.get());
    if (!callee) {
      fail("write_send: failed to create on-demand callee %s (id %d)",
           target->sym->name ? target->sym->name : "(anon)", target->sym->id);
      return;
    }
    if (!target->is_external && target->entry) {
      llvm::BasicBlock *saved_bb = Builder->GetInsertBlock();
      llvm::BasicBlock::iterator saved_ip = Builder->GetInsertPoint();
      // Save the CurrentDebugLocation too — translateFunctionBody
      // updates it to target's subprogram via translatePNode's
      // SetCurrentDebugLocation call. Without this restore, every
      // instruction we emit after returning here (CreateStore for the
      // call result, etc.) inherits target's DISubprogram as its
      // !dbg scope while living in the calling function's BB, which
      // verifyModule rejects with "wrong subprogram for function".
      llvm::DebugLoc saved_dbg = Builder->getCurrentDebugLocation();
      translateFunctionBody(target);
      if (saved_bb) Builder->SetInsertPoint(saved_bb, saved_ip);
      Builder->SetCurrentDebugLocation(saved_dbg);
    }
  }

  std::vector<llvm::Value *> args;
  DEBUG_LOG("write_send building args. callee expects %d args, call has %d rvals\n",
          (int)callee->arg_size(), n->rvals.n);

  // Map call site arguments to formal parameters (parallels cg.cc:612-616)
  unsigned arg_idx = 0;
  Var *v0 = n->rvals[0];
  for (MPosition *p : target->positional_arg_positions) {
    Var *formal_arg = target->args.get(p);

    DEBUG_LOG("  formal %d (MPos[0]=%d): formal_arg=%p, live=%d\n", arg_idx,
            p->pos.n > 0 ? (int)Position2int(p->pos[0]) : -1, (void *)formal_arg, formal_arg ? formal_arg->live : -1);

    if (!formal_arg || !formal_arg->live) {
      DEBUG_LOG("    Skipping non-live formal\n");
      continue;
    }

    // Skip nested positions (tuple fields) - only handle top-level arguments
    // This matches cg.cc:567 check: if (p->pos.n <= 1)
    if (p->pos.n > 1) {
      DEBUG_LOG("    Skipping nested position (pos.n=%d)\n", p->pos.n);
      continue;
    }

    // Get actual argument from call site using MPosition
    // This logic matches write_send_arg in cg.cc:553-580
    int i = Position2int(p->pos[0]) - 1;  // Convert MPosition to rvals index
    DEBUG_LOG("    MPosition calculation: pos[0]=%d -> i=%d\n", (int)Position2int(p->pos[0]), i);

    // Handle closure variables if needed (from cg.cc:556-565)
    if (is_closure_var(v0)) {
      if (i < v0->type->has.n) {
        i = 0;  // Simplified - full closure handling needs more work
      } else {
        i -= v0->type->has.n - 1;
      }
    }

    Var *actual_arg = nullptr;
    if (i >= 0 && i < n->rvals.n) {
      actual_arg = n->rvals[i];
    }

    if (actual_arg) {
      DEBUG_LOG("Arg %d: rval[%d] sym=%s (id=%d)\n", arg_idx, i,
              actual_arg->sym && actual_arg->sym->name ? actual_arg->sym->name : "(null)",
              actual_arg->sym ? actual_arg->sym->id : -1);
      llvm::Value *val = getLLVMValue(actual_arg, f);
      if (val) {
        args.push_back(val);
      } else {
        DEBUG_LOG("Argument %d value is null; substituting undef\n", arg_idx);
        if (arg_idx < callee->arg_size()) {
          args.push_back(llvm::UndefValue::get(callee->getArg(arg_idx)->getType()));
        }
      }
    } else {
      DEBUG_LOG("No actual arg for formal parameter %d; substituting undef\n", arg_idx);
      if (arg_idx < callee->arg_size()) {
        args.push_back(llvm::UndefValue::get(callee->getArg(arg_idx)->getType()));
      }
    }
    arg_idx++;
  }

  DEBUG_LOG("Created %zu args for call to %s\n", args.size(), target->sym->name);

  llvm::CallInst *call = Builder->CreateCall(callee, args);
  // Set debug location
  llvm::DISubprogram *sp = f->llvm->getSubprogram();
  int line_num = n->code ? n->code->line() : 0;
  if (sp) call->setDebugLoc(llvm::DILocation::get(*TheContext, line_num, 0, sp));

  // Result assignment
  if (n->lvals.n == 1 && !callee->getReturnType()->isVoidTy()) {
    Var *res_var = n->lvals[0];
    llvm::Value *dest_ptr = res_var->llvm_value;
    if (dest_ptr && (llvm::isa<llvm::AllocaInst>(dest_ptr) || llvm::isa<llvm::GlobalVariable>(dest_ptr))) {
      Builder->CreateStore(call, dest_ptr);
    } else {
      // Maybe simple SetLLVMValue if it's not memory?
      // But if it's a local var, it should be alloca.
      setLLVMValue(res_var, call, f);
    }
  }
}

int write_llvm_prim(Fun *ifa_fun, PNode *n) {
  if (!n->prim) return 0;
  DEBUG_LOG("write_llvm_prim entry. prim index=%d, name=%s\n", n->prim->index, n->prim->name);
  fflush(stderr);

  // Debug: print all rvals
  DEBUG_LOG("rvals.n=%d\n", n->rvals.n);
  for (int i = 0; i < n->rvals.n; i++) {
    DEBUG_LOG("rvals[%d]: sym=%s (id=%d), is_fun=%d\n", i,
            n->rvals[i]->sym && n->rvals[i]->sym->name ? n->rvals[i]->sym->name : "(null)",
            n->rvals[i]->sym ? n->rvals[i]->sym->id : -1, n->rvals[i]->sym ? n->rvals[i]->sym->is_fun : -1);
  }

  // Determine offset of arguments
  int o =
      (n->rvals.n > 0 && n->rvals[0]->sym && n->rvals[0]->sym->name && strcmp(n->rvals[0]->sym->name, "primitive") == 0)
          ? 2
          : 1;
  DEBUG_LOG("offset o=%d\n", o);
  fflush(stderr);
  // "primitive" symbol logic from cg.cc: (n->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  // We don't have sym_primitive available globally? It's in builtin.h, but check name "primitive".

  llvm::Function *llvm_func = ifa_fun->llvm;

  switch (n->prim->index) {
    case P_prim_operator:
      // sym_operator is the BIN-OP marker (`(send @operator x op y)`).
      // Every binary op is recognized by its *operator-token sym* via
      // the dispatch — we don't handle `P_prim_operator` directly
      // because the actual arithmetic / comparison / etc. lands in the
      // explicit `P_prim_add` ... `P_prim_xor` cases below. Return 0
      // so the dispatcher in translatePNode falls back to the generic
      // call path (which has the closure-dispatch logic).
      return 0;

    case P_prim_add:
    case P_prim_subtract:
    case P_prim_mult:
    case P_prim_div:
    case P_prim_mod:
    case P_prim_less:
    case P_prim_lessorequal:
    case P_prim_greater:
    case P_prim_greaterorequal:
    case P_prim_equal:
    case P_prim_notequal:
    case P_prim_or:
    case P_prim_and:
    case P_prim_xor: {
      if (n->rvals.n < o + 3) {
        fail("Primitive %s has insufficient arguments (has %d, needs %d)", n->prim->name, n->rvals.n, o + 3);
        return 1;
      }
      Var *lhs = n->lvals.n > 0 ? n->lvals[0] : nullptr;
      // rvals layout: [0]=__operator, [1]=first_operand, [2]=operator_symbol, [3]=second_operand
      // So with offset o=1, we use rvals[1] and rvals[3]
      Var *op1 = n->rvals[o];
      Var *op2 = n->rvals[o + 2];  // Skip the operator symbol at rvals[o+1]

      llvm::Value *v1 = getLLVMValue(op1, ifa_fun);
      llvm::Value *v2 = getLLVMValue(op2, ifa_fun);
      if (!v1 || !v2) {
        fail("Primitive %s operands missing LLVM values", n->prim->name);
        return 1;
      }

      // LLVM requires strict type matching for binary ops
      // TODO: Add type promotion/casting if needed

      bool is_float = v1->getType()->isFloatingPointTy();
      llvm::Value *res = nullptr;

      switch (n->prim->index) {
        case P_prim_add:
          res = is_float ? Builder->CreateFAdd(v1, v2) : Builder->CreateAdd(v1, v2);
          break;
        case P_prim_subtract:
          res = is_float ? Builder->CreateFSub(v1, v2) : Builder->CreateSub(v1, v2);
          break;
        case P_prim_mult:
          res = is_float ? Builder->CreateFMul(v1, v2) : Builder->CreateMul(v1, v2);
          break;
        case P_prim_div:
          res = is_float ? Builder->CreateFDiv(v1, v2) : Builder->CreateSDiv(v1, v2);
          break;
        case P_prim_mod:
          res = is_float ? Builder->CreateFRem(v1, v2) : Builder->CreateSRem(v1, v2);
          break;
        case P_prim_equal:
          res = is_float ? Builder->CreateFCmpOEQ(v1, v2) : Builder->CreateICmpEQ(v1, v2);
          break;
        case P_prim_notequal:
          res = is_float ? Builder->CreateFCmpUNE(v1, v2) : Builder->CreateICmpNE(v1, v2);
          break;
        case P_prim_less:
          res = is_float ? Builder->CreateFCmpOLT(v1, v2) : Builder->CreateICmpSLT(v1, v2);
          break;
        case P_prim_lessorequal:
          res = is_float ? Builder->CreateFCmpOLE(v1, v2) : Builder->CreateICmpSLE(v1, v2);
          break;
        case P_prim_greater:
          res = is_float ? Builder->CreateFCmpOGT(v1, v2) : Builder->CreateICmpSGT(v1, v2);
          break;
        case P_prim_greaterorequal:
          res = is_float ? Builder->CreateFCmpOGE(v1, v2) : Builder->CreateICmpSGE(v1, v2);
          break;
        case P_prim_or:
          res = Builder->CreateOr(v1, v2, "or");
          break;
        case P_prim_and:
          res = Builder->CreateAnd(v1, v2, "and");
          break;
        case P_prim_xor:
          res = Builder->CreateXor(v1, v2, "xor");
          break;
        default:
          fail("Unhandled primitive operation: %s", n->prim->name);
          return 1;
      }

      if (res && lhs) {
        // For comparisons, result is i1. If lhs is not i1 (e.g. i8 bool), zext it.
        if (res->getType()->isIntegerTy(1) && lhs->type->size > 1) {  // Assuming size in bytes? No, type->size is bits?
          // In cg.cc, bool is often int8.
          // getLLVMType maps bool to ... ?
          // Let's assume lhs->llvm_type is correct target type.
          llvm::Type *dest_ty = lhs->llvm_type ? lhs->llvm_type : getLLVMType(lhs->type);
          if (dest_ty && dest_ty != res->getType()) {
            res = Builder->CreateZExt(res, dest_ty);
          }
        }
        setLLVMValue(lhs, res, ifa_fun);
      }
      return 1;
    }
    case P_prim_make: {
      // Tuple/list/vector creation: lval = make(type, elem1, elem2, ...).
      // Uses GC_malloc (Boehm GC) so allocations are managed by the
      // same GC the runtime uses. See AUDIT §1 #1 / PRIMITIVES.md §14.
      if (n->lvals.n < 1) return 0;
      Var *res_var = n->lvals[0];
      llvm::Type *res_ty = getLLVMType(res_var->type);

      if (!res_ty) {
        fail("P_prim_make: could not get result type for %s",
             res_var->sym && res_var->sym->name ? res_var->sym->name : "(anon)");
        return 1;
      }

      llvm::Type *struct_ty = nullptr;
      Sym *res_sym_type = res_var->type;

      // Determine the struct type
      if (res_ty->isPointerTy()) {
        // If it's a pointer, try to get the pointee type from the symbol
        if (res_sym_type && res_sym_type->element && res_sym_type->element->type) {
          struct_ty = getLLVMType(res_sym_type->element->type);
        }
      } else if (res_ty->isStructTy()) {
        // If it's directly a struct type (for tuples)
        struct_ty = res_ty;
      }

      if (!struct_ty || !struct_ty->isStructTy()) {
        DEBUG_LOG("P_prim_make: could not resolve struct type for %s; falling back\n",
                  res_var->sym && res_var->sym->name ? res_var->sym->name : "(anon)");
        return 0;
      }

      uint64_t size = TheModule->getDataLayout().getTypeAllocSize(struct_ty);
      llvm::FunctionCallee gcMallocFunc = TheModule->getOrInsertFunction(
          "GC_malloc", llvm::FunctionType::get(llvm::PointerType::getUnqual(*TheContext),
                                                llvm::IntegerType::getInt64Ty(*TheContext), false));
      llvm::Value *struct_ptr = Builder->CreateCall(
          gcMallocFunc, llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(*TheContext), size));

      // Initialize fields. rvals: 0=prim, 1="make", 2=type, 3...=elements.
      for (int i = 3; i < n->rvals.n; i++) {
        int field_idx = i - 3;
        Var *field_val_var = n->rvals[i];
        if (field_val_var->sym && field_val_var->sym->is_fun) continue;  // Skip function-symbol "fields".
        llvm::Value *val = getLLVMValue(field_val_var, ifa_fun);
        if (val) {
          llvm::Value *gep = Builder->CreateStructGEP(struct_ty, struct_ptr, field_idx);
          Builder->CreateStore(val, gep);
        }
      }

      setLLVMValue(res_var, struct_ptr, ifa_fun);
      return 1;
    }
    case P_prim_new: {
      // Fresh instance of a type: lval = new(<Type>). Allocates a
      // zero-initialized struct via GC_malloc. Mirrors cg.cc:320-326
      // which calls _CG_prim_new(<type_name>).
      if (n->lvals.n != 1) {
        fail("P_prim_new: expected exactly one lvalue, got %d", n->lvals.n);
        return 1;
      }
      Var *res_var = n->lvals[0];
      Sym *type_sym = res_var->type;
      if (!type_sym) {
        fail("P_prim_new: result has no type");
        return 1;
      }
      llvm::Type *struct_ty = getLLVMType(type_sym);
      if (!struct_ty || !struct_ty->isStructTy()) {
        DEBUG_LOG("P_prim_new: result type for %s isn't a struct; falling back\n",
                  res_var->sym && res_var->sym->name ? res_var->sym->name : "(anon)");
        return 0;
      }
      uint64_t size = TheModule->getDataLayout().getTypeAllocSize(struct_ty);
      llvm::FunctionCallee gcMallocFunc = TheModule->getOrInsertFunction(
          "GC_malloc", llvm::FunctionType::get(llvm::PointerType::getUnqual(*TheContext),
                                                llvm::IntegerType::getInt64Ty(*TheContext), false));
      llvm::Value *struct_ptr = Builder->CreateCall(
          gcMallocFunc, llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(*TheContext), size));
      setLLVMValue(res_var, struct_ptr, ifa_fun);
      return 1;
    }
    case P_prim_assign: {
      // Reference-assignment: lval = (cast)rval[3]. Mirrors cg.cc:327-332.
      if (n->lvals.n != 1 || n->rvals.n < 4) {
        fail("P_prim_assign: malformed (lvals=%d, rvals=%d)", n->lvals.n, n->rvals.n);
        return 1;
      }
      Var *res_var = n->lvals[0];
      Var *rhs_var = n->rvals[3];
      llvm::Value *rhs_val = getLLVMValue(rhs_var, ifa_fun);
      if (!rhs_val) {
        DEBUG_LOG("P_prim_assign: rhs has no LLVM value for %s\n",
                  rhs_var->sym && rhs_var->sym->name ? rhs_var->sym->name : "(anon)");
        return 1;
      }
      llvm::Type *dst_ty = getLLVMType(res_var->type);
      if (dst_ty && dst_ty != rhs_val->getType()) {
        if (dst_ty->isIntegerTy() && rhs_val->getType()->isIntegerTy()) {
          rhs_val = Builder->CreateZExtOrTrunc(rhs_val, dst_ty, "assign.zext");
        } else if (dst_ty->isPointerTy() && rhs_val->getType()->isPointerTy()) {
          // Opaque pointers — no actual cast needed in LLVM 18.
        } else {
          DEBUG_LOG("P_prim_assign: dst type and rhs type differ but no clean cast; using rhs as-is\n");
        }
      }
      setLLVMValue(res_var, rhs_val, ifa_fun);
      return 1;
    }
    case P_prim_sizeof: {
      // sizeof(T) — emit the IF1 Sym's recorded `size` as a constant.
      // Mirrors cg.cc:361-373. The `o` offset selects which rval has
      // the type argument (2 if prefixed by `__primitive`, else 1).
      Sym *t_sym = n->rvals[o]->type;
      if (!t_sym) return 0;
      llvm::Value *size_val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), t_sym->size);
      if (n->lvals.n == 1) setLLVMValue(n->lvals[0], size_val, ifa_fun);
      return 1;
    }
    case P_prim_sizeof_element: {
      // sizeof(element_type(T)) — handles the record-of-records
      // corner case at cg.cc:374-392.
      Sym *t_sym = n->rvals[o]->type;
      if (!t_sym || !t_sym->element || !t_sym->element->type) return 0;
      uint64_t sz = 0;
      if (!t_sym->element->type->size && t_sym->type_kind == Type_RECORD) {
        sz = t_sym->has.n ? t_sym->has[0]->type->size : 0;
      } else {
        sz = t_sym->element->type->size;
      }
      llvm::Value *size_val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), sz);
      if (n->lvals.n == 1) setLLVMValue(n->lvals[0], size_val, ifa_fun);
      return 1;
    }
    case P_prim_clone:
    case P_prim_clone_vector: {
      // lval = clone(proto[, vec_size]). Mirrors cg.cc:345-360 which
      // calls _CG_prim_clone / _CG_prim_clone_vector runtime helpers.
      // Implementation: GC_malloc(sizeof(T)) + memcpy from the
      // prototype. Matches the runtime semantics — Boehm GC tracks
      // the new pointer; memcpy duplicates the byte image.
      if (n->lvals.n != 1) return 0;
      Var *res_var = n->lvals[0];
      Sym *type_sym = res_var->type;
      if (!type_sym) return 0;
      llvm::Type *struct_ty = getLLVMType(type_sym);
      if (!struct_ty || !struct_ty->isStructTy()) {
        DEBUG_LOG("P_prim_clone: result type for %s isn't a struct; falling back\n",
                  res_var->sym && res_var->sym->name ? res_var->sym->name : "(anon)");
        return 0;
      }
      uint64_t size = TheModule->getDataLayout().getTypeAllocSize(struct_ty);
      llvm::FunctionCallee gcMallocFunc = TheModule->getOrInsertFunction(
          "GC_malloc", llvm::FunctionType::get(llvm::PointerType::getUnqual(*TheContext),
                                                llvm::IntegerType::getInt64Ty(*TheContext), false));
      llvm::Value *new_ptr = Builder->CreateCall(
          gcMallocFunc, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), size));
      // Source pointer = rvals[2] (the prototype). Get the pointer
      // (not the loaded struct) the same way P_prim_period does.
      if (n->rvals.n < 3) return 0;
      Var *proto_var = n->rvals[2];
      llvm::Value *proto_ptr = nullptr;
      if (proto_var->llvm_value &&
          (llvm::isa<llvm::AllocaInst>(proto_var->llvm_value) ||
           llvm::isa<llvm::GlobalVariable>(proto_var->llvm_value))) {
        proto_ptr = proto_var->llvm_value;
      } else {
        proto_ptr = getLLVMValue(proto_var, ifa_fun);
      }
      if (proto_ptr && proto_ptr->getType()->isPointerTy()) {
        Builder->CreateMemCpy(new_ptr, llvm::MaybeAlign(8), proto_ptr, llvm::MaybeAlign(8), size);
      } else {
        DEBUG_LOG("P_prim_clone: proto pointer not available; skipping memcpy\n");
      }
      setLLVMValue(res_var, new_ptr, ifa_fun);
      return 1;
    }
    case P_prim_index_object: {
      // Result = obj[idx]. Mirrors cg.cc:261-292:
      //   - vector type:                obj->v[idx]
      //   - record with constant index:  ((T*)obj)->eN
      //   - other:                       runtime fallback (list lookup)
      if (n->lvals.n != 1 || n->rvals.n < o + 2) return 0;
      Var *res_var = n->lvals[0];
      Sym *obj_type = n->rvals[o]->type;
      if (!obj_type) return 0;
      Var *obj_var = n->rvals[o];
      Var *idx_var = n->rvals[o + 1];

      // Helper: extract a struct pointer from `obj_var` (avoiding the
      // load that getLLVMValue does for AllocaInst-backed locals).
      auto get_struct_ptr = [&](Var *v) -> llvm::Value * {
        if (v->llvm_value &&
            (llvm::isa<llvm::AllocaInst>(v->llvm_value) || llvm::isa<llvm::GlobalVariable>(v->llvm_value))) {
          return v->llvm_value;
        }
        return getLLVMValue(v, ifa_fun);
      };

      llvm::Value *obj_ptr = get_struct_ptr(obj_var);
      if (!obj_ptr || !obj_ptr->getType()->isPointerTy()) return 0;

      llvm::Type *obj_struct_ty = getLLVMType(obj_type);

      if (obj_type->is_vector) {
        // {fields...; element_t v[0];} layout. The `v` field is the
        // last element of the struct.
        llvm::Value *idx_val = getLLVMValue(idx_var, ifa_fun);
        if (!idx_val) return 0;
        int v_field_idx = obj_type->has.n;  // `v` follows the named fields
        llvm::Value *v_ptr = Builder->CreateStructGEP(obj_struct_ty, obj_ptr, v_field_idx, "vec.base");
        llvm::Type *elem_ty = obj_type->element && obj_type->element->type
                                  ? getLLVMType(obj_type->element->type)
                                  : llvm::Type::getInt8Ty(*TheContext);
        llvm::Value *elem_ptr = Builder->CreateInBoundsGEP(elem_ty, v_ptr, idx_val, "vec.elt");
        llvm::Value *loaded = Builder->CreateLoad(elem_ty, elem_ptr, "vec.load");
        setLLVMValue(res_var, loaded, ifa_fun);
        return 1;
      }

      // Record with constant index: idx_var->sym->constant gives the
      // field index as a string. cg.cc emits `e<constant>`.
      if (obj_type->type_kind == Type_RECORD && idx_var->sym && idx_var->sym->constant) {
        int field_idx = atoi(idx_var->sym->constant);
        llvm::Value *gep = Builder->CreateStructGEP(obj_struct_ty, obj_ptr, field_idx, "rec.idx");
        llvm::Type *elem_ty = getLLVMType(res_var->type);
        if (!elem_ty || elem_ty->isVoidTy()) return 0;
        llvm::Value *loaded = Builder->CreateLoad(elem_ty, gep, "rec.load");
        setLLVMValue(res_var, loaded, ifa_fun);
        return 1;
      }

      // List / non-constant index: defer to runtime helper.
      DEBUG_LOG("P_prim_index_object: list-style or non-constant index — falling back\n");
      return 0;
    }
    case P_prim_set_index_object: {
      // obj[idx] = val. Mirrors cg.cc:294-318. Handles vector and
      // record-with-constant-index cases; list-style defers to
      // runtime.
      if (n->rvals.n < o + 3) return 0;
      Var *obj_var = n->rvals[o];
      Var *idx_var = n->rvals[o + 1];
      Var *val_var = n->rvals[n->rvals.n - 1];
      Sym *obj_type = obj_var->type;
      if (!obj_type) return 0;

      auto get_struct_ptr = [&](Var *v) -> llvm::Value * {
        if (v->llvm_value &&
            (llvm::isa<llvm::AllocaInst>(v->llvm_value) || llvm::isa<llvm::GlobalVariable>(v->llvm_value))) {
          return v->llvm_value;
        }
        return getLLVMValue(v, ifa_fun);
      };

      llvm::Value *obj_ptr = get_struct_ptr(obj_var);
      if (!obj_ptr || !obj_ptr->getType()->isPointerTy()) return 0;
      llvm::Value *val_val = getLLVMValue(val_var, ifa_fun);
      if (!val_val) return 0;
      llvm::Type *obj_struct_ty = getLLVMType(obj_type);

      if (obj_type->is_vector) {
        llvm::Value *idx_val = getLLVMValue(idx_var, ifa_fun);
        if (!idx_val) return 0;
        int v_field_idx = obj_type->has.n;
        llvm::Value *v_ptr = Builder->CreateStructGEP(obj_struct_ty, obj_ptr, v_field_idx, "vec.base");
        llvm::Type *elem_ty = obj_type->element && obj_type->element->type
                                  ? getLLVMType(obj_type->element->type)
                                  : llvm::Type::getInt8Ty(*TheContext);
        llvm::Value *elem_ptr = Builder->CreateInBoundsGEP(elem_ty, v_ptr, idx_val, "vec.elt");
        Builder->CreateStore(val_val, elem_ptr);
        if (n->lvals.n) setLLVMValue(n->lvals[0], val_val, ifa_fun);
        return 1;
      }

      if (obj_type->type_kind == Type_RECORD && idx_var->sym && idx_var->sym->constant) {
        int field_idx = atoi(idx_var->sym->constant);
        llvm::Value *gep = Builder->CreateStructGEP(obj_struct_ty, obj_ptr, field_idx, "rec.idx");
        Builder->CreateStore(val_val, gep);
        if (n->lvals.n) setLLVMValue(n->lvals[0], val_val, ifa_fun);
        return 1;
      }

      DEBUG_LOG("P_prim_set_index_object: list-style or non-constant index — falling back\n");
      return 0;
    }
    case P_prim_destruct: {
      // Tuple destructuring: lvals[0..n-1] receive fields of rvals[o..]
      // Mirrors cg.cc:486-488 + `destruct_prim` (cg.cc:172-181):
      //   l->eN = r->eN  for each lvalue
      if (n->lvals.n == 0) return 1;
      for (int i = 0; i < n->lvals.n; i++) {
        Var *lhs = n->lvals[i];
        Var *rhs = n->rvals.v[o + i];
        if (!lhs->live) continue;
        llvm::Value *rhs_val = getLLVMValue(rhs, ifa_fun);
        if (rhs_val) setLLVMValue(lhs, rhs_val, ifa_fun);
      }
      return 1;
    }
    case P_prim_len: {
      // Result = length of obj. Mirrors cg.cc:333-344:
      //   string : _CG_string_len(obj)
      //   other  : _CG_prim_len(t, obj)
      // We declare the runtime helper externs and call them.
      Sym *t = n->rvals[o]->type;
      if (!t) return 0;
      Var *obj_var = n->rvals[o];
      llvm::Value *obj_val = getLLVMValue(obj_var, ifa_fun);
      if (!obj_val) return 0;
      llvm::Type *i64 = llvm::Type::getInt64Ty(*TheContext);
      llvm::Type *ptr = llvm::PointerType::getUnqual(*TheContext);
      llvm::Value *result = nullptr;
      if (sym_string->specializers.set_in(t)) {
        llvm::FunctionCallee fn = TheModule->getOrInsertFunction(
            "_CG_string_len", llvm::FunctionType::get(i64, ptr, false));
        result = Builder->CreateCall(fn, obj_val);
      } else {
        // _CG_prim_len takes (type_descriptor, obj). The C backend
        // passes rvals[o-1]->cg_string which is the type's runtime
        // descriptor; here we use the obj_val type's name as a
        // placeholder argument (the runtime can fall back to obj's
        // recorded size).
        llvm::FunctionCallee fn = TheModule->getOrInsertFunction(
            "_CG_prim_len", llvm::FunctionType::get(i64, {ptr, ptr}, false));
        Var *desc_var = n->rvals[o - 1];
        llvm::Value *desc_val = getLLVMValue(desc_var, ifa_fun);
        if (!desc_val) desc_val = llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*TheContext));
        result = Builder->CreateCall(fn, {desc_val, obj_val});
      }
      if (n->lvals.n && result) setLLVMValue(n->lvals[0], result, ifa_fun);
      return 1;
    }
    case P_prim_period: {
      // Struct member access (Getter): lval = rval[1].field where the
      // field is specified by rval[3] (a symbol Sym).
      if (n->rvals.n < 4) return 0;
      Var *obj_var = n->rvals[1];
      Var *field_sym_var = n->rvals[3];
      Var *res_var = n->lvals.n > 0 ? n->lvals[0] : nullptr;

      if (!res_var) return 1;  // No result needed.

      Sym *obj_type_sym = obj_var->type;
      if (!obj_type_sym) {
        fail("P_prim_period: object has no type for var %s",
             obj_var->sym && obj_var->sym->name ? obj_var->sym->name : "(anon)");
        return 1;
      }

      // Unwrap Type_SUM the same way the C backend does (cg.cc:176).
      // For a `T | nil` receiver, methods live on T (has[0]), not on
      // the sum itself (whose `has` is just the alternatives).
      if (obj_type_sym->type_kind == Type_SUM && obj_type_sym->has.n > 0) {
        obj_type_sym = obj_type_sym->has[0];
      }

      // Closure-creation: when the lvalue is a function type and the
      // analyzer marked this PNode as creating new Syms, the period
      // is method-binding (e.g. `obj.method` → a closure that pairs
      // selector + receiver). Mirror cg.cc:177-185: allocate the
      // closure via GC_malloc, then store rvals[3] (selector) and
      // rvals[1] (receiver) into fields 0 and 1.
      if (res_var->type && res_var->type->type_kind == Type_FUN && n->creates) {
        llvm::Type *closure_ty = getLLVMType(res_var->type);
        if (closure_ty && closure_ty->isStructTy()) {
          uint64_t closure_size = TheModule->getDataLayout().getTypeAllocSize(closure_ty);
          llvm::FunctionCallee gcMallocFunc = TheModule->getOrInsertFunction(
              "GC_malloc", llvm::FunctionType::get(
                               llvm::PointerType::getUnqual(*TheContext),
                               llvm::IntegerType::getInt64Ty(*TheContext), false));
          llvm::Value *closure_ptr = Builder->CreateCall(
              gcMallocFunc,
              llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(*TheContext), closure_size));

          if (llvm::Value *sel_val = getLLVMValue(n->rvals[3], ifa_fun)) {
            llvm::Value *gep0 = Builder->CreateStructGEP(closure_ty, closure_ptr, 0);
            Builder->CreateStore(sel_val, gep0);
          }
          if (llvm::Value *recv_val = getLLVMValue(n->rvals[1], ifa_fun)) {
            llvm::Value *gep1 = Builder->CreateStructGEP(closure_ty, closure_ptr, 1);
            Builder->CreateStore(recv_val, gep1);
          }
          setLLVMValue(res_var, closure_ptr, ifa_fun);
          return 1;
        }
        // closure_ty wasn't a struct — fall through to regular field
        // resolution and let it succeed-or-fail there.
        DEBUG_LOG("P_prim_period: closure-create branch: result type %s isn't a struct, falling through\n",
                  res_var->type->name ? res_var->type->name : "(anon)");
      }

      // Resolve field index by name match (or `eN` for tuples).
      int field_idx = -1;
      cchar *field_name = field_sym_var->sym->name;
      for (int i = 0; i < obj_type_sym->has.n; i++) {
        if (obj_type_sym->has[i]->name == field_name ||
            (obj_type_sym->has[i]->name && field_name && strcmp(obj_type_sym->has[i]->name, field_name) == 0)) {
          field_idx = i;
          break;
        }
      }
      if (field_idx == -1) {
        if (field_name && field_name[0] == 'e' && isdigit(field_name[1])) {
          field_idx = atoi(field_name + 1);
        } else if (!n->live) {
          // DCE marked this getter dead — the field genuinely doesn't
          // exist on this type (typical case: builtin-scalar method
          // binding like `int.__str__` where the IFA leaves a
          // FA-reachable PNode but DCE strips the result). The C
          // backend's parallel skips the per-kind emission for
          // non-live SENDs entirely (cg.cc:586); mirror that here.
          DEBUG_LOG("P_prim_period: skipping non-live unresolved getter (field=%s, type=%s)\n",
                    field_name, obj_type_sym->name);
          return 1;
        } else {
          fail("P_prim_period: could not resolve field %s in type %s",
               field_name, obj_type_sym->name);
          return 1;
        }
      }

      // For GEP we need a pointer to the struct. getLLVMValue loads
      // local AllocaInsts to their stored value (matching the SSA
      // convention) — for structs that gives the struct value, not a
      // pointer. When the underlying storage IS already a pointer
      // (alloca / global), use it directly; otherwise fall back to
      // getLLVMValue and hope it returns a pointer.
      llvm::Value *obj_ptr = nullptr;
      if (obj_var->llvm_value &&
          (llvm::isa<llvm::AllocaInst>(obj_var->llvm_value) ||
           llvm::isa<llvm::GlobalVariable>(obj_var->llvm_value))) {
        obj_ptr = obj_var->llvm_value;
      } else {
        obj_ptr = getLLVMValue(obj_var, ifa_fun);
      }
      if (!obj_ptr) {
        fail("P_prim_period: object value missing for %s",
             obj_var->sym && obj_var->sym->name ? obj_var->sym->name : "(anon)");
        return 1;
      }
      if (!obj_ptr->getType()->isPointerTy()) {
        // Recoverable: spill to a fresh alloca and use that.
        DEBUG_LOG("P_prim_period: obj_val for %s is not a pointer; spilling to alloca\n",
                  obj_var->sym && obj_var->sym->name ? obj_var->sym->name : "(anon)");
        llvm::AllocaInst *tmp = Builder->CreateAlloca(obj_ptr->getType(), nullptr, "period.spill");
        Builder->CreateStore(obj_ptr, tmp);
        obj_ptr = tmp;
      }

      llvm::Value *gep = Builder->CreateStructGEP(getLLVMType(obj_type_sym), obj_ptr, field_idx);
      llvm::Value *loaded = Builder->CreateLoad(getLLVMType(res_var->type), gep);
      setLLVMValue(res_var, loaded, ifa_fun);
      return 1;
    }
    case P_prim_setter: {
      // Struct field write: obj.field = val. Mirrors cg.cc:262-291
      // and the issue-011 Option A semantics — when lvals[0] is live,
      // assign val (rvals[4]) to it (not the receiver).
      // rvals: [__operator, obj, setter, field_sym, val]
      if (n->rvals.n < 5) return 0;
      Var *obj_var = n->rvals[1];
      Var *field_sym_var = n->rvals[3];
      Var *val_var = n->rvals[4];
      Var *res_var = n->lvals.n > 0 ? n->lvals[0] : nullptr;

      Sym *obj_type_sym = obj_var->type;
      if (!obj_type_sym) {
        fail("P_prim_setter: object has no type for var %s",
             obj_var->sym && obj_var->sym->name ? obj_var->sym->name : "(anon)");
        return 1;
      }

      // Unwrap Type_SUM the same way the C backend does (cg.cc:212).
      if (obj_type_sym->type_kind == Type_SUM && obj_type_sym->has.n > 0) {
        obj_type_sym = obj_type_sym->has[0];
      }

      // Resolve field index (same logic as P_prim_period).
      int field_idx = -1;
      cchar *field_name = field_sym_var->sym->name;
      for (int i = 0; i < obj_type_sym->has.n; i++) {
        if (obj_type_sym->has[i]->name == field_name ||
            (obj_type_sym->has[i]->name && field_name && strcmp(obj_type_sym->has[i]->name, field_name) == 0)) {
          field_idx = i;
          break;
        }
      }
      if (field_idx == -1) {
        if (field_name && field_name[0] == 'e' && isdigit(field_name[1])) {
          field_idx = atoi(field_name + 1);
        } else {
          fail("P_prim_setter: could not resolve field %s in type %s",
               field_name, obj_type_sym->name);
          return 1;
        }
      }

      // Get an actual pointer to the object's storage (same shape as
      // P_prim_period's fix above).
      llvm::Value *obj_ptr = nullptr;
      if (obj_var->llvm_value &&
          (llvm::isa<llvm::AllocaInst>(obj_var->llvm_value) ||
           llvm::isa<llvm::GlobalVariable>(obj_var->llvm_value))) {
        obj_ptr = obj_var->llvm_value;
      } else {
        obj_ptr = getLLVMValue(obj_var, ifa_fun);
      }
      if (!obj_ptr || !obj_ptr->getType()->isPointerTy()) {
        DEBUG_LOG("P_prim_setter: obj_ptr for %s is not a pointer; spilling\n",
                  obj_var->sym && obj_var->sym->name ? obj_var->sym->name : "(anon)");
        if (!obj_ptr) return 1;
        llvm::AllocaInst *tmp = Builder->CreateAlloca(obj_ptr->getType(), nullptr, "setter.spill");
        Builder->CreateStore(obj_ptr, tmp);
        obj_ptr = tmp;
      }

      llvm::Value *val_llvm = getLLVMValue(val_var, ifa_fun);
      if (!val_llvm) {
        fail("P_prim_setter: val value missing for %s",
             val_var->sym && val_var->sym->name ? val_var->sym->name : "(anon)");
        return 1;
      }

      // The store itself: ((obj_type *)obj_ptr)->eN = val.
      llvm::Value *gep = Builder->CreateStructGEP(getLLVMType(obj_type_sym), obj_ptr, field_idx);
      Builder->CreateStore(val_llvm, gep);

      // Issue 011 Option A: when the lvalue is live, write val to it.
      if (res_var && res_var->live) {
        setLLVMValue(res_var, val_llvm, ifa_fun);
      }
      return 1;
    }
    case P_prim_primitive: {
      // Handle named primitives like "print", "println"
      // From cg.cc:456-458, name is at rvals[1]
      if (n->rvals.n < 2) return 0;
      Var *name_var = n->rvals[1];
      if (!name_var->sym) return 0;
      cchar *name = name_var->sym->name;
      if (!name) name = name_var->sym->constant;
      if (!name) return 0;

      DEBUG_LOG("P_prim_primitive: name='%s', rvals.n=%d\n", name, n->rvals.n);
      llvm::Module *TheModule = ifa_fun->llvm->getParent();

      DEBUG_LOG("P_prim_primitive: checking if name='%s' is print/println\n", name);

      if (strcmp(name, "print") == 0 || strcmp(name, "println") == 0) {
        DEBUG_LOG("P_prim_primitive: handling print/println\n");
        // Declare printf
        llvm::FunctionCallee printfFunc = TheModule->getOrInsertFunction(
            "printf", llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(*TheContext),
                                              llvm::PointerType::getUnqual(*TheContext), true));

        // Build format string and args
        // From cg.cc:81, arguments start at index 2
        std::string fmt_str = "";
        std::vector<llvm::Value *> args;
        args.push_back(nullptr);  // Placeholder for format string

        bool is_println = (strcmp(name, "println") == 0);
        DEBUG_LOG("P_prim_primitive print: processing %d args starting at index 2\n", n->rvals.n - 2);

        for (int i = 2; i < n->rvals.n; i++) {
          Var *arg = n->rvals[i];
          DEBUG_LOG("  print arg %d: type=%p, sym=%s\n", i - 2, (void *)arg->type,
                  arg->type && arg->type->name ? arg->type->name : "(null)");

          llvm::Value *val = getLLVMValue(arg, ifa_fun);
          if (!val) {
            DEBUG_LOG("  WARNING: getLLVMValue returned NULL for arg %d\n", i - 2);
            continue;
          }

          // Match cg.cc:81-103 type checking logic
          bool doln = (i == n->rvals.n - 1) && is_println;

          // Check type based on Var's type symbol (matching cg.cc)
          if (arg->type == sym_int8 || arg->type == sym_int16 || arg->type == sym_int32) {
            fmt_str += doln ? "%d\n" : "%d";
          } else if (arg->type == sym_bool || arg->type == sym_uint8 || arg->type == sym_uint16 ||
                     arg->type == sym_uint32) {
            fmt_str += doln ? "%u\n" : "%u";
          } else if (arg->type == sym_int64) {
            fmt_str += doln ? "%lld\n" : "%lld";
            // Cast to long long
            val = Builder->CreateSExtOrTrunc(val, llvm::Type::getInt64Ty(*TheContext));
          } else if (arg->type == sym_uint64) {
            fmt_str += doln ? "%llu\n" : "%llu";
            val = Builder->CreateZExtOrTrunc(val, llvm::Type::getInt64Ty(*TheContext));
          } else if (arg->type == sym_float32 || arg->type == sym_float64 || arg->type == sym_float128) {
            fmt_str += doln ? "%f\n" : "%f";
            // Extend to double for printf
            if (val->getType()->isFloatTy()) {
              val = Builder->CreateFPExt(val, llvm::Type::getDoubleTy(*TheContext));
            }
          } else if (arg->type == sym_string) {
            fmt_str += doln ? "%s\n" : "%s";
          } else {
            DEBUG_LOG("  WARNING: unsupported type for arg %d\n", i - 2);
            fmt_str += doln ? "<unsupported type>\n" : "<unsupported type>";
          }
          args.push_back(val);
        }

        // If no args and println, just print a newline (cg.cc:105)
        if (n->rvals.n < 3 && is_println) {
          fmt_str += "\n";
        }

        // Create global string for format
        llvm::Constant *fmt_const = llvm::ConstantDataArray::getString(*TheContext, fmt_str);
        llvm::GlobalVariable *fmt_global = new llvm::GlobalVariable(
            *TheModule, fmt_const->getType(), true, llvm::GlobalValue::PrivateLinkage, fmt_const, ".str.fmt");

        // Get pointer to start of format string
        llvm::Value *fmt_ptr =
            Builder->CreateInBoundsGEP(fmt_const->getType(), fmt_global,
                                       {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0),
                                        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0)});

        args[0] = fmt_ptr;
        llvm::CallInst *ci = Builder->CreateCall(printfFunc, args);
        if (n->lvals.n > 0) setLLVMValue(n->lvals[0], ci, ifa_fun);
        return 1;
      }
      return 0;
    }
    case P_prim_reply: {
      // Check if function returns void first
      if (llvm_func->getReturnType()->isVoidTy()) {
        Builder->CreateRetVoid();
        return 1;
      }

      if (n->rvals.n > 3) {
        Var *ret_val_var = n->rvals[3];

        // Check if return value is void symbol/type
        if (ret_val_var->sym && ret_val_var->sym->name && strcmp(ret_val_var->sym->name, "void") == 0) {
          // Returning void - treat as void return even if function type isn't void
          Builder->CreateRetVoid();
          return 1;
        }

        // Check if return type is void_type
        if (ret_val_var->type == sym_void_type || ret_val_var->type == sym_void) {
          Builder->CreateRetVoid();
          return 1;
        }

        llvm::Value *ret_llvm_val = getLLVMValue(ret_val_var, ifa_fun);
        if (!ret_llvm_val) {
          // Return value variable doesn't have LLVM value (dead code)
          // Return undef like C backend would skip or return placeholder
          ret_llvm_val = llvm::UndefValue::get(llvm_func->getReturnType());
          DEBUG_LOG("Return value not available, using undef for function %s\n", ifa_fun->sym->name);
        }

        std::string ret_type_str, expected_type_str;
        llvm::raw_string_ostream ret_os(ret_type_str), exp_os(expected_type_str);
        ret_llvm_val->getType()->print(ret_os);
        llvm_func->getReturnType()->print(exp_os);
        DEBUG_LOG("prim_reply return value type: %s, expected: %s\n", ret_os.str().c_str(),
                exp_os.str().c_str());

        if (ret_llvm_val->getType() == llvm_func->getReturnType()) {
          Builder->CreateRet(ret_llvm_val);
        } else {
          Builder->CreateRet(ret_llvm_val);  // Might verify fail - let LLVM catch type mismatch
        }
      } else {
        // No return value provided but function is non-void
        fail("Missing return value for non-void function %s", ifa_fun->sym->name);
      }
      return 1;
    }
    default:
      // Fallback for unhandled
      return 0;
  }
}
