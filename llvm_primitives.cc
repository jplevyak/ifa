/* -*-Mode: c++;-*-
   Copyright (c) 2023 John Plevyak, All Rights Reserved
*/
#include "llvm_internal.h"
#include "builtin.h"
#include "prim.h"
#include <ctype.h>

// ============================================================================
// Call Target Resolution
// ============================================================================

Fun *get_target_fun(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || fns->n != 1) {
    if (1) { // TODO: Check runtime error flags
        // For debugging, print what we found
        fprintf(stderr, "DEBUG: get_target_fun failed for PNode %p. fns=%p, fns->n=%d\n", n, fns, fns ? fns->n : 0);
    }
    
    // Fallback: Check n->rvals[0]
    if (n->rvals.n > 0) {
        Var *called_var = n->rvals[0];
        if (called_var && called_var->sym) {
            fprintf(stderr, "DEBUG: Attempting fallback for symbol %s (id %d)\n",
                    called_var->sym->name ? called_var->sym->name : "unnamed",
                    called_var->sym->id);
            if (all_funs_global) {
                fprintf(stderr, "DEBUG: Searching %d functions in all_funs_global\n", all_funs_global->n);
                forv_Fun(fx, *all_funs_global) {
                    if (!fx) {
                        fprintf(stderr, "DEBUG: Skipping null fun in all_funs_global\n");
                        continue;
                    }
                    if (fx->sym && fx->sym == called_var->sym) {
                        fprintf(stderr, "DEBUG: Found fallback target fun: %s\n",
                                fx->sym->name ? fx->sym->name : "unnamed");
                        return fx;
                    }
                }
                // Name match fallback (risky but trying it if sym IDs don't align for some reason)
                forv_Fun(fx, *all_funs_global) {
                    if (!fx) continue;
                    if (fx->sym && fx->sym->name && called_var->sym->name &&
                        strcmp(fx->sym->name, called_var->sym->name) == 0) {
                        fprintf(stderr, "DEBUG: Found fallback target fun by NAME: %s\n", fx->sym->name);
                        return fx;
                    }
                }
                fprintf(stderr, "DEBUG: Fallback search complete, no match found\n");
            } else {
                fprintf(stderr, "DEBUG: all_funs_global is NULL\n");
            }
        }
    }

    // fail("unable to resolve to a single function at call site"); // Non-fatal allowed?
    return 0;
  }
  return fns->v[0];
}

void write_send(Fun *f, PNode *n) {
    if (n->prim) {
       // Primitives should be handled by caller or specific helper
       return;
    }
    
    Fun *target = get_target_fun(n, f);
    if (!target) {
        fprintf(stderr, "FAIL (ignored): unable to resolve to a single function at call site %s\n", f->sym->name);
        return;
    }

    if (!target->sym) {
        fprintf(stderr, "FAIL: target function has null sym\n");
        return;
    }

    llvm::Function *callee = target->llvm;
    fprintf(stderr, "DEBUG: write_send target %s (Fun %p), llvm=%p\n",
            target->sym->name ? target->sym->name : "unnamed",
            (void*)target, (void*)callee);

    if (!callee) {
         // This should not happen if call graph discovery worked correctly
         // But keep as a safety fallback
         fprintf(stderr, "WARNING: Target function %s (id %d) not discovered during call graph walk!\n",
                 target->sym->name ? target->sym->name : "unnamed",
                 target->sym->id);
         fprintf(stderr, "WARNING: This indicates the function was not reachable during discovery.\n");
         fprintf(stderr, "WARNING: Creating it on-demand, but liveness info may be incomplete.\n");

         callee = createFunction(target, TheModule.get());
         if (!callee) {
             fail("Failed to create target function %s (id %d)",
                  target->sym->name ? target->sym->name : "unnamed",
                  target->sym->id);
             return;
         }

         // Translate the function body immediately for on-demand created functions
         if (!target->is_external && target->entry) {
             fprintf(stderr, "DEBUG: Translating body for on-demand created function %s (id %d)\n",
                     target->sym->name ? target->sym->name : "unnamed",
                     target->sym->id);

             // CRITICAL: Save the current Builder insert point before translating the on-demand function
             // Otherwise, after translating the callee, the Builder will still point to the callee's block,
             // causing subsequent instructions to be added to the wrong function
             llvm::BasicBlock *saved_bb = Builder->GetInsertBlock();
             llvm::BasicBlock::iterator saved_ip = Builder->GetInsertPoint();

             translateFunctionBody(target);

             // Restore the insert point to continue translating the calling function
             if (saved_bb) {
                 Builder->SetInsertPoint(saved_bb, saved_ip);
                 fprintf(stderr, "DEBUG: Restored Builder insert point to calling function after translating %s\n",
                         target->sym->name ? target->sym->name : "unnamed");
             }
         }
    }

    std::vector<llvm::Value *> args;
    fprintf(stderr, "DEBUG: write_send building args. callee expects %d args, call has %d rvals\n",
            (int)callee->arg_size(), n->rvals.n);

    // Map call site arguments to formal parameters (parallels cg.cc:612-616)
    unsigned arg_idx = 0;
    Var *v0 = n->rvals[0];
    forv_MPosition(p, target->positional_arg_positions) {
        Var *formal_arg = target->args.get(p);

        fprintf(stderr, "DEBUG:   formal %d (MPos[0]=%d): formal_arg=%p, live=%d\n", arg_idx,
                p->pos.n > 0 ? (int)Position2int(p->pos[0]) : -1,
                (void*)formal_arg, formal_arg ? formal_arg->live : -1);

        if (!formal_arg || !formal_arg->live) {
            fprintf(stderr, "DEBUG:     Skipping non-live formal\n");
            continue;
        }

        // Skip nested positions (tuple fields) - only handle top-level arguments
        // This matches cg.cc:567 check: if (p->pos.n <= 1)
        if (p->pos.n > 1) {
            fprintf(stderr, "DEBUG:     Skipping nested position (pos.n=%d)\n", p->pos.n);
            continue;
        }

        // Get actual argument from call site using MPosition
        // This logic matches write_send_arg in cg.cc:553-580
        int i = Position2int(p->pos[0]) - 1;  // Convert MPosition to rvals index
        fprintf(stderr, "DEBUG:     MPosition calculation: pos[0]=%d -> i=%d\n",
                (int)Position2int(p->pos[0]), i);

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
            fprintf(stderr, "DEBUG: Arg %d: rval[%d] sym=%s (id=%d)\n", arg_idx, i,
                    actual_arg->sym && actual_arg->sym->name ? actual_arg->sym->name : "(null)",
                    actual_arg->sym ? actual_arg->sym->id : -1);
            llvm::Value *val = getLLVMValue(actual_arg, f);
            if (val) {
                args.push_back(val);
            } else {
                fprintf(stderr, "Warning: Argument %d value is null\n", arg_idx);
                if (arg_idx < callee->arg_size()) {
                    args.push_back(llvm::UndefValue::get(callee->getArg(arg_idx)->getType()));
                }
            }
        } else {
            fprintf(stderr, "Warning: No actual arg for formal parameter %d\n", arg_idx);
            if (arg_idx < callee->arg_size()) {
                args.push_back(llvm::UndefValue::get(callee->getArg(arg_idx)->getType()));
            }
        }
        arg_idx++;
    }

    fprintf(stderr, "DEBUG: Created %zu args for call to %s\n", args.size(), target->sym->name);

    llvm::CallInst *call = Builder->CreateCall(callee, args);
    // Set debug location
    llvm::DISubprogram *sp = f->llvm->getSubprogram();
    int line_num = n->code ? n->code->line() : 0;
    if(sp) call->setDebugLoc(llvm::DILocation::get(*TheContext, line_num, 0, sp));
    
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
    fprintf(stderr, "DEBUG: write_llvm_prim entry. prim index=%d, name=%s\n", n->prim->index, n->prim->name);
    fflush(stderr);

    // Debug: print all rvals
    fprintf(stderr, "DEBUG: rvals.n=%d\n", n->rvals.n);
    for (int i = 0; i < n->rvals.n; i++) {
        fprintf(stderr, "DEBUG: rvals[%d]: sym=%s (id=%d), is_fun=%d\n",
                i,
                n->rvals[i]->sym && n->rvals[i]->sym->name ? n->rvals[i]->sym->name : "(null)",
                n->rvals[i]->sym ? n->rvals[i]->sym->id : -1,
                n->rvals[i]->sym ? n->rvals[i]->sym->is_fun : -1);
    }

    // Determine offset of arguments
    int o = (n->rvals.n > 0 && n->rvals[0]->sym && n->rvals[0]->sym->name &&
             strcmp(n->rvals[0]->sym->name, "primitive") == 0) ? 2 : 1;
    fprintf(stderr, "DEBUG: offset o=%d\n", o);
    fflush(stderr);
    // "primitive" symbol logic from cg.cc: (n->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
    // We don't have sym_primitive available globally? It's in builtin.h, but check name "primitive".
    
    llvm::Function *llvm_func = ifa_fun->llvm;

    switch (n->prim->index) {
        case P_prim_operator: {
             // Hijack for printf via operator(a, ".", b)
             // Expected args: a, ".", b
             if (n->rvals.n < 1) return 0;
             Var *arg1 = n->rvals[0]; // Assuming no "primitive" prefix
             // If prefixed, adjust.
             if (arg1->sym && arg1->sym->name && strcmp(arg1->sym->name, "primitive") == 0) {
                 if (n->rvals.n > 2) arg1 = n->rvals[2];
             }
             
             llvm::Value *val = getLLVMValue(arg1, ifa_fun);
             if (!val) return 0;
             
             llvm::Module *M = ifa_fun->llvm->getParent();
             llvm::FunctionCallee printfFunc = M->getOrInsertFunction("printf", 
                llvm::FunctionType::get(
                    llvm::IntegerType::getInt32Ty(M->getContext()), 
                    llvm::PointerType::getUnqual(M->getContext()), 
                    true));
             llvm::Value *fmt = Builder->CreateGlobalStringPtr("Output: %d\n");
             std::vector<llvm::Value *> args;
             args.push_back(fmt);
             args.push_back(val);
             Builder->CreateCall(printfFunc, args);
             return 1;
        }

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
            Var *op2 = n->rvals[o+2];  // Skip the operator symbol at rvals[o+1]

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
                if (res->getType()->isIntegerTy(1) && lhs->type->size > 1) { // Assuming size in bytes? No, type->size is bits?
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
             // Tuple creation: lval = make(type, elem1, elem2, ...)
             if (n->lvals.n < 1) return 0;
             Var *res_var = n->lvals[0];
             llvm::Type *res_ty = getLLVMType(res_var->type);

             if (!res_ty) {
                 fail("P_prim_make: Could not get result type");
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
                  fprintf(stderr, "P_prim_make: Could not resolve struct type for %s (res_ty is %s)\n",
                          res_var->sym && res_var->sym->name ? res_var->sym->name : "unknown",
                          res_ty->isPointerTy() ? "pointer" : (res_ty->isStructTy() ? "struct" : "other"));
                  return 0;
             }
             
             // Allocate memory
             uint64_t size = TheModule->getDataLayout().getTypeAllocSize(struct_ty);
             llvm::FunctionCallee mallocFunc = TheModule->getOrInsertFunction("malloc", 
                  llvm::FunctionType::get(llvm::PointerType::getUnqual(*TheContext), 
                                          llvm::IntegerType::getInt64Ty(*TheContext), false));
             
             llvm::Value *void_ptr = Builder->CreateCall(mallocFunc, llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(*TheContext), size));
             
             // Bitcast to struct pointer
             // LLVM 18 pointers are opaque, but we might need cast for GEP safety? 
             // Actually opaque pointers mean no bitcast needed for pointer itself, 
             // but GEP needs type.
             llvm::Value *struct_ptr = void_ptr; // Opaque pointer
             
             // Initialize fields
             // rvals: 0=prim, 1="make", 2=type, 3...=elements
             fprintf(stderr, "DEBUG: P_prim_make: initializing %d fields\n", n->rvals.n - 3);
             for (int i = 3; i < n->rvals.n; i++) {
                 int field_idx = i - 3;
                 Var *field_val_var = n->rvals[i];
                 fprintf(stderr, "DEBUG: P_prim_make: field %d, var=%p, sym=%s (id=%d), is_fun=%d\n",
                         field_idx,
                         (void*)field_val_var,
                         field_val_var->sym && field_val_var->sym->name ? field_val_var->sym->name : "(null)",
                         field_val_var->sym ? field_val_var->sym->id : -1,
                         field_val_var->sym ? field_val_var->sym->is_fun : -1);
                 // Skip function symbols - they're not field values
                 if (field_val_var->sym && field_val_var->sym->is_fun) {
                     fprintf(stderr, "DEBUG: P_prim_make: skipping function symbol %s\n",
                             field_val_var->sym->name ? field_val_var->sym->name : "unnamed");
                     continue;
                 }
                 llvm::Value *val = getLLVMValue(field_val_var, ifa_fun);
                 
                 if (val) {
                      llvm::Value *gep = Builder->CreateStructGEP(struct_ty, struct_ptr, field_idx);
                      Builder->CreateStore(val, gep);
                 }
             }
             
             setLLVMValue(res_var, struct_ptr, ifa_fun);
             return 1;
        }
        case P_prim_period: {
             // Struct member access (Getter)
             // lval = rval[1].field
             // Field is specified by rval[3] (Symbol)
             if (n->rvals.n < 4) return 0;
             Var *obj_var = n->rvals[1];
             Var *field_sym_var = n->rvals[3];
             Var *res_var = n->lvals.n > 0 ? n->lvals[0] : nullptr;
             
             if (!res_var) return 1; // No result needed?
             
             llvm::Value *obj_val = getLLVMValue(obj_var, ifa_fun);
             if (!obj_val) {
                 fail("P_prim_period: Object value missing for %s", obj_var->sym->name);
                 return 1;
             }
             
             // Check if it matches closure creation logic in cg.cc?
             // "if (n->lvals[0]->type->type_kind == Type_FUN && n->creates)" ...
             // For now, handle standard struct access.
             
             Sym *obj_type_sym = obj_var->type;
             if (!obj_type_sym) { fail("P_prim_period: Object has no type"); return 1; }
             
             // Resolve field index
             int field_idx = -1;
             cchar *field_name = field_sym_var->sym->name;
             
             // Special case for Tuples "e0", "e1"...?
             // Or iterate obj_type_sym->has
             for (int i = 0; i < obj_type_sym->has.n; i++) {
                 if (obj_type_sym->has[i]->name == field_name || 
                     (obj_type_sym->has[i]->name && field_name && strcmp(obj_type_sym->has[i]->name, field_name) == 0)) {
                     field_idx = i;
                     break;
                 }
             }
             
             if (field_idx == -1) {
                 // Try parsing "eN" if tuple?
                 if (field_name && field_name[0] == 'e' && isdigit(field_name[1])) {
                     field_idx = atoi(field_name + 1);
                 } else {
                     fail("P_prim_period: Could not resolve field %s in type %s", field_name, obj_type_sym->name);
                     return 1;
                 }
             }
             
             // GEP and Load
             // Ensure obj_val is pointer
             if (!obj_val->getType()->isPointerTy()) {
                  fail("P_prim_period: Object is not a pointer"); 
                  return 1;
             }
             
             llvm::Value *gep = Builder->CreateStructGEP(
                 getLLVMType(obj_type_sym), // Pointee type
                 obj_val, 
                 field_idx
             );
             llvm::Value *loaded = Builder->CreateLoad(getLLVMType(res_var->type), gep);
             setLLVMValue(res_var, loaded, ifa_fun);
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

             fprintf(stderr, "DEBUG: P_prim_primitive: name='%s', rvals.n=%d\n", name, n->rvals.n);
             llvm::Module *TheModule = ifa_fun->llvm->getParent();

             fprintf(stderr, "DEBUG: P_prim_primitive: checking if name='%s' is print/println\n", name);

             if (strcmp(name, "print") == 0 || strcmp(name, "println") == 0) {
                 fprintf(stderr, "DEBUG: P_prim_primitive: handling print/println\n");
                 // Declare printf
                 llvm::FunctionCallee printfFunc = TheModule->getOrInsertFunction("printf", 
                    llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(*TheContext), 
                                            llvm::PointerType::getUnqual(*TheContext), true));
                 
                 // Build format string and args
                 // From cg.cc:81, arguments start at index 2
                 std::string fmt_str = "";
                 std::vector<llvm::Value *> args;
                 args.push_back(nullptr); // Placeholder for format string

                 bool is_println = (strcmp(name, "println") == 0);
                 fprintf(stderr, "DEBUG: P_prim_primitive print: processing %d args starting at index 2\n", n->rvals.n - 2);

                 for (int i = 2; i < n->rvals.n; i++) {
                     Var *arg = n->rvals[i];
                     fprintf(stderr, "DEBUG:   print arg %d: type=%p, sym=%s\n", i-2,
                             (void*)arg->type, arg->type && arg->type->name ? arg->type->name : "(null)");

                     llvm::Value *val = getLLVMValue(arg, ifa_fun);
                     if (!val) {
                         fprintf(stderr, "DEBUG:   WARNING: getLLVMValue returned NULL for arg %d\n", i-2);
                         continue;
                     }

                     // Match cg.cc:81-103 type checking logic
                     bool doln = (i == n->rvals.n - 1) && is_println;

                     // Check type based on Var's type symbol (matching cg.cc)
                     if (arg->type == sym_int8 || arg->type == sym_int16 || arg->type == sym_int32) {
                         fmt_str += doln ? "%d\n" : "%d";
                     } else if (arg->type == sym_bool || arg->type == sym_uint8 ||
                                arg->type == sym_uint16 || arg->type == sym_uint32) {
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
                         fprintf(stderr, "DEBUG:   WARNING: unsupported type for arg %d\n", i-2);
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
                     *TheModule, fmt_const->getType(), true, llvm::GlobalValue::PrivateLinkage, 
                     fmt_const, ".str.fmt");
                 
                 // Get pointer to start of format string
                 llvm::Value *fmt_ptr = Builder->CreateInBoundsGEP(
                     fmt_const->getType(), fmt_global, 
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
                 Var* ret_val_var = n->rvals[3];

                 // Check if return value is void symbol/type
                 if (ret_val_var->sym && ret_val_var->sym->name &&
                     strcmp(ret_val_var->sym->name, "void") == 0) {
                     // Returning void - treat as void return even if function type isn't void
                     Builder->CreateRetVoid();
                     return 1;
                 }

                 // Check if return type is void_type
                 if (ret_val_var->type == sym_void_type || ret_val_var->type == sym_void) {
                     Builder->CreateRetVoid();
                     return 1;
                 }

                 llvm::Value* ret_llvm_val = getLLVMValue(ret_val_var, ifa_fun);
                 if (ret_llvm_val) {
                     std::string ret_type_str, expected_type_str;
                     llvm::raw_string_ostream ret_os(ret_type_str), exp_os(expected_type_str);
                     ret_llvm_val->getType()->print(ret_os);
                     llvm_func->getReturnType()->print(exp_os);
                     fprintf(stderr, "DEBUG: prim_reply return value type: %s, expected: %s\n",
                             ret_os.str().c_str(), exp_os.str().c_str());

                     if (ret_llvm_val->getType() == llvm_func->getReturnType()) {
                        Builder->CreateRet(ret_llvm_val);
                     } else {
                         Builder->CreateRet(ret_llvm_val); // Might verify fail - let LLVM catch type mismatch
                     }
                 } else {
                     fail("Could not get return value for non-void function %s", ifa_fun->sym->name);
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
