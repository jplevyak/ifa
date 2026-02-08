/* -*-Mode: c++;-*-
   Copyright (c) 2023 John Plevyak, All Rights Reserved
*/
#include "llvm_internal.h"
#include "prim.h"
#include "var.h"
#include <set>

// ============================================================================
// Basic Block Management
// ============================================================================

std::map<Label *, llvm::BasicBlock *> label_to_bb_map; // Global map for current function

llvm::BasicBlock *getLLVMBasicBlock(Label *label, llvm::Function *current_llvm_fun) {
    if (!label) {
        fail("Null Label provided to getLLVMBasicBlock");
        return nullptr;
    }
    if (label_to_bb_map.count(label)) {
        return label_to_bb_map[label];
    }
    // Labels in IF1 are often just numbers (label->id).
    // We need a unique name for the BasicBlock.
    std::string bb_name = "label_" + std::to_string(label->id);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*TheContext, bb_name, current_llvm_fun);
    label_to_bb_map[label] = bb;
    label->bb = bb; // Store in Label struct as well, consistent with code.h
    return bb;
}

// Forward declare
static void translatePNode(PNode *pn, Fun *ifa_fun);
llvm::Function *createFunction(Fun *ifa_fun, llvm::Module *module) {
  if (!ifa_fun) {
    fail("Null Fun provided to createFunction");
    return nullptr;
  }
  fprintf(stderr, "DEBUG: createFunction entry for %s (id %d)\n", ifa_fun->sym->name, ifa_fun->sym->id);

  if (ifa_fun->llvm) return ifa_fun->llvm;
  if (!ifa_fun->sym) {
    fail("Fun %p has no symbol associated.", (void*)ifa_fun);
    return nullptr;
  }

  // Determine return type
  fprintf(stderr, "DEBUG: createFunction step 1: determine ret type for %d rets\n", ifa_fun->rets.n);
  llvm::Type *llvm_ret_type;
  fprintf(stderr, "DEBUG: check rets: n=%d, rets[0]=%p, rets[0]->type=%p\n",
          ifa_fun->rets.n,
          (ifa_fun->rets.n > 0 ? ifa_fun->rets[0] : nullptr),
          (ifa_fun->rets.n > 0 && ifa_fun->rets[0] ? ifa_fun->rets[0]->type : nullptr));

  if (ifa_fun->rets.n == 1 && ifa_fun->rets[0]) {
      Var *ret_var = ifa_fun->rets[0];
      if (!ret_var->type || !ret_var->type->cg_string) {
          llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
      } else {
          llvm_ret_type = getLLVMType(ret_var->type);
      }
  } else if (ifa_fun->rets.n == 0) {
    llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
  } else {
      fprintf(stderr, "DEBUG: Unsupported ret count %d\n", ifa_fun->rets.n);
    fail("Function %s has %d return values, unsupported. Assumed void or single.", ifa_fun->sym->name, ifa_fun->rets.n);
    llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
  }
  if (!llvm_ret_type) {
      fail("Could not determine LLVM return type for function %s", ifa_fun->sym->name);
      return nullptr;
  }

  // Build argument list (parallels cg.cc's write_c_fun_proto at cg.cc:31-60)
  fprintf(stderr, "DEBUG: createFunction step 2: args\n");
  std::vector<llvm::Type *> llvm_arg_types;
  std::vector<Var *> live_args;

  fprintf(stderr, "DEBUG: Function %s has %d formal parameters (sym->has.n)\n",
          ifa_fun->sym->name, ifa_fun->sym->has.n);

  MPosition p;
  p.push(1);
  for (int i = 0; i < ifa_fun->sym->has.n; i++) {
    MPosition *cp = cannonicalize_mposition(p);
    p.inc();
    Var *arg_var = ifa_fun->args.get(cp);
    fprintf(stderr, "DEBUG:   formal %d: arg_var=%p, live=%d, sym=%s\n",
            i, arg_var,
            arg_var ? arg_var->live : -1,
            arg_var && arg_var->sym && arg_var->sym->name ? arg_var->sym->name : "(null)");
    // Check if argument is live according to either dead code elimination or FA
    bool arg_is_live = false;
    if (arg_var) {
        arg_is_live = arg_var->live;
        // Also check FA liveness by looking at AVars
        if (!arg_is_live) {
            int fa_live_count = 0;
            form_AVarMapElem(x, arg_var->avars) {
                AVar *av = x->value;
                if (av && av->live) {
                    arg_is_live = true;
                    fa_live_count++;
                }
            }
            if (fa_live_count > 0) {
                fprintf(stderr, "DEBUG:     Found %d FA-live AVars for arg %s\n", fa_live_count,
                        arg_var->sym && arg_var->sym->name ? arg_var->sym->name : "(null)");
            }
        }
    }
    if (arg_var && arg_is_live) {
      if (arg_var->type && arg_var->type->is_fun) {
        fprintf(stderr, "DEBUG:   Skipping function-typed formal %d\n", i);
        continue;
      }
      live_args.push_back(arg_var);
    }
  }

  fprintf(stderr, "DEBUG: Found %zu live args for %s\n", live_args.size(), ifa_fun->sym->name);

  // Now create LLVM types for live args
  for (Var *arg_var : live_args) {
    fprintf(stderr, "DEBUG: processing live arg %p (id %d, sym=%s)\n",
            arg_var, arg_var->id,
            arg_var->sym && arg_var->sym->name ? arg_var->sym->name : "(null)");

    llvm::Type *arg_llvm_type = nullptr;
    if (arg_var->type) {
      arg_llvm_type = getLLVMType(arg_var->type);

      // For struct/tuple types, function arguments should be pointers
      // (matching the C backend convention)
      if (arg_llvm_type && arg_llvm_type->isStructTy() && arg_var->type->type_kind == Type_RECORD) {
        fprintf(stderr, "DEBUG: Wrapping struct arg type in pointer for arg %s\n",
                arg_var->sym && arg_var->sym->name ? arg_var->sym->name : "(unnamed)");
        arg_llvm_type = llvm::PointerType::getUnqual(arg_llvm_type);
      }
    } else {
      arg_llvm_type = llvm::Type::getInt64Ty(*TheContext); // Fallback
    }

    if (arg_llvm_type) {
      llvm_arg_types.push_back(arg_llvm_type);
    } else {
      fail("Could not get LLVM type for argument %s of function %s",
           arg_var->sym && arg_var->sym->name ? arg_var->sym->name : "unnamed_arg",
           ifa_fun->sym->name);
      return nullptr;
    }
  }

  fprintf(stderr, "DEBUG: createFunction step 3: FunctionType::get\n");
  llvm::FunctionType *func_type = llvm::FunctionType::get(llvm_ret_type, llvm_arg_types, ifa_fun->is_varargs);

  // 3. Create llvm::Function
  fprintf(stderr, "DEBUG: createFunction step 4: Function::Create\n");
  std::string func_name = (ifa_fun->sym->name ? ifa_fun->sym->name : "func") + std::to_string(ifa_fun->id);
  if (ifa_fun->cg_string && strncmp(ifa_fun->cg_string, "_CG_", 4) == 0) {
      func_name = ifa_fun->cg_string;
  }
  
  if (!module) fail("Module is null");
  if (!func_type) fail("FuncType is null");

  // Linkage logic
  llvm::Function::LinkageTypes linkage = llvm::Function::ExternalLinkage;
  // FIX: Force External if no entry, regardless of builtin status
  if (!ifa_fun->entry) {
      linkage = llvm::Function::ExternalLinkage;
  } else {
      linkage = llvm::Function::InternalLinkage;
  }
  fprintf(stderr, "DEBUG: Linkage Fixed: entry=%p -> %d\n", ifa_fun->entry, (int)linkage);

  fprintf(stderr, "DEBUG: func_name='%s', module=%p, func_type=%p, varargs=%d, linkage=%d\n", 
          func_name.c_str(), module, func_type, ifa_fun->is_varargs, (int)linkage);

  llvm::Function *llvm_func = llvm::Function::Create(func_type, linkage, func_name, module);
  fprintf(stderr, "DEBUG: Function created %p\n", llvm_func);
  ifa_fun->llvm = llvm_func; // Store it

  // 3. Set argument names and store llvm::Argument in Var::llvm_value
  fprintf(stderr, "DEBUG: Setting arg names for %zu live args\n", live_args.size());
  for (unsigned i = 0; i < live_args.size() && i < llvm_func->arg_size(); ++i) {
      llvm::Argument *llvm_arg = llvm_func->getArg(i);
      Var *arg_var = live_args[i];

      if (arg_var->sym && arg_var->sym->name) {
          llvm_arg->setName(arg_var->sym->name);
      }
      arg_var->llvm_value = llvm_arg;
      fprintf(stderr, "DEBUG: Mapped arg %d: %s\n", i,
              arg_var->sym && arg_var->sym->name ? arg_var->sym->name : "(unnamed)");
  }
  fprintf(stderr, "DEBUG: Finished mapping arguments\n");

  fprintf(stderr, "DEBUG: Starting Entry Block creation. Entry=%p, External=%d\n", ifa_fun->entry, ifa_fun->is_external);
  fprintf(stderr, "DEBUG: TheContext=%p, llvm_func=%p\n", TheContext.get(), llvm_func);
  if (!TheContext) fail("TheContext is null");
  if (!llvm_func) fail("llvm_func is null");

  if (!ifa_fun->is_external && ifa_fun->entry) {
       llvm::BasicBlock *entry_bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_func);
       fprintf(stderr, "DEBUG: Entry Block Created: %p\n", entry_bb);
  }

  // Create Debug Info for this function
  if (DBuilder && UnitFile) {
    // Get source location info
    unsigned line_num = 0;
    if (ifa_fun->sym && ifa_fun->sym->ast) {
      line_num = ifa_fun->sym->ast->source_line();  // Use source_line() to get actual source line, not generated code
      fprintf(stderr, "DEBUG: Function %s: ast->line()=%d, ast->source_line()=%d\n",
              ifa_fun->sym->name, ifa_fun->sym->ast->line(), line_num);
    } else if (ifa_fun->entry && ifa_fun->entry->code) {
      line_num = ifa_fun->entry->code->source_line();  // Use source_line() instead of line()
      fprintf(stderr, "DEBUG: Function %s: code->line()=%d, code->source_line()=%d\n",
              ifa_fun->sym->name, ifa_fun->entry->code->line(), line_num);
    }

    // Create DISubroutineType
    llvm::SmallVector<llvm::Metadata *, 8> di_param_types;

    // Return type
    if (ifa_fun->rets.n == 1 && ifa_fun->rets[0] && ifa_fun->rets[0]->type) {
      llvm::DIType *di_ret_type = getLLVMDIType(ifa_fun->rets[0]->type, UnitFile);
      di_param_types.push_back(di_ret_type);
    } else {
      di_param_types.push_back(nullptr); // void return
    }

    // Parameter types
    for (Var *arg_var : live_args) {
      if (arg_var && arg_var->type) {
        llvm::DIType *di_arg_type = getLLVMDIType(arg_var->type, UnitFile);
        di_param_types.push_back(di_arg_type);
      } else {
        di_param_types.push_back(nullptr);
      }
    }

    llvm::DISubroutineType *di_func_type = DBuilder->createSubroutineType(
      DBuilder->getOrCreateTypeArray(di_param_types));

    // Create DISubprogram
    llvm::DISubprogram *sp = DBuilder->createFunction(
      UnitFile,                           // Scope
      func_name,                          // Name
      llvm_func->getName(),               // Linkage name
      UnitFile,                           // File
      line_num,                           // Line number
      di_func_type,                       // Type
      line_num,                           // ScopeLine
      llvm::DINode::FlagPrototyped,       // Flags
      llvm::DISubprogram::SPFlagDefinition // SPFlags
    );

    llvm_func->setSubprogram(sp);
    fprintf(stderr, "DEBUG: Created DISubprogram for %s at line %u\n", func_name.c_str(), line_num);

    // Create debug info for ALL source-level function parameters
    // This includes parameters that were optimized away
    MPosition p2;
    p2.push(1);
    for (int i = 0; i < ifa_fun->sym->has.n; i++) {
      MPosition *cp2 = cannonicalize_mposition(p2);
      p2.inc();
      Var *arg_var = ifa_fun->args.get(cp2);

      if (arg_var && arg_var->sym && arg_var->type && !arg_var->type->is_fun) {
        llvm::DIType *arg_di_type = getLLVMDIType(arg_var->type, UnitFile);

        if (arg_di_type) {
          unsigned arg_line = arg_var->sym->source_line() ? arg_var->sym->source_line() : line_num;
          llvm::DILocalVariable *param_var = DBuilder->createParameterVariable(
            sp,                                                  // Scope
            arg_var->sym->name ? arg_var->sym->name : "arg",    // Name
            i + 1,                                               // Arg number (1-based)
            UnitFile,                                            // File
            arg_line,                                            // Line
            arg_di_type,                                         // Type
            true                                                 // AlwaysPreserve
          );

          // Store the debug variable info
          arg_var->llvm_debug_var = param_var;

          // Track if this is a live arg (has actual LLVM parameter)
          bool is_live = arg_var->live;
          fprintf(stderr, "DEBUG: Created DIParameter for arg %d: %s (live=%d)\n", i,
                  arg_var->sym->name ? arg_var->sym->name : "(unnamed)", is_live);
        }
      }
    }
  }

  // Note: Function body translation is done in a separate pass after all functions are created
  // This ensures all function declarations exist before any body tries to call them

  // For functions that won't get translateFunctionBody called (external or no entry point),
  // ensure any created basic blocks have terminators
  if (ifa_fun->is_external || !ifa_fun->entry) {
    for (llvm::BasicBlock &BB : *llvm_func) {
      if (!BB.getTerminator()) {
        fprintf(stderr, "DEBUG: Basic block %s in function %s (id %d) has no terminator after createFunction, adding default\n",
                BB.getName().str().c_str(), ifa_fun->sym->name, ifa_fun->sym->id);
        // Create a builder - if block is not empty, insert after last instruction
        // Otherwise insert at beginning
        llvm::IRBuilder<> temp_builder(*TheContext);
        if (BB.empty()) {
          temp_builder.SetInsertPoint(&BB);
        } else {
          temp_builder.SetInsertPoint(&BB, BB.end());
        }
        if (llvm_func->getReturnType()->isVoidTy()) {
          temp_builder.CreateRetVoid();
        } else {
          temp_builder.CreateRet(llvm::UndefValue::get(llvm_func->getReturnType()));
        }
      }
    }
  }

  fprintf(stderr, "DEBUG: Finished createFunction for %d\n", ifa_fun->sym->id);
  return llvm_func;
}


// --- PNode Translation ---
void translateFunctionBody(Fun *ifa_fun) {
    fprintf(stderr, "DEBUG: translateFunctionBody entry for %s (id %d)\n", ifa_fun->sym->name, ifa_fun->id);
    if (!ifa_fun || !ifa_fun->llvm || !ifa_fun->entry) {
        fail("Invalid function or missing LLVM function/entry PNode for translateFunctionBody");
        return;
    }

    label_to_bb_map.clear(); // Clear map for each function
    
    llvm::Function *llvm_func = ifa_fun->llvm;

    // Create all basic blocks first by iterating PNodes once to find labels
    // This helps with forward branches.
    Vec<PNode *> pnodes;
    ifa_fun->collect_PNodes(pnodes); // Assumes this collects all relevant PNodes
    fprintf(stderr, "DEBUG: Collected %d PNodes\n", pnodes.n);

    if (ifa_fun->fa_all_Vars.n == 0) {
        fprintf(stderr, "DEBUG: fa_all_Vars empty, collecting vars...\n");
        ifa_fun->collect_Vars(ifa_fun->fa_all_Vars);
        fprintf(stderr, "DEBUG: Collected %d Vars\n", ifa_fun->fa_all_Vars.n);
    }

    forv_PNode(pn, pnodes) {
        if (pn->code) {
             if (pn->code->kind == Code_LABEL) {
                 if (pn->code->label[0]) {
                     getLLVMBasicBlock(pn->code->label[0], llvm_func);
                 }
             }
        }
    }

    fprintf(stderr, "DEBUG: Done labels loop. Checking entry code...\n");
    // Ensure entry block is correctly mapped if it's from a label
    if (ifa_fun->entry->code) {
        fprintf(stderr, "DEBUG: Entry code kind=%d\n", ifa_fun->entry->code->kind);
    } else {
        fprintf(stderr, "DEBUG: Entry code is NULL\n");
    }

    if (ifa_fun->entry->code && ifa_fun->entry->code->kind == Code_LABEL && ifa_fun->entry->code->label[0]) {
         llvm::BasicBlock* entry_bb = getLLVMBasicBlock(ifa_fun->entry->code->label[0], llvm_func);
         if (entry_bb != &llvm_func->getEntryBlock() && llvm_func->getEntryBlock().empty()) {
             // LLVM auto-creates an entry block - branch from it to our labeled entry
             Builder->SetInsertPoint(&llvm_func->getEntryBlock());
             Builder->CreateBr(entry_bb);
         } else if (entry_bb != &llvm_func->getEntryBlock()) {
             Builder->SetInsertPoint(&llvm_func->getEntryBlock());
             if (llvm_func->getEntryBlock().getTerminator() == nullptr) {
                Builder->CreateBr(entry_bb);
             }
         }
    }

    // Allocate local variables using AllocaInst (for mutable locals)


    fprintf(stderr, "DEBUG: Setting Builder InsertPoint to EntryBlock\n");
    if (llvm_func->getEntryBlock().empty()) {
        fprintf(stderr, "DEBUG: EntryBlock is empty, setting to beginning\n");
    } else {
        fprintf(stderr, "DEBUG: EntryBlock not empty, setting to beginning\n");
    }
    
    // Check Builder
    if (!Builder) {
        fail("Builder is null in translateFunctionBody");
        return;
    }
    Builder->SetInsertPoint(&llvm_func->getEntryBlock(), llvm_func->getEntryBlock().begin()); // Allocas at the top

    // Clear debug location before allocating locals to avoid wrong subprogram references
    Builder->SetCurrentDebugLocation(llvm::DebugLoc());

    fprintf(stderr, "DEBUG: InsertPoint set. Getting subprogram for locals...\n");
    llvm::DIFile* di_file_for_locals = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getFile() : nullptr;
    fprintf(stderr, "DEBUG: Got subprogram components\n");
    unsigned func_start_line = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getLine() : 0;

    fprintf(stderr, "DEBUG: Starting local vars loop\n");
    // Multiple Vars can share the same Sym (e.g., loop variable 'i' appears multiple times)
    // Create only one alloca per Sym to avoid duplicate allocations
    std::map<Sym*, llvm::AllocaInst*> sym_to_alloca;
    int var_loop_idx = 0;
    forv_Var(v, ifa_fun->fa_all_Vars) { // Or a more specific list of locals
        fprintf(stderr, "DEBUG: Loop idx %d, v=%p", var_loop_idx++, v);
        if (v && v->sym) {
            fprintf(stderr, ", sym=%s (id=%d), is_local=%d, is_formal=%d, has_llvm_value=%d",
                    v->sym->name ? v->sym->name : "(null)",
                    v->sym->id,
                    v->sym->is_local ? 1 : 0,
                    v->is_formal ? 1 : 0,
                    v->llvm_value ? 1 : 0);
            if (v->llvm_value && llvm::isa<llvm::Constant>(v->llvm_value)) {
                fprintf(stderr, ", llvm_value is constant: ");
                v->llvm_value->print(llvm::errs());
            }
            fprintf(stderr, "\n");
        } else {
            fprintf(stderr, ", no sym\n");
        }
        fflush(stderr);

        if (v && v->sym && v->sym->is_local && !v->is_formal) {
            auto it = sym_to_alloca.find(v->sym);
            if (it != sym_to_alloca.end()) {
                v->llvm_value = it->second;
                v->llvm_type = it->second->getAllocatedType();
                fprintf(stderr, "DEBUG: Reusing existing alloca for var %s (id %d)\n", v->sym->name ? v->sym->name : "(null)", v->sym->id);
                continue;
            }
        }

        if (v && v->sym && v->sym->is_local && !v->is_formal && !v->llvm_value) {
            llvm::Type *var_llvm_type = nullptr;
            if (!v->type) {
                fprintf(stderr, "DEBUG: Defaulting local var %s (id %d) with null type to i64\n", v->sym->name, v->sym->id);
                var_llvm_type = llvm::Type::getInt64Ty(*TheContext);
            } else {
                var_llvm_type = getLLVMType(v->type);
            }

            if (var_llvm_type && !var_llvm_type->isVoidTy()) {
                llvm::AllocaInst *alloca_inst = Builder->CreateAlloca(var_llvm_type, nullptr, v->sym->name ? v->sym->name : "local_var");
                v->llvm_value = alloca_inst;
                v->llvm_type = var_llvm_type;
                sym_to_alloca[v->sym] = alloca_inst;

                // Add debug info for this local variable
                if (DBuilder && llvm_func->getSubprogram() && di_file_for_locals) {
                    llvm::DIType *var_di_type = getLLVMDIType(v->type, di_file_for_locals);
                    unsigned var_line = v->sym->source_line() ? v->sym->source_line() : func_start_line; // Prefer var's own line

                    if (var_di_type) {
                        llvm::DILocalVariable *dil_var = DBuilder->createAutoVariable(
                            llvm_func->getSubprogram(), // Scope
                            v->sym->name ? v->sym->name : "var",
                            UnitFile,
                            var_line,
                            getLLVMDIType(v->type, UnitFile)
                        );

                        DBuilder->insertDeclare(
                             alloca_inst,
                             dil_var,
                             DBuilder->createExpression(),
                             llvm::DILocation::get(*TheContext, var_line, 0, llvm_func->getSubprogram()),
                             Builder->GetInsertBlock()
                        );
                    }
                }

            } else if (!var_llvm_type) {
                fail("Could not get LLVM type for local var %s", v->sym->name);
            }
        } else if (v && v->sym && v->sym->is_local && v->is_formal && !v->llvm_value) {
            // Handle tuple field parameters: variables marked as both is_local and is_formal
            // These are fields that need to be extracted from the tuple argument
            // For now, extract them from the corresponding function argument
            fprintf(stderr, "DEBUG: Handling tuple field formal: %s (id=%d)\n",
                    v->sym->name ? v->sym->name : "(null)", v->sym->id);

            // Find the tuple argument this field belongs to
            // The live arguments were stored in the function's llvm Function
            // We need to map this formal to the actual argument
            // For tuple operators, there's typically one tuple argument
            if (llvm_func->arg_size() > 0) {
                // Get the first (and likely only) argument - the tuple
                llvm::Argument *tuple_arg = llvm_func->arg_begin();

                // We need to extract the field from the tuple
                // But we need to know which field index this corresponds to
                // The C backend uses a1->e0, a1->e2, etc.
                // For now, let's create a placeholder that we'll fix later
                // We need more info about field mapping

                fprintf(stderr, "DEBUG: Tuple field formal needs extraction from tuple arg\n");
                // TODO: Need to determine field index and extract
            }
        }
    }
    fprintf(stderr, "DEBUG: Finished local vars loop\n");

    // Emit debug info for function parameters
    // This must be done after allocas are created, at the beginning of the function
    if (DBuilder && llvm_func->getSubprogram()) {
        MPosition p;
        p.push(1);
        for (int i = 0; i < ifa_fun->sym->has.n; i++) {
            MPosition *cp = cannonicalize_mposition(p);
            p.inc();
            Var *arg_var = ifa_fun->args.get(cp);

            if (arg_var && arg_var->llvm_debug_var) {
                unsigned arg_line = arg_var->sym->source_line() ? arg_var->sym->source_line() : func_start_line;
                llvm::DILocation *debug_loc = llvm::DILocation::get(*TheContext, arg_line, 0, llvm_func->getSubprogram());

                if (arg_var->llvm_value) {
                    // Parameter has a value (live parameter)
                    DBuilder->insertDbgValueIntrinsic(
                        arg_var->llvm_value,                    // Value
                        arg_var->llvm_debug_var,                // Variable
                        DBuilder->createExpression(),           // Expression
                        debug_loc,                              // Location
                        Builder->GetInsertBlock()               // Insert at current position
                    );
                    fprintf(stderr, "DEBUG: Emitted dbg.value for parameter %s\n",
                            arg_var->sym->name ? arg_var->sym->name : "(unnamed)");
                } else {
                    // Parameter was optimized away
                    // Don't emit a value - debugger will show it as "<optimized out>"
                    fprintf(stderr, "DEBUG: Parameter %s is optimized out (no debug value emitted)\n",
                            arg_var->sym->name ? arg_var->sym->name : "(unnamed)");
                }
            }
        }
    }

    // Now translate PNodes in order
    // A more robust way would be to iterate over basic blocks in some order (e.g., RPO)
    // and then PNodes within those blocks. For now, linear scan of collected PNodes.
    std::set<PNode *> visited_pnodes; // To handle graph traversal correctly if not linear

    // We need a worklist approach for translating PNodes based on CFG
    std::vector<PNode *> worklist;
    if (ifa_fun->entry) {
        worklist.push_back(ifa_fun->entry);
        visited_pnodes.insert(ifa_fun->entry);
        fprintf(stderr, "DEBUG: Starting worklist with entry PNode %p\n", (void*)ifa_fun->entry);
    } else {
        fprintf(stderr, "DEBUG: WARNING: ifa_fun->entry is NULL, worklist will be empty!\n");
    }
    fprintf(stderr, "DEBUG: Starting worklist with %zu items\n", worklist.size());

    unsigned worklist_idx = 0;
    while(worklist_idx < worklist.size()){
        PNode *current_pn = worklist[worklist_idx++];
        if (!current_pn) {
            fprintf(stderr, "DEBUG: Null PNode in worklist, skipping\n");
            continue;
        }
        translatePNode(current_pn, ifa_fun);

        // Add successors to worklist - ALWAYS add cfg_succ like C backend does (cg.cc:657-689)
        // C backend traverses CFG regardless of whether node is live
        if (current_pn->cfg_succ.n > 0) {
            forv_PNode(succ_pn, current_pn->cfg_succ) {
                if (succ_pn && visited_pnodes.find(succ_pn) == visited_pnodes.end()) {
                    worklist.push_back(succ_pn);
                    visited_pnodes.insert(succ_pn);
                }
            }
        }
    }
    fprintf(stderr, "DEBUG: Finished worklist processing\n");

    // Ensure all basic blocks have terminators
    std::string full_func_name = llvm_func->getName().str();
    fprintf(stderr, "DEBUG: Checking terminators for function %s (LLVM name: %s)\n",
            ifa_fun->sym->name, full_func_name.c_str());
    for (llvm::BasicBlock &BB : *llvm_func) {
        if (!BB.getTerminator()) {
            fprintf(stderr, "DEBUG: Basic block %s in function %s has no terminator (size=%zu), adding default\n",
                    BB.getName().str().c_str(), full_func_name.c_str(), BB.size());

            // Print existing instructions
            fprintf(stderr, "DEBUG: Existing instructions in block:\n");
            int inst_idx = 0;
            for (llvm::Instruction &I : BB) {
                fprintf(stderr, "DEBUG:   [%d] ", inst_idx++);
                I.print(llvm::errs());
                fprintf(stderr, "\n");
            }

            // Create a new IRBuilder for this specific terminator addition
            // This ensures we don't interfere with any ongoing translation
            llvm::IRBuilder<> temp_builder(*TheContext);
            temp_builder.SetInsertPoint(&BB);

            // Check return type
            llvm::Type *ret_type = llvm_func->getReturnType();
            fprintf(stderr, "DEBUG: Return type is void: %d\n", ret_type->isVoidTy());

            // Add appropriate terminator based on function return type
            llvm::Instruction *term_inst = nullptr;
            if (ret_type->isVoidTy()) {
                fprintf(stderr, "DEBUG: Creating RetVoid...\n");
                term_inst = temp_builder.CreateRetVoid();
                fprintf(stderr, "DEBUG: Added RetVoid to block %s\n", BB.getName().str().c_str());
            } else {
                fprintf(stderr, "DEBUG: Creating Ret with UndefValue...\n");
                llvm::Value *undef = llvm::UndefValue::get(ret_type);
                fprintf(stderr, "DEBUG: Created UndefValue\n");
                term_inst = temp_builder.CreateRet(undef);
                fprintf(stderr, "DEBUG: Added Ret(undef) to block %s\n", BB.getName().str().c_str());
            }

            // Print the created terminator
            if (term_inst) {
                fprintf(stderr, "DEBUG: Created terminator instruction: ");
                term_inst->print(llvm::errs());
                fprintf(stderr, "\n");
            }

            // Verify terminator was added
            if (!BB.getTerminator()) {
                fprintf(stderr, "ERROR: Failed to add terminator to block %s!\n", BB.getName().str().c_str());
            } else {
                fprintf(stderr, "DEBUG: Terminator successfully added. Block now has %zu instructions\n", BB.size());
            }
        }
    }
    fprintf(stderr, "DEBUG: Finished translateFunctionBody for %s\n", ifa_fun->sym->name);
}


static void simple_move(Var *lhs, Var *rhs, Fun *ifa_fun) {
    // Early returns matching C backend's simple_move (cg.cc:497-501)
    if (!lhs->live) return;  // Skip if LHS not live
    if (!rhs->type || !lhs->type) return;  // Skip if no types
    if (rhs->type == sym_void->type || lhs->type == sym_void->type) return;  // Skip void types

    llvm::Value *val = getLLVMValue(rhs, ifa_fun);
    if (!val) {
        // RHS doesn't have an LLVM value yet (might be dead code or forward reference)
        // C backend would skip this with: if (!rhs->cg_string) return;
        return;
    }
    setLLVMValue(lhs, val, ifa_fun);
}

static void do_phy_nodes(PNode *n, int isucc, Fun *ifa_fun) {
    forv_PNode(p, n->phy) {
        if (p->lvals.n > isucc && p->rvals.n > 0) { // Safety check
             simple_move(p->lvals[isucc], p->rvals[0], ifa_fun);
        }
    }
}

static void do_phi_nodes(PNode *n, int isucc, Fun *ifa_fun) {
    if (n->cfg_succ.n > isucc) {
        PNode *succ = n->cfg_succ[isucc];
        if (succ->phi.n) {
             // Find predecessor index
             int pred_idx = succ->cfg_pred_index.get(n);
             forv_PNode(pp, succ->phi) {
                 if (pp->rvals.n > pred_idx) {
                    simple_move(pp->lvals[0], pp->rvals[pred_idx], ifa_fun);
                 }
             }
        }
    }
}


void translatePNode(PNode *pn, Fun *ifa_fun) {
    if (!pn) return;
    if (!pn->code) {
        fprintf(stderr, "DEBUG: translatePNode: pn->code is NULL, skipping pn=%p\n", (void*)pn);
        return;
    }

    int code_kind = pn->code->kind;
    // Sanity check code_kind - valid values should be < 100
    if (code_kind < 0 || code_kind > 100) {
        fprintf(stderr, "ERROR: Invalid code_kind %d for pn=%p, skipping\n", code_kind, (void*)pn);
        return;
    }

    // Liveness check
    // Trust FA analysis (fa_live) - it's more accurate than dead.cc's live flag
    // The C backend checks both live && fa_live, but LLVM backend needs fa_live
    // to ensure control flow targets (labels) and value-producing operations are generated
    bool is_live = pn->fa_live;
    if (!is_live) {
        fprintf(stderr, "DEBUG: Skipping non-live PNode pn=%p (live=%d, fa_live=%d), code_kind=%d, lvals.n=%d",
                (void*)pn, pn->live, pn->fa_live, code_kind, pn->lvals.n);
        if (pn->lvals.n > 0 && pn->lvals[0]) {
            fprintf(stderr, ", lval[0] id=%d", pn->lvals[0]->id);
        }
        fprintf(stderr, ", but will process CFG successors\n");
        // Skip the instruction translation but continue to CFG processing at the end
        return; // For now, still return - we'll fix CFG traversal in worklist instead
    }

    fprintf(stderr, "DEBUG: translatePNode entry. pn=%p, code_kind=%d, line=%d\n", (void*)pn, code_kind, pn->code->line());
    fflush(stderr);
    if (!ifa_fun || !ifa_fun->llvm) {
        fail("Invalid ifa_fun or missing llvm_func for PNode translation");
        return;
    }
    llvm::Function *llvm_func = ifa_fun->llvm;

    // Check if current block already has a terminator or if there's no insert point
    // (except for Code_LABEL which sets a new insert point)
    if (code_kind != Code_LABEL) {
        llvm::BasicBlock *current_bb = Builder->GetInsertBlock();
        if (!current_bb) {
            fprintf(stderr, "DEBUG: No insert point set, skipping pn=%p, code_kind=%d\n", (void*)pn, code_kind);
            return;
        }
        if (current_bb->getTerminator()) {
            fprintf(stderr, "DEBUG: Current block already has terminator, skipping pn=%p, code_kind=%d, block_name=%s\n",
                    (void*)pn, code_kind, current_bb->getName().str().c_str());
            return;
        }
    }

    // Set current debug location
    unsigned line = pn->code->source_line();  // Use source_line() to get actual source line
    if (line == 0 && ifa_fun->ast) {
        line = ifa_fun->source_line();  // Use source_line() here too
    }
    unsigned col = 0; 
    llvm::DISubprogram *sp = llvm_func->getSubprogram();
    if (sp) {
        Builder->SetCurrentDebugLocation(llvm::DILocation::get(*TheContext, line, col, sp));
    } else {
        Builder->SetCurrentDebugLocation(llvm::DebugLoc());
    }

    switch (pn->code->kind) {
        case Code_LABEL: {
             if (pn->code->label[0]) {
                llvm::BasicBlock *bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                llvm::BasicBlock *current_bb = Builder->GetInsertBlock();
                // Avoid creating self-loop branches (br label %X inside %X)
                if (current_bb && current_bb != bb && !current_bb->getTerminator()) {
                    Builder->CreateBr(bb);
                }
                Builder->SetInsertPoint(bb);
            } else {
                fail("Code_LABEL PNode has no Label object");
            }
            break;
        }
        case Code_GOTO: {
            do_phi_nodes(pn, 0, ifa_fun);
            if (pn->code->label[0]) {
                llvm::BasicBlock *dest_bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                if (!Builder->GetInsertBlock()) {
                     fail("Builder has no insert block for GOTO. Ensure prior LABEL or entry setup.");
                     return;
                }
                Builder->CreateBr(dest_bb);
            } else {
                fail("Code_GOTO PNode has no destination Label object");
            }
            break;
        }
        case Code_MOVE: {
            if (pn->lvals.n == 1 && pn->rvals.n == 1) {
                Var *lhs_var = pn->lvals[0];
                Var *rhs_var = pn->rvals[0];
                simple_move(lhs_var, rhs_var, ifa_fun); // Use simple_move helper
            } else {
                fail("Code_MOVE PNode with unhandled number of lvals/rvals (%d/%d)", pn->lvals.n, pn->rvals.n);
            }
            break;
        }
        case Code_IF: {
             if (pn->rvals.n > 0) {
                Var* cond_var = pn->rvals[0];
                fprintf(stderr, "DEBUG: Code_IF condition var=%p, sym=%s, id=%d\n",
                        (void*)cond_var, cond_var->sym ? cond_var->sym->name : "(null)", cond_var->id);
                fflush(stderr);
                llvm::Value* cond_llvm_val = getLLVMValue(cond_var, ifa_fun);
                if (!cond_llvm_val) {
                    // Dead code eliminated condition - assume true branch
                    fprintf(stderr, "DEBUG: Code_IF condition var has no llvm_value, using constant true\n");
                    cond_llvm_val = llvm::ConstantInt::getTrue(*TheContext);
                }
                fprintf(stderr, "DEBUG: Code_IF got llvm_val=%p, type=%s\n",
                        (void*)cond_llvm_val, cond_llvm_val->getType()->isIntegerTy() ? "int" : "other");
                cond_llvm_val->print(llvm::errs());
                fprintf(stderr, "\n");
                fflush(stderr);

                // Ensure condition is i1
                if (cond_llvm_val->getType() != llvm::Type::getInt1Ty(*TheContext)) {
                    cond_llvm_val = Builder->CreateICmpNE(
                        cond_llvm_val, llvm::Constant::getNullValue(cond_llvm_val->getType()), "ifcond.tobool");
                }

                llvm::BasicBlock *true_bb = nullptr;
                llvm::BasicBlock *false_bb = nullptr;
                if (pn->code->label[0]) {
                    true_bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                    fprintf(stderr, "DEBUG: Code_IF true branch target: label_%d\n", pn->code->label[0]->id);
                }
                if (pn->code->label[1]) {
                    false_bb = getLLVMBasicBlock(pn->code->label[1], llvm_func);
                    fprintf(stderr, "DEBUG: Code_IF false branch target: label_%d\n", pn->code->label[1]->id);
                }

                if (!true_bb || !false_bb) { fail("Code_IF targets missing"); return; }

                // Handle constant-folded conditions (from dead code elimination)
                if (llvm::ConstantInt *const_cond = llvm::dyn_cast<llvm::ConstantInt>(cond_llvm_val)) {
                    // Branch to taken target only (matches C backend handling of if(0) / if(1))
                    bool cond_value = const_cond->isOne();
                    llvm::BasicBlock *target_bb = cond_value ? true_bb : false_bb;
                    fprintf(stderr, "DEBUG: Code_IF has constant condition: %s, branching to %s\n",
                            cond_value ? "true" : "false",
                            target_bb->getName().str().c_str());

                    Builder->CreateBr(target_bb);
                    Builder->SetInsertPoint(target_bb);
                } else {
                    // Dynamic condition - generate both branches
                    llvm::BasicBlock *if_true_bb = llvm::BasicBlock::Create(*TheContext, "if.true", llvm_func);
                    llvm::BasicBlock *if_false_bb = llvm::BasicBlock::Create(*TheContext, "if.false", llvm_func);

                    Builder->CreateCondBr(cond_llvm_val, if_true_bb, if_false_bb);

                    Builder->SetInsertPoint(if_true_bb);
                    do_phy_nodes(pn, 0, ifa_fun);
                    do_phi_nodes(pn, 0, ifa_fun);
                    Builder->CreateBr(true_bb);

                    Builder->SetInsertPoint(if_false_bb);
                    do_phy_nodes(pn, 1, ifa_fun);
                    do_phi_nodes(pn, 1, ifa_fun);
                    Builder->CreateBr(false_bb);

                    Builder->SetInsertPoint(true_bb);
                }
            } else {
                fail("Code_IF PNode has no condition variable");
            }
            break;
        }
        case Code_SEND: {
            fprintf(stderr, "DEBUG: Processing Code_SEND. prim=%p\n", pn->prim);
            fflush(stderr);
            if (pn->prim) {
                if (!write_llvm_prim(ifa_fun, pn)) {
                    fprintf(stderr, "DEBUG: write_llvm_prim returned false, calling write_send as fallback\n");
                    // Primitive not handled, try generic function call
                    write_send(ifa_fun, pn);
                }
            } else {
                fprintf(stderr, "DEBUG: Calling write_send\n");
                write_send(ifa_fun, pn);
            }
            break;
        }
        default:
            fprintf(stderr, "Unhandled PNode code kind: %d in function %s\n", pn->code->kind, ifa_fun->sym->name);
            break;
    }
}

