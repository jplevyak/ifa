#include "llvm_internal.h"
#include "cg_ir.h"
#include "codegen_common.h"
#include "fa.h"
#include "prim.h"
#include "var.h"
#include <set>

extern CGProgram *current_cg_program;

// ============================================================================
// Defensive helpers
// ============================================================================

// Walk every BasicBlock in `llvm_func` and add a default terminator to any
// block that doesn't have one. The terminator matches the function's
// declared return type — `ret void` for void-returning, `ret undef` for
// value-returning. Used at the end of `createFunction` (for external
// functions whose bodies aren't translated) and at the end of
// `translateFunctionBody` (catching any block the worklist left
// unterminated). See CODEGEN_PLAN §7.4 / AUDIT §4 — the two callsites
// previously inlined this same logic.
static void ensure_block_terminators(llvm::Function *llvm_func, cchar *origin) {
  llvm::Type *ret_ty = llvm_func->getReturnType();
  for (llvm::BasicBlock &BB : *llvm_func) {
    if (BB.getTerminator()) continue;
    DEBUG_LOG("ensure_block_terminators (%s): block %s in function %s has no terminator; adding default\n",
              origin, BB.getName().str().c_str(), llvm_func->getName().str().c_str());
    llvm::IRBuilder<> temp_builder(*TheContext);
    temp_builder.SetInsertPoint(&BB, BB.end());
    if (ret_ty->isVoidTy()) {
      temp_builder.CreateRetVoid();
    } else {
      temp_builder.CreateRet(llvm::UndefValue::get(ret_ty));
    }
    if (!BB.getTerminator()) {
      fail("ensure_block_terminators (%s): failed to add terminator to %s in %s", origin,
           BB.getName().str().c_str(), llvm_func->getName().str().c_str());
    }
  }
}

// ============================================================================
// Basic Block Management
// ============================================================================

std::map<Label *, llvm::BasicBlock *> label_to_bb_map;  // Global map for current function

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
  label->bb = bb;  // Store in Label struct as well, consistent with code.h
  return bb;
}

// Forward declare
static void translatePNode(PNode *pn, Fun *ifa_fun);
// ============================================================================
// createFunction helpers (phase-4 decomposition)
//
// createFunction used to be ~290 lines doing five jobs:
//   1. Determine the LLVM return type.
//   2. Build the live-args list and corresponding LLVM arg types.
//   3. Construct the llvm::Function declaration + entry BasicBlock.
//   4. Attach DISubprogram and DIParameterVariable debug info.
//   5. Ensure terminators on external / no-entry functions.
// Each step is now its own helper. The driver sequences them.
// ============================================================================

static llvm::Type *determine_return_type(Fun *ifa_fun) {
  if (ifa_fun->rets.n == 0) return llvm::Type::getVoidTy(*TheContext);
  if (ifa_fun->rets.n == 1 && ifa_fun->rets[0]) {
    Var *ret_var = ifa_fun->rets[0];
    if (!ret_var->type || !cg_get_string(ret_var->type)) return llvm::Type::getVoidTy(*TheContext);
    // getLLVMVarType — pyc returns heap aggregates by pointer.
    return getLLVMVarType(ret_var->type);
  }
  fail("createFunction: function %s has %d return values, unsupported (only 0 or 1 handled)",
       ifa_fun->sym->name ? ifa_fun->sym->name : "(anon)", ifa_fun->rets.n);
  return llvm::Type::getVoidTy(*TheContext);
}

// Determine whether `arg_var` counts as "live" for codegen purposes.
// An argument is live if its `live` bit is set OR any of its abstract
// vars is live (FA-level liveness). Function-typed formals are
// excluded — they're handled via the dispatcher path, not as direct
// parameters.
static bool arg_is_live_for_codegen(Var *arg_var) {
  if (!arg_var) return false;
  if (arg_var->live) return true;
  form_AVarMapElem(x, arg_var->avars) {
    if (x->value && x->value->live) return true;
  }
  return false;
}

static void build_arg_list(Fun *ifa_fun, std::vector<Var *> &live_args,
                           std::vector<llvm::Type *> &llvm_arg_types) {
  MPosition p;
  p.push(1);
  for (int i = 0; i < ifa_fun->sym->has.n; i++) {
    MPosition *cp = cannonicalize_mposition(p);
    p.inc();
    Var *arg_var = ifa_fun->args.get(cp);
    if (!arg_is_live_for_codegen(arg_var)) continue;
    if (arg_var->type && arg_var->type->is_fun) continue;  // Skip function-typed formals.
    live_args.push_back(arg_var);
  }
  for (Var *arg_var : live_args) {
    // getLLVMVarType handles the struct→pointer convention uniformly
    // (Type_RECORD, Type_FUN closures, Type_REF — all become `ptr`).
    llvm::Type *arg_llvm_type =
        arg_var->type ? getLLVMVarType(arg_var->type) : llvm::Type::getInt64Ty(*TheContext);
    if (!arg_llvm_type) {
      fail("createFunction: could not get LLVM type for argument %s of function %s",
           arg_var->sym && arg_var->sym->name ? arg_var->sym->name : "(anon)",
           ifa_fun->sym->name ? ifa_fun->sym->name : "(anon)");
      return;
    }
    llvm_arg_types.push_back(arg_llvm_type);
  }
}

static llvm::Function *create_llvm_function_decl(Fun *ifa_fun, llvm::Module *module,
                                                  llvm::Type *llvm_ret_type,
                                                  const std::vector<llvm::Type *> &llvm_arg_types,
                                                  const std::vector<Var *> &live_args,
                                                  std::string &func_name_out) {
  llvm::FunctionType *func_type =
      llvm::FunctionType::get(llvm_ret_type, llvm_arg_types, ifa_fun->is_varargs);
  func_name_out = (ifa_fun->sym->name ? ifa_fun->sym->name : "func") + std::to_string(ifa_fun->id);
  if (cg_get_string(ifa_fun) && strncmp(cg_get_string(ifa_fun), "_CG_", 4) == 0) {
    func_name_out = cg_get_string(ifa_fun);
  }
  llvm::Function::LinkageTypes linkage =
      ifa_fun->entry ? llvm::Function::InternalLinkage : llvm::Function::ExternalLinkage;
  llvm::Function *llvm_func = llvm::Function::Create(func_type, linkage, func_name_out, module);
  cg_set_llvm(ifa_fun, llvm_func);
  // Wire each live arg to its LLVM Argument.
  for (unsigned i = 0; i < live_args.size() && i < llvm_func->arg_size(); ++i) {
    llvm::Argument *llvm_arg = llvm_func->getArg(i);
    Var *arg_var = live_args[i];
    if (arg_var->sym && arg_var->sym->name) llvm_arg->setName(arg_var->sym->name);
    cg_set_llvm_value(arg_var, llvm_arg);
  }
  if (!ifa_fun->is_external && ifa_fun->entry) {
    llvm::BasicBlock::Create(*TheContext, "entry", llvm_func);
  }
  return llvm_func;
}

static void attach_debug_info(Fun *ifa_fun, llvm::Function *llvm_func,
                              const std::vector<Var *> &live_args, const std::string &func_name) {
  if (!DBuilder || !UnitFile) return;
  unsigned line_num = 0;
  if (ifa_fun->sym && ifa_fun->sym->ast) {
    line_num = ifa_fun->sym->ast->source_line();
  } else if (ifa_fun->entry && ifa_fun->entry->code) {
    line_num = ifa_fun->entry->code->source_line();
  }
  llvm::SmallVector<llvm::Metadata *, 8> di_param_types;
  if (ifa_fun->rets.n == 1 && ifa_fun->rets[0] && ifa_fun->rets[0]->type) {
    di_param_types.push_back(getLLVMDIType(ifa_fun->rets[0]->type, UnitFile));
  } else {
    di_param_types.push_back(nullptr);
  }
  for (Var *arg_var : live_args) {
    di_param_types.push_back(
        arg_var && arg_var->type ? getLLVMDIType(arg_var->type, UnitFile) : nullptr);
  }
  llvm::DISubroutineType *di_func_type =
      DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(di_param_types));
  llvm::DISubprogram *sp = DBuilder->createFunction(UnitFile, func_name, llvm_func->getName(), UnitFile,
                                                    line_num, di_func_type, line_num,
                                                    llvm::DINode::FlagPrototyped,
                                                    llvm::DISubprogram::SPFlagDefinition);
  llvm_func->setSubprogram(sp);

  // DIParameter info for every source-level parameter (live or
  // optimized-out — optimized-out get the metadata but no dbg.value).
  MPosition p2;
  p2.push(1);
  for (int i = 0; i < ifa_fun->sym->has.n; i++) {
    MPosition *cp2 = cannonicalize_mposition(p2);
    p2.inc();
    Var *arg_var = ifa_fun->args.get(cp2);
    if (!arg_var || !arg_var->sym || !arg_var->type || arg_var->type->is_fun) continue;
    llvm::DIType *arg_di_type = getLLVMDIType(arg_var->type, UnitFile);
    if (!arg_di_type) continue;
    unsigned arg_line = arg_var->sym->source_line() ? arg_var->sym->source_line() : line_num;
    llvm::DILocalVariable *param_var = DBuilder->createParameterVariable(
        sp, arg_var->sym->name ? arg_var->sym->name : "arg", i + 1, UnitFile, arg_line, arg_di_type, true);
    cg_set_llvm_debug_var(arg_var, param_var);
  }
}

llvm::Function *createFunction(Fun *ifa_fun, llvm::Module *module) {
  if (!ifa_fun) {
    fail("createFunction: null Fun");
    return nullptr;
  }
  if (cg_get_llvm(ifa_fun)) return cg_get_llvm(ifa_fun);
  if (!ifa_fun->sym) {
    fail("createFunction: Fun %p has no associated symbol", (void *)ifa_fun);
    return nullptr;
  }
  if (!module) {
    fail("createFunction: module is null");
    return nullptr;
  }

  llvm::Type *llvm_ret_type = determine_return_type(ifa_fun);
  if (!llvm_ret_type) return nullptr;
  std::vector<llvm::Type *> llvm_arg_types;
  std::vector<Var *> live_args;
  build_arg_list(ifa_fun, live_args, llvm_arg_types);
  std::string func_name;
  llvm::Function *llvm_func =
      create_llvm_function_decl(ifa_fun, module, llvm_ret_type, llvm_arg_types, live_args, func_name);
  if (!llvm_func) return nullptr;
  attach_debug_info(ifa_fun, llvm_func, live_args, func_name);

  // External / no-entry functions skip translateFunctionBody, so their
  // (typically-empty) blocks need terminators here.
  if (ifa_fun->is_external || !ifa_fun->entry) {
    ensure_block_terminators(llvm_func, "createFunction");
  }
  return llvm_func;
}

// ============================================================================
// CG_IR_PLAN Phase 3.2 — direct CGFun → llvm::Function lowering.
//
// Parallel to `createFunction(Fun*, ...)`. Consumes a CGFun
// (CGProgram-owned) instead of dereferencing IF1. The function's
// name, return type, and arg types come straight from CGFun fields;
// no `cg_get_string(ifa_fun)`, no `getLLVMVarType(arg->type)` walks.
//
// Until Phase 3.3 wires the LLVM backend over to CGProgram, this
// function is unused at production codegen time. The unit test
// (`run_create_llvm_function_from_cgfun`) verifies the contract:
// signature shape, linkage, llvm_handle caching on CGFun.
//
// Debug info (DISubprogram, DIParameterVariable) is NOT attached
// here — that needs the IF1 source-line plumbing, which Phase 3.3
// re-introduces by carrying source-pn / source-fun on CGInst /
// CGFun. The parallel landing keeps debug info out of scope to
// limit the diff; Phase 3.3 fills it back in.
// ============================================================================

llvm::Function *create_llvm_function_from_cgfun(CGFun *cf, llvm::Module *module) {
  if (!cf) {
    fail("create_llvm_function_from_cgfun: null CGFun");
    return nullptr;
  }
  if (cf->llvm_handle) return cf->llvm_handle;
  if (!module) {
    fail("create_llvm_function_from_cgfun: null Module");
    return nullptr;
  }

  llvm::Type *llvm_ret_type = cg_to_llvm_type(cf->return_type);
  if (!llvm_ret_type) llvm_ret_type = llvm::Type::getVoidTy(*TheContext);

  std::vector<llvm::Type *> llvm_arg_types;
  for (CGType *at : cf->arg_types) {
    llvm::Type *t = cg_to_llvm_type(at);
    if (!t) t = llvm::Type::getInt64Ty(*TheContext);
    llvm_arg_types.push_back(t);
  }

  bool is_varargs = cf->source_fun ? cf->source_fun->is_varargs : false;
  llvm::FunctionType *func_type =
      llvm::FunctionType::get(llvm_ret_type, llvm_arg_types, is_varargs);

  std::string func_name = cf->name ? std::string(cf->name)
                                   : ("cgfun." + std::to_string(reinterpret_cast<uintptr_t>(cf)));

  // External CGFuns get external linkage; everything else gets
  // internal linkage matching the existing C-side default.
  llvm::Function::LinkageTypes linkage =
      cf->is_external ? llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage;

  llvm::Function *llvm_func =
      llvm::Function::Create(func_type, linkage, func_name, module);
  cf->llvm_handle = llvm_func;

  // Wire each formal-arg slot (when present) to its LLVM Argument.
  // Phase 2's CGFun has `formal_arg_slots.n == 0` because the
  // initial cut didn't populate them; Phase 3.3 adds the slot
  // population to cg_normalize. This loop is a no-op until then.
  for (unsigned i = 0; i < cf->formal_arg_slots.n && i < llvm_func->arg_size(); ++i) {
    llvm::Argument *llvm_arg = llvm_func->getArg(i);
    CGSlot *fs = cf->formal_arg_slots[i];
    if (fs && fs->name) llvm_arg->setName(fs->name);
    if (fs) fs->llvm_handle = llvm_arg;
  }

  // For internal-linkage non-external functions, create an entry
  // BasicBlock so the function is well-formed even when called
  // before translateFunctionBody runs.
  if (!cf->is_external) {
    llvm::BasicBlock::Create(*TheContext, "entry", llvm_func);
  } else {
    ensure_block_terminators(llvm_func, "create_llvm_function_from_cgfun");
  }
  return llvm_func;
}

// --- PNode Translation ---
// ============================================================================
// translateFunctionBody helpers (phase-4 decomposition)
//
// translateFunctionBody used to be ~250 lines doing four jobs:
//   1. Walk PNodes pre-creating BasicBlocks for every LABEL.
//   2. Allocate stack slots (AllocaInst) for each local Var.
//   3. Emit debug info for function parameters.
//   4. Run the CFG-walk worklist that calls translatePNode per PNode.
// Each step is now its own helper. The driver `translateFunctionBody`
// just sequences them.
// ============================================================================

// Step 1: pre-create LLVM BasicBlocks for every Code_LABEL PNode and
// wire up the function's auto-created entry block to branch to the
// IF1 entry if needed.
static void prepare_basic_blocks(Fun *ifa_fun) {
  llvm::Function *llvm_func = cg_get_llvm(ifa_fun);
  Vec<PNode *> pnodes;
  ifa_fun->collect_PNodes(pnodes);
  if (ifa_fun->fa_all_Vars.n == 0) ifa_fun->collect_Vars(ifa_fun->fa_all_Vars);
  for (PNode *pn : pnodes) {
    if (pn->code && pn->code->kind == Code_LABEL && pn->code->label[0]) {
      getLLVMBasicBlock(pn->code->label[0], llvm_func);
    }
  }
  // If the IF1 entry is a Code_LABEL, branch from LLVM's auto-created
  // entry block to it.
  if (ifa_fun->entry->code && ifa_fun->entry->code->kind == Code_LABEL && ifa_fun->entry->code->label[0]) {
    llvm::BasicBlock *entry_bb = getLLVMBasicBlock(ifa_fun->entry->code->label[0], llvm_func);
    if (entry_bb != &llvm_func->getEntryBlock()) {
      Builder->SetInsertPoint(&llvm_func->getEntryBlock());
      if (llvm_func->getEntryBlock().getTerminator() == nullptr) Builder->CreateBr(entry_bb);
    }
  }
}

// Step 2: allocate stack slots for local variables (one AllocaInst per
// unique Sym), with optional debug-info DI variable declarations.
static void allocate_locals(Fun *ifa_fun) {
  llvm::Function *llvm_func = cg_get_llvm(ifa_fun);
  Builder->SetInsertPoint(&llvm_func->getEntryBlock(), llvm_func->getEntryBlock().begin());
  // Clear debug location before allocating locals to avoid wrong
  // subprogram references on the alloca instructions themselves.
  Builder->SetCurrentDebugLocation(llvm::DebugLoc());

  llvm::DIFile *di_file_for_locals = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getFile() : nullptr;
  unsigned func_start_line = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getLine() : 0;

  // Multiple Vars can share the same Sym (e.g., loop variable `i`
  // appears multiple times in the SSU form). Dedup by Sym so we emit
  // exactly one alloca per source variable.
  std::map<Sym *, llvm::AllocaInst *> sym_to_alloca;
  for (Var *v : ifa_fun->fa_all_Vars) {
    if (!v || !v->sym || !v->sym->is_local || v->is_formal) continue;

    // Reuse the existing alloca if this Sym has already been seen.
    auto it = sym_to_alloca.find(v->sym);
    if (it != sym_to_alloca.end()) {
      cg_set_llvm_value(v, it->second);
      cg_set_llvm_type(v, it->second->getAllocatedType());
      continue;
    }
    if (cg_get_llvm_value(v)) continue;

    // getLLVMVarType: heap aggregates become `ptr`, so the local slot
    // holds the GC_malloc'd pointer instead of being sized as the
    // struct itself.
    llvm::Type *var_llvm_type = v->type ? getLLVMVarType(v->type) : llvm::Type::getInt64Ty(*TheContext);
    if (!var_llvm_type) {
      fail("allocate_locals: could not get LLVM type for var %s", v->sym->name);
      continue;
    }
    if (var_llvm_type->isVoidTy()) continue;

    llvm::AllocaInst *alloca_inst =
        Builder->CreateAlloca(var_llvm_type, nullptr, v->sym->name ? v->sym->name : "local_var");
    cg_set_llvm_value(v, alloca_inst);
    cg_set_llvm_type(v, var_llvm_type);
    sym_to_alloca[v->sym] = alloca_inst;

    if (DBuilder && llvm_func->getSubprogram() && di_file_for_locals) {
      llvm::DIType *var_di_type = getLLVMDIType(v->type, di_file_for_locals);
      unsigned var_line = v->sym->source_line() ? v->sym->source_line() : func_start_line;
      if (var_di_type) {
        llvm::DILocalVariable *dil_var = DBuilder->createAutoVariable(
            llvm_func->getSubprogram(), v->sym->name ? v->sym->name : "var", UnitFile, var_line,
            getLLVMDIType(v->type, UnitFile));
        DBuilder->insertDeclare(alloca_inst, dil_var, DBuilder->createExpression(),
                                llvm::DILocation::get(*TheContext, var_line, 0, llvm_func->getSubprogram()),
                                Builder->GetInsertBlock());
      }
    }
  }
}

// Step 3: emit dbg.value intrinsics for every formal argument. Must
// run after `allocate_locals` (the dbg.value insertions land in the
// entry block at the current builder position).
static void emit_parameter_debug_info(Fun *ifa_fun) {
  if (!DBuilder) return;
  llvm::Function *llvm_func = cg_get_llvm(ifa_fun);
  llvm::DISubprogram *sp = llvm_func->getSubprogram();
  if (!sp) return;
  unsigned func_start_line = sp->getLine();

  MPosition p;
  p.push(1);
  for (int i = 0; i < ifa_fun->sym->has.n; i++) {
    MPosition *cp = cannonicalize_mposition(p);
    p.inc();
    Var *arg_var = ifa_fun->args.get(cp);
    if (!arg_var || !cg_get_llvm_debug_var(arg_var)) continue;
    unsigned arg_line = arg_var->sym->source_line() ? arg_var->sym->source_line() : func_start_line;
    llvm::DILocation *debug_loc = llvm::DILocation::get(*TheContext, arg_line, 0, sp);
    if (cg_get_llvm_value(arg_var)) {
      DBuilder->insertDbgValueIntrinsic(cg_get_llvm_value(arg_var), cg_get_llvm_debug_var(arg_var),
                                        DBuilder->createExpression(), debug_loc, Builder->GetInsertBlock());
    }
    // Optimized-out parameters intentionally emit no dbg.value — the
    // debugger displays them as "<optimized out>".
  }
}

// Step 4: run the CFG-walk worklist, dispatching each PNode to
// translatePNode. Successors are always added (even from non-live
// PNodes) to match the C backend's CFG traversal.
static void translate_pnodes_worklist(Fun *ifa_fun) {
  std::set<PNode *> visited_pnodes;
  std::vector<PNode *> worklist;
  worklist.push_back(ifa_fun->entry);
  visited_pnodes.insert(ifa_fun->entry);

  unsigned worklist_idx = 0;
  while (worklist_idx < worklist.size()) {
    PNode *current_pn = worklist[worklist_idx++];
    if (!current_pn) continue;
    translatePNode(current_pn, ifa_fun);
    for (PNode *succ_pn : current_pn->cfg_succ) {
      if (succ_pn && visited_pnodes.find(succ_pn) == visited_pnodes.end()) {
        worklist.push_back(succ_pn);
        visited_pnodes.insert(succ_pn);
      }
    }
  }
}

void translateFunctionBody(Fun *ifa_fun) {
  if (!ifa_fun || !cg_get_llvm(ifa_fun) || !ifa_fun->entry) {
    fail("translateFunctionBody: invalid Fun (missing llvm or entry)");
    return;
  }
  if (!Builder) {
    fail("translateFunctionBody: Builder is null");
    return;
  }
  label_to_bb_map.clear();
  prepare_basic_blocks(ifa_fun);
  allocate_locals(ifa_fun);
  emit_parameter_debug_info(ifa_fun);
  translate_pnodes_worklist(ifa_fun);
  ensure_block_terminators(cg_get_llvm(ifa_fun), "translateFunctionBody");
}

// translatePNode helpers, one per Code_kind. Phase-4 decomposition of
// the formerly-200-line translatePNode switch. Each helper is short
// and named for its kind so call traces and stack frames are
// self-explanatory.

static void translate_code_label(PNode *pn, Fun *ifa_fun) {
  if (!pn->code->label[0]) {
    fail("Code_LABEL PNode has no Label object");
    return;
  }
  llvm::BasicBlock *bb = getLLVMBasicBlock(pn->code->label[0], cg_get_llvm(ifa_fun));
  llvm::BasicBlock *current_bb = Builder->GetInsertBlock();
  // Avoid creating a self-loop branch (br label %X inside %X).
  if (current_bb && current_bb != bb && !current_bb->getTerminator()) {
    Builder->CreateBr(bb);
  }
  Builder->SetInsertPoint(bb);
}

static void do_phi_nodes(PNode *n, int isucc, Fun *ifa_fun);  // forward decl

static void translate_code_goto(PNode *pn, Fun *ifa_fun) {
  do_phi_nodes(pn, 0, ifa_fun);
  if (!pn->code->label[0]) {
    fail("Code_GOTO PNode has no destination Label object");
    return;
  }
  if (!Builder->GetInsertBlock()) {
    fail("Code_GOTO: Builder has no insert block (missing prior LABEL or entry setup)");
    return;
  }
  llvm::BasicBlock *dest_bb = getLLVMBasicBlock(pn->code->label[0], cg_get_llvm(ifa_fun));
  Builder->CreateBr(dest_bb);
}

void simple_move(Var *lhs, Var *rhs, Fun *ifa_fun);  // forward decl

static void translate_code_move(PNode *pn, Fun *ifa_fun) {
  if (pn->lvals.n != 1 || pn->rvals.n != 1) {
    fail("Code_MOVE PNode with unhandled number of lvals/rvals (%d/%d)", pn->lvals.n, pn->rvals.n);
    return;
  }
  simple_move(pn->lvals[0], pn->rvals[0], ifa_fun);
}

static void do_phy_nodes(PNode *n, int isucc, Fun *ifa_fun);  // forward decl

static void translate_code_if(PNode *pn, Fun *ifa_fun) {
  if (pn->rvals.n == 0) {
    fail("Code_IF PNode has no condition variable");
    return;
  }
  llvm::Function *llvm_func = cg_get_llvm(ifa_fun);
  Var *cond_var = pn->rvals[0];
  llvm::Value *cond_llvm_val = getLLVMValue(cond_var, ifa_fun);
  if (!cond_llvm_val) {
    // Dead-code-eliminated condition — assume true branch.
    DEBUG_LOG("Code_IF condition var has no llvm_value; using constant true\n");
    cond_llvm_val = llvm::ConstantInt::getTrue(*TheContext);
  }
  // Coerce condition to i1.
  if (cond_llvm_val->getType() != llvm::Type::getInt1Ty(*TheContext)) {
    cond_llvm_val = Builder->CreateICmpNE(
        cond_llvm_val, llvm::Constant::getNullValue(cond_llvm_val->getType()), "ifcond.tobool");
  }

  llvm::BasicBlock *true_bb = pn->code->label[0] ? getLLVMBasicBlock(pn->code->label[0], llvm_func) : nullptr;
  llvm::BasicBlock *false_bb = pn->code->label[1] ? getLLVMBasicBlock(pn->code->label[1], llvm_func) : nullptr;
  if (!true_bb || !false_bb) {
    fail("Code_IF targets missing");
    return;
  }

  // Constant-folded condition: branch to the taken target only.
  if (llvm::ConstantInt *const_cond = llvm::dyn_cast<llvm::ConstantInt>(cond_llvm_val)) {
    llvm::BasicBlock *target_bb = const_cond->isOne() ? true_bb : false_bb;
    Builder->CreateBr(target_bb);
    Builder->SetInsertPoint(target_bb);
    return;
  }

  // Dynamic condition: emit both branches with phi/phy node fixups in
  // an intermediate block per side.
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

static void translate_code_send(PNode *pn, Fun *ifa_fun) {
  if (pn->prim) {
    if (write_llvm_prim(ifa_fun, pn)) return;
    DEBUG_LOG("write_llvm_prim returned 0; falling back to write_send for prim %p\n", pn->prim);
  }
  write_send(ifa_fun, pn);
}

// Exposed for emit_cg.cc (CG_IR_PLAN Phase 3.3 Track 3).
void simple_move(Var *lhs, Var *rhs, Fun *ifa_fun) {
  // Early returns matching C backend's simple_move (cg.cc:497-501)
  if (!lhs->live) return;                                                  // Skip if LHS not live
  if (!rhs->type || !lhs->type) return;                                    // Skip if no types
  if (rhs->type == sym_void->type || lhs->type == sym_void->type) return;  // Skip void types

  llvm::Value *val = getLLVMValue(rhs, ifa_fun);
  if (!val) {
    // RHS doesn't have an LLVM value yet (might be dead code or forward reference)
    // C backend would skip this with: if (!cg_get_string(rhs)) return;
    return;
  }
  setLLVMValue(lhs, val, ifa_fun);
}

static void do_phy_nodes(PNode *n, int isucc, Fun *ifa_fun) {
  for (PNode *p : n->phy) {
    if (p->lvals.n > isucc && p->rvals.n > 0) {  // Safety check
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
      for (PNode *pp : succ->phi) {
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
    DEBUG_LOG("translatePNode: pn->code is NULL, skipping pn=%p\n", (void *)pn);
    return;
  }

  int code_kind = pn->code->kind;
  // Sanity check code_kind - valid values should be < 100
  if (code_kind < 0 || code_kind > 100) {
    fail("translatePNode: invalid code_kind %d for pn=%p", code_kind, (void *)pn);
    return;
  }

  // Liveness gate — `live && fa_live`, matching the C backend's
  // `write_c_pnode` (cg.cc:586). DCE-live AND FA-reachable; the
  // intersection is what survives codegen.
  //
  // The earlier comment said "trust fa_live alone, it's more
  // accurate than dead.cc's live flag." That advice was wrong —
  // running on `fa_live` alone made the LLVM backend skip body
  // PNodes inside for-loops (whose `live=1, fa_live=0` because the
  // IFA template version is shadowed by clones, but DCE rightly
  // keeps the surviving copy). The strict gate brought the
  // pyc-suite from 32 → 37 and codegen-llvm fixtures from 8 → 12.
  //
  // Known follow-up: iterator-style functions (e.g. range's
  // `__pyc_more__`) still misbehave because their SSU formal-arg →
  // renamed-local MOVEs are gated out, leaving the renamed `self`
  // uninitialized. The C backend masks this by running phi/phy
  // materialization OUTSIDE the live gate (cg.cc:648); the LLVM
  // backend's per-kind dispatch needs an analogous out-of-gate
  // pass. See ifa/issues/016-llvm-ssu-formal-arg-binding.md.
  bool is_live = pn->live && pn->fa_live;
  if (!is_live) {
    if (ifa_debug) {
      DEBUG_LOG("Skipping non-live PNode pn=%p (live=%d, fa_live=%d), code_kind=%d, lvals.n=%d", (void *)pn,
                pn->live, pn->fa_live, code_kind, pn->lvals.n);
      if (pn->lvals.n > 0 && pn->lvals[0]) {
        fprintf(stderr, ", lval[0] id=%d", pn->lvals[0]->id);
      }
      fprintf(stderr, ", but will process CFG successors\n");
    }
    // Skip the instruction translation but continue to CFG processing at the end
    return;  // For now, still return - we'll fix CFG traversal in worklist instead
  }

  DEBUG_LOG("translatePNode entry. pn=%p, code_kind=%d, line=%d\n", (void *)pn, code_kind,
          pn->code->line());
  fflush(stderr);
  if (!ifa_fun || !cg_get_llvm(ifa_fun)) {
    fail("Invalid ifa_fun or missing llvm_func for PNode translation");
    return;
  }
  llvm::Function *llvm_func = cg_get_llvm(ifa_fun);

  // Check if current block already has a terminator or if there's no insert point
  // (except for Code_LABEL which sets a new insert point)
  if (code_kind != Code_LABEL) {
    llvm::BasicBlock *current_bb = Builder->GetInsertBlock();
    if (!current_bb) {
      DEBUG_LOG("No insert point set, skipping pn=%p, code_kind=%d\n", (void *)pn, code_kind);
      return;
    }
    if (current_bb->getTerminator()) {
      DEBUG_LOG("Current block already has terminator, skipping pn=%p, code_kind=%d, block_name=%s\n",
              (void *)pn, code_kind, current_bb->getName().str().c_str());
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
    case Code_LABEL: translate_code_label(pn, ifa_fun); break;
    case Code_GOTO:  translate_code_goto(pn, ifa_fun);  break;
    case Code_MOVE:  translate_code_move(pn, ifa_fun);  break;
    case Code_IF:    translate_code_if(pn, ifa_fun);    break;
    case Code_SEND:  translate_code_send(pn, ifa_fun);  break;
    default:
      fail("translatePNode: unhandled code kind %d in function %s", pn->code->kind, ifa_fun->sym->name);
      break;
  }
}
