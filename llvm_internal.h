/* -*-Mode: c++;-*-
   Copyright (c) 2023 John Plevyak, All Rights Reserved
*/
#ifndef _llvm_internal_H_
#define _llvm_internal_H_

#include "ifadefs.h"
#include "fun.h"
#include "fa.h"
#include "pattern.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"
#include <memory>
#include <map>
#include <vector>

// Global LLVM state shared across all translation units
extern std::unique_ptr<llvm::LLVMContext> TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::unique_ptr<llvm::IRBuilder<>> Builder;
extern std::unique_ptr<llvm::DIBuilder> DBuilder;
extern llvm::DICompileUnit *CU;
extern llvm::DIFile *UnitFile;
extern Vec<Fun*> *all_funs_global;

// Helper macros
#ifndef forv_MPosition
#define forv_MPosition(_p, _v) forv_Vec(MPosition, _p, _v)
#endif

// ============================================================================
// Type System (llvm.cc)
// ============================================================================

// Convert IF1 type to LLVM type
llvm::Type *getLLVMType(Sym *sym);

// Convert IF1 type to LLVM debug info type
llvm::DIType *getLLVMDIType(Sym *sym, llvm::DIFile *di_file);

// Build C-style type strings for all symbols (needed before type translation)
void llvm_build_type_strings(FA *fa);

// ============================================================================
// Value/Constant Management (llvm.cc)
// ============================================================================

// Get or create LLVM value for a Var
llvm::Value *getLLVMValue(Var *var, Fun *ifa_fun);

// Set LLVM value for a Var (handles both SSA and memory-based vars)
void setLLVMValue(Var *var, llvm::Value *val, Fun *ifa_fun);

// Get LLVM constant for a compile-time constant Var
llvm::Constant *getLLVMConstant(Var *var);

// ============================================================================
// Function Translation (llvm_codegen.cc)
// ============================================================================

// Create LLVM function declaration (signature only)
llvm::Function *createFunction(Fun *ifa_fun, llvm::Module *module);

// Translate function body (PNodes) to LLVM IR
void translateFunctionBody(Fun *ifa_fun);

// Get or create LLVM BasicBlock for an IF1 Label
llvm::BasicBlock *getLLVMBasicBlock(Label *label, llvm::Function *func);

// Label to BasicBlock mapping (per-function, managed by llvm_codegen.cc)
extern std::map<Label *, llvm::BasicBlock *> label_to_bb_map;

// ============================================================================
// Primitive Operations (llvm_primitives.cc)
// ============================================================================

// Translate primitive operation (arithmetic, comparisons, tuple ops, etc.)
int write_llvm_prim(Fun *ifa_fun, PNode *n);

// Translate function call
void write_send(Fun *f, PNode *n);

// ============================================================================
// Utilities
// ============================================================================

// Check if a Var represents a closure
int is_closure_var(Var *v);

// Get target function from a call PNode
Fun *get_target_fun(PNode *n, Fun *f);

#endif // _llvm_internal_H_
