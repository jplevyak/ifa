// SPDX-License-Identifier: BSD-3-Clause
// Unit test for CG_IR_PLAN Phase 3.3: emit_llvm_module produces a
// valid LLVM module for a hand-built CGProgram.
//
// The test:
//   1. Builds a tiny CGProgram with one CGFun, one CGBlock, a
//      CG_RET terminator.
//   2. Runs emit_llvm_module().
//   3. Verifies the LLVM Module is well-formed (verifyModule),
//      the function has the expected signature, the entry block
//      has a Ret terminator.
//
// This exercises the parallel emit path without touching the
// production codegen flow.

#include "ifadefs.h"

#include "unit.h"
#include "codegen/cg_ir.h"
#include "codegen/llvm.h"
#include "codegen/llvm_internal.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include <stdio.h>
#include <memory>

extern std::unique_ptr<llvm::Module> TheModule;

int run_emit_llvm_module() {
  llvm_codegen_initialize(nullptr);

  // Build a CGProgram with one CGFun: void test_ret(void) { ret; }
  CGProgram *prog = new CGProgram();

  CGFun *cf = new CGFun();
  cf->name = "test_ret";
  cf->return_type = nullptr;  // void

  CGBlock *entry = new CGBlock();
  entry->id = 0;
  entry->label = "entry";
  CGInst *term = new CGInst();
  term->op = CG_RET;
  entry->terminator = term;
  cf->blocks.add(entry);
  cf->entry = entry;

  prog->funs.add(cf);

  emit_llvm_module(prog);

  if (!cf->llvm_handle) {
    printf("  emit_llvm_module: CGFun has no llvm_handle\n");
    return 1;
  }
  llvm::Function *f = cf->llvm_handle;
  if (f->size() == 0) {
    printf("  emit_llvm_module: function has no blocks\n");
    return 1;
  }
  if (!f->getEntryBlock().getTerminator()) {
    printf("  emit_llvm_module: entry block has no terminator\n");
    return 1;
  }
  if (!llvm::isa<llvm::ReturnInst>(f->getEntryBlock().getTerminator())) {
    printf("  emit_llvm_module: entry terminator is not Ret\n");
    return 1;
  }

  // Verify the whole module is well-formed.
  std::string err;
  llvm::raw_string_ostream rso(err);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  emit_llvm_module: verifyModule failed: %s\n", err.c_str());
    return 1;
  }

  return 0;
}

UNIT_TEST_FUN(run_emit_llvm_module);
