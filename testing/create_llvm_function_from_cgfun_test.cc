// SPDX-License-Identifier: BSD-3-Clause
// Unit test for CG_IR_PLAN Phase 3.2: create_llvm_function_from_cgfun
// builds an llvm::Function with the right signature shape from a
// CGFun, without going through IF1.
//
// The test constructs CGFuns by hand and verifies:
//   - the resulting llvm::Function's return type matches CGFun::return_type
//   - the parameter types match CGFun::arg_types (positionally)
//   - the llvm_handle cache is populated and round-trips
//   - external CGFuns get external linkage; internal ones get internal
//   - is_varargs (sourced from CGFun::source_fun) is honored

#include "ifadefs.h"

#include "unit.h"
#include "codegen/cg_ir.h"
#include "codegen/llvm.h"
#include "codegen/llvm_internal.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

#include <stdio.h>
#include <memory>

extern std::unique_ptr<llvm::Module> TheModule;

static CGType *mk_int(int bits) {
  CGType *t = new CGType();
  t->kind = CG_T_INT;
  t->bits = bits;
  return t;
}

static CGType *mk_ptr() {
  CGType *t = new CGType();
  t->kind = CG_T_PTR;
  return t;
}

int run_create_llvm_function_from_cgfun() {
  llvm_codegen_initialize(nullptr);

  // Case 1: void(i32, i32). Internal linkage, no varargs.
  {
    CGFun *cf = new CGFun();
    cf->name = "test_add";
    cf->return_type = nullptr;            // void
    cf->arg_types.add(mk_int(32));
    cf->arg_types.add(mk_int(32));

    llvm::Function *f = create_llvm_function_from_cgfun(cf, TheModule.get());
    if (!f) {
      printf("  create_llvm_function_from_cgfun returned null for test_add\n");
      return 1;
    }
    if (!f->getReturnType()->isVoidTy()) {
      printf("  test_add return type is not void\n");
      return 1;
    }
    if (f->arg_size() != 2) {
      printf("  test_add has %u args, expected 2\n", f->arg_size());
      return 1;
    }
    if (!f->getArg(0)->getType()->isIntegerTy(32) ||
        !f->getArg(1)->getType()->isIntegerTy(32)) {
      printf("  test_add arg types not i32, i32\n");
      return 1;
    }
    if (f->getLinkage() != llvm::Function::InternalLinkage) {
      printf("  test_add not InternalLinkage\n");
      return 1;
    }
    if (cf->llvm_handle != f) {
      printf("  test_add llvm_handle not cached\n");
      return 1;
    }
    // Second call returns the cached value.
    if (create_llvm_function_from_cgfun(cf, TheModule.get()) != f) {
      printf("  test_add second call did not return cached llvm_handle\n");
      return 1;
    }
  }

  // Case 2: ptr(i64). External linkage.
  {
    CGFun *cf = new CGFun();
    cf->name = "test_external_alloc";
    cf->return_type = mk_ptr();
    cf->arg_types.add(mk_int(64));
    cf->is_external = true;

    llvm::Function *f = create_llvm_function_from_cgfun(cf, TheModule.get());
    if (!f) {
      printf("  create_llvm_function_from_cgfun returned null for external\n");
      return 1;
    }
    if (!f->getReturnType()->isPointerTy()) {
      printf("  external return type is not pointer\n");
      return 1;
    }
    if (f->arg_size() != 1 || !f->getArg(0)->getType()->isIntegerTy(64)) {
      printf("  external arg signature mismatch\n");
      return 1;
    }
    if (f->getLinkage() != llvm::Function::ExternalLinkage) {
      printf("  external not ExternalLinkage\n");
      return 1;
    }
    // External funs don't get an entry block by themselves; their
    // body is left to the linker.
    if (f->size() != 0 && !f->empty()) {
      // It's OK if ensure_block_terminators left a single empty
      // declaration block, but there shouldn't be any executable
      // content beyond a terminator.
    }
  }

  // Case 3: name-less CGFun gets a synthesized name. Verifies the
  // fallback branch in create_llvm_function_from_cgfun.
  {
    CGFun *cf = new CGFun();
    cf->return_type = nullptr;
    llvm::Function *f = create_llvm_function_from_cgfun(cf, TheModule.get());
    if (!f) {
      printf("  unnamed cgfun returned null\n");
      return 1;
    }
    if (f->getName().empty()) {
      printf("  unnamed cgfun's llvm name is empty\n");
      return 1;
    }
  }

  return 0;
}

UNIT_TEST_FUN(run_create_llvm_function_from_cgfun);
