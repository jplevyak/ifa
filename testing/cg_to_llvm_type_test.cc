// SPDX-License-Identifier: BSD-3-Clause
// Unit test for CG_IR_PLAN Phase 3.1: cg_to_llvm_type(CGType*)
// lowers a CGType directly to llvm::Type without going through
// IF1's Sym → getLLVMType path.
//
// The test builds CGType instances by hand (no FA pipeline needed)
// for each CGTypeKind and verifies the resulting llvm::Type has
// the expected shape (width for INT/UINT/FLOAT, opaque pointer
// for PTR/FUN_PTR, structure layout for STRUCT).
//
// This test only exists once the LLVM Context is initialized; we
// initialize it lazily via `llvm_codegen_initialize` (the same
// hook the production codegen uses).

#include "ifadefs.h"

#include "unit.h"
#include "codegen/cg_ir.h"
#include "codegen/llvm.h"
#include "codegen/llvm_internal.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#include <stdio.h>

static int check_int_width(int bits, CGTypeKind kind, cchar *label) {
  CGType *t = new CGType();
  t->kind = kind;
  t->bits = bits;
  llvm::Type *lt = cg_to_llvm_type(t);
  if (!lt) {
    printf("  cg_to_llvm_type(%s, bits=%d) returned null\n", label, bits);
    return 1;
  }
  if (!lt->isIntegerTy(bits)) {
    printf("  cg_to_llvm_type(%s, bits=%d) returned wrong kind/width\n", label, bits);
    return 1;
  }
  if (t->llvm_handle != lt) {
    printf("  cg_to_llvm_type(%s, bits=%d) did not cache result\n", label, bits);
    return 1;
  }
  // Second call returns the same handle (cache hit).
  if (cg_to_llvm_type(t) != lt) {
    printf("  cg_to_llvm_type(%s, bits=%d) cache miss on second call\n", label, bits);
    return 1;
  }
  return 0;
}

int run_cg_to_llvm_type() {
  // Bring up the LLVM emission state (Context, Module, Builder).
  // Initializing with a null FA is safe — we only need the Context
  // for type construction; the type cache lives on CGType.
  llvm_codegen_initialize(nullptr);

  // VOID.
  {
    CGType *t = new CGType();
    t->kind = CG_T_VOID;
    llvm::Type *lt = cg_to_llvm_type(t);
    if (!lt || !lt->isVoidTy()) {
      printf("  CG_T_VOID did not lower to VoidTy\n");
      return 1;
    }
  }

  // BOOL.
  {
    CGType *t = new CGType();
    t->kind = CG_T_BOOL;
    llvm::Type *lt = cg_to_llvm_type(t);
    if (!lt || !lt->isIntegerTy(1)) {
      printf("  CG_T_BOOL did not lower to i1\n");
      return 1;
    }
  }

  // INT/UINT at every supported width.
  for (int bits : {1, 8, 16, 32, 64}) {
    if (check_int_width(bits, CG_T_INT, "CG_T_INT")) return 1;
    if (check_int_width(bits, CG_T_UINT, "CG_T_UINT")) return 1;
  }

  // FLOAT widths.
  for (int bits : {32, 64}) {
    CGType *t = new CGType();
    t->kind = CG_T_FLOAT;
    t->bits = bits;
    llvm::Type *lt = cg_to_llvm_type(t);
    if (!lt) {
      printf("  CG_T_FLOAT bits=%d returned null\n", bits);
      return 1;
    }
    bool ok = (bits == 32) ? lt->isFloatTy() : lt->isDoubleTy();
    if (!ok) {
      printf("  CG_T_FLOAT bits=%d wrong kind\n", bits);
      return 1;
    }
  }

  // PTR and FUN_PTR — opaque pointers, identical shape.
  for (CGTypeKind k : {CG_T_PTR, CG_T_FUN_PTR}) {
    CGType *t = new CGType();
    t->kind = k;
    llvm::Type *lt = cg_to_llvm_type(t);
    if (!lt || !lt->isPointerTy()) {
      printf("  pointer kind %d did not lower to pointer\n", (int)k);
      return 1;
    }
  }

  // STRUCT with two i32 fields.
  {
    CGType *i32 = new CGType();
    i32->kind = CG_T_INT;
    i32->bits = 32;
    CGType *s = new CGType();
    s->kind = CG_T_STRUCT;
    s->name = "test_struct";
    s->fields.add(i32);
    s->fields.add(i32);
    s->field_names.add("a");
    s->field_names.add("b");
    llvm::Type *lt = cg_to_llvm_type(s);
    if (!lt || !lt->isStructTy()) {
      printf("  CG_T_STRUCT did not lower to struct\n");
      return 1;
    }
    llvm::StructType *st = llvm::cast<llvm::StructType>(lt);
    if (st->getNumElements() != 2) {
      printf("  struct has %d elements, expected 2\n", st->getNumElements());
      return 1;
    }
    if (!st->getElementType(0)->isIntegerTy(32) ||
        !st->getElementType(1)->isIntegerTy(32)) {
      printf("  struct field types incorrect\n");
      return 1;
    }
  }

  // Null CGType: should return VoidTy as a safe fallback.
  {
    llvm::Type *lt = cg_to_llvm_type(nullptr);
    if (!lt || !lt->isVoidTy()) {
      printf("  cg_to_llvm_type(nullptr) did not return VoidTy\n");
      return 1;
    }
  }

  return 0;
}

UNIT_TEST_FUN(run_cg_to_llvm_type);
