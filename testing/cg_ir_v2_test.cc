// SPDX-License-Identifier: BSD-3-Clause
//
// cg_ir_v2_test.cc — round-trip tests for the CG_IR_v2 parser
// + printer. CG_IR_PLAN Phase 4, commit 1.
//
// Each test:
//   1. Parses a known-good textual program.
//   2. Verifies semantic structure (fun present, terminator
//      kind correct, etc).
//   3. Prints back to text.
//   4. Re-parses the printed text.
//   5. Verifies the re-parsed program matches structurally.
//
// This validates parse(print(prog)) ≡ prog (semantically) and
// covers the round-trip property from CG_IR_TEXT.md.

#include "ifadefs.h"

#include "codegen/cg_ir_v2.h"
#include "codegen/llvm.h"
#include "codegen/llvm_internal.h"
#include "unit.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include <memory>
#include <stdio.h>
#include <string.h>

extern std::unique_ptr<llvm::Module> TheModule;

namespace {

// Compact structural fingerprint used to compare two CGv2Programs
// semantically. Two programs with the same fingerprint have the
// same fun count, fun names, signatures, block counts, and
// terminator kinds. Sufficient for round-trip checks of the
// early test corpus.
//
// Returned string is GC-allocated; safe to discard.
cchar *fingerprint(CGv2Program *p) {
  char buf[2048];
  int off = 0;
  off += snprintf(buf + off, sizeof(buf) - off, "funs=%d ",
                  p->funs.n);
  for (CGv2Fun *f : p->funs) {
    off += snprintf(buf + off, sizeof(buf) - off,
                    "[%s blocks=%d sig.ret=%s main=%d",
                    f->name ? f->name : "?",
                    f->blocks.n,
                    f->signature && f->signature->ret
                        ? f->signature->ret->name : "?",
                    f->is_main ? 1 : 0);
    for (CGv2Block *b : f->blocks) {
      off += snprintf(buf + off, sizeof(buf) - off,
                      " {%s term=%d}",
                      b->name ? b->name : "?",
                      b->terminator ? (int)b->terminator->op : -1);
    }
    off += snprintf(buf + off, sizeof(buf) - off, "]");
  }
  return dupstr(buf);
}

bool run_one(cchar *label, cchar *text, int expected_funs,
             cchar *expected_first_fun_name) {
  cchar *err = 0;
  CGv2Program *p = cg_v2_parse(text, &err);
  if (!p) {
    printf("  %s: parse failed: %s\n", label, err ? err : "(no msg)");
    return false;
  }
  if (p->funs.n != expected_funs) {
    printf("  %s: expected %d funs, got %d\n", label,
           expected_funs, p->funs.n);
    return false;
  }
  if (expected_first_fun_name && expected_funs > 0) {
    CGv2Fun *f = p->funs[0];
    if (!f->name || strcmp(f->name, expected_first_fun_name) != 0) {
      printf("  %s: expected first fun name '%s', got '%s'\n",
             label, expected_first_fun_name,
             f->name ? f->name : "(null)");
      return false;
    }
  }

  cchar *fp1 = fingerprint(p);

  // Round-trip: print, re-parse, fingerprint, compare.
  cchar *text2 = cg_v2_print(p);
  if (!text2) {
    printf("  %s: print returned null\n", label);
    return false;
  }
  cchar *err2 = 0;
  CGv2Program *p2 = cg_v2_parse(text2, &err2);
  if (!p2) {
    printf("  %s: re-parse of printed text failed: %s\n  text:\n%s\n",
           label, err2 ? err2 : "(no msg)", text2);
    return false;
  }
  cchar *fp2 = fingerprint(p2);
  if (strcmp(fp1, fp2) != 0) {
    printf("  %s: round-trip fingerprint mismatch\n", label);
    printf("    original: %s\n", fp1);
    printf("    after:    %s\n", fp2);
    return false;
  }
  return true;
}

}  // namespace

int run_cg_ir_v2_roundtrip() {
  // Test 01 — empty void fn.
  cchar *test01 =
      "((fun %hi\n"
      "   :signature (void)\n"
      "   :entry %b0\n"
      "   (block %b0\n"
      "     :term (ret))))";
  if (!run_one("test01", test01, 1, "hi")) return 1;

  // Test 01 variant: void fn with two blocks, first branches
  // to the second which returns. Exercises CG2_BR terminator
  // and the multi-block path.
  cchar *test01_br =
      "((fun %two\n"
      "   :signature (void)\n"
      "   :entry %b0\n"
      "   (block %b0\n"
      "     :term (br %b1))\n"
      "   (block %b1\n"
      "     :preds (%b0)\n"
      "     :term (ret))))";
  if (!run_one("test01_br", test01_br, 1, "two")) return 1;

  // Test 01 main marker.
  cchar *test01_main =
      "((fun %main\n"
      "   :signature (void)\n"
      "   :main true\n"
      "   :entry %b0\n"
      "   (block %b0\n"
      "     :term (ret))))";
  if (!run_one("test01_main", test01_main, 1, "main")) return 1;

  // Negative test: bad syntax (mismatched parens) should
  // produce an error, not crash.
  cchar *bad =
      "((fun %x :signature (void) :entry %b0";
  cchar *err = 0;
  CGv2Program *bad_prog = cg_v2_parse(bad, &err);
  if (bad_prog || !err) {
    printf("  bad-syntax test: expected error, got %s prog and %s err\n",
           bad_prog ? "non-null" : "null", err ? "set" : "unset");
    return 1;
  }

  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_roundtrip);

// Phase 4 commit 2 — minimal LLVM emit for test 01.
//
// Parse test 01, emit into a fresh LLVM module, verify the
// module is well-formed and contains a function named "hi"
// with void return and a ret void terminator.
int run_cg_ir_v2_emit_test01() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %hi\n"
      "   :signature (void)\n"
      "   :entry %b0\n"
      "   (block %b0\n"
      "     :term (ret))))";

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }

  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  // The "hi" function should be in TheModule.
  llvm::Function *fn = TheModule->getFunction("hi");
  if (!fn) {
    printf("  function 'hi' not found in module\n");
    return 1;
  }
  if (!fn->getReturnType()->isVoidTy()) {
    printf("  'hi' return type is not void\n");
    return 1;
  }
  if (fn->arg_size() != 0) {
    printf("  'hi' has unexpected args (%u)\n",
           (unsigned)fn->arg_size());
    return 1;
  }
  if (fn->size() != 1) {
    printf("  'hi' has %u blocks, expected 1\n",
           (unsigned)fn->size());
    return 1;
  }
  llvm::BasicBlock &bb = fn->getEntryBlock();
  if (!bb.getTerminator()) {
    printf("  'hi' entry block has no terminator\n");
    return 1;
  }
  if (!llvm::isa<llvm::ReturnInst>(bb.getTerminator())) {
    printf("  'hi' entry terminator is not Ret\n");
    return 1;
  }

  // verifyModule on the whole module.
  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }

  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test01);

// Phase 4 commit 2 also covers the two-block br case so we
// know the block CFG wiring works for non-trivial flows.
int run_cg_ir_v2_emit_test01_br() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %two\n"
      "   :signature (void)\n"
      "   :entry %b0\n"
      "   (block %b0\n"
      "     :term (br %b1))\n"
      "   (block %b1\n"
      "     :preds (%b0)\n"
      "     :term (ret))))";

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(text, &err);
  if (!prog) { printf("  parse failed: %s\n", err ? err : ""); return 1; }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("two");
  if (!fn || fn->size() != 2) {
    printf("  expected fn 'two' with 2 blocks; got %s/%u\n",
           fn ? "found" : "null", fn ? (unsigned)fn->size() : 0);
    return 1;
  }
  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test01_br);

// Phase 4 commit 3 — test 02 round-trip.
//
// Test 02 (from CG_IR_TEXT.md): a function returning an int
// constant. Exercises:
//   - top-level `(const ...)` decl parsing/printing
//   - `(int N)` ImmValue
//   - cross-decl reference resolution (`%c42` in fun body
//     resolves to the program-scope const)
//   - non-void :signature
//   - CG2_RET with one rval
int run_cg_ir_v2_test02_roundtrip() {
  cchar *text =
      "((const %c42 (int 42) :type int64)\n"
      " (fun %answer\n"
      "   :signature (int64)\n"
      "   :entry %b0\n"
      "   (block %b0\n"
      "     :term (ret %c42))))";
  if (!run_one("test02", text, 1, "answer")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test02_roundtrip);

// Phase 4 commit 3 — test 02 LLVM emit.
//
// Verify: parse + emit produces a function `@answer` of type
// `i64()` whose entry block is `ret i64 42`.
int run_cg_ir_v2_emit_test02() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((const %c42 (int 42) :type int64)\n"
      " (fun %answer\n"
      "   :signature (int64)\n"
      "   :entry %b0\n"
      "   (block %b0\n"
      "     :term (ret %c42))))";

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }

  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("answer");
  if (!fn) {
    printf("  function 'answer' not found in module\n");
    return 1;
  }
  if (!fn->getReturnType()->isIntegerTy(64)) {
    printf("  'answer' return type is not i64\n");
    return 1;
  }
  if (fn->size() != 1) {
    printf("  'answer' has %u blocks, expected 1\n",
           (unsigned)fn->size());
    return 1;
  }
  llvm::BasicBlock &bb = fn->getEntryBlock();
  llvm::Instruction *term = bb.getTerminator();
  if (!term) {
    printf("  'answer' entry has no terminator\n");
    return 1;
  }
  auto *ret = llvm::dyn_cast<llvm::ReturnInst>(term);
  if (!ret) {
    printf("  'answer' terminator is not Ret\n");
    return 1;
  }
  llvm::Value *rv = ret->getReturnValue();
  if (!rv) {
    printf("  'answer' ret has no value\n");
    return 1;
  }
  auto *ci = llvm::dyn_cast<llvm::ConstantInt>(rv);
  if (!ci || !ci->getType()->isIntegerTy(64) ||
      ci->getSExtValue() != 42) {
    printf("  'answer' did not return i64 42\n");
    return 1;
  }

  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test02);
