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

// Phase 4 commit 4 — test 03 (identity fn) round-trip.
//
// Test 03: (fun %id :signature (int64 int64) ... :formals (%x)
//           (value %x :type int64 :scope formal)
//           (block %b0 :term (ret %x)))
//
// Exercises:
//   - :formals (%x) syntax
//   - (value %x :type T :scope formal) decl
//   - CGV_FORMAL value resolution
int run_cg_ir_v2_test03_roundtrip() {
  cchar *text =
      "((fun %id\n"
      "   :signature (int64 int64)\n"
      "   :entry %b0\n"
      "   :formals (%x)\n"
      "   (value %x :type int64 :scope formal)\n"
      "   (block %b0\n"
      "     :term (ret %x))))";
  if (!run_one("test03", text, 1, "id")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test03_roundtrip);

// Phase 4 commit 4 — test 03 LLVM emit.
//
// Verify @id has signature i64(i64), one block, terminator
// `ret i64 %x` where the returned value is the function's arg.
int run_cg_ir_v2_emit_test03() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %id\n"
      "   :signature (int64 int64)\n"
      "   :entry %b0\n"
      "   :formals (%x)\n"
      "   (value %x :type int64 :scope formal)\n"
      "   (block %b0\n"
      "     :term (ret %x))))";

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

  llvm::Function *fn = TheModule->getFunction("id");
  if (!fn) {
    printf("  function 'id' not found\n");
    return 1;
  }
  if (!fn->getReturnType()->isIntegerTy(64)) {
    printf("  'id' return type is not i64\n");
    return 1;
  }
  if (fn->arg_size() != 1) {
    printf("  'id' expected 1 arg, got %u\n",
           (unsigned)fn->arg_size());
    return 1;
  }
  llvm::Argument *arg0 = &*fn->arg_begin();
  if (!arg0->getType()->isIntegerTy(64)) {
    printf("  'id' arg0 is not i64\n");
    return 1;
  }
  if (fn->size() != 1) {
    printf("  'id' has %u blocks, expected 1\n",
           (unsigned)fn->size());
    return 1;
  }
  auto *ret = llvm::dyn_cast<llvm::ReturnInst>(
      fn->getEntryBlock().getTerminator());
  if (!ret) {
    printf("  'id' terminator is not Ret\n");
    return 1;
  }
  if (ret->getReturnValue() != arg0) {
    printf("  'id' did not return its arg\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test03);

// Phase 4 commit 5 — test 04 (sum2: local + binop add).
//
// Test 04: (fun %sum2 :formals (%a %b)
//             (value %a/%b :formal) (value %s :local)
//             (block %b0 (inst %i0 binop add %a %b => %s)
//                        :term (ret %s)))
//
// Exercises:
//   - (value %s :scope local)
//   - body inst: (inst %i0 binop add %a %b => %s)
//   - CGV_LOCAL def-use through value_map
int run_cg_ir_v2_test04_roundtrip() {
  cchar *text =
      "((fun %sum2\n"
      "   :signature (int64 int64 int64)\n"
      "   :entry %b0\n"
      "   :formals (%a %b)\n"
      "   (value %a :type int64 :scope formal)\n"
      "   (value %b :type int64 :scope formal)\n"
      "   (value %s :type int64 :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 binop add %a %b => %s)\n"
      "     :term (ret %s))))";
  if (!run_one("test04", text, 1, "sum2")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test04_roundtrip);

int run_cg_ir_v2_emit_test04() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %sum2\n"
      "   :signature (int64 int64 int64)\n"
      "   :entry %b0\n"
      "   :formals (%a %b)\n"
      "   (value %a :type int64 :scope formal)\n"
      "   (value %b :type int64 :scope formal)\n"
      "   (value %s :type int64 :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 binop add %a %b => %s)\n"
      "     :term (ret %s))))";

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

  llvm::Function *fn = TheModule->getFunction("sum2");
  if (!fn) { printf("  'sum2' not found\n"); return 1; }
  if (fn->arg_size() != 2) {
    printf("  'sum2' expected 2 args, got %u\n",
           (unsigned)fn->arg_size());
    return 1;
  }

  llvm::BasicBlock &bb = fn->getEntryBlock();
  // Expect an Add inst feeding the Ret.
  auto *ret = llvm::dyn_cast<llvm::ReturnInst>(bb.getTerminator());
  if (!ret) { printf("  no Ret\n"); return 1; }
  llvm::Value *rv = ret->getReturnValue();
  auto *bo = llvm::dyn_cast<llvm::BinaryOperator>(rv);
  if (!bo || bo->getOpcode() != llvm::Instruction::Add) {
    printf("  ret value is not an Add\n");
    return 1;
  }
  // Operands must be the function's two args, in order.
  llvm::Argument *arg0 = &*fn->arg_begin();
  llvm::Argument *arg1 = &*std::next(fn->arg_begin());
  if (bo->getOperand(0) != arg0 || bo->getOperand(1) != arg1) {
    printf("  add operands are not (arg0, arg1)\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test04);

// Phase 4 commit 6 — test 05 (sign: cond_br + binop lt).
//
// sign(x) = -1 if x < 0 else 1
//
// Exercises:
//   - multi-block CFG with cond_br divergence
//   - binop lt (signed int cmp)
//   - bool-typed local (the icmp result)
//   - constants returned in distinct successors
//   - negative int constant (int -1)
static cchar *test05_text =
    "((const %neg1 (int -1) :type int64)\n"
    " (const %one  (int  1) :type int64)\n"
    " (const %zero (int  0) :type int64)\n"
    " (fun %sign\n"
    "   :signature (int64 int64)\n"
    "   :entry %b0\n"
    "   :formals (%x)\n"
    "   (value %x :type int64 :scope formal)\n"
    "   (value %t0 :type bool :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 binop lt %x %zero => %t0)\n"
    "     :term (cond_br %t0 %b1 %b2))\n"
    "   (block %b1\n"
    "     :preds (%b0)\n"
    "     :term (ret %neg1))\n"
    "   (block %b2\n"
    "     :preds (%b0)\n"
    "     :term (ret %one))))";

int run_cg_ir_v2_test05_roundtrip() {
  if (!run_one("test05", test05_text, 1, "sign")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test05_roundtrip);

int run_cg_ir_v2_emit_test05() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test05_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("sign");
  if (!fn) { printf("  'sign' not found\n"); return 1; }
  if (fn->size() != 3) {
    printf("  'sign' has %u blocks, expected 3\n",
           (unsigned)fn->size());
    return 1;
  }

  // Entry block must end in CondBr(icmp slt, true_block, false_block).
  llvm::BasicBlock &entry = fn->getEntryBlock();
  auto *br = llvm::dyn_cast<llvm::BranchInst>(entry.getTerminator());
  if (!br || !br->isConditional()) {
    printf("  entry terminator is not a CondBr\n");
    return 1;
  }
  auto *cmp = llvm::dyn_cast<llvm::ICmpInst>(br->getCondition());
  if (!cmp || cmp->getPredicate() != llvm::CmpInst::ICMP_SLT) {
    printf("  CondBr cond is not an ICmp SLT\n");
    return 1;
  }
  llvm::Argument *arg0 = &*fn->arg_begin();
  if (cmp->getOperand(0) != arg0) {
    printf("  ICmp lhs is not arg0\n");
    return 1;
  }
  auto *rhs = llvm::dyn_cast<llvm::ConstantInt>(cmp->getOperand(1));
  if (!rhs || rhs->getSExtValue() != 0) {
    printf("  ICmp rhs is not 0\n");
    return 1;
  }

  // Each successor must return a distinct constant.
  llvm::BasicBlock *tb = br->getSuccessor(0);
  llvm::BasicBlock *fb = br->getSuccessor(1);
  auto check_ret = [](llvm::BasicBlock *b, int64_t want) -> bool {
    auto *r = llvm::dyn_cast<llvm::ReturnInst>(b->getTerminator());
    if (!r) return false;
    auto *c = llvm::dyn_cast<llvm::ConstantInt>(r->getReturnValue());
    return c && c->getSExtValue() == want;
  };
  if (!check_ret(tb, -1)) {
    printf("  true block does not return -1\n");
    return 1;
  }
  if (!check_ret(fb, 1)) {
    printf("  false block does not return 1\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test05);

// Phase 4 commit 7 — CG2_MOVE (value copy).
//
// Test 06 (full loop + :phi_by_pred + alloca lowering) is
// staged across two commits. This one lands plain CG2_MOVE
// alone — alias semantics in value_map. Next commit adds the
// phi/alloca lowering on top.
//
// Test shape ("copy"): identity-via-move.
//   y = x
//   return y
static cchar *test_copy_text =
    "((fun %copy\n"
    "   :signature (int64 int64)\n"
    "   :entry %b0\n"
    "   :formals (%x)\n"
    "   (value %x :type int64 :scope formal)\n"
    "   (value %y :type int64 :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 move %x => %y)\n"
    "     :term (ret %y))))";

int run_cg_ir_v2_test_copy_roundtrip() {
  if (!run_one("copy", test_copy_text, 1, "copy")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test_copy_roundtrip);

int run_cg_ir_v2_emit_test_copy() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test_copy_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("copy");
  if (!fn) { printf("  'copy' not found\n"); return 1; }
  llvm::Argument *arg0 = &*fn->arg_begin();
  auto *ret = llvm::dyn_cast<llvm::ReturnInst>(
      fn->getEntryBlock().getTerminator());
  if (!ret || ret->getReturnValue() != arg0) {
    printf("  'copy' did not return its arg via move alias\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_copy);

// Phase 4 commit 8 — test 06 (count_to: loop + :phi_by_pred).
//
// Second half of the test 06 landing. Adds the cross-block
// phi machinery using pyc's alloca/store/load convention:
//   - :phi_by_pred parses into CGv2PhiGroup lists
//   - emitter alloca's every phi-target local in entry
//   - resolve_value loads from the alloca on each use
//   - phi MOVEs emit store on the predecessor's edge
//
// count_to(n) = while i < n: i = i + 1; return i  (returns n)
static cchar *test06_text =
    "((const %zero (int 0) :type int64)\n"
    " (const %one  (int 1) :type int64)\n"
    " (fun %count_to\n"
    "   :signature (int64 int64)\n"
    "   :entry %b_entry\n"
    "   :formals (%n)\n"
    "   (value %n        :type int64 :scope formal)\n"
    "   (value %i_entry  :type int64 :scope local)\n"
    "   (value %i_loop   :type int64 :scope local)\n"
    "   (value %i_next   :type int64 :scope local)\n"
    "   (value %cond     :type bool  :scope local)\n"
    "   (block %b_entry\n"
    "     (inst %i0 move %zero => %i_entry)\n"
    "     :term (br %b_head))\n"
    "   (block %b_head\n"
    "     :phi_by_pred\n"
    "       ((%b_entry ((move %i_entry => %i_loop)))\n"
    "        (%b_body  ((move %i_next  => %i_loop))))\n"
    "     (inst %i1 binop lt %i_loop %n => %cond)\n"
    "     :term (cond_br %cond %b_body %b_exit))\n"
    "   (block %b_body\n"
    "     (inst %i2 binop add %i_loop %one => %i_next)\n"
    "     :term (br %b_head))\n"
    "   (block %b_exit\n"
    "     :term (ret %i_loop))))";

int run_cg_ir_v2_test06_roundtrip() {
  if (!run_one("test06", test06_text, 1, "count_to")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test06_roundtrip);

int run_cg_ir_v2_emit_test06() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test06_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("count_to");
  if (!fn) { printf("  'count_to' not found\n"); return 1; }
  if (fn->size() != 4) {
    printf("  'count_to' has %u blocks, expected 4\n",
           (unsigned)fn->size());
    return 1;
  }

  // verifyModule is the load-bearing check here — it catches
  // dominance violations from phi MOVEs landing in the wrong
  // block, mismatched alloca types, etc.
  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }

  // Spot-check: entry must contain an Alloca (for i_loop) and
  // end in br to head.
  llvm::BasicBlock &entry = fn->getEntryBlock();
  bool found_alloca = false;
  for (llvm::Instruction &i : entry) {
    if (llvm::isa<llvm::AllocaInst>(i)) { found_alloca = true; break; }
  }
  if (!found_alloca) {
    printf("  entry block has no alloca for phi-target\n");
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test06);

// Phase 4 commit 9 — test 07 (inter-fun call).
//
// main() { return id(7); }
//
// Exercises:
//   - CG2_CALL inst
//   - top-level (value %id_ref :scope fun_ref :target %id)
//     for the callee binding
//   - declare_fun() pre-pass so the call resolves regardless
//     of source ordering
static cchar *test07_text =
    "((const %c7 (int 7) :type int64)\n"
    " (fun %id\n"
    "   :signature (int64 int64)\n"
    "   :entry %b0\n"
    "   :formals (%x)\n"
    "   (value %x :type int64 :scope formal)\n"
    "   (block %b0\n"
    "     :term (ret %x)))\n"
    " (value %id_ref :type fun_ptr :scope fun_ref :target %id)\n"
    " (fun %main\n"
    "   :signature (int64)\n"
    "   :main true\n"
    "   :entry %b0\n"
    "   (value %r :type int64 :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 call %id_ref %c7 => %r)\n"
    "     :term (ret %r))))";

int run_cg_ir_v2_test07_roundtrip() {
  if (!run_one("test07", test07_text, 2, "id")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test07_roundtrip);

int run_cg_ir_v2_emit_test07() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test07_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *id_fn = TheModule->getFunction("id");
  llvm::Function *main_fn = TheModule->getFunction("main");
  if (!id_fn || !main_fn) {
    printf("  missing id/main fn\n");
    return 1;
  }

  // main's entry must contain a CallInst calling @id with a
  // single i64 7 arg, and Ret returns that call.
  llvm::BasicBlock &entry = main_fn->getEntryBlock();
  llvm::CallInst *call = nullptr;
  for (llvm::Instruction &i : entry) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      call = c; break;
    }
  }
  if (!call) {
    printf("  main has no CallInst\n");
    return 1;
  }
  if (call->getCalledFunction() != id_fn) {
    printf("  CallInst does not call @id\n");
    return 1;
  }
  if (call->arg_size() != 1) {
    printf("  CallInst expected 1 arg\n");
    return 1;
  }
  auto *arg_ci = llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(0));
  if (!arg_ci || arg_ci->getSExtValue() != 7) {
    printf("  CallInst arg is not 7\n");
    return 1;
  }
  auto *ret = llvm::dyn_cast<llvm::ReturnInst>(entry.getTerminator());
  if (!ret || ret->getReturnValue() != call) {
    printf("  Ret does not return the call\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test07);

// Phase 4 commit 10 — struct type decl parsing.
//
// Lands the syntax + data model for `(type %Name :kind struct
//   :is_heap_aggregate true :fields ((%f :type T :idx N)*))`
// in isolation. No new ops yet — CG_ALLOC and field accessors
// land in commit 11 atop this.
//
// Verifies: parse, round-trip, and that the type appears in
// prog->types with the expected name and field count.
int run_cg_ir_v2_type_decl_roundtrip() {
  cchar *text =
      "((type %Point :kind struct :is_heap_aggregate true\n"
      "   :fields ((%x :type int64 :idx 0)\n"
      "            (%y :type int64 :idx 1)))\n"
      " (fun %use\n"
      "   :signature (void)\n"
      "   :entry %b0\n"
      "   (block %b0 :term (ret))))";
  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (prog->types.n != 1) {
    printf("  expected 1 type, got %d\n", prog->types.n);
    return 1;
  }
  CGv2Type *t = prog->types[0];
  if (!t->name || strcmp(t->name, "Point") != 0) {
    printf("  type name expected 'Point', got '%s'\n",
           t->name ? t->name : "(null)");
    return 1;
  }
  if (t->fields.n != 2) {
    printf("  expected 2 fields, got %d\n", t->fields.n);
    return 1;
  }
  if (!t->is_heap_aggregate) {
    printf("  is_heap_aggregate not set\n");
    return 1;
  }
  // Round-trip: re-parse the printed form, check stable.
  cchar *back = cg_v2_print(prog);
  cchar *err2 = 0;
  CGv2Program *prog2 = cg_v2_parse(back, &err2);
  if (!prog2 || prog2->types.n != 1 ||
      strcmp(prog2->types[0]->name, "Point") != 0 ||
      prog2->types[0]->fields.n != 2) {
    printf("  round-trip failed; reparse err=%s\n",
           err2 ? err2 : "(none)");
    printf("  printed text:\n%s\n", back);
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_type_decl_roundtrip);

// Phase 4 commit 11 — test 08 (alloc + field store/load).
//
// struct Point { x: int64; y: int64 }
// def make_pt():
//   p = Point(); p.x = 3; return p.x
//
// Exercises:
//   - CG2T_STRUCT lowering to llvm::StructType
//   - CG2_ALLOC via GC_malloc external call
//   - CG2_FIELD_STORE / CG2_FIELD_LOAD via getelementptr
//   - per-fun ptr_struct map (lets field ops find the struct)
static cchar *test08_text =
    "((type %Point :kind struct :is_heap_aggregate true\n"
    "   :fields ((%x :type int64 :idx 0)\n"
    "            (%y :type int64 :idx 1)))\n"
    " (const %c3 (int 3) :type int64)\n"
    " (fun %make_pt\n"
    "   :signature (int64)\n"
    "   :entry %b0\n"
    "   (value %p :type ptr   :scope local)\n"
    "   (value %v :type int64 :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 alloc :type %Point => %p)\n"
    "     (inst %i1 field_store :field_idx 0 %p %c3)\n"
    "     (inst %i2 field_load  :field_idx 0 %p => %v)\n"
    "     :term (ret %v))))";

int run_cg_ir_v2_test08_roundtrip() {
  if (!run_one("test08", test08_text, 1, "make_pt")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test08_roundtrip);

int run_cg_ir_v2_emit_test08() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test08_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("make_pt");
  if (!fn) { printf("  'make_pt' not found\n"); return 1; }

  // Expect: CallInst (GC_malloc), GEP + Store, GEP + Load, Ret.
  llvm::BasicBlock &bb = fn->getEntryBlock();
  bool saw_malloc = false, saw_store = false, saw_load = false;
  for (llvm::Instruction &i : bb) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      if (c->getCalledFunction() &&
          c->getCalledFunction()->getName() == "GC_malloc")
        saw_malloc = true;
    } else if (llvm::isa<llvm::StoreInst>(&i)) {
      saw_store = true;
    } else if (llvm::isa<llvm::LoadInst>(&i)) {
      saw_load = true;
    }
  }
  if (!saw_malloc || !saw_store || !saw_load) {
    printf("  expected GC_malloc/Store/Load; saw %d/%d/%d\n",
           saw_malloc, saw_store, saw_load);
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test08);

// Phase 4 commit 12 — test 09 (global + construction-flow).
//
// THE STRUCTURAL ISSUE-017 TEST.
//
// struct Box { val: int64 }
// box: Box*  (global, init null)
// def init_box(): box = Box(); box.val = 42
// def get_val():  return box.val
//
// Exercises (and verifies that v2's structural fix holds):
//   - (global %name :type ptr :scope global) — top-level
//     global decl declared as llvm::GlobalVariable @box
//   - CG2_MOVE %tmp => %box — store into a global; carries
//     the ptr_struct annotation across the function boundary
//     via g_prog_ctx.global_ptr_struct (the cross-fn map)
//   - Resolve CGV_GLOBAL by loading @box at use site
//   - get_val emits load @box → GEP → load i64 — across the
//     function boundary, the struct type is known because
//     init_box registered it in the program-scope map
//
// Under v1, the cross-fn instruction leak of issue 017
// manifested here. Under v2, the per-CGFun cache stays per-
// fun and only the explicit program-scope global_ptr_struct
// crosses the boundary.
static cchar *test09_text =
    "((type %Box :kind struct :is_heap_aggregate true\n"
    "   :fields ((%val :type int64 :idx 0)))\n"
    " (const %c42 (int 42) :type int64)\n"
    " (global %box :type ptr :scope global)\n"
    " (fun %init_box\n"
    "   :signature (void)\n"
    "   :entry %b0\n"
    "   (value %tmp :type ptr :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 alloc :type %Box => %tmp)\n"
    "     (inst %i1 move %tmp => %box)\n"
    "     (inst %i2 field_store :field_idx 0 %box %c42)\n"
    "     :term (ret)))\n"
    " (fun %get_val\n"
    "   :signature (int64)\n"
    "   :entry %b0\n"
    "   (value %v :type int64 :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 field_load :field_idx 0 %box => %v)\n"
    "     :term (ret %v))))";

int run_cg_ir_v2_test09_roundtrip() {
  if (!run_one("test09", test09_text, 2, "init_box")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test09_roundtrip);

int run_cg_ir_v2_emit_test09() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test09_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  // Global @box must exist.
  llvm::GlobalVariable *box_gv = TheModule->getNamedGlobal("box");
  if (!box_gv) { printf("  @box not declared\n"); return 1; }
  if (!box_gv->getValueType()->isPointerTy()) {
    printf("  @box not a ptr\n");
    return 1;
  }

  llvm::Function *init_fn = TheModule->getFunction("init_box");
  llvm::Function *get_fn = TheModule->getFunction("get_val");
  if (!init_fn || !get_fn) {
    printf("  init_box/get_val missing\n");
    return 1;
  }

  // verifyModule is the load-bearing check — under v1 the
  // module would be malformed here (instructions leaking
  // across functions). Under v2 it must verify cleanly.
  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }

  // Spot-check: init_box must contain a Store into @box.
  bool found_store_to_box = false;
  for (llvm::Instruction &i : init_fn->getEntryBlock()) {
    if (auto *s = llvm::dyn_cast<llvm::StoreInst>(&i)) {
      if (s->getPointerOperand() == box_gv) {
        found_store_to_box = true;
        break;
      }
    }
  }
  if (!found_store_to_box) {
    printf("  init_box does not store into @box\n");
    return 1;
  }

  // Spot-check: get_val must contain a Load from @box.
  bool found_load_from_box = false;
  for (llvm::Instruction &i : get_fn->getEntryBlock()) {
    if (auto *l = llvm::dyn_cast<llvm::LoadInst>(&i)) {
      if (l->getPointerOperand() == box_gv) {
        found_load_from_box = true;
        break;
      }
    }
  }
  if (!found_load_from_box) {
    printf("  get_val does not load from @box\n");
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test09);

// Phase 4 commit 13 — test 10 (vector indexing).
//
// sum_first_two(v) = v[0] + v[1]
//
// Exercises:
//   - typed-ptr type decl (kind=ptr + :element)
//   - CG2_INDEX_LOAD via gep on the element type + load
static cchar *test10_text =
    "((type %IntVec :kind ptr :element int64)\n"
    " (const %zero (int 0) :type int64)\n"
    " (const %one  (int 1) :type int64)\n"
    " (fun %sum_first_two\n"
    "   :signature (int64 %IntVec)\n"
    "   :entry %b0\n"
    "   :formals (%v)\n"
    "   (value %v :type %IntVec :scope formal)\n"
    "   (value %a :type int64   :scope local)\n"
    "   (value %b :type int64   :scope local)\n"
    "   (value %s :type int64   :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 index_load %v %zero => %a)\n"
    "     (inst %i1 index_load %v %one  => %b)\n"
    "     (inst %i2 binop add  %a %b    => %s)\n"
    "     :term (ret %s))))";

int run_cg_ir_v2_test10_roundtrip() {
  if (!run_one("test10", test10_text, 1, "sum_first_two")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test10_roundtrip);

int run_cg_ir_v2_emit_test10() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test10_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("sum_first_two");
  if (!fn) { printf("  fn not found\n"); return 1; }

  // Should see two GEPs + two Loads + an Add.
  int n_gep = 0, n_load = 0, n_add = 0;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (llvm::isa<llvm::GetElementPtrInst>(&i)) n_gep++;
    else if (llvm::isa<llvm::LoadInst>(&i)) n_load++;
    else if (auto *bo = llvm::dyn_cast<llvm::BinaryOperator>(&i)) {
      if (bo->getOpcode() == llvm::Instruction::Add) n_add++;
    }
  }
  if (n_gep != 2 || n_load != 2 || n_add != 1) {
    printf("  expected 2 GEP + 2 Load + 1 Add; got %d/%d/%d\n",
           n_gep, n_load, n_add);
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test10);
