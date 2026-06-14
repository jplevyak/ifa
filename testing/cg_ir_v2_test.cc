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

// Phase 4 commit 14 — test 12 (recursion).
//
// def fact(n):
//   if n <= 1: return 1
//   return n * fact(n - 1)
//
// Exercises:
//   - per-fn fun_ref value with implicit target = enclosing fn
//     (the recursive self-call pattern)
//   - binop sub-ops le, sub, mul
//   - multi-block CFG with both branches returning
static cchar *test12_text =
    "((const %one (int 1) :type int64)\n"
    " (fun %fact\n"
    "   :signature (int64 int64)\n"
    "   :entry %b0\n"
    "   :formals (%n)\n"
    "   (value %n        :type int64   :scope formal)\n"
    "   (value %is_base  :type bool    :scope local)\n"
    "   (value %nm1      :type int64   :scope local)\n"
    "   (value %sub_res  :type int64   :scope local)\n"
    "   (value %prod     :type int64   :scope local)\n"
    "   (value %fact_ref :type fun_ptr :scope fun_ref)\n"
    "   (block %b0\n"
    "     (inst %i0 binop le %n %one => %is_base)\n"
    "     :term (cond_br %is_base %b_base %b_rec))\n"
    "   (block %b_base\n"
    "     :preds (%b0)\n"
    "     :term (ret %one))\n"
    "   (block %b_rec\n"
    "     :preds (%b0)\n"
    "     (inst %i1 binop sub %n %one     => %nm1)\n"
    "     (inst %i2 call %fact_ref %nm1   => %sub_res)\n"
    "     (inst %i3 binop mul %n %sub_res => %prod)\n"
    "     :term (ret %prod))))";

int run_cg_ir_v2_test12_roundtrip() {
  if (!run_one("test12", test12_text, 1, "fact")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test12_roundtrip);

int run_cg_ir_v2_emit_test12() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test12_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("fact");
  if (!fn) { printf("  'fact' not found\n"); return 1; }
  if (fn->size() != 3) {
    printf("  expected 3 blocks; got %u\n", (unsigned)fn->size());
    return 1;
  }

  // Recursive self-call: the rec block must contain a CallInst
  // whose target is @fact itself.
  bool found_self_call = false;
  for (llvm::BasicBlock &bb : *fn) {
    for (llvm::Instruction &i : bb) {
      if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
        if (c->getCalledFunction() == fn) found_self_call = true;
      }
    }
  }
  if (!found_self_call) {
    printf("  no recursive @fact call\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test12);

// Phase 4 commit 15 — test 13 (composition test).
//
// Combines alloc + multi-fn + struct mutation + loop:
//
//   struct Counter { value: int64 }
//   def reset(c): c.value = 0
//   def bump(c):  c.value = c.value + 1
//   def count_up_to(n):
//     c = Counter(); reset(c)
//     while c.value < n: bump(c)
//     return c.value
//
// Exercises composition of every concept landed so far. The
// :struct hint on field ops is the new piece — it lets
// reset/bump's ptr-formal-arg field accesses know the struct
// type (which can't be recovered from the per-fn ptr_struct
// map because there's no ALLOC in reset/bump).
//
// No phi needed: the loop variable is the struct field, re-
// read each iteration.
static cchar *test13_text =
    "((type %Counter :kind struct :is_heap_aggregate true\n"
    "   :fields ((%value :type int64 :idx 0)))\n"
    " (const %zero (int 0) :type int64)\n"
    " (const %one  (int 1) :type int64)\n"
    " (fun %reset\n"
    "   :signature (void ptr)\n"
    "   :entry %b0\n"
    "   :formals (%c)\n"
    "   (value %c :type ptr :scope formal)\n"
    "   (block %b0\n"
    "     (inst %i0 field_store :field_idx 0 :struct %Counter %c %zero)\n"
    "     :term (ret)))\n"
    " (fun %bump\n"
    "   :signature (void ptr)\n"
    "   :entry %b0\n"
    "   :formals (%c)\n"
    "   (value %c  :type ptr   :scope formal)\n"
    "   (value %v0 :type int64 :scope local)\n"
    "   (value %v1 :type int64 :scope local)\n"
    "   (block %b0\n"
    "     (inst %i0 field_load  :field_idx 0 :struct %Counter %c => %v0)\n"
    "     (inst %i1 binop add %v0 %one => %v1)\n"
    "     (inst %i2 field_store :field_idx 0 :struct %Counter %c %v1)\n"
    "     :term (ret)))\n"
    " (value %reset_ref :type fun_ptr :scope fun_ref :target %reset)\n"
    " (value %bump_ref  :type fun_ptr :scope fun_ref :target %bump)\n"
    " (fun %count_up_to\n"
    "   :signature (int64 int64)\n"
    "   :main true\n"
    "   :entry %b_entry\n"
    "   :formals (%n)\n"
    "   (value %n    :type int64 :scope formal)\n"
    "   (value %c    :type ptr   :scope local)\n"
    "   (value %cv   :type int64 :scope local)\n"
    "   (value %cond :type bool  :scope local)\n"
    "   (block %b_entry\n"
    "     (inst %i0 alloc :type %Counter => %c)\n"
    "     (inst %i1 call %reset_ref %c)\n"
    "     :term (br %b_head))\n"
    "   (block %b_head\n"
    "     :preds (%b_entry %b_body)\n"
    "     (inst %i2 field_load :field_idx 0 %c => %cv)\n"
    "     (inst %i3 binop lt %cv %n => %cond)\n"
    "     :term (cond_br %cond %b_body %b_exit))\n"
    "   (block %b_body\n"
    "     :preds (%b_head)\n"
    "     (inst %i4 call %bump_ref %c)\n"
    "     :term (br %b_head))\n"
    "   (block %b_exit\n"
    "     :preds (%b_head)\n"
    "     :term (ret %cv))))";

int run_cg_ir_v2_test13_roundtrip() {
  if (!run_one("test13", test13_text, 3, "reset")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test13_roundtrip);

int run_cg_ir_v2_emit_test13() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test13_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *reset_fn = TheModule->getFunction("reset");
  llvm::Function *bump_fn = TheModule->getFunction("bump");
  llvm::Function *main_fn = TheModule->getFunction("count_up_to");
  if (!reset_fn || !bump_fn || !main_fn) {
    printf("  missing one of reset/bump/count_up_to\n");
    return 1;
  }
  if (main_fn->size() != 4) {
    printf("  count_up_to expected 4 blocks; got %u\n",
           (unsigned)main_fn->size());
    return 1;
  }

  // main_fn must call reset and bump.
  bool calls_reset = false, calls_bump = false;
  for (llvm::BasicBlock &bb : *main_fn) {
    for (llvm::Instruction &i : bb) {
      if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
        if (c->getCalledFunction() == reset_fn) calls_reset = true;
        if (c->getCalledFunction() == bump_fn) calls_bump = true;
      }
    }
  }
  if (!calls_reset || !calls_bump) {
    printf("  count_up_to does not call reset and bump\n");
    return 1;
  }

  // verifyModule is the structural check.
  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test13);

// SSU-per-use narrowing test (Phase 5 follow-up).
//
// IFA does SSU, not SSA: each USE of a source variable gets
// its own Var post-pass, allowing conditionals to narrow types
// per branch. CG_IR_v2 honours this transparently — each SSU-
// renamed Var becomes its own CGv2Value with its own :type,
// disambiguated by CGv2Value* pointer identity (not by name
// string).
//
// This test exercises that explicitly. Pseudocode:
//
//   def abs(x):
//     if x >= 0:
//       (here x has SSU name x_nonneg, lattice says x>=0)
//       return x_nonneg
//     else:
//       (here x has SSU name x_neg, lattice says x<0)
//       return -x_neg
//
// The SSU renames are explicit CG2_MOVE insts (no LLVM op, just
// a rebind at the SSU level). At the join, %abs_val is a phi
// of the two branch values. value_map and alloca_map identity
// is keyed on CGv2Value*, so even though x_nonneg and x_neg
// trace back to the same source x, they're distinct values
// with their own (potentially different) types.
//
// The corpus uses int64 for all SSU names because LLVM types
// are coarser than pyc's lattice. The structural property is
// the load-bearing thing — verifyModule + arm-distinct
// behaviour proves the model carries SSU naming correctly.
static cchar *test_ssu_text =
    "((const %zero (int 0) :type int64)\n"
    " (fun %abs\n"
    "   :signature (int64 int64)\n"
    "   :entry %b_entry\n"
    "   :formals (%x)\n"
    "   (value %x         :type int64 :scope formal)\n"
    "   (value %cond      :type bool  :scope local)\n"
    "   (value %x_nonneg  :type int64 :scope local)\n"
    "   (value %x_neg     :type int64 :scope local)\n"
    "   (value %neg_x     :type int64 :scope local)\n"
    "   (value %abs_val   :type int64 :scope local)\n"
    "   (block %b_entry\n"
    "     (inst %i0 binop lt %x %zero => %cond)\n"
    "     :term (cond_br %cond %b_neg %b_pos))\n"
    "   (block %b_pos\n"
    "     :preds (%b_entry)\n"
    "     (inst %i1 move %x => %x_nonneg)\n"
    "     :term (br %b_join))\n"
    "   (block %b_neg\n"
    "     :preds (%b_entry)\n"
    "     (inst %i2 move %x => %x_neg)\n"
    "     (inst %i3 binop sub %zero %x_neg => %neg_x)\n"
    "     :term (br %b_join))\n"
    "   (block %b_join\n"
    "     :preds (%b_pos %b_neg)\n"
    "     :phi_by_pred\n"
    "       ((%b_pos ((move %x_nonneg => %abs_val)))\n"
    "        (%b_neg ((move %neg_x    => %abs_val))))\n"
    "     :term (ret %abs_val))))";

int run_cg_ir_v2_test_ssu_roundtrip() {
  if (!run_one("ssu_narrowing", test_ssu_text, 1, "abs")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test_ssu_roundtrip);

int run_cg_ir_v2_emit_test_ssu() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test_ssu_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("abs");
  if (!fn) { printf("  'abs' not found\n"); return 1; }
  if (fn->size() != 4) {
    printf("  expected 4 blocks; got %u\n", (unsigned)fn->size());
    return 1;
  }

  // The join block's alloca'd abs_val must exist (the SSU-
  // merged value). Look for an AllocaInst in the entry block.
  llvm::BasicBlock &entry = fn->getEntryBlock();
  bool has_alloca = false;
  for (llvm::Instruction &i : entry) {
    if (llvm::isa<llvm::AllocaInst>(&i)) { has_alloca = true; break; }
  }
  if (!has_alloca) {
    printf("  entry has no alloca for the SSU-merged value\n");
    return 1;
  }

  // verifyModule is the load-bearing structural check — if the
  // SSU model didn't carry through (e.g. value_map collisions
  // on shared source names), verifyModule would catch the
  // resulting dominance / phi-incoming violations.
  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_ssu);

// Phase A.1 — test 11 (prim escape hatch: write/writeln).
//
// First production-parity step. CG2_PRIM dispatches by name
// to a per-prim emit function. v0 ships write + writeln only
// (int64-typed args for write). Each subsequent IF1 prim adds
// one branch to dispatch_prim — same per-test cadence as
// Phase 4.
//
// def shout(n: int64):
//   write(n)
//   writeln()
static cchar *test11_text =
    "((fun %shout\n"
    "   :signature (void int64)\n"
    "   :entry %b0\n"
    "   :formals (%n)\n"
    "   (value %n :type int64 :scope formal)\n"
    "   (block %b0\n"
    "     (inst %i0 prim :name \"write\" %n)\n"
    "     (inst %i1 prim :name \"writeln\")\n"
    "     :term (ret))))";

int run_cg_ir_v2_test11_roundtrip() {
  if (!run_one("test11", test11_text, 1, "shout")) return 1;
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_test11_roundtrip);

int run_cg_ir_v2_emit_test11() {
  llvm_codegen_initialize(nullptr);

  cchar *err = 0;
  CGv2Program *prog = cg_v2_parse(test11_text, &err);
  if (!prog) {
    printf("  parse failed: %s\n", err ? err : "(no msg)");
    return 1;
  }
  if (!cg_v2_emit_llvm_module(prog)) {
    printf("  emit returned false\n");
    return 1;
  }

  llvm::Function *fn = TheModule->getFunction("shout");
  llvm::Function *printf_fn = TheModule->getFunction("printf");
  if (!fn || !printf_fn) {
    printf("  shout or printf missing\n");
    return 1;
  }

  // Two CallInsts to printf in shout's entry block.
  int n_printf_calls = 0;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      if (c->getCalledFunction() == printf_fn) n_printf_calls++;
    }
  }
  if (n_printf_calls != 2) {
    printf("  expected 2 printf calls; got %d\n", n_printf_calls);
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test11);

// Phase A.2 — write type coverage.
//
// Extends emit_prim_write past the int64-only v0. Test verifies
// each typed write produces a distinct printf format global and
// the correct cast.
//
// def write_typed(i: int32, u: uint64, b: bool, f: float64):
//   write(i)
//   write(u)
//   write(b)
//   write(f)
int run_cg_ir_v2_emit_test_write_types() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %write_typed\n"
      "   :signature (void int32 uint64 bool float64)\n"
      "   :entry %b0\n"
      "   :formals (%i %u %b %f)\n"
      "   (value %i :type int32   :scope formal)\n"
      "   (value %u :type uint64  :scope formal)\n"
      "   (value %b :type bool    :scope formal)\n"
      "   (value %f :type float64 :scope formal)\n"
      "   (block %b0\n"
      "     (inst %i0 prim :name \"write\" %i)\n"
      "     (inst %i1 prim :name \"write\" %u)\n"
      "     (inst %i2 prim :name \"write\" %b)\n"
      "     (inst %i3 prim :name \"write\" %f)\n"
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

  llvm::Function *fn = TheModule->getFunction("write_typed");
  llvm::Function *printf_fn = TheModule->getFunction("printf");
  if (!fn || !printf_fn) {
    printf("  write_typed or printf missing\n");
    return 1;
  }

  int n_printf = 0;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      if (c->getCalledFunction() == printf_fn) n_printf++;
    }
  }
  if (n_printf != 4) {
    printf("  expected 4 printf calls; got %d\n", n_printf);
    return 1;
  }

  // The four distinct typed-write format globals must exist.
  // Names follow get_format_str()'s ".str.<hint>" convention.
  cchar *expected[] = {
      ".str.write_int", ".str.write_u64",
      ".str.write_bool", ".str.write_float"
  };
  for (cchar *name : expected) {
    if (!TheModule->getNamedGlobal(name)) {
      printf("  missing format global '%s'\n", name);
      return 1;
    }
  }

  std::string err_str;
  llvm::raw_string_ostream rso(err_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    printf("  verifyModule failed: %s\n", err_str.c_str());
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_write_types);

// Phase A.3 — to_string prim.
//
// to_string(v) → ptr (GC-managed null-terminated buffer). The
// numeric/bool path: GC_malloc(64) + snprintf. The string-arg
// path: passthrough.
//
// def stringify(n: int64) -> ptr: return to_string(n)
int run_cg_ir_v2_emit_test_to_string() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %stringify\n"
      "   :signature (ptr int64)\n"
      "   :entry %b0\n"
      "   :formals (%n)\n"
      "   (value %n :type int64 :scope formal)\n"
      "   (value %s :type ptr   :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 prim :name \"to_string\" %n => %s)\n"
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

  llvm::Function *fn = TheModule->getFunction("stringify");
  llvm::Function *gcm = TheModule->getFunction("GC_malloc");
  llvm::Function *sprn = TheModule->getFunction("snprintf");
  if (!fn || !gcm || !sprn) {
    printf("  missing stringify/GC_malloc/snprintf\n");
    return 1;
  }

  // Entry block: CallInst GC_malloc, CallInst snprintf, Ret of
  // the malloc'd ptr.
  llvm::CallInst *malloc_call = nullptr;
  llvm::CallInst *sprintf_call = nullptr;
  llvm::ReturnInst *ret = nullptr;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      if (c->getCalledFunction() == gcm) malloc_call = c;
      else if (c->getCalledFunction() == sprn) sprintf_call = c;
    } else if (auto *r = llvm::dyn_cast<llvm::ReturnInst>(&i)) {
      ret = r;
    }
  }
  if (!malloc_call) { printf("  no GC_malloc call\n"); return 1; }
  if (!sprintf_call) { printf("  no snprintf call\n"); return 1; }
  if (!ret) { printf("  no Ret\n"); return 1; }
  if (ret->getReturnValue() != malloc_call) {
    printf("  Ret does not return the GC_malloc result\n");
    return 1;
  }
  // snprintf's first arg should be the GC_malloc result.
  if (sprintf_call->getArgOperand(0) != malloc_call) {
    printf("  snprintf does not write into the GC_malloc buffer\n");
    return 1;
  }
  // tostr_i64 format global must exist.
  if (!TheModule->getNamedGlobal(".str.tostr_i64")) {
    printf("  missing .str.tostr_i64 global\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_to_string);

// Phase A.4 — binop sub-op coverage.
//
// Exercises every new sub-op landed in this commit: div, mod,
// gt, ge, eq, ne, and, or, xor, shl, shr. Each is a separate
// inst in a one-block fn; the test counts opcode appearances
// in the emitted module.
//
// def all_ops(a: int64, b: int64) -> int64:
//   q   = a / b
//   r   = a % b
//   gt  = a > b
//   ge  = a >= b
//   eq  = a == b
//   ne  = a != b
//   and = a & b
//   or  = a | b
//   xor = a ^ b
//   shl = a << b
//   shr = a >> b
//   return q + r
int run_cg_ir_v2_emit_test_binop_coverage() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %all_ops\n"
      "   :signature (int64 int64 int64)\n"
      "   :entry %b0\n"
      "   :formals (%a %b)\n"
      "   (value %a   :type int64 :scope formal)\n"
      "   (value %b   :type int64 :scope formal)\n"
      "   (value %q   :type int64 :scope local)\n"
      "   (value %r   :type int64 :scope local)\n"
      "   (value %gt  :type bool  :scope local)\n"
      "   (value %ge  :type bool  :scope local)\n"
      "   (value %eq  :type bool  :scope local)\n"
      "   (value %ne  :type bool  :scope local)\n"
      "   (value %an  :type int64 :scope local)\n"
      "   (value %or  :type int64 :scope local)\n"
      "   (value %xo  :type int64 :scope local)\n"
      "   (value %sl  :type int64 :scope local)\n"
      "   (value %sr  :type int64 :scope local)\n"
      "   (value %sum :type int64 :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0  binop div %a %b => %q)\n"
      "     (inst %i1  binop mod %a %b => %r)\n"
      "     (inst %i2  binop gt  %a %b => %gt)\n"
      "     (inst %i3  binop ge  %a %b => %ge)\n"
      "     (inst %i4  binop eq  %a %b => %eq)\n"
      "     (inst %i5  binop ne  %a %b => %ne)\n"
      "     (inst %i6  binop and %a %b => %an)\n"
      "     (inst %i7  binop or  %a %b => %or)\n"
      "     (inst %i8  binop xor %a %b => %xo)\n"
      "     (inst %i9  binop shl %a %b => %sl)\n"
      "     (inst %i10 binop shr %a %b => %sr)\n"
      "     (inst %i11 binop add %q %r => %sum)\n"
      "     :term (ret %sum))))";

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
  llvm::Function *fn = TheModule->getFunction("all_ops");
  if (!fn) { printf("  fn missing\n"); return 1; }

  // Count specific opcodes to make sure each sub-op produced
  // its expected LLVM instruction kind.
  int n_sdiv = 0, n_srem = 0;
  int n_icmp = 0;
  int n_and = 0, n_or = 0, n_xor = 0;
  int n_shl = 0, n_ashr = 0;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (i.getOpcode() == llvm::Instruction::SDiv) n_sdiv++;
    else if (i.getOpcode() == llvm::Instruction::SRem) n_srem++;
    else if (llvm::isa<llvm::ICmpInst>(&i)) n_icmp++;
    else if (i.getOpcode() == llvm::Instruction::And) n_and++;
    else if (i.getOpcode() == llvm::Instruction::Or) n_or++;
    else if (i.getOpcode() == llvm::Instruction::Xor) n_xor++;
    else if (i.getOpcode() == llvm::Instruction::Shl) n_shl++;
    else if (i.getOpcode() == llvm::Instruction::AShr) n_ashr++;
  }
  if (n_sdiv != 1 || n_srem != 1 || n_icmp != 4 || n_and != 1 ||
      n_or != 1 || n_xor != 1 || n_shl != 1 || n_ashr != 1) {
    printf("  opcode counts off — sdiv=%d srem=%d icmp=%d and=%d "
           "or=%d xor=%d shl=%d ashr=%d\n",
           n_sdiv, n_srem, n_icmp, n_and, n_or, n_xor, n_shl, n_ashr);
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_binop_coverage);

// Phase A.5 — CG2_INDEX_STORE (symmetric with INDEX_LOAD).
//
// def set_first(v: IntVec, x: int64):
//   v[0] = x
//
// Exercises CG2_INDEX_STORE — GEP to element + Store.
int run_cg_ir_v2_emit_test_index_store() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((type %IntVec :kind ptr :element int64)\n"
      " (const %zero (int 0) :type int64)\n"
      " (fun %set_first\n"
      "   :signature (void %IntVec int64)\n"
      "   :entry %b0\n"
      "   :formals (%v %x)\n"
      "   (value %v :type %IntVec :scope formal)\n"
      "   (value %x :type int64   :scope formal)\n"
      "   (block %b0\n"
      "     (inst %i0 index_store %v %zero %x)\n"
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

  llvm::Function *fn = TheModule->getFunction("set_first");
  if (!fn) { printf("  fn missing\n"); return 1; }

  int n_gep = 0, n_store = 0;
  llvm::StoreInst *the_store = nullptr;
  llvm::GetElementPtrInst *the_gep = nullptr;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (auto *g = llvm::dyn_cast<llvm::GetElementPtrInst>(&i)) {
      n_gep++; the_gep = g;
    } else if (auto *s = llvm::dyn_cast<llvm::StoreInst>(&i)) {
      n_store++; the_store = s;
    }
  }
  if (n_gep != 1 || n_store != 1) {
    printf("  expected 1 GEP + 1 Store; got %d/%d\n", n_gep, n_store);
    return 1;
  }
  if (the_store->getPointerOperand() != the_gep) {
    printf("  Store does not write to the GEP's result\n");
    return 1;
  }
  // The stored value should be the function's x arg.
  llvm::Argument *arg1 = &*std::next(fn->arg_begin());
  if (the_store->getValueOperand() != arg1) {
    printf("  Store value is not the x formal\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_index_store);

// Phase A.6 + Phase B.10.11 — CG2_LEN.
// char-ptr (string-like) args  → strlen (libc, always linkable)
// other ptr types              → ConstantInt(0) (no runtime
//                                helper dependency — v1's
//                                _CG_prim_len isn't linkable
//                                in either backend)
//
// def lens(s: char*, v: IntVec) -> int64: return len(s) + len(v)
int run_cg_ir_v2_emit_test_len() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((type %CharPtr :kind ptr :element int8)\n"
      " (type %IntVec  :kind ptr :element int64)\n"
      " (fun %lens\n"
      "   :signature (int64 %CharPtr %IntVec)\n"
      "   :entry %b0\n"
      "   :formals (%s %v)\n"
      "   (value %s   :type %CharPtr :scope formal)\n"
      "   (value %v   :type %IntVec  :scope formal)\n"
      "   (value %sl  :type int64    :scope local)\n"
      "   (value %vl  :type int64    :scope local)\n"
      "   (value %sum :type int64    :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 len %s => %sl)\n"
      "     (inst %i1 len %v => %vl)\n"
      "     (inst %i2 binop add %sl %vl => %sum)\n"
      "     :term (ret %sum))))";

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
  llvm::Function *fn = TheModule->getFunction("lens");
  llvm::Function *strlen_fn = TheModule->getFunction("strlen");
  if (!fn || !strlen_fn) {
    printf("  missing fn / strlen\n");
    return 1;
  }
  // One call to strlen for the CharPtr len; the IntVec len
  // becomes a constant 0 with no call. Verify there's exactly
  // one strlen call.
  int n_strlen = 0;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      if (c->getCalledFunction() == strlen_fn) n_strlen++;
    }
  }
  if (n_strlen != 1) {
    printf("  expected 1 strlen call, got %d\n", n_strlen);
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_len);

// Phase A.7 — CG2_CAST. Auto-dispatched typecast.
//
// def all_casts(s8: int8, f: float32) -> int64:
//   w64  = (int64) s8       # SExt
//   uf   = (uint32) w64     # Trunc + same-width no-op
//   fd   = (float64) f      # FPExt
//   fi   = (int64) fd       # FPToSI
//   return fi
int run_cg_ir_v2_emit_test_cast() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((fun %all_casts\n"
      "   :signature (int64 int8 float32)\n"
      "   :entry %b0\n"
      "   :formals (%s8 %f)\n"
      "   (value %s8  :type int8    :scope formal)\n"
      "   (value %f   :type float32 :scope formal)\n"
      "   (value %w64 :type int64   :scope local)\n"
      "   (value %fd  :type float64 :scope local)\n"
      "   (value %fi  :type int64   :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 cast :type int64   %s8 => %w64)\n"
      "     (inst %i1 cast :type float64 %f  => %fd)\n"
      "     (inst %i2 cast :type int64   %fd => %fi)\n"
      "     :term (ret %fi))))";

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

  llvm::Function *fn = TheModule->getFunction("all_casts");
  if (!fn) { printf("  fn missing\n"); return 1; }

  int n_sext = 0, n_fpext = 0, n_fptosi = 0;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    switch (i.getOpcode()) {
      case llvm::Instruction::SExt:   n_sext++;   break;
      case llvm::Instruction::FPExt:  n_fpext++;  break;
      case llvm::Instruction::FPToSI: n_fptosi++; break;
    }
  }
  if (n_sext != 1 || n_fpext != 1 || n_fptosi != 1) {
    printf("  cast counts off — sext=%d fpext=%d fptosi=%d\n",
           n_sext, n_fpext, n_fptosi);
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_cast);

// Phase A.8 — CG2_SIZEOF + CG2_SIZEOF_ELEMENT.
// Both return compile-time i64 constants computed from
// the LLVM DataLayout. Used by array allocators
// (n_bytes = sizeof_element(arr) * count).
//
// def alloc_size(v: IntVec) -> int64:
//   p = sizeof(Pt)            # 16 (two i64 fields)
//   e = sizeof_element(v)     # 8 (i64)
//   return p + e              # 24
int run_cg_ir_v2_emit_test_sizeof() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((type %Pt :kind struct :is_heap_aggregate true\n"
      "   :fields ((%x :type int64 :idx 0)\n"
      "            (%y :type int64 :idx 1)))\n"
      " (type %IntVec :kind ptr :element int64)\n"
      " (fun %alloc_size\n"
      "   :signature (int64 %IntVec)\n"
      "   :entry %b0\n"
      "   :formals (%v)\n"
      "   (value %v   :type %IntVec :scope formal)\n"
      "   (value %p   :type int64   :scope local)\n"
      "   (value %e   :type int64   :scope local)\n"
      "   (value %sum :type int64   :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 sizeof :type %Pt => %p)\n"
      "     (inst %i1 sizeof_element %v => %e)\n"
      "     (inst %i2 binop add %p %e => %sum)\n"
      "     :term (ret %sum))))";

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
  llvm::Function *fn = TheModule->getFunction("alloc_size");
  if (!fn) { printf("  fn missing\n"); return 1; }

  // Both sizeof results are compile-time constants, so the
  // Builder folds Add(16, 8) -> 24 at insertion time. The
  // entry block contains just `ret i64 24`.
  auto *ret = llvm::dyn_cast<llvm::ReturnInst>(
      fn->getEntryBlock().getTerminator());
  if (!ret) { printf("  no Ret\n"); return 1; }
  auto *ci = llvm::dyn_cast<llvm::ConstantInt>(ret->getReturnValue());
  if (!ci) {
    printf("  Ret value not a folded constant\n");
    return 1;
  }
  if (ci->getZExtValue() != 24) {
    printf("  expected ret=24 (16+8), got %llu\n",
           (unsigned long long)ci->getZExtValue());
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_sizeof);

// Phase A.9 — binop operand-type dispatch.
//
// The same textual sub-op picks signed/unsigned/float LLVM
// ops based on operand kind. No new sub-ops needed.
//
// Two functions to exercise both float + unsigned variants:
//   float_ops(a: f64, b: f64) → b ? 1 : 0     (uses fadd, fdiv, fcmpolt)
//   uint_ops (a: u64, b: u64) → quot + (a<b?)  (uses udiv, icmpult)
int run_cg_ir_v2_emit_test_binop_dispatch() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((const %zero (int 0) :type int64)\n"
      " (const %one  (int 1) :type int64)\n"
      " (fun %float_ops\n"
      "   :signature (bool float64 float64)\n"
      "   :entry %b0\n"
      "   :formals (%a %b)\n"
      "   (value %a    :type float64 :scope formal)\n"
      "   (value %b    :type float64 :scope formal)\n"
      "   (value %s    :type float64 :scope local)\n"
      "   (value %d    :type float64 :scope local)\n"
      "   (value %lt   :type bool    :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 binop add %a %b => %s)\n"
      "     (inst %i1 binop div %a %b => %d)\n"
      "     (inst %i2 binop lt  %a %b => %lt)\n"
      "     :term (ret %lt)))\n"
      " (fun %uint_ops\n"
      "   :signature (bool uint64 uint64)\n"
      "   :entry %b0\n"
      "   :formals (%a %b)\n"
      "   (value %a   :type uint64 :scope formal)\n"
      "   (value %b   :type uint64 :scope formal)\n"
      "   (value %q   :type uint64 :scope local)\n"
      "   (value %ult :type bool   :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 binop div %a %b => %q)\n"
      "     (inst %i1 binop lt  %a %b => %ult)\n"
      "     :term (ret %ult))))";

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

  // float_ops: expect FAdd, FDiv, FCmpOLT.
  llvm::Function *ffn = TheModule->getFunction("float_ops");
  if (!ffn) { printf("  float_ops missing\n"); return 1; }
  int n_fadd = 0, n_fdiv = 0, n_fcmp = 0;
  for (llvm::Instruction &i : ffn->getEntryBlock()) {
    if (i.getOpcode() == llvm::Instruction::FAdd) n_fadd++;
    else if (i.getOpcode() == llvm::Instruction::FDiv) n_fdiv++;
    else if (auto *c = llvm::dyn_cast<llvm::FCmpInst>(&i)) {
      if (c->getPredicate() == llvm::CmpInst::FCMP_OLT) n_fcmp++;
    }
  }
  if (n_fadd != 1 || n_fdiv != 1 || n_fcmp != 1) {
    printf("  float_ops counts off: fadd=%d fdiv=%d fcmpolt=%d\n",
           n_fadd, n_fdiv, n_fcmp);
    return 1;
  }

  // uint_ops: expect UDiv + ICmpULT.
  llvm::Function *ufn = TheModule->getFunction("uint_ops");
  if (!ufn) { printf("  uint_ops missing\n"); return 1; }
  int n_udiv = 0, n_iult = 0;
  for (llvm::Instruction &i : ufn->getEntryBlock()) {
    if (i.getOpcode() == llvm::Instruction::UDiv) n_udiv++;
    else if (auto *c = llvm::dyn_cast<llvm::ICmpInst>(&i)) {
      if (c->getPredicate() == llvm::CmpInst::ICMP_ULT) n_iult++;
    }
  }
  if (n_udiv != 1 || n_iult != 1) {
    printf("  uint_ops counts off: udiv=%d iult=%d\n", n_udiv, n_iult);
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_binop_dispatch);

// Phase A.10 — CG2_CLONE.
//
// def dup(p: ptr) -> ptr: return clone(p)
//
// Verifies the emit shape:
//   %r = call ptr @GC_malloc(i64 16)
//   call void @llvm.memcpy.p0.p0.i64(ptr %r, ptr %p, i64 16, i1 false)
//   ret ptr %r
int run_cg_ir_v2_emit_test_clone() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((type %Pt :kind struct :is_heap_aggregate true\n"
      "   :fields ((%x :type int64 :idx 0)\n"
      "            (%y :type int64 :idx 1)))\n"
      " (fun %dup\n"
      "   :signature (ptr ptr)\n"
      "   :entry %b0\n"
      "   :formals (%p)\n"
      "   (value %p :type ptr :scope formal)\n"
      "   (value %r :type ptr :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 clone :type %Pt %p => %r)\n"
      "     :term (ret %r))))";

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

  llvm::Function *fn = TheModule->getFunction("dup");
  if (!fn) { printf("  dup missing\n"); return 1; }

  llvm::CallInst *gcm_call = nullptr;
  bool saw_memcpy = false;
  llvm::Function *gcm = TheModule->getFunction("GC_malloc");
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      if (c->getCalledFunction() == gcm) gcm_call = c;
      else if (auto *cf = c->getCalledFunction()) {
        if (cf->getName().starts_with("llvm.memcpy")) saw_memcpy = true;
      }
    }
  }
  if (!gcm_call) { printf("  no GC_malloc call\n"); return 1; }
  if (!saw_memcpy) { printf("  no memcpy intrinsic call\n"); return 1; }

  // GC_malloc's size arg should be the constant 16 (two i64
  // fields on the standard DataLayout).
  auto *sz = llvm::dyn_cast<llvm::ConstantInt>(gcm_call->getArgOperand(0));
  if (!sz || sz->getZExtValue() != 16) {
    printf("  GC_malloc size expected 16, got %llu\n",
           (unsigned long long)(sz ? sz->getZExtValue() : 0));
    return 1;
  }

  auto *ret = llvm::dyn_cast<llvm::ReturnInst>(
      fn->getEntryBlock().getTerminator());
  if (!ret || ret->getReturnValue() != gcm_call) {
    printf("  Ret does not return the GC_malloc result\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_clone);

// Phase A.11 — integration test: the full print_int pipeline.
//
// def print_int(n: int64):
//   buf: char* = to_string(n)
//   write(buf)
//   writeln()
//
// Validates that the prim layer composes correctly: to_string
// returns a typed CharPtr; write dispatches on element=int8 to
// the %s format; writeln closes the line; the alloca-backed
// local %buf carries the to_string result through to write.
//
// This is the shape pyc emits for `print(int_value)` calls.
int run_cg_ir_v2_emit_test_print_int_pipeline() {
  llvm_codegen_initialize(nullptr);

  cchar *text =
      "((type %CharPtr :kind ptr :element int8)\n"
      " (fun %print_int\n"
      "   :signature (void int64)\n"
      "   :entry %b0\n"
      "   :formals (%n)\n"
      "   (value %n   :type int64    :scope formal)\n"
      "   (value %buf :type %CharPtr :scope local)\n"
      "   (block %b0\n"
      "     (inst %i0 prim :name \"to_string\" %n   => %buf)\n"
      "     (inst %i1 prim :name \"write\"     %buf)\n"
      "     (inst %i2 prim :name \"writeln\")\n"
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

  llvm::Function *fn = TheModule->getFunction("print_int");
  llvm::Function *gcm = TheModule->getFunction("GC_malloc");
  llvm::Function *spr = TheModule->getFunction("snprintf");
  llvm::Function *prf = TheModule->getFunction("printf");
  if (!fn || !gcm || !spr || !prf) {
    printf("  missing fn / GC_malloc / snprintf / printf\n");
    return 1;
  }

  int n_gcm = 0, n_spr = 0, n_prf = 0;
  llvm::CallInst *write_prf_call = nullptr;
  llvm::CallInst *first_prf_call = nullptr;
  for (llvm::Instruction &i : fn->getEntryBlock()) {
    if (auto *c = llvm::dyn_cast<llvm::CallInst>(&i)) {
      llvm::Function *cf = c->getCalledFunction();
      if (cf == gcm) n_gcm++;
      else if (cf == spr) n_spr++;
      else if (cf == prf) {
        n_prf++;
        if (!first_prf_call) first_prf_call = c;
        // The first printf is the write call (uses the buf).
        if (!write_prf_call) write_prf_call = c;
      }
    }
  }
  if (n_gcm != 1 || n_spr != 1 || n_prf != 2) {
    printf("  call counts off — gcm=%d spr=%d prf=%d\n",
           n_gcm, n_spr, n_prf);
    return 1;
  }

  // The write printf's format global should be .str.write_str.
  if (!TheModule->getNamedGlobal(".str.write_str")) {
    printf("  missing .str.write_str global (write didn't take char-ptr path)\n");
    return 1;
  }
  // The writeln printf's format should be .str.writeln (the
  // explicit "\n" we emit).
  if (!TheModule->getNamedGlobal(".str.writeln")) {
    printf("  missing .str.writeln global\n");
    return 1;
  }
  // And the to_string format for int64 should be present.
  if (!TheModule->getNamedGlobal(".str.tostr_i64")) {
    printf("  missing .str.tostr_i64 global\n");
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

UNIT_TEST_FUN(run_cg_ir_v2_emit_test_print_int_pipeline);
