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

// Global LLVM state shared by llvm.cc (entry points, main wrapper)
// and cg_ir_v2_emit_llvm.cc (the production emit path).
extern std::unique_ptr<llvm::LLVMContext> TheContext;
extern std::unique_ptr<llvm::Module> TheModule;

// Optimize TheModule as `-b` does (clang -O2 with LTO; it also lowers the
// coroutines). With `link_runtime` the runtime bitcode is linked in and the
// module internalized; the JIT passes false. Returns the target machine it
// optimized for. llvm.cc.
namespace llvm { class TargetMachine; }
std::unique_ptr<llvm::TargetMachine> llvm_optimize_module(bool link_runtime, bool *linked_runtime);
extern std::unique_ptr<llvm::IRBuilder<>> Builder;
extern std::unique_ptr<llvm::DIBuilder> DBuilder;
extern llvm::DICompileUnit *CU;
extern llvm::DIFile *UnitFile;

// Bring up TheContext / TheModule / Builder / DBuilder.  Exposed
// for unit tests that need a live LLVMContext (production path
// calls it from llvm_codegen_print_ir).
void llvm_codegen_initialize(FA *fa);

// Build the `_CG_*` type-string prefixes on builtin Syms and run
// the cg_string assignment passes (consumed by cg_normalize_v2
// when it reads f->cg_string / s->cg_string).
void llvm_build_type_strings(FA *fa);

// Diagnostic prints from the LLVM backend.  Gated on `ifa_debug`
// (settable via `-d` / `-d -d` / ifa_debug = N) so default runs
// are silent and golden-file tests can diff stdout/stderr cleanly.
#define DEBUG_LOG(...) do { if (ifa_debug) fprintf(stderr, "DEBUG: " __VA_ARGS__); } while (0)

// Active-emission helpers for frontend-registered RegisteredPrim::llvm_cgfn
// callbacks invoked from emit_send_primitive().
llvm::Value *llvm_emit_get_var(Var *v);
void llvm_emit_put_result(Var *dst, llvm::Value *val);
llvm::Type *llvm_emit_type(Sym *s);
llvm::Value *llvm_emit_record_field(Sym *rec_ty, llvm::Value *obj, int idx, Sym **out_field_ty);
llvm::Value *llvm_emit_classtag_eq(Sym *rec_ty, llvm::Value *obj);

// LLVM 23 asserts in BasicBlock::getTerminator() when a block is not
// terminated yet (replacing the null test with BasicBlock::hasTerminator()),
// while LLVM 20 does not have BasicBlock::hasTerminator().
inline bool bb_has_terminator(const llvm::BasicBlock &bb) {
  return !bb.empty() && bb.back().isTerminator();
}
inline bool bb_has_terminator(const llvm::BasicBlock *bb) {
  return bb && !bb->empty() && bb->back().isTerminator();
}

#endif // _llvm_internal_H_
