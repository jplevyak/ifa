#include "llvm_internal.h"
#include <set>
#include <string>
#include "builtin.h"
#include "cg.h"
#include "codegen_common.h"
#include "fail.h"
#include "if1.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/IPO/Internalize.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#if LLVM_VERSION_MAJOR >= 22
#undef FREAD
#undef FWRITE
#include "llvm/Analysis/RuntimeLibcallInfo.h"
#endif
#include "llvm/Support/SourceMgr.h"
#include <fcntl.h>
#include <spawn.h>
#include <stdarg.h>
#include <sys/wait.h>
#include <unistd.h>
extern char **environ;
#include <sstream>

// DEBUG_LOG moved to llvm_internal.h; runtime-gated on ifa_debug.

// ============================================================================
// Global LLVM State (definitions)
// ============================================================================

std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::DIBuilder> DBuilder;
llvm::DICompileUnit *CU = nullptr;
llvm::DIFile *UnitFile = nullptr;

// Exposed for unit tests that need a live LLVMContext (CG_IR_PLAN
// Phase 3.1 onwards). Production codegen still calls it from
// llvm_codegen_print_ir as the static-link-time entry.
void llvm_codegen_initialize(FA *fa);
void llvm_codegen_initialize(FA *fa) {
  // Initialize LLVM components
  // The host target only: the module's triple is getDefaultTargetTriple()
  // below, and clang (not pyc) compiles the .ll. Initializing every target
  // linked each LLVM backend into a static pyc (`make pyc-static`) for
  // nothing.
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  // Tear down any previous-call state in reverse-dependency order.
  // The naive `TheContext = make_unique<...>()` assigns the new Context
  // first, which destroys the old Context — and then `TheModule = ...`
  // tries to destroy the old Module, which dereferences the just-freed
  // Context. Explicit reset() in reverse order keeps every destructor
  // running while its dependencies are still alive. CU and UnitFile
  // are raw pointers owned by DBuilder; null them so a re-entry that
  // doesn't go through createCompileUnit again can't dereference
  // dangling memory.
  CU = nullptr;
  UnitFile = nullptr;
  DBuilder.reset();
  Builder.reset();
  TheModule.reset();
  TheContext.reset();

  // v1-era file-scope caches (string_constants_map,
  // label_to_bb_map, reverse_call_graph) retired with the v1
  // LLVM backend; v2 owns its own per-CGFun caches inside
  // cg_ir_v2_emit_llvm.cc and rebuilds them from scratch on
  // each emit pass.

  TheContext = std::make_unique<llvm::LLVMContext>();
  // Use a more descriptive module ID, perhaps from FA or filename
  cchar *mod_id = fa && fa->pdb && fa->pdb->if1 && fa->pdb->if1->filename ? fa->pdb->if1->filename : "ifa_output";
  TheModule = std::make_unique<llvm::Module>(mod_id, *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
  DBuilder = std::make_unique<llvm::DIBuilder>(*TheModule);

  // Set target triple for the module. Module::setTargetTriple's
  // parameter type changed from StringRef to Triple somewhere after
  // LLVM 20 (confirmed: 18 and 20 both take StringRef; 22 takes
  // Triple) -- the >=19 threshold below was wrong (verified against
  // installed 18/20/22 headers), not a documented LLVM API version.
  // If this breaks again on a newer LLVM, check
  // llvm/IR/Module.h's setTargetTriple signature directly rather
  // than guessing another threshold.
  std::string TargetTriple = llvm::sys::getDefaultTargetTriple();
#if LLVM_VERSION_MAJOR >= 22
  TheModule->setTargetTriple(llvm::Triple(TargetTriple));
#else
  TheModule->setTargetTriple(TargetTriple);
#endif

  // Enable PIC for position-independent code generation
  TheModule->setPICLevel(llvm::PICLevel::BigPIC);
  TheModule->setPIELevel(llvm::PIELevel::Default);
  // Optionally, set data layout
  // std::string Error;
  // auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
  // if (Target) {
  //   auto TM = Target->createTargetMachine(TargetTriple, "generic", "", llvm::TargetOptions(),
  //   llvm::Optional<llvm::Reloc::Model>()); TheModule->setDataLayout(TM->createDataLayout());
  // } else {
  //   fail("Could not lookup target: %s", Error.c_str());
  // }
}

// is_closure_var(Var*) — moved to codegen_common.{h,cc}.


void llvm_build_type_strings(FA *fa) {
  // ifa/201: the same members exist on both backends. Before the names,
  // the slot map and any struct type, as in c_codegen_print_c.
  cg_elide_unread_slots(fa);
  cg_name_builtin_types(fa);
  // Annotate=false: LLVM IR identifiers can't carry comments. No
  // globals collection: the LLVM backend discovers globals separately
  // via createGlobalVariables. See codegen_common.{h,cc}.
  assign_fun_cg_strings(fa, /*annotate=*/false, /*globals=*/nullptr);
  Vec<Var *> globals;
  Vec<Sym *> allsyms;
  collect_types_and_globals(fa, allsyms, globals);
  // fp=nullptr: the LLVM backend doesn't emit struct-forward-decls;
  // struct types are built via getLLVMType when needed.
  assign_type_cg_strings_pass1(allsyms, /*fp=*/nullptr);
  assign_type_cg_strings_pass2(allsyms);
  // ifa/issues/124: `--refuse-imprecise` has to mean the same thing on
  // both backends. It was implemented only in cg.cc, so under `-b` it
  // was a silent no-op and the 124 test "passed" on LLVM purely because
  // its assertion did not exist. The scan needs nothing but the type
  // strings just assigned above.
  cg_scan_imprecise(fa);
  cg_check_imprecise();
}

extern llvm::Type *cg_llvm_type_of(Sym *s);

// ifa/issues/212: the C names of the program's exported functions. They stay
// external when the module is internalized for LTO, as `main` does.
static std::set<std::string> exported_names;

// The LLVM type an exported function declares for a formal or its result: a
// number type is itself, an object an opaque pointer.
static llvm::Type *export_llvm_type(Sym *t) {
  llvm::Type *ty = t->num_kind ? cg_llvm_type_of(t) : nullptr;
  return ty ? ty : llvm::PointerType::getUnqual(*TheContext);
}

// `v` (declared type) as type `to`: a number widens or narrows, an object
// pointer is a pointer, and an integer may carry an address.
static llvm::Value *export_convert(llvm::Value *v, llvm::Type *to, bool is_signed) {
  llvm::Type *from = v->getType();
  if (from == to) return v;
  if (from->isIntegerTy() && to->isIntegerTy()) return Builder->CreateIntCast(v, to, is_signed);
  if (from->isFloatingPointTy() && to->isFloatingPointTy()) return Builder->CreateFPCast(v, to);
  if (from->isIntegerTy() && to->isFloatingPointTy())
    return is_signed ? Builder->CreateSIToFP(v, to) : Builder->CreateUIToFP(v, to);
  if (from->isFloatingPointTy() && to->isIntegerTy())
    return is_signed ? Builder->CreateFPToSI(v, to) : Builder->CreateFPToUI(v, to);
  if (from->isIntegerTy() && to->isPointerTy()) return Builder->CreateIntToPtr(v, to);
  if (from->isPointerTy() && to->isIntegerTy()) return Builder->CreatePtrToInt(v, to);
  fail("exported function: no conversion between its declared and inferred LLVM types (ifa/issues/212)");
  return v;
}

// An exported function's entry point: the declared C name and every declared
// argument, forwarding to the clone's live formals (emit's prototype drops
// dead ones and the closure), mirroring cg.cc's write_c_export.
static void emit_llvm_exports(FA *fa) {
  Vec<Fun *> exported;
  cg_exported_funs(fa, exported);
  for (Fun *f : exported) {
    Sym *s = f->sym;
    llvm::Function *target = f->llvm;
    if (!target) fail("exported function %s (%s) was not emitted (ifa/issues/212)", s->name, s->export_name);
    std::vector<llvm::Type *> param_tys;
    for (Sym *t : s->export_args) param_tys.push_back(export_llvm_type(t));
    llvm::Type *ret_ty = s->export_ret ? export_llvm_type(s->export_ret) : llvm::Type::getVoidTy(*TheContext);
    llvm::Function *fn = llvm::Function::Create(llvm::FunctionType::get(ret_ty, param_tys, false),
                                                llvm::Function::ExternalLinkage, s->export_name, TheModule.get());
    exported_names.insert(s->export_name);
    Builder->SetInsertPoint(llvm::BasicBlock::Create(*TheContext, "entry", fn));
    std::vector<llvm::Value *> args;
    MPosition p;
    p.push(1);
    for (int i = 0; i < s->has.n; i++) {
      MPosition *cp = cannonicalize_mposition(p);
      p.inc();
      Var *v = f->args.get(cp);
      if (!v || !v->live) continue;
      if ((v->sym && v->sym->is_fun) || (v->type && v->type->is_fun)) {
        if (!i) continue;  // the closure: emit_fun's prototype leaves it out
      }
      if (!i) fail("exported function %s (%s) reads its own closure, which its C caller cannot pass", s->name, s->export_name);
      if (args.size() >= target->arg_size()) fail("exported function %s: more live formals than parameters", s->name);
      Sym *t = s->export_args[i - 1];
      args.push_back(export_convert(fn->getArg(i - 1), target->getArg(args.size())->getType(),
                                    t->num_kind != IF1_NUM_KIND_UINT));
    }
    if (args.size() != target->arg_size()) fail("exported function %s: its clone takes other parameters", s->name);
    llvm::Value *r = Builder->CreateCall(target, args);
    if (ret_ty->isVoidTy())
      Builder->CreateRetVoid();
    else if (r->getType()->isVoidTy())
      Builder->CreateRet(llvm::Constant::getNullValue(ret_ty));
    else
      Builder->CreateRet(export_convert(r, ret_ty, s->export_ret->num_kind != IF1_NUM_KIND_UINT));
  }
}

void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main_fun, cchar *input_filename) {
  DEBUG_LOG("llvm_codegen_print_ir started\n");
  llvm_codegen_initialize(fa);
  llvm_build_type_strings(fa);
  if (!fa) {
    fail("FA object is null in llvm_codegen_print_ir");
    return;
  }
  if (!main_fun) {
    fail("Main function is null in llvm_codegen_print_ir");
    return;
  }

  // Create DIBuilder Compile Unit
  // Use the input_filename passed in, which has the actual source file path
  cchar *src_filename =
      input_filename ? input_filename : (fa->pdb->if1->filename ? fa->pdb->if1->filename : "unknown.ifa");
  // Basic path handling, might need improvement for complex paths
  std::string full_path = src_filename;
  std::string dir = ".";
  std::string fname = full_path;
  size_t last_slash = full_path.find_last_of("/\\");
  if (last_slash != std::string::npos) {
    dir = full_path.substr(0, last_slash);
    fname = full_path.substr(last_slash + 1);
  }
  TheModule->setSourceFileName(fname);  // Set source file name on the Module
  if (DBuilder) {
    UnitFile = DBuilder->createFile(fname, dir);
    CU = DBuilder->createCompileUnit(llvm::dwarf::DW_LANG_C, UnitFile, "ifa-compiler", 0 /*isOptimized*/, "" /*flags*/,
                                     0 /*RV*/);

    // Add debug info version to module
    TheModule->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
  }

  // LLVM direct emit path (Phase R)
  extern bool cg_emit_llvm(FA *fa, Fun *main_fun);
  bool ok = cg_emit_llvm(fa, main_fun);
  if (!ok) {
    fail("cg_emit_llvm returned false");
    return;
  }
  // Populate f->llvm cache so the synthesized C main() wrapper
  // below can resolve the IF1 main_fun via cg_get_llvm(f).
  for (Fun *f : fa->funs) {
    if (!f || !f->cg_string) continue;
    if (llvm::Function *fn = TheModule->getFunction(f->cg_string)) {
      f->llvm = fn;
    }
  }

  emit_llvm_exports(fa);  // ifa/issues/212

  // Create a simple main function that calls the IF1 main_fun.
  // Takes the real (argc, argv) and threads them into _CG_set_argv
  // before anything else runs, so pyc_lib/sys.py's `sys.argv` (built
  // via _CG_argc()/_CG_argv_at() c-calls) reflects the actual
  // invocation instead of the fixed one-element stub it used to be --
  // mirrors the C backend's generated main() (cg.cc).
  llvm::Type *i32_ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::Type *i64_ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *ptr_ty = llvm::PointerType::getUnqual(*TheContext);
  llvm::FunctionType *main_func_type = llvm::FunctionType::get(i32_ty, {i32_ty, ptr_ty}, false);
  llvm::Function *llvm_main =
      llvm::Function::Create(main_func_type, llvm::Function::ExternalLinkage, "main", TheModule.get());
  llvm_main->getArg(0)->setName("argc");
  llvm_main->getArg(1)->setName("argv");
  llvm::BasicBlock *main_entry_bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_main);
  Builder->SetInsertPoint(main_entry_bb);

  {
    // ifa/issues/051: initialise the collector FIRST, mirroring the C
    // backend's MEM_INIT(). GC_INIT registers the static data segment
    // as roots; without it the module globals emitted below are not
    // scanned and objects reachable only from them are collected while
    // live (use-after-free, surfacing as a crash inside GC).
    llvm::FunctionType *gc_init_ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {}, false);
    llvm::Function *gc_init_fn = TheModule->getFunction("_CG_gc_init");
    if (!gc_init_fn)
      gc_init_fn = llvm::Function::Create(gc_init_ty, llvm::Function::ExternalLinkage, "_CG_gc_init", TheModule.get());
    Builder->CreateCall(gc_init_fn, {});
  }

  {
    llvm::FunctionType *set_argv_ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {i64_ty, ptr_ty}, false);
    llvm::Function *set_argv_fn = TheModule->getFunction("_CG_set_argv");
    if (!set_argv_fn)
      set_argv_fn = llvm::Function::Create(set_argv_ty, llvm::Function::ExternalLinkage, "_CG_set_argv", TheModule.get());
    llvm::Value *argc64 = Builder->CreateSExt(llvm_main->getArg(0), i64_ty);
    Builder->CreateCall(set_argv_fn, {argc64, llvm_main->getArg(1)});
  }

  if (cg_get_llvm(main_fun)) {  // If IF1 main function was generated
    Builder->CreateCall(cg_get_llvm(main_fun));

    // Inject a call to _CG_event_loop_run(NULL) at the end of main
    llvm::Function *loop_run_fn = TheModule->getFunction("_CG_event_loop_run");
    if (!loop_run_fn) {
        llvm::FunctionType *loop_run_ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {llvm::PointerType::getUnqual(*TheContext)}, false);
        loop_run_fn = llvm::Function::Create(loop_run_ty, llvm::Function::ExternalLinkage, "_CG_event_loop_run", TheModule.get());
    }
    Builder->CreateCall(loop_run_fn, {llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*TheContext))});
  } else {
    // This case should ideally not happen if main_fun is valid and processed
    DEBUG_LOG("IF1 main function '%s' not found or generated in LLVM module\n", main_fun->sym->name);
  }
  Builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));

  // (DBuilder->finalize() is called once below, after module verification.)

  // Debug: Check for unterminated blocks before verification
  DEBUG_LOG("Pre-verification check for unterminated blocks:\n");
  for (llvm::Function &F : *TheModule) {
    for (llvm::BasicBlock &BB : F) {
      if (!bb_has_terminator(BB)) {
        DEBUG_LOG("WARNING: Function %s has unterminated block %s (size=%zu)\n", F.getName().str().c_str(),
                BB.getName().str().c_str(), BB.size());
        DEBUG_LOG("Block instructions:\n");
        int idx = 0;
        for (llvm::Instruction &I : BB) {
          if (ifa_debug) {
            fprintf(stderr, "DEBUG:   [%d] ", idx++);
            I.print(llvm::errs());
            fprintf(stderr, "\n");
          }
        }
      }
    }
  }

  // Debug dump pre-verify (PYC_DUMP_LL=path enables).
  if (const char *dump_path = getenv("PYC_DUMP_LL")) {
    std::error_code dec;
    llvm::raw_fd_ostream dump_os(dump_path, dec);
    if (!dec) TheModule->print(dump_os, nullptr);
  }

  // Finalize debug info before serializing.  Keeps `!dbg !N`
  // metadata indices stable between the on-failure .ll dump and
  // the verifier output.
  if (DBuilder) {
    DBuilder->finalize();
  }

  // Serialize the module to the .ll file BEFORE running the
  // verifier.  Two reasons:
  //   1. Post-mortem: when verifyModule fails, fail() exits — so
  //      without this reorder the .ll never lands on disk and
  //      you can't inspect the broken IR.  With the reorder the
  //      .ll exists and the harness's tests/build/<name>.ll
  //      artifact is debuggable.
  //   2. Tests can diff `.ll` even on intentional verify-failure
  //      cases (the issues/ directory regression-trail).
  std::string ir_string;
  llvm::raw_string_ostream ir_rso(ir_string);
  TheModule->print(ir_rso, nullptr);
  fprintf(fp, "%s", ir_rso.str().c_str());
  fflush(fp);

  // Verify the module.  verifyModule was always a hard gate; the
  // change here is the .ll reorder above and the optional
  // per-function attribution under --verify-each.
  std::string error_str;
  llvm::raw_string_ostream rso(error_str);
  bool broken = llvm::verifyModule(*TheModule, &rso);

  // --verify-each (PYC_VERIFY_EACH=1): scan user functions
  // for `undef` operands.  verifyModule accepts `undef` (it's
  // valid LLVM IR), but pyc's codegen should never emit it: an
  // undef value typically means "the FA didn't narrow this rval
  // to anything" and the optimizer is allowed to propagate undef
  // through to any constant, producing wrong runtime output
  // that's not caught by the regular verifier.  Issue 017's
  // "passes undef self to __new__" was exactly this shape;
  // surfacing it as a verification failure converts the bug
  // class from "wrong output via UB" to "compile error".
  // Declarations and emit-builtin externals are skipped.  Read
  // the env var directly — the ifa library doesn't see pyc's
  // defs.h globals (the --verify-each flag in pyc.cc sets the
  // env so both flag forms work).
  const char *sv = getenv("PYC_VERIFY_EACH");
  bool strict = sv && sv[0] && sv[0] != '0';
  if (strict) {
    int undef_count = 0;
    std::string sample;
    for (llvm::Function &F : *TheModule) {
      if (F.isDeclaration()) continue;
      for (llvm::BasicBlock &BB : F) {
        for (llvm::Instruction &I : BB) {
          // Exclusions: undef in `ret` and `phi` is common and
          // harmless.  `ret undef` happens at the tail of v2's
          // __main__-shaped functions whose result is unused by
          // the synthesized C main() wrapper; `phi undef` is the
          // standard LLVM idiom for an SSU edge that's never
          // taken in practice but is structurally required by
          // the CFG.  Neither propagates to user-visible
          // computation.  Issue 017's "passes undef self" was
          // a `call` operand, which this still catches.
          if (llvm::isa<llvm::ReturnInst>(&I) ||
              llvm::isa<llvm::PHINode>(&I)) continue;
          for (unsigned op = 0; op < I.getNumOperands(); op++) {
            if (llvm::isa<llvm::UndefValue>(I.getOperand(op))) {
              undef_count++;
              if (sample.empty()) {
                llvm::raw_string_ostream sos(sample);
                sos << "@" << F.getName().str() << ": ";
                I.print(sos);
              }
            }
          }
        }
      }
    }
    if (undef_count > 0) {
      broken = true;
      rso << "  verify-each: " << undef_count
          << " undef operand(s) in user functions; first: "
          << sample << "\n";
    }
  }

  if (broken) {
    fail("LLVM module verification failed: %s",
         rso.str().c_str());
  }
}
void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *input_filename) {
  char fn[512];
  strncpy(fn, input_filename, sizeof(fn) - 1);
  fn[sizeof(fn) - 1] = '\0';
  char *dot = strrchr(fn, '.');
  if (dot) {
    strcpy(dot, ".ll");  // Replace extension with .ll
  } else {
    strcat(fn, ".ll");  // Append .ll if no extension
  }

  FILE *fp = fopen(fn, "w");
  if (!fp) {
    fail("Unable to open file %s for writing LLVM IR", fn);
    return;
  }
  llvm_codegen_print_ir(fp, fa, main, input_filename);
  fclose(fp);
  report_call_resolution(fa);  // ifa/129: same census as the C backend
  DEBUG_LOG("LLVM IR written to %s\n", fn);
}


// The runtime as LLVM bitcode (pyc_runtime_lto.o, built beside
// libpyc_runtime.a by the top-level Makefile). llvm_emit_object links it into
// the program's module before optimizing, so the runtime helpers
// (`_CG_str_eq`, `_CG_ord`, `_CG_list_resize`, ...) inline into the program
// the way the C backend's do from pyc_c_runtime.h. Measured 2026-10-18 (as an
// lld LTO link) on 8 corpus programs: 17% faster (geomean), sha 47%, sieve
// 30%. PYC_LLVM_LTO=0 turns it off.
static cchar *llvm_runtime_bitcode() {
  static char path[FILENAME_MAX];
  if (cchar *e = getenv("PYC_LLVM_LTO")) if (!atoi(e)) return nullptr;
  cchar *dirs[] = {system_dir, "..", nullptr};
  for (int i = 0; dirs[i]; i++) {
    if (snprintf(path, sizeof(path), "%s/pyc_runtime_lto.o", dirs[i]) >= (int)sizeof(path)) continue;
    if (access(path, R_OK) == 0) return path;
  }
  return nullptr;
}

// Optimize the program's module and write it as a native object, in this
// process, with the LLVM pyc is linked against. pyc used to write the .ll
// and run `clang -O2 -c` on it, which needs a clang at least as new as that
// LLVM: textual IR (and bitcode) only reads forward. A released pyc carries
// its own LLVM, so the user's clang 18 refused the IR of pyc's LLVM 20
// (`getelementptr inbounds nuw`). Now the system compiler only LINKS a
// plain object, which any version does. Returns whether the runtime
// bitcode was linked in (then the program needs no libpyc_runtime.a).
// Optimize TheModule as clang -O2 with LTO would (below), and return the
// target machine it was optimized for. With `link_runtime`, the runtime
// bitcode is linked in and every definition but `main` internalized, and
// `*linked_runtime` says whether that happened. The JIT passes false: its
// code finds the runtime in the running pyc by name, so those calls stay
// external. Either way the pipeline lowers the coroutines (llvm.coro.*),
// which the code generator cannot: the JIT handed LLJIT an unoptimized
// module and aborted on any generator (ifa/211).
std::unique_ptr<llvm::TargetMachine> llvm_optimize_module(bool link_runtime, bool *linked_runtime) {
  llvm::Module &M = *TheModule;
  *linked_runtime = false;
  std::string triple_str = M.getTargetTriple()
#if LLVM_VERSION_MAJOR >= 22
                               .str()
#endif
      ;
  std::string err;
#if LLVM_VERSION_MAJOR >= 22
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(M.getTargetTriple(), err);
#else
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(triple_str, err);
#endif
  if (!target) fail("llvm_optimize_module: no target for %s: %s", triple_str.c_str(), err.c_str());
  llvm::TargetOptions options;

  // The runtime bitcode, read first: the CPU and features clang compiled it
  // for are the ones the program is compiled for too. That is clang's
  // default for the triple (no -march: the program runs anywhere the
  // triple does), and it must match, because the inliner declines a callee
  // whose features are not a subset of the caller's: with "generic" on
  // x86-64 no runtime helper inlined (sha made 281 calls into the runtime,
  // not 51, and ran 2x slower). On aarch64 clang's features
  // (+neon, +outline-atomics, ...) are not implied by any CPU name, so
  // they are copied onto the program's functions, as clang stamps them.
  std::unique_ptr<llvm::Module> runtime;
  if (cchar *rt = link_runtime ? llvm_runtime_bitcode() : nullptr) {
    llvm::SMDiagnostic diag;
    runtime = llvm::parseIRFile(rt, diag, *TheContext);
    // A bitcode file written by a newer clang than this LLVM does not parse;
    // the program then links libpyc_runtime.a and the helpers do not inline.
    if (!runtime) DEBUG_LOG("runtime bitcode %s does not parse; using libpyc_runtime.a\n", rt);
  }
  std::string cpu = llvm::Triple(triple_str).getArch() == llvm::Triple::x86_64 ? "x86-64" : "generic";
  std::string features, tune;
  if (runtime) {
    for (llvm::Function &f : *runtime) f.removeFnAttr("probe-stack");
    for (llvm::Function &f : *runtime) {
      if (f.isDeclaration() || !f.hasFnAttribute("target-cpu")) continue;
      cpu = f.getFnAttribute("target-cpu").getValueAsString().str();
      if (f.hasFnAttribute("target-features")) features = f.getFnAttribute("target-features").getValueAsString().str();
      if (f.hasFnAttribute("tune-cpu")) tune = f.getFnAttribute("tune-cpu").getValueAsString().str();
      break;
    }
  }
  for (llvm::Function &f : M) {
    if (f.isDeclaration()) continue;
    if (!f.hasFnAttribute("target-cpu")) f.addFnAttr("target-cpu", cpu);
    if (!features.empty() && !f.hasFnAttribute("target-features")) f.addFnAttr("target-features", features);
    if (!tune.empty() && !f.hasFnAttribute("tune-cpu")) f.addFnAttr("tune-cpu", tune);
  }
  std::unique_ptr<llvm::TargetMachine> tm(target->createTargetMachine(
#if LLVM_VERSION_MAJOR >= 22
      M.getTargetTriple(),
#else
      triple_str,
#endif
      cpu, features, options, llvm::Reloc::PIC_, std::nullopt, llvm::CodeGenOptLevel::Default));
  if (!tm) fail("llvm_optimize_module: cannot create a target machine for %s", triple_str.c_str());
  M.setDataLayout(tm->createDataLayout());
  // The same two rounds the lld LTO link ran: the program through clang
  // -O2 -flto's pre-link pipeline, then, with the runtime bitcode linked in,
  // the full LTO pipeline, which inlines the helpers. One -O2 round after
  // linking left sha making 94 calls into the runtime where LTO made 51.
  auto run = [&](bool lto) {
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;
    // clang -O2's tuning: the PassBuilder defaults leave SLP vectorization
    // off, which clang turns on at -O2.
    llvm::PipelineTuningOptions pto;
    pto.LoopUnrolling = true;
    pto.LoopInterleaving = true;
    pto.LoopVectorization = true;
    pto.SLPVectorization = true;
    llvm::PassBuilder PB(tm.get(), pto);
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
    llvm::ModulePassManager MPM = lto ? PB.buildLTODefaultPipeline(llvm::OptimizationLevel::O2, nullptr)
                                      : PB.buildLTOPreLinkDefaultPipeline(llvm::OptimizationLevel::O2);
    MPM.run(M, MAM);
  };
  run(false);

  if (runtime) {
    runtime->setTargetTriple(M.getTargetTriple());
    runtime->setDataLayout(M.getDataLayout());
    *linked_runtime = !llvm::Linker::linkModules(M, std::move(runtime), llvm::Linker::Flags::LinkOnlyNeeded);
    if (!*linked_runtime) DEBUG_LOG("runtime bitcode did not link; using libpyc_runtime.a\n");
  }
  // What the lld LTO link did as it resolved symbols for an executable: every
  // definition but `main` becomes internal, so a function with one caller
  // inlines into it and a dead one is deleted (ifa/200's "Also": external
  // linkage blocked both). Only with the runtime linked in: otherwise the
  // program's object must still reach libpyc_runtime.a by name, and that
  // is declarations, which this leaves alone.
  if (*linked_runtime)
    llvm::internalizeModule(M, [](const llvm::GlobalValue &gv) {
      return gv.getName() == "main" || exported_names.count(gv.getName().str());  // ifa/issues/212
    });
  run(true);
  // PYC_DUMP_OPT_LL=path: the module as optimized, before code generation.
  if (const char *dump = getenv("PYC_DUMP_OPT_LL")) {
    std::error_code dec;
    llvm::raw_fd_ostream dos(dump, dec);
    if (!dec) M.print(dos, nullptr);
  }
  return tm;
}

// Optimize the program's module and write it as a native object
// (llvm_optimize_module, then code generation). Returns whether the runtime
// bitcode was linked in (then the program needs no libpyc_runtime.a).
static bool llvm_emit_object(cchar *obj_file) {
  bool linked_runtime = false;
  std::unique_ptr<llvm::TargetMachine> tm = llvm_optimize_module(true, &linked_runtime);
  llvm::Module &M = *TheModule;
  std::string triple_str = M.getTargetTriple()
#if LLVM_VERSION_MAJOR >= 22
                               .str()
#endif
      ;
  std::error_code ec;
  llvm::raw_fd_ostream out(obj_file, ec, llvm::sys::fs::OF_None);
  if (ec) fail("llvm_emit_object: cannot write %s: %s", obj_file, ec.message().c_str());
  llvm::legacy::PassManager codegen;
  // The library functions the target has, as llc and clang give their code
  // generators.
  llvm::TargetLibraryInfoImpl tlii{llvm::Triple(triple_str)};
  codegen.add(new llvm::TargetLibraryInfoWrapperPass(tlii));
#if LLVM_VERSION_MAJOR >= 22
  // LLVM 22 keeps which runtime library calls exist in its own analysis. The
  // pass manager's default one knows none of the triple's, so the code
  // generator expanded every llvm.memcpy into an inline loop: sha's hot
  // function had 9 fewer memcpy calls and 50 more instructions than llc's
  // from the same IR, and ran 30% slower. Registered as llc registers it.
  codegen.add(new llvm::RuntimeLibraryInfoWrapper(llvm::Triple(triple_str), tm->Options.ExceptionModel,
                                                  tm->Options.FloatABIType, tm->Options.EABIVersion,
                                                  tm->Options.MCOptions.ABIName));
#endif
  if (tm->addPassesToEmitFile(codegen, out, nullptr, llvm::CodeGenFileType::ObjectFile))
    fail("llvm_emit_object: the target cannot emit an object file");
  codegen.run(M);
  out.flush();
  return linked_runtime;
}

// The Boehm GC the program links: the static archives an installed pyc
// carries in <system_dir>/gc/lib (`make dist BUNDLE_GC=1`), so the program
// needs no libgc where it is built or run, else the system's -lgc -lgccpp.
// Mirrors Makefile.cg's GC_BUNDLE for the C backend.
static void llvm_gc_link_args(std::vector<char *> &argv) {
  static std::string gc, gccpp;
  static int decided = 0;
  if (!decided) {
    decided = -1;
    std::string dir = std::string(system_dir) + "/gc/lib/";
    if (access((dir + "libgc.a").c_str(), R_OK) == 0) {
      decided = 1;
      gc = dir + "libgc.a";
      if (access((dir + "libgccpp.a").c_str(), R_OK) == 0) gccpp = dir + "libgccpp.a";
    }
  }
  if (decided > 0) {
    if (!gccpp.empty()) argv.push_back((char *)gccpp.c_str());
    argv.push_back((char *)gc.c_str());
    argv.push_back((char *)"-lpthread");
  } else {
    argv.push_back((char *)"-lgc");
    argv.push_back((char *)"-lgccpp");
  }
}

int llvm_codegen_compile(cchar *input_filename) {
  // Derive .ll / .o / executable paths from the input filename.
  // Bounded `snprintf` with fail-on-truncation so we never silently
  // corrupt paths longer than FILENAME_MAX.
  char ll_file[FILENAME_MAX], obj_file[FILENAME_MAX], exe_file[FILENAME_MAX];
  if (snprintf(ll_file, sizeof(ll_file), "%s", input_filename) >= (int)sizeof(ll_file))
    fail("llvm_codegen_compile: input filename too long: %s", input_filename);
  if (snprintf(obj_file, sizeof(obj_file), "%s", input_filename) >= (int)sizeof(obj_file))
    fail("llvm_codegen_compile: input filename too long: %s", input_filename);
  if (snprintf(exe_file, sizeof(exe_file), "%s", input_filename) >= (int)sizeof(exe_file))
    fail("llvm_codegen_compile: input filename too long: %s", input_filename);

  char *dot_ll = strrchr(ll_file, '.');
  if (dot_ll) strcpy(dot_ll, ".ll");
  else strcat(ll_file, ".ll");
  char *dot_o = strrchr(obj_file, '.');
  if (dot_o) strcpy(dot_o, ".o");
  else strcat(obj_file, ".o");
  char *dot_exe = strrchr(exe_file, '.');
  if (dot_exe) *dot_exe = '\0';

  // -o / --output: use the requested path verbatim for the final
  // executable instead of the one derived from the input filename.
  // The intermediate .ll/.o still land next to the input file.
  if (codegen_output[0]) {
    if (snprintf(exe_file, sizeof(exe_file), "%s", codegen_output) >= (int)sizeof(exe_file))
      fail("llvm_codegen_compile: -o path too long: %s", codegen_output);
  }

  // The compiler that links the object into an executable. C++ (not C)
  // since the coroutine-lowered code links against C++ runtime pieces.
  // The system's clang++ (Xcode's on macOS): it only links an object, which
  // any version does (llvm_emit_object), so a released pyc does not need
  // Homebrew's LLVM. PYC_LLVM_CXX overrides it.
  cchar *cxx = getenv("PYC_LLVM_CXX");
  if (!cxx) cxx = "clang++";

  // Extra link inputs: the frontend's extra translation unit (pyc's foreign
  // call shims), compiled here to its own object, then --libs and the
  // foreign libraries the program names. `extra` is spliced into both link
  // lines below, after the program's object.
  std::vector<char *> extra;
  if (codegen_extra_source) {
    static char src_file[FILENAME_MAX], src_obj[FILENAME_MAX];
    if (snprintf(src_file, sizeof(src_file), "%s.ffi.cc", input_filename) >= (int)sizeof(src_file) ||
        snprintf(src_obj, sizeof(src_obj), "%s.ffi.o", input_filename) >= (int)sizeof(src_obj))
      fail("llvm_codegen_compile: input filename too long: %s", input_filename);
    FILE *sfp = fopen(src_file, "w");
    if (!sfp) fail("llvm_codegen_compile: unable to write %s", src_file);
    fputs(codegen_extra_source, sfp);
    fclose(sfp);
    Vec<cchar *> cflags;
    codegen_split_flags(codegen_extra_cflags, cflags);
    std::vector<char *> sargv = {(char *)cxx, (char *)"-O2", (char *)"-c", (char *)"-fPIC", (char *)"-std=c++23"};
    for (cchar *f : cflags) sargv.push_back((char *)f);
    sargv.insert(sargv.end(), {src_file, (char *)"-o", src_obj, nullptr});
    int res = codegen_spawn(cxx, sargv.data());
    if (res != 0) fail("llvm_codegen_compile: compiling %s failed (exit=%d)", src_file, res);
    extra.push_back(src_obj);
  }
  {
    Vec<cchar *> libs;
    codegen_split_flags(codegen_extra_libs, libs);
    for (cchar *l : codegen_link_libs) libs.add(l);
    for (cchar *l : libs) extra.push_back((char *)l);
  }

  // The object, written here (llvm_emit_object), then linked by the system
  // compiler: `cxx <obj> -o <exe> [-L<system_dir> -lpyc_runtime] -lm <gc>`.
  //
  // The GC (llvm_gc_link_args): the IR from P_prim_make / P_prim_new /
  // P_prim_clone calls GC_malloc (issue 012). libpyc_runtime.a holds the
  // runtime helpers out of line, for when the runtime bitcode was not linked
  // into the module; -L<system_dir> is where it is installed, -L.. where
  // `make test_llvm` in ifa/ finds it in the source tree.
  bool linked_runtime = llvm_emit_object(obj_file);
  DEBUG_LOG("LLVM IR from %s compiled to %s%s\n", ll_file, obj_file,
            linked_runtime ? " with the runtime bitcode" : "");
  {
    char libdir_arg[FILENAME_MAX];
    int n = snprintf(libdir_arg, sizeof(libdir_arg), "-L%s", system_dir);
    if (n < 0 || (size_t)n >= sizeof(libdir_arg))
      fail("llvm_codegen_compile: -L<system_dir> arg too long");
    std::vector<char *> argv = {(char *)cxx, obj_file};
    argv.insert(argv.end(), extra.begin(), extra.end());
    argv.insert(argv.end(), {(char *)"-o", exe_file, (char *)"-L/opt/homebrew/lib", (char *)"-L/usr/local/lib"});
    if (!linked_runtime) argv.insert(argv.end(), {libdir_arg, (char *)"-L..", (char *)"-lpyc_runtime"});
    argv.push_back((char *)"-lm");
#ifdef __linux__
    // the runtime dlopens libcrypto for hashlib; dlopen is in libc only
    // from glibc 2.34 (Makefile.cg's C-backend link has the same -ldl)
    argv.push_back((char *)"-ldl");
#endif
    llvm_gc_link_args(argv);
#ifdef __APPLE__
    argv.push_back((char *)"-stdlib=libc++");
#endif
    argv.push_back(nullptr);
    int res = codegen_spawn(cxx, argv.data());
    if (res != 0) {
      fail("llvm_codegen_compile: linking failed for %s (exit=%d)", obj_file, res);
      return res;
    }
  }
  DEBUG_LOG("Executable %s created successfully\n", exe_file);
  return 0;
}
