#include "llvm_internal.h"
#include "cg_ir_v2.h"
#include "cg_normalize_v2.h"
#include "cg_view.h"  // Phase A diff oracle (CG_VIRTUAL_PLAN)
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
#include <stdarg.h>
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
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  // llvm::InitializeAllAsmParsers(); // Not strictly needed for IR generation to .ll
  llvm::InitializeAllAsmPrinters();  // Needed for object file emission if done via PassManager

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

  // Set target triple for the module
  std::string TargetTriple = llvm::sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(llvm::Triple(TargetTriple));

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
#define S(_n) cg_set_string(if1_get_builtin(fa->pdb->if1, #_n), "_CG_" #_n);
#include "builtin_symbols.h"
#undef S
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

  // LLVM codegen — single path (cg_normalize_v2 + v2 emit).
  // The v1 LLVM path (createGlobalVariables + createFunction +
  // cg_normalize + translateFunctionBody) was retired alongside
  // issue 014: v2 LLVM strictly subsumes v1 (every test that
  // passed under v1 also passes under v2; v1 16/59, v2 80/0 at
  // retirement), so keeping the legacy translator alive only
  // diluted the maintenance surface.  IFA_LLVM_V2 is no longer
  // consulted — there's only the v2 path.
  CGv2Program *v2prog = cg_normalize_v2(fa);
  if (!v2prog) {
    fail("cg_normalize_v2 returned null");
    return;
  }
  // CG_VIRTUAL_PLAN Phase C.1: PYC_LLVM_VIEW=1 selects
  // the view-driven emit (rebuilds prog's bodies from
  // the view enumeration, then runs the existing LLVM
  // emit machinery).  Default is the materialized path
  // until Phase C closes the CGInstView::kind()
  // classification gaps reported by B.6's diff oracle.
  bool ok = getenv("PYC_LLVM_VIEW")
                ? cg_v2_emit_llvm_module_view(fa, v2prog)
                : cg_v2_emit_llvm_module(v2prog);
  if (!ok) {
    fail("cg_v2_emit_llvm_module returned false");
    return;
  }
  // CG_VIRTUAL_PLAN Phase A: compare the view-side
  // classification of every live PNode with the
  // materialized CGv2Program's per-instruction op
  // counts.  Histogram-level match is the Phase A
  // correctness check; bin-level disagreement reveals
  // classification gaps the views haven't yet covered.
  // Enable with PYC_VIEW_DIFF=1.
  if (getenv("PYC_VIEW_DIFF")) {
    cg_view_diff_report(fa, v2prog);
  }
  // CG_VIRTUAL_PLAN Phase B.6: instruction-level diff of
  // view-side enumeration vs materialized CGv2Insts.  This
  // is the safety net Phase D relies on — divergences mean
  // the view-driven emit (Phase C) would produce different
  // LLVM IR than the materialized path.  Phase B/C reports
  // divergences to stderr but does not fail the compile;
  // Phase D promotes a non-zero count to a hard error once
  // the materialized side is removed.  Enable with
  // PYC_LLVM_DIFF=1.
  if (getenv("PYC_LLVM_DIFF")) {
    cg_view_diff_module(fa, v2prog, input_filename);
  }
  // Populate f->llvm cache so the synthesized C main() wrapper
  // below can resolve the IF1 main_fun via cg_get_llvm(f).
  for (Fun *f : fa->funs) {
    if (!f || !f->cg_string) continue;
    if (llvm::Function *fn = TheModule->getFunction(f->cg_string)) {
      f->llvm = fn;
    }
  }

  // TODO: Implement PNode translation for each function's body
  // For now, functions will be declared but not defined (if not external)

  // Create a simple main function that calls the IF1 main_fun
  // This assumes the IF1 main_fun doesn't take argc, argv
  llvm::FunctionType *main_func_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(*TheContext), false);
  llvm::Function *llvm_main =
      llvm::Function::Create(main_func_type, llvm::Function::ExternalLinkage, "main", TheModule.get());
  llvm::BasicBlock *main_entry_bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_main);
  Builder->SetInsertPoint(main_entry_bb);

  if (cg_get_llvm(main_fun)) {  // If IF1 main function was generated
    Builder->CreateCall(cg_get_llvm(main_fun));
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
      if (!BB.getTerminator()) {
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
  // per-function attribution under --strict-verify.
  std::string error_str;
  llvm::raw_string_ostream rso(error_str);
  bool broken = llvm::verifyModule(*TheModule, &rso);

  // --strict-verify (PYC_STRICT_VERIFY=1): scan user functions
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
  // defs.h globals (the --strict-verify flag in pyc.cc sets the
  // env so both flag forms work).
  const char *sv = getenv("PYC_STRICT_VERIFY");
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
      rso << "  strict-verify: " << undef_count
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
  DEBUG_LOG("LLVM IR written to %s\n", fn);
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

  // Step 1: clang -c -fPIC <ll> -o <obj>
  {
    char *argv[] = {(char *)"clang", (char *)"-c", (char *)"-fPIC", ll_file, (char *)"-o", obj_file, nullptr};
    int res = codegen_spawn("clang", argv);
    if (res != 0) {
      fail("llvm_codegen_compile: clang -c failed for %s (exit=%d)", ll_file, res);
      return res;
    }
  }
  DEBUG_LOG("LLVM IR from %s compiled to %s\n", ll_file, obj_file);

  // Step 2: clang <obj> -o <exe> -L<system_dir> -lpyc_runtime
  //         -lm -lgc -lgccpp
  //
  // -lgc / -lgccpp: the IR emitted by P_prim_make / P_prim_new /
  // P_prim_clone references GC_malloc (Boehm GC). Without these the
  // link fails on undefined `GC_malloc`. See issue 012.
  //
  // -lpyc_runtime (D.3.5): out-of-line copies of the runtime
  // helpers (`_CG_string_alloc`, `_CG_chr`, `_CG_str_eq`, ...). The
  // C backend gets these via `#include "pyc_c_runtime.h"` per
  // generated .py.c; the LLVM backend produces a single .o that
  // references them as external symbols, so we link the pyc
  // runtime library here. -L points at the install location (the
  // same `system_dir` we use for Makefile.cg and __pyc__).
  {
    char libdir_arg[FILENAME_MAX];
    int n = snprintf(libdir_arg, sizeof(libdir_arg), "-L%s", system_dir);
    if (n < 0 || (size_t)n >= sizeof(libdir_arg))
      fail("llvm_codegen_compile: -L<system_dir> arg too long");
    char *argv[] = {(char *)"clang",         obj_file,
                    (char *)"-o",            exe_file,
                    libdir_arg,
                    (char *)"-lpyc_runtime",
                    (char *)"-lm",           (char *)"-lgc",
                    (char *)"-lgccpp",       nullptr};
    int res = codegen_spawn("clang", argv);
    if (res != 0) {
      fail("llvm_codegen_compile: linking failed for %s (exit=%d)", obj_file, res);
      return res;
    }
  }
  DEBUG_LOG("Executable %s created successfully\n", exe_file);
  return 0;
}
