// SPDX-License-Identifier: BSD-3-Clause
//
// jit.cc — ORCv2 LLJIT runner for the pyc JIT path (-j flag).
//
// Takes TheModule (already built and verified by llvm_codegen_print_ir)
// and runs it in-process via LLVM's ORCv2 LLJIT engine.
//
// Key contracts:
//   • Moves TheModule / TheContext into a ThreadSafeModule (transfers
//     ownership; TheModule is null after this call).
//   • Resolves runtime symbols (GC_malloc, _CG_string_alloc, etc.) via
//     DynamicLibrarySearchGenerator — these must be exported by the pyc
//     binary (Makefile links pyc_runtime.o + -rdynamic for this).
//   • Runs the "main" wrapper (emitted by llvm_codegen_print_ir) in a
//     fork() so that exit() in the Python code doesn't kill pyc itself.
//
// Stage-2 REPL cache:
//   If llvm_jit_cache_path is set (by repl.cc), the compiled module is
//   serialized as LLVM bitcode before being handed to the JIT.  A later
//   run can skip FA+codegen entirely by calling llvm_jit_read_cache().

#include "llvm_internal.h"
#include "fail.h"

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetSelect.h"

#include <cerrno>
#include <cstring>
#include <sys/wait.h>
#include <unistd.h>

// When non-empty, llvm_jit_execute() serializes TheModule here as LLVM
// bitcode before transferring it to the JIT.  Set by repl.cc; reset to
// '\0' after the write so a second call is a no-op.
char llvm_jit_cache_path[512] = {};

// Load a cached bitcode file into TheModule / TheContext.
// Returns true on success; false on any I/O or parse error (caller should
// fall back to the full FA+codegen pipeline).
bool llvm_jit_read_cache(const char *path) {
  auto mb = llvm::MemoryBuffer::getFile(path);
  if (!mb) return false;
  auto ctx = std::make_unique<llvm::LLVMContext>();
  auto mod = llvm::parseBitcodeFile((*mb)->getMemBufferRef(), *ctx);
  if (!mod) return false;
  TheContext = std::move(ctx);
  TheModule  = std::move(*mod);
  return true;
}

int llvm_jit_execute() {
  // Ensure LLVM knows about the native target.  This is a no-op when the
  // full pipeline already called llvm_codegen_initialize(), and required
  // when the cache-hit path skips that function entirely.
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // Build the LLJIT engine.  setNumCompileThreads(0) keeps compilation
  // single-threaded so forking afterward is safe (no live threads to
  // inherit in the child).
  auto jit_exp = llvm::orc::LLJITBuilder().setNumCompileThreads(0).create();
  if (!jit_exp)
    fail("JIT: %s", llvm::toString(jit_exp.takeError()).c_str());
  auto &jit = *jit_exp;

  // Expose all process symbols — GC (already linked into pyc), pyc_runtime
  // helpers (linked in via pyc_runtime.o + -rdynamic in Makefile).
  char prefix = jit->getDataLayout().getGlobalPrefix();
  auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(prefix);
  if (!gen)
    fail("JIT: DynamicLibrarySearchGenerator: %s", llvm::toString(gen.takeError()).c_str());
  jit->getMainJITDylib().addGenerator(std::move(*gen));

  // Write bitcode cache before ownership is transferred (one-shot).
  if (llvm_jit_cache_path[0] && TheModule) {
    std::error_code ec;
    llvm::raw_fd_ostream os(llvm_jit_cache_path, ec, llvm::sys::fs::OF_None);
    if (!ec) llvm::WriteBitcodeToFile(*TheModule, os);
    llvm_jit_cache_path[0] = '\0';
  }

  // Transfer ownership of TheModule into the JIT.
  // After this call TheModule and TheContext are null — the .ll file
  // written by llvm_codegen_write_ir is the only remaining IR artifact.
  auto tsm = llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext));
  if (auto err = jit->addIRModule(std::move(tsm)))
    fail("JIT addIRModule: %s", llvm::toString(std::move(err)).c_str());

  // Resolve the "main" entry point emitted by llvm_codegen_print_ir.
  auto sym = jit->lookup("main");
  if (!sym)
    fail("JIT lookup 'main': %s", llvm::toString(sym.takeError()).c_str());
  auto entry = sym->toPtr<int(*)(int, char **)>();

  // Fork so that exit() (or an abort/signal) from Python code doesn't
  // kill the pyc process.  The child inherits the JIT's mapped pages
  // copy-on-write and runs the entry directly.
  pid_t pid = fork();
  if (pid < 0)
    fail("JIT: fork() failed: %s", strerror(errno));
  if (pid == 0)
    exit(entry(0, nullptr));

  int status = 0;
  if (waitpid(pid, &status, 0) < 0)
    fail("JIT: waitpid failed: %s", strerror(errno));
  return WIFEXITED(status) ? WEXITSTATUS(status) : 1;
}
