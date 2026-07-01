#ifndef _llvm_h_
#define _llvm_h_

#include "fa.h"

void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main, cchar *input_filename);
void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *filename);
int llvm_codegen_compile(cchar *filename);

// JIT-execute the module built by the most recent llvm_codegen_write_ir
// call.  Runs the "main" wrapper in a fork() so that exit() from Python
// code does not kill the pyc process.  Returns the child's exit status
// (0 = success).  Requires pyc to be linked with pyc_runtime.o and
// -rdynamic so that runtime helpers are visible to GetForCurrentProcess.
// If llvm_jit_cache_path is non-empty, writes LLVM bitcode to that path
// before handing the module to the JIT (Stage-2 REPL cache protocol).
int llvm_jit_execute();

// Stage-2 REPL bitcode cache.
// llvm_jit_cache_path: set by the caller (repl.cc) before calling
//   llvm_jit_execute() on a cache-miss; cleared after the write.
// llvm_jit_read_cache: loads a previously written .bc file into
//   TheModule/TheContext; returns true on success so the caller can
//   skip FA+codegen and call llvm_jit_execute() directly.
extern char llvm_jit_cache_path[512];
bool llvm_jit_read_cache(const char *path);

// `pyc_llvm_write_cgfn` / `pyc_llvm_writeln_cgfn` /
// `pyc_llvm_to_string_cgfn` retired alongside the v1 LLVM
// backend (issue 014).  v2 LLVM's lower_send_prim routes
// `write`/`writeln` through libpyc_runtime.a's `_CG_write` /
// `_CG_writeln`, and `to_string` is no longer reachable from
// pyc-Python source (replaced by int.__str__ /
// float.__str__ in __pyc__/02_numeric.py).

#endif
