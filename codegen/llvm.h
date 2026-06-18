#ifndef _llvm_h_
#define _llvm_h_

#include "fa.h"

void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main, cchar *input_filename);
void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *filename);
int llvm_codegen_compile(cchar *filename);

// `pyc_llvm_write_cgfn` / `pyc_llvm_writeln_cgfn` /
// `pyc_llvm_to_string_cgfn` retired alongside the v1 LLVM
// backend (issue 014).  v2 LLVM's lower_send_prim routes
// `write`/`writeln` through libpyc_runtime.a's `_CG_write` /
// `_CG_writeln`, and `to_string` is no longer reachable from
// pyc-Python source (replaced by int.__str__ /
// float.__str__ in __pyc__/02_numeric.py).

#endif
