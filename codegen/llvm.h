#ifndef _llvm_h_
#define _llvm_h_

#include "fa.h"

void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main, cchar *input_filename);
void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *filename);
int llvm_codegen_compile(cchar *filename);

// Pyc-frontend RegisteredPrim LLVM cgfns (phase 3.2 of CODEGEN_PLAN).
// The pyc frontend registers these via prim_reg's third argument so
// the LLVM `P_prim_primitive` dispatcher can route `write` / `writeln`
// through them instead of falling back to write_send (which has no
// target Fun for primitive calls).
class PNode;
class Fun;
void pyc_llvm_write_cgfn(PNode *n, Fun *f);
void pyc_llvm_writeln_cgfn(PNode *n, Fun *f);

#endif
