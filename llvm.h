/* -*-Mode: c++;-*-
   Copyright (c) 2023 John Plevyak, All Rights Reserved
*/
#ifndef _llvm_h_
#define _llvm_h_

#include "fa.h"

void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main);
void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *filename);
int llvm_codegen_compile(cchar *filename);

#endif
