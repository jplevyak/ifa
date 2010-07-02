/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/

#ifndef _cg_h_
#define _cg_h_

#include "fa.h"

void c_codegen_print_c(FILE *fp, FA *fa, Fun *main);
void c_codegen_write_c(FA *fa, Fun *main, cchar *filename);
int c_codegen_compile(cchar *filename);

#endif
