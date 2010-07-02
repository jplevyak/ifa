/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#include "builtin.h"

#define S(_n) Sym* sym_##_n = 0;
#include "builtin_symbols.h"
#undef S

