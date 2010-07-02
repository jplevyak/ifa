/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _clone_h_
#define _clone_h_

#include "ifadefs.h"

class FA;
class EntrySet;
class AType;

int clone(FA *fa);
Sym *basic_type(FA *fa, AType *t, Sym *fail);
void log_test_fa(FA *fa);
Sym *to_basic_type(Sym *);
Sym *concrete_type_set_to_type(Vec<Sym *> &t);


#endif
