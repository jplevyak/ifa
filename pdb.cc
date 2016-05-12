/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"

#include "pdb.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"

PDB *pdb = 0;

PDB::PDB(IF1 *aif1) {
  if1 = aif1;
  pdb = this;
  fa = new FA(this);
}

Sym *PDB::find_global(char *s) { return if1_get_builtin(if1, s); }

void PDB::add(Fun *f) {
  f->id = funs.n;
  funs.add(f);
}
