/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "prim.h"
#include "if1.h"
#include "builtin.h"
#include "pnode.h"
#include "var.h"
#include "ast.h"

Prim::Prim(int aindex, cchar *astring, cchar *aname, int anargs, int apos, int anrets,
           PrimType *aarg_types, PrimType *aret_types, int options) {
  index = aindex;
  string = astring;
  name = aname;
  nargs = anargs;
  pos = apos;
  nrets = anrets;
  arg_types = aarg_types;
  ret_types = aret_types;
  nonfunctional = options & PRIM_NON_FUNCTIONAL;
}

Primitives::Primitives(IF1 *if1) {
  prim_init(this, if1);
}

Prim *Primitives::find(int nargs, Sym *f, Sym *a1, Sym *a2) {
  Prim *prim = 0;
  if (f == sym_operator) {
    nargs = nargs - 3;
    if (nargs < 0)
      nargs = 0;
    if (a1->is_symbol)
      prim = prim_map[nargs][0].get(a1->name);
    else {
      assert(a2->is_symbol);
      prim = prim_map[nargs][1].get(a2->name);
    }
    assert(prim);
  } else if (f == sym_primitive) {
    assert(a1->is_symbol);
    prim = prim_map[0][0].get(a1->name);
    if (!prim) prim = prim_primitive;
  }
  return prim;
}

Prim *
Primitives::find(Code *c) {
  if (c->kind != Code_SEND || c->rvals.n < 2)
    return 0;
  return find(c->rvals.n, c->rvals[0], c->rvals[1], c->rvals.n < 3 ? 0 : c->rvals[2]);
}

Prim *
Primitives::find(PNode *p) {
  if (p->rvals.n < 2)
    return 0;
  return find(p->rvals.n, p->rvals[0]->sym, p->rvals[1]->sym, 
              p->rvals.n < 3 ? 0 : p->rvals[2]->sym);
}

RegisteredPrim *prim_reg(cchar *name, PrimitiveTransferFunctionPtr ptr, PrimitiveCGPtr pcg) {
  RegisteredPrim *p = new RegisteredPrim(ptr, pcg);
  if1->primitives->registered_prims.put(name, p);
  return p;
}

RegisteredPrim *prim_get(cchar *name) {
  return if1->primitives->registered_prims.get(name);
}

