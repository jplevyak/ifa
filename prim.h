/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#ifndef _prim_H_
#define _prim_H_

class Prim;
class IF1;
class Code;
class AST;
class ParseAST;
class PNode;
class AType;
class EntrySet;
class Fun;

// all registered primitives are mapped to prim_primitive

typedef void (*PrimitiveTransferFunctionPtr)(PNode *pn, EntrySet *es);
typedef void (*PrimitiveCGPtr)(FILE *fp, PNode *n, Fun *f);

class RegisteredPrim : public gc {
 public:
  PrimitiveTransferFunctionPtr tfn;
  PrimitiveCGPtr cgfn;
  uint is_functional : 1;
  uint is_visible : 1;
  RegisteredPrim(PrimitiveTransferFunctionPtr atfn, PrimitiveCGPtr acgfn)
      : tfn(atfn), cgfn(acgfn), is_functional(1), is_visible(0) {}
};

class Primitives : public gc {
 public:
  Map<cchar *, Prim *> prim_map[3][2];
  Vec<Prim *> prims;
  ChainHashMap<cchar *, StringHashFns, RegisteredPrim *> registered_prims;

  Prim *find(int nargs, Sym *a0, Sym *a1, Sym *a2 = 0);
  Prim *find(Code *c);
  Prim *find(PNode *p);

  Primitives(IF1 *if1);
};

enum PrimType {
  PRIM_TYPE_ALL,
  PRIM_TYPE_ANY,
  PRIM_TYPE_SYMBOL,
  PRIM_TYPE_STRING,
  PRIM_TYPE_TUPLE,
  PRIM_TYPE_REF,
  PRIM_TYPE_CONT,
  PRIM_TYPE_A,
  PRIM_TYPE_ANY_NUM_A,
  PRIM_TYPE_ANY_NUM_B,
  PRIM_TYPE_ANY_NUM_AB,
  PRIM_TYPE_ANY_INT_A,
  PRIM_TYPE_ANY_INT_B,
  PRIM_TYPE_BOOL,
  PRIM_TYPE_SIZE
};

enum PrimOptions { PRIM_NON_FUNCTIONAL = 1 };

class Prim : public gc {
 public:
  int index;
  cchar *string;
  cchar *name;
  int nargs;  // -n means at least n
  int nrets;
  int pos;  // position of primitive symbol
  uint nonfunctional : 1;
  PrimType *arg_types;  // vector excluding primitive symbol
  PrimType *ret_types;
  Vec<AType *> args;
  Prim(int aindex, cchar *astring, cchar *aname, int anargs, int apos, int anrets, PrimType *aarg_types,
       PrimType *aret_types, int options);
};
#define forv_Prim(_c, _v) forv_Vec(Prim, _c, _v)

void prim_init(Primitives *p, IF1 *if1);
RegisteredPrim *prim_reg(cchar *name, PrimitiveTransferFunctionPtr ptrfn, PrimitiveCGPtr pcgfn = 0);
RegisteredPrim *prim_get(cchar *name);

#include "prim_data.h"

#endif
