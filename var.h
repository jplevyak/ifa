/* -*-Mode: c++;-*-
   Copyright (c) 2003-2010 John Plevyak, All Rights Reserved
*/
#ifndef _var_H_
#define _var_H_

class AVar;
class CreationSet;
class AType;
class Sym;
class PNode;
class SSUVar;
class CreationSet;
namespace llvm {
class Value;
class Type;
}

typedef MapElem<void *, AVar *> AVarMapElem;
typedef Map<void *, AVar *> AVarMap;
typedef Map<Var *, Var *> VarMap;

extern int var_id;

class Var : public gc {
 public:
  Sym                   *sym;
  int                   id;
  Sym                   *type;
  int                   mark;   // used by ssu.cpp
  PNode                 *def;
  Vec<PNode *>          uses;
  AVarMap               avars;  // used by fa.cpp
  CreationSet           *as_CreationSet; // used by fa.cpp
  unsigned int          is_internal:1;
  unsigned int          is_filtered:1;
  unsigned int          is_formal:1;
  unsigned int          live:1;
  Sym                   *constant;  // valid after dead code elimination
  cchar                 *cg_string; // used by cg.cpp
  llvm::Value           *llvm_value;
  llvm::Type            *llvm_type;

  // Temporary Space
  union {
    SSUVar *ssu;
  };

  Var *copy();
  Var(Sym *s);
};
#define forv_Var(_p, _v) forv_Vec(Var, _p, _v)
#define form_AVarMapElem(_p, _v) form_Map(AVarMapElem, _p, _v)

void pp(Var *);

#endif
