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
class DILocalVariable;
}  // namespace llvm

typedef MapElem<void *, AVar *> AVarMapElem;
typedef Map<void *, AVar *> AVarMap;
typedef Map<Var *, Var *> VarMap;

extern int var_id;

class Var : public gc {
 public:
  Sym *sym;
  int id;
  Sym *type;
  int mark;  // used by ssu.cpp
  PNode *def;
  Vec<PNode *> uses;
  AVarMap avars;                // used by fa.cpp
  CreationSet *as_CreationSet;  // used by fa.cpp
  unsigned int is_internal : 1;
  unsigned int is_filtered : 1;
  unsigned int is_formal : 1;
  unsigned int live : 1;
  Sym *constant;     // valid after dead code elimination
  cchar *cg_string;  // used by cg.cpp
  llvm::Value *llvm_value;
  llvm::Type *llvm_type;
  llvm::DILocalVariable *llvm_debug_var;  // Debug info variable

  // Temporary Space
  union {
    SSUVar *ssu;
  };

  Var *copy();
  Var(Sym *s);
};

// Content-based hashing for `Vec<Var*>::set_add` / `set_in`. `id` is
// monotonically assigned at construction (var.cc), so iteration order
// is deterministic across runs (see ifa/notes/004; this specialization
// was the one id-bearing pointer type missing from that landing).
template <> struct PointerHash<Var *> {
  static uintptr_t hash(Var *c) { return c ? (uintptr_t)c->id : 0; }
};

#define form_AVarMapElem(_p, _v) form_Map(AVarMapElem, _p, _v)

void pp(Var *);

#endif
