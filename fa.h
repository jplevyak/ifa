/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _fa_H_
#define _fa_H_

#include <sys/types.h>
#include "ifadefs.h"
#include "sym.h"
#include "code.h"
#include "prim.h"

#define DEFAULT_NUM_CONSTANTS_PER_VARIABLE      1
#define IFA_PASS_LIMIT                          100

#define GLOBAL_CONTOUR ((void*)1)

class Prim;
class RegisteredPrim;
class Fun;
class PNode;
class PDB;
class Edge;
class AVar;
class AEdge;
class AType;
class SettersClasses;
class SettersHashFns;
class Setters;
class CreationSet;
class ATypeViolation;
class CDB;
class Patterns;
class MatchCache;
class MPosition;
class EntrySet;

typedef Map<PNode *, Vec<AEdge *> *> EdgeMap;
typedef BlockHash<AEdge *, PointerHashFns> EdgeHash;
typedef Vec<CreationSet *> VecCreationSet;
typedef Vec<Vec<CreationSet *> *> CSSS;

class AType : public Vec<CreationSet *> { public:
  uint                  hash;
  AType                 *type;          // not including values (constants)
  Vec<CreationSet *>    sorted;
  Map<AType *, AType *> union_map;
  Map<AType *, AType *> intersection_map;
  Map<AType *, AType *> diff_map;

  AType(CreationSet *cs);
  AType(AType &a);
  AType() : hash(0) {}

  AType *constants();
};
#define forv_AType(_p, _v) forv_Vec(AType, _p, _v)

class AEdge : public gc {public:
  int                    id;
  EntrySet               *from, *to;
  PNode                  *pnode;
  Map<MPosition*,AVar*>  args;
  Map<MPosition*,AVar*>  filtered_args;
  Map<MPosition*,AType*> initial_types;
  Vec<AVar *>            rets;
  Fun                    *fun;
  Match                  *match;
  uint                   in_edge_worklist : 1;
  uint                   es_backedge : 1;
  uint                   es_cs_backedge : 1;
  LINK(AEdge,            edge_worklist_link);

  AEdge();
};
#define forv_AEdge(_p, _v) forv_Vec(AEdge, _p, _v)

class PendingMapHash { public:
  static uint hash(AEdge *e) { 
    return (uint)((uintptr_t)e->fun + (13 * (uintptr_t)e->pnode) + (100003 * (uintptr_t)e->from));
  }
  static int equal(AEdge *a, AEdge *b) {
    return (a->fun == b->fun) && (a->pnode == b->pnode) && (a->from == b->from);
  }
};

typedef HashMap<AEdge *, PendingMapHash, Vec<EntrySet *> *> PendingAEdgeEntrySetsMap;
typedef MapElem<AEdge *, Vec<EntrySet *> *> MapElemAEdgeEntrySets;

enum { DFS_white = 0, DFS_grey, DFS_black };

class EntrySet : public gc { public:
  Fun                                   *fun;
  int                                   id;
  uint                                  dfs_color : 2;
  Map<MPosition*,AVar*>                 args;
  Vec<AVar *>                           rets;
  Map<MPosition *, AType *>             filters;
  EdgeHash                              edges;
  EdgeMap                               out_edge_map;
  Vec<CreationSet *>                    creates;
  Vec<EntrySet *>                       display;
  Vec<AEdge *>                          out_edges;
  Vec<AEdge *>                          backedges;
  Vec<AEdge *>                          es_cs_backedges;
  Vec<CreationSet *>                    cs_backedges;
  EntrySet                              *split;
  PendingAEdgeEntrySetsMap              pending_es_backedge_map;
  Vec<EntrySet *>                       *equiv;         // clone.cpp

  EntrySet(Fun *af);
};
#define forv_EntrySet(_p, _v) forv_Vec(EntrySet, _p, _v)

class CreationSet : public gc { public:
  Sym                   *sym;
  int                   id;
  uint                  dfs_color : 2;
  uint                  clone_for_constants : 1;
  uint                  added_element_var : 1;
  uint                  closure_used : 1;
  uint                  tuple_able : 1;
  Vec<AVar *>           defs;
  AType                 *atype;         // the type that this creation set belongs to
  Vec<AVar *>           vars;
  Map<cchar *, AVar *>  var_map;
  Vec<EntrySet *>       ess;            // entry sets restricted by this creation set
  Vec<EntrySet *>       es_backedges;   // entry sets restricted by this creation set
  CreationSet           *split;         // creation set this one was split from
  Vec<CreationSet *>    *equiv;         // used by clone.cpp & fa.cpp
  Vec<CreationSet *>    not_equiv;      // used by clone.cpp
  Sym                   *type;          // used by clone.cpp & fa.capp

  CreationSet(Sym *s);
  CreationSet(CreationSet *cs);
};
#define forv_CreationSet(_p, _v) forv_Vec(CreationSet, _p, _v)

class SettersClasses : public Vec<Setters *> { public:
  uint                                  hash;
  Vec<Setters *>                        sorted;
};
#define forv_SettersClasses(_p, _v) forv_Vec(SettersClasses, _p, _v)

class Setters : public Vec<AVar *> { public:
  uint                  hash;
  Vec<AVar *>           sorted;
  SettersClasses        *eq_classes;
  Map<AVar *, Setters*> add_map;

  Setters() : hash(0), eq_classes(0) { }
};
#define forv_Setters(_p, _v) forv_Vec(Setters, _p, _v)

typedef MapElem<void *, int> MarkElem;
typedef Map<void *, int> MarkMap;
typedef Map<Sym *, CreationSet *> CSMap;
typedef MapElem<Sym *, CreationSet *> CSMapElem;

class AVar : public gc { public:
  Var                           *var;
  int                           id;
  void                          *contour;
  Vec<AVar *>                   forward;
  Vec<AVar *>                   backward;
  AVar                          *lvalue;
  AType                         *gen;
  AType                         *in;
  AType                         *out;
  AType                         *restrict;
  AVar                          *container;
  Setters                       *setters;
  Setters                       *setter_class;
  MarkMap                       *mark_map;
  CSMap                         *cs_map;
  MatchCache                    *match_cache;
  Sym                           *type;
  int                           ivar_offset;
  uint                          in_send_worklist:1;
  uint                          contour_is_entry_set:1;
  uint                          is_lvalue:1;
  uint                          live:1;
  Accum<AVar *>                 arg_of_send;
  LINK(AVar,                    send_worklist_link);

  AVar(Var *v, void *acontour);
};
#define forv_AVar(_p, _v) forv_Vec(AVar, _p, _v)

typedef Map<MPosition*, AVar *> MapMPositionAVar;
typedef MapElem<MPosition *, AVar *> MapMPositionAVarElem;
#define form_MPositionAVar(_p, _v) form_Map(MapMPositionAVarElem, _p, _v)

class ATypeChainHashFns { public:
  static uint hash(AType *a) { return a->hash; }
  static int equal(AType *a, AType *b) {
    if (a->sorted.n != b->sorted.n)
      return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i])
        return 0;
    return 1;
  }
};

class SettersHashFns { public:
  static uint hash(Setters *a) { return a->hash; }
  static int equal(Setters *a, Setters *b) {
    if (a->sorted.n != b->sorted.n)
      return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i])
        return 0;
    return 1;
  }
};

class SettersClassesHashFns { public:
  static uint hash(SettersClasses *a) { return a->hash; }
  static int equal(SettersClasses *a, SettersClasses *b) {
    if (a->sorted.n != b->sorted.n)
      return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i])
        return 0;
    return 1;
  }
};

enum ATypeViolation_kind {
  ATypeViolation_PRIMITIVE_ARGUMENT,
  ATypeViolation_SEND_ARGUMENT,
  ATypeViolation_DISPATCH_AMBIGUITY,
  ATypeViolation_MEMBER,
  ATypeViolation_MATCH,
  ATypeViolation_NOTYPE,
  ATypeViolation_BOXING,
  ATypeViolation_CLOSURE_RECURSION
};
  
class ATypeViolation : public gc { public:
  ATypeViolation_kind kind;
  AVar *av;
  AVar *send;
  AType *type;
  Vec<Fun *> *funs;

  ATypeViolation(ATypeViolation_kind akind, AVar *aav, AVar *asend) 
    : kind(akind), av(aav), send(asend), type(0), funs(0) {}
};
#define forv_ATypeViolation(_p, _v) forv_Vec(ATypeViolation, _p, _v)

class ATypeViolationHashFuns { public:
  static uint hash(ATypeViolation *x) { 
    return (uint)((uint)x->kind + (13 * (uintptr_t)x->av) + (100003 * (uintptr_t)x->send));
  }
  static int equal(ATypeViolation *x, ATypeViolation *y) {
    return x->kind == y->kind && x->av == y->av && x->send == y->send;
  }
};

class ATypeFold : public gc { public:
  Prim *p;
  AType *a;
  AType *b;
  AType *result;

  ATypeFold(Prim *ap, AType *aa, AType *ab, AType *aresult = 0) : p(ap), a(aa), b(ab), result(aresult) {}
};
#define forv_ATypeFold(_p, _v) forv_Vec(ATypeFold, _p, _v)

class ATypeFoldChainHashFns {
 public:
  static uint hash(ATypeFold *x) { 
    return (uint)((uintptr_t)x->p + (1009 * (uintptr_t)x->a) + (100003 * (uintptr_t)x->b));
  }
  static int equal(ATypeFold *x, ATypeFold *y) {
    return x->p == y->p && x->a == y->a && x->b == y->b;
  }
};

class FA : public gc { public:
  PDB *pdb;
  CDB *cdb;
  cchar *fn;
  Patterns *patterns;
  Vec<Fun *> funs;
  AEdge *top_edge;
  Vec<EntrySet *> ess;          // all used entry sets as array
  Vec<EntrySet *> ess_set;      // all used entry sets as set
  Vec<Sym *> basic_types;
  Vec<CreationSet *> css, css_set;
  Vec<AVar *> global_avars;

  int print_call_depth;
  bool permit_boxing;
  bool no_unused_instance_variables;
  int tuple_index_base;
  int num_constants_per_variable;

  FA(PDB *apdb) : pdb(apdb), cdb(0), patterns(0), top_edge(0),
                  print_call_depth(2), permit_boxing(0), no_unused_instance_variables(0), 
                  tuple_index_base(0), num_constants_per_variable(1) {}

  int analyze(Fun *f);
  int concretize();

  RegisteredPrim *register_primitive(cchar *name, PrimitiveTransferFunctionPtr ptr);
};

AVar *make_AVar(Var *, EntrySet *);
AVar *get_element_avar(CreationSet *);
Sym *coerce_num(Sym *, Sym *);
Sym *type_info(IFAAST *a, Sym *s = 0);
void call_info(Fun *f, IFAAST *a, Vec<Fun *> &funs);
int constant_info(IFAAST *a, Vec<Sym *> &constants, Sym *s);
int constant_info(Var *v, Vec<Sym *> &constants);
int symbol_info(Var *v, Vec<Sym *> &symbols);
AType *make_AType(CreationSet *cs);
AType *make_AType(Vec<CreationSet *> &css);
AType *make_abstract_type(Sym *s);
void fill_tvals(Fun *fn, PNode *p, int n);
void update_gen(AVar *v, AType *t);
void update_in(AVar *v, AType *t);
void flow_vars(AVar *v, AVar *vv);
void flow_var_type_permit(AVar *v, AType *t);
CreationSet *creation_point(AVar *v, Sym *s, int nvars = -1);
void prim_make(PNode *p, EntrySet *es, Sym *kind, int start = 2, int ref = 0);
void type_violation(ATypeViolation_kind akind, AVar *av, AType *type, AVar *send,
                    Vec<Fun *> *funs = NULL);
AType *type_cannonicalize(AType *t);
AType *type_diff(AType *, AType *);
AType *type_intersection(AType *, AType *);
AType *type_union(AType *a, AType *b);
void log_var_types(Var *, Fun *);
void set_container(AVar *av, AVar *container);
AVar * unique_AVar(Var *v, void *contour);
AVar *unique_AVar(Var *v, EntrySet *es);
void qsort_pointers(void **left, void **right);
void initialize_Sym_for_fa(Sym *s);
int function_dispatch(PNode *p, EntrySet *es, AVar *a0, CreationSet *s, 
                      Vec<AVar *> &args, Vec<char *> &names,
                      int is_closure, Partial_kind partial, PNode *visibility_point = 0);
void add_var_constraint(AVar *av, Sym *s = 0);
void return_nil_transfer_function(PNode *pn, EntrySet *es);
void return_int_transfer_function(PNode *pn, EntrySet *es);
void return_string_transfer_function(PNode *pn, EntrySet *es);
AType *make_size_constant_type(int n);


template<class C> void
qsort_by_id(C **left, C **right) {
 Lagain:
  if (right - left < 5) {
    for (C **y = right - 1; y > left; y--) {
      for (C **x = left; x < y; x++) {
        if (x[0]->id > x[1]->id) {
          C *t = x[0];
          x[0] = x[1];
          x[1] = t;
        }
      }
    }
  } else {
    C  **i = left + 1, **j = right - 1;
    C *x = *left;
    for (;;) {
      while (x->id < (*j)->id) j--;
      while (i < j && (*i)->id < x->id) i++;
      if (i >= j) break;
      C *t = *i;
      *i = *j;
      *j = t;
      i++; j--;
    }
    if (j == right - 1) {
      *left = *(right - 1);
      *(right - 1) = x;
      right--;
      goto Lagain;
    }
    if (left < j) qsort_by_id(left, j + 1);
    if (j + 2 < right) qsort_by_id(j + 1, right);
  }
}

template<class C> void
qsort_by_id(Vec<C *> &v) {
  if (v.n > 1)
    qsort_by_id(&v[0], v.end());
}

extern AType *bottom_type;
extern AType *void_type;
extern AType *unknown_type;
extern AType *top_type;
extern AType *any_type;
extern AType *bool_type;
extern AType *size_type;
extern AType *anyint_type;
extern AType *anynum_kind;
extern AType *fun_type;
extern AType *symbol_type;
extern AType *fun_symbol_type;
extern AType *anytype_type;

extern int analysis_pass;

void pp(AVar *);
void pp(AType *);
void pp(CreationSet *);

#endif
