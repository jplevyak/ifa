#ifndef _fa_H_
#define _fa_H_

#include <sys/types.h>
#include "code.h"
#include "ifadefs.h"
#include "prim.h"
#include "sym.h"

#define DEFAULT_NUM_CONSTANTS_PER_VARIABLE 1
// Default cap on outer convergence iterations. The runtime value lives
// in `FA::pass_limit` — change that to override per-FA instance. See
// `ifa/notes/`-style discussion at the cleanup entry in
// `ifa/analysis/CLEANUP.md` tier-2 item 4 for why the cap is soft
// (frontends inspect `FA::pass_limit_hit` instead of aborting).
#define IFA_PASS_LIMIT 100

#define GLOBAL_CONTOUR ((void *)1)

class Prim;
class RegisteredPrim;
class Fun;
class PNode;
class PDB;
class Edge;
class AVar;
class AEdge;
class AType;
class SettersHashFns;
class Setters;
class CreationSet;
class ATypeViolation;
class Patterns;
class MatchCache;
class MPosition;
class EntrySet;

typedef Map<PNode *, Vec<AEdge *> *> EdgeMap;
typedef BlockHash<AEdge *, PointerHashFns> EdgeHash;
typedef Vec<CreationSet *> VecCreationSet;
typedef Vec<Vec<CreationSet *> *> CSSS;

// Mix two pointer-sized values into a 32-bit hash. The bespoke
// `(13 * a) + (100003 * b)` pattern previously appeared inline in
// every hash-functions class; centralizing it here keeps the mixer
// consistent and gives future hashable types one obvious helper to
// reach for. Callers that need to mix three values do
// `a + combine_hash(b, c)` (matches the historical shape).
inline uint combine_hash(uintptr_t a, uintptr_t b) {
  return (uint)((13 * a) + (100003 * b));
}

class AType : public Vec<CreationSet *> {
 public:
  uint hash;
  AType *type;  // not including values (constants)
  Vec<CreationSet *> sorted;
  Map<AType *, AType *> union_map;
  Map<AType *, AType *> intersection_map;
  Map<AType *, AType *> diff_map;

  AType(CreationSet *cs);
  AType(AType &a);
  AType() : hash(0) {}

  AType *constants();
};

class AEdge : public gc {
 public:
  int id;
  EntrySet *from, *to;
  PNode *pnode;
  Map<MPosition *, AVar *> args;
  Map<MPosition *, AVar *> filtered_args;
  Map<MPosition *, AType *> initial_types;
  Vec<AVar *> rets;
  Fun *fun;
  Match *match;
  uint in_edge_worklist : 1;
  uint es_backedge : 1;
  uint es_cs_backedge : 1;
  LINK(AEdge, edge_worklist_link);

  AEdge();
};

class PendingMapHash {
 public:
  static uint hash(AEdge *e) {
    return (uint)(uintptr_t)e->fun + combine_hash((uintptr_t)e->pnode, (uintptr_t)e->from);
  }
  static int equal(AEdge *a, AEdge *b) { return (a->fun == b->fun) && (a->pnode == b->pnode) && (a->from == b->from); }
};

typedef HashMap<AEdge *, PendingMapHash, Vec<EntrySet *> *> PendingAEdgeEntrySetsMap;
typedef MapElem<AEdge *, Vec<EntrySet *> *> MapElemAEdgeEntrySets;

enum { DFS_white = 0, DFS_grey, DFS_black };

class EntrySet : public gc {
 public:
  Fun *fun;
  int id;
  uint dfs_color : 2;
  uint in_es_worklist : 1;
  Map<MPosition *, AVar *> args;
  Vec<AVar *> rets;
  Map<MPosition *, AType *> filters;
  EdgeHash edges;
  EdgeMap out_edge_map;
  Vec<CreationSet *> creates;
  Vec<EntrySet *> display;
  Vec<AEdge *> out_edges;
  Vec<AEdge *> backedges;
  Vec<AEdge *> es_cs_backedges;
  Vec<CreationSet *> cs_backedges;
  Vec<PNode *> live_pnodes;
  EntrySet *split;
  PendingAEdgeEntrySetsMap pending_es_backedge_map;
  Vec<EntrySet *> *equiv;  // clone.cpp
  LINK(EntrySet, es_worklist_link);

  EntrySet(Fun *af);
};

class CreationSet : public gc {
 public:
  Sym *sym;
  int id;
  uint dfs_color : 2;
  uint clone_for_constants : 1;
  uint added_element_var : 1;
  uint closure_used : 1;
  uint tuple_able : 1;
  Vec<AVar *> defs;
  AType *atype;  // the type that this creation set belongs to
  Vec<AVar *> vars;
  Map<cchar *, AVar *> var_map;
  Vec<EntrySet *> ess;           // entry sets restricted by this creation set
  Vec<EntrySet *> es_backedges;  // entry sets restricted by this creation set
  CreationSet *split;            // creation set this one was split from
  Vec<CreationSet *> *equiv;     // used by clone.cpp & fa.cpp
  Vec<CreationSet *> not_equiv;  // used by clone.cpp
  Sym *type;                     // used by clone.cpp & fa.capp
  Vec<cchar *> unknown_vars;

  CreationSet(Sym *s);
  CreationSet(CreationSet *cs);
};

class Setters : public Vec<AVar *> {
 public:
  uint hash;
  Vec<AVar *> sorted;
  Map<AVar *, Setters *> add_map;

  Setters() : hash(0) {}
};

typedef MapElem<void *, int> MarkElem;
typedef Map<void *, int> MarkMap;
typedef Map<Sym *, CreationSet *> CSMap;
typedef MapElem<Sym *, CreationSet *> CSMapElem;

class AVar : public gc {
 public:
  Var *var;
  int id;
  void *contour;
  Vec<AVar *> forward;
  Vec<AVar *> backward;
  AVar *lvalue;
  AType *gen;
  AType *in;
  AType *out;
  AType *restrict;
  AVar *container;
  Setters *setters;
  Setters *setter_class;
  MarkMap *mark_map;
  CSMap *cs_map;
  MatchCache *match_cache;
  Sym *type;
  int ivar_offset;
  uint in_send_worklist : 1;
  uint contour_is_entry_set : 1;
  uint is_lvalue : 1;
  uint live : 1;
  uint live_arg : 1;
  uint is_if_arg : 1;
  Accum<AVar *> arg_of_send;
  LINK(AVar, send_worklist_link);

  AVar(Var *v, void *acontour);
};

typedef Map<MPosition *, AVar *> MapMPositionAVar;
typedef MapElem<MPosition *, AVar *> MapMPositionAVarElem;
#define form_MPositionAVar(_p, _v) form_Map(MapMPositionAVarElem, _p, _v)

class ATypeChainHashFns {
 public:
  static uint hash(AType *a) { return a->hash; }
  static int equal(AType *a, AType *b) {
    if (a->sorted.n != b->sorted.n) return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i]) return 0;
    return 1;
  }
};

class SettersHashFns {
 public:
  static uint hash(Setters *a) { return a->hash; }
  static int equal(Setters *a, Setters *b) {
    if (a->sorted.n != b->sorted.n) return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i]) return 0;
    return 1;
  }
};

enum class ATypeViolation_kind {
  PRIMITIVE_ARGUMENT,
  SEND_ARGUMENT,
  DISPATCH_AMBIGUITY,
  MEMBER,
  MATCH,
  NOTYPE,
  BOXING,
  CLOSURE_RECURSION
};

class ATypeViolation : public gc {
 public:
  ATypeViolation_kind kind;
  AVar *av;
  AVar *send;
  AType *type;
  Vec<Fun *> *funs;

  ATypeViolation(ATypeViolation_kind akind, AVar *aav, AVar *asend)
      : kind(akind), av(aav), send(asend), type(nullptr), funs(nullptr) {}
};

class ATypeViolationHashFuns {
 public:
  static uint hash(ATypeViolation *x) {
    return (uint)x->kind + combine_hash((uintptr_t)x->av, (uintptr_t)x->send);
  }
  static int equal(ATypeViolation *x, ATypeViolation *y) {
    return x->kind == y->kind && x->av == y->av && x->send == y->send;
  }
};

class ATypeFold : public gc {
 public:
  Prim *p;
  AType *a;
  AType *b;
  AType *result;

  ATypeFold(Prim *ap, AType *aa, AType *ab, AType *aresult = nullptr) : p(ap), a(aa), b(ab), result(aresult) {}
};

class ATypeFoldChainHashFns {
 public:
  static uint hash(ATypeFold *x) {
    return (uint)(uintptr_t)x->p + combine_hash((uintptr_t)x->a, (uintptr_t)x->b);
  }
  static int equal(ATypeFold *x, ATypeFold *y) { return x->p == y->p && x->a == y->a && x->b == y->b; }
};

class FA : public gc {
 public:
  PDB *pdb;
  cchar *fn;
  Patterns *patterns;
  Vec<Fun *> funs;
  AEdge *top_edge;
  Vec<EntrySet *> ess;      // all used entry sets as array
  Vec<EntrySet *> ess_set;  // all used entry sets as set
  Vec<Sym *> basic_types;
  Vec<CreationSet *> css, css_set;
  Vec<AVar *> global_avars;

  int print_call_depth;
  bool permit_boxing;
  bool no_unused_instance_variables;
  int tuple_index_base;
  int num_constants_per_variable;
  // Cap on outer convergence iterations. Frontends may raise it for
  // pathological inputs or lower it for fail-fast tests.
  int pass_limit;
  // Set to true when `pass_limit` was reached with `analyze_again`
  // still set — i.e. the splitter wanted another pass and was cut
  // off. Frontends inspect this to distinguish a converged
  // `type_violations` from a snapshot mid-iteration.
  bool pass_limit_hit;

  FA(PDB *apdb)
      : pdb(apdb),
        patterns(0),
        top_edge(0),
        print_call_depth(2),
        permit_boxing(0),
        no_unused_instance_variables(0),
        tuple_index_base(0),
        num_constants_per_variable(1),
        pass_limit(IFA_PASS_LIMIT),
        pass_limit_hit(false) {}

  int analyze(Fun *f);
  int concretize();

  RegisteredPrim *register_primitive(cchar *name, PrimitiveTransferFunctionPtr ptr);
};

// Clear all module-level analysis state. Called by ifa_reset().
void fa_reset();

// ---------------------------------------------------------------------------
// FA-pass-event sidecar (issue 003)
// ---------------------------------------------------------------------------
// Records which splitter stage fired on which pass, plus before/after
// counts of ess / css / type_violations. Mirrors the InlineEvent
// pattern in ifa/optimize/inline.h — events are only recorded when
// fa_events_enable() has been called; production pays nothing.

enum class FAPassStage {
  TYPE_CONFLUENCE,        // split_ess_for_type
  MARK_TYPE,              // split_ess_for_mark_type
  SETTER,                 // split_for_setters (type-based)
  SETTER_OF_SETTER,       // split_for_setters_of_setters (type-based)
  MARK_SETTER,            // split_for_setters with marks
  MARK_SETTER_OF_SETTER,  // split_for_setters_of_setters with marks
  VIOLATION,              // split_for_violations
};

struct FAPassEvent {
  int pass;                  // 1-indexed analysis_pass at time of record
  FAPassStage stage;
  int splits;                // value returned by the split_* function
  int ess_before, ess_after;
  int css_before, css_after;
  int violations_before, violations_after;
};

void fa_events_enable();
void fa_events_disable();
void fa_events_reset();
const Vec<FAPassEvent *> &fa_events_get();

AVar *make_AVar(Var *, EntrySet *);
AVar *get_element_avar(CreationSet *);
Sym *coerce_num(Sym *, Sym *);
Sym *type_info(IFAAST *a, Sym *s = 0);
void call_info(Fun *f, IFAAST *a, Vec<Fun *> &funs);
int constant_info(IFAAST *a, Vec<Sym *> &constants, Sym *s);
int constant_info(Var *v, Vec<Sym *> &constants);
Sym *get_constant(Var *v);
Sym *get_constant(AVar *av);
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
void prim_make_constraints(PNode *p, EntrySet *es);
void type_violation(ATypeViolation_kind akind, AVar *av, AType *type, AVar *send, Vec<Fun *> *funs = nullptr);
AType *type_cannonicalize(AType *t);
AType *type_diff(AType *, AType *);
AType *type_intersection(AType *, AType *);
AType *type_union(AType *a, AType *b);
void log_var_types(Var *, Fun *);
void set_container(AVar *av, AVar *container);
AVar *unique_AVar(Var *v, void *contour);
AVar *unique_AVar(Var *v, EntrySet *es);
void qsort_pointers(void **left, void **right);
void initialize_Sym_for_fa(Sym *s);
int function_dispatch(PNode *p, EntrySet *es, AVar *a0, CreationSet *s, Vec<AVar *> &args, Vec<char *> &names,
                      int is_closure, Partial_kind partial, PNode *visibility_point = 0);
void add_var_constraint(AVar *av, Sym *s = 0);
void return_nil_transfer_function(PNode *pn, EntrySet *es);
void return_int_transfer_function(PNode *pn, EntrySet *es);
void return_string_transfer_function(PNode *pn, EntrySet *es);
AType *make_size_constant_type(int n);
void collect_types_and_globals(FA *fa, Vec<Sym *> &typesyms, Vec<Var *> &globalsyms);
void fa_dump_types(FA *fa, FILE *fp);

template <class C>
void qsort_by_id(C **left, C **right) {
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
    C **i = left + 1, **j = right - 1;
    C *x = *left;
    for (;;) {
      while (x->id < (*j)->id) j--;
      while (i < j && (*i)->id < x->id) i++;
      if (i >= j) break;
      C *t = *i;
      *i = *j;
      *j = t;
      i++;
      j--;
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

template <class C>
void qsort_by_id(Vec<C *> &v) {
  if (v.n > 1) qsort_by_id(&v[0], v.end());
}

extern FA *fa;

extern AType *bottom_type;
extern AType *void_type;
extern AType *unknown_type;
extern AType *top_type;
extern AType *any_type;
extern AType *bool_type;
extern AType *true_type;
extern AType *false_type;
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
