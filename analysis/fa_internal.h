// SPDX-License-Identifier: BSD-3-Clause
//
// ifa/167: the seam between fa.cc (the analysis) and fa_debug.cc (the
// diagnostics that report on it). NOT a public header -- fa.h is the
// public one, and its surface did not change when the diagnostics moved.
//
// Two directions cross here:
//   - the diagnostics fa.cc calls at convergence and between passes;
//   - the handful of fa.cc internals those diagnostics read.
// A name being here means a diagnostic needs it. If the second list grows
// much, the thing being reported on has probably moved into the wrong file.
#ifndef _fa_internal_H_
#define _fa_internal_H_

#include <map>
#include <string>
#include <vector>

#include "fa.h"

// ---- shared with fa.cc -------------------------------------------------
typedef MapElem<MPosition *, Var *> MapElemMPositionVarPair;

struct ElemCensus {
  std::map<Sym *, int> cs_count;                   // container sym -> CSs
  std::map<Sym *, std::set<void *>> types;         // -> distinct element ATypes
  std::map<Sym *, std::set<std::string>> shapes;   // -> distinct MERGED content class-sets
  std::map<Sym *, std::set<std::string>> pshapes;  // -> distinct POSITIONAL content shapes
  int n_cs = 0;     // container CreationSets, all of them
  int n_empty = 0;  // no content in either channel: indistinguishable by any observable
  int n_mixed = 0;  // content holds both a scalar and a container (issue 018)
  int n_novar = 0;  // no element AVar exists -- content, if any, is positional only
  // ifa/129 step 3's stated precondition: shedskin's demand gate refuses to
  // split a contour that only ONE site created, so it needs contours that
  // more than one site reached. `cs->defs` IS that set -- creation_point
  // does `cs->defs.set_add(v)` (fa.cc:783) for every site routed to the CS,
  // and clear_cs empties it each pass, so at convergence it is this pass's
  // site set. n_multidef == 0 means the gate is vacuous by construction.
  int n_multidef = 0;
  static int total(const std::map<Sym *, std::set<std::string>> &m) {
    int n = 0;
    for (auto &kv : m) n += (int)kv.second.size();
    return n;
  }
  int total_shapes() const { return total(shapes); }
  int total_pshapes() const { return total(pshapes); }
  int total_types() const {
    int n = 0;
    for (auto &kv : types) n += (int)kv.second.size();
    return n;
  }
};

typedef MapElem<MPosition *, AVar *> MapElemMPositionAVarPair;
struct CSFlowGraph : public gc {
  CreationSet *cs = nullptr;
  Vec<AType *> keys;                    // one per assign set
  Vec<Vec<AVar *> *> targets;           // containers written through each set
  Vec<Vec<AVar *> *> paths;             // nodes reached walking back from them
  Vec<Vec<AVar *> *> creation_points;   // per set: path nodes with no incoming edge
  Vec<AVar *> csites;                   // creation points on ANY path
  Vec<AVar *> emptycsites;              // cs->defs not on any path
  // How many assign sets a given creation point lies on -- shedskin's
  // `n.paths`, and the predicate route 1 keys on (`len(n.paths) == 1`).
  int site_set_count(AVar *n) {
    int c = 0;
    for (int i = 0; i < paths.n; i++)
      if (paths.v[i]->set_in(n)) ++c;
    return c;
  }
};

typedef MapElem<Fun *, int> MapElemFunPint;  // ifa/133: mints inside a SPLIT-CHILD contour
// ---- fa_lattice.cc ------------------------------------------------------
AType *type_coerce_numeric_constants(AType *t, Sym *w);
AType *type_num_fold(Prim *p, AType *a, AType *b);

// ---- fa.cc helpers fa_prims.cc calls ------------------------------------
int all_applications(PNode *p, EntrySet *es, AVar *a0, Vec<AVar *> &args, Vec<cchar *> &names, int is_closure, Partial_kind partial, PNode *visibility_point = nullptr, Vec<CreationSet *> *closures = 0);
Var **destruct(Var **lvals, int nlvals, AVar *r, Sym *t, AVar *result, int &tvars);
bool get_obj_index(AVar *index, int *i, int n);
void make_closure(AVar *result);
void make_period_closure(AVar *result, AVar *a, Vec<AVar *> &args);
void prim_make_vector_constraints(PNode *p, EntrySet *es);
void vector_elems(int rank, PNode *p, AVar *ae, AVar *elem, AVar *container, int n = 0);
void structural_assignment(CreationSet *new_cs, CreationSet *cs, PNode *p, EntrySet *es, bool merge = false, bool mix = false, Sym *elide_source = nullptr);
// issues/128 step 3: every record field write this pass, as (receiver
// AVar, field name). Classified at the split stage from CONVERGED types.
void record_field_write(AVar *obj, cchar *name);

// ---- fa_prims.cc --------------------------------------------------------
void add_prim_send_constraints(PNode *p, EntrySet *es, AVar *result, int o);

// ---- fa.cc internals the diagnostics read ------------------------------
void keyspace_cpa(AEdge *e, Vec<MPosition *> &pos, int i, std::string &tup, std::set<std::string> &out, int &budget);
void constant_strip_census(int &nstrip, int &nmulti, int &nsame);
void cselem_unknown_mint_census(int &minted, int &resolved, int &joinable, int &still_unfilled);
void element_census(ElemCensus &c);
bool mixed_basics(AVar *av);
extern std::map<std::string, CreationSet *> cselem_shape_canon;

extern Vec<Var *> fa_internal_vars;  // fa.cc: FA-created tvals, see fill_tvals
template <class F>
inline void foreach_avar(F f) {
  auto do_var = [&](Var *v) {
    for (int i = 0; i < v->avars.n; i++)
      if (v->avars[i].key) f(v->avars[i].value);
  };
  for (Sym *sy : fa->pdb->if1->allsyms) if (sy->var) do_var(sy->var);
  for (Fun *fn : fa->pdb->funs) for (Var *v : fn->fa_all_Vars) do_var(v);
  for (Var *v : fa_internal_vars) do_var(v);
  for (CreationSet *cs : fa->all_creation_sets) if (cs) {
    for (AVar *a : cs->vars) if (a) f(a);
    if (cs->added_element_var) { AVar *ev = get_element_avar(cs); if (ev) f(ev); }
  }
  for (AEdge *e : fa->all_aedges) if (e)
    form_MPositionAVar(x, e->filtered_args) if (x->value) f(x->value);
}
CSFlowGraph *build_cs_flow_graph(CreationSet *cs);
extern Vec<AType *> dk_recv_types;
extern Vec<Fun *> kd_flip_funs;
int compar_edge_id(const void *aa, const void *bb);
int compar_tv(const void *aa, const void *bb);
bool find_violation_user_loc(ATypeViolation *v, cchar **out_filename, int *out_line, int *out_col);
bool find_avar_user_loc(AVar *av, cchar **out_filename, int *out_line, int *out_col);
bool is_uninformative_violation(ATypeViolation *v);
bool atype_irrepresentable(AType *t);
bool classes_are_related(Vec<Sym *> &classes);
extern Vec<EntrySet *> ed_applied;
extern const int kShapeDepth;
extern std::map<std::string, int> cselem_shape_ids;
extern std::vector<std::string> cselem_shape_by_id;
extern const char *kStageName[FA::kNumFAPassStages];

// ---- the diagnostics themselves ----------------------------------------
void dbg_trace_fa_state(cchar *where);
void dbg_bind(AEdge *e, EntrySet *new_es, bool mint);
void show_sym_name(Sym *s, FILE *fp);
void show_type(Vec<CreationSet *> &t, FILE *fp, int verbose = ifa_verbose);
void show_sym(Sym *s, FILE *fp);
void show_fun(Fun *f, FILE *fp);
void show_atype(AType &t, FILE *fp, int level);
void show_name(FILE *fp, AVar *av);
void show_illegal_type(FILE *fp, ATypeViolation *v);
void show_call_tree(FILE *fp, PNode *p, EntrySet *es, int depth = 0);
void show_avar_call_tree(FILE *fp, AVar *av);
void show_candidates(FILE *fp, PNode *pn, Sym *arg0);
void show_violations(FA *fa, FILE *fp);
void show_numeric_coercions(FA *fa, FILE *fp);
void dbg_confluence_probe(AVar *av, bool added);
void dbg_dump_av(AVar *av);
void report_recv_cardinality();
void report_elem_confluence();
void report_cs_defs();
void report_cs_vars();
void report_fun_entry_sets();
void report_cs_flow_graphs();
void report_retconf();
void report_degenerate_avars();
void report_incompat();
void report_markwhy();
void report_keydrift();
void report_canon_stats();
void report_keyspace();
void report_cs_population();
void report_fun_contours();
void report_element_setters();
void report_element_types();
void report_stage_churn();
AVar *dbg_element_avar(CreationSet *cs);
void dbg_atype_str(AType *t, char *buf, int n, int depth);
void dbg_backwalk(AVar *av, int depth, int maxdepth, Vec<AVar *> &seen);
void dbg_dump_contours(int pass);
void report_forward_closure_all();
void report_creation_attribution();
void report_mixed_element_owners();
void report_demand_ratio();

#endif
