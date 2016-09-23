/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#include "ast.h"
#include "builtin.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "ifa.h"
#include "ifadefs.h"
#include "log.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

#define MERGE_UNIONS 1  // merge unions which use different sets of elements
#define CONVERT_LISTS_TO_TUPLES 1

#define BAD_NAME ((char *)-1)
#define BAD_AST ((IFAAST *)-1)

static void initialize() {
  forv_Fun(f, fa->funs) forv_Var( v, f->fa_all_Vars)
    for (int i = 0; i < v->avars.n; i++)
      if (v->avars[i].value && v->avars[i].key &&
          v->avars.v[i].value->contour == GLOBAL_CONTOUR)
        fa->global_avars.set_add(v->avars[i].value);
  fa->global_avars.set_to_vec();
  forv_CreationSet(cs, fa->css) {
    cs->equiv = new Vec<CreationSet *>();
    cs->equiv->add(cs);
  }
  forv_Fun(f, fa->funs) {
    f->ess.set_intersection(fa->ess_set);  // restrict to used entry sets
    forv_EntrySet(es, f->ess) if (es) {
      forv_AEdge(e, es->out_edges) if (e) {
        f->called_ess.set_add(e->to);
        e->to->fun->called_by_ess.set_add(es);
      }
      es->equiv = new Vec<EntrySet *>();
      es->equiv->add(es);
    }
    forv_EntrySet(es, f->ess) if (es) {
      es->out_edge_map.clear();
      forv_AEdge(e, es->out_edges) if (e) {
        // rebuild out_edge_map
        Vec<AEdge *> *ees = es->out_edge_map.get(e->pnode);
        if (!ees) es->out_edge_map.put(e->pnode, (ees = new Vec<AEdge *>));
        ees->set_add(e);
        // build called_css
        forv_MPosition(p, e->match->fun->positional_arg_positions) {
          AVar *av = e->args.get(p);
          if (av) f->called_css.set_union(*av->out);
        }
      }
    }
  }
  Vec<Fun *> funs_set;
  funs_set.set_union(fa->funs);
  forv_Fun(f, pdb->funs) {
    if (!funs_set.set_in(f)) f->ess.clear();
  }
  fa->ess.clear();
  fa->ess.append(fa->ess_set);
  forv_Sym(s, fa->pdb->if1->allsyms) {
    if (s->creators.n) {
      Vec<CreationSet *> creators;
      creators.move(s->creators);
      forv_CreationSet(cs, creators) if (fa->css_set.set_in(cs))
          s->creators.add(cs);
    }
  }
}

static AType *element_type(CreationSet *cs) {
  if (!cs->sym->element) return bottom_type;
  AType *t = get_element_avar(cs)->out;
  forv_AVar(av, cs->vars) t = type_union(t, av->out);
  return t;
}

int compar_fun_nesting(const void *ai, const void *aj) {
  int i = (*(Fun **)ai)->sym->nesting_depth;
  int j = (*(Fun **)aj)->sym->nesting_depth;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

// object inlining placeholder
static int equivalent_es_ivars(EntrySet *a, EntrySet *b) { return 1; }

Sym *to_basic_type(Sym *t) {
  if (t == sym_symbol) return t;
  if (t == sym_string) return t;
  if (t->num_kind) return t;
  if (t->is_constant) return t->type;
  if (t->is_symbol) return sym_symbol;
  if (t->type_kind == Type_TAGGED) return t->specializes[0];
  return 0;
}

static Sym *to_concrete_type(Sym *t) {
  t = t->type;
  if (t->is_symbol) return sym_symbol;
  if (t->type_kind == Type_TAGGED && t->specializes.n) return t->specializes[0];
  return t;
}

// return the Sym of some basic type, fail if basics are mixed or with non
// basics
// and NULL if basic
Sym *basic_type(FA *fa, AType *t, Sym *fail) {
  Sym non_basic;
  Sym *res = 0;
  forv_CreationSet(cs, *t) if (cs) {
    Sym *t = 0;
    if ((t = to_basic_type(cs->sym))) {
      if (!res)
        res = t;
      else
        res = fail;
    } else {
      if (!res)
        res = &non_basic;
      else if (res != &non_basic)
        res = fail;
    }
  }
  if (res == &non_basic) res = NULL;
  return res;
}

// clone for unboxing of basic types
static int equivalent_es_vars(Var *v, EntrySet *a, EntrySet *b) {
  AVar *va = make_AVar(v, a), *vb = make_AVar(v, b);
  if (va != vb) {
    if (basic_type(fa, va->out, (Sym *)-1) !=
        basic_type(fa, vb->out, (Sym *)-2))
      return 0;
  }
  return 1;
}

// clone for different sets of call targets
static int equivalent_es_pnode(PNode *n, EntrySet *a, EntrySet *b) {
  Vec<AEdge *> *va = a->out_edge_map.get(n);
  Vec<AEdge *> *vb = b->out_edge_map.get(n);
  if (!va || !vb) return 1;
  Vec<Vec<EntrySet *> *> ca, cb;
  forv_AEdge(ee, *va) ca.set_add(ee->to->equiv);
  forv_AEdge(ee, *vb) cb.set_add(ee->to->equiv);
  if (ca.some_disjunction(cb)) return 0;
  return 1;
}

static int prim_period_offset(PNode *p, EntrySet *es) {
  AVar *obj = make_AVar(p->rvals[1], es);
  AVar *selector = make_AVar(p->rvals[3], es);
  int offset = -1;
  forv_CreationSet(sel, *selector->out) if (sel) {
    cchar *symbol = sel->sym->name;
    if (!symbol) symbol = sel->sym->constant;
    if (!symbol) symbol = sel->sym->imm.v_string;
    assert(symbol);
    forv_CreationSet(cs, *obj->out) if (cs) {
      AVar *iv = cs->var_map.get(symbol);
      if (iv) {
        if (offset >= 0 && offset != iv->ivar_offset)
          fail("missmatched offsets");
        offset = iv->ivar_offset;
      }
    }
  }
  return offset;
}

class ES_FN {
 public:
  static int inline equivalent(EntrySet *a, EntrySet *b);
};

// The definition is:
// -- nested functions have same/compatible parents
// -- if cloning for constants, arguments have same constants
// -- variables have the same boxing
// -- calls are to the same set of functions
// -- all objects are created equivalent CreationSets
// -- if constant cloning is used, all constants must be the same
// -- '.' and ')' (cast) primitives must have unique symbols
//    and the layouts of the '.' targets are compatible at the
//    symbol (same offset)
inline int ES_FN::equivalent(EntrySet *a, EntrySet *b) {
  int nesting_depth = a->fun->sym->nesting_depth;
  if (nesting_depth) {
    Vec<void *> aparents, bparents;
    forv_AEdge(ee, a->edges) if (ee) if (a->display[nesting_depth - 1]->equiv)
        aparents.add(a->display[nesting_depth - 1]->equiv);
    forv_AEdge(ee, b->edges) if (ee) if (b->display[nesting_depth - 1]->equiv)
        bparents.add(b->display[nesting_depth - 1]->equiv);
    if (aparents.some_disjunction(bparents)) return 0;
  }
  if (a->fun->clone_for_constants) {
    forv_MPosition(p, a->fun->positional_arg_positions) {
      AVar *av = a->args.get(p);
      if (av->var->sym->clone_for_constants)
        if (av->out->constants() != b->args.get(p)->out->constants()) return 0;
    }
  }
  if (!equivalent_es_ivars(a, b)) return 0;
  forv_Var(v, a->fun->fa_all_Vars) if (!equivalent_es_vars(v, a, b)) return 0;
  forv_PNode(n, a->fun->fa_all_PNodes) {
    if (!equivalent_es_pnode(n, a, b)) return 0;
    // clone for creation points of concrete types
    if (n->lvals.n) {
      AVar *av = make_AVar(n->lvals[0], a);
      AVar *bv = make_AVar(n->lvals[0], b);
      if ((av->cs_map || bv->cs_map)) {
        if (!av->cs_map || !bv->cs_map) return 0;
        Vec<Sym *> cssyms;
        form_Map(CSMapElem, x, *av->cs_map) cssyms.set_add(x->key);
        form_Map(CSMapElem, x, *bv->cs_map) cssyms.set_add(x->key);
        forv_Sym(s, cssyms) if (s) {
          CreationSet *acs = av->cs_map->get(s), *bcs = bv->cs_map->get(s);
          if (!acs || !bcs) return 0;
          if (acs->equiv != bcs->equiv) return 0;
        }
        return 0;
      }
    }
    if (n->prim) {
      switch (n->prim->index) {
        default:
          break;
        case P_prim_period: {
          AVar *av = make_AVar(n->rvals[3], a);
          AVar *bv = make_AVar(n->rvals[3], b);
          if (av->out != bv->out) return 0;
          if (prim_period_offset(n, a) != prim_period_offset(n, b)) return 0;
          break;
        }
        case P_prim_cast: {
          AVar *av = make_AVar(n->rvals[2], a);
          AVar *bv = make_AVar(n->rvals[2], b);
          if (av->out != bv->out) return 0;
          break;
        }
      }
    }
  }
  return 1;
}

// this code was MUCH easier to write in Lisp
class AEDGE_FN {
 public:
  static int equivalent(AEdge *a, AEdge *b) { return a->pnode == b->pnode; }
};

class CS_SYM_FN {
 public:
  static int equivalent(CreationSet *a, CreationSet *b) {
    return a->sym == b->sym;
  }
};

class CS_EQ_FN {
 public:
  static int equivalent(CreationSet *a, CreationSet *b) {
    return a->sym == b->sym && !a->not_equiv.set_in(b);
  }
};

// C++'s answer to higher order functions: YUCK
template <class C, class FN>
static void sets_by_f(Vec<C *> &aset, Vec<Vec<C *> *> &asetset) {
  asetset.clear();
  Vec<C *> done;
  forv_Vec(C, x, aset) if (x) {
    if (done.set_add(x)) {
      Vec<C *> *equiv = new Vec<C *>();
      equiv->set_add(x);
      forv_Vec(C, y, aset) if (y) {
        if (!done.set_in(y) && FN::equivalent(x, y)) {
          forv_Vec(C, z, *equiv) if (z && !FN::equivalent(z, y)) goto Lskip;
          done.set_add(y);
          equiv->set_add(y);
        }
      Lskip:;
      }
      asetset.add(equiv);
    }
  }
}

template <class C, class FN>
static void sets_by_f_transitive(Vec<C *> &aset, Vec<Vec<C *> *> &asetset) {
  asetset.clear();
  forv_Vec(C, x, aset) if (x) {
    typedef Vec<C *> VecC;
    forv_Vec(VecC, s, asetset) {
      if (FN::equivalent(x, s->first_in_set())) {
        s->set_add(x);
        goto Lnext;
      }
    }
    {
      Vec<C *> *equiv = new Vec<C *>();
      equiv->set_add(x);
      asetset.add(equiv);
    }
  Lnext:;
  }
}

static void collect_ess_from_css(Vec<CreationSet *> &css,
                                 Vec<EntrySet *> &ess) {
  Vec<CreationSet *> todo_css;
  todo_css.copy(css);
  todo_css.set_to_vec();
  for (int i = 0; i < todo_css.n; i++) {
    CreationSet *cs = todo_css[i];
    forv_AVar(av, cs->defs) if (av) {
      if (av->contour_is_entry_set)
        ess.set_add((EntrySet *)av->contour);
      else if (av->contour != GLOBAL_CONTOUR)
        todo_css.add((CreationSet *)av->contour);
    }
  }
}

static inline void make_not_equiv(CreationSet *a, CreationSet *b) {
  a->not_equiv.set_add(b);
  b->not_equiv.set_add(a);
}

static void determine_basic_clones(Vec<Vec<CreationSet *> *> &css_sets_by_sym) {
  Vec<Vec<CreationSet *> *> xx;
  sets_by_f_transitive<CreationSet, CS_SYM_FN>(fa->css, css_sets_by_sym);
  // clone for unboxing of basic types
  for (int i = 0; i < css_sets_by_sym.n; i++) {
    for (int j = 0; j < css_sets_by_sym[i]->n; j++) {
      if (!css_sets_by_sym[i]->v[j]) continue;
      for (int k = 0; k < j; k++) {
        if (!css_sets_by_sym[i]->v[k]) continue;
        CreationSet *cs1 = css_sets_by_sym[i]->v[j],
                    *cs2 = css_sets_by_sym.v[i]->v[k];
        // both have elements or not
        if (!!cs1->sym->element != !!cs2->sym->element) {
          make_not_equiv(cs1, cs2);
          continue;
        }
        // if different number of instance variables
        if (cs1->vars.n != cs2->vars.n) {
          make_not_equiv(cs1, cs2);
          continue;
        }
        // for each variable
        int start = cs1->sym->element ? -1 : 0;
        for (int v = start; v < cs1->vars.n; v++) {
          AVar *av1 = 0, *av2 = 0;
          if (v < 0) {
            av1 = get_element_avar(cs1);
            av2 = get_element_avar(cs2);
          } else {
            av1 = cs1->vars[v];
            av2 = cs2->vars[v];
          }
          // ?? trigger only when one is empty
          if (MERGE_UNIONS && cs1->sym->is_union_type &&
              (av1->out->n == 0 || av2->out->n == 0))
            continue;
          // if the boxing or basic type is different
          if (basic_type(fa, av1->out, (Sym *)-1) !=
              basic_type(fa, av2->out, (Sym *)-2)) {
            make_not_equiv(cs1, cs2);
            continue;
          }
          // if we are cloning for constants and the constants are different
          if (av1->var->sym->clone_for_constants &&
              av1->out->constants() != av2->out->constants()) {
            make_not_equiv(cs1, cs2);
            continue;
          }
        }
      }
    }
  }
  Vec<Vec<CreationSet *> *> css_sets;
  sets_by_f<CreationSet, CS_EQ_FN>(fa->css, css_sets);
  for (int i = 0; i < css_sets_by_sym.n; i++) {
    Vec<CreationSet *> *s = css_sets[i];
    forv_CreationSet(cs, *s) if (cs) cs->equiv = s;
  }
}

static void determine_layouts() {
  forv_CreationSet(cs, fa->css) {
    unsigned int offset = 0;
    forv_AVar(iv, cs->vars) {
      unsigned int size = 0, alignment = 0;
      forv_CreationSet(x, *iv->out->type) if (x) {
        if (size && x->sym->size != size) fail("mismatched field sizes");
        size = x->sym->size;
        if (alignment && x->sym->alignment != alignment)
          fail("mismatched field alignments");
        alignment = x->sym->alignment;
      }
      if (alignment) offset = (offset + alignment - 1) & ~(alignment - 1);
      iv->ivar_offset = offset;
      offset += size;
    }
  }
}

// the core function: builds equiv_sets for entry sets and creation sets
static void determine_clones() {
  Vec<CreationSet *> changed_css, last_changed_css;
  Vec<EntrySet *> changed_ess, last_changed_ess;
  changed_ess.copy(fa->ess);
  changed_css.copy(fa->css);

  Vec<Vec<CreationSet *> *> css_sets_by_sym;
  determine_basic_clones(css_sets_by_sym);

  // find fixed point
  while (changed_css.n) {
    last_changed_css.move(changed_css);
    Vec<EntrySet *> last_changed_css_ess;
    collect_ess_from_css(last_changed_css, last_changed_css_ess);

    // recompute entry set equivalence
    while (changed_ess.n || last_changed_css.n) {
      changed_ess.set_to_vec();
      last_changed_ess.move(changed_ess);
      forv_Fun(f, fa->funs) {
        if (f->ess.some_intersection(last_changed_ess) ||
            f->called_ess.some_intersection(last_changed_ess) ||
            f->called_by_ess.some_intersection(last_changed_ess) ||
            f->called_css.some_intersection(last_changed_css) ||
            f->ess.some_intersection(last_changed_css_ess) ||
            f->called_ess.some_intersection(last_changed_css_ess)) {
          sets_by_f<EntrySet, ES_FN>(f->ess, f->equiv_sets);
          forv_EntrySet(es, f->ess) if (es) {
            Vec<EntrySet *> *myset = NULL;
            for (int i = 0; i < f->equiv_sets.n; i++)
              if (f->equiv_sets[i]->in(es)) {
                myset = f->equiv_sets[i];
                break;
              }
            if (myset->some_disjunction(*es->equiv)) {
              changed_ess.set_union(*myset);
              es->equiv = myset;
            }
          }
        }
      }
      last_changed_css.clear();
    }
    // recompute non equivalent creation set required for static dispatch
    forv_Fun(f, fa->funs) {
      forv_Vec(Vec<EntrySet *>, ess, f->equiv_sets) {
        Vec<AEdge *> calls_edges;
        Vec<Vec<AEdge *> *> edge_sets;
        forv_EntrySet(es, *ess) if (es) calls_edges.set_union(es->out_edges);
        sets_by_f_transitive<AEdge, AEDGE_FN>(calls_edges, edge_sets);
        for (int i = 0; i < edge_sets.n; i++)
          for (int j = 0; j < edge_sets[i]->n; j++)
            for (int k = 0; k < j; k++) {
              AEdge *e1 = edge_sets[i]->v[j], *e2 = edge_sets.v[i]->v[k];
              if (e1 && e2 && e1->match->fun == e2->match->fun &&
                  e1->to->equiv != e2->to->equiv) {
                forv_MPosition(p, e1->match->fun->positional_arg_positions) {
                  Vec<CreationSet *> d1, d2;
                  AVar *a1 = e1->args.get(p), *a2 = e2->args.get(p);
                  if (!a1 || !a2) continue;
                  a1->out->set_difference(*a2->out, d1);
                  a2->out->set_difference(*a1->out, d2);
                  forv_CreationSet(c1, d1) {
                    forv_CreationSet(c2, d2) {
                      if (c1->equiv == c2->equiv) make_not_equiv(c1, c2);
                    }
                  }
                }
              }
            }
      }
    }
    // recompute creation set equivalence from non-equivalence
    for (int i = 0; i < css_sets_by_sym.n; i++) {
      Vec<Vec<CreationSet *> *> css_sets;
      sets_by_f<CreationSet, CS_EQ_FN>(*css_sets_by_sym[i], css_sets);
      for (int j = 0; j < css_sets.n; j++) {
        Vec<CreationSet *> *s = css_sets[j];
        forv_CreationSet(cs, *s) if (cs) {
          if (cs->equiv->some_disjunction(*s)) changed_css.set_add(cs);
          cs->equiv = s;
        }
      }
    }
  }
}

Sym *concrete_type_set_to_type(Vec<Sym *> &t) {
  t.set_to_vec();
  if (!t.n)
    return sym_void;
  else if (t.n == 1)
    return t[0];
  else {
    Sym *tt = new_Sym();
    tt->type_kind = Type_SUM;
    tt->has.append(t);
    if (!(tt = if1->callback->make_LUB_type(tt))) return 0;
    return tt;
  }
}

static int compute_member_types(Vec<CreationSet *> *eqcss) {
  Sym *sym = eqcss->first_in_set()->type;
  Sym *orig_sym = eqcss->first_in_set()->sym;
  if (sym->is_fun) return 0;
  int n = 0;
  forv_CreationSet(cs, *eqcss) if (cs) {
    assert(!n || n == cs->vars.n);
    n = cs->vars.n;
  }
  sym->has.fill(n);
  int start = orig_sym->element ? -1 : 0;
  for (int i = start; i < n; i++) {
    Sym *&ss = (i < 0 ? sym->element : sym->has[i]);
    forv_CreationSet(cs, *eqcss) if (cs) {
      ss = ss ? ss->clone() : new_Sym();
      AVar *av = i < 0 ? get_element_avar(cs) : cs->vars[i];
      Vec<Sym *> t;
      forv_CreationSet(x, *av->out->type) if (x)
          t.set_add(to_concrete_type(x->type ? x->type : x->sym));
      if (!(ss->type = concrete_type_set_to_type(t))) return -1;
    }
  }
  return 0;
}

static bool tuple_able(CreationSet *cs) {
#ifdef CONVERT_LISTS_TO_TUPLES
  AVar *elem = get_element_avar(cs);
  return elem && elem->out == bottom_type;
#else
  return false;
#endif
}

static void get_sym_tup(Vec<CreationSet *> *eqcss, Sym **psym, bool *ptup) {
  Sym *sym = 0;
  bool tup = true;
  int n = -1;
  forv_CreationSet(cs, *eqcss) if (cs) {
    Sym *ct = to_concrete_type(cs->sym);
    if (!sym)
      sym = ct;
    else if (sym != ct)
      sym = (Sym *)-1;
    if (n < 0)
      n = cs->vars.n;
    else if (n != cs->vars.n)
      tup = false;
    tup = tup && cs->tuple_able;
  }
  *psym = sym;
  *ptup = tup;
}

static void set_tuple_able(CSSS &css_sets) {
  for (int i = 0; i < css_sets.n; i++) {
    Vec<CreationSet *> *eqcss = css_sets[i];
    Sym *sym = 0;
    bool tup = true;
    forv_CreationSet(cs, *eqcss) if (cs) {
      Sym *ct = to_concrete_type(cs->sym);
      if (!sym)
        sym = ct;
      else if (sym != ct)
        sym = (Sym *)-1;
      tup = tup && tuple_able(cs);
    }
    forv_CreationSet(cs, *eqcss) if (cs) cs->tuple_able = tup;
  }
}

// sets cs->sym to the new concrete symbol
static int define_concrete_types(CSSS &css_sets) {
  CSSS css_sets_local;
  set_tuple_able(css_sets);
  // for those only used in one way, do not clone
  for (int i = 0; i < css_sets.n; i++) {
    Vec<CreationSet *> *eqcss = css_sets[i];
    Sym *sym = 0;
    bool tup = true;
    get_sym_tup(eqcss, &sym, &tup);
    forv_CreationSet(cs, *eqcss) if (cs) cs->tuple_able = tup;
    // same sym
    if (sym != (Sym *)-1) {
      if (sym != sym_tuple && sym != sym_closure && !tup) {
        Vec<CreationSet *> creators;
        if (sym->abstract_type)
          eqcss->set_difference(*sym->abstract_type, creators);
        else
          creators.copy(*eqcss);
        if (!sym->creators.some_difference(creators)) {
          forv_CreationSet(cs, *eqcss) if (cs) cs->type = sym;
          continue;
        }
      }
    }
    css_sets_local.add(eqcss);
  }
  // clone those used in more than one way
  for (int i = 0; i < css_sets_local.n; i++) {
    Vec<CreationSet *> *eqcss = css_sets_local[i];
    Sym *sym = 0;
    bool tup = true;
    get_sym_tup(eqcss, &sym, &tup);
    // same sym
    if (sym != (Sym *)-1) {
      AVar *def = 0;
      forv_CreationSet(cs, *eqcss) if (cs) {
        forv_AVar(av, cs->defs) if (av) {
          if (!def)
            def = av;
          else if (def != av)
            def = (AVar *)-1;
        }
      }
      if (sym == sym_tuple || sym == sym_closure || tup) {
        // tuples use record type
        cchar *name = 0;
        IFAAST *ast = 0;
        int abstract = eqcss->n == 1 && eqcss->v[0]->defs.n == 0;
        Sym *s = abstract ? sym->copy() : sym->clone();
        s->type_kind = (sym == sym_tuple || tup) ? Type_RECORD : Type_FUN;
        s->creators.copy(*eqcss);
        Vec<CreationSet *> old_creators;
        old_creators.set_union(sym->creators);
        sym->creators.clear();
        old_creators.set_difference(s->creators, sym->creators);
        s->creators.set_to_vec();
        sym->creators.set_to_vec();
        forv_CreationSet(cs, *eqcss) if (cs) {
          Sym *ct = to_concrete_type(cs->sym);
          if (!name)
            name = ct->name;
          else if (ct->name != name)
            name = BAD_NAME;
          if (cs->defs.n != 1)
            ast = BAD_AST;
          else if (!ast)
            ast = cs->defs.first_in_set()->var->def->code->ast;
          else if (ast != cs->defs.first_in_set()->var->def->code->ast)
            ast = BAD_AST;
          cs->type = s;
        }
        if (name && name != BAD_NAME) s->name = name;
        if (ast && ast != BAD_AST) s->ast = ast;
      } else if (sym->type_kind == Type_PRIMITIVE ||
                 sym->type_kind == Type_TAGGED || sym->is_fun) {
        AVar *elem = get_element_avar(eqcss->first_in_set());
        Sym *s = sym;
        if (elem && elem->out != bottom_type) {
          int abstract = eqcss->n == 1 && eqcss->v[0]->defs.n == 0;
          s = abstract ? sym->copy() : sym->clone();
        }
        forv_CreationSet(cs, *eqcss) if (cs) cs->type = s;
      } else {
        if (eqcss->n == 1 && !eqcss->v[0]->defs.n) {
          // the abstract type
          forv_CreationSet(cs, *eqcss) if (cs) cs->type = sym;
        } else {
          int abstract = eqcss->n == 1 && eqcss->v[0]->defs.n == 0;
          Sym *s = abstract ? sym->copy() : sym->clone();
          cchar *name = 0;
          s->type_kind = sym->type_kind;
          s->creators.copy(*eqcss);
          Vec<CreationSet *> old_creators;
          old_creators.set_union(sym->creators);
          sym->creators.clear();
          old_creators.set_difference(s->creators, sym->creators);
          s->creators.set_to_vec();
          sym->creators.set_to_vec();
          forv_CreationSet(cs, *eqcss) if (cs) {
            cs->type = s;
            if (!name)
              name = cs->sym->type->name;
            else if (cs->sym->type->name != name)
              name = BAD_NAME;
          }
          if (name && name != BAD_NAME) s->name = name;
        }
      }
    } else {
      // if different sym use sum type
      sym = new_Sym();
      sym->type_kind = Type_SUM;
      forv_CreationSet(cs, *eqcss) if (cs) {
        cs->type = sym;
        sym->creators.add(cs);
      }
    }
  }
  return 0;
}

static int concretize_avar(AVar *av) {
  Sym *sym = 0, *type = 0;
  if (av->type) return 0;
  forv_CreationSet(cs, *av->out) if (cs) {
    if (!sym)
      sym = cs->type;
    else {
      if (sym != cs->type) {
        if (!type) {
          type = new_Sym();
          type->type_kind = Type_SUM;
          type->has.set_add(sym);
        }
        type->has.set_add(cs->type);
      }
    }
  }
  if (!type)
    av->type = sym;
  else {
    type->has.set_to_vec();
    if (type->has.n == 1)
      av->type = type->has[0];
    else if (!(av->type = if1->callback->make_LUB_type(type)))
      return -1;
  }
  return 0;
}

static int concretize_var_list_type(Var *v) {
  Sym *sym = 0;
  // check if we even need to try to convert a list
  bool all_list = false;
  AType *etype = bottom_type;
  for (int i = 0; i < v->avars.n; i++) {
    if (!v->avars[i].key) continue;
    forv_CreationSet(cs, *v->avars[i].value->out) if (cs) {
      if (!sym)
        sym = cs->type;
      else if (sym != cs->type)
        all_list = true;
    }
  }
  // check if everything is a list
  if (all_list) {
    for (int i = 0; i < v->avars.n; i++) {
      if (!v->avars[i].key) continue;
      forv_CreationSet(cs, *v->avars[i].value->out) if (cs) {
        if (cs->sym != sym_list)
          all_list = false;
        else
          etype = type_union(etype, element_type(cs));
      }
    }
  }
  if (all_list) {
    // if the element is a single type, use that
    v->type = sym_list->clone();
    v->type->element = new_Sym();
    if (etype->n == 1)
      v->type->element->type = etype->v[0]->type;
    else {
      Sym *t = new_Sym();
      t->type_kind = Type_SUM;
      forv_CreationSet(cs, *etype) if (cs) t->has.set_add(cs->type);
      if (!(v->type = if1->callback->make_LUB_type(t))) return -1;
    }
    return 1;
  }
  return 0;
}

static int concretize_var_type(Var *v) {
  Sym *sym = 0, *type = 0;
  if (v->type) return 0;
  for (int i = 0; i < v->avars.n; i++) {
    if (!v->avars[i].key) continue;
    if (v->avars[i].value->cs_map && v->def) {
      if (!v->def->creates) v->def->creates = new Vec<Sym *>;
      form_Map(CSMapElem, x, *v->avars[i].value->cs_map)
          v->def->creates->set_add(x->value->type);
    }
  }
  int ret = 0;
  if ((ret = concretize_var_list_type(v)) != 0) return ret;
  for (int i = 0; i < v->avars.n; i++) {
    if (!v->avars[i].key) continue;
    forv_CreationSet(cs, *v->avars[i].value->out) if (cs) {
      if (!sym)
        sym = cs->type;
      else {
        if (sym != cs->type) {
          if (!type) {
            type = new_Sym();
            type->type_kind = Type_SUM;
            type->has.set_add(sym);
          }
          type->has.set_add(cs->type);
        }
      }
    }
  }
  if (!type)
    v->type = sym;
  else {
    type->has.set_to_vec();
    if (type->has.n == 1)
      v->type = type->has[0];
    else if (!(v->type = if1->callback->make_LUB_type(type)))
      return -1;
  }
  return 0;
}

static int resolve_concrete_types(CSSS &css_sets) {
  for (int i = 0; i < css_sets.n; i++) {
    Vec<CreationSet *> *eqcss = css_sets[i];
    forv_CreationSet(cs, *eqcss) if (cs) {
      forv_AVar(av, cs->vars) if (concretize_avar(av) < 0) return -1;
      AVar *eav = get_element_avar(cs);
      if (eav)
        if (concretize_avar(eav) < 0) return -1;
    }
    Sym *sym = eqcss->first_in_set()->type;
    switch (sym->type_kind) {
      case Type_NONE:
      case Type_UNKNOWN:
      case Type_ALIAS:
      case Type_APPLICATION:
        break;
      case Type_SUM: {
        forv_CreationSet(cs, *eqcss) if (cs) sym->has.set_add(cs->sym->type);
        sym->has.set_to_vec();
        qsort(sym->has.v, sym->has.n, sizeof(sym->has[0]), compar_syms);
        if (!(sym = if1->callback->make_LUB_type(sym))) return -1;
        forv_CreationSet(cs, *eqcss) if (cs) cs->type = sym;
        break;
      }
      case Type_PRIMITIVE:  // to handle Sym.element e.g. List
      case Type_RECORD:
      case Type_REF:
      case Type_FUN:
      case Type_TAGGED:
        if (compute_member_types(eqcss) < 0) return -1;
        break;
    }
  }
  return 0;
}

static int build_concrete_types() {
  CSSS css_sets;
  forv_CreationSet(cs, fa->css) css_sets.set_add(cs->equiv);
  css_sets.set_to_vec();
  if (define_concrete_types(css_sets) < 0) return -1;
  if (resolve_concrete_types(css_sets) < 0) return -1;
  return 0;
}

static int concretize_types(Fun *f) {
  forv_Var(v, f->fa_all_Vars) if (concretize_var_type(v) < 0) return -1;
  return 0;
}

static void fixup_var(Var *v, Fun *f, Vec<EntrySet *> *ess) {
  AVarMap avs;
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key && ess->set_in((EntrySet *)v->avars.v[i].key)) {
      avs.put(v->avars[i].key, v->avars.v[i].value);
      v->avars.v[i].value->var = v;
    }
  v->avars.move(avs);
}

static void fixup_clone_vars(Fun *f, Vec<EntrySet *> *ess) {
  forv_Var(
      v, f->fa_all_Vars) if (v->sym->nesting_depth == f->sym->nesting_depth + 1)
      fixup_var(v, f, ess);
  else if (v->sym->nesting_depth) {
    if (!v->sym->is_fun)  // these aren't fixed up... probably should be, but
                          // they are constants
      forv_EntrySet(es, f->ess) if (es) {
        AVarMap avs;
        for (int i = 0; i < v->avars.n; i++) {
          EntrySet *x = (EntrySet *)v->avars[i].key;
          if (x && es->display[v->sym->nesting_depth - 1] == x)
            avs.put(x, v->avars[i].value);
          v->avars.v[i].value->var = v;
        }
        v->avars.move(avs);
      }
  }
}

static void fixup_clone_ess(Fun *f, Vec<EntrySet *> *ess) {
  f->ess.copy(*ess);
  forv_EntrySet(es, f->ess) if (es) {
    forv_PNode(p, es->live_pnodes) if (p) {
      if (f->nmap) p = f->nmap->get(p);
      p->fa_live = 1;
    }
    forv_AEdge(ee, es->edges) if (ee) ee->fun = f;
    es->fun = f;
  }
  f->equiv_sets.clear();
  forv_EntrySet(es, f->ess) if (es) f->equiv_sets.set_add(es->equiv);
}

static void fixup_clone(Fun *f, Vec<EntrySet *> *ess) {
  fixup_clone_ess(f, ess);
  fixup_clone_vars(f, ess);
}

void fixup_clone_tree(Fun *f, Vec<EntrySet *> *ess, Vec<Fun *> &fs) {
  fixup_clone(f, ess);
  fa->funs.add(f);
  Vec<Fun *> nested_fns;
  Vec<EntrySet *> ess_set;
  ess_set.set_union(*ess);
  for (int i = 0; i < f->fmap->n; i++)
    if (f->fmap->v[i].key) {
      if (f->fmap->v[i].value != f) nested_fns.add(f->fmap->v[i].key);
    }
  qsort(nested_fns.v, nested_fns.n, sizeof(nested_fns[0]), compar_fun_nesting);
  forv_Fun(ff, nested_fns) {
    Fun *new_ff = f->fmap->get(ff);
    Vec<EntrySet *> old_ess, new_ess;
    forv_EntrySet(es, ff->ess) if (es) {
      if (ess_set.set_in(es->display[f->sym->nesting_depth]))
        new_ess.set_add(es);
      else
        old_ess.set_add(es);
    }
    fixup_clone_ess(ff, &old_ess);
    fixup_clone_ess(new_ff, &new_ess);
    if (new_ess.n) fa->funs.add(new_ff);
    if (new_ess.n || old_ess.n) {
      for (int i = 0; i < fs.n; i++) {
        if (ff == fs[i]) {
          if (!old_ess.n)
            fs[i] = new_ff;
          else {
            fs.insert(i + 1, new_ff);
          }
          goto Lfound;
        }
      }
      assert(!"found");
    }
  Lfound:;
  }
}

static int clone_functions() {
  Vec<Fun *> fs;
  fs.copy(fa->funs);
  qsort(fs.v, fs.n, sizeof(fs[0]), compar_fun_nesting);
  forv_Fun(f, fs) {
    if (f->equiv_sets.n == 1) {
      fixup_clone(f, f->equiv_sets[0]);
      if (concretize_types(f) < 0) return -1;
    } else {
      Vec<Vec<EntrySet *> *> eqs(f->equiv_sets);
      for (int i = 0; i < eqs.n; i++) {
        if (i != eqs.n - 1) {
          Fun *ff = f->copy();
          fixup_clone_tree(ff, eqs[i], fs);
          if (concretize_types(ff) < 0) return -1;
        } else {
          fixup_clone(f, eqs[i]);
          if (concretize_types(f) < 0) return -1;
        }
      }
    }
  }
  // build cloned call graph
  // careful: only EntrySet::fun has been updated with the cloned function!
  // build Fun::calls
  forv_EntrySet(es, fa->ess) {
    Fun *f = es->fun;
    Vec<AEdge *> used_edges;
    EdgeMap new_out_edge_map;
    used_edges.set_union(es->out_edges);  // build set
    for (int i = 0; i < es->out_edge_map.n; i++) {
      if (es->out_edge_map[i].key) {
        PNode *old_pnode = es->out_edge_map[i].key, *pnode = old_pnode;
        if (f->nmap) pnode = f->nmap->get(pnode);
        Vec<AEdge *> *m = es->out_edge_map[i].value;
        if (m) {
          Vec<Fun *> *vf = f->calls.get(pnode);
          if (!vf) f->calls.put(pnode, (vf = new Vec<Fun *>));
          forv_AEdge(ee, *m) if (used_edges.set_in(ee))
              vf->set_add(ee->to->fun);
          // rebuild out_edge_map
          new_out_edge_map.put(pnode, m);
        }
      }
    }
    es->out_edge_map.move(new_out_edge_map);
  }
  // convert Fun::calls to vectors
  forv_Fun(f, fa->funs) for (int i = 0; i < f->calls.n;
                             i++) if (f->calls[i].key) f->calls[i]
      .value->set_to_vec();
  // invert Fun::calls to generated Fun::called
  forv_Fun(f, fa->funs) {
    for (int i = 0; i < f->calls.n; i++)
      if (f->calls[i].key) {
        forv_Fun(ff, *f->calls[i].value)
            ff->called.add(new CallPoint(f, f->calls[i].key));
      }
  }
  return 0;
}

void log_test_fa(FA *fa) {
  Vec<Var *> gvars;
  forv_Fun(f, fa->funs) {
    if (f->sym->source_line() > 0) {
      ALOG(test, fa)
      ("function %s %s:%d\n", f->sym->name ? f->sym->name : "<anonymous>",
       f->sym->filename(), f->sym->source_line());
      forv_CallPoint(cp, f->called) {
        Fun *ff = cp->fun;
        if (cp->pnode->code->line() > 0 && ff->source_line() > 0)
          ALOG(test, fa)
          (" called from %d in %s at %s:%d\n", cp->pnode->code->source_line(),
           ff->sym->name ? ff->sym->name : "<anonymous>", ff->filename(),
           ff->source_line());
      }
    }
    forv_Var(v, f->fa_all_Vars) {
      if (v->sym->source_line()) {
        if (!v->sym->nesting_depth) {
          gvars.set_add(v);
          continue;
        } else
          log_var_types(v, f);
      }
    }
  }
  gvars.set_to_vec();
  qsort_by_id(gvars);
  ALOG(test, fa)("globals\n");
  forv_Var(v, gvars) if ((!v->sym->is_constant || !v->sym->constant) &&
                         !v->sym->is_symbol && !v->sym->type_kind &&
                         !v->sym->is_fun) log_var_types(v, 0);
}

int clone(FA *afa) {
  initialize();
  determine_layouts();
  determine_clones();
  if (build_concrete_types() < 0) return -1;
  if (clone_functions() < 0) return -1;
  return 0;
}
