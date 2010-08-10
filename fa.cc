/* -*-Mode: c++;-*-
   Copyright (c) 2004-2010 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "pattern.h"
#include "prim.h"
#include "if1.h"
#include "builtin.h"
#include "pdb.h"
#include "fun.h"
#include "pnode.h"
#include "fa.h"
#include "ast.h"
#include "var.h"
#include "clone.h"
#include "graph.h"
#include "log.h"
#include "fail.h"
#include "timer.h"

/* runtime options 
*/
bool fgraph_pass_contours = false;

int analysis_pass = 0;

AType *bottom_type = 0;
AType *nil_type = 0;
AType *unknown_type = 0;
AType *void_type = 0;
AType *top_type = 0;
AType *any_type = 0;
AType *bool_type = 0;
AType *size_type = 0;
AType *anyint_type = 0;
AType *anynum_kind = 0;
AType *symbol_type = 0;
AType *string_type = 0;
AType *tuple_type = 0;
AType *anytype_type = 0;
AType *function_type = 0;

static int avar_id = 1;
static int aedge_id = 1;
static int creation_set_id = 1;
static int entry_set_id = 1;

static FA *fa = 0;
static Timer pass_timer, match_timer, extend_timer;

static ChainHash<AType *, ATypeChainHashFns> cannonical_atypes;
static ChainHash<Setters *, SettersHashFns> cannonical_setters;
static ChainHash<SettersClasses *, SettersClassesHashFns> cannonical_setters_classes;
static ChainHash<ATypeFold *, ATypeFoldChainHashFns> type_fold_cache;
static ChainHash<ATypeViolation *, ATypeViolationHashFuns> type_violation_hash;

static Que(AEdge, edge_worklist_link) edge_worklist;
static Que(AVar, send_worklist_link) send_worklist;
static Vec<EntrySet *> entry_set_done;
static Vec<ATypeViolation *> type_violations;

static int application(PNode *p, EntrySet *es, AVar *fun, CreationSet *s, 
                       Vec<AVar *> &args, Vec<cchar *> &names,
                       int is_closure, Partial_kind partial, 
                       PNode *visibility_point, Vec<CreationSet*> *closures);

AEdge::AEdge() : 
  from(0), to(0), pnode(0), fun(0), match(0), in_edge_worklist(0) 
{
  id = aedge_id++;
}

AVar::AVar(Var *v, void *acontour) : 
  var(v), contour(acontour), lvalue(0), gen(0), in(bottom_type), out(bottom_type), 
  restrict(0), container(0), setters(0), setter_class(0), mark_map(0),
  cs_map(0), match_cache(0), type(0), ivar_offset(0), in_send_worklist(0), contour_is_entry_set(0), 
  is_lvalue(0), live(0)
{
  id = avar_id++;
}

AType::AType(AType &a) {
  hash = 0;
  this->copy(a);
}

AType::AType(CreationSet *cs) {
  hash = 0;
  set_add(cs);
}

AVar *
unique_AVar(Var *v, void *contour) {
  assert(contour);
  AVar *av = v->avars.get(contour);
  if (av)
    return av;
  av = new AVar(v, contour);
  v->avars.put(contour, av);
  return av;
}

AVar *
unique_AVar(Var *v, EntrySet *es) {
  assert(es);
  AVar *av = v->avars.get(es);
  if (av)
    return av;
  av = new AVar(v, es);
  v->avars.put(es, av);
  av->contour_is_entry_set = 1;
  if (v->sym->is_lvalue) {
    av->lvalue = new AVar(v, es);
    av->lvalue->is_lvalue = 1;
    av->lvalue->contour_is_entry_set = 1;
  }
  return av;
}

CreationSet::CreationSet(Sym *s) : sym(s), dfs_color(DFS_white), clone_for_constants(0), 
                                   added_element_var(0), closure_used(0), tuple_able(0),
                                   atype(0), equiv(0), type(0)
{
  id = creation_set_id++;
}

CreationSet::CreationSet(CreationSet *cs) : dfs_color(DFS_white), added_element_var(0),
                                            closure_used(0), tuple_able(0), atype(0),
                                            equiv(0), type(0) 
{
  sym = cs->sym;
  id = creation_set_id++;
  clone_for_constants = cs->clone_for_constants;
  forv_AVar(v, cs->vars) {
    AVar *iv = unique_AVar(v->var, this);
    add_var_constraint(iv);
    vars.add(iv);
    if (iv->var->sym->name)
      var_map.put(iv->var->sym->name, iv);
  }
  sym->creators.add(this);
}

EntrySet::EntrySet(Fun *af) : fun(af), dfs_color(DFS_white), split(0), equiv(0) {
  id = entry_set_id++;
}

AVar *
make_AVar(Var *v, EntrySet *es) {
  if (v->sym->nesting_depth) {
    if (v->sym->nesting_depth != es->fun->sym->nesting_depth + 1)
      return unique_AVar(v, es->display[v->sym->nesting_depth-1]);
    return unique_AVar(v, es);
  }
  if (v->is_internal)
    return unique_AVar(v, es);
  return unique_AVar(v, GLOBAL_CONTOUR);
}

static inline AVar *
make_AVar(Var *v, AEdge *e) { 
  return make_AVar(v, e->to); 
}

AType *
make_AType(CreationSet *cs) {
  if (cs->atype)
    return cs->atype;
  return cs->atype = type_cannonicalize(new AType(cs));
}

AType *
make_abstract_type(Sym *s) {
  s = unalias_type(s);
  AType *a = s->abstract_type;
  if (a)
    return a;
  CreationSet *cs = new CreationSet(s);
  return s->abstract_type = make_AType(cs);
}

AType *
make_AType(Vec<CreationSet *> &css) {
  AType *t = new AType();
  t->set_union(css);
  return type_cannonicalize(t);
}

AType *
AType::constants() {
  AType *t = new AType();
  forv_CreationSet(cs, this->sorted)
    if (cs->sym->constant)
      t->set_add(cs);
  return type_cannonicalize(t);
}

void
update_in(AVar *v, AType *t) {
  AType *tt = type_union(v->in, t);
  if (tt != v->in) {
    assert(tt && tt != top_type);
    v->in = tt;
    if (v->restrict)
      tt = type_intersection(v->in, v->restrict);
    if (tt != v->out) {
      assert(tt != top_type);
      v->out = tt;
      forv_AVar(vv, v->arg_of_send.asvec) {
        if (!vv->in_send_worklist) {
          vv->in_send_worklist = 1;
          send_worklist.enqueue(vv);
        }
      }
      forv_AVar(vv, v->forward) if (vv)
        update_in(vv, tt);
    }
  }
}

void
update_gen(AVar *v, AType *t) {
  if (v->gen) {
    AType *tt = type_union(v->gen, t);
    if (tt == v->gen)
      return;
    v->gen = tt;
  } else
    v->gen = t;
  update_in(v, v->gen);
}

static void
flow_var_to_var(AVar *a, AVar *b) {
  if (a == b)
    return;
  if (a->forward.set_in(b))
    return;
  a->forward.set_add(b);
  b->backward.set_add(a);
  update_in(b, a->out);
}

void
flow_vars(AVar *v, AVar *vv) {
  if (v->lvalue) {
    if (vv->lvalue) {
      flow_var_to_var(v, vv);
      flow_var_to_var(vv->lvalue, v->lvalue);
    } else {
      flow_var_to_var(v, vv);
      flow_var_to_var(vv, v->lvalue);
    }
  } else {
    if (vv->lvalue) {
      flow_var_to_var(v, vv);
      flow_var_to_var(vv->lvalue, v);
    } else
      flow_var_to_var(v, vv);
  }
}

void
flow_vars_assign(AVar *rhs, AVar *lhs) {
  flow_var_to_var(rhs, lhs);
  if (lhs->lvalue)
    flow_var_to_var(rhs, lhs->lvalue);
}

CreationSet *
creation_point(AVar *v, Sym *s, int nvars) {
  CreationSet *cs = v->cs_map ? v->cs_map->get(s) : 0;
  EntrySet *es = (EntrySet*)v->contour;
  if (cs) {
    assert(cs->sym == s);
    goto Lfound;
  }
  if (s == sym_closure)
    goto Lunique;
  if ((void*)es == GLOBAL_CONTOUR)
    es = 0;
  if (es && es->split) {
    AVar *oldv = make_AVar(v->var, es->split);
    cs = oldv->cs_map ? oldv->cs_map->get(s) : 0;
    if (cs) {
      assert(cs->sym == s);
      goto Lfound;
    }
  }
  forv_CreationSet(x, s->creators) {
    if (s->abstract_type && x == s->abstract_type->v[0])
      continue;
    if (nvars != -1 || x->vars.n != nvars)
      continue;
    cs = x;
    goto Lfound;
  }
 Lunique:
  // new creation set
  cs = new CreationSet(s);
  s->creators.add(cs);
  forv_Sym(h, s->has) {
    assert(h->var);
    AVar *iv = unique_AVar(h->var, cs);
    add_var_constraint(iv);
    cs->vars.add(iv);
    if (h->name)
      cs->var_map.put(h->name, iv);
  }
 Lfound:
  if (!v->cs_map)
    v->cs_map = new CSMap;
  v->cs_map->put(s, cs);
  cs->defs.set_add(v);
  if (v->contour_is_entry_set)
    ((EntrySet*)v->contour)->creates.set_add(cs);
  update_gen(v, make_AType(cs));
  return cs;
}

//  all float combos become doubles
//  all signed/unsigned combos become signed
//  all int combos below 32 bits become signed 32 bits, above become signed 64 bits
Sym *
coerce_num(Sym *a, Sym *b) {
  if (a == b)
    return a;
  if (a == sym_string || b == sym_string)
    return sym_string;
  if (a->num_kind == b->num_kind) {
    if (a->num_index > b->num_index)
      return a;
    else
      return b;
  }
  if (b->num_kind == IF1_NUM_KIND_FLOAT) {
    Sym *t = b; b = a; a = t;
  }
  if (b->num_kind == IF1_NUM_KIND_COMPLEX) {
    Sym *t = b; b = a; a = t;
  }
  if (a->num_kind == IF1_NUM_KIND_COMPLEX) {
    if (b->num_kind == IF1_NUM_KIND_FLOAT) {
      if (a->num_index > b->num_index)
        return a;
      return if1->complex_types[b->num_index];
    }
    if (int_type_precision[b->num_kind] <= float_type_precision[a->num_kind])
      return a;
    if (int_type_precision[b->num_kind] >= 32)
      return sym_complex32;
    return sym_complex64;
  }
  if (a->num_kind == IF1_NUM_KIND_FLOAT) {
    if (int_type_precision[b->num_kind] <= float_type_precision[a->num_kind])
      return a;
    if (int_type_precision[b->num_kind] >= 32)
      return sym_float32;
    return sym_float64;
  }
  // mixed signed and unsigned
  if (a->num_index >= IF1_INT_TYPE_64 || b->num_index >= IF1_INT_TYPE_64)
    return sym_int64;
  else if (a->num_index >= IF1_INT_TYPE_32 || b->num_index >= IF1_INT_TYPE_32)
    return sym_int32;
  else if (a->num_index >= IF1_INT_TYPE_16 || b->num_index >= IF1_INT_TYPE_16)
    return sym_int16;
  else if (a->num_index >= IF1_INT_TYPE_8 || b->num_index >= IF1_INT_TYPE_8)
    return sym_int8;
  return sym_bool;
}

AType *
type_num_fold(Prim *p, AType *a, AType *b) {
  (void) p; p = 0; // for now
  a = type_intersection(a, anynum_kind);
  b = type_intersection(b, anynum_kind);
  ATypeFold f(p, a, b), *ff;
  if ((ff = type_fold_cache.get(&f)))
    return ff->result;
  AType *r = new AType();
  forv_CreationSet(acs, a->sorted) {
    Sym *atype = acs->sym->type;
    forv_CreationSet(bcs, b->sorted) {
      Sym *btype = bcs->sym->type;
      r->set_add(coerce_num(atype, btype)->abstract_type->v[0]);
    }
  }
  r = type_cannonicalize(r);
  type_fold_cache.put(new ATypeFold(p, a, b, r));
  return r;
}

void
qsort_pointers(void **left, void **right) {
 Lagain:
  if (right - left < 5) {
    for (void **y = right - 1; y > left; y--) {
      for (void **x = left; x < y; x++) {
        if (x[0] > x[1]) {
          void *t = x[0];
          x[0] = x[1];
          x[1] = t;
        } 
      } 
    }
  } else {
    void  **i = left + 1, **j = right - 1, *x = *left;
    for (;;) {
      while (x < *j) j--;
      while (i < j && *i < x) i++;
      if (i >= j) break;
      void *t = *i;
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
    if (left < j) qsort_pointers(left, j + 1);
    if (j + 2 < right) qsort_pointers(j + 1, right);
  }
}

AType *
type_cannonicalize(AType *t) {
  assert(!t->sorted.n);
  assert(!t->union_map.n);
  assert(!t->intersection_map.n);
  int consts = 0, rebuild = 0, nulls = 0;
  Vec<CreationSet *> nonconsts;
  forv_CreationSet(cs, *t) if (cs) {
    // strip out constants if the base type is included
    CreationSet *base_cs = 0;
    if (cs->sym->is_constant || (cs->sym->type->num_kind && cs->sym != cs->sym->type))
      base_cs = cs->sym->type->abstract_type->v[0];
    else if (cs->sym->type_kind == Type_TAGGED)
      base_cs = cs->sym->type->specializes[0]->abstract_type->v[0];
    if (base_cs) {
      if (t->set_in(base_cs)) {
        rebuild = 1;
        continue;
      }
      consts++;
      nonconsts.set_add(base_cs);
    } else { 
      if (!cs->sym->is_unique_type)
        nonconsts.set_add(cs);
      else
        nulls = 1;
    }
    t->sorted.add(cs);
  }
  if (consts > fa->num_constants_per_variable)
    rebuild = 1;
  if (rebuild) {
    t->sorted.clear();
    t->sorted.append(nonconsts);
    t->clear();
    t->set_union(t->sorted);
  }
  if (t->sorted.n > 1)
    qsort_by_id(t->sorted);
  unsigned int h = 0;
  for (int i = 0; i < t->sorted.n; i++)
    h = (uint)(intptr_t)t->sorted[i] * open_hash_primes[i % 256];
  t->hash = h ? h : h + 1; // 0 is empty
  AType *tt = cannonical_atypes.put(t);
  if (!tt) tt = t;
  // compute "type" (without constants)
  if (nonconsts.n) {
    if (nulls || consts)
      tt->type = make_AType(nonconsts);
    else
      tt->type = tt;
  } else
    tt->type = bottom_type;
  return tt;
}

AType *
type_union(AType *a, AType *b) {
  AType *r;
  if ((r = a->union_map.get(b)))
    return r;
  if (a == b || b == bottom_type) {
    r = a;
    goto Ldone;
  }
  if (a == bottom_type) {
    r = b;
    goto Ldone;
  }
  {
    AType *ab = type_diff(a, b);
    AType *ba = type_diff(b, a);
    r = new AType(*ab);
    forv_CreationSet(x, ba->sorted)
      r->set_add(x);
    forv_CreationSet(x, a->sorted)
      if (b->in(x))
        r->set_add(x);
    r = type_cannonicalize(r);
  }
 Ldone:
  a->union_map.put(b, r);
  return r;
}

static inline int
subsumed_by(Sym *a, Sym *b) {
  return (a == b) || a->type == b || 
    ((b->type_kind == Type_SUM || a->is_symbol || a->is_fun) && b->specializers.set_in(a->type));
}

AType *
type_diff(AType *a, AType *b) {
  AType *r;
  if ((r = a->diff_map.get(b)))
    return r;
  if (b == bottom_type) {
    r = a;
    goto Ldone;
  }
  r = new AType();
  forv_CreationSet(aa, a->sorted) {
    if (aa->defs.n && b->set_in(aa))
      continue;
    forv_CreationSet(bb, b->sorted) if (!bb->defs.n) {
      if (subsumed_by(aa->sym, bb->sym))
        goto Lnext;
    }
    r->set_add(aa);
  Lnext:;
  }
  r = type_cannonicalize(r);
 Ldone:
  a->diff_map.put(b, r);
  return r;
}

AType *
type_intersection(AType *a, AType *b) {
  AType *r;
  if ((r = a->intersection_map.get(b)))
    return r;
  if (a == b || a == bottom_type || b == top_type) {
    r = a;
    goto Ldone;
  }
  if (a == top_type || b == bottom_type) {
    r = b;
    goto Ldone;
  }
  r = new AType();
  forv_CreationSet(aa, a->sorted) {
    forv_CreationSet(bb, b->sorted) {
      if (aa->defs.n) {
        if (bb->defs.n) {
          if (aa == bb) {
            r->set_add(aa);
            goto Lnexta;
          }
        } else {
          if (subsumed_by(aa->sym, bb->sym)) {
            r->set_add(aa);
            goto Lnexta;
          }
        }
      } else {
        if (bb->defs.n) {
          if (subsumed_by(bb->sym, aa->sym))
            r->set_add(bb);
        } else {
          if (subsumed_by(aa->sym, bb->sym)) {
            r->set_add(aa);
            goto Lnexta;
          } else if (subsumed_by(bb->sym, aa->sym))
            r->set_add(bb);
        }
      }
    }
  Lnexta:;
  }
  r = type_cannonicalize(r);
 Ldone:
  a->intersection_map.put(b, r);
  return r;
}

static void
fill_rets(EntrySet *es, int n) {
  es->fun->rets.fill(n);
  es->rets.fill(n);
  for (int i = 0; i < n; i++)
    if (!es->rets[i]) {
      if (!i)
        es->rets[i] = make_AVar(es->fun->sym->ret->var, es);
      else {
        if (!es->fun->rets[i]) {
          Var *v = new Var(es->fun->sym->ret);
          es->fun->rets[i] = v;
          es->fun->fa_all_Vars.add(v);
        }
        es->rets[i] = make_AVar(es->fun->rets.v[i], es);
      }
    }
}

static int 
same_eq_classes(Setters *s, Setters *ss) {
  if (s == ss)
    return 1;
  if (!s || !ss)
    return 0;
  Vec<Setters *> sc1, sc2;
  forv_AVar(av, *s) if (av) {
    assert(av->setter_class);
    sc1.set_add(av->setter_class);
  }
  forv_AVar(av, *ss) if (av) {
    assert(av->setter_class);
    sc2.set_add(av->setter_class);
  }
  if (sc1.some_disjunction(sc2))
    return 0;
  return 1;
}

static int
edge_nest_compatible_with_entry_set(AEdge *e, EntrySet *es) {
  if (!es->fun->sym->nesting_depth)
    return 1;
  int ef_nd = e->from->fun->sym->nesting_depth, es_nd = es->fun->sym->nesting_depth;
  int n = ef_nd < es_nd ? ef_nd : es_nd; // MIN
  for (int i = 0; i < n; i++)
    if (e->from->display[i] != es->display.v[i])
      return 0;
  if (ef_nd < es_nd) // down call
    if (es->display[es_nd - 1] != e->from)
      return 0;
  return 1;
}

static int 
different_marked_args(AVar *a1, AVar *a2, int offset, AVar *basis = 0) {
  Vec<void *> marks1, marks2;
  AVar *basis1 = basis ? basis : a2;
  int found1 = 0, found2 = 0;
  if (a1->mark_map) {
    form_Map(MarkElem, x, *a1->mark_map) {
      if (basis1->mark_map) {
        int m = basis1->mark_map->get(x->key);
        if (m) {
          found1 = 1;
          if (m - offset == x->value)
            marks1.set_add(x->key);
        }
      }
    }
  }
  if (a2->mark_map) {
    form_Map(MarkElem, x, *a2->mark_map) {
      if (basis) {
        if (basis->mark_map) {
          int m = basis->mark_map->get(x->key);
          if (m) {
            found2 = 1;
            if (m - offset == x->value)
              marks2.set_add(x->key);
          }
        }
      } else {
        found2 = 1;
        marks2.set_add(x->key);
      }
    }
  }
  return found1 && found2 && marks1.some_disjunction(marks2);
}

static int
edge_type_compatible_with_edge(AEdge *e, AEdge *ee, EntrySet *es, int fmark = 0) {
  assert(e->args.n && ee->args.n);
  forv_MPosition(p, e->match->fun->positional_arg_positions) {
    AVar *e_arg = e->args.get(p), *ee_arg = ee->args.get(p);
    if (!e_arg || !ee_arg)
      continue;
    AType *etype = type_intersection(e_arg->out->type, e->match->formal_filters.get(p));
    AType *eetype = type_intersection(ee_arg->out->type, ee->match->formal_filters.get(p));
    if (!fmark) {
      if (etype->n && eetype->n && etype != eetype)
        return 0;
    } else {
      AVar *es_arg = es->args.get(p);
      if (different_marked_args(ee_arg, e_arg, 2, es_arg))
        return 0;
    }
  }
  if (e->rets.n != ee->rets.n)
    return 0;
  for (int i = 0; i < e->rets.n; i++) {
    if (ee->rets[i]->lvalue && e->rets.v[i]->lvalue) {
      if (!fmark) {
        if (ee->rets[i]->lvalue->out->type->n && e->rets.v[i]->lvalue->out->type->n &&
            ee->rets[i]->lvalue->out->type != e->rets.v[i]->lvalue->out->type)
          return 0;
      } else {
        if (different_marked_args(ee->rets[i]->lvalue, e->rets.v[i]->lvalue, 1,
                                  es->rets[i]->lvalue))
          return 0;
      }
    }
  }
  return 1;
}

static int
edge_type_compatible_with_entry_set(AEdge *e, EntrySet *es, int fmark = 0) {
  assert(e->args.n && es->args.n);
  if (!es->split) {
    forv_MPosition(p, e->match->fun->positional_arg_positions) {
      AVar *es_arg = es->args.get(p), *e_arg = e->args.get(p);
      if (!e_arg)
        continue;
      AType *etype = type_intersection(e_arg->out->type, e->match->formal_filters.get(p));
      if (!fmark) {
        if (etype->n && es_arg->out->type->n && etype != es_arg->out->type)
          return 0;
      } else
        if (different_marked_args(e_arg, es_arg, 2))
          return 0;
    }
    if (es->rets.n != e->rets.n)
      return 0;
    for (int i = 0; i < e->rets.n; i++) {
      if (es->rets[i]->lvalue && e->rets.v[i]->lvalue) {
        if (!fmark) {
          if (es->rets[i]->lvalue->out->type->n && e->rets.v[i]->lvalue->out->type->n &&
              es->rets[i]->lvalue->out->type != e->rets.v[i]->lvalue->out->type)
            return 0;
        } else
          if (different_marked_args(es->rets[i]->lvalue, e->rets.v[i]->lvalue, 1))
            return 0;
      }
    }
  } else {
    forv_AEdge(ee, es->edges) if (ee) {
      if (!ee->args.n)  
        continue;
      if (!edge_type_compatible_with_edge(e, ee, es, fmark))
        return 0;
    }
  }
  return 1;
}

static int 
sset_compatible(AVar *av1, AVar *av2) {
  if (!same_eq_classes(av1->setters, av2->setters))
    return 0;
  if (av1->lvalue && av2->lvalue)
    if (!same_eq_classes(av1->lvalue->setters, av2->lvalue->setters))
      return 0;
  return 1;
}

static int
edge_sset_compatible_with_edge(AEdge *e, AEdge *ee) {
  assert(e->args.n && ee->args.n);
  forv_MPosition(p, e->match->fun->positional_arg_positions) {
    AVar *eav = e->args.get(p), *eeav = ee->args.get(p);
    if (eav && eeav)
      if (!sset_compatible(eav, eeav))
        return 0;
  }
  if (e->rets.n != ee->rets.n)
    return 0;
  for (int i = 0; i < e->rets.n; i++)
    if (!sset_compatible(e->rets[i], ee->rets.v[i]))
      return 0;
  return 1;
}

static int
edge_sset_compatible_with_entry_set(AEdge *e, EntrySet *es) {
  assert(e->args.n && es->args.n);
  if (!es->split) {
    forv_MPosition(p, e->match->fun->positional_arg_positions) {
      AVar *av = e->args.get(p);
      if (av)
        if (!sset_compatible(av, es->args.get(p)))
          return 0;
    }
    if (es->rets.n != e->rets.n)
      return 0;
    for (int i = 0; i < es->rets.n; i++)
      if (!sset_compatible(e->rets[i], es->rets.v[i]))
        return 0;
  } else {
    forv_AEdge(ee, es->edges) if (ee) {
      if (!ee->args.n)  
        continue;
      if (!edge_sset_compatible_with_edge(e, ee))
        return 0;
    }
  }
  return 1;
}

static int
edge_constant_compatible_with_entry_set(AEdge *e, EntrySet *es) {
  forv_MPosition(p, e->match->fun->positional_arg_positions) {
    AVar *av = es->args.get(p);
    if (av->var->sym->clone_for_constants) {
      AType css;
      av->out->set_disjunction(*e->args.get(p)->out, css);
      forv_CreationSet(cs, css)
        if (cs->sym->constant)
          return 0;
    }
  }
  return 1;
}

static void
update_display(AEdge *e, EntrySet *es) {
  // add any we need
  for (int i = es->display.n; i < es->fun->sym->nesting_depth; i++)
    if (i < e->from->display.n)
      es->display.add(e->from->display[i]);
    else
      es->display.add(e->from);
  // verify everything
  for (int i = 0; i < es->fun->sym->nesting_depth; i++)
    if (i < e->from->display.n)
      assert(es->display[i] == e->from->display.v[i]);
    else
      assert(es->display[i] == e->from);
}

static void
set_entry_set(AEdge *e, EntrySet *es = 0) {
  EntrySet *new_es = es;
  if (!es) {
    new_es = new EntrySet(e->match->fun);
    e->match->fun->ess.add(new_es);
  }
  e->to = new_es;
  new_es->edges.put(e);
  if (new_es->fun->sym->nesting_depth)
    update_display(e, new_es);
  forv_MPosition(p, e->match->fun->positional_arg_positions) {
    Var *v = e->match->fun->args.get(p);
    AVar *av = make_AVar(v, new_es);
    new_es->args.put(p, av);
  }
  fill_rets(new_es, e->pnode->lvals.n);
}

static AEdge *
new_AEdge(Fun *f, PNode *p, EntrySet *from) {
  AEdge *e = new AEdge;
  e->pnode = p;
  e->from = from;
  e->fun = f;
  return e;
}

static AEdge *
new_AEdge(Match *m, PNode *p, EntrySet *from) {
  AEdge *e = new AEdge;
  e->pnode = p;
  e->from = from;
  e->fun = m->fun;
  e->match = m;
  return e;
}

static AEdge *
copy_AEdge(AEdge *ee, EntrySet *to) {
  AEdge *e = new_AEdge(ee->match, ee->pnode, ee->from);
  set_entry_set(e, to);
  if (!e->args.n) e->args.copy(ee->args);
  if (!e->rets.n) e->rets.copy(ee->rets);
  Vec<AEdge *> *ve = ee->from->out_edge_map.get(ee->pnode);
  if (!ve)
    ee->from->out_edge_map.put(ee->pnode, (ve = new Vec<AEdge *>));
  ve->set_add(e);
  return e;
}

#if 0
static int
initial_compatibility(AEdge *e, EntrySet *es) {
  forv_AEdge(ee, es->edges) if (ee)
    forv_MPosition(p, e->match->fun->positional_arg_positions)
      if (e->initial_types.get(p) != ee->initial_types.get(p))
        return 0;
  return 1;
}
#endif

static int
entry_set_compatibility(AEdge *e, EntrySet *es) {
  int val = INT_MAX;
  if (e->match->fun->split_unique)
    return 0;
  if (!edge_nest_compatible_with_entry_set(e, es))
    return 0;
  switch (edge_type_compatible_with_entry_set(e, es)) {
    case 1: break;
    case 0: 
#if 0
      // eager splitting doesn't help
      if (analysis_pass == 0 && !initial_compatibility(e, es))
        return 0;
#endif
      val -= 4; 
      break;
    case -1: return 0;
  }
  if (!edge_sset_compatible_with_entry_set(e, es))
    val -= 2;
  if (e->match->fun->clone_for_constants)
    if (!edge_constant_compatible_with_entry_set(e, es))
      val -= 1;
  return val;
}

static AEdge *
set_or_copy_AEdge(AEdge *e, EntrySet *es, Vec<AEdge *> &ees)  {
  if (!ees.n) {
    set_entry_set(e, es);
    ees.add(e);
    return e;
  } else {
    AEdge *new_e = copy_AEdge(e, es);
    ees.add(new_e);
    return new_e;
  }
}

static int
find_best_entry_sets(AEdge *e, Vec<AEdge *> &edges) {
  EntrySet *es = NULL;
  int val = -1;
  forv_EntrySet(x, e->match->fun->ess) {
    int v = entry_set_compatibility(e, x);
    if (v > 0 && v > val) {
      es = x;
      if (v == INT_MAX)
        break;
      val = v;
    }
  }
  if (es) {
    set_or_copy_AEdge(e, es, edges);
    return 1;
  }
  return 0;
}

static int
check_edge(AEdge *e, EntrySet *es) {
  form_MPositionAVar(x, e->args) {
    if (!x->key->is_positional())
      continue;
    AType *filter = e->match->formal_filters.get(x->key);
    AType *es_filter = es->filters.get(x->key);
    if (filter) {
      if (es_filter)
        filter = type_intersection(filter, es_filter);
    } else 
      filter = es_filter;
    if (filter && type_intersection(x->value->out, filter) == bottom_type)
      return 0;
  }
  return 1;
}

static int
check_split(AEdge *e, Vec<AEdge *> &ees)  {
  if (!e->from)
    return 0;
  if (Vec<EntrySet *> *ess = e->from->pending_es_backedge_map.get(e)) {
    forv_EntrySet(es, *ess)
      set_or_copy_AEdge(e, es, ees);
    return 1;
  }
  if (e->from->split) {
    Vec<AEdge *> *m = e->from->split->out_edge_map.get(e->pnode);
    if (m) {
      forv_AEdge(ee, *m) if (ee) {
        if (!check_edge(e, ee->to))
          continue;
        if (ee->match->fun == e->match->fun) {
         if (e->match->fun->split_unique || !edge_nest_compatible_with_entry_set(e, ee->to)) {
            set_entry_set(e);
            e->to->split = ee->to;
            ees.add(e);
            return 1;
          } else
            set_or_copy_AEdge(e, ee->to, ees);
        }
      }
      if (ees.n)
        return 1;
    }
  }
  return 0;
}

// check results of last session read from disk
static int
check_es_db(AEdge *e, Vec<AEdge *> &ees) {
  return 0;
}

static void 
make_entry_set(AEdge *e, Vec<AEdge *> &edges, EntrySet *split = 0, EntrySet *preference = 0) {
  if (e->to) { 
    edges.add(e);
    return;
  }
  if (check_split(e, edges)) return;
  if (check_es_db(e, edges)) return;
  EntrySet *es = 0;
  if (!split) {
    if (find_best_entry_sets(e, edges))
      return;
  }
  if (!es)
    es = preference;
  set_entry_set(e, es);
  if (!es)
    e->to->split = split;
  edges.add(e);
}

void
flow_var_type_permit(AVar *v, AType *t) {
  if (!v->restrict)
    v->restrict = t;
  else
    v->restrict = type_union(t, v->restrict);
  AType *tt = type_intersection(v->in, v->restrict);
  if (tt != v->out) {
    assert(tt != top_type);
    v->out = tt;
    forv_AVar(vv, v->arg_of_send.asvec) {
      if (!vv->in_send_worklist) {
        vv->in_send_worklist = 1;
        send_worklist.enqueue(vv);
      }
    }
    forv_AVar(vv, v->forward) if (vv)
      update_in(vv, v->out);
  }
}

static inline void
flow_var_type_permit(AVar *v, Sym *s) { 
  flow_var_type_permit(v, make_abstract_type(s)); 
}

void
add_var_constraint(AVar *av, Sym *s) {
  if (!s)
    s = av->var->sym;
  assert(s->type_kind != Type_VARIABLE);
  if (s->type && !s->is_pattern) {
    if (s->is_external && 
        (s->type->num_kind || s->type == sym_string || s->type->is_system_type))
      update_gen(av, s->type->abstract_type);
    if (s->is_constant) // for constants, the abstract type is the concrete type
      update_gen(av, make_abstract_type(s));
    if (s->is_symbol || s->is_fun) 
      update_gen(av, make_abstract_type(s));
    if (s->type_kind != Type_NONE)
      update_gen(av, make_abstract_type(s->meta_type));
  }
}

static void
add_var_constraints(EntrySet *es) {
  forv_Var(v, es->fun->fa_Vars)
    add_var_constraint(make_AVar(v, es));
}

AVar *
get_element_avar(CreationSet *cs) {
  if (!cs->sym->element)
    return 0;
  AVar *elem = unique_AVar(cs->sym->element->var, cs);
  cs->added_element_var = 1;
  return elem;
}

void
set_container(AVar *av, AVar *container) {
  assert(!av->container || av->container == container);
  av->container = container;
  if (av->lvalue)
    av->lvalue->container = container;
}

void
fill_tvals(Fun *fn, PNode *p, int n) {
  p->tvals.fill(n);
  for (int i = 0; i < n; i++) {
    if (!p->tvals[i]) {
      Sym *s = new_Sym();
      s->nesting_depth = fn->sym->nesting_depth + 1;
      s->in = fn->sym;
      p->tvals[i] = new Var(s);
      p->tvals[i]->is_internal = 1;
      s->var = p->tvals[i];
      fn->fa_all_Vars.add(p->tvals[i]);
    }
  }
}

static void
make_kind(PNode *p, EntrySet *es, Sym *kind, AVar *container, Vec<Var *> *vars, Vec<AVar *> *avars, int vstart, int tstart, int l)  {
  CreationSet *cs = creation_point(container, kind, l);
  cs->vars.fill(l);
  for (int i = 0; i < l; i++) {
    AVar *av = 0;
    if (avars)
      av = avars->v[vstart + i];
    else
      av = make_AVar(vars->v[vstart + i], es);
    Var *tv = p->tvals[tstart + i];
    tv->sym->is_lvalue = av->var->sym->is_lvalue;
    if (!cs->vars[i])
      cs->vars[i] = unique_AVar(av->var, cs);
    AVar *iv = cs->vars[i];
    AVar *atv = make_AVar(tv, es);
    set_container(atv, container);
    flow_vars(av, atv);
    flow_vars(atv, iv);
    if (iv->var->sym->name)
      cs->var_map.put(iv->var->sym->name, iv);
  }
}

void
prim_make(PNode *p, EntrySet *es, Sym *kind, int start, int ref) {
  assert(!ref); // unimplemented
  AVar *container = make_AVar(p->lvals[0], es);
  int l = p->rvals.n - start;
  fill_tvals(es->fun, p, l);
  make_kind(p, es, kind, container, &p->rvals, 0, start, 0, l);
}

static void
vector_elems(int rank, PNode *p, AVar *ae, AVar *elem, AVar *container, int n = 0) {
  AVar *e = ae;
  if (!e->contour_is_entry_set) {
    p->tvals.fill(++n);
    assert(container->contour_is_entry_set);
    EntrySet *es = (EntrySet*)container->contour;
    if (p->tvals[n-1])
      e = make_AVar(p->tvals[n-1], es);
    else {
      Sym *s = new_Sym();
      s->nesting_depth = es->fun->sym->nesting_depth + 1;
      assert(!e->var->sym->is_lvalue);
      s->in = es->fun->sym;
      Var *v = new Var(s);
      s->var = v;
      p->tvals[n-1] = v;
      es->fun->fa_all_Vars.add(v);
      e = make_AVar(v, es);
    }
    flow_vars(ae, e);
  }
  set_container(e, container);
  if (rank > 0) {
    forv_CreationSet(cs, e->out->sorted) {
      if (cs->sym != sym_tuple)
        flow_vars(e, elem);
      else {
        e->arg_of_send.add(container);
        forv_AVar(av, cs->vars)
          vector_elems(rank - 1, p, av, elem, container, n+1);
      }
    }
  } else
    flow_vars(e, elem);
}

static void
prim_make_vector(PNode *p, EntrySet *es) {
  AVar *container = make_AVar(p->lvals[0], es);
  CreationSet *cs = creation_point(container, sym_vector);
  AVar *elem = get_element_avar(cs);
  if (p->rvals.n > 2) {
    int rank = 0;
    p->rvals[2]->sym->imm_int(&rank);
    for (int i = 0; i < p->rvals.n - 3; i++) {
      Var *v = p->rvals[2 + i];
      AVar *av = make_AVar(v, es);
      vector_elems(rank, p, av, elem, container);
    }
  }
}

static void
make_closure_var(AVar *av, EntrySet *es, CreationSet *cs, AVar *result, int add, int i) {
  AVar *iv = unique_AVar(av->var, cs);
  PNode *pn = result->var->def;
  if (!pn->tvals[i]) {
    pn->tvals[i] = new Var(av->var->sym);
    pn->tvals[i]->is_internal = 1;
    es->fun->fa_all_Vars.add(pn->tvals[i]);
  }
  AVar *cav = make_AVar(pn->tvals[i], es);
  flow_vars(av, cav);
  set_container(cav, result);
  flow_var_to_var(cav, iv);
  if (add)
    cs->vars.add(iv);
}

static void
make_closure_var(Var *v, EntrySet *es, CreationSet *cs, AVar *result, int add, int i) {
  make_closure_var(make_AVar(v, es), es, cs, result, add, i);
}

static void
make_closure(AVar *result) {
  assert(result->contour_is_entry_set);
  PNode *pn = result->var->def;
  PNode *partial_application = result->var->def;
  CreationSet *cs = creation_point(result, sym_closure, partial_application->rvals.n);
  int add = !cs->vars.n;
  EntrySet *es = (EntrySet*)result->contour;
  pn->tvals.fill(partial_application->rvals.n);
  for (int i = 0; i < partial_application->rvals.n; i++)
    make_closure_var(partial_application->rvals[i], es, cs, result, add, i);
}

static void
make_period_closure(AVar *result, AVar *a, Vec<AVar *> &args) {
  assert(result->contour_is_entry_set);
  PNode *pn = result->var->def;
  PNode *partial_application = result->var->def;
  CreationSet *cs = creation_point(result, sym_closure, partial_application->rvals.n);
  flow_var_type_permit(result, make_AType(cs));
  EntrySet *es = (EntrySet*)result->contour;
  pn->tvals.fill(args.n);
  int add = !cs->vars.n;
  make_closure_var(a, es, cs, result, add, 0);
  for (int i = 0; i < args.n; i++)
    make_closure_var(args[i], es, cs, result, add, i+1);
}

// for send nodes, add simple constraints which do not depend 
// on the computed types (compare to add_send_edgse_pnodes)
static void
add_send_constraints(EntrySet *es) {
  forv_PNode(p, es->fun->fa_send_PNodes) {
    if (p->prim) {
      int start = 1;
      // return constraints
      for (int i = 0; i < p->lvals.n; i++) {
        int ii = i;
        if (p->prim->nrets < 0 || p->prim->nrets <= i)
          ii = -p->prim->nrets -1; // last
        switch (p->prim->ret_types[ii]) {
          case PRIM_TYPE_ANY: break;
          case PRIM_TYPE_BOOL: update_gen(make_AVar(p->lvals[i], es), bool_type); break;
          case PRIM_TYPE_STRING: update_gen(make_AVar(p->lvals[i], es), string_type); break;
          case PRIM_TYPE_SIZE: update_gen(make_AVar(p->lvals[i], es), size_type); break;
          case PRIM_TYPE_ANY_NUM_AB:
          case PRIM_TYPE_ANY_NUM_A:
          case PRIM_TYPE_ANY_NUM_B:
          case PRIM_TYPE_A: {
            for (int j = start; j < p->rvals.n; j++)
              if (j - start != p->prim->pos) {
                AVar *av = make_AVar(p->rvals[j], es), *res = make_AVar(p->lvals.v[0], es);
                av->arg_of_send.add(res);
              }
            break;
          }
          default: assert(!"case"); break;
        }
      }
      // specifics
      switch (p->prim->index) {
        default: break;
        case P_prim_reply:
          fill_rets(es, p->rvals.n - 3);
          for (int i = 3; i < p->rvals.n; i++) {
            AVar *r = make_AVar(p->rvals[i], es);
            flow_vars(r, es->rets[i - 3]);
          }
          break;
        case P_prim_tuple: prim_make(p, es, sym_tuple); break;
        case P_prim_list: prim_make(p, es, sym_list); break;
        case P_prim_vector: prim_make_vector(p, es); break;
        case P_prim_continuation: prim_make(p, es, sym_continuation); break;
        case P_prim_set: prim_make(p, es, sym_set); break;
        case P_prim_ref: prim_make(p, es, sym_ref, 3, 1); break;
      }
    }
  }
}

static void
add_move_constraints(EntrySet *es) {
  Fun *f = es->fun;
  forv_PNode(p, f->fa_phi_PNodes) {
    AVar *vv = make_AVar(p->lvals[0], es);
    forv_Var(v, p->rvals)
      flow_vars(make_AVar(v, es), vv);
  }
  forv_PNode(p, f->fa_phy_PNodes) {
    AVar *vv = make_AVar(p->rvals[0], es);
    forv_Var(v, p->lvals)
      flow_vars(vv, make_AVar(v, es));
  }
  forv_PNode(p, f->fa_move_PNodes) {
    for (int i = 0; i < p->rvals.n; i++) {
      AVar *lhs = make_AVar(p->lvals[i], es), *rhs = make_AVar(p->rvals.v[i], es);
      if (lhs->lvalue && rhs->lvalue)
        flow_vars(rhs, lhs);
      else
        flow_vars_assign(rhs, lhs);
    }
  }
}

static void
get_AEdges(Fun *f, PNode *p, EntrySet *from, Vec<AEdge *> &edges) {
  Vec<AEdge *> *ve = from->out_edge_map.get(p);
  if (!ve)
    from->out_edge_map.put(p, (ve = new Vec<AEdge *>));
  forv_AEdge(e, *ve) if (e) {
    if (f == e->fun)
      edges.add(e);
  }
  if (!edges.n) {
    AEdge *e = new_AEdge(f, p, from);
    ve->set_add(e);
    edges.add(e);
  }
}

static void
record_arg(PNode *pn, CreationSet *cs, AVar *a, Sym *s, AEdge *e, MPosition &p) {
  MPosition *cp = cannonicalize_mposition(p);
  e->args.put(cp, a);
  AType *t = type_intersection(a->out, e->match->formal_filters.get(cp));
  e->initial_types.put(cp, t->type);
  if (s->is_pattern) {
    forv_CreationSet(cs, t->sorted) {
      assert(s->has.n == cs->vars.n);
      p.push(1);
      for (int i = 0; i < s->has.n; i++) {
        record_arg(pn, cs, cs->vars[i], s->has.v[i], e, p);
        p.inc();
      }
      p.pop();
    }
  }
}

static void
record_args_rets(AEdge *e, Vec<AVar *> &a) {
  if (!e->args.n) {
    MPosition p;
    p.push(1);
    for (int i = 0; i < e->fun->sym->has.n; i++) {
      record_arg(e->pnode, 0, a[i], e->fun->sym->has.v[i], e, p);
      p.inc();
    }
  }
  if (!e->rets.n) {
    for (int i = 0; i < e->pnode->lvals.n; i++)
      e->rets.add(make_AVar(e->pnode->lvals[i], e->from));
  }
}

static void
make_AEdges(Match *m, PNode *p, EntrySet *from, Vec<AVar *> &args) {
  Vec<AEdge *> edges;
  get_AEdges(m->fun, p, from, edges);
  forv_AEdge(e, edges) {
    if (!e->match)
      e->match = m;
    else
      e->match->merge(m);
    record_args_rets(e, args);
    if (!e->in_edge_worklist) {
      e->in_edge_worklist = 1;
      edge_worklist.enqueue(e);
    }
  }
}

// returns 1 if any are partial, 0 if some matched and -1 if none matched
static int
all_applications(PNode *p, EntrySet *es, AVar *a0, Vec<AVar *> &args, Vec<cchar *> &names,
                 int is_closure, Partial_kind partial, 
                 PNode *visibility_point = 0, Vec<CreationSet *> *closures = 0) 
{
  if (!visibility_point) visibility_point = p;
  int incomplete = -2;
  a0->arg_of_send.add(make_AVar(p->lvals[0], es));
  forv_CreationSet(cs, a0->out->sorted)
    switch (application(p, es, a0, cs, args, names, is_closure, partial, visibility_point, closures)) {
      case -1: if (incomplete < 0) incomplete = -1; break;
      case 0: if (incomplete < 0) incomplete = 0; break;
      case 1: incomplete = 1; break;
    }
  return incomplete;
}

static int
partial_application(PNode *p, EntrySet *es, CreationSet *cs, 
                    Vec<AVar *> &args, Vec<cchar *> &names,
                    Partial_kind partial, PNode *visibility_point,
                    Vec<CreationSet *> *closures) 
{
  AVar *result = make_AVar(p->lvals[0], es);
  assert(result->var->def == p);
  AVar *fun = cs->vars[0];
  Vec<AVar *> a;
  a.copy(args);
  PNode *def = cs->defs[0]->var->def;
  for (int i = cs->vars.n - 1; i >= 1; i--) {
    cs->vars[i]->arg_of_send.add(result);
    a.add(cs->vars[i]);
  }
  Vec<cchar *> n;
  n.fill(args.n + def->rvals.n);
  for (int i = 0; i < def->code->names.n; i++)
    n[i] = def->code->names.v[i];
  for (int i = 1; i < names.n; i++)
    n[def->rvals.n + i - 1] = names.v[i];
  assert(!names.n || !names[0]);
  assert(cs->defs.n == 1);
  Vec<CreationSet*> c;
  if (closures) {
    if (closures->set_in(cs)) {
      type_violation(ATypeViolation_CLOSURE_RECURSION, cs->vars[0], 0, result, 0);
      return 0;
    }
  } else
    closures = &c;
  closures->set_add(cs);
  int r = all_applications(p, es, fun, a, n, 1, partial, def, closures);
  if (!r)
    cs->closure_used = 1;
  return r;
}

int
function_dispatch(PNode *p, EntrySet *es, AVar *a0, CreationSet *s, 
                  Vec<AVar *> &args, Vec<cchar *> &names,
                  int is_closure, Partial_kind partial, PNode *visibility_point) 
{
  if (!visibility_point) visibility_point = p;
  Vec<AVar *> a;
  int partial_result = 0;
  a.add(a0);
  for (int j = args.n - 1; j >= 0; j--)
    a.add(args[j]);
  Vec<Match *> matches;
  AVar *send = make_AVar(p->lvals[0], es);
  match_timer.start();
  if (pattern_match(a, names, send, is_closure, partial, visibility_point, matches)) {
    forv_Match(m, matches) {
      if (!m->is_partial && partial != Partial_ALWAYS)
        make_AEdges(m, p, es, a);
      else 
        partial_result = 1;
    }
  }
  match_timer.stop();
  return matches.n ? partial_result : -1;
}

static int
application(PNode *p, EntrySet *es, AVar *a0, CreationSet *cs, 
            Vec<AVar *> &args, Vec<cchar *> &names, 
            int is_closure, Partial_kind partial, PNode *visibility_point,
            Vec<CreationSet *> *closures) 
{
  if (sym_closure->implementors.set_in(cs->sym) && cs->defs.n)
    return partial_application(p, es, cs, args, names, partial, visibility_point, closures);
  return function_dispatch(p, es, a0, cs, args, names, is_closure, partial, visibility_point);
}

void
type_violation(ATypeViolation_kind akind, AVar *av, AType *type, AVar *send, Vec<Fun *> *funs) {
  ATypeViolation *v = new ATypeViolation(akind, av, send);
  v = type_violation_hash.put(v);
  if (!v->type)
    v->type = type;
  else
    v->type = type_union(v->type, type);
  if (funs) {
    if (v->funs)
      v->funs->set_union(*funs);
    else
      v->funs = new Vec<Fun *>(*funs);
  }
  type_violations.set_add(v);
}

static int
make_rest_tuple(EntrySet *es, PNode *p, AVar *to, Vec<AVar *> &v, int vstart, int tvals) {
  int t = tvals;
  int l = v.n - vstart;
  tvals += l + 1;
  AVar *container = make_AVar(p->tvals[t], es);
  fill_tvals(es->fun, p, tvals);
  make_kind(p, es, sym_tuple, container, 0, &v, vstart, t + 1, l);
  flow_vars(container, to);
  return tvals;
}

static Var **
destruct(Var **lvals, int nlvals, AVar *r, Sym *t, AVar *result, int &tvars) {
  Var **lend = lvals + nlvals;
  int nlend = nlvals - t->has.n;
  EntrySet *es = (EntrySet*)result->contour;
  r->arg_of_send.add(result);
  if (t->has.n) {
    forv_CreationSet(cs, r->out->sorted) {
      AVar *violation = 0;
      int r_tuple = sym_tuple->specializers.set_in(cs->sym->type) != 0;
      int t_tuple = sym_tuple->specializers.set_in(t) != 0;
      if (cs->sym->must_specialize->specializers.set_in(t) || (r_tuple && t_tuple)) {
        for (int i = 0; i < t->has.n; i++) {
          assert(t->has.v[i]->var == lvals[i]);
          AVar *l = make_AVar(lvals[i], es);
          AVar *av = NULL;
          if (!t_tuple && t->has_name(i))
            av = cs->var_map.get(t->has_name(i));
          else if (t_tuple && i < cs->vars.n) {
            av = cs->vars[i];
            if (t->has[i]->is_rest) {
              assert(i == t->has.n-1);
              tvars = make_rest_tuple(es, result->var->def, make_AVar(t->has[i]->var, es), cs->vars, i, tvars);
              goto Ldone;
            }
          }
          if (!av) {
            violation = make_AVar(t->has[i]->var, es);
            goto Lviolation;
          }
          flow_vars(av, l);
          lend = destruct(lend, nlend, av, t->has[i], result, tvars);
        }
        if (t->has.n > cs->vars.n) {
          if (t->has.n == cs->vars.n + 1 && t->has[t->has.n-1]->is_rest)
            flow_vars(unique_AVar(sym_empty_tuple->var, GLOBAL_CONTOUR), make_AVar(t->has[t->has.n-1]->var, es));
          else {
            violation = make_AVar(t->has[cs->vars.n]->var, es);
            goto Lviolation;
          }
        }
      Ldone:;
      } else {
      Lviolation:
        AVar *av = violation ? violation : r;
        if (!av->var->sym->name && t->name)
          av = r;
        if (!av->var->sym->name && cs->vars.n < t->has.n && t->has[cs->vars.n]->name)
          av = make_AVar(t->has[cs->vars.n]->var, es);
        if (!av->var->sym->name && cs->vars.n > t->has.n && t->has.n &&
            t->has[t->has.n-1]->name)
          av = make_AVar(t->has[t->has.n-1]->var, es);
        type_violation(ATypeViolation_MATCH, av, make_AType(cs), result);
      }
    }
  }
  return lend;
}

static int
get_obj_index(AVar *index, int *i, int n) {
  if (index->var->sym->type && index->var->sym->imm_int(i) == 0) {
    *i -= fa->tuple_index_base;
    if (*i >= 0 && *i < n)
      return 1;
  }
  if (index->out->n == 1 && index->out->v[0]->sym->is_constant)
    if (index->out->v[0]->sym->imm_int(i) == 0) {
      *i -= fa->tuple_index_base;
      if (*i >= 0 && *i < n)
        return 1;
    }
  return 0;
}

AType *
make_size_constant_type(int n) {
  Sym *t = size_constant(n);
  build_type_hierarchy();
  return make_abstract_type(t);
}

static void
structural_assignment(CreationSet *new_cs, CreationSet *cs, PNode *p, EntrySet *es, bool merge = false, bool mix = false) {
  AVar *result = p->lvals.n ? make_AVar(p->lvals[0], es) : 0;
  AVar *elem = get_element_avar(cs);
  if (elem)
    flow_vars(elem, get_element_avar(new_cs));
  if (mix && new_cs->sym->element) {
    fill_tvals(es->fun, p, new_cs->vars.n);
    for (int i = 0; i < new_cs->vars.n; i++) {
      AVar *tval = make_AVar(p->tvals[i], es);
      flow_vars(new_cs->vars[i], tval);
      set_container(tval, result);
      flow_vars(tval, get_element_avar(new_cs));
    }
    fill_tvals(es->fun, p, cs->vars.n);
    for (int i = 0; i < new_cs->vars.n; i++) {
      AVar *tval = make_AVar(p->tvals[i], es);
      flow_vars(new_cs->vars[i], tval);
      set_container(tval, result);
      flow_vars(tval, get_element_avar(new_cs));
    }
  }
  fill_tvals(es->fun, p, cs->sym->has.n);
  for (int i = 0; i < cs->sym->has.n; i++) {
    Sym *h = cs->sym->has[i];
    AVar *iv = unique_AVar(h->var, cs);
    AVar *tval = make_AVar(p->tvals[i], es);
    flow_vars(iv, tval);
    set_container(tval, result);
    AVar *niv = unique_AVar(h->var, new_cs);
    flow_vars(tval, niv);
    if (mix)
      flow_vars(tval, get_element_avar(new_cs));
  }
  for (int i = cs->sym->has.n; i < cs->vars.n; i++) {
    fill_tvals(es->fun, p, cs->vars.n);
    AVar *tval = make_AVar(p->tvals[i], es);
    flow_vars(cs->vars[i], tval);
    set_container(tval, result);
    if (!merge) {
      new_cs->vars.fill(cs->vars.n);
      if (!new_cs->vars[i])
        new_cs->vars[i] = unique_AVar(cs->vars[i]->var, new_cs);
      flow_vars(tval, new_cs->vars[i]);
    } else
      flow_vars(tval, get_element_avar(new_cs));
  }
}

// for send nodes, add call edges and more complex constraints
// which depend on the computed types (compare to add_send_constraints)
static void
add_send_edges_pnode(PNode *p, EntrySet *es) {
  if (!p->prim) {
    assert(p->lvals.n == 1);
    AVar *result = make_AVar(p->lvals[0], es);
    Vec<AVar *> args;
    for (int i = p->rvals.n - 1; i > 0; i--) {
      AVar *av = make_AVar(p->rvals[i], es);
      av->arg_of_send.add(result);
      args.add(av);
    }
    AVar *a0 = make_AVar(p->rvals[0], es);
    if (all_applications(p, es, a0, args, p->code->names, 0, (Partial_kind)p->code->partial) > 0)
      make_closure(result);
  } else {
    // argument and return constraints
    int n = p->prim->nargs < 0 ? -p->prim->nargs : p->prim->nargs;
    AVar *a = 0, *b = 0;
    int iarg = 0;
    for (int i = 1; i < p->rvals.n; i++) {
      if (i - 1 == p->prim->pos) continue;
      AVar *arg = make_AVar(p->rvals[i], es);
      // record violations
      if (type_diff(arg->out, p->prim->args[iarg]) != bottom_type)
        type_violation(ATypeViolation_PRIMITIVE_ARGUMENT, arg, 
                       type_diff(arg->out, p->prim->args[iarg]), 
                       make_AVar(p->lvals[0], es)); 
      switch (p->prim->arg_types[iarg]) {
        default: break;
        case PRIM_TYPE_ANY_NUM_A: a = arg; break;
        case PRIM_TYPE_ANY_NUM_B: b = arg; break;
        case PRIM_TYPE_ANY_INT_A: a = arg; break;
        case PRIM_TYPE_ANY_INT_B: b = arg; break;
      }
      if (i - 1 < n - 1) iarg++;
    }
    for (int i = 0; i < p->lvals.n; i++) {
      // connect the flows, but prevent values to pass
      // so that splitting can attribute causality
      if (p->prim->ret_types[i] == PRIM_TYPE_ANY_NUM_AB) {
        AVar *res = make_AVar(p->lvals[i], es);
        fill_tvals(es->fun, p, p->lvals.n);
        AVar *t = make_AVar(p->tvals[i], es);
        flow_var_type_permit(t, bottom_type);
        flow_vars(a, t);
        flow_vars(b, t);
        flow_vars(t, res);
        update_in(res, type_num_fold(p->prim, a->out, b->out));
      } else if (p->prim->ret_types[i] == PRIM_TYPE_ANY_NUM_A) {
        AVar *res = make_AVar(p->lvals[i], es);
        fill_tvals(es->fun, p, p->lvals.n);
        AVar *t = make_AVar(p->tvals[i], es);
        flow_var_type_permit(t, bottom_type);
        flow_vars(a, t);
        flow_vars(t, res);
        update_in(res, type_num_fold(p->prim, a->out, a->out));
      }
    }
    AVar *result = p->lvals.n ? make_AVar(p->lvals[0], es) : 0;
    if (result)
      for (int i = 0; i < p->rvals.n; i++)
        make_AVar(p->rvals[i], es)->arg_of_send.add(result);
    int o = p->rvals.v[0]->sym == sym_primitive ? 2 : 1;
    // specifics
    switch (p->prim->index) {
      default: break;
      case P_prim_primitive: {
        cchar *name = p->rvals[1]->sym->name;
        RegisteredPrim *rp = prim_get(name);
        if (!rp)
          fail("undefined primitive transfer function '%s'", name);
        rp->tfn(p, es);
        break;
      }
      case P_prim_meta_apply: {
#if 0
        AVar *a1 = make_AVar(p->rvals[1], es);
        AVar *a2 = make_AVar(p->rvals[2], es);
        Sym *s;
        forv_CreationSet(cs1, a1->out->sorted)
          forv_CreationSet(cs2, a2->out->sorted)
            if (cs1->sym->is_meta_type && cs2->sym->is_meta_type && 
                (s = meta_apply(cs1->sym->meta_type, cs2->sym->meta_type)))
              update_gen(result, make_abstract_type(s));
            else
              type_violation(ATypeViolation_SEND_ARGUMENT, a1, a1->out, result);
#else
        assert(!"implemented");
#endif
        break;
      }
      case P_prim_destruct: {
        assert(p->rvals.n - o == 2);
        int tvars = 0;
        destruct(p->lvals.v, p->lvals.n, make_AVar(p->rvals.v[o], es), p->rvals[o+1]->sym, result, tvars);
        break;
      }
      case P_prim_vector:
        prim_make_vector(p, es);
        break;
      case P_prim_index_object: {
        AVar *vec = make_AVar(p->rvals[o], es);
        AVar *index = make_AVar(p->rvals[o + 1], es);
        set_container(result, vec);
        forv_CreationSet(cs, vec->out->sorted) {
          if (sym_string->specializers.set_in(cs->sym))
            update_gen(result, sym_char->abstract_type);
          else {
            int i;
            bool is_const = get_obj_index(index, &i, cs->vars.n);
            if (cs->sym->element)
              flow_vars(get_element_avar(cs), result);
            if (is_const)
              flow_vars(cs->vars[i], result);
            else
              forv_AVar(av, cs->vars)
                flow_vars(av, result);
          }
        }
        break;
      }
      case P_prim_set_index_object: {
        AVar *vec = make_AVar(p->rvals[o], es);
        AVar *index = make_AVar(p->rvals[o + 1], es);
        AVar *val = make_AVar(p->rvals[o + 2], es);
        fill_tvals(es->fun, p, 1);
        AVar *tval = make_AVar(p->tvals[0], es);
        flow_vars(val, tval);
        set_container(tval, vec);
        forv_CreationSet(cs, vec->out->sorted) {
          if (sym_string->specializers.set_in(cs->sym)) {
            AType *d = type_diff(sym_char->abstract_type, val->out);
            if (d != bottom_type)
              type_violation(ATypeViolation_MATCH, val, d, result);
          } else {
            int i;
            bool is_const = get_obj_index(index, &i, cs->vars.n);
            if (is_const)
              flow_vars(tval, cs->vars[i]);
            else {
              if (cs->sym->element)
                flow_vars(tval, get_element_avar(cs));
              for (int i = 0; i < cs->vars.n; i++) 
                flow_vars(tval, cs->vars[i]);
            }
          }
        }
        flow_vars(val, result);
        break;
      }
      case P_prim_apply: {
        assert(p->lvals.n == 1);
        Vec<AVar *> args;
        Vec<cchar *> names;
        names.add(0);
        names.add(0);
        AVar *fun = make_AVar(p->rvals[1], es);
        AVar *a1 = make_AVar(p->rvals[3], es);
        args.add(a1);
        if (all_applications(p, es, fun, args, names, 0, (Partial_kind)p->code->partial) > 0)
          make_closure(result);
        break;
      }
      case P_prim_period: {
        AVar *obj = make_AVar(p->rvals[1], es);
        AVar *selector = make_AVar(p->rvals[3], es);
        Vec<AVar*> methods;
        set_container(result, obj);
        bool partial = p->code->partial != Partial_NEVER;
        forv_CreationSet(sel, selector->out->sorted) {
          cchar *symbol = sel->sym->name; 
          if (!symbol) symbol = sel->sym->constant;
          if (!symbol) symbol = sel->sym->imm.v_string;
          assert(symbol);
          forv_CreationSet(cs, obj->out->sorted) {
            AVar *iv = cs->var_map.get(symbol);
            if (iv) {
              iv->arg_of_send.add(result);
              if (partial) {
                flow_var_type_permit(result, type_diff(iv->out, function_type));
                flow_vars(iv, result);
                if (type_intersection(iv->out, function_type) != bottom_type)
                  methods.add(iv);
              } else
                flow_vars(iv, result);
            }
          }
        }
        forv_AVar(x, methods) {
          Vec<AVar *> args;
          Vec<cchar *> names;
          names.add(0);
          names.add(0);
          args.add(obj);
          if (all_applications(p, es, x, args, names, 0, (Partial_kind)p->code->partial) > 0)
            make_period_closure(result, x, args);
        }
        {       
          Vec<AVar *> args;
          Vec<cchar *> names;
          names.add(0);
          names.add(0);
          args.add(obj);
          if (all_applications(p, es, selector, args, names, 0, (Partial_kind)p->code->partial) > 0)
            make_period_closure(result, selector, args);
        }
        break;
      }
      case P_prim_setter: {
        AVar *obj = make_AVar(p->rvals[1], es);
        AVar *selector = make_AVar(p->rvals[3], es);
        AVar *val = make_AVar(p->rvals[4], es);
        fill_tvals(es->fun, p, 1);
        AVar *tval = make_AVar(p->tvals[0], es);
        flow_vars(val, tval);
        set_container(tval, obj);
        forv_CreationSet(sel, selector->out->sorted) {
          cchar *symbol = sel->sym->name; 
          if (!symbol) symbol = sel->sym->constant;
          if (!symbol) symbol = sel->sym->imm.v_string;
          assert(symbol);
          forv_CreationSet(cs, obj->out->sorted) {
            AVar *iv = cs->var_map.get(symbol);
            if (iv)
              flow_vars(tval, iv);
          }
        }
        flow_vars(val, result);
        break;
      }
      case P_prim_assign: {
        AVar *lhs = make_AVar(p->rvals[1], es);
        AVar *rhs = make_AVar(p->rvals[3], es);
        forv_CreationSet(cs, lhs->out->sorted) {
          if (cs->sym == sym_ref) {
            assert(cs->vars.n);
            AVar *av = cs->vars[0];
            flow_vars(rhs, av);
            flow_vars(rhs, result);
          } else {
            if (sym_anynum->specializers.set_in(cs->sym->type))
              update_in(result, cs->sym->type->abstract_type);
            else
              type_violation(ATypeViolation_MATCH, lhs, make_AType(cs), result);
          }
        }
        break;
      }
      case P_prim_deref: {
        AVar *ref = make_AVar(p->rvals[2], es);
        set_container(result, ref);
        forv_CreationSet(cs, ref->out->sorted) {
          AVar *av = cs->vars[0];
          flow_vars(av, result);
        }
        break;
      }
      case P_prim_new: {
        AVar *thing = make_AVar(p->rvals[p->rvals.n-1], es);
        forv_CreationSet(cs, thing->out->sorted)
          creation_point(result, cs->sym->meta_type); // recover original type
        break;
      }
      case P_prim_clone: {
        AVar *thing = make_AVar(p->rvals[p->rvals.n-1], es);
        forv_CreationSet(cs, thing->out->sorted) {
          CreationSet *new_cs = creation_point(result, cs->sym);
          structural_assignment(new_cs, cs, p, es);
        }
        break;
      }
      case P_prim_merge: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n-2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n-1], es);
        forv_CreationSet(cs, thing1->out->sorted) {
          CreationSet *new_cs = creation_point(result, cs->sym);
          structural_assignment(new_cs, cs, p, es, true);
          forv_CreationSet(cs2, thing2->out->sorted) {
            if (cs->sym == cs2->sym)
              structural_assignment(new_cs, cs2, p, es, true);
          }
        }
        break;
      }
      case P_prim_merge_in: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n-2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n-1], es);
        forv_CreationSet(cs, thing1->out->sorted) {
          forv_CreationSet(cs2, thing2->out->sorted) {
            if (cs->sym == cs2->sym)
              structural_assignment(cs, cs2, p, es, true, true);
          }
        }
        flow_vars(thing1, result);
        break;
      }
      case P_prim_coerce: {
        assert(p->rvals[1]->sym->abstract_type);
        Sym *s = p->rvals[1]->sym;
        AVar *rhs = make_AVar(p->rvals[2], es);
        Vec<CreationSet *> css;
        forv_CreationSet(cs, rhs->out->sorted)
          if (cs->sym->type == p->rvals[1]->sym)
            css.set_add(cs);
        if (css.n)
          update_gen(result, make_AType(css));
        else if (s->type->num_kind || s->type == sym_string || s->type->is_symbol)
          update_gen(result, s->abstract_type);
        break;
      }
      case P_prim_len: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = bottom_type;
        forv_CreationSet(cs, t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem)
            elem->arg_of_send.add(result);
          if ((elem && elem->out != bottom_type) || sym_string->specializers.set_in(cs->sym))
            rtype = type_union(rtype, size_type);
          else
            rtype = type_union(rtype, make_size_constant_type(cs->vars.n));
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_sizeof: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = bottom_type;
        forv_CreationSet(cs, t->out->sorted) {
          if (cs->sym->size)
            rtype = type_union(rtype, make_size_constant_type(cs->sym->size));
          else
            rtype = type_union(rtype, size_type);
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_sizeof_element: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = bottom_type;
        forv_CreationSet(cs, t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem) {
            forv_CreationSet(cs2, elem->out->sorted) {
              if (cs2->sym->size)
                rtype = type_union(rtype, make_size_constant_type(cs2->sym->size));
              else
                rtype = type_union(rtype, size_type);
            }
          }
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_cast: {
        assert(!"implemented");
        break;
      }
    }
  }
}

static void
add_send_edges(AEdge *e) {
  forv_PNode(p, e->match->fun->fa_send_PNodes)
    add_send_edges_pnode(p, e->to);
}

static inline int
is_fa_Var(Var *v) { 
  return v->sym->type || v->sym->aspect || v->sym->is_constant || v->sym->is_symbol;
}

static void
collect_Vars_PNodes(Fun *f) {
  f->fa_collected = 1;
  if (!f->entry)
    return;
  f->collect_Vars(f->fa_all_Vars, &f->fa_all_PNodes);
  qsort_by_id(f->fa_all_Vars);
  qsort_by_id(f->fa_all_PNodes);
  forv_Var(v, f->fa_all_Vars)
    if (is_fa_Var(v))
      f->fa_Vars.add(v);
  Primitives *prim = if1->primitives;
  forv_PNode(p, f->fa_all_PNodes) {
    if (p->code->kind == Code_MOVE)
      f->fa_move_PNodes.add(p);
    f->fa_phi_PNodes.append(p->phi);
    f->fa_phy_PNodes.append(p->phy);
    if (p->code->kind == Code_SEND) {
      p->prim = prim->find(p);
      f->fa_send_PNodes.add(p);
    }
  }
  forv_Var(v, f->fa_all_Vars)
    if (v->sym->clone_for_constants)
      f->clone_for_constants = 1;
}

static AVar *
get_filtered(AEdge *e, MPosition *p, AVar *av) {
  AVar *filtered = e->filtered_args.get(p);
  if (!filtered) {
    Var *filtered_v = new Var(av->var->sym);
    filtered_v->is_internal = 1;
    filtered_v->is_filtered = 1;
    e->filtered_args.put(p, (filtered = unique_AVar(filtered_v, e->to)));
  }
  return filtered;
}

static void
analyze_edge(AEdge *e_arg) {
  Vec<AEdge *> edges;
  make_entry_set(e_arg, edges);
  qsort_by_id(edges);
  forv_AEdge(ee, edges) {
    int regular_rets = ee->pnode->lvals.n;
    // verify filters
    form_MPositionAVar(x, ee->args) {
      if (!x->key->is_positional())
        continue;
      AType *filter = ee->match->formal_filters.get(x->key);
      AType *es_filter = ee->to->filters.get(x->key);
      if (filter) {
        if (es_filter)
          filter = type_intersection(filter, es_filter);
      } else 
        filter = es_filter;
      if (filter && type_intersection(x->value->out, filter) == bottom_type)
        goto LskipEdge;
    }
    if (ee->from)
      ee->from->out_edges.set_add(ee);
    form_MPositionAVar(x, ee->args) {
      if (!x->key->is_positional())
        continue;
      MPosition *p = x->key;
      AVar *actual = x->value, *formal = make_AVar(ee->to->fun->args.get(p), ee->to),
        *filtered = get_filtered(ee, p, formal);
      AType *edge_filter = ee->match->formal_filters.get(p);
      if (!edge_filter)
        continue;
      AType *es_filter = ee->to->filters.get(p);
      AType *filter = es_filter ? type_intersection(edge_filter, es_filter) : edge_filter;
      flow_var_type_permit(filtered, filter);
      forv_CreationSet(cs, filter->sorted)
        cs->ess.set_add(ee->to);
      flow_vars(actual, filtered);
      flow_vars(filtered, formal);
      if (p->pos.n > 1)
        set_container(filtered, get_filtered(ee, p->up, ee->to->args.get(p->up)));
      else if (!actual->contour_is_entry_set && actual->contour != GLOBAL_CONTOUR) // closure
        set_container(filtered, make_AVar(ee->pnode->rvals[0], ee->from));
    }
    if (ee->match->fun->sym->cont)
      creation_point(make_AVar(ee->match->fun->sym->cont->var, ee->to), sym_continuation);
    for (int i = 0; i < ee->pnode->lvals.n; i++)
      flow_vars(ee->to->rets[i], ee->rets.v[i]);
    fill_rets(ee->to, regular_rets + ee->match->fun->out_positions.n);
    for (int o = 0; o < ee->match->fun->out_positions.n; o++) {
      MPosition *p = ee->match->fun->out_positions[o];
      p = p ? p : ee->match->fun->out_positions[o];
      AVar *actual = ee->args.get(p);
      flow_vars(ee->to->rets[o + regular_rets], actual);
    }
    if (!entry_set_done.set_in(ee->to)) {
      entry_set_done.set_add(ee->to);
      if (!ee->match->fun->fa_collected)
        collect_Vars_PNodes(ee->match->fun);
      add_var_constraints(ee->to);
      add_move_constraints(ee->to);
      add_send_constraints(ee->to);
      add_send_edges(ee);
    }
  LskipEdge:;
  }
}

static void
refresh_top_edge(AEdge *e) {
  MPosition p, *cp;
  p.push(1);
  cp = cannonicalize_mposition(p);
  e->match->formal_filters.put(cp, any_type);
  AVar *av = make_AVar(sym___main__->var, e->to);
  e->args.put(cp, av);
  e->filtered_args.put(cp, av);
  update_gen(av, av->var->sym->abstract_type);
}

static AEdge *
make_top_edge(Fun *top) {
  AEdge *e = new AEdge();
  e->match = new Match(top);
  e->pnode = new PNode();
  Vec<AEdge *> edges;
  make_entry_set(e, edges);
  assert(edges.n == 1);
  sym___main__->var = new Var(sym___main__);
  refresh_top_edge(e);
  return e;
}

static int
is_return_value(AVar *av) {
  EntrySet *es = (EntrySet*)av->contour;
  forv_AVar(v, es->rets)
    if (v == av)
      return 1;
  return 0;
}

static void
show_sym_name(Sym *s, FILE *fp) {
  if (s->name)
    fprintf(fp, "%s", s->name);
  else if (s->constant)
    fprintf(fp, "\"%s\"", s->constant);
  else if (s->is_constant) {
    fputs( "\"", fp);
    fprint_imm(fp, s->imm);
    fputs( "\"", fp);
  } else
    fprintf(fp, "%d", s->id);
}

static void
show_type(Vec<CreationSet *> &t, FILE *fp) {
  Vec<Sym *> type;
  forv_CreationSet(cs, t) if (cs) {
    Sym *s = cs->sym;
    if (!ifa_verbose)
      s = s->type;
    type.set_add(s);
  }
  type.set_to_vec();
  qsort_by_id(type);
  fprintf(fp, "( ");
  forv_Sym(s, type) if (s) {
    show_sym_name(s, fp);
    fprintf(fp, " ");
  }
  fprintf(fp, ") ");
}

static void
show_sym(Sym *s, FILE *fp) {
  if (s->is_pattern) {
    fprintf(fp, "( ");
    forv_Sym(ss, s->has) {
      if (ss != s->has[0])
        fprintf(fp, ", ");
      show_sym(ss, fp);
    }
    fprintf(fp, ")");
  } else if (s->name)
    fprintf(fp, "%s", s->name);
  else if (s->constant)
    fprintf(fp, "\"%s\"", s->constant);
  else
    fprintf(fp, "_");
  if (s->type && s->type->name)
    fprintf(fp, " = %s", s->type->name);
  else if (s->must_implement && 
           s->must_implement == s->must_specialize) {
    fprintf(fp, " : ");
    show_sym_name(s->must_implement, fp);
  } else if (s->must_implement) {
    fprintf(fp, " < ");
    show_sym_name(s->must_implement, fp);
  } else if (s->must_specialize && !s->must_specialize->is_symbol) {
    fprintf(fp, " @ ");
    show_sym_name(s->must_specialize, fp);
  }
}

static void
show_fun(Fun *f, FILE *fp) {
  if (f->line() > 0)
    fprintf(fp, "%s:%d: ", f->filename(), f->source_line());
  forv_Sym(s, f->sym->has) {
    show_sym(s, fp);
    if (s != f->sym->has[f->sym->has.n-1])
      fprintf(fp, ", ");
  }
  if (ifa_verbose)
    fprintf(fp, " id:%d", f->sym->id);
}

static void
show_atype(AType &t, FILE *fp) {
  fprintf(fp, "( ");
  forv_CreationSet(cs, t.sorted) if (cs) {
    show_sym_name(cs->sym, fp);
    fprintf(fp, " id:%d ", cs->id);
  }
  fprintf(fp, ") ");
}

void
fa_print_backward(AVar *v, FILE *fp = 0) {
  if (!fp) fp = stdout;
  Vec<AVar *> done, todo;
  todo.add(v);
  done.set_add(v);
  for (int i = 0; i < todo.n; i++) {
    v = todo[i];
    if (v->var) {
      if (v->var->sym) {
        if (v->var->sym->name)
          fprintf(fp, "%s %d\n", v->var->sym->name, v->var->sym->id);
        else
          fprintf(fp, "%d\n", v->var->sym->id);
      } else
        fprintf(fp, "VAR %p\n", v->var);
    } else
      fprintf(fp, "AVAR %p\n", v);
    show_atype(*v->out, fp); fprintf(fp, "\n");
    forv_AVar(vv, v->backward) if (vv) {
      if (!done.set_in(vv)) {
        todo.add(vv);
        done.set_add(vv);
      }
    }
  }
}

void
fa_dump_var_types(AVar *av, FILE *fp, int verbose = ifa_verbose) {
  Var *v = av->var;
  if (verbose < 2 && (!v->sym->name || v->sym->is_symbol))
    return;
  if (!v->sym->in)
    fprintf(fp, "::");
  else if (v->sym->in->name)
    fprintf(fp, "%s::", v->sym->in->name);
  else
    fprintf(fp, "%d::", v->sym->in->id);
  if (v->sym->name)
    fprintf(fp, "%s(%d) ", v->sym->name, v->sym->id);
  else
    fprintf(fp, "(%d) ", v->sym->id);
  if (v->sym->is_constant) {
    if (v->sym->constant)
      fprintf(fp, "\"%s\" ", v->sym->constant);
    else {
      fprintf(fp, "\"");
      fprint_imm(fp, v->sym->imm);
      fprintf(fp, "\" ");
    }
  }
  show_type(*av->out, fp);
  fprintf(fp, "\n");
}

void
fa_dump_types(FA *fa, FILE *fp) {
  Vec<Var *> gvars;
  forv_EntrySet(es, fa->ess) {
    Fun *f = es->fun;
    if (f->sym->name)
      fprintf(fp, "function %s (%d) ", f->sym->name, f->sym->id);
    else
      fprintf(fp, "function %d ", f->sym->id);
    fprintf(fp, "entry set with %d edges\n", es->edges.count());
    Vec<Var *> vars;
    f->collect_Vars(vars);
    forv_Var(v, vars) {
      if (!v->sym->nesting_depth) {
        gvars.set_add(v);
        continue;
      }
      fa_dump_var_types(make_AVar(v, es), fp);
    }
  }
  gvars.set_to_vec();
  fprintf(fp, "globals\n");
  forv_Var(v, gvars)
    if (!v->sym->is_constant && !v->sym->is_symbol)
      fa_dump_var_types(unique_AVar(v, GLOBAL_CONTOUR), fp);
}

static void
show_name(FILE *fp, AVar *av) {
  if (av->var->sym->name) {
    if (ifa_verbose)
      fprintf(fp, "'%s':%d ", av->var->sym->name, av->var->sym->id);
    else
      fprintf(fp, "'%s' ", av->var->sym->name);
  } else if (ifa_verbose)
    fprintf(fp, "expr:%d ", av->var->sym->id);
  else
    fprintf(fp, "expression ");
}

static void
show_illegal_type(FILE *fp, ATypeViolation *v) {
  AVar *av = v->av;
  show_name(fp, av);
  if (ifa_verbose) {
    fprintf(fp, "id:%d ", av->var->sym->id);
    if (av->out->n) {
      fprintf(fp, ": ");
      show_type(*av->out, fp);
    }
  }
  fprintf(fp, "illegal: ");
  show_type(*v->type->type, fp);
  fprintf(fp, "\n");
}

static int
compar_edge_id(const void *aa, const void *bb) {
  AEdge *a = (*(AEdge**)aa);
  AEdge *b = (*(AEdge**)bb);
  int i = 0, j = 0;
  if (a->pnode && a->pnode->lvals.n)
    i = make_AVar(a->pnode->lvals[0], a->to)->id;
  if (b->pnode && b->pnode->lvals.n)
    j = make_AVar(b->pnode->lvals[0], b->to)->id;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

static void
show_call_tree(FILE *fp, PNode *p, EntrySet *es, int depth = 0) {
  depth++;
  if (depth > fa->print_call_depth || !p->code)
    return;
  if (depth > 1 && p->code->filename() && p->code->line() > 0) {
    for (int x = 0; x < depth; x++)
      fprintf(fp, " ");
    fprintf(fp, "called from %s:%d", p->code->filename(), p->code->line());
    if (ifa_verbose && p->lvals.n)
      fprintf(fp, " send:%d", p->lvals[0]->sym->id);
    fprintf(fp, "\n");

  }
  if (es != GLOBAL_CONTOUR) {
    Vec<AEdge*> edges;
    forv_AEdge(e, es->edges) if (e)
      edges.add(e);
    qsort(edges.v, edges.n, sizeof(edges[0]), compar_edge_id);
    forv_AEdge(e, edges)
      show_call_tree(fp, e->pnode, e->from, depth);
  }
}

void
show_avar_call_tree(FILE *fp, AVar *av) {
  EntrySet *es = (EntrySet*)av->contour;
  Vec<AEdge*> edges;
  forv_AEdge(e, es->edges) if (e)
    edges.add(e);
  qsort(edges.v, edges.n, sizeof(edges[0]), compar_edge_id);
  forv_AEdge(e, edges)
    show_call_tree(fp, e->pnode, e->from, 1);
}

static void
show_candidates(FILE *fp, PNode *pn, Sym *arg0) {
  Vec<Fun *> *pfuns = pn->code->ast->visible_functions(arg0);
  if (!pfuns)
    return;
  Vec<Fun *> funs(*pfuns);
  funs.set_to_vec();
  qsort_by_id(funs);
  fprintf(fp, "note: candidates are:\n");
  forv_Fun(f, funs) {
    show_fun(f, fp);
    fprintf(fp, "\n");
  }
}

static int
compar_tv(const void *aa, const void *bb) {
  int i, j, x;
  ATypeViolation *a = (*(ATypeViolation**)aa);
  ATypeViolation *b = (*(ATypeViolation**)bb);
  IFAAST *aast = a->send ? a->send->var->def->code->ast : 0;
  if (!aast) aast = a->av->var->sym->ast;
  IFAAST *bast = b->send ? b->send->var->def->code->ast : 0;
  if (!bast) bast = b->av->var->sym->ast;
  if (!aast || !bast) {
    if (bast) return -1;
    if (aast) return 1;
    goto Lskip;
  }
  if (!aast->pathname() || !bast->pathname()) {
    if (bast->pathname()) return -1;
    if (aast->pathname()) return 1;
  } else {
    int x = strcmp(aast->pathname(), bast->pathname());
    if (x) return x;
  }
  i = aast->line();
  j = bast->line();
  x = (i > j) ? 1 : ((i < j) ? -1 : 0);
  if (x)
    return x;
 Lskip:
  if (a->kind < b->kind)
    return -1;
  if (b->kind < a->kind)
    return 1;
  if (a->av && b->av) {
    if (a->av->var && b->av->var) {
      if (a->av->var->sym && b->av->var->sym) {
        i = a->av->var->sym->id;
        j = b->av->var->sym->id;
        x = (i > j) ? 1 : ((i < j) ? -1 : 0);
        if (x)
          return x;
      }
      i = a->av->var->id;
      j = b->av->var->id;
      x = (i > j) ? 1 : ((i < j) ? -1 : 0);
      if (x)
        return x;
    }
    i = a->av->id;
    j = b->av->id;
    x = (i > j) ? 1 : ((i < j) ? -1 : 0);
    if (x)
      return x;
  }
  if (a->send && b->send) {
    i = a->send->id;
    j = b->send->id;
    x = (i > j) ? 1 : ((i < j) ? -1 : 0);
    if (x)
      return x;
  }
  return 0;
}

static void
show_violations(FA *fa, FILE *fp) {
  Vec<ATypeViolation *> vv;
  forv_ATypeViolation(v, type_violations) if (v)
    vv.add(v);
  qsort(vv.v, vv.n, sizeof(vv[0]), compar_tv);
  forv_ATypeViolation(v, vv) if (v) {
    if (v->send && v->send->var->def->code->source_line() > 0)
      fprintf(fp, "%s:%d: ", v->send->var->def->code->filename(), 
              v->send->var->def->code->source_line());
    else if (v->av->var->sym->ast && v->av->var->sym->source_line() > 0)
      fprintf(fp, "%s:%d: ", v->av->var->sym->filename(), 
              v->av->var->sym->source_line());
    else if (!v->av->contour_is_entry_set && v->av->contour != GLOBAL_CONTOUR) {
      CreationSet *cs = (CreationSet*)v->av->contour;
      fprintf(fp, "%s:%d: class %s:: ", 
              cs->sym->filename(), cs->sym->source_line(), cs->sym->name);     
    } else {
      if (fruntime_errors)
        fprintf(fp, "warning: ");
      else
        fprintf(fp, "error: ");
    }
    switch (v->kind) {
      default: assert(0);
      case ATypeViolation_PRIMITIVE_ARGUMENT:
        fprintf(fp, "illegal primitive argument type ");
        show_illegal_type(fp, v);
        break;
      case ATypeViolation_SEND_ARGUMENT:
        if (v->av->var->sym->is_symbol &&
            v->send->var->def->rvals[0] == v->av->var) {
          fprintf(fp, "unresolved call '%s'", v->av->var->sym->name);
          if (ifa_verbose)
            fprintf(fp, " send:%d", v->send->var->sym->id);
          fprintf(fp, "\n");
          show_candidates(fp, v->send->var->def, v->av->var->sym);
        } else {
          fprintf(fp, "illegal call argument type ");
          show_illegal_type(fp, v);
        }
        break;
      case ATypeViolation_DISPATCH_AMBIGUITY:
        fprintf(fp, "%s: ambiguous call '%s'", fruntime_errors ? "warning" : "error", v->av->var->sym->name);
        if (ifa_verbose)
          fprintf(fp, " send:%d", v->send->var->sym->id);
        fprintf(fp, "\n");
        fprintf(fp, "note: candidates are:\n");
        forv_Fun(f, *v->funs) if (f) {
          show_fun(f, fp);
          fprintf(fp, "\n");
        }
        break;
      case ATypeViolation_MEMBER:
        if (v->av->out->n == 1)
          fprintf(fp, "unresolved member '%s'", v->av->out->v[0]->sym->name);
        else {
          fprintf(fp, "unresolved member\n");
          forv_CreationSet(selector, v->av->out->sorted)
            fprintf(fp, "  selector '%s'\n", selector->sym->name);
        }
        if (v->type->n == 1)
          fprintf(fp, "  class '%s'\n", v->type->v[0]->sym->name ? v->type->v[0]->sym->name : 
                  "<anonymous>");
        else {
          fprintf(fp, "  classes\n");
          forv_CreationSet(cs, v->type->sorted)
            fprintf(fp, "  class '%s'\n", cs->sym->name);
        }
        break;
      case ATypeViolation_MATCH:
        if (v->av->var->sym->name)
          fprintf(fp, "near '%s' unmatched type: ", v->av->var->sym->name);
        else
          fprintf(fp, "unmatched type: ");
        show_type(*v->type, fp);
        fprintf(fp, "\n");
        break;
      case ATypeViolation_NOTYPE:
        show_name(fp, v->av);
        fprintf(fp, "has no type\n");
        break;
      case ATypeViolation_BOXING:
        show_name(fp, v->av);
        fprintf(fp, "has mixed basic types:");
        show_type(*v->type, fp);
        fprintf(fp, "\n");
        break;
      case ATypeViolation_CLOSURE_RECURSION:
        show_name(fp, v->av);
        fprintf(fp, "is recursive closure\n");
        break;
    }
    if (v->send)
      show_call_tree(fp, v->send->var->def, (EntrySet*)v->send->contour);
    else if (v->av->contour_is_entry_set)
      show_avar_call_tree(fp, v->av);
    else if (v->av->contour != GLOBAL_CONTOUR)
      show_call_tree(fp, ((CreationSet*)v->av->contour)->defs.first()->var->def,
                     (EntrySet*)((CreationSet*)v->av->contour)->defs.first()->contour, 1);
  }
}

static cchar *fn(cchar *s) {
  if (!s)
    return "<none>";
  cchar *filename = strrchr(s, '/');
  if (filename)
    return filename + 1;        
  return s;
}

void
log_var_types(Var *v, Fun *f) {
  if (!v->sym->name || v->sym->is_symbol || v->is_internal)
    return;
  if (!v->sym->in)
    log(LOG_TEST_FA, "::");
  else if (v->sym->in->name)
    log(LOG_TEST_FA, "%s::", v->sym->in->name);
  else
    log(LOG_TEST_FA, "%d::", v->sym->in->id);
  if (v->sym->name) {
    if (v->sym->line() > 0)
      log(LOG_TEST_FA, "%s(%s:%d) ", v->sym->name, fn(v->sym->filename()), v->sym->source_line());
    else
      log(LOG_TEST_FA, "%s ", v->sym->name);
  } else
    log(LOG_TEST_FA, "(%s:%d) ", fn(v->sym->filename()), v->sym->source_line());
  Vec<CreationSet *> css;
  for (int i = 0; i < v->avars.n; i++) if (v->avars[i].key) {
    AVar *av = v->avars[i].value;
    // this test doesn't take into account nested variables
    if (!f || f->ess.set_in(((EntrySet*)av->contour)))
      css.set_union(*av->out);
  }
  log(LOG_TEST_FA, "( ");
  Vec<Sym *> syms;
  forv_CreationSet(cs, css) if (cs)
    syms.set_add(cs->sym->type);
  syms.set_to_vec();
  qsort_by_id(syms);
  forv_Sym(s, syms) {
    if (s->name)
      log(LOG_TEST_FA, "%s ", s->name);
    else if (s->constant)
      log(LOG_TEST_FA, "\"%s\" ", s->constant);
    else if (s->is_constant) {
      char c[128];
      sprint_imm(c, s->imm);
      log(LOG_TEST_FA, "\"%s\" ", c);
    }
    if (s->source_line())
      log(LOG_TEST_FA, "(%s:%d) ", fn(s->filename()), s->source_line());
  }
  log(LOG_TEST_FA, ")\n");
}

static void
collect_results() {
  // collect funs, ess and ess_set
  fa->funs.clear();
  fa->ess.clear();
  forv_EntrySet(es, entry_set_done) if (es) {
    fa->funs.set_add(es->fun);
    fa->ess.add(es);
  }
  fa->funs.set_to_vec();
  qsort_by_id(fa->funs);
  fa->ess_set.move(entry_set_done);
  // collect css and css_set
  fa->css.clear();
  fa->css_set.clear();
  forv_EntrySet(es, fa->ess) {
    forv_Var(v, es->fun->fa_all_Vars) {
      AVar *xav = make_AVar(v, es);
      for (AVar *av = xav; av; av = av->lvalue)
        fa->css_set.set_union(*av->out);
    }
  }
  forv_CreationSet(cs, fa->css_set) if (cs) 
    fa->css.add(cs);
  qsort_by_id(fa->css);
  // print results
  if (ifa_verbose)
    fa_dump_types(fa, stdout);
  if (fgraph_pass_contours) {
    char fn[2048];
    strcpy(fn, fa->fn);
    sprintf(fn + strlen(fn), ".%d", analysis_pass);
    graph_contours(fa, fn);
  }
}

static int
empty_type_minus_partial_applications(AType *a) {
  forv_CreationSet(aa, *a) if (aa) {
    if (aa->sym == sym_closure && aa->defs.n)
      continue;
    if (aa->sym->is_unique_type)
      continue;
    return 0;
  }
  return 1;
}

static AType *
type_minus_partial_applications(AType *a) {
  AType *r = new AType();
  forv_CreationSet(aa, *a) if (aa) {
    if (aa->sym == sym_closure && aa->defs.n)
      continue;
    r->set_add(aa);
  }
  r = type_cannonicalize(r);
  return r;
}

// for each call site, check that all args are covered
static void
collect_argument_type_violations() {
  forv_Fun(f, fa->funs) {
    forv_PNode(p, f->fa_send_PNodes) {
      if (p->prim) continue; // primitives handled elsewhere
      Vec<EntrySet *> ess;
      f->ess.set_intersection(fa->ess_set, ess);
      forv_EntrySet(from, ess) if (from) {
        Vec<AEdge *> *m = from->out_edge_map.get(p);
        if (!m) {
          if (p->code->partial == Partial_NEVER) {
            forv_Var(v, p->rvals) {
              AVar *av = make_AVar(v, from);
              type_violation(ATypeViolation_SEND_ARGUMENT, av, av->out, 
                             make_AVar(p->lvals[0], from));
            }
          }
        } else {
          Vec<AVar *> actuals;
          forv_AEdge(me, *m) {
            if (!from->out_edges.set_in(me))
              continue;
            form_MPositionAVar(x, me->args)
              if (x->key->is_positional())
                actuals.set_add(x->value);
          }
          forv_AVar(av, actuals) if (av) {
            AType *t = av->out;
            forv_AEdge(e, *m) {
              if (!from->out_edges.set_in(e))
                continue;
              form_MPositionAVar(x, e->args) {
                if (x->value != av)
                  continue;
                if (!x->key->is_positional())
                  continue;
                MPosition *p = x->key;
                AVar *filtered = e->filtered_args.get(p);
                if (filtered)
                  t = type_diff(t, filtered->out);
              }
            }
            if (!empty_type_minus_partial_applications(t)) {
              t = type_minus_partial_applications(t);
              type_violation(ATypeViolation_SEND_ARGUMENT, av, t, make_AVar(p->lvals[0], from));
            }
          }
        }
      }
    }
  }
}

static int
mixed_basics(AVar *av) {
  Vec<Sym *> basics;
  forv_CreationSet(cs, *av->out) if (cs) {
    Sym *b = to_basic_type(cs->sym->type);
    if (b)
      basics.set_add(b);
  }
  return basics.n > 1;
}

static void
collect_var_type_violations() {
  // collect NOTYPE violations
  forv_EntrySet(es, fa->ess) {
    forv_Var(v, es->fun->fa_all_Vars) {
      AVar *av = make_AVar(v, es);
      if (!av->var->is_internal && av->out == bottom_type && !is_Sym_OUT(av->var->sym))
        type_violation(ATypeViolation_NOTYPE, av, av->out, 0, 0);
    }
  }
  if (!fa->permit_boxing) {
    // collect BOXING violations
    forv_EntrySet(es, fa->ess) {
      forv_Var(v, es->fun->fa_all_Vars) {
        AVar *av = make_AVar(v, es);
        if (mixed_basics(av)) 
          type_violation(ATypeViolation_BOXING, av, av->out, 0, 0);
      }
    }
    forv_CreationSet(cs, fa->css) {
      forv_AVar(av, cs->vars) {
        if (mixed_basics(av)) 
          type_violation(ATypeViolation_BOXING, av, av->out, 0, 0);
      }
    }
  }
  if (fa->no_unused_instance_variables) {
    forv_CreationSet(cs, fa->css) {
      forv_AVar(av, cs->vars) {
        if (av->out == bottom_type)
          type_violation(ATypeViolation_NOTYPE, av, av->out, 0, 0);
      }
    }
  }
}

static void
convert_NOTYPE_to_void() {
  if (!fa->css_set.set_in(void_type->v[0])) {
    fa->css_set.set_add(void_type->v[0]);       
    fa->css.add(void_type->v[0]);       
  }
  forv_EntrySet(es, fa->ess) {
    forv_Var(v, es->fun->fa_all_Vars) {
      AVar *av = make_AVar(v, es);
      if (!av->var->is_internal && av->out == bottom_type && !is_Sym_OUT(av->var->sym))
        av->out = void_type;
    }
  }
  if (fa->no_unused_instance_variables) {
    forv_CreationSet(cs, fa->css) {
      forv_AVar(av, cs->vars) {
        if (av->out == bottom_type)
          av->out = void_type;
      }
    }
  }
}

void 
initialize_Sym_for_fa(Sym *s) {
  if (s->is_symbol || s->is_fun || s->type_kind)
    s->abstract_type = make_abstract_type(s);
  if (s->is_fun || s->is_pattern || s->type_kind)
    forv_Sym(ss, s->has)
      if (!ss->var)
        ss->var = new Var(ss);
  if (s->type_kind && s->element)
    s->element->var = new Var(s->element);
}

static void
initialize_symbols() {
  forv_Sym(s, fa->pdb->if1->allsyms)
    initialize_Sym_for_fa(s);
}

static void
initialize_primitives() {
  forv_Prim(p, fa->pdb->if1->primitives->prims) {
    p->args.clear();
    int n = p->nargs < 0 ? -p->nargs : p->nargs;
    for (int i = 0; i < n - 1; i++) {
      switch (p->arg_types[i]) {
        case PRIM_TYPE_ALL:             p->args.add(top_type); break;
        case PRIM_TYPE_ANY:             p->args.add(any_type); break;
        case PRIM_TYPE_SYMBOL:          p->args.add(symbol_type); break;
        case PRIM_TYPE_STRING:          p->args.add(string_type); break;
        case PRIM_TYPE_SIZE:            p->args.add(size_type); break;
        case PRIM_TYPE_TUPLE:           p->args.add(tuple_type); break;
        case PRIM_TYPE_CONT:            p->args.add(make_abstract_type(sym_continuation)); break;
        case PRIM_TYPE_REF:             p->args.add(make_abstract_type(sym_ref)); break;
        case PRIM_TYPE_ANY_NUM_A:       p->args.add(anynum_kind); break;
        case PRIM_TYPE_ANY_NUM_B:       p->args.add(anynum_kind); break;
        case PRIM_TYPE_ANY_INT_A:       p->args.add(anyint_type); break;
        case PRIM_TYPE_ANY_INT_B:       p->args.add(anyint_type); break;
        default: assert(!"case");       break;
      }
    }
  }
}

static void
initialize_global(Sym *s) {
  if (!s->var)
    s->var = new Var(s);
  add_var_constraint(make_AVar(s->var, (EntrySet*)GLOBAL_CONTOUR));
}

static void
initialize() {
  if1->callback->finalize_functions();
  bottom_type = type_cannonicalize(new AType());
  bottom_type->type = bottom_type;
  void_type = make_abstract_type(sym_void_type);
  any_type = make_abstract_type(sym_any);
  top_type = type_union(any_type, void_type);
  bool_type = make_abstract_type(sym_bool);
  size_type = make_abstract_type(sym_size);
  symbol_type = make_abstract_type(sym_symbol);
  string_type = make_abstract_type(sym_string);
  anyint_type = make_abstract_type(sym_anyint);
  function_type = make_abstract_type(sym_function);
  anynum_kind = make_abstract_type(sym_anynum);
  anytype_type = make_abstract_type(sym_anytype);
  nil_type = make_abstract_type(sym_nil_type);
  unknown_type = make_abstract_type(sym_unknown_type);
  tuple_type = make_abstract_type(sym_tuple);
  initialize_global(sym_nil);
  initialize_global(sym_empty_list);
  initialize_global(sym_empty_tuple);
  initialize_global(sym_unknown);
  initialize_global(sym_void);
  edge_worklist.clear();
  send_worklist.clear();
  initialize_symbols();
  initialize_primitives();
  build_arg_positions(fa);
  build_patterns(fa);
}

static void
initialize_pass() {
  pass_timer.restart();
  type_violations.clear();
  type_violation_hash.clear();
  entry_set_done.clear();
  refresh_top_edge(fa->top_edge);
}

static void
mark_es_backedges(EntrySet *es, Accum<EntrySet *> &ess) {
  ess.add(es);
  es->dfs_color = DFS_grey;
  forv_AEdge(e, es->out_edges) if (e) {
    if (e->to->dfs_color == DFS_white)
      mark_es_backedges(e->to, ess);
    else {
      if (e->to->dfs_color == DFS_grey) {
        e->es_backedge = 1;
        e->to->backedges.add(e);
      }
    }
  }
  es->dfs_color = DFS_black;
}

static void
compute_recursive_entry_sets() {
  Accum<EntrySet *> ess;
  mark_es_backedges(fa->top_edge->to, ess);
  forv_EntrySet(es, ess.asvec) es->dfs_color = DFS_white;
}

static void mark_es_cs_backedges(CreationSet *cs, Accum<EntrySet *> &ess, Accum<CreationSet *> &css);
static void mark_es_cs_backedges(EntrySet *es, Accum<EntrySet *> &ess, Accum<CreationSet *> &css);

static void
mark_es_cs_backedges(CreationSet *cs, Accum<EntrySet *> &ess, Accum<CreationSet *> &css) {
  css.add(cs);
  cs->dfs_color = DFS_grey;
  forv_EntrySet(es, cs->ess) if (es) {
    if (es->dfs_color == DFS_white)
      mark_es_cs_backedges(es, ess, css);
    else if (es->dfs_color == DFS_grey)
      es->cs_backedges.add(cs);
  }
  cs->dfs_color = DFS_black;
}

static void
mark_es_cs_backedges(EntrySet *es, Accum<EntrySet *> &ess, Accum<CreationSet *> &css) {
  ess.add(es);
  es->dfs_color = DFS_grey;
  forv_AEdge(e, es->out_edges) if (e) {
    EntrySet *es_succ = e->to;
    if (es_succ->dfs_color == DFS_white)
      mark_es_cs_backedges(es_succ, ess, css);
    else if (es_succ->dfs_color == DFS_grey) {
      e->es_cs_backedge = 1;
      es_succ->es_cs_backedges.add(e);
    }
  }
  forv_CreationSet(cs, es->creates) if (cs) {
    if (cs->dfs_color == DFS_white)
      mark_es_cs_backedges(cs, ess, css);
    else if (cs->dfs_color == DFS_grey)
      cs->es_backedges.add(es);
  }
  es->dfs_color = DFS_black;
}

// recursion amongst EntrySets and the CreationSets 
// created within them, and the EntrySets "created"
// (as in restricted) by those CreationSets
static void
compute_recursive_entry_creation_sets() {
  Accum<EntrySet *> ess;
  Accum<CreationSet *> css;
  mark_es_cs_backedges(fa->top_edge->to, ess, css);
  forv_EntrySet(es, ess.asvec) es->dfs_color = DFS_white;
  forv_CreationSet(cs, css.asvec) cs->dfs_color = DFS_white;
}

int
is_es_recursive(EntrySet *es) {
  if (es->split)
    return es->split->backedges.n;
  return es->backedges.n;
}

static int
is_es_recursive(AEdge *e) {
  EntrySet *es = e->from->split ? e->from->split : e->from;
  forv_AEdge(ee, es->backedges)
    if (ee->pnode == e->pnode && ee->fun == e->fun)
      return 1;
  return 0;
}

int
is_es_cs_recursive(EntrySet *es) {
  if (es->split)
    return es->split->es_cs_backedges.n;
  return es->es_cs_backedges.n;
}

static int
is_es_cs_recursive(AEdge *e) {
  EntrySet *es = e->from->split ? e->from->split : e->from;
  forv_AEdge(ee, es->es_cs_backedges)
    if (ee->pnode == e->pnode && ee->fun == e->fun)
      return 1;
  return 0;
}

int
is_es_cs_recursive(CreationSet *cs) {
  if (cs->split)
    return cs->split->es_backedges.n;
  return cs->es_backedges.n;
}

#define SPLIT_TYPE              0
#define SPLIT_SETTER            1

#define SPLIT_VALUE             0
#define SPLIT_MARK              1

#define SPLIT_EDGES             0
#define SPLIT_DYNAMIC           1

static void
collect_type_confluence(AVar *av, Vec<AVar *> &confluences) {
  forv_AVar(x, av->backward) if (x) {
    if (!x->out->type->n)
      continue;
    if (av->var->sym->clone_for_constants) {
      if (type_diff(av->in, x->out) != bottom_type) {
        confluences.set_add(av);
        break;
      }
    } else {
      if (x->out->type->n && type_diff(av->in->type, x->out->type) != bottom_type) {
        confluences.set_add(av);
        break;
      }
    }
  }
}

static void
collect_type_confluences(Vec<AVar *> &confluences) {
  confluences.clear();
  forv_EntrySet(es, fa->ess) {
    forv_Var(v, es->fun->fa_all_Vars) {
      AVar *xav = make_AVar(v, es);
      for (AVar *av = xav; av; av = av->lvalue)
        collect_type_confluence(av, confluences);
    }
  }
  forv_CreationSet(cs, fa->css) {
    forv_AVar(av, cs->vars) {
      if (!av->contour_is_entry_set && av->contour != GLOBAL_CONTOUR)
        collect_type_confluence(av, confluences);
    }
  }
  confluences.set_to_vec();
  qsort_by_id(confluences);
  forv_AVar(x, confluences)
    log(LOG_SPLITTING, "type confluence %s %d\n", 
        x->var->sym->name ? x->var->sym->name : "", x->var->sym->id);
}


static void
collect_es_marked_confluences(Vec<AVar *> &confluences, Accum<AVar *> &acc, int fsetters) {
  confluences.clear();
  forv_AVar(xav, acc.asvec) {
    for (AVar *av = xav; av; av = av->lvalue) {
      Vec<AVar *> &dir = fsetters ? av->forward : av->backward;
      forv_AVar(x, dir) if (x && x->mark_map) {
        if (different_marked_args(x, av, 1)) {
          confluences.set_add(av);
          break;
        }
      }
    }
  }
  confluences.set_to_vec();
  qsort_by_id(confluences);
}

static void
record_backedges(AEdge *e, EntrySet *es, PendingAEdgeEntrySetsMap &up_map) {
  form_Map(MapElemAEdgeEntrySets, m, up_map) {
    if (m->key->from == es)
      map_set_add(e->to->pending_es_backedge_map, 
                  new_AEdge(m->key->fun, m->key->pnode, e->to), 
                  m->value);
    else
      map_set_add(e->to->pending_es_backedge_map, m->key, m->value);
  }
  Vec<AEdge *> *backedges = &es->backedges;
  if (es->split)
    backedges = &es->split->backedges;
  forv_AEdge(ee, *backedges) {
    if (ee->from == es)
      map_set_add(e->to->pending_es_backedge_map, new_AEdge(ee->fun, ee->pnode, e->to), e->to);
    else
      map_set_add(e->to->pending_es_backedge_map, e, e->to);
  }
}

static EntrySet *
find_or_make_filtered_entry_set(EntrySet *orig_es, Map<MPosition *, AType *> &filters) {
  Fun *f = orig_es->fun;
  forv_EntrySet(es, f->ess)
    if (!es->filters.some_disjunction(filters))
      return es;
  EntrySet *new_es = new EntrySet(f);
  f->ess.add(new_es);
  new_es->filters.copy(filters);
  new_es->split = orig_es;
  return new_es;
}

static int
split_edges(AVar *av, int fsetters, int fmark) {
  int again = 0;
  EntrySet *es = (EntrySet*)av->contour;
  Vec<AEdge *> all_edges;
  forv_AEdge(ee, es->edges) if (ee)
    all_edges.add(ee);
  qsort_by_id(all_edges);
  MPosition *p = 0;
  form_MPositionAVar(x, es->args) {
    if (x->value == av) {
      p = x->key;
      break;
    }
  }
  assert(p);
  Map<CreationSet *, EntrySet *> cs_es_map;
  forv_CreationSet(cs, av->out->type->sorted) {
    Map<MPosition *, AType *> filters;
    filters.copy(es->filters);
    filters.put(p, make_AType(cs));
    EntrySet *tes = find_or_make_filtered_entry_set(es, filters);
    cs_es_map.put(cs, tes);
  }
  forv_AEdge(ee, all_edges) if (ee) {
    AVar *earg = es->args.get(p);
    EntrySet *old = ee->to;
    if (earg->out->n == 1)
      ee->to = cs_es_map.get(earg->out->v[0]);
    else {
      for (int i = 0; i < earg->out->type->sorted.n; i++) {
        CreationSet *cs = earg->out->type->sorted[i];
        if (!i)
          set_entry_set(ee, cs_es_map.get(cs));
        else
          ee = copy_AEdge(ee, cs_es_map.get(cs));
      }
    }
    if (ee->to != old) {
      again = 1;
      log(LOG_SPLITTING, "DISPATCH ES %d:%d, %s %d -> %d\n", 
          ee->from->id, ee->pnode->lvals[0]->sym->id, 
          es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id,
          old->id, ee->to->id);
    }
  }
  return again;
}

static int
split_entry_set(AVar *av, int fsetters, int fmark, int fdynamic) {
  EntrySet *es = (EntrySet*)av->contour;
  if (es->split)
    return 0;
  if (fdynamic)
    if (split_edges(av, fsetters, fmark))
      return 1;
  Vec<AEdge *> all_edges, do_edges, stay_edges;
  PendingAEdgeEntrySetsMap pending_es_backedge_map;
  forv_AEdge(ee, es->edges) if (ee)
    all_edges.add(ee);
  qsort_by_id(all_edges);
  int nedges = 0, non_rec_edges = 0;
  forv_AEdge(ee, all_edges) if (ee) {
    if (!ee->from) 
      continue;
    nedges++;
    pending_es_backedge_map.map_union(ee->from->pending_es_backedge_map);
    if (!fsetters ? is_es_recursive(ee) : is_es_cs_recursive(ee))
      continue;
    non_rec_edges++;
    if (!fsetters) {
      if (!edge_type_compatible_with_entry_set(ee, es, fmark))
        do_edges.add(ee);
      else
        stay_edges.add(ee);
    } else {
      if (!edge_sset_compatible_with_entry_set(ee, es))
        do_edges.add(ee);
      else
        stay_edges.add(ee);
    }
  }
  Vec<AEdge *> tedges;
  tedges.move(do_edges);
  forv_AEdge(e, tedges) {
    int compat = 1;
    forv_AEdge(ee, stay_edges) {
      if (!fsetters)
        compat = edge_type_compatible_with_edge(e, ee, es, fmark) && compat;
      else
        compat = edge_sset_compatible_with_edge(e, ee) && compat;
    }
    if (compat)
      stay_edges.add(e);
    else
      do_edges.add(e);
  }
  if (non_rec_edges == 1 && nedges != do_edges.n) 
    return 0;
  int split = 0;
  while (do_edges.n) {
    Vec<AEdge *> these_edges, next_edges;
    AEdge *e = do_edges[0];
    these_edges.add(e);
    for (int i = 1; i < do_edges.n; i++) {
      int compat = 0;
      AEdge *ee = do_edges[i];
      if (!fsetters)
        compat = edge_type_compatible_with_edge(e, ee, es, fmark);
      else
        compat = edge_sset_compatible_with_edge(e, ee);
      if (compat)
        these_edges.add(ee);
      else
        next_edges.add(ee);
    }
    if (!next_edges.n && !stay_edges.n)
      return split;
    forv_AEdge(x, these_edges) {
      x->to = 0;
      x->filtered_args.clear();
      es->edges.del(x);
    }
    forv_AEdge(x, these_edges) {
      Vec<AEdge *> new_edges;
      make_entry_set(x, new_edges, es, e->to);
      if (x->to != es) {
        record_backedges(x, es, pending_es_backedge_map);
        split = 1;
        log(LOG_SPLITTING, "SPLIT ES %d %s%s%s %d from %d -> %d\n", 
            es->id,
            fsetters ? "setters " : "",
            fmark ? "marks " : "",
            es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id,
            x->pnode->lvals[0]->sym->id, x->to->id);
      }
    }
    do_edges.move(next_edges);
  }
  return split;
}

static void
build_type_mark(AVar *av, CreationSet *cs, int mark = 1) {
  int m = av->mark_map ? av->mark_map->get(cs) : 0;
  if (!m) {
    if (!av->out->type->set_in(cs))
      return;
    if (!av->mark_map)
      av->mark_map = new MarkMap;
    av->mark_map->put(cs, mark);
  } else if (m > mark)
    av->mark_map->put(cs, mark);
  else if (m <= mark)
    return;
  forv_AVar(y, av->forward) if (y)
    build_type_mark(y, cs, mark + 1);
}

// To handle recursion, mark value*AVar distances from the nearest 
// AVar generating the value.  Dataflow is considered to be only
// from lower to higher distances for the purpose of splitting.
static void
build_type_marks(AVar *av, Accum<AVar *> &acc) {
  // collect all contributing nodes
  acc.add(av);
  forv_AVar(x, acc.asvec)
    forv_AVar(y, x->backward) if (y)
      acc.add(y);
  forv_AVar(x, acc.asvec)
    forv_AVar(y, x->forward) if (y)
      acc.add(y);
  // mark them
  forv_AVar(x, acc.asvec) {
    if (x->gen)
      forv_CreationSet(s, *x->gen) if (s && s->sym != sym_nil_type) {
        if (s->sym != s->sym->type)
          s = s->sym->type->abstract_type->v[0];
        build_type_mark(x, s);
      }
  }
}

static void
build_setter_mark(AVar *av, AVar *x, int mark = 1) {
  int m = av->mark_map ? av->mark_map->get(x) : 0;
  if (!m) {
    if (!av->setters->set_in(x))
      return;
    if (!av->mark_map)
      av->mark_map = new MarkMap;
    av->mark_map->put(x, mark);
  } else if (m > mark)
    av->mark_map->put(x, mark);
  else if (m <= mark)
    return;
  forv_AVar(y, av->backward) if (y)
    build_setter_mark(y, x, mark + 1);
}

// this is a backward problem, so search forward then back
// to find all the contributors and what they effect
static void
build_setter_marks(AVar *av, Accum<AVar *> &acc) {
  // collect all contributing nodes
  acc.add(av);
  forv_AVar(x, acc.asvec)
    forv_AVar(y, x->forward) 
      if (y && y->setters && y->setters->some_intersection(*av->setters))
        acc.add(y);
  forv_AVar(x, acc.asvec)
    forv_AVar(y, x->backward) 
      if (y && y->setters && y->setters->some_intersection(*av->setters))
        acc.add(y);
  // mark them
  forv_AVar(x, acc.asvec) if (x->setters)
    forv_AVar(y, *x->setters)
      if (x == y->container)
        build_setter_mark(x, y);
}

static void
clear_marks(Accum <AVar *> &acc) {
  forv_AVar(x, acc.asvec)
    x->mark_map = 0;
}

static void
clear_avar(AVar *av) {
  av->gen = 0;
  av->in = bottom_type;
  av->out = bottom_type;
  av->setters = 0;
  av->setter_class = 0;
  av->restrict = 0;
  av->backward.clear();
  av->forward.clear();
  av->arg_of_send.clear();
  av->mark_map = 0;
  if (av->lvalue)
    clear_avar(av->lvalue);
}

static void
clear_var(Var *v) {
  for (int i = 0; i < v->avars.n; i++) if (v->avars[i].key)
    clear_avar(v->avars[i].value);
}

static void
clear_edge(AEdge *e) {
  e->es_backedge = 0;
  e->es_cs_backedge = 0;
  e->args.clear();
  e->rets.clear();
  e->match->formal_filters.clear();
  form_MPositionAVar(x, e->filtered_args)
    clear_avar(x->value);
}

static void
clear_es(EntrySet *es) {
  forv_AEdge(ee, es->edges) if (ee)
    clear_edge(ee);
  es->out_edges.clear();
  es->backedges.clear();
  es->cs_backedges.clear();
  es->creates.clear();
}

static void
clear_cs(CreationSet *cs) {
  cs->defs.clear();      
  cs->ess.clear();      
  cs->es_backedges.clear();
  forv_AVar(v, cs->vars)
    clear_avar(v);
  if (cs->added_element_var)
    clear_avar(get_element_avar(cs));
  cs->closure_used = 0;
}

static void
foreach_var(void (*pfn)(Var*)) {
  forv_Sym(s, fa->pdb->if1->allsyms)
    if (s->var)
      pfn(s->var);
  forv_Fun(f, fa->funs)
    forv_Var(v, f->fa_all_Vars)
      pfn(v);
}

struct ClearVarFn { static void F(Var *v) { 
  clear_var(v); 
} };

static void 
clear_results() {
  foreach_var(clear_var);
  forv_CreationSet(cs, fa->css)
    clear_cs(cs);
  forv_EntrySet(es, fa->ess)
    clear_es(es);
  cannonical_setters.clear();
}

static Setters *
setters_cannonicalize(Setters *s) {
  assert(!s->sorted.n);
  forv_AVar(x, *s)
    if (x)
      s->sorted.add(x);
  if (s->sorted.n > 1)
    qsort_pointers((void**)&s->sorted[0], (void**)s->sorted.end());
  uint h = 0;
  for (int i = 0; i < s->sorted.n; i++)
    h = (uint)(intptr_t)s->sorted[i] * open_hash_primes[i % 256];
  s->hash = h ? h : h + 1; // 0 is empty
  Setters *ss = cannonical_setters.put(s);
  if (!ss) ss = s;
  return ss;
}

#if 0
// Eventual Optimization
static SettersClasses *
setters_classes_cannonicalize(SettersClasses *s) {
  assert(!s->sorted.n);
  forv_Setters(x, *s) if (x) s->sorted.add(x);
  if (s->sorted.n > 1)
    qsort_pointers((void**)&s->sorted[0], (void**)s->sorted.end());
  uint h = 0;
  for (int i = 0; i < s->sorted.n; i++)
    h = (uint)s->sorted[i] * open_hash_primes[i % 256];
  s->hash = h ? h : h + 1; // 0 is empty
  SettersClasses *ss = cannonical_setters_classes.put(s);
  if (!ss) ss = s;
  return ss;
}
#endif

static int
update_setter(AVar *av, AVar *s, Accum<AVar *> &avs) {
  Setters *new_setters = 0;
  avs.add(av);
  if (av->setters) {
    if (av->setters->in(s))
      return 0;
    new_setters = av->setters->add_map.get(s);
    if (new_setters)
      goto Ldone;
  }
  new_setters = new Setters;
  if (av->setters)
    new_setters->copy(*av->setters);
  new_setters->add(s);
  new_setters = setters_cannonicalize(new_setters);
  if (av->setters)
    av->setters->add_map.put(s, new_setters);
 Ldone:
  av->setters = new_setters;
  forv_AVar(x, av->backward) if (x)
    update_setter(x, s, avs);
  return 1;
}

static void
collect_cs_marked_confluences(Vec<AVar *> &confluences) {
  confluences.clear();
  forv_CreationSet(cs, fa->css) {
    forv_AVar(av, cs->vars) {
      forv_AVar(x, av->backward) if (x && x->mark_map) {
        if (!av->contour_is_entry_set && av->contour != GLOBAL_CONTOUR) {
          if (different_marked_args(x, av, 1)) {
            confluences.set_add(av);
            break;
          }
        }
      }
    }
  }
  confluences.set_to_vec();
  qsort_by_id(confluences);
}

static void
split_eq_class(Setters *eq_class, Vec<AVar *> &diff) {
  Setters *diff_class = new Setters, *remaining_class = new Setters;
  diff_class->set_union(diff);
  diff_class = setters_cannonicalize(diff_class);
  eq_class->set_difference(diff, *remaining_class);
  remaining_class = setters_cannonicalize(remaining_class);
  forv_AVar(x, *diff_class) if (x)
    x->setter_class = diff_class;
  forv_AVar(x, *remaining_class) if (x)
    x->setter_class = remaining_class;
}

// AVar->setter_class is the smallest set of setter AVars which
// are equivalent (have the same ->out and equivalent ->setters)
// On a new partition of setters this function recomputes the equiv sets
static void
recompute_eq_classes(Vec<Setters *> &ss) {
  forv_Setters(s, ss) {
    // build new class for unclassed setters
    Setters *new_s = NULL;
    forv_AVar(v, *s) if (v)
      if (!v->setter_class) {
        if (!new_s)
          new_s = new Setters;
        new_s->set_add(v);
      }
    if (new_s) {
      new_s = setters_cannonicalize(new_s);
      forv_AVar(v, *new_s) if (v)
        v->setter_class = new_s;
      // reparition existing classes
      forv_AVar(v, *s) if (v) {
        if (v->setter_class != new_s) {
          Vec<AVar *> diff;
          v->setter_class->set_difference(*s, diff);
          split_eq_class(v->setter_class, diff);
        }
      }
    }
  }
}

enum AKind { AKIND_TYPE, AKIND_SETTER, AKIND_MARK };

static int
compute_setters(AVar *av, Accum<AVar *> &avs, int akind = AKIND_TYPE) {
  if (av->contour_is_entry_set || av->contour == GLOBAL_CONTOUR)
    return 0;
  int setters_changed = 0;
  Vec<Setters *> ss;
  Vec<AVar *> *dir = akind == AKIND_SETTER ? &av->forward : &av->backward;
  forv_AVar(x, *dir) if (x) {
    assert(x->contour_is_entry_set);
    if (akind == AKIND_TYPE && !x->out->type->n) continue;
    if (akind == AKIND_MARK && !x->mark_map) continue;
#if 0
    // group
    for (int i = 0; i < ss.n; i++) {
      forv_AVar(a, *ss[i]) if (a) {
        if ((akind == AKIND_TYPE && 
             (!a->out->type->n || !x->out->type->n || a->out->type == x->out->type)) || 
            (akind == AKIND_SETTER && same_eq_classes(a->setters, x->setters)) ||
            (akind == AKIND_MARK && !different_marked_args(x, a, 1))) 
        {
          ss[i]->set_add(x);
          goto Ldone;
        }
      }
    }
#endif
    ss.add(new Setters);
    ss[ss.n - 1]->set_add(x);
    //  Ldone:;
  }
  for (int i = 0; i < ss.n; i++)
    ss[i] = setters_cannonicalize(ss.v[i]);
  recompute_eq_classes(ss);
  forv_AVar(x, *dir) if (x && x->setter_class)
    setters_changed |= update_setter(x->container, x, avs);
  return setters_changed;
}

static void
collect_setter_confluences(Accum<AVar *> &avs, 
                           Vec<AVar *> &setter_confluences, Vec<AVar *> &setter_starters) 
{
  forv_AVar(av, avs.asvec) {
    if (av->setters) {
      forv_AVar(x, av->forward) if (x) {
        if (x->setters && !same_eq_classes(av->setters, x->setters)) {
          setter_confluences.set_add(av);
          break;
        }
      }
      if (av->cs_map) {
        Vec<CreationSet *> css;
        form_Map(CSMapElem, x, *av->cs_map) if (fa->css_set.set_in(x->value))
          css.set_add(x->value);
        forv_AVar(s, *av->setters) if (s) {
          assert(s->setter_class);
          if (s->container->out->some_intersection(css))
            setter_starters.set_add(av);
        }
      }
    }
  }
  setter_confluences.set_to_vec();
  qsort_by_id(setter_confluences);
  setter_starters.set_to_vec();
  qsort_by_id(setter_starters);
}

static int
split_with_setter_marks(AVar *av) {
  Accum<AVar *> acc;
  build_setter_marks(av, acc);
  Vec<AVar *> confluences;
  collect_es_marked_confluences(confluences, acc, SPLIT_SETTER);
  int analyze_again = 0;
  forv_AVar(av, confluences) {
    if (av->contour_is_entry_set) {
      if (!av->is_lvalue) {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (is_return_value(aav))
          analyze_again |= split_entry_set(aav, SPLIT_SETTER, SPLIT_MARK, SPLIT_EDGES);
      } else
        if (av->var->is_formal)
          analyze_again |= split_entry_set(av, SPLIT_SETTER, SPLIT_MARK, SPLIT_EDGES);
    }
  }
  clear_marks(acc);
  return analyze_again;
}

static int
split_ess_setters_marks(Vec<AVar *> &confluences) {
  int analyze_again = 0;
  forv_AVar(av, confluences)
    if (av->contour_is_entry_set)
      analyze_again |= split_with_setter_marks(av);
  if (!analyze_again)
    forv_AVar(av, confluences)
      if (!av->contour_is_entry_set)
        analyze_again |= split_with_setter_marks(av);
  return analyze_again;
}

static int
split_ess_setters(Vec<AVar *> &confluences) {
  int analyze_again = 0;
  forv_AVar(av, confluences) {
    if (av->contour_is_entry_set) {
      if (!av->is_lvalue) {
        if (is_return_value(av))
          analyze_again |= split_entry_set(av, SPLIT_SETTER, SPLIT_VALUE, SPLIT_EDGES);
      } else {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (aav->var->is_formal)
          analyze_again |= split_entry_set(aav, SPLIT_SETTER, SPLIT_VALUE, SPLIT_EDGES);
      }
    }
  }
  return analyze_again;
}

static int
split_css(Vec<AVar *> &starters) {
  int analyze_again = 0;
  Vec<CreationSet *> css;
  forv_AVar(av, starters)
    form_Map(CSMapElem, x, *av->cs_map) if (fa->css_set.set_in(x->value))
      css.set_add(x->value);
  css.set_to_vec();
  qsort_by_id(css);
  forv_CreationSet(cs, css) {
    Vec<AVar *> starter_set, save;
    forv_AVar(av, starters)
      if (av->cs_map->get(cs->sym) == cs)
        starter_set.add(av);    
    while (starter_set.n > 1) {
      AVar *av = starter_set[0];
      Vec<AVar *> compatible_set;
      forv_AVar(v, starter_set) {
        if (same_eq_classes(v->setters, av->setters))
          compatible_set.set_add(v);
        else
          save.add(v);
      }
      starter_set.move(save);
      Vec<AVar *> new_defs;
      cs->defs.set_difference(compatible_set, new_defs);
      if (new_defs.n) {
        cs->defs.move(new_defs);
        CreationSet *new_cs = new CreationSet(cs);
        forv_AVar(v, compatible_set) if (v) {
          assert(cs == v->cs_map->get(cs->sym));
          v->cs_map->put(cs->sym, new_cs);
        }
        new_cs->split = cs;
        analyze_again = 1;
        log(LOG_SPLITTING, "SPLIT CS %d %s %d -> %d\n", cs->id,
            cs->sym->name ? cs->sym->name : "", cs->sym->id, 
            new_cs->id);
      }
    }
  }
  return analyze_again;
}

static int
split_for_setters(Accum<AVar *> &avs, int analyze_again) {
  Vec<AVar *> setter_confluences, setter_starters;
  collect_setter_confluences(avs, setter_confluences, setter_starters);
  if (split_ess_setters(setter_confluences))
    return 1;
  if (split_ess_setters_marks(setter_confluences))
    return 1;
  if (analyze_again)
    return 1;
  if (split_css(setter_starters))
    return 1;
  return analyze_again;
}

static int
split_with_type_marks(AVar *av, int fdynamic) {
  Accum<AVar *> acc;
  build_type_marks(av, acc);
  Vec<AVar *> confluences;
  collect_es_marked_confluences(confluences, acc, SPLIT_TYPE);
  int analyze_again = 0;
  forv_AVar(av, confluences) {
    if (av->contour_is_entry_set) {
      if (!av->is_lvalue) {
        if (av->var->is_formal) {
          if (split_entry_set(av, SPLIT_TYPE, SPLIT_MARK, fdynamic))
            analyze_again = 1;
        }
      } else {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (is_return_value(aav)) {
          if (split_entry_set(aav, SPLIT_TYPE, SPLIT_MARK, fdynamic))
            analyze_again = 1;
        }
      }
    }
  }
  clear_marks(acc);
  return analyze_again;
}

static void
collect_cs_setter_confluences(Vec<AVar *> &setters_confluences) {
  setters_confluences.clear();
  forv_CreationSet(cs, fa->css) {
    forv_AVar(av, cs->vars) {
      forv_AVar(x, av->forward) if (x) {
        if (!av->contour_is_entry_set && av->contour != GLOBAL_CONTOUR) {
          if (!same_eq_classes(av->setters, x->setters)) {
            setters_confluences.set_add(av);
            break;
          }
        }
      }
    }
    if (cs->added_element_var) {
      AVar *av = get_element_avar(cs);
      forv_AVar(x, av->forward) if (x) {
        if (!av->contour_is_entry_set && av->contour != GLOBAL_CONTOUR) {
          if (!same_eq_classes(av->setters, x->setters)) {
            setters_confluences.set_add(av);
            break;
          }
        }
      }
    }
  }
  setters_confluences.set_to_vec();
}

static int
split_ess_for_type(Vec<AVar *> &imprecisions, int fdynamic) {
  int analyze_again = 0;
  forv_AVar(av, imprecisions) {
    if (av->contour_is_entry_set) {
      if (!av->is_lvalue) {
        if (av->var->is_formal)
          analyze_again |= split_entry_set(av, SPLIT_TYPE, SPLIT_VALUE, fdynamic);
      } else {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (is_return_value(aav))
          analyze_again |= split_entry_set(aav, SPLIT_TYPE, SPLIT_VALUE, fdynamic);
      }
    }
  }
  return analyze_again;
}

static int
split_ess_for_mark_type(Vec<AVar *> &confluences) {
  int analyze_again = 0;
  // a) first those where the confluence is NOT at an instance variable
  forv_AVar(av, confluences)
    if (av->contour_is_entry_set)
      analyze_again |= split_with_type_marks(av, SPLIT_EDGES);
  // b) then those where the confluence is at an instance variable
  if (!analyze_again)
    forv_AVar(av, confluences)
      if (!av->contour_is_entry_set)
        analyze_again |= split_with_type_marks(av, SPLIT_EDGES);
  return analyze_again;
}

static int
back_reaching(AVar *av, Vec<AVar *> &reached) {
  if (reached.set_in(av))
    return 1;
  Accum<AVar *> seen;
  seen.add(av);
  forv_AVar(x, seen.asvec)
    forv_AVar(r, x->backward) if (r) {
      if (reached.set_in(r))
        return 1;
      seen.add(r);
    }
  return 0;
}

static void
all_back_reaching(Vec<AVar *> &dispatched, Vec<AVar *> &reached, Vec<AVar *> &result) {
  forv_AVar(av, dispatched)
    if (back_reaching(av, reached))
      result.set_add(av);
}

static int
is_call_result(AVar *av) {
  PNode *p = av->var->def;
  if (p && av->contour_is_entry_set) {
    EntrySet *es = (EntrySet*)av->contour;
    return !!es->out_edge_map.get(p);
  }
  return 0;
}

static int
result_is_different(AVar *result, AEdge *e) {
  for (int i = 0; i < e->pnode->lvals.n; i++)
    if (result == e->rets[i])
      return e->to->rets[i]->out->type != result->out->type;
  assert(!"found");
  return 0;
}

static void
collect_violation_imprecisions(Vec<ATypeViolation *> &violations, Vec<AVar *> &imprecisions) {
  forv_ATypeViolation(v, violations) if (v) {
    if (v->av->container && v->av->container->out->n > 1)
      imprecisions.set_add(v->av->container);
    if (is_call_result(v->av)) {
      Vec<AVar *> dispatched;
      PNode *p = v->av->var->def;
      EntrySet *es = (EntrySet*)v->av->contour;
      Vec<AEdge *> *ve = es->out_edge_map.get(p);
      if (ve) {
        forv_AEdge(e, *ve) if (e && es->out_edges.set_in(e)) {
          if (result_is_different(v->av, e)) {
            form_MPositionAVar(x, e->args) {
              if (e->to->filters.get(x->key))
                dispatched.set_add(x->value);
              if (e->match->formal_filters.get(x->key) != x->value->out)
                dispatched.set_add(x->value);
            }
          }
        }
      }
      Vec<AVar *> args;
      form_MPositionAVar(x, es->args)
        args.set_add(x->value);
      all_back_reaching(dispatched, args, imprecisions);
    }
  }
  imprecisions.set_to_vec();
}

static int
split_for_violations(Vec<ATypeViolation *> &violations) {
  Vec<AVar *> imprecisions;
  collect_violation_imprecisions(violations, imprecisions);
  int analyze_again = split_ess_for_type(imprecisions, SPLIT_DYNAMIC);
  if (!analyze_again)
    forv_AVar(av, imprecisions)
      analyze_again |= split_with_type_marks(av, SPLIT_DYNAMIC);
  return analyze_again;
}

static void
clear_splits() {
  forv_EntrySet(es, fa->ess) 
    es->split = 0;
  forv_CreationSet(cs, fa->css) 
    cs->split = 0;
}

static int
split_for_setters_of_setters(Vec<AVar *> &confluences) {
  int analyze_again = 0;
  // split based on setters
  while (!analyze_again) {
    // a) compute setters for ivar confluences
    collect_cs_setter_confluences(confluences);
    Accum<AVar *> avs;
    int progress = 0;
    forv_AVar(av, confluences)
      progress |= compute_setters(av, avs, AKIND_SETTER);
    // b) stop if no progress
    if (!progress) break;
    // c) split EntrySet(s) and CreationSet(s) for setter confluences
    if (split_for_setters(avs, analyze_again)) {
      analyze_again = 1;
      break;
    }
  }
  return analyze_again;
}

static int
extend_analysis() {
  int analyze_again = 0;
  extend_timer.restart();
  compute_recursive_entry_sets();
  compute_recursive_entry_creation_sets();
  clear_splits();
  Vec<AVar *> confluences;
  // 1) split EntrySets based on type using AVar::out
  collect_type_confluences(confluences);
  analyze_again = split_ess_for_type(confluences, SPLIT_EDGES);
  // 2) split EntrySets based on type using marks
  if (!analyze_again)
    analyze_again = split_ess_for_mark_type(confluences);
  // 3) split based on setters of type 
  if (!analyze_again) {
    Accum<AVar *> avs;
    forv_AVar(av, confluences)
      compute_setters(av, avs, AKIND_TYPE);
    if (split_for_setters(avs, analyze_again))
      analyze_again = 1;
    if (!analyze_again)
      analyze_again = split_for_setters_of_setters(confluences);
  }
  // 4) split based on setters of type using marks
  if (!analyze_again) {
    forv_AVar(av, confluences) {
      Accum<AVar *> acc;
      build_type_marks(av, acc);
      Vec<AVar *> marked_confluences;
      collect_cs_marked_confluences(marked_confluences);
      Accum<AVar *> avs;
      forv_AVar(av, marked_confluences)
        compute_setters(av, avs, AKIND_MARK);
      if (split_for_setters(avs, analyze_again))
        analyze_again = 1;
      if (!analyze_again)
        analyze_again = split_for_setters_of_setters(confluences);
    }
  }
  if (!analyze_again)
    // 5) split AEdges(s) and EntrySet(s) for violations based on type using dynamic dispatch
    analyze_again = split_for_violations(type_violations);
  extend_timer.stop();
  if (analysis_pass > IFA_PASS_LIMIT)
    analyze_again = 0;
  if (analyze_again)
    clear_results();
  pass_timer.stop();
  ++analysis_pass;
  if (ifa_verbose) {
    double flow = pass_timer.time - extend_timer.time - match_timer.time;
    printf("PASS %d COMPLETE: %f seconds, %f flow (%d%%), %f match (%d%%), %f extend (%d%%)\n", 
           analysis_pass, pass_timer.time, 
           flow, (int)(flow*100.0/pass_timer.time),
           match_timer.time, (int)(match_timer.time*100.0/pass_timer.time),
           extend_timer.time, (int)(extend_timer.time*100.0/pass_timer.time));
  }
  match_timer.accumulate();
  extend_timer.accumulate();
  pass_timer.accumulate();
  log(LOG_SPLITTING, "======== pass %d ========\n", analysis_pass);
  if (!analyze_again && ifa_verbose) {
    double flow = pass_timer.accumulator[0] - extend_timer.accumulator[0] - match_timer.accumulator[0];
    printf("COMPLETE: %f seconds, %f flow (%d%%), %f match (%d%%) cached (%d%%), %f extend (%d%%)\n", 
           pass_timer.accumulator[0], 
           flow, (int)(flow*100.0/pass_timer.accumulator[0]),
           match_timer.accumulator[0], (int)(match_timer.accumulator[0]*100.0/pass_timer.accumulator[0]),
           (int)(((double)pattern_match_hits / (double)pattern_match_complete) * 100.0),
           extend_timer.accumulator[0], (int)(extend_timer.accumulator[0]*100.0/pass_timer.accumulator[0]));
  }
  return analyze_again;
}

static void 
set_void_lub_types_to_void(Var *v) { 
  CreationSet *s = void_type->v[0];
  for (int i = 0; i < v->avars.n; i++) if (v->avars[i].key) {
    AVar *av = v->avars[i].value;
    if (av->out->in(s))
      av->out = void_type;
  }
}

static void
set_void_lub_types_to_void() {
  foreach_var(set_void_lub_types_to_void);
}

static void 
remove_unused_closures(Var *v) { 
  for (int i = 0; i < v->avars.n; i++) if (v->avars[i].key) {
    AVar *av = v->avars[i].value;
    forv_CreationSet(cs, av->out->sorted)
      if (cs->sym == sym_closure && !cs->closure_used) {
        Vec<CreationSet *> css;
        forv_CreationSet(cs, av->out->sorted) {
          if (cs->sym == sym_closure && !cs->closure_used)
            continue;
          css.add(cs);
        }
        av->out = make_AType(css);
        return;
      }
  }
}

static void
remove_unused_closures() {
  foreach_var(remove_unused_closures);
}

static void
complete_pass() {
  collect_results();
  collect_argument_type_violations();
  collect_var_type_violations();
  pass_timer.stop();
}

int
FA::analyze(Fun *top) {
  ::fa = this;
  initialize();
  top_edge = make_top_edge(top);
  do {
    initialize_pass();
    edge_worklist.enqueue(top_edge);
    while (edge_worklist.head || send_worklist.head) {
      while (AEdge *e = edge_worklist.pop()) {
        e->in_edge_worklist = 0;
        analyze_edge(e);
      }
      while (AVar *send = send_worklist.pop()) {
        send->in_send_worklist = 0;
        add_send_edges_pnode(send->var->def, (EntrySet*)send->contour);
      }
    }
    complete_pass();
  } while (extend_analysis());
  set_void_lub_types_to_void();
  remove_unused_closures();
  if1->callback->report_analysis_errors(type_violations);
  show_violations(fa, stderr);
  if (fruntime_errors)
    convert_NOTYPE_to_void();
  return (!fruntime_errors && type_violations.n) ? -1 : 0;
}

static Var *
info_var(IFAAST *a, Sym *s) {
  if (!s)
    s = a->symbol();
  if (!s)
    return 0;
  if (a && a->pnodes.n) {
    forv_PNode(n, a->pnodes) {
      forv_Var(v, n->lvals)
        if (v->sym == s)
          return v;
      forv_Var(v, n->lvals)
        if (v->sym == s)
          return v;
      forv_Var(v, n->rvals)
        if (v->sym == s)
          return v;
    }
  }
  if (s->var)
    return s->var;
  return 0;
}

// Given an IFAAST node and a Sym, find the Sym which
// corresponds to the concrete (post-cloning) type of the
// variable corresponding to the Sym at that IFAAST node.
Sym *
type_info(IFAAST *a, Sym *s) {
  Var *v = info_var(a, s);
  if (v)
    return v->type;
  return 0;
}

// Given a function and an IFAAST node, return the set of
// functions which could be called from that IFAAST node.
void
call_info(Fun *f, IFAAST *a, Vec<Fun *> &funs) {
  funs.clear();
  forv_PNode(n, a->pnodes) {
    Vec<Fun *> *ff = f->calls.get(n);
    if (ff)
      funs.set_union(*ff);
  }     
  funs.set_to_vec();
}

// Given a variable return the vector of constants
// which that variable could take on.
// Returns 0 for no constants or non-constant (e.g. some integer).
int
constant_info(Var *v, Vec<Sym *> &constants) {
  for (int i = 0; i < v->avars.n; i++) if (v->avars[i].key) {
    AVar *av = v->avars[i].value;
    forv_CreationSet(cs, *av->out) if (cs) {
      if (cs->sym->constant)
        constants.set_add(cs->sym);
      else {
        constants.clear();
        return 0;
      }
    }
  }
  constants.set_to_vec();
  return constants.n;
}

// Given an IFAAST node and a Sym, find the set of
// constants which could arrive at that point.
// make sure that there is not some dominating
// non-constant type.
int
constant_info(IFAAST *a, Vec<Sym *> &constants, Sym *s) {
  constants.clear();
  Var *v = info_var(a, s);
  if (v)
    return constant_info(v, constants);
  return 0;
}

int
symbol_info(Var *v, Vec<Sym *> &symbols) {
  for (int i = 0; i < v->avars.n; i++) if (v->avars[i].key) {
    AVar *av = v->avars[i].value;
    forv_CreationSet(cs, *av->out) if (cs) {
      if (cs->sym->is_symbol)
        symbols.set_add(cs->sym);
      else {
        symbols.clear();
        return 0;
      }
    }
  }
  symbols.set_to_vec();
  return symbols.n;
}

void
return_nil_transfer_function(PNode *pn, EntrySet *es) {
  AVar *result = make_AVar(pn->lvals[0], es);
  update_gen(result, make_abstract_type(sym_void));
}

void
return_int_transfer_function(PNode *pn, EntrySet *es) {
  AVar *result = make_AVar(pn->lvals[0], es);
  update_gen(result, make_abstract_type(sym_int));
}

void
return_string_transfer_function(PNode *pn, EntrySet *es) {
  AVar *result = make_AVar(pn->lvals[0], es);
  update_gen(result, make_abstract_type(sym_string));
}

// to be called from the debugger

void
pp(AVar *av) {
  printf("(AVar %d ", av->id);
  if1_dump_sym(stdout, av->var->sym);
  printf(" OUT: ");
  pp(av->out);
  printf(")\n");
}

void
pp(AType *t) {
  printf("(AType %d ", t->n);
  forv_CreationSet(cs, t->sorted)
    pp(cs);
  printf(")\n");
}

void
pp(CreationSet *cs) {
  printf("(CreationSet %d ", cs->id);
  if1_dump_sym(stdout, cs->sym);
  printf(" defs: %d ", cs->defs.n);
  printf(" vars: %d ", cs->vars.n);
  printf(")\n");
}
