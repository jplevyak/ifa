#include "ifadefs.h"

#include "fa.h"
#include "ast.h"
#include "builtin.h"
#include "clone.h"
#include "dead.h"
#include "fail.h"
#include "fun.h"
#include "graph.h"
#include "if1.h"
#include "inline.h"
#include "log.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "timer.h"
#include "var.h"

/* runtime options
 */
bool fgraph_pass_contours = false;
int write_code_exit = 0;

int analysis_pass = 0;

// The 17 canonical AType pointers (bottom_type, void_type, top_type,
// any_type, bool_type, ..., function_type) are now members of
// TypeWorld on FA (tier-3 reentrancy step 3). See fa.h.

// avar_id, aedge_id, creation_set_id, entry_set_id are now members
// of FA (tier-3 reentrancy step 4). See fa.h.

FA *fa = nullptr;
static Timer pass_timer, match_timer, extend_timer;

// cannonical_atypes / cannonical_setters / type_fold_cache /
// type_violation_hash now live on `FA::type_world` (tier-3
// reentrancy step 2). Access via `fa->type_world.X`.

// edge_worklist, send_worklist, es_worklist, entry_set_done,
// type_violations are now members of FA (tier-3 reentrancy step 1).
// Access via `fa->edge_worklist` etc. inside the splitter call tree
// (the global `fa` is set at FA::analyze entry).

static int application(PNode *p, EntrySet *es, AVar *fun, CreationSet *s, Vec<AVar *> &args, Vec<cchar *> &names,
                       int is_closure, Partial_kind partial, PNode *visibility_point, Vec<CreationSet *> *closures);

// Reset all module-level analysis state. Called by ifa_reset() so test
// runners can use a fresh IF1 without stale ATypes, worklists, or IDs
// from a prior run leaking in.
void fa_reset() {
  analysis_pass = 0;
  // Canonical types now live on TypeWorld per FA; FA destruction
  // handles them.
  // id counters live on FA now; FA destruction handles them.
  fa = nullptr;
  pass_timer.reset();    match_timer.reset();    extend_timer.reset();
  memset(pass_timer.accumulator,   0, sizeof(pass_timer.accumulator));
  memset(match_timer.accumulator,  0, sizeof(match_timer.accumulator));
  memset(extend_timer.accumulator, 0, sizeof(extend_timer.accumulator));
  // hash-cons caches (cannonical_atypes / cannonical_setters /
  // type_fold_cache / type_violation_hash) and the worklists are
  // now per-FA on FA::type_world / FA itself. FA destruction
  // handles them.
}

// ---------------------------------------------------------------------------
// FA-pass-event sidecar (issue 003). Disabled by default; production
// pays nothing. Mirrors InlineEvent in ifa/optimize/inline.cc.
// ---------------------------------------------------------------------------
// fa_events_enabled and fa_events_storage are now members of FA.
// The free functions below delegate via `pdb->fa` because they are
// called *before* FA::analyze sets the global `fa` pointer.

void fa_events_enable()  { if (pdb && pdb->fa) pdb->fa->fa_events_enabled = true; }
void fa_events_disable() { if (pdb && pdb->fa) pdb->fa->fa_events_enabled = false; }
void fa_events_reset()   { if (pdb && pdb->fa) pdb->fa->fa_events_storage.clear(); }
const Vec<FAPassEvent *> &fa_events_get() {
  static const Vec<FAPassEvent *> empty;
  return (pdb && pdb->fa) ? pdb->fa->fa_events_storage : empty;
}

// Issue 033 M0: display name for a FAPassStage, for -v measurement
// output. Order matches the enum in fa.h.
static const char *fa_pass_stage_name(FAPassStage stage) {
  static const char *names[FA::kNumFAPassStages] = {
      "type_confluence", "mark_type", "setter", "setter_of_setter", "mark_setter", "mark_setter_of_setter",
      "violation",
  };
  int i = (int)stage;
  return (i >= 0 && i < FA::kNumFAPassStages) ? names[i] : "?";
}

static void record_fa_event(FAPassStage stage, int splits, int ess_before, int css_before, int viol_before) {
  if (!fa->fa_events_enabled) return;
  FAPassEvent *e = new FAPassEvent;
  e->pass = analysis_pass + 1;  // extend_analysis hasn't incremented yet
  e->stage = stage;
  e->splits = splits;
  e->ess_before = ess_before;
  e->ess_after = fa->ess.n;
  e->css_before = css_before;
  e->css_after = fa->css.n;
  e->violations_before = viol_before;
  // type_violations is a Vec used as a pointer-set; `.n` is the
  // open-addressed table capacity (varies non-deterministically as
  // probe chains may or may not trigger set_expand), not the live
  // element count. Use `.set_count()` to report the actual number
  // of distinct violations. See issue 009.
  e->violations_after = fa->type_violations.set_count();
  if (getenv("IFA_DEBUG_VIOLATIONS")) {
    fprintf(stderr, "VIOL_EVENT pass=%d stage=%d viol.n=%d viol.set_count=%d\n",
            e->pass, (int)stage, fa->type_violations.n, fa->type_violations.set_count());
  }
  fa->fa_events_storage.add(e);
}

AEdge::AEdge() : from(nullptr), to(nullptr), pnode(nullptr), fun(nullptr), match(nullptr), in_edge_worklist(0) { id = fa->aedge_id++; }

uint PendingMapHash::hash(AEdge *e) {
  return (uint)(uintptr_t)(e->fun ? e->fun->id : 0) +
         combine_hash((uintptr_t)(e->pnode ? e->pnode->id : 0), (uintptr_t)(e->from ? e->from->id : 0));
}

AVar::AVar(Var *v, void *acontour)
    : var(v),
      contour(acontour),
      lvalue(nullptr),
      gen(nullptr),
      in(fa->type_world.bottom_type),
      out(fa->type_world.bottom_type),
      restrict(nullptr),
      restrict_pred(RP_None),
      restrict_pred_cls(nullptr),
      container(nullptr),
      setters(nullptr),
      setter_class(nullptr),
      mark_map(nullptr),
      cs_map(nullptr),
      match_cache(nullptr),
      type(nullptr),
      num_coerce(nullptr),
      ivar_offset(0),
      in_send_worklist(0),
      contour_is_entry_set(0),
      is_lvalue(0),
      live(0),
      live_arg(0),
      is_if_arg(0),
      escape(ES_Escape),  // Phase 1: conservative top
      needs_fat(0) {
  id = fa->avar_id++;
}

AType::AType(AType &a) {
  hash = 0;
  this->copy(a);
}

AType::AType(CreationSet *cs) {
  hash = 0;
  set_add(cs);
}

AVar *unique_AVar(Var *v, void *contour) {
  assert(contour);
  AVar *av = v->avars.get(contour);
  if (av) return av;
  av = new AVar(v, contour);
  v->avars.put(contour, av);
  return av;
}

AVar *unique_AVar(Var *v, EntrySet *es) {
  assert(es);
  AVar *av = v->avars.get(es);
  if (av) return av;
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

CreationSet::CreationSet(Sym *s)
    : sym(s),
      dfs_color(DFS_white),
      clone_for_constants(0),
      added_element_var(0),
      closure_used(0),
      tuple_able(0),
      atype(nullptr),
      equiv(nullptr),
      type(nullptr) {
  id = fa->creation_set_id++;
}

CreationSet::CreationSet(CreationSet *cs)
    : dfs_color(DFS_white), added_element_var(0), closure_used(0), tuple_able(0),
      atype(nullptr), equiv(nullptr), type(nullptr) {
  sym = cs->sym;
  id = fa->creation_set_id++;
  clone_for_constants = cs->clone_for_constants;
  for (AVar *v : cs->vars) {
    AVar *iv = unique_AVar(v->var, this);
    add_var_constraint(iv);
    vars.add(iv);
    if (iv->var->sym->name) var_map.put(iv->var->sym->name, iv);
  }
  sym->creators.add(this);
}

EntrySet::EntrySet(Fun *af)
    : fun(af), dfs_color(DFS_white), in_es_worklist(0), can_raise(0), split(nullptr), equiv(nullptr) {
  id = fa->entry_set_id++;
}

AVar *make_AVar(Var *v, EntrySet *es) {
  if (v->sym->nesting_depth) {
    if (v->sym->nesting_depth != es->fun->sym->nesting_depth + 1)
      return unique_AVar(v, es->display[v->sym->nesting_depth - 1]);
    return unique_AVar(v, es);
  }
  if (v->is_internal) return unique_AVar(v, es);
  return unique_AVar(v, GLOBAL_CONTOUR);
}

// static inline AVar *make_AVar(Var *v, AEdge *e) { return make_AVar(v, e->to); }

AType *make_AType(CreationSet *cs) {
  if (cs->atype) return cs->atype;
  return cs->atype = type_cannonicalize(new AType(cs));
}

AType *make_abstract_type(Sym *s) {
  s = unalias_type(s);
  AType *a = s->abstract_type;
  if (a) return a;
  CreationSet *cs = new CreationSet(s);
  return s->abstract_type = make_AType(cs);
}

AType *make_AType(Vec<CreationSet *> &css) {
  AType *t = new AType();
  t->set_union(css);
  return type_cannonicalize(t);
}

AType *AType::constants() {
  AType *t = new AType();
  for (CreationSet *cs : this->sorted) if (cs->sym->constant) t->set_add(cs);
  return type_cannonicalize(t);
}

static inline bool restrict_pred_keeps(AVar *v, CreationSet *cs) {
  switch (v->restrict_pred) {
    case RP_IsNilType:    return cs && cs->sym && cs->sym->type == sym_nil_type;
    case RP_IsNotNilType: return cs && cs->sym && cs->sym->type != sym_nil_type;
    case RP_IsInstanceOf:
      return cs && cs->sym && v->restrict_pred_cls && v->restrict_pred_cls->meta_type &&
             v->restrict_pred_cls->meta_type->implementors.in(cs->sym->type);
    case RP_NotInstanceOf:
      return cs && cs->sym && v->restrict_pred_cls && v->restrict_pred_cls->meta_type &&
             !v->restrict_pred_cls->meta_type->implementors.in(cs->sym->type);
    default:
      return true;
  }
}

static AType *apply_restrict_pred(AVar *v, AType *t) {
  if (v->restrict_pred == RP_None) return t;
  if (t == fa->type_world.bottom_type || t == fa->type_world.top_type) return t;
  AType *r = new AType();
  for (CreationSet *cs : t->sorted)
    if (restrict_pred_keeps(v, cs)) r->set_add(cs);
  return type_cannonicalize(r);
}

// Shared out-change propagation tail for update_in /
// flow_var_type_permit / flow_var_permit_pred (survey S1):
// enqueue dependent sends, resume any IF blocked on this AVar
// (add_pnode_constraints stops its CFG walk at a bottom-typed
// condition and relies on this re-enqueue), and push the new
// `out` forward. The permit variants historically re-implemented
// this tail and omitted the IF resume.
static void propagate_out_change(AVar *v) {
  if (!v->dirty) {
    v->dirty = 1;
    ++fa->dirty_avar_count;
  }
  for (AVar *vv : v->arg_of_send.asvec) {
    if (!vv->in_send_worklist) {
      vv->in_send_worklist = 1;
      fa->send_worklist.enqueue(vv);
    }
  }
  if (v->is_if_arg) {
    // A global AVar can be an if-arg too (e.g. the `True`
    // constant conditioning a top-level `while True:` —
    // issues/005). Its contour is the distinguished
    // `fa->global_es` (see GLOBAL_CONTOUR in fa.h), a real
    // EntrySet whose `in_es_worklist` is permanently 1, so
    // this deref is safe and the enqueue self-suppresses —
    // sound, since the global contour has no per-ES state
    // to re-analyze.
    EntrySet *es = (EntrySet *)v->contour;
    if (!es->in_es_worklist) {
      es->in_es_worklist = 1;
      fa->es_worklist.enqueue(es);
    }
  }
  // Issue 035: forward is an open-hash set — cascading update_in
  // in bucket (heap-layout) order lets the constant-cap's
  // order-sensitive union reach different fixpoints run to run.
  Vec<AVar *> fwd;
  for (AVar *vv : v->forward) if (vv) fwd.add(vv);
  if (fwd.n > 1) qsort_by_id(fwd);
  for (AVar *vv : fwd) update_in(vv, v->out);
}

// Issue 025 numeric unification: map every numeric CS of a type
// other than `w` to `w` -- constants to the coerced constant of `w`
// (0 -> 0.0, value-preserving, free at compile time), non-constant
// numerics to abstract `w` (the runtime value converts at the
// assignment: C's conversion-on-assignment; verified for the LLVM
// path by the regression tests). Applied to an AVar's out when
// av->num_coerce is set (see fa.h). Element-wise, so it is
// monotone: as `t` grows the result only grows -- required for use
// inside the fixpoint. Mapping the abstract narrows too (not just
// constants) matters even for constant-only programs: `in` keeps
// the original int constant, and once the loop's folded constants
// exceed num_constants_per_variable, type_cannonicalize's cap-strip
// rebuilds `in` with every constant's BASE type -- resurrecting an
// abstract int64 from the already-coerced-away constant.
static AType *type_coerce_numeric_constants(AType *t, Sym *w) {
  AType *r = t->coerce_map.get(w);
  if (r) return r;
  Vec<CreationSet *> css;
  int changed = 0;
  for (CreationSet *cs : t->sorted) {
    Sym *ct = cs->sym->type;
    if (ct && ct->num_kind && ct != w) {
      if (cs->sym->is_constant) {
        Immediate to;
        to.const_kind = w->num_kind;
        to.num_index = w->num_index;
        Immediate from = cs->sym->imm;
        coerce_immediate(&from, &to);
        css.set_add(make_abstract_type(imm_constant(to, w))->v[0]);
      } else
        css.set_add(make_abstract_type(w)->v[0]);
      changed = 1;
    } else
      css.set_add(cs);
  }
  r = changed ? make_AType(css) : t;
  t->coerce_map.put(w, r);
  return r;
}

void update_in(AVar *v, AType *t) {
  AType *tt = type_union(v->in, t);
  if (tt != v->in) {
    assert(tt && tt != fa->type_world.top_type);
    v->in = tt;
    if (v->restrict) tt = type_intersection(v->in, v->restrict);
    if (v->restrict_pred != RP_None) tt = apply_restrict_pred(v, tt);
    if (v->num_coerce) tt = type_coerce_numeric_constants(tt, v->num_coerce);
    if (tt != v->out) {
      assert(tt != fa->type_world.top_type);
      v->out = tt;
      propagate_out_change(v);
    }
  }
}

void update_gen(AVar *v, AType *t) {
  if (v->gen) {
    AType *tt = type_union(v->gen, t);
    if (tt == v->gen) return;
    v->gen = tt;
  } else
    v->gen = t;
  update_in(v, v->gen);
}

static void flow_var_to_var(AVar *a, AVar *b) {
  if (a == b) return;
  if (a->forward.set_in(b)) return;
  a->forward.set_add(b);
  b->backward.set_add(a);
  update_in(b, a->out);
}

void flow_vars(AVar *v, AVar *vv) {
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

void flow_vars_assign(AVar *rhs, AVar *lhs) {
  flow_var_to_var(rhs, lhs);
  if (lhs->lvalue) flow_var_to_var(rhs, lhs->lvalue);
}

CreationSet *creation_point(AVar *v, Sym *s, int nvars) {
  CreationSet *cs = v->cs_map ? v->cs_map->get(s) : 0;
  EntrySet *es = (EntrySet *)v->contour;
  if (cs) {
    assert(cs->sym == s);
    goto Lfound;
  }
  if (s == sym_closure) goto Lunique;
  // `es` may be the distinguished global contour (fa->global_es);
  // its `split` is always null, so the split-lookup below
  // naturally no-ops for globals.
  //
  // ifa/issues/045: instances of clone_methods_per_cs classes must
  // NOT reuse the split parent's CS -- distinct per-constant
  // contours exist precisely to give each constant binding its own
  // instance CS (issue 040: both range(0,0) and range(0,2) contours
  // funneled into ONE range CS through this reuse, merging the i/j
  // field constants the hard per-constant ES split had separated).
  {
    Sym *cmc = s->clone_methods_per_cs ? s : (s->type ? unalias_type(s->type) : 0);
    if (cmc && cmc->clone_methods_per_cs) goto Lcreators;
  }
  if (es && es->split) {
    AVar *oldv = make_AVar(v->var, es->split);
    cs = oldv->cs_map ? oldv->cs_map->get(s) : 0;
    if (cs) {
      assert(cs->sym == s);
      goto Lfound;
    }
  }
Lcreators:;
  for (CreationSet *x : s->creators) {
    if (s->abstract_type && x == s->abstract_type->v[0]) continue;
    if (nvars != -1 || x->vars.n != nvars) continue;
    cs = x;
    goto Lfound;
  }
Lunique:
  // new creation set
  cs = new CreationSet(s);
  s->creators.add(cs);
  for (Sym *h : s->has) {
    assert(h->var);
    AVar *iv = unique_AVar(h->var, cs);
    add_var_constraint(iv);
    cs->vars.add(iv);
    if (h->name) cs->var_map.put(h->name, iv);
  }
Lfound:
  if (!v->cs_map) v->cs_map = new CSMap;
  v->cs_map->put(s, cs);
  cs->defs.set_add(v);
  if (v->contour_is_entry_set) ((EntrySet *)v->contour)->creates.set_add(cs);
  update_gen(v, make_AType(cs));
  return cs;
}

//  all float combos become doubles
//  all signed/unsigned combos become signed
//  all int combos below 32 bits become signed 32 bits, above become signed 64
//  bits
Sym *coerce_num(Sym *a, Sym *b) {
  if (a == b) return a;
  if (a == sym_string || b == sym_string) return sym_string;
  if (a->num_kind == b->num_kind) {
    if (a->num_index > b->num_index)
      return a;
    else
      return b;
  }
  if (b->num_kind == IF1_NUM_KIND_FLOAT) {
    Sym *t = b;
    b = a;
    a = t;
  }
  if (b->num_kind == IF1_NUM_KIND_COMPLEX) {
    Sym *t = b;
    b = a;
    a = t;
  }
  // Survey B2: these lookups used to index the precision tables by
  // num_kind (the KIND enum, 0..4) instead of num_index, which made
  // every int operand read a precision of 8 or 16 -- so the
  // "does the int fit the float?" test always said yes, the widening
  // branches were dead, and (had they been reachable) the wide-int
  // case returned the NARROW float. Now: index by num_index, and a
  // >=32-bit int that doesn't fit widens to the 64-bit float/complex.
  if (a->num_kind == IF1_NUM_KIND_COMPLEX) {
    if (b->num_kind == IF1_NUM_KIND_FLOAT) {
      if (a->num_index > b->num_index) return a;
      return if1->complex_types[b->num_index];
    }
    if (int_type_precision[b->num_index] <= float_type_precision[a->num_index]) return a;
    if (int_type_precision[b->num_index] >= 32) return sym_complex64;
    return sym_complex32;
  }
  if (a->num_kind == IF1_NUM_KIND_FLOAT) {
    if (int_type_precision[b->num_index] <= float_type_precision[a->num_index]) return a;
    if (int_type_precision[b->num_index] >= 32) return sym_float64;
    return sym_float32;
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

AType *type_num_fold(Prim *p, AType *a, AType *b) {
  (void)p;
  p = 0;  // for now
  a = type_intersection(a, fa->type_world.anynum_kind);
  b = type_intersection(b, fa->type_world.anynum_kind);
  ATypeFold f(p, a, b), *ff;
  if ((ff = fa->type_world.type_fold_cache.get(&f))) return ff->result;
  AType *r = new AType();
  for (CreationSet *acs : a->sorted) {
    Sym *atype = acs->sym->type;
    for (CreationSet *bcs : b->sorted) {
      Sym *btype = bcs->sym->type;
      r->set_add(coerce_num(atype, btype)->abstract_type->v[0]);
    }
  }
  r = type_cannonicalize(r);
  fa->type_world.type_fold_cache.put(new ATypeFold(p, a, b, r));
  return r;
}

void qsort_pointers(void **left, void **right) {
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
    void **i = left + 1, **j = right - 1, *x = *left;
    for (;;) {
      while (x < *j) j--;
      while (i < j && *i < x) i++;
      if (i >= j) break;
      void *t = *i;
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
    if (left < j) qsort_pointers(left, j + 1);
    if (j + 2 < right) qsort_pointers(j + 1, right);
  }
}

AType *type_cannonicalize(AType *t) {
  assert(!t->sorted.n);
  assert(!t->union_map.n);
  assert(!t->intersection_map.n);
  int consts = 0, rebuild = 0, nulls = 0;
  Vec<CreationSet *> nonconsts;
  for (CreationSet *cs : *t) if (cs) {
    // strip out constants if the base type is included
    CreationSet *base_cs = nullptr;
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
      if (!cs->sym->is_unique_type)  // e.g. nil, void, or unknown
        nonconsts.set_add(cs);
      else
        nulls = 1;
    }
    t->sorted.add(cs);
  }
  if (consts > fa->num_constants_per_variable) rebuild = 1;
  if (rebuild) {
    t->sorted.clear();
    t->sorted.append(nonconsts);
    t->clear();
    t->set_union(t->sorted);
  }
  if (t->sorted.n > 1) qsort_by_id(t->sorted);
  unsigned int h = 0;
  // Accumulate (survey B1): `h =` here discarded all but the last
  // element, collapsing the hash-cons table's distribution to
  // last-element groups. Position sensitivity comes from the
  // per-index prime.
  for (int i = 0; i < t->sorted.n; i++) h += (uint)(intptr_t)t->sorted[i] * open_hash_primes[i % 256];
  t->hash = h ? h : h + 1;  // 0 is empty
  AType *tt = fa->type_world.cannonical_atypes.put(t);
  if (!tt) tt = t;
  // compute "type" (without constants)
  if (nonconsts.n) {
    if (nulls || consts)
      tt->type = make_AType(nonconsts);
    else
      tt->type = tt;
  } else
    tt->type = fa->type_world.bottom_type;
  return tt;
}

AType *type_union(AType *a, AType *b) {
  AType *r;
  if ((r = a->union_map.get(b))) return r;
  if (a == b || b == fa->type_world.bottom_type) {
    r = a;
    goto Ldone;
  }
  if (a == fa->type_world.bottom_type) {
    r = b;
    goto Ldone;
  }
  {
    AType *ab = type_diff(a, b);
    AType *ba = type_diff(b, a);
    r = new AType(*ab);
    for (CreationSet *x : ba->sorted) r->set_add(x);
    for (CreationSet *x : a->sorted) if (b->in(x)) r->set_add(x);
    r = type_cannonicalize(r);
  }
Ldone:
  a->union_map.put(b, r);
  return r;
}

static inline int subsumed_by(Sym *a, Sym *b) {
  return (a == b) || a->type == b || b->specializers.set_in(a->type);
}

AType *type_diff(AType *a, AType *b) {
  AType *r;
  if ((r = a->diff_map.get(b))) return r;
  if (b == fa->type_world.bottom_type) {
    r = a;
    goto Ldone;
  }
  r = new AType();
  for (CreationSet *aa : a->sorted) {
    if (aa->defs.n && b->set_in(aa)) continue;
    for (CreationSet *bb : b->sorted) if (!bb->defs.n) {
      if (subsumed_by(aa->sym, bb->sym)) goto Lnext;
    }
    r->set_add(aa);
  Lnext:;
  }
  r = type_cannonicalize(r);
Ldone:
  a->diff_map.put(b, r);
  return r;
}

AType *type_intersection(AType *a, AType *b) {
  // Issue 033: a null filter (a Map<MPosition*,AType*>::get() miss)
  // means "no constraint" -- analyze_edge already treats a missing
  // formal_filters entry this way (fa.cc, the `if (filter) {...}
  // else filter = es_filter;` / `if (filter && ...)` guards around
  // its own type_intersection calls). Some other callers passed a
  // possibly-null filter straight through without that guard,
  // crashing here on `b->sorted`/`a->sorted` when a position simply
  // has no recorded filter yet. Treat null as the intersection
  // identity (return the other operand) to match the established
  // semantic instead of requiring every caller to null-check first.
  if (!b) return a;
  if (!a) return b;
  AType *r;
  if ((r = a->intersection_map.get(b))) return r;
  if (a == b || a == fa->type_world.bottom_type || b == fa->type_world.top_type) {
    r = a;
    goto Ldone;
  }
  if (a == fa->type_world.top_type || b == fa->type_world.bottom_type) {
    r = b;
    goto Ldone;
  }
  r = new AType();
  for (CreationSet *aa : a->sorted) {
    for (CreationSet *bb : b->sorted) {
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
          if (subsumed_by(bb->sym, aa->sym)) r->set_add(bb);
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

static void fill_rets(EntrySet *es, int n) {
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

static bool same_eq_classes(Setters *s, Setters *ss) {
  if (s == ss) return true;
  if (!s || !ss) return false;
  Vec<Setters *> sc1, sc2;
  for (AVar *av : *s) if (av) {
    assert(av->setter_class);
    sc1.set_add(av->setter_class);
  }
  for (AVar *av : *ss) if (av) {
    assert(av->setter_class);
    sc2.set_add(av->setter_class);
  }
  if (sc1.some_disjunction(sc2)) return false;
  return true;
}

static bool edge_nest_compatible_with_entry_set(AEdge *e, EntrySet *es) {
  if (!es->fun->sym->nesting_depth) return true;
  int ef_nd = e->from->fun->sym->nesting_depth, es_nd = es->fun->sym->nesting_depth;
  int n = ef_nd < es_nd ? ef_nd : es_nd;  // MIN
  for (int i = 0; i < n; i++)
    if (e->from->display[i] != es->display.v[i]) return false;
  if (ef_nd < es_nd)  // down call
    if (es->display[es_nd - 1] != e->from) return false;
  return true;
}

static int different_marked_args(AVar *a1, AVar *a2, int offset, AVar *basis = 0) {
  Vec<void *> marks1, marks2;
  AVar *basis1 = basis ? basis : a2;
  int found1 = 0, found2 = 0;
  if (a1->mark_map) {
    form_Map(MarkElem, x, *a1->mark_map) {
      if (basis1->mark_map) {
        int m = basis1->mark_map->get(x->key);
        if (m) {
          found1 = 1;
          if (m - offset == x->value) marks1.set_add(x->key);
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
            if (m - offset == x->value) marks2.set_add(x->key);
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

static int edge_type_compatible_with_edge(AEdge *e, AEdge *ee, EntrySet *es, int fmark = 0) {
  assert(e->args.n && ee->args.n);
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *e_arg = e->args.get(p), *ee_arg = ee->args.get(p);
    if (!e_arg || !ee_arg) continue;
    AType *etype = type_intersection(e_arg->out->type, e->match->formal_filters.get(p));
    AType *eetype = type_intersection(ee_arg->out->type, ee->match->formal_filters.get(p));
    if (!fmark) {
      if (etype->n && eetype->n && etype != eetype) return 0;
    } else {
      AVar *es_arg = es->args.get(p);
      if (different_marked_args(ee_arg, e_arg, 2, es_arg)) return 0;
    }
  }
  if (e->rets.n != ee->rets.n) return 0;
  for (int i = 0; i < e->rets.n; i++) {
    if (ee->rets[i]->lvalue && e->rets.v[i]->lvalue) {
      if (!fmark) {
        if (ee->rets[i]->lvalue->out->type->n && e->rets.v[i]->lvalue->out->type->n &&
            ee->rets[i]->lvalue->out->type != e->rets.v[i]->lvalue->out->type)
          return 0;
      } else {
        if (different_marked_args(ee->rets[i]->lvalue, e->rets.v[i]->lvalue, 1, es->rets[i]->lvalue)) return 0;
      }
    }
  }
  return 1;
}

static int edge_type_compatible_with_entry_set(AEdge *e, EntrySet *es, int fmark = 0) {
  assert(e->args.n && es->args.n);
  if (!es->split) {
    for (MPosition *p : e->match->fun->positional_arg_positions) {
      AVar *es_arg = es->args.get(p), *e_arg = e->args.get(p);
      if (!e_arg) continue;
      AType *etype = type_intersection(e_arg->out->type, e->match->formal_filters.get(p));
      if (!fmark) {
        if (etype->n && es_arg->out->type->n && etype != es_arg->out->type) return 0;
      } else if (different_marked_args(e_arg, es_arg, 2))
        return 0;
    }
    if (es->rets.n != e->rets.n) return 0;
    for (int i = 0; i < e->rets.n; i++) {
      if (es->rets[i]->lvalue && e->rets.v[i]->lvalue) {
        if (!fmark) {
          if (es->rets[i]->lvalue->out->type->n && e->rets.v[i]->lvalue->out->type->n &&
              es->rets[i]->lvalue->out->type != e->rets.v[i]->lvalue->out->type)
            return 0;
        } else if (different_marked_args(es->rets[i]->lvalue, e->rets.v[i]->lvalue, 1))
          return 0;
      }
    }
  } else {
    for (AEdge *ee : es->edges) if (ee) {
      if (!ee->args.n) continue;
      if (!edge_type_compatible_with_edge(e, ee, es, fmark)) return 0;
    }
  }
  return 1;
}

static bool sset_compatible(AVar *av1, AVar *av2) {
  if (!same_eq_classes(av1->setters, av2->setters)) return false;
  if (av1->lvalue && av2->lvalue)
    if (!same_eq_classes(av1->lvalue->setters, av2->lvalue->setters)) return false;
  return true;
}

static bool edge_sset_compatible_with_edge(AEdge *e, AEdge *ee) {
  assert(e->args.n && ee->args.n);
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *eav = e->args.get(p), *eeav = ee->args.get(p);
    if (eav && eeav)
      if (!sset_compatible(eav, eeav)) return false;
  }
  if (e->rets.n != ee->rets.n) return false;
  for (int i = 0; i < e->rets.n; i++)
    if (!sset_compatible(e->rets[i], ee->rets.v[i])) return false;
  return true;
}

static bool edge_sset_compatible_with_entry_set(AEdge *e, EntrySet *es) {
  assert(e->args.n && es->args.n);
  if (!es->split) {
    for (MPosition *p : e->match->fun->positional_arg_positions) {
      AVar *av = e->args.get(p);
      if (av)
        if (!sset_compatible(av, es->args.get(p))) return false;
    }
    if (es->rets.n != e->rets.n) return false;
    for (int i = 0; i < es->rets.n; i++)
      if (!sset_compatible(e->rets[i], es->rets.v[i])) return false;
  } else {
    for (AEdge *ee : es->edges) if (ee) {
      if (!ee->args.n) continue;
      if (!edge_sset_compatible_with_edge(e, ee)) return false;
    }
  }
  return true;
}

static bool edge_constant_compatible_with_entry_set(AEdge *e, EntrySet *es) {
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *av = es->args.get(p);
    if (av->var->sym->clone_for_constants) {
      AType css;
      av->out->set_disjunction(*e->args.get(p)->out, css);
      for (CreationSet *cs : css) if (cs) if (cs->sym->constant) return false;
    }
  }
  return true;
}

static void update_display(AEdge *e, EntrySet *es) {
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

static void set_entry_set(AEdge *e, EntrySet *es = 0) {
  EntrySet *new_es = es;
  if (!es) {
    new_es = new EntrySet(e->match->fun);
    e->match->fun->ess.add(new_es);
  }
  e->to = new_es;
  new_es->edges.put(e);
  if (new_es->fun->sym->nesting_depth) update_display(e, new_es);
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    Var *v = e->match->fun->args.get(p);
    AVar *av = make_AVar(v, new_es);
    new_es->args.put(p, av);
  }
  fill_rets(new_es, e->pnode->lvals.n);
}

static AEdge *new_AEdge(Fun *f, PNode *p, EntrySet *from) {
  AEdge *e = new AEdge;
  e->pnode = p;
  e->from = from;
  e->fun = f;
  return e;
}

static AEdge *new_AEdge(Match *m, PNode *p, EntrySet *from) {
  AEdge *e = new AEdge;
  e->pnode = p;
  e->from = from;
  e->fun = m->fun;
  e->match = m;
  return e;
}

static AEdge *copy_AEdge(AEdge *ee, EntrySet *to) {
  AEdge *e = new_AEdge(ee->match, ee->pnode, ee->from);
  set_entry_set(e, to);
  if (!e->args.n) e->args.copy(ee->args);
  if (!e->rets.n) e->rets.copy(ee->rets);
  Vec<AEdge *> *ve = ee->from->out_edge_map.get(ee->pnode);
  if (!ve) ee->from->out_edge_map.put(ee->pnode, (ve = new Vec<AEdge *>));
  ve->set_add(e);
  return e;
}

static int entry_set_compatibility(AEdge *e, EntrySet *es) {
  int val = INT_MAX;
  if (e->match->fun->split_unique) return 0;
  if (!edge_nest_compatible_with_entry_set(e, es)) return 0;
  switch (edge_type_compatible_with_entry_set(e, es)) {
    case 1:
      break;
    case 0:
#if 0
      // eager splitting doesn't help
      if (analysis_pass == 0 && !initial_compatibility(e, es))
        return 0;
#endif
      val -= 4;
      break;
    case -1:
      return 0;
  }
  if (!edge_sset_compatible_with_entry_set(e, es)) val -= 2;
  if (e->match->fun->clone_for_constants) {
    if (!edge_constant_compatible_with_entry_set(e, es)) {
      // ifa/issues/045: for clone_methods_per_cs classes' functions
      // (ctor wrappers with clone_for_constants formals), differing
      // constants are a HARD incompatibility, not a preference --
      // the soft `val -= 1` still matches the merged ES when no
      // better candidate exists, so `range(0, 0)` and `range(0, 2)`
      // merged their j constants (-> constant cap -> generic int64)
      // and no violation ever forced the split (issue 040's chain,
      // link 2 in its final form). Scoped to the new opt-in flag:
      // making this hard for ALL clone_for_constants functions
      // (list.__getitem__ keys etc.) would eagerly fan out contours
      // that today only split on violation evidence.
      if (e->match->fun->sym && e->match->fun->sym->clone_methods_per_cs) return 0;
      val -= 1;
    }
  }
  return val;
}

static AEdge *set_or_copy_AEdge(AEdge *e, EntrySet *es, Vec<AEdge *> &ees) {
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

static int find_best_entry_sets(AEdge *e, Vec<AEdge *> &edges) {
  EntrySet *es = nullptr;
  int val = -1;
  for (EntrySet *x : e->match->fun->ess) {
    int v = entry_set_compatibility(e, x);
    if (v > 0 && v > val) {
      es = x;
      if (v == INT_MAX) break;
      val = v;
    }
  }
  if (es) {
    set_or_copy_AEdge(e, es, edges);
    return 1;
  }
  return 0;
}

static bool check_edge(AEdge *e, EntrySet *es) {
  form_MPositionAVar(x, e->args) {
    if (!x->key->is_positional()) continue;
    AType *filter = e->match->formal_filters.get(x->key);
    AType *es_filter = es->filters.get(x->key);
    if (filter) {
      if (es_filter) filter = type_intersection(filter, es_filter);
    } else
      filter = es_filter;
    if (filter && type_intersection(x->value->out, filter) == fa->type_world.bottom_type) return false;
  }
  return true;
}

// `avoid` (when non-null) is the EntrySet a type-driven split is
// detaching `e` AWAY from: pending-backedge and parent-split routes
// that would bind the edge straight back into it are skipped. The
// pending map's monomorphic-recursion binding ("recursion follows
// its split-off caller contour" -- record_backedges) is a default,
// not evidence; when the splitter has concrete type evidence that a
// recursive edge does NOT belong with its enclosing contour, the
// default must yield or the split silently no-ops and the same
// decision re-derives every pass (observed: 2-level polymorphic
// recursion -- f([[1,2],[3,4]]) -- stalled with the level-1 contour
// permanently holding {list, int64}).
static int check_split(AEdge *e, Vec<AEdge *> &ees, EntrySet *avoid = nullptr) {
  if (!e->from) return 0;
  if (Vec<EntrySet *> *ess = e->from->pending_es_backedge_map.get(e)) {
    // Issue 035: hash-set Vec — copy_AEdge creation order (edge
    // ids, schedule) must not follow heap layout.
    Vec<EntrySet *> sorted_ess;
    for (EntrySet *es : *ess) if (es && es != avoid) sorted_ess.add(es);
    qsort_by_id(sorted_ess);
    // Bind the edge to ONE recorded ES (the canonical first), not a
    // COPY per recorded ES: argument types haven't flowed at bind
    // time, so a fan-out can't be filtered here, and a residual
    // multi-ES fan on a DIRECT call (constant callee) survives to
    // codegen as an unresolvable dispatch -- write_send emitted
    // `if (fn == &clone1) ... else if (fn == &clone2)` over the
    // callee's own address, always taking branch 1 and calling the
    // wrong contour with the other level's receivers (garbage
    // field reads in issues/029's recursive deepcopy trees). If
    // the single binding is type-wrong, the next pass's splitter
    // re-derives the level split from real evidence -- the same
    // level-by-level convergence the recursive-ES machinery
    // already relies on.
    if (sorted_ess.n) {
      set_or_copy_AEdge(e, sorted_ess[0], ees);
      return 1;
    }
    // Every route was the avoided ES: fall through to the
    // split/fresh-ES paths below.
  }
  if (e->from->split) {
    Vec<AEdge *> *m = e->from->split->out_edge_map.get(e->pnode);
    if (m) {
      // Issue 035: same — first-match routing over a hash-set Vec.
      Vec<AEdge *> sorted_m;
      for (AEdge *ee : *m) if (ee) sorted_m.add(ee);
      qsort_by_id(sorted_m);
      for (AEdge *ee : sorted_m) if (ee) {
        if (ee->to == avoid) continue;
        if (!check_edge(e, ee->to)) continue;
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
      if (ees.n) return 1;
    }
  }
  return 0;
}

static void make_entry_set(AEdge *e, Vec<AEdge *> &edges, EntrySet *split = nullptr, EntrySet *preference = 0) {
  if (e->to) {
    edges.add(e);
    return;
  }
  // `split` is the ES this edge is being detached from (apply_entry_
  // set_split); routes that would re-bind straight back into it are
  // vetoed -- see check_split's `avoid` comment.
  if (check_split(e, edges, split)) return;
  EntrySet *es = nullptr;
  if (!split) {
    if (find_best_entry_sets(e, edges)) return;
  }
  if (!es) es = preference;
  set_entry_set(e, es);
  if (!es) e->to->split = split;
  edges.add(e);
}

void flow_var_type_permit(AVar *v, AType *t) {
  if (!v->restrict)
    v->restrict = t;
  else
    v->restrict = type_union(t, v->restrict);
  AType *tt = type_intersection(v->in, v->restrict);
  if (v->restrict_pred != RP_None) tt = apply_restrict_pred(v, tt);
  if (v->num_coerce) tt = type_coerce_numeric_constants(tt, v->num_coerce);
  if (tt != v->out) {
    assert(tt != fa->type_world.top_type);
    v->out = tt;
    propagate_out_change(v);
  }
}

void flow_var_permit_pred(AVar *v, AVarRestrictPred pred, Sym *cls) {
  if (pred == RP_None) return;
  if (v->restrict_pred == RP_None) {
    v->restrict_pred = pred;
    v->restrict_pred_cls = cls;
  } else if (v->restrict_pred != pred || v->restrict_pred_cls != cls) {
    return;  // composition not implemented; bail (survey S1 notes
             // the precision loss for chained predicates)
  }
  AType *tt = v->in;
  if (v->restrict) tt = type_intersection(tt, v->restrict);
  tt = apply_restrict_pred(v, tt);
  if (v->num_coerce) tt = type_coerce_numeric_constants(tt, v->num_coerce);
  if (tt != v->out) {
    assert(tt != fa->type_world.top_type);
    v->out = tt;
    propagate_out_change(v);
  }
}

// static inline void flow_var_type_permit(AVar *v, Sym *s) { flow_var_type_permit(v, make_abstract_type(s)); }

void add_var_constraint(AVar *av, Sym *s) {
  if (!s) s = av->var->sym;
  assert(s->type_kind != Type_VARIABLE);
  s = unalias_type(s);
  if (s->type && !s->is_pattern) {
    if (s->is_external && (s->type->num_kind || s->type == sym_string || s->type->is_system_type))
      update_gen(av, s->type->abstract_type);
    if (s->is_constant)  // for constants, the abstract type is the concrete
                         // type
      update_gen(av, make_abstract_type(s));
    if (s->is_symbol || s->is_fun) update_gen(av, make_abstract_type(s));
    if (s->type_kind != Type_NONE) update_gen(av, make_abstract_type(s->meta_type));
  }
}

AVar *get_element_avar(CreationSet *cs) {
  if (!cs->sym->element) return 0;
  AVar *elem = unique_AVar(cs->sym->element->var, cs);
  cs->added_element_var = 1;
  return elem;
}

void set_container(AVar *av, AVar *container) {
  assert(!av->container || av->container == container);
  av->container = container;
  if (av->lvalue) av->lvalue->container = container;
}

void fill_tvals(Fun *fn, PNode *p, int n) {
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

static void make_kind(PNode *p, EntrySet *es, Sym *kind, AVar *container, Vec<Var *> *vars, Vec<AVar *> *avars,
                      int vstart, int tstart, int l) {
  CreationSet *cs = creation_point(container, kind, l);
  cs->vars.fill(l);
  for (int i = 0; i < l; i++) {
    AVar *av = nullptr;
    if (avars)
      av = avars->v[vstart + i];
    else
      av = make_AVar(vars->v[vstart + i], es);
    Var *tv = p->tvals[tstart + i];
    tv->sym->is_lvalue = av->var->sym->is_lvalue;
    if (!cs->vars[i]) cs->vars[i] = unique_AVar(av->var, cs);
    AVar *iv = cs->vars[i];
    AVar *atv = make_AVar(tv, es);
    set_container(atv, container);
    flow_vars(av, atv);
    flow_vars(atv, iv);
    if (iv->var->sym->name) cs->var_map.put(iv->var->sym->name, iv);
  }
}

void prim_make_constraints(PNode *p, EntrySet *es) {
  AVar *container = make_AVar(p->lvals[0], es);
  Sym *kind = p->rvals[2]->sym;
  int start = 3;
  int l = p->rvals.n - start;
  fill_tvals(es->fun, p, l);
  make_kind(p, es, kind, container, &p->rvals, 0, start, 0, l);
}

static void vector_elems(int rank, PNode *p, AVar *ae, AVar *elem, AVar *container, int n = 0) {
  AVar *e = ae;
  if (!e->contour_is_entry_set) {
    p->tvals.fill(++n);
    assert(container->contour_is_entry_set);
    EntrySet *es = (EntrySet *)container->contour;
    if (p->tvals[n - 1])
      e = make_AVar(p->tvals[n - 1], es);
    else {
      Sym *s = new_Sym();
      s->nesting_depth = es->fun->sym->nesting_depth + 1;
      assert(!e->var->sym->is_lvalue);
      s->in = es->fun->sym;
      Var *v = new Var(s);
      s->var = v;
      p->tvals[n - 1] = v;
      es->fun->fa_all_Vars.add(v);
      e = make_AVar(v, es);
    }
    flow_vars(ae, e);
  }
  set_container(e, container);
  if (rank > 0) {
    for (CreationSet *cs : e->out->sorted) {
      if (cs->sym != sym_tuple)
        flow_vars(e, elem);
      else {
        e->arg_of_send.add(container);
        for (AVar *av : cs->vars) vector_elems(rank - 1, p, av, elem, container, n + 1);
      }
    }
  } else
    flow_vars(e, elem);
}

static void prim_make_vector_constraints(PNode *p, EntrySet *es) {
  int base = p->rvals[0]->sym == sym_primitive ? 4 : 3;
  AVar *container = make_AVar(p->lvals[0], es);
  AVar *vector = make_AVar(p->rvals[base - 2], es);
  AVar *element_type = make_AVar(p->rvals[base - 1], es);
  CreationSet *cs = creation_point(container, vector->var->sym->meta_type);
  AVar *elem = get_element_avar(cs);
  update_gen(elem, element_type->var->sym->meta_type->abstract_type);
  if (p->rvals.n > base) {
    int rank = 0;
    p->rvals[base]->sym->imm_int(&rank);
    for (int i = 0; i < p->rvals.n - (base + 1); i++) {
      Var *v = p->rvals[base + i];
      AVar *av = make_AVar(v, es);
      vector_elems(rank, p, av, elem, container);
    }
  }
}

static void make_closure_var(AVar *av, EntrySet *es, CreationSet *cs, AVar *result, int add, int i) {
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
  else if (i < cs->vars.n && cs->vars[i] != iv)
    // The closure CS persists across analysis passes (it's cached in
    // the result AVar's cs_map), but each pass clears all AVar state
    // and re-derives it. `iv` is keyed by `av->var`, while consumers
    // (partial_application's `fun = cs->vars[0]`, argument unpacking)
    // read the *positional* slots created by whichever pass/path
    // built the CS. If the Var carrying this field's value differs
    // from the one that created vars[i] (e.g. the receiver CS was
    // split between passes, so the method now arrives via a
    // different field Var; or the method-path vs selector-path
    // ordering changed), the flow above lands in an orphan AVar and
    // vars[i] stays bottom -- the closure's call site then sees an
    // empty fun slot, never completes, and remove_unused_closures()
    // strips the closure entirely (issue 030's "void/dead result
    // vars" fixpoint failure). Keep the positional slot fed no
    // matter which Var carries the value this pass.
    flow_var_to_var(cav, cs->vars[i]);
}

static void make_closure_var(Var *v, EntrySet *es, CreationSet *cs, AVar *result, int add, int i) {
  make_closure_var(make_AVar(v, es), es, cs, result, add, i);
}

static void make_closure(AVar *result) {
  assert(result->contour_is_entry_set);
  PNode *pn = result->var->def;
  PNode *partial_application = result->var->def;
  CreationSet *cs = creation_point(result, sym_closure, partial_application->rvals.n);
  int add = !cs->vars.n;
  EntrySet *es = (EntrySet *)result->contour;
  pn->tvals.fill(partial_application->rvals.n);
  for (int i = 0; i < partial_application->rvals.n; i++)
    make_closure_var(partial_application->rvals[i], es, cs, result, add, i);
}

static void make_period_closure(AVar *result, AVar *a, Vec<AVar *> &args) {
  assert(result->contour_is_entry_set);
  PNode *pn = result->var->def;
  PNode *partial_application = result->var->def;
  CreationSet *cs = creation_point(result, sym_closure, partial_application->rvals.n);
  flow_var_type_permit(result, make_AType(cs));
  EntrySet *es = (EntrySet *)result->contour;
  pn->tvals.fill(args.n);
  int add = !cs->vars.n;
  make_closure_var(a, es, cs, result, add, 0);
  for (int i = 0; i < args.n; i++) make_closure_var(args[i], es, cs, result, add, i + 1);
}

// for send nodes, add simple constraints which do not depend
// on the computed types (compare to add_send_edgse_pnodes)
static void add_send_constraints(PNode *p, EntrySet *es) {
  if (p->prim) {
    int start = 1;
    // return constraints
    for (int i = 0; i < p->lvals.n; i++) {
      int ii = i;
      if (p->prim->nrets < 0 || p->prim->nrets <= i) ii = -p->prim->nrets - 1;  // last
      switch (p->prim->ret_types[ii]) {
        case PRIM_TYPE_ANY:
          break;
        case PRIM_TYPE_STRING:
          update_gen(make_AVar(p->lvals[i], es), fa->type_world.string_type);
          break;
        case PRIM_TYPE_SIZE:
          update_gen(make_AVar(p->lvals[i], es), fa->type_world.size_type);
          break;
        case PRIM_TYPE_BOOL:
        case PRIM_TYPE_ANY_NUM_AB:
        case PRIM_TYPE_ANY_NUM_A:
        case PRIM_TYPE_ANY_NUM_B:
        case PRIM_TYPE_ANY_INT_A:
        case PRIM_TYPE_A: {
          for (int j = start; j < p->rvals.n; j++)
            if (j - start != p->prim->pos) {
              AVar *av = make_AVar(p->rvals[j], es), *res = make_AVar(p->lvals.v[0], es);
              av->arg_of_send.add(res);
            }
          break;
        }
        default:
          assert(!"case");
          break;
      }
    }
    // specifics
    switch (p->prim->index) {
      default:
        break;
      case P_prim_reply:
        fill_rets(es, p->rvals.n - 3);
        for (int i = 3; i < p->rvals.n; i++) {
          AVar *r = make_AVar(p->rvals[i], es);
          flow_vars(r, es->rets[i - 3]);
        }
        break;
      case P_prim_make:
        prim_make_constraints(p, es);
        break;
      case P_prim_vector:
        prim_make_vector_constraints(p, es);
        break;
    }
  }
}

static void get_AEdges(Fun *f, PNode *p, EntrySet *from, Vec<AEdge *> &edges) {
  Vec<AEdge *> *ve = from->out_edge_map.get(p);
  if (!ve) from->out_edge_map.put(p, (ve = new Vec<AEdge *>));
  for (AEdge *e : *ve) if (e) {
    if (f == e->fun) edges.add(e);
  }
  // Issue 035: ve is a hash set — when several split-product edges
  // exist for this (pnode, fun), its iteration order follows heap
  // layout, and make_AEdges ENQUEUES in this order, making the
  // whole flow schedule (and AVar id assignment) run-dependent.
  qsort_by_id(edges);
  if (!edges.n) {
    AEdge *e = new_AEdge(f, p, from);
    ve->set_add(e);
    edges.add(e);
  }
}

static void record_arg(PNode *pn, CreationSet *cs, AVar *a, Sym *s, AEdge *e, MPosition &p) {
  MPosition *cp = cannonicalize_mposition(p);
  e->args.put(cp, a);
  AType *t = type_intersection(a->out, e->match->formal_filters.get(cp));
  e->initial_types.put(cp, t->type);
  if (s->is_pattern) {
    for (CreationSet *cs : t->sorted) {
      if (s->has.n != cs->vars.n) {
        // Arity mismatch between a pattern formal and an actual's
        // CS is user-reachable input, not an internal invariant
        // (survey S2) -- report it instead of aborting.
        type_violation(ATypeViolation_kind::MATCH, a, make_AType(cs), nullptr);
        continue;
      }
      p.push(1);
      for (int i = 0; i < s->has.n; i++) {
        record_arg(pn, cs, cs->vars[i], s->has.v[i], e, p);
        p.inc();
      }
      p.pop();
    }
  }
}

static void record_args_rets(AEdge *e, Vec<AVar *> &a) {
  if (!e->args.n) {
    MPosition p;
    p.push(1);
    for (int i = 0; i < e->fun->sym->has.n; i++) {
      record_arg(e->pnode, 0, a[i], e->fun->sym->has.v[i], e, p);
      p.inc();
    }
  }
  if (!e->rets.n) {
    for (int i = 0; i < e->pnode->lvals.n; i++) e->rets.add(make_AVar(e->pnode->lvals[i], e->from));
  }
}

static void make_AEdges(Match *m, PNode *p, EntrySet *from, Vec<AVar *> &args) {
  Vec<AEdge *> edges;
  get_AEdges(m->fun, p, from, edges);
  for (AEdge *e : edges) {
    if (!e->match)
      e->match = m;
    else
      e->match->merge(m);
    record_args_rets(e, args);
    if (!e->in_edge_worklist) {
      e->in_edge_worklist = 1;
      fa->edge_worklist.enqueue(e);
    }
  }
}

// returns 1 if any are partial, 0 if some matched and -1 if none matched
static int all_applications(PNode *p, EntrySet *es, AVar *a0, Vec<AVar *> &args, Vec<cchar *> &names, int is_closure,
                            Partial_kind partial, PNode *visibility_point = nullptr, Vec<CreationSet *> *closures = 0) {
  if (!visibility_point) visibility_point = p;
  int incomplete = -2;
  a0->arg_of_send.add(make_AVar(p->lvals[0], es));
  for (CreationSet *cs : a0->out->sorted) switch (
      application(p, es, a0, cs, args, names, is_closure, partial, visibility_point, closures)) {
    case -1:
      if (incomplete < 0) incomplete = -1;
      break;
    case 0:
      if (incomplete < 0) incomplete = 0;
      break;
    case 1:
      incomplete = 1;
      break;
  }
  return incomplete;
}

static int partial_application(PNode *p, EntrySet *es, CreationSet *cs, Vec<AVar *> &args, Vec<cchar *> &names,
                               Partial_kind partial, PNode *visibility_point, Vec<CreationSet *> *closures) {
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
  for (int i = 0; i < def->code->names.n; i++) n[i] = def->code->names.v[i];
  for (int i = 1; i < names.n; i++) n[def->rvals.n + i - 1] = names.v[i];
  assert(!names.n || !names[0]);
  assert(cs->defs.n == 1);
  Vec<CreationSet *> c;
  if (closures) {
    if (closures->set_in(cs)) {
      type_violation(ATypeViolation_kind::CLOSURE_RECURSION, cs->vars[0], nullptr, result, nullptr);
      return 0;
    }
  } else
    closures = &c;
  closures->set_add(cs);
  int r = all_applications(p, es, fun, a, n, 1, partial, def, closures);
  if (!r) cs->closure_used = 1;
  return r;
}

int function_dispatch(PNode *p, EntrySet *es, AVar *a0, CreationSet *s, Vec<AVar *> &args, Vec<cchar *> &names,
                      int is_closure, Partial_kind partial, PNode *visibility_point) {
  if (!visibility_point) visibility_point = p;
  Vec<AVar *> a;
  int partial_result = 0;
  a.add(a0);
  for (int j = args.n - 1; j >= 0; j--) a.add(args[j]);
  Vec<Match *> matches;
  AVar *send = make_AVar(p->lvals[0], es);
  match_timer.start();
  if (pattern_match(a, names, send, is_closure, partial, visibility_point, matches)) {
    for (Match *m : matches) {
      if (!m->is_partial && partial != Partial_ALWAYS)
        make_AEdges(m, p, es, a);
      else
        partial_result = 1;
    }
  }
  match_timer.stop();
  return matches.n ? partial_result : -1;
}

static int application(PNode *p, EntrySet *es, AVar *a0, CreationSet *cs, Vec<AVar *> &args, Vec<cchar *> &names,
                       int is_closure, Partial_kind partial, PNode *visibility_point, Vec<CreationSet *> *closures) {
  if (sym_closure->implementors.set_in(cs->sym) && cs->defs.n)
    return partial_application(p, es, cs, args, names, partial, visibility_point, closures);
  return function_dispatch(p, es, a0, cs, args, names, is_closure, partial, visibility_point);
}

void type_violation(ATypeViolation_kind akind, AVar *av, AType *type, AVar *send, Vec<Fun *> *funs) {
  // Issue 009 investigation: env-gated one-line trace of every
  // call, suitable for diffing two runs. Emits the analysis pass,
  // the dedup key triple, and the AType hash so two runs can be
  // compared either by sequence or by set of unique triples.
  if (getenv("IFA_DEBUG_VIOLATIONS")) {
    fprintf(stderr, "VIOLATION pass=%d kind=%d av=%d send=%d type=%u\n",
            analysis_pass, (int)akind, av ? av->id : 0,
            send ? send->id : 0, type ? type->hash : 0u);
  }
  ATypeViolation *v = new ATypeViolation(akind, av, send);
  v = fa->type_world.type_violation_hash.put(v);
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
  fa->type_violations.set_add(v);
}

int type_violations_count() { return fa->type_violations.set_count(); }

static int make_rest_tuple(EntrySet *es, PNode *p, AVar *to, Vec<AVar *> &v, int vstart, int tvals) {
  int t = tvals;
  int l = v.n - vstart;
  tvals += l + 1;
  AVar *container = make_AVar(p->tvals[t], es);
  fill_tvals(es->fun, p, tvals);
  make_kind(p, es, sym_tuple, container, 0, &v, vstart, t + 1, l);
  flow_vars(container, to);
  return tvals;
}

static Var **destruct(Var **lvals, int nlvals, AVar *r, Sym *t, AVar *result, int &tvars) {
  Var **lend = lvals + nlvals;
  int nlend = nlvals - t->has.n;
  EntrySet *es = (EntrySet *)result->contour;
  r->arg_of_send.add(result);
  if (t->has.n) {
    for (CreationSet *cs : r->out->sorted) {
      AVar *violation = nullptr;
      int r_tuple = sym_tuple->specializers.set_in(cs->sym->type) != 0;
      int t_tuple = sym_tuple->specializers.set_in(t) != 0;
      if (cs->sym->must_specialize->specializers.set_in(t) || (r_tuple && t_tuple)) {
        for (int i = 0; i < t->has.n; i++) {
          assert(t->has.v[i]->var == lvals[i]);
          AVar *l = make_AVar(lvals[i], es);
          AVar *av = nullptr;
          if (!t_tuple && t->has_name(i))
            av = cs->var_map.get(t->has_name(i));
          else if (t_tuple && i < cs->vars.n) {
            av = cs->vars[i];
            if (t->has[i]->is_rest) {
              assert(i == t->has.n - 1);
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
          if (t->has.n == cs->vars.n + 1 && t->has[t->has.n - 1]->is_rest)
            flow_vars(unique_AVar(sym_empty_tuple->var, GLOBAL_CONTOUR), make_AVar(t->has[t->has.n - 1]->var, es));
          else {
            violation = make_AVar(t->has[cs->vars.n]->var, es);
            goto Lviolation;
          }
        }
      Ldone:;
      } else {
      Lviolation:
        AVar *av = violation ? violation : r;
        if (!av->var->sym->name && t->name) av = r;
        if (!av->var->sym->name && cs->vars.n < t->has.n && t->has[cs->vars.n]->name)
          av = make_AVar(t->has[cs->vars.n]->var, es);
        if (!av->var->sym->name && cs->vars.n > t->has.n && t->has.n && t->has[t->has.n - 1]->name)
          av = make_AVar(t->has[t->has.n - 1]->var, es);
        type_violation(ATypeViolation_kind::MATCH, av, make_AType(cs), result);
      }
    }
  }
  return lend;
}

static bool get_obj_index(AVar *index, int *i, int n) {
  if (index->var->sym->type && index->var->sym->imm_int(i) == 0) {
    *i -= fa->tuple_index_base;
    if (*i >= 0 && *i < n) return true;
  }
  if (index->out->n == 1 && index->out->v[0]->sym->is_constant)
    if (index->out->v[0]->sym->imm_int(i) == 0) {
      *i -= fa->tuple_index_base;
      if (*i >= 0 && *i < n) return true;
    }
  return false;
}

AType *make_size_constant_type(int n) {
  Sym *t = size_constant(n);
  build_type_hierarchy();
  return make_abstract_type(t);
}

AType *make_constant(Immediate &imm, Sym *t) {
  Sym *c = imm_constant(imm, t);
  build_type_hierarchy();
  return make_abstract_type(c);
}

// merge adds the vars in cs but not in new_cs to new_cs.
// mix causes the vars from cs and new_cs to also flow to new_cs->element
static void structural_assignment(CreationSet *new_cs, CreationSet *cs, PNode *p, EntrySet *es, bool merge = false,
                                  bool mix = false) {
  AVar *result = p->lvals.n ? make_AVar(p->lvals[0], es) : 0;
  AVar *elem = get_element_avar(cs);
  int o = elem ? 1 : 0;
  if (elem && new_cs->sym->element) {
    fill_tvals(es->fun, p, 1);
    AVar *tval = make_AVar(p->tvals[0], es);
    flow_vars(elem, tval);
    set_container(tval, result);
    flow_vars(tval, get_element_avar(new_cs));
  }
  if (mix && new_cs->sym->element) {
    fill_tvals(es->fun, p, o + new_cs->vars.n);
    for (int i = 0; i < new_cs->vars.n; i++) {
      AVar *tval = make_AVar(p->tvals[o + i], es);
      flow_vars(new_cs->vars[i], tval);
      set_container(tval, result);
      flow_vars(tval, get_element_avar(new_cs));
    }
    fill_tvals(es->fun, p, o + cs->vars.n);
    for (int i = 0; i < cs->vars.n; i++) {
      AVar *tval = make_AVar(p->tvals[o + i], es);
      flow_vars(cs->vars[i], tval);
      set_container(tval, result);
      flow_vars(tval, get_element_avar(new_cs));
    }
  }
  fill_tvals(es->fun, p, o + cs->sym->has.n);
  for (int i = 0; i < cs->sym->has.n; i++) {
    Sym *h = cs->sym->has[i];
    AVar *iv = unique_AVar(h->var, cs);
    AVar *tval = make_AVar(p->tvals[o + i], es);
    flow_vars(iv, tval);
    set_container(tval, result);
    AVar *niv = unique_AVar(h->var, new_cs);
    flow_vars(tval, niv);
    if (mix) flow_vars(tval, get_element_avar(new_cs));
  }
  for (int i = cs->sym->has.n; i < cs->vars.n; i++) {
    fill_tvals(es->fun, p, o + cs->vars.n);
    AVar *tval = make_AVar(p->tvals[o + i], es);
    flow_vars(cs->vars[i], tval);
    set_container(tval, result);
    if (!merge) {
      new_cs->vars.fill(cs->vars.n);
      if (!new_cs->vars[i]) new_cs->vars[i] = unique_AVar(cs->vars[i]->var, new_cs);
      flow_vars(tval, new_cs->vars[i]);
    } else
      flow_vars(tval, get_element_avar(new_cs));
  }
}

// for send nodes, add call edges and more complex constraints
// which depend on the computed types (compare to add_send_constraints)
static void add_send_edges_pnode(PNode *p, EntrySet *es) {
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
    if (all_applications(p, es, a0, args, p->code->names, 0, (Partial_kind)p->code->partial) > 0) make_closure(result);
  } else {
    // argument and return constraints
    int n = p->prim->nargs < 0 ? -p->prim->nargs : p->prim->nargs;
    AVar *a = nullptr, *b = nullptr;
    int iarg = 0;
    for (int i = 1; i < p->rvals.n; i++) {
      if (i - 1 == p->prim->pos) continue;
      AVar *arg = make_AVar(p->rvals[i], es);
      // record violations
      if (type_diff(arg->out, p->prim->args[iarg]) != fa->type_world.bottom_type)
        type_violation(ATypeViolation_kind::PRIMITIVE_ARGUMENT, arg, type_diff(arg->out, p->prim->args[iarg]),
                       make_AVar(p->lvals[0], es));
      switch (p->prim->arg_types[iarg]) {
        default:
          break;
        case PRIM_TYPE_ANY_NUM_A:
          a = arg;
          break;
        case PRIM_TYPE_ANY_NUM_B:
          b = arg;
          break;
        case PRIM_TYPE_ANY_INT_A:
          a = arg;
          break;
        case PRIM_TYPE_ANY_INT_B:
          b = arg;
          break;
      }
      if (i - 1 < n - 1) iarg++;
    }
    for (int i = 0; i < p->lvals.n; i++) {
      // connect the flows, but prevent values from passing
      // so that splitting can attribute causality
      if ((p->prim->ret_types[i] == PRIM_TYPE_ANY_NUM_AB || p->prim->ret_types[i] == PRIM_TYPE_ANY_NUM_A ||
           p->prim->ret_types[i] == PRIM_TYPE_ANY_INT_A || p->prim->ret_types[i] == PRIM_TYPE_BOOL) &&
          n == 3) {
        AVar *res = make_AVar(p->lvals[i], es);
        fill_tvals(es->fun, p, p->lvals.n);
        AVar *t = make_AVar(p->tvals[i], es);
        flow_var_type_permit(t, fa->type_world.bottom_type);
        flow_vars(a, t);
        flow_vars(b, t);
        flow_vars(t, res);
        // can we fold this?
        if (a->out && b->out && a->out->n && b->out->n) {
          AType *nt = p->prim->ret_types[i] == PRIM_TYPE_BOOL ? fa->type_world.bool_type : type_num_fold(p->prim, a->out, b->out);
          if (a->out->n == 1 && b->out->n == 1 && a->out->v[0]->sym->imm.const_kind &&
              b->out->v[0]->sym->imm.const_kind) {
            Immediate imm;
            if (!fold_constant(p->prim->index, &a->out->v[0]->sym->imm, &b->out->v[0]->sym->imm, &imm))
              update_in(res, make_constant(imm, nt->v[0]->sym));
            else
              update_in(res, nt);
          } else
            update_in(res, nt);
        }
      } else if ((p->prim->ret_types[i] == PRIM_TYPE_ANY_NUM_A || p->prim->ret_types[i] == PRIM_TYPE_ANY_INT_A ||
                  p->prim->ret_types[i] == PRIM_TYPE_BOOL) &&
                 n == 2) {
        AVar *res = make_AVar(p->lvals[i], es);
        fill_tvals(es->fun, p, p->lvals.n);
        AVar *t = make_AVar(p->tvals[i], es);
        flow_var_type_permit(t, fa->type_world.bottom_type);
        flow_vars(a, t);
        flow_vars(t, res);
        if (a->out && a->out->n) {
          AType *nt = p->prim->ret_types[i] == PRIM_TYPE_BOOL ? fa->type_world.bool_type : type_num_fold(p->prim, a->out, a->out);
          if (a->out->n == 1 && a->out->v[0]->sym->imm.const_kind) {
            Immediate imm;
            if (!fold_constant(p->prim->index, &a->out->v[0]->sym->imm, 0, &imm))
              update_in(res, make_constant(imm, nt->v[0]->sym));
            else
              update_in(res, nt);
          } else
            update_in(res, nt);
        }
      }
    }
    AVar *result = p->lvals.n ? make_AVar(p->lvals[0], es) : 0;
    // CONTRACT (survey S2): every snapshot-style transfer below
    // (isinstance, len, merge, index_object, destruct, period,
    // ... -- anything iterating an operand's ->out->sorted at
    // execution time) relies on THIS blanket registration to be
    // re-run when operand types arrive later. It hangs off the
    // result AVar, so a prim SEND without lvals has no resume
    // path -- such prims must not read operand->out in their
    // transfer (today only reply-shaped prims are lval-less).
    if (result)
      for (int i = 0; i < p->rvals.n; i++) make_AVar(p->rvals[i], es)->arg_of_send.add(result);
    int o = p->rvals.v[0]->sym == sym_primitive ? 2 : 1;
    // specifics
    switch (p->prim->index) {
      default:
        break;
      case P_prim_await: {
        AVar *a = make_AVar(p->rvals[o], es);
        flow_vars(a, result);
        break;
      }
      case P_prim_yield: {
        // issues/014: unlike P_prim_await just above (whose result
        // flows from a real, call-graph-visible callee return value),
        // a yield expression's result (`x` in `x = yield foo`) is
        // whatever a *later*, call-graph-invisible `.send(v)` call
        // delivers -- no IF1 edge connects __pyc_generator__.send()'s
        // `value` formal to this primitive; the transfer happens only
        // through the C++ promise's `sent` field at runtime (see
        // pyc_c_runtime.h's yield_awaiter). Flowing the yielded
        // value's own type into the result here (the original,
        // `.send()`-less design, matching P_prim_await's shape) is
        // unsound whenever a generator's yielded expression depends
        // on its own previously-received values (`total += x; yield
        // total`): with FA seeing no other source, the fixed point
        // legitimately-but-uselessly collapses the whole loop to a
        // compile-time constant seeded by the first yield, which
        // then gets constant-folded away entirely -- silently
        // breaking .send() (observed: co_yield of a hardcoded literal
        // instead of the real running value). Anchor to the generic
        // int64 type instead, the same "opaque, non-constant" trick
        // as the coroutine-handle placeholder
        // (_CG_generator_placeholder_return, see gen_fun_pyda) --
        // correct for today's only supported payload shape (v1
        // scope: int64 smuggled through void*, same as the yielded-
        // out value itself). `a` (the yielded value) is already
        // registered as reachable/used by the generic per-rval
        // make_AVar/arg_of_send loop above this switch -- no need to
        // touch it again here, unlike P_prim_await's case, since it
        // does not feed the result's type.
        update_gen(result, sym_int64->abstract_type);
        break;
      }
      case P_prim_id: {
        // id(x): the operand's address (or value bits for unboxed
        // scalars) as a plain int64 -- the result's type never
        // depends on the operand's.
        update_gen(result, sym_int64->abstract_type);
        break;
      }
      case P_prim_primitive: {
        cchar *name = p->rvals[1]->sym->name;
        RegisteredPrim *rp = prim_get(name);
        if (!rp) fail("undefined primitive transfer function '%s'", name);
        rp->tfn(p, es);
        break;
      }
      case P_prim_meta_apply: {
        cchar *file = p->code && p->code->filename() ? p->code->filename() : "<unknown>";
        int line = p->code ? p->code->line() : 0;
        fail("P_prim_meta_apply transfer function not implemented at %s:%d; "
             "no live frontend emits this prim — see ifa/notes/003-cast-and-meta-apply-prims.md",
             file, line);
        break;
      }
      case P_prim_destruct: {
        assert(p->rvals.n - o == 2);
        int tvars = 0;
        destruct(p->lvals.v, p->lvals.n, make_AVar(p->rvals.v[o], es), p->rvals[o + 1]->sym, result, tvars);
        break;
      }
      case P_prim_vector:
        prim_make_vector_constraints(p, es);
        break;
      case P_prim_index_object: {
        AVar *vec = make_AVar(p->rvals[o], es);
        AVar *index = make_AVar(p->rvals[o + 1], es);
        set_container(result, vec);
        for (CreationSet *cs : vec->out->sorted) {
          if (sym_string->specializers.set_in(cs->sym))
            update_gen(result, sym_char->abstract_type);
          else {
            int i;
            bool is_const = get_obj_index(index, &i, cs->vars.n);
            if (cs->sym->element) flow_vars(get_element_avar(cs), result);
            if (!cs->sym->is_vector) {
              if (is_const)
                flow_vars(cs->vars[i], result);
              else
                for (AVar *av : cs->vars) flow_vars(av, result);
            }
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
        for (CreationSet *cs : vec->out->sorted) {
          if (sym_string->specializers.set_in(cs->sym)) {
            AType *d = type_diff(sym_char->abstract_type, val->out);
            if (d != fa->type_world.bottom_type) type_violation(ATypeViolation_kind::MATCH, val, d, result);
          } else {
            int i;
            bool is_const = get_obj_index(index, &i, cs->vars.n);
            if (cs->sym->is_vector) {
              if (cs->sym->element) flow_vars(tval, get_element_avar(cs));
            } else if (is_const)
              flow_vars(tval, cs->vars[i]);
            else {
              if (cs->sym->element) flow_vars(tval, get_element_avar(cs));
              for (int i = 0; i < cs->vars.n; i++) flow_vars(tval, cs->vars[i]);
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
        if (all_applications(p, es, fun, args, names, 0, (Partial_kind)p->code->partial) > 0) make_closure(result);
        break;
      }
      case P_prim_period: {
        AVar *obj = make_AVar(p->rvals[1], es);
        AVar *selector = make_AVar(p->rvals[3], es);
        Vec<AVar *> methods;
        set_container(result, obj);
        bool partial = p->code->partial != Partial_NEVER;
        for (CreationSet *sel : selector->out->sorted) {
          cchar *symbol = sel->sym->name;
          if (!symbol) symbol = sel->sym->constant;
          if (!symbol) symbol = sel->sym->imm.v_string;
          assert(symbol);
          for (CreationSet *cs : obj->out->sorted) {
            AVar *iv = cs->var_map.get(symbol);
            if (iv) {
              iv->arg_of_send.add(result);
              if (partial) {
                // Function-valued fields split two ways, following
                // Python's actual rule -- a function found on the
                // CLASS binds as a method, a function stored as an
                // INSTANCE attribute does not (issue 025
                // first-class-function-in-field):
                //  - METHOD-like values keep the historical behavior
                //    (filtered out of the direct flow, re-routed
                //    through a method-binding partial application):
                //    real methods and capturing-def carriers
                //    (Fun->sym->self set), and class-body lambdas /
                //    defs (Sym::in is a class, i.e. non-fun) -- pyc
                //    stores class attributes as prototype fields, so
                //    definition scope is the FA-visible equivalent of
                //    "found on the class".
                //  - BARE function values (a module- or
                //    function-level def stored in an instance
                //    attribute: fun set, no self, in absent or a
                //    function) flow through UNBOUND --
                //    `self.cf(3, 1)` calls cf(3, 1), not
                //    cf(self, 3, 1). Previously they were bound too,
                //    so any call through such a field dispatched with
                //    the object inserted as the first argument and
                //    matched nothing (timsort's self.comparefn).
                // Mixed fields (both kinds) conservatively flow both
                // forms; permits and bindings only ever grow, so the
                // fixpoint stays monotone.
                AType *fnpart = type_intersection(iv->out, fa->type_world.function_type);
                bool all_bare = fnpart != fa->type_world.bottom_type;
                for (CreationSet *fcs : fnpart->sorted) {
                  Fun *ff = fcs->sym->fun;
                  if (!ff) { all_bare = false; break; }
                  // issue 027 feature: @staticmethod lives in a class
                  // scope like a method but takes NO receiver -- reads
                  // through an instance must flow the raw function
                  // value unbound, overriding the class-scope test.
                  if (ff->sym->is_static_method) continue;
                  if (ff->sym->self || (ff->sym->in && !ff->sym->in->is_fun)) { all_bare = false; break; }
                }
                if (all_bare) {
                  flow_var_type_permit(result, iv->out);
                  flow_vars(iv, result);
                } else {
                  flow_var_type_permit(result, type_diff(iv->out, fa->type_world.function_type));
                  flow_vars(iv, result);
                  if (fnpart != fa->type_world.bottom_type) methods.add(iv);
                }
              } else
                flow_vars(iv, result);
            }
          }
        }
        for (AVar *x : methods) {
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
        for (CreationSet *sel : selector->out->sorted) {
          cchar *symbol = sel->sym->name;
          if (!symbol) symbol = sel->sym->constant;
          if (!symbol) symbol = sel->sym->imm.v_string;
          assert(symbol);
          for (CreationSet *cs : obj->out->sorted) {
            AVar *iv = cs->var_map.get(symbol);
            if (iv)
              flow_vars(tval, iv);
            else
              cs->unknown_vars.add(symbol);
          }
        }
        flow_vars(val, result);
        break;
      }
      case P_prim_assign: {
        AVar *lhs = make_AVar(p->rvals[1], es);
        AVar *rhs = make_AVar(p->rvals[3], es);
        for (CreationSet *cs : lhs->out->sorted) {
          if (cs->sym == sym_ref) {
            assert(cs->vars.n);
            AVar *av = cs->vars[0];
            flow_vars(rhs, av);
            flow_vars(rhs, result);
          } else {
            if (sym_anynum->specializers.set_in(cs->sym->type))
              update_in(result, cs->sym->type->abstract_type);
            else
              type_violation(ATypeViolation_kind::MATCH, lhs, make_AType(cs), result);
          }
        }
        break;
      }
      case P_prim_deref: {
        AVar *ref = make_AVar(p->rvals[2], es);
        set_container(result, ref);
        for (CreationSet *cs : ref->out->sorted) {
          AVar *av = cs->vars[0];
          flow_vars(av, result);
        }
        break;
      }
      case P_prim_new: {
        AVar *thing = make_AVar(p->rvals[p->rvals.n - 1], es);
        for (CreationSet *cs : thing->out->sorted) creation_point(result, cs->sym->meta_type);  // recover original type
        break;
      }
      // NB P_prim_copy result CSs must stay FRESH (creation_point),
      // not shared with the source: an experiment sharing them
      // (update_gen(result, thing->out)) created a within-pass
      // divergence for self-referential deepcopy -- each copy
      // contour's result list unioned back into the SOURCE CS's
      // field, which re-widened the copier's own input and spawned
      // another contour, unboundedly (genetic2's TreeNode). The
      // same-class layout agreement the sharing was after is
      // guaranteed by determine_layouts' canonical field ordering
      // instead (clone.cc).
      case P_prim_copy:
      case P_prim_clone_vector:
      case P_prim_clone: {
        AVar *thing = make_AVar(p->rvals[o], es);
        for (CreationSet *cs : thing->out->sorted) {
          CreationSet *new_cs = creation_point(result, cs->sym);
          structural_assignment(new_cs, cs, p, es);
        }
        break;
      }
      case P_prim_is: {
        // Real identity comparison.  Lattice: if the two
        // operand AVars' CS-sets are disjoint, the result
        // is statically False.  Otherwise it's polymorphic
        // bool — we can't prove True or False at compile
        // time (two AVars sharing a CS might or might not
        // hold the same instance at runtime).
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        bool overlap = false;
        for (CreationSet *cs1 : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs1 == cs2) { overlap = true; break; }
          }
          if (overlap) break;
        }
        AType *rtype = overlap ? fa->type_world.bool_type : fa->type_world.false_type;
        update_gen(result, rtype);
        break;
      }
      case P_prim_isinstance: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);  // instance
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);  // type
        // Give the frontend first refusal: it may recognize this
        // specific check as foldable via language/runtime-specific
        // knowledge FA structurally can't derive on its own (see
        // IFACallbacks::provably_constant_isinstance, ifa.h, for the
        // full rationale and the conservatism contract). Default
        // (nullptr) falls straight through to the normal
        // CreationSet-intersection logic below, unchanged.
        if (AType *forced = if1->callback->provably_constant_isinstance(thing1, es, p)) {
          update_gen(result, forced);
          break;
        }
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs1 : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs2->sym->meta_type && cs2->sym->meta_type->implementors.in(cs1->sym->type))
              rtype = type_union(rtype, fa->type_world.true_type);
            else
              rtype = type_union(rtype, fa->type_world.false_type);
          }
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_issubclass: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs1 : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs2->sym->type->implementors.in(cs1->sym->type))
              rtype = type_union(rtype, fa->type_world.true_type);
            else
              rtype = type_union(rtype, fa->type_world.false_type);
          }
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_merge: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        for (CreationSet *cs : thing1->out->sorted) {
          CreationSet *new_cs = creation_point(result, cs->sym);
          structural_assignment(new_cs, cs, p, es, true);
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs->sym == cs2->sym) structural_assignment(new_cs, cs2, p, es, true);
          }
        }
        break;
      }
      case P_prim_merge_in: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        for (CreationSet *cs : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs->sym == cs2->sym) structural_assignment(cs, cs2, p, es, true, true);
          }
        }
        flow_vars(thing1, result);
        break;
      }
      case P_prim_coerce: {
        Sym *s = unalias_type(p->rvals[p->rvals.n - 2]->sym);
        assert(s->abstract_type);
        AVar *rhs = make_AVar(p->rvals[p->rvals.n - 1], es);
        Vec<CreationSet *> css;
        // Compare against the type operand at its positional slot
        // (n-2), not rvals[1] -- in the @primitive-prefixed form
        // rvals[1] is the prim-name symbol and the filter could
        // never match (survey S5).
        for (CreationSet *cs : rhs->out->sorted) if (cs->sym->type == p->rvals[p->rvals.n - 2]->sym) css.set_add(cs);
        if (css.n)
          update_gen(result, make_AType(css));
        else if (s->type->num_kind || s->type == sym_string || s->type->is_symbol)
          update_gen(result, s->abstract_type);
        break;
      }
      case P_prim_len: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem) elem->arg_of_send.add(result);
          if ((elem && elem->out != fa->type_world.bottom_type) || sym_string->specializers.set_in(cs->sym))
            rtype = type_union(rtype, fa->type_world.size_type);
          else
            rtype = type_union(rtype, make_size_constant_type(cs->vars.n));
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_sizeof: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) {
          if (cs->sym->size)
            rtype = type_union(rtype, make_size_constant_type(cs->sym->size));
          else
            rtype = type_union(rtype, fa->type_world.size_type);
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_sizeof_element: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem) {
            for (CreationSet *cs2 : elem->out->sorted) {
              if (cs2->sym->size)
                rtype = type_union(rtype, make_size_constant_type(cs2->sym->size));
              else
                rtype = type_union(rtype, fa->type_world.size_type);
            }
          }
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_typeof: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) rtype = type_union(rtype, make_abstract_type(cs->sym->meta_type));
        update_gen(result, rtype);
        break;
      }
      case P_prim_typeof_element: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem)
            for (CreationSet *cs2 : elem->out->sorted) rtype = type_union(rtype, make_abstract_type(cs2->sym->meta_type));
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_cast: {
        cchar *file = p->code && p->code->filename() ? p->code->filename() : "<unknown>";
        int line = p->code ? p->code->line() : 0;
        fail("P_prim_cast transfer function not implemented at %s:%d; "
             "no live frontend emits this prim — see ifa/notes/003-cast-and-meta-apply-prims.md",
             file, line);
        break;
      }
    }
  }
}

// Walk back from a Var's def through the wrapper shapes
// pyc emits, returning the originating discriminator PNode.
// Returns v->def if no recognized wrapper is found.
//
// Shapes peeled:
//   - pure MOVE chains
//   - the SEND3 (invocation) → SEND2 (period-bind) → SEND1
//     chain that the frontend emits around every `if cond:`
//     via the __pyc_to_bool__ method dispatch.
//     SEND3 has 1 rval (the bound method); SEND2 has 4
//     rvals: [sym_operator, recv, sym_period, method_sym].
//
// Walk a single-predecessor CFG chain from `from` looking for the
// Code_IF PNode that gates it (issue 059). Bounded the same way
// peel_wrapper_def itself is -- a branch is typically just a label
// (the jump target) then the write itself, but this doesn't assume
// an exact hop count.
static PNode *find_gating_if(PNode *from, int max_depth) {
  PNode *w = from;
  for (int hop = 0; hop < max_depth && w; hop++) {
    if (w->code && w->code->kind == Code_IF && w->rvals.n) return w;
    if (w->cfg_pred.n != 1) return nullptr;
    w = w->cfg_pred[0];
  }
  return nullptr;
}

// Issue 059: does `p` (a PNode with phi children -- i.e. a CFG merge
// point) have a phi entry for `cur` that matches pyc's `guarded_bool`
// helper's exact shape (python_ifa_build_if1.cc)? `guarded_bool`
// collapses an arbitrary discriminator check into a plain boolean by
// merging two branches: the else branch UNCONDITIONALLY writes the
// literal constant `sym_false`; the then branch writes whatever
// `build_then` returned.
//
// Soundness requires BOTH branches to be the literal constants
// `sym_true`/`sym_false`, not just the else branch being `sym_false`.
// If `build_then` returns something else -- a guard's result
// (`case None if cond:`), or a real AND-fold of sub-pattern matches
// (`case Point(x=0, y=0):`, `case [a, b]:`) -- then `result == true`
// still implies the discriminator was true (an AND can only be true
// if every operand, including the discriminator, was true), but
// `result == false` does NOT imply the discriminator was false: it
// could equally mean the discriminator was true but the guard/
// sub-pattern failed. Narrowing the false-branch view in that case
// would be UNSOUND (confirmed empirically: it produced a wrong
// captured value, not just a missed optimization, when guard-gated).
// `combine_bool`'s own short-circuit (`a == sym_true` returns `b`
// unchanged) means a pattern kind with no discriminating sub-pattern
// at all (e.g. every attribute/element itself a bare capture) DOES
// collapse `build_then`'s result to literal `sym_true` naturally --
// so this restriction excludes exactly the unsound cases without
// needing to special-case which pattern kind produced them.
//
// If found, returns the enclosing if1_if's own condition Var -- the
// real discriminator `guarded_bool` wrapped -- so the caller can
// continue peeling into it (composes with nested guarded_bool calls).
static Var *peel_guarded_bool_merge(PNode *p, Var *cur, int max_depth) {
  for (PNode *ph : p->phi) {
    if (ph->lvals.n != 1 || ph->lvals[0] != cur || ph->rvals.n != 2) continue;
    PNode *common_if = nullptr;
    bool saw_true = false, saw_false = false;
    for (Var *branch_val : ph->rvals) {
      PNode *w = branch_val->def;
      if (!w || !w->code || w->code->kind != Code_MOVE || w->rvals.n != 1) return nullptr;
      Sym *src_sym = w->rvals[0]->sym;
      if (src_sym == sym_true) saw_true = true;
      else if (src_sym == sym_false) saw_false = true;
      else return nullptr;
      PNode *gating_if = find_gating_if(w, max_depth);
      if (!gating_if) return nullptr;
      if (!common_if) common_if = gating_if;
      else if (common_if != gating_if) return nullptr;
    }
    return (saw_true && saw_false && common_if) ? common_if->rvals.v[0] : nullptr;
  }
  return nullptr;
}

// Used by issue-025 narrowing recognition to look through
// the wrapper for `is None`, `is not None`, isinstance, etc.
// Depth-bounded as a safety net; see ifa/analysis/NOTES.md.
static PNode *peel_wrapper_def(Var *v, int max_depth = 6) {
  if (!v || !v->def) return v ? v->def : nullptr;
  Var *cur = v;
  PNode *p = v->def;
  for (int hop = 0; hop < max_depth && p && p->code; hop++) {
    bool advanced = false;
    if (p->code->kind == Code_MOVE && p->rvals.n == 1) {
      Var *src = p->rvals[0];
      if (src && src->def && src->def != p) {
        cur = src;
        p = src->def;
        advanced = true;
      }
    }
    if (!advanced && p->code->kind == Code_SEND && p->rvals.n == 1) {
      Var *bound = p->rvals[0];
      if (bound && bound->def && bound->def != p &&
          bound->def->code && bound->def->code->kind == Code_SEND &&
          bound->def->rvals.n == 4) {
        PNode *bind = bound->def;
        Var *recv = bind->rvals[1];
        if (recv && recv->def && recv->def != bind) {
          cur = recv;
          p = recv->def;
          advanced = true;
        }
      }
    }
    // issue 059: peel through a guarded_bool-style boolean collapse
    // -- a phi-merge (at the CFG join point after an if/else) with
    // exactly two sources, one of which unconditionally writes
    // `sym_false`. See peel_guarded_bool_merge's own comment.
    if (!advanced && p->phi.n) {
      Var *discriminator = peel_guarded_bool_merge(p, cur, max_depth);
      if (discriminator && discriminator->def && discriminator->def != p) {
        cur = discriminator;
        p = discriminator->def;
        advanced = true;
      }
    }
    if (!advanced) break;
  }
  return p;
}

static void add_pnode_constraints(PNode *p, EntrySet *es, Vec<PNode *> &done) {
  es->live_pnodes.set_add(p);
  for (PNode *n : p->phi) {
    AVar *vv = make_AVar(n->lvals[0], es);
    for (Var *v : n->rvals) flow_vars(make_AVar(v, es), vv);
  }
  for (Var *v : p->rvals) make_AVar(v, es)->live_arg = 1;
  switch (p->code->kind) {
    default:
      break;
    case Code_SEND:
      add_send_constraints(p, es);
      add_send_edges_pnode(p, es);
      break;
    case Code_MOVE:
      for (int i = 0; i < p->rvals.n; i++) {
        AVar *lhs = make_AVar(p->lvals[i], es), *rhs = make_AVar(p->rvals.v[i], es);
        if (lhs->lvalue && rhs->lvalue)
          flow_vars(rhs, lhs);
        else
          flow_vars_assign(rhs, lhs);
      }
      break;
    case Code_IF: {
      AVar *cond = make_AVar(p->rvals.v[0], es);
      AType *t = cond->out;
      if (t == fa->type_world.bottom_type) return;
      AType *b = type_intersection(t, fa->type_world.bool_type);
      AType *e = type_diff(t, b);
      if (e != fa->type_world.bottom_type) type_violation(ATypeViolation_kind::PRIMITIVE_ARGUMENT, cond, e, nullptr);
      // Note: previously this case `break`'d out (skipping
      // the per-branch blocks below) when `b == bool_type`
      // — i.e. cond was a polymorphic bool that could be
      // either True or False.  In that case the post-switch
      // default code merged both branches' phy lvals from
      // a single rval AVar, losing the per-branch SSU
      // distinction.  Issue 025 keeps the per-branch blocks
      // running for polymorphic conds so each branch's
      // SSU-renamed AVar gets its own narrowed-type flow,
      // enabling discriminator-based narrowing (isinstance,
      // is None) to take effect.

      // Issue 025: per-branch type narrowing.  When the
      // condition is the result of a recognized
      // discriminator primitive (today: prim_isinstance),
      // narrow the operand's per-branch SSU-renamed Var
      // (which the phy node below already creates as
      // lvals[0]=True, lvals[1]=False) by restricting it
      // to the matching/non-matching CreationSets.  The
      // restrict propagates via `out = in ∩ restrict`
      // (see update_in), so the narrowing flows through to
      // downstream uses in each branch without affecting
      // the other.
      // Issue 025: detect narrowing predicates that wrote
      // cond, and apply per-branch type filters to the SSU
      // per-branch Vars (which always exist — see the
      // ifa/tests/ir/ssu/14_isinstance_narrow.ir fixture).
      //
      // Two cases handled:
      //  (a) direct prim_isinstance call (rare — Python
      //      `isinstance` is a wrapper function),
      //  (b) call to the isinstance wrapper (recognized by
      //      callee sym name == "isinstance" with two args).
      //
      // Status: the narrowing successfully targets the
      // per-branch SSU AVars (v_v1 / v_v2), but pyc's
      // strict no-boxing default emits BOXING violations
      // on the ORIGINAL Var (v) BEFORE these narrowed views
      // get a chance to gate downstream uses.  See issue
      // 025 for the deeper design constraint and follow-on
      // work needed (liveness-aware BOXING, or
      // SSU-rewrite-and-prune).
      Var *narrow_operand = nullptr;
      AType *narrow_true_type = nullptr;
      AType *narrow_false_type = nullptr;
      // Issue 026 Bug 5 fix: when narrowing can be
      // expressed as a type-level predicate (`is None`,
      // `is not None`, isinstance against a single class),
      // record it here so the lv view re-evaluates as new
      // CSs arrive at v->in.  Otherwise we fall back to
      // the snapshot-AType path (narrow_*_type above).
      AVarRestrictPred narrow_true_pred = RP_None;
      AVarRestrictPred narrow_false_pred = RP_None;
      Sym *narrow_pred_cls = nullptr;
      // Peel pyc's `if cond:` wrapper to find the
      // discriminator PNode.  Frontend lowers `if cond:` as:
      //   SEND1: t = cond_op(...)              ; the discriminator
      //   SEND2: m = operator cond_op . __pyc_to_bool__
      //   SEND3: bool_cond = m()
      //   IF bool_cond
      // peel_wrapper_def follows that chain (plus MOVE chains).
      //
      // The whole discriminator-recognition + narrowing
      // setup below is gated on `ifa_narrow` so we can
      // measure FA precision with/without narrowing in
      // isolation.  When disabled, narrow_operand stays
      // nullptr and the apply sites below fall through to
      // the unnarrowed flow_vars.
      PNode *iso_def = ifa_narrow ? peel_wrapper_def(p->rvals.v[0]) : nullptr;
      Var *operand_var = nullptr;
      Var *type_var = nullptr;
      bool is_none_check = false;   // narrowing target: nil_type
      bool is_not_none_check = false;
      if (iso_def) {
        if (iso_def->prim &&
            iso_def->prim->index == P_prim_isinstance) {
          int n = iso_def->rvals.n;
          if (n >= 2) {
            operand_var = iso_def->rvals[n - 2];
            type_var = iso_def->rvals[n - 1];
          }
        } else if (iso_def->code &&
                   iso_def->code->kind == Code_SEND &&
                   iso_def->rvals.n >= 3) {
          // SEND layout:
          //   rvals[0] = function ref
          //   rvals[1..] = args
          // Recognized patterns:
          //   - Python `isinstance(obj, ci)` wrapper.
          //   - `x is None` / `x is not None` via the
          //     __is__ / __nis__ method dispatch (issue 004
          //     partial fix).  For these, one of the two
          //     operands is the None constant.
          Var *fn_var = iso_def->rvals[0];
          cchar *fname = (fn_var && fn_var->sym && fn_var->sym->name)
                             ? fn_var->sym->name : nullptr;
          if (fname && !strcmp(fname, "isinstance") &&
              iso_def->rvals.n >= 3) {
            operand_var = iso_def->rvals[1];
            type_var = iso_def->rvals[2];
          } else if (fname &&
                     (!strcmp(fname, "__is__") ||
                      !strcmp(fname, "__nis__")) &&
                     iso_def->rvals.n >= 3) {
            // rvals[1] = self, rvals[2] = x.  Narrow whichever
            // operand isn't the None constant.  If both or
            // neither, skip.
            Var *self_v = iso_def->rvals[1];
            Var *x_v = iso_def->rvals[2];
            AVar *self_av = make_AVar(self_v, es);
            AVar *x_av = make_AVar(x_v, es);
            bool self_is_none = false, x_is_none = false;
            for (CreationSet *cs : self_av->out->sorted)
              if (cs->sym->type == sym_nil_type) { self_is_none = true; break; }
            for (CreationSet *cs : x_av->out->sorted)
              if (cs->sym->type == sym_nil_type) { x_is_none = true; break; }
            // Find the non-None operand for narrowing.
            if (self_is_none && !x_is_none) {
              operand_var = x_v;
            } else if (x_is_none && !self_is_none) {
              operand_var = self_v;
            }
            if (operand_var) {
              if (!strcmp(fname, "__is__")) is_none_check = true;
              else is_not_none_check = true;
            }
          }
        }
      }
      if (operand_var) {
        AVar *operand_av = make_AVar(operand_var, es);
        AType *tt = fa->type_world.bottom_type;
        AType *ft = fa->type_world.bottom_type;
        if (type_var) {
          // isinstance path: True if operand's type implements
          // type_var's instance-class.
          AVar *type_av = make_AVar(type_var, es);
          // If type_av resolves to a single class, install a
          // predicate so late-arriving operand CSs get
          // re-classified.  (Issue 026 Bug 5.)
          if (type_av->out && type_av->out->sorted.n == 1) {
            CreationSet *cs2 = type_av->out->sorted[0];
            if (cs2 && cs2->sym && cs2->sym->meta_type) {
              narrow_pred_cls = cs2->sym;
              narrow_true_pred = RP_IsInstanceOf;
              narrow_false_pred = RP_NotInstanceOf;
            }
          }
          for (CreationSet *cs1 : operand_av->out->sorted) {
            bool matches = false;
            for (CreationSet *cs2 : type_av->out->sorted) {
              if (cs2->sym->meta_type &&
                  cs2->sym->meta_type->implementors.in(cs1->sym->type)) {
                matches = true;
                break;
              }
            }
            if (matches) tt = type_union(tt, make_AType(cs1));
            else ft = type_union(ft, make_AType(cs1));
          }
        } else if (is_none_check || is_not_none_check) {
          // `x is None` / `x is not None`: True for None CSes,
          // False for everything else.  For __nis__, swap the
          // True / False partitions.  Both forms are
          // expressible as type-level predicates, so install
          // them — late-arriving CSs at operand_av->in get
          // classified correctly without re-running this
          // constraint setup.
          narrow_true_pred  = is_none_check ? RP_IsNilType    : RP_IsNotNilType;
          narrow_false_pred = is_none_check ? RP_IsNotNilType : RP_IsNilType;
          for (CreationSet *cs1 : operand_av->out->sorted) {
            bool is_none = (cs1->sym->type == sym_nil_type);
            if (is_none_check ? is_none : !is_none)
              tt = type_union(tt, make_AType(cs1));
            else
              ft = type_union(ft, make_AType(cs1));
          }
        }
        if (tt != fa->type_world.bottom_type ||
            ft != fa->type_world.bottom_type ||
            narrow_true_pred != RP_None || narrow_false_pred != RP_None) {
          narrow_operand = operand_var;
          narrow_true_type = tt;
          narrow_false_type = ft;
        }
      }

      if (type_intersection(b, fa->type_world.true_type) != fa->type_world.bottom_type) {
        for (PNode *n : p->phy) {
          AVar *vv = make_AVar(n->rvals[0], es);
          AVar *lv = make_AVar(n->lvals.v[0], es);
          flow_vars(vv, lv);
          if (narrow_operand && n->rvals[0] == narrow_operand) {
            if (narrow_true_pred != RP_None)
              flow_var_permit_pred(lv, narrow_true_pred, narrow_pred_cls);
            else if (narrow_true_type && narrow_true_type != fa->type_world.bottom_type)
              flow_var_type_permit(lv, narrow_true_type);
          }
        }
        PNode *n = p->cfg_succ[0];
        if (done.set_add(n)) add_pnode_constraints(n, es, done);
      }
      if (type_intersection(b, fa->type_world.false_type) != fa->type_world.bottom_type) {
        for (PNode *n : p->phy) {
          AVar *vv = make_AVar(n->rvals[0], es);
          AVar *lv = make_AVar(n->lvals.v[1], es);
          flow_vars(vv, lv);
          if (narrow_operand && n->rvals[0] == narrow_operand) {
            if (narrow_false_pred != RP_None)
              flow_var_permit_pred(lv, narrow_false_pred, narrow_pred_cls);
            else if (narrow_false_type && narrow_false_type != fa->type_world.bottom_type)
              flow_var_type_permit(lv, narrow_false_type);
          }
        }
        PNode *n = p->cfg_succ[1];
        if (done.set_add(n)) add_pnode_constraints(n, es, done);
      }
      return;
    }
  }
  for (PNode *n : p->phy) {
    AVar *vv = make_AVar(n->rvals[0], es);
    for (Var *v : n->lvals) flow_vars(vv, make_AVar(v, es));
  }
  for (PNode *n : p->cfg_succ) if (done.set_add(n)) add_pnode_constraints(n, es, done);
}

static void add_es_constraints(EntrySet *es) {
  for (Var *v : es->fun->fa_Vars) add_var_constraint(make_AVar(v, es));
  Vec<PNode *> done;
  add_pnode_constraints(es->fun->entry, es, done);
}

static inline bool is_fa_Var(Var *v) {
  return v->sym->type || v->sym->aspect || v->sym->is_constant || v->sym->is_symbol;
}

static void collect_Vars_PNodes(Fun *f) {
  f->fa_collected = 1;
  if (!f->entry) return;
  f->collect_Vars(f->fa_all_Vars, &f->fa_all_PNodes);
  qsort_by_id(f->fa_all_Vars);
  qsort_by_id(f->fa_all_PNodes);
  for (Var *v : f->fa_all_Vars) if (is_fa_Var(v)) f->fa_Vars.add(v);
  Primitives *prim = if1->primitives;
  for (PNode *p : f->fa_all_PNodes) {
    if (p->code->kind == Code_MOVE) f->fa_move_PNodes.add(p);
    if (p->code->kind == Code_IF) f->fa_if_PNodes.add(p);
    f->fa_phi_PNodes.append(p->phi);
    f->fa_phy_PNodes.append(p->phy);
    if (p->code->kind == Code_SEND) {
      p->prim = prim->find(p);
      f->fa_send_PNodes.add(p);
    }
  }
  for (Var *v : f->fa_all_Vars) if (v->sym->clone_for_constants) f->clone_for_constants = 1;
}

static AVar *get_filtered(AEdge *e, MPosition *p, AVar *av) {
  AVar *filtered = e->filtered_args.get(p);
  if (!filtered) {
    Var *filtered_v = new Var(av->var->sym);
    filtered_v->is_internal = 1;
    filtered_v->is_filtered = 1;
    e->filtered_args.put(p, (filtered = unique_AVar(filtered_v, e->to)));
  }
  return filtered;
}

// Issue 035: canonical order for an edge's positional arg
// positions. form_MPositionAVar walks the args Map in bucket
// order, which follows the interned MPositions' ADDRESSES (ASLR),
// and analyze_edge's arg loop CREATES the formal/filtered AVars —
// so bucket order set the AVar id-assignment order and made every
// downstream qsort_by_id canonicalization run-dependent (issue 035:
// FA trajectories, clone sets, and generated C varied between
// identical runs). Positional position paths are int-encoded
// (int2Position), so ordering by path is layout-independent.
static int compar_mposition_path(const void *a, const void *b) {
  MPosition *x = *(MPosition **)a, *y = *(MPosition **)b;
  int n = x->pos.n < y->pos.n ? x->pos.n : y->pos.n;
  for (int i = 0; i < n; i++) {
    uintptr_t xi = (uintptr_t)x->pos[i], yi = (uintptr_t)y->pos[i];
    if (xi != yi) return xi < yi ? -1 : 1;
  }
  return x->pos.n - y->pos.n;
}

static void positional_arg_positions_in_order(AEdge *e, Vec<MPosition *> &out) {
  form_MPositionAVar(x, e->args) if (x->key->is_positional()) out.add(x->key);
  if (out.n > 1) qsort(out.v, out.n, sizeof(out[0]), compar_mposition_path);
}

static void analyze_edge(AEdge *e_arg) {
  Vec<AEdge *> edges;
  make_entry_set(e_arg, edges);
  qsort_by_id(edges);
  for (AEdge *ee : edges) {
    int regular_rets = ee->pnode->lvals.n;
    Vec<MPosition *> arg_positions;
    positional_arg_positions_in_order(ee, arg_positions);
    // verify filters
    for (MPosition *p : arg_positions) {
      AVar *actual = ee->args.get(p);
      AType *filter = ee->match->formal_filters.get(p);
      AType *es_filter = ee->to->filters.get(p);
      if (filter) {
        if (es_filter) filter = type_intersection(filter, es_filter);
      } else
        filter = es_filter;
      if (filter && type_intersection(actual->out, filter) == fa->type_world.bottom_type) goto LskipEdge;
    }
    if (ee->from) ee->from->out_edges.set_add(ee);
    for (MPosition *p : arg_positions) {
      AVar *actual = ee->args.get(p), *formal = make_AVar(ee->to->fun->args.get(p), ee->to),
           *filtered = get_filtered(ee, p, formal);
      AType *edge_filter = ee->match->formal_filters.get(p);
      if (!edge_filter) continue;
      AType *es_filter = ee->to->filters.get(p);
      AType *filter = es_filter ? type_intersection(edge_filter, es_filter) : edge_filter;
      flow_var_type_permit(filtered, filter);
      for (CreationSet *cs : filter->sorted) cs->ess.set_add(ee->to);
      flow_vars(actual, filtered);
      flow_vars(filtered, formal);
      if (p->pos.n > 1)
        set_container(filtered, get_filtered(ee, p->up, ee->to->args.get(p->up)));
      else if (!actual->contour_is_entry_set && actual->contour != GLOBAL_CONTOUR)  // closure
        set_container(filtered, make_AVar(ee->pnode->rvals[0], ee->from));
    }
    if (ee->match->fun->sym->cont) creation_point(make_AVar(ee->match->fun->sym->cont->var, ee->to), sym_continuation);
    for (int i = 0; i < ee->pnode->lvals.n; i++) flow_vars(ee->to->rets[i], ee->rets.v[i]);
    fill_rets(ee->to, regular_rets + ee->match->fun->out_positions.n);
    for (int o = 0; o < ee->match->fun->out_positions.n; o++) {
      MPosition *p = ee->match->fun->out_positions[o];
      p = p ? p : ee->match->fun->out_positions[o];
      AVar *actual = ee->args.get(p);
      flow_vars(ee->to->rets[o + regular_rets], actual);
    }
    if (!fa->entry_set_done.set_in(ee->to)) {
      fa->entry_set_done.set_add(ee->to);
      if (!ee->match->fun->fa_collected) collect_Vars_PNodes(ee->match->fun);
      for (PNode *p : ee->match->fun->fa_if_PNodes) make_AVar(p->rvals[0], ee->to)->is_if_arg = 1;
      add_es_constraints(ee->to);
    }
  LskipEdge:;
  }
}

static void refresh_top_edge(AEdge *e) {
  MPosition p, *cp;
  p.push(1);
  cp = cannonicalize_mposition(p);
  e->match->formal_filters.put(cp, fa->type_world.any_type);
  AVar *av = make_AVar(sym___main__->var, e->to);
  e->args.put(cp, av);
  e->filtered_args.put(cp, av);
  update_gen(av, av->var->sym->abstract_type);
}

static AEdge *make_top_edge(Fun *top) {
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

static bool is_return_value(AVar *av) {
  EntrySet *es = (EntrySet *)av->contour;
  for (AVar *v : es->rets) if (v == av) return true;
  return false;
}

static void show_sym_name(Sym *s, FILE *fp) {
  if (s->name)
    fprintf(fp, "%s", s->name);
  else if (s->constant)
    fprintf(fp, "\"%s\"", s->constant);
  else if (s->is_constant) {
    fputs("\"", fp);
    fprint_imm(fp, s->imm);
    fputs("\"", fp);
  } else
    fprintf(fp, "%d", s->id);
}

static void show_type(Vec<CreationSet *> &t, FILE *fp, int verbose = ifa_verbose) {
  if (verbose < 3) {
    Vec<Sym *> type;
    for (CreationSet *cs : t) if (cs) {
      Sym *s = cs->sym;
      if (!ifa_verbose) s = s->type;
      type.set_add(s);
    }
    type.set_to_vec();
    qsort_by_id(type);
    if (type.n > 1) fprintf(fp, "( ");
    for (Sym *s : type) if (s) {
      show_sym_name(s, fp);
      fprintf(fp, " ");
    }
    if (type.n > 1) fprintf(fp, ") ");
  } else {
    fprintf(fp, "( ");
    for (CreationSet *cs : t) if (cs) {
      show_sym_name(cs->sym, fp);
      fprintf(fp, " ");
      if (cs->vars.n) fprintf(fp, "[ ");
      for (AVar *av : cs->vars) {
        show_sym_name(av->var->sym, fp);
        fprintf(fp, ":");
        show_type(*av->out, fp, verbose - 1);
        fprintf(fp, " ");
      }
      if (cs->added_element_var && get_element_avar(cs)->out) {
        fprintf(fp, " *elements*:");
        show_type(*get_element_avar(cs)->out, fp, verbose - 1);
      }
      if (cs->vars.n) fprintf(fp, " ] ");
    }
    fprintf(fp, ") ");
  }
}

static void show_sym(Sym *s, FILE *fp) {
  if (s->is_pattern) {
    fprintf(fp, "( ");
    for (Sym *ss : s->has) {
      if (ss != s->has[0]) fprintf(fp, ", ");
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
  else if (s->must_implement && s->must_implement == s->must_specialize) {
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

static void show_fun(Fun *f, FILE *fp) {
  if (f->line() > 0) fprintf(fp, "%s:%d: ", f->filename(), f->source_line());
  for (Sym *s : f->sym->has) {
    show_sym(s, fp);
    if (s != f->sym->has[f->sym->has.n - 1]) fprintf(fp, ", ");
  }
  if (ifa_verbose) fprintf(fp, " id:%d", f->sym->id);
}

static void show_atype(AType &t, FILE *fp, int level) {
  fprintf(fp, "( ");
  for (CreationSet *cs : t.sorted) if (cs) {
    show_sym_name(cs->sym, fp);
    fprintf(fp, " id:%d ", cs->id);
    if (level > 0) {
      for (AVar *av : cs->vars) {
        show_sym_name(av->var->sym, fp);
        show_atype(*av->out, fp, level - 1);
      }
    }
  }
  fprintf(fp, ") ");
}

void fa_print_backward(AVar *v, FILE *fp = 0) {
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
    int verbose = ifa_verbose < 3 ? 0 : ifa_verbose - 3;
    show_atype(*v->out, fp, verbose);
    fprintf(fp, "\n");
    for (AVar *vv : v->backward) if (vv) {
      if (!done.set_in(vv)) {
        todo.add(vv);
        done.set_add(vv);
      }
    }
  }
}

void fa_dump_var_types(AVar *av, FILE *fp, int verbose = ifa_verbose) {
  Var *v = av->var;
  if (verbose < 2 && (!v->sym->name || v->sym->is_symbol)) return;
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

void fa_dump_types(FA *fa, FILE *fp) {
  Vec<Var *> gvars;
  for (EntrySet *es : fa->ess) {
    Fun *f = es->fun;
    if (f->sym->name)
      fprintf(fp, "function %s (%d) ", f->sym->name, f->sym->id);
    else
      fprintf(fp, "function %d ", f->sym->id);
    fprintf(fp, "entry set with %d edges\n", es->edges.count());
    Vec<Var *> vars;
    f->collect_Vars(vars);
    for (Var *v : vars) {
      if (!v->sym->nesting_depth) {
        gvars.set_add(v);
        continue;
      }
      fa_dump_var_types(make_AVar(v, es), fp);
    }
  }
  gvars.set_to_vec();
  fprintf(fp, "globals\n");
  for (Var *v : gvars) if (!v->sym->is_constant && !v->sym->is_symbol)
      fa_dump_var_types(unique_AVar(v, GLOBAL_CONTOUR), fp);
}

static void show_name(FILE *fp, AVar *av) {
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

static void show_illegal_type(FILE *fp, ATypeViolation *v) {
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

static int compar_edge_id(const void *aa, const void *bb) {
  AEdge *a = (*(AEdge **)aa);
  AEdge *b = (*(AEdge **)bb);
  // Reporting-only sort: compare by stable IR sym ids, NOT via
  // make_AVar — creating AVars here both mutates analysis state
  // from a print path and walks es->display for the caller pnode's
  // Var, which reads out of bounds (null es) when the caller is
  // more deeply nested than the callee contour's display (pygasus
  // aborted while printing its violations; pylife hit the same on
  // the issue-033 stage-C branch, where this fix first landed).
  int i = 0, j = 0;
  if (a->pnode && a->pnode->lvals.n) i = a->pnode->lvals[0]->sym->id;
  if (b->pnode && b->pnode->lvals.n) j = b->pnode->lvals[0]->sym->id;
  if (i != j) return (i > j) ? 1 : -1;
  i = a->from ? a->from->id : 0;
  j = b->from ? b->from->id : 0;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

static void show_call_tree(FILE *fp, PNode *p, EntrySet *es, int depth = 0) {
  depth++;
  if (depth > fa->print_call_depth || !p->code) return;
  if (depth > 1 && p->code->filename() && p->code->line() > 0) {
    for (int x = 0; x < depth; x++) fprintf(fp, " ");
    fprintf(fp, "called from %s:%d", p->code->filename(), p->code->line());
    if (ifa_verbose && p->lvals.n) fprintf(fp, " send:%d", p->lvals[0]->sym->id);
    fprintf(fp, "\n");
  }
  // `es` may be the distinguished global contour (fa->global_es),
  // whose edges vec is always empty — the loop below no-ops.
  Vec<AEdge *> edges;
  for (AEdge *e : es->edges) if (e) edges.add(e);
  qsort(edges.v, edges.n, sizeof(edges[0]), compar_edge_id);
  for (AEdge *e : edges) show_call_tree(fp, e->pnode, e->from, depth);
}

void show_avar_call_tree(FILE *fp, AVar *av) {
  EntrySet *es = (EntrySet *)av->contour;
  Vec<AEdge *> edges;
  for (AEdge *e : es->edges) if (e) edges.add(e);
  qsort(edges.v, edges.n, sizeof(edges[0]), compar_edge_id);
  for (AEdge *e : edges) show_call_tree(fp, e->pnode, e->from, 1);
}

static void show_candidates(FILE *fp, PNode *pn, Sym *arg0) {
  Vec<Fun *> *pfuns = pn->code->ast->visible_functions(arg0);
  if (!pfuns) return;
  Vec<Fun *> funs(*pfuns);
  funs.set_to_vec();
  qsort_by_id(funs);
  fprintf(fp, "note: candidates are:\n");
  for (Fun *f : funs) {
    show_fun(f, fp);
    fprintf(fp, "\n");
  }
}

static int compar_tv(const void *aa, const void *bb) {
  int i, j, x;
  ATypeViolation *a = (*(ATypeViolation **)aa);
  ATypeViolation *b = (*(ATypeViolation **)bb);
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
  if (x) return x;
Lskip:
  if (a->kind < b->kind) return -1;
  if (b->kind < a->kind) return 1;
  if (a->av && b->av) {
    if (a->av->var && b->av->var) {
      if (a->av->var->sym && b->av->var->sym) {
        i = a->av->var->sym->id;
        j = b->av->var->sym->id;
        x = (i > j) ? 1 : ((i < j) ? -1 : 0);
        if (x) return x;
      }
      i = a->av->var->id;
      j = b->av->var->id;
      x = (i > j) ? 1 : ((i < j) ? -1 : 0);
      if (x) return x;
    }
    i = a->av->id;
    j = b->av->id;
    x = (i > j) ? 1 : ((i < j) ? -1 : 0);
    if (x) return x;
  }
  if (a->send && b->send) {
    i = a->send->id;
    j = b->send->id;
    x = (i > j) ? 1 : ((i < j) ? -1 : 0);
    if (x) return x;
  }
  return 0;
}

static void show_violations(FA *fa, FILE *fp) {
  Vec<ATypeViolation *> vv;
  for (ATypeViolation *v : fa->type_violations) if (v) vv.add(v);
  qsort(vv.v, vv.n, sizeof(vv[0]), compar_tv);
  for (ATypeViolation *v : vv) if (v) {
    if (v->send && v->send->var->def->code->source_line() > 0)
      fprintf(fp, "%s:%d: ", v->send->var->def->code->filename(), v->send->var->def->code->source_line());
    else if (v->av->var->sym->ast && v->av->var->sym->source_line() > 0)
      fprintf(fp, "%s:%d: ", v->av->var->sym->filename(), v->av->var->sym->source_line());
    else if (!v->av->contour_is_entry_set && v->av->contour != GLOBAL_CONTOUR) {
      CreationSet *cs = (CreationSet *)v->av->contour;
      fprintf(fp, "%s:%d: class %s:: ", cs->sym->filename(), cs->sym->source_line(), cs->sym->name);
    } else {
      if (fruntime_errors)
        fprintf(fp, "warning: ");
      else
        fprintf(fp, "error: ");
    }
    switch (v->kind) {
      default:
        assert(0);
      case ATypeViolation_kind::PRIMITIVE_ARGUMENT:
        fprintf(fp, "illegal primitive argument type ");
        show_illegal_type(fp, v);
        break;
      case ATypeViolation_kind::SEND_ARGUMENT:
        if (v->av->var->sym->is_symbol && v->send->var->def->rvals[0] == v->av->var) {
          fprintf(fp, "unresolved call '%s'", v->av->var->sym->name);
          if (ifa_verbose) fprintf(fp, " send:%d", v->send->var->sym->id);
          fprintf(fp, "\n");
          show_candidates(fp, v->send->var->def, v->av->var->sym);
        } else {
          fprintf(fp, "illegal call argument type ");
          show_illegal_type(fp, v);
        }
        break;
      case ATypeViolation_kind::DISPATCH_AMBIGUITY:
        fprintf(fp, "%s: ambiguous call '%s'", fruntime_errors ? "warning" : "error", v->av->var->sym->name);
        if (ifa_verbose) fprintf(fp, " send:%d", v->send->var->sym->id);
        fprintf(fp, "\n");
        fprintf(fp, "note: candidates are:\n");
        for (Fun *f : *v->funs) if (f) {
          show_fun(f, fp);
          fprintf(fp, "\n");
        }
        break;
      case ATypeViolation_kind::MEMBER:
        if (v->av->out->n == 1)
          fprintf(fp, "unresolved member '%s'", v->av->out->v[0]->sym->name);
        else {
          fprintf(fp, "unresolved member\n");
          for (CreationSet *selector : v->av->out->sorted) fprintf(fp, "  selector '%s'\n", selector->sym->name);
        }
        if (v->type->n == 1)
          fprintf(fp, "  class '%s'\n", v->type->v[0]->sym->name ? v->type->v[0]->sym->name : "<anonymous>");
        else {
          fprintf(fp, "  classes\n");
          for (CreationSet *cs : v->type->sorted) fprintf(fp, "  class '%s'\n", cs->sym->name);
        }
        break;
      case ATypeViolation_kind::MATCH:
        if (v->av->var->sym->name)
          fprintf(fp, "near '%s' unmatched type: ", v->av->var->sym->name);
        else
          fprintf(fp, "unmatched type: ");
        show_type(*v->type, fp);
        fprintf(fp, "\n");
        break;
      case ATypeViolation_kind::NOTYPE:
        show_name(fp, v->av);
        fprintf(fp, "has no type\n");
        break;
      case ATypeViolation_kind::BOXING:
        show_name(fp, v->av);
        fprintf(fp, "has mixed basic types:");
        show_type(*v->type, fp);
        fprintf(fp, "\n");
        break;
      case ATypeViolation_kind::CLOSURE_RECURSION:
        show_name(fp, v->av);
        fprintf(fp, "is recursive closure\n");
        break;
    }
    if (v->send)
      show_call_tree(fp, v->send->var->def, (EntrySet *)v->send->contour);
    else if (v->av->contour_is_entry_set)
      show_avar_call_tree(fp, v->av);
    else if (v->av->contour != GLOBAL_CONTOUR)
      show_call_tree(fp, ((CreationSet *)v->av->contour)->defs.first()->var->def,
                     (EntrySet *)((CreationSet *)v->av->contour)->defs.first()->contour, 1);
  }
}

static cchar *fn(cchar *s) {
  if (!s) return "<none>";
  cchar *filename = strrchr(s, '/');
  if (filename) return filename + 1;
  return s;
}

void log_var_types(Var *v, Fun *f) {
  if (!v->sym->name || v->sym->is_symbol || v->is_internal) return;
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
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key) {
      AVar *av = v->avars[i].value;
      // this test doesn't take into account nested variables
      if (!f || f->ess.set_in(((EntrySet *)av->contour))) css.set_union(*av->out);
    }
  log(LOG_TEST_FA, "( ");
  Vec<Sym *> syms;
  for (CreationSet *cs : css) if (cs) syms.set_add(cs->sym->type);
  syms.set_to_vec();
  qsort_by_id(syms);
  for (Sym *s : syms) {
    if (s->name)
      log(LOG_TEST_FA, "%s ", s->name);
    else if (s->constant)
      log(LOG_TEST_FA, "\"%s\" ", s->constant);
    else if (s->is_constant) {
      char c[128];
      sprint_imm(c, sizeof(c), s->imm);
      log(LOG_TEST_FA, "\"%s\" ", c);
    }
    if (s->source_line()) log(LOG_TEST_FA, "(%s:%d) ", fn(s->filename()), s->source_line());
  }
  log(LOG_TEST_FA, ")\n");
}

static void collect_results() {
  // collect funs, ess and ess_set
  fa->funs.clear();
  fa->ess.clear();
  for (EntrySet *es : fa->entry_set_done) if (es) {
    fa->funs.set_add(es->fun);
    fa->ess.add(es);
  }
  fa->funs.set_to_vec();
  qsort_by_id(fa->funs);
  // Issue 033 D7: unlike fa->css (sorted a few lines below), fa->ess
  // was left in fa->entry_set_done's WORKLIST-COMPLETION order. No
  // current consumer is order-sensitive to it (each either
  // canonicalizes its own output or performs an order-independent
  // per-(ES,Var) test), so this wasn't a live bug -- but it was a
  // foot-gun: any future direct consumer that assumed sorted order,
  // or ran a greedy/first-match pass over fa->ess, would silently
  // reintroduce this whole class of nondeterminism. Sort once, here,
  // rather than re-auditing every future call site.
  qsort_by_id(fa->ess);
  fa->ess_set.move(fa->entry_set_done);
  // collect css and css_set
  fa->css.clear();
  fa->css_set.clear();
  for (EntrySet *es : fa->ess) {
    for (Var *v : es->fun->fa_all_Vars) {
      AVar *xav = make_AVar(v, es);
      for (AVar *av = xav; av; av = av->lvalue) fa->css_set.set_union(*av->out);
    }
  }
  for (CreationSet *cs : fa->css_set) if (cs) fa->css.add(cs);
  qsort_by_id(fa->css);
  // print results
  if (ifa_verbose) fa_dump_types(fa, stdout);
  if (fgraph_pass_contours) {
    char fn[2048];
    strcpy(fn, fa->fn);
    snprintf(fn + strlen(fn), sizeof(fn) - strlen(fn), ".%d", analysis_pass);
    graph_contours(fa, fn);
  }
}

static bool empty_type_minus_partial_applications(AType *a) {
  for (CreationSet *aa : *a) if (aa) {
    if (aa->sym == sym_closure && aa->defs.n) continue;
    if (aa->sym->is_unique_type) continue;
    return false;
  }
  return true;
}

static AType *type_minus_partial_applications(AType *a) {
  AType *r = new AType();
  for (CreationSet *aa : *a) if (aa) {
    if (aa->sym == sym_closure && aa->defs.n) continue;
    r->set_add(aa);
  }
  r = type_cannonicalize(r);
  return r;
}

// for each call site, check that all args are covered
static void collect_argument_type_violations() {
  for (Fun *f : fa->funs) {
    for (PNode *p : f->fa_send_PNodes) {
      if (p->prim) continue;  // primitives handled elsewhere
      Vec<EntrySet *> ess;
      f->ess.set_intersection(fa->ess_set, ess);
      for (EntrySet *from : ess) if (from) {
        if (!from->live_pnodes.set_in(p)) continue;
        Vec<AEdge *> *m = from->out_edge_map.get(p);
        if (!m) {
          if (p->code->partial == Partial_NEVER) {
            for (Var *v : p->rvals) {
              AVar *av = make_AVar(v, from);
              type_violation(ATypeViolation_kind::SEND_ARGUMENT, av, av->out, make_AVar(p->lvals[0], from));
            }
          }
        } else {
          Vec<AVar *> actuals;
          for (AEdge *me : *m) {
            if (!from->out_edges.set_in(me)) continue;
            form_MPositionAVar(x, me->args) if (x->key->is_positional()) actuals.set_add(x->value);
          }
          for (AVar *av : actuals) if (av) {
            AType *t = av->out;
            for (AEdge *e : *m) {
              if (!from->out_edges.set_in(e)) continue;
              form_MPositionAVar(x, e->args) {
                if (x->value != av) continue;
                if (!x->key->is_positional()) continue;
                MPosition *p = x->key;
                AVar *filtered = e->filtered_args.get(p);
                if (filtered) {
                  t = type_diff(t, filtered->out);
                }
              }
            }
            if (!empty_type_minus_partial_applications(t)) {
              t = type_minus_partial_applications(t);
              type_violation(ATypeViolation_kind::SEND_ARGUMENT, av, t, make_AVar(p->lvals[0], from));
            }
          }
        }
      }
    }
  }
}

static bool mixed_basics(AVar *av) {
  Vec<Sym *> basics;
  for (CreationSet *cs : *av->out) if (cs) {
    Sym *b = to_basic_type(cs->sym->type);
    if (b) basics.set_add(b);
  }
  return basics.n > 1;
}

static bool is_only_used_by_phy_or_phi(Var *v) {
  if (!v) return false;
  if (!v->uses.n) return true;
  for (PNode *p : v->uses) {
    if (p->code) {
      if (p->code->kind == Code_SEND && p->prim && 
          (p->prim->index == P_prim_isinstance || p->prim->index == P_prim_is)) {
        continue;
      }
      return false;
    }
  }
  return true;
}

static void collect_var_type_violations() {
  // collect NOTYPE violations
  for (EntrySet *es : fa->ess) {
    for (Var *v : es->fun->fa_all_Vars) {
      AVar *av = make_AVar(v, es);
      if (av->live_arg && !av->var->sym->is_fake && !av->var->is_internal && av->out == fa->type_world.bottom_type &&
          !is_Sym_OUT(av->var->sym)) {
        // ifa/issues/040: dump the receiver's CreationSet(s) for each
        // NOTYPE violation -- added while tracing an empty-list
        // literal (e.g. `k = []`) failing to type-check ONLY when a
        // non-empty list of some other concrete element type also
        // exists in the program. `arg[N] out.sorted.n` shows whether
        // this ES's formal is genuinely monomorphic (n==1, one
        // CreationSet) or still a union at violation-collection time
        // -- the empty-list case turned out to be the former (its own
        // dedicated ES, not shared with the non-empty list's), which
        // ruled out CreationSet-equivalence merging
        // (`clone.cc:determine_basic_clones`'s `cs1->vars.n !=
        // cs2->vars.n` check already keeps them apart) as the cause.
        if (getenv("PYC_DBG_NOTYPE")) {
          fprintf(stderr, "NOTYPE: var=%s fun=%s(%p) es=%p live=%d uses.n=%d\n",
                  v->sym->name ? v->sym->name : "?", es->fun->sym && es->fun->sym->name ? es->fun->sym->name : "?",
                  (void *)es->fun, (void *)es, v->live, v->uses.n);
          for (int argi = 0; argi < es->args.n; argi++) {
            if (!es->args.v[argi].key) continue;
            AVar *aav = es->args.v[argi].value;
            fprintf(stderr, "  arg[%d] var=%s out.n=%d out.sorted.n=%d\n", argi,
                    aav && aav->var && aav->var->sym->name ? aav->var->sym->name : "?", aav && aav->out ? aav->out->n : -1,
                    aav && aav->out ? aav->out->sorted.n : -1);
            if (aav && aav->out) {
              for (CreationSet *cs : aav->out->sorted) {
                fprintf(stderr, "    cs sym=%s vars.n=%d\n", cs && cs->sym && cs->sym->name ? cs->sym->name : "?",
                        cs ? cs->vars.n : -1);
              }
            }
          }
        }
        type_violation(ATypeViolation_kind::NOTYPE, av, av->out, nullptr, nullptr);
      }
    }
  }
  if (!fa->permit_boxing) {
    // collect BOXING violations
    for (EntrySet *es : fa->ess) {
      for (Var *v : es->fun->fa_all_Vars) {
        AVar *av = make_AVar(v, es);
        if (!is_only_used_by_phy_or_phi(av->var) && mixed_basics(av))
          type_violation(ATypeViolation_kind::BOXING, av, av->out, nullptr, nullptr);
      }
    }
    for (CreationSet *cs : fa->css) {
      for (AVar *av : cs->vars) {
        if (!av->var || !is_only_used_by_phy_or_phi(av->var)) {
          if (mixed_basics(av)) type_violation(ATypeViolation_kind::BOXING, av, av->out, nullptr, nullptr);
        }
      }
    }
  }
  if (fa->no_unused_instance_variables) {
    for (CreationSet *cs : fa->css) {
      for (AVar *av : cs->vars) {
        if (av->live_arg && av->out == fa->type_world.bottom_type) type_violation(ATypeViolation_kind::NOTYPE, av, av->out, nullptr, nullptr);
      }
    }
  }
}

static void convert_NOTYPE_to_void() {
  if (!fa->css_set.set_in(fa->type_world.void_type->v[0])) {
    fa->css_set.set_add(fa->type_world.void_type->v[0]);
    fa->css.add(fa->type_world.void_type->v[0]);
  }
  for (EntrySet *es : fa->ess) {
    for (Var *v : es->fun->fa_all_Vars) {
      AVar *av = make_AVar(v, es);
      if (!av->var->is_internal && av->out == fa->type_world.bottom_type && !is_Sym_OUT(av->var->sym)) av->out = fa->type_world.void_type;
    }
  }
  if (fa->no_unused_instance_variables) {
    for (CreationSet *cs : fa->css) {
      for (AVar *av : cs->vars) {
        if (av->out == fa->type_world.bottom_type) av->out = fa->type_world.void_type;
      }
    }
  }
}

void initialize_Sym_for_fa(Sym *s) {
  if (s->is_symbol || s->is_fun || s->type_kind) s->abstract_type = make_abstract_type(s);
  if (s->is_fun || s->is_pattern || s->type_kind) for (Sym *ss : s->has) if (!ss->var) ss->var = new Var(ss);
  if (s->type_kind && s->element) s->element->var = new Var(s->element);
}

static void initialize_symbols() { for (Sym *s : fa->pdb->if1->allsyms) initialize_Sym_for_fa(s); }

static void initialize_primitives() {
  for (Prim *p : fa->pdb->if1->primitives->prims) {
    p->args.clear();
    int n = p->nargs < 0 ? -p->nargs : p->nargs;
    for (int i = 0; i < n - 1; i++) {
      switch (p->arg_types[i]) {
        case PRIM_TYPE_ALL:
          p->args.add(fa->type_world.top_type);
          break;
        case PRIM_TYPE_ANY:
          p->args.add(fa->type_world.any_type);
          break;
        case PRIM_TYPE_SYMBOL:
          p->args.add(fa->type_world.symbol_type);
          break;
        case PRIM_TYPE_STRING:
          p->args.add(fa->type_world.string_type);
          break;
        case PRIM_TYPE_SIZE:
          p->args.add(fa->type_world.size_type);
          break;
        case PRIM_TYPE_TUPLE:
          p->args.add(fa->type_world.tuple_type);
          break;
        case PRIM_TYPE_CONT:
          p->args.add(make_abstract_type(sym_continuation));
          break;
        case PRIM_TYPE_REF:
          p->args.add(make_abstract_type(sym_ref));
          break;
        case PRIM_TYPE_ANY_NUM_A:
          p->args.add(fa->type_world.anynum_kind);
          break;
        case PRIM_TYPE_ANY_NUM_B:
          p->args.add(fa->type_world.anynum_kind);
          break;
        case PRIM_TYPE_ANY_INT_A:
          p->args.add(fa->type_world.anyint_type);
          break;
        case PRIM_TYPE_ANY_INT_B:
          p->args.add(fa->type_world.anyint_type);
          break;
        default:
          assert(!"case");
          break;
      }
    }
  }
}

static void initialize_global(Sym *s) {
  if (!s->var) s->var = new Var(s);
  add_var_constraint(make_AVar(s->var, (EntrySet *)GLOBAL_CONTOUR));
}

static void initialize() {
  if1->callback->finalize_functions();
  fa->type_world.bottom_type = type_cannonicalize(new AType());
  fa->type_world.bottom_type->type = fa->type_world.bottom_type;
  fa->type_world.void_type = make_abstract_type(sym_void_type);
  fa->type_world.any_type = make_abstract_type(sym_any);
  fa->type_world.top_type = type_union(fa->type_world.any_type, fa->type_world.void_type);
  fa->type_world.bool_type = make_abstract_type(sym_bool);
  Immediate imm;
  imm.v_bool = 1;
  fa->type_world.true_type = make_abstract_type(if1_const(if1, sym_bool, "true", &imm));
  imm.v_bool = 0;
  fa->type_world.false_type = make_abstract_type(if1_const(if1, sym_bool, "false", &imm));
  fa->type_world.size_type = make_abstract_type(sym_size);
  fa->type_world.symbol_type = make_abstract_type(sym_symbol);
  fa->type_world.string_type = make_abstract_type(sym_string);
  fa->type_world.anyint_type = make_abstract_type(sym_anyint);
  fa->type_world.function_type = make_abstract_type(sym_function);
  fa->type_world.anynum_kind = make_abstract_type(sym_anynum);
  fa->type_world.anytype_type = make_abstract_type(sym_anytype);
  fa->type_world.nil_type = make_abstract_type(sym_nil_type);
  fa->type_world.unknown_type = make_abstract_type(sym_unknown_type);
  fa->type_world.tuple_type = make_abstract_type(sym_tuple);
  initialize_global(sym_nil);
  initialize_global(sym_empty_list);
  initialize_global(sym_empty_tuple);
  initialize_global(sym_unknown);
  initialize_global(sym_void);
  fa->edge_worklist.clear();
  fa->send_worklist.clear();
  initialize_symbols();
  initialize_primitives();
  build_arg_positions(fa);
  build_patterns(fa);
}

static void initialize_pass() {
  pass_timer.restart();
  fa->type_violations.clear();
  fa->type_world.type_violation_hash.clear();
  fa->entry_set_done.clear();
  fa->dup_split_attempts = 0;  // issue 033 stage A per-pass counter
  fa->cs_dup_split_attempts = 0;  // issue 033 D5 per-pass counter
  fa->dirty_avar_count = 0;    // issue 033 M4 probe
  fa->examined_avar_count = 0;  // issue 033 M4 probe
  refresh_top_edge(fa->top_edge);
}

static void mark_es_backedges(EntrySet *es, Accum<EntrySet *> &ess) {
  ess.add(es);
  es->dfs_color = DFS_grey;
  for (AEdge *e : es->out_edges) if (e) {
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

static void compute_recursive_entry_sets() {
  Accum<EntrySet *> ess;
  mark_es_backedges(fa->top_edge->to, ess);
  for (EntrySet *es : ess.asvec) es->dfs_color = DFS_white;
}

static void mark_es_cs_backedges(CreationSet *cs, Accum<EntrySet *> &ess, Accum<CreationSet *> &css);
static void mark_es_cs_backedges(EntrySet *es, Accum<EntrySet *> &ess, Accum<CreationSet *> &css);

static void mark_es_cs_backedges(CreationSet *cs, Accum<EntrySet *> &ess, Accum<CreationSet *> &css) {
  css.add(cs);
  cs->dfs_color = DFS_grey;
  for (EntrySet *es : cs->ess) if (es) {
    if (es->dfs_color == DFS_white)
      mark_es_cs_backedges(es, ess, css);
    else if (es->dfs_color == DFS_grey)
      es->cs_backedges.add(cs);
  }
  cs->dfs_color = DFS_black;
}

static void mark_es_cs_backedges(EntrySet *es, Accum<EntrySet *> &ess, Accum<CreationSet *> &css) {
  ess.add(es);
  es->dfs_color = DFS_grey;
  for (AEdge *e : es->out_edges) if (e) {
    EntrySet *es_succ = e->to;
    if (es_succ->dfs_color == DFS_white)
      mark_es_cs_backedges(es_succ, ess, css);
    else if (es_succ->dfs_color == DFS_grey) {
      e->es_cs_backedge = 1;
      es_succ->es_cs_backedges.add(e);
    }
  }
  for (CreationSet *cs : es->creates) if (cs) {
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
static void compute_recursive_entry_creation_sets() {
  Accum<EntrySet *> ess;
  Accum<CreationSet *> css;
  mark_es_cs_backedges(fa->top_edge->to, ess, css);
  for (EntrySet *es : ess.asvec) es->dfs_color = DFS_white;
  for (CreationSet *cs : css.asvec) cs->dfs_color = DFS_white;
}

int is_es_recursive(EntrySet *es) {
  if (es->split) return es->split->backedges.n;
  return es->backedges.n;
}

static int is_es_recursive(AEdge *e) {
  EntrySet *es = e->from->split ? e->from->split : e->from;
  for (AEdge *ee : es->backedges) if (ee->pnode == e->pnode && ee->fun == e->fun) return 1;
  return 0;
}

int is_es_cs_recursive(EntrySet *es) {
  if (es->split) return es->split->es_cs_backedges.n;
  return es->es_cs_backedges.n;
}

static int is_es_cs_recursive(AEdge *e) {
  EntrySet *es = e->from->split ? e->from->split : e->from;
  for (AEdge *ee : es->es_cs_backedges) if (ee->pnode == e->pnode && ee->fun == e->fun) return 1;
  return 0;
}

int is_es_cs_recursive(CreationSet *cs) {
  if (cs->split) return cs->split->es_backedges.n;
  return cs->es_backedges.n;
}

#define SPLIT_TYPE 0
#define SPLIT_SETTER 1

#define SPLIT_VALUE 0
#define SPLIT_MARK 1

#define SPLIT_EDGES 0
#define SPLIT_DYNAMIC 1

// Issue 033 (stage A): which extend_analysis stage is currently
// driving splits (an FAPassStage value). Set by extend_analysis
// before each split_* stage; forms part of the split-ledger key.
static int cur_split_stage = -1;

SplitDecision *FA::ledger_find(Fun *afun, int stage, MPosition *pos, AType *partition, uint sig) {
  SplitDecision probe;
  probe.fun = afun;
  probe.stage = stage;
  probe.pos = pos;
  probe.partition = partition;
  probe.sig = sig;
  return split_ledger.get(&probe);
}

SplitDecision *FA::ledger_add(Fun *afun, int stage, MPosition *pos, AType *partition, EntrySet *product, uint sig) {
  SplitDecision *d = new SplitDecision;
  d->fun = afun;
  d->stage = stage;
  d->pos = pos;
  d->partition = partition;
  d->sig = sig;
  d->pass_made = analysis_pass;
  d->product = product;
  SplitDecision *existing = split_ledger.put(d);
  return existing ? existing : d;
}

SplitDecision *FA::ledger_find_cs(uint sig) { return ledger_find(nullptr, 0, nullptr, nullptr, sig); }

SplitDecision *FA::ledger_add_cs(uint sig, CreationSet *product) {
  SplitDecision *d = new SplitDecision;
  d->sig = sig;
  d->pass_made = analysis_pass;
  d->cs_product = product;
  SplitDecision *existing = split_ledger.put(d);
  return existing ? existing : d;
}

static void collect_type_confluence(AVar *av, Vec<AVar *> &confluences) {
  for (AVar *x : av->backward) if (x) {
    if (!x->out->type->n) continue;
    if (av->var->sym->clone_for_constants) {
      if (type_diff(av->in, x->out) != fa->type_world.bottom_type) {
        confluences.set_add(av);
        break;
      }
    } else {
      if (x->out->type->n && type_diff(av->in->type, x->out->type) != fa->type_world.bottom_type) {
        confluences.set_add(av);
        break;
      }
    }
  }
}

static void collect_type_confluences(Vec<AVar *> &confluences) {
  confluences.clear();
  for (EntrySet *es : fa->ess) {
    for (Var *v : es->fun->fa_all_Vars) {
      AVar *xav = make_AVar(v, es);
      for (AVar *av = xav; av; av = av->lvalue) {
        ++fa->examined_avar_count;  // issue 033 M4 probe
        collect_type_confluence(av, confluences);
      }
    }
  }
  for (CreationSet *cs : fa->css) {
    for (AVar *av : cs->vars) {
      ++fa->examined_avar_count;  // issue 033 M4 probe
      if (!av->contour_is_entry_set && av->contour != GLOBAL_CONTOUR) collect_type_confluence(av, confluences);
    }
    if (cs->added_element_var) collect_type_confluence(get_element_avar(cs), confluences);
  }
  confluences.set_to_vec();
  qsort_by_id(confluences);
  for (AVar *x : confluences) {
      cchar *contour_tag = x->contour_is_entry_set ? "ES" : "CS";
      cchar *role_tag = x->is_lvalue ? "lval" : (x->var->is_formal ? "formal" : "other");
      log(LOG_SPLITTING, "[confluence] av %d %s [%s/%s] ", x->id,
          x->var->sym->name ? x->var->sym->name : "(anon)", contour_tag, role_tag);
      for (CreationSet *cs : x->in->sorted) {
        if (cs->sym)
           log(LOG_SPLITTING, "%s ", cs->sym->name ? cs->sym->name : "");
        else
            log(LOG_SPLITTING, "(%d) ", cs->id);
      }
     log(LOG_SPLITTING, "\n");
  }
}

static void collect_es_marked_confluences(Vec<AVar *> &confluences, Accum<AVar *> &acc, int fsetters) {
  confluences.clear();
  for (AVar *xav : acc.asvec) {
    for (AVar *av = xav; av; av = av->lvalue) {
      Vec<AVar *> &dir = fsetters ? av->forward : av->backward;
      for (AVar *x : dir) if (x && x->mark_map) {
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

// Issue 035: canonical order for pending-map iteration. The map
// buckets by RAW pointers (PendingMapHash over fun/pnode/from), so
// form_Map order follows heap layout — and record_backedges CREATES
// AEdges in that order, making edge ids (the key every qsort_by_id
// canonicalization sorts on) run-dependent.
static int compar_pending_key(const void *a, const void *b) {
  AEdge *x = (*(MapElemAEdgeEntrySets **)a)->key, *y = (*(MapElemAEdgeEntrySets **)b)->key;
  int i = x->fun ? x->fun->id : 0, j = y->fun ? y->fun->id : 0;
  if (i != j) return i < j ? -1 : 1;
  i = x->pnode ? x->pnode->id : 0, j = y->pnode ? y->pnode->id : 0;
  if (i != j) return i < j ? -1 : 1;
  i = x->from ? x->from->id : 0, j = y->from ? y->from->id : 0;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

static void record_backedges(AEdge *e, EntrySet *es, PendingAEdgeEntrySetsMap &up_map) {
  Vec<MapElemAEdgeEntrySets *> elems;
  form_Map(MapElemAEdgeEntrySets, m, up_map) elems.add(m);
  if (elems.n > 1) qsort(elems.v, elems.n, sizeof(elems[0]), compar_pending_key);
  for (MapElemAEdgeEntrySets *m : elems) {
    if (m->key->from == es)
      map_set_add(e->to->pending_es_backedge_map, new_AEdge(m->key->fun, m->key->pnode, e->to), m->value);
    else
      map_set_add(e->to->pending_es_backedge_map, m->key, m->value);
  }
  Vec<AEdge *> *backedges = &es->backedges;
  if (es->split) backedges = &es->split->backedges;
  for (AEdge *ee : *backedges) {
    if (ee->from == es)
      map_set_add(e->to->pending_es_backedge_map, new_AEdge(ee->fun, ee->pnode, e->to), e->to);
    else
      map_set_add(e->to->pending_es_backedge_map, e, e->to);
  }
}

static EntrySet *find_or_make_filtered_entry_set(EntrySet *orig_es, Map<MPosition *, AType *> &filters) {
  Fun *f = orig_es->fun;
  EntrySet *res = nullptr;
  for (EntrySet *es : f->ess) if (!es->filters.some_disjunction(filters)) {
    res = es;
    break;
  }
  if (!res) {
    res = new EntrySet(f);
    f->ess.add(res);
    res->filters.copy(filters);
    res->split = orig_es;
  }
  // Issue 033 stage A (record-only): ledger each filter entry that
  // narrows orig_es. A hit means an earlier pass already split this
  // fun at this position for this partition — the splitter is
  // redoing work on a re-derived flow state.
  form_MPositionAType(x, filters) {
    if (!x->key || !x->value) continue;
    if (orig_es->filters.get(x->key) == x->value) continue;
    SplitDecision *d = fa->ledger_find(f, cur_split_stage, x->key, x->value);
    if (!d)
      fa->ledger_add(f, cur_split_stage, x->key, x->value, res);
    else if (d->pass_made != analysis_pass) {  // intra-pass repeats aren't re-derivation
      ++fa->dup_split_attempts;
      log(LOG_SPLITTING, "[ledger] DUP filtered fun %s %d stage %d pos %p part %p/%d (first pass %d, product %d)\n",
          f->sym->name ? f->sym->name : "", f->sym->id, cur_split_stage, (void *)x->key, (void *)x->value,
          x->value->sorted.n, d->pass_made, d->product ? d->product->id : -1);
    }
  }
  return res;
}

[[nodiscard]] static int split_edges(AVar *av, int fsetters, int fmark) {
  int again = 0;
  EntrySet *es = (EntrySet *)av->contour;
  Vec<AEdge *> all_edges;
  for (AEdge *ee : es->edges) if (ee) all_edges.add(ee);
  qsort_by_id(all_edges);
  MPosition *p = nullptr;
  form_MPositionAVar(x, es->args) {
    if (x->value == av) {
      p = x->key;
      break;
    }
  }
  assert(p);
  Map<CreationSet *, EntrySet *> cs_es_map;
  for (CreationSet *cs : av->out->type->sorted) {
    Map<MPosition *, AType *> filters;
    filters.copy(es->filters);
    filters.put(p, make_AType(cs));
    EntrySet *tes = find_or_make_filtered_entry_set(es, filters);
    cs_es_map.put(cs, tes);
  }
  // Re-pointing an edge at a different ES must go through the full
  // re-entry recipe apply_entry_set_split uses (null `to`, clear the
  // stale per-edge filtered_args whose AVars are contoured on the
  // OLD to, remove the edge from the old ES's edge set, then
  // set_entry_set) — NOT a bare `ee->to = tes` assignment. The
  // find_or_make_filtered_entry_set products routed into here are
  // BARE EntrySets (filters + split lineage only; no display, args,
  // or rets — set_entry_set is the only thing that populates those),
  // and analyze_edge's make_entry_set early-returns on a non-null
  // e->to, so nothing downstream ever repairs one. A direct
  // assignment therefore ships analyze_edge an ES whose empty
  // display/rets it indexes blindly: make_AVar(formal, es) reads
  // es->display[depth-1] out of bounds and derefs the garbage as a
  // contour (the pystone/tictactoe/amaze/othello/score4/voronoi2
  // SIGSEGV family, pyc issue 025), and guarding just that moves the
  // crash to the rets[i] flow below it.
  auto redispatch = [](AEdge *ee, EntrySet *tes) {
    if (!tes || ee->to == tes) return;
    if (ee->to) ee->to->edges.del(ee);
    ee->to = 0;
    ee->filtered_args.clear();
    set_entry_set(ee, tes);
  };
  for (AEdge *ee : all_edges) if (ee) {
    AVar *earg = es->args.get(p);
    EntrySet *old = ee->to;
    // Probe with the constant-stripped type view throughout:
    // cs_es_map is keyed by av->out->type CSs, but a raw
    // single-element out can be a CONSTANT CS ("3" rather than
    // int64), whose map lookup misses and used to null ee->to
    // (survey B5). An empty type view (e.g. pure-nil out) leaves
    // the edge untouched, as before.
    AType *ety = earg->out->type;
    if (ety->sorted.n == 1)
      redispatch(ee, cs_es_map.get(ety->sorted.v[0]));
    else {
      for (int i = 0; i < ety->sorted.n; i++) {
        CreationSet *cs = ety->sorted[i];
        if (!i)
          redispatch(ee, cs_es_map.get(cs));
        else
          ee = copy_AEdge(ee, cs_es_map.get(cs));
      }
    }
    if (ee->to != old) {
      again = 1;
      log(LOG_SPLITTING, "DISPATCH ES %d:%d, %s %d -> %d\n", ee->from->id, ee->pnode->lvals[0]->sym->id,
          es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, old->id, ee->to->id);
    }
  }
  return again;
}

// Issue 033 stage C: nested-function contours are additionally
// keyed by their lexical display, which the (position, partition)
// filters key does not capture. Before routing a group into a
// product ES, verify the whole group implies one display and that
// it matches what the product already has — the exact invariant
// update_display asserts (entries the product lacks are extended
// from the first routed edge, so only existing entries constrain).
// Issue 033 stage C: the group's full type signature — the union
// (constant-stripped) of the group's argument types at EVERY
// positional arg, plus each ret's lvalue type — hashed over
// canonical AType pointers. Type-value group compatibility is
// type-equality per position and per ret, so this is exactly what
// identifies "the same grouping decision" across passes; keying on
// one position alone merged distinct groups (int/float results
// were mistyped in builtins_batch).
// Returns 0 when the group has NO STABLE IDENTITY — callers must
// then neither route nor record. Two soundness rules, both learned
// from builtins_batch (three __str__ call sites' groups from
// passes 0/1/2 funneled into one product that pass 3 then had to
// split apart, do=2/3 — the int/float sum poisoning):
//  - mirror edge_type_compatible_with_edge EXACTLY: it compares
//    per-edge FILTER-INTERSECTED types, so the key must too, or
//    groups the predicate distinguishes (same raw types, different
//    match filters) collide;
//  - the predicate treats an EMPTY intersected type as compatible
//    with anything (a wildcard). A wildcard cannot be represented
//    in a snapshot key — an edge whose types haven't arrived yet
//    would match any recorded partition — so such groups are
//    unroutable this pass.
static uint group_signature(Vec<AEdge *> &these_edges, Fun *f) {
  uint h = 0;
  int i = 0;
  for (MPosition *p : f->positional_arg_positions) {
    AType *t = fa->type_world.bottom_type;
    for (AEdge *x : these_edges) {
      AVar *a = x->args.get(p);
      if (!a) continue;
      AType *flt = x->match->formal_filters.get(p);
      AType *et = flt ? type_intersection(a->out->type, flt) : a->out->type;
      if (!et->n) return 0;  // wildcard: no identity yet
      t = type_union(t, et);
    }
    h += (uint)(uintptr_t)t * open_hash_primes[i++ % 256];
  }
  int nrets = these_edges[0]->rets.n;
  for (int r = 0; r < nrets; r++) {
    AType *t = fa->type_world.bottom_type;
    for (AEdge *x : these_edges)
      if (r < x->rets.n && x->rets[r]->lvalue) {
        AType *rt = x->rets[r]->lvalue->out->type;
        if (!rt->n) return 0;  // wildcard: no identity yet
        t = type_union(t, rt);
      }
    h += (uint)(uintptr_t)t * open_hash_primes[i++ % 256];
  }
  return h ? h : 1;  // 0 is reserved for "no identity" / filtered-path keys
}

static bool group_display_ok(Vec<AEdge *> &these_edges, EntrySet *product, Fun *f) {
  int nd = f->sym->nesting_depth;
  if (!nd) return true;
  Vec<EntrySet *> disp;
  if (product) disp.copy(product->display);
  AEdge *e0 = these_edges[0];
  for (int i = disp.n; i < nd; i++) disp.add(i < e0->from->display.n ? e0->from->display[i] : e0->from);
  for (int i = 0; i < nd; i++) if (!disp[i]) return false;
  for (AEdge *x : these_edges)
    for (int i = 0; i < nd; i++) {
      EntrySet *want = i < x->from->display.n ? x->from->display.v[i] : x->from;
      if (disp[i] != want) return false;
    }
  return true;
}

// Issue 033 M2b: decide-then-apply. The grouping DECISION (which
// edges of an EntrySet partition away from it, and into which
// groups) is computed by decide_entry_set_split against an
// unmutated graph and carried in this record; the graph MUTATION
// (detaching the groups, parking or ledger-routing them) happens
// in apply_entry_set_split. For the legacy callers (stages 2-5,
// which still decide-and-apply per AVar in sequence) the two run
// back-to-back inside split_entry_set, byte-equivalent to the old
// interleaved shape; stage 1 (split_ess_for_type, non-dynamic)
// decides ALL its confluences against the same converged snapshot
// before applying any of them, which removes the intra-stage
// order dependence (the 009/021 family) structurally instead of
// by qsort suppression.
struct ESSplitDecision : public gc {
  AVar *av = nullptr;
  EntrySet *es = nullptr;
  MPosition *avpos = nullptr;
  int fsetters = 0, fmark = 0;
  // Every edge considered (for the pending-backedge-map rebuild at
  // apply time — the map merges each edge's from-side pending map,
  // which intra-stage applications don't mutate).
  Vec<AEdge *> all_edges;
  // The groups to detach, in decision order. The "remainder stays"
  // rule is already folded in: when stay_edges was empty, the last
  // group is dropped here (it keeps the original ES as its home),
  // matching the old loop's single-group-exhausted short-circuit.
  Vec<Vec<AEdge *> *> groups;
};

static ESSplitDecision *decide_entry_set_split(AVar *av, int fsetters, int fmark) {
  EntrySet *es = (EntrySet *)av->contour;
  if (es->split) {
    log(LOG_SPLITTING, "[ses] av %d es %d short-circuit: es->split set\n", av->id, es->id);
    return nullptr;
  }
  // Issue 033 stage A: the confluence position driving this split
  // (same lookup split_edges does). Return-value confluences have
  // no argument position and are not ledgered yet.
  MPosition *avpos = nullptr;
  form_MPositionAVar(x, es->args) {
    if (x->value == av) {
      avpos = x->key;
      break;
    }
  }
  Vec<AEdge *> all_edges, do_edges, stay_edges;
  for (AEdge *ee : es->edges) if (ee) if (ee->args.n) all_edges.add(ee);
  qsort_by_id(all_edges);
  // Type-driven grouping includes RECURSIVE edges when the recursion
  // is STRUCTURAL DESCENT: resolving recursion to monomorphic
  // contours is core IFA design, and the machinery is already in
  // place -- when a recursive edge splits away, record_backedges
  // plants the recursion's pnode in the product's
  // pending_es_backedge_map, and check_split binds it next pass to
  // the same ES as its (split-off) caller contour; polymorphic
  // recursion then re-splits level by level until each contour is
  // monomorphic (leaf contours converge when per-contour condition
  // folding kills the recursive branch). The old blanket exclusion
  // made a self-recursive function with one caller permanently
  // unsplittable (the non_rec==1 short-circuit below): its formal
  // stayed a union of ALL recursion depths' types -- pyc issues/025
  // R1 item 5, deepcopy's `obj` boxed to void*.
  //
  // The separability gate: a recursive edge only joins the grouping
  // when the recursion is LEVEL-DESCENDING at the confluence
  // position -- its type there must be IDENTICAL TO or DISJOINT FROM
  // every other edge's (deepcopy's {outer-list} -> {inner-lists} ->
  // {int64}: each level's actuals partition cleanly, every call site
  // stays monomorphic after the split, no runtime dispatch is ever
  // needed between the same-class level contours). A PARTIAL overlap
  // (same-shape recursion over one union, e.g. a kind-discriminated
  // Expr tree whose lhs/rhs actuals are {Expr#1, Expr#2, None}
  // against a caller's {Expr#2}) means the recursion must stay fused
  // with its caller contour: splitting it both re-derives forever
  // (each product recreates the same confluence; the union's
  // runtime-dead members, e.g. None, strand in contours where
  // nothing resolves) and fans single call sites out across
  // same-class contours that runtime dispatch cannot discriminate
  // (tests/expr_evaluator.py regressed BOTH ways -- compile
  // diagnostics under an ungated version of this change, a
  // "polymorphic dispatch: no branch matched" abort under a
  // rec-vs-nonrec-only disjointness gate).
  auto ety_at = [&](AEdge *ee) -> AType * {
    AVar *a = avpos ? ee->args.get(avpos) : nullptr;
    return a ? type_intersection(a->out->type, ee->match->formal_filters.get(avpos))
             : fa->type_world.bottom_type;
  };
  bool have_nonrec = false;
  if (!fsetters)
    for (AEdge *ee : all_edges) if (ee && ee->from && !is_es_recursive(ee)) { have_nonrec = true; break; }
  int nedges = 0, non_rec_edges = 0;
  for (AEdge *ee : all_edges) if (ee) {
    if (!ee->from) continue;
    nedges++;
    bool rec = !fsetters ? is_es_recursive(ee) : is_es_cs_recursive(ee);
    if (rec) {
      // The setter path keeps the blanket exclusion: setter
      // equivalence over recursive DATA (es_cs backedges) isn't
      // level-separable the way argument types are.
      if (fsetters) continue;
      AType *ety = ety_at(ee);
      // No live non-recursive caller: a dead cycle; leave it fused.
      bool separable = have_nonrec && ety->n;
      if (separable) for (AEdge *oe : all_edges) if (oe && oe != ee && oe->from) {
        AType *oty = ety_at(oe);
        if (!oty->n || oty == ety) continue;
        if (type_intersection(ety, oty) != fa->type_world.bottom_type) {
          separable = false;
          break;
        }
      }
      if (!separable) continue;
    } else
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
  for (AEdge *e : tedges) {
    int compat = 1;
    for (AEdge *ee : stay_edges) {
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
  log(LOG_SPLITTING, "[ses] av %d es %d %s%s nedges=%d non_rec=%d do=%d stay=%d\n",
      av->id, es->id, fsetters ? "setters " : "", fmark ? "marks " : "",
      nedges, non_rec_edges, do_edges.n, stay_edges.n);
  // The single-real-caller short-circuit only applies to the setter
  // path now: on the type path recursive edges group like any other
  // edge (see above), and for a NON-recursive ES this check was
  // always redundant (non_rec==1 implies nedges==1, and a lone edge
  // either stays -- groups empty -- or hits the single-group-
  // exhausted break below).
  if (fsetters && non_rec_edges == 1 && nedges != do_edges.n) {
    log(LOG_SPLITTING, "[ses] av %d es %d short-circuit: non_rec_edges==1 && nedges!=do_edges.n\n", av->id, es->id);
    return nullptr;
  }
  ESSplitDecision *dec = new ESSplitDecision;
  dec->av = av;
  dec->es = es;
  dec->avpos = avpos;
  dec->fsetters = fsetters;
  dec->fmark = fmark;
  dec->all_edges.copy(all_edges);
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
    if (!next_edges.n && !stay_edges.n) {
      // Remainder stays: this (last) group keeps the original ES.
      log(LOG_SPLITTING, "[ses] av %d es %d short-circuit: single group exhausted (groups=%d)\n", av->id, es->id,
          dec->groups.n);
      break;
    }
    Vec<AEdge *> *g = new Vec<AEdge *>;
    g->copy(these_edges);
    dec->groups.add(g);
    do_edges.move(next_edges);
  }
  if (!dec->groups.n) return nullptr;
  return dec;
}

[[nodiscard]] static int apply_entry_set_split(ESSplitDecision *dec) {
  EntrySet *es = dec->es;
  AVar *av = dec->av;
  MPosition *avpos = dec->avpos;
  int fsetters = dec->fsetters, fmark = dec->fmark;
  if (es->split) {
    log(LOG_SPLITTING, "[ses] av %d es %d apply short-circuit: es->split set\n", av->id, es->id);
    return 0;
  }
  PendingAEdgeEntrySetsMap pending_es_backedge_map;
  for (AEdge *ee : dec->all_edges) if (ee) {
    if (!ee->from) continue;
    // Issue 035: was map_union, which routed through the BASE
    // Map::put (pointer-equality keys) on this content-keyed
    // HashMap AND replaced the value vec on hit; merge each entry
    // through the content-correct map_set_add instead.
    form_Map(MapElemAEdgeEntrySets, x, ee->from->pending_es_backedge_map) if (x->key)
        map_set_add(pending_es_backedge_map, x->key, x->value);
  }
  int split = 0;
  for (Vec<AEdge *> *gp : dec->groups) {
    Vec<AEdge *> &these_edges = *gp;
    AEdge *e = these_edges[0];
    // The group's type partition at the confluence position, on the
    // constant-stripped ->type view (raw ->out re-derives constants
    // differently under the constant cap — issue 033 D4 note).
    AType *part = fa->type_world.bottom_type;
    if (avpos)
      for (AEdge *x : these_edges) {
        AVar *a = x->args.get(avpos);
        if (a) part = type_union(part, a->out->type);
      }
    // Issue 033 stage C (D4, revised): when a type-value group's
    // (position, partition) key was already split for in an EARLIER
    // pass, route the group to that decision's product contour
    // instead of minting a fresh bare ES — the per-pass contour
    // manufacturing behind issue 033's divergence. The product
    // stays a plain bare ES: an earlier revision parked groups on
    // FILTERED entry sets, but a filter is a snapshot of the
    // group's partition, and when an argument widens in a later
    // pass analyze_edge silently drops the complement (there is no
    // per-CS edge fan-out here, unlike split_edges) — fysphun
    // regressed 0 -> 3 "has no type" violations exactly that way.
    // Same-pass repeats keep the legacy parking (grouping within a
    // pass is already consistent), as do mark- and setter-driven
    // groups: those are grouped along dimensions a type partition
    // doesn't characterize (mark distances, setter classes), so
    // partition-keyed routing could merge groups the splitter
    // meant separated.
    EntrySet *product = nullptr;
    uint gsig = 0;
    if (!fsetters && !fmark && part != fa->type_world.bottom_type) {
      gsig = group_signature(these_edges, es->fun);
      SplitDecision *d = gsig ? fa->ledger_find(es->fun, cur_split_stage, avpos, part, gsig) : nullptr;
      if (d && d->pass_made != analysis_pass && d->product && d->product != es &&
          group_display_ok(these_edges, d->product, es->fun)) {
        product = d->product;
        ++fa->dup_split_attempts;
        log(LOG_SPLITTING, "[ledger] ROUTE group es %d fun %s %d pos %p part %p/%d -> product %d (first pass %d)\n",
            es->id, es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, (void *)avpos, (void *)part,
            part->sorted.n, d->product->id, d->pass_made);
      }
    }
    if (product) {
      if (!product->split) product->split = es;
      for (AEdge *x : these_edges) {
        x->to = 0;
        x->filtered_args.clear();
        es->edges.del(x);
        set_entry_set(x, product);
        record_backedges(x, es, pending_es_backedge_map);
        split = 1;
        log(LOG_SPLITTING, "SPLIT ES %d (ledger) %s %d from %d -> %d\n", es->id,
            es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, x->pnode->lvals[0]->sym->id,
            x->to->id);
      }
    } else {
      for (AEdge *x : these_edges) {
        x->to = 0;
        x->filtered_args.clear();
        es->edges.del(x);
      }
      for (AEdge *x : these_edges) {
        Vec<AEdge *> new_edges;
        make_entry_set(x, new_edges, es, e->to);
        if (x->to != es) {
          record_backedges(x, es, pending_es_backedge_map);
          split = 1;
          log(LOG_SPLITTING, "SPLIT ES %d %s%s%s %d from %d -> %d\n", es->id, fsetters ? "setters " : "",
              fmark ? "marks " : "", es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id,
              x->pnode->lvals[0]->sym->id, x->to->id);
        }
      }
      // Issue 033 stage A (record-only) for the non-enforced paths.
      if (avpos && part != fa->type_world.bottom_type) {
        EntrySet *gproduct = nullptr;
        for (AEdge *x : these_edges) if (x->to && x->to != es) {
          gproduct = x->to;
          break;
        }
        if (gproduct) {
          if (!gsig) gsig = group_signature(these_edges, es->fun);
        }
        // gsig == 0: the group has no stable identity this pass
        // (wildcard types) — neither route nor record.
        if (gproduct && gsig) {
          SplitDecision *d = fa->ledger_find(es->fun, cur_split_stage, avpos, part, gsig);
          if (!d)
            fa->ledger_add(es->fun, cur_split_stage, avpos, part, gproduct, gsig);
          else if (d->pass_made != analysis_pass) {  // intra-pass repeats aren't re-derivation
            ++fa->dup_split_attempts;
            log(LOG_SPLITTING,
                "[ledger] DUP group es %d fun %s %d stage %d pos %p part %p/%d (first pass %d, product %d)\n",
                es->id, es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, cur_split_stage,
                (void *)avpos, (void *)part, part->sorted.n, d->pass_made, d->product ? d->product->id : -1);
          }
        }
      }
    }
  }
  return split;
}

[[nodiscard]] static int split_entry_set(AVar *av, int fsetters, int fmark, int fdynamic) {
  EntrySet *es = (EntrySet *)av->contour;
  if (es->split) {
    log(LOG_SPLITTING, "[ses] av %d es %d short-circuit: es->split set\n", av->id, es->id);
    return 0;
  }
  if (fdynamic)
    if (split_edges(av, fsetters, fmark)) return 1;
  ESSplitDecision *dec = decide_entry_set_split(av, fsetters, fmark);
  return dec ? apply_entry_set_split(dec) : 0;
}

static void build_type_mark(AVar *av, CreationSet *cs, int mark = 1) {
  int m = av->mark_map ? av->mark_map->get(cs) : 0;
  if (!m) {
    if (!av->out->type->set_in(cs)) {
      log(LOG_SPLITTING, "[btm] av %d skip: cs %d (sym %s) not in out->type (size=%d)\n",
          av->id, cs->id, cs->sym && cs->sym->name ? cs->sym->name : "(anon)",
          av->out->type->set_count());
      return;
    }
    log(LOG_SPLITTING, "[btm] av %d MARK cs %d (sym %s) dist=%d\n",
        av->id, cs->id, cs->sym && cs->sym->name ? cs->sym->name : "(anon)", mark);
    if (!av->mark_map) av->mark_map = new MarkMap;
    av->mark_map->put(cs, mark);
  } else if (m > mark)
    av->mark_map->put(cs, mark);
  else if (m <= mark)
    return;
  for (AVar *y : av->forward) if (y) build_type_mark(y, cs, mark + 1);
}

// To handle recursion, mark value*AVar distances from the nearest
// AVar generating the value.  Dataflow is considered to be only
// from lower to higher distances for the purpose of splitting.
// Issue 033 M5-prelude: joint form of build_type_marks, seeded from
// MANY confluences at once (the stage-4 B4/P3 shape). Backward- and
// forward-closure both distribute over union, so the joint closure
// is EXACTLY the union of the per-seed closures; marks are
// per-(AVar, CS) minimum distances from generation points, so joint
// seeding computes the min over all seeds' gen sets — the same
// deterministic union semantics the stage-4 rework adopted. One
// closure + one collect replaces N per-confluence recomputations
// (measured 47.7s closure + 36.6s collect of pygasus's 85s
// mark_type cost before this rework).
static void build_joint_type_marks(Vec<AVar *> &seeds, Accum<AVar *> &acc) {
  // collect all contributing nodes — index-based so adds appended
  // to acc.asvec during iteration are visited (transitive closure).
  // The range-for over `acc.asvec` captures end() at loop entry and
  // only walks the 1-hop neighborhood — see issue 007 for the
  // finding that this was a long-standing one-level cap.
  for (AVar *av : seeds) if (av) acc.add(av);
  for (int i = 0; i < acc.asvec.n; i++) {
    AVar *x = acc.asvec.v[i];
    for (AVar *y : x->backward) if (y) acc.add(y);
  }
  for (int i = 0; i < acc.asvec.n; i++) {
    AVar *x = acc.asvec.v[i];
    for (AVar *y : x->forward) if (y) acc.add(y);
  }
  // mark them
  for (AVar *x : acc.asvec) {
    if (x->gen) for (CreationSet *s : *x->gen) if (s && s->sym != sym_nil_type) {
        CreationSet *orig = s;
        if (s->sym != s->sym->type) s = s->sym->type->abstract_type->v[0];
        log(LOG_SPLITTING, "[btm-seed] av %d gen-cs %d (sym %s) -> mark-cs %d (sym %s) %s\n",
            x->id, orig->id, orig->sym && orig->sym->name ? orig->sym->name : "(anon)",
            s->id, s->sym && s->sym->name ? s->sym->name : "(anon)",
            orig == s ? "no-subst" : "SUBST");
        build_type_mark(x, s);
      }
  }
}

static void build_type_marks(AVar *av, Accum<AVar *> &acc) {
  Vec<AVar *> seeds;
  seeds.add(av);
  build_joint_type_marks(seeds, acc);
}

static void build_setter_mark(AVar *av, AVar *x, int mark = 1) {
  int m = av->mark_map ? av->mark_map->get(x) : 0;
  if (!m) {
    // The backward recursion below reaches arbitrary AVars; null
    // setters means "empty" (same guard as build_setter_marks'
    // loops). Unguarded, this was an ASLR-dependent crash: pylife
    // segfaulted here on ~4 of 5 runs (null this in Vec::set_in).
    if (!av->setters || !av->setters->set_in(x)) return;
    if (!av->mark_map) av->mark_map = new MarkMap;
    av->mark_map->put(x, mark);
  } else if (m > mark)
    av->mark_map->put(x, mark);
  else if (m <= mark)
    return;
  for (AVar *y : av->backward) if (y) build_setter_mark(y, x, mark + 1);
}

// this is a backward problem, so search forward then back
// to find all the contributors and what they effect
static void build_setter_marks(AVar *av, Accum<AVar *> &acc) {
  // collect all contributing nodes — index-based so elements
  // appended during iteration are visited (transitive closure).
  // A range-for here both capped the closure at one hop AND
  // iterated a Vec whose backing store can be reallocated by
  // add() — the same defect fixed in build_type_marks (see the
  // comment there); survey finding B3.
  acc.add(av);
  for (int i = 0; i < acc.asvec.n; i++) {
    AVar *x = acc.asvec.v[i];
    for (AVar *y : x->forward) if (y && y->setters && y->setters->some_intersection(*av->setters)) acc.add(y);
  }
  for (int i = 0; i < acc.asvec.n; i++) {
    AVar *x = acc.asvec.v[i];
    for (AVar *y : x->backward) if (y && y->setters && y->setters->some_intersection(*av->setters)) acc.add(y);
  }
  // mark them (no additions here — plain iteration is safe)
  for (AVar *x : acc.asvec) if (x->setters) for (AVar *y : *x->setters) if (x == y->container) build_setter_mark(x, y);
}

static void clear_marks(Accum<AVar *> &acc) { for (AVar *x : acc.asvec) x->mark_map = 0; }

// Per-pass reset. NOTE what deliberately SURVIVES a pass (survey
// S3) -- the analysis re-derives flow state from scratch each
// pass, but identity-carrying caches persist:
//   - av->cs_map: CreationSet identity across passes. Load-bearing:
//     consumers hold positional slots into these CSs (see the
//     issue-030 fixpoint fix in make_closure_var; the invariant is
//     "a CS's positional vars[i] must be fed by every pass that
//     feeds the CS, regardless of which Var carries the value").
//   - av->container: structural parenthood, stable across passes.
//   - av->type / av->ivar_offset: written post-convergence by clone.
//   - av->match_cache: SURVIVES across passes (issue 033 S4-E,
//     2026-07-14; reverses survey P2's clear-to-bound-growth).
//     Soundness: entries key on exact canonical AType pointers at
//     every position including nested CS vars (canonical ATypes are
//     never cleared), the visibility PNode, and the closure/partial
//     flags; the match result additionally depends only on the
//     pattern tables, which are fixed for the whole convergence
//     (add_patterns has no mid-FA caller). A stale entry therefore
//     misses, never lies. The payoff: flow re-derives the same
//     monotone type-growth sequence every pass (post-035
//     determinism), so pass k's pattern_match calls are pass k-1's
//     -- retention converts nearly all of them to hits (pygasus:
//     ~25% miss rate -> <1%, match 54s -> seconds). Growth is
//     bounded by distinct (send x type-state) over one convergence;
//     if a future frontend runs multiple FA convergences in one
//     process, this needs a generation stamp -- today there is
//     exactly one FA per process.
//   - av->num_coerce: numeric-confluence coercion target (issue
//     025), set between passes by fa_coerce_numeric_confluences.
//     MUST survive: it has to be in force from the first instant
//     of the next pass so type_coerce_numeric_constants is
//     element-wise monotone for the whole pass.
static void clear_avar(AVar *av) {
  av->gen = 0;
  av->in = fa->type_world.bottom_type;
  av->out = fa->type_world.bottom_type;
  av->setters = 0;
  av->setter_class = 0;
  av->restrict = 0;
  av->restrict_pred = RP_None;
  av->restrict_pred_cls = nullptr;
  av->backward.clear();
  av->forward.clear();
  av->arg_of_send.clear();
  av->mark_map = 0;
  av->live_arg = 0;
  av->needs_fat = 0;
  av->dirty = 0;  // issue 033 M4 probe
  if (av->lvalue) clear_avar(av->lvalue);
}

static void clear_var(Var *v) {
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key) clear_avar(v->avars[i].value);
}

static void clear_edge(AEdge *e) {
  e->es_backedge = 0;
  e->es_cs_backedge = 0;
  e->args.clear();
  e->rets.clear();
  e->match->formal_filters.clear();
  form_MPositionAVar(x, e->filtered_args) clear_avar(x->value);
}

static void clear_es(EntrySet *es) {
  for (AEdge *ee : es->edges) if (ee) clear_edge(ee);
  es->out_edges.clear();
  es->backedges.clear();
  es->cs_backedges.clear();
  es->creates.clear();
  es->live_pnodes.clear();
}

static void clear_cs(CreationSet *cs) {
  cs->defs.clear();
  cs->ess.clear();
  cs->es_backedges.clear();
  for (AVar *v : cs->vars) clear_avar(v);
  if (cs->added_element_var) clear_avar(get_element_avar(cs));
  cs->closure_used = 0;
  cs->unknown_vars.clear();
}

static void foreach_var(void (*pfn)(Var *)) {
  for (Sym *s : fa->pdb->if1->allsyms) if (s->var) pfn(s->var);
  for (Fun *f : fa->funs) for (Var *v : f->fa_all_Vars) pfn(v);
}

struct ClearVarFn {
  static void F(Var *v) { clear_var(v); }
};

static void clear_results() {
  foreach_var(clear_var);
  for (CreationSet *cs : fa->css) clear_cs(cs);
  for (EntrySet *es : fa->ess) clear_es(es);
  fa->type_world.cannonical_setters.clear();
}

// Issue 025 numeric unification (see fa.h decl and AVar::num_coerce).
// For each BOXING violation whose AVar mixes ONLY numeric basic
// types and where at least one member is a numeric CONSTANT narrower
// than the widest member: annotate the AVar with the widest type.
// The next pass then coerces the constant at exactly this (Var,
// contour) flow point -- flow- and contour-sensitive, unlike any
// source-level rewrite (the same `x = 0` MOVE may feed an int-only
// specialization that must keep int, and the confluence may arise
// hops from any MOVE, e.g. against a restrict-narrowed monomorphic
// numeric). Runtime (non-constant) narrow members are left alone --
// they would need an inserted conversion -- so their violations
// persist and are reported honestly. Terminates: each call either
// annotates a previously-unannotated (or wider-retarget) AVar or
// returns 0; targets only widen (coerce_num).
// Annotate `av` if its converged out is a pure-numeric mix.
// Returns 1 when newly annotated (or retargeted wider).
static int coerce_annotate(AVar *av) {
  Sym *w = nullptr;
  Vec<Sym *> basics;
  for (CreationSet *cs : av->out->sorted) {
    Sym *bt = to_basic_type(cs->sym->type);
    if (!bt) continue;  // non-basics don't block (mirrors mixed_basics)
    if (!bt->num_kind) return 0;
    basics.set_add(bt);
    w = w ? coerce_num(w, bt) : bt;
  }
  // Need an actual mix: at least two distinct numeric basics.
  if (!w || basics.set_count() < 2) return 0;
  if (av->num_coerce == w) return 0;
  av->num_coerce = w;
  if (getenv("PYC_DBG_NUMC"))
    fprintf(stderr, "[numc] annotate av#%d '%s' -> %s\n", av->id,
            av->var && av->var->sym && av->var->sym->name ? av->var->sym->name : "?", w->name ? w->name : "?");
  return 1;
}

int fa_coerce_numeric_confluences(Vec<ATypeViolation *> &violations) {
  (void)violations;  // scan directly: see phi-carrier note below
  int annotated = 0;
  // Scan every ES-contour variable rather than the BOXING violations:
  // the violation collector deliberately skips Vars
  // only_used_by_phy_or_phi, but the SSU loop-carry temp (the phi
  // carrier) holds the same numeric mix and becomes a C variable at
  // codegen -- leaving it unannotated leaves an _CG_any behind.
  for (EntrySet *es : fa->ess) {
    for (Var *v : es->fun->fa_all_Vars) {
      AVar *av = make_AVar(v, es);
      annotated += coerce_annotate(av);
      if (av->lvalue) annotated += coerce_annotate(av->lvalue);
    }
  }
  // Ivars of compiler-internal `closure` CSs: pyc lowers a
  // function's locals through its closure frame record, so a
  // loop-carried local's storage IS a closure ivar; leaving it
  // unannotated lets the mix cycle back in through the frame. USER
  // record fields (named classes) stay excluded: mixed numeric
  // fields across instances are the classtag-dispatch machinery's
  // domain (issues 029/030), and coercing them would silently
  // change field polymorphism.
  for (CreationSet *cs : fa->css) {
    if (!cs || cs->sym != sym_closure) continue;
    for (AVar *av : cs->vars) annotated += coerce_annotate(av);
  }
  // The re-run must re-derive flow from scratch: unlike the
  // monotone-growth reanalyze repairs (field promotion), coercion
  // changes what an existing out computes, which is only legal from
  // a clean slate. Same phase as extend_analysis's clear (fa.cc
  // "if (analyze_again) clear_results()").
  if (annotated) clear_results();
  return annotated;
}

static Setters *setters_cannonicalize(Setters *s) {
  assert(!s->sorted.n);
  for (AVar *x : *s) if (x) s->sorted.add(x);
  if (s->sorted.n > 1) qsort_pointers((void **)&s->sorted[0], (void **)s->sorted.end());
  uint h = 0;
  // Accumulate (survey B1) — see type_cannonicalize.
  for (int i = 0; i < s->sorted.n; i++) h += (uint)(intptr_t)s->sorted[i] * open_hash_primes[i % 256];
  s->hash = h ? h : h + 1;  // 0 is empty
  Setters *ss = fa->type_world.cannonical_setters.put(s);
  if (!ss) ss = s;
  return ss;
}

[[nodiscard]] static int update_setter(AVar *av, AVar *s, Accum<AVar *> &avs) {
  Setters *new_setters = nullptr;
  avs.add(av);
  if (av->setters) {
    if (av->setters->in(s)) return 0;
    new_setters = av->setters->add_map.get(s);
    if (new_setters) goto Ldone;
  }
  new_setters = new Setters;
  if (av->setters) new_setters->copy(*av->setters);
  new_setters->add(s);
  new_setters = setters_cannonicalize(new_setters);
  if (av->setters) av->setters->add_map.put(s, new_setters);
Ldone:
  av->setters = new_setters;
  for (AVar *x : av->backward) if (x) (void)update_setter(x, s, avs);
  return 1;
}

static void collect_cs_marked_confluences(Vec<AVar *> &confluences) {
  confluences.clear();
  for (CreationSet *cs : fa->css) {
    for (AVar *av : cs->vars) {
      int nback_marked = 0, ndiff = 0;
      for (AVar *x : av->backward) if (x && x->mark_map) {
        nback_marked++;
        if (!av->contour_is_entry_set && av->contour != GLOBAL_CONTOUR) {
          if (different_marked_args(x, av, 1)) {
            ndiff++;
            confluences.set_add(av);
            break;
          }
        }
      }
      if (av->mark_map || nback_marked)
        log(LOG_SPLITTING, "[ccmc] cs %d (sym %s) ivar av %d marked=%d back_marked=%d diff=%d\n",
            cs->id, cs->sym && cs->sym->name ? cs->sym->name : "(anon)", av->id,
            av->mark_map ? 1 : 0, nback_marked, ndiff);
    }
  }
  confluences.set_to_vec();
  qsort_by_id(confluences);
}

static void split_eq_class(Setters *eq_class, Vec<AVar *> &diff) {
  Setters *diff_class = new Setters, *remaining_class = new Setters;
  diff_class->set_union(diff);
  diff_class = setters_cannonicalize(diff_class);
  eq_class->set_difference(diff, *remaining_class);
  remaining_class = setters_cannonicalize(remaining_class);
  for (AVar *x : *diff_class) if (x) x->setter_class = diff_class;
  for (AVar *x : *remaining_class) if (x) x->setter_class = remaining_class;
}

// AVar->setter_class is the smallest set of setter AVars which
// are equivalent (have the same ->out and equivalent ->setters)
// On a new partition of setters this function recomputes the equiv sets
static void recompute_eq_classes(Vec<Setters *> &ss) {
  for (Setters *s : ss) {
    // build new class for unclassed setters
    Setters *new_s = nullptr;
    for (AVar *v : *s) if (v) if (!v->setter_class) {
      if (!new_s) new_s = new Setters;
      new_s->set_add(v);
    }
    if (new_s) {
      new_s = setters_cannonicalize(new_s);
      for (AVar *v : *new_s) if (v) v->setter_class = new_s;
      // reparition existing classes
      for (AVar *v : *s) if (v) {
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

[[nodiscard]] static int compute_setters(AVar *av, Accum<AVar *> &avs, int akind = AKIND_TYPE) {
  if (av->contour_is_entry_set || av->contour == GLOBAL_CONTOUR) return 0;
  int setters_changed = 0;
  Vec<Setters *> ss;
  Vec<AVar *> *dir = akind == AKIND_SETTER ? &av->forward : &av->backward;
  for (AVar *x : *dir) if (x) {
    assert(x->contour_is_entry_set);
    if (akind == AKIND_TYPE && !x->out->type->n) continue;
    if (akind == AKIND_MARK && !x->mark_map) continue;
    ss.add(new Setters);
    ss[ss.n - 1]->set_add(x);
  }
  for (int i = 0; i < ss.n; i++) ss[i] = setters_cannonicalize(ss.v[i]);
  recompute_eq_classes(ss);
  for (AVar *x : *dir) if (x && x->setter_class) setters_changed |= update_setter(x->container, x, avs);
  return setters_changed;
}

static void collect_setter_confluences(Accum<AVar *> &avs, Vec<AVar *> &setter_confluences,
                                       Vec<AVar *> &setter_starters) {
  for (AVar *av : avs.asvec) {
    if (av->setters) {
      for (AVar *x : av->forward) if (x) {
        if (x->setters && !same_eq_classes(av->setters, x->setters)) {
          setter_confluences.set_add(av);
          break;
        }
      }
      if (av->cs_map) {
        Vec<CreationSet *> css;
        form_Map(CSMapElem, x, *av->cs_map) if (fa->css_set.set_in(x->value)) css.set_add(x->value);
        for (AVar *s : *av->setters) if (s) {
          assert(s->setter_class);
          if (s->container->out->some_intersection(css)) setter_starters.set_add(av);
        }
      }
    }
  }
  setter_confluences.set_to_vec();
  qsort_by_id(setter_confluences);
  setter_starters.set_to_vec();
  qsort_by_id(setter_starters);
}

[[nodiscard]] static int split_with_setter_marks(AVar *av) {
  Accum<AVar *> acc;
  build_setter_marks(av, acc);
  Vec<AVar *> confluences;
  collect_es_marked_confluences(confluences, acc, SPLIT_SETTER);
  int analyze_again = 0;
  for (AVar *av : confluences) {
    if (av->contour_is_entry_set) {
      if (!av->is_lvalue) {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (is_return_value(aav)) analyze_again |= split_entry_set(aav, SPLIT_SETTER, SPLIT_MARK, SPLIT_EDGES);
      } else if (av->var->is_formal)
        analyze_again |= split_entry_set(av, SPLIT_SETTER, SPLIT_MARK, SPLIT_EDGES);
    }
  }
  clear_marks(acc);
  return analyze_again;
}

[[nodiscard]] static int split_ess_setters_marks(Vec<AVar *> &confluences) {
  int analyze_again = 0;
  for (AVar *av : confluences) if (av->contour_is_entry_set) analyze_again |= split_with_setter_marks(av);
  if (!analyze_again)
    for (AVar *av : confluences) if (!av->contour_is_entry_set) analyze_again |= split_with_setter_marks(av);
  return analyze_again;
}

[[nodiscard]] static int split_ess_setters(Vec<AVar *> &confluences) {
  int analyze_again = 0;
  for (AVar *av : confluences) {
    if (av->contour_is_entry_set) {
      if (!av->is_lvalue) {
        //      This proved overly conservative for pyc.sf.net as one edge
        //      carried None which was
        //      overwritten by a setter such that the confluence occured within
        //      the constructor.
        //      if (is_return_value(av))
        analyze_again |= split_entry_set(av, SPLIT_SETTER, SPLIT_VALUE, SPLIT_EDGES);
      } else {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (aav->var->is_formal) analyze_again |= split_entry_set(aav, SPLIT_SETTER, SPLIT_VALUE, SPLIT_EDGES);
      }
    }
  }
  return analyze_again;
}

// Issue 033 D5: cross-pass identity for a split_css decision. The
// partition split_css applies is "these defs share setter
// equivalence classes" — but Setters/setter_class pointers are
// hash-consed in cannonical_setters, which clear_results() CLEARS,
// so they cannot appear in a cross-pass key (issue 033 D0). The
// stable proxy: the CreationSet's sym, the sorted def-Var sym ids
// of the compatible group (Var/Sym are interned for the life of
// the FA), and the CONTENT of the group's setters — each setter
// AVar's Var sym id paired with its canonical constant-stripped
// value type (canonical ATypes are never cleared). The value types
// keep two same-Var-set groups distinct (an int-writing and a
// float-writing instance of the same creation point must not share
// a key — the ES ledger learned this as the builtins_batch
// int/float poisoning; D5's own note says value types alone are
// too weak and def ids alone can't discriminate either, so the key
// carries both). Same wildcard rule as the ES group_signature: an
// unflowed setter value (empty ->type) means the group has NO
// stable identity this pass — return 0, caller must neither record
// nor count. Setter contributions accumulate commutatively (sum),
// so Vec-set iteration order cannot perturb the hash.
static uint cs_group_signature(CreationSet *cs, Vec<AVar *> &compatible_set) {
  Vec<int> def_ids;
  for (AVar *v : compatible_set) if (v) def_ids.add(v->var->sym->id);
  qsort(def_ids.v, def_ids.n, sizeof(def_ids[0]),
        [](const void *a, const void *b) { return *(const int *)a - *(const int *)b; });
  uint h = (uint)(uintptr_t)cs->sym * open_hash_primes[0];
  int i = 1;
  for (int id : def_ids) h += (uint)id * open_hash_primes[i++ % 256];
  for (AVar *v : compatible_set) if (v) {
    if (!v->setters) continue;
    for (AVar *s : *v->setters) if (s) {
      if (!s->out->type->n) return 0;  // unflowed setter: no identity yet
      h += (uint)combine_hash((uintptr_t)s->var->sym->id, (uintptr_t)s->out->type);
    }
  }
  return h ? h : 1;  // 0 is reserved for "no identity"
}

[[nodiscard]] static int split_css(Vec<AVar *> &starters) {
  int analyze_again = 0;
  Vec<CreationSet *> css;
  for (AVar *av : starters) form_Map(CSMapElem, x, *av->cs_map) if (fa->css_set.set_in(x->value)) css.set_add(x->value);
  css.set_to_vec();
  qsort_by_id(css);
  for (CreationSet *cs : css) {
    Vec<AVar *> starter_set, save;
    for (AVar *av : starters) if (av->cs_map->get(cs->sym) == cs) starter_set.add(av);
    log(LOG_SPLITTING, "[scss] cs %d (sym %s) starter_set=%d defs=%d\n", cs->id,
        cs->sym && cs->sym->name ? cs->sym->name : "(anon)", starter_set.n, cs->defs.set_count());
    while (starter_set.n > 1) {
      AVar *av = starter_set[0];
      Vec<AVar *> compatible_set;
      for (AVar *v : starter_set) {
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
        for (AVar *v : compatible_set) if (v) {
          assert(cs == v->cs_map->get(cs->sym));
          v->cs_map->put(cs->sym, new_cs);
        }
        new_cs->split = cs;
        analyze_again = 1;
        log(LOG_SPLITTING, "SPLIT CS %d %s %d -> %d\n", cs->id, cs->sym->name ? cs->sym->name : "", cs->sym->id,
            new_cs->id);
        // Issue 033 D5 (record-only): ledger this CS split decision
        // and count cross-pass re-derivations, the CS-side analog of
        // the ES ledger's dup_splits metric. No behavior change —
        // enforcement (routing the group to d->cs_product instead of
        // minting new_cs) is a separate step, gated on this metric
        // showing re-derivation actually occurs.
        uint csig = cs_group_signature(cs, compatible_set);
        if (!csig) {
          log(LOG_SPLITTING, "[ledger] CS split cs %d -> %d NO IDENTITY (unflowed setter)\n", cs->id, new_cs->id);
        } else {
          SplitDecision *d = fa->ledger_find_cs(csig);
          if (!d) {
            fa->ledger_add_cs(csig, new_cs);
            log(LOG_SPLITTING, "[ledger] RECORD CS split cs %d sym %s %d sig %u product cs %d\n", cs->id,
                cs->sym->name ? cs->sym->name : "", cs->sym->id, csig, new_cs->id);
          } else if (d->pass_made != analysis_pass) {  // intra-pass repeats aren't re-derivation
            ++fa->cs_dup_split_attempts;
            log(LOG_SPLITTING, "[ledger] DUP CS split cs %d sym %s %d sig %u (first pass %d, product cs %d)\n",
                cs->id, cs->sym->name ? cs->sym->name : "", cs->sym->id, csig, d->pass_made,
                d->cs_product ? d->cs_product->id : -1);
          }
        }
      }
    }
  }
  return analyze_again;
}

[[nodiscard]] static int split_for_setters(Accum<AVar *> &avs, int analyze_again) {
  Vec<AVar *> setter_confluences, setter_starters;
  collect_setter_confluences(avs, setter_confluences, setter_starters);
  if (split_ess_setters(setter_confluences)) return 1;
  if (split_ess_setters_marks(setter_confluences)) return 1;
  if (analyze_again) return 1;
  if (split_css(setter_starters)) return 1;
  return analyze_again;
}

// Issue 033 M5 prelude: stage-2 sub-phase cost accumulators, printed
// with the -v stage breakdown at convergence. mark_type dominates
// extend cost at pygasus scale (M0 finding: 81-87% of extend); these
// attribute that cost to closure-building vs diagnostics vs collect
// vs the split machinery so the fix targets the real term.
static double stage2_closure_time = 0, stage2_diag_time = 0, stage2_collect_time = 0, stage2_split_time = 0;

[[nodiscard]] static int split_with_type_marks(AVar *av, int fdynamic) {
  Timer s2_timer;
  Accum<AVar *> acc;
  build_type_marks(av, acc);
  stage2_closure_time += s2_timer.lap();
  // Diagnostic: count closure size, mark-seed candidates (gen != null), and
  // how many AVars actually got a mark_map populated.
  // Guarded: log() is a FUNCTION, so its arguments (set_count()
  // walks, per closure member) evaluate even with logging off —
  // unguarded, these diagnostics are O(closure) per confluence on
  // the hot path.
  if (logging(LOG_SPLITTING)) {
  int closure_marked = 0, closure_with_gen = 0, closure_gen_nonempty = 0;
  for (AVar *x : acc.asvec) if (x) {
    if (x->mark_map) closure_marked++;
    if (x->gen) {
      closure_with_gen++;
      if (x->gen->n) closure_gen_nonempty++;
    }
  }
  log(LOG_SPLITTING, "[stage2-marks] av %d closure=%d with_gen=%d gen_nonempty=%d marked=%d\n",
      av->id, acc.asvec.n, closure_with_gen, closure_gen_nonempty, closure_marked);
  for (AVar *x : acc.asvec) if (x) {
    log(LOG_SPLITTING, "[stage2-marks]   closure-member av %d %s gen=%d out-type=%d\n",
        x->id, x->var && x->var->sym && x->var->sym->name ? x->var->sym->name : "(anon)",
        x->gen ? x->gen->set_count() : -1,
        x->out && x->out->type ? x->out->type->set_count() : -1);
  }
  }
  stage2_diag_time += s2_timer.lap();
  Vec<AVar *> confluences;
  collect_es_marked_confluences(confluences, acc, SPLIT_TYPE);
  stage2_collect_time += s2_timer.lap();
  if (logging(LOG_SPLITTING))
    log(LOG_SPLITTING, "[stage2-marks] av %d marked-confluences=%d\n",
        av->id, confluences.set_count());
  int analyze_again = 0;
  for (AVar *cav : confluences) {
    if (cav->contour_is_entry_set) {
      if (!cav->is_lvalue) {
        if (cav->var->is_formal) {
          int r = split_entry_set(cav, SPLIT_TYPE, SPLIT_MARK, fdynamic);
          log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d ES/formal split_entry_set -> %d\n", cav->id, r);
          if (r) analyze_again = 1;
        } else {
          log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d ES/non-formal-rval skipped\n", cav->id);
        }
      } else {
        AVar *aav = unique_AVar(cav->var, cav->contour);
        if (is_return_value(aav)) {
          int r = split_entry_set(aav, SPLIT_TYPE, SPLIT_MARK, fdynamic);
          log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d ES/return split_entry_set -> %d\n", cav->id, r);
          if (r) analyze_again = 1;
        } else {
          log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d ES/lval-non-return skipped\n", cav->id);
        }
      }
    } else {
      log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d CS-contour skipped\n", cav->id);
    }
  }
  clear_marks(acc);
  stage2_split_time += s2_timer.lap();
  return analyze_again;
}

static void collect_cs_setter_confluences(Vec<AVar *> &setters_confluences) {
  setters_confluences.clear();
  for (CreationSet *cs : fa->css) {
    for (AVar *av : cs->vars) {
      for (AVar *x : av->forward) if (x) {
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
      for (AVar *x : av->forward) if (x) {
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
  // Issue 033 D7: every sibling collector in this file
  // (collect_type_confluences, collect_cs_marked_confluences,
  // collect_es_marked_confluences, collect_setter_confluences)
  // sorts its output before returning; this one didn't. Its sole
  // consumer, split_for_setters_of_setters, feeds the result
  // straight into compute_setters(..., AKIND_SETTER) -- called every
  // pass from both extend_analysis stage 3 and stage 4 -- so an
  // unstable order here was a live, frequently-exercised source of
  // pass-to-pass nondeterminism, not just a theoretical gap.
  qsort_by_id(setters_confluences);
}

[[nodiscard]] static int split_ess_for_type(Vec<AVar *> &imprecisions, int fdynamic) {
  int analyze_again = 0;
  // Issue 033 M2b: for the plain (non-dynamic) stage-1 path, DECIDE
  // every confluence's split against the same unmutated, converged
  // state, then APPLY. The old shape decided each confluence against
  // a graph already mutated by the previous confluences' splits —
  // deterministic post-035, but order-dependent by construction (the
  // 009/021 family). One semantic change, deliberate: when two
  // confluences target the SAME EntrySet (different positions), the
  // old shape split it twice in one pass — the second split
  // partitioning edges that had been rerouted THIS pass and never
  // re-flowed (exactly the unflowed-contour hazard from the M2a
  // post-mortem, in miniature). Now the first decision per ES wins
  // and later ones are deferred: if the imprecision survives the
  // re-flow, the next pass re-collects it and decides it against
  // settled types. The dynamic path (stage 5's refinable violations)
  // keeps the legacy per-AVar shape — split_edges mutates the graph
  // as it goes, so batched deciding would read its own stage's
  // mutations, the exact thing M2b exists to prevent.
  Vec<ESSplitDecision *> decisions;
  for (AVar *av : imprecisions) {
    if (av->contour_is_entry_set) {
      AVar *target = nullptr;
      if (!av->is_lvalue) {
        if (av->var->is_formal)
          target = av;
        else
          log(LOG_SPLITTING, "[stage1] av %d ES/non-formal-rval skipped\n", av->id);
      } else {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (is_return_value(aav))
          target = aav;
        else
          log(LOG_SPLITTING, "[stage1] av %d ES/lval-non-return skipped\n", av->id);
      }
      if (!target) continue;
      if (fdynamic) {
        int r = split_entry_set(target, SPLIT_TYPE, SPLIT_VALUE, fdynamic);
        log(LOG_SPLITTING, "[stage1] av %d ES/%s split_entry_set -> %d\n", av->id, av->is_lvalue ? "return" : "formal",
            r);
        analyze_again |= r;
      } else {
        ESSplitDecision *dec = decide_entry_set_split(target, SPLIT_TYPE, SPLIT_VALUE);
        log(LOG_SPLITTING, "[stage1] av %d ES/%s decide -> %d groups\n", av->id, av->is_lvalue ? "return" : "formal",
            dec ? dec->groups.n : 0);
        if (dec) decisions.add(dec);
      }
    } else {
      log(LOG_SPLITTING, "[stage1] av %d CS-contour skipped (passes to stage2)\n", av->id);
    }
  }
  Vec<EntrySet *> applied;
  for (ESSplitDecision *dec : decisions) {
    if (applied.set_in(dec->es)) {
      log(LOG_SPLITTING, "[stage1] av %d es %d DEFERRED: es already split this pass (next pass re-decides)\n",
          dec->av->id, dec->es->id);
      continue;
    }
    // Claim the ES whether or not the apply reports a split: the
    // apply may mutate (detach/re-park edges) even when nothing
    // ends up counted, and a second decision must never run against
    // a possibly-touched ES.
    applied.set_add(dec->es);
    int r = apply_entry_set_split(dec);
    log(LOG_SPLITTING, "[stage1] av %d es %d apply -> %d\n", dec->av->id, dec->es->id, r);
    analyze_again |= r;
  }
  return analyze_again;
}

// Issue 033 M5-prelude: split the jointly-marked ES confluences via
// the M2b decide-then-apply machinery — every decision computed
// against the same converged, jointly-marked state, first decision
// per EntrySet wins, later ones defer to the next pass (same
// arbitration as stage 1).
[[nodiscard]] static int split_marked_es_confluences(Vec<AVar *> &marked) {
  Vec<ESSplitDecision *> decisions;
  for (AVar *cav : marked) {
    if (!cav->contour_is_entry_set) {
      log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d CS-contour skipped\n", cav->id);
      continue;
    }
    AVar *target = nullptr;
    if (!cav->is_lvalue) {
      if (cav->var->is_formal)
        target = cav;
      else
        log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d ES/non-formal-rval skipped\n", cav->id);
    } else {
      AVar *aav = unique_AVar(cav->var, cav->contour);
      if (is_return_value(aav))
        target = aav;
      else
        log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d ES/lval-non-return skipped\n", cav->id);
    }
    if (!target) continue;
    ESSplitDecision *dec = decide_entry_set_split(target, SPLIT_TYPE, SPLIT_MARK);
    log(LOG_SPLITTING, "[stage2-marks]   marked-conf av %d decide -> %d groups\n", cav->id,
        dec ? dec->groups.n : 0);
    if (dec) decisions.add(dec);
  }
  int analyze_again = 0;
  Vec<EntrySet *> applied;
  for (ESSplitDecision *dec : decisions) {
    if (applied.set_in(dec->es)) {
      log(LOG_SPLITTING, "[stage2-marks] av %d es %d DEFERRED: es already split this pass\n", dec->av->id,
          dec->es->id);
      continue;
    }
    applied.set_add(dec->es);
    int r = apply_entry_set_split(dec);
    log(LOG_SPLITTING, "[stage2-marks] av %d es %d apply -> %d\n", dec->av->id, dec->es->id, r);
    analyze_again |= r;
  }
  return analyze_again;
}

[[nodiscard]] static int split_ess_for_mark_type(Vec<AVar *> &confluences) {
  // Issue 033 M5-prelude: the old shape called split_with_type_marks
  // per confluence — each call rebuilt the full backward+forward
  // transitive closure, re-ran the global marked-confluence collect,
  // and re-attempted splits, making stage 2 O(confluences x
  // universe): 81-87% of pygasus's extend time for progress on 5
  // passes (M0 measurement). Joint seeding (the landed stage-4
  // B4/P3 shape) computes the identical union closure once, marks
  // once (min distances over all seeds' gens — see
  // build_joint_type_marks for why this is the same-or-more-defined
  // semantics), collects once, and splits the marked set through
  // M2b decide-then-apply. The a)/b) priority (ES-contour
  // confluences first, CS-contour ones only if a) found nothing) is
  // preserved.
  int analyze_again = 0;
  Timer s2_timer;
  // a) first those where the confluence is NOT at an instance variable
  {
    Vec<AVar *> seeds;
    for (AVar *av : confluences) if (av->contour_is_entry_set) seeds.add(av);
    if (seeds.n) {
      Accum<AVar *> acc;
      build_joint_type_marks(seeds, acc);
      stage2_closure_time += s2_timer.lap();
      Vec<AVar *> marked;
      collect_es_marked_confluences(marked, acc, SPLIT_TYPE);
      stage2_collect_time += s2_timer.lap();
      log(LOG_SPLITTING, "[stage2-marks] (ES-contour) seeds=%d closure=%d marked=%d\n", seeds.n, acc.asvec.n,
          marked.n);
      analyze_again = split_marked_es_confluences(marked);
      clear_marks(acc);
      stage2_split_time += s2_timer.lap();
    }
  }
  // b) then those where the confluence is at an instance variable
  if (!analyze_again) {
    Vec<AVar *> seeds;
    for (AVar *av : confluences) if (!av->contour_is_entry_set) seeds.add(av);
    if (seeds.n) {
      Accum<AVar *> acc;
      build_joint_type_marks(seeds, acc);
      stage2_closure_time += s2_timer.lap();
      Vec<AVar *> marked;
      collect_es_marked_confluences(marked, acc, SPLIT_TYPE);
      stage2_collect_time += s2_timer.lap();
      log(LOG_SPLITTING, "[stage2-marks] (CS-contour) seeds=%d closure=%d marked=%d\n", seeds.n, acc.asvec.n,
          marked.n);
      analyze_again = split_marked_es_confluences(marked);
      clear_marks(acc);
      stage2_split_time += s2_timer.lap();
    }
  }
  return analyze_again;
}

static bool back_reaching(AVar *av, Vec<AVar *> &reached) {
  if (reached.set_in(av)) return true;
  Accum<AVar *> seen;
  seen.add(av);
  // Index-based: elements appended during iteration must be
  // visited (full backward closure), and a range-for would hold
  // pointers into a Vec that add() can reallocate. Survey B3.
  for (int i = 0; i < seen.asvec.n; i++) {
    AVar *x = seen.asvec.v[i];
    for (AVar *r : x->backward) if (r) {
      if (reached.set_in(r)) return true;
      seen.add(r);
    }
  }
  return false;
}

static void all_back_reaching(Vec<AVar *> &dispatched, Vec<AVar *> &reached, Vec<AVar *> &result) {
  for (AVar *av : dispatched) if (back_reaching(av, reached)) result.set_add(av);
}

static bool is_call_result(AVar *av) {
  PNode *p = av->var->def;
  if (p && av->contour_is_entry_set) {
    EntrySet *es = (EntrySet *)av->contour;
    return es->out_edge_map.get(p) != nullptr;
  }
  return false;
}

static bool result_is_different(AVar *result, AEdge *e) {
  for (int i = 0; i < e->pnode->lvals.n; i++)
    if (result == e->rets[i]) return e->to->rets[i]->out->type != result->out->type;
  assert(!"found");
  return false;
}

static void collect_violation_imprecisions(Vec<ATypeViolation *> &violations, Vec<AVar *> &imprecisions) {
  for (ATypeViolation *v : violations) if (v) {
    if (v->av->container && v->av->container->out->n > 1) imprecisions.set_add(v->av->container);
    if (is_call_result(v->av)) {
      Vec<AVar *> dispatched;
      PNode *p = v->av->var->def;
      EntrySet *es = (EntrySet *)v->av->contour;
      Vec<AEdge *> *ve = es->out_edge_map.get(p);
      if (ve) {
        for (AEdge *e : *ve) if (e && es->out_edges.set_in(e)) {
          if (result_is_different(v->av, e)) {
            form_MPositionAVar(x, e->args) {
              if (e->to->filters.get(x->key)) dispatched.set_add(x->value);
              if (e->match->formal_filters.get(x->key) != x->value->out) dispatched.set_add(x->value);
            }
          }
        }
      }
      Vec<AVar *> args;
      form_MPositionAVar(x, es->args) args.set_add(x->value);
      all_back_reaching(dispatched, args, imprecisions);
    }
  }
  imprecisions.set_to_vec();
  // Issue 033 D7: canonicalize before split_ess_for_type /
  // split_with_type_marks iterate this in a first-wins-short-circuit
  // loop (split_entry_set returns early once es->split is set) --
  // without a stable order, which AVar "drives" a given ES's split
  // this pass depends on open-addressed hash-bucket layout.
  qsort_by_id(imprecisions);
}

[[nodiscard]] static int split_for_violations(Vec<ATypeViolation *> &violations) {
  Vec<AVar *> imprecisions;
  collect_violation_imprecisions(violations, imprecisions);
  log(LOG_SPLITTING, "[stage5] %d violations -> %d imprecisions\n", violations.n, imprecisions.n);
  // Issue 033 D6: a Var whose violation already drove two full
  // stage-5 split attempts and still violates is not refinable by
  // contour splitting (e.g. fysphun's numeric-coercion residue) —
  // exclude it instead of manufacturing contours every pass. The
  // count is per-Var (stable identity) and persistent, so a
  // violation that a split DID resolve never reaches the cutoff.
  Vec<AVar *> refinable;
  for (AVar *av : imprecisions) {
    int attempts = fa->violation_split_attempts.get(av->var);
    if (attempts >= 2) {
      log(LOG_SPLITTING, "[nonrefinable] var %d %s: %d stage-5 split attempts, excluding\n", av->var->sym->id,
          av->var->sym->name ? av->var->sym->name : "", attempts);
      continue;
    }
    fa->violation_split_attempts.put(av->var, attempts + 1);
    log(LOG_SPLITTING, "[stage5-attempt] var %d %s attempt %d (av %d)\n", av->var->sym->id,
        av->var->sym->name ? av->var->sym->name : "", attempts + 1, av->id);
    refinable.add(av);
  }
  int analyze_again = split_ess_for_type(refinable, SPLIT_DYNAMIC);
  if (!analyze_again) for (AVar *av : refinable) analyze_again |= split_with_type_marks(av, SPLIT_DYNAMIC);
  return analyze_again;
}

static void clear_splits() {
  for (EntrySet *es : fa->ess) es->split = 0;
  for (CreationSet *cs : fa->css) cs->split = 0;
}

// NOTE deliberately takes no confluence input: the loop's first
// action collects its own CS setter confluences. It used to take
// the caller's type-confluences Vec by reference as scratch --
// collect_cs_setter_confluences clear()s and refills it -- which
// clobbered `confluences` between extend_analysis stages 3 and 4,
// so stage 4's mark seeding always ran over the emptied vector
// and the mark-setter stages could never fire (issue 007/032).
[[nodiscard]] static int split_for_setters_of_setters() {
  int analyze_again = 0;
  Vec<AVar *> confluences;
  // split based on setters
  while (!analyze_again) {
    // a) compute setters for ivar confluences
    collect_cs_setter_confluences(confluences);
    Accum<AVar *> avs;
    int progress = 0;
    for (AVar *av : confluences) progress |= compute_setters(av, avs, AKIND_SETTER);
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

// ifa/issues/045: precision splitting of method contours per
// receiver CreationSet, for classes explicitly marked
// clone_methods_per_cs (pyc: classes whose __init__ params use
// __pyc_clone_constants__). Violation-driven stages never separate
// same-class receiver CSs (no violation arises in the merged method
// itself), but the merge lets one receiver's field WRITES widen
// every sibling CS's fields, destroying per-CS constants callers
// fold on (issue 040's range/__pyc_more__ trace). Runs ONLY when
// every stage above found nothing, and reuses split_edges'
// find_or_make_filtered_entry_set routing, so products are re-FOUND
// (not re-minted) across passes -- the issue 033 stability rule.
static bool cs_is_per_cs_method_class(CreationSet *cs) {
  if (!cs || !cs->sym) return false;
  if (cs->sym->clone_methods_per_cs) return true;
  Sym *t = cs->sym->type ? unalias_type(cs->sym->type) : 0;
  return t && t->clone_methods_per_cs;
}

[[nodiscard]] static int split_for_per_cs_method_receivers() {
  int analyze_again = 0;
  int n_ess = fa->ess.n;
  for (int i = 0; i < n_ess; i++) {
    EntrySet *es = fa->ess[i];
    // NOTE es->split is LINEAGE (set on every filtered-split
    // product), not "being split away" -- most method ESs are
    // products, so do NOT skip on it. Skip only edge-less ESs
    // (emptied by earlier splits).
    if (!es) continue;
    if (!es->fun || !es->fun->sym) continue;
    bool has_edges = false;
    for (AEdge *ee : es->edges) if (ee) { has_edges = true; break; }
    if (!has_edges) continue;
    // Deterministic arg order (issue 035): positional positions,
    // not args-map bucket order.
    for (MPosition *p : es->fun->positional_arg_positions) {
      AVar *av = es->args.get(p);
      if (!av || !av->out || !av->out->type || av->out->type->sorted.n < 2) continue;
      bool all_flagged = true;
      Sym *cls = 0;
      for (CreationSet *cs : av->out->type->sorted) {
        if (!cs_is_per_cs_method_class(cs)) { all_flagged = false; break; }
        Sym *t = cs->sym->clone_methods_per_cs ? cs->sym : unalias_type(cs->sym->type);
        if (!cls) cls = t;
        else if (cls != t) { all_flagged = false; break; }  // one class per split
      }
      if (!all_flagged) continue;
      if (split_edges(av, 0, 0)) {
        analyze_again = 1;
        log(LOG_SPLITTING, "[per-cs] split es %d fun %s %d at arg %p (%d receiver CSs)\n", es->id,
            es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, (void *)p, av->out->type->sorted.n);
      }
    }
  }
  return analyze_again;
}

// The five split stages (extend_analysis minus its stall/pass-cap
// bookkeeping), extracted so the sticky stall guard in
// extend_analysis can skip them wholesale once the guard has fired.
[[nodiscard]] static int run_split_stages() {
  int analyze_again = 0;
  // Issue 033 M0: per-stage wall-clock measurement. `stage_timer`
  // is lapped at each stage boundary below (whether or not that
  // stage found work) so fa->stage_time[] accumulates the true
  // cost of every stage visited this pass, not just the winning
  // one -- that's what makes a plateau's cost breakdown legible.
  Timer stage_timer;
  compute_recursive_entry_sets();
  compute_recursive_entry_creation_sets();
  clear_splits();
  // Snapshots taken before each split_* call so the sidecar can record
  // the delta this stage produced. See fa_events_storage / record_fa_event.
  int ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
  Vec<AVar *> confluences;
  // 1) split EntrySets based on type using AVar::out
  collect_type_confluences(confluences);
  cur_split_stage = (int)FAPassStage::TYPE_CONFLUENCE;
  analyze_again = split_ess_for_type(confluences, SPLIT_EDGES);
  fa->stage_time[(int)FAPassStage::TYPE_CONFLUENCE] += stage_timer.lap();
  log(LOG_SPLITTING, "split_ess_for_type %d\n", analyze_again);
  if (analyze_again) {
    record_fa_event(FAPassStage::TYPE_CONFLUENCE, analyze_again, ess0, css0, viol0);
    ++fa->stage_progress_count[(int)FAPassStage::TYPE_CONFLUENCE];
  }
  // 2) split EntrySets based on type using marks
  // Issue 033 S5 M2: REVERTED to the original short-circuit
  // (`if (!analyze_again)`) 2026-07-11. The "unlock stage 2
  // unconditionally" variant was landed, verified against pyc C/LLVM
  // 179/0, all 16 ifa-test phases, and a shedskin corpus sweep
  // (23->25 compiled, strict superset) -- but none of those exercise
  // a long-running, many-pass program, and it turned out to
  // segfault fysphun.py (a shedskin corpus member, previously a
  // clean 18-pass/0-violation convergence) partway through pass 15,
  // deterministically, in `Vec<CreationSet*>::begin()`. Confirmed by
  // bisection against the M1-only commit (no crash there) that this
  // stage-2 change is the proximate cause; the precise mechanism
  // (why a many-pass program specifically) was not pinned down
  // before reverting -- see the issue 033 doc for what's known and
  // the open trail for a future attempt. Lesson for next time:
  // "safe" per the standard suite + one corpus sweep is not
  // sufficient evidence for a change to extend_analysis; the
  // fixtures and pyc test corpus are all short-running, and this
  // bug only manifested many passes in on a numerically-heavy
  // program.
  if (!analyze_again) {
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::MARK_TYPE;
    analyze_again = split_ess_for_mark_type(confluences);
    fa->stage_time[(int)FAPassStage::MARK_TYPE] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::MARK_TYPE, analyze_again, ess0, css0, viol0);
      ++fa->stage_progress_count[(int)FAPassStage::MARK_TYPE];
    }
  }
  log(LOG_SPLITTING, "split_ess_for_mark_type %d\n", analyze_again);
  // 3) split based on setters of type
  if (!analyze_again) {
    Accum<AVar *> avs;
    for (AVar *av : confluences) (void)compute_setters(av, avs, AKIND_TYPE);
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::SETTER;
    if (split_for_setters(avs, analyze_again)) analyze_again = 1;
    fa->stage_time[(int)FAPassStage::SETTER] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::SETTER, analyze_again, ess0, css0, viol0);
      ++fa->stage_progress_count[(int)FAPassStage::SETTER];
    }
    log(LOG_SPLITTING, "split_for_setters %d\n", analyze_again);
    if (!analyze_again) {
      ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
      cur_split_stage = (int)FAPassStage::SETTER_OF_SETTER;
      analyze_again = split_for_setters_of_setters();
      fa->stage_time[(int)FAPassStage::SETTER_OF_SETTER] += stage_timer.lap();
      if (analyze_again) {
        record_fa_event(FAPassStage::SETTER_OF_SETTER, analyze_again, ess0, css0, viol0);
        ++fa->stage_progress_count[(int)FAPassStage::SETTER_OF_SETTER];
      }
    }
    log(LOG_SPLITTING, "split_for_setters_of_setters %d\n", analyze_again);
  }
  // 4) split based on setters of type using marks.
  //
  // Reworked (survey B4/P3): the previous shape looped over every
  // type confluence, and per iteration seeded that confluence's
  // marks, re-ran a GLOBAL collect + the full setter-split
  // cascade, and never cleared the marks — so iteration k saw the
  // union of marks from iterations 1..k-1 (an iteration-order
  // dependence, issue-009/021 flavor), the stage was quadratic,
  // and when nothing split the stale marks leaked into stage 5
  // and the converged state. Marks are per-(AVar, CS) minimum
  // distances from generation points; seeding all confluences
  // jointly computes the same minima deterministically, and one
  // collect + one split over the joint marking dominates every
  // per-confluence run of the old loop.
  if (!analyze_again) {
    Accum<AVar *> acc;
    // Issue 033 M5-prelude: ONE build_joint_type_marks call, not a
    // per-confluence loop with the shared accumulator. The loop form
    // re-scanned and re-marked the whole (growing) union closure per
    // seed — the final mark state is identical (build_type_mark
    // min-updates are idempotent and the last iteration already
    // marked over the full union), but the cost was O(confluences x
    // union): 21 of pygasus's 23.8 extend seconds after the stage-2
    // joint rework exposed it as the next term.
    build_joint_type_marks(confluences, acc);
    Vec<AVar *> marked_confluences;
    collect_cs_marked_confluences(marked_confluences);
    Accum<AVar *> avs;
    for (AVar *av : marked_confluences) {
      int r = compute_setters(av, avs, AKIND_MARK);
      log(LOG_SPLITTING, "[stage4] marked-conf av %d %s compute_setters(MARK) -> %d\n", av->id,
          av->var && av->var->sym && av->var->sym->name ? av->var->sym->name : "(anon)", r);
    }
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::MARK_SETTER;
    if (split_for_setters(avs, analyze_again)) analyze_again = 1;
    fa->stage_time[(int)FAPassStage::MARK_SETTER] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::MARK_SETTER, analyze_again, ess0, css0, viol0);
      ++fa->stage_progress_count[(int)FAPassStage::MARK_SETTER];
    }
    log(LOG_SPLITTING, "split_for_setters with marks %d\n", analyze_again);
    if (!analyze_again) {
      ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
      cur_split_stage = (int)FAPassStage::MARK_SETTER_OF_SETTER;
      analyze_again = split_for_setters_of_setters();
      fa->stage_time[(int)FAPassStage::MARK_SETTER_OF_SETTER] += stage_timer.lap();
      if (analyze_again) {
        record_fa_event(FAPassStage::MARK_SETTER_OF_SETTER, analyze_again, ess0, css0, viol0);
        ++fa->stage_progress_count[(int)FAPassStage::MARK_SETTER_OF_SETTER];
      }
    }
    log(LOG_SPLITTING, "split_for_setters_of_setters with marks %d\n", analyze_again);
    clear_marks(acc);
  }
  if (!analyze_again) {
    // 5) split AEdges(s) and EntrySet(s) for violations based on type using
    // dynamic dispatch
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::VIOLATION;
    analyze_again = split_for_violations(fa->type_violations);
    fa->stage_time[(int)FAPassStage::VIOLATION] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::VIOLATION, analyze_again, ess0, css0, viol0);
      ++fa->stage_progress_count[(int)FAPassStage::VIOLATION];
    }
  }
  log(LOG_SPLITTING, "split_for_violations %d\n", analyze_again);
  // 6) precision: per-receiver-CS method contours for
  // clone_methods_per_cs classes (ifa/issues/045). Only on full
  // quiescence of stages 1-5 so it cannot perturb their
  // trajectories within a pass.
  if (!analyze_again) {
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::PER_CS_RECEIVER;
    analyze_again = split_for_per_cs_method_receivers();
    fa->stage_time[(int)FAPassStage::PER_CS_RECEIVER] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::PER_CS_RECEIVER, analyze_again, ess0, css0, viol0);
      ++fa->stage_progress_count[(int)FAPassStage::PER_CS_RECEIVER];
    }
  }
  log(LOG_SPLITTING, "split_for_per_cs_method_receivers %d\n", analyze_again);
  return analyze_again;
}

[[nodiscard]] static int extend_analysis() {
  int analyze_again = 0;
  extend_timer.restart();
  // Issue 033: the stall guard is STICKY. pass_limit_hit is set by
  // the stall guard and the pass cap below; without this entry
  // check it only zeroed the current pass's continue vote -- the
  // stages had already run and split by the time the guard was
  // consulted. So whenever the frontend's reanalyze() callback
  // kept the outer loop alive, every subsequent pass split again,
  // with the guard "firing" after the fact each time: unbounded
  // contour growth with no cap at all, since the pass-limit check
  // was gated on analyze_again, which the guard had just zeroed
  // (observed on bh under the issue033-stage-c branch: 73+
  // non-improving passes, ess 492 -> 2317, until an external
  // timeout). Once the guard fires, the splitter stays suppressed.
  // It re-arms only when a reanalyze-driven pass genuinely improves
  // the violation count below its best (annotator progress can
  // legitimately unblock refinement; strict improvement bounds the
  // number of re-arms by the violation count itself) or resolves
  // ALL violations (fysphun's shape: the guard fires on the
  // plateau, the coercion annotator then clears every residual
  // violation, and the splitter safely resumes pure precision
  // splitting -- zero-violation passes never advance the stall
  // counter, so this cannot be the runaway; the hard pass cap
  // below still bounds pathological re-arm cycles).
  // Only the flag is cleared here: best_violations/stall_passes are
  // left to the tail bookkeeping below, which sees the same improved
  // count and resets them exactly as it would have pre-guard -- so
  // re-armed stall counting is identical to the never-fired case.
  if (fa->pass_limit_hit) {
    int v = fa->type_violations.set_count();
    if (analysis_pass <= fa->pass_limit && (v == 0 || v < fa->best_violations)) {
      fa->pass_limit_hit = false;
      log(LOG_SPLITTING, "STALL GUARD re-armed at pass %d: %d violations\n", analysis_pass, v);
    }
  }
  if (!fa->pass_limit_hit) analyze_again = run_split_stages();
  extend_timer.stop();
  if (analyze_again) {
    // Divergence (stall) guard. Split decisions are not idempotent
    // across passes, so some inputs never reach a fixed point: ess
    // and violation counts oscillate while per-pass cost grows
    // superlinearly, making pass_limit unreachable in wall time
    // (issue 025 compile timeouts). Splitting exists to resolve
    // violations; if the count hasn't improved on its best in
    // stall_limit consecutive passes, treat further splitting as
    // divergence and stop. Zero-violation passes (pure precision
    // splitting) don't advance the stall counter.
    //
    // Dup-aware (043 shape C, see IFA_STALL_LIMIT's note): only
    // passes that RE-DERIVED split decisions (per-pass ledger dup
    // counters -- oscillation) advance the stall counter; a
    // non-improving pass of purely FIRST-TIME splits is structural
    // descent (a contour chain exposing one new confluence per
    // pass) and gets the looser IFA_NONIMPROVE_LIMIT instead.
    // Observed shape: a recursive function iterating nested lists
    // needs ~14 dup-free passes (its iterator method chain splits
    // one link per pass) while its single boxing violation waits on
    // the last link -- the unconditional counter stopped it at 8
    // with the violation stranded.
    int v = fa->type_violations.set_count();
    if (v > 0) {
      if (v < fa->best_violations) {
        fa->best_violations = v;
        fa->stall_passes = 0;
        fa->nonimprove_passes = 0;
      } else {
        bool rederived = fa->dup_split_attempts + fa->cs_dup_split_attempts > 0;
        if (rederived) ++fa->stall_passes;
        ++fa->nonimprove_passes;
        if (fa->stall_passes >= fa->stall_limit || fa->nonimprove_passes >= IFA_NONIMPROVE_LIMIT) {
          fa->pass_limit_hit = true;
          log(LOG_SPLITTING,
              "STALL LIMIT reached at pass %d, %d violations (best %d): %d re-deriving (limit %d), "
              "%d non-improving (limit %d); stopping\n",
              analysis_pass, v, fa->best_violations, fa->stall_passes, fa->stall_limit, fa->nonimprove_passes,
              IFA_NONIMPROVE_LIMIT);
          analyze_again = 0;
        }
      }
    }
  }
  if (analysis_pass > fa->pass_limit) {
    // We've hit the configured pass cap: force termination, surface
    // the trip on LOG_SPLITTING, and flag it on FA so the frontend
    // can distinguish a converged type_violations set from this
    // mid-iteration snapshot. The existing violations stay in
    // type_violations — callers iterating them get the snapshot, but
    // they can check fa->pass_limit_hit to know they're holding
    // partial results. Unconditional (issue 033): this was gated on
    // analyze_again, which the stall guard zeroes when it fires, so
    // the cap was unreachable exactly when the loop was running away
    // via reanalyze()-driven passes.
    if (!fa->pass_limit_hit)
      log(LOG_SPLITTING, "PASS LIMIT %d reached at pass %d, %d violations remain (mid-iteration)\n",
          fa->pass_limit, analysis_pass, fa->type_violations.set_count());
    fa->pass_limit_hit = true;
    analyze_again = 0;
  }
  if (analyze_again) clear_results();
  pass_timer.stop();
  ++analysis_pass;
  if (ifa_verbose) {
    double flow = pass_timer.time - extend_timer.time - match_timer.time;
    printf(
        "PASS %d COMPLETE: %f seconds, %f flow (%d%%), %f match (%d%%), %f "
        "extend (%d%%), %d ess, %d css, %d violations, %d dup_splits, %d cs_dups, "
        "%ld dirty/%ld examined avars\n",
        analysis_pass, pass_timer.time, flow, (int)(flow * 100.0 / pass_timer.time), match_timer.time,
        (int)(match_timer.time * 100.0 / pass_timer.time), extend_timer.time,
        (int)(extend_timer.time * 100.0 / pass_timer.time), fa->ess.n, fa->css.n,
        fa->type_violations.set_count(), fa->dup_split_attempts, fa->cs_dup_split_attempts, fa->dirty_avar_count,
        fa->examined_avar_count);
  }
  if (write_code_exit == analysis_pass) {
    if1_simple_dead_code_elimination(fa->pdb->if1);
    ifa_code("if1");
    exit(1);
  }
  match_timer.accumulate();
  extend_timer.accumulate();
  pass_timer.accumulate();
  log(LOG_SPLITTING, "======== pass %d ========\n", analysis_pass);
  if (!analyze_again && ifa_verbose) {
    double flow = pass_timer.accumulator[0] - extend_timer.accumulator[0] - match_timer.accumulator[0];
    printf(
        "COMPLETE: %f seconds, %f flow (%d%%), %f match (%d%%) cached (%d%%), "
        "%f extend (%d%%)\n",
        pass_timer.accumulator[0], flow, (int)(flow * 100.0 / pass_timer.accumulator[0]), match_timer.accumulator[0],
        (int)(match_timer.accumulator[0] * 100.0 / pass_timer.accumulator[0]),
        (int)(((double)pattern_match_hits / (double)pattern_match_complete) * 100.0), extend_timer.accumulator[0],
        (int)(extend_timer.accumulator[0] * 100.0 / pass_timer.accumulator[0]));
    // Issue 033 M0: per-stage breakdown of the extend cost above,
    // and how many passes each stage was the one to report
    // progress (first-stage-wins truncation point -- see S5 M2).
    double stage_total = 0;
    for (int i = 0; i < FA::kNumFAPassStages; i++) stage_total += fa->stage_time[i];
    printf("  stage breakdown (of %f extend seconds):\n", stage_total);
    for (int i = 0; i < FA::kNumFAPassStages; i++) {
      if (fa->stage_time[i] == 0 && fa->stage_progress_count[i] == 0) continue;
      printf("    %-22s %f s (%d%%), progress on %ld pass%s\n", fa_pass_stage_name((FAPassStage)i),
             fa->stage_time[i], stage_total > 0 ? (int)(fa->stage_time[i] * 100.0 / stage_total) : 0,
             fa->stage_progress_count[i], fa->stage_progress_count[i] == 1 ? "" : "es");
    }
    // Issue 033 M5 prelude: attribute mark_type's dominant cost.
    if (stage2_closure_time + stage2_diag_time + stage2_collect_time + stage2_split_time > 0)
      printf("    mark_type sub-phases: closure %f s, diag %f s, collect %f s, split+clear %f s\n",
             stage2_closure_time, stage2_diag_time, stage2_collect_time, stage2_split_time);
  }
  return analyze_again;
}

// Issue 029 step 1: identify AVars that participate in a
// polymorphic confluence and propagate the marker backward
// through producer flow edges.
//
// An AVar is a "confluence" iff its `out` contains
// CreationSets from 2+ distinct non-nil metatypes.  Single-
// metatype AVars (even with multiple CSs of the same class)
// stay monomorphic; nil-only or T+nil stay monomorphic
// (nil = NULL pointer, encoded by absence).
//
// Propagation: once an AVar A is marked, every AVar B with
// B in A->backward (i.e. B flows to A) is also marked.
// Rationale: B's producer must materialize a value
// compatible with A's fat representation, so B carries the
// fat-ness backward to the materialization point.
// Terminates because the bit is monotone: each AVar gets
// marked at most once.
//
// Step 1 (this commit) only computes the bit and exposes
// it via `PYC_DEBUG_FAT=1`.  Codegen consumption lands in
// later steps.
// Distinct non-nil meta-types seen across all AVars of a
// single Var.  Per-AVar may be monomorphic (FA splits
// aggressively), but the Var's storage representation must
// be wide enough to cover the union seen by every ES that
// references it — that's where polymorphism lands at the
// codegen layer.  Uses `cs->type` (the CS's grouping type
// — e.g. `bool` for both True/False, the class for an
// instance) rather than `cs->sym` (which would split bool
// CSs into True+False, ints into per-value constants, etc.
// — those aren't polymorphism for dispatch).
// Map a CS to its grouping type for dispatch purposes.
// - Class instances: cs->sym is the class.  Use it.
// - Value-type constants (sym_true, sym_false, int
//   literals): cs->sym is the constant Sym; the actual
//   class is `cs->sym->type` (e.g. sym_bool).  Walk one
//   level up so True+False group as bool, 5+7 group as
//   int, etc.  Otherwise the grouping would treat every
//   distinct constant value as its own type and we'd see
//   spurious confluences everywhere.
static Sym *cs_group_type(CreationSet *cs) {
  if (!cs || !cs->sym) return nullptr;
  Sym *s = cs->sym;
  if (s->is_constant && s->type) s = s->type;
  return s;
}

static int distinct_nonnil_metatypes(Var *v) {
  Vec<Sym *> seen;
  form_AVarMapElem(x, v->avars) {
    AVar *av = x->value;
    if (!av || !av->out) continue;
    for (CreationSet *cs : av->out->sorted) {
      Sym *g = cs_group_type(cs);
      if (!g) continue;
      if (g == sym_nil_type) continue;
      seen.set_add(g);
    }
  }
  return seen.n;
}

static void mark_fat_avars() {
  // Collect initial confluence AVars.
  Vec<AVar *> worklist;
  int n_total = 0;
  int n_initial = 0;
  int n_var_total = 0;
  int n_var_initial = 0;
  for (Fun *f : fa->funs) {
    for (Var *v : f->fa_all_Vars) {
      n_var_total++;
      // Count total AVars for reporting.
      form_AVarMapElem(x, v->avars) {
        if (x->value) n_total++;
      }
      if (distinct_nonnil_metatypes(v) > 1) {
        n_var_initial++;
        form_AVarMapElem(x, v->avars) {
          AVar *av = x->value;
          if (av && !av->needs_fat) {
            av->needs_fat = 1;
            worklist.add(av);
            n_initial++;
          }
        }
      }
    }
  }
  // Backward propagation: a producer that flows into a fat
  // AVar must also be fat (it materializes the fat value).
  while (worklist.n) {
    AVar *a = worklist.pop();
    for (AVar *b : a->backward) {
      if (b && !b->needs_fat) {
        b->needs_fat = 1;
        worklist.add(b);
      }
    }
  }
  if (getenv("PYC_DEBUG_FAT_ALL")) {
    // Dump every Var's union of grouping types.  Lets us
    // see WHY a Var was or wasn't classified as fat.
    for (Fun *f : fa->funs) {
      for (Var *v : f->fa_all_Vars) {
        Vec<Sym *> seen;
        form_AVarMapElem(x, v->avars) {
          AVar *av = x->value;
          if (!av || !av->out) continue;
          for (CreationSet *cs : av->out->sorted) {
            Sym *g = cs_group_type(cs);
            if (g) seen.set_add(g);
          }
        }
        if (seen.n < 1) continue;
        cchar *fname = f->sym && f->sym->name ? f->sym->name : "(anon)";
        cchar *vname = v->sym && v->sym->name ? v->sym->name : "(anon)";
        fprintf(stderr, "[all] fun=%s var=%s id=%d types={", fname, vname, v->id);
        int n = 0;
        for (Sym *s : seen) {
          if (n++) fprintf(stderr, ", ");
          fprintf(stderr, "%s", s && s->name ? s->name : "(anon)");
        }
        fprintf(stderr, "}\n");
      }
    }
  }
  if (getenv("PYC_DEBUG_FAT")) {
    int n_marked = 0;
    int n_var_marked = 0;
    for (Fun *f : fa->funs) {
      for (Var *v : f->fa_all_Vars) {
        bool v_fat = false;
        form_AVarMapElem(x, v->avars) {
          AVar *av = x->value;
          if (av && av->needs_fat) {
            n_marked++;
            v_fat = true;
          }
        }
        if (v_fat) n_var_marked++;
      }
    }
    fprintf(stderr, "[fat] %d/%d AVars marked needs_fat (initial AVars: %d)\n",
            n_marked, n_total, n_initial);
    fprintf(stderr, "[fat] %d/%d Vars marked needs_fat (initial Vars: %d)\n",
            n_var_marked, n_var_total, n_var_initial);
    // Per-fun summary of fat AVars by Var name.
    for (Fun *f : fa->funs) {
      bool any_in_fun = false;
      for (Var *v : f->fa_all_Vars) {
        bool v_fat = false;
        form_AVarMapElem(x, v->avars) {
          if (x->value && x->value->needs_fat) { v_fat = true; break; }
        }
        if (v_fat) {
          if (!any_in_fun) {
            cchar *fname = f->sym && f->sym->name ? f->sym->name : "(anon)";
            fprintf(stderr, "[fat]   fun %s:\n", fname);
            any_in_fun = true;
          }
          cchar *vname = v->sym && v->sym->name ? v->sym->name : "(anon)";
          // Print up to 4 distinct metatypes in this Var's
          // unified out.
          Vec<Sym *> seen;
          form_AVarMapElem(x, v->avars) {
            AVar *av = x->value;
            if (!av || !av->out) continue;
            for (CreationSet *cs : av->out->sorted) {
              Sym *g = cs_group_type(cs);
              if (!g) continue;
              if (g == sym_nil_type) continue;
              seen.set_add(g);
            }
          }
          fprintf(stderr, "[fat]     var %s (id %d): types {", vname, v->id);
          int n = 0;
          for (Sym *s : seen) {
            if (n++) fprintf(stderr, ", ");
            if (n > 4) { fprintf(stderr, "..."); break; }
            fprintf(stderr, "%s", s && s->name ? s->name : "(anon)");
          }
          fprintf(stderr, "}\n");
        }
      }
    }
  }
}

static void set_void_lub_types_to_void(Var *v) {
  CreationSet *s = fa->type_world.void_type->v[0];
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key) {
      AVar *av = v->avars[i].value;
      if (av->out->in(s)) av->out = fa->type_world.void_type;
    }
}

static void set_void_lub_types_to_void() { foreach_var(set_void_lub_types_to_void); }

static void remove_unused_closures(Var *v) {
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key) {
      AVar *av = v->avars[i].value;
      for (CreationSet *cs : av->out->sorted) if (cs->sym == sym_closure && !cs->closure_used) {
        Vec<CreationSet *> css;
        for (CreationSet *cs : av->out->sorted) {
          if (cs->sym == sym_closure && !cs->closure_used) continue;
          css.add(cs);
        }
        av->out = make_AType(css);
        return;
      }
    }
}

static void remove_unused_closures() { foreach_var(remove_unused_closures); }

static void complete_pass() {
  collect_results();
  collect_argument_type_violations();
  collect_var_type_violations();
  pass_timer.stop();
}

// Per-contour "can this ES's own body, or anything transitively
// reachable via out_edges, raise" -- see EntrySet::can_raise (fa.h)
// for the full rationale. Seeded from Sym::direct_raise (clone-
// invariant: every ES of a Sym shares that Sym's own IF1 body, so
// the seed doesn't depend on the ES); propagated via a small,
// self-contained fixed point over fa->ess/EntrySet::out_edges,
// independent of and much cheaper than FA's own type-inference
// fixed point. Re-run fresh at the top of every pass (see
// analyze_to_convergence below) so it always reflects the CURRENT
// ES/AEdge graph -- which may still be growing early on, as
// polymorphic call sites resolve more candidates pass over pass.
// Monotonic (only ever sets bits, never clears): an under-approximate
// answer on an early pass (missing edge not yet discovered) is
// always safe -- worst case, a foldable check stays real for one
// extra pass -- and self-corrects once the edge appears, with no
// separate "did anything change" signal needed to justify another
// pass (that's already driven by extend_analysis()'s own type-graph
// convergence criteria, unrelated to and unaffected by this).
static void compute_es_can_raise() {
  for (EntrySet *es : fa->ess) {
    if (es->can_raise) continue;
    if (es->fun && es->fun->sym && es->fun->sym->direct_raise) es->can_raise = 1;
  }
  bool changed = true;
  while (changed) {
    changed = false;
    for (EntrySet *es : fa->ess) {
      if (es->can_raise) continue;
      for (AEdge *e : es->out_edges) {
        if (e && e->to && e->to->can_raise) {
          es->can_raise = 1;
          changed = true;
          break;
        }
      }
    }
  }
}

// ifa/issues/057: the flow-to-fixpoint inner loop below (edge/send/es
// worklists) has no bound at all, unlike the outer extend_analysis()
// splitting loop (pass_limit + the issue-033 stall guard). A
// non-convergent input -- confirmed via ifa/issues/055 and 057, both
// FA's polymorphic type union failing to stabilize for some AVar --
// churns this inner loop forever: hundreds of thousands of edges
// processed with fa->ess.n (distinct EntrySets) completely flat,
// consuming unbounded memory (observed >1GB and still climbing after
// 280s on 057's 4-line repro) with no diagnostic, ever. Worse: the
// PER-EDGE cost itself grows over time as the stuck AVar's type union
// keeps accumulating without ever stabilizing (measured: the first
// ~140K edges took ~15s, the next 200K took over 120s) -- so a raw
// edge-count threshold is unreliable, either too slow to trip (if set
// high enough to tolerate legitimate large passes) or fires on a
// slow-but-finite legitimate program. A wall-clock stagnation timeout
// is robust to this regardless of per-edge cost: as long as fa->ess.n
// (distinct EntrySets) keeps growing at all, the clock keeps
// resetting and legitimate large programs are unaffected. Calibrated
// against the largest known-converging corpus example (pygasus,
// issue 033's own worst case): its busiest single pass processes
// ~65K edges while fa->ess.n grows by hundreds *within that same
// pass* (973 -> 4832 across passes, never flat for long) -- nowhere
// close to STALL_TIMEOUT_SECONDS of zero growth. This does not fix
// *why* convergence fails (that's 055/057's still-open root cause) --
// it converts an unbounded hang/OOM into a clean, bounded failure
// with a diagnostic pointing at the actual bug class.
static const long STALL_CHECK_INTERVAL = 20000;
static const time_t STALL_TIMEOUT_SECONDS = 120;

static void analyze_to_convergence() {
  do {
    compute_es_can_raise();
    initialize_pass();
    fa->edge_worklist.enqueue(fa->top_edge);
    long edge_count = 0;
    int last_ess_check = fa->ess.n;
    time_t last_ess_change_time = time(nullptr);
    while (fa->edge_worklist.head || fa->send_worklist.head) {
      while (AEdge *e = fa->edge_worklist.pop()) {
        e->in_edge_worklist = 0;
        analyze_edge(e);
        if ((++edge_count % STALL_CHECK_INTERVAL) == 0) {
          if (fa->ess.n > last_ess_check) {
            last_ess_check = fa->ess.n;
            last_ess_change_time = time(nullptr);
          } else if (time(nullptr) - last_ess_change_time > STALL_TIMEOUT_SECONDS) {
            fail(
                "FA flow analysis made no EntrySet progress for %lds (%ld "
                "edges processed) -- non-convergent input (see "
                "ifa/issues/057-sorted-tolist-fa-nonconvergence.md)",
                (long)STALL_TIMEOUT_SECONDS, edge_count);
          }
        }
      }
      while (AVar *send = fa->send_worklist.pop()) {
        send->in_send_worklist = 0;
        add_send_edges_pnode(send->var->def, (EntrySet *)send->contour);
      }
      while (EntrySet *es = fa->es_worklist.pop()) {
        es->in_es_worklist = 0;
        add_es_constraints(es);
      }
    }
    complete_pass();
    // The pass cap bounds the WHOLE loop, including passes kept
    // alive only by reanalyze() (issue 033): with the splitter
    // suppressed by the sticky stall guard, extend_analysis returns
    // 0 but a frontend annotator that never quiesces could
    // otherwise drive flow passes forever.
  } while ((extend_analysis() || if1->callback->reanalyze(fa->type_violations)) && analysis_pass <= fa->pass_limit);
}

int FA::analyze(Fun *top) {
  ::fa = this;
  if (!global_es) {
    // The distinguished global contour (see GLOBAL_CONTOUR in
    // fa.h). A real EntrySet so `(EntrySet *)contour` derefs on
    // global AVars are safe; in_es_worklist stays permanently 1
    // so no worklist ever picks it up (it has no edges/pnodes to
    // analyze); not registered in fa->ess so clone/equivalence
    // passes never see it.
    global_es = new EntrySet(top);
    global_es->in_es_worklist = 1;
  }
  initialize();
  top_edge = make_top_edge(top);
  analyze_to_convergence();
  // Experimental: mid-FA inlining (issue 026 followup).
  // After first convergence, run simple_inlining to fold
  // identity-fun wrappers (e.g. type-specialized
  // __pyc_to_bool__), then reset per-ES live-pnode caches
  // and re-converge so FA's second pass sees the cleaner
  // IR.  Gated on `ifa_fa_inline`; default off (production
  // runs simple_inlining post-FA via ifa_optimize()).
  if (ifa_fa_inline) {
    mark_live_funs(this);
    simple_inlining(this);
    // Stale constraint state: inlining marked some PNodes
    // dead and possibly added new ones.  Drop each ES's
    // live_pnodes set; add_es_constraints repopulates it
    // on the next pass.  AVars on Vars survive (Vars are
    // stable across inlining), so flow info already
    // computed isn't lost — the second pass re-derives
    // constraints for the new shape and converges from
    // there.
    for (EntrySet *es : ess) es->live_pnodes.clear();
    type_violations.clear();
    analyze_to_convergence();
  }
  // Issue 029 step 1: identify polymorphic confluences for
  // future fat-pointer codegen.  No effect on this pass yet
  // (just sets the AVar bit + optional debug dump).
  mark_fat_avars();
  set_void_lub_types_to_void();
  remove_unused_closures();
  if1->callback->report_analysis_errors(type_violations);
  if (show_violation_output) show_violations(fa, stderr);
  if (fruntime_errors) convert_NOTYPE_to_void();
  return (!fruntime_errors && type_violations.set_count()) ? -1 : 0;
}

static Var *info_var(IFAAST *a, Sym *s) {
  if (!s) s = a->symbol();
  if (!s) return 0;
  if (a && a->pnodes.n) {
    for (PNode *n : a->pnodes) {
      for (Var *v : n->lvals) if (v->sym == s) return v;
      for (Var *v : n->lvals) if (v->sym == s) return v;
      for (Var *v : n->rvals) if (v->sym == s) return v;
    }
  }
  if (s->var) return s->var;
  return 0;
}

// Given an IFAAST node and a Sym, find the Sym which
// corresponds to the concrete (post-cloning) type of the
// variable corresponding to the Sym at that IFAAST node.
Sym *type_info(IFAAST *a, Sym *s) {
  Var *v = info_var(a, s);
  if (v) return v->type;
  return 0;
}

// Given a function and an IFAAST node, return the set of
// functions which could be called from that IFAAST node.
void call_info(Fun *f, IFAAST *a, Vec<Fun *> &funs) {
  funs.clear();
  for (PNode *n : a->pnodes) {
    Vec<Fun *> *ff = f->calls.get(n);
    if (ff) funs.set_union(*ff);
  }
  funs.set_to_vec();
}

// Given a variable return the vector of constants
// which that variable could take on.
// Returns 0 for no constants or non-constant (e.g. some integer).
int constant_info(Var *v, Vec<Sym *> &constants) {
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key) {
      AVar *av = v->avars[i].value;
      for (CreationSet *cs : *av->out) if (cs) {
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

Sym *constant(Sym *s) {
  if (s->constant || s->is_symbol || s->is_fun || s->type_kind) return s;
  return nullptr;
}

Sym *get_constant(Var *v) {
  if (Sym *c = constant(v->sym)) return c;
  Sym *c = nullptr;
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key) {
      Sym *cc = get_constant(v->avars[i].value);
      if (!cc || (c && c != cc))
        return nullptr;
      else
        c = cc;
    }
  return c;
}

Sym *get_constant(AVar *av) {
  Sym *c = nullptr;
  for (CreationSet *cs : *av->out) if (cs) {
    if (cs->sym->constant || cs->sym->is_meta_type) {
      if (c && c != cs->sym) return nullptr;
      c = cs->sym;
    } else
      return nullptr;
  }
  return c;
}

// Given an IFAAST node and a Sym, find the set of
// constants which could arrive at that point.
// make sure that there is not some dominating
// non-constant type.
int constant_info(IFAAST *a, Vec<Sym *> &constants, Sym *s) {
  constants.clear();
  Var *v = info_var(a, s);
  if (v) return constant_info(v, constants);
  return 0;
}

int symbol_info(Var *v, Vec<Sym *> &symbols) {
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key) {
      AVar *av = v->avars[i].value;
      for (CreationSet *cs : *av->out) if (cs) {
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

void return_nil_transfer_function(PNode *pn, EntrySet *es) {
  AVar *result = make_AVar(pn->lvals[0], es);
  update_gen(result, make_abstract_type(sym_void));
}

void return_int_transfer_function(PNode *pn, EntrySet *es) {
  AVar *result = make_AVar(pn->lvals[0], es);
  update_gen(result, make_abstract_type(sym_int));
}

void return_string_transfer_function(PNode *pn, EntrySet *es) {
  AVar *result = make_AVar(pn->lvals[0], es);
  update_gen(result, make_abstract_type(sym_string));
}

void collect_types_and_globals(FA *fa, Vec<Sym *> &typesyms, Vec<Var *> &globals) {
  // collect all syms
  for (Fun *f : fa->funs) {
    if (!f->live) continue;
    Vec<Var *> vars;
    f->collect_Vars(vars);
    for (Var *v : vars) {
      if ((v->live && !v->sym->is_local && v->sym->nesting_depth != f->sym->nesting_depth + 1) || v->sym->is_symbol ||
          v->sym->is_fun)
        globals.set_add(v);
      if (v->type && v->live) typesyms.set_add(v->type);
    }
  }
  // collect type has syms
  int again = 1;
  while (again) {
    again = 0;
    Vec<Sym *> loopsyms;
    loopsyms.copy(typesyms);
    for (int i = 0; i < loopsyms.n; i++)
      if (loopsyms[i] && loopsyms.v[i]->type_kind) {
        for (Sym *s : loopsyms[i]->has) {
          again = typesyms.set_add(s) || again;
          if (s->var && s->var->type) again = typesyms.set_add(s->var->type) || again;
        }
      }
  }
  typesyms.set_to_vec();
  globals.set_to_vec();
  // Issue 035: both sets accumulate over raw pointers, so
  // set_to_vec yields heap-layout order — and codegen numbers
  // globals (g%d) and emits type declarations in iteration order,
  // making the generated C vary between identical runs.
  qsort_by_id(typesyms);
  qsort_by_id(globals);
}

// to be called from the debugger

void pp(AVar *av) {
  printf("(AVar %d ", av->id);
  if1_dump_sym(stdout, av->var->sym);
  printf(" OUT: ");
  pp(av->out);
  printf(")\n");
}

void pp(AType *t) {
  printf("(AType %d ", t->n);
  for (CreationSet *cs : t->sorted) pp(cs);
  printf(")\n");
}

void pp(CreationSet *cs) {
  printf("(CreationSet %d ", cs->id);
  if1_dump_sym(stdout, cs->sym);
  printf(" defs: %d ", cs->defs.n);
  printf(" vars: %d ", cs->vars.n);
  printf(")\n");
}
