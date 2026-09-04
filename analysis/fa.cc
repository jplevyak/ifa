#include "ifadefs.h"

#include <algorithm>
#include <map>
#include <set>
#include <vector>
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

#include <set>
#include <string>

// ifa/issues/074: which splitter stage is running, for the per-stage
// churn probes (IFA_DBG_STAGE). -1 outside extend_analysis.
static int cur_split_stage = -1;
bool fgraph_pass_contours = false;
int write_code_exit = 0;
int analysis_pass = 0;
FA *fa = nullptr;
static Timer pass_timer, match_timer, extend_timer;
// ifa/111 probe: the units the propagation worklists actually process.
// `examined_avar_count` counts the SPLITTER's exhaustive sweep, not this,
// so per-unit cost cannot be derived from it. PYC_DBG_WORK=1 to print.
static long work_edges, work_sends, work_escons;
// ifa/issues/111: selective (closure-scoped) per-pass invalidation
// instead of clear_results()'s full from-bottom reset. Default OFF --
// M2 landed this switch and the differential harness BEFORE any
// behaviour change, deliberately: the failure mode that matters here
// is silent precision drift, not a crash, so the equivalence check has
// to exist before there is anything to check.
//
// File-local rather than an EXTERN in common/fail.h beside ifa_narrow:
// adding a global there makes ifa-test pull main.o out of libifa_gc.a
// to resolve it, which then collides on log_tag and wants
// compile_one_file. Nothing outside fa.cc needs to see this.
static int ifa_selective = 0;

// ifa/issues/111 M1: EntrySets whose incoming-edge set changed this
// pass. `es->split` alone is NOT the full seed: split_edges signals
// `again` on a REDISPATCH (`ee->to != old`), which retargets an
// existing edge to an ALREADY-EXISTING EntrySet -- no new contour, no
// split mark. Both the old and new target lose/gain an input, so both
// (and their forward closures) are invalidated. Cleared by
// clear_splits() alongside the split marks.
static Vec<EntrySet *> fa_pass_retargeted;

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
      "type_confluence", "mark_type",   "setter",         "setter_of_setter", "mark_setter",
      "mark_setter_of_setter", "violation", "per_cs_receiver", "csm_element_cs",
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

// ifa/issues/112: hash FA's whole computed state -- every AVar's `out`
// CreationSet set, in a canonical walk -- at a named point. Called per
// pass and at the FA/clone boundaries, so two runs' traces can be
// diffed and the FIRST differing label names the interval that
// introduced the divergence.
void dbg_trace_fa_state(cchar *where) {
  static int on = -1;
  if (on < 0) on = getenv("IFA_DBG_FATRACE") ? 1 : 0;
  if (!on) return;
  Vec<AVar *> avs;
  for (Fun *f : fa->funs) if (f)
    for (Var *v : f->fa_all_Vars) if (v)
      form_AVarMapElem(x, v->avars) if (x->value) avs.add(x->value);
  for (CreationSet *cs : fa->css) if (cs)
    for (AVar *iv : cs->vars) if (iv) avs.add(iv);
  if (avs.n > 1) qsort_by_id(avs);
  unsigned long h = 1469598103934665603UL;
#define FAMIX2(x) (h = (h ^ (unsigned long)(x)) * 1099511628211UL)
  for (AVar *av : avs) {
    FAMIX2(av->id);
    if (av->out) {
      Vec<int> ids;
      for (CreationSet *c : *av->out) if (c) ids.add(c->id);
      if (ids.n > 1) qsort(ids.v, ids.n, sizeof(ids[0]), [](const void *a, const void *b) {
        int x = *(const int *)a, y = *(const int *)b; return (x > y) - (x < y);
      });
      for (int i : ids) FAMIX2(i);
    }
    FAMIX2(0x9e3779b9UL);
  }
#undef FAMIX2
  fprintf(stderr, "FASTATE %s navars=%d h=%lx\n", where, avs.n, h);
}

AEdge::AEdge() : from(nullptr), to(nullptr), pnode(nullptr), fun(nullptr), match(nullptr), in_edge_worklist(0) {
  id = fa->aedge_id++;
  fa->all_aedges.add(this);  // ifa/issues/098: authoritative list for clear_results
}

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
      no_static_arity(0),
      atype(nullptr),
      equiv(nullptr),
      type(nullptr) {
  id = fa->creation_set_id++;
  fa->all_creation_sets.add(this);  // ifa/issues/098: authoritative list for clear_results
}

CreationSet::CreationSet(CreationSet *cs)
    : dfs_color(DFS_white), added_element_var(0), closure_used(0), tuple_able(0),
      no_static_arity(0), atype(nullptr), equiv(nullptr), type(nullptr) {
  sym = cs->sym;
  no_static_arity = cs->no_static_arity;  // issues/110: durable, so splits inherit it
  seq_src.copy(cs->seq_src);              // issues/110: durable source memory too
  // ifa/issues/066: durable lineage, collapsed to the root as we go.
  split_origin = cs->split_origin ? cs->split_origin : cs;
  id = fa->creation_set_id++;
  fa->all_creation_sets.add(this);  // ifa/issues/098: authoritative list for clear_results
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
  fa->all_entry_sets.add(this);  // ifa/issues/098: authoritative list for clear_results
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
  if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_csmint[cur_split_stage];
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

// The invariant every consumer of the flow graph relies on is
// `b->in >= a->out` for each link a -> b. Returning early when the link
// already exists breaks it: the link is created once, at whatever moment
// the constraint generator first ran, and if `a` was empty then, the
// re-assert is skipped forever after -- `propagate_out_change` only
// pushes on a CHANGE to `a->out`, so a value that arrived in between is
// never delivered. Measured on ifa/issues/100's exception repro: a
// bound-method closure's captured-receiver slot sat at `in = {}` while
// its feeder held a concrete type and the link was present, so
// `partial_application` dispatched with an empty `self`, pattern_match
// found nothing, and the call went NOTYPE. Re-assert unconditionally --
// `update_in` is a no-op when nothing changes, so this costs a union
// test on an already-established edge.
static void flow_var_to_var(AVar *a, AVar *b) {
  if (a == b) return;
  if (!a->forward.set_in(b)) {
    a->forward.set_add(b);
    b->backward.set_add(a);
  }
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

static int cselem_enabled();  // ifa/issues/101, defined with the other flags
static int csmold_enabled();  // ifa/issues/101, ditto
// ifa/issues/074 (PYC_CSELEM=3): re-key container CreationSet identity on
// the RECEIVER's structural element shape. Defined with capture_elem_keys.
static bool cselem_shape_key(AVar *v, Sym *s, std::string &out);
static CreationSet *cselem_shape_reuse(AVar *v, Sym *s);
static void cselem_shape_claim(const std::string &key, CreationSet *cs);

static cchar *dbg_cs_route = nullptr;      // ifa/issues/055: which reuse route fired
static cchar *dbg_cs_route_want = getenv("IFA_DBG_CSROUTE");

CreationSet *creation_point(AVar *v, Sym *s) {
  dbg_cs_route = nullptr;
  CreationSet *cs = v->cs_map ? v->cs_map->get(s) : 0;
  EntrySet *es = (EntrySet *)v->contour;
  if (cs) {
    assert(cs->sym == s);
    dbg_cs_route = "cs_map";
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
    if (cmc && cmc->clone_methods_per_cs) goto Lno_split_parent;
  }
  // ifa/issues/055: the CreationSet follows the EntrySet split.
  //
  // PYC_CSSPLIT=0 restores the old behaviour: a split child INHERITS its
  // parent's instance CS, so every contour of a function shares the one
  // CreationSet its allocation site produced. That is what made
  // set.difference's `r = set()` a single CS across all three of its
  // contours -- the int one, the str one, and the chained one -- which
  // forced that CS's element type to int64|str and, because the chained
  // contour takes it as receiver AND returns it, fed the shared site
  // from itself. The splitter then chased the symptom forever: 146 <->
  // 149 EntrySets, period 2, to the pass cap.
  //
  // The exemption for this already existed but was reachable only via
  // `clone_methods_per_cs` (the `goto Lno_split_parent` above), and that flag
  // is set in exactly one place -- python_ifa_build_syms.cc, when a
  // class's __init__ has a __pyc_clone_constants__ parameter. `set`
  // and `dict` take no ctor arguments at all, so they could never
  // qualify, even though their instances need separating by ELEMENT
  // type rather than by constant. Splitting with the ES instead makes
  // the exemption unnecessary: the other stages (TYPE_CONFLUENCE,
  // SETTER, SETTER_OF_SETTER on this repro) already split the contour
  // that contains the creation point, and the CS now follows.
  //
  // Bounded, not a new growth source: split products are found durably
  // across passes (find_or_make_filtered_entry_set searches fun->ess)
  // and `cs_map` persists across clear_avar, so a split child mints its
  // instance once and memoizes it.
  // DEFAULT 1. Measured against PYC_CSSPLIT=0 after the three defects it
  // first exposed were fixed (all on the default path, all latent before
  // this): optimize/dead.cc's fa->funs rebuild, analysis/clone.cc's
  // per-CreationSet field layout, and codegen/cg.cc's uncast container
  // subscript.
  //
  //   corpus     67 of 77 compile either way, program for program,
  //              and sunfish improves (400s timeout -> clean failure)
  //   pyc suite  296 passed / 14 known  ->  297 passed / 0 failed /
  //              13 known (the 055 repro flips KNOWN -> PASS)
  //   ifa/055    6-line repro: 52 passes pass_limit_hit CONVERGED=0
  //              -> 28 passes CONVERGED=1, 0 violations, right answer
  //   plcfrs     still does not converge, but 4378 -> 2451 violations
  //              and ess 1246 -> 850
  //
  // Set to 0 to restore split-parent inheritance.
  static int cssplit = -1;
  if (cssplit < 0) {
    cchar *cv = getenv("PYC_CSSPLIT");
    cssplit = cv ? atoi(cv) : 1;
  }
  if (es && es->split && !cssplit) {
    AVar *oldv = make_AVar(v->var, es->split);
    cs = oldv->cs_map ? oldv->cs_map->get(s) : 0;
    if (cs) {
      assert(cs->sym == s);
      dbg_cs_route = "split_parent";
      goto Lfound;
    }
  }
  // ifa/issues/129 step 2: a `creators` reuse route stood here and was
  // DEAD -- `if (nvars != -1 || x->vars.n != nvars) continue;` continues
  // on every iteration (nvars == -1 makes the second test always true,
  // nvars != -1 makes the first). Dead since IFA 0.6, the commit that
  // first published this file, so it never selected a CreationSet in the
  // history of the code and `nvars` had no consumer but this loop.
  //
  // Deleted rather than repaired, because both repairs are whole-program
  // merges and neither is the reuse a demand-driven splitter wants:
  //
  //   `&&`  with nvars == -1 (every caller but make_kind) it takes the
  //         FIRST creator of the sym unconditionally -- one CreationSet
  //         per class for the whole program.
  //   `==`  ("no arity asked for -> skip this route") narrows it to
  //         make_kind, but then fuses every record CS of one sym and
  //         arity program-wide, which is exactly the per-position
  //         precision ifa/issues/104 depends on tuples keeping.
  //
  // The reuse this route gestures at is real -- shedskin's
  // `ifa_class_types`/`classes_nr` index, which MOVES an allocation site
  // onto an existing contour whose deduced element types agree. That is
  // keyed on the converged CONTENT, not on arity, and it is 129 step 3
  // route 6. `cselem_shape_reuse` below is its per-site approximation.
Lno_split_parent:;
  // ifa/issues/101 (PYC_CSELEM): before minting another container CS for
  // this site, ask what element type the site converged to on the
  // PREVIOUS pass, and reuse an existing CS of the same sym that
  // converged to the same thing. Container CSs are minted per
  // allocation-site x contour but their element types collapse to far
  // fewer distinct values -- corpus-wide 1994 CS for 341 element shapes,
  // and `stereo` alone is 185 CS for 2. shedskin gets this for free
  // because its `list<T>` is keyed on T; here the element type is an
  // ATTRIBUTE of a CS identified by its creation site, so equivalent
  // containers stay distinct and every container METHOD is then split
  // per CS rather than per element type.
  //
  // Deliberately keyed on the durable, converged element type and never
  // on the current one: every container CS starts empty and acquires
  // elements later, so canonicalizing on "currently empty" would merge a
  // list that will become list<int> with one that will become list<str>.
  // Sites whose CSs converged to DIFFERENT element types are marked
  // ambiguous and left alone -- there the extra CreationSets are earning
  // their keep.
  if (cselem_enabled() && cselem_enabled() != 3 && s != sym_closure && s->element) {
    AType *want = v->var ? fa->var_elem_key.get(v->var) : nullptr;
    bool ambig = v->var && fa->var_elem_ambig.get(v->var);
    if (getenv("IFA_DBG_CSELEM"))
      fprintf(stderr, "[cselem-try] p=%d sym=%s var=%s want=%p ambig=%d creators=%d keyed=%d\n", analysis_pass,
              s->name ? s->name : "?", (v->var && v->var->sym->name) ? v->var->sym->name : "?", (void *)want,
              ambig ? 1 : 0, s->creators.n, [&] {
                int n = 0;
                for (CreationSet *x : s->creators) if (x && x->elem_key_pass >= 0) ++n;
                return n;
              }());
    if (want && !ambig) {
      for (CreationSet *x : s->creators)
        if (x && x->elem_key_pass >= 0 && x->elem_key == want &&
            !(s->abstract_type && x == s->abstract_type->v[0])) {
          if (getenv("IFA_DBG_CSELEM"))
            fprintf(stderr, "[cselem] p=%d sym=%s var=%s -> reuse cs=%d (elem_key %p)\n", analysis_pass,
                    s->name ? s->name : "?", v->var->sym->name ? v->var->sym->name : "?", x->id, (void *)want);
          cs = x;
          dbg_cs_route = "cselem";
          goto Lfound;
        }
    }
  }
  // ifa/issues/074 (PYC_CSELEM=3): re-key on the RECEIVER's element SHAPE.
  //
  // shedskin never has this problem because `list<T>` IS keyed on T: one
  // template instantiation per element type, and `list<T>::__deepcopy__`
  // returns `list<T>` by signature. Here the element type is an ATTRIBUTE
  // of a CreationSet identified by its creation SITE x contour, so
  // `list<int64>` built at one site in two contours is two CreationSets
  // that nothing downstream can see as the same type -- which is why
  // copy-of-copy-of-copy never closes: every level mints a fresh CS, that
  // CS is a fresh element type for the level above, and the recursion has
  // no fixed point to reach.
  //
  // Mode 1 (var_elem_key) cannot do this. It keys on the durable element
  // type of the SITE and vetoes any site that converged to more than one
  // -- and `r` in list.__deepcopy__ converges to a different element type
  // in every contour, so it is exactly the case mode 1 declines.
  //
  // Keyed on (site, receiver shape) rather than on the shape alone: this
  // canonicalizes the contours of ONE allocation site, it does not fuse
  // unrelated sites.
  if (cselem_enabled() == 3 && s != sym_closure && s->element) {
    if (CreationSet *x = cselem_shape_reuse(v, s)) {
      if (!(s->abstract_type && x == s->abstract_type->v[0])) {
        cs = x;
        dbg_cs_route = "csshape";
        goto Lfound;
      }
    }
  }
  // ifa/issues/101 direction 2: the mold fallback (see csmold_enabled).
  // Last resort before minting -- every earlier path (cs_map memo, split
  // parent inheritance) has already declined. clone_methods_per_cs
  // classes are excluded: issue 045 established that their instances
  // MUST stay per-contour, since the per-constant contours exist exactly
  // to give each constant binding its own instance CS.
  //
  // ifa/issues/105, MODE 3: the mold must not undo a SPLIT. PYC_CSSPLIT=1
  // (ifa/055, default) exists so that "the CreationSet follows the
  // EntrySet split"; handing a split CHILD the mold its parent already
  // owns puts both contours back on one container instance and defeats
  // exactly that. The two defaults were added a day apart and the mold,
  // being later in creation_point, silently wins.
  //
  // Measured on a chain of four nested copy.deepcopy calls: SEVEN split
  // children of list.__deepcopy__ took the mold (every csmold hit in the
  // run had a non-null es->split), and the shared `r` came back with
  // element type {list<itself>, int64, int64, list, int64} -- both
  // self-referential and container/scalar mixed. Three copies is clean,
  // four is not, and PYC_CSMOLD=0 makes every level monomorphic again.
  {
    // Block-scoped: the forward gotos above may not jump past an
    // initialization, so these cannot live at function scope.
    int mold = csmold_enabled();
    bool split_child = v->contour_is_entry_set && es && es->split;
    bool eligible = mold && (mold == 2 || s->element) && !(mold == 3 && split_child);
    if (eligible && s != sym_closure && v->var) {
      Sym *cmc0 = s->clone_methods_per_cs ? s : (s->type ? unalias_type(s->type) : 0);
      if (!(cmc0 && cmc0->clone_methods_per_cs)) {
        for (CreationSet *x : s->creators)
          if (x && x->creation_var == v->var && !(s->abstract_type && x == s->abstract_type->v[0])) {
            if (getenv("IFA_DBG_CSMOLD"))
              fprintf(stderr, "[csmold] p=%d sym=%s var=%s es=%d split=%d -> reuse cs=%d\n", analysis_pass,
                      s->name ? s->name : "?", v->var->sym->name ? v->var->sym->name : "?", es ? es->id : -1,
                      split_child ? es->split->id : -1, x->id);
            cs = x;
            dbg_cs_route = "csmold";
            goto Lfound;
          }
      }
    }
  }
Lunique:
  // new creation set
  if (getenv("IFA_DBG_CSMINT"))
    fprintf(stderr, "[csmint] p=%d sym=%s varid=%d varsym=%d var=%s es=%d split=%d parent_had=%d closure=%d cmc=%d\n",
            analysis_pass, s->name ? s->name : "?", v->var ? v->var->id : -1,
            (v->var && v->var->sym) ? v->var->sym->id : -1,
            v->var && v->var->sym && v->var->sym->name ? v->var->sym->name : "?",
            es ? es->id : -1, (es && es->split) ? es->split->id : -1,
            (es && es->split) ? (make_AVar(v->var, es->split)->cs_map ? 1 : 0) : -1, s == sym_closure ? 1 : 0,
            (s->clone_methods_per_cs || (s->type && unalias_type(s->type)->clone_methods_per_cs)) ? 1 : 0);
  dbg_cs_route = "MINT";
  cs = new CreationSet(s);
  cs->creation_var = v->var;  // ifa/issues/101: for the per-site element key
  // ifa/issues/074: claim this (site, receiver-shape) so the next contour
  // with the same receiver shape reuses it instead of minting again.
  if (cselem_enabled() == 3 && s != sym_closure && s->element) {
    std::string shape_key;
    if (cselem_shape_key(v, s, shape_key)) cselem_shape_claim(shape_key, cs);
  }
  if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_csmint[cur_split_stage];
  s->creators.add(cs);
  for (Sym *h : s->has) {
    assert(h->var);
    AVar *iv = unique_AVar(h->var, cs);
    add_var_constraint(iv);
    cs->vars.add(iv);
    if (h->name) cs->var_map.put(h->name, iv);
  }
Lfound:
  if (dbg_cs_route_want && s->name && !strcmp(s->name, dbg_cs_route_want))
    fprintf(stderr, "CSROUTE p=%d sym=%s var=%s es=%d split=%d -> cs=%d via %s\n", analysis_pass, s->name,
            v->var && v->var->sym && v->var->sym->name ? v->var->sym->name : "?", es ? es->id : -1,
            (es && es->split) ? es->split->id : -1, cs ? cs->id : -1, dbg_cs_route ? dbg_cs_route : "?");
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
  CreationSet *nil_cs = nullptr;  // issue 060 -- decided after the loop
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
      else if (cs->sym->type == sym_nil_type)
        nil_cs = cs;  // issue 060: keep-or-strip decided after the loop
      else
        nulls = 1;  // void / unknown: always stripped from ->type
    }
    t->sorted.add(cs);
  }
  // issue 060: nil_type (None) is normally stripped from the ->type
  // projection (is_unique_type), so a pointer-shaped `T | None` union
  // stays a single clone -- None is a null pointer there, unambiguous,
  // and a frontend may sanction that merge (pyc does). But IFA's core
  // discipline is to split incompatible types, and None IS
  // incompatible with a raw scalar (int/bool/float): under the unboxed
  // representation they share the zero bit pattern, so a shared clone
  // literally cannot tell `None` from `0`/`False` (issue 060). Keep nil
  // in ->type whenever the union also carries a num_kind scalar, so the
  // type-splitter puts the None value in its own contour instead of
  // coercing it to `(scalar)NULL`.
  if (nil_cs) {
    bool has_scalar = false;
    for (CreationSet *c : nonconsts)
      if (c && c->sym->type && c->sym->type->num_kind) { has_scalar = true; break; }
    if (has_scalar)
      nonconsts.set_add(nil_cs);  // keep nil in ->type (no nulls: it is not stripped)
    else
      nulls = 1;  // pointer / other: strip nil as before
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

static void recompute_eq_classes(Vec<Setters *> &ss);  // ifa/issues/111 M3 option 1

static bool same_eq_classes(Setters *s, Setters *ss) {
  if (s == ss) return true;
  if (!s || !ss) return false;
  // ifa/issues/111 M3 option 1: class LAZILY rather than assert.
  //
  // setter_class is normally assigned by compute_setters over a pass's
  // confluences, which works because the full reset rebuilds every
  // Setters set in the same pass -- so every member was classed here.
  // Selective invalidation preserves sets across passes, so a member
  // can survive that this pass's classing never reached.
  //
  // recompute_eq_classes is exactly the primitive that assigns a class
  // to unclassed members (and repartitions existing classes around
  // them), so calling it here computes the SAME answer the stage would
  // have, rather than inventing one. Answering `false` for unclassed
  // instead was tried and refuses EntrySet merges, which costs
  // convergence -- see the note below.
  if (ifa_selective) {
    bool unclassed = false;
    for (AVar *av : *s) if (av && !av->setter_class) { unclassed = true; break; }
    if (!unclassed)
      for (AVar *av : *ss) if (av && !av->setter_class) { unclassed = true; break; }
    if (unclassed) {
      Vec<Setters *> both;
      both.add(s);
      both.add(ss);
      recompute_eq_classes(both);
    }
  }
  Vec<Setters *> sc1, sc2;
  // NOTE (ifa/issues/111 M3): answering `false` here for an unclassed
  // AVar instead of asserting was tried, so that selective
  // invalidation could tolerate preserved-but-unclassed AVars. It
  // refuses EntrySet merges, contours grow without bound and the
  // analysis stops converging (collatz ran to the 120s timeout). The
  // assert stays: it is the louder, more diagnosable failure.
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

static long mark_cs_differ = 0, mark_cs_same = 0;

// ifa/issues/074 (PYC_CPAMARK): swap the cartesian-product name in for the
// mark. different_marked_args already compares two sets of CreationSets --
// the CPA question -- but only over CSs admitted by the distance filter
// `m - offset == x->value`. With this on, the filter is dropped and the CS
// sets are compared directly, so the split rule becomes "which CreationSet
// is here", with no depth term. IFA_DBG_MARKWHY predicts the effect: it is
// exactly the `cs_same` verdicts (98% of hq2x's, 2% of the listcomp
// repro's) that stop firing.
static int cpa_mark_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CPAMARK");
    e = v ? atoi(v) : 0;
  }
  return e;
}
static int mark_why_enabled() {
  static int e = -1;
  if (e < 0) e = getenv("IFA_DBG_MARKWHY") ? 1 : 0;
  return e;
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
          if (cpa_mark_enabled() || m - offset == x->value) marks1.set_add(x->key);
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
            if (cpa_mark_enabled() || m - offset == x->value) marks2.set_add(x->key);
          }
        }
      } else {
        found2 = 1;
        marks2.set_add(x->key);
      }
    }
  }
  int diff = found1 && found2 && marks1.some_disjunction(marks2);
  // ifa/issues/074 (IFA_DBG_MARKWHY): mark_map is keyed by CreationSet, so
  // the test above is a CS-SET comparison -- the same question a
  // cartesian-product contour name asks -- but taken over only the CSs
  // that pass the distance filter `m - offset == x->value`. This counts
  // how often that filter changes the answer: `cs_differ` = the raw CS
  // sets differ too (a CPA name would separate these as well), `cs_same`
  // = identical CS sets separated purely by depth-from-generator, i.e.
  // a split no type-tuple naming would ever make.
  if (diff && mark_why_enabled()) {
    Vec<void *> all1, all2;
    if (a1->mark_map) form_Map(MarkElem, x, *a1->mark_map)
        if (basis1->mark_map && basis1->mark_map->get(x->key)) all1.set_add(x->key);
    if (a2->mark_map) form_Map(MarkElem, x, *a2->mark_map) {
        if (!basis) all2.set_add(x->key);
        else if (basis->mark_map && basis->mark_map->get(x->key)) all2.set_add(x->key);
      }
    if (all1.some_disjunction(all2)) ++mark_cs_differ; else ++mark_cs_same;
  }
  return diff;
}

// ifa/issues/074 (IFA_DBG_INCOMPAT): which CLAUSE of the compatibility
// test separates edges from contours. The `rets` clause compares the
// CALLER's return-destination types -- which are downstream of what this
// very contour returns, so a contour's identity partly depends on its own
// result. If that clause is doing the work, the naming is circular by
// construction.
static long ic_arg = 0, ic_ret = 0, ic_retn = 0;
// ifa/issues/074: of the stage-1 splits that actually fire, how many were
// triggered by a FORMAL confluence versus a RETURN-VALUE confluence.
static long tc_formal = 0, tc_return = 0;
static int ld_dup_es = 0, ld_dup_cs = 0, ld_churn = 0;
static long tc_seen = 0, tc_skip_rval = 0, tc_skip_lval = 0, tc_skip_cs = 0, tc_dec = 0, tc_defer = 0;

// ifa/issues/124: `->type` strips a pure-nil AType to bottom (make_AType's
// is_unique_type branch; the 060 carve-out that KEEPS nil only fires when
// the same AType also carries a num_kind scalar, which a lone `{None}`
// does not). The partitioner below guards every comparison with
// `->n &&`, so an edge passing only None reads as "nothing known yet" and
// is compatible with everything -- and it costs the split twice: the nil
// edge stays in the ES, and then the genuinely-differing edges are pulled
// back OUT of do_edges when re-tested against it (measured on
// ifa/issues/124's repro: do=0 stay=2 groups=0 on every pass, forever).
//
// Give the SPLITTER a view that separates "not analyzed" (raw empty too)
// from "carries only nil" (raw non-empty), without touching the ->type
// projection every other consumer reads -- narrowing, defaulted params
// and the recursion-separability gate all rely on nil being transparent
// there (tests is_not_none_narrow / minmax_3arg / expr_evaluator each
// regress if ->type itself is changed).
//
// Constants are deliberately NOT unstripped here: a raw single-element
// out can be a constant CS ("3" rather than int64) and partitioning on
// that is clone-per-constant (survey B5).
static AType *split_type_view(AVar *a, AType *filter) {
  AType *t = type_intersection(a->out->type, filter);
  if (t->n || !a->out->n) return t;
  for (CreationSet *c : a->out->sorted)
    if (!c->sym || c->sym->type != sym_nil_type) return t;  // constants etc: unchanged
  return type_intersection(a->out, filter);
}

static int edge_type_compatible_with_edge(AEdge *e, AEdge *ee, EntrySet *es, int fmark = 0) {
  assert(e->args.n && ee->args.n);
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *e_arg = e->args.get(p), *ee_arg = ee->args.get(p);
    if (!e_arg || !ee_arg) continue;
    AType *etype = split_type_view(e_arg, e->match->formal_filters.get(p));
    AType *eetype = split_type_view(ee_arg, ee->match->formal_filters.get(p));
    if (!fmark) {
      if (etype->n && eetype->n && etype != eetype) return ++ic_arg, 0;
    } else {
      AVar *es_arg = es->args.get(p);
      if (different_marked_args(ee_arg, e_arg, 2, es_arg)) return ++ic_arg, 0;
    }
  }
  if (e->rets.n != ee->rets.n) return ++ic_retn, 0;
  for (int i = 0; i < e->rets.n; i++) {
    if (ee->rets[i]->lvalue && e->rets.v[i]->lvalue) {
      if (!fmark) {
        if (ee->rets[i]->lvalue->out->type->n && e->rets.v[i]->lvalue->out->type->n &&
            ee->rets[i]->lvalue->out->type != e->rets.v[i]->lvalue->out->type)
          return ++ic_ret, 0;
      } else {
        if (different_marked_args(ee->rets[i]->lvalue, e->rets.v[i]->lvalue, 1, es->rets[i]->lvalue))
          return ++ic_ret, 0;
      }
    }
  }
  return 1;
}

static int typekey_enabled();
static int canon_enabled();

static int edge_type_compatible_with_entry_set(AEdge *e, EntrySet *es, int fmark = 0) {
  assert(e->args.n && es->args.n);
  if (!es->split) {
    for (MPosition *p : e->match->fun->positional_arg_positions) {
      AVar *es_arg = es->args.get(p), *e_arg = e->args.get(p);
      if (!e_arg) continue;
      AType *etype = split_type_view(e_arg, e->match->formal_filters.get(p));
      if (!fmark) {
        AType *stype = split_type_view(es_arg, nullptr);
        if (typekey_enabled() && es->type_key_pass >= 0) {
          AType *k = es->type_key.get(p);
          if (k) stype = k;  // durable key wins over the mid-pass value
        }
        if (etype->n && stype->n && etype != stype) return ++ic_arg, 0;
      } else if (different_marked_args(e_arg, es_arg, 2))
        return ++ic_arg, 0;
    }
    if (es->rets.n != e->rets.n) return ++ic_retn, 0;
    for (int i = 0; i < e->rets.n; i++) {
      if (es->rets[i]->lvalue && e->rets.v[i]->lvalue) {
        if (!fmark) {
          if (es->rets[i]->lvalue->out->type->n && e->rets.v[i]->lvalue->out->type->n &&
              es->rets[i]->lvalue->out->type != e->rets.v[i]->lvalue->out->type)
            return ++ic_ret, 0;
        } else if (different_marked_args(es->rets[i]->lvalue, e->rets.v[i]->lvalue, 1))
          return ++ic_ret, 0;
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

// ifa/issues/074 (detach-route reuse, PYC_HARDREUSE=2). A POSITIVE type
// match, as opposed to `edge_type_compatible_with_entry_set`'s "no
// conflict": that one only rejects when BOTH sides are non-empty
// (`etype->n && es_arg->out->type->n && ...`), so a contour whose
// argument types are unpopulated is trivially "compatible" -- issue
// 097's exact mechanism. On the detach route that is how mode 1 merged
// `defaultdict(int)` with `defaultdict(list)`: it reused an empty
// contour, which then accumulated both. Here every positional argument
// must be typed on BOTH sides and identical, and there must be at least
// one such argument, so reuse needs evidence rather than the absence of
// counter-evidence.
// `cs_granular` (PYC_HARDREUSE=3) compares the CreationSet sets rather
// than the type-level view. The type-level view is what defeats mode 2:
// `deep_copy_list`'s recursion levels are BOTH `list` (list-of-list vs
// list-of-int) and both defaultdicts are `defaultdict` -- type-identical
// at every positional argument, yet they must stay apart, which is
// precisely the separation the lexical display used to supply
// (issues/100). The CreationSets differ where the types do not.
static bool edge_type_identical_to_entry_set(AEdge *e, EntrySet *es, bool cs_granular) {
  if (!e->args.n || !es->args.n) return false;
  if (es->split) return false;  // a split product is characterized by its edges, not its own args
  if (es->rets.n != e->rets.n) return false;
  int matched = 0;
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *es_arg = es->args.get(p), *e_arg = e->args.get(p);
    if (!e_arg || !es_arg) return false;
    AType *eview = cs_granular ? e_arg->out : e_arg->out->type;
    AType *sview = cs_granular ? es_arg->out : es_arg->out->type;
    AType *etype = type_intersection(eview, e->match->formal_filters.get(p));
    if (!etype->n || !sview->n) return false;
    if (etype != sview) return false;
    matched++;
  }
  return matched > 0;
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

static int csm_enabled();
static int hard_reuse_enabled();

// Build `es`'s lexical display from the first edge that reaches it.
//
// The display exists for ONE consumer: make_AVar's resolution of a Var
// belonging to an enclosing scope (`es->display[nesting_depth - 1]`),
// i.e. genuine nested functions. It is deliberately NOT part of contour
// identity any more: the consistency assert that used to live here
// enforced "one lexical display per contour", which is what made
// `edge_nest_compatible_with_entry_set` reject an otherwise-perfect
// routing candidate and forced check_split's lineage branch to mint a
// fresh contour per recursion level (073's "sole unbounded EntrySet
// generator"; measured on yopyra at 34-68 new contours per pass,
// forever -- see ifa/issues/074). pyc gives every method a
// nesting_depth it does not need, so that identity constraint was
// mostly enforcing a *phantom* display (issue 064).
//
// A contour therefore keeps whatever display its first edge stamped,
// and a later edge with a different lexical display now shares the
// contour. The consequence is a precision loss, not unsoundness: an
// enclosing-scope Var resolves through the first stamp, so two callers'
// captured variables union rather than staying separate.
static void update_display(AEdge *e, EntrySet *es) {
  for (int i = es->display.n; i < es->fun->sym->nesting_depth; i++)
    if (i < e->from->display.n)
      es->display.add(e->from->display[i]);
    else
      es->display.add(e->from);
}

// ifa/issues/055: defined with the other contour-tracing helpers below.
static void dbg_atype_str(AType *t, char *buf, int n, int depth);

// ifa/issues/055: PYC_DBG_BIND=<fun name> logs every edge->EntrySet
// binding for that function -- MINT vs REUSE, the chosen ES, and the
// edge's ACTUAL argument types. set_entry_set is the one chokepoint
// both routes go through, so nothing can bind behind its back. This is
// what identifies the caller that re-admits a second receiver
// CreationSet into an already-monomorphic contour.
static void dbg_bind(AEdge *e, EntrySet *new_es, bool mint) {
  static cchar *want = nullptr;
  static int checked = 0;
  if (!checked) { want = getenv("PYC_DBG_BIND"); checked = 1; }
  if (!want || !e || !e->match || !e->match->fun || !e->match->fun->sym) return;
  cchar *nm = e->match->fun->sym->name;
  if (!nm || strcmp(nm, want)) return;
  char args[512];
  args[0] = 0;
  int used = 0;
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *av = e->args.get(p);
    char t[160];
    dbg_atype_str(av ? av->out : nullptr, t, (int)sizeof t, 0);
    used += snprintf(args + used, (int)sizeof args - used, "%s%s", used ? " " : "", t);
    if (used >= (int)sizeof args - 1) break;
  }
  fprintf(stderr, "BIND pass=%d %s e=%d %s es=%d from_es=%d line=%d actuals=[%s]\n", analysis_pass, want, e->id,
          mint ? "MINT " : "REUSE", new_es ? new_es->id : -1, e->from ? e->from->id : -1,
          e->pnode && e->pnode->code ? e->pnode->code->line() : -1, args);
}

static void set_entry_set(AEdge *e, EntrySet *es = 0) {
  EntrySet *new_es = es;
  if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) {
    if (es) ++fa->dbg_stage_reuse[cur_split_stage]; else ++fa->dbg_stage_mint[cur_split_stage];
  }
  if (!es) {
    new_es = new EntrySet(e->match->fun);
    e->match->fun->ess.add(new_es);
  }
  if (e->to && e->to != new_es) fa_pass_retargeted.set_add(e->to);  // ifa/111 M1: old target
  if (new_es) fa_pass_retargeted.set_add(new_es);                   // ifa/111 M1: new target
  dbg_bind(e, new_es, es == nullptr);
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

static int entry_set_compatibility(AEdge *e, EntrySet *es) {
  int val = INT_MAX;
  if (e->match->fun->split_unique) return 0;
  // ifa/issues/075: `es->filters` (the restriction
  // find_or_make_filtered_entry_set applies -- e.g. CSM's "only this
  // CreationSet") was otherwise invisible here: a candidate whose
  // filters have ZERO type-overlap with the edge's actual arguments
  // could still be selected, purely on accumulated-type score.
  // find_best_entry_sets (the caller) is the general-purpose fallback
  // for "where does an edge with unresolved ->to go" -- reachable any
  // time a route gets nulled (redispatch, apply_entry_set_split, CSM's
  // own machinery) -- and had none of check_edge's filter awareness
  // that check_split's recursive-knot reuse already applies a few
  // lines below. Genuine correctness fix, verified zero-regression
  // (PYC_CSM on or off) -- but NOT sufficient on its own to stop a
  // filtered product from being outscored by the wider, unfiltered
  // original it was split FROM: this only rejects a ZERO-overlap
  // candidate, and a still-union-typed edge has non-empty overlap with
  // EITHER a CS-partitioned sibling OR the unfiltered original (which
  // has no filter to conflict with at all), so this gate never fires
  // for that case and the SCORING (unchanged) still picks the
  // original's exact type match over a sibling's partial one. Traced
  // and confirmed on sha.py: this fix does not change its outcome
  // (still fails to converge) -- see the issue doc's "Filter-aware
  // attempt" update for the full trace and why a further scoring
  // tweak was considered and rejected as likely to relocate rather
  // than fix that specific case.
  if (!check_edge(e, es)) return 0;
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

// ifa/issues/075: when an edge's actual argument type at some position
// is itself a union spanning >=2 EntrySets that are each filtered
// (find_or_make_filtered_entry_set) to a DIFFERENT, non-overlapping
// CreationSet-based AType at that position -- genuine CS-partition
// siblings, e.g. CSM's element-CS split products -- binding the edge
// to a SINGLE "best" target (find_best_entry_sets' only mode, below)
// necessarily widens THAT target's accumulated type to include the
// whole union, undoing the partition; the sibling that loses the
// score contest never sees this edge at all. Confirmed on sha.py this
// is what silently undoes CSM's split every time a caller reproduces
// the wide-typed argument (see the issue doc's "root cause" trail).
//
// Fan the edge out across every sibling it overlaps instead, mirroring
// split_edges' own redispatch+copy_AEdge shape (set_or_copy_AEdge,
// just above, already implements "first bind, rest copy" -- unused
// for more than one target until now). Only fires when the candidate
// siblings' filters JOINTLY COVER the edge's whole effective type at
// that position: a partial match (some CS in the union has no
// matching filtered sibling) falls through to the single-best logic
// unchanged, since fanning across an incomplete partition would drop
// the uncovered remainder's representation entirely -- worse than not
// fanning. entry_set_compatibility (already filter-aware) is reused
// per candidate, so nest/type/sset/constant compatibility are all
// still enforced exactly as the single-best path enforces them.
static bool find_fanout_entry_sets(AEdge *e, Vec<AEdge *> &edges) {
  form_MPositionAVar(pa, e->args) {
    if (!pa->key->is_positional()) continue;
    AVar *a = pa->value;
    if (!a || !a->out) continue;
    MPosition *p = pa->key;
    AType *filter = e->match->formal_filters.get(p);
    AType *eff = filter ? type_intersection(a->out, filter) : a->out;
    if (eff->sorted.n < 2) continue;
    Vec<EntrySet *> cands;
    AType *covered = fa->type_world.bottom_type;
    for (EntrySet *x : e->match->fun->ess) {
      AType *es_filter = x->filters.get(p);
      if (!es_filter) continue;  // only genuine CS-partition siblings
      AType *ov = type_intersection(eff, es_filter);
      if (ov == fa->type_world.bottom_type) continue;
      if (entry_set_compatibility(e, x) <= 0) continue;
      cands.add(x);
      covered = type_union(covered, ov);
    }
    if (cands.n < 2 || covered != eff) continue;
    qsort_by_id(cands);
    for (int i = 0; i < cands.n; i++) set_or_copy_AEdge(e, cands[i], edges);
    return true;
  }
  return false;
}

static int find_best_entry_sets(AEdge *e, Vec<AEdge *> &edges) {
  if (find_fanout_entry_sets(e, edges)) return 1;
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
        // A candidate is only a routing target if it currently HAS a
        // target. `out_edge_map` is not a set of live, bound edges: it
        // also holds edges get_AEdges minted but dispatch never bound,
        // and — the case that bites here — the edges of a group
        // apply_entry_set_split has just detached (`x->to = 0` for the
        // whole group, then re-bind one at a time), which are visible
        // in this map for the duration of that second loop. Both
        // deref null below (`ee->to->filters` inside check_edge,
        // `ee->match->fun`). Latent since the split-lineage path was
        // written; exposed by ifa/issues/098's reset fix shifting
        // dijkstra's contour trajectory onto it.
        if (!ee->to || !ee->match) continue;
        if (ee->to == avoid) continue;
        if (!check_edge(e, ee->to)) continue;
        if (ee->match->fun == e->match->fun) {
          if (e->match->fun->split_unique) {
            // Issue 073: this split-lineage mint is the sole unbounded
            // EntrySet generator (measured: ~all divergence-time mints on
            // adatron/057/plcfrs came from here). It fires once per
            // recursive invocation because the candidate `ee->to` carries
            // the split-PARENT's display while `e->from` is the CHILD, so
            // `edge_nest_compatible` fails by construction every level, and
            // each mint links a new `->split` — an unbounded call-context
            // chain that bypasses the ordinary `(type x data)` dedup.
            //
            // On the normal (flow-time) call path, tie the recursive knot
            // by *exact type identity* instead: if an existing contour of
            // this fun is a HARD type match (edge_type_compatible == 1) and
            // nest-compatible (so update_display won't assert and no
            // type-different contours merge — a soft find_best_entry_sets
            // reuse would, regressing match_seq), reuse it; recursion of
            // same-depth methods shares one display, so once its arg types
            // stabilize (finite type domain) such a sibling exists and the
            // knot ties. A genuinely new monomorphic arg-type tuple finds no
            // hard match and still mints its own contour. `split_unique`
            // keeps its forced-fresh contract; a split-detach (`avoid`)
            // keeps the lineage mint (find_best is skipped there anyway).
            if (!avoid && !e->match->fun->split_unique) {
              EntrySet *knot = nullptr;
              for (EntrySet *x : e->match->fun->ess)
                if (x && edge_type_compatible_with_entry_set(e, x) == 1)
                  if (!knot || x->id < knot->id) knot = x;  // deterministic (issue 035)
              if (knot) {
                set_or_copy_AEdge(e, knot, ees);
                return 1;
              }
            }
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

// ifa/issues/074 canonicalization stats (IFA_DBG_CANON).
static long canon_hit = 0, canon_miss = 0, canon_conflict = 0, canon_conflict_honored = 0;

// This edge's type tuple: the filtered actual type at each positional
// argument. Canonical ATypes are hash-consed for the life of the FA, so
// these pointers compare by identity and stay valid across passes.
static bool edge_canon_key(AEdge *e, Map<MPosition *, AType *> &key) {
  if (!e->args.n) return false;
  int n = 0;
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *a = e->args.get(p);
    if (!a) continue;
    AType *t = type_intersection(a->out->type, e->match->formal_filters.get(p));
    if (!t->n) return false;  // no evidence yet -- do not canonicalize on it
    key.put(p, t);
    n++;
  }
  return n > 0;
}

static bool same_canon_key(Map<MPosition *, AType *> &a, Map<MPosition *, AType *> &b) {
  int na = 0, nb = 0;
  form_MPositionAType(x, a) if (x->key) {
    na++;
    if (b.get(x->key) != x->value) return false;
  }
  form_MPositionAType(x, b) if (x->key) nb++;
  return na && na == nb;
}

// Find the contour of this edge's callee already named by `key`.
static EntrySet *find_canonical_entry_set(AEdge *e, Map<MPosition *, AType *> &key) {
  EntrySet *found = nullptr;
  for (EntrySet *x : e->match->fun->ess)
    if (x && x->canon_key_set && same_canon_key(key, x->canon_key))
      if (!found || x->id < found->id) found = x;  // deterministic (issue 035)
  return found;
}

// ifa/issues/101: what DISCRIMINATOR the split currently being applied
// used. Hard reuse matches candidate contours on argument types, so it is
// only sound evidence when types are what the split separated on -- a
// setter- or mark-driven split can produce two contours with identical
// argument types on purpose, and reusing across them undoes it. Set
// around the split route's make_entry_set call; 0 outside.
static int cur_split_type_only = 0;

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
  } else if (hard_reuse_enabled()) {
    EntrySet *hard = nullptr;
    // Mode 5: mode 4, but only when the split's own discriminator was
    // argument types. See cur_split_type_only.
    if (hard_reuse_enabled() >= 4 && !(hard_reuse_enabled() >= 5 && !cur_split_type_only)) {
      // Route by the contour's durable type key: find the contour whose
      // recorded (converged, previous-pass) formal types EQUAL this
      // edge's filtered actuals. A lookup, not a score -- so the answer
      // does not depend on when in the pass it is asked, and there is no
      // symmetric "anything but `split`" choice to ping-pong between.
      for (EntrySet *x : e->match->fun->ess) {
        if (!x || x == split || x->type_key_pass < 0 || !x->args.n) continue;
        bool all = true;
        int matched = 0;
        for (MPosition *p : e->match->fun->positional_arg_positions) {
          AVar *e_arg = e->args.get(p);
          if (!e_arg) continue;
          AType *k = x->type_key.get(p);
          AType *etype = type_intersection(e_arg->out->type, e->match->formal_filters.get(p));
          if (!k || !etype->n || k != etype) { all = false; break; }
          matched++;
        }
        if (all && matched && (!hard || x->id < hard->id)) hard = x;
      }
      if (hard) {
        if (getenv("IFA_DBG_HARDREUSE"))
          fprintf(stderr, "HARDREUSE4 p=%d fun=%s#%d e=%d split=es%d -> es%d (fun has %d ess)\n", analysis_pass,
                  e->match->fun->sym && e->match->fun->sym->name ? e->match->fun->sym->name : "?",
                  e->match->fun->sym ? e->match->fun->sym->id : -1, e->id, split ? split->id : -1, hard->id,
                  e->match->fun->ess.n);
        set_or_copy_AEdge(e, hard, edges);
        return;
      }
      hard = nullptr;
    }
    for (EntrySet *x : e->match->fun->ess)
      // `fun->ess` on this route also holds BARE products (filters +
      // split lineage only; set_entry_set is what populates args/rets),
      // which entry_set_compatibility asserts on
      // (`edge_type_compatible_with_entry_set`'s `assert(e->args.n &&
      // es->args.n)` -- kanoodle aborts without this). The flow-time
      // caller never sees them, so the assert has always held there.
      if (hard_reuse_enabled() < 4 && x && x != split && x->args.n && e->args.n &&
          entry_set_compatibility(e, x) == INT_MAX &&
          (hard_reuse_enabled() < 2 ||
           edge_type_identical_to_entry_set(e, x, hard_reuse_enabled() >= 3)))
        if (!hard || x->id < hard->id) hard = x;  // deterministic (issue 035)
    if (hard) {
      if (getenv("IFA_DBG_HARDREUSE"))
        fprintf(stderr, "HARDREUSE p=%d fun=%s#%d e=%d from=es%d split=es%d -> es%d (fun has %d ess)\n",
                analysis_pass, e->match->fun->sym && e->match->fun->sym->name ? e->match->fun->sym->name : "?",
                e->match->fun->sym ? e->match->fun->sym->id : -1, e->id, e->from ? e->from->id : -1,
                split ? split->id : -1, hard->id, e->match->fun->ess.n);
      set_or_copy_AEdge(e, hard, edges);
      return;
    }
  }
  if (!es) es = preference;
  Map<MPosition *, AType *> ckey;
  bool have_key = canon_enabled() && !e->match->fun->split_unique && edge_canon_key(e, ckey);
  if (have_key && !es) {
    EntrySet *canon = find_canonical_entry_set(e, ckey);
    if (canon && canon == split) {
      // The canonical home for this edge's types IS the contour the
      // splitter is detaching it from: the split is asking for a
      // separation the type tuple says does not exist. This is the
      // measurement the canonicalization exists to produce.
      ++canon_conflict;
      if (getenv("IFA_DBG_CANON"))
        fprintf(stderr, "CANON-CONFLICT p=%d fun=%s#%d e=%d split=es%d (fun has %d ess)\n", analysis_pass,
                e->match->fun->sym && e->match->fun->sym->name ? e->match->fun->sym->name : "?",
                e->match->fun->sym ? e->match->fun->sym->id : -1, e->id, split->id, e->match->fun->ess.n);
      if (canon_enabled() >= 2) es = canon; else ++canon_conflict_honored;
    } else if (canon) {
      ++canon_hit;
      es = canon;
    } else
      ++canon_miss;
  }
  set_entry_set(e, es);
  if (have_key && !e->to->canon_key_set) {
    e->to->canon_key.copy(ckey);
    e->to->canon_key_set = 1;
  }
  if (!es) {
    e->to->split = split;
    // ifa/issues/075: this is the "leftover group" mint -- an edge
    // being detached FROM `split` that neither the ledger nor a
    // preference could route, so a fresh ES is minted for it (and
    // whatever other leftovers this same detach's later edges share
    // `e->to` as their own `preference`, above). Without this, the
    // fresh ES defaults to EntrySet's ctor: no filters at all, even
    // though every edge landing here is, by construction, still an
    // edge of `split` -- so if `split` itself was filtered (e.g. one
    // of CSM's element-CS products), this silently DISCARDS that
    // restriction, recreating an unfiltered catch-all that reabsorbs
    // whatever union `split`'s own filter was keeping apart. Confirmed
    // as the actual seed of sha.py's non-termination: traced split
    // with filters.n==1 minting a leftover ES with filters.n==0,
    // which CSM then finds "divergent" and re-splits every pass,
    // forever (see the issue doc's "structural leak" update for the
    // full trace). Copying split's filters here is the direct fix --
    // this leftover home is still a subset of split's own scope, so
    // inheriting its restriction is always correct, never a widening.
    if (split) e->to->filters.copy(split->filters);
  }
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
  CreationSet *cs = creation_point(container, kind);
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
    // ifa/issues/104: deliberately NO flow into the container's generic
    // element here. `list` does not do it either, and that is exactly why
    // a heterogeneous list read only by constant indices keeps precise
    // per-field types: its element stays BOTTOM, which is what
    // tuple_able() tests for. The element is populated on USE -- dynamic
    // indexing, iteration, append -- not on construction. An earlier
    // version of PYC_TUPELEM added the flow here and broke 11 tests by
    // leaking a heterogeneous tuple's field union into its constant-index
    // reads. Monomorphicity is asked of `cs->vars`, not of the element.
    if (iv->var->sym->name) cs->var_map.put(iv->var->sym->name, iv);
  }
}

// ifa/issues/109: record a violation when sizeof_element's receiver spans
// CreationSets that cannot share one concrete container type.
static int sizeof_viol_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_SIZEOF_VIOL");
    e = v ? atoi(v) : 0;
  }
  return e;
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
  CreationSet *cs = creation_point(result, sym_closure);
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
  CreationSet *cs = creation_point(result, sym_closure);
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
          // issues/114: a GENERATOR's return may never be a singleton
          // constant, however certain FA is of the value.
          //
          // For every other function, "FA proved the return is 5" and
          // "the C call produces 5" are the same statement. For a
          // generator they are not: the backends discard the emitted
          // return entirely and hand back the coroutine handle instead
          // (cg.cc's `return (%s)(uintptr_t)__g_1014.handle.address()`,
          // and cg_emit_llvm.cc's counterpart), while fn->ret carries
          // whatever the body's yields and returns put there -- a type
          // channel, not a value. So a body that yields ONE constant
          // (`yield 1`, then a raise or a fall-through) looked like a
          // function certain to return 1, and dead.cc's get_constant
          // let every consumer inline that literal in place of the
          // handle: the generator object was built around the address
          // 1 and the first resume segfaulted, with nothing visibly
          // wrong in the emitted C.
          //
          // Unioning in the constant's own abstract type is enough --
          // two distinct CreationSets is exactly what get_constant
          // refuses to fold -- and it costs nothing else: the widened
          // arm is the type the constant already had. Done here rather
          // than at the fold because SSU gives the call's result and
          // each later use separate Vars AND separate Syms, so there is
          // no single downstream thing to exempt; the type is.
          //
          // Same root as issues/022's P_prim_await liveness exception:
          // a coroutine handle is not a value the optimizer may reason
          // about through its contents.
          if (es->fun->sym->is_generator)
            for (CreationSet *cs : r->out->sorted)
              if (cs->sym->is_constant && cs->sym->type)
                update_gen(es->rets[i - 3], make_abstract_type(cs->sym->type));
        }
        break;
      case P_prim_make:
        prim_make_constraints(p, es);
        break;
      case P_prim_make_seq: {
        // issues/110: make_seq(kind, src) -- a container of `kind` with
        // NO fixed arity, whose generic element is seeded from `src`'s
        // element. This is prim_make's dynamic-length counterpart:
        // make_kind fills cs->vars one per argument, which names a fixed
        // arity; here there are no per-index vars at all, only the
        // element. Populating it is exactly what makes tuple_able()
        // false, so clone.cc gives the CreationSet LIST LAYOUT.
        AVar *container = make_AVar(p->lvals[0], es);
        Sym *kind = p->rvals[2]->sym;
        AVar *src = make_AVar(p->rvals[3], es);
        CreationSet *cs = creation_point(container, kind);
        cs->no_static_arity = 1;
        AVar *elem = get_element_avar(cs);
        if (elem) {
          // Every element type the source can yield flows into ours.
          //
          // EVERY source AVar goes through vector_elems, including the
          // source's own generic element: both it and the per-index vars
          // are CS-contoured, and a raw CS -> CS flow edge puts a
          // CS-contoured var in elem->backward, which is exactly what
          // compute_setters asserts against (`x->contour_is_entry_set`;
          // measured: genetic2_idioms aborts the compiler). vector_elems
          // lands each value in a fresh entry-set-contoured tval of this
          // pnode first, so every edge reaching elem starts at an ES var.
          // src->out is a per-pass snapshot like everything else, and a
          // single pass where it reads empty would discard every element
          // edge built below (measured: correct on pass 3, empty on pass
          // 4 -- the last -- so the element ended bottom). Remember the
          // last NON-EMPTY set on the CreationSet and drive the loop
          // from that. Sound over-approximation: the element is a union,
          // so keeping a source that has genuinely gone away can only
          // widen it, never drop a type that is still live.
          if (src->out->sorted.n) {
            cs->seq_src.clear();
            for (CreationSet *scs : src->out->sorted) cs->seq_src.add(scs);
          }
          int slot = 0;
          for (CreationSet *scs : cs->seq_src) {
            AVar *selem = get_element_avar(scs);
            if (selem) vector_elems(0, p, selem, elem, container, slot++);
            // A LITERAL source has a bottom generic element -- that is
            // the tuple_able design: `make` fills per-index vars and
            // leaves the element unpopulated, so it stays record-shaped
            // until something uses it generically. `tuple([1,2,3])` must
            // still get an element type, so take it from the per-index
            // vars.
            //
            // vector_elems, NOT a bare update_gen: these AVars are
            // CS-contoured, and a flow EDGE straight from one into an
            // element var trips compute_setters' `contour_is_entry_set`
            // assertion. vector_elems is the sanctioned trampoline for
            // exactly that -- it lands the value in a fresh entry-set-
            // contoured tval of this pnode and flows THAT into elem.
            //
            // A durable edge is REQUIRED, not a nicety. update_gen takes
            // a SNAPSHOT of fv->out, and nothing orders this constraint
            // after the source literal's own `make` within a pass, nor
            // re-runs it when the source is repopulated after the
            // per-pass clear_avar. Measured on a recursive class with a
            // {None, tuple} field: the source var read SET on pass 3 and
            // bottom again on pass 4, the LAST pass, so the element
            // finished bottom, the CS stayed tuple_able, and clone.cc
            // gave it RECORD layout with ZERO members -- the empty
            // record cg.cc reports as "runtime error: bad getter".
            // arg_of_send alone does not fix it: the source var never
            // changes during the final pass, so nothing re-enqueues.
            for (int i = 0; i < scs->vars.n; i++)
              if (scs->vars[i]) vector_elems(0, p, scs->vars[i], elem, container, slot++);
          }
        }
        break;
      }
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
    // ifa/issues/055: the loop is over the callee's FORMALS but indexes
    // the caller's ACTUALS, and the two need not agree -- a callee with
    // default parameters is legitimately reached by a call that supplies
    // fewer (measured on kmeanspp: `__eq__` with has.n=5 entered with
    // a.n=3, so a[3] read past the end and record_arg dereferenced null).
    // A formal with no actual has nothing to record; the default wrapper
    // is what supplies it. Latent -- it needs a dispatch that reaches
    // such a callee directly, which is why it only surfaced once
    // PYC_PROMOTE_FIRST changed which paths are reached.
    for (int i = 0; i < e->fun->sym->has.n && i < a.n; i++) {
      if (!a.v[i]) { p.inc(); continue; }
      record_arg(e->pnode, 0, a.v[i], e->fun->sym->has.v[i], e, p);
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
  ATypeViolation *nv = new ATypeViolation(akind, av, send);
  ATypeViolation *v = fa->type_world.type_violation_hash.put(nv);
  // ChainHash::put returns null when `nv` is newly pushed into an
  // ALREADY-EXISTING bucket (a hash collision between two distinct
  // (kind, av, send) triples, no equal entry found) -- in that case
  // `nv` itself is the canonical entry, exactly as type_cannonicalize
  // handles the same `if (!tt) tt = t;` idiom. Without this guard the
  // `v->type` read below segfaults on the first hash collision among
  // type violations (found via shedskin adatron).
  if (!v) v = nv;
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
// elide_source (issue 078, Option D): the raw Sym operand `cs`'s value
// came from, if the caller can identify one -- P_prim_clone passes the
// literal Sym referenced at its clone-source operand (p->rvals[o]->sym).
// When that Sym is a class's own prototype (cls->self), it may carry a
// non-empty clone_elides_fields (set by gen_class_pyda when the class's
// OWN __init__ unconditionally overwrites the field -- see issue 078)
// naming fields safe to skip copying into new_cs: the prototype Sym is
// never reachable from Python source, so this can only be non-null and
// non-empty for the __new__-synthesized clone(proto, t), never a user
// clone() call on some other, already-constructed instance -- checking
// cs->sym here instead would be wrong, since cs->sym is the CLASS Sym,
// identical for the prototype and for every other instance of the same
// class.
static void structural_assignment(CreationSet *new_cs, CreationSet *cs, PNode *p, EntrySet *es, bool merge = false,
                                  bool mix = false, Sym *elide_source = nullptr) {
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
    // issue 078 (Option D): skip crediting `niv` with this field's
    // clone-time copy when elide_source (the clone's literal source
    // operand Sym, e.g. a class's own prototype) says its class's
    // OWN __init__ unconditionally overwrites this field before any
    // instance built this way is observable. Purely a type-precision
    // decision -- doesn't change what runtime code clone() emits
    // (cg.cc's P_prim_clone is a single whole-struct
    // _CG_prim_clone_dst call, not per-field, so the real value is
    // still physically copied at runtime regardless).
    if (!elide_source || !elide_source->clone_elides_fields.n ||
        !elide_source->clone_elides_fields.in(if1_cannonicalize_string(if1, h->name)))
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
            // ifa/issues/081: nt can be an empty AType here -- e.g. one
            // operand's type falls outside fa->type_world.anynum_kind
            // (a bool operand, unless the frontend opted bool into the
            // numeric lattice via IFACallbacks::bool_is_numeric) -- in
            // which case nt->v[0] would index off the end of an empty
            // Vec. Salvage the same way the other "can't fully fold"
            // cases in this function already do, rather than crash.
            if (!nt->n)
              update_in(res, nt);
            else if (!fold_constant(p->prim->index, &a->out->v[0]->sym->imm, &b->out->v[0]->sym->imm, &imm))
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
            // ifa/issues/081: see the binary-op case above -- nt can be
            // empty here too (e.g. an un-opted-in bool operand).
            if (!nt->n)
              update_in(res, nt);
            else if (!fold_constant(p->prim->index, &a->out->v[0]->sym->imm, 0, &imm))
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
          else if (sym_bytes->specializers.set_in(cs->sym))
            // bytes shares str's exact char* buffer layout (see
            // sym_bytes registration) but indexes/iterates to plain int,
            // matching CPython's `bytes[i]` -- unlike sym_char, which is
            // aliased to sym_string (python_ifa_sym.cc), sym_int is
            // aliased to sym_int64, a genuine scalar.
            update_gen(result, sym_int->abstract_type);
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
        // issue 078 (Option D): the literal Sym referenced at the
        // clone-source operand -- for the __new__-synthesized
        // clone(proto, t), this is always cls->self, the class's own
        // prototype; for any other clone() call it's whatever Sym the
        // source expression resolves to (never a class prototype, per
        // structural_assignment's comment above). Passed through so
        // the per-field copy can consult its clone_elides_fields.
        Sym *clone_source_sym = p->rvals[o]->sym;
        for (CreationSet *cs : thing->out->sorted) {
          CreationSet *new_cs = creation_point(result, cs->sym);
          structural_assignment(new_cs, cs, p, es, false, false, clone_source_sym);
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
          // issues/114: a CreationSet with NO DEFS was not built by any
          // creation site in this program -- it is abstract, or
          // synthesised for the result of an opaque `__pyc_c_call__`
          // (a generator's value channel is exactly that). Its
          // `vars.n` is 0 because nothing ever filled it, NOT because
          // the container is empty, so folding len() to 0 is simply
          // wrong. Measured: a tuple arriving through a generator had
          // correct contents -- `len(x)` and `x[0]` were right at the
          // use site -- but inside tuple::__eq__, whose formal carries
          // the synthesised CS, `len(self)` folded to 0, so the very
          // first `n != len(t)` check returned False and `x == (1, 2)`
          // was silently False for a tuple that WAS (1, 2).
          if (cs->no_static_arity || !cs->defs.n || (elem && elem->out != fa->type_world.bottom_type) ||
              sym_string->specializers.set_in(cs->sym) || sym_bytes->specializers.set_in(cs->sym))
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
        // ifa/issues/109: sizeof_element needs ONE container layout. If
        // the receiver spans CreationSets that will become distinct
        // concrete types -- different sym, or different arity, which for
        // a record-shaped tuple means a different type -- their sum has
        // no `element` and codegen fails with "sizeof_element of
        // non-container type". FA used to just union the sizes and say
        // nothing, so the splitter had no reason to separate them.
        //
        // Recording a violation here is what makes the EXISTING backward
        // machinery do the work: split_for_violations (stage 5) splits
        // the offending AVar, and that split propagates back to the
        // caller's contour automatically. No annotation, no special case
        // in codegen -- FA simply has to know the constraint.
        if (sizeof_viol_enabled() && t->out->sorted.n > 1) {
          Sym *sym0 = nullptr;
          int arity0 = -1;
          bool uniform = true;
          for (CreationSet *cs : t->out->sorted) {
            if (!cs->sym) continue;
            if (!sym0) { sym0 = cs->sym; arity0 = cs->vars.n; }
            else if (sym0 != cs->sym || arity0 != cs->vars.n) { uniform = false; break; }
          }
          if (!uniform) type_violation(ATypeViolation_kind::BOXING, t, t->out, nullptr, nullptr);
        }
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
        // ifa/issues/050 3b stage 1: first refusal on a load from a
        // mutable global cell. See IFACallbacks::provably_constant_load.
        // Skipping the flow below is the point, not a side effect: it is
        // what leaves the cell with no consumer, which is what makes a
        // write-only cell unobservable to the BOXING check.
        if (if1->callback) {
          if (AType *forced = if1->callback->provably_constant_load(rhs, es, p)) {
            update_gen(lhs, forced);
            continue;
          }
        }
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
      if (e != fa->type_world.bottom_type) {
        AVar *if_send = new AVar(*cond);
        Var *vif = new Var(*cond->var);
        vif->def = p;
        if_send->var = vif;
        type_violation(ATypeViolation_kind::PRIMITIVE_ARGUMENT, cond, e, if_send);
      }
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
      //  (b) call to the isinstance/is-None/is-not-None wrapper,
      //      recognized by callee sym name matching whatever
      //      IFACallbacks::narrowing_isinstance_name() /
      //      narrowing_is_none_name() / narrowing_is_not_none_name()
      //      (ifa.h) the frontend supplies -- nullptr (ifa's own
      //      default) means this frontend has none, so nothing here
      //      matches. ifa/issues/082.
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
          // ifa/issues/082: the specific names recognized here are a
          // frontend policy (IFACallbacks::narrowing_isinstance_name /
          // narrowing_is_none_name / narrowing_is_not_none_name,
          // ifa.h), not baked into ifa's own generic FA -- a frontend
          // that doesn't override them (nullptr default) gets none of
          // this narrowing, so an unrelated same-named function is
          // never misinterpreted.
          cchar *isinstance_name = if1->callback->narrowing_isinstance_name();
          cchar *is_none_name = if1->callback->narrowing_is_none_name();
          cchar *is_not_none_name = if1->callback->narrowing_is_not_none_name();
          if (fname && isinstance_name && !strcmp(fname, isinstance_name) &&
              iso_def->rvals.n >= 3) {
            operand_var = iso_def->rvals[1];
            type_var = iso_def->rvals[2];
          } else if (fname &&
                     ((is_none_name && !strcmp(fname, is_none_name)) ||
                      (is_not_none_name && !strcmp(fname, is_not_none_name))) &&
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
              if (is_none_name && !strcmp(fname, is_none_name)) is_none_check = true;
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

// ifa/issues/041, third hypothesis: the dump is supposed to be read-only,
// but make_AVar/unique_AVar CREATE on miss -- so a -v run mutates FA
// state at a pass boundary, in the middle of iterating structures the
// allocation can touch. Count how often that actually happens before
// changing anything: IFA_DBG_DUMPALLOC reports creations per dump.
static void *fa_dump_contour_for(Var *v, EntrySet *es) {
  if (v->sym->nesting_depth) {
    if (v->sym->nesting_depth != es->fun->sym->nesting_depth + 1) {
      int i = v->sym->nesting_depth - 1;
      if (i >= es->display.n) return nullptr;  // caller has already skipped these
      return (void *)es->display.v[i];
    }
    return (void *)es;
  }
  if (v->is_internal) return (void *)es;
  return GLOBAL_CONTOUR;
}

void fa_dump_types(FA *fa, FILE *fp) {
  Vec<Var *> gvars;
  int dump_creates = 0, dump_finds = 0;
  bool dump_alloc_dbg = getenv("IFA_DBG_DUMPALLOC") != nullptr;
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
      // make_AVar reads es->display[nesting_depth - 1] for a Var
      // belonging to an ENCLOSING scope, and collect_Vars hands back
      // every Var the Fun mentions regardless of whether THIS EntrySet's
      // display covers it. Out of bounds there returns garbage that
      // unique_AVar dereferences as a contour -- a real and documented
      // SIGSEGV family here (see the note at
      // find_or_make_filtered_entry_set: pyc issue 025's
      // pystone/tictactoe/amaze/othello/score4/voronoi2 crashes).
      //
      // Investigated as the cause of ifa/issues/041's intermittent -v
      // crash and MEASURED NOT TO FIRE on either of that issue's two
      // sighting inputs (bh, pygasus: zero skips under
      // IFA_DBG_DUMPSKIP). So this is hardening, not that fix: an
      // unchecked index into a Vec, on a path where the consequence is
      // known to be a wild dereference, is worth a bound test whether or
      // not it is the bug someone is currently chasing.
      int depth = v->sym->nesting_depth;
      if (depth != f->sym->nesting_depth + 1 && (depth - 1) >= es->display.n) {
        if (getenv("IFA_DBG_DUMPSKIP"))
          fprintf(stderr, "[dumpskip] fun %s var %s depth=%d display=%d\n",
                  f->sym->name ? f->sym->name : "?", v->sym->name ? v->sym->name : "?", depth, es->display.n);
        continue;
      }
      // ifa/issues/041: LOOK UP, never create. make_AVar allocates on
      // miss, so the dump was mutating FA state at a pass boundary --
      // measured on bh: 27, 82 and 8 AVars created in the first three
      // dumps, and `-v` ended the run with total_ess 415 vs 414 and
      // total_css 1578 vs 1577 without it. The verbose dump was
      // perturbing the analysis it exists to measure, which matters more
      // than the crash it was being investigated for: every -v pass
      // trajectory used to verify a change was reading a slightly
      // different analysis than the real compile.
      //
      // A (Var, contour) pair with no AVar was never reached by the
      // analysis, so it has no type to show; skipping it loses nothing.
      void *c = fa_dump_contour_for(v, es);
      AVar *av = c ? v->avars.get(c) : nullptr;
      if (dump_alloc_dbg) {
        if (!av) ++dump_creates;
        else ++dump_finds;
      }
      if (!av) continue;
      fa_dump_var_types(av, fp);
    }
  }
  gvars.set_to_vec();
  fprintf(fp, "globals\n");
  for (Var *v : gvars) if (!v->sym->is_constant && !v->sym->is_symbol) {
      AVar *gav = (AVar *)v->avars.get(GLOBAL_CONTOUR);
      if (dump_alloc_dbg) {
        if (!gav) ++dump_creates;
        else ++dump_finds;
      }
      if (gav) fa_dump_var_types(gav, fp);
  }
  if (dump_alloc_dbg)
    fprintf(stderr, "[dumpalloc] pass creates=%d finds=%d\n", dump_creates, dump_finds);
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

static bool extract_ast_loc(IFAAST *ast, cchar **out_filename, int *out_line, int *out_col) {
  if (!ast) return false;
  int sl = ast->source_line();
  if (sl > 0 && ast->pathname()) {
    *out_filename = ast->pathname();
    *out_line = sl;
    *out_col = ast->column();
    return true;
  }
  return false;
}

static bool extract_pnode_loc(PNode *p, cchar **out_filename, int *out_line, int *out_col, int depth = 0) {
  if (!p || depth > 5) return false;
  if (p->code && extract_ast_loc(p->code->ast, out_filename, out_line, out_col)) return true;

  for (PNode *pred : p->cfg_pred) {
    if (pred && pred->code && extract_ast_loc(pred->code->ast, out_filename, out_line, out_col)) return true;
  }

  for (PNode *prev : p->phy) {
    if (extract_pnode_loc(prev, out_filename, out_line, out_col, depth + 1)) return true;
  }

  for (Var *v : p->rvals) {
    if (!v) continue;
    if (v->def && v->def != p && extract_pnode_loc(v->def, out_filename, out_line, out_col, depth + 1)) return true;
  }
  for (Var *v : p->lvals) {
    if (!v) continue;
    if (v->def && v->def != p && extract_pnode_loc(v->def, out_filename, out_line, out_col, depth + 1)) return true;
  }

  for (Var *v : p->rvals) {
    if (!v) continue;
    if (v->sym && extract_ast_loc(v->sym->ast, out_filename, out_line, out_col)) return true;
  }
  for (Var *v : p->lvals) {
    if (!v) continue;
    if (v->sym && extract_ast_loc(v->sym->ast, out_filename, out_line, out_col)) return true;
  }

  return false;
}

static bool extract_contour_user_loc(EntrySet *es, cchar **out_filename, int *out_line, int *out_col, int depth = 0) {
  if (!es || depth > 10) return false;

  for (AEdge *e : es->edges) {
    if (!e) continue;
    if (e->pnode && extract_pnode_loc(e->pnode, out_filename, out_line, out_col, depth + 1)) return true;
    if (e->from && extract_contour_user_loc(e->from, out_filename, out_line, out_col, depth + 1)) return true;
  }

  if (es->fun) {
    if (extract_ast_loc(es->fun->ast, out_filename, out_line, out_col)) return true;
    if (es->fun->sym && extract_ast_loc(es->fun->sym->ast, out_filename, out_line, out_col)) return true;
  }

  return false;
}

static bool find_violation_user_loc(ATypeViolation *v, cchar **out_filename, int *out_line, int *out_col) {
  if (v->send && v->send->var && v->send->var->def) {
    if (extract_pnode_loc(v->send->var->def, out_filename, out_line, out_col)) return true;
  }

  if (v->av && v->av->var && v->av->var->def) {
    if (extract_pnode_loc(v->av->var->def, out_filename, out_line, out_col)) return true;
  }

  if (v->send && v->send->var && v->send->var->sym) {
    if (extract_ast_loc(v->send->var->sym->ast, out_filename, out_line, out_col)) return true;
  }

  if (v->av && v->av->var && v->av->var->sym) {
    if (extract_ast_loc(v->av->var->sym->ast, out_filename, out_line, out_col)) return true;
  }

  if (v->send && v->send->contour) {
    if (extract_contour_user_loc((EntrySet *)v->send->contour, out_filename, out_line, out_col)) return true;
  }
  if (v->av && v->av->contour_is_entry_set && v->av->contour) {
    if (extract_contour_user_loc((EntrySet *)v->av->contour, out_filename, out_line, out_col)) return true;
  }

  return false;
}

// ifa/issues/112 family: `fa->type_violations` is populated with
// set_add, so a Vec-as-set iterates in POINTER-HASH order, which moves
// between runs. show_violations already sorted around that to keep
// diagnostics stable; the same order is needed by any consumer whose
// WORK depends on it. PycCompiler::reanalyze promotes fields in this
// order, and the promotion order decides struct slot assignment
// (issues/121) -- so here it is a correctness matter, not cosmetics.
void fa_sorted_type_violations(Vec<ATypeViolation *> &src, Vec<ATypeViolation *> &out) {
  out.clear();
  for (ATypeViolation *v : src) if (v) out.add(v);
  if (out.n > 1) qsort(out.v, out.n, sizeof(out[0]), compar_tv);
}

static void show_violations(FA *fa, FILE *fp) {
  Vec<ATypeViolation *> vv;
  for (ATypeViolation *v : fa->type_violations) if (v) vv.add(v);
  qsort(vv.v, vv.n, sizeof(vv[0]), compar_tv);
  Vec<cchar *> printed;
  for (ATypeViolation *v : vv) if (v) {
    char *buf = nullptr;
    size_t size = 0;
    FILE *memfp = open_memstream(&buf, &size);
    if (!memfp) memfp = fp;

    cchar *filename = nullptr;
    int line = 0;
    int col = 0;

    find_violation_user_loc(v, &filename, &line, &col);

    if (!filename || line <= 0) {
      if (v->send && v->send->var && v->send->var->def && v->send->var->def->code) {
        if (!filename) filename = v->send->var->def->code->filename();
        if (line <= 0) line = v->send->var->def->code->line();
      } else if (v->av && v->av->var && v->av->var->sym && v->av->var->sym->ast) {
        if (!filename) filename = v->av->var->sym->filename();
        if (line <= 0) line = v->av->var->sym->line();
      } else if (v->av && !v->av->contour_is_entry_set && v->av->contour != GLOBAL_CONTOUR) {
        CreationSet *cs = (CreationSet *)v->av->contour;
        if (cs && cs->sym) {
          if (!filename) filename = cs->sym->filename();
          if (line <= 0) line = cs->sym->line();
        }
      }
    }
    if ((!filename || !filename[0]) && fa->funs.n && fa->funs[0]->sym) filename = fa->funs[0]->sym->filename();

    // issues/018: a BOXING violation is an ERROR even in permissive
    // mode. Permissive mode's bargain is "warn, and insert a runtime
    // check" -- but a variable whose type mixes basic types (int64 and
    // str, say) has NO RUNTIME REPRESENTATION at all, so there is no
    // check to insert and nothing downstream can recover. Reporting it
    // as a warning produced the worst available outcome: 8 warnings,
    // exit 0, and a binary that aborts with "matching function not
    // found" the moment the value is used. shedskin reaches the same
    // wall and at least fails at build time (its generated C++ gets
    // `invalid conversion from '__ss_int' to 'pyobj*'`).
    // issues/018: BOXING has no representation, so it is an error in
    // every environment. ifa/issues/039: DEFINITELY_UNBOUND likewise --
    // no execution of the program is correct, so refusing costs
    // nothing. MAYBE_UNBOUND is deliberately NOT here: it is a strict
    // warning and otherwise a runtime check, because a "possibly
    // unbound" read can be perfectly valid (a short-circuit guard --
    // see the issue), and erroring on it would reject correct programs.
    bool always_fatal = v->kind == ATypeViolation_kind::BOXING ||
                        v->kind == ATypeViolation_kind::DEFINITELY_UNBOUND;
    // ifa/issues/039: MAYBE_UNBOUND is a WARNING even under --strict,
    // and deliberately so. A "possibly unbound" read can be perfectly
    // correct -- `if first or d < bd:` short-circuits the read away
    // (tests/scope_read_before_write.py) -- so erroring on it rejects
    // valid programs. Its enforcement is a RUNTIME check, or an
    // auto-initialisation under `safe`; the compile-time message is
    // advisory in every environment.
    bool always_warning = v->kind == ATypeViolation_kind::MAYBE_UNBOUND;
    cchar *severity = always_fatal ? "error" : (always_warning || fruntime_errors) ? "warning" : "error";

    if (filename && line > 0) {
      if (col > 0)
        fprintf(memfp, "%s:%d:%d: %s: ", filename, line, col, severity);
      else
        fprintf(memfp, "%s:%d: %s: ", filename, line, severity);
    } else {
      fprintf(memfp, "%s: ", severity);
    }

    switch (v->kind) {
      default:
        assert(0);
      case ATypeViolation_kind::PRIMITIVE_ARGUMENT:
        fprintf(memfp, "illegal primitive argument type ");
        show_illegal_type(memfp, v);
        break;
      case ATypeViolation_kind::SEND_ARGUMENT:
        if (v->av->var->sym->is_symbol && v->send->var->def->rvals[0] == v->av->var) {
          fprintf(memfp, "unresolved call '%s'", v->av->var->sym->name);
          if (ifa_verbose) fprintf(memfp, " send:%d", v->send->var->sym->id);
          fprintf(memfp, "\n");
          show_candidates(memfp, v->send->var->def, v->av->var->sym);
        } else {
          fprintf(memfp, "illegal call argument type ");
          show_illegal_type(memfp, v);
        }
        break;
      case ATypeViolation_kind::DISPATCH_AMBIGUITY:
        fprintf(memfp, "ambiguous call '%s'", v->av->var->sym->name);
        if (ifa_verbose) fprintf(memfp, " send:%d", v->send->var->sym->id);
        fprintf(memfp, "\n");
        fprintf(memfp, "note: candidates are:\n");
        for (Fun *f : *v->funs) if (f) {
          show_fun(f, memfp);
          fprintf(memfp, "\n");
        }
        break;
      case ATypeViolation_kind::MEMBER:
        if (v->av->out->n == 1)
          fprintf(memfp, "unresolved member '%s'", v->av->out->v[0]->sym->name);
        else {
          fprintf(memfp, "unresolved member\n");
          for (CreationSet *selector : v->av->out->sorted) fprintf(memfp, "  selector '%s'\n", selector->sym->name);
        }
        if (v->type->n == 1)
          fprintf(memfp, "  class '%s'\n", v->type->v[0]->sym->name ? v->type->v[0]->sym->name : "<anonymous>");
        else {
          fprintf(memfp, "  classes\n");
          for (CreationSet *cs : v->type->sorted) fprintf(memfp, "  class '%s'\n", cs->sym->name);
        }
        break;
      case ATypeViolation_kind::MATCH:
        if (v->av->var->sym->name)
          fprintf(memfp, "near '%s' unmatched type: ", v->av->var->sym->name);
        else
          fprintf(memfp, "unmatched type: ");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        break;
      case ATypeViolation_kind::NOTYPE:
        show_name(memfp, v->av);
        fprintf(memfp, "has no type\n");
        break;
      case ATypeViolation_kind::BOXING:
        show_name(memfp, v->av);
        fprintf(memfp, "has mixed basic types:");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        break;
      case ATypeViolation_kind::MAYBE_UNBOUND:
        show_name(memfp, v->av);
        fprintf(memfp, "may be used before assignment on some path; type is:");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        break;
      case ATypeViolation_kind::DEFINITELY_UNBOUND:
        show_name(memfp, v->av);
        fprintf(memfp, "is used before assignment on every path; type is:");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        break;
      case ATypeViolation_kind::CLOSURE_RECURSION:
        show_name(memfp, v->av);
        fprintf(memfp, "is recursive closure\n");
        break;
    }

    if (filename && line > 0) {
      show_source_caret(memfp, filename, line, col);
    }

    if (v->send)
      show_call_tree(memfp, v->send->var->def, (EntrySet *)v->send->contour);
    else if (v->av->contour_is_entry_set)
      show_avar_call_tree(memfp, v->av);
    else if (v->av->contour != GLOBAL_CONTOUR)
      show_call_tree(memfp, ((CreationSet *)v->av->contour)->defs.first()->var->def,
                     (EntrySet *)((CreationSet *)v->av->contour)->defs.first()->contour, 1);

    if (memfp != fp) {
      fclose(memfp);
      if (buf) {
        bool dup = false;
        for (cchar *p : printed) {
          if (p && strcmp(p, buf) == 0) {
            dup = true;
            break;
          }
        }
        if (!dup) {
          printed.add(buf);
          fputs(buf, fp);
        } else {
          free(buf);
        }
      }
    }
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

// ifa/issues/098 (second defect): `EntrySet::out_edge_map` is never
// reset -- `get_AEdges` reads it to reuse the same AEdge across passes,
// so clearing it would mint fresh edges every pass and destroy every
// `e->to` binding the splitter's cross-pass routing depends on. An
// entry therefore survives from the FIRST pass in which the send
// dispatched, and "entry exists" is NOT "dispatched this pass".
// `EntrySet::out_edges` is the per-pass fact -- `clear_es` empties it
// and `analyze_edge` re-adds an edge only after its filter gate passes
// -- so an entry none of whose edges are in `from->out_edges` means
// dispatch failed COMPLETELY this pass, and must be reported exactly
// like a missing entry. Without this the `else` arm runs, finds no
// analyzed edge to inspect, and reports nothing: dispatch failure is
// silent (mastermind2 compiles rc=0 with `if`s stuck on bottom
// conditions because `__pyc__` has no `list.__lt__` -- issues/122).
static bool dispatched_this_pass(EntrySet *from, Vec<AEdge *> *m) {
  if (!m) return false;
  for (AEdge *me : *m) if (me && from->out_edges.set_in(me)) return true;
  return false;
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
        if (m) fa->dbg_dispatch_total_sites++;
        if (!dispatched_this_pass(from, m)) {
          if (m) fa->dbg_dispatch_fail_sites++;
          if (p->code->partial == Partial_NEVER) {
            if (m) fa->dbg_dispatch_fail_reported++;
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
        // ifa/issues/050 3b: a global cell that NOTHING READS is
        // unobservable, so the union of its stores cannot be wrong. This
        // cannot be phrased as a liveness test -- Var::live is set by
        // dead-code elimination, which runs after this (see the sibling
        // check below for the same trap) -- but FA already knows the
        // consumer set during analysis, and that is the property that
        // actually matters here. Only global cells: a local with no
        // consumers is a different situation and still worth reporting.
        if (av->contour == GLOBAL_CONTOUR) {
          int readers = 0;
          for (AVar *c : av->forward) if (c) ++readers;
          if (getenv("PYC_DBG_GCELL") && av->var && av->var->sym && av->var->sym->name)
            fprintf(stderr, "[gcell] %s readers=%d mixed=%d\n", av->var->sym->name, readers, mixed_basics(av) ? 1 : 0);
          if (!readers) continue;
        }
        if (!is_only_used_by_phy_or_phi(av->var) && mixed_basics(av))
          type_violation(ATypeViolation_kind::BOXING, av, av->out, nullptr, nullptr);
      }
    }
    // ifa/issues/039: report the definite-assignment fact computed by
    // find_maybe_unbound (ssu.cc). It is a CFG property, not a type
    // one, so it rides on Var rather than on the AVar's type -- but it
    // is reported as an ATypeViolation so it inherits the existing
    // severity plumbing: error under --strict, warning under
    // --permissive, with no new flags. The phi/phy carriers are skipped
    // for the same reason BOXING skips them: they are plumbing, and
    // reporting one names an internal Var instead of the user's.
    for (EntrySet *es : fa->ess) {
      for (Var *v : es->fun->fa_all_Vars) {
        AVar *av = make_AVar(v, es);
        if (is_only_used_by_phy_or_phi(av->var)) continue;
        // Named locals only. 549 of the 550 flags on the issue's repro
        // are compiler-generated temporaries with no name -- normal in
        // lowered code, and reporting one names an internal Var rather
        // than anything the user wrote. The FLAG stays set on them,
        // because the non-strict default-initialisation wants exactly
        // those slots; it is only the diagnostic that filters.
        // No `av->var->live` test: Var::live is set by dead-code
        // elimination, which runs AFTER this -- it is 0 for everything
        // here, so requiring it silently suppressed every report. Same
        // ordering trap as Var::is_formal (set by build_patterns, also
        // later), which is why the SSU pass reads formals from
        // f->sym->has instead.
        if (av->var && av->var->sym && av->var->sym->name && av->var->sym->maybe_unbound)
          type_violation(av->var->sym->definitely_unbound ? ATypeViolation_kind::DEFINITELY_UNBOUND
                                                          : ATypeViolation_kind::MAYBE_UNBOUND,
                         av, av->out, nullptr, nullptr);
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
  work_edges = work_sends = work_escons = 0;  // ifa/111 probe
  fa->edge_worklist.clear();
  fa->send_worklist.clear();
  initialize_symbols();
  initialize_primitives();
  build_arg_positions(fa);
  build_patterns(fa);
}

static void initialize_pass() {
  pass_timer.restart();
  fa->dbg_dispatch_fail_sites = 0;      // ifa/issues/098 second defect
  fa->dbg_dispatch_fail_reported = 0;
  fa->dbg_dispatch_total_sites = 0;
  fa->type_violations.clear();
  fa->type_world.type_violation_hash.clear();
  fa->entry_set_done.clear();
  // ifa/issues/074 (IFA_DBG_INCOMPAT): these are incremented by
  // extend_analysis, which runs AFTER complete_pass, so shadow them here
  // for the probe before the reset wipes the previous pass's tally.
  ld_dup_es = fa->dup_split_attempts;
  ld_dup_cs = fa->cs_dup_split_attempts;
  ld_churn = fa->rederive_churn;
  fa->dup_split_attempts = 0;  // issue 033 stage A per-pass counter
  fa->cs_dup_split_attempts = 0;  // issue 033 D5 per-pass counter
  fa->rederive_churn = 0;         // issue 074: the guard's real input
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

// ifa/issues/124 probe: is a named function's FORMAL seen as a type
// confluence at all? `append(self, x)` is called with an int from one
// comprehension and None from another, so arg3 ought to be one.
static void dbg_confluence_probe(AVar *av, bool added) {
  static cchar *want = nullptr;
  static int checked = 0;
  if (!checked) { want = getenv("IFA_DBG_CONFLUENCE"); checked = 1; }
  if (!want || !av->contour_is_entry_set) return;
  EntrySet *es = (EntrySet *)av->contour;
  if (!es->fun || !es->fun->sym || !es->fun->sym->name || strcmp(es->fun->sym->name, want)) return;
  if (!av->var || !av->var->is_formal) return;
  fprintf(stderr, "CONFL p=%d es=%d formal=%s added=%d in:", analysis_pass, es->id,
          av->var->sym->name ? av->var->sym->name : "?", added ? 1 : 0);
  for (CreationSet *c : av->in->type->sorted) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
  fprintf(stderr, " out:");
  for (CreationSet *c : av->out->type->sorted) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
  // EVERY backward writer, including empty-typed ones the earlier probe
  // filtered out -- the question is where `None` enters when no writer
  // seems to carry it.
  fprintf(stderr, " | writers(%d):", av->backward.n);
  for (AVar *x : av->backward) if (x) {
    EntrySet *xes = x->contour_is_entry_set ? (EntrySet *)x->contour : nullptr;
    fprintf(stderr, " {av=%d %s/es%d:", x->id,
            xes && xes->fun && xes->fun->sym && xes->fun->sym->name ? xes->fun->sym->name : "?",
            xes ? xes->id : -1);
    for (CreationSet *c : x->out->type->sorted) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
    fprintf(stderr, " RAW:");
    for (CreationSet *c : x->out->sorted)
      fprintf(stderr, " %s#%d%s", c->sym->name ? c->sym->name : "?", c->id, c->sym->is_constant ? "(const)" : "");
    fprintf(stderr, "}");
  }
  fprintf(stderr, "\n");
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
  dbg_confluence_probe(av, confluences.set_in(av) != 0);
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
    // ifa/issues/099: an inherited entry that is ABOUT the split product
    // (its key is being re-homed from `es` onto `e->to`, or already names
    // `e->to`) must have its VALUE re-homed with its key. The key was
    // rewritten `es` -> `e->to` here and the value was not, so a recorded
    // route pointing at `es` survived into the product's own map as "a
    // recursive call from `e->to` may go back to `es`" -- precisely the
    // binding this split exists to undo. check_split then vetoes only the
    // contour it is detaching from on THIS pass, so once the two contours
    // each hold a route to the other, the veto leaves exactly the one
    // just vacated and the edge swaps contours every pass, forever, with
    // nothing growing (bh: 10 edges over two contour pairs; pylife: ONE
    // edge; linalg: one). Entries about OTHER contours are left alone --
    // this split only re-homes its own group, `es` still exists and may
    // still hold unrelated edges, so redirecting their routes would
    // over-reach.
    bool about_product = m->key->from == es || m->key->from == e->to;
    Vec<EntrySet *> rehomed;
    Vec<EntrySet *> *value = m->value;
    if (about_product) {
      for (EntrySet *v : *m->value) if (v) rehomed.set_add(v == es ? e->to : v);
      value = &rehomed;
    }
    if (m->key->from == es)
      map_set_add(e->to->pending_es_backedge_map, new_AEdge(m->key->fun, m->key->pnode, e->to), value);
    else
      map_set_add(e->to->pending_es_backedge_map, m->key, value);
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
      ++fa->rederive_churn;  // re-derived a filter the ledger already had
      if (getenv("IFA_DBG_INCOMPAT"))
        fprintf(stderr, "REDERIVE p=%d FILTER fun=%s#%d es=%d pos=%p part=%d/%d first_pass=%d\n", analysis_pass,
                f->sym->name ? f->sym->name : "?", f->sym->id, orig_es->id, (void *)x->key, x->value->sorted.n,
                x->value->n, d->pass_made);
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
  // ifa/issues/109: the caller may hand us an AVar that is no longer at
  // an argument position of `es` -- split_for_per_cs_method_receivers
  // walks positional_arg_positions and can split earlier positions in the
  // same pass, which rewrites es->args underneath the later ones. That is
  // "nothing to split here", not a broken invariant, so skip rather than
  // assert. (Before the receiver fan was widened to same-class container
  // receivers this was unreachable, which is why it was an assert.)
  if (!p) return 0;
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
  // ifa/issues/075 Piece 1+3: a bare cs_es_map product can be display-
  // incompatible with some of the edges that share its CS (the same
  // list/dict method called from multiple lexical displays) -- the
  // pre-075 behavior below just leaves such an edge un-split, which is
  // exactly what keeps a container-method's element AVar a cross-
  // instance union (063/075). When csm_enabled(), fan an incompatible
  // edge into its OWN product instead: keyed on (cs_es_map target x
  // this edge's display), via find_or_make_display_variant (above),
  // which reuses an existing sibling -- durably, across passes, since
  // it searches tes->fun->ess exactly like find_or_make_filtered_
  // entry_set does -- rather than one each. 073 proved (type x
  // display) is bounded, so this is a finite fan-out, not a new
  // divergence source. Flag off (default): identical to the original
  // skip-on-incompatible behavior.
  // Resolve cs_es_map's target for THIS edge: the shared product if
  // display-compatible (or already where the edge is), else -- CSM only
  // -- a per-display sibling of it; else null (caller must leave the
  // edge alone, the pre-075 behavior).
  // The display no longer gates this route: it is built for make_AVar's
  // enclosing-scope resolution, not used as contour identity, so an edge
  // whose lexical display differs from the product's is routed into the
  // product anyway (it keeps the display its first edge stamped).
  auto resolve_target = [&](AEdge *ee, CreationSet *cs) -> EntrySet * { return cs_es_map.get(cs); };
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
    // ifa/issues/074: identify the single edge that TYPE_CONFLUENCE
    // redispatches every pass once contour counts have gone flat --
    // assignment churn, as distinct from growth. Probe-only.
    if (getenv("IFA_DBG_CHURN"))
      fprintf(stderr, "[churn] p=%d stage=%d fun=%s edge=%p from_es=%d to_es=%d -> %d\n", analysis_pass,
              cur_split_stage, ee->match && ee->match->fun && ee->match->fun->sym->name ? ee->match->fun->sym->name : "?",
              (void *)ee, ee->from ? ee->from->id : -1, ee->to ? ee->to->id : -1, tes->id);
    if (ee->to) ee->to->edges.del(ee);
    ee->to = 0;
    if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_detach[cur_split_stage];
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
    // Issue 034 family (sudoku5): every product ES this edge would be
    // routed or COPIED into must be display-compatible, or the
    // set_entry_set -> update_display these paths call asserts.
    // resolve_target either returns a display-compatible target (the
    // shared one, or -- CSM only -- a per-display sibling) or null; a
    // null anywhere means leave the WHOLE edge on its current contour
    // (sound -- a later pass re-decides it against settled state)
    // rather than mis-stamping a shared product's display. The copies
    // share ee->from, so the original edge's display gates them all.
    bool all_compat = true;
    Vec<EntrySet *> targets;
    for (int i = 0; i < ety->sorted.n; i++) {
      EntrySet *tes = resolve_target(ee, ety->sorted[i]);
      if (!tes) {
        all_compat = false;
        break;
      }
      targets.add(tes);
    }
    if (!all_compat) continue;
    if (ety->sorted.n == 1)
      redispatch(ee, targets.v[0]);
    else {
      for (int i = 0; i < ety->sorted.n; i++) {
        if (!i)
          redispatch(ee, targets[i]);
        else
          ee = copy_AEdge(ee, targets[i]);
      }
    }
    if (ee->to != old) {
      again = 1;
      // ifa/issues/076: was missing a %d for `ee->to->id` (6 args,
      // 5 placeholders) -- vfprintf silently drops the trailing arg,
      // so this printed `old->id` (the PRE-redispatch target) as the
      // "-> N" value, never the actual new target. Traces read against
      // this line before the fix (issue 076's own investigation
      // included) had the redispatch destination backwards.
      log(LOG_SPLITTING, "DISPATCH ES %d:%d, %s %d, %d -> %d\n", ee->from->id, ee->pnode->lvals[0]->sym->id,
          es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, old ? old->id : -1, ee->to->id);
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
static int compar_int(const void *a, const void *b) {
  int x = *(const int *)a, y = *(const int *)b;
  return x < y ? -1 : (x > y ? 1 : 0);
}

// issue 065 / 043 shape B: a cross-pass-STABLE signature for a
// setter/mark-driven split group, keyed on the setter SITES (the writing
// Vars' sym ids -- stable IR) rather than the per-(Var,EntrySet) setter
// AVars whose identity shifts as the splitter mutates the ES structure
// (the reason the type-only group_signature can't key these groups and
// they were excluded from issue-033 routing). Two passes whose element-
// type partition writes from the same setter sites produce the same
// signature, giving the routing a stable product to reuse. Returns 0
// ("no identity") when no setter sites are visible yet, so such a group
// stays unroutable this pass rather than colliding on an empty key.
static uint setter_site_signature(Vec<AEdge *> &these_edges, Fun *f) {
  uint h = 0;
  int i = 0;
  bool any = false;
  for (MPosition *p : f->positional_arg_positions) {
    Vec<int> sites;
    for (AEdge *x : these_edges) {
      AVar *a = x->args.get(p);
      if (!a) continue;
      if (a->setters)
        for (AVar *s : *a->setters) if (s && s->var && s->var->sym) sites.set_add(s->var->sym->id);
      if (a->lvalue && a->lvalue->setters)
        for (AVar *s : *a->lvalue->setters) if (s && s->var && s->var->sym) sites.set_add(s->var->sym->id);
    }
    i++;
    if (!sites.n) continue;
    any = true;
    sites.set_to_vec();
    if (sites.n > 1) qsort(sites.v, sites.n, sizeof(int), compar_int);
    uint ph = 0;
    for (int sid : sites) ph = ph * 31u + (uint)sid;
    h += ph * open_hash_primes[(i - 1) % 256];
  }
  if (!any) return 0;
  return h ? h : 1;
}

// ifa/issues/101: include the return types in group_signature. 1 is the
// historical behaviour and stays the DEFAULT; 0 drops the term.
//
// Dropping it was the obvious repair for the ledger cycle described at
// the use site, and it measured EXACTLY INERT -- byte-identical
// final_pass/violations/ess/css on go, linalg, plcfrs and sudoku5. So the
// two cycling signatures do NOT differ in their return term, and the
// hypothesis behind this flag is wrong: the difference is in the argument
// term, which means linalg's 792/692 pair is most likely two distinct
// groups SWAPPING contours rather than one group cycling. Kept as a
// measured negative result, defaulted to the historical behaviour since
// it buys nothing and has not been swept.
static int gsigret_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_GSIGRET");
    e = v ? atoi(v) : 1;
  }
  return e;
}

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
  // ifa/issues/101: the RETURN term makes this signature depend on which
  // contour the group is CURRENTLY routed to -- `x->rets[r]->lvalue` gets
  // its type from the callee, so the same group hashes differently
  // depending on where it already sits. That lets the ledger hold a
  // CYCLE: measured on linalg's __deepcopy__, gsig 16821760 (recorded
  // p40) says the home is es 692 and gsig 33861632 (p58) says it is es
  // 792, so the group alternates 792 -> 692 -> 792 to the pass cap with
  // no growth and no progress.
  //
  // A group's identity should be a property of the GROUP -- the callers,
  // their argument types, the callee function -- not of the routing
  // decision already made about it. For a fixed `f` and fixed argument
  // types the return type is a consequence of the analysis, not an
  // independent discriminator, so dropping the term collapses the two
  // signatures into one. PYC_GSIGRET=1 restores the old behaviour.
  if (gsigret_enabled()) {
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
  }
  return h ? h : 1;  // 0 is reserved for "no identity" / filtered-path keys
}

// issue 074 Stage 4 / 073: the highest display slot index [0, nd) this
// fun's body actually consumes for free-variable resolution in make_AVar
// -- i.e. it references a Var at nesting_depth k+1 owned by a proper
// ancestor scope (k+1 <= nd). Slots above this are INERT: build_display
// still fills them, but make_AVar never reads them, so two contours that
// differ ONLY in an inert slot are the same contour for correctness.
// This is 064's "phantom method display": a Python method's slots >= 1
// are inert because captures are lowered to explicit closure classes
// (maybe_synthesize_closure_pyda), so only display[0] (the module
// singleton) is ever consumed. Genuine V nested functions / issue-001
// synthesized closure carriers reference an ancestor free var and so
// keep a live slot >= 1 -- for them the enforcement below is unchanged.
// -1 = no slot consumed. Cached; dynamic fa_all_Vars additions are own
// locals (nd == f->nd+1), which never lower this bound.

// issue 074 Stage 4: enable the display-demotion (ROUTE across inert
// display slots + non-asserting inert slots in update_display). Behind a
// flag for the prototype/measurement.

// ifa/issues/075: element-CS container-method separation (pyc's analog
// of shedskin's func_copy-per-dcpa). 0 off (default, byte-identical to
// baseline), 2 split (fans split_edges per (CS x display), Piece 1; the
// demand-driven stage itself is split_container_methods_per_element_cs
// below). 1 is reserved for a future side-effect-free dump/probe mode
// (see 075 Piece 1) -- not yet built, treated as off.
static int csm_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSM");
    e = v ? atoi(v) : 0;
  }
  return e;
}

// ifa/issues/074 (detach-route growth): on the DETACH route
// (`make_entry_set` with a non-null `split`), offer the edge an existing
// contour when that contour is a HARD match -- `entry_set_compatibility`
// == INT_MAX, i.e. no penalty of any kind: type-compatible, sset-
// compatible, constant-compatible, filters admit it. `split` itself is
// vetoed.
//
// Motivation: skipping the scoring path entirely on this route means a
// detached edge is never offered an existing contour -- it takes the
// pending/lineage route or gets a BRAND-NEW one. With the display out of
// contour identity (issues/100) that is now the dominant growth source:
// sudoku4/genetic2 leak an exactly steady `split-fresh=2` plus ~134/42
// new edges for the fresh contours' bodies every pass, to the pass cap;
// hq2x re-manufactures ~250 edges a pass the same way.
//
// OFF by default: it regresses 6 tests (see issues/074's 2026-08-13
// census). Using the *soft* score here is far worse (59 failures) -- it
// re-merges what the split just separated, 073's match_seq hazard -- so
// only the hard match is worth carrying as an experiment. The 6 include
// recursive_polymorphic and match_map_star, i.e. exact type identity is
// NOT sufficient evidence that a contour is not what the split is
// separating; the flag exists to keep investigating that.
// 0 off (default); 1 = entry_set_compatibility == INT_MAX; 2 = that AND
// a POSITIVE type-level match; 3 = that AND a positive CreationSet-level
// match (see edge_type_identical_to_entry_set); 4 = LOOKUP BY DURABLE
// TYPE KEY -- shedskin's model, where the type tuple names the contour
// rather than being a property compatibility-tested against it. Requires
// PYC_TYPEKEY.
// ifa/issues/074: match compatibility against EntrySet::type_key (the
// previous pass's CONVERGED formal types) rather than the contour's
// momentary mid-pass accumulation. The point is durability: shedskin
// binds a contour to a fixed type tuple and keeps that binding across
// passes, so routing is a lookup rather than a race. Off by default.
static int typekey_enabled() {
  static int e = -1;
  if (e < 0) e = getenv("PYC_TYPEKEY") ? 1 : 0;
  return e;
}

// ifa/issues/074: canonicalize contour creation on the durable type key
// -- at most one contour per (fun, type tuple), found by lookup and
// created on miss, the way find_or_make_filtered_entry_set already works
// for CS partitions. Durable keys alone (PYC_TYPEKEY) proved necessary
// but not sufficient: they make matching stable in TIME but not unique
// in SPACE, so two same-keyed contours still let the `x != split` veto
// alternate. Canonicalization removes the duplicates, so there is
// nothing to alternate between.
//   1 = canonicalize, but never hand an edge back to the contour the
//       splitter is detaching it FROM (conflicts logged, split honored)
//   2 = full canonicalization: reuse even then, so a split that
//       disagrees with the canonical key becomes a no-op
// IFA_DBG_CANON=1 logs every conflict and prints per-pass stats.
static int canon_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CANON");
    e = v ? atoi(v) : 0;
  }
  return e;
}

// Defaults to 5 as of 2026-08-16 (was 0 -- off). Mode 5 is mode 4's
// durable-type-key reuse on the detach route, RESTRICTED to splits whose
// own discriminator was argument types (see cur_split_type_only).
//
// ifa/issues/101: the detach route (`if (!split) find_best_entry_sets`)
// never offers a detached edge an existing contour, so every caller split
// cascades into fresh callee contours. Measured on linalg: half its 1290
// contours share an argument-type tuple with another, and the total
// EXCEEDS full cartesian-product specialization (957) by 35% --
// `__pyc_to_bool__` alone had 14 live contours with one type tuple
// between them.
//
// Mode 4 (types alone) was measured and is NOT safe: it breaks sudoku5's
// convergence outright (26 -> 273 violations) and worsens go and plcfrs,
// because a setter- or mark-driven split can produce two contours with
// identical argument types on purpose. Mode 5 adds exactly that
// condition and every one of those regressions disappears.
//
// Corpus, 77 programs: zero exit-code changes, zero pass_limit_hit
// changes, corpus violations 7435 -> 6399 (-13.9%), ess lower on 41
// programs (-2.3% overall), +1.5% analysis time.
static int hard_reuse_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_HARDREUSE");
    e = v ? atoi(v) : 5;
  }
  return e;
}

// ifa/issues/074: disable mark-based splitting stages, as an
// investigable option.
//   0 = mark-based splitting on (pre-2026-08-14 behaviour)
//   1 = skip MARK_TYPE (stage 2)  -- THE DEFAULT
//   2 = also skip MARK_SETTER / MARK_SETTER_OF_SETTER (stage 4)
// Marks exist to separate two contours that carry the SAME argument
// types but different value origins (IFA.md §6.2, "recursion-meets-
// polymorphism without k-CFA"). That is by construction a
// distinction no type-tuple contour name can express -- so if
// contours are canonicalized on their type key, mark splits are
// unnameable. IFA_DBG_KEYSPACE=1 measures the gap they open.
// ifa/issues/074: extend the self-product complement eviction to the
// `v > 0` case (residual violations). See the comment at the eviction
// site for what each mode tests.
//
//   0 = off: eviction only at whole-program convergence (pre-074 shape)
//   1 = evict only the type-disjoint complement          (unsound)
//   2 = keep the group, evict nothing                    (unsound)
//   3,4 = durable key == the recorded partition          (never fires)
//   5 = durable key stable across two passes: per-contour convergence
//
// **5 is the default.** The eviction's real precondition is that THIS
// CONTOUR has stopped moving, which `nviol_this_pass == 0` only ever
// approximated whole-program-wide.
// ifa/issues/074: gate the ES ledger ROUTE on the recorded product still
// being a compatible home for the group; =2 also refreshes an entry
// proven stale. OFF by default, and kept only as a measured control:
// both modes do stop the churn, but by declining a route you only mint
// instead, so the growth comes straight back (repro ess 144 -> 279 for
// mode 1, 257 for mode 2). PYC_SELFPROD=6 fixes the same oscillation
// from the other end without that cost.
// ifa/issues/101: canonicalize CONTAINER CreationSet identity on the
// durable element type instead of the creation site x contour. Off by
// default. See capture_elem_keys() and creation_point's Lcanon.
//
// 3 = ifa/issues/074: key on the RECEIVER's structural element SHAPE
// instead (`list<list<float64>>`), which is what shedskin gets free from
// `list<T>` being keyed on T. Off by default, and MEASURED:
//
//   deepcopy_recursive_nested_growth.py, guards off, ess/css by pass
//     mode 0   0:77/599  20:175/782  40:280/997  60:385/1212
//              80:490/1427  100:595/1642      -- dead linear, +5.25/pass
//     mode 3   0:77/599  20:159/749  40:157/740  60:211/852
//              80:223/877  100:207/850        -- bounded, band ~210
//
// That is 074's headline defect gone: the growth is UNBOUNDED at the
// default and BOUNDED here (-62% ess, -46% css at pass 101). What is
// left is an oscillation inside the band, which is a different and much
// smaller problem than divergence -- but CONVERGED is still 0, and the
// repro still does not compile.
//
// Not the default because of the cost: pyc suite is identical
// (301/0/14), corpus goes from 5 failing to 9 -- kanoodle, plcfrs and
// rdb time out and quameon fails to compile. The likely reason is in
// cselem_shape_key: the canon map is MONOTONE and global, and the shape
// it keys on is read LIVE on the pass the contour is created, because
// that is the only moment creation_point is ever consulted (`cs_map`
// answers ever after). A merge decided from an incomplete shape can
// never be revisited, and the splitter then has to work around it. That
// is ifa/issues/066 -- the decision is keyed per pass, not per creation
// site -- and making the canonicalization revisitable is the next step,
// not a wider or narrower key.
static int cselem_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSELEM");
    e = v ? atoi(v) : 0;
  }
  return e;
}

// ifa/issues/101: shedskin's MOLD FALLBACK. When a contour has no live
// split parent to inherit an allocation instance from, shedskin does not
// mint -- ifa_seed_template falls back to `gx.orig_types[node]`, the
// allocation's instance in the dcpa=0/cpa=0 mold, i.e. the one every
// other contour of that function uses. It therefore keeps ONE container
// instance per allocation SITE, shared across contours, and lets `ifa()`
// split it later when it finds a concrete imprecision -- by which time
// the element types are known.
//
// pyc has no such fallback: `creation_point` mints unconditionally, so a
// site allocates once per (site x contour) from pass 0 onward. That is
// what makes `stereo` create 185 container CreationSets covering 2
// element shapes on pass 0 alone.
//
// 1 = containers only (s->element), the DEFAULT from 2026-08-16.
// 2 = every eligible sym; measured and rejected -- it costs plcfrs
// dearly (violations 2232 -> 4353, ess 850 -> 1213) for a few css on
// other programs. 0 restores the old mint-unconditionally behaviour.
// 3 = containers only AND never for a SPLIT CHILD contour, the DEFAULT
// from 2026-08-28 (ifa/105); see the mode-3 note at the fallback itself.
// Measured against mode 1: pyc suite identical (300 passed / 0 failed /
// 15 known), corpus identical program for program (the same five fail:
// chess, go, linalg, othello3, sudoku5), and the whole bounded
// copy-of-copy family -- four or more nested copy.deepcopy calls --
// goes from a BOXING refusal to compiling and running correctly.
//
// Corpus at mode 1, 77 programs: zero exit-code changes, zero
// pass_limit_hit changes, violations 6399 -> 4276 (-33.2%, plcfrs alone
// 4355 -> 2232), ess and css lower on 3 and 4 programs and HIGHER ON
// NONE, analysis time -2.8%.
static int csmold_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSMOLD");
    e = v ? atoi(v) : 3;
  }
  return e;
}

// ifa/issues/101: detect and break 2-cycles in the ES split ledger.
//
// DEFAULT 1 since ifa/issues/055. Without it the ledger can hold the
// same group signature twice with each EntrySet recorded as the other's
// product, so "route this group to its durable home" says 147 -> 223
// AND 223 -> 147 -- a cycle by construction. One edge then ping-pongs
// between the two forever, and because apply_entry_set_split sets
// `split = 1` on edge RE-POINTING rather than on creating a contour,
// split_ess_for_type reports progress every pass while creating
// nothing. Measured on tests/dict_pair_swap_setdiff_nonconvergence.py:
// ess and css pinned at 189/806 from pass 24 to the cap, confluences
// pinned at 104, d_ess = d_css = 0, and the analysis never converges.
//
// That churn also STARVES every later stage, since they all sit behind
// `if (!analyze_again)` -- which is why the setter back-flow that would
// split the dict never ran (zero setters existed program-wide).
//
//   repro      51 passes CONVERGED=0 -> 32 passes CONVERGED=1, 0 viol
//   suite      297 passed/14 known   -> 298 passed/0 failed/13 known
//   corpus     67 of 77 compile either way, program for program, and
//              sunfish improves (400s timeout -> clean failure)
//   plcfrs     still does not converge; passes 45 -> 36 but violations
//              2451 -> 5353 and ess 850 -> 1524
// ifa/issues/055: can `from` reach `to` through the recorded route
// relation? Used to refuse a route that would close a cycle of any
// length -- the general form of route_last's A<->B check.
static bool route_reaches(EntrySet *from, EntrySet *to, Vec<EntrySet *> &path, Vec<EntrySet *> &seen) {
  if (!from) return false;
  if (from == to) { path.add(from); return true; }
  if (!seen.set_add(from)) return false;
  Vec<EntrySet *> *adj = fa->route_adj.get(from);
  if (adj)
    for (EntrySet *n : *adj)
      if (n && route_reaches(n, to, path, seen)) { path.add(from); return true; }
  return false;
}

// DEFAULT 3 -- the GENERAL form (ifa/issues/055). A 2-cycle is not the
// only shape the ledger can hold: A->B->C->A is the same disease with
// three signatures, and route_last's one-step memory cannot see it.
// Mode 3 records the whole route relation (FA::route_adj) and refuses
// any route that would close a cycle of ANY length, so the relation is
// acyclic by construction rather than by pattern-matching one shape.
// Measured strictly >= mode 1: repro 32 -> 31 passes, suite identical
// (298 passed / 0 failed / 13 known), corpus identical program for
// program (67 of 77), plcfrs improves (5353 -> 4993 violations, ess
// 1524 -> 1420). On everything measured only 2-cycles actually occur,
// so the extra reach is insurance, not yet a demonstrated win.
//
// Mode 4 -- "never re-assign to ANY previously routed EntrySet" -- is
// the tempting stronger rule and it is WRONG, measurably: 6 suite
// failures, two of them hard compile failures (test_heapq,
// tuple_compare). Repeating the SAME route every pass is the ledger
// WORKING, a re-derived group landing in its established home; only a
// return to an ABANDONED home is pathological. "Closes a cycle" is
// exactly that distinction, "never revisit" conflates the two. Kept
// only so the difference stays measurable.
static int routecycle_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_ROUTECYCLE");
    e = v ? atoi(v) : 3;
  }
  return e;
}

// ifa/issues/055: treat "routed the same group to the same home as last
// pass" as NOT progress. Default 0 while it is measured.
static int routestable_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_ROUTESTABLE");
    e = v ? atoi(v) : 0;
  }
  return e;
}

static int routegate_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_ROUTEGATE");
    e = v ? atoi(v) : 0;
  }
  return e;
}

// Defaults to 6 as of 2026-08-16 (was 5): accept a period-2 flip-flop as
// a settled contour, not just a constant one. Measured over the whole
// shedskin corpus as EXACTLY inert -- 77 programs, zero changes to
// rc/violations/ess/css/final_pass/pass_limit_hit, -0.3% time -- while
// making tests/deepcopy_recursive_nested_growth.py converge outright
// (pass 46, pass_limit_hit=0, 0 violations, against mode 5's pass 102
// with 4 violations). See the mode-6 comment at the use site.
static int selfprod_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_SELFPROD");
    e = v ? atoi(v) : 6;
  }
  return e;
}

//
// **On by default from 2026-08-14.** Same design rule the lexical display
// got in issue 100: a contour merge may be prevented by types or CS
// partitioning, never by provenance -- and mark distance IS provenance
// (depth from a generating AVar), which is why no type tuple can name
// what it separates. This is NOT widening (issue 057's prohibition): it
// merges no type-distinct contours; it refuses exactly the redundant
// split 057 itself names, contours "type-identical to existing ones" --
// hq2x's monomorphic PIXELxx_yy helpers get setkey=1, cpakey=1 and 36
// contours, one per call site. The VIOLATION stage still calls
// split_with_type_marks(SPLIT_DYNAMIC), so marks stay available as
// demand-driven repair where a type violation actually appears.
static int nomark_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_NOMARK");
    e = v ? atoi(v) : 1;
  }
  return e;
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
  // Issue 074 Stage 1: the type-compatible "stay" set (edges that keep
  // es). Carried so the self-product complement eviction can re-home it.
  Vec<AEdge *> stay_edges;
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
  // ifa/issues/124 probe: for a named function, dump each edge's actual
  // type at the confluence position against the ES's, and the verdict.
  if (const char *dn = getenv("IFA_DBG_DECIDE")) {
    if (es->fun && es->fun->sym && es->fun->sym->name && !strcmp(es->fun->sym->name, dn)) {
      AVar *es_arg = avpos ? es->args.get(avpos) : nullptr;
      fprintf(stderr, "DECIDE p=%d es=%d av=%d nedges=%d es_type:", analysis_pass, es->id, av->id, all_edges.n);
      if (es_arg) for (CreationSet *c : es_arg->out->type->sorted) fprintf(stderr, " %s#%d", c->sym->name?c->sym->name:"?", c->id);
      fprintf(stderr, "\n");
      for (AEdge *ee : all_edges) if (ee && ee->from) {
        AType *t = ety_at(ee);
        AVar *a = avpos ? ee->args.get(avpos) : nullptr;
        fprintf(stderr, "   edge from_es=%d ety(n=%d):", ee->from->id, t->n);
        for (CreationSet *c : t->sorted) fprintf(stderr, " %s#%d", c->sym->name?c->sym->name:"?", c->id);
        fprintf(stderr, " RAW:");
        if (a) for (CreationSet *c : a->out->sorted) fprintf(stderr, " %s#%d", c->sym->name?c->sym->name:"?", c->id);
        fprintf(stderr, " compat=%d\n", edge_type_compatible_with_entry_set(ee, es, fmark));
      }
    }
  }
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
  dec->stay_edges.copy(stay_edges);  // issue 074: for self-product complement eviction
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
  if (const char *dn = getenv("IFA_DBG_DECIDE"))
    if (es->fun && es->fun->sym && es->fun->sym->name && !strcmp(es->fun->sym->name, dn))
      fprintf(stderr, "DECIDE-OUT p=%d es=%d av=%d do=%d stay=%d groups=%d\n", analysis_pass, es->id, av->id,
              do_edges.n, stay_edges.n, dec->groups.n);
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
  SplitDecision *route_d = nullptr;  // ifa/issues/055: the ledger decision that routed, if any
  bool stay_evicted = false;  // issue 074: self-product complement eviction, once per apply
  // Issue 074: the self-product eviction is only sound when types have
  // CONVERGED (0 violations): then the self-product is a spurious precision
  // flip-flop (pygmy) and resolving it cannot change correctness. With
  // residual violations the union is still widening, so the recorded
  // "home == es" decision is stale and eviction mis-homes real content
  // (amaze/linalg regressed 884->915 / 170->187 without this gate).
  int nviol_this_pass = fa->type_violations.set_count();
  for (Vec<AEdge *> *gp : dec->groups) {
    Vec<AEdge *> &these_edges = *gp;
    AEdge *e = these_edges[0];
    // The group's type partition at the confluence position, on the
    // constant-stripped ->type view (raw ->out re-derives constants
    // differently under the constant cap — issue 033 D4 note).
    // ifa/issues/124: via split_type_view, so a group formed BY the
    // nil distinction gets a filter that actually admits nil. Reading
    // the bare ->type here made a pure-nil group's partition bottom,
    // which starves the formal in the product contour (`illegal call
    // argument type 'c' illegal:` with an empty type — minmax_3arg).
    // The view unstrips nil only, never constants, so the D4 note
    // above still holds.
    AType *part = fa->type_world.bottom_type;
    if (avpos)
      for (AEdge *x : these_edges) {
        AVar *a = x->args.get(avpos);
        if (a) part = type_union(part, split_type_view(a, nullptr));
      }
    // ifa/issues/101: what KIND of CreationSet is the splitter actually
    // partitioning on? `closure` CSs are minted unique per site x contour
    // (creation_point's `if (s == sym_closure) goto Lunique` bypasses
    // reuse), so a position receiving closures has a type that grows with
    // the contour count and every partition it yields is necessarily
    // first-time. Probe-only.
    // ifa/issues/104 pricing: for each split partition, report whether it
    // mixes list and tuple, and whether the TUPLE CreationSets in it are
    // HOMOGENEOUS (all fields the same type). Representation unification
    // can only merge a tuple into a list layout when it is homogeneous;
    // a heterogeneous tuple is a record and has no list form. Probe-only.
    // ifa/issues/104 (revised goal): how many split partitions contain
    // TUPLES OF THE SAME ELEMENT TYPE BUT DIFFERENT ARITY? Those are
    // distinct record types in pyc today, so any variable holding both
    // is a union -- yet they are ONE type under a list representation
    // (element type T, runtime length), which is exactly shedskin's
    // variable-length homogeneous `tuple<T>`. This is the population a
    // tuple-to-list representation would collapse.
    if (getenv("IFA_DBG_TUPARITY") && avpos) {
      std::map<void *, std::set<int>> arities_by_elem;  // element AType -> arities
      int nhomo = 0, nhet = 0;
      for (CreationSet *c : part->sorted) {
        if (!c->sym || c->sym != sym_tuple) continue;
        AType *first = nullptr;
        bool homo = true;
        int n = 0;
        for (AVar *fv : c->vars) if (fv) {
          ++n;
          if (!first) first = fv->out->type;
          else if (fv->out->type != first) { homo = false; break; }
        }
        if (!n) continue;
        if (!homo) { ++nhet; continue; }
        ++nhomo;
        arities_by_elem[(void *)first].insert(n);
      }
      int multi = 0;
      for (auto &kv : arities_by_elem) if (kv.second.size() > 1) ++multi;
      if (multi)
        fprintf(stderr, "TUPARITY fun=%s homo=%d het=%d elemtypes=%d MULTIARITY=%d\n",
                es->fun->sym->name ? es->fun->sym->name : "?", nhomo, nhet, (int)arities_by_elem.size(), multi);
    }
    if (getenv("IFA_DBG_TUPHOMO") && avpos) {
      int nlist = 0, ntuple = 0, nhomo = 0, nhet = 0;
      for (CreationSet *c : part->sorted) {
        if (!c->sym) continue;
        if (c->sym == sym_tuple) {
          ++ntuple;
          AType *first = nullptr;
          bool homo = true;
          for (AVar *fv : c->vars) {
            if (!fv) continue;
            if (!first)
              first = fv->out->type;
            else if (fv->out->type != first) { homo = false; break; }
          }
          if (c->vars.n <= 1) homo = true;
          if (homo) ++nhomo; else ++nhet;
        } else if (c->sym->name && !strcmp(c->sym->name, "list"))
          ++nlist;
      }
      if (nlist && ntuple)
        fprintf(stderr, "TUPHOMO mixed fun=%s list=%d tuple=%d homo=%d het=%d\n",
                es->fun->sym->name ? es->fun->sym->name : "?", nlist, ntuple, nhomo, nhet);
    }
    if (getenv("IFA_DBG_SPLITSYM") && avpos) {
      int nclosure = 0, ntotal = 0;
      for (CreationSet *c : part->sorted) {
        ++ntotal;
        if (c->sym == sym_closure) ++nclosure;
      }
      fprintf(stderr, "SPLITSYM p=%d stage=%d fun=%s n=%d closure=%d |", analysis_pass, cur_split_stage,
              es->fun->sym->name ? es->fun->sym->name : "?", ntotal, nclosure);
      for (CreationSet *c : part->sorted)
        fprintf(stderr, " %s", c->sym && c->sym->name ? c->sym->name : "?");
      fprintf(stderr, "\n");
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
    // issue 065 / 043 shape B: type-stage groups key on the type
    // partition (group_signature); setter/mark-stage groups key on the
    // cross-pass-STABLE setter SITES (setter_site_signature) so they can
    // route across passes too -- previously they were unroutable (the
    // per-ES setter-AVar identity shifts as splitting proceeds), which
    // left the container-method element-CS splits re-minting every pass.
    if (!fsetters && !fmark) {
      if (part != fa->type_world.bottom_type) gsig = group_signature(these_edges, es->fun);
    } else {
      gsig = setter_site_signature(these_edges, es->fun);
    }
    // Issue 074 Stage 1 / issue 065 gap 2: self-product complement eviction.
    // When this group's ledger home is es ITSELF, the group is es's recorded
    // canonical content and the current detach-the-group path re-derives it
    // forever (pygmy's period-2 flip-flop). Instead keep the group in es and
    // evict the type-compatible COMPLEMENT (stay_edges) to its OWN fresh
    // product, once, so es re-monomorphises to the recorded group. The other
    // do-groups are routed to their own homes by the loop below, so only
    // stay_edges needs re-homing here -- and it goes to a SEPARATE product
    // (never merged with the other groups), avoiding the amaze/linalg
    // regression of the "evict everything into one contour" first cut.
    //
    // ifa/issues/074 (PYC_SELFPROD): the `v > 0` case, which is the
    // majority of the oscillating set and was left as "needs the genuine
    // stale-vs-valid discrimination". Measured 2026-08-14: on all 11
    // TYPE_CONFLUENCE programs, EVERY cross-pass GROUP re-derivation has
    // `recorded == es` -- i.e. it is this case, disabled by the gate, and
    // the fallthrough mints a fresh contour every pass forever.
    //
    // The discriminator the earlier attempt lacked: with types still
    // widening, a recorded "home == es" may be stale, and evicting a
    // stay_edge that legitimately belongs mis-homes real content
    // (amaze 884->915, linalg 170->187). But an edge whose type at this
    // position is DISJOINT from the group's partition cannot belong to
    // the recorded group under any later widening -- type sets only grow,
    // and a disjoint set stays disjoint from `part` only if it never
    // acquires one of part's CSs. So evict only the disjoint complement
    // and leave every overlapping (hence possibly-valid) stay_edge alone.
    if (avpos && gsig && (nviol_this_pass == 0 || selfprod_enabled())) {
      SplitDecision *dd = fa->ledger_find(es->fun, cur_split_stage, avpos, part, gsig);
      if (dd && dd->pass_made != analysis_pass && dd->product == es) {
        bool conservative = nviol_this_pass != 0;
        int mode = selfprod_enabled();
        // Modes 3/4: the DURABLE TYPE KEY as the stale-vs-valid test.
        // `es->type_key` is captured in complete_pass, after the flow
        // fixpoint, so it is the whole-pass-invariant converged type at
        // this position -- unlike `->out->type`, which is this pass's
        // still-widening value and is what modes 1 and 2 (and the
        // 2026-07-30 attempt) tested against. The recorded decision
        // "partition `part` lives in `es`" is still VALID exactly when
        // `es` converged to `part` and nothing else; if its key has moved
        // off `part`, the contour is no longer the recorded partition's
        // home and the decision is stale, so fall through to a normal
        // split. Mode 3 then keeps the group and evicts nothing; mode 4
        // also evicts the complement, which is 1b's full action.
        if (conservative && mode >= 3) {
          bool ok;
          if (mode <= 4) {
            // 3/4: is `es` still exactly the recorded partition's home?
            AType *k = es->type_key_pass >= 0 ? es->type_key.get(avpos) : nullptr;
            ok = k == part;
          } else {
            // 5: has `es` LOCALLY converged? 1b's `nviol_this_pass == 0`
            // is a whole-program convergence proxy for "the recorded
            // home == es decision is settled"; the property it actually
            // needs is per-CONTOUR. A contour whose durable type key is
            // identical on two consecutive passes has stopped moving
            // even though the program has not, so its self-product is a
            // stable flip-flop (pygmy's case) rather than a union still
            // widening -- which is exactly the discrimination the
            // 2026-07-30 attempt lacked.
            // 5: stability only. 6: stability OR a period-2 flip-flop.
            //
            // ifa/issues/074: mode 5 cannot fire inside the very disease
            // it names. Measured on
            // tests/deepcopy_recursive_nested_growth.py: es 119 of `total`
            // is evaluated 40 times and returns "stale" on every one,
            // because the group this test would pin keeps being ejected
            // to es 138 and dragged back by the ledger route -- and that
            // motion is what changes 119's key each pass. A test that
            // demands "has stopped moving" can never hold while its own
            // remedy is what keeps the contour moving.
            //
            // A period-2 flip-flop IS settled, just not constant: the
            // contour has two states and alternates. That is exactly the
            // "stable flip-flop rather than a union still widening"
            // the mode-5 comment above describes, and capture_type_keys
            // already computes the predicate for its own `kd_flip`
            // counter.
            ok = es->type_key_pass == analysis_pass && es->key_hash[0] &&
                 (es->key_hash[0] == es->key_hash[1] ||
                  (mode >= 6 && es->key_hash[0] == es->key_hash[2] && es->key_hash[0] != es->key_hash[1]));
          }
          if (getenv("IFA_DBG_INCOMPAT"))
            fprintf(stderr, "SELFPROD p=%d fun=%s#%d es=%d %s\n", analysis_pass,
                    es->fun->sym->name ? es->fun->sym->name : "?", es->fun->sym->id, es->id,
                    ok ? "VALID" : "stale");
          if (!ok) goto Lnormal_split;  // stale: today's behaviour
          if (mode == 3) continue;      // valid: keep the group, evict nothing
        } else if (conservative && mode == 2) {
          // Mode 2: keep the group in `es` but evict NOTHING. Drops the
          // fresh-contour mint without re-homing any edge.
          continue;
        }
        if (!stay_evicted) {
          stay_evicted = true;
          EntrySet *scomp = nullptr;
          for (AEdge *x : dec->stay_edges) if (x && x->from && x->to == es) {
            if (conservative && mode == 1) {
              AVar *xa = x->args.get(avpos);
              if (!xa) continue;
              AType *xt = type_intersection(xa->out->type, x->match->formal_filters.get(avpos));
              if (!xt->n || type_intersection(xt, part) != fa->type_world.bottom_type) continue;
            }
            if (getenv("IFA_DBG_CHURN"))
              fprintf(stderr, "[churn-evict] p=%d stage=%d fun=%s es=%d edge_to=%d scomp=%d\n", analysis_pass,
                      cur_split_stage, es->fun->sym->name ? es->fun->sym->name : "?", es->id, x->to ? x->to->id : -1,
                      scomp ? scomp->id : -1);
            x->to = 0;
            if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_detach[cur_split_stage];
            x->filtered_args.clear();
            es->edges.del(x);
            if (!scomp) {
              set_entry_set(x);
              scomp = x->to;
              if (!scomp->split) scomp->split = es;
            } else
              set_entry_set(x, scomp);
            record_backedges(x, es, pending_es_backedge_map);
            if (getenv("IFA_DBG_CHURN"))
              fprintf(stderr, "[churn-scomp] p=%d e=%d es=%d -> %d\n", analysis_pass, x->id, es->id,
                      x->to ? x->to->id : -1);
            split = 1;
          }
        }
        continue;  // keep this (self-product) group in es
      }
    }
  Lnormal_split:;
    // ifa/issues/074: set when the ledger's recorded home is proven to be
    // no longer a home for this group. Read again in the record path
    // below, where mode 2 refreshes the entry.
    bool home_ok = true;
    if (avpos && gsig) {
      SplitDecision *d = fa->ledger_find(es->fun, cur_split_stage, avpos, part, gsig);
      route_d = d;  // ifa/issues/055: visible at the routing block below
      if (getenv("IFA_DBG_CHURN")) {
        fprintf(stderr, "[churn-look] p=%d fun=%s es=%d gsig=%u found=%d pass_made=%d product=%d self=%d",
                analysis_pass, es->fun->sym->name ? es->fun->sym->name : "?", es->id, gsig, d ? 1 : 0,
                d ? d->pass_made : -1, (d && d->product) ? d->product->id : -1,
                (d && d->product == es) ? 1 : 0);
        // ifa/issues/101: the EDGE SET behind the signature. Two ledger
        // records naming each other's contour are either one group
        // cycling (same edges) or two groups swapping (disjoint edges);
        // only the edge identities tell them apart.
        fprintf(stderr, " edges=%d [", these_edges.n);
        for (AEdge *x : these_edges) fprintf(stderr, " %d", x->id);
        fprintf(stderr, " ] from=[");
        for (AEdge *x : these_edges) fprintf(stderr, " %d", x->from ? x->from->id : -1);
        fprintf(stderr, " ]\n");
      }
      // The display no longer gates this ROUTE either (see update_display):
      // it was the dominant reason a group with a recorded product
      // re-minted instead of routing (074's `es_othermint`, 064's
      // phantom method display).
      // ifa/issues/074: is the recorded home still a home for THIS
      // group? The self-product branch above has an elaborate staleness
      // test (SELFPROD modes 3/4/5, on the durable type key); this ROUTE
      // had none at all, and the asymmetry is a period-2 generator.
      // Measured on tests/deepcopy_recursive_nested_growth.py: the
      // ledger routes a group of `total` into es 119 every even pass,
      // and the splitter -- correctly, because 119 also holds
      // stay_edges the group is type-incompatible with -- ejects it
      // again every odd pass, for ever. Re-check the group against what
      // actually lives in the product now, and treat an incompatible
      // home as a stale entry.
      if (routegate_enabled() && d && d->product && d->product != es && these_edges.n && these_edges[0]->args.n) {
        for (AEdge *y : d->product->edges) if (y && y->args.n) {
          bool c = fsetters ? edge_sset_compatible_with_edge(these_edges[0], y)
                            : (bool)edge_type_compatible_with_edge(these_edges[0], y, d->product, fmark);
          if (!c) { home_ok = false; break; }
        }
        if (getenv("IFA_DBG_CHURN") && !home_ok)
          fprintf(stderr, "[churn-stale] p=%d fun=%s es=%d product=%d INCOMPATIBLE-HOME\n", analysis_pass,
                  es->fun->sym->name ? es->fun->sym->name : "?", es->id, d->product->id);
      }
      if (d && d->pass_made != analysis_pass && d->product && d->product != es && home_ok) {
        product = d->product;
        // ifa/issues/101: break a ledger cycle. If `product` was itself
        // routed to `es` earlier, the two contours are, by the ledger's
        // own testimony, homes for the same group; following both
        // records alternates for ever. Pin deterministically to the
        // lower id -- and when that is `es`, decline the route entirely
        // so the group simply stays put.
        // Mode 3: keep the ROUTE RELATION ACYCLIC. Refuse (by pinning to
        // the cycle's lowest-id member) any route that would close a
        // cycle of any length, instead of pattern-matching the 2-cycle.
        if (routecycle_enabled() >= 3) {
          Vec<EntrySet *> path, seen;
          if (route_reaches(product, es, path, seen)) {
            EntrySet *canon = es;
            for (EntrySet *x : path) if (x && x->id < canon->id) canon = x;
            if (getenv("IFA_DBG_CHURN"))
              fprintf(stderr, "[churn-cycleN] p=%d fun=%s es=%d -> %d closes a %d-cycle, pinning to %d\n",
                      analysis_pass, es->fun->sym->name ? es->fun->sym->name : "?", es->id, product->id, path.n,
                      canon->id);
            d->product = canon;
            if (canon == es) continue;  // already home: no route, no split
            product = canon;
          }
          if (product) {
            Vec<EntrySet *> *adj = fa->route_adj.get(es);
            if (!adj) { adj = new Vec<EntrySet *>; fa->route_adj.put(es, adj); }
            // Mode 4 is the literal "never re-assign to an EntrySet this
            // source has been routed to before". It is STRICTLY stronger
            // than mode 3 and, unlike it, refuses the STABLE case too:
            // in the steady state a re-derived group is routed to the
            // same home every pass, which is the ledger working, not
            // churning. Kept as a knob to make that difference
            // measurable rather than argued.
            if (routecycle_enabled() >= 4 && adj->in(product)) continue;
            adj->set_add(product);
          }
        } else if (routecycle_enabled()) {
          EntrySet *back = fa->route_last.get(product);
          if (back == es) {
            // A <-> B. Measured on linalg's __deepcopy__: ONE edge (2633,
            // from contour 797), one group, two signatures -- the callee
            // is recursive, so its result flows back into the caller's
            // variable and feeds the next call's argument, giving the
            // group two argument-type states that each name the other's
            // contour. No signature computed from current types can be
            // stable through that; the period-2 cycle IS the settled
            // state, exactly as for PYC_SELFPROD=6.
            EntrySet *canon = (es->id < product->id) ? es : product;
            if (getenv("IFA_DBG_CHURN"))
              fprintf(stderr, "[churn-cycle] p=%d fun=%s es=%d <-> product=%d, pinning to %d\n", analysis_pass,
                      es->fun->sym->name ? es->fun->sym->name : "?", es->id, product->id, canon->id);
            if (routecycle_enabled() >= 2) {
              // Pin the ledger to the canonical contour so BOTH
              // signatures agree from here on, then leave the group
              // alone when it is already there. Mode 1 instead declined
              // the route, which only sent the group down the mint path
              // and regrew contours (linalg ess 722 -> 901).
              d->product = canon;
              if (canon == es) {
                fa->route_last.put(es, es);
                continue;  // already home: no route, no split
              }
              product = canon;
            } else if (es->id < product->id)
              product = nullptr;  // mode 1: keep the group here (measured: worse)
          }
          if (product) fa->route_last.put(es, product);
        }
        ++fa->dup_split_attempts;
        if (getenv("IFA_DBG_INCOMPAT"))
          fprintf(stderr, "REDERIVE p=%d ROUTE fun=%s#%d es=%d -> product=%d first_pass=%d\n", analysis_pass,
                  es->fun->sym->name ? es->fun->sym->name : "?", es->fun->sym->id, es->id, d->product->id,
                  d->pass_made);
        log(LOG_SPLITTING, "[ledger] ROUTE group es %d fun %s %d pos %p part %p/%d sig %u -> product %d (first pass %d)\n",
            es->id, es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, (void *)avpos, (void *)part,
            part->sorted.n, gsig, d->product->id, d->pass_made);
      }
    }
    if (product) {
      if (!product->split) product->split = es;
      // ifa/issues/055: is this the same route as last pass?
      bool stable_route = route_d && (route_d->last_route_pass == analysis_pass - 1) &&
                          (route_d->last_route_product == product);
      if (route_d) { route_d->last_route_pass = analysis_pass; route_d->last_route_product = product; }
      for (AEdge *x : these_edges) {
        if (getenv("IFA_DBG_CHURN"))
          fprintf(stderr, "[churn] p=%d stage=%d fun=%s es=%d edge_to=%d product=%d noop=%d\n", analysis_pass,
                  cur_split_stage, es->fun->sym->name ? es->fun->sym->name : "?", es->id, x->to ? x->to->id : -1,
                  product->id, x->to == product ? 1 : 0);
        x->to = 0;
        if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_detach[cur_split_stage];
        x->filtered_args.clear();
        es->edges.del(x);
        set_entry_set(x, product);
        record_backedges(x, es, pending_es_backedge_map);
        if (getenv("IFA_DBG_CHURN"))
          fprintf(stderr, "[churn-ledger] p=%d e=%d es=%d -> %d%s\n", analysis_pass, x->id, es->id,
                  product ? product->id : -1, stable_route ? " (stable, no progress)" : "");
        // ifa/issues/055: routing a re-derived group to the SAME home it
        // went to on the previous pass is the ledger working, not new
        // information. Claiming progress for it keeps analyze_again true
        // for ever: measured on plcfrs and its 36-line repro as
        // `TYPE_CONFL returned=1 d_ess=0 d_css=0` on every pass to the
        // cap, which starves SETTER and everything after it. Do the
        // routing, but do not call it progress.
        if (!stable_route || routestable_enabled() == 0) split = 1;
        log(LOG_SPLITTING, "SPLIT ES %d (ledger) %s %d from %d -> %d\n", es->id,
            es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id, x->pnode->lvals[0]->sym->id,
            x->to->id);
      }
    } else {
      for (AEdge *x : these_edges) {
        if (getenv("IFA_DBG_CHURN"))
          fprintf(stderr, "[churn-mint] p=%d stage=%d fun=%s es=%d edge_to=%d gsig=%u\n", analysis_pass,
                  cur_split_stage, es->fun->sym->name ? es->fun->sym->name : "?", es->id, x->to ? x->to->id : -1,
                  gsig);
        x->to = 0;
        if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_detach[cur_split_stage];
        x->filtered_args.clear();
        es->edges.del(x);
      }
      for (AEdge *x : these_edges) {
        Vec<AEdge *> new_edges;
        cur_split_type_only = (!fsetters && !fmark) ? 1 : 0;
        make_entry_set(x, new_edges, es, e->to);
        cur_split_type_only = 0;
        if (getenv("IFA_DBG_CHURN"))
          fprintf(stderr, "[churn-mint-to] p=%d es=%d -> %d\n", analysis_pass, es->id, x->to ? x->to->id : -1);
        if (x->to != es) {
          record_backedges(x, es, pending_es_backedge_map);
          split = 1;
          log(LOG_SPLITTING, "SPLIT ES %d %s%s%s %d from %d -> %d\n", es->id, fsetters ? "setters " : "",
              fmark ? "marks " : "", es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id,
              x->pnode->lvals[0]->sym->id, x->to->id);
        }
      }
      // Issue 033 stage A: record the group's product for cross-pass
      // routing. `gsig` was computed above for the right stage (type ->
      // group_signature, setter/mark -> setter_site_signature); a
      // setter/mark group records regardless of the (often bottom) type
      // partition. gsig == 0 means no stable identity this pass -- neither
      // route nor record.
      if (avpos && gsig) {
        EntrySet *gproduct = nullptr;
        for (AEdge *x : these_edges) if (x->to && x->to != es) {
          gproduct = x->to;
          break;
        }
        if (gproduct) {
          SplitDecision *d = fa->ledger_find(es->fun, cur_split_stage, avpos, part, gsig);
          if (!d)
            fa->ledger_add(es->fun, cur_split_stage, avpos, part, gproduct, gsig);
          else if (d->pass_made != analysis_pass) {  // intra-pass repeats aren't re-derivation
            ++fa->dup_split_attempts;
            ++fa->rederive_churn;  // minted a product the ledger already named
            // ifa/issues/074 mode 2: the recorded home was proven stale
            // above (the group is type-incompatible with what now lives
            // in it), and the splitter has just re-homed the group
            // somewhere that IS compatible. Freezing the pass-11 answer
            // is what makes the oscillation permanent -- the route drags
            // the group back every other pass and the splitter correctly
            // ejects it again. Adopt the new home instead. Deliberately
            // NOT an unconditional refresh: the ledger's value is that a
            // decision is durable, so it is only revised on proof that
            // it has become wrong.
            if (routegate_enabled() >= 2 && !home_ok) {
              if (getenv("IFA_DBG_CHURN"))
                fprintf(stderr, "[churn-refresh] p=%d fun=%s es=%d %d -> %d\n", analysis_pass,
                        es->fun->sym->name ? es->fun->sym->name : "?", es->id, d->product ? d->product->id : -1,
                        gproduct->id);
              d->product = gproduct;
              d->pass_made = analysis_pass;
            }
            if (getenv("IFA_DBG_INCOMPAT"))
              fprintf(stderr, "REDERIVE p=%d GROUP fun=%s#%d es=%d recorded=%d now=%d first_pass=%d\n",
                      analysis_pass, es->fun->sym->name ? es->fun->sym->name : "?", es->fun->sym->id, es->id,
                      d->product ? d->product->id : -1, gproduct->id, d->pass_made);
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
// ifa/issues/111: the four populations AVar state lives in. Named once
// here because every attempt that open-coded them missed one:
//   1. Var::avars, reached via allsyms + pdb->funs (as foreach_var)
//   2. cs->vars
//   3. the CreationSet ELEMENT AVar (get_element_avar)
//   4. e->filtered_args
// clear_results reaches all four through clear_var/clear_cs/clear_edge;
// anything else that needs them should use this rather than a fifth
// hand-rolled walk.
template <class F>
static void foreach_avar(F f) {
  auto do_var = [&](Var *v) {
    for (int i = 0; i < v->avars.n; i++)
      if (v->avars[i].key) f(v->avars[i].value);
  };
  for (Sym *sy : fa->pdb->if1->allsyms) if (sy->var) do_var(sy->var);
  for (Fun *fn : fa->pdb->funs) for (Var *v : fn->fa_all_Vars) do_var(v);
  for (CreationSet *cs : fa->all_creation_sets) if (cs) {
    for (AVar *a : cs->vars) if (a) f(a);
    if (cs->added_element_var) { AVar *ev = get_element_avar(cs); if (ev) f(ev); }
  }
  for (AEdge *e : fa->all_aedges) if (e)
    form_MPositionAVar(x, e->filtered_args) if (x->value) f(x->value);
}

// ifa/issues/111 M3 (third cut): scope the VALUE reset with a predicate
// applied HERE, instead of maintaining a second enumeration.
//
// AVar state lives in four populations -- Var::avars via foreach_var,
// cs->vars, e->filtered_args, and the CreationSet element AVar via
// get_element_avar -- and clear_results' walk is the only code that
// knows all four. Earlier cuts re-implemented that walk and kept
// discovering populations by hitting assertion failures. Every one of
// those funnels through clear_avar, so gating it here is complete by
// construction.
//
// Non-null means selective: clear only AVars in the set.
static Vec<AVar *> *fa_clear_only = nullptr;

static void clear_avar(AVar *av) {
  if (fa_clear_only && !fa_clear_only->set_in(av)) {
    // Preserved: keep the value AND its setter equivalence class.
    // setter_class is assigned by a SPLITTER STAGE, which only visits
    // what the propagation re-derived -- so a preserved AVar is never
    // re-classed. same_eq_classes asserts every member of a Setters set
    // has a class, so zeroing the pair here (two earlier attempts) left
    // preserved AVars classless inside sets rebuilt this pass. Setter
    // state therefore travels WITH the value state: preserve both, and
    // preserve the interning table (cannonical_setters, clear_results)
    // so the class pointers stay valid.
    if (av->lvalue) clear_avar(av->lvalue);
    return;
  }
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
  // `match` is null on an edge get_AEdges minted for a (pnode, fun)
  // pair that dispatch has not bound yet -- reachable now that
  // clear_results walks fa->all_aedges rather than only the edges of
  // reached contours (ifa/issues/098). Such an edge has nothing else
  // per-pass on it, but the null check has to be here.
  if (e->match) e->match->formal_filters.clear();
  form_MPositionAVar(x, e->filtered_args) clear_avar(x->value);
}

// ifa/issues/111 M3: EntrySets whose constraints must be rebuilt.
// Non-null means selective.
static Vec<EntrySet *> *fa_rebuild_only = nullptr;

static void clear_es(EntrySet *es) {
  // Structural, rebuilt by the top-edge TRAVERSAL, which still runs in
  // full -- so it is cleared in full. Left alone it does not go stale,
  // it ACCUMULATES, because the traversal appends to it.
  es->out_edges.clear();
  es->backedges.clear();
  es->cs_backedges.clear();
  es->creates.clear();
  // live_pnodes is the actual lever: an ES that keeps it skips
  // add_es_constraints entirely, which is the per-pass work being
  // saved. An ES whose AVars were cleared MUST lose it -- clear_avar
  // dropped the flow edges and add_es_constraints is the only thing
  // that rebuilds them.
  if (!fa_rebuild_only || fa_rebuild_only->set_in(es)) es->live_pnodes.clear();
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

// ifa/issues/098: `fa->pdb->funs`, not `fa->funs`. The latter is the
// set of functions the last completed pass REACHED, so a function that
// drops out of the call graph for a pass and comes back later kept its
// AVars' values across the gap. pdb->funs is every function FA can
// dispatch to (build_patterns/build_arg_positions are built from it),
// so it is the correct domain for a reset -- and a strict superset, so
// the post-convergence users of foreach_var (set_void_lub_types_to_void,
// remove_unused_closures) just visit some extra bottom-typed AVars,
// which are no-ops for both.
static void foreach_var(void (*pfn)(Var *)) {
  for (Sym *s : fa->pdb->if1->allsyms) if (s->var) pfn(s->var);
  for (Fun *f : fa->pdb->funs) for (Var *v : f->fa_all_Vars) pfn(v);
}

struct ClearVarFn {
  static void F(Var *v) { clear_var(v); }
};

// Reset every piece of state a pass derives, so the next pass starts
// from bottom everywhere. See FA::all_aedges (fa.h) for why this walks
// the authoritative registries rather than `fa->ess`/`fa->css` (which
// describe what the last pass reached, not what needs resetting), and
// analyze_to_convergence for why it now runs before EVERY pass rather
// than only after a splitting one. Identity-carrying state that must
// survive a pass is listed in clear_avar's comment.
// ifa/issues/111 M3: the set the NEXT pass must clear. Computed here,
// at end of pass, because clear_avar destroys `forward` -- by the time
// analyze_to_convergence wants it, the graph it is derived from is
// gone. Empty means "clear nothing", which is only correct after a
// pass that changed nothing; analyze_to_convergence checks
// fa_selective_armed rather than emptiness so the very first pass
// (which has no predecessor state to preserve) still clears fully.
static Vec<AVar *> fa_invalidate_avars;
static bool fa_selective_armed = false;

// ifa/issues/111 M3: clear only what the last pass's splitting could
// have invalidated, instead of the whole program.
//
// The set is fa_invalidate_avars, computed by
// probe_invalidation_closure() at END of the previous pass (it must be:
// clear_avar destroys `forward`, the graph the closure is derived
// from). Soundness rests on the invariant in the issue -- within a pass
// the fixed point only GROWS, a split REFINES, so a split contour's
// AVars can end lower and must go to bottom, as must anything
// transitively forward-reachable from them; everything else has
// unchanged inputs and its old value is still its fixed point.
//
// Returns false when it declines, so the caller falls back to the full
// reset rather than silently doing less.
static void clear_results();  // ifa/issues/111 M3: selective path reuses it

static bool clear_results_selective() {
  if (!ifa_selective || !fa_selective_armed) return false;

  // What to reset: the closure the last pass's splitting invalidated,
  // plus every AVar of an EntrySet the last pass did NOT reach. The
  // second set is ifa/issues/098's trap -- such an ES holds values from
  // whenever it was last reached, which no amount of forward-
  // reachability from THIS pass's splits will find. `ess_set` is the
  // reached set; `all_entry_sets` is authoritative.
  static Vec<AVar *> to_clear;
  static Vec<EntrySet *> to_rebuild;
  to_clear.clear();
  to_rebuild.clear();
  for (AVar *av : fa_invalidate_avars) if (av) to_clear.set_add(av);
  // Never preserve an AVar that participates in setter equivalence.
  // setter_class is assigned ONLY by split_eq_class, driven by the
  // splitter over what the propagation re-derived; a preserved AVar is
  // never re-classed, and same_eq_classes asserts every member of a
  // Setters set has a class. Adding the members of every live Setters
  // set to the closure keeps that invariant by construction.
  {
    Vec<AVar *> setter_members;
    auto note = [&](AVar *a) {
      if (!a) return;
      if (a->setters) for (AVar *m : *a->setters) if (m) setter_members.set_add(m);
      if (a->lvalue && a->lvalue->setters)
        for (AVar *m : *a->lvalue->setters) if (m) setter_members.set_add(m);
    };
    for (Sym *sy : fa->pdb->if1->allsyms) if (sy->var)
      for (int i = 0; i < sy->var->avars.n; i++)
        if (sy->var->avars[i].key) note(sy->var->avars[i].value);
    for (Fun *f : fa->pdb->funs) for (Var *v : f->fa_all_Vars)
      for (int i = 0; i < v->avars.n; i++)
        if (v->avars[i].key) note(v->avars[i].value);
    for (CreationSet *cs : fa->all_creation_sets) if (cs)
      for (AVar *a : cs->vars) note(a);
    for (AVar *m : setter_members) if (m) to_clear.set_add(m);
  }
  for (EntrySet *es : fa->all_entry_sets) {
    if (!es || fa->ess_set.set_in(es)) continue;
    to_rebuild.set_add(es);
    for (Var *v : es->fun->fa_Vars) to_clear.set_add(make_AVar(v, es));
  }

  // An AVar's owning EntrySet needs its constraints rebuilt too:
  // clear_avar drops the flow edges and only add_es_constraints
  // restores them.
  Vec<void *> es_ptrs;
  for (EntrySet *es : fa->all_entry_sets) if (es) es_ptrs.set_add((void *)es);
  for (AVar *av : to_clear)
    if (av && av->contour && es_ptrs.set_in(av->contour))
      to_rebuild.set_add((EntrySet *)av->contour);

  // ifa/issues/111 M3 option 2: setter state is only valid while every
  // member of a Setters set carries a class, and classes come ONLY from
  // compute_setters over THIS pass's confluences. Clearing AVar X
  // therefore invalidates the setter state of every AVar whose set
  // NAMES X -- a backward step the forward closure cannot supply. Zero
  // those sets (not their values) so the pass rebuilds them; the
  // alternative is same_eq_classes asserting on a classless member.
  foreach_avar([&](AVar *a) {
    if (!a || !a->setters) return;
    for (AVar *m : *a->setters)
      if (m && to_clear.set_in(m)) {
        a->setters = 0;
        a->setter_class = 0;
        return;
      }
  });

  // One walk, the SAME walk the full reset uses -- the predicates are
  // applied inside clear_avar and clear_es. That is the whole point of
  // this cut: a parallel enumeration kept missing AVar populations.
  fa_clear_only = &to_clear;
  fa_rebuild_only = &to_rebuild;
  clear_results();
  fa_clear_only = nullptr;
  fa_rebuild_only = nullptr;

  for (EntrySet *es : to_rebuild) if (es && !es->in_es_worklist) {
    es->in_es_worklist = 1;
    fa->es_worklist.enqueue(es);
  }

  if (getenv("IFA_DBG_CLOSURE"))
    fprintf(stderr, "SELCLEAR pass=%d cleared_avars=%d rebuilt_ess=%d of %d\n",
            analysis_pass, to_clear.count(), to_rebuild.count(), fa->all_entry_sets.n);
  return true;
}

static void clear_results() {
  foreach_var(clear_var);
  for (CreationSet *cs : fa->all_creation_sets) clear_cs(cs);
  for (EntrySet *es : fa->all_entry_sets) clear_es(es);
  for (AEdge *e : fa->all_aedges) clear_edge(e);
  // ifa/issues/111 M3: under selective invalidation the interning table
  // must survive -- preserved AVars keep setter_class pointers into it
  // (see clear_avar). Content-keyed, so cleared AVars re-intern against
  // the same entries.
  if (!fa_clear_only) fa->type_world.cannonical_setters.clear();
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
  // unannotated lets the mix cycle back in through the frame.
  //
  // issue 025 (fysphun): user record fields with a PURE numeric mix
  // (`self.x = 0` in __init__, then float values assigned -- a
  // `int64 | float64` field) get the same treatment. Without it the
  // field has no single C type and codegen degrades it to `_CG_void`,
  // breaking arithmetic on it (`(double)(void*)`). coerce_annotate
  // self-gates on a pure-numeric mix -- a field holding class
  // instances, or numeric MIXED with a pointer/class type, has a
  // non-numeric member and is left untouched, so this stays clear of
  // the classtag-dispatch machinery's domain (issues 029/030); only
  // the int-vs-float unboxed-scalar unification pyc already applies to
  // locals is extended to fields.
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym) continue;
    bool eligible = cs->sym == sym_closure || (cs->sym->type && cs->sym->type->type_kind == Type_RECORD);
    if (eligible)
      for (AVar *av : cs->vars) annotated += coerce_annotate(av);
    // issues/035: a container's ELEMENT gets the same treatment --
    // deliberately OUTSIDE the `eligible` test above, since a list's
    // CreationSet is neither a closure frame nor a Type_RECORD. `x = n
    // * [0]` then `x[i] += 1.5` leaves the element a pure `int64|float64`
    // mix with no single C type, and codegen then trips the
    // element/value num_kind guard at run time. Widening it to float
    // makes the program run, and produces byte-for-byte what shedskin
    // gives for the same source -- its typestr.py has the identical
    // special case (`{int_, float_}` -> `float_`) ahead of its
    // "dynamic (sub)type" rejection. `cs->vars` does not reach the
    // element AVar, hence get_element_avar.
    // Guarded on `added_element_var` so this never MATERIALISES one --
    // calling get_element_avar unconditionally perturbs
    // collect_type_confluence program-wide on every pass (see
    // split_container_methods_per_element_cs's note).
    if (cs->added_element_var) annotated += coerce_annotate(get_element_avar(cs));
  }
  // The re-run must re-derive flow from scratch: unlike the
  // monotone-growth reanalyze repairs (field promotion), coercion
  // changes what an existing out computes, which is only legal from a
  // clean slate. That is no longer this function's job -- since
  // ifa/issues/098, analyze_to_convergence clears before EVERY pass, so
  // returning nonzero is sufficient to get the clean slate (and the
  // explicit clear here would have been a redundant second pass over
  // every Var, contour and edge).
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
  // ifa/issues/055 probe: does the seed actually reach update_setter?
  static cchar *want = nullptr;
  static int checked = 0;
  if (!checked) { want = getenv("PYC_DBG_SETTERSEED"); checked = 1; }
  int n_dir = 0, n_classed = 0, n_container = 0;
  if (want) {
    for (AVar *x : *dir) if (x) { ++n_dir; if (x->setter_class) ++n_classed; if (x->setter_class && x->container) ++n_container; }
  }
  for (AVar *x : *dir) if (x && x->setter_class) setters_changed |= update_setter(x->container, x, avs);
  if (want && av->var && av->var->sym && av->var->sym->name && !strcmp(av->var->sym->name, want))
    fprintf(stderr, "SETSEED p=%d av=%d %s akind=%d dir=%d classed=%d with_container=%d changed=%d\n",
            analysis_pass, av->id, want, akind, n_dir, n_classed, n_container, setters_changed);
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
// ifa/issues/066: drop the per-pass term from the CS split signature so
// the ledger can recognise a re-derived split. See the use below.
// Defaults to 3 (the durable setter type: canonical SET of split-chain
// roots) as of 2026-08-16. Measured over the whole shedskin corpus at
// ZERO exit-code changes and zero changes to violations/ess/css/
// final_pass on all 77 programs, +1.1% analysis time, with the full test
// suite unchanged -- while cutting
// tests/deepcopy_recursive_nested_growth.py's runaway contour growth to
// the best figure any mode reaches (ess 272 -> 144 at pass 102, CS mints
// 20 -> 4).
//
// The other modes are kept as controls, and the pair of them is the
// argument for 3: mode 1 (drop the setter type outright) reaches the
// same 144 but pays for it in precision -- linalg 27 -> 74 violations,
// plcfrs losing 173 contours' worth -- while mode 0 keeps the precision
// and none of the convergence. Mode 3 is the first to get both, which is
// what makes it defaultable. Mode 2 is superseded: same idea, but hashed
// over the raw `sorted` sequence, so it was order- and
// multiplicity-sensitive and only got half way (164).
static int cskey_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSKEY");
    e = v ? atoi(v) : 3;
  }
  return e;
}

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
      // ifa/issues/066 (PYC_CSKEY): the setter's TYPE is the only
      // per-pass term in this signature -- every other input is a
      // source-level Sym id. Including it means a CS split re-derived on
      // a later pass hashes differently and the ledger cannot recognise
      // it, so `split_css` clones the same CreationSet again, each clone
      // becoming the next parent. Measured on
      // tests/deepcopy_recursive_nested_growth.py: 20 splits, `found=0`
      // on every one, a fresh `list` CS every ~10 passes for ever. The
      // type is still required to have FLOWED (the guard above); it just
      // does not get to be part of the identity.
      if (cskey_enabled() == 1) {
        // 1: drop the type entirely. Durable, and it does stop the
        // growth -- but too blunt: it merges splits the setter type
        // legitimately separated, taking linalg from 27 to 74 violations
        // and plcfrs's contour count down with its precision. Kept only
        // as the control that proves which term is at fault.
        h += (uint)combine_hash((uintptr_t)s->var->sym->id, (uintptr_t)s->var->sym->id);
      } else if (cskey_enabled() == 2) {
        // 2: SUPERSEDED BY 3, kept as the intermediate control. Keep the
        // type, but identify each of its CreationSets by the ROOT of its
        // split chain. A CS and its clones then hash alike --
        // which is the whole point, since the clone chain is what makes
        // the signature drift -- while two genuinely different lists
        // still differ.
        //
        // This reads `split_origin`, NOT `split`. The first attempt walked
        // `split` and was inert -- byte-identical to mode 0 -- because
        // clear_splits() zeroes `split` at the top of every pass: it is a
        // within-pass scratch marker, and the chain we need to collapse
        // spans ~10 passes per link. `split_origin` is set once in the
        // clone constructor and never cleared.
        uintptr_t th = 0;
        int i2 = 0;
        for (CreationSet *c : s->out->type->sorted) {
          CreationSet *root = c->split_origin ? c->split_origin : c;
          th += (uintptr_t)root->id * open_hash_primes[i2++ % 256];
        }
        h += (uint)combine_hash((uintptr_t)s->var->sym->id, th);
      } else if (cskey_enabled() == 3) {
        // 3: the durable setter type. Mode 2's idea -- identify the
        // type's CreationSets by their split-chain root -- but hashed as
        // a canonical SET rather than the raw `sorted` sequence. Mode 2
        // got that wrong twice, and the trace showed both:
        //
        //  * ORDER. `sorted` is ordered by CS *id*, so mapping to roots
        //    permutes it: at p10 the roots were [1005, 1038] and at p20
        //    the same two roots arrived as [1038, 1091->1005]. With
        //    position-indexed primes the identical set hashed
        //    differently, and the p20 split could not match p10's.
        //  * MULTIPLICITY. As the clone chain grows a setter's type
        //    accumulates several clones of ONE original (p39:
        //    `1091(r1005) 1112(r1005)`). Mapping to roots turns that
        //    into {1005, 1005}, which must be the same identity as
        //    {1005} -- the clones are the very thing being collapsed.
        //
        // So: dedupe the roots, sort them, then hash. Note the
        // set_to_vec() -- Vec::set_add leaves null holes that iteration
        // would otherwise walk into.
        Vec<CreationSet *> roots;
        for (CreationSet *c : s->out->type->sorted) roots.set_add(c->split_origin ? c->split_origin : c);
        roots.set_to_vec();
        Vec<int> rids;
        for (CreationSet *r : roots) if (r) rids.add(r->id);
        qsort(rids.v, rids.n, sizeof(rids[0]),
              [](const void *a, const void *b) { return *(const int *)a - *(const int *)b; });
        uintptr_t th = 0;
        int i2 = 0;
        for (int rid : rids) th += (uintptr_t)rid * open_hash_primes[i2++ % 256];
        h += (uint)combine_hash((uintptr_t)s->var->sym->id, th);
      } else
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
        // Issue 066 part 1 (enforcement): the CS-side analog of the
        // issue-033 ES product routing (4498-4513). cs_group_signature
        // keys this split group on stable IR (cs->sym + def-Var sym ids
        // + setter value types), so a group re-derived on a later pass
        // is re-attached to the CreationSet it was first moved into
        // instead of minting a fresh duplicate every pass. See the
        // `route` predicate below for the (deliberately narrow) case
        // this enforces and the self-product case it leaves alone.
        uint csig = cs_group_signature(cs, compatible_set);
        SplitDecision *d = csig ? fa->ledger_find_cs(csig) : nullptr;
        // Route ONLY when the recorded home is a DIFFERENT CreationSet:
        // move the re-derived group straight into that durable duplicate
        // instead of minting a fresh one. The self-product case
        // (d->cs_product == cs) is deliberately NOT routed here -- the
        // group's home IS the CS we are splitting from, but so is a
        // remainder of a different setter class that arrived on reflow,
        // and this peel-one-class-per-iteration loop cannot keep the
        // home group while evicting that remainder without merging the
        // two (measured: it collapses a dispatch distinction and
        // pyc_declare crashes with "matching function not found"). So
        // self-product falls back to the original mint + record-only DUP
        // count (the 065-gap-2 analog, left to the phase-ordering half).
        bool route = d && d->pass_made != analysis_pass && d->cs_product && d->cs_product != cs;
        // ifa/issues/066: dump the SETTER TYPE composition behind csig, so
        // a csig that drifted between two passes can be attributed --
        // genuinely new CreationSets flowing in, vs. clone identity that
        // split_origin should already have collapsed. Probe-only.
        if (getenv("IFA_DBG_CSTYPE")) {
          fprintf(stderr, "[cstype] p=%d cs=%d csig=%u", analysis_pass, cs->id, csig);
          for (AVar *v : compatible_set) if (v && v->setters)
            for (AVar *st : *v->setters) if (st) {
              fprintf(stderr, " | sv%d:", st->var->sym->id);
              for (CreationSet *c : st->out->type->sorted) {
                CreationSet *r = c->split_origin ? c->split_origin : c;
                fprintf(stderr, " %d%s", c->id, r != c ? "" : "*");
                if (r != c) fprintf(stderr, "(r%d)", r->id);
              }
            }
          fprintf(stderr, "\n");
        }
        if (getenv("IFA_DBG_CSSPLIT"))
          fprintf(stderr, "[csledger] p=%d cs=%d csig=%u found=%d pass_made=%d product=%d self=%d route=%d\n",
                  analysis_pass, cs->id, csig, d ? 1 : 0, d ? d->pass_made : -1,
                  (d && d->cs_product) ? d->cs_product->id : -1,
                  (d && d->cs_product == cs) ? 1 : 0, route ? 1 : 0);
        cs->defs.move(new_defs);
        CreationSet *new_cs;
        if (route) {
          new_cs = d->cs_product;  // route into the recorded duplicate
          ++fa->cs_dup_split_attempts;
          log(LOG_SPLITTING, "[ledger] ROUTE CS split cs %d sym %s %d sig %u -> product cs %d (first pass %d)\n",
              cs->id, cs->sym->name ? cs->sym->name : "", cs->sym->id, csig, new_cs->id, d->pass_made);
        } else {
          new_cs = new CreationSet(cs);
          if (getenv("IFA_DBG_CSSPLIT"))
            fprintf(stderr, "[cssplit] p=%d sym=%s cs=%d -> %d\n", analysis_pass,
                    cs->sym && cs->sym->name ? cs->sym->name : "?", cs->id, new_cs->id);
          if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_csmint[cur_split_stage];
          new_cs->split = cs;
        }
        for (AVar *v : compatible_set) if (v) {
          assert(cs == v->cs_map->get(cs->sym));
          v->cs_map->put(cs->sym, new_cs);
        }
        analyze_again = 1;
        if (!route) {
          log(LOG_SPLITTING, "SPLIT CS %d %s %d -> %d\n", cs->id, cs->sym->name ? cs->sym->name : "", cs->sym->id,
              new_cs->id);
          // Record-only ledger (issue 033 D5): first-time groups RECORD
          // their durable home; a prior-pass re-derivation of the SAME
          // signature is a self-product DUP (counted for the stall
          // guard's oscillation signal, but still minted here -- see the
          // route comment above).
          if (!csig)
            log(LOG_SPLITTING, "[ledger] CS split cs %d -> %d NO IDENTITY (unflowed setter)\n", cs->id, new_cs->id);
          else if (!d) {
            fa->ledger_add_cs(csig, new_cs);
            log(LOG_SPLITTING, "[ledger] RECORD CS split cs %d sym %s %d sig %u product cs %d\n", cs->id,
                cs->sym->name ? cs->sym->name : "", cs->sym->id, csig, new_cs->id);
          } else if (d->pass_made != analysis_pass) {  // intra-pass repeats aren't re-derivation
            ++fa->cs_dup_split_attempts;
            ++fa->rederive_churn;  // minted a CS the ledger already named
            log(LOG_SPLITTING, "[ledger] DUP CS split cs %d sym %s %d sig %u (first pass %d, product cs %d)\n", cs->id,
                cs->sym->name ? cs->sym->name : "", cs->sym->id, csig, d->pass_made,
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
  if (nomark_enabled() < 2 && split_ess_setters_marks(setter_confluences)) return 1;
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
    ++tc_seen;
    if (av->contour_is_entry_set) {
      AVar *target = nullptr;
      if (!av->is_lvalue) {
        if (av->var->is_formal)
          target = av;
        else
          ++tc_skip_rval, log(LOG_SPLITTING, "[stage1] av %d ES/non-formal-rval skipped\n", av->id);
      } else {
        AVar *aav = unique_AVar(av->var, av->contour);
        if (is_return_value(aav))
          target = aav;
        else
          ++tc_skip_lval, log(LOG_SPLITTING, "[stage1] av %d ES/lval-non-return skipped\n", av->id);
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
        if (dec) ++tc_dec, decisions.add(dec);
      }
    } else {
      ++tc_skip_cs;
      log(LOG_SPLITTING, "[stage1] av %d CS-contour skipped (passes to stage2)\n", av->id);
    }
  }
  Vec<EntrySet *> applied;
  for (ESSplitDecision *dec : decisions) {
    if (applied.set_in(dec->es)) {
      ++tc_defer;
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
    if (const char *dn = getenv("IFA_DBG_DECIDE"))
      if (dec->es->fun && dec->es->fun->sym && dec->es->fun->sym->name && !strcmp(dec->es->fun->sym->name, dn))
        fprintf(stderr, "APPLY p=%d es=%d av=%d -> %d\n", analysis_pass, dec->es->id, dec->av->id, r);
    log(LOG_SPLITTING, "[stage1] av %d es %d apply -> %d\n", dec->av->id, dec->es->id, r);
    if (r) (dec->av->is_lvalue ? tc_return : tc_formal)++;
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
  fa_pass_retargeted.clear();  // ifa/issues/111 M1
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
// ifa/issues/104 option 1: fan a receiver position per CreationSet even
// when the CSs are of DIFFERENT container classes. The stage below
// otherwise bails on a mixed-class receiver ("one class per split"), and
// `{list, tuple}` is exactly that case -- which is why the union survives
// into the shared accessors. Measured on plcfrs: 61 of the mixed
// partitions are `__getitem__`, plus __eq__/len/__len__/__iter__/__lt__.
//
// This is what shedskin gets for free: list<T>::__getitem__ and
// tuple2<A,B>::__getitem__ are separate template instantiations, so no
// single __getitem__ ever sees a union.
static int recvfan_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_RECVFAN");
    e = v ? atoi(v) : 0;
  }
  return e;
}

// A container CreationSet: has an element, i.e. list/tuple/dict/set.
static bool cs_is_container(CreationSet *cs) {
  return cs && cs->sym && cs->sym->element;
}

static bool cs_is_per_cs_method_class(CreationSet *cs) {
  if (!cs || !cs->sym) return false;
  if (cs->sym->clone_methods_per_cs) return true;
  Sym *t = cs->sym->type ? unalias_type(cs->sym->type) : 0;
  return t && t->clone_methods_per_cs;
}

// ifa/issues/104: when true, this stage is running OUTSIDE its usual
// quiescence gate and must restrict itself to the mixed-container
// receivers it was lifted for -- fanning the ordinary
// per-cs-method-class receivers early perturbs stages 1-5, which is what
// the gate exists to prevent (measured: 19 suite failures).
static int per_cs_forced = 0;

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
      // ifa/issues/104: a receiver that is a MIXED-CLASS set of
      // containers (the `{list, tuple}` case) is fanned per CS when
      // PYC_RECVFAN is on, bypassing both the per-cs-method-class gate
      // and the one-class-per-split rule below.
      bool mixed_container = false;
      if (recvfan_enabled()) {
        mixed_container = true;
        Sym *c0 = 0;
        for (CreationSet *cs : av->out->type->sorted) {
          if (!cs_is_container(cs)) { mixed_container = false; break; }
          if (!c0) c0 = cs->sym;
          else if (c0 != cs->sym) c0 = (Sym *)-1;
        }
        // NOTE deliberately NOT requiring the classes to DIFFER. sunfish
        // slices a receiver whose type is `tuple | tuple` -- two distinct
        // CONCRETE tuple types, same class. Each has an element; their
        // SUM does not, so __pyc_getslice__'s `sizeof_element(self)`
        // fails at codegen with "non-container type". Fanning the
        // receiver per CreationSet gives each clone a monomorphic
        // receiver, which is what dispatch is for -- ifa/issues/109.
        (void)c0;
      }
      if (!mixed_container)
      for (CreationSet *cs : av->out->type->sorted) {
        if (!cs_is_per_cs_method_class(cs)) { all_flagged = false; break; }
        Sym *t = cs->sym->clone_methods_per_cs ? cs->sym : unalias_type(cs->sym->type);
        if (!cls) cls = t;
        else if (cls != t) { all_flagged = false; break; }  // one class per split
      }
      if (mixed_container) all_flagged = true;
      // Outside the quiescence gate, do the mixed-container fan only.
      if (per_cs_forced == 1 && !mixed_container) continue;
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

// ifa/issues/075 Piece 2: decide-then-apply for CSM. CSM partitions by
// CS IDENTITY, not by type compatibility, so this cannot reuse
// ESSplitDecision/decide_entry_set_split verbatim (075's own note) --
// it is the CS-identity analog: DECIDE snapshots the receiver union
// and its edges before any mutation; APPLY (mirroring split_edges'
// cs_es_map + (CS x display) fan-out, Piece 1 above) acts ONLY on that
// frozen snapshot, never on live state another decision in the same
// batch may
//
// MEASURED RESULT, corrected from the plan's framing: decide-then-
// apply alone does NOT remove failure-class 2. Applying more than one
// decision per pass is actively unsafe without Piece 3 (durable
// alloc-site keying) -- apply_csm_split's pick_display_variant mints a
// FRESH sibling EntrySet whenever it doesn't find a compatible one
// among the ones it minted so far THIS call; nothing gives those
// siblings a stable cross-pass identity the way
// find_or_make_filtered_entry_set's CS-level products already have
// (reused via `!es->filters.some_disjunction(filters)`). So a
// recurring incompatibility re-mints a NEW sibling every pass instead
// of reusing the last one, and batching many decisions per pass
// compounds that into unbounded EntrySet growth -- confirmed
// empirically: fa->ess.n went 123 -> 2747 in under 8 seconds on
// dijkstra2 before the per-pass apply cap below was restored, and a
// naive "scan every candidate, not just the first" version separately
// dropped the suite from 222/18 (Piece 1's number) to 9/241 by forcing
// get_element_avar's side effect broadly (fixed: the demand signal
// below is now read-only, gated on cs->added_element_var, exactly like
// 075's own dump-mode discipline required). Piece 3 is therefore a
// hard prerequisite for Piece 2 to add anything beyond Piece 1's
// already-capped behavior, not an optional refinement -- see the apply
// loop below.
// have already moved.
struct CSMSplitDecision : public gc {
  AVar *av = nullptr;
  EntrySet *es = nullptr;
  MPosition *p = nullptr;
  AType *ety = nullptr;    // av->out->type at decide time (frozen)
  Vec<AEdge *> all_edges;  // es->edges at decide time (frozen)
};

static CSMSplitDecision *decide_csm_split(AVar *av) {
  EntrySet *es = (EntrySet *)av->contour;
  if (!av->out || !av->out->type) return nullptr;
  MPosition *p = nullptr;
  form_MPositionAVar(x, es->args) {
    if (x->value == av) {
      p = x->key;
      break;
    }
  }
  if (!p) return nullptr;
  CSMSplitDecision *dec = new CSMSplitDecision;
  dec->av = av;
  dec->es = es;
  dec->p = p;
  dec->ety = av->out->type;
  for (AEdge *ee : es->edges) if (ee) dec->all_edges.add(ee);
  qsort_by_id(dec->all_edges);
  if (!dec->all_edges.n) return nullptr;
  return dec;
}

// Mirrors split_edges' body (cs_es_map + Piece 1's (CS x display) fan-
// out in its redispatch) exactly, EXCEPT it reads the receiver's type
// and edge set from the frozen `dec` snapshot instead of live AVar/ES
// state -- the only change decide-then-apply requires, since dec->es
// is guaranteed untouched-this-batch by the caller's per-ES dedup
// (split_container_methods_per_element_cs below), so `dec->all_edges`
// still all point at dec->es when this runs.
[[nodiscard]] static int apply_csm_split(CSMSplitDecision *dec) {
  int again = 0;
  EntrySet *es = dec->es;
  MPosition *p = dec->p;
  AType *ety = dec->ety;
  Map<CreationSet *, EntrySet *> cs_es_map;
  for (CreationSet *cs : ety->sorted) {
    Map<MPosition *, AType *> filters;
    filters.copy(es->filters);
    filters.put(p, make_AType(cs));
    EntrySet *tes = find_or_make_filtered_entry_set(es, filters);
    cs_es_map.put(cs, tes);
  }
  auto resolve_target = [&](AEdge *ee, CreationSet *cs) -> EntrySet * { return cs_es_map.get(cs); };

  auto redispatch = [](AEdge *ee, EntrySet *tes) {
    if (!tes || ee->to == tes) return;
    if (ee->to) ee->to->edges.del(ee);
    if (getenv("IFA_DBG_CHURN"))
      fprintf(stderr, "[churn-csm] p=%d stage=%d edge_to=%d tes=%d\n", analysis_pass, cur_split_stage,
              ee->to ? ee->to->id : -1, tes->id);
    ee->to = 0;
    if (cur_split_stage >= 0 && cur_split_stage < FA::kNumFAPassStages) ++fa->dbg_stage_detach[cur_split_stage];
    ee->filtered_args.clear();
    set_entry_set(ee, tes);
  };
  for (AEdge *ee : dec->all_edges) if (ee) {
    EntrySet *old = ee->to;
    bool all_compat = true;
    Vec<EntrySet *> targets;
    for (int i = 0; i < ety->sorted.n; i++) {
      EntrySet *tes = resolve_target(ee, ety->sorted[i]);
      if (!tes) {
        all_compat = false;
        break;
      }
      targets.add(tes);
    }
    if (!all_compat) continue;
    if (ety->sorted.n == 1)
      redispatch(ee, targets.v[0]);
    else {
      for (int i = 0; i < ety->sorted.n; i++) {
        if (!i)
          redispatch(ee, targets[i]);
        else
          ee = copy_AEdge(ee, targets[i]);
      }
    }
    if (ee->to != old) again = 1;
  }
  return again;
}

// ifa/issues/074: CARTESIAN-PRODUCT contour naming (PYC_CPA), pyc's
// analog of shedskin's dcpa -- the fix the mark investigation converged
// on. `PYC_CPAMARK` established that no *comparison* can help once a
// union exists (inside it the CreationSet sets are equal by
// construction, so only depth discriminates -- which is what MARK_TYPE
// was doing and why it both over-splits and is load-bearing). The union
// has to be prevented from becoming a contour NAME at all: when a
// positional formal's live type is a union of >= 2 CreationSets, fan the
// contour into one filtered contour per single CS and re-dispatch every
// edge across them. Each product is pinned to one CS at that position,
// so it cannot re-fire there.
//
// The decide/apply pair is 075's, unchanged -- `decide_csm_split` and
// `apply_csm_split` are already generic (record av/es/position/type,
// then `find_or_make_filtered_entry_set` per CS and fan the edges with
// `copy_AEdge`). ONLY the demand signal differs: 075 additionally
// requires the union to be same-container-with-divergent-elements,
// which is one special case of this.
//
// PYC_CPA=N caps the union size fanned (N is the cap, so PYC_CPA=4 fans
// unions of 2..4 CSs); 0 is off. The cap is the analog of shedskin's own
// dcpa limit -- the product multiplies across argument positions, and
// only one position per contour is fanned per pass so that growth is
// paced rather than taken all at once.
static int cpa_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CPA");
    e = v ? atoi(v) : 0;
  }
  return e;
}

[[nodiscard]] static int split_ess_cartesian_product() {
  int limit = cpa_enabled();
  if (limit < 2) return 0;
  int n_ess = fa->ess.n;
  Vec<CSMSplitDecision *> decisions;
  for (int i = 0; i < n_ess; i++) {
    EntrySet *es = fa->ess[i];
    if (!es || !es->fun || !es->fun->sym) continue;
    if (es->fun->split_unique) continue;
    bool has_edges = false;
    for (AEdge *ee : es->edges) if (ee) { has_edges = true; break; }
    if (!has_edges) continue;
    for (MPosition *p : es->fun->positional_arg_positions) {
      AVar *av = es->args.get(p);
      if (!av || !av->out || !av->out->type) continue;
      int n = av->out->type->sorted.n;
      if (n < 2 || n > limit) continue;
      // Already pinned to a single CS here by an earlier fan (or by any
      // other filtered split): the product exists, do not re-derive it.
      AType *f = es->filters.get(p);
      if (f && f->sorted.n == 1) continue;
      // Fan ONLY a union that is a fixed point -- one where every
      // incoming edge carries the whole union, so `etype == stype` and
      // TYPE_CONFLUENCE has nothing to see. If some edge carries a
      // strict subset, stage 1 can separate it on types alone and will;
      // fanning there is the over-approximation that costs contours for
      // nothing (measured: without this, the listcomp repro's
      // `list.append` went to 13 contours for 4 type combinations).
      bool fixpoint = true;
      for (AEdge *ee : es->edges) if (ee && ee->args.n && ee->match) {
        AVar *ea = ee->args.get(p);
        if (!ea) continue;
        AType *et = type_intersection(ea->out->type, ee->match->formal_filters.get(p));
        if (et->n && et != av->out->type) { fixpoint = false; break; }
      }
      if (!fixpoint) continue;
      CSMSplitDecision *dec = decide_csm_split(av);
      if (dec) {
        decisions.add(dec);
        break;  // one position per contour per pass -- pace the product
      }
    }
  }
  Vec<EntrySet *> applied;
  int analyze_again = 0;
  for (CSMSplitDecision *dec : decisions) {
    if (applied.set_in(dec->es)) continue;
    applied.set_add(dec->es);
    int r = apply_csm_split(dec);
    log(LOG_SPLITTING, "[cpa] av %d es %d fun %s %d apply -> %d\n", dec->av->id, dec->es->id,
        dec->es->fun->sym->name ? dec->es->fun->sym->name : "", dec->es->fun->sym->id, r);
    if (r) analyze_again = 1;
  }
  return analyze_again;
}

// ifa/issues/075: element-CS container-method separation -- pyc's
// analog of shedskin's func_copy-per-dcpa (063's 2026-07-31 update,
// "Why shedskin can and pyc can't"). Demand signal: a container-method
// receiver whose live type is a union of >=2 CreationSets that are all
// the SAME container type (cs->sym->element non-null; the unaliased
// type equal across the group) but whose ELEMENT types diverge --
// exactly the shape that merges e.g. dijkstra2's `dists:
// list[dict[Vertex,float]]` and `paths: list[dict[Vertex,list[Vertex]]]`
// into one `float u list[Vertex]` element AVar, the genuine "no type"
// this issue is about. Placement (run_split_stages, stage 7): now
// mirrors PER_CS_RECEIVER (ifa/issues/045) -- only on quiescence of
// every stage above, not first/unconditional every pass. An earlier
// version ran first, before type confluence; investigation
// (2026-08-05, issue doc) found that starved stages 1-6 on programs
// whose divergence doesn't resolve in one split, which is worse than
// the imprecision this stage exists to fix.
//
// Piece 2 (decide-then-apply, see above): every qualifying receiver in
// the pass is DECIDED against the same unmutated snapshot before
// anything is APPLIED, with a per-ES dedup -- first decision touching
// a given ES wins, later ones this pass defer to next pass (mirrors
// split_ess_for_type's stage-1 discipline exactly). Piece 1 alone (one
// split per pass, decide-and-apply interleaved) reproduced the
// 2026-07-31 prototype's "not landable" regression almost exactly
// (suite -17/+18 fails, corpus +1/-32) -- this batched form is what
// the plan calls the actual fix for that failure class. Gated on
// csm_enabled() == 2 (PYC_CSM=2); flag off is a no-op, byte-identical
// to baseline.
[[nodiscard]] static int split_container_methods_per_element_cs() {
  if (csm_enabled() != 2) return 0;
  int n_ess = fa->ess.n;
  Vec<CSMSplitDecision *> decisions;
  for (int i = 0; i < n_ess; i++) {
    EntrySet *es = fa->ess[i];
    if (!es) continue;
    if (!es->fun || !es->fun->sym) continue;
    bool has_edges = false;
    for (AEdge *ee : es->edges) if (ee) { has_edges = true; break; }
    if (!has_edges) continue;
    for (MPosition *p : es->fun->positional_arg_positions) {
      AVar *av = es->args.get(p);
      if (!av || !av->out || !av->out->type || av->out->type->sorted.n < 2) continue;
      // Same container type across the whole receiver union.
      Sym *container_type = 0;
      bool all_same_container = true;
      for (CreationSet *cs : av->out->type->sorted) {
        if (!cs->sym || !cs->sym->element) { all_same_container = false; break; }
        Sym *t = cs->sym->type ? unalias_type(cs->sym->type) : cs->sym;
        if (!container_type) container_type = t;
        else if (container_type != t) { all_same_container = false; break; }
      }
      if (!all_same_container) continue;
      // Divergent element types across that same union -- the demand
      // signal. Piece 2 (decide-then-apply) scans EVERY qualifying
      // receiver in fa->ess each pass, not just the first (Piece 1's
      // shape) -- so, unlike a single decide-and-apply call where
      // "we're about to act on this receiver" justified forcing the
      // element AVar into existence, this scan must stay read-only
      // exactly like 075's dump-mode discipline requires: read a CS's
      // element only when cs->added_element_var is already set.
      // Calling get_element_avar unconditionally here perturbs
      // collect_type_confluence broadly across the WHOLE program on
      // EVERY pass (confirmed: dropped the C-backend suite from
      // 222/18 fails, Piece 1's number, to 9/241 -- essentially every
      // test's FA trajectory changed). A CS whose element AVar isn't
      // added yet just isn't decidable YET; it becomes so once
      // something else in the flow (e.g. the method's own `self[i]`)
      // creates it, same as any other pass.
      AType *first_elem = 0;
      bool divergent = false;
      bool all_added = true;
      for (CreationSet *cs : av->out->type->sorted) {
        if (!cs->added_element_var) { all_added = false; break; }
        AVar *elem = get_element_avar(cs);
        AType *et = elem && elem->out ? elem->out->type : 0;
        if (!first_elem) first_elem = et;
        else if (first_elem != et) divergent = true;
      }
      if (!all_added || !divergent) continue;
      CSMSplitDecision *dec = decide_csm_split(av);
      if (dec) decisions.add(dec);
    }
  }
  Vec<EntrySet *> applied;
  int analyze_again = 0;
  for (CSMSplitDecision *dec : decisions) {
    if (applied.set_in(dec->es)) {
      log(LOG_SPLITTING, "[csm] av %d es %d DEFERRED: es already split this pass (next pass re-decides)\n",
          dec->av->id, dec->es->id);
      continue;
    }
    applied.set_add(dec->es);
    int r = apply_csm_split(dec);
    log(LOG_SPLITTING, "[csm] av %d es %d fun %s %d apply -> %d\n", dec->av->id, dec->es->id,
        dec->es->fun->sym->name ? dec->es->fun->sym->name : "", dec->es->fun->sym->id, r);
    // ifa/issues/075: find_or_make_display_variant (used by
    // apply_csm_split above) durably reuses an existing sibling across
    // passes via an INDEXED lookup (EntrySet::display_variants, fa.h).
    // Combined with the quiescence-gated placement (run_split_stages,
    // stage 7 -- see that call site's comment) and make_entry_set's
    // filter-inheriting leftover-ES mint (fa.cc, near check_split),
    // termination no longer depends on this cap -- both capped
    // (one-apply-per-pass, below) and uncapped (batch every decided ES)
    // now terminate on the full corpus. They were NOT equally good,
    // though: measured head-to-head, capped is the more faithful
    // choice. Uncapped nets one MORE compiled program (53 vs. 52: it
    // recovers `kanoodle`, capped doesn't), but at the cost of THREE
    // programs that capped matches baseline on EXACTLY dropping to a
    // noisier COMPILED_C_WARN under uncapped -- and one of the three is
    // sha.py itself, the program this whole apply-batching investigation
    // was chasing (see the issue doc's full trail). Capped reproduces
    // baseline's clean sha.py output byte-for-byte; uncapped does not.
    // Kept the cap for that reason -- exact fidelity on the target case
    // over one extra loosely-"compiled" program elsewhere.
    if (r) {
      analyze_again = 1;
      break;
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
  if (!analyze_again) {
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    collect_type_confluences(confluences);
    cur_split_stage = (int)FAPassStage::TYPE_CONFLUENCE;
    analyze_again = split_ess_for_type(confluences, SPLIT_EDGES);
    fa->stage_time[(int)FAPassStage::TYPE_CONFLUENCE] += stage_timer.lap();
    // ifa/issues/055: a stage that returns "made progress" while the
    // contour counts do not move keeps the whole cascade alive and
    // starves every later stage. Report the claim next to the effect.
    if (getenv("PYC_DBG_STAGEDELTA"))
      fprintf(stderr, "STAGEDELTA p=%d TYPE_CONFL returned=%d confluences=%d d_ess=%d d_css=%d viol=%d\n",
              analysis_pass, analyze_again, confluences.n, fa->ess.n - ess0, fa->css.n - css0,
              fa->type_violations.set_count());
    log(LOG_SPLITTING, "split_ess_for_type %d\n", analyze_again);
    if (analyze_again) {
      record_fa_event(FAPassStage::TYPE_CONFLUENCE, analyze_again, ess0, css0, viol0);
      ++fa->stage_progress_count[(int)FAPassStage::TYPE_CONFLUENCE];
    }
  }
  // 1b) ifa/issues/074 (PYC_CPA): cartesian-product naming. Placed right
  // after stage 1 and gated the same way, because this is exactly the
  // situation it exists for: TYPE_CONFLUENCE found no confluence, yet a
  // formal still holds a union -- which is the union-as-a-contour-name
  // fixed point (every edge carries the union, so etype == stype and
  // there is nothing for a type test to see). Fanning here also puts it
  // where MARK_TYPE used to act, which is the point: it replaces the
  // depth proxy with the name.
  if (!analyze_again) {
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::CARTESIAN_PRODUCT;
    analyze_again = split_ess_cartesian_product();
    fa->stage_time[(int)FAPassStage::CARTESIAN_PRODUCT] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::CARTESIAN_PRODUCT, analyze_again, ess0, css0, viol0);
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d CARTESIAN_PRODUCT returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
      ++fa->stage_progress_count[(int)FAPassStage::CARTESIAN_PRODUCT];
    }
    log(LOG_SPLITTING, "split_ess_cartesian_product %d\n", analyze_again);
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
    analyze_again = nomark_enabled() >= 1 ? 0 : split_ess_for_mark_type(confluences);
    fa->stage_time[(int)FAPassStage::MARK_TYPE] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::MARK_TYPE, analyze_again, ess0, css0, viol0);
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d MARK_TYPE    returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
      ++fa->stage_progress_count[(int)FAPassStage::MARK_TYPE];
    }
  }
  log(LOG_SPLITTING, "split_ess_for_mark_type %d\n", analyze_again);
  // 3) split based on setters of type
  // ifa/issues/055: PYC_SETTERGATE=1 lifts the quiescence gate on the
  // SETTER stage, the same way PYC_RECVFAN=2 lifts it on
  // PER_CS_RECEIVER. Measured on the plcfrs 9-line repro: the field
  // whose union needs splitting IS collected as a type confluence (30
  // times), but split_ess_for_type finds work on every one of those
  // passes, so `!analyze_again` is false and compute_setters is never
  // called on it -- zero setters exist in the whole program, so the
  // demand-driven back-flow never starts.
  static int settergate = -1;
  if (settergate < 0) settergate = getenv("PYC_SETTERGATE") ? atoi(getenv("PYC_SETTERGATE")) : 0;
  // ifa/issues/055 probe: PYC_NOSETTER=1 skips the SETTER stage entirely.
  static int nosetter = -1;
  if (nosetter < 0) nosetter = getenv("PYC_NOSETTER") ? atoi(getenv("PYC_NOSETTER")) : 0;
  if (nosetter) { /* skip */ } else if (!analyze_again || settergate) {
    Accum<AVar *> avs;
    // ifa/issues/111 M3 option 2: under selective invalidation, class
    // EVERY CS-contoured AVar, not just this pass's confluences.
    //
    // setter_class comes only from here, so with the full reset the
    // invariant "every member of a Setters set was classed this pass"
    // holds because every set is also BUILT this pass. Selective
    // invalidation preserves sets across passes, so members can survive
    // that this pass's confluence list never reaches, and
    // same_eq_classes asserts on them. Widening the coverage restores
    // the invariant by brute force. Affordable in principle: M1
    // measured the whole `extend` phase at ~0.4% of FA time.
    if (ifa_selective) {
      Vec<AVar *> all_cs_avars;
      foreach_avar([&](AVar *a) {
        if (a && !a->contour_is_entry_set && a->contour != GLOBAL_CONTOUR) all_cs_avars.set_add(a);
      });
      for (AVar *av : all_cs_avars) if (av) (void)compute_setters(av, avs, AKIND_TYPE);
    } else {
      // ifa/issues/124: compute_setters only ever visits `confluences`.
      // If a container's ELEMENT AVar is not collected as a type
      // confluence, its writers never get a setter_class, no setter is
      // recorded on the creation point, and the SETTER stage has
      // nothing to split -- which is what happens to the two lists that
      // share a `list::append` contour. Report the split of the
      // confluence list so that is visible rather than inferred.
      if (getenv("IFA_DBG_SETTERCONF")) {
        int n_cs = 0, n_elem = 0;
        for (AVar *av : confluences)
          if (av && !av->contour_is_entry_set && av->contour != GLOBAL_CONTOUR) {
            n_cs++;
            CreationSet *c = (CreationSet *)av->contour;
            if (c && c->sym && c->sym->element && c->sym->element->var == av->var) n_elem++;
          }
        fprintf(stderr, "SETTERCONF p=%d confluences=%d cs_contoured=%d container_elements=%d\n", analysis_pass,
                confluences.n, n_cs, n_elem);
      }
      for (AVar *av : confluences) (void)compute_setters(av, avs, AKIND_TYPE);
    }
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::SETTER;
    int viol_before_setter = fa->type_violations.set_count();
    if (split_for_setters(avs, analyze_again)) analyze_again = 1;
    if (getenv("PYC_DBG_STAGEDELTA"))
      fprintf(stderr, "STAGEDELTA p=%d SETTER      returned=%d d_ess=%d d_css=%d viol=%d (was %d)\n",
              analysis_pass, analyze_again, fa->ess.n - ess0, fa->css.n - css0,
              fa->type_violations.set_count(), viol_before_setter);
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
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d SETTER_OF_SETTER returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
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
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d MARK_SETTER  returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
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
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d MARK_SETTER_OF_SETTER returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
        ++fa->stage_progress_count[(int)FAPassStage::MARK_SETTER_OF_SETTER];
      }
    }
    log(LOG_SPLITTING, "split_for_setters_of_setters with marks %d\n", analyze_again);
    clear_marks(acc);
  }
  // ifa/issues/109: PYC_SIZEOF_VIOL >= 2 also lifts this stage's
  // quiescence gate. Recording the sizeof_element violation is useless
  // while VIOLATION is STARVED by the first-stage-wins cascade -- measured
  // on sunfish, whose STAGES line is `TYPE_CONFL SETTER SETTER_OF_SETTER`
  // and never reaches stage 5, so the violation is recorded and nothing
  // ever splits on it.
  if (!analyze_again || sizeof_viol_enabled() >= 2) {
    // 5) split AEdges(s) and EntrySet(s) for violations based on type using
    // dynamic dispatch
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::VIOLATION;
    analyze_again = split_for_violations(fa->type_violations) || analyze_again;
    fa->stage_time[(int)FAPassStage::VIOLATION] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::VIOLATION, analyze_again, ess0, css0, viol0);
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d VIOLATION    returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
      ++fa->stage_progress_count[(int)FAPassStage::VIOLATION];
    }
  }
  log(LOG_SPLITTING, "split_for_violations %d\n", analyze_again);
  // 6) precision: per-receiver-CS method contours for
  // clone_methods_per_cs classes (ifa/issues/045). Only on full
  // quiescence of stages 1-5 so it cannot perturb their
  // trajectories within a pass.
  // ifa/issues/104: PYC_RECVFAN=2 also lifts the quiescence gate. The
  // stage is otherwise STARVED on the programs this is aimed at --
  // TYPE_CONFLUENCE fires every pass on plcfrs/rdb/sudoku5, so
  // `!analyze_again` is never true and the receiver fan never runs at
  // all. That is why RECVFAN=1 measured byte-identical to baseline.
  if (!analyze_again || recvfan_enabled() >= 2) {
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::PER_CS_RECEIVER;
    // 2 = lifted gate, mixed-container receivers only (measured INERT --
    // the {list,tuple} fan never fires). 3 = lifted gate, fan everything
    // the stage already handles, which is where the win actually is.
    per_cs_forced = analyze_again ? (recvfan_enabled() >= 3 ? 2 : 1) : 0;
    analyze_again = split_for_per_cs_method_receivers() || analyze_again;
    per_cs_forced = 0;
    fa->stage_time[(int)FAPassStage::PER_CS_RECEIVER] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::PER_CS_RECEIVER, analyze_again, ess0, css0, viol0);
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d PER_CS_RECEIVER returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
      ++fa->stage_progress_count[(int)FAPassStage::PER_CS_RECEIVER];
    }
  }
  log(LOG_SPLITTING, "split_for_per_cs_method_receivers %d\n", analyze_again);
  // 7) ifa/issues/075 (PYC_CSM=2): element-CS container-method
  // separation. Placement changed 2026-08-05 after investigation
  // (see the issue doc): originally ran FIRST, unconditionally, every
  // pass -- which on a program whose divergence doesn't resolve in
  // one CS-partition split (dijkstra2's genuinely nested containers)
  // meant it found work almost every pass and, via the SAME
  // if-(!analyze_again) discipline every stage here already follows,
  // starved stages 1-6 almost completely (traced: stage 1 reached 4
  // times in an 8s window vs. dozens of this stage's own decisions).
  // The stages it starved are exactly the ones with actual recursion-
  // separability / self-product-eviction safeguards (see
  // decide_entry_set_split's comments); this stage has no analog of
  // either. Now mirrors PER_CS_RECEIVER immediately above: only on
  // full quiescence of every stage above, so it can no longer preempt
  // work the rest of the pipeline could otherwise resolve on its own,
  // and any contours it DOES create get a full pass through 1-6
  // (recursion-aware, self-product-aware) before this stage can act
  // on them again. Flag off (default): returns 0 immediately, so this
  // block never fires -- byte-identical to baseline regardless of
  // placement.
  if (!analyze_again) {
    ess0 = fa->ess.n, css0 = fa->css.n, viol0 = fa->type_violations.set_count();
    cur_split_stage = (int)FAPassStage::CSM_ELEMENT_CS;
    analyze_again = split_container_methods_per_element_cs();
    fa->stage_time[(int)FAPassStage::CSM_ELEMENT_CS] += stage_timer.lap();
    if (analyze_again) {
      record_fa_event(FAPassStage::CSM_ELEMENT_CS, analyze_again, ess0, css0, viol0);
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "STAGEDELTA p=%d CSM_ELEMENT_CS returned=%d d_ess=%d d_css=%d viol=%d\\n", analysis_pass,
                analyze_again, fa->ess.n - ess0, fa->css.n - css0, fa->type_violations.set_count());
      ++fa->stage_progress_count[(int)FAPassStage::CSM_ELEMENT_CS];
    }
  }
  log(LOG_SPLITTING, "split_container_methods_per_element_cs %d\n", analyze_again);
  // ifa/issues/074: back to "no stage running". Without this, every
  // contour/CreationSet the NEXT pass's flow creates was attributed to
  // whichever stage happened to run last -- which is what made the
  // per-stage `reuse` column read in the thousands for stages that
  // re-bind nothing (CSM_ELEMENT_CS's reuse=2280 on rubik was flow).
  cur_split_stage = -1;
  return analyze_again;
}

// ifa/issues/111 M1: how much of the program does a pass's splitting
// actually invalidate?
//
// Selective invalidation is only worth building if the answer is
// "a small fraction". The soundness rule it would rest on (see the
// issue) is: a split ES's AVars must reset to bottom, because a split
// REFINES and the intra-pass fixed point only ever GROWS -- so must
// everything transitively reachable via `forward`, since a value
// derived from the old, wider one would never shrink. Everything else
// has unchanged inputs and could be preserved.
//
// So the closure measured here IS the set the fix would have to clear.
// Seed: contours this pass split. `clear_splits()` zeroes es->split /
// cs->split at the top of run_split_stages, and splitting sets them,
// so immediately after it returns those marks are exactly this pass's
// changes -- no new bookkeeping.
//
// Probe only: walks the graph, prints, changes nothing. Enable with
// IFA_DBG_CLOSURE=1.
static void probe_invalidation_closure() {
  const bool dbg = getenv("IFA_DBG_CLOSURE") != nullptr;
  if (!dbg && !ifa_selective) return;

  // Every AVar that exists, and the seed set in one walk.
  Vec<AVar *> all, seed;
  Vec<void *> split_contours;
  for (EntrySet *es : fa->ess) if (es->split) split_contours.set_add((void *)es);
  for (CreationSet *cs : fa->css) if (cs->split) split_contours.set_add((void *)cs);
  int n_marked = split_contours.count();
  for (EntrySet *es : fa_pass_retargeted) if (es) split_contours.set_add((void *)es);
  int n_retarget = split_contours.count() - n_marked;

  auto collect = [&](Var *v) {
    for (int i = 0; i < v->avars.n; i++) {
      if (!v->avars[i].key) continue;
      AVar *av = v->avars[i].value;
      all.add(av);
      if (split_contours.set_in(av->contour)) seed.add(av);
    }
  };
  for (Sym *sy : fa->pdb->if1->allsyms) if (sy->var) collect(sy->var);
  for (Fun *f : fa->pdb->funs) for (Var *v : f->fa_all_Vars) collect(v);
  // A split CreationSet's own field AVars live on the CS, not on a Var.
  for (CreationSet *cs : fa->css) if (cs->split)
    for (AVar *av : cs->vars) if (av) seed.add(av);

  // Forward closure: BFS over av->forward, the same graph
  // flow_var_to_var maintains for every value flow (including edge
  // args/rets). Intact here because clear_avar has not run yet.
  Vec<AVar *> reached, work;
  for (AVar *av : seed) if (reached.set_add(av)) work.add(av);
  int head = 0;
  while (head < work.n) {
    AVar *av = work.v[head++];
    for (AVar *nx : av->forward) if (nx && reached.set_add(nx)) work.add(nx);
  }

  int n_all = all.n, n_seed = seed.n, n_closure = reached.count();
  if (dbg)
    fprintf(stderr, "CLOSURE pass=%d marked=%d retargeted=%d seed_avars=%d closure=%d all=%d pct=%d\n",
            analysis_pass, n_marked, n_retarget, n_seed, n_closure, n_all,
            n_all ? (int)((double)n_closure * 100.0 / (double)n_all) : 0);

  // Hand the closure to the next pass. `reached` is a set-Vec, so copy
  // the dense members out.
  fa_invalidate_avars.clear();
  for (AVar *av : reached) if (av) fa_invalidate_avars.add(av);
  fa_selective_armed = true;
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
  probe_invalidation_closure();  // ifa/issues/111 M1 (IFA_DBG_CLOSURE)
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
    // Issue 074 Stage 1: did this pass add any new contours? Productive
    // precision splitting grows ess/css; a re-deriving pass that grows
    // neither is pure issue-033 non-idempotent churn.
    bool grew = fa->ess.n != fa->prev_ess_n || fa->css.n != fa->prev_css_n;
    fa->prev_ess_n = fa->ess.n;
    fa->prev_css_n = fa->css.n;
    if (v > 0) {
      fa->zero_viol_stall_passes = 0;
      if (v < fa->best_violations) {
        fa->best_violations = v;
        fa->stall_passes = 0;
        fa->nonimprove_passes = 0;
      } else {
        bool rederived = fa->rederive_churn > 0;
        // ifa/issues/055 experiment B (PYC_STALL_REANALYZE): a pass the
        // FRONTEND asked for is expected to look worse -- field
        // promotion exposes fields, which exposes type flow, and the
        // violation count rises before it falls (measured 44 -> 325 ->
        // 52 on the plcfrs repro). Counting that as non-improvement
        // stops the analysis while the repair is still progressing.
        static int stall_reanalyze = -1;
        if (stall_reanalyze < 0)
          stall_reanalyze = getenv("PYC_STALL_REANALYZE") ? atoi(getenv("PYC_STALL_REANALYZE")) : 0;
        if (!(stall_reanalyze && fa->last_pass_reanalyze)) {
          if (rederived) ++fa->stall_passes;
          ++fa->nonimprove_passes;
        }
        if (fa->stall_passes >= fa->stall_limit || fa->nonimprove_passes >= fa->nonimprove_limit) {
          fa->pass_limit_hit = true;
          log(LOG_SPLITTING,
              "STALL LIMIT reached at pass %d, %d violations (best %d): %d re-deriving (limit %d), "
              "%d non-improving (limit %d); stopping\n",
              analysis_pass, v, fa->best_violations, fa->stall_passes, fa->stall_limit, fa->nonimprove_passes,
              fa->nonimprove_limit);
          analyze_again = 0;
        }
      }
    } else {
      // v == 0: types have converged. A pass that RE-DERIVED a split
      // (dup>0) yet added no new contours is non-idempotent churn, not
      // productive precision splitting -- the v>0 guard above skips it
      // (`if (v > 0)`), so pygmy et al. run to the hard cap. Bound it
      // with the same stall_limit; a genuinely productive zero-violation
      // pass grows ess/css (or splits dup-free) and resets the counter.
      bool rederived = fa->rederive_churn > 0;
      if (rederived && !grew) {
        if (++fa->zero_viol_stall_passes >= fa->stall_limit) {
          fa->pass_limit_hit = true;
          analyze_again = 0;
          log(LOG_SPLITTING, "ZERO-VIOL STALL at pass %d: %d re-deriving no-growth passes (limit %d); stopping\n",
              analysis_pass, fa->zero_viol_stall_passes, fa->stall_limit);
        }
      } else
        fa->zero_viol_stall_passes = 0;
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
  // NOTE (ifa/issues/098): the per-pass reset used to live here, gated
  // on `analyze_again`. That made it conditional on the SPLITTER having
  // work to do, so a pass the frontend requested through
  // IFACallbacks::reanalyze() (pyc's field promotion) ran on top of the
  // previous pass's fully populated flow state. analyze_to_convergence
  // now clears before every pass instead, which is also the only place
  // that can do it without wiping the converged results the caller
  // needs when this returns 0.
  pass_timer.stop();
  ++analysis_pass;
  // ifa/issues/112: COARSE per-pass trace of FA's computed state, for
  // locating the FIRST divergence between two runs.
  //
  // An id-based hash is useless as an aggregate verdict -- ids are
  // creation-order counters, so everything downstream of a divergence
  // looks different whether or not it is. But it is exactly right for a
  // chronological trace: UP TO the first divergence, every counter has
  // handed out the same numbers to the same objects, so the first
  // differing line IS the divergence rather than its wake. Diff two
  // runs' traces and take the first differing record.
  //
  // Reads only -- v->avars.get-style traversal via form_AVarMapElem and
  // no make_AVar, because minting an AVar here would shift the very
  // counters under test (cf. ifa/041, where the -v dump mutated the
  // analysis it measured).
  static int dbg_fatrace = -1;
  if (dbg_fatrace < 0) dbg_fatrace = getenv("IFA_DBG_FATRACE") ? 1 : 0;
  if (dbg_fatrace) {
    char lbl[64];
    snprintf(lbl, sizeof(lbl), "pass-%d", analysis_pass);
    dbg_trace_fa_state(lbl);
  }
  if (0) {
    Vec<AVar *> avs;
    for (Fun *f : fa->funs) if (f)
      for (Var *v : f->fa_all_Vars) if (v)
        form_AVarMapElem(x, v->avars) if (x->value) avs.add(x->value);
    for (CreationSet *cs : fa->css) if (cs)
      for (AVar *iv : cs->vars) if (iv) avs.add(iv);
    if (avs.n > 1) qsort_by_id(avs);
    unsigned long h = 1469598103934665603UL;
#define FAMIX(x) (h = (h ^ (unsigned long)(x)) * 1099511628211UL)
    for (AVar *av : avs) {
      FAMIX(av->id);
      if (av->out) {
        Vec<int> ids;
        for (CreationSet *c : *av->out) if (c) ids.add(c->id);
        if (ids.n > 1) qsort(ids.v, ids.n, sizeof(ids[0]), [](const void *a, const void *b) {
          int x = *(const int *)a, y = *(const int *)b; return (x > y) - (x < y);
        });
        for (int i : ids) FAMIX(i);
      }
      FAMIX(0x9e3779b9UL);
    }
#undef FAMIX
    fprintf(stderr, "FATRACE pass=%d navars=%d ess=%d css=%d h=%lx\n", analysis_pass, avs.n, fa->ess.n, fa->css.n, h);
  }
  {
    extern void dbg_trace_avar(cchar *where);
    char buf[64];
    snprintf(buf, sizeof(buf), "fa-pass-%d", analysis_pass);
    dbg_trace_avar(buf);
  }
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
  if (getenv("PYC_DBG_WORK"))  // ifa/111 probe
    fprintf(stderr, "WORK pass=%d edges=%ld sends=%ld es_constraints=%ld pass_time=%f\n", analysis_pass, work_edges,
            work_sends, work_escons, pass_timer.time);
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

// ifa/issues/112: this used to walk `v->avars` by raw slot index. That
// map is keyed by EntrySet POINTERS, so the walk followed heap layout --
// and because the body `return`s after the FIRST AVar carrying an
// unused closure, WHICH AVar got cleaned moved between runs of the same
// compile. Measured on msp_ss: one AVar came out `{closure}` in some
// runs and `{void_type}` in others, and that fed its Var's type, the
// inlining decisions that compare types by pointer, and finally the
// emitted C.
//
// Fixed by walking in AVar-id order. NOTE the `return` is still
// suspicious on its own terms -- it means only ONE AVar per Var is ever
// cleaned, where `break` (clean every AVar) looks like the intent. That
// is a behaviour question, deliberately not changed here; this commit
// only makes the existing choice reproducible.
static void remove_unused_closures(Var *v) {
  Vec<AVar *> avs;
  for (int i = 0; i < v->avars.n; i++)
    if (v->avars[i].key && v->avars[i].value) avs.add(v->avars[i].value);
  if (avs.n > 1) qsort_by_id(avs);
  for (AVar *av : avs)
    {
      for (CreationSet *cs : av->out->sorted) if (cs->sym == sym_closure && !cs->closure_used) {
        Vec<CreationSet *> css;
        for (CreationSet *cs : av->out->sorted) {
          if (cs->sym == sym_closure && !cs->closure_used) continue;
          css.add(cs);
        }
        av->out = make_AType(css);
        break;
      }
    }
}

static void remove_unused_closures() { foreach_var(remove_unused_closures); }

// ifa/issues/098's invariant check, gated on IFA_DBG_EDGEARGS (same
// zero-cost-when-off shape as the PYC_DBG_* probes elsewhere in this
// file). If a call is reachable then some execution schedule reaches
// it, and in that schedule every actual argument holds a value -- so a
// call edge whose arguments this pass RECORDED must, at quiescence,
// have a non-bottom `out` at every one of them. Non-empty `e->args` is
// exactly the "recorded this pass" test: clear_results empties it
// before every pass and only record_args_rets (reached through a
// successful dispatch) refills it. A survivor is therefore an edge
// holding some OTHER pass's actuals, which is precisely the failure 098
// documents -- its worst-pass reading on the old reset scheme was 1207
// (chess), 599 (rdb), 382 (sudoku5), 252 (msp_ss), 235 (mastermind2);
// it is 0 on every pass of every shedskin example now. Kept because the fix (see clear_results /
// analyze_to_convergence) is easy to silently undo: anything that
// re-narrows the reset's domain, or re-introduces a pass that skips it,
// shows up here immediately.
//   IFA_DBG_EDGEARGS=1 ./pyc prog.py 2>&1 | grep EDGEARG
static void audit_edge_arg_values() {
  if (!getenv("IFA_DBG_EDGEARGS")) return;
  int bad = 0;
  for (AEdge *e : fa->all_aedges) {
    if (!e->to || !e->from || !e->match) continue;
    Vec<MPosition *> positions;
    positional_arg_positions_in_order(e, positions);
    for (MPosition *p : positions) {
      AVar *actual = e->args.get(p);
      if (!actual || actual->out != fa->type_world.bottom_type) continue;
      bad++;
      fprintf(stderr, "EDGEARG pass=%d e=%d %s -> %s pos=%d actual=av%d\n", analysis_pass, e->id,
              e->from->fun && e->from->fun->sym && e->from->fun->sym->name ? e->from->fun->sym->name : "?",
              e->to->fun && e->to->fun->sym && e->to->fun->sym->name ? e->to->fun->sym->name : "?",
              (int)Position2int(p->last()), actual->id);
    }
  }
  fprintf(stderr, "EDGEARG SUMMARY pass=%d stale_bottom_args=%d edges=%d\n", analysis_pass, bad,
          fa->all_aedges.n);
}

// ifa/issues/074: freeze each reached contour's converged formal types
// into its durable type key, for the NEXT pass's compatibility matching.
// Taken here, after the flow fixpoint, so the value is whole-pass
// invariant rather than a mid-pass snapshot.
// ifa/issues/074: is a contour's NAME stable from pass to pass?
//
// A splitter that keeps firing on the same contour is doing one of two
// very different things. If the contour's converged formal types GREW,
// the analysis is discovering polymorphism and will stop once the type
// lattice tops out -- slow, but terminating. If they CHANGED
// non-monotonically -- the contour lost a CreationSet it held last pass
// -- the naming itself is oscillating, and no amount of extra passes
// converges it. IFA_DBG_KEYDRIFT=1 separates the two.
static long kd_stable = 0, kd_grew = 0, kd_shrank = 0, kd_new = 0, kd_flip = 0;
static Vec<Fun *> kd_flip_funs;

// ifa/issues/101: stamp each container CreationSet with its converged
// element type, then roll those up per creation site. Runs in
// complete_pass, AFTER the flow fixpoint, so the value is the
// whole-pass-invariant element type rather than a mid-pass accumulation
// -- the same discipline EntrySet::type_key needs (066).
//
// READ-ONLY with respect to element AVars: get_element_avar() would
// CREATE one and set added_element_var (which gates numeric coercion),
// so only CSs that already have one are considered.
// ifa/issues/105: find the ORIGIN of type degeneration. Report AVars
// whose type spans more than N distinct SYMS (not CreationSets) --
// a variable holding bool+int+str+float+list+dict+user-classes at once.
// Reported with the defining function and source line so the smallest,
// earliest one can be read in the source. Probe-only.
static void report_degenerate_avars() {
  const char *v = getenv("IFA_DBG_DEGEN");
  if (!v) return;
  int thresh = atoi(v) > 0 ? atoi(v) : 6;
  std::map<std::string, int> by_site;
  for (EntrySet *es : fa->entry_set_done) if (es && es->fun) {
    for (Var *var : es->fun->fa_all_Vars) {
      AVar *av = make_AVar(var, es);
      if (!av->out || !av->out->type) continue;
      std::set<Sym *> syms;
      for (CreationSet *c : av->out->type->sorted) if (c->sym) syms.insert(c->sym);
      if ((int)syms.size() < thresh) continue;
      char buf[512];
      snprintf(buf, sizeof buf, "fun=%s var=%s line=%d nsyms=%d",
               es->fun->sym->name ? es->fun->sym->name : "?",
               var->sym->name ? var->sym->name : "_",
               var->sym->ast ? var->sym->ast->line() : -1, (int)syms.size());
      by_site[buf]++;
    }
  }
  for (auto &kv : by_site) fprintf(stderr, "[degen] %s x%d\n", kv.first.c_str(), kv.second);
}

// ifa/issues/074: the structural shape of a type, spelled `list<list<int64>>`.
//
// Reads the DURABLE elem_key (captured at the end of the previous pass),
// never the live element AVar: every container starts out empty and
// acquires its elements later, so a live read would call two containers
// equal merely because neither has filled in yet -- the same trap the
// mode-1 key comments already record. A container holding itself closes
// with `@`, and the depth is capped: an unbounded structural walk is the
// very non-termination this is meant to fix.
static const int kShapeDepth = 6;

static void atype_shape(AType *t, std::string &out, int depth, Vec<CreationSet *> &seen) {
  if (!t || !t->sorted.n) {
    out += "%";  // NOT '_': Python dunder names are full of underscores, and
    return;      // an unfilled-element test on '_' rejected every one of them
  }
  int n = 0;
  for (CreationSet *cs : t->sorted) {
    if (!cs || !cs->sym) continue;
    if (n++) out += "|";
    out += cs->sym->name ? cs->sym->name : "?";
    if (!cs->sym->element) continue;
    bool cycle = false;
    for (CreationSet *x : seen)
      if (x == cs) { cycle = true; break; }
    if (cycle) { out += "<@>"; continue; }
    if (depth >= kShapeDepth) { out += "<...>"; continue; }
    seen.add(cs);
    out += "<";
    atype_shape(cs->elem_key, out, depth + 1, seen);
    out += ">";
    seen.pop();
  }
}

// The `self` POSITION of an EntrySet -- positional slot 2. Slot 1 is the
// callee itself.
//
// Selected by POSITION NUMBER, not by index into the sorted vector:
// compar_mposition_path does not order these numerically, and measuring
// (IFA_DBG_CSELEM=2) showed sorted index 0 carrying pos=2 and index 1
// carrying pos=1 -- so keying on the index silently read the callee's
// type as the receiver's on every method in the program.
static MPosition *es_self_position(EntrySet *es) {
  if (!es) return nullptr;
  form_MPositionAVar(x, es->args) {
    MPosition *p = x->key;
    if (p && p->pos.n == 1 && p->is_positional() && Position2int(p->last()) == 2) return p;
  }
  return nullptr;
}

// (allocation site, receiver shape) -> the CreationSet that claimed it.
// Deliberately NOT cleared per pass: shapes converge, so entries only
// stabilize, and merging must be monotone or the canonicalization itself
// becomes a source of churn.
static std::map<std::string, CreationSet *> cselem_shape_canon;

// creation_point is hot, and atype_shape walks the whole type structure
// building a std::string every call. On a program with large unions
// (adatron) that alone hung the analysis INSIDE a single pass -- no pass
// summary in 150s -- with no contour growth at all. Two bounds: memoize
// on the AType (they are hash-consed, and cleared per pass because
// elem_key moves), and refuse outright to shape a wide union, where
// canonicalization is least meaningful anyway.
static const int kShapeMaxMembers = 4;
static std::map<AType *, std::string> cselem_shape_memo;
static int cselem_shape_memo_pass = -1;

static bool atype_shape_cached(AType *t, std::string &out) {
  if (!t) return false;
  if (t->sorted.n > kShapeMaxMembers) return false;
  if (cselem_shape_memo_pass != analysis_pass) {
    cselem_shape_memo.clear();
    cselem_shape_memo_pass = analysis_pass;
  }
  auto it = cselem_shape_memo.find(t);
  if (it != cselem_shape_memo.end()) {
    out = it->second;
    return true;
  }
  Vec<CreationSet *> seen;
  std::string shape;
  atype_shape(t, shape, 0, seen);
  cselem_shape_memo[t] = shape;
  out = shape;
  return true;
}

static bool cselem_shape_key(AVar *v, Sym *s, std::string &out) {
  if (!v || !v->var || !v->contour_is_entry_set) return false;
  EntrySet *es = (EntrySet *)v->contour;
  // DURABLE receiver type (EntrySet::type_key, frozen after the previous
  // pass's flow fixpoint), never the live `recv->out`. Reading the live
  // one merged two levels that were both still empty and therefore both
  // shaped `list<_>` -- and because the canon map is monotone, that merge
  // was permanent. It cost the whole bounded copy-chain family.
  static int dbg = getenv("IFA_DBG_CSELEM") ? atoi(getenv("IFA_DBG_CSELEM")) : 0;
  if (dbg > 1) {
    Vec<MPosition *> ps;
    form_MPositionAVar(x, es->args) if (x->key->is_positional()) ps.add(x->key);
    if (ps.n > 1) qsort(ps.v, ps.n, sizeof(ps[0]), compar_mposition_path);
    for (int i = 0; i < ps.n; i++) {
      AVar *a = es->args.get(ps.v[i]);
      Vec<CreationSet *> sn;
      std::string sh;
      atype_shape(a && a->out ? a->out->type : nullptr, sh, 0, sn);
      fprintf(stderr, "[csshape-pos] p=%d es=%d fun=%s i=%d pos=%d shape=%s\n", analysis_pass, es->id,
              es->fun && es->fun->sym && es->fun->sym->name ? es->fun->sym->name : "?", i,
              (int)Position2int(ps.v[i]->last()), sh.c_str());
    }
  }
  MPosition *self = es_self_position(es);
  if (!self) return false;
  // Prefer the DURABLE receiver type (EntrySet::type_key, frozen after the
  // previous pass's flow fixpoint). But the decision has to be made the
  // moment the contour is FIRST asked for its container -- after that the
  // `cs_map` memo answers and this code never runs again -- and a contour
  // is asked on the very pass it is created, when nothing durable exists
  // for it yet (`type_key_pass == -1` for every EntrySet that reached here
  // on the recursive repro). So fall back to the live receiver type, with
  // the `_` guard below carrying the weight the durable key otherwise
  // would. That is ifa/066's shape -- the decision is keyed per pass
  // rather than per creation site -- and this is how to live with it, not
  // a fix for it.
  AType *rt = es->type_key_pass >= 0 ? es->type_key.get(self) : nullptr;
  if (!rt) {
    AVar *recv = es->args.get(self);
    rt = recv && recv->out ? recv->out->type : nullptr;
  }
  if (!rt) return false;
  std::string shape;
  if (!atype_shape_cached(rt, shape)) return false;
  // Any `_` in the shape is an element type that has not arrived yet, and
  // two shapes that differ only in what has not arrived are not known to
  // be equal. Decline rather than guess; the next pass will have it.
  if (shape.find('%') != std::string::npos) {
    if (dbg) fprintf(stderr, "[csshape-no] p=%d es=%d shape=%s (unfilled)\n", analysis_pass, es->id, shape.c_str());
    return false;
  }
  char pre[96];
  snprintf(pre, sizeof pre, "v%d|%s|", v->var->id, s->name ? s->name : "?");
  out = std::string(pre) + shape;
  return true;
}

static CreationSet *cselem_shape_reuse(AVar *v, Sym *s) {
  std::string key;
  if (!cselem_shape_key(v, s, key)) return nullptr;
  auto it = cselem_shape_canon.find(key);
  if (it == cselem_shape_canon.end() || !it->second || it->second->sym != s) return nullptr;
  if (getenv("IFA_DBG_CSELEM"))
    fprintf(stderr, "[csshape] p=%d %s -> reuse cs=%d\n", analysis_pass, key.c_str(), it->second->id);
  return it->second;
}

static void cselem_shape_claim(const std::string &key, CreationSet *cs) {
  if (cselem_shape_canon.find(key) == cselem_shape_canon.end()) {
    cselem_shape_canon[key] = cs;
    if (getenv("IFA_DBG_CSELEM"))
      fprintf(stderr, "[csshape] p=%d %s <- mint cs=%d\n", analysis_pass, key.c_str(), cs->id);
  }
}

static void capture_elem_keys() {
  if (!cselem_enabled()) return;
  int n_keyed = 0, n_sites = 0;
  fa->var_elem_key.clear();
  fa->var_elem_ambig.clear();
  for (CreationSet *cs : fa->css) if (cs && cs->sym && cs->sym->element) {
    if (!cs->added_element_var) continue;
    AVar *e = unique_AVar(cs->sym->element->var, cs);
    if (!e) continue;
    cs->elem_key = e->out->type;
    cs->elem_key_pass = analysis_pass;
    ++n_keyed;
    if (!cs->creation_var) continue;
    ++n_sites;
    AType *prev = fa->var_elem_key.get(cs->creation_var);
    if (!prev)
      fa->var_elem_key.put(cs->creation_var, cs->elem_key);
    else if (prev != cs->elem_key)
      fa->var_elem_ambig.put(cs->creation_var, 1);  // site is genuinely polymorphic
  }
  if (getenv("IFA_DBG_CSELEM"))
    fprintf(stderr, "[cselem-cap] p=%d keyed=%d with_var=%d\n", analysis_pass, n_keyed, n_sites);
}

static void capture_type_keys() {
  bool drift = getenv("IFA_DBG_KEYDRIFT") != nullptr;
  if (!typekey_enabled() && !drift && selfprod_enabled() < 3) return;
  for (EntrySet *es : fa->entry_set_done) if (es && es->fun) {
    int grew = 0, shrank = 0, fresh = es->type_key_pass < 0;
    unsigned int h = 0;
    int i = 0;
    form_MPositionAVar(x, es->args) {
      if (!x->key->is_positional() || !x->value) continue;
      AType *now = x->value->out->type;
      h += (unsigned int)(uintptr_t)now * open_hash_primes[i++ % 256];
      if (drift) {
        if (!fresh) {
          AType *was = es->type_key.get(x->key);
          if (was && was != now) {
            // lost something it held last pass == non-monotone rename
            if (type_diff(was, now) != fa->type_world.bottom_type) shrank = 1;
            else grew = 1;
          }
        }
      }
      es->type_key.put(x->key, now);
    }
    es->type_key_pass = analysis_pass;
    // key(N) == key(N-2) != key(N-1): a period-2 flip-flop, which no
    // number of further passes resolves. Distinct from a key that is
    // still moving monotonically toward a fixed point. key_hash[0] is
    // this pass's, [1] the previous pass's -- PYC_SELFPROD=5 reads them
    // as a per-contour "has stopped moving" test, so they are maintained
    // whenever the key is captured at all, not only under the probe.
    int flip = !fresh && h != es->key_hash[0] && h == es->key_hash[1];
    es->key_hash[2] = es->key_hash[1];
    es->key_hash[1] = es->key_hash[0];
    es->key_hash[0] = h;
    if (!drift) continue;
    if (fresh) ++kd_new;
    else if (flip) {
      ++kd_flip;
      kd_flip_funs.set_add(es->fun);
    } else if (shrank)
      ++kd_shrank;
    else if (grew)
      ++kd_grew;
    else
      ++kd_stable;
  }
}

static void report_incompat() {
  if (!getenv("IFA_DBG_INCOMPAT")) return;
  fprintf(stderr,
          "INCOMPAT p=%d arg=%ld ret=%ld retn=%ld | stage1 seen=%ld skip(rval=%ld lval=%ld cs=%ld) dec=%ld "
          "defer=%ld split(formal=%ld return=%ld)\n",
          analysis_pass, ic_arg, ic_ret, ic_retn, tc_seen, tc_skip_rval, tc_skip_lval, tc_skip_cs, tc_dec, tc_defer,
          tc_formal, tc_return);
  fprintf(stderr, "LEDGER p=%d dup_es=%d dup_cs=%d churn=%d\n", analysis_pass, ld_dup_es, ld_dup_cs, ld_churn);
  ic_arg = ic_ret = ic_retn = tc_formal = tc_return = 0;
  tc_seen = tc_skip_rval = tc_skip_lval = tc_skip_cs = tc_dec = tc_defer = 0;
}

static void report_markwhy() {
  if (!mark_why_enabled()) return;
  fprintf(stderr, "MARKWHY p=%d cs_differ=%ld cs_same=%ld\n", analysis_pass, mark_cs_differ, mark_cs_same);
  mark_cs_differ = mark_cs_same = 0;
}

static void report_keydrift() {
  if (!getenv("IFA_DBG_KEYDRIFT")) return;
  fprintf(stderr, "KEYDRIFT p=%d stable=%ld grew=%ld shrank=%ld flip=%ld new=%ld", analysis_pass, kd_stable, kd_grew,
          kd_shrank, kd_flip, kd_new);
  if (kd_flip) {
    kd_flip_funs.set_to_vec();  // set_add leaves null holes
    qsort_by_id(kd_flip_funs);
    fprintf(stderr, " flip_funs=");
    for (Fun *f : kd_flip_funs)
      if (f && f->sym) fprintf(stderr, "%s#%d,", f->sym->name ? f->sym->name : "?", f->sym->id);
  }
  fprintf(stderr, "\n");
  kd_stable = kd_grew = kd_shrank = kd_new = kd_flip = 0;
  kd_flip_funs.clear();
}

static void report_canon_stats() {
  if (!canon_enabled() || !getenv("IFA_DBG_CANON")) return;
  fprintf(stderr, "CANON p=%d hit=%ld miss=%ld conflict=%ld conflict_honored=%ld\n", analysis_pass, canon_hit,
          canon_miss, canon_conflict, canon_conflict_honored);
  canon_hit = canon_miss = canon_conflict = canon_conflict_honored = 0;
}

// ifa/issues/074: how many contours would each NAMING scheme give this
// function, versus how many IFA actually built?
//
//   ess    -- contours IFA built (splitters included)
//   setkey -- distinct tuples of argument type SETS (what PYC_CANON
//             names by, and what entry_set_compatibility compares)
//   cpakey -- distinct tuples of SINGLE CreationSets over all the
//             function's call edges: the cartesian-product naming
//             shedskin's dcpa uses
//
// The question this answers: is mark-based splitting recovering a
// distinction that cartesian-product naming already makes structurally
// (cpakey ~ ess >> setkey), or one that no type-tuple naming can make
// (ess >> cpakey)?
static void keyspace_cpa(AEdge *e, Vec<MPosition *> &pos, int i, std::string &tup, std::set<std::string> &out,
                         int &budget) {
  if (budget <= 0) return;
  if (i == pos.n) {
    out.insert(tup);
    --budget;
    return;
  }
  AVar *a = e->args.get(pos[i]);
  if (!a) return keyspace_cpa(e, pos, i + 1, tup, out, budget);
  AType *t = type_intersection(a->out->type, e->match->formal_filters.get(pos[i]));
  size_t keep = tup.size();
  for (CreationSet *cs : t->sorted) {
    tup += std::to_string(cs ? cs->id : -1) + ",";
    keyspace_cpa(e, pos, i + 1, tup, out, budget);
    tup.resize(keep);
  }
}

static void report_keyspace() {
  if (!getenv("IFA_DBG_KEYSPACE")) return;
  const char *only = getenv("IFA_DBG_KEYSPACE_FUN");
  for (Fun *f : fa->pdb->funs) {
    if (f->ess.n < 2) continue;
    if (only && !(f->sym->name && strstr(f->sym->name, only))) continue;
    std::set<std::string> setkeys, cpakeys;
    Vec<MPosition *> pos;
    for (MPosition *p : f->positional_arg_positions) pos.add(p);
    int budget = 20000;
    for (EntrySet *es : f->ess) if (es) {
      std::string k;
      for (MPosition *p : pos) {
        AVar *a = es->args.get(p);
        k += std::to_string((uintptr_t)(a ? (void *)a->out->type : nullptr)) + "|";
      }
      setkeys.insert(k);
      if (only && getenv("IFA_DBG_KEYSPACE_DUMP")) {
        int nedges = 0;
        for (AEdge *ee : es->edges) if (ee) ++nedges;
        fprintf(stderr, "  [key] es=%d edges=%d filters=%d split=%d", es->id, nedges, es->filters.n,
                es->split ? es->split->id : -1);
        for (MPosition *p : pos) {
          AVar *a = es->args.get(p);
          fprintf(stderr, " |");
          if (a && a->out && a->out->type)
            for (CreationSet *cs : a->out->type->sorted)
              fprintf(stderr, " %s#%d", cs->sym && cs->sym->name ? cs->sym->name : "?", cs->id);
        }
        fprintf(stderr, "\n");
      }
      for (AEdge *e : es->edges) if (e && e->args.n && e->match) {
        std::string tup;
        keyspace_cpa(e, pos, 0, tup, cpakeys, budget);
      }
    }
    fprintf(stderr, "KEYSPACE p=%d fun=%s#%d ess=%d setkey=%d cpakey=%d%s\n", analysis_pass,
            f->sym->name ? f->sym->name : "?", f->sym->id, f->ess.n, (int)setkeys.size(), (int)cpakeys.size(),
            budget <= 0 ? " (cpa truncated)" : "");
  }
}

static const char *kStageName[FA::kNumFAPassStages] = {
    "TYPE_CONFL",  "MARK_TYPE",   "SETTER",      "SETTER_OF_SETTER", "MARK_SETTER",
    "MARK_SET_OF_SET", "VIOLATION", "PER_CS_RECV", "CSM_ELEM_CS",  "CPA"};

// TEMP probe: which splitter STAGE is producing the per-pass churn.
// ifa/issues/101: per-pass CreationSet population, grouped by the SYM of
// the allocation site. Tests whether contour growth is feeding CS growth
// -- creation_point mints one CS per (site x contour), so a self-
// amplifying loop would show a handful of syms with CS counts tracking
// the contour count. Probe-only.
static void report_cs_population() {
  if (!getenv("IFA_DBG_CSPOP")) return;
  std::map<std::string, int> by_sym;
  for (CreationSet *cs : fa->css) if (cs)
    by_sym[cs->sym && cs->sym->name ? cs->sym->name : "(anon)"]++;
  std::vector<std::pair<std::string, int>> v(by_sym.begin(), by_sym.end());
  std::sort(v.begin(), v.end(), [](auto &a, auto &b) { return a.second > b.second; });
  fprintf(stderr, "CSPOP p=%d css=%d syms=%d |", analysis_pass, fa->css.n, (int)v.size());
  for (int i = 0; i < 8 && i < (int)v.size(); i++) fprintf(stderr, " %s:%d", v[i].first.c_str(), v[i].second);
  fprintf(stderr, "\n");
}

// ifa/issues/101: for each CONTAINER CreationSet, its ELEMENT type. The
// question this answers: CreationSets exist to carry container
// parameterization (shedskin's `list<T>`), so how many DISTINCT element
// types are there, against how many CreationSets? A large gap means CS
// identity is over-discriminating -- keyed on allocation site x contour
// rather than on the parameter it is supposed to capture.
// ifa/issues/124: dump every EntrySet of a named function with the
// CreationSets its positional formals hold. Answers "should ES
// splitting have split this?" directly -- if one contour's receiver
// formal holds two different container CSs, it should have.
static void report_fun_contours() {
  cchar *want = getenv("IFA_DBG_FUNCONTOURS");
  if (!want) return;
  for (EntrySet *es : fa->ess) if (es && es->fun && es->fun->sym && es->fun->sym->name) {
    if (strcmp(es->fun->sym->name, want)) continue;
    fprintf(stderr, "FUNC %s es=%d", want, es->id);
    for (MPosition *p : es->fun->positional_arg_positions) {
      AVar *a = es->args.get(p);
      if (!a) continue;
      fprintf(stderr, " | arg%d:", (int)Position2int(p->pos[0]));
      for (CreationSet *c : a->out->sorted)
        fprintf(stderr, " %s#%d", c->sym && c->sym->name ? c->sym->name : "?", c->id);
    }
    fprintf(stderr, "\n");
  }
}

// ifa/issues/129 step 1: the element-shape census, shared by the per-pass
// ELEMTYPE probe and the end-of-analysis DEMAND ratio.
//
// The question it answers: CreationSets exist to carry container
// parameterization (shedskin's `list<T>`), so how many DISTINCT element
// types are there, against how many CreationSets? shedskin's data-contour
// identity IS that element-class tuple (`ifa_class_types` / `classes_nr`,
// audited in ifa/issues/129), so `shapes` is the contour count pyc would
// have if CS identity were demand-driven, and `cs / shapes` is the factor
// by which it over-discriminates today -- ifa/issues/128 measured 95 list
// CSs standing for 6 element types on chess.
//
// It is READ-ONLY; see the note at the AVar read in element_census().
//
// Keyed on the container SYM POINTER, and the element shape on the element
// CreationSets' SYM IDS -- never on names. Two classes can share a name
// (CLAUDE.md), and a name-keyed census silently merges them; names here are
// for display only.
struct ElemCensus {
  std::map<Sym *, int> cs_count;                   // container sym -> CSs
  std::map<Sym *, std::set<void *>> types;         // -> distinct element ATypes
  std::map<Sym *, std::set<std::string>> shapes;   // -> distinct MERGED content class-sets
  std::map<Sym *, std::set<std::string>> pshapes;  // -> distinct POSITIONAL content shapes
  int n_cs = 0;     // container CreationSets, all of them
  int n_empty = 0;  // no content in either channel: indistinguishable by any observable
  int n_mixed = 0;  // content holds both a scalar and a container (issue 018)
  int n_novar = 0;  // no element AVar exists -- content, if any, is positional only
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

// A container's content lives in TWO channels, and a census that reads only
// one is wrong. make_kind fills `cs->vars` per position and deliberately
// does NOT flow it into the generic element (fa.cc, ifa/issues/104: a
// heterogeneous tuple read by constant indices keeps precise per-field
// types only because its element stays bottom, which is what tuple_able()
// tests for). So `[1,2,3]` and `["x","y"]` BOTH present an empty element
// AVar -- measured -- and an element-only census calls them one shape and
// reports a 2x over-discrimination that is not there.
//
// Hence two denominators, and the honest answer is between them:
//   shapes   content classes MERGED across the element and every position.
//            This is shedskin's identity for a one-tvar container: two
//            int lists of different lengths are ONE contour.
//   pshapes  the element set plus the ORDERED per-position class-sets.
//            This is record identity: arity and field order count.
// A demand-driven splitter lands between `shapes` (list-like) and
// `pshapes` (record-like), so cs/shapes bounds the over-discrimination
// from above and cs/pshapes from below.
static void element_census(ElemCensus &c) {
  for (CreationSet *cs : fa->css) if (cs && cs->sym && cs->sym->element) {
    ++c.n_cs;
    c.cs_count[cs->sym]++;
    // READ-ONLY, and it must stay that way. get_element_avar() is not an
    // accessor: it calls unique_AVar (which CREATES the AVar when absent)
    // and sets cs->added_element_var, and that flag gates element numeric
    // coercion in fa_coerce_numeric_confluences. A probe that called it
    // would be mutating the analysis it is measuring. A CS with no element
    // AVar is counted with a bottom element channel -- its content, if
    // any, is in `vars` and is read below -- and tallied as `novar`.
    AType *et = nullptr;
    if (cs->added_element_var) {
      if (AVar *e = unique_AVar(cs->sym->element->var, cs)) et = e->out ? e->out->type : nullptr;
    } else {
      ++c.n_novar;
    }
    c.types[cs->sym].insert((void *)et);
    std::set<int> merged;
    std::string pkey = "e:";
    bool has_scalar = false, has_container = false;
    auto absorb = [&](AType *t, std::string &into) {
      if (!t) return;
      for (CreationSet *x : t->sorted) {
        int id = x->sym ? x->sym->id : -1;
        merged.insert(id);
        into += " " + std::to_string(id);
        if (x->sym && x->sym->element)
          has_container = true;
        else if (x->sym && x->sym->type && x->sym->type->num_kind)
          has_scalar = true;
      }
    };
    absorb(et, pkey);
    for (AVar *iv : cs->vars) {
      pkey += "|";
      absorb(iv && iv->out ? iv->out->type : nullptr, pkey);
    }
    std::string mkey;
    for (int id : merged) mkey += " " + std::to_string(id);
    c.shapes[cs->sym].insert(mkey);
    c.pshapes[cs->sym].insert(pkey);
    if (merged.empty()) ++c.n_empty;
    if (has_scalar && has_container) ++c.n_mixed;
  }
}

static void report_element_setters() {
  if (!getenv("IFA_DBG_ELEMSETTER")) return;
  for (CreationSet *cs : fa->css) if (cs && cs->sym && cs->sym->element) {
    if (!cs->added_element_var) continue;  // read-only, see element_census()
    AVar *e = unique_AVar(cs->sym->element->var, cs);
    if (!e) continue;
    fprintf(stderr, "ELEM cs=%d sym=%s elem_av=%d ntypes=%d nback=%d\n", cs->id,
            cs->sym->name ? cs->sym->name : "?", e->id, e->out->type->n, e->backward.n);
    for (AVar *b : e->backward) if (b) {
      EntrySet *bes = b->contour_is_entry_set ? (EntrySet *)b->contour : nullptr;
      cchar *fn = bes && bes->fun && bes->fun->sym && bes->fun->sym->name ? bes->fun->sym->name : "?";
      fprintf(stderr, "   <- av=%d in_fun=%s es=%d container=%d setter_class=%d\n", b->id, fn,
              bes ? bes->id : -1, b->container ? b->container->id : -1, b->setter_class ? 1 : 0);
    }
    for (AVar *d : cs->defs) if (d)
      fprintf(stderr, "   def av=%d setters=%d cs_map=%d\n", d->id, d->setters ? d->setters->n : -1,
              d->cs_map ? 1 : 0);
  }
}

// ifa/issues/101: per container sym, its CS count against the element
// types and element class-shapes those CSs stand for.
// ifa/issues/124: dump every EntrySet of a named function with the
// CreationSets its positional formals hold (report_fun_contours, above).
static void report_element_types() {
  if (!getenv("IFA_DBG_ELEMTYPE")) return;
  ElemCensus c;
  element_census(c);
  report_element_setters();
  if (getenv("IFA_DBG_ELEMTYPE_DUMP")) {
    // Print each container CS with its element type spelled out as the
    // SYMS of the element CreationSets, plus their ids. If the distinct
    // element types collapse to a handful of shapes, the extra
    // discrimination is inner-CS identity, not a real type difference.
    std::map<std::string, int> shape;
    for (CreationSet *cs : fa->css) if (cs && cs->sym && cs->sym->element) {
      if (!cs->added_element_var) continue;  // read-only, see element_census()
      AVar *e = unique_AVar(cs->sym->element->var, cs);
      if (!e) continue;
      std::string byid, bysym;
      for (CreationSet *x : e->out->type->sorted) {
        bysym += std::string(" ") + (x->sym && x->sym->name ? x->sym->name : "?");
        byid += " " + std::to_string(x->id);
      }
      fprintf(stderr, "  [elem] cs=%d %s elem_syms=[%s ] elem_ids=[%s ]\n", cs->id,
              cs->sym->name ? cs->sym->name : "?", bysym.c_str(), byid.c_str());
      shape[bysym]++;
    }
    fprintf(stderr, "  [elem] distinct SYM-shapes: %d\n", (int)shape.size());
    for (auto &kv : shape) fprintf(stderr, "  [elem]   %-40s x%d\n", kv.first.c_str(), kv.second);
  }
  // Sorted by name, then id: std::map over Sym* is POINTER order, which
  // reorders the line run to run and makes two logs undiffable.
  std::vector<Sym *> syms;
  for (auto &kv : c.cs_count) syms.push_back(kv.first);
  std::sort(syms.begin(), syms.end(), [](Sym *a, Sym *b) {
    int r = strcmp(a->name ? a->name : "", b->name ? b->name : "");
    return r ? r < 0 : a->id < b->id;
  });
  fprintf(stderr, "ELEMTYPE p=%d |", analysis_pass);
  for (Sym *s : syms)
    fprintf(stderr, " %s: %d CS / %d elemtypes / %d shapes / %d pshapes;", s->name ? s->name : "(anon)",
            c.cs_count[s], (int)c.types[s].size(), (int)c.shapes[s].size(), (int)c.pshapes[s].size());
  fprintf(stderr, " || empty=%d mixed=%d novar=%d\n", c.n_empty, c.n_mixed, c.n_novar);
}

static void report_stage_churn() {
  if (!getenv("IFA_DBG_STAGE")) return;
  fprintf(stderr, "STAGE p=%d", analysis_pass);
  for (int i = 0; i < FA::kNumFAPassStages; i++)
    if (fa->dbg_stage_detach[i] || fa->dbg_stage_mint[i] || fa->dbg_stage_reuse[i] || fa->dbg_stage_csmint[i])
      fprintf(stderr, " %s(det=%ld mint=%ld reuse=%ld csmint=%ld)", kStageName[i], fa->dbg_stage_detach[i],
              fa->dbg_stage_mint[i], fa->dbg_stage_reuse[i], fa->dbg_stage_csmint[i]);
  // NOTE deliberately no violation count here: type_violations is
  // collected by collect_var_type_violations() inside extend_analysis(),
  // which runs AFTER complete_pass, so reading it at this point reports
  // 0 (or a stale tally) rather than this pass's. See the VIOL line
  // emitted at the collection site instead.
  fprintf(stderr, " | ess=%d css=%d\n", fa->ess.n, fa->css.n);
  for (int i = 0; i < FA::kNumFAPassStages; i++)
    fa->dbg_stage_detach[i] = fa->dbg_stage_mint[i] = fa->dbg_stage_reuse[i] = fa->dbg_stage_csmint[i] = 0;
}

static void complete_pass() {
  report_stage_churn();
  report_cs_population();
  report_element_types();
  report_fun_contours();
  report_canon_stats();
  report_keyspace();
  capture_type_keys();
  capture_elem_keys();
  report_degenerate_avars();
  report_keydrift();
  report_markwhy();
  report_incompat();
  audit_edge_arg_values();
  collect_results();
  collect_argument_type_violations();
  if (getenv("IFA_DBG_DISPATCHFAIL"))
    fprintf(stderr, "DISPATCHFAIL pass=%d total=%ld sites=%ld reported=%ld\n", analysis_pass,
            fa->dbg_dispatch_total_sites, fa->dbg_dispatch_fail_sites, fa->dbg_dispatch_fail_reported);
  collect_var_type_violations();
  // ifa/issues/104: of the VIOLATIONS actually recorded, how many are on
  // an AVar whose type holds tuples of DIFFERING ARITY? That is the
  // direct "do mixed-arity tuples cause real failures here" question --
  // as opposed to merely occurring. Probe-only.
  if (getenv("IFA_DBG_ARITYVIOL")) {
    int nviol = 0, narity = 0, ntuple = 0, nhomo_arity = 0;
    for (ATypeViolation *v : fa->type_violations) {
      if (!v || !v->av || !v->av->out || !v->av->out->type) continue;
      ++nviol;
      int ntup = 0, n0 = -1;
      bool mixed = false;
      for (CreationSet *c : v->av->out->type->sorted) {
        if (!c->sym || c->sym != sym_tuple) continue;
        ++ntup;
        if (n0 < 0) n0 = c->vars.n;
        else if (n0 != c->vars.n) mixed = true;
      }
      if (ntup > 1) ++ntuple;
      if (mixed) {
        ++narity;
        // Would one variable-length homogeneous tuple<T> cover this? Only
        // if every tuple in the union is monomorphic AND they agree.
        Sym *e0 = nullptr;
        bool homo = true;
        for (CreationSet *c : v->av->out->type->sorted) {
          if (!c->sym || c->sym != sym_tuple) continue;
          for (AVar *fv : c->vars) if (fv) {
            Sym *t = fv->out ? basic_type(fa, fv->out, (Sym *)-1) : nullptr;
            if (!t) { homo = false; break; }
            if (!e0) e0 = t;
            else if (e0 != t) { homo = false; break; }
          }
          if (!homo) break;
        }
        if (homo) ++nhomo_arity;
        if (getenv("IFA_DBG_ARITYWHERE")) {
          Var *vr = v->av->var;
          fprintf(stderr, "[aritywhere] fun=%s var=%s kind=%d arities=", 
                  vr && vr->sym && vr->sym->in && vr->sym->in->name ? vr->sym->in->name : "?",
                  vr && vr->sym && vr->sym->name ? vr->sym->name : "_", (int)v->kind);
          for (CreationSet *c : v->av->out->type->sorted)
            if (c->sym == sym_tuple) fprintf(stderr, "%d,", c->vars.n);
          // ALSO the non-tuple syms in the same type: if a violation's
          // type mixes tuples with scalars/other classes, the boxing
          // problem is that mix, not the tuple arity.
          fprintf(stderr, " others=");
          for (CreationSet *c : v->av->out->type->sorted)
            if (c->sym != sym_tuple) fprintf(stderr, "%s,", c->sym && c->sym->name ? c->sym->name : "?");
          fprintf(stderr, "\n");
        }
      }
    }
    fprintf(stderr, "ARITYVIOL p=%d violations=%d with_multi_tuple=%d WITH_MIXED_ARITY=%d homogeneous=%d\n",
            analysis_pass, nviol, ntuple, narity, nhomo_arity);
  }
  // ifa/issues/074: sample the violation count HERE, at the collection
  // point -- this is the only place in the pass where it is meaningful.
  if (getenv("IFA_DBG_STAGE"))
    fprintf(stderr, "VIOL p=%d n=%d ess=%d css=%d\n", analysis_pass, fa->type_violations.set_count(), fa->ess.n,
            fa->css.n);
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

// ifa/issues/039, the `safe` environment: substitute a typed zero for
// the phi operands that mark "control reached this merge without ever
// assigning the variable" (marked by mark_unbound_phi_operands,
// optimize/ssu.cc).
//
// It has to happen HERE, between passes, because the zero's type is the
// merged type, and that is exactly what the pass just computed. Pass 1
// gives the marked operand nothing, so the merge carries only the
// assigning paths' type; this reads that off, rewrites the operand to a
// constant of it, and asks for another pass. From then on the operand is
// an ordinary constant and this is a no-op, so the loop settles.
//
// Between-pass IF1 rewriting is sound because analyze_to_convergence
// clears every Var, contour and edge before EVERY pass (ifa/issues/098),
// so the next pass rebuilds all constraints from the rewritten code. It
// is also why this is not a flow edge or an update_gen: a snapshot taken
// during one pass can be lost on the final one, whereas rewritten IF1 is
// re-read every pass by construction.
static Sym *unbound_fill_constant(Var *lval) {
  Vec<CreationSet *> css;
  for (int i = 0; i < lval->avars.n; i++)
    if (lval->avars[i].key && lval->avars[i].value) css.set_union(*lval->avars[i].value->out);
  Sym *basic = nullptr;
  for (CreationSet *cs : css) {
    if (!cs || !cs->sym) continue;
    Sym *t = cs->sym->is_constant ? cs->sym->type : cs->sym;
    if (!t) return nullptr;
    // ifa's own zero is the zero of a NUMBER. Anything else -- a
    // record, a string, a closure -- has no language-neutral zero, so
    // ifa declines rather than inventing one; that fill is the
    // frontend's to supply (IFACallbacks).
    if (t->num_kind != IF1_NUM_KIND_INT && t->num_kind != IF1_NUM_KIND_UINT && t->num_kind != IF1_NUM_KIND_FLOAT)
      return nullptr;
    if (basic && basic != t) return nullptr;  // more than one, no single zero
    basic = t;
  }
  if (!basic) return nullptr;
  return if1_const(if1, basic, basic->num_kind == IF1_NUM_KIND_FLOAT ? "0.0" : "0");
}

// ifa/issues/055: per-pass contour trace. PYC_DBG_CONTOURS=<fun name>
// prints, at the end of every pass, one line per EntrySet of that
// function (its formals' and return's types) and one line per
// CreationSet of the named container, with its element type. That is
// exactly the information needed to see WHERE the ideal monomorphic
// contours fail to be derived: the repro's ideal is two difference
// contours, set<int64> and set<str>, each with its own `r`.
static AVar *dbg_element_avar(CreationSet *cs) {
  if (!cs || !cs->sym || !cs->sym->element) return nullptr;
  // Scan rather than get_element_avar(): that one CREATES the AVar and
  // sets added_element_var, which a debug path must not do.
  for (AVar *av : cs->vars)
    if (av && av->var && av->var->sym == cs->sym->element) return av;
  return nullptr;
}

static void dbg_atype_str(AType *t, char *buf, int n, int depth) {
  if (n <= 0) return;
  buf[0] = 0;
  if (!t) { snprintf(buf, n, "<null>"); return; }
  if (t == fa->type_world.bottom_type) { snprintf(buf, n, "bottom"); return; }
  int used = 0, count = 0;
  for (CreationSet *cs : t->sorted) {
    if (!cs || !cs->sym) continue;
    if (used >= n - 1) break;
    if (count++) used += snprintf(buf + used, n - used, "|");
    if (used >= n - 1) break;
    Sym *ty = cs->sym->is_constant && cs->sym->type ? cs->sym->type : cs->sym;
    cchar *nm = ty->name ? ty->name : "?";
    used += snprintf(buf + used, n - used, "%s#%d", nm, cs->id);
    if (depth > 0 && used < n - 1) {
      AVar *e = dbg_element_avar(cs);
      if (e) {
        char sub[128];
        dbg_atype_str(e->out, sub, (int)sizeof sub, depth - 1);
        used += snprintf(buf + used, n - used, "<%s>", sub);
      }
    }
  }
  if (!count) snprintf(buf, n, "{}");
}

// ifa/issues/055: bounded backward walk from a CreationSet field, to
// find WHERE a union is formed. Prints each AVar with its contour and
// its out type; the first AVar whose out already holds the whole union
// is the merge point, and the edge below it is where a split would have
// to happen.
static void dbg_backwalk(AVar *av, int depth, int maxdepth, Vec<AVar *> &seen) {
  if (!av || depth > maxdepth || !seen.set_add(av)) return;
  char t[224];
  dbg_atype_str(av->out, t, (int)sizeof t, 0);
  char where[96];
  if (av->contour_is_entry_set) {
    EntrySet *e = (EntrySet *)av->contour;
    snprintf(where, sizeof where, "es%d:%s", e ? e->id : -1,
             e && e->fun && e->fun->sym && e->fun->sym->name ? e->fun->sym->name : "?");
  } else {
    CreationSet *c = (CreationSet *)av->contour;
    snprintf(where, sizeof where, "cs%d:%s", c ? c->id : -1,
             c && c->sym && c->sym->name ? c->sym->name : "?");
  }
  int nback = 0;
  for (AVar *b : av->backward) if (b) ++nback;
  fprintf(stderr, "%*sBACK av=%d %s@%s nback=%d out=%s\n", depth * 2, "", av->id,
          av->var && av->var->sym && av->var->sym->name ? av->var->sym->name : "?", where, nback, t);
  for (AVar *b : av->backward) if (b) dbg_backwalk(b, depth + 1, maxdepth, seen);
}

static void dbg_dump_contours(int pass) {
  static cchar *want = nullptr;
  static int checked = 0;
  if (!checked) { want = getenv("PYC_DBG_CONTOURS"); checked = 1; }
  if (!want) return;
  int n_es = 0;
  for (EntrySet *es : fa->ess) {
    if (!es || !es->fun || !es->fun->sym || !es->fun->sym->name) continue;
    if (strcmp(es->fun->sym->name, want)) continue;
    ++n_es;
    char args[768];
    args[0] = 0;
    int used = 0;
    for (int i = 0; i < es->args.n; i++) {
      if (!es->args.v[i].key) continue;
      AVar *av = es->args.v[i].value;
      char t[192];
      dbg_atype_str(av ? av->out : nullptr, t, (int)sizeof t, 1);
      used += snprintf(args + used, (int)sizeof args - used, "%s%s", used ? " " : "", t);
      if (used >= (int)sizeof args - 1) break;
    }
    char ret[192];
    dbg_atype_str(es->rets.n && es->rets.v[0] ? es->rets.v[0]->out : nullptr, ret, (int)sizeof ret, 1);
    // ifa/issues/055: setters on the RETURN AVar -- that is the object
    // AVar that split_css needs as a "setter starter" (setters are
    // registered on x->container, i.e. the object, not on the field).
    AVar *rv = es->rets.n ? es->rets.v[0] : nullptr;
    fprintf(stderr, "CONTOUR pass=%d %s es=%d args=[%s] ret=%s ret_setters=%d ret_class=%d ret_cs_map=%d\n", pass,
            want, es->id, args, ret, (rv && rv->setters) ? rv->setters->set_count() : -1,
            (rv && rv->setter_class) ? rv->setter_class->set_count() : -1, (rv && rv->cs_map) ? 1 : 0);
  }
  // PYC_DBG_CONTOURS=* : which functions own the contour growth.
  if (!strcmp(want, "*")) {
    Vec<Fun *> funs;
    Vec<int> counts;
    for (EntrySet *es : fa->ess) {
      if (!es || !es->fun) continue;
      int at = -1;
      for (int i = 0; i < funs.n; i++) if (funs.v[i] == es->fun) { at = i; break; }
      if (at < 0) { funs.add(es->fun); counts.add(1); }
      else counts.v[at]++;
    }
    for (int round = 0; round < 8; round++) {
      int best = -1;
      for (int i = 0; i < counts.n; i++) if (counts.v[i] > 0 && (best < 0 || counts.v[i] > counts.v[best])) best = i;
      if (best < 0 || counts.v[best] < 2) break;
      cchar *nm = funs.v[best]->sym && funs.v[best]->sym->name ? funs.v[best]->sym->name : "?";
      fprintf(stderr, "  TOPFUN pass=%d %-24s contours=%d\n", pass, nm, counts.v[best]);
      counts.v[best] = 0;
    }
    fprintf(stderr, "CONTOUR pass=%d SUMMARY total_ess=%d total_css=%d\n", pass, fa->ess.n, fa->css.n);
    return;
  }
  // PYC_DBG_CSSYM names the container whose CreationSets to dump
  // (default "set"); its data fields are printed with CS ids.
  static cchar *cssym = nullptr;
  static int cssym_checked = 0;
  if (!cssym_checked) {
    cssym = getenv("PYC_DBG_CSSYM");
    if (!cssym) cssym = "set";
    cssym_checked = 1;
  }
  int n_cs = 0;
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym || !cs->sym->name || strcmp(cs->sym->name, cssym)) continue;
    ++n_cs;
    // pyc's `set` is a Python-level class, so there is no sym->element:
    // the element type lives in the _items field. Dump the CS's member
    // AVars instead.
    char flds[512];
    flds[0] = 0;
    int fu = 0;
    for (AVar *av : cs->vars) {
      if (!av || !av->var || !av->var->sym) continue;
      cchar *fn = av->var->sym->name;
      // Data fields only. The method slots (__and__, add, ...) are also
      // in cs->vars and are long enough to overflow this buffer before
      // reaching `_items`, which is the one that matters.
      if (!fn || fn[0] != '_' || (fn[1] == '_' && fn[2])) continue;
      char t[192];
      dbg_atype_str(av->out, t, (int)sizeof t, 1);
      // ifa/issues/055: also report the setter state, since split_css
      // (the demand-driven CS split) only ever sees AVars that carry
      // setters -- an empty `setters` here means the back-flow never
      // reached this field and the CS can never be split from it.
      fu += snprintf(flds + fu, (int)sizeof flds - fu, "%s%s=%s[setters=%d class=%d]", fu ? " " : "", fn, t,
                     av->setters ? av->setters->set_count() : -1,
                     av->setter_class ? av->setter_class->set_count() : -1);
      if (fu >= (int)sizeof flds - 1) break;
    }
    // ifa/issues/055: defs is the number of AVars that created this CS.
    // split_css partitions THAT set, and its loop is `while
    // (starter_set.n > 1)` -- so a CS with a single def can never be
    // split, however the setters partition.
    fprintf(stderr, "  %sCS pass=%d cs=%d defs=%d %s\n", cssym, pass, cs->id, cs->defs.set_count(), flds);
  }
  // ifa/issues/113 link: are the `_items` AVars of distinct set CSs in
  // ONE setter equivalence class? If so their values are merged by the
  // partition, which is what would make three separate sets appear to
  // share all three backing lists.
  {
    Vec<void *> classes;
    int n_items = 0;
    for (CreationSet *cs : fa->css) {
      if (!cs || !cs->sym || !cs->sym->name || strcmp(cs->sym->name, cssym)) continue;
      for (AVar *av : cs->vars) {
        if (!av || !av->var || !av->var->sym || !av->var->sym->name) continue;
        if (strcmp(av->var->sym->name, "_items") && strcmp(av->var->sym->name, "_keys")) continue;
        ++n_items;
        char ft[256];
        dbg_atype_str(av->out, ft, (int)sizeof ft, 0);
        fprintf(stderr, "  ITEMS pass=%d setcs=%d av=%d out=%s setter_class=%p\n", pass, cs->id, av->id, ft,
                (void *)av->setter_class);
        // ifa/issues/055 next step: WHO contributes each CreationSet.
        // Walk the incoming flow edges and name each source AVar by its
        // Var, its contour, and what it carries.
        for (AVar *src : av->backward) {
          if (!src) continue;
          char st[192];
          dbg_atype_str(src->out, st, (int)sizeof st, 0);
          cchar *vn = src->var && src->var->sym && src->var->sym->name ? src->var->sym->name : "?";
          char where[96];
          if (src->contour_is_entry_set) {
            EntrySet *ses = (EntrySet *)src->contour;
            snprintf(where, sizeof where, "es%d:%s", ses ? ses->id : -1,
                     ses && ses->fun && ses->fun->sym && ses->fun->sym->name ? ses->fun->sym->name : "?");
          } else {
            CreationSet *scs = (CreationSet *)src->contour;
            snprintf(where, sizeof where, "cs%d:%s", scs ? scs->id : -1,
                     scs && scs->sym && scs->sym->name ? scs->sym->name : "?");
          }
          fprintf(stderr, "    <- src av=%d %s@%s line=%d out=%s\n", src->id, vn, where,
                  src->var && src->var->def && src->var->def->code ? src->var->def->code->line() : -1, st);
        }
        if (av->setter_class) classes.set_add((void *)av->setter_class);
        if (getenv("PYC_DBG_BACKWALK")) {
          Vec<AVar *> seen;
          dbg_backwalk(av, 0, atoi(getenv("PYC_DBG_BACKWALK")), seen);
        }
      }
    }
    fprintf(stderr, "  ITEMS pass=%d SUMMARY _items_avars=%d distinct_setter_classes=%d\n", pass, n_items,
            classes.n);
  }
  // ifa/issues/055: how much of the setter machinery is engaged at all.
  int with_setters = 0, with_class = 0, cs_with_multidef = 0;
  foreach_avar([&](AVar *a) {
    if (!a) return;
    if (a->setters) ++with_setters;
    if (a->setter_class) ++with_class;
  });
  for (CreationSet *c : fa->css) if (c && c->defs.set_count() > 1) ++cs_with_multidef;
  fprintf(stderr,
          "CONTOUR pass=%d SUMMARY %s_contours=%d %s_CSs=%d total_ess=%d avars_with_setters=%d "
          "avars_with_setter_class=%d css_with_multiple_defs=%d\n",
          pass, want, n_es, cssym, n_cs, fa->ess.n, with_setters, with_class, cs_with_multidef);
}

static bool apply_unbound_fills() {
  if (!fauto_init_unbound) return false;
  bool changed = false;
  for (Fun *f : fa->funs) {
    if (!f->entry) continue;
    Vec<PNode *> nodes;
    f->collect_PNodes(nodes);
    for (PNode *p : nodes)
      for (PNode *ph : p->phi) {
        if (!ph->lvals.n || !ph->lvals[0]) continue;
        for (int i = 0; i < ph->rvals.n; i++) {
          Var *v = ph->rvals.v[i];
          if (!v || !v->is_unbound_fill) continue;
          Sym *z = unbound_fill_constant(ph->lvals[0]);
          if (getenv("IFA_DBG_UNBOUND_FILL"))
            fprintf(stderr, "UNBOUND_FILL %s: operand %d -> %s\n", f->sym->name ? f->sym->name : "?", i,
                    z ? (z->type && z->type->name ? z->type->name : "const") : "DECLINED");
          if (!z) continue;  // no zero for this type; leave the path as it was
          Var *zv = z->var ? z->var : (z->var = new Var(z));
          ph->rvals.v[i] = zv;
          // A constant only acquires its value through fa_Vars ->
          // add_var_constraint, and collect_Vars_PNodes builds that list
          // ONCE per Fun (fa_collected), long before this runs. A
          // constant introduced here is therefore invisible unless it is
          // registered by hand -- it contributes bottom, the merge stays
          // a single value, and the function folds exactly as it did
          // before the fill. That failure is silent and type-dependent:
          // `0` is usually already in the program from somewhere else,
          // so int cases appeared to work while float ones did not.
          if (f->fa_collected) {
            if (!f->fa_all_Vars.in(zv)) {
              f->fa_all_Vars.add(zv);
              qsort_by_id(f->fa_all_Vars);  // keep the id order the rest of FA assumes
            }
            if (is_fa_Var(zv) && !f->fa_Vars.in(zv)) f->fa_Vars.add(zv);
          }
          changed = true;
        }
      }
  }
  if (changed && getenv("IFA_DBG_UNBOUND_FILL")) fprintf(stderr, "UNBOUND_FILL: substituted typed zeros\n");
  return changed;
}

// ifa/issues/129 step 1: the DEMAND RATIO -- one number for the goal
// statement in CLAUDE.md, emitted once per converged analysis so a corpus
// sweep can carry it (corpus_sweep.sh reads this line).
//
//   container_cs  container CreationSets the analysis ended with
//   shapes        the content class-sets those CSs stand for, MERGED across
//                 the element channel and every position -- the contour
//                 count shedskin's one-tvar identity (`ifa_class_types`)
//                 would produce
//   pshapes       the same, but position-sensitive: record identity, where
//                 arity and field order count
//   ratio         container_cs / shapes -- the over-discrimination factor,
//                 1.0 being demand-driven and higher being splitting driven
//                 by structure (ifa/issues/128)
//   pratio        container_cs / pshapes -- the same factor under the
//                 strictest identity any demand-driven scheme could want,
//                 so `pratio > 1` is splitting NO identity justifies
//
// Two ratios because a container's content lives in two channels and the
// right identity for pyc is somewhere between them; see element_census().
// `ess`/`css` are here because a change that improves the ratio by
// splitting MORE somewhere else is not an improvement, and reading the
// ratio alone would hide that.
static void report_demand_ratio() {
  if (!getenv("IFA_DBG_DEMAND")) return;
  ElemCensus c;
  element_census(c);
  int shapes = c.total_shapes(), pshapes = c.total_pshapes();
  fprintf(stderr,
          "DEMAND passes=%d ess=%d css=%d container_cs=%d shapes=%d pshapes=%d ratio=%.2f pratio=%.2f "
          "elemtypes=%d empty=%d mixed=%d novar=%d\n",
          analysis_pass, fa->ess.n, fa->css.n, c.n_cs, shapes, pshapes,
          shapes ? (double)c.n_cs / shapes : 0.0, pshapes ? (double)c.n_cs / pshapes : 0.0, c.total_types(),
          c.n_empty, c.n_mixed, c.n_novar);
}

static void analyze_to_convergence() {
  // ifa/issues/098: the per-pass reset belongs here, unconditionally,
  // for two reasons. (1) Every pass must start from bottom, whether the
  // splitter or the frontend's reanalyze() asked for it -- the old
  // placement (extend_analysis, gated on `analyze_again`) skipped the
  // reset entirely for reanalyze()-driven passes. (2) This is the only
  // place that can reset without destroying the result: when the loop
  // exits, the converged state must still be there for clone/codegen,
  // so the reset has to happen BEFORE a pass, never after one.
  // `analysis_pass == 0` has nothing to reset (initialize() just built
  // the world), and clearing there would drop the seeded globals.
  bool first_pass = true;
  bool loop_again = false;  // ifa/issues/055
  do {
    // ifa/issues/111 M3: clear only the closure the last pass
    // invalidated, when that is armed and enabled. Falls back to the
    // full reset whenever it declines -- including the first pass,
    // which has no predecessor state to preserve.
    bool first_pass_full_reset = true;
    if (!first_pass && clear_results_selective())
      first_pass_full_reset = false;
    else if (!first_pass)
      clear_results();
    first_pass = false;
    compute_es_can_raise();
    initialize_pass();
    // The top edge is enqueued even under selective invalidation, and
    // that is deliberate. Skipping it (seeding only the affected edges)
    // leaves the STRUCTURE unrebuilt -- fa->ess, es->out_edges,
    // es->creates are all repopulated by this traversal -- and
    // build_call_dominators then walked a call graph with null nodes
    // and segfaulted (optimize/dom.cc:19).
    //
    // The saving does not come from skipping the walk. It comes from
    // what the walk finds already done: a preserved ES keeps its
    // live_pnodes, so add_es_constraints is a no-op for it, and its
    // AVars keep last pass's values, so update_in changes nothing and
    // propagate_out_change never fires. Re-walking a settled contour is
    // the cheap part -- hq2x pass 15 walked ~30 000 AVars with 79 dirty
    // in 0.081s.
    fa->edge_worklist.enqueue(fa->top_edge);
    long edge_count = 0;
    int last_ess_check = fa->ess.n;
    time_t last_ess_change_time = time(nullptr);
    while (fa->edge_worklist.head || fa->send_worklist.head) {
      while (AEdge *e = fa->edge_worklist.pop()) {
        e->in_edge_worklist = 0;
        ++work_edges;  // ifa/111 probe
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
        ++work_sends;  // ifa/111 probe
        add_send_edges_pnode(send->var->def, (EntrySet *)send->contour);
      }
      while (EntrySet *es = fa->es_worklist.pop()) {
        es->in_es_worklist = 0;
        ++work_escons;  // ifa/111 probe
        add_es_constraints(es);
      }
    }
    complete_pass();
    dbg_dump_contours(analysis_pass);  // ifa/issues/055
    // The pass cap bounds the WHOLE loop, including passes kept
    // alive only by reanalyze() (issue 033): with the splitter
    // suppressed by the sticky stall guard, extend_analysis returns
    // 0 but a frontend annotator that never quiesces could
    // otherwise drive flow passes forever.
    // ifa/issues/055: which of the three actually asks for another pass?
    {
      // ifa/issues/055 experiment A (PYC_PROMOTE_FIRST): drive the
      // frontend repair to a FIXED POINT before any splitting. By
      // default extend_analysis() runs first and short-circuits
      // reanalyze, so splitting interleaves with promotion and each
      // promotion re-perturbs contours the splitter had just settled.
      // DEFAULT 2 since ifa/issues/055. By default extend_analysis()
      // ran first and short-circuited reanalyze, so splitting
      // interleaved with the frontend's repair and every promotion
      // re-perturbed contours the splitter had just settled -- measured
      // on plcfrs's 36-line repro as violations settling at 44, being
      // multiplied to 325 by a 2-field promotion, and never recovering
      // their best before the stall guard stopped the analysis.
      //
      // Mode 2 orders it structural-repair -> splitting -> type-reading
      // repair (ifa_reanalyze_phase). Mode 1, which moves ALL of
      // reanalyze early, is wrong: pyc's numeric-confluence coercion
      // reads CONVERGED types, and running it before splitting breaks 11
      // tests, all EXEC and all numeric (bool_ordering,
      // modulo_float_and_sign, minmax_3arg, sum_start_arg,
      // mixed_numeric_field). Structural repair wants to be early; the
      // type-reading half must stay late.
      //
      //   plcfrs   stall at 37 passes, 4993 violations -> CONVERGES,
      //            and now COMPILES
      //   corpus   67 of 77 -> 71 of 77, zero losses: plcfrs, quameon,
      //            rdb and sunfish gained
      //   suite    298 passed / 0 failed, unchanged, both backends
      //
      // Getting here needed four codegen/FA fixes for latent bugs this
      // reordering exposed: the record_args_rets arity crash, sparse
      // struct field numbering, the zero-element tuple cast, and the
      // voidish coerce. All are on the default path and stand on their
      // own.
      static int promote_first = -1;
      if (promote_first < 0) promote_first = getenv("PYC_PROMOTE_FIRST") ? atoi(getenv("PYC_PROMOTE_FIRST")) : 2;
      int ext = 0, rea = 0, fil = 0;
      if (promote_first >= 2) {
        // Structural repair to a fixed point first, splitting next, and
        // the type-reading half of the repair last -- where it has
        // always been.
        ifa_reanalyze_phase = 1;
        rea = if1->callback->reanalyze(fa->type_violations) ? 1 : 0;
        ifa_reanalyze_phase = 0;
        if (!rea) {
          ext = extend_analysis();
          if (!ext) {
            ifa_reanalyze_phase = 2;
            rea = if1->callback->reanalyze(fa->type_violations) ? 1 : 0;
            ifa_reanalyze_phase = 0;
          }
        }
      } else if (promote_first) {
        rea = if1->callback->reanalyze(fa->type_violations) ? 1 : 0;
        if (!rea) ext = extend_analysis();
      } else {
        ext = extend_analysis();
        if (!ext) rea = if1->callback->reanalyze(fa->type_violations) ? 1 : 0;
      }
      if (!ext && !rea) fil = apply_unbound_fills() ? 1 : 0;
      fa->last_pass_reanalyze = (rea != 0);
      if (getenv("PYC_DBG_STAGEDELTA"))
        fprintf(stderr, "PASSEND p=%d extend=%d reanalyze=%d fills=%d viol=%d\n", analysis_pass, ext, rea, fil,
                fa->type_violations.set_count());
      loop_again = (ext || rea || fil);
    }
  } while (loop_again && analysis_pass <= fa->pass_limit);
  if (getenv("PYC_DBG_OSC"))
    fprintf(stderr, "OSC final_pass=%d pass_limit_hit=%d violations=%d ess=%d css=%d selective=%d\n", analysis_pass,
            fa->pass_limit_hit ? 1 : 0, fa->type_violations.set_count(), fa->ess.n, fa->css.n, ifa_selective);
  report_demand_ratio();
}

int FA::analyze(Fun *top) {
  ::fa = this;
  // ifa/issues/074: the stall guard SUPPRESSES the splitter for the rest
  // of the run once it fires, so its limit decides whether a program is
  // "non-convergent" or merely slow. Overridable so that can be measured.
  // ifa/issues/111: read here, beside the other FA loop knobs, so the
  // differential harness can flip it per run without a rebuild. M2:
  // the flag exists and is reported; it changes nothing until M3.
  if (cchar *sv = getenv("IFA_SELECTIVE")) ifa_selective = atoi(sv);
  if (cchar *sl = getenv("IFA_STALL_LIMIT")) stall_limit = atoi(sl);
  if (cchar *nl = getenv("IFA_NONIMPROVE_LIMIT")) nonimprove_limit = atoi(nl);
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
  // ifa/issues/074: WHICH splitter stages this program actually demanded,
  // in cascade order, once for the whole analysis. Deliberately a SET and
  // not per-pass counts -- the counts move with every FA change, but "did
  // this program need the setter splitter at all" is a stable, meaningful
  // property of the program, and is what tests/splitter_*.py pin.
  // ifa/issues/074: the one stable bit a convergence test can assert.
  // PYC_DBG_OSC's line carries pass counts and contour totals, which move
  // with every FA change; "did the analysis reach a fixed point" does
  // not, and is the actual property under test. See
  // tests/deepcopy_recursive_nested_growth.py.
  if (getenv("PYC_DBG_CONVERGED")) fprintf(stderr, "CONVERGED=%d\n", pass_limit_hit ? 0 : 1);
  if (getenv("PYC_DBG_STAGES")) {
    fprintf(stderr, "STAGES:");
    for (int i = 0; i < kNumFAPassStages; i++)
      if (stage_progress_count[i]) fprintf(stderr, " %s", kStageName[i]);
    fprintf(stderr, "\n");
  }
  // issues/018: BOXING is fatal regardless of permissive mode -- see
  // the severity note in show_violations. Boxing the value is
  // deliberately NOT an option here (project decision), so a mixed
  // basic-type union cannot be represented at all and refusing is the
  // only honest answer.
  int n_fatal = 0;
  for (ATypeViolation *v : type_violations)
    if (v && (v->kind == ATypeViolation_kind::BOXING ||
              v->kind == ATypeViolation_kind::DEFINITELY_UNBOUND))
      ++n_fatal;
  int n_advisory = 0;
  for (ATypeViolation *v : type_violations)
    if (v && v->kind == ATypeViolation_kind::MAYBE_UNBOUND) ++n_advisory;
  return ((!fruntime_errors && type_violations.set_count() > n_advisory) || n_fatal) ? -1 : 0;
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
