// ifa/191: an initial value that is always overwritten before any read.
//
// A field's AVar has no program point: every store anywhere flows into it,
// and it only grows, so `self.x = None` in __init__ stays in x's type even
// when every path overwrites it before anything reads it. Retracting the
// store inside the fixed point would be non-monotone (closed/078's Option
// B), so this decides at a FIXED POINT instead, as a recorded decision: the
// store is put in a ledger, the pass re-runs from bottom, and the setter
// transfer (fa_prims.cc) does not flow a recorded store's value into the
// field. Every recorded store is re-walked at every later fixed point.
//
// The walk (OwWalk) is the IFA_DBG_OVERWRITE probe's: see its comment.
#include "ifadefs.h"
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>
#include "fa.h"
#include "fa_flags.h"
#include "fa_census.h"
#include "fa_internal.h"

// ifa/191 direction 2 probe: can a None stored into a field ever be READ
// before the same object's field is overwritten? Read-only; decides nothing.
//
// From the store S1 (`o.f = None` in contour es), walk forward over the CFG,
// into callees (summarised) and, when the function returns, out to every
// caller. `tracked` is the set of Vars, per frame, that MUST hold the object:
// o itself, plain MOVEs of it, a callee formal its actual is bound to, a call
// result when the callee returns it. A path ends at a store of f through a
// tracked Var (the overwrite). The None is OBSERVED if a path reaches a read
// of f -- or any primitive that may read fields -- on a value whose type
// MAY include the object's CreationSet. Reads are checked by type, so an
// alias that escaped is still caught; only overwrites need must-alias.
//
// Verdicts: NEVER (no read before an overwrite on any path -- the None
// could be dropped), OBSERVED (with the node), BUDGET (gave up), and the
// walk notes whether a raising callee sat on a path (RAISE).
namespace {
struct OwWalk {
  Sym *field;
  Vec<CreationSet *> css;  // the object's CreationSets in S1's contour
  int budget = 200000;
  bool observed = false, raise = false, out_of_budget = false;
  PNode *obs_at = nullptr;
  EntrySet *obs_es = nullptr;
  Vec<EntrySet *> on_stack;
  Vec<EntrySet *> up_seen;

  bool may_be_obj(Var *v, EntrySet *es) {
    if (!v) return false;
    AVar *av = make_AVar(v, es);
    if (!av || !av->out) return false;
    for (CreationSet *c : av->out->sorted) if (c && css.set_in(c)) return true;
    return false;
  }
  void observe(PNode *n, EntrySet *es) {
    if (!observed) { obs_at = n; obs_es = es; }
    observed = true;
  }
  static Sym *sel(PNode *n) { return (n->rvals.n >= 4 && n->rvals[3]) ? n->rvals[3]->sym : nullptr; }

  // Callee summary: walk es from its entry with `formals` tracked. Returns
  // true if EVERY path overwrites before exit; sets *returns_obj if a reply
  // hands back a tracked Var. Reads are recorded through observe().
  bool callee(EntrySet *es, Vec<Var *> &formals, bool *returns_obj) {
    if (on_stack.in(es)) return false;  // recursion: assume it exits, unkilled
    on_stack.add(es);
    Vec<Var *> tracked;
    for (Var *v : formals) tracked.set_add(v);
    bool exits = false;
    walk(es, es->fun->entry, tracked, &exits, returns_obj, false);
    on_stack.pop();
    return !exits;
  }

  // Walk from `start` in es. `up`: this is S1's frame or a caller frame, so
  // reaching the exit continues in the callers; otherwise it is a callee
  // frame and reaching the exit sets *exits.
  void walk(EntrySet *es, PNode *start, Vec<Var *> &tracked0, bool *exits, bool *returns_obj, bool up) {
    Vec<PNode *> work, seen;
    Vec<Var *> tracked;
    for (Var *v : tracked0) if (v) tracked.set_add(v);
    if (start && seen.set_add(start)) work.add(start);
    for (int i = 0; i < work.n && !observed; i++) {
      if (--budget < 0) { out_of_budget = true; observed = true; return; }
      PNode *n = work[i];
      bool killed = false;
      if (n->code && n->code->kind == Code_MOVE) {
        for (int k = 0; k < n->rvals.n && k < n->lvals.n; k++)
          if (tracked.set_in(n->rvals[k])) tracked.set_add(n->lvals[k]);
      } else if (n->code && n->code->kind == Code_SEND) {
        Vec<AEdge *> *ve = es->out_edge_map.get(n);
        bool has_edges = false;
        if (ve) for (AEdge *e : *ve) if (e && e->to) has_edges = true;
        if (has_edges) {
          bool all_kill = true, any = false;
          for (AEdge *e : *ve) {
            if (!e || !e->to) continue;
            any = true;
            if (e->to->can_raise) raise = true;
            Vec<Var *> formals;
            for (int k = 0; k < e->args.n; k++) {
              if (!e->args[k].key || !e->args[k].value) continue;
              AVar *a = e->args[k].value;
              if (a->var && tracked.set_in(a->var)) {
                Var *fv = e->to->fun->args.get(e->args[k].key);
                if (fv) formals.add(fv);
              }
            }
            bool ret_obj = false;
            bool k_all = callee(e->to, formals, &ret_obj);
            if (ret_obj) for (Var *l : n->lvals) tracked.set_add(l);
            if (!k_all) all_kill = false;
            if (observed) return;
          }
          if (any && all_kill) killed = true;
        } else if (n->prim) {
          int pi = n->prim->index;
          if (pi == P_prim_setter) {
            if (sel(n) == field && tracked.set_in(n->rvals[1])) killed = true;
          } else if (pi == P_prim_period) {
            if (sel(n) == field && may_be_obj(n->rvals[1], es)) observe(n, es);
          } else if (pi == P_prim_reply) {
            if (n->rvals.n && tracked.set_in(n->rvals[n->rvals.n - 1]) && returns_obj) *returns_obj = true;
          } else if (pi == P_prim_isinstance || pi == P_prim_is || pi == P_prim_id || pi == P_prim_make) {
            // touches the object without reading its fields
          } else {
            for (Var *v : n->rvals) if (may_be_obj(v, es)) { observe(n, es); break; }
          }
        } else {
          // an unresolved send: anything may happen to an object it is given
          for (Var *v : n->rvals) if (may_be_obj(v, es)) { observe(n, es); break; }
        }
      }
      if (observed) return;
      if (killed) continue;
      if (n == es->fun->exit) {
        if (!up) { *exits = true; continue; }
        // Continue in every caller, after its call, with what the caller can
        // still name: actuals bound to tracked formals, and the call result.
        if (up_seen.set_add(es) == nullptr) continue;
        for (AEdge *e : es->edges) {
          if (!e || !e->from || !e->pnode) continue;
          Vec<Var *> ctracked;
          for (int k = 0; k < e->args.n; k++) {
            if (!e->args[k].key || !e->args[k].value) continue;
            Var *fv = es->fun->args.get(e->args[k].key);
            if (fv && tracked.set_in(fv) && e->args[k].value->var) ctracked.add(e->args[k].value->var);
          }
          for (Var *l : e->pnode->lvals) if (l && returns_obj && *returns_obj) ctracked.add(l);
          bool dummy_exits = false, dummy_ret = false;
          for (PNode *s : e->pnode->cfg_succ) {
            walk(e->from, s, ctracked, &dummy_exits, &dummy_ret, true);
            if (observed) return;
          }
        }
        continue;
      }
      for (PNode *s : n->cfg_succ) if (s && seen.set_add(s)) work.add(s);
    }
  }
};
}  // namespace

// Is `p` (in contour es) a store of None into a field? Fills `w` with the
// field and the object's CreationSets.
static bool none_store(PNode *p, EntrySet *es, OwWalk &w) {
  if (!p->prim || p->prim->index != P_prim_setter || p->rvals.n < 5 || !p->rvals[3]) return false;
  AVar *val = make_AVar(p->rvals[4], es);
  if (!val || !val->out || val->out->sorted.n != 1 || val->out->sorted[0]->sym->type != sym_nil_type) return false;
  AVar *o = make_AVar(p->rvals[1], es);
  if (!o || !o->out) return false;
  w.field = p->rvals[3]->sym;
  for (CreationSet *c : o->out->sorted) if (c) w.css.set_add(c);
  return w.field && w.css.n;
}

// Does the field, on the object's CreationSets, also hold a number? That is
// the irrepresentable {None, scalar} union this exists for, and the filter
// that keeps the walk off every other None store (a pointer field holds None
// as null and needs nothing).
static bool field_holds_scalar(OwWalk &w) {
  for (CreationSet *c : w.css) {
    if (!c || !w.field->name) continue;
    AVar *fv = c->var_map.get(w.field->name);
    if (fv && fv->out)
      for (CreationSet *t : fv->out->sorted)
        if (t && t->sym && t->sym->type && t->sym->type->num_kind) return true;
  }
  return false;
}

static void run_walk(PNode *p, EntrySet *es, OwWalk &w) {
  Vec<Var *> tracked;
  tracked.add(p->rvals[1]);
  bool exits = false, ret = false;
  for (PNode *s : p->cfg_succ) {
    w.walk(es, s, tracked, &exits, &ret, true);
    if (w.observed) break;
  }
}

// ---- the decision -------------------------------------------------------

static Vec<PNode *> overwritten;  // the ledger: stores that do not flow

static bool overwrite_enabled() {
  static int on = -1;
  if (on < 0) on = getenv("PYC_OVERWRITE") ? atoi(getenv("PYC_OVERWRITE")) : 1;
  return on != 0;
}

bool store_is_overwritten(PNode *p) { return overwrite_enabled() && overwritten.set_in(p); }

// At a fixed point: record every None store whose field also holds a scalar
// and that no contour of its function can read before an overwrite, and
// re-walk every store already recorded. Returns 1 if the ledger grew (the
// caller runs another pass). A recorded store that has become observable is
// the issue's stop condition: the walk's premise is wrong, so it fails loudly
// rather than being quietly withdrawn.
int decide_overwritten_stores() {
  if (!overwrite_enabled()) return 0;
  static int dbg = getenv("IFA_DBG_OVERWRITE") ? 1 : 0;
  int grew = 0;
  for (Fun *f : fa->funs) {
    if (!f) continue;
    for (PNode *p : f->fa_send_PNodes) {
      bool recorded = overwritten.set_in(p) != nullptr;
      bool any = false, all_never = true, scalar = false;
      PNode *obs = nullptr;
      for (EntrySet *es : f->ess) {
        OwWalk w;
        if (!none_store(p, es, w)) continue;
        if (!recorded && !field_holds_scalar(w)) continue;
        scalar = scalar || field_holds_scalar(w);
        any = true;
        run_walk(p, es, w);
        if (w.observed) { all_never = false; obs = w.obs_at; break; }
      }
      if (!any) continue;
      if (recorded) {
        if (!all_never)
          fail("ifa/191: a None store recorded as never read (%s line %d) is now observable at node %d",
               f->sym && f->sym->name ? f->sym->name : "?", p->code ? p->code->line() : -1, obs ? obs->id : -1);
        continue;
      }
      if (all_never && scalar) {
        overwritten.set_add(p);
        grew = 1;
        if (dbg)
          fprintf(stderr, "[overwrite] pass=%d RECORD fun=%s line=%d field=%s\n", analysis_pass,
                  f->sym && f->sym->name ? f->sym->name : "?", p->code ? p->code->line() : -1,
                  p->rvals[3]->sym && p->rvals[3]->sym->name ? p->rvals[3]->sym->name : "?");
      }
    }
  }
  return grew;
}

// ---- the probe ----------------------------------------------------------

void report_overwrite_probe() {
  static int want = getenv("IFA_DBG_OVERWRITE") ? 1 : 0;
  if (!want) return;
  for (Fun *f : fa->funs) {
    if (!f) continue;
    for (PNode *p : f->fa_send_PNodes) {
      for (EntrySet *es : f->ess) {
        OwWalk w;
        if (!none_store(p, es, w)) continue;
        run_walk(p, es, w);
        fprintf(stderr, "[overwrite] pass=%d %s%s%s fun=%s es=%d field=%s scalar=%d", analysis_pass,
                w.out_of_budget ? "BUDGET" : (w.observed ? "OBSERVED" : "NEVER"), w.raise ? " RAISE" : "",
                overwritten.set_in(p) ? " RECORDED" : "", f->sym && f->sym->name ? f->sym->name : "?", es->id,
                w.field->name ? w.field->name : "?", field_holds_scalar(w) ? 1 : 0);
        if (w.observed && !w.out_of_budget && w.obs_at)
          fprintf(stderr, " at=p%d prim=%s fun=%s line=%d", w.obs_at->id,
                  w.obs_at->prim && w.obs_at->prim->name ? w.obs_at->prim->name : "send",
                  w.obs_es && w.obs_es->fun->sym && w.obs_es->fun->sym->name ? w.obs_es->fun->sym->name : "?",
                  w.obs_at->code ? w.obs_at->code->line() : -1);
        fprintf(stderr, "\n");
      }
    }
  }
}
