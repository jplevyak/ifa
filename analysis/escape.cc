// SPDX-License-Identifier: BSD-3-Clause
// IFA escape analysis — Phases 2-4.  See ESCAPE_PLAN.md for
// the lattice, transfer functions, and the multi-phase
// rollout that consumes the result.
//
// Scope:
// - Phase 2 (intra-procedural): per-EntrySet fixed point over
//   the per-AVar escape lattice with transfer rules for
//   Code_MOVE, prim_period, prim_set_period, prim_reply,
//   prim_make / new / clone.  Fresh allocations seed at
//   ES_NoEscape; use sites pull values up to ES_Escape.
// - Phase 3 (inter-procedural): at each non-prim Code_SEND,
//   look up the callee set via Fun::calls and consult each
//   callee's arg_escapes to decide whether each arg position
//   propagates Escape into the caller.  Outer fixed point
//   re-runs the per-EntrySet pass until no Fun::arg_escapes
//   flips.
//
// The pass is a no-op when ifa_escape_in_fa is off — codegen
// then falls back to the Stage 3 post-IFA analysis.

#include "ast.h"
#include "code.h"
#include "escape.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "ifadefs.h"
#include "prim.h"
#include "prim_data.h"
#include "sym.h"
#include "var.h"

namespace {

// Get the per-EntrySet AVar for `v`, but only if it makes
// sense to ask: locals/internals live in their EntrySet's
// contour; globals live in GLOBAL_CONTOUR and are always
// treated as escaping (so we skip them).
//
// Returns nullptr for vars we don't track (constants,
// globals).
AVar *avar_for(Var *v, EntrySet *es) {
  if (!v || !v->sym) return nullptr;
  if (v->sym->is_constant) return nullptr;
  // make_AVar routes nesting/global vars to the right contour
  // already (see fa.cc:201).  We only want the per-EntrySet
  // local view here; globals don't have meaningful per-fn
  // escape status (they always escape).
  if (!v->is_internal && !v->sym->nesting_depth) {
    return nullptr;  // global — treat as escape (default top)
  }
  AVar *av = v->avars.get(es);
  return av;
}

// True iff this PNode is a SEND that names a primitive.
bool is_prim_send(PNode *p) {
  return p && p->code && p->code->kind == Code_SEND && p->prim;
}

// True iff this PNode is a fresh-allocation primitive whose
// lval starts non-escaping.
bool is_fresh_alloc(PNode *p) {
  if (!is_prim_send(p)) return false;
  switch (p->prim->index) {
    case P_prim_make:
    case P_prim_new:
    case P_prim_clone:
    case P_prim_clone_vector:
      return true;
    default:
      return false;
  }
}

// Look up the set of possible callee Funs at PNode `p` in
// EntrySet `es`.  Uses `es->out_edge_map` so we work both
// pre-clone (when Fun::calls hasn't been built) and post-
// clone.  Returns whether at least one callee was found.
bool callees_at(EntrySet *es, PNode *p, Vec<Fun *> &out) {
  if (!es) return false;
  Vec<AEdge *> *edges = es->out_edge_map.get(p);
  if (!edges) return false;
  for (AEdge *ee : *edges) {
    if (!ee || !ee->to || !ee->to->fun) continue;
    out.set_add(ee->to->fun);
  }
  return out.n > 0;
}

// True iff the arg at position `formal_idx` is known to NOT
// escape its callees.  Conservative: any callee with
// unpopulated arg_escapes, or any callee that says Escape at
// this position, returns false.
bool arg_safe_across_callees(Vec<Fun *> &cs, int formal_idx) {
  if (cs.n == 0) return false;
  for (Fun *callee : cs) {
    if (!callee) return false;
    if (callee->arg_escapes.n == 0) return false;  // no info
    if (formal_idx < 0 || formal_idx >= callee->arg_escapes.n)
      return false;  // shape mismatch — be safe
    if (callee->arg_escapes[formal_idx] != ES_NoEscape) return false;
  }
  return true;
}

// Apply the transfer function for `p` to the current
// per-AVar lattice within `es`.  Returns true iff any AVar's
// escape bit changed.
bool transfer(PNode *p, EntrySet *es) {
  if (!p || !p->live || !p->code) return false;
  bool changed = false;
  auto join_av = [&](AVar *dst, EscapeStatus s) {
    if (!dst) return;
    EscapeStatus cur = (EscapeStatus)dst->escape;
    EscapeStatus joined = join_escape(cur, s);
    if (joined != cur) {
      dst->escape = joined;
      changed = true;
    }
  };
  auto get_es = [&](Var *v) -> EscapeStatus {
    AVar *av = avar_for(v, es);
    if (!av) return ES_Escape;  // global/constant — top
    return (EscapeStatus)av->escape;
  };

  switch (p->code->kind) {
    case Code_MOVE: {
      // lval := rval — escape transitive through aliasing.
      // Also, if lval is global (no AVar in this contour),
      // the rval escapes (rval flows into the global slot).
      if (p->lvals.n < 1 || p->rvals.n < 1) break;
      AVar *lv = avar_for(p->lvals[0], es);
      EscapeStatus rs = get_es(p->rvals[0]);
      if (!lv) {
        // lval is a global / unmodelled slot — source escapes.
        AVar *rv = avar_for(p->rvals[0], es);
        join_av(rv, ES_Escape);
      } else {
        join_av(lv, rs);
      }
      break;
    }
    case Code_SEND: {
      Prim *pr = p->prim;
      if (!pr) {
        // Non-primitive SEND — concrete or closure-dispatch
        // call.  Phase 3: consult each callee's arg_escapes
        // (via es->out_edge_map).  rvals[0] is the function
        // ref; rvals[1..] are user args mapping 1:1 to the
        // callee's positional formals.
        //
        // If a callee's arg_escapes is empty (not yet
        // analyzed) or marks that slot as escaping, the
        // caller's rval at that position is forced to
        // Escape.  Otherwise it can stay non-escaping.
        Vec<Fun *> cs;
        callees_at(es, p, cs);
        // rvals[0] is the function reference itself; for
        // closure dispatch this is the closure ptr.  It
        // doesn't escape the caller just by being called
        // (the call site reads it locally), but if the
        // closure captures any non-escaping locals, those
        // captured-locals' escape status was already
        // pulled up by the closure-creation code path.
        // Don't touch rvals[0] here.
        for (int i = 1; i < p->rvals.n; i++) {
          int formal_idx = i - 1;
          bool safe = arg_safe_across_callees(cs, formal_idx);
          if (!safe) {
            join_av(avar_for(p->rvals[i], es), ES_Escape);
          }
        }
        break;
      }
      switch (pr->index) {
        case P_prim_period:
        case P_prim_index_object: {
          // lval := self.field — the loaded value aliases
          // whatever `self` aliases, so lval inherits self's
          // escape status.
          if (p->lvals.n < 1 || p->rvals.n < 2) break;
          AVar *lv = avar_for(p->lvals[0], es);
          // rvals = [primitive_marker, self, field_name?]
          // Walk back from end for the self ref; conservative
          // choice: join all non-constant rvals' escape into lval.
          EscapeStatus s = ES_NoEscape;
          for (Var *rv : p->rvals) s = join_escape(s, get_es(rv));
          join_av(lv, s);
          break;
        }
        case P_prim_setter:
        case P_prim_set_index_object: {
          // self.field := value — if self escapes (or is a
          // global slot we don't track), value escapes too.
          // rvals layout: [marker, self, ..., value]
          if (p->rvals.n < 3) break;
          Var *self_v = p->rvals[1];
          Var *value_v = p->rvals[p->rvals.n - 1];
          EscapeStatus self_es = get_es(self_v);
          join_av(avar_for(value_v, es), self_es);
          break;
        }
        case P_prim_reply: {
          // Return value escapes (caller observes it).
          // rvals[reply.n-1] is the value being returned.
          if (p->rvals.n < 1) break;
          Var *ret_v = p->rvals[p->rvals.n - 1];
          join_av(avar_for(ret_v, es), ES_Escape);
          break;
        }
        case P_prim_make:
        case P_prim_new:
        case P_prim_clone:
        case P_prim_clone_vector: {
          // Fresh allocation — lval starts at ES_NoEscape
          // (the seed; uses pull it up).  Nothing to do here
          // for the join — the seed was set in seed_lattice().
          break;
        }
        default:
          // Other prims (arithmetic, comparison, etc.) —
          // result is a value, no escape propagation.  But
          // some prims pass references (e.g. __pyc_c_call__
          // delegates to C); be conservative and escape every
          // ptr arg.  Phase 2 takes the simpler route: only
          // escape the lval if any arg escapes (carries
          // through), and don't perturb arg escapes.
          if (p->lvals.n >= 1) {
            EscapeStatus s = ES_NoEscape;
            for (Var *rv : p->rvals) s = join_escape(s, get_es(rv));
            join_av(avar_for(p->lvals[0], es), s);
          }
          break;
      }
      break;
    }
    case Code_IF:
    case Code_GOTO:
    case Code_LABEL:
    case Code_SEQ:
    case Code_CONC:
    case Code_NOP:
    case Code_SUB:
      // No data flow that affects escape.
      break;
  }
  return changed;
}

// Seed the per-AVar escape lattice for `es`.
//
// Lattice direction: bottom = NoEscape (we want to prove
// values can stay this way), top = Escape (forced by use
// sites).  The seed starts every per-EntrySet AVar at
// ES_NoEscape (optimistic bottom).  The transfer at each use
// site then pulls values UP to Escape as needed.  At fixed
// point the survivors are provably non-escaping.
//
// AVar's ctor initializes escape=ES_Escape (conservative top)
// for the no-analysis path; here we override to ES_NoEscape
// before applying transfer to drive the analysis to a useful
// answer.
//
// Walks the Fun's fa_all_PNodes (post-clone-stable, unlike
// es->live_pnodes which may carry pre-clone pointers needing
// nmap remap — see clone.cc:fixup_clone_ess).  Filters by
// `p->fa_live` so we only seed for the analysis-live subset.
void seed_lattice(EntrySet *es) {
  for (PNode *p : es->fun->fa_all_PNodes) {
    if (!p) continue;
    for (Var *v : p->lvals) {
      AVar *av = avar_for(v, es);
      if (av) av->escape = ES_NoEscape;
    }
    for (Var *v : p->rvals) {
      AVar *av = avar_for(v, es);
      if (av) av->escape = ES_NoEscape;
    }
  }
  // Seed formal args too — they live in es->args and need to
  // be candidates for non-escape.
  form_Map(MapMPositionAVarElem, x, es->args) {
    if (x->value) x->value->escape = ES_NoEscape;
  }
}

// Per-EntrySet flow-insensitive fixed point.  `seed` is true
// only on the first analysis pass — re-runs (from the outer
// inter-procedural loop) preserve the existing per-AVar
// state so monotonic refinement holds.
void analyze_es(EntrySet *es, bool seed) {
  if (!es || !es->fun) return;
  if (seed) seed_lattice(es);
  const int max_iters = 64;
  for (int iter = 0; iter < max_iters; iter++) {
    bool changed = false;
    for (PNode *p : es->fun->fa_all_PNodes) {
      if (!p) continue;
      if (transfer(p, es)) changed = true;
    }
    if (!changed) break;
  }
}

// Walk fa->ess and group by EntrySet::fun.  Pre-clone the
// Fun::ess vector hasn't been populated yet (clone's
// fixup_clone_ess does that), so we group on the fly.
void group_ess_by_fun(FA *fa, Map<Fun *, Vec<EntrySet *> *> &out) {
  for (EntrySet *es : fa->ess) {
    if (!es || !es->fun) continue;
    Vec<EntrySet *> *v = out.get(es->fun);
    if (!v) { v = new Vec<EntrySet *>(); out.put(es->fun, v); }
    v->add(es);
  }
}

// Project the per-EntrySet AVar escape onto Fun::arg_escapes.
//
// For multiple EntrySets per Fun (per-context specialization),
// take the JOIN: an arg escapes from the Fun's perspective if
// ANY context's formal escapes.  Phase 4's cloning will split
// these back out per context.
void project_to_fun(Fun *f, Vec<EntrySet *> *ess) {
  if (!f || f->positional_arg_positions.n == 0) return;
  if (!ess || ess->n == 0) return;
  Vec<uint8_t> escapes;
  for (int i = 0; i < f->positional_arg_positions.n; i++) {
    escapes.add(ES_NoEscape);
  }
  for (EntrySet *es : *ess) {
    if (!es) continue;
    for (int i = 0; i < f->positional_arg_positions.n; i++) {
      MPosition *p = f->positional_arg_positions[i];
      AVar *av = es->args.get(p);
      if (!av) continue;
      if (av->escape == ES_Escape) escapes[i] = (uint8_t)ES_Escape;
    }
  }
  f->arg_escapes.clear();
  for (uint8_t e : escapes) f->arg_escapes.add(e);
}

}  // namespace

// Returns true iff `a` and `b` have the same per-formal
// escape signature.  Used to detect arg_escapes flips between
// inter-procedural iterations.
bool same_escapes(const Vec<uint8_t> &a, const Vec<uint8_t> &b) {
  if (a.n != b.n) return false;
  for (int i = 0; i < a.n; i++) if (a[i] != b[i]) return false;
  return true;
}

void compute_escape(FA *fa) {
  if (!fa) return;
  // Phase 6: always run.  Codegen relies on per-AVar escape
  // being populated to set CGv2Value::escapes during
  // lowering — there's no longer a fallback path.  The
  // ifa_escape_in_fa flag is preserved for compatibility
  // with tests that still consult it, but no production
  // code path gates on it anymore.

  // Group EntrySets by Fun once.  Pre-clone, Fun::ess is
  // empty (clone's fixup_clone_ess populates it later); we
  // build the same mapping locally from fa->ess.
  Map<Fun *, Vec<EntrySet *> *> ess_by_fun;
  group_ess_by_fun(fa, ess_by_fun);

  // Phase 3 outer loop: iterate until no Fun::arg_escapes
  // signature changes.  In practice settles in 1-3 passes
  // because the lattice is monotonic and depths are small.
  const int max_outer = 8;
  int outer_passes = 0;
  bool changed = true;
  while (changed && outer_passes < max_outer) {
    changed = false;
    outer_passes++;
    for (Fun *f : fa->funs) {
      if (!f) continue;
      // Pre-clone `Fun::live` is not yet set (mark_live_funs
      // runs later); filter on having EntrySets instead.
      Vec<EntrySet *> *ess = ess_by_fun.get(f);
      if (!ess || ess->n == 0) continue;
      // Remember the pre-pass per-formal signature so we can
      // detect a flip after re-analyze.
      Vec<uint8_t> prev;
      prev.copy(f->arg_escapes);
      for (EntrySet *es : *ess) {
        if (!es) continue;
        analyze_es(es, /*seed=*/outer_passes == 1);
      }
      project_to_fun(f, ess);
      if (!same_escapes(prev, f->arg_escapes)) changed = true;
    }
  }

  int funs_analyzed = 0;
  int total_formals = 0;
  int formals_no_escape = 0;
  for (Fun *f : fa->funs) {
    if (!f) continue;
    if (f->arg_escapes.n > 0) {
      funs_analyzed++;
      for (uint8_t e : f->arg_escapes) {
        total_formals++;
        if (e == ES_NoEscape) formals_no_escape++;
      }
    }
  }
  if (ifa_verbose > 0) {
    fprintf(stderr,
            "[escape] Phases 2-4: %d outer passes, analyzed "
            "%d funs, %d/%d formals NoEscape\n",
            outer_passes, funs_analyzed, formals_no_escape,
            total_formals);
  }
}
