// SPDX-License-Identifier: BSD-3-Clause
// IFA escape analysis — Phase 2 (intra-procedural).  See
// ESCAPE_PLAN.md for the lattice, transfer functions, and the
// multi-phase rollout that consumes the result.
//
// Phase 2 scope: per-EntrySet flow-insensitive fixed point
// over the per-AVar escape lattice.  Fresh allocations
// (prim_make / prim_new / prim_clone) start at ES_NoEscape;
// uses pull them up to ES_Escape.  Every concrete CALL is
// treated as escaping its args (Phase 3 reads the callee's
// arg_escapes to refine).  The per-formal projection is
// written back to Fun::arg_escapes for cg_normalize_v2's
// readback path.
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
        // call.  Phase 2: conservatively escape every arg.
        // (Phase 3 reads callee arg_escapes to refine.)
        for (Var *rv : p->rvals) join_av(avar_for(rv, es), ES_Escape);
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
    if (!p || !p->fa_live) continue;
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

// Per-EntrySet flow-insensitive fixed point.
void analyze_es(EntrySet *es) {
  if (!es || !es->fun) return;
  seed_lattice(es);
  const int max_iters = 64;
  for (int iter = 0; iter < max_iters; iter++) {
    bool changed = false;
    for (PNode *p : es->fun->fa_all_PNodes) {
      if (!p || !p->fa_live) continue;
      if (transfer(p, es)) changed = true;
    }
    if (!changed) break;
  }
}

// Project the per-EntrySet AVar escape onto Fun::arg_escapes.
//
// For multiple EntrySets per Fun (per-context specialization),
// take the JOIN: an arg escapes from the Fun's perspective if
// ANY context's formal escapes.  Phase 4's cloning will split
// these back out per context.
void project_to_fun(Fun *f) {
  if (!f || f->positional_arg_positions.n == 0) return;
  Vec<uint8_t> escapes;
  for (int i = 0; i < f->positional_arg_positions.n; i++) {
    escapes.add(ES_NoEscape);
  }
  bool any = false;
  for (EntrySet *es : f->ess) {
    if (!es) continue;
    any = true;
    for (int i = 0; i < f->positional_arg_positions.n; i++) {
      MPosition *p = f->positional_arg_positions[i];
      AVar *av = es->args.get(p);
      if (!av) continue;
      if (av->escape == ES_Escape) escapes[i] = (uint8_t)ES_Escape;
    }
  }
  if (!any) return;
  // Conservative default for un-analyzed Funs (no live
  // EntrySet) is to leave arg_escapes empty so the readback
  // in cg_normalize_v2 falls through to the Stage 3 scan.
  f->arg_escapes.clear();
  for (uint8_t e : escapes) f->arg_escapes.add(e);
}

}  // namespace

void compute_escape(FA *fa) {
  if (!ifa_escape_in_fa || !fa) return;
  int funs_analyzed = 0;
  int total_formals = 0;
  int formals_no_escape = 0;
  for (Fun *f : fa->funs) {
    if (!f || !f->live) continue;
    for (EntrySet *es : f->ess) {
      if (!es) continue;
      analyze_es(es);
    }
    project_to_fun(f);
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
            "[escape] Phase 2: analyzed %d funs, %d/%d formals "
            "marked NoEscape\n",
            funs_analyzed, formals_no_escape, total_formals);
  }
}
