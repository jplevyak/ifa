#include "ifadefs.h"

#include "ifa.h"

#include "ssu.h"
#include "dom.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "ifalog.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

static void print_pnode(PNode *n, cchar *s) {
  printf("  %s: RVALS ", s);
  for (Var *v : n->rvals) {
    printf("%d:", v->id);
    if1_dump_sym(stdout, v->sym);
  }
  printf(" LVALS ");
  for (Var *v : n->lvals) {
    printf("%d:", v->id);
    if1_dump_sym(stdout, v->sym);
  }
  printf("\n");
}

static void print_renamed(Fun *f) {
  Vec<PNode *> nodes;
  f->collect_PNodes(nodes);
  for (PNode *n : nodes) {
    for (PNode *p : n->phi) print_pnode(p, "phi");
    if1_dump_code(stdout, n->code, 0);
    if (n->rvals.n || n->lvals.n) print_pnode(n, "node");
    for (PNode *p : n->phy) print_pnode(p, "phy");
  }
}

static void print_ssu(Fun *f, Vec<PNode *> nodes) {
  if (ifa_verbose > 2) {
    int phi = 0, phy = 0;
    for (PNode *n : nodes) {
      phi += n->phi.n;
      phy += n->phy.n;
    }
    printf("%d phi nodes\n", phi);
    printf("%d phy nodes\n", phy);
  }
  if (ifa_verbose > 3) print_renamed(f);
}

static inline PNode *new_phiphy(PNode *n, Var *v, int phy) {
  PNode *p = new PNode();
  p->rvals.add(v);
  if (phy)
    p->lvals.fill(n->cfg_succ.n);
  else {
    p->lvals.fill(1);
    p->rvals.fill(n->cfg_pred.n);
  }
  return p;
}

typedef Env<Var *, Var *> VarEnv;
typedef Vec<PNode **> EdgeSet;

static inline int maybe_live(PNode *n, Var *v) { return n->live_vars->get(v) != 0; }

static int merge_live(PNode *n, PNode *p) {
  int changed = 0;
  // Issue 035 root cause: this was `changed = !put(v)` — plain
  // assignment — so only the LAST var iterated out of p->live_vars
  // decided whether the fixpoint loop kept going, and live_vars'
  // raw-pointer bucket order chose which var that was. Under some
  // heap layouts the loop terminated with liveness incomplete,
  // placing fewer phys (fun-dependent), shifting every subsequent
  // PNode/Var id, and cascading into run-to-run different clone
  // sets and generated code.
  for (Var *v : *p->live_vars) if (v) if (!n->lvals.in(v)) changed |= !n->live_vars->put(v);
  return changed;
}

static void approximate_liveness(Fun *f, Vec<PNode *> &nodes) {
  int changed = 1;
  for (PNode *n : nodes) n->live_vars = new BlockHash<Var *, VarIdHashFns>;
  while (changed) {
    changed = 0;
    for (PNode *n : nodes) {
      for (PNode *p : n->cfg_succ) changed |= merge_live(n, p);
      for (Var *v : n->rvals) if (v->sym->is_local) changed |= !n->live_vars->put(v);
    }
  }
}

// ifa/issues/039: definite assignment -- mark every local with a USE
// reachable by a path that does not assign it. CPython raises
// UnboundLocalError for exactly this; pyc reads stack garbage silently
// on both backends.
//
// DEFAULT OFF (IFA_UNBOUND=1 to enable). The fixed point below does not
// converge on some functions; see "Where it stands" in the issue. It is
// hard-bounded so an enabled run cannot hang, and exhausting the bound
// abandons the result rather than reporting a wrong one.
//
// Why a separate pass, and not the hook the issue originally proposed.
// `get_Var`'s "no entry in the renaming environment" fallback looks like
// the right signal for "no reaching definition" and is not, in BOTH
// directions -- measured on the issue's own four-line repro:
//
//   * it FIRES for bound values: 77 hits, including `self`, `item` and
//     `key`, which are formal parameters and always bound. The env is
//     scoped, so a formal's binding is not visible at every point a phi
//     argument is resolved.
//   * it does NOT fire for the actual unbound local. `y`'s phi got
//     env_hit=1 on BOTH predecessor edges, because the push/pop is
//     conditional (`cfg_succ.n != 1 && cfg_pred.n != 1`) -- so a plain
//     `if` with no `else` leaks the assigning branch's renaming onto the
//     non-assigning edge.
//
// The fact is therefore not recoverable from renaming state at all. It
// is an ordinary forward MUST analysis:
//
//     assigned_out[n] = assigned_in[n] u lvals(n)
//     assigned_in[n]  = INTERSECTION over preds of assigned_out
//     assigned_in[entry] = {}     (formals arrive as entry's own lvals)
//
// initialised optimistically (every non-entry node starts with every
// local assigned) and shrunk, the standard way to get the greatest
// solution.
// Reverse postorder over the CFG, for the forward analyses below. A
// forward analysis visits every predecessor before its successors in
// RPO, so one sweep propagates a change the whole length of an acyclic
// path instead of one edge per sweep. Sweeping in collection order
// instead never reached a fixed point on ~42 builtin functions: a bit
// travelled around a cycle forever (measured on __pyc_tolist__ -- node
// 275 lost bit 8, 273 gained it, then 275 gained it back, at constant
// total popcount).
static void compute_rpo(Fun *f, Vec<PNode *> &nodes, Vec<PNode *> &rpo) {
  Vec<PNode *> stack, post, seen;
  Vec<int> next_child;
  stack.add(f->entry);
  next_child.add(0);
  seen.set_add(f->entry);
  while (stack.n) {
    PNode *n = stack[stack.n - 1];
    int &ci = next_child[next_child.n - 1];
    if (ci < n->cfg_succ.n) {
      PNode *c = n->cfg_succ[ci++];
      if (c && seen.set_add(c)) { stack.add(c); next_child.add(0); }
    } else {
      post.add(n);
      stack.n--;
      next_child.n--;
    }
  }
  for (int i = post.n - 1; i >= 0; i--) rpo.add(post[i]);
  // anything unreachable from entry still needs a slot in the sweep
  for (PNode *n : nodes) if (!seen.set_in(n)) rpo.add(n);
}

static const int kUnboundMaxSweeps = 200;

static void find_maybe_unbound(Fun *f, Vec<PNode *> &nodes, Vec<Var *> &locals) {
  // The `safe` environment cannot auto-initialize what was never
  // identified, so requesting it turns the analysis on regardless.
  static int enabled = -1;
  if (enabled < 0) enabled = getenv("IFA_UNBOUND") ? 1 : 0;
  if (!enabled && !fauto_init_unbound) return;
  if (!f->entry || !locals.n || !nodes.n) return;

  // Dense bit indices: a Vec-of-Var set with a linear set_in inside the
  // fixed point did not finish on the builtin library.
  Map<Var *, int> idx;
  for (int i = 0; i < locals.n; i++) idx.put(locals[i], i + 1);  // 1-based; get() gives 0 for absent
  int nbits = locals.n, nwords = (nbits + 63) / 64;
  // TWO analyses over the same CFG, differing only in the meet
  // (ifa/issues/039, design 2026-08-25):
  //   MUST (intersection, initialised to top) -> "definitely assigned"
  //   MAY  (union, initialised to bottom)     -> "assigned on some path"
  // from which
  //   definitely_unbound = !may_assigned
  //   possibly_unbound   = may_assigned && !must_assigned
  // Definitely is a compile error in EVERY environment -- no path makes
  // the program correct, so refusing costs nothing. Possibly is a
  // strict-mode warning and otherwise a runtime check, or an
  // auto-initialisation under `safe`.
  Map<PNode *, uint64_t *> outmap, mayout;
  for (PNode *n : nodes) {
    uint64_t *w = (uint64_t *)MALLOC(nwords * sizeof(uint64_t));
    uint64_t fill = (n == f->entry) ? 0 : ~(uint64_t)0;
    for (int i = 0; i < nwords; i++) w[i] = fill;
    outmap.put(n, w);
    uint64_t *m = (uint64_t *)MALLOC(nwords * sizeof(uint64_t));
    for (int i = 0; i < nwords; i++) m[i] = 0;
    mayout.put(n, m);
  }

  // Iterate in REVERSE POSTORDER. The first cut swept `nodes` in
  // collection order with in-place updates, and never reached a fixed
  // point on ~42 builtin functions: a bit travelled around a cycle
  // forever (measured on __pyc_tolist__ -- node 275 lost bit 8, 273
  // gained it, then 275 gained it back, at constant total popcount).
  // A forward analysis visits every predecessor before its successors
  // in RPO, so one sweep propagates a change the whole length of an
  // acyclic path instead of one edge per sweep.
  Vec<PNode *> rpo;
  compute_rpo(f, nodes, rpo);

  // Formals are bound on entry BY DEFINITION, and must be seeded --
  // they do NOT arrive as the entry node's own lvals, and assuming they
  // did made the analysis report essentially every parameter in the
  // builtin library as possibly unbound (485 hits in ___init___ alone,
  // plus self, x, l, t ...).
  //
  // Read from f->sym->has, NOT Var::is_formal: that bit is set by
  // build_patterns, which runs inside FA, long after build_ssu -- so at
  // this point it is always 0 and seeding from it seeds nothing.
  Vec<Var *> formals;
  for (Sym *a : f->sym->has) if (a && a->var) formals.set_add(a->var);

  uint64_t *in = (uint64_t *)MALLOC(nwords * sizeof(uint64_t));
  uint64_t *mayin = (uint64_t *)MALLOC(nwords * sizeof(uint64_t));
  bool changed = true;
  int sweeps = 0;
  while (changed) {
    changed = false;
    // Bail out rather than spin. Nothing has been flagged yet -- the
    // flagging loop runs after this one -- so an unconverged sweep
    // simply reports nothing, which is the safe direction.
    if (++sweeps > kUnboundMaxSweeps) return;
    for (PNode *n : rpo) {
      bool first = true;
      for (int i = 0; i < nwords; i++) in[i] = 0;
      if (n == f->entry) {
        // entry's IN is the formals, not the empty set
        for (Var *v : formals) {
          int b = idx.get(v);
          if (b) { --b; in[b >> 6] |= ((uint64_t)1 << (b & 63)); }
        }
      }
      if (n != f->entry) {
        for (PNode *p : n->cfg_pred) {
          if (!p) continue;
          uint64_t *po = outmap.get(p);
          if (!po) continue;
          if (first) {
            for (int i = 0; i < nwords; i++) in[i] = po[i];
            first = false;
          } else
            for (int i = 0; i < nwords; i++) in[i] &= po[i];
        }
      }
      for (Var *v : n->lvals) if (v && v->sym->is_local) {
        int b = idx.get(v);
        if (b) { --b; in[b >> 6] |= ((uint64_t)1 << (b & 63)); }
      }
      uint64_t *cur = outmap.get(n);
      for (int i = 0; i < nwords; i++)
        if (in[i] != cur[i]) {
          cur[i] = in[i];
          changed = true;
        }

      // MAY, same node, UNION meet (grows from bottom).
      for (int i = 0; i < nwords; i++) mayin[i] = 0;
      if (n == f->entry) {
        for (Var *v : formals) {
          int b = idx.get(v);
          if (b) { --b; mayin[b >> 6] |= ((uint64_t)1 << (b & 63)); }
        }
      } else
        for (PNode *p : n->cfg_pred) {
          uint64_t *po = mayout.get(p);
          if (po) for (int i = 0; i < nwords; i++) mayin[i] |= po[i];
        }
      for (Var *v : n->lvals) if (v && v->sym->is_local) {
        int b = idx.get(v);
        if (b) { --b; mayin[b >> 6] |= ((uint64_t)1 << (b & 63)); }
      }
      uint64_t *mcur = mayout.get(n);
      for (int i = 0; i < nwords; i++)
        if (mayin[i] != mcur[i]) { mcur[i] = mayin[i]; changed = true; }
    }
  }

  for (PNode *n : nodes) {
    bool first = true;
    for (int i = 0; i < nwords; i++) in[i] = 0;
    for (int i = 0; i < nwords; i++) mayin[i] = 0;
    if (n == f->entry)
      for (Var *v : formals) {
        int b = idx.get(v);
        if (b) { --b; in[b >> 6] |= ((uint64_t)1 << (b & 63)); mayin[b >> 6] |= ((uint64_t)1 << (b & 63)); }
      }
    if (n != f->entry)
      for (PNode *p : n->cfg_pred) {
        uint64_t *po = mayout.get(p);
        if (po) for (int i = 0; i < nwords; i++) mayin[i] |= po[i];
      }
    if (n != f->entry) {
      for (PNode *p : n->cfg_pred) {
        uint64_t *po = outmap.get(p);
        if (!po) continue;
        if (first) { for (int i = 0; i < nwords; i++) in[i] = po[i]; first = false; }
        else for (int i = 0; i < nwords; i++) in[i] &= po[i];
      }
    }
    for (Var *v : n->rvals) if (v && v->sym->is_local) {
      int b = idx.get(v);
      if (!b) continue;
      --b;
      bool must = (in[b >> 6] & ((uint64_t)1 << (b & 63))) != 0;
      bool may = (mayin[b >> 6] & ((uint64_t)1 << (b & 63))) != 0;
      if (must) continue;              // definitely assigned: fine
      v->sym->maybe_unbound = 1;
      if (!may) v->sym->definitely_unbound = 1;  // no path assigns it at all
    }
  }
}

// ifa/issues/039, the `safe` environment. Give the "control got here
// without ever assigning it" edge of a phi a value of its own, so the
// analysis stops treating that path as unreachable.
//
// This is what the codegen-level `T x = {};` cannot do by itself. FA
// sees an unassigned path contribute *bottom*, so for
//
//     def f(c):
//         if c: y = 42
//         return y
//
// the union reaching `return y` is the single constant {42}: the whole
// function folds to a literal and there is no slot left to initialize.
// f(0) returned 42. Marking the operand here is what keeps the merge
// alive long enough for FA to give it a type -- the typed zero itself
// is substituted later, by the repair in fa.cc, which is the first
// point at which the type is known.
//
// Runs after rename_vars, on the final Var objects, and keys on Sym:
// each SSU name is a distinct Var but "assigned" is a property of the
// original local. A fresh Var per operand (rather than a flag on the
// shared one) keeps the marking unambiguous -- `get_Var`'s env-miss
// fallback hands the SAME original Var to several operand slots.
//
// Note this cannot be read off the renaming state, which is why it is a
// second dataflow rather than a hook in rename_edge: measured on the
// repro, `get_Var`'s env miss fires for BOUND values (77 hits, incl.
// formals) and does NOT fire for the actual unbound local, because the
// push/pop is conditional (`cfg_succ.n != 1 && cfg_pred.n != 1`), so a
// plain `if` with no `else` leaks the assigning branch's renaming onto
// the non-assigning edge.
static void mark_unbound_phi_operands(Fun *f, Vec<PNode *> &nodes) {
  if (!fauto_init_unbound) return;
  if (!f->entry || !nodes.n) return;

  Map<Sym *, int> idx;  // 1-based; get() gives 0 for absent
  int nbits = 0;
  auto note = [&](Var *v) {
    if (v && v->sym && v->sym->is_local && !idx.get(v->sym)) idx.put(v->sym, ++nbits);
  };
  for (PNode *n : nodes) {
    for (Var *v : n->lvals) note(v);
    for (Var *v : n->rvals) note(v);
    for (PNode *p : n->phi) { for (Var *v : p->lvals) note(v); for (Var *v : p->rvals) note(v); }
  }
  if (!nbits) return;
  int nwords = (nbits + 63) / 64;

  Vec<PNode *> rpo;
  compute_rpo(f, nodes, rpo);

  Map<PNode *, uint64_t *> outmap;
  for (PNode *n : nodes) {
    uint64_t *w = (uint64_t *)MALLOC(nwords * sizeof(uint64_t));
    // optimistic init (everything assigned) everywhere but entry, then
    // shrink -- the standard way to get the greatest solution of a MUST
    // analysis.
    uint64_t fill = (n == f->entry) ? 0 : ~(uint64_t)0;
    for (int i = 0; i < nwords; i++) w[i] = fill;
    outmap.put(n, w);
  }

  Vec<Var *> formals;
  for (Sym *a : f->sym->has) if (a && a->var) formals.set_add(a->var);

  uint64_t *in = (uint64_t *)MALLOC(nwords * sizeof(uint64_t));
  bool changed = true;
  int sweeps = 0;
  while (changed) {
    changed = false;
    if (++sweeps > kUnboundMaxSweeps) return;  // abandon rather than spin
    for (PNode *n : rpo) {
      bool first = true;
      for (int i = 0; i < nwords; i++) in[i] = 0;
      if (n == f->entry) {
        // `formals` is a set-mode Vec: iterating it yields the hash
        // table's EMPTY SLOTS as nulls too, so this must guard before
        // dereferencing. (find_maybe_unbound's equivalent loop keys on
        // the Var itself, and Map::get tolerates a null key, which is
        // why it never showed the fault.)
        for (Var *v : formals) {
          if (!v) continue;
          int b = idx.get(v->sym);
          if (b) { --b; in[b >> 6] |= ((uint64_t)1 << (b & 63)); }
        }
      } else
        for (PNode *p : n->cfg_pred) {
          if (!p) continue;
          uint64_t *po = outmap.get(p);
          if (!po) continue;
          if (first) { for (int i = 0; i < nwords; i++) in[i] = po[i]; first = false; }
          else for (int i = 0; i < nwords; i++) in[i] &= po[i];
        }
      // A phi DEFINES the variable at the merge, so everything after it
      // is assigned. That is what concentrates unboundness onto the
      // operand edges instead of leaving it smeared over every later
      // read. phy lvals are deliberately NOT defs: a phy renames a value
      // across a branch split without giving it one, so counting it
      // would report an unassigned local as assigned.
      for (PNode *p : n->phi)
        for (Var *v : p->lvals) {
          int b = idx.get(v->sym);
          if (b) { --b; in[b >> 6] |= ((uint64_t)1 << (b & 63)); }
        }
      for (Var *v : n->lvals) {
        int b = idx.get(v->sym);
        if (b) { --b; in[b >> 6] |= ((uint64_t)1 << (b & 63)); }
      }
      uint64_t *cur = outmap.get(n);
      for (int i = 0; i < nwords; i++)
        if (in[i] != cur[i]) { cur[i] = in[i]; changed = true; }
    }
  }

  int marked = 0;
  for (PNode *n : nodes) {
    if (!n->phi.n) continue;
    for (PNode *ph : n->phi) {
      if (!ph->lvals.n || !ph->lvals[0]) continue;
      int b = idx.get(ph->lvals[0]->sym);
      if (!b) continue;
      --b;
      for (PNode *pred : n->cfg_pred) {
        if (!pred) continue;
        int di = n->cfg_pred_index.get(pred);
        if (di < 0 || di >= ph->rvals.n) continue;
        uint64_t *po = outmap.get(pred);
        if (po && (po[b >> 6] & ((uint64_t)1 << (b & 63)))) continue;  // assigned on this edge
        Var *nv = new Var(ph->lvals[0]->sym);
        nv->is_unbound_fill = 1;
        ph->rvals[di] = nv;
        ++marked;
      }
    }
  }
  if (marked && getenv("IFA_DBG_UNBOUND_FILL"))
    fprintf(stderr, "UNBOUND_FILL %s: %d operand(s)\n", f->sym->name ? f->sym->name : "?", marked);
}

static inline Var *new_Var(Var *v, VarEnv &e, Fun *f) {
  if (!v->sym->is_local) return v;
  Var *vv = new Var(v->sym);
  e.put(v, vv);
  return vv;
}

static inline Var *get_Var(Var *v, VarEnv &env, Fun *f) {
  if (!v->sym->is_local) return v;
  Var *vv = env.get(v);
  if (vv) return vv;
  return v;
}

static void rename_edge(Fun *f, PNode *d, VarEnv &env, Vec<PNode *> &nset) {
  for (PNode *p : d->phi) p->lvals[0] = new_Var(p->rvals[0]->sym->var, env, f);
  for (int i = 0; i < d->rvals.n; i++) d->rvals[i] = get_Var(d->rvals[i]->sym->var, env, f);
  for (int i = 0; i < d->lvals.n; i++) d->lvals[i] = new_Var(d->lvals[i]->sym->var, env, f);
  for (PNode *p : d->phy) p->rvals[0] = get_Var(p->rvals[0]->sym->var, env, f);
  for (int i = 0; i < d->cfg_succ.n; i++) {
    if (d->cfg_succ.n != 1 && d->cfg_pred.n != 1) env.push();
    PNode *dd = d->cfg_succ[i];
    int di = dd->cfg_pred_index.get(d);
    for (PNode *p : d->phy) p->lvals[i] = new_Var(p->rvals[0]->sym->var, env, f);
    for (PNode *p : dd->phi) p->rvals[di] = get_Var(p->rvals[0]->sym->var, env, f);
    if (nset.set_add(dd)) rename_edge(f, dd, env, nset);
    if (d->cfg_succ.n != 1 && d->cfg_pred.n != 1) env.pop();
  }
}

static void rename_vars(Fun *f, Vec<PNode *> nodes) {
  for (PNode *n : nodes) for (int i = 0; i < n->cfg_pred.n; i++) n->cfg_pred_index.put(n->cfg_pred[i], i);
  VarEnv env;
  env.push();
  Vec<PNode *> nset;
  nset.add(f->entry);
  rename_edge(f, f->entry, env, nset);
}

static int place_phi(Vec<Var *> vrs) {
  int changed = 0;
  for (Var *v : vrs) {
    Vec<PNode *> w;
    w.copy(v->ssu->defs);
    while (w.n) {
      PNode *n = w.pop();
      for (Dom *d : n->dom->front) {
        PNode *y = (PNode *)d->node;
        if (!v->ssu->phis.in(y) && maybe_live(y, v)) {
          changed = 1;
          y->phi.add(new_phiphy(y, v, 0));
          v->ssu->phis.add(y);
          v->ssu->uses.append(y->cfg_pred);
          if (!v->ssu->defs.in(y)) {
            v->ssu->defs.add(y);
            w.add(y);
          }
        }
      }
    }
  }
  return changed;
}

static int place_phy(Vec<Var *> vrs) {
  int changed = 0;
  for (Var *v : vrs) {
    Vec<PNode *> w;
    w.copy(v->ssu->uses);
    while (w.n) {
      PNode *n = w.pop();
      for (Dom *d : n->rdom->front) {
        PNode *y = (PNode *)d->node;
        if (!v->ssu->phys.in(y) && maybe_live(y, v)) {
          changed = 1;
          y->phy.add(new_phiphy(y, v, 1));
          v->ssu->phys.add(y);
          v->ssu->defs.append(y->cfg_succ);
          if (!v->ssu->uses.in(y)) {
            v->ssu->uses.add(y);
            w.add(y);
          }
        }
      }
    }
  }
  return changed;
}

void Fun::build_ssu() {
  if (!entry) return;
  build_cfg_dominators(this);
  Vec<Var *> vars, vrs;
  Vec<PNode *> pnodes;
  collect_Vars(vars, &pnodes);
  approximate_liveness(this, pnodes);
  for (Var *v : vars) {
    if (v->sym->is_local) {
      v->ssu = new SSUVar;
      vrs.add(v);
    }
  }
  for (PNode *p : pnodes) {
    for (Var *v : p->lvals) if (v->sym->is_local) v->ssu->defs.add(p);
    for (Var *v : p->rvals) if (v->sym->is_local) v->ssu->uses.add(p);
  }
  find_maybe_unbound(this, pnodes, vrs);  // ifa/issues/039 (default off)
  // Phi placement and phy placement feed each other: a
  // new phy creates new defs (its lvals at branch entries
  // — see `place_phy` line `defs.append(y->cfg_succ)`),
  // which give `place_phi` more work; a new phi creates
  // new uses (`place_phi` line `uses.append(y->cfg_pred)`),
  // which give `place_phy` more work.  Iterate until both
  // converge.
  //
  // The previous condition `while (phi && phy)` stopped
  // as soon as EITHER pass made no progress on its own
  // — which mis-handled the common shape where a var has
  // no real defs (read-only parameter like `self`).  For
  // those vars the initial phi pass places nothing, the
  // initial phy pass places branch splits, but the
  // cascading phi at the merge point — needed by codegen
  // to converge per-branch renames — was never placed.
  // Result: codegen emits per-branch SSU renames for
  // `self`, with no merge convergence, and post-if code
  // reads from an uninitialized branch-rename.  See
  // `ifa/issues/028-fibheap-blockers.md` Bug B.
  int phi = place_phi(vrs);
  int phy = place_phy(vrs);
  while (phi || phy) {
    phi = place_phi(vrs);
    phy = place_phy(vrs);
  }
  rename_vars(this, pnodes);
  for (PNode *n : pnodes) {
    for (PNode *p : n->phi) for (Var *v : p->lvals) v->def = n;
    for (Var *v : n->lvals) v->def = n;
    for (PNode *p : n->phy) for (Var *v : p->lvals) v->def = n;
  }
  mark_unbound_phi_operands(this, pnodes);  // ifa/issues/039 (--safe only)
  for (Var *v : vrs) v->ssu = 0;
  print_ssu(this, pnodes);
}
