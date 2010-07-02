/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved

  Static Single Use Form 
    - from the Illinois Concert Compiler circa 1992
    - new implementation circa 2008
  
  References
    Ron Cytron, Jeanne Ferrante, Barry K. Rosen, Mark N. Wegman, and F. Kenneth Zadeck. 
    Efficiently computing static single assignment form and the control dependence graph. 
    ACM Transactions on Programming Languages and Systems, 13(4):451-490, Oct 1991.

    Jeremy Singer, Static Program Analysis Based on Virtual Register Renaming, 
    University of Cambridge Technical report, UCAM-CL-TR-660, Feb 2006. 
*/

#include "ifadefs.h"
#include "ssu.h"
#include "fun.h"
#include "pnode.h"
#include "var.h"
#include "if1.h"
#include "fail.h"
#include "prim.h"
#include "dom.h"

static void
print_pnode(PNode *n, cchar *s) {
  printf("  %s: RVALS ", s);
  forv_Var(v, n->rvals) {
    printf("%d:", v->id);
    if1_dump_sym(stdout, v->sym);
  }
  printf(" LVALS ");
  forv_Var(v, n->lvals) {
    printf("%d:", v->id);
    if1_dump_sym(stdout, v->sym);
  }
  printf("\n");
}

static void
print_renamed(Fun *f) {
  Vec<PNode *> nodes;
  f->collect_PNodes(nodes);
  forv_PNode(n, nodes) {
    forv_PNode(p, n->phi) print_pnode(p, "phi");
    if1_dump_code(stdout, n->code, 0);
    if (n->rvals.n || n->lvals.n)
      print_pnode(n, "node");
    forv_PNode(p, n->phy) print_pnode(p, "phy");
  }
}

static void
print_ssu(Fun *f, Vec<PNode *> nodes) {
  if (ifa_verbose > 2) {
    int phi = 0, phy = 0;
    forv_PNode(n, nodes) {
      phi += n->phi.n;
      phy += n->phy.n;
    }
    printf("%d phi nodes\n", phi);
    printf("%d phy nodes\n", phi);
  }
  if (ifa_verbose > 3)
    print_renamed(f);
}

static inline PNode *
new_phiphy(PNode *n, Var *v, int phy) {
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

static inline int
maybe_live(PNode *n, Var *v) {
  return n->live_vars->get(v) != 0;
}

static int
merge_live(PNode *n, PNode *p) {
  int changed = 0;
  forv_Var(v, *p->live_vars) if (v)
    if (!n->lvals.in(v))
      changed = !n->live_vars->put(v);
  return changed;
}

static void
approximate_liveness(Fun *f, Vec<PNode *> &nodes) {
  int changed = 1;
  forv_PNode(n, nodes)
    n->live_vars = new BlockHash<Var *, PointerHashFns>;
  while (changed) {
    changed = 0;
    forv_PNode(n, nodes) {
      forv_PNode(p, n->cfg_succ)
        changed |= merge_live(n, p);
      forv_Var(v, n->rvals)
        if (v->sym->is_local)
          changed |= !n->live_vars->put(v);
    }
  }
}

static inline Var *
new_Var(Var *v, VarEnv &e, Fun *f) {
  if (!v->sym->is_local)
    return v;
  Var *vv = new Var(v->sym);
  e.put(v, vv);
  return vv;
}

static inline Var *
get_Var(Var *v, VarEnv &env, Fun *f) {
  if (!v->sym->is_local)
    return v;
  Var *vv = env.get(v);
  if (vv)
    return vv;
  return v;
}

static void
rename_edge(Fun *f, PNode *d, VarEnv &env, Vec<PNode *> &nset) {
  forv_PNode(p, d->phi)
    p->lvals[0] = new_Var(p->rvals[0]->sym->var, env, f);
  for (int i = 0; i < d->rvals.n; i++)
    d->rvals[i] = get_Var(d->rvals[i]->sym->var, env, f);
  for (int i = 0; i < d->lvals.n; i++)
    d->lvals[i] = new_Var(d->lvals[i]->sym->var, env, f);
  forv_PNode(p, d->phy)
    p->rvals[0] = get_Var(p->rvals[0]->sym->var, env, f);
  for (int i = 0; i < d->cfg_succ.n; i++) {
    if (d->cfg_succ.n != 1 && d->cfg_pred.n != 1)
      env.push();
    PNode *dd = d->cfg_succ[i];
    int di = dd->cfg_pred_index.get(d);
    forv_PNode(p, d->phy)
      p->lvals[i] = new_Var(p->rvals[0]->sym->var, env, f);
    forv_PNode(p, dd->phi)
      p->rvals[di] = get_Var(p->rvals[0]->sym->var, env, f);
    if (nset.set_add(dd))
      rename_edge(f, dd, env, nset);
    if (d->cfg_succ.n != 1 && d->cfg_pred.n != 1)
      env.pop();
  }
}

static void
rename_vars(Fun *f, Vec<PNode *> nodes) {
  forv_PNode(n, nodes) 
    for (int i = 0; i < n->cfg_pred.n; i++)
      n->cfg_pred_index.put(n->cfg_pred[i], i);
  VarEnv env;
  env.push();
  Vec<PNode *> nset;
  nset.add(f->entry);
  rename_edge(f, f->entry, env, nset);
}

static int
place_phi(Vec<Var *> vrs) {
  int changed = 0;
  forv_Var(v, vrs) {
    Vec<PNode *> w;
    w.copy(v->ssu->defs);        
    while (w.n) {
      PNode *n = w.pop();
      forv_Dom(d, n->dom->front) {
        PNode *y = (PNode*)d->node;
        if (!v->ssu->phis.in(y) && maybe_live(y, v)) {
          changed = 1;
          y->phi.add(new_phiphy(y, v, 0));
          v->ssu->phis.add(y);
          v->ssu->uses.append(y->cfg_pred);
          if (!v->ssu->defs.in(y)) {
            v->ssu->defs.add(y);
            w.add(y);
          }}}}}
  return changed;
}

static int
place_phy(Vec<Var *> vrs) {
  int changed = 0;
  forv_Var(v, vrs) {
    Vec<PNode *> w;
    w.copy(v->ssu->uses);        
    while (w.n) {
      PNode *n = w.pop();
      forv_Dom(d, n->rdom->front) {
        PNode *y = (PNode*)d->node;
        if (!v->ssu->phys.in(y) && maybe_live(y, v)) {
          changed = 1;
          y->phy.add(new_phiphy(y, v, 1));
          v->ssu->phys.add(y);
          v->ssu->defs.append(y->cfg_succ);
          if (!v->ssu->uses.in(y)) {
            v->ssu->uses.add(y);
            w.add(y);
          }}}}}
  return changed;
}

void
Fun::build_ssu() {
  if (!entry)
    return;
  build_cfg_dominators(this);
  Vec<Var *> vars, vrs;
  Vec<PNode *> pnodes;
  collect_Vars(vars, &pnodes);
  approximate_liveness(this, pnodes);
  forv_Var(v, vars) {
    if (v->sym->is_local) {
      v->ssu = new SSUVar;
      vrs.add(v);
    }
  }
  forv_PNode(p, pnodes) {
    forv_Var(v, p->lvals) if (v->sym->is_local)
      v->ssu->defs.add(p);
    forv_Var(v, p->rvals) if (v->sym->is_local)
      v->ssu->uses.add(p);
  }
  int phi = place_phi(vrs);
  int phy = place_phy(vrs);
  while (phi && phy) {
    phi = place_phi(vrs);
    phy = phi && place_phy(vrs);
  }
  rename_vars(this, pnodes);
  forv_PNode(n, pnodes) {
    forv_PNode(p, n->phi) forv_Var(v, p->lvals) v->def = n;
    forv_Var(v, n->lvals) v->def = n;
    forv_PNode(p, n->phy) forv_Var(v, p->lvals) v->def = n;
  }
  forv_Var(v, vrs) v->ssu = 0;
  print_ssu(this, pnodes);
}

