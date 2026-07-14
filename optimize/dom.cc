#include "ifadefs.h"

#include "dom.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "pnode.h"

/*
 Fast Dominator Algorithm from Tarjan.
 Dominator Frontier Algorithm from Cytron et el. Tech Report
*/

#define VNULL (vertex[0])

Dom::Dom(void *n) : node(n), semi(-1), size(1), label(0), parent(0), child(0), ancestor(0), idom(0) {}

static int df_traversal(Dom *d, uint n, Vec<Dom *> &vertex) {
  d->semi = ++n;
  d->label = d;
  vertex.add(d);
  for (Dom *dd : d->succ) {
    if (dd->semi < 0) {
      dd->parent = d;
      n = df_traversal(dd, n, vertex);
    }
  }
  return n;
}

static void df_compress(Dom *v, Vec<Dom *> &vertex) {
  if (v->ancestor->ancestor == VNULL) return;
  df_compress(v->ancestor, vertex);
  if (v->ancestor->label->semi < v->label->semi) v->label = v->ancestor->label;
  v->ancestor = v->ancestor->ancestor;
}

static Dom *df_eval(Dom *v, Vec<Dom *> &vertex) {
  if (v->ancestor == VNULL) return v->label;
  df_compress(v, vertex);
  if (v->ancestor->label->semi >= v->label->semi) return v->label;
  return v->ancestor->label;
}

static void df_link(Dom *v, Dom *w, Vec<Dom *> &vertex) {
  Dom *s = w;
  while (w->label->semi < s->child->label->semi) {
    Dom *scc = s->child->child;
    if (s->size + scc->size >= 2 * s->child->size) {
      s->child->ancestor = s;
      s->child = scc;
    } else {
      s->child->size = s->size;
      s = s->ancestor = s->child;
    }
  }
  s->label = w->label;
  assert(v != w);
  v->size += w->size;
  if (v->size < 2 * w->size) {
    Dom *t = s;
    s = v->child;
    v->child = t;
  }
  while (s != VNULL) {
    s->ancestor = v;
    s = s->child;
  }
}

static void compute_semi(Dom *w, Vec<Dom *> &vertex) {
  for (Dom *v : w->pred) {
    int semiu = df_eval(v, vertex)->semi;
    if (semiu < w->semi) w->semi = semiu;
  }
  vertex[w->semi]->bucket.add(w);
  df_link(w->parent, w, vertex);
  while (w->parent->bucket.n) {
    Dom *v = w->parent->bucket.pop();
    Dom *u = df_eval(v, vertex);
    v->idom = u->semi < v->semi ? u : w->parent;
  }
}

static void compute_dom(Dom *w, Vec<Dom *> &vertex) {
  if (w->idom != vertex[w->semi]) w->idom = w->idom->idom;
}

static void find_dominators(Vec<Dom *> &vertex) {
  for (int i = vertex.n - 1; i > 1; i--) compute_semi(vertex[i], vertex);
  for (int i = 2; i < vertex.n; i++) compute_dom(vertex[i], vertex);
  vertex[1]->idom = VNULL;
}

static void make_dominator_tree(Vec<Dom *> &vertex) {
  for (Dom *x : vertex) if (x != VNULL && x->idom != VNULL) x->idom->children.add(x);
}

static void find_dominator_frontier_internal(Dom *n) {
  for (Dom *x : n->children) find_dominator_frontier_internal(x);
  for (Dom *x : n->succ) if (x->idom != n) n->front.set_add(x);
  for (Dom *x : n->children) for (Dom *y : x->front) if (y) if (y->idom != n) n->front.set_add(y);
}

static void find_dominator_frontier(Dom *n, Vec<Dom *> &vertex) {
  find_dominator_frontier_internal(n);
  for (Dom *x : vertex) if (x != VNULL) x->front.set_to_vec();
}

static void dom_replace(Dom *d, void *a, void *b) {
  if (d->label == a) d->label = (Dom *)b;
  if (d->parent == a) d->parent = (Dom *)b;
  if (d->child == a) d->child = (Dom *)b;
  if (d->ancestor == a) d->ancestor = (Dom *)b;
  if (d->idom == a) d->idom = (Dom *)b;
}

static int dom_dfs(Dom *d, int n = 0) {
  d->dfs = ++n;
  for (Dom *x : d->children) n = dom_dfs(x, n);
  return n;
}

static void dom_build_intervals(Dom *d) {
  if (d->idom) d->intervals.copy(d->idom->intervals);
  d->intervals.insert(d->dfs);
  for (Dom *x : d->children) dom_build_intervals(x);
}

static void make_dom_intervals(Dom *d) {
  dom_dfs(d);
  dom_build_intervals(d);
}

void build_dominators(Dom *d) {
  Vec<Dom *> vertex;
  vertex.add(new Dom(0));
  vertex[0]->semi = 0;
  vertex[0]->size = 0;
  df_traversal(d, 0, vertex);
  for (Dom *x : vertex) dom_replace(x, (void *)0, (void *)VNULL);
  find_dominators(vertex);
  make_dominator_tree(vertex);
  find_dominator_frontier(d, vertex);
  for (Dom *x : vertex) dom_replace(x, (void *)VNULL, (void *)0);
  make_dom_intervals(d);
}

void build_cfg_dominators(Fun *f) {
  Vec<PNode *> pnodes;
  f->collect_PNodes(pnodes);
  for (PNode *p : pnodes) {
    p->dom = new Dom(p);
    p->rdom = new Dom(p);
  }
  // The node universe is collect_PNodes' backward-from-exit
  // reachability -- the same universe every other pass uses. A CFG
  // region that is forward-reachable from entry but has NO path to
  // exit is outside it: its PNodes have no Dom (null), yet universe
  // nodes still hold cfg_succ edges INTO the region, so df_traversal
  // (forward from entry) used to walk through a null Dom and
  // segfault. The one observed producer of such a region (othello2's
  // vs_cpu_ugi: a break-label scoping bug lowering `break` to a
  // goto-to-self cycle) was a frontend bug, fixed at the source
  // (python_ifa_build_syms.cc, PY_for_stmt/PY_while_stmt label
  // save/restore) -- this guard stays as defense-in-depth, because
  // pruning such edges is sound for dominance within the universe:
  // a path entry -> n -> exit can never detour through a no-exit
  // region (once in, never out), so no dominance relation among
  // universe nodes depends on them. NOTE: liveness/SSU (ssu.cc) walk
  // the same universe and would also misbehave on a real no-exit
  // region, so if one ever becomes legitimately constructible, it
  // needs a universe-level answer, not more edge guards.
  for (PNode *p : pnodes) {
    for (PNode *pp : p->cfg_pred) if (pp->dom) {
      p->dom->pred.add(pp->dom);
      p->rdom->succ.add(pp->rdom);
    }
    for (PNode *pp : p->cfg_succ) if (pp->dom) {
      p->dom->succ.add(pp->dom);
      p->rdom->pred.add(pp->rdom);
    }
  }
  build_dominators(f->entry->dom);
  build_dominators(f->exit->rdom);
}

void build_call_dominators(FA *fa) {
  for (Fun *f : fa->funs) f->dom = new Dom(f);
  for (Fun *f : fa->funs) {
    Vec<Fun *> calls;
    f->calls_funs(calls);
    for (Fun *ff : calls) f->dom->succ.add(ff->dom);
    f->called_by_funs(calls);
    for (Fun *ff : calls) f->dom->pred.add(ff->dom);
  }
  build_dominators(if1->top->fun->dom);
}
