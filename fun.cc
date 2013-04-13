/* -*-Mode: c++;-*-
   Copyright (c) 2004-2009 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "pattern.h"
#include "fun.h"
#include "pnode.h"
#include "if1.h"
#include "ast.h"
#include "var.h"
#include "fail.h"

static int fun_id = 1;

void Fun::init_fun() {
  id = fun_id++;
  is_external = 0;
  is_generic = 0;
  is_varargs = 0;
  is_eager = 0;
  is_lazy = 0;
  nested_in = 0;
  clone_for_constants = 0;
  split_unique = 0;
  split_eager = 0;
  execution_frequency = 0.0;
  fa_collected = 0;
  promotion_cache = 0;
  coercion_cache = 0;
  generic_cache = 0;
  order_cache = 0;
  default_cache = 0;
  nmap = 0;
  vmap = 0;
  wraps = 0;
  size = -1;    
  live = 0;
  cg_string = 0;
  cg_structural_string = 0;
  llvm = 0;
}

static void
build_uses(PNode *n, int flags = 0) {
  forv_Var(v, n->rvals)
    v->uses.add(n);
  if (!(flags & FUN_COLLECT_VARS_NO_TVALS))
    forv_Var(v, n->tvals)
      v->uses.add(n);
  forv_PNode(p, n->phi)
    build_uses(p);
  if (!(flags & FUN_COLLECT_VARS_NO_PHY))
    forv_PNode(p, n->phy)
      build_uses(p);
}

static void 
build_uses(Fun *f) {
  Vec<PNode *> nodes;
  f->collect_PNodes(nodes);
  forv_PNode(p, nodes)
    build_uses(p);
}

Fun::Fun(Sym *asym) {
  sym = asym;
  asym->fun = this;
  ast = sym->ast;
  init_fun();
  if (ifa_verbose > 2) {
    if (asym->name)
      printf("function %s (%d:%d)\n", asym->name, id, asym->id);
    else
      printf("function %d\n", asym->id);
  }
  build_cfg();
  build_ssu();
  build_uses(this);
  setup_ast();
  check_invariants(this);
}

Fun::Fun() {
  sym = NULL;
  init_fun();
}

int
compar_funs(const void *ai, const void *aj) {
  int i = (*(Fun**)ai)->id;
  int j = (*(Fun**)aj)->id;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

void
Fun::setup_ast() {
  Vec<PNode *> nodes;
  collect_PNodes(nodes);
  forv_PNode(n, nodes) {
    if (n->code && n->code->ast)
      n->code->ast->pnodes.add(n);
  }
  nodes.clear();
}

void
Fun::collect_PNodes(Vec<PNode *> &v) {
  Vec<PNode *> sv;
  if (!entry)
    return;
  v.add(exit);
  sv.set_add(exit);
  for (int i = 0; i < v.n; i++)
    forv_PNode(p, v[i]->cfg_pred) if (p)
      if (sv.set_add(p))
        v.add(p);
}

static void
collect_Vars_PNode(PNode *n, Accum<Var *> &vars, int flags = 0) {
  forv_Var(v, n->rvals)
    vars.add(v);
  forv_Var(v, n->lvals)
    vars.add(v);
  if (!(flags & FUN_COLLECT_VARS_NO_TVALS))
    forv_Var(v, n->tvals)
      vars.add(v);
  forv_PNode(p, n->phi)
    collect_Vars_PNode(p, vars);
  if (!(flags & FUN_COLLECT_VARS_NO_PHY))
    forv_PNode(p, n->phy)
      collect_Vars_PNode(p, vars);
}

void
Fun::collect_Vars(Vec<Var *> &avars, Vec<PNode *> *nodes, int flags) {
  Vec<PNode *> sv, v;
  Accum<Var *> vars;
  if (!nodes) 
    nodes = &v;
  else
    nodes->clear();
  if (!entry)
    return;
  nodes->add(exit);
  for (int i = 0; i < nodes->n; i++) {
    collect_Vars_PNode(nodes->v[i], vars, flags);
    forv_PNode(p, nodes->v[i]->cfg_pred)
      if (sv.set_add(p))
        nodes->add(p);
  }
  forv_MPosition(p, positional_arg_positions)
    vars.add(args.get(p));
  forv_Var(v, rets)
    vars.add(v);
  avars.copy(vars.asvec);
}

static void
copy_var(Var **av, Fun *f) {
  Var *v = *av;
  if (v->sym->nesting_depth == f->sym->nesting_depth + 1) {
    if (!(v = f->vmap->get(*av))) {
      v = (*av)->copy();
      f->vmap->put(*av, v);
    }
  } else if (v->sym->nesting_depth) {
    Var *vv = f->vmap->get(*av);
    if (vv)
      v = vv;
  }
  *av = v;
}

static PNode *
copy_pnode(PNode *node, Fun *f) {
  PNode *n = new PNode();
  n->code = node->code;
  n->rvals.copy(node->rvals);
  n->lvals.copy(node->lvals);
  n->tvals.copy(node->tvals);
  n->cfg_succ.copy(node->cfg_succ);
  n->cfg_pred.copy(node->cfg_pred);
  n->phi.copy(node->phi);
  n->phy.copy(node->phy);
  n->prim = node->prim;
  for (int i = 0; i < n->rvals.n; i++)
    copy_var(&n->rvals[i], f);
  for (int i = 0; i < n->lvals.n; i++)
    copy_var(&n->lvals[i], f);
  for (int i = 0; i < n->tvals.n; i++)
    copy_var(&n->tvals[i], f);
  return n;
}

Fun *
Fun::copy(int copy_ast, VarMap *var_map) {
  Fun *f = new Fun();
  f->sym = sym;
  f->fmap = new Map<Fun *, Fun*>;
  f->nmap = new Map<PNode *, PNode*>;
  f->vmap = var_map ? new VarMap(*var_map) : new VarMap();
  f->wraps = wraps;

  Vec<PNode *> nodes;

  collect_Vars(fa_all_Vars, &fa_all_PNodes);
  forv_PNode(n, fa_all_PNodes) {
    PNode *p = copy_pnode(n, f);
    nodes.add(p);
    f->nmap->put(n, p);
    for (int i = 0; i < n->phi.n; i++) {
      PNode *nn = n->phi[i];
      PNode *pp = copy_pnode(nn, f);
      nodes.add(pp);
      f->nmap->put(nn, pp);
      p->phi[i] = pp;
    }
    for (int i = 0; i < n->phy.n; i++) {
      PNode *nn = n->phy[i];
      PNode *pp = copy_pnode(nn, f);
      nodes.add(pp);
      f->nmap->put(nn, pp);
      p->phy[i] = pp;
    }
  }
  forv_PNode(n, nodes) {
    for (int i = 0; i < n->cfg_succ.n; i++)
      n->cfg_succ[i] = f->nmap->get(n->cfg_succ.v[i]);
    for (int i = 0; i < n->cfg_pred.n; i++)
      n->cfg_pred[i] = f->nmap->get(n->cfg_pred.v[i]);
  }
  f->arg_syms.copy(arg_syms);
  f->arg_positions.copy(arg_positions);
  f->positional_arg_positions.copy(positional_arg_positions);
  forv_MPosition(p, f->positional_arg_positions) {
    Var *v = args.get(p);
    copy_var(&v, f);
    f->args.put(p, v);
  }
  f->rets.copy(rets);
  for (int i = 0; i < f->rets.n; i++)
    copy_var(&f->rets[i], f);
  for (int i = 0; i < f->vmap->n; i++)
    if (f->vmap->v[i].key) {
      f->vmap->v[i].value->def = f->nmap->get(f->vmap->v[i].value->def);
      for (int j = 0; j < f->vmap->v[i].value->uses.n; j++)
        f->vmap->v[i].value->uses[j] = f->nmap->get(f->vmap->v[i].value->uses[j]);
    }
  f->fa_all_PNodes.copy(fa_all_PNodes);
  for (int i = 0; i < f->fa_all_PNodes.n; i++)
    f->fa_all_PNodes[i] = f->nmap->get(f->fa_all_PNodes.v[i]);
  f->fa_all_Vars.copy(fa_all_Vars);
  for (int i = 0; i < f->fa_all_Vars.n; i++) {
    Var *v = f->vmap->get(f->fa_all_Vars[i]);
    if (v)
      f->fa_all_Vars[i] = v;
  }
  f->entry = f->nmap->get(entry);
  f->exit = f->nmap->get(exit);
  f->ess.copy(ess);
  ASTCopyContext context;
  context.nmap = f->nmap;
  context.vmap = f->vmap;
  context.fmap = f->fmap;
  context.fmap->put(this, f);
  f->ast = (ast && copy_ast)? ast->copy_tree(&context) : 0;
  if1_write_log();
  return f;
}

void
Fun::calls_funs(Vec<Fun *> &calls_funs) {
  calls_funs.clear();
  Vec<Vec<Fun *> *> calls_funs_vecs;
  calls.get_values(calls_funs_vecs);
  for (int i = 0; i < calls_funs_vecs.n; i++)
    calls_funs.set_union(*calls_funs_vecs[i]);
  calls_funs.set_to_vec();
}

void
Fun::called_by_funs(Vec<Fun *> &called_by) {
  called_by.clear();
  forv_CallPoint(c, called)
    called_by.set_add(c->fun);
  called_by.set_to_vec();
}

cchar *
Fun::pathname() {
  return ast->pathname();
}

cchar *
Fun::filename() {
  cchar *fn = pathname();
  cchar *r = strrchr(fn, '/');
  if (r) return r+1; else return fn;
}

int
Fun::line() {
  return ast->line(); 
}

int
Fun::source_line() {
  return ast->source_line();
}

void rebuild_cfg_pred_index(Fun *f) {
  forv_PNode(n, f->fa_all_PNodes) {
    n->cfg_pred_index.clear();
    for (int i = 0; i < n->cfg_pred.n; i++)
      n->cfg_pred_index.put(n->cfg_pred[i], i);
  }
}

void check_invariants(Fun *f) {
#ifndef DEBUG
  if (!f->entry)
    return;
  Accum<PNode *> nodes;
  nodes.add(f->exit);
  forv_PNode(p, nodes.asvec)
    forv_PNode(n, p->cfg_pred)
      nodes.add(n);
  forv_PNode(p, nodes.asvec) {
    forv_PNode(n, p->cfg_pred)
      assert(n->cfg_succ.in(p));
    forv_PNode(n, p->cfg_succ)
      assert(n->cfg_pred.in(p));
  }
  Accum<PNode *> forward_nodes;
  forward_nodes.add(f->entry);
  forv_PNode(p, nodes.asvec)
    forv_PNode(n, p->cfg_succ)
      forward_nodes.add(n);
  assert(!nodes.asset.some_disjunction(forward_nodes.asset));
#endif
}

