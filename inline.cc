/* -*-Mode: c++;-*-
   Copyright (c) 2003-2013 John Plevyak, All Rights Reserved
*/
#include "inline.h"
#include "clone.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "ifadefs.h"
#include "loop.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"

#define LOOP_FREQUENCY 10.0

static void dfs_order(Fun *f, Vec<Fun *> &funs, Vec<Fun *> &fset) {
  if (!fset.set_add(f)) return;
  funs.add(f);
  Vec<Fun *> calls_funs;
  f->calls_funs(calls_funs);
  forv_Fun(ff, calls_funs) dfs_order(ff, funs, fset);
}

static void local_loop_frequency_estimation(LoopNode *l, float f) {
  forv_LoopNode(n, l->children) {
    if (n->node)
      ((PNode *)n->node)->execution_frequency = f;
    else
      local_loop_frequency_estimation(n, f * LOOP_FREQUENCY);
  }
}

static void local_frequency_estimation(Fun *f) {
  if (f->loops->loops)
    local_loop_frequency_estimation(f->loops->loops, LOOP_FREQUENCY);
  Vec<PNode *> nodes;
  f->collect_PNodes(nodes);
  forv_PNode(n, nodes) if (n->execution_frequency < 1.0)
      n->execution_frequency = 1.0;
}

static void global_loop_frequency_estimation(LoopNode *l, float f) {
  forv_LoopNode(n, l->children) {
    assert(n != l);
    if (n->node)
      ((Fun *)n->node)->execution_frequency = f;
    else
      global_loop_frequency_estimation(n, f * LOOP_FREQUENCY);
  }
}

static void global_frequency_estimation(FA *fa) {
  if (fa->pdb->loops->loops)
    global_loop_frequency_estimation(fa->pdb->loops->loops, LOOP_FREQUENCY);
  forv_Fun(f, fa->funs) if (f->execution_frequency < 1.0)
      f->execution_frequency = 1.0;
  Vec<Fun *> funs, fset;
  dfs_order(fa->pdb->if1->top->fun, funs, fset);
  // propagate down the call tree
  forv_Fun(f, funs) {
    float freq = f->execution_frequency;
    Vec<PNode *> nodes;
    f->collect_PNodes(nodes);
    forv_PNode(n, nodes) n->execution_frequency *= freq;
    f->execution_frequency = 0;
    forv_CallPoint(c, f->called) {
      if (c->fun != f && f->loop_node->dfs_ancestor(c->fun->loop_node))
        f->execution_frequency += freq * c->pnode->execution_frequency;
    }
    if (f->execution_frequency < 1.0) f->execution_frequency = 1.0;
  }
}

int frequency_estimation(FA *fa) {
  find_all_loops(fa);
  forv_Fun(f, fa->funs) local_frequency_estimation(f);
  global_frequency_estimation(fa);
  return 0;
}

static int is_closure_create(PNode *n) {
  return (n->lvals[0]->type->type_kind == Type_FUN && n->creates);
}

static int is_period_prim(PNode *n) {
  return (n->prim && n->prim->index == P_prim_period);
}

static int is_closure_call(PNode *n) {
  if (n->code->kind != Code_SEND) return 0;
  Sym *t = n->rvals[0]->type;
  if (!t || t->type_kind != Type_FUN || t->fun || !t->has.n) return 0;
  return 1;
}

static int is_simple_closure_create(PNode *n, bool verify_other = true);

static PNode *simple_closure_call(PNode *n, bool verify_other = true) {
  if (!is_closure_call(n)) return 0;
  PNode *p = n->cfg_pred[0];
  Var *v = n->rvals[0];
  while (p->code->kind == Code_MOVE && v == p->lvals[0]) {
    v = p->rvals[0];
    p = p->cfg_pred[0];
  }
  if (!is_closure_create(p) || p->lvals[0] != v) return 0;
  if (verify_other && !is_simple_closure_create(p, false)) return 0;
  return p;
}

static int is_simple_closure_create(PNode *n, bool verify_other) {
  if (!is_closure_create(n)) return 0;
  PNode *s = n->cfg_succ[0];
  Var *v = n->lvals[0];
  if (n->lvals[0]->uses.n != 1 || n->lvals[0]->uses[0] != s) return 0;
  while (s->code->kind == Code_MOVE && s->rvals[0] == v) {
    v = s->lvals[0];
    s = s->cfg_succ[0];
  }
  if (verify_other && !simple_closure_call(s, false)) return 0;
  return 1;
}

static Var *first_var(Var *v) {  // does not move through PHI/PHY
  while (v->def && v->def->code && v->def->code->kind == Code_MOVE)
    v = v->def->rvals[0];
  return v;
}

static Var *new_live_Var(Sym *s) {
  Var *v = new Var(s);
  v->type = s->type;
  v->live = 1;
  return v;
}

static void sub_constants(PNode *p) {
  Vec<Var *> rvals;
  rvals.move(p->rvals);
  forv_Var(v, rvals) {
    if (Sym *c = constant(v))
      p->rvals.add(new_live_Var(c));
    else
      p->rvals.add(v);
  }
  forv_PNode(n, p->phi) sub_constants(n);
  forv_PNode(n, p->phy) sub_constants(n);
}

static int reaching_def(Var *v, PNode *p) {
  Accum<Var *> vars;
  vars.add(v);
  forv_Var(v, vars.asvec) {
    if (v->def == p) return 1;
    if (v->def && v->def->code->kind == Code_MOVE)
      forv_Var(x, v->def->rvals) vars.add(x);
  }
  return 0;
}

static int reaching_var(Var *v, Var *vv) {
  Accum<Var *> vars;
  vars.add(v);
  forv_Var(v, vars.asvec) {
    if (v == vv) return 1;
    if (v->def && v->def->code->kind == Code_MOVE)
      forv_Var(x, v->def->rvals) vars.add(x);
  }
  return 0;
}

static void insert_move_before(Fun *f, PNode *p, Var *rhs, Var *lhs) {
  check_invariants(f);
  PNode *n = new PNode(new Code(Code_MOVE));
  forv_PNode(x, p->cfg_pred) x->cfg_succ[x->cfg_succ.index(p)] = n;
  n->cfg_pred.copy(p->cfg_pred);
  if (f->entry == p) {
    f->entry = n;
    p->cfg_pred.add(n);
  } else {
    p->cfg_pred[0] = n;
    p->cfg_pred.n = 1;
  }
  n->cfg_succ.add(p);
  n->lvals.add(lhs);
  n->rvals.add(rhs);
  n->live = 1;
  n->fa_live = 1;
  check_invariants(f);
}

static void inline_single_pnode(Fun *f, PNode *p, Fun *fn, PNode *s) {
  Vec<Var *> rvals;
  rvals.move(p->rvals);
  p->prim = s->prim;
  f->calls.put(p, fn->calls.get(s));
  forv_Var(v, s->rvals) {
    if (v->constant) {
      p->rvals.add(new_live_Var(v->constant));
      continue;
    }
    Sym *fs = first_var(v)->sym;
    int i = fn->sym->has.index(fs);
    assert(i >= 0);
    if (rvals[i]->constant) {
      p->rvals.add(new_live_Var(rvals[i]->constant));
      continue;
    }
    if (rvals[i]->type == v->type) {
      p->rvals.add(rvals[i]);
    } else {
      Var *vv = new_live_Var(rvals[i]->sym);
      if (rvals[i]->type->type_kind != Type_SUM) {
        vv->type = rvals[i]->type;
        vv->avars = rvals[i]->avars;
      } else {
        assert(v->type->type_kind != Type_SUM);
        vv->type = v->type;
        vv->avars = v->avars;
      }
      insert_move_before(f, p, rvals[i], vv);
      p->rvals.add(vv);
    }
  }
  check_invariants(f);
}

static void convert_to_move(PNode *p, int i) {
  p->code = new Code(*p->code);
  p->code->kind = Code_MOVE;
  p->rvals[0] = p->rvals[i];
  p->rvals.n = 1;
}

static int inline_single_sends(FA *fa) {
  Map<Fun *, PNode *> single_send;
  Map<Fun *, int> identity_send;
  forv_Fun(f, fa->funs) {  // find single prim send functions
    assert(f->live);
    PNode *p = 0, *reply = 0;
    forv_PNode(n, f->fa_all_PNodes) {
      if (!n->code || n->code->kind == Code_MOVE || !n->live) continue;
      // forv_Var(v, n->rvals) { assert(v->live || v->constant); }
      if (n->prim == prim_reply) {
        if (!reply) {
          reply = n;
          continue;
        } else {
          p = f->exit;  // bail
          break;
        }
      }
      if (n->code->kind == Code_SEND && is_closure_create(n)) continue;
      if (!p)
        p = n;
      else {
        p = f->exit;  // bail
        break;
      }
    }
    if (!p) {
      // check for identity function
      if (reply) {
        for (int i = 0; i < f->sym->has.n; i++) {
          // if (f->sym->self == f->sym->has.v[i])
          // continue;
          if (reaching_var(reply->rvals[reply->rvals.n - 1],
                           f->sym->has[i]->var)) {
            identity_send.put(
                f, i + 1);  // offset by 1 to avoid collision with empty (0)
            continue;
          }
        }
      }
      continue;
    }
    if (p == f->exit || p->code->kind != Code_SEND || !p->prim ||
        f->calls.get(p))
      continue;
    forv_Var(v, p->rvals) {
      Sym *fs = first_var(v)->sym;
      if (!((fs && (f->sym->has.index(fs) >= 0)) || v->sym->is_constant ||
            v->sym->is_symbol))
        goto Lskip;
    }
    if (reply && !reaching_def(reply->rvals[reply->rvals.n - 1], p)) continue;
    single_send.put(f, p);
  Lskip:;
  }
  forv_Fun(f, fa->funs) {
    assert(f->live);
    forv_PNode(p, f->fa_all_PNodes) {
      if (!p->live) continue;
      // forv_Var(v, p->rvals) { assert(v->live || v->constant); }
      Vec<Fun *> *calls = f->calls.get(p);
      if (p->code && p->code->kind == Code_SEND && !is_closure_call(p)) {
        // inline single send functions
        if (calls && calls->n == 1) {
          Fun *fn = calls->v[0];
          PNode *s = single_send.get(fn);
          if (s) inline_single_pnode(f, p, fn, s);
          int i = identity_send.get(fn);
          if (i) convert_to_move(p, i - 1);
        }
      } else {
        PNode *c = simple_closure_call(p);
        if (c) {
          Vec<Var *> rvals;
          rvals.move(p->rvals);
          c->live = 0;
          c->lvals[0]->live = 0;
          if (is_period_prim(c)) {
            p->rvals.add(c->rvals.v[3]);
            p->rvals.add(c->rvals.v[1]);
          } else {
            forv_Var(v, c->rvals) p->rvals.add(v);
          }
          for (int i = 1; i < rvals.n; i++) p->rvals.add(rvals[i]);
          if (calls && calls->n == 1) {
            Fun *fn = calls->v[0];
            PNode *s = single_send.get(fn);
            if (s) inline_single_pnode(f, p, fn, s);
            int i = identity_send.get(fn);
            if (i) convert_to_move(p, i - 1);
          }
        }
      }
    }
    f->collect_Vars(f->fa_all_Vars, &f->fa_all_PNodes);
  }
  forv_Fun(f, fa->funs) {
    forv_PNode(p, f->fa_all_PNodes) if (p->live) sub_constants(p);
    f->collect_Vars(f->fa_all_Vars, &f->fa_all_PNodes);
  }
  return 0;
}

int simple_inlining(FA *fa) {
  inline_single_sends(fa);
  return 0;
}
