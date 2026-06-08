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

// ---------------------------------------------------------------------------
// Inline-event sidecar
// ---------------------------------------------------------------------------

static bool inline_events_enabled = false;
static Vec<InlineEvent *> inline_events_storage;

void inline_events_enable() { inline_events_enabled = true; }
void inline_events_disable() { inline_events_enabled = false; }
void inline_events_reset() { inline_events_storage.clear(); }
const Vec<InlineEvent *> &inline_events_get() { return inline_events_storage; }

static void record_inline_event(InlineEventKind k, Fun *caller, PNode *pnode, Fun *callee) {
  if (!inline_events_enabled) return;
  InlineEvent *e = new InlineEvent;
  e->kind = k;
  e->caller = caller;
  e->pnode = pnode;
  e->callee = callee;
  inline_events_storage.add(e);
}

static void dfs_order(Fun *f, Vec<Fun *> &funs, Vec<Fun *> &fset) {
  if (!fset.set_add(f)) return;
  funs.add(f);
  Vec<Fun *> calls_funs;
  f->calls_funs(calls_funs);
  for (Fun *ff : calls_funs) dfs_order(ff, funs, fset);
}

static void local_loop_frequency_estimation(LoopNode *l, float f) {
  for (LoopNode *n : l->children) {
    if (n->node)
      ((PNode *)n->node)->execution_frequency = f;
    else
      local_loop_frequency_estimation(n, f * LOOP_FREQUENCY);
  }
}

static void local_frequency_estimation(Fun *f) {
  if (f->loops->loops) local_loop_frequency_estimation(f->loops->loops, LOOP_FREQUENCY);
  Vec<PNode *> nodes;
  f->collect_PNodes(nodes);
  for (PNode *n : nodes) if (n->execution_frequency < 1.0) n->execution_frequency = 1.0;
}

static void global_loop_frequency_estimation(LoopNode *l, float f) {
  for (LoopNode *n : l->children) {
    assert(n != l);
    if (n->node)
      ((Fun *)n->node)->execution_frequency = f;
    else
      global_loop_frequency_estimation(n, f * LOOP_FREQUENCY);
  }
}

static void global_frequency_estimation(FA *fa) {
  if (fa->pdb->loops->loops) global_loop_frequency_estimation(fa->pdb->loops->loops, LOOP_FREQUENCY);
  for (Fun *f : fa->funs) if (f->execution_frequency < 1.0) f->execution_frequency = 1.0;
  Vec<Fun *> funs, fset;
  dfs_order(fa->pdb->if1->top->fun, funs, fset);
  // propagate down the call tree
  for (Fun *f : funs) {
    float freq = f->execution_frequency;
    Vec<PNode *> nodes;
    f->collect_PNodes(nodes);
    for (PNode *n : nodes) n->execution_frequency *= freq;
    f->execution_frequency = 0;
    for (CallPoint *c : f->called) {
      if (c->fun != f && f->loop_node->dfs_ancestor(c->fun->loop_node))
        f->execution_frequency += freq * c->pnode->execution_frequency;
    }
    if (f->execution_frequency < 1.0) f->execution_frequency = 1.0;
  }
}

int frequency_estimation(FA *fa) {
  find_all_loops(fa);
  for (Fun *f : fa->funs) local_frequency_estimation(f);
  global_frequency_estimation(fa);
  return 0;
}

static int is_closure_create(PNode *n) {
  // n->lvals can be empty for 0-result primitive sends and the Var's
  // type can be unset for vars no analysis path touched — both happen
  // legitimately for synthetic test-harness primitives. Treat either
  // as "not a closure create".
  if (n->lvals.n == 0) return 0;
  if (!n->lvals[0]->type) return 0;
  return (n->lvals[0]->type->type_kind == Type_FUN && n->creates);
}

static int is_period_prim(PNode *n) { return (n->prim && n->prim->index == P_prim_period); }

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
  while (v->def && v->def->code && v->def->code->kind == Code_MOVE) v = v->def->rvals[0];
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
  for (Var *v : rvals) {
    if (Sym *c = get_constant(v))
      p->rvals.add(new_live_Var(c));
    else
      p->rvals.add(v);
  }
  for (PNode *n : p->phi) sub_constants(n);
  for (PNode *n : p->phy) sub_constants(n);
}

static int reaching_def(Var *v, PNode *p) {
  Accum<Var *> vars;
  vars.add(v);
  // Must use index-based loop: vars.add() inside the loop appends to vars.asvec,
  // growing the worklist. Range-for captures end once and misses new elements;
  // it also produces dangling iterators if asvec reallocates its buffer.
  for (int i = 0; i < vars.asvec.n; i++) {
    Var *v = vars.asvec.v[i];
    if (v->def == p) return 1;
    if (v->def && v->def->code->kind == Code_MOVE) for (Var *x : v->def->rvals) vars.add(x);
  }
  return 0;
}

static int reaching_var(Var *v, Var *vv) {
  Accum<Var *> vars;
  vars.add(v);
  // Must use index-based loop: vars.add() inside the loop appends to vars.asvec,
  // growing the worklist. Range-for captures end once and misses new elements;
  // it also produces dangling iterators if asvec reallocates its buffer.
  for (int i = 0; i < vars.asvec.n; i++) {
    Var *v = vars.asvec.v[i];
    if (v == vv) return 1;
    if (v->def && v->def->code->kind == Code_MOVE) for (Var *x : v->def->rvals) vars.add(x);
  }
  return 0;
}

static void insert_move_before(Fun *f, PNode *p, Var *rhs, Var *lhs) {
  check_invariants(f);
  PNode *n = new PNode(new Code(Code_MOVE));
  for (PNode *x : p->cfg_pred) x->cfg_succ[x->cfg_succ.index(p)] = n;
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
  for (Var *v : s->rvals) {
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

// ---------------------------------------------------------------------------
// Chain-aware matcher (issue 006 / Gap B): match a straight-line
// sequence of >=2 primitive SENDs feeding a single reply. Each chain
// element reads only formals, constants, or earlier chain lvals.
// Returns the chain Vec on match, nullptr otherwise.
// ---------------------------------------------------------------------------
static Vec<PNode *> *match_prim_chain(Fun *f) {
  Vec<PNode *> chain;
  PNode *reply = nullptr;
  for (PNode *n : f->fa_all_PNodes) {
    if (!n->code || !n->live) continue;
    if (n->code->kind == Code_MOVE || n->code->kind == Code_NOP) continue;
    if (n->prim == prim_reply) {
      if (reply) return nullptr;  // multiple replies
      reply = n;
      continue;
    }
    if (n->code->kind == Code_SEND && is_closure_create(n)) continue;
    if (n->code->kind != Code_SEND) return nullptr;  // label / if / goto / etc.
    if (!n->prim || f->calls.get(n)) return nullptr;  // closure-call (Gap C)
    chain.add(n);
  }
  if (chain.n < 2) return nullptr;  // single SEND is handled by the existing matcher

  // Each chain SEND's rvals must reach: a formal of fn, a constant, or
  // an earlier chain element's lval. No reads from later sends or globals.
  for (int i = 0; i < chain.n; i++) {
    PNode *n = chain[i];
    for (Var *v : n->rvals) {
      Sym *fs = first_var(v)->sym;
      if (fs && f->sym->has.index(fs) >= 0) continue;  // formal
      if (v->sym->is_constant || v->sym->is_symbol) continue;
      // Otherwise must reach an earlier chain element's lval.
      bool ok = false;
      for (int j = 0; j < i && !ok; j++) {
        for (Var *lv : chain[j]->lvals) {
          if (reaching_var(v, lv->sym->var)) { ok = true; break; }
        }
      }
      if (!ok) return nullptr;
    }
  }
  // If a reply is present, it must read the last chain element's lval.
  // (Bodies without an explicit reply are accepted; matches the
  // single_send matcher convention — the call site's lval determines
  // where the result goes.)
  if (reply) {
    if (reply->rvals.n < 1) return nullptr;
    Var *retv = reply->rvals[reply->rvals.n - 1];
    PNode *last = chain[chain.n - 1];
    bool reply_ok = false;
    for (Var *lv : last->lvals) {
      if (reaching_var(retv, lv->sym->var)) { reply_ok = true; break; }
    }
    if (!reply_ok) return nullptr;
  }

  Vec<PNode *> *result = new Vec<PNode *>;
  result->move(chain);
  return result;
}

// Splice a chain wrapper at the call site `p` (in caller `f`, callee
// `fn`). chain[0] re-uses `p`; chain[1..N-1] are inserted as new PNodes
// linked into `p->cfg_succ`. Intermediate lvals get fresh Vars in the
// caller's namespace; the final chain element writes to `p`'s original
// lval. Formal references substitute caller-supplied actuals (with
// type-coercion MOVEs as needed, same as inline_single_pnode).
static void inline_prim_chain(Fun *f, PNode *p, Fun *fn, Vec<PNode *> *chain) {
  // Save call-site state.
  Vec<Var *> actuals;
  actuals.move(p->rvals);
  Var *call_site_lval = p->lvals.n ? p->lvals[0] : nullptr;

  // Allocate fresh intermediate Vars for chain[0..N-2]; chain[N-1]
  // writes to the call site's original lval.
  Vec<Var *> intermediates;
  for (int i = 0; i < chain->n; i++) {
    if (i == chain->n - 1) {
      intermediates.add(call_site_lval);
    } else {
      Var *body_lval = chain->v[i]->lvals[0];
      Var *fresh = new_live_Var(body_lval->sym);
      fresh->type = body_lval->type;
      fresh->avars = body_lval->avars;
      intermediates.add(fresh);
    }
  }

  // Substitute one chain element's rvals into the destination PNode.
  auto subst_rvals = [&](PNode *dst, PNode *src, int src_index) {
    dst->rvals.clear();
    for (Var *v : src->rvals) {
      if (v->constant) { dst->rvals.add(new_live_Var(v->constant)); continue; }
      Sym *fs = first_var(v)->sym;
      int formal_idx = (fs ? fn->sym->has.index(fs) : -1);
      if (formal_idx >= 0) {
        Var *actual = actuals[formal_idx];
        if (actual->constant) {
          dst->rvals.add(new_live_Var(actual->constant));
        } else if (actual->type == v->type) {
          dst->rvals.add(actual);
        } else {
          // Type coercion: insert a MOVE that retypes the actual.
          Var *vv = new_live_Var(actual->sym);
          if (actual->type->type_kind != Type_SUM) {
            vv->type = actual->type;
            vv->avars = actual->avars;
          } else {
            assert(v->type->type_kind != Type_SUM);
            vv->type = v->type;
            vv->avars = v->avars;
          }
          insert_move_before(f, dst, actual, vv);
          dst->rvals.add(vv);
        }
        continue;
      }
      // Otherwise must reach an earlier chain element's lval.
      bool resolved = false;
      for (int j = 0; j < src_index && !resolved; j++) {
        for (Var *lv : chain->v[j]->lvals) {
          if (reaching_var(v, lv->sym->var)) {
            dst->rvals.add(intermediates[j]);
            resolved = true;
            break;
          }
        }
      }
      assert(resolved);
    }
  };

  // chain[0]: reuse `p`.
  p->prim = chain->v[0]->prim;
  f->calls.put(p, fn->calls.get(chain->v[0]));
  p->lvals.clear();
  if (intermediates[0]) p->lvals.add(intermediates[0]);
  subst_rvals(p, chain->v[0], 0);

  // chain[1..N-1]: insert new PNodes between p and p's original succ.
  PNode *prev = p;
  for (int i = 1; i < chain->n; i++) {
    PNode *n = new PNode(new Code(Code_SEND));
    n->prim = chain->v[i]->prim;
    f->calls.put(n, fn->calls.get(chain->v[i]));
    if (intermediates[i]) n->lvals.add(intermediates[i]);
    // CFG insertion: n takes prev's successors; prev now points only at n.
    Vec<PNode *> old_succ;
    old_succ.copy(prev->cfg_succ);
    for (PNode *s : old_succ) {
      s->cfg_pred[s->cfg_pred.index(prev)] = n;
      n->cfg_succ.add(s);
    }
    prev->cfg_succ.clear();
    prev->cfg_succ.add(n);
    n->cfg_pred.add(prev);
    n->live = 1;
    n->fa_live = 1;
    subst_rvals(n, chain->v[i], i);
    prev = n;
  }
  check_invariants(f);
}

static int inline_single_sends(FA *fa) {
  Map<Fun *, PNode *> single_send;
  Map<Fun *, int> identity_send;
  Map<Fun *, Vec<PNode *> *> chain_send;  // chain of >=2 prim sends (issue 006)
  for (Fun *f : fa->funs) {  // find chain-eligible functions first
    assert(f->live);
    if (Vec<PNode *> *chain = match_prim_chain(f)) chain_send.put(f, chain);
  }
  for (Fun *f : fa->funs) {  // find single prim send functions
    assert(f->live);
    PNode *p = 0, *reply = 0;
    for (PNode *n : f->fa_all_PNodes) {
      if (!n->code || n->code->kind == Code_MOVE || !n->live) continue;
      // for (Var *v : n->rvals) { assert(v->live || v->constant); }
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
          if (reaching_var(reply->rvals[reply->rvals.n - 1], f->sym->has[i]->var)) {
            identity_send.put(f, i + 1);  // offset by 1 to avoid collision with empty (0)
            continue;
          }
        }
      }
      continue;
    }
    if (p == f->exit || p->code->kind != Code_SEND || !p->prim || f->calls.get(p)) continue;
    for (Var *v : p->rvals) {
      Sym *fs = first_var(v)->sym;
      if (!((fs && (f->sym->has.index(fs) >= 0)) || v->sym->is_constant || v->sym->is_symbol)) goto Lskip;
    }
    if (reply && !reaching_def(reply->rvals[reply->rvals.n - 1], p)) continue;
    single_send.put(f, p);
  Lskip:;
  }
  for (Fun *f : fa->funs) {
    assert(f->live);
    for (PNode *p : f->fa_all_PNodes) {
      if (!p->live) continue;
      // for (Var *v : p->rvals) { assert(v->live || v->constant); }
      Vec<Fun *> *calls = f->calls.get(p);
      if (p->code && p->code->kind == Code_SEND && !is_closure_call(p)) {
        // inline single send functions
        if (calls && calls->n == 1) {
          Fun *fn = calls->v[0];
          Vec<PNode *> *chain = chain_send.get(fn);
          if (chain) {
            inline_prim_chain(f, p, fn, chain);
            record_inline_event(INLINE_PRIM_CHAIN, f, p, fn);
          } else {
            PNode *s = single_send.get(fn);
            if (s) {
              inline_single_pnode(f, p, fn, s);
              record_inline_event(INLINE_SINGLE_SEND, f, p, fn);
            }
            int i = identity_send.get(fn);
            if (i) {
              convert_to_move(p, i - 1);
              record_inline_event(INLINE_IDENTITY, f, p, fn);
            }
          }
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
            for (Var *v : c->rvals) p->rvals.add(v);
          }
          for (int i = 1; i < rvals.n; i++) p->rvals.add(rvals[i]);
          record_inline_event(INLINE_CLOSURE, f, p, 0);
          if (calls && calls->n == 1) {
            Fun *fn = calls->v[0];
            Vec<PNode *> *chain = chain_send.get(fn);
            if (chain) {
              inline_prim_chain(f, p, fn, chain);
              record_inline_event(INLINE_PRIM_CHAIN, f, p, fn);
            } else {
              PNode *s = single_send.get(fn);
              if (s) {
                inline_single_pnode(f, p, fn, s);
                record_inline_event(INLINE_SINGLE_SEND, f, p, fn);
              }
              int i = identity_send.get(fn);
              if (i) {
                convert_to_move(p, i - 1);
                record_inline_event(INLINE_IDENTITY, f, p, fn);
              }
            }
          }
        }
      }
    }
    f->collect_Vars(f->fa_all_Vars, &f->fa_all_PNodes);
  }
  for (Fun *f : fa->funs) {
    for (PNode *p : f->fa_all_PNodes) if (p->live) sub_constants(p);
    f->collect_Vars(f->fa_all_Vars, &f->fa_all_PNodes);
  }
  return 0;
}

int simple_inlining(FA *fa) {
  inline_single_sends(fa);
  return 0;
}
