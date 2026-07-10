#ifndef _dom_H
#define _dom_H

class Fun;

struct Dom : public gc {
  // Issue 035: stable creation-order serial for deterministic
  // hashing — Dom sets (front, children, ...) are Vec hash sets,
  // and raw-pointer bucketing made dominance-frontier iteration
  // (hence phi placement order, hence Var creation order and every
  // downstream id) follow heap layout across runs.
  static inline int id_counter = 1;
  int id = id_counter++;
  void *node;
  Vec<Dom *> pred, succ;

  int dfs, semi, size;
  Dom *label, *parent, *child, *ancestor, *idom;
  Vec<Dom *> children, bucket, front;
  Intervals intervals;

  int is_dominated_by(Dom *n)  // this is dominated by n
  {
    return intervals.in(n->dfs);
  }

  Dom(void *n);
};

template <> struct PointerHash<Dom *> {
  static uintptr_t hash(Dom *c) { return c ? (uintptr_t)c->id : 0; }
};

void build_dominators(Dom *g);
void build_cfg_dominators(Fun *f);
void build_call_dominators(FA *fa);

#endif
