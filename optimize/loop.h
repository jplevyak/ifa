#ifndef _loop_H_
#define _loop_H_

class FA;
class Fun;

struct LoopNode : public gc {
  int index;
  void *node;
  LoopNode *parent;
  Vec<LoopNode *> children;
  Vec<LoopNode *> loops;
  Vec<LoopNode *> pred;
  Vec<LoopNode *> succ;
  Vec<LoopNode *> dom_children;
  int pre_dfs, post_dfs;
  int pre_dom, post_dom;
  uint processed : 1;
  uint in_worklist : 1;
  int dfs_ancestor(LoopNode *);
  int dom_ancestor(LoopNode *);
  LoopNode(int i, void *n = 0);
};

struct LoopGraph : public gc {
  LoopNode *loops;
  LoopNode *entry;
  Vec<LoopNode *> nodes;
  Vec<Vec<LoopNode *> *> levels;
  UnionFind uf;

  void unify(LoopNode *n, LoopNode *m);
  LoopNode *find(LoopNode *n);

  LoopGraph();
};

void find_loops(LoopGraph *g);
// Per-Fun local-loop detection. `fa` is currently unused (the
// implementation reads only Fun state); callers from inside FA pass
// theirs, tests may pass NULL.
void find_local_loops(FA *fa, Fun *f);
void find_recursive_loops(FA *f);
void find_all_loops(FA *fa);

#endif
