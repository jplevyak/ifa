/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _dom_H
#define _dom_H

class Fun;

struct Dom : public gc {
  void *node;
  Vec<Dom *> pred, succ;

  int dfs, semi, size;
  Dom *label, *parent, *child, *ancestor, *idom;
  Vec<Dom *> children, bucket, front;
  Intervals intervals;

  int is_dominated_by(Dom *n) // this is dominated by n
    { return intervals.in(n->dfs); }

  Dom(void *n);
};
#define forv_Dom(_p, _v) forv_Vec(Dom, _p, _v)

void build_dominators(Dom *g);
void build_cfg_dominators(Fun *f);
void build_call_dominators(FA *fa);

#endif

