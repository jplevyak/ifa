/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#ifndef _pn_H_
#define _pn_H_

class Code;
class Prim;
class Dom;
class LoopNode;
class Fun;
class MPosition;
class Sym;

/* #define CONC_IMPLEMENTED 1 */

// Program node

class PNode : public gc { public:
  Code *code;
  int id;
  uint live : 1;
  Vec<Var *> lvals; // variables this node assigns
  Vec<Var *> rvals; // variables this node reads
  Vec<Var *> tvals; // temporary variables used by this node

  // Control-Flow Graph (CFG): cfg.cpp, ssu.cpp 
  int mark; // ssu.c
  Vec<PNode *> cfg_succ;
  Vec<PNode *> cfg_pred;
#ifdef CONC_IMPLEMENTED
  Vec<PNode *> conc_succ;
  Vec<PNode *> conc_pred;
#endif

  // Single-Static Value Functions
  Vec<PNode *> phi; // MOVE nodes that logically follow this node
  Vec<PNode *> phy; // MOVE nodes that logically precede this node

  Prim *prim; // primitive

  // Temporary Space
  union {
    LoopNode *loop_node; // loop.cpp
    BlockHash<Var *, PointerHashFns> *live_vars; // ssu.cpp
  };
  Map<PNode *, int> cfg_pred_index; // cg.cpp
  Dom *dom, *rdom; // dominators and reverse dominators dom.cpp 

  Vec<Sym *> *creates; // cloning

  float execution_frequency;
  float false_branch_frequency; // inline.cpp

  PNode(Code *c);
  PNode();
};
#define forv_PNode(_p, _v) forv_Vec(PNode, _p, _v)

int compar_pnodes(const void *ai, const void *aj);

typedef Vec<PNode *> VecPNode;
typedef Map<PNode *, VecPNode> MapPNVecPN;

void pp(PNode *);

#endif

