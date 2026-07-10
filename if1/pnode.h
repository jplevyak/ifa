#ifndef _pn_H_
#define _pn_H_

class Code;
class Prim;
struct Dom;
struct LoopNode;
class Fun;
class MPosition;
class Sym;
class VarIdHashFns;  // var.h (issue 035)

/* #define CONC_IMPLEMENTED 1 */

// Program node

class PNode : public gc {
 public:
  Code *code;
  int id;
  uint live : 1;
  uint fa_live : 1;
  Vec<Var *> lvals;  // variables this node assigns
  Vec<Var *> rvals;  // variables this node reads
  Vec<Var *> tvals;  // temporary variables used by this node

  // Control-Flow Graph (CFG): cfg.cpp, ssu.cpp
  int mark;  // ssu.c
  Vec<PNode *> cfg_succ;
  Vec<PNode *> cfg_pred;
#ifdef CONC_IMPLEMENTED
  Vec<PNode *> conc_succ;
  Vec<PNode *> conc_pred;
#endif

  // Single-Static Value Functions
  Vec<PNode *> phi;  // MOVE nodes that logically follow this node
  Vec<PNode *> phy;  // MOVE nodes that logically precede this node

  Prim *prim;  // primitive

  // Temporary Space
  union {
    LoopNode *loop_node;                        // loop.cpp
    BlockHash<Var *, VarIdHashFns> *live_vars;  // ssu.cpp (id-hashed, issue 035)
  };
  Map<PNode *, int> cfg_pred_index;  // cg.cpp
  Dom *dom, *rdom;                   // dominators and reverse dominators dom.cpp

  Vec<Sym *> *creates;  // cloning

  float execution_frequency;
  float false_branch_frequency;  // inline.cpp

  PNode(Code *c);
  PNode();
};

int compar_pnodes(const void *ai, const void *aj);

// Content-based hashing for `Vec<PNode*>::set_add` / `set_in` (used
// extensively by cfg_succ/cfg_pred/phi/phy and codegen's visited-node
// tracking, e.g. cg.cc's write_c_pnode `done` set). `id` is monotonically
// assigned at construction (pnode.cc), so iteration/lookup is deterministic
// and collision-free across runs (see ifa/notes/004; like Var, this was a
// pointer-hashed id-bearing type missing from that landing's six).
template <> struct PointerHash<PNode *> {
  static uintptr_t hash(PNode *c) { return c ? (uintptr_t)c->id : 0; }
};

typedef Vec<PNode *> VecPNode;
typedef Map<PNode *, VecPNode> MapPNVecPN;

void pp(PNode *);

#endif
