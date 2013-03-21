/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _fun_H_
#define _fun_H_

class PDB;
class ESet;
class AVar;
struct LoopGraph;
struct LoopNode;
struct Dom;
class CDB_EntrySet;
class MPosition;
class EntrySet;
class Fun;
class Sym;
class PNode;
class CreationSet;
class Match;
class CoercionCache;
class PromotionCache;
class GenericCache;
class OrderCache;
class DefaultCache;
namespace llvm { class Function; }

#define FUN_COLLECT_VARS_NO_PHY        0x01
#define FUN_COLLECT_VARS_NO_TVALS      0x02

class CallPoint : public gc {
 public:
  Fun   *fun;
  PNode *pnode;
  CallPoint(Fun *afun, PNode *apnode) : fun(afun), pnode(apnode) {}
};
#define forv_CallPoint(_c, _v) forv_Vec(CallPoint, _c, _v)

// Functions

class Fun : public gc {
 public:
  int id;
  Sym *sym;
  IFAAST *ast;
  Fun *nested_in;
  Vec<Fun*> nested;
  
  // pnode
  PNode *entry;
  PNode *exit;

  uint is_generic : 1;
  uint is_external : 1;
  uint is_varargs : 1;
  uint is_eager : 1;   // will evaulate for Partial_OK
  uint is_lazy : 1;    // will not match Partial_NEVER
  
  // cdb
  int prof_id;
  Vec<int> prof_ess;
  Vec<CDB_EntrySet *> es_info;

  // fa
  uint fa_collected : 1;
  uint clone_for_constants : 1;
  uint split_unique : 1; // must be split uniquely for each caller
  uint split_eager : 1; // split eagerly for different argument types
  Vec<EntrySet *> ess;
  Vec<Var *> fa_Vars;
  Vec<Var *> fa_all_Vars;
  Vec<PNode *> fa_all_PNodes;
  Vec<PNode *> fa_move_PNodes;
  Vec<PNode *> fa_if_PNodes;
  Vec<PNode *> fa_phi_PNodes;
  Vec<PNode *> fa_phy_PNodes;
  Vec<PNode *> fa_send_PNodes;

  // pattern
  Vec<MPosition *> arg_positions;
  Vec<MPosition *> positional_arg_positions;
  Map<MPosition *, MPosition *> named_to_positional;
  Map<MPosition*, Sym*> arg_syms;
  Map<MPosition *, Var*> args;
  Vec<Var *> rets;
  Vec<MPosition *> out_positions;
  Map<MPosition *, IFAAST *> default_args;
  Vec<MPosition *> generic_args;
  PromotionCache *promotion_cache;
  CoercionCache *coercion_cache;
  GenericCache *generic_cache;
  OrderCache *order_cache;
  DefaultCache *default_cache;
  
  // clone
  Vec<EntrySet *> called_ess;
  Vec<EntrySet *> called_by_ess;
  Vec<CreationSet *> called_css;
  Vec<Vec<EntrySet *> *> equiv_sets;
  Map<PNode *, PNode*> *nmap;
  Map<Var *, Var*> *vmap;
  Map<Fun *, Fun*> *fmap;

  // clone typings and call graph
  Map<PNode *, Vec<Fun *> *> calls;
  void calls_funs(Vec<Fun *> &funs);
  Vec<CallPoint *> called;
  void called_by_funs(Vec<Fun *> &funs);

  // wrappers and instantiations
  Fun *wraps;

  // loop
  LoopGraph *loops;
  LoopNode *loop_node;
  Dom *dom;
  
  // inline
  float execution_frequency;
  int size;
  
  // cg
  uint live:1;
  char *cg_string;
  char *cg_structural_string;

  // llvm
  llvm::Function *llvm;
  
  cchar *pathname();
  cchar *filename();
  int line();
  int source_line();
  
  void collect_PNodes(Vec<PNode *> &v);
  void collect_Vars(Vec<Var *> &v, Vec<PNode *> *vv = 0, int flags = 0);

  void build_cfg();
  void build_ssu();
  void setup_ast();
  void init_fun();

  Fun(Sym *afn);
  Fun();
  Fun *copy(int copy_ast = 1, Map<Var *, Var *> *var_map = 0);
};
#define forv_Fun(_f, _v) forv_Vec(Fun, _f, _v)

int compar_funs(const void *ai, const void *aj);
void rebuild_cfg_pred_index(Fun *);

#endif
