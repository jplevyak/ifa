#ifndef _ifa_H_
#define _ifa_H_

#include "common.h"

class Sym;
class Match;
class Fun;
class ATypeViolation;
class ASTCopyContext;
class PNode;
class MPosition;
class AVar;
class AType;
class EntrySet;

/*
  Interface object between analysis symbols (Sym) and front end symbols
  Typically the front-end specific subclass will contain a pointer to the front
  end symbol
*/
class IFASymbol : public gc {
 public:
  virtual cchar *pathname() = 0;
  virtual int column() { return 0; }
  virtual int line() = 0;         // source line number (0 if none)
  virtual int source_line() = 0;  // user source line (0 if builtin/system)
  virtual IFASymbol *copy() = 0;
  virtual Sym *clone() { return copy()->sym; }

  Sym *sym;

  IFASymbol() : sym(0) {}
};

/*
  Interface object between analysis to front end AST nodes
  Typically the front-end specific subclass will contain a pointer to the front
  end AST node
*/
class IFAAST : public gc {
 public:
  Vec<PNode *> pnodes;

  virtual cchar *pathname() = 0;
  virtual int column() { return 0; }
  virtual int line() = 0;
  virtual int source_line() = 0;
  virtual Sym *symbol() = 0;
  virtual Vec<Fun *> *visible_functions(Sym *arg0) { return NULL; }  // NULL == ALL
  virtual IFAAST *copy_tree(ASTCopyContext *context) = 0;
  virtual IFAAST *copy_node(ASTCopyContext *context) = 0;
  virtual void html(FILE *fp, Fun *f) {}
  virtual void graph(FILE *fp) {}  // calling graph_node/graph_edge in graph.h
};

/*
  Interface for callbacks from the analysis core to the front end specific
  translator
*/
class IFACallbacks : public gc {
 public:
  virtual void finalize_functions() {}
  virtual Sym *new_Sym(cchar *name) = 0;  // { return (new IFASymbol)->sym; }
  virtual Sym *make_LUB_type(Sym *s) { return s; }
  virtual int formal_to_generic(Sym *s, Sym **ret_generic, int *ret_bind_to_value) { return false; }
  virtual Sym *instantiate(Sym *, Map<Sym *, Sym *> &substitutions) { return 0; }
  virtual Fun *order_wrapper(Fun *, Map<MPosition *, MPosition *> &substitutions) { return 0; }
  virtual Sym *promote(Fun *, Sym *, Sym *, Sym *) { return NULL; }
  virtual Fun *promotion_wrapper(Fun *, Map<MPosition *, Sym *> &substitutions) { return 0; }
  virtual Sym *coerce(Sym *actual, Sym *formal) { return NULL; }
  virtual Fun *coercion_wrapper(Fun *, Map<MPosition *, Sym *> &substitutions) { return 0; }
  virtual Fun *default_wrapper(Fun *, Vec<MPosition *> &defaults) { return 0; }
  virtual Fun *instantiate_generic(Fun *, Map<Sym *, Sym *> &substitutions) { return 0; }
  virtual bool reanalyze(Vec<ATypeViolation *> &type_violations) { return false; }
  // Called by FA's own P_prim_isinstance transfer function
  // (analysis/fa.cc), before it falls back to its normal
  // CreationSet-intersection logic, for every isinstance() check FA
  // analyzes. Lets a frontend fold a check FA itself structurally
  // cannot -- not because of a type-inference gap, but because the
  // imprecision comes from language/runtime-specific knowledge FA
  // has no notion of (e.g. pyc's exception-propagation slot, whose
  // whole-program-shared-global imprecision is a known, deliberate
  // FA limitation -- see ifa/issues/031). `operand_av` is the
  // checked value's AVar (`thing1` in the transfer function),
  // `es` the current contour, `send_pnode` the isinstance SEND
  // itself (for callers that need call-site context via
  // `send_pnode->code->ast` and `es->out_edges`). Return
  // `fa->type_world.true_type`/`false_type` to force that result, or
  // nullptr (the default) for "no opinion, use FA's normal logic" --
  // the overwhelming majority of isinstance() checks in any program,
  // including nearly all of pyc's own, take this path unchanged.
  // Must be conservative: an unrecognized pattern or unresolved call
  // info returns nullptr, never a forced (and possibly wrong) type.
  virtual AType *provably_constant_isinstance(AVar *operand_av, EntrySet *es, PNode *send_pnode) { return nullptr; }
  virtual void report_analysis_errors(Vec<ATypeViolation *> &type_violations) {}
  virtual bool c_codegen_pre_file(FILE *fp) { return false; }
};

void ifa_init(IFACallbacks *callbacks);
// Tear down all process-wide IFA state so the next ifa_init() starts
// fresh. Lets a single process run multiple isolated analyses
// (primarily for the test harness). Discards the current IF1/PDB/FA,
// clears canonicalization tables and worklists in fa.cc, resets
// pattern.cc and ast.cc state, and nulls all sym_* global pointers
// (`init_ast` will recreate them on the next init).
void ifa_reset();
int ifa_analyze(cchar *fn);
int ifa_optimize();
void ifa_cg(cchar *fn);
void ifa_compile(cchar *fn);

enum GraphType { GraphViz, VCG };
extern int graph_type;
void ifa_graph(cchar *fn);
void ifa_html(cchar *fn, cchar *mktree_dir);
void ifa_code(cchar *fn);

extern bool fgraph_pass_contours;
extern bool fdce_if1;
extern bool fruntime_errors;

#include "ifadefs.h"

#endif
