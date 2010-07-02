/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
   
   Public interface classes and functions for Iterative Flow Analysis (IFA)
*/
#ifndef _ifa_H_
#define _ifa_H_

#include "plib.h"

class Sym;
class Match;
class Fun;
class ATypeViolation;
class ASTCopyContext;
class PNode;
class MPosition;

/*
  Interface object between analysis symbols (Sym) and front end symbols
  Typically the front-end specific subclass will contain a pointer to the front end symbol
*/
class IFASymbol : public gc {
 public:
  virtual cchar *pathname() = 0;
  virtual int line() = 0; // source line number (0 if none)
  virtual int source_line() = 0; // user source line (0 if builtin/system)
  virtual IFASymbol *copy() = 0;
  virtual Sym *clone() { return copy()->sym; }

  Sym *sym;

  IFASymbol() : sym(0) {}
};

/*
  Interface object between analysis to front end AST nodes
  Typically the front-end specific subclass will contain a pointer to the front end AST node
*/
class IFAAST : public gc {
 public:
  Vec<PNode *> pnodes;

  virtual cchar *pathname() = 0;
  virtual int line() = 0;
  virtual int source_line() = 0;
  virtual Sym *symbol() = 0;
  virtual Vec<Fun *> *visible_functions(Sym *arg0) { return NULL; }  // NULL == ALL
  virtual IFAAST *copy_tree(ASTCopyContext* context) = 0;
  virtual IFAAST *copy_node(ASTCopyContext* context) = 0;
  virtual void html(FILE *fp, Fun *f) {}
  virtual void graph(FILE *fp) {} // calling graph_node/graph_edge in graph.h
};
#define forv_IFAAST(_x, _v) forv_Vec(IFAAST, _x, _v)

/*
  Interface for callbacks from the analysis core to the front end specific translator
*/
class IFACallbacks : public gc {
public:
  virtual void finalize_functions() {}
  virtual Sym *new_Sym(cchar *name) = 0; // { return (new IFASymbol)->sym; }
  virtual Sym *make_LUB_type(Sym *s) { return s; }
  virtual int formal_to_generic(Sym *s, Sym **ret_generic, int *ret_bind_to_value) { return false; }
  virtual Sym *instantiate(Sym *, Map<Sym *, Sym *> &substitutions) { return 0; }
  virtual Fun* order_wrapper(Fun *, Map<MPosition *, MPosition *> &substitutions) { return 0; }
  virtual Sym *promote(Fun *, Sym *, Sym *, Sym *) { return NULL; }
  virtual Fun* promotion_wrapper(Fun *, Map<MPosition *, Sym *> &substitutions) { return 0; }
  virtual Sym *coerce(Sym *actual, Sym *formal) { return NULL; }
  virtual Fun* coercion_wrapper(Fun *, Map<MPosition *, Sym *> &substitutions) { return 0; }
  virtual Fun* default_wrapper(Fun *, Vec<MPosition *> &defaults) { return 0; }
  virtual Fun* instantiate_generic(Fun *, Map<Sym *, Sym*> &substitutions) { return 0; }
  virtual void report_analysis_errors(Vec<ATypeViolation*> &type_violations) { }
};

void ifa_init(IFACallbacks *callbacks);
int ifa_analyze(cchar *fn);
int ifa_optimize();
void ifa_cg(cchar *fn);
void ifa_compile(cchar *fn);

enum GraphType { GraphViz, VCG };
extern int graph_type;
void ifa_graph(cchar *fn);
void ifa_html(cchar *fn, char *mktree_dir);

extern int print_call_depth;
extern int fanalysis_errors;
extern int fgraph_pass_contours;
extern int fdce_if1;

#include "ifadefs.h"

#endif
