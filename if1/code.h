#ifndef _code_h_
#define _code_h_

#include "ifadefs.h"

class Label;
class Prim;
class PNode;
class Sym;

namespace llvm {
  class BasicBlock;
}

enum Code_kind { Code_SUB = 0, Code_MOVE, Code_SEND, Code_IF, Code_LABEL, Code_GOTO, Code_SEQ, Code_CONC, Code_NOP };

enum Partial_kind { Partial_OK = 0, Partial_NEVER = 1, Partial_ALWAYS = 2 };

extern cchar *code_string[];

class Code;

class Code : public gc {
 public:
  Code_kind kind;
  Vec<Sym *> rvals;
  Vec<Sym *> lvals;
  Vec<cchar *> names;
  Label *label[2];
  Vec<Code *> sub;
  IFAAST *ast;
  Prim *prim;

  cchar *pathname();
  cchar *filename();
  int line();
  int source_line();  // prevent printing of headers by setting line number to 0

  unsigned int partial : 2;
  unsigned int live : 1;
  unsigned int flattened : 1;
  // ifa/issues/049: set by the frontend. `exc_exit` marks a GOTO to the
  // function's exit taken because an exception is pending (a raise, or a
  // propagated one): the exceptional exit, which produces no value. The
  // flow analysis does not follow it to the exit, so "reaches its exit"
  // means the normal return. `exc_check` marks the IF that tests for a
  // pending exception right after a call.
  unsigned int exc_exit : 1;
  unsigned int exc_check : 1;
  Code *cont;  // used by cfg.cpp
  PNode *pn;   // used by cfg.cpp

  Code(Code_kind k = Code_SUB) {
    memset((void*)this, 0, sizeof *this);
    kind = k;
  }
  Code(Code &c) {
    kind = c.kind;
    rvals.copy(c.rvals);
    lvals.copy(c.lvals);
    names.copy(c.names);
    label[0] = c.label[0];
    label[1] = c.label[1];
    sub.copy(c.sub);
    ast = c.ast;
    prim = c.prim;
    partial = c.partial;
    live = c.live;
    flattened = c.flattened;
    exc_exit = c.exc_exit;
    exc_check = c.exc_check;
    cont = c.cont;
    pn = c.pn;
  }
  int is_group() { return kind == Code_SUB || kind == Code_SEQ || kind == Code_CONC; }
};

class Label : public gc {
 public:
  int id;
  unsigned int live : 1;
  union {
    Code *code;  // used by cfg.cc
    llvm::BasicBlock *bb;  // used by llvm.cc
  };

  Label() { memset((void*)this, 0, sizeof *this); }
};

#endif
