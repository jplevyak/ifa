/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _ast_to_if1_H
#define _ast_to_if1_H

#include <stdio.h>

#include "ast.h"
#include "map.h"
#include "scope.h"
#include "builtin.h"
#include "sym.h"
#include "ifa.h"

// C++'s manditory heap'o forward declarations
struct D_ParseNode;
class IF1;
class Code;
class Sym;
class Prim;
class Label;
class ParseAST;
class PNode;
class Fun;

class PCallbacks : public IFACallbacks {
public:
  void finalize_functions();
  void new_SUM_type(Sym *);
  Sym *new_Sym(cchar *name = 0);
};

// see ast_kinds.h for details
enum AST_kind {
#define S(_x) AST_##_x,
#include "ast_kinds.h"
#undef S
  AST_MAX
};

enum Constructor { Make_TUPLE, Make_VECTOR, Make_SET };
enum Intent { Intent_const, Intent_in, Intent_out, Intent_inout };

class ParseAST : public IFAAST {
 public:
  AST_kind kind;
  unsigned int scope_kind:2; // Scope_kind from scope.h
  unsigned int constructor:2;
  unsigned int intent:2;
  unsigned int def_ident_label:1;
  unsigned int op_index:1;
  unsigned int in_tuple:1;
  unsigned int in_apply:1;
  unsigned int is_assign:1;
  unsigned int is_simple_assign:1;
  unsigned int is_ref:1;
  unsigned int is_application:1;
  unsigned int is_comma:1;
  unsigned int is_inc_dec:1;
  unsigned int rank;
  ParseAST *parent;
  Vec<ParseAST *> children;
  Sym *sym;
  cchar *string;
  cchar *destruct_name;
  cchar *arg_name;
  cchar *builtin;
  Prim *prim;
  cchar *_pathname;
  int _line;
  Scope *scope;
  cchar *constant_type;
  Sym *container;
  Label *label[2];      // before and after for loops (continue,break)
  Code  *code;
  Sym   *rval;

  ParseAST *last() { return children[children.n-1]; }
  void add(ParseAST *a);
  void add(D_ParseNode *pn);
  void add_below(D_ParseNode *pn);
  void set_location(D_ParseNode *pn);
  void set_location_and_add(D_ParseNode *pn);
  ParseAST *get(AST_kind k);

  Sym *symbol() { return rval ? rval : sym; } 
  IFAAST *copy_tree(ASTCopyContext* context);
  IFAAST *copy_node(ASTCopyContext* context);

  cchar *pathname();
  int line();
  int source_line();
  void propagate(Vec<PNode *> *nodes);
  void html(FILE *fp, Fun *f);
  void graph(FILE *fp);

  ParseAST(AST_kind k, D_ParseNode *pn = 0);
};
#define forv_ParseAST(_x, _v) forv_Vec(ParseAST, _x, _v)

ParseAST *new_AST(AST_kind k, D_ParseNode *pn = 0);
Sym *new_sym(IF1 *i, Scope *scope = 0, cchar *s = 0, Sym *sym = 0);
void ast_print(FILE *fp, ParseAST *a, int indent = 0);
void ast_print_recursive(FILE *fp, ParseAST *a, int indent = 0);
int ast_gen_if1(IF1 *if1, Vec<ParseAST *> &av);
int ast_constant_fold(IF1 *if1, ParseAST *ast);
void convert_string_to_immediate(char *str, Sym *type, Immediate *imm);

extern cchar *AST_name[];

#endif
