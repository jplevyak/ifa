/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
struct D_Sym;
struct D_Scope;

class IF1;
class Sym;
class ParseAST;

typedef struct ParseSym {
  uint type_id : 1;
  D_Sym *sym;
  D_Scope *scope;
} ParseSym;

typedef struct ParseNode {
  ParseAST *ast;
  D_Sym *sym;
  D_Scope *saved_scope;
} V_ParseNode;

typedef struct Globals {
  IF1 *i;
  D_Scope *parallel_scope;
  int errors;
} Globals;

#define D_UserSym ParseSym
#define D_ParseNode_User ParseNode
#define D_ParseNode_Globals Globals
