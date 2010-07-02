/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _ast_H_
#define _ast_H_

#include <stdio.h>
#include "ifadefs.h"
#include "ifa.h"

class Fun;
class PNode;
class Sym;
class IF1;
class Var;
class IFACallbacks;

class ASTCopyContext : public gc {
 public:
  Map<Fun *, Fun *> *fmap;
  Map<PNode *, PNode *> *nmap;
  Map<Var *, Var *> *vmap;
  Map<Sym *, Sym *> smap;
  ASTCopyContext() : fmap(0), nmap(0), vmap(0) {}
};

void init_ast(IFACallbacks *callbacks); // called by ifa_init
void init_default_builtin_types(); // will not overwrite/modify pre-set builtin types 
void finalize_types(IF1 *, int import_included_ivars = true);
void build_type_hierarchy(int compute_structural_value_hierarchy = 0);

void unalias_sym(Sym *s);
void make_meta_type(Sym *s);

void new_module(Sym *&sym, Sym *init_fun);
void new_builtin_symbol(Sym *&sym, cchar *name, cchar *builtin_name = 0);
void new_builtin_primitive_type(Sym *&sym, cchar *name, cchar *builtin_name = 0);
void new_builtin_alias_type(Sym *&sym, cchar *name, Sym *alias, cchar *builtin_name = 0);
void new_builtin_global_variable(Sym *&sym, cchar *name, cchar *builtin_name = 0);
void new_builtin_unique_object(Sym *&sym, cchar *name, Sym *sym_type, cchar *builtin_name = 0);
void new_builtin_lub_type(Sym *&sym, cchar *name, cchar *builtin_name, ...);

#endif
