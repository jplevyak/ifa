/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#ifndef _scope_h_
#define _scope_h_

class Sym;

// must match the corresponding D_SCOPE_XXX if the DParser is used
enum Scope_kind { Scope_INHERIT, Scope_RECURSIVE, Scope_PARALLEL, Scope_SEQUENTIAL };

class Scope : public gc {
 public:
  unsigned int kind : 2;
  Map<cchar *, Sym *> hash;
  Scope *up;
  Scope *next;

  Sym *in;

  Sym *get(cchar *name, Sym **container = 0);
  Sym *get_local(cchar *name, Sym **container = 0);
  void put(cchar *name, Sym *s);

  void add_dynamic(Scope *s, Sym *sy = 0);

  Scope *global();
  Scope *module();
  Scope *function();

  Scope() : kind(Scope_RECURSIVE), up(0), next(0), in(NULL) {}
  Scope(Scope *s, unsigned int k, Sym *an_in) : up(s), next(0), in(an_in) {
    if (k == Scope_INHERIT)
      kind = up->kind;
    else
      kind = k;
  }

  // private
  Vec<Scope *> dynamic;
  Vec<Sym *> dynamic_container;
};
#define forv_Scope(_x, _v) forv_Vec(Scope, _x, _v)

#endif
