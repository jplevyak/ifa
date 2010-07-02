/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "scope.h"
#include "sym.h"

void 
Scope::add_dynamic(Scope *s, Sym *sy) {
  dynamic.add(s);
  dynamic_container.add(sy);
}

static Sym *
get_dynamic(Scope *scope, Sym *scope_container, cchar *name, Sym **container) {
  Sym *s;
  if ((s = scope->hash.get(name))) {
    if (container && scope_container)
      *container = scope_container;
    return s;
  }
  for (int i = 0; i < scope->dynamic.n; i++)
    if ((s = get_dynamic(scope->dynamic[i], scope->dynamic_container.v[i], name, container))) {
      if (container && scope_container)
        *container = scope_container;
      return s;
    }
  return NULL;
}

Sym *
Scope::get_local(cchar *name, Sym **container) {
  Sym *s;
  if ((s = hash.get(name))) {
    if (container)
      *container = 0;
    return s;
  }
  for (int i = 0; i < dynamic.n; i++)
    if ((s = get_dynamic(dynamic[i], dynamic_container.v[i], name, container)))
      return s;
  return NULL;
}

Sym *
Scope::get(cchar *name, Sym **container) {
  Sym *s;
  if ((s = get_local(name, container)))
    return s;
  if (up)
    return up->get(name, container);
  return NULL;
}

Scope *
Scope::global() {
  Scope *s = this;
  while (s->up) s = s->up;
  return s;
}

Scope *
Scope::module() {
  Scope *s = this;
  while (s->up->up) s = s->up;
  return s;
}

Scope *
Scope::function() {
  Scope *s = this;
  while (s->up->up && (!s->in || !s->in->is_fun)) s = s->up;
  if (s->in->is_module)
    s = s->in->init->scope;
  return s;
}

void
Scope::put(cchar *name, Sym *s) {
  MapElem<cchar *, Sym *> e(name, NULL);
  MapElem<cchar *, Sym *> *x = hash.set_in(e);
  if (x) {
    x->value = s;
  } else
    hash.put(name, s);
}
