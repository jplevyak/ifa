/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "ast.h"
#include "sym.h"
#include "builtin.h"
#include "if1.h"
#include "pattern.h"
#include "fa.h"

static int type_hierarchy_built = 0;
static int finalized_types = 0;

void
new_module(Sym *&sym, Sym *fun) {
  if (!sym)
    sym = new_Sym();
  sym->type = sym_module;
  sym->is_module = 1;
  sym->init = fun;
}

void
new_builtin_symbol(Sym *&sym, cchar *name, cchar *builtin_name) {
  if (!sym)
    sym = if1_make_symbol(if1, name);
  if1_set_builtin(if1, sym, builtin_name ? builtin_name : name);
}

void
new_builtin_primitive_type(Sym *&sym, cchar *name, cchar *builtin_name)  {
  name = if1_cannonicalize_string(if1, name);
  if (!sym) {
    sym = new_Sym(name);
    sym->type_kind = Type_PRIMITIVE;
  }
  if1_set_builtin(if1, sym, builtin_name ? builtin_name : name);
}

void
new_builtin_alias_type(Sym *&sym, cchar *name, Sym *alias, cchar *builtin_name) {
  if (!sym) {
    sym = new_Sym(name);
    sym->type_kind = Type_ALIAS;
    sym->alias = alias;
  }
  if1_set_builtin(if1, sym, builtin_name ? builtin_name : name);
}

void 
new_builtin_global_variable(Sym *&sym, cchar *name, cchar *builtin_name) {
  if (!sym) {
    sym = new_Sym(name);
    sym->nesting_depth = 0;
  }
  if1_set_builtin(if1, sym, builtin_name ? builtin_name : name);
}

void
new_builtin_unique_object(Sym *&sym, cchar *name, Sym *sym_type, cchar *builtin_name) {
  bool predefined = !!sym;
  new_builtin_global_variable(sym, name, builtin_name);
  if (!predefined) {
    sym->type_kind = Type_NONE;
    sym->type = sym_type;
    sym->is_external = 1;
  }
}

void
new_builtin_lub_type(Sym *&sym, cchar *name, cchar *builtin_name, ...)  {
  bool predefined = !!sym;
  if (!sym) {
    sym = new_Sym(name);
    sym->type_kind = Type_SUM;
  }
  if1_set_builtin(if1, sym, builtin_name ? builtin_name : name);
  if (!predefined) {
    va_list ap;
    va_start(ap, builtin_name);
    Sym *s = 0;
    do {
      if ((s = va_arg(ap, Sym*)))
        sym->has.add(s);
    } while (s);
    forv_Sym(ss, sym->has)
      ss->inherits_add(sym);
  }
}

static void init_builtin_symbols() {
  new_builtin_symbol(sym_primitive, "__primitive", "primitive");
  new_builtin_symbol(sym_reply, "reply");
  new_builtin_symbol(sym_make_tuple, "make_tuple");
  new_builtin_symbol(sym_make_vector, "make_vector");
  new_builtin_symbol(sym_make_list, "make_list");
  new_builtin_symbol(sym_make_continuation, "__make_continuation", "make_continuation");
  new_builtin_symbol(sym_make_set, "make_set");
  new_builtin_symbol(sym_len, "len");
  new_builtin_symbol(sym_new, "new");
  new_builtin_symbol(sym_clone, "clone");
  new_builtin_symbol(sym_index_object, "index_object");
  new_builtin_symbol(sym_set_index_object, "set_index_object");
  new_builtin_symbol(sym_operator, "__operator", "operator");
  new_builtin_symbol(sym_destruct, "destruct");
  new_builtin_symbol(sym_meta_apply, "__meta_apply", "meta_apply");
  new_builtin_symbol(sym_period, ".", "period");
  new_builtin_symbol(sym_setter, ".=", "setter");
  new_builtin_symbol(sym_assign, "=", "assign");
  new_builtin_symbol(sym_coerce, "coerce");
  new_builtin_symbol(sym_merge, "merge");
  new_builtin_symbol(sym_merge_in, "merge_in");
  new_builtin_symbol(sym_type_assert, "type_assert");
  new_builtin_symbol(sym_exclamation, "!", "exclamation");
}

void init_default_builtin_types() {
  new_builtin_global_variable(sym_system, "system");
  new_builtin_global_variable(sym_new_object, "new_object");

  new_builtin_primitive_type(sym_symbol, "symbol");
  new_builtin_lub_type(sym_any, "any", 0, 0);
  new_builtin_primitive_type(sym_module, "module");
  new_builtin_primitive_type(sym_function, "function");
  new_builtin_primitive_type(sym_closure, "closure");
  new_builtin_primitive_type(sym_continuation, "continuation");
  new_builtin_primitive_type(sym_vector, "vector");
  new_builtin_primitive_type(sym_list, "list");
  new_builtin_primitive_type(sym_tuple, "tuple");
  new_builtin_primitive_type(sym_nil_type, "nil_type");
  new_builtin_primitive_type(sym_unknown_type, "unknown_type");
  new_builtin_primitive_type(sym_void_type, "void_type");
  new_builtin_primitive_type(sym_ref, "ref");
  new_builtin_primitive_type(sym_value, "value");
  new_builtin_primitive_type(sym_set, "set");

  new_builtin_primitive_type(sym_object_pointer, "object_pointer");
  new_builtin_primitive_type(sym_object, "object");
  sym_object->type_kind = Type_RECORD;
  new_builtin_primitive_type(sym_bool, "bool");

  new_builtin_primitive_type(sym_int8,   "int8");
  new_builtin_primitive_type(sym_int16,  "int16");
  new_builtin_primitive_type(sym_int32,  "int32");
  new_builtin_primitive_type(sym_int64,  "int64");

  new_builtin_primitive_type(sym_uint8,  "uint8");
  new_builtin_primitive_type(sym_uint16, "uint16");
  new_builtin_primitive_type(sym_uint32, "uint32");
  new_builtin_primitive_type(sym_uint64, "uint64");

  new_builtin_primitive_type(sym_float32, "float32");
  new_builtin_primitive_type(sym_float64, "float64");
  new_builtin_primitive_type(sym_float128, "float128");

  new_builtin_primitive_type(sym_complex32, "complex32");
  new_builtin_primitive_type(sym_complex64, "complex64");
  new_builtin_primitive_type(sym_complex128, "complex128");

  new_builtin_lub_type(sym_anyint, "anyint", 0, sym_int8, sym_uint8, sym_int16, sym_uint16,
                       sym_int32, sym_uint32, sym_int64, sym_uint64, 0);

  new_builtin_lub_type(sym_anyfloat, "anyfloat", 0, sym_float32, sym_float64, sym_float128, 0);
  new_builtin_lub_type(sym_anycomplex, "anycomplex", 0, sym_complex32, sym_complex64, sym_complex128, 0);
  new_builtin_lub_type(sym_anynum, "anynum", 0, sym_anyint, sym_anyfloat, sym_anycomplex, 0);

  new_builtin_primitive_type(sym_string, "string");

  if1_set_primitive_types(if1);
  
  new_builtin_alias_type(sym_char, "char", sym_uint8);
  new_builtin_alias_type(sym_uint, "uint", sym_uint64);
  new_builtin_alias_type(sym_int, "int", sym_int64);
  new_builtin_alias_type(sym_float, "float", sym_float64);
  new_builtin_alias_type(sym_complex, "complex", sym_complex64);
  new_builtin_alias_type(sym_size, "size", sym_int64);
  new_builtin_alias_type(sym_enum_element, "enum_element", sym_int64);

  make_meta_type(sym_any);
  sym_anytype = sym_any->meta_type;
  new_builtin_primitive_type(sym_anytype, "anytype");
  sym_anytype->implements.add(sym_any);
  sym_anytype->specializes.add(sym_any);

  make_meta_type(sym_nil_type);
  sym_nil_type->implements.add(sym_object_pointer);
  sym_nil_type->specializes.add(sym_object_pointer);

  sym_any->implements.add(sym_unknown_type);
  sym_any->specializes.add(sym_unknown_type);
  sym_object_pointer->implements.add(sym_any);
  sym_object_pointer->specializes.add(sym_any);
  sym_object->implements.add(sym_object_pointer);
  sym_object->specializes.add(sym_object_pointer);
  sym_value->implements.add(sym_any);
  sym_value->specializes.add(sym_any);

  sym_function->implements.add(sym_closure);
  sym_function->specializes.add(sym_closure);

  sym_any->is_system_type = 1;
  sym_anytype->is_system_type = 1;
  sym_value->is_system_type = 1;
  sym_object->is_system_type = 1;
  sym_nil_type->is_system_type = 1;
  sym_unknown_type->is_system_type = 1;
  sym_void_type->is_system_type = 1;

  new_builtin_unique_object(sym_nil, "nil", sym_nil_type);
  sym_nil_type->is_unique_type = 1;
  new_builtin_unique_object(sym_empty_list, "empty_list", sym_list);
  new_builtin_unique_object(sym_empty_tuple, "empty_tuple", sym_tuple);
  new_builtin_unique_object(sym_void, "void", sym_void_type);
  sym_void_type->is_unique_type = 1;
  new_builtin_unique_object(sym_unknown, "unknown", sym_unknown_type);
  sym_unknown_type->is_unique_type = 1;

  if (!sym_true) {
    Immediate imm;
    imm.v_bool = 1;
    sym_true = if1_const(if1, sym_bool, "true", &imm);
    sym_true->name = sym_true->constant;
  }
  if1_set_builtin(if1, sym_true, "true");
  if (!sym_false) {
    Immediate imm;
    imm.v_bool = 0;
    sym_false = if1_const(if1, sym_bool, "false", &imm);
    sym_false->name = sym_false->constant;
  }
  if1_set_builtin(if1, sym_false, "false");
}

void init_ast(IFACallbacks *callbacks) {
  if1->callback = callbacks;
  init_builtin_symbols();
}

void unalias_sym(Sym *s) {
  Sym *us = unalias_type(s);
  if (s != us) {
    forv_Sym(ss, s->specializes) if (ss) {
      assert(ss != us);
      us->specializes.add(ss);
    }
    forv_Sym(ss, s->includes) if (ss) {
      assert(ss != us);
      us->includes.add(ss);
    }
    forv_Sym(ss, s->implements) if (ss) {
      assert(ss != us);
      us->implements.add(ss);
    }
  }
  if (s->type_kind) {
    for (int x = 0; x < s->specializes.n; x++)
      s->specializes[x] = unalias_type(s->specializes.v[x]);
    for (int x = 0; x < s->includes.n; x++)
      s->includes[x] = unalias_type(s->includes.v[x]);
    for (int x = 0; x < s->implements.n; x++)
      s->implements[x] = unalias_type(s->implements.v[x]);
  }
  if (s->must_specialize)
    s->must_specialize = unalias_type(s->must_specialize);
  if (s->must_implement)
    s->must_implement = unalias_type(s->must_implement);
  s->type = unalias_type(s->type);
  s->in = unalias_type(s->in);
}

static void
implement(Sym *s, Sym *ss, Accum<Sym *> &types) {
  if (!ss->implements.in(s))
    ss->implements.add(s);
  s->implementors.set_add(ss);
  types.add(s);
  types.add(ss);
}

static void
specialize(Sym *s, Sym *ss, Accum<Sym *> &types) {
  assert(s != ss);
  if (!ss->specializes.in(s))
    ss->specializes.add(s);
  s->specializers.set_add(ss);
  types.add(s);
  types.add(ss);
}

static void
implement_and_specialize(Sym *s, Sym *ss, Accum<Sym *> &types) {
  implement(s, ss, types);
  specialize(s, ss, types);
}

static int
related(Sym *x, Sym *y) {
  return (x == y || 
          x->instantiates == y ||
          y->instantiates == x ||
          (x->instantiates && x->instantiates == y->instantiates));
}

#define E(_x) ((Vec<const void *>*)(_x)->temp)

static void 
compute_single_structural_type_hierarchy(Vec<Sym *> types, int is_union) {
  Vec<Vec<Sym *>*> by_size;
  // collect by size & build set of elements
  forv_Sym(s, types) {
    by_size.fill(s->has.n + 1);
    if (!by_size[s->has.n])
      by_size[s->has.n] = new Vec<Sym *>;
    by_size[s->has.n]->add(s);
    s->temp = (void*)new Vec<void *>;
    for (int i = 0; i < s->has.n; i++) {
      if (s->has[i]->name)
        E(s)->set_add(s->has[i]->name);
      else
        E(s)->set_add(int2Position(i));
    }
  }
  // naive n**2 algorithm
  for (int i = 1; i < by_size.n; i++) {
    if (by_size[i]) {
      forv_Sym(s, *by_size[i]) {
        for (int j = i; j < by_size.n; j++) {
          if (by_size[j]) forv_Sym(ss, *by_size.v[j]) {
            if (!related(s, ss)) {
              if (!E(s)->some_difference(*E(ss))) {
                for (int ii = 0; ii < s->has.n; ii++)
                  for (int jj = 0; jj < ss->has.n; jj++)
                    if (s->has[ii]->name == ss->has.v[jj]->name &&
                        s->has[ii]->type != ss->has.v[jj]->type)
                      goto Lskip;
                if (!is_union)
                  s->specializers.set_add(ss);
                else
                  ss->specializers.set_add(s);
              Lskip:;
              }
            }
          }
        }
      }
    }
  }
  // handle empty records
  if (by_size.n && by_size[0]) {
    forv_Sym(s, *by_size[0]) {
      forv_Sym(ss, types) {
        if (!related(s, ss))
          s->specializers.set_add(ss);
      }
    }
  }
  // clear temp storage
  forv_Sym(s, types)
    s->temp = 0;
}
#undef E

static void 
compute_structural_type_hierarchy(Accum<Sym *> types) {
  Vec<Sym *> record_types, union_types;
  forv_Sym(s, types.asvec) if (s) {
    if (s->type_kind == Type_RECORD && s->is_value_type) {
      if (!s->is_union_type)
        record_types.add(s);
      else
        union_types.add(s);
    }
  }
  compute_single_structural_type_hierarchy(record_types, 0);
  compute_single_structural_type_hierarchy(union_types, 1);
}

static void
add_implementor(Sym *s, Sym *ss) {
  if (ss->implementors.set_add(s))
    forv_Sym(x, ss->implements)
      add_implementor(s, x);
}

static void
add_specializer(Sym *s, Sym *ss) {
  if (ss->specializers.set_add(s))
    forv_Sym(x, ss->specializes)
      add_specializer(s, x);
}

/*
  C3 linearization to determine method resolution order from:
  "A Monotonic Superclass Linearization for Dylan",
    by Kim Barrett, Bob Cassel, Paul Haahr,
    David A. Moon, Keith Playford, and P. Tucker Withington.
    (OOPSLA 1996)
*/
typedef Vec<Sym *> VSym;

static Sym *candidate(Sym *c, Vec<VSym *> &todo) {
  forv_Vec(VSym, y, todo)
    if (y->index(c) > 0)
      return 0;
  return c;
}

static void merge(Vec<Sym *> &rdone, Vec<VSym*> &todo) {
  forv_Vec(VSym, x, todo)
    if (x->n) goto Lnotdone;
  rdone.reverse();
  return;
Lnotdone:;
  Sym *c = 0;
  forv_Vec(VSym, y, todo)
    if (y->n && (c = candidate(y->v[0], todo)))
      break;
  if (c) {
    forv_Vec(VSym, y, todo)
      if (y->n && y->v[0] == c) 
        y->remove_index(0);
    if (!rdone.in(c))
      rdone.insert(0, c);
    merge(rdone, todo);
  } else
    fail("inconsistent precedence graph in C3 linearization");
}

static void
c3_linearization(Sym *c) {
  if (c->dispatch_types_built)
    return;
  c->dispatch_types_built = 1;
  c->dispatch_types.clear();
  if (!c->specializes.n)
    return;
  Vec<Sym*> rdone;
  Vec<Vec<Sym*>*> todo;
  rdone.add(c);
  forv_Sym(x, c->specializes) {
    c3_linearization(x);
    todo.add(new Vec<Sym *>(x->dispatch_types));
  }
  todo.add(new Vec<Sym *>(c->specializes));
  merge(rdone, todo);
  c->dispatch_types.move(rdone);
}

void
build_type_hierarchy(int compute_structural_value_hierarchy) {
  Accum<Sym *> types, meta_types;
  Vec<Sym *> new_types;
  for (int x = type_hierarchy_built; x < if1->allsyms.n; x++) {
    Sym *s = if1->allsyms[x];
    if (s->type_kind) {
      new_types.add(s);
      types.add(s);
    }
    // constants act as point types
    if (s->is_symbol) {
      implement_and_specialize(sym_symbol, s, types);
      s->type = sym_symbol;
      s->meta_type = s;
    }
    if (s->is_fun)
      implement_and_specialize(sym_function, s, types);
    if (s->is_constant || s->is_symbol || s->is_fun)
      s->must_implement_and_specialize(s);
    
    forv_Sym(ss, s->implements)
      implement(ss, s, types);
    forv_Sym(ss, s->specializes)
      specialize(ss, s, types);
    // functions implement and specializes of the initial symbol in their pattern
    // which may be a constant or a constant constrainted variable
    // this permits overloading of selectors with multiple functions
    if (s->is_fun && s->has.n) {
      Sym *a = s->has[0];
      if (a->is_symbol)
        implement_and_specialize(a, s, types);
      else if (a->must_specialize && a->must_specialize->is_symbol)
        implement_and_specialize(a->must_specialize, s, types);
    }
    if (s->instantiates) 
      implement_and_specialize(s->instantiates, s, types);
  }
  qsort_by_id(types.asvec);
  forv_Sym(s, new_types)
    s->must_implement_and_specialize(s);
  forv_Sym(s, types.asvec) if (s) {
    s->implementors.set_add(s);
    s->specializers.set_add(s);
  }
  // compute structural type hierarchy
  if (compute_structural_value_hierarchy) 
    compute_structural_type_hierarchy(types);
  // map subtyping and subclassing to meta_types
  forv_Sym(s, types.asvec) if (!s->is_meta_type) {
    forv_Sym(ss, s->implementors) if (ss && s->meta_type != ss->meta_type)
      implement(s->meta_type, ss->meta_type, meta_types);
    forv_Sym(ss, s->specializers) if (s != sym_any && ss && s->meta_type != ss->meta_type)
      specialize(s->meta_type, ss->meta_type, meta_types);
  }
    
  forv_Sym(s, types.asvec) if (s) {
    if (!s->is_system_type) {
      if (s->is_meta_type)
        implement_and_specialize(sym_anytype, s, types);
      else if (s->is_value_type)
        implement_and_specialize(sym_value, s, types);
      else 
        implement_and_specialize(sym_any, s, types);
    }
  }
  // compute implementor/specializers closure
  Vec<Sym *> todo, changed, next_changed;
  types.asset.set_union(meta_types.asset);
  types.asvec.clear();
  types.asvec.append(types.asset);
  // build implementors/specializers
  forv_Sym(s, new_types) {
    forv_Sym(x, s->implements)
      forv_Sym(y, x->implements)
        add_implementor(s, y);
    forv_Sym(x, s->specializes)
      forv_Sym(y, x->specializes)
        add_specializer(s, y);
  }
  // linearize classes for dispatch
  for (int x = type_hierarchy_built; x < if1->allsyms.n; x++)
    c3_linearization(if1->allsyms[x]);
  type_hierarchy_built = if1->allsyms.n;
}

static void
collect_includes(Sym *s, Vec<Sym *> &include_set, Vec<Sym *> &includes, Vec<Sym *> &in_includes) {
  if (!include_set.in(s) && in_includes.set_add(s)) {
    forv_Sym(ss, s->includes)
      collect_includes(ss, include_set, includes, in_includes);
    if (include_set.set_add(s))
      includes.add(s);
  }
}

static void
collect_include_vars(Sym *s, Sym *in = 0) {
  Vec<Sym *> saved;
  if (!in)
    saved.move(s->has);
  else
    in->has.append(s->has);
  forv_Sym(ss, s->includes)
    if (ss->type_kind == Type_RECORD)
      collect_include_vars(ss, in ? in : s);
  if (!in)
    s->has.append(saved);
}

static void
include_instance_variables(IF1 *i) {
  Vec<Sym *> include_set, includes;
  for (int x = finalized_types; x < i->allsyms.n; x++) {
    Sym *s = i->allsyms[x];
    Vec<Sym *> in_includes;
    if (s->type_kind == Type_RECORD && s->includes.n)
      collect_includes(s, include_set, includes, in_includes);
  }
  forv_Sym(s, includes) {
    if (s->id < finalized_types)
      continue;
    if (s->includes.n)
      collect_include_vars(s);
  }
}

static void
set_value_for_value_classes(IF1 *i) {
  sym_value->is_value_type = 1;
  sym_anynum->is_value_type = 1;
  Vec<Sym *> implementers;
  for (int x = finalized_types; x < i->allsyms.n; x++) {
    Sym *s = i->allsyms[x];
    if (s->type_kind && s->implements.n)
      implementers.add(s);
  }     
  int changed = 1;
  while (changed) {
    changed = 0;
    for (int x = finalized_types; x < i->allsyms.n; x++) {
      Sym *s = i->allsyms[x];
      forv_Sym(ss, s->implements)
        if (ss->is_value_type && !s->is_value_type) { 
          changed = 1;
          s->is_value_type = 1;
        }
    }
  }
}

static void
compute_isa(IF1 *i) {
  for (int x = finalized_types; x < i->allsyms.n; x++) {
    Sym *s = i->allsyms[x];
    // compute closure of has for Type_SUM
    if (s->type_kind == Type_SUM) {
      Accum<Sym *> acc;
      forv_Sym(ss, s->has)
        acc.add(ss);
      forv_Sym(ss, acc.asvec) {
        if (ss->type_kind == Type_SUM)
          forv_Sym(sss, ss->has)
            acc.add(sss);
      }
      forv_Sym(ss, acc.asvec)
        if (ss->type_kind != Type_SUM) 
          s->isa.set_add(ss);
    }
  }
}

void
make_meta_type(Sym *s) {
  if (!s->meta_type)
    s->meta_type = new_Sym();
  if (s->is_constant && !s->meta_type) {
    s->meta_type = s->type->meta_type;
    return;
  }
  s->meta_type->is_meta_type = 1;
  s->meta_type->in = s->in;
  s->meta_type->name = s->name;
  s->meta_type->type = s->meta_type;
  s->meta_type->ast = s->ast;
  s->meta_type->meta_type = s;
  s->meta_type->type_kind = Type_PRIMITIVE;
}

static void
set_type_and_meta_type(IF1 *i) {
  for (int x = finalized_types; x < i->allsyms.n; x++) {
    Sym *s = i->allsyms[x];
    if (s->type_kind != Type_NONE)
      s->type = s;
    if (s->type_kind) {
      if (!s->meta_type)
        make_meta_type(s);
    }
  }
}

static void
compute_type_size(Sym *s) {
  if (s->type_kind || s->is_constant || s->is_symbol) {
    Sym *type = s->type;
    if (!type) 
      return; // wait till after finalize_symbols
    if (type->is_symbol || type->type_kind == Type_TAGGED) {
      type = unalias_type(sym_int);
      s->size = if1_numeric_size(if1, type);
      s->alignment = if1_numeric_alignment(if1, type);
    } else if (type->num_kind) {
      s->size = if1_numeric_size(if1, type);
      s->alignment = if1_numeric_alignment(if1, type);
    } else {
      s->size = if1->pointer_size;
      s->alignment = if1->pointer_alignment;
    }
  }
}

static void
compute_type_sizes(IF1 *i) {
  for (int x = finalized_types; x < i->allsyms.n; x++)
    compute_type_size(i->allsyms[x]);
}

static void
unalias_syms(IF1 *i) {
  for (int x = finalized_types; x < i->allsyms.n; x++)
    unalias_sym(i->allsyms[x]);
}

static void
fixup_nesting_depth(IF1 *i) {
  for (int x = finalized_types; x < i->allsyms.n; x++) {
    Sym *s = i->allsyms[x];
    if (s->is_constant || s->is_symbol)
      s->nesting_depth = 0;
    else if (s->type_kind)
      s->nesting_depth = 0;
    compute_type_size(s);
  }
}

void
finalize_types(IF1 *i, int import_included_ivars) {
#define S(_n) assert(sym_##_n);
#include "builtin_symbols.h"
#undef S
  unalias_syms(i);
  if (import_included_ivars)
    include_instance_variables(i);
#define S(_n) sym_##_n = unalias_type(sym_##_n);
#include "builtin_symbols.h"
#undef S
  set_value_for_value_classes(i);
  compute_isa(i);
  set_type_and_meta_type(i);
  fixup_nesting_depth(i);
  compute_type_sizes(i);
  finalized_types = i->allsyms.n;
}


