/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#include "ast.h"
#include "builtin.h"
#include "fa.h"
#include "if1.h"
#include "ifadefs.h"

cchar *type_kind_string[] = {"NONE",      "UNKNOWN",     "SUM",      "RECORD",
                             "VECTOR",    "FUN",         "REF",      "ENUM",
                             "PRIMITIVE", "APPLICATION", "VARIABLE", "ALIAS"};

BasicSym::BasicSym(void)
    : name(NULL),
      in(NULL),
      type(NULL),
      aspect(NULL),
      must_specialize(NULL),
      must_implement(NULL),
      ast(NULL),
      var(NULL),
      asymbol(NULL),
      nesting_depth(0),
      cg_string(NULL),
      llvm_value(NULL),
      is_builtin(0),
      is_read_only(0),
      is_constant(0),
      is_lvalue(0),
      is_local(0),
      is_default_arg(0),
      is_exact_match(0),
      is_module(0),
      is_fun(0),
      is_symbol(0),
      is_pattern(0),
      is_rest(0),
      is_generic(0),
      is_external(0),
      is_this(0),
      is_fake(0),
      intent(0),
      is_meta_type(0),
      is_unique_type(0),
      is_value_type(0),
      is_system_type(0),
      is_union_type(0),
      is_structure(0),
      is_vector(0),
      fun_returns_value(0),
      live(0),
      type_kind(0),
      num_kind(0),
      num_index(0),
      clone_for_constants(0),
      dispatch_types_built(0),
      type_live(0) {}

Sym::Sym()
    : constant(NULL),
      size(0),
      alignment(0),
      scope(NULL),
      labelmap(NULL),
      fun(NULL),
      code(NULL),
      self(NULL),
      ret(NULL),
      cont(NULL),
      instantiates(NULL),
      match_type(NULL),
      abstract_type(NULL),
      alias(NULL),
      init(NULL),
      meta_type(NULL),
#ifdef CONSTANT_TYPES
      constant_type(NULL),
#endif
      element(NULL),
      temp(NULL),
      llvm_type(NULL) {
}

cchar *Sym::pathname() {
  cchar *p = 0;
  if (asymbol) p = asymbol->pathname();
  if (!p && ast) p = ast->pathname();
  if (p) return p;
  return "<unknown>";
}

cchar *Sym::has_name(int i) {
  if (i < has_names.n) return has_names[i];
  return 0;
}

cchar *Sym::filename() {
  cchar *fn = pathname();
  cchar *r = strrchr(fn, '/');
  if (r)
    return r + 1;
  else
    return fn;
}

int Sym::line() {
  int l = 0;
  if (asymbol) l = asymbol->line();
  if (!l && ast) l = ast->line();
  return l;
}

int Sym::source_line() {
  int l = 0;
  if (asymbol) l = asymbol->source_line();
  if (!l && ast) l = ast->source_line();
  return l;
}

void Sym::copy_values(Sym *s) {
  int temp_id = id;
  *this = *s;
  id = temp_id;
}

Sym *Sym::clone() {
  if (asymbol) return asymbol->clone();
  Sym *new_sym = copy();
  return new_sym;
}

Sym *Sym::copy() {
  Sym *s = new_Sym();
  s->copy_values(this);
  if (s->element) s->element = s->element->copy();
  return s;
}

int compar_syms(const void *ai, const void *aj) {
  uint32 i = (*(Sym **)ai)->id;
  uint32 j = (*(Sym **)aj)->id;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

int Sym::imm_int(int *result) {
  int i = 0;
  switch (type->num_kind) {
    default:
      return -1;
    case IF1_NUM_KIND_UINT: {
      switch (type->num_index) {
        case IF1_INT_TYPE_8:
          i = imm.v_uint8;
          break;
        case IF1_INT_TYPE_16:
          i = imm.v_uint16;
          break;
        case IF1_INT_TYPE_32:
          i = imm.v_uint32;
          break;
        case IF1_INT_TYPE_64:
          i = imm.v_uint64;
          break;
        default:
          return -1;
      }
      break;
    }
    case IF1_NUM_KIND_INT: {
      switch (type->num_index) {
        case IF1_INT_TYPE_8:
          i = imm.v_int8;
          break;
        case IF1_INT_TYPE_16:
          i = imm.v_int16;
          break;
        case IF1_INT_TYPE_32:
          i = imm.v_int32;
          break;
        case IF1_INT_TYPE_64:
          i = imm.v_int64;
          break;
        default:
          return -1;
      }
      break;
    }
  }
  *result = i;
  return 0;
}

Sym *unalias_type(Sym *s) {
  if (!s) return s;
  if (s->type_kind == Type_ALIAS) {
    Vec<Sym *> aliases;
    do {
      if (!s->alias) return 0;
      Sym *ss = s->alias;
      if (aliases.set_in(ss)) fail("circular type alias");
      aliases.set_add(ss);
      s = ss;
    } while (s->type_kind == Type_ALIAS);
  }
  return s;
}

void if1_set_int_type(IF1 *p, Sym *t, int signd, int size) {
  int ss = 0;
  size >>= 3;
  while (size) {
    ss++;
    size >>= 1;
  }
  p->int_types[ss][signd] = t;
  t->num_kind = signd ? IF1_NUM_KIND_INT : IF1_NUM_KIND_UINT;
  t->num_index = ss;
}

void if1_set_float_type(IF1 *p, Sym *t, int size) {
  int ss = 0;
  size >>= 4;
  ss = size - 1;
  p->float_types[ss] = t;
  t->num_kind = IF1_NUM_KIND_FLOAT;
  t->num_index = ss;
}

void if1_set_complex_type(IF1 *p, Sym *t, int size) {
  int ss = 0;
  size >>= 4;
  ss = size - 1;
  p->complex_types[ss] = t;
  t->num_kind = IF1_NUM_KIND_COMPLEX;
  t->num_index = ss;
}

int if1_numeric_size(IF1 *p, Sym *t) {
  switch (t->num_kind) {
    case IF1_NUM_KIND_NONE:
      assert(!"bad case");
      break;
    case IF1_NUM_KIND_INT:
    case IF1_NUM_KIND_UINT:
      if (!t->num_index) return sizeof(bool);
      return 1 << ((t->num_index ? t->num_index : 1) - 1);
    case IF1_NUM_KIND_FLOAT:
      return 2 + (2 * t->num_index);
    case IF1_NUM_KIND_COMPLEX:
      return 2 * (2 + (2 * t->num_index));
  }
  return 0;
}

#define MAKE_ALIGNOF(_t) \
  struct AlignOf##_t {   \
    char a;              \
    _t b;                \
  }

#define ALIGNOF(_t) ((int)(intptr_t) & (((struct AlignOf##_t *)0)->b))

MAKE_ALIGNOF(bool);
MAKE_ALIGNOF(uint8);
MAKE_ALIGNOF(uint16);
MAKE_ALIGNOF(uint32);
MAKE_ALIGNOF(uint64);
MAKE_ALIGNOF(float32);
MAKE_ALIGNOF(float64);
MAKE_ALIGNOF(float128);
MAKE_ALIGNOF(complex32);
MAKE_ALIGNOF(complex64);
typedef char *alignstring;
MAKE_ALIGNOF(alignstring);

int if1_numeric_alignment(IF1 *p, Sym *t) {
  int res = -1;
  switch (t->num_kind) {
    default:
      assert(!"case");
      break;
    case IF1_NUM_KIND_UINT:
    case IF1_NUM_KIND_INT:
      switch (t->num_index) {
        case IF1_INT_TYPE_1:
          return ALIGNOF(bool);
          break;
        case IF1_INT_TYPE_8:
          return ALIGNOF(uint8);
          break;
        case IF1_INT_TYPE_16:
          return ALIGNOF(uint16);
          break;
        case IF1_INT_TYPE_32:
          return ALIGNOF(uint32);
          break;
        case IF1_INT_TYPE_64:
          return ALIGNOF(uint64);
          break;
        default:
          assert(!"case");
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      switch (t->num_index) {
        case IF1_FLOAT_TYPE_32:
          return ALIGNOF(float32);
        case IF1_FLOAT_TYPE_64:
          return ALIGNOF(float64);
        case IF1_FLOAT_TYPE_128:
          return ALIGNOF(float128);
        default:
          assert(!"case");
      }
      break;
    case IF1_NUM_KIND_COMPLEX:
      switch (t->num_index) {
        case IF1_FLOAT_TYPE_32:
          return ALIGNOF(complex32);
        case IF1_FLOAT_TYPE_64:
          return ALIGNOF(complex64);
        default:
          assert(!"case");
      }
      break;
    case IF1_CONST_KIND_STRING:
      return ALIGNOF(alignstring);
  }
  return res;
}

void Sym::inherits_add(Sym *s) {
  implements.add(s);
  specializes.add(s);
  includes.add(s);
}

void Sym::must_implement_and_specialize(Sym *s) {
  assert((!must_implement || must_implement == s) &&
         (!must_specialize || must_specialize == s));
  must_implement = s;
  must_specialize = s;
}

Sym *Sym::scalar_type() {
  if (type && type->num_kind) return type;
  return 0;
}

Sym *Sym::coerce_to(Sym *to) {
  if (this == to) return this;
  Sym *s1 = this->scalar_type(), *s2 = to->scalar_type();
  if (s1 && s2) {
    Sym *t = coerce_num(s1, s2);
    if (t == s2) return s2;
    return NULL;
  }
  if (element) {
    Sym *s1 = element->scalar_type(), *s2 = to->scalar_type();
    if (s1 && s2) {
      Sym *t = coerce_num(s1, s2);
      if (t == s2) return s2;
      return NULL;
    }
  }
  if (s1) {
    if (to == sym_string) return to;
  }
  return NULL;
}

void Sym::convert_constant_string_to_immediate() {
  if (!is_constant) return;
  imm.const_kind = type->num_kind;
  imm.num_index = type->num_index;
  convert_string_to_immediate(constant, &imm);
}

static Sym *int_constant_internal(IF1 *i, int n, Sym *t) {
  Immediate imm;
  imm.v_int32 = n;
  if (n >= 0 && n < 10) {
    char c[2];
    c[0] = n + '0';
    c[1] = 0;
    return if1_const(i, t, c, &imm);
  } else {
    char str[100];
    sprintf(str, "%d", n);
    return if1_const(i, t, str, &imm);
  }
}

Sym *int32_constant(int n) { return int_constant_internal(if1, n, sym_int32); }

Sym *int64_constant(int n) { return int_constant_internal(if1, n, sym_int64); }

Sym *size_constant(int n) { return int_constant_internal(if1, n, sym_size); }

Sym *imm_constant(Immediate &imm, Sym *t) {
  char str[256];
  sprint_imm(str, imm);
  return if1_const(if1, t, str, &imm);
}

void pp(Sym *s) {
  printf("(SYM %d ", s->id);
  if1_dump_sym(stdout, s);
  printf(")");
}
