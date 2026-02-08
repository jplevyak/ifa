/* -*-Mode: c++;-*-
   Copyright (c) 2003-2010 John Plevyak, All Rights Reserved
*/
#include <ctype.h>

#include "ifadefs.h"

#include "cg.h"
#include "builtin.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "pattern.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

static inline cchar *c_type(Var *v) {
  if (!v->type || !v->type->cg_string) return "_CG_void";
  return v->type->cg_string;
}

static inline cchar *c_type(Sym *s) {
  if (!s->type || !s->type->cg_string) return "_CG_void";
  return s->type->cg_string;
}

static void write_c_fun_proto(FILE *fp, Fun *f, int type = 0) {
  assert(f->rets.n == 1);
  fputs(c_type(f->rets[0]), fp);
  if (type)
    fputs(" (*", fp);
  else
    fputs(" ", fp);
  if (type)
    fputs(f->cg_structural_string, fp);
  else
    fputs(f->cg_string, fp);
  if (type)
    fputs(")(", fp);
  else
    fputs("(", fp);
  MPosition p;
  p.push(1);
  int wrote_one = 0;
  for (int i = 0; i < f->sym->has.n; i++) {
    MPosition *cp = cannonicalize_mposition(p);
    p.inc();
    Var *v = f->args.get(cp);
    if (!v->live) continue;
    if (wrote_one) fputs(", ", fp);
    wrote_one = 1;
    fputs(c_type(v), fp);
    fprintf(fp, " a%d", i);
  }
  fputs(")", fp);
}

static void write_c_type(FILE *fp, Var *v) { fprintf(fp, "%s", c_type(v)); }

static int application_depth(PNode *n) {
  if (n->rvals[0]->type->fun)
    return 1;
  else
    return 1 + application_depth(n->rvals[0]->def);
}

static void write_c_apply_arg(FILE *fp, cchar *base, int n, int i) {
  fputs(base, fp);
  for (int j = i; j < n - 2; j++) fputs("->e0", fp);
  if (!i)
    fputs("->e0", fp);
  else
    fputs("->e1", fp);
}

static int cg_writeln(FILE *fp, Vec<Var *> &vars, int ln) {
  for (int i = 2; i < vars.n; i++) {
    bool doln = i == vars.n - 1 && ln;
    cchar *sln = doln ? "\\n" : "";
    if (vars[i]->type == sym_int8 || vars[i]->type == sym_int16 || vars[i]->type == sym_int32)
      fprintf(fp, "printf(\"%%d%s\", %s);\n", sln, vars[i]->cg_string);
    else if (vars[i]->type == sym_bool || vars[i]->type == sym_uint8 || vars[i]->type == sym_uint16 ||
             vars[i]->type == sym_uint32)
      fprintf(fp, "printf(\"%%u%s\", %s);\n", sln, vars[i]->cg_string);
    else if (vars[i]->type == sym_int64)
      fprintf(fp, "printf(\"%%lld%s\", (long long int)%s);\n", sln, vars[i]->cg_string);
    else if (vars[i]->type == sym_uint64)
      fprintf(fp, "printf(\"%%llu%s\", (unsigned long long int)%s);\n", sln, vars[i]->cg_string);
    else if (vars[i]->type == sym_float32 || vars[i]->type == sym_float64 || vars[i]->type == sym_float128)
      fprintf(fp, "_CG_float_printf(%s,%d);\n", vars[i]->cg_string, doln);
    else if (vars[i]->type == sym_string) {
      if (ln || strcmp("_CG_String(\"\")", vars[i]->cg_string)) {
        if (doln)
          fprintf(fp, "printf(\"%%s%s\", %s);\n", sln, vars[i]->cg_string);
        else
          fprintf(fp, "fputs(%s, stdout);\n", vars[i]->cg_string);
      }
    } else
      fprintf(fp, "printf(\"<unsupported type>%s\");\n", sln);
  }
  if (vars.n < 3 && ln) fputs("putchar(\'\\n\');\n", fp);
  return 0;
}

static cchar *num_string(Sym *s) {
  switch (s->num_kind) {
    default:
      assert(!"case");
    case IF1_NUM_KIND_UINT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:
          return "_CG_bool";
        case IF1_INT_TYPE_8:
          return "_CG_uint8";
        case IF1_INT_TYPE_16:
          return "_CG_uint16";
        case IF1_INT_TYPE_32:
          return "_CG_uint32";
        case IF1_INT_TYPE_64:
          return "_CG_uint64";
        default:
          assert(!"case");
      }
      break;
    case IF1_NUM_KIND_INT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:
          return "_CG_bool";
        case IF1_INT_TYPE_8:
          return "_CG_int8";
        case IF1_INT_TYPE_16:
          return "_CG_int16";
        case IF1_INT_TYPE_32:
          return "_CG_int32";
        case IF1_INT_TYPE_64:
          return "_CG_int64";
        default:
          assert(!"case");
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      switch (s->num_index) {
        case IF1_FLOAT_TYPE_32:
          return "_CG_float32";
        case IF1_FLOAT_TYPE_64:
          return "_CG_float64";
        case IF1_FLOAT_TYPE_128:
          return "_CG_float128";
        default:
          assert(!"case");
          break;
      }
      break;
  }
  return 0;
}

static cchar *c_rhs(Var *v) {
  if (!v->sym->is_fun) {
    if (!v->cg_string)
      return "0";
    else
      return v->cg_string;
  } else {
    char s[100];
    snprintf(s, sizeof(s), "((_CG_function*)%s)", v->cg_string);
    return dupstr(s);
  }
}

static void destruct_prim(FILE *fp, Var *l, Var *r) {
  int is_tuple = sym_tuple->specializers.set_in(l->sym) != 0;
  for (int i = 0; i < l->sym->has.n; i++) {
    if (!is_tuple && l->sym->has_name(i)) {
      assert(0);
    } else {
      fprintf(fp, "  %s->e%d = %s->e%d;\n", l->cg_string, i, r->cg_string, i);
    }
  }
}

static int write_c_prim(FILE *fp, FA *fa, Fun *f, PNode *n) {
  int o = (n->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  bool listish_tuple = false;
  switch (n->prim->index) {
    default:
      return 0;
    case P_prim_reply: {
      fprintf(fp, "  return %s;\n", c_rhs(n->rvals[3]));
      break;
    }
    case P_prim_make:
      if (sym_tuple->specializers.set_in(n->rvals[2]->sym)) {
      Ltuple:
        fputs("  ", fp);
        cchar *t = c_type(n->lvals[0]);
        int voidish = n->rvals.n < 4 && n->lvals[0]->type->element->type == sym_void;
        fprintf(fp, "%s = _CG_prim_tuple%s(%s, %d);\n", n->lvals[0]->cg_string, listish_tuple ? "_list" : "",
                voidish ? "int*" : t, n->rvals.n - 2);
        for (int i = 3; i < n->rvals.n; i++)
          fprintf(fp, "  %s->e%d = %s;\n", n->lvals[0]->cg_string, i - 3, n->rvals.v[i]->cg_string);
      } else if (sym_list->specializers.set_in(n->rvals[2]->sym) || n->rvals[2]->sym->is_vector) {
        Sym *t = n->lvals.v[0]->type, *e = t->element->type;
        if (t->type_kind == Type_RECORD) {
          listish_tuple = true;
          goto Ltuple;
        }
        fputs("  ", fp);
        assert(n->lvals.n == 1);
        e = e ? e : sym_void_type;
        assert(n->lvals[0]->cg_string);
        fprintf(fp, "%s = ", n->lvals[0]->cg_string);
        fprintf(fp, "(_CG_list)_CG_prim_list(%s,%d);\n", e->cg_string, n->rvals.n - 3);
        for (int i = 3; i < n->rvals.n; i++) {
          fprintf(fp, "  ((%s*)(_CG_list_ptr(%s)))[%d] = ", e->cg_string, n->lvals[0]->cg_string, i - 3);
          fputs(n->rvals[i]->cg_string, fp);
          fprintf(fp, ";\n");
        }
      }
      break;
    case P_prim_period: {
      cchar *t = c_type(n->lvals[0]);
      cchar *symbol = 0;
      Vec<Sym *> symbols;
      symbol_info(n->rvals[3], symbols);
      if (symbols.n == 1)
        symbol = symbols[0]->name;
      else if (n->rvals[3]->sym->is_symbol)
        symbol = n->rvals[3]->sym->name;
      else {
        assert(!"selector");
      }
      Sym *obj = n->rvals[1]->type;
      if (obj->type_kind == Type_SUM) obj = obj->has[0];
      if (n->lvals[0]->type->type_kind == Type_FUN && n->creates) {  // creates a closure
        fprintf(fp, "  %s = _CG_prim_closure(%s);\n", n->lvals[0]->cg_string, t);
        if (n->prim && n->prim->index == P_prim_period) {
          fprintf(fp, "  %s->e%d = %s;\n", n->lvals[0]->cg_string, 0, n->rvals.v[3]->cg_string);
          fprintf(fp, "  %s->e%d = %s;\n", n->lvals[0]->cg_string, 1, n->rvals.v[1]->cg_string);
        } else {
          for (int i = 0; i < n->rvals.n; i++)
            fprintf(fp, "  %s->e%d = %s;\n", n->lvals[0]->cg_string, i, n->rvals.v[i]->cg_string);
        }
      } else {
        for (int i = 0; i < obj->has.n; i++) {
          if (symbol == obj->has[i]->name) {
            assert(n->lvals[0]->cg_string);
            fprintf(fp, "  %s = (%s)((%s)%s)->e%d; /* %s */\n", n->lvals[0]->cg_string, t, obj->cg_string,
                    n->rvals[1]->cg_string, i, symbol);
            goto Lgetter_found;
          }
        }
        if (!fruntime_errors)
          fail("getter not resolved");
        else
          fputs("  assert(!\"runtime error: getter not resolved\");\n", fp);
      Lgetter_found:;
      }
      break;
    }
    case P_prim_setter: {
      cchar *symbol = n->rvals[3]->sym->is_symbol ? n->rvals[3]->sym->name : 0;
      if (!symbol) {
        Vec<Sym *> symbols;
        symbol_info(n->rvals[3], symbols);
        assert(symbols.n == 1);
        symbol = symbols[0]->name;
      }
      Sym *obj = n->rvals[1]->type;
      if (obj->type_kind == Type_SUM) obj = obj->has[0];
      for (int i = 0; i < obj->has.n; i++) {
        if (symbol == obj->has[i]->name) {
          fprintf(fp, "  ((%s)%s)->e%d = (%s)%s;\n", obj->cg_string, n->rvals[1]->cg_string, i, c_type(obj->has[i]),
                  c_rhs(n->rvals.v[4]));
          if (n->lvals[0]->live)
            fprintf(fp, "  %s = ((%s)%s);\n", n->lvals[0]->cg_string, obj->cg_string, n->rvals[1]->cg_string);
          goto Lsetter_found;
        }
      }
      fail("setter not resolved");
    Lsetter_found:
      break;
    }
    case P_prim_apply: {
      assert(!"unimplemented");
      fputs("  ", fp);
      int incomplete = 0;  // n->lvals.n && n->lvals[0]->type->var &&
                           // n->lvals.v[0]->type->var->def == n;
      if (incomplete) {
        fprintf(fp, "Fmake_apply(%s, ", n->lvals[0]->cg_string);
        write_c_type(fp, n->lvals[0]);
        fprintf(fp, ", %s, %s);\n", n->rvals[0]->cg_string, n->rvals.v[2]->cg_string);
      } else {
        if (n->lvals.n) {
          assert(n->lvals.n == 1);
          fprintf(fp, "%s = ", n->lvals[0]->cg_string);
        }
        int depth = application_depth(n);
        write_c_apply_arg(fp, n->rvals[0]->cg_string, depth, 0);
        fputs("((T__symbol) 0", fp);
        for (int i = 1; i < depth; i++) {
          fputs(", ", fp);
          write_c_apply_arg(fp, n->rvals[0]->cg_string, depth, i);
        }
        fputs(", ", fp);
        fputs(n->rvals[2]->cg_string, fp);
        fputs(");\n", fp);
      }
      break;
    }
    case P_prim_index_object: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      Sym *t = n->rvals[o]->type;
      Sym *e = n->lvals[0]->type;
      if (t->is_vector) {
        fprintf(fp, "%s = %s->v[%s];\n", n->lvals[0]->cg_string, n->rvals[o]->cg_string, n->rvals[o + 1]->cg_string);
      } else if (t->type_kind != Type_RECORD || !n->rvals[o + 1]->sym->constant) {
        if (n->lvals[0]->live) fprintf(fp, "%s = ", n->lvals[0]->cg_string);
        if (sym_string->specializers.set_in(t))
          fprintf(fp, "_CG_char_from_string(%s,%s);\n", n->rvals[o]->cg_string, n->rvals[o + 1]->cg_string);
        else {
          fprintf(fp, "((%s", e->cg_string);
          for (int i = o + 1; i < n->rvals.n; i++) fprintf(fp, "*");
          if (t->type_kind == Type_RECORD)
            fprintf(fp, ")(%s))", n->rvals[o]->cg_string);
          else
            fprintf(fp, ")(_CG_list_ptr(%s)))", n->rvals[o]->cg_string);
          for (int i = o + 1; i < n->rvals.n; i++) {
            if (i != o + 1) fputs(", ", fp);
            fprintf(fp, "[%s-%d]", n->rvals[i]->cg_string, fa->tuple_index_base);
          }
          fprintf(fp, ";\n");
        }
      } else {
        if (fruntime_errors && t->type_kind == Type_RECORD && !t->has.n)
          fputs("assert(!\"runtime error: bad getter\");\n", fp);
        else
          fprintf(fp, "%s = ((%s)%s)->e%s;\n", n->lvals[0]->cg_string, t->cg_string, n->rvals[o]->cg_string,
                  n->rvals[o + 1]->sym->constant);
      }
      break;
    }
    case P_prim_set_index_object: {
      fputs("  ", fp);
      Sym *t = n->rvals[o]->type;
      if (t->is_vector) {
        fprintf(fp, "%s->v[%s] = %s;\n", n->rvals[o]->cg_string, n->rvals[o + 1]->cg_string,
                n->rvals[n->rvals.n - 1]->cg_string);
      } else if (t->type_kind != Type_RECORD || !n->rvals[o + 1]->sym->constant) {
        fprintf(fp, "((%s", n->lvals[0]->type->cg_string);
        for (int i = o + 1; i < n->rvals.n - 1; i++) fprintf(fp, "*");
        if (t->type_kind == Type_RECORD)
          fprintf(fp, ")(%s))", n->rvals[o]->cg_string);
        else
          fprintf(fp, ")(_CG_list_ptr(%s)))", n->rvals[o]->cg_string);
        for (int i = o + 1; i < n->rvals.n - 1; i++) {
          if (!fa->tuple_index_base)
            fprintf(fp, "[%s]", n->rvals[i]->cg_string);
          else
            fprintf(fp, "[%s-%d]", n->rvals[i]->cg_string, fa->tuple_index_base);
        }
        fprintf(fp, " = %s;\n", n->rvals[n->rvals.n - 1]->cg_string);
      } else {
        fprintf(fp, "((%s)%s)->e%s = %s;\n", t->cg_string, n->rvals[o]->cg_string, n->rvals[o + 1]->sym->constant,
                n->rvals[n->rvals.n - 1]->cg_string);
      }
      break;
    }
    case P_prim_new: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      fprintf(fp, "%s = ", n->lvals[0]->cg_string);
      fprintf(fp, "_CG_prim_new(%s);\n", n->lvals[0]->type->cg_string);
      break;
    }
    case P_prim_assign: {
      fputs("  ", fp);
      fprintf(fp, "%s = (%s)", n->lvals[0]->cg_string, num_string(n->lvals.v[0]->type));
      fprintf(fp, "%s;\n", n->rvals[3]->cg_string);
      break;
    }
    case P_prim_len: {
      fputs("  ", fp);
      if (n->lvals.n && n->lvals[0]->cg_string) fprintf(fp, "%s = ", n->lvals[0]->cg_string);
      Sym *t = n->rvals[o]->type;
      if (sym_string->specializers.set_in(t))
        fprintf(fp, "_CG_string_len(%s);\n", n->rvals[o]->cg_string);
      else {
        assert(n->rvals[o]->cg_string);
        fprintf(fp, "_CG_prim_len(%s,%s);\n", n->rvals[o - 1]->cg_string, n->rvals[o]->cg_string);
      }
      break;
    }
    case P_prim_clone_vector:
    case P_prim_clone: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      if (n->lvals[0]->cg_string) fprintf(fp, "%s = ", n->lvals[0]->cg_string);
      if (n->prim->index == P_prim_clone)
        fprintf(fp, "(%s)_CG_prim_clone(", n->lvals[0]->type->cg_string);
      else
        fprintf(fp, "(%s)_CG_prim_clone_vector(", n->lvals[0]->type->cg_string);
      for (int i = 2; i < n->rvals.n; i++) {
        if (i > 2) fprintf(fp, ", ");
        fputs(n->rvals[i]->cg_string, fp);
      }
      fputs(");\n", fp);
      break;
    }
    case P_prim_sizeof: {
      if (n->lvals.n) {
        assert(n->lvals.n == 1);
        if (n->lvals[0]->cg_string)
          fprintf(fp, "  %s = ", n->lvals[0]->cg_string);
        else
          fprintf(fp, "  ");
      } else
        fprintf(fp, "  ");
      Sym *t = n->rvals[o]->type;
      fprintf(fp, "%d;\n", t->size);
      break;
    }
    case P_prim_sizeof_element: {
      if (n->lvals.n) {
        assert(n->lvals.n == 1);
        if (n->lvals[0]->cg_string)
          fprintf(fp, "  %s = ", n->lvals[0]->cg_string);
        else
          fprintf(fp, "  ");
      } else
        fprintf(fp, "  ");
      Sym *t = n->rvals[o]->type;
      if (!t->element->type->size && t->type_kind == Type_RECORD) {
        if (t->has.n)
          fprintf(fp, "%d;\n", t->has[0]->type->size);
        else
          fprintf(fp, "0;\n");
      } else
        fprintf(fp, "%d;\n", t->element->type->size);
      break;
    }
    case P_prim_primitive: {
      if (n->lvals.n) {
        assert(n->lvals.n == 1);
        if (n->lvals[0]->cg_string)
          fprintf(fp, "  %s = ", n->lvals[0]->cg_string);
        else
          fprintf(fp, "  ");
      } else
        fprintf(fp, "  ");
      cchar *name = n->rvals[1]->sym->name;
      if (!name) name = n->rvals[1]->sym->constant;
      if (!strcmp("print", name))
        cg_writeln(fp, n->rvals, 0);
      else if (!strcmp("println", name))
        cg_writeln(fp, n->rvals, 1);
      else {
        RegisteredPrim *p = prim_get(name);
        if (p && p->cgfn)
          p->cgfn(fp, n, f);
        else {
          fprintf(fp, "_CG_%s_%s(", n->prim->name, name);
          bool first = true;
          for (int i = 2; i < n->rvals.n; i++) {
            if (n->rvals[i]->cg_string) {
              if (!first) fprintf(fp, ", ");
              fputs(n->rvals[i]->cg_string, fp);
              first = false;
            }
          }
          fputs(");\n", fp);
        }
      }
      break;
    }
    case P_prim_destruct:
      for (int i = 0; i < n->lvals.n; i++) destruct_prim(fp, n->lvals.v[i], n->rvals.v[o + i]);
      break;
  }
  return 1;
}

static Fun *get_target_fun(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || fns->n != 1) {
    if (!fruntime_errors) fail("unable to resolve to a single function at call site");
    return 0;
  }
  return fns->v[0];
}

static void simple_move(FILE *fp, Var *lhs, Var *rhs) {
  if (!lhs->live) return;
  if (lhs->sym->type_kind || rhs->sym->type_kind) return;
  if (!lhs->cg_string || !rhs->cg_string) return;
  if (rhs->type == sym_void->type || lhs->type == sym_void->type) return;
  if (!rhs->sym->fun) {
    ASSERT(rhs->cg_string);
    if (rhs->type != lhs->type)
      fprintf(fp, "  %s = (%s)%s;\n", lhs->cg_string, c_type(lhs), rhs->cg_string);
    else
      fprintf(fp, "  %s = %s;\n", lhs->cg_string, rhs->cg_string);
  } else if (rhs->cg_string)
    fprintf(fp, "  %s = (_CG_function)&%s;\n", lhs->cg_string, rhs->cg_string);
  else
    fprintf(fp, "  %s = NULL;\n", lhs->cg_string);
}

static int write_c_fun_arg(FILE *fp, Fun *f, char *s, char *e, Sym *sym, int i, int &wrote_one) {
  if (!i && sym->type_kind == Type_FUN && !sym->fun && sym->has.n) {
    assert(0);
    for (int i = 0; i < sym->type->has.n; i++) {
      if (i) fprintf(fp, ", ");
      snprintf(e, 4096 - (e - s), "->e%d", i);
      write_c_fun_arg(fp, f, s, s + strlen(s), sym->has[i], i, wrote_one);
      fputs(s, fp);
    }
  } else {
    if (wrote_one) fprintf(fp, ", ");
    wrote_one = 1;
    Fun *f = sym->type->fun;
    if (f)
      fprintf(fp, "((_CG_function)&%s)", f->cg_string);
    else {
      snprintf(e, 4096 - (e - s), "->e%d", i);
      fputs(s, fp);
    }
  }
  return 1;
}

static int is_closure_var(Var *v) {
  Sym *t = v->type;
  return (t && t->type_kind == Type_FUN && !t->fun && t->has.n);
}

static void write_arg_position(FILE *fp, MPosition *p) {
  for (int i = 0; i < p->pos.n; i++) {
    if (is_intPosition(p->pos[i])) {
      if (!i)
        fprintf(fp, "a%d", (int)Position2int(p->pos[i]) - 1);
      else
        fprintf(fp, "->e%d", (int)Position2int(p->pos[i]) - 1);
    }
  }
}

static void write_send_arg(FILE *fp, Fun *f, PNode *n, MPosition *p, int &wrote_one) {
  int i = Position2int(p->pos[0]) - 1;
  Var *v0 = n->rvals[0];
  if (is_closure_var(v0)) {
    if (i < v0->type->has.n) {
      char ss[4096];
      snprintf(ss, sizeof(ss), "(%s)%s", c_type(f->args.get(p)), v0->cg_string);
      char *ee = ss + strlen(ss);
      write_c_fun_arg(fp, f, ss, ee, v0->type->has[i], i, wrote_one);
      return;
    } else
      i -= v0->type->has.n - 1;
  }
  Var *v = n->rvals[i];
  if (p->pos.n <= 1) {
    if (wrote_one) fprintf(fp, ", ");
    wrote_one = 1;
    fputs(c_rhs(v), fp);
  } else {
#if 0
    // These are inside tuples: potentially we could unwrap the tuple and pass the arguments directly.
    fputs(c_rhs(v), fp);
    for (int i = 1; i < p->pos.n; i++) 
      if (is_intPosition(p->pos[i])) {
        fprintf(fp, "->e%d", (int)Position2int(p->pos[i])-1);
      }
#endif
  }
}

static void write_send(FILE *fp, Fun *f, PNode *n) {
  fputs("  ", fp);
  if (n->prim) {
    if (n->lvals.n) {
      assert(n->lvals.n == 1);
      if (n->lvals[0]->cg_string) fprintf(fp, "%s = ", n->lvals[0]->cg_string);
    }
    fprintf(fp, "_CG_%s(", n->prim->name);
    int comma = 0;
    int start = 1;
    if (n->rvals[0]->sym == sym_primitive) start = 2;
    for (int i = start; i < n->rvals.n; i++) {
      cchar *s = n->rvals[i]->cg_string;
      if (!s) s = n->rvals[i]->sym->cg_string;
      assert(s);
      if (comma) fprintf(fp, ", ");
      comma = 1;
      fputs(s, fp);
    }
    fputs(");\n", fp);
  } else {
    Fun *target = get_target_fun(n, f);
    if (target) {
      if (n->lvals.n) {
        assert(n->lvals.n == 1);
        if (n->lvals[0]->cg_string) fprintf(fp, "%s = ", n->lvals[0]->cg_string);
      }
      fputs(target->cg_string, fp);
      fputs("(", fp);
      int wrote_one = 0;
      forv_MPosition(p, target->positional_arg_positions) {
        Var *av = target->args.get(p);
        if (!av->live) continue;
        write_send_arg(fp, target, n, p, wrote_one);
      }
      fputs(");\n", fp);
    } else {
      fputs("assert(!\"runtime error: matching function not found\");\n", fp);
    }
  }
}

static void do_phy_nodes(FILE *fp, PNode *n, int isucc) {
  forv_PNode(p, n->phy) simple_move(fp, p->lvals[isucc], p->rvals.v[0]);
}

static void do_phi_nodes(FILE *fp, PNode *n, int isucc) {
  if (n->cfg_succ.n) {
    PNode *succ = n->cfg_succ[isucc];
    if (succ->phi.n) {
      int i = succ->cfg_pred_index.get(n);
      forv_PNode(pp, succ->phi) simple_move(fp, pp->lvals[0], pp->rvals.v[i]);
    }
  }
}

static void write_c_pnode(FILE *fp, FA *fa, Fun *f, PNode *n, Vec<PNode *> &done) {
  if (n->live && n->fa_live) switch (n->code->kind) {
      case Code_LABEL:
        fprintf(fp, " L%d:;\n", n->code->label[0]->id);
        break;
      case Code_MOVE:
        for (int i = 0; i < n->lvals.n; i++) simple_move(fp, n->lvals[i], n->rvals.v[i]);
        break;
      case Code_SEND:
        if (n->prim)
          if (write_c_prim(fp, fa, f, n)) break;
        write_send(fp, f, n);
        break;
      case Code_IF:
      case Code_GOTO:
        break;
      default:
        assert(!"case");
    }
  switch (n->code->kind) {
    case Code_IF:
      if (n->live && n->fa_live) {
        if (n->rvals[0]->sym == true_type->v[0]->sym) {
          do_phy_nodes(fp, n, 0);
          do_phi_nodes(fp, n, 0);
          if (done.set_add(n->cfg_succ[0])) write_c_pnode(fp, fa, f, n->cfg_succ[0], done);
        } else if (n->rvals[0]->sym == false_type->v[0]->sym) {
          do_phy_nodes(fp, n, 1);
          do_phi_nodes(fp, n, 1);
          if (done.set_add(n->cfg_succ[1])) write_c_pnode(fp, fa, f, n->cfg_succ[1], done);
        } else {
          fprintf(fp, "  if (%s) {\n", n->rvals[0]->cg_string);
          do_phy_nodes(fp, n, 0);
          do_phi_nodes(fp, n, 0);
          if (done.set_add(n->cfg_succ[0]))
            write_c_pnode(fp, fa, f, n->cfg_succ[0], done);
          else
            fprintf(fp, "  goto L%d;\n", n->code->label[0]->id);
          fprintf(fp, "  } else {\n");
          do_phy_nodes(fp, n, 1);
          do_phi_nodes(fp, n, 1);
          if (done.set_add(n->cfg_succ[1]))
            write_c_pnode(fp, fa, f, n->cfg_succ[1], done);
          else
            fprintf(fp, "  goto L%d;\n", n->code->label[1]->id);
          fputs("  }\n", fp);
        }
      } else {
        do_phy_nodes(fp, n, 0);
        do_phi_nodes(fp, n, 0);
      }
      break;
    case Code_GOTO:
      do_phi_nodes(fp, n, 0);
      if (n->live && n->fa_live) fprintf(fp, "  goto L%d;\n", n->code->label[0]->id);
      break;
    case Code_SEND:
      if ((!n->live || !n->fa_live) && n->prim && n->prim->index == P_prim_reply)
        fprintf(fp, "  return 0;\n");
      else
        do_phi_nodes(fp, n, 0);
      break;
    default:
      do_phi_nodes(fp, n, 0);
      break;
  }
  int extra_goto = n->cfg_succ.n == 1 && n->code->kind != Code_GOTO && n->code->kind != Code_LABEL;
  forv_PNode(p, n->cfg_succ) if (done.set_add(p)) {
    write_c_pnode(fp, fa, f, p, done);
    extra_goto = 0;
  }
  if (extra_goto && n->cfg_succ[0]->live && n->cfg_succ[0]->fa_live) {
    assert(n->cfg_succ[0]->code->kind == Code_LABEL);
    fprintf(fp, "  goto L%d;\n", n->cfg_succ[0]->code->label[0]->id);
  }
}

static void write_c_args(FILE *fp, Fun *f) {
  forv_MPosition(p, f->positional_arg_positions) {
    Var *v = f->args.get(p);
    Sym *s = v->sym;
    if (v->cg_string && !s->is_symbol && !s->is_fun && v->live) {
      fprintf(fp, "  %s = ", v->cg_string);
      write_arg_position(fp, p);
      fprintf(fp, ";\n");
    }
  }
}

static bool lt_type_id(Var *a, Var *b) {
  int ta = a->type ? a->type->id : -1;
  int tb = b->type ? b->type->id : -1;
  return (ta < tb);
}

static void write_c(FILE *fp, FA *fa, Fun *f, Vec<Var *> *globals = 0) {
  if (!f->live) return;
  fputs("\n", fp);
  write_c_fun_proto(fp, f);
  if (!f->entry) {
    fputs(" { }\n", fp);
    return;
  }
  fputs(" {\n", fp);
  int index = 0;
  Vec<Var *> vars, defs;
  f->collect_Vars(vars, 0, FUN_COLLECT_VARS_NO_TVALS);
  forv_Var(v, vars) if (v->sym->is_local || v->sym->is_fake) v->cg_string = 0;
  forv_Var(v, vars) if (!v->is_internal && !v->sym->is_fake) {
    if (!v->cg_string && v->live && !v->sym->is_symbol && v->type != sym_continuation) {
      char s[100];
      snprintf(s, sizeof(s), "t%d", index++);
      v->cg_string = dupstr(s);
      defs.add(v);
    }
  }
  defs.qsort(lt_type_id);
  Sym *last_t = (Sym *)-1;
  forv_Var(v, defs) {
    if (v->type != last_t) {
      if (last_t != (Sym *)-1) fprintf(fp, ";\n");
      fputs("  ", fp);
      write_c_type(fp, v);
      fprintf(fp, " %s", v->cg_string);
    } else
      fprintf(fp, ", %s", v->cg_string);
    last_t = v->type;
  }
  if (defs.n) fprintf(fp, ";\n\n");
  if (globals)
    forv_Var(v, *globals) if (!v->sym->is_fun && v->sym->fun && !v->sym->type_kind && v->cg_string)
        fprintf(fp, "  %s = %s;\n", v->cg_string, v->sym->fun->cg_string);
  write_c_args(fp, f);
  rebuild_cfg_pred_index(f);
  Vec<PNode *> done;
  done.set_add(f->entry);
  write_c_pnode(fp, fa, f, f->entry, done);
  fputs("}\n", fp);
}

static inline bool homogeneous_tuple(Sym *s) {
  for (int i = 0; i < s->has.n - 1; i++)
    for (int j = i; j < s->has.n; j++)
      if (s->has[i]->type != s->has[j]->type) return false;
  return true;
}

static int build_type_strings(FILE *fp, FA *fa, Vec<Var *> &globals) {
// build builtin map
#define S(_n) if1_get_builtin(fa->pdb->if1, #_n)->cg_string = "_CG_" #_n;
#include "builtin_symbols.h"
#undef S
  // assign functions a C type string
  int f_index = 0;
  forv_Fun(f, fa->funs) {
    if (!f->live) continue;
    char s[100];
    if (f->sym->name) {
      if (f->sym->has.n > 1 && f->sym->has[1]->must_specialize)
        snprintf(s, sizeof(s), "_CG_f_%d_%d/*%s::%s*/", f->sym->id, f_index, f->sym->has[1]->must_specialize->name,
                 f->sym->name);
      else
        snprintf(s, sizeof(s), "_CG_f_%d_%d/*%s*/", f->sym->id, f_index, f->sym->name);
    } else
      snprintf(s, sizeof(s), "_CG_f_%d_%d", f->sym->id, f_index);
    f->cg_string = dupstr(s);
    snprintf(s, sizeof(s), "_CG_pf%d", f_index);
    f->cg_structural_string = dupstr(s);
    f->sym->cg_string = f->cg_structural_string;
    f_index++;
    if (f->sym->var) globals.set_add(f->sym->var);
  }
  Vec<Sym *> allsyms;
  collect_types_and_globals(fa, allsyms, globals);
  // assign creation sets C type strings
  if (allsyms.n) fputs("/*\n Type Declarations\n*/\n\n", fp);
  forv_Sym(s, allsyms) {
    if (s->num_kind)
      s->cg_string = num_string(s);
    else if (s->is_symbol) {
      s->cg_string = "_CG_symbol";
    } else {
      if (s->cg_string) {
        // skip
      } else {
        switch (s->type_kind) {
          default:
            s->cg_string = dupstr("_CG_any");
            break;
          case Type_FUN:
            if (s->fun) break;
          // fall through
          case Type_RECORD: {
            if (s->has.n) {
              char ss[100];
              fprintf(fp, "/* %s */ struct _CG_s%d; ", s->name ? s->name : "", s->id);
              fprintf(fp, "typedef struct _CG_s%d *_CG_ps%d;\n", s->id, s->id);
              snprintf(ss, sizeof(ss), "_CG_ps%d", s->id);
              s->cg_string = dupstr(ss);
            } else
              s->cg_string = "_CG_void";
            break;
          }
        }
      }
    }
  }
  // resolve types
  forv_Sym(s, allsyms) {
    if (s->fun)
      s->cg_string = s->fun->cg_structural_string;
    else if (s->is_symbol)
      s->cg_string = sym_symbol->cg_string;
    if (s->type_kind == Type_SUM && s->has.n == 2) {
      if (s->has[0] == sym_nil_type)
        s->cg_string = s->has[1]->cg_string;
      else if (s->has[1] == sym_nil_type)
        s->cg_string = s->has[0]->cg_string;
    }
  }
  if (allsyms.n) fputs("\n", fp);
  // define function types and prototypes
  Vec<Fun *> live_funs;
  forv_Fun(f, fa->funs) {
    if (!f->live) continue;
    live_funs.add(f);
  }
  if (live_funs.n) fputs("/*\n Function Prototypes\n*/\n\n", fp);
  if (live_funs.n) {
    fprintf(fp, "typedef _CG_function %s", live_funs[0]->cg_structural_string);
    for (int i = 1; i < live_funs.n; i++) fprintf(fp, ", %s", live_funs[i]->cg_structural_string);
    fprintf(fp, ";\n");
  }
  forv_Fun(f, live_funs) {
    write_c_fun_proto(fp, f);
    fputs(";\n", fp);
  }
  if (live_funs.n) fputs("\n", fp);
  // define structs
  if (allsyms.n) fputs("/*\n Type Definitions\n*/\n\n", fp);
  forv_Sym(s, allsyms) {
    switch (s->type_kind) {
      default:
        break;
      case Type_FUN:
        if (s->fun) break;
      // fall through
      case Type_RECORD: {
        if (s->has.n) {
          fprintf(fp, "struct _CG_s%d {\n", s->id);
          for (int i = 0; i < s->has.n; i++) {
            if (s->has[i]->type && (!s->has[i]->var || s->has[i]->var->live)) {
              fputs("  ", fp);
              fputs(c_type(s->has[i]), fp);
              fprintf(fp, " e%d;", i);
              if (s->has[i]->name) fprintf(fp, " /* %s */;", s->has[i]->name);
              fputs("\n", fp);
            }
          }
          if (s->is_vector) {
            fputs("  ", fp);
            fputs(c_type(s->element), fp);
            fprintf(fp, " v[0];");
          }
          fprintf(fp, "};\n");
        }
      }
    }
  }
  if (allsyms.n) fputs("\n/*\n Builtin Functions\n*/\n\n", fp);
  forv_Sym(s, allsyms) {
    if (s->type_kind == Type_RECORD && s->creators.n && s->creators[0]->sym == sym_list && homogeneous_tuple(s) &&
        s->has.n)
      fprintf(fp, "_CG_TUPLE_TO_LIST_FUN(%d, %d);\n", s->id, s->has.n);
  }
  return 0;
}

void c_codegen_print_c(FILE *fp, FA *fa, Fun *init) {
  Vec<Var *> globals;
  int index = 0;
  if (!if1->callback->c_codegen_pre_file(fp)) fprintf(fp, "#include \"c_runtime.h\"\n\n");
  if (build_type_strings(fp, fa, globals) < 0) fail("unable to generate C code: no unique typing");
  if (globals.n) {
    fputs("\n/*\n Global Variables\n*/\n\n", fp);
  }
  forv_Var(v, globals) if (v->sym->is_fun) v->cg_string = v->sym->fun->cg_string;
  forv_Var(v, globals) {
    Sym *s = unalias_type(v->sym);
    if (!v->live) continue;
    // Skip type definitions - they're handled in the Type Definitions section
    if (s->type_kind) continue;
    if (v->type == sym_nil_type) {
      v->cg_string = "NULL";
      continue;
    }
    if (s->imm.const_kind != IF1_NUM_KIND_NONE && s->imm.const_kind != IF1_CONST_KIND_STRING) {
      char ss[100];
      sprint_imm(ss, sizeof(ss), s->imm);
      v->cg_string = dupstr(ss);
    } else if (s->constant) {
      if (v->type == sym_string) {
        char *x = escape_string(s->constant);
        v->cg_string = (char *)MALLOC(strlen(x) + 20);
        STRCPYZ(v->cg_string, "_CG_String(");
        STRCAT(v->cg_string, x);
        STRCAT(v->cg_string, ")");
      } else
        v->cg_string = s->constant;
    } else if (s->is_symbol) {
      char ss[100];
      snprintf(ss, sizeof(ss), "_CG_Symbol(%d, \"%s\")", s->id, s->name);
      v->cg_string = dupstr(ss);
    } else if (s->is_fun) {
    } else {
      char ss[100];
      if (s->name)
        snprintf(ss, sizeof(ss), "/* %s %d */ g%d", s->name, s->id, index++);
      else
        snprintf(ss, sizeof(ss), "/* %d */ g%d", s->id, index++);
      v->cg_string = dupstrs(ss);
      write_c_type(fp, v);
      fputs(" ", fp);
      fputs(v->cg_string, fp);
      fputs(";\n", fp);
    }
  }
  fputs("\n/*\n Functions\n*/\n", fp);
  forv_Fun(f, fa->funs) if (f != init && !f->is_external) write_c(fp, fa, f);
  write_c(fp, fa, init, &globals);
  fprintf(fp,
          "\nint main(int argc, char *argv[]) { (void)argc; (void) argv;\n"
          "  MEM_INIT();\n"
          "  %s();\n"
          "  return 0;\n"
          "}\n",
          init->cg_string);
}

void c_codegen_write_c(FA *fa, Fun *main, cchar *filename) {
  char fn[512];
  strcpy(fn, filename);
  strcat(fn, ".c");
  FILE *fp = fopen(fn, "w");
  c_codegen_print_c(fp, fa, main);
  fclose(fp);
}

int c_codegen_compile(cchar *filename) {
  char target[512], s[1024];
  strcpy(target, filename);
  *strrchr(target, '.') = 0;
  snprintf(s, sizeof(s),
           "make --no-print-directory -f %s/Makefile.cg CG_ROOT=%s CG_TARGET=%s "
           "CG_FILES=%s.c %s %s",
           system_dir, system_dir, target, filename, codegen_optimize ? "OPTIMIZE=1" : "",
           codegen_debug ? "DEBUG=1" : "");
  return system(s);
}
