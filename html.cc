/* -*-Mode: c++;-*-
   Copyright (c) 2004-2009 John Plevyak, All Rights Reserved
*/
#include "ast.h"
#include "builtin.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "ifadefs.h"
#include "log.h"
#include "pattern.h"
#include "prim.h"
#include "var.h"

#define ANON "*anon*"

// #define FUNCTION_SYMBOLS 1
// #define LOCAL_SYMBOLS 1
// #define INCLUDE_PARENT_TYPES 1
// #define GLOBAL_SYMBOLS 1

static void dump_header(FILE *fp, cchar *fn, char *mktree_dir) {
  fprintf(fp, "<HTML>\n");
  fprintf(fp, "<HEAD>\n");
  fprintf(fp, "<TITLE> Program Dump for %s </TITLE>\n", fn);
  fprintf(fp, "<SCRIPT SRC=\"%s/mktree.js\" LANGUAGE=\"JavaScript\"></SCRIPT>", mktree_dir);
  fprintf(fp, "<LINK REL=\"stylesheet\" HREF=\"%s/mktree.css\">", mktree_dir);
  fprintf(fp, "</HEAD>\n");
  fprintf(fp,
          "<div style=\"text-align: center;\"><big><big><span "
          "style=\"font-weight: bold;\">");
  fprintf(fp, "Program Dump for %s <br></span></big></big>\n", fn);
  fprintf(fp, "<div style=\"text-align: left;\">\n\n");
  fprintf(fp, "<style type=\"text/css\">\n");
  fprintf(fp, "body { font-family: arial, sans-serif; }\n");
  fprintf(fp, "ul { list-style: none; margin:0; padding:0 }\n");
  fprintf(fp, "</style>\n");
  fprintf(fp, "<H1>Contents</H1>\n\n");
  fprintf(fp, "<ul>\n");
  fprintf(fp,
          "<li><a href=\"#CONCRETE_TYPES\">Concrete Types "
          "(Clones/Instantiations)</a>\n");
  fprintf(fp,
          "<li><a href=\"#CONCRETE_FUNCTIONS\">Concrete Functions "
          "(Clones/Instantiations)</a>\n");
#if FUNCTION_SYMBOLS
  fprintf(fp, "<li><a href=\"#FUNCTION_SYMBOLS\">Function Symbols</a>\n");
#endif
  fprintf(fp, "<li><a href=\"#GLOBALS\">Global/Module Symbols</a>\n");
#ifdef LOCAL_SYMBOLS
  fprintf(fp, "<li><a href=\"#SYMBOLS\">Local Symbols</a>\n");
#endif
  fprintf(fp, "</ul>\n");
}

static void dump_footer(FILE *fp) { fprintf(fp, "</HTML>\n"); }

static int has_no_out_edges(Sym *s) {
  if (s->var) form_AVarMapElem(p, s->var->avars) if (p->value->forward.n || p->value->arg_of_send.asvec.n) return 0;
  return 1;
}

typedef int (*sym_pred_fn)(Sym *s);

static int is_internal_type(Sym *s) { return s->type_kind == Type_SUM && !s->name; }

static void dump_sym_sym(FILE *fp, Sym *ss, int comma) {
  if (comma) fprintf(fp, ", ");
  if (!ss->is_system_type && !ss->is_builtin && !ss->in)
    fprintf(fp, "<a href=\"#SYM_%d\">%s (%d)</a>", ss->id, ss->name ? ss->name : ANON, ss->id);
  else
    fprintf(fp, "<a>%s</a>", ss->name ? ss->name : ANON);
  if (ss->type && ss->type != ss) {
    if (!ss->type->is_system_type && !ss->type->is_builtin)
      fprintf(fp, " : <a href=\"#SYM_%d\">%s (%d)</a>", ss->type->id, ss->type->name ? ss->type->name : ANON,
              ss->type->id);
    else
      fprintf(fp, " : <a>%s</a>", ss->type->name ? ss->type->name : ANON);
  }
}

static void dump_sub_sym(FILE *fp, Sym *ss, cchar *title) {
  if (ss && ss->type) {
    fprintf(fp, "<TR><TD><TD>%s&nbsp;<TD>", title);
    dump_sym_sym(fp, ss, 0);
  }
}

static void dump_sym_list(FILE *fp, Sym *s, Vec<Sym *> &l, cchar *title, sym_pred_fn fn = 0) {
  if (l.n) {
    Vec<Sym *> v;
    forv_Sym(ss, l) if (ss && ss != s && (!fn || !fn(ss))) v.add(ss);
    if (v.n) {
      dump_sub_sym(fp, v[0], title);
      for (int i = 1; i < v.n; i++) dump_sym_sym(fp, v[i], 1);
      fprintf(fp, "\n");
    }
  }
}

static void dump_sym(FILE *fp, Sym *t) {
  fprintf(fp, "<b><A NAME=\"SYM_%d\">%s (%d)</A></b>\n", t->id, t->name ? t->name : ANON, t->id);
  fprintf(fp, "<TABLE BORDER=0, CELLSPACING=0, CELLPADDING=0>\n");
  fprintf(fp, "<TR><TD WIDTH=30><TD WIDTH=150>ID<TD>%d\n", t->id);
  if (t->in && t->in->name)
    fprintf(fp, "<TR><TD WIDTH=30><TD WIDTH=150>In<TD>%s %s\n",
            t->in->is_module ? "module" : (t->type_kind != Type_NONE ? "type" : "function"), t->in->name);
  if (t->line() > 0 && t->filename() && *t->filename())
    fprintf(fp, "<TR><TD><TD>Location<TD>%s:%d\n", t->filename(), t->line());
  if (t->is_builtin) {
    cchar *name = if1->builtins_names.get(t);
    if (name) fprintf(fp, "<TR><TD><TD>Builtin<TD>%s\n", name);
  }
  dump_sub_sym(fp, t->aspect, "Aspect");
  if (t->type_kind != Type_NONE) fprintf(fp, "<TR><TD><TD>Type Kind<TD>%s\n", type_kind_string[t->type_kind]);
  Sym *type = t->type;
  if (!type && t->var && t->var->type) type = t->var->type;
  if (type) {
    if (t->is_symbol) type = sym_symbol;
    if (t->type != type) dump_sub_sym(fp, type, "Type");
  }
  dump_sym_list(fp, t, t->implements, "Implements", is_internal_type);
  dump_sym_list(fp, t, t->specializes, "Specializes ");
  dump_sym_list(fp, t, t->includes, "Includes");
  if (t->must_specialize && t != t->must_specialize) dump_sub_sym(fp, t->must_specialize, "Must Specialize");
  if (t->must_implement && t != t->must_implement) dump_sub_sym(fp, t->must_implement, "Must Implement");
  dump_sym_list(fp, t, t->has, "Has", has_no_out_edges);
  dump_sub_sym(fp, t->self, "Self");
  dump_sub_sym(fp, t->ret, "Ret");
  dump_sub_sym(fp, t->cont, "Cont");
  dump_sub_sym(fp, t->init, "Init");
  fprintf(fp, "</TABLE><BR>\n");
}

void dump_sym_name(FILE *fp, Sym *s) {
  fprintf(fp, "<a href=\"#SYM_%d\">%s (%d)</a>", s->id, s->name ? s->name : ANON, s->id);
}

static void dump_var_type(FILE *fp, Var *v, int &wrote_one) {
  if (wrote_one) fprintf(fp, ", ");
  wrote_one = 1;
  dump_sym_name(fp, v->sym);
  fprintf(fp, " : ");
  dump_sym_name(fp, v->type);
  Vec<Sym *> consts;
  if (constant_info(v, consts)) {
    fprintf(fp, " constants {");
    forv_Sym(s, consts) {
      fprintf(fp, " ");
      fprint_imm(fp, s->imm);
    }
    fprintf(fp, " }");
  }
}

static void dump_var_type_list(FILE *fp, Vec<Var *> &vars) {
  int wrote_one = 0;
  for (int i = 0; i < vars.n; i++) dump_var_type(fp, vars[i], wrote_one);
}

static void dump_var_type_marg_positions(FILE *fp, Vec<MPosition *> &arg_positions, Map<MPosition *, Var *> &vars) {
  int wrote_one = 0;
  forv_MPosition(p, arg_positions) {
    Var *v = vars.get(p);
    if (v->live) dump_var_type(fp, v, wrote_one);
  }
}

static void dump_fun_name(FILE *fp, Fun *f) {
  const char *name = f->sym->name ? f->sym->name : ANON;
  const char *sname = f->sym->in ? f->sym->in->name : "";
  if (!sname) sname = ANON;
  fprintf(fp, "<a href=\"#FUN_%d\">%s::%s (%d)</a>", f->id, sname, name, f->id);
}

void dump_fun_list(FILE *fp, Vec<Fun *> &funs) {
  for (int i = 0; i < funs.n; i++) {
    if (i) fprintf(fp, ", ");
    dump_fun_name(fp, funs[i]);
  }
}

static void dump_functions(FILE *fp, Vec<Sym *> funs) {
  fprintf(fp,
          "<H1><A NAME=\"CONCRETE_FUNCTIONS\">Concrete Functions "
          "(Clones/Instantiations)</A></H1>\n\n");
  forv_Sym(fs, funs) {
    Fun *f = fs->fun;
    const char *name = f->sym->name ? f->sym->name : ANON;
    const char *sname = f->sym->in ? f->sym->in->name : "";
    if (!sname) sname = ANON;
    if (!f->sym->in)
      fprintf(fp, "<b><A NAME=\"FUN_%d\">%s::%s (%d)</A></b>\n", f->id, sname, name, f->id);
    else
      fprintf(fp, "<b><A NAME=\"FUN_%d\">%s</A></b>\n", f->id, name);
    fprintf(fp, "<TABLE BORDER=0, CELLSPACING=0, CELLPADDING=0>\n");
    if (f->ast && f->line()) fprintf(fp, "<TR><TD><TD>Location<TD>%s:%d\n", f->filename(), f->line());
    fprintf(fp, "<TR><TD WIDTH=30><TD WIDTH=150>Args<TD>\n");
    dump_var_type_marg_positions(fp, f->positional_arg_positions, f->args);
    fprintf(fp, "<TR><TD><TD>Rets<TD>\n");
    dump_var_type_list(fp, f->rets);
    fprintf(fp, "<TR><TD><TD>Calls<TD>\n");
    Vec<Fun *> funs;
    for (int i = 0; i < f->calls.n; i++)
      if (f->calls[i].value) funs.set_union(*f->calls[i].value);
    funs.set_to_vec();
    dump_fun_list(fp, funs);
    fprintf(fp, "<TR><TD><TD>Called by<TD>\n");
    funs.clear();
    forv_CallPoint(cp, f->called) funs.set_add(cp->fun);
    funs.set_to_vec();
    dump_fun_list(fp, funs);
    Vec<Var *> all_vars, vars;
    f->collect_Vars(all_vars);
    forv_Var(v, all_vars) if (v->sym->name && v->live && v->type && v->type->name && !v->sym->is_fun &&
                              !v->sym->is_symbol && !v->sym->is_constant && v->sym->type_kind == Type_NONE) vars.add(v);
    if (vars.n) {
      fprintf(fp, "<TR><TD<TD>Variables<TD>%s : %s<TD>\n", vars[0]->sym->name, vars[0]->type->name);
      for (int i = 1; i < vars.n; i++)
        fprintf(fp, "<TR><TD><TD><TD>%s : %s<TD>\n", vars[i]->sym->name, vars[i]->type->name);
    }
    if (f->ast) {
      fprintf(fp, "<TR><TD><TD>AST<TD>\n");
      fprintf(fp, "<ul class =\"mktree\" id=\"funtree%d\">\n", f->id);
      f->ast->html(fp, f);
      fprintf(fp, "</ul>\n");
      fprintf(fp,
              "<a href=\"#\" class=\"button\" "
              "onClick=\"expandTree('funtree%d'); return false;\">[Expand "
              "AST]</a>&nbsp;&nbsp;\n",
              f->id);
      fprintf(fp,
              "<a href=\"#\" class=\"button\" "
              "onClick=\"collapseTree('funtree%d'); return false;\">[Collapse "
              "AST]</a>\n",
              f->id);
    }
    fprintf(fp, "</TABLE><br>\n");
  }
}

static void dump_symbols(FILE *fp, FA *fa) {
  Vec<Sym *> syms, concrete_types, funs, globals, other, tmp;
  // collect concrete types
  forv_CreationSet(
      cs, fa->css) if (cs->type && !cs->type->is_symbol && !cs->type->is_fun && cs->type->type_kind != Type_PRIMITIVE &&
                       (cs->type->type_kind != Type_RECORD || (cs->type->creators.n && cs->type->type_live)))
      concrete_types.set_add(cs->type);
  concrete_types.set_to_vec();

  forv_EntrySet(es, fa->ess) if (es->fun->live) funs.set_add(es->fun->sym);
  funs.set_to_vec();

  // all live symbols
  syms.clear();
  forv_Fun(f, fa->funs) forv_Var(v, f->fa_all_Vars) if (v->live) syms.set_add(v->sym);

  // collect globals
  forv_Sym(s, syms) if (s) if (s->name && !s->is_constant && !s->is_fun && !s->is_symbol && !has_no_out_edges(s) &&
                               !s->type_kind != Type_NONE && !s->is_local) globals.set_add(s);
  globals.set_to_vec();

  // others
  syms.set_difference(globals, other);
  other.set_to_vec();

  qsort(concrete_types.v, concrete_types.n, sizeof(concrete_types[0]), compar_syms);
  qsort(funs.v, funs.n, sizeof(funs[0]), compar_syms);
  qsort(other.v, other.n, sizeof(other[0]), compar_syms);
  qsort(globals.v, globals.n, sizeof(globals[0]), compar_syms);

  // Concrete Types
  fprintf(fp,
          "<H1><A NAME=\"CONCRETE_TYPES\">Concrete Types "
          "(Clones/Instantiations)</A></H1>\n\n");
  forv_Sym(t, concrete_types) if (!is_internal_type(t)) dump_sym(fp, t);
  fprintf(fp, "\n");

  // Functions
  dump_functions(fp, funs);

#if FUNCTION_SYMBOLS
  // Function Symbols
  fprintf(fp, "<H1><A NAME=\"FUNCTION_SYMBOLS\">Function Symbols</A></H1>\n\n");
  forv_Sym(t, funs) dump_sym(fp, t);
  fprintf(fp, "\n");
#endif
  // Globals Symbols
  fprintf(fp, "<H1><A NAME=\"GLOBALS\">Global/Module Symbols</A></H1>\n\n");
  forv_Sym(t, globals) dump_sym(fp, t);
  fprintf(fp, "\n");
#ifdef LOCAL_SYMBOLS
  // Other Symbols
  fprintf(fp, "<H1><A NAME=\"SYMBOLS\">Local Symbols</A></H1>\n\n");
  forv_Sym(t, other) if (!is_internal_type(t)) dump_sym(fp, t);
  fprintf(fp, "\n");
#endif
}

void dump_html(FA *fa, cchar *fn, char *mktree_dir) {
  char hfn[512];
  sprintf(hfn, "%s.html", fn);
  FILE *fp = fopen(hfn, "w");
  dump_header(fp, fn, mktree_dir);
  dump_symbols(fp, fa);
  dump_footer(fp);
  fclose(fp);
}
