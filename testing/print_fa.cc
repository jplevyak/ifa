// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for FA::analyze.

#include "ifadefs.h"

#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "pdb.h"
#include "pnode.h"
#include "sym.h"
#include "var.h"
#include "testing/print_fa.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

static int compar_es_by_fun_then_id(const void *a, const void *b) {
  EntrySet *ea = *(EntrySet *const *)a, *eb = *(EntrySet *const *)b;
  cchar *na = (ea->fun && ea->fun->sym && ea->fun->sym->name) ? ea->fun->sym->name : "";
  cchar *nb = (eb->fun && eb->fun->sym && eb->fun->sym->name) ? eb->fun->sym->name : "";
  int c = strcmp(na, nb);
  if (c) return c;
  return (ea->id > eb->id) - (ea->id < eb->id);
}

// NOTE: fa-init is currently a *setup-only* phase. Running the full
// FA::analyze loop requires more environment setup than the test
// harness currently provides (sym___main__ needs to be wired into a
// Fun, abstract_type needs to be set on the right Syms, etc.) and a
// trivial fixture asserts in update_in() because the top-edge AVar
// has a NULL abstract_type. See phases/05_fa_analyze.md §7. For now
// this printer verifies that the pre-analysis setup chain
// (init_default_builtin_types → finalize_types → build_type_hierarchy
// → if1_finalize_*) runs cleanly and reports the resulting IF1 state.
void print_fa_normalized(FILE *fp, IF1 *p) {
  // Build Funs and register with PDB so a future FA::analyze pass
  // would see them.
  Vec<Sym *> closures;
  for (Sym *c : p->allclosures) if (c->code) closures.add(c);
  if (closures.n > 1)
    qsort(closures.v, closures.n, sizeof(Sym *), compar_closure_by_name);
  for (Sym *c : closures) {
    Fun *f = new Fun(c, FUN_BUILD_ALL);
    if (!c->var) c->var = new Var(c);
    for (Sym *a : c->has) if (!a->var) a->var = new Var(a);
    if (c->ret && !c->ret->var) c->ret->var = new Var(c->ret);
    pdb->add(f);
  }

  NameAssigner na;
  na.assign_all(p);

  fputs(";; phase: fa-init\n\n", fp);
  fputs(";; FA::analyze() is not yet invoked by the test harness — see\n", fp);
  fputs(";; ifa/testing/phases/05_fa_analyze.md §7. This printer locks in\n", fp);
  fputs(";; pre-analysis state (post-finalize, post-build_type_hierarchy)\n", fp);
  fputs(";; so any change to that setup is observable.\n\n", fp);

  fputs("(closures\n", fp);
  for (Sym *c : closures) {
    fputs("  ", fp);
    na.print_ref(fp, c);
    fprintf(fp, " args=%d ret=%s fun=%s\n",
            c->has.n,
            c->ret && c->ret->name ? c->ret->name : "?",
            c->fun ? "set" : "null");
  }
  if (closures.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);

  fprintf(fp, "(pdb funs=%d)\n\n", pdb->funs.n);

  // Count syms by interesting buckets.
  int constants = 0, symbols = 0, funs = 0, types = 0, has_meta = 0;
  for (Sym *s : p->allsyms) {
    if (s->is_constant) constants++;
    if (s->is_symbol) symbols++;
    if (s->is_fun) funs++;
    if (s->type_kind) types++;
    if (s->meta_type) has_meta++;
  }
  fputs("(syms-classification\n", fp);
  fprintf(fp, "  total:     %d\n", p->allsyms.n);
  fprintf(fp, "  constants: %d\n", constants);
  fprintf(fp, "  symbols:   %d\n", symbols);
  fprintf(fp, "  funs:      %d\n", funs);
  fprintf(fp, "  types:     %d\n", types);
  fprintf(fp, "  has-meta:  %d\n", has_meta);
  fputs(")\n\n", fp);
}
