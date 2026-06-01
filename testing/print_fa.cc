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

// fa-init runs one analysis pass on the (sym___main__-rooted) test
// program and prints summary state. The user's `(entry %x)` from the
// .ir is not actually called from sym___main__ — it's just registered
// as a closure — so most fixtures only exercise sym___main__'s own
// trivial reply body. Driving user code from sym___main__ is a
// follow-on.
void print_fa_normalized(FILE *fp, IF1 *p) {
  // Build Funs for every closure (including the synthetic
  // sym___main__) and register with PDB.
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

  int rc = -2;
  if (if1->top && if1->top->fun) {
    rc = pdb->fa->analyze(if1->top->fun);
  }
  FA *fa = pdb->fa;

  fputs("(summary\n", fp);
  fprintf(fp, "  rc:              %d\n", rc);
  fprintf(fp, "  entry-sets:      %d\n", fa->ess.n);
  fprintf(fp, "  creation-sets:   %d\n", fa->css.n);
  fprintf(fp, "  global-avars:    %d\n", fa->global_avars.n);
  fprintf(fp, "  funs:            %d\n", fa->funs.n);
  fprintf(fp, "  basic-types:     %d\n", fa->basic_types.n);
  fputs(")\n\n", fp);

  // Per-EntrySet summary, sorted by (fun name, id).
  Vec<EntrySet *> ess;
  for (EntrySet *es : fa->ess) ess.add(es);
  if (ess.n > 1) qsort(ess.v, ess.n, sizeof(EntrySet *), compar_es_by_fun_then_id);
  fputs("(entry-sets\n", fp);
  for (EntrySet *es : ess) {
    fputs("  ", fp);
    if (es->fun && es->fun->sym) na.print_ref(fp, es->fun->sym);
    else fputs("?", fp);
    fprintf(fp, " args=%d rets=%d edges=%d creates=%d\n",
            es->args.n, es->rets.n, es->edges.length(), es->creates.n);
  }
  if (ess.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);

  // Closure registration summary — what's in the PDB.
  fputs("(closures-registered\n", fp);
  for (Sym *c : closures) {
    fputs("  ", fp);
    na.print_ref(fp, c);
    fprintf(fp, " args=%d fun=%s\n", c->has.n, c->fun ? "set" : "null");
  }
  if (closures.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);
}
