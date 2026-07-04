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
#include "testing/fa_setup.h"
#include "testing/print_fa.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// issues/010 (ifa): same-name Sym pairs (e.g. a "%next" type Sym and a
// "#next" selector Sym) tie under strcmp alone; without a deterministic
// secondary key, qsort's tie-break falls through to this Vec's pre-sort
// (pointer/hash-keyed Map iteration) order. Here that order feeds Fun
// registration *before* fa->analyze runs, so the tie doesn't just affect
// print formatting -- it can change real analysis counts (creates=N)
// from run to run. Sym::id (assignment order) makes this a total order.
static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  int c = strcmp(na, nb);
  if (c) return c;
  return (sa->id > sb->id) - (sa->id < sb->id);
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
  // sym___main__). Sort by name first. Skip the spliced user entry
  // when building Funs — its Code tree is now a sub-tree of
  // sym___main__'s code, and building a CFG on it twice corrupts the
  // per-Code pn back-link. We still keep it in the printed list so
  // the golden shows it was registered.
  Vec<Sym *> closures;
  for (Sym *c : p->allclosures) closures.add(c);
  if (closures.n > 1)
    qsort(closures.v, closures.n, sizeof(Sym *), compar_closure_by_name);
  for (Sym *c : closures) {
    if (c == fa_setup_user_entry) continue;
    if (!c->code) continue;
    Fun *f = new Fun(c, FUN_BUILD_ALL);
    if (!c->var) c->var = new Var(c);
    for (Sym *a : c->has) if (!a->var) a->var = new Var(a);
    if (c->ret && !c->ret->var) c->ret->var = new Var(c->ret);
    // analyze_edge() at fa.cc:2097 dereferences fun->sym->cont->var
    // when a call edge to this fun is analyzed — without a Var here,
    // a real closure dispatch into this Fun segfaults the moment FA
    // tries to thread the continuation.
    if (c->cont && !c->cont->var) c->cont->var = new Var(c->cont);
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

  // Closure registration summary — what's in the PDB. Mark the
  // spliced user entry with `(spliced)`; it's reachable from
  // sym___main__'s body but doesn't have its own Fun.
  fputs("(closures-registered\n", fp);
  for (Sym *c : closures) {
    fputs("  ", fp);
    na.print_ref(fp, c);
    if (c == fa_setup_user_entry)
      fprintf(fp, " args=%d (spliced into @__main__)\n", c->has.n);
    else
      fprintf(fp, " args=%d fun=%s\n", c->has.n, c->fun ? "set" : "null");
  }
  if (closures.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);
}
