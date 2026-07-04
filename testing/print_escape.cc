// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the in-FA escape analysis (Phases 2-4 of
// ESCAPE_PLAN.md).  Forces `ifa_escape_in_fa = 1` so the
// pre-clone escape pass runs even when the build hasn't
// flipped the global default.  Dumps:
//
//   (escape
//     fun NAME ess=N escape-sig=[E|N E|N ...]
//     ...
//   )
//
// `escape-sig[i]` is the per-formal escape status (E = Escape,
// N = NoEscape) for each positional formal of the function,
// JOINed across the function's EntrySets — same view that
// codegen sees via Fun::arg_escapes.

#include "ifadefs.h"

#include "analysis/escape.h"
#include "clone.h"
#include "code.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "pdb.h"
#include "pnode.h"
#include "sym.h"
#include "var.h"
#include "testing/fa_setup.h"
#include "testing/print_escape.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// issues/010 (ifa): same-name Sym pairs tie under strcmp alone; without
// a deterministic secondary key, qsort's tie-break falls through to
// this Vec's pre-sort (pointer/hash-keyed Map iteration) order, which
// can vary across runs/builds. Sym::id (assignment order, never
// address-dependent) makes this a total order.
static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  int c = strcmp(na, nb);
  if (c) return c;
  return (sa->id > sb->id) - (sa->id < sb->id);
}

static void print_sym_name(FILE *fp, Sym *s) {
  if (!s) { fputs("?", fp); return; }
  if (s->name) fprintf(fp, "%s", s->name);
  else fputs("(anon)", fp);
}

void print_escape_normalized(FILE *fp, IF1 *p) {
  // Standard fa-pipeline setup.
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
    if (c->cont && !c->cont->var) c->cont->var = new Var(c->cont);
    pdb->add(f);
  }

  fputs(";; phase: escape\n\n", fp);
  if (!if1->top || !if1->top->fun) {
    fputs("(skipped — no top closure)\n", fp);
    return;
  }

  // Force the in-FA escape pass on for this phase.  Snapshot
  // and restore so leaving the test environment doesn't affect
  // unrelated phases.
  int prev_flag = ifa_escape_in_fa;
  ifa_escape_in_fa = 1;
  int fa_rc = pdb->fa->analyze(if1->top->fun);
  FA *fa = pdb->fa;
  // compute_escape runs as part of ifa_analyze in production,
  // but here we call it directly so the test phase doesn't
  // depend on the orchestration sequencing.  It's idempotent
  // — running twice gives the same result.
  compute_escape(fa);
  ifa_escape_in_fa = prev_flag;

  // Gather funs and sort by source-symbol name for stable
  // output.
  Vec<Fun *> funs;
  for (Fun *f : fa->funs) if (f) funs.add(f);
  // Local lambdaless sort: by name then id.
  for (int i = 1; i < funs.n; i++) {
    Fun *cur = funs[i];
    int j = i - 1;
    cchar *cn = cur->sym && cur->sym->name ? cur->sym->name : "";
    while (j >= 0) {
      cchar *jn = funs[j]->sym && funs[j]->sym->name
                      ? funs[j]->sym->name : "";
      int c = strcmp(jn, cn);
      if (c < 0) break;
      if (c == 0 && funs[j]->id <= cur->id) break;
      funs[j + 1] = funs[j];
      j--;
    }
    funs[j + 1] = cur;
  }

  fprintf(fp, "(summary fa_rc=%d funs=%d)\n\n", fa_rc, fa->funs.n);

  fputs("(escape\n", fp);
  for (Fun *f : funs) {
    if (!f || !f->sym) continue;
    // Count EntrySets via a local pass — pre/post-clone the
    // f->ess vector may not yet be populated (clone's
    // fixup_clone_ess does it).  Use fa->ess and filter by
    // fun for a stable count.
    int ess_count = 0;
    for (EntrySet *es : fa->ess)
      if (es && es->fun == f) ess_count++;

    fputs("  fun ", fp);
    print_sym_name(fp, f->sym);
    fprintf(fp, " ess=%d", ess_count);

    // arg_escapes signature (left empty by IFA for funs with
    // no formals or no live EntrySets).
    if (f->arg_escapes.n > 0) {
      fputs(" escape-sig=[", fp);
      for (int i = 0; i < f->arg_escapes.n; i++) {
        if (i) fputc(' ', fp);
        fputc(f->arg_escapes[i] ? 'E' : 'N', fp);
      }
      fputc(']', fp);
    } else {
      fputs(" escape-sig=[]", fp);
    }
    fputc('\n', fp);
  }
  fputs(")\n", fp);
}
