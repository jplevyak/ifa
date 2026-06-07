// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for FA convergence (issue 003).
//
// Runs the same setup as print_fa but enables the FAPassEvent sidecar
// around the FA::analyze call. The golden then locks:
//   - total pass count
//   - per-stage split totals
//   - per-pass history (which stage fired, how many splits)
//
// Time fields are deliberately suppressed (wallclock, non-deterministic).

#include "ifadefs.h"

#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "pdb.h"
#include "sym.h"
#include "var.h"
#include "testing/fa_setup.h"
#include "testing/print_fa_converge.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

static cchar *stage_name(FAPassStage s) {
  switch (s) {
    case FAPassStage::TYPE_CONFLUENCE: return "type";
    case FAPassStage::MARK_TYPE: return "mark-type";
    case FAPassStage::SETTER: return "setter";
    case FAPassStage::SETTER_OF_SETTER: return "setter-of-setter";
    case FAPassStage::MARK_SETTER: return "mark-setter";
    case FAPassStage::MARK_SETTER_OF_SETTER: return "mark-setter-of-setter";
    case FAPassStage::VIOLATION: return "violation";
  }
  return "?";
}

static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

void print_fa_converge_normalized(FILE *fp, IF1 *p) {
  // Same Fun-build dance as print_fa_normalized.
  Vec<Sym *> closures;
  for (Sym *c : p->allclosures) closures.add(c);
  if (closures.n > 1) qsort(closures.v, closures.n, sizeof(Sym *), compar_closure_by_name);
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

  fputs(";; phase: fa-converge\n\n", fp);

  fa_events_reset();
  fa_events_enable();
  int rc = -2;
  if (if1->top && if1->top->fun) rc = pdb->fa->analyze(if1->top->fun);
  fa_events_disable();

  const Vec<FAPassEvent *> &events = fa_events_get();

  // Determine the highest pass index seen (zero if no events).
  int max_pass = 0;
  for (FAPassEvent *e : events) if (e->pass > max_pass) max_pass = e->pass;

  // Per-stage totals.
  constexpr int num_stages = (int)FAPassStage::VIOLATION + 1;
  int total_by_stage[num_stages] = {0};
  for (FAPassEvent *e : events) total_by_stage[(int)e->stage] += e->splits;

  fputs("(pass-counts\n", fp);
  fprintf(fp, "  rc:                %d\n", rc);
  fprintf(fp, "  total-passes:      %d\n", max_pass);
  fprintf(fp, "  events:            %d\n", events.n);
  for (int s = 0; s < num_stages; s++)
    if (total_by_stage[s])
      fprintf(fp, "  splits[%s]: %d\n", stage_name((FAPassStage)s), total_by_stage[s]);
  fputs(")\n\n", fp);

  fputs("(history\n", fp);
  if (events.n == 0) {
    fputs("  (no events — converged in one pass)\n", fp);
  } else {
    // violations= now reports type_violations.set_count() — the live
    // element count, not the open-addressed table capacity (the
    // capacity oscillated with allocation order and is what made the
    // numbers look non-deterministic; see issue 009).
    for (FAPassEvent *e : events) {
      fprintf(fp,
              "  pass %d %s splits=%d ess=%d→%d css=%d→%d violations=%d→%d\n",
              e->pass, stage_name(e->stage), e->splits,
              e->ess_before, e->ess_after,
              e->css_before, e->css_after,
              e->violations_before, e->violations_after);
    }
  }
  fputs(")\n", fp);
}
