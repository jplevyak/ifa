// SPDX-License-Identifier: BSD-3-Clause
// Phase printers for DCE + frequency estimation.

#include "ifadefs.h"

#include "clone.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "optimize/dead.h"
#include "optimize/dom.h"
#include "optimize/inline.h"
#include "pdb.h"
#include "pnode.h"
#include "sym.h"
#include "var.h"
#include "testing/fa_setup.h"
#include "testing/print_dce.h"

#include <math.h>
#include <stdio.h>
#include <string.h>

// Shared setup: build per-closure Funs, run FA::analyze, then clone.
// Returns the FA after these steps so the caller can run further
// passes (mark_live_*, frequency_estimation) before printing.
static FA *common_setup(IF1 *p, int &fa_rc, int &clone_rc) {
  Vec<Sym *> closures;
  for (Sym *c : p->allclosures) closures.add(c);
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
  if (!if1->top || !if1->top->fun) { fa_rc = -2; clone_rc = -2; return pdb->fa; }
  fa_rc = pdb->fa->analyze(if1->top->fun);
  clone_rc = clone(pdb->fa);
  return pdb->fa;
}

static int compar_fun_by_sym(const void *a, const void *b) {
  Fun *fa = *(Fun *const *)a, *fb = *(Fun *const *)b;
  cchar *na = (fa->sym && fa->sym->name) ? fa->sym->name : "";
  cchar *nb = (fb->sym && fb->sym->name) ? fb->sym->name : "";
  int c = strcmp(na, nb);
  if (c) return c;
  return (fa->id > fb->id) - (fa->id < fb->id);
}

// ---------------------------------------------------------------------------
// dce
// ---------------------------------------------------------------------------

void print_dce_normalized(FILE *fp, IF1 *p) {
  int fa_rc = 0, clone_rc = 0;
  FA *fa = common_setup(p, fa_rc, clone_rc);
  // ifa_analyze ordering: build_cfg_dominators before mark_live_code
  // (mark_live_pnodes uses dom info for IF/GOTO liveness).
  for (Fun *f : fa->funs) build_cfg_dominators(f);
  mark_live_code(fa);
  mark_live_types(fa);
  mark_live_funs(fa);

  fputs(";; phase: dce\n\n", fp);
  fprintf(fp, "(summary fa_rc=%d clone_rc=%d funs=%d css=%d ess=%d)\n\n",
          fa_rc, clone_rc, fa->funs.n, fa->css.n, fa->ess.n);

  // Per-Fun PNode live/dead counts.
  Vec<Fun *> funs;
  for (Fun *f : fa->funs) funs.add(f);
  if (funs.n > 1) qsort(funs.v, funs.n, sizeof(Fun *), compar_fun_by_sym);

  fputs("(per-fun-live\n", fp);
  for (Fun *f : funs) {
    int live_pn = 0, dead_pn = 0, live_var = 0, dead_var = 0;
    for (PNode *pn : f->fa_all_PNodes) (pn->live ? live_pn : dead_pn)++;
    for (Var *v : f->fa_all_Vars) (v->live ? live_var : dead_var)++;
    fputs("  fun ", fp);
    fprintf(fp, "%s", f->sym && f->sym->name ? f->sym->name : "?");
    fprintf(fp, " live=%d/%d pnodes=%d/%d vars=%d/%d\n",
            f->live, 1,
            live_pn, live_pn + dead_pn,
            live_var, live_var + dead_var);
  }
  if (funs.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);

  // Type liveness summary — counts of type Syms marked live vs not.
  int live_types = 0, dead_types = 0;
  for (Sym *s : p->allsyms) if (s->type_kind) {
    if (s->type_live) live_types++;
    else dead_types++;
  }
  fprintf(fp, "(types live=%d dead=%d)\n\n", live_types, dead_types);

  // Live funs list (Fun::live).
  fputs("(live-funs\n", fp);
  for (Fun *f : funs) {
    cchar *n = f->sym && f->sym->name ? f->sym->name : "?";
    fprintf(fp, "  %s: %s\n", n, f->live ? "live" : "dead");
  }
  if (funs.n == 0) fputs("  (none)\n", fp);
  fputs(")\n", fp);
}

// ---------------------------------------------------------------------------
// freq
// ---------------------------------------------------------------------------

// Format a float in a way that round-trips deterministically across
// runs and is short enough to read. Goldens lock these values, so we
// pick a stable rendering (`%g` with 6 sig figs).
static void print_freq(FILE *fp, float f) {
  if (f == 0.0f) fputs("0", fp);
  else fprintf(fp, "%.6g", f);
}

void print_freq_normalized(FILE *fp, IF1 *p) {
  int fa_rc = 0, clone_rc = 0;
  FA *fa = common_setup(p, fa_rc, clone_rc);
  // Match ifa_analyze ordering: build_cfg_dominators then
  // mark_live_code, then frequency_estimation. mark_live_funs runs
  // later (in ifa_optimize) and is NOT done here — calling it now
  // would prune fa->funs and leave dangling Fun::calls entries that
  // build_call_dominators would deref.
  for (Fun *f : fa->funs) build_cfg_dominators(f);
  mark_live_code(fa);
  frequency_estimation(fa);

  fputs(";; phase: freq\n\n", fp);
  fprintf(fp, "(summary fa_rc=%d clone_rc=%d funs=%d)\n\n",
          fa_rc, clone_rc, fa->funs.n);

  Vec<Fun *> funs;
  for (Fun *f : fa->funs) funs.add(f);
  if (funs.n > 1) qsort(funs.v, funs.n, sizeof(Fun *), compar_fun_by_sym);

  // Per-fun frequency.
  fputs("(freq-funs\n", fp);
  for (Fun *f : funs) {
    cchar *n = f->sym && f->sym->name ? f->sym->name : "?";
    fputs("  ", fp);
    fputs(n, fp);
    fputs(" ", fp);
    print_freq(fp, f->execution_frequency);
    fputc('\n', fp);
  }
  if (funs.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);

  // Per-fun max PNode frequency (don't list every PNode — just the
  // peak so the golden stays small).
  fputs("(freq-pnode-peaks\n", fp);
  for (Fun *f : funs) {
    float peak = 0;
    for (PNode *pn : f->fa_all_PNodes)
      if (pn->execution_frequency > peak) peak = pn->execution_frequency;
    cchar *n = f->sym && f->sym->name ? f->sym->name : "?";
    fputs("  ", fp);
    fputs(n, fp);
    fputs(" peak=", fp);
    print_freq(fp, peak);
    fputc('\n', fp);
  }
  if (funs.n == 0) fputs("  (none)\n", fp);
  fputs(")\n", fp);
}
