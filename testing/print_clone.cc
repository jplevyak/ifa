// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the post-clone state.

#include "ifadefs.h"

#include "clone.h"
#include "code.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "pdb.h"
#include "pnode.h"
#include "sym.h"
#include "var.h"
#include "testing/fa_setup.h"
#include "testing/print_clone.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// Same as print_dispatch's helpers — reused but kept local because
// importing them would mean exposing more.
static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

static int compar_fun_by_sym(const void *a, const void *b) {
  Fun *fa = *(Fun *const *)a, *fb = *(Fun *const *)b;
  cchar *na = (fa->sym && fa->sym->name) ? fa->sym->name : "";
  cchar *nb = (fb->sym && fb->sym->name) ? fb->sym->name : "";
  int c = strcmp(na, nb);
  if (c) return c;
  return (fa->id > fb->id) - (fa->id < fb->id);
}

static int compar_cs_by_sym(const void *a, const void *b) {
  CreationSet *ca = *(CreationSet *const *)a, *cb = *(CreationSet *const *)b;
  cchar *na = (ca->sym && ca->sym->name) ? ca->sym->name : "";
  cchar *nb = (cb->sym && cb->sym->name) ? cb->sym->name : "";
  int c = strcmp(na, nb);
  if (c) return c;
  return (ca->id > cb->id) - (ca->id < cb->id);
}

static void print_sym_name(FILE *fp, Sym *s) {
  if (!s) { fputs("?", fp); return; }
  if (s->name) fprintf(fp, "%s", s->name);
  else if (s->constant) fprintf(fp, "\"%s\"", s->constant);
  else fputs("(anon)", fp);
}

void print_clone_normalized(FILE *fp, IF1 *p) {
  // Same setup as fa-init/dispatch.
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

  fputs(";; phase: clone\n\n", fp);
  if (!if1->top || !if1->top->fun) {
    fputs("(skipped — no top closure)\n", fp);
    return;
  }
  int fa_rc = pdb->fa->analyze(if1->top->fun);
  int clone_rc = clone(pdb->fa);
  FA *fa = pdb->fa;

  fprintf(fp, "(summary fa_rc=%d clone_rc=%d funs=%d css=%d ess=%d)\n\n",
          fa_rc, clone_rc, fa->funs.n, fa->css.n, fa->ess.n);

  // ---------- CS equivalence classes ----------
  // cs->equiv groups CSes that ended up sharing a concrete type.
  // We pick the rep (first member of the class) and dedupe.
  Vec<CreationSet *> reps;
  for (CreationSet *cs : fa->css) {
    if (!cs->equiv || cs->equiv->n == 0) {
      // Singleton, no equiv structure.
      if (!reps.in(cs)) reps.add(cs);
      continue;
    }
    CreationSet *rep = cs->equiv->v[0];
    if (!reps.in(rep)) reps.add(rep);
  }
  if (reps.n > 1) qsort(reps.v, reps.n, sizeof(CreationSet *), compar_cs_by_sym);

  fputs("(cs-equiv\n", fp);
  for (CreationSet *rep : reps) {
    fputs("  class[", fp);
    print_sym_name(fp, rep->sym);
    fputs("] members=", fp);
    if (rep->equiv && rep->equiv->n > 1) {
      fprintf(fp, "%d type=", rep->equiv->n);
    } else {
      fputs("1 type=", fp);
    }
    print_sym_name(fp, rep->type);
    fputc('\n', fp);
  }
  if (reps.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);

  // ---------- ES equivalence classes per Fun ----------
  fputs("(es-equiv\n", fp);
  Vec<Fun *> funs;
  for (Fun *f : fa->funs) funs.add(f);
  if (funs.n > 1) qsort(funs.v, funs.n, sizeof(Fun *), compar_fun_by_sym);
  for (Fun *f : funs) {
    fputs("  fun ", fp);
    print_sym_name(fp, f->sym);
    fprintf(fp, " ess=%d equiv-classes=%d\n", f->ess.n, f->equiv_sets.n);
  }
  if (funs.n == 0) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);

  // ---------- call graph ----------
  fputs("(call-graph\n", fp);
  for (Fun *f : funs) {
    if (f->calls.n == 0) continue;
    fputs("  fun ", fp);
    print_sym_name(fp, f->sym);
    fputc('\n', fp);
    // calls is Map<PNode*, Vec<Fun*>*>. Iterate and sort target funs
    // by name for stable diff.
    Vec<PNode *> pns;
    for (int i = 0; i < f->calls.n; i++)
      if (f->calls[i].key && f->calls[i].value) pns.add(f->calls[i].key);
    // Sort by id for stable order; PNode ids are deterministic per
    // analyze run.
    for (int i = 1; i < pns.n; i++)
      for (int j = i; j > 0 && pns[j]->id < pns[j - 1]->id; j--) {
        PNode *t = pns[j]; pns[j] = pns[j - 1]; pns[j - 1] = t;
      }
    for (PNode *pn : pns) {
      Vec<Fun *> *vf = f->calls.get(pn);
      if (!vf || !vf->n) continue;
      // Sort targets.
      Vec<Fun *> targets;
      for (Fun *t : *vf) if (t) targets.add(t);
      if (targets.n > 1) qsort(targets.v, targets.n, sizeof(Fun *), compar_fun_by_sym);
      fputs("    pnode-send →", fp);
      for (Fun *t : targets) { fputc(' ', fp); print_sym_name(fp, t->sym); }
      fputc('\n', fp);
    }
  }
  bool any_calls = false;
  for (Fun *f : funs) if (f->calls.n) { any_calls = true; break; }
  if (!any_calls) fputs("  (no inter-fun calls)\n", fp);
  fputs(")\n\n", fp);

  // ---------- new Funs ----------
  // pdb->funs grows as cloning creates new Funs. Compare to the
  // closures we registered above to find any new ones.
  fputs("(new-funs\n", fp);
  bool any_new = false;
  for (Fun *f : pdb->funs) {
    if (!f->sym) continue;
    bool original = false;
    for (Sym *c : closures) if (c == f->sym) { original = true; break; }
    if (original) continue;
    any_new = true;
    fputs("  ", fp);
    print_sym_name(fp, f->sym);
    if (f->wraps && f->wraps->sym) {
      fputs(" cloned-from=", fp);
      print_sym_name(fp, f->wraps->sym);
    }
    fputc('\n', fp);
  }
  if (!any_new) fputs("  (none)\n", fp);
  fputs(")\n", fp);
}
