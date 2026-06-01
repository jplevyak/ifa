// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for FA dispatch results.

#include "ifadefs.h"

#include "code.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"
#include "var.h"
#include "testing/fa_setup.h"
#include "testing/print_dispatch.h"
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

static int compar_pn_by_id(const void *a, const void *b) {
  PNode *pa = *(PNode *const *)a, *pb = *(PNode *const *)b;
  return (pa->id > pb->id) - (pa->id < pb->id);
}

// Print "@__main__#0" / "%add#3" — function name + ES ordinal among
// its specializations (so two %add ESes are distinguishable in the
// golden).
static void print_es_ref(FILE *fp, EntrySet *es,
                         Map<EntrySet *, int> &es_ordinal,
                         NameAssigner &na) {
  if (es->fun && es->fun->sym) na.print_ref(fp, es->fun->sym);
  else fputs("?", fp);
  fprintf(fp, "#%d", es_ordinal.get(es));
}

// Pick a stable, short label for what a Sym carries — sym->name if
// available, else its constant form, else "?". Used to print the
// rval list at each SEND.
static void print_rval_sym(FILE *fp, Sym *s) {
  if (!s) { fputs("?", fp); return; }
  if (s->name) fprintf(fp, "%s", s->name);
  else if (s->constant) fprintf(fp, "\"%s\"", s->constant);
  else if (s->is_constant) fputs("(imm)", fp);
  else fputs("?", fp);
}

// Per-CS sym name for an AType (the "what creation sites contribute
// here" view). Returns at most a few CSes; collapses larger sets to
// "{n}".
static void print_atype_brief(FILE *fp, AType *t) {
  if (!t || t->n == 0) { fputs("∅", fp); return; }
  if (t->n > 6) { fprintf(fp, "{%d}", t->n); return; }
  fputc('{', fp);
  for (int i = 0; i < t->sorted.n; i++) {
    if (i) fputc(' ', fp);
    Sym *s = t->sorted[i]->sym;
    if (s->name) fprintf(fp, "%s", s->name);
    else if (s->constant) fprintf(fp, "\"%s\"", s->constant);
    else fputs("?", fp);
  }
  fputc('}', fp);
}

void print_dispatch_normalized(FILE *fp, IF1 *p) {
  // Same setup as print_fa_normalized: build Funs, run FA.
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

  fputs(";; phase: dispatch\n\n", fp);

  if (!if1->top || !if1->top->fun) {
    fputs("(skipped — no top closure)\n", fp);
    return;
  }
  int rc = pdb->fa->analyze(if1->top->fun);
  FA *fa = pdb->fa;

  NameAssigner na;
  na.assign_all(p);

  // Sort ESes deterministically and assign per-Fun ordinals so the
  // golden can distinguish e.g. `%add#0` from `%add#1` when the
  // splitter has produced two specializations.
  Vec<EntrySet *> ess;
  for (EntrySet *es : fa->ess) ess.add(es);
  if (ess.n > 1) qsort(ess.v, ess.n, sizeof(EntrySet *), compar_es_by_fun_then_id);

  Map<EntrySet *, int> es_ordinal;
  Map<Sym *, int> fun_counter;
  for (EntrySet *es : ess) {
    Sym *s = es->fun ? es->fun->sym : 0;
    int n = fun_counter.get(s);
    es_ordinal.put(es, n);
    fun_counter.put(s, n + 1);
  }

  fprintf(fp, "(summary rc=%d ess=%d edges=", rc, ess.n);
  int total_edges = 0;
  for (EntrySet *es : ess) total_edges += es->edges.length();
  fprintf(fp, "%d)\n\n", total_edges);

  for (EntrySet *es : ess) {
    fputs("(es ", fp);
    print_es_ref(fp, es, es_ordinal, na);
    fputc('\n', fp);

    // Group out-edges by PNode for stable per-call-site output.
    Vec<PNode *> pnodes;
    typedef MapElem<PNode *, Vec<AEdge *> *> PNAEElem;
    form_Map(PNAEElem, e, es->out_edge_map) if (e->value && e->value->n) pnodes.add(e->key);
    if (pnodes.n > 1) qsort(pnodes.v, pnodes.n, sizeof(PNode *), compar_pn_by_id);

    if (pnodes.n == 0) fputs("  (no out edges)\n", fp);
    for (PNode *pn : pnodes) {
      Vec<AEdge *> *edges = es->out_edge_map.get(pn);
      if (!edges || !edges->n) continue;

      // Describe the SEND.
      Code *c = pn->code;
      if (!c) { fputs("  ?\n", fp); continue; }
      fputs("  send", fp);
      for (Var *v : pn->rvals) {
        if (!v || !v->sym) continue;
        fputc(' ', fp);
        print_rval_sym(fp, v->sym);
      }
      if (pn->lvals.n) {
        fputs(" =>", fp);
        for (Var *v : pn->lvals) {
          if (!v || !v->sym) continue;
          fputc(' ', fp);
          print_rval_sym(fp, v->sym);
        }
      }
      // Show what FA found at this site.
      fputs("\n    → ", fp);
      for (int i = 0; i < edges->n; i++) {
        AEdge *ae = edges->v[i];
        if (!ae || !ae->to) continue;
        if (i) fputs(", ", fp);
        print_es_ref(fp, ae->to, es_ordinal, na);
      }
      fputc('\n', fp);
      // For each outgoing edge, dump the actual->formal arg ATypes
      // (the matched filter the splitter looked at). Sort by formal
      // MPosition for stability.
      for (AEdge *ae : *edges) {
        if (!ae || !ae->to) continue;
        Vec<MPosition *> ps;
        form_MPositionAType(it, ae->initial_types) if (it->key) ps.add(it->key);
        if (ps.n == 0) continue;
        fputs("      args→ ", fp);
        for (int i = 0; i < ps.n; i++) {
          if (i) fputc(' ', fp);
          MPosition *mp = ps[i];
          if (mp->pos.n == 1 && is_intPosition(mp->pos[0]))
            fprintf(fp, "[%d]:", (int)Position2int(mp->pos[0]));
          else
            fputs("[?]:", fp);
          print_atype_brief(fp, ae->initial_types.get(mp));
        }
        fputc('\n', fp);
        break;  // one row per edge is enough for the golden
      }
    }
    fputs(")\n\n", fp);
  }
}
