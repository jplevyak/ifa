// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for build_patterns. Builds Funs for every closure,
// registers them with the PDB, allocates a stub FA, then runs the
// pattern-build pass and prints `Patterns::types` + `MType::funs`.

#include "ifadefs.h"

#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "pattern.h"
#include "pdb.h"
#include "sym.h"
#include "var.h"
#include "testing/print_patterns.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

static void ensure_var(Sym *s) {
  if (s && !s->var) s->var = new Var(s);
}

static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

static void format_mposition(FILE *fp, MPosition *mp) {
  fputs("[", fp);
  for (int i = 0; i < mp->pos.n; i++) {
    if (i) fputc(',', fp);
    const void *p = mp->pos[i];
    if (is_intPosition(p)) fprintf(fp, "%d", (int)Position2int(p));
    else fprintf(fp, "\"%s\"", (cchar *)p);
  }
  fputs("]", fp);
}

// Stable string form for sorting MPositions (5-digit zero-padded ints
// so lexical order matches numeric).
static int compar_mposition_str(const void *a, const void *b) {
  MPosition *ma = *(MPosition *const *)a, *mb = *(MPosition *const *)b;
  char sa[128], sb[128]; int na = 0, nb = 0;
  auto fmt = [](char *buf, int &n, int cap, MPosition *m) {
    for (int i = 0; i < m->pos.n && n < cap - 16; i++) {
      const void *p = m->pos[i];
      if (i) buf[n++] = ',';
      if (is_intPosition(p))
        n += snprintf(buf + n, cap - n, "%05d", (int)Position2int(p));
      else
        n += snprintf(buf + n, cap - n, "\"%s\"", (cchar *)p);
    }
    buf[n] = 0;
  };
  fmt(sa, na, sizeof(sa), ma);
  fmt(sb, nb, sizeof(sb), mb);
  return strcmp(sa, sb);
}

static int compar_sym_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

void print_patterns_normalized(FILE *fp, IF1 *p) {
  fputs(";; phase: patterns\n\n", fp);

  // Phase 1: build per-closure Funs and register with the PDB so
  // build_patterns(fa) sees them via fa->pdb->funs.
  Vec<Sym *> closures;
  for (Sym *c : p->allclosures) if (c->code) closures.add(c);
  if (closures.n > 1)
    qsort(closures.v, closures.n, sizeof(Sym *), compar_closure_by_name);
  for (Sym *c : closures) {
    Fun *f = new Fun(c, FUN_BUILD_CFG_ONLY);
    for (Sym *a : c->has) ensure_var(a);
    ensure_var(c->ret);
    pdb->add(f);
  }

  // Phase 2: arg-positions then patterns.
  FA *fa = new FA(pdb);
  pdb->fa = fa;
  build_arg_positions(fa);
  build_patterns(fa);

  NameAssigner na;
  na.assign_all(p);

  // Phase 3: print the reverse index. fa->patterns->types is the set
  // of dispatch-type Syms seen by build_patterns; each has a non-null
  // ->match_type.
  Vec<Sym *> types;
  for (Sym *t : fa->patterns->types) types.add(t);
  if (types.n > 1) qsort(types.v, types.n, sizeof(Sym *), compar_sym_by_name);

  bool any = false;
  for (Sym *t : types) {
    if (!t->match_type) continue;
    any = true;
    fputs("(mtype ", fp);
    na.print_ref(fp, t);
    fputc('\n', fp);

    // Collect & sort MPositions.
    Vec<MPosition *> mps;
    typedef MapElem<MPosition *, Vec<Fun *> *> MPFunsElem;
    form_Map(MPFunsElem, e, t->match_type->funs)
      if (e->value) mps.add(e->key);
    if (mps.n > 1) qsort(mps.v, mps.n, sizeof(MPosition *), compar_mposition_str);

    for (MPosition *mp : mps) {
      fputs("  pos", fp);
      format_mposition(fp, mp);
      fputs(":", fp);
      // Sort the funs by closure-sym name for stable diff.
      Vec<Fun *> *funs = t->match_type->funs.get(mp);
      Vec<Fun *> sorted;
      for (Fun *fn : *funs) sorted.add(fn);
      if (sorted.n > 1) {
        // Insertion sort by name.
        for (int i = 1; i < sorted.n; i++)
          for (int j = i; j > 0; j--) {
            cchar *nm = sorted[j]->sym->name ? sorted[j]->sym->name : "";
            cchar *pn = sorted[j - 1]->sym->name ? sorted[j - 1]->sym->name : "";
            if (strcmp(nm, pn) >= 0) break;
            Fun *tmp = sorted[j]; sorted[j] = sorted[j - 1]; sorted[j - 1] = tmp;
          }
      }
      for (Fun *fn : sorted) { fputc(' ', fp); na.print_ref(fp, fn->sym); }
      fputc('\n', fp);
    }
    fputs(")\n\n", fp);
  }
  if (!any) fputs("(empty)\n\n", fp);
}
