// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for build_arg_positions.

#include "ifadefs.h"

#include "fun.h"
#include "if1.h"
#include "pattern.h"
#include "pnode.h"
#include "sym.h"
#include "var.h"
#include "testing/print_argpos.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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
    if (is_intPosition(p)) {
      fprintf(fp, "%d", (int)Position2int(p));
    } else {
      fprintf(fp, "\"%s\"", (cchar *)p);
    }
  }
  fputs("]", fp);
}

static int compar_mposition_str(const void *a, const void *b) {
  // Sort by string form: produces stable, name-order output.
  // Build small strings for the two positions and compare.
  MPosition *ma = *(MPosition *const *)a;
  MPosition *mb = *(MPosition *const *)b;
  char sa[128], sb[128];
  int na = 0, nb = 0;
  auto append = [](char *buf, int &n, int cap, MPosition *m) {
    for (int i = 0; i < m->pos.n && n < cap - 16; i++) {
      const void *p = m->pos[i];
      if (i) buf[n++] = ',';
      if (is_intPosition(p)) {
        n += snprintf(buf + n, cap - n, "%05d", (int)Position2int(p));
      } else {
        n += snprintf(buf + n, cap - n, "\"%s\"", (cchar *)p);
      }
    }
    buf[n] = 0;
  };
  append(sa, na, sizeof(sa), ma);
  append(sb, nb, sizeof(sb), mb);
  return strcmp(sa, sb);
}

// ---------------------------------------------------------------------------
// Per-Fun
// ---------------------------------------------------------------------------

static void ensure_var(Sym *s) {
  if (!s) return;
  if (!s->var) s->var = new Var(s);
}

static void print_one_fun(FILE *fp, Sym *closure_sym, NameAssigner &na) {
  Fun *f = new Fun(closure_sym, FUN_BUILD_CFG_ONLY);

  // Pre-create Vars build_arg_positions will dereference.
  for (Sym *a : closure_sym->has) ensure_var(a);
  ensure_var(closure_sym->ret);

  build_arg_positions(f);

  fputs(";; Fun ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);

  fputs("(positions ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);

  // Collect all MPositions (positional + named), de-dup, sort.
  Vec<MPosition *> positions;
  for (MPosition *p : f->arg_positions) if (!positions.in(p)) positions.add(p);
  // Named positions show up in named_to_positional (key side).
  form_MPositionMPosition(e, f->named_to_positional) {
    MPosition *named = e->key;
    if (named && !positions.in(named)) positions.add(named);
  }
  if (positions.n > 1)
    qsort(positions.v, positions.n, sizeof(MPosition *), compar_mposition_str);

  bool any = false;
  for (MPosition *mp : positions) {
    Sym *fsym = f->arg_syms.get(mp);
    any = true;
    fputs("  pos", fp);
    format_mposition(fp, mp);
    fputs(" -> ", fp);
    if (fsym) na.print_ref(fp, fsym);
    else fputs("?", fp);
    // Annotations.
    if (fsym && fsym->intent == Sym_OUT) fputs(" :out", fp);
    if (fsym && fsym->intent == Sym_INOUT) fputs(" :inout", fp);
    if (fsym && fsym->is_rest) fputs(" :rest", fp);
    if (fsym && fsym->is_pattern) fputs(" :pattern", fp);
    if (f->positional_arg_positions.in(mp)) fputs(" :positional", fp);
    // Named mapping → positional.
    MPosition *positional = f->named_to_positional.get(mp);
    if (positional && positional != mp) {
      fputs(" :alias-of pos", fp);
      format_mposition(fp, positional);
    }
    fputc('\n', fp);
  }
  if (!any) fputs("  (no args)\n", fp);

  // Returns: print f->rets summary.
  if (f->rets.n) {
    fputs("  rets:", fp);
    for (Var *v : f->rets) {
      fputc(' ', fp);
      if (v && v->sym) na.print_ref(fp, v->sym);
      else fputs("?", fp);
    }
    fputc('\n', fp);
  }

  fputs(")\n\n", fp);
}

void print_argpos_normalized(FILE *fp, IF1 *p) {
  fputs(";; phase: argpos\n\n", fp);

  NameAssigner na;
  na.assign_all(p);

  Vec<Sym *> closures;
  for (Sym *c : p->allclosures) closures.add(c);
  if (closures.n > 1)
    qsort(closures.v, closures.n, sizeof(Sym *), compar_closure_by_name);

  for (Sym *c : closures) {
    if (!c->code) continue;
    print_one_fun(fp, c, na);
  }
}
