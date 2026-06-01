// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the CFG pass. See ifa/testing/phases/02_cfg_ssu.md.

#include "ifadefs.h"

#include "code.h"
#include "fun.h"
#include "if1.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"
#include "testing/print_cfg.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// Collect closures into a list sorted by assigned name → output order is
// independent of allocation order (i.e. of the order closures appear in
// the source .ir).
static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

// Format a Sym list as "%a %b %c" using the shared NameAssigner.
static void print_sym_list(FILE *fp, NameAssigner &na, Vec<Var *> &vs) {
  // NOTE: Var is a thin wrapper that carries Sym* — but at this phase
  // the lvals/rvals on a PNode are Var* whose sym is what we name. Each
  // Var has a .sym field. (See if1/var.h.)
  for (int i = 0; i < vs.n; i++) {
    if (i) fputc(' ', fp);
    na.print_ref(fp, vs[i]->sym);
  }
}

static void print_pnode_line(FILE *fp, PNode *n, NameAssigner &na,
                             Map<PNode *, cchar *> &pnames) {
  cchar *nm = pnames.get(n);
  if (!nm) nm = "?";
  cchar *kind = (n->code && n->code->kind >= 0 && n->code->kind <= 8)
                    ? code_string[n->code->kind]
                    : "?";
  fprintf(fp, "  %%%s  %-5s", nm, kind);

  // Per-kind detail.
  Code *c = n->code;
  if (c) {
    switch (c->kind) {
      case Code_MOVE:
        fputc(' ', fp); print_sym_list(fp, na, n->rvals);
        fputs(" -> ", fp); print_sym_list(fp, na, n->lvals);
        break;
      case Code_SEND:
        if (c->prim) fprintf(fp, " %s", c->prim->name ? c->prim->name : "?");
        else fputs(" -", fp);
        if (n->rvals.n) { fputc(' ', fp); print_sym_list(fp, na, n->rvals); }
        if (n->lvals.n) { fputs(" -> ", fp); print_sym_list(fp, na, n->lvals); }
        break;
      case Code_IF:
        if (n->rvals.n) { fputc(' ', fp); print_sym_list(fp, na, n->rvals); }
        break;
      case Code_LABEL:
      case Code_GOTO:
      case Code_SUB:
      case Code_SEQ:
      case Code_CONC:
      case Code_NOP:
        // No extra detail; succ/pred carry the structure.
        break;
    }
  }

  if (n->cfg_succ.n) {
    fputs("  succ:[", fp);
    for (int i = 0; i < n->cfg_succ.n; i++) {
      if (i) fputc(' ', fp);
      cchar *sn = pnames.get(n->cfg_succ[i]);
      fprintf(fp, "%%%s", sn ? sn : "?");
    }
    fputc(']', fp);
  }
  if (n->cfg_pred.n) {
    fputs("  pred:[", fp);
    for (int i = 0; i < n->cfg_pred.n; i++) {
      if (i) fputc(' ', fp);
      cchar *pn = pnames.get(n->cfg_pred[i]);
      fprintf(fp, "%%%s", pn ? pn : "?");
    }
    fputc(']', fp);
  }
  fputc('\n', fp);
}

// Walk PNodes in deterministic DFS pre-order from entry. The pnames map
// keys already give us this order (we walked in DFS to assign names).
// Re-walk here so the listing is the same order as the names.
static void collect_pnodes_dfs(PNode *n, Vec<PNode *> &out, Vec<PNode *> &seen) {
  if (!n) return;
  if (seen.set_in(n)) return;
  seen.set_add(n);
  out.add(n);
  for (PNode *s : n->cfg_succ) collect_pnodes_dfs(s, out, seen);
}

static void print_one_fun(FILE *fp, Sym *closure_sym, NameAssigner &na) {
  // Build a CFG-only Fun so SSU phi/phy don't pollute the diff.
  Fun *f = new Fun(closure_sym, FUN_BUILD_CFG_ONLY);

  fputs(";; Fun ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);

  Map<PNode *, cchar *> pnames;
  assign_pnode_names(f, pnames);

  Vec<PNode *> nodes, seen;
  collect_pnodes_dfs(f->entry, nodes, seen);

  fputs("(fun ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);
  if (f->entry) {
    cchar *en = pnames.get(f->entry);
    fprintf(fp, "  entry:  %%%s\n", en ? en : "?");
  } else {
    fputs("  entry:  -\n", fp);
  }
  if (f->exit) {
    cchar *ex = pnames.get(f->exit);
    fprintf(fp, "  exit:   %%%s\n", ex ? ex : "?");
  }
  fprintf(fp, "  pnodes: %d\n", nodes.n);
  fputs(")\n", fp);

  fputs("(pnodes ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);
  for (PNode *n : nodes) print_pnode_line(fp, n, na, pnames);
  fputs(")\n\n", fp);
}

void print_cfg_normalized(FILE *fp, IF1 *p) {
  fputs(";; phase: cfg\n\n", fp);

  NameAssigner na;
  na.assign_all(p);

  // Stable order: sort closures by name.
  Vec<Sym *> closures;
  for (Sym *c : p->allclosures) closures.add(c);
  if (closures.n > 1)
    qsort(closures.v, closures.n, sizeof(Sym *), compar_closure_by_name);

  for (Sym *c : closures) {
    if (!c->code) continue;
    print_one_fun(fp, c, na);
  }
}
