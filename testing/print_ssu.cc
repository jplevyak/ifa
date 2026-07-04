// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the SSU pass. See ifa/testing/phases/02_cfg_ssu.md.

#include "ifadefs.h"

#include "code.h"
#include "fun.h"
#include "if1.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"
#include "var.h"
#include "testing/print_ssu.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// ---------------------------------------------------------------------------
// Var naming
// ---------------------------------------------------------------------------
//
// After SSU, a single source Sym may have multiple Var instances (one
// per def). Naming strategy:
//   - The first Var encountered for a Sym takes the Sym's name.
//   - Subsequent Vars get name_v1, name_v2, … in the order they're
//     first seen during a deterministic DFS through PNodes (entry-DFS
//     pre-order, then phi/phy of each node).
//
// This matches what print_cfg.cc does for un-renamed Vars (which all
// share their Sym's name) and lets the SSU diff show e.g.
//   %x → %x (def %p1) %x_v1 (def %p3 phi)

struct VarNamer {
  Map<Var *, cchar *> name_of;
  Map<Sym *, int> sym_counter;
  NameAssigner &na;

  VarNamer(NameAssigner &n) : na(n) {}

  cchar *name(Var *v) {
    if (!v) return "<null>";
    cchar *n = name_of.get(v);
    if (n) return n;
    cchar *base = na.name_of.get(v->sym);
    if (!base) base = v->sym->name ? v->sym->name : "v";
    int seq = sym_counter.get(v->sym);
    sym_counter.put(v->sym, seq + 1);
    cchar *out;
    if (seq == 0) {
      out = base;
    } else {
      char buf[256];
      snprintf(buf, sizeof(buf), "%s_v%d", base, seq);
      out = if1_cannonicalize_string(if1, buf);
    }
    name_of.put(v, out);
    return out;
  }

  // Print a Var ref. Uses Sym sigil ('@' / '#' / '%') based on Sym kind,
  // mirroring NameAssigner::print_ref.
  void print_ref(FILE *fp, Var *v) {
    if (!v) { fputs("%nil", fp); return; }
    char sigil = '%';
    if (if1->builtins_names.get(v->sym)) sigil = '@';
    else if (v->sym->is_symbol) sigil = '#';
    fprintf(fp, "%c%s", sigil, name(v));
  }
};

// ---------------------------------------------------------------------------
// PNode collection (DFS pre-order from entry — same order as cfg printer)
// ---------------------------------------------------------------------------

static void collect_pnodes_dfs(PNode *n, Vec<PNode *> &out, Vec<PNode *> &seen) {
  if (!n) return;
  if (seen.set_in(n)) return;
  seen.set_add(n);
  out.add(n);
  for (PNode *s : n->cfg_succ) collect_pnodes_dfs(s, out, seen);
}

// Pre-seed VarNamer by walking the same order the printer will use,
// so the first Var seen for each Sym gets the base name.
static void seed_var_names(Vec<PNode *> &pnodes, VarNamer &vn) {
  for (PNode *n : pnodes) {
    for (PNode *p : n->phy) {
      for (Var *v : p->rvals) if (v) vn.name(v);
      for (Var *v : p->lvals) if (v) vn.name(v);
    }
    for (Var *v : n->rvals) if (v) vn.name(v);
    for (Var *v : n->lvals) if (v) vn.name(v);
    for (Var *v : n->tvals) if (v) vn.name(v);
    for (PNode *p : n->phi) {
      for (Var *v : p->rvals) if (v) vn.name(v);
      for (Var *v : p->lvals) if (v) vn.name(v);
    }
  }
}

// ---------------------------------------------------------------------------
// Per-PNode SSU block
// ---------------------------------------------------------------------------

static void print_phi_phy(FILE *fp, Vec<PNode *> &nodes, VarNamer &vn,
                          Map<PNode *, cchar *> &pnames, cchar *which) {
  fprintf(fp, "(%s\n", which);
  bool any = false;
  for (PNode *n : nodes) {
    Vec<PNode *> &lst = (strcmp(which, "phi") == 0) ? n->phi : n->phy;
    if (lst.n == 0) continue;
    any = true;
    cchar *nm = pnames.get(n);
    fprintf(fp, "  %%%s:\n", nm ? nm : "?");
    for (int i = 0; i < lst.n; i++) {
      PNode *p = lst[i];
      fprintf(fp, "    [%d] lvals=[", i);
      for (int j = 0; j < p->lvals.n; j++) {
        if (j) fputc(' ', fp);
        vn.print_ref(fp, p->lvals[j]);
      }
      fputs("] rvals=[", fp);
      for (int j = 0; j < p->rvals.n; j++) {
        if (j) fputc(' ', fp);
        vn.print_ref(fp, p->rvals[j]);
      }
      fputs("]\n", fp);
    }
  }
  if (!any) fputs("  (none)\n", fp);
  fputs(")\n\n", fp);
}

static void print_rename_map(FILE *fp, Vec<PNode *> &nodes, VarNamer &vn,
                             NameAssigner &na, Map<PNode *, cchar *> &pnames) {
  // Group Vars by Sym in deterministic seen-order.
  Vec<Sym *> sym_order;
  Map<Sym *, Vec<Var *> *> by_sym;
  auto add = [&](Var *v) {
    if (!v) return;
    Vec<Var *> *bucket = by_sym.get(v->sym);
    if (!bucket) {
      bucket = new Vec<Var *>;
      by_sym.put(v->sym, bucket);
      sym_order.add(v->sym);
    }
    if (!bucket->in(v)) bucket->add(v);
  };
  for (PNode *n : nodes) {
    for (PNode *p : n->phy) {
      for (Var *v : p->rvals) add(v);
      for (Var *v : p->lvals) add(v);
    }
    for (Var *v : n->rvals) add(v);
    for (Var *v : n->lvals) add(v);
    for (Var *v : n->tvals) add(v);
    for (PNode *p : n->phi) {
      for (Var *v : p->rvals) add(v);
      for (Var *v : p->lvals) add(v);
    }
  }

  fputs("(rename\n", fp);
  bool any = false;
  for (Sym *s : sym_order) {
    Vec<Var *> *bucket = by_sym.get(s);
    if (!bucket || bucket->n < 2) continue;  // only show split syms
    any = true;
    fputs("  ", fp);
    na.print_ref(fp, s);
    fputs(" -> ", fp);
    for (int i = 0; i < bucket->n; i++) {
      if (i) fputc(' ', fp);
      vn.print_ref(fp, (*bucket)[i]);
      Var *v = (*bucket)[i];
      cchar *dn = v->def ? pnames.get(v->def) : NULL;
      fprintf(fp, "(def %s)", dn ? dn : "?");
    }
    fputc('\n', fp);
  }
  if (!any) fputs("  (no splits)\n", fp);
  fputs(")\n\n", fp);
}

static void print_live_vars(FILE *fp, Vec<PNode *> &nodes, VarNamer &vn,
                            Map<PNode *, cchar *> &pnames) {
  fputs("(live-vars\n", fp);
  for (PNode *n : nodes) {
    if (!n->live_vars) continue;
    // Collect into a sortable list (by name).
    Vec<Var *> lvs;
    for (Var *v : *n->live_vars) if (v) lvs.add(v);
    if (lvs.n == 0) continue;
    // Insertion-sort by current name — n is small.
    for (int i = 1; i < lvs.n; i++)
      for (int j = i; j > 0 && strcmp(vn.name(lvs[j]), vn.name(lvs[j - 1])) < 0; j--) {
        Var *t = lvs[j]; lvs[j] = lvs[j - 1]; lvs[j - 1] = t;
      }
    cchar *nm = pnames.get(n);
    fprintf(fp, "  %%%s:", nm ? nm : "?");
    for (Var *v : lvs) { fputc(' ', fp); vn.print_ref(fp, v); }
    fputc('\n', fp);
  }
  fputs(")\n\n", fp);
}

// ---------------------------------------------------------------------------
// Per-fun driver
// ---------------------------------------------------------------------------

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

static void print_one_fun(FILE *fp, Sym *closure_sym, NameAssigner &na) {
  Fun *f = new Fun(closure_sym, FUN_BUILD_CFG_SSU);

  fputs(";; Fun ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);

  Map<PNode *, cchar *> pnames;
  assign_pnode_names(f, pnames);

  Vec<PNode *> nodes, seen;
  collect_pnodes_dfs(f->entry, nodes, seen);

  VarNamer vn(na);
  seed_var_names(nodes, vn);

  print_phi_phy(fp, nodes, vn, pnames, "phi");
  print_phi_phy(fp, nodes, vn, pnames, "phy");
  print_rename_map(fp, nodes, vn, na, pnames);
  print_live_vars(fp, nodes, vn, pnames);
}

void print_ssu_normalized(FILE *fp, IF1 *p) {
  fputs(";; phase: ssu\n\n", fp);

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
