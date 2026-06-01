// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for local loop detection.

#include "ifadefs.h"

#include "dom.h"
#include "fun.h"
#include "if1.h"
#include "loop.h"
#include "pnode.h"
#include "sym.h"
#include "testing/print_loops.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// A LoopNode wraps either a real PNode (node != NULL) or a synthesized
// "loop rep" (node == NULL). Only loop reps have meaningful children
// for nesting; PNode wrappers are leaves.
static bool is_loop_rep(LoopNode *n) { return n && n->node == NULL; }

static cchar *pnode_name(Map<PNode *, cchar *> &pnames, PNode *n) {
  if (!n) return "?";
  cchar *nm = pnames.get(n);
  return nm ? nm : "?";
}

static void collect_pnodes_dfs(PNode *n, Vec<PNode *> &out, Vec<PNode *> &seen) {
  if (!n) return;
  if (seen.set_in(n)) return;
  seen.set_add(n);
  out.add(n);
  for (PNode *s : n->cfg_succ) collect_pnodes_dfs(s, out, seen);
}

// Recursively assign l0, l1, … to loop reps in DFS pre-order through
// the tree rooted at `n`.
static void assign_loop_names(LoopNode *n, Map<LoopNode *, cchar *> &names,
                              int &counter) {
  if (!n || !is_loop_rep(n)) return;
  if (names.get(n)) return;
  char buf[32];
  snprintf(buf, sizeof(buf), "l%d", counter++);
  names.put(n, if1_cannonicalize_string(if1, buf));
  for (LoopNode *c : n->children) assign_loop_names(c, names, counter);
}

// Collect every PNode-wrapper LoopNode in the sub-tree of `rep`.
// "Members" = all PNode descendants (including those in nested loops).
static void collect_members(LoopNode *rep, Vec<PNode *> &out) {
  if (!rep) return;
  for (LoopNode *c : rep->children) {
    if (is_loop_rep(c)) {
      collect_members(c, out);
    } else if (c->node) {
      out.add((PNode *)c->node);
    }
  }
}

static int compar_pnode_by_name(const void *a, const void *b, void *ctx) {
  Map<PNode *, cchar *> *pn = (Map<PNode *, cchar *> *)ctx;
  cchar *na = pn->get(*(PNode *const *)a);
  cchar *nb = pn->get(*(PNode *const *)b);
  return strcmp(na ? na : "", nb ? nb : "");
}

static void print_loop_tree(FILE *fp, LoopNode *n, int depth,
                            Map<LoopNode *, cchar *> &lnames,
                            Map<PNode *, cchar *> &pnames) {
  if (!n || !is_loop_rep(n)) return;
  for (int i = 0; i < depth; i++) fputs("  ", fp);
  cchar *nm = lnames.get(n);
  Vec<PNode *> members;
  collect_members(n, members);
  // Sort by PNode name for stable output.
  qsort_r(members.v, members.n, sizeof(PNode *), compar_pnode_by_name, &pnames);
  fprintf(fp, "%s depth=%d members=[", nm ? nm : "?", depth + 1);
  for (int i = 0; i < members.n; i++) {
    if (i) fputc(' ', fp);
    fprintf(fp, "%%%s", pnode_name(pnames, members[i]));
  }
  fputs("]\n", fp);
  for (LoopNode *c : n->children) {
    if (is_loop_rep(c)) print_loop_tree(fp, c, depth + 1, lnames, pnames);
  }
}

static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

static void print_one_fun(FILE *fp, Sym *closure_sym, NameAssigner &na) {
  Fun *f = new Fun(closure_sym, FUN_BUILD_CFG_ONLY);
  build_cfg_dominators(f);
  find_local_loops(NULL, f);

  fputs(";; Fun ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);

  Map<PNode *, cchar *> pnames;
  assign_pnode_names(f, pnames);

  Vec<PNode *> dfs;
  Vec<PNode *> seen;
  collect_pnodes_dfs(f->entry, dfs, seen);
  (void)dfs;

  fputs("(loops ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);

  // Find all top-level loop reps: parent == NULL && node == NULL and
  // have content (children with parent set to this rep).
  // `g->loops` only points to one (the last created); a function with
  // disjoint loops has several, so iterate g->nodes for completeness.
  Vec<LoopNode *> roots;
  if (f->loops) {
    for (LoopNode *n : f->loops->nodes) {
      if (is_loop_rep(n) && !n->parent && n->children.n > 0) roots.add(n);
    }
  }
  if (roots.n == 0) {
    fputs("  (none)\n", fp);
  } else {
    Map<LoopNode *, cchar *> lnames;
    int counter = 0;
    for (LoopNode *r : roots) assign_loop_names(r, lnames, counter);
    for (LoopNode *r : roots) print_loop_tree(fp, r, 0, lnames, pnames);
  }
  fputs(")\n\n", fp);
}

void print_loops_normalized(FILE *fp, IF1 *p) {
  fputs(";; phase: loops\n\n", fp);

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
