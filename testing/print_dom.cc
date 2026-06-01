// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the dominator computation.

#include "ifadefs.h"

#include "dom.h"
#include "fun.h"
#include "if1.h"
#include "pnode.h"
#include "sym.h"
#include "testing/print_dom.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

static cchar *pnode_name(Map<PNode *, cchar *> &pnames, PNode *n) {
  if (!n) return "-";
  cchar *nm = pnames.get(n);
  return nm ? nm : "?";
}

// A Dom carries its host PNode in the `node` field as a void*.
static PNode *dom_host(Dom *d) { return d ? (PNode *)d->node : NULL; }

static void collect_pnodes_dfs(PNode *n, Vec<PNode *> &out, Vec<PNode *> &seen) {
  if (!n) return;
  if (seen.set_in(n)) return;
  seen.set_add(n);
  out.add(n);
  for (PNode *s : n->cfg_succ) collect_pnodes_dfs(s, out, seen);
}

static int compar_closure_by_name(const void *a, const void *b) {
  Sym *sa = *(Sym *const *)a, *sb = *(Sym *const *)b;
  cchar *na = sa->name ? sa->name : "";
  cchar *nb = sb->name ? sb->name : "";
  return strcmp(na, nb);
}

// ---------------------------------------------------------------------------
// Per-fun blocks
// ---------------------------------------------------------------------------

static void print_idom_block(FILE *fp, cchar *header, Vec<PNode *> &nodes,
                             Map<PNode *, cchar *> &pnames,
                             Dom *(PNode::*field)) {
  fprintf(fp, "(%s\n", header);
  for (PNode *p : nodes) {
    Dom *d = p->*field;
    PNode *idom_host = d ? dom_host(d->idom) : NULL;
    fprintf(fp, "  %%%s -> %s%s\n",
            pnode_name(pnames, p),
            idom_host ? "%" : "",
            idom_host ? pnode_name(pnames, idom_host) : "-");
  }
  fputs(")\n\n", fp);
}

static void print_frontier_block(FILE *fp, Vec<PNode *> &nodes,
                                 Map<PNode *, cchar *> &pnames) {
  fputs("(frontier\n", fp);
  bool any = false;
  for (PNode *p : nodes) {
    if (!p->dom || p->dom->front.n == 0) continue;
    any = true;
    // Sort frontier by PNode name for stable output.
    Vec<PNode *> hosts;
    for (Dom *d : p->dom->front) {
      PNode *h = dom_host(d);
      if (h) hosts.add(h);
    }
    for (int i = 1; i < hosts.n; i++)
      for (int j = i; j > 0 && strcmp(pnode_name(pnames, hosts[j]),
                                      pnode_name(pnames, hosts[j - 1])) < 0; j--) {
        PNode *t = hosts[j]; hosts[j] = hosts[j - 1]; hosts[j - 1] = t;
      }
    fprintf(fp, "  %%%s:", pnode_name(pnames, p));
    for (PNode *h : hosts) fprintf(fp, " %%%s", pnode_name(pnames, h));
    fputc('\n', fp);
  }
  if (!any) fputs("  (empty)\n", fp);
  fputs(")\n\n", fp);
}

static void print_one_fun(FILE *fp, Sym *closure_sym, NameAssigner &na) {
  Fun *f = new Fun(closure_sym, FUN_BUILD_CFG_ONLY);
  build_cfg_dominators(f);

  fputs(";; Fun ", fp);
  na.print_ref(fp, closure_sym);
  fputc('\n', fp);

  Map<PNode *, cchar *> pnames;
  assign_pnode_names(f, pnames);

  Vec<PNode *> nodes, seen;
  collect_pnodes_dfs(f->entry, nodes, seen);

  print_idom_block(fp, "idom", nodes, pnames, &PNode::dom);
  print_idom_block(fp, "idom-rev", nodes, pnames, &PNode::rdom);
  print_frontier_block(fp, nodes, pnames);
}

void print_dom_normalized(FILE *fp, IF1 *p) {
  fputs(";; phase: dom\n\n", fp);

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
