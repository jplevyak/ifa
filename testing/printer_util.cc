// SPDX-License-Identifier: BSD-3-Clause
// Shared printer utilities. See testing/printer_util.h.

#include "ifadefs.h"

#include "code.h"
#include "fun.h"
#include "if1.h"
#include "pnode.h"
#include "sym.h"
#include "testing/printer_util.h"

#include <stdio.h>
#include <string.h>

// ---------------------------------------------------------------------------
// NameAssigner
// ---------------------------------------------------------------------------

cchar *NameAssigner::make_unique(cchar *base) {
  int c = name_counts.get(base);
  name_counts.put(base, c + 1);
  if (c == 0) return base;
  char buf[256];
  snprintf(buf, sizeof(buf), "%s_%d", base, c);
  return if1_cannonicalize_string(if1, buf);
}

void NameAssigner::assign_all(IF1 *p) {
  // Per-category counters → naming is stable across runs regardless of
  // the order user/builtin Syms were registered.
  int const_n = 0, anon_n = 0;
  for (Sym *s : p->allsyms) {
    if (name_of.get(s)) continue;
    cchar *bname = p->builtins_names.get(s);
    if (bname) { name_of.put(s, bname); continue; }
    if (s->is_symbol && s->name) { name_of.put(s, s->name); continue; }
    if (s->is_constant) {
      char buf[64];
      snprintf(buf, sizeof(buf), "c%d", const_n++);
      name_of.put(s, if1_cannonicalize_string(if1, buf));
      continue;
    }
    if (s->name) {
      name_of.put(s, make_unique(s->name));
    } else {
      char buf[64];
      snprintf(buf, sizeof(buf), "t%d", anon_n++);
      name_of.put(s, if1_cannonicalize_string(if1, buf));
    }
  }
}

void NameAssigner::print_ref(FILE *fp, Sym *s) {
  if (!s) { fputs("%%nil", fp); return; }
  cchar *n = name_of.get(s);
  if (!n) { fprintf(fp, "%%unknown_%d", s->id); return; }
  char sigil = '%';
  if (if1->builtins_names.get(s)) sigil = '@';
  else if (s->is_symbol) sigil = '#';
  fprintf(fp, "%c%s", sigil, n);
}

// ---------------------------------------------------------------------------
// LabelNames
// ---------------------------------------------------------------------------

void LabelNames::assign(Code *root) {
  int counter = 0;
  walk(root, counter);
}

void LabelNames::walk(Code *c, int &counter) {
  if (!c) return;
  if (c->kind == Code_LABEL && c->label[0] && !names.get(c->label[0])) {
    char buf[32];
    snprintf(buf, sizeof(buf), "L%d", counter++);
    names.put(c->label[0], if1_cannonicalize_string(if1, buf));
  }
  for (Code *sub : c->sub) walk(sub, counter);
}

cchar *LabelNames::get(Label *l) {
  cchar *n = names.get(l);
  if (n) return n;
  char buf[32];
  snprintf(buf, sizeof(buf), "L_ext%d", l->id);
  return if1_cannonicalize_string(if1, buf);
}

// ---------------------------------------------------------------------------
// PNode naming
// ---------------------------------------------------------------------------

static void dfs_pnodes(PNode *n, Map<PNode *, cchar *> &names, int &counter) {
  if (!n) return;
  if (names.get(n)) return;
  char buf[32];
  snprintf(buf, sizeof(buf), "p%d", counter++);
  names.put(n, if1_cannonicalize_string(if1, buf));
  // cfg_succ order is determined by build_cfg: for IF, true label
  // before false label; for straight-line code, single successor.
  for (PNode *s : n->cfg_succ) dfs_pnodes(s, names, counter);
}

void assign_pnode_names(Fun *f, Map<PNode *, cchar *> &names) {
  if (!f || !f->entry) return;
  int counter = 0;
  dfs_pnodes(f->entry, names, counter);
}
