// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for simple_inlining (uses the InlineEvent sidecar
// from optimize/inline.cc).

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
#include "testing/print_inline.h"

#include <stdio.h>
#include <string.h>

static int compar_event(const void *a, const void *b) {
  InlineEvent *ea = *(InlineEvent *const *)a;
  InlineEvent *eb = *(InlineEvent *const *)b;
  // (caller name, callee name, kind, pnode id) — stable across runs
  cchar *ca = (ea->caller && ea->caller->sym && ea->caller->sym->name) ? ea->caller->sym->name : "";
  cchar *cb = (eb->caller && eb->caller->sym && eb->caller->sym->name) ? eb->caller->sym->name : "";
  int c = strcmp(ca, cb);
  if (c) return c;
  cchar *ka = (ea->callee && ea->callee->sym && ea->callee->sym->name) ? ea->callee->sym->name : "";
  cchar *kb = (eb->callee && eb->callee->sym && eb->callee->sym->name) ? eb->callee->sym->name : "";
  c = strcmp(ka, kb);
  if (c) return c;
  if (ea->kind != eb->kind) return (int)ea->kind - (int)eb->kind;
  int pa = ea->pnode ? ea->pnode->id : 0;
  int pb = eb->pnode ? eb->pnode->id : 0;
  return (pa > pb) - (pa < pb);
}

static cchar *kind_str(InlineEventKind k) {
  switch (k) {
    case INLINE_SINGLE_SEND: return "single-send";
    case INLINE_IDENTITY:    return "identity";
    case INLINE_CLOSURE:     return "closure-collapse";
    case INLINE_PRIM_CHAIN:  return "prim-chain";
  }
  return "?";
}

void print_inline_normalized(FILE *fp, IF1 *p) {
  // Setup: build Funs, register with PDB, run analyze + clone +
  // dom + DCE + freq, then turn on the event sidecar and run
  // simple_inlining.
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

  fputs(";; phase: inline\n\n", fp);
  if (!if1->top || !if1->top->fun) {
    fputs("(skipped — no top closure)\n", fp);
    return;
  }
  int fa_rc = pdb->fa->analyze(if1->top->fun);
  int clone_rc = clone(pdb->fa);
  FA *fa = pdb->fa;
  for (Fun *f : fa->funs) build_cfg_dominators(f);
  mark_live_code(fa);
  frequency_estimation(fa);
  mark_live_funs(fa);

  // Sidecar collection: only enabled around the inliner call. The
  // production frontend never calls inline_events_enable(), so its
  // bookkeeping is skipped.
  inline_events_reset();
  inline_events_enable();
  simple_inlining(fa);
  inline_events_disable();

  fprintf(fp, "(summary fa_rc=%d clone_rc=%d funs=%d events=%d)\n\n",
          fa_rc, clone_rc, fa->funs.n, inline_events_get().n);

  // Print events sorted.
  Vec<InlineEvent *> events;
  for (InlineEvent *e : inline_events_get()) events.add(e);
  if (events.n > 1) qsort(events.v, events.n, sizeof(InlineEvent *), compar_event);

  fputs("(events\n", fp);
  for (InlineEvent *e : events) {
    cchar *caller_n = (e->caller && e->caller->sym && e->caller->sym->name) ? e->caller->sym->name : "?";
    cchar *callee_n = (e->callee && e->callee->sym && e->callee->sym->name) ? e->callee->sym->name : "—";
    fprintf(fp, "  %s caller=%s callee=%s pnode=%d\n",
            kind_str(e->kind), caller_n, callee_n,
            e->pnode ? e->pnode->id : 0);
  }
  if (events.n == 0) fputs("  (none)\n", fp);
  fputs(")\n", fp);
}
