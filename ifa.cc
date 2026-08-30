#include "ifa.h"
#include "ast.h"
#include "cg.h"
#include "clone.h"
#include "optimize/dead.h"
#include "optimize/dom.h"
#include "analysis/escape.h"
#include "fa.h"
#include "fun.h"
#include "graph.h"
#include "html.h"
#include "if1.h"
#include "ifadefs.h"
#include "optimize/inline.h"

void ifa_dbg_bodies(cchar *tag);  // ifa/issues/112 probe, defined below
#include "log.h"
#include "pattern.h"
#include "pdb.h"

void ifa_init(IFACallbacks *callbacks) {
  new IF1;
  new PDB(if1);
  init_ast(callbacks);
}

void ifa_reset() {
  // Per-subsystem state. Order matters where one resetter touches
  // pointers held by another (e.g., fa_reset clears AType globals which
  // reference Sym globals that ast_reset nulls).
  fa_reset();
  pattern_reset();
  ast_reset();
  if1 = NULL;
  pdb = NULL;
}

int ifa_analyze(cchar *fn) {
  if1_finalize(if1);
  if1_write_log();
  if (!fdce_if1) fail("unable to translate dead code");
  for (int i = 0; i < if1->allclosures.n; i++) {
    Fun *f = new Fun(if1->allclosures[i]);
    if (!f) fail("IF1 invalid");
    pdb->add(f);
  }
  FA *fa = pdb->fa;
  fa->fn = fn;
  if (fa->analyze(if1->top->fun) < 0) return -1;
  // ESCAPE_PLAN.md Phases 2-4: intra+inter-procedural escape
  // lattice.  Runs BEFORE clone so the per-EntrySet escape
  // signature is available to ES_FN::equivalent — that lets
  // clone refuse to merge EntrySets whose formals diverge in
  // escape status (Phase 4).  No-op when
  // ifa_escape_in_fa==0; codegen then uses the Stage 3
  // fallback.
  compute_escape(fa);
  if (clone(fa) < 0) return -1;
  for (Fun *f : fa->funs) build_cfg_dominators(f);
  if (mark_live_code(fa) < 0) return -1;
  if (get_int_config("alog.test.fa") > 0) log_test_fa(fa);
  frequency_estimation(fa);
  ifa_dbg_bodies("after-clone");
  return 0;
}

// ifa/issues/112 probe: fingerprint every live Fun's BODY MEMBERSHIP --
// which PNodes belong to which Fun -- at a named pipeline point. Two
// runs that agree at a point have a deterministic IR there, so any
// emitted-C difference arises LATER. Answers "is this a codegen issue,
// or does it predate codegen?" without reading either.
void ifa_dbg_bodies(cchar *tag) {
  if (!getenv("IFA_DBG_BODIES")) return;
  // inline_single_sends guards on `f->sym->has.index(fs)` and bails when
  // that index runs past the call site's rvals -- so a class's `has`
  // ORDER can flip an inlining decision. issues/121 canonicalised
  // promotion order only WITHIN a reanalyze round, so this checks
  // whether has order is actually stable run to run.
  {
    Vec<Sym *> recs;
    for (Sym *s2 : if1->allsyms) if (s2 && s2->type_kind == Type_RECORD && s2->has.n) recs.add(s2);
    qsort_by_id(recs);
    unsigned long hh = 1469598103934665603UL;
    for (Sym *s2 : recs) {
      for (Sym *m : s2->has) hh = (hh ^ (unsigned long)(m ? m->id : 0)) * 1099511628211UL;
      hh = (hh ^ 0x9e3779b9UL) * 1099511628211UL;
    }
    fprintf(stderr, "HASORDER %s nrec=%d h=%lx\n", tag, recs.n, hh);
  }
  Vec<Fun *> funs;
  for (Fun *f : pdb->fa->funs) if (f) funs.add(f);
  // The RAW order matters on its own: codegen emits bodies with
  // `for (Fun *f : fa->funs)` (cg.cc), so fa->funs order is emission
  // order. Print it before sorting, or a reordering here is invisible.
  {
    unsigned long ho = 1469598103934665603UL;
    for (Fun *f : funs) ho = (ho ^ (unsigned long)f->id) * 1099511628211UL;
    fprintf(stderr, "FUNORDER %s n=%d h=%lx\n", tag, funs.n, ho);
  }
  qsort_by_id(funs);
  for (Fun *f : funs) {
    Vec<PNode *> ps;
    for (PNode *p : f->fa_all_PNodes) if (p) ps.add(p);
    qsort_by_id(ps);
    unsigned long h = 1469598103934665603UL;  // FNV-1a
    for (PNode *p : ps) {
      unsigned long v = (unsigned long)p->id * 31 + (unsigned long)(p->code ? p->code->kind : 0);
      h = (h ^ v) * 1099511628211UL;
    }
    // Count phi/phy too: they are NOT in fa_all_PNodes, so a hash over
    // that list alone cannot see SSU placement differences (ifa/112 --
    // this is what made the emitted-C instability look like a codegen
    // issue when the phi COUNT was already varying).
    int nphi = 0, nphy = 0;
    for (PNode *p : ps) { nphi += p->phi.n; nphy += p->phy.n; }
    // Hash rvals too. Membership and phi counts alone are NOT enough:
    // simple_inlining rewrites a call site's ARGUMENT LIST in place
    // (inline.cc), which changes rvals without changing which PNodes
    // belong to which Fun -- invisible to a membership-only hash, and
    // that gap is what made this look like a codegen issue (ifa/112).
    unsigned long hr = 1469598103934665603UL;
    for (PNode *p : ps) {
      for (Var *v : p->rvals) hr = (hr ^ (unsigned long)(v ? v->id : 0)) * 1099511628211UL;
      hr = (hr ^ 0x9e3779b9UL) * 1099511628211UL;  // node separator
    }
    fprintf(stderr, "BODY %s fun=%d n=%d phi=%d phy=%d h=%lx rv=%lx\n", tag, f->id, ps.n, nphi, nphy, h, hr);
  }
}

void ifa_graph(cchar *fn) { graph(pdb->fa, fn); }

void ifa_html(cchar *fn, cchar *mktree_dir) { dump_html(pdb->fa, fn, mktree_dir); }

void ifa_code(cchar *fn) {
  char hfn[512];
  snprintf(hfn, sizeof(hfn), "%s.code", fn);
  FILE *fp = fopen(hfn, "w");
  if1_write(fp, pdb->if1);
  fclose(fp);
}

int ifa_optimize() {
  mark_live_funs(fa);
  if (simple_inlining(pdb->fa) < 0) return -1;
  mark_live_types(pdb->fa);
  mark_live_funs(pdb->fa);
  return 0;
}

void ifa_cg(cchar *fn) {
  ifa_dbg_bodies("before-cg");
  c_codegen_write_c(pdb->fa, if1->top->fun, fn);
}

void ifa_compile(cchar *fn) { c_codegen_compile(fn); }
