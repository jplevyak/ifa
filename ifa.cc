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
  return 0;
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

void ifa_cg(cchar *fn) { c_codegen_write_c(pdb->fa, if1->top->fun, fn); }

void ifa_compile(cchar *fn) { c_codegen_compile(fn); }
