/* -*-Mode: c++;-*-
   Copyright (c) 1992-2008 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "ifa.h"
#include "ast.h"
#include "if1.h"
#include "cg.h"
#include "clone.h"
#include "dead.h"
#include "dom.h"
#include "fa.h"
#include "fun.h"
#include "inline.h"
#include "pdb.h"
#include "html.h"
#include "graph.h"
#include "log.h"

void ifa_init(IFACallbacks *callbacks) {
  new IF1;
  new PDB(if1);
  init_ast(callbacks);
}

int
ifa_analyze(cchar *fn) {
  if1_finalize(if1);
  if1_write_log();
  if (!fdce_if1)
    fail("unable to translate dead code");
  for (int i = 0; i < if1->allclosures.n; i++) {
    Fun *f = new Fun(if1->allclosures[i]);
    if (!f)
      fail("IF1 invalid");
    pdb->add(f);
  }
  FA *fa = pdb->fa;
  fa->fn = fn;
  if (fa->analyze(if1->top->fun) < 0) return -1;
  if (clone(fa) < 0) return -1;
  forv_Fun(f, fa->funs) build_cfg_dominators(f);
  if (mark_live_code(fa) < 0) return -1;
  if (get_int_config("alog.test.fa") > 0)
    log_test_fa(fa);
  frequency_estimation(fa);
  return 0;
}

void 
ifa_graph(cchar *fn) {
  graph(pdb->fa, fn);
}

void 
ifa_html(cchar *fn, char *mktree_dir) {
  dump_html(pdb->fa, fn, mktree_dir);
}

int
ifa_optimize() {
  if (simple_inlining(pdb->fa) < 0) return -1;
  mark_live_types(pdb->fa);
  mark_live_funs(pdb->fa);
  return 0;
}

void 
ifa_cg(cchar *fn) {
  c_codegen_write_c(pdb->fa, if1->top->fun,  fn);
}

void 
ifa_compile(cchar *fn) {
  c_codegen_compile(fn);
}

