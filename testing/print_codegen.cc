// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the C backend.

#include "ifadefs.h"

#include "cg.h"
#include "clone.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "optimize/dead.h"
#include "optimize/dom.h"
#include "optimize/inline.h"
#include "pdb.h"
#include "sym.h"
#include "var.h"
#include "testing/fa_setup.h"
#include "testing/print_codegen.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_codegen_c_normalized(FILE *fp, IF1 *p) {
  // ------------------------------------------------------------------
  // Setup (same as dce/freq printers).
  // ------------------------------------------------------------------
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

  fputs(";; phase: codegen-c\n\n", fp);
  if (!if1->top || !if1->top->fun) {
    fputs("(skipped — no top closure)\n", fp);
    return;
  }

  // ifa_analyze pipeline.
  int fa_rc = pdb->fa->analyze(if1->top->fun);
  int clone_rc = clone(pdb->fa);
  FA *fa = pdb->fa;
  for (Fun *f : fa->funs) build_cfg_dominators(f);
  mark_live_code(fa);
  frequency_estimation(fa);

  // ifa_optimize pipeline.
  mark_live_funs(fa);
  // simple_inlining can be aggressive; skip if any earlier step
  // failed so we don't compound errors.
  if (fa_rc == 0 && clone_rc == 0) {
    simple_inlining(fa);
    mark_live_types(fa);
    mark_live_funs(fa);
  }

  // ------------------------------------------------------------------
  // Run the C backend into a memstream so we can lightly normalise.
  // ------------------------------------------------------------------
  char *buf = NULL;
  size_t buf_n = 0;
  FILE *mem = open_memstream(&buf, &buf_n);
  if (!mem) {
    fputs("(open_memstream failed)\n", fp);
    return;
  }
  c_codegen_print_c(mem, fa, if1->top->fun);
  fclose(mem);

  fprintf(fp, "(summary fa_rc=%d clone_rc=%d funs=%d bytes=%zu)\n\n",
          fa_rc, clone_rc, fa->funs.n, buf_n);

  // ------------------------------------------------------------------
  // Light normalisation:
  //   - drop the leading `#include "c_runtime.h"` line.
  //   - trim a trailing blank line.
  // ------------------------------------------------------------------
  fputs(";; c-output (#include line stripped):\n", fp);
  cchar *cur = buf;
  cchar *end = buf + buf_n;
  while (cur < end) {
    cchar *nl = (cchar *)memchr(cur, '\n', end - cur);
    cchar *line_end = nl ? nl : end;
    size_t line_len = line_end - cur;

    // Skip the `#include "c_runtime.h"` line.
    if (line_len >= 9 && memcmp(cur, "#include ", 9) == 0) {
      cur = nl ? nl + 1 : end;
      continue;
    }
    fwrite(cur, 1, line_len, fp);
    if (nl) fputc('\n', fp);
    cur = nl ? nl + 1 : end;
  }

  if (buf) free(buf);
}
