// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the C backend.

#include "ifadefs.h"

#include "cg.h"
#include "clone.h"
#include "code.h"
#include "codegen/llvm.h"
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

// ---------------------------------------------------------------------------
// LLVM backend printer (issue 002).
//
// The pipeline mirror is identical to the C side (fa → clone → dom →
// dce → freq → inline → liveness); only the codegen call and the
// normalizer differ. The line-by-line normalizer is the load-bearing
// part: LLVM IR carries several host-/version-specific fragments that
// would otherwise host-lock the golden.
// ---------------------------------------------------------------------------

// Match a literal prefix. Returns true if `cur..cur+len` starts with
// `prefix` (an C string). Avoids dragging in std::string_view.
static bool ll_starts_with(cchar *cur, size_t len, cchar *prefix) {
  size_t plen = strlen(prefix);
  return len >= plen && memcmp(cur, prefix, plen) == 0;
}

// Does `cur..cur+len` contain `needle` (anywhere)? Linear scan, fine
// for the short lines LLVM IR emits.
static bool ll_contains(cchar *cur, size_t len, cchar *needle) {
  size_t nlen = strlen(needle);
  if (len < nlen) return false;
  for (size_t i = 0; i + nlen <= len; i++) {
    if (memcmp(cur + i, needle, nlen) == 0) return true;
  }
  return false;
}

// Write `line_len` bytes to `fp` with any "!dbg !N" / "#dbg_declare(...)"
// debug-info span removed. LLVM 17+/22 emits these on most instructions
// and on `define` headers; the metadata numbers vary with module-level
// emission order, so the goldens have to be insensitive to them.
//
// For "!dbg !N": find the span and the preceding ", " or " " separator,
// drop both. Preserve everything after the digit token (a trailing " {"
// on define headers, end-of-line elsewhere).
//
// For "#dbg_declare(...)" / "#dbg_value(...)" (LLVM 22 debug records):
// strip the entire line (these are pure debug annotations with no
// effect on the IR semantics).
static void write_stripping_dbg(FILE *fp, cchar *line, size_t line_len) {
  // Drop debug-record annotation lines wholesale.
  // The leading whitespace is variable so search for the literal token.
  for (size_t i = 0; i + 5 <= line_len; i++) {
    if (line[i] == '#' && i + 4 <= line_len &&
        (memcmp(line + i, "#dbg_", 5) == 0)) {
      // Check the prefix is only whitespace.
      bool only_ws = true;
      for (size_t j = 0; j < i; j++) {
        if (line[j] != ' ' && line[j] != '\t') { only_ws = false; break; }
      }
      if (only_ws) return;  // drop the line
      break;
    }
  }

  // Find " !dbg !N" / ", !dbg !N" inline.
  for (size_t pos = 0; pos + 6 <= line_len; pos++) {
    if (memcmp(line + pos, "!dbg !", 6) != 0) continue;
    // Identify the run of digits after "!dbg !".
    size_t token_end = pos + 6;
    while (token_end < line_len && line[token_end] >= '0' && line[token_end] <= '9') token_end++;
    // Eat the preceding ", " or " ".
    size_t trim_start = pos;
    if (trim_start >= 2 && line[trim_start - 2] == ',' && line[trim_start - 1] == ' ') {
      trim_start -= 2;
    } else if (trim_start >= 1 && line[trim_start - 1] == ' ') {
      trim_start -= 1;
    }
    fwrite(line, 1, trim_start, fp);
    fwrite(line + token_end, 1, line_len - token_end, fp);
    return;
  }
  fwrite(line, 1, line_len, fp);
}

