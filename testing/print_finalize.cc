// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for `finalize`. See ifa/testing/phases/01_if1_finalize.md.
//
// Output layout:
//   ;; phase: finalize
//   (prim-bound ...)   ; per-closure list of SEND → prim name
//   (live ...)         ; live counts (syms / closures)
//   ;; full state:
//   (if1 ...)          ; round-trippable via write_ir

#include "ifadefs.h"

#include "code.h"
#include "if1.h"
#include "prim.h"
#include "sym.h"
#include "testing/print_finalize.h"
#include "testing/write_ir.h"

// Walk a Code tree, calling visit() on every SEND in pre-order.
template <class F>
static void walk_sends(Code *c, F visit) {
  if (!c) return;
  if (c->kind == Code_SEND) visit(c);
  for (Code *sub : c->sub) walk_sends(sub, visit);
}

void print_finalize_normalized(FILE *fp, IF1 *p) {
  fputs(";; phase: finalize\n\n", fp);

  // ------------------------------------------------------------------ prim-bound
  fputs("(prim-bound\n", fp);
  for (Sym *c : p->allclosures) {
    if (!c->code) continue;
    // Collect sends + their (1-based) index in pre-order so the
    // sequence is stable across runs.
    int idx = 0;
    bool emitted_header = false;
    walk_sends(c->code, [&](Code *send) {
      idx++;
      if (!emitted_header) {
        fprintf(fp, "  in %%%s:\n", c->name ? c->name : "<anon>");
        emitted_header = true;
      }
      cchar *prim_name = send->prim ? send->prim->name : "(none)";
      fprintf(fp, "    send[%d] = %s\n", idx, prim_name);
    });
  }
  fputs(")\n\n", fp);

  // ------------------------------------------------------------------ live counts
  int syms_live = 0, syms_dead = 0;
  for (Sym *s : p->allsyms) (s->live ? syms_live : syms_dead)++;
  int closures_live = 0;
  for (Sym *c : p->allclosures) if (c->live) closures_live++;
  fprintf(fp, "(live\n");
  fprintf(fp, "  syms:     %d live, %d dead\n", syms_live, syms_dead);
  fprintf(fp, "  closures: %d live (of %d total)\n", closures_live, p->allclosures.n);
  fprintf(fp, ")\n\n");

  // ------------------------------------------------------------------ full state
  fputs(";; full state:\n", fp);
  write_ir(fp, p);
}
