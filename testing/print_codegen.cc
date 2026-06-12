// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the C backend.

#include "ifadefs.h"

#include "cg.h"
#include "cg_ir.h"
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

void print_codegen_llvm_normalized(FILE *fp, IF1 *p) {
  // ------------------------------------------------------------------
  // Setup — same as print_codegen_c_normalized.
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

  fputs(";; phase: codegen-llvm\n\n", fp);
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
  if (fa_rc == 0 && clone_rc == 0) {
    simple_inlining(fa);
    mark_live_types(fa);
    mark_live_funs(fa);
  }

  // ------------------------------------------------------------------
  // Run the LLVM backend into a memstream so we can normalize.
  // ------------------------------------------------------------------
  char *buf = NULL;
  size_t buf_n = 0;
  FILE *mem = open_memstream(&buf, &buf_n);
  if (!mem) {
    fputs("(open_memstream failed)\n", fp);
    return;
  }
  llvm_codegen_print_ir(mem, fa, if1->top->fun, "test.ir");
  fclose(mem);

  fprintf(fp, "(summary fa_rc=%d clone_rc=%d funs=%d bytes=%zu)\n\n",
          fa_rc, clone_rc, fa->funs.n, buf_n);

  // ------------------------------------------------------------------
  // Line-by-line normalizer.
  //
  // Strip-list (entire line dropped):
  //   - "; ModuleID = ..."
  //   - "source_filename = ..."     (input filename leaks into here)
  //   - "target triple = ..."       (host triple)
  //   - "target datalayout = ..."   (host data layout)
  //   - Any line starting with `!` — module-level named metadata
  //     (`!llvm.module.flags`, `!llvm.dbg.cu`) and all metadata
  //     definitions (`!N = ...`, including all `!DI*` debug info).
  //
  // Surviving lines have any trailing ", !dbg !N" / " !dbg !N" annotation
  // stripped via write_stripping_dbg(). The `!N` references that remain
  // (e.g. in `define ... !dbg !20`) are removed by the same pass.
  // ------------------------------------------------------------------
  fputs(";; llvm-ir (host-specific module lines + !dbg metadata stripped):\n", fp);
  cchar *cur = buf;
  cchar *end = buf + buf_n;
  while (cur < end) {
    cchar *nl = (cchar *)memchr(cur, '\n', end - cur);
    cchar *line_end = nl ? nl : end;
    size_t line_len = line_end - cur;

    bool drop = false;
    if (line_len > 0) {
      if (ll_starts_with(cur, line_len, "; ModuleID =")) drop = true;
      else if (ll_starts_with(cur, line_len, "source_filename =")) drop = true;
      else if (ll_starts_with(cur, line_len, "target triple =")) drop = true;
      else if (ll_starts_with(cur, line_len, "target datalayout =")) drop = true;
      else if (cur[0] == '!') drop = true;
      // The trailing "attributes #N = { ... }" lines vary across LLVM
      // versions; strip to keep goldens stable.
      else if (ll_starts_with(cur, line_len, "attributes #")) drop = true;
    }

    if (!drop) {
      write_stripping_dbg(fp, cur, line_len);
      if (nl) fputc('\n', fp);
    }
    cur = nl ? nl + 1 : end;
  }

  if (buf) free(buf);
  // Silence unused-helper warning if `ll_contains` is not hit by the
  // strip list above; it stays available for future strip rules.
  (void)ll_contains;
}

// ---------------------------------------------------------------------------
// cg-normalize phase printer (CG_IR_PLAN Phase 2).
//
// Same FA + clone + DCE pipeline as the codegen-c / codegen-llvm
// phases, then invokes `cg_normalize(fa)` and dumps the resulting
// CGProgram in a stable textual form. Output is sorted by
// stable keys (sym ID, fun ID, block ID) so per-run nondeterminism
// (Map iteration order, allocator address) doesn't show through.
// ---------------------------------------------------------------------------

static cchar *cg_type_kind_str(CGTypeKind k) {
  switch (k) {
    case CG_T_VOID:    return "void";
    case CG_T_INT:     return "int";
    case CG_T_UINT:    return "uint";
    case CG_T_FLOAT:   return "float";
    case CG_T_BOOL:    return "bool";
    case CG_T_PTR:     return "ptr";
    case CG_T_STRUCT:  return "struct";
    case CG_T_FUN_PTR: return "funptr";
  }
  return "?";
}

static cchar *cg_slot_kind_str(CGSlotKind k) {
  switch (k) {
    case CG_SLOT_GLOBAL:   return "global";
    case CG_SLOT_LOCAL:    return "local";
    case CG_SLOT_FORMAL:   return "formal";
    case CG_SLOT_CONSTANT: return "const";
  }
  return "?";
}

static cchar *cg_op_str(CGOp op) {
  switch (op) {
    case CG_NOP:          return "NOP";
    case CG_LOAD:         return "LOAD";
    case CG_STORE:        return "STORE";
    case CG_GEP_FIELD:    return "GEP_FIELD";
    case CG_LOAD_FIELD:   return "LOAD_FIELD";
    case CG_STORE_FIELD:  return "STORE_FIELD";
    case CG_CALL:         return "CALL";
    case CG_ALLOC:        return "ALLOC";
    case CG_CAST:         return "CAST";
    case CG_PRIM_OP:      return "PRIM_OP";
    case CG_PRIM_CGFN:    return "PRIM_CGFN";
    case CG_BR:           return "BR";
    case CG_COND_BR:      return "COND_BR";
    case CG_RET:          return "RET";
    case CG_UNREACHABLE:  return "UNREACHABLE";
  }
  return "?";
}

static void print_cgtype(FILE *fp, CGType *t) {
  if (!t) { fputs("?", fp); return; }
  fprintf(fp, "%s", cg_type_kind_str(t->kind));
  if (t->bits) fprintf(fp, ":%d", t->bits);
}

static void print_cgvalue(FILE *fp, CGValue *v) {
  if (!v) { fputs("?", fp); return; }
  switch (v->kind) {
    case CG_V_NONE:      fputs("none", fp); break;
    case CG_V_INST:      fputs("inst", fp); break;
    case CG_V_SLOT:
      if (v->slot && v->slot->source_sym)
        fprintf(fp, "%%s%d", v->slot->source_sym->id);
      else
        fputs("%?", fp);
      break;
    case CG_V_IMMEDIATE: fputs("imm", fp); break;
    case CG_V_FUN:       fputs("fun", fp); break;
  }
}

static int cmp_cgslots_by_id(const void *a, const void *b) {
  CGSlot *x = *(CGSlot **)a;
  CGSlot *y = *(CGSlot **)b;
  if (x->id != y->id) return x->id - y->id;
  return 0;
}

static int cmp_cgfuns_by_source_id(const void *a, const void *b) {
  CGFun *x = *(CGFun **)a;
  CGFun *y = *(CGFun **)b;
  int xi = x->source_fun ? x->source_fun->id : 0;
  int yi = y->source_fun ? y->source_fun->id : 0;
  return xi - yi;
}

void print_cg_normalize_normalized(FILE *fp, IF1 *p) {
  // Setup — same as print_codegen_c_normalized.
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

  fputs(";; phase: cg-normalize\n\n", fp);
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
  if (fa_rc == 0 && clone_rc == 0) {
    simple_inlining(fa);
    mark_live_types(fa);
    mark_live_funs(fa);
  }

  CGProgram *prog = cg_normalize(fa);

  fprintf(fp, "(summary fa_rc=%d clone_rc=%d funs=%d globals=%d types=%d)\n\n",
          fa_rc, clone_rc, prog->funs.n, prog->globals.n, prog->types.n);

  // Globals (sorted by sym id).
  Vec<CGSlot *> sorted_globals;
  for (CGSlot *s : prog->globals) sorted_globals.add(s);
  qsort(sorted_globals.v, sorted_globals.n, sizeof(CGSlot *), cmp_cgslots_by_id);
  if (sorted_globals.n) {
    fputs(";; globals\n", fp);
    for (CGSlot *s : sorted_globals) {
      fprintf(fp, "(slot %%s%d :kind %s :type ", s->id, cg_slot_kind_str(s->kind));
      print_cgtype(fp, s->type);
      if (s->name) fprintf(fp, " :name %s", s->name);
      fputs(")\n", fp);
    }
    fputc('\n', fp);
  }

  // Functions (sorted by source Fun id).
  Vec<CGFun *> sorted_funs;
  for (CGFun *cf : prog->funs) sorted_funs.add(cf);
  qsort(sorted_funs.v, sorted_funs.n, sizeof(CGFun *), cmp_cgfuns_by_source_id);
  if (sorted_funs.n) fputs(";; funs\n", fp);
  for (CGFun *cf : sorted_funs) {
    fprintf(fp, "(fun #%d", cf->source_fun ? cf->source_fun->id : 0);
    if (cf->name) fprintf(fp, " :name %s", cf->name);
    if (cf->is_main) fputs(" :main", fp);
    if (cf->is_external) fputs(" :external", fp);
    fputs("\n  :return-type ", fp);
    print_cgtype(fp, cf->return_type);
    fprintf(fp, "\n  :arg-types (");
    for (int i = 0; i < cf->arg_types.n; i++) {
      if (i) fputc(' ', fp);
      print_cgtype(fp, cf->arg_types[i]);
    }
    fputs(")\n", fp);
    fprintf(fp, "  :locals %d :blocks %d\n", cf->locals.n, cf->blocks.n);
    for (CGBlock *b : cf->blocks) {
      fprintf(fp, "  (block #%d :label %s :preds %d :succs %d", b->id,
              b->label ? b->label : "?", b->preds.n, b->succs.n);
      if (b->body.n) {
        fputs("\n    :body (\n", fp);
        for (CGInst *inst : b->body) {
          fprintf(fp, "      (%s", cg_op_str(inst->op));
          if (inst->slot) fprintf(fp, " :slot %%s%d", inst->slot->id);
          for (CGValue *cv : inst->rvals) {
            fputc(' ', fp);
            print_cgvalue(fp, cv);
          }
          fputs(")\n", fp);
        }
        fputs("    )", fp);
      }
      if (b->terminator) {
        fprintf(fp, "\n    :term (%s", cg_op_str(b->terminator->op));
        for (CGValue *cv : b->terminator->rvals) {
          fputc(' ', fp);
          print_cgvalue(fp, cv);
        }
        fputc(')', fp);
      }
      fputs(")\n", fp);
    }
    fputs(")\n", fp);
  }
}
