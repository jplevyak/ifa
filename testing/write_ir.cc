// SPDX-License-Identifier: BSD-3-Clause
// .ir text-format writer. See ifa/testing/IF1_TEXT_FORMAT.md.

#include "ifadefs.h"

#include "code.h"
#include "if1.h"
#include "num.h"
#include "sym.h"
#include "testing/printer_util.h"
#include "testing/write_ir.h"

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

// ---------------------------------------------------------------------------
// Per-section writers
// ---------------------------------------------------------------------------

static void write_imm(FILE *fp, Immediate &imm, Sym *type) {
  cchar *t = type && type->name ? type->name : "int64";
  switch (imm.const_kind) {
    case IF1_NUM_KIND_UINT:
      switch (imm.num_index) {
        case IF1_INT_TYPE_1: fprintf(fp, "(bool %s)", imm.v_bool ? "true" : "false"); return;
        case IF1_INT_TYPE_8: fprintf(fp, "(uint8 %u)", imm.v_uint8); return;
        case IF1_INT_TYPE_16: fprintf(fp, "(uint16 %u)", imm.v_uint16); return;
        case IF1_INT_TYPE_32: fprintf(fp, "(uint32 %u)", imm.v_uint32); return;
        case IF1_INT_TYPE_64: fprintf(fp, "(uint64 %llu)", (unsigned long long)imm.v_uint64); return;
      }
      break;
    case IF1_NUM_KIND_INT:
      switch (imm.num_index) {
        case IF1_INT_TYPE_1: fprintf(fp, "(bool %s)", imm.v_bool ? "true" : "false"); return;
        case IF1_INT_TYPE_8: fprintf(fp, "(int8 %d)", imm.v_int8); return;
        case IF1_INT_TYPE_16: fprintf(fp, "(int16 %d)", imm.v_int16); return;
        case IF1_INT_TYPE_32: fprintf(fp, "(int32 %d)", imm.v_int32); return;
        case IF1_INT_TYPE_64: fprintf(fp, "(int64 %lld)", (long long)imm.v_int64); return;
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      switch (imm.num_index) {
        case IF1_FLOAT_TYPE_32: fprintf(fp, "(float32 %.9g)", imm.v_float32); return;
        case IF1_FLOAT_TYPE_64: fprintf(fp, "(float64 %.17g)", imm.v_float64); return;
        case IF1_FLOAT_TYPE_128: fprintf(fp, "(float128 %.17g)", (double)imm.v_float128); return;
      }
      break;
    case IF1_CONST_KIND_STRING:
      fprintf(fp, "(string \"%s\")", imm.v_string ? imm.v_string : "");
      return;
    case IF1_CONST_KIND_SYMBOL:
      fprintf(fp, "(symbol \"%s\")", imm.v_string ? imm.v_string : "");
      return;
  }
  (void)t;
  fprintf(fp, "(unknown_imm)");
}

static cchar *type_kind_word(int k) {
  switch (k) {
    case Type_UNKNOWN: return "UNKNOWN";
    case Type_SUM: return "SUM";
    case Type_RECORD: return "RECORD";
    case Type_FUN: return "FUN";
    case Type_REF: return "REF";
    case Type_TAGGED: return "TAGGED";
    case Type_PRIMITIVE: return "PRIMITIVE";
    case Type_APPLICATION: return "APPLICATION";
    case Type_VARIABLE: return "VARIABLE";
    case Type_ALIAS: return "ALIAS";
    default: return NULL;
  }
}

static void write_sym_attrs(FILE *fp, Sym *s, NameAssigner &na) {
  if (s->type) { fputs(" :type ", fp); na.print_ref(fp, s->type); }
  if (s->in) { fputs(" :in ", fp); na.print_ref(fp, s->in); }
  if (s->type_kind) fprintf(fp, " :kind %s", type_kind_word(s->type_kind));
  if (s->is_local) fputs(" :is-local", fp);
  if (s->is_lvalue) fputs(" :is-lvalue", fp);
  if (s->is_constant && !s->imm.const_kind) {
    // is_constant set but no immediate — emit the flag explicitly.
    fputs(" :is-constant", fp);
  }
  if (s->is_external) fputs(" :is-external", fp);
  if (s->is_this) fputs(" :is-this", fp);
  if (s->is_fake) fputs(" :is-fake", fp);
  if (s->is_pattern) fputs(" :is-pattern", fp);
  if (s->intent) {
    cchar *iw = "IN";
    if (s->intent == Sym_INOUT) iw = "INOUT";
    else if (s->intent == Sym_OUT) iw = "OUT";
    fprintf(fp, " :intent %s", iw);
  }
  if (s->nesting_depth) fprintf(fp, " :nesting-depth %d", s->nesting_depth);
  if (s->size) fprintf(fp, " :size %u", s->size);
  if (s->alignment) fprintf(fp, " :alignment %u", s->alignment);
  if (s->constant) fprintf(fp, " :constant \"%s\"", s->constant);
  if (s->imm.const_kind) { fputs(" :immediate ", fp); write_imm(fp, s->imm, s->type); }
  if (s->has.n) {
    fputs(" :has (", fp);
    for (int i = 0; i < s->has.n; i++) {
      if (i) fputc(' ', fp);
      na.print_ref(fp, s->has[i]);
    }
    fputc(')', fp);
  }
  if (s->ret) { fputs(" :ret ", fp); na.print_ref(fp, s->ret); }
  if (s->cont) { fputs(" :cont ", fp); na.print_ref(fp, s->cont); }
  if (s->alias) { fputs(" :alias ", fp); na.print_ref(fp, s->alias); }
  if (s->element) { fputs(" :element ", fp); na.print_ref(fp, s->element); }
}

static void write_code_form(FILE *fp, Code *c, NameAssigner &na, LabelNames &lbls) {
  switch (c->kind) {
    case Code_NOP:
      // Skip; bodies usually contain a leading NOP that we don't need to round-trip.
      return;
    case Code_MOVE:
      fputs("  (move ", fp);
      na.print_ref(fp, c->rvals[0]);
      fputc(' ', fp);
      na.print_ref(fp, c->lvals[0]);
      fputs(")\n", fp);
      return;
    case Code_SEND:
      fputs("  (send", fp);
      for (Sym *r : c->rvals) { fputc(' ', fp); na.print_ref(fp, r); }
      if (c->lvals.n) {
        fputs(" =>", fp);
        for (Sym *l : c->lvals) { fputc(' ', fp); na.print_ref(fp, l); }
      }
      fputs(")\n", fp);
      return;
    case Code_LABEL:
      fprintf(fp, "  (label %%%s)\n", lbls.get(c->label[0]));
      return;
    case Code_GOTO:
      fprintf(fp, "  (goto %%%s)\n", lbls.get(c->label[0]));
      return;
    case Code_IF:
      fputs("  (if ", fp);
      na.print_ref(fp, c->rvals[0]);
      fprintf(fp, " %%%s %%%s)\n", lbls.get(c->label[0]), lbls.get(c->label[1]));
      return;
    case Code_SUB:
    case Code_SEQ:
    case Code_CONC:
      // Recurse into sub-list. For SEQ/CONC we currently emit the
      // children at the same level — the (seq ...) form is reserved
      // for future use when we need to preserve explicit grouping.
      for (Code *s : c->sub) write_code_form(fp, s, na, lbls);
      return;
  }
}

static void write_fun(FILE *fp, Sym *s, NameAssigner &na) {
  fprintf(fp, "(fun ");
  na.print_ref(fp, s);
  fputc('\n', fp);

  if (s->has.n) {
    fputs("  :args (", fp);
    for (int i = 0; i < s->has.n; i++) {
      if (i) fputc(' ', fp);
      na.print_ref(fp, s->has[i]);
    }
    fputs(")\n", fp);
  }
  if (s->ret) { fputs("  :rets (", fp); na.print_ref(fp, s->ret); fputs(")\n", fp); }
  if (s->cont) { fputs("  :cont ", fp); na.print_ref(fp, s->cont); fputc('\n', fp); }
  if (s->in) { fputs("  :in ", fp); na.print_ref(fp, s->in); fputc('\n', fp); }
  if (s->nesting_depth) fprintf(fp, "  :nesting-depth %d\n", s->nesting_depth);

  fputs("  :body\n", fp);
  if (s->code) {
    LabelNames lbls;
    lbls.assign(s->code);
    write_code_form(fp, s->code, na, lbls);
  }
  fputs(")\n", fp);
}

static void write_plain_sym(FILE *fp, Sym *s, NameAssigner &na, cchar *head) {
  fprintf(fp, "(%s ", head);
  na.print_ref(fp, s);
  write_sym_attrs(fp, s, na);
  fputs(")\n", fp);
}

// ---------------------------------------------------------------------------
// Top-level write_ir
// ---------------------------------------------------------------------------

void write_ir(FILE *fp, IF1 *p) {
  if (!p) return;
  NameAssigner na;
  na.assign_all(p);

  // 1. Imports for any builtin referenced by a user Sym.
  // We don't emit (import ...) for builtins that aren't aliased
  // — references use @name directly. The user table will be populated
  // when re-parsed; @ resolves via if1_get_builtin which is set up by
  // ifa_init.

  // 2. Type-defining Syms (type_kind != Type_NONE) come before regular
  //    Syms so :type refs resolve.
  bool any_types = false;
  for (Sym *s : p->allsyms) {
    if (if1->builtins_names.get(s)) continue;  // builtins skip
    if (s->is_symbol) continue;                // emitted on demand by refs
    if (s->is_constant) continue;              // emitted as ref-on-demand below
    if (s->is_fun) continue;                   // emitted via (fun ...) later
    if (!s->type_kind) continue;
    if (!any_types) { fputs(";; types\n", fp); any_types = true; }
    write_plain_sym(fp, s, na, "type");
  }
  if (any_types) fputc('\n', fp);

  // 3. Plain Syms (variables, etc.).
  bool any_syms = false;
  for (Sym *s : p->allsyms) {
    if (if1->builtins_names.get(s)) continue;
    if (s->is_symbol) continue;
    if (s->is_fun) continue;
    if (s->type_kind) continue;  // already emitted
    if (!any_syms) { fputs(";; syms\n", fp); any_syms = true; }
    write_plain_sym(fp, s, na, "sym");
  }
  if (any_syms) fputc('\n', fp);

  // 4. Function definitions.
  bool any_funs = false;
  for (Sym *c : p->allclosures) {
    if (if1->builtins_names.get(c)) continue;
    if (!any_funs) { fputs(";; funs\n", fp); any_funs = true; }
    write_fun(fp, c, na);
  }
  if (any_funs) fputc('\n', fp);

  // 5. Entry.
  if (p->top) {
    fputs(";; entry\n", fp);
    fputs("(entry ", fp);
    na.print_ref(fp, p->top);
    fputs(")\n", fp);
  }
}
