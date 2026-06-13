// SPDX-License-Identifier: BSD-3-Clause
//
// cg_ir_v2_print.cc — printer (S-expression form).
//
// Round-trip target: print(parse(text)) ≡ text modulo
// whitespace and comments. parse(print(prog)) ≡ prog
// (semantically).
//
// Like the parser, this is the v0 form. Each new test landing
// extends the printer for the new syntax it uses.

#include "ifadefs.h"

#include "codegen/cg_ir_v2.h"

#include <stdio.h>
#include <string.h>

namespace {

// Simple growable text buffer.
struct Buf {
  char *data;
  int len;
  int cap;

  Buf() : data((char *)MALLOC(256)), len(0), cap(256) { data[0] = 0; }

  void put(char c) {
    if (len + 2 > cap) grow(len + 2);
    data[len++] = c;
    data[len] = 0;
  }
  void puts_(cchar *s) {
    int n = strlen(s);
    if (len + n + 1 > cap) grow(len + n + 1);
    memcpy(data + len, s, n);
    len += n;
    data[len] = 0;
  }
  void putf(cchar *fmt, ...) {
    char tmp[256];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(tmp, sizeof(tmp), fmt, ap);
    va_end(ap);
    puts_(tmp);
  }

  void grow(int min_cap) {
    while (cap < min_cap) cap *= 2;
    char *nd = (char *)MALLOC(cap);
    memcpy(nd, data, len + 1);
    data = nd;
  }
};

static void print_type_ref(Buf &b, CGv2Type *t) {
  if (!t || !t->name) {
    b.puts_("?");
    return;
  }
  b.puts_(t->name);
}

static void print_imm(Buf &b, const CGv2Immediate &imm) {
  switch (imm.kind) {
    case CGv2Immediate::I_INT:
      b.putf("(int %lld)", (long long)imm.v.i);
      break;
    case CGv2Immediate::I_UINT:
      b.putf("(uint %llu)", (unsigned long long)imm.v.u);
      break;
    case CGv2Immediate::I_FLOAT:
      b.putf("(float %g)", imm.v.f);
      break;
    case CGv2Immediate::I_BOOL:
      b.puts_(imm.v.b ? "(bool true)" : "(bool false)");
      break;
    case CGv2Immediate::I_STR:
      // imm.str already includes its surrounding quotes (the
      // parser preserved them).
      b.puts_("(str ");
      b.puts_(imm.str ? imm.str : "\"\"");
      b.put(')');
      break;
    case CGv2Immediate::I_SYM:
      b.puts_("(sym ");
      b.puts_(imm.str ? imm.str : "?");
      b.put(')');
      break;
    case CGv2Immediate::I_NIL:
      b.puts_("(nil)");
      break;
    case CGv2Immediate::I_UNDEF:
      b.puts_("(undef)");
      break;
    case CGv2Immediate::I_NONE:
    default:
      b.puts_("(undef)");
      break;
  }
}

static void print_const(Buf &b, CGv2Value *v) {
  b.puts_("  (const %");
  b.puts_(v->name ? v->name : "?");
  b.put(' ');
  print_imm(b, v->imm);
  b.puts_(" :type ");
  print_type_ref(b, v->type);
  b.put(')');
}

static cchar *binop_name(CGv2BinSub s) {
  switch (s) {
    case CG2B_ADD:  return "add";
    case CG2B_SUB:  return "sub";
    case CG2B_MUL:  return "mul";
    case CG2B_LT:   return "lt";
    case CG2B_LE:   return "le";
    case CG2B_NONE: return "?";
  }
  return "?";
}

static void print_inst(Buf &b, CGv2Inst *inst) {
  b.puts_("      (inst %");
  b.puts_(inst->name ? inst->name : "?");
  switch (inst->op) {
    case CG2_BINOP:
      b.puts_(" binop ");
      b.puts_(binop_name(inst->sub_op));
      break;
    case CG2_MOVE:
      b.puts_(" move");
      break;
    case CG2_CALL:
      b.puts_(" call");
      break;
    case CG2_ALLOC:
      b.puts_(" alloc :type ");
      print_type_ref(b, inst->type_arg);
      break;
    case CG2_FIELD_STORE:
      b.putf(" field_store :field_idx %d", inst->field_idx);
      if (inst->type_arg) {
        b.puts_(" :struct ");
        print_type_ref(b, inst->type_arg);
      }
      break;
    case CG2_FIELD_LOAD:
      b.putf(" field_load :field_idx %d", inst->field_idx);
      if (inst->type_arg) {
        b.puts_(" :struct ");
        print_type_ref(b, inst->type_arg);
      }
      break;
    case CG2_INDEX_LOAD:
      b.puts_(" index_load");
      break;
    default:
      b.puts_(" nop");
      break;
  }
  for (CGv2Value *v : inst->rvals) {
    b.puts_(" %");
    b.puts_(v->name ? v->name : "?");
  }
  if (inst->lvals.n > 0) {
    b.puts_(" =>");
    for (CGv2Value *v : inst->lvals) {
      b.puts_(" %");
      b.puts_(v->name ? v->name : "?");
    }
  }
  b.put(')');
}

static void print_term(Buf &b, CGv2Inst *inst) {
  if (!inst) {
    b.puts_("(unreachable)");
    return;
  }
  switch (inst->op) {
    case CG2_RET:
      if (inst->rvals.n == 0) {
        b.puts_("(ret)");
      } else {
        b.puts_("(ret %");
        b.puts_(inst->rvals[0]->name ? inst->rvals[0]->name : "?");
        b.put(')');
      }
      break;
    case CG2_BR:
      b.puts_("(br %");
      b.puts_(inst->br_target && inst->br_target->name
                  ? inst->br_target->name
                  : "?");
      b.put(')');
      break;
    case CG2_COND_BR:
      b.puts_("(cond_br ");
      if (inst->rvals.n > 0 && inst->rvals[0]->name) {
        b.put('%');
        b.puts_(inst->rvals[0]->name);
      } else {
        b.put('?');
      }
      b.puts_(" %");
      b.puts_(inst->br_true && inst->br_true->name
                  ? inst->br_true->name
                  : "?");
      b.puts_(" %");
      b.puts_(inst->br_false && inst->br_false->name
                  ? inst->br_false->name
                  : "?");
      b.put(')');
      break;
    case CG2_UNREACHABLE:
      b.puts_("(unreachable)");
      break;
    default:
      b.puts_("(unreachable)");
      break;
  }
}

static void print_block(Buf &b, CGv2Block *blk) {
  b.puts_("    (block %");
  b.puts_(blk->name ? blk->name : "?");
  // Predecessors (only if non-empty).
  if (blk->preds.n > 0) {
    b.puts_(" :preds (");
    for (int i = 0; i < blk->preds.n; i++) {
      if (i) b.put(' ');
      b.put('%');
      b.puts_(blk->preds[i]->name ? blk->preds[i]->name : "?");
    }
    b.put(')');
  }
  // Phi groups.
  if (blk->phi_by_pred.n > 0) {
    b.puts_("\n      :phi_by_pred (");
    for (int i = 0; i < blk->phi_by_pred.n; i++) {
      CGv2PhiGroup *g = blk->phi_by_pred[i];
      if (i) b.put(' ');
      b.puts_("(%");
      b.puts_(g->pred && g->pred->name ? g->pred->name : "?");
      b.puts_(" (");
      for (int j = 0; j < g->moves.n; j++) {
        CGv2Inst *mv = g->moves[j];
        if (j) b.put(' ');
        b.puts_("(move %");
        if (mv->rvals.n > 0 && mv->rvals[0]->name)
          b.puts_(mv->rvals[0]->name);
        else
          b.put('?');
        b.puts_(" => %");
        if (mv->lvals.n > 0 && mv->lvals[0]->name)
          b.puts_(mv->lvals[0]->name);
        else
          b.put('?');
        b.put(')');
      }
      b.puts_("))");
    }
    b.put(')');
  }
  // Body insts.
  for (CGv2Inst *inst : blk->body) {
    b.put('\n');
    print_inst(b, inst);
  }
  // Terminator.
  b.puts_("\n      :term ");
  print_term(b, blk->terminator);
  b.put(')');
}

static void print_sig(Buf &b, CGv2Sig *sig) {
  b.put('(');
  print_type_ref(b, sig->ret);
  for (CGv2Type *a : sig->args) {
    b.put(' ');
    print_type_ref(b, a);
  }
  b.put(')');
}

static cchar *scope_name(CGv2ValueScope s) {
  switch (s) {
    case CG2V_LOCAL:    return "local";
    case CG2V_FORMAL:   return "formal";
    case CG2V_GLOBAL:   return "global";
    case CG2V_CONSTANT: return "constant";
    case CG2V_FUN_REF:  return "fun_ref";
    case CG2V_SYMBOL:   return "symbol";
  }
  return "local";
}

static void print_value_decl(Buf &b, CGv2Value *v) {
  b.puts_("    (value %");
  b.puts_(v->name ? v->name : "?");
  if (v->type) {
    b.puts_(" :type ");
    print_type_ref(b, v->type);
  }
  b.puts_(" :scope ");
  b.puts_(scope_name(v->scope));
  if (v->target_name) {
    b.puts_(" :target %");
    b.puts_(v->target_name);
  }
  b.put(')');
}

static void print_fun(Buf &b, CGv2Fun *f) {
  b.puts_("  (fun %");
  b.puts_(f->name ? f->name : "?");
  b.puts_("\n    :signature ");
  print_sig(b, f->signature);
  if (f->entry && f->entry->name) {
    b.puts_("\n    :entry %");
    b.puts_(f->entry->name);
  }
  if (f->is_main) b.puts_("\n    :main true");
  if (f->formals.n > 0) {
    b.puts_("\n    :formals (");
    for (int i = 0; i < f->formals.n; i++) {
      if (i) b.put(' ');
      b.put('%');
      b.puts_(f->formals[i]->name ? f->formals[i]->name : "?");
    }
    b.put(')');
  }
  b.put('\n');
  for (CGv2Value *v : f->formals) {
    print_value_decl(b, v);
    b.put('\n');
  }
  for (CGv2Value *v : f->locals) {
    print_value_decl(b, v);
    b.put('\n');
  }
  for (int i = 0; i < f->blocks.n; i++) {
    if (i) b.put('\n');
    print_block(b, f->blocks[i]);
  }
  b.puts_(")");
}

}  // namespace

static void print_type_decl(Buf &b, CGv2Type *t) {
  b.puts_("  (type %");
  b.puts_(t->name ? t->name : "?");
  b.puts_(" :kind ");
  switch (t->kind) {
    case CG2T_STRUCT: b.puts_("struct"); break;
    case CG2T_PTR:    b.puts_("ptr"); break;
    default: b.puts_("?"); break;
  }
  if (t->is_heap_aggregate) b.puts_(" :is_heap_aggregate true");
  if (t->element) {
    b.puts_(" :element ");
    print_type_ref(b, t->element);
  }
  if (t->fields.n > 0) {
    b.puts_(" :fields (");
    for (int i = 0; i < t->fields.n; i++) {
      CGv2TypeField *f = t->fields[i];
      if (i) b.put(' ');
      b.puts_("(%");
      b.puts_(f->name ? f->name : "?");
      b.puts_(" :type ");
      print_type_ref(b, f->type);
      b.putf(" :idx %d", f->idx);
      b.put(')');
    }
    b.put(')');
  }
  b.put(')');
}

static void print_global(Buf &b, CGv2Value *v) {
  b.puts_("  (value %");
  b.puts_(v->name ? v->name : "?");
  if (v->type) {
    b.puts_(" :type ");
    print_type_ref(b, v->type);
  }
  b.puts_(" :scope ");
  b.puts_(scope_name(v->scope));
  if (v->target_name) {
    b.puts_(" :target %");
    b.puts_(v->target_name);
  }
  b.put(')');
}

cchar *cg_v2_print(CGv2Program *prog) {
  Buf b;
  b.put('(');
  bool first = true;
  for (CGv2Type *t : prog->types) {
    if (!first) b.put('\n');
    print_type_decl(b, t);
    first = false;
  }
  for (CGv2Value *cv : prog->constants) {
    if (!first) b.put('\n');
    print_const(b, cv);
    first = false;
  }
  for (CGv2Value *gv : prog->globals) {
    if (!first) b.put('\n');
    print_global(b, gv);
    first = false;
  }
  for (CGv2Fun *f : prog->funs) {
    if (!first) b.put('\n');
    print_fun(b, f);
    first = false;
  }
  b.put(')');
  return dupstr(b.data);
}
