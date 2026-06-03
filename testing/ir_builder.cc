// SPDX-License-Identifier: BSD-3-Clause
//
// Layer 2 IR builders. See ir_builder.h.

#include "ifadefs.h"

#include "builtin.h"
#include "code.h"
#include "if1.h"
#include "num.h"
#include "sym.h"
#include "testing/ir_builder.h"

#include <string.h>

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

namespace {
inline Vec<Sym *> *as_vec(void *erased) { return static_cast<Vec<Sym *> *>(erased); }
}  // namespace

// ---------------------------------------------------------------------------
// CodeBuilder
// ---------------------------------------------------------------------------

CodeBuilder::CodeBuilder() : code_(nullptr) {}

void CodeBuilder::move(Sym *src, Sym *dst) { if1_move(if1, &code_, src, dst); }

void CodeBuilder::label(Label *l) { if1_label(if1, &code_, nullptr, l); }

void CodeBuilder::goto_(Label *l) { if1_goto(if1, &code_, l); }

Sym *CodeBuilder::send_method(Sym *method, Sym *recv,
                              std::initializer_list<Sym *> args, Sym *result) {
  if (!result) result = ir::local();
  // (send method recv args... => result)
  // nrvals = 1 (method) + 1 (recv) + args.size(); nlvals = 1.
  int nr = 2 + (int)args.size();
  Code *c = if1_send1(if1, &code_);
  if1_add_send_arg(if1, c, method);
  if1_add_send_arg(if1, c, recv);
  for (Sym *a : args) if1_add_send_arg(if1, c, a);
  if1_add_send_result(if1, c, result);
  (void)nr;
  return result;
}

Sym *CodeBuilder::send_op(std::initializer_list<Sym *> args, Sym *result) {
  if (!result) result = ir::local();
  Code *c = if1_send1(if1, &code_);
  if1_add_send_arg(if1, c, sym_operator);
  for (Sym *a : args) if1_add_send_arg(if1, c, a);
  if1_add_send_result(if1, c, result);
  return result;
}

void CodeBuilder::send_prim(Sym *prim_sym,
                            std::initializer_list<Sym *> rvals_after,
                            std::initializer_list<Sym *> lvals) {
  Code *c = if1_send1(if1, &code_);
  if1_add_send_arg(if1, c, sym_primitive);
  if1_add_send_arg(if1, c, prim_sym);
  for (Sym *a : rvals_after) if1_add_send_arg(if1, c, a);
  for (Sym *l : lvals) if1_add_send_result(if1, c, l);
}

void CodeBuilder::reply(Sym *cont, Sym *ret) {
  send_prim(sym_reply, {cont, ret}, {});
}

void CodeBuilder::splice(Code *sub) { if (sub) if1_gen(if1, &code_, sub); }

void CodeBuilder::if_(Sym *cond,
                      std::function<void(CodeBuilder &)> then_,
                      std::function<void(CodeBuilder &)> else_) {
  // Build sub-bodies into temporary CodeBuilders.
  CodeBuilder then_b;
  then_(then_b);
  Code *then_code = then_b.finish();
  Code *else_code = nullptr;
  if (else_) {
    CodeBuilder else_b;
    else_(else_b);
    else_code = else_b.finish();
  }
  // if1_if takes a "cond evaluation code" (we have none — caller
  // already emitted), the cond Sym, then/else codes + result Syms.
  // Use null for code/result-syms since we don't track an if-expr
  // result here.
  if1_if(if1, &code_, /*ifcond*/ nullptr, cond,
         then_code, /*if_var*/ nullptr,
         else_code, /*then_var*/ nullptr,
         /*result*/ nullptr);
}

void CodeBuilder::while_(std::function<Sym *(CodeBuilder &)> cond_emit,
                         std::function<void(CodeBuilder &)> body_emit) {
  Label *cont = if1_alloc_label(if1);
  Label *brk = if1_alloc_label(if1);
  CodeBuilder cond_b, body_b;
  Sym *cond_sym = cond_emit(cond_b);
  body_emit(body_b);
  if1_loop(if1, &code_, cont, brk, cond_sym,
           /*before*/ nullptr,
           cond_b.finish(),
           /*after*/ nullptr,
           body_b.finish());
}

Code *CodeBuilder::finish() {
  Code *c = code_;
  code_ = nullptr;
  return c;
}

// ---------------------------------------------------------------------------
// ClosureBuilder
// ---------------------------------------------------------------------------

ClosureBuilder::ClosureBuilder(cchar *name)
    : fn_(new_Sym(name)), self_(nullptr), args_storage_(new Vec<Sym *>) {
  fn_->cont = new_Sym();
  fn_->ret = new_Sym();
}

ClosureBuilder &ClosureBuilder::arg(Sym *s) {
  as_vec(args_storage_)->add(s);
  return *this;
}

ClosureBuilder &ClosureBuilder::args(std::initializer_list<Sym *> ss) {
  for (Sym *s : ss) as_vec(args_storage_)->add(s);
  return *this;
}

ClosureBuilder &ClosureBuilder::self(Sym *s) {
  self_ = s;
  fn_->self = s;
  return *this;
}

Sym *ClosureBuilder::method(cchar *method_name, Sym *recv_type) {
  // Per V's gen_fun convention: first formal is a fresh sym
  // constrained to the method-symbol; second formal is self
  // constrained to the receiver type. Rename the fn to match the
  // method name so the dispatch tag (if1_make_symbol(fn_name) in
  // body()) lines up with the call-site's symbol.
  fn_->name = if1_cannonicalize_string(if1, method_name);
  // Self sym, constrained to recv_type via must_specialize.
  Sym *self_sym = ir::local("self");
  self_sym->must_implement_and_specialize(recv_type);
  self_ = self_sym;
  fn_->self = self_sym;
  return self_sym;
}

Sym *ClosureBuilder::body(
    std::function<void(CodeBuilder &cb, Sym *cont, Sym *ret)> emit) {
  CodeBuilder cb;
  emit(cb, fn_->cont, fn_->ret);
  Code *body_code = cb.finish();

  // if1_closure takes nargs + Sym** to the formals. The convention
  // (matching V's gen_fun and the test harness's fa_setup) is:
  //   args[0] = dispatch symbol (= fn name)
  //   args[1] = self if present
  //   args[2..] = positional formals
  Vec<Sym *> *user_args = as_vec(args_storage_);
  Vec<Sym *> closure_args;
  closure_args.add(if1_make_symbol(if1, fn_->name));
  if (self_) closure_args.add(self_);
  for (Sym *s : *user_args) closure_args.add(s);
  if1_closure(if1, fn_, body_code, closure_args.n, closure_args.v);
  return fn_;
}

// ---------------------------------------------------------------------------
// RecordBuilder
// ---------------------------------------------------------------------------

RecordBuilder::RecordBuilder(cchar *name)
    : type_(new_Sym(name)), fields_storage_(new Vec<Sym *>) {
  type_->type_kind = Type_RECORD;
}

RecordBuilder &RecordBuilder::field(cchar *name, Sym *type) {
  Sym *f = new_Sym(name);
  f->in = type_;
  if (type) f->type = type;
  as_vec(fields_storage_)->add(f);
  return *this;
}

RecordBuilder &RecordBuilder::vector(Sym *element_type) {
  type_->is_vector = 1;
  type_->element = element_type ? element_type : new_Sym();
  return *this;
}

Sym *RecordBuilder::build() {
  Vec<Sym *> *fields = as_vec(fields_storage_);
  for (Sym *f : *fields) type_->has.add(f);
  return type_;
}

// ---------------------------------------------------------------------------
// ir:: helpers
// ---------------------------------------------------------------------------

namespace ir {

Sym *new_instance(CodeBuilder &cb, Sym *type, cchar *result_name) {
  Sym *result = local(result_name);
  // (send sym_primitive sym_new type => result)
  cb.send_prim(sym_new, {type}, {result});
  return result;
}

void set_field(CodeBuilder &cb, Sym *obj, cchar *field, Sym *val) {
  Sym *field_sym = if1_make_symbol(if1, field);
  Sym *dead = local();
  // (send sym_operator obj sym_setter sym_field val => dead)
  cb.send_op({obj, sym_setter, field_sym, val}, dead);
}

Sym *get_field(CodeBuilder &cb, Sym *obj, cchar *field, cchar *result_name) {
  Sym *field_sym = if1_make_symbol(if1, field);
  Sym *result = local(result_name);
  // (send sym_operator obj sym_period sym_field => result)
  return cb.send_op({obj, sym_period, field_sym}, result);
}

Sym *call_method(CodeBuilder &cb, Sym *obj, cchar *method,
                 std::initializer_list<Sym *> args, cchar *result_name) {
  Sym *method_sym = if1_make_symbol(if1, method);
  Sym *result = local(result_name);
  return cb.send_method(method_sym, obj, args, result);
}

Sym *call_fn(CodeBuilder &cb, Sym *fn, std::initializer_list<Sym *> args,
             cchar *result_name) {
  Sym *result = local(result_name);
  // (send fn args... => result) — fn is rval[0], no separate recv.
  Code *c = if1_send1(if1, cb.slot());
  if1_add_send_arg(if1, c, fn);
  for (Sym *a : args) if1_add_send_arg(if1, c, a);
  if1_add_send_result(if1, c, result);
  return result;
}

void vec_set(CodeBuilder &cb, Sym *vec, Sym *index, Sym *val) {
  // (send sym_primitive sym_set_index_object vec index val => _)
  Sym *dead = local();
  cb.send_prim(sym_set_index_object, {vec, index, val}, {dead});
}

Sym *vec_get(CodeBuilder &cb, Sym *vec, Sym *index, cchar *result_name) {
  Sym *result = local(result_name);
  cb.send_prim(sym_index_object, {vec, index}, {result});
  return result;
}

Sym *local(cchar *name) {
  Sym *s = new_Sym(name);
  s->is_local = 1;
  s->nesting_depth = LOCALLY_NESTED;
  return s;
}

Sym *symbol(cchar *name) {
  Sym *s = if1_make_symbol(if1, name);
  return s;
}

// Internal: wire up a constant Sym with the same fields parse_ir.cc's
// register_const_sym sets. Without these, FA reports NOTYPE because
// make_AVar can't seed an AType.
static Sym *register_const(Sym *target, Sym *type_sym) {
  target->is_constant = 1;
  if (type_sym) {
    target->type = type_sym;
    target->meta_type = target;
    if (!target->implements.in(type_sym)) target->implements.add(type_sym);
    if (!target->specializes.in(type_sym)) target->specializes.add(type_sym);
    if1->constants.put(&target->imm, target);
  }
  return target;
}

Sym *const_int32(int v) {
  Sym *s = new_Sym();
  s->imm.set_int(v, IF1_INT_TYPE_32);
  return register_const(s, if1_get_builtin(if1, "int32"));
}

Sym *const_int64(long long v) {
  Sym *s = new_Sym();
  s->imm.set_int(v, IF1_INT_TYPE_64);
  return register_const(s, if1_get_builtin(if1, "int64"));
}

Sym *const_float64(double v) {
  Sym *s = new_Sym();
  s->imm.set_float(v, IF1_FLOAT_TYPE_64);
  return register_const(s, if1_get_builtin(if1, "float64"));
}

Sym *const_string(cchar *str) {
  Sym *s = new_Sym();
  s->imm.const_kind = IF1_CONST_KIND_STRING;
  s->imm.v_string = if1_cannonicalize_string(if1, str);
  s->constant = s->imm.v_string;
  return register_const(s, if1_get_builtin(if1, "string"));
}

}  // namespace ir
