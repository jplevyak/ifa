// SPDX-License-Identifier: BSD-3-Clause
//
// Layer 2 of the IR builder (Phase 09 step C 7.1). Ergonomic
// composites over the if1_* APIs, designed for synthetic IFA test
// shapes but generally reusable for any IFA frontend.
//
// See ifa/testing/phases/09d_generator_design.md §2 for the API
// layering rationale. See 09b_ast_to_if1_patterns.md for the
// patterns V's lowering uses — this header captures the equivalents
// without the AST/scope plumbing.
//
// Three classes + a small free-function namespace:
//
//   CodeBuilder    — accumulates a Code stream (sends, moves,
//                    labels, control flow).
//   ClosureBuilder — collects formals + body, calls if1_closure.
//   RecordBuilder  — constructs a RECORD type with fields.
//   ir::*          — single-call helpers for the most common
//                    patterns (allocate, get/set field, method call).

#ifndef IFA_TESTING_IR_BUILDER_H
#define IFA_TESTING_IR_BUILDER_H

#include "ifadefs.h"

#include <functional>
#include <initializer_list>

class Code;
class Label;
class Sym;

// ---------------------------------------------------------------------------
// CodeBuilder
// ---------------------------------------------------------------------------
//
// Wraps a Code* and offers high-level emit operations. The
// underlying Code* is exposed via finish() for handoff to
// if1_closure or other consumers.
//
// All methods append to the builder's internal Code stream in
// source order. No reordering, no optimization — same semantics as
// calling the if1_* APIs in sequence.

class CodeBuilder {
 public:
  CodeBuilder();

  // Direct emission.
  void move(Sym *src, Sym *dst);
  void label(Label *l);
  void goto_(Label *l);

  // Method dispatch:
  //   (send method_sym receiver args... => result)
  // If result is null, a fresh temp is created and returned.
  Sym *send_method(Sym *method, Sym *recv, std::initializer_list<Sym *> args,
                   Sym *result = nullptr);

  // Operator dispatch:
  //   (send sym_operator args... => result)
  // For binary: args = {lhs, op_sym, rhs}.
  // For unary: args = {op_sym, operand}.
  // For field access / setter: see ir::get_field / ir::set_field.
  Sym *send_op(std::initializer_list<Sym *> args, Sym *result = nullptr);

  // Primitive dispatch:
  //   (send sym_primitive prim_sym rvals... => lvals...)
  // Use rvals_after for additional args after the prim symbol.
  void send_prim(Sym *prim_sym, std::initializer_list<Sym *> rvals_after,
                 std::initializer_list<Sym *> lvals);

  // Reply terminator (fun-body end):
  //   (send sym_primitive sym_reply cont ret)
  void reply(Sym *cont, Sym *ret);

  // Splice another builder's output into this one.
  void splice(Code *sub);

  // Lambda-based control flow.
  //
  // if_: cond must already be in the stream above; emits the
  // conditional branch into then_ / else_ sub-builders.
  void if_(Sym *cond,
           std::function<void(CodeBuilder &)> then_,
           std::function<void(CodeBuilder &)> else_ = {});

  // while_: cond_emit emits the cond evaluation and returns the
  // bool Sym; body_emit emits the loop body.
  void while_(std::function<Sym *(CodeBuilder &)> cond_emit,
              std::function<void(CodeBuilder &)> body_emit);

  // Finalize: returns the accumulated Code tree. The builder
  // shouldn't be used after finish().
  Code *finish();

  // Access the raw Code* (for advanced use; prefer finish()).
  Code *raw() { return code_; }
  Code **slot() { return &code_; }  // for direct if1_* calls

 private:
  Code *code_;
};

// ---------------------------------------------------------------------------
// ClosureBuilder
// ---------------------------------------------------------------------------
//
// Fluent interface for constructing a closure (function). Creates
// the fn Sym, collects formal args, and calls if1_closure when
// body() is invoked.
//
// Typical use:
//
//   Sym *fn = ClosureBuilder("my_fun")
//     .arg(x_sym)
//     .arg(y_sym)
//     .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
//       cb.send_method(sym_add, x_sym, {y_sym}, ret);
//       cb.reply(cont, ret);
//     });
//
// The fn->cont and fn->ret Syms are created and passed into the
// body lambda; the caller is responsible for emitting a reply.

class ClosureBuilder {
 public:
  // Creates the fn Sym with the given name.
  explicit ClosureBuilder(cchar *name);

  // Append a positional formal.
  ClosureBuilder &arg(Sym *s);

  // Convenience: add multiple formals at once.
  ClosureBuilder &args(std::initializer_list<Sym *> ss);

  // Set the closure's self (receiver) sym, for method-shape funs.
  ClosureBuilder &self(Sym *s);

  // Mark this closure as a method named `method_name` on receiver
  // type `recv_type`. The first formal (created internally) is
  // a method-symbol placeholder constrained via
  // must_implement_and_specialize to the method's symbol Sym, and
  // self() is set up with must_specialize on recv_type. Returns
  // the self Sym so the body can reference it. After method(),
  // .arg(...) adds positional formals beyond self.
  Sym *method(cchar *method_name, Sym *recv_type);

  // Build the body and finalize via if1_closure.
  // Returns the fn Sym (also accessible later via fn()).
  Sym *body(std::function<void(CodeBuilder &cb, Sym *cont, Sym *ret)> emit);

  // Access the fn Sym at any point.
  Sym *fn() const { return fn_; }

 private:
  Sym *fn_;
  Sym *self_;
  // Storage for collected formals before if1_closure is called.
  void *args_storage_;  // erased Vec<Sym*>*; see .cc
};

// ---------------------------------------------------------------------------
// RecordBuilder
// ---------------------------------------------------------------------------
//
// Constructs a RECORD-kind type with a list of named fields.
//
// Typical use:
//
//   Sym *Point = RecordBuilder("Point")
//     .field("x")
//     .field("y")
//     .build();
//
// build() finalizes the type and returns the type Sym.

class RecordBuilder {
 public:
  explicit RecordBuilder(cchar *name);
  RecordBuilder &field(cchar *name, Sym *type = nullptr);
  // Mark this type as a vector — sets is_vector=1 and creates an
  // `element` Sym. Used for shapes targeting the split_css setter
  // path (the only post-type stage pyc programs actually reach,
  // via list runtime). element_type defaults to a fresh "any" Sym.
  RecordBuilder &vector(Sym *element_type = nullptr);
  Sym *build();

 private:
  Sym *type_;
  void *fields_storage_;  // erased Vec<Sym*>*
};

// ---------------------------------------------------------------------------
// ir:: helpers
// ---------------------------------------------------------------------------
//
// Single-call wrappers for the most common construction patterns.
// All emit into the supplied CodeBuilder.

namespace ir {

// Allocate a new instance of `type` (via sym_primitive sym_new).
// Returns a fresh result Sym.
Sym *new_instance(CodeBuilder &cb, Sym *type, cchar *result_name = nullptr);

// Field setter: emit (send sym_operator obj sym_setter sym_field val => _).
// The lval is a fresh unnamed temp (intentionally — setters discard).
void set_field(CodeBuilder &cb, Sym *obj, cchar *field, Sym *val);

// Field getter: emit (send sym_operator obj sym_period sym_field => result).
// Returns the result Sym.
Sym *get_field(CodeBuilder &cb, Sym *obj, cchar *field,
               cchar *result_name = nullptr);

// Method call: emit (send method_sym obj args... => result).
// `method` is the name; this helper looks up / creates the symbol Sym.
Sym *call_method(CodeBuilder &cb, Sym *obj, cchar *method,
                 std::initializer_list<Sym *> args,
                 cchar *result_name = nullptr);

// Direct closure call: emit (send fn args... => result). No
// receiver / method-symbol indirection — fn IS the dispatch
// target (the .ir form `(send %f %a => %r)`).
Sym *call_fn(CodeBuilder &cb, Sym *fn, std::initializer_list<Sym *> args,
             cchar *result_name = nullptr);

// Vector element write: (send sym_primitive sym_set_index_object
// vec index val => _). For vector-shaped CSes, this writes val
// into the CS's element AVar (the entry point for split_css).
void vec_set(CodeBuilder &cb, Sym *vec, Sym *index, Sym *val);

// Vector element read: (send sym_primitive sym_index_object
// vec index => result). Returns the result Sym.
Sym *vec_get(CodeBuilder &cb, Sym *vec, Sym *index,
             cchar *result_name = nullptr);

// Create a fresh local Sym (nesting_depth = LOCALLY_NESTED so the
// finalize pass adjusts it to fn_depth + 1).
Sym *local(cchar *name = nullptr);

// Create a fresh symbol Sym (for dispatch tags / field names).
Sym *symbol(cchar *name);

// Constant Syms — fully wired (is_constant, type, meta_type,
// implements/specializes, registered in if1->constants) so FA sees
// them as typed values. Mirrors what parse_ir.cc's register_const_sym
// does for .ir fixtures.
//
// Requires init_default_builtin_types() to have been called (the
// fa-init / fa-converge etc. phases handle this via pre_parse).
Sym *const_int32(int v);
Sym *const_int64(long long v);
Sym *const_float64(double v);
Sym *const_string(cchar *s);

}  // namespace ir

#endif  // IFA_TESTING_IR_BUILDER_H
