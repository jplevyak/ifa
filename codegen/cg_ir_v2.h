// SPDX-License-Identifier: BSD-3-Clause
//
// cg_ir_v2.h — CG_IR_PLAN Phase 4 incremental landing.
//
// This is the v2 implementation header. Focused subset of
// CG_IR_SKETCH.h — only what's needed to round-trip the
// initial test programs from the synthetic corpus
// (ifa/codegen/CG_IR_TEXT.md).
//
// Growth model: this header expands one concept at a time as
// each synthetic test landing requires it. Keeping the header
// small lets each commit be small and reviewable.
//
// What's in v0 (this landing):
// - Forward declarations + a couple of types
// - The minimum needed for test 01 (empty void fn) and
//   test 02 (constant return) to round-trip through
//   parse → CGProgram → print → diff.
//
// What's deliberately NOT here yet:
// - CGFun's backend_cache (lands when emit_llvm_v2 lands)
// - CGBlock::phi_by_pred (lands with test 06)
// - CG_INDEX_LOAD / CG_PRIM / etc (land with their tests)
// - Per-CGInst source location (lands when debug info ships)
//
// See:
// - ifa/codegen/CG_IR_META_PLAN.md (the overall plan)
// - ifa/codegen/CG_IR_SKETCH.h (the full design target)
// - ifa/codegen/CG_IR_TEXT.md (textual form + test corpus)

#ifndef _cg_ir_v2_H_
#define _cg_ir_v2_H_

#include "fa.h"

class CGv2Type;
class CGv2Value;
class CGv2Inst;
class CGv2Block;
class CGv2Fun;
class CGv2Program;

// Naming note: classes are prefixed CGv2 to keep them clearly
// separate from cg_ir.h's CGType/CGSlot/CGValue (still in
// production use). When Phase 5 retires v1, we rename CGv2 ->
// CG and delete cg_ir.h.

// ============================================================
//   TYPES
// ============================================================

enum CGv2TypeKind {
  CG2T_VOID,
  CG2T_INT,           // bits = 1/8/16/32/64
  CG2T_UINT,
  CG2T_FLOAT,         // bits = 32/64/128
  CG2T_BOOL,
  CG2T_PTR,
  CG2T_STRUCT,
  CG2T_FUN_PTR,
  CG2T_REF,
  CG2T_SUM,
  CG2T_SYMBOL,
};

class CGv2Type : public gc {
 public:
  int id;
  cchar *name;          // canonical name ("void", "int64", user types)
  CGv2TypeKind kind;
  int bits;             // for numeric kinds; 0 otherwise

  // Fields, element, fun_sig, alias_of, is_heap_aggregate land
  // when their corresponding test cases land. Keep this lean.

  CGv2Type() : id(0), name(0), kind(CG2T_VOID), bits(0) {}
};

// ============================================================
//   VALUES
// ============================================================

enum CGv2ValueScope {
  CG2V_LOCAL,
  CG2V_FORMAL,
  CG2V_GLOBAL,
  CG2V_CONSTANT,
  CG2V_FUN_REF,
  CG2V_SYMBOL,
};

// CGv2Immediate — tagged union for compile-time constants.
// Mirrors the textual `(int N)` / `(float N)` / `(str "s")`
// syntax from CG_IR_TEXT.md.
class CGv2Immediate : public gc {
 public:
  enum Kind { I_NONE, I_INT, I_UINT, I_FLOAT, I_BOOL, I_STR,
              I_SYM, I_NIL, I_UNDEF };
  Kind kind;
  union {
    int64_t i;
    uint64_t u;
    double f;
    bool b;
  } v;
  cchar *str;       // for I_STR and I_SYM
  CGv2Immediate() : kind(I_NONE), str(0) { v.i = 0; }
};

class CGv2Value : public gc {
 public:
  int id;
  cchar *name;          // explicit, replaces v1's cg_string side-channel
  CGv2Type *type;
  CGv2ValueScope scope;

  // For CG2V_CONSTANT
  CGv2Immediate imm;

  CGv2Value() : id(0), name(0), type(0), scope(CG2V_LOCAL) {}
};

// ============================================================
//   OPERATIONS
// ============================================================

enum CGv2Op {
  CG2_NOP,
  CG2_MOVE,
  // Body ops:
  CG2_BINOP,           // sub_op selects add/sub/mul/lt/eq/...
  // Terminators:
  CG2_BR,
  CG2_COND_BR,
  CG2_RET,
  CG2_UNREACHABLE,
};

// Sub-kind for CG2_BINOP (and later CG2_UNOP/CG2_CAST). Lands
// one entry per landing test: test 04 adds ADD; test 05 adds
// LT; etc. CG2B_NONE is the default for non-binop insts.
enum CGv2BinSub {
  CG2B_NONE,
  CG2B_ADD,
  CG2B_LT,
};

class CGv2Inst : public gc {
 public:
  int id;
  cchar *name;              // textual id (%i0); optional
  CGv2Op op;
  CGv2BinSub sub_op;        // CG2B_NONE for non-binop

  Vec<CGv2Value *> rvals;
  Vec<CGv2Value *> lvals;

  // For terminators
  CGv2Block *br_target;     // CG2_BR
  CGv2Block *br_true;       // CG2_COND_BR
  CGv2Block *br_false;      // CG2_COND_BR

  CGv2Inst() : id(0), name(0), op(CG2_NOP), sub_op(CG2B_NONE),
               br_target(0), br_true(0), br_false(0) {}
};

// ============================================================
//   BLOCKS
// ============================================================

class CGv2Block : public gc {
 public:
  int id;
  cchar *name;              // label name (optional)

  Vec<CGv2Inst *> body;
  CGv2Inst *terminator;

  Vec<CGv2Block *> preds;
  Vec<CGv2Block *> succs;

  CGv2Block() : id(0), name(0), terminator(0) {}
};

// ============================================================
//   FUNCTIONS
// ============================================================

// Function signature: ret + arg types.
class CGv2Sig : public gc {
 public:
  CGv2Type *ret;
  Vec<CGv2Type *> args;
  bool is_varargs;
  CGv2Sig() : ret(0), is_varargs(false) {}
};

class CGv2Fun : public gc {
 public:
  int id;
  cchar *name;
  CGv2Sig *signature;

  CGv2Block *entry;
  Vec<CGv2Block *> blocks;

  Vec<CGv2Value *> formals;
  Vec<CGv2Value *> locals;

  bool live;
  bool is_external;
  bool is_varargs;
  bool is_main;

  CGv2Fun()
    : id(0), name(0), signature(0), entry(0),
      live(true), is_external(false), is_varargs(false),
      is_main(false) {}
};

// ============================================================
//   PROGRAM
// ============================================================

class CGv2Program : public gc {
 public:
  Vec<CGv2Type *> types;     // user-declared (program-scope)
  Vec<CGv2Value *> constants;
  Vec<CGv2Value *> globals;
  Vec<CGv2Fun *> funs;
  CGv2Fun *main_fun;

  // Predefined types — populated by program builder; nullptr
  // if no parser/builder has run.
  CGv2Type *t_void;
  CGv2Type *t_bool;
  CGv2Type *t_int8;
  CGv2Type *t_int16;
  CGv2Type *t_int32;
  CGv2Type *t_int64;
  CGv2Type *t_uint8;
  CGv2Type *t_uint16;
  CGv2Type *t_uint32;
  CGv2Type *t_uint64;
  CGv2Type *t_float32;
  CGv2Type *t_float64;
  CGv2Type *t_sym;
  CGv2Type *t_nil;

  CGv2Program();
  CGv2Type *lookup_type(cchar *name) const;
};

// ============================================================
//   PARSE / PRINT
// ============================================================
//
// The textual form is the source of truth (see
// CG_IR_TEXT.md). Round-trip property: parse(print(prog)) ==
// prog (semantically), and print(parse(text)) == text (modulo
// whitespace and comments).

// Parse a textual program. On parse error, returns nullptr and
// writes the error to `err` (cchar*; nullable). The error
// string is GC-allocated.
CGv2Program *cg_v2_parse(cchar *text, cchar **err);

// Print a program to a freshly GC-allocated text buffer.
cchar *cg_v2_print(CGv2Program *prog);

// Emit a CGv2Program into the current LLVM Module
// (TheModule from llvm.cc). Caller must initialize via
// llvm_codegen_initialize first. Returns true on success;
// on partial failure the module may be malformed (caller
// should run verifyModule).
bool cg_v2_emit_llvm_module(CGv2Program *prog);

#endif // _cg_ir_v2_H_
