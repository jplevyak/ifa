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
  CG2T_PTR,           // typed ptr — element MUST be non-null
  CG2T_OPAQUE,        // opaque ptr — no indexable element known
  CG2T_STRUCT,
  CG2T_VECTOR,        // struct prefix + trailing flexible array
                      // (`@vector("s")` classes like bytearray);
                      // `fields` is the prefix, `element` is the
                      // trailing array's element type
  CG2T_FUN_PTR,
  CG2T_REF,
  CG2T_SUM,
  CG2T_SYMBOL,
};

// Struct field. Owned by the parent CGv2Type via fields[].
class CGv2TypeField : public gc {
 public:
  cchar *name;
  CGv2Type *type;
  int idx;
  CGv2TypeField() : name(0), type(0), idx(0) {}
};

class CGv2Type : public gc {
 public:
  int id;
  cchar *name;          // canonical name ("void", "int64", user types)
  CGv2TypeKind kind;
  int bits;             // for numeric kinds; 0 otherwise

  // For CG2T_STRUCT / CG2T_VECTOR.
  Vec<CGv2TypeField *> fields;
  bool is_heap_aggregate;     // alloc via GC heap (vs stack)

  // For CG2T_PTR / CG2T_REF — the element type pointed to.
  // For CG2T_VECTOR — the trailing flexible-array element type.
  // INVARIANT: when kind == CG2T_PTR, `element` is non-null.
  // Opaque ptrs are CG2T_OPAQUE (with element == nullptr).
  CGv2Type *element;

  // (fun_sig, alias_of land with their tests.)

  CGv2Type() : id(0), name(0), kind(CG2T_VOID), bits(0),
               is_heap_aggregate(false), element(0) {}
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

  // For CG2V_FUN_REF: the textual fn name this ref binds to.
  // Resolved at emit time via TheModule->getFunction().
  cchar *target_name;

  CGv2Value() : id(0), name(0), type(0), scope(CG2V_LOCAL),
                target_name(0) {}
};

// ============================================================
//   OPERATIONS
// ============================================================

enum CGv2Op {
  CG2_NOP,
  CG2_MOVE,
  // Body ops:
  CG2_BINOP,           // sub_op selects add/sub/mul/lt/eq/...
  CG2_CALL,            // rvals[0]=fun ref, rvals[1..]=args, lvals[0]=result
  CG2_ALLOC,           // type_arg=struct, lvals[0]=ptr to heap aggregate
  CG2_FIELD_STORE,     // field_idx=N, rvals=(ptr, value)
  CG2_FIELD_LOAD,      // field_idx=N, rvals=(ptr), lvals[0]=loaded
  CG2_INDEX_LOAD,      // rvals=(ptr, idx), lvals[0]=loaded element
  CG2_INDEX_STORE,     // rvals=(ptr, idx, value)
  CG2_LEN,             // rvals=(obj), lvals[0]=i64 length
  CG2_CAST,            // type_arg=dst type, rvals=(src), lvals[0]=cast result
  CG2_SIZEOF,          // type_arg=T, lvals[0]=i64 sizeof(T)
  CG2_SIZEOF_ELEMENT,  // rvals=(typed_ptr), lvals[0]=i64 sizeof(ptr->element)
  CG2_CLONE,           // type_arg=T, rvals=(proto), lvals[0]=new ptr-to-T
  CG2_C_CALL,          // prim_name=C fn name, type_arg=ret type,
                       // rvals=args (each arg's type comes from its
                       // CGv2Value::type); lvals[0]=result (optional).
                       // External function declared on-demand.
  CG2_PRIM,            // prim_name + rvals=args, lvals[0]=result (optional)
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
  // Arithmetic (signed)
  CG2B_ADD,
  CG2B_SUB,
  CG2B_MUL,
  CG2B_DIV,
  CG2B_MOD,
  // Comparison (signed)
  CG2B_LT,
  CG2B_LE,
  CG2B_GT,
  CG2B_GE,
  CG2B_EQ,
  CG2B_NE,
  // Bitwise / logical
  CG2B_AND,
  CG2B_OR,
  CG2B_XOR,
  CG2B_SHL,
  CG2B_SHR,
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

  // For aggregate ops
  CGv2Type *type_arg;       // CG2_ALLOC (type to allocate), optional
                            // :struct hint on CG2_FIELD_*
  int field_idx;            // CG2_FIELD_STORE / CG2_FIELD_LOAD

  // For CG2_PRIM — name of the primitive (e.g. "write"). The
  // emitter dispatches to a per-prim emit function by name.
  cchar *prim_name;

  CGv2Inst() : id(0), name(0), op(CG2_NOP), sub_op(CG2B_NONE),
               br_target(0), br_true(0), br_false(0),
               type_arg(0), field_idx(0), prim_name(0) {}
};

// ============================================================
//   BLOCKS
// ============================================================

// Phi-by-pred group: for one predecessor of a block, the list
// of MOVE insts to execute on the edge from that pred into
// this block. The emitter places these in the predecessor's
// terminator emission (mirroring LLVM's phi-on-edge semantics
// via the alloca-and-store convention).
class CGv2PhiGroup : public gc {
 public:
  CGv2Block *pred;
  Vec<CGv2Inst *> moves;       // all CG2_MOVE
  CGv2PhiGroup() : pred(0) {}
};

class CGv2Block : public gc {
 public:
  int id;
  cchar *name;              // label name (optional)

  Vec<CGv2Inst *> body;
  CGv2Inst *terminator;

  Vec<CGv2Block *> preds;
  Vec<CGv2Block *> succs;

  // phi_by_pred: groups of MOVE insts per predecessor. For
  // each pred P, the group's `moves` execute on the edge
  // (P → this). Empty if this block has no phis.
  Vec<CGv2PhiGroup *> phi_by_pred;

  CGv2Block() : id(0), name(0), terminator(0) {}
};

// ============================================================
//   FUNCTIONS
// ============================================================

// Function signature: ret + arg types.
//
// Issue 023 Stage 2 — when the IF1 return type is a value-type
// RECORD (Sym::is_value_type=1 → CG2T_PTR whose element CGv2Type
// has is_heap_aggregate=false), the signature is rewritten to
// LLVM sret form: `ret` becomes t_void, a `ptr sret(struct)`
// is implicitly prepended to the LLVM signature, and CG2_RET in
// the body emits `ret void` (after the value has been written
// through the sret slot).  Callers see `is_sret==true` and
// alloca a slot of `sret_struct` to pass as the implicit first
// arg.  The `CGv2Type *ret` field still holds the *logical*
// return type so callers can recover the dst CGv2Value's type;
// only the LLVM-level signature shape is different.
class CGv2Sig : public gc {
 public:
  CGv2Type *ret;
  Vec<CGv2Type *> args;
  bool is_varargs;
  bool is_sret;
  CGv2Type *sret_struct;   // the CG2T_STRUCT pointee for sret slot
  CGv2Sig() : ret(0), is_varargs(false), is_sret(false),
              sret_struct(0) {}
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
  CGv2Type *t_fun_ptr;
  CGv2Type *t_ptr;

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
