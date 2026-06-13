// SPDX-License-Identifier: BSD-3-Clause
//
// CG_IR — the codegen-time intermediate representation produced by
// `cg_normalize(fa)` and consumed by both the C backend
// (`c_codegen_print_c`) and the LLVM backend
// (`llvm_codegen_print_ir`).
//
// Phase 1 of CG_IR_PLAN — header-only scaffolding. The
// declarations here are the contract; the implementation in
// `cg_normalize.cc` is a stub through Phase 1 and gets filled in
// during Phase 2. The two printers continue to consume IF1
// directly until Phases 3-4 swap them over.
//
// Design rationale: see [ifa/CODE_GEN_IR.md](../CODE_GEN_IR.md).
// Execution plan: see [CG_IR_PLAN.md](CG_IR_PLAN.md).

#ifndef _cg_ir_H_
#define _cg_ir_H_

#include "fa.h"

// Forward declarations from IF1 — included via fa.h's transitive
// includes, but declared here for documentation.
class Fun;
class Sym;
class Var;
class PNode;
class Prim;

// LLVM types — opaque pointers stashed on CGSlot / CGFun / CGType.
// Declared as `void *` here so the header is consumable without
// pulling in LLVM headers. The LLVM backend casts back at the
// use site.
namespace llvm { class Value; class Function; class Type; }

// Forward declarations for circular references within CG_IR.
class CGProgram;
class CGFun;
class CGBlock;
class CGInst;
class CGValue;
class CGSlot;
class CGType;

// =====================================================================
// CGType — explicit "what this value or slot holds" at LLVM-IR / C
// level. Replaces the on-the-fly derivation from Sym's flag soup
// (`type_kind`, `num_kind`, `is_value_type`, `has.n`, `is_fun`).
// =====================================================================

enum CGTypeKind {
  CG_T_VOID,
  CG_T_INT,         // signed integer; `bits` is the width
  CG_T_UINT,        // unsigned integer; `bits` is the width
  CG_T_FLOAT,       // float; `bits` is the width (32/64/128)
  CG_T_BOOL,        // i1
  CG_T_PTR,         // opaque pointer — heap aggregate, ref, etc.
                    //   Variables of CG_T_PTR are stored as
                    //   pointers (matching `_CG_psN` in C).
  CG_T_STRUCT,      // value-typed record (rare in pyc; common in V)
  CG_T_FUN_PTR,     // pointer to a function; call sites consume
                    //   the CGFun signature, not the field types here.
};

class CGType : public gc {
 public:
  CGTypeKind kind;
  int bits;                      // INT/UINT/FLOAT width
  Vec<CGType *> fields;          // STRUCT field types, in order
  Vec<cchar *> field_names;      // parallel to fields, for diagnostics
  CGType *element;               // arrays/vectors element type
  Sym *source;                   // IF1 Sym this came from (debugging)
  cchar *name;                   // emission name (`_CG_psN`, `i32`, ...)

  // Variables of this type are stored as pointers (matching the C
  // backend's `_CG_psN` typedef convention).
  bool is_heap_aggregate() const { return kind == CG_T_PTR; }

  // LLVM-side cache, populated lazily by `cg_to_llvm_type()`. Phase
  // 3.1 of CG_IR_PLAN — parallel function until 3.3 wires it up.
  llvm::Type *llvm_handle;

  CGType()
    : kind(CG_T_VOID), bits(0), element(0),
      source(0), name(0), llvm_handle(0) {}
};

// =====================================================================
// CGSlot — addressable storage. Globals, formal-arg slots, locals.
// =====================================================================

enum CGSlotKind {
  CG_SLOT_GLOBAL,
  CG_SLOT_LOCAL,                 // alloca / C local
  CG_SLOT_FORMAL,                // function parameter
  CG_SLOT_CONSTANT,              // immediate / constant pool
};

class CGSlot : public gc {
 public:
  CGSlotKind kind;
  CGType *type;
  cchar *name;                   // user-level name (preserved for debug info)
  cchar *cg_name;                // emission name (`t0`, `%local_var3`, `@y`)
  int id;
  Sym *source_sym;               // back-ref for debugging
  Var *source_var;               // back-ref for debugging
  Immediate *imm;                // for CG_SLOT_CONSTANT
  llvm::Value *llvm_handle;      // AllocaInst*/GlobalVariable*/Argument*,
                                 //   set lazily by the LLVM printer

  CGSlot()
    : kind(CG_SLOT_LOCAL), type(0), name(0), cg_name(0),
      id(0), source_sym(0), source_var(0), imm(0),
      llvm_handle(0) {}
};

// =====================================================================
// CGValue — a use-site reference. Either an immediate, a slot
// (resolved via CG_LOAD), or the result of a prior CGInst (SSA).
// =====================================================================

enum CGValueKind {
  CG_V_NONE,
  CG_V_INST,                     // result of a prior CGInst in this CGBlock
  CG_V_SLOT,                     // slot reference (loaded by the printer)
  CG_V_IMMEDIATE,                // constant
  CG_V_FUN,                      // function pointer for direct call
};

class CGValue : public gc {
 public:
  CGValueKind kind;
  CGInst *inst;                  // for CG_V_INST
  CGSlot *slot;                  // for CG_V_SLOT
  CGFun *fun;                    // for CG_V_FUN
  Immediate imm;                 // for CG_V_IMMEDIATE
  CGType *type;                  // result type (for typed printers)

  CGValue() : kind(CG_V_NONE), inst(0), slot(0), fun(0), type(0) {}
};

// =====================================================================
// CGInst — one operation. Either a non-terminator (goes in
// `CGBlock::body`) or a terminator (goes in `CGBlock::terminator`).
// =====================================================================

enum CGOp {
  // Pure operations (go in CGBlock::body)
  CG_NOP,
  CG_LOAD,             // result = load(slot)
  CG_STORE,            // store(rvals[0] → slot)
  CG_GEP_FIELD,        // result = &(rvals[0]->field[field_idx])
  CG_LOAD_FIELD,       // result = rvals[0]->field[field_idx]
  CG_STORE_FIELD,      // store(rvals[1] → rvals[0]->field[field_idx])
  CG_CALL,             // result = fun(rvals...)
  CG_ALLOC,            // result = GC_malloc(sizeof(type))
  CG_CAST,             // result = (target_type) rvals[0]
  CG_PRIM_OP,          // arithmetic / comparison / bitwise — kind in `prim`
  CG_PRIM_CGFN,        // dispatch to RegisteredPrim::cgfn
                       //   (write/writeln/to_string/__pyc_c_call__/...)

  // Terminators (go in CGBlock::terminator; must be last)
  CG_BR,               // unconditional branch to block's succs[0]
  CG_COND_BR,          // rvals[0] ? succs[0] : succs[1]
  CG_RET,              // ret rvals[0] (or void)
  CG_UNREACHABLE,      // verifyModule-required terminator filler
};

class CGInst : public gc {
 public:
  CGOp op;
  CGType *result_type;           // for CG_V_INST consumers
  Vec<CGValue *> rvals;
  CGSlot *slot;                  // for LOAD/STORE/GEP_FIELD/STORE_FIELD/ALLOC
  int field_idx;                 // for *_FIELD ops, pre-resolved at
                                 //   normalization time (no more atoi at
                                 //   emission — see for-loop bug history)
  Prim *prim;                    // for CG_PRIM_OP / CG_PRIM_CGFN
  cchar *prim_name;              // for CG_PRIM_CGFN dispatch
  unsigned src_line;             // for !dbg
  cchar *src_file;
  PNode *source_pn;              // back-ref for debugging
  // Phase 2.4 emits phi/phy MOVEs as CG_STOREs in the block body
  // so the cg-normalize golden visualizes them. The LLVM emitter
  // re-emits the same MOVEs at terminator time via
  // emit_phi_phy (Track 3). To avoid double-emission, the body
  // CG_STOREs carry this bit so emit_cg_inst skips them.
  unsigned is_phi_phy : 1;

  CGInst()
    : op(CG_NOP), result_type(0), slot(0), field_idx(-1),
      prim(0), prim_name(0), src_line(0), src_file(0),
      source_pn(0), is_phi_phy(0) {}
};

// =====================================================================
// CGBlock — one basic block.
// =====================================================================

class CGBlock : public gc {
 public:
  Vec<CGInst *> body;            // non-terminator instructions, in order
  CGInst *terminator;            // required (BR/COND_BR/RET/UNREACHABLE)
  Vec<CGBlock *> preds;
  Vec<CGBlock *> succs;
  int id;
  cchar *label;                  // emission name (`L42`, `%entry`, ...)
  PNode *source_pn;              // the LABEL PNode this came from

  CGBlock() : terminator(0), id(0), label(0), source_pn(0) {}
};

// =====================================================================
// CGFun — one function (= one Fun post-clone).
// =====================================================================

class CGFun : public gc {
 public:
  Fun *source_fun;               // back-ref (post-clone Fun)
  cchar *name;
  CGType *return_type;
  Vec<CGType *> arg_types;
  Vec<CGSlot *> formal_arg_slots;// pre-allocated; entry block stores args in
  Vec<CGBlock *> blocks;         // in CFG order; entry first
  CGBlock *entry;
  Vec<CGSlot *> locals;          // all CGSlots used in this fun
  bool is_external;              // declaration only (extern symbol)
  bool is_main;                  // pyc top-level
  llvm::Function *llvm_handle;   // set by the LLVM printer

  CGFun()
    : source_fun(0), name(0), return_type(0),
      entry(0), is_external(false), is_main(false),
      llvm_handle(0) {}
};

// =====================================================================
// CGProgram — the whole compilation unit (one per pyc compile).
// =====================================================================

class CGProgram : public gc {
 public:
  Vec<CGFun *> funs;
  CGFun *main_fun;               // entry point (pyc __main__)
  Vec<CGSlot *> globals;
  Vec<CGType *> types;           // emitted at top of C output
  Map<Fun *, CGFun *> fun_map;   // for cross-Fun call resolution
  Map<Sym *, CGSlot *> sym_to_slot;
  Map<Sym *, CGType *> sym_to_type;
  IFACallbacks *frontend;        // c_codegen_pre_file et al.

  CGProgram() : main_fun(0), frontend(0) {}
};

// =====================================================================
// The normalization pass — Phase 2 fills this in. Phase 1 stub:
// returns an empty CGProgram (no funs, no globals, no types).
// =====================================================================

CGProgram *cg_normalize(FA *fa);

#endif  // _cg_ir_H_
