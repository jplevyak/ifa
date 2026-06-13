// SPDX-License-Identifier: BSD-3-Clause
//
// CG_IR — codegen-time scaffolding produced by `cg_normalize(fa)`
// and consumed by the LLVM backend (and Phase 4 onwards, the C
// backend). The header declarations are the contract; cg_normalize
// fills them in from IF1 state and emit_cg consumes them.
//
// Status (2026-06-13): SCAFFOLDING WITH ESCAPE HATCHES. CG_IR is
// not an independent IR — see CG_IR_v2.md for the audit that
// confirms this. The escape hatches:
//
// - **CGType wraps Sym**: all production emission paths reach
//   the LLVM Type via `getLLVMType(CGType::source)`. CGType's
//   `kind`, `bits`, `field_names` are decorative (read only by
//   the cg-normalize golden printer).
//
// - **CGSlot wraps Var**: `slot_pointer(CGSlot)` falls back to
//   `cg_get_llvm_value(source_var)` in production. CGSlot::
//   llvm_handle is set only in standalone unit tests.
//
// - **CGValue is a thin tag over CGSlot**: only CG_V_SLOT is
//   exercised in production. CG_V_INST and CG_V_IMMEDIATE are
//   dead arms.
//
// - **CGInst::source_pn is THE dispatch key** (not a debugging
//   aid as previously documented). Every back-translation in
//   emit_cg_inst routes through `write_llvm_prim(source_fun,
//   source_pn)`. Without source_pn, ~95% of CGInsts can't be
//   emitted.
//
// What CG_IR genuinely contributes:
//
// - **CGBlock + block-level CFG**: per-basic-block iteration order
//   (which the IF1 worklist didn't guarantee) and explicit
//   `terminator` + `succs` metadata for the LLVM emitter.
// - **emit_terminator's phi/phy placement**: phi/phy MOVEs are
//   emitted in predecessor blocks rather than at the source
//   PNode — issue 016's structural fix.
//
// What CG_IR _doesn't_ contribute but should (CG_IR_v2 finding):
//
// - **Per-CGFun value cache**: the per-Var llvm::Value cache is
//   program-scoped; llvm::Value identity is function-scoped. This
//   gap is the structural root of issue 017 (construction-flow
//   `ptr undef`, cross-function instruction leak). A surgical fix
//   to update the program-scoped cache from emit_cg_inst causes
//   verifyModule failures, confirming the cache needs to move
//   on-CGFun. Deferred to v2.
//
// Design rationale: see [ifa/CODE_GEN_IR.md](../CODE_GEN_IR.md).
// Execution plan: see [CG_IR_PLAN.md](CG_IR_PLAN.md).
// Honest assessment after the swap: see [CG_IR_v2.md](CG_IR_v2.md).

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
// CGType — wraps `Sym *source` for the emitter. Production
// emission paths use `getLLVMType(source)`; the other fields
// (kind/bits/fields/field_names/element/name) are populated by
// cg_normalize for the cg-normalize golden printer's benefit
// and are otherwise decorative. The audit (CG_IR_v2 §A) tagged
// CGType for v2 removal.
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
// CGSlot — wraps `Var *source_var` (and `Sym *source_sym` for
// globals/constants). `slot_pointer(CGSlot)` in production falls
// back to `cg_get_llvm_value(source_var)` because cg_normalize
// never populates `llvm_handle` outside unit-test scaffolding.
// The other fields (kind/name/cg_name/id/imm) are populated
// by cg_normalize for the cg-normalize golden printer's benefit.
// Audit (CG_IR_v2 §B) tagged for v2 removal.
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
// CGValue — tagged reference. In production only CG_V_SLOT is
// exercised; CG_V_INST and CG_V_IMMEDIATE are dead arms. The
// SLOT path delegates through CGSlot's source_var fallback.
// Audit (CG_IR_v2 §C) tagged for v2 collapse to a plain Var*.
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
  PNode *source_pn;              // LOAD-BEARING dispatch key — every
                                 //   back-translation calls
                                 //   write_llvm_prim(source_fun, source_pn)
                                 //   (~95% of CGInsts in production).
                                 //   NOT a debugging aid.
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
  Map<Fun *, CGFun *> fun_map;   // load-bearing — translateFunctionBody
                                 //   looks up the CGFun for the current
                                 //   Fun here.
  Map<Sym *, CGSlot *> sym_to_slot;  // populated by cg_normalize, read
                                     //   only by the cg-normalize golden
                                     //   printer. CG_IR_v2 §D removal
                                     //   candidate.
  Map<Sym *, CGType *> sym_to_type;  // same — golden-only.
  IFACallbacks *frontend;        // c_codegen_pre_file et al.

  CGProgram() : main_fun(0), frontend(0) {}
};

// =====================================================================
// The normalization pass — Phase 2 fills this in. Phase 1 stub:
// returns an empty CGProgram (no funs, no globals, no types).
// =====================================================================

CGProgram *cg_normalize(FA *fa);

#endif  // _cg_ir_H_
