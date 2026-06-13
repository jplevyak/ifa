// SPDX-License-Identifier: BSD-3-Clause
//
// CG_IR_SKETCH.h — Phase 2 of CG_IR_META_PLAN.
//
// HEADER-ONLY DESIGN SKETCH. No implementation. No production
// use. This file exists to make the 8-concept "must expose"
// list from Phase 1 (CG_IR_NEEDS.md) concrete enough to
// review, criticize, and walk through with example programs.
//
// What this is NOT:
// - A replacement for cg_ir.h. The current CG_IR shipped Phase
//   3.4 (production swap, 38/37 LLVM-suite). This sketch
//   describes what cg_ir.h would look like AFTER a v2
//   migration that addresses the audit findings.
// - A drop-in. The actual v2 work (Phase 4 of the meta-plan)
//   ports cg_normalize and emit_cg one synthetic test at a
//   time. This sketch is the target shape.
//
// What this IS:
// - A reviewable strawman. The project owner reads this,
//   says "yes/no/change X," and the resulting agreement is
//   the design contract.
// - Documentation of the 5 open-question decisions with
//   rationale (see "Decisions on open questions" below).
// - Five walkthroughs at the end showing how concrete
//   programs lower.
//
// See:
// - ifa/codegen/CG_IR_META_PLAN.md — the meta-plan this is
//   Phase 2 of
// - ifa/codegen/CG_IR_SURVEY.md — Phase 0 field classification
// - ifa/codegen/CG_IR_NEEDS.md — Phase 1 backend reads + the
//   8-concept distillation
// - ifa/codegen/CG_IR_v2.md — the audit that motivated v2
//
// ============================================================
//   DECISIONS ON OPEN QUESTIONS
// ============================================================
//
// Phase 1 surfaced 5 open questions. Reasoned defaults below;
// each is reviewable independently.
//
// Q1: Where do per-(CGFun, CGValue) backend caches live?
// A1: ON CGFun, as a map. Rationale:
//     - Function-scoped by construction; the cache LIFETIME
//       matches the CGFun's emission session.
//     - Closes issue 017's class structurally: no
//       program-scoped Var-cache to leak across functions.
//     - Performance is acceptable: map lookup happens N times
//       per emission; N is small (one function at a time).
//     - Alternative considered: per-CGValue with nullable
//       CGFun* owner. Rejected because it makes CGValue
//       larger (heavier hot data structure) for no
//       structural win.
//
// Q2: Do program-scope CGValues exist (globals, constants)?
// A2: YES. CGProgram::globals and CGProgram::types are
//     program-scope. CGFun::locals + CGFun::formals are
//     function-scope. CGValue::scope discriminates. This is
//     required: globals are real things and must outlive any
//     single function emission.
//
// Q3: Phi MOVEs — explicit records or implicit insts?
// A3: EXPLICIT records on CGBlock, per predecessor. Rationale:
//     - Makes the IR self-describing without needing source_pn
//       back-references.
//     - Required for textual form: a CG_IR program in text
//       must express phi without referencing IF1.
//     - The current emit_terminator's "walk source_pn->phi"
//       was a Phase 3.4 expedient, not a long-term design.
//     - Per-pred is the natural unit: each edge has its own
//       Move sequence.
//
// Q4: Should CGType expose is_heap_aggregate?
// A4: YES, first-class. Rationale:
//     - Issue 015 is open and waiting for this.
//     - The flag exists on Sym::is_value_type; promoting it
//       to CGType makes the existing info accessible to the
//       emitter without reaching back through Sym.
//     - Trivial to compute at cg_normalize time.
//
// Q5: Source location granularity?
// A5: PER-CGInst. Rationale:
//     - LLVM precedent. Per-instruction !dbg metadata is the
//       norm.
//     - Finer granularity = better debug info for breakpoints
//       inside a single statement.
//     - The CGSourceLoc struct is tiny (3 ints); cost is
//       negligible.
//
// Any of these can be revised; the sketch's structure
// reflects them.

#ifndef _cg_ir_sketch_H_
#define _cg_ir_sketch_H_

#include "fa.h"
// NOTE on includes: we keep Prim from IF1 (the deliberate
// escape hatch, Phase 1 §7) and that drags in Sym/Var/PNode
// references. v2 minimizes those to "back-pointers for
// debugging / dispatching to the existing per-prim emitter"
// rather than load-bearing reads.

namespace llvm { class Value; class Function; class Type;
                 class BasicBlock; }

class CGType;
class CGValue;
class CGInst;
class CGBlock;
class CGFun;
class CGProgram;
class CGMove;
class CGLabel;

// ============================================================
//   1. TYPES                              (Phase 1 §1)
// ============================================================
//
// CGType is the language-agnostic type model. Maps 1:1 to
// Phase 0's survival set of Sym fields. Critically, CGType
// does NOT carry per-emission caches — those live on CGFun's
// type-resolution map. CGType is shared across all CGFuns in
// a CGProgram.

enum CGTypeKind {
  CG_T_VOID,
  CG_T_INT,           // bits = 1/8/16/32/64
  CG_T_UINT,          // bits = 1/8/16/32/64
  CG_T_FLOAT,         // bits = 32/64/128
  CG_T_BOOL,          // bits = 1
  CG_T_PTR,           // opaque pointer (heap aggregate, ref)
  CG_T_STRUCT,        // value-typed record (rare in pyc; common in V POD)
  CG_T_FUN_PTR,       // pointer to a function; signature in fields[]
  CG_T_REF,           // pointer with explicit element type
  CG_T_SUM,           // tagged union (T | nil etc)
  CG_T_SYMBOL,        // pyc symbol-literal type
};

class CGField {
 public:
  CGType *type;
  cchar *name;                 // optional field name
  int idx;                     // explicit ordinal — fixes per-prim
                               //   GEP indexing (current code does
                               //   atoi("e%d") at emit time; pre-
                               //   resolve here)
};

class CGFunSig {
 public:
  CGType *ret;
  Vec<CGType *> args;
  bool is_varargs;
};

class CGType : public gc {
 public:
  int id;                      // stable identity for diagnostics
  cchar *name;                 // emission name ("range", "int64")

  CGTypeKind kind;
  int bits;                    // numeric width for INT/UINT/FLOAT/BOOL

  // STRUCT/REF/SUM/FUN_PTR field model. For RECORD-like types,
  // fields are ordered + named. The order is THE source of truth
  // for struct GEP — no longer derived from Sym->has at emit
  // time. This closes the field-index resolution bugs we hit
  // during issue 016 investigation.
  Vec<CGField> fields;

  // For REF / vector: element type. For SUM with `T | nil`
  // pattern, the non-nil alternative.
  CGType *element;

  // For FUN_PTR: the signature. NOT in `fields` — signatures
  // and structural records are different.
  CGFunSig *fun_sig;

  // Alias chain for type-equivalence. Resolved at IR-build
  // time, not at emission time. Code reading `unaliased()`
  // gets the resolved tip.
  CGType *alias_of;

  // is_heap_aggregate (Phase 1 §4 decision Q4). When true,
  // variables of this type are stored as pointers to the
  // type's struct. This is the LLVM-side `getLLVMVarType`
  // distinction made explicit. Issue 015's POD-record work
  // flips this to false for `@pyc_struct`-decorated types.
  bool is_heap_aggregate;

  // Back-reference to the IF1 Sym this came from. ESCAPE HATCH.
  // Used by the per-prim emitters via source_sym; NOT by CG_IR
  // emission semantics. Removable if/when frontends stop
  // producing IF1.
  Sym *source_sym;

  CGType *unaliased() const;   // walks alias_of to the tip
};

// ============================================================
//   2. VALUES                             (Phase 1 §2)
// ============================================================
//
// CGValue is the flat namespace replacement for Sym + Var +
// AVar at emission time. Each CGValue has:
// - identity (id, optional name)
// - type
// - scope (LOCAL/FORMAL/GLOBAL/CONSTANT/FUN_REF/SYMBOL)
// - single defining inst (post-SSA)
// - optional constant payload
//
// CGValue does NOT carry the per-emission backend cache. That
// lives on CGFun (Q1 decision). This is the structural fix for
// issue 017: value identity has function scope baked in.

enum CGValueScope {
  CGV_LOCAL,                   // alloca / SSA in some CGFun
  CGV_FORMAL,                  // function parameter
  CGV_GLOBAL,                  // program-scope variable
  CGV_CONSTANT,                // compile-time constant payload
  CGV_FUN_REF,                 // function-pointer constant (the function itself)
  CGV_SYMBOL,                  // pyc symbol-literal
};

class CGImmediate {
 public:
  // Tagged union for compile-time constants. Mirrors IF1's
  // `Immediate` but explicit about what each variant carries.
  // The textual form (Phase 3) will serialize these as
  // (int 42) / (float 3.14) / (str "hi") / (sym foo) etc.
  enum Kind { I_NONE, I_INT, I_UINT, I_FLOAT, I_STR, I_SYM,
              I_NIL, I_BOOL };
  Kind kind;
  union {
    int64_t i;
    uint64_t u;
    double f;
    cchar *str;
    int sym_id;
    bool b;
  } v;
};

class CGValue : public gc {
 public:
  int id;                      // stable; for textual form + debug
  cchar *name;                 // explicit, replaces cg_string side-channel.
                               //   Required for textual round-trip.
                               //   For pyc: derives from Sym->name or
                               //   "t%d" for temps.
  CGType *type;
  CGValueScope scope;

  // The PNode/CGInst that defines this value. For LOCAL/FORMAL/
  // computed values, this is non-null. For GLOBAL, the
  // "definition" is the program initializer; def may be null.
  // For CONSTANT and FUN_REF, def is null.
  //
  // Each value has EXACTLY ONE def (post-SSA invariant). Phase 2
  // build-time invariant; checked at IR validation.
  CGInst *def;

  // If scope == CGV_LOCAL or CGV_FORMAL, the owning CGFun. For
  // CGV_GLOBAL / CGV_CONSTANT / CGV_FUN_REF / CGV_SYMBOL, null
  // (program-scope).
  CGFun *defined_in;

  // For CGV_CONSTANT: the payload.
  // For CGV_SYMBOL: name is the symbol name.
  // For CGV_FUN_REF: target_fun points to the referenced CGFun.
  CGImmediate imm;
  CGFun *target_fun;           // for CGV_FUN_REF

  // Emit-time gates. The IR captures the FA-determined live
  // bit (Var::live || Var::is_formal etc). The backend
  // ignores values where this is false unless they're phi/phy
  // MOVE participants.
  bool live;

  // Internal/fake hint — backend can skip emission of temps
  // that the analyzer optimized out.
  bool is_internal;

  // Back-reference to the IF1 Var. ESCAPE HATCH for diagnostic /
  // dispatch to existing per-prim emitters via source_var.
  Var *source_var;
};

// ============================================================
//   3. OPERATIONS                         (Phase 1 §3)
// ============================================================
//
// CGInst is a single operation in a CGBlock's body. The op set
// is small (~10 cases) and language-agnostic. SEND with a
// `prim` hint is the dispatch seam to the per-prim emitters;
// structural ops like CG_GEP / CG_ALLOC are first-class for
// the common cases.

enum CGOp {
  CG_NOP,                      // skip; informational

  // Memory / value flow
  CG_MOVE,                     // SSA-style rename: lvals[0] := rvals[0]
  CG_LOAD,                     // result := load lvals[0] from rvals[0] (slot)
  CG_STORE,                    // store rvals[0] into rvals[1]'s slot
  CG_ALLOC,                    // result := alloc of type (GC heap for is_heap_aggregate)

  // Field access (struct GEP)
  CG_FIELD_LOAD,               // result := rvals[0]->fields[field_idx]
  CG_FIELD_STORE,              // rvals[0]->fields[field_idx] := rvals[1]

  // Index access (vector / array)
  CG_INDEX_LOAD,               // result := rvals[0][rvals[1]]
  CG_INDEX_STORE,              // rvals[0][rvals[1]] := rvals[2]

  // Calls and primitives
  CG_CALL,                     // result := call rvals[0] (a CGV_FUN_REF) with rvals[1..]
  CG_PRIM,                     // primitive dispatched via `prim` hint;
                               //   backend uses write_llvm_prim / write_c_prim

  // Arithmetic / comparison (specialized via sub-kind for LLVM,
  // emit-as-text for C with operator-overload _CG_int64 etc).
  CG_BINOP,                    // op2 in [add, sub, mul, div, mod, lt, le, gt, ge,
                               //   eq, ne, and, or, xor, shl, shr]
  CG_UNOP,                     // op2 in [neg, not, bitnot]
  CG_CAST,                     // typecast

  // Terminators (one per block; in CGBlock::terminator)
  CG_BR,                       // unconditional branch
  CG_COND_BR,                  // conditional: rvals[0] ? succs[0] : succs[1]
  CG_RET,                      // return rvals[0] (or void)
  CG_UNREACHABLE,              // verifyModule filler
};

// Sub-kind for CG_BINOP / CG_UNOP / CG_CAST.
enum CGOp2 {
  CG2_NONE,
  CG2_ADD, CG2_SUB, CG2_MUL, CG2_DIV, CG2_MOD,
  CG2_LT,  CG2_LE,  CG2_GT,  CG2_GE,  CG2_EQ, CG2_NE,
  CG2_AND, CG2_OR,  CG2_XOR, CG2_SHL, CG2_SHR,
  CG2_NEG, CG2_NOT, CG2_BITNOT,
  CG2_TRUNC, CG2_ZEXT, CG2_SEXT, CG2_BITCAST,
};

struct CGSourceLoc {
  unsigned line;
  unsigned column;             // optional, 0 = unknown
  cchar *file;                 // canonicalized path; nullable
};

class CGInst : public gc {
 public:
  int id;                      // stable identity for textual form + diagnostics
  CGOp op;
  CGOp2 op2;                   // sub-kind for BINOP/UNOP/CAST; CG2_NONE otherwise

  CGType *result_type;         // for value-producing ops; null for terminators / stores

  Vec<CGValue *> rvals;        // inputs (reads)
  Vec<CGValue *> lvals;        // outputs (writes); usually 0 or 1

  // For FIELD_LOAD/FIELD_STORE: pre-resolved index. Fixes the
  // current per-prim emitter's atoi("e%d") at emit time.
  int field_idx;

  // For CG_PRIM: dispatch hint. The backend's write_llvm_prim
  // / write_c_prim handles per-prim emission via Prim::index.
  // Escape hatch but contractual (Phase 1 §7).
  Prim *prim;

  // For terminators
  CGBlock *br_target;          // CG_BR
  CGBlock *br_true;            // CG_COND_BR
  CGBlock *br_false;           // CG_COND_BR

  // Gate (Phase 0: live && fa_live collapsed)
  bool live;

  CGSourceLoc src_loc;

  // Back-reference to the IF1 PNode. ESCAPE HATCH for
  // dispatching to existing per-prim emitters (write_llvm_prim
  // takes PNode*). Removable when per-prim emitters are
  // ported to consume CGInst directly.
  PNode *source_pn;
};

// CGMove is a phi-MOVE record. Used per CGBlock-predecessor
// edge. Lifted from the current emit_phi_phy's per-source_pn
// walk to an explicit data structure (Q3 decision).
class CGMove : public gc {
 public:
  CGValue *lhs;                // value being defined (in successor block)
  CGValue *rhs;                // value being read (in predecessor block)
  CGSourceLoc src_loc;
};

// ============================================================
//   4. BLOCKS                             (Phase 1 §4)
// ============================================================

class CGLabel : public gc {
 public:
  int id;                      // for "L%d" emission and textual form
  cchar *name;                 // optional human-readable
};

class CGBlock : public gc {
 public:
  int id;
  CGLabel *label;              // null for the synthetic entry block

  Vec<CGInst *> body;          // non-terminator insts in order
  CGInst *terminator;          // exactly one; CG_BR/COND_BR/RET/UNREACHABLE

  Vec<CGBlock *> preds;
  Vec<CGBlock *> succs;

  // Phi MOVEs from each predecessor edge (Q3 decision).
  // map[pred-block] = ordered list of CGMoves to execute
  // in the predecessor's block before branching to THIS
  // block.
  //
  // The emitter places these in the predecessor's terminator
  // emission (mirroring LLVM's phi-on-edge semantics via the
  // alloca-and-store convention).
  Map<CGBlock *, Vec<CGMove *>> phi_by_pred;
};

// ============================================================
//   5. FUNCTIONS                          (Phase 1 §5)
// ============================================================
//
// CGFun has the load-bearing addition v2 owes: a per-function
// value cache. This is THE structural fix for issue 017's class.
// Value identity is function-scoped by construction; the LLVM
// backend's llvm::Value identity scope (function-scoped) now
// matches the IR's value identity scope.

class CGFun : public gc {
 public:
  int id;
  cchar *name;
  CGFunSig *signature;

  CGBlock *entry;
  Vec<CGBlock *> blocks;       // in CFG order; entry first

  Vec<CGValue *> formals;      // by ordinal position; type matches signature
  Vec<CGValue *> locals;       // declared locals — backend allocates storage

  // Flags
  bool live;
  bool is_external;
  bool is_varargs;
  bool is_main;                // pyc's __main__ entry

  // === The structural issue-017 fix ===
  //
  // Per-CGFun value cache. The backend resolves
  // (CGValue -> llvm::Value*) through this map, NOT through
  // a global cache on CGValue. Two consequences:
  //
  // 1. Cross-function leak is structurally impossible. Each
  //    CGFun emission session has its own cache; values from
  //    a previous CGFun's emission don't reach this one.
  //
  // 2. The textual form can serialize values without
  //    backend-specific contamination. A CGValue in text is
  //    just an id + name + type; the backend cache is built
  //    fresh each emission.
  //
  // The map is built by the LLVM emitter during emit_cgfun;
  // populated by inst emission (e.g., CG_ALLOC emits a call
  // to GC_malloc and records the result here); read by
  // downstream inst emission (e.g., CG_CALL reading args).
  //
  // The C backend's analogous "per-function name table" lives
  // here too — populated with "t%d" strings during the
  // function's emission session, discarded at end.
  void *backend_cache;         // opaque per-backend (void*).
                               //   Phase 4 implementations:
                               //   LLVM = Map<CGValue*, llvm::Value*>
                               //   C    = Map<CGValue*, cchar *>

  // Back-ref. ESCAPE HATCH.
  Fun *source_fun;
};

// ============================================================
//   6. PROGRAM                            (Phase 1 §6)
// ============================================================

class CGProgram : public gc {
 public:
  Vec<CGType *> types;         // program-scope; shared across funs

  Vec<CGValue *> globals;      // module-level variables
  Vec<CGValue *> constants;    // interned constants (numbers, strings, syms)

  Vec<CGFun *> funs;
  CGFun *main_fun;             // pyc's __main__

  // For inter-function call resolution: maps a CGV_FUN_REF
  // value's target_fun to the corresponding CGFun.
  Map<Fun *, CGFun *> fun_map;
};

// ============================================================
//   7. PRIMITIVES — escape hatch (deliberate)
// ============================================================
//
// CG_IR does NOT redefine primitives. The 30 P_prim_* cases
// in IF1 are stable and the per-prim emitters in
// llvm_primitives.cc / cg.cc are stable. CG_IR exposes them
// via CG_PRIM ops with a Prim* hint; the backend's existing
// dispatch handles emission.
//
// This is the deliberate escape hatch (Phase 1 §7). When
// frontends without IF1 emerge, the Prim* model needs to be
// replaced; until then, this is the right boundary.

// ============================================================
//   8. SOURCE LOCATIONS                   (Phase 1 §8)
// ============================================================
//
// CGSourceLoc is defined above (in §3 near CGInst). Per-CGInst
// granularity, 3 int fields, trivial cost.

// ============================================================
//   API ENTRY POINTS
// ============================================================
//
// What does cg_normalize_v2(FA*) return? What does emit_cgfun
// accept?

// Build a CGProgram from the post-FA / post-clone / post-DCE /
// post-SSU IF1 state. Idempotent; safe to call multiple times.
CGProgram *cg_normalize_v2(FA *fa);

// LLVM emission. Reads from CGProgram, writes IR. Internally
// builds a per-CGFun backend cache (set into CGFun::backend_cache
// during emission, discarded after).
void emit_llvm_module_v2(CGProgram *prog);

// C emission. Same shape.
void emit_c_module_v2(FILE *fp, CGProgram *prog);

// Validation. Checks IR invariants (SSA: every CGValue has
// exactly one def except program-scope; types match;
// terminators are well-formed; phi MOVEs reference valid
// blocks).
void cg_validate(CGProgram *prog);

// Textual form (Phase 3 of meta-plan; placeholder declarations).
void cg_print_text(FILE *fp, CGProgram *prog);
CGProgram *cg_parse_text(FILE *fp);

#endif // _cg_ir_sketch_H_

// ============================================================
//   WALKTHROUGHS — 5 program shapes lowering to this IR
// ============================================================
//
// Each walkthrough shows:
// (a) the source program (Python-ish)
// (b) the corresponding CG_IR in pseudo-textual form (Phase 3
//     will formalize the syntax)
// (c) commentary on what each element represents
//
// The textual form here uses S-expressions; actual Phase 3
// syntax may differ.
//
// ──────────────────────────────────────────────────────────
// Walkthrough 1: empty void fn
// ──────────────────────────────────────────────────────────
//
//   def hi(): pass
//
// CG_IR:
//
//   (fun hi
//     :signature (void)
//     :entry %b0
//     (block %b0
//       :term (ret)))
//
// Concepts exercised: CGFun with empty body, single block, RET
// terminator with no return value. Trivial baseline.
//
// ──────────────────────────────────────────────────────────
// Walkthrough 2: fn returning a constant
// ──────────────────────────────────────────────────────────
//
//   def answer(): return 42
//
// CG_IR:
//
//   (const %c0 (int 42) :type int64)
//
//   (fun answer
//     :signature (int64)
//     :entry %b0
//     (block %b0
//       :term (ret %c0)))
//
// Concepts: CGValue with CGV_CONSTANT scope, RET with a value.
// The constant lives at CGProgram::constants (program-scope).
//
// ──────────────────────────────────────────────────────────
// Walkthrough 3: fn returning its arg
// ──────────────────────────────────────────────────────────
//
//   def id(x): return x
//
// CG_IR:
//
//   (fun id
//     :signature (int64 int64)
//     :entry %b0
//     :formals (%x)
//     (value %x :type int64 :scope formal)
//     (block %b0
//       :term (ret %x)))
//
// Concepts: CGValue with CGV_FORMAL scope; def-use chain
// (RET reads %x).
//
// ──────────────────────────────────────────────────────────
// Walkthrough 4: two-block fn with branch
// ──────────────────────────────────────────────────────────
//
//   def sign(x):
//     if x < 0:
//       return -1
//     return 1
//
// CG_IR:
//
//   (const %neg1 (int -1) :type int64)
//   (const %one  (int 1)  :type int64)
//   (const %zero (int 0)  :type int64)
//
//   (fun sign
//     :signature (int64 int64)
//     :entry %b0
//     :formals (%x)
//     (value %x :type int64 :scope formal)
//
//     (value %t0 :type bool :scope local :def %i0)
//
//     (block %b0
//       :label entry
//       (inst %i0 (binop lt %x %zero) => %t0)
//       :term (cond_br %t0 %b1 %b2))
//
//     (block %b1
//       :label if_true
//       :preds (%b0)
//       :term (ret %neg1))
//
//     (block %b2
//       :label if_false
//       :preds (%b0)
//       :term (ret %one)))
//
// Concepts: CG_BINOP with sub-kind CG2_LT; conditional branch;
// multiple blocks with preds; values flowing through the IR
// across blocks (NOTE: %x is read in %b0 only; no phi needed).
//
// ──────────────────────────────────────────────────────────
// Walkthrough 5: fn with struct alloc + field write + field read
// ──────────────────────────────────────────────────────────
//
//   class Point:
//     x = 0
//     y = 0
//   p = Point()
//   p.x = 3
//   print(p.x)
//
// CG_IR (showing the main fn only):
//
//   (type %Point :kind struct :is_heap_aggregate true
//     :fields ((%x int64) (%y int64)))
//
//   (const %c3   (int 3) :type int64)
//
//   (global %p :type ptr-to-Point :scope global)
//
//   (fun __main__
//     :signature (void)
//     :entry %b0
//
//     (value %tmp0 :type ptr-to-Point :scope local :def %i0)
//     (value %tmp1 :type int64       :scope local :def %i2)
//
//     (block %b0
//       :label entry
//       (inst %i0 (alloc :type Point) => %tmp0)
//       (inst %i1 (move %tmp0 %p))                    ; binds to global
//       (inst %i2 (field_store %p :field_idx 0 %c3))  ; p.x = 3
//       (inst %i3 (field_load  %p :field_idx 0) => %tmp1) ; tmp1 = p.x
//       (inst %i4 (prim print %tmp1))                 ; print(tmp1)
//       :term (ret)))
//
// Concepts: CG_ALLOC with type info; CG_MOVE binding malloc
// result to global (THE issue-014 fix made explicit); CG_FIELD_STORE
// and CG_FIELD_LOAD with pre-resolved field_idx (fixes
// resolve-at-emit-time bugs from current code); CG_PRIM for the
// print() call (dispatched to existing per-prim emitter via
// `prim` hint).
//
// ──────────────────────────────────────────────────────────
// Walkthrough discussion
// ──────────────────────────────────────────────────────────
//
// What the walkthroughs validate:
//
// 1. The 8 concepts are sufficient. Every operation in the 5
//    programs maps to one of CG_MOVE / CG_BINOP / CG_FIELD_*
//    / CG_ALLOC / CG_PRIM / CG_BR / CG_RET. No mystery
//    construct needed.
//
// 2. Phi MOVEs don't show up in these examples — they appear
//    only in programs with loops or convergent branches with
//    differing definitions. Walkthrough 4 has a divergent
//    branch but no convergence point (both branches return).
//    A 6th walkthrough (loop) would exercise CGMove on
//    CGBlock::phi_by_pred; deferred to Phase 3's larger test
//    corpus.
//
// 3. Issue 017 doesn't appear because the per-CGFun cache is
//    implicit: each CGFun owns its value-resolution scope. A
//    "construction-flow" malloc result naturally lives in the
//    function that emitted it; no global Var-cache to leak.
//
// 4. The escape hatches (source_pn, source_var, source_sym,
//    Prim*) are used only by per-prim back-translation in
//    CG_PRIM. The core IR doesn't reach into IF1 anywhere.
//
// 5. The textual form (Phase 3) is feasible: each construct
//    above has a clear S-expression shape, parseable
//    round-trip.
//
// What the walkthroughs DON'T validate:
//
// - Loops with phi/phy MOVEs.
// - Recursive function calls.
// - Generic dispatch (clone-and-specialize).
// - Closure capture.
//
// These belong in Phase 3's full corpus (10-15 hand-written
// programs of escalating complexity).
//
// ──────────────────────────────────────────────────────────
// What stays as IF1 (escape hatches summary)
// ──────────────────────────────────────────────────────────
//
// - Prim*: kept verbatim. CG_PRIM dispatches via prim->index.
// - source_pn on CGInst: back-translation seam for the existing
//   per-prim emitters. Production code keeps using
//   write_llvm_prim(source_fun, source_pn).
// - source_var on CGValue: diagnostic.
// - source_sym on CGType: diagnostic.
// - source_fun on CGFun: diagnostic + IF1 sym lookup for the
//   per-prim emitters that read it.
//
// These are CONTRACTUAL escape hatches. They're documented as
// "intentionally retained" (constraint #2 from the meta-plan's
// accepted constraints).
//
// ──────────────────────────────────────────────────────────
// What goes away from v1
// ──────────────────────────────────────────────────────────
//
// - CGSlot (v1): wrapped Var; production fell through to
//   Var::llvm_value anyway. v2 uses CGValue directly with
//   scope discriminator + CGFun-scoped backend cache.
// - CGType (v1): had decorative kind/bits/fields. v2 keeps
//   the same shape but makes the source_sym escape hatch
//   first-class (v1 also had this; v2 documents it).
// - CGValue (v1): 5-way union with 4 dead arms. v2's CGValue
//   has scope discriminator + actual payloads, no dead arms.
// - CGInst::source_pn (v1) "for debugging": v2 documents it
//   as LOAD-BEARING dispatch key (matches reality).
// - CGProgram::sym_to_slot / sym_to_type (v1): unused except
//   by the cg-normalize printer. v2 drops them; the printer
//   iterates types/globals/funs directly.
// - CGInst::is_phi_phy (v1): workaround bit for Phase 2.4's
//   in-body STOREs. v2 doesn't emit phi MOVEs as in-body
//   STOREs; they live on CGBlock::phi_by_pred per Q3.
// - cg_to_llvm_type (Phase 3.1, v1 parallel function): v2
//   integrates type resolution into emit_llvm_module's
//   per-CGFun setup; no parallel function.
// - create_llvm_function_from_cgfun (Phase 3.2, v1 parallel):
//   same — integrated into emit_llvm_module.
//
// ──────────────────────────────────────────────────────────
// Open Q's for Phase 5 (semantics doc)
// ──────────────────────────────────────────────────────────
//
// Even after the 5 open-question decisions above, three
// subsidiary questions remain that the semantics doc will
// formalize:
//
// SQ1: How are uninitialized CGValues represented? (LLVM has
//      `undef`; C has uninitialized declarations.) Probably
//      via a CGV_UNDEF kind or a flag on existing kinds.
//
// SQ2: How do we represent the SUM type's tag discriminator?
//      Currently `T | nil` collapses to T*-or-null; richer
//      SUM types need explicit tag fields. Issue 015 territory.
//
// SQ3: How does the IR represent closures? V's closures are
//      structs with captured values; pyc's are similar. CGType
//      with Type_FUN + has[] models this; the per-prim
//      P_prim_make path constructs them. A clean v2 might
//      promote closure construction to a first-class CG_OP.
//
// These don't block Phase 3 or 4 — the existing escape hatches
// handle them. They surface for Phase 5's "this is the
// contract" formal write-up.
