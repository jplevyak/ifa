// SPDX-License-Identifier: BSD-3-Clause
//
// Shared helpers between the C backend (`cg.cc`) and the LLVM
// backend (`llvm.cc` + `llvm_codegen.cc` + `llvm_primitives.cc`).
//
// Per CODEGEN_PLAN §5 (phase 2), the duplication that existed
// between the two backends — identical `num_string`,
// `is_closure_var`, `c_type`, and the core of `get_target_fun` —
// lives here. The backends still own their own primitive
// emission switches and printing; only the shared bits move.

#ifndef _codegen_common_H_
#define _codegen_common_H_

#include "fa.h"

// -------------------------------------------------------------
// Type-name strings
// -------------------------------------------------------------

// Map a numeric Sym (IF1_NUM_KIND_INT / UINT / FLOAT) to its
// `_CG_*` runtime type-name string. Identical mapping is used by
// both backends. Fails (via `fail()`) on an unknown kind/index.
cchar *num_string(Sym *s);

// Return v's IF1 type's `cg_string` (the canonical C/LLVM type
// name), or `"_CG_void"` if no type is associated yet.
// Both backends consume this; only the C backend writes type
// names directly, but the LLVM backend uses it for diagnostic
// purposes and via the shared type-string assignment pass.
cchar *c_type(Var *v);
cchar *c_type(Sym *s);

// -------------------------------------------------------------
// Closure / dispatch helpers
// -------------------------------------------------------------

// Is `v` typed as a closure (a Type_FUN with subfields and no
// fully-resolved `fun` pointer)? Used to identify dispatched
// indirect call sites.
int is_closure_var(Var *v);

// Resolve a SEND PNode to its single concrete target Fun, or
// nullptr if the call site has zero or multiple resolutions.
//
// The C backend's variant calls `fail(...)` when no resolution
// is possible (via `fruntime_errors`); the LLVM backend has a
// "search by sym, then by name" fallback over a global function
// list. `get_target_fun_core` is the shared core; the LLVM
// wrapper lives in `llvm_primitives.cc` and adds the fallback.
Fun *get_target_fun_core(PNode *n, Fun *f);

// -------------------------------------------------------------
// Type-string assignment pass (cg_string population)
// -------------------------------------------------------------

// Walk live Funs and assign each one's `cg_string` and
// `cg_structural_string`. `annotate=true` adds `/*name*/` or
// `/*Type::name*/` comment suffixes to the cg_string (the C
// backend's convention; the LLVM backend uses `false` since
// LLVM identifiers can't carry comments). `globals` (if
// non-null) receives each Fun's `sym->var` — the C backend
// collects these to emit later; the LLVM backend passes
// nullptr.
void assign_fun_cg_strings(FA *fa, bool annotate, Vec<Var *> *globals);

// First pass over `allsyms`: assign each Sym a `cg_string`
// (`_CG_int32`, `_CG_psN`, `_CG_void`, etc.). For Type_RECORD
// types with fields, when `fp` is non-null, also emit a forward
// declaration line of the form
//   `/* name */ struct _CG_sN; typedef struct _CG_sN *_CG_psN;\n`
// to `fp` — the C backend uses this; the LLVM backend passes
// nullptr (struct types are built directly via `getLLVMType`).
void assign_type_cg_strings_pass1(Vec<Sym *> &allsyms, FILE *fp);

// Second pass over `allsyms`: resolve `s->fun`-bearing Syms to
// their Fun's `cg_structural_string`, resolve `is_symbol` to the
// builtin `_CG_symbol`, and collapse Type_SUM `T | nil` to `T`.
// No I/O. Identical between the two backends.
void assign_type_cg_strings_pass2(Vec<Sym *> &allsyms);

// -------------------------------------------------------------
// Process invocation
// -------------------------------------------------------------

// Spawn a command via `posix_spawnp` (PATH-search) and wait for it.
// `argv` must be a NULL-terminated array of char* (cast-away const
// is acceptable since the spawn family takes char *const argv[]).
// Returns the child's exit code on normal exit; -1 on spawn or wait
// failure. No shell interpretation — arguments are passed literally.
//
// Replaces direct `system()` calls in `c_codegen_compile` /
// `llvm_codegen_compile`, eliminating shell-quoting concerns for
// filenames with spaces or special characters. See CODEGEN_PLAN §7.3.
int codegen_spawn(const char *file, char *const argv[]);

// -------------------------------------------------------------
// Codegen context — superseded by CG_IR's EmissionContext
// -------------------------------------------------------------
//
// The scaffolding `class Codegen` (CODEGEN_PLAN §5.1) was removed
// in CG_IR_PLAN Phase 0 §5.5. The wholesale "subclass Codegen,
// migrate hundreds of file-scope globals" rewrite was never
// instantiated and never read; the actual state-leak bug was
// already fixed by the phase-0.1 reset hooks. CG_IR_PLAN Phase 3
// introduces an `EmissionContext` struct sized for CGFun emission
// (see CG_IR_PLAN §8 / cg_ir.h) — the natural successor.
//
// See CODEGEN_PLAN.md §8 (revised) and CG_IR_PLAN.md §5.5 for the
// recorded decision.

class PNode;

// -------------------------------------------------------------
// Failure reporting with PNode context (phase 5.3)
// -------------------------------------------------------------

// Structured `fail()` variant that includes PNode source-location
// context when available. Use this in primitive emitters and
// per-PNode helpers so the user sees `<file>:<line>: ` prefix
// pointing at the offending IR site, rather than just the
// emitter's stock error message.
//
// Falls back to plain `fail()` when no source info is reachable.
// Always terminates (no return).
void codegen_fail(PNode *n, cchar *fmt, ...) __attribute__((noreturn, format(printf, 2, 3)));

// Same for Var-level errors (variable's source line if available).
void codegen_fail(Var *v, cchar *fmt, ...) __attribute__((noreturn, format(printf, 2, 3)));

// -------------------------------------------------------------
// Primitive emission contract (phase 2.3 stub)
// -------------------------------------------------------------

// PrimEmitter is the polymorphic seam between the per-primitive
// switches in cg.cc / llvm_primitives.cc. Each backend implements
// the interface; the per-primitive dispatcher (currently still
// an inline `switch` block in each backend) walks the switch.
//
// Phase 5.2 scaffolding: the interface exists and documents the
// contract. The dispatchers still inline-switch by `prim->index`
// for performance and because the switch cases need access to
// backend-specific state (FILE* in C, IR Builder in LLVM). When
// the Codegen base class adds the per-backend state machine, the
// switches can be migrated to virtual methods. See CODEGEN_PLAN
// §8 and PRIMITIVES.md §15 (the primitive emission contract).
//
// Primitive emission contract (recap from PRIMITIVES.md §15):
//
//   - rvals[0] is either the dispatched function symbol or the
//     `__primitive` / `__operator` marker.
//   - For `__primitive`-marked SENDs, rvals[1] is the primitive
//     name; arguments start at rvals[2].
//   - lvals[0], if present, receives the primitive's result. Only
//     emit the lvalue assignment when `lvals[0]->live` is true.
//   - Side effects (struct write, printf, etc.) emit
//     unconditionally; only the lvalue store is gated on live.
//
// See PRIMITIVES.md §14 (Backend coverage matrix) for the
// per-primitive status and pinpoint-fixture map.
class VirtualCGEmitter {
 public:
  virtual ~VirtualCGEmitter() = default;

  // emit_move and emit_send_call handle the two fundamental operations that
  // every backend must implement.  emit_send_default_prim is the fallthrough
  // for any primitive not claimed by a more specific hook — silently dropping
  // it would be wrong, so it stays pure-virtual too.
  virtual void emit_move(PNode *pn) = 0;
  virtual bool emit_send_default_prim(PNode *pn) = 0;
  virtual void emit_send_call(PNode *pn) = 0;

  // Catch-all primitive hook, called before the specialized send methods.
  // A backend that routes all primitives through a single switch (e.g. the
  // C backend's write_c_prim) overrides this and returns true on success.
  // Backends with per-operation LLVM IR builders leave this as false and
  // override the specialized methods below instead.
  virtual bool emit_send_any_prim(PNode *pn) { return false; }

  // Specialized primitive hooks.  Each defaults to false (not handled) so
  // backends only override the operations they care about.  The dispatch in
  // virtual_cg_emit_send calls emit_send_any_prim first; these are only
  // reached when it returns false.
  virtual bool emit_send_unaryop(PNode *pn) { return false; }
  virtual bool emit_send_binop(PNode *pn) { return false; }
  virtual bool emit_send_period(PNode *pn) { return false; }
  virtual bool emit_send_setter(PNode *pn) { return false; }
  virtual bool emit_send_new(PNode *pn) { return false; }
  virtual bool emit_send_clone(PNode *pn) { return false; }
  virtual bool emit_send_len(PNode *pn) { return false; }
  virtual bool emit_send_strcat(PNode *pn) { return false; }
  virtual bool emit_send_is(PNode *pn) { return false; }
  virtual bool emit_send_coerce(PNode *pn) { return false; }
  virtual bool emit_send_make(PNode *pn) { return false; }
  virtual bool emit_send_index_load(PNode *pn) { return false; }
  virtual bool emit_send_index_store(PNode *pn) { return false; }
  virtual bool emit_send_sizeof(PNode *pn) { return false; }
  virtual bool emit_send_primitive(PNode *pn) { return false; }
};

// Returns true if the SEND operation's lvalue was fully constant-folded
// by the IF1 optimizer, meaning we shouldn't emit the computation at runtime.
bool virtual_cg_is_const_folded_send(PNode *pn);

// The unified primitive dispatch loop for SEND operations.
// Maps `pn->prim->index` to the appropriate `emit_send_*` interface hook.
void virtual_cg_emit_send(VirtualCGEmitter *emitter, PNode *pn);

// -------------------------------------------------------------
// Side-channel accessors (CG_IR_PLAN Phase 0 §5.4)
// -------------------------------------------------------------
//
// The codegen-time fields on Sym/Var/Fun (`cg_string`,
// `llvm_value`, `llvm_type`, etc.) are deleted in CG_IR_PLAN
// Phase 5 in favor of CGFun / CGProgram-owned tables. To make
// that deletion one mechanical PR, the readers and writers go
// through these accessors now — there are ~270 of them across
// cg.cc, llvm.cc, llvm_codegen.cc, llvm_primitives.cc, and
// codegen_common.cc.
//
// Phase 5 then:
//   1. Replaces each accessor body with a lookup into the
//      CGProgram-owned side table (or removes the accessor if
//      the field is no longer needed).
//   2. Deletes the field declarations on Sym/Var/Fun.
// No call site outside this header changes.

#include "fun.h"
#include "sym.h"
#include "var.h"

static inline cchar *cg_get_string(Sym *s) { return s->cg_string; }
static inline void   cg_set_string(Sym *s, cchar *v) { s->cg_string = v; }
static inline cchar *cg_get_string(Var *v) { return v->cg_string; }
static inline void   cg_set_string(Var *v, cchar *s) { v->cg_string = s; }
static inline char  *cg_get_string(Fun *f) { return f->cg_string; }
static inline void   cg_set_string(Fun *f, char *s) { f->cg_string = s; }

static inline char  *cg_get_structural_string(Fun *f) { return f->cg_structural_string; }
static inline void   cg_set_structural_string(Fun *f, char *s) { f->cg_structural_string = s; }

static inline llvm::Value *cg_get_llvm_value(Sym *s) { return s->llvm_value; }
static inline void         cg_set_llvm_value(Sym *s, llvm::Value *v) { s->llvm_value = v; }
static inline llvm::Value *cg_get_llvm_value(Var *v) { return v->llvm_value; }
static inline void         cg_set_llvm_value(Var *v, llvm::Value *val) { v->llvm_value = val; }

static inline llvm::Type *cg_get_llvm_type(Sym *s) { return s->llvm_type; }
static inline void        cg_set_llvm_type(Sym *s, llvm::Type *t) { s->llvm_type = t; }
static inline llvm::Type *cg_get_llvm_type(Var *v) { return v->llvm_type; }
static inline void        cg_set_llvm_type(Var *v, llvm::Type *t) { v->llvm_type = t; }

static inline llvm::DILocalVariable *cg_get_llvm_debug_var(Var *v) { return v->llvm_debug_var; }
static inline void                   cg_set_llvm_debug_var(Var *v, llvm::DILocalVariable *d) { v->llvm_debug_var = d; }

static inline llvm::Function *cg_get_llvm(Fun *f) { return f->llvm; }
static inline void            cg_set_llvm(Fun *f, llvm::Function *fn) { f->llvm = fn; }

#endif  // _codegen_common_H_
