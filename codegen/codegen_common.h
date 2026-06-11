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
// Codegen context — per-codegen-run state (phase 5.1 scaffolding)
// -------------------------------------------------------------

class PNode;

// `Codegen` is the per-run state owner for a single
// `c_codegen_print_c` / `llvm_codegen_print_ir` invocation.
// Instantiating it (typically as a local in the print driver)
// establishes a known lifetime for backend state — no more
// file-scope-state leaking across compilations.
//
// The base class holds backend-agnostic fields: the FA the run is
// emitting for, the entry Fun, and the source filename. Each
// backend subclasses this to add its own state (the C backend
// adds a FILE*; the LLVM backend adds Context/Module/Builder etc.).
//
// Phase 5 scaffolding: the type exists and the scaffold for
// instantiation lives in `llvm_codegen_print_ir`. Wholesale
// migration of every reference to file-scope globals into
// `codegen->TheContext` / `codegen->Builder` style accesses is
// deferred — there are hundreds of access sites across three
// files. The destructor invariants and reset hooks are already
// in place via the phase-0.1 state-leak fix; the Codegen type
// codifies the lifetime contract so a future migration is
// strictly mechanical. See CODEGEN_PLAN §8 and AUDIT §4.
class Codegen {
 public:
  FA *fa;
  Fun *entry_fun;
  cchar *input_filename;

  Codegen(FA *fa_, Fun *entry_fun_, cchar *input_filename_)
      : fa(fa_), entry_fun(entry_fun_), input_filename(input_filename_) {}
  virtual ~Codegen() = default;

  // Non-copyable; codegen state is unique per run.
  Codegen(const Codegen &) = delete;
  Codegen &operator=(const Codegen &) = delete;
};

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
class PrimEmitter {
 public:
  virtual ~PrimEmitter() = default;

  // Emit code for a single SEND PNode whose `prim` field is
  // already resolved. Returns 1 if the primitive was handled,
  // 0 if it should fall through to the generic call path.
  // Backends override this with their dispatch (typically a
  // `switch (n->prim->index)`).
  virtual int emit(Fun *ifa_fun, PNode *n) = 0;
};

#endif  // _codegen_common_H_
