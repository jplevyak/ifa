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
// Primitive emission contract (phase 2.3 stub)
// -------------------------------------------------------------

// PrimEmitter is the (currently-documentation-only) seam between
// the per-primitive switches in cg.cc / llvm_primitives.cc and a
// future unified dispatch (CODEGEN_PLAN phase 5). The interface
// codifies what each backend must implement for each primitive,
// even though the actual dispatch is still inline switches today.
//
// Document each primitive's rvals/lvals contract in PRIMITIVES.md
// before implementing a new emitter. The convention:
//
//   - rvals[0] is either the dispatched function symbol or the
//     `__primitive` marker.
//   - For `__primitive`-marked SENDs, rvals[1] is the primitive
//     name; arguments start at rvals[2].
//   - lvals[0], if present, receives the primitive's result. Only
//     emit the lvalue assignment when `lvals[0]->live` is true.
//
// See PRIMITIVES.md §14 (Backend coverage matrix) for the
// per-primitive status and pinpoint-fixture map.
//
// (The actual dispatch table lands in phase 5; this header
// reserves the namespace.)

#endif  // _codegen_common_H_
