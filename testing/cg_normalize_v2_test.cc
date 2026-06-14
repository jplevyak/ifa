// SPDX-License-Identifier: BSD-3-Clause
//
// cg_normalize_v2_test.cc — Phase B unit test.
//
// Mirrors cg_normalize_test.cc but for the v2 entry point.
// Each Phase B landing extends this file with a test that
// verifies the new IF1 → CGv2 translation.

#include "ifadefs.h"

#include "ifa.h"
#include "pdb.h"
#include "unit.h"
#include "codegen/cg_ir_v2.h"
#include "codegen/cg_normalize_v2.h"
#include "testing/test_callbacks.h"

#include <stdio.h>

// Phase B.1 — stub contract.
//
// cg_normalize_v2() called against a freshly-initialized FA
// (no user input) returns a non-null, empty CGv2Program. Just
// the predefined types — no funs, no globals, no constants.
int run_cg_normalize_v2_stub() {
  ifa_reset();
  ifa_init(new IRCallbacks);

  CGv2Program *p = cg_normalize_v2(pdb->fa);
  if (!p) {
    printf("  cg_normalize_v2 returned NULL\n");
    return 1;
  }
  if (p->funs.n != 0) {
    printf("  expected 0 funs, got %d\n", p->funs.n);
    return 1;
  }
  if (p->main_fun != 0) {
    printf("  expected null main_fun, got non-null\n");
    return 1;
  }
  if (p->globals.n != 0) {
    printf("  expected 0 globals, got %d\n", p->globals.n);
    return 1;
  }
  if (p->constants.n != 0) {
    printf("  expected 0 constants, got %d\n", p->constants.n);
    return 1;
  }
  if (p->types.n != 0) {
    printf("  expected 0 user types, got %d\n", p->types.n);
    return 1;
  }
  // Predefined types should be wired up — at minimum int64
  // and void must resolve via lookup_type.
  if (!p->lookup_type("int64") || !p->lookup_type("void")) {
    printf("  predefined types not wired up\n");
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_normalize_v2_stub);

// Phase B.1 — null FA contract.
//
// cg_normalize_v2(nullptr) returns a non-null, empty program.
// Same defensive contract as v1's cg_normalize().
int run_cg_normalize_v2_null() {
  CGv2Program *p = cg_normalize_v2(nullptr);
  if (!p) {
    printf("  cg_normalize_v2(nullptr) returned NULL\n");
    return 1;
  }
  if (p->funs.n != 0 || p->globals.n != 0 ||
      p->constants.n != 0 || p->types.n != 0) {
    printf("  expected empty program for null FA\n");
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_normalize_v2_null);

// Phase B.2 — type translation.
//
// build_types() walks fa->sym_set via collect_types_and_globals
// and populates the sym→CGv2Type map. Numeric Syms short-
// circuit to predefined CGv2Type instances; user struct types
// (Type_RECORD) become fresh CGv2Type entries with field lists.
//
// In a freshly-initialized FA there are no user types — the
// expected outcome is that build_types runs cleanly without
// adding entries to prog->types (predefined types stay
// reserved). The lookup_type contract still holds.
int run_cg_normalize_v2_builtin_types() {
  ifa_reset();
  ifa_init(new IRCallbacks);

  CGv2Program *p = cg_normalize_v2(pdb->fa);
  if (!p) {
    printf("  cg_normalize_v2 returned NULL\n");
    return 1;
  }
  // No user types in an empty FA — prog->types must stay empty.
  if (p->types.n != 0) {
    printf("  expected 0 user types, got %d\n", p->types.n);
    return 1;
  }
  // Predefined still resolvable.
  CGv2Type *i64 = p->lookup_type("int64");
  CGv2Type *flt = p->lookup_type("float64");
  CGv2Type *vd  = p->lookup_type("void");
  if (!i64 || !flt || !vd) {
    printf("  predefined int64/float64/void not resolvable\n");
    return 1;
  }
  if (i64->kind != CG2T_INT || i64->bits != 64) {
    printf("  predefined int64 wrong shape\n");
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_normalize_v2_builtin_types);

// Phase B.3 — globals/constants pass runs cleanly on empty FA.
// The freshly-initialized FA has no user-declared globals, so
// prog->constants and prog->globals should both stay empty.
// This validates that build_globals's walker doesn't add
// spurious entries from builtin syms.
int run_cg_normalize_v2_no_globals() {
  ifa_reset();
  ifa_init(new IRCallbacks);

  CGv2Program *p = cg_normalize_v2(pdb->fa);
  if (!p) {
    printf("  cg_normalize_v2 returned NULL\n");
    return 1;
  }
  if (p->constants.n != 0) {
    printf("  expected 0 constants, got %d\n", p->constants.n);
    return 1;
  }
  if (p->globals.n != 0) {
    printf("  expected 0 globals, got %d\n", p->globals.n);
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_normalize_v2_no_globals);

// Phase B.4 — function declarations.
//
// build_funs walks fa->funs and translates each live Fun
// (with an entry PNode) into a CGv2Fun with signature +
// formals. No body emission yet — Phase B.5+.
//
// In a freshly-initialized FA there are no user functions, so
// prog->funs stays empty and main_fun stays null. This
// validates the walker doesn't synthesize spurious entries.
int run_cg_normalize_v2_no_funs() {
  ifa_reset();
  ifa_init(new IRCallbacks);

  CGv2Program *p = cg_normalize_v2(pdb->fa);
  if (!p) { printf("  cg_normalize_v2 returned NULL\n"); return 1; }
  if (p->funs.n != 0) {
    printf("  expected 0 funs, got %d\n", p->funs.n);
    return 1;
  }
  if (p->main_fun != nullptr) {
    printf("  expected null main_fun\n");
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_normalize_v2_no_funs);
