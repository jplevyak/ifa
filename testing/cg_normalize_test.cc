// SPDX-License-Identifier: BSD-3-Clause
// Unit test for Phase 1 of CG_IR_PLAN: cg_normalize(fa) returns a
// non-null, empty CGProgram for any input.
//
// Phase 1 only verifies the stub contract — that the entry point is
// callable, returns a non-null pointer, and the returned CGProgram's
// member vectors are empty (no funs, no globals, no types, no main).
// Phase 2 will replace this stub with the real normalization pass and
// extend the test accordingly.
//
// Registered with the UnitTest framework so `ifa --test` exercises it.

#include "ifadefs.h"

#include "ifa.h"
#include "pdb.h"
#include "unit.h"
#include "codegen/cg_ir.h"
#include "testing/test_callbacks.h"

#include <stdio.h>

// Run cg_normalize() against a freshly-initialized FA with no user
// input. The Phase 1 stub returns an empty CGProgram regardless of
// what's in the FA, so we don't need to parse anything.
int run_cg_normalize_stub() {
  ifa_reset();
  ifa_init(new IRCallbacks);

  CGProgram *p = cg_normalize(pdb->fa);
  if (!p) {
    printf("  cg_normalize returned NULL\n");
    return 1;
  }
  if (p->funs.n != 0) {
    printf("  cg_normalize stub returned %d funs, expected 0\n", p->funs.n);
    return 1;
  }
  if (p->main_fun != 0) {
    printf("  cg_normalize stub set main_fun, expected null\n");
    return 1;
  }
  if (p->globals.n != 0) {
    printf("  cg_normalize stub returned %d globals, expected 0\n", p->globals.n);
    return 1;
  }
  if (p->types.n != 0) {
    printf("  cg_normalize stub returned %d types, expected 0\n", p->types.n);
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_cg_normalize_stub);
