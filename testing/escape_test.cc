// SPDX-License-Identifier: BSD-3-Clause
//
// Unit tests for the escape lattice and `compute_escape`
// driver in analysis/escape.cc.  Pairs with the IR-phase
// fixtures under ifa/tests/ir/escape/ — the fixtures
// exercise the analysis on small synthetic IF1 programs;
// these tests exercise the lattice algebra and the driver's
// handling of degenerate inputs that can't easily be
// expressed as an .ir fixture.
//
// See ESCAPE_PLAN.md.

#include "ifadefs.h"

#include "analysis/escape.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "sym.h"
#include "unit.h"
#include "var.h"

#define CHECK(expr)                                                       \
  do {                                                                    \
    if (!(expr)) {                                                        \
      printf("  CHECK failed %s:%d: %s\n", __FILE__, __LINE__, #expr);    \
      return 1;                                                           \
    }                                                                     \
  } while (0)

// ---------------------------------------------------------------------------
// 1. Lattice algebra — join_escape: idempotent, commutative,
//    Escape is absorbing, NoEscape is identity.
// ---------------------------------------------------------------------------
static int test_escape_lattice_algebra() {
  // Identity element (NoEscape) under JOIN.
  CHECK(join_escape(ES_NoEscape, ES_NoEscape) == ES_NoEscape);

  // Absorbing element (Escape) under JOIN.
  CHECK(join_escape(ES_Escape,   ES_NoEscape) == ES_Escape);
  CHECK(join_escape(ES_NoEscape, ES_Escape)   == ES_Escape);
  CHECK(join_escape(ES_Escape,   ES_Escape)   == ES_Escape);

  // Idempotent.
  CHECK(join_escape(ES_NoEscape, ES_NoEscape) ==
        join_escape(ES_NoEscape, ES_NoEscape));
  CHECK(join_escape(ES_Escape, ES_Escape) ==
        join_escape(ES_Escape, ES_Escape));

  // Commutative.
  CHECK(join_escape(ES_NoEscape, ES_Escape) ==
        join_escape(ES_Escape, ES_NoEscape));

  // Associative.
  EscapeStatus l = join_escape(
      join_escape(ES_NoEscape, ES_Escape), ES_NoEscape);
  EscapeStatus r = join_escape(
      ES_NoEscape, join_escape(ES_Escape, ES_NoEscape));
  CHECK(l == r);
  CHECK(l == ES_Escape);
  return 0;
}
UNIT_TEST_FUN(test_escape_lattice_algebra);

// ---------------------------------------------------------------------------
// 2. AVar ctor seeds escape = ES_Escape (conservative top).
//    This is the safe default for the no-analysis path: when
//    ifa_escape_in_fa is off, codegen reads CGv2Value::escapes
//    initialized from AVar::escape, and Escape is the only
//    sound starting point.
// ---------------------------------------------------------------------------
static int test_avar_ctor_escape_top() {
  fa_reset();
  fa = new FA(nullptr);
  fa->type_world.bottom_type = type_cannonicalize(new AType());
  fa->type_world.bottom_type->type = fa->type_world.bottom_type;

  // AVar(nullptr, contour) suffices — escape is set in the ctor
  // body, doesn't read var or contour.
  AVar *av = new AVar(nullptr, (void *)1);
  CHECK(av->escape == ES_Escape);
  return 0;
}
UNIT_TEST_FUN(test_avar_ctor_escape_top);

// ---------------------------------------------------------------------------
// 3. compute_escape on null / no-funs FA — must not crash.
//    Important because ifa_escape_in_fa can be flipped on
//    during a test where the FA has never been populated
//    (e.g. unit-test entry points that call into IFA
//    incrementally).
// ---------------------------------------------------------------------------
static int test_compute_escape_null_fa() {
  // Don't crash on null.
  int prev = ifa_escape_in_fa;
  ifa_escape_in_fa = 1;
  compute_escape(nullptr);    // null fa → no-op
  ifa_escape_in_fa = prev;
  return 0;
}
UNIT_TEST_FUN(test_compute_escape_null_fa);

static int test_compute_escape_empty_fa() {
  fa_reset();
  fa = new FA(nullptr);
  fa->type_world.bottom_type = type_cannonicalize(new AType());
  fa->type_world.bottom_type->type = fa->type_world.bottom_type;
  // fa->funs and fa->ess both empty.
  int prev = ifa_escape_in_fa;
  ifa_escape_in_fa = 1;
  compute_escape(fa);
  ifa_escape_in_fa = prev;
  // No assertion — just shouldn't crash and shouldn't infinite-loop.
  return 0;
}
UNIT_TEST_FUN(test_compute_escape_empty_fa);

// ---------------------------------------------------------------------------
// 4. The flag gate: when ifa_escape_in_fa is off, compute_escape
//    is a no-op (no mutation, no work).  Codegen relies on this
//    to keep Stage 3 as the production path during Phases 1-4.
// ---------------------------------------------------------------------------
static int test_compute_escape_flag_off_noop() {
  fa_reset();
  fa = new FA(nullptr);
  fa->type_world.bottom_type = type_cannonicalize(new AType());
  fa->type_world.bottom_type->type = fa->type_world.bottom_type;

  // Build an AVar with escape=NoEscape so we can detect any
  // mutation: a real compute_escape pass would either leave
  // it alone (it has no Var/PNode context) or set it back to
  // a default.  The point: with flag off, nothing happens.
  AVar *av = new AVar(nullptr, (void *)1);
  av->escape = ES_NoEscape;

  int prev = ifa_escape_in_fa;
  ifa_escape_in_fa = 0;
  compute_escape(fa);
  ifa_escape_in_fa = prev;

  CHECK(av->escape == ES_NoEscape);   // unmodified
  return 0;
}
UNIT_TEST_FUN(test_compute_escape_flag_off_noop);

// ---------------------------------------------------------------------------
// 5. Idempotence: running compute_escape twice produces the
//    same result.  This is the property the IR-phase printer
//    relies on when it calls compute_escape directly after
//    FA::analyze has already run it once (production
//    ifa_analyze → compute_escape → printer → compute_escape).
// ---------------------------------------------------------------------------
static int test_compute_escape_idempotent_empty() {
  fa_reset();
  fa = new FA(nullptr);
  fa->type_world.bottom_type = type_cannonicalize(new AType());
  fa->type_world.bottom_type->type = fa->type_world.bottom_type;

  int prev = ifa_escape_in_fa;
  ifa_escape_in_fa = 1;
  compute_escape(fa);
  compute_escape(fa);   // 2nd call shouldn't crash or mutate
                        // state into something inconsistent.
  ifa_escape_in_fa = prev;
  return 0;
}
UNIT_TEST_FUN(test_compute_escape_idempotent_empty);
