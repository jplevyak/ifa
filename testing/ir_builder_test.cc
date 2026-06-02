// SPDX-License-Identifier: BSD-3-Clause
//
// Smoke test for ir_builder (Phase 09 step C 7.1). Verifies the
// Layer 2 builders construct a syntactically valid IF1 program
// without crashing.
//
// No FA-level assertions yet — this just locks "the API works at
// all." Behavioral verification happens via the synthetic .synth
// fixtures in C 7.3+.

#include "ifadefs.h"

#include "ast.h"
#include "builtin.h"
#include "code.h"
#include "ifa.h"
#include "if1.h"
#include "pdb.h"
#include "sym.h"
#include "testing/ir_builder.h"
#include "testing/test_callbacks.h"
#include "unit.h"

#include <stdio.h>
#include <string.h>

namespace {

// Build a trivial closure: fn(x) { return x }.
// Verify: an entry appears in if1->allclosures with the expected
// args.n, ret, and a body Code tree.
static int test_trivial_closure() {
  ifa_init(new IRCallbacks);

  Sym *x = ir::local("x");
  Sym *fn = ClosureBuilder("trivial")
      .arg(x)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        cb.move(x, ret);
        cb.reply(cont, ret);
      });

  // Trivial expectations: fn is in allclosures, has a code body,
  // and its has-list (formals) contains x.
  int ok = 1;
  bool found = false;
  for (Sym *c : if1->allclosures) if (c == fn) { found = true; break; }
  if (!found) {
    fprintf(stderr, "trivial_closure: fn not in if1->allclosures\n");
    ok = 0;
  }
  if (!fn->code) {
    fprintf(stderr, "trivial_closure: fn->code is null\n");
    ok = 0;
  }
  bool has_x = false;
  for (Sym *s : fn->has) if (s == x) { has_x = true; break; }
  if (!has_x) {
    fprintf(stderr, "trivial_closure: fn->has does not contain x\n");
    ok = 0;
  }

  ifa_reset();
  return ok ? 0 : 1;
}

// Build a record type with two fields, then construct a closure
// that allocates one, writes a field, and reads it back.
static int test_record_and_field_access() {
  ifa_init(new IRCallbacks);

  Sym *Point = RecordBuilder("Point").field("x").field("y").build();

  Sym *fn = ClosureBuilder("use_point")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *p = ir::new_instance(cb, Point, "p");
        // Store a symbol (no need for a numeric immediate here —
        // any Sym works for the builder smoke test).
        Sym *tag = ir::symbol("hello");
        ir::set_field(cb, p, "x", tag);
        Sym *v = ir::get_field(cb, p, "x", "v");
        cb.move(v, ret);
        cb.reply(cont, ret);
      });

  int ok = 1;
  if (Point->type_kind != Type_RECORD) {
    fprintf(stderr, "record: Type_RECORD not set\n");
    ok = 0;
  }
  if (Point->has.n != 2) {
    fprintf(stderr, "record: expected 2 fields, got %d\n", Point->has.n);
    ok = 0;
  }
  if (!fn->code) {
    fprintf(stderr, "record: fn->code is null\n");
    ok = 0;
  }

  ifa_reset();
  return ok ? 0 : 1;
}

// Run both subtests, return non-zero on any failure.
int test_ir_builder() {
  int rc = 0;
  rc |= test_trivial_closure();
  rc |= test_record_and_field_access();
  return rc;
}

}  // namespace

UNIT_TEST_FUN(test_ir_builder);
