// SPDX-License-Identifier: BSD-3-Clause
//
// Unit tests for the lattice operations in fa.cc:
//   type_union, type_intersection, type_diff, type_cannonicalize.
//
// These operations are pointer-pure once the canonical AType table is
// populated: same inputs return the same AType pointer, and the
// algebraic identities (idempotency, commutativity, absorbing/identity
// elements) hold by construction. The tests below build a minimal
// canonical-type world from scratch — just fa->type_world.bottom_type plus a handful
// of "boring" abstract types — and verify those identities directly,
// without spinning up a full FA pass.
//
// Why minimal scaffolding instead of calling the real initialize()?
//   - The real initialize() sets up worklists, builtin primitives,
//     global AVars, etc. — none of which the lattice ops touch.
//   - A boring Sym (all flags zero, type_kind = Type_NONE, type points
//     at itself) takes the canonicalization fast path: nothing is a
//     constant, nothing aliases a base type, nothing's a Type_TAGGED.
//   - That's enough to exercise the algebraic behavior. Real-program
//     behavior (constants, Type_SUM/TAGGED hierarchies, num_kind
//     coercions) is exercised by the phase tests under `make test`.

#include "ifadefs.h"

#include "fa.h"
#include "sym.h"
#include "unit.h"

#define CHECK(expr)                                                       \
  do {                                                                    \
    if (!(expr)) {                                                        \
      printf("  CHECK failed %s:%d: %s\n", __FILE__, __LINE__, #expr);    \
      return 1;                                                           \
    }                                                                     \
  } while (0)

// Allocate a "boring" type Sym — no constants, no aliasing, no
// num_kind, type points at itself. This is the minimum a CreationSet's
// Sym needs to satisfy type_cannonicalize's fast path.
static Sym *boring_sym(cchar *name) {
  Sym *s = new Sym();
  s->name = name;
  s->type = s;
  return s;
}

static AType *boring_type(cchar *name) {
  return make_abstract_type(boring_sym(name));
}

// Stand up just enough state for the lattice ops to work:
//   - a fresh FA with num_constants_per_variable=1 (the ctor default),
//   - fa->type_world.bottom_type built from an empty AType (matches initialize()
//     at fa.cc fa->type_world.bottom_type = type_cannonicalize(new AType())).
//
// fa_reset() at the top makes the test idempotent across runs of the
// UnitTest harness — without it a second --test invocation in the
// same process would inherit stale canonical_atypes entries.
static void init_minimal_lattice() {
  fa_reset();
  fa = new FA(nullptr);
  fa->type_world.bottom_type = type_cannonicalize(new AType());
  fa->type_world.bottom_type->type = fa->type_world.bottom_type;
}

// ---------------------------------------------------------------------------
// type_union: identity, idempotency, commutativity, associativity
// ---------------------------------------------------------------------------
static int test_type_union() {
  init_minimal_lattice();
  AType *A = boring_type("A");
  AType *B = boring_type("B");
  AType *C = boring_type("C");

  // bottom is the identity element
  CHECK(type_union(fa->type_world.bottom_type, A) == A);
  CHECK(type_union(A, fa->type_world.bottom_type) == A);

  // idempotent
  CHECK(type_union(A, A) == A);

  // commutative — same canonical AType regardless of arg order
  CHECK(type_union(A, B) == type_union(B, A));

  // associative
  AType *left  = type_union(type_union(A, B), C);
  AType *right = type_union(A, type_union(B, C));
  CHECK(left == right);

  // a ∪ b strictly contains a and b
  AType *AB = type_union(A, B);
  CHECK(AB->set_in(A->sorted[0]));
  CHECK(AB->set_in(B->sorted[0]));
  return 0;
}
UNIT_TEST_FUN(test_type_union);

// ---------------------------------------------------------------------------
// type_intersection: identity, absorbing element, idempotency, disjointness
// ---------------------------------------------------------------------------
static int test_type_intersection() {
  init_minimal_lattice();
  AType *A = boring_type("A");
  AType *B = boring_type("B");

  // bottom is absorbing — intersection with bottom is bottom
  CHECK(type_intersection(fa->type_world.bottom_type, A) == fa->type_world.bottom_type);
  CHECK(type_intersection(A, fa->type_world.bottom_type) == fa->type_world.bottom_type);

  // idempotent
  CHECK(type_intersection(A, A) == A);

  // distinct boring types are disjoint — their intersection is bottom
  CHECK(type_intersection(A, B) == fa->type_world.bottom_type);

  // a ∩ (a ∪ b) == a (absorption law)
  AType *AB = type_union(A, B);
  CHECK(type_intersection(A, AB) == A);
  CHECK(type_intersection(AB, A) == A);
  return 0;
}
UNIT_TEST_FUN(test_type_intersection);

// ---------------------------------------------------------------------------
// type_diff: identity, self, distinct, union round-trip
// ---------------------------------------------------------------------------
static int test_type_diff() {
  init_minimal_lattice();
  AType *A = boring_type("A");
  AType *B = boring_type("B");

  // x - bottom = x
  CHECK(type_diff(A, fa->type_world.bottom_type) == A);
  // x - x = bottom
  CHECK(type_diff(A, A) == fa->type_world.bottom_type);
  // distinct boring types — diff leaves the LHS untouched
  CHECK(type_diff(A, B) == A);
  // bottom - x = bottom
  CHECK(type_diff(fa->type_world.bottom_type, A) == fa->type_world.bottom_type);

  // (a ∪ b) - a = b for disjoint boring types
  AType *AB = type_union(A, B);
  CHECK(type_diff(AB, A) == B);
  CHECK(type_diff(AB, B) == A);
  return 0;
}
UNIT_TEST_FUN(test_type_diff);

// ---------------------------------------------------------------------------
// type_cannonicalize: structural equality => pointer equality.
// (The function asserts its input is not-yet-canonical — sorted.n == 0 —
// so it is one-way by contract. What we verify is the hash-cons
// guarantee: any two structurally-identical inputs collapse to the
// same canonical AType pointer.)
// ---------------------------------------------------------------------------
static int test_type_cannonicalize() {
  init_minimal_lattice();
  AType *A = boring_type("A");
  AType *B = boring_type("B");

  // Two fresh ATypes built from the same CreationSets canonicalize to
  // the same pointer regardless of insertion order — the whole point
  // of the canonical table.
  AType *t1 = new AType();
  AType *t2 = new AType();
  t1->set_add(A->sorted[0]);
  t1->set_add(B->sorted[0]);
  t2->set_add(B->sorted[0]);
  t2->set_add(A->sorted[0]);
  AType *c1 = type_cannonicalize(t1);
  AType *c2 = type_cannonicalize(t2);
  CHECK(c1 == c2);

  // ... and the canonical pointer matches the AType type_union built
  // independently from the same operands.
  AType *AB = type_union(A, B);
  CHECK(c1 == AB);
  return 0;
}
UNIT_TEST_FUN(test_type_cannonicalize);

// ---------------------------------------------------------------------------
// Cross-op: union/intersection/diff agree on the disjoint-set algebra
// ---------------------------------------------------------------------------
static int test_lattice_cross_ops() {
  init_minimal_lattice();
  AType *A = boring_type("A");
  AType *B = boring_type("B");

  // (a ∪ b) - (a ∩ b) == (a ∪ b)  -- for disjoint a, b, the
  // intersection is bottom and the union is unchanged by the diff.
  AType *u = type_union(A, B);
  AType *i = type_intersection(A, B);
  CHECK(i == fa->type_world.bottom_type);
  CHECK(type_diff(u, i) == u);

  // a ∪ b == (a - b) ∪ (b - a) ∪ (a ∩ b)  -- the full inclusion-
  // exclusion identity. For disjoint a,b, this reduces to a ∪ b == a ∪ b.
  AType *ab = type_diff(A, B);
  AType *ba = type_diff(B, A);
  AType *both = type_intersection(A, B);
  AType *rebuilt = type_union(type_union(ab, ba), both);
  CHECK(rebuilt == u);
  return 0;
}
UNIT_TEST_FUN(test_lattice_cross_ops);

// ---------------------------------------------------------------------------
// type_violation dedup is order-invariant (regression test for issue 009).
//
// The bug closed by issue 009 was a printer mis-reporting `.n`
// (Vec-as-set table capacity) instead of `.set_count()` (live element
// count) at every site that read `type_violations`. Adjacent failure
// modes worth nailing down:
//
//   - Someone changes ATypeViolationHashFuns::equal so that distinct
//     triples accidentally compare equal — count would deflate.
//   - Someone changes the hash function such that collision behavior
//     changes — count would still be right (set is collision-tolerant)
//     but reporting via `.n` would be more or less affected.
//   - Someone reverts type_violations_count() back to reading `.n`.
//
// This test covers all three by exercising the
// "set_count() == #unique triples regardless of insertion order"
// invariant.
// ---------------------------------------------------------------------------
static AVar *dummy_avar() {
  // type_violation() only uses AVar pointers as opaque hash keys —
  // never derefs them. A zero-initialized AVar with a unique id is
  // enough. Pass `(void *)1` for contour so the field isn't null
  // (a null contour is a sentinel meaning "global" in other paths,
  // not relevant here but cheap to avoid).
  return new AVar(nullptr, (void *)1);
}

static int test_type_violation_dedup_invariance() {
  init_minimal_lattice();

  // Build six distinct AVar* keys: 3 "av" sides, 3 "send" sides.
  AVar *a0 = dummy_avar();
  AVar *a1 = dummy_avar();
  AVar *a2 = dummy_avar();
  AVar *s0 = dummy_avar();
  AVar *s1 = dummy_avar();
  AVar *s2 = dummy_avar();

  // Distinct triples we'll record: (a0,s0), (a1,s0), (a2,s1), (a0,s2).
  // Four unique under the (kind, av, send) dedup key.
  using K = ATypeViolation_kind;

  // Order A: a0/s0, a1/s0, a2/s1, a0/s2, then duplicate a1/s0 and a2/s1.
  type_violation(K::SEND_ARGUMENT, a0, fa->type_world.bottom_type, s0);
  type_violation(K::SEND_ARGUMENT, a1, fa->type_world.bottom_type, s0);
  type_violation(K::SEND_ARGUMENT, a2, fa->type_world.bottom_type, s1);
  type_violation(K::SEND_ARGUMENT, a0, fa->type_world.bottom_type, s2);
  type_violation(K::SEND_ARGUMENT, a1, fa->type_world.bottom_type, s0);  // dup of #2
  type_violation(K::SEND_ARGUMENT, a2, fa->type_world.bottom_type, s1);  // dup of #3
  CHECK(type_violations_count() == 4);

  // Reset (clears type_violations + type_violation_hash + fa->type_world.bottom_type).
  init_minimal_lattice();
  a0 = dummy_avar();
  a1 = dummy_avar();
  a2 = dummy_avar();
  s0 = dummy_avar();
  s1 = dummy_avar();
  s2 = dummy_avar();

  // Order B: same triples but reversed, with duplicates interleaved
  // earlier than the original record.
  type_violation(K::SEND_ARGUMENT, a0, fa->type_world.bottom_type, s2);
  type_violation(K::SEND_ARGUMENT, a0, fa->type_world.bottom_type, s2);  // immediate dup
  type_violation(K::SEND_ARGUMENT, a2, fa->type_world.bottom_type, s1);
  type_violation(K::SEND_ARGUMENT, a1, fa->type_world.bottom_type, s0);
  type_violation(K::SEND_ARGUMENT, a0, fa->type_world.bottom_type, s0);
  type_violation(K::SEND_ARGUMENT, a1, fa->type_world.bottom_type, s0);  // dup
  CHECK(type_violations_count() == 4);

  // Different `kind` is a separate triple even with the same av/send.
  init_minimal_lattice();
  a0 = dummy_avar();
  s0 = dummy_avar();
  type_violation(K::SEND_ARGUMENT, a0, fa->type_world.bottom_type, s0);
  type_violation(K::NOTYPE,        a0, fa->type_world.bottom_type, s0);
  type_violation(K::SEND_ARGUMENT, a0, fa->type_world.bottom_type, s0);  // dup of #1
  CHECK(type_violations_count() == 2);
  return 0;
}
UNIT_TEST_FUN(test_type_violation_dedup_invariance);
