#include "common.h"

// Return 1 (failure) with a message if expr is false.
#define CHECK(expr)                                                     \
  do {                                                                  \
    if (!(expr)) {                                                      \
      printf("  CHECK failed %s:%d: %s\n", __FILE__, __LINE__, #expr); \
      return 1;                                                         \
    }                                                                   \
  } while (0)

// Cast int to void* for use as non-zero pointer set elements.
static void *ip(int x) { return (void *)(intptr_t)x; }

// ---------------------------------------------------------------------------
// Vector: basic add, inline storage, n tracking
// ---------------------------------------------------------------------------
static int vec_add_basic() {
  Vec<int> v;
  CHECK(v.n == 0);
  CHECK(v.v == nullptr);

  v.add(10);
  CHECK(v.n == 1);
  CHECK(v.v == v.e);  // inline storage
  CHECK(v.v[0] == 10);

  v.add(20);
  v.add(30);
  v.add(40);
  CHECK(v.n == 4);
  CHECK(v.v == v.e);  // still inline (VEC_INTEGRAL_SIZE = 4)

  v.add(50);          // 5th element triggers heap allocation
  CHECK(v.n == 5);
  CHECK(v.v != v.e);
  CHECK(v.v[4] == 50);

  // Verify all values survived the copy to heap.
  CHECK(v.v[0] == 10);
  CHECK(v.v[1] == 20);
  CHECK(v.v[2] == 30);
  CHECK(v.v[3] == 40);
  return 0;
}
UNIT_TEST_FUN(vec_add_basic);

// ---------------------------------------------------------------------------
// Vector: growth through many reallocations
// ---------------------------------------------------------------------------
static int vec_growth() {
  Vec<int> v;
  for (int i = 0; i < 1000; i++) v.add(i);
  CHECK(v.n == 1000);
  int sum = 0;
  for (int i = 0; i < 1000; i++) sum += v.v[i];
  CHECK(sum == 999 * 500);
  return 0;
}
UNIT_TEST_FUN(vec_growth);

// ---------------------------------------------------------------------------
// Vector: pop
// ---------------------------------------------------------------------------
static int vec_pop() {
  Vec<int> v;
  v.add(1);
  v.add(2);
  v.add(3);

  CHECK(v.pop() == 3);
  CHECK(v.n == 2);
  CHECK(v.pop() == 2);
  CHECK(v.n == 1);
  CHECK(v.pop() == 1);
  CHECK(v.n == 0);
  // After last element popped, clear() is called: v should be reset.
  CHECK(v.v == nullptr);

  // pop on empty returns 0.
  CHECK(v.pop() == 0);
  return 0;
}
UNIT_TEST_FUN(vec_pop);

// ---------------------------------------------------------------------------
// Vector: get() bounds-safe access
// ---------------------------------------------------------------------------
static int vec_get() {
  Vec<int> v;
  v.add(100);
  v.add(200);
  CHECK(v.get(0) == 100);
  CHECK(v.get(1) == 200);
  CHECK(v.get(2) == 0);   // out of bounds → 0
  CHECK(v.get(-1) == 0);  // negative → 0
  return 0;
}
UNIT_TEST_FUN(vec_get);

// ---------------------------------------------------------------------------
// Vector: operator[], first(), last()
// ---------------------------------------------------------------------------
static int vec_accessors() {
  Vec<int> v;
  for (int i = 1; i <= 5; i++) v.add(i);
  CHECK(v[0] == 1);
  CHECK(v[4] == 5);
  CHECK(v.first() == 1);
  CHECK(v.last() == 5);
  CHECK(v.end() == v.v + 5);
  return 0;
}
UNIT_TEST_FUN(vec_accessors);

// ---------------------------------------------------------------------------
// Vector: reserve() pre-allocates without changing n
// ---------------------------------------------------------------------------
static int vec_reserve() {
  Vec<int> v;
  v.reserve(100);
  CHECK(v.n == 0);
  CHECK(v.v != nullptr);
  CHECK(v.v != v.e);

  for (int i = 0; i < 100; i++) v.add(i);
  CHECK(v.n == 100);

  // All values correct.
  int sum = 0;
  for (int i = 0; i < 100; i++) sum += v.v[i];
  CHECK(sum == 99 * 50);
  return 0;
}
UNIT_TEST_FUN(vec_reserve);

// ---------------------------------------------------------------------------
// Vector: fill() extends with zero elements
// ---------------------------------------------------------------------------
static int vec_fill() {
  Vec<int> v;
  v.add(7);
  v.fill(5);
  CHECK(v.n == 5);
  CHECK(v.v[0] == 7);
  CHECK(v.v[1] == 0);
  CHECK(v.v[4] == 0);

  // fill() with n <= current n is a no-op.
  v.fill(3);
  CHECK(v.n == 5);
  return 0;
}
UNIT_TEST_FUN(vec_fill);

// ---------------------------------------------------------------------------
// Vector: append() skips null/zero elements from source
// ---------------------------------------------------------------------------
static int vec_append() {
  Vec<int> a, b;
  a.add(1);
  a.add(2);
  a.add(0);  // null — will be skipped by append
  a.add(3);

  b.add(10);
  b.append(a);
  CHECK(b.n == 4);  // 10, 1, 2, 3 (0 skipped)
  CHECK(b.v[0] == 10);
  CHECK(b.v[1] == 1);
  CHECK(b.v[2] == 2);
  CHECK(b.v[3] == 3);
  return 0;
}
UNIT_TEST_FUN(vec_append);

// ---------------------------------------------------------------------------
// Vector: prepend()
// ---------------------------------------------------------------------------
static int vec_prepend() {
  Vec<int> a, b;
  a.add(1);
  a.add(2);

  b.add(10);
  b.add(20);
  b.prepend(a);

  CHECK(b.n == 4);
  CHECK(b.v[0] == 1);
  CHECK(b.v[1] == 2);
  CHECK(b.v[2] == 10);
  CHECK(b.v[3] == 20);
  return 0;
}
UNIT_TEST_FUN(vec_prepend);

// ---------------------------------------------------------------------------
// Vector: insert at index (ref form, value form, vec form)
// ---------------------------------------------------------------------------
static int vec_insert() {
  Vec<int> v;
  v.add(1);
  v.add(3);

  // Insert value at index 1: [1, 2, 3]
  v.insert(1, 2);
  CHECK(v.n == 3);
  CHECK(v.v[0] == 1 && v.v[1] == 2 && v.v[2] == 3);

  // Insert ref at index 0: [0, 1, 2, 3]
  v.insert(0) = 0;
  CHECK(v.n == 4);
  CHECK(v.v[0] == 0 && v.v[1] == 1);

  // Insert vec at index 2: [0, 1, A, B, 2, 3]
  Vec<int> ins;
  ins.add(100);
  ins.add(200);
  v.insert(2, ins);
  CHECK(v.n == 6);
  CHECK(v.v[2] == 100 && v.v[3] == 200);
  CHECK(v.v[4] == 2 && v.v[5] == 3);

  // push() inserts at front.
  v.push(999);
  CHECK(v.n == 7);
  CHECK(v.v[0] == 999);
  return 0;
}
UNIT_TEST_FUN(vec_insert);

// ---------------------------------------------------------------------------
// Vector: remove_index() and remove()
// ---------------------------------------------------------------------------
static int vec_remove() {
  Vec<int> v;
  for (int i = 1; i <= 5; i++) v.add(i);  // [1,2,3,4,5]

  v.remove_index(2);  // remove 3 → [1,2,4,5]
  CHECK(v.n == 4);
  CHECK(v.v[0] == 1 && v.v[1] == 2 && v.v[2] == 4 && v.v[3] == 5);

  v.remove(4);  // remove by value → [1,2,5]
  CHECK(v.n == 3);
  CHECK(v.v[0] == 1 && v.v[1] == 2 && v.v[2] == 5);

  v.remove(99);  // not found — no change
  CHECK(v.n == 3);

  v.remove_index(0);  // remove first → [2,5]
  CHECK(v.n == 2 && v.v[0] == 2 && v.v[1] == 5);
  return 0;
}
UNIT_TEST_FUN(vec_remove);

// ---------------------------------------------------------------------------
// Vector: reverse()
// ---------------------------------------------------------------------------
static int vec_reverse() {
  Vec<int> v;
  for (int i = 1; i <= 6; i++) v.add(i);
  v.reverse();
  CHECK(v.n == 6);
  for (int i = 0; i < 6; i++) CHECK(v.v[i] == 6 - i);

  Vec<int> odd;
  for (int i = 1; i <= 5; i++) odd.add(i);
  odd.reverse();
  CHECK(odd.v[0] == 5 && odd.v[2] == 3 && odd.v[4] == 1);
  return 0;
}
UNIT_TEST_FUN(vec_reverse);

// ---------------------------------------------------------------------------
// Vector: qsort()
// ---------------------------------------------------------------------------
static bool int_lt(int a, int b) { return a < b; }

static int vec_qsort() {
  Vec<int> v;
  int vals[] = {5, 3, 8, 1, 9, 2, 7, 4, 6};
  for (int x : vals) v.add(x);
  v.qsort(int_lt);
  CHECK(v.n == 9);
  for (int i = 0; i < 9; i++) CHECK(v.v[i] == i + 1);

  // Already sorted.
  v.qsort(int_lt);
  for (int i = 0; i < 9; i++) CHECK(v.v[i] == i + 1);

  // Single element — no crash.
  Vec<int> s;
  s.add(42);
  s.qsort(int_lt);
  CHECK(s.v[0] == 42);
  return 0;
}
UNIT_TEST_FUN(vec_qsort);

// ---------------------------------------------------------------------------
// Vector: copy constructor, copy(), move()
// ---------------------------------------------------------------------------
static int vec_copy_move() {
  Vec<int> src;
  for (int i = 0; i < 8; i++) src.add(i);

  // Copy constructor.
  Vec<int> c1(src);
  CHECK(c1.n == 8);
  for (int i = 0; i < 8; i++) CHECK(c1.v[i] == i);
  // Independent: modify src, c1 unchanged.
  src.v[0] = 99;
  CHECK(c1.v[0] == 0);

  // copy() overwrites.
  Vec<int> c2;
  c2.copy(src);
  CHECK(c2.n == 8 && c2.v[0] == 99 && c2.v[1] == 1);

  // move() transfers ownership, clears source.
  Vec<int> dst;
  dst.move(src);
  CHECK(dst.n == 8);
  CHECK(src.n == 0 && src.v == nullptr);

  // Vec(vv, MOVE) constructor.
  Vec<int> m2(dst, Vec<int>::MOVE);
  CHECK(m2.n == 8);
  CHECK(dst.n == 0);
  return 0;
}
UNIT_TEST_FUN(vec_copy_move);

// ---------------------------------------------------------------------------
// Vector: Vec(C) single-element constructor
// ---------------------------------------------------------------------------
static int vec_single_ctor() {
  Vec<int> v(42);
  CHECK(v.n == 1);
  CHECK(v.v == v.e);
  CHECK(v.v[0] == 42);
  return 0;
}
UNIT_TEST_FUN(vec_single_ctor);

// ---------------------------------------------------------------------------
// Vector: in(), index(), add_exclusive()
// ---------------------------------------------------------------------------
static int vec_search() {
  Vec<int> v;
  for (int i = 1; i <= 5; i++) v.add(i);

  CHECK(v.in(3) != nullptr);
  CHECK(*v.in(3) == 3);
  CHECK(v.in(99) == nullptr);

  CHECK(v.index(4) == 3);
  CHECK(v.index(1) == 0);
  CHECK(v.index(99) == -1);

  CHECK(v.add_exclusive(99) == 1);  // new element added
  CHECK(v.n == 6);
  CHECK(v.add_exclusive(99) == 0);  // already present
  CHECK(v.n == 6);
  return 0;
}
UNIT_TEST_FUN(vec_search);

// ---------------------------------------------------------------------------
// Vector: count() counts non-zero elements
// ---------------------------------------------------------------------------
static int vec_count() {
  Vec<int> v;
  v.add(0);
  v.add(1);
  v.add(0);
  v.add(2);
  CHECK(v.n == 4);
  CHECK(v.count() == 2);  // only non-zero values
  return 0;
}
UNIT_TEST_FUN(vec_count);

// ---------------------------------------------------------------------------
// Set: linear phase (n <= SET_LINEAR_SIZE = 4), deduplication
// ---------------------------------------------------------------------------
static int set_linear() {
  Vec<void *> s;
  CHECK(s.is_vec());

  // First element.
  void **p1 = s.set_add(ip(10));
  CHECK(p1 != nullptr);  // newly added
  CHECK(s.n == 1);
  CHECK(s.is_vec());  // still vec-mode: n < SET_LINEAR_SIZE, i == 0

  // Duplicate returns null.
  CHECK(s.set_add(ip(10)) == nullptr);
  CHECK(s.n == 1);

  s.set_add(ip(20));
  s.set_add(ip(30));
  CHECK(s.n == 3);

  // set_in() in linear phase uses linear scan.
  CHECK(s.set_in(ip(10)) != nullptr);
  CHECK(s.set_in(ip(20)) != nullptr);
  CHECK(s.set_in(ip(99)) == nullptr);

  // 4th element (SET_LINEAR_SIZE): still linear.
  s.set_add(ip(40));
  CHECK(s.n == 4);
  CHECK(s.set_in(ip(40)) != nullptr);
  return 0;
}
UNIT_TEST_FUN(set_linear);

// ---------------------------------------------------------------------------
// Set: transition from linear to hash at 5th unique element
// ---------------------------------------------------------------------------
static int set_transition() {
  Vec<void *> s;
  for (int i = 1; i <= 4; i++) s.set_add(ip(i));
  CHECK(s.n == 4);
  CHECK(s.is_vec());  // still linear: i == 0

  // 5th element triggers hash mode.
  s.set_add(ip(5));
  CHECK(s.is_set());  // now in hash mode: i > 0 && i < n
  // n is now the hash table size (prime2[SET_INITIAL_INDEX=2] == 7).
  CHECK(s.n == 7);
  CHECK(s.set_count() == 5);

  // All original elements still findable.
  for (int i = 1; i <= 5; i++) CHECK(s.set_in(ip(i)) != nullptr);
  CHECK(s.set_in(ip(99)) == nullptr);
  return 0;
}
UNIT_TEST_FUN(set_transition);

// ---------------------------------------------------------------------------
// Set: hash phase — many elements, table expansion
// ---------------------------------------------------------------------------
static int set_hash_large() {
  Vec<void *> s;
  for (int i = 1; i <= 50; i++) s.set_add(ip(i));

  CHECK(s.is_set());
  CHECK(s.set_count() == 50);

  // All present, no false positives.
  for (int i = 1; i <= 50; i++) CHECK(s.set_in(ip(i)) != nullptr);
  CHECK(s.set_in(ip(0)) == nullptr);
  CHECK(s.set_in(ip(51)) == nullptr);

  // Duplicates still rejected.
  for (int i = 1; i <= 50; i++) CHECK(s.set_add(ip(i)) == nullptr);
  CHECK(s.set_count() == 50);
  return 0;
}
UNIT_TEST_FUN(set_hash_large);

// ---------------------------------------------------------------------------
// Set: first_in_set() returns any non-zero element
// ---------------------------------------------------------------------------
static int set_first() {
  Vec<void *> s;
  CHECK(s.first_in_set() == nullptr);  // empty

  s.set_add(ip(42));
  CHECK(s.first_in_set() == ip(42));

  for (int i = 1; i <= 10; i++) s.set_add(ip(i));
  CHECK(s.first_in_set() != nullptr);
  return 0;
}
UNIT_TEST_FUN(set_first);

// ---------------------------------------------------------------------------
// Set: set_remove() — removes a single element
// ---------------------------------------------------------------------------
static int set_remove() {
  Vec<void *> s;
  s.set_add(ip(1));
  s.set_add(ip(2));
  s.set_add(ip(3));

  // Remove from linear phase.
  s.set_remove(ip(2));
  CHECK(s.set_in(ip(1)) != nullptr);
  CHECK(s.set_in(ip(2)) == nullptr);
  CHECK(s.set_in(ip(3)) != nullptr);

  // Remove from hash phase.
  Vec<void *> h;
  for (int i = 1; i <= 10; i++) h.set_add(ip(i));
  h.set_remove(ip(5));
  CHECK(h.set_in(ip(5)) == nullptr);
  for (int i = 1; i <= 10; i++)
    if (i != 5) CHECK(h.set_in(ip(i)) != nullptr);
  return 0;
}
UNIT_TEST_FUN(set_remove);

// ---------------------------------------------------------------------------
// Set: set_union() — in-place union; returns 1 if changed, 0 if not
// ---------------------------------------------------------------------------
static int set_union() {
  Vec<void *> a, b;
  for (int i = 1; i <= 5; i++) a.set_add(ip(i));
  for (int i = 3; i <= 8; i++) b.set_add(ip(i));

  int changed = a.set_union(b);
  CHECK(changed == 1);
  CHECK(a.set_count() == 8);
  for (int i = 1; i <= 8; i++) CHECK(a.set_in(ip(i)) != nullptr);

  // Union with a subset: no change.
  Vec<void *> sub;
  sub.set_add(ip(1));
  sub.set_add(ip(2));
  changed = a.set_union(sub);
  CHECK(changed == 0);
  CHECK(a.set_count() == 8);

  // Vec(vv, SET) constructor copies as a set.
  Vec<void *> c(b, Vec<void *>::SET);
  CHECK(c.set_count() == b.set_count());
  for (int i = 3; i <= 8; i++) CHECK(c.set_in(ip(i)) != nullptr);
  return 0;
}
UNIT_TEST_FUN(set_union);

// ---------------------------------------------------------------------------
// Set: set_intersection() in-place — keeps only elements in both sets
// ---------------------------------------------------------------------------
static int set_intersection_inplace() {
  Vec<void *> a, b;
  for (int i = 1; i <= 6; i++) a.set_add(ip(i));
  for (int i = 4; i <= 9; i++) b.set_add(ip(i));

  int changed = a.set_intersection(b);
  CHECK(changed == 1);  // elements were removed
  CHECK(a.set_count() == 3);
  for (int i = 4; i <= 6; i++) CHECK(a.set_in(ip(i)) != nullptr);
  for (int i = 1; i <= 3; i++) CHECK(a.set_in(ip(i)) == nullptr);

  // Intersection with superset: no change.
  Vec<void *> sup;
  for (int i = 1; i <= 20; i++) sup.set_add(ip(i));
  Vec<void *> orig;
  orig.set_add(ip(4));
  orig.set_add(ip(5));
  changed = orig.set_intersection(sup);
  CHECK(changed == 0);
  return 0;
}
UNIT_TEST_FUN(set_intersection_inplace);

// ---------------------------------------------------------------------------
// Set: set_intersection() into result
// ---------------------------------------------------------------------------
static int set_intersection_result() {
  Vec<void *> a, b, result;
  for (int i = 1; i <= 5; i++) a.set_add(ip(i));
  for (int i = 3; i <= 7; i++) b.set_add(ip(i));

  a.set_intersection(b, result);
  CHECK(result.set_count() == 3);
  for (int i = 3; i <= 5; i++) CHECK(result.set_in(ip(i)) != nullptr);
  CHECK(result.set_in(ip(1)) == nullptr);
  CHECK(result.set_in(ip(6)) == nullptr);
  return 0;
}
UNIT_TEST_FUN(set_intersection_result);

// ---------------------------------------------------------------------------
// Set: set_difference() into result — elements in a not in b
// ---------------------------------------------------------------------------
static int set_difference_result() {
  Vec<void *> a, b, result;
  for (int i = 1; i <= 6; i++) a.set_add(ip(i));
  for (int i = 4; i <= 9; i++) b.set_add(ip(i));

  a.set_difference(b, result);
  CHECK(result.set_count() == 3);
  for (int i = 1; i <= 3; i++) CHECK(result.set_in(ip(i)) != nullptr);
  for (int i = 4; i <= 6; i++) CHECK(result.set_in(ip(i)) == nullptr);
  return 0;
}
UNIT_TEST_FUN(set_difference_result);

// ---------------------------------------------------------------------------
// Set: set_disjunction() into result — elements in either but not both
// ---------------------------------------------------------------------------
static int set_disjunction_result() {
  Vec<void *> a, b, result;
  for (int i = 1; i <= 5; i++) a.set_add(ip(i));
  for (int i = 4; i <= 8; i++) b.set_add(ip(i));

  a.set_disjunction(b, result);
  // disjunction = (a - b) ∪ (b - a) = {1,2,3} ∪ {6,7,8} = {1,2,3,6,7,8}
  CHECK(result.set_count() == 6);
  for (int i = 1; i <= 3; i++) CHECK(result.set_in(ip(i)) != nullptr);
  CHECK(result.set_in(ip(4)) == nullptr);
  CHECK(result.set_in(ip(5)) == nullptr);
  for (int i = 6; i <= 8; i++) CHECK(result.set_in(ip(i)) != nullptr);
  return 0;
}
UNIT_TEST_FUN(set_disjunction_result);

// ---------------------------------------------------------------------------
// Set: some_intersection(), some_difference(), some_disjunction()
// ---------------------------------------------------------------------------
static int set_some_predicates() {
  Vec<void *> a, b, c, empty;
  for (int i = 1; i <= 5; i++) a.set_add(ip(i));
  for (int i = 4; i <= 8; i++) b.set_add(ip(i));
  for (int i = 10; i <= 15; i++) c.set_add(ip(i));  // disjoint from a

  // some_intersection: true iff any element is shared.
  CHECK(a.some_intersection(b) == 1);  // {4,5} shared
  CHECK(a.some_intersection(c) == 0);  // disjoint

  // some_difference: true iff any element of `this` is not in `vv`.
  CHECK(a.some_difference(b) == 1);  // {1,2,3} in a not in b
  Vec<void *> sub;
  sub.set_add(ip(1));
  CHECK(sub.some_difference(a) == 0);  // {1} ⊆ a

  // some_disjunction: true iff sets are not equal.
  CHECK(a.some_disjunction(b) == 1);  // differ
  Vec<void *> a2;
  for (int i = 1; i <= 5; i++) a2.set_add(ip(i));
  CHECK(a.some_disjunction(a2) == 0);  // identical content
  return 0;
}
UNIT_TEST_FUN(set_some_predicates);

// ---------------------------------------------------------------------------
// Set → Vec conversion: set_to_vec() compacts hash table into dense vec
// ---------------------------------------------------------------------------
static int set_to_vec_conversion() {
  Vec<void *> s;
  for (int i = 1; i <= 8; i++) s.set_add(ip(i));
  CHECK(s.is_set());
  int cnt = s.set_count();
  CHECK(cnt == 8);

  s.set_to_vec();
  CHECK(s.is_vec());
  CHECK(s.n == 8);

  // All 8 elements present (order arbitrary).
  Vec<void *> found;
  for (int i = 0; i < s.n; i++) found.set_add(s.v[i]);
  for (int i = 1; i <= 8; i++) CHECK(found.set_in(ip(i)) != nullptr);
  return 0;
}
UNIT_TEST_FUN(set_to_vec_conversion);

// ---------------------------------------------------------------------------
// Vec → Set conversion: vec_to_set() deduplicates and indexes for fast lookup
// ---------------------------------------------------------------------------
static int vec_to_set_conversion() {
  Vec<void *> v;
  // Add with duplicates.
  for (int i = 1; i <= 5; i++) v.add(ip(i));
  v.add(ip(3));  // duplicate
  v.add(ip(5));  // duplicate
  CHECK(v.n == 7);

  v.vec_to_set();
  CHECK(v.is_set());
  CHECK(v.set_count() == 5);  // duplicates removed
  for (int i = 1; i <= 5; i++) CHECK(v.set_in(ip(i)) != nullptr);
  return 0;
}
UNIT_TEST_FUN(vec_to_set_conversion);

// ---------------------------------------------------------------------------
// Set: is_vec() and is_set() predicates
// ---------------------------------------------------------------------------
static int set_is_vec_is_set() {
  Vec<void *> v;
  CHECK(v.is_vec());
  CHECK(!v.is_set());

  // Linear phase (≤ 4 elements): still vec-mode.
  for (int i = 1; i <= 4; i++) v.set_add(ip(i));
  CHECK(v.is_vec());

  // 5th element → hash mode.
  v.set_add(ip(5));
  CHECK(v.is_set());
  CHECK(!v.is_vec());

  // After set_to_vec(): back to vec.
  v.set_to_vec();
  CHECK(v.is_vec());
  CHECK(!v.is_set());
  return 0;
}
UNIT_TEST_FUN(set_is_vec_is_set);

// ---------------------------------------------------------------------------
// Range-based for: basic iteration over a vector (all n elements)
// ---------------------------------------------------------------------------
static int vec_range_for_basic() {
  Vec<int> v;
  for (int i = 1; i <= 5; i++) v.add(i);
  int sum = 0;
  for (int x : v) sum += x;
  CHECK(sum == 15);  // 1+2+3+4+5
  return 0;
}
UNIT_TEST_FUN(vec_range_for_basic);

// ---------------------------------------------------------------------------
// Range-based for: elements are references — modifications are reflected
// ---------------------------------------------------------------------------
static int vec_range_for_modify() {
  Vec<int> v;
  for (int i = 1; i <= 4; i++) v.add(i);
  for (int &x : v) x *= 2;
  CHECK(v.v[0] == 2 && v.v[1] == 4 && v.v[2] == 6 && v.v[3] == 8);
  return 0;
}
UNIT_TEST_FUN(vec_range_for_modify);

// ---------------------------------------------------------------------------
// Range-based for: empty vector — loop body never executes
// ---------------------------------------------------------------------------
static int vec_range_for_empty() {
  Vec<int> v;
  int count = 0;
  for (int x : v) { (void)x; count++; }
  CHECK(count == 0);
  return 0;
}
UNIT_TEST_FUN(vec_range_for_empty);

// ---------------------------------------------------------------------------
// Range-based for: const Vec<int>
// ---------------------------------------------------------------------------
static int vec_range_for_const() {
  Vec<int> v;
  for (int i = 1; i <= 3; i++) v.add(i);
  const Vec<int> &cv = v;
  int sum = 0;
  for (int x : cv) sum += x;
  CHECK(sum == 6);
  return 0;
}
UNIT_TEST_FUN(vec_range_for_const);

// ---------------------------------------------------------------------------
// values(): skips null/zero elements — useful in set (hash) mode
// ---------------------------------------------------------------------------
static int vec_values_range() {
  // In set (hash) mode n is the table size; null slots are hash holes.
  Vec<void *> s;
  for (int i = 1; i <= 8; i++) s.set_add(ip(i));
  CHECK(s.is_set());
  CHECK(s.n > 8);   // hash table is larger than element count

  int count = 0;
  Vec<void *> seen;
  for (void *p : s.values()) { seen.add(p); count++; }
  CHECK(count == 8);
  for (int i = 1; i <= 8; i++) CHECK(seen.in(ip(i)) != nullptr);

  // values() on a plain vector skips zero entries.
  Vec<int> v;
  v.add(1); v.add(0); v.add(3); v.add(0); v.add(5);
  int sum = 0, cnt = 0;
  for (int x : v.values()) { sum += x; cnt++; }
  CHECK(cnt == 3 && sum == 9);  // 1+3+5, zeros skipped
  return 0;
}
UNIT_TEST_FUN(vec_values_range);

// ---------------------------------------------------------------------------
// Accum: unique insertion-ordered accumulation
// ---------------------------------------------------------------------------
static int accum_test() {
  Accum<void *> acc;

  acc.add(ip(10));
  acc.add(ip(20));
  acc.add(ip(10));  // duplicate — should not appear twice in asvec
  acc.add(ip(30));
  acc.add(ip(20));  // duplicate

  // asvec has unique elements in insertion order.
  CHECK(acc.asvec.n == 3);
  CHECK(acc.asvec.v[0] == ip(10));
  CHECK(acc.asvec.v[1] == ip(20));
  CHECK(acc.asvec.v[2] == ip(30));

  // asset has the same unique elements (as a set).
  CHECK(acc.asset.set_count() == 3);
  CHECK(acc.asset.set_in(ip(10)) != nullptr);
  CHECK(acc.asset.set_in(ip(30)) != nullptr);
  CHECK(acc.asset.set_in(ip(99)) == nullptr);

  acc.clear();
  CHECK(acc.asvec.n == 0);
  return 0;
}
UNIT_TEST_FUN(accum_test);

// ---------------------------------------------------------------------------
// Intervals: insert and membership test, interval merging
// ---------------------------------------------------------------------------
static int intervals_test() {
  Intervals iv;

  // Empty.
  CHECK(iv.in(5) == 0);

  // Single point: stored as [1, 1].
  iv.insert(1);
  CHECK(iv.n == 2);
  CHECK(iv.in(1) == 1);
  CHECK(iv.in(0) == 0);
  CHECK(iv.in(2) == 0);

  // Adjacent above: [1,1] + 2 → [1,2].
  iv.insert(2);
  CHECK(iv.n == 2);
  CHECK(iv.in(2) == 1);

  // Gap: [1,2] and [6,6].
  iv.insert(6);
  CHECK(iv.n == 4);

  // Adjacent above existing interval: [6,6] + 7 → [6,7].
  iv.insert(7);
  CHECK(iv.n == 4);

  // New isolated point: [1,2], [6,7], [9,9].
  iv.insert(9);
  CHECK(iv.n == 6);

  // Isolated: [1,2], [4,4], [6,7], [9,9].
  iv.insert(4);
  CHECK(iv.n == 8);

  // Bridge [4,4] and [6,7] via [4,5] and [5,6]: insert 5.
  iv.insert(5);
  // 5 is adjacent to both 4 and 6: [4,4] + 5 → [4,5], then [4,5]+[6,7] → [4,7].
  CHECK(iv.n == 6);

  // Bridge [1,2] and [4,7] via 3.
  iv.insert(3);
  // [1,2]+3=[1,3], then [1,3]+[4,7]=[1,7].
  CHECK(iv.n == 4);

  // Bridge [1,7] and [9,9] via 8.
  iv.insert(8);
  CHECK(iv.n == 2);

  // All in [1,9].
  for (int i = 1; i <= 9; i++) CHECK(iv.in(i) == 1);
  CHECK(iv.in(0) == 0);
  CHECK(iv.in(10) == 0);

  // Inserting an already-in value is a no-op.
  iv.insert(5);
  CHECK(iv.n == 2);
  return 0;
}
UNIT_TEST_FUN(intervals_test);

// ---------------------------------------------------------------------------
// UnionFind: size, find, unify, path compression
// ---------------------------------------------------------------------------
static int union_find_test() {
  UnionFind uf;

  // Initialize 6 singleton elements.
  uf.size(6);
  CHECK(uf.n == 6);

  // Each element is its own representative.
  for (int i = 0; i < 6; i++) CHECK(uf.find(i) == i);

  // Unify {0,1} and {2,3}.
  uf.unify(0, 1);
  uf.unify(2, 3);
  CHECK(uf.find(0) == uf.find(1));
  CHECK(uf.find(2) == uf.find(3));
  CHECK(uf.find(0) != uf.find(2));
  // 4 and 5 still singletons.
  CHECK(uf.find(4) != uf.find(0));
  CHECK(uf.find(5) != uf.find(0));

  // Unify the two pairs.
  uf.unify(1, 3);
  CHECK(uf.find(0) == uf.find(2));
  CHECK(uf.find(1) == uf.find(3));
  CHECK(uf.find(0) == uf.find(3));

  // 4 and 5 still separate.
  CHECK(uf.find(4) != uf.find(0));
  CHECK(uf.find(5) != uf.find(0));
  CHECK(uf.find(4) != uf.find(5));

  // Merge everything.
  uf.unify(0, 4);
  uf.unify(4, 5);
  for (int i = 0; i < 6; i++) CHECK(uf.find(i) == uf.find(0));

  // Path compression: find() returns same root after repeated calls.
  int r = uf.find(3);
  CHECK(uf.find(3) == r);
  CHECK(uf.find(5) == r);

  // size() can grow: new elements are singletons.
  uf.size(8);
  CHECK(uf.n == 8);
  CHECK(uf.find(6) == 6);
  CHECK(uf.find(7) == 7);
  CHECK(uf.find(6) != uf.find(0));
  return 0;
}
UNIT_TEST_FUN(union_find_test);
