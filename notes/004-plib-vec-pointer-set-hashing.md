# 004 — `Vec::set_add_internal` pointer-bucket hashing

plib's `Vec<C *>` doubles as an open-addressed hash set when
populated via `set_add()`. Its bucket index is computed as
`(uintptr_t)c % n`, so for pointer keys the distribution depends
entirely on GC-allocated addresses — different across runs, even
on identical input. The AUDIT (§3.4) flagged this as the deeper
fix worth filing as cross-cutting follow-up; this note captures
what's known, what 009 revealed about its actual manifestation
profile, and what the deeper fix would look like.

## What the code does

`ifa/common/vec.h:380` (verbatim, current as of June 2026):

```cpp
template <class C, class A, int S>
C *Vec<C, A, S>::set_add_internal(C c) {
  int j, k;
  if (n) {
    uintptr_t h = (uintptr_t)c;
    h = h % n;
    for (k = h, j = 0; j < i + 3; j++) {
      if (!v[k]) {
        v[k] = c;
        return &v[k];
      } else if (v[k] == c)
        return 0;
      k = (k + open_hash_primes[j]) % n;
    }
  }
  Vec<C, A, S> vv;
  vv.move_internal(*this);
  set_expand();
  if (vv.v) set_union(vv);
  return set_add(c);
}
```

`set_in_internal` (line 401) uses the same probe scheme. The
expansion sequence comes from `prime2` (`ifa/common/vec.cc:6`):
`{7, 13, 31, 61, 127, 251, 509, ...}` — initial size `prime2[2] = 7`,
then up to ~25% load before triggering `set_expand`.

Two observable effects flow from `(uintptr_t)c` driving the
bucket:

- **Iteration order over a populated set is non-deterministic
  across runs.** `for (T *x : my_set)` walks `v[0..n-1]` in
  storage order, which is bucket-order, which depends on the
  pointer values, which depend on GC heap layout.
- **Table capacity (the `.n` field) is non-deterministic.**
  Whether 11 inserts fit in a 13-slot table without triggering
  `set_expand` depends on whether the probe chains all land in
  empty slots within `i+3` hops — a function of the pointer
  values' home buckets and collision patterns.

## How 009 manifested the bug — and how it *didn't*

[Issue 009](../issues/009-fa-violations-nondeterminism.md)
investigated alternating violation counts (13 vs 31 across runs)
for the `nested_iterator` shape. The AUDIT hypothesis was that
the *iteration-order* effect was the cause: a loop somewhere in
`extend_analysis` was visiting AVars in different orders across
runs, recording different sets of violations.

That hypothesis turned out to be wrong for this fixture. The
analysis is fully deterministic — the same 11 unique
`(kind, av, send)` triples are recorded in every run. What
varied was just the *table capacity* of the `type_violations`
set: 11 inserts sometimes fit in 13 slots, sometimes triggered
expansion to 31, and the printer was reading `.n` (capacity)
instead of `.set_count()` (live count). Closing 009 was a
one-line-per-site fix at every `.n` read site (~10 sites in
`fa.cc`).

So 009 surfaced *one* effect of pointer-bucket hashing (the
capacity oscillation) but not the *other* (iteration-order
non-determinism). That doesn't mean the iteration-order effect
is theoretical:

- A scan of all 17 fa-converge fixtures showed 9 of them mis-
  reported `.n` consistently (not just nested_iterator's
  visible alternation). Some had `set_count=8` reported as
  `.n=13`, etc. — same bug, different probability mass.
- For *iteration order*: the codebase has been pragmatically
  containing this for years. `fa.cc` alone has 17 `qsort_by_id`
  call sites whose only purpose is to normalize iteration order
  after a `set_add`-populated `Vec` is about to be walked. The
  AUDIT §3.3 wanted "any `Vec<Ptr*>` constructed by
  `set_add`/`set_union` and then iterated must call
  `qsort_by_id` before iteration unless the loop body is
  pointer-order-independent" to become a code-review checklist
  item.

Total `set_add` / `set_in` call sites in the ifa core
(`analysis/`, `if1/`, `optimize/`): ~244 as of June 2026, of
which ~110 are in `analysis/`. Every one is a potential
non-determinism source if the immediately-following iteration
loop isn't either order-insensitive or explicitly sorted. The
17 explicit `qsort_by_id` calls in `fa.cc` show the discipline;
the 200+ unsorted sites are either provably order-insensitive
or latent.

## Why we didn't do the deeper fix as part of 009

Three reasons:

1. **Cross-cutting blast radius.** `Vec<C *>` is templated and
   used throughout plib (`common/`), `if1/`, `analysis/`,
   `optimize/`, `codegen/`, and the pyc frontend. Changing the
   hash function would touch all 244 call sites' behavior
   (silently, since the API stays identical) and could shake
   loose latent bugs in unrelated subsystems. That's a
   multi-week project, not a step in closing 009.

2. **009 doesn't actually need it.** The closing fix for 009
   was `.n` → `.set_count()`. That made the printer correct
   regardless of bucket distribution. The deeper fix is for
   *future* bugs in iteration-order paths, not for 009 itself.

3. **The pragmatic workaround already exists.** The 17
   `qsort_by_id` sites in `fa.cc` (and similar discipline in
   other subsystems) demonstrate that the convention "sort
   before iterating an unordered set" is well-understood and
   applied where it matters. The deeper fix would be cleaner
   but isn't blocking anything specific.

## What the deeper fix would look like

Three plausible shapes, ordered by increasing invasiveness:

### Option A — keep the data structure, add an iteration helper

Add a `Vec<C *>::sorted_view()` that returns a sorted snapshot
for iteration. Convention becomes "if you `set_add`, you iterate
via `sorted_view`." Most existing `qsort_by_id` sites become
single-call. Capacity-vs-count is still a footgun unless
`.set_count()` is used everywhere (we've fixed it in `fa.cc`'s
violation-reporting sites but not elsewhere).

Migration: ~17 sites in `fa.cc` become cleaner; other places
adopt as needed. No behavior change.

### Option B — replace `(uintptr_t)c % n` with content-based hashing

For pointer types with a stable `id` field, hash on `c->id`
instead. Most IFA pointer types qualify: `AVar`, `AEdge`,
`CreationSet`, `EntrySet`, `Fun`, `Sym` all have monotonic
`int id` fields assigned at construction. The hash function
becomes a template specialization or a trait:

```cpp
template <class C> struct PointerHash { static size_t hash(C *c); };
template <> struct PointerHash<AVar> { static size_t hash(AVar *c) { return c->id; } };
// ... and similarly for the other id-bearing types
```

`set_add_internal` reads `PointerHash<C>::hash(c)` instead of
`(uintptr_t)c`. Pointer-typed `Vec`s for non-id types fall back
to the current `(uintptr_t)c` mixer.

Migration: schema change, but the API stays identical. Iteration
order becomes id-order (predictable). Capacity oscillation goes
away because hash inputs are stable across runs.

This is what AUDIT §3.4 sketched: "replace `(uintptr_t)c % n`
with content-based hashing or chain by stable id."

### Option C — separate `IdSet<T>` container

Introduce a dedicated `IdSet<C *>` that's not a `Vec` at all —
maybe a sorted `Vec<C *>` with binary-search membership, or a
`Map<int, C *>` keyed by id. Migrate `set_add`-style call sites
to it where order-determinism matters. Leaves `Vec<C *>::set_add`
alone for places that genuinely don't care about iteration.

Most invasive but most principled. The current ambiguity —
"Vec doubles as set" — is the root issue; option C names the
two roles separately.

## Migration notes (for either B or C)

- The 17 explicit `qsort_by_id` sites can be deleted once the
  underlying iteration is id-stable. Each is a one-line
  deletion.
- Every site that reads `.n` on a Vec-as-set is a measurement
  bug per 009's discovery. Sweep for them
  (`grep -rn '\.n\b' | grep -v 'sorted\.n\|something_else\.n'`)
  and convert to `.set_count()`. Most are already in
  `analysis/fa.cc` and were fixed for 009.
- The `set_count()` vs `.n` confusion is itself an API smell.
  Renaming `.n` to `.capacity` (and adding `.size` as an alias
  for `.set_count()`) would make the wrong reads compile-error
  detectable. Cross-cutting; defer until someone takes on
  option B or C.

## What depends on this

- **Future 009-like bugs.** Any new code that iterates a
  `set_add`-populated `Vec` and makes decisions based on
  iteration order can produce non-deterministic output. The
  `qsort_by_id` convention is the load-bearing workaround.
- **Tier 3 reentrancy work** (CLEANUP.md tier 3 items 2-7). If
  multiple FA instances run concurrently, their per-instance
  `Vec`-as-set tables will diverge in capacity and iteration
  order; the deeper fix removes that variable.
- **Reproducible-build claims for pyc.** Pyc compilations
  don't currently surface this because the violation stage
  rarely fires in production code, but if a pyc program ever
  *does* exercise a non-deterministic iteration path, output
  drift is possible.

## See also

- [../analysis/AUDIT.md §3.4](../analysis/AUDIT.md) — the
  original "deeper fix" sketch.
- [../analysis/AUDIT.md §3.3](../analysis/AUDIT.md) — the
  "sort by id before iterating" convention.
- [../issues/009-fa-violations-nondeterminism.md](../issues/009-fa-violations-nondeterminism.md)
  — the investigation that surfaced the capacity-reporting
  effect but not the iteration-order effect.
- `ifa/common/vec.h:380` — `set_add_internal` implementation.
- `ifa/common/vec.cc:6` — `prime2` expansion sequence.

## What landed (June 2026): options A + B

Both option A and option B from this note landed together as a
single bundle:

**Option B** — `ifa/common/vec.h` now declares a
`PointerHash<C>` trait whose primary template still returns
`(uintptr_t)c` (so non-pointer `Vec<T>` users and pointer
`Vec<T*>` users without an id-bearing pointee see no behavior
change). `set_add_internal` and `set_in_internal` index via
`PointerHash<C>::hash(c)`. Explicit specializations on `c->id`
are provided in the headers that own the six id-bearing pointer
types:

- `ifa/analysis/fa.h` — `AVar`, `AEdge`, `EntrySet`, `CreationSet`
- `ifa/if1/sym.h` — `Sym` (via inherited `BasicSym::id`)
- `ifa/if1/fun.h` — `Fun`

The specializations live in the same header that defines each
type, so any TU that uses `Vec<T*>::set_add` for an id-bearing T
has already seen the specialization (it includes the type's
header). The note in `vec.h` documents this co-location
requirement.

**Option A** — `ifa/analysis/fa.h` adds a free
function template:

```cpp
template <class C>
Vec<C *> sorted_view(const Vec<C *> &v);
```

next to the existing `qsort_by_id`. It returns a sorted-by-id
snapshot, skipping null hash-table holes; leaves the input
untouched. Intended as the non-mutating replacement for the
`qsort_by_id(v); for (x : v) ...` pattern. No call sites migrated
in this PR — see issue 010 for the migration plan.

**Verification.** `fa-converge` is byte-identical across 5+
consecutive runs of every fixture (including `nested_iterator`,
which issue 009 surfaced); `./ifa --test` 52/0; full ifa
`make test` clean across all phases (finalize, freq,
codegen-c, codegen-llvm, inline, fa-converge 17/17); top-level
`make test` + `./test_pyc` 73 pass / 2 expected fail / 0 fail.

**What stayed deferred.** The two follow-ons from this note's
"Migration notes" section:

- The API rename (`Vec::n` → `Vec::capacity`, add `Vec::size`
  as alias for `set_count()`) — makes the capacity-vs-count
  footgun compile-error detectable.
- Migration of the 17 explicit `qsort_by_id` sites in `fa.cc` to
  `sorted_view()` — and eventual deletion of `qsort_by_id`.

Both are now tracked in
[../issues/010-vec-set-api-cleanup.md](../issues/010-vec-set-api-cleanup.md).
The motivating correctness issue (non-determinism) is resolved by
the changes that landed; the deferred items are cleanup, not
fixes.

## History

Filed June 2026 as part of issue 009's Step 6. Options A + B
landed June 2026 (this commit); remaining cleanup moved to
issue 010.
