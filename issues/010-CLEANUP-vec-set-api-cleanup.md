# Issue 010: Vec-as-set API cleanup and qsort_by_id migration

**Status:** open — deferred follow-on after options A + B of
[../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md)
landed June 2026.
**Affects:** `ifa/common/vec.h`, all Vec consumers (frontend,
ifa/, codegen), `ifa/analysis/fa.cc` (the 17 explicit
`qsort_by_id` sites).
**Related:** [009-fa-violations-nondeterminism.md](closed/009-fa-violations-nondeterminism.md)
(the surface fix that motivated notes/004), [../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md)
(design discussion of A/B/C, what landed and what's deferred).
[../../issues/021-scope-map-pointer-hash-nondeterminism.md](../../issues/closed/021-scope-map-pointer-hash-nondeterminism.md)
is the same bug class in the pyc frontend's `PycScope::map`, not
touched by this issue's scope (ifa's own `Vec`-as-set). Issue 021's
second investigation (added after a `Var *` `PointerHash` gap was
found and fixed — `Var` was missing from the six types notes/004's
options A+B specialized) confirmed *this* issue's deferred audit is
what's actually blocking pyc build reproducibility: `expr_evaluator.py`
compiled 8 times still produced 8 distinct `.ll` outputs even after
both the `PycScope::map` and `Var *` fixes landed, traced to dozens
of remaining unspecialized `set_add`/`set_in` sites over `PNode *`,
`Dom *`, `CallPoint *`, `MatchCacheEntry *`, `llvm::Value *`, etc.
whose iteration order feeds back into Sym/Var/Fun id-assignment
order during FA cloning. Treat 021's remaining scope as folded into
this issue rather than tracked twice.

## Background

`plib`'s `Vec<C, A, S>` doubles as an open-addressed hash set
when populated via `set_add()`. Two long-standing API smells
remain after options A + B landed:

1. **`Vec::n` is the table capacity, not the live element count.**
   For Vec-in-set mode, the live count is `Vec::set_count()` (or
   the `size_t` returned by walking `Vec::values()`). For
   Vec-in-array mode, capacity equals size. Reading `.n` on a
   Vec-as-set is a measurement bug — exactly the bug issue 009
   surfaced (the closing patch fixed 10 such sites in `fa.cc`,
   but the API smell that made those sites look correct is still
   present everywhere `Vec` is used).

2. **`qsort_by_id` mutates the set in place.** The 17 sites in
   `fa.cc` follow the pattern `qsort_by_id(s); for (x : s) ...`,
   leaving `s` permanently sorted as a side effect. After
   options A + B landed, `s` is already in deterministic
   bucket-by-id order, but the goldens are locked to *strict*
   id-sorted order — so removing the calls would shift goldens.
   `sorted_view(s)` (added alongside option A) is the
   non-mutating replacement.

## What's in scope

This issue covers two cleanup tasks that can land independently:

### Task A — `Vec` API rename

- Rename `Vec::n` → `Vec::capacity` (the underlying table size).
- Add `Vec::size()` as an alias for `set_count()` (the live
  element count). For Vec-as-array, `size() == capacity`.
- Audit every `.n` read across the tree (~1000+ sites):
  - In Vec-as-array contexts: rename to `.size()` (semantic
    no-op).
  - In Vec-as-set contexts that meant "capacity": rename to
    `.capacity` (semantic no-op, rare).
  - In Vec-as-set contexts that should have meant "count":
    rename to `.size()` (this is the bug 009 closed).

Practical migration approach: deprecate `.n` with
`[[deprecated("use .size() or .capacity")]]` (compiles, warns)
to ease the audit; remove the field once warnings are clean.

**Blast radius.** Every `Vec` consumer — thousands of call
sites across `ifa/`, frontend, codegen. Most are trivial renames.
The audit is the bulk of the work; the rename itself is
mechanical.

### Task B — `qsort_by_id` → `sorted_view` migration

After options A + B, iteration over a `set_add`-populated
`Vec<T*>` for an id-bearing T is already deterministic (bucket
order under `c->id` hash). The 17 `qsort_by_id` sites in `fa.cc`
are still load-bearing because they enforce *strict* ascending
id order, which the goldens depend on. Migration plan:

1. Replace each `qsort_by_id(s); for (x : s) ...` with
   `for (x : sorted_view(s)) ...`. The output is identical
   (sorted snapshot); the mutating side-effect on `s` is gone.
2. Walk dependent callers to confirm none observe the post-sort
   state of `s`.
3. Once all sites are converted, delete `qsort_by_id` itself
   (it has no other callers — the search at filing time
   showed only the 17 fa.cc sites plus four in `if1/ast.cc` and
   `if1/pattern.cc` and one in `analysis/clone.cc`).

**Blast radius.** Local to ~22 call sites in 4 files. Goldens
should stay byte-identical (the iteration order is identical;
only the side effect on `s` changes).

## Verification plan

After Task A (API rename):

- `make` clean across whole tree.
- Full `make test` + `./test_pyc` + `./ifa-test` all phases.
- Grep for any remaining `.n` reads — none should exist in
  non-deprecated form.

After Task B (sorted_view migration):

- `make test_pyc` and ifa `make test` byte-identical to
  pre-migration goldens.
- `qsort_by_id` symbol no longer referenced anywhere.

## What this unblocks

- **Future 009-class footguns.** Once Task A lands, any new code
  that reads `.n` on a Vec-as-set is a compile-time error (or
  deprecation warning). The class of bug issue 009 closed
  becomes unreachable.
- **Cleaner fa.cc.** The 17 in-place `qsort_by_id` sites become
  one-line `sorted_view` calls without the mutating side effect.

## Why not do this with options A + B?

Three reasons (per the discussion in notes/004):

1. **Blast radius asymmetry.** Options A + B touched ~6 headers
   and 0 call sites of substance. Task A here touches ~1000+
   sites across the tree. Bundling them would make the A+B
   correctness fix harder to review and bisect.
2. **No motivating bug.** A + B fixed a real non-determinism
   issue (009-class). Task A is "while you're here" cleanup —
   real but not load-bearing. Task B is purely cosmetic after
   A + B (iteration is already deterministic; only the goldens'
   strict-sort expectation makes it observable).
3. **Goldens may shift under Task B.** If any site iterates a
   set without going through `qsort_by_id` and the new bucket
   order happens to differ from the previous accidental order,
   that golden will shift. Wanted to land A + B with goldens
   guaranteed-stable first, then do Task B with full attention
   on any shifts.

## Cardinality instrumentation (2026-08-04): container-swap question settled, empirically

A container-choice discussion prompted by this issue ("would a
standard vector/set library do better than `Vec`?") led to adding
opt-in instrumentation rather than guessing: `ifa_vec_stats_enabled`
(env var `IFA_VEC_STATS`, off by default — a single well-predicted
branch per `add`/`set_add` call, zero behavior/perf change when unset)
records a per-process histogram of "size reached" events, dumped via
one `write()` to `IFA_VEC_STATS_FILE` (default `/tmp/ifa_vec_stats.log`)
at exit. See the comment block in `vec.h` next to the declarations for
the full design (why two separate histograms, why they overlap at
small sizes, the saturating-bucket caveat at the tail).

Measured across the full `test_pyc.py` suite (both backends),
`ifa --test`, and a full shedskin corpus sweep (~1,050 process
invocations, ~212.8M distinct `Vec`-as-array growth episodes and
~35.1M distinct `Vec`-as-set growth episodes):

- **99.01%** of `Vec`-as-array usage never exceeds the embedded
  4-slot inline buffer — never heap-allocates at all.
- **97.51%** of `Vec`-as-set usage never exceeds cardinality 4 —
  never leaves the linear-scan fast path, never touches the
  open-addressing hash table (`PointerHash`, probing, prime-sized
  resize) at all.
- For sets specifically: 63.1% end at exactly 1 member, 24.1% at
  exactly 2, 9.5% at exactly 3 — 96.7% at cardinality ≤3.
- For general `Vec::add` growth, cardinality **2** (not 1) is the
  single most common final size (62.1% vs. 26.1% at exactly 1) — a
  mild surprise relative to the "mostly cardinality 1" prior, but the
  overall shape is the same story.
- A real but tiny tail exists (a handful of structures — likely FA
  worklists/AVar sets — pushing past 1,000+ elements, generating
  16-17M raw insert events each in the corpus sweep) but represents
  well under 0.1% of distinct objects.

**Conclusion this settles:** the open-addressing hash-table code path
in `Vec::set_add_internal` — the part a "better" standard hash-set
implementation (Abseil's swisstable, LLVM's `DenseSet`, etc.) would
actually improve on — is cold: ~97.5% of all `Vec`-as-set instances
never reach it. Swapping containers would optimize a path the vast
majority of usage never touches, while the *actual* remaining
problems here (Task A/B above) are API-clarity and correctness
issues a container swap doesn't fix for free either way. Reinforces:
this issue's existing scope (rename + `sorted_view` migration) is the
right-sized fix; a container replacement is not warranted by the
measured workload. Instrumentation kept in the tree (not reverted)
as a cheap, permanent, opt-in tool for re-measuring after any future
change that might shift this distribution.

## Design sketch (2026-08-04): `BaseVecSet` + `Vec` + `Set` — option C, revisited

Raised as a stronger alternative to Task A: instead of *renaming*
`.n` to make the capacity-vs-count footgun compile-error-detectable
(Task A), *split the type* so array-mode and set-mode aren't the same
class at all — a caller literally cannot call `set_add` on something
declared `Vec`, or `add`/`operator[]` on something declared `Set`.
This is option C from
[../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md),
given a concrete shape: a shared, non-virtual base (`BaseVecSet`)
owning the storage/growth mechanics, with `Vec` and `Set` as sibling
subclasses each exposing only the API for their own role.

### Blast-radius findings (before designing further, so the design
### accounts for the real shape of usage, not a guess)

- **475** `set_add`/`set_in`-family call sites across **27 files** —
  same order of magnitude as Task A's own ~1000+ `.n`-read audit, not
  smaller.
- **~467** distinct `Vec<...>` declarations found by a rough grep
  (undercounts multi-line/nested-template declarations, so treat as a
  floor).
- **The complication that matters most: `set_to_vec()` has 50+ call
  sites** (`fa.cc`, `clone.cc`, `pattern.cc`, `graph.cc`, `cfg.cc`,
  `dom.cc`, `html.cc`, `map.h`, `fun.cc`). This is not an edge case —
  it's a core, idiomatic pattern in this codebase: build a
  deduplicated collection via `set_add`, then compact it *in place*
  into a dense array to iterate/sort/return. `vec_to_set()` (the
  reverse) has a handful of sites too. Any design that makes `Vec`
  and `Set` genuinely separate types has to give this idiom a real,
  first-class replacement — it can't be waved off as rare.
- [issue 010's own instrumentation](#cardinality-instrumentation-2026-08-04-container-swap-question-settled-empirically)
  (added answering a related "would a different container be faster"
  question) already showed the *performance* case for touching this
  area is weak — 97.5% of sets never reach the hash-table code path
  at all. So the motivation for this design is **API clarity and
  compile-time misuse prevention**, not speed. That's a legitimate
  reason to do it, but it changes the priority calculus versus, say,
  the FA-convergence work dominating `ifa/issues/` right now — this
  competes on code-health grounds, not urgency.

### Proposed type structure

```cpp
// Shared storage + mechanics only. No array-only or set-only public
// API lives here. Non-virtual (no vtable — matches Vec's existing
// zero-overhead style; Vec/Set are never used polymorphically through
// a BaseVecSet* the way, say, PycCallbacks is).
template <class C, class A = DefaultAlloc, int S = VEC_INTEGRAL_SHIFT_DEFAULT>
class BaseVecSet : public gc {
 protected:
  int n;      // Vec: live count. Set: table capacity (unchanged meaning
              // from today's Vec::n -- Task A's rename still applies
              // here, now scoped to a class where the ambiguity is at
              // least no longer ALSO an array-vs-set ambiguity).
  int i;      // size index (sets) / reserve (vecs) -- unchanged.
  C *v;
  C e[VEC_INTEGRAL_SIZE];

  BaseVecSet();
  ~BaseVecSet();
  void free();
  void reset();
  void addx();                      // internal growth, shared by both
  C *set_add_internal(C c);         // internal open-addressing insert
  C *set_in_internal(C c);          // internal open-addressing lookup
  void set_expand();
  void move_internal(BaseVecSet &v);
  void copy_internal(const BaseVecSet &v);

 public:
  // Genuinely mode-agnostic operations -- same meaning either way.
  void clear();
  int length() const { return n; }
  int write(int fd);
  int read(int fd);
  ValuesRange values() const;       // hole-skipping iteration; already
                                     // safe for both modes today
};

template <class C, class A = DefaultAlloc, int S = VEC_INTEGRAL_SHIFT_DEFAULT>
class Vec : public BaseVecSet<C, A, S> {
 public:
  // Sequential/positional API -- everything that assumes no holes and
  // a meaningful index: add, add(), push, insert, remove_index,
  // remove, operator[], get, first, last, pop, reverse, qsort/sort,
  // fill, append, prepend, index, in, add_exclusive, begin, end,
  // copy, move, operator=.
};

template <class C, class A = DefaultAlloc, int S = VEC_INTEGRAL_SHIFT_DEFAULT>
class Set : public BaseVecSet<C, A, S> {
 public:
  // Dedup/membership API: set_add, set_in, set_remove, set_union,
  // set_intersection (both overloads), some_intersection,
  // some_disjunction, some_difference, set_disjunction,
  // set_difference, set_count, first_in_set, set_clear, copy, move,
  // operator=.
};
```

`is_vec()`/`is_set()` (today's *runtime* predicates, checked by
reading `i`/`n` state) become unnecessary and should simply not exist
on the new types — which class a given object is IS the answer, known
at compile time, everywhere. That's the main compile-time-safety win
made concrete.

### The conversion problem, resolved: free functions, not methods

Mirrors the existing `sorted_view` precedent (`ifa/analysis/fa.h`) —
a free function template that builds and returns a *different*
`Vec`-family type by value, rather than a same-type in-place mutation:

```cpp
// Replaces Vec::set_to_vec(). Compacts s's live members into a dense
// Vec (stable order is NOT guaranteed, same as today's set_to_vec --
// callers that need a specific order already go through sorted_view
// or qsort_by_id separately). Leaves s empty, mirroring the
// "consuming transfer" feel of Vec::move() elsewhere in this file.
template <class C, class A, int S>
Vec<C, A, S> drain_to_vec(Set<C, A, S> &s);

// Replaces Vec::vec_to_set(). Moves v's elements into a new Set via
// set_add (deduplicating). Leaves v empty.
template <class C, class A, int S>
Set<C, A, S> drain_to_set(Vec<C, A, S> &v);
```

**Migration cost per call site, honestly stated:** this is *not* a
one-line rename. Because C++ doesn't allow redeclaring a name with a
different type in the same scope, `x.set_to_vec();` becomes something
like:

```cpp
// before:
Set<T> x;
... x.set_add(...) ...
x.set_to_vec();
... x.v[i] / x.n used as a dense array from here on ...

// after:
Set<T> x;
... x.set_add(...) ...
Vec<T> x_vec = drain_to_vec(x);
... x_vec[i] / x_vec.n used from here on ...
```

i.e. a new variable plus a rename of every downstream use in that
scope — bounded and mechanical per site, but real work multiplied
across 50+ sites, not a sed script.

### What moves where — full placement, so the audit is checkable against this table

| Stays on `BaseVecSet` (protected/shared) | Moves to `Vec` | Moves to `Set` |
|---|---|---|
| storage (`v`, `n`, `i`, `e[]`), `free`, `reset`, `clear`, `addx`, `set_add_internal`, `set_in_internal`, `set_expand`, `move_internal`, `copy_internal`, `write`, `read`, `values()`, `length()` | `add`, `add()`, `push`, `insert` (both overloads), `remove_index`, `remove`, `operator[]`, `get`, `first`, `last`, `pop`, `reverse`, `qsort`, `fill`, `append`, `prepend`, `index`, `in`, `add_exclusive`, `begin`, `end`, `copy`, `move`, `operator=` | `set_add`, `set_in`, `set_remove`, `set_union`, `set_intersection` ×2, `some_intersection`, `some_disjunction`, `some_difference`, `set_disjunction`, `set_difference`, `set_count`, `first_in_set`, `set_clear`, `copy`, `move`, `operator=` |

`copy`/`move`/`operator=` appear on both `Vec` and `Set` because their
*implementation* differs by mode (array copy vs. set copy_internal's
capacity-aware path) even though the shape is the same — each just
calls the shared `copy_internal`/`move_internal` on `BaseVecSet`.

`Accum<C,A,S>` (`vec.h`) becomes the motivating "this was always two
different things" example: `Vec<C> asvec; Vec<C> asset;` today reads
as two of the same type; under this design it's honestly
`Vec<C> asvec; Set<C> asset;` — no `.n`-vs-count ambiguity left to
even audit there.

### Migration strategy

Given the scale (475 call sites, 50+ conversions needing real design
attention), land this incrementally, not as one sweep:

1. **Land `BaseVecSet`/`Vec`/`Set`/`drain_to_vec`/`drain_to_set` as new,
   additive types** in `vec.h`, alongside the existing `Vec` (kept
   as-is, unrenamed, so nothing breaks). Zero migration in this step —
   pure addition, verified by `make` + full suite staying green.
2. **Pilot in one subsystem.** `ifa/optimize/` is a reasonable first
   target — smaller than `analysis/fa.cc` (which has the bulk of the
   475 sites and the majority of `set_to_vec` call sites, so it's the
   highest-risk, do-it-last file, not the pilot), self-contained, and
   already exercises both roles (`dom.cc`'s `front.set_to_vec()` is
   exactly the idiom that needs the `drain_to_vec` treatment).
   Gate: full suite green, `./ifa --test` green, no golden-output
   drift (this is a pure type change, output must be byte-identical).
3. **Expand file-by-file**, `analysis/fa.cc` last (highest call-site
   density, highest risk of an ambiguous "is this really a set"
   judgment call, and where the FA-convergence work already in flight
   elsewhere in `ifa/issues/` makes merge conflicts most likely — best
   to sequence after that work settles, not race it).
4. **Once every `set_add`/`set_in` site's declaration has moved to
   `Set`**, the old dual-mode `Vec` collapses to *be* the new `Vec`
   (drop the now-dead `set_*` methods from it) — no separate rename
   step needed, the split *is* the rename.

Each step's own regression gate: full `test_pyc.py` (both backends) +
`ifa --test` + `ifa`'s own `make test` phases, byte-identical output
(this is a pure refactor, no behavior is supposed to change anywhere).

### Open questions / risks

- **Is any single object genuinely used with *interleaved* array and
  set operations** (not sequential-via-`set_to_vec`, but literally
  alternating `add()`/`set_add()` calls on the same instance over its
  lifetime)? Not checked yet — if this exists anywhere, that call site
  needs its own design (probably: it was relying on set semantics
  throughout and the `add()` calls are either a latent bug or provably
  only run before any `set_add()`, i.e. same idiom as `set_to_vec`
  read backward). Worth a targeted grep-and-read pass before Step 2.
- **Compile-time/binary-size cost** of the extra template
  instantiation layer — expected small (no vtable, same code shape,
  just split across two classes instead of one), but not measured.
- **`reserve()`/`fill()`** are Vec-only in the table above but touch
  fields (`i`, capacity growth) that overlap with `Set`'s use of `i` as
  a size *index* rather than a reserve count — double-check
  `BaseVecSet`'s protected fields don't let a `Vec`-only method
  accidentally corrupt a `Set`'s invariants if some shared internal
  helper is reused incorrectly during implementation.
- Naming: `Set` and `BaseVecSet` don't collide with anything in the
  tree today (checked) — but `Set` is a common enough name that it's
  worth a final check against any in-flight branches before landing.

### Verification plan

1. Step 1 (additive) lands with zero output diff anywhere — pure new
   code, nothing calls it yet.
2. Pilot subsystem (Step 2): full suite + `ifa --test` byte-identical
   before/after; specifically re-run the `dom.cc`/`front.set_to_vec()`
   path (dominator-frontier computation) since it's exactly the
   `drain_to_vec` idiom and touches FA-adjacent correctness.
3. Before declaring any file "migrated," grep it for `set_add`,
   `set_in`, `set_to_vec`, `vec_to_set`, and `.n` reads and confirm
   every one now resolves against the correct type (compiler enforces
   most of this automatically — a `Vec` with no `set_*` calls left and
   a `Set` with no positional calls left is the exit criterion, and a
   failed build is the audit).

## See also

- [../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md)
  — design discussion of A/B/C and what landed.
- [009-fa-violations-nondeterminism.md](closed/009-fa-violations-nondeterminism.md)
  — the bug that surfaced the capacity-vs-count footgun.
- `ifa/common/vec.h` — the `Vec` definition.
- `ifa/analysis/fa.h` — `qsort_by_id` and `sorted_view`
  templates.
