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

## See also

- [../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md)
  — design discussion of A/B/C and what landed.
- [009-fa-violations-nondeterminism.md](closed/009-fa-violations-nondeterminism.md)
  — the bug that surfaced the capacity-vs-count footgun.
- `ifa/common/vec.h` — the `Vec` definition.
- `ifa/analysis/fa.h` — `qsort_by_id` and `sorted_view`
  templates.
