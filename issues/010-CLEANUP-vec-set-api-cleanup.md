# Issue 010: split `Vec`'s two roles (array vs open-addressed set) into two types

**Status:** open, CLEANUP. No behaviour change is intended. Rewritten
2026-09-28; the full design sketch and the instrumentation writeup are in
git: `git show 3f36072b:ifa/issues/010-CLEANUP-vec-set-api-cleanup.md`.
The reproducibility half that used to live here (closed issues/021's
unspecialized pointer-keyed sets) moved to
[147](147-analysis-result-depends-on-the-binary-not-the-inputs.md), where
it is a correctness concern.

## The smell

`plib`'s `Vec<C, A, S>` is a dense array until `set_add()` is called, and
then it becomes an open-addressed hash set. So:

1. **`Vec::n` is the table CAPACITY in set mode**, not the live count
   (`set_count()`). Reading `.n` on a set is a measurement bug. That is
   exactly closed/009 (10 sites in `fa.cc`), and the API still invites it.
2. **Iterating a set-mode `Vec` yields its empty slots as nulls.** 039
   segfaulted on 3 tests keying `v->sym` over a `set_add`-built `Vec`.
3. **`qsort_by_id` sorts in place** (17 sites in `fa.cc`), leaving the
   set permanently sorted as a side effect. `sorted_view(s)` is the
   non-mutating replacement.

## The fix: `BaseVecSet` + `Vec` + `Set`

A non-virtual shared base owns storage and growth. `Vec` exposes only the
positional API (`add`, `operator[]`, `sort`, …). `Set` exposes only
membership (`set_add`, `set_in`, `set_union`, `set_count`, …). Then
`is_vec()`/`is_set()` become the static type, and misuse is a compile
error. The core idiom (build with `set_add`, then compact in place with
`set_to_vec()`: 50+ sites) becomes a free function
`Vec<C> drain_to_vec(Set<C> &)`. Each site needs a new variable, so it is
mechanical but not a sed script.

**Scale:** 475 `set_add`/`set_in` sites in 27 files, 50+ `set_to_vec`
sites. **Not a performance change:** `IFA_VEC_STATS` measured 97.5% of
sets never leaving the ≤4 linear-scan path, so a different hash set would
optimise a cold path.

## Migration (incremental; each step byte-identical output)

1. Add the new types to `vec.h`, unused.
2. Pilot in `ifa/optimize/` (`dom.cc`'s `front.set_to_vec()` is the
   idiom).
3. File by file, `analysis/fa.cc` last.
4. When no `Vec` has `set_*` calls left, drop them from `Vec`. The split
   is the rename.
5. Replace the 17 `qsort_by_id` sites with `sorted_view`. Goldens are
   locked to strict id order, so check each shift.

Before step 2, check whether any single object interleaves `add()` and
`set_add()` over its lifetime.

## Verification

At every step: `make test` byte-identical, `ifa --test`, every `ifa-test`
phase. Exit criterion: the compiler rejects every mixed use.
