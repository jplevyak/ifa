# Issue 009: FA violation count is non-deterministic across runs

**Status:** open (worked around by dropping the field from the
`fa-converge` printer's history output; the underlying
non-determinism is unfixed).
**Affects:** `ifa/analysis/fa.cc` — the violation collection /
counting path. Likely related to issue 008 (FA crash on
nested_iterator) — both may share a root cause in iteration
order.
**Workaround landed:** `testing/print_fa_converge.cc` no longer
emits `violations=X→Y` in the per-pass history; all existing
goldens re-blessed without that field. Split counts and ess/css
remain (those are stable).
**Related:** [008-fa-crash-on-nested-iterator-shape.md](008-fa-crash-on-nested-iterator-shape.md),
[007-mark-type-stage-coverage.md](007-mark-type-stage-coverage.md).

## Symptom

For at least one synthetic shape (`nested_iterator`), repeated
runs of the SAME FA invocation produce different values for
`type_violations.n`:

```
$ for i in 1 2 3 4 5; do
    ./ifa-test --phase fa-converge nested_iterator |
      grep "violations=";
  done
pass 3 violation splits=1 ess=10→10 css=29→29 violations=13→13
pass 3 violation splits=1 ess=10→10 css=29→29 violations=31→31
pass 3 violation splits=1 ess=10→10 css=29→29 violations=13→13
pass 3 violation splits=1 ess=10→10 css=29→29 violations=31→31
pass 3 violation splits=1 ess=10→10 css=29→29 violations=13→13
```

The pass count, stage sequence, split counts, ess.n, and css.n
are all stable. Only the violation count alternates — between 13
and 31 in this specific case, with roughly 50/50 probability.

The other synthetic and pyc-suite-derived fa-converge goldens
appear to be deterministic in practice but might also be
affected at lower probability.

## Root cause hypothesis

The "13 vs 31" alternation suggests two consistent outcomes
selected by some non-deterministic predicate. Most likely
candidates:

1. **Hash table iteration order keyed on pointer addresses.**
   IFA's `ChainHash`/`Map` containers (in plib) often iterate
   by bucket order. If the bucket index is `(uintptr_t)key %
   nbuckets`, GC heap allocations between runs produce
   different bucket distributions and different iteration
   orders. If `type_violation()` (`fa.cc:1389`) is called from
   such iteration with `type_violation_hash.put(v)` deduping,
   the order in which violations are observed affects how many
   get collapsed by the hash's equality check.

2. **`set_add` / `Vec` insertion via `pointer_set` that
   silently dedups by pointer identity** — same flavor of bug.
   If a transfer function records violations against AVars whose
   identity changes between runs (different allocation order →
   different pointer values → different set membership), the
   resulting violation count differs.

3. **Order-dependent type_diff / type_union** in `type_violation`:
   ```c
   v->type = type_union(v->type, type);
   ```
   `type_union` should be order-independent, but if its
   implementation builds intermediate ATypes whose identity
   depends on construction order, downstream code that counts
   distinct violations might miscount.

## Why this matters beyond the printer workaround

- **Issue 008 (the crash) is likely the same root cause.**
  Non-deterministic iteration order over a structure where some
  entries are stale or freed would explain BOTH the count
  variation AND the intermittent segfault. Fixing one likely
  fixes the other.

- **Pyc compilations may be silently affected.** If FA's
  violation count is non-deterministic, the violation-stage
  splitter's behavior is too. Pyc programs that *would* trigger
  the violation stage (if any reach it) may produce different
  specialization results across compilations.

- **Test reliability.** We had to drop a useful diagnostic from
  the test golden because of this. Other diagnostics may also
  be quietly unstable; we just haven't noticed them yet.

- **Reproducibility of pyc-produced C code.** If FA's choices
  vary across builds, the generated C code's optimization
  decisions vary too. That's a real surprise for users
  expecting deterministic builds.

## Verification plan

1. **Quick check**: instrument `fa.cc:1389` `type_violation()` to
   log each invocation's `v->av->id` (AVar id) and `type` value.
   Across two runs of the same fixture, compare the logs. If the
   *sequence* of calls differs, that's iteration order. If the
   sequence is the same but the count of unique entries differs
   downstream, that's deduplication order.

2. **Find the iteration source**: grep `fa.cc` for `form_Map(`,
   `for (... :` over ChainHash / pointer_set structures, and
   identify which one feeds `type_violation()` directly or
   indirectly. Likely candidates: `add_send_edges_pnode`'s
   per-CS loops; `dispatch_type` resolution.

3. **Fix**: either sort the iteration by stable id before
   processing, OR change the violation-collection structure to
   be order-independent (e.g., use a sorted vector or a
   hash-by-content rather than hash-by-pointer).

4. **Validate**: re-introduce `nested_iterator.synth`. Run
   `make test-ir` 20× — every run should produce the same
   golden output AND no crash (assuming 008 was indeed a
   downstream symptom).

## What this unblocks

- **Issue 008** likely closes as a side effect.
- The dropped `violations=X→Y` field can be restored to the
  fa-converge printer, giving back the per-pass diagnostic.
- `nested_iterator` (and other "deep" shapes) become safe to
  add to the synthetic test set, restoring violation-stage
  coverage to issue 007.
- Reproducible-build guarantee for pyc compilations.

## Why deferred

- Investigation requires debugger / valgrind / careful FA
  reading. Unbounded scope for what was already a stretch goal
  in Phase 09.
- Workaround is in place (printer doesn't emit the unstable
  field); no test failures with current fixture set.
- Pyc's existing test suite is unaffected by the symptom in
  practice (the violation stage never fires in pyc tests, per
  issue 007's recon).
