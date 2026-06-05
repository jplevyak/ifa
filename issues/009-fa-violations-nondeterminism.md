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
   orders. If `type_violation()` (`fa.cc:1369` as of June 2026)
   is called from such iteration with
   `type_violation_hash.put(v)` deduping, the order in which
   violations are observed affects how many get collapsed by
   the hash's equality check.

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

Six steps, each landable independently. Steps 1–4 are the surface
fix; step 5 is the regression test; step 6 files the deeper plib
follow-up. Current file:line references are as of June 2026 —
expect drift.

### Step 1 — Recover the reproducer and confirm the alternation

The `nested_iterator` fixture and its `IRShape` builder were
deleted in commit `a6e98e2` and are recoverable from `a6e98e2^`:
- `ifa/tests/synthetic/nested_iterator.synth` (7 lines).
- The `nested_iterator` function and registry entry in
  `ifa/testing/ir_shapes.cc`.
- The `ir_shapes.h` declaration.

Restore them on a throwaway branch. Loop
`./ifa-test --phase fa-converge nested_iterator` 20 times and
confirm the documented alternation. The fa-converge printer
omits the violation count by default since this issue was
filed; gate the field on `IFA_PRINT_VIOLATIONS=1` and re-enable
it for the run.

**Output:** confirmation the symptom reproduces in HEAD; recorded
both alternating violation counts for use as Step 2's diff
oracles.

#### Step 1 results (recorded 2026-06-05)

Restored on the working branch (cleanup-tier-3 era). The
`IFA_PRINT_VIOLATIONS=1` env-var gate was added to
`testing/print_fa_converge.cc` so the violations field can be
re-emitted on demand without disturbing the default goldens.

20 standalone runs of
`IFA_PRINT_VIOLATIONS=1 ./ifa-test --rebless --phase fa-converge nested_iterator`:

| Pattern (per-pass `violations_after`) | Count |
|---------------------------------------|-------|
| `13, 13, 13`                          | 15 (75%) |
| `13, 31, 31`                          | 5 (25%) |

Sharper observations than the original "13 vs 31" framing:

- **Pass 1 always records 13 violations.** Deterministic.
  Whatever produces violations in pass 1 is order-independent.
- **Pass 2 records either +0 or +18 new violations.** This is
  where the non-determinism enters. Pass 2's transfer-function
  pass adds 18 violations under one iteration order and zero
  under the other.
- **Pass 3 inherits the divergence** (it adds zero in both
  branches), so the final count is whatever pass 2 settled on.
- **No standalone crash** observed in 20 runs. This matches
  issue 008's claim that the crash only fires when
  `nested_iterator` runs alongside other fixtures via
  `make test-ir`.
- **No third bucket** — exactly two outcomes. This suggests a
  single binary iteration-order decision somewhere in pass 2,
  not a fan-out of multiple non-deterministic sites.

**Implication for Step 2:** find what pass 2's `extend_analysis`
does that records exactly 18 violations or zero, depending on
iteration order. 18 is a chunky number — likely one loop touching
18 AVars, all flagged under one ordering, none under the other.
That sharpens the instrumentation target: log every
`type_violation()` call with the current `analysis_pass`, then
diff the pass-2 slices of two runs against each other; the run
with 18 extra calls in pass 2 names the iteration site.

### Step 2 — Instrument `type_violation` to log every call

Add a one-shot debug print at `fa.cc:1369` (the entry to
`type_violation`), gated on an env var so it doesn't ship enabled:

```cpp
if (getenv("IFA_DEBUG_VIOLATIONS")) {
  fprintf(stderr, "violation kind=%d av=%d send=%d type_hash=%u\n",
          (int)akind, av->id, send ? send->id : 0, type->hash);
}
```

Capture two runs (one producing the lower count, one producing the
higher) into two log files. Diff them. The first differing line is
the divergence point.

**Expected outcomes:**

- **A.** Logs differ in the *sequence* of `(kind, av, send)`
  triples but the *set* is identical. Then the count is the same
  and the alternation is from somewhere else — back to the drawing
  board.
- **B.** Logs differ in the *set* of triples (likely per the
  AUDIT's analysis at §3.2). The high-count log has the extra
  triples coming from a single iteration that visits AVars in a
  different order from the low-count log.

In case B, walk the call stack upward from the divergence point:
who called `type_violation`? Which loop is that caller inside?
What is the iteration source?

**Output:** the offending iteration site (likely
`collect_argument_type_violations`'s `actuals` loop at
`fa.cc:2652`, or one of the per-AVar loops downstream of
`from->out_edges` / `from->out_edge_map`).

### Step 3 — Sort the offending iteration by id

Once Step 2 names the site, prepend `qsort_by_id(...)` before the
loop. For `collect_argument_type_violations`:

```cpp
for (AEdge *me : *m) {
  if (!from->out_edges.set_in(me)) continue;
  form_MPositionAVar(x, me->args) if (x->key->is_positional()) actuals.set_add(x->value);
}
qsort_by_id(actuals);     // <-- NEW
for (AVar *av : actuals) if (av) { ... }
```

If Step 2 also implicates the `Vec<AEdge *> *m = ... out_edge_map.get(p)`
iteration, sort a copy:

```cpp
Vec<AEdge *> ms;
ms.copy(*m);
qsort_by_id(ms);
for (AEdge *me : ms) { ... }
```

There may be more than one site — Step 2's stack walk catches
them. Each `qsort_by_id` insertion is one line and behaviorally
trivial: the loop now visits AVars/edges by stable, allocation-
order-independent id.

`fa.cc` already has 17 `qsort_by_id` sites doing exactly this for
the same reason; the AUDIT §3.3 wants this to become a
code-review checklist item.

**Output:** N call sites annotated; ~N–5N lines of diff total.

### Step 4 — Re-run the 20× test, restore the printer field

```bash
for i in $(seq 1 20); do
  ./ifa-test --phase fa-converge nested_iterator | grep violations=
done
```

All 20 lines should agree. If they don't, Step 2 missed an
iteration source — go back.

Once stable, also run **all** fa-converge fixtures 20× — the
AUDIT notes that other goldens "appear deterministic in practice
but might also be affected at lower probability." Hunt any others
the same way.

Then: restore the `violations=X→Y` field in
`testing/print_fa_converge.cc` and re-bless all `fa-converge`
goldens. Note that `qsort_by_id` insertions can shift the *number*
of passes by one even on fixtures that weren't previously
alternating — the convergence theorem says the final state is the
same, but the work inside a pass is reordered. That's expected and
benign; re-bless those goldens too.

**Output:** restored printer field, re-blessed goldens, locked-in
determinism for the violation counts.

### Step 5 — Add regression tests

Two regression tests, both small:

**5a — Unit test in `ifa/testing/lattice_test.cc`.** A focused test
on `type_violation()`'s dedup invariance:

> Given the same `(kind, av, send)` triples in two different
> orders, `type_violations` ends up with the same size.

Requires more setup than the existing lattice tests (real AVars,
not just synthetic ATypes), but it's the natural home and would
catch any regression where someone changes the
`type_violation_hash` equality / hash without realizing.

**5b — Restore `nested_iterator.synth` to `ifa/tests/synthetic/`.**
Run it 20× as part of the harness. The existing test harness diffs
golden output; if the violation count is stable, the diff is
clean. If issue 008 (the crash) was indeed the same root cause —
likely per AUDIT §3.5 — this also restores synthetic coverage of
the violation stage that was lost to issue 007's coverage gap.

**Output:** lattice unit test for dedup invariance + restored
fixture + harness run 20× clean.

### Step 6 — File the deeper plib fix as a follow-up note

The fundamental fix described in AUDIT §3.4 is to replace
`Vec::set_add_internal` (`ifa/common/vec.h:380`) `(uintptr_t)c % n`
hashing with **content-based** hashing (`c->id`-based, or a
separate ChainHash). That change touches all of plib and every
`Vec`-as-set call site — it's the right long-term fix but
disproportionate for closing 009.

We close 009 with the surface fix (Steps 1–5). For the deeper fix,
write `ifa/notes/004-plib-vec-pointer-set-hashing.md` capturing:

- the AUDIT §3.4 reasoning,
- why we didn't do it as part of 009 (cross-cutting blast radius),
- what we'd do (replacement scheme, migration path),
- what depends on it (any future "we got bitten by pointer-set
  iteration order *again*" debugging).

This mirrors how CDB and eager-splitting were handled in tier 1:
cleanup landed, intent for the larger follow-up preserved as a
note. The note can graduate to a numbered `ifa/issues/` file if
someone decides to take it on.

**Output:** new note 004, ready to graduate to an issue when
someone wants it.

## What this unblocks

- **Issue 008** very likely closes as a side effect (same root
  cause hypothesis from AUDIT §3.5). If not, the
  `nested_iterator` restoration in Step 5b will produce a
  *deterministic* crash — much easier to debug than the current
  intermittent one.
- **Issue 007** gets its violation-stage synthetic coverage back.
- **Reproducible builds for pyc.** Today FA's choices technically
  vary across builds; in practice pyc programs don't trigger the
  violation stage, but the latent bug exists. Closing 009 closes
  the reproducibility hole.
- **Restored printer diagnostic.** `violations=X→Y` field comes
  back, useful for debugging splitter behavior.
- **Tier 3 (the big reentrancy work)** becomes safe to attempt —
  its validation depends on stable goldens. AUDIT §10's last
  paragraph explicitly says this: *"The work above does not
  require any of the larger refactors in §2 or §4. Save those for
  after 009 is closed."*

## Risks / things to watch

1. **The actual divergence point may not be where the AUDIT
   guesses.** Step 2's instrumentation is the truth source. If it
   points somewhere unexpected (e.g. `function_dispatch`'s
   `matches` ordering, which depends on the pattern library
   *outside* this directory in `ifa/if1/pattern.cc`), we may need
   a different fix path — possibly an issue file against that
   library.

2. **Other shapes may also be affected at lower probability.** The
   AUDIT notes other goldens "appear deterministic in practice but
   might also be at lower probability." Step 4's 20× run on all
   fixtures catches them.

3. **`qsort_by_id` insertions can slightly change splitter
   behavior.** Sorting reorders the work inside a pass. The
   convergence theorem says the final state is the same, but the
   *number of passes* could shift by one. Be ready to re-bless
   goldens for that reason even on fixtures that weren't
   previously alternating — that's expected and benign.

4. **The 008 crash may have a separate root cause.** AUDIT §3.5
   names a plausible mechanism (`clear_avar` at `fa.cc:3246`
   doesn't evict the AVar from pointer-set caches; next-pass
   iteration may chase a stale field). If Step 5b's restored
   fixture still crashes after Steps 3–4, treat 008 as a separate
   problem and split out the work — don't conflate them.

## Effort estimate

- Step 1: ~1 hour (recover fixture, confirm).
- Step 2: 2–4 hours (instrument, capture, diff, walk the stack).
- Step 3: 1 hour (add the `qsort_by_id` calls).
- Step 4: 1 hour (run 20×, re-bless goldens).
- Step 5: 2–3 hours (unit test + restored fixture).
- Step 6: 1 hour (write the note).

**Total: ~one focused day** if the AUDIT's hypothesis is right.
Multiple days if Step 2 points somewhere the AUDIT didn't
anticipate.

## Why deferred

- Investigation requires debugger / careful FA reading. Unbounded
  scope for what was already a stretch goal in Phase 09.
- Workaround is in place (printer doesn't emit the unstable
  field); no test failures with current fixture set.
- Pyc's existing test suite is unaffected by the symptom in
  practice (the violation stage never fires in pyc tests, per
  issue 007's recon).
