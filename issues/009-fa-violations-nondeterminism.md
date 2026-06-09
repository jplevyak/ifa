# Issue 009: FA violation count is non-deterministic across runs

**Status:** **closed** June 2026. Diagnosis was a surprise: the
symptom was a *measurement* bug in the printer, not an
iteration-order non-determinism as the AUDIT had hypothesized.
`type_violations` is a `Vec`-as-set; `.n` is the open-addressed
table capacity (oscillates with allocation order), and
`.set_count()` is the live element count (deterministic). The
fix was a one-line-per-site replacement at ~10 reporting sites
in `fa.cc`. Steps 1-6 of the verification plan all landed; see
`## Status` near the bottom of this file.
**Affects:** `ifa/analysis/fa.cc` — the violation collection /
counting path. Initially assumed related to issue 008 (FA crash
on nested_iterator) via a shared root cause; that turned out not
to be the case — see the Step 4 surprise section for the
observation that 008 stopped reproducing after the 009 fix
(reason unclear, separate investigation needed).
**Related:** [008-fa-crash-on-nested-iterator-shape.md](008-fa-crash-on-nested-iterator-shape.md),
[007-mark-type-stage-coverage.md](007-mark-type-stage-coverage.md),
[../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md)
(deeper plib follow-up).

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

#### Step 2 results (recorded 2026-06-05)

The Step 1 framing turned out to be wrong. Step 2's
instrumentation conclusively showed the analysis is
**deterministic** for this fixture — the symptom is a measurement
bug.

#### Instrumentation

`type_violation()` (`fa.cc:1369`) got a one-line env-gated stderr
trace printing `(analysis_pass, kind, av->id, send->id, type->hash)`.
A companion stderr line was added to `record_fa_event()` printing
both `type_violations.n` and `type_violations.set_count()` at
event-record time, so the discrepancy was visible directly.

#### Findings

Across 20 standalone runs:

| Metric                          | Pass 1     | Pass 2     | Pass 3     |
|---------------------------------|------------|------------|------------|
| `set_count()` (live count)      | 13 always  | 11 always  | 11 always  |
| `.n` (table capacity)           | 13 always  | 13 or 31   | 13 or 31   |
| `type_violation()` calls/pass   | 13 always  | 14 always  | 14 always  |
| Unique `(kind,av,send)` triples | 13 always  | 11 always  | 11 always  |

The set of violations recorded is *identical* in every run. Only
the underlying `Vec`-as-set's table capacity oscillates.

#### Why `.n` oscillates

`Vec<C *>` doubles as a pointer-set; `set_add` populates an
open-addressed table whose initial size is `prime2[2] = 7` and
which expands through 13 → 31 → 61 … (see `ifa/common/vec.h:598`,
`ifa/common/vec.cc:6`). In set mode, `Vec::n` is the **table
size** (capacity), not the live element count — that's
`set_count()` (`ifa/common/vec.h:493`).

`set_add_internal` expands when it can't find a slot within `i+3`
probes from the home bucket. With 11 elements and 13 slots, that
"sometimes fits, sometimes doesn't" depending on each pointer's
home bucket — which is `(uintptr_t)c % n`, i.e., depends on
GC-allocated addresses. ~75% of runs fit; ~25% trigger expansion
to 31.

#### What the printer was doing wrong

`record_fa_event()` records
`e->violations_after = type_violations.n` (`fa.cc:115`). Every
`viol0 = type_violations.n` snapshot at the splitter trip-recording
sites (`fa.cc:3720`, `3729`, `3738`, `3743`, `3758`, `3763`,
`3773`) does the same. All of these report **capacity**, not
**count**.

`Vec::clear()` and the per-pass `initialize_pass()` at
`fa.cc:2867` reset the set, which is why each pass's measurement
is independent (cleared → 11 inserts → either fits in cap-13 or
expands to cap-31).

The pass-limit trip log (`fa.cc:3789`) and the return-code check
(`fa.cc:3890`) also read `.n`. The trip log mis-reports the same
way. The return-code check is *coincidentally* correct: `n == 0`
exactly when no violations have been added (which is the live
count too), and `n > 0` is the load-bearing test. Still worth
making consistent.

#### What this means for the rest of the plan

- **Step 3 (sort the offending iteration by id) is not needed for
  this fixture.** There's no iteration-order divergence in the
  analysis. The fix is a one-line correction at each `.n` →
  `.set_count()` site in `fa.cc`.
- **Step 6 (deeper plib pointer-set hashing fix) is not blocked
  by 009.** That AUDIT §3.4 work stands on its own — the
  pointer-bucket hashing is still real, it just doesn't surface
  through `type_violations.n` the way 009 originally guessed.
  Worth a note (the plib hashing scheme dictates capacity-growth
  behavior that the printer was inadvertently probing), but not a
  blocker.
- **Issue 008 (the crash) is NOT explained by this finding.** The
  shared-root-cause hypothesis from AUDIT §3.5 was that count
  alternation and the crash arose from the same iteration-order
  bug. With count alternation now diagnosed as a measurement bug,
  the crash must have a separate cause. Likely candidate per
  AUDIT §3.5: `clear_avar` not evicting AVars from pointer-set
  caches, leading to use-after-clear on multi-fixture runs.
  Investigate when restoring `nested_iterator.synth` to the
  multi-fixture test set.
- **Other shapes may also be affected by the same measurement
  bug** at their own probabilities. Any pass that produces >7
  violations and ≤ next-prime-step elements is a candidate. Worth
  a sweep after the fix lands.
- **Reproducible-build worry for pyc was based on the same false
  premise.** If the analysis is deterministic and only the
  *printer's* count is unstable, pyc compilations were never
  actually affected.

#### Step 2 follow-up: how widespread was the mis-report?

A sweep of all 17 fa-converge fixtures (5 runs each) showed the
bug affects 9 of them. The `.n` value over-reports the real
violation count whenever the open-addressed table sized up beyond
the live element count.

Per-fixture summary (sample of the `(.n, set_count)` pairs seen
across 5 runs):

| Fixture                            | Affected? | Sample pairs                                       |
|------------------------------------|-----------|----------------------------------------------------|
| `01_monomorphic`                   | no        | (no events)                                        |
| `02_splitter`                      | no        | p1(3,3)                                            |
| `03_cascade`                       | yes       | p1(7,5) p2(3,3)                                    |
| `04_setter_split`                  | no        | p1(1,1)                                            |
| `05_violation`                     | no        | (no events)                                        |
| `iterator_copy`                    | yes       | p1(13,8) p2(13,11) p3(13,11)                       |
| `iterator_missing_field`           | no        | p1(2,2) p2(2,2) p3(2,2)                            |
| `missing_field_dispatch`           | no        | p1(2,2)                                            |
| `nested_iterator`                  | yes       | p1(13,13) p2(13|31,11) p3(13|31,11)                |
| `noop_main`                        | no        | (no events)                                        |
| `polymorphic_formal_2types`        | no        | p1(7,7)                                            |
| `polymorphic_formal_3types_2each`  | yes       | p1(13,11)                                          |
| `same_type_dispatch_2`             | yes       | p1(7,6)                                            |
| `setter_chain_2types`              | yes       | p1(13,8)                                           |
| `stored_fn_dispatch_2`             | yes       | p1(7,6)                                            |
| `vector_iterator`                  | yes       | p1(13,8) p2(13,9) p3(13,9)                         |
| `vector_polymorphic_writes_2`      | yes       | p1(13,10)                                          |

Only `nested_iterator` happened to land near a probe-collision
threshold often enough that its `.n` value visibly *alternated*
(13 vs 31). The other 8 affected fixtures mis-reported
consistently — which is why nobody noticed before
`nested_iterator` came along: the printer was *systematically*
over-counting by a fixed amount per fixture, so the goldens
locked in that wrong-but-stable number.

### Step 3 — Apply the fix

Replace every read of `type_violations.n` in `fa.cc` with
`type_violations.set_count()`:

- `record_fa_event()` line 115: `e->violations_after = …`
- `extend_analysis()` lines 3724, 3733, 3742, 3747, 3762, 3767,
  3777: the seven `viol0 = …` snapshots per splitter stage
- pass-limit trip log at line 3793
- final return-code check at line 3894 (for consistency; was
  coincidentally correct since `n > 0` iff `set_count > 0` once
  the set is populated)

The env-gated `VIOLATION` and `VIOL_EVENT` debug prints stay in
the tree, gated on `IFA_DEBUG_VIOLATIONS=1`. They cost nothing in
production and were load-bearing for this diagnosis.

#### Step 3 verification (recorded 2026-06-05)

After the fix, 20 standalone runs of `nested_iterator` all
produced identical output:

```
pass 1 type splits=1 ess=6→6 css=29→29 violations=13→13
pass 2 type splits=1 ess=9→9 css=29→29 violations=11→11
pass 3 violation splits=1 ess=10→10 css=29→29 violations=11→11
```

Determinism confirmed. Pass-2 violation count is 11 (the real
count of distinct triples recorded at that pass), not the
oscillating 13/31 (table capacities).

Full test suite clean: `./ifa --test` (51/0), `make test` (all
phases, fa-converge now 17/17 with `nested_iterator` included),
`make test_llvm`, `./test_pyc` (73 pass / 2 expected fail).

### Step 4 — Restore the printer field, re-bless goldens

The `IFA_PRINT_VIOLATIONS` env gate dropped from
`testing/print_fa_converge.cc`. `violations=X→Y` is now emitted
unconditionally — it's deterministic and useful.

14 of the 17 fa-converge goldens were re-blessed. The other 3
(`01_monomorphic`, `05_violation`, `noop_main`) had no events to
print, so they were unchanged. The reblessed values match the
scan's `set_count` predictions exactly — e.g. `iterator_copy`'s
pass-1 violations went from missing (the pre-Step-4 omitted
form) to `8→8`; `nested_iterator`'s pass-2/3 from missing to
`11→11`; `vector_iterator`'s pass-1 to `8→8`.

#### Step 4 verification (recorded 2026-06-05)

- 20 standalone runs of `nested_iterator` after the printer was
  restored: all identical, 0 failures.
- `./ifa --test`: 51/0.
- `make test`: 15 phases all clean (`fa-converge` 17/17 including
  `nested_iterator`).
- `make test_llvm`: pass.
- `./test_pyc`: 73 pass / 2 expected fail / 0 fail.

#### Surprise: issue 008 may also be gone

While verifying Step 4, ran the multi-fixture `fa-converge` phase
40× (30 of just `fa-converge`, plus 10 full-phase
`./ifa-test`). **Zero segfaults.**

Issue 008 documented an intermittent crash *only* when
`nested_iterator` ran alongside other fixtures, with claimed
30-60% reproducibility. At those rates, 40 runs should show
~12-24 crashes. Observing zero is real signal — either:

1. The crash was actually downstream of the `.n`-vs-`set_count`
   confusion in some non-obvious way (e.g. a code path that
   sized a buffer off `viol0 = type_violations.n`).
2. The crash was sensitive to something else that changed in
   the tier 0/1/2 cleanup (boundary fix, dormant-code removal,
   etc.).
3. The crash is still latent at lower probability than 008
   estimated. Possible but a sharp drop from 30-60% to <2.5%
   would need explanation.

Not closing 008 unilaterally — but worth a separate
investigation pass to either reproduce it on another
configuration or update 008 with the current empirical
non-reproducibility.

### Step 5 — Add regression tests

A new `test_type_violation_dedup_invariance` UNIT_TEST_FUN
in `ifa/testing/lattice_test.cc` exercises the
"set_count() == #unique triples regardless of insertion order"
invariant directly:

- Build 6 distinct AVar* keys (3 av-side, 3 send-side). Record
  4 distinct (kind, av, send) triples plus 2 duplicates in some
  order; CHECK `type_violations_count() == 4`.
- Reset; record the same 4 distinct triples in different order
  (with the duplicates interleaved earlier); CHECK the same
  result.
- Verify `kind` is part of the dedup key: same av/send with two
  different kinds yields 2 distinct triples.

A new public helper `int type_violations_count()` in `fa.h` /
`fa.cc` exposes `type_violations.set_count()` so the test can
read it without touching the static. (Useful as a debug
affordance too.)

This catches the regressions worth catching:

- `type_violations_count()` getting reverted to read `.n` → test
  fails (the empty set's first expand goes to capacity 7,
  not 4).
- `ATypeViolationHashFuns::equal` getting broken (always-true →
  count collapses to 1; always-false → count inflates to 6) →
  test fails in either direction.
- The `kind` field being dropped from the dedup key → the third
  check fails.

`./ifa --test` count: **52 / 0** (was 51).

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

#### Step 6 results (recorded 2026-06-05)

[`../notes/004-plib-vec-pointer-set-hashing.md`](../notes/004-plib-vec-pointer-set-hashing.md)
landed. The note captures:

- `set_add_internal` verbatim, the `prime2` expansion sequence,
  and the two observable effects (iteration-order
  non-determinism + capacity oscillation).
- How 009 manifested only the capacity effect, why the iteration-
  order effect didn't surface for `nested_iterator`, and the ~17
  `qsort_by_id` sites in `fa.cc` that show how the codebase has
  been pragmatically containing the iteration-order concern.
- Total surface: ~244 `set_add` / `set_in` call sites across the
  ifa core, of which ~110 are in `analysis/`. Each is a latent
  non-determinism site if its post-`set_add` iteration loop isn't
  either order-insensitive or explicitly sorted.
- Three migration shapes (sorted iteration helper /
  content-based hashing / dedicated IdSet container), ordered by
  invasiveness.
- Cross-references to the AUDIT sections and the closing of
  this issue.

No code change. The note is parked at `ifa/notes/` per the same
convention as the CDB and eager-splitting deferrals; it can
graduate to a numbered `ifa/issues/` file if someone scopes the
work.

#### Step 6 follow-on: options A + B landed (June 2026)

Notes/004's option A (`sorted_view` helper) and option B
(`PointerHash<C>` trait + id-based specializations for the six
id-bearing pointer types) landed together as one bundle. After
the change, `fa-converge` is byte-identical across 5+ runs of
every fixture (`nested_iterator` included). The remaining
cleanup work — the `.n` → `.capacity` / `.size` rename and the
17 `qsort_by_id` → `sorted_view` migrations — is filed as
[010-vec-set-api-cleanup.md](010-vec-set-api-cleanup.md).

## Status

Steps 1-6 complete; Step 6 follow-on (options A + B) landed
June 2026. Closing this issue. Remaining cleanup work tracked
in [010](010-vec-set-api-cleanup.md).

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
