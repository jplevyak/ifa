# Issue 008: FA crash on synthetic `nested_iterator` shape

**Status:** **closed (could not reproduce)** June 2026. The
fixture was restored as part of issue 009's investigation
(commit `95e1598`). 550 stress runs (500× `--phase fa-converge`
multi-fixture + 50× full `ifa-test`) produced 0 crashes. A
single valgrind pass showed 89 errors, all inside Boehm GC's
stack-scanning / heap-block allocation (well-known GC false
positives) — zero in FA code. The root cause remains uncertain
(009's measurement-bug diagnosis didn't explain a crash); the
most plausible reading is that one of the tier 1/2/3 cleanups
between original filing and this follow-up happened to mask or
fix the trigger. See "Follow-up — June 2026" section below.
The issue file stays in the tree as history; reopen if the
pattern recurs.
**Affects:** `ifa/analysis/fa.cc` (somewhere in extend_analysis or
its callees); only manifests when the synthetic `nested_iterator`
shape is run via the test harness.
**Related:** Phase 09 step C 7.7 follow-on (the "fix shapes via
method dispatch" round), [007-mark-type-stage-coverage.md](../007-mark-type-stage-coverage.md),
[009-fa-violations-nondeterminism.md](009-fa-violations-nondeterminism.md)
(closed; restored the fixture and tickled the same code path
without crashes).

## Symptom

When `nested_iterator.synth` is present in the synthetic fixture
set, `make test-ir` produces a `Segmentation fault (core dumped)`
intermittently during the `fa-converge` phase run. Reproducible
roughly 30-60% of the time.

Standalone `./ifa-test --phase fa-converge nested_iterator`
**does not crash**. The crash only manifests when the fixture
runs alongside the other synthetic fixtures in the same phase
invocation, AND only in some fraction of runs.

The fa-converge output for nested_iterator is also non-
deterministic across runs of just that one fixture — the
violations counts in the (history) block alternate between
`violations=13→13` and `violations=13→31`. The splits and pass
counts are stable. The non-determinism in the violations counts
was masked by dropping that field from the printer
(`testing/print_fa_converge.cc` doesn't emit it anymore).

## What nested_iterator was doing

Reproduces pyc's list-runtime structure with two nesting levels:

```
V_inner : vector with primitive elements
V_outer : vector whose element type is V_inner
It      : iterator record (vec, pos fields)
__getitem__ / __setitem__ methods on each V type (via
                ir::install_subscript_methods)

consume_outer(vv):
  outer.__getitem__(0).__getitem__(0)   # two method-dispatched indexings

main:
  outer1 holds inner1 holds int
  outer2 holds inner2 holds float
  consume_outer(outer1); consume_outer(outer2)
```

When stable (no crash), FA converges in 3 passes with
`splits[type]=2, splits[violation]=1` — the first synthetic
shape to fire the violation stage.

## Hypotheses

The non-determinism + intermittent crash + the FACT that this
specific shape reaches a stage no other shape reaches all point
to: this shape exercises a code path in `extend_analysis` /
`split_for_violations` that has either a use-after-free or a
race condition on internal data structures' iteration order.

**Likely same root cause as [009](009-fa-violations-nondeterminism.md).**
Issue 009 documents the broader non-determinism in
`type_violations.n` (the count alternates between runs even
when no crash occurs). A reasonable theory: hash table or
pointer_set iteration order, keyed on GC heap addresses, causes
both the count variation AND occasionally hits a path where
some entry is freed before use. Fix one, fix both.

Worth investigating:
- Run under valgrind. The crash signature should be obvious.
- Compare with the `pyc` test programs that have violations
  recorded (none of which trigger the violation stage either, so
  there's no positive control).
- Read `split_for_violations` and
  `collect_violation_imprecisions` looking for AVar /
  CreationSet pointer arithmetic on a vector that may resize.
- See issue 009's verification plan for the iteration-order
  investigation steps.

## What this unblocks

- Re-introducing `nested_iterator.synth` restores synthetic
  coverage of the violation splitter stage.
- The deeper investigation likely uncovers an FA-level bug that
  could affect production pyc compilations of nested-container
  code (pyc programs may avoid this pattern, but the latent bug
  is still there).

## Why deferred

- Investigation requires reading FA internals with a debugger;
  unbounded scope for what was supposed to be a synthetic
  coverage round.
- The four splitter stages still uncovered (mark-type,
  setter-of-setter, mark-setter, mark-setter-of-setter) are
  ALSO blocked by issue 007 — so even with a stable
  nested_iterator we wouldn't have full coverage. The violation
  stage's loss is one more gap on a list of five.
- Pyc test suite is unaffected — the bug is shape-specific and
  pyc lowering doesn't produce this exact pattern.

## Verification plan

1. Restore `nested_iterator.synth` from git history (the
   fixture file + golden).
2. Run `make test-ir` 10× to confirm crash reproduces.
3. Run under valgrind to get the crash site.
4. Read fa.cc around the crash point; identify the actual issue.
5. Fix it.
6. Re-confirm crash gone; un-revert the fixture; lock the
   violation-stage coverage.

## Follow-up — June 2026

`nested_iterator.synth` and its `IRShape` builder were restored
on commit `95e1598` (as part of closing issue 009). Once
restored, this issue's reproducer is back in the test set, so
the follow-up reduced to: does the crash still fire?

**Stress results.** Running the multi-fixture form that
originally triggered the crash 30-60% of the time:

| Test                                                      | Runs | rc=0  | Crashes (rc=139) | Other |
|-----------------------------------------------------------|------|-------|------------------|-------|
| `./ifa-test --phase fa-converge` (multi-fixture)          | 500  | 500   | 0                | 0     |
| `./ifa-test` (all phases, multi-fixture)                  | 50   | 50    | 0                | 0     |

At the original 30-60% reproducibility, 500 runs at zero is
absurdly unlikely (probability < 1e-100). The failure mode has
shifted.

**Valgrind.** A single `valgrind --track-origins=yes` pass on
the multi-fixture `fa-converge` run reported 89 distinct errors
(53 "Conditional jump or move depends on uninitialised
value(s)", 36 "Use of uninitialised value of size 8"). **All
89 are inside Boehm GC's stack-scanning / heap-block allocation
code paths** — `GC_push_all_eager`, `GC_with_callee_saves_pushed`,
`GC_mark_from`, `GC_allochblk_nth`, etc. — which are well-known
GC false positives (the conservative collector inherently reads
"uninitialized" stack/heap memory looking for pointer-shaped
roots). Zero errors in `analyze`, `extend_analysis`,
`split_for_violations`, or any FA code. The deepest FA-internal
stacks we see end at `unique_AVar` / `make_AVar` /
`add_es_constraints` / `analyze_edge` / `FA::analyze` — i.e. the
call paths *into* GC malloc, not bugs in FA logic.

**Diagnosis is uncertain.** Issue 009's diagnosis (a printer
measurement bug, not an iteration-order non-determinism) doesn't
explain a crash. So one of these things is true:

1. The 008 crash was always a downstream consequence of *some
   other* bug that one of the tier 0 / tier 1 / tier 2 / tier 3
   cleanups happened to mask or fix. The most plausible
   candidates (in rough order):
   - Tier 1's `SettersClasses` infrastructure removal (deleted
     dead writes to `Setters::eq_classes`).
   - Tier 1's `cdb.{cc,h}` deletion (removed dead reads of
     `Fun::prof_id` / `prof_ess` / `es_info` whose initialization
     status was uncertain).
   - Tier 3 reentrancy steps 1-4 (per-FA state is now structural,
     so any cross-pass leak via `fa_reset()` band-aids is gone).
2. The crash is still latent and 008's original repro environment
   differed from ours in a way we haven't isolated (different libc,
   different Boehm GC version, different OS scheduling). Possible
   but unfalsifiable.

**Recommendation.** Close with no fix attributed. The fixture
stays in the suite, so if the crash recurs the regression will
be visible immediately. Reopen with a fresh investigation if it
does.

## What this *did* unblock

- `nested_iterator.synth` is back in the synthetic fa-converge
  set, restoring violation-stage coverage (was issue 007 §2 of
  the gap-list).
- Issue 009 closed cleanly with `nested_iterator` as its
  validation fixture.
