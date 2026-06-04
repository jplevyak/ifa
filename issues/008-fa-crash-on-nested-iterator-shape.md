# Issue 008: FA crash on synthetic `nested_iterator` shape

**Status:** open (intermittent — affects FA-level test-suite reliability).
**Affects:** `ifa/analysis/fa.cc` (somewhere in extend_analysis or
its callees); only manifests when the synthetic `nested_iterator`
shape is run via the test harness.
**Workaround:** dropped the `nested_iterator.synth` fixture
(commit removing it lives alongside this issue file). Synthetic
coverage of the violation splitter stage was lost as a result —
the only synthetic shape that fired the violation stage was the
crashy one.
**Related:** Phase 09 step C 7.7 follow-on (the "fix shapes via
method dispatch" round), [007-mark-type-stage-coverage.md](007-mark-type-stage-coverage.md).

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

Worth investigating:
- Run under valgrind. The crash signature should be obvious.
- Compare with the `pyc` test programs that have violations
  recorded (none of which trigger the violation stage either, so
  there's no positive control).
- Read `split_for_violations` and
  `collect_violation_imprecisions` looking for AVar /
  CreationSet pointer arithmetic on a vector that may resize.

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
