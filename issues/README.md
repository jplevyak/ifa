# ifa/issues

Open work items for the IFA library — each file documents one
issue: the symptom, the root cause as far as we've traced it, a
proposed fix (or a set of options), and what fixing it would
unblock.

These are *not* GitHub issues; the project doesn't track work
there. They're checked-in documents that travel with the code so
that:

- a future investigator can pick up the trail without re-doing the
  debugging,
- a code-search for the affected file finds the issue alongside,
- the proposed fix is reviewed alongside the code that has the
  workaround.

## Conventions

- Filenames: `NNN-short-slug.md`, NNN zero-padded. Pick the next
  number; don't reuse.
- One issue per file. Cross-link with relative paths.
- Status: `open`, `in-progress`, `partial`, `closed`.  Closed
  issues move into [`closed/`](closed/) (a flat archive — they
  stay in the tree as history) with a closing commit ref in the
  file's status line.
- Cite specific files / line numbers / commits where helpful.
- Include a "Verification plan" so the next person knows how to
  prove the fix works.
- Include a "What this unblocks" section — issues with no
  consequence should not be filed.

## Current open issues

- [007-mark-type-stage-coverage.md](007-mark-type-stage-coverage.md)
  — **partial.** 3 of 7 splitter stages reached (`type`,
  `setter`, `violation`).  Remaining: `mark-type`,
  `setter-of-setter`, `mark-setter`, `mark-setter-of-setter`.
  Either needs a targeted recursive-polymorphic shape or a
  dead-code archeology pass on the unreached stages.
- [010-vec-set-api-cleanup.md](010-vec-set-api-cleanup.md) —
  Two-task follow-on from
  [closed/009](closed/009-fa-violations-nondeterminism.md):
  rename `Vec::n` to `capacity` + add `size` alias (compile-error
  the count-vs-capacity footgun); migrate `qsort_by_id; for(x:s)`
  sites to `sorted_view()`.  Deferred because the rename touches
  ~1000+ Vec consumer sites.
- [015-pyc-pod-records-no-frontend-hook.md](015-pyc-pod-records-no-frontend-hook.md)
  — Feature gap: pyc has no frontend hook for declaring POD
  records.
- [019-v2-flat-list-header.md](019-v2-flat-list-header.md) —
  v2 LLVM flat-list-allocator emits the wrong list-header
  layout.
- [020-v2-list-add-empty-body.md](020-v2-list-add-empty-body.md) —
  v2 LLVM `list.__add__` lowers to an empty body for some
  specializations.
- [021-v2-call-arg-swap.md](021-v2-call-arg-swap.md) —
  v2 LLVM `CG2_CALL` walks formals in IF1-MPosition order;
  ordering convention deserves a documented invariant.
- [022-iterative-inlining.md](022-iterative-inlining.md) —
  `simple_inlining` runs `inline_single_sends` once.  Wrappers
  that *become* chains after one inlining round are never
  reconsidered.  Follow-on to [closed/006](closed/006-simple-inlining-multi-send-chain.md):
  the chain matcher's infrastructure is ready; iteration is
  what would make Gap A's targets reachable.

## Closed (archive)

Closed issues live in [`closed/`](closed/) with the closing
commit ref recorded in each file's status line.  They stay in
the tree as history — a code-search for the affected file finds
the trail of investigation even after the fix has landed.

Currently 15 closed issues:
[001](closed/001-keepalive-vs-explicit-reply.md),
[002](closed/002-codegen-llvm-normalizer.md),
[003](closed/003-fa-converge-determinism.md),
[004](closed/004-find-local-loops-siblings.md),
[005](closed/005-retire-speculative-sym-level-dce.md),
[006](closed/006-simple-inlining-multi-send-chain.md),
[008](closed/008-fa-crash-on-nested-iterator-shape.md),
[009](closed/009-fa-violations-nondeterminism.md),
[011](closed/011-setter-codegen-vs-analyzer-mismatch.md),
[012](closed/012-test-llvm-gc-link.md),
[013](closed/013-pyc-llvm-default-off.md),
[014](closed/014-llvm-construction-flow-to-slots.md),
[016](closed/016-llvm-ssu-formal-arg-binding.md),
[017](closed/017-iterator-construction-undef-self.md),
[018](closed/018-v2-loop-after-undef.md).

## When to file an issue here vs fix it now

File an issue when:
- The fix is more than ~1 hour of work *and* doesn't block the
  current task.
- The fix needs a design decision (multiple plausible approaches).
- The fix touches a subsystem the current task isn't auditing.
- You found a real-but-rare bug that has a clean workaround.

Fix it now when:
- It blocks the current task.
- It's a one-line fix and the test you'd write to verify it is the
  one you're already writing.
- The current PR is the natural place for it (the reviewer would
  spot the workaround and ask why).
