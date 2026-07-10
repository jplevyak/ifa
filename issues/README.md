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
- [025-intra-function-union-narrowing.md](025-intra-function-union-narrowing.md) —
  Broader: IFA doesn't narrow runtime union types
  intra-function on conditional branches.  **Partially
  fixed June 2026** for the `is None` shape (composes
  with [closed/024](closed/024-is-comparison-narrowing.md));
  `isinstance(v, T)` on runtime unions and
  other discriminator patterns remain follow-on work.
- [026-recursive-self-mutation-struct-collapse.md](026-recursive-self-mutation-struct-collapse.md) —
  Recursive types with >1 self-typed field lose fields in
  the synthesized C struct.  **Two fixes June 2026**:
  (1) prototype-vs-instance allocation size via
  `_CG_prim_clone_dst`; (2) struct field-index elision via
  cg_field_live (dead setters dropped, struct keeps live
  fields at has-index `eN`).  DLL and manual tree now
  work.  BST insert still blocked by a third bug —
  Node-in-function CS doesn't establish field-iv tracking
  → field reads through dispatch over union receivers
  miss the inside-function Node's value.
- [030-polymorphic-dispatch-fat-pointers.md](030-polymorphic-dispatch-fat-pointers.md) —
  core classtag dispatch implemented on both backends;
  remaining scope is value-identity dispatch on raw
  callables beyond the bare-callable landing.
- [031-globals-outside-fa-precision.md](031-globals-outside-fa-precision.md) —
  steps 1 and 2 landed 2026-07-04; step 3 folded into
  029/030's scope.
- [032-fa-survey-findings.md](032-fa-survey-findings.md) —
  tracking umbrella for actionable findings from the
  2026-07 `fa.cc` semantic survey; check items off with
  their closing commit as they land.
- [033-splitter-non-idempotent-divergence.md](033-splitter-non-idempotent-divergence.md) —
  splitting loop has no fixed point on some inputs;
  mitigated with a stall guard (commit `21dbdad4`) but the
  root non-idempotence is untouched.
- [034-pygasus-update-display-assert.md](034-pygasus-update-display-assert.md) —
  `update_display` assert on pygasus, undiagnosed; unmasked
  (not introduced) by the 033 stall guard.
- [035-nondeterministic-codegen-clone-order.md](035-nondeterministic-codegen-clone-order.md) —
  heap-layout-dependent compiles (different `.c` per run, some
  layouts miscompiling): **FIXED 2026-07-10** — twelve defects
  from SSU liveness through clone to codegen; determinism sweep
  36 → 0 flaky tests. Open only for the harness double-compile
  gate and the issue033-stage-c revalidation.
- [036-llvm-phy-lowering-wrong-value.md](036-llvm-phy-lowering-wrong-value.md) —
  v2 LLVM backend miscompiles expr_evaluator (returns 0 for 6)
  now that complete liveness places more phys; C backend lowers
  the same IR correctly. Deterministic repro; the one remaining
  suite failure.

## Closed (archive)

Closed issues live in [`closed/`](closed/) with the closing
commit ref recorded in each file's status line.  They stay in
the tree as history — a code-search for the affected file finds
the trail of investigation even after the fix has landed.

Currently 24 closed issues:
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
[018](closed/018-v2-loop-after-undef.md),
[019](closed/019-v2-flat-list-header.md),
[020](closed/020-v2-list-add-empty-body.md),
[021](closed/021-v2-call-arg-swap.md),
[022](closed/022-iterative-inlining.md),
[023](closed/023-v2-is-value-type-consumer.md),
[024](closed/024-is-comparison-narrowing.md),
[027](closed/027-v2-llvm-narrowed-loop-loses-struct-type.md),
[028](closed/028-fibheap-blockers.md),
[029](closed/029-polymorphic-dispatch.md).

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
