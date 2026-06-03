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
- Status: `open`, `in-progress`, `closed` (with closing commit
  ref). Closed issues stay in the tree as history.
- Cite specific files / line numbers / commits where helpful.
- Include a "Verification plan" so the next person knows how to
  prove the fix works.
- Include a "What this unblocks" section — issues with no
  consequence should not be filed.

## Current open issues

- [001-keepalive-vs-explicit-reply.md](001-keepalive-vs-explicit-reply.md) —
  the test-harness keepalive SEND crashes FA when a `.ir` user fun
  has its own `(send @primitive @reply …)`. **Closed by `f2fc2d2`**
  (keepalive removed as part of 005); kept for the investigation
  trail.
- [002-codegen-llvm-normalizer.md](002-codegen-llvm-normalizer.md) —
  no `codegen-llvm` test phase; needs a line-by-line normalizer
  for host-specific LLVM-IR text variation.
- [003-fa-converge-determinism.md](003-fa-converge-determinism.md) —
  `fa-converge` phase needed a per-pass event sidecar (mirroring
  `InlineEvent`) so pass counts and per-stage splits could be
  golden-tested. **Closed:** FAPassEvent sidecar + printer + 5
  fixtures landed. Follow-up: add fixtures that exercise setter /
  mark-setter / violation stages (current set only covers `type`).
- [004-find-local-loops-siblings.md](004-find-local-loops-siblings.md) —
  `find_local_loops` reported nested loops as siblings, breaking
  the loop-tree-walking frequency estimator. **Closed:** two-part
  fix in `find_loop` (walk-up-to-REP) and `collapse` (inherit
  entry preds). New `freq/03_nested_loops.ir` golden locks the
  inner-body peak frequency at 100 = `LOOP_FREQUENCY^2`.
- [005-retire-speculative-sym-level-dce.md](005-retire-speculative-sym-level-dce.md) —
  retire `if1_simple_dead_code_elimination`'s speculative SEND/MOVE
  kills in favor of FA-level `mark_live_code`. **Closed:** all six
  steps landed. Sym-level pass now does structural label-pruning
  only. Pyc's `asymbol` blanket-set kept (load-bearing for scope
  resolution — separate cleanup).
- [006-simple-inlining-multi-send-chain.md](006-simple-inlining-multi-send-chain.md) —
  `simple_inlining` misses straight-line multi-SEND wrappers like
  `def add_one(self): return self.v + 1`. Extending the single-SEND
  pattern matcher to a chain matcher would catch the most common
  method-wrapper shape in pyc-emitted IR. Stays within the "simple"
  boundary (no iteration, no cost model).
- [007-mark-type-stage-coverage.md](007-mark-type-stage-coverage.md) —
  the post-type splitter stages (`mark-type`, `setter-of-setter`,
  `mark-setter`, `mark-setter-of-setter`) aren't triggered by any
  pyc test, V test, or synthetic shape attempted. Same root cause:
  every shape produces at least one stage-1-qualifying confluence
  that pre-empts the later stages. Either there's a shape pattern
  not yet tried, or these are dead code.

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
