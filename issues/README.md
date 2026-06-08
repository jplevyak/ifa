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
  **Closed June 2026.** `codegen-llvm` phase + normalizer
  landed with 4 locked-in `.ir` fixtures (`01_baseline`,
  `02_call`, `03_record_with_field`, `04_two_records`)
  covering integer / function-call / record / GEP codegen
  paths. The normalizer strips host-specific module-level lines
  (`target triple`, named metadata, `!N = ...` debug-info
  table) and `, !dbg !N` / `#dbg_declare(...)` debug
  annotations. The multi-fixture state-leak called out in the
  initial commit was traced to destructor-ordering in
  `llvm_codegen_initialize` (old Module's destructor accesses
  freed Context) and fixed with explicit `reset()` in
  reverse-dependency order. Plan §5 fixtures #24 / #25
  (linkage counting + verifyModule smoke test) remain as future
  enhancements that need printer changes beyond the normalizer
  itself.
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
  chain-aware matcher landed (`match_prim_chain` + `inline_prim_chain`
  + `INLINE_PRIM_CHAIN` event, June 2026). Implementation matches
  the issue's spec; fires on real pyc code (e.g. sieve.py,
  dict_basic.py); test suite stays green. **But:** the issue's
  original example (`add_one` surviving) no longer reproduces in
  HEAD — existing closure-collapse + single_send + DCE pipeline
  has already squeezed the wrapper count to its floor. Coverage
  win for current pyc tests is zero. Infrastructure in place for
  future cases; the natural next ask is "Gap A: iterative
  inlining" (file as follow-on if desired).
- [007-mark-type-stage-coverage.md](007-mark-type-stage-coverage.md) —
  post-type splitter stages coverage. **Partial:** 3 of 7 stages
  reached (type / setter / violation; violation came back when
  008 closed and `nested_iterator` was restored June 2026).
  Remaining open: `mark-type`, `setter-of-setter`, `mark-setter`,
  `mark-setter-of-setter`. June 2026 follow-up added a structural
  reading of the four splitters and a dead-code hypothesis; next
  move is either one targeted recursive-polymorphic shape OR a
  dead-code archeology pass to remove the unreached stages.
- [008-fa-crash-on-nested-iterator-shape.md](008-fa-crash-on-nested-iterator-shape.md) —
  intermittent FA-level segfault when `nested_iterator` ran
  alongside other fixtures. **Closed June 2026 (could not
  reproduce):** fixture restored as part of 009; 550 stress runs
  produced 0 crashes; valgrind found no FA-code errors. The 009
  measurement-bug diagnosis didn't explain a crash, so one of
  the tier 0-3 cleanups apparently masked or fixed the trigger.
  Fixture stays in the suite as a tripwire if it recurs.
- [009-fa-violations-nondeterminism.md](009-fa-violations-nondeterminism.md) —
  FA's `type_violations.n` reported value alternated across
  runs. **Closed June 2026.** Surprise diagnosis: the analysis
  was deterministic; the printer was reading `.n` (table
  capacity of the underlying `Vec`-as-set) instead of
  `.set_count()` (live element count). Fix was a one-line-per-
  site swap at ~10 reporting sites in `fa.cc`. Scan of all 17
  fa-converge fixtures showed 9 were silently mis-reporting,
  only `nested_iterator` happened to alternate visibly.
  Cross-cutting plib follow-on filed as
  [../notes/004-plib-vec-pointer-set-hashing.md](../notes/004-plib-vec-pointer-set-hashing.md).
  Side observation: issue 008 stopped reproducing in 40 runs
  post-fix (cause unclear, separate investigation).

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
