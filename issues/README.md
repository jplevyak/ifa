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

- Filenames: `NNN-CAT-short-slug.md`, NNN zero-padded, CAT one of
  the category tags below. Pick the next number; don't reuse.
  Closed issues keep their original `NNN-short-slug.md` name (no
  category tag) — the tag is a navigation aid for the open list,
  which is where it earns its keep; retrofitting it onto the
  archive isn't worth the churn.
- Category tags (see "2026-08-06 triage" below for how these were
  chosen):
  - **FA** — core flow-analysis / type-inference / splitter /
    convergence algorithm (`fa.cc` and friends).
  - **DISPATCH** — polymorphic method dispatch / classtag /
    per-CS method cloning.
  - **CGEN** — C backend codegen specifically (`cg.cc`).
  - **LLVM** — LLVM backend codegen specifically
    (`cg_emit_llvm.cc`, `llvm_*.cc`).
  - **CLEANUP** — non-functional code-quality / API-clarity work.
  - **SURVEY** — a tracking umbrella aggregating findings that are
    themselves filed (or foldable) elsewhere; prefer closing a
    SURVEY once its items land rather than letting it linger.
- One issue per file. Cross-link with relative paths.
- Status: `open`, `in-progress`, `partial`, `closed`.  Closed
  issues move into [`closed/`](closed/) (a flat archive — they
  stay in the tree as history) with a closing commit ref (or date,
  if no single commit captures it) in the file's status line.
- Cite specific files / line numbers / commits where helpful.
- Include a "Verification plan" so the next person knows how to
  prove the fix works.
- Include a "What this unblocks" section — issues with no
  consequence should not be filed.
- When one issue's remaining scope turns out to be entirely
  covered by another (a later doc reframes/corrects/subsumes an
  earlier one), close the earlier one as **superseded** rather than
  leaving two open docs describing the same problem. Preserve it in
  `closed/` as history — don't delete — and add a one-line pointer
  at the top of the surviving doc so a reader lands on the
  derivation trail.

## 2026-08-06 triage & reorganization

Full-corpus triage of all 38 then-open issues (via 8 parallel
survey passes reading every file in full), prompted by the open
list having drifted badly out of sync with reality: several issues
were plainly fixed but never moved to `closed/`, a few were
self-superseded (a later dated section in the same file overturned
the header), the README's own "Current open issues" list had
silently stopped being maintained (it indexed only 16 of the 38
files), and a real cluster of FA-convergence issues (033/063/064/
065/066/067/072/073/074/075 plus 047/048/052/055/057) had grown
organically over ~6 weeks with heavy but inconsistently-recorded
cross-referencing.

**Decisions made:**

1. **Closed as resolved** (fix landed and verified, doc just never
   archived): [026](closed/026-recursive-self-mutation-struct-collapse.md),
   [031](closed/031-globals-outside-fa-precision.md),
   [032](closed/032-fa-survey-findings.md),
   [035](closed/035-nondeterministic-codegen-clone-order.md),
   [046](closed/046-optional-none-field-inline-type-sum-assert.md),
   [057](closed/057-sorted-tolist-fa-nonconvergence.md),
   [070](closed/070-embedded-nul-literal-truncation.md),
   [073](closed/073-teach-splitter-productive-vs-inert-context.md).
2. **Closed as superseded/subsumed** (remaining scope, if any, now
   lives entirely in a surviving doc):
   [033](closed/033-splitter-non-idempotent-divergence.md) → forward
   work continues under [074](074-FA-cross-pass-oscillation-plan.md);
   [063](closed/063-no-type-bucket-triage.md) → forked into
   [075](075-FA-element-cs-method-split-idempotent-plan.md) (build
   plan), [067](closed/067-dijkstra2-heap-tuple-precision-and-use-before-def.md)
   (dijkstra2 attribution correction), and 074 (oscillation-vs-
   genuine-no-type distinction);
   [064](closed/064-method-phantom-display-blocks-es-split-routing.md) →
   confirmed dead end by its own text, retired by 074;
   [065](closed/065-mark-stage-es-split-routing-and-growing-product.md) →
   reframed and corrected by [066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md);
   [067](closed/067-dijkstra2-heap-tuple-precision-and-use-before-def.md) →
   its landed half (Part B) is done, its open half (Part A) is
   exactly [068](068-FA-derive-structural-ops-record-field-fold.md)'s
   unbuilt tuple-side design.
   This turns a tangled 10-file cluster into 4 surviving open docs
   (066, 068, 074, 075) each with a clear, non-overlapping remaining
   scope, plus a preserved derivation trail in `closed/`.
3. **Not merged**, despite living in the same problem family —
   each has its own unconfirmed root cause or independent repro and
   would lose information if folded into a sibling: 047, 048, 052,
   055 (FA-convergence/container-element family, but each a
   distinct, still-unresolved mechanism — 055 in particular was
   *explicitly retested* against 057's fix and confirmed not
   resolved by it, so it stays a separate doc even though 057 is now
   closed).
4. **Renamed with a category prefix** (see Conventions) — the 25
   issues that remain open after (1)/(2), listed below by category.
5. **Repo-wide cross-links fixed** for every renamed/moved file:
   other `ifa/issues/` docs, `ifa/issues/closed/` docs, the
   top-level `issues/` tree, and prose docs (`CLAUDE.md`,
   `ifa/CODE_GEN_IR.md`, `ifa/LIVENESS.md`,
   `ifa/codegen/archive/CG_IR_PLAN.md`, `ifa/notes/005-*.md`,
   `ifa/testing/phases/09_synthetic_coverage.md`, `tests/PARITY.md`).

Net: 38 open → 25 open (13 closed, 0 net new files), a stale README
index replaced with one that actually lists every open issue,
grouped by category and by epic-vs-targeted scope.

## Current open issues

### FA — large, open-ended (the convergence / container-element-precision cluster)

These are intertwined: all trace back to the same underlying gap
(shared `list`/`dict` method contours don't discriminate by element
type, and split decisions aren't stably keyed across passes), per
the [033](closed/033-splitter-non-idempotent-divergence.md) →
[063](closed/063-no-type-bucket-triage.md) investigation lineage.

- [074-FA-cross-pass-oscillation-plan.md](074-FA-cross-pass-oscillation-plan.md)
  — the current master plan, sequencing and re-measuring the whole
  cluster (033/063/064/065/066) after
  [073](closed/073-teach-splitter-productive-vs-inert-context.md)'s
  fix landed. 17/77 corpus programs still oscillate; Stage 4
  (display-liveness demotion) built but not landed (net positive,
  regresses 2 programs); "lever b" redirected the residual to
  caller-contour multiplication and genuine no-type violations, not
  a fixable splitter bug.
- [075-FA-element-cs-method-split-idempotent-plan.md](075-FA-element-cs-method-split-idempotent-plan.md)
  — concrete build plan (successor to 063) to clone shared
  `list`/`dict` methods per element-CS, shedskin's `func_copy`-per-
  `dcpa` model. Prototype gets dijkstra2 + pylife FAIL→COMPILED;
  landing it idempotently (so it stops backsliding) is the open
  work. `ant`/`kanoodle` remain unresolved corpus regressions from
  the naive version.
- [066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md)
  — the corrected framing (absorbing 065): CS identity is
  re-derived from scratch every pass instead of being keyed
  per-creation-site, causing oscillation. Part 1 (ROUTE enforcement)
  landed 2026-07-23, zero regressions, but flagged with an unverified
  correctness caveat (pygmy render swings 49%, no oracle). Part 2
  (self-product/phase-ordering) deferred.
- [072-FA-empty-container-notype-current-mechanism-and-plan.md](072-FA-empty-container-notype-current-mechanism-and-plan.md)
  — empty/imprecise-container element-type inference (the 043
  family). A default-seeding prototype was built, measured
  net-negative, and removed; the surviving design is a narrower
  write-attribution split (steps 1-3), not yet built.
- [007-FA-mark-type-stage-coverage.md](007-FA-mark-type-stage-coverage.md)
  — 5 of 7 splitter stages reached; `setter-of-setter` and
  `mark-setter-of-setter` remain structurally hard to trigger (the
  cascade self-defeats: setter-of-setter only runs if setter found
  nothing in the *same* pass).
- [025-FA-intra-function-union-narrowing.md](025-FA-intra-function-union-narrowing.md)
  — IFA's "narrowing" is clone-time specialization, not true
  flow-sensitive refinement. The `is None` / `isinstance`-on-union
  cases are fixed; phi-merge re-discrimination and `==`-constant
  narrowing remain open.
- [068-FA-derive-structural-ops-record-field-fold.md](068-FA-derive-structural-ops-record-field-fold.md)
  — treat classes and tuples uniformly as "records" and derive
  `__eq__`/`__lt__`/`__hash__`/etc. as field-folds over ordinary
  sends. Class-side landed 2026-07-24 and verified; the tuple side
  (which is what closed-067's remaining Part A needs) is designed
  but unbuilt.
- [071-FA-chess-accumulated-union-notype-cascade.md](071-FA-chess-accumulated-union-notype-cascade.md)
  — chess.py's remaining blocker is the issue-018/030 heterogeneous
  `linePieces` tuple-of-tuples (mixing arities). 2026-08-06 addendum
  compares shedskin's vector-backed `tuple2<T,T>` (arity not part of
  the type) to pyc's per-arity struct model and proposes generalizing
  pyc's existing dynamic-tuple-degrades-to-list compromise to any
  same-element-type tuple, as a design note for the 018/030 boxing
  work.

### FA — targeted

- [039-FA-uninitialized-local-reads-silent.md](039-FA-uninitialized-local-reads-silent.md)
  — reading a local unassigned on some CFG path is silent UB, not a
  diagnostic (`place_phi` is liveness- not definite-assignment-
  driven). Proposed fix: an 18th canonical `AType`
  (`uninitialized_type`).
- [041-FA-verbose-type-dump-intermittent-segfault.md](041-FA-verbose-type-dump-intermittent-segfault.md)
  — two unreproduced-on-demand segfaults in the `-v` per-pass type
  dump, both under machine load; likely the same null-guard bug
  class 033 found and fixed elsewhere in `fa.cc`, unconfirmed.
- [048-FA-deepcopy-flow-divergence-genetic2.md](048-FA-deepcopy-flow-divergence-genetic2.md)
  — genetic2's repeated-deepcopy-and-graft pattern produces
  ever-longer copy-of-copy CS chains, each re-matched against a
  growing candidate product; 033's landed MatchCache retention does
  *not* help here (confirmed — distinct mechanism, per-chain not
  per-pass reuse needed).
- [049-FA-raise-only-contour-notype.md](049-FA-raise-only-contour-notype.md)
  — a function reached only via its raising branch gets a
  bottom-typed return. Two fix prototypes (placeholder-move,
  violation-suppression) were built and reverted 2026-08-06 as
  unsafe; downgraded to "likely cosmetic warning, not correctness
  bug" since the baseline already salvages it via
  `convert_NOTYPE_to_void`.
- [050-FA-general-constant-propagation-unreachable-code.md](050-FA-general-constant-propagation-unreachable-code.md)
  — no SCCP-style fixed point; only one ad-hoc point detector exists
  (`can_raise`). Direction 3a (native can-raise fact in FA's own
  fixed point) landed 2026-07-18; 1/2/3b remain open, 3b being a
  large general global-slot-propagation feature.
- [052-FA-shared-method-branch-reopens-empty-list-fragility.md](052-FA-shared-method-branch-reopens-empty-list-fragility.md)
  — adding *any* branch to a shared `clone_methods_per_cs` method
  can reopen closed-040's empty-list fragility; worked around at the
  codegen level, not fixed at the FA level. No test currently catches
  this class of regression.
- [055-FA-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md](055-FA-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
  — adding `set.__sub__` hangs/crashes compiling plcfrs.py (flat
  EntrySet count, growing worklist — a non-convergence signature).
  Root cause not isolated past bisection; **explicitly retested
  against closed-057's fix and confirmed NOT resolved by it** — a
  distinct repro in the same disease family.
- [081-FA-int-mult-bool-constant-fold-segfault.md](081-FA-int-mult-bool-constant-fold-segfault.md)
  — `n * b` (`int` times `bool`) segfaults the compiler itself inside
  `add_send_edges_pnode`'s constant-folding, before either backend's
  codegen is ever reached — confirmed via gdb backtrace, backend-
  agnostic. Found while independently verifying closed-062's fix;
  unrelated to it (this is FA-level constant-folding, not codegen
  scalar coercion). No workaround available to the pyc user short of
  avoiding the literal combination, with no diagnostic pointing at it.

### DISPATCH

- [030-DISPATCH-polymorphic-dispatch-fat-pointers.md](030-DISPATCH-polymorphic-dispatch-fat-pointers.md)
  — core classtag dispatch implemented on both backends. Mixed
  plain-function/closure-carrier dispatch **fixed on both backends**
  2026-08-06 (classtag compare + direct call, no method-pointer-slot
  infrastructure needed; the LLVM half also required restructuring
  `emit_send_call`'s per-candidate loop to stop bailing to a wholly
  separate, uninitialized-alloca-reading bare-callable pass, bringing
  it to parity with `cg.cc`'s general classtag+plain mixing). Remaining
  open: high-fan-out table dispatch (vs. if/else chain) was never
  built (now a perf concern, not correctness — 11-subclass fanout
  works).
- [079-DISPATCH-single-candidate-dispatch-unchecked-cast.md](079-DISPATCH-single-candidate-dispatch-unchecked-cast.md)
  — dispatch's "single candidate" fast path emits an unchecked cast
  when the receiver's union has *another* member that doesn't
  implement the method at all (never a dispatch candidate, so
  silently uncovered). `bh.py` segfaults this way. Not attempted —
  touches the hottest dispatch path in codegen.

### CGEN (C backend)

- [054-CGEN-remove-unconditional-tuple-list-header.md](054-CGEN-remove-unconditional-tuple-list-header.md)
  — a same-day plcfrs fix made *every* tuple allocate a 16-byte
  list-header unconditionally, even when never needed. Deliberately
  deferred (safe but imprecise) — revisit only if profiling shows it
  matters.
- [061-CGEN-multi-tuple-list-null-element-type.md](061-CGEN-multi-tuple-list-null-element-type.md)
  — a list of tuples emits `(null)*` or an incompatible-pointer cast
  when several distinct tuple record types coexist and get
  `.sort()`ed together. Same bug *class* as 056 (malformed C instead
  of a guarded degrade); not a duplicate.

### LLVM backend

### CLEANUP

- [010-CLEANUP-vec-set-api-cleanup.md](010-CLEANUP-vec-set-api-cleanup.md)
  — started as a small deferred rename (`Vec::n`→`capacity`/`size()`)
  plus a `qsort_by_id`→`sorted_view()` migration; has grown into a
  full `BaseVecSet`/`Vec`/`Set` split proposal ("option C revisited")
  with a 475-site, 27-file migration plan. Non-functional throughout
  (output must stay byte-identical). Folded in closed-021.

## Closed (archive)

Closed issues live in [`closed/`](closed/) with the closing
commit ref (or date) recorded in each file's status line.  They
stay in the tree as history — a code-search for the affected file
finds the trail of investigation even after the fix has landed.

Currently 59 closed issues:
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
[026](closed/026-recursive-self-mutation-struct-collapse.md),
[027](closed/027-v2-llvm-narrowed-loop-loses-struct-type.md),
[028](closed/028-fibheap-blockers.md),
[029](closed/029-polymorphic-dispatch.md),
[031](closed/031-globals-outside-fa-precision.md),
[032](closed/032-fa-survey-findings.md),
[033](closed/033-splitter-non-idempotent-divergence.md),
[034](closed/034-pygasus-update-display-assert.md),
[035](closed/035-nondeterministic-codegen-clone-order.md),
[036](closed/036-llvm-phy-lowering-wrong-value.md),
[037](closed/037-matcher-cartesian-cs-product.md),
[038](closed/038-LLVM-coro-split-second-suspend-unreachable.md),
[040](closed/040-empty-list-shared-clone-type-inference.md),
[042](closed/042-null-meta-type-build-type-hierarchy-segfault.md),
[043](closed/043-empty-container-inference-options.md),
[044](closed/044-mixed-length-tuple-list-len-miscompile.md),
[045](closed/045-receiver-cs-method-cloning.md),
[046](closed/046-optional-none-field-inline-type-sum-assert.md),
[047](closed/047-different-arity-tuple-iteration-shared-cs.md),
[051](closed/051-LLVM-nested-list-index-mixed-union-crash.md),
[053](closed/053-tuple-unpack-target-heterogeneous-arity-segfault.md),
[056](closed/056-CGEN-degraded-index-type-raw-c-compile-error.md),
[057](closed/057-sorted-tolist-fa-nonconvergence.md),
[058](closed/058-polymorphic-classtag-dispatch-drops-extra-arguments.md),
[059](closed/059-narrowing-peel-wrapper-boolean-collapse-gap.md),
[060](closed/060-none-branch-dropped-mixed-with-literal-bool-sequence.md),
[062](closed/062-LLVM-mixed-int-float-scalar-coercion.md),
[063](closed/063-no-type-bucket-triage.md),
[064](closed/064-method-phantom-display-blocks-es-split-routing.md),
[065](closed/065-mark-stage-es-split-routing-and-growing-product.md),
[067](closed/067-dijkstra2-heap-tuple-precision-and-use-before-def.md),
[069](closed/069-per-arity-tuple-types-scope.md),
[070](closed/070-embedded-nul-literal-truncation.md),
[073](closed/073-teach-splitter-productive-vs-inert-context.md),
[076](closed/076-mutation-driven-receiver-divergence-not-cloned.md),
[077](closed/077-primitive-equality-codegen-missing-salvage-guard.md),
[078](closed/078-class-body-default-plus-init-override-permanently-unions.md),
[080](closed/080-LLVM-index-type-mismatch-no-salvage-guard.md).

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
