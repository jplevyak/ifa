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

## 2026-08-12/13 — the FA convergence session: what changed and what it invalidated

Five days of work in this area landed in two days; because it moved
several long-standing premises, here is the consolidated trail. Source
changes are four commits; everything else is measurement.

**What landed (source).**

1. **[098](098-FA-per-pass-reset-scoped-to-reachable-set.md) — the
   per-pass reset was scoped to the *previous* pass's reachable set.**
   `clear_results` reset per-edge/contour/CS state by walking `fa->ess`
   (which is just the last pass's `entry_set_done`) and did not run at
   all on a `reanalyze()`-driven pass. 19-24% of edges per pass carried
   an older pass's `args`/`rets`/`formal_filters` into the current one,
   and a stale `Match::formal_filters` made `analyze_edge`'s gate skip a
   live edge permanently. Fixed by resetting over authoritative
   registries (`FA::all_aedges` et al.) before *every* pass. Also fixed a
   latent null-deref in `check_split` that the new trajectory exposed.
   An `IFA_DBG_EDGEARGS` audit now guards the invariant.
2. **[099](099-FA-pending-backedge-avoid-veto-forces-period-2.md)
   (partial) — a structurally forced period-2 flip-flop.**
   `record_backedges` re-homed an inherited pending entry's KEY onto the
   split product but copied its VALUE verbatim, so each of two contours
   kept a route to the other; `check_split`'s `avoid` veto then left
   exactly the one just vacated. `pylife`'s entire non-convergence was
   **one edge**. Fixed the asymmetry; `loop` converges. Still open: the
   churn *relocated* into slow growth for bh/pylife/linalg.
3. **[100](100-FA-display-removed-from-contour-identity.md) — the
   lexical display is no longer contour identity.** Design decision. The
   display now serves only `make_AVar`'s enclosing-scope resolution (and
   clone's equivalence). `edge_nest_compatible_with_entry_set`,
   `edge_display_compatible`, `find_or_make_display_variant`,
   `EntrySet::display_variants`, `group_display_ok`,
   `fun_max_live_display_slot`, `stage4_enabled`, `PYC_STAGE4`,
   `Fun::max_live_display_slot` and `update_display`'s consistency assert
   are all gone. Contour counts drop 40-80% corpus-wide; `yopyra`
   converges. Cost: precision falls widely and oscillators net 16 → 20.
4. **`flow_var_to_var` must re-assert `b->in >= a->out`.** A
   *pre-existing* dropped-value bug the display removal exposed: the
   early return on an already-established link skipped the re-assert
   forever, so a value arriving after the link was created was never
   delivered. 098's own probe had measured this at zero — the display
   checks were keeping the affected contours apart. This is what fixed
   100's two exception-path miscompiles.

**Premises this invalidated.** Several long-standing conclusions rested
on things that turned out not to hold:

- **[074](074-FA-cross-pass-oscillation-plan.md)'s headline metric was
  partly measuring the stall guard.** Re-basing with
  `IFA_STALL_LIMIT`/`IFA_NONIMPROVE_LIMIT` disabled cut the genuine
  target set from 17 programs to **8**, and showed the guard is *causing
  miscompiles* (`sudoku5`, `msp_ss` compile to crashing binaries and to
  correct ones when their descent is allowed to finish). Violation counts
  alone are not a quality metric — `rdb` "converges" to 1 violation by
  emitting 1% of its former C.
- **074's Stage 1 "lifecycle facts" argument was false when written**
  (it assumed `clear_edge` ran on every edge — that is exactly 098).
- **074's Stage 0 and Stage 4 are retired**, and the basis on which it
  ruled out Stage 2 is stale.
- **[075](075-FA-element-cs-method-split-idempotent-plan.md)'s Piece 3 no
  longer exists** — its machinery was deleted with the display gate it
  worked around.
- **[066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md)
  is not the oscillation's lever**: CreationSet *splitting* measures ~0
  corpus-wide (twice), and `copy_AEdge` is 0 everywhere.
- **[097](097-CGEN-callsite-vs-clone-formal-type-mismatch.md)'s mechanism
  got wider**: `entry_set_compatibility` lost its nest gate, so its soft
  `val -= 4` type score is now more load-bearing, not less.

**Where the oscillation actually stands.** Two distinct diseases, both
now measured rather than inferred:

- *Assignment churn, no growth* — a fixed edge set swapping between a
  fixed contour set. 099 explains and fixes the period-2 form; `hq2x` is
  the current extreme (~250 edges detached and re-parked per pass, ~1 new
  edge, 102 passes).
- *Contour growth* — and it has **moved**. The old driver
  (`check_split`'s lineage-mint, blocked by the display) is gone; the
  remaining one is the *detach* route: `make_entry_set` skips
  `find_best_entry_sets` whenever `split` is non-null, so a detached edge
  is never offered an existing contour. `sudoku4`/`genetic2` show a
  byte-identical `split-fresh=2` leak every pass to the pass cap.
  Two repairs measured and rejected (soft reuse: 59 test failures; hard
  type-identity reuse: 6, including `recursive_polymorphic` and
  `match_map_star`). **Exact type identity is not sufficient evidence
  that a contour is not what the split is separating** — the detach route
  needs a positive grouping reason, which is Stage 1 (ii) with a much
  sharper target than when it was written.

**Investigation flags landed (all off by default, all measured).** These
exist so the next attempt starts from evidence rather than a rebuild:

| flag | what it does | result |
|---|---|---|
| `PYC_HARDREUSE=1..4` | offer a detached edge an existing contour (progressively stricter tests; 4 = lookup by durable key) | 260-261/265 — every mode manufactures a period-2 flip-flop of its own |
| `PYC_TYPEKEY=1` | durable per-contour type key, captured converged, matched against instead of the mid-pass value | **265/0** — the only clean one; corpus a wash |
| `PYC_CANON=1\|2` | canonicalize contour creation on that key (find-by-key-else-create) | 259/7 and 237/32; the conflict log is the real output |
| `PYC_NOMARK=0\|1\|2` | skip mark-based ES splitting (`1` = `MARK_TYPE`, **the default**; `2` also the setter-mark stages; `0` restores the old behaviour), leaving marks armed only on the `VIOLATION` repair path | **default-on 2026-08-14** — guard trips 18→10, −55% time, −12.3% ess, −6.8% C, mastermind2 starts compiling; 12 programs fewer violations, 5 more |
| `IFA_DBG_STAGE=1` | attribute every edge detach/mint/reuse **and CreationSet mint** to the splitter stage that caused it | showed the CS-minting stages drive `TYPE_CONFLUENCE` |
| `IFA_DBG_KEYSPACE=1` | per function per pass: contours built vs. distinct type-set tuples vs. distinct cartesian-product tuples | the measurement that indicted `MARK_TYPE` |
| `IFA_DBG_KEYDRIFT=1` | per pass: contours whose type key was stable / grew / shrank non-monotonically / flip-flopped | separates "still converging" from "oscillating" |
| `PYC_SELFPROD=0..5` | self-product complement eviction in the `v>0` case. `0` off (pre-074), `1` evict type-disjoint complement, `2` evict nothing, `3`/`4` durable key == recorded partition, **`5` durable key stable for two passes (per-contour convergence) — NOW THE DEFAULT** | 1/2 break linalg's cycle but damage the corpus; 3/4 never fire; **`5` landed default-on** — sunfish 1200 s timeout → 43 s compile, tictactoe 137 violations → 0, **zero exit-code regressions** |
| `PYC_CPA=N` | cartesian-product naming: fan a positional formal whose type is a fixed-point union of 2..N CreationSets into one contour per CS (new `CARTESIAN_PRODUCT` stage after `TYPE_CONFLUENCE`) | mechanism works (breaks the union, target violation gone) but **callee-side only is a net loss** — 10 programs newly fail to compile, `chull` 2 → 121 violations; real CPA needs the caller-side fan |
| `PYC_CPAMARK=1` | drop `different_marked_args`' distance filter, comparing the CreationSet sets directly | **refuted** — identical to `PYC_NOMARK` on hq2x, chull and the repro: 100% of `MARK_TYPE`'s contribution is the depth term |
| `IFA_DBG_MARKWHY=1` | for every mark verdict of "different", whether the *unfiltered* CreationSet sets differ too, or are identical and separated only by depth | repro 2% pure-distance vs hq2x **98%** — the two populations separate almost perfectly |
| `IFA_DBG_INCOMPAT=1` | which clause of the compatibility test separates edges (`arg` vs `ret`), stage-1 confluence disposition, and `REDERIVE` ROUTE/GROUP/FILTER | `ret`=0 everywhere; the GROUP quarter is 100% `v>0` self-product |
| `IFA_STALL_LIMIT`, `IFA_NONIMPROVE_LIMIT` | override the divergence guards (were compile-time constants) | takes the guard out of the measurement |
| `IFA_DBG_EDGEARGS=1` | 098's invariant audit (bound edges must have values at recorded args) | — |

**Type marks and canonicalization are mutually exclusive.** `MARK_TYPE`
exists to split two edges that carry the *same* argument types but
different value origins (IFA.md §6.2, "recursion-meets-polymorphism
without k-CFA") — a distinction no type-tuple contour name can express,
which is precisely what `PYC_CANON`'s conflict counter counts. It is the
price of naming contours by type *sets*: inside a dataflow cycle every
contributor carries the same union, so plain type splitting goes blind
and marks restore the ordering the union destroyed. Shedskin needs no
equivalent because CPA names by singletons.

`IFA_DBG_KEYSPACE` shows marks are not doing that job. On `hq2x`,
`__setitem__`'s type keyspace is **stationary from pass 7** (8 type-set
tuples, 17 CPA tuples) while its contour count grows 20 → 287; and the
~24 monomorphic one-line `PIXELxx_yy` functions (setkey=1, cpakey=1) get
**one contour per call site, one added per pass**, up to 36. That is
1-CFA by accretion on a function with a single argument type. Details
and the corpus numbers in 074.

**Which splits actually oscillate.** Of the nine splitter stages, only
**`TYPE_CONFLUENCE`** and **`MARK_TYPE`** produce steady-state *edge*
churn — but that framing turned out to be an artifact of metering only
edges. `IFA_DBG_STAGE` now also counts **CreationSet** mints, and the two
halves have completely different causes:

- **`MARK_TYPE` is the cause of its own churn**, building contours no
  type-tuple can name (see above).
- **`TYPE_CONFLUENCE` is a responder.** It mints **no CreationSets at
  all**; `SETTER`, `SETTER_OF_SETTER` and `CSM_ELEMENT_CS` mint them while
  moving zero edges — which is why they scored ~0 under the old meter. A
  new CreationSet widens types, which re-opens type confluences, which
  restarts `TYPE_CONFLUENCE`. Because `run_split_stages` gates every stage
  on `if (!analyze_again)`, the two can never progress on the same pass —
  they are forced to **alternate**. `linalg` does this as an exact
  **period-10 limit cycle**: `SETTER` mints 2 CreationSets, nine passes of
  `TYPE_CONFLUENCE` re-partitioning add 34 contours and 46 CreationSets,
  `SETTER` fires again — identical numbers every cycle, zero progress on
  the residual violations.

Two corrections fell out. The guard's `dup_split_attempts` term is ~75%
ledger *ROUTE recoveries* (edges re-routed to the product recorded on an
earlier pass), 0% filter re-derivation, and ~25% **`v>0` self-product** —
and that last quarter is 100% of it on every program: the ledger's
recorded product for the key IS the contour being split, the
`nviol_this_pass == 0` gate closes, and the fallthrough mints a fresh
contour every pass forever. That is the `TYPE_CONFLUENCE` growth, and `PYC_SELFPROD=5` now suppresses
it soundly: **1b's `nviol_this_pass == 0` gate was a whole-program proxy
for a per-CONTOUR property**, and a contour whose durable type key is
unchanged across two consecutive passes has settled even when the program
has not. Corpus: zero exit-code regressions, sunfish's 900 s timeout
becomes a 43 s compile, tictactoe converges naturally at 0 violations.
**Landed default-on 2026-08-14**; `PYC_SELFPROD=0` restores the old shape.
Two programs newly converge naturally. Still open, but not blockers:
msp_ss/softrender lose some precision, and neither newly-compiling program
*runs* — tictactoe now reaches codegen at 0 violations and trips pyc
[issues/035](../../issues/035-list-element-cast-salvage-guard-and-set-item-union.md)'s
int/float list gap (its read path lacks the guard its write path has),
which the convergence fix exposed rather than caused. And
`cur_split_stage` was never reset after `run_split_stages`, so the next
pass's flow-time contours were attributed to whichever stage ran last —
that is what made `reuse` read in the thousands for stages that re-bind
nothing. Full tables in 074.

Tree state at the end: `test_pyc.py` 265 passed / 14 expected fails / 0
failed / 4 skipped (both backends), `ifa --test` 58/0, zero exit-code
changes across the 84-program shedskin sweep.

## Current open issues

### FA — large, open-ended (the convergence / container-element-precision cluster)

These are intertwined: all trace back to the same underlying gap
(shared `list`/`dict` method contours don't discriminate by element
type, and split decisions aren't stably keyed across passes), per
the [033](closed/033-splitter-non-idempotent-divergence.md) →
[063](closed/063-no-type-bucket-triage.md) investigation lineage.

- [100-FA-display-removed-from-contour-identity.md](100-FA-display-removed-from-contour-identity.md)
  — the lexical display is now used ONLY for what it is for: `make_AVar`
  resolving an enclosing-scope Var (nested functions), plus clone's
  equivalence. Every use of it as *contour identity* is gone
  (`edge_nest_compatible_with_entry_set`, `edge_display_compatible`,
  `find_or_make_display_variant`, `group_display_ok`, Stage 4's
  live-slot machinery, and `update_display`'s consistency assert).
  Design decision, taken knowing the cost. Benefit: the display was a
  major contour-growth driver (074's census: 34-68 fresh contours per
  pass on yopyra from the lineage-mint alone) — ess drops 40-80%
  corpus-wide and yopyra converges. Cost: precision falls widely, the
  oscillator count nets 16 → 20. It also briefly broke two
  exception-path tests, which turned out to expose a *pre-existing*
  dropped-value bug in `flow_var_to_var` (an early return on an
  already-established link skipped the `b->in >= a->out` re-assert, so a
  value arriving after the link was created was never delivered); fixed
  the same day, suite back to 265/14/0/4. Retires 074's Stage 0/4 and
  invalidates the basis on which it ruled out Stage 2.
- [099-FA-pending-backedge-avoid-veto-forces-period-2.md](099-FA-pending-backedge-avoid-veto-forces-period-2.md)
  — a *structurally forced* period-2 oscillation, and the entire
  non-convergence of three programs (bh, pylife, linalg — 074's
  "stable residual" group). `check_split`'s pending-backedge route
  binds an edge to the lowest-id recorded contour after vetoing
  `avoid`, and `avoid` is exactly the contour the splitter is
  detaching the edge *from*; when the recorded set has two members
  the veto leaves precisely the one just vacated, so the edge swaps
  every pass forever with zero growth (0 new edges/EntrySets/
  CreationSets per pass). pylife's whole non-convergence is **one
  edge**. **Partially fixed 2026-08-13**: the flip-flop came from
  `record_backedges` re-homing an inherited entry's KEY onto the split
  product but copying its VALUE verbatim, so each contour kept a route
  back to its sibling. Re-homing the value with the key removes it —
  `loop` now converges (`plh=1` p38/2 viol → `plh=0` p58/0 viol),
  pylife 90→54 and sudoku4 160→142 violations, oscillators 17→16, zero
  exit-code changes on the sweep. But bh/pylife/linalg still do not
  converge: their churn RELOCATED into slow contour growth (074's other
  shape) rather than stopping, so the issue stays open on its second
  condition — the splitter re-deciding every pass.
- [098-FA-per-pass-reset-scoped-to-reachable-set.md](098-FA-per-pass-reset-scoped-to-reachable-set.md)
  — **mostly fixed 2026-08-12**, and it was upstream of 033/074's
  splitting-oscillation work, so their measurements are worth
  re-taking. FA re-derived flow state each pass over the subgraph *this*
  pass reaches, but `clear_results` reset it over the subgraph the
  *previous* pass reached (`fa->ess` is just `entry_set_done`), and
  didn't run at all on a `reanalyze()`-driven pass. 19-24% of edges per
  pass therefore carried an older pass's `args`/`rets`/`formal_filters`
  into the current one, and a stale `formal_filter` made `analyze_edge`
  skip a live edge permanently — leaving bound call edges with no values
  at their arguments at quiescence, which no reachable call can have.
  Fixed by resetting over authoritative registries (`FA::all_aedges` et
  al.) before every pass; also fixed a latent null-deref in
  `check_split` that the new trajectory exposed. Zero exit-code changes
  across the shedskin sweep, `test_pyc.py` unchanged on both backends,
  invariant now 0 on every pass everywhere (was up to 1207 on `chess`),
  and a permanent `IFA_DBG_EDGEARGS` audit guards it. **Still open:**
  `EntrySet::out_edge_map` is never reset, which makes a total dispatch
  failure invisible to `collect_argument_type_violations` — clearing the
  map is not viable (`get_AEdges` needs it for cross-pass edge
  identity), so the collector needs the fix instead. Supersedes the
  original "order-dependent per-pass fixed point" diagnosis, which the
  measurements refute.
- [074-FA-cross-pass-oscillation-plan.md](074-FA-cross-pass-oscillation-plan.md)
  — the master plan, **substantially re-measured 2026-08-12/13** (see the
  dated session section above). Target set re-based from 17 programs to
  8 by disabling the stall guards; growth mechanism re-censused after
  [100](100-FA-display-removed-from-contour-identity.md); Stage 0 and
  Stage 4 retired, and the basis for ruling out Stage 2 invalidated.
  **The churn is now stage-attributed: only `TYPE_CONFLUENCE` and
  `MARK_TYPE` produce it** — nothing measurable from the other seven
  stages (with the caveat that the first-stage-wins cascade starves
  them). Investigation flags landed off-by-default: `PYC_HARDREUSE`,
  `PYC_TYPEKEY`, `PYC_CANON`, `PYC_NOMARK`, `IFA_DBG_STAGE`,
  `IFA_DBG_KEYSPACE`. **`MARK_TYPE`'s splits are shown unnameable by any
  type-tuple scheme, and a net loss on this corpus** — `PYC_NOMARK=1`
  gives −26% analysis time, −12% contours, one more program compiling,
  and an unchanged test suite, at the cost of precision on five programs.
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
  flow-sensitive refinement. `is None` on a class-or-None union works
  end-to-end; `isinstance` picking the right branch over a union of
  user classes also works, but via an unrelated shared-clone-mis-fold
  fix, not real narrowing. The other three originally-filed cases
  (phi-merge re-discrimination, real narrowed-value use, `==`-constant
  return-type narrowing) all turn out to be
  [018](../../issues/018-dict-mixed-key-types-boxing-failure.md)'s gap
  in disguise — a raw scalar union has no coherent runtime
  representation at all, so narrowing (even if built out further)
  wouldn't fix them; tracked there now, not here.
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
  class 033 found and fixed elsewhere in `fa.cc`, unconfirmed. Its own
  filed ASAN-soak verification plan hit a blocker — see 094.
- [094-FA-asan-heisenbug-blocks-sanitizer-diagnostics.md](094-FA-asan-heisenbug-blocks-sanitizer-diagnostics.md)
  — found attempting 041's own ASAN soak: an intermittent
  `PycModule::filename` corruption/segfault on the simplest possible
  ASAN-built input, which stopped reproducing the moment any
  debugger or debug print looked at it. Suspected (not confirmed)
  Boehm GC conservative-scan root miss under ASAN's altered stack
  layout — the same disease class 041 itself suspects, caught
  elsewhere. Calls into question whether an ASAN soak is a reliable
  technique for this codebase's intermittent-segfault bugs at all.
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
- [086-FA-self-recursive-copy-arg-notype-cascade.md](086-FA-self-recursive-copy-arg-notype-cascade.md)
  — a self-recursive function whose recursive call passes
  `arg.copy()` (not the parameter directly) degrades entirely to
  NOTYPE and crashes at runtime on its first call, every time.
  Minimal 3-line repro, not container-specific. Real-world trigger:
  the classic Norvig sudoku-solver backtracking idiom
  (`shedskin_examples/sudoku2`, `sudoku4`).

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
- [090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md](090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md)
  — a loop-carried variable whose tuple arity changes each iteration
  (`t = t + (i, i+1)`) or whose type spans `None`/tuple (`move = None`
  then reassigned inside the loop) fails with "unable to resolve to a
  single function at call site" — a clean compile-time reject, not a
  crash, but possibly a genuine architectural limit (tuples are
  fixed-arity types, per closed-069) rather than a bug with a real
  fix. sunfish's real blocker (past the stale "sizeof_element"
  claim and the `sum()`-missing-`start`-arg gap, both resolved this
  session).
- [093-CGEN-int-float-union-move-not-coerced.md](093-CGEN-int-float-union-move-not-coerced.md)
  — a plain MOVE (not a binop — see closed-062) storing an int-typed
  value into a variable FA unified to `float64` isn't coerced. C
  backend gets the value right but the wrong `__str__` (`1.0` not
  `1`); LLVM backend stores the raw int bits into the float slot with
  no `sitofp`, producing a completely wrong value
  (`4.94...e-324`). Found via `7.py`'s real-argv-triggered branch.
- [097-CGEN-callsite-vs-clone-formal-type-mismatch.md](097-CGEN-callsite-vs-clone-formal-type-mismatch.md)
  — **PARTIAL**: an ordinary call site's actual argument type can
  diverge from the specific callee *clone's* formal parameter type
  (`emit_send_call` now guards the unsafe scalar-into-voidish-formal
  direction, same 056/077/096 convention — `msp_ss.py` compiles clean
  as of this fix). Root cause traced and confirmed **not a duplicate**
  of 076/030/018/045: `entry_set_compatibility` (`fa.cc:1059`) scores
  a candidate `EntrySet`'s compatibility against a momentary snapshot
  of its accumulated formal type, taken *before* the ES's own
  already-committed callers had their contribution (re-)flowed in that
  pass — directly confirmed by instrumentation (a `str.__eq__` edge
  scored fully compatible with exactly 1 of 7 candidate `EntrySet`s:
  the one whose type happened to be momentarily unpopulated). A
  resequencing fix was implemented and **reverted** — it regressed 3
  tests by tripping a more fundamental, pre-existing gap, now filed
  separately as [098](098-FA-per-pass-reset-scoped-to-reachable-set.md).
  This issue's own fix is blocked on 098 landing first.

### LLVM

- [095-LLVM-str-or-none-union-wrong-value.md](095-LLVM-str-or-none-union-wrong-value.md)
  — a `str | None` local's `is not None` check misbehaves and reads
  back a garbage value on the `None` branch, LLVM-only (C backend
  correct). Found implementing a real `getopt.py`. Not yet traced past
  a minimal repro; possibly related to 093's union-storage family, not
  confirmed the same mechanism (no numeric coercion needed here).

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

Currently 64 closed issues:
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
[080](closed/080-LLVM-index-type-mismatch-no-salvage-guard.md),
[081](closed/081-FA-int-mult-bool-constant-fold-segfault.md),
[082](closed/082-narrowing-wrapper-names-hardcoded-in-fa.md),
[083](closed/083-CGEN-print-println-name-collision-risk.md),
[084](closed/084-CGEN-LLVM-bool-constant-name-matching-workaround.md),
[085](closed/085-CGEN-dead-if-unresolved-condition-no-guard.md),
[087](closed/087-DISPATCH-out-of-order-keyword-args.md),
[088](closed/088-llvm-class-list-field-plus-construct-segfault.md),
[089](closed/089-DISPATCH-closure-pyc-to-bool-no-candidate.md),
[091](closed/091-DISPATCH-nonrecord-builtin-constructor-not-first-class.md),
[092](closed/092-DISPATCH-3arg-minmax-plus-multi-shape-return-crash.md),
[096](closed/096-extend-c-call-salvage-guard-past-str-comparisons.md).

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

- [101](101-FA-first-time-forever-splitting.md) — the residual non-convergence (`go`, `linalg`, `plcfrs`) is first-time-forever splitting, not re-derivation.
- [102](102-corpus-programs-compile-then-abort-at-runtime.md) — 27 of 68 corpus programs compile cleanly and then abort at runtime on unresolved dispatch; no sweep or harness in this repo sees it.
