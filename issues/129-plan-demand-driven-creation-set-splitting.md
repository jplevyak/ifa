# 129 — THE PLAN: demand-driven CreationSet splitting

**This is the single integrated plan.** [128](128-cs-identity-over-discriminates-vs-element-type.md),
[131](131-demand-driven-constant-splitting.md),
[133](133-split-a-container-on-its-element-type.md),
[134](134-remove-the-frontend-forced-split-opt-in.md),
[146](146-remove-all-arbitrary-splitting.md),
[151](151-split-an-entryset-on-a-constant-argument-on-demand.md) and
[152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md) are steps
or constraints inside it; none of them carries a competing plan. If one
seems to, this file wins and the other should be corrected.

*Rewritten 2026-09-12. The previous 2886-line chronological record is in
git history; its numbers were from 2026-09-05 and were badly stale.*

## The goal, and the one lever

CLAUDE.md: IFA starts from the **minimum** contours and splits only on
demand. pyc meets this on the EntrySet side and not on the CreationSet
side — `creation_point` keys identity on *(allocation site × contour)*, so
data contours start MAXIMALLY split and never merge (`multidef=0`
corpus-wide: every CreationSet has exactly one creation point).

**The lever is `PYC_CSDCPA1=2` — start merged, one CreationSet per sym.**
Making it the default is the goal of this plan. It is the only thing
measured that moves the number, and it takes `ess` DOWN rather than up.

**The design it implements, in three steps:**

1. **Coarse identity by construction** — `dcpa1`. Not "reuse": one contour
   per sym, needing no evidence. (Why mint-time reuse keying is a dead
   frame: 128, "Where reuse fits".)
2. **Demand-driven separation** — give the precision back only where
   something observed a distinction and could not proceed. And the demand
   is observed where the union is USED, which is almost never where the
   merge happened — so step 2 is incomplete without
   [152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md),
   which walks the demand back to the CreationSet that actually merged.
3. **A ledger** so a re-derived separation re-attaches to the contour it
   first made instead of minting a fresh one. Matters MORE the coarser you
   start (`ledger_find_cs`/`ledger_add_cs`,
   `find_or_make_filtered_entry_set`).

## Where it stands — SUPERSEDED, see the 2026-09-15 line below

| | old default | `PYC_CSDCPA1=2` |
| --- | --- | --- |
| suite | **318 / 0** | 315 / **3** |
| corpus compile failures | **2** (othello3, rdb) | **7** |
| programs with warnings | 36 | **33** |
| container CS / shapes | 2768 / 627 = **4.41** | 2138 / 629 = **3.40** |
| `pratio` | 2.98 | **2.31** |

*(Measured 2026-09-12, when the flip was opt-in. Kept because the −23%
container-CreationSet result is what justified pursuing it.)*

### Measured 2026-09-15 — the bill is THREE, and the suite is clean

The flip is the default, and so are
[152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md)'s backtrack
and [154](154-FA-a-container-has-two-content-channels.md)'s two-channel
content fix. The "seven programs" bill below was paid by
[153](153-FA-positional-record-slots-lose-identity-and-reads.md) and 154, not
by the steps this plan predicted:

| | pre-flip default | **today's default** |
| --- | --- | --- |
| suite | 318 / 0 | **317 / 0** |
| corpus compile failures | 2 (othello3, rdb) | **3** (othello3, rdb, sudoku4) |
| total corpus warnings | — | **1364** (from 1973 at the flip alone, −31%) |
| container CS / shapes | 2768 / 627 = 4.41 | **2051 / 614 = 3.34** |
| `pratio` | 2.98 | **2.26** |

**−26% container CreationSets against the pre-flip default, with one more
compile failure instead of five more.** `bh` and `sudoku3` went from failing
to compiling AND matching CPython; `chull`, `plcfrs` and `sudoku5` compile;
`linalg` 108 → 33 warnings. The suite is 318 → 317 only because one test was
added and one `known_issue` was retired
(`arity1_literal_shares_contour` flipped to PASS — it pinned exactly the
merged arity-1 literal 154 fixed).

`sudoku4` is the one remaining regression, and it is understood:
[146](146-remove-all-arbitrary-splitting.md) E's "1 group: every creation
point on the same assign sets" decline. `PYC_ESBLOCK=1` fixes it and costs
`softrender` instead, so the count stays 3 — measured in 146.

## The remaining bill, attributed

**Suite — 3, of which 1 is bookkeeping:**

| test | owner |
| --- | --- |
| `splitter_mark_type` | **benign.** Only the `STAGES` line differs (`TYPE_CONFL SETTER` vs `TYPE_CONFL`); same warning, same `CALLS: direct=69 dynamic=0`. Re-bless at the flip. |
| `listcomp_element_separation` | [133](133-split-a-container-on-its-element-type.md) — fixed by `PYC_ESBLOCK`, which is opt-in because it costs plcfrs/sudoku5 through 146 E |
| `arity1_literal_shares_contour` | 133's separator question — arity-1 literals whose slot types differ but whose union is representable, so no demand is raised at the merge |

**Corpus — 7, of which 2 (othello3, rdb) also fail at the default. The 5
flag-only failures are two groups:**

| group | programs | first error | owner |
| --- | --- | --- | --- |
| element/slot union with no representation | plcfrs, sudoku3, sudoku5 | `'x' has mixed basic types: (…)`, two of them naming `tuple` | [146](146-remove-all-arbitrary-splitting.md) E |
| layout / blind cast | chull, sudoku4 | `object layout: 'Edge' is blind-cast to 'Vertex' … member width differs` | **not 135** — see below |

So **two mechanisms stand between here and the flip**, not a long list.

### Corrected 2026-09-12: it is SEVEN programs, not five

*(Historical. Paid down to ONE — `sudoku4` — by 2026-09-15; see the table
above.)*

The table above was measured with a **compile** sweep at the flag arm. The
flip is now staged on branch `stage-csdcpa1-default` and swept in **check**
mode, which shows two regressions a compile sweep cannot see:

| program | main | flip |
| --- | --- | --- |
| `bh` | run 0 | **run 134** — compiles CLEAN, then aborts |
| `kanoodle` | run 0 | **run 139** — compiles CLEAN, then SIGSEGVs |

These are the [102](102-corpus-programs-compile-then-abort-at-runtime.md)
class — the worst outcome this project names — and they were invisible in
every flag-arm measurement taken before, because all of them were
compile-mode. **Measure the flag arm in `check` mode from now on.**

It also changes how the five compile regressions read. Three of them
(`chull`, `plcfrs`, `sudoku4`) were ALREADY aborting or segfaulting at the
default, so the flip changes their failure MODE rather than losing working
behaviour — arguably an improvement, since a compile-time refusal beats a
silent crash. Only `sudoku3` and `sudoku5` were running correctly
(`sudoku3`'s sole stdout difference from CPython is the wall-clock line it
prints itself) and are genuine losses.

So the honest ledger for the flip is: **−23% container CreationSets, 3 fewer
warning programs, 3 fewer stdout mismatches, against 2 programs that stop
working, 2 that start crashing silently, and 3 that change how they fail.**

## The work, in order

**1. 146 E — separate a formal that holds N tuple CreationSets.** The
critical path. One shared, UNROLLED `tuple.__eq__`/`__lt__` contour serves
many tuple shapes; its single `x` per slot position merges their slot types
into `{int64, str}`, and every use is a BOXING violation. This is what the
removed CARTESIAN_PRODUCT splitter did by fanning, and
`tests/splitter_cartesian_product.py` already specifies what the
demand-driven replacement must be: demand (an unresolved dispatch or an
irrepresentable union) plus dispatch-aware filtering of the RECEIVER — the
position that decides dispatch in single-dispatch OO — never a fan over
receivers.

Clears plcfrs, sudoku3, sudoku5, and unblocks `PYC_ESBLOCK`, which clears
`listcomp_element_separation`.

**2. The layout pair — and `chull` is NOT a flip regression.**

Re-measured 2026-09-14: `chull`'s promoted-field layout is **byte-identical
at the default and under the flip** — same cross-class promotion, same
conflicting slots (`Edge.onhull → 22`, `Vertex.newface → 22`). The layout
defect is pre-existing and latent on main, which is why chull compiles there
and then SEGFAULTS (`run 139`). The flip only creates a union receiver that
makes the latent conflict reachable, so ifa/123's contract catches at
compile time what main corrupts at runtime.

So this row is not a cost of the flip; it is a pre-existing bug the flip
exposes. It was attributed to
[135](135-empty-sibling-contour-wins-the-clone-merge.md) and that is wrong —
`Edge` and `Vertex` are unrelated classes, so neither the prototype route
nor the empty-sibling clone merge applies. The real mechanism is
[issues/121](../../issues/closed/121-sibling-subclass-field-layout.md)'s,
incompletely fixed: `sorted_unknown_vars` name-sorts each promotion BATCH,
which does not align classes whose batches differ across `reanalyze` passes,
nor classes whose pre-promotion `has` counts already differ (15 vs 16 here).
Evidence and the layout dump are in 135.

`sudoku4`, `kanoodle`, `path_tracing` and `pygmy` were attributed on the
same evidence at the same time — **re-verify before trusting them.**

**3. 133's separator question.** Arity is currently the ONLY working
separator for list literals, and it is doing all the work alone. The
demand is unobservable at the moment of the merge and the merge is
unrecoverable at the moment the demand appears, so the answer is not a
better demand test — it is **which other representation properties of a
literal are knowable at construction**. The slot's basic-type kind is the
obvious candidate. Clears `arity1_literal_shares_contour`.

**4. 151 — constants separated on demand.** Not on the flip's critical
path, but constants are the largest identified share of what start-merged
destroys, and it is what lets [134](134-remove-the-frontend-forced-split-opt-in.md)
close (93 `__pyc_clone_constants__` annotations across 8 files, plus the 20
`fa.cc` sites they gate). Its acceptance test is that `bool.__not__`'s
annotation can be removed with ifa/150's corpus result intact.

**5. The flip itself — STAGED on `stage-csdcpa1-default`.** The branch
carries the one-line default change, the two `.known_issue` sidecars, and
49 re-blessed goldens (12 synthetic fixtures x 4 phases, plus
`04_setter_split.ir`), each diffed rather than blanket-re-blessed: `ess` and
`funs` are unchanged in all 12 and `css` is down in all 12. Merge it when
steps 1 and 2 land and the seven regressions above are gone; keep
`PYC_CSDCPA1=0` as the escape hatch for one release.

**6. [111](111-FA-selective-invalidation-per-pass.md) — convergence cost.**
A performance lever for the extra passes start-merged costs. NOT a
precondition: that claim was retracted (see below).

## What is settled, so it is not re-litigated

- **A merge CAN be taken back.** `analyze_to_convergence` resets *before*
  each pass, so flow state is re-derived from bottom every pass; what
  persists is the DECISION (`av->cs_map`), not the types. Demonstrated
  twice in code. 128 and 111 are **not** one change.
- **128 is not the blocker and pulls the other way.** An ES split mints
  FEWER CreationSets through `creation_point`, not more; and reducing the
  tuple CreationSet count costs ten corpus programs while making sudoku5's
  comparison contour worse (22 → 32 shapes). 146 E is the blocker.
- **`dcpa1`'s `tuple` exclusion stays.** Arity and position are part of a
  tuple's TYPE. Dropping it was measured on one binary, env toggled: 12
  compile failures against 2.
- **Mint-time reuse keying cannot work.** The key is evaluated when the
  element is unfilled by construction — 402 of 795 such mints never acquire
  a shape. `PYC_CSELEM=3` is suite-clean and still regresses `rdb`.

## Do not restart from these

`PYC_CSELEM=3` (superseded by dcpa1); `PYC_CSSITELESS` (made contours
worse on 4 of 5 programs); `PYC_CSRESPLIT`, `PYC_CSREJOIN` (inert or
superseded); `PYC_CSCALLSITE=3` and `PYC_WALKCTX` (reverted — a fan, and a
wrong premise; see 133 "Do not retry"); 131's cap-strip premise
(falsified by its own step 1 — `cstrip` measured 0/0/0).

## Verification at the flip

- Suite green on **both** backends with the flag on.
- `./corpus_sweep.sh -m check`, compared **per program**: `compile_rc`,
  `run_rc`, `cpy_rc`, `stdout_match`. Totals are not enough — two programs
  can swap and net to zero.
- `ess`/`css` down, not up. This exists so contours can merge safely.
- Same-binary, env-toggled A/B only. A 1–2 program movement between two
  BUILDS is not attributable ([147](147-analysis-result-depends-on-the-binary-not-the-inputs.md)).
