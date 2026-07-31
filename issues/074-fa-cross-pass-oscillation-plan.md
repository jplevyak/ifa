# 074 — Plan: solve the FA cross-pass splitter oscillation (033/063/065/066 master plan)

**Status:** plan, 2026-07-30. Synthesizes and *sequences* the existing
diagnostic work in [033](033-splitter-non-idempotent-divergence.md) /
[063](063-no-type-bucket-triage.md) /
[064](064-method-phantom-display-blocks-es-split-routing.md) /
[065](065-mark-stage-es-split-routing-and-growing-product.md) /
[066](066-cs-split-decision-keyed-per-pass-not-per-creation-site.md) into
one actionable build order, **re-grounded on current measurements** and
updated for the one thing that changed since that work (2026-07-23): the
[073](073-teach-splitter-productive-vs-inert-context.md) `check_split`
type-identity fix (2026-07-30), which ties recursion knots by *type*
identity and may have dissolved the central obstacle those issues hit.
**Affects:** `ifa/analysis/fa.cc` — `run_split_stages` (the 8-stage
sequence), `split_for_per_cs_method_receivers`, `split_ess_setters`/
`split_css`/`creation_point`, the issue-033 ledger (`ledger_*`,
`cs_group_signature`, `setter_site_signature`); `python_ifa_build_syms.cc`
`def_fun_pyda` (method `nesting_depth`).

## The problem, measured (2026-07-30, post-`check_split`-fix)

`check_split` (073) fixed the *intra*-pass unbounded generator. A distinct
*cross*-pass oscillation remains: FA runs to the pass cap / stall guard
(`pass_limit_hit=1`) with (usually) residual violations, never converging.
Measured across the whole corpus (full FA, `PYC_DBG_OSC` probe on
`FA::analyze`'s final pass): **17 of ~77 programs oscillate**
(`pass_limit_hit=1`):

| program | final_pass | violations | ess.n | | program | final_pass | violations | ess.n |
|---|---|---|---|---|---|---|---|---|
| softrender | 30 | 881 | 775 | | loop | 62 | 64 | 1566 |
| sudoku5 | 21 | 511 | 926 | | dijkstra2 | 43 | 170 | 946 |
| rubik | 33 | 417 | 875 | | linalg | 16 | 170 | 1025 |
| amaze | 16 | 884 | 688 | | sudoku3 | 16 | 105 | 472 |
| pylife | 41 | 90 | 399 | | yopyra | 55 | 86 | 822 |
| timsort | 17 | 66 | 376 | | chess | 39 | 63 | 3132 |
| go | 28 | 56 | 604 | | sudoku4 | 40 | 38 | 1964 |
| genetic2 | 50 | 3 | 650 | | bh | 53 | 2 | 530 |
| **pygmy** | **102** | **0** | **466** | | | | | |

Two shapes are present. Most have **residual violations** (the growing
union prevents a clean type fixpoint). **pygmy** is the pure case: it hits
the *hard* pass cap (100) with **0 violations** — types have converged but
the splitter keeps re-deriving/re-minting the same partition forever
(pure 033 non-idempotence, no union confound). (Calibration: slow-but-
*converging* programs — chull 45, chaos 35, adatron 28 — sit at
`pass_limit_hit=0, violations=0`; high pass count alone ≠ oscillation.)
**dijkstra2 — the canonical 063/065/066 canary — still oscillates**,
confirming this is a live, distinct issue the `check_split` fix
(intra-pass) did not touch. genetic2/adatron (this session's compile
wins) *compile* but genetic2 is in the oscillating set — it compiles
**despite** hitting the cap, with 3 residual violations.

## Root cause (established by 065/066)

Two coupled mechanisms; both must be fixed:

1. **A genuinely growing container-element union** (043 shape B).
   Shared container methods (`list`/`dict` `__getitem__`/`__eq__`/`__len__`
   /`__setitem__`/…) run over a heterogeneous element union; as more of the
   program resolves, the union widens, so the split products keep widening
   and never settle (065 "gap 2 / the deeper reason").
2. **Unstable cross-pass split-decision identity** (066). The durable
   split decision is keyed on the *re-created per-pass* CreationSet / the
   per-(Var,EntrySet) setter AVars — which shift as splitting proceeds — not
   on the stable source creation site. So re-flow re-derives a *different*
   partition each pass and the ledger has nothing stable to route to
   (why the issue-033 routing excludes the setter/mark stages).
3. **Circularity in the current architecture.** `run_split_stages` runs 8
   gated stages, one per pass; the per-element-CS method split
   (`split_for_per_cs_method_receivers`, `PER_CS_RECEIVER`) runs **only on
   quiescence of stages 1-5** — which an oscillating program never reaches
   — so the split that would stop the union never fires (065's final
   update). And `PER_CS_RECEIVER`/`clone_methods_per_cs` separate CSs *at
   creation*, so they cannot fan out a *union receiver* arriving on a
   recursive edge anyway.

## What changed since the 2026-07-23 analysis — the pivotal unknown

Every 064/065 attempt to let the CS partition (not the display) carry
per-recursion-level separation was blocked by the same wall: **zeroing
method `nesting_depth` re-fused `recursive_polymorphic`'s recursion
levels** (`len`/`__getitem__` over `list[list[int]]`→`list[int]`→`int`
collapsed to a union). But that re-fusion was **soft type-merging** — a
type-incompatible contour reused with a `val−4` penalty — which is exactly
the mechanism 073's `check_split` fix now **hard-gates** (it ties a
recursive knot only on `edge_type_compatible == 1`, and the same soft
merge was what regressed `match_seq` before I required the hard match).

> **RESOLVED 2026-07-30 (see "Stage 0 result" below): the second branch
> holds.** Dropping the display still re-fuses `recursive_polymorphic`
> (suite 231/4), because `check_split`'s hard-gate covers only the
> recursion-*routing* branch, while the container methods `len`/
> `__getitem__` that actually re-fuse are *normally dispatched* and still
> re-merge through `find_best_entry_sets`'s soft type match. The CS
> partition (Stages 1-2) is required; the cheaper "hard method-dispatch
> type gate" substitute was tested and **ruled out** (it breaks
> convergence — see Stage 0 result). So Stage 2 is unavoidable.

**So the single most decisive experiment for this plan is to re-run
064's `nesting_depth 0` prototype *with* the `check_split` fix in place**
and measure `recursive_polymorphic`. Two outcomes, each collapsing the
plan differently:

- **If it no longer regresses** (type identity now supplies the per-level
  separation the display was faking): the method display is *finally*
  droppable, 064 dissolves, and much of the oscillation may resolve with a
  far smaller change than the full CS-fan-out — possibly just "methods
  `nd 0` + the existing setter-site routing." Re-measure the oscillating
  set immediately.
- **If it still regresses**: the CS partition genuinely must carry the
  separation, and the full build below (Stages 1-2) is required.

Do this experiment first; it decides how much of the rest is needed.

## The build (in order; each step gated on full verification)

### Stage 0 — the pivotal experiment — DONE 2026-07-30: display still load-bearing

Ran it (a clean FA-side proxy for "methods `nd 0`" that avoids 064's
make_AVar resolution desync: behind an env flag, `edge_nest_compatible_
with_entry_set` / `group_display_ok` / `edge_display_compatible` all
ignore the display and `update_display` tolerates differences, so the
display is dropped from contour *identity* but still built for
resolution; reverted after). **Result: suite 231/4** —
`recursive_polymorphic` (level-refusion), `match_none`, `match_map_star`,
`match_seq`. **So the `check_split` fix did NOT make the display
droppable; the second branch holds — the CS partition must carry the
separation (Stages 1-2 required).**

Mechanism (confirmed from the errors): `recursive_polymorphic`'s
`flatten_sum` is `nd 1` (display `[module]`, already always-compatible —
so it was never the display's job). The separation the display provided
was for the *normally-dispatched container methods* it calls — `len` /
`__getitem__` (`nd 2`), whose `display[1]` = the per-level `flatten_sum`
contour kept `len(list[list])` and `len(list[int])` in distinct
contours. `check_split`'s type-identity knot-tying only covers the
*recursion-routing* (`e->from->split`) branch, **not** normal method
dispatch, so it does not replace this. With the display gone, the
re-fusion happens through `find_best_entry_sets`'s **soft** type match
(`val−4` reuse of a type-incompatible contour), so `x[i]` unions
`list ∪ int` and feeds back into the recursive formal `x` → the illegal
`'x' illegal: int64`/`list`.

**Hard-type-gate sub-experiment — DONE 2026-07-30: DEAD END.** Tested
"drop the display **and** make method-dispatch type matching hard"
(`entry_set_compatibility`'s `case 0: val−=4` → `return 0` behind a
`PYC_HARDTYPE` flag, so `find_best_entry_sets` never soft-reuses a
type-incompatible contour). Result: **`recursive_polymorphic` times out
(non-convergence) — and it does so with `HARDTYPE` alone, display kept**
(a currently-*passing* program). So a *global* hard type gate is strictly
worse than the status quo. The reason is the deep one 040/033 gestured at
and this makes explicit: **soft type matching is load-bearing for
convergence, not just laziness.** `val−4` lets a contour *absorb* type
widening as it flows (an EntrySet is a widening point); forbidding that
mints a fresh contour per intermediate type state, so the contour set
churns as types converge and the flow fixpoint never settles. Making the
type gate hard is exactly the eager fan-out `#if 0 // eager splitting
doesn't help` already disables one line above the edited case.

**Consequence:** the display's per-level separation *cannot* be replaced
by a cheap global type-gate change; the separation must be **scoped and
demand-driven** — created only where a same-type receiver's element types
actually diverge, and only for the affected container methods. That is
precisely Stage 2 (the CS-directed fan-out). The shortcut is ruled out;
Stage 2 is required.

### Stage 1 — stable, creation-site-keyed durable CS/setter identity (066)
Independently of Stage 0, key the durable split decision on the **stable
source site** (066's `gx.alloc_info` analog): a persistent
`creation-site → CS-duplicate` map, populated when a split is *decided* and
re-applied verbatim by `creation_point`/`clear_cs` on re-flow instead of
re-derived. This is the generalization of the already-landed
`setter_site_signature`/`cs_group_signature` ROUTE (066 part 1) from "a
per-pass routing hint" to "the durable identity of the split." Makes the
partition reproduce deterministically across passes → the ledger has a
stable target → the setter/mark stages can route (065 gap 1) without the
wrong-merge hazard.

**Started 2026-07-30 — measurement corrected the target; a churn-bound
landed as increment 1a.** Instrumenting per-pass stage/dup counts
(probe removed) showed **pygmy is NOT a CS-side case** (`cs_dup=0`): it is
the **ES type-split re-deriving the *same* 3 decisions every pass**
(`win=type_confluence dup=3`), with a completely frozen state
(`ess=466 css=1599 viol=0` identical from pass ~40 to the cap). So Stage 1
has two independent targets: (i) the **ES type-split** re-derivation
(pygmy, and the machinery behind the `dup_split_attempts` ledger at the
`TYPE_CONFLUENCE` stage), and (ii) the **CS creation-site keying** (066,
for the container-union-growth `v>0` oscillators, which show `cs_dup`
activity). The determinism root — *why* `collect_type_confluences` +
`decide_entry_set_split` re-detect and re-split the identical 3 groups
every pass after `clear_results`, and why the ES ledger ROUTE stops the
`ess` growth but not the `analyze_again=1` signal — is the real (ii)/(i)
fix and is not yet built.

- **Increment 1a — LANDED (churn bound, not the root):** the stall guard
  was gated `if (v > 0)` (`fa.cc`), so a *zero-violation* pass that
  re-derives a split (`dup>0`) yet adds no new contours (`ess`/`css`
  unchanged) — pure issue-033 churn — was never bounded and ran to the
  hard pass cap. Added a symmetric zero-violation branch (same
  `stall_limit`, reset on `ess`/`css` growth): pygmy 102 → 48 passes,
  same result. **Suite 235/0 both backends; full corpus sweep identical
  (zero category changes).** This is a *bound* on the pure-churn case,
  not the determinism fix — pygmy still sets `pass_limit_hit`; the root
  (i)/(ii) work is what makes such passes not re-derive in the first
  place.

- **Root of pygmy's re-derivation — FOUND 2026-07-30: issue 065 gap 2
  (self-product ES re-mint).** Instrumented the two ES-type ledger dup
  sites (probe removed). In the frozen state pygmy's 3 dups/pass are all
  `shade`/`getreflected` (a softrender's `Shaderinfo` shading path) at a
  **monomorphic** partition `part=[Shaderinfo]` — i.e. there is *no real
  type union* to separate; the split is a pure non-idempotence artifact.
  The signature: `shade` es=430 logs `DUP-MINT product=430`, i.e.
  `d->product == es` — the ledger recorded `es` as its *own* product, so
  the ROUTE guard `d->product != es` (`fa.cc:4659`) fails, there is
  nothing to route to, and it re-mints; its sibling es=431 `DUP-ROUTE ->
  430`; and the group's edge count `ngrp` alternates 1↔2 pass-to-pass — a
  **period-2 flip-flop** of the edge partition between the two sibling
  ESs. This is exactly the **065 "gap 2 — self-product re-minting"**
  case, and the one 066 part 1 left **NOT enforced** (its ROUTE only
  fires when `d->cs_product != cs`). pygmy is the *stable* variant of it
  (frozen, 0 violations) rather than 065's growing-union variant.
  **Fix direction (065/066's deferred half):** on the self-product case
  (`d->product == es`), do not re-mint — recognize the group's home *is*
  `es` and instead **evict the complement** (the flip-flopping sibling
  edges) to *their* recorded home, so `es` re-monomorphises to its
  recorded group and the partition stops oscillating. 065 measured that
  the naive "just keep the group in `es`" makes things far worse (37→605
  on dijkstra2 / 227→226 crash on `pyc_declare`), so the eviction must be
  paired with the stable creation/site keying (this Stage's (ii)); that
  pairing is the concrete next build. Note this is the **ES** self-product
  (`d->product`), sibling to the **CS** self-product (`d->cs_product ==
  cs`) 066 part 1 deferred — the same disease on both axes.

- **Naive complement-eviction attempt — DONE 2026-07-30: does NOT work
  (needs the stable keying).** Behind a `PYC_SELFPROD` flag, on a
  self-product re-derivation (`d->product == es`) the group's re-split was
  *suppressed* (its edges left durably where they are, via `continue` in
  `apply_entry_set_split` after `gsig`). Measured (probe removed):
  - **pygmy 49 → 102 passes (worse)**, still 0 violations. Suppressing the
    group removes its `dup` signal, which *disables* increment 1a's
    zero-viol stall (it keys on `dup>0`), so pygmy runs to the hard cap —
    and the confluence is simply re-detected every pass without being
    resolved. The skip *relocates* the churn; it does not stop it.
  - **dijkstra2 identical** (43 passes, 170 violations) — no help, but
    (unlike 065's reverted "keep the group in `es`", 37→605) **no
    regression**: `continue` does not *move* edges into `es`, so it never
    makes `es` polymorphic. It just leaves the confluence unresolved.

  Conclusion: suppression is not eviction. Correct eviction has to move
  the *widening complement* off `es` so `es` re-monomorphises to its
  recorded group — which requires knowing *which* current edges are that
  recorded group, i.e. **stable per-edge/creation-site identity ((ii))**.
  That identity is the prerequisite; the self-product handling cannot be
  done first. So the build order is fixed: **(ii) stable creation-site
  keying → then complement eviction on both the ES and CS self-product**.

- **Scoping + working prototype — 2026-07-30. The eviction DIRECTION is
  correct; the crude form regresses; the refinement is identified.**

  *Lifecycle facts (grounding the "stable key" question).* `clear_edge`
  (`fa.cc`) clears an edge's *flow* (args/rets/filters) but **not**
  `e->to` (its product ES), `e->from`, `e->pnode`, or `e->match`; and the
  `split_ledger` is **not** in `clear_results`. So the durable substrate
  already exists — edge structure + `e->to` + the ledger survive passes;
  only AVar types and `cs->defs` re-derive. Consequence for the ES side:
  the ledger's `group_signature` (arg/ret types) is *already* a stable
  key when types have converged (pygmy's do — `[Shaderinfo]`), so the ES
  self-product does **not** need a new keying map; it needs the eviction.
  (The CS side / 066 is separate: `creation_point` re-derives `cs->defs`
  each pass and *does* need creation-site keying — that is Stage-1 (ii)
  proper, still unbuilt.)

  *Prototype (PYC_SELFPROD2, reverted).* On a self-product group
  (`d->product == es`), keep the group in `es` and evict the **complement**
  (all other edges currently in `es`) to a fresh product, so `es`
  re-monomorphises. Measured:
  - **pygmy converges NATURALLY** — `pass_limit_hit` 1 → **0** (43 passes,
    0 violations); the flip-flop is gone. **Suite 235/0** (C).
  - **dijkstra2 identical** (no regression) — unlike 065's reverted
    "keep the group in `es`" (37→605); this evicts rather than accreting.
  - **But amaze (v884→915) and linalg (170→187) REGRESS** — more
    violations. Cause: evicting the *entire* complement into **one**
    product merges genuinely-different-typed complement edges into a
    polymorphic contour (065's own hazard, moved off `es` onto the comp).

  *Refinement — LANDED 2026-07-30 as increment 1b.* The refinement first
  tried was "evict `stay_edges` to their own product, separate from the
  other groups" — but it **still** regressed amaze/linalg (884→915,
  170→187). That disproved the merge hypothesis and revealed the real
  discriminator: the self-product ledger decision is only valid when types
  have **converged**. pygmy oscillates at **0 violations** (a spurious
  precision flip-flop — eviction is safe); amaze/linalg carry many
  violations (union still widening — the recorded `home == es` is stale,
  so eviction mis-homes real content). **The fix that landed gates the
  eviction on `nviol_this_pass == 0`.** With that gate:
  - **pygmy converges naturally** — `pass_limit_hit` 1 → **0** (first
    oscillation *resolved*, not merely bounded).
  - **amaze / linalg / dijkstra2 and the rest of the set: unchanged**
    (regression gone).
  - **Suite 235/0 on BOTH backends; full corpus sweep identical to
    baseline (zero per-example changes).**

  Landed unconditional in `apply_entry_set_split` (`fa.cc`): on a
  type-stage (`avpos && gsig`), zero-violation, self-product
  (`d->product == es`) group, keep the group in `es` and evict the
  compatible `stay_edges` (now carried on `ESSplitDecision`) to a fresh
  product, once. Soundness: at 0 violations all types are consistent, so
  which contour holds the flip-flopping edges is a pure precision choice —
  resolving it cannot change codegen correctness. The **`v>0`
  self-product** (amaze/linalg/dijkstra2 — the majority of the oscillating
  set) still needs the genuine stale-vs-valid discrimination, i.e. Stage-1
  (ii)'s stable keying; that remains the next build.

- **Dup-category scoping — 2026-07-30. REDIRECTS the target: the
  oscillators are ES-side, NOT CS-side.** Before building the CS
  creation-site keying (Stage-1 (ii) / 066), categorized every cross-pass
  dup (probe removed) across the 17-program set into: `es_self`
  (self-product, 1b's case), `es_route` (ES route to a *different*
  product), `es_othermint` (ES group that *has* a recorded product but
  re-mints anyway), `filt`, `cs`. Cumulative over all passes:

  | prog | viol | es_self | es_route | es_othermint | cs |
  |---|---|---|---|---|---|
  | dijkstra2 | 170 | 3 | 27 | **83** | **0** |
  | sudoku5 | 511 | 12 | 112 | **154** | 0 |
  | rubik | 417 | 13 | **147** | 1 | 0 |
  | chess | 63 | 0 | 19 | 31 | 0 |
  | amaze | 884 | 21 | 68 | 2 | 0 |
  | linalg | 170 | 4 | 37 | 17 | 0 |
  | loop | 64 | 1 | 18 | 40 | 2 |

  **`cs ≈ 0` for the ENTIRE set** (max 6, on pygmy). So **066's CS
  creation-site keying is the wrong lever for the oscillation** — none of
  these programs churn on the CS side. The dominant churn is two ES-side
  categories:
  - **`es_othermint`** (dijkstra2 83, sudoku5 154, chess 31, loop 40 …):
    a group that *has* a recorded product but re-mints because the ES
    ROUTE (`fa.cc:4664`) is blocked by **`group_display_ok`** — i.e.
    **issue 064's phantom method display** (065 gap 1). This is the same
    load-bearing display Stage 0 proved cannot simply be dropped (it
    supplies container-method per-recursion-level separation), so
    unblocking the route needs that separation moved onto the CS/type axis
    first — **Stage 2**, not a keying map.
  - **`es_route`** (rubik 147, sudoku5 112, dijkstra2 27 …): a group that
    routes to its recorded product every pass yet still signals
    `analyze_again=1`. **Idempotent-route lever investigated 2026-07-30 —
    it does NOT exist.** Instrumenting the route application (probe
    removed): every routed edge is a **genuine move** (`es → product`);
    rubik measured `noop=0, moved=200`. So there are no "already-applied"
    re-routes to skip. The churn is a genuine **flow↔split oscillation**:
    the split routes the edge to `product`, then next pass *flow*
    re-dispatches the same call to `es` (the caller's dispatch target was
    never redirected to `product`), so the split moves it out again. The
    split *outcome* is identical every pass, but the edge really bounces,
    so it can't be recognized as a no-op. It is already **stall-bounded**
    (the `v>0` guard fires after `stall_limit` re-deriving passes) — so
    there is no unbounded growth here; the residual is that the split
    can't *resolve* its violations because its product never becomes the
    dispatch target. The real fix is **dispatch coherence** (redirect the
    caller's dispatch to the split product so flow stops undoing the
    split) — the `check_split` / `pending_es_backedge` / `out_edge_map`
    machinery — which is the deep 033 core, not a small idempotence tweak.

  **Consequence for the plan:** Stage-1 (ii) as "066 CS creation-site
  keying" is **deprioritized for the oscillation** (the CS side is quiet);
  it stays relevant only for genuine CS re-derivation (pyc_declare/pygmy's
  CS ROUTE, 066's own repros). Both cheap ES-side levers are now ruled
  out — `es_othermint` is 064's load-bearing phantom display (needs Stage
  2 first), and `es_route` is a genuine flow↔split dispatch-coherence
  oscillation (already bounded; needs the `check_split` dispatch-coherence
  fix, not a signal tweak). **So the oscillation's residual (`v>0`) has no
  small remaining lever: it reduces to (a) Stage 2 — move container-method
  separation onto the CS/type axis so 064's display becomes inert and the
  `es_othermint` routes unblock — and/or (b) making the ES-split product
  the caller's dispatch target so `es_route` splits stick.** Both are
  large. The session's tractable wins (1a, 1b) are landed; the remainder
  is the genuine 033/064 core.

### Stage 2 — main-loop CS-directed ES fan-out (065's linchpin) — MEASURED, RULED OUT 2026-07-30
A new split stage in `run_split_stages`, running **every pass** (not on
quiescence — that is the circularity break), on a **demand signal** (so no
explosion): when a method ES's receiver arg is a union of **same-TYPE CSs
with divergent element types**, create one product contour per receiver CS
and route each CS's edges/flow to it. Key it on Stage 1's stable
CS-creation-site signature for issue-033 idempotence. This is what
`PER_CS_RECEIVER` cannot do (it separates at creation, not a union
receiver) and what stops the union from growing at its source.

> **Measure-first result 2026-07-30: Stage 2 does NOT address the
> oscillation; do NOT build it.** Before prototyping, a temporary probe
> (`PYC_DBG_STAGE2`, in `apply_entry_set_split`, removed after)
> characterized every re-minting group (`othermint`/`route`/`self`) across
> 7 oscillators by (a) its cross-pass category and (b) the CS-union shape
> at the **split position** `avpos` (n CSs, all-same-container-type?,
> element-type union, elements-divergent?). Three findings, each fatal to
> the Stage-2 premise:
>
> 1. **The Stage-2 demand signal is a small minority of the churn.**
>    Re-mints whose split position is a *same-type container union with
>    divergent elements* (the exact fan-out target), as a fraction of all
>    re-mints: dijkstra2 **1/113**, sudoku5 **3/278**, rubik **5/161**,
>    amaze 9/91, loop 7/59, softrender 8/65 — only **linalg 19/58 (33%)**
>    is non-trivial (and those are mostly `len`/`deepcopy` over a container
>    arg, not a *receiver* fan-out). The dominant split shapes are instead
>    **unions of NON-container objects** (`cont=0`: sudoku5 200/278, rubik
>    120/161, amaze 49/91 — polymorphic object dispatch) and **pure-display
>    monomorphic re-mints** (`nCS≤1`: dijkstra2 62/113, softrender 45%,
>    loop 44%).
> 2. **There is no growing union for Stage 2 to stop.** Tracking the split
>    union's size across passes: sudoku5 `__eq__` *shrinks* 22→18→8→2 as
>    separation proceeds; rubik's 112 `__getitem__` re-routes are a
>    **single-pass burst** (pass 17) at a stable `nCS 2-3`. The 043-shape-B
>    "genuinely growing container-element union" 065 gap 2 posited is **not
>    present** in the measured oscillators — the unions are small and
>    static-or-shrinking; the re-derivation is pure cross-pass
>    non-idempotence, not growth.
> 3. **The churn reduces to the two mechanisms already named in the
>    Stage-1 dup-category scoping, both display/dispatch — neither a
>    container fan-out.** (a) **`es_othermint`** (dijkstra2 55% of re-mints
>    are `mono`, all **stage-0/TYPE_CONFLUENCE**, 60 across 32 contours;
>    sudoku5 `__getitem__` 66 + `__eq__` 46): the ES-split ROUTE is blocked
>    by **`group_display_ok`** on type-identical or type-partition-matching
>    groups whose edges span different caller displays = **064's phantom
>    display / 073**. (b) **`es_route`** (rubik `__getitem__` **×112**,
>    sudoku5/dijkstra2 `len`): the split routes to its recorded product but
>    *flow* re-dispatches the call to the original ES next pass = the
>    **flow↔split dispatch-coherence** oscillation.
>
> **Consequence.** The plan's premise chain for Stage 2 — "the display is
> load-bearing (Stage 0) *because* it supplies container-method
> per-recursion-level separation, so move that job to the CS axis (Stage 2)
> and the display goes inert" — **breaks at the middle link for the
> oscillators**: in the oscillators the display is NOT doing container
> separation (their re-mints are non-container/pure-display), so a
> receiver-CS fan-out cannot make their displays inert. Stage 0's
> load-bearing evidence (`recursive_polymorphic`) is a *separate, passing*
> program whose display genuinely does element separation; that program is
> a Stage-4 *regression guard*, not a member of the oscillating set. So the
> two are decoupled: **the oscillation's real levers are (a) the display in
> the ES-split ROUTE gate `group_display_ok` — extend 073's landed
> type-identity knot-tying (which fixed the `check_split` recursion branch)
> to the ROUTE, co-modifying `update_display`, so a type-partition-matching
> group routes across inert displays — and (b) dispatch coherence for the
> `route` population (the deep 033 core: redirect the caller's dispatch to
> the split product so flow stops undoing the split).** Stage 2 is retired
> from this plan.

### Stage 3 — compose with the existing stages / phase ordering (066)
Reach the ES-split fixpoint, then the CS split, and do not let a CS split
re-open an already-decided ES split. The `e->to` durability already nearly
gives this for ESs; the missing half is not re-deriving a decided ES split
from a CS change.

### Stage 4 — demote the display from the ES-split ROUTE gate (the real lever, per the Stage-2 measurement)
With Stage 2 retired (see its measure-first result above), this is the
**primary remaining lever** for the oscillation's dominant `es_othermint`
population, not a Stage-1/2-blocked cleanup. The measurement decoupled the
two display roles the plan had conflated:
- **In the oscillators**, `group_display_ok` blocks the ES-split ROUTE for
  groups whose *type partition already matches* a recorded product but
  whose edges span different caller displays (dijkstra2's 60 stage-0
  `mono` re-mints; sudoku5's `__getitem__`/`__eq__`). The display here is
  **inert** — it separates nothing the type partition doesn't already.
- **In `recursive_polymorphic`** (a passing program, NOT an oscillator —
  it is the Stage-4 *regression guard*), the display separates genuinely
  different element types through normally-dispatched `len`/`__getitem__`
  (Stage 0). That separation must survive.

073's landed `check_split` fix already resolved the analogous split in the
**recursion-routing** branch by reusing a contour only on a **hard type
match** (`edge_type_compatible == 1`), which preserves
`recursive_polymorphic` *by type*. The concrete Stage-4 build is the same
move one level up, in `apply_entry_set_split`'s ROUTE: when a group's
`(avpos, part, gsig)` matches a prior-pass product (already a type-partition
match), **route to it even when `group_display_ok` fails**, co-modifying
`update_display` (fa.cc:958 assert) to rebuild the display from the routed
edge rather than assert equality (064 item 2 / 073's "hard constraint on
the fix"). Because the ROUTE already keys on the type partition, relaxing
*only* the display gate there cannot merge type-different groups — the
exact safety `recursive_polymorphic` needs, and stronger than Stage 0's
blanket display-drop (which regressed via the *soft* `find_best_entry_sets`
match, a different site the ROUTE does not use). Verify by re-running the
Stage-0 nodisp probe and the full regressor list; a later cleanup can then
set genuine methods' `nesting_depth` to 0 (distinguishing the issue-001
synthesized closure carriers, which keep it). The `es_route` /
dispatch-coherence population (rubik) is *not* fixed by this and remains
the separate 033-core lever (b).
(The cheaper "hard method-dispatch type gate" alternative was tested and
ruled out — it breaks convergence; see the Stage 0 result.)

> **Prototype built + measured 2026-07-30 (behind `PYC_STAGE4`,
> flag-gated, not landed).** Implemented the display-liveness demotion:
> a per-`Fun` cached `max_live_display_slot` (`fun.h` +
> `fun_max_live_display_slot` in `fa.cc`) = the highest display slot the
> fun's body actually consumes in `make_AVar` (a referenced Var at
> `nesting_depth` k+1 owned by a proper ancestor scope); slots above it
> are inert. `group_display_ok` (type-stage ROUTE only, `!fsetters &&
> !fmark`) enforces only the live slots, and `update_display` asserts
> only the live slots, so a type-partition-matching group ROUTEs across
> inert (caller-context) display slots instead of re-minting. For the
> Python frontend the mask is empty above the module singleton (captures
> are lowered to explicit closure classes), so a genuine method's caller
> slot is inert; genuine V closures / issue-001 carriers reference an
> ancestor free var and keep a live slot, so their enforcement is
> unchanged.
>
> **Correctness/determinism gate — CLEAN:** suite **235/0 on BOTH
> backends** with the flag on; corpus sweep **identical 53-compiled set**
> (zero COMPILED→FAIL, zero swaps, diffed); `recursive_polymorphic`
> compiles clean (the type-partition `gsig` gate protects it — its
> contours differ by type, not just display). Flag-off is byte-identical
> to baseline (all logic gated on `stage4_enabled()`).
>
> **Oscillation gate — NET-POSITIVE BUT NOT PRECISION-NEUTRAL (so not
> yet landable):** violations at the cap — rubik **417 → 128** (−289;
> passes 33 → 21), sudoku5 **511 → 434**, dijkstra2 **170 → 140** (passes
> 43 → 36), loop **64 → 53**; amaze/linalg neutral. **Regressions:**
> softrender **881 → 895 (+14)** and pygmy compile-time **43 → 77 passes**
> (still converges, `pass_limit_hit=0`, 0 violations). **No oscillator
> reaches `pass_limit_hit=0`** — Stage 4 removes the `es_othermint`
> re-mint churn but the `es_route`/dispatch-coherence residual (lever b)
> still caps every program. The softrender/pygmy regressions are the
> issue-033 mint→route trajectory sensitivity, NOT lost data separation
> (`group_signature` keys on `->type`, which preserves non-constant CS
> identity, so the ROUTE cannot merge CS-different groups): routing a
> group into a *durable accumulated* product vs. minting a *fresh* one
> reaches a different frozen state — program-specifically better (rubik)
> or worse (softrender). So Stage 4 alone meets correctness but violates
> the "none regress" precision gate for two programs; landing it wants
> either lever (b) composed in (so the routed splits actually stick and
> converge, likely dissolving the trajectory noise) or a
> trajectory-stabilizing refinement. Kept behind the flag as a verified,
> documented partial.

## Verification plan (per step — issue-033 fragility demands it)

1. **Oscillation gate:** the measured oscillating set (all 17
   `pass_limit_hit=1` programs above) moves toward `pass_limit_hit=0` with
   strictly fewer violations; none regress. **pygmy is the cleanest unit
   test** — 0 violations, pure re-derivation, so a Stage-1 determinism fix
   alone should drop it off the cap without any precision change. Re-use
   the `PYC_DBG_OSC` probe (temporary, on `FA::analyze`;
   `final_pass`/`pass_limit_hit`/`violations`/`ess.n`).
2. **Suite:** `test_pyc.py` and `PYC_FLAGS=-b test_pyc.py` at **235/0**
   both backends (current baseline), zero regressions.
3. **Determinism gate + full corpus sweep:** no COMPILED→FAIL; watch the
   historically fragile casualties — 064's `recursive_polymorphic`/
   `exception_propagation`; 065's mark-routing losses `chess`/`mastermind2`
   /`sat`; 066's `pygmy`/`pyc_declare` — all must stay green.
4. **No new `check_split` regressions:** `match_seq`/`match_none` stay
   green (Stage 0/4 touch the same display machinery 073 relies on).

## Risks

This is the most-reverted surface in the tree (033 M2/M3, 064, 065's two
reverts, 066's self-product deferral). Every "small" change here has
needed a full land-verify-revert cycle against a *many-pass* corpus member
(the standard short-running suite is insufficient — see the fysphun
stage-2 segfault note in `run_split_stages`). Gate each step on the full
oscillation + determinism + corpus sweep, and prefer landing Stage 1
(pure determinism, no new precision) before Stage 2 (new fan-out).

## What this unblocks

Convergence of the whole "no type" / oscillation bucket (063): dijkstra2,
amaze, chess, go, linalg, loop, bh, genetic2 stop hitting the pass cap and
resolve their residual violations; the container-element-union family
(043 shape B) is closed at its source; and — via Stage 4 — the method
phantom display (064) is retired, simplifying the splitter long-term. The
corpus evidence that the *decide-then-durable-with-stable-keys* shape
converges is shedskin, which compiles the whole corpus this bucket is
drawn from.
