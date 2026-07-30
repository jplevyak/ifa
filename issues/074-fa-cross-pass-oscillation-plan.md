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

### Stage 0 — the pivotal experiment (above)
Re-run 064's method-`nesting_depth 0` with the `check_split` fix; measure
`recursive_polymorphic`, the suite (both backends), and the oscillating
set. Record which branch we're on before writing anything larger.

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

### Stage 2 — main-loop CS-directed ES fan-out (065's linchpin)
A new split stage in `run_split_stages`, running **every pass** (not on
quiescence — that is the circularity break), on a **demand signal** (so no
explosion): when a method ES's receiver arg is a union of **same-TYPE CSs
with divergent element types**, create one product contour per receiver CS
and route each CS's edges/flow to it. Key it on Stage 1's stable
CS-creation-site signature for issue-033 idempotence. This is what
`PER_CS_RECEIVER` cannot do (it separates at creation, not a union
receiver) and what stops the union from growing at its source.

### Stage 3 — compose with the existing stages / phase ordering (066)
Reach the ES-split fixpoint, then the CS split, and do not let a CS split
re-open an already-decided ES split. The `e->to` durability already nearly
gives this for ESs; the missing half is not re-deriving a decided ES split
from a CS change.

### Stage 4 — drop the method display (only if Stage 0 permits)
With per-level separation now on the CS/type axis, set genuine methods'
`nesting_depth` to 0 (distinguishing them from the issue-001 synthesized
closure carriers, which keep it), dissolving 064 and simplifying the
display machinery. Skip if Stage 0 shows the display is still load-bearing.

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
