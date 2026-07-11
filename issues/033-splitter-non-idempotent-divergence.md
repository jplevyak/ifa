# Issue 033: Splitting loop has no fixed point on some inputs (non-idempotent, order-dependent split decisions)

**Status:** open, fix in progress. Divergence is *mitigated* (stall
guard, commit `21dbdad4`) so affected programs terminate quickly
instead of timing out; the root cause — split decisions that are
not idempotent across passes — is untouched. **The current plan
going forward is [S5](#s5-merge-plan-adopting-shedskins-round-structure-into-ifa)**,
which adopts shedskin's round structure (decide-from-converged-
state, persist decisions outside the graph) and supersedes the
original per-pass-ledger land order. Prerequisite work that already
landed (ledger observability, a violation-splitting cutoff, a
determinism/ordering audit) is summarized in
[Completed stages](#completed-stages); the `issue033-stage-c`
branch's status and role in the forward plan is in
[its own section](#issue033-stage-c-branch). Full implementation
detail for the landed work, and the original stage-by-stage ledger
design S5 supersedes, is archived at
[closed/033-ledger-design-detail.md](closed/033-ledger-design-detail.md).

**RESOLUTION UPDATE (2026-07-10, after the
[035](035-nondeterministic-codegen-clone-order.md) determinism
fixes): the core divergence symptom is GONE on main WITHOUT stage
C.** All three diverging examples now reach genuine fixed points
with the stall guard never firing — fysphun 18 passes/0 violations,
kmeanspp 21/6, pylife 13/60 — i.e. much of the "non-idempotence"
was heap-layout nondeterminism re-rolling split decisions between
passes (the issue-009/021 family, living in the container library
and SSU liveness rather than the splitter itself). The splitter's
per-pass amnesia is still real, but with deterministic re-derivation
it re-makes the SAME decisions, which is enough for a fixed point
on the known inputs — for now. The remaining payoff (pygasus-scale
plateau cost, see S2) is what S5 targets.

**Affects:** `ifa/analysis/fa.cc` — `extend_analysis()` and every
`split_*` stage it drives; `analyze_to_convergence()`'s outer loop
contract.
**Related:** [032-fa-survey-findings.md](032-fa-survey-findings.md)
(survey umbrella; B4/P3 fixed one instance of the same class inside
stage 4), closed
[009-fa-violations-nondeterminism.md](closed/009-fa-violations-nondeterminism.md)
and [021-v2-call-arg-swap.md](closed/021-v2-call-arg-swap.md) (the
"iteration-order dependence" family this belongs to),
[034-pygasus-update-display-assert.md](034-pygasus-update-display-assert.md)
(crash unmasked by the mitigation),
[037-matcher-cartesian-cs-product.md](closed/037-matcher-cartesian-cs-product.md)
(S4-C's sibling on the match side, landed — see S3(a)).

## Symptom

Five shedskin-corpus examples (fysphun, kmeanspp, pygasus, pylife,
stereo — pyc issue 025's "compile timeout" bucket) never finished
FA: `analyze_to_convergence()` looped with per-pass cost growing
superlinearly. `IFA_PASS_LIMIT` (100) exists but was unreachable in
wall time.

Measured trajectory on fysphun (`pyc -v`, with the ess/css/violation
counts added to the per-pass report in `21dbdad4`):

```
pass  7: 199 ess, 548 css,  84 violations   <- best ever reached
pass  8: 210 ess, 550 css,  99 violations   \
 ...          (plateau at 99)                | 8 passes, no improvement
pass 15: 224 ess, 560 css,  99 violations   /
pass 18: 367 ess, 578 css, 226 violations   \
pass 19: 497 ess, 578 css, 326 violations    |
pass 22: 850 ess, 578 css, 598 violations    |  oscillation: ess and
pass 23: 589 ess, 578 css, 397 violations    |  violations swing up
pass 24: 875 ess, 578 css, 617 violations    |  and down together
pass 26: 878 ess, 578 css, 620 violations    |
pass 27: 617 ess, 578 css, 419 violations   /
pass 26 cost 9.9s, pass 27 cost 17.9s (98%/77% in extend)
```

The state never exactly repeats (no simple cycle), it *oscillates
with drift* — so neither a pass cap nor exact-state cycle detection
catches it at useful cost.

## Root cause (as far as traced)

The outer loop `do { flow-to-fixpoint } while (extend_analysis()
|| reanalyze(...))` is only a fixed-point iteration if
`extend_analysis()`'s split decisions are **monotone and
idempotent**: a split made in pass k should stay made, and
re-presenting the same imprecision should not split again. Neither
holds:

1. **Split state is per-pass.** `clear_splits()` zeroes
   `EntrySet::split` / `CreationSet::split` every pass, and
   `clear_results()` throws away the flow state that justified the
   split. The next pass re-derives confluences from scratch on the
   *new* contour set and makes fresh decisions.

2. **Decisions are order-dependent.** Which confluence gets split
   first determines which ESs exist when the next confluence is
   examined; `split_ess_for_type`, the mark stages, and
   `split_for_violations` all iterate hash-ordered `Vec`-sets whose
   layout shifts as ess/css ids change (the issue 009/021 iteration-
   order family). Stage 4's pre-B4 shape — per-confluence mark
   seeding that leaked marks across iterations, fixed in the survey
   (032 B4/P3) — was one concrete instance; the fysphun trace shows
   the same disease at loop granularity: passes 22..27 REMOVE
   contours a previous pass added (ess drops from 850 to 589), then
   re-add different ones.

3. **`split_for_violations` splits blind.** Stage 5 splits on every
   violation imprecision each pass with `SPLIT_DYNAMIC`, whether or
   not the same violation was already split for in a previous pass
   and survived. When a violation is NOT resolvable by contour
   refinement (fysphun's residue is numeric-coercion-shaped, see
   §Starvation), stage 5 manufactures contours forever.

## Additional finding: the `||` short-circuit starves `reanalyze`

`while (extend_analysis() || if1->callback->reanalyze(...))` only
consults the frontend's reanalysis callback when the splitter
reports no work. On a diverging input the splitter ALWAYS reports
work, so the pyc numeric-coercion annotator — which existed
precisely to resolve fysphun's class of violations — never ran.
With the stall guard forcing the splitter to rest, the annotator
resolves **all 99 residual violations** and fysphun's FA converges
(0.94s total, was 300s+ timeout). Any future scheduling change
should preserve this fairness property or make it explicit
(e.g. run `reanalyze` between passes rather than only at splitter
exhaustion).

## Mitigation in place (not the fix)

`FA::stall_limit` (`IFA_STALL_LIMIT`, default 8): if the violation
count fails to improve on its best for that many consecutive
passes, `extend_analysis` sets `pass_limit_hit` and stops
splitting. Zero-violation passes (pure precision splitting) never
advance the counter, so converging programs are unaffected. This
guarantees wall-clock termination and, via the starvation fix-by-
accident above, actually *converges* some previously-diverging
inputs. It does NOT make splitting deterministic or idempotent.

## What it unblocks

- The remaining diverging inputs (kmeanspp, pylife residues) —
  largely resolved by 035; the live payoff now is pygasus-scale
  performance (S2).
- Trustworthy FA telemetry (pass counts become stable metrics under
  the determinism gate, D7 / [035](035-nondeterministic-codegen-clone-order.md)).
- Removes the risk that stall_limit=8 clips a legitimately slow
  but converging program (none known today; the guard only fires
  on nonzero-violation plateaus).

## Completed stages

Three pieces of prerequisite work landed before S5 was written.
They're summarized here because they directly motivate S5's shape;
full detail (code sites, exact findings, log excerpts) is archived
as D2 / D6 / D7 in
[closed/033-ledger-design-detail.md](closed/033-ledger-design-detail.md).

- **Ledger record-only + `dup_splits` observability** (landed
  2026-07-10). Added a persistent `(fun, stage, position,
  partition)`-keyed ledger that *records* split decisions and
  counts re-derivation hits without changing behavior. Confirmed
  the key shape against the real fysphun divergence (`dup_splits >
  0` exactly on the plateau) — but also found that **converging
  recursive/dispatch-heavy programs (expr_evaluator, richards) hit
  the ledger too**, up to 3 dups/pass, all byte-identical
  re-derived decisions. That finding is why S5's milestones are
  gated on "suites unchanged," not "zero ledger hits on converging
  inputs" — enforcement (M2/M3) is expected to route those repeats
  to their recorded product, not just diverging-input ones.
- **Non-refinable violation cutoff for `split_for_violations`**
  (landed 2026-07-10). Implemented as designed, all suites green —
  but instrumentation showed stage 5 is barely reached at all:
  stages 1-4 report work on almost every pass, so stage 5 (and this
  cutoff) is currently near-dead code. This starvation dynamic — the
  *same* disease as the `reanalyze` starvation above, but internal
  to `extend_analysis` — is exactly what S5's M2 (stop
  short-circuiting across stages) is designed to fix; once stages
  1-4 stop re-reporting work every pass, stage 5 gets consulted
  regularly and this cutoff becomes load-bearing.
- **Ordering / determinism audit** (landed 2026-07-09/10). Read
  every `collect_*`/`split_*` function feeding `extend_analysis`;
  found and fixed three real nondeterminism sources (missing
  `qsort_by_id` in two collectors, and in `fa->ess` itself). This is
  the prerequisite that makes repeated runs byte-identical, which is
  what lets S5's milestones use exact pass-count fixtures as an
  acceptance gate instead of fuzzy wall-clock comparisons.

## `issue033-stage-c` branch

A branch (`issue033-stage-c`, tip `2d514f58`) implements the
original "stage C" design — cross-pass ledger routing for
`split_entry_set`'s greedy groups, keyed on a full per-position/
per-ret `group_signature` (not just one position) with wildcard
rejection and display-feasibility checks (full spec archived as D4
in [closed/033-ledger-design-detail.md](closed/033-ledger-design-detail.md)).
It is **sound and fully green** (both suites 177/0 under the
determinism gate, ifa-test green, corpus member set unchanged) and
**parked as unnecessary** for today's corpus: with 035's determinism
fixes, the trio converges without it, and the branch's
earlier-observed pylife improvement to 52 violations turned out to
be an artifact of a since-fixed bogus-merge bug, not a real win over
main.

Status relative to main: the branch last synced main at `8abc6aba`
(which already included the 035 determinism fixes); main has since
advanced 8 commits (issue 037's matcher fix, this plan's own
updates) that the branch does not have. **Do not merge as-is.** Per
M3 below, the branch's routing was built against today's interleaved
first-stage-wins extend loop; merging it before M2 lands would
reintroduce the exact builtins_batch wildcard-key hazard class M3 is
designed to close structurally. Action: leave the branch parked;
rebase it onto main after M2 lands, as the concrete starting point
for M3's revival.

## Shedskin comparison (2026-07-11, from ../shedskin/shedskin/infer.py)

Shedskin analyzes pygasus successfully with the same underlying
theory (Agesen CPA for function polymorphism + Plevyak IFA for
data polymorphism). This section records the architectural
comparison in detail, the measured anatomy of pygasus's remaining
cost in pyc, sketches of adversarial test cases that isolate each
cost class, and suggested investigation directions. All numbers
are reproducible: the harness determinism gate guarantees
byte-identical recompiles, so per-pass timings and counts are
stable metrics.

### S1. The two loop architectures

pyc/ifa (analyze_to_convergence):

```
do {
  flow to fixpoint over the FULL program and FULL contour universe
  extend_analysis():          // from the converged-but-provisional state
    collect confluences over ALL ess/css        (per pass, from scratch)
    run split stages 1..5, STOP at first stage that makes progress
    clear_results()                              (throw flow state away)
} while (splitter made progress || reanalyze());
```

shedskin (iterative_dataflow_analysis):

```
while True:
  propagate():                // worklist over the constraint graph
    cpa() per NEW callsite/arg-type combination:
      - if |funcs| x |arg product| > cpa_limit: DON'T BIND, mark limited
      - if this round already admitted 5 new funcs / 1 new alloc: DON'T
        BIND yet (incremental admission)
      - else create the (func, dcpa, cpa) template ONCE (memoized)
  split = ifa()               // data-polymorphism splits, ONCE, from
                              // the round's converged state
  if no split:
      if work was deferred (incremental / cpa_limited): widen and continue
      else: DONE
  apply splits to alloc_info; RESET the graph; RESEED from alloc_info
```

The load-bearing differences:

| dimension | pyc/ifa | shedskin |
|---|---|---|
| when splits decided | every pass, from provisional state | once per round, from converged state |
| what persists across rounds | `av->cs_map` only (S3) | `alloc_info`: every allocation-site→contour assignment |
| specialization creation | edges/ES re-derived per pass | `(func,dcpa,cpa)` templates memoized forever |
| work admission | whole program every pass | ≤5 new funcs, ≤1 new alloc per round |
| explosion valve | pass_limit=100, stall guard=8 | CPA_LIMIT=10 (lazy doubling), MAXITERS=30, maxhits=3 |
| deferral | `incomplete_call` (types not yet arrived) | same PLUS product-cap and admission-cap deferral |

Splitting from converged state is what makes shedskin's decisions
naturally idempotent — the exact property this issue's ledger
(D1-D9) tries to retrofit onto per-pass splitting. Persistence
(`alloc_info` ≈ the ledger ≈ the stage-C branch) is the other
half: shedskin can afford to RESET the whole graph every round because
the decisions survive.

### S2. Anatomy of pygasus's remaining 200s (post-037, `a7c192f2`)

19 passes, terminating with a complete 788-violation diagnosis.
Cost breakdown from the `-v` PASS lines:

- **Pass 1: 32s, ~95% match.** The hot sends are 3-candidate
  `_exec` METHOD calls (multi-candidate — the 037 single-candidate
  collapse deliberately skips them), 4 args each with unions like
  {int64-const, int64, str, None, float64}. Each send re-matches
  on every argument-type growth event (the MatchCache keys on
  exact per-position ATypes and misses throughout convergence);
  ~120K leaf evaluations for the hottest single send in 40s.
- **Passes 2-5: ~4.5s each,** mixed flow/match; contour universe
  grows to ~3750 ess / 4200 css.
- **Passes 6-11: ~18.5s each, 76-81% extend** — the plateau. Six
  passes re-run confluence collection + the split machinery over
  3800 ess x edges to make ~10 ess of progress per pass while
  violations sit at ~1700-1725. This is the S1 "per-pass amnesia"
  cost at scale, minus the nondeterministic wandering (035 fixed
  that): the splitter re-derives and re-decides largely the SAME
  decisions each pass.
- **Passes 12-19:** the reanalyze/coercion annotator engages,
  violations fall 1725 → 788, extend spikes alternate with cheap
  passes.

So: one-third match (attackable by 037-style exactness), two-thirds
extend-plateau (attackable only by the shedskin round structure /
ledger persistence).

### S3. Adversarial test-case sketches

Each isolates one cost class. They belong in a `benchmarks/fa/` or
`tests/perf/` set gated on wall-clock budgets (the determinism gate
makes their pass counts exact regression metrics, not just
timings).

**(a) arity x union-width match product — single candidate**
(the 037 class; **LANDED** as `tests/high_arity_single_candidate.py`):

Pre-037, a 13-argument single-candidate call with `{const, int64}` unions at
each position cost 2^13 leaf evaluations per `pattern_match` invocation.
Fixed by dispatch-equivalence collapsing in `find_best_matches` (issue 037):
the single viable class per position is identified in O(N_cs) and the subtree
is entered once; `set_filters` expands the representative back to the full
class. See `tests/high_arity_single_candidate.py` for the regression test.

**(b) multi-candidate product** (pygasus's pass-1 shape; open):

```python
class A:
  def go(self, x, y, z, w): return x
class B:
  def go(self, x, y, z, w): return y
class C:
  def go(self, x, y, z, w): return z
os = [A(), B(), C()]
for o in os:
  o.go(1, 2.5, "s", None)
  o.go(2, 3.5, "t", None)     # more constants -> wider unions
```

3 candidates x 4 positions x {const,base,...} unions. Scale the
argument count and the union widths; the match cost should be
~product today and ~sum after multi-candidate collapsing (S4-C).

**(c) extend plateau / contour-universe grind** (the dominant
pygasus cost; open):

```python
# K wrapper funs x M call contexts -> ess ~ K*M; plus an
# unresolvable numeric residue to keep the splitter engaged.
def w0(x): return x
def w1(x): return w0(x)
# ... w2..wK generated, chained ...
acc = 0
for v in [1, 2.5]:
  acc = acc + v          # persistent BOXING violation (fysphun-shaped)
# fan-in: call wK(int), wK(float), wK(str) from M distinct funs
```

Generate with a script at K=200, M=20 (≈4000 ess). Metric:
extend-seconds/pass and passes-to-termination. Today each plateau
pass costs O(ess x edges) collection + split machinery; the
target architectures (S4-A/B) should make plateau passes either
disappear (splits persist) or collapse to near-zero cost (nothing
changed => nothing recollected).

**(d) genuine CPA explosion** (needs the deferral valve, S4-D —
exactness cannot help because the return type really depends on
every argument):

```python
def pair(a, b): return (a, b)
def quad(a, b, c, d): return (pair(a, b), pair(c, d))
# leaves drawn from {int, float, str}: the tuple type lattice is
# the full 3^N product; every combination is a distinct, USED type.
v = quad(1, 1.0, "s", 1)
w = quad("s", 1, 1.0, "s")
# ... enough call sites to visit a large fraction of the product
```

pyc must either enumerate (exploding contours/types) or defer and
merge (losing precision to tuple-of-union). shedskin answers with
CPA_LIMIT + lazy doubling; the test locks whatever policy pyc
adopts, including the violation/diagnostic quality when capped.

**(e) data-polymorphism split churn** (ledger/idempotence at CS
granularity — D5 territory):

```python
def fill(l, v): l.append(v)
def drain(l): return l[0]
pools = [[] for _ in range(50)]
for i, p in enumerate(pools):
  fill(p, i)          # int pools
qools = [[] for _ in range(50)]
for q in qools:
  fill(q, 1.5)        # float pools, SAME fill/append contours
print(drain(pools[0]) + 1, drain(qools[0]) + 1.5)
```

The shared `fill`/`append`/element-avar contours merge int|float
until split_css separates the list creation sites — re-derived
every pass today. Metric: how many passes re-make the same CS
split (observable via `dup_splits` once D5 records CS keys).

**(f) nested-function display interaction** (the stage-C display
lesson; guards ledger-routing correctness for closures):

```python
def outer(v):
  def mid(w):
    def inner(x): return x + w + v
    return inner
  return mid
fs = [outer(1)(2), outer(1.5)(2.5)]
print(fs[0](3), fs[1](3.5))
```

Closure contours at depth 2 with polymorphic captured vars: any
persistent-decision mechanism must key display chains (group_
display_ok exists on the stage-C branch) or corrupt make_AVar's
display walks.

### S4. Investigation directions

Ordered by expected payoff for pygasus-scale inputs; A and B are
alternatives at different ambition levels, C/D are independent.

**A. Dirty-marked extend (cheap, incremental; attacks the plateau
cost without architecture change).** Today every extend pass
re-collects confluences by walking ALL of fa->ess x args x
backward (`collect_type_confluences` etc.). But between plateau
passes almost nothing changed. Investigate: mark AVars whose
`out` changed since the last extend (a bit + a list, set in
update_in — the [upd] instrumentation from the 035 hunt shows
exactly where); re-collect only confluences reachable from dirty
AVars; stages with an empty dirty set are free. First step is
measurement: add per-stage timers inside extend_analysis (the
sidecar FAPassEvent machinery from issue 003 already has the
shape) and confirm the plateau is collection-dominated vs
split-machinery-dominated. Risk: low — collection is
read-only; a wrong dirty set shows up as a missed confluence,
which the determinism gate + fa-converge fixtures would catch as
a changed pass count.

**B. Split-from-converged-state + persistence (the shedskin
shape; the real fix, supersedes the per-pass ledger).** Restructure
the outer loop so contour DECISIONS are made only from converged
state and survive graph resets:

```
round:
  flow to fixpoint (as today)
  decide ALL splits for this round from the converged state
    (not first-stage-progress-wins: batch stages 1..5)
  record decisions in the ledger (fun/pos/partition/sig — the
    stage-C keys, plus CS keys per D5)
  clear flow state; REBUILD contours from the ledger; next round
```

Two sub-investigations before committing: (1) how much of the
plateau exists only because extend stops at the first stage with
progress — count stage-progress patterns across the pygasus trace
(if pass k splits stage 1 and pass k+1 splits stage 3 on the same
state, batching halves the passes); (2) whether the stage-C
branch's route-to-product mechanism becomes fully sound when
routing only happens from converged state — the builtins_batch
wildcard hazard (empty intersected types) cannot occur at
convergence for live code, which would let the branch's ledger
serve as the reseeding store directly. The branch (`2d514f58`)
is green and parked; this is its revival path.

**C. Multi-candidate match collapsing (attacks pygasus pass 1,
~32s -> ~seconds).** Extend the 037 equivalence classes to
multi-candidate sends. Soundness analysis already done (037
addendum): key = per-candidate (accept, exact, this) bit-vectors
+ `cs->sym->type`, PLUS `cs->sym` itself when the candidates'
dispatch types differ at the position (subsumes_arg compares
`cs->sym` against isa sets; identical dispatch types make its
verdict CS-independent; coercion/promotion/verify work on
`cs->sym->type` which the key preserves). Validate against sketch
(b) and the full battery; watch `formal_dispatch_types` mutation
across leaves (coercion state evolves per PMatch during a single
pattern_match — collapsing must not change the SEQUENCE of
distinct concrete ->type values a candidate sees).

**D. CPA_LIMIT-style deferral valve (safety net for sketch (d);
small).** In pattern_match, before find_best_matches: compute
`candidates x prod(per-position class counts)`; if above a limit,
return 0 through the `incomplete_call` path (the send re-fires
when types change). Escalate at quiescence: when extend_analysis
finds no work AND capped sends exist, double the FA-level limit
and re-enqueue them (a Vec<AVar*> of capped sends on FA).
Interactions to check: the stall guard (a capped send's violations
shouldn't count as stall), reanalyze ordering, and diagnostics
(capped-at-exit sends need a clear violation message, not silence
— shedskin's maxhits=3 give-up is its weakest part, don't copy
that silently).

**E. Subset-aware MatchCache (only if C is insufficient).** The
cache misses all through convergence because it keys exact
ATypes. Monotonicity suggests caching the per-position
viable-class PARTITION (from 037) rather than the match result:
a new AType whose CSs all fall into existing classes reuses the
cached outcome with the new CSs appended to their classes'
filters. Needs a careful invalidation story for is_exact_match
positions; measure C first — collapsing may make re-matching so
cheap the cache stops mattering.

**F. Incremental admission (long-term, architectural).** Shedskin
grows the analyzed world by ≤5 functions per round, so early
specialization mistakes are cheap. pyc's equivalent would be
SCC-ordered bottom-up analysis with per-fun contour budgets —
a research-sized change; only worth revisiting if A-D leave a
gap on real inputs.

Verdict: shedskin's solution is applicable. (a)-already-landed:
exactness collapsing + viability pruning (037) removed the match
product for single-candidate sends. The recommended sequence is
A (measure + dirty-mark) -> C (multi-candidate collapse) -> B
(converged-state splitting with the ledger/stage-C as the
persistence store) -> D (deferral valve), with S3's sketches
landed as regression benchmarks alongside each.

## S5. Merge plan: adopting shedskin's round structure into ifa

This is the actionable plan distilled from S1-S4. It supersedes
the D10 land order for the remaining ledger stages (the D-design's
mechanics — keys, display feasibility, bare products — carry over
unchanged; what changes is WHEN decisions are made and what
persists). Each milestone lands independently with the full gate:
pyc C + LLVM suites under the harness determinism check, ifa-test
all phases, corpus member-set comparison (sets, not counts —
issue pyc/028's lesson), and the S3 benchmarks' pass counts.

The core insight being adopted, stated once (see the
provisional-vs-converged discussion): both systems split at flow
fixpoints, but shedskin (i) computes its COMPLETE split plan
against an immutable snapshot before mutating anything, (ii)
persists every decision outside the graph (`alloc_info`), and
(iii) can therefore reset and reseed instead of incrementally
mutating a live contour graph whose survival semantics are
accidental. pyc today interleaves deciding with mutating
(first-stage-wins, mid-stage ES creation), persists nothing by
design (`av->cs_map` survives by exception), and relies on
`Fun::ess` object survival plus deterministic re-derivation
(post-035) for its de-facto idempotence.

### M0. Measurement + benchmark harness (prereq, no behavior change)

- Per-stage wall-clock timers inside `extend_analysis`, reported
  in the `-v` PASS line and the issue-003 FAPassEvent sidecar
  (the sidecar already carries per-stage before/after counts; add
  micros). Distinguish: confluence COLLECTION vs split MACHINERY
  vs pending-map bookkeeping.
- Stage-progress histogram: for each pass, which stage made
  progress. Quantifies how much of the plateau is
  first-stage-wins truncation (predicts M2's win).
- Dirty-AVar counter: how many AVars' `out` changed since the
  previous extend (predicts M4's win).
- Land the S3 sketches under `tests/perf/` (generated where
  needed, sketch (c) with K=200/M=20), asserting PASS COUNTS
  (exact under determinism) with generous wall-clock ceilings.
- Acceptance: all suites unchanged; pygasus/sketch numbers
  recorded here as the baseline table.

**Status: partially landed 2026-07-11.** Per-stage wall-clock
timers (`FA::stage_time[7]`, lapped at each stage boundary in
`extend_analysis` regardless of whether that stage found work) and
the stage-progress histogram (`FA::stage_progress_count[7]`,
incremented alongside `record_fa_event`) are in, printed as a
per-stage breakdown under `-v` at convergence. Not yet done: the
dirty-AVar counter and the S3 `tests/perf/` sketches (needs sketch
generation + a wall-clock-ceiling test convention that doesn't
exist yet). Verified against all three suites (pyc C 178/0, pyc
LLVM 178/0, ifa-test all 16 phases including fa-converge) —
unchanged, confirming the instrumentation is behavior-preserving.

**Finding: `mark_type` (`split_ess_for_mark_type`, stage 2), not
the winning stage, dominates extend cost on all three trio
examples** — 70-89% of extend time, despite reporting progress on
only 3-4 of the 10-21 passes (`type_confluence` wins more passes
but costs far less per pass):

| example | passes | final ess/css/viol | mark_type share of extend | mark_type progress-passes |
|---|---|---|---|---|
| fysphun | 18 | 218/556/0 | 70% (0.138s) | 3 |
| kmeanspp | 21 | 333/706/6 | 80% (0.137s) | 4 |
| pylife | 13 | 301/829/60 | 84% (0.070s) | 4 |

These pass/ess/violation counts match the RESOLUTION UPDATE
baseline at the top of this doc exactly (fysphun 18/0, kmeanspp
21/6, pylife 13/60), confirming no behavior change. The new
finding sharpens M2/M4's target: `type_confluence` "wins"
(short-circuits) most passes, but `mark_type`'s collection
(`collect_cs_marked_confluences` + the joint mark-seeding loop,
fa.cc stage 4) is the expensive step that keeps running underneath
it every pass regardless of who wins — batching (M2) won't shrink
this cost by itself since mark_type still has to run; dirty-marking
(M4) is what actually targets it, and should be prioritized to
cover stage 4's collectors first if M4 is split into sub-steps.

### M1. Multi-candidate match collapsing (S4-C; independent track)

Attacks pygasus pass 1 (~32s). Mechanics and soundness key as
specified in S4-C / the 037 addendum. Order-independent of
M2-M5; do it first because it is contained (pattern.cc only) and
its test (sketch (b)) is the simplest.
- Acceptance: sketch (b) match cost/timing goes from ~product to
  ~sum; pygasus pass-1 wall time drops materially; full suites +
  037's regression test unchanged.

**Status: LANDED 2026-07-11.** `Matcher::find_best_matches`
(`ifa/if1/pattern.cc`) generalizes the nmat==1 dispatch-equivalence
collapsing to `nmat > 1` live non-generic candidates (capped at 64,
past which it falls back to full enumeration — no realistic call
site approaches that arity of candidates): a CS is grouped with
others at a position by the joint per-candidate accept/exact/this
vote bitmask, `cs->sym->type`, and (only when the live candidates'
formal dispatch types differ at that position — the case
`subsumes_arg` reads the raw symbol for) `cs->sym` itself. Globally
non-viable CSs (no live candidate accepts them) are pruned rather
than enumerated, generalizing the 037 addendum's single-candidate
viability pruning. Any candidate with a pattern/varargs/missing
formal at the position, or any generic candidate, bails that
position out to full enumeration — same conservatism as the
existing single-candidate guard.

- **pygasus pass 1: 11.1s (was ~32s)**, match share of the pass
  down from ~95% to 90%; final diagnosis unchanged (788 violations,
  exact match against the previously-recorded baseline) — confirms
  the collapsing changes cost, not outcome. The remaining pass-1
  cost and the rest of the run is the extend-phase plateau (M2-M4
  territory, not this milestone's target).
- Landed `tests/multi_candidate_dispatch.py` (3 receiver types, one
  shared 4-arg signature, each position a widening constant/type
  union) as the sketch-(b) regression, alongside 037's
  `tests/high_arity_single_candidate.py`. Verified byte-for-byte
  against a `pyc`-compiled-and-run binary AND against CPython
  (`python3 tests/multi_candidate_dispatch.py`), not just the
  compile-succeeds check — sketch (b) as originally written in S3
  triggered an unrelated pre-existing runtime crash (a
  "matching function not found" abort from mixing int/float/str
  return types through one `print()` call; reproduced identically
  with this change reverted, so it predates M1 and is out of scope
  here — the same assert class tracked in
  [030-polymorphic-dispatch-fat-pointers.md](030-polymorphic-dispatch-fat-pointers.md))
  — the landed test avoids that by keeping return types homogeneous
  per receiver while still exercising heterogeneous argument
  unions.
- Verified: pyc C 179/0 (178 + the new test), pyc LLVM 179/0,
  ifa-test all 16 phases (including `patterns`/`dispatch`, which
  exercise the matcher directly) unchanged; shedskin corpus sweep
  member set identical (23 compiled, same names) to the pre-change
  baseline; `bh` still fails on its known pre-existing issue
  ([pyc issues/028](../../issues/028-raise-exception-regression-qualified-dispatch.md)),
  not a new failure.

### M2. Batch-stage extend (immutable-snapshot property, step 1)

Remove first-stage-wins: run stages 1..5 every extend, each over
the SAME snapshot's collected confluences. Two sub-steps:

- M2a: keep today's interleaved mutation within a stage but stop
  short-circuiting across stages (`analyze_again |=` instead of
  `if (!analyze_again)`). Cheap; changes pass counts (fixture
  goldens re-blessed knowingly, fa-converge sidecar diffs
  reviewed stage by stage). Risk: stage 2/4 mark machinery ran
  historically only when stage 1/3 found nothing — verify marks
  are cleared per stage (they are — `clear_marks(acc)`) and that
  double-splitting the same ES within one pass short-circuits
  (it does — `es->split`).
- M2b: decide-then-apply within each stage: collection loops
  produce a decision LIST (av, group partition, target) computed
  against the unmutated state; application runs afterward. This
  removes the last intra-pass order dependence (the D7/009/021
  family becomes structurally impossible rather than
  qsort-suppressed). This is the natural refactor point to route
  both the bare-parking path and the ledger through one
  "apply_split(decision)" function, whose decision record is the
  already-landed `SplitDecision` struct (fun, stage, position,
  canonical partition AType; full field/hashing spec archived as D1
  in [closed/033-ledger-design-detail.md](closed/033-ledger-design-detail.md)).
- Acceptance: fa-converge sidecar fixtures re-blessed and stable
  under two consecutive runs (byte-identical); stage-progress
  histogram (M0) shows first-stage-wins truncation gone; full
  suites unchanged; no new `pass_limit_hit`/stall-guard firings.

### M3. Ledger persistence from converged snapshots (stage-C
revival; the alloc_info analog)

Rebase and merge branch `issue033-stage-c` (green at `2d514f58`;
status/staleness detail in
[its section above](#issue033-stage-c-branch)) on top of M2.
Semantics change vs the parked branch: keys are recorded and
routed ONLY from post-M2 snapshot decisions. The builtins_batch
hazard class (wildcard/partial-type signatures colliding across
passes at different convergence stages) is structurally absent:
a converged snapshot of live code has no empty intersected types
at covered positions, and M2b means every key in one pass
derives from one consistent state. Keep the branch's
group_signature exactness (filter-intersected types, wildcard
rejection) as a defensive invariant — it should never fire; add
an assert-under-fixture that it doesn't.

Key shapes to carry over unchanged from the branch/original design
(full spec archived as D1/D4/D5 in
[closed/033-ledger-design-detail.md](closed/033-ledger-design-detail.md)):
ES/group keys are `(fun, stage, position, canonical partition
AType)`, with groups additionally keyed by the FULL per-position/
per-ret `group_signature` (not one position) plus lexical-display
feasibility (`group_display_ok`) — a single-position key merged
distinct groups and mistyped int results as float (the
builtins_batch bug this section's hazard-avoidance is about). CS
keys are advisory: a hash of the sorted def-Var id signature of
`compatible_set` (setters aren't stable across `clear_results`, so
can't be keyed on directly).
- Extend the ledger to CS splits (the CS key shape above) so data
  polymorphism decisions persist too — this is the direct
  `alloc_info` equivalent and what sketch (e) locks.
- Acceptance: sketch (c) and pygasus plateau passes show
  route-hits replacing re-splits (`dup_splits` becomes a
  productivity metric: routed-per-pass); ess growth monotone
  across passes on the trio + pygasus; outcomes unchanged
  (violation sets identical).

### M4. Dirty-marked collection (S4-A)

With M3, most plateau passes re-derive types into an unchanged
landscape. Collection is now the residual cost: mark AVars whose
`out` changed since the last extend (set in `update_in` /
`flow_var_type_permit`'s propagate tail); collectors walk the
dirty set's neighborhoods instead of all of `fa->ess x args x
backward`. An extend over an empty dirty set is free — making
"nothing changed" passes O(1) and turning the outer loop's
convergence check cheap enough to run stages more often.
- Risk: a missed dirty path = a missed confluence = wrong
  convergence. Mitigation: fixture mode that runs BOTH collectors
  and asserts equality (the determinism gate makes the comparison
  exact); soak on the full corpus before removing the check.
- Acceptance: dirty-set/full-collector equality fixture passes on
  the full corpus; plateau-pass extend-seconds (M0 timers) drop
  toward zero on sketch (c) and pygasus; suites + all S3 sketch
  pass counts unchanged.

### M5. Reset-and-reseed rounds (the full shedskin shape; only if
M2-M4 leave a gap)

With decisions fully persistent (M3) and collection incremental
(M4), evaluate whether the remaining per-round cost justifies the
final step: rebuild the contour universe from the ledger at round
start (ESs/CSs constructed from keys; displays from the recorded
feasibility data) and make `clear_results` a true graph reset.
This buys shedskin's memory behavior and removes `Fun::ess`
survival as load-bearing state, at the cost of a reseeding pass.
Decide on M0-metrics evidence, not aesthetics — if pygasus-class
inputs converge in seconds after M4, stop there.

### M6. Deferral valve (S4-D; safety net, independent)

The CPA_LIMIT analog for sketch (d)'s genuine product explosions,
through the existing `incomplete_call` deferral path, with
escalation at extend-quiescence and explicit diagnostics for
capped-at-exit sends. Land last: after M1 the corpus doesn't need
it, and its stall-guard interaction is simpler to reason about
once M2-M3 have made pass semantics boring.
- Acceptance: sketch (d) compiles (capped, not hung) with an
  explicit capped-send diagnostic, never silent; stall guard does
  not fire on a capped send's residual violations; suites
  unchanged.

### Explicitly NOT adopted from shedskin

- **Incremental function/allocation admission** (S4-F): the
  payoff overlaps M3/M4's, and it changes user-visible analysis
  order; revisit only if a real input defeats M2-M5.
- **maxhits=3 silent give-up**: pyc keeps hard diagnostics
  (`pass_limit_hit`, the stall guard, violation reporting). Any
  budget that fires must leave a violation trail, never silence.
- **Graph-reset-first** (reset before persistence exists):
  ordering matters — persistence (M3) must land before any reset
  semantics (M5), or we rebuild the 033 divergence with extra
  steps.


---

## Archived design detail

The original stage-by-stage per-pass ledger design (background
mechanics, the `SplitDecision` struct, stage A-F specs, the
termination argument, and a change-inventory/sizing table) has been
moved to
[closed/033-ledger-design-detail.md](closed/033-ledger-design-detail.md).
It predates the shedskin comparison above and is superseded as the
land order by S5, but its key shapes and empirically-established
corrections (particularly D1 and D4) remain the implementation
reference for S5's M2/M3.
