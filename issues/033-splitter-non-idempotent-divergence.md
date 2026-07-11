# Issue 033: Splitting loop has no fixed point on some inputs (non-idempotent, order-dependent split decisions)

**Status:** open, fix in progress. Divergence is *mitigated* (stall
guard, commit `21dbdad4` on the pyc side of the tree) so affected
programs terminate quickly instead of timing out; the root cause —
split decisions that are not idempotent across passes — is
untouched. Of the staged fix below: D7 (ordering audit) landed
2026-07-09; **stage A (record-only ledger + `dup_splits`
observability) landed 2026-07-10** — see D2 for findings, including
one revision to the D11-A acceptance expectations. Next per land
order: D6, then B, C, E. **D6 also landed 2026-07-10** — a no-op on
today's corpus (see D6 for why its premise no longer holds
post-mitigation); the live driver is the stage-0/1 group path.

**RESOLUTION UPDATE (2026-07-10, after the
[035](035-nondeterministic-codegen-clone-order.md) determinism
fixes): the core divergence symptom is GONE on main WITHOUT stage
C.** All three diverging examples now reach genuine fixed points
with the stall guard never firing — fysphun 18 passes/0 violations,
kmeanspp 21/6, pylife 13/60 — i.e. much of the "non-idempotence"
was heap-layout nondeterminism re-rolling split decisions between
passes (the issue-009/021 family, living in the container library
and SSU liveness rather than the splitter itself). The splitter's
per-pass amnesia (D0) is still real, but with deterministic
re-derivation it re-makes the SAME decisions, which is enough for a
fixed point on the known inputs.

**Stage C status (branch `issue033-stage-c`): sound and fully
green as of `2d514f58`, parked as unnecessary.** The builtins_batch
miscompile was root-caused with the deterministic logs: the ledger
key hashed RAW argument types while the grouping predicate
(`edge_type_compatible_with_edge`) compares per-edge
FILTER-INTERSECTED types, and the predicate treats an EMPTY
intersected type as compatible-with-anything — a wildcard that a
snapshot key cannot represent. Three `__str__` call sites' groups
(passes 0/1/2) matched one key and were funneled into one product
(ES 170), which pass 3 then had to split apart (do=2/3) without
being able to record the re-splits (key occupied); the trajectory
shift merged the int/float iterator contours downstream
(`__list_iter__::__next__` ret became float64, both `sum` clones
returned float64, print formatted the int total's bits). Fix on
the branch: `group_signature` now mirrors the predicate exactly
(filter-intersected types) and returns "no identity" for groups
containing a wildcard, which are neither routed nor recorded that
pass. With the fix the branch is fully green (both suites 177/0
under the determinism gate, ifa-test green, corpus member set
unchanged) — and behaviorally almost identical to main: exactly ONE
route fires in builtins_batch, and the trio traces match main
pass-for-pass (the branch's earlier pylife "improvement" to 52
violations was an artifact of the bogus merges). Parked: correct
but currently valueless; pick it up only if a genuinely diverging
input reappears.
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
(crash unmasked by the mitigation).

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

## Proposed real fix

Make split decisions persistent and keyed, so re-running the
splitter is idempotent:

- Give each split a canonical key — e.g. (fun, dispatch position,
  type partition) for ES splits, (creation site, field, partition)
  for CS splits — interned like `cannonical_atypes`.
- Keep a cross-pass `Map<key, generation>`; a key that was already
  split for is never split again unless the partition it produced
  was itself refined (generation bump).
- `split_for_violations` additionally records which violations it
  split for; a violation that survives its own split verbatim is
  marked non-refinable and excluded from stage 5 thereafter.
- Iterate confluences in a canonical order (sorted by stable ids,
  the `compar_*` idiom used elsewhere) so the surviving decisions
  are order-independent.

With idempotent splits the contour set grows monotonically, the
outer loop is a genuine fixed-point iteration, and both
`IFA_PASS_LIMIT` and the stall guard become true safety nets
instead of load-bearing.

## Verification plan

- fysphun/kmeanspp/pylife under `-v`: pass count bounded, ess/css
  monotone nondecreasing, no oscillation, outcome unchanged or
  better vs the stall-guard baseline.
- Full suites both backends + ifa-test, plus the fa-converge
  fixtures (issue 003 sidecar) which lock per-pass stage events.
- Re-run the issue 009/021 flake checks: with canonical ordering,
  repeated runs must produce identical pass counts and violation
  sets.

## What it unblocks

- The remaining diverging inputs (kmeanspp, pylife residues).
- Trustworthy FA telemetry (pass counts become stable metrics).
- Removes the risk that stall_limit=8 clips a legitimately slow
  but converging program (none known today; the guard only fires
  on nonzero-violation plateaus).

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
(the 037 class; now handled; keep as regression):

```python
def f(a,b,c,d,e,g,h,i,j,k,l,m,n): return a
x = f(1,2,3,4,5,6,7,8,9,10,11,12,13)      # all {const,int64} pairs
y = f(1.0,2,3,4,5,6,7,8,9,10,11,12,13)    # perturb one position
```

Pre-037 this is 2^13+ leaf matches per re-match. Variant for the
viability pruning: make a few positions carry a type the callee
rejects part-time (e.g. thread a `str` through one argument on a
cold path) so every position has an accepted AND a rejected class
mid-convergence — pre-pruning that re-explodes to 2^13 REJECTED
combinations even with collapsing.

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

### M1. Multi-candidate match collapsing (S4-C; independent track)

Attacks pygasus pass 1 (~32s). Mechanics and soundness key as
specified in S4-C / the 037 addendum. Order-independent of
M2-M5; do it first because it is contained (pattern.cc only) and
its test (sketch (b)) is the simplest.

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
  "apply_split(decision)" function.

### M3. Ledger persistence from converged snapshots (stage-C
revival; the alloc_info analog)

Merge branch `issue033-stage-c` (green at `2d514f58`) on top of
M2. Semantics change vs the parked branch: keys are recorded and
routed ONLY from post-M2 snapshot decisions. The builtins_batch
hazard class (wildcard/partial-type signatures colliding across
passes at different convergence stages) is structurally absent:
a converged snapshot of live code has no empty intersected types
at covered positions, and M2b means every key in one pass
derives from one consistent state. Keep the branch's
group_signature exactness (filter-intersected types, wildcard
rejection) as a defensive invariant — it should never fire; add
an assert-under-fixture that it doesn't.
- Extend the ledger to CS splits (D5's advisory design) so data
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

# Detailed implementation design (the split-decision ledger)

Everything below is written to be implementable without prior
familiarity with the splitter. File/line references are as of
commit `21dbdad4`.

## D0. Background: how splitting actually works today

Read this first; the design hangs off these five mechanisms.

1. **Contour storage.** EntrySets live in the per-function list
   `Fun::ess` and are NEVER deleted. The global `fa->ess` is
   rebuilt every pass by `collect_results()` (fa.cc:3064) from
   `fa->entry_set_done` — it lists only the ESs *reached* this
   pass. That is why `fa->ess.n` can DROP between passes while
   total allocated contours only grow: an oscillation is the flow
   re-derivation reaching a different subset of `Fun::ess` each
   pass.

2. **Edge → ES routing.** When an `AEdge` needs a target,
   `make_entry_set` (fa.cc:1052) tries, in order:
   `check_split(e, ...)` (fa.cc:1031 — follows
   `e->from->split->out_edge_map` when the caller ES was itself
   split this pass), then `find_best_entry_sets` (scores every
   existing ES of the callee via `entry_set_compatibility`, which
   consults `EntrySet::filters` through `check_edge`), then
   creates a fresh ES.

3. **Split path A — filtered (already half-keyed).**
   `split_edges` (fa.cc:3546, the `SPLIT_DYNAMIC` route) builds a
   `Map<MPosition*, AType*> filters` per receiver CreationSet and
   calls `find_or_make_filtered_entry_set` (fa.cc:3536), which
   REUSES an existing ES of the function whose filters are
   compatible (`!some_disjunction`) and only otherwise allocates
   (recording the filters on the new ES). This path has a natural
   persistent key; its defect is only that nothing stops the same
   confluence from being re-collected and re-acted-on after
   `clear_results()`.

4. **Split path B — greedy grouping (the unkeyed one).**
   `split_entry_set` (fa.cc:3598) partitions an ES's incoming
   edges into pairwise-compatible groups
   (`edge_type_compatible_with_edge`), seeds each group from
   `do_edges[0]` (greedy — group shape depends on iteration
   order), and parks each group in a NEW bare ES via
   `make_entry_set(x, ..., es, e->to)`. The only record is
   `new_es->split = orig_es`, which `clear_splits()` erases at the
   start of the next `extend_analysis`. Nothing prevents the next
   pass from regrouping the same edges differently.

5. **CS splits.** `split_css` (fa.cc:~4130) groups a CreationSet's
   defs by setter-equivalence (`same_eq_classes(v->setters, ...)`)
   and moves each group into a `new CreationSet(cs)` with
   `new_cs->split = cs`. Same per-pass amnesia as path B. NOTE:
   setter objects are hash-consed in
   `type_world.cannonical_setters`, which `clear_results()`
   CLEARS — setter pointers are therefore unusable in any
   cross-pass key.

**Stable-identity inventory** (safe to key on across passes):
`Fun*`, `Sym*`, `Var*`, `PNode*`, `MPosition*` (globally interned,
pattern.h), and `AType*` (hash-consed in
`type_world.cannonical_atypes`, which `clear_results()` does NOT
clear — verify: fa.cc:3861 clears only `cannonical_setters`).
**Unstable** (never key on): `EntrySet*`/`CreationSet*` contents,
`AVar->out` object identity mid-pass, setters, anything cleared by
`clear_avar`/`clear_es`/`clear_cs`.

## D1. The ledger data structure

Add to `FA` (fa.h), next to the type_world:

```cpp
// Issue 033: persistent record of split decisions, so re-running
// the splitter over a re-derived flow state is idempotent.
// Key components are all interned/stable (see issue 033 D0):
// fun, the FAPassStage that made the split, the argument
// position driving it, and the canonical AType partition
// assigned to the product contour. Never cleared between passes
// (only in FA::initialize / a fresh analyze()).
struct SplitDecision {
  Fun *fun;
  int stage;          // FAPassStage of the split site
  MPosition *pos;     // confluence/dispatch position (interned)
  AType *partition;   // canonical (cannonical_atypes) filter type
  int pass_made;      // analysis_pass at record time (diagnostics)
  EntrySet *product;  // ES created/selected (nullptr for CS splits)
};
```

Hashing: all four key fields are pointers with stable identity.
Use the house position-sensitive accumulation idiom (see the AType
hash in fa.cc ~590: `h += (uint)(intptr_t)ptr *
open_hash_primes[i % 256]`, primes from common/vec.h:188) over the
four fields, into a `ChainHash` (the `cannonical_atypes` pattern,
fa.h:410) or a plain `Map<uintptr_t, Vec<SplitDecision*>>`;
collisions resolved by comparing the four fields. Add:

```cpp
  SplitDecision *ledger_find(Fun*, int stage, MPosition*, AType*);
  SplitDecision *ledger_add (Fun*, int stage, MPosition*, AType*, EntrySet*);
  int dup_split_attempts;   // per-pass diagnostic counter
```

Debug-assert on insert that `partition` is canonical:
`assert(partition == fa->type_world.cannonical_atypes.put(partition))`.

## D2. Stage A — record-only + observability (land first, no behavior change)

**Status: LANDED 2026-07-10** (fa.h: `SplitDecision` +
`SplitDecisionHashFns` + `FA::split_ledger`/`dup_split_attempts`/
`ledger_find`/`ledger_add`; fa.cc: `cur_split_stage` set per stage
in `extend_analysis`, probe/record in
`find_or_make_filtered_entry_set` and `split_entry_set`'s group
loop, counter reset in `initialize_pass`, `dup_splits` appended to
the `-v` PASS line). All three suites green (pyc C 177/0, pyc LLVM
177/0, ifa-test 58 units + 20/20 fa-converge fixtures — the
fixtures lock per-pass stage events, so green there confirms
record-only). Findings against the acceptance predictions below:

- One refinement over the spec: a ledger hit whose decision was
  recorded in the *same* pass (`pass_made == analysis_pass`) is NOT
  counted — with an empty ledger, fysphun pass 1 showed 17 hits,
  all intra-pass repeats (two groups/ESs of one fun sharing a key
  within one extend), which is not the cross-pass re-derivation
  this counter is meant to expose. With the filter, pass 1 = 0.
- fysphun: `dup_splits > 0` exactly on the plateau as predicted
  (pass 6: 6, passes 8/9/10/15: 1, pass 16: 2) — key shape
  validated against the real divergence. (Trajectory differs from
  the §Symptom trace because the stall guard + reanalyze now
  converge fysphun at pass 16 with 0 violations.)
- fibheap_full: 0 dups on every pass, as predicted.
- **expr_evaluator and richards show NONZERO dups (up to 3/pass) —
  the D11-A "converging tests must be 0" criterion was too strict,
  but NOT because the key is too coarse.** LOG_SPLITTING traces
  (`[ledger] DUP ...`, kept in tree) show every hit is a
  byte-identical decision re-made across passes on a
  recursive/dispatch-heavy fun (`evaluate`, `__pyc_to_bool__`,
  `__gt__`), all on the stage-0/1 group path — including
  self-referential cases (`es 33 ... product 33`: the contour
  *created for* a partition in pass 0 re-accumulates mixed edges
  and is re-split under the identical key later). I.e. the
  per-pass-amnesia disease exists, bounded, in converging programs
  too; it terminates there only because recursion stabilizes.
  Consequence for stages B/C: enforcement will fire on converging
  inputs as well (routing those groups to the recorded product
  instead of re-splitting), so their acceptance gate is "full
  suites unchanged", not "no ledger hits outside diverging inputs".

1. Thread `int stage` (the `FAPassStage` values already defined for
   the issue-003 sidecar) into `split_edges`, `split_entry_set`,
   and `find_or_make_filtered_entry_set` call sites.
2. In `find_or_make_filtered_entry_set`: for each (position,
   AType) pair in `filters` that differs from `orig_es->filters`,
   probe the ledger. On miss: `ledger_add`. On hit:
   `++fa->dup_split_attempts` (do NOT change behavior yet).
3. In `split_entry_set`, when a group lands in a new ES
   (`x->to != es` after `make_entry_set`): compute the group's
   partition (see D4) and record/probe the same way.
4. Print `dup_splits=%d` in the `-v` PASS line (fa.cc:4509)
   and reset the counter in `initialize_pass`.

Acceptance for stage A: suites unchanged (record-only);
`pyc -v fysphun.py` shows `dup_splits > 0` beginning around pass 8
(the plateau) — this validates the key shape against the real
divergence before any enforcement. If dup_splits stays 0 on
fysphun, the key is wrong — stop and re-derive (most likely the
partition ATypes differ across passes because constants haven't
been stripped; see D4 note).

## D3. Stage B — enforce on the filtered path (SPLIT_DYNAMIC)

In `split_edges` (fa.cc:3546), before building `cs_es_map`:

```cpp
// Probe: have we already split this fun at this position for
// this receiver partition? If every receiver CS's key hits, the
// contours already exist; routing is find_best_entry_sets' job.
bool all_known = true;
for (CreationSet *cs : av->out->type->sorted)
  if (!fa->ledger_find(es->fun, stage, p, make_AType(cs))) { all_known = false; break; }
if (all_known) { ++fa->dup_split_attempts; return 0; }
```

and record each miss when its ES is made. `make_AType(cs)`
(fa.cc:219, decl fa.h:577) returns the canonical singleton, so the
key matches across passes as long as the receiver CS set is the
same — which
is exactly the "no new information" case we want to stop acting
on. If the CS set grew (new CS from a CS split), its key misses
and the split proceeds: that is the legitimate-refinement case.

## D4. Stage C — put split_entry_set's groups on filters

**Status: implemented 2026-07-10 (branch `issue033-stage-c`),
blocked on issue 035; three corrections to the design below were
established empirically and must be kept by whoever lands it:**

1. **Groups must NOT be parked on filtered entry sets.** A filter
   is a snapshot of the group's partition; when an argument widens
   in a later pass, `analyze_edge` drops the complement flow
   silently (LskipEdge / permit-intersection) because the group
   path has no per-CS edge fan-out, unlike `split_edges`. fysphun
   regressed 0 -> 3 "expr has no type" under filtered parking.
   Products stay plain bare ESs; idempotence comes from routing
   cross-pass ledger hits to the recorded product.
2. **The (pos, partition) key alone is too coarse for groups.**
   Type-value grouping is type-equality at EVERY position plus
   rets, so the ledger key needs the full signature
   (`group_signature`: hash of per-position and per-ret canonical
   partition ATypes). Single-position keys merged distinct groups
   and mistyped int results as float (builtins_batch).
3. **Nested-function contours are additionally keyed by lexical
   display.** Route a group into a product only if the whole group
   implies one display consistent with the product's
   (`group_display_ok`, mirroring `update_display`'s add-then-
   verify), and never create-then-abandon a product (empty-display
   orphans in `Fun::ess` break `make_AVar`'s display walks
   downstream — kmeanspp/pylife asserts).

Original design follows (superseded by the above where they
conflict). This is the core change. In `split_entry_set`'s group loop
(fa.cc:3652), replace the bare
`make_entry_set(x, new_edges, es, e->to)` parking with:

1. Find the confluence position `p` for `av` in `es->args` (the
   same `form_MPositionAVar` lookup `split_edges` uses; hoist it).
2. For the group `these_edges`, compute the partition:
   `AType *part = bottom; for (x : these_edges) part =
   type_union(part, x->args.get(p)->out->type)`. Note the `->type`
   accessor: every canonical AType carries its CONSTANT-STRIPPED
   view (computed during canonicalization, fa.cc ~570-600), and
   both `split_edges` and the compatibility predicates already
   key off it. Using the raw `->out` instead would make partitions
   pass-unstable (constant CSs like "3" vs int64 re-derive
   differently under the constant cap) — this is the most likely
   silent mistake in the whole implementation; the stage-A
   dup-counter check on fysphun exists to catch exactly it.
3. Probe ledger with (es->fun, stage, p, part). Hit: do NOT make
   a new ES; instead route the group's edges to the hit's
   `product` ES via `set_entry_set` (it exists — product ESs are
   never deleted) and count `dup_split_attempts`. Miss:
   `filters = es->filters + (p -> part)`, call
   `find_or_make_filtered_entry_set(es, filters)`, route the
   group there, and `ledger_add(..., product)`.
4. Keep `new_es->split = orig_es` exactly as today (check_split
   and the intra-pass short-circuits depend on it).

The greedy group SEEDING stays, but becomes harmless: whatever
order groups form in, each group's (position, partition) key is
canonical, so a re-derived pass either reproduces the same keys
(all hit; zero new contours) or produces genuinely refined ones.

## D5. Stage D — CS splits

Key shape for `split_css`: (`cs->sym`, stage, position = nullptr,
partition = canonical AType of the compatible_set's VALUE types)
is NOT sufficient — two def-groups can share value types and
differ only by setters, and setters aren't stable. Use instead the
sorted def-Var identity signature: hash of the sorted
`v->var->sym->id` list of `compatible_set`, stored in a parallel
FA-level `Map<uintptr_t, int>` (advisory, CS-only ledger). On
re-derivation the same def partition hashes identically; a hit
skips the split. This is deliberately weaker (advisory only) —
if stage A observation shows CS splits are not a driver of the
fysphun-class divergence (expected: the trace shows css stops
growing at 578 while ess oscillates), stage D can be deferred
indefinitely.

## D6. Stage E — split_for_violations non-refinability

**Status: LANDED 2026-07-10, with a finding that revises this
section's premise.** Implemented as spec'd below
(`FA::violation_split_attempts`, exclusion at >2 attempts,
`[nonrefinable]` log; plus `[stage5]`/`[stage5-attempt]`
LOG_SPLITTING diagnostics). All three suites green; fysphun/
kmeanspp/pylife traces byte-identical to the stage-A baseline.
**The predicted "kmeanspp/pylife pass counts drop" did NOT happen,
because stage 5 is not the divergence driver in the current
(post-stall-guard, post-B4) tree**: instrumentation shows
`split_for_violations` is reached at most once per compile (stages
1–4 report work on essentially every pass — the same starvation
dynamic as the §reanalyze finding, but internal to
`extend_analysis`), and when it IS reached,
`collect_violation_imprecisions` produces **0 imprecisions** from
as many as 509 violations (kmeanspp final pass: 7 -> 0, pylife:
127 -> 0, fysphun: 509 -> 0) — the numeric-coercion-shaped
residues have no container AVar and no differing-dispatch call
result, so the collector filters every violation out. The
"manufacture contours forever" arm this stage guards against is
therefore dead code TODAY; the guard still lands because stages
B/C will change exactly the condition that keeps it dead (once
stages 1–4 stop re-reporting work, stage 5 gets consulted every
pass on residual violations, and this cutoff becomes load-bearing
as designed). The live divergence driver, per the stage-A ledger
traces, is the stage-0/1 group path (`split_entry_set`) — stages
B/C remain the real fix.

Original spec follows.

Add `Map<Var *, int> violation_split_attempts` to FA (persistent).
In `split_for_violations` (fa.cc:4369): for each imprecision AVar
`av`, bump `violation_split_attempts[av->var]`. If the count
exceeds 2 (two full split attempts for the same underlying Var
that both failed to remove its violation), drop it from
`imprecisions` before splitting and log
`LOG_SPLITTING "[nonrefinable] var %d"`. Key on `Var*` (stable),
not `AVar*` (contour pointer changes as ESs are superseded).
This directly kills the stage-5 "manufacture contours forever"
arm even before stages C/D land, and is ~15 lines — it can be
landed second, right after stage A, for early corpus relief.

## D7. Stage F — ordering audit (determinism hardening)

**Status: audit performed 2026-07-09; two real gaps found and
fixed, one original claim in this section corrected, one
hypothesized deeper bug investigated and refuted.** Every
`collect_*`/`split_*` function feeding `extend_analysis` was read
end to end and traced against its consumer(s) to determine whether
consumption is (a) order-independent (membership-only test, or a
final canonicalizing sort makes traversal order irrelevant) or (b)
order-sensitive (first-match short-circuit, e.g.
`split_entry_set`'s `if (es->split) return 0`, or a greedy seed
like `do_edges[0]`/`starter_set[0]`). Findings, corrected from the
original draft of this section:

- **`collect_type_confluences` — ORIGINAL CLAIM WAS WRONG.** It
  already ends with `confluences.set_to_vec(); qsort_by_id
  (confluences);` (fa.cc:3485-3486) — verified by direct
  `git blame` (present since Feb 2026, long before this session).
  No fix needed; correcting the record here since the design doc
  is meant to be trustworthy source material for whoever implements
  D1-D6.
- **`collect_violation_imprecisions` — CONFIRMED, FIXED.** Ended
  with a bare `imprecisions.set_to_vec();`, no sort. Its output
  feeds `split_ess_for_type`'s and `split_with_type_marks`'s
  straight `for (AVar *av : imprecisions)` loops, both of which
  call `split_entry_set` per element — order determines which
  `AVar` "drives" a given ES's split this pass (subsequent AVars
  hitting an already-split ES short-circuit via `es->split`).
  Fixed: `qsort_by_id(imprecisions);` added (fa.cc, in
  `collect_violation_imprecisions`).
- **`collect_cs_setter_confluences` — NEW FINDING, NOT IN THE
  ORIGINAL LIST, FIXED.** Every sibling collector in this file
  (`collect_type_confluences`, `collect_cs_marked_confluences`,
  `collect_es_marked_confluences`, `collect_setter_confluences`)
  sorts its output; this one didn't. Its sole consumer,
  `split_for_setters_of_setters`, feeds the result straight into
  `compute_setters(..., AKIND_SETTER)` — and that function is
  called every pass from BOTH `extend_analysis` stage 3 and stage
  4, so this was a live, frequently-exercised gap, not a
  theoretical one. Doubly notable: the enclosing function already
  carries a comment describing a PRIOR order-dependence bug fixed
  in this exact spot (issue 007/032 — confluences vector clobbered
  across stages) — this collector's missing sort is a sibling
  defect the 007/032 fix didn't happen to touch. Fixed:
  `qsort_by_id(setters_confluences);` added (fa.cc, in
  `collect_cs_setter_confluences`).
- **The `Accum<AVar*> avs` fed to `split_for_setters` (stages 3
  and 4) — ORIGINAL CLAIM WAS WRONG; investigated in depth,
  concluded NO FIX NEEDED.** The original draft asserted this
  needed sorting. Tracing it fully: `avs`'s insertion order comes
  from `update_setter`'s recursive backward propagation
  (fa.cc:3953), which is a **standard memoized reachability
  closure** — for a fixed setter `s`, the early-return
  `if (av->setters->in(s)) return 0` only prunes an AVar whose
  backward recursion has ALREADY completed via some other path (a
  visited-set check, not a value chosen among competing
  alternatives), so the final `av->setters` membership is
  provably independent of traversal order. Its companion,
  `recompute_eq_classes` (fa.cc:4010), has a general "reparition"
  code path that WOULD be order-sensitive for a multi-element
  input, but its one and only call site (`compute_setters`,
  fa.cc:4048) always passes singleton `Setters` objects, for which
  that path is unreachable (a singleton either already has a
  `setter_class` and the function no-ops, or doesn't and gets one
  assigned with no other element to reparition against). `avs`'s
  own traversal order therefore doesn't affect what gets computed;
  its only consumer, `collect_setter_confluences`, already
  produces sorted, membership-only output regardless. Left
  unchanged (a code comment could be added here to save the next
  reader this investigation, but is not required for correctness).
- `split_edges`'s `all_edges` (fa.cc:3549 `qsort_by_id`) and
  `cs_es_map` construction over `av->out->type->sorted` (sorted by
  AType canonicalization) — confirmed fine as originally claimed.
- `split_entry_set`'s `all_edges` (fa.cc:3609 `qsort_by_id`) —
  confirmed fine: the edge LIST is canonically ordered, so
  `do_edges[0]`'s greedy seed is at least deterministic run-to-run.
  Note this does NOT make the GROUPING outcome itself
  order-independent of split HISTORY — that's what D1-D4's ledger
  is for; D7 only guarantees the iteration order feeding the
  greedy seed is stable, not that the greedy algorithm's grouping
  choice is optimal or history-independent.
- `split_css`'s `starters` parameter — confirmed fine: it arrives
  pre-sorted from its caller (`split_for_setters` passes
  `collect_setter_confluences`'s already-sorted `setter_starters`),
  and `split_css`'s own internal `css` local is separately sorted
  (fa.cc:4131-4132) before its `starter_set[0]` greedy grouping —
  same caveat as `split_entry_set` above (stable order, not
  history-independent grouping).
- **`fa->ess` itself — CONFIRMED foot-gun, FIXED.** Rebuilt every
  pass by `collect_results()` (fa.cc:3064) from
  `fa->entry_set_done` in WORKLIST-COMPLETION order (not sorted) —
  unlike the sibling `fa->css`, which IS sorted two lines later in
  the same function (fa.cc:3083). Every CURRENT direct consumer of
  `fa->ess` (`collect_type_confluences`, `collect_cs_marked_
  confluences`, `collect_cs_setter_confluences`, `collect_var_
  type_violations`, `fa_coerce_numeric_confluences`,
  `fa_dump_types`) either canonicalizes its own output before
  return or performs an order-independent per-(ES,Var) test/action
  with no cross-element interaction, so this was NOT a live bug —
  but it was exactly the kind of foot-gun that produces one
  silently: a future consumer that assumed sorted order (as every
  sibling collector correctly does) or that performed a greedy/
  first-match pass directly over `fa->ess` would reintroduce this
  whole class of bug with no local signal that anything was wrong.
  Fixed: `qsort_by_id(fa->ess);` added in `collect_results`,
  immediately after the `fa->ess.add(es)` loop and its `fa->funs`
  sort, before `fa->css` is derived from `fa->ess` (so `fa->css`'s
  own construction now also walks `fa->ess` in canonical order,
  though its output was already independently sorted regardless).

This stage doesn't change what converges; it makes repeated runs
byte-identical so the issue-003 sidecar fixtures can lock pass
counts. **Verified**: three consecutive `pyc -v` runs of fysphun
produce IDENTICAL ess/css/violation counts on every pass (only
wall-clock timing numbers differ in a byte diff) — before this
fix, this had not been checked run-to-run; re-verified after
landing the `fa->ess` sort with the same result. Full corpus sweep
run three times across the three D7 fixes (the two collector
sorts, then again after the `fa->ess` sort): identical 22-compiled/
55-failed split each time, same member set (ant, astar, genetic,
mandelbrot, nbody, neural2 compile clean; bh, block, brainfuck,
chess, collatz, hq2x, life, mao, neural1, pisang, richards, rsync,
sudoku1, sudoku4, voronoi, webserver compile with warnings) — no
regression from any of the three fixes. The two examples that
showed timeout/warning changes across EARLIER sweep runs (oliva2,
stereo, from before this D7 work started) were independently
confirmed to fail IDENTICALLY at the pre-D7 commit (`e7b76a37`)
across four repeated runs each, ruling out the D7 fixes as the
cause — they are pre-existing, unrelated gaps (oliva2: a
primitive-argument type mismatch; stereo: a timeout still gated by
the stall guard from the mitigation, not touched by D7).
**2026-07-10 note:** the member set above drifted after D7 landed
— still 22/55, but bh and richards regressed to FAIL while go and
loop started compiling. Bisected (worktree at `7d7a86a2`) to
`a32a6467` (issue 027's qualified-static-dispatch commit), NOT to
any issue-033 work; filed as pyc-side
[issues/028](../../issues/028-raise-exception-regression-qualified-dispatch.md).
Stage A + D6 were validated against the post-`a32a6467` set and
changed nothing.

## D8. What NOT to change

- `clear_splits` / `EntrySet::split` / `check_split`: intra-pass
  routing machinery; the ledger complements it, does not replace
  it.
- `find_best_entry_sets` scoring: product ESs carry filters, so
  existing compatibility logic already prefers them.
- The stall guard: keep as a safety net. After stages A–E it
  should never fire; add `assert(!fa->pass_limit_hit)` to the
  fa-converge FIXTURES only (not production) to catch regressions.
- `reanalyze` scheduling: unchanged; with splitting idempotent the
  splitter reports no-work as soon as contours stabilize, and the
  annotator runs as designed.

## D9. Termination argument (for the reviewer)

With enforcement on, a split happens only under a ledger MISS.
Every key's partition either equals a previous pass's (hit — no
work) or is new. New keys require new canonical ATypes at the
position, which requires the CS universe or the flow state at the
confluence to have genuinely changed. CS growth is bounded by
`split_css`'s def partitions (each split strictly shrinks
`cs->defs`, well-founded), and per-(fun, pos) partitions are
subsets of the CS universe, so the key space is finite for a
finite CS universe. Total splits are therefore finite; the outer
loop terminates without the stall guard. (The stall guard stays
because D5 is advisory and because "finite" can still be "large".)

## D10. Change inventory & sizing

| File | Change | Est. |
|---|---|---|
| `analysis/fa.h` | SplitDecision, FA::ledger fields + 2 methods, dup counter | **DONE** (stage A) |
| `analysis/fa.cc` `find_or_make_filtered_entry_set` | probe/record | **DONE** (stage A) |
| `analysis/fa.cc` `split_edges` | all-known probe (D3) | ~15 |
| `analysis/fa.cc` `split_entry_set` | group partition + filters routing (D4); record-only probe of each group **DONE** (stage A) | ~50 |
| `analysis/fa.cc` `split_for_violations` | non-refinable marking (D6) | **DONE** (no-op today, see D6) |
| `analysis/fa.cc` `split_css` | advisory signature map (D5, deferrable) | ~20 |
| `analysis/fa.cc` `initialize_pass` / PASS print | counter reset + report | **DONE** (stage A) |
| ordering audit (D7) | **DONE** (2026-07-09): `qsort_by_id` added in `collect_violation_imprecisions`, `collect_cs_setter_confluences`, and `collect_results` (`fa->ess`) | 3 lines landed |
| tests | fa-converge fixture + determinism double-run + corpus greps | — |

Land order: A (record-only) → D6 → B → C → E → (D5 if ever
needed). D7 landed out of order (audit-only, no behavior change
beyond removing two nondeterminism sources) since it was safe to
verify independently of the ledger machinery. Each lands with all
three suites (pyc C, pyc LLVM, ifa-test) plus the acceptance checks
below.

## D11. Acceptance checks per landing

- **A**: suites green; fysphun `-v` shows nonzero dup_splits on
  the plateau; dup_splits == 0 on 5 converging tests (e.g.
  fibheap_full, expr_evaluator, richards) — if converging tests
  show dups, the key is too coarse (would enforce wrongly later).
  **Outcome (2026-07-10): first two held; the third did not, and
  the "too coarse" inference was wrong — see D2 findings.
  Converging recursive programs genuinely re-make identical split
  decisions across passes (verified via the `[ledger] DUP` log
  traces), so B/C acceptance is "suites + corpus unchanged or
  better", not "no hits on converging inputs".**
- **D6**: kmeanspp/pylife pass counts drop; no suite change.
  **Outcome (2026-07-10): suites unchanged ✓; pass counts did NOT
  drop and were not going to — stage 5 is starved by stages 1-4
  and its collector yields zero imprecisions on these residues
  (see D6 status note). The guard is in place for when B/C
  un-starve stage 5.**
- **B/C**: fysphun/kmeanspp/pylife: ess monotone nondecreasing
  across passes (grep the `-v` trail), stall guard no longer
  fires (`STALL LIMIT` absent from logs), outcomes unchanged or
  improved; full corpus sweep bucket counts not worse anywhere.
- **E**: two consecutive full runs of the ifa-test fixtures
  produce byte-identical sidecar event streams (issue 003
  machinery gives this for free — extend the harness to diff).
