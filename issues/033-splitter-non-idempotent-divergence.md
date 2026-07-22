# Issue 033: Splitting loop has no fixed point on some inputs (non-idempotent, order-dependent split decisions)

**Status:** open, fix in progress. Divergence is *mitigated* (stall
guard, commit `21dbdad4`) so affected programs terminate quickly
instead of timing out; the root cause — split decisions that are
not idempotent across passes — is untouched. **S5**
(shedskin's round structure: decide-from-converged-state, persist
decisions outside the graph) **remains the intended shape of the
real fix, but M2/M3/M4 — the milestones that would actually
implement it — are each individually blocked or reverted as of
2026-07-11**, every one for a different, concretely-evidenced
reason found by testing specific real corpus programs to
completion (not by the standard suites, which none of these three
findings were visible in): M2 (batch-stage extend) hangs pygasus;
M3 (ledger persistence / stage-C revival) breaks the stall guard on
bh, reintroducing genuine unbounded divergence; M4 (dirty-marked
collection) is a no-op under the current architecture regardless of
correctness, confirmed by direct measurement before any code was
written. See each milestone's own status for the full account. Two
general-hygiene bug fixes from the M2 investigation remain landed
(a `type_intersection` null-check and an `equivalent_es_pnode`
Vec-null-slot guard) since they're correct independent of M2's
fate. Prerequisite work from before this M2/M3/M4 round (ledger
observability, a violation-splitting cutoff, a determinism/ordering
audit) is summarized in [Completed stages](#completed-stages); the
`issue033-stage-c` branch's status is in
[its own section](#issue033-stage-c-branch). Full implementation
detail for that earlier landed work, and the original stage-by-stage
ledger design S5 supersedes, is archived at
[closed/033-ledger-design-detail.md](closed/033-ledger-design-detail.md).

**UPDATE (2026-07-13): M3's blocker is dissolved — the bh
divergence was a latent hole in MAIN's stall guard, not a stage-C
defect.** The guard was not sticky (it only zeroed the current
pass's continue vote, after that pass's splitting had already
happened), the hard pass cap was gated on the very flag the guard
had just zeroed, and the outer loop is revived by `reanalyze()` —
so any input whose annotator keeps returning `true` on a frozen
violation plateau splits forever, one "zombie" pass at a time,
with the guard impotently firing every pass and NOTHING bounding
the loop (bh under stage-C blew straight through
`IFA_PASS_LIMIT=100` and was only stopped by an external timeout).
The M3 section's earlier "v < best keeps resetting stall_passes"
hypothesis was wrong — it is inconsistent with the observed frozen
count. Fixed on main (sticky guard + re-arm on genuine improvement
+ unconditional pass cap + loop-level cap; see the mitigation
section) and verified: with the fix cherry-picked onto
`issue033-stage-c-rebased`, bh terminates deterministically at
pass 32 with 23 violations — the same count and failure class as
main's bh — and the trio converges with main-equivalent results.
M3 is no longer blocked by this, and its full verification round
(suites, corpus sweep member-set + determinism, trio+pygasus+bh to
completion) **ran green the same day** — see the end of the M3
section. **M3 then LANDED (2026-07-13, this commit): the stage-C
payload was rebased onto current main (both commits cherry-picked
cleanly; the `equivalent_es_pnode` fix the branch and main had
found independently merged to a comment-only delta) and the entire
bar re-ran green on the rebased result — pyc C/LLVM 189/0, all 16
ifa-test phases zero reblessing, sweeps deterministic with the
compiled member set identical to pre-merge main, trio/bh/pygasus
outcomes byte-matching the verification round. Cross-pass ledger
routing (split persistence, the `alloc_info` analog) is now ON by
default in `split_entry_set`'s type-value group path.**

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
[034-pygasus-update-display-assert.md](closed/034-pygasus-update-display-assert.md)
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

**Hardened (2026-07-13), closing the hole found root-causing M3's
bh divergence** (see the top-of-doc update): the guard is now
STICKY — `extend_analysis` checks `pass_limit_hit` at entry and
skips the split stages entirely once it has fired, instead of
merely zeroing that pass's continue vote after the splitting had
already happened. It re-arms (clears the flag only, leaving
`best_violations`/`stall_passes` to the unchanged tail bookkeeping
so the stall accounting stays baseline-identical — an earlier
draft that reset them at entry fired the guard one pass early per
plateau) when a `reanalyze()`-driven pass genuinely improves the
violation count below its best, or resolves ALL violations
(fysphun's shape: the annotator clears the plateau and the
splitter safely resumes pure precision splitting). The hard pass
cap is now unconditional rather than gated on `analyze_again`
(which the stall guard had just zeroed — the cap was unreachable
exactly when the loop was running away), and
`analyze_to_convergence`'s loop condition is additionally bounded
by `analysis_pass <= pass_limit` so `reanalyze()`-driven passes
can't run unbounded either. Verified behavior-equivalent on
converging inputs: pyc C/LLVM suites 189/0, all 16 ifa-test phases
zero reblessing; fysphun/pylife/pygasus PASS-trajectories
byte-identical (pygasus 20 passes / 788 violations, final-pass
dirty/examined counts matching the M4 section's recorded numbers
exactly); bh identical through pass 28 with the same final state
(400 ess/1140 css/23 violations) and diagnostics; kmeanspp same 6
violations with a 3-ess-smaller final universe — the delta is
exactly baseline's final "zombie" pass splits, contours that were
created after the guard fired and never re-flowed before
termination, which the sticky guard no longer manufactures. (One
unexplained observation during verification: a single bh run with
an intermediate build of this fix segfaulted in `fa_dump_types`
under `-v` at pass 3; it never reproduced across 10+ subsequent
runs of the same scenario on either build and is noted here only
for the record.)

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
**UPDATE (2026-07-11): rebased and tested to completion against the
full corpus (see the M3 section below for the full account) — it
is NOT sound.** It breaks the stall guard on `bh`: the same
violations-plateau shape that plain main resolves cleanly at pass
29 via the stall guard instead grows unboundedly past pass 93 (492
-> 2317 ess) with no sign of stopping under this branch. The
"sound and fully green" assessment below is superseded; it was
accurate against the suites and corpus-sweep-by-bucket check
available at the time, neither of which exercises a program long
enough or in the right shape to expose this. The branch itself
(`2d514f58`) is unchanged and still parked. The rebase-onto-main
attempt that exposed the bug is preserved separately on
[`issue033-stage-c-rebased`](https://github.com/jplevyak/pyc/tree/issue033-stage-c-rebased)
(see the M3 section below) — do not revive either branch without
root-causing and fixing the stall-guard interaction first.
*(2026-07-13: done — the stall-guard interaction was root-caused
to a latent hole in main's guard itself and fixed on main; the
fixed branch terminates on bh. See the M3 correction. The branches
remain parked pending a full M3 verification round, but this
warning no longer blocks them.)*

Original assessment (superseded, kept for the historical record):
it was believed **sound and fully green** (both suites 177/0 under
the determinism gate, ifa-test green, corpus member set unchanged)
and **parked as unnecessary** for the then-current corpus: with
035's determinism fixes, the trio converges without it, and the
branch's earlier-observed pylife improvement to 52 violations
turned out to be an artifact of a since-fixed bogus-merge bug, not
a real win over main.

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

**(b) multi-candidate product** (pygasus's pass-1 shape; **LANDED**
as `tests/multi_candidate_dispatch.py`, M1):

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

3 candidates x 4 positions x {const,base,...} unions. Multi-candidate
collapsing (S4-C / M1) landed 2026-07-11 in `find_best_matches`
(`ifa/if1/pattern.cc`), generalizing 037's single-candidate
equivalence-class collapsing to K live candidates. Verified:
pygasus pass 1 32s -> 11.1s (match cost ~product -> ~sum, matching
this sketch's prediction), regression test
`tests/multi_candidate_dispatch.py` (a corrected version of this
exact sketch — the literal code above triggers an unrelated
pre-existing runtime crash mixing int/float/str through one
`print()` call, tracked in
[030-polymorphic-dispatch-fat-pointers.md](030-polymorphic-dispatch-fat-pointers.md);
the landed test keeps return types homogeneous per receiver to
avoid it while still exercising the collapsing).

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
granularity — D5 territory; **LANDED 2026-07-13** as
`tests/cs_split_pools.py`, simplified to a single shared creation
point, alongside the D5 record-only ledger — whose measurement
answered this sketch's question: cross-pass CS re-derivation does
not occur on any corpus member, see the D5 block at the end of the
M3 section):

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

**Confirmed as a real, live gap (2026-07-19),** via
[057](057-sorted-tolist-fa-nonconvergence.md)'s root-cause work — a
4-line `sorted()` + `dict.items()` repro hangs the compiler — though
the specific mechanism turned out narrower than "CPA explosion" and
closer to home: dedicated instrumentation (seven rounds, the full
trail is in 057's own file) found `tuple.__lt__` recursing into
itself (a speculative "what if these tuple elements are also tuples"
dispatch, forced by imprecise union typing) hits `check_split`'s
`e->from->split` branch — checked *before* `find_best_entry_sets`
ever runs, and able to handle (return early for) the edge entirely on
its own. That branch finds a candidate in the split-parent's
`out_edge_map` every time, but rejects it because
`edge_nest_compatible_with_entry_set` finds a **permanent** (not
drifting) closure-display mismatch between the recursive call's own
nesting context and its split-parent's recorded one — confirmed via
the specific pointer values staying constant across every sample —
and its fallback for that case mints a brand-new, still-orphaned
`EntrySet` unconditionally, one per recursive invocation, forever.
This is the disease this section's own "pending map's monomorphic-
recursion binding... is a default, not evidence" comment already
worried about — previously observed only as "re-derives every pass"
at the outer-loop level; 057 found the same disease one level down,
inside a single pass, at the level of one recursive call edge. Not
`find_best_entry_sets`/`entry_set_compatibility` (the `pyc` analog of
shedskin's `cpa()`, and this section's original target) at all —
those are barely even reached for this specific edge shape, since
`check_split` intercepts and resolves it first. Not fixed there
either — `check_split`'s `e->from->split` branch and
`edge_nest_compatible_with_entry_set` are now the confirmed,
narrow, concrete implementation point for whoever picks this up, a
smaller and more targeted surface than a general CPA_LIMIT valve
across all of `find_best_entry_sets` would have been.

**E. Subset-aware MatchCache (only if C is insufficient).** The
cache misses all through convergence because it keys exact
ATypes. Monotonicity suggests caching the per-position
viable-class PARTITION (from 037) rather than the match result:
a new AType whose CSs all fall into existing classes reuses the
cached outcome with the new CSs appended to their classes'
filters. Needs a careful invalidation story for is_exact_match
positions; measure C first — collapsing may make re-matching so
cheap the cache stops mattering.

**LANDED 2026-07-14, in a simpler form than sketched — the
measurement redirected again.** The real gap wasn't the exact-AType
key: it was that `clear_avar` wiped `av->match_cache` EVERY PASS
(survey P2's growth cap), so each pass re-matched the identical
monotone type-growth sequence from scratch. Retention across
passes is sound — entries key on canonical AType pointers at every
position including nested CS vars (canonical ATypes are never
cleared), the visibility PNode, and flags, and the pattern tables
are fixed for the whole convergence (`add_patterns` has no mid-FA
caller) — so a stale entry misses, never lies. With retention the
hit rate went 75% -> 93% and the hit path became the cost (linear
entry scan + the recursive `get_all_args` walk per lookup), fixed
by a `top_out` pointer pre-filter (the top-level args' `->out`
ATypes stored per entry — a necessary condition checked before the
full nested validation) and MRU move-to-front (at most one entry
can fully validate, so order affects only scan length). pygasus:
62.3s -> 49.9s total FA, trajectories byte-identical everywhere
(pure memoization), full bar green (C/LLVM 190/0, 16 phases zero
reblessing, sweep member set unchanged, deterministic). The
remaining ~37s match share is hit-path SEMANTIC work — per-pass
`arg_of_send` re-registration through `get_all_args` plus
per-consumer Match copies, both required for re-fire correctness
under the clear_results/re-flow architecture — i.e. per-pass
re-flow volume, not cache design. The full subset-aware partition
scheme above stays unbuilt: at a 93% hit rate with exact keys it
has no remaining target.

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
per-stage breakdown under `-v` at convergence. The dirty-AVar
counter landed later the same day, in the course of investigating
M4 (`AVar::dirty` / `FA::dirty_avar_count` / `examined_avar_count`
— see the M4 section for the mechanism and the finding it produced:
M4 is blocked on M3, not merely easier with it). Still not done:
the S3 `tests/perf/` sketches (needs sketch generation + a
wall-clock-ceiling test convention that doesn't exist yet).
Verified against all three suites (pyc C 178/0, pyc LLVM 178/0,
ifa-test all 16 phases including fa-converge) — unchanged,
confirming the instrumentation is behavior-preserving.

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
*(2026-07-14 postscript: neither batching nor dirty-marking turned
out to be the answer — the cost was per-confluence closure/collect
REDUNDANCY inside the stages, fixed directly by the joint reworks
recorded in the M5 status block; mark_type+mark_setter fell from
~106s to ~2s on pygasus.)*

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

### M2. Batch-stage extend (immutable-snapshot property, step 1) — REVERTED TWICE (crash, then a pygasus hang/severe slowdown after the crash was fixed; see status below). No part of M2a is on main. Do not retry cross-stage batching without M4 first, or without testing pygasus. The reverted diff is preserved on branch [`issue033-stage2-batching`](https://github.com/jplevyak/pyc/tree/issue033-stage2-batching) (checkpoint only, not mergeable as-is — see its note near the end of this section). **UPDATE 2026-07-13: M2b (decide-then-apply WITHIN stage 1) is LANDED — see the M2b status block at the end of this section. The M2a (cross-stage) verdict above stands unchanged.**

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

**Status: M2a attempted and REVERTED 2026-07-11 — real regressions,
not safe as literally specified.** Implementation: replaced the
four top-level `if (!analyze_again) { ...next stage... }` guards
with per-stage local result variables accumulated via
`analyze_again |=`, keeping the existing WITHIN-stage short-circuit
(setter vs setter-of-setter; mark-setter vs mark-setter-of-setter)
untouched, exactly as spec'd above.

The ifa-test synthetic fixtures behaved exactly as predicted and
were benign: fa-init/dispatch/freq diffs were pure entry-set-id
reordering (same multiset, symmetric contours swapped), fa-converge
diffs were either harmless extra same-pass no-op events (an already
this-pass-split ES correctly short-circuiting via `es->split`, per
the risk note) or, on `nested_iterator`, a genuine improvement
(3 passes -> 2, final violations 9 -> 8, matching the S5 M2 intro's
"if pass k splits stage 1 and pass k+1 splits stage 3 on the same
state, batching halves the passes" prediction). All 16 ifa-test
phases reblessed and green, stable across repeated runs.

**The pyc-level suite caught what the ifa-test fixtures didn't**:
`./test_pyc.py` went from 179/0 to 177 passed / 3 failed on
previously-clean tests, two distinct failure classes:
- `builtins_batch.py`: hard crash,
  `fa.cc:929: update_display: Assertion 'es->display[i] ==
  e->from->display.v[i]' failed` — an EntrySet for a nested-function
  contour received edges from two different enclosing-activation
  contexts. This is the exact hazard D4 (stage-C) built
  `group_display_ok` to guard against, for a different mechanism
  (there: ledger-routing a group into a product ES; here: plain
  batching lets a later stage in the SAME pass route into an ES
  whose display was committed by an earlier stage's action this
  pass) — M2a's risk note (marks-clearing, `es->split`) didn't
  anticipate this class.
- `expr_evaluator.py`, `pyc_declare.py`: no crash, but residual
  `expression has no type` diagnostics on code that compiled clean
  before — some AVar that used to get resolved no longer does.
  Root cause not yet traced; plausibly a completeness issue from
  running a later stage's collection against confluences whose
  underlying state an earlier stage already mutated THIS pass (the
  same "stale snapshot" risk the M2a note accepted for `es->split`
  purposes, but which may also cause a stage to skip work it would
  have caught had it run against a truly fresh collection).

Reverted cleanly (`git checkout` on `fa.cc` and the reblessed
fixtures); main is unaffected, still at the M1 commit. Two real
findings for the next attempt:
1. The nested-function display hazard means M2 needs a
   `group_display_ok`-equivalent guard on EVERY ES-routing decision
   made after the first stage in a pass, not just on the ledger's
   grouped-product path as D4 assumed — i.e., this is infrastructure
   M2a needs BEFORE M3, not something M3 was uniquely responsible
   for.
2. The "expression has no type" regressions suggest M2b's
   decide-then-apply restructuring (collect all stages' decisions
   against one unmutated snapshot, apply afterward) may be a
   PREREQUISITE for safe batching, not an optional follow-on
   cleanup — M2a's "keep today's interleaved mutation, just stop
   short-circuiting across stages" premise looks insufficient on
   its own. Next attempt should investigate M2b's snapshot-isolation
   FIRST, with display-consistency checking built in from the
   start, rather than retrying M2a's minimal-diff shape.

**Bisection (2026-07-11), confirming (2) is the deeper problem:**
unlocked the four top-level short-circuits ONE STAGE AT A TIME
against the full pyc suite (not just the fixtures) to isolate which
stage's batching causes the regressions.

- **Stage 2 (mark_type) unlocked alone: clean.** `builtins_batch`
  compiles and executes correctly (its only `x` is a pre-existing,
  unrelated CPython-xfail marker for the cross-verify step, not a
  pyc defect).
- **Stage 3 (setter / setter-of-setter) additionally unlocked:
  `builtins_batch` becomes clean too (the display assert is gone at
  this point), but `pyc_declare` develops a genuine WRONG-ANSWER
  miscompile** — not a completeness warning, an incorrect runtime
  result: `c2 = C("2")` (a string-valued polymorphic field) prints
  `<object>` instead of `2` on both occurrences in the test. This is
  worse than the display-consistency crash: it's silent data
  corruption in a CreationSet/setter-based field-type partition,
  not caught by any assertion. (Stage 4/5 were not independently
  re-tested after this — the full-batch run already showed both the
  display crash AND completeness regressions, and this bisection's
  purpose was root-causing, not full stage-by-stage acceptance.)

Working theory: `split_for_setters`'s `compute_setters` calls
(stage 3) read `confluences` — the AVar/ES list collected ONCE at
the top of `extend_analysis`, before stage 1 runs. When stage 1
already split entry sets this pass (which stage 2 alone never does
enough to trigger, apparently, but stage 1 + stage 3 together do),
stage 3's setter-equivalence computation runs against AVars/edges
whose underlying EntrySet graph has already been mutated by stage
1's action THIS pass — a mismatch between the snapshot `confluences`
represents and the graph's actual current state. For plain ES
routing this mismatch is caught by `es->split`'s short-circuit (per
the original M2a risk note); for setter/CS-partition decisions,
apparently nothing catches it, and the partition itself can come
out wrong. This is a stronger, more concrete version of finding
(2) above: M2a is not just risky in the abstract, it has a
demonstrated wrong-answer failure mode, confirmed to originate in
stage 3's setter splitting reading a stage-1-mutated graph through
a stale confluence snapshot. Fixing this almost certainly requires
either M2b's full decide-then-apply isolation (compute every
stage's decisions against the SAME pre-mutation snapshot, apply all
of them afterward) or, short of that, re-collecting `confluences`
fresh before each stage that runs after a mutating stage — the
latter defeats most of M2a's cost-saving intent (the point was to
reuse one collection across stages) and reintroduces per-stage
collection cost, so it is not a real shortcut around M2b.

Both attempts left no code changes on `main` at the time (reverted
cleanly each time; verified pyc C 179/0 before and after).

**Follow-up investigation (2026-07-11): tried a narrower "re-collect
`confluences` fresh before every stage" variant (instead of reusing
one snapshot across stages 1-4) to test whether staleness was the
whole story.** It was not: identical 3 failures (`builtins_batch`
crash, `expr_evaluator`/`pyc_declare` incompleteness). `pyc_declare`
specifically now shows `c4` (not `c2`) with "has no type", and
`-v` shows it converging in 5 passes at 12 residual violations,
versus the baseline's 13 passes with a resolving jump from 18 to 0
violations between passes 5 and 6. So the batched version isn't
just producing a locally wrong partition — it's **declaring
convergence prematurely** while real, resolvable violations remain
stranded. This ruled out plain snapshot staleness as the root
cause; something deeper was permanently losing information.

**Root cause, confirmed mechanistically (not just empirically):**
`analyze_to_convergence`'s outer loop is
`do { flow-to-fixpoint; complete_pass(); } while (extend_analysis() || reanalyze());`
(fa.cc ~5082). Flow propagation (the edge/send/es-worklist loops)
runs *only* in that outer loop, strictly BEFORE `extend_analysis()`
is called — `extend_analysis()` itself never re-enters flow. So any
new contour a stage creates mid-call (e.g. stage 1's
`split_ess_for_type`) has **zero flow-derived type information**
for the rest of that same call: its `av->out->type->n == 0` until
the *next* outer-loop iteration re-flows it. This isn't a staleness
bug fresh re-collection can paper over — `collect_type_confluences`
correctly *discovers* the new AVar/edge structurally, but there is
no type data yet to collect.

This explains why stage 2 and stage 3 react to the same unflowed
contours completely differently:
- **Stage 2** (`split_ess_for_mark_type`) needs >=2 distinct types
  at a position to call something a confluence. An unflowed contour
  contributes 0 types, so it just fails that test and is silently
  skipped — a benign, non-committal no-op, correctly deferred to
  next pass when flow catches up.
- **Stage 3** (`split_for_setters` -> `split_css`) has an identical
  guard in `compute_setters`
  (`if (akind == AKIND_TYPE && !x->out->type->n) continue;`, fa.cc:4246)
  that *also* skips unflowed contours — but `split_css` then
  **permanently partitions** a CreationSet's `defs` from whatever
  incomplete `starters` subset it saw this call
  (`cs->defs.set_difference(...); cs->defs.move(new_defs);`,
  fa.cc:4352-4354). CS splits only ever subdivide, never re-merge —
  there is no mechanism to revisit a partition once made. An
  incomplete view produces a wrong, IRREVERSIBLE commitment, not a
  deferral. (Stage 4 shares the same `split_css` call path and is
  presumably equally unsafe, though not separately re-verified after
  this finding — see "what's still open" below.)

The `[scss]`/`SPLIT CS` log trace from the earlier bisection is a
direct illustration: baseline waits until pass 5 (once stages 1-2
have exhausted their own ES-graph growth across 4 full passes,
22->32 ess) before stage 3 gets a turn at all, then partitions the
`C`-instance CreationSet into its full 6-way split in one shot.
Batched-with-stage-3 gets a turn on pass 1 — while the ES graph is
still only 22 entries, four passes short of grown — sees 1 (then 5,
never 6) starters, and locks in a partial partition that later
passes cannot correct.

**Landed a scoped-safe partial win reflecting this** (`analysis/fa.cc`,
same commit as this write-up): stage 2 unlocked unconditionally
(batched with stage 1, no short-circuit between them); stages 3, 4,
5 left exactly as before (still gated behind
`if (!analyze_again)`), since only stage 2's failure mode is proven
benign. Verified: pyc C 179/0, pyc LLVM 179/0, all 16 ifa-test
phases green with **zero fixture reblessing needed** (unlike full
M2a, which touched fa-init/dispatch/freq/fa-converge); shedskin
corpus sweep **25 compiled (was 23), a strict superset** — `oliva2`
and `kanoodle` newly compile clean, nothing lost; stable across two
repeated sweeps. `bh`/`pygasus` unchanged (still their known
pre-existing failures).

**What's still open for a future attempt:**
1. Stage 3/4 batching needs either (a) re-running flow-to-fixpoint
   between stage transitions within `extend_analysis` itself — a
   materially bigger change than "decide-then-apply on one
   snapshot" (M2b as originally scoped assumed the snapshot itself
   was the only problem; it isn't, the missing ingredient is a flow
   step, not just consistent data), or (b) M3/M5's round-based
   redesign, which sidesteps the issue by construction: splits are
   only ever decided from a state that has ALREADY been fully
   flow-converged, and new contours get their own fresh round with
   its own flow pass before anything downstream tries to partition
   them.
2. Stage 4 was never independently isolated (only tested bundled
   with 1+2+3 or the full 1-5 batch); given it shares `split_css`'s
   irreversible-partition mechanism with stage 3, it should be
   assumed equally unsafe to batch until proven otherwise, not
   tested as a candidate for a stage-2-style scoped win.
3. Stage 5 (`split_for_violations`) doesn't consume `confluences`
   at all (reads the live `fa->type_violations` set directly) and
   was never implicated in either failure — plausibly safe to batch
   in isolation, but not verified; worth a dedicated bisection
   before assuming so.

**CORRECTION (2026-07-11, later same day): the stage-2 batching
above was reverted — it crashes fysphun.py.** While preparing M3
(rebasing `issue033-stage-c` to attempt the ledger revival), the
rebased branch segfaulted on fysphun deterministically, partway
through pass 15, inside `Vec<CreationSet*>::begin()` (confirmed via
kernel segfault log address resolution — `addr2line` against the
crash `ip`, consistently at the same offset across repeated runs).
Bisecting against the branch's parent commits on main showed the
crash reproduces on **plain main with only the stage-2 change
applied, no stage-C** — checking out the M1-only commit
(`bf708ea6`, before stage-2 batching landed) and rebuilding makes
the crash disappear; fysphun converges cleanly again (18 passes, 0
violations). So the "stage 2 is safe" conclusion above was wrong,
or at least incomplete: it's safe against every fixture and test
this repo has *except* a real, long-running (15+ pass), numerically
heavy program — exactly the class of input this issue is about.

**The precise mechanism was not pinned down before reverting** (the
crash site, `Vec<CreationSet*>::begin()`, is called from hundreds of
sites in fa.cc via range-for loops, and reproducing it under `gdb`
was impractically slow — ptrace overhead alone pushed it well past
90s where the un-instrumented binary crashes in under 60s). What is
known: it happens at the START of pass 15 (right after pass 14's
actions, which included a stage-2 split), inside stage 1's
processing, on a fun/EntrySet whose graph structure stage 2's
now-more-frequent action apparently corrupted or left inconsistent
on the prior pass. This is consistent with — though not proven to
be the same bug as — the general class this section already
diagnoses (a stage's action within a pass invalidating a premise a
later consumer relies on), just manifesting as a dangling/garbage
container pointer rather than a wrong-answer partition or a display
assert. A future attempt should instrument `EntrySet`/`CreationSet`
construction and destruction-adjacent paths (contours are never
freed, per D0, so "dangling" here likely means "never properly
linked into whatever structure stage 1's next-pass traversal
expects," not a use-after-free) and reproduce with a much smaller
input than 222-line fysphun if at all possible.

**Reverted cleanly**: `analysis/fa.cc`'s stage 2 gate is back to
`if (!analyze_again)`, byte-for-byte the same as before any M2 work
started. Verified: pyc C 179/0, pyc LLVM 179/0, all 16 ifa-test
phases green, shedskin corpus sweep back to the exact 23-member
baseline (the `oliva2`/`kanoodle` gains from the buggy stage-2
change are gone — an acceptable trade for not shipping a crash).
`issue033-stage-c`'s local branch pointer, which the M3 rebase
attempt had moved forward, was reset back to its original commit
(`2d514f58`) — not pushed anywhere, no shared state affected.

**Process lesson, stated plainly because it cost real time**: the
verification bar used for the (reverted) stage-2 change — full pyc
C/LLVM suite, all ifa-test phases, one shedskin corpus sweep
comparing compiled-member sets — was not sufficient for a change to
`extend_analysis`. Every one of those either runs briefly (the pyc
test corpus) or only checks whether a `.c` file gets emitted (the
corpus sweep), neither of which exercises 15+ passes of sustained
splitting on a real program. **Any future change to
`extend_analysis` must be checked against the shedskin trio
(fysphun, kmeanspp, pylife) and pygasus specifically, run to
completion (or the stall guard), not just folded into the generic
suites** — this issue's whole subject matter is exactly the
many-pass, large-contour-universe regime those three exercise and
the standard suites don't.

M2 is now fully reverted; nothing from M2 is on main. M3 is
therefore blocked on a correctly-diagnosed, correctly-fixed M2 (or
an M3 design that doesn't depend on M2 batching at all — worth
reconsidering, since `issue033-stage-c`'s own `group_signature`
wildcard-rejection is a general defense against incomplete-type-info
hazards and may not need M2 as a prerequisite; see the branch's
mechanics above).

**ROOT CAUSE FOUND (2026-07-11, later still): a real, fixable bug —
missing null-checks on `Map::get()` misses, not a fundamental
architecture problem.** `gdb` was far too slow to catch the crash
live (ptrace overhead pushed even a 90-280s budget past the point
where the un-instrumented binary already crashed in under 60s).
Building a throwaway worktree with AddressSanitizer instead
(`-fsanitize=address -fno-omit-frame-pointer` added to both
Makefiles; `-fsanitize-ignorelist` and a one-line skip for
primitive index 57 to route around an unrelated pre-existing
`initialize_primitives` global-buffer-overflow on `await`'s
registration, out of scope here) got a complete, symbolized stack
trace on the first run:

```
#0 Vec<CreationSet*>::begin()               common/vec.h:105
#1 type_intersection(AType*, AType*)        analysis/fa.cc:694
#2 edge_type_compatible_with_entry_set(...) analysis/fa.cc:841
#3 split_entry_set(...)                     analysis/fa.cc:3788
#4 split_ess_for_type(...)                  analysis/fa.cc:4479
#5 extend_analysis()                        analysis/fa.cc:4670
```

`type_intersection(AType *a, AType *b)` (fa.cc:681) never null-checks
`b` before `for (CreationSet *bb : b->sorted)` (fa.cc:694).
`edge_type_compatible_with_entry_set` (fa.cc:835) calls it as
`type_intersection(e_arg->out->type, e->match->formal_filters.get(p))`
(fa.cc:811, 812, 841) — a direct, unchecked `Map::get()` result. A
miss there is not a bug in the map: `analyze_edge` (fa.cc:2592),
the flow-step consumer of the SAME map, already treats a missing
filter as a legitimate "no constraint" state
(`if (filter) { ... } else filter = es_filter;` at fa.cc:2605-2608,
and `if (filter && ...) goto LskipEdge;` at fa.cc:2609 — a null
filter simply skips the check). `edge_type_compatible_with_entry_set`
never got the memo. **Fix, verified**: two lines at the top of
`type_intersection` —
```cpp
if (!b) return a;
if (!a) return b;
```
— matching `analyze_edge`'s existing null-is-unconstrained
semantics. With the fix, fysphun's FA no longer crashes: it runs
past pass 15 (previously fatal) through pass 17, **reaching 0
violations** (358 ess / 570 css at pass 17, vs. baseline's 218/556
at pass 18 — a different but not obviously worse shape; the analysis
genuinely converges under stage-2 batching once this crash is out of
the way).

**A second, different crash follows immediately after, same
pattern, different subsystem.** With the `type_intersection` fix
in place, fysphun crashes again a few passes later — no longer in
FA/`extend_analysis` but in the CLONE phase:
`equivalent_es_pnode(PNode*, EntrySet*, EntrySet*)` (clone.cc:147)
reads `ee->to->equiv` for every edge in an ES's `out_edge_map`
(clone.cc:152) without checking `ee->to` for null — and `to` is
exactly the field `split_entry_set` zeroes mid-split
(`x->to = 0;` before rerouting, see D0/D4) before reassigning it.
Not independently root-caused beyond identifying the exact null
dereference and its likely source (an edge examined by clone
equivalence-checking while mid-reroute) — this is reported, not yet
fixed.

**What this changes about the picture**: the earlier
"flow only happens between outer passes, so any post-first-stage
contour is fundamentally unsafe to examine" theory was too
pessimistic as a blanket claim. The actual failure mode is narrower
and much more ordinary: specific call sites across the codebase
(so far: one in `fa.cc`'s split machinery, one in `clone.cc`'s
equivalence checking) silently assumed invariants — "a formal
filter is always recorded," "an edge's `to` is always resolved" —
that held under the OLD strictly-sequential pass structure (by the
time any of these functions ran, everything upstream had a full
pass to settle) but don't hold once M2's batching lets stages,
and the graph mutations they cause, compound within a single pass.
This is a **finite, auditable class of bug** (missing null-checks
on values that are legitimately absent in an in-flux state), not
proof that M2-style batching is unsound. It is NOT yet proven that
this is the *only* remaining class, or that all instances of it have
been found — two were found in the course of getting ONE test
program (fysphun) to run a few passes further, and both were found
by symptom (a crash), not by systematic audit.

**Both fixes landed (2026-07-11, same day, standalone).** The
`clone.cc` crash turned out simpler than expected once traced
precisely: the ASAN fault address (`0x10`, matching
`offsetof(AEdge, to)` exactly) meant `ee` itself — the `AEdge*`
Vec element — was null, not `ee->to`. `out_edge_map`'s vectors are
set-mode (built via `set_add` in `initialize()`), and this
codebase's own convention (`.n` is table capacity, not live count;
every other set-mode-Vec consumer guards with `if (x)`, e.g.
`for (AEdge *e : es->out_edges) if (e) {...}` a few lines above in
the same file) already establishes that `[0,n)` legitimately
contains null slots. `equivalent_es_pnode` was simply missing that
standard guard — not a semantic judgment call at all, unlike
`type_intersection`. Fix: `for (AEdge *ee : *va) if (ee && ee->to) ...`.

Landed both fixes as a standalone commit (not re-enabling M2's
stage-2 batching in the same commit — that's still a separate
decision). Verified behavior-neutral on today's unbatched main
(pyc C/LLVM 179/0, all 16 ifa-test phases, zero fixture reblessing
— the null cases these guards catch don't arise without M2's
altered pass timing). Verified the actual fix under ASAN with M2's
stage-2 batching re-applied in a throwaway worktree: fysphun's FA,
previously crashing at pass 15, now converges cleanly to 0
violations through pass 18; the shedskin corpus sweep with M2
re-applied shows 25 compiled (up from 23: `oliva2` and `kanoodle`
newly clean), no crashes anywhere in the failure buckets (only the
same benign "has no type" warnings and pre-existing "cannot find
module" entries as baseline). `kmeanspp`/`pylife` were checked in
the non-ASAN corpus sweep (clean, no crashes, same benign warnings
as baseline) — an ASAN-specific run for those two hit an unrelated
worktree module-path-resolution quirk not chased down (confirmed
unrelated: reproduces identically with `-D` pointed at either the
worktree or the main repo, and disappears entirely outside ASAN).

**Re-attempted the same day, following the process lesson exactly —
and it correctly caught a THIRD, more severe problem: M2's stage-2
batching hangs (or is catastrophically slower than) pygasus.**
Re-applied the stage-2 batching commit on top of both crash fixes,
verified the full bar in order: pyc C/LLVM 179/0, all 16 ifa-test
phases with zero fixture reblessing, two consecutive corpus sweeps
(deterministic, 25 compiled, no crashes) — all clean, as expected.
Then, per the process lesson, ran the trio and pygasus to actual
completion instead of stopping at the corpus sweep's coarse
pass/fail bucketing:
- fysphun: clean, 18 passes, 0 violations at pass 17, no crash —
  matches the ASAN verification.
- kmeanspp: clean, 21 passes, 5 violations (vs. the documented
  baseline's 6 — equal or better), no crash.
- pylife: clean, 13 passes, 60 violations — exact match to the
  documented baseline, no crash.
- **pygasus: never printed `PASS 1`. Ran at 99.9% CPU for 7+
  minutes with the output file completely static (confirmed via
  two checks 135 seconds apart, zero new lines) before being
  killed.** Reverting stage-2 batching and rebuilding restored
  pygasus's PASS 1 to 11.4 seconds, byte-for-byte matching the
  earlier (pre-M2) measurement — ruling out environmental
  flukiness and confirming the stage-2 change is unambiguously the
  cause.

**Why the trio didn't catch this: it's a SCALE problem, not a
many-pass problem.** The trio (200-400 ess) already showed, via
M0's own measurement, that stage 2's collection is disproportionately
expensive relative to how often it wins (`mark_type` 70-89% of
extend cost while `type_confluence` wins most passes) — but at
that scale it's merely wasteful, not fatal. `extend_analysis`'s
original short-circuit (`if (!analyze_again)`) meant stage 2 was
skipped entirely on every pass where stage 1 found new work — and
for a large program, stage 1 keeps finding new work for MANY passes
before it ever runs dry. Pygasus's pass 1 alone discovers 904 ess /
3673 css (10x the trio's total contour count); under M2's batching,
stage 2's expensive mark-based collection now runs unconditionally
against that entire freshly-discovered universe on pass 1 itself,
before stage 1 has had any chance to let the universe stabilize.
Whatever stage 2's collection cost scales with (contour count,
likely worse-than-linear given the trio's own 70-89% share at 1/10th
the scale), pygasus's pass 1 hits it at full unmitigated force. This
is exactly the cost M4 (dirty-marked collection) exists to fix —
this finding suggests M2 and M4 may not be independently orderable
the way S5 originally sequenced them: **stage-2 batching without
M4's dirty-marking to bound its collection cost is not viable on
large programs**, not merely "not yet verified."

**Reverted again**, same day. Verified: pyc C 179/0, pygasus PASS 1
back to 11.4s. Nothing from M2 is on main; the two crash fixes
(which are unconditionally good hygiene regardless of M2's fate)
remain landed.

**Recommendation for any future attempt**: don't re-attempt stage-2
batching in isolation again. Either (a) land M4 (dirty-marked
collection) FIRST, so stage 2's collection cost is bounded by what
actually changed rather than the full contour universe, then retry
M2's batching on top of it, or (b) test any future `extend_analysis`
change against pygasus SPECIFICALLY and FIRST, before the trio —
it is the one corpus member whose scale actually exercises the cost
dimension this issue is about, and every other test in this repo
undersells that dimension by 1-2 orders of magnitude.

**The exact reverted change is preserved on branch
`issue033-stage2-batching`** (pushed to origin, built on top of the
two crash fixes above — do NOT merge as-is; it's the same code that
hangs pygasus). Unlike `issue033-stage-c`, this isn't a design
that's sound-but-parked — it's confirmed not viable standalone.
Its value is purely as a checkpoint: the batching diff itself is
small and mechanical (see D8-adjacent reasoning above for exactly
what it does and doesn't touch), so there's no design work saved by
keeping it, only re-typing. Revive it by rebasing onto whatever
lands M4, not by merging it on its own.

**M2b: decide-then-apply within stage 1 — LANDED 2026-07-13.**
`split_entry_set` is now split into `decide_entry_set_split` (pure:
computes the group partition — edge classification, the greedy
pairwise-compatible grouping, all short-circuits including the
remainder-stays rule — against an unmutated graph, into an
`ESSplitDecision` record) and `apply_entry_set_split` (all
mutation: pending-backedge-map rebuild, detach, ledger-route or
park, stage-A recording), with `split_entry_set` reduced to a thin
wrapper that runs the two back-to-back — so stages 2–5 keep their
old per-AVar semantics through the same code, and any future stage
batches by reusing the pair. Stage 1 (`split_ess_for_type`,
non-dynamic path) now DECIDES every confluence against the same
converged snapshot before applying any of them, removing the
intra-stage order dependence (the 009/021 family) structurally
rather than by qsort suppression. One deliberate semantic change:
when two confluences target the SAME EntrySet at different
positions, the old shape split it a second time in one pass,
partitioning edges that had been rerouted THIS pass and never
re-flowed — the M2a unflowed-contour hazard in miniature. Now the
first decision per ES wins and later ones defer to the next pass's
re-collection (which decides them against re-flowed state). The
dynamic path (stage 5's refinable violations) keeps the legacy
shape: `split_edges` mutates as it goes, so batched deciding there
would read its own stage's mutations.

Verified per this section's own process lesson, pygasus first:
pygasus 20 passes / 788 violations / 3451 ess with a byte-identical
PASS trajectory (no hang — M2b adds no collection cost, unlike
M2a's stage-2 unlock); fysphun/kmeanspp/pylife/bh PASS trajectories
byte-identical to the D5-landing baseline. The deferral is NOT
dead code — it fires 14/6/9 times on fysphun/kmeanspp/bh — yet
outcomes are unchanged everywhere, confirming the old same-pass
second splits were no-ops re-derived against the post-split
remainder (now they're structurally impossible instead of
accidentally harmless). pyc C 190/0, LLVM 190/0, all 16 ifa-test
phases with zero fixture reblessing (fa-converge event-identical),
sweep member set unchanged.

What M2b deliberately does NOT change: no cross-stage batching
(M2a's pygasus verdict stands — stage 2's collection cost needs
M4-class bounding, and stages 3/4 need a flow step or the round
structure). What it unblocks: split decisions are now first-class
records applied through one funnel, which is exactly the shape the
S5-B round structure needs — a round computes its complete
`Vec<ESSplitDecision *>` from converged state and applies it.

### M3. Ledger persistence from converged snapshots (stage-C
revival; the alloc_info analog) — ATTEMPTED 2026-07-11, REVERTED: rebased branch breaks the stall guard on `bh`, a genuine unbounded divergence. See status near the end of this section. **UPDATE 2026-07-13: LANDED — the bh divergence root-caused to a latent stall-guard hole on MAIN (not a stage-C defect), fixed; full verification bar green twice (on the old base, then re-run on the payload rebased onto current main), merged. See the top-of-doc update and the correction + verification record at the end of this section.**

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

**Status (2026-07-11): attempted, reverted — a real, severe
divergence, caught by testing a full corpus member to completion
per this issue's own hard-won process lesson.** Rebased
`issue033-stage-c` (`2d514f58`) onto current main (21 commits
behind; M0/M1, the two crash fixes, and the M4 probe had landed
since its last rebase) in an isolated, detached worktree — three
conflicts, all trivial-to-moderate: two pure comment collisions
(kept both sides' reasoning where they differed — see below), and
one genuine merge of `FA::dirty_avar_count`/`examined_avar_count`
(new on main) alongside the branch's `SplitDecision::sig` field
addition (both needed, no overlap). Notably, one of the "comment"
conflicts revealed that **the stage-C branch had independently
found and fixed the exact same `equivalent_es_pnode` null-`AEdge*`
bug this session's ASAN investigation found** — on fysphun, during
the branch's own earlier development. Two independent
investigations, same bug, same fix, same reproducing input —
strong corroboration the fix itself is correct; merged both
comments' reasoning into one.

Standard bar all green: pyc C/LLVM 179/0, all 16 ifa-test phases
with zero fixture changes, corpus sweep 23 compiled (the pre-M2
baseline set — stage-C alone doesn't add M2's oliva2/kanoodle
gains, expected since they're unrelated), no crashes. **Then,
per the process lesson from the M2 investigation, checked every
corpus member's outcome for anomalies before declaring success —
not just the pass/fail bucket — and `bh` had silently changed from
its usual fast, benign failure to `(compile timeout 90s)`.**

Investigated directly rather than dismissing it as noise:
- **Plain main (no stage-C) on the identical `bh.py`**: violations
  plateau at 23 for passes 20-28 (9 non-improving passes — at or
  past `IFA_STALL_LIMIT`'s default of 8), `dup_splits` frozen at 3
  throughout, ess/css still climbing each of those passes. The
  stall guard correctly fires: dirty-AVar count collapses from
  ~14000 to 419 at pass 29 (the guard's forced-stop signature), and
  the compile ends normally with the same pre-existing "Exception
  has no type" diagnosis this repo already knows about.
- **Rebased `issue033-stage-c` on the same `bh.py`**: the identical
  plateau SHAPE (violations frozen, this time at 29, `dup_splits`
  frozen at exactly 4) — but it never stops. By the time the
  external 90-second timeout killed it, `ess` had grown 492 -> 2317
  and `css` 1086 -> 2473 over 73+ consecutive non-improving passes
  — nine times past the stall limit with no sign of slowing, each
  pass more expensive than the last as the ever-growing universe
  gets rescanned (per-pass extend cost climbing from ~2.1s to
  ~3.6s across the tail alone).

**This is the exact pathology issue 033's stall guard exists to
catch, and stage-C's ledger-routing changes defeat it on this
input** — not a performance regression like M2's pygasus finding,
a correctness regression in the safety net itself. Not root-caused
to the specific line: `dup_split_attempts`/`FA::stall_passes`/
`FA::best_violations` live in `extend_analysis`'s outer bookkeeping,
which the stage-C diff doesn't directly touch, so the interaction
is indirect — most likely something about ledger-routed groups
causing `fa->type_violations`' count to look like it's improving
(or never quite failing to improve) when it structurally isn't,
letting `v < fa->best_violations` keep resetting `stall_passes`
before it can reach the limit. Worth root-causing with the same
ASAN-first, measure-before-fixing discipline the M2 crashes used,
rather than guessing at a patch.

**CORRECTION + ROOT CAUSE (2026-07-13): the hypothesis above was
wrong, and the guard was the bug.** A frozen violation count
cannot keep beating its own best — the observed plateau (v frozen
at 29) is inconsistent with `v < best_violations` resetting
anything. The actual mechanism, read straight off main's code: the
stall guard fired every pass, but (a) it was not sticky — it only
zeroed that pass's `analyze_again` AFTER the split stages had
already run and split, (b) the hard pass cap was gated on
`analyze_again`, which the guard had just zeroed, so the cap was
unreachable on exactly this path, and (c) the outer loop is
`extend_analysis() || reanalyze()`, so a frontend annotator that
keeps returning true (which stage-C's routed contours apparently
cause, by perturbing the residual violation set each pass) revives
the loop indefinitely. Every revived pass runs the full split
stages again — unbounded "zombie" splitting with the guard firing
impotently each time. Re-reproduced on the unfixed
`issue033-stage-c-rebased` worktree: bh blows through
`IFA_PASS_LIMIT=100` (2492 ess at pass 100, still growing, killed
by external timeout). Main has the identical hole, merely masked:
on main's bh the annotator happens to quiesce right after the
guard fires, and main's bh baseline in fact contains 2-3 zombie
passes (ess 394 -> 400 on a frozen 23-violation plateau) whose
split products were never re-flowed before termination. Fix landed
on main (sticky guard + bounded re-arm + unconditional pass cap +
loop-level cap; details in the mitigation section). With the fix
cherry-picked onto the branch: bh terminates deterministically at
pass 32, 717 ess / 1257 css / 23 violations — the same final
violation count and failure class as main's bh — and the trio
converges with main-equivalent results (fysphun 18/0 identical,
kmeanspp 21 passes/6 violations, pylife 13/60). M3 is therefore
unblocked; revival still owes its own full verification round
(suites, corpus sweep member-set, trio+pygasus+bh to completion)
before merging.

**VERIFICATION ROUND COMPLETE (2026-07-13, same day): the full M3
acceptance bar is green on `issue033-stage-c-rebased` (`24871ab3`)
+ the stall-guard fix, applied in a detached worktree.** Results,
against the branch's own baseline and current main:

- pyc C suite 179/0, pyc LLVM suite 179/0 (the branch's corpus
  size — it predates the match/case tests), all 16 ifa-test phases
  green with zero fixture changes.
- Corpus sweep: 23 compiled, **member set identical to main's**,
  byte-identical `results.tsv` across two consecutive sweeps
  (determinism). The single outcome-text difference across all 77
  examples is `chull` reporting a different variable first
  (`'fold'` vs `'v2'` has no type) within the same failure class.
  bh's sweep line matches main's exactly — the
  "(compile timeout 90s)" anomaly that flagged the original
  divergence is gone.
- bh: 32 passes / 23 violations, deterministic across repeated
  runs; same final violation count and failure class as main's 29
  passes / 23.
- Trio to completion: fysphun 18/0 (identical to main), kmeanspp
  21/6, pylife 13/60 (main-equivalent).
- pygasus to completion: 20 passes, **788 violations — identical
  diagnosis to main** — ~6% faster in total FA wall time (158.6s
  vs 169.1s summed pass time) with a smaller final universe (3451
  vs 3594 ess; the ledger's route-hits visibly replace re-splits
  from pass 4 on).
- The one original acceptance criterion NOT met: "ess growth
  monotone across passes" — but main itself doesn't meet it (both
  dip identically when the coercion annotator engages, e.g.
  3827 -> 3658 ess main pass 11 -> 12), so it is not a stage-C
  regression; it remains an aspiration for full S5 (converged-state
  deciding + persistence), not a gate this branch can be held to.

**What's left before merging is a decision plus mechanical work,
not verification**: the branch is built on an older main (it
predates the match/case + issue-024/026 commits and this fix), so
landing means rebasing the stage-C payload onto current main
(which already contains the guard fix) and re-running this same
bar once on the result. Note also that enforcement changes
analysis outcomes only in the routed cases — the observed
universe-shrink on pygasus/bh is the intended effect (per-pass
re-split manufacturing replaced by routing to the recorded
product), and every observed final diagnosis is unchanged.

**LANDED (2026-07-13, same day, this commit).** Both payload
commits cherry-picked cleanly onto current main (no conflicts; the
`equivalent_es_pnode` fix that the branch and main had each found
independently merged to a comment-only delta in `clone.cc`),
squashed into this single landing commit. The full bar re-ran
green on the rebased result before merging:
- pyc C 189/0, pyc LLVM 189/0 (current main's corpus, including
  the match/case tests the old branch base predated), all 16
  ifa-test phases with zero fixture reblessing.
- Corpus sweep 23 compiled, member set identical to pre-merge
  main, byte-identical across two consecutive sweeps; `chull`'s
  first-reported-variable difference is the only outcome-text
  delta, same as the pre-rebase round.
- To completion, all byte-matching the pre-rebase verification
  round: fysphun 18/0, kmeanspp 21/6, pylife 13/60, bh 32 passes /
  23 violations, pygasus 20 passes / 788 violations (~165s total
  FA, 3451 final ess vs main's 3594).
Cross-pass ledger routing is now on by default for
`split_entry_set`'s type-value group path. The remaining S5-M3
scope NOT covered by this landing: extending the ledger to CS
splits (the D5 key shape, sketch (e)) and deciding from converged
snapshots (M2b), which stay open under their own milestones.

**CS-split ledger (D5): record-only LANDED 2026-07-13; enforcement
deferred WITH EVIDENCE — measured zero cross-pass CS re-derivation
on the entire corpus.** What landed (`analysis/fa.cc`/`fa.h`):
`cs_group_signature` (the D5 key, strengthened per the ES ledger's
builtins_batch lesson: `cs->sym` + the sorted def-Var sym ids of
the compatible group + each setter AVar's Var id paired with its
canonical constant-stripped value type — Setters/setter_class
pointers live in `cannonical_setters`, which `clear_results`
clears, so setter CONTENT is the stable proxy; same
wildcard-rejection rule as `group_signature` for unflowed setter
values), `ledger_find_cs`/`ledger_add_cs` (fun/pos/partition-null
keys, identity entirely in `sig`, so they cannot collide with ES
keys; stage deliberately excluded — a stage-3-then-stage-4
re-derivation is still a re-derivation), a
`SplitDecision::cs_product` field recording the product
CreationSet (contour pointers are never freed, so routing later is
a small step), a `cs_dups` counter in the `-v` PASS line, and
`RECORD/DUP/NO IDENTITY` LOG_SPLITTING lines in `split_css`.

The measurement (validated first — RECORD counts confirm the key
is live and wildcards never fire, so the zeros are real):

| input | SPLIT CS events | unique recorded | intra-pass repeats | cross-pass dups | wildcards |
|---|---|---|---|---|---|
| fysphun | 9 | 6 | 3 | 0 | 0 |
| kmeanspp | 16 | 12 | 4 | 0 | 0 |
| pygasus | 36 | 10 | 26 | 0 | 0 |
| pylife, bh | 0 | — | — | — | — |
| sketch (e) test | 1 | 1 | 0 | 0 | 0 |

Interpretation: CS split decisions ALREADY persist across passes —
`av->cs_map` is the one piece of state `clear_results` preserves
(the S1 table's "what persists" row), so a split's def-repointing
survives and the next pass's flow re-populates the partitioned
defs rather than re-deriving the split. D5's own deferral clause
("if stage A observation shows CS splits are not a driver … stage
D can be deferred indefinitely") applies, now with corpus-wide
numbers instead of a prediction. Routing enforcement is therefore
NOT landed — it would be unreachable code with real hazard surface
(the ES ledger needed display-feasibility and wildcard guards to
route safely; the CS analog would need its own soundness argument
for merging same-key groups across contours). The metric stays on
permanently: if a future change (M2 batching, M5 reset-and-reseed
— which resets exactly the `cs_map` state this persistence lives
on) makes `cs_dups` go nonzero on any corpus member, that is the
signal to build routing, and `cs_product` is already recorded for
it. Landed `tests/cs_split_pools.py` (S3 sketch (e), simplified to
a single shared creation point that only `split_css` can
disambiguate) locking the split's end-to-end precision against
CPython. Verified: pyc C 190/0, LLVM 190/0 (189 + the new test),
all 16 ifa-test phases zero reblessing; trio/bh/pygasus PASS
trajectories byte-identical to the M3-landing baseline (modulo the
new PASS-line field); corpus sweep member set unchanged (the only
sweep-text delta was `othello3`, a never-compiling member whose
pre-existing parse-stage segfault flips between "crash" and
"syntax error" under load — reproduced identically on the
pre-change build, standalone and in-sweep).

**Reverted cleanly**: the rebase was done in a detached-HEAD
worktree (`git worktree add -d`), so the `issue033-stage-c` branch
itself was never touched — it's still at `2d514f58`, identical to
`origin/issue033-stage-c`. Nothing from this attempt is on main.
The rebase-conflict-resolution work itself (three conflicts: the
merged `equivalent_es_pnode` comment above, one other pure-comment
conflict, and one genuine field merge of the M4 probe alongside the
branch's `SplitDecision::sig` addition) was redone and preserved on
branch [`issue033-stage-c-rebased`](https://github.com/jplevyak/pyc/tree/issue033-stage-c-rebased)
(pushed to origin, built on current main at the time) so it doesn't
have to be redone from scratch — **confirmed to break the stall
guard on `bh`, do not merge.** Its only value is as a checkpoint:
current main plus `issue033-stage-c`'s unchanged payload,
conflicts pre-resolved, with the divergence already reproduced
against it once. Revive by root-causing the stall-guard interaction
first, not by starting from this branch and hoping.
*(2026-07-13: done exactly that way — root-caused (main's guard),
fixed, and the payload merged to main after the bar re-ran green
on a fresh rebase. `issue033-stage-c` and `issue033-stage-c-rebased`
are now historical; their payload is on main.)*

**M2, M3, and M4 are now all blocked or reverted** *(2026-07-13
update: M3 is LANDED — its blocker was a latent main stall-guard
hole, since fixed; see the correction above. M4's premise should
now be re-measured, per its section)*, each for a
different, concrete, well-evidenced reason: M2 (stage-2 batching)
hangs pygasus; M3 (ledger revival) breaks the stall guard on bh;
M4 (dirty-marked collection) is a no-op under the current
architecture regardless of correctness. The common thread across
all three: **every one of these regressions was invisible to the
standard suites + corpus-sweep-by-bucket check, and only surfaced
by running specific, real, at-scale corpus members to completion**
— fysphun and pygasus for M2, pygasus for M4, bh for M3. Any future
attempt at any of these should budget for that verification step
from the start, not add it after a "looks safe" first pass.

### M4. Dirty-marked collection (S4-A) — BLOCKED, confirmed by measurement, not implemented. **2026-07-13 re-measurement with M3 landed: STILL blocked** — M3's ledger persists contour *decisions*, not AVar flow state; `clear_results()` still resets every AVar each pass, and the landed-M3 pygasus run shows the same >99% plateau re-touch rate as before (e.g. 90303 dirty / 78654 examined at pass 11, vs pre-M3 main's 92199/80887). The actual prerequisite is M5-style reset-and-reseed (or any change that lets AVar state survive passes), not M3 as this section originally guessed.

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

**Status (2026-07-11): measured before writing any collector code,
and the measurement itself answers the question — M4 is genuinely
blocked on M3 (or M5), not merely easier with it.** Landed a small,
behavior-neutral probe (`AVar::dirty`, set in `propagate_out_change`
— the single shared exit point for every flow-update path, per the
existing comment there — and a `dirty_avar_count` /
`examined_avar_count` pair on `FA`, printed in the `-v` PASS line)
before touching any collector, specifically to avoid a repeat of
this issue's pattern: build the optimization, discover on a large
input that its premise doesn't hold. Verified behavior-neutral:
pyc C/LLVM 179/0, all 16 ifa-test phases, zero fixture changes.

The measurement: on pygasus's own documented plateau (S2 above,
passes 6-11, "~10 ess of progress per pass"), `dirty_avar_count`
is essentially FLAT across all six plateau passes — 91391, 91895,
92107, 92185, 92192, 92199 — a >99% RE-touch rate of a ~92K-AVar
universe every single pass, despite the structural contour count
(ess/css) barely moving. Fysphun shows the same shape at its own
scale (passes 2-15 hold at 85-90% of the examined universe dirty
throughout its own plateau). The only passes where the dirty count
drops to (near) zero are the terminal "confirm nothing left"
passes — fysphun's pass 18 (0 dirty / 5365 examined) and pygasus's
pass 20 (0 dirty / 74640 examined) — a real but narrow win (one
pass per compile, not the plateau this milestone targets).

**Root cause, and why this isn't fixable by writing dirty-aware
collectors alone**: `clear_avar` (called every pass via
`clear_results()`, whenever `analyze_again`) unconditionally resets
EVERY AVar's `in`/`out` to `bottom_type` and clears its
`backward`/`forward` edge sets completely. The next pass's
flow-to-fixpoint therefore has to re-derive the ENTIRE reachable
graph from scratch regardless of how little conceptually changed —
which is exactly why `propagate_out_change` (and thus `dirty`)
fires for nearly the whole universe every pass: from `clear_avar`'s
perspective, EVERY AVar's `out` "changed" this pass, because it
went from a forced bottom_type back up to its real value. Dirty-
tracking collectors would correctly report "everything is dirty,
scan it all" every plateau pass — functionally equivalent to
today's unconditional full scan, with the bookkeeping overhead of
maintaining `dirty` on top. This is the mechanism behind the S5
plan's own "With M3, most plateau passes re-derive into an
unchanged landscape" framing: M3's persistence (or M5's more
complete reset-and-reseed) is what would make "AVar state carries
over, unchanged, across passes" true in the first place. Until one
of those lands, "dirty since last extend" is nearly synonymous with
"exists," and building collectors around it buys nothing.

**Left the probe on main** (harmless, `-v`-only output, useful for
re-checking this exact question once M3 lands — if the ratio drops
sharply on M3's persisted state, that's the green light to proceed
with the actual collector rewrite this section describes). Did not
write any dirty-aware collector logic, since the measurement shows
it would be a no-op at best under the current architecture.

### M5. Reset-and-reseed rounds (the full shedskin shape; only if
M2-M4 leave a gap) — **CLOSED BY ITS OWN GATE (2026-07-14): the
re-measured evidence pointed away from the round structure
entirely; the plateau cost was stage-2/4 per-confluence redundancy,
now fixed. See the status block below.**

With decisions fully persistent (M3) and collection incremental
(M4), evaluate whether the remaining per-round cost justifies the
final step: rebuild the contour universe from the ledger at round
start (ESs/CSs constructed from keys; displays from the recorded
feasibility data) and make `clear_results` a true graph reset.
This buys shedskin's memory behavior and removes `Fun::ess`
survival as load-bearing state, at the cost of a reseeding pass.
Decide on M0-metrics evidence, not aesthetics — if pygasus-class
inputs converge in seconds after M4, stop there.

**Status (2026-07-14).** Followed the gate literally before
building anything: re-instrumented pygasus's dominant `mark_type`
cost (M0's own finding) with sub-phase timers. The answer was not
"round structure" at all:

1. **~14s of the 98s was diagnostic overhead**: `log()` is a
   variadic FUNCTION, so the `[stage2-marks]` per-closure-member
   diagnostics evaluated two `set_count()` walks per member per
   confluence even with logging off. Guarded with
   `if (logging(LOG_SPLITTING))`.
2. **The rest was per-confluence redundancy, the same disease
   B4/P3 fixed in stage 4's seeding**: `split_with_type_marks`
   rebuilt the full backward+forward transitive closure (47.7s)
   and re-ran the marked-confluence collect (36.6s) once PER
   CONFLUENCE — O(confluences x universe). Since backward- and
   forward-closure both distribute over union, the joint closure
   over all seeds is EXACTLY the union of the per-seed closures;
   marks are per-(AVar, CS) min distances, so joint seeding is the
   same-or-more-defined semantics stage 4's rework already adopted.
   Landed `build_joint_type_marks` + a stage-2 driver that builds
   one closure, collects once, and splits the marked set through
   M2b decide-then-apply. **mark_type: 84.8s -> 1.7s.**
3. That exposed stage 4's seeding as the next term (21s): its
   B4/P3 "joint" loop still called `build_type_marks` per
   confluence with a SHARED accumulator, re-scanning and re-marking
   the growing union per seed — and, it turned out, accidentally
   computing a RICHER closure (each call's backward pass re-expands
   from earlier seeds' forward sets, approximating an alternating
   bwd/fwd fixpoint whose depth depended on the seed COUNT — a
   seed-order/count dependence, 009/021 flavor). Replaced with one
   `build_joint_type_marks` call: both stages now use the same
   well-defined, seed-count-independent closure (bwd* then fwd* of
   the seed set). **mark_setter: 21s -> 0.08s.**

Net: **pygasus total FA 165s -> 62s** (was ~200s pre-M1, ~300s+
timeout pre-033). Extend is now 2.8s (3%) of the compile; the
remaining profile is **match-dominated (53.8s, 62%, 75% cache-hit)**
— S4-E MatchCache territory, nothing the round structure would
touch. Outcomes: pygasus 19 passes (was 20 — the accidental
richer-closure stage-4 splits on old pass 14 don't happen; 24 fewer
css) with the IDENTICAL 788-violation diagnosis and identical final
ess; fysphun 18 passes / 0 violations / 217 ess (one fewer ES than
before, equivalent convergence); kmeanspp/pylife/bh trajectories
byte-identical. pyc C 190/0, LLVM 190/0, all 16 ifa-test phases
with ZERO fixture reblessing (the synthetic fixtures' closures were
already whole-graph, so union semantics change nothing there);
sweep member set unchanged; deterministic across repeated runs.
(One verification run crashed in the `-v` type dump — the second
sighting of a pre-existing intermittent, now filed as
[041](041-verbose-type-dump-intermittent-segfault.md).)

**Verdict:** the round structure's premise — an extend plateau that
per-pass re-derivation makes unavoidable — no longer exists on any
known input. M5 stays closed unless a future input shows a
divergence or plateau that M3's routing + these collection fixes
don't already handle; the next real performance frontier is the
match phase (S4-C landed; S4-E subset-aware MatchCache is the open
item), which is outside this issue's scope. Side effect worth
noting for M2a's ledger: stage 2's collection is now cheap enough
that M2a's pygasus-hang premise ("stage-2 collection at full
universe scale every pass") may no longer hold — unattempted, and
there is currently no motivating input.

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
