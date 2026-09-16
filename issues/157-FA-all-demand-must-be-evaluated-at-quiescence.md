# ifa/157 — ALL demand must be evaluated at quiescence

**Status:** in-progress, opened 2026-09-15.

**Author's directive, 2026-09-15:** *"All demand should be done after
quiescence. All."*

This is an architecture issue, not a bug report. It says WHEN a demand test
may run, and today essentially none of them run at that time — either because
they are edge-triggered and have gone blind by then, or because they are
gated on a quiescence that never arrives.

## The rule

A demand is a property of the CONVERGED types: *this AVar holds a union that
something cannot proceed on*. It is therefore asked **once types have reached
a fixed point**, **level-triggered**, **re-asked every pass**.

The contrast is an *edge-triggered* test, which asks *did a writer just bring
something new?* That fires while the union is FORMING, once, and then goes
blind — because once the union has settled nothing is new, and the demand
that is by then plainly visible is never reported again.

Both forms observe the same union. Only one of them observes it at a time
when the analysis can act on it.

## The measurement

**1. Stage 1's detector is edge-triggered.** `collect_type_confluence`
(`ifa/analysis/fa.cc:6060`-ish) flags an AVar only on the pass some writer
contributes a type it does not already have:

```c
if (x->out->type->n && type_diff(av->in->type, x->out->type) != bottom_type) {
  confluences.set_add(av); trigger = x; break;
}
```

Counting `IFA_DBG_CONFLUENCE` reports on `softrender`, restricted to formals
whose type is exactly `{int64, float64}` — the mix that whole investigation
was about:

| | count |
| --- | --- |
| `added=0` (the mix is present, nothing flagged) | **1184** |
| `added=1` (flagged) | 47 |

**96% of the time the union is sitting right there and no confluence is
raised.**

**2. Quiescence, which stages 6+ are gated on, is essentially never reached.**
Measured with `PYC_DBG_QUIESCE` (added at the `PER_CS_RECEIVER` gate in
`run_split_stages`, `fa.cc:12324`):

| program | passes reaching quiescence |
| --- | --- |
| `softrender` | **3 of 56** |
| `fysphun` | **3 of 18** |
| `sudoku4` | **0 of 19** |

Stage counts agree: `TYPE_CONFLUENCE` fires on nearly every pass (56 on
`softrender`, 19 on `sudoku4`), while `PER_CS_RECEIVER` and `CSM_ELEMENT_CS`
fire **never**.

**3. These two facts are the same fact.** An edge-triggered stage 1 keeps
finding transient work every pass, so `analyze_again` is never 0, so no stage
that waits for quiescence ever runs. The ordering is right on paper and
inoperative in practice.

## Why this is upstream of several open issues

- **ifa/156 (int/float)** — a formal holding `{int64, float64}` is invisible
  to an edge-triggered detector after the pass it formed on. No new demand
  SOURCE is needed for it; a different TIME is.
- **ifa/146's ESBLOCK trade** — `sudoku4` vs `softrender`, where the lever
  moves the failure rather than removing it. Both arms are demands being
  evaluated mid-formation.
- **ifa/148's stage-1 starvation** — "stage 1 claims ~35 of 41 passes and
  starves everything below it" is this issue's symptom stated per-stage.
- **ifa/045 / ifa/075** — `PER_CS_RECEIVER` and `CSM_ELEMENT_CS` are not
  disabled; they are unreachable.

## The one place the target architecture already exists

`split_css_by_defs(int quiescent)` — route 4, `CS_DEF_PARTITION` — takes
quiescence as a **parameter** rather than being gated out by it. It runs
every pass and *tightens* its criterion once quiescence is reached. That is
why ifa/143's level-triggered `csdemand` works at all, and it is the shape
every other stage should have:

> a stage is not "allowed to run only at quiescence"; it is TOLD whether the
> types it is looking at are converged, and asks a correspondingly strong or
> weak question.

## FIRST ATTEMPT, measured dead: level-triggering the detector

**2026-09-15.** The obvious repair — ask the LEVEL question alongside the
edge one, *does this AVar HOLD an irrepresentable union?*, on the converged
type — was built (`PYC_CONFLEVEL`) and measured. Three formulations:

| | predicate |
| --- | --- |
| `=1` | irrepresentable, excluding a pure numeric mix |
| `=2` | the same, INCLUDING one (ifa/156's int-vs-float case) |
| `=3` | irrepresentable AND something STUCK on it — a recorded type violation or an unresolved dispatch argument (`confluence_is_demanded`) |

**All three are dead.**

| `softrender` | confluences (p=1) | stage-1 `d_ess` (p=1) | warnings | quiescent |
| --- | --- | --- | --- | --- |
| off | 923 | 347 | 13 | 3 of 56 |
| `=2` | 3295 | **348** | 13 | 3 of 55 |
| `=3` | +20599 nominated | unchanged | 13 | 3 of 55 |

**3.6× the candidates and ONE more split.** And on `sudoku4` it is worse than
inert — warnings 39 → **206** (`=2`) / **54** (`=3`) — which is
[146](146-remove-all-arbitrary-splitting.md)'s non-monotone diagnostic. Per
CLAUDE.md the lever was DELETED rather than defaulted off. What survives is a
probe (`IFA_DBG_CONFLEVEL`), which counts and changes nothing.

### Why it cannot work — two measurements

**1. 83–87% of confluences never reach a partitioner.** Stage 1's only
actuator is *split an EntrySet on a FORMAL* (or on a return value). A union
observed on a non-formal rvalue — a local, a temporary — is counted and
dropped at `tc_skip_rval`. `IFA_DBG_INCOMPAT` on `softrender` p=1:

```
off  stage1 seen=923  skip(rval=769 lval=0 cs=64)  dec=90  split(formal=66)
=2   stage1 seen=3295 skip(rval=2861 lval=0 cs=280) dec=93 split(formal=67)
```

2372 extra candidates moved `dec` from 90 to 93. `skip(lval)` is **0** on
every pass of every program measured: a return value is never a confluence.

**2. In the rest there is nothing to partition** — every in-edge already
carries the union, so `etype == stype` and
`edge_type_compatible_with_entry_set` sees no disagreement. That is ifa/146's
self-blinding, and it is stop condition 1 below, hit exactly as written.

### And the framing was wrong: types were never stale

`analyze_to_convergence` drains the edge, send and ES worklists **to empty**
before calling `extend_analysis` → `run_split_stages`. So the types every
split stage reads are ALREADY at a fixed point, on every pass, today. The
edge-triggered detector is not reading stale types; it is reading converged
types and asking a question about their *history*.

**Which means the `!analyze_again` gate is not a quiescence gate.** It is
named one throughout `fa.cc` — "runs only on quiescence of all stages above",
"only on full quiescence of stages 1-5" — and what it actually tests is *did
a higher-priority stage act this pass*. Types are converged on both sides of
it. It is a CASCADE PRIORITY gate wearing a convergence gate's name, and it
starves every lower stage for as long as a higher one has work, which
([148](148-stage-5-starvation-root-caused.md)) is every pass, because a
separation propagates exactly one contour per pass.

So `PYC_DBG_QUIESCE`'s 3-of-56 and 0-of-19 are real but they do not mean what
the name says. They measure how often stage 1 ran out of work, not how often
the analysis converged — it converged every time.

## The fix, restated

The directive stands and is already satisfied in the one sense that was in
doubt: **demand IS evaluated on converged types.** The defect is the cascade.

1. **Stop calling it quiescence.** Rename the gate and its comments to what
   they test. The misnomer is load-bearing: it is why "stages 6+ are starved"
   read as a convergence problem for two issues running.

2. **Give every stage the route-4 shape.** `split_css_by_defs(int quiescent)`
   takes the condition as a PARAMETER and runs every pass, asking a weaker
   question early and a stronger one late. `PER_CS_RECEIVER` and
   `CSM_ELEMENT_CS` should do the same, so a stage with a genuine demand can
   act instead of never running — 0 firings across every program measured.

   **Not by lifting the gate.** That was `PYC_RECVFAN=2`, removed 2026-09-07,
   and its note states the rule: *"the answer has to be a reason this stage
   may act, not permission to act without one."* Each stage needs its demand
   test written before it gets the parameter.

3. **Give stage 1 an actuator for a non-formal rvalue.** This is the finding
   with the largest measured headroom — 769 of 923 confluences per pass on
   `softrender`, 438 of 480 on `sudoku4` — and it is not a splitting policy
   question, it is a missing mechanism. The natural shape is
   [152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md)'s
   backtrack applied on the EntrySet side: a union observed on a local is a
   demand *somewhere*, and the place to act is the formal or creation point
   upstream that supplies it. 152 established that the demand is observed
   where the union is USED and almost never where the merge HAPPENED; stage 1
   still only knows how to act at the point of observation.

   Confirm the headroom before building it: of the skipped rvalues, how many
   backtrack to a formal whose in-edges actually DISAGREE? If the answer is
   near zero, this is the same self-blinding again and the work is in the
   partitioner instead.

## Stop conditions, written before measuring

- ~~If level-triggered detection finds a demand at quiescence but **nothing to
  partition**, the detector is not the blocker and the issue moves to the
  partitioner.~~ **HIT, 2026-09-15.** 3.6× candidates, one extra split.
- ~~If making stage 1 level-triggered *increases* contour counts corpus-wide,
  it is failing ifa/146's non-monotone diagnostic.~~ **HIT.** `sudoku4`
  39 → 206 warnings. Lever deleted, negative result recorded above.
- If quiescence is reached but stages 6+ then fire on every pass forever, the
  gate was load-bearing for termination and the stages themselves need demand
  tests before they need permission.
- For step 3: if the skipped rvalues backtrack to formals whose in-edges
  already agree, stop — the actuator is not the limit either.

## Verification plan

- six CI gates (CLAUDE.md "Change acceptance")
- `IFA_DBG_CONFLEVEL` — of the AVars whose CONVERGED type is an
  irrepresentable union, how many does the edge-triggered test flag?
  Baseline: `softrender` flagged=11890 unflagged=**34878** (75% blind),
  `sudoku4` flagged=1327 unflagged=**12662** (90% blind). This generalizes
  ifa/156's 47-vs-1184 from numeric formals to every irrepresentable union —
  and the attempt above shows closing it is not sufficient.
- `IFA_DBG_INCOMPAT` — `seen` / `skip(rval)` / `dec` / `split(formal)`.
  Baseline `softrender` p=1: 923 / 769 / 90 / 66.
- corpus `-m check` A/B against `main`, both arms freshly built
  (`sweep-measures-the-binary-not-the-tree`)

## See also

- [156](156-FA-split-int-from-float-coerce-last.md) — where this was found;
  its "CORRECTION" section is the origin of this issue
- [146](146-remove-all-arbitrary-splitting.md) — the non-monotone diagnostic
  this must pass
- [148](148-stage-5-starvation-root-caused.md) — stage 1 starving what is
  below it, stated per-stage
- [143](143-shared-container-method-contours-refuse-cs-splits.md) — the
  level-triggered `csdemand` that already works
- [133](133-split-a-container-on-its-element-type.md) — `PYC_CONFDEMAND`, the
  probe whose predicate step 1 reuses
