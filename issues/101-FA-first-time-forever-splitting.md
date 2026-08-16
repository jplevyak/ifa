# 101 — the residual non-convergence is *first-time-forever* splitting, not re-derivation

**Status:** open, characterized 2026-08-16 after
[074](074-FA-cross-pass-oscillation-plan.md)'s two fixes
([066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md)'s
durable setter type and `PYC_SELFPROD=6`) cleared the reproducer. This
issue names what is left on the corpus, and why neither of those fixes
touched it.

## The set is three programs, not four

Of 77 shedskin programs, 8 do not produce a converged result. Five of
those are not FA problems at all:

| program | what it actually is |
|---|---|
| `minilight`, `quameon`, `tarsalzp` | multi-module programs pyc cannot load — `cannot find module 'ml'` / `'wave_func'` / `'com.github.tarsa…'`, failing in 0.3 s. Never reach FA. |
| `othello3` | 380 s and no `OSC` line — a separate mode, not yet characterized. |
| **`sudoku5`** | **converges at pass 47** once `IFA_STALL_LIMIT` / `IFA_NONIMPROVE_LIMIT` are raised, ending at its own minimum of 26 violations. Its apparent non-convergence is purely the *stall guard* cutting it off at pass 40. |

So the genuine set is **`go`, `linalg`, `plcfrs`** — which is exactly the
trio the 2026-08-13 census found, independently reproduced here.

## Shape: contours grow steeply, violations do not fall

Measured with the guards raised so the analysis runs to the pass cap:

| | ess growth | violations: first → min → last |
|---|---|---|
| `go` | 114 → 675 (**×5.9**) | 135 → **46** @p9 → 107 |
| `linalg` | 124 → 1195 (**×9.6**) | 182 → **21** @p26 → 100 |
| `plcfrs` | 144 → 1346 (**×9.3**) | 231 → **74** @p89 → 85 |
| `sudoku5` (converges) | 130 → 486 (×3.7) | 186 → **26** @p44 → 26 |

Two things stand out:

1. **`go` and `linalg` end far worse than their own minimum** — `go`
   bottoms at 46 violations on pass 9 and finishes at 107; `linalg`
   bottoms at 21 on pass 26 and finishes at 100. Ninety more passes and
   five to ten times the contours buy a *worse* answer. `plcfrs` is the
   benign one: it ends at 85 against a minimum of 74, still improving.
2. **Violations are wildly non-monotone.** `go`: 48 @p30 → 163 @p40 →
   240 @p50. `plcfrs`: 200 @p30 → **1311** @p40 → 481 @p50. `linalg`
   spikes to 465 at p60 from 27 at p50. This is not a fixpoint being
   approached slowly; it is thrashing.

## Why the 074 fixes do not apply: the splits are nearly all first-time

The decisive measurement. `REDERIVE-GROUP` counts a split whose group
signature the ledger had already recorded — i.e. work being redone —
against the total number of split decisions:

| program | split decisions | re-derived | **first-time** |
|---|---|---|---|
| `linalg` | 2128 | 101 | **95.3 %** |
| `go` | 655 | 21 | **96.8 %** |
| `plcfrs` | 4205 | 82 | **98.0 %** |
| `sudoku5` | 1285 | 10 | **99.2 %** |

The reproducer that 066 and `PYC_SELFPROD=6` fixed was the *opposite*
case: one signature, recorded once at pass 11, re-derived every pass for
ninety passes. The ledger could recognise it, and teaching it to do so
ended the oscillation.

Here there is nothing to recognise. **The splitter is inventing a
genuinely new partition almost every time it acts.** No amount of
ledger/routing/canonicalization work helps, because those mechanisms all
key on a repeat that never happens. That is why `PYC_SELFPROD=6` measured
as *exactly* inert across all 77 programs: it fires only on self-product
contours, and those are a rounding error here (`linalg`: 101 stale + 106
valid against 2128 decisions).

Stage attribution over the last 20 passes confirms where it comes from —
`TYPE_CONFLUENCE` dominates everywhere, with a high mint fraction:

```
go       TYPE_CONFL det=152 mint= 37 reuse=115   SETTER_OF_SETTER det=21 mint=20 reuse=1
linalg   TYPE_CONFL det=639 mint=276 reuse=363   SETTER csmint=39
plcfrs   TYPE_CONFL det=297 mint=120 reuse=177
sudoku5  TYPE_CONFL det=855 mint=101 reuse=754      <-- converges; 12% mint
```

The mint:reuse ratio tracks the outcome. `sudoku5` mints 12 % of its
redispatches and converges; `linalg` mints 43 % and grows ×9.6. Note also
`go`'s `SETTER_OF_SETTER`: 20 mints against 1 reuse — a stage that
essentially never reuses a contour.

## Fix direction

This is the shape [057](057-FA-nonconvergence-monomorphization.md)
names, and the author's recorded correction there applies: the answer is
**monomorphization plus a productivity invariant**, not widening and not
a bigger `CPA_LIMIT`. Concretely, what is missing is a rule that makes
splitting *earn* its contours — a split that does not reduce violations
(or does not reduce some other measure of imprecision) should not be
retained. Today nothing connects the two: `go` and `linalg` are allowed
to quintuple and decuple their contour counts while their violation
counts get worse.

The per-Var `violation_split_attempts` cap (issue 033 D6) is the existing
instance of that idea, but it is scoped to stage 5
(`split_for_violations`). `TYPE_CONFLUENCE` — which is where essentially
all of this growth comes from — has no equivalent.

## Verification plan

- `go`, `linalg`, `plcfrs` reach a fixed point with guards raised
  (`pass_limit_hit=0`), or terminate with a violation count at or below
  their measured minimum (46 / 21 / 74).
- `sudoku5` must keep converging, and its 47 passes should not grow.
- Full corpus A/B: no exit-code changes across the 77 programs
  (68 rc=0, 8 rc=1, 1 rc=134 as of 2026-08-16).

## Measurement note — a trap that cost a cycle here

`fa->type_violations` is cleared in `initialize_pass()` and populated by
`collect_var_type_violations()` **inside `extend_analysis()`**, which
runs *after* `complete_pass()`. A probe that samples it from
`complete_pass` therefore reports 0 or a stale tally, not that pass's
violations — the first version of the per-pass trace here read `viol=0`
for `go` on every pass while the `OSC` line for the same run said 104.
The `VIOL` line is now emitted at the collection site, which is the only
point in the pass where the count means anything.

The same off-by-one already applies by design to the `STAGE` line's
stage counters (they are incremented by `extend_analysis`, so the line
printed at pass N reports pass N-1's splitting); that one is documented
at `initialize_pass`.

## What this unblocks

The last three non-convergent corpus programs, and with them the ability
to treat `pass_limit_hit` as a genuine red flag rather than a mixed
signal. It also gates raising the stall guard: `sudoku5` shows the guard
is currently *masking* convergence that would happen a few passes later.
