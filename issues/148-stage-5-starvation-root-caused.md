# ifa/148 — stage 5 is starved, and the starvation is load-bearing

**Root-caused 2026-09-11** on `sudoku5` at the `PYC_CSDCPA1=2` arm, after
both ifa/133 and ifa/146 E had to route around it.

The premise: a stage blocks stage 5 by reporting progress, and progress
must be finite if the stage works. Measured, it is *almost* finite, and
part of it is not progress at all.

## Part 1 — 44% of applies report a split they did not make

`apply_entry_set_split` returns "split" while creating **no EntrySet**:

```
[rederive] TOTAL applies-reporting-split=1145 of which created NO EntrySet=501
```

Those are RE-DERIVATIONS. Contours are rebuilt every pass, so the split
made on an earlier pass is decided again; the ledger routes each group to
the product it already recorded (`d->product`), and the edges land exactly
where they landed last pass. Nothing changed — but `analyze_again` is set,
which keeps the first-stage-wins cascade alive and starves everything
below.

On `sudoku5` that is **6 passes of outright false progress**:

```
                       passes  claimed-with-zero-new-ES
  PYC_NOOPSPLIT=0        41              6
  PYC_NOOPSPLIT=1        42              0
```

`PYC_NOOPSPLIT=1` makes an apply that creates no EntrySet report no
progress. It eliminates the false claim exactly. The re-park itself still
happens; only the CLAIM changes.

The hazard was already named at the probe site, and never checked:
*"ifa/issues/055: a stage that returns 'made progress' while the contour
counts do not move keeps the whole cascade alive and starves every later
stage. Report the claim next to the effect."*

## Part 2 — the other ~35 passes are real, slow, oscillating progress

TYPE_CONFLUENCE claims 41 of 41 passes. On the passes it is not lying it
creates 30-80 EntrySets, and the confluence count does not fall
monotonically:

```
pass  595 507 468 462 444 ... 510 509 492 472 481 472 464 459 458 493 493 ... 287 286 286
d_ess 130  62  37  39  32 ...   5   2   2   1   4   2   0   0   0   0   0 ...   0   0   0
```

It converges (41 passes, `CONVERGED=1`), but it oscillates on the way and
claims nearly every pass. First-stage-wins then leaves stage 5 with **2
passes out of 41** — `analyze_again=1 -> starved` on the other 39, with
573-750 violations never acted on.

So the cascade is finite. It is just so nearly-always-busy at stage 1 that
stage 5 is effectively unreachable, which is why both ifa/133 (route 4)
and ifa/146 E had to be wired to run unconditionally instead.

## Part 3 — and removing the false progress makes things WORSE

This is the part that matters for anyone tempted to fix Part 1 and stop.

With `PYC_NOOPSPLIT=1`, stage 1 does fall silent (p=33: `TYPE_CONFL
returned=0`, `SETTER returned=0`) and **stage 5 finally runs**. What it
then does:

```
STAGEDELTA p=33 VIOLATION returned=1 d_ess=5 viol=108
STAGEDELTA p=34 TYPE_CONFL confluences=1039 d_ess=75 viol=1216
```

Five EntrySet splits take violations from **108 to 1216** and confluences
from 361 to 1039. Whole-run cost at the default arm:

| | suite | corpus |
| --- | --- | --- |
| default | 315 passed / 0 failed | 2 cfail |
| `PYC_NOOPSPLIT=1` | **311 / 4** | **3 cfail** (+ sudoku5) |

**The starvation is load-bearing.** It is not protecting a correct stage 5
from a scheduling accident; it is hiding a stage 5 whose ACTION is wrong.
That also explains the long-standing note that lifting the quiescence gate
"costs 16 suite tests" — the gate was never the problem.

## What to do

1. **Fix stage 5's action, not the gate.** The next question is why
   `split_ess_for_type(refinable, SPLIT_DYNAMIC)` on a violation's
   imprecisions multiplies violations tenfold. Until that is answered,
   un-starving stage 5 is a regression by construction.
2. **Keep `PYC_NOOPSPLIT` opt-in.** It is a correct fix to a real defect
   (Part 1) whose benefit is blocked by a second defect (Part 3). Landing
   it alone trades a measured 4 suite tests and a corpus program for a
   scheduling nicety.
3. Note that ifa/133's route 4 and ifa/146 E's receiver split both already
   run unconditionally precisely because of this. Neither needs the gate
   lifted; they are the pattern to follow while stage 5 is broken.
