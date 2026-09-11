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

## Part 3 — CORRECTED 2026-09-11: the damage is the FIX, not stage 5

The first version of this section said "removing the false progress lets
stage 5 run, and stage 5's action is what regresses things". That is
wrong, and the correction matters more than the original claim.

`PYC_NOOPSPLIT=1` costs 4 suite tests (`deepcopy_copy_of_copy_chain`,
`format_string_int_float_mismatch`, `set_ops_chained_mixed_elem_types`,
`tuple_compare`). On `tuple_compare` stage 5 DOES run — and splits
nothing:

```
[stage5] p=7 analyze_again=0 -> RUNS (violations=7)
[stage5] p=9 analyze_again=0 -> RUNS (violations=7)
```

no `[stage5in]` lines at all, so `refinable` is empty. Stage 5 is not the
cause. The cause is the fix:

```
                  passes   result
  default            14    0 warnings
  PYC_NOOPSPLIT=1    10    4 warnings  (illegal ... 'x' illegal: tuple, in <tuple_cmp>)
```

**The premise "nothing changed" was too strong.** A re-derived split
creates no EntrySet, but it DOES re-point edges onto the recorded
products, and the types downstream of that re-park have not re-flowed yet.
Reporting no progress ends the pass loop four passes early, before the
re-park settles. Both runs print `CONVERGED=1`, which means only "did not
hit the pass limit" — not "reached a fixed point".

So the correct condition is not "created no EntrySet" but **"the edge ->
EntrySet assignment is identical to the previous pass's"**, which is a
durable per-edge comparison, not a counter. `PYC_NOOPSPLIT` as written is
the right diagnosis with the wrong test, and stays opt-in.

## Part 4 — stage 5's action IS a per-CS fan, and fixing that is not enough

Separately, and confirmed: `split_edges` — the `fdynamic` path stage 5
uses — builds one filtered EntrySet per CreationSet in the receiver's
type:

```c
for (CreationSet *cs : av->out->type->sorted) {
  filters.put(p, make_AType(cs));
  EntrySet *tes = find_or_make_filtered_entry_set(es, filters);
```

Partition size = the CreationSet count: ifa/144's signature, and exactly
what ifa/146 E deleted from CARTESIAN_PRODUCT. It survived here because
stage 5 is starved and nobody looked. `PYC_SPLITEDGES2=1` bounds it to two
groups, as ifa/133's ES-block split and ifa/146 E both do.

It works as a bound (`sudoku5`: `d_ess=5` -> `d_ess=2`) and it changes
nothing else: the violation transient after the split is identical (1216
either way), and the same 4 suite tests fail. So the fan is real but is
not what makes stage 5 expensive.

Also tested and EXONERATED: `find_or_make_filtered_entry_set` reuses a
contour whose filters are merely NOT DISJOINT rather than matching, which
looked like it would merge the outer and inner invocations of a recursive
`<tuple_cmp>`. `PYC_FILTEREQ=1` requires them to match and changes nothing
(4 warnings either way).

## What to do

1. **Make the no-op test durable.** Compare the edge -> EntrySet
   assignment against the previous pass instead of counting EntrySets.
   That is the fix Part 1 deserves, and it is what would let the pass loop
   stop for the right reason.
2. **`CONVERGED=1` is not a fixed-point claim.** It reports only that the
   pass limit was not hit. A program can "converge" four passes before its
   types settle, and `tuple_compare` does.
3. The per-CS fan in `split_edges` (Part 4) should land whatever happens to
   the rest — it is the same arbitrary partition the project has removed
   twice elsewhere — but it needs its own measurement, since on the
   evidence here it is behaviour-neutral.
