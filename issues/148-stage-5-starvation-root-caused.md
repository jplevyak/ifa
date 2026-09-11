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

## Part 5 — why eliminating the wasted passes made things WORSE

Because they are not wasted. `tests/tuple_compare.py`, default arm:

```
p=7  confl=41 d_ess=0 viol=7
p=8  confl=40 d_ess=1 viol=7
p=9  confl=40 d_ess=0 viol=7
p=10 confl=44 d_ess=4 viol=7     <- FOUR new EntrySets
p=11 confl=36 d_ess=2 viol=0     <- two more, and violations reach ZERO
p=12 confl=34 d_ess=0 viol=0
```

`PYC_NOOPSPLIT=1` stops at p=9. The passes it removes, p=10 and p=11, are
the ones that finish the job. And note `confl` moves **40 -> 44 across a
pass that split nothing**: the derived types are still changing with no
structural change at all.

That is the real defect, and it is one line of concept:

> **`analyze_again` means two different things — "a stage split something"
> and "run another pass" — and they are not the same condition.**

A pass does NOT reach a type fixed point. `analyze_to_convergence` resets
and re-derives, but the DECISIONS persist (`av->cs_map`, the split
ledger), so successive passes derive different types even when nothing
splits. The loop needs to run while the state is moving; what it actually
tests is whether a stage split. The false-progress claim of Part 1 was
accidentally standing in for the missing condition, which is why removing
it truncated the analysis.

### The fix: terminate on a fixed point, not on a split

`PYC_TYPEMOVE=1` hashes every live contour's positional formal types at
the end of the pass and sets `analyze_again` when the hash differs from
the previous pass's. ATypes are hash-consed for the life of the FA, so
pointer identity is a sound cross-pass comparison — the same property
`es->type_key` already relies on.

```
                                   passes  warns
  default                            14      0
  PYC_NOOPSPLIT=1                    10      4
  PYC_NOOPSPLIT=1 PYC_TYPEMOVE=1     15      0
```

and it stops for the right reason —

```
[typemove] p=11 moved=1 aa=1
[typemove] p=12 moved=1 aa=0     <- stages found nothing; the STATE had moved
[typemove] p=13 moved=0 aa=0     <- stop
```

Whole suite with both: **313 passed / 2 failed**, against 311/4 for
`NOOPSPLIT` alone — so the correct termination test recovers half the
damage and the remaining two (`container_scalar_union_add`,
`set_ops_chained_mixed_elem_types`) are a smaller, separate question. On
`sudoku5` stage 5's opportunities double, RUNS 2 -> 4.

Not landable yet at 2 suite tests, but the structural finding stands on
its own: **the pass loop has never had a termination condition, only a
splitting condition.**

## What to do

1. **Separate the two meanings of `analyze_again`** (Part 5). The loop
   should run while the abstract state moves; a stage splitting is one
   reason the state moves, not the definition of it. `PYC_TYPEMOVE=1` is
   the prototype and it costs 2 suite tests; finishing it is the
   highest-value item here, because it is a correctness property of the
   whole analysis rather than a splitter detail.
2. **`CONVERGED=1` is not a fixed-point claim.** It reports only that the
   pass limit was not hit. A program can "converge" four passes before its
   types settle, and `tuple_compare` does. Any conclusion in this repo
   resting on that flag should be re-read.
3. The per-CS fan in `split_edges` (Part 4) should land whatever happens to
   the rest — it is the same arbitrary partition the project has removed
   twice elsewhere — but it needs its own measurement, since on the
   evidence here it is behaviour-neutral.
