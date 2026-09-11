# ifa/148 — stage 5 is starved, and the starvation is load-bearing

**Root-caused 2026-09-11** on `sudoku5` at the `PYC_CSDCPA1=2` arm, after
both ifa/133 and ifa/146 E had to route around it.

The premise: a stage blocks stage 5 by reporting progress, and progress
must be finite if the stage works. Measured, it is *almost* finite, and
part of it is not progress at all.

## Part 1 — RETRACTED: those applies are not no-ops

The original Part 1 said 44% of applies "report a split they did not
make" — 501 of 1145 create no EntrySet, so the claim was that the ledger
routes the edges to an already-recorded product and nothing changes.

**Measured, and false.** `IFA_DBG_REPARK` snapshots every in-edge's target
before an apply and compares after:

```
sudoku5:        applies with NO new EntrySet = 501, of which moved >=1 edge = 501
                total edges moved = 1512
tuple_compare:  41 of 41, likewise
```

**Every single one re-points edges.** Creating no EntrySet means the split
REUSED existing contours, not that it did nothing. `d_ess == 0` is simply
the wrong test for "nothing changed", and `analyze_again` is correct on
those applies.

`PYC_NOOPSPLIT` — which returned 0 from them — is deleted. It was a wrong
mechanism built on this wrong premise, and it truncated the analysis
(Part 5).

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

### Why the types move with no new contours — no decision is lost

Nothing fails to persist. Two facts, both measured:

1. **The splitter runs AFTER the analysis in each pass.** A structural
   change made in pass N is therefore only reflected in pass N+1's derived
   types — a one-pass lag by construction. After the last change you still
   need one pass to observe it and one more to confirm nothing moved.
2. **An apply that creates no contour still re-points edges** (Part 1:
   501 of 501 on sudoku5, 1512 edges). It is a genuine change to the call
   graph, so it genuinely needs another pass.

`analyze_to_convergence` DOES iterate its worklist to exhaustion, so the
dataflow reaches a fixed point within a pass. What does not converge in one
pass is the STRUCTURE, because only the splitter changes it and it runs
once per pass.

So `analyze_again` is right to be set by a structural change. What is
missing is the converse: nothing tests whether the state has stopped
moving, so the loop cannot tell "no stage acted, and we are done" from "no
stage acted, but last pass's change has not been observed yet".

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

## Part 6 — the right test already existed, unused (`PYC_ROUTESTABLE`, now default)

Prompted by the author's question: *"no pass should need to split, since
all decisions are stored in the table, right?"*

Half right, and the half that is right was already implemented.

### The decisions ARE in the table, and they ARE re-derived

`IFA_DBG_INCOMPAT` on `tuple_compare`:

```
LEDGER p=2 dup_es=6   p=4 dup_es=7   p=5 dup_es=14   p=6 dup_es=14   p=10 dup_es=1
REDERIVE p=10 ROUTE fun=__getitem__ es=210 -> product=118 first_pass=0
```

A decision first recorded at **pass 0** is still being applied at **pass
10**, and on the late passes every split is a recorded decision, not a new
one (`p=10: dec=2 split=1`).

### But "no pass should split" does not follow

A `SplitDecision` is keyed on `(fun, stage, position, partition AType)`. It
can only fire once a contour of that fun actually CARRIES that partition at
that position — which requires the types to have propagated that far. New
contours keep appearing (from other splits) and keep matching old
decisions, so the replay is spread across passes by construction. That is
the `first_pass=0` applied at `p=10` line. Finite — 13 passes, activity
decaying — but not collapsible into one pass.

So two things must be told apart, and my deleted `PYC_NOOPSPLIT`
conflated them:

| | new information? |
| --- | --- |
| applying a recorded decision to a NEWLY-APPEARED contour | **yes** — real work, needs the pass |
| re-routing an UNCHANGED group to the SAME product as last pass | **no** |

### The second test already existed and was never switched on

`SplitDecision::last_route_pass` / `last_route_product`, `stable_route`,
and `PYC_ROUTESTABLE` — added with a comment stating this exact
starvation:

> *"With the full per-pass reset the edges are rebuilt every pass, so a
> stable group is re-derived and re-routed to the same home for ever —
> correct behaviour, but it was being reported as progress, which keeps
> analyze_again true and starves every later stage. Repeating last pass's
> routing is not new information."*

Defaulted to 0 and, as far as this issue can tell, never measured.
Measured now, one binary, env toggled:

| | suite | corpus | sudoku5 |
| --- | --- | --- | --- |
| off | 315 / 0 | 2 cfail, 43 warns, 2740 CS | 39 starved |
| on | 315 / 0 | **identical** — 2 cfail, 43 warns, 2740 CS | 37 starved |

Free. **Defaulted ON.** It removes the part of the starvation that is
genuinely false progress, without touching the part that is real work.

### What is left

The remaining starvation is Part 2: stage 1 doing real, slow, oscillating
work on ~35 of 41 passes. That is not false progress and cannot be
suppressed; it is answered either by making stage 1 converge faster or by
not using first-stage-wins to schedule stage 5. Both ifa/133's route 4 and
ifa/146 E's receiver split already sidestep it by running unconditionally.

## Part 7 — split the whole ES path at once (`PYC_ESPATH`): right idea, wrong selector

The author's proposal, and it targets Part 2 directly. A filtered contour
narrows its formal; for that narrowing to reach the next callee, THAT
callee needs its own filtered product — which only the splitter makes, and
it runs once per pass. So a separation propagates **one contour per pass**,
which is why stage 1 claims ~35 of 41 passes on `sudoku5` doing real work.

Taking the whole path at once is a SCHEDULING change, not a policy one:
every contour it adds is one the existing rung would have split later
anyway, by the same type-shaped test, and `decide_entry_set_split` still
decides each partition. Only the timing changes.

`PYC_ESPATH=1` walks backward from each confluence and offers the formals
feeding it as imprecisions in the same pass.

### On a small program it is exactly what was hoped for

`tests/tuple_compare.py`:

| | passes | new EntrySets (total) | applies | warnings |
| --- | --- | --- | --- | --- |
| default | 13 | 343 | 184 | 0 |
| `PYC_ESPATH=1` | **10** | **336** | **175** | 0 |

Fewer passes AND slightly fewer splits, same answer. Pure scheduling, as
predicted.

### On a large program the selector fans

`sudoku5` at the flag arm: passes 39 -> 20 (the intended halving), but
ess 631 -> 719, css 1478 -> 1903, and it stops compiling. Corpus at the
DEFAULT arm: `compile_fail` 2 -> 5 (+ plcfrs, softrender, sudoku5), while
the suite is **315/0 either way** — so the suite does not see this at all.

Two selectors were tried and both admit the same set:

- *"the formal shares a CreationSet with the confluence"* — too weak by
  construction: on a program where one big union is everywhere, every
  upstream formal overlaps it.
- *"the formal's union is a SUBSET of the confluence's"* — identical
  numbers on `sudoku5` (ess=719, css=1903), because the confluence union
  is itself large, so most of the program is a subset of it.

### What is actually missing

Both selectors are type tests against the confluence, and when the
confluence is a big union no type test can distinguish "on the path to
THIS merge" from "upstream and overlapping". The path is a REACHABILITY
property — which contours the conflicting values actually flowed
through — and it wants the same treatment ifa/133 gave the CreationSet
backflow: walk the specific edges the conflict came in on, not every
formal whose type intersects.

Kept opt-in with the measurements above. The prototype establishes that
the scheduling win is real (13 -> 10 passes, fewer splits, same answer)
and isolates the open problem to selecting the path.
