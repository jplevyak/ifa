# ifa/156 — split int from float where possible; coerce only as a last resort

**Status:** open. Directive + a measured account of why the current ladder
cannot carry it. No lever landed: the obvious one was tried and is a no-op.

## The directive

**Author, 2026-09-15: split `int` from `float` where possible, and coerce only
as a last resort.**

This is a CORRECTNESS argument, not a precision one, and it inverts today's
priority. Numeric coercion widens the int, so pyc prints `1.0` where CPython
prints `1` — CLAUDE.md names that deviation explicitly, and
[145](145-numeric-coercion-is-not-gated-on-permissive-mode.md) makes coercion
a permissive-only device that `--strict` must refuse. **Splitting keeps the
int an int**, so it is the answer that matches CPython; coercion is the lossy
fallback taken when no split can separate the paths.

## What is already right

**The pass ordering.** `promote_first=2` (the default) runs
structural repair → **the splitter** → reanalyze phase 2, and coercion lives
in phase 2 (`do_coerce = ifa_reanalyze_phase != 1`). So the splitter already
gets first refusal on every pass; coercion only annotates what is left. The
ordering does not need changing.

## What is wrong — the mix is never OFFERED to the splitter

`elem_irrepresentable` ends:

```c
  return nb > 1 && !all_num;
```

`!all_num` excludes a pure numeric mix from being a demand at all, and the
comment says why: *"a pure-numeric mix — `{int64, float64}` is resolved by
`coerce_annotate`, so flagging it would split what coercion fixes."* So
`{int64, float64}` is not a demand anywhere in the ladder, **by design** — the
system delegates it wholesale.

## Dropping that exclusion is NOT enough — measured

Tried: offer a pure numeric mix as a demand (a `PYC_NUMSPLIT` lever).
**It changes nothing**, on `fysphun`, `softrender`, `sudoku4` or `bh` — with
`PYC_CSSLOTDEMAND=1` as well, so both content channels were open.

The reason is structural, and it is the useful result here:

| program | coercion targets that are **ES**-contoured | **CS**-contoured |
| --- | --- | --- |
| `fysphun` | 283 | 15 |
| `softrender` | 513 | 15 |

**~96% of numeric coercion's targets are EntrySet-contoured — locals and
formals.** `coerce_annotate`'s own first loop says so: it scans
`for (EntrySet *es : fa->ess) for (Var *v : es->fun->fa_all_Vars)`, and only
then the closure/record CS vars and container elements.

And the ENTIRE demand ladder is about CreationSets: `elem_irrepresentable`,
`cs_elem_irrepresentable`, route 4's candidate set,
[152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md)'s backtrack.
**So opening the numeric case reaches roughly 4% of it**, which is why the
lever measures as a no-op. It was removed rather than left off
(CLAUDE.md: delete an inert lever, do not default it away).

## "We should be splitting EntrySets on mixed primitive formals" — why that does not fire

It should, and nothing excludes basic types from the machinery. Three
independent things stop it, and all three are measured on `softrender`.

**1. The confluence detector is EDGE-triggered, not level-triggered.**
`collect_type_confluence` flags an AVar only when some writer contributes a
type it does not already have:

```c
if (x->out->type->n && type_diff(av->in->type, x->out->type) != bottom_type) {
  confluences.set_add(av); ...
}
```

So a formal is a confluence only on the pass the union is FORMING. Once
`{int64, float64}` has settled, every writer is a subset, `type_diff` is
bottom, and the AVar is never reported again. Counting `IFA_DBG_CONFLUENCE`
reports for formals whose type is exactly `{int64, float64}`:

| | count |
| --- | --- |
| `added=0` (present, not flagged) | **1184** |
| `added=1` (flagged) | 47 |

**96% of the time the mix is sitting right there and no confluence is
raised.** A concrete one:

```
CONFL p=2 es=548 formal=x added=0  in: int64#6 float64#86  out: int64#6 float64#86
  | writers(1): {av=43886 __add__/es548: int64#6 float64#86 RAW: int64#6 float64#86}
```

Note the writer is `__add__/es548` — the SAME contour. The formal's writer is
inside its own contour: a self-loop, which is the fixed point made concrete,
and with ONE writer there is nothing to separate at that point even in
principle.

**2. In the 47 where it does fire, there is nothing to partition.** Every
in-edge already carries the union, so `etype == stype` and
`edge_type_compatible_with_entry_set` sees no disagreement — the same
self-blinding recorded in [146](146-remove-all-arbitrary-splitting.md).

**3. And the demand ladder could not consume it anyway**, because ~96% of
these AVars are EntrySet-contoured while the ladder is CreationSet-only (the
table above).

So "split on mixed primitive formals" is not a switch that is off — it is a
detector that has already stopped looking by the time the mix is observable,
over a fixed point that leaves nothing local to key on. The fix is not a
different demand SOURCE but a different TIME: see the correction below.

## CORRECTION, and the real defect: quiescence is never reached

I wrote that "the demand cannot come from a confluence". **That is wrong and
is withdrawn.** The confluence IS FA's demand mechanism — that is the core of
the design, not a detail. The defect measured above is not that confluences
cannot carry this demand; it is that **detection is edge-triggered instead of
being evaluated at quiescence**.

**Author's directive, 2026-09-15: ALL demand should be evaluated after
quiescence. All.** That directive is now
[157](157-FA-all-demand-must-be-evaluated-at-quiescence.md), which carries the
restructure; everything from here to the end of this section is its origin
and is kept for the trail.

That is the correct architecture and it is what "start minimal, split only on
demand" actually requires: let types reach a fixed point, then ask — on
CONVERGED types, level-triggered — which AVars hold a union something cannot
proceed on. An opportunistic test that fires while a union is FORMING sees a
transient and then goes blind, which is exactly the 1184-vs-47 result above.

**And the pipeline already believes this.** Stages 6+ are gated on
`!analyze_again`, i.e. quiescence of stages 1-5, with the reasoning written
out at `PER_CS_RECEIVER`. The file even records the failure:

> The stage IS starved on plcfrs/rdb/sudoku5 — TYPE_CONFLUENCE fires every
> pass there, so `!analyze_again` is never true and this never runs. […] The
> answer has to be a reason this stage may act, not permission to act without
> one.

**Measured (`PYC_DBG_QUIESCE`, new) — it is far worse than "starved on three
programs":**

> **Corrected in [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md),
> 2026-09-15.** These numbers are real but the label is wrong. Types are
> ALREADY converged every time the split stages run — `analyze_to_convergence`
> drains its worklists first — so `!analyze_again` is not a quiescence gate,
> and this table measures how often stage 1 ran out of work, not how often the
> analysis converged. It converged every pass. The starvation below is a
> cascade-priority problem, and level-triggering the detector was built,
> measured and deleted.

| program | passes reaching quiescence |
| --- | --- |
| `softrender` | **3 of 56** |
| `fysphun` | **3 of 18** |
| `sudoku4` | **0 of 19** |

On `sudoku4` — the program this whole line of work is about — the gate stages
6+ hang on is reached **zero times**. Stage counts agree: `TYPE_CONFL` fires
on nearly every pass (56 on softrender, 19 on sudoku4) while
`PER_CS_RECEIVER` and `CSM_ELEMENT_CS` fire **never**.

Route 4 escapes only because it takes quiescence as a PARAMETER
(`split_css_by_defs(int quiescent)`) rather than being gated out — which is
why ifa/143's level-triggered `csdemand` works at all, and is the one place
the architecture the directive describes is already implemented.

**So the ordering is right on paper and inoperative in practice.** An
edge-triggered stage 1 keeps finding transient work, quiescence never arrives,
and every demand test that waits for it never runs. That is upstream of the
int/float question, of `sudoku4`, and of ifa/146's ESBLOCK trade — all three
are downstream symptoms of demand being evaluated at the wrong time.

### What that implies for the work

Not "find another demand source". The restructure is:

1. **Make stage 1 level-triggered at quiescence.** Ask "does this AVar hold a
   union?" on converged types, not "did a writer just bring something new?".
   The edge-triggered form should not be what drives splitting.
2. **Then the numeric case needs no special demand source at all** — a formal
   holding `{int64, float64}` is visible at quiescence, every pass, instead of
   for one transient window.
3. **Coercion keeps its place as the last resort**, in reanalyze phase 2,
   applying only to what no split could separate.

## What the directive actually needs

An **EntrySet-side demand rung**, which does not exist today:

1. **The demand.** `coerce_annotate` already finds every pure numeric mix. The
   case to raise is the one it currently abandons — a narrow member that is
   not an immediate, where the constant rewrite cannot help (see
   [146](146-remove-all-arbitrary-splitting.md)'s account of `softrender`).
   Coercion knowing it cannot proceed IS the demand; nothing watches it today.
2. **The backtrack.** The mix cannot be keyed where it is observed: measured,
   every in-edge of every mixed contour already carries `{int64, float64}`,
   so "can this contributor carry int64" answers yes for all of them. It must
   be walked back to the pure/mixed boundary — and that boundary exists in
   quantity: ~641 pure-int and ~5663 pure-float AVars sit above ~639 mixed
   ones on `softrender` (`PYC_DBG_BOXPURE`).
3. **The split.** At the boundary the key IS type-shaped — which numeric basic
   this path carries — so it is a legitimate partition and not provenance.
   Bounded by the demand (two numeric kinds), never by a count of
   contributors (ifa/144).
4. **Coercion last.** Only for what survives: a genuine temporal mix like
   `bh`'s `Vec3` (CLAUDE.md's counterexample), where one object holds a float
   then an int over its lifetime and no contour split can separate it.

## Verification plan

- [ ] a fixture: two call paths, one int-only and one float-only, meeting at a
      shared formal — the analysis should split, not coerce, and the program
      should print CPython's `1` rather than `1.0`
- [ ] corpus `-m check`: `stdout_differs` should FALL, since each program that
      currently prints a coerced `1.0` is a program whose output disagrees
      with CPython
- [ ] coercion annotations should fall on the same programs (fysphun 298,
      softrender 528 today) without the violations rising

## What this unblocks

Every program whose output differs from CPython because an int was widened.
It is also the mechanism [146](146-remove-all-arbitrary-splitting.md) needs
for `softrender`: with the int and float paths split, `PYC_ESBLOCK` would no
longer relocate a mix past the point where coercion could repair it, and the
`sudoku4`/`softrender` trade would come apart.
