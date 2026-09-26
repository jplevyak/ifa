# 170 — contours minted on a pass's transient types are never re-merged when they converge equal

**Status: open.** Root-caused 2026-09-25 while counting unnecessary
contours on `voronoi` against a hand-derived minimum and shedskin (see
[169](closed/169-FA-constants-leak-into-split-grouping-through-the-dispatch-filter.md)'s
census). Analysis time is the cost: every contour is re-analysed every pass.

*2026-09-26: 169's constant-strip is now the default, and `int`'s arithmetic
is no longer annotated (151), so `voronoi`'s default is now 171 contours
for 142 exact signatures. That gap still includes the remaining annotated
constants (`range`, tuple keys); the 16 duplicates measured below, with
every constant mechanism off, are this issue's own share.*

## Symptom

`shedskin_examples/voronoi` (57 lines, compiles, matches CPython). With both
constant mechanisms switched off -- `PYC_NO_FORCED_SPLIT=1` for
[134](134-remove-the-frontend-forced-split-opt-in.md) and the
constant-strip probe of 169 -- there are still **156 contours for 140
distinct argument signatures**: 16 contours are exact duplicates of a
sibling of the same Fun. `int.__add__` is the clearest (`IFA_DBG_FUNES=__add__`):

```
es=75  args= [int64] [int64]    <- 1 edge  from _init_by_array
es=124 args= [int64] [int64]    <- 17 edges from _genrand/_init_by_array/append
es=125 args= [int64] [int64]    <- 3 edges from _init_genrand/_genrand/_init_by_array
```

No constants on any of them. `IFA_DBG_INCOMPAT` shows every rejection on the
argument clause, none on returns.

## Root cause

`PYC_DBG_BIND=__add__` shows when each was minted:

```
BIND pass=0 __add__ e=99  MINT es=75  from_es=68 line=99 actuals=[int64 int64]
BIND pass=0 __add__ e=99  MINT es=124 from_es=68 line=99 actuals=[int64|float64 int64]
BIND pass=0 __add__ e=228 MINT es=125 from_es=90 line=39 actuals=[int64|float64 int64|float64]
BIND pass=0 __add__ e=199 MINT es=137 from_es=62 line=19 actuals=[int64|float64 int64]
BIND pass=0 __add__ e=229 MINT es=138 from_es=90 line=39 actuals=[int64|float64 int64|float64]
```

All in **pass 0**, on `int64|float64` actuals, at `pyc_lib/random.py`
lines 39 and 99 -- the Mersenne Twister, which is pure `int`. The `float`
is the analysis's own: pass 0 starts with ONE contour of `int.__add__`
(IFA's minimal start, correct), `float` inherits `int.__add__`, so
`voronoi.py:19`'s `xrand + xoff` shares it, its return is `{int, float}`,
and that flows into `random`'s globals (`_mti`, `_mt`'s element). The
first split then mints contours keyed on those transient unions. Once the
split separates `float` from `int`, the actuals of es=124 and es=125 both
converge to `[int64][int64]` -- and nothing ever merges two contours whose
converged types agree. They persist, and so do their callees' splits.

This is not the start-merged CreationSet default: `PYC_CSDCPA1=0` gives
the identical count (156 / 140). The durable-key routing knobs do not
touch it either -- `PYC_HARDREUSE=0/4/5`, `PYC_TYPEKEY=1` all leave 156 --
because they choose WHICH existing contour a re-bound edge goes to; they
never retire one.

The data side has the same shape. In pass 0 `CS_DEF_PART` partitions the
start-merged `list` CreationSet (8 creation points) on flow keys taken
while every list's element was `{int64, str, tuple}`, and later stages
refine from there; `voronoi` ends with two `list[int64]` CreationSets
(cs=1077, arity -1, and one arity 0) against shedskin's one, and container
methods are then contoured per receiver CreationSet.

## Why it matters under this project's rule

A contour exists because something observed a distinction that required
it (AGENTS.md). es=124's distinction from es=75 was observed on a type that
no longer exists at convergence -- the observation was about pass 0's
imprecision, not about the program. [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md)
says every demand is a property of the CONVERGED types; the corollary this
issue adds is that a contour whose demand has evaporated at convergence is
unjustified and should go. `analyze_to_convergence` already re-derives
types from bottom every pass, so retiring a contour costs nothing in
soundness -- only the DECISION persists, and it is the decision that is
stale.

## Proposed fix

At the end of a pass (types converged), for each Fun, group its EntrySets
by converged argument types (constants stripped at unannotated formals,
the same view the splitter should use -- 169). Where two contours agree at
every formal and neither is kept apart by a still-live demand, re-home the
younger one's edges onto the older and retire it; the next pass
re-derives. The same for CreationSets of one sym whose converged content
agrees (element type, per-position tuple types, arity -- ifa/132).

Stop condition, written before building: if the merge re-creates the same
split on the next pass (the pair oscillates), the split has a live demand
this issue misread, and the next step is to find what the demand is -- not
to add hysteresis.

## Verification plan

- `voronoi` under `PYC_NO_FORCED_SPLIT=1` + 169's fix: contours == exact
  signatures (156 -> 140); `int.__add__` has one `[int64][int64]` contour.
- At the default: `voronoi` below 196 (169's number) with output unchanged.
- Corpus `-m check`: no verdict regresses; ess and compile time down.
- `make test` green on both backends.

## What this unblocks

The 16-of-93 excess on `voronoi` that neither constant fix reaches, and
every contour a program inherits from the pass-0 union of a shared numeric
method -- which is every program whose floats touch `int`'s methods before
the first split separates them.
