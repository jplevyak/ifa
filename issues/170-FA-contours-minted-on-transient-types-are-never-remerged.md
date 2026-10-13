# 170 — a contour decision taken on a pass's transient types is never revisited

**Status: open.** Root-caused 2026-09-25 on `voronoi`. Rewritten 2026-09-28
to also carry [097](closed/097-CGEN-callsite-vs-clone-formal-type-mismatch.md)
(an edge routed to a contour whose type was momentarily empty) and the
re-join residual of [144](closed/144-route-4-fans-per-creation-point-instead-of-partitioning.md).
The common root: **a split or a routing decision is taken on types that
are not yet the converged ones, and nothing takes it back when the
converged types no longer justify it.**

## Why it matters under this project's rule

A contour exists because something observed a distinction that required it
(AGENTS.md). A distinction observed on pass-0 imprecision, or on a
half-populated contour, is a statement about the analysis, not about the
program. [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md)'s rule
is that demand is a property of CONVERGED types. The corollary here: **a
contour whose demand has evaporated at convergence is unjustified and
should go.** Retiring one costs nothing in soundness, because
`analyze_to_convergence` re-derives every type from bottom each pass. Only
the DECISION persists, and the decision is what is stale.

## Instance 1 — duplicate EntrySets from pass-0 unions (voronoi)

With every constant mechanism off (`PYC_NO_FORCED_SPLIT=1` plus 169's
strip), `voronoi` has **156 contours for 140 distinct argument
signatures**. `int.__add__` (`IFA_DBG_FUNES=__add__`):

```
es=75  args= [int64] [int64]    <- 1 edge  from _init_by_array
es=124 args= [int64] [int64]    <- 17 edges
es=125 args= [int64] [int64]    <- 3 edges
```

`PYC_DBG_BIND=__add__`: all of them were minted in pass 0 on `int64|float64`
actuals, inside the Mersenne Twister (pure `int`). The float came from IFA's
minimal start: `float` inherits `int.__add__`, `voronoi.py:19`'s `xrand +
xoff` shares its one contour, and the `{int, float}` return flows into
`random`'s globals. Once the split separates float from int, es=124 and
es=125 both converge to `[int64][int64]`, and nothing merges them. The data
side has the same shape: `CS_DEF_PART` partitions the start-merged `list`
CS on pass-0 flow keys, and `voronoi` ends with two `list[int64]` CSs
against shedskin's one.

Not the start-merged default (`PYC_CSDCPA1=0` gives the same 156 / 140).
Not the routing knobs (`PYC_HARDREUSE=0/4/5`, `PYC_TYPEKEY=1` all leave 156).
Those choose WHICH existing contour a re-bound edge goes to. None of them
ever retires a contour.

## Instance 2 — routing on a momentarily-empty contour (msp_ss, 097)

`entry_set_compatibility` scores an edge against candidate EntrySets using
each candidate's formal types AS THEY ARE MID-PASS. On `msp_ss`,
`loadTIText`'s `l[0] == ord('q')` edge was scored against 7 `str.__eq__`
contours. Six correctly conflicted; ES 607 scored fully compatible because
its `x` formal had not yet been re-flowed this pass (its real contributors
are `str.strip`'s string constants). `find_best_entry_sets` took it, the
formal then grew to `{str, int64}`, and the routing was never revisited.
The emitted C had an argument type that disagreed with the clone's formal.
(A codegen guard now turns that into a diagnostic.)

An attempt to defer routing until the worklist drained was reverted in
2026-08. It changed which contours a pass reached and hit
[closed/098](closed/098-FA-per-pass-reset-scoped-to-reachable-set.md)'s
stale-state gap. 098 is fixed, so that blocker is gone.

### Instance 2b — the same, with a per-constant formal (2026-10-05)

Measured with `--fa-inline=0`, on `tests/cross_type_eq.py` and
`tests/minmax_3arg.py` (both pass on the default path). Reduced repro
(`b = "a"; print(c != b); print(True == b); print(True != b);
print(False == True); print(True != False); print(c == False)` with
`c = 2.5`): `bool.__eq__`'s `if isinstance(x, bool):` in a contour where
`x` is a `str` sends `False` to `bool.__pyc_to_bool__`, whose formal is
`__pyc_clone_constants__`-annotated. At convergence that edge sits in the
`False` contour, which is correct. But on the final pass it was first bound
to the contour that serves `__str__`'s non-constant `bool`. At the top of a
pass every formal is bottom, so `edge_type_compatible_with_entry_set`
returns `0` ("less compatible"), not `-1`, and the score can still pick it.
The `ret -> result` flow `analyze_edge` installs for that first binding
outlives the rebinding. So the condition reads `bool`, the dead `True` arm
is walked, and its narrowed `x` is reported `has no type`.

Two fixes are possible, and either would do. A constant-keeping formal could
hard-reject (`-1`) a contour holding a different constant or the abstract
type. Or rebinding an edge could retract the return flow of its previous
binding.

**Why this only shows up now.** Until 2026-10-05 the default path never
reached it. Mid-FA inlining folds the bool wrappers away, and the
post-inlining `analyze_to_convergence` skipped its first reset, so it
started from the previous convergence's values. That skip was itself this
issue's root at its largest: every value from before the last split was
carried into the final pass and could only grow. It is fixed (the reset
keys on `analysis_pass > 0`). Removing it exposed four single-convergence
bugs that the stale pass had been hiding, all fixed alongside:
`compute_es_can_raise` ran on cleared `out_edges` (so `except` never caught
with `--fa-inline=0`), the generator-return widening was a walk-time
snapshot, `make_seq` read its source once (`tuple(genexpr)` was untyped),
and a call's ifa/178 gate could only engage through stale edges. With
`--fa-inline=0` the suite went from 13 failures to these 2.

## Instance 3 — split products that converge equal (144)

Route 4 used to fan one contour per creation point. It now partitions by
assign-set signature, which no longer MAKES the duplicates. But nothing
collapses two products that later converge to identical content (`bh`'s
`Vec3`: 18 of 20 contours byte-identical across all 29 members before the
fan was removed). `cselem_rejoin_unknown_mints` (`PYC_CSREJOIN=1`) is the
only re-join in the tree, and it deliberately never touches split products.

## The fix: re-derive decisions at convergence

At the end of a pass (types converged), before the split stages:

1. **EntrySets.** Group each Fun's contours by converged argument types
   (constants stripped at formals that do not want them; 169, 151). Where
   two agree at every formal and no live demand keeps them apart, re-home
   the younger one's edges onto the older and retire it.
2. **Routing.** An edge whose actual types are incompatible with its
   contour's CONVERGED formals is re-routed. That is the same comparison
   step 1 makes, run from the edge's side.
3. **CreationSets.** Same-sym CSs whose converged content agrees (element
   type, per-position slot types, arity; 132) are compatible. Collapse
   them, the split-back direction of `PYC_CSREJOIN`.

The next pass re-derives. This is the compatibility leg of the three-way
rule: identity may stay fine, but two contours with no demand between them
are compatible.

**Stop condition, written before building:** if a merge re-creates the same
split on the next pass (the pair oscillates), the split has a live demand
this issue misread. Find what the demand is. Do not add hysteresis.

## Step 1 landed (2026-10-12): `merge_equivalent_entry_sets`

`PYC_ESMERGE`, default 1, in `fa.cc`. It runs as the LAST split stage, and
only on a pass where no stage acted. Two contours of one function merge when
all of the following hold:

- **Identical state.** Every AVar of the function has the same converged
  `out` in both. Every creation point maps to the same CreationSets
  (`cs_map`). The filters and the lexical display agree.
- **No coercion.** No AVar in either contour is coerced (`num_coerce`).
- **Same setter classes** (`same_eq_classes`) for every AVar.
- **Settled pass.** No type violation is outstanding.
- **No live demand on what they create.** Nothing either contour creates
  has an irrepresentable element or a pure int/float mix.
- **Not asserted this pass.** No ledger decision made or routed a group into
  either contour on this pass.

The younger contour's in-edges move to the older, with the same calls a
split uses in reverse (`set_entry_set`, `record_backedges`). Ledger
products, `route_last` and `route_adj` entries that name the retired contour
are pointed at the survivor. `IFA_DBG_ESMERGE` logs every merge.

The stop condition fired three times. Each time it named a demand the
first version misread, and none was answered with hysteresis:

1. **A split made as a MEANS.** First run before the split stages,
   `tests/splitter_setter.py` oscillated. The setter stage splits `__new__`
   so two Boxes get two creation points, and the two contours stay identical
   until the follow-up CreationSet split lands. Fix: merge last, on a quiet
   pass, when every follow-up has happened.
2. **Coercion.** An int contour and a float contour both read float64 once
   one is widened. Merging them put `random.py:135` (neural2) and
   `sudoku2.py:235` back into int/float unions, one warning each. Fix:
   never merge a coerced contour.
3. **The setter criterion.** On minilight a merge created a setter
   confluence in `__getitem__` es=887. The setter stage split it off
   (`SPLIT ES 887 setters ... -> 2831`) and the next quiet pass merged it
   back: 1,969 merges, the pass cap (35 → 102 passes), compile 12.7 s →
   26.8 s. That criterion is not type-shaped (ifa/146), so the converged
   types cannot show it. Fix: compare setter classes directly. minilight
   then runs 36 passes in 12.9 s and drops 767 → 729 EntrySets.

**Measured** (sweep `check__PYC_ESMERGE_1__82e9b98f+880362ce` vs
`check__default__82e9b98f+ad98a906`, same binary):
- **Verdicts:** identical on all 77 programs, including warning counts.
- **EntrySets:** 31,960 → 31,081 (−2.7%). CreationSets 120,904 → 120,849.
- **Compile time:** 2,747 s → 2,548 s (−7%): pygmy 231 → 164 s, sunfish
  249 → 184 s, plcfrs 130 → 119 s.
- **voronoi:** 178 → 172 EntrySets. `int.__add__` has one `[int64][int64]`
  contour where it had two (es=82 and es=148), and the output is unchanged.
- **`make test`:** green on both backends.

**Still open:** step 2's msp_ss check (`reroute_uncovered_edges`, from
ifa/178's fix 2, already re-takes stale routes at quiescence and may cover
it), and step 3, which merges equal CreationSets, the analogue of this for
data.

## Verification

- `voronoi` under `PYC_NO_FORCED_SPLIT=1`: contours == exact signatures
  (156 → 140); `int.__add__` has one `[int64][int64]` contour. At the
  default, fewer than today's 169 with output unchanged.
- `msp_ss`: no argument-vs-formal type mismatch at the `ord('q')` site
  (the program fails for other reasons; check the site, not the verdict).
- Corpus `-m check`, same binary: no verdict regresses; `ess`, `css` and
  compile time fall.
- `make test` green on both backends.
