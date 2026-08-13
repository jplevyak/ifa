# 099 — `check_split`'s pending-backedge route + the `avoid` veto force a period-2 flip-flop when the recorded candidate set has two members

**Status:** open, root-caused 2026-08-13 while answering "why is the
system oscillating?" for [074](074-FA-cross-pass-oscillation-plan.md).
The mechanism is *structural* — not a heuristic misfiring, not a timing
race — and is mechanically forced whenever two conditions coincide. Not
fixed: the obvious repairs each have a plausible failure mode (below) and
this is the most-reverted surface in the tree.

**Affects:** `ifa/analysis/fa.cc` — `check_split`'s pending-backedge
branch (~1231), `record_backedges` (~4333),
`EntrySet::pending_es_backedge_map` (fa.h), and
`apply_entry_set_split`'s detach loop (~5049), which supplies `avoid`.

## Symptom

Three of [074](074-FA-cross-pass-oscillation-plan.md)'s eight genuinely
non-convergent programs — its "stable residual" group, **bh**, **pylife**
and **linalg** — never converge, yet **nothing grows**. Measured per pass
over the last ten passes of each: zero new AEdges, zero new EntrySets,
zero new CreationSets, zero `copy_AEdge`. The entire per-pass activity is
a handful of *existing* edges being detached and re-bound:

| program | new edges/pass | new ES/pass | new CS/pass | edges re-bound/pass |
|---|---|---|---|---|
| bh | 0 | 0 | 0 | 10 |
| pylife | 0 | 0 | 0 | 1 |
| linalg | 2 | 1 | 1 | 3 |

`pylife` is the minimal case: **one edge** is responsible for the whole
non-convergence.

## Root cause

`check_split`'s first branch is the "recursion follows its split-off
caller contour" default:

```cpp
if (Vec<EntrySet *> *ess = e->from->pending_es_backedge_map.get(e)) {
  Vec<EntrySet *> sorted_ess;
  for (EntrySet *es : *ess) if (es && es != avoid) sorted_ess.add(es);   // <-- the veto
  qsort_by_id(sorted_ess);
  if (sorted_ess.n) { set_or_copy_AEdge(e, sorted_ess[0], ees); return 1; }
}
```

`avoid` is non-null only when `make_entry_set` is reached from
`apply_entry_set_split` — and it is then **exactly the contour the
splitter is detaching this edge away from**, i.e. the one the edge is
currently in. The veto exists for a good reason (its comment: the pending
map's binding is "a default, not evidence", and must yield when the
splitter has concrete type evidence against it).

But `pending_es_backedge_map` is a `map_set_add` accumulator that is
**never cleared** — not by `clear_results`, not per pass — so a key's
candidate set only grows. Once it holds exactly **two** contours, the two
rules compose into a forced alternation:

- pass *N*: edge sits in A; splitter detaches it, `avoid = A`; after the
  veto exactly `{B}` remains; edge binds to **B**.
- pass *N+1*: edge sits in B; splitter detaches it, `avoid = B`; after the
  veto exactly `{A}` remains; edge binds to **A**.

Period 2, forever, with no growth and no way for the flow analysis to
break the tie. Instrumented output (probe removed), `pylife`, last five
passes — one edge, one candidate pair:

```
PEND p=35 e=2016 fun=set avoid=524 cands={507 524} after_avoid={507} -> 507
PEND p=36 e=2016 fun=set avoid=507 cands={507 524} after_avoid={524} -> 524
PEND p=37 e=2016 fun=set avoid=524 cands={507 524} after_avoid={507} -> 507
PEND p=38 e=2016 fun=set avoid=507 cands={507 524} after_avoid={524} -> 524
PEND p=39 e=2016 fun=set avoid=524 cands={507 524} after_avoid={507} -> 507
```

`bh`, same shape at ten edges and two contour pairs ({556,570} and
{558,571}), every edge swapping every pass:

```
REBIND p=49 e=1141 load_tree@bh.py:415 es556 -> es570 via=pend
REBIND p=49 e=1142 load_tree@bh.py:415 es558 -> es571 via=pend
REBIND p=50 e=1141 load_tree@bh.py:415 es570 -> es556 via=pend
REBIND p=50 e=1142 load_tree@bh.py:415 es571 -> es558 via=pend
...
```

`linalg`: edge 1872 (`__ne__`), candidates `{794, 1214}`. Across all
three programs the candidate-set size histogram is dominated by **2**
(bh: 186 occurrences of size 2 vs 47 of size 1 and 26 of size 3).

Two conditions must coincide, and **either one alone is harmless**:

1. the splitter keeps re-deciding to split this contour (so `avoid` keeps
   being supplied), and
2. the recorded candidate set has exactly two members (so the veto leaves
   exactly one, and it is always the one just vacated).

A one-element set binds stably (`bh` shows `cands={556} avoid=570 -> 556`
repeatedly, no flip). A three-element set still alternates but over a
larger cycle.

## Why this is worth fixing on its own

It is the entire non-convergence of three programs, it is *provably*
non-terminating rather than merely slow, and — unlike the rest of
[074](074-FA-cross-pass-oscillation-plan.md)'s territory — it has a
single, small, well-localized cause. It also explains why these three are
074's "stable residual" shape (identical violation counts and
byte-identical program output with and without the stall guards): the
flip-flop changes nothing observable, it just never stops, so the guards
are all that terminate the compile.

## Fix options (none tried; each has a specific hazard)

**A — don't bind to a candidate the veto reduced to a single
just-vacated contour.** If, after removing `avoid`, the survivors are a
strict subset that the edge has already occupied in a previous pass, fall
through to the split/fresh path instead of binding. Needs a per-edge
"contours I have been bound to" record; hazard: that record is exactly
the kind of cross-pass state 098 showed is easy to leave stale.

**B — make the pending map's binding stable under the veto.** The map is
a *default* for recursive edges; the veto turns it into an alternator.
Prefer the candidate that the edge is *already* in when it is a member
(i.e. veto only when the edge is not already correctly bound), so a split
that has no better evidence leaves the edge where it is. Hazard: this is
close to the "suppression is not eviction" attempt 074 records under
Stage 1, which relocated churn rather than stopping it — though that was
on a different route.

**C — bound the map.** It is `map_set_add`-accumulated and never cleared;
a key whose set has grown past one member means the recursive default is
already ambiguous. Reset it per pass, or refuse to use a key whose set
has more than one member (fall through to the ordinary routes). Hazard:
the map is what ties monomorphic recursion to its caller's contour; 074's
Stage-1 notes call the surviving `e->to` + ledger the "durable substrate",
so weakening it may reopen the recursion-fan-out that
[closed/073](closed/073-teach-splitter-productive-vs-inert-context.md)
closed.

**D — stop the splitter re-deciding** (condition 1). This is 074's own
Stage-1 territory and is the larger, harder half; A/B/C are attractive
precisely because they break the loop at the other condition.

## Verification plan

1. `bh`, `pylife`, `linalg` must reach `pass_limit_hit=0`, and their
   program output must stay byte-identical (it is currently unaffected by
   the flip-flop — see 074's re-base table — so any output change is a
   regression, not a win).
2. Re-run 074's re-based classification: Group C should shrink from 8 to
   5; nothing may move from Group A into B or C.
3. `test_pyc.py` both backends and the full shedskin sweep, diffed
   against a baseline — `check_split` is the surface 073, 064 and 065 all
   reverted changes on.
4. Watch `match_seq` / `match_none` specifically (073's regression canaries
   for this function).

## What this unblocks

Three of the eight genuinely non-convergent programs, and it removes the
"stable residual" shape from 074's target set entirely — leaving 074 to
deal with only the *growth* shape (plcfrs, rubik, yopyra, loop, go),
which has a different mechanism (see 074's census: growth is driven by
`apply_entry_set_split` minting fresh contours, because `make_entry_set`
skips `find_best_entry_sets` whenever `split` is non-null).
