# 207 — call matching re-matches the whole argument union every time it grows by one member

**Status: open; the measurements are taken (2026-11-08, below), and
direction 1 is supported.** Filed 2026-11-08 from
[203](203-shrink-the-graph-fa-walks.md)'s re-scoping. That measurement
put FA's time in call matching, not flow, on every program where FA takes
real time. This issue is for the matching algorithm itself
(`ifa/if1/pattern.cc`: `pattern_match`, `match_cache_hit`, `Matcher`).
[186](186-FA-pass-one-confluence-through-shared-builtins.md)'s two
directions, deferred connection and a wide-union `AType`, become
candidate answers here. 186 keeps othello3 as its witness.

## Measured (tree `2ddb82de`, temporary probe, since removed)

**Probe.** `IFA_DBG_MATCHPROF` timed every `function_dispatch`
(`fa.cc`) with `steady_clock` and recorded:
- per pass: calls, match-cache hits, misses, early returns for an
  incomplete call, and the argument width (the CreationSets summed over
  the argument AVars);
- per send: its total time;
- repeats: how often the same (send, EntrySet) was dispatched again in
  the same pass.

Each program was compiled alone.

**sunfish**, the clearest case, totals over 31 passes:

| | count | time |
| --- | --- | --- |
| dispatch calls | 4 612 683 | |
| cache hits | 4 318 611 (94%) | 6.0 s |
| cache misses | 168 422 (3.7%) | **61.7 s** (≈ 0.37 ms each) |
| incomplete calls | 125 650 | 0.0 s |
| edge making (`make_AEdges`) | | 1.5 s |
| calls that repeat a (send, EntrySet) already dispatched this pass | 4 371 403 (95%) | |

By pass:

| pass | calls | misses | avg width | max width | miss time |
| --- | --- | --- | --- | --- | --- |
| 0 | 844 897 | 35 124 | 17.4 | 205 | 19.1 s |
| 1–4 | 222 K–350 K each | 19 K–24 K each | ~12 | 103–125 | 6.3–8.6 s each |
| 5–9 | 213 K–354 K each | 4 K–15 K each | 6–11 | 55–94 | 1.2–7.0 s each |
| 10–30 | ~57 K–60 K each | ≤ 1 539 each | ~3 | ≤ 27 | ≤ 0.9 s each |

The cost is concentrated in sends on comparison operators over wide
unions. The top 25 sends by time are almost all `__eq__`/`__lt__`, at
width ≈ 101, with miss rates of 40–70%. For example, the `__eq__` inside
`dict.__setitem__` takes 6.7 s over 5 604 calls, 2 690 of them misses.

**plcfrs.** Matching is 32.4 s. The top sends are again
`__lt__`/`__eq__`, at width 137, with 50–80% misses.

**chess.** Matching is 1.0 s, at widths 33–71, with few misses. This is
the healthy case.

**The timer.** `-v`'s per-pass `match` timer exceeds the pass time on
pass 1 (sunfish: 22.4 s of a 9.1 s pass, `flow -148%`). Some matching
runs outside `pass_timer`, so FA time printed by `-v` is understated.
Fix the accounting before using it as the yardstick.

## Mechanism

`match_cache_hit` keys an entry on the exact `AType` pointer of every
top-level argument (`e->top_out.v[i] != args.v[i]->out`), then validates
the nested positions. The flow worklist re-fires a send whenever an
argument's type grows. A type that grows by one CreationSet is a new
hash-consed `AType`, so the lookup misses, and the `Matcher` re-matches
the **whole** union from scratch: visible functions, per-position
filters, candidate selection. The cache lets later passes replay the same
growth sequence cheaply. That is why hits are 94% and cost little. But
on the passes where the unions are first built, every growth step is a
full re-match.

So a send whose argument reaches width *w* one member at a time does
about *w* full matches of cost O(*w*), which is quadratic in union
width. sunfish's numbers fit: 95% of dispatches repeat a send already
dispatched that pass, and the cost lives in the passes, and the sends,
with the widest unions. 186's othello3, with a union of about 2 039
contours on pass 1, is the same mechanism at its extreme.

## What a new algorithm must keep

- **Identity.** The result may depend only on the argument types (sets
  of CreationSets) and the visibility point: never on provenance, never
  on eager CPA splitting (AGENTS.md). Caching per CreationSet is caching
  on type identity, not provenance. A CreationSet is a member of the
  type.
- **closed/037's fix.** The matcher must not enumerate the cartesian
  product of per-argument CreationSets.
- **Every consumer of a `Match`.** That means `make_AEdges`, the
  per-edge `filtered_args`, partial applications, the keyword-argument
  rejection (`PYC_KWSTRICT`, issues/103), `dispatch_arity_epoch`, and
  `arg_of_send` registration.

## Directions, to be chosen by measurement

1. **Incremental matching.** Match a growth step by its delta, not by
   the whole union. The candidate set for a union is the union of the
   candidates for its members position by position (that is what the
   matcher's per-position filters compute). So cache per (send or
   selector, visibility point, position, CreationSet) → the filter
   result. A growth step then costs O(new members × positions), and a
   send's total cost is linear in its width. The open question is which
   parts of a `Match` are not member-decomposable (the formal-to-actual
   map is per candidate; `filtered_args` are per edge and grow by
   union), and whether merging into the existing edge
   (`e->match->merge`) is enough for them.
2. **Deferred connection** (186 direction 1, shedskin's `CPA_LIMIT`
   scheduling). Do not re-dispatch a send whose argument union is still
   growing past a width threshold until the worklist has otherwise
   drained, then dispatch once with the settled union. This is
   scheduling, not identity. It cuts the *w* steps to a few without
   changing a single match's cost.
3. **A wide-union `AType`** (186 direction 2). A bitset over CreationSet
   ids above a width threshold, so union, diff and subset (and the
   cache's validation walk) cost O(*w*/64). This lowers a single match's
   cost and composes with either of the above.

1 removes the quadratic, 2 removes most of the steps, and 3 is a
constant factor. Try 1 first if the decomposition holds for everything a
`Match` carries; otherwise 2, which needs no new invariant.

## The measurements, 2026-11-08 (tree `24c7d5f1`, temporary probes, since removed)

**Probes.** The same `IFA_DBG_MATCHPROF` probe as above, extended:
- `steady_clock` laps inside `pattern_match`, one per stage;
- per (send, EntrySet), the number of distinct argument-type tuples
  dispatched in a pass (the growth steps), against the widest argument
  width it reached;
- each pass's wall time, process CPU and thread CPU, next to
  `pass_timer` and `match_timer`;
- the dispatch count at every `initialize_pass`.

Each program was compiled alone.

**Where the time goes, in seconds, whole compile:**

| program | matching | hits (lookup) | misses | in a miss: `find_all_matches` | `prune`/`find_best_matches`/`reverify` | `cannonicalize_matches` | lookup, ctor, `find_visible_functions`, keywords | `make_AEdges` |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| sunfish | 69.3 | 7.6 | 59.7 | **50.3** | 7.4 | 1.6 | 0.3 | 1.5 |
| plcfrs | 32.3 | 5.3 | 25.7 | **17.3** | 7.4 | 0.4 | 0.4 | 1.0 |
| othello3 | 587.5 | **266.2** | 299.5 | 94.7 | **152.4** | 40.6 | 9.8 | 20.7 |
| dijkstra2 | 0.6 | 0.2 | 0.3 | 0.25 | 0.08 | 0.01 | 0 | 0 |
| pylife | 0.2 | 0.1 | 0.1 | 0.10 | 0.01 | 0 | 0 | 0 |

**Answers to the three questions:**

- **Inside a miss, the cost is the matcher's per-member loop.** It is not
  the cache lookup, not the visible-function scan and not the keyword
  filter, which together are under 2%. `find_arg_matches` iterates over
  every CreationSet of each argument and, for each, calls
  `pattern_match_sym(cs->sym, position, candidates)` and
  `update_match_map`, recursing into the members of a pattern
  (tuple) formal. That is O(*w* × candidates) a miss. At othello3's
  width (about 1 000) candidate selection (`find_best_matches`) and
  canonicalisation overtake it, and so does the hit path. A hit's
  validation (`get_all_args` and the position-by-position comparison)
  costs 266 s, as much as the misses. So at that width even a hit is
  O(*w*).
- **The *w*-steps model holds, as a fraction of *w*.** Distinct
  argument-type tuples per (send, contour) per pass, by the widest
  argument reached:

  | program | width 32–99 | width 100+ |
  | --- | --- | --- |
  | sunfish | 10–14 steps at ~50 | 15–25 steps at ~102 |
  | plcfrs | 11–23 steps at ~63 | 10–22 steps at ~135 |
  | othello3 | 3–4 steps at ~52 | 27 at ~956 on pass 0; **282–293 at ~903** on later passes |

  Members arrive in batches, so the steps are about *w*/3 to *w*/5, not
  *w*. The cost is still quadratic. The steps repeat every pass, because
  each pass re-derives types from bottom and the growth reaches different
  intermediate `AType`s. That is why sunfish's passes 1–4 still miss
  about 20 K times each: the cross-pass cache only hits when a growth
  sequence repeats exactly.
- **The comparison operators dominate wherever matching does.**
  - sunfish's and plcfrs's top sends are `__eq__`/`__lt__` at width
    100–140.
  - othello3's top four are `__eq__` at width 1 907, 23–31 s each over
    80–137 calls.
  - dijkstra2 and pylife (196's witnesses, not 207's) match in under
    1 s. Matching is not their problem.

**The timer.** Pass 0 is several FA passes. plcfrs runs `initialize_pass`
six times before `analysis_pass` advances: the frontend's structural
repair is driven to a fixed point first (055's mode 2, `extend_analysis`
/ `reanalyze`). Each run restarts `pass_timer` and leaves `match_timer`
running, so `-v`'s "PASS 1" line holds only the last run's pass time
against all the runs' matching (plcfrs 0.55 s against 6.9 s, sunfish
8.8 s against 22.0 s). From pass 1 on, wall time, process CPU, thread CPU
and `pass_timer` agree to within 0.02 s; Boehm's marker threads do not
distort them. **Fix before using `-v` as the yardstick:** restart
`match_timer` (and `extend_timer`) with `pass_timer`, or count the
sub-passes. The pass counts printed today also omit them.

**What this means for the directions:**
1. **Incremental matching is the one that removes the quadratic,** and
   the decomposition it needs is already in the code. Per argument
   CreationSet, the matcher computes `pattern_match_sym` on `cs->sym`,
   the position and the current candidate set. Only `update_match_map`
   and the pattern recursion are per CreationSet. So a growth step can
   match only its new members, against the candidates the old members
   left. The open part is still everything `Match` carries:
   - the per-candidate formal-to-actual map;
   - the per-edge `filtered_args`;
   - `find_best_matches`' choice among candidates. That is a function of
     the whole union, but it is re-taken only when a new member changes
     which candidates survive.
2. **Deferred connection** cuts the steps, but not the per-step O(*w*),
   and at othello3's width a single hit is O(*w*) as well.
3. **A wide-union `AType`** is the fix for the hit path at othello3's
   width (`get_all_args`' comparison walk). It is the one place a
   constant factor is not enough on its own.

**Next:** prototype 1 on sunfish behind a flag, with the match results
compared against the current matcher on every dispatch. The result must
be identical, so a mismatch is a bug in the decomposition, not a
precision trade.

## Verification

- sunfish's and plcfrs's matching time is down by a large factor.
- Every corpus `check` verdict and every EntrySet and CreationSet count
  is unchanged, since the matching result must be identical.
- `make test` is green on both backends.
- othello3 compiles under the 400 s cap (186's stop condition).
