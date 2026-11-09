# 207 — call matching re-matches the whole argument union every time it grows by one member

**Status: open.** The measurements are taken, and a prototype of
direction 1 is exact (0 mismatches in 494 249 checked dispatches) but buys
only 11–17%, which says what the real algorithm needs (2026-11-08,
below). Filed 2026-11-08 from
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

## Prototype, 2026-11-08: incremental matching from the cache (in the tree, off by default)

**What it is.** `IFA_INCMATCH`, in `pattern_match` (`incmatch_try`). `=1`
uses it; `=2` computes it, runs the full matcher anyway, and compares the
two Match by Match, printing an `INCMATCH` summary at exit. It is kept as
the reference the redesign is checked against, not as a lever to turn on.
A miss whose arguments EXTEND an existing
cache entry E (every argument type a superset of E's, the selector
unchanged, no nested pattern positions) is answered from E in one of two
ways:

- **Twin path.** Each new CreationSet has a twin in E's type at the same
  position: the same `Sym`, static arity and member count. Every test
  the matcher applies to a member reads only what twins share:
  `pattern_match_sym` keys on the `Sym`, `arity_admits` on the static
  arity, and candidate selection on `cs->sym` and `cs->sym->type`. So
  the new member goes into exactly the formal filters its twin is in. The
  twin must appear in no other argument.
- **Delta path.** Otherwise, the result decomposes over combinations of
  argument members: `find_best_cs_match` picks winners per combination,
  and `set_filters` adds that combination's members. So the full result
  is E's result merged with the matches of the new combinations only.
  Partition those by the first grown position that holds a new member.
  Run *k* sets the grown positions before it to their old types, it to
  its delta, and those after to their full types. The runs reuse
  `pattern_match_full`, with the argument `out`s swapped temporarily and
  cache insertion suppressed.

**Correctness.** Check mode over all 606 corpus and suite compiles:
494 249 dispatches answered by the prototype and checked against the
full matcher. **0 mismatches.** The one bug it found: merging filters
with `type_union`, which subsumes members, where the matcher builds an
explicit set (`cache_copy`'s `make_AType`). That produced 61 mismatches
in five programs before the fix. sunfish's and plcfrs's generated C are
byte-identical with it on.

**Speed** (each program compiled alone):

| | matching, default | matching, prototype | dispatches answered by it |
| --- | --- | --- | --- |
| sunfish | 67.3 s | 56.0 s (−17%) | 103 976 of 168 408 misses |
| plcfrs | 31.1 s | 27.6 s (−11%) | 125 661 of 197 475 misses |

**Why so little.**
- **The cheap paths take the cheap misses.** The twin path costs about
  47 µs per dispatch, but it answers the narrow ones.
- **The delta runs cost about a full match each.** One run per grown
  position still walks the FULL width of every position it does not
  restrict: `find_all_matches` visits every member of every argument,
  and candidate selection enumerates their classes. On sunfish the
  answered dispatches took 35.0 s, most of it in the multi-position
  delta runs (13 519 dispatches where two or more positions grew at
  once, the commonest shape on `__eq__`/`__lt__`).
- **What remains is 10.2 s of full matches,** chiefly:
  - "not a superset of any entry" (19 K, 4.4 s), because types re-derived
    each pass grow through different intermediate types;
  - "first sight" (42 K, 2.7 s).

**What the real algorithm needs, then.** The decomposition is exact, so
the gap is in reuse, not correctness. The matcher has to keep **per
(send, position) state across dispatches**:
- for each argument CreationSet already seen there, the candidates that
  accept it and the formal it reaches;
- and per combination class, the winners;

so that a growth step does work proportional to its new members times
the classes at the other positions, not to the full widths. Two things
make that a redesign, not a patch:
- `Matcher` is built per call and discarded;
- `find_best_matches`' class collapsing (037) is computed per call over
  the whole union.

The cross-pass misses ("not a superset") suggest keying that state on
CreationSets, not on `AType`s. The state then survives the per-pass
reset, as decisions do (AGENTS.md: keep decisions, rebuild structure).

**Not addressed by this prototype:** othello3's hit path (266 s of cache
validation at width about 1 000) is direction 3's.

## Verification

- sunfish's and plcfrs's matching time is down by a large factor.
- Every corpus `check` verdict and every EntrySet and CreationSet count
  is unchanged, since the matching result must be identical.
- `make test` is green on both backends.
- othello3 compiles under the 400 s cap (186's stop condition).

## The redesign plan (2026-11-08)

**Idea: match member CLASSES, not members, and keep the answers.** The
prototype established two facts:
- members with one dispatch signature (`Sym`, static arity, member count)
  are treated identically;
- the result decomposes over combinations of argument members.

So per (send, position) the matcher keeps state that persists across
dispatches:

1. **member → class.** A CreationSet's dispatch signature, the twin rule
   generalised. Classes are what the matcher's tests read; members only
   decide what goes into the output filters.
2. **class × position → accepting candidates.** `pattern_match_sym` is
   already memoised per (`Sym`, position) in `sym_match_cache`; this adds
   the arity and `None` tests.
3. **class tuple → winners,** computed by the existing
   `find_best_cs_match`/`set_filters` on class representatives, as 037's
   collapse already does within one call. It depends only on the
   candidate set and the class tuple, so it can be shared across sends
   (every `__eq__` over the same classes).
4. **Output from current members.** A function's filter at a formal is
   the current members whose class is in one of its winning tuples. That
   is set work, not matching.

A growth step then costs (new classes × classes elsewhere) lookups, and
nothing when the new member's class is known. A "hit" becomes "no new
members since the last dispatch", which ends othello3's 266 s validation
walk. The state is keyed on CreationSets, which persist across passes,
so the per-pass reset replays lookups.

**Steps, each with its stop condition:**

1. **Fix `-v`'s timer. Done 2026-11-08.** `initialize_pass` no longer
   restarts `pass_timer`. The first run of a numbered pass starts it, and
   later runs `snap()`, so a pass's time covers all its runs. The `-v`
   line now ends with `N runs` (plcfrs pass 1: 6, sunfish: 5).
   - Corrected totals: sunfish FA 97.0 s (match 67.7, flow 26.0);
     plcfrs 54.0 s (match 29.6, flow 19.8).
   - The earlier "flow 2.5 s" for sunfish was the timer bug: pass 1's
     flow was negative and cancelled the rest.
2. **Measure class compression. Done 2026-11-08.** A temporary probe
   (`IFA_DBG_CLASSES`, removed) ran at the end of every full match. It
   counted members against signature classes (`Sym`, static arity,
   `no_static_arity`, member count) at each position. It also checked
   that every member of a class, at a position it holds alone, lands in
   the same (function, formal) filters.

   **Uniformity holds everywhere: 0 uneven positions** in 474 311 (plcfrs
   and sunfish) and 1 241 915 (othello3) checked.

   **Compression, over the misses with a position at least 32 wide:**

   | program | wide misses | width → classes | combinations, members → classes |
   | --- | --- | --- | --- |
   | plcfrs | 104 717 | 52.3 → 12.9 | 2^6.6 ≈ 97 → 2^4.2 ≈ 18 |
   | sunfish | 66 100 | 41.2 → 15.6 | 2^6.2 ≈ 74 → 2^4.6 ≈ 24 |
   | othello3 | 1 043 735 | 887.6 → **848.5** | 2^9.8 → 2^9.7 |

   **What it means.**
   - **Compression is 3–5× on sunfish and plcfrs, and none on othello3,**
     whose union is the `Flip` subclasses, each its own `Sym`. So the
     stop condition as written ("classes ≈ width") is hit on othello3.
   - **But that condition mis-stated the plan.** The redesign has two
     factors, and compression is the smaller. The larger is **reuse**: a
     class is matched once per send, not once per growth step per pass.
     That is the 10–25 steps per pass on sunfish and plcfrs, 280–290 on
     othello3, and again on every pass, since the state is keyed on
     CreationSets, which persist. Reuse does not depend on compression.
   - **On othello3, then, the whole gain is reuse.** Its cost moves to the
     output. A function's filter is still O(*w*) to build per dispatch.
     The cheap case to exploit is a filter equal to the whole argument
     type, so its `AType` pointer is reused; a single `__eq__` candidate
     accepting everything is that case. That is step 5's other half,
     alongside ending the hit-validation walk.

   **Revised stop condition for step 3:** on sunfish, a class is matched
   at most once per (send, position) over the whole compile. If the
   per-class work is not then a small fraction of today's matching (it
   is 3–5× fewer classes times 10–25× fewer steps), stop and re-profile
   before step 4.
3. **Per-send state and class-level matching.** Fall back to the full
   matcher for nested pattern positions, named or out-of-position
   arguments, varargs and generic candidates. Every answer is checked
   against the full matcher (`IFA_INCMATCH=2`'s harness); a mismatch is
   a bug, never a trade.
4. **Share the combination table** across sends.
5. **Replace the `AType`-keyed cache.** Unchanged arguments mean an
   unchanged answer.
6. **Retire the prototype,** and the old cache wherever the new state
   covers it.

**Verification:**
- the corpus's generated C is byte-identical for every program;
- `make test` is green;
- `check` sweeps are unchanged on both backends;
- a timed A/B on sunfish, plcfrs and othello3 shows a large fall in
  matching;
- 186's stop condition: othello3 under the cap.

**Risks:**
- **Nested patterns and named arguments.** The signature would have to
  include member classes; they are a fallback at first.
- **Invalidation.** `add_patterns`, new instantiation wrappers and a
  `dispatch_arity_epoch` change must drop the state.
- **`Matcher` is per call and woven through `PMatch`,** so step 3 is
  the bulk of the work.
