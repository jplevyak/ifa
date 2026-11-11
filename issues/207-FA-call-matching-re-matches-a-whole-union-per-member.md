# 207 — call matching re-matches the whole argument union every time it grows by one member

**Status: open.** The measurements are taken, and a prototype of
direction 1 is exact (0 mismatches in 494 249 checked dispatches) but buys
only 11–17%, which says what the real algorithm needs (2026-11-08,
below). **The direction changed on 2026-11-09: stop re-dispatching when a
union grows, rather than make the re-dispatch faster.** See
[the last section](#revised-direction-2026-11-09-dispatch-as-a-standing-constraint),
which supersedes steps 3–6 of the redesign plan. **Landed 2026-11-10:**
a dispatch that decides nothing is no longer run, and the match cache is
hashed; othello3's matching went 356 s → 104 s with identical output
([the 2026-11-10 sections](#what-each-dispatch-finds-changed-2026-11-10-tree-56c21a33-temporary-probe-since-removed)).
**Also 2026-11-10:** `find_best_matches` groups members by verdict key;
matching 104 s → 46 s, and othello3 compiles inside the sweep's cap for
the first time ([last section](#landed-2026-11-10-find_best_matches-collapses-by-verdict-key)).
Filed 2026-11-08 from
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
   (2026-11-09: most of this already exists. See the last section.)
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

   **Corrected 2026-11-09: "none on othello3" came from the class key.**
   These classes group members by their own `Sym`, not by what the
   candidates can tell apart. Grouped by verdict, othello3 does compress.
   See the last section.

   **Revised stop condition for step 3:** on sunfish, a class is matched
   at most once per (send, position) over the whole compile. If the
   per-class work is not then a small fraction of today's matching (it
   is 3–5× fewer classes times 10–25× fewer steps), stop and re-profile
   before step 4.
3. **Per-send state and class-level matching. Tried 2026-11-08; the stop
   condition was hit, and it is not in the tree.**

   **The design.** `IFA_CLASSMATCH`, in `pattern_match`. Per (send,
   `is_closure`, `partial`, visibility point) it keeps:
   - per position, class → representative;
   - per tuple of classes, the winners, from one full-matcher run on one
     current member per position (`incmatch_no_record`, arguments'
     `out`s swapped).

   A dispatch classifies its members, looks up every tuple of present
   classes, and builds filters from the current members of the winning
   classes. It records a cache entry so that later dispatches hit.

   **What was learned building it:**
   - **Attribute formals by position, not by identity.** A positional
     formal *q* is fed by actual *q*−1. Attributing by representative
     identity made every send where one CreationSet sits at two positions
     ambiguous (`self` and an argument sharing a contour, `x == x`): 60 K
     of sunfish's misses fell back permanently. Positional attribution
     left 0.
   - **Do not put a member's contents in the class key.** Keying tuples on
     their members' `AType`s, so nested pattern formals could be copied,
     made a tuple's class change every time its members grew: 8 052 962
     representative runs and 604 s on sunfish. Nested formals have to be
     a fallback, or need a key that does not move with growth.
   - **It is exact.** In check mode it answered every sunfish miss
     (168 362), with 0 mismatches against the full matcher, and the C was
     byte-identical.

   **Why it stopped: it is slower.**

   | | matching, default | matching, class matcher |
   | --- | --- | --- |
   | sunfish | 67.7 s | **270.9 s** |
   | plcfrs | 29.6 s | **181.5 s** |

   Its own cost on sunfish is 63 s: 784 642 representative runs (40.4 s,
   about 50 µs each, since a full `Matcher` on width-1 arguments is not
   cheap) and 22.8 s of assembly. The other ~208 s is outside it, and the
   likely cause is the cache it feeds:
   - it records an entry per answer, pinned to the arity epoch;
   - `match_cache_hit` scans and validates every entry of a send.

   Two more measured facts: 44 920 state resets when the selector's type
   changed, each discarding the send's tuples; and 243 K dispatches
   reached it against 168 K misses before, so its entries hit less
   often.

   **Re-profile before trying again.** Three things are now known to
   matter more than the class idea itself:
   - **The cache's scan.** Step 5's replacement of the `AType`-keyed
     cache has to come first, or with this. Bolted onto the old cache, the
     state makes hits worse.
   - **The cost of one representative run.** It must be a few
     microseconds, so a class-level match has to be its own code path,
     not a `Matcher` call on narrowed arguments.
   - **State keyed on the selector's type,** kept rather than reset.

   The patch is kept outside the tree. The prototype
   (`IFA_INCMATCH`, below) is still the committed reference.
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

## Revised direction, 2026-11-09: dispatch as a standing constraint

**The finding: most re-matching produces no new routing.** So the
algorithm to replace is the re-dispatch, not the matcher inside it. No
prototype of this exists yet. The percentages below are upper bounds on
what it can save.

### How the code behaves (tree `fee4d441`)

- **A `Match` is only a function, its per-formal filters, its visibility
  points and `is_partial`** (`pattern.h`). `cache_copy` discards the rest
  of `PMatch`. That answers the open question under direction 1 about what
  a `Match` carries.
- **The filters are snapshots.** `analyze_edge` calls
  `flow_var_type_permit(filtered, filter)` with an `AType` listing the
  members present at dispatch. The `flow_vars(actual, filtered)` edge is
  permanent, so a new member reaches the existing edge and stops at the
  permit until the send re-fires and re-matches.
- **Deferred connection mostly exists.** The worklist loop in
  `analyze_to_convergence` drains edges, then flow, then sends, and
  `in_send_worklist` limits a send to one firing per round. So the growth
  steps measured above are rounds, and a type crossing a call boundary
  waits one round for the re-match. Rounds, whole compile: sunfish 10 746
  over 31 passes (about 350 a pass), plcfrs 12 264 over 67, othello3
  1 504 over 15, chess 1 472 over 16.

### Measured (temporary probe on a scratch copy of `fee4d441`, not in the tree)

**Probe.** At each full match that returned a result, it recorded each
member's **verdict** at each formal: the set of functions whose filter
holds it. Members with one verdict at one formal form a **verdict class**.
A miss has **no new route** if every (formal, function, verdict) triple
was already seen. "Seen" is scoped two ways: the same send AVar in the
same pass, or the same send site (`send->var`) in any pass and any
contour. Matches that returned nothing are not counted (sunfish: 137 963
counted of about 168 000 misses). The time is the `pattern_match_full`
call, excluding the probe.

The four programs ran concurrently, so read the shares, not the seconds.

| program | miss time | no new route, same contour and pass | no new route, same site, any pass |
| --- | --- | --- | --- |
| sunfish | 57.8 s (137 963 misses) | 83% of time (99 219) | **93%** (126 587) |
| plcfrs | 26.7 s (158 396) | 77% (118 967) | **92%** (150 337) |
| othello3 | 286.9 s (1 126 001) | 22% (186 791) | **72%** (845 536) |
| chess | 0.67 s (8 289) | 6% (2 279) | 10% (5 044) |

Compression over wide misses: mean width of the widest position against
verdict classes there, and argument combinations (sum over formals of
log2 width against log2 classes, averaged).

| program | wide misses | width → verdict classes | combinations |
| --- | --- | --- | --- |
| sunfish (32–499) | 51 080 | 40.1 → 4.3 (step 2's `Sym` classes: 15.6) | 2^6.3 → 2^2.0 |
| plcfrs (32–499) | 76 062 | 50.7 → 4.7 (step 2: 12.9) | 2^6.9 → 2^2.2 |
| othello3 (500+) | 5 400 | 875 → 371, mixed | 2^10.0 → 2^5.0 |

othello3 looks bimodal: the arithmetic mean is 371 classes and the
geometric mean about 32. That fits sends whose hundreds of overrides are
real routes alongside sends with few routes, but the probe did not
separate them by selector.

The site-scoped column matters most for othello3 (72% against 22%), so
the state has to persist across passes and be shared across contours, as
decisions do.

### The alternatives

| option | what it does | verdict |
| --- | --- | --- |
| **A. Dispatch as a standing constraint** | Filters admit verdict classes instead of listing members. A new member of a routed class crosses the existing edge during flow. The send fires only for a new class. | **Chosen.** It removes the re-fires, so it also attacks rounds and hits. |
| **B. Shared dispatch table** | A memo from (candidates, tuple of verdict classes) to winners, across sends: multimethod dispatch compression (Dujardin et al.; Chambers and Chen's lookup DAGs). | A's slow path. Alone, it still re-fires and rebuilds O(*w*) filters. |
| **C. Per-send delta matching** (the 2026-11-08 plan) | Semi-naive evaluation at each send. | Exact, but the output stays O(*w*) a dispatch, and step 3 was 4–6× slower. Superseded. |
| **D. Width-threshold deferral** | Hold wide sends back. | Little left to gain: sends already follow flow. |
| **E. Bitset `AType`** | Wide set operations cost O(*w*/64). | Reconsider after A, which removes most of those operations. |
| **F. Hash the match cache** | Replaces the linear entry scan in `match_cache_hit`. | Small and exact. The fallback if A stalls. |
| **G. Shrink the unions** | Parametric summaries, or widening. | [GENERICS.md](../../GENERICS.md) estimates about 20% of FA on othello3. Widening is ruled out by the 057 directive. |

### Why A should reach the same fixed point

A member enters a filter only through a tuple of classes already present,
which is the decomposition the prototype checked (0 mismatches in 494 249
dispatches). So at quiescence the filter is the same set. Only the
arrival order changes.

### Steps, each with its stop condition

1. **Check mode for a static verdict key.** Compute each member's verdict
   class from what the matcher's tests read (`Sym`, static arity,
   `no_static_arity`, per candidate) and compare that partition with the
   matcher's on every miss, as `IFA_INCMATCH=2` does.
   **Stop if they disagree:** the class is not a function of the member
   alone, and A does not hold as designed.

   **Done 2026-11-09: they agree; the stop condition was not hit.**
   `IFA_VERDICTKEY=1` (`=2` recomputes every key instead of using the
   memo), in `pattern.cc`, off by default.

   **The key.** `Sym` and arity alone would not compress. So the key
   records, over the candidates the selector admits, which ones accept the
   member after the arity, `is_exact_match` and `is_this` tests and
   `verify_arg`. For each it adds the promotion, coercion and
   `reverify_filters` outcome. Where the accepting candidates' formal
   types differ, it adds what `subsumes_arg` reads (`isa`, and the types'
   relative order in the member's MRO). The member's `Sym` goes in only
   when a generic candidate accepts it.

   **Two checks.** *Partition:* members with one key at a position are in
   the same (function, formal) filters of the finished match. *Tuple:* a
   leaf's winners are a function of the tuple of keys, across dispatches,
   contours and passes of one send site. The tuple check sees only the
   representatives of `find_best_matches`' existing class collapse.

   **Result, all 77 corpus programs and 531 suite programs:** 2 219 504
   dispatches checked (every full match that returned a result),
   49 530 570 leaves, **0 partition and 0 tuple mismatches**. Skipped: 297
   dispatches with named actuals. Nine suite programs are refused before
   flow analysis. The memo-off run gives identical counts on chess,
   sunfish and plcfrs.

   **The check can fail.** A temporary mutation that dropped the accepting
   candidates from the key gave 40 464 partition mismatches on chess, and
   one that dropped only the `subsumes_arg` part was caught on a
   `class B(C, D)` / `class E(D, C)` program, which is the case that part
   exists for. Chess does not need that part; the mutation was not run on
   the rest of the corpus.

   **Compression**, at the widest position of a dispatch, widths 32–499
   except othello3's second row:

   | program | dispatches | width | keys | verdict classes | step 2's `Sym` classes |
   | --- | --- | --- | --- | --- | --- |
   | sunfish | 66 100 | 41.2 | 6.1 | 4.6 | 15.6 |
   | plcfrs | 104 717 | 52.3 | 4.8 | 4.5 | 12.9 |
   | chess | 853 | 34.4 | 5.0 | 4.2 | |
   | othello3 (500+) | 1 037 546 | 892.5 | 4.0 | 4.0 | 848.5 |

   The key is close to the matcher's own partition: it is finer at 49 942
   of the positions outside othello3 and 182 on othello3, and that costs
   only some compression. Distinct key tuples over the whole compile:
   sunfish 26 252, plcfrs 14 088, othello3 757 631.

   **Unreconciled:** the probe above reported othello3 at 875 → 371 over
   5 400 wide misses, and this check reports 892.5 → 4.0 over 1 037 546.
   Step 2's count of wide misses (1 043 735) agrees with this one. That
   probe is gone, so the difference in population is not explained, and
   371 should not be relied on.

   **Pattern formals do not arise in pyc.** Only the V frontend and the IR
   test parser set `is_pattern` (`skip_pattern=0` everywhere), so the
   exclusion in step 2 costs nothing on Python programs.
2. **Class-admitting filters for sends with no pattern formal.** Replace
   the snapshot permit with a class predicate (`restrict_pred` is the
   existing mechanism to look at), and fire a send only for a new class.
   Materialise `formal_filters` from current members at the end of the
   pass for the consumers that read them (clone, codegen), and apply
   `cs->ess.set_add` as a member crosses.
   **Stop if** sunfish's dispatches and rounds do not fall by a large
   factor: the percentages above did not translate, so re-profile.

   **Tried 2026-11-09: the stop condition was hit on rounds.** The first
   half is in the tree, off by default (`IFA_STANDING=1`; `=2` keeps
   today's dispatch and checks every prediction against the matcher).

   **What it does.** After a real dispatch a send remembers, per position,
   each present member's key and which (function, formal) filters that key
   reaches. If a later firing brings no new key, it builds the `Match` the
   matcher would have returned and hands it to the unchanged `make_AEdges`.
   So `formal_filters` stay current and no consumer changes, which replaces
   the plan's end-of-pass materialisation. It covers plain sends only: no
   closures, partial results or named actuals. The state lasts one flow
   run.

   **Exact.** 12 477 861 predictions over the corpus and suite, **0
   mismatches**; exit codes agree on all 608 programs. The check found one
   real bug: an argument's type is not monotone. When an abstract type
   arrives, `type_union` subsumes the constants it covers, so a member can
   leave. A departed member now falls back to the matcher (`shrunk`:
   468 746 of 58 M firings).

   **Generated C:** 574 of 577 byte-identical. Two differ only in
   temporary numbering. kmeanspp has two extra temp-and-move pairs with
   the same contour counts (323/1019); the cause is not established, and
   its only output is a wall-clock line, so running it shows nothing. No
   `check` sweep has been run with this on.

   **Time** (each compiled alone, `GC_INITIAL_HEAP_SIZE=8G`, see below):

   | program | FA, today | FA, standing | matching, today | matching, standing | full matches |
   | --- | --- | --- | --- | --- | --- |
   | sunfish | 39.3 s | 23.5 s | 22.3 s | 9.6 s | 137 963 → 92 309 |
   | plcfrs | 44.1 s | 33.3 s | 22.6 s | 12.0 s | 158 396 → 122 834 |
   | othello3 | 427.6 s | 398.6 s | 349.0 s | 328.7 s | not counted |

   Contour counts and passes are unchanged on all three. othello3 is still
   over 186's cap: 91% of its firings are refused by this path (14 988 881
   of 16 417 869).

   **Why the stop condition was hit.** The second half of the step ran
   every send the standing path can answer inside the edge and flow drain,
   so a crossing would not wait for a round. Rounds on sunfish went from
   10 746 to 10 735, so that change was deleted. A temporary probe
   classified what each round waited on (each round is counted once, in
   this order of precedence):

   | rounds | a send had to be matched because |
   | --- | --- |
   | 750 | first dispatch of the send in this run |
   | 5 321 | the standing path refuses it (closure or partial) |
   | 3 822 | only primitive sends were waiting |
   | 0 | a new verdict class, with nothing else waiting |
   | 842 | nothing had to be matched |

   So a verdict class never holds up a round on its own. Rounds are set by
   closure sends and primitive transfer functions, which this design does
   not touch. `PYC_DBG_WORK` now prints `cum_rounds`.

   **The garbage collector is the larger cost, and it was charged to
   matching.** sunfish, unchanged code: FA is 92.4 s by default and 39.1 s
   under `GC_DONT_GC=1`. `GC_INITIAL_HEAP_SIZE=8G` gives 40.3 s FA and
   wall time 140 s → 48 s, at 9.1 GB peak RSS. Every "matching" time
   earlier in this issue was measured with the default heap and includes
   collections triggered by the matcher's allocation. The shares remain
   useful, but the seconds overstate what a faster matcher can save.

   **Landed 2026-11-10: the compilers start with a 4 GiB heap**
   (`mem_init_heap` in `ifa/common/common.h`, called from `MEM_INIT`, at
   most an eighth of physical memory, `GC_INITIAL_HEAP_SIZE` still
   overrides; generated programs keep their own `MEM_INIT`). The
   collector runs only when the heap cannot satisfy an allocation, so a
   larger start means fewer collections, not a different trigger. Ladder,
   each compile alone, wall / FA / peak RSS:

   | starting heap | sunfish | plcfrs | hashlib_md5 (suite) |
   | --- | --- | --- | --- |
   | default | 138.9 s / 92.0 s / 2.0 GB | 66.2 s / 53.7 s / 0.96 GB | 1.56 s / 0.17 s / 0.21 GB |
   | 1 GiB | 113.2 s / 93.4 s / 2.0 GB | | |
   | 2 GiB | 63.5 s / 52.9 s / 2.5 GB | 49.7 s / 41.8 s / 2.4 GB | 1.04 s / 0.15 s / 0.43 GB |
   | 4 GiB | 49.6 s / 42.2 s / 4.6 GB | 46.8 s / 39.0 s / 4.6 GB | |
   | 8 GiB | 47.4 s / 39.7 s / 9.1 GB | | 1.04 s / 0.15 s / 0.43 GB |

   4 GiB is the knee: within 5% of 8 GiB at half the memory. The heap
   does not change the analysis: `compile -e GC_INITIAL_HEAP_SIZE=262144`
   against `check__default__5fa6edcc+a866b08f` (same binary) differs in
   none of compile rc, warnings, ess, css, container CSs or shapes on any
   of 77 programs. The `check` verdicts equal `eb635d2a`'s (othello3
   compile-fail, othello2 stdout); cs/shapes 2670 → 2664 comes from the
   commits since then, since both heaps give 2664. Lowest MemAvailable
   during the `check` sweep (compile -j32): 66.7 GB of 125 GB. Sweep compile
   phase: sunfish 176 s → 67 s, ac_encode 156 s → 71 s; othello3 stays at the
   400 s cap.
3. **The shared table (B) as the slow path,** on a per-class code path
   rather than a `Matcher` run. Step 3 above measured a `Matcher` on
   narrowed arguments at about 50 µs, which is too slow.
4. **Delete the prototype and the old cache** wherever the new state
   covers them, then decide about E on what remains.

### Risks

- **Byte-identical C may not hold.** Arrival order changes, so any
  decision that reads transient types can shift. The first place to look
  is edge-to-EntrySet binding. A difference is a finding under
  [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md),
  not automatically a failure, but it weakens the verification criterion
  above: if it happens, the test becomes unchanged `check` verdicts and
  contour counts explained case by case.
- **Pattern formals.** A tuple's class depends on nested contents, which
  keep growing (step 3's 8 052 962 representative runs), so those sends
  stay on the current matcher at first.
- **Class membership can change.** A flip in `no_static_arity`, a new
  instantiation wrapper or `add_patterns` must invalidate the classes
  they touch.

### Not measured

- How much of othello3's 266 s hit path goes away. No-new-route
  dispatches that hit the cache were not counted, so 186's stop condition
  (othello3 under the cap) is unproven.
- The fall in rounds, and so any saving in flow (27% of FA on sunfish).
- The cost of the per-member class lookup that replaces a dispatch.

## What each dispatch finds changed, 2026-11-10 (tree `56c21a33`, temporary probe, since removed)

**The question.** Before building a delta-driven dispatch: is the cost in
dispatches whose inputs did not change, and where do they come from? The
guess was the closure loop in `all_applications`. **That guess was wrong.**
The fan-out of a call send is 1 (sunfish: 196 of 2.9 M firings dispatch
more than once; othello3: none). The fan-out is in the `obj.attr`
primitive.

**Probe.** `IFA_DBG_DISPPROF`, in `function_dispatch`. Per (send,
`is_closure`, visibility point, argument AVars) it kept the arguments'
`out` pointers from the last dispatch in the same flow run, and classified
each dispatch:
- *first*: no dispatch of that tuple yet this run;
- *unchanged*: every `out` is the pointer it was;
- *changed*: one or more differ.

It timed the whole `function_dispatch` call and attributed it to the
caller: a call send, or `P_prim_period`'s two calls of `all_applications`
(the **member loop**, one call per receiver CreationSet whose member of
that name holds a method, and the **selector** dispatch). It also counted
the entries `match_cache_hit` scans. Each program compiled alone, 4 GiB
heap.

**othello3** (FA 454 s, matching 356 s):

| caller | class | cache | dispatches | time | avg width |
| --- | --- | --- | --- | --- | --- |
| member loop | unchanged | hit | 9 661 357 | **135.3 s** | 847 |
| member loop | first | miss | 861 043 | **68.3 s** | 891 |
| member loop | first | hit | 2 388 859 | **36.1 s** | 832 |
| member loop | changed | either | 345 926 | 12.4 s | 892 |
| call send | changed | miss | 92 144 | 62.5 s | 309 |
| call send | unchanged | hit | 1 821 098 | 17.8 s | 12 |
| call send | first | either | 221 851 | 11.9 s | |
| selector | all | | 709 099 | 12.4 s | |

- **The member loop is 252 s, 71% of matching.** It fired 32 423 times
  with a mean of 409 members (maximum 898), so 13.3 M of the 16.5 M
  dispatches are its.
- **Each of its dispatches is (one member's AVar, the WHOLE receiver
  union).** So a firing on a receiver of width *w* does *w* dispatches of
  width *w*.
- **The cache scan is the hit cost.** A lookup scans 2 921 entries on
  average (maximum 7 671), because every (member, receiver type) pair of a
  send is its own entry in one list. sunfish's mean is 7.1, plcfrs's 21.4.
- **74% of all dispatches find nothing changed** (12.2 M), for 157 s.

**sunfish** (FA 43.7 s, matching 25.2 s) is a different case:

| caller | class | cache | dispatches | time |
| --- | --- | --- | --- | --- |
| call send | changed | miss | 83 500 | **19.3 s** |
| call send | unchanged | hit | 2 489 389 | 2.7 s |
| call send | first | either | 126 153 | 1.4 s |
| member loop and selector | all | | 1 537 671 | 1.5 s |

93% of its dispatches find nothing changed, but they cost 3.6 s. Its time
is real re-matching of call sends as arguments grow, which is what
`IFA_STANDING` answers. Its member loop has 1.7 members on average.
plcfrs (first run of the probe, before the caller split) has sunfish's
shape: 13.7 s of 20 s in changed misses, 3.6 s in unchanged hits.

**What it means.**
- **Two diseases, not one.** sunfish and plcfrs pay for re-matching on
  growth. othello3 pays for the member loop's *w* × *w* and for the cache
  scan. Standing dispatch refuses the member loop's sends, since their
  result is a partial application; that is its 91% refused.
- **Next for othello3:** dispatch the member loop once per distinct member
  VALUE (the methods the members hold), not once per receiver CreationSet,
  and skip a dispatch whose inputs are the pointers they were. Open
  question first: what the member loop decides that the selector dispatch
  after it does not, since both run for a receiver whose member is a
  method.
- **Option F is not small on othello3.** Replacing the linear scan is
  worth most of the 198 s of hits there, and close to nothing elsewhere.

The probe is not in the tree.

## Landed 2026-11-10: a dispatch that decides nothing is not run

Three changes, all exact, all on by default (`pattern.cc`:
`MatchCache::index`, `dispatch_unchanged`, `dispatch_remember`; called from
`function_dispatch`).

1. **The match cache is hashed.** `match_cache_hit` finds its candidates
   by a hash of the flags, the visibility point and the top-level argument
   types, then compares and validates them as before. The linear scan and
   its move-to-front are gone.
2. **Unchanged inputs.** Per (send, argument AVars) a memo keeps the
   argument types and result of the last dispatch. A dispatch in the same
   flow run with the same arity epoch and pointer-identical types returns
   the remembered result and does nothing else. Not used when the last
   dispatch read a nested position (a pattern formal).
3. **The witness rule.** A dispatch (function value, receiver) whose
   matches are all partial makes no edges, so it decides only whether a
   match exists. That is monotone in the receiver, because the result
   decomposes over combinations of members. The memo keeps one receiver
   member from the match's filter; while the function value is the same
   type and that member is in the receiver, the answer is "a partial
   match" without matching. It holds across flow runs.

**Tried and dead: one dispatch per distinct member value.** othello3's
wide member read is `flip_table[...].go()`: 894 classes, each defining its
own `go`. Over 1 900 firings the member loop saw 1 543 283 members holding
1 428 217 distinct values, each a distinct function. There is nothing to
share, which is why the witness rule is per member.

**Time** (each compiled alone, 4 GiB heap):

| | FA | matching | wall |
| --- | --- | --- | --- |
| othello3, before | 454.2 s | 356.4 s | 666 s |
| othello3, 1 and 2 | 243.5 s | 157.0 s | 495 s |
| othello3, 1, 2 and 3 | 183.1 s | 104.3 s | 365 s |
| sunfish, 1 only (`IFA_DISPMEMO=0`) → all | 45.1 s → 35.4 s | 29.1 s → 20.8 s | 51.8 s → 42.0 s |
| plcfrs, 1 only → all | 37.5 s → 32.7 s | 18.6 s → 14.9 s | 44.8 s → 39.9 s |

**Exact.**
- othello3's and sunfish's generated C are byte-identical to the tree
  before.
- `IFA_DISPMEMO=2` runs the matcher behind every witness answer: 218 086
  answers over the suite and 1 253 818 over the corpus
  (`compile__IFA_DISPMEMO_2__56c21a33+56b2ee1a`), **0 mismatches**. No
  mutation was run to show this check can fail.
- `check__default__56c21a33+56b2ee1a` equals
  `check__default__5fa6edcc+a866b08f` in every column on 76 programs.
  othello3 differs only in now printing contour counts (4 883 ess, 11 500
  css): it finishes flow analysis inside the 400 s cap.
- `make test` is green.

**186's stop condition is not met yet.** Alone, othello3 compiles in 365 s.
In the sweep, at `-j32`, it still reaches the cap. Of the 365 s, 183 s is
flow analysis and about 180 s comes after it; that half has not been
profiled.

**What is left in othello3's matching (104 s).** Pass 1 is 45 s of it and
did not move with any of the three changes (45.2, 45.2, 45.3 s). The probe
above points at a few hundred very large matches: 572 call-send
dispatches where several arguments changed took 34 s, about 60 ms each.
Those are the two-argument sends (`__eq__`) over two wide unions, where
candidate selection enumerates member combinations. Step 1's verdict keys
compress those positions from about 890 members to 4 classes, so the
next step is to collapse `find_best_matches` by verdict key, where it
collapses by `Sym` today.

**Not answered:** what the member loop decides that the selector dispatch
after it does not. It no longer sets the cost, so it was left.

## Landed 2026-11-10: `find_best_matches` collapses by verdict key

**The cost was where the last section guessed.** A temporary probe (tree
`0b55bb75`, removed) timed each full match and its phases, on othello3
compiled alone:

| full matches, by time | count | total |
| --- | --- | --- |
| under 10 ms | 362 716 | 37.1 s |
| 10–100 ms | 33 | 1.2 s |
| 100 ms and over | 55 | **62.9 s** |

- **54 of the 55 are `a == b`**, from five source lines of `__pyc__`. One
  argument pair is 886–952 wide on both sides, and 10 or 11 `__eq__`
  candidates are live.
- **58.4 s of their 60.9 s is inside `find_best_matches`**: 701 406 to
  814 506 leaves a dispatch, 1 to 4 s each. That is about 838 × 838.
- **Why.** The multi-candidate collapse keyed a class on (votes,
  `cs->sym->type`, and the raw `cs->sym` when the candidates' formal types
  differ). `int.__eq__`, `str.__eq__` and `object.__eq__` differ, so every
  `Sym` was its own class, on both sides.

**The change** (`pattern.cc`: `vk_collapse_key`, `vk_core`; on by default,
`IFA_VKCOLLAPSE=0` restores the old key). The class key is now (votes,
verdict key). The verdict key is step 1's, computed over the candidates
that cover the member: per candidate its promotion, coercion, `verify_arg`
and `reverify_filters` outcome, and, where their formal types differ, what
`subsumes_arg` reads (`isa`, and the types' order in the member's MRO).
`vk_core` is that computation, shared with `IFA_VERDICTKEY`'s check. It is
memoised on (live candidates and formals, votes, `Sym`, aspect, arity).
The votes still come from the matcher's own filters. The paths that bailed
before still bail: a generic candidate, a pattern or missing formal, more
than 64 candidates.

**Time** (each compiled alone, 4 GiB heap):

| | FA | matching | pass 1 matching | wall |
| --- | --- | --- | --- | --- |
| othello3, before | 183.4 s | 105.9 s | 45.2 s | 367 s |
| othello3, after | 138.4 s | 45.9 s | 27.9 s | 348 s |
| sunfish, after | 33.6 s (was 35.4) | 20.6 s (was 20.8) | | 40.2 s |
| plcfrs, after | 32.2 s (was 32.7) | 13.8 s (was 14.9) | | 39.4 s |

Full matches of 100 ms and over on othello3: 55 → 1 (a 2.6 s closure
dispatch of `go` with 894 candidates, most likely a collection, since its
siblings take 10 ms). The `-v` flow figure rose (53 s → 77 s) as matching
fell, which fits collections moving from one timer to the other; it was
not investigated.

**Exact.**
- othello3's generated C is byte-identical to `0b55bb75`'s.
- `IFA_VKCOLLAPSE=2` runs the matcher again with the old key after every
  full match and compares the results: 194 464 over the suite (511
  programs) and 776 467 over the corpus
  (`compile__IFA_VKCOLLAPSE_2__0b55bb75+ae4581cf`, 76 programs; othello3
  doubles its matching in this mode and reaches the cap), **0
  mismatches**.
- **The check can fail.** A mutation that made the key a constant, leaving
  the votes, gave 2 mismatches on a `class B(C, D)` / `class E(D, C)`
  program. It gave none on the suite: no suite program has two members
  with the same votes that the leaf tells apart.
- `check__default__0b55bb75+ae4581cf` equals
  `check__default__56c21a33+56b2ee1a` on 76 programs. **othello3 now
  compiles in the sweep (352 s at `-j32`, cap 400 s), runs, and its stdout
  matches CPython:** `compile_fail` 1 → 0.
- `make test` is green.

**[186](186-FA-pass-one-confluence-through-shared-builtins.md)'s
verification is met, with 48 s to spare.** That margin is one sweep on one
machine.

**What is left.**
- **After flow analysis.** Alone, othello3 is 348 s, of which FA is 138 s.
  The other 210 s has not been profiled, and it is now the larger half.
- **othello3's matching, 46 s.** Pass 1 is 28 s. The probe's bands after
  the change: 15.4 s in 239 395 matches of 10–100 µs, 14.6 s in 2 343
  matches of 1–10 ms, 6.5 s in 45 576 matches of 0.1–1 ms. The 1–10 ms
  band is the 894-candidate `go` sends, where `find_all_matches` is most
  of each (7 of 10 ms).
- **sunfish and plcfrs** did not move. Their matching is re-matching of
  call sends on growth, which `IFA_STANDING` (off by default) answers.
