# 186 — pass 1 pushes a whole-program union through the shared builtins (othello3)

**Status:** open. Diagnosed 2026-10-08. othello3 now compiles and matches
CPython (2026-10-09), in 994 s, nearly all of it FA: still over the
sweep's 400 s cap. The
within-pass stall guard that failed it is deleted (see "The stall guard
was wrong").

## Symptom

othello3 does not compile. Before 2026-10-08 the ifa/057 stall guard
failed it after ~120 s of pass 1, reporting non-convergence. With the
guard fixed, pass 1 ran for 50 minutes (7.2 GB resident) without
finishing. othello3's own README says shedskin takes about an hour on it.

## What the program is

Generated code: 830 `Flip` subclasses, 64 `Put` subclasses, 1,848 stores
into `flip_table`, 256 `.go()` dispatch sites. About 2,000 CreationSets in
all, counting ints, strs and tuples.

## Mechanism

Pass 1 starts minimal: one EntrySet per function. Every shared builtin
contour therefore merges every caller's values. Measured holders of the
same ~2,039-member union (ints, strs, tuples, every flip and put class):

- `__list_iter__.__next__`'s `x`, the single list-iterator creation point
  inside `list.__iter__`;
- `len`'s `x`, `join`'s `seq`, `__eq__` / `__ne__`'s `x`;
- `str_state`'s `digit`;
- the comprehension variable `h` in `for h in human_moves`, which only
  ever iterates strings. `__next__`'s merged return flows back into every
  loop.

The union then spreads to tens of thousands of AVars one CreationSet at a
time, so each AVar is rebuilt about 2,000 times: **~105 million AVar type
changes averaging ~900 CreationSets each in 32 minutes.** The demand that
would separate the callers is evaluated at quiescence, after the pass, and
the pass never gets there.

Not the cause: start-merged list CreationSets. With `PYC_CSDCPA1=0` the
same unions form and pass 1 still stalls.

## Fixed on the way (2026-10-08)

1. **Dispatch was cubic in the receiver union** (`ifa/if1/pattern.cc`),
   found by sampling stacks under gdb.
   - The multi-candidate votes collapse was gated on `nmat`, which stops
     counting at 2, so it ran with all 800 `go` candidates. Its vote mask
     `1ull << ci` is undefined past bit 63, so receivers with different
     candidates could be merged. It is now gated on the real candidate
     count (`ncand <= 64`), and above 8 candidates the full enumeration
     indexes candidates by receiver CreationSet.
   - `clear_matches` swept every candidate at every leaf; it now clears
     only the candidates the leaf touched.
   - `reverify_filters` ran at every leaf; it now runs once per
     `pattern_match`, with the same result.

   A synthetic othello3-shaped program with 800 classes: pass-1 match time
   76 s -> 10.5 s (whole pass 82 s -> 16 s), output equal to CPython at
   every size.
2. **The ifa/057 stall guard never saw progress within a pass**
   (`fa.cc`). It read `fa->ess.n`, which `collect_results` rebuilds only at
   the end of a pass, so it was a flat 120 s cap on any pass
   ([closed/057](closed/057-sorted-tolist-fa-nonconvergence.md) had already
   noted the reading was misleading). It now counts EntrySets as
   `set_entry_set` mints them (`fa_es_minted`).

## How shedskin handles it

From its source (`~/projects/shedskin/shedskin/infer.py`), not from a run.
shedskin never forms the union, by three mechanisms:

1. **Eager CPA.** `cpa()` makes a template per tuple of concrete argument
   types at every call (`create_template`, `func.cp[dcpa][c]`), so `len`,
   `__eq__` and `__next__` get one template per receiver type. The list
   iterator follows: `pyiter.__iter__` is `return __iter(self.unit)`,
   templated per list contour, and its allocation gets a contour per
   template (`alloc_info` keys on function x cartesian product x node).
2. **`CPA_LIMIT` defers big unions.** It starts at 10. A call whose
   functions x argument-type combinations exceed it is **not connected**
   (`cpa_limited = True; return`); after convergence the limit doubles and
   the analysis restarts. The 830-receiver `.go()` sites wait for 7
   doublings.
3. **Incremental program.** 5 user functions and 1 allocation site per
   round (`INCREMENTAL_FUNCS`, `INCREMENTAL_ALLOCS`), to a fixed point in
   between. That, with the restarts, is the hour its README reports.

Only 2 and 3 carry over. 1 splits on a fact, not a demand, and its
allocation identity is the (site x contour) product `PYC_CSDCPA1=2`
replaced (ifa/146 already retired `PYC_CPA` on the same ground). 2 and 3
decide WHEN a value reaches its readers, not what contours exist. In pyc,
deferring a call still pours the union into one minimal contour once it
is connected; the gain is that it arrives once, settled, instead of
~2,000 times.

## The deferral prototype, and what it found (2026-10-09)

Shedskin's mechanisms 2 and 3 are scheduling, so the prototype took the
scheduling half: **settle a growing union before readers see it.**

Propagation was eager and depth-first: `propagate_out_change` recursed
through `forward` on every change, so each CreationSet a union gained
re-walked the whole downstream graph. Now an AVar whose `out` changes is
queued once (`flow_worklist`, coalescing on `in_flow_worklist`), and
`analyze_to_convergence` drains the queue after the edges and before the
sends. Each AVar pushes whatever its `out` is by then, and dispatch sees
settled unions.

That alone cut out-changes 57x (pass 1: 105M -> 1.85M) and did not finish
pass 1: the stall guard stopped it after 420k edges. Sampling then found
the cost in three places, each the size of the union per step:

| where | cost | fix |
| --- | --- | --- |
| `type_union` (`fa_lattice.cc`) | a flow edge re-asserted onto an AVar that already holds its source still built two diffs and a result, each ~2,000 wide, and memoized the pair; 8 of 12 samples were full GCs triggered by these | subset fast path: if one canonical operand contains the other, it is the union |
| `collect_argument_type_violations` | `t = type_diff(t, filtered)` once per dispatch edge (up to 800 per `.go()` site), each step a new canonical AType; ~1,200 s between passes 1 and 2 | `type_diff_all`: one filtering pass, canonicalized once |
| `get_all_args` (`pattern.cc`), the match-cache lookup | canonicalized the child position of every member of every CS in the union, then discarded nearly all | ask once per member index; recurse only for wanted indices |

Then codegen ran the heap out: `cg_note_blind_cast` recorded one
layout obligation per union member per site, with no dedup (repeated
256 MB `Vec` growth, then a wrapped heap-expand size and SIGSEGV). An
obligation is a function of `(cast_to, actual, slot)`, so it is now
recorded once: 692,110 obligations, 0 violations.

Measured on othello3 (stall guard disabled for measurement):

| | pass 1 | pass 2 | pass 3 | FA total | compile |
| --- | --- | --- | --- | --- | --- |
| before | never finished in 50 min | | | | |
| deferral only | stall guard at 420k edges | | | | |
| + subset union | 342 s | | | | |
| + one-pass diff | 351 s | 276 s | 251 s | | |
| + per-index `get_all_args` | 184 s | 132 s | 91 s | ~860 s, 13 passes | 1,554 s, rc 0 |
| all but deferral | 197 s | 152 s | 112 s | | |

FA converges: pass 13 repeats pass 12 (5,096 EntrySets, same edge
count). The binary's output matches CPython's except the `moves/sec`
timing line (1.6M vs 0.77M). No warnings.

**Attribution.** Deferral is worth 7-19% of FA time on top of the other
three, despite the 57x fewer out-changes: once each change was cheap, the
count stopped mattering much. It changes the fixed point slightly
(pass 1 EntrySets 3,819 vs 3,821), as any reordering of an
order-sensitive union does (ifa/147).

**The rest of the 1,554 s** is ~690 s after FA: clone, 37 MB of
emitted C, and the C compile. Profiled below.

## After FA (profiled 2026-10-09)

One run of c5f9d7f6 with temporary phase timers, 1,500 s wall:

| phase | s |
| --- | --- |
| FA | 847 |
| clone | 310 |
| mark_live_code | 25 |
| write C | 38 |
| clang++ | 262 |
| everything else | < 1 |

**clone: `cs_member_merge_enlarges`** (`clone.cc`, ifa/153), 27 of 28
stack samples, a third of them in GC. `determine_basic_clones` calls it
for every pair of same-sym CreationSets and every member. Each call
builds both members' concrete class sets with linear dedup, so a member
holding the ~2,000-class union costs O(n^2) per pair and allocates
throughout. The answer depends only on the two members' `out` ATypes,
which are hash-consed: equal pointers mean "no enlargement", and a
per-AType sorted class set makes the rest linear. Not yet done.

**clang++: `-g` at `-O2`.** Timing the same 37 MB `.c` file:

| flags | s | peak RSS |
| --- | --- | --- |
| `-g -O2` (what `Makefile.cg` does) | 263 | 23 GB |
| `-O2 -gline-tables-only` | 122 | 1.6 GB |
| `-O2` | 119 | 1.2 GB |
| `-O1` | 80 | 1.2 GB |
| `-g -O0` | 50 | 1.7 GB |

The compile's 23 GB peak, earlier attributed to pyc, is clang's
variable-location debug info. The code it is fed is the deeper cost: each
of the 64 `put_*::go` methods is ~10,160 lines of C, because each
`.go()` site over the 830 Flip classes is emitted as an 830-arm
classtag `if` chain (`cg.cc`, the poly-dispatch emitter) whose arms all
call the same slot `e13` with the same signature, differing only in the
struct cast. The blind-cast obligations already prove every member
agrees with each arm's layout up to that slot (692,110 checked, 0
violations), so when every arm has the same slot and signature the chain
is one slot call.

### Fixed (2026-10-09)

1. **One slot call.** The poly-dispatch emitter now emits a single slot
   call when every classtag arm has the same slot, function-pointer type
   and arguments, and every class agrees with the first class's layout up
   to that slot (`cg_layout_mismatch`, factored out of
   `cg_check_layout_contract`, so a disagreeing class keeps the chain).
   Each class's cast is still recorded as an obligation, and its slot as
   used.
2. **`cs_member_merge_enlarges` is linear.** Equal `out` ATypes answer
   without looking; otherwise each AType's sorted concrete class set is
   computed once per `determine_basic_clones`. Same answers.
3. **No `-g` unless asked.** `Makefile.cg` passed `-g` unconditionally,
   though `pyc -g` (off by default) already adds it through `DEBUG=1`.

othello3, alone, on c5f9d7f6 plus these:

| | before | after |
| --- | --- | --- |
| compile | 1,500 s | 994 s |
| peak RSS | 23 GB | 3.4 GB |
| emitted C | 37.6 MB | 5.8 MB |
| binary | 12 MB | 1.0 MB |
| layout obligations | 692,110 | 896 |
| `moves/sec` | 1.6M | 80M (CPython 0.77M) |

Output matches CPython except that timing line. FA's ~850 s is now
almost all of the compile, so the 400 s cap is FA's to meet.

`make test` green. Sweep `check__default__c5f9d7f6+47b658d3` against
`95f22d8d+5b37a30a`: every verdict, warning count, ESS and CSS identical
over 77 programs; othello3 still 124 at the cap.

**Sweep** `check__default__95f22d8d+5b37a30a` (the four fixes plus
deferral, guard unchanged) against `check__default__7176ad62+6c65e828`:
no compile, run or stdout verdict changes, except othello3 1 -> 124,
because the guard no longer stops it before the 400 s cap. EntrySets
+0.22% (31,166 -> 31,235) and CreationSets +0.24% over 76 programs, both
directions (pygmy +47 / +269, mastermind2 +20 ESs; sudoku5 -6, life -4),
consistent with a reordering of an order-sensitive union and not a
precision change. Warnings -2 in softrender, -1 in tictactoe.

## The stall guard was wrong

closed/057's root cause was UNBOUNDED ENTRYSET MINTING (`Fun::ess`
growing without limit). The within-pass guard counts minting as progress,
so it cannot catch that runaway; what it does catch is a long pass that
mints nothing. othello3's pass 2 reaches its last EntrySet (4,830) at
+2 s and ends at +136 s, so the guard fails a converging pass. It is also
wall-clock, so its verdict depends on the machine. Its first version read
`fa->ess.n`, frozen within a pass, so it was a flat 120 s cap: neither
version ever measured what its comment claims. Nothing in the test suite
depends on it (`make test` and the IR phases never trip it).

Deleted 2026-10-09, with `fa_es_minted`. A within-pass runaway now runs
to the caller's timeout instead of failing with a diagnostic; the
cross-pass guards (`IFA_STALL_LIMIT`, the pass cap) are unaffected.

## Directions (not chosen)

This section predates the prototype. Its premise, that the number of
updates was the problem and making each one cheaper would not help, was
wrong: the per-update costs above were most of it, and deferral, which
attacks the count, was worth 7-19%. What remains is the confluence itself
and FA's ~850 s against a 400 s cap (the post-FA time is fixed above).

- **Remove the confluence.** The list iterator is the most visible one,
  but `len`, `join` and the comparisons show the same pattern, so a
  fix specific to `__list_iter__` is not enough. The question is why a
  minimal pass-1 contour of a builtin must carry every caller's union
  before any demand can be asked.
- **Settle a growing union before readers see it.** Order the worklist so
  an AVar's type stops growing before its consumers are re-run, so each
  AVar updates once rather than ~2,000 times.

Stop condition for either: if pass 1 on othello3 still exceeds 400 s with
the union's holders unchanged, the model (update count dominates) is
wrong, and the per-update cost needs re-profiling.

## Verification

- `./pyc shedskin_examples/othello3/othello3.py` compiles under the
  sweep's cap and its output matches CPython.
- `make test` green; a `check` sweep with no outcome regressions.
