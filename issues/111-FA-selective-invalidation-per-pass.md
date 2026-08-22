# 111 — every FA pass re-derives the whole program from bottom, so a pass that changes nothing costs full price

**Status:** open, filed 2026-08-22. **M1 DONE (green). M2 DONE (harness landed; found ifa/112). M3 STARTED — first cut runs but is WRONG; default-off, see below.**
**Affects:** `ifa/analysis/fa.cc` — `clear_results` / `clear_avar` /
`clear_es` / `clear_cs` / `clear_edge`, `analyze_to_convergence`,
`extend_analysis`, `run_split_stages` / `clear_splits`.

## Symptom

`shedskin_examples/hq2x` (1590 lines) compiles in **31.0 s**, of which
**27.6 s (89%) is flow analysis**. shedskin — written in Python —
translates the same program in **14.0 s**. pyc is 2.2x SLOWER on this
input despite being C++.

Per-pass (`pyc -v`):

    pass  1   0.16s  ess=129  css=888   viol=116
    pass  6   0.02s  ess=220  css=913   viol=27
    pass  7   3.71s  ess=292  css=2319  viol=1194   <- CS count 2.5x
    pass  8   3.52s  ess=600  css=2337  viol=66
    pass 11   3.35s  ess=615  css=2350  viol=0      <- converged
    pass 12   3.39s  ess=615  css=2351  viol=0
    pass 13   3.31s  ess=617  css=2352  viol=0
    pass 14   3.26s  ess=618  css=2352  viol=0
    pass 15   0.08s  ess=623  css=2362  viol=0

**Passes 11-14 cost 13.3 s — 48% of FA time — to add 3 entry sets and 2
creation sets with zero violations outstanding.** Verified against a
temporary `IFA_PASS_LIMIT` knob: stopping at pass 11, 12, or running to
100 produces **byte-identical** emitted C (25796 lines, 0 warnings). So
those passes buy nothing observable and cost ~6 s (20% of wall).

## Cause

`analyze_to_convergence()` calls `clear_results()` at the top of every
pass. That is not just a value reset: `clear_avar` clears `forward` and
`backward`, so the CONSTRAINT GRAPH is destroyed and rebuilt by
`add_es_constraints` from scratch. Each pass is therefore a full
from-bottom re-analysis of the entire program, whatever the splitter
actually changed.

Pass 15 is the existence proof of what the alternative costs: the same
~30 000 AVars, but only **79 dirty**, in **0.081 s** — 40x cheaper than
a from-bottom pass over the same graph.

### Why not just stop earlier

Considered and rejected. A blunt pass cap is wrong, measured:

    program   IFA_PASS_LIMIT=12          default              emitted C
    hq2x      26.4s rc=0  0 warnings     30.4s rc=0  0 warn   IDENTICAL
    amaze      2.8s rc=1  415 warnings    6.9s rc=0  13 warn  differs
    msp_ss     7.6s rc=1  310 warnings   15.7s rc=0 226 warn  differs
    rubik     10.3s rc=0  101 warnings   14.1s rc=0  91 warn  differs
    rdb        9.2s rc=0  172 warnings   17.6s rc=0 318 warn  differs

`amaze` and `msp_ss` FAIL TO COMPILE when truncated — they are still
mid-convergence (amaze never reaches 0 violations at all: 43 passes,
stalled at 15 from pass 34 on).

A narrower gate was also considered — `extend_analysis`'s `v == 0`
branch already has a productivity test, but it uses
`grew = (ess.n != prev || css.n != prev)`, a BOOLEAN, so +1 contour
scores the same as +308 (pass 8 grew ess by 308 for 3.5 s; pass 13 grew
it by 2 for 3.3 s). Making that threshold-based would work on hq2x but
is an arbitrary number papering over an invalid progress metric.

**Author's direction (2026-08-22): no artificial pass limits. The
analysis should recognise it is not making progress and terminate on its
own.** Selective invalidation is the better answer precisely because it
does not need to: a pass that changes almost nothing costs almost
nothing, so an extra pass to notice convergence stops being expensive.

## The invariant the fix rests on

Within a pass the analysis is a least fixed point from bottom —
`update_in` / `propagate_out_change` only ever GROW values. A split
REFINES, so a split EntrySet's AVars can legitimately end LOWER than
last pass. Hence:

1. A split ES's AVars **must** reset to bottom (the old value is now too
   wide).
2. Any AVar transitively reachable via `av->forward` from a reset AVar
   may hold a value DERIVED from that too-wide one, and since iteration
   only grows it would never shrink. **Must also reset.**
3. Everything NOT forward-reachable has unchanged inputs, so its old
   value is still its fixed point. **Safe to preserve.**

## Two pieces already exist

- **Seed set.** `clear_splits()` zeroes `es->split` / `cs->split` at the
  top of `run_split_stages()`; splitting sets them. So immediately after
  `run_split_stages()` returns, `{es : es->split}` u `{cs : cs->split}`
  is exactly what the pass changed. No new bookkeeping needed.
- **Flow graph.** `flow_var_to_var` maintains `a->forward` /
  `b->backward` for every value flow, including through edge args/rets.
  The closure is a walk over `forward` — and it is intact at END of
  pass, which is exactly when it is needed.

## Milestones

- [x] **M1 — measure the closure. No behaviour change.** DONE 2026-08-22, result below: median 0%, p90 3%, max 16%. After
  `run_split_stages()`, walk `forward` from the split ESs'/CSs' AVars;
  report `|closure| / |all AVars|` per pass. Corpus sweep. **This number
  decides whether the rest is worth building**: if closures are
  routinely near-total (one split landing on a shared builtin would do
  it), stop here.
- [x] **M2 — differential harness, BEFORE any behaviour change.** DONE
  2026-08-22: `ifa/tests/selective_diff.sh`, `IFA_SELECTIVE` flag
  (file-local in fa.cc, default 0, reported on the `PYC_DBG_OSC` line so
  a run can prove the flag reached FA). Result below — including
  ifa/issues/112, which the harness found immediately.
- [~] **M3 — selective clear. IN PROGRESS**, first cut committed default-off; three findings below. Closure-scoped clear; seed the worklist
  with affected ESs' edges instead of `top_edge`. Hazards, each needing
  an explicit answer:
  - `clear_edge` drops `e->args` / `e->rets` and `match->formal_filters`
    — edges among preserved ESs must keep theirs.
  - `clear_cs` drops `cs->defs` / `cs->ess` — per-pass CS state that
    preserved ESs still reference.
  - `fa->type_violations` is cleared in `initialize_pass()` and refilled
    by the pass; with partial re-analysis, violations owned by preserved
    AVars would silently vanish. Retain-and-merge, do not re-collect.
  - `type_world.cannonical_setters` and `entry_set_done` are cleared
    globally.
  - **ifa/issues/098 trap**: an ES unreached last pass holds values from
    whenever it WAS last reached (`foreach_var`'s comment documents this
    exact bug). Unreached-last-pass must be seeded dirty.
- [ ] **M4 — acceptance.** M2 harness green across the corpus; both
  suites; all five CI gates (CLAUDE.md "Change acceptance"); timing
  delta reported per program, not only in aggregate.

## Progress / results

*(append findings here as milestones land — dated, with the numbers)*

- **2026-08-22 — filed.** Baseline measurements above taken at
  `55cb0d2b`. Corpus compile-time ranking (successful compiles only):
  pygasus 37.3 s / 1713 loc, hq2x 30.1 s / 1590, rdb 18.4 s / 620,
  msp_ss 16.0 s / 1699, rubik 14.2 s / 979, amaze 7.5 s / 489.
  shedskin comparison: amaze pyc 7.6 s vs ss 54.2 s (pyc 7.2x faster);
  rdb pyc 18.3 s vs ss 30.2 s (pyc 1.6x faster); **hq2x pyc 31.0 s vs
  ss 14.0 s (pyc 2.2x slower)**. shedskin run from
  `/home/jplevyak/projects/shedskin` via PYTHONPATH — the installed
  `~/bin/shedskin` is broken (`ModuleNotFoundError`). Single runs, not
  averaged.

### 2026-08-22 — M1 result: GREEN (median 0%, p90 3%, max 16%)

`probe_invalidation_closure()` (fa.cc, `IFA_DBG_CLOSURE=1`) walks
`av->forward` from the seed after `run_split_stages()` and reports
`|closure| / |all AVars|`. Seven programs, 254 passes:

    program   passes  pct=0  median  max   worst pass (closure/all)
    amaze         43     27       0    9   p9:   5553/58441
    bh            50     46       0    8   p4:   3079/37779
    chull         56     52       0    7   p13:  2755/38278
    hq2x          15     11       0   16   p6:  15644/95854
    msp_ss        35     28       0    8   p9:  12730/157827
    rdb           29     16       0   11   p8:  18494/163304
    rubik         26     22       0   10   p8:  13190/127306

    TOTAL 254 passes; pct=0 on 202 (79%); median 0%; p90 3%; max 16%.

**79% of passes invalidate nothing at all, and no pass anywhere
exceeded 16%.** The feared case — a split landing on a shared builtin
and dirtying the world — does not occur in this corpus.

hq2x's expensive tail is the sharpest illustration:

    pass 11  3.35s  closure=6      of 99994 AVars
    pass 12  3.39s  closure=3      of 100045
    pass 13  3.31s  closure=0      of 100049
    pass 14  3.26s  closure=0      of 100837

Four passes, 13.3 s, to re-derive ~100 000 AVars when **at most six**
could have changed.

### M1 also corrected the seed set

The plan above asserted `{es : es->split}` was the full seed. **It is
not**, and the probe caught it: hq2x passes 8-13 reported ZERO split
marks while still costing a full pass, and `reanalyze()` was not even
reached (it is only consulted when `extend_analysis` returns 0). The
missing path is `split_edges`, which sets `again = 1` on a **redispatch**
(`ee->to != old`) — retargeting an existing edge to an ALREADY-EXISTING
EntrySet. No contour is minted, so nothing is marked.

Fixed by recording both the old and the new target in `set_entry_set()`,
the single point where `e->to` changes, into `fa_pass_retargeted`
(cleared by `clear_splits()` beside the split marks). M3 must seed from
this vec as well as from `->split`, or it will preserve state that a
redispatch invalidated — a silent precision bug of exactly the ifa/098
family.

### 2026-08-22 — M2: harness landed, and the control earned its keep

`ifa/tests/selective_diff.sh [-q] <prog>... | --corpus` compiles each
program under `IFA_SELECTIVE=0` and `=1` and compares exit code, FA's
converged state (`final_pass` / `violations` / `ess` / `css` via
`PYC_DBG_OSC`) and the emitted C byte for byte.

`IFA_SELECTIVE` is a **no-op at M2** by design, so the run is a control:
any divergence means the COMPILER is not reproducible, which would make
this harness useless at M3.

**It found exactly that on its first corpus run.** `msp_ss` emitted
different C on two identical invocations — same FA state
(`violations=454 ess=891 css=2726`), same 40211 lines, but temps
renumbered and one getter relocated between functions. Filed as
[112](112-CGEN-nondeterministic-emitted-c.md).

That drove two design corrections worth recording, because the naive
harness would have been actively misleading at M3:

1. **A determinism control is mandatory, not nice-to-have.** Each
   program is compiled under sel=0 TWICE. Without it every unstable
   program reads as a regression caused by the change under test, and
   points the investigation at the wrong code.
2. **One control run is not enough — 112 is INTERMITTENT.** The first
   version reported msp_ss as a genuine divergence because its two
   sel=0 runs happened to agree. The harness now ESCALATES: on a C
   mismatch it runs two further sel=0 compiles before concluding, and
   only calls it a divergence if all four agree with each other. Cost
   is paid only when there is something to explain.

Unstable programs are reported `UNSTABLE` and compared on FA state
alone, which is reproducible for msp_ss and is the property M3 most
needs to preserve anyway.

### 2026-08-22 — M3 first cut: runs, is wrong, and the harness said so immediately

`clear_results_selective()` in fa.cc, behind `IFA_SELECTIVE=1`
(**default 0 — the shipped path is untouched**, all five CI gates green,
294 passed / 0 failed both backends).

Shape: `probe_invalidation_closure()` now stores its closure in
`fa_invalidate_avars` at END of pass (it must be there: `clear_avar`
destroys `forward`, so by the time the next pass wants the closure the
graph it comes from is gone). `analyze_to_convergence` then calls
`clear_results_selective()` instead of `clear_results()`, falling back
to the full reset whenever it declines.

**It does not work yet.** `selective_diff.sh collatz sieve loop`:
3 of 3 diverge.

    sieve  sel=0: final_pass=8  violations=0    ess=243 css=863
           sel=1: final_pass=7  violations=864  ess=112 css=815
    loop   sel=0: final_pass=26 violations=0    ess=353 css=1128
           sel=1: final_pass=6  violations=4    ess=53  css=599   (+ SIGSEGV)

The signature is consistent and diagnostic: **selective terminates far
too early with far fewer entry sets and more violations.** Preserving
state changes what "converged" means — the worklists drain because
nothing is dirty, the splitter sees no progress, and the outer loop
stops long before the analysis is actually done. This is a design
problem, not a small bug.

### Three things learned, each a constraint on the next attempt

1. **Do not clear edge or ES containers — only AVars and constraint
   state.** The first cut called `clear_edge`/`clear_es`, which empty
   `e->args`, `e->rets`, `out_edges`, `creates`. Only the edge-CREATION
   path refills those (`set_entry_set`'s `fill_rets`, `get_AEdges`), and
   enqueueing a cleared edge does not go through it: `analyze_edge`
   dereferenced an emptied `e->rets` against a non-empty
   `pnode->lvals` and segfaulted (fa.cc:3606). The values in those
   containers ARE AVars, which the closure clears individually, so the
   structure can and must be left alone.

2. **The top edge must still be enqueued.** Seeding only the affected
   edges leaves the STRUCTURE unrebuilt — `fa->ess`, `es->out_edges`,
   `es->creates` are all repopulated by that traversal — and
   `build_call_dominators` then walked a call graph with null nodes and
   segfaulted (dom.cc:19). The saving was never meant to come from
   skipping the walk: it comes from what the walk finds already done
   (a preserved ES keeps `live_pnodes`, so `add_es_constraints` is a
   no-op, and its AVars keep their values, so `update_in` changes
   nothing). hq2x pass 15 is the proof that re-walking a settled contour
   is cheap: ~30 000 AVars, 79 dirty, 0.081 s.

3. **`live_pnodes` must be cleared for a dirty ES.** `clear_avar` drops
   the flow edges and `add_es_constraints` is the only thing that
   rebuilds them — and it is a no-op for an ES that still believes its
   pnodes are live.

### Where to resume

The early-termination signature points at CreationSet state, which the
first cut barely touches: it resets only `closure_used` and
`unknown_vars`, while `clear_cs` also clears `cs->defs` and `cs->ess` —
a CS's membership in contours. Preserving those across a split leaves a
CS claiming membership in a contour it no longer belongs to, which
would plausibly both suppress new splits and under-report reachable
ESs. That is the first thing to test.

Also unresolved: the interaction with `extend_analysis`'s progress
detection. With state preserved, `grew`/`rederive_churn` no longer mean
what they meant, and the outer loop's termination has to be re-derived
rather than assumed.

## Verification plan

- hq2x FA time drops materially with emitted C byte-identical to today.
- M2's differential harness reports zero divergence corpus-wide.
- Both suites unchanged; all five CI gates green.
- No regression in per-program compile time anywhere (report the full
  distribution, not the mean — a mechanism that helps hq2x and hurts
  twenty small programs is not a win).

## What this unblocks

Compile times on the analysis-heavy end of the corpus, where pyc
currently loses to a Python implementation. Also removes the incentive
to bound pass counts artificially: with per-pass cost proportional to
what changed, letting the analysis run until it genuinely converges
stops being expensive.

## Related

- [074](074-FA-cross-pass-oscillation-plan.md), [101](101-FA-first-time-forever-splitting.md)
  — non-convergence. Distinct problem: `amaze`'s profile is 43 CHEAP
  passes that never converge, which selective invalidation does not
  help. This issue is about the cost of a pass, not the number of them.
- [098](098-FA-per-pass-reset-scoped-to-reachable-set.md) — why the
  reset is where it is, and the stale-AVar bug that shaped it. Fixed
  2026-08-12 (one follow-on left open), and directly adjacent: 098
  scoped the reset to the REACHABLE set; this issue scopes it to the
  CHANGED set. Its post-mortem is the best available guide to what goes
  wrong when the scope is too narrow.
