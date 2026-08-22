# 111 — every FA pass re-derives the whole program from bottom, so a pass that changes nothing costs full price

**Status:** open, filed 2026-08-22. **M1 in progress.**
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

- [ ] **M1 — measure the closure. No behaviour change.** After
  `run_split_stages()`, walk `forward` from the split ESs'/CSs' AVars;
  report `|closure| / |all AVars|` per pass. Corpus sweep. **This number
  decides whether the rest is worth building**: if closures are
  routinely near-total (one split landing on a shared builtin would do
  it), stop here.
- [ ] **M2 — differential harness, BEFORE any behaviour change.**
  `IFA_SELECTIVE=0|1`; run both on the same program; assert identical
  final `ess` / `css` / `violations` / emitted C. Corpus + both suites.
  Built first because the failure mode that matters is silent precision
  drift, not a crash.
- [ ] **M3 — selective clear.** Closure-scoped clear; seed the worklist
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
