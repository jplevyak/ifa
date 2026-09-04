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
[112](closed/112-CGEN-nondeterministic-emitted-c.md).

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

#### A note on the corpus harness run of 2026-08-22 — DISCARD IT

A `--corpus` run was started during M2 and reported
`compared 72 / diverged 34 / unstable 1`. **Those numbers are void.**
The run was launched while `IFA_SELECTIVE` was still a no-op, and `pyc`
was rebuilt underneath it several times while M3 was being written, so
the later programs were compared against a half-implemented selective
path. The 34 divergences are M3's own breakage, not an M2 result.

Recorded because a stale log with a plausible-looking summary is worse
than no log: the only defensible reading is that the pre-M3 portion
agreed and found `msp_ss` unstable. A clean corpus control needs a
stable binary, which means re-running it whenever M3 is next touched.

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

### 2026-08-22 — second cut: the CreationSet hypothesis was wrong, and what replaced it

The recorded next suspect (preserved `cs->defs` / `cs->ess`) was tested
and is **not** the cause on its own — but testing it produced the
structural insight the first cut lacked:

**Per-pass state divides by WHAT REBUILDS IT, not by affected vs
unaffected.**

- **Structural** bookkeeping — `cs->defs`, `cs->ess`, `es->out_edges`,
  `es->creates`, backedges — is rebuilt by the top-edge TRAVERSAL,
  which still runs in full. So it must be cleared in full: left alone
  it does not go stale, it **accumulates**, because the traversal
  appends. Clearing it is cheap (container resets, not a fixed point).
- **Value** state — AVar `in`/`out`/`gen`/`forward`/`backward` — is
  rebuilt by PROPAGATION. That is the expensive part and the only part
  worth scoping.

That split is implemented and is the right shape. It is still not
sufficient.

### The blocker: setter equivalence classes cannot be scoped

`same_eq_classes` (fa.cc) asserts every AVar in a `Setters` set carries
a `setter_class`. `setter_class` is assigned by a SPLITTER STAGE, and
`Setters` objects are interned in the global
`type_world.cannonical_setters`. Under a full reset every AVar starts
classless and the stage assigns them all; under a scoped reset a
preserved AVar that is never re-derived never gets one, and a surviving
interned `Setters` still names it. Clearing the table alone fails the
assert from one side, resetting the per-AVar pair alone fails it from
the other; doing both is necessary and STILL not sufficient.

### The real obstacle, and the design change it implies

AVar state has **at least four distinct populations**, and
`clear_results` is the only code that knows all of them:

1. `Var::avars` reached via `allsyms` + `pdb->funs` (`foreach_var`)
2. `cs->vars` (`clear_cs`)
3. `e->filtered_args` (`clear_edge`)
4. the CreationSet ELEMENT AVar via `get_element_avar` (`clear_cs`'s
   `added_element_var` line)

Each was found the same way: implement, hit the assert, discover
another population. Population 4 was the one the diagnostic finally
named — an unnamed AVar on a contour outside the reached set.

**So the next attempt should not write a parallel enumeration at all.**
Refactor `clear_results` to take a predicate — "clear this AVar?" —
so there is exactly ONE walk over the four populations and the
selective path cannot drift from it. Every failure above is a
consequence of maintaining a second, incomplete copy of that walk. That
is the concrete change to make before trying again.

### 2026-08-23 — third cut: the predicate refactor works; the setter machinery blocks

The recorded next step is **done and was right**: `clear_avar` and
`clear_es` now take the scoping decision themselves, via
`fa_clear_only` / `fa_rebuild_only`, and `clear_results_selective()`
sets those and calls **the same `clear_results()` the full reset uses**.
One enumeration, so the selective path can no longer miss an AVar
population — which was the previous cut's whole failure mode. That
part should be kept.

It exposed a harder blocker underneath.

### The blocker, stated precisely

`setter_class` is assigned **only** by `split_eq_class`, driven by the
splitter over what the propagation re-derived. `same_eq_classes`
asserts every AVar in a `Setters` set has one. Selective invalidation
deliberately re-derives less, so the full reset's invariant —
*everything in a Setters set was classed this pass* — no longer holds.

**Setter-set membership is determined DURING a pass, while the
preserve/clear decision must be made BEFORE it.** That is the
irreducible conflict, and it defeats every scoping strategy:

| approach | outcome |
|---|---|
| Zero `setters`/`setter_class` on all AVars | assert: preserved AVars land in sets rebuilt this pass, unclassed |
| Preserve both, and the `cannonical_setters` interning table with them | assert: *cleared* AVars re-enter sets unclassed |
| Make `same_eq_classes` answer `false` for unclassed (conservative) | no assert, but it refuses EntrySet merges — contours grow without bound and `collatz` never converges (120 s timeout) |
| Pre-add every live `Setters` member to the closure | assert: membership is decided during the next pass, so a closure computed at the end of this one cannot cover it |

The conservative variant was reverted deliberately: a loud assert is
more diagnosable than silent non-convergence, and the note stays in
`same_eq_classes` so the next person does not re-try it.

### 2026-08-23 — option 2 tried (widen setter-stage coverage): does NOT fix it

Two forms, both implemented and both still asserting:

- **Backward invalidation.** Clearing AVar X invalidates the setter
  state of every AVar whose set NAMES X — a backward step the forward
  closure cannot supply. Implemented via the new `foreach_avar` helper.
  Still asserts.
- **Literal option 2**: run `compute_setters(.., AKIND_TYPE)` over
  EVERY CS-contoured AVar instead of just this pass's confluences
  (kept in the tree, gated on `ifa_selective`). Still asserts.

Diagnostic from the surviving case: `av#5979`, `setters=(nil)`, no
class, appearing as a MEMBER of another AVar's set at pass 10. So it is
neither reached by the widened classing nor covered by the backward
invalidation — the coupling is not a coverage problem, which is what
option 2 assumed.

**Six approaches have now failed for the same underlying reason.** Two
things follow, and the second matters more than the first:

1. Option 2 is closed. Do not re-try coverage widening.
2. The remaining option (1: class lazily in `same_eq_classes` — compute
   rather than refuse or assert) is the only one that attacks the
   actual conflict, since it removes the dependency on *when* an AVar
   was classed. It is also the one that changes shared code rather than
   the selective path, so it needs its own justification independent of
   this issue.

### A note on cost, before anyone resumes

M1 said closures are tiny (median 0%, p90 3%) and hq2x's four wasted
passes touch at most 6 AVars of ~100 000. That upside is real and
unchanged. But the six failures are all in ONE subsystem — setter
equivalence — and none of them is about the closure being wrong. A
reasonable reading is that the value/structure split this issue is
built on is sound, and setter equivalence is simply not expressible in
it as written. If option 1 also fails, the honest conclusion is that
selective invalidation needs the setter machinery reworked FIRST, as a
separate piece of work, rather than more attempts here.

### 2026-08-23 — option 1 tried (lazy classing): past the assert, but not correct

`same_eq_classes` now calls `recompute_eq_classes` on demand when a
member is unclassed, instead of asserting (gated on `ifa_selective`,
kept in the tree). That is the right primitive — it is exactly what
assigns classes to unclassed members and repartitions around them, so
it computes the same KIND of answer the stage would, rather than
inventing one.

The assert is gone. The results are still wrong:

    t1.py (4 lines)   converges, but 8 "mixed basic types" warnings vs 1
    collatz           does not converge -- 18 passes and still going at
                      150 s, where sel=0 finishes in 6. cleared_avars
                      oscillates 182 / 3175 / 850 / 1526: churn, not
                      progress.

**The reason connects option 1 to option 2 and closes both.**
`recompute_eq_classes(both)` sees TWO Setters sets; the stage sees the
whole collection at once. Equivalence classing is a global partition,
so computing it from a local pair gives a different partition, which
gives different split decisions, which gives churn and precision loss.
Making the lazy version faithful means giving it the full collection —
which is option 2, already closed.

### Conclusion: stop, and rework the setter machinery first

Seven approaches, one cause. The failures are not about the closure
being wrong — M1's measurement stands (median 0%, p90 3%, max 16%) and
the value/structure split is sound. They are all about one subsystem:

> **`setter_class` is a global partition, computed in a batch over a
> whole pass's confluences, and every member of every live `Setters`
> set is required to carry one. That cannot be scoped (the preserve
> decision precedes membership), deferred (local classing diverges
> from global), or widened (coverage is not the gap).**

So selective invalidation is blocked on a prerequisite, not on its own
design. Filed as
**[113](113-FA-setter-equivalence-is-a-global-batch-partition.md)** —
making setter equivalence incremental, or per-contour, or otherwise
independent of "everything classed in one batch this pass". That is FA
redesign, not an optimisation, and it should be justified on its own
terms rather than merely as a prerequisite for this issue. **Resume
here only after 113.**

Everything landed here is default-off and keeps its value for that
work: `foreach_avar` (the four AVar populations named once), the
predicate-threaded `clear_results` (one enumeration), the closure
computation, and `selective_diff.sh`.

### Superseded: where to resume — attack the classing, not the scoping

Three options, in the order they look promising:

1. **Class lazily.** Give `same_eq_classes` a path that CLASSES an
   unclassed AVar on demand instead of asserting. Different from the
   conservative variant that failed: compute the answer rather than
   refuse it. This decouples classing from what the propagation
   re-derived, which is the actual conflict.
2. **Run the setter stage over all AVars**, regardless of re-derivation.
   `extend` is only ~0.4% of FA time (M1), so covering everything may
   simply be affordable — worth measuring before assuming otherwise.
3. **Coarsen the granularity** so a `Setters` set cannot span the
   preserve/clear boundary. Least attractive: it makes the closure a
   function of setter topology rather than value flow, and M1's small
   closures are what make this worth doing at all.

Do NOT resume by scoping harder. Four attempts say the scoping is not
where the problem is.

### Superseded resume note

Refactor `clear_results` to take a per-AVar predicate first (see
above), then re-express the selective path as that predicate. Only then
re-test the setter-class interaction, which is the one piece that may
genuinely resist scoping and might have to stay globally reset.

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

- [113](113-FA-setter-equivalence-is-a-global-batch-partition.md) — **the blocker**, extracted from M3.
- [074](074-FA-cross-pass-oscillation-plan.md), [101](101-FA-first-time-forever-splitting.md)
  — non-convergence. Distinct problem: `amaze`'s profile is 43 CHEAP
  passes that never converge, which selective invalidation does not
  help. This issue is about the cost of a pass, not the number of them.
- [098](closed/098-FA-per-pass-reset-scoped-to-reachable-set.md) — why the
  reset is where it is, and the stale-AVar bug that shaped it. Fixed
  2026-08-12, CLOSED 2026-08-31, and directly adjacent: 098
  scoped the reset to the REACHABLE set; this issue scopes it to the
  CHANGED set. Its post-mortem is the best available guide to what goes
  wrong when the scope is too narrow.

## 2026-09-04: caching a MONOMORPHIC contour's interface — assessment

Proposal: cache analysis at the ES/function level for monomorphic
contours by recording the AVars with outgoing (non-internal) flow edges,
and reflow those on an incoming call.

### Where the cost actually is — a correction to the framing

M3's constraint 2 already measured this: **re-walking a settled contour
is cheap.** hq2x pass 15 is ~30 000 AVars, 79 dirty, **0.081 s**. The
expense is not the traversal, and not re-deriving an interface — it is
that `clear_results()` wipes every AVar's value first, so the walk has
to rebuild what it just threw away.

That matters for the proposal: recording interface AVars and reflowing
them still requires the BODY's AVars to hold their values, or the reflow
has nothing to propagate through. And once they hold their values, the
ordinary walk is already the cheap thing — the interface record buys
nothing on top. **The win is in preserving AVar values, not in
memoising an interface.** So this reduces to M3, which is where the
effort should go.

### Does "monomorphic" dodge the blocker? Possibly — and for a reason
### none of the seven attempts used

The blocker ([113](113-FA-setter-equivalence-is-a-global-batch-partition.md))
is that `same_eq_classes` asserts every member of a live `Setters` set
carries a `setter_class` assigned THIS pass, while membership is decided
DURING the pass and the preserve/clear decision must be made before it.

But `compute_setters` is only ever called over **confluences** —
`collect_cs_setter_confluences`, and the type-stage equivalent. ifa/124's
note in `fa.cc` says so explicitly: *"compute_setters only ever visits
`confluences`. If a container's ELEMENT AVar is not collected as a type
confluence, its writers never get a setter_class."*

A **settled monomorphic contour has no confluence by definition** —
nothing disagrees at any of its AVars. So its AVars should never be
classed and never enter a `Setters` set, and preserving them should not
be able to trip the assert. That is a materially different precondition
from the seven attempts, all of which scoped by *what changed* and so
preserved contours that ARE confluences.

### The risk that decides it, and the measurement

`update_setter` records on the container and propagates **backward** to
writers. A monomorphic contour's AVar can therefore be pulled into some
OTHER contour's Setters set as a backward writer, without being a
confluence itself. If that happens often, the restriction buys nothing.

**The measurement:** at the end of a converged pass, count the AVars in
live `Setters` sets whose `EntrySet`'s function has exactly one ES
(monomorphic), against the total. If that count is ~0, preserving
settled monomorphic contours is viable and M3 has a route it has not
tried. If it is large, this dies for the same reason as the others and
should be recorded as attempt eight.

Not yet run — it needs a build, and a corpus sweep was in flight.

## 2026-09-04: how shedskin handles multiple passes — it does NOT cache

Read `shedskin/infer.py` directly (the checkout at
`~/projects/shedskin`), because shedskin translates hq2x in 14 s against
pyc's 31 s and the obvious guess is that it memoises something. It does
not. Its `iterative_dataflow_analysis` is:

```
backup = backup_network(gx)          # ONCE: snapshot the pristine graph
while True:
    propagate(gx)                    # forward CPA to fixpoint
    split = ifa(gx)                  # backward phase: find imprecision
    if not split: return
    ... record split decisions in gx.alloc_info ...
    restore_network(gx, backup)      # RESET to the pristine graph
```

Same algorithms as pyc, by name (its docstring cites Agesen's CPA and
Plevyak's IFA).

### What each one actually keeps — the difference is STRUCTURE, not values

"Resets harder" was too loose. The two differ on what survives, and it
is not the types:

| | pyc `clear_results()` | shedskin `restore_network()` |
|---|---|---|
| node/AVar types | cleared to **bottom**, re-derived by the walk | restored to the **original seeds** from the snapshot |
| duplicate/contour STRUCTURE | **KEPT** — every EntrySet and CreationSet minted by an earlier split survives | **DISCARDED** — `func.cp = {}`; duplicates are rebuilt |
| the split decisions | implicit, in the surviving contours' `filters` | explicit, in `gx.alloc_info` + `cl.dcpa` |
| constraint graph | edge containers cleared, objects kept | `gx.constraints` / `gx.cnode` restored from the snapshot |
| setter classing | `cannonical_setters` cleared | no equivalent |

Concretely, `clear_es` zeroes `out_edges`, `backedges`, `cs_backedges`,
`creates` and `live_pnodes` — and does **not** touch `filters`,
`display` or `split`. So a contour that a previous pass split stays
split, carrying the partition that made it. `clear_cs` and `clear_avar`
are the same shape: containers emptied, values bottomed, identity and
membership preserved.

**pyc preserves the structure and clears the values; shedskin preserves
the decisions and rebuilds the structure.** Both reach the same place —
pyc re-derives the seed types by walking, shedskin restores them
directly — so the types are a wash.

The consequence is the cost model. pyc's per-pass work is proportional
to the ACCUMULATED contour count, because every ES ever split is still
there to be re-walked: hq2x carries `ess=615 css=2350` by pass 11 and
pays for all of them on passes 12, 13 and 14 to add three entry sets.
shedskin's per-round work is proportional to the ADMITTED SUBSET — five
new functions and one new allocation site — with the duplicates for that
subset rebuilt from a small decision table.

So the thing pyc keeps is exactly the thing that makes its passes
expensive, and the thing shedskin keeps (`alloc_info`) is small by
construction.

**So caching contour results is not where shedskin's speed comes from.**
Three other things are:

| knob | value | effect |
|---|---|---|
| `INCREMENTAL_FUNCS` | **5** | at most five new functions admitted per round, growing from the call-graph root |
| `INCREMENTAL_ALLOCS` | **1** | one new allocation site per round |
| `CPA_LIMIT` | **10**, doubled on hit | caps cartesian-product duplication, relaxes only when it binds |
| `MAXITERS` | **30**, `maxhits == 3` | gives up rather than running to convergence |

Each round is cheap because the analysed SET is small, not because
previous work was preserved. pyc analyses the whole program every pass
and runs to convergence.

### What this means for M3 and for the caching proposal

The proposal above (cache a monomorphic contour's interface) and M3
(preserve AVar values) are both bets on *incremental preservation*.
shedskin is evidence that the alternative lever — **incremental
admission**, plus hard caps and a willingness to stop — is enough to be
2.2x faster on the same input with the same algorithms. It also
sidesteps ifa/113 entirely: nothing derived survives a round, so
setter classing is recomputed from scratch every time and its
batch-partition invariant is never violated.

That does not make M3 wrong, but it reframes the priority: an
admission-order experiment (analyse the call-graph root first and widen)
is cheap to try, needs no change to the setter machinery, and has a
working implementation to compare against. It should be measured before
more effort goes into preservation, which has now failed seven times for
one reason.

The monomorphic-contour measurement stated above is still worth running
— it is the one thing that could unblock M3 — but it is no longer the
only route to the 48%.


### Correction: shedskin DOES keep the function split — content-addressed

"Discards the function duplicates" is right about the numbering and
wrong about the information. The `alloc_info` key is:

```python
alloc_id = (parent.ident, cart, node.thing)
#            function      CPA context   allocation site
```

`cart` is the **cartesian product** — the tuple of `(class, dcpa)`
argument types that defines the function duplicate. So the function
split IS preserved, keyed by CONTENT (which argument types) rather than
by the duplicate index. `func.cp` can be thrown away because nothing that survives a
round ever refers to a `cpa`. The number is a per-round ordinal —

```python
func.cp[dcpa][c] = cpa = len(func.cp[dcpa])     # infer.py:1401
...
for func in gx.allfuncs: func.cp = {}           # restore_network, :2108
```

— assigned in first-seen order, so it need not even AGREE between
rounds. `gx.alloc_info`, the one table that crosses the reset, is keyed
by `(name, argument-type tuple, site)` and never mentions it. The
numbering is disposable exactly because the identity is
content-addressed.

That answers "how can it split the allocation site if the surrounding
function contour is not split": the surrounding contour IS split, and is
identified by the very thing that makes it distinct.

**Newly-split contours inherit from a "mother".** When a `cart` appears
that has no `alloc_info` entry, `ifa_seed_template` searches for an
existing entry for the same `(function, site)` whose `cart` differs
*only* in ways a class split explains:

```python
if a != b and not (a[0] is b[0] and a[1] in a[0].splits
                   and a[0].splits[a[1]] == b[1]):
    break        # not the mother
else:
    mother_alloc_id = (id, c, thing)
```

and copies the mother's allocation type. So a freshly split function
duplicate starts from the assignment of the contour it descends from —
the split lineage is reconstructed from the class-split table rather
than carried in objects.

**A recursive tree copy resolves through the DATA half, not the function
half.** Recursing on the same class gives the same `cart` at every
depth, so CPA alone would not separate the levels. What separates them
is IFA splitting the class into duplicates per allocation site; those
duplicates change the `(class, dcpa)` pairs inside `cart`, which then
produces distinct function duplicates. Data polymorphism drives function
duplication — the same direction pyc's CS-driven ES splitting works.

**So the real difference is narrower than stated above.** Both systems
identify a contour by content — pyc's `find_or_make_filtered_entry_set`
searches by `filters`, and ifa/100 made the display explicitly *not*
part of contour identity for the same reason. shedskin rebuilds the
objects from that identity each round; pyc keeps them. The cost
consequence stands (pyc re-walks every accumulated contour, shedskin
only the admitted subset), but "shedskin discards the structure" was
overstated: it discards the *representation*, not the *identity*.


## Measured: chess, pyc vs shedskin, same source (2026-09-04)

Both run on the identical file — pyc's `shedskin_examples/chess/chess.py`
copied into a scratch dir for shedskin, so the added `printBoard` and
`rowAttack` return are in both.

| | pyc | shedskin |
|---|---|---|
| passes / iterations | **31** (+1 confirming) | **38** |
| structure | 32 whole-program passes | 38 iterations over **26 incremental rounds** |
| analysis time | ~29 s (FA only) | **6.7 s** (whole prebuild) |
| total front-end | 47.8 s (C++ compile of the emitted code is 1.65 s of it) | 7.1 s |
| final contours | ess=1591, css=6433 | 3987 templates (cumulative, not comparable) |

**The pass counts are close. The cost per pass is not.** That is the
finding — pyc is not iterating wildly more than shedskin; it is doing
far more work per iteration.

### pyc: 26 of 32 passes are pure splitting

```
  1   0.74s  ess=207   css=1769  viol=1340  examined=19342
  2   1.19s  ess=579   css=3540  viol=6224  examined=48492
  6      —   first pass with ZERO type violations
  8   0.66s  ess=927   css=3835  viol=0     examined=54011
 16   0.99s  ess=1374  css=5557  viol=0     examined=82678
 24   1.10s  ess=1536  css=6191  viol=0     examined=92735
 28   1.24s  ess=1594  css=6573  viol=0     examined=99507
 31   1.04s  ess=1591  css=6433  viol=0     examined=96915
```

Type violations reach **zero at pass 6**. The remaining 25 passes carry
no type errors at all — they are the splitter refining contours (ess
927 → 1591, css 3835 → 6433). And the cost per pass **rises** over that
stretch, 0.66 s → 1.24 s, because `examined` grows 54k → 100k avars:
every pass re-walks the whole accumulated network to make a decision
about a shrinking frontier.

### shedskin: 26 rounds, most converging in one iteration

```
round  1: 4 iters      rounds 2-26: 1,1,1,2,1,2,2,1,1,2,1,1,3,
                                    1,2,1,2,1,1,1,1,2,1,1,1
```

Only the first round needs 4 iterations. Twenty of the remaining 25
converge in a **single** iteration, because `alloc_info` already holds
the answer for everything admitted earlier and the round only has to
propagate for the newly admitted 5 functions / 1 allocation site.

### What this says about the proposal

The gap is not iteration count, so a change that reduces pyc's pass
count is aiming at the wrong quantity. The lever is the 25 zero-
violation passes, each re-examining ~100k avars to service a frontier of
a few dozen contours. Both directions in this issue attack that:
selective invalidation (don't re-walk what cannot have changed) and
incremental admission (don't have it in the network yet). The measured
shape — violations gone at pass 6, cost per pass still climbing at pass
28 — says the second is where the 4× lives.

Caveat: the ~29 s FA figure is the sum of pyc's own per-pass timers
under `-v`, whose symbol dump inflates the wall clock; the 47.8 s total
is from a clean non-verbose run.


### Why shedskin's iterations are cheap: the graph does not grow

Instrumented `propagate`/`ifa`/`restore_network` on the same chess run
(driver in the session scratchpad; no changes to the shedskin checkout).

```
ITER   1 propagate 0.076s  cnodes=20049  admitted= 0  templates= 220
ITER   7 propagate 0.083s  cnodes=19406  admitted= 3  templates= 758
ITER  33 propagate 0.082s  cnodes=19845  admitted=16  templates=3349
ITER  38 propagate 0.116s  cnodes=19915  admitted=19  templates=4007
                                   propagate total 3.36 s / 38 iters
                                   ifa total       0.19 s  (0.005 s each)
```

**The live node count does not move: 20049 → 19915 across 38
iterations**, while templates grow 220 → 4007 (18×). Cost per iteration
grows only 0.076 → 0.116 s.

pyc over the same program:

```
pass  1  0.74s  examined=19342   ess=207
pass 31  1.04s  examined=96915   ess=1591
```

**Both systems start at the same size** — 19342 examined avars vs 20049
cnodes. pyc's grows 5×; shedskin's does not grow at all.

### The mechanism

`restore_network` resets `gx.types` / `gx.constraints` / `gx.cnode` to
the pre-analysis snapshot every round. The templates a round builds are
thrown away with it and rebuilt next round — which is why `gx.templates`
reaches 4007 cumulative while the live graph stays at 20k. What survives
is `gx.alloc_info`, a **side table** of allocation decisions that is
never walked during propagation.

pyc does the opposite: contours accumulate IN the graph (`ess` 207 →
1591, `css` 1769 → 6433) and every pass re-walks all of them.

So the ~10× per-pass gap is **entirely work volume** — see the
correction below. pyc's per-unit cost is BETTER than shedskin's, as C++
against CPython should be.

And shedskin's splitter is free: `ifa()` costs 0.005 s an iteration,
0.19 s for the whole compile. pyc's equivalent work is spread through
the 25 zero-violation passes that cost ~25 s.

### The design consequence

This is the sharpest form of the issue. Selective invalidation makes
pyc walk less of an ever-growing graph. shedskin's answer is that the
graph should not grow: keep the decisions, discard the derived
structure, re-derive it against a bounded base each round. That is a
much larger change than M1/M2 contemplate, and it is what the 5× is.


### Correction: the per-unit cost is not the problem — C++ is 9x faster

Two earlier per-node figures in this issue were wrong, both from bad
denominators. `examined_avar_count` counts `collect_type_confluences`'s
exhaustive `ess x fa_all_Vars` sweep — a SPLITTER diagnostic — not the
propagation worklist. Dividing pass time by it compares pyc's splitter
scan against shedskin's propagation and means nothing.

pyc's propagation unit is the **AEdge**. Counted directly
(`PYC_DBG_WORK=1`, probe added with this entry):

| | work units | time | per unit |
|---|---|---|---|
| pyc | **22,511,055** edge visits | 27.3 s | **1.21 µs** |
| shedskin | **329,329** node visits | 3.7 s | **11.22 µs** |

**pyc is ~9× cheaper per work unit and does ~68× more of them.**
68 / 9.2 = 7.4×, which is exactly the measured wall-clock ratio. The
implementation is not slow; it is asked to do too much.

(The units are not strictly comparable — an AEdge visit is coarser than
a CNode visit — so the 9× is indicative, not exact. The 68× and the
growth curve below are the load-bearing numbers.)

### The growth is within a single compile

```
pyc     pass  1     30,823 edges       pass 31  1,484,115 edges   (48x)
shedskin iter 1      9,871 visits      iter 38     10,239 visits  (flat)
```

pyc re-seeds `edge_worklist` from `top_edge` every pass and re-walks the
whole accumulated contour graph; as `ess` goes 207 → 1591 the per-pass
edge count goes up 48×. shedskin's per-iteration visit count does not
move — see "what actually persists" below for why.

So the target is not per-edge efficiency and not the pass count. It is
that pass N costs 48× pass 1 for the same program.


### Correction: "rebuilds only the admitted subset" was wrong

That phrasing conflated two things. `restore_network` runs at the end of
**every iteration** of `iterative_dataflow_analysis`, not at round
boundaries, and the reset is total and exact. Measured per iteration:

```
ITER  1  0.079s  base_in=20543 -> after=21742  contour_dups=9161  templates+=220  admitted= 0
         after restore: cnodes=20543  contour_dups=8123
ITER 38  0.113s  base_in=20543 -> after=21598  contour_dups=9025  templates+=133  admitted=19
         after restore: cnodes=20543  contour_dups=8123
```

**`base_in` is 20543 on every one of 38 iterations, and the restore
returns to exactly 20543 / 8123 every time.** There is no growing
"admitted subset" living in the graph. Each iteration builds ~500-1400
nodes and ~130 templates on top of a FIXED base and then discards them.

So the per-iteration cost is bounded by construction, and admission is
not what bounds it. Admission (`INCREMENTAL_FUNCS=5`,
`INCREMENTAL_ALLOCS=1`, the `continue` at infer.py:1290) decides WHICH
functions may be contoured during a rebuild — precision scheduling, and
a cap on how much one rebuild can cost. What keeps cost flat across
iterations is the reset.

### What actually persists across the reset

Not graph structure. Four things, all small:

1. **`gx.alloc_info`** — `(func ident, cart, site) -> (class, dcpa)`, the
   allocation decisions. Never walked by propagation.
2. **Mutations of the backup snapshot itself.** `beforetypes = backup[0]`
   is edited between iterations: constructor nodes in functions are
   cleared (`beforetypes[node] = set()`) and split classes are seeded
   onto global nodes (`beforetypes[n] = {(cl, newnr)}`). The reset target
   evolves; its SIZE does not.
3. **`cl.dcpa` / `cl.splits`** — the class-split table, which is also
   what `ifa_seed_template`'s mother-lookup consults.
4. **`gx.added_funcs_set` / `added_allocs_set`, `cpa_limit`, `maxhits`** —
   admission and limit state.

That is the shape pyc does not have: **knowledge in a side table,
structure re-derived against a snapshot of fixed size.** pyc keeps the
structure and re-walks it, which is why its per-pass cost tracks `ess`.

(Incidental: shedskin's analysis is not run-to-run deterministic — three
instrumented runs of the same file took 38, 38 and 39 iterations, from
Python set iteration order over id-hashed objects. Iteration counts here
are ±1.)
