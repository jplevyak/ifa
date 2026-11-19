# 203 — shrink the graph FA walks: demand-placed renames, copy propagation, dead arms as a decision

**Status: open; re-scoped 2026-11-08 by the measurement below.** Filed
2026-11-03 from
[closed/193](closed/193-review-optimize-against-analysis-2026-11.md) C1–C5
and [closed/195](closed/195-what-the-concert-compiler-still-has-that-ifa-does-not.md)
C3, without measurements (the reviewing container could not build).

**The measurement says this is not where FA time goes.** It is a
code-health issue, and the useful parts are C4 (now "delete it"), C2's
truthiness wrapper, and C3's second half. The speed problem is
[207](207-FA-call-matching-re-matches-a-whole-union-per-member.md)'s (the
matching algorithm), and
[186](186-FA-pass-one-confluence-through-shared-builtins.md)'s and
[185](closed/185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md)'s:
call matching on early-pass unions.

## Measured 2026-11-08 (tree `d57c13bb`)

**Setup.** Each program was compiled alone from a scratch copy. FA time is
the sum of `-v`'s `PASS N COMPLETE` split. The IR counts come from a
temporary probe (`IFA_DBG_IRSIZE`, since removed) that summed
`collect_Vars_PNodes`' lists over every analysed Fun, at the first
convergence and at the end.

| program | FA | flow | match | extend | passes | Vars | phy lvals | MOVEs | sends (`to_bool` wrappers) | AVars |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| chess | 3.0 s | 1.2 | 1.7 | 0.2 | 16 | 16 620 | 4 288 | 1 058 | 3 568 (289) | 82 K |
| voronoi2 | 3.0 s | 1.4 | 0.9 | 0.7 | 25 | 20 021 | 4 198 | 1 340 | 4 369 (302) | 115 K |
| go | 6.1 s | 1.6 | 0.6 | 3.9 | 27 | 17 497 | 2 906 | 1 386 | 4 059 (244) | 87 K |
| richards | 0.6 s | 0.3 | 0.0 | 0.2 | 19 | 9 808 | 800 | 506 | 2 155 (83) | 44 K |
| bh | 1.5 s | 0.9 | 0.3 | 0.3 | 30 | 12 980 | 2 340 | 835 | 2 799 (110) | 46 K |
| pystone | 0.1 s | 0.1 | 0.0 | 0.0 | 7 | 7 275 | 562 | 476 | 1 656 (82) | 26 K |
| plcfrs | 47.9 s | 12.6 | **31.3** | 4.0 | 67 | 27 656 | 6 444 | 1 823 | 5 930 (406) | 472 K |
| sunfish | 75.7 s | 2.5 | **69.9** | 3.3 | 31 | 38 845 | 9 944 | 2 961 | 10 132 (1 136) | 669 K |

**Correction (2026-11-08, after 207 step 1 fixed `-v`'s pass timer).** The
`flow` column above is wrong for the two expensive programs. Pass 1 is
several runs (the frontend's structural `reanalyze`), and the timer kept
only the last run, so pass 1's `flow` came out negative (sunfish −13.4 s)
and cancelled the real flow of the later passes in the sum. With the timer
fixed:

| program | FA | flow | match | extend |
| --- | --- | --- | --- | --- |
| sunfish | 97.0 s | **26.0 s (27%)** | 67.7 s (70%) | 3.3 s |
| plcfrs | 54.0 s | **19.8 s (37%)** | 29.6 s (55%) | 4.6 s |

Matching still dominates, but flow is a quarter to a third of FA, not 3%.
The upper bound for C1 and C2 below (phy lvals 18–26% of Vars, MOVEs
6–8%) is then roughly 5–9 s on these two, not under 1 s. That is still
not the first lever, but it is not negligible.

**What it shows:**
- **Flow is not the cost where there is a cost.** The two programs with
  real FA time spend it in matching: sunfish 70 of 76 s, plcfrs 31 of
  48 s. Flow, which C1 and C2 shrink, is 2.5 s and 12.6 s. On the small
  programs everything is a few seconds. closed/111's 2026-10-17 numbers
  said the same.
- **The upper bound is small.** Phy lvals are 18–26% of Vars and MOVEs
  6–8%. Removing every one would at best cut flow by about a third:
  under 1 s on sunfish and about 4 s on plcfrs.
- **The truthiness wrapper is 11% of sunfish's sends.** Each `if cond:`
  wrapper is a `period` read of `__pyc_to_bool__` plus a call, and they
  remain at the end of FA. That is the one item here that touches
  matching.
- **Mid-FA inlining does nothing.** Its `simple_inlining` makes **0
  decisions** (`IFA_DBG_BODIES`) in all 574 corpus and suite compiles that
  reach it. The PNode and send counts are identical before and after. With
  it off (`IFA_FA_INLINE=0`), every corpus verdict, every EntrySet and
  CreationSet count (`check__IFA_FA_INLINE_0__d57c13bb+fcb3e59c` against
  `check__default__0b9265ba+0048dc1a`) and the suite (495/0) are
  unchanged. It costs one re-convergence pass and the post-inlining reset
  that [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
  2b found four bugs in.

**What follows, in order:**

1. **C4: delete mid-FA inlining.** Remove the `ifa_fa_inline` block in
   `FA::analyze`, the flag, and the reset path, with a same-binary `check`
   A/B. `simple_inlining` still runs post-FA in `ifa_optimize`. If it is
   ever wanted again, it should be a pre-FA syntactic rewrite or a logged
   decision, as the original C4 said.
2. **C2, the truthiness wrapper only.** Where the operand of `if cond:` is
   a bool by construction (a comparison, `not`, `isinstance`, a bool
   literal), lower to the IF directly. Measure the send count and sunfish's
   and plcfrs's `match` time before and after. Stop condition: under 5% of
   `match`, stop there and record it.
3. **C3, second half:** collect violations over `es->live_pnodes` instead
   of `fa_all_Vars`. Pure cleanup, done when convenient.
4. **C2's copy chains and the nine walkers.** Worth doing for `fa.cc`'s
   readability, judged by the walkers it deletes, not by time.
5. **C1, C3's first half, C5:** not worth doing for performance. Take them
   up only if a program appears whose late passes or flow dominate.

The sections below are the original proposals, kept as the record. Their
"Why" premise, that FA cost is Vars × contours, is the part the
measurement does not support.

## Measured 2026-11-11: three quarters of the flow graph is copies

**Probe** (temporary, removed; tree `a5860b47` plus
[208](208-whole-compile-time-where-it-goes.md)'s changes). At the end of
every pass, over every AVar with a non-bottom type: its live predecessors,
whether it has exactly one and holds the pointer-identical type, and the
strongly connected components of the `forward` graph. All 77 corpus
programs; totals are over each program's last pass.

| | AVars with a type | flow edges | copies | in a cycle | left after collapsing both |
| --- | --- | --- | --- | --- | --- |
| corpus, 77 programs | 1 791 549 | 2 210 337 | 1 332 676 (74%) | 124 199 (6.9%) | 25% |
| othello3 | 426 221 | 1 031 350 | 376 052 (88%) | 823 | 12% |
| plcfrs | 62 112 | 57 339 | 43 988 (71%) | 8 157 | 28% |
| sunfish | 48 975 | 46 630 | 35 086 (72%) | 4 086 | 28% |
| quameon | 47 305 | 42 602 | 35 483 (75%) | 5 941 | 24% |
| rubik | 86 866 | 80 548 | 62 393 (72%) | 6 849 | 28% |

- **A copy** here is an AVar with one live predecessor, no `restrict`, no
  `gen`, and the same converged type as that predecessor. It is observed,
  not proved: nothing checked that the edge could never carry less.
- **Per program** the remainder is 12% to 34% of the live AVars, median
  30%. The first pass has the same shape as the last (76% copies, 23%
  left), so this is the IR's structure, not something splitting creates.
- **Cycles are few and small.** 13 729 in the corpus, the largest 1 035
  AVars (hq2x). 124 134 of the 124 199 AVars in a cycle hold the same type
  as the rest of their cycle. Collapsing them is worth 7%; the copies are
  the other 68%.
- **Distinct types are scarce:** one for every 14 typed AVars.

**Where the time is, which decides what this is worth.** The same probe
timed the worklist loop by kind of work, summed over the corpus:

| work | time | items |
| --- | --- | --- |
| `add_es_constraints` (build a contour's constraints) | 75.3 s | 636 334 |
| `add_send_edges_pnode` (fire a send) | 47.4 s | 4 678 613 |
| `analyze_edge` | 39.6 s | 8 646 725 |
| `push_forward` (propagate a changed type) | 6.1 s | 60 398 065 |

**Propagation is 4% of it.** Collapsing copies saves almost nothing in
flow itself: a type crossing a copy is a pointer compare. What it saves is
the work that is per Var, per contour, per pass: `add_es_constraints`
builds an AVar and an edge for every one of these, and `clear_avar`,
`collect_var_type_violations` and `make_AVar`'s lookup then walk them
(`make_AVar` alone is 3 to 9% of a compile's samples in
[208](208-whole-compile-time-where-it-goes.md)).

**What it means for the steps below.**
- The 2026-11-08 bound ("phy lvals 18–26% of Vars, MOVEs 6–8%") counted
  Vars. Counted as AVars at convergence, the redundant share is 75%, so
  C1 and C2 are worth more than that bound said, and the two together
  should account for most of the 74%. Which of them owns how much was not
  measured: the probe did not record what kind of PNode made each edge.
  **Do that first.**
- It has to happen in the IR, before FA (C1, C2), not as a collapse of
  AVars during FA. A collapse during FA would still build every AVar in
  `add_es_constraints`, which is where the time is.
- An online cycle collapse is not worth building: 7%, and propagation is
  not the cost.

## Why

FA's cost is (Vars × contours) flow edges, re-derived every pass from
bottom. On every pass, for every contour, `add_pnode_constraints` makes
one AVar per (Var, contour) it meets, every phi and phy lval included, and
one flow edge per MOVE, phi operand and phy copy (`fa.cc:2949–3270`);
`collect_var_type_violations` then calls `make_AVar` on every Var in
`fa_all_Vars` for every contour, dead arms included (`fa.cc:4176`). The
graph is fixed by `build_ssu` before any type is known. So each Var the
optimize passes leave in a function costs an AVar and its edges in every
contour of every pass.

## Measure first

`pyc -v -v -v` prints per-function CFG node, phi and phy counts
(`build_cfg`, `print_ssu`). Over the corpus: Vars per function, phy lvals
per function, MOVE PNodes per function, and FA time. These are the
baselines every step below is judged against.

## C1. Place phy renames on demand (largest)

`place_phy` (`ssu.cc:525`) gives every local live across a branch a fresh
name on each successor; FA installs a `flow_vars` edge per phy per
successor per contour (`fa.cc:3233–3268`); codegen materialises each as a
move. The only thing a phy buys is per-branch narrowing, and FA narrows
exactly one Var per `IF`: `narrow_operand`, the discriminator's operand
`peel_wrapper_def` finds. Every other phy is an identity copy whose two
lvals converge. The operand is syntactically known before FA, so
`place_phy` can be restricted to it.

The Concert ancestor took the other route (closed/195 C3): place
everywhere, then `short-out-phys` and `remove-dead-phys` to a fixed point,
with two guards worth keeping in either design: never propagate through a
copy whose type differs from its source by more than `None` (the
narrowing case), and never short-circuit a phi through another phi on the
same conditional.

Stop condition: any suite or corpus verdict change names a consumer of
phy that is not narrowing; that consumer is the finding. Fixture
`14_isinstance_narrow.ir` pins the needed case; `11_if_join_phi` and
`13_loop_phi` pin that phis are untouched; [202](202-health-docs-levers-probes-tests-and-ci.md)
lists the fixtures to add first.

## C2. Collapse copy chains before FA

The frontend emits a MOVE at 49 sites; `fa.cc` has nine MOVE-chain walkers
to see through them (`first_var`, `folded_load_source`,
`peel_wrapper_def` bounded at 6, `peel_guarded_bool_merge`,
`walk_to_actuator`, `nil_receiver_var`, `reaching_def`, `reaching_var`,
`exc_check_operand_provably_safe`). Copy propagation after SSU on
local-to-local MOVEs whose source has one definition and is not a cell
(global, field, exception slot) removes the Var and the edge; the
`provably_constant_load` hook survives because a cell read is excluded,
and the replacement use inherits the `ast`. Separately, `if cond:` lowers
to three sends and an IF (`fa.cc:3092`), with the `__pyc_to_bool__`
wrapper removed only by mid-FA inlining after the first convergence; where
the operand is a bool by construction, emit the IF directly.

Stop condition: the nine walkers shorten or go, `peel_wrapper_def`'s bound
is never reached, diagnostics still locate every violation.

## C3. Dead arms as a decision table; violations over live sets

FA already skips an `IF` arm whose condition is constant in that contour
(`fa.cc:3232,3250`), [119](119-sccp-as-an-outer-fixed-point-over-fa.md)'s
stage 1 in effect, but re-derives it every pass and still mints AVars for
the arm's Vars. Record per (contour, IF) which arm was dead at the end of
a pass; skip it next pass; iterate violation collection over
`es->live_pnodes` instead of `fa_all_Vars`. The second half is
independent and pure cost. Stop condition from 119: a second-order fold
starts to type; `ess`/`css` and pass counts do not rise.

## C4. Decide what mid-FA inlining is

`FA::analyze` runs `simple_inlining` once after the first convergence,
minting Vars and rewriting `rvals`; it is the one IR mutation inside the
loop, done once rather than re-derived, unlogged, and keyed-on Vars change
under the persistent decisions (`av->cs_map`). [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
2b lists four bugs the post-inlining reset exposed. Either a pre-FA
syntactic rewrite for statically known wrappers, or a logged decision the
ledger can be invalidated against; the Concert ancestor ran the cleanup
twice before inlining and `fixup-after-inlining` per method after, each
followed by recomputing reaching definitions (closed/195). Stop
condition: `--fa-inline=0` and the default give the same corpus verdicts,
which today they do not.

## C5. Feed FA's per-contour liveness forward

At convergence `es->live_pnodes` (re-derived by `settle_gated_contours`)
says which PNodes each contour reached; `mark_live_code` starts over. Seed
`mark_initial_dead_and_alive` from the union of live sets (through
`fa_live`, which codegen already reads); the IF/GOTO special cases for
nodes FA never reached go away.

## Verification

As planned: Vars per function and FA time down on the corpus baselines; `make test`
green on both backends; corpus `check` verdicts identical per program.
