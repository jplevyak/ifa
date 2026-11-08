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
