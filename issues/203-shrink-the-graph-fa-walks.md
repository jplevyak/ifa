# 203 — shrink the graph FA walks: demand-placed renames, copy propagation, dead arms as a decision

**Status: open, not started.** Filed 2026-11-03 from
[closed/193](closed/193-review-optimize-against-analysis-2026-11.md) C1–C5
and [closed/195](closed/195-what-the-concert-compiler-still-has-that-ifa-does-not.md)
C3. The measurements it asks for were not taken (the reviewing container
could not build); take them first.

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

Vars per function and FA time down on the corpus baselines; `make test`
green on both backends; corpus `check` verdicts identical per program.
