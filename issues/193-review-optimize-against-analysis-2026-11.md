# 193 — review of `ifa/optimize/` against `ifa/analysis/`: hazards, dead work, and the IR simplifications that shrink the graph FA walks

**Status: open. Umbrella / audit**, like [146](146-remove-all-arbitrary-splitting.md).
Written 2026-11-03 against `829f43d`. Static reading plus counts over the
sources and the checked-in fixtures; the compiler could not be built in
the reviewing container, so the two counts §C asks for are not measured
here. Companion to ifa/192, the review of the analysis side. Close this
issue by filing or fixing each row and striking it.

## Verdict

`ifa/optimize` is small (2,701 lines in seven passes), algorithmically
sound, and finished before the analysis grew to fourteen thousand lines.
It shapes the graph FA walks on every pass but never gets a say in it
afterwards. The single largest FA speed-up available is in this
directory, not in `fa.cc`: SSU's branch renames (phy) are placed on every
live variable at every branch, and the frontend's copy chains are walked
inside FA rather than removed before it. FA pays for both once per
contour per pass, from bottom.

## State

| | |
| --- | --- |
| lines | cfg 129 · ssu 603 · dom 227 · loop 212 · dead 491 · inline 762 · exc_check_fold 70 |
| last change | 2026-10-14; `fa.cc` took 15 commits in the same month |
| fixtures | 24 `.ir`: cfg 6 · ssu 5 · dom 3 · loops 3 · freq 3 · dce 2 · inline 2 |
| dead code | 93 lines of `#if 0` in `dead.cc` |
| passes with no consumer | 2: frequency estimation and the call-graph loop tree |
| invariant checks on by default | 0: `check_invariants` is compiled out when `DEBUG=1`, the default |

Nothing in this directory is filed against `ifa/optimize`. ifa/039 landed
its definite-assignment dataflow in `ssu.cc`; ifa/010 names `dom.cc` as
the Vec/Set pilot. Every other reference is a null guard added from the
outside, each a symptom of the passes running over slightly different
node universes (§A3).

## What runs when

| when | pass | note |
| --- | --- | --- |
| `Fun` ctor, before FA | `build_cfg` | Code → PNode; universe = backward from exit |
| `Fun` ctor, before FA | `build_ssu` | dominators, syntactic liveness, phi at joins, **phy at every branch**, rename; ifa/039's dataflow |
| every FA pass | (none) | FA re-walks the same graph |
| between the two convergences | `simple_inlining` | mutates PNodes, mints Vars (§C4) |
| after clone | `build_cfg_dominators` | again, per clone |
| after clone | `mark_live_code` | AVar-backward liveness; O(n²) IF/GOTO cases |
| after clone | `frequency_estimation` | `find_all_loops` + `build_call_dominators`; read by no emitter |
| `pyc.cc` | `mark_exc_checks_constant` | folds `__pyc_exc__` checks by name |
| `ifa_optimize` | `simple_inlining` ×≤4, `mark_live_types`, `mark_live_funs` | |

Two facts drive the rest: the graph FA analyses is fixed by `build_ssu`
before any type is known, so every placement there is made without
demand; and DCE runs once, after cloning, so nothing it learns reaches a
later FA pass, and nothing FA learns per contour (`es->live_pnodes`)
reaches it except through the `fa_live` bit codegen reads.

## What is strong

- **The algorithms are the right ones and cited**: Tarjan dominators with
  Cytron's frontier, Ramalingam's loop tree over Sreedhar-Gao-Lee,
  frontier phi placement.
- **Determinism was done at the source** (`finalize_cfg` sorts
  `cfg_pred`, the frontier is sorted, `Dom` carries a serial; ifa/112,
  ifa/035).
- **ifa/039's dataflow is textbook**: two meets, one RPO sweep, dense
  bitsets, a sweep cap that abandons rather than lies.
- **The inliner is honest about its bails**, each with the issue that
  found it (046, 049, 165, 002, 014).
- **Every pass has a normalised printer** (`print_cfg` … `print_inline`),
  which is the harness §C needs.

## A. Correctness and latent hazards

No known miscompile on the corpus. Each row is a condition relied on and
not checked, or a check that does not run.

| # | finding | severity | owner |
| --- | --- | --- | --- |
| A1 | **`check_invariants` is compiled out in the default build.** Its body is under `#ifndef DEBUG`; both Makefiles set `DEBUG=1` and pass `-DDEBUG=1`, so the four calls in `inline.cc` and the one in the `Fun` ctor are empty in every normal build and in CI. CFG_SSU.md calls it "release-mode CFG sanity", which is what the guard says and the opposite of what is wanted. Flip to `#ifdef DEBUG`, run the IR phases. `fun.cc:327–342`, `Makefile:9,45–46`. | defect | new |
| A2 | **The inliner splices into the CFG with an unasserted precondition.** `insert_move_before` rewires every pred of `p` to the new MOVE and sets `p->cfg_pred` to one node. At a join (phis attached) the phi `rvals` would stay sized to the old pred count. Safe today only because joins are `Code_LABEL` PNodes and the inliner inserts before a `Code_SEND`; nothing says so. Assert `p->cfg_pred.n == 1 \|\| p == f->entry` and `!p->phi.n`. `inline.cc:246–264`. | unchecked | new |
| A3 | **Three passes agree on the node universe by accident.** `collect_PNodes` walks backward from exit, `build_dominators` forward from entry, `remove_unreachable` prunes only forward-unreachable preds. A region reachable from entry with no path to exit is outside the universe but has edges into it; `dom.cc`, `loop.cc` (twice) and `inline.cc` each carry a null guard for that shape, found one crash at a time. A `while True:` with no break is inside only because the lowering emits a structural false edge. Define the universe once in `finalize_cfg` (nodes on some entry-to-exit path), prune there, delete the five guards. `cfg.cc:42–73`, `dom.cc:171–208`, `loop.cc:83–90,172–178,192–203`. | universe | new |
| A4 | **A store's liveness is a two-hop forward probe because its temporary has no definition.** `P_prim_setter` / `set_index_object` stay live by `forward_live(tvals[0], 2)`; every other producer is kept live through `Var::def` when backward liveness reaches its result, but `fill_tvals` creates the temporary with no `def`. OPTIMIZE.md §8.4 states the consequence (a store observed beyond two hops is deleted); minpng was this family, fixed by a special case. Set `tvals[i]->def = p` in `fill_tvals`; the setter becomes an ordinary producer and `forward_live` is deleted. `dead.cc:194–201,227–238`, `fa.cc:1817–1831`. | heuristic | new |
| A5 | **`exc_check_fold.cc` decides by a Python name inside the ifa library**: `strcmp(sym->name, "__pyc_exc__")` and `sym_nil_type` on the fourth operand, called from `pyc.cc`, against "ifa is not a Python compiler" and "never decide by name". Mark the Sym from the frontend (`Sym::is_exception_slot`) and test the bit, or move the pass behind `IFACallbacks`. The closed/172 and 190 item 3 work on threading the pending exception touches this anyway. `exc_check_fold.cc:27–70`, `pyc.cc:314`. | name | new |
| A6 | **Three traversals recurse to the graph's depth**: `mark_live_avar` over `backward`, `rename_edge` over `cfg_succ` (one frame and an `Env` push per CFG node; othello3's 1,848-store function is a frame per statement), `df_traversal`. Same family as NOTES.md §7; ifa/186 converted FA's hot one to a worklist. | recursion | new |

## B. Dead and redundant work

| # | finding | owner |
| --- | --- | --- |
| B1 | **Frequency estimation and the call-graph loop tree have no consumer.** `frequency_estimation` runs `find_all_loops` (a loop tree per clone plus `build_call_dominators` over every clone) and writes `PNode::execution_frequency` / `Fun::execution_frequency`; the only reader is `testing/print_dce.cc`. `false_branch_frequency` is never written. The inliner does not consult it. On othello3 that is a dominator computation over a ~5k-node call graph for nothing. Give it a consumer (`__builtin_expect` on the emitted `if`, a size × frequency inliner gate) or run it only under the `freq` phase. `inline.cc:59–118`, `ifa.cc:69`. | new |
| B2 | **IF and GOTO liveness are O(n²)** when the reverse dominance frontier already names control dependence. `mark_live_pnodes` scans `fa_all_PNodes` and tests dominance twice per node inside the fixed point; `rdom->front` from `build_cfg_dominators` gives the branches a node is control-dependent on directly, and walking it from each newly live node is linear. `dead.cc:214–217,245–249`, `dom.cc:110–130`, OPTIMIZE.md §8.3. | new |
| B3 | **Dominators are built twice per function; `collect_Vars` up to six times** (`build_ssu`, `Fun::copy`, once per inline pass per function, `recollect_Vars_PNodes`). A dirty bit on `Fun` removes the repeats. `ssu.cc:552`, `ifa.cc:66`, `inline.cc:718,722`, `fun.cc:231`. | new |
| B4 | **93 lines of `#if 0` in `dead.cc`** referencing a field that no longer exists. The notes/ convention is to write the note and delete the code; this is `notes/006`. `dead.cc:395–491`. | new |

## C. Simplifying the graph FA walks

**How FA consumes the graph.** On every pass, for every contour,
`add_pnode_constraints` walks the CFG and makes one AVar per (Var,
contour) it meets, every phi and phy lval included, and one flow edge per
MOVE, phi operand and phy copy (`fa.cc:2949–3270`).
`collect_var_type_violations` then calls `make_AVar` on every Var in
`fa_all_Vars` for every contour, dead arms included (`fa.cc:4176–4180`).
So each Var the optimize passes leave in a function costs an AVar and its
edges in every contour of every pass, and every pass starts from bottom.

| # | change | what it buys | owner |
| --- | --- | --- | --- |
| C1 | **Place phy renames on demand.** `place_phy` gives every local live across a branch a fresh name on each successor (`ssu.cc:525–548`). FA installs a `flow_vars` edge per phy per successor per contour (`fa.cc:3233–3268`); codegen materialises each as a move. The only thing a phy buys is per-branch narrowing, and FA narrows exactly one Var per `IF`: `narrow_operand`, the discriminator's operand found by `peel_wrapper_def`. Every other phy is an identity copy whose two lvals converge. The operand is syntactically known before FA, so `place_phy` can be restricted to it. Fixture `14_isinstance_narrow.ir` pins the needed case; `11_if_join_phi` and `13_loop_phi` pin that phis are untouched. **Measure first**: Vars per function and FA time on the corpus. Stop condition: any suite or corpus verdict change names a consumer of phy that is not narrowing; that consumer is the finding. | largest | new |
| C2 | **Collapse copy chains before FA.** The frontend emits a MOVE at 49 sites in `python_ifa_build_if1.cc`; `fa.cc` has nine MOVE-chain walkers to see through them (`first_var`, `folded_load_source`, `peel_wrapper_def` bounded at 6, `peel_guarded_bool_merge`, `walk_to_actuator`, `nil_receiver_var`, `reaching_def`, `reaching_var`, `exc_check_operand_provably_safe`). Copy propagation after SSU on local-to-local MOVEs whose source has one definition and is not a cell removes the Var and the edge; the `provably_constant_load` hook survives because a cell read is excluded by the rule, and the replacement use inherits the `ast`. Separately, the `if cond:` lowering is three sends and an IF per test, with the `__pyc_to_bool__` wrapper removed only by mid-FA inlining after the first convergence; where the operand is a bool by construction the frontend can emit the IF directly. | large | new |
| C3 | **Dead arms as a between-pass decision; violations over live sets.** FA already skips an `IF` arm whose condition is constant in that contour (`fa.cc:3232,3250`), ifa/119's stage 1 in effect, but re-derives it every pass and still mints AVars for the arm's Vars in every contour. Record per (contour, IF) which arm was dead; skip it next pass; iterate violation collection over `es->live_pnodes` instead of `fa_all_Vars`. The second half is independent and pure cost. | medium | 119 |
| C4 | **Mid-FA inlining mutates the graph the persistent decisions are keyed on.** `FA::analyze` runs `simple_inlining` once after the first convergence, minting Vars and rewriting `rvals`; `recollect_Vars_PNodes` rebuilds the lists. `av->cs_map` is keyed on Vars, so a creation point in an inlined body re-mints; an inlined call site no longer exists for the ledger's routing. ifa/170 2b lists four bugs the post-inlining reset exposed. It is the one IR mutation inside the loop, done once rather than re-derived, and unlogged. Either a pre-FA syntactic rewrite for statically known wrappers, or a logged decision the ledger can be invalidated against. Stop condition: `--fa-inline=0` and the default give the same corpus verdicts, which today they do not. `fa.cc:14032–14060`. | design | 170 |
| C5 | **Feed FA's per-contour liveness forward.** At convergence `es->live_pnodes` (re-derived by `settle_gated_contours`, `fa.cc:13437`) says which PNodes each contour reached; `mark_live_code` starts over. A PNode outside every live set is dead by construction; seeding from their union (through `fa_live`) shortens the fixed point and removes the IF/GOTO cases for nodes FA never reached. | small | new |

## D. Code and doc health

| # | finding |
| --- | --- |
| D1 | **Both docs carry sizes and line numbers from before ifa/039 and the chain inliner.** CFG_SSU.md: `ssu.cc` 205 (603), `dom.cc` 180 (227), `loop.cc` 171 (212), `cfg.cc` 102 (129). OPTIMIZE.md: `dead.cc` 318 (491), `inline.cc` sections `12–73` / `75–330` (762 lines). Every `file:line` in the two "symptom → start here" tables is off. Neither mentions `exc_check_fold.cc`, `find_maybe_unbound`, `mark_unbound_phi_operands`, `match_prim_chain` or `sub_constants`' await exception; §8.9/8.11 detail frequency estimation's prerequisites without saying it has no consumer. Keep the gotcha lists; drop the numbers. |
| D2 | **Fixtures are enough to pin the printers, not the behaviour C would change.** Before C1 the `ssu` phase needs: a narrowed branch operand, an un-narrowed one, a loop-carried variable narrowed inside the loop, nested branches on one operand, a `while True:` opening a function. Before C2 `inline` needs identity-through-join (§8.6 says it is missed) and a chain whose intermediate is read twice. `dce` has no fixture for a setter kept live only by a reader, which A4 must preserve. |
| D3 | small: `PNode` overlays `live_vars`/`loop_node` in a union (CFG_SSU.md §7.2); `convert_to_move` leaves `p->prim` set on a MOVE; `inline_single_pnode` aliases the callee's `calls` Vec; `simple_closure_call` hard-codes the period send's operand layout; `rename_edge`'s conditional Env push/pop leaks a branch's renaming onto the non-assigning edge (ifa/039's comment), worth a note in CFG_SSU.md §4.8. |

## Plan, by value against effort

1. **A1 + A2**: flip the guard, add the join assert, run `make test`.
   Stop condition: a failure is a real splice bug, not a reason to turn
   the check back off.
2. **B1 + B4 + D1**: frequency under the `freq` phase only until it has a
   consumer; the `#if 0` block to `notes/006`; fix the docs. Stop
   condition: corpus compile times do not rise, goldens unchanged.
3. **A4 + B2**: setter temporaries get a def, `forward_live` goes; control
   dependence from the reverse frontier. Stop condition: `dce` goldens
   identical, a long-chain store fixture passes, corpus unchanged.
4. **C1**, demand-placed phy. Measure Vars per function and FA time first
   (`pyc -v -v -v` prints per-function phi, phy and CFG node counts from
   `print_ssu` and `build_cfg`).
5. **C2**, copy propagation after SSU and a direct IF on a bool operand.
   Stop condition: the nine walkers shorten or go, `peel_wrapper_def`'s
   bound is never reached, diagnostics still locate every violation.
6. **C3**, dead arms as a decision table; violations over live sets. Stop
   condition from 119: a second-order fold starts to type; `ess`/`css`
   and pass counts do not rise.
7. **C4**, decide what mid-FA inlining is.
8. **A3**, one node universe; delete the five guards. Stop condition: IR
   phases and corpus unchanged, a goto-to-self fixture refused at CFG
   build.

## Evidence

Line numbers against `829f43d`.

| claim | source |
| --- | --- |
| pass order and drivers | `ifa/ifa.cc:49–70,250–255`; `pyc.cc:290–315`; `fa.cc:14032–14060`; `fun.cc:52–73` |
| CFG build, universe | `cfg.cc` (all); `fun.cc:95–101` |
| SSU placement and rename | `ssu.cc:84–94,475–603` |
| ifa/039 dataflows | `ssu.cc:96–459` |
| dominators, loops | `dom.cc`, `loop.cc` (all) |
| DCE, forward_live, O(n²) | `dead.cc:52–319`; `fa.cc:1817–1831`; `fa_prims.cc:316–325` |
| frequency has no consumer | `inline.cc:59–118`; `grep -rn execution_frequency` → `testing/print_dce.cc` only |
| inliner splicing and bails | `inline.cc:246–329,435–536,538–725` |
| check_invariants guard | `fun.cc:327–342`; `Makefile:9,45–46`; `ifa/Makefile:6,54–55` |
| exc_check_fold by name | `exc_check_fold.cc:27–70`; `pyc.cc:314` |
| FA's consumption of phi, phy, MOVE, IF | `fa.cc:2949–3270,3282–3322,4176–4180,13391–13500` |
| MOVE-chain walkers | `fa.cc:2738,2763,7104,10206,14238`; `inline.cc:176,218,232`; `exc_check_fold.cc:29` |
| codegen materialisation | `cg.cc:2972–2984,3022–3129`; `codegen_common.cc:1842` |
| doc drift | `CFG_SSU.md:138,208,393,483–500`; `OPTIMIZE.md:35–58,214,279,424–505` |

The same review, as a page: <https://claude.ai/artifact/4rfdGwkdqJLptoZiy6aPy1>.
