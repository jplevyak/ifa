# 204 — ifa/optimize: fixes, unchecked preconditions and dead work

**Status: open, a checklist.** Filed 2026-11-03 from
[closed/193](closed/193-review-optimize-against-analysis-2026-11.md) A1–A6
and B1–B4, and [closed/195](closed/195-what-the-concert-compiler-still-has-that-ifa-does-not.md)
C5. No known miscompile on the corpus; each row is a condition relied on
and not checked, or work nothing reads. Rows 1–3 are a day.

## Correctness

1. [ ] **`check_invariants` is compiled out in the default build.** Its
       body is under `#ifndef DEBUG`; both Makefiles set `DEBUG=1` and
       pass `-DDEBUG=1`, so the four calls in `inline.cc` and the one in
       the `Fun` constructor are empty in every normal build and in CI.
       Flip to `#ifdef DEBUG`; run the IR phases. Stop condition: a failure
       is a real splice bug, not a reason to turn the check back off.
       (`fun.cc:327–342`, `Makefile:9,45`.)
2. [ ] **The inliner splices with an unasserted precondition.**
       `insert_move_before` rewires every pred of `p` to the new MOVE and
       sets `p->cfg_pred` to one node; at a join the phi `rvals` would stay
       sized to the old pred count. Safe only because joins are `LABEL`
       PNodes and the inliner inserts before a `SEND`. Assert
       `p->cfg_pred.n == 1 || p == f->entry` and `!p->phi.n`
       (`inline.cc:246`).
3. [ ] **A store's liveness is a two-hop forward probe because its temporary
       has no definition.** `P_prim_setter` / `set_index_object` stay live by
       `forward_live(tvals[0], 2)`; every other producer is kept live through
       `Var::def`, but `fill_tvals` creates the temporary with no `def`.
       OPTIMIZE.md §8.4 states the consequence (a store observed beyond two
       hops is deleted; minpng was this family). Set `tvals[i]->def = p` in
       `fill_tvals` (`fa.cc:1817`); the setter becomes an ordinary producer
       and `forward_live` is deleted. Add a fixture for a store observed
       through a long chain.
4. [ ] **Three passes agree on the node universe by accident.**
       `collect_PNodes` walks backward from exit, `build_dominators` forward
       from entry, `remove_unreachable` prunes only forward-unreachable
       preds. A region reachable from entry with no path to exit is outside
       the universe but has edges into it; `dom.cc`, `loop.cc` (twice) and
       `inline.cc` each carry a null guard for that shape. Define the
       universe once in `finalize_cfg` (nodes on some entry-to-exit path),
       prune there, delete the five guards. Stop condition: IR phases and
       corpus unchanged; a goto-to-self fixture is refused at CFG build.
5. [ ] **`exc_check_fold.cc` decides by a Python name inside the library**
       (`strcmp(sym->name, "__pyc_exc__")`, called from `pyc.cc`). Mark the
       Sym from the frontend (`Sym::is_exception_slot`) and test the bit, or
       move the pass behind `IFACallbacks`. The pending-exception threading
       ([issues/172](../../issues/172-generator-send-channel-and-stopiteration-value.md))
       touches this anyway.
6. [ ] **Three traversals recurse to the graph's depth**: `mark_live_avar`
       over `backward`, `rename_edge` over `cfg_succ` (one frame and an
       `Env` push per CFG node; othello3's 1,848-store function is a frame
       per statement), `df_traversal`. Worklists, as ifa/186 did for FA.

## Dead and redundant work

7. [ ] **Frequency estimation and the call-graph loop tree have no
       consumer.** `frequency_estimation` runs `find_all_loops` (a loop tree
       per clone plus `build_call_dominators` over every clone) and writes
       `execution_frequency`; the only reader is `testing/print_dce.cc`;
       `false_branch_frequency` is never written. Either give it a consumer
       (a `__builtin_expect` on the emitted `if`; a size × frequency gate on
       the inliner; or the profile channel of
       [198](198-decision-snapshot-across-compiles.md)) or run it only under
       the `freq` test phase.
8. [ ] **IF and GOTO liveness are O(n²)** (`dead.cc:214,245`): a scan of
       `fa_all_PNodes` with two dominance tests per node inside the fixed
       point. The reverse dominance frontier (`rdom->front`) already names
       the branches a node is control-dependent on; walking it from each
       newly live node is linear.
9. [ ] **Dominators are built twice per function; `collect_Vars` up to six
       times** (`build_ssu`, `Fun::copy`, once per inline pass per function,
       `recollect_Vars_PNodes`). A dirty bit on `Fun`.
10. [ ] **Dead-object pruning** (closed/195 C5): an allocation whose every
        reaching use is a store into it, a move of it, or a free is dead.
        `mark_live_avars` keeps an object live if any field is live and has
        no written-never-read rule for whole objects. Measure how many
        write-only objects the corpus allocates before building it.

## Verification

Each row: `dce`, `ssu`, `inline`, `dom`, `loops` goldens identical unless
the row is about them; `make test` green on both backends; corpus `check`
verdicts identical per program.
