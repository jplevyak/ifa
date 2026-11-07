# 204 — ifa/optimize: fixes, unchecked preconditions and dead work

**Status: open, a checklist.** Filed 2026-11-03 from
[closed/193](closed/193-review-optimize-against-analysis-2026-11.md) A1–A6
and B1–B4, and [closed/195](closed/195-what-the-concert-compiler-still-has-that-ifa-does-not.md)
C5. No known miscompile on the corpus; each row is a condition relied on
and not checked, or work nothing reads. Rows 1 and 2 are done (2026-11-06); row 3 was tried and is not a day.

## Correctness

1. [x] **`check_invariants` is compiled out in the default build.**
       Done 2026-11-06. The guard was `#ifndef DEBUG` in a build that
       always defines `DEBUG`; flipped to `#ifdef DEBUG`. Enabled, it fired
       on 7 of 15 `fa-init` fixtures, including straight-line code, and the
       bug was in the CHECK: both closures were range-for loops over a
       `Vec` that grows inside the loop, which stop at the end they saw on
       entry, so the backward walk stopped one hop from exit; the forward
       walk also walked the wrong set. Both now index the Vec. With the
       check correct, all 16 IR phases and the suite pass on both backends,
       so no splice bug was hiding behind it. A violation now names the
       function and the PNodes on each side before it asserts
       (`if1/fun.cc`).
2. [x] **The inliner splices with an unasserted precondition.** Done
       2026-11-06: `insert_move_before` asserts
       `p->cfg_pred.n == 1 || p == f->entry` and `!p->phi.n`
       (`inline.cc`). The suite and IR phases never trip it.
3. [ ] **A store's liveness is a two-hop forward probe because its temporary
       has no definition.** `P_prim_setter` / `set_index_object` stay live by
       `forward_live(tvals[0], 2)`; every other producer is kept live through
       `Var::def`, but `fill_tvals` creates the temporary with no `def`.
       OPTIMIZE.md §8.4 states the consequence (a store observed beyond two
       hops is deleted; minpng was this family). Set `tvals[i]->def = p` in
       `fill_tvals` (`fa.cc:1817`); the setter becomes an ordinary producer
       and `forward_live` is deleted. Add a fixture for a store observed
       through a long chain.

       **Tried 2026-11-06, reverted; not a day.** Setting `def` in
       `fill_tvals` itself breaks 191 compiles: FA-internal prims own
       temporaries too (`merge_in`), and a `def` made those nodes live with
       no run-time form (`_CG_prim_merge_in` undeclared). Setting it only at
       the two stores (`fa_prims.cc` `set_index_object` and `setter`) left 6
       failures with `forward_live` deleted:
       - `deepcopy_tuple_copy_of_copy`, `deepcopy_objects`: a scalar
         `P_prim_copy` node is live at codegen while its own lval is dead
         and its only use, the setter, is dead too, so codegen emits a bare
         `t2;` (`-Wunused-value`). The setter is live after `mark_live_code`
         and dead by codegen, or a `def` names a node in another clone.
         Either way, liveness reached through `Var::def` disagrees with
         liveness reached through lvals. The probe that showed it: at
         `P_prim_copy`'s scalar arm in `cg.cc`, print the node's `live`, its
         lval's `live` and each use's `live` when the lval has no C name.
       - `exception_hierarchy_base`, `open_failure_raises`,
         `raise_exception_qualified`, `raise_string`: a class-tag test
         names exception type objects (`_CG_type_IOError_…`) that were never
         emitted, so a store becoming live widened some receiver without
         `mark_live_types` seeing the class.
       Stop condition for the next attempt: explain the copy node (which
       clone owns the `def` that revived it) before changing anything else.
       One run of the deepcopy binary segfaulted out of 46; not reproduced.
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
       ([issues/172](../../issues/172-stopiteration-value-reads-the-global-exception-slot.md))
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
