# 195 — what the Concert compiler (1992–96) still has that ifa does not

**Status: open. Umbrella**, like [146](146-remove-all-arbitrary-splitting.md).
Written 2026-11-03 from a reading of the Illinois Concert compiler's
Lisp sources (84 files, 49k lines; `flow-analysis.lisp`, `concert-db.lisp`,
`clone.lisp`, `inline.lisp`, `cleanup-program-graph.lisp`,
`ip-constant-propogation.lisp`, `object-pruning.lisp`, `top-level.lisp`
read in full, the rest by header). The sources carry a University of
Illinois confidential-distribution header: every row below is an idea to
re-implement in ifa's own terms, never code to port. Nothing was run.
Close this issue by filing or fixing each row and striking it.

## Verdict

`flow-analysis.lisp` is ifa's ancestor and almost all of it is in the tree
in a stronger form: the confluence walk, the split ladder with the cascade
gate (`extend-type-analysis` runs each split kind only `when (not
*type-extend-again*)`), recursive-edge exclusion by SCC over the ES × CS
graph, the "remainder stays" rule in `split-caller`, the pass cap
(`*type-extend-inference* 30`). Five mechanisms are not in the tree. Two of
them are the things three reviews already asked for
([192](192-review-analysis-and-issues-2026-11.md) D2 and D4,
[193](193-review-optimize-against-analysis-2026-11.md) C1 and C2), one
answers [050](050-FA-general-constant-propagation-unreachable-code.md),
one extends closed/123, and one is a cheap analysis mode.

## What NOT to take

- **Contour identity.** Concert keys a creation set on (allocation site ×
  caller entry set) (`make-the-cid`) and requires an entry set's receiver
  creation set to match before anything else (`check-entry-set`,
  `find-entry-set-internal`). That is the identity ifa/128 replaced and the
  receiver-keyed method contour [146](146-remove-all-arbitrary-splitting.md)
  lists as a dead end. AGENTS.md's directives supersede the 1995 design; the
  rows below are independent of it.
- **`clear-constraints`** clears only the type variables transitively
  reachable from the split entry sets: the selective invalidation
  [closed/111](closed/111-FA-selective-invalidation-per-pass.md) measured and
  closed as not needed.
- The concurrency machinery (`touch.lisp`, `extend-region.lisp`, mega-locks,
  locality splitting, `structure-transformation.lisp`) is the distributed
  runtime's and has no counterpart here.
- `dead-code-elimination.lisp` and `copy-propagation.lisp` are RTL-level
  passes inside code generation (`code-generate.lisp:239,762`); `dead.cc`
  already does more at the IF1 level.

## The five

| # | Concert mechanism | what it buys ifa | feeds |
| --- | --- | --- | --- |
| C1 | **concert-db: a decision snapshot across compiles.** `write-ti-info` serialises, per method, the equivalence classes of entry sets (after `calculate-clones #'equiv-es-write-ti`) with their edges and creation-set ids, and per class the creation sets with their creation points. `find-es-from-db` / `find-cs-from-db` consult it on **pass 1 only**, before the split ladder runs. Two details improve on 192 D2's sketch. **Edit-stable ids** (`program-point-id`, `id-program-points`): a call site is `selector%nargs%depth`, a loop is named by its controlling variable, collisions broken by source character position; nothing depends on allocation order, so a snapshot survives an edit elsewhere. closed/147 made ids deterministic across *builds*; this makes them stable across *edits*, which an edit-compile loop needs. **It stores equivalence classes, not raw contours**, so it re-seeds the coarsest partition the program needed and the next run splits further only on a demand; raw contours would re-seed the ones [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) says were minted on transient types. ifa's keys are already close: `AEdge` on `(fun, pnode, from)`, `SplitDecision` on `(fun, stage, pos, partition)`; what is missing is a stable name for a PNode and for a partition's CreationSet. | warm start; most of a large program's convergence | 192 D2, [notes/001](../notes/001-compilation-database.md) |
| C2 | **Profile channel.** The same file writes `PCE` (per-method) and `PL` (per-loop) counts from the generated program and reads them back onto `:prof-count`. In the snapshot nothing consumes them, but the plumbing is the thing `frequency_estimation` stands in for. | a consumer for frequencies, or their deletion | 193 B1 |
| C3 | **`remove-excess-moves-and-phys` before FA and after inlining.** `short-out-phys` substitutes a phy's source for every lval use; `remove-dead-phys`; then `move-eliminator` (`through-move`, copy propagation) and `remove-dead-phis` (a phi whose operands are one value becomes a move) iterate to a fixed point (`cleanup-program-graph.lisp:196–300`). Two guards in `through-move` are the ones ifa needs: do not propagate through a copy whose type differs from its source by more than `nullclass` (the narrowing case, the one place a copy carries information), and never short-circuit a phi through another phi on the same conditional. Concert placed phys everywhere and deleted the ones nothing needed; 193 C1 proposed placing them on demand. Same invariant: a branch rename survives only where a type can differ across it. | fewer Vars per contour per pass | 193 C1, C2 |
| C4 | **Write-once globals** (`write-once`, `distribute-write-once-globals`, `read-local-write-once-globals`). A global whose every `set_global` edge is sourced inside the initialisation entry sets is write-once; every read outside those entry sets is rewritten from a send to a direct load. `tests/global_slot_call_graph_precision.py` (050) is this shape. ifa has the hook (`provably_constant_load`) and the call-graph facts; it lacks the classification. `ip-constant-node` also turns a global read whose only reaching definitions are one numeric constant into a move. | 050 stages 2–3 for the common case | 050 |
| C5 | **`dead-object?`** (`object-pruning.lisp:17–103`): an allocation whose every reaching use is a store into it, a move of it, or a free is dead, and `prune-dead-objects` removes the closure. `mark_live_avars` keeps an object live if any field is live and has no written-never-read rule for whole objects. With method slots 93–98 % unread (closed/123), write-only objects are the next thing to measure. | fewer allocations in emitted code | closed/123, 193 §6.5 |
| C6 | **Cheap analysis modes beside the precise one.** `fast-types` is one entry set per (method × receiver creation set) with no splitting; `palsberg-types` is per call site. One function selects a mode; every transfer function is shared (`extended-infer`, `find-entry-set`). A `--fast` that skips `extend_analysis` is the cheapest partial-result contract 192 D4 asked for. | a time budget with a defined answer | 192 D4 |

Smaller, one line each:

- `is-recursive-edge-low` takes a **level**; `extend-type-analysis` retries a
  type split one recursion level deeper ("kick up to level 2") when level 1
  found nothing. ifa's `recgate` has no second level.
- `switch-dispatches` emits a tag switch only when the candidate count is at
  most `*switch-dispatch-limit*` (5): [030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md)'s missing threshold.
- `fit-size` (`inline.lisp:589`) gates inlining on callee size, inline
  depth, and whether the site is in a loop; `simple_inlining` has none of
  these, which is fine while it inlines only wrappers.
- `ca-transform` runs `remove-excess-moves-and-phys-all` twice before
  inlining and `fixup-after-inlining` per method after, each followed by
  `recompute-reaching-definitions`: the discipline 193 C4 asks of mid-FA
  inlining.

## Design for C1, as far as the reading settles it

1. **Name the program points.** A `PNode`'s stable name is its enclosing
   function's qualified name, the send's selector (or primitive name),
   arity, and nesting depth; ties by source line and column, then by order.
   A `Fun`'s name is its qualified name plus a hash of its IF1 body
   (`ifa_dbg_bodies` already computes one). A `CreationSet` partition is
   named by the stable names of its creation points.
2. **Write after convergence, from the clone equivalence** (closed/121's
   `compute_clone_equivalence`): per Fun, each equivalence class of
   EntrySets as the set of `(caller pnode name, caller class id)` edges; per
   class, each CreationSet class as its creation-point names; the ledger's
   `SplitDecision`s by `(fun name, stage, position, partition name)`.
3. **Read on pass 1 only**, in `find_best_entry_sets` and `creation_point`,
   before any compatibility scoring, for a Fun whose body hash is unchanged;
   a changed Fun starts minimal. Every pass still re-derives from bottom, so
   a stale seed costs precision for one pass and never correctness.
4. **Stop condition** (192 D2): a second compile of an unchanged program
   converges in one pass with identical C; a one-function edit reconverges
   in fewer passes than cold and gives C identical to cold.

## Evidence

| claim | source (Concert, `concert.tar`) |
| --- | --- |
| snapshot write and read | `concert-db.lisp:105–149,151–247,584–617` |
| edit-stable ids | `concert-db.lisp:298–351` |
| pass-1-only seeding | `flow-analysis.lisp:3584–3617` |
| profile channel | `concert-db.lisp:48–77,168–211`; `code-generate.lisp:228` |
| phy/move cleanup | `cleanup-program-graph.lisp:196–430` |
| write-once globals | `ip-constant-propogation.lisp:124–245,249–274` |
| dead objects | `object-pruning.lisp:17–146` |
| analysis modes | `flow-analysis.lisp:46–81,2121–2139` |
| split ladder and cascade gate | `flow-analysis.lisp:959–1080` |
| recursion level | `flow-analysis.lisp:1773–1807,1881–1915` |
| identity (not to take) | `flow-analysis.lisp:352–362,2331–2336,2442–2467` |
| pass order | `top-level.lisp:274–393` |
