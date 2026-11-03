# 198 — a decision snapshot across compiles (warm start)

**Status: open, not started.** Filed 2026-11-03 from
[closed/192](closed/192-review-analysis-and-issues-2026-11.md) D2 and
[closed/195](closed/195-what-the-concert-compiler-still-has-that-ifa-does-not.md)
C1 and C2. The deleted compilation database is
[notes/001](../notes/001-compilation-database.md); its stated prerequisite,
deterministic ids, was met by closed/147.

## Why

Every pass re-derives types from bottom, and nothing is carried between
compiles. A 13-pass othello3 pays 90–250 s per pass, and an edit-compile
loop pays all of it again. The decisions that persist across passes
(`av->cs_map`, `AEdge::to`, the split ledger keyed on interned
`(fun, stage, pos, partition)`) are exactly what a snapshot would store.
Because only decisions persist, a stale snapshot costs one pass of
precision and never correctness.

## Design (from the Concert ancestor's `concert-db`, closed/195)

1. **Name the program points, edit-stably.** A `PNode`'s name is its
   function's qualified name, the send's selector or primitive name, its
   arity and nesting depth; ties broken by source line and column, then by
   order. A `Fun`'s name is its qualified name plus a hash of its IF1 body
   (`ifa_dbg_bodies` computes one). A CreationSet partition is named by
   its creation points' names. Nothing depends on allocation order, so a
   snapshot survives an edit elsewhere in the file.
2. **Write after convergence, from the clone equivalence**
   (`compute_clone_equivalence`, closed/121): per Fun, each equivalence
   class of EntrySets as its set of `(caller pnode name, caller class)`
   edges; per class, each CreationSet class as its creation-point names;
   the ledger's `SplitDecision`s by `(fun name, stage, position, partition
   name)`. Storing classes, not raw contours, re-seeds the coarsest
   partition the program needed; raw contours would re-seed the ones
   [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)
   says were minted on transient types.
3. **Read on pass 1 only**, in `find_best_entry_sets` and
   `creation_point`, before any compatibility scoring, for a Fun whose
   body hash is unchanged; a changed Fun starts minimal.
4. **Profile channel** (optional, second step): the generated program
   writes per-function and per-loop counts; the snapshot reads them back.
   That is the consumer `frequency_estimation` never had
   ([204](204-optimize-pass-fixes.md)).

## Stop condition

A second compile of an unchanged program converges in one pass with
identical C. A one-function edit reconverges in fewer passes than cold and
gives C identical to cold. If either fails, the seed is being treated as
a constraint instead of a decision; find where.

## Verification

The two stop-condition tests as fixtures; corpus `check` with the snapshot
present is identical in every verdict and contour count to a cold run
(the seed must change time, never the answer).
