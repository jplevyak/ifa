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

## Measured 2026-11-11: how much of flow analysis runs library code

**Probe** (temporary, removed). Every item the worklist loop processes was
timed and charged to the function whose code it interprets: a call edge to
its callee, a send and a contour's constraints to the function that
contains them. Functions are classed by source file: `__pyc__` (the
builtin library), `pyc_lib/` (the standard-library modules), or the
program. All 77 corpus programs. The loop is about half of flow analysis;
the split stages, `complete_pass` and the per-pass reset are outside it
and were not attributed.

| | worklist time | in `__pyc__` | in `pyc_lib` | in the program |
| --- | --- | --- | --- | --- |
| corpus, all passes | 168.6 s | 40% | 2% | 57% |
| corpus, passes 0–3 | 70.6 s | 41% | 2% | 57% |
| sunfish | 18.6 s | 81% | 4% | 16% |
| plcfrs | 19.5 s | 56% | 1% | 42% |
| othello3 | 59.5 s | 21% | 0% | 78% |
| hq2x | 8.8 s | 2% | 0% | 98% |
| the median program | | 53% | | |

- **Half of a typical program's analysis is the library's.** The corpus
  total is 40% because othello3 and hq2x, which are the two largest and
  are dominated by their own code, pull it down.
- **It is the same share in the first four passes as overall.** The
  library is not a start-up cost that splitting then removes. It is
  re-analysed, contour by contour, in every pass.
- **Inside the library the time is contour construction**
  (`add_es_constraints`): 8.5 s of sunfish's 15 s there, 8.5 s of
  othello3's 12.3 s. A send that sits in user code and dispatches into the
  library is charged to the user here, so the library's share of
  *matching* is understated.

**What it means for this issue.**
- **Scope the first snapshot to `__pyc__`.** Its functions' bodies are the
  same in every compile, so step 1's edit-stable names and body hashes
  are trivially stable, and the snapshot can be built once, when pyc is
  built, from a corpus run. It is then a warm start for every program on
  its first compile, which the per-program snapshot cannot be.
- **The bound is the median program's 53%, and the snapshot reaches less
  than that.** A decision snapshot removes passes (the re-derivation that
  places a split the library always needs), not the cost of analysing a
  library contour once. What it can save is the library's share of the
  passes after the first, on programs whose pass count is set by library
  splits. That was not measured: the probe did not record which function
  each split was in. **Count splits per pass by library and program
  before building anything.**
- **Summaries are the other consumer of this number.** A library function
  whose result is a function of its arguments' types does not need a
  contour at all ([GENERICS.md](../../GENERICS.md), G1). The 40% is the
  upper bound for that too, and GENERICS.md's own estimate for othello3
  (20% of FA) is close to othello3's 21% here.

## Stop condition

A second compile of an unchanged program converges in one pass with
identical C. A one-function edit reconverges in fewer passes than cold and
gives C identical to cold. If either fails, the seed is being treated as
a constraint instead of a decision; find where.

## Verification

The two stop-condition tests as fixtures; corpus `check` with the snapshot
present is identical in every verdict and contour count to a cold run
(the seed must change time, never the answer).
