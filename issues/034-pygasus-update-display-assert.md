# Issue 034: update_display assert on pygasus (unmasked by the stall guard)

**Status:** open, undiagnosed. Newly *visible*, not newly
*introduced*: before the issue-033 stall guard, pygasus diverged in
the splitting loop and timed out before ever reaching this state.
**Affects:** `ifa/analysis/fa.cc:update_display()` (assert at
fa.cc:909, `es->display[i] == e->fun` invariant).
**Related:** [033-splitter-non-idempotent-divergence.md](033-splitter-non-idempotent-divergence.md)
(the guard that exposed it); possibly the emptied-split-ES crash
family (pyc issue 025: pystone, tictactoe, sudoku2's unique_AVar
assert) — all are "stale contour structure after aggressive
splitting" shapes.

## Symptom

```
$ pyc -D <root> shedskin_examples/pygasus/pygasus.py
pyc: analysis/fa.cc:909: void update_display(AEdge *, EntrySet *):
     Assertion `es->display[i] == e->fun' failed.   (SIGABRT)
```

Reproduces deterministically in ~seconds. Previously this input ran
until the 180s sweep timeout, so the sweep classified it "compile
timeout" and nobody saw the assert.

## What is known

- `update_display` maintains the per-EntrySet lexical-display array
  (enclosing-function chain) when an edge is analyzed into an ES.
  The assert says an ES is being reused for an edge whose function
  chain disagrees with the display already recorded — i.e. an ES
  outlived (or was re-matched across) a split that should have
  separated the two callers, or the display was stamped from a
  contour that a later pass restructured.
- Given issue 033 (split state cleared per pass, decisions
  re-derived), a stale display is plausible wherever ESs are
  recycled across passes while their edge sets are rebuilt.
- kmeanspp dies with SIGSEGV shortly after its no-type diagnostics
  on the same corpus run; not yet confirmed whether it shares this
  root.

## Next steps

1. Reproduce under a debugger; capture `es->id`, `e->fun`,
   `es->display[i]`, and `analysis_pass` at the assert. Determine
   whether the ES was created in an earlier pass than the edge.
2. Check whether the ES was a split product (`es->split` lineage)
   whose display was copied from the parent before the parent's
   edges were re-partitioned.
3. If it is the cross-pass-reuse shape, the issue-033 "real fix"
   (persistent, keyed splits) likely subsumes it; otherwise
   update_display needs to re-stamp (not assert) when an ES is
   re-entered with a different but valid chain.

## What it unblocks

pygasus (shedskin corpus, pyc issue 025), and possibly the wider
emptied-split-ES crash family if the roots coincide.
