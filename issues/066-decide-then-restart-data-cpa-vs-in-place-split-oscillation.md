# 066 — pyc's ES/CS-split oscillation is architectural; shedskin's decide-then-restart data-CPA phase is the real fix

**Status:** open, framed 2026-07-23 after reading shedskin's
`shedskin/infer.py` to see how it handles the same data-polymorphism
problem pyc oscillates on. This reframes the whole
[063](063-no-type-bucket-triage.md) / [065](065-mark-stage-es-split-routing-and-growing-product.md)
/ [033](033-splitter-non-idempotent-divergence.md) family: the fix pyc
actually wants is an **architectural** one (decouple the split decision
from the forward fixpoint and restart), not more in-place-split routing
tuning.
**Affects:** `ifa/analysis/fa.cc`'s entire in-place splitting model —
`decide_entry_set_split` / `split_edges` / `apply_entry_set_split`, the
issue-033 product-routing ledger, and the setter-site signature
([065](065-mark-stage-es-split-routing-and-growing-product.md)).

## The problem, restated

pyc reaches convergence and refines its EntrySet/CreationSet partition in
**one interleaved forward fixpoint**: it splits ESs/CSs *in place while
types are still flowing*. A split perturbs the flow, which changes what
needs splitting, which perturbs the flow again — the non-idempotent
divergence of issue 033. Concretely (dijkstra2 + object comparison, and
`recursive_polymorphic` with methods `nd 0`): a shared container method
over a heterogeneous-element union has its split products **re-mint every
pass** because the union is still growing when the split decision is
made. Every fix attempted in 063/064/065 (nesting_depth, mark-stage
routing, self-product keep, setter-site signature) is a patch on this
in-place model; the setter-site signature (landed) removes one class of
re-mint but cannot stop a union that is still widening as it splits.

## How shedskin solves the same thing (`shedskin/infer.py`)

Shedskin runs the SAME two analyses pyc descends from — Agesen's CPA for
function polymorphism and Plevyak's IFA for data polymorphism — but
keeps them on **separate axes and separate schedules**. Every
constraint-graph node carries `(dcpa, cpa)`: a class/data-duplicate
number and a function-duplicate number.

- **Function polymorphism → CPA, during propagation** (`cpa`, infer.py:1248):
  a function is duplicated per cartesian product of its argument types.
  Monotone.
- **Data polymorphism (container element types) → IFA, in a separate
  BACKWARD phase** (`ifa`, infer.py:1469) run on the CONVERGED graph, then
  applied by **restarting the whole analysis**.

The driver `iterative_dataflow_analysis` (infer.py:1800):

```
backup = backup_network(gx)          # save the initial graph
while True:
    propagate(gx)                    # FORWARD: types to a FIXPOINT. No class splitting.
    split = ifa(gx)                  # BACKWARD: on the converged graph, decide dcpa splits
    if not split: break              # converged
    ... bake splits into gx.alloc_info (alloc-site -> new dcpa); class_copy(...)
    restore_network(gx, backup)      # RESET the graph and re-run from scratch
```

`ifa_flow_graph` (infer.py:1715) is the backward-flow-to-creation-points
step pyc keeps reaching for: it groups a container var's incoming writes
by element type (`assignsets`), traces each backward to its **creation
points** (`backflow_path`; allocation sites are nodes with no inputs),
finds **confluence points** where creation sites of *different* element
types merge (`ifa_confluence_point`, infer.py:1703), and partitions the
creation sites by element type, giving each partition a fresh dcpa
(`ifa_split_class`, infer.py:1773 → `cl.newdcpa += 1`). Duplicating the
CLASS per dcpa (`class_copy`, infer.py:788) makes every method of that
copy monomorphic in its element type automatically — no per-method
in-place fan-out is ever needed.

## Why shedskin doesn't oscillate — the crux

The split decision is made **once per round on a fully converged,
stable graph**, never against a half-formed union. Propagation within a
round is monotone (no splitting), so it converges cleanly; then the
entire element-type partition is decided; then the graph is **thrown
away and rebuilt** with the partition baked into a persistent
`alloc_info` (allocation-site → dcpa) table. Each round strictly adds
dcpa copies until no confluence remains. There is no "split perturbs the
in-flight fixpoint" feedback loop because there is no in-flight split.

It also avoids the per-container blow-up (pyc's 200/27 when `list` was
flagged `clone_methods_per_cs`, see 065) two ways: `ifa()` splits only at
**actual confluence points** (divergent element types actually meeting),
and a `CPA_LIMIT` + incremental seeding bound the cartesian product.

## The implication for pyc

The "CS-directed ES fan-out" 065 concludes pyc needs *is*
`ifa_split_class` (allocation-site → dcpa). But the real lesson is
architectural, and it retires the in-place-tuning direction:

1. **pyc's oscillation is inherent to splitting in place during the
   fixpoint.** No amount of routing-signature work (issue 033 ledger,
   setter-site signature) removes it, because the instability is that the
   partition is decided against a still-growing union. Those patches cap
   symptoms; they can't make a mid-fixpoint split idempotent.
2. **The fix is a decide-then-restart data-CPA phase.** Run FA to a
   fixpoint WITHOUT CS/element splitting; in a backward pass, group each
   container CS's element writes (setters) by type, trace to creation
   points, and decide a per-creation-point CS partition on that stable
   graph; then RESET and re-run with the partition pinned in a persistent
   creation-site → CS-duplicate map (pyc's analog of `alloc_info`).
   Repeat until no element-type confluence remains.
3. **This dissolves the downstream issues.** With container CSs
   separated per element type at their creation points, a container
   method's receiver is monomorphic — the union that drives dijkstra2's
   stall never forms, and level-descending recursion's per-level
   separation comes from the dcpa partition, so the method display
   ([064](064-method-phantom-display-blocks-es-split-routing.md)) is no
   longer load-bearing and methods can be `nesting_depth 0` (display
   reserved for real nested functions, as intended).

## Cost / feasibility

This is a large change to FA's control structure: FA currently interleaves
splitting into its worklist fixpoint; a decide-then-restart model needs
(a) a clean "propagate to fixpoint with splitting disabled" mode, (b) a
persistent creation-site → CS-duplicate map that survives a graph reset
(pyc already has hints of this — `saved_scopes`, the CS ledger's
cross-pass identity), and (c) a backward element-confluence pass mirroring
`ifa_flow_graph`. It is a rewrite of the CS-splitting half of the
splitter, not a patch — but it is the shape that provably converges
(shedskin compiles the whole corpus this bucket is drawn from). The
already-landed setter-site signature and the setter→creation-point CS
split are reusable pieces: the confluence detection and the
setter-class → creation-site backward walk are exactly what
`ifa_flow_graph` does; what changes is doing it on a converged graph and
applying it via restart instead of in place.

## What this unblocks

The entire "no type" / oscillation bucket that in-place splitting can't
close: dijkstra2 and the object-comparison default (063/064), the
container-element union family (043 shape B), and the issue-033
non-idempotent divergence at its root — all of which are the same
in-place-split instability shedskin structurally avoids.
