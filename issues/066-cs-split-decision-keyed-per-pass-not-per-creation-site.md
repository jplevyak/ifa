# 066 — The split oscillation is a durable-decision *keying* bug, not an architecture gap: CS identity is per-pass, should be per-creation-site

**Status:** open, framed 2026-07-23 (reading shedskin's `shedskin/infer.py`
to see how it handles the same data-polymorphism problem), **corrected
2026-07-23** after tracing pyc's actual pass structure. The first draft
claimed pyc splits "in place during the fixpoint" and needs an
architectural rewrite to shedskin's decide-then-restart model. **That was
wrong** — pyc already IS decide-then-durable. The real bug is narrower and
lives inside the existing loop: the durable identity of a CreationSet
split is keyed on the re-created per-pass CS, not on the stable
creation site, so re-flow re-derives it (the issue-033/065 oscillation).
This reframes [063](063-no-type-bucket-triage.md) /
[064](064-method-phantom-display-blocks-es-split-routing.md) /
[065](065-mark-stage-es-split-routing-and-growing-product.md) /
[033](033-splitter-non-idempotent-divergence.md).
**Affects:** `ifa/analysis/fa.cc` — `clear_results`/`clear_cs`,
`creation_point`'s split-parent CS reuse, the `cs->split` lineage, and the
issue-033 product-routing ledger (incl. the landed `setter_site_signature`).

## pyc is already decide-then-durable (the corrected picture)

The analysis flows to a fixpoint, splits durably, clears the *flow* (not
the contours), and re-flows:

- `clear_results()` (fa.cc) keeps `fa->ess`/`fa->css` and the `cs->split`
  lineage; it clears only AVar types/args/rets and per-contour results.
- **`clear_edge` does NOT clear `e->to`** — an edge's ES membership
  survives a clear — and `make_entry_set` short-circuits
  `if (e->to) return;`. So the **ES side is durably associated**: existing
  call edges stay pinned to their split EntrySet across re-flow.
- `run_split_stages` is **gated** (`if (!analyze_again)`): exactly one
  stage fires per pass, ES stages (type/mark) prioritized over the
  setter/CS stages, and within `split_for_setters`, `split_css` runs only
  if `split_ess_setters` found nothing. So a single pass never splits both
  an ES and a CS.

There is no "split mid-fixpoint." Architecturally this is the same shape
as shedskin's `iterative_dataflow_analysis` (propagate → decide → re-run).

## The actual bug: the CS side is NOT durably associated

The asymmetry is the whole story. `clear_cs` (fa.cc) wipes `cs->defs`,
`cs->ess`, and `cs->creates`. So unlike an edge's `e->to`, a
CreationSet's **membership is re-derived from scratch every re-flow** —
via `creation_point`'s split-parent reuse plus the `cs->split` lineage.
When that re-derivation lands differently, or a re-triggered split mints a
fresh product instead of recognizing the prior decision, the partition
churns — the re-mint / growing-union oscillation of
[065](065-mark-stage-es-split-routing-and-growing-product.md).

The issue-033 ledger and the landed `setter_site_signature` are attempts
to stabilize that re-derivation, but they key on the **shifting per-pass
ES/CS identity**, which is exactly why they only partly work (065's
"setter-class identity is per-(Var,EntrySet), unstable across passes").

## The cross-pass ES↔CS coupling (the phase-ordering half)

Because a CS split changes types, it re-opens a *type* confluence → an ES
split next pass → which changes a setter confluence → a CS split again.
The gating prevents both in one pass, but the passes ping-pong. This is
the coupling that "reach the ES-split fixpoint, then split CSs" targets:
once a CS partition is durably fixed, re-flow reproduces it deterministically
and does not keep re-opening already-decided ES splits, so refinement is
monotone (each round strictly adds decisions) instead of oscillating.

## What shedskin does differently — and it's just the key

Shedskin runs the same two analyses (Agesen CPA for functions, Plevyak
IFA for data), and its loop (`iterative_dataflow_analysis`, infer.py:1800)
is the same decide-then-re-run shape. The one thing it does that pyc
doesn't is **key the durable data-split decision on the source allocation
site**: `gx.alloc_info: (func, cartesian-ctx, alloc-node) → (class, dcpa)`.
`ifa()` (infer.py:1469) decides the element-type partition on the
converged graph — `ifa_flow_graph` (infer.py:1715) groups a container
var's writes by element type, traces each back to its creation points
(`backflow_path`; allocation sites have no inputs), finds confluence
points (`ifa_confluence_point`, infer.py:1703), and assigns each partition
a fresh dcpa (`ifa_split_class`, infer.py:1773). On the next round the
graph is rebuilt and each allocation site takes its dcpa from `alloc_info`
**deterministically**, because a source site is stable IR. That determinism
is the entire difference: pyc's `cs->split`/ledger identity is the
re-created CS, shedskin's is the source site.

## The fix (small, inside the existing loop)

Not a rewrite of the splitter's control structure — a re-keying of the
durable CS store plus the phase ordering:

1. **Durably associate re-derived CSs by a stable creation-site key.**
   Give the CS split a persistent `creation-site → CS-duplicate` map (the
   `alloc_info` analog), populated when a split is *decided* and re-applied
   verbatim by `creation_point`/`clear_cs` re-flow instead of re-derived.
   This is the generalization of `setter_site_signature` from "a routing
   hint scored each pass" to "the durable identity of the split." The
   confluence detection and setter→creation-point backward walk it needs
   already exist (`collect_setter_confluences`, `compute_setters`, the
   setter-site signature); what changes is that the decision is *stored*
   against the creation site and *pinned*, not recomputed.
2. **Reach the ES-split fixpoint, then split CSs, and don't re-open decided
   ES splits.** The `e->to` durability already nearly gives this for ESs;
   the missing half is not letting a CS split re-derive an ES split that a
   stable-keyed decision already made.

With the CS partition pinned by creation site, a container's method
receiver becomes monomorphic in its element type, so: dijkstra2's
container-element union never forms, level-descending recursion's
per-level separation comes from the CS partition (not the method display,
so [064](064-method-phantom-display-blocks-es-split-routing.md) dissolves
and methods can be `nesting_depth 0`), and [043](closed/043-empty-container-inference-options.md)
shape B is subsumed.

## What this unblocks

The entire "no type" / oscillation bucket that the current per-pass CS
keying can't stabilize — dijkstra2 and the object-comparison default
(063/064), the container-element union family (043 shape B), and the
issue-033 non-idempotent divergence at its root. The corpus evidence that
the *decide-then-durable-with-stable-keys* shape converges is shedskin,
which compiles the whole corpus this bucket is drawn from.
