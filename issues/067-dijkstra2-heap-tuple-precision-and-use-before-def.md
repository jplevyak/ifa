# 067 — dijkstra2's "no type" is a use-before-def masking a heap-tuple positional-precision loss, NOT a CS-keying case

**Status:** root-caused 2026-07-23 by direct measurement, while
preparing to implement [066](066-cs-split-decision-keyed-per-pass-not-per-creation-site.md).
Filed to correct the record: dijkstra2 was used as the headline repro
for the "no type" / oscillation bucket in
[063](063-no-type-bucket-triage.md)/[064](064-method-phantom-display-blocks-es-split-routing.md)/[065](065-mark-stage-es-split-routing-and-growing-product.md)/066,
but the measured root is a different tangle and those issues' dijkstra2
framing is wrong. **Affects:** the `shedskin_examples/dijkstra2` repro
and, at root, `ifa/analysis/fa.cc`'s tuple-comparison / tuple-in-container
precision — the [057](057-sorted-tolist-fa-nonconvergence.md) family.

## Measured facts (with `-l s`)

`pyc -D <root> dijkstra2.py` stalls with 12 residual violations, the
visible one being `warning: 'wt' has no type` — `wt` from
`wt, nodes = bidirectional_dijkstra(G, (0,0), (n-1,n-1))` (line 108).

- **`split_css` never fires.** All 120 `[scss]` probes are
  `starter_set=1 defs=1`; **0 `SPLIT CS`**, **0 CS-side ledger
  re-derivations** (`cs_dup_split_attempts == 0`). The CreationSet side
  is completely stable on this program — so 066's CS-keying lever is not
  exercised at all here.
- The re-split churn is **100% ES-side** on shared container methods
  (`__getitem__` ×90, `__eq__` ×45, `len` ×28, `__lt__` ×22,
  `__setitem__` ×19, …). Only 6 of those are caught as ES-ledger
  re-derivations (`__pyc_to_bool__`), blocked from routing by
  `group_display_ok` (064).

## The actual dependency chain

`bidirectional_dijkstra` (dijkstra2.py:37) has three return shapes —
`return (0.0, [source])`, `return (finaldist, finalpath)`, and
`return None` — and:

1. **`finaldist` is used before assignment.** Its initializer
   `#finaldist = 1e30000` is **commented out** in the vendored source;
   `finaldist` is first *read* in `finaldist > totaldist` and only
   assigned inside that branch. At runtime the `finalpath == []`
   short-circuit hides it on the first iteration; statically it means
   the returned tuple's slot 0 can be untyped, and the `wt` unpack at
   line 108 has no type. **This is the surface cause and it masks the
   deeper one.**

2. **Heap-tuple positional-precision loss (the real blocker).** With
   `finaldist` initialized and `return None` replaced by a real tuple,
   the stall does **not** clear — it moves to the `heapq` fringe. The
   fringe holds `(float, Vertex)` tuples
   (`heapq.heappush(fringe[dir], (vwLength, w))`;
   `(dist, v) = heapq.heappop(...)`), and the analysis collapses the
   tuple slots into unions: `illegal ... 'l' illegal: ( float64 Vertex )`
   and `illegal primitive argument type 'x' illegal: ( list Vertex )`.
   That is the tuple-comparison / tuple-in-container precision problem of
   the **[057](057-sorted-tolist-fa-nonconvergence.md)** family
   (`check_split`'s recursive-edge EntrySet reuse over heterogeneous
   tuple elements), surfacing here through `heapq`'s generic
   list-of-tuples rather than `sorted()`/`list()`.

3. **Object `==` on `Vertex`** (line 39 `if source == target`; `Vertex`
   defines `__lt__` but no `__eq__`, and is also used as a `dict` key)
   is a genuine contributor, but neutralizing it (`==` → `is`) does
   **not** move the stall — the heap-tuple loss (2) dominates.

## Consequence for 063–066

The "container-element union fixed by CS-level element-type separation"
framing that 063/064/065/066 pinned on dijkstra2 is **not** what this
program stalls on. dijkstra2 belongs to the **057** (tuple/heap
precision) bucket plus a use-before-def, with a side of missing
`object.__eq__`. 066's CS-keying mechanism may still be right for *other*
members of the no-type bucket (a program that actually re-derives CS
splits), but it must be validated on such a program — dijkstra2 will not
exercise it. See 066's status note and the CS-re-derivation validation
search that accompanies its implementation.

## What fixing dijkstra2 actually needs

In dependency order: (a) the **057** tuple/heap precision fix (the real
blocker); (b) a default `object.__eq__`/`__hash__` so objects work as
`dict` keys and `==` operands without an explicit override; (c) the
vendored `finaldist` use-before-def is arguably an upstream bug, but a
tolerant "read-before-any-write local ⇒ union with its later-assigned
type" would let pyc compile it as CPython runs it. None of these are
066.
