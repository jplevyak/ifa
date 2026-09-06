# 136 — a creation point's identity should be (ES × call site)

**Status:** open, design stated by the author 2026-09-06. Supersedes the
call-site splitter measured in
[129](129-plan-demand-driven-creation-set-splitting.md), which is the same
handle used at the wrong stage.

## The statement

> The identity of a creation point should be ES × call site. The types can
> be used for assignment to a CS but not for identity. Call site
> difference should not result in incompatibility in a CS — it is just
> identity, not compatibility.

Three separate things, and pyc currently conflates the first two:

| | what decides it | today |
| --- | --- | --- |
| **assignment** — which CreationSet a value flows into | types | types ✓ |
| **identity** — which creation point this is | ES × call site | ES only |
| **compatibility** — may two creation points share a CS | demand | — |

## Why today's identity is too coarse

A creation point is an `AVar`, and an `AVar` is `(Var, contour)`. The
contour is an `EntrySet`, and `find_best_entry_sets` /
`entry_set_compatibility` route a call edge to whichever existing ES is
**type**-compatible. So call sites whose argument types agree collapse
onto one contour *before any CreationSet exists*.

`cs->defs` is a set of those AVars, so it can only ever distinguish what
the contours distinguish. Measured on `sudoku3`: `__pyc_getslice__` has
**1 contour with 7 in-edges**, so the container allocated inside it has
`defs=1` — nothing for CS-side partitioning to work with, no matter how
strong the demand. 361 of 428 single-def candidates there are in that
shape.

Under the proposed identity the same creation point has **7** identities,
one per call site, all still assigned to one CreationSet because nothing
has yet demanded otherwise.

## The ordering this fixes

The ES split becomes a **consequence** of a demanded CS partition, not its
trigger:

1. Creation points are identified per (ES × call site).
2. They are ASSIGNED to CreationSets by type — call-site difference alone
   never separates them.
3. Demand (an irrepresentable element, a type confluence) asks a
   CreationSet to split.
4. The partition is computed over creation-point identities.
5. Realizing a partition whose parts share an EntrySet requires splitting
   that EntrySet — which is exactly the third clause,
   *"ESs are split as necessary to separate the creation points so the CS
   can split"*, now arrived at from the right end.

## What this says about `PYC_CSCALLSITE` (ifa/129)

That experiment split the EntrySet FIRST, to manufacture distinguishable
defs, and let `CS_DEF_PARTITION` follow. The cascade worked — 7 ES splits
produced 14 further CS splits with no new CS-side machinery — but the
trigger was wrong, so call-site difference became an *incompatibility*.
Measured cost on `sudoku3`: passes 38 → 47, `ess` 652 → 797, container CS
54 → 64, `mixed` 18 → 7, and **no verdict changed**; `plcfrs` and
`sudoku5` got worse. It is the right handle at the wrong stage.

## What it costs to build

`cs->defs` currently holds `AVar *`. Per-call-site identity needs a richer
def record — an `(AVar, AEdge)` pair, or an equivalent — and every place
that reads `defs` as "the creation points" has to agree on the new
granularity: `creation_point`'s `cs->defs.set_add(v)`, `split_css`'s
re-point, `CS_DEF_PARTITION`, `P_prim_len`'s `!cs->defs.n`, and the
`IFA_DBG_*` probes. The re-point itself (`v->cs_map->put(sym, new_cs)`) is
keyed on the AVar, so a partition that separates two identities sharing
one AVar cannot be expressed until step 5 splits the ES — which is the
ordering above, and is why identity and realization must stay distinct.

**Not started.** The measurement that motivates it is in
[129](129-plan-demand-driven-creation-set-splitting.md)'s "call-site
splitting" section; the shape of the fix is this file.
