# 136 — a creation point's identity should be (ES × call site)

**Status:** open. Design stated by the author 2026-09-06, then **partly
retracted the same day** — see "Correction" below: the identity this file
asks for already exists, and the real question is about contour count, not
identity. Kept because the three-way distinction (assignment / identity /
compatibility) is the durable part and is now also in CLAUDE.md.

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

## Why today's identity looked too coarse (and is not)

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

## Correction — the identity is ALREADY (call site × ES)

An earlier revision of this file claimed `cs->defs` needs a richer record
(an `(AVar, AEdge)` pair) to carry per-call-site identity. **Wrong**, and
the author said so directly: *"an AVar cs->defs element is the result var
of a call and an es context (as are all AVars) so it has per call site and
per es identity."*

Verified in the source: every creation point is registered as
`AVar *result = make_AVar(p->lvals[0], es)` — the allocating node's **lval
Var**, in the current ES. Each call expression has its own result Var, so
an AVar already IS (call site × ES). Nothing needs building for identity.

## So the remaining question is sharper, and it is NOT identity

For `__pyc_getslice__` the allocating node inside it is one syntactic
site, and `__pyc_getslice__` has one contour — so `defs=1` is **correct**.
There genuinely is one creation point. Its seven callers are not seven
creation points; they are seven users of one.

That relocates the problem entirely:

- It is **not** that identity is too coarse — it is exactly as fine as the
  program is.
- It is that **one creation point serves seven callers**, and separating
  them is a question about how many CONTOURS `__pyc_getslice__` has, not
  about how creation points are identified.

And contour count is compatibility, which by the rule at the top of this
file is decided by demand — so the open question is: *what demand, and
what mechanism, gives `__pyc_getslice__` a second contour when its callers
agree on every formal's types?* CPA cannot (types agree). ifa/129's
`PYC_CSCALLSITE` can, and made things worse, because it did it as a
trigger rather than to realize a demanded partition.

**Nothing here is buildable until that question has an answer.** The three
mechanisms tried so far (`PYC_ESFORCS`, the type-side third clause,
`PYC_CSCALLSITE`) all attacked it from the splitter side and all came back
inert on the verdict.
