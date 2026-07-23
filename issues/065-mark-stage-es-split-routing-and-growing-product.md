# 065 — ES-split product routing: mark-stage exclusion, and self-product re-minting on a growing union

**Status:** open, investigated 2026-07-23 (pursuing the dijkstra2 /
"no type" oscillation, [063](063-no-type-bucket-triage.md)). Two routing
gaps identified; **both attempted fixes reverted as net-negative**.
**Affects:** `ifa/analysis/fa.cc` issue-033 product routing (~4448-4520),
`group_signature`, and the stall guard.

## Context

After the phantom-method-display fix ([064](064-method-phantom-display-blocks-es-split-routing.md))
unblocks the *type-stage* routing (242 → 37 violations on dijkstra2),
two residual routing problems keep it from converging.

## Gap 1 — mark-stage groups are excluded from routing

The routing enforcement is gated `!fsetters && !fmark` (fa.cc:4450): only
TYPE-stage split groups route; MARK-stage and SETTER-stage groups always
re-mint. On dijkstra2 the residual re-derivations are all MARK-stage
(`__len__`/`__getitem__`/`__eq__`/`__setitem__`). The gate exists because
`group_signature` is purely type-based (arg/return type unions), so two
mark-groups with the same types but different mark distances would share
a signature and route together — merging groups the splitter meant
separated (the comment at 4442-4447).

**Attempt (reverted):** allow mark-stage routing (drop `!fmark`). It DID
route more (ROUTE 5→14, DUP 32→17) but **regressed the corpus 51 → 48**
(lost chess, mastermind2, sat) — exactly the wrong-merge hazard. A safe
version needs a **mark-aware group signature** (fold the mark
distances/classes into the key), not the type-only `group_signature`.

## Gap 2 — self-product re-minting on a growing union

The remaining route-fails after Gap 1 are `d->product == es`: the ledger
recorded THIS es as the group's canonical product in an earlier pass, es
has since **widened** (more edges/types flowed in), so the group now
looks type-incompatible and `decide_entry_set_split` splits it off again
— but the product is es, so there is nothing to route to and it re-mints
every pass.

**Attempt (reverted):** on `d->product == es`, keep the group in es
(trust the stable ledger over the shifted type). It made things far
worse — **37 → 605 violations** — because keeping type-incompatible edges
in es makes es a giant polymorphic contour, spraying violations
downstream. The correct behavior would be to split the *complement*
(the newly-arrived edges that widened es) OFF es so es re-monomorphises
to its recorded group, but the split machinery currently detaches the
recorded group instead.

## The deeper reason it doesn't converge

`d->product == es` re-minting is a symptom: the underlying union is
**genuinely growing** on dijkstra2+`object.__eq__`. The container-element
union (shared `dict`/`list` methods over `Vertex→float` / `Vertex→list`
mixes, [043](closed/043-empty-container-inference-options.md) shape B)
keeps widening as more of the program resolves, so the split products
never stop widening. Routing stabilizes the *bookkeeping* but cannot stop
a growing union. The only thing that fully converged this repro was
making the container-internal element comparison a single tolerant
primitive (global `==`→`prim_is`), i.e. removing the per-arm dispatch
that feeds the union — the issue-043 shape-B lever, not a routing fix.

## Bottom line

Full convergence of the dijkstra2 / "no type" oscillation needs, in
order: (1) [064](064-method-phantom-display-blocks-es-split-routing.md)
(unblock type-stage routing — the big win, done-but-net-negative pending
the closure/display co-modification), (2) a **mark-aware** signature so
mark-stage groups route without wrong merges, and (3) the issue-043
shape-B fix (CS-level element-type separation of shared container
methods) so the union stops growing in the first place. Routing alone
(1+2) reduces the oscillation ~6.5x but cannot close it while (3) leaves
the union growing.
