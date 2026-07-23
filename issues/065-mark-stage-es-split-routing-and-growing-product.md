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

## Update 2026-07-23 (branch): the existing per-CS lever is constant-oriented and cannot do element-type separation

With the user's note that the display machinery is *for nested
functions* (so a method carrying a per-caller display is a misuse, and
container-method per-recursion-level separation should come from
CS-level splitting, not the display), the plan is: **shape B first** —
give container methods proper per-element-CS separation, after which
methods can legitimately be `nesting_depth 0` and the display is
reserved for real closures.

Tested the obvious lever — route `list` CSs into `PER_CS_RECEIVER` by
setting `sym_list->clone_methods_per_cs = 1` (the same flag
`__list_iter__`/`range` use) together with methods `nd = 0`. **Badly
net-negative: suite 227/0 → 200/27** (widespread `matching function not
found` runtime crashes; `recursive_polymorphic`'s formal changed
`int64` → `list` but still didn't separate). Reason:
`clone_methods_per_cs` is the **constant**-cloning track (per-constant
instance CSs via `creation_point` skipping split-parent reuse); applied
to `list` it over-mints list CSs per creation site and shatters dispatch.
It separates by *constant*, not by *element type*.

So shape B needs a **new, demand-driven, element-type** mechanism, not a
reuse of the constant lever. The pieces are already half-present and
converge with issue 065:

- The **setter→creation-point CS split** already separates the container
  CSs by element type, stably (confirmed).
- The **setter-driven ES split** (`split_ess_setters`) already tries to
  split the container METHOD contour per setter class (= element type) —
  but it re-mints every pass because it is excluded from the issue-033
  product routing (065 gap 1), and the product then re-splits on the
  growing union (065 gap 2).

Therefore shape B ≈ **a stable, setter-class-keyed ES-split product
routing** for the container methods, driven by the element-type
(setter-class) confluence, running in the main split loop (not the
quiescence-only `PER_CS_RECEIVER`, since the oscillating case never
quiesces). That single mechanism would: (a) separate `list`/`dict`
methods per element-CS in the main loop, replacing the per-caller
display so methods can be `nd 0` (dissolving 064), and (b) stop the
container-element union from growing (dissolving dijkstra2's stall).
The blocker is exactly the missing **mark/setter-aware group signature**
(065 gap 1): the type-only `group_signature` can't key setter-class
groups, so the routing has nothing stable to route to. That signature is
the concrete next build.
