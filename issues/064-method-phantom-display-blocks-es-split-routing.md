# 064 — Methods carry a phantom `nesting_depth`, blocking issue-033 ES-split product routing

**Status:** open, root-caused 2026-07-23 (pursuing the dijkstra2 /
"no type" oscillation, [063](063-no-type-bucket-triage.md)). A direct
fix was prototyped and **reverted: it is net-negative** (see "Why the
obvious fix regresses"). Filed so the correct fix (co-modifying the
display machinery, or distinguishing real methods from closure carriers)
starts from measured facts.
**Affects:** `python_ifa_build_syms.cc` `def_fun_pyda`
(`nesting_depth = scope_stack.n - 1`); consumed by the splitter's
display machinery in `ifa/analysis/fa.cc` (`group_display_ok`,
`edge_display_compatible`, `ES_FN::equivalent` in `clone.cc`).

## Symptom / mechanism

The issue-033 ES-split product-routing enforcement (fa.cc:4448-4460 —
route a re-derived split group to its previously-minted product instead
of re-minting) is **blocked for shared methods**. Instrumenting the
routing decision on dijkstra2 (with the `object.__eq__` identity default
that triggers the stall): of the failed routes, **~68% fail
`group_display_ok`**, all on the oscillating shared methods
(`__eq__`, `__lt__`, `__pyc_to_bool__`, `len`, `__getitem__`).

Root cause: those methods have `nesting_depth == 1`. `def_fun_pyda`
computes `scope_stack.n - 1`, which counts the enclosing **class-body
scope** (the class is built as a function too, so the class scope carries
a `fun`). But a method does not capture its class body via a runtime
display — it reaches class state through `self`/globals, and pyc
synthesizes closure-carrier classes for real closures anyway (the
issue-001 note in `def_fun_pyda`). So the class level is a **phantom
display level**. With `nesting_depth > 0`, the display machinery treats
the method as a closure capturing its *dynamic caller's* frame; a split
group of the method therefore spans many caller displays,
`group_display_ok` returns false, routing is skipped, and a fresh
product is minted every pass → the splitter oscillates to the stall
limit.

Setting real methods' `nesting_depth` to 0 confirms the mechanism:
dijkstra2 drops from **242 violations (stall) to 37 (best 25)** and the
DISPLAY route-fails vanish.

## Why the obvious fix regresses (measured, reverted)

Zeroing a method's `nesting_depth` (immediate enclosing scope is a class)
is **net-negative**, so it was not landed:

- **Suite:** 227/0 → 223/4. `recursive_polymorphic` (compile failure —
  the issue-001 synthesized `class closure` carriers ARE real closures
  and need the display), `exception_propagation` (runtime `getter not
  resolved` crash), `match_none`/`match_seq` (spurious new
  `illegal call argument` warnings; they still run correctly).
- **Corpus sweep:** 51 → 47 compiled (lost chaos, chess, mastermind2,
  pisang, sat; gained sudoku3).

So `nesting_depth` is not a cosmetic per-method attribute — it changes
how every method clones/splits corpus-wide, and the display-building
side (`update_display` / `display[]` arrays / `ES_FN::equivalent`)
assumes the old lexical depth. Reducing lexical depth without
co-reducing the built display array desynchronizes them.

## What a correct fix needs

1. **Distinguish real user/builtin methods from synthesized
   closure-carrier methods** (issue 001, `maybe_synthesize_closure_pyda`
   in the build_if1 pass): carriers keep `nesting_depth > 0`, genuine
   methods get 0.
2. **Co-modify the display-building side** so the runtime display array
   and the lexical `nesting_depth` stay consistent when methods drop to
   depth 0 (otherwise `group_display_ok`/`ES_FN::equivalent` compare
   against a display level that no longer exists).

Both are real but larger than a one-line `nesting_depth` change. The
payoff is unblocking the ES-split routing for shared methods (the
242→37 win), which is a prerequisite for the dijkstra2 / "no type"
convergence — see [065](065-mark-stage-es-split-routing.md) for the
routing side and [063](063-no-type-bucket-triage.md) for the whole
chain.

## Correction 2026-07-23 (on branch es-split-convergence): the method display is NOT phantom — it does real work

Reworking this on the branch showed the "phantom display" framing above
is wrong, and zeroing method `nesting_depth` is the wrong direction.

`recursive_polymorphic` regresses because its top-level recursive
functions (`flatten_sum`, `g`/`h`) are **unchanged** by the fix (the
module scope has `in == null`, so they are not misclassified as
methods) — yet they still break. The cause: the *container methods they
call* (`len`, `__getitem__`) get `nd = 0`, and their `nd > 0`
caller-based display was providing **per-caller = per-recursion-level
clone separation**. Level-descending recursion (list-of-list → list →
int64) relies on each depth getting its own `len`/`__getitem__` contour;
collapsing them to one contour re-fuses the levels and the recursive
formal `x` unions all depths' types (`illegal call argument type 'x'
illegal: int64`). So the method display is genuine precision, and
`group_display_ok` is *correctly* refusing to route a group across
distinct caller displays (that would merge per-caller contours and lose
that precision).

**Consequence for the plan:** 064 as "give methods nesting_depth 0" is a
dead end — the routing block is a symptom of a genuinely-needed
per-caller display, not a bug. The oscillation's real root is the
**growing union** (issue [065](065-mark-stage-es-split-routing-and-growing-product.md)
gap 2 / issue 043 shape B): the shared container methods over a
heterogeneous-element union keep widening, so the per-caller contours
churn. The productive order is therefore **shape B first** — stop the
union from growing by keeping the container-internal element operations
from dispatching per-arm over the union (a tolerant comparison primitive,
the global-`==`→`prim_is` lever that DID converge, generalized to
identity-for-objects / value-for-value-types). With the union stable, the
per-caller method contours stop churning and the existing routing +
display machinery converge on their own — no `nesting_depth` surgery
needed. 064/065 are downstream of that and may dissolve once it lands.
