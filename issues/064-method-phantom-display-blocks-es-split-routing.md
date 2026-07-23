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
