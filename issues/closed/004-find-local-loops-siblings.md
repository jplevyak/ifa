# Issue 004: `find_local_loops` reports nested loops as siblings

**Status:** closed (fix in `find_loop` + `collapse`, plus a new
`freq/03_nested_loops.ir` fixture showing inner-body peak = 100).
**Affects:** `ifa/optimize/loop.cc:find_loops`,
`ifa/optimize/loop.cc:find_local_loops`.
**Related:** `ifa/testing/phases/03_dom_loops.md` §7,
`ifa/tests/ir/loops/03_nested_loops.ir`, commit `34a7efb`.

## Resolution

Two coupled fixes in `loop.cc`:

1. **`find_loop`'s worklist walk** walks `zr->parent` chain up to
   the outermost-unparented REP before checking ancestry or adding
   to body. Without this, the union-find's `find()` returns
   whichever body PNode became the root of the equivalence class
   (lowest index), not the REP we built — so the outer collapse
   never saw the inner REP and never set its parent.

2. **`collapse`** now inherits entry preds: any pred of a body
   member that walks to a node outside body becomes a pred of the
   new REP. Without this, the outer-level walk short-circuits at
   the inner REP and misses outer-body PNodes reachable only
   through the inner header's external entry (e.g. `L_o_body` in
   the test fixture).

Verified with `loops/03_nested_loops.ir` (now shows nested tree
with l0 outer containing l1 inner at depth 2) and the new
`freq/03_nested_loops.ir` (inner-body peak frequency = 100 =
`LOOP_FREQUENCY^2`, exactly what the plan-doc test #12 wanted).

## Symptom

For a `.ir` fixture with a while-loop nested inside a while-loop:

```text
(fun %nested
  :body
    (label %L_o_top)
    (if %c1 %L_o_body %L_o_end)
    (label %L_o_body)
      (label %L_i_top)
      (if %c2 %L_i_body %L_i_end)
      (label %L_i_body)
        (move %x %r)
        (goto %L_i_top)        ;; inner back-edge
      (label %L_i_end)
      (goto %L_o_top)          ;; outer back-edge
    (label %L_o_end))
```

`find_local_loops` produces TWO loop reps with `parent == NULL` —
i.e. two top-level (sibling) loops, not one outer enclosing one
inner. The `loops/03_nested_loops.ir.loops.expected` golden:

```
(loops %nested
l0 depth=1 members=[%p4 %p5 %p6 %p7]
l1 depth=1 members=[%p0 %p1 %p2 %p3 %p8 %p9]
)
```

Both `l0` (the inner body PNodes) and `l1` (the outer-only PNodes)
report `depth=1`; neither's members include the other.

Textbook loop-tree behavior would have the outer rep contain the
inner rep as a child, with the outer's "members" being its
direct-body PNodes plus a pointer to the inner rep:

```
l_outer depth=1 members=[%p0 %p1 %p2 %p3 %p8 %p9]
  l_inner depth=2 members=[%p4 %p5 %p6 %p7]
```

This matters for users of the loop tree — e.g.,
`local_frequency_estimation` in `ifa/optimize/inline.cc`:

```c
static void local_loop_frequency_estimation(LoopNode *l, float f) {
  for (LoopNode *n : l->children)
    local_loop_frequency_estimation(n, f * LOOP_FREQUENCY);
}
```

If the inner loop isn't a child of the outer, it doesn't get
multiplied by `LOOP_FREQUENCY^2` — the inner body's
`execution_frequency` ends up at `10.0` instead of `100.0`. (The
plan-doc test #12 `nested_loops_100x` would fail today.)

## Root cause analysis

(Trace from commit `34a7efb` plus reading `loop.cc:find_loops`.)

`find_loops` processes `g->levels` from deepest to shallowest:

```c
for (int i = g->levels.n - 1; i >= 0; i--) {
  for (LoopNode *x : *g->levels[i]) {
    Vec<LoopNode *> worklist;
    for (LoopNode *y : x->pred) {
      if (y->dfs_ancestor(x) && y->dom_ancestor(x)) worklist.add(y);
      ...
    }
    if (worklist.n) find_loop(g, x, worklist);
  }
  ...
}
```

When the inner-loop header `L_i_top` is processed (deeper level
first), `find_loop` builds the inner body via union-find. The
inner body's PNodes get `parent = inner_rep`.

When the outer-loop header `L_o_top` is processed, its preds
include the back-edge GOTO at the end of the outer body — but
that GOTO is INSIDE the inner loop (it sits after `(label
%L_i_end)`). Via union-find, the back-edge's representative
resolves to `inner_rep`.

The expectation is that `inner_rep` is then pulled into the outer
loop's body and gets `parent = outer_rep`. But the current
algorithm's check at the outer iteration

```c
for (LoopNode *y : x->pred) {
  if (y->dfs_ancestor(x) && y->dom_ancestor(x)) worklist.add(y);
```

…uses `y` (the raw PNode-wrapper LoopNode), not
`g->find(y)` (the union-find rep). `y`'s `pre_dfs`/`pre_dom` are
the raw PNode's, which weren't updated when `inner_rep` was
collapsed.

For the outer body's back-edge GOTO (which is in the inner loop's
body), the raw GOTO is a DFS descendant of `L_o_top` and dominated
by `L_o_top` — both checks pass. So `y` (the GOTO wrapper) gets
added to the outer's worklist. Then `find_loop(g, L_o_top,
[GOTO])` runs.

In `find_loop`'s worklist iteration, the inner's
representative IS pulled in via `g->find` of the GOTO's preds. But
that mechanism only pulls in the inner's *body PNodes* (via union-
find resolution), not the `inner_rep` LoopNode itself. So the
outer collapse sets `parent = outer_rep` on the inner body PNodes,
but not on `inner_rep` — leaving `inner_rep->parent == NULL`.

Net effect: both reps are "top-level" in the children-tree sense.
The body PNodes are unified into the outer (correct for liveness
analysis), but the loop-tree structure that
`local_loop_frequency_estimation` recurses through is flat.

## Proposed fix

Three approaches, listed by intrusiveness:

### Option A — set `inner_rep->parent` explicitly when the outer absorbs it

In `find_loop` (the part that walks `worklist` and collects `body`),
after `g->find(z)` resolves a pred to an existing loop-rep, record
the inner rep so `collapse` can set its parent too. Roughly:

```c
static void find_loop(...) {
  Vec<LoopNode *> body, b, inner_reps;
  ...
  for (LoopNode *y : x->pred) {
    LoopNode *zr = g->find(y);
    if (zr != y && !zr->node) inner_reps.set_add(zr);
    ...
  }
  ...
  collapse(g, b, rep);
  for (LoopNode *ir : inner_reps) ir->parent = rep;
}
```

This is the smallest change but it's a one-level fix — if loops
are 3-deep, you'd still need to walk grandparents.

### Option B — generalize `collapse` to set parents on all nodes (including reps) in the union-find equivalence class

Touch `collapse` so the loop that sets `z->parent = header` walks
*all* the LoopNodes whose union-find rep equals each `z`. Untangles
the multi-level case for free, at the cost of an extra scan per
collapse.

### Option C — rewrite to use Tarjan's classical loop-tree algorithm

The current code is a Sreedhar-Gao-Lee variant. Tarjan-style
algorithms (dominator-based loop nest forest) directly build a
parent-child tree without union-find sleight of hand. Bigger
change but the result is well-understood.

### Recommendation

Try **Option A** first — it's surgical and the fixture in `loops/`
will diff visibly. If it works for 2-deep, extend to multi-level.
If Option A gets gnarly, escalate to Option B or C.

## Verification plan

1. Implement Option A.
2. Re-bless `loops/03_nested_loops.ir.loops.expected`:
   ```
   l0 depth=1 members=[%p0 %p1 %p2 %p3 %p8 %p9]
     l1 depth=2 members=[%p4 %p5 %p6 %p7]
   ```
3. Add a `freq/03_nested_loops.ir` fixture with the same shape;
   verify the inner body PNode peak is `100` (= `10 * 10`).
4. Run pyc e2e — production might depend on the current behavior,
   so check carefully. If pyc's loop-frequency estimates change,
   document the effect (probably an improvement: better loop-body
   weighting).

## What this unblocks

- Plan §5 test #12 `nested_loops_100x` (correct inner-body
  frequency).
- More accurate inliner heuristics for code inside nested loops.
- A semantically correct loop tree for any future analysis that
  walks `LoopNode::children`.

## Why deferred

- The wrong-but-stable behavior has been in pyc production for a
  while; nothing observably broken in pyc's test suite.
- The fix needs care: getting Option A wrong could break union-find
  invariants elsewhere in `loop.cc` / `clone.cc`.
- The golden is locked; any future investigation can use it as a
  regression marker (the diff between current and fixed output is
  the spec).
