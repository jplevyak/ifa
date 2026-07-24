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

## Deeper dig (2026-07-23): the isolated codegen root + the four layers

Minimal repros (`scratchpad/h*.py`, `d1.py`) pin the deepest blocker to a
**codegen gap**, cleanly separated from dijkstra2's other tangles.

**The isolated root — h1 (6 lines):** a `heapq` heap of `(float, V)`
tuples where `V` is a user class with `__lt__`+`__eq__`. It **compiles
clean but aborts at runtime**: `assert(!"runtime error: unresolved tuple
comparison")` in `_siftdown`'s `newitem < parent`.

- `tuple.__lt__` (`__pyc__/04_sequence.py:319`) lowers to the
  `tuple_lt` **primitive**, whose C codegen
  (`ifa/codegen/cg.cc` `emit_tuple_lt_expr` → `emit_elem_lt`) inlines the
  element comparisons as a C ternary chain.
- `emit_elem_lt` handles **only** numeric, string, and nested-tuple
  elements; a **user-class element returns `false`** → the
  salvage-assert (issue 056) fires. `emit_elem_eq` is the same for `==`.
- Control: **h3** — the identical program with `(float, int)` tuples —
  compiles *and runs correctly* (matches CPython). So the gap is
  precisely "a tuple element whose type is a user class."
- The dispatch machinery is not missing: **d1** shows a *direct*
  `a < b` (V.`__lt__`) compiles fine; only `(0.0,a) < (0.0,b)` asserts.
  The problem is that the whole tuple compare is ONE primitive send, so
  the FA never instantiated an element-`__lt__` send/clone for codegen to
  call — `emit_elem_lt` has a type Sym but no resolved contour-specific
  clone. This is the **codegen half of [057](057-sorted-tolist-fa-nonconvergence.md)**
  (the earlier 057 work was on the FA-convergence side, `check_split`).

**The four layers of dijkstra2 (in the order pyc hits them):**

1. **Compile-level "no type" — missing `object.__eq__`.** `Vertex` (has
   `__lt__`, no `__eq__`) is a `dict` key in `dists`/`seen`/`paths`, so
   the dict body's `if self._keys[i] == key:` (`__pyc__:1407/1414/1452`)
   and tuple `!=`'s `if l[i] != self[i]:` (`__pyc__:751`) are unresolved.
   This is what the *stock* repro stalls on (`wt` no type at line 108).
2. **`finaldist` use-before-def** masks the rest (commented-out
   `#finaldist = 1e30000`).
3. **Tuple/user-class-element comparison** — the h1 codegen gap above.
   Reached once 1+2 are worked around; a runtime abort, not a warning.
4. **FA tuple-slot precision loss** — even with `__eq__`+`__hash__`
   added, ~92 compile errors persist (`( list Vertex )`,
   `( float64 Vertex )` rejected by primitives). The program mixes
   **five** 2-tuple shapes — `(int,int)` coord keys, `(Vertex,int)`
   neighs, `(float,Vertex)` heap, `(float,list)` returns — and a single
   `dict` keyed by *both* `Vertex` and `(int,int)`. pyc's **shared**
   generic tuple/dict comparison methods union those heterogeneous
   element/key types, producing the scrambled `( list Vertex )` tuples.
   This layer *is* the container-element-union family
   ([065](065-mark-stage-es-split-routing-and-growing-product.md) /
   043 shape B) — the shared method needs per-element-CS specialization —
   so 066's mechanism is relevant here after all, just not via
   `split_css` (which never fires) but via the ES-side method fan-out.

**Fix assessment for layer 3 (the clean, independent bug):** the correct
fix generates a real per-tuple-shape comparison that dispatches each
element through normal `__lt__`/`__eq__` sends (so user-class elements
resolve like any method call), replacing the inline-primitive expansion —
or, as a narrower shortcut, `emit_elem_lt`/`emit_elem_eq` could call a
user-class element's method **when that method is monomorphic** (single
clone). Both are real codegen work, but layer 3 is independently
fixable and independently useful (any `heapq`/`sorted` over
`(key, object)` tuples hits it), and does not require solving layer 4.

## Layer 3 — implementation (2026-07-23): part B landed, part A attempted and backed out

Layer 3 splits cleanly into two halves:

- **Part B — codegen (LANDED, commit `9ef52f82`).** `emit_elem_lt`/
  `emit_elem_eq` (`cg.cc`) now compare a user-class tuple element by
  calling its resolved `__lt__`/`__eq__` clone (`cg_find_elem_compare_fun`
  matches self/other arg types; note the method arg layout is
  `[selector, self, other]`, so self is `positional_arg_positions[1]`).
  Works whenever the clone already exists (verified: h4, a heap of
  `(float,V)` with `V` compared elsewhere, compiles clean and matches
  CPython). Zero regressions, suite 227/0. This is the whole fix for any
  program that also compares the element outside the tuple.

- **Part A — FA clone instantiation (ATTEMPTED, REVERTED).** For the pure
  case (h1, the tuple comparison is the element's ONLY comparison), no
  clone exists, so part B has nothing to call. The FA `tuple_lt`/`tuple_eq`
  transfer function (`fa.cc`) must instantiate it. The scaffolding works
  and is safe in principle: iterate each **concrete** operand tuple CS and
  each **constant** slot (`cs->vars[i]`) — precise, no variable-index
  union collapse — and dispatch the element method; its bool result flows
  harmlessly into the already-bool tuple result, and codegen routes a prim
  PNode through `write_c_prim` (ignoring `f->calls`), so no double call is
  emitted. **The blocker:** the element method must be *dispatched by
  selector symbol* — instance `var_map` holds only data fields, not
  methods (confirmed: `ecs->var_map.get("__lt__")` is null even though the
  class defines it; real method calls resolve via the `P_prim_period`
  selector path `all_applications(p, es, selector, …)`, fa.cc:2156, not
  var_map). Synthesizing a `__lt__` **selector AVar** in the generic ifa
  layer (which has no pyc-symbol knowledge and no reusable selector Var at
  a `tuple_lt` site), plus replicating the full two-arg method call with
  per-pass idempotency and without perturbing the splitter, is deep FA
  surgery — not a safe bounded change, so it was reverted (suite gate).
  **Concrete next step for a dedicated effort:** obtain the element
  method's dispatch target without a selector Var — either add a generic
  `(class-CS, method-name) → method-value AVar` accessor (the class
  prototype, not the instance `var_map`), or intern a selector symbol +
  Var once and reuse it — then call `all_applications` per concrete CS /
  constant slot as the reverted scaffold already did. A frontend
  alternative (inject element comparisons at tuple literals) is rejected:
  it would **execute** user `__lt__`/`__eq__` at tuple creation, changing
  behavior for any side-effecting comparison and adding per-tuple cost.
