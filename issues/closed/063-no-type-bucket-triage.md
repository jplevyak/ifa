# 063 — The "no type" corpus bucket: root cause + three codegen-robustness fixes + residual triage

**Status: CLOSED — superseded (archived 2026-08-06).** The three
codegen-robustness fixes below landed directly. The FA-root
investigation is thorough but was left open-ended here; every
forward-looking action item it generated has since been forked into
a still-open issue with a narrower, non-overlapping scope: the
concrete build plan → [075](../075-FA-element-cs-method-split-idempotent-plan.md),
the dijkstra2 repro-attribution correction →
[067](067-dijkstra2-heap-tuple-precision-and-use-before-def.md)
(also closed, folded into
[068](../068-FA-derive-structural-ops-record-field-fold.md)), and the
oscillation-vs-genuine-no-type distinction →
[074](../074-FA-cross-pass-oscillation-plan.md). Kept here as the
historical diagnosis/derivation trail (including a since-corrected
"oscillation" framing in the pre-07-31 sections — see 074 for the
correction).

Original status line, superseded: partially addressed 2026-07-22 (investigation requested as
a follow-up to the tuple-comparison work). Three surface codegen-
robustness gaps fixed here (commits below). The bucket is **multi-
rooted** on the FA side — the "Update 2026-07-22 (pm)" section shows
one significant root (chull-family) is *missing default object
`__eq__`/`__ne__`*, distinct from the empty-container / None-field
inference family
([040](040-empty-list-shared-clone-type-inference.md) /
[043](043-empty-container-inference-options.md) /
[052](../052-FA-shared-method-branch-reopens-empty-list-fragility.md)); both
FA roots remain open and both reduce to the same lever —
**CreationSet-level (element-type) splitting of shared container
methods**, NOT anything receiver-specific (a mid-investigation draft
that blamed "per-receiver cloning" was wrong and is corrected in that
section). Net corpus effect of what landed: **39 → 51 pyc→C compiles,
zero regressions.**

## The bucket

24 shedskin examples emit a "no type" diagnostic — either
`warning: 'x' has no type` (a local Var FA never typed) or
`file.py:N: expression has no type` (an expression, with a "called
from" trace). Root cause, traced via chull (the smallest cascade, 4
no-types): fields initialised to an **empty list** and filled later,

```python
class Face:
    def __init__(self, edge=[None,None,None], vertex=[None,None,None], ...):
        self.edge = []          # <-- empty; element type must come from
        self.vertex = []        #     later appends / assignments
```

FA fails to back-propagate the later element type into the empty-list
creation, so `self.edge[i]` / `self.vertex[i]` read as no-type, and
every `==`/`!=`/arithmetic use of them cascades (amaze 224, rubik 322,
othello2 265 downstream no-type errors from a handful of roots). This
is exactly the [040]/[043]/[052] empty-container inference family — a
known, deep, partially-fixed FA problem, not a new one.

## What was fixed here (codegen robustness, not the FA root)

Once FA salvages the untypable values to void/any (the `runtime_errors`
default), the salvaged shapes reached three codegen sites that emitted
**raw, unsalvageable C** instead of degrading to a runtime-error assert
(the convention established in [056](056-CGEN-degraded-index-type-raw-c-compile-error.md)).
Each was fixed to degrade, per that convention:

1. **Unresolved tuple comparison → runtime assert** (not a compile
   abort). `P_prim_tuple_lt`/`_eq` called `codegen_fail()` on a
   union/non-tuple operand; now emits `assert(!"runtime error:
   unresolved tuple comparison")` when `fruntime_errors`. Recovered
   **mastermind2**.
2. **Jump to a not-live label → runtime trap** (not a dangling
   `goto`). A `Code_LABEL` emits `L%d:;` only when live; the
   branch-taken / `Code_GOTO` paths emitted `goto L%d` at a
   FA-salvaged (not-live) target, producing `use of undeclared label`
   — the single dominant terminal C error of this bucket. New
   `emit_goto_or_trap` (liveness condition identical to the label-
   emission guard, so a live label is never wrongly trapped) degrades
   the dead jump. Recovered **ac_encode, bh, doom, mwmatching, pisang,
   sha, sieve, softrender** (8).
3. **Constant record-index getter into a nameless destination →
   skipped**. The non-record sibling guards with `n->lvals[0]->live`;
   the constant `Type_RECORD` index getter didn't, emitting
   `(null) = (...)->e0` ("expression is not assignable"). Guarded on
   `cg_get_string(n->lvals[0])`. Recovered **amaze, othello,
   voronoi2** (3).

All three verified: `test_pyc.py` + `PYC_FLAGS=-b test_pyc.py` 227/227
both backends, isolated corpus sweep +12/−0.

## Residual blockers (still FAIL, deeper roots)

After the three fixes, the remaining "no type" examples fail on:

- **Operator dispatch fell back to a raw arithmetic primitive with a
  void/mismatched operand** — `invalid operands to binary expression`:
  `_CG_prim_mult(3, "*", "---")` (sudoku2 — `3 * "---"` string-repeat
  mis-lowered to numeric multiply), `_CG_prim_and`/`_CG_prim_xor` on a
  void operand (othello2, minpng). Not a surface guard: FA chose the
  wrong dispatch because an operand was untyped. Fixing needs the FA
  root, or a broad "trap when an arithmetic primitive gets a
  void/incompatible operand" codegen guard (higher risk — deferred).
- **`(null)*` list element type** (chull) — a list whose element type
  is None/unresolved; this is [061](../061-CGEN-multi-tuple-list-null-element-type.md)'s
  sibling for None elements.
- **member access on a void value** (rubik: `no member named 'e0'`),
  **invalid C++ cast** (yopyra), **generator returns void**
  (sudoku5), **string-escape `\x`** (rdb), **no matching function /
  pointer-int compare** (tonyjpegdecoder) — each idiosyncratic,
  downstream of the same no-type salvage.

## What a full fix would look like

The high-leverage fix is the FA one: make empty-container fields
(`self.x = []` then filled) resolve their element type from the later
appends/assignments — the [040]/[043]/[052] machinery — which would
dissolve the cascade at its source and likely clear most of this bucket
at once. The codegen-robustness fixes here are the right stopgap
(programs compile and trap safely instead of failing to build) but do
not make the affected programs *run* correctly: several now hit a
deeper `getter not resolved` runtime assert from the same unresolved-
field root (mastermind2, sha, mwmatching confirmed).

## Update 2026-07-22 (pm): the bucket has MULTIPLE roots — chull's is *object comparison*, not empty containers

Digging into the FA root fix corrected the direction above. chull's
no-type is NOT the empty-container family — it is **missing default
object `__eq__`/`__ne__`**. Minimal repros (no empty container, no
union needed):

```python
class V:
    def __init__(self, x): self.x = x
a = V(1); b = V(2)
print(a != b)     # -> "matching function not found" (compiles, traps)
print(a == a)     # same
```

Root: user classes derive from `object`, but neither `object`,
`__pyc_any_type__`, nor `__pyc_None_type__` defines `__eq__`/`__ne__`
(only `bool`/`str`/numerics/containers do). CPython defaults an
override-less object to **identity** comparison; pyc has no such
fallback, so `==`/`!=` on a plain instance dispatches to nothing —
`matching function not found` at runtime, `expression has no type` at
compile time. chull's `fv.vertex[i] != e.endpts[0]` (Vertex objects) is
exactly this; adding the default cleared chull's no-type (4 → 0).

### Why the obvious fix does not land as-is

Adding identity `__eq__`/`__ne__` to `object` (via the `is` primitive,
issue 028):

```python
def __eq__(self, x): return __pyc_primitive__(__pyc_symbol__("is"), self, x)
def __ne__(self, x): return not __pyc_primitive__(__pyc_symbol__("is"), self, x)
```

is **semantically correct and passes the full suite both backends
(227/227)**, and fixes the repros above — but in the corpus sweep it
**regresses dijkstra2 (COMPILED → FAIL) with zero offsetting gains**, so
it was reverted.

**Correction (traced 2026-07-22, later): the dijkstra2 regression is
NOT a "receiver" problem, and an earlier draft of this section that
blamed a shared `object.__eq__` clone unioning operands per call site
was wrong.** Splitting in this analysis is uniform over *all* argument
positions — `edge_type_compatible_with_entry_set` (fa.cc:887) iterates
`positional_arg_positions` and splits when any position's type differs;
the receiver is just position 0. Even the misleadingly-named
`split_for_per_cs_method_receivers` (fa.cc:5568) scans every positional
position. So two `__eq__` sites with different `x` types are *already*
separated; the argument is not unioned across call sites, and nothing
about the receiver is special. (The `PER_CS_RECEIVER` name is historical
— that stage splits on CreationSet *identity* for `clone_methods_per_cs`
classes, i.e. same *type* / different *CS*; those classes just happen to
sit in the `self` slot.)

What actually happens (verified via the violation call-traces): the
illegal union lands on `object.__eq__`'s `x`, but **every call site is a
container method comparing an element** — `dict.__getitem__`'s
`self._keys[i] == key`, `list.__eq__`'s `l[i] != self[i]`,
`list.__contains__`'s `x == item`. `list`/`dict` are *not* on the
`clone_methods_per_cs` track, so those methods are shared across every
container CreationSet of the same type regardless of element type, and
the element AVar inside is already the union of all element types in the
program (`(float64 Vertex)`, `(list Vertex)` in dijkstra2 — its dicts
mix `Vertex→float` and `Vertex→list`; its lists mix `list[Vertex]` and
`list[tuple]`). Adding `object.__eq__` did not *create* that union — it
made the previously-unresolved element comparison *resolve* to the
identity primitive, whose argument-type check (fa.cc:1860) then flags
the pre-existing union as illegal. The `(float64 Vertex)` shape is
additionally a representation-incompatible union — an unboxed scalar
unioned with a pointer, which a single identity/pointer comparison
genuinely cannot do (issue 060 territory).

So the two real gaps are (a) **CS-based (element-type) splitting of
shared container methods** — separating `list[Vertex].__eq__` from
`list[tuple].__eq__`, same *type* different *CS*, which type-based
splitting is structurally blind to (issue 043 "shape B"), and (b)
**representation-split unions at an identity site** (issue 060). Neither
is about the receiver, and neither is about `object.__eq__` — that
method was only the messenger that surfaced a pre-existing
container-element union. Minimal 2-container repros (even a single
polymorphic `x in lst` call site — `rc2.py`) *do* compile: the splitter
separates them at small scale, so dijkstra2's failure is a
scale/shape-specific splitter-precision limit (no stall fired — the
analysis converged; it simply had no violation forcing container
separation), consistent with the 040/043/052 family.

**No net corpus gain either.** Only chull's no-type is object-
comparison-rooted; the other high-count examples (amaze 224, doom 145,
rubik 217, othello2 265, …) are unchanged by the default — their
no-types have other roots. And chull, with its no-type cleared, still
fails on its `(null)*` None-list residual (issue 061's sibling), so even
it does not reach COMPILED.

### Takeaway

The "no type" bucket is genuinely multi-rooted; there is no single FA
fix that clears it. The three landed codegen-robustness fixes (above)
are the honest, safe, net-positive stopgap (39 → 51, zero regressions).
The remaining FA roots are:
- **object comparison** — needs a default identity `==`/`!=` for
  override-less classes (correct and suite-clean in isolation), but it
  only pays off once the *container* methods it flows through are split
  per element-CS (issue 043 shape B) and identity tolerates / avoids
  representation-split operands (issue 060). Not a receiver problem.
- **empty-container / optional-object element typing** (issue 043) —
  the same CS-per-element-type splitting, plus the union/absorption
  design there.
Both reduce to the same investment: **CreationSet-level (element-type)
splitting of shared container methods**, which the current
violation-driven splitter leaves merged because no violation forces the
separation. That is the real lever — not per-receiver anything.
Object-comparison repros (`rc.py`, `rc2.py` shapes) are kept in this
issue for whoever picks it up.

## Update 2026-07-23: tried lowering override-less `==` to identity three ways — the real blocker is the container-element union, not object comparison

Attempted the "lower override-less `==`/`!=` to `prim_is`" fix and
measured FA convergence on dijkstra2 (`-l s` splitting trace) at each
step:

1. **`object.__eq__`/`__ne__` as a Python method** (body `prim_is`, or a
   new always-bool `prim_object_eq` to rule out `prim_is`'s
   overlap-sensitive transfer): **STALLS** — pass 24–25, ~247–264
   violations, 8 re-deriving (issue 033 oscillation). Any shared
   `object.__eq__` *method* contour unions operands per-arm and churns
   the splitter. The always-bool primitive made no difference, so the
   transfer isn't the cause — the method dispatch contour is.
2. **Dispatch fallback (no method)**: contribute `bool` to `__eq__`/
   `__ne__` results directly in `add_send_edges_pnode` (no edge/contour)
   and skip arg-violation reporting for those sends. **STILL STALLS** —
   and the `illegal: (float64 Vertex)` / `(list Vertex)` unions persist
   unchanged. That's the tell: those unions are **not** from object
   comparison at all.
3. **Global `==`→`prim_is`** (frontend, all comparisons): **CONVERGES**
   (pass 28, every split stage 0, per-CS stage runs) — but breaks value
   equality (`sizeof_element in __add__` from dijkstra2's `(int,int)`-keyed
   dict losing `tuple.__eq__`).

Conclusion: the convergence blocker is the **container-element union**
(issue 043 "shape B") flowing through the element comparisons *inside*
shared `dict`/`list` methods (`self._keys[i] == key`, `self[i] == item`).
Global `prim_is` converges only because it makes **every** element
comparison a tolerant primitive; any object-only fix leaves the value-arm
element comparisons (`tuple.__eq__` etc. dispatched on union operands)
churning. Object comparison was merely the *trigger* that let those
container-element unions fully resolve and flow (without it, the Vertex
arms stay salvaged/cut, so the union never completes and dijkstra2
compiles).

So "override-less `==` → `prim_is`" is **necessary but not sufficient**:
necessary to make object comparison resolve without a method contour,
insufficient because it doesn't touch the shared-container-method element
union. The real lever remains **CreationSet-level (element-type) splitting
of shared container methods** (issue 043 shape B) — separate
`dict[Vertex→float]`/`dict[Vertex→list]`/`dict[(int,int)→…]` contours so
each `self._keys[i]`/`self._vals[i]` is monomorphic — or, equivalently,
make container-internal element comparison a tolerant primitive. All
experiments reverted; tree clean, suite 227/0.

## Update 2026-07-23: pursued the stable per-receiver-CS ES-split fix — root found, fix direction works (242→37) but couples to the closure machinery

Traced why the shared comparison/container-method ES splits oscillate and
found the precise blocker, then a fix direction that dramatically helps
but doesn't land cleanly yet.

**The CS split works.** The setter→creation-point creation-set split
(`split_css`) fires correctly and *stably* on dijkstra2 (3 dict + 4 list
CS splits, 0 DUP re-derivations). It is not the problem.

**The blocker is `group_display_ok` gating the issue-033 ES-split product
routing.** Instrumenting the routing decision (fa.cc:4453): of the failed
routes, ~68% (15/22) fail `group_display_ok`, on exactly the oscillating
methods (`__eq__`, `__lt__`, `__pyc_to_bool__`, `len`, `__getitem__`).
Those methods have `nesting_depth == 1`, so the display machinery treats
them as closures that capture their *dynamic caller's* frame; the split
group therefore spans many caller displays, `group_display_ok` returns
false, routing is skipped, and a fresh product is minted every pass →
non-convergence.

**Why methods have `nesting_depth 1`:** `python_ifa_build_syms.cc`
(`def_fun_pyda`) computed `nesting_depth = scope_stack.n - 1`, which
counts the enclosing CLASS-body scope. But a class body is not a runtime
closure frame a method captures via a display (methods reach class state
through `self`/globals; pyc synthesizes closure-carrier classes for real
closures anyway — the issue-001 note). So the class level is a phantom
display level, and it is what blocks the routing.

**The fix direction works.** Setting methods' `nesting_depth` to 0 drops
dijkstra2 from **242 violations (stall) to 37 (best 25)** and eliminates
all the DISPLAY route-fails — the shared methods' splits now route
stably. That is the mechanism the whole "no type"/dijkstra2 stall hinges
on, confirmed.

**Why it doesn't land as-is (2 remaining problems):**
1. **Closure-carrier coupling.** `nesting_depth` is shared with the
   issue-001 closure-carrier synthesis: the synthesized `class closure`
   carriers ARE real closures and need `nesting_depth > 0`, but a naive
   "methods → 0" (immediate enclosing scope is a class) catches them too,
   regressing `recursive_polymorphic` (a `class closure:: illegal call
   argument type` type error) plus `match_none`/`match_seq`/
   `exception_propagation` (compile-output diffs). A correct fix must
   distinguish a genuine user/builtin method from a synthesized
   closure-carrier method (carriers are created via
   `maybe_synthesize_closure_pyda`, in the build_if1 pass), or
   co-modify the display-building side so lexical depth and the runtime
   display array stay consistent.
2. **Residual setter/mark-stage oscillation.** Even with methods at 0,
   ~27 DUP re-derivations remain, now on the SETTER/MARK-stage splits of
   `__len__`/`len`/`__getitem__`/`__setitem__` — which are deliberately
   excluded from the issue-033 type-partition routing (fa.cc:4442-4447,
   "setter classes aren't characterized by a type partition"). Closing
   these needs a setter-class-keyed routing ledger, a second step.

Net: the "stable per-receiver-CS ES split" reduces to **(a) give real
methods `nesting_depth 0`** (unblocks the type-stage routing — the big
win, 242→37) **and (b) add setter-class-keyed product routing** for the
residual setter-stage splits. (a) is gated on cleanly separating methods
from closure-carriers; both are real but bounded follow-ups. All
experiments reverted; suite 227/0.

## Update 2026-07-31: why shedskin types the genuine no-type, and the corrected fix (from [074](../074-FA-cross-pass-oscillation-plan.md)'s measurements)

[074](../074-FA-cross-pass-oscillation-plan.md) measured the cross-pass
oscillation and **decoupled two things this issue had run together**: the
*oscillation* (hits the pass cap) and the *genuine no-type violations* (the
residual). They are NOT the same problem and do NOT share a fix:

- **The oscillation** (dijkstra2 hits the cap) is **display/dispatch
  caller-multiplication**, not a growing container union. 074 ruled out the
  container-fan-out lever for it (the same-type container-element union is
  ≤3% of the re-mint churn; the split-position union *shrinks*, not grows)
  and the "dispatch bounce" lever (216/217 rubik routes are distinct fresh
  edges — no bounce). The one mechanism that reduces that churn is the
  Stage-4 display-identity demotion (rubik 417→128; flag-gated,
  `PYC_STAGE4`).
- **The genuine no-type** (`illegal ... ( list Vertex )`) is *this* issue's
  043-shape-B container-element merge — and it is a **separate** lever.

### The concrete case (dijkstra2, re-confirmed 2026-07-31)

`illegal primitive argument type 'x' illegal: ( list Vertex )` reaching
`heapq`'s `<tuple_cmp>`. dijkstra2 holds parallel same-keyed containers
with different value types — `dists`/`seen` are `list[dict[Vertex→float]]`,
`paths` is `list[dict[Vertex→list[Vertex]]]`, `fringe` is
`list[list[(float,Vertex)]]`. `Vertex` (a dict key) and `list[Vertex]` (a
`paths` value) flow through the **same shared `dict`/`list` method
contours**, merging into the representation-incompatible union
`list ∪ Vertex`, which a single tuple comparison genuinely cannot do.

### Why pyc can't type it — the precise code-level gap

pyc's contour identity is **(type × display)**, not (type × data-context):

1. `split_css` DOES separate the CreationSets by element type (measured
   here: 3 dict + 4 list splits on dijkstra2, stable) — the *data* is
   separated.
2. **But nothing clones the shared `list`/`dict` *methods* per element-CS.**
   Confirmed at the source: the only per-CS-method track,
   `clone_methods_per_cs` → `split_for_per_cs_method_receivers`
   (`PER_CS_RECEIVER`), is set **only** on classes whose `__init__` uses
   `__pyc_clone_constants__` (`python_ifa_build_syms.cc:1940`: `range`,
   `__list_iter__`, `slice`) — **never on `list`/`dict`**. So the single
   `dict.__getitem__` contour reads one element AVar that is the union of
   *every* same-type container's element (`float ∪ list[Vertex]`),
   regardless of which CS called it.
3. Even that mechanism runs **only on quiescence** of stages 1-5 (065's
   circularity: quiescence needs the per-CS split, the per-CS split needs
   quiescence), which dijkstra2 never reaches.

So pyc separates the data but keeps the *code operating on the data*
shared, and the element types merge straight back.

### Why shedskin *can* — the reference mechanism (`shedskin/infer.py`)

Shedskin's constraint-node identity is **`(thing, dcpa, cpa)`** — `dcpa` is
the container-variant (data-polymorphism) index, part of *every* node's
identity, **including the nodes inside container methods**:

1. **`class_copy`/`func_copy` (infer.py ~1114-1173) clone the container's
   *methods* per `dcpa`.** `list.__getitem__` on variant `dcpa=2` reads its
   element from `(list.unit, 2, 0)` — physically separate from
   `(list.unit, 1, 0)`. `dists`'s dict-of-floats and `paths`'s
   dict-of-lists are different dcpas, so their `__getitem__` clones **never
   share an element type**. This is the piece pyc lacks.
2. **`ifa()` splits by element type as a deterministic, demand-driven
   step** on the *converged* graph each round (decide-then-restart, NOT
   violation-driven, NOT quiescence-gated within a pass): `ifa_flow_graph`
   groups a container var's writes by merged element type, `backflow_path`
   traces each back to allocation sites, `ifa_confluence_point` finds where
   sites mix types, `ifa_split_class` mints a fresh dcpa per partition.
3. **`gx.alloc_info` keys the split on `(func.ident, cartesian-ctx,
   alloc-AST-node)`** — the stable allocation node — so every round
   reproduces the same dcpa (new splits inherit from the "mother"
   contour). CPA_LIMIT/widening is only a backstop for *function*-template
   explosion, never the primary mechanism.

### The one-line difference

> Shedskin puts the element-type partition (`dcpa`) into contour identity
> and clones the container's **methods** per partition — always,
> deterministically, keyed on the allocation site. pyc splits the
> CreationSet but keeps the **method** contour shared (keyed on
> type×display), and its one per-CS-method mechanism doesn't cover
> `list`/`dict` and is gated behind a quiescence the union-churn prevents.

### The corrected fix (a re-scoped "Stage 2", targeting the no-type NOT the oscillation)

Clone `list`/`dict` (container) methods per **element-CS** — pyc's analog
of shedskin's `func_copy`-per-dcpa. Concretely, extend
`split_for_per_cs_method_receivers` (or a sibling stage) to fire the
existing demand-driven per-CS edge fan-out (`split_edges`) when a container
method ES's **receiver** is a union of **same-TYPE container CSs with
divergent element types**, and run it **every pass** (breaking 065's
quiescence circularity) rather than only on quiescence, keyed for
idempotence on the stable creation site (066). This is **not** the
oscillation lever (074 measured that away); it is the container-element
separation this issue has pointed at since the 040/043/052 family. Prototype
+ measurement tracked in the follow-up below / [074](../074-FA-cross-pass-oscillation-plan.md).

### Prototype 2026-07-31: the mechanism is VALIDATED (dijkstra2 FAIL→COMPILED), the naive application is NOT landable

Built the corrected Stage 2 behind `PYC_CSM` (a `split_container_methods_
per_element_cs` stage in `run_split_stages`, running every pass, one split
per pass, on the demand signal "receiver = union of same-TYPE container CSs
with divergent element types", firing the existing `split_edges` per-CS
fan-out) together with 074's `PYC_STAGE4` display demotion. **Reverted after
measuring — the tree is clean — but the result is decisive both ways.**

**It WORKS — the shedskin-derived design resolves the genuine no-type.**
With `PYC_CSM=2 PYC_STAGE4=1`, dijkstra2:
- `( list Vertex )` illegal-union violations: **gone (0)** — the
  container-element merge this whole issue is about is *resolved*.
- **`pass_limit_hit` 1 → 0** (converges — no longer caps), `final_pass`
  42 → 23, violations **170 → 44** (residual is a *different*, smaller
  root: 24 `has no type`), `ess` **946 → 111**.
- **FAIL → COMPILED** (emits `dijkstra2.py.c`). `pylife` also gained.

That is direct confirmation that **element-CS method cloning + demoting the
display from contour identity is the correct fix** — pyc's analog of
shedskin's `(thing, dcpa, cpa)` identity with `func_copy`-per-dcpa.

**The coupling is real: CSM REQUIRES Stage 4, at `split_edges` too.** CSM
alone (no Stage 4) **segfaults**: `split_edges`' per-CS `redispatch` is
gated by `edge_display_compatible`, so a display-spanning container receiver
creates filtered product ESs that no edge can redispatch into → an orphan
bare ES whose empty `display[]` a later `make_AVar` derefs out of bounds
(the pystone/tictactoe/amaze SIGSEGV family this very function's comment
warns about). The fix was to extend Stage 4's inert-slot demotion into
`edge_display_compatible` as well; with that, CSM+Stage 4 runs to completion.

**But the naive application is catastrophically fragile — do NOT land it as
built.** Full gate with both flags on: suite **235 → 217** (19 fails,
container-heavy: deepcopy/dict/list/genexpr/set), corpus **53 → 22**
(**+2 dijkstra2/pylife, −33** — ac_encode, adatron, bh, chull, life,
mastermind2, othello, pygmy, sat, sha, sieve, voronoi2 …, plus 1 segfault).
The cause is the **application**, not the mechanism: driving the *dynamic*
`split_edges` every pass, mid-oscillation, on unsettled contours churns and
breaks container-heavy programs — the exact issue-033 non-idempotence /
unflowed-contour hazard (M2b) that stage 1's *decide-then-apply* path and the
fysphun stage-2 revert exist to avoid.

**The refinement is exactly what shedskin does (and 066 specified):** run the
element-CS split as a **decide-then-apply** step against the *converged*
graph (not the dynamic per-pass `split_edges`), keyed on the **stable
allocation/creation site** (066's `alloc_info` analog) so it reproduces
deterministically and re-applies verbatim across passes instead of
re-deriving. The mechanism is proven; the durable-keying + decide-then-apply
application is the remaining build. (Flag-off is byte-identical to baseline;
the prototype code was reverted, this measurement is the artifact.)

**Correction (2026-07-31): the display does NOT need removing; drop Stage 4
from the necessary set.** [073](073-teach-splitter-productive-vs-inert-context.md)
proves `(type × display)` is bounded (a constant multiplier), so
display-out-of-identity is never a *correctness* requirement — it was only a
shortcut to stop `split_edges`' `redispatch` orphaning display-incompatible
products (the CSM-alone segfault). The correct fix is to fan the container
receiver out per **`(CS × display)`** (each display-distinct edge gets its
own product; no orphan), leaving the display in identity. The full,
concrete, step-by-step build — CSM fanned per `(CS × display)` +
decide-then-apply + stable-site keying, with code anchors and a
combination-sweep fitness function — is
**[075](../075-FA-element-cs-method-split-idempotent-plan.md)**.
