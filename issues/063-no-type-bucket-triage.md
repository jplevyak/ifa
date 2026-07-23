# 063 — The "no type" corpus bucket: root cause + three codegen-robustness fixes + residual triage

**Status:** partially addressed 2026-07-22 (investigation requested as
a follow-up to the tuple-comparison work). Three surface codegen-
robustness gaps fixed here (commits below). The bucket is **multi-
rooted** on the FA side — the "Update 2026-07-22 (pm)" section shows
one significant root (chull-family) is *missing default object
`__eq__`/`__ne__`*, distinct from the empty-container / None-field
inference family
([040](closed/040-empty-list-shared-clone-type-inference.md) /
[043](closed/043-empty-container-inference-options.md) /
[052](052-shared-method-branch-reopens-empty-list-fragility.md)); both
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
(the convention established in [056](056-degraded-index-type-raw-c-compile-error.md)).
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
  is None/unresolved; this is [061](061-c-backend-multi-tuple-list-null-element-type.md)'s
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
