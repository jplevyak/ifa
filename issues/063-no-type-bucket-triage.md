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
FA roots remain open and both are blocked on the same per-receiver
method-cloning precision (issues 033/045). Net corpus effect of what
landed: **39 → 51 pyc→C compiles, zero regressions.**

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
it was reverted. Two independent reasons it does not pay off yet:

1. **FA shared-clone operand union (the 033/040/052 precision family).**
   `object.__eq__(self, x)` is one method body inherited by every class;
   `x` is unconstrained, so a single shared clone unions *every* `==`
   operand in the program. dijkstra2 keys a dict by `Vertex` and stores
   `float` distances; the shared `__eq__` clone's `x` becomes
   `(float64 Vertex)`, which then flows illegally
   (`illegal call argument type '( float64 Vertex )'`). The fix needs
   the default comparison methods **cloned per receiver CS** (the
   issue 045 `clone_methods_per_cs` / `PER_CS_RECEIVER` machinery) so
   each receiver's `__eq__` sees only its own operands — the same
   splitting-precision wall this whole family keeps hitting. Marking
   `object` itself `clone_methods_per_cs` is far too broad (every user
   class) — a targeted lever, or a frontend lowering of override-less
   `==`/`!=` straight to `prim_is`, is the real shape.
2. **No net corpus gain.** Only chull's no-type is object-comparison-
   rooted; the other high-count examples (amaze 224, doom 145,
   rubik 217, othello2 265, …) are unchanged by the default — their
   no-types have other roots (empty-container unions, dead union
   cross-products, etc.). And chull, with its no-type cleared, still
   fails on its `(null)*` None-list residual (issue 061's sibling), so
   even it does not reach COMPILED.

### Takeaway

The "no type" bucket is genuinely multi-rooted; there is no single FA
fix that clears it. The three landed codegen-robustness fixes (above)
are the honest, safe, net-positive stopgap (39 → 51, zero regressions).
The two real FA root fixes each need the per-receiver method-cloning
precision that issues 033/040/045/052 circle:
- **object comparison** (this update) — needs override-less `==`/`!=`
  to resolve to identity *per receiver* without unioning operands.
- **empty-container / optional-object element typing** (issue 043) —
  needs the union/absorption or per-contour CS split described there.
Both are blocked on the same splitter-precision investment, not on any
local patch. `tests/`-worthy repros for the object-comparison root are
kept in this issue for whoever picks up the per-receiver lever.
