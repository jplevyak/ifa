# 063 — The "no type" corpus bucket: root cause + three codegen-robustness fixes + residual triage

**Status:** partially addressed 2026-07-22 (investigation requested as
a follow-up to the tuple-comparison work). Three surface codegen-
robustness gaps fixed here (commits below); the underlying FA root cause
is the empty-container / None-field inference family
([040](closed/040-empty-list-shared-clone-type-inference.md) /
[043](closed/043-empty-container-inference-options.md) /
[052](052-shared-method-branch-reopens-empty-list-fragility.md)) and
remains open. Net corpus effect: **39 → 51 pyc→C compiles, zero
regressions.**

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
