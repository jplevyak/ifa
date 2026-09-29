# 093 — LLVM: an int MOVE into a float64 slot stores the raw bits (no `sitofp`)

**Status:** open, root-caused, small fix. Re-verified 2026-09-28.
Rewritten; history in git:
`git show 3f36072b:ifa/issues/093-CGEN-int-float-union-move-not-coerced.md`.

## Repro

```python
import sys
def p(obj): print(obj)
if len(sys.argv) > 1 and sys.argv[1] == "a": x = 1
else: x = 2.0
p(x)
```

Run with argument `a`. The condition must be runtime-varying, or FA folds
the branch away.

| | output |
| --- | --- |
| CPython | `1` |
| C backend | `1.0` |
| LLVM backend (`-b`) | `5e-324` — **wrong value** |

## Two separate things

1. **LLVM value bug (this issue).** FA unifies `x` to `float64` storage.
   The C backend casts (`g2 = (_CG_float64)1;`). The LLVM backend emits
   `store i64 1, ptr @x` into a `double` global, and reading it back
   reinterprets the bits (the smallest denormal). **Fix:** at a scalar
   MOVE whose source `num_kind` is integer and whose destination is
   float, insert `CreateSIToFP`, the same coercion closed/062 added in
   `emit_send_binop`. Audit the C backend's MOVE shapes at the same
   time.
2. **`1.0` instead of `1` (not this issue).** That is numeric coercion
   widening a genuine temporal `{int, float}` union. The union is real:
   `x` really is either. CPython keeps the value's own type, so matching
   it means NOT collapsing the union to one storage type (a split, or a
   tagged scalar). That is [156](156-FA-split-int-from-float-coerce-last.md)'s
   business, and coercion is permissive-only (`--strict` refuses this
   program).

## Verification

LLVM prints `1.0` with `a` and `2.0` without, matching the C backend. Add
a `tests/` fixture with a runtime-varying condition and an `.env` for the
argument. Both backends' suites stay green.
