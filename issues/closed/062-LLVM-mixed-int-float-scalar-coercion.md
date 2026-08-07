# 062 — LLVM backend: mixed int/float scalar arithmetic isn't coerced to a common type (verifier failure)

**Status: CLOSED** — already fixed 2026-08-07, resolution summary
corrected 2026-08-07 after independent re-verification found two
inaccuracies in the first draft (below).

### Resolution Summary
Verified that mixed int/float arithmetic and comparisons on the LLVM backend are fully supported and pass verification. `emit_send_binop` in `ifa/codegen/cg_emit_llvm.cc` (lines 1090–1102) automatically inserts `Builder->CreateSIToFP` int-to-float conversions and `Builder->CreateSExt` integer width promotions before emitting binary operations and comparisons — landed in commit `c6b2d9bb` ("Fix codegen.", 2026-07-29), a full week before this issue was even filed (2026-07-22 filing, but the fix predates the *closure* by that much — the coercion was already present and working; `77915b19` ("issue 077", 2026-08-05), cited in an earlier draft of this summary, only added the `is_binop_family` guard and a `codegen_fail` fallback for the *non*-coercible case, later itself superseded by issue 080's `emit_salvage_trap`). Combined with issue 080's `emit_salvage_trap` for non-scalar type mismatches, mixed int/float expressions compile and execute correctly on `-b`.

Independently re-verified 2026-08-07 with a test constructed specifically to defeat constant-folding (values read from a file at runtime, matching this repo's own `async_driven.py` technique) — neither this issue's own filed repro nor `tests/mixed_int_float.py` actually exercises the runtime coercion path: both use only literal-constant arguments, so pyc's frontend folds the whole computation away at compile time and the generated code contains **zero** `sitofp` instructions either way, passing regardless of whether the fix works. The non-foldable version does exercise it (8 `sitofp` instructions in the generated `.ll`) and produces output byte-identical to the C backend and to CPython across `+ - * / < ==`. `tests/mixed_int_float.py` is not wrong, just weaker evidence than its own description implies — left as-is per the specific scope asked for here (correct the doc, file the segfault issue below; strengthening the test is a separate, not-yet-requested follow-up).

`shedskin_examples/tictactoe/tictactoe.py` does **not** currently compile and execute cleanly on `-b` (this summary's original claim) — directly tested 2026-08-07: it aborts almost immediately on **both** backends via a runtime salvage-guard trap (`assert(!"runtime error: list element type mismatch")` on C, SIGILL via `llvm.trap()` on LLVM), before any of the program's real output prints. Same underlying cause on both backends, so not an LLVM-specific regression and not int/float-related — a different, pre-existing salvage trap (likely already covered by [issues/035](../../../issues/035-list-element-cast-salvage-guard-and-set-item-union.md)'s family, still open) blocks tictactoe before this issue's own fix would ever be exercised by that particular program today. Not this issue's bug and not fixed here; flagged so the citation isn't taken as a clean, currently-reproducible verification.

Added `tests/mixed_int_float.py` and `tests/mixed_int_float.py.exec.check` to the test suite, confirming 100% execution parity across both backends (for the constant-folded shape it actually exercises — see above).

**Original filing follows.**

**Status:** open, found 2026-07-22 while verifying the tuple-comparison
primitive work on both backends (issue 025 / tictactoe). Filed rather
than fixed: it is an independent, pre-existing LLVM-backend numeric
coercion gap (nothing to do with tuples), and a correct fix touches the
shared scalar-arithmetic emission for `+ - * / <` etc. across the whole
backend, needing full regression verification. The C backend is
unaffected (C's usual arithmetic conversions coerce `int` vs `double`
automatically).
**Affects:** `ifa/codegen/cg_emit_llvm.cc` binary-arithmetic / compare
emission for numeric operands whose two sides have different `num_kind`
(int vs float) or different integer widths (bool `i8` vs int `i64`).

## Symptom

A mixed int/float expression fails the LLVM module verifier:

```
fail: LLVM module verification failed:
  Both operands to a binary operator are not of the same type!
  %7 = mul i64 15, double %6
  ...
  %17 = fdiv double %16, i64 %15
  Both operands to FCmp instruction are not of the same type!
  %21 = fcmp oeq double %19, i64 %20
```

The backend emits an `int`-typed operand and a `double`-typed operand
into one `mul`/`add`/`fdiv`/`fcmp` without a preceding `sitofp`.

## Root cause

Scalar numeric codegen assumes both operands already share a type. When
FA leaves one side `int` and the other `float` (e.g. an int literal
combined with a float local — `15 * sig(...)`, `1 / (1 + comp*x)`), the
LLVM emitter never inserts the `sitofp` (int→double) promotion that the
generated C would get for free from C's implicit conversions. Same story
for `bool` (`i8`) mixed with `int` (`i64`): different integer widths in
one `icmp`/binop.

Note the tuple-comparison primitive added in the same session does the
right thing for its *element* fields — see `llvm_num_unify` in
`cg_emit_llvm.cc`, which promotes to `double` (float present) or
sign-extends to `i64` before comparing. The bug here is the *general*
scalar path outside that primitive, which has no equivalent.

## Repro

LLVM backend only (compiles+runs fine on the default C backend):

```python
def sig(x, shift=0, comp=1):
    return 1 / (1 + comp * x - shift)
print(sig(2.0))
a = 15 * sig(0.5, 0.5, 10)
print(a > 0.0)
```

`./pyc -b -D . mix.py` → verifier failure as above. This is exactly why
`shedskin_examples/tictactoe/tictactoe.py` compiles and runs correctly
on the C backend but fails LLVM verification: its `sig()` and score
math are all mixed int/float. (The tuple sort in tictactoe is fine on
both backends; only the scalar arithmetic trips this.)

## What a fix would look like

At the shared numeric binary-op / compare emission site, when the two
operand `num_kind`s differ (or their LLVM widths differ), insert the
coercion before the op: `sitofp` the integer side to `double` if either
side is float; otherwise `sext`/`zext` both integers to a common width.
`llvm_num_unify` in the same file is a ready-made template for the
element-level version. Relates to issue 025's numeric-confluence
coercion (`fa_coerce_numeric_confluences`), but that operates at the FA
level for record fields; this gap is purely in LLVM scalar emission.

## What this unblocks

tictactoe on the LLVM backend, and any LLVM-compiled program doing mixed
int/float arithmetic — a common shape across the shedskin corpus.
