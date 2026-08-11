# 093 — Scalar MOVE into a union-narrowed-to-float64 slot isn't coerced (C: wrong repr; LLVM: wrong value)

**Status:** open, found 2026-08-11 diagnosing a hand-written repro
(gitignored scratch file at the repo root while investigating a
different bug, not checked in — see "Symptom" for the self-contained
version) right after fixing real-argv threading (`sys.argv` used to
be a hardcoded `["pyc"]` constant, which let FA fold this program's
`if` away entirely; with real argv both branches are reachable for
the first time, surfacing this).

**Affects:** `ifa/codegen/cg.cc` (C backend scalar MOVE emission) and
`ifa/codegen/cg_emit_llvm.cc` (LLVM backend scalar MOVE emission) —
wherever a `Code_MOVE`'s source value's concrete `num_kind` (int)
differs from the destination `Var`'s declared/unified type (float64)
after FA collapses a numeric union to one storage representation.

**Related:** [closed/062](closed/062-LLVM-mixed-int-float-scalar-coercion.md)
— same "mixed int/float needs a conversion" family, but scoped to
*binary operator* operand types (`emit_send_binop`, e.g. `15 *
sig(x)`). This issue is a plain MOVE/assignment into a variable whose
type FA already unified to `float64` — a different code path,
confirmed still broken against current HEAD (062's fix doesn't touch
it).

## Symptom

```python
if cond:
    x = 1      # int
else:
    x = 2.0    # float
p(x)           # def p(obj): print(obj)
```

CPython prints `1` or `2.0`. pyc:

| | cond=True (should print `1`) | cond=False (should print `2.0`) |
|---|---|---|
| C backend | `1.0` — wrong representation, right value | `2.0` ✓ |
| LLVM backend | `4.9406564584124654e-324` — wrong value | `2.0` ✓ |

Reproducer needs a genuinely runtime-varying `cond` — a compile-time
constant lets FA fold the branch away before this code path is ever
exercised (see "Verification plan"), e.g.:

```python
import sys
def p(obj):
    print(obj)
if len(sys.argv) > 1 and sys.argv[1] == "a":
    x = 1
else:
    x = 2.0
p(x)
```

Save as `t.py`, then `./pyc -D. t.py -o /tmp/tc && /tmp/tc a` (C) vs.
`./pyc -D. -b t.py -o /tmp/tl && /tmp/tl a` (LLVM).

## Root cause

FA infers `x : int | float` and unifies the union to a single
`float64` storage representation rather than keeping two clones —
`p`'s formal parameter is monomorphized to `_CG_float64` only
(confirmed via generated code: no int-typed path through `p` exists
at all). That's a legitimate FA/clone decision on its own; the bug is
that neither backend's MOVE emission coerces the *int-typed* source
value before storing it into that float64 slot, on the branch where
the live value is actually the int:

- **C backend**: does cast — generated code has `g2 =
  (_CG_float64)1;` — so the stored value is numerically correct
  (`1.0`). But by the time `p()` runs, `x` is unconditionally
  `_CG_float64`; all trace it started as a Python `int` is gone, and
  `float64::__str__` formats it with a decimal point. Wrong
  *representation*, not a value or memory-safety bug — and arguably a
  real, deeper limitation of collapsing the union at all rather than a
  simple codegen oversight (see "Open question" below).
- **LLVM backend**: does **not** cast at all. Generated IR:
  ```llvm
  @x = internal global double 0.000000e+00
  ...
  store i64 1, ptr @x, align 4        ; int branch -- raw bits, no sitofp
  ...
  store double 2.000000e+00, ptr @x, align 8   ; float branch, correct
  ...
  %g6 = load double, ptr @x, align 8
  ```
  The int branch's MOVE stores the raw 64-bit pattern of `1` directly
  into a `double`-typed slot, no `sitofp`. Reading it back as `double`
  reinterprets those bits: `1`'s bit pattern as IEEE-754 double is
  `4.9406564584124654e-324` (smallest denormal) — exactly the observed
  output. Straightforward missing coercion at this MOVE site, same
  category of fix 062 already made for binops
  (`Builder->CreateSIToFP`), just not applied here.

## Open question (flagging, not resolving here)

Even with the LLVM value bug fixed to match the C backend, is
"correct value, wrong repr" (`1.0` instead of `1`) an acceptable
target, or does actually matching CPython require *not* collapsing
`int | float` locals to one storage representation at all (a deeper
FA/clone change — keeping `p` polymorphic over the union instead of
monomorphized to `float64`)? At minimum the two backends should agree
with each other; matching CPython exactly may need the larger fix.
Whoever picks this up should decide which target this issue is
actually closing before starting.

## What needs to change

Minimally (makes both backends agree, closes the wrong-*value*
LLVM bug; does not resolve the "Open question" above):
- `cg_emit_llvm.cc`: wherever a scalar MOVE's source `num_kind` (int)
  differs from the destination `Var`'s declared `num_kind` (float),
  insert `CreateSIToFP` before the store — mirrors
  `emit_send_binop`'s existing coercion (062) and `llvm_num_unify`'s
  pattern for tuple-element comparisons, at the MOVE site instead of
  a binop site.
- `cg.cc` gets this specific repro's value right already (the cast is
  present); hasn't been swept for every MOVE shape, so worth a quick
  audit while touching the LLVM side rather than assuming it's
  universally correct.

## Verification plan

- The "Symptom" repro above on both backends, with and without a real
  invocation argument, matched against CPython's `1` / `2.0`.
- New regression test under `tests/` exercising a non-constant-
  foldable `int | float` union reaching a plain (non-arithmetic)
  consumer — needs a genuinely runtime-varying condition, since a
  compile-time-constant one lets FA fold the branch away before this
  code path is ever exercised (the same trap closed-062's own first
  verification draft fell into).
- Full `test_pyc.py`, both backends.
- Compile-only sweep of `shedskin_examples/` to catch any new C
  compile errors from wherever the fix lands; the value-correctness
  class doesn't fail loudly, so the full test suite is the real check.

## What this unblocks

Correct behavior for any program where a local/parameter is genuinely
`int | float` (not just combined inside one arithmetic expression,
which 062 already covers) and reaches a consumer that doesn't do
arithmetic on it — printing, storing, passing to another function.
Currently silently wrong on both backends, worse (wrong value, not
just wrong format) on LLVM.
