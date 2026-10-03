# 180 — LLVM backend: `**` emitted no code, so every power was 0

**Status:** CLOSED 2026-10-02. Filed the same day as "nbody diverges from
CPython", which misdescribed it.

## Symptom

`shedskin_examples/nbody` with `-b` printed `0.000000000` for every energy.
It was not drift. Every `**` in every program compiled with `-b` produced
0: `x = 2.5; print(x ** 2)` printed `0.0`, and `print(n ** 2)` printed `0`.
The C backend was right. It was never caught because no test used `**`,
and the corpus sweep runs only the C backend.

## Root cause

`P_prim_pow` had no LLVM emitter. It is not in `emit_send_binop`'s
`is_binop_family`. The generic `emit_send_default_prim` declined it,
because it tries to pass every rval as an argument, including the `"**"`
operator symbol, and `value_for_var` has no value for that. Then
`virtual_cg_emit_send` reached its final bare `return`, so the node
produced no code. Its result Var was never assigned, and a read of it came
back as a zero constant, so the program compiled and ran. That silent
fall-through is filed separately as
[181](181-LLVM-unhandled-prim-emits-nothing.md).

## Fix

- `emit_send_pow` (cg_emit_llvm.cc). For int ** int it calls
  `_CG_int_pow`. Otherwise it calls libm `pow` on doubles, marked
  `nobuiltin`: LLVM folds `pow(x, 2.0)` to `x*x`, which differs from
  glibc's pow, and so from CPython, on 8,253 of 10^7 inputs. The C backend
  has the same guard as `-fno-builtin-pow` in `Makefile.cg`.
- `_CG_int_pow` (pyc_c_runtime.h). CPython's int ** int is exact, and both
  backends used to compute it as a double, so `3**39` lost its low digits.
  It now uses repeated squaring in uint64. A negative exponent truncates
  as before, because pyc types int ** int as int (CPython returns a float
  there).

## Verification

`tests/pow_operator.py` matches CPython on both backends. LLVM nbody
matches CPython exactly (7.0 s; CPython takes 37 s).
