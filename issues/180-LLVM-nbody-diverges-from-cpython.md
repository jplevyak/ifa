# 180 — LLVM backend: nbody's trajectory diverges from CPython

**Status:** open. Filed 2026-10-02, found while making the C backend
default to `-O2`.

## Symptom

`shedskin_examples/nbody` with `-b` runs with rc 0 and prints energies that
agree with CPython to about 6 digits and then drift apart. The first
differing line is `-0.170243810` (CPython) against `-0.170243808`. The C
backend at `-O2` matches CPython exactly.

## What is known

- The C backend had the same symptom at `-O2` and the cause was found:
  clang rewrites `pow(x, 2.0)` as `x*x`. That product is correctly
  rounded, and glibc's `pow` is not; they disagree on 8,253 of 10^7
  uniform inputs in [0, 10). CPython computes `x**2` with libm's `pow`.
  `pow(x, 3)` and `pow(x, 0.5)` are not rewritten and match. nbody's
  dynamics amplify the last-bit difference. `Makefile.cg` now passes
  `-fno-builtin-pow`.
- The LLVM backend's cause is different, or at least not visible the same
  way. Its emitted `nbody.ll` contains NO call to `pow` and no `llvm.pow`
  intrinsic, so `**` on a float is lowered some other way, and the
  divergence comes from that lowering.

## Next

Find how the LLVM emitter lowers `P_prim_pow` for floats. It is not in
`emit_send_binop`'s `is_binop_family`. If it computes `x**n` by repeated
multiplication or by `exp`/`log`, it must call libm's `pow` instead,
marked `nobuiltin` so LLVM's own `pow(x, 2.0)` rewrite does not
reintroduce the C backend's bug.
