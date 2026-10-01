# 179 — LLVM backend: quameon compiles and runs but computes `nan`

**Status:** open. Filed 2026-09-30, found while fixing
[132](132-arity-is-representation-not-provenance.md).

## Symptom

`shedskin_examples/quameon` with `-b` compiles, runs with rc 0, and prints
`nan` for every energy, `-.5*potential energy 0.0094` where CPython prints
`1.6957`, and `acceptance ratio = 1.0` where CPython prints `0.574`. The
first three lines (`Atom`, `nuclear charges = [2]`,
`n-n potential energy = 0.0`) match. The C backend, on the same analysis,
matches CPython exactly.

## What is known

- It is NOT 132's dispatch ambiguity. The same `nan` appears with that
  conflict removed at the source (`charges = [float(atom[1][0])]`), with
  the current compiler.
- quameon never reached this point before 132's fix (both backends
  aborted at `matching function not found`), so it is a latent LLVM bug,
  not a regression.
- `random` matches CPython bit-for-bit on both backends
  (`random.seed(3)`: same `uniform`/`random`/`randint`).

## Next

Bisect within the program. Print intermediate values in a copy (the
first electron position after a move, the first `compute_en_value`), and
compare `-b` against the C backend to find the first divergence.
