# 179 — LLVM backend: quameon compiles and runs but computes `nan`

**Status: CLOSED 2026-10-12. Fixed by `c479bd6d`
([180](180-LLVM-pow-operator-emitted-nothing.md)): `**` had no LLVM emitter
and was silently dropped.** Filed 2026-09-30, found while fixing
[132](132-arity-is-representation-not-provenance.md).

## Closure (2026-10-12)

Bisected by building the two commits in a worktree and running
`pyc -b quameon.py` on each:

| compiler | `after warm-up, average energy` | `-.5*potential energy` | `acceptance ratio` |
| --- | --- | --- | --- |
| `c479bd6d^` | `nan nan` | `0.0094...` | `1.0` |
| `c479bd6d` | `-2.1328... 0.2673...` | `1.6957...` | `0.5740...` |

The second row is CPython's answer. quameon's orbital normalisation is all
`**`: `2**(3 / 4.0) * math.pi**(-3 / 4.0) * alpha**(3 / 4.0)` and
`r**-1` in `orbital/atomic_sto_primitive.py`. With no code emitted for
the operator, every wave-function value was garbage, the Metropolis step
accepted every move (`acceptance ratio = 1.0`), and the energies were
`nan`. At HEAD (`9e3fb17f`), `-b` output matches the C backend and CPython
on all 90 non-`TIME` lines. `tests/pow_operator.py` (from 180) guards the
emitter on both backends.

The original report is kept below.

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
