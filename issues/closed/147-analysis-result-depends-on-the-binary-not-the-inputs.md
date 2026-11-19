# ifa/147 — the analysis result depends on the BINARY, not only on its inputs

**Status:** CLOSED 2026-10-16. Both verification criteria hold on
`e05f62b5`, and the symptom no longer reproduces; see "Closed" at the end.
Rewritten 2026-09-28. It also carried the reproducibility half of
[010](../010-CLEANUP-vec-set-api-cleanup.md) (and, through it, closed
issues/021).

## Symptom

**Adding provably dead, `getenv`-gated diagnostic code to `fa.cc` changes
which corpus programs compile.** Measured 2026-09-10 at the default arm:
`b847e122` compiles `linalg` and `sudoku5`; the same tree plus probes only
(`static void dbg_*` helpers whose first statement is a `getenv` check,
calls to them, and one unused typedef) fails both. Corpus `compile_fail`
went 2 → 4.

Separately (010 / issues/021): `expr_evaluator.py` compiled 8 times gave 8
distinct `.ll` outputs, traced to unspecialized `set_add`/`set_in` over
`PNode *`, `Dom *`, `CallPoint *`, `MatchCacheEntry *` and
`llvm::Value *`, whose iteration order feeds Sym/Var/Fun id assignment
during cloning.

## Why it matters most

Every conclusion about FA is drawn from `corpus_sweep.sh`, at a resolution
of one or two programs. If a build perturbation moves two programs:

- a one- or two-program change between two BUILDS is not evidence about
  the change under test;
- a mechanism invented to explain such a movement explains noise. On
  2026-09-10 that produced four commits, all reverted (`9f321c89`,
  `39079fd9`, `e3c76a51`, `3f0cb228`).

**Standing rule until this is fixed:** only a same-binary, env-toggled A/B
is attributable. `-e` on `corpus_sweep.sh` exists for exactly this.

## Likely mechanism (not proven)

Pointer-keyed hashing (`combine_hash((uintptr_t)a, …)` over `AVar *`,
`AEdge *`, `CreationSet *`, `MPosition *`) makes bucket order follow
allocation addresses, which follow code size and layout. closed/035
recorded this exact family ("bucket order set the AVar id-assignment
order") and fixed one site by imposing a canonical order. The bisection
resists a simple story: some perturbations move results and some do not,
which is consistent with a few specific order-dependent decisions.

## What to do

1. **Canonicalise every order that reaches a decision.** Any iteration
   whose order can affect a split, a key, an id assignment, or a worklist
   must go through a stable-id order (`sorted_view`, or a Vec sorted by
   id) before use. Audit `form_Map` / `form_Vec` / range-for over
   pointer-keyed containers inside `split_*`, `creation_point`,
   `analyze_*`, the ledger, and clone's id assignment.
2. **An oracle that does not need the corpus:** build the same tree twice
   with different dead padding (or run under different ASLR/malloc
   perturbation, `MALLOC_PERTURB_`, `GC_` env settings) and diff the
   `-v` per-pass `ess`/`css`/violation lines on a handful of programs.
   Any difference names a decision to canonicalise.
3. **Record the floor.** Sweep N trees differing only in dead code and put
   the spread in `corpus_sweep.sh`'s documentation, next to the warnings
   about concurrent sweeps and stale binaries.

## Verification

Two builds differing only in dead code give byte-identical emitted C and
identical per-program sweep rows. `expr_evaluator.py` compiled 8 times
gives one `.ll`.

## Closed (2026-10-16): the result no longer depends on the binary

Measured on `e05f62b5` with the issue's own perturbation: a second build of
the same tree with 40 never-executing `getenv`-gated `static` helpers added
to `fa.cc`, and calls to them in a function FA calls per AVar
(18,619,096-byte binary vs 18,570,432).

- **Corpus, the issue's verification.** `./corpus_sweep.sh -m compile` from
  each binary: `compile__default__e05f62b5+83753197` (main) and
  `compile__default__e05f62b5+e5287435` (padded) are identical in all 77
  rows and every column -- `compile_rc`, warnings, `ess`, `css`,
  container CSs, shapes. In 2026-09-10's measurement this perturbation took
  `compile_fail` from 2 to 4.
- **Emitted code.** linalg, sudoku5 (the two programs that moved), quameon,
  chull, sat, plcfrs and richards give byte-identical C from the two
  binaries, about 200,000 lines, apart from the `#include` line naming each
  build's own directory.
- **One binary, repeated runs.** The same seven compiled 4 times each with
  ASLR on (`randomize_va_space=2`, so every run's heap addresses differ):
  one C file per program. `tests/expr_evaluator.py`, which gave 8 distinct
  `.ll` in 8 compiles, gives one `.ll` in 8.
- **A standing gate exists.** `test_pyc.py` compiles every test twice and
  byte-compares the generated code (closed/035's determinism gate), on both
  backends, as part of `make test`. One test opts out
  (`list_index_type_mismatch_salvage.py`).

**Not done:** "What to do" 1, the audit of every pointer-keyed iteration that
reaches a decision. These measurements cannot rule out an order-dependent
decision no measured program reaches; if two builds ever disagree again,
the oracle above (pad, sweep, diff) is the first thing to run, and its
first differing `-v` pass line names the decision.

**The working rule stays, as practice rather than as a known defect:** an
A/B that toggles an env var on one binary is still the cleanest attribution,
and `corpus_sweep.sh -e` exists for it.
