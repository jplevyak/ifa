# ifa/147 — the analysis result depends on the BINARY, not only on its inputs

**Status:** open. Rewritten 2026-09-28. It now also carries the
reproducibility half of [010](010-CLEANUP-vec-set-api-cleanup.md) (and,
through it, closed issues/021).

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
