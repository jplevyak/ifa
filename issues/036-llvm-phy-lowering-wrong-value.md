# Issue 036: v2 LLVM backend returns wrong value when SSU places phys the C backend handles fine (expr_evaluator)

**Status:** open. **Found:** 2026-07-10, exposed by the issue 035
determinism fixes; now DETERMINISTIC (previously this class of
failure was heap-layout roulette).
**Affects:** the v2 LLVM path (`cg_normalize_v2` /
`cg_v2_emit_llvm_module`) — phy (reverse-phi / branch-split rename)
lowering.
**Related:** [035](035-nondeterministic-codegen-clone-order.md)
(whose `merge_live` fix completes the liveness approximation and
therefore places MORE phys — correct per the SSU algorithm),
`issues/028-fibheap-blockers.md` Bug B (a previous phy-placement
gap in the same machinery, fixed on the placement side).

## Symptom

```
./pyc -b -D . tests/expr_evaluator.py && ./expr_evaluator
```

prints `-3, 0, ...` — the second expression evaluates to 0 instead
of 6. The C backend (`./pyc` without `-b`) compiles the identical
IR correctly (exec matches `expr_evaluator.py.exec.check`), and
every other test passes on BOTH backends — this is the only LLVM
EXEC failure in the suite.

## Analysis so far

The 035 audit fixed `merge_live` in `ifa/optimize/ssu.cc`: it
accumulated its fixpoint-progress flag with `=` instead of `|=`, so
liveness converged early under some heap layouts and `place_phy`
placed FEWER phys than the algorithm specifies. With liveness
complete, expr_evaluator's recursive `evaluate` gets additional
phys at its branch splits. The C backend lowers these correctly;
the LLVM path loses a branch's renamed value (result 0 = a dropped
merge/rename, consistent with the 028 Bug B failure shape but on
the emission side rather than placement).

Deterministic now: 3/3 runs produce the identical (wrong) binary.

## Repro / verification plan

- `PYC_FLAGS=-b python3 test_pyc.py expr_eval` — currently the one
  failing test; fix = suite green on both backends.
- Diff the emitted IR for `evaluate` between backends (`--dparse`?
  use `IFA_LLVM` .ll output) at the phy merge points.

## What it unblocks

- Both-backends-green gate for issue 035's harness determinism
  check, and trust in the v2 LLVM path for control-flow-heavy
  recursive code.
