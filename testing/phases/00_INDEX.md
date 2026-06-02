# Per-Phase Plan Index

Each file below covers one phase (or tightly-coupled group of
phases). Pick one and pick it up; they're independent once the
core test runner and refactorings exist.

For the overall plan see [../../TESTING.md](../../TESTING.md).

| # | File | Phases covered | IFA doc reference | Status | Owner |
|---|---|---|---|---|---|
| 01 | [01_if1_finalize.md](01_if1_finalize.md) | `if1_finalize` (DCE, prim binding, flattening, nesting fixup) | [IR.md](../../IR.md) §4.2 | TODO | — |
| 02 | [02_cfg_ssu.md](02_cfg_ssu.md) | `Fun::build_cfg`, `Fun::build_ssu` | [CFG_SSU.md](../../CFG_SSU.md) §2–4 | TODO | — |
| 03 | [03_dom_loops.md](03_dom_loops.md) | `build_cfg_dominators`, `find_all_loops` | [CFG_SSU.md](../../CFG_SSU.md) §3, §5 | TODO | — |
| 04 | [04_patterns.md](04_patterns.md) | `build_arg_positions`, `build_patterns`, `pattern_match` | [DISPATCH.md](../../DISPATCH.md) | TODO | — |
| 05 | [05_fa_analyze.md](05_fa_analyze.md) | `FA::analyze` flow loop + `extend_analysis` splitter | [IFA.md](../../IFA.md) | TODO | — |
| 06 | [06_clone.md](06_clone.md) | `clone(fa)` | [CLONE.md](../../CLONE.md) | TODO | — |
| 07 | [07_dce_optimize.md](07_dce_optimize.md) | `mark_live_*`, `simple_inlining`, `frequency_estimation` | [OPTIMIZE.md](../../OPTIMIZE.md) | TODO | — |
| 08 | [08_codegen.md](08_codegen.md) | `c_codegen_print_c`, `llvm_codegen_print_ir` | [CODEGEN_C.md](../../CODEGEN_C.md), [CODEGEN_LLVM.md](../../CODEGEN_LLVM.md) | TODO | — |
| 09 | [09_synthetic_coverage.md](09_synthetic_coverage.md) | Synthetic IR generator for splitter-stage coverage; V deletion plan | [IFA.md](../../IFA.md), [issues/003](../../issues/003-fa-converge-determinism.md) | Step 1 in progress | — |

## Standard structure of each phase doc

1. **Phase summary.** What runs, what state it depends on, what state it produces.
2. **Output format.** The normalized text format the runner diffs.
   Always defined here before any test is written.
3. **Per-printer signature.** Function name + location.
4. **Test cases.** Numbered list of fixtures with `.ir` sketch and
   expected coverage.
5. **Refactoring dependencies.** Pointers to [REFACTORING.md](../REFACTORING.md).
6. **Acceptance.** Pass criteria for considering the phase "covered".

When picking up a phase: claim it by adding your name to the "Owner"
column above, then start work in the linked file. The doc has
acceptance criteria you check off as you finish each test.

## Suggested order

Independent in principle, but a natural progression that minimizes
inter-track surprises:

1. **01 finalize** — small, surfaces text-format gaps early.
2. **02 cfg_ssu** + **03 dom_loops** — the run-block after `Fun(Sym*)`.
3. **04 patterns** — read-only; doesn't change IR state.
4. **05 fa_analyze** — biggest. Has the most refactoring deps.
5. **06 clone** — depends on FA state being right.
6. **07 dce_optimize** — depends on clone.
7. **08 codegen** — final consumer.

Phases 04, 05, 06 can proceed in parallel by separate people, as
long as the phase-output formats are agreed up front.

## Cross-cutting deliverables

Some work spans phases and isn't owned by a single phase doc:

- **Common printer utilities** in `ifa/testing/printers/util.{cc,h}`:
  name-assignment, sorted-Vec emission, common indent style.
  Land alongside the first phase doc that needs them (phase 01).
- **The `.ir` writer** ([../IF1_TEXT_FORMAT.md](../IF1_TEXT_FORMAT.md) §7)
  is needed by phases 01 and 02 (to emit post-finalize / post-CFG
  state as a checkpointable `.ir`). Land with phase 01.
