# IFA Testing Plan

A staged plan to build a test infrastructure for the IFA library that
operates on **IF1 intermediate code directly**, bypassing the V and
Python frontends. Each compiler phase gets a normalized output format
suitable for golden-file regression testing. Coverage is built up phase
by phase.

This file is the master plan. Sub-files cover orthogonal pieces so
multiple people can work in parallel.

| Track | File | Scope |
|---|---|---|
| Input format | [testing/IF1_TEXT_FORMAT.md](testing/IF1_TEXT_FORMAT.md) | Textual representation of IF1 that uses symbolic names, not raw IDs |
| Test runner | [testing/TEST_RUNNER.md](testing/TEST_RUNNER.md) | CLI harness, golden-file model, integration with `ifa --test` |
| Refactoring | [testing/REFACTORING.md](testing/REFACTORING.md) | Cleanups to the codebase required (or strongly enabled) by testability |
| Per-phase tests | [testing/phases/00_INDEX.md](testing/phases/00_INDEX.md) | Status board for the 8 per-phase plans |

---

## 1. Why IF1-level testing

The existing test suite (`tests/*.py` via `./test_pyc`) tests the
pyc Python frontend end-to-end. It's slow (CPython diff per test, plus
clang compile), broad (every test exercises every phase), and brittle
(a flow-analysis bug shows up as wrong stdout, not as wrong types
inferred at the right program point).

A complementary IF1-level suite gives us:

- **Focused coverage** — each test exercises one phase with the
  others held constant.
- **Fast iteration** — no CPython, no clang, milliseconds per case.
- **Determinism** — IF1-only tests have no system-Python / system-cc
  dependency.
- **Documentable invariants** — the per-phase output format *is* the
  spec of what the phase produces.

The format work also has a secondary win: each per-phase normalized
output is a candidate **inter-phase interface**. If a future
refactoring wants to checkpoint IF1 between phases (e.g., to revive
the dormant CDB cache, see [IFA.md](IFA.md) §11.5), these formats
become the on-disk schema.

---

## 2. Architecture

Three layers, each independently developable:

```
                                                  +---------------------+
                  +----------------+               | per-phase output  |
   .ir text file →| IF1 builder    |→ in-memory IF1 → | normalizer     |→ .out text
                  +----------------+               +---------------------+
                  testing/IF1_TEXT_FORMAT.md       testing/phases/XX_*.md
                                                     │
                                                     ▼
                                              +-----------------+
                                              | golden compare  |   testing/TEST_RUNNER.md
                                              +-----------------+
                                                     │
                                                     ▼
                                                   PASS/FAIL
```

### Layer A — `.ir` text format
A symbolic, line-oriented representation of IF1. Uses
human-chosen names (e.g. `%my_var`, `@int32`) instead of pointer-based
IDs. The parser produces real `Sym*` / `Code*` / etc. and registers
them with `IF1`.

### Layer B — phase output normalizer
For each pipeline phase (IF1 finalize, CFG, SSU, dominators, ...,
codegen), a printer that:
- emits a fully-deterministic text representation;
- uses the input names where available, else assigns stable
  `%t<n>` names;
- is invariant under irrelevant ordering differences (sorts where
  semantics allow).

### Layer C — test runner
Reads `.ir` files, drives the IFA phases up to a specified phase,
runs the matching normalizer, diffs against `.expected` golden file.
Reports PASS/FAIL. Reblessing supported.

---

## 3. Track-by-track roadmap

```
Track 1: Input format        Track 2: Test runner
  └─ design spec               └─ harness binary
  └─ parser impl               └─ Makefile target
  └─ writer impl               └─ rebless mode
  └─ round-trip test           └─ verbose mode
       │                            │
       └────────────┬──────────────┘
                    ▼
         Tracks 3a..3h: per-phase
            (in parallel)
                    ▼
              Track 4: coverage
                    ▼
              Track 5: refactoring (continuous)
```

Refactoring (track 5) is opportunistic — each phase track surfaces
specific testability blockers that get fixed when needed. Cross-cutting
refactors are listed in [testing/REFACTORING.md](testing/REFACTORING.md).

### Milestones

| M | Deliverable | Tracks involved |
|---|---|---|
| M0 | This plan reviewed | — |
| M1 | `.ir` parser + writer round-trip a sample IF1 | 1 |
| M2 | Test runner `ifa-test` builds; `--list-phases` works | 2 |
| M3 | Phase 1 (if1_finalize) tests pass + golden format frozen | 3a |
| M4 | Phase 2-3 (CFG, SSU, dom, loops) tests pass | 3b–3c |
| M5 | Phase 4-5 (patterns, FA) tests pass | 3d–3e |
| M6 | Phase 6-7 (clone, DCE, optimize) tests pass | 3f–3g |
| M7 | Phase 8 (codegen) tests pass | 3h |
| M8 | Coverage matrix complete; CI gating on IFA test pass | 4 |

Milestones can overlap; nothing past M2 strictly waits on its
predecessor (a test author can stub-out missing phases as long as the
phase-output format is defined).

---

## 4. Status board

Update this table as each item lands.

| Item | Owner | Status | Notes |
|---|---|---|---|
| `testing/IF1_TEXT_FORMAT.md` | — | landed | v1 spec; open questions resolved |
| `testing/TEST_RUNNER.md` | — | drafted | needs review |
| `testing/REFACTORING.md` | — | drafted | needs review |
| Parser implementation | — | DONE | `ifa/testing/parse_ir.{cc,h}` |
| Writer implementation | — | DONE | `ifa/testing/write_ir.{cc,h}` |
| Round-trip test | — | DONE | `ifa/testing/roundtrip_test.cc`; 3 cases pass via `ifa --test` |
| `ifa-test` binary | — | TODO | see TEST_RUNNER.md §2 |
| Phase 01: if1_finalize | — | TODO | [phases/01](testing/phases/01_if1_finalize.md) |
| Phase 02: CFG + SSU | — | TODO | [phases/02](testing/phases/02_cfg_ssu.md) |
| Phase 03: dominators + loops | — | TODO | [phases/03](testing/phases/03_dom_loops.md) |
| Phase 04: patterns | — | TODO | [phases/04](testing/phases/04_patterns.md) |
| Phase 05: FA::analyze | — | TODO | [phases/05](testing/phases/05_fa_analyze.md) |
| Phase 06: clone | — | TODO | [phases/06](testing/phases/06_clone.md) |
| Phase 07: DCE + inline | — | TODO | [phases/07](testing/phases/07_dce_optimize.md) |
| Phase 08: codegen | — | TODO | [phases/08](testing/phases/08_codegen.md) |
| Refactor: per-`FA` worklists | — | TODO | [REFACTORING.md](testing/REFACTORING.md) §3 |
| Refactor: deterministic IDs | — | TODO | [REFACTORING.md](testing/REFACTORING.md) §4 |
| Refactor: split `Fun(Sym*)` | — | TODO | [REFACTORING.md](testing/REFACTORING.md) §5 |

---

## 5. Working principles

Bake these into every test file and every output format. They make
diffs readable and changes attributable.

1. **No raw IDs in input or output.** Every reference is a symbolic
   name. The runner controls ID assignment internally; tests never
   see it.
2. **Deterministic order.** Any list (PNode order, AVar order,
   `has` member order) is sorted by name or by some other stable
   key before printing. The exception is intentionally-ordered
   structures (e.g., CFG predecessor index, dispatch MRO) — those
   are preserved.
3. **Per-phase output is layered.** Phase 2's output extends phase
   1's by adding the new fields it produced (`cfg_succ`, `cfg_pred`),
   not by re-printing everything from scratch. A `--full` flag
   shows the whole state if needed for debugging.
4. **Output files are diff-friendly.** Line-oriented; one fact per
   line; aligned where it helps; no trailing whitespace; LF
   line endings.
5. **Goldens live next to tests.** `foo.ir` →
   `foo.ir.finalize.expected`, `foo.ir.cfg.expected`, etc. One
   `.expected` per phase the test exercises.
6. **Tests are small.** A test that needs more than ~30 lines of
   `.ir` belongs as an end-to-end test (`./test_pyc`), not here. If
   a phase has a behavior that genuinely requires a large input,
   make it a *fixture* (a separate `.ir` file referenced by name).
7. **Failing tests print the diff and the path.** Re-running with
   `--rebless <pattern>` updates the golden after inspection.

---

## 6. Refactoring posture

Some testability work requires changing the library. The plan is
opportunistic:
- Refactors with broad benefit and no functional risk land first
  ([REFACTORING.md](testing/REFACTORING.md) §1–§2).
- Refactors that unlock a specific phase test land alongside that
  phase's first test cases (e.g., per-`FA` worklists land with
  phase 05).
- Refactors that change semantics (e.g., disabling DCE in finalize
  to test it separately) are gated behind opt-in flags so existing
  callers are unaffected.

No refactor should be merged that breaks the existing pyc end-to-end
suite (`make test`).

---

## 7. Out of scope (for this plan)

- pyc-side unit tests (TODO separately; see
  [`../tests/README.md`](../tests/README.md)).
- LLVM backend tests beyond text-IR equivalence
  ([phases/08_codegen.md](testing/phases/08_codegen.md) §LLVM has
  notes).
- Profiling / performance regression tests. Different infra
  (Criterion-style benchmarks) needed; see future
  `PERFORMANCE.md`.
- Fuzz testing. Could layer on top of the `.ir` parser once it
  exists; tracked as a follow-up in
  [TEST_RUNNER.md](testing/TEST_RUNNER.md) §future.

---

## 8. How to start contributing

1. Read this file, then `IF1_TEXT_FORMAT.md` and `TEST_RUNNER.md`.
2. Pick a phase from `phases/00_INDEX.md` that's `TODO` and
   unassigned. Mark it in-progress in the status board above.
3. Read the corresponding deep doc:
   - For format work: read [IR.md](IR.md).
   - For analysis phases: read [IFA.md](IFA.md).
   - For clone: [CLONE.md](CLONE.md).
   - Etc.
4. The per-phase plan files have concrete acceptance criteria and
   a starter list of test cases. Implement, write golden files,
   submit.
5. If you hit a testability blocker, note it in
   [REFACTORING.md](testing/REFACTORING.md) and either fix or
   work around (the per-phase file should explain its workaround
   if any).
