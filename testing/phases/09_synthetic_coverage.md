# Phase 09 — Synthetic IR coverage for splitter stages

Cross-cutting plan for getting systematic coverage of IFA's deeper
splitter stages (mark-type, setter, setter-of-setter, mark-setter,
mark-setter-of-setter, violation) using an in-tree IR generator
rather than a specific frontend.

Reference: [IFA.md](../../IFA.md) §splitter,
[ifa/issues/003-fa-converge-determinism.md](../../issues/003-fa-converge-determinism.md).

---

## 1. Motivation

The `fa-converge` phase (issue 003) added pass-event instrumentation
and 5 fixtures, but the fixtures only exercise the `type` stage.
The empirical recon there showed:

- pyc's full test suite hits `type` (~30 tests) and `setter`
  (4 tests). No pyc test hits `mark-type`, `setter-of-setter`,
  `mark-setter`, `mark-setter-of-setter`, or `violation`.
- Hand-writing `.ir` for those stages turns out to be costly: the
  setter trigger in pyc programs goes through `split_css` (the
  third inner path), which needs container-relative AVars with
  `cs_map` populated — not the shape `13_setter_split.ir` produces
  via `@period`/`@setter` against a RECORD field.

So we have two structural problems:

1. **No source-language tests reach 5 of 7 splitter stages.** Either
   those stages are reachable only from V (legacy frontend, slated
   for removal), or they're effectively dead code, or pyc just
   doesn't emit the right shapes.
2. **The natural fix — "write a test that triggers stage N" — is
   blocked.** Source-language tests (pyc Python) don't have a way
   to express the needed shape; hand-written `.ir` requires
   reproducing internals of pyc's runtime lowering.

A library-level testing strategy that doesn't depend on any
frontend is the way out.

## 2. Architectural principle

**IFA is a library. Its tests must not depend on any specific
frontend.** Testing IFA through pyc is dependency inversion: it
makes the library's tests load-bearing on a separate consumer.

Two consequences for this plan:

- Synthetic IR coverage lives entirely under `ifa/`. No pyc, no V,
  no Python, no .py files.
- The generator constructs IF1 using the same builder APIs the
  frontends use (`if1_send`, `if1_closure`, `if1_make_symbol`,
  `if1_get_builtin`, etc.). Same call paths as production; bugs in
  those APIs surface in tests AND in frontends.

## 3. Design

### 3.1 `ifa/testing/ir_builder.{h,cc}` — shape generators

A small library of C++ functions that construct IF1 trees of
specified shapes by calling existing builder APIs:

```c
// ifa/testing/ir_builder.h
namespace IRShape {

// Container-with-element pattern. n_types distinct element types
// flow into the same container's element field via different
// allocations; n_callsites independent read sites. Stresses the
// type / mark-type stages.
void polymorphic_container(int n_types, int n_callsites);

// Cascading dispatch: depth nested polymorphic dispatches each
// using the result of the previous. Stresses cascade / multi-pass
// convergence.
void cascading_dispatch(int depth);

// Recursive specialization: a recursive function called with
// n_initial_types initial argument types, each branching to
// branch_factor recursive calls of varying type. Stresses
// violation stage and convergence under recursion.
void recursive_specialization(int n_initial_types, int branch_factor,
                              int recursion_depth);

// Setter-chain: field A is written from field B's value, B is
// written from field C's value, etc. Chain length k. Stresses
// setter-of-setter and its mark variant.
void setter_chain(int chain_length, int n_types);

// Dispatch-violation: a call site where some flow paths type-error
// but specializing the caller eliminates the violation. Stresses
// the violation stage.
void dispatch_violation(int n_callers, int n_targets);

}  // namespace IRShape
```

The functions populate the global `if1` (already constructed by
`ifa_init`) and return. The test harness then runs the same
pipeline as any other fixture: build_cfg, FA::analyze, mark_live,
printer, golden diff.

### 3.2 `ifa/tests/synthetic/` — fixture entries

Each synthetic fixture is a tiny C++ stub that names a shape and
its parameters:

```c
// ifa/tests/synthetic/setter_polymorphic_3types_5sites.cc
#include "testing/ir_builder.h"
void synth_build() { IRShape::polymorphic_container(3, 5); }
```

The runner finds these via filename convention (just like the `.ir`
fixtures), compiles them into a small loader, runs the requested
phase, diffs against `<name>.<phase>.expected`.

Alternative if the C++-per-fixture overhead is too high: a tiny
config-file format that the runner parses into shape-name +
parameters and dispatches to `IRShape::*`. E.g.

```
;; setter_polymorphic_3types_5sites.synth
shape: polymorphic_container
n_types: 3
n_callsites: 5
```

The config-file path is simpler to add fixtures with but requires
maintaining a parameter parser. Start with C++ stubs; promote to a
config format if the fixture count grows past ~20.

### 3.3 Integration with existing phases

The same printers used by `.ir` fixtures (`print_fa_converge_*`,
`print_fa_*`, `print_dce_*`, etc.) work unchanged — they take an
`IF1*` and don't care how it was constructed. Synthetic fixtures
plug into any existing phase.

A single `.synth` fixture can have golden output under multiple
phases:
- `setter_polymorphic_3types_5sites.fa-converge.expected`
- `setter_polymorphic_3types_5sites.dce.expected`
- etc.

This matches how `.ir` fixtures already work.

### 3.4 What this is NOT

- **Not a new source language.** No grammar, no parser, no AST.
- **Not a new IR format.** It uses the existing IF1 in-memory
  representation, populated by existing builder APIs.
- **Not a fuzzer.** Each shape is named and parameterized; the
  fuzzing case (random `.ir`, coverage-bucket-driven) is a possible
  follow-up but out of scope here.

## 4. Sequenced plan

### Phase A — Inventory ✅ DONE

Three companion documents:
- [09a_frontend_inventory.md](09a_frontend_inventory.md) —
  what's in `ifa/frontend/` (43k LOC, all V-only; deletion grouped
  for sequencing).
- [09b_ast_to_if1_patterns.md](09b_ast_to_if1_patterns.md) —
  the lowering patterns V's `ast_to_if1.cc` demonstrates,
  cataloged by AST shape → IF1 recipe. Reference material for the
  generator API design.
- [09c_splitter_triggers.md](09c_splitter_triggers.md) —
  for each of 7 splitter stages, the precondition + IF1 shape
  needed to trigger it. Coverage targets for the generator.

### Step 1 — Recon on V's test suite ✅ DONE

Ran V's three test programs (`for1.v`, `for2.v`, `literal.v`)
through the `FAPassEvent` sidecar.

| V test | passes | stages fired |
|--------|--------|--------------|
| `for1.v` | 0 | none |
| `for2.v` | 1 | type=1 |
| `literal.v` | 1 | type=1 |

**Outcome (b): V adds zero stage coverage beyond what pyc already
provides.** The `type` stage is already hit by ~30 pyc tests. V's
test suite doesn't reach `mark-type`, `setter`,
`setter-of-setter`, `mark-setter`, `mark-setter-of-setter`, or
`violation` — same as pyc's gap.

Conclusion: the 5 uncovered stages are **not** V-specific. They're
genuinely under-tested across both frontends. V can be deleted
with no IFA-test-coverage loss; synthetic shape generation
(Step 3) is the only path to covering the remaining stages.

### Step 2 — Decide V's fate

**Decision: delete V** (outcome (b) from Step 1).

V provides no IFA-test-coverage value. Its three test programs all
fall within pyc's `type`-stage coverage. The 5 unused stages are
not V-specific. Keeping V around as a "second frontend for
validation" is the only remaining argument and is weak: V isn't
maintained, its lowering has its own quirks (the
`nesting_depth=0` reset escape hatch was already removed via
issue 005), and a second frontend that produces a strict subset of
the shapes the primary frontend produces doesn't add architectural
robustness.

Concrete V-deletion work (separate commits for bisectability):
- `ifa/frontend/v.g`, `ifa/frontend/v.g.d_parser.{cc,d}`
- `ifa/frontend/ast_to_if1.cc` shrinks to whatever's pyc-shared.
- `ifa/frontend/scope.cc`, `ifa/frontend/make_ast.cc`,
  `ifa/frontend/parse.cc` reviewed for V-only paths.
- `ifa/tests/*.v` deleted.
- `make test_llvm` target dropped.
- Doc updates in `ARCHITECTURE.md`, `FRONTEND.md`, `LLVM.md`.

### Step 3 — Build the IR generator

`ifa/testing/ir_builder.{h,cc}` with 2-3 initial `IRShape::*`
functions covering the most-needed missing stages. Choose based on
Step 1's findings — don't speculatively generate all 5 stages
upfront.

Time estimate: 2-3 days for the initial generator + first 2-3
shapes + test-harness integration.

### Step 4 — Port V-unique patterns OR build coverage for unused stages

If Step 1 revealed V-only patterns, translate them to `IRShape::*`
calls. If Step 1 revealed unused stages with no current trigger,
the generator becomes the only path to coverage; build the shapes
needed to trigger them by reading `extend_analysis` and matching
its preconditions.

### Step 5 — Delete V

After Step 4 captures any unique coverage. V's grammar, parser,
AST→IF1 lowering, and `tests/*.v` all go. The
`frontend/ast_to_if1.cc` legacy shrinks to whatever's still shared.

### Step 6 — Document patterns

Update [IFA.md](../../IFA.md) and the splitter section with a
"how to trigger stage X" reference, pointing at the canonical
`IRShape::*` for each. Future contributors don't have to
re-derive the trigger preconditions.

## 5. Test cases (initial seed)

These mirror what the issue 003 plan would have asked for, but
generated programmatically:

| # | Shape | Stresses |
|---|---|---|
| 01 | `polymorphic_container(2, 2)` | type stage baseline (matches `02_splitter`) |
| 02 | `polymorphic_container(3, 5)` | type + likely mark-type |
| 03 | `polymorphic_container(2, 5)` with field-as-closure | setter stage |
| 04 | `cascading_dispatch(3)` | cascade (multi-pass type splits) |
| 05 | `recursive_specialization(3, 2, 3)` | violation under recursion |
| 06 | `setter_chain(3, 2)` | setter-of-setter |
| 07 | `dispatch_violation(2, 3)` | violation stage |

Goldens are generated by `--rebless` once each shape is implemented;
the goldens lock the actual stage event sequence.

## 6. Refactoring dependencies

None new. Uses:
- Existing IF1 builder APIs (`if1_*` in `ifa/if1/`).
- Existing `fa_events_*` sidecar (issue 003).
- Existing phase printers (no changes).
- Existing test harness's fixture discovery (needs a small
  extension to recognize `.synth` or `.cc` fixtures alongside
  `.ir`).

## 7. Acceptance

- [ ] V test recon completed (Step 1) and outcome documented.
- [ ] V either deleted (Step 5) or its unique coverage captured in
      synthetic shapes (Step 4).
- [ ] `ifa/testing/ir_builder.{h,cc}` lands with at least the
      shapes needed to trigger every splitter stage that has a real
      trigger.
- [ ] Per-stage canary fixture exists for each splitter stage that
      isn't dead code, with golden output locked.
- [ ] No remaining frontend dependency in IFA's test suite (`make
      test-ir` runs without pyc).

## 8. Open questions

- **C++-stub vs config-file fixtures.** Start with C++ stubs (one
  per fixture). If fixture count grows past ~20, promote to config
  files with a small parser in the runner. Either way, the
  generator API stays the same.
- **Whether to keep `.ir` fixtures at all.** Probably yes — they're
  good for "minimal reproduction of a specific bug" cases where a
  hand-crafted 10-line .ir is the clearest expression. Synthetic
  shapes are for combinatorial coverage; .ir is for minimal repros.
- **Generator unit-test the generator itself.** A bug in
  `IRShape::polymorphic_container(2, 2)` would produce wrong
  goldens. Mitigated by: (a) the shapes are small enough to
  visually inspect their first golden when introduced; (b) the
  goldens themselves are the regression marker — if generator
  changes break shape output, the golden diff makes it visible.
