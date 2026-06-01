# IFA Documentation Index

Per-subsystem reference docs for the IFA library (`ifa/`). For the
project-wide index see [../CLAUDE.md](../CLAUDE.md); for the
top-level compilation flow that ties this library to the pyc Python
frontend see [../PIPELINE.md](../PIPELINE.md).

---

## Read first

- [ARCHITECTURE.md](ARCHITECTURE.md) — IFA library internal
  architecture: directory map, the `ifa_*` orchestration sequence,
  the `IFACallbacks` boundary, cross-cutting invariants. The "start
  here" doc for the library.

## Intermediate form

- [IR.md](IR.md) — IF1 intermediate form: `IF1`, `Sym`, `Code`,
  `PNode`, `Var`, `Fun`, `Immediate`, the type-hierarchy /
  `Type_kind` model, builtin symbols, lifecycle, gotchas.
- [CAST.md](CAST.md) — Numeric coercion & cast tables
  (`cast_code.cc`, `check_cast.cc`, `num.cc`, `fa.cc:coerce_num`):
  the 4×8 type matrix, `make_cast_code` generator,
  `coerce_immediate` / `fold_result` / `fold_constant`.

## Frontend

- [FRONTEND.md](FRONTEND.md) — V-language frontend
  (`ifa/frontend/*`): the `ifa` CLI's lowering pass, `ParseAST`,
  scope chain, prelude loading, and comparison with the pyc Python
  frontend (which is at the repo root, see
  [../PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md)).

## Analysis pipeline

- [IFA.md](IFA.md) — Iterative Flow Analysis (`analysis/fa.cc`):
  paper synthesis, data structures (`AType`, `CreationSet`,
  `EntrySet`, `AEdge`, `AVar`), the flow loop, the 5 splitting
  strategies, hooks, gotchas.
- [CLONE.md](CLONE.md) — Type-directed cloning
  (`analysis/clone.cc`): paper synthesis, partition refinement
  (`determine_clones`), equivalence functions, concrete-type
  synthesis, function cloning.
- [DISPATCH.md](DISPATCH.md) — Pattern matching and dispatch
  resolution (`if1/pattern.cc`): `MPosition`, `Match`/`PMatch`,
  `Matcher`, `MType` reverse index, frontend callbacks for
  generics/coercion/defaults, match cache.
- [PRIMITIVES.md](PRIMITIVES.md) — Compiler primitives
  (`if1/prim.cc`, `if1/prim_data.dat`): table primitives vs
  registered, `Prim`/`Primitives`/`RegisteredPrim`, transfer +
  codegen functions, the 56 `P_prim_*` constants, how to add new
  primitives.

## Lowering & optimisation

- [CFG_SSU.md](CFG_SSU.md) — Control flow, SSU form, dominators,
  and loops (`optimize/{cfg,ssu,dom,loop}.cc`): the four passes
  triggered by `Fun(Sym*)`, phi/phy placement, Tarjan + Cytron
  dominators, Ramalingam loop detection.
- [OPTIMIZE.md](OPTIMIZE.md) — Post-analysis transforms
  (`optimize/{dead,inline}.cc`): `mark_live_code/types/funs` DCE,
  frequency estimation, single-send + identity + closure-collapse
  inlining, pass-ordering rules.

## Code generation

- [CODEGEN_C.md](CODEGEN_C.md) — C backend (`codegen/cg.cc`):
  top-level emission, per-Fun + per-PNode walk, phi/phy
  materialisation, primitive switch, runtime `_CG_*` conventions,
  compile driver.
- [CODEGEN_LLVM.md](CODEGEN_LLVM.md) — LLVM backend
  (`codegen/llvm*.cc`): the `-b`/`USE_LLVM` path, module/context
  globals, type/value mapping, function declaration + body
  translation, `write_llvm_prim`, compile driver, known limitations.

## Utility layer

- [COMMON.md](COMMON.md) — Plib utility layer (`ifa/common/*`):
  `Vec` (dual-mode array/set), `Map`/`HashMap`/`ChainHash`/
  `BlockHash`/`Env`, `Accum`, lists/queues, `ArgumentState`,
  config, log, fail, timer, services, unit tests, GC-aware
  allocation, container-choice guide.

## Historical / stubs

- [LLVM.md](LLVM.md) — Stub redirecting to
  [CODEGEN_LLVM.md](CODEGEN_LLVM.md). The previous IR-serialization
  format described here is no longer in the tree.

## Testing

- [TESTING.md](TESTING.md) — Master plan for the IF1-level test
  infrastructure: parser for `.ir` files, per-phase normalized
  output formats, test runner. Sub-files under
  [testing/](testing/) cover format spec, runner, refactorings,
  and per-phase plans.

## Issues

- [issues/README.md](issues/README.md) — Open work items for the
  library. Numbered markdown files (`NNN-short-slug.md`); each
  documents symptom, root cause, proposed fix, and what fixing it
  unblocks.

## Operator / build docs

- [AGENTS.md](AGENTS.md) — Build / test / architecture overview
  for AI agents (`make`, test commands, env vars).
- [CLAUDE.md](CLAUDE.md) — `@AGENTS.md` redirect.
- [README.md](README.md) — Build instructions for the standalone
  IFA library.

---

## "Where do I start?"

| If you want to… | Read first |
|---|---|
| Understand the library at all | [ARCHITECTURE.md](ARCHITECTURE.md) |
| Change how types flow through analysis | [IFA.md](IFA.md) |
| Fix wrong dispatch / overload resolution | [DISPATCH.md](DISPATCH.md) |
| Fix a cloning / monomorphisation bug | [CLONE.md](CLONE.md) |
| Add a new compiler primitive | [PRIMITIVES.md](PRIMITIVES.md) |
| Fix a CFG / SSU / dominator / loop bug | [CFG_SSU.md](CFG_SSU.md) |
| Add or change an optimisation pass | [OPTIMIZE.md](OPTIMIZE.md) |
| Fix wrong C output | [CODEGEN_C.md](CODEGEN_C.md) |
| Work on the LLVM backend | [CODEGEN_LLVM.md](CODEGEN_LLVM.md) |
| Understand a `Sym` / `Code` / `PNode` / `Var` / `Fun` field | [IR.md](IR.md) |
| Understand numeric coercion or constant folding | [CAST.md](CAST.md) |
| Use the V-language frontend / `ifa` CLI | [FRONTEND.md](FRONTEND.md) |
| Understand a `Vec` / `Map` / `Accum` / `Que` API | [COMMON.md](COMMON.md) |

For the pyc-side frontend and runtime (which live at the repository
root, not under `ifa/`), see
[../PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md) and
[../RUNTIME.md](../RUNTIME.md).
