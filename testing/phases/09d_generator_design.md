# Phase 09 — Phase B: IR generator API design

The detailed design for the synthetic IR generator that closes the
splitter-stage coverage gap without depending on any specific
frontend.

Synthesizes input from:
- [09a_frontend_inventory.md](09a_frontend_inventory.md) — what
  exists in `ifa/frontend/`; what's deletable when.
- [09b_ast_to_if1_patterns.md](09b_ast_to_if1_patterns.md) — the
  IF1-construction patterns V's lowering demonstrates.
- [09c_splitter_triggers.md](09c_splitter_triggers.md) — per-stage
  trigger preconditions; coverage targets.

---

## 1. Design principles

Three commitments before any code:

1. **Library-internal.** The generator lives entirely under
   `ifa/testing/`. No new top-level dependency. `libifa_gc.a`
   stays frontend-free.

2. **Same builders the frontends use.** The generator constructs
   IF1 by calling `if1_*` from `ifa/if1/if1.{h,cc}`. Same code
   paths as production; bugs in those APIs surface in both
   frontends and synthetic fixtures.

3. **Explicit CS identity.** Per 09c, multiple splitter stages
   care about CreationSet distinctions that the type system can't
   see. The generator API must let a fixture say "two distinct
   allocations of the same type" without relying on type-stage
   absorption.

## 2. API layering

Three layers, designed bottom-up but specified top-down here so
the layering is visible at a glance.

### Layer 1 — Raw `if1_*` (no wrapping)

The existing API (see 09b §"API surface used") is the right
abstraction for "emit a single IF1 thing." Direct use, no shim.

### Layer 2 — Code-builder helpers (`ifa/testing/ir_builder.{h,cc}`)

Ergonomic composites that eliminate the repetitive `&body`-
passing and args-array boilerplate in V's `gen_*` patterns.

```cpp
// ifa/testing/ir_builder.h

class CodeBuilder {
  Code *code_ = nullptr;
 public:
  // Direct emission.
  void move(Sym *src, Sym *dst);
  void label(Label *l);
  void goto_(Label *l);

  // Sends in the three idiomatic flavors.
  // - Method dispatch: `(send <method_sym> <receiver> args... => result)`
  Sym *send_method(Sym *method, Sym *recv, std::initializer_list<Sym *> args,
                   Sym *result = nullptr);
  // - Operator: `(send sym_operator a sym_op b => result)` — n-ary in `args`
  Sym *send_op(Sym *op_sym, std::initializer_list<Sym *> args,
               Sym *result = nullptr);
  // - Primitive: `(send sym_primitive <prim_sym> args... => results...)`
  void send_prim(Sym *prim_sym, std::initializer_list<Sym *> rvals,
                 std::initializer_list<Sym *> lvals);
  // - Reply terminator
  void reply(Sym *cont, Sym *ret);

  // Control flow with lambda bodies.
  void if_(Sym *cond, std::function<void(CodeBuilder &)> then_,
                       std::function<void(CodeBuilder &)> else_ = {});
  void while_(std::function<Sym *(CodeBuilder &)> cond,
              std::function<void(CodeBuilder &)> body);

  // Insert sub-builder's output (delegates to if1_gen).
  void splice(Code *sub);

  // Hand off to if1_closure / other consumers.
  Code *finish() { return code_; }
};

// Closure builder — collects formals + body via fluent interface,
// emits if1_closure on finish().
class ClosureBuilder {
  Sym *fn_ = nullptr;
  Vec<Sym *> args_;
  Sym *self_ = nullptr;
  // ...
 public:
  ClosureBuilder(cchar *name);  // creates the fn Sym
  ClosureBuilder &arg(Sym *s) { args_.add(s); return *this; }
  ClosureBuilder &args(std::initializer_list<Sym *> ss);
  ClosureBuilder &self(Sym *s) { self_ = s; return *this; }
  // Build the body; receives the builder + cont/ret Syms so the
  // caller can emit a reply at the end.
  Sym *body(std::function<void(CodeBuilder &, Sym *cont, Sym *ret)>);
};

// Record-type builder.
class RecordBuilder {
  Sym *type_ = nullptr;
 public:
  RecordBuilder(cchar *name);
  RecordBuilder &field(cchar *name, Sym *type = nullptr);
  Sym *build();
};

// Allocation + field accessors — the most common patterns,
// hoisted to one-liners.
namespace ir {
  // Returns a fresh Sym holding a new instance of `type`.
  Sym *new_instance(CodeBuilder &cb, Sym *type, cchar *result_name = nullptr);
  // Field setter: emits `(send sym_operator obj sym_setter sym_field val => _)`
  void set_field(CodeBuilder &cb, Sym *obj, cchar *field, Sym *val);
  // Field getter: returns the result Sym.
  Sym *get_field(CodeBuilder &cb, Sym *obj, cchar *field,
                 cchar *result_name = nullptr);
  // Method call: `obj.method(args...) → result`
  Sym *call_method(CodeBuilder &cb, Sym *obj, cchar *method,
                   std::initializer_list<Sym *> args,
                   cchar *result_name = nullptr);
}
```

Layer 2 is **~300 LOC** estimated, mirroring V's `gen_op`,
`gen_fun`, `gen_loop`, `gen_if` functions without the AST/scope
plumbing.

### Layer 3 — Shape generators (`ifa/testing/ir_shapes.{h,cc}`)

Whole-program constructors targeting specific splitter stages.

```cpp
// ifa/testing/ir_shapes.h
namespace IRShape {

// Each function below builds a complete IF1 program rooted at
// sym___main__. Caller is responsible for ifa_init(...) and the
// pipeline (ifa_analyze, etc.).

// Stage 1 (type): n_types distinct typed values flow into a
// single shared formal via n_callsites callers.
void polymorphic_formal(int n_types, int n_callsites);

// Stage 2 (mark-type): two CSes of the same type, distinguished
// only by allocation site, with downstream dispatch that depends
// on the distinction.
void same_type_dispatch();

// Stage 3c (setter / split_css): vector-element setter pattern
// mirroring pyc's list runtime. Two vectors with distinct element
// types, both feeding a shared reader.
void vector_element_polymorphism(int n_types);

// Stage 4 (setter-of-setter): two record types in a setter chain
// — value flows through field A then field B.
void setter_chain(int chain_length, int n_types);

// Stage 7 (violation): polymorphic receiver where some flow paths
// lack the dispatched method; splitting the caller resolves it.
void missing_method_dispatch();

// Stages 5, 6 added when stages 2/4 are working.
}
```

Each `IRShape::*` is **~50-100 LOC**: build types, build closures,
build main body, done. The shape doesn't include `ifa_init` — the
runner does that.

## 3. Fixture format

Two file types under `ifa/tests/synthetic/`:

### Primary: `.synth` config

```text
;; ifa/tests/synthetic/poly_3types_2sites.synth
shape: polymorphic_formal
n_types: 3
n_callsites: 2
```

Runner parses with ~30 lines of code (key: value lines), dispatches
to the named `IRShape::*` with the given parameters. Goldens at
`<name>.<phase>.expected`.

### Escape hatch: `.cc` stub

```cpp
// ifa/tests/synthetic/custom_weird_shape.cc
#include "testing/ir_builder.h"
extern "C" void synth_build() {
  // arbitrary IF1 construction here
}
```

For one-off shapes where the parameter space of `IRShape::*`
doesn't fit. Runner compiles + loads via the shared object loader
(or, simpler: links into the test binary at build time and looks
up by name).

**Start with `.synth` only.** Promote to `.cc` if the fixture
count grows past ~20 OR a fixture needs arbitrary code.

## 4. Runner integration

`ifa/testing/ifa_test_main.cc` already discovers fixtures by
walking `ifa/tests/ir/<phase>/*.ir`. Extend it:

1. Look in `ifa/tests/synthetic/*.synth` as well.
2. For each `.synth`, parse parameters, dispatch to
   `IRShape::<shape_name>(args...)`.
3. Use the same per-phase printer + golden-diff pipeline.

Fixture-discovery code shouldn't grow by more than ~50 LOC.

A single `.synth` fixture has goldens for whatever phases apply,
just like `.ir` fixtures:

```
ifa/tests/synthetic/
  poly_3types_2sites.synth
  poly_3types_2sites.fa-converge.expected
  poly_3types_2sites.dce.expected
```

## 5. Validating the generator

The generator constructs IF1; how do we know IT's correct?

Two complementary checks:

1. **Self-validation via existing phases.** Every synthetic
   fixture runs through the existing phase printers (`fa-init`,
   `fa-converge`, `dce`, etc.). The phase outputs ARE the
   regression markers: a generator change that breaks IF1
   construction shows up as goldens drifting in obvious ways
   (PNode counts, ess/css totals, fa-rc).

2. **V-equivalence check (during transition).** While V's
   `ast_to_if1.cc` is still around (Phase 09 deletion group 3,
   deferred), pick 1-2 V test programs and re-implement their
   shape via `IRShape::*`. Compare the produced IF1 (modulo Sym
   naming) against V's lowering output of the same program. If
   they match, the generator covers V's API surface; if not, the
   delta tells us what builder is missing.

   When V is deleted, this check goes away. Hopefully unused by
   then.

No additional generator-specific test suite. The synthetic
fixtures themselves are the test surface.

## 6. Implementation order (Phase C plan)

Concrete sequence with rough effort estimates. Each step is a
commit.

**Step 7.1: Layer 2 — CodeBuilder + helpers** (1 day)
- `ifa/testing/ir_builder.{h,cc}` with `CodeBuilder`,
  `ClosureBuilder`, `RecordBuilder`, the `ir::` helpers.
- One unit test exercising each builder via a "build hello-world"
  fixture (a single closure that does `move x → r; reply`).
- Wire into `ifa/Makefile` `LIB_SRCS`.

**Step 7.2: Runner integration** (½ day)
- Extend fixture scanner to find `.synth` files.
- Parse `.synth` config (~30 LOC).
- Dispatch table `cchar* → void(*)(int...)` for shape names.

**Step 7.3: First shape — `polymorphic_formal`** (½ day)
- Mirror what `02_splitter.ir` does, but via the generator.
- First `.synth` fixture: `poly_2types_2sites.synth`.
- Golden under `fa-converge` phase locks `splits[type] = 1`,
  same as `02_splitter`.
- This proves end-to-end: builder → fixture → runner → golden.

**Step 7.4: Stage-2 shape — `same_type_dispatch`** (1 day)
- `IRShape::same_type_dispatch()`. First fixture that should hit
  `mark-type` (per 09c's analysis).
- If `mark-type` doesn't fire, that's data — the stage may be
  unreachable. File as dead-code-candidate issue.

**Step 7.5: Stage-4 shape — `setter_chain`** (1 day)
- `IRShape::setter_chain(chain_length, n_types)`. First fixture
  targeting `setter-of-setter`.
- Same outcome split: either stage fires (good, lock it) or
  doesn't (dead-code candidate).

**Step 7.6: Stage-7 shape — `missing_method_dispatch`** (1 day)
- `IRShape::missing_method_dispatch()`. Polymorphic receiver
  where dispatch fails on one type.
- Should hit `violation`.

**Step 7.7: Stage-3c shape — `vector_element_polymorphism`** (2 days)
- Hardest. Requires `@vector` primitive setup + the cs_map
  plumbing. Look at pyc's list-runtime IR (dump via `pyc -x 1`)
  for reference shape.

**Step 7.8: V deletion sequence** (1 day)
- Group 1: delete grammars (`v.g`, `c.g`, parser tables — 40k LOC).
- Group 2: delete parser driver (`parse.cc`, `make_ast.cc`, etc.).
- Group 3: delete `ast_to_if1.cc`, `scope.cc`.
- Update `ifa/main.cc` and `ifa/Makefile`.
- Delete `ifa/tests/*.v`.
- Doc updates (`ARCHITECTURE.md`, `FRONTEND.md`, `LLVM.md`).

**Step 7.9: Stage 5 + 6 attempts** (1-2 days, possibly drops)
- Try generator-targeted shapes for `mark-setter` and
  `mark-setter-of-setter`. If they fire, lock fixtures. If not,
  add to dead-code-candidate list for a follow-up audit.

**Step 7.10: Documentation** (½ day)
- Update `IFA.md` and a new `ifa/testing/IR_GENERATOR.md` with
  the generator API + how to write synthetic fixtures.
- Update Phase 09 doc to mark steps complete.

**Total effort: ~9-12 days.** Spread across commits, each
independently bisectable.

## 7. Risk + mitigations

| Risk | Mitigation |
|------|------------|
| Generator API turns out wrong after first 2-3 shapes | Layer 2 is small (~300 LOC); cheap to refactor. The shapes themselves are short. |
| Stage 5/6 prove unreachable | That's a finding worth having. File a separate dead-code-removal issue. |
| Stage 3c (split_css) too complex to construct | Fall back to "pyc tests cover it" (the 4 existing pyc tests). The fixture is a nice-to-have, not load-bearing. |
| V's lowering reference becomes stale during deletion | Defer group 3 deletion until generator is mature. Per 09a's grouping. |
| `.synth` parsing too rigid for some fixture | Promote to `.cc` escape hatch. |

## 8. Acceptance criteria

- [ ] `ifa/testing/ir_builder.{h,cc}` lands with Layer 2 API.
- [ ] Runner can discover and execute `.synth` fixtures.
- [ ] At least one shape per stage that has a real trigger has a
      locked golden under `fa-converge`.
- [ ] V deleted entirely (groups 1, 2, 3).
- [ ] `libifa_gc.a` builds with no frontend dependency.
- [ ] `make test-ir` runs without pyc and exercises stages
      `type`, `setter`, plus any new stages the generator unlocked.
- [ ] `IR_GENERATOR.md` documents how to write new fixtures.
- [ ] Dead-code-candidate audit issue filed for any stage that
      survived deletion-targeted shape construction (i.e., the
      generator tried to trigger it and failed).

## 9. Open questions

- **Self vs container parameter passing.** V's lowering wires
  `fn->self` for method-style funs and emits a method-shape send.
  The generator API should make method-vs-function distinction
  explicit; sketch above uses `ClosureBuilder::self()`. Validate
  on the first method-shape shape.
- **Per-shape randomness for fuzzing follow-up.** Not in scope
  for this phase but worth ensuring Layer 2's API supports a
  seeded variant later (the generator should be deterministic; a
  fuzzer would wrap it with parameter ranges).
- **Generator-specific timing/perf.** Some shapes may be slow to
  analyze (recursive patterns, large containers). Document a
  per-fixture timeout in `.synth` if needed; default tight (5s?)
  to catch runaway FA.

## 10. What this doesn't do

- Doesn't add new IFA features. It only constructs IF1 that
  exercises existing ones.
- Doesn't change the `if1_*` API surface. If a builder turns out
  awkward, the right fix is in Layer 2, not in `ifa/if1/`.
- Doesn't add a new test runner. Reuses `ifa-test`.
- Doesn't lock IFA behavior more tightly than goldens do today
  — `.synth` fixtures produce text goldens just like `.ir`
  fixtures.

---

Next: Phase C — execute. Step 7.1 (Layer 2) is the first
implementation commit.
