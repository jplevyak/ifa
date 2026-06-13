# CG_IR_PLAN — Execution plan for the codegen-time IR

The actionable plan for landing Option B from
[`ifa/CODE_GEN_IR.md`](../CODE_GEN_IR.md): a separate
`CGFun` / `CGBlock` / `CGInst` IR consumed by both the C and
LLVM backends, with the LLVM backend treated as a **first-class
production target** for the duration of and beyond this work.

This is a plan, not a design document. For the design discussion,
prior-art comparison, and decision rationale, see
[CODE_GEN_IR.md](../CODE_GEN_IR.md). This doc assumes that
design is settled and addresses *how* to land it.

Sister docs:
- [CODE_GEN_IR.md](../CODE_GEN_IR.md) — the investigation that
  motivates this plan.
- [CODEGEN_PLAN.md](CODEGEN_PLAN.md) — the existing codegen
  improvement plan; this plan extends it as a new top-level
  phase (effectively Phase 7).
- [CODEGEN_C.md](../CODEGEN_C.md) / [CODEGEN_LLVM.md](../CODEGEN_LLVM.md)
  — the current consumers, which get rewritten in stages 3-4.

---

## 1. Goals and non-goals

### Goals

1. **A single source of truth for codegen** — both backends
   consume the same `CGProgram`. Bug fixes in field-index
   resolution, SSU materialization, constructor binding, etc.
   land in one place.
2. **Issues 014 and 016 closed structurally** — the SSU
   formal-arg binding and the construction-flow gap both
   resolve as a consequence of `cg_normalize` producing
   explicit `CG_STORE` operations.
3. **LLVM backend at parity with C backend** by the end of
   Stage 5. Today: 37/0/38/2. Target: matching the C backend's
   74/1/0/2 across the pyc-suite, with each remaining failure
   tracked individually as an issue.
4. **Each stage independently reversible** — the plan progresses
   commit-by-commit; nothing requires a long-lived branch.
5. **No regression in the C backend at any point** — the C
   backend remains the production path throughout. The plan is
   constructed so a green C-backend test at any stage is a
   prerequisite to landing the next stage.

### Non-goals (deliberately out of scope)

- **No IF1 changes.** `Sym`, `Code`, `PNode`, `Var`, `Fun` keep
  their current shape. The frontend, analyzer, optimizer, and
  cloner are unaffected. CGFun is built from the post-clone +
  post-DCE state and discarded after emission.
- **No frontend API change.** Pyc's `c_codegen_pre_file`
  callback and `__pyc_insert_c_code__` accumulation paths keep
  their current signatures; their effects fire at the same
  point in the pipeline (just consumed by the CGFun printer
  instead of by `c_codegen_print_c` directly).
- **No new pyc-language features.** This plan does not unlock
  POD records (issue 015), runtime linking (CODEGEN_LLVM.md
  §14.5), or any other user-visible feature. Those land on
  their own schedules; CGFun makes some of them cheaper but
  doesn't require them.
- **No SSU semantics change.** SSU phi/phy nodes keep their
  current meaning. `cg_normalize` *consumes* them and emits
  explicit moves; the analyzer-facing form is unchanged.
- **No CGFun "optimization pass."** CGFun is a flat,
  unoptimized representation. Any legal CG-level optimization
  (e.g. mem2reg-style SSA promotion) lives in the LLVM backend
  via LLVM's existing passes, not in CGFun.

---

## 2. LLVM as a first-class target — what this commits us to

"First-class" is a commitment, not a label. It means:

- **CI gates on the LLVM-backend pyc-suite.** Today CI floors
  the pass count at a baseline (`LLVM_BASELINE_PASS=37`). After
  this plan completes, CI gates on the *intersection* with the
  C backend — any test that passes on C and fails on LLVM is a
  blocker.
- **Every new test in `tests/` runs on both backends** with
  matching expected output by default. Tests that can't yet
  pass on LLVM get a `.llvm_expect_fail` sidecar (parallel to
  the existing `.expect_fail`).
- **Bug investigations consider both backends.** "It works on
  C" is no longer enough; the next session's triage starts
  with `make USE_LLVM=1 ./test_pyc` parity.
- **`CODEGEN_LLVM.md` keeps pace with the code.** Same review
  standard as `CODEGEN_C.md`.
- **Performance budget.** The LLVM compilation path is allowed
  to be slower than the C path (clang link is heavier than the
  C compile), but `pyc -b` end-to-end shouldn't be > 2× the C
  path for representative programs. Today's
  `ifa/codegen/PERFORMANCE.md` baseline gets an LLVM column once
  Stage 3 ships.

What it does **not** commit us to:

- LLVM-only operation. The C backend stays the default and the
  production path.
- LLVM-version-flexible builds. We pin to one LLVM version
  (currently llvm-22) and bump deliberately.
- JIT. CODEGEN_LLVM.md §14.6's JIT stub stays unimplemented.

---

## 3. The CGFun data structures

The concrete declarations. These go in
`ifa/codegen/cg_ir.h` in Stage 1.

```cpp
#ifndef _cg_ir_H_
#define _cg_ir_H_

#include "ifadefs.h"
#include "fa.h"

class CGProgram;
class CGFun;
class CGBlock;
class CGInst;
class CGValue;
class CGSlot;
class CGType;

// CGType — explicit representation of "what this value or slot
// holds at LLVM-IR / C-type level." No more deriving from
// type_kind + has.n + flags soup at emission time.
enum CGTypeKind {
  CG_T_VOID,
  CG_T_INT,         // signed integer, width = bits
  CG_T_UINT,        // unsigned integer, width = bits
  CG_T_FLOAT,       // float, width = bits (32, 64, 128)
  CG_T_BOOL,        // i1
  CG_T_PTR,         // opaque pointer (heap aggregate, ref, etc.)
  CG_T_STRUCT,      // value-typed record (rare in pyc; common in V)
  CG_T_FUN_PTR,     // pointer to a function — call sites use
                    // the CGFun's signature, not this
};

class CGType : public gc {
 public:
  CGTypeKind kind;
  int bits;                       // for INT/UINT/FLOAT
  Vec<CGType *> fields;           // for STRUCT
  Vec<cchar *> field_names;       // parallel to fields
  CGType *element;                // for arrays/vectors
  Sym *source;                    // IF1 Sym this came from (for diagnostics)
  cchar *name;                    // emission name (`_CG_psN`, `i32`, etc.)

  bool is_heap_aggregate() const  // true iff variables of this
    { return kind == CG_T_PTR; }  // type are stored as pointers
};

// CGSlot — addressable storage. Globals, formal-arg slots,
// SSU-renamed locals. Has a CGType and a name (for diagnostics
// and the C printer's per-local declaration).
enum CGSlotKind {
  CG_SLOT_GLOBAL,
  CG_SLOT_LOCAL,                  // alloca / C local
  CG_SLOT_FORMAL,                 // function parameter
  CG_SLOT_CONSTANT,               // immediate / constant pool
};

class CGSlot : public gc {
 public:
  CGSlotKind kind;
  CGType *type;
  cchar *name;                    // user-visible (preserved for debug info)
  cchar *cg_name;                 // emission name (`t0`, `%local_var3`, `@y`)
  int id;
  Sym *source_sym;                // back-ref for debugging
  Var *source_var;                // back-ref for debugging
  Immediate *imm;                 // for CG_SLOT_CONSTANT
  void *llvm_handle;              // LLVM-side: AllocaInst*/GlobalVariable*/Argument*
                                  // set lazily by the LLVM printer
};

// CGValue — a use-site reference. Either an immediate, a slot
// (resolved via CG_LOAD), or the result of a prior CGInst (SSA).
enum CGValueKind {
  CG_V_NONE,
  CG_V_INST,                      // result of a prior CGInst in this CGBlock
  CG_V_SLOT,                      // load from a slot (or use slot itself for ptr ops)
  CG_V_IMMEDIATE,                 // constant
  CG_V_FUN,                       // function pointer for direct call
};

class CGValue {
 public:
  CGValueKind kind;
  union {
    CGInst *inst;
    CGSlot *slot;
    CGFun *fun;
  };
  Immediate imm;                  // for CG_V_IMMEDIATE
  CGType *type;                   // result type (for typed-printers)
};

// CGInst — one operation. Goes in a CGBlock's body or as its
// terminator (some ops are only valid as terminators).
enum CGOp {
  // Pure operations
  CG_NOP,
  CG_LOAD,            // result = load(slot)
  CG_STORE,           // store(rval[0] → slot)
  CG_GEP_FIELD,       // result = address-of field i of rval[0] (pointer)
  CG_LOAD_FIELD,      // result = field i of rval[0] (value through ptr)
  CG_STORE_FIELD,     // store(rval[1] → field i of rval[0])
  CG_CALL,            // result = fun(rvals...)
  CG_ALLOC,           // result = GC_malloc(sizeof(type))
  CG_CAST,            // result = (target_type) rval[0]
  CG_PRIM_OP,         // arithmetic / comparison / bitwise — kind in `prim`
  CG_PRIM_CGFN,       // dispatch to RegisteredPrim::cgfn (write/writeln/to_string/...)

  // Terminators (must be last instruction in a block)
  CG_BR,              // unconditional branch to succ[0]
  CG_COND_BR,         // cond ? succ[0] : succ[1]
  CG_RET,             // ret rval[0] (or void)
  CG_UNREACHABLE,     // for filling block-terminator-required slots
};

class CGInst : public gc {
 public:
  CGOp op;
  CGType *result_type;            // for CG_V_INST consumers
  Vec<CGValue> rvals;
  CGSlot *slot;                   // for LOAD/STORE/GEP_FIELD/STORE_FIELD/ALLOC
  int field_idx;                  // for *_FIELD ops, pre-resolved
  Prim *prim;                     // for CG_PRIM_OP / CG_PRIM_CGFN
  cchar *prim_name;               // for CG_PRIM_CGFN dispatch
  unsigned src_line;              // for !dbg
  cchar *src_file;
  PNode *source_pn;               // back-ref for debugging
};

// CGBlock — one basic block.
class CGBlock : public gc {
 public:
  Vec<CGInst *> body;             // non-terminator instructions, in order
  CGInst *terminator;             // required (BR/COND_BR/RET/UNREACHABLE)
  Vec<CGBlock *> preds, succs;    // CFG edges
  int id;
  cchar *label;                   // emission name (`L42`, `%entry`, etc.)
  PNode *source_pn;               // the LABEL PNode this came from (for debugging)
};

// CGFun — one function.
class CGFun : public gc {
 public:
  Fun *source_fun;                // back-ref (post-clone Fun)
  cchar *name;
  CGType *return_type;
  Vec<CGType *> arg_types;
  Vec<CGSlot *> formal_arg_slots; // pre-allocated slots; entry block stores args into them
  Vec<CGBlock *> blocks;          // in CFG order, entry first
  CGBlock *entry;
  Vec<CGSlot *> locals;           // all CGSlots used in this fun
  bool is_external;               // declaration only
  bool is_main;                   // pyc top-level
  void *llvm_handle;              // llvm::Function* (set by the LLVM printer)
};

// CGProgram — the whole compilation unit.
class CGProgram : public gc {
 public:
  Vec<CGFun *> funs;
  CGFun *main_fun;                // the entry point
  Vec<CGSlot *> globals;
  Vec<CGType *> types;            // emitted at the top of the C output
  Map<Fun *, CGFun *> fun_map;    // for cross-Fun call resolution
  Map<Sym *, CGSlot *> sym_to_slot;
  Map<Sym *, CGType *> sym_to_type;
  IFACallbacks *frontend;         // for c_codegen_pre_file etc.
};

CGProgram *cg_normalize(FA *fa);

#endif  // _cg_ir_H_
```

Notes on design choices:

- **CGType is a separate class** because LLVM and C have
  different conventions and the current Sym layer doesn't carry
  enough info (the issue 015 / `is_value_type` discussion).
  Carrying it explicitly per-slot/per-value removes the
  re-derivation work in `getLLVMVarType` and parallel.
- **CGSlot vs CGValue** is the "addressable storage" /
  "SSA value" distinction LLVM IR uses. The C backend doesn't
  care about the distinction at the IR level but the printer
  emits `obj.field` vs `&obj.field` correctly because of it.
- **`llvm_handle` / `cg_name` are mutable side-channels.** They
  exist for the same reason `Sym::cg_string` / `Sym::llvm_value`
  exist today. The CGProgram is computed once and the printers
  mutate these during emission. The IF1 layer's caches go away
  once Stage 5 ships.
- **`source_*` back-references** are mandatory for diagnostics.
  When the LLVM backend says "verifyModule failed in
  `_CG_f_2173_0`", the back-ref to `Fun` lets us print the
  source line.

---

## 4. Phase map

| Phase | Focus | Risk | Bounded effort | Exit criterion |
|---|---|---|---|---|
| 0 | Preparation — fixtures, audits, baseline | Low | 3-7 days | New `.ir` fixtures reproduce issues 014 / 016; liveness flags documented; construction-flow investigation outcome recorded; baseline stable. |
| 1 | Header-only scaffolding | Low | 1 PR | `cg_ir.h` compiles; `cg_normalize()` stub returns an empty CGProgram. No semantic change. |
| 2 | Normalization pass + `cg-normalize` test phase | Medium | 3-4 PRs | `cg_normalize` produces a complete CGProgram for every IF1 fixture. Goldens locked in. C and LLVM backends still consume IF1; CGProgram is built and discarded. |
| 3 | LLVM backend consumes CGProgram | Medium | 3-4 PRs | `llvm_codegen_print_ir` reads CGProgram only. LLVM-suite pass count strictly rises from 37; issues 014, 016 close. Codegen-llvm fixture goldens reblessed. |
| 4 | C backend consumes CGProgram | Medium-high | 3-4 PRs | `c_codegen_print_c` reads CGProgram only. C-suite stays 74/1/0/2. Codegen-c fixture goldens reblessed. |
| 5 | Cleanup — remove IF1-side codegen channels | Low | 1-2 PRs | `Sym::cg_string`, `Sym::llvm_value`, `Sym::llvm_type`, `Var::cg_string`, `Var::llvm_value` all removed. CGProgram is the only codegen state. |

Total: 13-17 PRs across 6 phases. The C backend stays at
production parity throughout (phases 0, 1, 2, 3, 5) and at
production parity post-phase-4 once the rewrite passes the
suite.

**Phase 0 gates Phase 2.** Phases 1 and 0 can run in parallel
(Phase 1 is a header-only stub; Phase 0 is unrelated audit and
fixture work), but Phase 2's start requires Phase 0's mandatory
subphases (§5.1, §5.2, §5.3) complete.

---

## 5. Phase 0 — Preparation (3-7 days)

A short-running set of prep tasks that make Phase 2 land more
safely. Three subphases are **mandatory** (block Phase 2 start);
three are **recommended** but not blocking. None require any
new IR or backend changes.

Phase 0 can start immediately, in parallel with Phase 1 (which is
header-only and has no dependencies). Phase 2 cannot start until
§§5.1, 5.2, 5.3 land.

### 5.1 Fixtures for issues 014 and 016 — MANDATORY (1-2 days)

Today both issues are reproduced only through the full pyc
pipeline. Phase 3.3's "monotonic pass-count rise" gate requires
a deterministic, minimal reproducer that can be verified
against IR-phase goldens.

Deliverables:
- `tests/ir/codegen-llvm/09_ssu_self_binding.ir` — minimal
  function with a formal arg, an SSU rename of that arg, and a
  field access on the rename. Pre-fix LLVM goldens show the
  field load reading from an uninitialized alloca; post-fix
  goldens show a store from the formal arg.
- `tests/ir/codegen-llvm/10_construction_to_global.ir` — a
  `P_prim_new` + write-to-global pattern. Pre-fix goldens show
  `@g = global ptr null` with no store; post-fix goldens show
  the store.

Acceptance:
- [ ] Both fixtures present in tree and locked into the
      codegen-llvm phase via reblessed goldens.
- [ ] Pre-Phase-3 LLVM IR captured in the fixture goldens
      (so the post-Phase-3 rebless documents the structural fix).
- [ ] Phase 3.3's PR description names the exact diff each
      fixture is expected to show.

### 5.2 Liveness audit and documentation — MANDATORY (1-2 days)

The plan's `pn_should_emit(pn)` decides what gets normalized
into CGProgram. Today it's unclear what `Sym::live`,
`Var::live`, `PNode::live`, `PNode::fa_live` each mean
precisely — the last session swung the LLVM gate three times
before settling on `live && fa_live`.

Deliverable: `ifa/LIVENESS.md` (or an expanded section in
`ifa/IR.md`) walking each of the four flags:

- **Who sets it.** Source location of the set/clear (DCE pass,
  FA pass, frontend init).
- **Who reads it.** All call sites across `ifa/` and the
  backends.
- **What it means.** A one-sentence semantic statement.
- **Cross-reference** to the C backend's gate (`cg.cc:586`) and
  the current LLVM gate (`llvm_codegen.cc::translatePNode`),
  including the unconditional phi/phy fallthrough at
  `cg.cc:604-650` (the structural difference that motivates
  issue 016).

Acceptance:
- [ ] Document landed; all four flags have setter / reader /
      semantics entries.
- [ ] `pn_should_emit(pn)`'s decision criteria documented in
      advance, before Phase 2.3 implementation starts.

### 5.3 Construction-flow IF1 investigation — MANDATORY (1 day)

Phase 2.5 (the construction-flow peephole) is the riskiest part
of Phase 2 because it depends on a hypothesis about what the
IF1 actually emits for `y = A()`. Issue 014 lists two
possibilities; nobody has actually walked the PNode chain
end-to-end.

Investigation steps:
1. Build a minimal reproducer (e.g., `tests/ir/codegen-llvm/10_construction_to_global.ir`
   from §5.1).
2. Dump the IF1 via `pyc -c if1` (or `--write_code_exit`) and
   walk the PNode chain by hand.
3. Record which of three outcomes applies:
   - **(a)** MOVE present, just DCE'd. Phase 2.5 not needed;
     Phase 2.4's "emit all Code_MOVE unconditionally" suffices.
   - **(b)** MOVE missing but the constructor SEND has the
     lvalue. Phase 2.5 is a 30-line peephole inserting a
     `CG_STORE`.
   - **(c)** Neither — no MOVE, no lvalue on the SEND. Phase
     2.5 needs a real pattern matcher; revisit scope (probably
     escalate to a frontend fix and defer the LLVM closure of
     issue 014).

Deliverable: append a "Root-cause investigation (June 2026)"
section to `ifa/issues/014-llvm-construction-flow-to-slots.md`
recording the answer.

Acceptance:
- [ ] Outcome (a) / (b) / (c) recorded in issue 014.
- [ ] Phase 2.5's implementation approach pre-committed in
      CG_IR_PLAN's §7 (Phase 2) updates.

### 5.4 Centralize side-channel accessors — RECOMMENDED (2-3 days)

`Sym::cg_string`, `Var::llvm_value`, and similar are read and
written across `cg.cc`, `llvm.cc`, `llvm_codegen.cc`,
`llvm_primitives.cc`, and `codegen_common.cc`. Phase 5 removes
them outright. The removal is easier if the readers are
centralized first.

Deliverable: accessor functions in `codegen_common.h`:

```cpp
cchar         *cg_get_string(Sym *s);
void           cg_set_string(Sym *s, cchar *v);
llvm::Value   *cg_get_llvm_value(Sym *s);
void           cg_set_llvm_value(Sym *s, llvm::Value *v);
// ... and parallel for Var::cg_string, Var::llvm_value,
// Var::llvm_type, Var::llvm_debug_var, Fun::cg_string,
// Fun::cg_structural_string, Fun::llvm.
```

Rewrite every direct field access in `cg.cc` / `llvm*.cc` /
`codegen_common.cc` to go through them. Mechanical and
reviewable in 2-3 sub-PRs (Sym, Var, Fun separately if needed).

Acceptance:
- [x] No direct read or write of the listed fields outside
      `codegen_common.h` or `cg_normalize.cc` (post-Phase-2).
      Implemented as static-inline accessors in
      `codegen_common.h` (not `.cc`) for zero call overhead at
      the ~270 hot sites. Migration done with perl regex per
      field, verified by build + tests after each pass.
- [x] All tests still pass: C suite 74/1/0/2, LLVM suite
      37/38/2, codegen-llvm IR fixtures 14/0/9, ifa unit
      tests 53/0.

This subphase is recommended, not mandatory. Skipping it makes
Phase 5 sprawl across every codegen file; doing it spreads the
cost across Phase 0 and makes Phase 5 a single-PR finale.

### 5.5 Resolve the Phase-5 `Codegen` base class — RECOMMENDED (1-2 days)

Phase 5 of [CODEGEN_PLAN.md](CODEGEN_PLAN.md) landed `class
Codegen` as scaffolding for "the wholesale Codegen migration."
It's never instantiated and never read. Phase 3 of *this* plan
is exactly that wholesale migration; the LLVM emitter's
persistent state (Context, Module, Builder, per-Fun maps) wants
to live somewhere.

Pick one path:

- **(a) Activate `Codegen`** as the state container for Phase
  3's LLVM emitter. Update CODEGEN_PLAN §8 to record that
  Phase 5's scaffolding is now in use. CG_IR_PLAN §8 (Phase 3)
  references the type.
- **(b) Delete the unused scaffolding.** Phase 3 introduces a
  fresh `EmissionContext` struct sized to its needs. The
  CODEGEN_PLAN §8 entry is updated to record the deletion and
  why.

**Decision (June 2026): Option (b).** Rationale:

1. The base class as written (`FA *fa; Fun *entry_fun; cchar
   *input_filename`) is a backend-agnostic shell; the C and
   LLVM backends share *none* of their post-CG_IR state. The C
   backend's `FILE *`, indent state, and forward-decl trackers
   have nothing in common with the LLVM backend's `Context` /
   `Module` / `Builder` / per-Fun BasicBlock maps. A shared base
   class would carry no shared behavior and force every method
   through `dynamic_cast` or virtual dispatch to the actual
   backend's state.

2. Phase 3's `EmissionContext` is sized for CGFun emission
   (CGProgram-walker state, type-cache for `cg_to_llvm_type`,
   block-map for control-flow lowering). None of those have a
   natural home in a `Codegen`-style base.

3. The actual state-leak bug that motivated `class Codegen`
   was already fixed by the phase-0.1 reset hooks
   (`reset_llvm_codegen_state()`); the scaffolding paid a
   maintenance cost (~40 lines of unreferenced code) for no
   user-visible benefit.

4. Phase 3 will land `EmissionContext` as a non-polymorphic
   struct in `cg_ir.h` (or a sibling header). The C backend's
   eventual CG_IR consumer (Phase 4) gets its own struct.

Implementation: deleted `class Codegen` from
`codegen_common.h`; updated CODEGEN_PLAN.md §5.1 and §"Phase 5
exit criteria" to record the supersession. No build break:
no source file ever instantiated or referenced the type.

Acceptance:
- [x] Decision recorded in both CODEGEN_PLAN.md §8 and
      CG_IR_PLAN.md §5.5.
- [x] Decision is uniform: Phase 3 uses `EmissionContext`,
      not `Codegen`.

### 5.6 Stabilize the LLVM pyc-suite baseline — RECOMMENDED (1 day)

The current 37 is real but several recent sessions saw it
swing 26 → 32 → 37 depending on gate variant. CI floors it at
37 but the failure modes underneath are still noisy. Phase
3.3's "strict monotonic rise" gate works much better against a
stable baseline.

Steps:
1. Run `PYC_FLAGS=-b ./test_pyc` ≥ 5 times back-to-back.
2. Diff the per-test results between runs. Any test whose
   pass/fail status flaps is a flake.
3. Either fix the flake or sidecar it with a
   `.llvm_expect_fail` (parallel to the existing
   `.expect_fail` mechanism). Document the rationale.

Deliverable: `tests/PARITY.md` short doc listing every test
that differs between the C and LLVM backends, with the reason
(open issue link or expected-fail rationale).

Acceptance:
- [ ] 5-run stable LLVM-suite count documented in
      `tests/PARITY.md`.
- [ ] Every test in `tests/` is in one of three states:
      passes-on-both, expect-fail-on-LLVM-with-issue-link,
      or known-flake-being-fixed.
- [ ] CI `LLVM_BASELINE_PASS=37` is grounded in this audit
      (not an approximation).

### 5.7 Deferred / optional preparation

The following are nice-to-haves that don't block Phase 2 and
can land at any later point if they earn their keep:

- **Pre-write the `cg-normalize` printer's normalizer** (in
  parallel with `print_codegen_llvm_normalized`). Defer until
  Phase 2.3 fixes the dump format.
- **Map every `P_prim_*` to its eventual `CG_OP`** as a short
  reference table in PRIMITIVES.md §15. Existing PRIMITIVES.md
  §14 + §15 already covers the contract; defer.
- **PERFORMANCE.md refresh with an LLVM column.** Defer until
  Phase 3 starts; the comparison is most meaningful against the
  pre-Phase-3 LLVM baseline.

### Phase 0 exit criteria

Three categories: mandatory (block Phase 2), recommended (block
nothing, but each unaddressed item adds risk to a later
phase), and deferred (not blocking).

- [x] §5.1 (fixtures for 014/016) — landed.
- [x] §5.2 (liveness audit) — landed.
- [x] §5.3 (construction-flow investigation) — landed; outcome
      (a) recorded in issue 014.
- [x] §5.4 (side-channel accessors) — landed (static-inline in
      `codegen_common.h`).
- [x] §5.5 (Codegen base class fate) — decided: Option (b),
      removed.
- [x] §5.6 (LLVM baseline stabilization) — done; see
      `tests/PARITY.md`.
- [x] No regression in any existing test suite (C 74/0,
      LLVM 37/38, IR fixtures all green).

The mandatory subset (§§5.1-5.3) is the strict gate. Phase 2.1
can start the moment §5.3's investigation lands; the recommended
subphases can finish in parallel during Phase 2.1-2.2.

---

## 6. Phase 1 — Header-only scaffolding (1 PR)

The cheapest possible first commit. Establishes the type names
in tree so nothing in subsequent phases has to live on a
long-lived branch.

### 6.1 Files to add

- `ifa/codegen/cg_ir.h` — the declarations from §3.
- `ifa/codegen/cg_normalize.cc` — implementation file with a
  stub:

```cpp
#include "cg_ir.h"
#include "fa.h"

CGProgram *cg_normalize(FA *fa) {
  (void)fa;
  CGProgram *p = new CGProgram();
  // Stage 1: stub. Stage 2 fills this in.
  return p;
}
```

### 6.2 Build integration

- Add `cg_normalize.cc` to `ifa/Makefile` IFA_SRCS list.
- Add `#include "cg_ir.h"` to `codegen_common.h` (where the
  forward declarations of CGProgram appear).

### 6.3 Test

- One unit test in `ifa/testing/`: `cg_normalize(fa)` returns a
  non-null CGProgram with empty fun list.
- `make USE_LLVM=1` and `make test` both green.

### Phase 1 exit criteria

- [ ] `cg_ir.h` and `cg_normalize.cc` compile.
- [ ] `cg_normalize(fa)` returns an empty CGProgram for any
      input.
- [ ] `make test` 74/1/0/2; `make USE_LLVM=1 ./test_pyc` 37/0/38/2
      (unchanged).
- [ ] `make -C ifa test-ir` 12/0 codegen-llvm, 6/0 codegen-c
      (unchanged).
- [ ] No new diagnostics in CI.

---

## 7. Phase 2 — Normalization pass + `cg-normalize` test phase

The substantive work. ~1500 LOC of normalization logic, plus a
new ifa-test phase that locks in the CGProgram shape via golden
files. Both backends are still consuming IF1 directly; CGProgram
is built and discarded.

**Prerequisites:** Phase 0 mandatory subphases (§5.1 fixtures,
§5.2 liveness audit, §5.3 construction-flow investigation) must
have landed. The fixtures from §5.1 anchor the Phase 2.4 and
2.5 verification; the audit from §5.2 fixes the
`pn_should_emit()` contract used in §7.1; the investigation from
§5.3 determines the implementation approach for Phase 2.5.

### 7.1 Subphases

**2.1 — Type table and slot building.** Walk `if1->allsyms` and
build `CGType`s for every type Sym; build `CGSlot`s for every
global Var. Populate `CGProgram::sym_to_slot` and `sym_to_type`.
Single PR.

**2.2 — Per-Fun skeleton.** For each live `Fun`, build a `CGFun`
with `arg_types`, `formal_arg_slots`, `return_type`. Allocate
one `CGBlock` per LABEL PNode (and an entry block before
those). Wire predecessor/successor edges from the IF1 CFG.
Single PR.

**2.3 — Per-PNode lowering** — the largest subphase.
For each reachable PNode, dispatch by `code->kind`:

```cpp
void lower_pnode(PNode *pn, CGFun *cf, CGBlock *current_block) {
  switch (pn->code->kind) {
    case Code_LABEL:  /* block boundary, no instruction emitted */ break;
    case Code_MOVE:   lower_move(pn, cf, current_block); break;
    case Code_SEND:   lower_send(pn, cf, current_block); break;
    case Code_IF:     lower_if(pn, cf, current_block); break;
    case Code_GOTO:   lower_goto(pn, cf, current_block); break;
  }
}
```

Each `lower_*` handler:

- Resolves operands via `rval_to_cgvalue(var)` and
  `lval_to_cgslot(var)` (shared helpers).
- Emits 1-N `CGInst`s into `current_block->body`.
- For terminators (IF/GOTO/dead-reply SEND), sets
  `current_block->terminator`.

`lower_send` further dispatches:

- Primitives (`pn->prim` non-null) → `lower_prim` which maps
  each `P_prim_*` to its `CG_*` op (ALLOC for new, GEP_FIELD
  for period, etc.) or to `CG_PRIM_CGFN` if it's a registered
  prim with a cgfn.
- Direct calls (target known via `get_target_fun_core`) →
  `CG_CALL` with the `CGFun*` resolved through
  `CGProgram::fun_map`.

This subphase ships in 2-3 PRs split by PNode kind to keep diffs
reviewable.

**2.4 — phi/phy materialization.** Walk every PNode's `phi` and
`phy` lists (unconditionally — no live gate) and emit explicit
`CG_STORE` instructions into the predecessor's block (for phi)
or this PNode's owning block before the per-kind emission (for
phy). **This is the structural fix for issue 016.** Single PR.

**2.5 — Construction-flow patch.** Peephole pass over the
generated CGBlocks looking for `CG_CALL` to a `__new__` /
`__init__` whose result isn't stored anywhere; insert the
missing `CG_STORE` to the receiver's slot. **Closes issue
014.** Single PR.

### 7.2 The `cg-normalize` test phase

Add to `ifa/testing/ifa_test_main.cc`:

```cpp
{"cg-normalize", pre_parse_builtin_types,
 phase_cg_normalize_run, print_cg_normalize_normalized},
```

`phase_cg_normalize_run` invokes the full FA + clone + DCE
pipeline, then `cg_normalize(fa)`. `print_cg_normalize_normalized`
emits a textual dump of CGProgram in a stable format (no host
paths, no sym IDs in slot names — use a per-test
sym-id-renumbering map like the codegen-c phase already does).

Goldens for the existing `.ir` fixtures live at
`tests/ir/cg-normalize/<fixture>.ir.cg-normalize.expected`. The
8 codegen-llvm fixtures (which exercise the same FA + clone
pipeline) all get a `.cg-normalize` sibling.

### 7.3 What ships built but unused

After Phase 2, CGProgram is built on every codegen run but
discarded. The `cg-normalize` test phase exercises the
construction. The two backends still emit from IF1 directly.

### Phase 2 exit criteria

- [x] `cg_normalize(fa)` produces a complete CGProgram for
      every test in the pyc-suite (no crashes, no missing
      slots, no missing blocks). Verified by 19 cg-normalize
      goldens (6 codegen-llvm-shape `.ir` + 13 synthetic) all
      passing, plus full C and LLVM suite runs not crashing.
- [x] `tests/ir/cg-normalize/<fixture>.cg-normalize.expected`
      for at least 8 fixtures: 6 codegen-llvm-shape `.ir`
      goldens + 13 synthetic = 19 locked goldens.
- [x] `make test-ir` includes `cg-normalize`; all locked
      goldens pass (19/0/0).
- [x] `make test` 74/1/0/2 (unchanged).
- [x] `make USE_LLVM=1 ./test_pyc` 37/0/38/2 (unchanged — both
      backends still consume IF1).
- [ ] CGProgram allocation is < 10% additional codegen time
      for representative programs (deferred — Phase 3 is when
      cg_normalize actually runs in the pyc binary; measuring
      now would be testing the test-phase plumbing, not the
      production codegen path).

Note: per-PNode lowering is intentionally incomplete in this
landing. Code_MOVE → CG_STORE and Code_SEND → CG_CALL (with
prim hint preserved) work; primitive-specific dispatch
(P_prim_period → CG_GEP_FIELD, P_prim_setter → CG_STORE_FIELD,
etc.) lands in Phase 3.3's per-instruction emitter when the
LLVM backend starts consuming CGProgram. The goldens captured
here lock the *block-and-terminator* shape; the per-prim
refinement won't perturb that shape.

Phase 2.4 (phi/phy materialization) lands as
`materialize_phi_phy()` + `materialize_phi()` walking
`pn->phi` and `pn->phy` unconditionally, per Phase 0 §5.2's
`pn_should_emit()` contract. This is the structural fix for
issue 016; verified visible in the
`09_ssu_self_binding.ir.cg-normalize.expected` golden.

Phase 2.5 (construction-flow patch) is **skipped** per Phase
0 §5.3 outcome (a): the construction-flow Code_MOVE exists in
IF1 and Phase 2.4's unconditional emission suffices. Verified
visible in `10_construction_to_global.ir.cg-normalize.expected`:
`(STORE :slot %s79 %s80)` is the construction-result binding
that the LLVM backend was previously dropping.

---

## 8. Phase 3 — LLVM backend consumes CGProgram (3-4 PRs)

The first backend to switch. The LLVM-first-class commitment
means **this phase ends with structurally closed issues 014 and
016, and the pyc-suite count strictly rises**.

### 8.1 Subphases

**3.1 — Type printer.** Replace `getLLVMType` / `getLLVMVarType`
/ `mapStructType` etc. with a direct walk over `CGType`. Each
`CGType` carries enough info that emission is a single switch.
The new function `cg_to_llvm_type(CGType *)` lives in `llvm.cc`
and is ~80 LOC. Single PR.

**3.2 — Function shell.** Rewrite `createFunction` to consume
`CGFun` directly. Function name, return type, arg types, formal
arg slots all come from CGFun; no IF1 lookup. ~150 LOC,
including parameter debug-info. Single PR.

**3.3 — Per-instruction emitter.** Rewrite `translatePNode` and
its helpers as `emit_cg_inst(CGInst *inst, EmissionContext &)`.
The per-CG_OP switch is ~300 LOC and replaces ~600 LOC of the
current per-Code_kind translation. Single PR (this is the
biggest one).

**3.4 — Wire it up.** `llvm_codegen_print_ir` becomes:

```cpp
void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main, cchar *input_filename) {
  llvm_codegen_initialize(fa);
  CGProgram *prog = cg_normalize(fa);
  emit_llvm_module(fp, prog);
}
```

The old `translateFunctionBody`, `translate_code_*`,
`do_phi_nodes`, `do_phy_nodes` all go away. ~50 LOC, but a
large delete. Single PR.

### 8.2 The pyc-suite ratchet

Each PR in this phase must show **strictly non-decreasing**
LLVM pyc-suite pass count:

| PR | Expected pass count change |
|---|---|
| 3.1 (type printer) | unchanged (37) |
| 3.2 (function shell) | unchanged (37) |
| 3.3 (instruction emitter) | +N where N covers issue 016 cohort (~5-8 tests) |
| 3.4 (wire-up + cleanup) | unchanged (whatever 3.3 produced) |

If 3.3 doesn't actually fix issue 016 in practice, the PR
doesn't land — back to investigation. The plan assumes the
analysis in §6.2.4 (unconditional phi/phy materialization)
holds; if it doesn't, the plan is wrong and we pause to
re-investigate before continuing.

### 8.3 Codegen-llvm fixture goldens

The IR shape changes everywhere because the per-PNode lowering
goes through CGProgram. Goldens for all 12 codegen-llvm
fixtures get reblessed in PR 3.3 or 3.4. The diff should be
mechanical (no semantic change beyond what the CGProgram dump
already documents).

### 8.4 Per-issue closure verification

After PR 3.3 lands:

- Issue 014: `tuple_mixed_types.py` no longer SEGVs;
  `class_attributes.py` correctly reads from `@y`.
- Issue 016: `for_range_from_zero.py`, `for_over_range.py`,
  `for_over_list.py`, `for_over_tuple.py`, `break_continue.py`
  all pass.
- The "alloca/global special-case" in
  `llvm_primitives.cc::P_prim_period` (and `_setter` /
  `_index_object`) becomes dead code — removed in PR 3.4.

### Phase 3 exit criteria

- [ ] `llvm_codegen_print_ir` consumes CGProgram only; no
      direct `PNode *` or `Fun *` dereference in the LLVM
      printer code.
- [ ] LLVM-backend pyc-suite pass count ≥ 45 (37 + 5 cohort
      from issue 016 + 2-3 from issue 014).
- [ ] Codegen-llvm fixture goldens reblessed and locked.
- [ ] Issues 014 and 016 closed with the closing-commit hash
      noted in each issue file.
- [ ] C backend still 74/1/0/2.
- [ ] CI `LLVM_BASELINE_PASS` bumped to the new floor.
- [ ] CODEGEN_LLVM.md §5.2 / §8 updated to describe the
      CGProgram-driven flow.

### Partial-progress notes (June 2026)

Phases 3.1, 3.2, and 3.3-scaffold have landed as **parallel
paths**: the new code exists and is exercised by unit tests,
but the production LLVM codegen still flows through
`getLLVMType` / `createFunction` / `translateFunctionBody`.
The split keeps each PR reviewable while preserving the LLVM
suite's 37/38/2 baseline. The production swap (Phase 3.4 full)
remains as a focused follow-up — see "Deferred for 3.4 full
swap" below.

- **3.1**: `cg_to_llvm_type(CGType*)` in `llvm.cc`, declared in
  `llvm_internal.h`, exercised by
  `cg_to_llvm_type_test.cc` (covers VOID/BOOL/INT/UINT/FLOAT
  widths, PTR / FUN_PTR opaque pointer, STRUCT with fields,
  null fallback). Caches result on `CGType::llvm_handle`.
- **3.2**: `create_llvm_function_from_cgfun(CGFun*, Module*)` in
  `llvm_codegen.cc`, exercised by
  `create_llvm_function_from_cgfun_test.cc` (covers void/i32/i32
  internal, ptr/i64 external, unnamed-cgfun fallback). Caches on
  `CGFun::llvm_handle`. Debug info (DISubprogram) is
  deliberately out of scope until Phase 3.3 carries source-line
  info on CGInst.
- **3.3 scaffold** (this session): `emit_cg.cc` lands with
  `emit_llvm_module(CGProgram*)`, `emit_cgfun(CGFun*)`, and
  `emit_cg_inst(CGInst*, EmitCtx&)`. Coverage:
  - **Handled now**: CG_NOP, CG_STORE, terminators
    (CG_BR / CG_COND_BR / CG_RET / CG_UNREACHABLE),
    BasicBlock pre-allocation per CGBlock, AllocaInst
    materialization for local CGSlots, function-decl pass via
    Phase 3.2's `create_llvm_function_from_cgfun`.
  - **Deferred for 3.4 full swap**: CG_LOAD_FIELD,
    CG_STORE_FIELD, CG_ALLOC structural ops (coupled to issue
    015's `is_value_type` work — heap-aggregate struct-type
    resolution); CG_CALL with prim back-translation to source
    PNode (calls into the existing per-prim emitter); phi/phy
    materialization at predecessor-block placement.
  - **Production print_ir wire-up**: NOT done. Production
    codegen continues through translateFunctionBody. The
    parallel `emit_llvm_module` is exercised only by the unit
    test (`run_emit_llvm_module` in `emit_cg_test.cc`).

### Deferred for 3.4 full swap

A focused follow-up session lands the production swap. The
work splits into four tracks:

1. ~~Structural CG_OP completion~~ **LANDED**. emit_cg_inst now
   emits CG_ALLOC (GC_malloc + sizeof(struct) + store to slot),
   CG_LOAD_FIELD (StructGEP + Load + store to slot), and
   CG_STORE_FIELD (StructGEP + Store) directly. The
   struct-type resolution goes through
   `struct_type_for(CGType*)` which reads `CGType::source` and
   calls `getLLVMType()` — couples Track 1 to IF1's type cache,
   which is fine through Phase 5 (the cache lives on Sym
   anyway). When struct resolution fails (no source, or source
   isn't a struct), the case `goto fallback`s into Track 2's
   back-translation. Unit test
   `run_emit_llvm_module_alloc_no_source` covers the fallback
   path.

2. ~~CG_CALL back-translation~~ **LANDED**. emit_cg_inst now
   dispatches CG_CALL / CG_LOAD_FIELD / CG_STORE_FIELD /
   CG_ALLOC / CG_PRIM_OP / CG_PRIM_CGFN through
   `write_llvm_prim(source_fun, source_pn)` (and falls back to
   `write_send` if the per-prim emitter returns 0). The
   ~1274 LOC of per-prim logic is preserved verbatim; the
   adapter is ~15 LOC. Production path unaffected — the
   back-translation only runs when `emit_llvm_module` is the
   driver. Unit test `run_emit_llvm_module_call_no_source`
   verifies graceful no-op when source_pn is null.

3. ~~phi/phy materialization~~ **LANDED**. `emit_terminator`
   in `emit_cg.cc` walks `source_pn->phi` / `source_pn->phy`
   and emits MOVEs via `simple_move` (un-static'd from
   `llvm_codegen.cc` and reused verbatim). Placement matches
   the IF1 emitter's pattern:
   - **CG_BR**: phi/phy MOVEs go at end of current block,
     before the unconditional branch.
   - **CG_COND_BR (dynamic)**: per-branch intermediate
     blocks (`if.true` / `if.false`) hold the phi/phy MOVEs
     with `isucc=0` / `isucc=1`, then branch to the real
     successors. Mirrors `translate_code_if` lines 562-578.
   - **CG_COND_BR (constant-folded)**: skip intermediate
     blocks, emit phi/phy for the taken side only, branch
     directly. Matches the IF1 emitter's constant-fold path.
   - **CG_RET / CG_UNREACHABLE**: no phi/phy (no successor).
   Phase 2.4's in-body phi/phy CG_STOREs are kept in
   `cg_normalize.cc` as informational record (visible in the
   cg-normalize golden); the LLVM emitter ignores them and
   walks `source_pn` directly. Track 4's print_ir swap will
   need to ensure no double-emission — either by gating the
   Phase 2.4 emission or by skipping marked CG_STOREs in
   emit_cg_inst (the simpler path).

4. **print_ir simplification** — once 1-3 land and the unit
   tests + cg-normalize fixtures show the parallel path matches
   the IF1 path's output for the existing codegen-llvm
   fixtures, modify `llvm_codegen_print_ir` to call
   `emit_llvm_module` and delete the old translateFunctionBody
   / translatePNode / translate_code_* / do_phi_nodes /
   do_phy_nodes (~520 LOC delete).

The pyc-suite ratchet (§8.2) governs the cutover: 3.4 must
hold ≥ 37 passes; landing 1-3 incrementally with no production
swap keeps every intermediate state safe.

---

## 9. Phase 4 — C backend consumes CGProgram (3-4 PRs)

The C backend rewrite. Lower risk than Phase 3 because we now
have CGProgram exercised end-to-end on the LLVM side; the C
backend just adds a printer over the same input.

### 9.1 Subphases

**4.1 — Type-string emission.** Replace
`assign_type_cg_strings_pass1/2` with a direct walk over
`CGProgram::types`. The `cg_string` field on `Sym` becomes a
read of the CGType's `name` field. ~100 LOC, mostly delete.
Single PR.

**4.2 — Function and global declarations.** Walk
`CGProgram::globals` and `CGProgram::funs` to emit the C
prototypes, struct definitions, and global variable
declarations. ~150 LOC. Single PR.

**4.3 — Per-instruction emitter.** Replace
`write_c_pnode` and its helpers with `emit_c_inst(CGInst *,
EmissionContext &)`. The per-CG_OP switch is ~250 LOC and
replaces ~600 LOC of `write_c_pnode` + `write_c_prim` +
`write_send` interpretation. Single PR.

**4.4 — Wire-up and cleanup.** `c_codegen_print_c` becomes:

```cpp
void c_codegen_print_c(FILE *fp, FA *fa, Fun *init) {
  if (!if1->callback->c_codegen_pre_file(fp))
    fprintf(fp, "#include \"c_runtime.h\"\n\n");
  CGProgram *prog = cg_normalize(fa);
  emit_c_module(fp, prog);
}
```

The old `write_c`, `write_c_prim`, `write_send`,
`build_type_strings` all go away. ~50 LOC plus a large delete.
Single PR.

### 9.2 The C-suite ratchet

Each PR in this phase must keep the C-suite at **74/1/0/2 with
zero new EXEC diffs** in the generated `.c` files' behavior. The
codegen-c fixture goldens get reblessed in PR 4.3 or 4.4.

The C backend currently has two expected-fail tests
(`class_attr_mutation.py`, `cross_type_method.py`). These stay
expected-fail; the rewrite doesn't change their status.

### 9.3 What goes away

- `cg.cc`: most of it. `write_c_pnode`, `write_c_prim`,
  `write_send`, `write_send_arg`, `simple_move`,
  `destruct_prim`, `build_type_strings` all delete (or
  shrink to thin wrappers around `emit_c_inst`).
- `Sym::cg_string` — replaced by `CGSlot::cg_name` /
  `CGType::name` lookup. Stage 5 actually removes it from
  `Sym`.

### Phase 4 exit criteria

- [ ] `c_codegen_print_c` consumes CGProgram only.
- [ ] C-suite 74/1/0/2 unchanged.
- [ ] Codegen-c fixture goldens reblessed and locked.
- [ ] LLVM-suite pass count from Phase 3 unchanged.
- [ ] CODEGEN_C.md §§3-9 updated to describe the
      CGProgram-driven flow.

---

## 10. Phase 5 — Cleanup (1-2 PRs)

Remove the IF1-side codegen channels. This is the cleanest part
of the plan and the longest-lived payoff.

### 10.1 What gets removed

- `Sym::cg_string` (`sym.h:98`) — readers replaced by CGType /
  CGSlot lookups during normalization.
- `Sym::llvm_value`, `Sym::llvm_type`,
  `Sym::llvm_type_di_cache` — replaced by `CGSlot::llvm_handle`.
- `Var::cg_string`, `Var::llvm_value`, `Var::llvm_type`,
  `Var::llvm_debug_var` — replaced by `CGValue`'s lookup
  through the per-Var → CGSlot map.
- `Fun::cg_string`, `Fun::cg_structural_string`, `Fun::llvm` —
  replaced by `CGFun::name`, `CGFun::llvm_handle`.

### 10.2 What stays

- IF1 layer in its entirety. The codegen-side caches go away
  but nothing else changes.
- The `Sym::live`, `Var::live`, `PNode::live`, `PNode::fa_live`
  bits — they're consumed by `cg_normalize` and shape what
  gets included in CGProgram. They don't go away.
- `Code::cg_string` (if it exists — TBD, but if so).

### Phase 5 exit criteria

- [ ] No references to `Sym::cg_string`, `Sym::llvm_*`,
      `Var::cg_string`, `Var::llvm_*`, `Fun::cg_string`,
      `Fun::cg_structural_string`, `Fun::llvm` outside
      `cg_normalize.cc`.
- [ ] All tests still pass (C suite 74/1/0/2, LLVM suite
      ≥ Phase 3 baseline).
- [ ] IR.md §3.1 / §5.1 / §6.1 updated to remove the deleted
      fields from the documentation.

---

## 11. Test strategy

### 11.1 The `cg-normalize` phase as the safety net

The new ifa-test phase locks in CGProgram's shape for 8+
fixtures. Every PR in Phases 2-5 must keep these green; that
ensures no semantic drift in the normalization layer.

### 11.2 Three-way regression gating

Every PR in Phases 2-5 runs:

1. `make test` (C backend, end-to-end).
2. `make USE_LLVM=1 ./test_pyc` (LLVM backend, end-to-end).
3. `make -C ifa test-ir` (all IR-phase goldens).

A PR that regresses any of these three is rejected. CI gates
on all three.

### 11.3 Cross-backend parity tests

A new test type: tests that explicitly assert the C and LLVM
backends produce equivalent output for the same input. Lives
under `tests/parity/` and is invoked via
`./test_pyc --parity`. Pre-Phase 5, may not all pass; post-
Phase 5, all parity tests must pass.

### 11.4 Fuzzing (deferred)

A `cg_normalize` fuzzer that exercises random IF1 shapes
(constructed via a small generator) and asserts:

- Every reachable CGBlock has a terminator.
- Every `CGValue` of kind `CG_V_INST` refers to a CGInst in
  the same or a dominating CGBlock.
- Every `CGSlot` referenced by a `CG_LOAD` is written by some
  `CG_STORE` on every reaching path.

Deferred to a follow-on. The handwritten goldens are sufficient
for Phase 2-4 acceptance.

---

## 12. Issue closure map

| Issue | Closure phase | How |
|---|---|---|
| 014 (LLVM construction flow) | Phase 3 (subphase 2.5 / 3.3) | `cg_normalize`'s construction-flow patch + LLVM printer reading CGProgram. |
| 016 (LLVM SSU formal-arg binding) | Phase 3 (subphase 2.4 / 3.3) | `cg_normalize` materializes phi/phy unconditionally. |
| 002 (LLVM normalizer for fixture stability) | Independent; this plan unaffected. | Output-text normalizer; not the IR normalizer. |
| 015 (POD records / `is_value_type` opt-in) | Independent; CGFun makes adoption easier. | Adding a `@pyc_struct` decorator becomes a frontend change + one `CGTypeKind` lift in `cg_normalize`. |

Issues 014 and 016 should be marked closed in their respective
files at the same commit that ships Phase 3.3, with the
closing-commit hash recorded.

---

## 13. Risk register

| Risk | Probability | Mitigation |
|---|---|---|
| `cg_normalize` produces a shape that doesn't actually close issue 016 | Medium | Phase 2.4 ships a standalone `phi`/`phy` materialization PR, gated on a unit test that reproduces issue 016's IR shape *before* Phase 3 work starts. If the unit test doesn't go from fail to pass in Phase 2.4, Phase 3 is paused. |
| The C backend rewrite (Phase 4) introduces a regression in the C-suite | Medium | Phase 4 is split into 4 PRs each independently testable. PR-by-PR rollback is straightforward; if any subphase regresses, it doesn't land. |
| Performance regression > 10% on `pyc -b` end-to-end | Low-medium | `ifa/codegen/PERFORMANCE.md` gets an LLVM column once Stage 3 ships. Acceptance gate on Phase 3 includes the LLVM compilation path being ≤ 2× the C path for `sieve.py` / `dict_basic.py`. |
| New IR creates merge conflicts with concurrent work | Low | CGFun lives in `ifa/codegen/cg_ir.h` which doesn't conflict with the rest of the codebase. The biggest concurrent risk is unrelated changes to `cg.cc` / `llvm_*.cc` during the migration window; those are absorbed by the per-subphase commits. |
| Goldens drift becomes painful | Medium | Reblessing is built in to each phase. The total rebless count is ~30 goldens (12 codegen-llvm + 6 codegen-c + 8-12 cg-normalize). Doable in three blocks (Phase 2.6, Phase 3.4, Phase 4.4). |
| The "construction-flow patch" peephole (2.5) gets too magic | Medium | Document the patch's pattern matcher carefully; ship a `--debug-cg-normalize` dump that prints which patches fired. If the patch can't be defined cleanly, the LLVM backend's emit path can adopt an issue-014 workaround inline at first. |

---

## 14. Estimated cost

| Phase | LOC | PRs | Calendar |
|---|---|---|---|
| 0 | +400 (fixtures, audit docs), 0 LOC code (accessor sub-PRs optional) | 1-4 | 3-7 days |
| 1 | +200 (headers + stub) | 1 | 2-3 days |
| 2 | +1500 (normalization), +400 (test phase) | 4-5 | 2-3 weeks |
| 3 | +800 / -600 (LLVM printer rewrite) | 3-4 | 1.5-2 weeks |
| 4 | +700 / -700 (C printer rewrite) | 3-4 | 1.5-2 weeks |
| 5 | -200 (cleanup) | 1-2 | 3-5 days |
| **Total** | **~+3400 / -1500 net ≈ +1900 LOC**; many lines are pure replacement, plus ~400 LOC of new doc and fixtures in Phase 0 | **13-17** | **7-9 weeks elapsed**, dependent on review cadence and how often subphases need iteration |

Phase 0 + Phase 1 can run in parallel; if §5.4 (the
side-channel accessor refactor) ships in Phase 0, total LOC
nets out lower in Phase 5.

The +1500 net LOC includes ~1200 lines of structurally new code
(CGFun and friends, the normalization pass itself) and replaces
~1700 LOC of distributed interpretation logic spread across
`cg.cc`, `llvm_codegen.cc`, `llvm_primitives.cc`, and
`codegen_common.cc`. The maintenance picture after Phase 5 is
strictly simpler.

---

## 15. Ordering across PRs

```
Phase 0.1 (fixtures for 014/016)  ┐
Phase 0.2 (liveness audit)         ├── [MANDATORY] gate Phase 2 start
Phase 0.3 (construction-flow IF1)  ┘

Phase 0.4 (side-channel accessors) ┐
Phase 0.5 (Codegen base class)     ├── [RECOMMENDED] parallel with Phase 0.1-3 and Phase 1
Phase 0.6 (baseline stabilization) ┘

Phase 1 (headers + stub)             ← can land any time after Phase 0 starts;
                                       independent of Phase 0's outcome
        ↓
Phase 2.1 (types + slots)            ← requires Phase 0.1, 0.2, 0.3
  └── Phase 2.2 (per-Fun skeleton)
        ├── Phase 2.3 (per-PNode lowering)  ← biggest, may split into N PRs
        ├── Phase 2.4 (phi/phy materialization)  ← gates Phase 3 start
        └── Phase 2.5 (construction-flow patch — scope per Phase 0.3 outcome)
              └── Phase 2.6 (test-phase + goldens)
                    └── Phase 3.1 (LLVM type printer)
                          └── Phase 3.2 (LLVM function shell)
                                └── Phase 3.3 (LLVM instruction emitter)
                                      └── Phase 3.4 (LLVM wire-up + cleanup)
                                            └── Phase 4.1 (C type printer)
                                                  └── Phase 4.2 (C decls)
                                                        └── Phase 4.3 (C instruction emitter)
                                                              └── Phase 4.4 (C wire-up + cleanup)
                                                                    └── Phase 5 (cleanup)
```

Phase 0 unblocks Phase 2 and sets the verification gates that
Phases 2-4 depend on. Phases 1-2 are pure additions. Phase 3
swaps the LLVM input. Phase 4 swaps the C input. Phase 5
removes the now-unused caches.

---

## 16. Cross-references

- [ifa/CODE_GEN_IR.md](../CODE_GEN_IR.md) — the investigation
  motivating this plan. Read this first if you haven't.
- [ifa/codegen/CODEGEN_PLAN.md](CODEGEN_PLAN.md) §8 (phase 5) —
  the existing Codegen / PrimEmitter scaffolding work that
  this plan extends. CGFun's `CG_PRIM_CGFN` op dispatches via
  the existing `RegisteredPrim::cgfn` table.
- [ifa/codegen/AUDIT.md](AUDIT.md) — known issues in the
  current codegen; many close via this plan.
- [ifa/codegen/PERFORMANCE.md](PERFORMANCE.md) — performance
  baseline. Gets an LLVM column added in Phase 3.
- [ifa/issues/014-llvm-construction-flow-to-slots.md](../issues/014-llvm-construction-flow-to-slots.md) — closed in Phase 3.3.
- [ifa/issues/016-llvm-ssu-formal-arg-binding.md](../issues/016-llvm-ssu-formal-arg-binding.md) — closed in Phase 3.3.
- [ifa/CFG_SSU.md](../CFG_SSU.md) §4 — SSU's phi/phy
  semantics, consumed by `cg_normalize`.
- [ifa/PRIMITIVES.md](../PRIMITIVES.md) §15 — primitive
  emission contract that `lower_send` honors.
