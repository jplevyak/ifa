# CG_IR_SEMANTICS.md — definitive operational rules for CG_IR_v2

This is the synthesis of Phases 0–4 of the CG_IR meta-plan. It
states what the IR *is*, what its invariants are, what the
backends are allowed to assume, and what the frontends are
required to produce. The textual form ([CG_IR_TEXT.md](CG_IR_TEXT.md))
is the source of truth for syntax; this doc is the source of
truth for semantics.

The document is short on purpose. CG_IR_v2 itself is small:
five named concepts, seven invariants, one structural lemma.
If this doc grows beyond a single sitting's read, the IR has
drifted.

## Provenance

| Source | Role |
|---|---|
| [CG_IR_META_PLAN.md](CG_IR_META_PLAN.md) | the plan that produced this work |
| [CG_IR_SURVEY.md](CG_IR_SURVEY.md) | Phase 0: IF1 field classification |
| [CG_IR_NEEDS.md](CG_IR_NEEDS.md) | Phase 1: 8-concept "must expose" list |
| [CG_IR_SKETCH.h](CG_IR_SKETCH.h) | Phase 2: full design sketch (target shape) |
| [CG_IR_TEXT.md](CG_IR_TEXT.md) | Phase 3: textual form + 13-test corpus |
| `cg_ir_v2*.cc/.h` | Phase 4: working implementation (15 commits) |
| `ifa/testing/cg_ir_v2_test.cc` | Phase 4: in-source corpus + emit verification |

This document supersedes the analysis text in `CG_IR_SKETCH.h`'s
comments. The sketch remains as design-history reference; this
doc is what the implementation honours and what the migration
plan targets.

## 1. The five named concepts

CG_IR_v2 exposes exactly five program-level concepts. Everything
else is an attribute on one of them.

1. **`CGv2Type`** — a value's shape. Predefined leaf types
   (int8/16/32/64, uint*, float32/64, bool, void, ptr, fun_ptr,
   sym, nil) plus user-declared structs (`CG2T_STRUCT`) and
   typed-ptrs (`CG2T_PTR` with `:element T`).

2. **`CGv2Value`** — a named SSA-style value with a `CGv2Type`
   and a `CGv2ValueScope` (one of LOCAL, FORMAL, GLOBAL,
   CONSTANT, FUN_REF, SYMBOL). Constants carry an immediate
   payload; fun_refs carry a target function name.

3. **`CGv2Inst`** — one operation. Has an op (`CGv2Op`), an
   optional binop sub-op (`CGv2BinSub`), rvals (`Vec<CGv2Value*>`),
   lvals, plus a few op-specific fields (`type_arg`, `field_idx`,
   `br_target`, `br_true`/`br_false`).

4. **`CGv2Block`** — a basic block: body (an ordered `Vec<CGv2Inst*>`),
   a single terminator, predecessor/successor lists, and an
   optional `phi_by_pred` (per-pred MOVE list, for SSA phi
   lowering through the alloca-store convention).

5. **`CGv2Fun`** — a function: signature, entry block, ordered
   block list, formals, locals, plus flags (`is_external`,
   `is_main`, `is_varargs`).

A **`CGv2Program`** is just the collection of these — types,
constants, globals, funs — with predefined-type accessors.

That's the entire surface. No `CGType` wrapping `Sym`, no
`CGSlot` wrapping `Var`, no IF1 escape hatches in the core IR.

## 2. The seven invariants

Frontends must produce IR satisfying these; backends are
allowed to assume them.

1. **Single textual definition**. Each `CGv2Value` is defined
   exactly once in its declaring scope's textual form. (Multiple
   *stores* to the same alloca slot are allowed for phi-target
   locals and globals; this is store-into-slot, not redefinition.)

2. **Types are known at every value**. Every `CGv2Value` has a
   `CGv2Type`. The backend never asks "what type is this?" —
   the answer is in front of it.

3. **Function-scoped value identity**. A `CGv2Value` is meaningful
   *within* its declaring function. The backend's per-fn emit
   state (`value_map`, `alloca_map`, `ptr_struct`) is the only
   place a `CGv2Value*` maps to an `llvm::Value*`. This is the
   structural lemma (§4).

4. **One terminator per block**. `CGv2Block::terminator` is set;
   it's CG2_RET, CG2_BR, CG2_COND_BR, or CG2_UNREACHABLE.

5. **Predecessor consistency**. If block B lists A in its preds,
   A's terminator must transfer control to B. (The implementation
   does not enforce this on parse — it's a frontend contract.)

6. **Phi MOVEs are by-predecessor, not by-block**.
   `Block::phi_by_pred` is the canonical form for SSA phis. A
   group `{ pred=P, moves=[(move %src => %dst)...] }` semantically
   places those MOVEs on the edge P→this-block. The backend
   emits them as stores on P's edge (LLVM phi-on-edge via
   alloca-store).

7. **Cross-function references are by name**. `CG2_CALL`
   resolves its callee through a fun_ref `CGv2Value` whose
   `target_name` is looked up in `TheModule` at emit time. There
   is no `CGv2Fun*` field on a call inst. This makes the
   declare-then-emit pre-pass (§5.2) load-bearing.

## 3. The textual form

The canonical form is S-expression. Grammar is in
[CG_IR_TEXT.md](CG_IR_TEXT.md); examples in `cg_ir_v2_test.cc`'s
test01–test13. The round-trip property holds:

- `parse(print(prog))` is semantically equal to `prog`
  (verified across the 13-test corpus via the `fingerprint()`
  helper in the test file).
- `print(parse(text))` is `text` modulo whitespace and comments.

The textual form is also the *test corpus*. Each landing commit
extended the parser + printer + emit + a synthetic test in
lockstep. The result: a 14-test in-source corpus (the 13 corpus
tests plus a synthesized "copy" test) covering every concept on
the must-expose list.

## 4. The structural lemma: issue 017's class

**Claim**: under v2, the cross-function instruction leak that
manifested as issue 017 cannot occur.

**Sketch**:

- The only function-scoped maps that carry `CGv2Value* →
  llvm::Value*` live in `EmitFunCtx` (`value_map`, `alloca_map`,
  `ptr_struct`). They are stack-allocated per `emit_fun()` call
  and discarded when it returns.

- The only program-scoped maps that cross function boundaries
  carry `CGv2Value* → llvm::GlobalVariable*` (`g_prog_ctx.global_map`)
  and `CGv2Value* → CGv2Type*` (`g_prog_ctx.global_ptr_struct`).
  Neither carries `llvm::Value*` of an *instruction*. A
  `GlobalVariable*` lives at module scope (so referencing it
  from any function is valid). A `CGv2Type*` is pure IR data.

- Therefore no `llvm::Instruction*` ever crosses a function
  boundary via the maps.

- The remaining channel for an `llvm::Value*` to cross is the
  `Builder` state itself. `Builder->SetInsertPoint(bb)` is
  called at the top of each emit_fun's block loop; nothing
  outside that loop emits instructions. The Builder reads its
  insert-point's parent function for every instruction it
  creates, so cross-function emission is structurally impossible.

**Verification**: test 09 (`run_cg_ir_v2_emit_test09`) is the
issue-017 trigger test. `verifyModule` is the load-bearing
assertion: under v1 it failed at this point, under v2 it
passes (commit `252e298`).

## 5. The frontend ↔ CG_IR contract

The frontend produces a `CGv2Program`. The backend consumes it.
Neither knows about the other's internals.

### 5.1 What the frontend produces

A frontend (currently: IF1's `cg-normalize` pass, in v1's
existing `cg_normalize.cc`; eventually: any frontend) must
produce a `CGv2Program` where:

- All types referenced by values are declared (either
  predefined or in `prog->types`).
- All values referenced by insts are declared in the value's
  scope (per-fn `values_by_name` for LOCAL/FORMAL/FUN_REF,
  per-prog `constants`/`globals` for CONSTANT/GLOBAL).
- All seven invariants above hold.
- Each function's blocks are in CFG order with entry first.

The frontend is free to do whatever it wants internally —
flow analysis, SSU, type inference, constant folding. The
contract is the *output*: a CGv2Program with the above
properties. The IR is a *contract*, not a *workflow*.

### 5.2 What the backend assumes

Backends (`cg_ir_v2_emit_llvm.cc`; eventually a C backend, an
LLVM-bitcode-only backend, etc.) follow this pattern:

1. **Pre-declare globals.** For each `gv` with `scope==GLOBAL`,
   create the corresponding `llvm::GlobalVariable` (or equivalent
   target-language artifact).

2. **Pre-declare functions.** For each `cf` in `prog->funs`,
   create the corresponding `llvm::Function` declaration. This
   is the *load-bearing* step for invariant 7 — without it,
   forward `CG2_CALL` would fail to resolve.

3. **Emit each function's body.** Set up an `EmitFunCtx`. Bind
   formals into `value_map` from the LLVM function's args.
   Alloca every phi-target local in entry. Emit each block:
   body insts first, then per-edge phi MOVEs (from succs whose
   phi_by_pred lists this block as a pred), then the
   terminator.

4. **Run `verifyModule`.** This is *also* part of the contract —
   if it fails, the backend has produced malformed IR, which
   means either the frontend violated an invariant or the
   backend has a bug. (For the v2 corpus, it doesn't fail.)

### 5.3 What CG_IR explicitly does NOT model

In keeping with "tight scope":

- **No FA / lattice / types-as-sets-of-creation-sets.** Once
  the frontend has done flow analysis, the result is a
  resolved type on every value. The backend never sees `AType`
  or `AVar`.

- **No SSU rename machinery.** The frontend resolves SSU; the
  IR's values are the post-rename names. Issue 016's SSU
  formal-arg binding is, in this model, just a regular
  per-fn `(value %x :scope formal)` — no special case.

  IFA does **SSU**, not SSA: each *use* of a source variable
  gets its own Var post-pass (not just each definition), which
  lets conditionals narrow types per branch. CG_IR_v2 honours
  this transparently — each SSU-renamed Var becomes its own
  `CGv2Value` with its own `:type`. The disambiguator is
  `CGv2Value*` pointer identity, **not** the textual name
  string. Two SSU values originating from the same source `x`
  get two distinct `CGv2Value` instances in `prog->{values,
  globals,constants}` (whichever scope applies) and two
  separate `value_map` slots in `EmitFunCtx`.

  Branch-narrowed types appear as different `:type`
  annotations on different `(value ...)` decls. Type-widening
  at a join point happens either through `:phi_by_pred` (one
  MOVE per pred narrows-up to the join's widened type) or via
  a plain `CG2_MOVE` when only one pred reaches the merge.

  This is exercised end-to-end by `run_cg_ir_v2_emit_test_ssu`
  (the `ssu_narrowing` corpus test): an `abs(x)` function
  where the SSU pass would rename `x` to `x_nonneg` in the
  ≥0 branch and `x_neg` in the <0 branch, joined to
  `%abs_val` at the merge. `verifyModule` is the load-bearing
  assertion — if `value_map` collided on shared source
  names, the resulting dominance / phi-incoming violations
  would surface there.

- **No optimization framework.** CG_IR is the *emission*
  layer. Optimization happens before (IF1-level transforms
  like inlining, dead code) or after (LLVM's pass manager).
  CG_IR doesn't grow a `cse` or `licm` pass.

- **No `Sym*` / `Var*` / `PNode*` back-references.** These
  belonged to v1's `CGType`/`CGSlot`/`CGValue` and were
  decorative — the audit ([CG_IR_v2.md](CG_IR_v2.md)) showed
  the LLVM emitter never reads them. They are absent from
  CG_IR_v2.

- **No per-instruction source location.** Debug info will
  ship as a separate annotation layer when needed; it's not
  in the core data model.

- **No `Prim*` escape hatch yet.** Test 11 (`CG_PRIM`) is
  deferred. The escape hatch will be added when the runtime's
  prim table has a mockable interface — it's an open design
  question, not a fait accompli.

## 6. Migration plan: retiring v1's `cg_ir.h`

The goal is to retire `cg_ir.h` / `cg_ir.cc` / `emit_cg.cc` /
`cg_normalize.cc`'s v1 surface in favour of v2, without
dropping the pyc-suite below its current 74/0 + 74/0 baselines
(C + LLVM).

### 6.1 Phase A — Production-shape parity (the gap)

Before any retirement, v2's emitter must reach feature parity
with v1's `translateFunctionBody` (the production LLVM emit
path). The 14-test corpus covers the *concepts* but not the
full IF1 → LLVM lowering. The gap is roughly:

- **CG_PRIM with the IF1-prim escape hatch** (test 11's deferred
  concept). The v1 `emit_cg.cc` handles ~80 primitives via a
  big switch. v2 needs the same coverage, ideally as a per-prim
  emitter registered against the prim table. **Effort**: a
  per-prim-emitter day, plus a port from `emit_cg.cc`'s switch.

- **Variadic / external functions** (existing in v2 via the
  declare_fun pre-pass; needs validation against the actual IF1
  prim signatures).

- **Closures** (Phase 5 subsidiary question SQ3 from the
  meta-plan — currently unresolved). Decision-blocked.

- **Sum-type discriminator** (SQ2 — also unresolved).

### 6.2 Phase B — cg-normalize emits CGv2Program

Today, `cg_normalize.cc` populates v1's CGProgram. To use v2 in
production, `cg_normalize` needs a sibling path that populates a
CGv2Program. This can ship behind a flag (`IFA_LLVM_V2=1`) so
the existing path stays green.

**Pass criterion**: with `IFA_LLVM_V2=1`, the pyc-suite must
reach the same 74/0 baseline as the v1 path.

### 6.3 Phase C — Default to v2, keep v1 as escape hatch

Once the flagged path is green for one calendar week of CI runs,
flip the default. Keep `IFA_LLVM_V2=0` as an emergency rollback
switch for one more week.

### 6.4 Phase D — Delete v1

After two weeks at the new default with no rollbacks, delete:

- `cg_ir.h`, `cg_ir.cc`
- `emit_cg.cc`'s v1 entry points
- The v1 path in `cg_normalize.cc`
- The `IFA_LLVM_V2` flag wiring
- Any tests that hand-construct v1 `CGProgram` (replace with v2
  textual form via `cg_v2_parse`).

Rename `CGv2*` → `CG*` in a single sweep. The naming was a
migration aid; once v1 is gone, it's just noise.

### 6.5 What does NOT migrate

The C backend (`cg.cc`) is staying. Per the project owner's
constraint #1, the C backend remains until LLVM achieves parity.
After Phase D, the C backend reads v1's CGProgram one of two
ways:

- Option 1 (simpler): keep a thin v1 → v2 adapter so cg.cc
  reads v2 too.
- Option 2 (riskier): port cg.cc to v2 directly.

Option 1 is the recommendation until the LLVM backend has been
"the default" for ~3 months and the C backend is genuinely a
fallback.

## 7. What this doc deliberately omits

- **A full grammar reference**. That's [CG_IR_TEXT.md](CG_IR_TEXT.md).
- **A field-by-field class reference**. That's `cg_ir_v2.h`.
- **A history of revert attempts**. That's [CG_IR_META_PLAN.md](CG_IR_META_PLAN.md).
- **The 8-concept "must expose" derivation**. That's [CG_IR_NEEDS.md](CG_IR_NEEDS.md).
- **The IF1 field audit**. That's [CG_IR_SURVEY.md](CG_IR_SURVEY.md).

If you want to argue with the model, this doc is the one to
read. If you want to use the IR, read the test corpus.

## 8. Pass criterion for this document

The project owner reads it and either:

- agrees with the model as stated — this becomes the contract,
  and the migration plan (§6) becomes the work order; or
- flags a specific invariant, concept, or migration step as
  wrong — that becomes a concrete revision, not a "rewrite the
  IR" pivot.

If neither holds — if the disagreement is at a level that
requires re-deriving the model — Phase 5 has revealed something
Phase 2's sketch missed, and the meta-plan's "more pain points
are a certainty without a very principled process" answer is
again the right framing. We'd back up to Phase 2.
