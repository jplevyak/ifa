# CODEGEN_LLVM_CONTRACT

The contract between IF1 (FA's converged output) and the
LLVM backend.  Phase 1 of the CODEGEN_PLAN cleanup —
make implicit semantics explicit so future codegen
changes can be reviewed against a spec rather than against
the current implementation.

Companion to:
- [CODEGEN_LLVM.md](CODEGEN_LLVM.md) — the WHAT (mostly).
- [CODEGEN_C.md](CODEGEN_C.md) — sister contract for the
  C emitter (one to be written).
- [codegen/CG_IR_PARITY_PLAN.md](codegen/CG_IR_PARITY_PLAN.md)
  — where the LLVM backend currently lags vs C.

This document is the **specification**.  Behavior in
`llvm.cc`, `cg_normalize_v2.cc`, and
`cg_ir_v2_emit_llvm.cc` that diverges from this is a
**bug in the implementation**, not a feature.  If the
divergence is intentional, this document gets updated
first.

---

## 1. Scope

The LLVM backend turns a converged `FA *` into an
executable.  It does NOT:

- Run FA itself (the caller has).
- Run optimization passes (`ifa_optimize` is the caller's
  responsibility before this backend runs).
- Validate the program at the language level.

It DOES:

- Translate IF1 → CGv2Program (via `cg_normalize_v2`).
- Emit LLVM IR from CGv2Program (via
  `cg_v2_emit_llvm_module`).
- Synthesize a C-ABI `main()` that calls IF1's main_fun.
- Verify the produced module (`llvm::verifyModule`).
- Write a `.ll` text file.
- Spawn clang to link against `libpyc_runtime.a` and
  Boehm GC.

Entry point: `llvm_codegen_write_ir(fa, main_fun, fn)` +
`llvm_codegen_compile(fn)`, both in `llvm.cc`.  Despite
the file name, **the actual IR emission goes through the
v2 path** (the historical "v1 direct emitter" was retired
in issue 014; nothing in this contract refers to v1).

## 2. Input contract — what FA must hand us

### 2.1 Required FA state

When `llvm_codegen_write_ir` is called, the FA must
satisfy:

- **Convergence**: `FA::analyze` has returned without
  type violations (or the caller has chosen to proceed
  through `fruntime_errors`).
- **Liveness marked**: `mark_live_code` and
  `mark_live_funs` have run.  AVars and PNodes carry
  correct `live` / `fa_live` bits.
- **Concretization**: `concretize_types` has run for
  every live Fun.  Each `Var::type` is a concrete Sym
  with a meaningful `cg_string` after pass1/pass2
  type-string assignment (`codegen_common.cc`).
- **Call graph populated**: `Fun::calls.get(pnode)`
  returns the resolved target Funs for every SEND
  PNode.  Empty vector or `nullptr` means "no resolved
  target" — handled per §3.4 (matching-function-not-
  found assert at runtime).
- **`f->cg_string` set** for every live Fun (per
  `assign_fun_cg_strings`).

If any of these are violated, behavior is undefined.
The emitter does NOT defensively check — that's the FA's
contract.

### 2.2 Recognized IF1 PNode kinds

The emitter handles exactly the following `Code::kind`
values per PNode:

| PNode kind  | Translation outcome |
|-------------|---------------------|
| `Code_SEND` | one or more LLVM instructions, depending on the `Prim` it dispatches to (see §3) |
| `Code_MOVE` | `CG2_MOVE`, which compiles to LLVM `bitcast` or no-op when types match |
| `Code_LABEL`| `CGv2Block` boundary; LLVM `BasicBlock` |
| `Code_GOTO` | `CG2_BR`; LLVM unconditional branch |
| `Code_IF`   | `CG2_COND_BR`; LLVM `br i1 %cond, label, label` |

`Code_SUB`, `Code_SEQ`, `Code_CONC`, `Code_NOP` are
structural and not directly emitted — they get
flattened during CG IR normalization.  Any other kind
is unrecognized and reaches an `assert(!"case")` in
emission.

### 2.3 Recognized primitives at SEND sites

The SEND emitter dispatches by `PNode::prim->index`.
Recognized prims and their LLVM lowerings:

| Primitive | LLVM lowering |
|-----------|---------------|
| `P_prim_reply` | `ret <value>` |
| `P_prim_period` (getter) | GEP + load through `_CG_ps<N>` struct |
| `P_prim_setter` | GEP + store; elide if field is dead |
| `P_prim_make` (tuple/list) | `_CG_prim_tuple` / `_CG_prim_list` runtime call, per-slot stores |
| `P_prim_new` | `_CG_prim_new` (GC_MALLOC) |
| `P_prim_clone` / `P_prim_clone_vector` | `_CG_prim_clone_dst` runtime call |
| `P_prim_isinstance(x, sym_nil_type)` | `icmp eq ptr %x, null` |
| `P_prim_is` (issue 028 step 4) | **TODO: LLVM port pending.**  In C: `(void*)a == (void*)b`. |
| `P_prim_add` / `_subtract` / `_mul` / etc | `CG2_BINOP` → LLVM `add`/`sub`/`mul` |
| `P_prim_assign` (ref deref) | load + store through `_CG_ref` |
| `P_prim_destruct` | sequence of per-field assignments |
| `P_prim_coerce` | `CG2_CAST` (bitcast or trunc/sext per src/dst kinds) |
| `P_prim_primitive` | `CG2_C_CALL` to the named runtime helper (`_CG_prim_<name>`) |
| `P_prim_typeof`, `P_prim_typeof_element`, `P_prim_sizeof`, `P_prim_sizeof_element` | constant fold at emit time |
| Any other `Prim` not in this list | unrecognized; falls through to "matching function not found" |

The C backend has a parallel switch in `cg.cc:write_c_prim`.
**Any new primitive added to one side must land on both.**

### 2.4 Recognized type kinds

| Sym `type_kind` | LLVM type |
|-----------------|-----------|
| `Type_RECORD` (with `has.n > 0`) | `%struct._CG_s<id>` (pointer-shaped at use sites) |
| `Type_RECORD` (empty) | `void *` |
| `Type_FUN` (with `s->fun`) | function pointer to `s->fun`'s LLVM signature |
| `Type_FUN` (closure, no `s->fun`) | `%struct._CG_s<id>` for the closure struct |
| numeric kinds (int8/16/32/64, float/double, bool) | matching LLVM integer or float type |
| symbol type | `i32` (the symbol's id) |
| nil_type | `void *` (= NULL) |
| `Type_SUM` | resolved via `assign_type_cg_strings_pass2`: 2-element `None | T` → T's LLVM type.  Multi-class SUMs collapse to `void *` (`_CG_any`) and require explicit cast at use sites. |
| string | runtime `_CG_String` struct, opaque to the emitter |

## 3. Output contract — what we promise to emit

### 3.1 Module structure

Every emitted module contains:

- One LLVM `Function` per live IF1 `Fun` with its
  cg_string as the function name.
- One LLVM global per IF1 global Var that survives
  liveness (e.g. class prototypes, default-arg
  storage).
- One synthesized `main` function with C ABI (`i32(void)`)
  that calls IF1's `main_fun` (which receives the global
  initializers) and returns 0.

### 3.2 Per-function structure

For each live Fun:

- LLVM function signature derived from the Fun's formal
  positions and their `cg_string`s.
- Function body = one LLVM BasicBlock per `CGv2Block`,
  per IF1 LABEL/GOTO partition.
- Each block ends with exactly one terminator
  (`br`, `ret`, or `unreachable`).
- Each instruction in the block corresponds to either
  one `CGv2Inst` or one phi/phy resolution.

### 3.3 Verification

After every function is emitted, the module is run
through `llvm::verifyModule`.  Failure aborts with the
verifier's error message; the partially-written `.ll` is
preserved so the failing module is inspectable.

### 3.4 Runtime failures the emitter delegates to

The emitter does NOT crash on these cases — it emits
code that does:

- **Unresolved dispatch** (`fns->n != 1` for a SEND with
  no resolved target): emit a call to `_CG_assert_fail`
  or equivalent that prints "matching function not
  found".  Today: not yet wired; emitter may just emit
  an unreachable.  See issue 029 for the indirect-
  dispatch fix.
- **Type violation surviving FA**: if `fruntime_errors`
  is on, runtime checks emitted; otherwise emission
  proceeds with a best-effort cast.

## 4. Invariants the emitter assumes (and verifies where cheap)

### 4.1 Liveness consistency

For any PNode `p` and live Var `v` referenced by
`p->rvals` or `p->lvals`, `v` is also live.  Codegen
emits no instructions for dead PNodes.  Tests rely on
this — e.g. issue 026 dead-field elision.

### 4.2 cg_string totality

Every live Sym, Var, and Fun has a non-null `cg_string`
after `assign_*_cg_strings`.  An emitted instruction
referencing a Sym with `cg_string == nullptr` is a bug
upstream — verifier will catch the resulting
ill-formed module.

### 4.3 Single-target call sites

`f->calls.get(p)` is either:
- nullptr / empty → unresolved (emit runtime trap).
- One Fun → emit direct call.
- Multiple Funs → **today: unsupported, emits trap.**
  Issue 029 sketches fat-pointer / indirect dispatch.

### 4.4 Field-slot stability

For a class hierarchy, every subclass's instance
struct places inherited methods at the **same field
index** as the base class.  This is required for the
indirect-dispatch fix (issue 029) but is also relied
on implicitly by the C backend's
`resolve_union_receiver`.  Today: holds for the test
suite; not enforced by FA.

### 4.5 Convention: GC pointer semantics

- All class-instance pointers are GC-allocated.  Codegen
  does not insert GC roots — Boehm GC handles
  conservative root tracking.
- All `_CG_ref` (ref) types are GC-allocated wrappers.
- Stack-allocated locals exist only for primitive
  values and for closure activation records that don't
  escape (post-`mark_live_escape` analysis, if enabled
  via `ifa_escape_in_fa`).

## 5. Known gaps (today, June 2026)

LLVM-vs-C parity at 92/7.  The 7 LLVM failures all stem
from C-side fixes that haven't been ported:

| C-side fix | Where in C | LLVM port needed |
|-----------|-----------|-----------------|
| `prim_is` codegen | `cg.cc:709` | Add `P_prim_is` case in v2 emit or treat as CG2_BINOP-EQ |
| Voidish-arg cast at SENDs | `cg.cc:write_send_arg` | Add bitcast on union-typed args to CG2_CALL |
| `resolve_union_receiver` | `cg.cc` helper | Apply at the GEP base type for union-receiver getters/setters |

Two related FA fixes are shared (`concretize_var_list_type`
and the SSU phi cascade) and don't need an LLVM port.

## 6. How to extend this contract

When adding a new codegen feature:

1. Update §2.3 (primitives) or §2.4 (type kinds) — write
   the rule BEFORE implementing.
2. Implement in C backend (`cg.cc`) — gcc validates the
   shape.
3. Implement in LLVM backend.
4. Run both suites: `./test_pyc` and
   `PYC_FLAGS=-b ./test_pyc`.  Both must pass.
5. If §4 (invariants) needs a new entry, add it.

The doc is the single source of truth for "what the
LLVM backend does."  Anything in the code that doesn't
match this doc is a bug to be fixed.

## 7. Glossary

- **CG IR v2**: the intermediate representation in
  `cg_ir_v2.h` (CGv2Program, CGv2Type, CGv2Value,
  CGv2Inst, CGv2Block, CGv2Fun).
- **cg_normalize_v2**: IF1 → CGv2Program translator
  in `cg_normalize_v2.cc`.
- **cg_v2_emit_llvm_module**: CGv2Program → LLVM IR
  emitter in `cg_ir_v2_emit_llvm.cc`.
- **cg_string**: a Sym's C type identifier (e.g.
  `_CG_ps3461`) used by both backends to name
  emitted types.  Set during pass1/pass2 in
  `codegen_common.cc`.

---

*Phase 1 of the LLVM backend cleanup.  Phases 2+
(internal seam naming, diagnostic instrumentation, CI
parity enforcement) layer on top of this contract.*
