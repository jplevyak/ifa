# CG_IR_v2 — assessment after the LLVM swap

Written: 2026-06-13.

This is an **assessment**, not a design. It surveys what the C
backend (`cg.cc`) actually uses, compares against CG_IR's
current surface (cg_ir.h), and proposes a slimmer model.

## Method

Audited every field access and dispatch in `cg.cc` (~918
LOC). Tabulated against the CGType / CGSlot / CGValue /
CGInst / CGBlock / CGFun / CGProgram class definitions in
`cg_ir.h`. Cross-referenced with what `emit_cg.cc` and
`cg_normalize.cc` actually produce and consume.

## What the C backend actually depends on

Three categories.

### 1. Name strings (the load-bearing mutation)

The C backend writes once during `build_type_strings`, then
reads in every emission site:

| Field | Setter sites | Reader sites | Purpose |
|---|---|---|---|
| `Sym::cg_string` | 1 (assign_type_cg_strings_pass1) | 127 | type name (`_CG_psN`, `_CG_int64`) |
| `Var::cg_string` | 11 | 127 | variable name (`t0`, `g3`) |
| `Fun::cg_string` | 1 (assign_fun_cg_strings) | many | function name (`_CG_f_105_6`) |
| `Fun::cg_structural_string` | 1 | 3 | function-pointer typedef (`_CG_pf0`) |

These four strings are the C backend's **entire model of
emission identity**. No SSA cache, no per-function scoping.

### 2. Direct IF1 tree walk

The C backend dispatches by `Code::kind` and walks
`PNode::cfg_succ` recursively (`write_c_pnode`):

- `Code_LABEL` → `L%d:;\n`
- `Code_MOVE` → `simple_move(lvals[i], rvals[i])`
- `Code_SEND` → `write_c_prim(...)` or `write_send(...)`
- `Code_IF` → `if (cond) { ... } else { ... }`
- `Code_GOTO` → `goto L%d;`

phi/phy materialization runs **outside the live gate** (lines
604–649): `do_phi_nodes(n, isucc)` and `do_phy_nodes(n,
isucc)` are called for every PNode regardless of liveness.
This is what makes issue 016 not exist on the C backend.

### 3. Per-primitive dispatch

The C backend handles 16 `P_prim_*` cases:

```
P_prim_apply, P_prim_assign, P_prim_clone, P_prim_clone_vector,
P_prim_destruct, P_prim_index_object, P_prim_len, P_prim_make,
P_prim_new, P_prim_period, P_prim_primitive, P_prim_reply,
P_prim_set_index_object, P_prim_setter, P_prim_sizeof,
P_prim_sizeof_element
```

Arithmetic (`P_prim_add`, `P_prim_less`, etc.) is **not in
this list** — the C backend leaves arithmetic to the C
compiler via operator overloading on `_CG_int64` and friends.
The LLVM backend handles 30 cases because it can't rely on
overloading and emits `Add`/`ICmpSLT`/`FMul`/etc. directly.

This asymmetry is structural and won't go away.

## What CG_IR currently claims

From `cg_ir.h`:

```
CGType     (kind, bits, fields, field_names, element, source, name, llvm_handle)
CGSlot     (kind, type, name, cg_name, id, source_sym, source_var, imm, llvm_handle)
CGValue    (kind, inst, slot, fun, imm, type)
CGInst     (op, result_type, rvals, slot, field_idx, prim, prim_name,
            src_line, src_file, source_pn, is_phi_phy)
CGBlock    (body, terminator, preds, succs, id, label, source_pn)
CGFun      (source_fun, name, return_type, arg_types, formal_arg_slots,
            blocks, entry, locals, is_external, is_main, llvm_handle)
CGProgram  (funs, main_fun, globals, types, fun_map, sym_to_slot, sym_to_type,
            frontend)
```

## Where CG_IR is over-engineered

Six concrete cases.

### A. CGType is a thin wrapper around `Sym *source`

`emit_cg.cc::struct_type_for(CGType *t)` is:

```cpp
if (!t || !t->source) return nullptr;
llvm::Type *st = getLLVMType(t->source);  // ← all the work happens here
```

Every direct emission that uses `CGType` ends up calling
`getLLVMType(source)`, which is the existing IF1-Sym-keyed
type cache. CGType's own fields (`kind`, `bits`,
`field_names`) are **unused by the emitter**. They show up
only in the cg-normalize golden, where they're documentation,
not load-bearing.

`cg_to_llvm_type(CGType*)` (Phase 3.1) is exercised only by
unit tests. The production CG_ALLOC / CG_LOAD_FIELD paths
fall back to `getLLVMType(source)` because that's what the
existing struct-field-index machinery is built on.

**Cost**: CGType keeps a parallel data model alive that the
emitter doesn't read.

**Win if we drop it**: ~250 LOC, plus the entire
`cg_to_llvm_type` test scaffolding, plus the
`sym_to_type` map in CGProgram.

### B. CGSlot is a thin wrapper around `Var *source_var`

`emit_cg.cc::slot_pointer(CGSlot *s)`:

```cpp
if (!s) return nullptr;
if (s->llvm_handle) return s->llvm_handle;            // ← never set in production
if (s->source_var) return cg_get_llvm_value(s->source_var);  // ← always taken
if (s->source_sym) return cg_get_llvm_value(s->source_sym);
return nullptr;
```

`s->llvm_handle` is never set by `cg_normalize` — it's set
only when `emit_cgfun`'s `materialize_local_slots` runs for
the standalone unit-test path. In production,
`allocate_locals` (the IF1-side helper) populated the
Var-keyed cache before `emit_cgfun_body` runs, so
`slot_pointer` always takes the source_var fallback.

CGSlot's other fields (`kind`, `name`, `cg_name`, `id`,
`imm`) are inspected only by the cg-normalize printer.

**Cost**: CGSlot duplicates state that lives on Var.

**Win if we drop it**: ~80 LOC, plus the `sym_to_slot` map
and `formal_arg_slots` on CGFun.

### C. CGValue is a tagged union of "what `resolve_value`
should do"

```cpp
case CG_V_SLOT:      return load_via_slot(cv->slot);
case CG_V_FUN:       return cv->fun ? cv->fun->llvm_handle : nullptr;
case CG_V_IMMEDIATE: return nullptr;  // Phase 4 work
case CG_V_INST:      return nullptr;  // unused
case CG_V_NONE:      return nullptr;
```

CG_V_INST is unused (no inst → CGValue path exists).
CG_V_IMMEDIATE is also unused. CG_V_NONE is a sentinel.
CG_V_FUN is one indirection (return llvm_handle directly).
CG_V_SLOT is the only working case.

**Cost**: CGValue is a 5-way union with 4 dead arms.

**Win if we drop it**: ~60 LOC. CGInst's `rvals` becomes
`Vec<CGSlot *>` directly (or `Vec<Var *>` if we also drop
CGSlot per case B).

### D. CGProgram::sym_to_type / sym_to_slot are unused at
emission

The maps are built by `build_cgtypes` / `build_cgslots` and
consulted only by the cg-normalize printer. The LLVM emitter
reaches each type via `inst->slot->type->source` →
`getLLVMType(source)`. Never via the map.

**Cost**: Two Map<Sym*, ...> with no consumer.

**Win if we drop them**: ~30 LOC + the build-time pass.

### E. CGInst::is_phi_phy is a marker for double-emission

Phase 2.4 emits phi/phy MOVEs as in-body CG_STOREs. Track 3's
emit_terminator re-emits them via simple_move. The
`is_phi_phy` bit tells emit_cg_inst to skip the body STOREs.

If Phase 2.4 **didn't emit the body STOREs in the first
place**, the bit isn't needed. The cg-normalize golden loses
visual proof that phi/phy MOVEs exist; we'd surface them as
metadata on the terminator instead (e.g., a Vec<PNode*> on
CGBlock for "phi MOVEs to materialize before branching").

**Cost**: A bit + a marker pass + reader logic. Minor.

**Win if we drop it**: ~15 LOC + clearer semantics
(materialization happens in exactly one place).

### F. CGInst::source_pn is mandatory for production but
called "for debugging" in cg_ir.h

```cpp
PNode *source_pn;              // back-ref for debugging
```

Every back-translation needs source_pn:

```cpp
PNode *spn = inst->source_pn;
Fun *sf = ctx.cf ? ctx.cf->source_fun : nullptr;
if (!spn || !sf) break;
write_llvm_prim(sf, spn);  // ← all per-prim emission goes through this
```

Without source_pn, emit_cg_inst can't dispatch to the
existing per-prim emitters in `llvm_primitives.cc`. Track 2
(CG_CALL back-translation) is the load-bearing path for ~95%
of CGInsts in real programs.

So source_pn is **not a debugging aid** — it's the actual
dispatch key. The cg_ir.h comment is misleading.

This is fine if we accept that CG_IR is "scaffold over IF1,"
not "independent IR." But it should say so.

## What CG_IR actually contributes (the part to keep)

Two things:

### 1. Block structure

`CGBlock` + `cg_normalize`'s pass-1 boundary detection (LABEL
PNodes → blocks) + emit_cgfun_body's per-block iteration.
This is **genuinely useful** because it gives the LLVM emitter
a stable per-basic-block context, which the IF1 worklist
order didn't guarantee.

### 2. Terminator metadata

`CGBlock::terminator` (with `succs` populated) gives the
emitter explicit branch targets. Without this, emit_terminator
would have to re-derive successors from `source_pn->cfg_succ`
(which it does anyway as a fallback for phi/phy).

That's it. Those two pieces solved CG_IR_PLAN's Phase 2.4 +
3.3 in a structurally clean way.

## Where CG_IR fell short (the missing piece)

CG_IR _doesn't_ solve the LLVM backend's actual hard problem:

**Per-Var llvm::Value cache is program-scoped, but llvm::Value
identity is function-scoped.**

This is issue 017 / the cross-function instruction leak / the
construction-flow `ptr undef`. The IF1 path's `getLLVMValue`
has a `scope_mismatch` check (llvm.cc:1232) that clears stale
cache entries when the cached instruction's parent function
differs from the current one. The fallback path can produce
`undef` when constant-recovery can't reconstitute the value.

A function-scoped cache would prevent both the leak and the
undef. CG_IR _could_ have provided this via a per-CGFun
`Map<Var *, llvm::Value *>` and a `resolve_value` API that
threads through CGFun, but instead it added CGType / CGSlot /
CGValue (a parallel type model) and kept the per-Var cache
globally on Var.

**This was the architecture's biggest miss.**

## Proposed CG_IR_v2 (sketch)

Drop A through E from above. Keep block structure + terminator
metadata. **Add per-CGFun value cache.**

```cpp
class CGBlock : public gc {
 public:
  Vec<PNode *> body;            // PNodes lowered in order, in this block
  PNode *terminator;             // the closing PNode (LABEL/GOTO/IF/SEND@reply)
  Vec<PNode *> phi;              // phi MOVE PNodes, materialized at term-emit
  Vec<PNode *> phy;              // phy MOVE PNodes, materialized at term-emit
  Vec<CGBlock *> preds, succs;
  int id;
  cchar *label;
  llvm::BasicBlock *llvm_handle; // cached for cross-block branches
};

class CGFun : public gc {
 public:
  Fun *source_fun;
  Vec<CGBlock *> blocks;
  CGBlock *entry;
  llvm::Function *llvm_handle;

  // The new piece: per-function value cache. Replaces Var::llvm_value
  // as the canonical lookup. emit_cg's getLLVMValue first checks this
  // map; cache miss triggers the constant-recovery path. Population
  // happens via setLLVMValue inside the emitter, scoped to this CGFun.
  Map<Var *, llvm::Value *> values;
};

class CGProgram : public gc {
 public:
  Vec<CGFun *> funs;
  Map<Fun *, CGFun *> fun_map;
  CGFun *main_fun;
};
```

That's it. No CGType, no CGSlot, no CGValue, no CGInst (the
per-instruction emission stays IF1-shaped through
`write_llvm_prim`). The IR is **block structure + scoped
value cache**.

Lines of code: ~80 in cg_ir.h, ~150 in cg_normalize.cc,
~300 in emit_cg.cc. Down from current ~250 / ~600 / ~350.

The cg-normalize test phase still works, dumping CGFun
shapes. The fixture format changes (drop slot/type sections),
golden reblesses needed.

## Trade-offs

**In favor of v2**:

- Eliminates the parallel type/slot/value model that bugs
  hide in.
- Fixes the structural cause of issue 017 (function-scoped
  value cache).
- Smaller surface for Phase 4 (C backend) to consume — it
  doesn't need any of CGType/CGSlot/CGValue.
- Honest about what CG_IR is: scaffolding for the LLVM
  swap, not an independent IR.

**Against v2**:

- Loses the "explicit type/slot/value model" goal that
  CG_IR_PLAN started with. The CGProgram dump becomes less
  self-describing.
- The cg-normalize golden's visual record of types and slots
  goes away; debugging via golden becomes harder.
- Future work to make CGProgram truly independent of IF1 (no
  source_pn, self-describing instructions) becomes a fresh
  start, not an evolution.
- Reblessing all 19 cg-normalize fixtures.

## Recommendation

**Don't rewrite immediately.** The current CG_IR shipped Phase
3.4 (production swap) at 38/37 — it's working production code.
A rewrite risks regression on the path we just stabilized.

**Do the following before Phase 4**:

1. **Treat current CG_IR as "scaffold + escape hatches."**
   Document this explicitly in cg_ir.h: "CGType wraps Sym,
   CGSlot wraps Var, source_pn is the dispatch key. This is
   a per-block emission scaffold, not an IR." Stop adding
   features to CGType / CGSlot / CGValue.

2. **Fix issue 017 with the targeted per-CGFun value cache
   only.** Add `Map<Var *, llvm::Value *> CGFun::values` and
   a `resolve_value(Var*, CGFun*)` API in emit_cg.cc. Don't
   touch the rest of CG_IR. This is ~50 LOC and surgical.

3. **Phase 4 (C backend → CGProgram) reuses CGBlock +
   terminator metadata only.** The C backend doesn't need
   CGType/CGSlot/CGValue at all — it has `cg_string` and
   walks the IF1 tree. CGFun gives it block ordering for
   labeled-goto emission, which is mostly already there.

4. **Phase 5 cleanup deletes CGType / CGSlot / CGValue.**
   By then both backends are on CGFun + CGBlock; the unused
   types can go.

This gives v2 incrementally without the disruption of a
rewrite, and it addresses issue 017 in a focused
surgical-strike way.

## Open question

If the per-CGFun value cache fixes issue 017, does it also
fix the for-loop cohort and unlock the suite count beyond
38? Probably yes for the construction-flow chain, but the
field-index investigation showed at least one other subtle
bug (the wrong-struct-layout case from a previous build).
Worth measuring once the cache lands.
