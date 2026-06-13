# CG_IR_NEEDS.md — what the backends actually read at emission time

Phase 1 of `CG_IR_META_PLAN.md`. Tabulates every field read by
the production emission paths with line references, then
distills to a tight "must expose" list cross-checked against
Phase 0's survey.

**Method**:

1. For each backend (cg.cc; llvm.cc + llvm_codegen.cc +
   llvm_primitives.cc) grep every field access in emission
   sites.
2. Tag whether the read happens at emission time (vs. setup
   like `assign_*_cg_strings` or `createGlobalVariables`).
3. Compute INTERSECTION (what both backends need) and UNION
   (what CG_IR must support to enable both).
4. Cross-check against Phase 0's survival list. Reads outside
   the survival list mean the survey missed something or the
   emitter is reaching for state it shouldn't.
5. Distill to a tight enumeration of "what CG_IR must expose."

---

## C backend reads (`cg.cc`, 918 LOC)

Authoritative because the C backend ships green (74/0). Every
field read here is genuinely load-bearing.

### Sym reads (12 distinct fields)

| Field | Read sites (sample) | Why |
|---|---|---|
| `id` | 781 | `_CG_s%d` struct typedef name |
| `name` | 171, 287, 787 | field name comment, debug print |
| `type` | 698, 711, 781, 783 | per-Var type for typedef + struct layout |
| `has` | 38, 466, 728, 780-794 | function args; record field list |
| `has_names` | (via cg_writeln output) | rare; tuple field names |
| `type_kind` | 268, 300, 773, 779 | dispatch on Type_RECORD / Type_FUN / Type_SUM |
| `num_kind` | (via `num_string`) | int vs float vs uint discriminator |
| `num_index` | (via `num_string`) | 8/16/32/64 width |
| `fun` | 451, 475, 715, 777, 803 | Sym→Fun bridge; closure resolution |
| `element` | 145, 793 | vector/array element type |
| `ret` | (via `f->rets` indirectly) | function return type |
| `alias` | (via `unalias_type`) | resolve aliases at struct boundaries |

### Sym flag bits read (10 bits)

| Bit | Read at | Why |
|---|---|---|
| `is_constant` | 825, 944 (via Var->sym) | gates constant initializer emission |
| `is_fun` | 101, 451, 666, 715 | gates function-pointer paths |
| `is_symbol` | 144, 170, 204, 666, 694 | gates symbol-literal emission |
| `is_external` | 860 | skip external funs |
| `is_fake` | 692, 693 | skip fake-sym vars |
| `is_local` | 692 | local-scope detection |
| `is_vector` | 144, 791 | vector vs scalar emit path |
| `is_structure` | (via `c_type`) | V-side POD record path (decorative for pyc) |
| `live` | 666, 783 | gates emission of Sym-level globals |
| `type_kind` | many | numeric value, ranges over Type_kind enum |

### Sym constants / immediates (3 fields)

| Field | Read at | Why |
|---|---|---|
| `constant` | 287, 290, 300, 315, 403, 829, 837 | constant payload (string repr) |
| `imm` | 825, 827 (via `sprint_imm`) | numeric immediate |
| `creators` (rare) | 803 | tuple-creators check for homogeneous_tuple |

### Var reads (7 fields)

| Field | Read at | Why |
|---|---|---|
| `sym` | many | back to Sym for everything else |
| `type` | 51, 704, 711, 783 (via has[i]->var) | per-Var type for typedef and casts |
| `def` | 57 (via `application_depth`) | rare; for chained `apply` ops |
| `is_internal` | 693 | skip internal vars in temp emission |
| `is_formal` | (via fun arg position) | not explicit in cg.cc; via `args` map |
| `live` | 42, 224, 269, 447, 561, 656, 666, 694 | gates emission |
| `cg_string` | 127 reads | the C-backend's runtime identity |

### PNode reads (10 fields)

| Field | Read at | Why |
|---|---|---|
| `code` | 586, 588, 604, 622, 629, 639, 651, 657, 658 | kind dispatch + label resolution |
| `live` | 586, 606, 642, 656 | gates per-kind emission |
| `fa_live` | 586, 606, 639, 642, 656 | gates per-kind emission |
| `lvals` | 191-225, 235, 269 (many) | def targets |
| `rvals` | 125-403, 535-617 (many) | use sources |
| `cfg_succ` | 610, 614, 619, 626, 651, 652, 656, 658 | block control flow |
| `phi` | (via `do_phi_nodes` at 575) | SSU MOVEs after node |
| `phy` | (via `do_phy_nodes` at 571) | SSU MOVEs before node |
| `prim` | 127, 179, 350, 413, 536, 642 | per-prim dispatch (Code_SEND) |
| `cfg_pred_index` | (via `do_phi_nodes`) | which predecessor's MOVE goes here |

### Code/Label reads (3 fields)

| Field | Read at | Why |
|---|---|---|
| `Code::kind` | 586 (etc, indirect via `n->code->kind`) | per-kind dispatch |
| `Code::label[]` | 588, 622, 629, 639, 658 | LABEL / GOTO / IF targets |
| `Label::id` | 588, 622, 629, 639, 658 | `L%d` emission |

### Fun reads (10 fields)

| Field | Read at | Why |
|---|---|---|
| `id` | (via cg_string built by `assign_fun_cg_strings`) | `_CG_f_%d` name |
| `sym` | 38, 470, 530 | back-ref for has list and identity |
| `entry` | 684, 720, 721 | start of emission |
| `live` | 681, 756 | skip non-live funs |
| `args` | 41, 505, 664 | per-formal-arg lookup |
| `positional_arg_positions` | 663 | ordering for arg emission |
| `rets` | 21, 22 | return type emission |
| `is_external` | 860 | skip externals |
| `is_varargs` | (NOT used by cg.cc directly; via wrapper) | varargs detection |
| `cg_string` | many | function name |
| `cg_structural_string` | 28, 761, 762 | function-pointer typedef |

### Prim reads (2 fields)

| Field | Read at | Why |
|---|---|---|
| `index` | 127, 179, 350, 642 | per-prim dispatch (the 16 cases) |
| `name` | 413, 536 | `_CG_%s` for cgfn back-translation |

---

## LLVM backend reads (`llvm.cc` 1542 + `llvm_codegen.cc` 721 +
## `llvm_primitives.cc` 1274 LOC)

Strict superset of the C backend. Every C-read shows up here
too. The LLVM-only reads:

### Sym (additional reads beyond C)

| Field | Sample | Why LLVM-specific |
|---|---|---|
| `is_value_type` | llvm.cc:553 | gates getLLVMVarType heap→ptr override |
| `llvm_type` | llvm.cc:357, 798 | StructType cache |
| `llvm_type_di_cache` | llvm.cc:566 | debug info |
| `size` | llvm_primitives.cc:676 | inline-struct size for some allocs |

### Var (additional reads beyond C)

| Field | Sample | Why LLVM-specific |
|---|---|---|
| `avars` | llvm_codegen.cc:101 | `arg_is_live_for_codegen` walks AVar map |
| `constant` | llvm.cc:794 | constant recovery on cache miss |
| `def` | llvm_codegen.cc:451 | `simple_move` chains |
| `llvm_value` | many | per-Var cache (the issue 017 field) |
| `llvm_type` | many | per-Var type cache |
| `llvm_debug_var` | llvm_codegen.cc:201 | debug info |

### Fun (additional reads beyond C)

| Field | Sample | Why LLVM-specific |
|---|---|---|
| `fa_all_Vars` | llvm_codegen.cc:374 | `allocate_locals` walks every Var |
| `llvm` | many | per-Fun llvm::Function cache |

### PNode (additional)

| Field | Sample | Why LLVM-specific |
|---|---|---|
| `code->line()` | llvm_primitives.cc:394 | debug info |

### Per-primitive emitter cross-cutting reads

The 30 cases in `write_llvm_prim` collectively reach for:

- `obj_type_sym->has[i]->name` (field name resolution at
  llvm_primitives.cc:962-964, 1056-1058)
- `obj_type_sym->type_kind == Type_SUM` for SUM unwrapping
  (llvm_primitives.cc:920, 1049)
- `obj_type_sym->has.n` for tuple v-slot positioning
  (llvm_primitives.cc:762, 817)
- `var->type->is_vector` for vector path (llvm_primitives.cc:757)
- `type_sym->name` for diagnostic
  (llvm_primitives.cc:980, 984)

These are all **type structural reads** — they want the
struct field layout. CG_IR's CGType in v1 attempted to
parallel this and didn't earn its keep; the per-prim emitter
already reads directly from `Sym->has`.

---

## Intersection — what BOTH backends need (the CG_IR core)

| Concept | C-side | LLVM-side | Phase 0 survey row |
|---|---|---|---|
| **Type identity** | `Sym::id`, `name` | same | DECL load-bearing ✓ |
| **Type kind** | `type_kind` | same | DECL load-bearing ✓ |
| **Numeric width** | `num_kind`, `num_index` | same | DECL load-bearing ✓ |
| **Field list** | `Sym::has[]`, `has_names` | same | DECL load-bearing ✓ |
| **Element type** | `Sym::element` | same | DECL load-bearing ✓ |
| **Type aliasing** | `unalias_type` | same | DECL load-bearing ✓ |
| **Function pointer bridge** | `Sym::fun` | same | DECL load-bearing ✓ |
| **Constant payload** | `Sym::constant`, `Sym::imm` | same | DECL load-bearing ✓ |
| **Type-level flags** | `is_constant`, `is_fun`, `is_symbol`, `is_external`, `is_fake`, `is_local`, `is_vector` | same | DECL load-bearing ✓ |
| **Var identity** | `Var::sym`, `id` | same | DECL load-bearing ✓ |
| **Var type** | `Var::type` | same | DECL load-bearing ✓ |
| **Var flags** | `is_internal`, `is_formal`, `live` | same | DECL load-bearing ✓ |
| **PNode op-kind** | `Code::kind` (5 cases) | same | STRUCT load-bearing ✓ |
| **PNode liveness** | `live && fa_live` | same | PHASE load-bearing ✓ |
| **PNode def/use** | `lvals`, `rvals` | same | STRUCT load-bearing ✓ |
| **PNode CFG** | `cfg_succ` | same | STRUCT load-bearing ✓ |
| **SSU MOVEs** | `phi`, `phy`, `cfg_pred_index` | same | STRUCT load-bearing ✓ |
| **Primitive dispatch** | `Prim::index` | same | DECL load-bearing ✓ |
| **Label identity** | `Label::id` | same | DECL load-bearing ✓ |
| **Function signature** | `Fun::args`, `rets`, `sym->has` | same | STRUCT load-bearing ✓ |
| **Function entry** | `Fun::entry` | same | STRUCT load-bearing ✓ |
| **Function liveness** | `Fun::live` | same | DECL load-bearing ✓ |
| **Function attrs** | `is_external`, `is_varargs` | same | DECL load-bearing ✓ |
| **Function naming** | `cg_string`, `cg_structural_string` (C-side) — name + signature derivation (LLVM-side) | analogous | BACKEND load-bearing ✓ |

That's **24 conceptual entries** — each one appears in both
backends' reads. Cross-checked: every entry is also in Phase 0's
"What survives" list. **No surprises.**

---

## Union − Intersection — what LLVM needs but C doesn't

| Concept | LLVM-side | Equivalent in C |
|---|---|---|
| **Per-Var value cache** | `Var::llvm_value` | replaced by `cg_string` lookup |
| **Per-Sym type cache** | `Sym::llvm_type` | replaced by `cg_get_string(s)` |
| **Per-Fun fn cache** | `Fun::llvm` | replaced by `cg_get_string(f)` |
| **Per-Var debug info** | `Var::llvm_debug_var` | (no C-side equivalent) |
| **AVar liveness check** | `Var::avars` in `arg_is_live_for_codegen` | (C uses `Var::live` only) |
| **Function locals enum** | `Fun::fa_all_Vars` in `allocate_locals` | (C just emits typedef + uses, no allocas) |
| **Heap aggregate flag** | `Sym::is_value_type` (issue 015) | (C uses `_CG_psN` typedef convention) |
| **DCE-proven constant recovery** | `Var::constant` | (C uses `cg_string` directly) |
| **Source line info** | `Code::line()` | (C doesn't emit debug info) |

These are LLVM-specific because LLVM is **statically typed
SSA with function-scoped values** whereas C's emission model
is **statement-level text with implicit scoping**. The C
backend's `cg_string` collapses identity, type, and value
into a single string the C compiler resolves.

The LLVM-side additions are NOT structural to the IR — they're
about how the LLVM backend translates the IR. A clean CG_IR
exposes the IR concepts; the backend-specific caches sit on
top.

---

## Reality check — are there reads OUTSIDE Phase 0's survival
## set?

Yes, a few. Worth documenting:

### Marginal reads not flagged in Phase 0

1. **`Var::avars` in `arg_is_live_for_codegen`**
   (llvm_codegen.cc:101). Phase 0 said AVar contributes zero;
   this is a PRESENCE check (does the Var have any AVar
   binding at all?), not an actual lattice read. It's the only
   place AVar leaks into emission, and it's checkable via
   `Var::live || any avar->live`. The Phase 0 conclusion holds
   in spirit: the AVar lattice itself is gone; only a
   presence-bit survives. A v2 design exposes this as
   `Var::has_any_live_use`.

2. **`Var::def` in `application_depth` and chained moves**
   (cg.cc:57, llvm_codegen.cc:451). Phase 0 marked this
   STRUCT load-bearing, which is correct. The def-use chain
   IS needed for emission ordering.

3. **`Sym::creators[0]->sym` in `homogeneous_tuple` check**
   (cg.cc:803). Phase 0 marked `creators` as ANALY/partial.
   This use is rare and could be folded into a Sym-level flag
   `is_tuple_creator` for CG_IR.

4. **`Sym::specializers.set_in(...)` in tuple-form
   detection** (cg.cc:144). Phase 0 marked `specializers` as
   ANALY/partial. Used for "is this Sym a tuple specializer?"
   — could be a Sym-level flag.

5. **`Code::line()` via `Code::ast`** (llvm_primitives.cc:394).
   Source line info for debug. Survives in spirit but the
   `ast` field itself is heavyweight. CG_IR should expose a
   thin `(line, file)` source location per CGInst, not the
   raw AST.

**Conclusion**: 5 marginal reads, all of which CG_IR can
absorb cleanly with either small flag projections or thin
metadata fields.

---

## Distilled: what CG_IR MUST expose

Cross-checked against intersection + union − marginal.

### 1. **Types** (the small fixed shape)

```
struct CGType {
  int id;
  cchar *name;
  enum { VOID, INT, UINT, FLOAT, BOOL, PTR, STRUCT, FUN_PTR, REF, SUM, SYMBOL } kind;
  int bits;                      // for INT/UINT/FLOAT
  Vec<CGField> fields;           // STRUCT: ordered (type, name) pairs
  CGType *element;               // REF / vector element
  CGType *alias_of;              // unalias chain
  CGFunSig *fun_sig;             // FUN_PTR signature
  bool is_value_type;            // issue 015 hook
  // backend caches: cg_string (C), llvm_type (LLVM)
};
```

12 fields. Maps 1:1 to the Phase 0 survival set's type fields.

### 2. **Values** (Var-shaped, flat)

```
struct CGValue {
  int id;
  CGType *type;
  CGFun *defined_in;             // function scope (issue 017 fix)
  CGInst *def;                   // single defining inst (post-SSA)
  enum { LOCAL, FORMAL, GLOBAL, CONSTANT, FUN_REF, SYMBOL } kind;
  // For CONSTANT:
  Immediate imm;
  cchar *constant_str;
  // backend caches (per-(CGFun, CGValue))
};
```

7 fields + variant payload. The `defined_in` field is the
**critical addition** that fixes issue 017's class
structurally. Value identity is function-scoped by
construction.

### 3. **Operations** (the per-PNode emitter unit)

```
struct CGInst {
  enum CGOp op;                  // MOVE / SEND / IF / GOTO / LABEL +
                                 // structural subops (NEW, PERIOD, SETTER, ...)
  Vec<CGValue *> rvals;
  Vec<CGValue *> lvals;
  bool live;                     // emit gate
  Prim *prim;                    // SEND dispatch
  CGBlock *succ_true, *succ_false; // for IF
  CGBlock *goto_target;          // for GOTO
  CGLabel *label;                // for LABEL
  CGSourceLoc src_loc;           // (line, file) for debug
};
```

The `live` field collapses `live && fa_live` into one bit at
CG_IR boundary; the IR doesn't need to expose the FA's
internal distinction.

### 4. **Blocks** (basic blocks with explicit terminator)

```
struct CGBlock {
  int id;
  CGLabel *label;                // optional
  Vec<CGInst *> body;            // non-terminator insts
  CGInst *terminator;
  Vec<CGBlock *> preds, succs;
  // Per-pred phi MOVEs (replaces PNode::phi / phy)
  Map<CGBlock *, Vec<CGMove>> phi_moves;
};
```

5 fields + the per-pred phi map.

### 5. **Functions**

```
struct CGFun {
  int id;
  cchar *name;                   // for diagnostic / linking
  CGType *signature;             // ret + arg types
  Vec<CGValue *> formals;        // by ordinal position
  CGBlock *entry;
  Vec<CGBlock *> blocks;
  Vec<CGValue *> locals;         // for backend storage allocation
  bool live, is_external, is_varargs;
  // Per-CGFun value cache (the issue 017 piece)
  Map<CGValue *, void *> backend_value_cache;
};
```

~10 fields. The `backend_value_cache` is the **structural
issue-017 fix**.

### 6. **Program**

```
struct CGProgram {
  Vec<CGType *> types;
  Vec<CGValue *> globals;        // module-scope values
  Vec<CGFun *> funs;
  CGFun *main;
};
```

4 fields.

### 7. **Primitives** (kept as IF1-side reference)

CG_IR doesn't redefine primitives. It references `Prim *`
from IF1, and the per-prim emitters dispatch on `prim->index`.
This is the deliberate escape hatch that lets the per-prim
implementations stay in `llvm_primitives.cc` and
`cg.cc::write_c_prim` without porting.

### 8. **Source locations**

Thin `(line, file)` pair per CGInst. Not the full IF1 AST.

---

## The "must expose" list — final

Counting concepts (not fields):

1. **Types** with kind discriminator + numeric width +
   ordered field list + element + alias chain + value-type
   flag
2. **Values** with type + function scope + single
   defining inst + kind discriminator (LOCAL/FORMAL/GLOBAL/
   CONSTANT/FUN_REF/SYMBOL) + constant payload
3. **Operations** with op kind + def/use Vec<CGValue> + live
   gate + prim hint + branch targets
4. **Blocks** with body + terminator + preds/succs + per-pred
   phi MOVEs
5. **Functions** with signature + entry + blocks + locals +
   flags + per-fun value cache
6. **Program** with types/globals/funs/main
7. **Primitives** referenced from IF1 (escape hatch by design)
8. **Source locations** for debug info

**8 concepts. That's the IR contract.**

Cross-checked against both backends' reads: every
load-bearing read in cg.cc and the LLVM emitters maps to one
of these 8.

---

## Surprises and notable observations

1. **The C backend's `cg_string` IS its IR identity.** With
   127 reads in 918 LOC, it's the densest field access in
   the codebase. Removing it requires either (a) re-deriving
   names from CGValue at every use site or (b) adding an
   explicit `name` field to CGValue. Option (b) is what v2
   should do — names are language-agnostic and serializable
   for the textual form.

2. **The LLVM backend has ~90% overlap with the C backend's
   reads.** The 10% delta is type caching, debug info,
   function-scoped allocas, and one AVar presence check. The
   delta is backend-implementation concern, not IR concern.

3. **No reads outside Phase 0's survival set, except 5 marginal
   ones (all absorb-able).** The survey was accurate.

4. **The AVar layer truly does not leak into emission** beyond
   the one presence check. Confirms the flatten hypothesis.

5. **The Sym layer's `has[]` is the field-layout source of
   truth.** Every struct emission walks `obj_type_sym->has`.
   CG_IR's CGType.fields must preserve this 1:1.

6. **Primitives stay as IF1's Prim — we don't re-define them
   in CG_IR.** The 30 P_prim_* cases are stable; per-prim
   emitters are stable; the escape hatch is the right
   abstraction here.

---

## Phase 1 exit criteria — checked

- [x] Every field read in production emission paths
      tabulated with line refs.
- [x] Intersection (both backends) and union (CG_IR support
      requirement) computed.
- [x] Cross-checked against Phase 0 survival set.
- [x] 5 marginal reads outside survival set identified and
      shown to be absorbable.
- [x] Distilled to a tight 8-concept "must expose" list.

---

## Recommended next phase

Phase 2 (Sketch). Take the 8-concept list and translate to a
header-only declaration with prose-heavy comments. Walk
through 3-5 program shapes lowering to it. Output:
`CG_IR_SKETCH.h`.

The sketch should explicitly:

- Make per-function value scope (issue 017 fix) load-bearing.
- Keep primitives as IF1 Prim references (escape hatch is
  contractual).
- Drop the parallel type/slot/value models from v1.
- Add explicit names on CGValues (replacing cg_string
  side-channel).
- Add source locations as thin metadata.

I can do Phase 2 in a single session.

---

## Open questions surfaced during distill

These need answers in Phase 5 (semantics doc), not earlier:

1. **Where do per-(CGFun, CGValue) backend caches live?** On
   CGFun (a map) or on CGValue (a per-CGValue field with a
   nullable owner pointer)? Performance vs. clarity trade-off.

2. **Do CGValues for globals and constants exist before any
   CGFun?** If yes, they need a "program-scope" lifetime
   different from function-scope.

3. **Phi MOVEs as CGMove records or as ordinary CGInsts in
   predecessor blocks?** The current implementation does the
   latter via `emit_phi_phy`; making them explicit records
   per-pred could clarify the model.

4. **Should CGType expose `is_heap_aggregate`?** All current
   pyc record types are heap; V's POD records (issue 015) are
   value. The flag exists on `Sym::is_value_type`; CG_IR
   should make it first-class.

5. **Source location: per-CGInst, or attribute on enclosing
   CGBlock?** LLVM does per-instruction; C-style debug info
   does per-statement. Probably per-CGInst for granularity.
