# CG_IR_SURVEY.md — what IF1 carries, what survives into emission

Phase 0 of `CG_IR_META_PLAN.md`. Pure reading + classification.

**Goal**: classify every field on the core IF1 data structures
(Sym/Var/AVar/PNode/Code/Fun/Prim/AType + supporting types) as
load-bearing or vestigial at emission time. Output: this doc.

**Method**: read each header file, tabulate every field with:

- Which phase populates it
- Which phase reads it
- Whether the codegen layer (cg.cc, llvm.cc, llvm_codegen.cc,
  llvm_primitives.cc, cg_normalize.cc, emit_cg.cc) reads it

Classification legend:

| Tag | Meaning |
|---|---|
| **DECL** | Declarative — language-level identity, scoping, type |
| **ANALY** | Analysis — FA lattice, AType, flow facts |
| **STRUCT** | Structural — IF1 shape: PNode/Code/CFG/dominators |
| **BACKEND** | Backend annotation — side-channel cache (cg_string, llvm_value) |
| **PHASE** | Phase-specific scratch — meaningful only mid-pass |
| **HIST** | Historical / vestigial — looks unused or supplanted |

Survived = read by cg.cc or one of the LLVM emitter files at
production codegen time.

---

## Sym (`if1/sym.h`)

`BasicSym` base + `Sym` extension. Where IF1's language-level
identity lives. Every variable, type, function, and module is
a Sym.

### BasicSym fields

| Field | Class | Populated by | Read by | Survives to emission? |
|---|---|---|---|---|
| `id` | DECL | Sym constructor | everywhere | **YES** (struct names `_CG_sN`) |
| `name` | DECL | parser | everywhere | **YES** (function names, field names) |
| `in` | DECL | parser/scope | scope.cc, fa.cc | NO (scoping resolved pre-codegen) |
| `type` | DECL | analyzer | many | **YES** (field/var types) |
| `aspect` | DECL | analyzer | fa.cc | NO |
| `must_specialize` | DECL | parser | fa.cc, pattern | NO |
| `must_implement` | DECL | parser | fa.cc, pattern | NO |
| `ast` | DECL | parser | many | partial (line info only) |
| `var` | BACKEND | fa.cc, cg.cc | many | **YES** (canonical Var for globals) |
| `asymbol` | DECL | frontend | frontend only | NO |
| `nesting_depth` | DECL | parser | scope.cc, fa.cc | NO |
| `cg_string` | BACKEND | cg.cc, llvm.cc | cg.cc, llvm.cc | **YES** (load-bearing) |
| `llvm_value` | BACKEND | llvm.cc | llvm.cc | **YES** (per-Sym global cache) |
| `is_builtin` | DECL | parser | fa.cc | NO |
| `is_read_only` | DECL | parser | cg.cc | partial (const globals) |
| `is_constant` | DECL | parser, fa | cg.cc, llvm.cc | **YES** (gates `getLLVMConstant`) |
| `is_lvalue` | DECL | parser | scope, fa | NO |
| `is_local` | DECL | parser | cg.cc, scope | **YES** (gates global vs local) |
| `is_default_arg` | DECL | parser | pattern | NO |
| `is_exact_match` | DECL | parser | pattern | NO |
| `is_module` | DECL | parser | scope, fa | NO |
| `is_fun` | DECL | parser/closure | many | **YES** (gates function-pointer paths) |
| `is_symbol` | DECL | parser | many | **YES** (gates symbol-name emission) |
| `is_pattern` | DECL | parser | pattern | NO |
| `is_rest` | DECL | parser | pattern | NO |
| `is_generic` | DECL | parser | fa | NO |
| `is_external` | DECL | parser | many | **YES** (gates external linkage) |
| `is_this` | DECL | parser | scope, fa | NO |
| `is_fake` | DECL | parser, fa | cg.cc | **YES** (skipped in var emission) |
| `intent` | DECL | parser | fa, pattern | NO |
| `is_meta_type` | DECL | parser | fa | NO |
| `is_unique_type` | DECL | parser | fa | NO |
| `is_value_type` | DECL | parser | llvm.cc | **YES** (issue 015 hook) |
| `is_system_type` | DECL | parser | fa | NO |
| `is_union_type` | DECL | parser | fa | NO |
| `is_structure` | DECL | parser, fa | cg.cc partial | partial (V's POD records) |
| `is_vector` | DECL | parser, fa | cg.cc, llvm.cc | **YES** (vector emit path) |
| `fun_returns_value` | ANALY | fa | cg.cc | rare |
| `live` | PHASE | dce | many | **YES** (gates emission) |
| `type_kind` | DECL | parser | many | **YES** (struct vs primitive vs sum etc) |
| `num_kind` | DECL | parser | many | **YES** (int vs float vs uint) |
| `num_index` | DECL | parser | many | **YES** (8/16/32/64 width) |
| `clone_for_constants` | PHASE | fa | clone | NO |
| `dispatch_types_built` | PHASE | fa | fa | NO |
| `type_live` | PHASE | dce | cg, llvm | partial (decorative for ifs) |

### Sym (extension) fields

| Field | Class | Populated by | Read by | Survives? |
|---|---|---|---|---|
| `constant` | DECL | parser | cg.cc, llvm.cc | **YES** |
| `imm` | DECL | parser | many | **YES** |
| `size` | DECL | parser | sym | partial |
| `alignment` | DECL | parser | sym | partial |
| `scope` | PHASE | scope.cc | scope, fa | NO |
| `labelmap` | PHASE | ast.cc | ast | NO |
| `generic_args` | DECL | parser | fa | NO |
| `has` | DECL | parser | many | **YES** (field list — load-bearing) |
| `has_names` | DECL | parser | rare | rare (tuple named fields) |
| `fun` | DECL | parser, fa | many | **YES** (Sym→Fun bridge) |
| `code` | DECL | parser | many | partial (build phase only) |
| `self` | DECL | parser | scope, fa | NO |
| `ret` | DECL | parser | fa, cg | **YES** (return type sym) |
| `cont` | DECL | parser | fa, cg | partial (continuation symbol) |
| `instantiates` | DECL | parser | fa | NO |
| `substitutions` | DECL | parser | fa | NO |
| `match_type` | ANALY | pattern | pattern | NO |
| `abstract_type` | ANALY | fa | fa | NO |
| `creators` | ANALY | fa | cg.cc partial | partial (cg.cc:803) |
| `specializes` | DECL | parser | fa | NO |
| `includes` | DECL | parser | scope | NO |
| `implements` | DECL | parser | fa | NO |
| `isa` | ANALY | fa | fa | NO |
| `alias` | DECL | parser | many | **YES** (via `unalias_type`) |
| `init` | DECL | parser | fa | NO |
| `meta_type` | DECL | parser | fa | NO |
| `element` | DECL | parser | cg.cc, llvm.cc | **YES** (vector/array element type) |
| `implementors` | ANALY | fa | fa, pattern | NO |
| `specializers` | ANALY | fa | fa, pattern, cg | partial (cg.cc:144) |
| `dispatch_types` | ANALY | fa | fa, pattern | NO |
| `temp` | PHASE | various | various | NO (scratch) |
| `llvm_type` | BACKEND | llvm.cc | llvm.cc | **YES** (StructType cache) |
| `llvm_type_di_cache` | BACKEND | llvm.cc | llvm.cc | partial (debug info) |

**Sym summary** (out of ~80 fields):

- **Survive to emission**: ~25. Dominated by `id`, `name`,
  `type`, `has`, the is_* flags that gate emission paths
  (is_constant/is_local/is_fun/is_symbol/is_external/
  is_fake/is_value_type/is_vector), num_kind/num_index/
  type_kind, `constant`/`imm`, `alias`/`element`/`ret`/`fun`,
  and the three side-channels `cg_string`/`llvm_value`/
  `llvm_type`.
- **DECL but resolved-and-discardable**: ~30. Identity-level
  fields that the analyzer consumes (`aspect`,
  `must_specialize`, etc.) — by the time codegen runs, the
  facts are baked into `type`/`has`/`ret`.
- **ANALY**: ~10. Flow-analysis facts — `creators`,
  `specializers`, `dispatch_types`, `abstract_type`. Mostly
  not read at emission, with a few exceptions (cg.cc:803
  reads `creators[0]->sym`).
- **PHASE**: ~6. Scratch space; not used at codegen.
- **BACKEND**: 3 (cg_string, llvm_value, llvm_type). These
  are the side-channels.

**Implication for CG_IR**: the post-emission Sym view is
about 25 fields, dominated by id/name/type/has/type_kind +
the bit flags. Everything else can be "compiled away" at
the CG_IR boundary.

---

## Var (`if1/var.h`)

Per-SSA-version instance of a Sym. Each Var has a single
definition (post-SSU).

| Field | Class | Populated by | Read by | Survives? |
|---|---|---|---|---|
| `sym` | DECL | Var constructor | many | **YES** (back to Sym) |
| `id` | DECL | Var ctor | many | **YES** (debug/diagnostic) |
| `type` | DECL | fa.cc | many | **YES** (load-bearing) |
| `mark` | PHASE | ssu.cc | ssu.cc | NO |
| `def` | STRUCT | cfg/ssu | many | **YES** (def-use chains) |
| `uses` | STRUCT | ssu.cc | various | partial |
| `avars` | ANALY | fa.cc | fa.cc | NO |
| `as_CreationSet` | ANALY | fa.cc | fa.cc | NO |
| `is_internal` | DECL | parser | cg.cc | **YES** (gates emission) |
| `is_filtered` | ANALY | fa.cc | fa.cc | NO |
| `is_formal` | DECL | parser, fa | many | **YES** (gates alloca/arg path) |
| `live` | PHASE | dce | many | **YES** (gates emission) |
| `constant` | ANALY | dce | llvm.cc | **YES** (constant recovery) |
| `cg_string` | BACKEND | cg.cc | cg.cc | **YES** |
| `llvm_value` | BACKEND | llvm.cc | llvm.cc | **YES** (the problematic one) |
| `llvm_type` | BACKEND | llvm.cc | llvm.cc | **YES** |
| `llvm_debug_var` | BACKEND | llvm.cc | llvm.cc | partial (DI) |
| `ssu` (union) | PHASE | ssu.cc | ssu.cc | NO |

**Var summary** (18 fields):

- 8 survive to emission, plus 4 backend annotations.
- 6 are PHASE/ANALY and gone by emission time.

**Key insight**: a Var at emission time is `(Sym*, type*, id,
is_formal, is_internal, live, constant) + cg_string/
llvm_value/llvm_type cache`. The whole `avars`/`mark`/`uses`
machinery is gone. The SSA shape is implicit in `def` (each
Var has exactly one defining PNode).

**Issue 017 lives here**: `llvm_value` is the
program-scoped cache that should be function-scoped.

---

## AVar (`analysis/fa.h`)

The analysis layer. **Vanishes by codegen time** — used only
during FA to track abstract types per Var per context.

| Field | Class | Survives? |
|---|---|---|
| `var` | back-ref | back-ref only |
| `id` | DECL | NO |
| `contour` | ANALY | NO |
| `forward`/`backward` | ANALY | NO |
| `creation_set` | ANALY | NO |
| `setters` | ANALY | NO |
| `out`/`in` | ANALY | NO |
| `type` (AType) | ANALY | NO (collapsed into Var::constant) |
| ... | ANALY | NO |

**AVar summary**: zero fields read at emission. The whole
class **collapses entirely at the CG_IR boundary**. This
confirms the project owner's "flatten Sym/Var/AVar pyramid"
intuition — AVar is the layer that disappears.

What survives from FA's analysis: type assignments (Var::type
populated from AVar's lattice convergence) and constant
recovery (Var::constant set when DCE proves a value is
known). Those are projected DOWN onto the Var layer; the
AVar machinery doesn't survive.

---

## PNode (`if1/pnode.h`)

Program node. Wraps a Code with control-flow + analysis
state. Each PNode is one statement-level operation.

| Field | Class | Populated by | Read by | Survives? |
|---|---|---|---|---|
| `code` | STRUCT | parser | many | **YES** (kind, label, prim) |
| `id` | DECL | PNode ctor | rare | partial (debug) |
| `live` | PHASE | dce | many | **YES** (gates emission) |
| `fa_live` | PHASE | fa, dce | many | **YES** (gates emission) |
| `lvals` | STRUCT | parser, ssu | many | **YES** (def targets) |
| `rvals` | STRUCT | parser, ssu | many | **YES** (use sources) |
| `tvals` | STRUCT | parser | cg.cc | partial (temp vars) |
| `mark` | PHASE | ssu | ssu | NO |
| `cfg_succ` | STRUCT | cfg.cc | many | **YES** (CFG edges) |
| `cfg_pred` | STRUCT | cfg.cc | many | **YES** (CFG edges) |
| `phi` | STRUCT | ssu.cc | many | **YES** (SSU MOVEs after) |
| `phy` | STRUCT | ssu.cc | many | **YES** (SSU MOVEs before) |
| `prim` | DECL | parser | many | **YES** (dispatch key) |
| `loop_node`/`live_vars` (union) | PHASE | loop.cc, ssu.cc | optimization | NO |
| `cfg_pred_index` | STRUCT | cfg.cc | cg.cc, llvm | **YES** (phi predecessor index) |
| `dom`/`rdom` | STRUCT | dom.cc | dom | NO (rebuilt if needed) |
| `creates` | ANALY | clone | clone | NO |
| `execution_frequency` | ANALY | freq | inline, llvm.cc | partial (decorative) |
| `false_branch_frequency` | ANALY | freq | inline | NO |

**PNode summary** (~20 fields):

- 11 survive to emission: code, live, fa_live, lvals, rvals,
  cfg_succ, cfg_pred, phi, phy, prim, cfg_pred_index.
- The 11 form **the actual emission unit**. Everything else
  is build/analyze-time machinery.

**Implication for CG_IR**: a CGInst should encode the same
11 items, but typed-and-named:

- `kind` (from code->kind: MOVE/SEND/IF/GOTO/LABEL)
- `live` && `fa_live` (or "skip this" decision projected
  down to a boolean)
- `lvals` and `rvals` (the def-use pointers)
- `cfg_succ`/`cfg_pred` (rolled into block CFG)
- `phi`/`phy` (per-edge MOVE lists)
- `prim` (for SEND dispatch)
- `cfg_pred_index` (per-pred index for phi resolution)

This is small enough to fit on a screen.

---

## Code (`if1/code.h`)

Pre-CFG syntax-tree-like representation. By codegen time,
PNode has flattened most of this. The Code object remains
attached as `PNode::code`.

| Field | Class | Survives? |
|---|---|---|
| `kind` | STRUCT | **YES** (dispatch) |
| `rvals` (of Sym*) | DECL | NO (replaced by PNode::rvals of Var*) |
| `lvals` (of Sym*) | DECL | NO (same) |
| `names` | DECL | rare |
| `label[2]` | STRUCT | **YES** (LABEL/IF/GOTO targets) |
| `sub` | STRUCT | NO (flattened) |
| `ast` | DECL | partial (line info) |
| `prim` | DECL | partial (rare; PNode::prim is the canonical) |
| `partial` | DECL | NO (early phase) |
| `live` | PHASE | partial |
| `flattened` | PHASE | NO |
| `cont` | STRUCT | NO (build-phase) |
| `pn` | STRUCT | back-ref to PNode |

**Code summary**: by emission time, the only Code fields
that matter are `kind` and `label[]`. PNode subsumes the
rest. This is structurally redundant data — a CG_IR could
collapse Code into PNode entirely.

---

## Label (`if1/code.h`)

| Field | Class | Survives? |
|---|---|---|
| `id` | DECL | **YES** (L%d emission) |
| `live` | PHASE | partial |
| `code`/`bb` (union) | BACKEND | **YES** (BasicBlock cache) |

**Label summary**: 3 fields, 2 load-bearing. A CG_IR label
is essentially `(id, llvm::BasicBlock*)`.

---

## Fun (`if1/fun.h`)

A function. Largest IF1 class — ~50 fields. Carries everything
about a callable.

### Emission-time loadbearing

- `id`, `sym`, `entry`, `exit`
- `is_external`, `is_varargs`, `live`
- `args` (Map<MPosition*, Var*>), `rets`
- `positional_arg_positions` (for ordering)
- `cg_string`, `cg_structural_string`, `llvm` (cache)

### Build / analyze / clone / pattern / inline / loop machinery

The other ~35 fields. Examples: `nested`, `nested_in`,
`ess`, `fa_Vars`, `fa_all_Vars`, `fa_all_PNodes`,
`promotion_cache`, `coercion_cache`, `equiv_sets`, `nmap`,
`vmap`, `fmap`, `calls`, `called`, `wraps`, `loops`,
`loop_node`, `dom`, `execution_frequency`, `size`.

**Fun summary** (~50 fields, ~12 survive emission): the bulk
is build/analyze state. CG_IR's CGFun needs ~10 fields
mirroring the survivors. Everything else can be left in IF1's
Fun and accessed via `source_fun` escape hatch.

---

## Prim (`if1/prim.h`)

Primitives have a small, stable shape.

| Field | Class | Survives? |
|---|---|---|
| `index` | DECL | **YES** (per-prim dispatch key) |
| `string`/`name` | DECL | partial (diagnostic) |
| `nargs`/`nrets` | DECL | partial |
| `pos` | DECL | rare |
| `nonfunctional` | DECL | rare |
| `arg_types`/`ret_types` | DECL | NO (analyzer use) |
| `args` (AType) | ANALY | NO |

**Prim summary**: 1 field is load-bearing (`index`). The rest
is analyzer/pattern machinery. RegisteredPrim adds `cgfn` and
`llvm_cgfn` — these are function pointers the backends
dispatch through.

---

## Cross-cutting: what cg.cc + llvm_*.cc actually read

(Per the CG_IR_v2 audit, refreshed.)

### `cg.cc` (C backend) reads at emission time:

- **Sym**: id, name, type, has, has_names, type_kind, num_kind,
  num_index, fun, element, ret, alias, constant, imm,
  is_constant, is_fun, is_symbol, is_external, is_fake,
  is_local, is_vector, is_structure, cg_string,
  creators[0]->sym (rare), specializers (rare), live
- **Var**: sym, type, def (rare), is_internal, is_formal,
  live, cg_string
- **PNode**: code, live, fa_live, lvals, rvals, cfg_succ,
  cfg_pred (via cfg_pred_index), phi, phy, prim, cfg_pred_index
- **Code**: kind, label[]
- **Label**: id
- **Fun**: id, sym, entry, exit, live, args,
  positional_arg_positions, rets, is_external, is_varargs,
  cg_string, cg_structural_string
- **Prim**: index

### LLVM emitter reads at emission time (superset of C):

All of the C-backend reads, PLUS:

- **Sym**: is_value_type, llvm_value, llvm_type,
  llvm_type_di_cache
- **Var**: avars (for liveness check in `arg_is_live_for_codegen`),
  constant, llvm_value, llvm_type, llvm_debug_var
- **Fun**: fa_all_Vars (allocate_locals), `llvm` cache
- **Prim**: registered prim's cgfn / llvm_cgfn dispatch

The **intersection** of what both backends need is what CG_IR
must expose. The intersection is roughly **80% of the C
backend's read set** + the few LLVM-specific bits the C
backend doesn't need (debug info, value type for issue 015).

---

## The Sym/Var/AVar pyramid — flattening analysis

The project owner's intuition: at emission time, the pyramid
collapses. The survey confirms it:

| Layer | Fields read at emission | Notes |
|---|---|---|
| Sym | ~25 / ~80 (~30%) | Mostly declarative + 3 BACKEND caches |
| Var | ~8 / ~18 (~45%) | SSA def, type, formality flags + cache |
| AVar | 0 / many (0%) | **Entirely gone by emission** |

**The flatten model**:

```
At emission time, an IF1 "value" is:
  (Var, Sym=Var->sym, Type=Var->type, Constant=Var->constant)

The Var carries:
  - identity: Var* itself (pointer), Var::id (numeric)
  - def site: PNode* (single, post-SSU)
  - flags: is_formal, is_internal, live
  - cache: llvm_value (the problematic one)

The Sym carries:
  - identity: id, name
  - structural: type, has, type_kind, num_kind, num_index
  - flags: is_constant/is_fun/is_symbol/is_external/is_fake/
    is_local/is_vector/is_value_type
  - constant payload: constant, imm
  - relations: fun (Sym→Fun), alias, element, ret
  - cache: cg_string, llvm_type
```

That's **about 20 effective fields total**. The CG_IR value
model can be: a Var-shaped struct with these fields exposed
as a flat record. Sym is reachable as `Var->sym`; the AVar
layer simply doesn't exist post-emission.

---

## Operation set distillation

Post-survey, the operations the emitter dispatches on are:

| Operation | Code_kind | When | Notes |
|---|---|---|---|
| LABEL | Code_LABEL | block boundaries | trivial |
| MOVE | Code_MOVE | value rename | the SSU primitive |
| SEND | Code_SEND | call / primitive | dispatched by Prim::index |
| IF | Code_IF | conditional branch | terminator |
| GOTO | Code_GOTO | unconditional branch | terminator |

The SEND case further dispatches on `Prim::index` to 16 cases
(C) or 30 cases (LLVM, including arithmetic). Of those:

- **Structural primitives** (used by both backends): `apply`,
  `assign`, `clone`, `clone_vector`, `destruct`,
  `index_object`, `len`, `make`, `new`, `period`, `primitive`,
  `reply`, `set_index_object`, `setter`, `sizeof`,
  `sizeof_element` (16).
- **Arithmetic / comparison / logical** (LLVM only; C uses
  C operator overloading on `_CG_int64` etc): `add`,
  `subtract`, `mult`, `div`, `mod`, `less`, `lessorequal`,
  `greater`, `greaterorequal`, `equal`, `notequal`, `and`,
  `or`, `xor`, `operator` (~15).

A language-agnostic CG_IR exposes:

- The 5 Code_kinds as basic ops.
- The structural-primitive set as **explicit ops** (e.g.,
  CG_ALLOC for new, CG_LOAD_FIELD for period). This is what
  the current CG_IR attempted via `CGOp`.
- The arithmetic set as a single CG_BINOP (with sub-kind),
  letting backends specialize.
- A back-channel for "not yet promoted to an explicit op"
  via CG_CALL+prim hint.

---

## What survives — the load-bearing emission set

Distilled from the survey, the minimum information set the
emitter genuinely needs:

### Per type
- id, name
- kind (RECORD/PRIMITIVE/FUN/REF/SUM/...)
- For PRIMITIVE numeric: num_kind, num_index (bit width)
- For RECORD: fields (ordered list of field syms with names)
- For FUN: signature (args, ret)
- For REF/aggregate: element type
- alias (for unwrapping)
- size/alignment hints

### Per function
- name
- signature (arg types + ret type)
- entry block
- ordered list of blocks
- formal arg → Var mapping
- linkage (external/internal)

### Per block
- label id (for diagnostic naming)
- ordered list of PNodes (or CGInsts)
- terminator (or "implicit fall-through" with target)
- preds, succs
- per-pred phi MOVEs to emit

### Per PNode / operation
- kind (Label/Move/Send/If/Goto)
- live + fa_live (gates emission)
- lvals (Vec<Var*>)
- rvals (Vec<Var*>)
- prim (for Send)
- For If: condition Var + true/false labels
- For Goto: target label
- For Move: lhs/rhs Var

### Per value (Var)
- type
- def site (which PNode/CGInst produces it)
- formal vs local vs global vs constant
- is_internal (skip-emission hint)
- constant payload (post-DCE)
- **per-emission-context cache** (NOT global) — the bit
  CG_IR_v2 audit identified as missing

### Per primitive
- index (dispatch key)
- registered cgfn / llvm_cgfn pointers

That's the **target shape**. Everything else in IF1 is either
build-phase machinery or vestigial.

---

## Headline conclusions

1. **The flatten hypothesis is supported.** AVar contributes
   zero emission-time information. Sym contributes ~25/80
   fields. Var contributes ~8/18. The flatten model is real
   and the codegen layer already uses only this slice.

2. **The IR contract surface is small.** ~20 effective fields
   across Sym+Var, ~11 PNode fields, ~5 Code/Label fields,
   ~12 Fun fields, 1 Prim field. Under 60 distinct field
   reads total in the production emission paths.

3. **The current CG_IR is over-engineered.** It introduced
   CGType/CGSlot/CGValue as parallel models but the emitter
   still routes through `Var->sym->type` etc. The audit's
   findings are confirmed by the field-by-field tally.

4. **Issue 017's root is on Var, not on a missing CG_IR
   abstraction.** `Var::llvm_value` is the program-scoped
   cache that should be function-scoped. The fix lives at
   the Var → emission-time-value mapping layer, not in
   CGType or CGSlot.

5. **Language-agnostic shape is achievable.** The structural
   primitives are language-agnostic (new/clone/period/setter
   work the same for any class-based language). The
   arithmetic primitives can be specialized via CG_BINOP +
   sub-kind. The Code_kind dispatch is small and stable.

6. **The cg_string side-channel is the C backend's runtime.**
   `cg.cc` reads `cg_string` 127 times. It's the C backend's
   name-resolution mechanism. A CG_IR for the C backend
   either keeps this side-channel or replaces it with
   explicit per-CGValue name strings.

---

## Phase 0 exit criteria — checked

- [x] Every classified field is either clearly needed at
      emission time, or clearly only needed during an earlier
      phase. Marked **YES**/partial/NO/rare in every table.
- [x] The "needed at emission" set is small enough to list in
      a section. See "What survives" above (~60 distinct
      fields).
- [x] Survey output justifies whether the flatten / minimal-IR
      hypothesis holds. **It holds.**

---

## Recommended next phase

Phase 1 (Distill) is the natural next step. It should:

1. For each backend, enumerate every field read at emission
   time, with line references.
2. Cross-check against this survey's "what survives" list.
3. Identify any field reads NOT in the survival list (would
   indicate the survey missed something).
4. Produce a tight 5-to-10-item "this is what the IR must
   expose" list with the provenance trace.

I can do Phase 1 in a single session, building on this
survey's tables.

---

## Open questions surfaced during the survey

1. **`Sym::var` is the canonical Var for globals** (and
   sometimes for builtin syms). The codegen layer treats
   `sym->var` as the "real" Var. CG_IR should make this
   explicit: per-Sym canonical Var.

2. **`Var::constant`** is populated by DCE when a value is
   provably constant. The LLVM backend uses this for constant
   recovery when getLLVMValue's cache misses. CG_IR should
   model this as "this value has a known compile-time
   constant" attribute, not as a side-channel.

3. **`Fun::fa_all_Vars`** is what `allocate_locals` walks to
   allocate AllocaInsts. CG_IR should provide an explicit
   "locals to allocate" list per CGFun (the current CGFun's
   `locals` field is the right shape but unpopulated in
   production).

4. **`PNode::cfg_pred_index`** is used by `do_phi_nodes` to
   pick the right rval index for SSU phi MOVEs. CG_IR's
   block-level CFG should preserve this — Track 3's
   `emit_phi_phy` re-derives it from PNode at terminator
   time, which is fine for now but could be pre-computed at
   normalization time.

5. **`Prim::index` is the universal dispatch key.** The 16
   structural + ~15 arithmetic = ~30 distinct cases. CG_IR
   could either:
   - Expose all 30 as CG_OPs (broad but rigid)
   - Expose the 5 most common + CG_CALL+prim fallback
     (current approach)
   - Expose categories (struct-op / arith / cgfn) with
     sub-discrimination

   The choice has implications for cross-frontend stability.
