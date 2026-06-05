# IF1 — Intermediate Form (Reference)

A working reference for `ifa/if1/` — the IFA intermediate form. Everything
the rest of the library operates on flows through these types, so understand
this doc before reading any other deep doc.

Sister docs: [IFA.md](IFA.md) consumes this IR; [CLONE.md](CLONE.md)
rewrites it; codegen reads it.

---

## 1. What IF1 is

IF1 is a hierarchical, **symbol-centric** intermediate form. Almost
everything is a `Sym` (variable, constant, function, type, module, even
labels for some purposes). A function body is a tree of `Code` operations
(SEND, MOVE, IF, GOTO, LABEL, plus group nodes SUB/SEQ/CONC). When a
function enters analysis, the tree is lowered into a graph of `PNode`s in
SSU form (see [CFG_SSU.md](CFG_SSU.md) when written), and analysis happens
over `Var × contour → AVar` (see [IFA.md](IFA.md)).

Three rules cover ~90% of the design:

1. **Interning everywhere.** Strings, constants, types, ATypes — pointer
   equality is semantic equality. There is one `IF1` per process
   (`ifa/if1/if1.cc:21`) and it owns all interning tables.
2. **Symbols carry the type.** A `Sym` is *both* the symbol and the type:
   a function's `Sym` has `is_fun` and its arg/return Syms in `has`; a
   class is a `Sym` with `type_kind=Type_RECORD` and its fields in `has`;
   a constant is a `Sym` with `is_constant` and its value in `imm`.
3. **Bit-flags as type discriminator.** `Sym` has ~30 bit-flags
   (`is_fun`, `is_constant`, `is_module`, etc.) plus three multi-bit
   fields (`type_kind`, `num_kind`, `num_index`). Most code branches on
   these rather than on a tagged-union kind.

---

## 2. The `IF1` singleton (`if1.cc` / `if1.h`)

```c
extern IF1 *if1;     // process-wide singleton, created by ifa_init
```

Owns:

| Field | Purpose |
|---|---|
| `strings : StringChainHash<>` | All canonicalised strings. `if1_cannonicalize_string(p, s)` is the entry point — never compare strings except by `cchar*` equality. |
| `symbols : Map<cchar*, Sym*>` | User-defined symbol-kind Syms (`#foo`-style). Populated by `if1_make_symbol`. |
| `builtins : Map<cchar*, Sym*>` | Compiler builtins. Populated by `if1_set_builtin`; looked up by `if1_get_builtin`. |
| `builtins_names : Map<Sym*, cchar*>` | Reverse map; given a builtin Sym, get the name. |
| `constants : HashMap<Immediate*, ImmHashFns, Sym*>` | Constant interning by `Immediate` value. `if1_const(if1, type, str)` looks here first. |
| `allsyms : Vec<Sym*>` | Every Sym ever registered, in insertion order. `if1_register_sym` appends. |
| `alllabels : Vec<Label*>` | Every label, in insertion order. |
| `allclosures : Vec<Sym*>` | Every callable closure (function/lambda/class init). `if1_closure` appends. Iterated by `ifa_analyze` to build `Fun`s. |
| `int_types[5][2]` / `float_types[8]` / `complex_types[8]` | Lookup tables for numeric types by size. Filled by `if1_set_int_type` / `_set_float_type` / `_set_complex_type`. |
| `top : Sym*` | The `__main__` closure. Set by `if1_finalize`. |
| `primitives : Primitives*` | Primitive-call table; see [PRIMITIVES.md](PRIMITIVES.md) when written. |
| `callback : IFACallbacks*` | Frontend hook table. Set by `ifa_init`. See [ARCHITECTURE.md](ARCHITECTURE.md) §IFACallbacks. |
| `pointer_size` / `pointer_alignment` | Defaults from host (`sizeof(void*)`, `__alignof__(void*)`). |
| `partial_default : Partial_kind` | Default for SEND's `partial` field — V uses `Partial_OK`, pyc forces `Partial_NEVER`. |
| `filename : cchar*` | Current file (for diagnostics). |

Critical invariant: re-init is not supported. `new IF1` in `ifa_init`
always creates the singleton; the constructor sets `if1 = this`. If you
construct another `IF1`, the global pointer rebinds and earlier state
becomes unreachable but not GC'd if anything still references it.

---

## 3. The `Sym` (`sym.h` / `sym.cc`)

The most important type in the codebase. A 400+-byte struct that
represents:

- variables (local, global, instance, formal),
- constants (`is_constant`, `imm`),
- user-defined symbols / atoms (`is_symbol`),
- functions / closures (`is_fun`, `code`, `has`),
- types (`type_kind != Type_NONE`),
- modules (`is_module`).

### 3.1 `BasicSym` — the common header (`sym.h:50`)

```c
class BasicSym : public gc {
  int id;                // monotonic counter; NOT stable across clone()
  cchar *name;           // user-level name (interned)
  Sym *in;               // containing module / class / function
  Sym *type;             // type of this symbol (a Sym itself)
  Sym *aspect;           // mascarades as another type (e.g. super())
  Sym *must_specialize;  // dispatch constraint
  Sym *must_implement;   // type-checking constraint
  IFAAST *ast;           // back to the frontend AST node
  Var *var;              // FA's view (set when first AVar is made)
  IFASymbol *asymbol;    // back to frontend symbol object
  int nesting_depth;     // 0 = global; LOCALLY_NESTED (-1) = TBD; >0 = nested
  cchar *cg_string;      // codegen-emitted C name
  llvm::Value *llvm_value;
  // ... ~30 bit-flags + type_kind (4 bits) + num_kind (3) + num_index (3)
};
```

### 3.2 The flag soup

Most of `Sym`'s personality lives in single-bit flags. Common ones:

| Flag | Meaning |
|---|---|
| `is_builtin` | Built into the compiler (not from user source). |
| `is_constant` | Holds an immediate value in `imm` and `constant` (string repr). |
| `is_lvalue` | Can appear on the left of an assignment. |
| `is_local` | Local variable; eligible for SSU conversion. |
| `is_module` | A module. |
| `is_fun` | A function/closure (`code` is its body, `has` are arguments). |
| `is_symbol` | A user-level atom (`#foo` / `'foo`). |
| `is_pattern` | A pattern used in dispatch. |
| `is_rest` | A rest/`*args` parameter. |
| `is_generic` | A generic type parameter. |
| `is_external` | External-declared (treat as opaque type). |
| `is_this` | The implicit method receiver. |
| `is_fake` | Synthesized; not needed at runtime. |
| `is_meta_type` | Type of a type. |
| `is_unique_type` | Single-instance type (e.g. `nil_type`). |
| `is_value_type` | Pass-by-value. |
| `is_system_type` | Don't add to the user hierarchy. |
| `is_union_type` | Union type. |
| `is_structure` | C-compatible structure. |
| `is_vector` | Homogeneous; only `element` matters. |
| `is_default_arg` | Default argument. |
| `is_exact_match` | `must_specialize/implement` is exact (not LUB). |
| `fun_returns_value` | Set by analysis when a fn returns a value (vs `void`). |
| `live` | Set by `if1_simple_dead_code_elimination` (`if1.cc:656`). |
| `clone_for_constants` | Analysis should try to make this constant. |
| `type_live` | Set during type-DCE. |
| `dispatch_types_built` | C3-linearization cache flag. |

`intent` (2 bits): `Sym_IN` / `Sym_INOUT` / `Sym_OUT`. The
`is_Sym_OUT(s)` macro tests output parameters.

### 3.3 `Type_kind` (`sym.h:27`)

```
Type_NONE         // Sym is not a type (variable, constant, function...)
Type_UNKNOWN      // forward-declared or type-parameter
Type_SUM          // union — has[] are the alternatives
Type_RECORD       // struct/class — has[] are the fields
Type_FUN          // function type
Type_REF          // pointer (not currently used)
Type_TAGGED       // variant — tag + type
Type_PRIMITIVE    // int/float/etc.
Type_APPLICATION  // application of a Type_ALIAS with args
Type_VARIABLE     // type variable
Type_ALIAS        // alias for another type (resolve with unalias_type)
```

`num_kind` (3 bits) and `num_index` (3 bits) further specialise primitives:

```
num_kind:   IF1_NUM_KIND_NONE / _UINT / _INT / _FLOAT / _COMPLEX
            + IF1_CONST_KIND_STRING / _SYMBOL (overflow of num_kind enum)
num_index:  IF1_INT_TYPE_{1,8,16,32,64}  for int/uint
            IF1_FLOAT_TYPE_{16,32,48,64,80,96,112,128}  for float/complex
```

### 3.4 The full `Sym` extras (`sym.h:110`)

On top of `BasicSym`, `Sym` adds:

```c
cchar *constant;   // string form of constant (interned)
Immediate imm;     // immediate value (see num.h)
unsigned size, alignment;  // for layout

Scope *scope;      // ast.cpp: function/type/module scopes
LabelMap *labelmap;

Vec<Sym *> generic_args;

Vec<Sym *> has;           // members/fields/args (for *fun* and *type*)
Vec<cchar *> has_names;   // optional tuple-field names

Fun *fun;                 // FA's Fun for this *fun* sym
Code *code;               // function body (for *fun*)
Sym *self, *ret, *cont;   // self / return-value / continuation

Sym *instantiates;
Map<Sym *, Sym *> substitutions;

MType *match_type;           // pattern.cpp
AType *abstract_type;        // FA's view of this type
Vec<CreationSet *> creators; // every CS that produces this *type*

Vec<Sym *> specializes;      // declared superclasses
Vec<Sym *> includes;         // mixin includes
Vec<Sym *> implements;       // declared supertypes
Vec<Sym *> isa;              // transitive SUM members

Sym *alias;                  // for Type_ALIAS
Sym *init;                   // module/class init function
Sym *meta_type;              // meta-type (class-of-class); inverse pointer
Sym *element;                // element type for vectors/aggregates

Vec<Sym *> implementors;     // computed by build_type_hierarchy
Vec<Sym *> specializers;     // computed by build_type_hierarchy
Vec<Sym *> dispatch_types;   // C3-linearised dispatch order

void *temp;                  // algorithmic scratch
llvm::Type *llvm_type;
```

### 3.5 Type-hierarchy bookkeeping (`ast.cc:392`)

`build_type_hierarchy()`:

1. Walks every type Sym since `type_hierarchy_built`.
2. For each, propagates `implements`/`specializes`/`includes` into the
   `implementors`/`specializers` sets on its supertypes.
3. Adds `sym_any` / `sym_value` / `sym_anytype` as universal supers
   (unless `is_system_type`).
4. Lifts subtyping/subclassing to the meta-type layer too.
5. Runs **C3 linearisation** (`c3_linearization`) per class to populate
   `dispatch_types` (the Dylan/Python MRO algorithm).

The cache pointer `type_hierarchy_built` records how many `allsyms` have
been processed; subsequent invocations are incremental.

### 3.6 `finalize_types` (`ast.cc:594`)

Called by the frontend after each batch of new Syms / types. Asserts every
builtin in `builtin_symbols.h` is registered, unaliases all newly-added
Syms, propagates includes-of-includes, lifts `is_value_type` through
implementation, computes `isa` closure for `Type_SUM`, sets
`type/meta_type` on every new type Sym, fixes `nesting_depth`, and
computes `size`/`alignment`.

After finalize, you can read `s->type`, `s->meta_type`, `s->size`,
`s->alignment` reliably.

### 3.7 Common Sym helpers

- `unalias_type(s)` — follow `Type_ALIAS` chains until a real type. Detects
  cycles via `Vec::set_in`.
- `s->scalar_type()` — return `s->type` if `num_kind`; else NULL.
- `s->coerce_to(t)` — numeric coerce, or scalar→string.
- `s->imm_int(&out)` — extract integer immediate.
- `s->inherits_add(ss)` — add `ss` to all three of
  `implements`/`specializes`/`includes`.
- `s->must_implement_and_specialize(ss)` — set both constraint pointers.
- `s->copy()` — shallow copy + new id.
- `s->clone()` — copy via `asymbol->clone()` if the frontend has one,
  else `copy()`.
- `imm_constant(imm, t)` / `int32_constant(n)` / `int64_constant(n)` /
  `size_constant(n)` — produce interned constant Syms.

---

## 4. `Code` (`code.h`)

The pre-CFG IR. A `Code` is one of:

```
Code_SUB     // anonymous group — children in `sub`
Code_MOVE    // rvals[0] → lvals[0]   (single assignment)
Code_SEND    // call: rvals are args, lvals are results
Code_IF      // if rvals[0] goto label[0]; else fall through to label[1]
Code_LABEL   // declares label[0]
Code_GOTO    // jump to label[0]
Code_SEQ     // sequential group (children in `sub`)
Code_CONC    // concurrent group (children in `sub`)  — CONC_IMPLEMENTED off by default
Code_NOP
```

`is_group()` is true for SUB/SEQ/CONC.

Fields:
- `rvals : Vec<Sym*>` — read operands.
- `lvals : Vec<Sym*>` — written operands.
- `names : Vec<cchar*>` — optional rval names (for keyword args on SEND).
- `label[2]` — for IF: `[0]`=then, `[1]`=else; for LABEL/GOTO: `[0]`=this.
- `sub : Vec<Code*>` — children for group nodes.
- `ast : IFAAST*` — back to frontend AST for diagnostics/cloning.
- `prim : Prim*` — filled in by `if1_finalize` for primitive sends.
- `partial : 2` — `Partial_OK` / `Partial_NEVER` / `Partial_ALWAYS`
  (whether the call may take incomplete arguments).
- `live : 1` — set by IF1 DCE.
- `flattened : 1` — set by `if1_flatten_code` to prevent revisit.
- `cont : Code*` / `pn : PNode*` — wiring set by CFG construction.

The constructor `Code(Code_kind k = Code_SUB)` zeros the whole struct
then sets `kind`. The copy constructor copies everything (including
`cont`/`pn`, which is usually wrong — only `Fun::copy()` callers should
duplicate Code).

### 4.1 The `if1_*` builders (`if1.cc`)

The frontend builds Code trees by calling these. Most take a `Code **c`
out-parameter and append into `(*c)->sub`:

| Builder | Effect |
|---|---|
| `if1_gen(p, &c, cc)` | Append `cc` to `c`'s sub-list. |
| `if1_seq(p, &c, cc)` | Wrap `cc` in a `Code_SEQ` group, append. |
| `if1_conc(p, &c, cc)` | Wrap `cc` in a `Code_CONC` group, append. |
| `if1_move(p, &c, a, b)` | `b = a`. |
| `if1_send(p, &c, args, results, ...)` | Variadic SEND: positional rvals + lvals. |
| `if1_send1(p, &c, ast)` | Empty SEND; use `if1_add_send_arg`/`if1_add_send_result` to fill. |
| `if1_add_send_arg(p, c, a, name)` | Append rval; sets `names[i]` if keyword. |
| `if1_add_send_result(p, c, r)` | Append lval. |
| `if1_alloc_label(p)` / `if1_label(p, &c, ast, l)` | Allocate / emit a LABEL. |
| `if1_goto(p, &c, label)` / `if1_set_goto(p, g, label)` | Emit / patch GOTO. |
| `if1_if_goto(p, &c, cond, ast)` | Emit IF; patch with `if1_if_label_true/false`. |
| `if1_if(p, &c, cond, condvar, then, thenvar, else, elsevar, result, ast)` | High-level if-then-else builder. |
| `if1_loop(p, &c, cont, brk, condvar, before, cond, after, body, ast)` | High-level loop builder; handles do/while via `before==body`. |
| `if1_operator(p, &c, a1, a2, a3)` | SEND with up to 3 args; returns a fresh result Sym. |
| `if1_closure(p, f, code, nargs, args)` | Register `f` as a callable closure with body `code`. |
| `if1_finalize_closure(p, c)` | Per-closure DCE + flatten + nesting fixup. |
| `if1_const(p, type, str, imm, asym)` | Look-up-or-create a constant Sym. |
| `if1_make_symbol(p, name, end)` | Look-up-or-create a `is_symbol` Sym. |
| `if1_set_builtin(p, s, name, end)` | Mark `s` as a builtin under `name`. |
| `if1_register_sym(p, s, name)` | Assign id, add to `allsyms`, intern name. |

### 4.2 `if1_finalize` (`if1.cc:755`)

Called once after the frontend is done building. Sets `if1->top`,
resolves `Prim *` on every SEND via `Primitives::find`, runs whole-program
DCE if `fdce_if1` (default true), then for each closure flattens nested
SEQ/CONC chains and fixes up any `LOCALLY_NESTED` Syms.

The DCE pass (`if1_simple_dead_code_elimination`):
1. Reset all `live` bits.
2. Seed: every Sym with `nesting_depth == 0` or `asymbol` is live.
3. Seed: every closure + its `ret` + its arg Syms is live.
4. Iterate `mark_code_live` on each closure body until no `live` bit
   flips.
5. Iterate `mark_live` until no Sym `live` bit flips.
6. `mark_dead` on each closure body to clear unreached MOVE/SEND `live`s.

### 4.3 `Label` (`code.h:68`)

```
class Label : public gc {
  int id;
  unsigned int live : 1;
  union {
    Code *code;            // cfg.cc points back to the LABEL Code
    llvm::BasicBlock *bb;  // llvm codegen overlays the bb
  };
};
```

The union is **not** safe to use both ways simultaneously. The CFG pass
uses `code`; the LLVM backend overlays `bb` (after CFG is no longer
referenced via the label).

---

## 5. `PNode` (`pnode.h` / `pnode.cc`)

The post-CFG node. One `PNode` per executable `Code` (MOVE / SEND / IF /
LABEL / GOTO; group nodes don't become PNodes). Built by
`Fun::build_cfg()` via `cfg.cc:build_pn_cfg`.

```c
class PNode : public gc {
  Code *code;
  int id;                          // stable across passes
  uint live : 1;                   // set by mark_live_code
  uint fa_live : 1;                // set by FA when reached
  Vec<Var *> lvals, rvals, tvals;  // tvals = temporaries owned by this node
  int mark;                        // ssu.cc temp
  Vec<PNode *> cfg_succ, cfg_pred; // CFG edges
  Vec<PNode *> phi;                // MOVE PNodes that logically follow
  Vec<PNode *> phy;                // MOVE PNodes that logically precede
  Prim *prim;                      // copy of code->prim
  union {                          // overlaid scratch
    LoopNode *loop_node;           //   loop.cc
    BlockHash<Var*,...> *live_vars;//   ssu.cc
  };
  Map<PNode *, int> cfg_pred_index;// cg.cc
  Dom *dom, *rdom;                 // dominators / reverse-dominators
  Vec<Sym *> *creates;             // cloning: types this node allocates
  float execution_frequency;
  float false_branch_frequency;
};
```

Two constructors:
- `PNode()` — zero-init; used by phi/phy synth and by `Fun::copy()`.
- `PNode(Code *c)` — wraps a Code, pulling its rvals/lvals into `Var`s
  (creating new `Var(s)` for any rval/lval Sym that doesn't have one yet).

**The `phi`/`phy` distinction.** `phi` are MOVE PNodes that *logically
run after* this PNode at confluence points; `phy` are MOVE PNodes that
*logically precede* it. Together they form IFA's SSU (Single Static Use)
treatment — symmetric in defs and uses. See [CFG_SSU.md](CFG_SSU.md)
(when written) for details.

### 5.1 `Var` (`var.h` / `var.cc`)

A per-`Sym` usage record:

```c
class Var : public gc {
  Sym *sym;
  int id;
  Sym *type;             // assigned during cloning
  int mark;              // ssu.cc temp
  PNode *def;            // where this Var is defined
  Vec<PNode *> uses;     // PNodes that read this Var
  AVarMap avars;         // FA: contour → AVar
  CreationSet *as_CreationSet;
  unsigned is_internal : 1;
  unsigned is_filtered : 1;   // FA: synthesised filter Var
  unsigned is_formal : 1;
  unsigned live : 1;          // DCE result
  Sym *constant;              // post-DCE: known-constant value
  cchar *cg_string;
  llvm::Value *llvm_value;
  llvm::Type *llvm_type;
  llvm::DILocalVariable *llvm_debug_var;
  union { SSUVar *ssu; };
};
```

One `Var` per `Sym` usage. `Sym::var` holds the canonical Var for that
Sym (set lazily by `PNode(Code*)` when first needed). `Var::avars` is the
per-contour map used by IFA — `make_AVar(v, contour)` indexes into it.

`Var::copy()` makes a fresh Var with the same Sym (used by `Fun::copy`
when duplicating a function body).

---

## 6. `Fun` (`fun.h` / `fun.cc`)

A specialisation of a closure. There may be multiple `Fun`s per closure
Sym after cloning. The constructor `Fun(Sym *)` (`fun.cc:54`) is where
CFG and SSU get built:

```c
Fun::Fun(Sym *asym) {
  sym = asym; asym->fun = this; ast = sym->ast;
  init_fun();
  build_cfg();        // ifa/optimize/cfg.cc
  build_ssu();        // ifa/optimize/ssu.cc
  build_uses(this);
  setup_ast();
  check_invariants(this);
}
```

So `new Fun(closure)` in `ifa_analyze` (`ifa.cc:29`) does serious work,
not just allocation.

### 6.1 Fields by section

**Identity:** `id`, `sym`, `ast`, `nested_in`, `nested`.

**PNodes:** `entry`, `exit` (the only canonical PNode pointers; everything
else is reached by walking `cfg_pred` from `exit`).

**Flags:** `is_generic`, `is_external`, `is_varargs`, `is_eager` (will
evaluate for `Partial_OK`), `is_lazy` (will not match `Partial_NEVER`).

**FA fields:** `fa_collected` flag, `clone_for_constants`, `split_unique`,
`split_eager`, plus per-Fun caches of the analysis state:
- `ess : Vec<EntrySet*>` — specialisations of this Fun (see IFA.md).
- `fa_Vars / fa_all_Vars / fa_all_PNodes / fa_move_PNodes /
  fa_if_PNodes / fa_phi_PNodes / fa_phy_PNodes / fa_send_PNodes` —
  sorted-by-id slices for efficient iteration. Populated by
  `fa.cc:collect_Vars_PNodes`.

**Pattern (dispatch):**
- `arg_positions / positional_arg_positions` — `MPosition*` for each arg.
- `named_to_positional` — maps keyword args to positional.
- `arg_syms / args` — per-position Sym and Var.
- `rets / out_positions` — return slots.
- `default_args` — defaulted positions.
- `generic_args` — generic positions.
- `promotion_cache / coercion_cache / generic_cache / order_cache /
  default_cache` — dispatch result caches (see DISPATCH.md when written).

**Clone fields:**
- `called_ess / called_by_ess / called_css` — populated by
  `clone.cc:initialize`.
- `equiv_sets` — partition of `ess` after `determine_clones`.
- `nmap : Map<PNode*, PNode*>*` — old→new PNode map (for cloned bodies).
- `vmap : Map<Var*, Var*>*` — old→new Var map.
- `fmap : Map<Fun*, Fun*>*` — old→new nested-Fun map.

**Call graph (post-clone):** `calls : Map<PNode*, Vec<Fun*>*>`,
`called : Vec<CallPoint*>`. Built at the end of `clone_functions`.

**Wrappers / instantiations:** `wraps` — when this Fun is a wrapper
around another (default-arg supplier, coercion shim, etc.).

**Loop / dom:** `loops : LoopGraph*`, `loop_node : LoopNode*`,
`dom : Dom*`.

**Inline:** `execution_frequency`, `size`.

**Codegen:** `live`, `cg_string`, `cg_structural_string`,
`llvm : llvm::Function*`.

### 6.2 Key methods

- `collect_PNodes(v)` — reverse-BFS from `exit` via `cfg_pred`.
- `collect_Vars(v, &nodes, flags)` — same plus arg/ret Vars; flags can
  suppress `tvals` / `phy` walk.
- `calls_funs(funs)` — union of `calls.values()` as a set.
- `called_by_funs(by)` — derived from `called`.
- `Fun::copy(copy_ast, var_map)` — deep-copy PNodes, Vars, args, rets;
  builds `fmap/nmap/vmap`. Used by `clone_functions` when a Fun has
  multiple equivalence classes.

`CallPoint` is a `(Fun*, PNode*)` pair used in the inverse call graph.

`check_invariants(f)` (release-mode only; under `#ifndef DEBUG` it's a
no-op) verifies `cfg_succ`/`cfg_pred` symmetry and that forward and
reverse BFS reach the same set of PNodes.

`rebuild_cfg_pred_index(f)` rebuilds the per-PNode `cfg_pred_index` map;
called by codegen after any CFG manipulation.

---

## 7. `Immediate` and the numeric tables (`num.h` / `num.cc`)

`Immediate` is a 16-byte tagged-union holding a constant value:

```c
class Immediate : public gc {
  unsigned const_kind : 4;   // IF1_NUM_KIND_* or IF1_CONST_KIND_*
  unsigned num_index  : 3;   // IF1_INT_TYPE_* or IF1_FLOAT_TYPE_*
  union {
    bool, int8/16/32/64, uint8/16/32/64,
    float32/64/128, complex32/64, cchar *v_string;
  };
};
```

Constants are interned in `IF1::constants` keyed by `Immediate*` value
(memcmp-equality). Two `if1_const` calls with the same `Immediate` return
the same Sym.

`Immediate` helpers:
- `set_int/uint/float/bool` — typed setters.
- `int_value()` / `uint_value()` — extract by `num_index`.
- `int_size()` — bits.
- `operator=(const Immediate&)` — memcpy assign.
- `ImmHashFns` — hash by byte content; equal by memcmp.

Numeric type tables:
- `int_type_precision[5] = {1, 8, 16, 32, 64}`
- `float_type_precision[8] = {16, 32, 48, 64, 80, 96, 112, 128}`
- `num_kind_string[4][8]` — printable names.

Constant folding lives in `fold_constant(op, im1, im2, &out)`
(`num.cc:fold_constant`). It dispatches on the `P_prim_*` opcode and uses
`DO_FOLD` macros that expand into per-type C arithmetic. `coerce_immediate`
widens to a common type before folding.

Companion functions:
- `sprint_imm(buf, size, imm)` / `fprint_imm(fp, imm)` — print.
- `convert_string_to_immediate(str, imm)` — parse based on
  `imm->const_kind` / `num_index`.

---

## 8. Builtin Syms (`builtin.h`, `builtin_symbols.h`, `ast.cc`)

`builtin_symbols.h` is the master list. It uses an `S(name)` macro
expanded twice:

- In `builtin.h`, to declare `extern Sym *sym_##name`.
- In `if1.cc`, to populate `builtin_strings[]`.
- In `builtin.h` again, to populate `enum Builtin { Builtin_##name }`.

Every entry must have a backing global `Sym *sym_##name` somewhere
(usually defined in `builtin.cc`). The frontend's `build_builtin_symbols`
(in pyc, `python_ifa_main.cc`'s `build_environment`) populates them.

After all builtins are registered, `finalize_types(if1)` asserts each
`sym_##name` is non-null. If you add a new builtin, add it to
`builtin_symbols.h` AND wire it up in the frontend's
`build_environment` (or the V frontend's equivalent).

Common builtins to know:
- `sym_any` — top of the type lattice.
- `sym_void`, `sym_void_type` — unit / its type.
- `sym_anyint`, `sym_anynum`, `sym_anyfloat`, `sym_anycomplex` — numeric
  super-types.
- `sym_int{8,16,32,64}`, `sym_uint{8,16,32,64}`, `sym_float{32,64,128}`,
  `sym_complex{32,64,128}` — concrete numerics.
- `sym_bool`, `sym_true`, `sym_false`, `sym_size`, `sym_char`,
  `sym_string`, `sym_symbol`.
- `sym_nil`, `sym_nil_type`, `sym_empty_list`, `sym_empty_tuple`.
- `sym_list`, `sym_tuple`, `sym_vector`, `sym_set`, `sym_ref`,
  `sym_closure`, `sym_continuation`, `sym_function`.
- `sym_primitive` — the prefix for compiler primitives.
- `sym___main__` — the top-level entry closure.
- Plus the `P_prim_*` table-driven primitives (see `prim_data.h`).

---

## 9. The `AST` layer (`ast.h` / `ast.cc`)

Despite the name, `ifa/if1/ast.cc` is *not* the parser AST — that's in
`frontend/`. This file builds out the **type hierarchy** from
already-constructed Syms.

Functions to know:
- `init_ast(callbacks)` — called by `ifa_init`; sets
  `if1->callback` and registers built-in primitive types via
  `init_builtin_symbols`.
- `new_module(sym, init_fun)` — set up a module Sym.
- `new_builtin_symbol/primitive_type/alias_type/global_variable/unique_object/lub_type`
  — create-or-register helpers used by the frontend.
- `unalias_sym(s)` — collapse `Type_ALIAS` indirections on a Sym (and its
  super-/sub-type Vecs).
- `make_meta_type(s)` — allocate `s->meta_type` if missing.
- `finalize_types(if1, import_included_ivars)` — see §3.6.
- `build_type_hierarchy(compute_structural_value_hierarchy)` — see §3.5.

`ASTCopyContext` holds `(fmap, nmap, vmap)` for `IFAAST::copy_tree` /
`copy_node`. The frontend AST classes implement those virtuals using the
context to translate old→new pointers when a Fun is cloned.

---

## 10. Lifecycle: Sym, Var, AVar, PNode

A reading guide for "when does X exist?":

```
Frontend (build_syms):
  new Sym → is_local / is_fun / is_module / type_kind set
            → in / type / nesting_depth / has populated
            → registered via if1_register_sym

Frontend (build_if1):
  if1_send / _move / _if / ... → Code nodes
  if1_closure(f, code, ...)    → f added to allclosures

if1_finalize:
  Prim* attached to each SEND Code
  DCE marks `live`
  Code trees flattened

new Fun(closure):
  build_cfg → Code → PNode graph; phi/phy nodes inserted via build_ssu
  PNode(Code*) auto-creates Var for any unbound Sym
  build_uses populates Var::uses

FA::analyze:
  per-Var, per-contour AVar allocated via make_AVar
  Var::avars[contour] = AVar
  Sym::var canonicalised to the first Var

clone():
  Fun::copy() may create new Funs, mapping via fmap/nmap/vmap
  cs->type / av->type / Var::type assigned
  Sym::has[i] possibly cloned for record types
  Fun::calls / Fun::called rebuilt

mark_live_*:
  PNode::live, Var::live, Code::live, Sym::live set

codegen:
  Sym::cg_string, Var::cg_string assigned and emitted
```

After `clone()`, `Sym::id` is no longer a stable identifier — fresh
clones get new ids. Use `PNode::id` or pointer identity for caches that
must survive cloning.

---

## 11. Pretty-printers (debugger entry points)

All defined at the bottom of their respective files:
- `pp(Sym*)` in `sym.cc:403` — one-line dump.
- `pp(Var*)` in `var.cc:33`.
- `pp(PNode*)` in `pnode.cc:41`.
- `pp(Code*)` in `if1.cc`.
- `pp(Immediate&)` in `if1.cc:858` — returns int char count.

Plus high-volume dumpers:
- `if1_dump_sym(fp, s)` / `if1_dump_code(fp, code, indent)` /
  `if1_dump(fp, code)` — multi-line dumps.
- `print_code(fp, code, indent, lf)` — for the textual IF1 file
  (`ifa_code` invocation).
- `print_syms(fp, syms, start)` — dump only live Syms from a range.
- `if1_write(fp, p, start)` — public entry for textual IF1.
- `if1_write_log()` — auto-dump to the `alog` log if
  `alog.test.if1` is enabled.

---

## 12. Common gotchas

### 12.1 The `Code::cont`/`Code::pn` back-pointers
`Code` has `cont : Code *` and `pn : PNode *` fields used by CFG
construction. The `Code` copy constructor copies both. If you copy a
Code that's already part of a built CFG, you'll get a duplicate PNode
back-pointer — that's almost never what you want. Only `Fun::copy` is
expected to invoke `Code(Code&)` paths, and it goes through
`copy_pnode` which re-wires.

### 12.2 Sym `var` field
`Sym::var` is set lazily by `PNode(Code*)` to the first `Var(s)` made
for `s`. If you create Vars for the same Sym in multiple Funs *before*
any PNode wraps it (rare but possible), `Sym::var` won't be the one you
expect.

### 12.3 `nesting_depth` triage
`0` means global; `LOCALLY_NESTED` (`-1`) means "to be assigned";
positive values are nested-function depth. `if1_fixup_nesting` runs at
finalize to bump LOCALLY_NESTED Syms to `f->nesting_depth + 1`. If a Sym
arrives at analysis still LOCALLY_NESTED, it didn't go through a closure
body — likely a bug in the frontend.

### 12.4 Constant interning by `Immediate`
Two constants with the same byte representation collapse to one Sym.
That's almost always what you want, except for cases like
`int32 1` vs `int64 1` — those don't collapse only because
`(const_kind, num_index)` differ. If you create constants programmatically
and want distinct Syms, you need distinct `Immediate` byte patterns.

### 12.5 `type_kind_string[]` length mismatch
`sym.cc:7` defines `type_kind_string[]` with 12 entries including names
("VECTOR", "ENUM") that don't appear in the `Type_kind` enum (`sym.h:27`,
which has 11). Don't trust indexing by `Type_kind` value to give the
right string in all versions of the source; cross-check the enum.

### 12.6 `Label::live` is set by IF1 DCE only
The post-CFG live analysis (`optimize/dead.cc`) sets `PNode::live` and
`Var::live`, but `Label::live` reflects only the *IF1*-level liveness
from `if1_simple_dead_code_elimination`. Don't conflate them.

### 12.7 `fdce_if1`
A global bool defaulting to `true` (`if1.cc:22`). `ifa_analyze` fails if
it's false (`ifa.cc:28: fail("unable to translate dead code")`). The
switch exists for debugging but the rest of the pipeline assumes DCE
ran.

### 12.8 Multiple Syms for one frontend symbol
A frontend symbol (e.g. a Python variable) can correspond to multiple
IF1 Syms over its lifetime (clones during analysis, wrapper Funs for
defaults/coercion). The frontend interface objects (`IFASymbol`,
`PycSymbol`) are the stable identity; `Sym` is the analysis-time
identity.

---

## 13. Where to read first when something's wrong

| Symptom | Start here |
|---|---|
| "missing symbol" / "unknown name" | frontend `build_syms`; check `IF1::symbols` / `IF1::builtins` |
| "wrong type assigned to a var" | `clone.cc:concretize_var_type` (see [CLONE.md](CLONE.md)) |
| "constant not interned" | `if1_const` — verify the `Immediate` byte pattern is identical |
| "stale builtin Sym pointer after rebuild" | a global `sym_*` was not re-initialised by `build_environment` |
| "MOVE/SEND silently dropped" | DCE marked it dead; check `Code::live` in IR dump |
| "wrong call dispatch" | `Sym::dispatch_types` (C3-linearisation in `ast.cc:c3_linearization`) or pattern.cc |
| "type appears in `allsyms` but with no name / has / type" | post-`Fun::copy()` clone with deferred fields; consumers must not rely on `name` |
| "CFG looks wrong" | the `Fun(Sym*)` ctor or `cfg.cc:build_pn_cfg` — see [CFG_SSU.md](CFG_SSU.md) when written |
| "constant folding produced wrong sign" | `num.cc:fold_constant` macros (`DO_FOLD` vs `DO_FOLDI` vs `DO_FOLDF`) — check `op` switch routes to the right macro |
| "infinite loop in `unalias_type`" | circular `Type_ALIAS`; `unalias_type` calls `fail("circular type alias")` on detect, but only after the cycle |

---

## 14. References

- `ifa/if1/if1.cc` — IF1 builders.
- `ifa/if1/sym.cc` — Sym implementation.
- `ifa/if1/ast.cc` — type hierarchy + finalize.
- `ifa/if1/num.cc` — Immediate, constant folding.
- `ifa/if1/{pnode,var,fun}.cc` — analysis-side IR.
- Code-level sister docs: [IFA.md](IFA.md), [CLONE.md](CLONE.md),
  [ARCHITECTURE.md](ARCHITECTURE.md).
- For *how* the V or Python frontends *build* this IR: `FRONTEND.md` /
  `PYTHON_FRONTEND.md` (TODO).
