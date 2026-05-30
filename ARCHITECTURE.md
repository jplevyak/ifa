# IFA Library — Architecture

A map of the IFA library's internal structure. Read this before descending
into any specific subsystem. For the end-to-end compilation flow (including
the pyc Python frontend on top), see [../PIPELINE.md](../PIPELINE.md).

For deep dives, see the per-subsystem docs:

| Subsystem | Doc | State |
|---|---|---|
| IF1 IR (Sym/Code/PNode/Fun/Var/num) | `IR.md` | TODO |
| Pattern matching / dispatch | `DISPATCH.md` | TODO |
| Primitives | `PRIMITIVES.md` | TODO |
| Flow analysis | [`IFA.md`](IFA.md) | done |
| Cloning | [`CLONE.md`](CLONE.md) | done |
| CFG / SSU / dominators / loops | `CFG_SSU.md` | TODO |
| Dead code / inlining / freq | `OPTIMIZE.md` | TODO |
| V-language frontend | `FRONTEND.md` | TODO |
| C codegen | `CODEGEN_C.md` | TODO |
| LLVM codegen | `CODEGEN_LLVM.md` | TODO |
| Casts / numeric coercion | `CAST.md` | TODO |
| Common (Vec/Map/log/...) | `COMMON.md` | TODO |
| IR serialization (historical) | [`LLVM.md`](LLVM.md) | stub / redirect |

---

## Directory map

```
ifa/
├── ifa.cc / ifa.h            ← top-level orchestration + IFACallbacks interface
├── main.cc                   ← stand-alone `ifa` CLI (for V tests; pyc has its own)
├── ifadefs.h                 ← common types/macros pulled in everywhere
│
├── common/                   ← plib-style utilities (Vec, Map, log, arg, ...)
│
├── frontend/                 ← V-language frontend (legacy but still the IFA test bed)
│   ├── parse.{cc,h}          ← dparser scaffolding; FrontEnd table
│   ├── parse_structs.h
│   ├── make_ast.{cc,h}       ← ParseAST construction
│   ├── ast_kinds.h           ← AST_* enum
│   ├── ast_to_if1.{cc,h}     ← V AST → IF1 lowering (1876 lines, the big one)
│   ├── scope.{cc,h}          ← lexical scope chain
│   ├── v.g  / v.g.d_parser.cc       ← V language grammar
│   ├── python.g / python.g.d_parser.cc ← (duplicated for tests; root uses its own)
│   └── c.g                   ← C dparser (for the cast generator)
│
├── if1/                      ← IF1 intermediate form + IR construction
│   ├── if1.{cc,h}            ← IF1 owner: allsyms, allclosures, primitives, builders
│   ├── sym.{cc,h}            ← Sym + BasicSym + Type_kind
│   ├── code.h                ← Code IR node (rvals/lvals/labels/sub)
│   ├── pnode.{cc,h}          ← PNode (CFG/SSU node)
│   ├── fun.{cc,h}            ← Fun (function/closure); ctor auto-builds CFG+SSU
│   ├── var.{cc,h}            ← Var (per-Sym usage)
│   ├── num.{cc,h}            ← Immediate, IF1_num_kind, numeric type tables
│   ├── prim.{cc,h}           ← Primitives, Prim, RegisteredPrim, PrimType
│   ├── prim_data.{cc,h,dat}  ← generated primitive metadata
│   ├── pattern.{cc,h}        ← pattern matching + Match + MPosition (1568 lines)
│   ├── ast.{cc,h}            ← low-level AST plumbing
│   ├── builtin.{cc,h}        ← builtin Sym registration
│   ├── builtin_symbols.h     ← S(...) macro list of builtins
│   ├── cast_code.cc          ← generated coercion table (827 lines)
│   ├── make_cast_code.cc     ← generator for above
│   └── check_cast.cc         ← coercion-table validator
│
├── analysis/                 ← whole-program flow analysis + cloning
│   ├── fa.{cc,h}             ← IFA flow analysis (4034 lines)             [IFA.md]
│   ├── clone.{cc,h}          ← Type-directed cloning (1031 lines)         [CLONE.md]
│   ├── pdb.{cc,h}            ← PDB (program database) singleton
│   ├── cdb.{cc,h}            ← CDB (compilation database) — DORMANT
│   ├── graph.{cc,h}          ← GraphViz/VCG visualisation
│   └── ifalog.{cc,h}         ← analysis-specific logging
│
├── optimize/                 ← lowering & optimisation passes
│   ├── cfg.cc                ← build_cfg (Code → PNode graph)
│   ├── ssu.{cc,h}            ← build_ssu (phi/phy insertion)
│   ├── dom.{cc,h}            ← dominator construction
│   ├── loop.{cc,h}           ← loop detection (Sreedhar-Gao-Lee variant)
│   ├── dead.{cc,h}           ← mark_live_code / _types / _funs (DCE)
│   └── inline.{cc,h}         ← simple_inlining + frequency_estimation
│
└── codegen/                  ← backends
    ├── cg.{cc,h}             ← C codegen (995 lines)
    ├── llvm.{cc,h}           ← LLVM driver (1589 lines)
    ├── llvm_codegen.cc       ← per-Fun/PNode LLVM IR
    ├── llvm_primitives.cc    ← per-primitive LLVM impls
    ├── llvm_internal.h
    └── make_prims.cc         ← primitive-table generator (build time)
```

---

## The `ifa_*` orchestration sequence

The library exports a tiny C-style API in `ifa.h`. Drivers (the `ifa` CLI in
`main.cc`, the `pyc` driver in `pyc.cc`) call these in order. Source:
`ifa/ifa.cc`.

```c
void ifa_init(IFACallbacks *callbacks);   // create IF1 + PDB; install callbacks
int  ifa_analyze(cchar *fn);              // finalize IF1, build Funs, FA::analyze, clone, dom, DCE, freq
int  ifa_optimize();                      // live-funs + simple inlining + re-mark live
void ifa_cg(cchar *fn);                   // emit C
void ifa_compile(cchar *fn);              // shell out to system C compiler
// Optional / debug
void ifa_graph(cchar *fn);
void ifa_html(cchar *fn, cchar *mktree_dir);
void ifa_code(cchar *fn);                 // dump IF1 textual form
```

`ifa_analyze` is where the heavy lifting happens. The current body
(`ifa.cc:23-43`):

```c
int ifa_analyze(cchar *fn) {
  if1_finalize(if1);                      // close types, finalize symbols
  if1_write_log();
  if (!fdce_if1) fail("unable to translate dead code");
  for (Sym *c : if1->allclosures) {       // one Fun per closure
    Fun *f = new Fun(c);                  // ← builds CFG + SSU in the ctor
    pdb->add(f);
  }
  FA *fa = pdb->fa;
  fa->fn = fn;
  if (fa->analyze(if1->top->fun) < 0) return -1;        // → IFA.md
  if (clone(fa) < 0) return -1;                          // → CLONE.md
  for (Fun *f : fa->funs) build_cfg_dominators(f);       // → CFG_SSU.md
  if (mark_live_code(fa) < 0) return -1;                 // → OPTIMIZE.md
  if (alog.test.fa > 0) log_test_fa(fa);
  frequency_estimation(fa);                              // → OPTIMIZE.md
  return 0;
}
```

This is the single most important function in the library — every phase
ordering decision lives here. If you reorder it, update both this doc and
`PIPELINE.md`.

---

## The `IFACallbacks` boundary

`IFACallbacks` (in `ifa/ifa.h:58`) is the only interface the analysis core
asks the frontend to satisfy. The pyc Python frontend implements it as
`PycCallbacks` (in `python_ifa.h:15`), itself a thin shim over
`PycCompiler` (which extends `PycCallbacks`); the V frontend implements it
as `PCallbacks` (in `ifa/frontend/ast_to_if1.cc`).

The full interface, with who calls each and what a frontend typically does:

| Callback | Default | Who calls | Frontend responsibility |
|---|---|---|---|
| `finalize_functions()` | no-op | `FA::analyze` → `initialize()` at the start of analysis | Last chance to add/adjust `Fun`s before analysis begins. pyc uses this for late closure resolution and default-arg wrappers. |
| `new_Sym(name)` | abstract | Whenever the IFA core or cloning needs a fresh Sym (`new_Sym()` macro in `if1.h`) | Allocate a frontend-specific subclass of `IFASymbol` and bind `sym->asymbol = this`. |
| `make_LUB_type(s)` | identity | `clone.cc` when collapsing a `Type_SUM` into a single named sym | Build/look up the canonical LUB sym understood by codegen. Return NULL to abort cloning. |
| `formal_to_generic(s, &g, &bind)` | false | `pattern.cc` during overload resolution | True if `s` is a generic formal; sets `g` to the constraint and `bind` to whether to bind by value. |
| `instantiate(s, subs)` | NULL | `pattern.cc` (generics) | Instantiate a generic sym with a substitution map. |
| `order_wrapper(f, subs)` | NULL | `pattern.cc` | Wrap a Fun to permute arguments to a canonical order. |
| `promote / promotion_wrapper` | NULL | `pattern.cc` | Implicit promotion (e.g. `int → float`). |
| `coerce / coercion_wrapper` | NULL | `pattern.cc` | Explicit coercion (e.g. type cast). |
| `default_wrapper(f, defs)` | NULL | `pattern.cc` | Emit a wrapper that supplies default arguments at missing positions. |
| `instantiate_generic(f, subs)` | NULL | `pattern.cc` | Clone a generic function with type substitutions. |
| `reanalyze(violations)` | false | `FA::analyze` outer loop | Allows the frontend to react to violations and request another pass even when the splitter is done. pyc uses this to retry with relaxed constraints. |
| `report_analysis_errors(violations)` | no-op | end of `FA::analyze` | Format and emit surviving type errors to the user. |
| `c_codegen_pre_file(fp)` | false | top of `cg.cc:c_codegen_write_c` | Write `#include`s / prologue to the generated `.c`. pyc uses this to emit `#include "pyc_c_runtime.h"`. |

In addition, `IFAAST` (the analysis's view of a frontend AST node, in
`ifa.h:33-52`) and `IFASymbol` (the analysis's view of a frontend symbol,
in `ifa.h:17-31`) are virtual base classes the frontend subclasses. The
analysis only ever calls the virtual methods listed there:
`pathname/line/source_line/symbol/copy_tree/copy_node/visible_functions/html`
and `copy/clone`.

**Practical rule:** if the analysis needs to know something language-specific
(scoping, naming, generics, coercions), it goes through `IFACallbacks` or
through a virtual on `IFAAST`/`IFASymbol`. If you're adding a feature that
the analysis can't already express through those, you either need a new
callback or a frontend-only transformation before analysis runs.

---

## Cross-cutting invariants

Things that hold across the whole library. Touching anything in violation
of these will cause silent corruption.

### 1. `Sym` pointer identity is type identity

Every `Sym *` is owned by `IF1::allsyms`. Strings are interned via
`if1_cannonicalize_string`. Constants are interned via `IF1::constants`.
`AType *`, `Setters *`, and `SettersClasses *` are hash-consed (see
`fa.cc:55-57`). Pointer equality means semantic equality almost everywhere.

Corollary: never construct a `Sym` directly via `new Sym`. Always go through
`new_Sym(name)` (the macro in `if1.h`), which routes through the frontend's
`IFACallbacks::new_Sym`. Never construct an `AType` with raw `new AType`
without ending with `type_cannonicalize`. The public builders
(`make_AType`, `make_abstract_type`) do this for you.

### 2. `IF1 *if1` and `PDB *pdb` are process-wide singletons

Set up by `ifa_init` (creates `new IF1`) and the `PDB` constructor
(`pdb.cc:11`). Re-init is not supported. Two concurrent analyses in one
process would corrupt each other; the worklists in `fa.cc` are static too.

### 3. `Fun(Sym *)` builds CFG and SSU as a side effect

`Fun::Fun(Sym *)` in `fun.cc:60-66` calls `build_cfg()` then `build_ssu()`
then `build_uses()` then `setup_ast()` then `check_invariants()`. There is
no separate "build" entry point. If you want a Fun without CFG, you don't.

### 4. Two `Vec` mental models

`Vec<T*>` is sometimes a dense array (push, iterate by index), sometimes a
sparse set (`set_add`, `set_in`, `set_to_vec`). Both modes use the same
type; the discipline is at the call site. When iterating a set-mode `Vec`,
allow for null entries (the iterator does this automatically:
`for (T *x : v) if (x) ...`).

### 5. IDs

`PNode::id`, `AVar::id`, `AEdge::id`, `EntrySet::id`, `CreationSet::id` are
allocated at construction from monotonic counters in `fa.cc`. They're stable
across analysis passes and used by `qsort_by_id` for deterministic
iteration. **`Sym::id` is NOT stable across cloning** — `Sym::clone()`
allocates a fresh id. Don't cache by `Sym::id`.

### 6. `live` bits

Several places set `live` to mark reachable code (`PNode::live`,
`Var::live`, `Code::live`, `Sym::live`). These bits are computed by
`mark_live_code/types/funs` in `optimize/dead.cc` from the analysis output.
Treat them as a read-only consequence of analysis, not as something to set
manually. Reset only when re-running analysis.

### 7. Garbage collection

Boehm GC tracks everything inheriting `gc` (and most plib containers).
Don't `delete` IFA objects. Don't keep raw pointers to memory the GC
can't see (e.g., interior pointers of mmap'd buffers without keeping the
buffer rooted).

### 8. Worklist re-entrancy

The three worklists in `fa.cc` (`edge_worklist`, `send_worklist`,
`es_worklist`) are `static` globals. Items carry `in_*_worklist` bits to
prevent double-enqueue. Don't enqueue from outside `fa.cc` — go through the
existing helpers (`update_in`, `update_gen`).

---

## What lives where: by concern

If you're hunting for a specific concept, this table maps concept → home.

| Concept | Lives in |
|---|---|
| Type inference | `analysis/fa.cc` ([IFA.md](IFA.md)) |
| Monomorphisation | `analysis/clone.cc` ([CLONE.md](CLONE.md)) |
| Overload resolution / generics | `if1/pattern.cc` |
| Dispatch (`.` operator, method lookup) | `analysis/fa.cc` `P_prim_period` + `if1/pattern.cc` |
| Builtin primitive set | `if1/prim_data.cc` (generated) + `if1/prim.cc` |
| Transfer functions for primitives | callback `tfn` field, registered via `prim_reg` |
| Constant folding | `if1/num.cc` (`fold_constant`) + `fa.cc:type_num_fold` |
| Numeric type promotion | `if1/num.cc` + `fa.cc:coerce_num` |
| Type casts / boxing | `if1/cast_code.cc` (generated), `if1/check_cast.cc` |
| Scope chain (V) | `frontend/scope.cc` |
| Lexical display (closures) | `analysis/fa.cc` `EntrySet::display` + `update_display` |
| Loops (loop detection) | `optimize/loop.cc` |
| Dominators | `optimize/dom.cc` |
| Phi/phy insertion (SSU) | `optimize/ssu.cc` |
| Dead code elimination | `optimize/dead.cc` |
| Inlining | `optimize/inline.cc` |
| Frequency estimation | `optimize/inline.cc` `frequency_estimation` |
| C output | `codegen/cg.cc` |
| LLVM output | `codegen/llvm*.cc` |
| GraphViz / VCG dumps | `analysis/graph.cc` |
| HTML visualisation | `common/html.cc` + `analysis/graph.cc` |
| Logging / log channels | `common/log.cc` + per-area `LOG_*` constants |
| Config (`~/.ifalib`, `ifa.init`) | `common/config.cc` |
| Argument parsing | `common/arg.cc` |
| Unit tests | `common/unit.cc` (framework); test bodies live next to the code (`vec_test.cc` etc.) |
| Boehm GC plumbing | `common/defalloc.h` |

---

## A few practical notes

- The pyc driver (`pyc.cc` at the repository root) duplicates a couple of
  pieces of `ifa/main.cc` because it embeds the same library differently
  (no `--code` flag, different defaults, etc.). When something in
  `ifa/main.cc` changes, check whether `pyc.cc` needs the matching change.
- `ifa/frontend/` is the V-language frontend. pyc does NOT use it — pyc
  has its own frontend at the repository root (`python_*.cc`). The V
  frontend is kept because it's the original IFA test bed and many of the
  unit tests under `ifa/tests/` exercise it.
- The `__pyc__/` builtin module (Python side) and `prim_data.dat` (IFA
  side) are the two "tables you edit by hand to add to the language."
  Changes to either typically require a rebuild and a test sweep.
- `if1->callback->c_codegen_pre_file` is how pyc gets `#include
  "pyc_c_runtime.h"` into emitted C. If you ever change the runtime
  header layout, that's the seam.
- `fdce_if1` (a global bool) must be true for `ifa_analyze` to proceed.
  It's set true in the build by default. If you ever see "unable to
  translate dead code", check that switch.

---

## When this document goes stale

Triggers that require an update here:

- Adding/removing a phase in `ifa.cc:ifa_analyze` or `ifa.cc:ifa_optimize`.
- Adding/removing/renaming an `IFACallbacks` virtual.
- Reorganising directories under `ifa/`.
- Adding a new top-level export from `ifa.h`.

Triggers that do NOT require an update: bug fixes, internal refactors that
preserve the orchestration, line-count changes, comment changes.
