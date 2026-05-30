# FRONTEND — V Language Front End

A working reference for `ifa/frontend/` — the V-language frontend used
by the standalone `ifa` CLI. **pyc does NOT use this frontend** —
pyc has its own at the repository root (see
[PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md)). The V frontend exists
because it was the original IFA test bed and many of the unit tests in
`ifa/tests/*.v` exercise it.

Sister docs: [PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md) (parallel
structure for Python), [IR.md](IR.md) (the IR this frontend produces),
[ARCHITECTURE.md](ARCHITECTURE.md) §IFACallbacks.

---

## 1. In one paragraph

`compile_one_file(fn)` in `parse.cc` detects the file extension
(`.v` or `.py`), loads `prelude.<ext>` from `IFA_SYSTEM_DIRECTORY`
first, then loads the user file. Each file is parsed with DParser
using the grammar at `frontend/v.g` (or `frontend/python.g`), and the
grammar actions construct a `ParseAST` tree. `ast_gen_if1(if1, av)`
in `ast_to_if1.cc` lowers the ParseAST into IF1 — a 1900-line
recursive descent that's the same idea as pyc's two-pass
`build_syms` + `build_if1`, but compressed into a single pass with
heavier reliance on the `Scope` chain (`frontend/scope.cc`). After
that, `compile_one_file` invokes `ifa_analyze` → `ifa_optimize` →
either C codegen (default) or LLVM codegen (`IFA_LLVM` env var).
`PCallbacks` is the V frontend's `IFACallbacks` impl — minimal,
since V's type system is simpler than Python's.

---

## 2. File map

```
ifa/frontend/
├── parse.h                Public API: `compile_one_file(fn)`.
├── parse_structs.h        DParser type overrides: ParseSym, V_ParseNode, Globals.
├── parse.cc               (177 lines) Entry, whitespace handler, FrontEnd table, load_file.
├── make_ast.h / make_ast.cc       (81 lines) ParseAST construction helpers.
├── ast_kinds.h            S(...) macro list of AST_* kinds.
├── ast_to_if1.h / ast_to_if1.cc   (1876 lines) The lowering pass.
├── scope.h / scope.cc     (68 lines) Lexical scope chain.
├── v.g                    V language grammar.
├── v.g.d_parser.cc        Generated parser (28821 lines).
├── python.g               Python grammar (duplicated; pyc uses the root copy).
├── python.g.d_parser.cc   Generated.
└── c.g                    C grammar for the cast generator.
```

---

## 3. Lifecycle

### 3.1 `compile_one_file(fn)` (`parse.cc:162`)

```c
int compile_one_file(cchar *fn) {
  ifa_init(new PCallbacks);
  if (load_one(fn) < 0) return -1;
  if (ifa_analyze(fn) < 0) return -1;
  if (ifa_optimize() < 0) return -1;
  if (getenv("IFA_LLVM")) {
    llvm_codegen_write_ir(pdb->fa, if1->top->fun, fn);
    llvm_codegen_compile(fn);
  } else {
    ifa_cg(fn);
    ifa_compile(fn);
  }
  return 0;
}
```

Called from `ifa/main.cc` for each file argument.

### 3.2 `load_one(fn)` (`parse.cc:133`)

1. Strip extension; find the matching `FrontEnd` in `langs[]`
   (`parse.cc:29`).
2. Load `$IFA_SYSTEM_DIRECTORY/prelude.<ext>` first — the language's
   built-in declarations.
3. Load the user file.
4. Call `ast_gen_if1(if1, av)` with both ParseASTs.

The prelude is a real .v / .py file that declares the built-in types,
operators, and runtime symbols. The frontend doesn't hard-code them;
they come from prelude. This is different from pyc, where the
builtin module (`__pyc__/`) is also a `.py` file but is concatenated
via `dparse_builtin_dir`.

### 3.3 `load_file(fn, fe)` (`parse.cc:98`)

Standard DParser invocation:
1. `new_D_Parser(fe->compilation_tables, *fe->user_size)`.
2. Set `loc.pathname`, `loc.line`, `loc.col`, `save_parse_tree = 1`.
3. Install whitespace handler (`fe->whitespace`).
4. Allocate and zero `Globals` of `*fe->globals_size`, set `i = if1`.
5. `buf_read(fn, &buf, &len)`; `dparse(p, buf, len)`.
6. Return `pn->user.ast` (the ParseAST root).

---

## 4. ParseAST (`ast_to_if1.h:42`)

The V-language AST node. Larger than pyc's `PyDAST` because the
lowering happens in one pass and ParseAST carries the IF1 result
fields alongside:

```c
class ParseAST : public IFAAST {
  AST_kind kind;
  unsigned scope_kind : 2, constructor : 2, intent : 2;
  unsigned def_ident_label : 1, op_index : 1, in_tuple : 1,
           in_apply : 1, is_assign : 1, is_simple_assign : 1,
           is_ref : 1, is_application : 1, is_comma : 1, is_inc_dec : 1;
  unsigned rank;
  ParseAST *parent;
  Vec<ParseAST *> children;
  Sym *sym;
  cchar *string;
  cchar *destruct_name;
  cchar *arg_name;
  cchar *builtin;
  Prim *prim;
  cchar *_pathname;
  int _line;
  Scope *scope;
  cchar *constant_type;
  Sym *container;
  Label *label[2];        // before / after for loops (continue, break)
  Code *code;
  Sym *rval;
};
```

Compare to pyc's `PyDAST` (which doesn't carry `code`/`label`/`rval`
on the AST itself — pyc puts those on `PycAST`). The V frontend is
more entangled but operates in fewer passes.

### 4.1 AST kinds (`ast_kinds.h`)

71 kinds across modules, identifiers, functions, types, constructors,
expressions, flow control. Notable categories:

- **Module:** `AST_in_module`, `AST_use_module`, `AST_import`,
  `AST_export`, `AST_as`, `AST_extern`, `AST_extern_include`.
- **Identifier:** `AST_const`, `AST_qualified_ident`, `AST_global`,
  `AST_scope`, `AST_ident`, `AST_var`, `AST_def_ident`,
  `AST_declare_ident`.
- **Function:** `AST_def_fun`, `AST_pattern`, `AST_pattern_type`,
  `AST_init`, `AST_intent`, `AST_arg`, `AST_rest`.
- **Constructor:** `AST_new`, `AST_object`, `AST_list`, `AST_vector`.
- **Type:** `AST_def_type`, `AST_where`, `AST_type`,
  `AST_def_type_param`, `AST_type_param`, `AST_inherits`,
  `AST_must_implement`, `AST_must_specialize`, `AST_implements`,
  `AST_specializes`, `AST_includes`, `AST_vector_type`,
  `AST_sum_type`, `AST_record_type`, `AST_fun_type`,
  `AST_ref_type`, `AST_tagged_type`, `AST_type_application`.
- **Expression:** `AST_op`, `AST_ifexpr`, `AST_block`, `AST_conc`,
  `AST_seq`, `AST_with`, `AST_with_scope`.
- **Flow control:** `AST_label`, `AST_break`, `AST_continue`,
  `AST_goto`, `AST_return`, `AST_if`, `AST_loop`, `AST_loop_cond`.

The macro `S(name)` in `ast_kinds.h` is expanded twice:
- In `ast_to_if1.h`, to populate the `AST_kind` enum.
- In `ast_to_if1.cc`, to populate `AST_name[]`.

### 4.2 ParseAST methods

- `add(ParseAST*)` — append a child, set parent.
- `add(D_ParseNode*)` — dig into the parse tree and append all the
  ParseASTs found.
- `add_below(pn)` — add to the *child*'s list (used in deeply nested
  grammar productions).
- `set_location(pn)` / `set_location_and_add(pn)` — copy line/pathname
  from a parse node.
- `get(AST_kind)` — find first child of a given kind.
- `last()` — `children[children.n - 1]`.
- `symbol()` — `rval` if set, else `sym` (which is the IFAAST API).
- `pathname()` / `line()` / `source_line()` — for diagnostics.
- `copy_tree(context)` / `copy_node(context)` — clone for cloning.
- `propagate(nodes)` — push PNode pointers up to ancestors.
- `html(fp, f)` / `graph(fp)` — debug output.

---

## 5. DParser integration (`parse_structs.h`)

```c
typedef struct ParseSym {
  unsigned type_id : 1;
  D_Sym *sym;
  D_Scope *scope;
} ParseSym;

typedef struct ParseNode {
  ParseAST *ast;
  D_Sym *sym;
  D_Scope *saved_scope;
} V_ParseNode;

typedef struct Globals {
  IF1 *i;
  int errors;
} Globals;

#define D_UserSym ParseSym
#define D_ParseNode_User ParseNode
#define D_ParseNode_Globals Globals
```

DParser invokes grammar actions with `D_ParseNode *pn`; the action
can set `pn->user.ast = new_AST(...)` and the parser propagates that
up the tree. After `dparse` returns, the root's `user.ast` is the
ParseAST root.

DParser also has built-in scope management — `D_Sym` / `D_Scope` —
which the grammar uses to track symbol declarations during parsing.
This is in addition to the post-parse `Scope` chain that
`ast_to_if1` walks.

---

## 6. The lowering pass (`ast_to_if1.cc`, 1876 lines)

### 6.1 Entry: `ast_gen_if1(if1, av)` (declared in `ast_to_if1.h:102`)

Takes the prelude + user-file ParseASTs and produces:
- Sym tables populated with user-defined types, functions, vars.
- Code trees attached to function Syms.
- Builtin symbols set up via `set_builtin`.
- The top closure registered with `if1->top`.

### 6.2 The recursive descent

`ast_gen_if1` walks the ParseAST recursively. Different `AST_kind`s
trigger different handlers; the file is one giant switch-on-kind
spanning the bulk of the 1876 lines.

The pass:
1. Builds the global scope from the prelude.
2. Walks the user file, entering scopes as needed
   (`AST_def_fun`, `AST_def_type`, `AST_block`).
3. For each statement, creates the IF1 Codes via `if1_send` etc.
4. For each declaration, allocates and registers Syms via
   `new_sym` / `new_constant`.
5. For each type, builds inheritance via `Sym::inherits_add`.
6. After the user file, finalises types via `finalize_types` and
   `build_type_hierarchy`.

### 6.3 Scope chain (`scope.cc` / `scope.h`)

```c
class Scope : public gc {
  unsigned kind : 2;       // Scope_INHERIT / RECURSIVE / PARALLEL / SEQUENTIAL
  Map<cchar *, Sym *> hash;
  Scope *up, *next;
  Sym *in;
  Vec<Scope *> dynamic;          // "imported" scopes
  Vec<Sym *> dynamic_container;
};
```

The four scope kinds match DParser's `D_SCOPE_*`:
- `Scope_RECURSIVE` — bindings visible throughout (most common).
- `Scope_SEQUENTIAL` — bindings visible only after declaration.
- `Scope_PARALLEL` — parallel/non-interleaved.
- `Scope_INHERIT` — inherits parent's kind.

`Scope::get(name, &container)`:
1. Check own `hash`.
2. Check each `dynamic` scope (imports).
3. Walk `up` recursively.

`add_dynamic(s, sy)` plugs an imported scope into the current scope
so its names are visible via `get`.

`global()` / `module()` / `function()` walk `up` to find the right
enclosing scope.

Compare to pyc's `PycScope`: pyc uses sentinel markers
(`GLOBAL_USE`/`NONLOCAL_USE`/etc.) inside the map; V uses
plain bindings and the scope-kind enum for visibility rules.

---

## 7. PCallbacks (`ast_to_if1.cc:32`)

The V frontend's `IFACallbacks` implementation — much thinner than
pyc's:

```c
class PCallbacks : public IFACallbacks {
public:
  void finalize_functions();       // empty in V
  void new_SUM_type(Sym *);        // asserts type_kind == Type_SUM
  Sym *new_Sym(cchar *name = 0);   // allocates Sym, registers with IF1
};
```

V doesn't override `default_wrapper`, `make_LUB_type`, `coerce`,
`promote`, `instantiate`, `instantiate_generic`, etc. — they all
use the `IFACallbacks` defaults (mostly no-ops returning NULL).
This works because V's type system is simpler than Python's: less
ad-hoc dispatch, no default arguments, no decorators, no `super`.

The `instantiate` / `make_LUB_type` defaults (returning the input
unchanged or NULL) mean some advanced features in V tests rely on
the analysis's intrinsic handling rather than frontend hooks.

---

## 8. Helper functions in `ast_to_if1.cc`

### 8.1 `find_prim(ast)` (`ast_to_if1.cc:67`)

Look up the `Prim *` for a grammar-produced operator/primitive
SEND. Walks the children to identify the marker Sym, then queries
`if1->primitives->prim_map[nargs][pos]`. See
[PRIMITIVES.md](PRIMITIVES.md) §4.1 for the map structure.

### 8.2 `new_sym(if1, scope, name, sym)` (`ast_to_if1.cc:430`)

The V-frontend's sym allocator. Distinct from `if1_register_sym` —
this one also installs into `scope->hash` if a scope is given.

### 8.3 `set_builtin(if1, sym, start, end)` (`ast_to_if1.cc:446`)

Wrapper around `if1_set_builtin` with name interning.

### 8.4 `build_builtin_syms(if1, ast)` (`ast_to_if1.cc:513`)

Walks the prelude ParseAST looking for `AST_extern` declarations
(`extern type int32 ...` and friends) and registers them as
builtins. This is how V learns the connection between the prelude's
`int32` name and the compiler's `sym_int32` global.

### 8.5 `build_constant_syms(if1, ast)` (`ast_to_if1.cc:550`)

Walks the prelude for constant declarations and creates the
interned constant Syms.

### 8.6 `ast_to_type(ast)` (`ast_to_if1.cc:562`)

Maps `AST_record_type` / `AST_sum_type` / `AST_vector_type` /
`AST_fun_type` etc. to the corresponding `Type_kind` enum value.

### 8.7 `ast_qualified_ident_*` helpers (`ast_to_if1.cc:317-365`)

A family of helpers for resolving `Module.subscope.name` qualified
identifiers. Walks an `AST_qualified_ident` chain through the scope
hierarchy to find the target Sym.

### 8.8 `ast_print`, `ast_print_recursive`, `ast_pp` (`ast_to_if1.cc:371-396`)

Debug printers. `ast_pp` is the gdb entry point.

---

## 9. Grammar (`v.g`)

DParser grammar. Compiled to `v.g.d_parser.cc` (28821 lines) by
`make_dparser`. Not for casual reading. Notable rules:

- `module_def` — top-level structure.
- `def_fun` — function definition with args + body.
- `def_type` — type definitions (record, sum, vector, fun).
- `expr` — expression productions, hundreds of variants.
- `statement` — control flow + assignment.

Grammar actions set `$$->user.ast = new_AST(...)` with the
appropriate `AST_kind`. The actions also call
`set_location_and_add` to capture line/file from each rule.

### 9.1 Whitespace handler (`parse.cc:41`)

`no_preprocessor_whitespace` handles `//` and `/* ... */`
comments, including nested `/* */`. Updates `loc->line` for newlines
inside comments. Used for V (`langs[0]`) but NOT Python — pyc has
its own `python_whitespace` (in the generated parser) that handles
INDENT/DEDENT.

---

## 10. The prelude

`$IFA_SYSTEM_DIRECTORY/prelude.v` is the V language's standard
library. It declares:

- All built-in types: `int8`, `int16`, ..., `float32`, ...,
  `string`, `symbol`, `list`, `tuple`, etc.
- Operators: `+`, `-`, `*`, etc., with their signatures.
- Built-in primitives via `extern` declarations referencing
  primitive names from the table (matching
  [PRIMITIVES.md](PRIMITIVES.md)).
- Default initial values for the type lattice.

For Python, there's a similar `prelude.py` consumed by the V-style
load path — but pyc's main driver bypasses this entirely in favour
of `__pyc__/`.

---

## 11. What pyc does differently

The pyc and V frontends solve the same problem but make different
choices. Compare:

| Concern | V frontend | pyc frontend |
|---|---|---|
| Parser | DParser via `frontend/parse.cc` | DParser via `python_parse.cc` |
| Builtin module | `prelude.v` (single file) | `__pyc__/` (concatenated dir) |
| Lowering passes | 1 pass (`ast_gen_if1`) | 2 passes (`build_syms` → `build_if1`) |
| Scope model | `Scope` chain + dynamic imports | `PycScope` stack + sentinels |
| AST nodes | `ParseAST` (carries Code+Label) | `PyDAST` (carries kind only) + `PycAST` (wrapper, carries Code+Label) |
| `IFACallbacks` overrides | `new_Sym`, `new_SUM_type`, `finalize_functions` (empty) | All of those plus `default_wrapper`, `reanalyze`, `c_codegen_pre_file` |
| Imports | DParser `D_Scope` + `add_dynamic` | recursive `import_file` with saved scope stack |
| Generics | Frontend defaults (NULL) | Same — neither uses them |
| Numeric coercion | via prelude operator overloads | Same |

The V frontend is the "purer" of the two — it sticks to the
IFA-original design where the analysis does most of the work. pyc's
two-pass model is necessary to handle Python's dynamism (late
binding, decorators, class scopes).

---

## 12. Gotchas

### 12.1 V is not the production frontend
`pyc.cc` does NOT call `compile_one_file`. The V frontend is
exercised only via `ifa <file.v>`. Don't expect changes to
`ast_to_if1.cc` to affect pyc compilations.

### 12.2 Prelude is mandatory
`load_one` fails if `prelude.<ext>` can't be loaded. The prelude
sets up `sym_int32` etc., so without it the IF1 globals are NULL
and analysis crashes. `IFA_SYSTEM_DIRECTORY` must be set correctly.

### 12.3 DParser scope ≠ IFA Scope
DParser maintains its own `D_Scope` for parser disambiguation
(deciding whether a token is a type name or a variable). The IFA
`Scope` chain in `scope.cc` is *separate* and lives post-parse.
Don't confuse them when reading the grammar.

### 12.4 The duplicated Python grammar
`frontend/python.g` exists but pyc uses `python.g` at the root.
The two have diverged. If you fix a Python grammar bug, decide
which version is authoritative for what you're doing:
- For pyc compilations: root `python.g`.
- For `ifa <file.py>`: `frontend/python.g`.
The intent was probably to consolidate; the current state is
duplicated.

### 12.5 ParseAST.code is set during lowering
`ParseAST::code` is initially NULL and gets set by `ast_to_if1`
when the AST node's IF1 is emitted. If you call `ast_print`
mid-lowering, you may see partial code attached.

### 12.6 Scope_INHERIT loses information
`Scope_INHERIT` copies the parent's kind. If you query
`scope->kind` later you'll get the inherited value, not
`INHERIT`. Don't rely on `kind == Scope_INHERIT` ever being true
after construction.

### 12.7 `Globals::errors` accumulator
DParser grammar actions can set `pg->errors` to flag a semantic
error. `load_file` checks this after `dparse` and aborts. The
counter is module-local (per-file), not global.

### 12.8 `_pathname` is per-AST
`ParseAST::_pathname` is set per AST node via `set_location`. If
you construct an AST programmatically without calling
`set_location`, `pathname()` returns NULL and diagnostics will be
ungrounded.

### 12.9 V-language operator overloading goes through prelude
V's `+` for int isn't hardcoded — it's a `def_fun` in `prelude.v`
that wraps `prim_add`. The frontend lowers `a + b` as a regular
function call; pattern matching (see [DISPATCH.md](DISPATCH.md))
resolves to the prelude's overload. This is why V can dispatch on
operators uniformly.

### 12.10 IFA_LLVM is checked at the very end
The branch between C and LLVM codegen happens after analysis +
optimization. Both paths share the same `pdb->fa` state — you
can't get different analysis results based on backend choice.

---

## 13. Symptom → start-here

| Symptom | Start here |
|---|---|
| "unknown extension" | `parse.cc:langs[]` — add an entry for new languages |
| "cannot read file 'prelude.v'" | `IFA_SYSTEM_DIRECTORY` not set; or prelude moved |
| "syntax error in V code" | `v.g` grammar; run `make` to regenerate `v.g.d_parser.cc` |
| "wrong scope visibility" | `Scope::get` walk — check `kind` and `dynamic` |
| "ParseAST kind missing" | `ast_kinds.h` — add the entry, then handle in `ast_to_if1.cc` switch |
| "imported module not found" | `add_dynamic` — check the import statement produced an `AST_use_module` |
| "type hierarchy wrong" | `ast_to_if1.cc:build_constant_syms` + `Sym::inherits_add` + `build_type_hierarchy` in `ast.cc` |
| "duplicate symbol" | `Scope::put` doesn't reject duplicates; check the grammar's scope action |
| "V test that worked before fails" | Compare `ast_print` output of working vs broken; check for `Code::ast` regressions |
| "operator not dispatched" | `find_prim` in `ast_to_if1.cc` + `Primitives::find` |

---

## 14. References

- `ifa/frontend/parse.{cc,h}` — entry point.
- `ifa/frontend/parse_structs.h` — DParser overrides.
- `ifa/frontend/ast_to_if1.{cc,h}` — lowering.
- `ifa/frontend/scope.{cc,h}` — scope chain.
- `ifa/frontend/make_ast.{cc,h}` — ParseAST construction.
- `ifa/frontend/v.g` — V grammar.
- `ifa/frontend/ast_kinds.h` — AST_* enum.
- `$IFA_SYSTEM_DIRECTORY/prelude.v` — V standard library.
- Sister docs: [PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md) (parallel
  design for Python), [IR.md](IR.md) (the IR being produced),
  [DISPATCH.md](DISPATCH.md) (operator dispatch),
  [PRIMITIVES.md](PRIMITIVES.md) (the prims operators lower to),
  [ARCHITECTURE.md](ARCHITECTURE.md) §IFACallbacks (the callback
  contract `PCallbacks` implements).
