# PRIMITIVES — Compiler Primitives

A working reference for `ifa/if1/{prim,prim_data}.{cc,h}` and the
`prim_data.dat` table that drives them. Primitives are the way the
analysis and codegen recognise built-in operations (arithmetic,
dispatch, allocation, etc.) inside the otherwise-generic IF1 SEND nodes.

Sister docs: [IR.md](IR.md) §4 (Code/SEND), [IFA.md](IFA.md) §5.5
(how transfer functions are invoked), [DISPATCH.md](DISPATCH.md)
(how non-primitive sends become dispatch).

---

## 1. What a primitive is, in one paragraph

Every SEND in IF1 either resolves to a user-defined function (handled by
[DISPATCH.md](DISPATCH.md)) or to a *primitive*: a fixed builtin
recognised by name. Primitives have signatures (arg type slots, return
type slots), a position-of-marker convention (so the matcher knows which
rval is the primitive's identity), and three pieces of behaviour: an
**arg-validation** filter computed from the signature, an optional
**transfer function** that the IFA analysis calls to refine the result
type, and an optional **codegen function** that the C/LLVM backend
calls to emit the operation. The "table" primitives (~56 of them) are
declared in `prim_data.dat` and generated into `prim_data.{cc,h}` by
`make_prims`. Frontend-specific primitives (e.g., pyc's
`__pyc_c_call__`) are registered at runtime via `prim_reg`.

---

## 2. Two kinds of primitive

| Kind | Declared | Identified at SEND by | Registered with | Examples |
|---|---|---|---|---|
| **Table primitive** | `prim_data.dat` → generated `prim_data.{cc,h}` | `Primitives::prim_map[nargs][pos]` lookup using a Sym's `name` | `prim_init` at IF1 ctor | `prim_add`, `prim_period`, `prim_make`, `prim_reply`, ... |
| **Registered primitive** | runtime via `prim_reg(name, tfn, cgfn)` | maps to `prim_primitive` (table) with a name-lookup in `registered_prims` | frontend (`add_primitive_transfer_functions` in pyc; equivalent in V) | `__pyc_c_call__`, `__pyc_format_string__`, `to_string`, `write`, `writeln` |

Both produce a `Prim *` attached to a `Code` at `if1_finalize` time
(`Primitives::find(Code*)` is called per SEND). The analysis dispatches
on `code->prim->index` (a small integer); see `fa.cc:add_send_edges_pnode`
(the big switch in [IFA.md](IFA.md) §5.5).

---

## 3. The `Prim` class (`prim.h:62`)

```c
class Prim : public gc {
  int index;            // P_prim_* constant; identifies this primitive
  cchar *string;        // the SEND "name" (e.g. "+", "period", "make")
  cchar *name;          // the C identifier (e.g. "prim_add")
  int nargs;            // -n means at least n; positive means exactly
  int nrets;
  int pos;              // which rval slot holds the primitive marker
  uint nonfunctional : 1; // can't be DCE'd if lvals unused (side effects)
  PrimType *arg_types;  // vector excluding the marker rval
  PrimType *ret_types;
  Vec<AType *> args;    // computed at FA init: filter AType per arg
};
```

### 3.1 `PrimType` enum (`prim.h:42`)

The signature slot types:

```
PRIM_TYPE_ALL          // any type, even invalid (lattice ⊤)
PRIM_TYPE_ANY          // any value type (sym_any)
PRIM_TYPE_SYMBOL       // a user symbol (sym_symbol)
PRIM_TYPE_STRING       // sym_string
PRIM_TYPE_TUPLE        // sym_tuple
PRIM_TYPE_REF          // sym_ref pointer
PRIM_TYPE_CONT         // sym_continuation
PRIM_TYPE_A            // result is "same as A"
PRIM_TYPE_ANY_NUM_A    // any numeric, named A
PRIM_TYPE_ANY_NUM_B    // any numeric, named B
PRIM_TYPE_ANY_NUM_AB   // numeric LUB(A,B) — used for binary ops
PRIM_TYPE_ANY_INT_A    // any integer, named A
PRIM_TYPE_ANY_INT_B    // any integer, named B
PRIM_TYPE_BOOL         // result is bool
PRIM_TYPE_SIZE         // result is sym_size
```

The `_A` / `_B` / `_AB` naming makes binary-op signatures express
"both args must be numeric; the result is their LUB" without naming a
specific type. The analysis (`fa.cc:add_send_edges_pnode`) interprets
these symbolically — `PRIM_TYPE_ANY_NUM_AB` triggers `type_num_fold`
to compute the actual return type per argument-pair.

### 3.2 `nargs` and `pos`

`nargs` counts **all** rvals (including the marker). Negative = vararg
with minimum: `-3` means "at least 3 rvals." `pos` indicates which slot
holds the marker Sym (`sym_operator` or `sym_primitive`, plus the
specific op marker like `sym_add`).

Two SEND shapes appear in IF1:
- **`operator` form**: rvals = `(sym_operator, lhs, op_sym, rhs)` —
  `pos = 1` puts `lhs` first, the op marker second. Used for binary
  operators where one of the args is the "receiver" for dispatch
  purposes.
- **`primitive` form**: rvals = `(sym_primitive, op_sym, args...)` —
  `pos = 0` puts the marker first. Used for unary, vararg, and
  non-operator builtins.

### 3.3 `nonfunctional`

Set via `PRIM_NON_FUNCTIONAL` in the `options` arg to the constructor.
Marks primitives with side effects (writes, allocation,
`prim_set_index_object`, `prim_period`/`prim_setter` for impure dispatch).
`is_functional(p, code)` in `if1.cc:398` consults this and refuses to
DCE the call.

### 3.4 `Prim::args` (computed at FA init)

`fa.cc:initialize_primitives` (~`fa.cc:2737`) walks `Prim::arg_types` and
populates `Prim::args[i]` with the matching `AType *` from the global
type-lattice (`any_type`, `symbol_type`, `string_type`, etc.). This is
the per-position filter the analysis uses to validate arguments
(`add_send_edges_pnode`'s `PRIMITIVE_ARGUMENT` violation check).

---

## 4. `Primitives` (`prim.h:29`)

The IF1 singleton's primitive table:

```c
class Primitives : public gc {
  Map<cchar *, Prim *> prim_map[3][2];                                // [nargs - 2][pos] → Prim
  Vec<Prim *> prims;                                                  // all table prims in declaration order
  ChainHashMap<cchar *, StringHashFns, RegisteredPrim *> registered_prims;
  Prim *find(int nargs, Sym *a0, Sym *a1, Sym *a2 = 0);
  Prim *find(Code *c);
  Prim *find(PNode *p);
};
```

`prim_map[nargs_minus_2][pos]` is a 3×2 array of maps. Lookups are
fast (one hash). The array bounds match the only signatures in use:
- `nargs - 2 ∈ {0, 1, 2}` (i.e., 2-arg, 3-arg, 4-arg sends; vararg
  primitives use whichever slot covers the *minimum* args).
- `pos ∈ {0, 1}` (marker is at position 0 or 1).

### 4.1 `Primitives::find` (`prim.cc:25`)

```c
Prim *find(int nargs, Sym *f, Sym *a1, Sym *a2 = 0) {
  if (f == sym_operator) {
    n = nargs - 3;
    if (a1->is_symbol)  return prim_map[n][0].get(a1->name);
    else                return prim_map[n][1].get(a2->name);
  }
  if (f == sym_primitive) {
    return prim_map[0][0].get(a1->name) ?: prim_primitive;
  }
  return 0;
}
```

So:
- `(sym_operator, op, a)` (`pos=0`) — unary operator.
- `(sym_operator, a, op, b)` (`pos=1`) — binary operator.
- `(sym_primitive, op_name, args...)` (`pos=0`) — any compiler primitive.
- Anything else → not a primitive → user-defined call.

`prim_primitive` is the fallback for `(sym_primitive, unknown_name)` —
the SEND is recognised as "a primitive call" but the name isn't in the
table; the registered-prims map provides the transfer/codegen for it.
This is how `__pyc_c_call__` etc. work.

The `(Code *)` and `(PNode *)` overloads just unpack rvals and forward.

### 4.2 Where `prim` gets attached

`if1_finalize` (`if1.cc:755`) walks every closure body via
`find_primitives` and calls `Primitives::find(c)` on each SEND,
storing the result in `Code::prim`. After this, the analysis can
dispatch on `code->prim->index` without re-doing the lookup.

`PNode(Code *c)` (`pnode.cc:21`) copies `c->prim` into `pn->prim`.

---

## 5. `RegisteredPrim` (`prim.h:19`)

For frontend-specific primitives:

```c
class RegisteredPrim : public gc {
  PrimitiveTransferFunctionPtr tfn;     // tfn(pn, es): refine result type
  PrimitiveCGPtr cgfn;                  // cgfn(fp, n, f): emit C
  uint is_functional : 1;               // default true; set false if side-effecting
  uint is_visible : 1;                  // set true for primitives whose name should be exposed
};
```

### 5.1 Registration

```c
RegisteredPrim *prim_reg(cchar *name,
                         PrimitiveTransferFunctionPtr ptr,
                         PrimitiveCGPtr pcg = 0);
```

Stores into `if1->primitives->registered_prims` keyed by `name` (must
be interned via `if1_cannonicalize_string` or come from a Sym's `name`).

Frontend usage (pyc, `python_ifa_main.cc:108`):

```c
prim_reg(sym_write->name,                 return_nil_transfer_function,    write_codegen)->is_visible = 1;
prim_reg(sym_writeln->name,               return_nil_transfer_function,    writeln_codegen)->is_visible = 1;
prim_reg(sym___pyc_c_call__->name,        c_call_transfer_function,        c_call_codegen)->is_visible = 1;
prim_reg(sym___pyc_format_string__->name, format_string_transfer_function, format_string_codegen)->is_visible = 1;
prim_reg(sym___pyc_to_str__->name,        to_str_transfer_function,        to_str_codegen)->is_visible = 1;
prim_reg("to_string",                     return_string_transfer_function);
```

### 5.2 Lookup

```c
RegisteredPrim *prim_get(cchar *name);  // prim.cc:61
```

Used by `fa.cc:add_send_edges_pnode` in the `P_prim_primitive` arm:

```c
case P_prim_primitive: {
  cchar *name = p->rvals[1]->sym->name;
  RegisteredPrim *rp = prim_get(name);
  if (!rp) fail("undefined primitive transfer function '%s'", name);
  rp->tfn(p, es);
  break;
}
```

So a `(sym_primitive, op_name, ...)` SEND with an unknown name is a
hard failure at analysis time — the frontend forgot to register.

### 5.3 `is_visible` / `is_functional`

- `is_visible` — set true by registration sites that want the primitive
  *visible* to user code (callable by name). pyc sets it true for
  `write`, `writeln`, `__pyc_c_call__`, `__pyc_format_string__`,
  `__pyc_to_str__`. The default is false; `to_string` for example is
  not visible (only the runtime invokes it).
- `is_functional` — defaults true. Set false to mark the primitive as
  side-effecting; `is_functional(p, code)` in `if1.cc:398` queries this
  for `prim_primitive` SENDs and refuses to DCE them.

`is_functional` is the registered analogue of the table primitives'
`PRIM_NON_FUNCTIONAL` option.

---

## 6. Transfer functions

A transfer function refines the IFA analysis result of a primitive SEND.
Signature:

```c
typedef void (*PrimitiveTransferFunctionPtr)(PNode *pn, EntrySet *es);
```

Called from `fa.cc:add_send_edges_pnode` when the SEND's `prim` is
`prim_primitive` and the registered name has a `tfn`. The function
mutates the analysis state by calling `update_gen` / `update_in` /
`flow_vars` on the result AVar.

Example (pyc, `python_ifa_main.cc:42`):

```c
static void c_call_transfer_function(PNode *pn, EntrySet *es) {
  AVar *a = make_AVar(pn->rvals[2], es);
  AVar *result = make_AVar(pn->lvals[0], es);
  if (a->out->n == 1 && a->out->v[0]->sym->is_meta_type)
    update_gen(result, make_abstract_type(a->out->v[0]->sym->meta_type));
  else
    flow_vars(a, result);
}
```

Pre-built transfer functions in `fa.cc` (declared in `fa.h:351-353`):
- `return_nil_transfer_function(pn, es)` — result type is `nil_type`.
- `return_int_transfer_function(pn, es)` — result type is
  `anyint_type`.
- `return_string_transfer_function(pn, es)` — result type is
  `string_type`.

For table primitives, the analysis dispatches on `Prim::index` (a
giant switch in `fa.cc:add_send_edges_pnode`) rather than calling a
function pointer — see [IFA.md](IFA.md) §5.5 for the list of cases.

---

## 7. Codegen functions

```c
typedef void (*PrimitiveCGPtr)(FILE *fp, PNode *n, Fun *f);
```

Called by `cg.cc` when emitting C for a SEND whose `prim` has a
registered `cgfn`. The function writes the C statement to `fp` using
`Var::cg_string` for the args.

Example (pyc, `python_ifa_main.cc:52`):

```c
static void c_call_codegen(FILE *fp, PNode *n, Fun *f) {
  fputs(n->rvals[3]->sym->constant, fp);  // the C function name
  fputs("(", fp);
  int first = 1;
  for (int i = 5; i < n->rvals.n; i += 2) {
    if (!first) fputs(", ", fp); else first = 0;
    fputs(n->rvals[i]->cg_string, fp);
  }
  fputs(");\n", fp);
}
```

Table primitives' codegen is in `cg.cc` directly (per `Prim::index`),
not via function pointers. The LLVM backend has its own switch in
`llvm_primitives.cc`.

---

## 8. The data table: `prim_data.dat`

The 56 table primitives are declared in `ifa/prim_data.dat`. Format,
one primitive per `;`-terminated line:

```
NAME "STRING" NARGS POS [NRES] {ARG_TYPES} {RET_TYPES} [nonfunctional];
```

Fields:
- `NAME` — the C variable holding the `Prim*` (e.g., `prim_add`).
- `"STRING"` — the SEND-side name used to look up the primitive (e.g.,
  `"+"` or `"period"`).
- `NARGS` — number of rvals (including marker). Negative = at least.
- `POS` — `0` or `1`, the marker position.
- `NRES` (optional) — number of lvals; default 1.
- `{ARG_TYPES}` — `PrimType` list, excluding marker.
- `{RET_TYPES}` — `PrimType` list.
- `nonfunctional` (optional) — sets `PRIM_NON_FUNCTIONAL`.

Example lines:

```
prim_add "+" 3 1 {PRIM_TYPE_ANY_NUM_A, PRIM_TYPE_ANY_NUM_B} {PRIM_TYPE_ANY_NUM_AB} ;
prim_period "." 3 1 {PRIM_TYPE_ANY, PRIM_TYPE_ANY} {PRIM_TYPE_ANY} nonfunctional;
prim_reply "reply" -3 0 {PRIM_TYPE_CONT, PRIM_TYPE_ALL} {PRIM_TYPE_ALL} nonfunctional;
prim_make "make" -3 0 {PRIM_TYPE_ANY, PRIM_TYPE_ANY} {PRIM_TYPE_ANY} ;
```

### 8.1 Generation

`make_prims` (`ifa/codegen/make_prims.cc`) parses `prim_data.dat` and
emits two files:
- `prim_data.h` — `extern Prim *prim_X;` plus `#define P_prim_X N` for
  each line. The `P_prim_X` constants are used by `fa.cc`'s switch.
- `prim_data.cc` — variable definitions plus `prim_init(p, if1)` which
  is called by the `IF1::IF1()` constructor (via
  `Primitives::Primitives(if1) → prim_init`).

The `index` in each `Prim` constructor call is the line number
(0-based) in `prim_data.dat`. Matches `P_prim_X` exactly.

**Important:** `prim_data.cc` and `prim_data.h` are committed to the
repository, but they're regenerated from `prim_data.dat`. If you edit
`.cc`/`.h` directly, the next build will overwrite. Edit `.dat` and
re-run `make_prims`.

### 8.2 The Makefile relationship

`make_prims` (the binary) is built from `codegen/make_prims.cc` (the
generator source). However, the current `Makefile` does NOT have a
rule that depends `prim_data.cc` on `prim_data.dat` — they're treated
as static sources. To regenerate manually:

```bash
cd ifa
./make_prims                       # writes prim_data.{h,cc} alongside the .dat file
mv prim_data.{h,cc} if1/           # if invoking from the wrong cwd
```

If you add a primitive, the workflow is:
1. Edit `prim_data.dat`.
2. Run `make_prims` to regenerate.
3. Add the case to `fa.cc:add_send_edges_pnode`'s switch.
4. Add the codegen case to `cg.cc` (and to `llvm_primitives.cc` if
   you support LLVM).
5. If the frontend needs to emit the new primitive, expose the marker
   Sym (e.g. `sym_my_new_primitive` in `builtin_symbols.h` and
   `build_environment`).

---

## 9. The `P_prim_*` constants (full list)

From `prim_data.h` (auto-generated, but the table is stable):

```
0   prim_operator          marker for binary operators (rvals[0])
1   prim_period            obj.attr lookup       (pos=1, nonfunc)
2   prim_setter            obj.attr = val        (pos=1, nonfunc)
3   prim_pow               a ** b
4   prim_mult              a * b
5   prim_div               a / b
6   prim_mod               a % b
7   prim_add               a + b
8   prim_subtract          a - b
9   prim_lsh               a << b
10  prim_rsh               a >> b
11  prim_less              a < b                 (returns BOOL)
12  prim_lessorequal       a <= b
13  prim_greater           a > b
14  prim_greaterorequal    a >= b
15  prim_equal             a == b
16  prim_notequal          a != b
17  prim_and               a & b                 (bitwise)
18  prim_xor               a ^ b
19  prim_or                a | b
20  prim_land              a && b                (logical)
21  prim_lor               a || b
22  prim_assign            a = b                 (ref-assignment)
23  prim_apply             a ^^ b                (closure apply)
24  prim_by                a by b
25  prim_seqcat            a # b                 (sequence cat)
26  prim_plus              +a                    (unary, pos=0)
27  prim_minus             -a
28  prim_not               ~a                    (bitwise not)
29  prim_lnot              !a                    (logical not)
30  prim_deref             *ref
31  prim_cast              (type)a               (pos=0)
32  prim_strcat            s :: t                (string concat)
33  prim_ref               &a
34  prim_primitive         (primitive name args)  — fallback dispatcher
35  prim_reply             (primitive reply cont val)  — function return  (nonfunc)
36  prim_make              (primitive make kind args)  — allocate compound
37  prim_vector            (primitive make_vector ...) — allocate vector
38  prim_new               (primitive new type)  — fresh instance of type
39  prim_clone             (primitive clone obj) — duplicate object
40  prim_clone_vector      (primitive clone_vector obj n)
41  prim_isinstance        (primitive isinstance obj type)
42  prim_issubclass        (primitive issubclass type1 type2)
43  prim_index_object      obj[i]
44  prim_set_index_object  obj[i] = v             (nonfunc)
45  prim_meta_apply        (primitive meta_apply ...)
46  prim_destruct          (primitive destruct ...)  — tuple unpacking  (nonfunc)
47  prim_coerce            (primitive coerce type val)
48  prim_merge             (primitive merge a b)
49  prim_merge_in          (primitive merge_in a b)
50  prim_type_assert       (primitive type_assert val type)
51  prim_len               len(obj)
52  prim_sizeof            sizeof(type)
53  prim_sizeof_element    sizeof(element_type(t))  → SIZE
54  prim_typeof            typeof(val)
55  prim_typeof_element    typeof(element(t))
```

For the analysis semantics of each, see `fa.cc:add_send_edges_pnode`
(the big switch — [IFA.md](IFA.md) §5.5 calls out the highlights).

---

## 10. How a SEND becomes a primitive call

End-to-end example: Python source `a + b` becomes the SEND:

```
rvals = [sym_operator, a, sym_add, b]
lvals = [result]
pos = 1   (sym_add is at rvals[1])
```

1. **Frontend (build_if1)** emits the SEND via `if1_send` /
   `if1_operator`. At this point `code->prim == nullptr`.
2. **`if1_finalize`** walks the closure body, calls
   `Primitives::find(code)` which:
   - Sees `rvals[0] == sym_operator`.
   - `nargs - 3 = 1`, `pos = 1`, so `prim_map[1][1].get("+")` →
     `prim_add`.
   - Stores `code->prim = prim_add`.
3. **`PNode(code)`** copies `code->prim` to `pn->prim`.
4. **`FA::analyze`** runs `add_send_edges_pnode(pn, es)`. Sees
   `pn->prim != nullptr`, enters the primitive branch. The switch on
   `pn->prim->index` runs the `P_prim_add` case (numeric folding via
   `type_num_fold`).
5. **Codegen** (`cg.cc`) sees `Code::prim == prim_add` and emits
   `result = a + b;` (C-side).

For a registered primitive like `__pyc_c_call__`:

```
rvals = [sym_primitive, sym___pyc_c_call__, ret_type_sym, "fn_name", args...]
```

1. **`if1_finalize`** calls `Primitives::find` → `rvals[0] ==
   sym_primitive` → `prim_map[0][0].get("__pyc_c_call__")` →
   typically *not found* → fallback `prim_primitive`.
2. **`add_send_edges_pnode`** enters the `P_prim_primitive` case,
   reads `rvals[1]->name` ("__pyc_c_call__"), calls
   `prim_get(name)` → `RegisteredPrim`, runs its `tfn`.
3. **Codegen** runs the registered `cgfn` (`c_call_codegen`).

---

## 11. Adding a new primitive

### 11.1 Table primitive (universal, used by all frontends)

1. Add a line to `ifa/prim_data.dat`.
2. Run `make_prims` (regenerates `prim_data.{cc,h}`).
3. Add a case to the switch in `fa.cc:add_send_edges_pnode` (using the
   new `P_prim_X` macro).
4. Add codegen in `cg.cc` (and `llvm_primitives.cc`).
5. Rebuild.

### 11.2 Registered primitive (frontend-specific)

1. Declare the marker Sym in your frontend's symbol table (for pyc,
   add to `pyc_symbols.h` and `build_environment`).
2. Write the transfer function and codegen function.
3. Call `prim_reg(name, tfn, cgfn)` from your frontend's startup
   (`add_primitive_transfer_functions` in pyc).
4. Emit the SEND from your frontend: rvals = `(sym_primitive,
   sym_marker, ...)`.

Use 11.2 unless the primitive is meaningful to multiple frontends.

---

## 12. Pyc's registered primitives (current list)

From `python_ifa_main.cc:add_primitive_transfer_functions` plus
`pyc_symbols.h`:

| Name | tfn | cgfn | Purpose |
|---|---|---|---|
| `write` | `return_nil_transfer_function` | `write_codegen` | `print()` without newline |
| `writeln` | `return_nil_transfer_function` | `writeln_codegen` | `print()` with newline |
| `__pyc_c_call__` | `c_call_transfer_function` | `c_call_codegen` | Inline raw C call |
| `__pyc_format_string__` | `format_string_transfer_function` | `format_string_codegen` | f-string body |
| `__pyc_to_str__` | `to_str_transfer_function` | `to_str_codegen` | `str()` invocation |
| `to_string` | `return_string_transfer_function` | — | (no codegen; runtime invokes) |

`__pyc_c_call__`, `write`, `writeln`, `__pyc_format_string__`,
`__pyc_to_str__` have `is_visible = 1`. The first four can be emitted by
user code via the `pyc_compat` aliases or via `__pyc_*` direct
invocations.

Pyc-side helpers `__pyc_c_code__`, `__pyc_insert_c_code__`,
`__pyc_insert_c_header__`, `__pyc_include_c_header__` are NOT
registered primitives — they're handled at frontend lowering time
(`build_builtin_call_pyda` in `python_ifa_build_if1.cc`) and emit raw
C via `ctx.c_code` + `c_codegen_pre_file`.

---

## 13. Gotchas

### 13.1 The `pos` ambiguity
`pos` indicates the marker rval, not the receiver. For binary operators
the LHS is at slot 0 and the marker at slot 1 — `pos = 1`. Don't
confuse this with the dispatch convention in DISPATCH.md where `args[0]`
is the function position.

### 13.2 `nargs` includes the marker
A binary primitive has `nargs = 3` (lhs, op_marker, rhs). Walking
arguments in transfer functions starts at index 1 (or 2 for prim
form), not 0.

### 13.3 `arg_types[]` excludes the marker
But `nargs` counts it. So `arg_types[i]` describes `rvals[i+1]` for
`pos=0` or `rvals[i + (i < pos ? 0 : 1)]` for non-zero `pos`. The
analysis (`add_send_edges_pnode`) handles this with `if (i - 1 ==
p->prim->pos) continue;` skipping the marker slot.

### 13.4 Vararg primitives use `nargs - 2` as the map key
`prim_make`, `prim_reply`, `prim_clone_vector`, etc. have `nargs <
0` meaning "at least |n|." The lookup uses `nargs - 2` clamped to
`[0, 2]`, so vararg primitives with minimum 3 args register under
slot `[0]`. The `Primitives::find` logic in `prim.cc:25` only checks
`nargs - 3` (subtract one extra for the `sym_operator` marker); the
generated `prim_init` puts vararg prims in slot 0 of `prim_map[][]`.

### 13.5 `prim_primitive` is the universal fallback
A `(sym_primitive, name, ...)` SEND with an unknown `name` resolves to
`prim_primitive` (not nullptr). The analysis then queries
`registered_prims` for the name. **A missing registration is a fatal
`fail()` at analysis time**, not a silent no-op.

### 13.6 `is_functional` defaults true
Both `Prim::nonfunctional` and `RegisteredPrim::is_functional` default
to *functional* (i.e., DCE-eligible). If your new primitive has side
effects, you must mark it explicitly:
- Table: add `nonfunctional` to the `.dat` line.
- Registered: set `prim_reg(...)->is_functional = 0;` (the field is
  inverted vs. table — `is_functional = 0` means side-effecting).

### 13.7 `is_visible` controls user accessibility
A registered primitive without `is_visible` is callable only by the
frontend's internal lowering — user Python code can't reach it. Set
`is_visible = 1` if the primitive is intended for direct user
invocation (or via a compat shim).

### 13.8 `prim_data.{cc,h}` are generated but committed
They live in the repo so the build doesn't depend on `make_prims`
running first. If you edit them directly and forget to update the
`.dat`, the next `make_prims` invocation will revert your edit.

### 13.9 `Prim::args` is populated late
`Prim::args[i]` (the per-arg AType filter) is empty until
`fa.cc:initialize_primitives` runs at the start of analysis. Don't
read it during `if1_finalize` or earlier — it's all NULL.

### 13.10 No registered-prim version of `arg_types`
`RegisteredPrim` has no signature — the transfer function is solely
responsible for validating arguments. If you want
`ATypeViolation_PRIMITIVE_ARGUMENT` checks for a registered prim,
emit them yourself inside the `tfn` via `type_violation(...)`.

### 13.11 The fp at `cgfn` is unbuffered for most builds
`cg.cc` opens the output `FILE *fp` with `fopen`. Writes are buffered.
If your `cgfn` mixes `fputs` with `fprintf`, that's fine; but don't
mix in raw `write(fileno(fp), ...)` calls — flush ordering will surprise.

### 13.12 Primitives do NOT get per-call-site ES specialization

A `(sym_primitive, name, ...)` SEND resolves to a single Prim and runs
its transfer function in the *caller's* EntrySet. Unlike a user-fun
SEND, the primitive doesn't get its own ES per call site, so the
splitter cascade (`split_ess_for_type` and friends — see
[IFA.md](IFA.md) §splitter) cannot specialize the primitive's
behavior per receiver-CS. The transfer function instead iterates *all*
CSes flowing into the relevant rval and unions their results into a
single output AVar.

**Concrete consequence for indexing.** `prim_index_object` (and its
companion `prim_set_index_object`) iterate every CS in the `vec` rval's
out-set and call `flow_vars(get_element_avar(cs), result)` per CS. The
result AVar receives the *union* of all per-CS element types in one
shot. If the same `vec` AVar carries instances of `V<int>` and
`V<float>`, the read site's lval is `int | float` — and no later
ES-level split can disentangle that, because the union happened inside
a single primitive call.

**The escape hatch is method dispatch.** `obj[i]` in pyc lowers not
to a direct `prim_index_object` SEND but to a method call:
`obj.__getitem__(i)`. That's a user-fun-shaped SEND; the splitter
*can* fork it per-receiver-CS via the normal type-stage / mark
machinery, giving each list-CS its own specialized `__getitem__`
resolution. The simple inliner ([OPTIMIZE.md](OPTIMIZE.md)
§simple_inlining) then removes the dispatch overhead in the
monomorphic case, so there's no runtime cost for the indirection.

**Implication for frontend / IR-builder authors.** When the
polymorphism at a primitive call site needs per-CS specialization,
emit a method-dispatch SEND, not the primitive directly. The
primitive is the right tool for monomorphic-by-construction
operations (arithmetic on a single numeric type, identity, marker
sends like `reply`); the method-dispatch path is the right tool
anywhere CS identity carries real type information that downstream
analysis depends on.

This design tradeoff — keeping primitives simple at the cost of
per-call ES specialization — is deliberate. Adding ES-level splitting
to primitives would mean per-CS transfer-function specialization and
matching codegen variants, expanding the analysis surface
substantially. The method-dispatch detour gives the same end result
(per-CS specialization where it matters) with no new machinery, and
the inliner buys back the runtime overhead.

---

## 14. Backend coverage matrix

The single dashboard for "which primitives does each backend handle, and which have pinpoint fixtures?" Update this alongside any change to `cg.cc:write_c_prim` or `llvm_primitives.cc:write_llvm_prim`. See
[codegen/AUDIT.md §3](codegen/AUDIT.md) for the backend-gap discussion, and
[codegen/CODEGEN_PLAN.md](codegen/CODEGEN_PLAN.md) §6 (phase 3) for the
order of LLVM-side primitive implementation work.

Status legend:
- ✓ — implemented.
- ⚠ — implemented but with known caveats (see notes column).
- ✗ — not implemented.
- — — not applicable (e.g., arithmetic primitives don't have a single
  "fixture" but are covered by the binary-op switch).

| Primitive | C backend | LLVM backend | Pinpoint fixture | Notes |
|---|---|---|---|---|
| `prim_reply` | ✓ `cg.cc:189` | ✓ `llvm_primitives.cc:589` | `01_baseline.ir` (both) | Function return. |
| `prim_make` (tuple/list) | ✓ `cg.cc:193` | ⚠ `llvm_primitives.cc:355` | — | LLVM uses `malloc`, should use GC allocator. |
| `prim_period` (getter) | ✓ `cg.cc:222` | ⚠ `llvm_primitives.cc:429` | `04_getter.ir` (C only) | LLVM `fail`s on non-pointer obj_val (see attempted `codegen-llvm/06_getter.ir`). |
| `prim_setter` | ✓ `cg.cc:262` (issue 011) | ✗ | `03_setter.ir` (C), `05_setter.ir` (LLVM, framework only) | LLVM doesn't emit the store; phase 3 target. |
| `prim_apply` | ✓ `cg.cc:293` (asserts unimplemented) | ✗ | — | Closure apply. Asserted-unimplemented in C, missing in LLVM. |
| `prim_index_object` | ✓ `cg.cc:320` | ✗ | — | `a[i]`. Phase 3 target on LLVM. |
| `prim_set_index_object` | ✓ `cg.cc:353` | ✗ | — | `a[i] = v`. Phase 3 target on LLVM. |
| `prim_new` | ✓ `cg.cc:379` | ✗ | (used in 03/04/05/06 C fixtures + LLVM fixtures) | Fresh instance. Phase 3 target on LLVM. |
| `prim_assign` | ✓ `cg.cc:386` | ✗ | — | Ref assignment. Phase 3 target on LLVM. |
| `prim_len` | ✓ `cg.cc:392` | ✗ | — | `len(obj)`. Phase 3 target on LLVM. |
| `prim_clone` | ✓ `cg.cc:404` | ✗ | `05_clone.ir` (C), `07_clone.ir` (LLVM, framework only) | LLVM doesn't emit clone call; phase 3 target. |
| `prim_clone_vector` | ✓ `cg.cc:404` | ✗ | — | Same path as `prim_clone`. |
| `prim_sizeof` | ✓ `cg.cc:420` | ✗ | — | Constant emission. Phase 3 target on LLVM. |
| `prim_sizeof_element` | ✓ `cg.cc:433` | ✗ | — | Same. |
| `prim_destruct` | ✓ `cg.cc:486` | ✗ | — | Tuple unpacking. Phase 3 target on LLVM. |
| `prim_primitive` (registered dispatch) | ✓ `cg.cc:452` (via `RegisteredPrim->cgfn`) | ⚠ `llvm_primitives.cc:493` (print/println hardcoded only) | — | LLVM has no `RegisteredPrim` LLVM-side hook. Phase 3 §3.2. |
| `prim_operator` | — | ⚠ `llvm_primitives.cc:230` (hardcoded `"Output: %d\n"` printf) | — | The LLVM branch looks like a debug placeholder; phase 3 §3.4. |
| Arithmetic / comparison / logical (`prim_add`, `prim_mult`, `prim_less`, ...) | ✓ via runtime helpers (`cg.cc` registered prims) | ✓ `llvm_primitives.cc:255-354` (one switch) | — | Signed-only on LLVM; unsigned variants missing. |
| Type-side primitives (`prim_isinstance`, `prim_issubclass`, `prim_typeof`, `prim_meta_apply`, `prim_coerce`, `prim_merge`, `prim_merge_in`, `prim_type_assert`) | ✓ via runtime helpers | ✗ | — | Pyc routes through `prim_primitive`; needs the LLVM-side `RegisteredPrim` hook from phase 3 §3.2. |

### Known LLVM-backend gaps surfaced by phase 1 fixturing

While creating the codegen-llvm parallels (CODEGEN_PLAN phase 1.2),
two LLVM-side bugs surfaced and are filed for phase 3:

- `(send @operator obj @period #field => r)` after a setter on a
  freshly-`@new`'d record — `getLLVMValue(obj_var)` returns a
  non-pointer and `P_prim_period` `fail`s. Tried as
  `codegen-llvm/06_getter.ir`; dropped.
- Minimal record-only fixture with no constants and no setter
  (`@new + move`) crashes the LLVM backend with SIGTRAP during
  printer teardown. Tried as `codegen-llvm/08_sum_type.ir`;
  dropped. The C-side equivalent `codegen-c/06_sum_type.ir`
  works fine.

### Notes on the codegen-c pinpoint fixtures

The codegen-c fixtures (`03_setter.ir`, `04_getter.ir`, `05_clone.ir`,
`06_sum_type.ir`) all currently produce goldens of `bytes=338` —
identical-modulo-fn-id to `01_baseline.ir`. The reason: the synthesized
`__main__` wrapper in `fa_setup_environment` doesn't observe top's
return, so DCE elides every primitive operation whose result only flows
to top's return. The fixtures still:

- Document the `.ir` DSL spelling for each primitive (input).
- Detect regressions in the framework-level emission (function naming,
  prototype emission, etc.).
- Serve as templates for when a future "preserve" mode (a CODEGEN_PLAN
  phase 4 target) makes operations observable.

The codegen-llvm parallels have richer goldens (3885-4926 bytes) because
the LLVM backend emits `alloca` instructions and struct type
definitions regardless of DCE — so the type-emission paths ARE locked
in by those goldens. See `tests/ir/codegen-llvm/05_setter.ir.codegen-llvm.expected`
vs `01_baseline.ir.codegen-llvm.expected` for the difference.

---

## 15. Primitive emission contract

The seam between the per-primitive analyzer switch in
`fa.cc:add_send_edges_pnode` and the per-primitive codegen
switches in `cg.cc:write_c_prim` / `llvm_primitives.cc:write_llvm_prim`
is a contract every primitive obeys. Documenting it lets a new
backend (or a refactor like CODEGEN_PLAN phase 5's unified
dispatch) check correctness mechanically.

The contract has three parts.

### 15.1 SEND PNode shape

Every primitive call lowers to a `Code_SEND` PNode with the
following `rvals` / `lvals` layout:

| Position | Meaning |
|---|---|
| `rvals[0]` | Either the dispatched user function's sym (for `(send fn args)`) or the `@primitive` / `@operator` marker (for primitive calls). |
| `rvals[1]` | For `@primitive`-marked SENDs: the primitive name (e.g. `@new`, `@clone`, `print`, `println`). For binary operators (`@operator …`): the receiver. |
| `rvals[2]+` | Primitive arguments. The argument offset `o` (from `cg.cc:184`) is 2 if `rvals[0]->sym == sym_primitive`, else 1. |
| `lvals[0]` | The result. `lvals.n == 0` for void-result primitives (e.g. `@reply`); `lvals.n == 1` otherwise. Multi-result primitives (rare; `@destruct`) carry `lvals[0..n-1]`. |

### 15.2 Per-primitive contract details

| Primitive | `rvals` layout | `lvals[0]` semantics | `live` gating? |
|---|---|---|---|
| `@new <T>` | `[__primitive, new, T]` | New instance of T (pointer). | always live; codegen must emit. |
| `@clone <obj>` | `[__primitive, clone, obj]` | Copy of obj (pointer of same type). | always live. |
| `@operator obj @period <sym>` | `[__operator, obj, period, sym]` | `obj.field` value. | always live. |
| `@operator obj @setter <sym> <val>` | `[__operator, obj, setter, sym, val]` | The `val` that was assigned (issue 011 Option A semantics). | emit lvalue assignment only when `lvals[0]->live` is true. |
| `@primitive @index_object obj idx` | `[__primitive, index_object, obj, idx, …]` | `obj[idx]`. Multiple `idx` rvals for nested indexing. | `lvals[0]->live` gates the result store; the array bounds check (if any) emits regardless. |
| `@primitive @set_index_object obj idx val` | `[__primitive, set_index_object, obj, idx, val]` | The `val` (analyzer flows val to result). | gated. |
| `@primitive @assign lhs rhs` | `[__primitive, assign, lhs, rhs]` | The `rhs` (assignment returns the value). | gated. |
| `@primitive @len obj` | `[__primitive, len, obj]` | int64 length. | gated. |
| `@primitive @sizeof T` / `@sizeof_element T` | `[__primitive, sizeof, T]` | int64 sizeof. | gated. |
| `@primitive @reply cont ret` | `[__primitive, reply, cont, ret]` | (no lvalue — this IS the function return). | n/a. |
| `@primitive @make kind args…` | `[__primitive, make, kind, args…]` | Allocated compound (tuple, list, vector). | gated. |
| `@primitive @destruct args…` | `[__primitive, destruct, args…]` | Decomposes lvals[0..n-1] from a tuple. | each lval gated individually. |
| `@primitive @print …` / `@println …` | `[__primitive, print/println, args…]` | int (printf return code; unused in practice). | gated; but the side effect is always emitted. |

### 15.3 Backend obligations

For each primitive in §15.2, the C backend (`cg.cc:write_c_prim`)
and the LLVM backend (`llvm_primitives.cc:write_llvm_prim`)
must:

1. **Emit the operation's side effect** (struct write, array
   store, printf call, etc.) unconditionally. This happens
   even when `lvals[0]->live` is false — the result might be
   unused but the side effect matters.
2. **Emit the lvalue assignment** only when `lvals[0]->live`
   is true. For setter-style primitives where the analyzer
   flows `val` to the lvalue (see issue 011), the lvalue
   assignment must use the val (not the receiver).
3. **Respect `fruntime_errors`**: when set, emit runtime
   checks (e.g. `assert(!"runtime error: getter not resolved")`)
   instead of `fail`ing at compile time.

The `PrimEmitter` interface in `codegen_common.h` reserves a
namespace for a future unified dispatch (CODEGEN_PLAN phase 5)
that codifies this contract as virtual methods. Until that
lands, the inline `switch (n->prim->index)` blocks in each
backend's `write_*_prim` are the authoritative implementation.

---

## 16. Symptom → start-here

| Symptom | Start here |
|---|---|
| "undefined primitive transfer function 'X'" | Frontend forgot `prim_reg("X", ...)` |
| "wrong type from `+` on user ints" | `type_num_fold` in `fa.cc`, plus `coerce_num` in `sym.cc` |
| "DCE removed my side-effecting call" | Missing `nonfunctional` flag (table) or `is_functional = 0` (registered) |
| "primitive name found but wrong overload picked" | `Primitives::find` — verify the SEND rvals layout matches `pos`/`nargs` |
| "new primitive emits NULL from `if1_finalize`" | Map slot wrong: `prim_map[nargs-2-1?][pos]` — review `prim.cc:25` carefully |
| "codegen produces wrong C for a registered prim" | The `cgfn` — also confirm `cg.cc` even calls it (registered prims hit the `P_prim_primitive` path) |
| "regenerated prim_data lost my changes" | You edited `.cc`/`.h` directly; edit `.dat` instead |
| "isinstance returns wrong" | `P_prim_isinstance` in `fa.cc:add_send_edges_pnode` — `cs->sym->meta_type->implementors.in(...)` check |

---

## 17. References

- `ifa/if1/prim.cc` + `prim.h` — `Prim`, `Primitives`,
  `RegisteredPrim`, `prim_reg`, `prim_get`.
- `ifa/if1/prim_data.cc` + `prim_data.h` — generated table.
- `ifa/prim_data.dat` — source of the table.
- `ifa/codegen/make_prims.cc` — the generator.
- `ifa/analysis/fa.cc` `add_send_edges_pnode` — the big switch on
  `Prim::index`. See [IFA.md](IFA.md) §5.5.
- `ifa/codegen/cg.cc` — C codegen per primitive.
- `ifa/codegen/llvm_primitives.cc` — LLVM codegen per primitive.
- `python_ifa_main.cc:add_primitive_transfer_functions` — pyc's
  registered primitives.
- Sister docs: [IR.md](IR.md), [IFA.md](IFA.md), [DISPATCH.md](DISPATCH.md),
  [PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md) §10.
