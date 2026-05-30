# CAST — Numeric Coercion & Cast Tables

A working reference for `ifa/if1/{cast_code,check_cast,make_cast_code}.cc`
plus the constant-folding and numeric-coercion machinery in `num.cc`,
plus the analysis-side `coerce_num` in `fa.cc`. The "cast" story
crosses three locations and one build-time generator; this doc unifies
them.

Sister docs: [IR.md](IR.md) §3, §7 (`Immediate`, `Type_kind`),
[IFA.md](IFA.md) §7 (`type_num_fold`),
[PRIMITIVES.md](PRIMITIVES.md) (`PRIM_TYPE_ANY_NUM_AB`).

---

## 1. In one paragraph

When the analysis folds binary numeric operators (e.g., constant
`1 + 2.0`), it must coerce both operands to a common type, perform the
operation, and produce a result of the LUB type. The coercion step
lives in `coerce_immediate(from, to)` — a 4×8×4×8 = 1024-entry switch
table written by `make_cast_code` from a 4×8 numeric-type matrix. The
matrix encodes which `(const_kind, num_index)` slots are valid (e.g.,
no signed-int-7, no float-48). `check_cast` is the predicate version
returning whether a coercion is well-formed. The generator
`make_cast_code.cc` is a 110-line C++ program built once and run to
produce `cast_code.cc` + `check_cast.cc`, both committed to the
repo. At analysis time, `coerce_num(a, b)` (in `fa.cc`) does the
type-level promotion (Sym → Sym), while `coerce_immediate` does the
value-level conversion. Together they implement C-style usual
arithmetic conversions: float ≻ complex, signed ≻ unsigned, widest ≻
narrowest; result never narrower than 32-bit.

---

## 2. File map

```
ifa/if1/
├── num.h                  Immediate, IF1_num_kind, IF1_*_type enums, prototypes
├── num.cc                 sprint_imm, fold_constant, coerce_immediate body,
│                          fold_result (type-level LUB), DO_FOLD macros
├── make_cast_code.cc      generator (build-time only)
├── cast_code.cc           generated: body of coerce_immediate (4-level switch)
└── check_cast.cc          generated: body of check_coerce_immediate

ifa/analysis/
└── fa.cc                  coerce_num (Sym→Sym promotion), type_num_fold
                           (per-type folding driver)
```

The generator is run once by `make $(MAKE_CAST_CODE) && ./make_cast_code`
in `ifa/if1/`; the produced `cast_code.cc` and `check_cast.cc` are
committed to the repo so a normal build doesn't need to regenerate.

---

## 3. The numeric type matrix

`Immediate::const_kind` (4 bits) plus `num_index` (3 bits) names every
concrete numeric type:

| `const_kind` | meaning | valid `num_index` values |
|---|---|---|
| `IF1_NUM_KIND_NONE` (0) | empty Immediate | n/a |
| `IF1_NUM_KIND_UINT` (1) | unsigned int / bool | `IF1_INT_TYPE_{1,8,16,32,64}` |
| `IF1_NUM_KIND_INT` (2) | signed int | `IF1_INT_TYPE_{1,8,16,32,64}` |
| `IF1_NUM_KIND_FLOAT` (3) | float | `IF1_FLOAT_TYPE_{32,64,128}` |
| `IF1_NUM_KIND_COMPLEX` (4) | complex | `IF1_FLOAT_TYPE_{32,64}` |
| `IF1_CONST_KIND_STRING` (5) | (overflow into the enum) | n/a — `v_string` |
| `IF1_CONST_KIND_SYMBOL` (6) | (overflow into the enum) | n/a — `v_string` |

`IF1_INT_TYPE_1` is the bool slot. `IF1_INT_TYPE_8/16/32/64` are
standard widths. `IF1_FLOAT_TYPE_*` includes 16/32/48/64/80/96/112/128
but only 32/64/128 are wired up in the implementation. Most enum
values exist for completeness; the unused ones produce `assert(!"case")`
branches in the generated code.

The build-time table in `make_cast_code.cc:19`:

```c
static const char *num_kind_string[4][8] = {
    {0, 0, 0, 0, 0, 0, 0, 0},                                   // NONE
    {"bool", "uint8", "uint16", "uint32", "uint64", 0, 0, 0},   // UINT
    {"bool", "int8",  "int16",  "int32",  "int64",  0, 0, 0},   // INT
    {0, "float32", 0, "float64", 0, 0, 0, "float128"}           // FLOAT
};
```

Zero entries → invalid; non-zero entries → the C type name used in the
generated switch (`v_uint8`, `v_int32`, `v_float64`, etc.).

`IF1_NUM_KIND_COMPLEX` is **NOT** in this table; the cast generator
only handles UINT/INT/FLOAT. Complex coercion currently falls through
to the `default: assert(!"case"); break;` arm. If you add complex
literals, extend the matrix and regenerate.

---

## 4. The generator (`make_cast_code.cc`)

A 110-line standalone C++ program. Two functions:

### 4.1 `make_cast_code()` (`make_cast_code.cc:28`)

Generates `cast_code.cc`: the body of `coerce_immediate(from, to)`.
Emits a 4-level switch:

```c
switch (to->const_kind) {
  case 1: switch (to->num_index) {
    case 0:   // bool
      switch (from->const_kind) {
        case 1: switch (from->num_index) {
          case 0: to->v_bool = (bool)!!from->v_bool; break;
          case 1: to->v_bool = (bool)!!from->v_uint8; break;
          ...
        }
        case 2: switch (from->num_index) { ... }
        case 3: switch (from->num_index) { ... }
      }
    case 1:   // uint8
      ...
  }
  case 2: switch (to->num_index) { ... }   // INT side
  case 3: switch (to->num_index) { ... }   // FLOAT side
}
```

Special case: when the target is `bool` (`IF1_NUM_KIND_UINT` +
`IF1_INT_TYPE_1`), the cast uses `!!from->v_X` (the double-bang idiom
for "any nonzero → true"). All other casts use a plain C cast
`(to_type)from_value`.

### 4.2 `make_check_cast()` (`make_cast_code.cc:69`)

Generates `check_cast.cc`: the body of `check_coerce_immediate`.
Same structure but returns `true` for any valid `(to_kind, to_idx,
from_kind, from_idx)` quadruple and `false` for invalid ones.

### 4.3 Build & run

```bash
cd ifa
make make_cast_code        # builds the binary from codegen/make_cast_code.cc...
                           #  actually it's at if1/make_cast_code.cc per Makefile
                           #  (the Makefile rule is `if1/cast_code.cc if1/check_cast.cc:
                           #   $(MAKE_CAST_CODE)`)
cd if1
../make_cast_code          # produces cast_code.cc, check_cast.cc in cwd
```

The Makefile rule auto-runs the generator if the outputs are stale.
You only need to manually invoke if you've edited `make_cast_code.cc`
or want to force regeneration.

---

## 5. The runtime API (`num.h` / `num.cc`)

### 5.1 `coerce_immediate(from, to)` (`num.cc:289`)

```c
void coerce_immediate(Immediate *from, Immediate *to) {
#include "cast_code.cc"
}
```

The whole function body is the generated switch. The caller pre-sets
`to->const_kind` and `to->num_index` to the desired result type; the
function writes the appropriate `to->v_*` field by casting the matching
`from->v_*` field. After return, the union slot at the index for the
target type holds the coerced value.

**Convention:** the caller is responsible for keeping `to->const_kind`
and `to->num_index` set correctly. If they're zero or invalid, the
generated `assert(!"case")` fires.

### 5.2 `check_coerce_immediate(from, to)` (`num.cc:293`)

Predicate: true iff the (from, to) pair is a defined conversion.
Used by analysis-time validation before calling `coerce_immediate` to
avoid `assert` aborts. Currently has no callers in `fa.cc` —
present for use by future range/overflow analysis.

### 5.3 `fold_result(im1, im2, imm)` (`num.cc:669`)

Type-level LUB. Given two operand Immediates, sets `imm->const_kind`
and `imm->num_index` to the type the result should have, following
C's usual arithmetic conversion rules:

```
if same const_kind:    pick the larger num_index
if mixed with FLOAT:   FLOAT wins; pick width by int precision
if mixed signedness:   signed wins; clamp result to ≥ 32 bits
```

Specifically:
- Same `const_kind` → result is whichever has larger `num_index`.
- One is FLOAT → result is FLOAT; result width depends on the int's
  precision (≤ float's precision: keep float; ≥ 32 bits: float32; else
  float64).
- Mixed UINT/INT → result is INT; width is the larger of `num_index` if
  both ≥ 64 → `IF1_INT_TYPE_64`; ≥ 32 → `IF1_INT_TYPE_32`; ≥ 16 →
  `IF1_INT_TYPE_16`; ≥ 8 → `IF1_INT_TYPE_8`; else `IF1_INT_TYPE_1`.

This is computed *before* coercing values, so the caller knows what
type to coerce *into* before calling `coerce_immediate(im1, &coerce);
coerce_immediate(im2, &coerce);`.

### 5.4 `fold_constant(op, im1, im2, imm)` (`num.cc:725`)

The driver that combines everything:
1. Take two operand Immediates and a `P_prim_*` opcode.
2. Compute the result type via `fold_result`.
3. Coerce both operands via `coerce_immediate`.
4. Apply the operation using the `DO_FOLD` / `DO_FOLDI` / `DO_FOLDF` /
   `DO_FOLD1` / `DO_FOLD1I` macros (which expand into per-type
   switches over the union slots).
5. Store the result in `imm`.

`DO_FOLD` is for any numeric, `DO_FOLDI` is integer-only (e.g.,
modulo, shifts), `DO_FOLDF` is float-only (e.g., `pow`). The
single-operand variants `DO_FOLD1` / `DO_FOLD1I` are for unary ops
like negate, complement, logical not.

See `fold_constant` in [IR.md](IR.md) §7 for the call shape; this
function is invoked by `fa.cc:add_send_edges_pnode` when both args
to a binary numeric prim are constants.

---

## 6. Analysis-time coercion (`coerce_num`, `fa.cc:314`)

```c
Sym *coerce_num(Sym *a, Sym *b);
```

The Sym-level analog of `fold_result`. Given two argument Syms,
returns the result Sym (always a builtin type — `sym_int32`,
`sym_float64`, `sym_string`, etc.). Rules:

```
if a == b:                                 return a
if either is sym_string:                   return sym_string
if same num_kind:                          larger num_index wins
swap so a is the "wider" kind (FLOAT > COMPLEX > INT > UINT)
if a is COMPLEX:
  if b is FLOAT with same precision:       return a
  pick complex width by int precision
if a is FLOAT:
  if b is small int:                       return a
  pick float width by int precision
mixed signed/unsigned:
  ≥ 64 bits:                               sym_int64
  ≥ 32 bits:                               sym_int32
  ≥ 16 bits:                               sym_int16
  ≥ 8 bits:                                sym_int8
  else:                                    sym_bool
```

This is invoked by `type_num_fold` (`fa.cc:359`), which itself is
called by `add_send_edges_pnode` for any binary numeric primitive
(`prim_add`, `prim_subtract`, `prim_mult`, ...). The result is then
either propagated as the result AType, or — if both args are
singleton constants — passed through `fold_constant` (the value-
level coercion + arithmetic) and the result is `imm_constant(...)`.

---

## 7. End-to-end example: `int32 + float64`

Source: `let x: int32 = 1; let y: float64 = 2.5; let z = x + y;`

Analysis-time:
1. The SEND for `+` has `Prim *prim = prim_add` (table primitive).
2. `add_send_edges_pnode` matches the `P_prim_add` case.
3. Both rvals have non-empty `out` ATypes. Each has one CreationSet
   with `sym->imm.const_kind` set.
4. `type_num_fold(prim, a->out, b->out)`:
   - Intersects each with `anynum_kind` (no narrowing needed here).
   - For each (acs, bcs) pair: `coerce_num(int32, float64)` →
     `float64`.
   - Builds the result AType from `float64->abstract_type->v[0]`.
5. Both args are singleton constants:
   - `fold_constant(P_prim_add, &x_imm, &y_imm, &result_imm)`:
     - `fold_result(&x_imm, &y_imm, &coerce)` → `(FLOAT, 64)`.
     - `coerce_immediate(&x_imm, &coerce)` → `coerce.v_float64 =
       (float64)x_imm.v_int32 = 1.0`.
     - `coerce_immediate(&y_imm, &coerce_y)` → `coerce_y.v_float64 =
       2.5` (no-op).
     - Apply `DO_FOLD(+)` → `result_imm.v_float64 = 1.0 + 2.5 = 3.5`.
6. `update_in(result, make_constant(result_imm, sym_float64))`.

Codegen time:
- The Var `z` has `type = sym_float64`, `constant = imm_constant(...)`.
- `cg.cc:write_c` declares `double z;` (well, `_CG_float64 z;`).
- The MOVE emits `z = 3.5;` (constant substituted in).

If x and y weren't constant, step 5 would skip and emit
`z = _CG_add(...)` at codegen.

---

## 8. Gotchas

### 8.1 The 4×8 matrix vs the enum
The matrix is 4 rows (NONE, UINT, INT, FLOAT). The
`IF1_num_kind` enum has 5 values (NONE..COMPLEX); the
generator silently ignores COMPLEX. Add a COMPLEX row to the matrix
and regenerate if you ever fold complex constants. Currently complex
values flow through the analysis untyped at the immediate level —
the type-level path (`coerce_num`) handles complex, but the
value-level (`coerce_immediate`) doesn't.

### 8.2 `IF1_INT_TYPE_1` is bool
The bool slot is `num_index = 0` (under either UINT or INT). The
generator special-cases bool destinations to use `!!` for proper
truthiness. Anywhere outside that special case, bool-as-a-number
follows ordinary integer semantics.

### 8.3 `IF1_NUM_KIND_NONE` is the sentinel
A zero `const_kind` means the Immediate is unset. The generator's
top-level switch has `case 1, 2, 3` only; passing `const_kind = 0`
hits the `default: assert(!"case")` branch. This is intentional:
no-op or uninitialized Immediates shouldn't be passed to
`coerce_immediate`.

### 8.4 The generator is hand-edited, not parameterized
The matrix is hard-coded. Adding a new type (e.g., `int128`) needs:
1. Add the type name to `num_kind_string[][]`.
2. Add the corresponding enum value to `num.h`'s `IF1_int_type`.
3. Add the corresponding union field to `Immediate` (e.g.,
   `int128 v_int128`).
4. Add the appropriate `DO_FOLD*` macro branches in `num.cc`.
5. Add the size/alignment in `if1_set_int_type` etc. in `sym.cc`.
6. Regenerate.
7. Update `coerce_num` and `fold_result` to handle the new precision
   level.

Don't skip steps 4-7 — the generated `cast_code.cc` alone isn't
enough.

### 8.5 `fold_result` and `coerce_num` are parallel but separate
`fold_result` works on Immediates; `coerce_num` works on Syms.
Adding a new numeric type requires updating BOTH. They have the same
structure but the matching is by-eye, not enforced. Bug class:
adding a new type to one but not the other → analysis-time confusion.

### 8.6 The `check_cast.cc` predicate is unused
No code in the analysis or codegen calls `check_coerce_immediate`.
It's defined and compiled, but `grep check_coerce_immediate` returns
only the definition. If you find yourself wanting "is this coercion
safe?" semantics, the function is ready — just call it.

### 8.7 String/symbol Immediates don't go through cast_code
`IF1_CONST_KIND_STRING` (5) and `IF1_CONST_KIND_SYMBOL` (6) are
*not* in the cast matrix. They use `v_string` directly. Cast paths
for strings are handled by `coerce_num` returning `sym_string`
and `coerce_to` in `sym.cc` doing the actual coercion at the type
level.

### 8.8 Complex coercion is partial
`coerce_num` handles complex at the type level. `coerce_immediate`
does not. If a program with complex-constant folding ever arrives,
the analysis will type it correctly but the constant folding will
abort. Currently no pyc test or V test exercises complex constants
enough to trip this — but the gap is there.

### 8.9 Float precision in `fold_result`
The integer→float width mapping (`int_type_precision[i] <=
float_type_precision[f]`) determines whether an int operand keeps
its native float counterpart or gets widened. The constants
`int_type_precision[]` and `float_type_precision[]` are in `num.h`
(`{1, 8, 16, 32, 64}` and `{16, 32, 48, 64, 80, 96, 112, 128}`).
Make sure those arrays stay in sync with the enum values; the
generator doesn't read them, so a mismatch silently produces wrong
arithmetic.

### 8.10 Generated files are NOT auto-rebuilt on enum changes
`make` triggers regeneration only when `make_cast_code.cc` itself
changes (the Makefile rule). Editing `num.h`'s enums doesn't trigger
regeneration. After enum changes, manually `cd if1; ../make_cast_code`.

---

## 9. Symptom → start-here

| Symptom | Start here |
|---|---|
| "assert(!"case") fired in coerce_immediate" | The (from_kind, from_idx, to_kind, to_idx) hit an unmapped slot; check the matrix in `make_cast_code.cc` |
| "wrong precision after fold" | `fold_result` — verify the LUB rule for the operand-kind pair |
| "wrong type returned from analysis fold" | `coerce_num` in `fa.cc` — the Sym-level analog |
| "complex constant arithmetic crashes" | `coerce_immediate` matrix lacks complex; complete the gap |
| "constant arithmetic produces unexpected sign" | `DO_FOLD*` macros — pick the right macro for signed vs unsigned |
| "regenerated tables but build fails" | Make sure `cast_code.cc` is in the right directory (`if1/cast_code.cc`); regeneration runs in `if1/` cwd |
| "string concatenation type-folds wrong" | `coerce_num` returns `sym_string` if either is string; `fold_constant` doesn't handle string concat (that's a separate primitive) |
| "bool used as int and lost a bit" | The `!!` double-bang in the bool-target case clears info; check whether the destination should really be bool |

---

## 10. References

- `ifa/if1/num.{cc,h}` — Immediate, fold_result, fold_constant,
  coerce_immediate body.
- `ifa/if1/make_cast_code.cc` — build-time generator.
- `ifa/if1/cast_code.cc` — generated: body of coerce_immediate.
- `ifa/if1/check_cast.cc` — generated: body of check_coerce_immediate.
- `ifa/analysis/fa.cc` `coerce_num`, `type_num_fold` — type-level
  coercion driver. See [IFA.md](IFA.md) §7.
- `ifa/if1/sym.cc` `coerce_to`, `if1_set_int_type`,
  `if1_numeric_size`, `if1_numeric_alignment` — Sym-level numeric
  metadata.
- ISO C standard §6.3.1.8 ("Usual arithmetic conversions") — the
  reference rule that `coerce_num` and `fold_result` approximate.
- Sister docs: [IR.md](IR.md) §3 (Type_kind, Sym flags), §7 (Immediate),
  [IFA.md](IFA.md) §7 (constants and primitives),
  [PRIMITIVES.md](PRIMITIVES.md) (the `PRIM_TYPE_ANY_NUM_*` slots
  consumed by numeric prims).
