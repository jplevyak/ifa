# Issue 015: pyc has no frontend opt-in for `is_value_type` POD records

**Status:** open (feature gap, not a bug).
**Affects:** the pyc Python frontend
(`python_ifa_build_syms.cc:gen_class_pyda`,
`python_ifa_main.cc`), `Sym::is_value_type`,
`ifa/codegen/llvm.cc:getLLVMVarType`.
**Surfaced while:** reviewing the getLLVMVarType migration
(`ifa/codegen/CODEGEN_PLAN.md` references commit 06bec4a) and
considering whether `is_value_type` should be the mechanism for
the value-vs-pointer codegen choice.

## What's wired up today

`Sym::is_value_type` (`sym.h:87`) is documented in `IR.md:125` as
"Pass-by-value." Its lifecycle:

- **Set** explicitly on `sym_value` and `sym_anynum` in
  `ast.cc:521-523`.
- **Lifted transitively** through `Sym::implements` by
  `set_value_for_value_classes` (`ast.cc:524-538`) — fixed-point
  iteration. In practice that's the numeric hierarchy: int*,
  uint*, float*, complex, bool, and anything that implements
  them.
- **Consumed** at `ast.cc:338` (structural type hierarchy
  computation: `Type_RECORD && is_value_type` are structural
  records), at `ast_to_if1.cc:1521` (V-language type-check), and
  — after this issue's referenced commit — at
  `ifa/codegen/llvm.cc:getLLVMVarType` (the POD-record override:
  `Type_RECORD && is_value_type` keeps value semantics; default
  is `ptr`).

## What's missing

**The pyc Python frontend never sets `is_value_type` on a
user-defined class.** No `@struct`-style decorator, no
inheritance from a value-class base, no implicit promotion. So
the codegen seam exists but is unreachable from pyc source:

```python
class A:
  x: int = 2
  y: int = 3
# A is Type_RECORD, is_value_type=0 → pyc allocates instances
# on the GC heap, passes pointers.
```

Compare to V the language, which has a separate `Sym::is_structure`
bit (`sym.h:88`, `IR.md:128`) for "C-compatible structure" — V
exposes both POD records and value-record semantics in its
surface syntax. Pyc has no analogous construct yet.

## Why this matters

The default-to-pointer model is correct for nearly every Python
program: classes are heap objects, the GC manages lifetime, and
identity is by reference. But there are real cases where a POD
opt-in would be useful:

- **Numeric vector types** (`vec3`, `mat4`, `quat`) where the
  user wants struct-on-the-stack performance and `==` to mean
  structural equality.
- **Closure capture optimization** — small `dataclass`-style
  Plain-Old-Data captures avoid an extra GC allocation.
- **Interop with C** — pyc's `__pyc_c_call__` primitive
  (`python_ifa_main.cc:42-64`) needs to pass struct-by-value to
  C functions; today that's awkward because everything is a
  pointer.

The codegen seam is now in place; the pyc-side opt-in is the
missing piece.

## Proposed design

Three approachable shapes, ranked by surface-area:

### Option A — `@pyc_struct` decorator (smallest)

A new builtin decorator in `__pyc__/00_runtime.py`:

```python
@pyc_struct
class Vec3:
  x: float
  y: float
  z: float
```

Frontend handling in `python_ifa_build_if1.cc:build_if1_pyda`
(parallel to the existing `@vector("s")` decorator handling,
which sets `cls->is_vector = 1` and `cls->element`):

```cpp
// In the decorator scan loop:
if (is_named_decorator(d, "pyc_struct")) {
  cls->is_value_type = 1;
  cls->is_structure = 1;  // V's parallel bit; harmless to set
}
```

The IFA's existing `set_value_for_value_classes` lift already
propagates the bit through `implements`, so subclasses inherit.

### Option B — Inherit from `__pyc_struct__`

```python
class Vec3(__pyc_struct__):
  x: float
  y: float
  z: float
```

`__pyc_struct__` is defined in `__pyc__/00_runtime.py` with
`is_value_type = 1`. The existing `implements`-lift then does
the work — no new frontend code needed at all. Slightly less
discoverable as a Python user, but parallel to how `object`
already works.

### Option C — Implicit promotion for "POD-shaped" classes

Detect classes whose body is purely typed field declarations
(no methods that mutate self, no inheritance beyond `object`)
and auto-set `is_value_type`. Most concise for the user, most
work for the compiler, and historically controversial in
languages that have tried it (C# `struct` vs `class` is opt-in
for exactly this reason).

**Recommendation: Option A.** Explicit, mirrors decorator-style
opt-ins that already exist (`@vector`), and the implementation
is ~10 lines in `build_if1_pyda` plus a one-line decorator
in `00_runtime.py`.

## Verification plan

After the chosen option lands:

- A test fixture `tests/struct_basic.py` that declares
  `@pyc_struct class Point` and asserts inline-vs-pointer
  layout (e.g. `sys.getsizeof` or a runtime ABI check that
  takes the struct by value).
- LLVM codegen-llvm fixture pair: with and without
  `is_value_type` set, confirming the IR shape switches between
  `%struct alloca` and `ptr alloca`.
- The pyc-suite `make USE_LLVM=1 ./test_pyc` pass count stays
  ≥ baseline (the override is a no-op for tests that don't use
  the decorator).
- C-backend behavior unchanged (the C backend's
  `cg_string`-based dispatch also defaults to pointer for
  Type_RECORD; making it honor `is_value_type` would be a
  parallel follow-on).

## What fixing this unblocks

- A user-facing path to "stack-allocated struct" in pyc.
- The closure-capture optimization for POD captures.
- Tighter pyc-↔-C interop via `__pyc_c_call__` for struct args.
- A symmetry between V (which has `is_structure` records) and
  pyc (which would have `@pyc_struct` records).

## Related

- `ifa/codegen/llvm.cc:getLLVMVarType` — the codegen seam this
  issue would let surface code reach.
- `ifa/IR.md` §3.4 — documents `is_value_type`'s intent
  (already calls it "Pass-by-value").
- `ifa/if1/ast.cc:521-538` — the existing IFA-side propagation
  through `implements`. Frontend opt-in plugs into the same
  pipeline.
- `python_ifa_build_if1.cc:build_if1_pyda` PY_decorated case —
  the `@vector("s")` parallel for any decorator-based opt-in.
- V's `is_structure` (sym.h:88, IR.md:128) — the closest
  existing precedent for "record laid out inline".
- `ifa/issues/014-llvm-construction-flow-to-slots.md` —
  unrelated to this issue but referenced by the same
  getLLVMVarType migration commit.

## C-backend parity question (out of scope here)

The C backend's value-vs-pointer choice is currently encoded in
the `cg_string` naming convention (`_CG_int32` for value,
`_CG_psN` for pointer) inside `assign_type_cg_strings_pass1`.
If pyc grows POD records, the C backend would need to honor
`is_value_type` similarly — `Type_RECORD && is_value_type`
should set `cg_string` to the value-typed struct name
(e.g. `_CG_sN`), not the pointer typedef. That's a parallel
change in `codegen_common.cc` and not required for the LLVM
backend to function. File a sub-issue if and when Option A
lands.
