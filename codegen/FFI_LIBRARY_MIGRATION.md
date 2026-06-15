# FFI_LIBRARY_MIGRATION.md — generic c-call + library-side prims

After Phase B.10's twelve-commit ratchet, v2 LLVM stands at **47
/28** vs v1 LLVM **38/37**. The 28 remaining EXEC failures all
need runtime infrastructure: list/dict/tuple string formatting,
class method dispatch, float `.0` representation, etc.

Continuing on the current ratchet path would mean either (a)
adding C++-side runtime helpers per failure, or (b) extending
v2's inline-emission code per typed prim. Both compound the
codegen-side complexity.

**Proposal**: pivot to a third path — make `__pyc_c_call__`
the universal FFI primitive and move printing/formatting/
length to library code written in pyc-Python.

This document is the design.

## 1. What we already have

`__pyc_c_call__` is a pyc primitive that's been in use since at
least the `chr`/`ord` builtins. Its convention:

```python
__pyc_c_call__(ret_type, "fn_name", arg_type, arg_value, ...)
```

Example (from `__pyc__/05_builtins.py`):

```python
def chr(x):
    return __pyc_c_call__(str, "_CG_chr", int, x)
```

The mechanism:
- A `c_call_transfer_function` reads `rvals[2]` (the ret type
  symbol) and uses it as the result's abstract type during FA.
- A `c_call_codegen` (C backend only) emits the C call by name:
  `_CG_chr(x);` etc.
- The LLVM backend currently has **no `llvm_cgfn` for
  `__pyc_c_call__`** — that's the missing piece.

The library uses this for ~10 callers today:
- `chr`, `ord`, `exit` (builtins)
- `str.__mul__`, `str.__eq__` (string ops via `_CG_string_*`
  runtime helpers)
- `list.__pyc_getslice__`, `list.__add__`, `list.__mul__`,
  `list.append`, etc. (sequence ops via `_CG_list_*` runtime
  helpers)

So the convention is established. The compiler just needs to
honor it on the LLVM side, and the library can absorb the rest.

## 2. What stays C++ vs what moves to library

**Stays in the compiler (C++)**:
- `__pyc_c_call__` itself (the FFI primitive)
- IR-level ops with no runtime equivalent: `CG_BINOP`, `CG_BR`,
  `CG_FIELD_LOAD/STORE`, etc.
- `GC_malloc` declaration + call (Boehm GC is foundational)
- The structural CG_IR ops covered by Phase 4's synthetic
  test corpus

**Moves to the library (pyc-Python via `__pyc_c_call__`)**:
- `write` → `__pyc_c_call__(int, "printf", str, "%lld", int, n)`
  variants per type
- `writeln` → `__pyc_c_call__(int, "printf", str, "\n")`
- `to_string` → `__pyc_c_call__(int, "snprintf", str, buf,
  int, 64, str, fmt, int, n)` (writes a fresh GC_malloc'd
  buffer)
- `to_str` → just `to_string`'s output wrapped as a str
- `format_string` → library implementation calling snprintf
- Float `.0` formatting → library-side check on whole-number
  floats, append `".0"`
- `list.__str__` → walks the list, calls `__repr__` on each
  element, joins with ", " (the current implementation almost
  works)
- `tuple.__str__` / `dict.__str__` — same pattern

The bulk of remaining EXEC failures get unblocked by moving
these into library code that the analyzer can specialize.

## 3. Backend support (the gap to close)

The compiler needs three things:

### 3.1 Recognize `__pyc_c_call__` in `cg_normalize_v2`

Add a `lower_send_c_call` to the `lower_send` dispatcher
(parallels v1's `c_call_codegen` in `python_ifa_main.cc:56`).

Input SEND rvals (per `PRIMITIVES.md:497`):
```
rvals = [sym_primitive, sym___pyc_c_call__,
         ret_type_sym, "fn_name", arg_type_sym, arg_value, ...]
```

Output: a `CG2_C_CALL` inst with:
- `prim_name` = the called C function (extracted from
  `rvals[3]`)
- `type_arg` = the return type as CGv2Type
- `rvals` = pairs of (arg_type, arg_value) for each arg —
  every other entry from rvals[4..]

### 3.2 New `CG2_C_CALL` op + v2 emit

```cpp
case CG2_C_CALL: {
  cchar *fn_name = inst->prim_name;
  llvm::Type *ret = to_llvm_type(inst->type_arg);
  // Walk inst->rvals in pairs (type, value). The type
  // CGv2Value is a Sym ref that resolves via to_llvm_type;
  // the value CGv2Value is the actual arg.
  std::vector<llvm::Type *> param_tys;
  std::vector<llvm::Value *> args;
  for (int i = 0; i + 1 < inst->rvals.n; i += 2) {
    CGv2Value *type_marker = inst->rvals[i];
    CGv2Value *value = inst->rvals[i + 1];
    llvm::Type *pt = to_llvm_type(type_marker->type);
    llvm::Value *v = resolve_value(ctx, value);
    if (!pt || !v) return;
    param_tys.push_back(pt);
    args.push_back(coerce_to(v, pt));   // existing arg coercion
  }
  // Get-or-declare callee with the computed signature.
  llvm::FunctionType *ft = llvm::FunctionType::get(ret,
      param_tys, /*isVarArg=*/false);
  llvm::Function *callee = TheModule->getFunction(fn_name);
  if (!callee) {
    callee = llvm::Function::Create(ft,
        llvm::Function::ExternalLinkage, fn_name, TheModule.get());
  }
  // verifyModule check: callee's existing signature must
  // match what we're passing. If pyc has two c_call sites
  // with the same name but different types, the second one's
  // declaration will conflict. (v1 has the same constraint
  // implicitly.)
  llvm::Value *r = Builder->CreateCall(callee, args, ...);
  if (inst->lvals.n > 0) put_result(ctx, inst->lvals[0], r);
  break;
}
```

### 3.3 Type-marker resolution

The CGv2Value carrying the "type marker" (e.g. `str` in
`__pyc_c_call__(int, "snprintf", str, buf, ...)`) needs to
resolve to a CGv2Type. Today, those CGv2Values are themselves
locals whose `type` is the class itself (`str` class, `int`
class). When we encounter one as an arg of c_call, we read
ITS type — which is the underlying class. `to_llvm_type` on
that class gives the right LLVM type (ptr-to-str-struct, i64,
etc).

There's an edge case: the type marker is sometimes a literal
sym, not a Var. The `lower_send_c_call` would need to handle
both shapes. v1's `c_call_codegen` reads `n->rvals[3]->sym->
constant` for the C function name; same pattern for the type
marker.

## 4. Migration plan

Each step is its own commit per the meta-plan's per-CG_OP
cadence.

### D.1 — this design doc

Lands the proposal. No code changes. Pass criterion: project
owner agrees with the architecture or flags concrete
disagreements. (Same pattern as the B.10.10 design.)

### D.2 — `CG2_C_CALL` op + minimal emit

Add the op to `cg_ir_v2.h`. Add `lower_send_c_call` to
`cg_normalize_v2.cc` (recognized via the prim index for
`__pyc_c_call__`). Add the emit case to
`cg_ir_v2_emit_llvm.cc`. Synthetic unit test: a textual-form
program that does `__pyc_c_call__(int, "puts", str, "hi")`
and verifies the LLVM emits `declare i32 @puts(ptr)` plus a
single `call i32 @puts(ptr ...)`.

Pyc-suite expected impact: 0 (no real code uses CG2_C_CALL
yet). Unit test coverage: +1 — `run_cg_ir_v2_emit_test_c_call`.

### D.3 — move `write` + `writeln` to library

Replace the registered-prim implementations in
`python_ifa_main.cc:103-112` with library code that uses
`__pyc_c_call__`. Add to `__pyc__/00_runtime.py` or a new
`__pyc__/_io.py`:

```python
def write(x):
    if isinstance(x, int):
        __pyc_c_call__(int, "printf", str, "%lld", int, x)
    elif isinstance(x, float):
        # libc %g format; B.10's float-format gap fixed
        # separately
        __pyc_c_call__(int, "printf", str, "%g", float, x)
    elif isinstance(x, str):
        __pyc_c_call__(int, "printf", str, "%s", str, x)
    # ... other types ...

def writeln():
    __pyc_c_call__(int, "printf", str, "\n")
```

(The exact shape depends on how pyc's `isinstance` works
post-analysis — likely the analyzer specializes each `write(x)`
call site to the matching arm. Worth verifying during the
landing.)

Remove `pyc_llvm_write_cgfn` and `pyc_llvm_writeln_cgfn`
from v2's `cg_ir_v2_emit_llvm.cc`. Remove the dispatch
entries from `dispatch_prim`.

Pyc-suite expected impact: 0 to slightly positive. The write
path was already working; this just shifts where the work
happens. Risk: missing isinstance arm → unhandled write call.

### D.4 — move `to_string` to library

```python
def to_string(x):
    if isinstance(x, int):
        buf = __pyc_c_call__(ptr, "GC_malloc", int, 64)
        __pyc_c_call__(int, "snprintf", ptr, buf, int, 64,
                       str, "%lld", int, x)
        return buf
    # ... etc ...
```

Same pattern. Remove `emit_prim_to_string` from v2 emit.

Pyc-suite expected impact: 0 to slightly positive.

### D.5 — float `.0` suffix in library

Once `to_string` is in library code, the float branch can do:

```python
elif isinstance(x, float):
    buf = __pyc_c_call__(ptr, "GC_malloc", int, 64)
    __pyc_c_call__(int, "snprintf", ptr, buf, int, 64,
                   str, "%g", float, x)
    # Append ".0" if no '.' in the result — Python style
    if __pyc_c_call__(ptr, "strchr", ptr, buf, int, 46) == 0:
        __pyc_c_call__(int, "strcat", ptr, buf, str, ".0")
    return buf
```

Pyc-suite expected impact: `logical_operators.py` should pass
(if the only remaining issue is `0` vs `0.0`).

### D.6 — list `__str__` review + tuple/dict equivalent

The existing `list.__str__` uses `for i in range(len(self))`
and string concat. If the analyzer can fold these to constant
calls and the library's `range`/`str.__add__` are working,
list printing might just work. If not: write list `__str__`
purely via `__pyc_c_call__` to libc (manual sprintf
formatting).

Pyc-suite expected impact: ~5 tests (`list_print`,
`list_concat` after concat, `list_or_concat`, etc.).

### D.7 — verify ratchet, retire dead code

After D.2-D.6:
- v2's `cg_ir_v2_emit_llvm.cc` should shed `emit_prim_write`,
  `emit_prim_writeln`, `emit_prim_to_string`, `get_format_str`,
  `get_printf`, `get_snprintf`, and possibly more.
- `python_ifa_main.cc`'s `write_codegen`,
  `writeln_codegen`, `to_str_codegen` can be removed (or
  retained only for the C backend until C reaches parity).

Pyc-suite expected post-D.7: 50+/25- (rough), with the
remaining failures being genuinely runtime gaps (dict
hashing, etc.) rather than printing/formatting issues.

## 5. Risks

| Risk | Mitigation |
|---|---|
| `__pyc_c_call__` args are weakly typed at the call site; v2 emit may mis-coerce | Add per-arg type coercion (existing pattern from B.10.2's CG_CALL coercion). Validate against verifyModule for each landing. |
| Library code's `isinstance` doesn't fan out cleanly under v2's analyzer | D.3 tests on a simple program first (`print(1); print(1.5); print("hi")`). If analyzer doesn't specialize, write each arm as a separate function and dispatch by overload. |
| Removing `pyc_llvm_write_cgfn` breaks v1 LLVM tests | Don't remove during D.3 — register both inline and library paths; v2 prefers library. Cleanup in D.7. |
| Library code needs `bytearray` / `ptr` types that don't exist in pyc syntax | Define a `ptr` type marker in `__pyc__/00_runtime.py` that resolves to an opaque pointer. Use `bytearray` for `GC_malloc`'d buffers. |
| `strchr`/`strcat` aren't always linked | Both are in libc; should be safe. Worst case: declare them with linkage attributes. |
| The C backend stays valid throughout | All migrations stay symmetric with C — pyc-Python library code compiles to either backend the same way. v1's `c_call_codegen` already handles the C side. |

## 6. Pass criteria

**Per landing**: each commit either improves the pyc-suite or
holds it stable; never regresses.

**For the whole migration**:
- v2 LLVM pyc-suite ≥ 50 passing tests (currently 47)
- All printing/formatting failures resolved (`logical_operators`,
  `list_print`, `string_repr`, `string_format`, etc.)
- v1 LLVM baseline preserved (38/37)
- v1 C baseline preserved (74/0)
- Unit tests 105/0 + new c_call unit test = 106/0
- `cg_ir_v2_emit_llvm.cc` has fewer hard-coded
  printf/snprintf/strlen calls; the runtime escape happens
  through `CG2_C_CALL` only

## 7. What this design omits

- The C backend's perspective on the migration. `c_call_codegen`
  already exists and works; D.3-D.6 just move work into
  pyc-Python files that both backends see.
- Recursive `__repr__` / `__str__` for nested containers
  (lists of dicts of tuples). Solvable via the library once
  D.6 lands.
- Dict hashing infrastructure. Orthogonal to FFI; needs its
  own design (probably "Phase E: dict runtime support").
- Closures (SQ3) and sum types (SQ2) — still project-owner-
  blocked.
- The migration plan's Phase C (flip the default to v2). Stays
  pending until this work plateaus at a count the project
  owner trusts.

## 8. Pass criterion for this document

Project owner reads it and either:
- agrees with the architecture (universal FFI + library-side
  prims) → D.2 begins
- agrees on the direction but wants a different sub-step
  ordering → revise §4
- pushes back on a specific aspect (e.g. "the analyzer can't
  handle isinstance arms inside library write — won't work
  for that case") → revise the affected step or pivot to an
  alternative
- decides FFI-first is the wrong move for now → log and
  return to runtime-helper-per-failure ratcheting

If none of the above — disagreement at the level of "what's
even failing now" — back up to fresh investigation before any
code lands.
