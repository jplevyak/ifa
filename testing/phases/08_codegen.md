# Phase 08 — Code Generation (C and LLVM)

Tests for `c_codegen_print_c` (default backend) and
`llvm_codegen_print_ir` (LLVM backend).

Reference: [CODEGEN_C.md](../../CODEGEN_C.md),
[CODEGEN_LLVM.md](../../CODEGEN_LLVM.md). Implementation:
`ifa/codegen/cg.cc`, `ifa/codegen/llvm*.cc`.

---

## 1. What runs

For each backend:
- **C** — `c_codegen_print_c(fp, fa, main)`. Writes the full
  `.c` file: prologue, type declarations, global Vars, function
  bodies, `int main()` shim.
- **LLVM** — `llvm_codegen_print_ir(fp, fa, main,
  input_filename)`. Writes `.ll` (LLVM textual IR).

Both are pure functions of the post-inline state — no side effects
on `FA`. Easy to invoke in tests.

## 2. Input state expected

Post-inline FA + clone: `fa->funs`, `Var::type`, `Var::cg_string`,
`Fun::cg_string` all set. `Sym::cg_string` set by `build_type_strings`
(called inside `c_codegen_print_c`). Run from cwd that contains
`pyc_c_runtime.h` (or use `-I` to find it).

## 3. Output format

Two phase names: `codegen-c` and `codegen-llvm`.

### 3.1 `<test>.codegen-c.expected`

The full generated `.c` file, but with some normalisation:

- Strip the `#include "pyc_c_runtime.h"` prologue (it's
  invariant).
- Strip `int main()` boilerplate (also invariant).
- Reorder global Var declarations alphabetically by name.
- Reorder function definitions alphabetically by name (currently
  emission order is `fa->funs` order).
- Normalise temp Var names: `t0, t1, ...` per-function based on
  declaration order in `write_c` (already deterministic, but
  document).

```
;; phase: codegen-c
;; Generated C, normalized

(types
  // type Tn for class %my_record
  struct T1 { _CG_int32 e0; _CG_int32 e1; };
  typedef struct T1 *T1;
)

(globals
  _CG_int32 g0 = 0;
  _CG_String g1 = _CG_String("hello");
)

(fun %add
  _CG_int32 %add(_CG_int32 t0, _CG_int32 t1) {
    _CG_int32 t2;
    t2 = _CG_prim_add(t0, t1);
    return t2;
  }
)

(fun %main
  void %main(void) {
    /* ... */
  }
)
```

### 3.2 `<test>.codegen-llvm.expected`

The full `.ll` file, but with:

- Strip module metadata (target triple, DI compile-unit, debug-
  info versions) — they vary by host.
- Strip auto-generated debug-info metadata.
- Normalise SSA register numbering per-function.
- Replace LLVM `!N` metadata refs with `!Nrm` (n=normalised).

```
;; phase: codegen-llvm
;; Normalized LLVM IR

define i32 @add(i32 %a, i32 %b) {
entry:
  %sum = add nsw i32 %a, %b
  ret i32 %sum
}

define void @main() {
entry:
  call void @__main__()
  ret void
}
```

If the LLVM backend produces materially different output between
versions, the normaliser handles it (per-version snapshots
otherwise).

## 4. Printer

```c
// ifa/testing/printers/print_codegen.{cc,h}
void print_codegen_c_normalized(FILE *fp, FA *fa, Fun *main);
void print_codegen_llvm_normalized(FILE *fp, FA *fa, Fun *main, cchar *fixture_name);
```

Each calls the existing emitter into a buffer, then runs the
normaliser.

## 5. Test cases

### C backend

| # | Test | Exercises |
|---|---|---|
| 01 | `simple_add_c` | Binary `+` → `_CG_prim_add` call. |
| 02 | `const_fold_c` | Constant args folded → emitted as literals. |
| 03 | `if_else_c` | IF emits `if (cond) { ... } else { ... }`. |
| 04 | `loop_c` | Loop emits goto-based control flow. |
| 05 | `tuple_create_c` | `make` of tuple → `_CG_prim_tuple` call + field stores. |
| 06 | `list_create_c` | `make` of list → `_CG_prim_list` call. |
| 07 | `record_field_c` | `obj.attr` → `((T)obj)->eN` getter. |
| 08 | `setter_c` | `obj.attr = v` → `((T)obj)->eN = v` setter. |
| 09 | `direct_call_c` | Monomorphic call → direct C call by name. |
| 10 | `polymorphic_assert` | Multi-target (post-clone) → `assert(!"...")` runtime fallback. |
| 11 | `registered_primitive_c` | `__pyc_c_call__` → calls registered `cgfn`. |
| 12 | `phi_phy_materialization` | phi → MOVE at successor; phy → MOVE at predecessor. |
| 13 | `dead_pnode_skipped` | DCE'd PNode → no emission. |
| 14 | `branch_eliminate` | IF with constant cond → only live branch emitted. |
| 15 | `extra_goto` | Single-succ-to-LABEL path → explicit `goto LN`. (Regression for project memory bug.) |

### LLVM backend

| # | Test | Exercises |
|---|---|---|
| 20 | `simple_add_llvm` | Binary `+` → `add nsw`. |
| 21 | `if_else_llvm` | IF → `br i1, label %T, label %F` + phi at join. |
| 22 | `record_gep` | `obj.attr` → `getelementptr` + `load`. |
| 23 | `function_decl_only` | External / non-live Fun → declaration only, no body. |
| 24 | `external_linkage_audit` | Every Fun has ExternalLinkage. |
| 25 | `verifyModule_passes` | `llvm::verifyModule` returns success. |
| 26 | `recursive_struct_type` | Recursive struct → opaque-cached, no infinite recursion. |

## 6. Refactoring dependencies

- [REFACTORING.md](../REFACTORING.md) §6 (DEBUG_PRINT audit) —
  **needed for LLVM tests** because llvm.cc currently has
  unconditional `fprintf(stderr, "DEBUG: ...")` lines.
- §7 (printers) — the normaliser is non-trivial; allow time.
- §3 (deterministic IDs) — type ids in C output are stable only
  with deterministic Sym ids.

## 7. Acceptance

- [x] C printer landed (`testing/print_codegen.{cc,h}` → `codegen-c`
      phase). Runs the full ifa pipeline (analyze → clone →
      build_cfg_dominators → mark_live_code → frequency_estimation
      → mark_live_funs → simple_inlining → mark_live_types →
      mark_live_funs), then captures `c_codegen_print_c`'s output
      into a memstream, strips the `#include "c_runtime.h"`
      prologue, and emits the rest verbatim.
- [~] C tests: 2 of 15 (`01_baseline` is the empty-body case,
      `02_call` exercises a typed-int32 call). Caveat: the synthetic
      `sym___main__` body wires `reply` to `sym_nil`, so most user
      computation gets marked dead by mark_live_funs and elided
      from the C output. The goldens lock the honest "monomorphized,
      DCE'd, emit just the live skeleton" outcome.
- [ ] LLVM printer — not landed. Pre-requisite was the DEBUG_PRINT
      audit (REFACTORING.md §6); ifa-side that landed in `49dda85`
      so the LLVM backend is no longer noisy by default. The
      remaining work is wiring `llvm_codegen_print_ir` into a
      printer + a normaliser for host-specific text variations.
- [x] No DEBUG noise on stderr during test runs (across all 48
      phase fixtures).
- [ ] Normaliser handles host-specific LLVM differences — N/A
      until LLVM printer.
- [ ] "extra goto" regression — would need a fixture where a CFG
      with a non-trivial succ-to-LABEL path produces an emission
      that requires the explicit `goto LN`. Doable but needs the
      user body to survive DCE.
- [ ] verifyModule — N/A until LLVM printer.
