# Issue 002: LLVM backend has no test-harness golden

**Status:** open (deferred from phase 08)
**Affects:** `ifa/codegen/llvm*.cc`, `ifa/testing/`.
**Related:** `ifa/testing/phases/08_codegen.md` §3.2,
`ifa/testing/print_codegen.{cc,h}` (C side done).
**Workaround:** the test harness only covers the C backend today.

## Symptom

The `codegen-c` phase locks `c_codegen_print_c`'s output into
goldens. There is no equivalent `codegen-llvm` phase covering
`llvm_codegen_print_ir`. So:

- Changes to the LLVM backend's output aren't caught as diffs.
- The classic regressions the C side detects (Sym-id drift,
  ordering changes, dead-code accidentally getting emitted) go
  unnoticed on the LLVM path.
- The plan's "verifyModule passes" smoke test (phase 08 test #25)
  doesn't exist.

## Root cause

`llvm_codegen_print_ir` emits real LLVM textual IR. That output is
deterministic for a given input but contains a few host-specific
fragments that vary across hosts and LLVM versions:

1. **Target triple** (`target triple = "x86_64-linux-gnu"`).
2. **Data layout** (`target datalayout = "..."`).
3. **DI compile-unit metadata** (`!DICompileUnit(...)`) including
   producer string, filename, host paths, build date in some
   versions.
4. **Auto-numbered metadata IDs** (`!0`, `!1`, …) whose numbering
   depends on emission order — stable run-to-run but sensitive to
   any reordering inside the LLVM module.
5. **SSA register numbering** within functions (`%1`, `%2`, …) —
   deterministic but again sensitive to internal reorderings.
6. **Per-version syntax tweaks** between LLVM 15 / 17 / 22 (opaque
   pointers, debug-record format, etc.). Pyc's prebuilt LLVM is
   pinned but the test harness should ideally tolerate the local
   build's version.

Without a normalizer, any golden file is host-locked.

## Proposed fix: a small line-by-line normalizer

The plan calls for a "normaliser" in `print_codegen_llvm_normalized`
— here's a concrete sketch.

### Sketch

```c
// ifa/testing/print_codegen.cc — alongside print_codegen_c_normalized
void print_codegen_llvm_normalized(FILE *fp, IF1 *p) {
  // Run the full ifa pipeline (same as codegen-c).
  ...
  char *buf = NULL;
  size_t buf_n = 0;
  FILE *mem = open_memstream(&buf, &buf_n);
  llvm_codegen_print_ir(mem, fa, if1->top->fun, /*input_filename*/"test.ir");
  fclose(mem);

  // Normalize line-by-line.
  Map<cchar *, int> md_remap;    // !N → !Nrm (per-fixture stable)
  Map<cchar *, int> reg_remap;   // %N → %Nrm (per-function)
  int next_md = 0, next_reg = 0;
  bool in_define = false;

  for_each_line(buf, buf_n, [&](cchar *line, size_t len) {
    // Drop host-specific lines.
    if (line_starts_with(line, "target triple")) return;
    if (line_starts_with(line, "target datalayout")) return;
    if (line_starts_with(line, "!llvm.dbg.")) return;
    if (line_starts_with(line, "!llvm.module.flags")) return;
    if (line_contains(line, "DICompileUnit") ||
        line_contains(line, "DIFile") ||
        line_contains(line, "DISubprogram")) return;

    // Normalize !N metadata refs in surviving lines.
    cchar *out = remap_md_refs(line, len, md_remap, &next_md);

    // Track function boundaries to scope the SSA register namespace.
    if (line_starts_with(out, "define ")) {
      in_define = true;
      reg_remap.clear();
      next_reg = 0;
    } else if (line_starts_with(out, "}")) {
      in_define = false;
    }
    if (in_define) out = remap_ssa_regs(out, reg_remap, &next_reg);

    fputs(out, fp);
    fputc('\n', fp);
  });

  free(buf);
}
```

`remap_md_refs` / `remap_ssa_regs` walk the line looking for `!N` /
`%N` tokens (where N is a sequence of digits not preceded by an
identifier character) and replace each with its remapped form
(`!0rm`, `!1rm`, …). Use a `cchar *`→int hash keyed by the integer
suffix.

### Why this approach

- **Stays line-oriented**: avoids dragging in an LLVM-IR parser.
- **Host invariants live in the strip-list**: when LLVM version
  changes the triple format or adds new debug metadata, the strip
  list grows; the goldens don't.
- **Per-function SSA reset**: matches LLVM's own per-function `%N`
  numbering convention, so the normalized output is human-readable
  and the diff is over semantic changes.
- **`md_remap` is global-per-fixture**: cross-function metadata
  refs (e.g. type pointers) still match up.

## Test cases (from plan §5, LLVM half)

| # | Test | Notes |
|---|---|---|
| 20 | `simple_add_llvm` | `add nsw` for int+int. |
| 21 | `if_else_llvm` | `br i1` + phi at join. |
| 22 | `record_gep` | `getelementptr` + `load`. |
| 23 | `function_decl_only` | non-live Fun → declaration only. |
| 24 | `external_linkage_audit` | every Fun has ExternalLinkage. |
| 25 | `verifyModule_passes` | `llvm::verifyModule` returns success. |
| 26 | `recursive_struct_type` | opaque-cached, no infinite recursion. |

(25) is a smoke test — the printer calls `verifyModule` and emits
`"verify-ok"` or `"verify-fail: <reason>"`. The golden locks
"verify-ok"; any LLVM-malformed output fails this gate.

## Verification plan

1. Implement `print_codegen_llvm_normalized` per the sketch.
2. Register a `codegen-llvm` phase.
3. Write 7 fixtures matching the plan list, rebless on the dev
   host.
4. Smoke-test on a second host (different LLVM version) — if any
   golden diverges, extend the strip list and re-document.
5. Wire `make test-ir` to run `codegen-llvm` after `codegen-c`.

## What this unblocks

- LLVM-side regressions caught by goldens.
- A `verifyModule` smoke test that detects any LLVM-malformed
  output the codegen might start producing.
- Symmetry with the C backend's golden coverage.

## Why deferred

- Non-trivial normalizer to write correctly across LLVM versions.
- Lower marginal value: production pyc primarily uses the C
  backend; the LLVM path is a stretch goal already.
- The DEBUG_PRINT prereq (REFACTORING.md §6) already landed
  (`49dda85`), so this isn't blocked — just unscheduled.
