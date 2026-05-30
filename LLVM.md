# LLVM.md — historical / now empty

This document **previously** described an `IFA_IR_V1` textual
serialization format implemented in `ir_serialize.cc` /
`ir_deserialize.cc`. Those source files no longer exist in this tree
(verified by `find -name "ir_serialize*"` returning empty and
`grep IFA_IR_V1` returning no hits). The `.ir` file format is not
part of the current build.

## Current state

The LLVM pipeline goes directly from in-memory IF1 → LLVM IR
(textual `.ll`) → object file via `clang`. No intermediate `.ir`
serialization is involved. See:

- [`CODEGEN_LLVM.md`](CODEGEN_LLVM.md) — the live LLVM backend
  (`codegen/llvm*.cc`).
- [`CODEGEN_C.md`](CODEGEN_C.md) — the default C backend.
- [`IR.md`](IR.md) — the in-memory IF1 IR that both backends consume.

## If you're looking for the old format

The IFA_IR_V1 format spec previously documented here described:
header magic, `SYMS` / `LABELS` / `CODES` / `FUNS` / `ENTRY`
sections, length-prefixed string encoding, ID-based references for
circular structures, and a flag bitmask for Sym attributes
(`is_local`, `is_constant`, `is_external`, `is_fun`). It was a
text-based on-disk form intended to bridge the frontend and the
LLVM backend across processes.

If you need to revive cross-process IR serialization:
- The on-disk format spec is in the git history of this file.
- The consuming code never existed in upstream form — what was
  documented was a planned bridge.
- The current architecture passes IF1 in-process from
  `ast_to_if1` → `ifa_analyze` → `llvm_codegen_write_ir` with no
  serialization between steps. See [PIPELINE.md](../PIPELINE.md).

## Why we keep this file

The filename `ifa/LLVM.md` is referenced by older external docs
and might be searched by readers expecting LLVM-specific content.
This stub redirects them to the right place rather than 404'ing.
The `DOCUMENTATION_PLAN.md` entry for this file is "replace or
delete"; this is the replace.
