# CLAUDE.md

This file provides guidance to AI Agents when working with code in this repository.

## Build

Prerequisites: Boehm GC, PCRE, and [dparser](https://github.com/jplevyak/dparser) (built with `D_USE_GC=1`).

```bash
make              # Build ifa executable, libifa_gc.a, and ifa.cat man page
make clean        # Remove build artifacts
make install      # Install to $(PREFIX)/bin, lib, include, man
```

The build uses `clang++` and `llvm-ar`. LLVM libraries are discovered via `llvm-config`. Parser files `v.g.d_parser.cc` and `python.g.d_parser.cc` are generated from `.g` grammars via `make_dparser`.

## Tests

```bash
make test_llvm          # Build and run the LLVM backend test (test_llvm.v)
./ifa --test            # Run built-in unit tests (UnitTest framework)
./ifa tests/for1.v      # Run a single test file; produces tests/for1.v.c and output
```

Test files live in `tests/`. Each test has `.v` (source), `.v.check` (expected output), `.v.out` (actual output), and `.v.c` (generated C code). `CLEAN_FILES` includes `tests/*.out` and `tests/*.c`.

## Running the Compiler

```bash
./ifa <file.v>             # Compile a V language source file
IFA_LLVM=1 ./ifa <file.v>  # Use the LLVM backend (produces .o via LLVM)
./ifa -t <file.v>          # Generate HTML visualization
./ifa -c <file.v>          # Dump core code representation
./ifa -G <file.v>          # Generate GraphViz/VCG program graphs
./ifa -l <flags>           # Enable debug logging
./ifa -d / -v              # Debug / verbose mode
```

Key environment variables: `IFA_SYSTEM_DIRECTORY`, `IFA_LLVM`, `IFA_HTML`, `IFA_GRAPH`, `IFA_LOG_FLAGS`, `CHPL_LOG_DIR`.

## Architecture

IFA is a static-analysis-based compiler for a V-like functional language. The compilation pipeline:

```
Source (.v)
  → Parsing (parse.cc, scope.cc, make_ast.cc, v.g grammar via dparser)
  → AST (ast.h/ast.cc, make_ast.h)
  → IF1 IR (ast_to_if1.cc) — lower-level IR with Syms, Codes, Labels
  → Flow Analysis / Type Inference (fa.cc/fa.h) — multi-pass, infers ATypes
  → CFG Construction (cfg.cc, pdb.cc, pnode.cc)
  → Optimizations: dead code (dead.cc), dominance (dom.cc), inlining (inline.cc), loops (loop.cc)
  → LLVM Codegen (llvm.cc, llvm_codegen.cc, llvm_primitives.cc)
  → .o / .ll / .v.c output
```

### Key Data Structures

- **IF1** (`if1.h`): The intermediate form. Owns all `Sym*`, `Label*`, closures, and string interning (`StringChainHash`).
- **Sym** (`sym.h`): Symbol (variable, type, function, constant). Has `Type_kind` enum, flags (`is_local`, `is_fun`, `is_constant`, etc.), nested symbol lists.
- **Code** (`code.h`): CFG/AST node. Kinds: `SUB, MOVE, SEND, IF, LABEL, GOTO, SEQ, CONC, NOP`. Has `rvals` (inputs), `lvals` (outputs), `labels`, `sub` (children).
- **Fun** (`fun.h`): Function/closure. Owns entry/exit `PNode`s, argument/return `Var`s, nested functions, and the calls map.
- **FA** (`fa.h`): Flow analysis engine. Manages `AVar` (analysis variables), `AType` (sets of `CreationSet`s), `EntrySet` (call specializations), and `AEdge` (call edges). Drives type inference and function cloning.
- **PNode** (`pnode.h`): Control-flow node in SSA-like (SSU) form with CFG predecessor/successor links, phi/phy functions, live variable sets.

### IR Serialization

The `.ir` format (documented in `LLVM.md`) bridges the frontend and LLVM backend. Sections: `IFA_IR_V1` header → `SYMS` → `LABELS` → `CODES` → `FUNS` → `ENTRY`. Implementation split between `ir_serialize.cc` and `ir_deserialize.cc`. Serialization marks live nodes only; deserialization uses two-pass allocation to handle circular references.

### Parser Generation

Grammar files `v.g` (V language) and `python.g` are compiled by `make_dparser` into `*.d_parser.cc` files that are then compiled into the binary. The generated files are not checked in—run `make` to regenerate them.

### Memory Management

Boehm GC is used throughout (`USE_GC=1`). `MEM_INIT()` is called at startup. All allocations go through GC-aware allocators from plib. The `libifa_gc.a` variant links against `-lgc`.
