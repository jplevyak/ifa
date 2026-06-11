# Issue 012: `make test_llvm` link fails on `GC_malloc`

**Status:** open (pre-existing).
**Affects:** `ifa/Makefile:230` (the `test_llvm` target),
`ifa/codegen/llvm.cc:1342` (`llvm_codegen_compile`'s clang link
step). Surfaced when wiring up GitHub Actions for the
[CODEGEN_PLAN phase 6.1 CI gates](../codegen/CODEGEN_PLAN.md).

## Symptom

Running the V-language LLVM backend test:

```
$ cd ifa && make test_llvm
IFA_LLVM=1 ./ifa test_llvm.v
/bin/ld: test_llvm.o: in function `_CG_f_147_9':
./test_llvm.v:40:(.text+0xa): undefined reference to `GC_malloc'
/bin/ld: ./test_llvm.v:40:(.text+0x33): undefined reference to `GC_malloc'
...
clang: error: linker command failed with exit code 1
fail: llvm_codegen_compile: linking failed for test_llvm.o (exit=1)
make: *** [Makefile:232: test_llvm] Error 1
```

`test_llvm.v` uses runtime allocations (Boehm GC) but neither
the in-process `llvm_codegen_compile` link step
(`clang <obj> -o <exe> -lm`) nor the redundant explicit
`clang test_llvm.o -o test_llvm` in the Makefile passes
`-lgc -lgccpp`.

`IFA_LLVM=1 ./test_pyc` (the pyc-side LLVM backend test
suite) works fine because the pyc compile path goes through
`Makefile.cg`, which does link `-lgc`.

## Root cause

Two places omit the Boehm GC link flags:

1. `ifa/codegen/llvm.cc:1376`:
   ```c++
   char *argv[] = {"clang", obj_file, "-o", exe_file, "-lm", nullptr};
   int res = codegen_spawn("clang", argv);
   ```
   The link step needs `-lgc -lgccpp` when the IR uses
   `GC_malloc`. The C backend's `Makefile.cg` adds these via
   its `CG_LIBS` variable; the LLVM backend's
   `llvm_codegen_compile` doesn't have an equivalent.

2. `ifa/Makefile:233`:
   ```make
   clang test_llvm.o -o test_llvm
   ```
   This redundant link step (the previous line already produced
   the executable via `llvm_codegen_compile`) doesn't pass any
   GC flags either. If kept, it should mirror the in-process
   link command.

Note that CODEGEN_LLVM.md §14.5 ("Runtime helpers aren't linked
by default") already documents this gap for the pyc runtime
(`pyc_c_runtime`). This issue extends that to the GC runtime,
which is needed by *both* backends' output.

## Proposed fix

Option A — Add GC flags to `llvm_codegen_compile`:

```c++
char *argv[] = {"clang", obj_file, "-o", exe_file,
                "-lm", "-lgc", "-lgccpp", nullptr};
```

The flags are GC-runtime-specific, not pyc-runtime-specific, so
this belongs in the generic LLVM compile path. The cost is
adding two hard-coded `-l` flags; the benefit is making the V
LLVM backend match the C LLVM backend, which already gets `-lgc`
through `Makefile.cg`.

Option B — Mirror the C backend's `Makefile.cg`-style indirection
by introducing a `Makefile.llvm` (or extending `Makefile.cg`) so
the link step is configurable. Larger surface; better long-term
hygiene.

Once either option lands, also drop the redundant
`clang test_llvm.o -o test_llvm` from `ifa/Makefile:233` — the
in-process link already produced the executable.

## Verification plan

After the fix:
- `cd ifa && make test_llvm` exits 0.
- `IFA_LLVM=1 ./test_pyc` still 74/1/0/2 (no regression).
- CI workflow re-enables the `make test_llvm` step.

## What fixing this unblocks

- Re-enabling `make test_llvm` in the GitHub Actions workflow
  (see `.github/workflows/ci.yml`).
- Closing the long-standing LLVM-vs-C parity gap noted in
  CODEGEN_LLVM.md §14.5 for the GC runtime.

## Related

- `ifa/CODEGEN_LLVM.md` §14.5 — pre-existing note about
  runtime helpers not being linked.
- `ifa/codegen/CODEGEN_PLAN.md` §9.1 — phase 6.1 CI gates
  (where the workflow currently skips `make test_llvm`).
