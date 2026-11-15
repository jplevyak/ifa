# 211 — `pyc -j` aborts in LLVM on any generator

**Status:** closed 2026-11-15. Found 2026-11-14 while smoke-testing the
relocatable install. A crash of the compiler, not a wrong answer: `pyc -j`
only. The hypothesis below was right; see Resolution.

## Symptom

```python
def f(n):
    for i in range(n):
        yield i
print(sum(f(4)))
```

```
$ pyc -j k.py
LLVM ERROR: Do not know how to promote this operator's operand!
Aborted
```

`pyc` and `pyc -b` compile and run the same program and print 6, as
CPython does. The JIT runs programs without a generator (the `make dist`
smoke test, `dist_smoke.sh`, checks one).

## What is known

- Not new: the build from before the 2026-11-14 changes (relocatable
  install, in-process object emission) aborts the same way. The JIT path
  (`ifa/codegen/jit.cc`) does not go through `llvm_emit_object`.
- The message is SelectionDAG type legalization: an operation on a type
  the target cannot promote. The JIT hands the module to ORC's LLJIT
  without running the module pipeline, so the coroutine intrinsics that
  `-b` lowers in its `-O2` pipeline (CoroEarly/CoroSplit/CoroCleanup) may
  reach code generation unlowered. That is the first thing to check:
  whether the module LLJIT compiles still holds `llvm.coro.*` calls.

## Fix

Probably: run the coroutine lowering passes (or the same `-O2` module
pipeline `llvm_emit_object` runs) on the module before handing it to
LLJIT, sharing the pipeline setup with `llvm_emit_object` rather than
restating it.

## Verification

The program above prints 6 under `pyc -j`; `dist_smoke.sh` gets a
generator in its JIT check.

## Resolution (2026-11-15)

**Cause, confirmed.** `llvm_jit_execute` (`jit.cc`) handed TheModule to
LLJIT exactly as emitted, with no optimization pipeline. The `-b` path's
`-O2` pipeline is what lowers the coroutine intrinsics (CoroEarly,
CoroSplit, CoroCleanup); unlowered, they reached the JIT's code generator,
which aborts on them.

**Fix.** `llvm_emit_object` (`llvm.cc`) is now two parts:
`llvm_optimize_module(link_runtime, &linked)`, the pipeline (pre-link
`-O2`, optionally the runtime bitcode linked in and the module
internalized, the LTO pipeline), and the code generation after it. The
JIT calls `llvm_optimize_module(false, ...)` before it writes the REPL's
bitcode cache and before `addIRModule`: the same pipeline as `-b`, minus
the runtime linking, because JIT'd code finds the runtime in the running
pyc by name and those calls must stay external. A module read back from
the REPL's cache was optimized before it was written, so it is not
optimized twice.

**Found with it: the JIT called `main(0, NULL)`.** `sys.argv` was `[]`, so
every program that derives a run-time value from `len(sys.argv)` (many
suite tests, to keep a value from folding) computed something else under
`-j`: `pow_operator`, `tuple_slice`, `sys_module` and five more printed
wrong answers. `llvm_jit_execute(argc, argv)` now takes them: the script
under `-j`, `['']` in the REPL, as CPython gives.

**Measured** on every suite program with an `.exec.check` (no
`.known_issue`, `.check_fail` or `.flags`), under `pyc -j`, against the
build before the fix: 410 → 433 matching. The 18 the pipeline fixed are
the generator and async programs (`generator_*`, `test_async_*`,
`async_*`, `bytes_join`, `str_builders`, ...); the argv fix gave 5 more.
None that matched before differs now. Of the 21 that still differ, 3 are
`ctypes` programs, which `--jit` refuses by design (issues/188), and 18
differ only by compile-time warnings that `-j` prints in the same stream
as the program's output.

**Fixtures.** `tests/jit_generator.py` (`.flags` `-j`; generators,
`yield from`, a generator expression, `sys.argv`), which the build before
the fix aborts on; `dist_smoke.sh`'s JIT check has a generator, so every
release tarball is checked too. `make test` green, 524 passed on both
backends.
