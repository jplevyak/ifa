# 211 — `pyc -j` aborts in LLVM on any generator

**Status:** open. Found 2026-11-14 while smoke-testing the relocatable
install. A crash of the compiler, not a wrong answer: `pyc -j` only.

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
