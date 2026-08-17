# 102 — 27 of 68 corpus programs compile cleanly and then abort at runtime

**Status:** open, found 2026-08-16 while root-causing what looked like
output nondeterminism in the `PYC_CSMOLD` verification
([101](101-FA-first-time-forever-splitting.md)). **Nothing in the test
harness or in any sweep used on this project sees these failures.**

## Symptom

Surveying the whole shedskin corpus, recording the compiler's exit status
**and the binary's** separately:

| outcome | count |
|---|---|
| compile fails | 8 |
| **compile OK, binary crashes** | **27** |
| compile OK, 60 s timeout (not classified) | 23 |
| compile OK, runs clean | 18 |

**Of the 68 programs that compile, 27 — 39 % — crash when run.**

```
adatron amaze bh block doom genetic2 kmeanspp life lz2 mastermind2
mwmatching neural1 othello othello2 path_tracing pisang pygasus rdb
rsync rubik sat solitaire sudoku3 sudoku4 sudoku5 sunfish voronoi2
```

Verified as genuine miscompiles rather than environment problems — these
run to completion under CPython in the same directory and crash under
pyc:

| program | CPython | pyc |
|---|---|---|
| `life` | rc=0 | **SIGABRT** |
| `othello` | rc=0 | **SIGABRT** |
| `amaze` | rc=0 | **SIGSEGV** |
| `mwmatching` | rc=0 | **SIGABRT** |

## Cause: pyc's own dispatch stubs, firing at runtime

The aborts are assertions pyc itself emits where FA failed to resolve a
call:

```
life:       Assertion `!"runtime error: matching function not found"' failed.
kmeanspp:   Assertion `!"runtime error: matching function not found"' failed.
othello:    Assertion `!"runtime error: getter not resolved"' failed.
mwmatching: Assertion `!"runtime error: getter not resolved"' failed.
rubik:      Assertion `!"runtime error: getter not resolved"' failed.
```

So this is the
[018](../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md) family — an
unresolved polymorphic dispatch — but reaching **runtime** rather than
stopping the build. Codegen emits an abort stub for the unresolved case,
the program compiles, and it dies when that path is first taken.

`solitaire` is a different shape worth separating: it allocates
runaway memory (`GC Warning: Repeated allocation of very large block
(appr. size 3517100 KiB)` — 3.5 GB) and then SIGSEGVs, where CPython
completes in 35 s.

## Why nothing caught this

Every measurement used on this project reads the **compiler's** exit
status:

- the corpus sweeps in [074](074-FA-cross-pass-oscillation-plan.md) and
  101 record `rc` from `pyc`, which is 0 for all 27;
- `violations` / `ess` / `css` / `pass_limit_hit` are FA-internal and say
  nothing about whether the emitted binary works;
- `test_pyc.py` covers its own tests' runtime via `.exec.check`, but the
  corpus has no such expectations and is not run at all.

This is the third measurement trap recorded in this repo's notes — *"the
harness stops at the first failing stage, so a COMPILE-OUT diff hides a
runtime crash behind it; run the binaries directly"* — and it was hit
again here, during a verification written specifically to avoid the
first one.

It also means **`rc=0` on a corpus sweep is not evidence a change is
safe**, which is how it has been used repeatedly, including by me
earlier in this session.

## Fix direction

Two separable pieces:

1. **The dispatch failures themselves** are 018/030. Nothing new is
   needed to characterise them — but their scale is new information: this
   is not a couple of edge-case programs, it is 39 % of everything that
   compiles.
2. **The measurement gap** is independently worth closing, and is cheap:
   a corpus runner that records compile status *and* run status, so a
   change that turns a working binary into a crashing one is visible.
   Without it, no sweep in this repo can distinguish "compiles" from
   "works".

## Verification plan

- A corpus runner exists and reports run status per program.
- The 27 shrink. `life`, `othello`, `amaze` and `mwmatching` are the
  cheapest starting points: small programs, clean CPython runs, and one
  of the two assertion messages each.
- `solitaire`'s runaway allocation is tracked separately from the
  dispatch aborts — it is not the same failure.

## What this unblocks

An honest baseline. At present the project can report "68 of 76 corpus
programs compile" while fewer than 18 are known to actually run, and no
regression in that gap would be detected by any existing check.
