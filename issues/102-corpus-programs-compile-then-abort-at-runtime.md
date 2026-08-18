# 102 — 27 of 68 corpus programs compile cleanly and then abort at runtime

**Status:** open, found 2026-08-16 while root-causing what looked like
output nondeterminism in the `PYC_CSMOLD` verification
([101](101-FA-first-time-forever-splitting.md)). **Nothing in the test
harness or in any sweep used on this project sees these failures.**

## Symptom

Surveying the whole shedskin corpus, recording the compiler's exit status
**and the binary's** separately:

| outcome | 2026-08-16 | **2026-08-18** |
|---|---|---|
| compile fails | 8 | **12** |
| **compile OK, binary crashes** | **27** | **24** |
| compile OK, 60 s timeout (not classified) | 23 | 23 |
| compile OK, runs clean | 18 | 18 |

**Of the 65 programs that compile, 24 — 37 % — crash when run.**

> **Re-measured 2026-08-18 after
> [issues/107](../../issues/107-undefined-names-warn-then-segfault.md).**
> Three programs moved from *crash* to *compile-fail* — `rdb`, `sunfish`
> and `voronoi2`, which reference CPython builtins pyc does not implement
> (`EOFError`, `divmod`, `property`). **Nothing newly crashes**, and the
> clean and timeout sets are unchanged. That is this issue's fix
> direction #2 working in miniature: a runtime crash became a
> compile-time diagnostic. (`tarsalzp`, a pre-existing multi-module
> failure, was missing from the first survey, which is why compile-fail
> reads +4 rather than +3.)

```
adatron amaze bh block doom genetic2 kmeanspp life lz2 mastermind2
mwmatching neural1 othello othello2 path_tracing pisang pygasus
rsync rubik sat solitaire sudoku3 sudoku4 sudoku5
```
(`rdb`, `sunfish` and `voronoi2` left this list on 2026-08-18 — they now
fail to compile, see above.)

Verified as genuine miscompiles rather than environment problems — these
run to completion under CPython in the same directory and crash under
pyc:

| program | CPython | pyc |
|---|---|---|
| `life` | rc=0 | **SIGABRT** |
| `othello` | rc=0 | **SIGABRT** |
| `amaze` | rc=0 | **SIGSEGV** |
| `mwmatching` | rc=0 | **SIGABRT** |

## Why a "compile-time" issue shows up at runtime

`cg.cc:2055` is the mechanism. When codegen reaches a call site it cannot
emit, it does **not** fail the build:

```cpp
fputs("  assert(!\"runtime error: matching function not found\");\n", fp);
```

It writes an abort stub into the generated C. So the program compiles
cleanly and dies if and when that path executes. The *same message* also
appears as a hard compile-time `fail()` on other paths (018's own
reproducers stop at `sizeof_element of non-container type` inside
`__pyc__.py`), which is why 018 is written up as a compilation issue.

**The shared string is what makes this misleading, and it misled the
first version of this issue.** `PYC_DBG_DISPATCH` shows the corpus aborts
are mostly *not* 018's problem.

## Three distinct causes, measured

`PYC_DBG_DISPATCH` over 14 crashers, 120 dispatch failures total:

| class | count | what it is |
|---|---|---|
| **A** | **114 (95 %)** | `fns=-1` — **no candidates at all**, operand typed `void_type` (bottom). FA gave the value no type. |
| **B** | 6 | `fns=2` with two same-named candidates on an identical receiver type, e.g. kmeanspp `cand=append cand=append r1=_:list` — two *clones* of one container method that codegen cannot discriminate. |

Class A dominates and is a **NOTYPE / bottom-typed operand** problem, not
a union-representation problem. `life` is representative — iterating a
tuple leaves `__next__`'s result untyped, so every downstream use becomes
a stub, and the loop *is* entered at runtime:

```c
t13 = __tuple_iter__::__pyc_more__(t14);
if (t13) {
  t15 = t14;
  __tuple_iter__::__next__(t15);
  assert(!"runtime error: matching function not found");   /* result untyped */
  ...
  assert(!"runtime error: getter not resolved");
```

Class B *is* the 018/030/[101](101-FA-first-time-forever-splitting.md)
shape — container methods split per receiver CreationSet into clones with
the same C-level signature. It is real but rare (kmeanspp 5, adatron 1).

Two further programs crash with **zero** dispatch failures, so they are a
third cause again:

- `pisang` — `Assertion !"runtime error: list element type mismatch"`
- `block` — SIGSEGV with no diagnostic
- `solitaire` — runaway allocation (3.5 GB) then SIGSEGV; CPython
  finishes in 35 s

## Correction to the first version of this issue

It attributed all 27 crashes to the 018/030 family on the strength of the
assertion text. That is wrong: **95 % are bottom-typed operands (class
A)**, which is the NOTYPE family
([049](049-FA-raise-only-contour-bottom-return.md) territory), and only
~5 % are the union/clone-discrimination problem 018 and 030 describe.

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

1. **Class A — bottom-typed operands — is the bulk of the work** and is
   NOT 018. **`life`'s nine class-A failures are now root-caused, and the
   cause is a frontend argument-binding bug, not FA:**
   [issues/103](../issues/103-unknown-kwarg-silently-bound-positionally.md).
   `life` calls `product((0,1), repeat=...)`; `pyc_lib`'s `product` has no
   `repeat` parameter; pyc silently binds the value to `B` instead of
   raising `TypeError`; the body then iterates an **int**, which has no
   `__iter__` candidate, so FA types everything downstream bottom. Worth
   checking how many other class-A programs have the same upstream cause
   before assuming FA is at fault anywhere.
2. **Codegen should not silently emit an abort stub.** Whatever the
   upstream cause, `cg.cc:2055` turning "I cannot emit this call" into a
   runtime assert is what converts a diagnosable compile failure into a
   37 %-of-corpus runtime crash rate. At minimum it should be reportable
   (a count at end of compilation, or an opt-in hard error).

   **Precedent, 2026-08-18:** `issues/107` did exactly this for undefined
   names — a condition the compiler had already detected and merely
   warned about, which then segfaulted. Making it an error moved three
   programs out of the crash column with no other change. The same
   argument applies to the abort stubs.
3. **The measurement gap** is independently worth closing, and is cheap:
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


## Methodology correction: `run_rc=1` is not a crash (2026-08-18)

This issue's survey classified any non-zero run status as a crash. That
is wrong for **exit code 1**, which is an ordinary Python failure exit —
an uncaught exception, `sys.exit(1)`, or a program's own error path.

`rdb` is the worked example. It was counted as a crasher on `run_rc=1`;
measured against CPython it is **fully working**:

```
CPython rc=1   pyc rc=1   output IDENTICAL
```

It exits 1 because there is no iPod directory to read — exactly as
CPython does. (It reached this state via
[issues/107](../../issues/107-undefined-names-warn-then-segfault.md)'s
`EOFError` fix; before that it did not compile.)

Re-classifying the current survey:

| | count |
|---|---|
| **SIGNAL crashes** (SIGSEGV/SIGABRT — unambiguous) | **23** |
| `run_rc=1` (ambiguous, must be compared to CPython) | 1 — `sat`, genuinely broken (`Unhandled exception`, where CPython runs on) |

So the headline should be **"23 programs crash with a signal"**, and any
`rc=1` must be diffed against CPython before being counted. The earlier
27 and 24 figures both included at least one working program.

This is the same class of error as the `rc=$?`-after-a-pipe bug recorded
above: a status code was read as more meaningful than it is.