# pyc Codegen Performance Baseline

A reproducible wall-time + memory baseline for `pyc` end-to-end
compilation, recorded as part of [CODEGEN_PLAN §9.3](CODEGEN_PLAN.md)
(phase 6.3 — performance pass). The aim is **regression detection**,
not absolute speed claims: future PRs through the codegen subsystem
should be able to re-run these commands and notice a > 10% wall-time
or maxRSS regression on any fixture.

## 1. Methodology

### 1.1 What's measured

For each representative fixture, three numbers:

- **wall (s)** — wall-clock time of `pyc <fixture>`, end-to-end:
  parse → IF1 → flow analysis → clone → optimize → C codegen →
  spawn `cc` → final binary.
- **maxRSS (kB)** — peak resident set size of the `pyc` process
  itself (does *not* include the spawned `cc`).
- **emitted .c (B)** — size of the generated C file (a proxy for
  "how much work the codegen has to do").

End-to-end is reported because (a) it's what users see, and (b) the
pyc compile path is a single process invocation with `posix_spawnp`
to `cc`, so the `cc` time is part of the user-visible cost.

### 1.2 What's *not* measured (yet)

- **Per-pass breakdown.** `pyc -v` prints FA pass timings (see §3.3
  below), but the rest of the pipeline (parse, clone, DCE, inline,
  codegen) isn't instrumented. Adding `Timer` instances around the
  major passes is a bounded follow-up.
- **`perf record` flame graphs.** `perf_event_paranoid` is set to
  `4` on the recording host; profiling without `CAP_PERFMON` /
  `CAP_SYS_ADMIN` requires root. A future PR should either (a) lower
  paranoid to `1` on a dedicated CI runner, (b) use a `-pg` /
  `-fprofile-generate` build, or (c) add manual `clock_gettime`
  instrumentation.
- **LLVM backend.** The default build doesn't define `USE_LLVM`
  (`Makefile:14`), so `PYC_LLVM=1` is a no-op and the LLVM-codegen
  path can't be measured from a default checkout. To bench LLVM, set
  `USE_LLVM=1` in the top-level Makefile or environment, rebuild,
  then re-run the commands in §1.3 below with `PYC_LLVM=1` prepended.
  See [issue 013](../issues/013-pyc-llvm-default-off.md).

### 1.3 Reproducing the numbers

```bash
# From the pyc repo root, after `make`.
mkdir -p /tmp/pyc_perf
cp tests/sieve.py tests/dict_basic.py tests/logical_operators.py \
   tests/class_inheritance.py tests/builtins.py /tmp/pyc_perf/

cd /tmp/pyc_perf
PYC=$OLDPWD/pyc
for f in sieve.py dict_basic.py logical_operators.py \
         class_inheritance.py builtins.py; do
  # Warm up to amortize disk cache.
  $PYC -D"$OLDPWD" $f > /dev/null 2>&1

  # 5 timing runs; report median.
  for i in 1 2 3 4 5; do
    /usr/bin/time -f "%e %M" $PYC -D"$OLDPWD" $f > /dev/null 2>>/tmp/runs
  done
  echo "$f: $(sort -n /tmp/runs | head -3 | tail -1)"
  : > /tmp/runs
done
```

## 2. Baseline numbers (2026-06-11)

Recorded on commit `c6b1093` (post phase 6.1 CI gates).

### 2.1 Host

- CPU: AMD Ryzen 9 3950X (16-core)
- OS: Linux 6.8.0-111-generic, Ubuntu 24.04 derivative
- Compiler: clang++ 22.0 (Ubuntu LLVM nightly snapshot)
- pyc built with default flags (`make` — no `OPTIMIZE=1`, no `USE_LLVM`)

### 2.2 End-to-end (C backend)

5-run median; variance across runs ≤ 5% on every fixture.

| Fixture | LOC | wall (s) | maxRSS (kB) | emitted .c (B) | binary (B) |
|---|---:|---:|---:|---:|---:|
| `sieve.py`              | 22 | 0.24 | 52 736 |  8 064 | 37 864 |
| `dict_basic.py`         |  5 | 0.25 | 52 480 |  7 519 | 37 400 |
| `class_inheritance.py`  | 41 | 0.24 | 51 712 |  3 704 | 33 624 |
| `logical_operators.py`  | 59 | 0.28 | 54 528 | 11 350 | 43 968 |
| `builtins.py`           | 37 | 0.32 | 56 320 | 33 059 | 68 648 |

`builtins.py` is the heaviest of the five despite being neither the
longest nor the shortest source — it stresses
`__pyc__/05_builtins.py` (abs, all, any, isinstance, range, len, chr,
ord, hex, …) and thus pulls in the most builtin code through the IF1
emitter. Its emitted `.c` is ~3× larger than any other fixture's.

### 2.3 Subprocess split

The wall time is dominated by the spawned `cc` invocation; the IF1
+ analysis + codegen portion of pyc itself is comparatively cheap.

Measured by re-running `Makefile.cg` against the cached `.c` from
the previous step (i.e. without re-running pyc):

| Fixture | wall (s) total | cc-only (s) | pyc-only (s, implied) |
|---|---:|---:|---:|
| `sieve.py`             | 0.24 | 0.15 | 0.09 |
| `dict_basic.py`        | 0.25 | 0.16 | 0.09 |
| `class_inheritance.py` | 0.24 | 0.15 | 0.09 |
| `logical_operators.py` | 0.28 | 0.16 | 0.12 |
| `builtins.py`          | 0.32 | 0.18 | 0.14 |

So pyc's own work — including all of parse + IF1 + FA + clone + DCE
+ codegen — is ~90-140 ms on these fixtures; `cc` is ~150-180 ms.
Even fundamental codegen speedups would only move the **pyc-only**
slice; total user-visible wall time is bottlenecked by `cc`.

### 2.4 Flow-analysis sub-breakdown (`pyc -v`)

Reading off the last "COMPLETE" line of `pyc -v` (FA accumulator
across all passes):

| Fixture | FA total (s) | FA passes | dominant FA phase |
|---|---:|---:|---|
| `sieve.py`             | 0.0053 | 2 | flow (61%) |
| `dict_basic.py`        | 0.0064 | 3 | flow (58%) |
| `class_inheritance.py` | 0.0050 | 3 | flow (64%) |
| `logical_operators.py` | 0.0243 | 4 | flow (60%) |
| `builtins.py`          | 0.0348 | 5 | flow (61%) |

Flow analysis is ~60-67% of FA on every fixture; the rest is
roughly 30% match (cached hit-rate 52-94%) and 2-6% extend. FA is
**2-13% of total pyc time** — codegen and the spawned `cc` together
dominate.

## 3. Hot-spot hypothesis check

CODEGEN_PLAN §9.3 suspected two hot spots:

- **Type-string allocation loop** (`build_type_strings` and the
  shared helpers in `codegen_common.cc:assign_*_cg_strings*`)
- **Per-function PNode worklist** (the `write_c_pnode` /
  `translatePNode` recursive walk plus the LLVM `verifyFunction`)

We can't confirm either without a working `perf record` here, but
the cc-vs-pyc split in §2.3 says that even **if both hot spots are
100% of the pyc-only work today**, optimizing them all the way to
zero buys 90-140 ms per compile — meaningful at scale, but not the
"first thing to fix" until programs become large enough that the
pyc-only slice exceeds the cc slice. That happens somewhere past
`builtins.py`'s 33 KB of emitted C; no fixture in `tests/` is near
that threshold yet.

The deferred follow-ups in §1.2 are the way to confirm; without
them, "the hot spots are still those two" remains a hypothesis.

## 4. Regression-detection workflow

To check whether a PR regresses codegen performance:

1. Check out `main`, run §1.3, save the table.
2. Check out the PR branch, run §1.3.
3. Compare cell-by-cell. A > 10% wall-time or maxRSS increase on
   any fixture is a regression worth investigating.

The 10% bound is conservative — these compile times are short
enough that system noise eats ~5%. A 10% delta on the median of
5 runs is real.

## 5. References

- [CODEGEN_PLAN.md §9.3](CODEGEN_PLAN.md) — phase 6.3 plan.
- [CODEGEN_C.md §9](../CODEGEN_C.md) — the `c_codegen_compile`
  driver that spawns the `cc` subprocess we measure in §2.3.
- [CODEGEN_LLVM.md §11](../CODEGEN_LLVM.md) — the LLVM backend's
  `clang -c` + link drive, currently dormant from a default build
  (issue 013).
- [issues/013-pyc-llvm-default-off.md](../issues/013-pyc-llvm-default-off.md)
  — why §2 here doesn't include LLVM numbers.
