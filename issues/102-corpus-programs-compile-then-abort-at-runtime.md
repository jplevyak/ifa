# 102 — programs that compile cleanly and then abort: codegen turns an unresolved call into a silent runtime stub

**Status:** open. Rewritten 2026-09-28 against sweep `4b61e721`. It now
also carries [149](closed/149-the-largest-diagnostic-class-reports-nothing.md)'s
work list. The history (the 2026-08 census, 37% of compiling programs
crashing, and the measurement traps) is in git:
`git show 3f36072b:ifa/issues/102-corpus-programs-compile-then-abort-at-runtime.md`.

## The defect

When codegen cannot resolve a call, it emits
`assert(!"runtime error: matching function not found")` (`cg.cc:2737`) or
a `getter not resolved` / `list element type mismatch` stub, and the
compile succeeds. A condition the compiler has already detected becomes a
runtime crash. At `4b61e721` these compile with **zero warnings** and then
abort on such a stub:

| program | stub |
| --- | --- |
| `adatron` | `matching function not found` in a `_CG_nil_type` function |
| `life` | `matching function not found`, `(_CG_ps…, _CG_nil_type)` |
| `quameon` | `matching function not found`, `(_CG_ps…, _CG_list)` (see [132](132-arity-is-representation-not-provenance.md)) |
| `pisang` | `list element type mismatch`, `(_CG_any, _CG_int64)` |
| `loop` | SIGSEGV (`run_rc=139`), not yet classified |

`tests/list_tuple_union_method.py` (`.known_issue`) is the minimal shape:
a `{list, tuple}` union reaching a shared method has two candidates with
the same C receiver type and no runtime tag.

## Principle

**An unresolved dispatch that reaches codegen is a demand that FA never
observed.** It must be a compile-time VIOLATION, not a stub:

- it is fatal-by-default like every other violation (closed/158), so the
  program is refused rather than miscompiled;
- once it is a violation, the demand ladder can answer it. For example,
  CONST_DEMAND can split `int.__ne__` over 1/3 in `itertools.product`,
  which is what retires one of [134](134-remove-the-frontend-forced-split-opt-in.md)'s
  annotations.

Precedent: `issues/107` made undefined names an error instead of a
warning-then-segfault, and three programs left the crash column with no
other change.

## Fix

1. In FA's post-convergence checks, report any send whose candidate set
   codegen will not be able to discriminate: no candidate, or ≥2
   candidates that share a C receiver type and have no tag. Use the SAME
   predicate codegen uses (`poly_dispatch_classtag_targets`,
   `get_target_fun_core`). Call it; do not restate it (AGENTS.md: never
   decide by name, never reimplement codegen's answer).
2. Keep the codegen stub only as an internal-error backstop that cannot be
   reached when FA reported nothing. An `IFA_DBG` count of emitted stubs
   should read 0 on the whole corpus.

## The untyped-value work list (from 149)

Ranking the corpus's unresolved calls by cause (2026-09-12): the
missing-method era is over. 86% of unresolved calls are a **cascade from
an untyped value**, because every named operand type has the operator.
So the work is "why is this value untyped". The actionable roots are the
NAMED `'X' has no type` sites. Re-rank at HEAD. On 2026-09-12 the top was
doom `data = self.entry_data[b'VERTEXES']` (a dict value channel, driving
all 20 of doom's unresolved calls), then msp_ss `dataOut` / `blkin`,
plcfrs `rule` (plcfrs has since been fixed), othello2 `value`.

For each root: find the confluence and backtrack the demand (AGENTS.md).
Never add a missing method to make a cascade go away. A missing method is
only real when the named receiver type lacks it (then it goes to the
top-level `issues/`, as `list.copy`/`dict.copy` do, see
[086](closed/086-list-and-dict-have-no-copy-method.md)).

## Measurement rules (kept, because each was learned the hard way)

- `compile_rc=0` is not evidence a change is safe. Use
  `./corpus_sweep.sh -m check`, which records run status and CPython
  agreement.
- `run_rc=1` is not a crash until diffed against CPython (some programs
  exit 1 by design).
- The harness stops at the first failing stage, so a COMPILE-OUT diff can
  hide a runtime crash behind it. Run the binaries.

## Verification

The five programs above either run correctly or are refused at compile
time with a diagnostic naming the send. `tests/list_tuple_union_method.py`
is refused with a diagnostic (update its `.check`), and no program that
matched CPython regresses.
