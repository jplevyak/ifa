# 205 — the interface an AI-centric development loop needs: structured diagnostics, a type query, a budget, re-entrancy

**Status: open, not started.** Filed 2026-11-03 from
[closed/192](closed/192-review-analysis-and-issues-2026-11.md) D1, D3, D4,
B5 and C5, [closed/195](closed/195-what-the-concert-compiler-still-has-that-ifa-does-not.md)
C6, and [notes/005](../notes/005-singleton-fa-and-pdb.md). The warm-start
half is [198](198-decision-snapshot-across-compiles.md).

## Where the engine stands

| need | today | gap |
| --- | --- | --- |
| structured diagnostics | prose on stderr from `show_violations`; `report_analysis_errors` gets `Vec<ATypeViolation *>` with kind, AVar, send | no JSON or SARIF; no stable violation id; location recovery falls back three times |
| explanation | the backtrack walks exist (`collect_violation_imprecisions`, `backtrack_to_own_formal`, `cs_backflow`, `find_blocking_es`, `report_dispatch_origin`) and print under `IFA_DBG_*` | not attached to the violation; an agent cannot ask "why is this a union" through the API |
| type query | `type_info`, `call_info`, `constant_info` (fa.h); `fa_dump_types` under `-v` | keyed on `IFAAST *`, not source position; no "type of every expression" export |
| bounded time | pass cap and stall heuristics | no wall-clock budget; `pass_limit_hit` not surfaced; no partial-result contract |
| re-entrancy | `FA` owns its state; `ifa_reset()` for sequential use | globals `fa`, `pdb`, `if1`, `analysis_pass`, `fruntime_errors`; one analysis per process |

## D1. A diagnostic object with its explanation attached

Extend `ATypeViolation` with the result of the walk the stages already
do: the confluence contour (EntrySet or CreationSet id and sym), its
creation points with locations, the writers that supplied each member of
the union, and a classification in the vocabulary the issues already use:
*temporal* (one object, two types over time: the fix is in the program,
`PYC_CHANGES.md`), *polymorphism* (a common user ancestor: hoist, do not
split), *one container*, *local merge*, *global slot*, *separable by ES
split*, *separable by CS split*. `report_dispatch_origin` produces exactly
this for dispatch; the same walk applied to BOXING and PRIMITIVE_ARGUMENT
gives an agent the one fact it needs: whether the fix is in the compiler
or in the program. Emit as JSON behind `--diagnostics=json`; keep the
prose. Stop condition: every corpus refusal and warning is classified.

## D4. A budget and a partial-result contract

`--time-budget=N`: on expiry finish the current pass, run no split stage,
report `converged=false` with violations marked mid-iteration. The bit
exists (`pass_limit_hit`); the CLI does not expose it. The cheapest
partial answer is a mode that never splits: the Concert ancestor's
`fast-types` (one entry set per method and receiver class, no
`extend_analysis`) was one flag over the same transfer functions
(closed/195 C6).

## Type query

Per source position, the converged type of every expression and the
callee set of every call, as JSON. `fa_dump_types` already walks it; it
needs the position key and the format.

## D3. Server mode

Reentrancy step 5 in notes/005's hybrid shape: pass `FA *` to the
top-level free functions, add back-pointers on `EntrySet` and
`CreationSet` only. One process can then hold an analysis per open file,
and the corpus sweep can run in-process in parallel instead of forking 77
compilers. Stop condition: two `FA::analyze` calls on two threads produce
the same results as two processes.

## Verification

D1: a fixture per classification, each asserting the JSON. D4: a program
cut off by the budget reports `converged=false` and the same violations a
full run reports as a subset. D3: the thread test above.
