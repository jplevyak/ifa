# 087 — out-of-order keyword arguments fail to dispatch (in-order works)

**Status:** open, found 2026-08-08. Confirmed still real — named
precisely in
[issues/025](../../issues/025-shedskin-examples-coverage.md)'s TODO
list item 16 ("`f(high=9, low=2)` still fail to compile (safely — no
miscompile)") but never turned into its own issue file.

**Affects:** `ifa/if1/pattern.cc`'s matcher (`pattern_match` →
`Matcher::find_all_matches`) — confirmed NOT the pyc frontend
(`python_ifa_build_if1.cc` correctly tags every keyword argument with
its parameter name via `if1_add_send_arg(..., cannonicalize_string(...))`
regardless of the order it appears at the call site; verified by
reading that code path directly).

## Repro

```python
def f(low, high):
    return (low, high)

print(f(low=2, high=9))   # matches f's declared parameter order
print(f(high=9, low=2))   # reversed
```
CPython: `(2, 9)` both times (keyword arguments are matched by name,
order at the call site is irrelevant). pyc: the first (in-declaration-
order) call compiles and runs correctly; the second (reversed) call
compiles with `illegal call argument type` warnings and, if actually
reached at runtime, aborts via a guarded salvage trap
(`assert(!"runtime error: matching function not found")`) — confirmed
**not** a silent miscompile and **not** a whole-program crash: verified
with `stdbuf`-forced unbuffered output that every statement *before*
the bad call still executes and prints correctly; the trap is
localized to the one bad call site, matching the original TODO note's
"safely" characterization exactly.

## What's confirmed vs. not

Confirmed:
- The frontend passes both calls' arguments identically in *shape*
  (each keyword argument tagged with its own name, positional
  arguments untagged) — the only difference between the two calls is
  the *order* the named arguments appear in the SEND's argument list,
  since the frontend builds them in call-site (i.e. source) order, not
  sorted or otherwise normalized.
- The actual matching entry point is `pattern_match`
  (`ifa/if1/pattern.cc:1600`), which calls `Matcher::find_all_matches`
  — a backtracking matcher over `MPosition`s (canonicalized
  position/name paths) that's substantial and general-purpose (shared
  by every call in every ifa-based frontend, not pyc-specific).

Not traced further: the exact line/branch inside
`find_all_matches`/`find_best_matches` that's order-sensitive. Given
the in-order case's SEND has kwargs in the same left-to-right order as
`f`'s formal parameter list, and the reversed case doesn't, the likely
shape of the bug is that some part of the matcher walks `names`
positionally (assuming the Nth named argument corresponds to the Nth
formal-with-a-name it hasn't matched yet) rather than doing a true
name→formal lookup via `named_to_positional` for every argument
independent of the others' order — but this is a hypothesis, not
confirmed by tracing the actual matcher logic, which is intricate
enough (recursive backtracking, `MPosition` canonicalization) to
warrant its own dedicated pass rather than guessing at a fix here.

## Verification plan

- Trace `Matcher::find_all_matches`/`find_best_matches` with the
  minimal repro above (this issue's own two-line function is a clean,
  general, non-corpus-specific case — no need for a shedskin example)
  to find the actual order-dependent step.
- `python3 repro.py` → `(2, 9)` twice is the reference.
- Full `test_pyc.py` both backends — this is core dispatch matching,
  a hot, shared path; treat any change as needing the full sweep, not
  a spot-check.
- Add a regression test once fixed (`tests/kwarg_out_of_order.py` or
  similar) — nothing in the current suite exercises reversed-order
  keyword arguments (`tests/keyword_args.py` and
  `tests/kwarg_past_default.py` both exist for related but distinct
  keyword-argument bugs, neither covers pure reordering).

## What this unblocks

Correct, order-independent keyword-argument dispatch — matches
CPython's actual semantics (keyword arguments are matched by name,
never by position). Currently a silent trap for any call written with
keywords in a different order than the callee's declaration, which is
completely ordinary, unremarkable Python style (there's no convention
requiring keyword arguments to be written in declaration order). Low
urgency per the original TODO note (safe failure, easy workaround —
reorder the call or use positional args) but a real correctness gap
in core dispatch, not corpus-specific.
