# 049 — a function whose ONLY reached branch raises gets a bottom-typed return (NOTYPE), pre-existing since issue 011

**Status:** open. Found 2026-07-17/18 while implementing issue 011's
Tier 2 (`Fun::can_raise`, post-FA precise gating via `Fun::calls`).
Confirmed via `git worktree add ... 04d56587` (the very first
issue-011 landing commit, before ANY can_raise gating work existed)
that this bug pre-dates all of today's work — it is intrinsic to the
original exception-handling implementation, not a regression from
[011](../../issues/closed/011-exception-handling-unimplemented.md)'s
Tier 1/Tier 2 gating.

## Symptom

Compiling a program where a function that can raise is called ONLY
with argument(s) that hit the raising branch — i.e. no call anywhere
in the whole program ever reaches that function's normal `return` —
fails to type with spurious `NOTYPE` ("expression has no type")
violations, even though the program is functionally sound (the
result is genuinely never read on the raising path; the caller's
post-call check always short-circuits first). `try`/`except` and
method dispatch are NOT required to trigger it — the minimal repro
is a bare top-level call:

```python
def risky(n):
    if n > 5:
        raise ValueError("too big")
    return n

print(risky(9))
```

```
$ ./pyc ifa/issues/repro049.py
ifa/issues/repro049.py:1: expression has no type
  called from ifa/issues/repro049.py:6
ifa/issues/repro049.py:6: illegal call argument type expression illegal:
ifa/issues/repro049.py:6: expression has no type
ifa/issues/repro049.py:6: expression has no type
ifa/issues/repro049.py:6: expression has no type
fail: program does not type
```

The same shape wrapped in `try`/`except`, or reached through several
frames of propagation, or via a method call elsewhere in the program,
all fail identically — those elements are coincidental, not causal
(see "How this was isolated" below; an earlier round of this
investigation chased "method call + try/except" as the trigger before
finding the real one).

**The fix is one call away**: adding ANY call to the same function
where the argument does NOT hit the raising branch makes the whole
program type again:

```python
def risky(n):
    if n > 5:
        raise ValueError("too big")
    return n

print(risky(3))   # <- added: does not raise
print(risky(9))   # <- same call as before, now fine
```

## Root cause (hypothesis, well-supported but not proven by reading
the fixed-point code directly)

`python_ifa_build_if1.cc`'s `PY_raise_stmt` case (line ~3354-3399)
deliberately routes the raise straight to `goto_exc_target` WITHOUT
going through the function's normal fall-through nil-move into
`fn->ret` — see the comment there: "the exceptional path contributes
NOTHING to the return type: the caller never reads the dead result
because its check fires first." This was intentional, to fix a
different, earlier bug (see the `PY_try_stmt` comment a few lines
down: an earlier version routed the no-finally unmatched-dispatch
fallthrough through `Lresume`'s merge point and it "merged a spurious
extra type arm into whatever the try's OTHER (live) exits define").

The interaction: pyc's FA clones per call-site argument shape
(literal/CS-specific contours). When `risky` is called ONLY with an
argument whose CS makes `n > 5` resolve toward the raising arm for
EVERY contour that exists in the program, `fn->ret`'s AVar for those
contours has **zero reaching definitions at all** (not merely a type
mismatch or missing field — genuinely no `MOVE` into `ret` occurs on
any live path for that contour), and `collect_var_type_violations`
(`ifa/analysis/fa.cc:3396`) flags it as NOTYPE since it's
`live_arg` (the call's result is used somewhere, e.g. passed to
`print`) but `av->out == bottom_type`.

When a SEPARATE call elsewhere reaches the normal-return branch, that
call's contour (or a contour `risky`'s parameter widens/merges into,
across passes) DOES define `fn->ret`, and whatever unification/reuse
happens across contours resolves the previously-empty one too. This
is consistent with every empirical observation below.

`PycCompiler::reanalyze` (`python_ifa_sym.cc:301`) — the mechanism
that's supposed to give FA a second chance on NOTYPE violations — only
handles the field-promotion case (`v->av->var->def->rvals.n < 2` skips
everything else, `python_ifa_sym.cc:311`); it has no path that
re-derives a return value for a contour with zero reaching defs, so
this class of violation never resolves no matter how many passes run.
`-v` shows the violation count pinned at the same nonzero value across
every pass (not decreasing, not increasing) for the failing repro —
consistent with "genuinely stable, structurally unreachable" rather
than "still converging."

`fruntime_errors`/`-r` (`convert_NOTYPE_to_void`, `fa.cc:6016`) was
checked as a possible existing escape hatch — it does NOT currently
suppress this failure (`ifa_analyze` still returns -1 and `pyc.cc`
still calls `fail("program does not type")` with `-r` passed on the
repro above). Worth understanding separately why (`-r`'s "f"-type CLI
arg spec looked possibly different from the other boolean "F"-type
flags in `pyc.cc`'s `arg_desc` — didn't chase this, might just be a
flag-parsing quirk unrelated to the core bug).

## How this was isolated

Started from a false lead: two repros (`Calculator`/`use_calculator`
method call co-existing with a `caller`/`risky` `try`/`except` pair)
both failed, while trimmed-down variants without the method call or
without the `try`/`except` both worked — pointing at "method dispatch
disturbs unrelated try/except convergence." That theory did not
survive isolation:

1. Stripped to just `risky`/`caller` + `try`/`except`, no class, no
   method call, called only with `caller(9)` (raises) → **still
   fails** (2 violations). Method dispatch was never the cause.
2. Stripped `try`/`except` entirely — bare `print(risky(9))` at
   module level → **still fails** (5 violations). `try`/`except` was
   never the cause either.
3. Went back to the working baseline test
   (`tests/exception_propagation.py`, `run(3)` then `run(9)` through
   a 3-frame call chain into a `try`/`except`) — genuinely 0
   violations. The difference from the failing minimal repros: this
   test always calls the can-raise chain with BOTH a safe and an
   unsafe argument.
4. Confirmed directly: adding `print(caller(3))` before
   `print(caller(9))` to the failing `try`/`except` repro drops
   violations to 0. Confirmed again on the bare-call repro: adding
   `print(risky(3))` before `print(risky(9))` also drops it to 0.

So the real trigger is specifically: **no call anywhere in the whole
program reaches the function's normal-return branch** — everything
else (`try`/`except`, intermediate frames, method dispatch) is
incidental to how the earlier repros happened to be shaped.

## What this unblocks / who hits it

Any real program where a "validate-and-raise" style function's
raising branch is the only one exercised by the concrete call sites
present in that compilation unit — a very ordinary shape (e.g. an
internal helper always called with data that's already known-bad at
one call site, with the "good" case only reached via a code path the
whole-program analysis doesn't happen to instantiate with a
non-raising literal/CS). This is a real gap in
[011](../../issues/closed/011-exception-handling-unimplemented.md)'s
exception-handling support, independent of the can_raise gating work
(Tier 1/Tier 2), which only decide whether to EMIT a check — they
don't touch this return-type convergence path at all and can't cause
or fix it.

## Proposed fix directions (not attempted)

1. Treat a return-position `Var` with zero reaching definitions for a
   given contour as **provably unreachable** (like a function that
   never returns — `NoReturn`) rather than `NOTYPE`: skip the NOTYPE
   check when `av->live_arg` is true solely because of a caller that
   itself never observes the value along any live path (would need
   confirming the caller's own use of the result is provably
   downstream of a will-always-propagate check — nontrivial to prove
   in general, but the common case here is exactly "the only use is
   the pending-exception check itself").
2. Alternatively, give the raise path a typed-but-dead contribution to
   `fn->ret` that CANNOT merge into other contours' live type (e.g. a
   dedicated "never" CreationSet used only for this purpose) instead
   of contributing nothing — would need to verify it doesn't
   resurrect the original bug the current no-contribution design was
   built to avoid (the `PY_try_stmt` "spurious extra type arm" issue
   documented in `python_ifa_build_if1.cc`).
3. Extend `PycCompiler::reanalyze` with a case that recognizes "AVar
   has zero reaching defs, and its `Var`'s only def-providing PNode is
   unreachable under the current contour's constraints" and converts
   the violation to void locally, mirroring what `-r`'s
   `convert_NOTYPE_to_void` does globally but scoped early enough to
   let `ifa_analyze` succeed without requiring `-r`.

## Verification plan

- Minimal repro above (`repro049.py`, bare call, no try/except)
  compiles cleanly (`./pyc` exits 0, no NOTYPE diagnostics) and runs
  correctly (raises/propagates as expected).
- `tests/exception_propagation.py` continues to pass (already does —
  it always pairs a safe and unsafe call, so it never exercised this
  gap; add a case there, or a new dedicated test, that calls a
  can-raise function ONLY with a raising argument, to lock in the fix
  and prevent regression).
- Full suites (`./test_pyc.py`, `PYC_FLAGS=-b ./test_pyc.py`,
  `make test-unit`, `make test-ir`) stay green.
