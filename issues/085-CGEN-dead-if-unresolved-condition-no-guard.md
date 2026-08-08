# 085 — `Code_IF` whose own condition is unresolved has no salvage guard when exactly one successor is live (both backends)

**Status:** open, filed 2026-08-07. Supersedes/corrects
[issues/025](../../issues/025-shedskin-examples-coverage.md)'s
`rsync.py` entry, which claimed this was "filed as a known gap" — no
such issue was ever actually filed (found via a full coverage audit of
that document, see its own TODO list item 6).

**Affects:** `ifa/codegen/cg.cc`'s `write_c_pnode` (`Code_IF` case,
~line 1972-2003) and `ifa/codegen/cg_emit_llvm.cc`'s
`emit_block_terminator` (`Code_IF` case, ~line 3325-3380).

## Background — why this supersedes the rsync entry rather than just re-filing it

issues/025 diagnosed `rsync.py`'s silent `SIGSEGV`/`SIGILL` (no
diagnostic at all) as: "FA marks `blockchecksums`'s loop body dead (a
downstream 'has no type' violation), and dead-code elimination empties
the loop's `if` branch entirely with no runtime-error guard at all: no
`assert`, no `goto`, the function just falls off its end." That
specific claim was never turned into a filed issue.

**Re-verified 2026-08-07: `rsync.py`'s original symptom is gone.**
Rebuilt fresh, ran it directly — it now compiles (with warnings) and
**aborts cleanly** via an existing, unrelated salvage guard:
```
rsync: rsync.py.c:653: ... Assertion `!"runtime error: getter not resolved"' failed.
```
This is a different, already-guarded failure mode reached earlier in
the same function, not the "falls off the end with zero diagnostic"
behavior originally described. So citing rsync.py as the repro for
this issue would be misleading — it no longer demonstrates the
mechanism, even though the mechanism itself, verified below, is real.

## The mechanism (confirmed by direct code reading, both backends)

**C backend** (`cg.cc`'s `write_c_pnode`): when a `Code_IF` PNode's own
condition is a compile-time-provable constant, its live-and-fa_live
fast path handles it correctly (recognizes `true_type`/`false_type`
and only recurses into the one real successor — this is fine,
intentional, and the common case). But when the PNode is `!(n->live &&
n->fa_live)` for the *other* reason — FA couldn't resolve the
condition at all (an unresolved call, salvaged to NOTYPE) rather than
folding it to a known constant — the code takes this branch instead:
```cpp
} else {
  do_phy_nodes(fp, n, 0);
  do_phi_nodes(fp, n, 0);
}
```
No `if` test is emitted, no guard, nothing. Control then falls through
to the function's general "recurse into every `cfg_succ`" loop a few
lines later (`for (PNode *p : n->cfg_succ) if (done.set_add(p))
write_c_pnode(...)`), which runs **regardless of `n`'s own liveness**.
If only ONE of the two successors ends up independently live (the
common case for a real unresolved-condition salvage — one arm's
downstream code still gets used, the other doesn't), execution falls
straight through into it with **no runtime check that the condition
actually held** — silently running code that assumed a branch was
taken which, at the C level, was never actually evaluated.

**LLVM backend** (`cg_emit_llvm.cc`'s `emit_block_terminator`): same
structural gap, different failure shape. When `!(closer->live &&
closer->fa_live)`, it falls through to the generic conditional-branch
path and tries `value_for_var(ctx, closer->rvals.v[0])` on the
unresolved condition Var — which returns `nullptr` for a genuinely
unresolved value. Then:
- If **both** successors have basic blocks: `Builder->CreateUnreachable()` —
  arguably *worse* than the C backend's silent fallthrough, since LLVM's
  `unreachable` is an optimizer license ("assume this is never
  executed"), not a runtime-checked trap; if it % actually executes,
  behavior is undefined in a way the optimizer can exploit, not just a
  skipped check.
- If only **one** successor has a basic block (the case actually
  observed below): `Builder->CreateBr(t_bb)` or `CreateBr(f_bb)` —
  unconditionally branches into it, exact same "no check ever
  happened" gap as the C backend.

## Confirmed still live today (corpus + full test-suite sweep)

Instrumented `write_c_pnode` with a temporary debug print flagging
every `Code_IF` that is `!(live && fa_live)` with at least one
independently-live successor, then ran the full `test_pyc.py` suite
and a fresh sweep of all 86 `shedskin_examples/`. Three hits:

- `tests/logical_operators.py` / `tests/my_bool4.py`'s `test_cond` —
  almost certainly **benign**: three literal-boolean call sites
  (`test_cond(True, True, True)` etc.), so this is very likely
  ordinary compile-time constant-folding of `a and b and c`, just
  performed one layer earlier (by dead-code elimination) than
  `write_c_pnode`'s own constant-fast-path normally handles. Not
  investigated further; flagged only for completeness.
- `tests/with_exception.py`'s `raises_comma_form_inner_suppresses` —
  not investigated; plausibly also benign (exception-propagation
  branch structure, not an unresolved call).
- **`shedskin_examples/msp_ss/msp_ss.py`, function `main`** — the
  interesting one: correlates directly with a genuine compile-time
  warning at the *exact* source line, `if filename == '-':` (line
  1604), `warning: unresolved call '__eq__'`. This is the real
  "FA gave up, condition unresolved" flavor the rsync diagnosis
  originally described, not a constant fold. **Not confirmed to
  actually misbehave at runtime** — attempted to force execution down
  this path (`./msp_ss -c /dev/null -`) but the tool needs a real
  serial port to get that far (confirmed even CPython's own reference
  run fails immediately for environmental reasons — `termios.error`
  trying to open a comport that doesn't exist in this sandbox), so the
  dangerous branch's actual runtime behavior is unverified. The
  *static* pattern (dead `if`, unresolved — not constant-folded —
  condition, one live successor) is confirmed real via the compiler's
  own diagnostic; whether it manifests as silent misbehavior at
  runtime for this specific program is not proven.

## What a fix needs

The core problem: `write_c_pnode`/`emit_block_terminator` currently
cannot distinguish *why* a `Code_IF` is `!fa_live` — a provably-dead
branch from constant-folding (safe to elide entirely, already handled
correctly elsewhere) vs. an unresolved/salvaged condition (NOT proven
unreachable, needs the same `emit_goto_or_trap`-style guard issue 056
already established for the analogous "jump to a dead label" case).
Both currently take the identical "silently fall through, no check"
path. A fix likely needs either:
- A way to tell the two cases apart at the PNode (e.g. distinguishing
  "constant-folded" from "salvaged-to-NOTYPE" liveness, if `dead.cc`
  doesn't already track this distinctly), so only the genuinely
  unresolved case gets a guard — inserting one unconditionally would
  regress the (larger, benign, already-correct) constant-fold case
  with spurious traps.
- Or, more conservatively: emit the guard in *both* cases (correctness
  over precision) if telling them apart turns out to be expensive —
  benign constant-fold cases would just get a dead, unreachable-in-
  practice trap that never fires, which is safe if less clean.

## Verification plan

- Construct a clean, minimal, understood repro that reliably reaches
  the unresolved-condition/one-live-successor shape at runtime (not
  yet done here — `msp_ss` needs hardware; the two test-suite hits
  found are suspected benign and weren't confirmed as the dangerous
  flavor). A heterogeneous-union comparison shape (mirroring
  tictactoe/056/077's established "partial dunder coverage across a
  union" trigger) is the most likely angle.
- Once a repro exists demonstrating actual wrong behavior (not just
  the static pattern), verify the fix closes it and doesn't regress
  the constant-fold fast path (`tests/logical_operators.py`,
  `tests/my_bool4.py`, `tests/with_exception.py` must stay byte-
  identical).
- Full `test_pyc.py` both backends; re-sweep all 86 shedskin examples
  for the same instrumented pattern to confirm no new occurrences
  and no regressions in examples that currently compile.

## What this unblocks

Closes a real, if currently unconfirmed-as-exploited, soundness gap in
one of the oldest and most heavily-relied-on parts of both backends
(control-flow emission for every `if` in every compiled program). Not
urgent — no currently-known program actually demonstrates wrong output
or a crash traceable to this specific mechanism — but worth tracking
properly now that it's been isolated, rather than leaving it as an
undocumented claim the way it sat in issues/025 since 2026-07-19.
