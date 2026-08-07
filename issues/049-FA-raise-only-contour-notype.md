# 049 — a function whose ONLY reached branch raises gets a bottom-typed return (NOTYPE), pre-existing since issue 011

**Status:** open. Found 2026-07-17/18 while implementing issue 011's
Tier 2 (`Fun::can_raise`, post-FA precise gating via `Fun::calls`).
Confirmed via `git worktree add ... 04d56587` (the very first
issue-011 landing commit, before ANY can_raise gating work existed)
that this bug pre-dates all of today's work — it is intrinsic to the
original exception-handling implementation, not a regression from
[011](../../issues/closed/011-exception-handling-unimplemented.md)'s
Tier 1/Tier 2 gating.

**2026-08-06 update:** the mechanism is now precisely pinned (the
violating AVar is the raising function's OWN return var, made
`live_arg` by its own unconditional `reply` — never anything
caller-side; see "Two prototypes attempted and reverted" near the
bottom of this file). Two concrete fix attempts were built, tested
against more than the headline repro, and reverted — neither is safe
to land as implemented, though attempt 2 got the recognition condition
right and just needs its replacement-value half finished. Practical
stakes are also lower than this doc originally documented: the
headline minimal repro already compiles-with-warnings and RUNS
CORRECTLY on the current baseline (issue 038 alone, no fix from this
issue applied) — what's demonstrated to remain is a spurious
compile-time warning on an otherwise-working program for that shape,
not confirmed silent corruption. Read the 2026-08-06 section before
attempting a third fix.

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

## Proposed fix directions

1. ~~Treat a return-position `Var` with zero reaching definitions for a
   given contour as **provably unreachable**... skip the NOTYPE check
   when `av->live_arg` is true solely because of a caller that itself
   never observes the value along any live path.~~ **This framing was
   wrong** — see "Two prototypes attempted and reverted" below.
   `av->live_arg` is NOT set by any caller's use of the result; it's
   set by the raising FUNCTION's OWN unconditional `reply` (every
   function has exactly one, reading `fn->ret` regardless of which
   edge reached it). No caller-side reachability fact, however
   precise, can affect this violation — confirmed empirically, not
   just by re-reading the code, so treat this as settled, not merely
   suspected.
2. Give the raise path a typed-but-dead contribution to `fn->ret` that
   CANNOT merge into other contours' live type (e.g. a dedicated
   "never" CreationSet used only for this purpose) instead of
   contributing nothing. **Attempted 2026-08-06, reverted** — see
   below. The `_CG_any`-typed placeholder tried does NOT stay
   harmlessly inert the way this direction assumed: once a real return
   value and the placeholder share a contour (any function that's
   *sometimes* called with a raising argument and sometimes not — the
   ordinary case, not an edge case), the placeholder's type unions
   into the real one and corrupts it. A "never" CreationSet that's
   genuinely inert under FA's union operation (not just `_CG_any`, a
   real but generic type) might avoid this, but that's a different,
   unimplemented idea, not what was tried.
3. Extend `PycCompiler::reanalyze` with a case that recognizes "AVar
   has zero reaching defs, and its `Var`'s only def-providing PNode is
   unreachable under the current contour's constraints" and converts
   the violation to void locally, mirroring what `-r`'s
   `convert_NOTYPE_to_void` does globally but scoped early enough to
   let `ifa_analyze` succeed without requiring `-r`. **A close relative
   was attempted 2026-08-06 (as a new `IFACallbacks` hook rather than a
   `reanalyze` case) and reverted** — see below. The direction (exempt
   `fn->ret` specifically, recognized precisely, when the contour
   `can_raise`) is the ONE thing that actually matched the real
   mechanism, but suppressing the violation report alone isn't enough:
   it also has to be UNIONED with `void_type` (mirroring what
   `convert_NOTYPE_to_void` already does for every OTHER bottom AVar)
   rather than just skipped, or codegen inherits a genuinely-bottom
   AVar it was never built to handle. Untried refinement: call the
   equivalent of `av->out = fa->type_world.void_type;` directly inside
   the new hook (or ensure `convert_NOTYPE_to_void`'s own sweep still
   catches this AVar) instead of only suppressing the report.

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

## A second, distinct trigger: TWO independently-raising functions, each individually fine (found 2026-08-06, sudoku2 dig)

While investigating `shedskin_examples/sudoku2/sudoku2.py`
(`issues/037`), found a shape this doc's stated mechanism does NOT
cover: two SEPARATE functions (or a free function and an unrelated
record-class method), each with its own `if cond: raise ...; return
x` body, each called with BOTH a raising and a non-raising argument
(so neither is "reached only via its raising branch" — the specific
condition this doc's root-cause section names) — fail to type when
BOTH exist in the same program, even though each compiles and runs
cleanly completely alone:

```python
def f(x):
    if x < 0:
        raise ValueError("neg")
    return x

class Foo:
    def m(self, x):
        if x < 0:
            raise ValueError("neg")
        return x

print(f(5)); print(f(-1))              # f alone: fine
print(Foo().m(5)); print(Foo().m(-1))  # Foo.m alone: fine
# f AND Foo.m together in one program: "expression has no type",
# then a runtime assert (getter not resolved / matching function not
# found, seen both ways depending on exact shape) if forced through
# with -r.
```

Confirmed pre-existing on unmodified HEAD (`5bec10ac`-era, well before
any 2026-08-06 changes) via direct isolation — not a regression from
that day's `str.index()`/`_CG_str_ne` fixes, which happened to be
what surfaced it in `sudoku2.py` (`fread()`'s `try/except ValueError`
around `str.index()`, once `str.index()` existed and could raise,
became sudoku2's second independent raiser alongside... actually just
itself across two call sites within `fread`'s loop — the minimal
repro above shows the SAME class of failure needs only two *separate*
raising call sites, function identity doesn't matter). Not yet
root-caused to the same precision as the single-function case above
(no FA trace done); given how closely it rhymes — two contours whose
`fn->ret` AVars each depend on the raise-vs-return branch structure,
now interacting across TWO different Funs instead of one — this is
likely the same underlying "raise path contributes nothing to
`fn->ret`" design (see root-cause section above) hitting a
cross-function interaction the single-function analysis didn't
anticipate, but that's a hypothesis, not a confirmed trace.

**Status, updated 2026-08-06:** the `f`/`Foo.m` repro immediately
above no longer reproduces on current HEAD — retested directly while
closing out the correction below, and it now compiles and runs
correctly (propagates `ValueError: neg` to an unhandled-exception exit
for `f(-1)`, matches CPython). It was NOT fixed by design; nothing in
this session touched the `fn->ret`/zero-reaching-defs mechanism the
root-cause section above describes. Most likely an incidental
side-effect of unrelated FA-splitting-trajectory changes elsewhere
this session (`issues/037`'s two fixes) — this codebase's splitter is
already documented elsewhere (issue 033) as sensitive to unrelated
changes in exactly this way. Treat this repro as **not currently
reproducing, not confirmed fixed** — the underlying mechanism
(described in this doc's root-cause section, confirmed via direct
`goto_exc_target` instrumentation on the ORIGINAL single-function
`risky` repro at the top of this file, which still reproduces
unchanged) is presumably still latent and could resurface with a
different arrangement of code. Whoever picks this up next should
re-derive a fresh two-raiser repro rather than trust this exact one.

### Correction, 2026-08-06: sudoku2's actual runtime blocker was NOT this bug

The "third shape" this section used to describe here (a
`try`/`except` around `s.index(digit)` in a loop, compiling clean but
silently returning garbage) — and the claim that it was blocking
`shedskin_examples/sudoku2/sudoku2.py` from running — turned out to be
a **completely different, unrelated bug**, now root-caused and fixed:
[issues/038](../../issues/closed/038-pyc-program-has-raise-builtin-call-gap.md).
Root cause: `pyc_program_has_raise` (the whole-program gate deciding
whether ANY exception-checking code gets emitted at all) is armed by
five specific user-code AST shapes (`raise`/`assert`/the three `yield`
forms) — none of which cover an ordinary method call into a builtin
that raises, the exact shape `str.index()` ([037](../../issues/closed/037-sudoku2-str-ne-void-cast-and-str-index.md))
introduced. A program whose only raise is reachable that way never
armed the gate, so no exception-checking code existed anywhere —
including around the user's own `try`/`except` — and the raise's own,
correct-in-isolation "leave `fn->ret` undefined on this path" behavior
(this doc's own root-cause mechanism, working exactly as designed)
became an uninitialized-memory read with nothing to short-circuit it
first. That's a build-time gate-arming gap, unrelated to the
zero-reaching-defs FA mechanism this doc is actually about — this
doc's own root-cause repro (`risky`, top of file) still reproduces
identically after issue 038's fix, confirming the two are separate.
**With issue 038 fixed, `sudoku2.py` now runs to completion, output
byte-identical to `python3`** — it was never blocked by this issue's
mechanism at all, only by 038's.

## Two prototypes attempted and reverted, 2026-08-06 — read this before attempting a third

User asked to dig into "more/less DCE" as a possible fix direction.
Two concrete prototypes were built, tested, and reverted (working tree
is clean; neither landed). Documenting both in full because the
process of building and breaking them pinned down the actual
mechanism this doc's original root-cause section only partially had
right, and because both failure modes are non-obvious enough that a
future attempt should start from here, not from scratch.

### Prior surprise: the baseline (issue 038 alone) already salvages the minimal repro

Before either prototype: retested `risky(9)`-only (the exact repro at
the top of this file) against current HEAD with neither prototype
applied. It **compiles with the same 4 warnings as always documented
here, exits 0, and RUNS CORRECTLY** — `Unhandled exception: too big`,
matching CPython's `ValueError: too big` exactly. `fruntime_errors`'s
existing `convert_NOTYPE_to_void` salvage (on by default in a plain
`pyc file.py` invocation, no `-r` needed — confirmed empirically, not
matching this doc's own older assumption that `-r` was required and
insufficient) already downgrades the NOTYPE violation to a harmless
`void`-typed AVar, and since the value is genuinely never read (the
caller's pending-exception check, correctly emitted now that issue
038 closed the gate-arming gap, always fires first), the program is
already CORRECT — just noisy at compile time. This substantially
lowers this issue's remaining practical stakes: what's left, for the
shapes tested, is a spurious compile-time warning on an
otherwise-working program, not silent corruption. (Whether that holds
for every shape this doc documents — the two-raiser repro, `sudoku2`
before its own two other fixes — wasn't re-audited; only the minimal
`risky` repro was rechecked here.)

### Attempt 1 (Option 2 — "less DCE", the generator-placeholder idea): reverted

Mirrored issue 014's generator fix exactly: instead of
`goto_exc_target` leaving `fn->ret` undefined on the raise edge
(current behavior, when `fun_returns_value`), give it an
unconditionally-reachable, FA-opaque placeholder move — an opaque
`__pyc_c_call__` to a new `_CG_raise_placeholder_return()`
(`pyc_c_runtime.h`), declared type `any` (`sym_any`, chosen over a
literal like `sym_nil` specifically to avoid resurrecting issue 071's
`T|None` union problem).

**Result on the exact minimal repro (`risky(9)`, no sibling call):
worked.** Compiled clean, ran correctly, `Unhandled exception: too
big`.

**Result once the same function also has a genuine successful return
elsewhere — the common case, not an edge case — two failure modes:**

- `print(risky(3)); print(risky(9))` (two static call sites, one safe
  one not — this doc's own "the fix is one call away" shape):
  **segfaults the compiler itself.** Backtrace lands in
  `optimize/dom.cc`'s `df_traversal`/`build_dominators`
  (dominator-tree construction, called from `find_recursive_loops` /
  `frequency_estimation`, well AFTER FA itself converges) — a null
  vertex during dominance computation. Looks like a latent bug in that
  pass, newly exposed by the CFG shape the extra opaque call site
  creates (a call immediately followed by a jump out of the function),
  not a bug in FA's type lattice itself. Not chased further.
- `try: risky(9) except ValueError: ...` followed by the genuinely
  successful `risky(2)`: doesn't crash, but **`risky(2)`'s real return
  value prints as `<instance>` instead of `2`.** The `any`-typed
  placeholder unions into the SAME contour's `fn->ret` as the real
  `int` return (both calls end up sharing one contour here), widening
  the whole thing to a boxed/generic type — so the genuinely-used
  value gets corrupted, not just the dead one.

That second failure mode is disqualifying on its own: it silently
mishandles exactly the pattern real "validate or return" code uses
most, trading a loud compile-time NOTYPE for a silently wrong answer —
a strictly worse failure mode. Reverted (`pyc_c_runtime.h`,
`python_ifa_build_if1.cc`'s `goto_exc_target`) — confirmed clean via
`git diff`/grep for the added symbol names, rebuilt, reconfirmed the
minimal repro back to its original (documented) warning-emitting
state.

### Attempt 2 (Option 1, corrected — "more DCE", but the RIGHT mechanism this time): reverted

The first thing this attempt found, before writing any code: **the
original "Option 1" framing above (skip the NOTYPE check when the
CALLER never observes the value) targets the wrong variable.**
`PYC_DBG_NOTYPE=1` on the minimal repro shows the violating AVar's
`fun`/`es` as `risky` itself, never `__main__`/the caller. Confirmed
directly: implementing exactly that framing (extending
`provably_constant_isinstance` to return `false_type` for a
provably-always-raising callee, so the CALLER's post-call
`isinstance(__pyc_exc__, NoneType)` true-arm — and therefore its use
of the call result — becomes unreachable) compiles and runs
**identically, warning for warning**, whether the extension is present
or not. Zero effect. The reason: `av->live_arg` for `fn->ret` is set
by `risky`'s OWN unconditional `reply` statement (`gen_fun_pyda`,
every function has exactly one, and it reads `fn->ret` regardless of
which edge — raise or normal return — reached it), not by anything in
any particular caller's control flow. Pruning a caller's branch can't
touch that.

Corrected the target: added a new
`IFACallbacks::notype_violation_is_benign(AVar *av, EntrySet *es)`
hook (`ifa/ifa.h`, mirroring
`provably_constant_isinstance`'s existing contract exactly — default
`false`/conservative, frontend gets first refusal), called from
`collect_var_type_violations` (`ifa/analysis/fa.cc`) right before it
would report a NOTYPE violation. pyc's implementation
(`python_ifa_sym.cc`): true when `av` IS this contour's own return
AVar (matches some `es->rets[i]`) AND `es->can_raise`.

**Result: made things WORSE, deterministically, on the exact minimal
repro that already worked on the unmodified baseline.** `risky(9)`-only
went from "compiles with 4 warnings, runs correctly" (see the prior
section above) to a **hard compile failure** —
`repro.py.c: error: use of undeclared identifier '_CG_String_n'` and
`error: unexpected type name '_CG_ps1032': expected expression`,
reproduced identically across repeated runs (not flaky). Root cause,
understood after the fact: suppressing the violation REPORT doesn't
give the AVar a real type — it stays genuinely `bottom_type`. The
EXISTING salvage step, `convert_NOTYPE_to_void`
(`ifa/analysis/fa.cc`), is what normally converts a bottom AVar to
harmless `void` — and it runs independently, so in principle it should
still have caught this one. It's possible `type_violations`'
presence/absence feeds back into `analyze_to_convergence`'s own
pass-count / `reanalyze` scheduling in a way that changed how many
passes ran or which ones, leaving this AVar in a state
`convert_NOTYPE_to_void`'s own sweep didn't visit the same way — not
confirmed, the interaction wasn't traced further once the regression
was clear. Reverted (`ifa/ifa.h`, `ifa/analysis/fa.cc`,
`python_ifa_int.h`, `python_ifa_sym.cc`) and reconfirmed the minimal
repro back to its working-with-warnings state.

### Takeaways for whoever picks this up next

- The mechanism IS now precisely pinned: `fn->ret`'s `live_arg` for a
  raise-only contour comes from the function's own `reply`, not any
  caller. Direction 1 above only makes sense reframed around that.
- A `reanalyze`-style fix (direction 3) that suppresses the violation
  needs to ALSO union `void_type` into the AVar itself (or otherwise
  ensure `convert_NOTYPE_to_void`'s own sweep still reaches it) — not
  just skip the report — or codegen inherits a value it can't handle.
  This is the most promising remaining direction: it's the one closest
  to actually working (attempt 2 got the *recognition* condition
  right, just not the *replacement* value).
- A placeholder-value fix (direction 2) needs a genuinely inert "never"
  type — one FA's own union operation absorbs without widening a
  sibling contour's real type — not `_CG_any`, which is a real,
  generic type that DOES participate in unions.
- Both prototypes' failure modes only showed up once tested against
  shapes beyond the single minimal repro (a sibling successful call,
  in attempt 1's case; nothing beyond the minimal repro even needed to
  break it, in attempt 2's case). Test broadly (this doc's own
  variants, the full suite, a corpus sweep) before trusting a fix that
  only looks correct on the headline repro.
- Given the practical stakes are now lower than this doc originally
  documented (the minimal repro already runs correctly on today's
  baseline — see above), a third attempt should be weighed against
  that: is landing this worth the FA-internals risk, for what may now
  mostly be a cosmetic warning rather than a correctness bug?
