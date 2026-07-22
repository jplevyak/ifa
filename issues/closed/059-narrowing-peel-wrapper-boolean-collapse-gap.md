# 059 — issue 025's per-branch narrowing never engaged for match/case's generated code

**Status: CLOSED** — FIXED 2026-07-22 (`dd6e3928`) in
`ifa/analysis/fa.cc` (`peel_wrapper_def` extended, plus a new
`peel_guarded_bool_merge`/`find_gating_if` pair). Verified sound and
regression-free (full suite 219/219 both backends, `ifa`'s own unit
tests 58/0, `make test_llvm` clean, shedskin corpus sweep
byte-identical before/after except one already-documented flaky
example — see "Verification" below). **Now user-visible**: the
downstream blocker ([060](060-none-branch-dropped-mixed-with-literal-bool-sequence.md),
the more severe `None`-plus-scalar bug found while verifying this fix)
was fixed 2026-07-21, and `issues/023`'s compile-time guard has since
been removed — every `case None:` combination this fix helped enable
now compiles and matches CPython (`tests/match_none.py`).
**Affects:** `ifa/analysis/fa.cc` (`peel_wrapper_def`, new
`peel_guarded_bool_merge`/`find_gating_if` helpers, the `Code_IF` case
in `add_pnode_constraints`) — the issue 025 narrowing-predicate
recognizer. The motivating symptom lives in pyc's frontend
(`python_ifa_build_if1.cc`'s `build_match_pyda`/`build_pattern_match`/
`guarded_bool`), but the fix belongs here, in the shared IFA narrowing
infrastructure — no frontend changes were needed.
**Related:** [../../issues/023-structural-pattern-matching.md](../../../issues/closed/023-structural-pattern-matching.md)
(the `case None:`-combination crash this fixes, partially — see
"What's still blocked"); [025](../025-intra-function-union-narrowing.md)
(the per-branch narrowing feature this extends — read that file first,
especially "Investigation notes" and "Refinement", which this builds
on directly); [060](060-none-branch-dropped-mixed-with-literal-bool-sequence.md)
(a separate, more severe bug found while verifying this fix); (closed)
[../../issues/closed/026-polymorphic-method-dispatch-partial-override-crash.md](../../../issues/closed/026-polymorphic-method-dispatch-partial-override-crash.md)
(ruled out as the same bug — identical assertion text, different
mechanism).

## Symptom (original)

```python
def test(val):
    match val:
        case None:
            print("none")
        case x:
            print("other", x)

test(None)
test(5)
```

Crashed at runtime: `Assertion '!"runtime error: matching function not
found"'` (dispatching `x.__str__()` for `print()`, since `x`'s static
type is `None | int` and neither carries a classtag). The exact same
union written by hand instead of through `match`/`case` worked
correctly:

```python
def test(val):
    if val is None:
        print("none")
    else:
        x = val
        print("other", x)
```

## Root cause

Confirmed via a clean A/B test: compiling the `match`/`case` repro
above with `IFA_NARROW=0` vs. the default `IFA_NARROW=1` produced
**byte-identical generated C** — issue 025's per-branch narrowing
recognizer had *zero effect* on this code shape, while the
hand-written form was genuinely narrowing-dependent (`IFA_NARROW=1`
works, `=0` crashes identically). This isolated the bug to narrowing
specifically, not BOXING, dispatch resolution, or anything else.

**Why it never engaged:** `build_pattern_match`'s `guarded_bool`
helper (`python_ifa_build_if1.cc`) — the uniform mechanism every
isinstance-based pattern kind uses to turn "does this sub-check pass"
into a plain boolean Sym — collapses the real discriminator (`cond`,
e.g. `isinstance(subject, NoneType)`) into a phi-merged `result` Var
(`result = then_val` on one branch, `result = sym_false` on the
other) *before* `build_match_pyda`'s outer per-arm dispatch ever sees
it. Issue 025's `peel_wrapper_def` (the walk-back that's supposed to
find the real discriminator behind wrapper code) only handled two
shapes — a single-source `Code_MOVE` chain, and the specific 3-SEND
`__pyc_to_bool__` bind-and-invoke chain — neither of which matches a
value phi-merged from two *different* if1_if branches. So it stopped
at `guarded_bool`'s collapsed boolean and never reached the isinstance
call underneath.

**Confirmed via direct PNode/SSU inspection** (temporary
`getenv`-gated debug instrumentation, added and removed during this
work): the merged `result` Var's `->def` is a `Code_LABEL` PNode (the
CFG join point after the if/else) carrying exactly one `phi` entry
with 2 `rvals` — one from each branch's own single-source `Code_MOVE`.
Walking each branch's MOVE PNode back through its single CFG
predecessor (a label, the branch's own entry point) reaches the SAME
enclosing `Code_IF` PNode in both cases, whose own condition Var is
the real discriminator (confirmed: the isinstance-wrapper-call SEND,
exactly the shape `add_pnode_constraints`'s existing recognition logic
already knows how to read — `fname == "isinstance"`, `rvals[1]` =
operand, `rvals[2]` = type).

**Confirmed NOT blocked by BOXING** (issue 025's other documented
blocker, for multi-basic-type unions like `int | str`): `to_basic_type`
(`ifa/analysis/clone.cc`) returns `nullptr` for `__pyc_None_type__`
the same way it does for user classes, and `mixed_basics` only flags
a violation when 2+ *different* basic types are present — a
`None | int` union has exactly one (`int`), so it never trips BOXING.
The peel-wrapper gap was the only remaining blocker for this shape.

## Fix implemented

Two new helpers in `ifa/analysis/fa.cc`, and `peel_wrapper_def`
extended to call them as a third "advance" case in its existing
bounded loop:

- `find_gating_if(PNode *from, int max_depth)`: walks a
  single-predecessor CFG chain from a branch's write PNode looking
  for the `Code_IF` that gates it (typically 2 hops: the write, then
  the branch's own entry label, then the if).
- `peel_guarded_bool_merge(PNode *p, Var *cur, int max_depth)`: given
  a label PNode `p` with phi children, finds the phi entry for `cur`,
  and — **only if BOTH of its two branch sources are the literal
  constants `sym_true` and `sym_false`** (one each) — confirms both
  branches' writes are gated by the SAME enclosing if-statement and
  returns that if-statement's own condition Var.

**Critical soundness constraint, found during verification, not in
the original design sketch**: the fix requires **both** branches to
be literal constants, not just the else branch being `sym_false`. An
earlier version only checked the else branch (matching `guarded_bool`'s
unconditional `result = sym_false` else-move) and let the then-branch
be anything — this is **unsound**: `result == true` always implies
the discriminator was true (an AND can only be true if every operand,
including the discriminator, was true), but `result == false` does
**not** imply the discriminator was false when the then-branch
computes something other than bare `sym_true` (a guard's result, or a
real AND-fold of sub-pattern matches) — it could equally mean the
discriminator was true but the guard/sub-pattern failed. Verified
concretely: the looser version produced **silently wrong output**
(not a crash) for `case None if flag: ... case x: ...` across multiple
calls with different `flag` values (one call's captured value read a
completely unrelated integer from a *different* call site — a
clone/CreationSet mixup, not just a missed optimization). Tightening
to "both branches literal constants" excludes exactly the unsound
cases: `combine_bool`'s own short-circuit (`a == sym_true` returns `b`
unchanged) means a pattern kind with no discriminating sub-pattern at
all (e.g. a class pattern whose every attribute is itself a bare
capture) still collapses to literal `sym_true` naturally, so the
restriction doesn't need to special-case *which* pattern kind produced
the shape — it falls out of `combine_bool`'s existing algebra.

## Verification

Re-ran the original A/B test after the fix: `IFA_NARROW=1` now runs
the original repro correctly (matching CPython byte-for-byte);
`IFA_NARROW=0` still reproduces the original crash (confirms the fix
is genuinely routed through the narrowing mechanism, not some other
accidental change).

Tested via a temporary `getenv`-gated bypass of `issues/023`'s
compile-time guard (added, used, then fully reverted — `git diff`
confirmed clean on `python_ifa_build_if1.cc` afterward):

- `case None:` + bare capture (the original repro): **fixed**, matches
  CPython.
- `case None:` + class pattern (`Point(x=x, y=y)`, and `Point(x=0,
  y=0)` with a genuinely discriminating sub-pattern): **fixed**,
  matches CPython. (The literal-sub-pattern case works even without
  my fix engaging — `Point` instances carry a classtag, issue 026/030's
  mechanism, so dispatch over `{None, Point, ...}` doesn't need
  narrowing at all; only pure classtag-less primitive unions like
  `None | int` did.)
- `case None:` + mapping pattern (`{"a": x}`): **fixed**, matches
  CPython.
- All three combined in one match statement, plus a wildcard fallback:
  **fixed**, matches CPython.
- `case None if flag:` (a guard on the `None` arm) + capture fallback,
  across multiple calls with different `flag` values: **correctly
  falls back to the pre-existing crash** (loud, not silently wrong) —
  confirms the soundness fix above actually excludes this case rather
  than mishandling it.
- `case None:` + literal (`case 5:`), `True`/`False`, or sequence
  (`case [a, b]:`) patterns: **still produces silently wrong output**
  — confirmed via testing with `IFA_NARROW=0` that this is completely
  independent of narrowing/this fix (identical wrong output either
  way) — a different, more severe, pre-existing bug, filed separately
  as [060](060-none-branch-dropped-mixed-with-literal-bool-sequence.md).

Full suite (`test_pyc.py`, both backends): 219/219 passed, 6 expected
fails, 0 failed — identical to pre-fix. `ifa`'s own `--test`: 58/0.
`make test_llvm`: clean. Full shedskin corpus sweep (`shedskin_sweep.sh`),
compared before (`git stash` the fa.cc change, rebuild, sweep) vs.
after (rebuild with the fix, sweep): byte-identical results across all
77 examples except `pygasus`, which showed a different failure
*signature* (compile timeout vs. a crash) in each run — already
documented elsewhere (`ifa/issues/033`'s M3 section) as a known,
pre-existing, non-deterministic flake, not a regression (it failed in
both runs, only how it failed differed).

## What's still blocked

`issues/023`'s compile-time guard remains **fully active,
unconditionally** — this fix, while verified correct and safe, does
not by itself make any *new* program compile, because the frontend
never reaches the point of exercising it. Relaxing the guard requires:

1. Deciding whether to relax it gradually per confirmed-safe pattern
   kind (capture, class, mapping) while [060](060-none-branch-dropped-mixed-with-literal-bool-sequence.md)
   remains open for literal/`True`/`False`/sequence, or wait for both
   to close together. Relaxing it for the wrong subset would trade a
   loud, safe compile-time refusal for a silent wrong answer — worse,
   not better — so this should not be done casually.
2. [060](060-none-branch-dropped-mixed-with-literal-bool-sequence.md)
   closing, if the decision is to relax everything at once.
3. A real `tests/match_none_combined.py` (or similar), which can only
   be added meaningfully once the guard is actually relaxed for at
   least the pattern kinds it covers — there is currently no way to
   reach this fixed code path through pyc's normal compile flow to
   write a real regression test against it.

## What this unblocks

Once `issues/023`'s guard is relaxed for the confirmed-safe subset:
`case None: ... case x: ...` and `case None: ... case SomeClass(...):
...`/`case None: ... case {...}: ...` idioms — genuinely common
patterns in real Python code — compile and run correctly. More
broadly, `peel_guarded_bool_merge`'s shape (peeling through a
boolean-collapsed if/else merge) is not `match`/`case`-specific; any
other frontend code that collapses a discriminator into a boolean
before branching on it would benefit from the same extension
automatically.
