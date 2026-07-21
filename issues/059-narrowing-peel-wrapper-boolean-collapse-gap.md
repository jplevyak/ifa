# 059 — issue 025's per-branch narrowing never engages for match/case's generated code (root-caused, not fixed)

**Status:** open, root-caused 2026-07-22. Not fixed.
**Affects:** `ifa/analysis/fa.cc` (`peel_wrapper_def`, the `Code_IF`
case in `add_pnode_constraints`) — the issue 025 narrowing-predicate
recognizer. The motivating symptom lives in pyc's frontend
(`python_ifa_build_if1.cc`'s `build_match_pyda`/`build_pattern_match`/
`guarded_bool`), but the actual fix belongs here, in the shared IFA
narrowing infrastructure.
**Related:** [../../issues/023-structural-pattern-matching.md](../../issues/023-structural-pattern-matching.md)
(the `case None:`-combination crash this root-causes); [025](025-intra-function-union-narrowing.md)
(the per-branch narrowing feature this issue found a gap in — read
that file first, especially its "Investigation notes" and
"Refinement" sections, which this issue builds on directly);
[closed/024](closed/024-is-comparison-narrowing.md) (the `is`/`is not`
lowering issue 025's narrowing recognizes); (closed)
[../../issues/closed/026-polymorphic-method-dispatch-partial-override-crash.md](../../issues/closed/026-polymorphic-method-dispatch-partial-override-crash.md)
(ruled out as the same bug — identical assertion text, but a
different mechanism: 026 was a classtag gap for `Type_SUM`-typed
class instances; this is a primitive/boxed-type union with no
classtag involved at all, and BOXING doesn't even gate it — see
"Root cause" below).

## Symptom

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

Crashes at runtime: `Assertion '!"runtime error: matching function
not found"'` (dispatching `x.__str__()` for `print()`, since `x`'s
static type is `None | int` and neither carries a classtag). Currently
guarded against at compile time by `issues/023`'s
`pattern_contains_none`/`pattern_is_risky_with_none` check in
`build_match_pyda`, which refuses to compile any match statement
combining `case None:` with a capture, literal, `True`/`False`, or
sequence/mapping/class pattern.

The exact same union shape, written by hand instead of through
`match`/`case`, works correctly:

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
above with `IFA_NARROW=0` vs. the default `IFA_NARROW=1` produces
**byte-identical generated C** — issue 025's per-branch narrowing
recognizer has *zero effect* on this code shape. Compiling the
hand-written `if`/`else` version the same way: `IFA_NARROW=1` runs
correctly; `IFA_NARROW=0` reproduces the identical crash. This proves
narrowing is the mechanism that makes the hand-written form work, and
that it never engages at all for `match`/`case`'s generated form —
not a difference in some other layer (BOXING, dispatch resolution,
etc.).

**Why it never engages:** `build_pattern_match`'s `guarded_bool`
helper (`python_ifa_build_if1.cc`) is the uniform mechanism every
narrowing/isinstance-based pattern kind uses to turn "does this
sub-check pass" into a plain boolean Sym:

```cpp
static Sym *guarded_bool(Sym *cond, Code **code, PycAST *case_ast,
                          const std::function<Sym *(Code **)> &build_then) {
  Sym *result = new_sym(case_ast);
  Code *then_code = 0;
  Sym *then_val = build_then(&then_code);
  if1_move(if1, &then_code, then_val, result, case_ast);
  Code *else_code = 0;
  if1_move(if1, &else_code, sym_false, result, case_ast);
  if1_if(if1, code, 0, cond, then_code, 0, else_code, 0, 0, case_ast);
  return result;
}
```

For `case None:`, `cond` is the real discriminator
(`isinstance(subject, NoneType)`). But `build_match_pyda`'s *outer*
per-arm dispatch (the if1_if that actually gates each case body,
built by chaining `build_pattern_match`'s return values) doesn't see
`cond` directly — it sees `result`, a value that's **phi-merged from
two different branches of an inner if1_if** (`then_val` on one side,
`sym_false` on the other). The real discriminator (`cond`) is one
level of indirection further in.

Issue 025's `peel_wrapper_def` (`fa.cc`) is the walk-back function
that's supposed to find the real discriminator behind wrapper code.
It handles exactly two shapes:

1. A single-source `Code_MOVE` (`p->rvals.n == 1`) — follow to the
   source Var's def.
2. The specific 3-SEND `__pyc_to_bool__` bind-and-invoke chain that
   sits between every `if cond:` and its discriminator.

It does **not** handle a value phi-merged from two *different*
branches of an enclosing if1_if (case 1's `rvals.n == 1` guard
excludes a phi/merge point outright) — exactly the shape
`guarded_bool` produces. So `peel_wrapper_def(match_result_var)`
stops immediately; it never walks into the inner if1_if to find the
isinstance call underneath, and the per-branch narrowing that would
otherwise correctly restrict `x`'s type to plain `int` in the
"didn't match None" branch never applies.

**Confirmed NOT blocked by BOXING** (issue 025's other documented
structural blocker, item 3 in its "Summary of structural blockers"):
`to_basic_type` (`ifa/analysis/clone.cc`) returns a real type for
`int` (`num_kind` is set) but `nullptr` for `__pyc_None_type__` (falls
through, same as it does for user classes) — and `mixed_basics`
(`fa.cc`) only flags a violation when **two or more *different*
basic types** are present (`basics.n > 1`). A `None | int` union has
exactly one counted basic type (`int`), so it never trips BOXING —
same as issue 025's own `Node | None` finding. This means: unlike the
harder `int | str` (multi-basic-type) case issue 025 explicitly
punted on, **BOXING is already out of the way for every
`None`-plus-one-primitive-type union** — the peel-wrapper gap above
is the *only* remaining blocker for this specific shape.

## Why this is a `peel_wrapper_def` gap, not a `build_match_pyda` gap

This could in principle be worked around from the pyc frontend side
(restructure `build_match_pyda`'s outer per-arm dispatch to use the
real discriminator directly instead of `guarded_bool`'s collapsed
boolean, at least for pattern kinds with nothing else to combine).
But `guarded_bool` is the uniform interface every isinstance-based
pattern kind relies on (sequence, mapping, class, literal-narrowing,
None-singleton) and reworking its calling convention to expose the
raw discriminator alongside the boolean would be a broader
`build_pattern_match` refactor, with its own risk of subtly changing
behavior for patterns that guard-fold or AND/OR-combine multiple
checks (or-patterns, sequence-pattern length+isinstance nesting,
etc.) that need the boolean-collapsed form regardless.

Extending `peel_wrapper_def` instead keeps the fix contained to the
one place already designed to accumulate more "how to see through a
wrapper" cases — issue 025's own history shows exactly this kind of
incremental extension (first the 3-SEND `__pyc_to_bool__` chain, then
`__is__`/`__nis__` recognition) — and is not pyc-frontend-specific:
any future frontend code shape that collapses a discriminator into a
plain boolean via the same phi-merge pattern would benefit
automatically, not just `match`/`case`.

## Proposed fix sketch

Extend `peel_wrapper_def` (or add a sibling helper it calls into) to
recognize a **two-branch same-result phi-merge where one branch's
source is `sym_false` (or another provably-constant value)**:

1. Given the condition Var's def PNode `p`, check whether `p` is
   the phi/merge point for a Var written by **exactly two** enclosing
   if1_if branches (the per-branch SSU views issue 025's own
   "Investigation notes" section already confirmed exist for every
   `Code_IF`, e.g. `ifa/tests/ir/ssu/14_isinstance_narrow.ir`'s
   `v_v1`/`v_v2` shape — this needs the equivalent for a value
   *written* per-branch, not just read).
2. If one branch's write is a MOVE from a constant (`sym_false`,
   matching `guarded_bool`'s else-branch exactly) and the other
   branch's write comes from an arbitrary computed value, treat the
   **enclosing if1_if's own condition** as the thing to continue
   peeling from (recurse `peel_wrapper_def` on it, since a nested
   `guarded_bool` inside another `guarded_bool` — e.g. a sequence
   pattern's outer isinstance + inner length check — needs the same
   treatment at each level).
3. This needs a concrete IF1/SSU fixture first (mirroring issue 025's
   own `ifa/tests/ir/ssu/14_isinstance_narrow.ir`) to nail down
   exactly how a per-branch **write** (not just a per-branch read) to
   the same source-level Var is represented in this codebase's SSU
   form, before writing the peel logic against it — the existing
   `peel_wrapper_def` only ever deals with per-branch *reads*
   (`v_v1`/`v_v2` views of an unchanged operand), not a value that's
   *assigned* differently in each branch and then merged. This is the
   main unknown the "Investigation notes" section below is for.

## Investigation notes (2026-07-22)

Confirmed narrowing is fully inert on the match/case shape via direct
A/B testing (`IFA_NARROW=0` vs `1`, byte-identical generated C either
way) — see "Root cause" above. Confirmed the hand-written equivalent
IS narrowing-dependent the same way (works with `IFA_NARROW=1`,
crashes identically with `IFA_NARROW=0`) — ruling out any other
explanation (dispatch resolution, BOXING, or something match/case
-specific outside of narrowing). Confirmed BOXING doesn't gate this
specific union shape (`mixed_basics`/`to_basic_type` read directly,
not just inferred from the clean compile).

Not yet done: dumping the actual PNode/Var/phi graph for the
`case None:` repro (via whatever this codebase's IR-dump tooling for
SSU looks like — issue 025's own fixture-based verification used
`ifa/tests/ir/ssu/14_isinstance_narrow.ir`-style artifacts) to confirm
exactly how `guarded_bool`'s phi-merged `result` Var is represented at
the PNode level, and whether the "two-branch write, one side
constant" shape is detectable with a bounded, `peel_wrapper_def`-style
walk or needs different machinery entirely. This is the concrete next
step before attempting an implementation, matching how issue 025's own
scope estimate was revised downward only after checking a fixture
directly rather than assuming from source reading alone.

## Proposed verification plan

1. The motivating repro (`case None:` / `case x: print(x)`) runs
   correctly, matching CPython, on both backends.
2. `case None:` combined with a literal, `True`/`False`, and a
   sequence/mapping/class pattern (the other combinations
   `issues/023`'s guard currently blocks) all work too — each is a
   distinct call site through `guarded_bool`, so each needs checking
   independently, not just the capture-pattern case.
3. `issues/023`'s compile-time refusal (`pattern_contains_none`/
   `pattern_is_risky_with_none` in `build_match_pyda`) is relaxed or
   removed once the underlying crash is confirmed gone — turning a
   currently-rejected program into a correctly-compiling one, not
   just silencing the check.
4. The pre-existing narrowing tests/behavior for the hand-written
   `is None`/`isinstance` forms (issue 025's own coverage) stay
   unaffected — this is an *extension* to `peel_wrapper_def`'s
   recognized shapes, not a rewrite of the existing ones.
5. Full suite stays green on both backends; `ifa`'s own unit tests
   and `make test_llvm` stay clean (this touches shared `fa.cc`
   narrowing code that other parts of the analysis rely on).
6. New test: `tests/match_none_combined.py` (or similar) exercising
   the previously-blocked combinations, verified against real
   `python3` output.

## What this unblocks

Closes `issues/023`'s one remaining pattern-matching limitation
(`case None:` can currently only combine with a wildcard or other
`case None:` arms) — the overwhelmingly common `case None: ... case
x: ...`/`case SomeClass(): ... case None: ...` idioms real Python
code uses freely. More broadly, if the "peel through a phi-merged
boolean collapse" extension proves general (not narrowly tied to
`guarded_bool`'s exact shape), it would let issue 025's narrowing
engage for *any* frontend-generated code that collapses a
discriminator into a boolean before branching on it — plausibly a
recurring shape beyond just `match`/`case`.
