# 060 — `case None:` silently disappears from codegen when combined with a literal/True-False/sequence pattern

**Status:** open, symptom confirmed and narrowed, not root-caused.
Found 2026-07-22 while verifying [059](059-narrowing-peel-wrapper-boolean-collapse-gap.md)'s
fix against the full range of combinations
[../../issues/023-structural-pattern-matching.md](../../issues/023-structural-pattern-matching.md)'s
compile-time guard currently blocks.
**Affects:** unknown precisely yet — plausibly `ifa/analysis/fa.cc`'s
constant-folding/dead-arm-elimination machinery, or `ifa/analysis/clone.cc`'s
CreationSet unification across call sites passing `None` and an `int`
literal to the same function. Not the same code 059 touches
(confirmed: reproduces identically with `IFA_NARROW=0`, i.e.
independent of narrowing/`peel_wrapper_def`).
**Related:** [059](059-narrowing-peel-wrapper-boolean-collapse-gap.md)
(found while testing that fix, but a different bug — 059 is about
narrowing never engaging; this is about a whole `if` branch vanishing
from codegen); [../../issues/023-structural-pattern-matching.md](../../issues/023-structural-pattern-matching.md)
(the `case None:`-combination limitation both this and 059 bear on).

## Symptom

```python
def test(val):
    match val:
        case None:
            print("none")
        case 5:
            print("five")
        case v2:
            print("other", v2)

test(None)
test(5)
test(9)
```

Expected (CPython): `none` / `five` / `other 9`.

pyc prints: `other 0` / `five` / `other 9` — the `case None:` branch
is never taken for `test(None)`; it falls through to the capture
fallback, and `v2` holds `0` (the raw representation `None` shares
with the same-slot integer encoding) rather than being recognized as
`None` at all.

**Not a crash** — this compiles clean and runs clean, silently
producing the wrong answer. Confirmed independent of
[059](059-narrowing-peel-wrapper-boolean-collapse-gap.md)'s fix and
of narrowing generally: identical wrong output with `IFA_NARROW=0`.
`case None:` combined with `True`/`False` patterns and with sequence
patterns reproduces the identical shape (the `None` arm silently
never matches, subject falls through to whatever's next):

```python
def test(val):
    match val:
        case None: print("none")
        case True: print("true")
        case False: print("false")
test(None)   # prints "false", not "none"

def test2(val):
    match val:
        case None: print("none")
        case [a, b]: print("seq", a, b)
        case v2: print("other", v2)
test2(None)  # prints "seq 1 2" against test2([1, 2])'s OWN clone --
             # i.e. same wrong-branch symptom, different manifestation
```

**Not affected** (confirmed working, matching CPython): `case None:`
combined with a bare capture (`case x:`), a class pattern
(`case Point(x=x, y=y):`, including one with a genuinely
discriminating sub-pattern like `Point(x=0, y=0)`), or a mapping
pattern (`case {"a": x}:`) — see 059's own verification notes. The
common thread in the *working* cases: `Point`/`dict` instances carry
a classtag (`__pyc_tag`, issue 026/030's mechanism); the *broken*
cases (literal `int`/`str`, `True`/`False`/`bool`, `list`/`tuple`
elements) are exactly the types issue 030 explicitly excludes from
classtag tagging ("raw-layout types"). Plausible, **not confirmed**:
whatever mechanism decides "is the `None`-check's own `if` reachable
at all for this clone" may be conflating `None`'s identity with the
literal integer representation specifically for these
classtag-less, raw-scalar-representation types, in a way it doesn't
for tagged class/dict instances.

## What's been ruled out

- Not narrowing/`peel_wrapper_def` (059's fix) — reproduces
  identically with `IFA_NARROW=0`.
- Not specific to `match`/`case` lowering choices (three different
  `case None:` lowerings were tried per 023's original investigation,
  all failed identically for the *crash* case; this bug wasn't
  re-tried against all three, but the fact that it's independent of
  narrowing suggests it's not about which isinstance-check shape
  `build_pattern_match` emits either).

## Not yet done

Full root-cause trace (matching the rigor 059's fix used — an actual
PNode/CreationSet dump for this exact repro) was not attempted; this
was found and characterized while verifying 059, not chased further,
to keep that fix's own scope bounded. Suspect starting points: dump
`test`'s EntrySet/CreationSet state around the point `test(None)`'s
argument gets unified with `test(5)`/`test(9)`'s int arguments (does
`None`'s CreationSet get merged into / conflated with an int
CreationSet during clone-sharing, losing its distinct identity before
the `isinstance(val, NoneType)` check ever runs?); alternatively,
check whether the `case None:` arm's own `if1_if` gets constant-folded
to always-false by `mark_exc_checks_constant`-style dead-arm
elimination (issue 011's own dead-code work) based on an incorrectly
computed static type for `val` at that specific clone.

## Verification plan

1. The exact repro above (`case None: / case 5: / case v2:`) prints
   `none` / `five` / `other 9`, matching CPython.
2. The `True`/`False` and sequence-pattern variants above also match
   CPython.
3. Once root-caused, re-run [059](059-narrowing-peel-wrapper-boolean-collapse-gap.md)'s
   own combinations (capture, class, mapping) to confirm they're
   unaffected by whatever fix lands here.
4. Full suite + shedskin corpus sweep stay clean on both backends
   (this plausibly touches clone/CreationSet-sharing machinery used
   everywhere, not just `match`/`case`).

## What this unblocks

Together with [059](059-narrowing-peel-wrapper-boolean-collapse-gap.md),
closes [../../issues/023-structural-pattern-matching.md](../../issues/023-structural-pattern-matching.md)'s
one remaining pattern-matching limitation. Until this is fixed,
`issues/023`'s compile-time guard (`pattern_contains_none`/
`pattern_is_risky_with_none` in `build_match_pyda`) must **not** be
relaxed for literal, `True`/`False`, or sequence patterns combined
with `case None:` — doing so would trade a loud compile-time refusal
for a silent wrong answer, strictly worse. It's safe to relax only
for the capture/class/mapping subset 059 already verified sound (not
yet done either, pending a decision on whether to relax the guard
gradually per-pattern-kind or wait for this issue too — see 059's own
notes).
