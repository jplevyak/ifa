# 060 — `case None:` silently disappears from codegen when combined with a literal/True-False/sequence pattern

**Status:** partial fix landed 2026-07-21. **Mechanism 1 fixed**:
`build_isinstance_call` now uses the raw `sym_primitive` isinstance
send instead of the shared wrapper. **Mechanism 2 still open** — the
`True`/`False` + `None` combination remains wrong, and
`issues/023`'s compile-time guard has **not** been relaxed for any
combination yet (a separate follow-on decision; see "What this
unblocks").
**Affects:** `python_ifa_build_if1.cc`'s `build_isinstance_call`
(mechanism 1, fixed) and `ifa/codegen/cg.cc`'s `emit_send_is`
(mechanism 2, both its `is_nil_check` branch and its
classtag-disjunction branch — still open).
**Related:** [059](059-narrowing-peel-wrapper-boolean-collapse-gap.md)
(found while testing that fix, but unrelated — reproduces identically
with `IFA_NARROW=0`); [030](030-polymorphic-dispatch-fat-pointers.md)
(raw-layout / classtag-less types, directly implicated in mechanism
2); [closed/011](closed/011-setter-codegen-vs-analyzer-mismatch.md)
docs the *first* occurrence of mechanism 1, for except-clauses, and
its existing fix; `../../issues/023-structural-pattern-matching.md`
(the `case None:`-combination limitation both this and 059 bear on).

## Symptom

```python
def test(val):
    match val:
        case None: print("none")
        case 5: print("five")
        case v2: print("other", v2)
test(None)  # prints "other 0", not "none"

def test2(val):
    match val:
        case None: print("none")
        case True: print("true")
        case False: print("false")
test2(False)  # prints "none", not "false"
```

Both compile clean and run clean — no crash, just a silently wrong
answer.

## Root cause 1 (FIXED): `build_isinstance_call` shared one polymorphic
`isinstance()` clone across every pattern kind in the match — the
exact bug class [closed/011](closed/011-setter-codegen-vs-analyzer-mismatch.md)
already found and fixed for `except` clauses, never ported to
`match`/`case`

`build_isinstance_call` (`python_ifa_build_if1.cc` ~1087) built
`isinstance(obj, cls)` as an ordinary call through the Python-level
`isinstance()` wrapper (`__pyc__/05_builtins.py`). Once a match
statement checks **two different classes** this way (e.g. `None`'s
`NoneType` check and `5`'s `int` check), FA generalized the wrapper
into one shared polymorphic clone, exactly as documented for
except-clauses in issue 011 — losing the ability to answer each call
site's question independently.

**Fixed**: switched to the **raw `sym_primitive` isinstance send**
issue 011 already uses for `except X:` clauses
(`if1_send(if1, code, 4, 1, sym_primitive, make_symbol("isinstance"),
obj, cls, result)`), never routing through the shared wrapper. Verified
(temporary `getenv`-gated experiment during investigation, then the
permanent change, both re-tested identically):

- `case None: / case 5: / case v2:` → **fixed**, matches CPython.
- `case None: / case x:` (bare capture) → **fixed**, matches CPython.
- `case None: / case [a, b]: / case v2:` → **fixed**, matches CPython
  (contrary to `build_isinstance_call`'s own old code comment
  claiming the raw form breaks sequence-pattern OR-composition —
  not reproduced, including with an actual tuple subject exercising
  the `is_list or is_tuple` combine_bool path).
- Full suite (219/219, both backends), `ifa --test` (58/0), `make
  test_llvm`, and a shedskin corpus sweep (byte-identical
  before/after diff of the results.tsv) all stay clean — this change
  affects every pattern kind's isinstance check, not just the ones
  combined with `None`, and nothing regressed.

This mechanism alone does **not** explain the `True`/`False` case —
see mechanism 2, still open.

## Root cause 2 (deeper, general codegen gap): `None` and a falsy raw
scalar (`False`, `0`, `0.0`) can share the identical zero/NULL bit
pattern in an unsplit contour, and neither of `cg.cc`'s two isinstance
runtime-dispatch paths can tell them apart

pyc avoids boxing an `Optional[int]`-shaped parameter: when FA doesn't
split a formal's union into separate per-type clones, a raw scalar
type (`int64`, `bool`, `float`) sharing a contour with `None`
represents `None` as literal `0`/`NULL` in that same slot. Confirmed
directly in generated C for `case None: / case True: / case False:`
called with `test(False)`:

```c
t18 = ((void *)(intptr_t)t5 == NULL);   // the `case None:` check
...
_CG_f_8300_2/*test*/((_CG_bool)NULL);   // test(False) — False passed as NULL!
```

`False` and `None` are **literally the same bit pattern** at this call
site, so the `case None:` check (`cg.cc`'s `emit_send_is`,
`is_nil_check` branch, ~line 1037-1069: `x == NULL`) matches `False`
too. This is representation-sound for *pointer*-based types (a real
`list`/`tuple`/`str`/class instance is never at address `NULL`, which
is exactly why the sequence-pattern case above works once mechanism 1
is fixed) but fundamentally ambiguous for *scalar* types where `0` is
also a legitimate value.

A second, independent symptom of the same shared-representation root
cause, reachable from **plain user code with no `match`/`case` at
all**:

```python
def test(v):
    if isinstance(v, int):
        print("int")
    else:
        print("other")
test(5)
test(None)
```
prints `other` / `other` (expected `int` / `other`) — wrong even for
the plain `int` call. Traced to `cg.cc`'s *other* `emit_send_is`
branch (the classtag-disjunction path, ~line 1082-1119): once FA
leaves `int64` and `__pyc_None_type__` merged in one un-split
`EntrySet` (confirmed via instrumentation: only one `es` ever exists
for `test`, and its operand AVar's `CreationSet` set eventually
contains both), the runtime check is emitted as a disjunction over
the checked class's classtag-bearing implementors
(`*(_CG_TypeObject**)opnd == &_CG_type_X`). `int` (like `bool`,
`float`, and any other raw-layout type per
[030](030-polymorphic-dispatch-fat-pointers.md)'s tagging exclusion)
has **no classtag**, so the implementors list is empty and codegen
hard-codes the check to `= 0` — always false, regardless of the
operand's real runtime type. Confirmed this does *not* happen for two
genuinely-cloned types (`int`+`str`, `int`+`list` both compile and run
correctly — FA does clone `test` into two separate `EntrySet`s for
those combinations, each resolving to a stable per-clone constant);
it specifically happens when `None` is one of the two types, because
`None`-shares-a-scalar-slot is a deliberate convention that keeps
them merged rather than splitting.

Net effect for `issues/023`'s blocked combinations, mapped to
mechanism:

| combination | mechanism hit | fixed by the raw-form fix? |
|---|---|---|
| `None` + literal (`int`/`str`) + capture/wildcard | 1 (wrapper-clone-sharing) | **yes**, landed |
| `None` + bare capture | 1 | **yes**, landed |
| `None` + sequence pattern | 1 (sequence targets are pointer-typed, so mechanism 2's `is_nil_check` ambiguity doesn't apply to them) | **yes**, landed |
| `None` + `True`/`False` | 2 (`is_nil_check`'s `x == NULL` is ambiguous with `False`'s zero representation) | **no** — needs a representation-level fix, not just the isinstance-send-shape change |

Note: `issues/023`'s compile-time guard still unconditionally refuses
all four combinations above — mechanism 1's fix makes three of them
safe in principle, but the guard hasn't been narrowed to say so yet
(a separate, not-yet-requested follow-on; see "What this unblocks").

## What's ruled out

- Not narrowing/`peel_wrapper_def` (059's fix) — every repro above
  reproduces identically with `IFA_NARROW=0`.
- Not a compile-time constant-folding bug in the ordinary sense: FA's
  own `P_prim_isinstance` transfer function (`fa.cc` ~2207) computes
  the *correct*, honestly-polymorphic `bool_type` for the mixed-union
  cases (confirmed via instrumentation) — it is not silently folding
  to a wrong constant. The wrong answer is introduced downstream, in
  codegen's runtime-dispatch fallback for a type it (`cg.cc`) has no
  way to discriminate at the machine level, and (mechanism 1,
  separately) in the wrapper-clone-sharing that corrupts *which*
  class is even being asked about.

## Proposed fix directions

1. **Mechanism 1 — DONE.** Ported issue 011's raw `sym_primitive`
   isinstance send into `build_isinstance_call`, exactly as done for
   `except`-clause dispatch.
2. **Mechanism 2 (still open)** — genuinely needs a design decision, not a
   one-line fix:
   - Option A: give `None` a distinguishable representation even
     inside a shared scalar contour (e.g. a side discriminant/tag
     bit alongside the scalar payload for any formal whose union
     includes `None` and a raw scalar type) — correct but touches
     the calling convention for every `Optional[scalar]` parameter,
     wide blast radius.
   - Option B: force FA to **always** clone a formal into separate
     `EntrySet`s when its union mixes `None` with a raw scalar type,
     the same way it already does for two boxed/pointer types
     (`int` vs `str`) — narrower blast radius (only affects
     clone/splitting heuristics, not the runtime representation
     itself), but needs to find where that splitting decision is
     made (likely `ifa/analysis/clone.cc`) and confirm mixing
     `None`+scalar isn't relied upon elsewhere as a deliberate
     boxing-avoidance optimization worth preserving for *non*-isinstance
     code paths.
   - Either way, `cg.cc`'s classtag-disjunction path (the `hw3.py`
     symptom) needs its own fix regardless of mechanism 1/2: a
     classtag-less checked class should never silently hard-code `0`
     — at minimum it should be a compile-time diagnostic (the check
     is unimplementable as constructed) rather than a silent wrong
     answer, since this is reachable from plain user `isinstance()`
     calls with zero match/case involvement.

## Verification plan

1. ~~All four combinations in the table above match CPython.~~ Done
   for 3 of 4 (literal, capture, sequence); `True`/`False` still
   wrong, tracked as mechanism 2.
2. The standalone `isinstance(v, int)` / `test(5)`, `test(None)`
   repro (no `match`/`case`) matching CPython is still open —
   mechanism 2's classtag-disjunction half, unaffected by mechanism
   1's fix (confirmed: this repro doesn't involve `build_isinstance_call`
   at all, it's plain user-level `isinstance()`).
3. Done: 059's own verified combinations (capture, class, mapping)
   re-checked, unaffected by mechanism 1's fix.
4. Done: full suite (219/219 both backends), `ifa --test` (58/0),
   `make test_llvm`, and a shedskin corpus sweep (byte-identical
   results.tsv before/after) all clean.

## What this unblocks

Together with [059](059-narrowing-peel-wrapper-boolean-collapse-gap.md),
mechanism 1's fix makes 3 of `../../issues/023-structural-pattern-matching.md`'s
4 blocked combinations (literal, capture, sequence) safe in
principle — but the guard itself hasn't been relaxed yet (a separate
follow-on, not yet done). The `True`/`False` combination must stay
blocked until mechanism 2 has a real fix — relaxing it earlier would
trade a loud compile-time refusal for a silent wrong answer, strictly
worse. Mechanism 2's classtag-less diagnostic (or full fix) also
matters independent of `match`/`case` entirely: it's a latent
correctness gap in plain `isinstance()` whenever a raw scalar type
shares a clone with `None`.
