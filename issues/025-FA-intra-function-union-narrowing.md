# 025 — branch-correlation narrowing is unimplemented: correlated arms over a CLASS union warn spuriously

**Status:** open, and **rescoped 2026-08-22** — the title above is the
third one this issue has had, and the first that describes what is
actually left.

What this issue was filed as ("IFA doesn't narrow union types
intra-function") is no longer the defect. Narrowing *works* for the
case that has a coherent runtime representation: `is None` on a
class-or-None union, end-to-end on both backends. The three
originally-filed scalar cases were reassigned to
[issues/018](../../issues/closed/018-dict-mixed-key-types-boxing-failure.md) in
2026-08-11 and re-confirmed there today. What remains is one thing:
**branch correlation is not recognised as a discriminator**, and over a
union of user *classes* that costs spurious diagnostics — not wrong
answers.

It cannot be closed as superseded: 018 does not cover branch
correlation, so there would be no surviving doc for it.

Re-derived from fresh repros 2026-08-22 (at `0f92e6d4`), not carried
over from notes — see "Re-verification 2026-08-22" near the end, which
also retires one item this doc had been asserting without evidence.

**Affects:** `ifa/analysis/fa.cc` (`peel_wrapper_def`, `Code_IF`'s
per-branch narrowing application, `flow_var_type_permit`);
`python_ifa_build_if1.cc`'s `build_builtin_call_pyda` (where the
unrelated isinstance-on-classes fix lives). For Cases 1-3's actual
fix, see [018](../../issues/closed/018-dict-mixed-key-types-boxing-failure.md)
instead.

## Problem

Pyc narrows union types in exactly one situation: when the union
arises from cross-function polymorphism. There, "narrowing" is really
clone-time specialization — pyc produces one `Fun` per concrete
argument type at a call boundary, and each clone sees a single
concrete type, no flow-sensitivity involved. For a union that's
genuinely intra-function (phi-merged from two branches, or read from
a polymorphic field), nothing narrows it. Three shapes (kept as
originally filed — still a useful illustration of what narrowing
*would* look like; see "Current state" for what actually blocks each
one today):

**Case 1 — phi-merged union, re-discriminated by a correlated branch:**
```python
def f(flag):
  if flag: x = 5
  else: x = "hi"
  # x is int|str via phi merge
  if flag: return x + 10        # narrowing needed: x is int here
  else: return x + " world"     # narrowing needed: x is str here
```
The second `if flag:` correlates perfectly with the first (SSU
preserves `flag`'s identity through the merge) — pyc could in
principle deduce `x`'s branch-narrowed type from that correlation. It
doesn't (and, per "Current state" below, that's not actually why this
fails).

**Case 2 — `isinstance` on an intra-function union:**
```python
def maybe(b):
  if b: return 5
  return "hi"
def use(b):
  v = maybe(b)                  # v is int|str at runtime
  if isinstance(v, int): return v + 10
  return v + " world"
```
`isinstance` is exactly the discriminator pyc's splitter already
understands — but only at a function boundary (so cloning can split
on it), not for a union that arose inside one function.

**Case 3 — `==` against a discriminating value, narrowing a per-branch
return type:**
```python
def f(flag):
  if flag == 0: return "zero"
  return flag + 10
```
`flag` itself has a single type (`int`); the "mixed basic types"
violation comes from `f`'s own return type being `int|str`. Per-branch
return-type inference would let the call site work; pyc's return-type
inference doesn't do this.

## Current state (re-verified 2026-08-11 — all three cases re-derived from a fresh repro, not trusted from old notes)

- **Case 1: still broken, both backends — and not actually about
  branch correlation.** A stripped-down repro with **no companion
  branch, no correlation, no function at all** —
  ```python
  if cond:            # cond must be genuinely runtime-varying, e.g. from sys.argv
      x = 5
  else:
      x = "hi"
  print(x + 10)
  ```
  — fails identically to the original two-branch version: C backend
  gets a hard compile error (`_CG_strcat` called with an `int` where
  `const char*` is expected); LLVM backend fails module verification
  (`call parameter type does not match function signature`). Since
  there's no discriminating predicate anywhere in this repro for any
  narrowing mechanism (built or hypothetical) to even engage with,
  Case 1 was never really a narrowing problem — see 018 for the actual
  mechanism (FA clones an inherently invalid specialization of
  `str.__add__` fed an int argument, and there's no salvage guard,
  compounded by primitive types having no classtag or other runtime
  discriminator for dispatch to fall back on).
- **Case 2, three sub-cases with very different status:**
  - *Using `isinstance`'s own True/False result to pick a branch, over
    a union of user classes* (`isinstance(a, Dog)` where `a: Dog |
    Cat`) — **fixed 2026-08-03**, but by an unrelated bug fix, not by
    this issue's narrowing mechanism (see below). Verified still
    passing today: `tests/isinstance_union.py`, both backends.
  - *Actually using the narrowed type for an operation inside the
    branch* (the literal repro above: `v + 10` where `v: int | str`)
    — still broken, and — newly confirmed — for the same 018 reason as
    Case 1, not a narrowing-routing problem: a **bare `print(v)`**,
    with `isinstance` and `+` both removed entirely, crashes
    identically (`assert(!"runtime error: matching function not
    found")`) for this exact `v = maybe(b)` function-return-union
    shape. Whatever consumes `v` first hits the wall; `isinstance`
    was never the load-bearing part of the repro.
  - *`isinstance` against a builtin type (`list`) reached through a
    container's own mixed-basic-type elements* — reclassified
    2026-08-03, already understood as 018's territory before today's
    broader finding.
- **Case 3: also re-derived today as the same 018 mechanism**, not a
  distinct return-type-inference gap: `f`'s `int|str` return value
  crashes identically the moment a caller does anything with it
  (`assert(!"runtime error: matching function not found")`), same as
  Cases 1 and 2. Never had anything to do with per-branch return-type
  inference specifically — any caller-side consumption of a
  scalar-union return value hits this, regardless of how the callee
  produced it.
- **The one real win, unaffected by any of the above: `is None`
  narrowing on a recursive class-or-None union works end-to-end**,
  both backends — `tests/recursive_list_is_none.py` (re-verified
  today: prints `6 / 55 / 0` on both). This is the motivating
  linked-list/tree pattern from issues 004 and 024, and the one case
  where the per-branch AVar-refinement mechanism built for this issue
  actually pays off — because a class-or-None union, unlike a scalar
  union, already has a coherent runtime representation (a pointer,
  optionally classtag-headed) that never trips 018's `BOXING`
  violation in the first place.

## Mechanism as built: per-branch AVar refinement

The infrastructure this issue needed mostly already existed in
`fa.cc`: `AVar::restrict` (an upper-bound type filter — `update_in`
recomputes `v->out = v->in ∩ v->restrict` on every change),
`flow_var_type_permit(v, t)` (the API to add to `restrict`, already
used for constant folding and splitter filtering), and `Code_IF`
already walking `p->phy` nodes that SSU-rename the branch condition's
operand per-branch (`v_v1` for the True view, `v_v2` for the False
view — confirmed via the `ifa/tests/ir/ssu/14_isinstance_narrow.ir`
fixture; each per-branch view is its own `(Var, contour)` AVar, so
narrowing one can't corrupt the other). What this issue added:
`peel_wrapper_def` (`fa.cc`), which at a `Code_IF` walks back from the
condition Var's def PNode through pure-MOVE chains and the
`__pyc_to_bool__` binding/invocation pair every `if cond:` compiles
through, to find the real discriminator underneath — recognizing
`prim_isinstance`, `__is__`/`__nis__`, and the Python `isinstance`
wrapper SEND by callee name — and calls `flow_var_type_permit` on the
discriminated operand's per-branch SSU view. Gated by `--narrow` /
`IFA_NARROW` (default on). This is real, working infrastructure — it's
just not what's blocking Cases 1-3 (see below).

## Why the basic-type-union cases don't reduce to a narrowing fix at all (revised 2026-08-11)

Earlier revisions of this doc explained Cases 1/2 by a BOXING-gate
framing: pyc emits a `BOXING` violation for any AVar whose resolved
type set mixes *basic* types (`collect_var_type_violations`,
`mixed_basics`/`to_basic_type` in `analysis/clone.cc`), that check
runs on the **original** Var, not the per-branch SSU views the
narrowing mechanism refines, so even applied narrowing supposedly
couldn't help until the gate itself was rerouted (sketched as
"liveness-aware BOXING" / "SSU rewrite-and-prune," neither built).
That's real as far as it goes, but re-deriving Cases 1-3 from scratch
today shows it's not the *proximate* blocker: the minimal Case 1/3
repros above have **no discriminating predicate anywhere nearby** for
narrowing — built or hypothetical — to even engage with, and they fail
exactly the same way Case 2 does. The actual, common mechanism (see
[018](../../issues/closed/018-dict-mixed-key-types-boxing-failure.md) for the
full trace) is that a raw scalar union has no coherent runtime
representation *at all* — FA clones an inherently invalid
specialization for one union member (e.g. `str.__add__` fed an `int`)
with no salvage guard, and separately, `int64`/`str` are never
`Type_RECORD` so `cg_has_classtag` is always false for them — pyc's
polymorphic dispatch (built around classtags for class instances) has
no discriminator for a primitive-typed receiver at all, regardless of
narrowing.

This means even a *complete* narrowing implementation — branch-
correlation recognition (Case 1 as literally written) plus the
sketched SSU rewrite-and-prune (routing downstream uses to the
narrowed per-branch view) — would only help the subset of call sites
where a recognized discriminating predicate directly and syntactically
gates the consuming code. It would do nothing for Case 3 (the union
arises inside a *different* function than the one consuming it — no
local predicate to recognize) or for the plain-consumption shape
(no guard at all, arguably the most common real one). 018's fix
(element-CS method cloning, or general boxing) is the actual
dependency for Cases 1-3; this issue's own narrowing mechanism is not
the blocking piece, and the BOXING-gate sketches above are kept here
only as a historical record of a genuine but secondary/insufficient
angle — not the recommended next step.

## What actually fixed the isinstance-on-user-classes case (2026-08-03)

Not the narrowing mechanism above — a separate, unrelated bug in the
same territory, found while re-checking [030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md)'s
isinstance claims. The Python-level `isinstance()` (`__pyc__/05_builtins.py`)
is a thin wrapper; once two *different* classes are ever checked
anywhere in a program, FA generalizes both call sites into one shared
clone taking a runtime class argument (the same sharing risk
`PY_try_stmt`'s except-clause dispatch already avoids on purpose) —
and that shared clone gets mis-constant-folded to a hardcoded `return
0` (`False`), unconditionally, for every caller. (Never traced past
generated C to the specific FA pass/AVar responsible for the
mis-fold.) Fixed by sidestepping the shared clone entirely:
`build_builtin_call_pyda` now intercepts a direct 2-positional-arg
`isinstance(obj, cls)` call and builds the same raw
`sym_primitive`/`"isinstance"` send every *other* isinstance lowering
in this codebase already uses (is-None, `match`/`case`,
`except`-clause, `yield from`'s `StopIteration` check) — each call
site gets its own genuinely monomorphic send, so there's no shared
clone left for FA to mis-fold. This explains why `describe(a)`'s
`isinstance(a, Dog)` now correctly picks the right branch even though
`a`'s type is never actually narrowed *inside* that branch — the fix
makes the `isinstance` call itself return the right boolean; it's
orthogonal to the per-branch narrowing mechanism above. Note this fix
is for *class* receivers specifically — `isinstance(v, int)` against a
scalar-union receiver still constant-folds to `False` unconditionally
(confirmed while re-deriving Case 2 today), because the underlying
value has nothing to check in the first place (018's mechanism again).

## Pre-FA-inlining: name-based recognition (a fragility that did NOT bite)

`isinstance` is a one-line Python wrapper
(`__pyc_primitive__(__pyc_symbol__("isinstance"), obj, ...)`), and a
textbook single-SEND inlining candidate — but `simple_inlining` runs
in `ifa_optimize()`, *after* `fa->analyze()` in `ifa_analyze()`. By
the time `peel_wrapper_def` runs during FA, it sees a SEND to the
wrapper, not a direct `prim_isinstance` call, so the recognition above
matches the wrapper's callee name specifically rather than seeing
through arbitrary single-SEND wrappers generally. A user's own
discriminator helper (e.g. `is_kind_of(x, T)`) won't be recognized.
Structural fix — run `simple_inlining`'s single-SEND case before FA,
or FA→inline→re-FA — not attempted; cost implications unmeasured.

**2026-08-22: the mechanism above is accurate, the predicted
consequence is not.** A user-defined `is_end(n): return n is None`
driving the same linked-list narrowing compiles clean and prints the
right answer, so clone-time specialization is evidently carrying the
narrowing across that call boundary without `peel_wrapper_def` needing
to see through the helper. Kept as a description of how recognition
works; retired as a reason to do anything (item 3 under "What's still
open"). Cases 1-3 don't get this far regardless.

## What's still open

1. **Cases 1-3 (scalar/basic-type-union narrowing or plain
   consumption)** — now understood to be
   [018](../../issues/closed/018-dict-mixed-key-types-boxing-failure.md)'s
   gap, not this issue's own. Track status there; nothing in this
   issue's own per-branch narrowing mechanism blocks or unblocks them.
2. **Branch-correlation recognition** — Case 1's literal original
   mechanism: "the same Var tested as an earlier branch condition
   implies a value's type in a later branch testing that same Var."
   Never built; only `isinstance` / `is None` / `is not None` are
   recognized. **This is now the whole of what this issue is about**,
   and the class-typed analogue it was waiting on has been found (see
   the re-verification below): `tests/branch_correlation_class_union.py`,
   carrying a `.known_issue` tag pointing here.

   Cost is **spurious diagnostics, not wrong answers**. That bounds
   how much this is worth: nothing miscompiles, so the case for
   building correlation recognition rests on diagnostic quality, and
   on whether a shape exists where the warning escalates to a refusal.
   None found yet.
3. ~~The isinstance/`__is__` wrapper-recognition workaround
   (pre-FA-inlining) is name-based, not general.~~ **RETIRED
   2026-08-22 — asserted, never tested, and it does not reproduce.**
   The recognition IS name-based (that part is true), but the
   predicted consequence is not observable on the natural shape: a
   user's own helper

   ```python
   def is_end(n):  return n is None
   def sum_list(node):
       if is_end(node): return 0
       return node.value + sum_list(node.next)
   ```

   compiles with **no warnings** and prints the right answer, same as
   `tests/recursive_list_is_none.py`'s direct `if node is None`. FA's
   clone-time specialization evidently carries the narrowing across
   that call boundary without `peel_wrapper_def` needing to see
   through the helper at all. Not proof that no helper shape fails —
   but the concern as written was speculative, and one turn of
   evidence contradicts it, so it should not steer work until someone
   produces a shape that actually breaks.

## Re-verification 2026-08-22 (at `0f92e6d4`)

Everything below re-derived from fresh repros, not carried over.

**The two wins hold.** `tests/recursive_list_is_none.py` and
`tests/isinstance_union.py` both pass.

**Cases 1-3 still fail, identically, and still belong to 018.** All
three now compile with rc=0 and abort at runtime with
`matching function not found` — a change in *symptom* from the
2026-08-11 note, which recorded a hard C compile error and an LLVM
module-verification failure for Case 1. The 2026-08-19 refusal work
(issues/048 / [closed/052](../../issues/closed/052-llvm-nil-test-on-scalar-union-prints-none-for-zero.md))
moved several of these from "emit something invalid" to "refuse or
abort", so do not read the old error strings as current.

**The class-typed analogue of Case 1 exists** — the thing item 2 was
waiting on:

```python
if flag: a = Dog()
else:    a = Cat()
if flag: print(a.bark())    # warning: illegal call argument type ... Cat
else:    print(a.meow())    # warning: illegal call argument type ... Dog
```

Two spurious warnings, one per arm — and it **runs correctly on both
paths** (`woof` / `meow`, matching CPython), because classtag dispatch
resolves the call at runtime regardless of what FA believed. This is
not 018's territory: a class union has a coherent runtime
representation, which is exactly why it runs where the scalar cases do
not. Captured as `tests/branch_correlation_class_union.py` with a
`.check` recording **no warnings** — the right answer — plus a
`.known_issue` naming this issue, so it flips to PASS by itself when
correlation lands.

**Item 3 retired** — see "What's still open" above. The predicted
failure does not reproduce.

## Verification

- `tests/recursive_list_is_none.py` — passes both backends (re-run
  today).
- `tests/isinstance_union.py` — passes both backends (re-run today);
  covers the fixed isinstance-on-user-classes case plus two
  non-union sanity checks against different classes in the same
  function.
- `tests/isinstance_dynamic.py` — documents the builtin-type boundary;
  its gap is tracked in issue 018, not here.
- `tests/branch_correlation_class_union.py` — **this issue's own
  fixture** as of 2026-08-22. Reports KNOWN until correlation
  recognition lands, then PASS; its `.check` holds the correct
  (warning-free) compile output, so nothing has to be un-baked.
- Cases 1-3 and the bare-consumption/bare-`print` variants above all
  still fail (re-run 2026-08-22, now as a runtime abort rather than
  the compile error recorded in 2026-08-11) — no `tests/` fixtures
  exist for them; tracked under
  [018](../../issues/closed/018-dict-mixed-key-types-boxing-failure.md)
  going forward rather than here.
- Full `test_pyc.py`, both backends, whenever this issue's own
  narrowing mechanism (not 018's) is touched again.

## Cross-references

- [018](../../issues/closed/018-dict-mixed-key-types-boxing-failure.md) —
  **the actual blocker for Cases 1-3**; a raw scalar/basic-type union
  has no coherent runtime representation for any polymorphic consumer.
  Start there, not here, for that work.
- [closed/024](closed/024-is-comparison-narrowing.md) — `__is__`
  dispatch on union receivers; a dependency for the working `is None`
  case, satisfied.
- [closed/059](closed/059-narrowing-peel-wrapper-boolean-collapse-gap.md)
  — `peel_wrapper_def` didn't peel through `match`/`case`'s
  phi-merged-bool shape; **fixed 2026-07-22** (confirmed — an earlier
  revision of this doc still called it "root-caused, not fixed," which
  was stale).
- [030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md) — classtag
  dispatch/isinstance mechanism; confirmed sound in isolation, not at
  fault for the shared-clone mis-fold this issue fixed. Its classtag
  mechanism structurally can't cover Cases 1-3 either (`cg_has_classtag`
  requires `Type_RECORD`; scalars never qualify) — see 018.
- [`ifa/IFA.md`](../IFA.md) §6, [`ifa/CLONE.md`](../CLONE.md) —
  background on setter-splitting and clone-time specialization.

## What this unblocks

Honestly: **not much, and that is the point of the rescope.**

Already unblocked and shipped: real recursive data structures (linked
lists, trees) via `is None` narrowing, composing with 024.

What building item 2 would buy: two spurious warnings disappear from
`tests/branch_correlation_class_union.py`, and idiomatic correlated-
branch code over class unions stops being reported as an error when it
is not one. That is a **diagnostic-quality** win — the programs already
run correctly. Worth doing when someone is in `Code_IF` anyway; not
worth a campaign on its own unless a shape turns up where the warning
escalates to a refusal.

The scalar-union cases (Cases 1-3 as actually filed) are 018's to
unblock, not this issue's — and those DO miscompile, so that is where
the value is.
