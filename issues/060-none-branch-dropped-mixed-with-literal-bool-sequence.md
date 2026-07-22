# 060 — `case None:` silently disappears from codegen when combined with a literal/True-False/sequence pattern

**Status:** BOTH mechanisms fixed 2026-07-21.
**Mechanism 1** (`python_ifa_build_if1.cc`'s `build_isinstance_call`):
switched to the raw `sym_primitive` isinstance send instead of the
shared wrapper. **Mechanism 2** (the deeper, general one):
`type_cannonicalize` (`fa.cc`) now keeps `nil_type` in the AType
`->type` projection whenever the union also carries a `num_kind`
scalar, so IFA's type-splitter separates the `None` value into its own
contour instead of merging it with the scalar and coercing it to
`(scalar)NULL`. `is None`/isinstance then folds statically per contour.
Verified: general (non-`match`) repro, all four `match` combinations,
`Optional[pointer]` still single-clone (frontend-sanctioned merge
preserved), full suite 220/220 both backends, `ifa --test` 58/0,
`test_llvm`, shedskin sweep (no regressions; `chess` advances
`FAIL`→compiles). Regression tests: `tests/none_scalar_split.py` (the
general fix) and `tests/match_none.py` (all four `match` combinations).
With both mechanisms fixed, `issues/023`'s compile-time guard and its
two helper functions have been **removed** — `case None:` now composes
with every pattern kind.
**Affects:** `python_ifa_build_if1.cc`'s `build_isinstance_call`
(mechanism 1) and `ifa/analysis/fa.cc`'s `type_cannonicalize`
(mechanism 2).
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
see mechanism 2 (now also fixed).

## Root cause 2 (deeper, general): `None` and a falsy raw scalar
(`False`, `0`, `0.0`) share the identical zero/NULL bit pattern in an
**unsplit** contour

> **Fixed** by making FA split that contour — see "Fixes" above. The
> symptom below is the *unsplit* behavior; the real defect was FA
> merging `None` with a scalar in the first place (root cause: nil
> stripped from `->type`), not anything codegen could paper over. The
> analysis of the two `cg.cc` dispatch paths is kept because it's how
> the bug was first localized and confirms why a codegen-only patch
> was the wrong layer.

pyc avoids boxing an `Optional[int]`-shaped parameter: when FA doesn't
split a formal's union into separate per-type clones, a raw scalar
type (`int64`, `bool`, `float`) sharing a contour with `None`
represents `None` as literal `0`/`NULL` in that same slot. Confirmed
directly in generated C for `case None: / case True: / case False:`
called with `test(False)`, **before** the split fix:

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

## Fixes

1. **Mechanism 1 — DONE.** Ported issue 011's raw `sym_primitive`
   isinstance send into `build_isinstance_call`, exactly as done for
   `except`-clause dispatch.
2. **Mechanism 2 — DONE via splitting (Option B), the correct lever.**
   IFA's core discipline is to split incompatible types, and `None` is
   incompatible with a raw scalar — they share the zero bit pattern
   under the unboxed representation. The one-line-idea fix: in
   `type_cannonicalize` (`fa.cc`), stop unconditionally stripping
   `nil_type` from the `->type` projection; keep it whenever the union
   also carries a `num_kind` scalar. Every type-based split gate
   (`collect_type_confluence`, `edge_type_compatible_*`,
   `find_best_entry_sets`) partitions on `->type`, so once `{None,
   scalar}` presents as a genuine two-type union there, FA naturally
   splits the `None` caller into its own contour — no new machinery,
   no special-case codegen. `is None`/isinstance then folds to a
   compile-time constant in each monomorphic contour. Combining `None`
   with a **pointer** type is unchanged (no scalar → nil still
   stripped → single clone, `None` as null pointer): that merge is a
   frontend-sanctioned allowance for a language where a null pointer
   unambiguously means `None`, not IFA's default. The earlier
   "Option A" (bespoke per-scalar runtime tags / NaN-boxing) is
   **not** needed. See the investigation notes below for why the
   codegen-side `cg.cc` approach was the wrong layer.

### Investigation that led to the splitting fix (2026-07-21)

The following traces *why* splitting is the right lever and how the
merge was happening — kept for the next investigator. (Earlier this
section argued splitting was "not a contained fix"; that conclusion
was wrong. The blocker it described — the merged formal losing `None`
before the splitter runs — was itself a downstream effect of the
`->type` stripping, and fixing the stripping at the source in
`type_cannonicalize` dissolves it.)

**The bug is fully general — not `match`/`case`-specific at all.** The
minimal reproducer needs no `match`, no pattern-matching lowering,
nothing pyc-frontend-specific:

```python
def show(v):
    if v is None: print("none")
    else:         print("notnone")
show(None)   # none
show(True)   # notnone
show(False)  # notnone  -- pyc prints "none"
```
`show(int)` collapses identically (`show(None)`/`show(5)`/`show(0)` →
`none`/`val`/`none`, wrong on the last). Reproduces with `IFA_NARROW=0`
— not the narrowing feature.

**What FA actually does** (traced via `[ses]`/confluence logging plus
`update_in`/EntrySet-formal instrumentation, all reverted):

1. All three call edges (`show(None)`, `show(True)`, `show(False)`)
   merge into **one** shared `EntrySet`. The generated C is a single
   clone `show(_CG_bool a1)` (or `_CG_int64` for the int case), and
   the calls compile to `show((_CG_bool)NULL)`, `show(1)`, `show(0)` —
   so `None` and `False` are **the identical bit pattern `0`** at the
   ABI boundary, and the body's `v == NULL` matches both.
2. The merge happens because `nil_type` is an `is_unique_type` Sym
   (`ast.cc` ~205) and is **stripped from the AType `->type`
   projection** by `type_cannonicalize` (`fa.cc` ~613: `if
   (!cs->sym->is_unique_type) nonconsts.set_add(cs); else nulls = 1;`).
   The projection of `{None, bool}` is just `{bool}`. Every gate in the
   type-based splitter partitions on `->type`:
   `collect_type_confluence`, `edge_type_compatible_with_edge`,
   `edge_type_compatible_with_entry_set`, and `find_best_entry_sets`'s
   `entry_set_compatibility`. So a `{None, scalar}` formal looks
   **monomorphic scalar** to all of them — no confluence is ever
   recorded, no split is ever attempted.
3. Even the routing back-stop is soft: `entry_set_compatibility`
   returns a *penalty* (`val -= 4`), not a hard reject, for a
   type-incompatible edge — so with only one candidate ES the `None`
   edge merges into the scalar ES anyway.
4. Confirmed the distinction *is* preserved when the parameter is
   dead: `show(v){ print("x") }` with `v` unused compiles to two
   trivial no-arg clones. The bug specifically needs `v` **live** (an
   `is None`/isinstance check makes it live), which is exactly when the
   scalar-typed shared clone forms.

This is the same design that makes pointer-shaped `T | None`
(`Optional[Node]`) *correctly* stay unboxed and unsplit — `None` as a
null pointer is unambiguous there, and issue 025's "`Node | None`
doesn't trigger BOXING" refinement relies on it. The scalar case is
the one place the stripping is unsound, because `0` is a legitimate
scalar value.

**First experiment (too shallow — superseded).** A narrowly-gated
nil-aware split *view* was added only in the splitter's own gates
(helpers `atype_has_nil` / `atype_has_num_scalar` / `split_view_pair`,
wired into `collect_type_confluence`, `edge_type_compatible_with_edge`,
`edge_type_compatible_with_entry_set`). **It didn't fire**, and the
reason pointed straight at the real fix: by the time the splitter runs
the merged formal's `out` reads `{bool}`, not `{None, bool}`, even
though the `None` caller edge genuinely carries `nil_type` (verified:
its actual-arg `AVar` is `{__pyc_None_type__}`). The nil is stripped
by the `->type` projection *upstream* of the splitter — in the flow,
routing, and canonicalization that all consult `->type`. Patching the
gates alone was treating symptoms.

**The actual fix: strip nil at its source condition, in
`type_cannonicalize`.** Keeping `nil_type` in `->type` for the
`{nil, scalar}` mix (and only that mix) makes the whole pipeline
nil-aware at once — the flow no longer converges `None` away, edge
routing sees the two-type union, and the confluence/grouping gates
partition correctly — because every one of them reads `->type`. The
feared blast radius did not materialize: `Optional[pointer]` is
untouched (no scalar → nil still stripped → single clone), and the
shedskin corpus sweep showed **no regressions** (one example, `chess`,
advances from `FAIL` to compiling). The change is ~20 lines in one
function, gated by `has_scalar`.

### Option A (distinct runtime tags per scalar kind) — NOT needed

Kept for context; the splitting fix above makes this unnecessary. The
representation collapse also has a single codegen site,
`assign_type_cg_strings_pass2` (`ifa/codegen/codegen_common.cc`
~555-569):

```cpp
if (s->type_kind == Type_SUM && s->has.n == 2) {
  if (s->has[0] == sym_nil_type)
    cg_set_string(s, cg_get_string(s->has[1]));
  else if (s->has[1] == sym_nil_type)
    cg_set_string(s, cg_get_string(s->has[0]));
}
```

This collapses **every** 2-element `SUM{None, T}` to `T`'s own raw
C type, unconditionally — correct when `T` is pointer-shaped (a null
pointer already means `None` unambiguously) but wrong when `T` is a
raw scalar (`int64`/`bool`/`float64`), where `None`'s chosen `0`
encoding collides with a legitimate value of `T`. Fixing this
requires making the collapse conditional on `T`'s shape, and picking
a real per-`T` "is this `None`" discriminant when `T` is scalar.
Per-scalar-kind feasibility:

- **`bool`** (`_CG_bool` is a full `uint8`, only values 0/1 are ever
  legitimate): free. Use `2` as the `None` sentinel — no storage
  overhead, no collision risk, no semantic gap. The easy case.
- **`int64`/other fixed-width ints**: no spare bit pattern exists —
  every value in range is legitimate. Would need a reserved sentinel
  (e.g. `INT64_MIN`) to represent `None`, which is a **real, narrow
  semantic gap**: a program that legitimately stores that exact value
  in an `Optional[int]` slot would misbehave. Same *class* of
  deliberate CPython-divergence pyc already accepts elsewhere (the
  numeric-confluence 0-vs-0.0 compromise noted in
  [025](025-intra-function-union-narrowing.md)), but this one is
  user-visible in a more direct way and should be a conscious,
  signed-off decision, not silently shipped.
- **`float64`**: NaN-boxing — reserve one specific NaN bit pattern as
  the `None` sentinel, distinguishable from any NaN real computation
  produces (well-precedented technique, used by V8/SpiderMonkey/
  LuaJIT). No semantic gap in practice (a NaN is already
  non-representable-as-a-normal-value semantically), but touches
  every float primitive that could produce a NaN, since results must
  avoid colliding with the reserved payload.

**Assessment: the practical near-term fix.** Confined to one codegen
site (plus `cg.cc`'s two `emit_send_is` branches, already mapped in
mechanism 2's root-cause section, which would need to read whatever
new discriminant this introduces), doesn't touch the splitter at
all, and `bool` alone would fully fix `none_bool.py` (issue 023's
last blocked combination) at zero semantic cost. `int64`/`float64`
support is separable, follow-on work with its own tradeoffs to sign
off on.

Note the standalone `isinstance(v, int)` symptom (mechanism 2's
root-cause section) also disappears with the split fix: once FA gives
`int` and `None` separate contours, the `isinstance` folds to a
compile-time constant in each and `cg.cc`'s classtag-disjunction path
is never reached with a merged `{int, None}` operand. (A defensive
compile-time diagnostic for a genuinely classtag-less runtime
`isinstance` remains reasonable belt-and-suspenders, but is no longer
load-bearing for this bug.)

## Verification plan

1. Done: all four `match` combinations (literal, capture, sequence,
   `True`/`False`) match CPython — verified with the guard temporarily
   removed (`tests/none_scalar_split.py` covers the general,
   non-`match` core; the `match` combos were spot-checked directly).
2. Done: the standalone `isinstance(v, int)` / `is None` repro with no
   `match`/`case` (`show(None)`/`show(True)`/`show(False)`,
   `show(5)`/`show(0)`) matches CPython — this is the fundamental fix,
   captured by `tests/none_scalar_split.py`.
3. Done: `None` + pointer (`Optional[Node]`) still compiles to a
   single clone (no split, no contour explosion), output correct —
   frontend-sanctioned merge preserved.
4. Done: 059's own verified combinations (capture, class, mapping)
   re-checked, unaffected.
5. Done: full suite (219/219 both backends), `ifa --test` (58/0),
   `make test_llvm`, shedskin corpus sweep (no regressions; `chess`
   advances `FAIL`→compiles, `sudoku5`'s pre-existing issue-034 assert
   only shifts line number).

## What this unblocks

Both mechanisms are now fixed, so
`../../issues/023-structural-pattern-matching.md`'s compile-time guard
(`pattern_contains_none` / `pattern_is_risky_with_none` in
`build_match_pyda`) and its two helper functions have been **removed** —
`case None:` now composes with every pattern kind, closing issue 023's
last limitation. `tests/match_none.py` was rewritten from an
expected-limitation test into a passing combination test covering
`None` + literal, `None` + `True`/`False`, `None` + sequence + capture,
and `None` + bare capture, matching CPython on both backends. The
general `None`-plus-scalar soundness fix also matters well beyond
`match`/`case`: it corrects any `Optional[int|bool|float]` value
flowing to a shared function that tests `is None`/`isinstance`.
