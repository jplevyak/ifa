# Issue 039: Reading a local that's unassigned on some CFG path is silent undefined behavior — no lattice element for "uninitialized"

**Status:** open. **Found:** 2026-07-12, verifying the issue-023
`match`/`case` capture-pattern fix (`9b73ed62`).
**Affects:** `ifa/optimize/ssu.cc` (phi placement / rename — the
mechanism), `ifa/analysis/fa.h`/`fa.cc` (`TypeWorld`'s canonical
`AType`s, `ATypeViolation_kind` — the proposed fix surface),
`ifa/codegen/cg.cc` and `ifa/codegen/cg_emit_llvm.cc` (both
backends' alloca/local-slot emission — where the silence
ultimately manifests as a real bug).

## Symptom

A local variable read on a control-flow path where it was never
assigned reads whatever garbage happens to be in its storage slot
(stack/alloca reuse from a prior call, typically) instead of
producing any defined value or a diagnostic. Two independent,
minimal repros, both compiling clean with zero warnings:

```python
def f(cond):
    if cond:
        y = 5
    print(y)

f(True)
f(False)
```
Real Python: `5` then `UnboundLocalError: cannot access local
variable 'y' where it is not associated with a value` (an
`UnboundLocalError`, not a `NameError` — CPython's compiler already
knows `y` is local to `f` because it's assigned *somewhere* in `f`,
regardless of the runtime branch). pyc: `5` then some other
stack-garbage integer — different every run, and in the specific
case observed happened to also print `5` due to coincidental stack
reuse between the two calls to `f`, which is exactly the kind of
result that makes this class of bug easy to miss in casual testing.

```python
def classify(val):
    match val:
        case 1:
            print("one")
        case x:
            print("other:", x)
    print("after match, x is", x)

classify(1)
```
Same shape, reached via the issue-023 capture-pattern fix: `x` is
correctly scoped as local to the whole function (matching Python's
"assigned anywhere ⇒ local everywhere" rule — confirmed pyc does
NOT fall back to an outer/global `x` on the `case 1:` branch, which
would be a *different*, worse bug), but reading it after a branch
that never executed `case x:` reads garbage instead of erroring.

## Root cause

Traced to the SSU phi-placement/renaming pass, not anything specific
to `match`/`case` (issue 023's fix is not implicated — this
reproduces identically with a plain `if`).

1. **`place_phi` (`ssu.cc:133-156`) is liveness-driven, not
   definite-assignment-driven.** A phi gets inserted at a CFG join
   for variable `v` when `v` is `maybe_live` there (`ssu.cc:66`,
   `n->live_vars->get(v) != 0`) — i.e. "is `v` possibly used after
   this point," the standard SSA-construction criterion. This is
   correct for placing phis *where needed*, but it says nothing
   about whether every predecessor edge actually has a reaching
   definition to feed that phi. A join with one assigning
   predecessor and one non-assigning predecessor gets a phi placed
   just the same as a join where both predecessors assign.

2. **`get_Var` (`ssu.cc:101-105`) has no "no reaching definition"
   case.** When `rename_edge` (`ssu.cc:108-122`) fills in a phi's
   rval for a given predecessor edge, it calls `get_Var(v, env, f)`,
   which does `env.get(v)` (the current SSA renaming environment)
   and, on a miss, falls back to `return v` — the **original,
   un-renamed Var**, not any kind of "unassigned" marker:
   ```cpp
   static inline Var *get_Var(Var *v, VarEnv &env, Fun *f) {
     if (!v->sym->is_local) return v;
     Var *vv = env.get(v);
     if (vv) return vv;
     return v;
   }
   ```
   On the predecessor edge that never assigned `v`, this silently
   feeds the phi the variable's own pre-assignment storage identity
   — the same slot every other read of `v` shares — rather than
   anything FA could recognize as "empty/unknown on this path."

3. **Consequently FA's type union at the join doesn't see a gap.**
   `ATypeViolation_kind::NOTYPE` already exists precisely for "this
   live AVar's `out` type is `bottom_type` (no type at all)"
   (`collect_var_type_violations`, `fa.cc:3288-3297`) and does fire
   in some shapes (observed as `warning: 'x' has no type` on an
   unrelated mixed-literal-type match test during this
   investigation) — but it doesn't fire here, because the
   unassigned-edge's contribution isn't `bottom_type`, it's just...
   whatever the *other* (assigning) edge established, absorbed
   without a trace through the shared, un-renamed Var identity.
   `NOTYPE` is built to catch "no type anywhere," not "no type on
   *this* path, but a real type on others."

4. **Both backends leave the slot genuinely uninitialized.**
   Neither `cg.cc` (C backend local declarations) nor
   `cg_emit_llvm.cc` (`CreateAlloca` at `cg_emit_llvm.cc:2733`,
   phi-target slots) zero-initializes or sentinel-fills a
   phi-target's storage. This is consistent with the type system
   giving codegen no reason to think it should — from FA's
   perspective, by the time codegen runs, `y`'s type is a clean
   `int`, no hint that one path never wrote to it.

## Proposed direction

Make "uninitialized on this path" a first-class, trackable fact in
the *same* type lattice every other value already flows through,
rather than a special case bolted onto renaming. Concretely:

1. **Add an 18th canonical `AType`** to `TypeWorld`
   (`fa.h:446-473`, alongside `bottom_type`, `nil_type`,
   `unknown_type`, etc.) — call it `uninitialized_type`. Populated
   in `TypeWorld::initialize()` the same way the existing 17 are.
2. **At `get_Var`'s no-reaching-definition fallback**
   (`ssu.cc:101-105`), instead of silently returning the original
   Var, contribute `uninitialized_type` as that edge's value for the
   phi. (Mechanically this likely means: the fallback path needs to
   route through a distinguishable "no def" Var/AVar pairing that
   FA's ordinary backward-edge type union already knows how to fold
   in as `uninitialized_type`, rather than literally reusing the
   pre-renaming Var's own identity — needs a design pass on exactly
   how to thread this without disturbing the renaming environment's
   invariants elsewhere; not a one-line change.)
3. **A live AVar whose `out` type includes `uninitialized_type`**
   (a real union member, not the whole type) is now staticaly
   detectable — the same way `mixed_basics` already detects a
   union of incompatible basic types for `BOXING`
   (`fa.cc:3298-3313`). Add
   `ATypeViolation_kind::MAYBE_UNINITIALIZED` (or fold into
   `NOTYPE`'s reporting path if the distinction doesn't earn its
   own kind) and collect it in `collect_var_type_violations`
   alongside the existing `NOTYPE`/`BOXING` passes.
4. **This is strictly better than CPython's runtime
   `UnboundLocalError`**: pyc would catch this at compile time,
   before the program ever runs, for any case FA can prove
   statically — which is most cases, since it's exactly the kind of
   local, intraprocedural fact flow analysis is good at. A residual
   runtime guard (check-and-raise, or check-and-fall-back-to-a-
   defined-sentinel) is a reasonable fallback for the cases FA
   can't fully resolve (e.g. genuinely runtime-dependent branches
   the type system doesn't attempt to prove infeasible), using the
   same `uninitialized_type` tag to decide where a guard is needed
   — but the compile-time diagnostic is the main win and doesn't
   need the runtime piece to be useful on its own.

## What this unblocks

- Correct, *loud* behavior (compile-time diagnostic, ideally) for
  every existing and future CFG shape where SSU places a phi across
  a predecessor lacking a reaching definition — not just
  `match`/`case` capture patterns (found while verifying issue
  023's fix), but any conditionally-assigned local anywhere in the
  language: `if`/`elif` chains missing a final `else`, loops whose
  body may execute zero times, `try`/`except` (once issue 011
  lands), etc. This is a foundational correctness gap in the local-
  variable model, not a syntax-specific one.
- A real, static analogue of CPython's `UnboundLocalError` — pyc
  currently has no equivalent at all; today an uninitialized read
  is silently accepted and produces platform/build/stack-layout-
  dependent garbage, the same general class of nondeterminism this
  project has already chased down twice this year in unrelated
  subsystems (see
  [033](033-splitter-non-idempotent-divergence.md) and
  [035](035-nondeterministic-codegen-clone-order.md) — different
  root causes, same "silent, layout-dependent wrongness" shape).

## Effort estimate

Comparable to issue 023's class/sequence/mapping pattern work — a
small feature in its own right, not a quick fix:
- New canonical `AType` + `TypeWorld::initialize()` wiring: small,
  well-trodden (17 existing precedents to copy).
- Threading `uninitialized_type` through `get_Var`'s fallback
  without breaking renaming invariants elsewhere in `ssu.cc`: the
  real unknown — needs a careful look at every other `get_Var`
  call site and at how `rename_edge` uses the phi rvals downstream
  before committing to the exact mechanics.
- New `ATypeViolation_kind` + `collect_var_type_violations` wiring:
  small, direct precedent in the same function (`NOTYPE`/`BOXING`).
- Optional runtime-guard codegen (both backends): medium, only
  needed if compile-time proof isn't judged sufficient on its own.

## Verification plan

1. Both repros above (`f(cond)`; the `match`/`case` capture shape)
   should either produce a compile-time diagnostic or, at minimum,
   a well-defined runtime error/sentinel — never silent garbage.
2. Sweep the existing test suite for any test that happens to rely
   on (or accidentally tolerates) an uninitialized read going
   unnoticed — none expected, but worth checking given how long
   this has apparently gone undetected.
3. A dedicated ifa-test synthetic fixture (mirrors this codebase's
   usual pattern) exercising: if-without-else, a loop that may run
   zero times, and the match/case capture shape, each checked for
   the new diagnostic.
