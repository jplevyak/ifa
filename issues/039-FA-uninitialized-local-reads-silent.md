# Issue 039: Reading a local that's unassigned on some CFG path is silent undefined behavior — no lattice element for "uninitialized"

**Status:** open. **Design settled 2026-08-24; first implementation
landed DEFAULT OFF the same day and does not yet converge — see "Where
it stands".**

**Design settled 2026-08-24** (see "Design decision"
below, which supersedes parts of "Proposed direction"). **Found:**
2026-07-12, verifying the issue-023
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

## Design decision (2026-08-24)

Re-verified first: the repro still reproduces exactly as filed. `pyc`
compiles it clean (rc=0, no warnings) and prints `5 5` — the second call
reads stack garbage that happens to be 5 — where CPython raises
`UnboundLocalError`.

### Two levels, mapped onto flags that already exist

- **strict** (`--strict`) — a possibly-unbound read is a hard compile
  error.
- **non-strict** (`--permissive`, the default) — the slot is
  default-initialized and the program runs.

**No new flags.** pyc already has `--strict` / `--permissive` wired to
`runtime_errors`, and `show_violations` already prints `error` vs
`warning` off that. So modelling this as an
`ATypeViolation_kind` gives the two levels for free; the only extra work
in non-strict is the initializing codegen.

### THREE lattice facts, not two — this is the crux

"Unknown" and "unbound" are different, they are currently BOTH
unmodelled, and they demand OPPOSITE treatment in non-strict mode:

| fact | element | status today |
|---|---|---|
| **bottom** — no type reaches here at all | `bottom_type` | live; drives `NOTYPE` (`'x' has no type`) |
| **unknown** — a value definitely EXISTS, its type is not modelled | `unknown_type` / `sym_unknown` | element exists but is **never produced as a value** — `sym_unknown` has exactly two references, its creation and `initialize_global`. Its one live use is `dispatch_type` returning `sym_unknown_type` for OUT params (`pattern.cc:150`), and **pyc never creates an OUT sym**, so in pyc it is entirely dead |
| **unbound** — no value on THIS path, though the variable has a type on others | — | **not modelled at all.** This issue |

**So do NOT reuse `unknown` for this**, even though it is dead weight
and even though `type_union` is set-union over CreationSets (so
`{int64, unknown}` would stay a detectable 2-element set rather than
collapsing — that objection does not apply). The reason is semantic and
it bites in non-strict mode:

- `unknown` — a value exists; we merely cannot dispatch on it.
  Zero-initializing it would be **wrong**: it silently replaces a real
  inbound value with 0.
- `unbound` — no value exists. Non-strict MUST materialize one.

Two secondary reasons: `sym_unknown_type` already means "OUT parameter"
to `dispatch_type`, so reuse makes an unbound local and an OUT param
indistinguishable at every dispatch site; and `sym_any` implements and
specializes `sym_unknown_type`, putting unknown at the TOP of the
hierarchy, whereas absence-of-value belongs near the bottom. Neither
affects `type_union`, but both affect pattern matching.

### A fourth case, deliberately routed elsewhere

A variable unbound on EVERY path (`del x; print(x)`, or a name assigned
only in dead code) is `bottom`, not unbound-on-a-path. CPython also
raises `UnboundLocalError` there, but it should reuse `NOTYPE`'s
existing machinery rather than the new element.

### What non-strict initializes to

**The zero of the type inferred from the ASSIGNING paths** — not `None`
/ nil.

`y` is `int` on its assigning path, so fill `0` and the type stays
`int`, introducing no new union member. Injecting `None` instead would
create `{None, int64}`, which is exactly the union pyc now REFUSES as
unrepresentable ([../../issues/048](../../issues/048-none-int-field-pair-runtime-abort.md),
and [018](../../issues/closed/018-dict-mixed-key-types-boxing-failure.md),
closed 2026-08-24). Non-strict would then reject programs that strict
merely warned about — backwards.

This is well-defined precisely BECAUSE `uninitialized` is a distinct
union member sitting alongside the real types, rather than having
replaced them.

Note the deliberate CPython divergence: CPython raises
`UnboundLocalError`, non-strict pyc yields a defined default. Same shape
as the documented `{int,float}` coercion compromise
([../../issues/035](../../issues/035-list-element-cast-salvage-guard-and-set-item-union.md)).

## Where it stands (2026-08-24)

### The proposed mechanism is WRONG — measured, not argued

"Proposed direction" item 2 says to hook `get_Var`'s
no-reaching-definition fallback (`ssu.cc:101-105`). That fallback is not
the signal it looks like, **in both directions**, on this issue's own
four-line repro:

- **It fires for BOUND values.** 77 hits on that program, including
  `self`, `item` and `key` — formal parameters, always bound. The
  renaming environment is scoped, so a formal's binding is not visible
  at every point a phi argument is resolved.
- **It does not fire for the actual unbound local.** `y`'s phi resolved
  with `env_hit=1` on **both** predecessor edges. The push/pop is
  conditional (`if (d->cfg_succ.n != 1 && d->cfg_pred.n != 1)`), so a
  plain `if` with no `else` leaks the assigning branch's renaming onto
  the non-assigning edge. There is no fallback to hook for the very
  shape the issue is about.

So the fact is **not recoverable from renaming state**. Item 2 should be
struck.

### Nor is a lattice element needed

"Proposed direction" item 1 (an 18th canonical `AType`) was implemented
and then **reverted**. Definite assignment is a CFG property, not a type
property, so it rides on `Var::maybe_unbound` instead.

Reverting was not just tidiness: adding `sym_unbound` / `sym_unbound_type`
to `builtin_symbols.h` **renumbers every Sym id**, which broke 16 `dce`
ir-goldens. `ATypeViolation_kind::MAYBE_UNBOUND` is likewise appended
LAST, since inserting mid-enum renumbers `CLOSURE_RECURSION`.

### What is implemented

- `Var::maybe_unbound` (`if1/var.h`).
- `find_maybe_unbound` (`optimize/ssu.cc`), a forward MUST analysis:
  `assigned_out[n] = assigned_in[n] u lvals(n)`, `assigned_in[n]` the
  intersection over predecessors, entry seeded empty (formals arrive as
  entry's own lvals), initialised optimistically and shrunk. Dense
  bitmaps — a `Vec<Var*>` set with a linear `set_in` inside the fixed
  point did not finish on the builtin library.
- `ATypeViolation_kind::MAYBE_UNBOUND`, collected in
  `collect_var_type_violations` and reported by `show_violations`.
  Reporting it as an ATypeViolation is what buys the two levels with no
  new flags: error under `--strict`, warning under `--permissive`.

### THE BLOCKER: the fixed point does not converge

**Default OFF (`IFA_UNBOUND=1` to enable), hard-bounded at 200 sweeps,
and exhausting the bound abandons the result rather than reporting a
wrong one.** With it off, all five CI gates pass (296 passed / 0
failed).

Enabled, ~42 functions in the builtin library never reach a fixed point.
Measured on `__pyc_tolist__` (19 nodes, 13 locals, so ≤247 bit-changes
should suffice): the total popcount descends monotonically
1152 → 1090 → 1028 → … → 111, and then **flips bits forever at constant
popcount 111** — a genuine cycle, not slowness.

Ruled out, each by direct probe: duplicate nodes in the node list (0),
predecessors missing from the map (0), self-loops (0), unreachable
non-entry nodes (0), and the map failing to return what was stored (0
mismatches). The update is monotone by inspection — `in` is the
intersection of predecessors' current `out`, `out := in u gen`, `gen`
constant, everything initialised to top — so a bit should never be able
to come back on. It does. **That contradiction is the thing to debug
next**, and it is the whole remaining blocker.

Also unverified as a consequence: whether the flag reaches
`show_violations` end to end, and the non-strict zero-init codegen,
which was not started.

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
  [033](closed/033-splitter-non-idempotent-divergence.md) and
  [035](closed/035-nondeterministic-codegen-clone-order.md) — different
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
