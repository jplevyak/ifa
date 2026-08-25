# Issue 039: Reading a local that's unassigned on some CFG path is silent undefined behavior — no lattice element for "uninitialized"

**Status:** open. **Design REVISED 2026-08-25: two severities
(definitely / possibly) x three environments (strict / permissive /
safe), and the policy must be language-neutral — see the top section.**
Implementation is DEFAULT OFF; the `possibly` analysis converges and
reports at two levels, `definitely` is not yet computed.

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

## Design REVISED 2026-08-25 (author) — two severities x three environments

This supersedes the 2026-08-24 section below wherever they disagree. The
earlier design had one fact ("possibly unbound") and two environments,
and that could not work: see (b) under the 2026-08-24 implementation
notes — `if first or d < bd:` is a TRUE "possibly unbound" and a VALID
Python program, so a strict compile error would reject legitimate code.

### Two levels of the fact

- **DEFINITELY** used unbound: NO path reaching this use assigns the
  variable. `def f(): print(y); y = 1`.
- **POSSIBLY** used unbound: some path assigns it, some does not.

They fall out of the same dataflow with two different meets: MUST
(intersection) gives "definitely assigned", MAY (union) gives "assigned
on at least one path". Then

    definitely_unbound = not may_assigned
    possibly_unbound   = may_assigned and not must_assigned

### Three environments

| | definitely | possibly |
|---|---|---|
| **strict** | compile error | compile **warning**, plus the runtime check |
| **permissive** | compile error | **runtime error** (inserted check) |
| **safe** (auto-initialize) | compile error | **auto-initialize**, no check |

So: **definitely is ALWAYS a compile error**, in every environment —
there is no path on which the program is correct, so nothing is lost by
refusing. **Possibly is a strict-mode warning and a runtime error**,
unless auto-initialize is set, in which case the slot is initialized
instead.

That is what makes the short-circuit case work: `if first or d < bd:`
is *possibly* unbound, so it compiles everywhere, gets a runtime check
under strict/permissive that never fires (because `or` short-circuits
the read away), and gets an initialization under safe. All three
behaviours are correct for it.

`safe` is a NEW environment. pyc today has only `--strict` /
`--permissive`.

### It must be GENERAL — ifa is not a Python compiler

ifa targets more than Python, so none of the policy may be hardcoded:

- The **analysis** (both meets, both facts) belongs in ifa and is
  language-neutral.
- The **diagnostic wording** may not say `UnboundLocalError`.
- What a runtime check RAISES is a frontend decision and belongs behind
  `IFACallbacks` — pyc raises `UnboundLocalError`, another frontend does
  something else, and a frontend that wants neither can decline.
- What auto-initialize FILLS WITH is likewise per-frontend, though the
  ifa-level default (the zero of the type inferred from the assigning
  paths — see 2026-08-24) is language-neutral and should stand.

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

### The convergence bug: FIXED — it was iteration order

The fixed point failed on ~42 builtin functions. Cause: the sweep
visited `nodes` in collection order. Measured on `__pyc_tolist__`, a
single bit travelled around a cycle forever at constant total popcount
(node 275 lost bit 8, 273 gained it, then 275 gained it back).

**Iterating in REVERSE POSTORDER fixes it.** A forward analysis visits
every predecessor before its successors in RPO, so one sweep propagates
a change the whole length of an acyclic path instead of one edge per
sweep. Ruled out before finding this, each by direct probe: duplicate
nodes, missing predecessors, self-loops, unreachable nodes, and the map
failing to return what was stored — all zero.

### Three ordering traps, all the same shape

Each cost a wrong result that looked like a different bug:

1. **`Var::is_formal` is not set at SSU time** — `build_patterns` sets
   it, and that runs inside FA. Seeding formals from it seeded nothing,
   and the analysis reported essentially every parameter in the builtin
   library as possibly unbound (485 in `___init___` alone). Read formals
   from `f->sym->has` instead.
2. **`Var::live` is not set at `collect_var_type_violations` time** —
   dead-code elimination sets it, later still. Requiring it silently
   suppressed every report.
3. **SSU RENAMES Vars** (`new Var(v->sym)`) after this pass runs, so a
   flag on the pre-rename Var never reaches FA. The flag lives on
   `Sym::maybe_unbound`; the sym is shared across renamed copies and is
   per-function for a local, which is the right granularity.

(Adding that Sym bit also produced `fail: no instance for type 'int'`
until a `make clean` — the header-rebuild gotcha, not a code bug.)

### Both levels work, end to end

    $ pyc u1.py                 # non-strict, the default
    u1.py:2:11: warning: 'y' may be used before assignment on some path; type is:int64
    rc=0

    $ pyc --strict u1.py
    u1.py:2:11: error: 'y' may be used before assignment on some path; type is:int64
    rc=1

No new flags, exactly as designed — reporting it as an ATypeViolation
inherits `show_violations`' existing severity logic.

### THE BLOCKER NOW: it over-reports

Enabled, 8 suite tests fail. 549 of the 550 raw flags on the repro are
unnamed compiler temporaries (already filtered out of the diagnostic —
the FLAG stays set on them because non-strict initialisation wants
exactly those slots). What remains is two distinct categories:

**(a) match/case capture lowering — 7 tests.** `case [[a, b], c]:`
warns about `a` at a use INSIDE its own branch, where the pattern has
certainly bound it. A false positive, and the shape is 039's own second
repro, so the lowering needs looking at before this can be trusted.

**(b) A TRUE positive that must not be an error.**
`tests/scope_read_before_write.py` does
`if first or d < bd:` and its own comment says it "reads bd before its
first write". The analysis is right — and the program is **valid
Python**, because `or` short-circuits so the read never happens on the
first iteration. CPython accepts it.

**(b) is a design finding, not a bug.** It means strict mode cannot
simply error on may-be-unbound, or it rejects legitimate programs.
Either the analysis must model short-circuit evaluation, or strict must
only reject where the read is unbound on a path it can prove
*reachable*. That decision has to be made before this is enabled by
default, and it was not visible until the analysis worked.

### Not started

The non-strict default-initialisation codegen (fill with the zero of the
type inferred from the assigning paths). It waits on (a) and (b),
because initialising on a false positive is a silent semantic change.

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

## Where it stands (2026-08-25) — both facts land; policy half-done

Supersedes "Where it stands (2026-08-24)". Behind `IFA_UNBOUND=1`,
default OFF. Five CI gates green with it off.

### Landed

- **Both meets, one sweep.** `find_maybe_unbound` in `optimize/ssu.cc`
  runs a forward MUST (intersection, top-init) and a MAY (union,
  bottom-init) over the same bitvectors, in **reverse postorder** —
  RPO is not a tuning choice, collection order did not converge
  (~42 builtin functions cycled forever at constant popcount).
- **The facts live on `Sym`, not `Var`.** SSU renames Vars *after* this
  pass runs (`new_Var` makes `new Var(v->sym)`), so a flag on the Var is
  invisible to FA. `Sym::maybe_unbound` / `Sym::definitely_unbound`.
- **Formals come from `f->sym->has`,** not `Var::is_formal` — that bit is
  set by `build_patterns` *inside* FA, i.e. after SSU. Reading it here
  gave 2158 flags instead of 550.
- **No `Var::live` filter** at violation-collection time: that bit is set
  by dead-code elimination, which runs after FA, so it is 0 for
  everything and suppressed every diagnostic.
- `ATypeViolation_kind::{MAYBE,DEFINITELY}_UNBOUND`, appended **last** in
  the enum (inserting mid-enum renumbers `CLOSURE_RECURSION`).

### Severity, as the revised design specifies

| | permissive | strict |
|---|---|---|
| definitely | **error, rc=1** ✓ | **error, rc=1** ✓ |
| possibly | warning, rc=0 ✓ | **warning, rc=0** ✓ |

`DEFINITELY_UNBOUND` joins BOXING in `always_fatal`. `MAYBE_UNBOUND` is
`always_warning` — it is *not* escalated by `--strict`, and is excluded
from the fatal count in `FA::analyze` — because its enforcement is the
runtime check, not a compile-time refusal.

### The over-report is inherent, and it settles the default

With the pass on, 8 of 296 tests differ, all COMPILE-OUT. Every one is a
**correct** "possibly unbound" in the CFG and a **safe** program in fact:

- `scope_read_before_write.py`: `if first or d < bd:` — `bd` is read only
  when `first` is False, which is exactly when it was assigned.
- `exception_propagation.py`: `v = risky(n)` in a `try`, `return v` after
  the `finally`; the flagged path is the one where the exception
  re-propagates and `return v` is never reached.
- `match_seq.py` and 5 other match tests: `case [[a, b], c]` — captures
  are bound exactly when the arm body is entered.

All three are safe for a **correlation** reason — between a flag and a
write, between an exception edge and a return, between a pattern test and
its body — that a MUST/MAY dataflow structurally cannot represent. These
are not lowering bugs to be fixed one by one; sharpening the analysis
does not remove the category.

So: **the "possibly" compile-time warning should stay off by default**
(it is advisory and noisy), while the runtime check is what enforces it,
and `definitely` — which has no false positives, since no path assigns —
is what goes on by default as an error.

### Not implemented

1. **The runtime check for `possibly`.** The whole enforcement story.
2. **`safe` / auto-initialize.** A new environment; pyc has only
   `--strict` / `--permissive`.
3. **The `IFACallbacks` hooks** that keep 1 and 2 general — what a check
   raises, what auto-init fills with. Nothing may name `UnboundLocalError`
   in ifa.

## `safe` lands at codegen — and only half works (2026-08-25)

`--safe` exists (pyc.cc `safe_mode_arg`, mirrored into ifa as
`fauto_init_unbound`), turns the analysis on by itself, and makes
`write_c` declare every flagged local as `T x = {};` — value-init, the
one spelling that means "zero" for every representation the backend
emits, so nothing there needs to know which it got.

It works where the local survives to codegen:

    pyc --safe tests/scope_read_before_write.py    39 zero-inits
    pyc        tests/scope_read_before_write.py     0

**But codegen is too late for the simplest case there is**, and this is
the finding that matters:

```python
def f(c):
    if c:
        y = 42
    return y
print(f(1)); print(f(0))       # prints 42, 42
```

`f(0)` returns **42**, with or without `--safe`. There is no slot left
to initialize: FA sees the unassigned path contribute *bottom*, so the
union reaching `return y` is `{42}` — a single constant — and the whole
function constant-folds away. Both call sites become
`_CG_f_2087_0/*int64::__str__*/()` returning the literal 42.

This is the original "silent" symptom of this issue seen from the other
end. The unassigned path is not merely unreported, it is treated as
*unreachable*, so it cannot be repaired downstream of the analysis that
erased it.

So auto-initialize has to be an **analysis-level** change: the
unassigned path must contribute a real value, not nothing. Sketch —
`y_entry = zero_of(y_phi)`, a primitive whose transfer maps an AType to
the zero-constant CreationSet of each member. The self-reference is fine
in a fixed-point engine: `{42}` → `{0}` → `{0,42}` → stable. That also
answers "what type is the zero?" without a second FA run, which is what
made the codegen-level version look attractive in the first place.

The frontend still picks the fill (IFACallbacks); ifa's default stays
the zero of the inferred type.

### Where it goes, and why it is not a small change

The join is one place -- `add_pnode_constraints` (analysis/fa.cc):

```cpp
for (PNode *n : p->phi) {
  AVar *vv = make_AVar(n->lvals[0], es);
  for (Var *v : n->rvals) flow_vars(make_AVar(v, es), vv);
}
```

For an unbound operand the `flow_vars` becomes "contribute the zero of
`vv`'s own type". Two things make that more than an edit:

1. **It cannot be a snapshot.** Reading `vv->out` once and stuffing the
   zero in is the `update_gen` trap -- a snapshot can be lost on the
   final pass. It has to be a real transfer node that re-fires when
   `vv` changes, i.e. a primitive in the fixed-point engine, not a flow
   edge.
2. **Marking the operand needs a second dataflow.** `find_maybe_unbound`
   runs *before* `place_phi` (ssu.cc), so no phi exists when the facts
   are computed, and `get_Var`'s env-miss is not the signal -- measured
   wrong in both directions, see the pass comment. After phi placement
   the structure is actually better (a phi *is* a def, so unboundness
   concentrates exactly on the operand edges), so the marking pass wants
   to run there and ask, per phi and per predecessor, whether that
   pred's `assigned_out` carries the bit.

Both are tractable; neither is a one-liner, and (1) lands in the part of
FA this project has repeatedly found fragile (ifa/055, ifa/057,
PYC_SELFPROD, ifa/111). It is gated behind `--safe`, so the blast radius
is contained.

The runtime check for `possibly` is blocked on the SAME erasure: if FA
folds the function to a constant there is no read left to check, so the
check would be optimized away exactly where it was needed.

`unbound_read_handler()` is the hook for the other half — it returns the
name of a `void f(const char *)` runtime function for the backends to
call on a failed check, so nothing in ifa names `UnboundLocalError`.
Nothing calls it yet: the runtime check needs per-read instrumentation
(a shadow flag per flagged Sym, set at each write, tested at each read)
in both backends, and that is not written.

### Status

- analysis, both facts, both severities — **done**
- `--safe` plumbing + codegen zero-init — **done, insufficient alone**
- analysis-level fill so the unassigned path survives FA — **not done**
- runtime check for `possibly` — **not done** (hook exists, no callers)
- LLVM backend for any of the above — **not done**

## The fill works (2026-08-25) — as an IF1 rewrite, not a transfer node

`--safe` now produces the right answer on the case the last section
said it could not:

```python
def g(c):
    if c: x = 2.5
    return x
print(g(1)); print(g(0))
```

    default   2.5 2.5     (unchanged)
    --safe    2.5 0.0     C backend AND LLVM backend

int, float, and a string case that correctly declines:

    safe1 (int)     42 0
    safe3 (float)   2.5 0.0
    safe2 (str)     hi hi     -- no language-neutral zero, so ifa
                                 declines rather than inventing one

### How, and why not the transfer node

The sketch above proposed a `zero_of` primitive in the fixed-point
engine. It is not needed, and the thing that replaced it is smaller and
lands in a far less dangerous place.

**`mark_unbound_phi_operands`** (optimize/ssu.cc) runs after
`rename_vars` and replaces each "control got here without ever assigning
it" phi operand with a *fresh* Var flagged `is_unbound_fill`. It is a
second MUST dataflow, keyed on Sym, in which a phi lval counts as a def
(that is what concentrates unboundness onto the operand edges) and a phy
lval does not (a phy renames a value without giving it one). A fresh Var
per operand rather than a flag on the shared one, because `get_Var`'s
env-miss hands the SAME original Var to several operand slots.

**`apply_unbound_fills`** (analysis/fa.cc) then runs BETWEEN passes, in
the `extend_analysis() || reanalyze() || ...` chain, and rewrites those
operands to a typed zero constant. Between-pass IF1 rewriting is sound
because `analyze_to_convergence` clears every Var, contour and edge
before EVERY pass (ifa/issues/098), so the next pass re-reads the
rewritten code. That is also precisely why this is neither a flow edge
nor an `update_gen`: a snapshot taken during one pass can be lost on the
final one, whereas rewritten IF1 is re-read every pass by construction.

The bootstrap is the point of doing it between passes: pass 1 gives the
marked operand nothing, so the merge carries only the assigning paths'
type — which is exactly the type the zero must have. Pass 2 sees
`{0, 42}` and no longer folds. From then on the operand is an ordinary
constant, `apply_unbound_fills` reports no change, and the loop settles.

Both backends get it for free because it is an IF1 rewrite, not a
codegen feature. `cg.cc`'s `T x = {};` stays as a safety net for the
locals the fill declines.

### Two bugs worth remembering

**A constant created between passes is invisible unless registered.**
Constants get their value through `fa_Vars` -> `add_var_constraint`, and
`collect_Vars_PNodes` builds that list ONCE per Fun (`fa_collected`).
A constant introduced later contributes bottom, the merge stays a single
value, and the function folds exactly as before — silently. It is also
*type-dependent*, which is what made it confusing: `0` is usually
already in the program from somewhere else, so int cases worked while
float ones did not. The fix registers the Var in `fa_all_Vars` (re-sorted
by id) and `fa_Vars`.

**A set-mode `Vec` yields its empty slots as nulls.** `formals` is built
with `set_add`, and iterating it hands back nulls once it grows into
hash-table mode. `find_maybe_unbound`'s equivalent loop never showed the
fault because it keys on the Var and `Map::get` tolerates a null key;
this pass keys on `v->sym` and segfaulted on 3 tests.

### Status

- analysis, both facts, both severities — **done**
- `--safe`: IF1-level typed-zero fill, both backends — **done**
  (numeric types; other types decline, awaiting the frontend hook)
- runtime check for `possibly` — **not done** (hook exists, no callers)

Full suite under `--safe`: 288 passed, 8 failed, and all 8 are the
COMPILE-OUT warning diffs of the section above — no behaviour failures.
Five CI gates green with defaults.
