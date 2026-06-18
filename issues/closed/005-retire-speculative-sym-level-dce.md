# Issue 005: retire speculative Sym-level DCE

**Status:** closed. All six steps landed. Pyc's `asymbol` blanket-
setting kept (audit revealed structural dependency in `scope_sym`);
that's a separate cleanup tracked outside this issue.
**Affects:** `ifa/if1/if1.cc` (`if1_simple_dead_code_elimination`),
`ifa/if1/prim_data.cc` (nonfunctional flags), pyc frontend
(`python_ifa_sym.cc`'s `new_PycSymbol`), ifa test harness
(`ifa/testing/fa_setup.cc`'s keepalive).
**Related:** [001-keepalive-vs-explicit-reply.md](001-keepalive-vs-explicit-reply.md)
(closed by 005 step 2b — keepalive removed, crash mode gone by
construction).
**Commits landed:** `74139ab` (flag), `39e3eb2` (drop `PRIM_NON_FUNCTIONAL`
from `prim_period`), `afa5750` (clarify `prim_destruct`'s flag),
`4a7cefe` (step 1: flip default), `f2fc2d2` (step 2b: remove
keepalive), `d480b79` (step 3: delete gated branches + flag),
`e6b8004` (step 4: drop `prim_destruct`'s flag), `faf6e76`
(step 5: drop asymbol clause from seed loop).

## Symptom

`if1_simple_dead_code_elimination` (the IF1-level DCE, called from
`if1_finalize_dce` before FA runs) does two speculative kills in
`mark_dead`:

```c
case Code_MOVE:
  if (fdce_if1_speculative && !code->lvals[0]->live) code->live = 0;
  break;
case Code_SEND:
  if (fdce_if1_speculative && is_functional(p, code) && !code->lvals[0]->live)
    code->live = 0;
  break;
```

These rely on the seed loop at `if1.cc:507` —
`if (!s->nesting_depth || s->asymbol) mark_sym_live(s);` — to catch
every observable Sym. The seed loop is conservative for true globals
(`!nesting_depth`) and for frontend-tagged Syms (`asymbol != null`).
Anything else has to flow live-ness through `mark_live`'s SEND
propagation gate, which fires only for `!is_functional` (closure calls
with `code->prim == null`).

The fundamental problem: Sym-level DCE has no semantic root set. It
defers to whatever the frontend marks as observable, and each frontend
invents its own escape hatch:

| Frontend | Escape hatch |
|----------|--------------|
| **pyc** | `new_PycSymbol` unconditionally sets `s->sym->asymbol = s`. Every pyc Sym becomes a live root. |
| **V** | `BasicSym` ctor defaults `nesting_depth=0`; `finalize_symbols` (`ast_to_if1.cc:1761`) explicitly *resets* unowned Syms back to 0. Every unowned Sym becomes a "global." |
| **ifa-test** | Keepalive primitive in `fa_setup.cc` registers `__test_keepalive` with `is_visible=1`, emits a SEND that consumes the user entry's return. `kp_result` must stay global so Sym-level DCE keeps the keepalive Code alive in the first place. |

All three work around the same root cause. The FA-level
`mark_live_code` (`ifa/optimize/dead.cc`) uses `is_visible` /
`nonfunctional` primitive metadata as its root set — that's the
authoritative signal. The Sym-level pass is a coarse, frontend-
dependent approximation of work FA-level does precisely.

## Root cause

The Sym-level pass exists for two valid jobs and one speculative one:

1. **Structural pruning** (valid). Unreachable Code (after a GOTO,
   labels with no incoming edges) gets `live=0` via `mark_code_live`
   and the `Code_LABEL` branch of `mark_dead`. Pure control-flow
   analysis, no frontend cooperation needed.
2. **Sym::live bookkeeping** (valid). Downstream consumers
   (`if1_flatten_code` at `if1.cc:540`, others) read `Sym::live` to
   decide what to emit. The pass populates this.
3. **Speculative SEND/MOVE kills** (invalid). Tries to eliminate
   syntactically-dead-looking SENDs/MOVEs. Requires a frontend-supplied
   "what's observable" signal that doesn't exist as a first-class
   concept.

Job 3 is what creates the need for the three escape hatches. It's
duplicating work that FA-level does correctly — and doing it worse,
because FA-level has type/flow information and a real root set
(`is_visible` primitives, nonfunctional prims with trackable targets).

## What's landed (this issue's commits)

### `74139ab` — gate the speculative kills

Added `bool fdce_if1_speculative` (default `true` → no behavior change)
gating both speculative branches. Empirically validated: pyc 73/73 +
2 expected fails, ifa-test 50+/14 phases green at both `true` and
`false`. The default is the only thing keeping current behavior; the
implementation already supports running without the speculative kills.

### `39e3eb2` — drop `PRIM_NON_FUNCTIONAL` from `prim_period`

`prim_period` was defensively flagged nonfunctional to cover
"`obj.attr` might trigger `__getattr__`." But IFA's IR lowers
computed-attribute semantics to explicit method calls — raw
`prim_period` is always a pure read or an unobservable closure-field
write during closure-create. The flag was making Sym-level keep
closure-create SENDs alive that FA-level (correctly) wanted to kill,
which would break codegen if the default flipped without this fix.

### `afa5750` — explain `prim_destruct`'s flag

Documented that `prim_destruct`'s `PRIM_NON_FUNCTIONAL` is *not* a
side-effect marker — it's a workaround for Sym-level `mark_dead`
checking only `lvals[0]->live`. `prim_destruct` is tuple unpacking
(one SEND, multiple lvals); without the flag, a multi-lval SEND
where `lvals[0]` is dead but `lvals[1+]` is live would be
incorrectly killed. FA-level iterates all lvals correctly and doesn't
need the workaround. Flag can be dropped once `fdce_if1_speculative`
defaults to false.

This also retracted an earlier framing (in `74139ab`'s commit message
and in issue 001) that called `prim_destruct` an "intrinsic untracked
side effect." It isn't — and *no* prim in IFA's set actually has
intrinsic side effects. Every `PRIM_NON_FUNCTIONAL` prim either
tracks a target (reply/setter/set_index_object) or works around the
Sym-level mark_dead limitation.

## What's left

### Step 1 — flip `fdce_if1_speculative` default to `false`

One-line change in `ifa/if1/if1.cc:23`. Already empirically validated.
Keep the flag for one release cycle for rollback.

Verification: pyc 73/73 + 2 expected fails, `make test-ir` all phases
green. (Both already confirmed at flag=false in #72 / `74139ab`.)

### Step 2 — remove the escape hatches (one commit each)

Each removable independently because none of them was actually
load-bearing for correctness once the speculative kills are off.

a) **pyc `asymbol` blanket-setting.** ~~Remove `s->sym->asymbol = s;`
from `python_ifa_sym.cc:7` (and the matching line in `:21`
`PycSymbol::copy`).~~ **Skipped — audit revealed structural
dependency.** `scope_sym` (`python_ifa_build_syms.cc:57-60`) casts
`sym->asymbol` to `PycSymbol*` to populate the scope map. The
blanket setting is load-bearing for pyc's scope resolution, not
just for DCE. Removing it would require changing the
`scope_stack`'s value type from `PycSymbol*` to `Sym*` (or
introducing a separate Sym→PycSymbol lookup map) — out of scope
for this issue. The asymbol field stays blanket-set as a
frontend-internal convention. Step 5 (below) handles the DCE
side independently.

b) **ifa-test keepalive.** Remove the `__test_keepalive` registration
and SEND emission in `ifa/testing/fa_setup.cc:75-102`. Without
speculative DCE, the keepalive's purpose disappears — FA-level
`mark_live_code` is the live anchor, and user code only needs to be
syntactically reachable from `sym___main__` (the splice already
arranges that). Re-bless `ifa/tests/ir/dce/01_baseline.ir.dce.expected`
and `02_unused_fun.ir.dce.expected` if their per-fun live counts
change (they probably will — without the keepalive, dead-stripping
gets more aggressive).

c) **V `finalize_symbols` nesting_depth=0 reset.** Out of scope per
"V isn't a priority and will be replaced." Leave as-is.

### Step 3 — drop the now-redundant gated branches

After step 2 has baked: delete the two `if (fdce_if1_speculative
&& ...)` branches in `if1.cc:mark_dead`'s `Code_MOVE` and
`Code_SEND` cases. Delete the `fdce_if1_speculative` declaration
itself.

### Step 4 — drop `PRIM_NON_FUNCTIONAL` from `prim_destruct`

Now that the Sym-level mark_dead workaround is gone, the flag is
no longer load-bearing. Remove from `prim_data.cc:360`.

### Step 5 — simplify the seed loop

`if1.cc:507` becomes `if (!s->nesting_depth) mark_sym_live(s);`
(drops the `|| s->asymbol` branch, since no frontend should be
relying on it for liveness anymore). Update the seed-loop comment.

### Step 6 — second look at `is_functional`

After steps 1-5, `is_functional()`'s only remaining caller is
`mark_live`'s SEND propagation gate (`if1.cc:467`). That's a different
use — it controls whether to propagate liveness forward from a live
result back to args/lvals. Still valid, still used. No change expected
but worth confirming.

## Verification plan

After each commit:
1. `make pyc && make test` → 73 passed, 2 expected fails, 0 failed.
2. `make test-ir` → all 14 phases / 50+ fixtures green.
3. For any `.expected` rebless, diff before/after to confirm the
   change is what we wanted (typically: per-fun live counts shrink as
   FA-level DCE more aggressively prunes without the escape-hatch
   over-conservation).

For step 2a specifically, also run with `IFA_HTML=...` or whatever
pyc's source-mapping mode is to confirm `asymbol`-mediated
`pathname()`/`line()` still works for the Syms that should still have
it (probably: ones created with an `ast` argument).

## What this unblocks

- **Issue 001 (keepalive-vs-explicit-reply crash) becomes trivial.**
  Once the keepalive is gone, `kp_result`'s global-vs-LOCALLY_NESTED
  dilemma vanishes with it. The crash mode (FA's `make_AVar` in
  `GLOBAL_CONTOUR` on a non-global rval) goes away because there's no
  global-contour SEND fan-in point for the keepalive. Inline-phase
  fixtures that explicitly reply (the ones currently blocked by 001)
  become writable.
- **Architectural clarity.** One DCE pass instead of two. FA-level is
  the single source of truth for "is this code observably dead." The
  Sym-level pass shrinks to structural pruning + `Sym::live`
  bookkeeping — both genuinely useful, neither requiring frontend
  cooperation.
- **Frontend simplification.** pyc's `new_PycSymbol` becomes simpler
  (no blanket `asymbol = this`). ifa-test's `fa_setup.cc` loses ~30
  lines of keepalive scaffolding. The "asymbol means source-loc, not
  liveness" invariant becomes enforceable.

## Open risks / why deferred

- **V coverage is thin** (one test). Out of scope per "V will be
  replaced" — accepting the risk.
- **Performance.** Without speculative Sym-level kills, FA processes
  marginally more PNodes before pruning them. Empirically not visible
  in the test suite (`make test` time unchanged at 18s). Worth a
  pyc-self-host or large-input check before the release; if it's a
  problem, the right answer is to make FA-level DCE pruning happen
  before the more expensive analysis stages, not to revive Sym-level
  speculation.
- **`asymbol` audit.** Step 2a's "verify nothing else reads asymbol
  expecting it always-set" is the only step with a non-trivial audit.
  Grep `asymbol` in pyc + ifa codegen to confirm before landing.
