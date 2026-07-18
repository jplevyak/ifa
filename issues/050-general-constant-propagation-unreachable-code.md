# 050 — no general constant-propagation/dead-code fixed point; the current mechanism only consumes point facts fed in by ad-hoc detectors

**Status:** open, not started. Scoping/motivation only, written
2026-07-18 right after landing issue 011's exception-check
dead-code-elimination work (see
[011](../../issues/011-exception-handling-unimplemented.md)'s
"Exception-check dead-code elimination" and "Closing the residual"
sections, and `ifa/optimize/dead.{h,cc}`'s `mark_var_constant`/
`reclaim_dead_producer_chain`).

## What exists today

Both backends' `Code_IF` termination code (`cg.cc`'s
`write_c_pnode`, `cg_emit_llvm.cc`'s `emit_block_terminator` /
`discover_blocks` / `emit_pnode`, the last three now sharing a
`const_if_successor` helper) already special-case a branch condition
Var whose `->sym` is *exactly* FA's own canonical
`type_world.true_type`/`false_type` constant Sym: only the live arm
is emitted, and (as of the work above) nothing dead is left behind —
no branch, no orphaned LLVM block, no stale-liveness residue.

Two ways a Var's `->sym` ends up being that canonical constant:

1. **FA's own constant folding**, for ordinary user code FA can
   resolve on its own — e.g. an `isinstance()` call whose checked
   value's `AVar->out` is a single `CreationSet` in every reaching
   contour. This happens naturally, inside FA's normal fixed point,
   with no extra plumbing.
2. **A point detector feeding a fact FA can't derive itself.**
   Today there is exactly one: `ifa/optimize/exc_check_fold.cc`'s
   `mark_exc_checks_constant`, which feeds `Fun::can_raise` (a
   call-graph-precise fact, computed via `Fun::calls` post-clone) —
   FA's own type inference is provably too imprecise to derive this
   on its own, because `__pyc_exc__` is one shared mutable global
   whose `AType` at any read site is the union of *every* raise
   anywhere in the whole program, not a call-graph-aware per-site
   fact. `mark_var_constant`/`reclaim_dead_producer_chain`
   (`ifa/optimize/dead.{h,cc}`) are the small, reusable primitives
   this detector uses, deliberately factored out so a *future*
   detector doesn't have to re-derive the same "how do I safely mark
   a Var constant and reclaim its now-dead producer chain" logic.

## What's missing

Path (2) above only works because a human identified one specific
fact (`Fun::can_raise`), wrote one specific detector for it
(`mark_exc_checks_constant`), and wired it into `pyc.cc`'s `compile()`
by hand, right after the analysis pass that computes the fact. There
is no general mechanism that:

- **Discovers** unreachability/constant conditions from call-graph or
  cross-function facts on its own — every new fact needs its own
  bespoke detector, hand-written and hand-wired.
- **Iterates to a fixed point.** Point detectors run once, at a fixed
  point in the pipeline (`compute_fun_can_raise()` → `exc_check_fold`
  → codegen). If folding one condition could, in principle, expose a
  SECOND condition as now-provably-constant (e.g. a nested check whose
  own condition depends on a Var only reachable through the branch the
  first fold just proved dead), nothing re-runs to catch it. This is
  the textbook shape of **sparse conditional constant propagation
  (SCCP)** — the standard compiler algorithm that alternates constant
  propagation and dead-code elimination until neither makes further
  progress — and pyc doesn't have one. What it has is closer to a
  handful of independent point-fixes: FA's own single-pass constant
  folding, plus now one hand-written detector, rather than a unified,
  iterative mechanism.
- **Generalizes past `Code_IF` conditions.** The mechanism only
  triggers dead-code elimination for a branch's condition specifically.
  A constant fact about some OTHER Var (e.g. "this call always returns
  literal 5") that doesn't happen to feed a `Code_IF` condition gets no
  benefit from any of this machinery at all today.

## Why this matters

Anyone who finds a NEW call-graph-precise or cross-function fact worth
folding on (the way `Fun::can_raise` was for issue 011) currently has
to: identify the fact, write a from-scratch detector pass mirroring
`exc_check_fold.cc`'s shape, and manually wire it into `pyc.cc` at the
right pipeline point relative to whatever analysis produces the fact.
That's a real, repeatable amount of work per fact, and it's easy to
get pipeline ordering wrong (the fact-producing analysis must be fully
converged before the detector runs, and the detector must run before
codegen). A general mechanism would let any such fact plug into ONE
place instead.

No concrete second fact/detector has been identified yet as of this
writing — this issue is about the missing GENERAL capability, not a
specific known gap being caught by a specific known fact today.

## Proposed directions (none committed to; ordered roughly by size)

1. **Keep doing point detectors, but formalize the pattern.** Document
   `exc_check_fold.cc` as the reference shape (detect → `call_info`/
   equivalent → `mark_var_constant` → `reclaim_dead_producer_chain`)
   so the NEXT fact's detector is a known, small template rather than
   something written from scratch. Lowest effort, no new
   infrastructure, but doesn't solve the fixed-point or
   past-`Code_IF` generalization gaps.
2. **A general fixed-point sweep** that runs after all currently-known
   fact-producers (today: FA itself, `compute_fun_can_raise`) have
   converged: repeatedly (a) walk live `Code_IF` conditions for any
   newly-constant Var (from FA's own resolution or any detector's
   feed), (b) fold + reclaim via the existing primitives, (c) re-check
   whether reclaiming exposed anything new, until no change. Turns the
   current "run once, in a fixed slot" pattern into a real SCCP-style
   loop. Detectors still have to identify NEW facts, but no longer
   have to reason about ordering/cascading themselves.
3. **Deeper FA integration.** Give FA's own transfer functions
   call-graph awareness (so a fact like `can_raise` could be derived
   INSIDE FA's normal fixed point, not via a separate post-hoc pass
   reading `Fun::calls` after the fact) — would close the root cause
   of why FA's own constant folding is insufficient for
   whole-program-shared state like `__pyc_exc__`, rather than working
   around it per-fact. Biggest, riskiest, most likely to interact with
   FA's existing precision/performance tradeoffs in ways that need
   careful study first (see `ifa/issues/032-fa-survey-findings.md` and
   related FA-precision issues for the general shape of that kind of
   work in this codebase).

## Verification plan (once a direction is chosen)

- A synthetic repro with TWO chained conditions, where folding the
  first is required to make the second provably foldable, to
  distinguish "point fix" from "real fixed point."
- Generated-code diffing (the `git worktree` A/B technique used
  throughout issue 011's landing) against the current point-detector
  behavior, to confirm no regression for the one fact that already
  works.
- `ifa-test`'s existing phase framework (`--phase fa-converge`,
  `--phase inline`, etc.) is the natural home for golden-fixture
  coverage of whatever gets built, following precedent.

## What this unblocks

Nothing is currently blocked on this — issue 011's `Fun::can_raise`
detector works today via the point-fix pattern. This issue exists so
the NEXT such fact doesn't require re-deriving "how do I plug a
call-graph fact into dead-code elimination" from first principles, and
so a genuinely cascading case (fact A's fold exposes fact B) doesn't
silently under-optimize without anyone noticing, since nothing today
would report that as a bug — it would just look like a missed, unnoticed
optimization.
