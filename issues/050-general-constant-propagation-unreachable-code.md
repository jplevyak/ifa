# 050 — no general constant-propagation/dead-code fixed point; the current mechanism only consumes point facts fed in by ad-hoc detectors

**Status:** partial. Direction 3a **implemented 2026-07-18** (same
day, in a follow-up session after this issue was filed) — see its
own section below for the full design and verification record.
Directions 1, 2, and 3b remain open/not started. Originally written
right after landing issue 011's exception-check dead-code-elimination
work (see [011](../../issues/011-exception-handling-unimplemented.md)'s
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
3. **Deeper FA integration.** Splits into two sub-options of very
   different size, scoped 2026-07-18 (same day, in a follow-up
   discussion after this issue was first filed):

   ### 3a — a native `can_raise` fact inside FA's own fixed point (bounded, concretely buildable)

   FA already builds a finer-grained, *during-analysis* call graph —
   `AEdge` (`from`/`to`/`pnode`/`fun`, `fa.h`) — that it uses for its
   own interprocedural argument/return propagation, and this
   converges *before* `clone()` runs (`ifa.cc`: `fa->analyze()` at
   line 47, `clone(fa)` at line 56). `Fun::calls`, what today's
   `compute_fun_can_raise` reads, is a *post*-convergence
   materialization built by `clone()` — not available during FA's own
   fixed point, which is why `can_raise` is computed after the fact
   today rather than natively.

   3a: propagate a boolean (seeded the same way `Sym::direct_raise` is
   today) through `AEdge`s during FA's *own* worklist, converging
   alongside type propagation instead of after it in a separate pass.
   Teach the `isinstance(t, nil_type)` transfer function to consult it
   for the `__pyc_exc__`-sourced pattern specifically and fold to
   `true_type` inside FA itself.

   **Status: implemented 2026-07-18, same day.** Landed as a properly
   layered feature, not a `__pyc_exc__`-name hardcode in shared code:
   a new generic `IFACallbacks::provably_constant_isinstance(AVar
   *operand_av, EntrySet *es, PNode *send_pnode)` virtual (`ifa.h`,
   default `nullptr` = "no opinion, use normal logic"), consulted by
   `fa.cc`'s `P_prim_isinstance` transfer function before its own
   CreationSet-intersection logic. `fa.cc` stays fully frontend-
   agnostic — it has no notion of `__pyc_exc__` anywhere; pyc's
   override (`PycCompiler::provably_constant_isinstance`,
   `python_ifa_sym.cc`) does the pattern-matching, using a new generic
   (non-pyc-specific) `EntrySet::can_raise` fact (`fa.h`) computed by
   `compute_es_can_raise()` (`fa.cc`, file-local) — a small,
   self-contained fixed point over `fa->ess`/`EntrySet::out_edges`,
   seeded from `Sym::direct_raise`, re-run fresh at the top of every
   FA pass (monotonic, so an under-approximation on an early pass
   self-corrects as the ES/`AEdge` graph grows — no separate
   "did anything change" signal needed; `extend_analysis()`'s own
   convergence criteria already keep the outer loop running until the
   graph stabilizes).

   One correction to the payoff claim above, found empirically during
   verification: disabling Tier 2 (`compute_fun_can_raise`/
   `mark_exc_checks_constant`) alone showed Tier 3a's fold *does*
   remove the check/branch on its own — confirming genuine native
   integration — but the `__pyc_exc__` slot-read `MOVE`'s dead
   residual came BACK. `mark_live_code` treats constness and liveness
   as deliberately orthogonal (a pre-existing, general design choice,
   not something this or the Tier-2 work introduced): a constant-
   folded `SEND`'s own inputs can still be marked live even though
   codegen separately elides the `SEND`'s emission via
   `virtual_cg_is_const_folded_send`. So **Tier 2 is NOT superseded**
   — both tiers now run, and both do genuinely non-redundant work:
   Tier 3a folds the check/branch during FA's own fixed point (works
   even for future non-pyc consumers of the same hook, and is
   philosophically the "right" layer for it); Tier 2's
   `reclaim_dead_producer_chain` still does the liveness cleanup Tier
   3a's native integration doesn't, by itself, provide.

   One real bug caught during implementation, worth remembering:
   `EntrySet::out_edges` (`Vec<AEdge*>`) can contain **null entries**
   — both `compute_es_can_raise()` and
   `provably_constant_isinstance`'s call-site lookup crashed (SIGSEGV
   on `print(1)`, the simplest possible program) until each `AEdge*`
   from that Vec was null-checked before dereferencing. Caught via
   printf-bisection (gdb was unreliable in this environment) after a
   full `make clean && make` ruled out a stale-build-artifact
   explanation first.

   Verified: full suites 203/0 × 2 backends, unit 58/0, IR 20/0
   (all 5 phases), the separate V-language frontend's own test suite
   (`ifa/tests/*.v`, `make test_llvm`) unaffected (confirms the
   default no-op hook is safe for a consumer that doesn't override
   it), `tests/exception_propagation.py` deterministic across 3
   compiles, shedskin corpus sweep unchanged (47/47 `rc=` results
   identical to pre-change baseline), and a generated-C A/B diff
   (`git worktree`) across every `tests/*.py` file that got as far as
   `import_module` alphabetically before timing out — every
   difference found was benign (pure `_CG_f_*` renumbering from
   discovery-order changes, or strictly MORE dead-code elimination /
   different but still-correct inlining decisions in the four
   exception-related files, confirmed by `test_pyc.py`'s own
   output-matching pass for all of them).

   ### 3b — general interprocedural slot promotion for global scalars (large, deferred)

   The full realization: not a boolean fact, but making *any* global
   Sym's read resolve to a call-graph-precise value, the way issue
   [031](031-globals-outside-fa-precision.md) explicitly deferred
   ("load CSE, a real dataflow optimization, not a contour question")
   when it landed per-read local temps for globals (Steps 1-2, 2026-07-04).
   Steps 1-2 gave each global *read* its own EntrySet-contoured,
   SSU-renamed temp — but the *value* that temp loads is still the
   flow-insensitive, whole-program union of every write to the cell,
   because nothing propagates per-contour reaching-write information
   into the cell itself.

   **Design.** Give each global Sym a per-EntrySet summary AType
   ("what could this global hold on entry to / after this ES"),
   computed by forward flow within an ES's own PNode graph (reusing
   FA's existing per-Var flow machinery) and threaded across ES
   boundaries by extending `AEdge` with an implicit global-in/-out
   pair, propagated through the same `in_edge_worklist` mechanism
   real arguments already use. **The one decision that bounds risk**:
   this summary must NOT become part of `EntrySet` equivalence/
   splitting the way real arguments legitimately do — it's an
   annotation over the existing ES graph, computed to a fixed point,
   never a reason to create a new clone (same shape as
   `compute_escape`'s lattice, which already runs over FA's ES/`AEdge`
   graph without touching clone equivalence). This caps the blowup:
   the number of ESes doesn't change, only the precision of what's
   known about a global read inside an existing one improves.

   In compiler terms this is closer to **interprocedural mem2reg /
   memory SSA for a scalar slot** than "scalar replacement" (SROA) —
   there's no aggregate decomposition. It's strictly about the global
   *slot's* own reference identity (which `CreationSet` it currently
   points to), which is orthogonal to and unaffected by FA's EXISTING
   per-field precision for whatever OBJECT that slot might point to
   (`CreationSet::var_map`/`unknown_vars`, the same machinery
   `promote_field` uses — issue 011's field-promotion work). Object
   field precision and slot-reference precision are two already-
   separate concerns in FA's model; 3b only touches the second.

   **3b subsumes 3a directly, not as a special case bolted on.**
   `can_raise` becomes nothing but "is `nil_type` the only
   `CreationSet` in `__pyc_exc__`'s converged per-ES summary at this
   read" — one query against the general mechanism. The *unmodified*
   `P_prim_isinstance` transfer function already produces
   `true_type`/`false_type` from exactly that input shape (that's how
   ordinary user `isinstance()` already folds), so no
   `__pyc_exc__`-specific code is needed at all once 3b lands.
   `exc_check_fold.cc`, `compute_fun_can_raise`,
   `Sym::direct_raise`/`Fun::can_raise`, and
   `mark_var_constant`/`reclaim_dead_producer_chain` all become
   deletable. Precision-wise nothing is lost either: because global
   summaries deliberately aren't a splitting axis, their precision
   ceiling is per-ES — exactly the granularity `Fun::can_raise`
   (a per-clone fact) already has today, so the risk-bounding design
   choice costs nothing relative to what 3a already delivers.

   **Effort**: large — this is a core-FA feature, not a pyc-local
   change. Rough pieces: new per-(global, ES) summary storage +
   intraprocedural forward propagation (moderate, reuses existing
   flow patterns); `AEdge` extension + interprocedural worklist
   integration converging *with* type inference, correctly handling
   recursive/cyclic call graphs (the largest single piece);
   `clone.cc` changes so concretization of a global-derived Var uses
   its own ES's summary instead of the shared `GLOBAL_CONTOUR` AVar
   (moderate, shouldn't need to touch equivalence logic given the
   non-splitting-axis design). `ssu.cc` and both codegen backends are
   likely untouched — the global Var itself stays flow-insensitive by
   design, and precision improvements should reach codegen for free
   through the already-proven `is_const_folded_send`/
   `const_if_successor` path. Given `exc_check_fold.cc` alone (a
   single-purpose pass) took a full session including careful A/B
   verification, and this touches FA's *own* fixed point, adds a new
   convergence dimension, and has blast radius across every ifa-based
   frontend (pyc *and* the V-language frontend — `ifa/tests/*.v`,
   only 3 files, a thin safety net for whatever this changes outside
   pyc) — expect roughly an order of magnitude more work, spread
   across multiple sessions.

   **Risk**: high, and one category is a correctness risk, not just
   a performance one.
   - *Soundness under cycles*: the fixed point must start pessimistic
     and converge upward, same as FA's type inference already does —
     get the backedge-merge logic wrong and the result is too
     *precise*, not just imprecise. Unlike today's conservative
     failure mode (check stays live, correct but unoptimized), a
     wrong "this global can only be X here" claim used to fold away a
     real check is a silent miscompilation.
   - *Performance*: an extra state dimension threaded through FA's
     already convergence-sensitive fixed point, on top of documented
     existing stall/performance issues
     ([048](048-deepcopy-flow-divergence-genetic2.md)'s `genetic2`
     divergence).
   - *Interacts with issue 031's existing scar tissue*: ~15 scattered
     `GLOBAL_CONTOUR` guards already exist to keep that sentinel safe;
     this touches the same territory.

   **Recommendation**: still hold off, now more confidently — 3a is
   implemented (see above) and delivers the one concrete, known-needed
   benefit at a fraction of the cost and risk 3b would carry; issue
   031 already deferred the general version once, for the same
   reason. Treat this write-up as a ready-to-pick-up plan for whenever
   a *second* independent global-precision need materializes, not a
   queued task. Note the subsumption claim needed one correction once
   3a actually landed: 3a alone did NOT make Tier 2's
   `reclaim_dead_producer_chain` unnecessary (see 3a's verification
   notes above — `mark_live_code`'s constness/liveness orthogonality
   is a separate, general property unrelated to WHERE the fold
   happens). Whether 3b would close *that* gap too, or would need its
   own separate liveness-reclaim step, is an open question for
   whoever picks this up.

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
