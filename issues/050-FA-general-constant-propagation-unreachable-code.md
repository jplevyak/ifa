# 050 — no general constant-propagation/dead-code fixed point; the current mechanism only consumes point facts fed in by ad-hoc detectors

**Status:** partial. Direction 3a **implemented 2026-07-18** (same
day, in a follow-up session after this issue was filed) — see its
own section below for the full design and verification record.
Directions 1, 2, and 3b remain open/not started. Originally written
right after landing issue 011's exception-check dead-code-elimination
work (see [011](../../issues/closed/011-exception-handling-unimplemented.md)'s
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
   [031](closed/031-globals-outside-fa-precision.md) explicitly deferred
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
     ([048](048-FA-deepcopy-flow-divergence-genetic2.md)'s `genetic2`
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


## 2026-08-29: measured what FA already does, and what is left

Asked the sharp version of this question — FA does constant propagation
*in the type lattice*, so what is still required? Measured rather than
reasoned:

**FA's in-lattice propagation is already sparse and CONDITIONAL, and it
cascades.**

```python
c = 0
if c: x = "str"
else: x = 5
print(x + 1)          # compiles, prints 6
```

The dead arm contributes NOTHING to `x` — if it did, `x` would be
`{int64, str}` and the program would be refused. Add a level and it still
holds:

```python
c = 0
if c: d = 1
else: d = 0
if d: x = "str"
else: x = 5
print(x + 1)          # compiles, prints 6
```

`c` folds, which folds `d`, which types `x`. That is the fold-then-refold
cascade this issue asks for, and FA's own iterative worklist already
delivers it *for facts FA can derive itself*. So the "no SCCP" framing
above is too strong: what is missing is not conditional propagation and
not iteration, it is (a) the layer above FA and (b) the facts FA cannot
derive.

**(a) The detector layer is still one hand-wired detector.** Still
exactly one — `exc_check_fold.cc`'s `mark_exc_checks_constant`, wired by
hand in `pyc.cc` after `compute_fun_can_raise()`. Nothing re-enters FA
after it folds, so a fold that would expose a new FA-derivable fact does
not get one. Directions 1 and 2 are about this layer only, and neither
has a second fact to justify it yet.

**(b) The substantive gap is 3b, and it now has a repro.**
`tests/global_slot_call_graph_precision.py`, 8 lines, `.known_issue`:

```python
g = 0
def setup():
    global g
    g = "five"
def use():
    return len(g)
setup()
print(use())
```

CPython prints `4`; pyc refuses with `program does not type`. `g` is
written twice and read once, and the call graph proves the read sees a
`str` — but the read gets the flow-insensitive union of every write,
`{int64, str}`. issues/031 gave each global READ its own contoured temp;
nothing gives the VALUE call-graph precision, which is exactly what 3b
describes.

Note this is NOT [018](../../issues/018-dict-mixed-key-types-boxing-failure.md):
no valid program state has `g` holding both, so no representation is
missing. It is interprocedural mem2reg for a scalar slot, as 3b says.

**So the answer to "what is still required" is 3b, and only 3b** — plus
directions 1/2 whenever a second foldable fact actually turns up. The
constant propagation and the fixed point are already there.


## Pursuing 3b: where it actually starts, measured

3b is written as one large interprocedural summary (per-ES global
in/out threaded through `AEdge`). Measuring first shows it decomposes,
and that the interprocedural half is NOT the first thing needed.

**The cell is not ordered even inside ONE function.**

```python
g = 0
g = "five"
print(len(g))        # CPython: 4;  pyc: fail: program does not type
```

No call graph is involved. The second write kills the first, and the
ordering is right there in the function's own PNode graph -- but a
module-data cell lives in the single shared `GLOBAL_CONTOUR` AVar
(fa.h's GLOBAL_CONTOUR note), so the read still gets `{int64, str}`.
`tests/global_slot_module_level_order.py` pins it; the interprocedural
version is `tests/global_slot_call_graph_precision.py`.

That reframes the work. issues/031 step 2 gave each global READ an
EntrySet-contoured, SSU-renamable temp and deliberately left the WRITES
sharing one cell ("the cell's shared GLOBAL_CONTOUR AVar keeps the sound
flow-insensitive union of all stores"). **The missing half is the writes.**

### Staged plan, smallest first

1. **Intra-function SSU for module-data cells.** Rename the cell inside a
   function exactly as a local, so a read after a rewrite sees only the
   last write. Reuses the SSU pass that already exists; needs no new
   dataflow. Fixes `global_slot_module_level_order.py`.

   Soundness bound: the chain must break at any call that could itself
   write the cell. So it needs --

2. **A per-Fun mod-set over module-data cells**, transitive over the call
   graph. Structurally identical to `compute_fun_can_raise()` (post-clone,
   over `Fun::calls`), which already computes exactly this shape for a
   single boolean; this generalizes it from one bit to a small set. A call
   whose mod-set contains the cell ends the current SSU chain and starts a
   fresh one at the shared cell value.

3. **Only then the interprocedural summary** 3b describes — per-ES
   global in/out threaded through `AEdge`, an annotation over the ES graph
   that never participates in ES equivalence. This is what
   `global_slot_call_graph_precision.py` needs, and stages 1-2 are its
   intra-procedural transfer function, so it cannot be built first anyway.

Stage 1 alone would already type the common Python idiom of declaring a
global with a placeholder (`g = None`, `count = 0`) and rewriting it with
the real value before any read -- a shape that today poisons the cell's
type for the whole program.


### CORRECTION: fold in the TRANSFER FUNCTION, not by renaming — and then reflow is free

The staging above says stage 1 is "SSU-rename module-data cells". That is
the wrong shape, and it is bigger than it needs to be. Two facts settle
it, both checked in the tree:

**1. Reflow is only a problem ABOVE FA.** "Fold, then re-run to catch
what the fold exposed" is the question the post-FA detector layer forces,
because `exc_check_fold` runs once in a fixed pipeline slot. Inside FA
there is nothing to arrange: its worklist already iterates, and folds
already cascade — measured above, `c` folds, which folds `d`, which types
`x`. So anything folded inside a transfer function gets reflow for free,
and the whole "iterate the detector" design problem disappears rather
than being solved.

**2. There is already a precedent for folding in a transfer function,
and it is 3a.** `IFACallbacks::provably_constant_isinstance(AVar
*operand_av, EntrySet *es, PNode *send_pnode)` (`ifa.h:128`) is consulted
by `fa.cc`'s `P_prim_isinstance` transfer function (`fa.cc:3117`) before
its own logic. Exactly the ingredients a global load needs.

**The hook already has what it needs.** A global read is
`if1_move(cell_sym → temp)` (issues/031 step 2), and FA's `Code_MOVE`
transfer (`fa.cc:3438`) runs with both `p` (the PNode) and `es` in hand:

```cpp
case Code_MOVE:
  for (int i = 0; i < p->rvals.n; i++) {
    AVar *lhs = make_AVar(p->lvals[i], es), *rhs = make_AVar(p->rvals.v[i], es);
    ...
```

**And dominators are available during FA**, which is the fact that makes
the walk possible without touching SSU: `build_ssu()` calls
`build_cfg_dominators(this)` (`ssu.cc:552`) from the `Fun` constructor
(`fun.cc:67`), long before `fa->analyze()`. The `build_cfg_dominators`
loop in `ifa.cc:57` is a REBUILD after cloning, not the first one.

So stage 1 becomes: **at a global-load MOVE, walk back over the PNode CFG
to the nearest dominating write of that cell; if one is found, use its
AVar's type instead of the shared cell's union.** No IR change, no SSU
change, no new pass, no reflow plumbing -- one callback consulted from
one existing transfer function, and FA's own fixed point does the rest.

Stage 2 is unchanged and is what bounds the walk: it must stop at any
call whose transitive mod-set contains the cell. Stage 3 (the
interprocedural per-ES summary) is unchanged, and stages 1-2 remain its
intra-procedural transfer function.


### Prototyped stage 1 as a transfer-function fold. TWO walls, both measured.

Built it: `IFACallbacks::provably_constant_load(AVar *src_av, EntrySet
*es, PNode *move_pnode)`, consulted from `fa.cc`'s `Code_MOVE` transfer,
with a pyc-side implementation that folds a global load to the nearest
dominating store iff **every** store to that cell program-wide is in the
same Fun and dominates the load. It compiles, the callback fires, and the
dominating store is found (`[gload] g best=...`). The program still does
not type, for two independent reasons, and both matter more than the
mechanism does.

**Wall 1: FA's lattice is MONOTONE, so an early answer is permanent.**
`update_gen(lhs, forced)` — what 3a uses, and the only application
available in a transfer function — *unions* into the destination. The set
of stores this walk can see grows as analysis proceeds
(`Fun::fa_move_PNodes` fills in as EntrySets are analysed), so an early
pass folds to `int64`, a later one to `str`, and the destination ends up
holding both. The fold poisons exactly what it was meant to sharpen.

3a does not hit this because `can_raise` is a fact that is *stable* once
the ES graph exists. **Any transfer-function fold must be computed from a
fact that cannot sharpen across passes**, and "the stores I have seen so
far" is not one. That is a real constraint on the whole
fold-in-the-transfer-function idea, not an implementation slip.

**Wall 2: folding loads does not fix the SLOT.** Even with every load
folded, the diagnostics include:

    error: 'g' has mixed basic types:( int64 str )

The cell is a real storage slot in the emitted program, and it genuinely
holds an `int64` at one point and a `str` at another. Folding *reads*
cannot change that; the slot has to be SPLIT (or every read folded and
the slot then proven dead, which nothing currently does). So the SSU
renaming this section talked itself out of is necessary after all — the
transfer-function fold is the *loads* half, and splitting the slot is the
*stores* half, and the program needs both.

### Corrected staging

1. **Split the slot** — SSU-rename module-data cells within a function so
   `g = 0; g = "five"` become two distinct slots. This is what makes the
   program representable at all, and it makes the load resolution fall
   out of SSU rather than needing a walk.
2. **A per-Fun mod-set over module-data cells** (unchanged) — bounds
   where a renaming chain must break.
3. **The interprocedural per-ES summary** (unchanged).

A transfer-function fold remains attractive for facts that are stable
across passes, and the extension point is a clean two-line addition when
one turns up — but it is not the road to 3b. The prototype is preserved
at `wip_gload_callback.cc` in the session scratch.


### The dead-code aspect: it cannot help TODAY, but it points at a cheaper Wall 2

Wall 2 above ends with "or every read folded and the slot then proven
dead, which nothing currently does". Checking why:

**The BOXING violation is raised inside FA, and liveness does not exist
yet.** `fa.cc`'s own comment on the sibling check says it outright:

    // No `av->var->live` test: Var::live is set by dead-code
    // elimination, which runs AFTER this -- it is 0 for everything
    // here, so requiring it silently suppressed every report.

and BOXING is `always_fatal` (`fa.cc:4313`), so the compile is already
refused before `mark_live_code` or `reclaim_dead_producer_chain` ever
run. A write-only global cell is dead by any reasonable definition, and
no amount of DCE reaches it in time. That is an ORDERING fact, not a
fundamental one.

**But it suggests a cheaper Wall 2 than splitting the slot.** FA does
know, during analysis, whether anything reads an AVar — `AVar::forward`
is its consumer set. If the BOXING check skipped a cell whose AVar has no
consumers, then folding every read (which skips the
`flow_vars_assign(rhs, lhs)` edge, so the cell gains no consumer) would
leave nothing to diagnose. No slot splitting, no SSU change, no
dependence on DCE ordering.

That reshapes the plan again:

| obstacle | with slot splitting | with a consumer-aware BOXING check |
|---|---|---|
| Wall 2 (the slot's own type) | split the cell into two Vars | skip the check when nothing reads the cell |
| Wall 1 (monotone fold) | still applies | still applies |

So Wall 1 — a transfer-function fold must use a fact that cannot sharpen
across passes — becomes the ONLY remaining obstacle, and it is the more
tractable of the two: compute the cell's store set ONCE, after FA's
EntrySet graph has stabilised, rather than incrementally as
`Fun::fa_move_PNodes` fills in. `compute_es_can_raise()` already
establishes that pattern for 3a's fact.

Not implemented; recorded because it is a materially smaller stage 1 than
the SSU renaming, and because the reason DCE cannot be used is a specific
ordering constraint worth knowing rather than a dead end.


## Stage 1 IMPLEMENTED (2026-08-29)

Both halves, exactly as the two walls above required.

**Loads.** `IFACallbacks::provably_constant_load(AVar *src_av, EntrySet
*es, PNode *move_pnode)` (`ifa.h`), consulted from `fa.cc`'s `Code_MOVE`
transfer. pyc's implementation folds a global load to the nearest
DOMINATING store iff **every** store to that cell program-wide is in the
same Fun and dominates the load -- so no other store can reach, and in
particular no call can write the cell, because a call that did would BE a
store in another Fun and would fail the test.

**Wall 1 (monotonicity) is handled by where the store set comes from.**
It is built from `if1->allclosures` -- the static closure list, complete
before `analyze()` is entered -- and NOT from `Fun::fa_move_PNodes` or
`fa->funs`, both of which fill in as analysis proceeds. Measured with the
lazy sources: `fa->funs` is EMPTY on the first call, the un-folded path
runs on pass 0, and `update_gen()` unions the whole-program type into the
load temp permanently. The static list gives the same answer on every
pass, which is what the contract requires.

**Wall 2 (the slot's own type) is handled by the consumer-aware BOXING
check.** `fa.cc`'s BOXING collection now skips an AVar in `GLOBAL_CONTOUR`
that nothing reads. Folding the load skips the `flow_vars_assign` edge,
so a fully-folded cell has no consumers and is unobservable; its store
union cannot be wrong. This is deliberately NOT a liveness test --
`Var::live` is set by dead-code elimination, which runs after this -- but
FA already knows the consumer set during analysis, and that is the
property that matters. Only global cells: a local with no consumers is a
different situation and still worth reporting.

**Results.** `tests/global_slot_module_level_order.py` flips from
`.known_issue` to PASS -- `g = 0; g = "five"; print(len(g))` prints 4.
Two goldens moved, both verified as improvements rather than lost
coverage:

- `tests/listcomp_element_separation.py.check` becomes EMPTY. Its
  `illegal call argument type 'a' illegal: B` warning is gone because `h`
  is a module-level cell, and resolving its load keeps `aas`' element type
  at `A`. The contour-naming limitation that test documents is unchanged;
  the program no longer reaches it. An unlooked-for second win.
- `ifa/tests/synthetic/vector_polymorphic_writes_2.synth.fa-converge.expected`
  goes `violations=8→8` to `violations=7→7`: one unobservable global's
  BOXING violation is no longer reported. Nothing else in the fixture
  moved.

Five CI gates green; suite 306 passed / 0 failed / 15 known on both
backends.

`tests/global_slot_call_graph_precision.py` stays a known issue by
construction -- its store is in another function -- and is what stages 2
and 3 are for.
