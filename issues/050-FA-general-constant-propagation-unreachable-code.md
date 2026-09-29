# 050 — a global slot's value is not call-graph precise (interprocedural global propagation)

**Status:** open on stages 2-3. Rewritten 2026-09-28. The title used to
be "no general constant-propagation fixed point". That framing is
retired: FA already does conditional propagation with cascading folds,
and the SCCP half moved to [119](119-sccp-as-an-outer-fixed-point-over-fa.md).
The full record is in git:
`git show 3f36072b:ifa/issues/050-FA-general-constant-propagation-unreachable-code.md`.

## What remains

`tests/global_slot_call_graph_precision.py` (`.known_issue`):

```python
g = 0
def setup():
    global g
    g = "five"
def use():
    return len(g)
setup()
print(use())        # CPython: 4;  pyc: program does not type
```

A module-data cell lives in the single shared `GLOBAL_CONTOUR` AVar, so a
read gets the flow-insensitive union of every write, `{int64, str}`. The
call graph proves `use()` sees only `str`. This is NOT closed/018: no
valid program state holds both, so no representation is missing. It is
interprocedural mem2reg for a scalar slot.

## Landed: stage 1 (2026-08-29), the intra-function case

`g = 0; g = "five"; print(len(g))` now prints 4
(`tests/global_slot_module_level_order.py`). Two pieces:

- **Loads.** `IFACallbacks::provably_constant_load`, consulted from the
  `Code_MOVE` transfer, folds a global load to the nearest DOMINATING store
  iff every store to that cell program-wide is in the same Fun and
  dominates the load. The store set is built from `if1->allclosures`
  (static, complete before `analyze()`), NOT from `fa_move_PNodes` or
  `fa->funs`, which fill in during analysis. **A transfer-function fold
  must be computed from a fact that cannot sharpen across passes**,
  because `update_gen` unions, so an early wrong answer is permanent
  (Wall 1).
- **The slot.** The BOXING check skips a `GLOBAL_CONTOUR` AVar that
  nothing reads (`AVar::forward` empty). A fully-folded cell is
  unobservable, so its store union cannot be wrong (Wall 2). This is NOT a
  liveness test: `Var::live` is set by DCE, after FA.

(closed/172 then taught ifa/146's backflow walk to cross a folded global
load, because the fold is a snapshot with no backward edge.)

## Open

2. **A per-Fun mod-set over module-data cells**, transitive over the call
   graph. It has the same shape as `compute_fun_can_raise()` (post-clone,
   over `Fun::calls`), generalised from one bit to a small set. It bounds
   where a fold may look: a call whose mod-set contains the cell ends the
   chain.
3. **The interprocedural summary**: per-ES global in/out threaded through
   `AEdge`, an annotation that never participates in ES equivalence.
   Stages 1-2 are its intra-procedural transfer function, so it cannot be
   built first.

**Constraint on both:** whatever facts stages 2-3 feed into a transfer
function must be stable across passes (Wall 1), or be applied as IF1
rewrites between passes (the way `apply_unbound_fills` does in
[039](039-FA-uninitialized-local-reads-silent.md)), which the next pass
re-reads from scratch.

## Verification

`tests/global_slot_call_graph_precision.py` flips KNOWN → PASS. The
stage-1 fixture still passes. Corpus `check` neutral or better.
