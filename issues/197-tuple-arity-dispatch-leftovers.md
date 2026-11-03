# 197 — tuple arity dispatch: two leftovers from the sunfish work

**Status: open.** Filed 2026-11-03 from
[closed/185](closed/185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md)
items 3 and 4. The arity dispatch itself (per-arity method bodies,
`Sym::dispatch_arity`, the fixed-arity header bit, the layout plan) is
landed and sunfish matches CPython in 136 s.

## 1. A `no_static_arity` flip is not re-dispatched within its pass (defect)

The flip happens in `make_kind` (`fa.cc`), mid-pass, and changes no AType,
so every send already dispatched on the old arity keeps its target until
the next pass. On the final pass that leaves a stale dispatch, and the
fixed-arity header bit is decided from the converged CreationSets, so the
program aborts at run time with `arity dispatch: no branch matched`.
Nothing checks for it.

**Fix:** record each flip in a per-pass set; re-enqueue the sends whose
operands hold the flipped CreationSet (they are in `arg_of_send`), or at
minimum request another pass when the set is non-empty on a pass that
would otherwise be the last. Stop condition: a suite test that flips on
the final pass passes; corpus verdicts unchanged.

## 2. Same-arity tuples with different element types in one list (refusal)

`[(1, "a"), ("a", 1)]` does not compile. The per-arity body's single
contour holds both CreationSets, `self[0]` is `{int, str}`, and no stage
splits the contour by element type. The union is per-object, not
temporal, so a contour split is the right answer; the demand is the
BOXING violation on `self[0]`, and the handle is the receiver
CreationSet. Check first whether stage 5 (`split_edges`) declines because
`cs_slot_sig_equal` reads the slots as equal, which closed/134 added for
same-layout receivers; here the layouts differ.

## Verification

A fixture for each; `make test` green on both backends; corpus `check`
unchanged.
