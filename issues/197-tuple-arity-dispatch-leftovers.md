# 197 — tuple arity dispatch: two leftovers from the sunfish work

**Status: open; item 1 fixed 2026-11-06.** Filed 2026-11-03 from
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

**Fix (landed 2026-11-06):** `analyze_to_convergence` remembers
`dispatch_arity_epoch` at the start of each pass. A pass during which it
moved sets `arity_flipped`, which joins `loop_again`, so that pass is
never the last. The next pass starts from a full reset (`arity_flipped`
also disarms selective invalidation, so `IFA_SELECTIVE` cannot preserve the
stale contour), and every send dispatches with `no_static_arity` already
set; the match cache keys on the epoch (`pattern.cc`). `PYC_DBG_STAGEDELTA`'s
`PASSEND` line prints `arity=`. Re-enqueuing only the affected sends within
the pass would be cheaper, but there is no reverse index from a CreationSet
to the AVars that hold it, so it would scan every send.

**No fixture yet.** Compiled with `PYC_DBG_STAGEDELTA=1`, no suite program
has a pass kept alive by the flip alone (`PASSEND ... extend=0 reanalyze=0
fills=0 retract=0 arity=1`), so the suite does not reach the case this
fixes. Arity is part of a tuple's CreationSet identity, so the flip is the
safety net for merges that arrive by other routes (the `cs_map` memo,
split-parent inheritance, the mold; see closed/185's sunfish case), and a
fixture has to provoke one of those. Stop condition still to meet: such a
fixture passes, and corpus verdicts are unchanged (met: see the sweep below; the extra pass
never fired on the corpus, contour counts are identical).

## 2. Same-arity tuples with different element types in one list (refusal)

`[(1, "a"), ("a", 1)]` does not compile. The per-arity body's single
contour holds both CreationSets, `self[0]` is `{int, str}`, and no stage
splits the contour by element type. The union is per-object, not
temporal, so a contour split is the right answer; the demand is the
BOXING violation on `self[0]`, and the handle is the receiver
CreationSet. Check first whether stage 5 (`split_edges`) declines because
`cs_slot_sig_equal` reads the slots as equal, which closed/134 added for
same-layout receivers; here the layouts differ.

**Sweep, 2026-11-06** (A5, A1, 204 rows 1–2, 039 item 1, 197 item 1 together,
against `eb635d2a`): `check` mode identical per program in every column,
contour counts included (`sweeps/check__default__eb635d2a.tsv` vs
`check__default__eb635d2a+23d143af.tsv`); `-b` compile mode identical,
with no program refused by the LLVM layout check
(`compile__PYC_LLVM_1__eb635d2a.tsv` vs `…+4653f5e1.tsv`).

## Verification

A fixture for each; `make test` green on both backends; corpus `check`
unchanged.
