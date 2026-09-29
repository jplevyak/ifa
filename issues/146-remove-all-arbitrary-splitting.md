# 146 — remove every arbitrary split; only demand splitting

**Status: open. Umbrella / audit.** Rewritten 2026-09-28. The full
chronological record, 2026-09-08 → 09-25, is in git:
`git show 3f36072b:ifa/issues/146-remove-all-arbitrary-splitting.md`.

**Author's imperative:** *"no arbitrary splitting, only demand splitting."*
This issue owns the audit. It lists what has been removed, what is still in
the tree, and the rules that decide which is which. The positive plan (how
demand separates what start-merged identity merges) is
[129](129-plan-demand-driven-creation-set-splitting.md). When a split fires
is [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md).

## The test

A splitting mechanism is **arbitrary** when its partition size is a COUNT OF
THINGS (callers, creation points, receiver CreationSets) rather than the
number of distinct things the demand distinguishes. Ask:

1. **Would this split happen if the demand were absent?** If yes, it is
   arbitrary.
2. **Does the demand alone decide WHETHER, with the handle deciding only
   WHICH parts?** If yes, it is a mechanism, and allowed.

**A demand is something OBSERVING a distinction and being unable to
proceed**: a type violation, an irrepresentable union, a dispatch that cannot
resolve. *"This formal's type is a union"*, *"this CreationSet has several
creation points"* and *"these values came from different places"* are FACTS.
Reading a fact as a demand is how an arbitrary splitter passes the test.
That has happened twice here (`PYC_CPA`, and route 4's per-creation-point
fan).

**The diagnostic: an arbitrary lever is NON-MONOTONE.** More splitting
makes the results worse. Examples: `PYC_RECVFAN=2` left 1 warning on `bh`
and `=3` left 5; `PYC_CSPEEL2` added 9.5% contours and a compile failure.
Conversely, a lever whose removal COSTS contours and programs is earning
its keep.

**Delete, don't default off.** An off-by-default arbitrary lever gets
reached the moment a program resists, and it reads as sanctioned because it
is in the tree. And "is it off by default?" is the wrong audit question.
The mark splitters were found by auditing CALL SITES, not flags: one flag
had two consumers with different thresholds, and two splitters were never
gated at all.

## Removed

| mechanism | what it did | when |
| --- | --- | --- |
| `PYC_RECVFAN` | one method ES per receiver CreationSet, no demand test; modes ≥2 also lifted a stage gate | `ede0210f` |
| route 4's per-creation-point fan | every creation point got its own contour once any demand reached it; now partitions by assign-set signature (−37% container CSs) | `3e8dcb10` |
| `kCsDefSplitRipe` | the coarsest rung waited on a clock (3 passes), not on the finer rungs declining | `ede0210f` |
| `split_es_by_call_site`'s fan | one group per in-edge | `e4edc2c3` |
| `kCsDefSplitMax` and the caller caps | refused on a count, not on the partition the demand names | `20f76f27`, `e4edc2c3` |
| `PYC_CSSPLIT=1` | a split EntrySet minted a fresh CreationSet: a contour split because its surrounding contour split (−26% container CSs on removal) | 2026-09-08 |
| `PYC_CSDEFPART=2` fan fallback | fanned when a non-container CS had no flow graph; replaced by giving plain classes a content channel (`cs_content_avars`) | 2026-09-08 |
| `MARK_TYPE`, `split_ess_setters_marks`, `split_with_type_marks` | mark distance is depth-from-a-generating-AVar: provenance | 2026-09-08 |
| `CARTESIAN_PRODUCT` (`PYC_CPA`) | fanned every positional formal with 2..N CSs; zero references to any violation | 2026-09-08 |
| `PYC_CSPEEL2`, `PYC_CSFAN`, `PYC_CONFLEVEL`, `PYC_NOOPSPLIT`, `PYC_NUMSPLIT`, `PYC_SETTERGATE=3`, `PYC_ESDEMAND` | built, measured non-monotone or inert, deleted (see "Dead ends") | 2026-09-15..17 |

## Still in the tree

### D. `MARK_SETTER` / `MARK_SETTER_OF_SETTER` are LIVE — the "done" claim was false

A 2026-09-21 entry here said all four mark splitters and the mark
scaffolding were deleted, with `kNumFAPassStages = 8`. **That never
landed.** At `3f36072b`, `fa.cc:11268-11337` still runs
`build_joint_type_marks` → `collect_cs_marked_confluences` →
`compute_setters(..., AKIND_MARK)` → `split_for_setters`, and `AVar::mark_map`,
`MarkMap` and all 12 `FAPassStage` values are still in `fa.h`.

What DID land is its stated prerequisite. `voronoi2` failed without
MARK_SETTER because `sym_nil->var` picked up `{None, Site}` from container
slots and codegen stopped emitting `NULL` for it. That codegen fix is at
`cg.cc:3707` (`v->sym == sym_nil || v->type == sym_nil_type`).

**Next:** delete the stage and the mark scaffolding. Measure `voronoi2`
(compile_rc=1 today, for other reasons) and `plcfrs` (the only program
where it fired, and where it removed contours). Retire the
`mark_distance_skew` / `mark_setter_skew` `.known_issue` sidecars and
`cpa_mark_enabled` / `mark_why_enabled` with it. *Stop condition:* if a
program that works today regresses, find the demand the mark split was
standing in for; do not keep the mark stage.

### Stage 5's per-CreationSet fan in `split_edges`

`split_edges` (the `fdynamic` path) builds one filtered EntrySet per
CreationSet in the receiver's type, so its partition size is the CS count.
It survived because stage 5 is starved (157). `PYC_SPLITEDGES2=1` bounds it
to two groups and measured behaviour-neutral (148 Part 4). Since
2026-09-26 it also DECLINES when every receiver CS is identical by type
(`cs_slot_sig_equal`; see [134](134-remove-the-frontend-forced-split-opt-in.md)).
**Land the two-group bound as the only behaviour and delete the flag.**

### `SETTER_OF_SETTER`'s confluence test has no type condition

Found in [074](closed/074-FA-cross-pass-oscillation-plan.md): before the
parent-first gate, its main target was `__list_iter__.position`, always
`int64`, with a setter count growing every pass (348 → 1212). Nothing there
observes a distinction. `PYC_SOSDEMAND=1` (acting only on members whose
type is a union) exists and is default 0. Decide it with the two-question
test.

### Off-by-default levers — inventory at `3f36072b`

Every one below is a `getenv` in `fa_flags.cc` or `fa.cc` that alters
splitting or routing. **DELETE** means measured dead, measured
non-monotone, or arbitrary by the test above. **MEASURE** means untested
since `PYC_CSDCPA1=2` became the default, so decide by a same-binary corpus
`check` A/B. **PENDING** means it is a correct mechanism waiting on a named
issue.

| lever | origin | verdict |
| --- | --- | --- |
| `PYC_CSM` | 075 element-CS method fan | DELETE — never runs; forced, errors 0 → 9 (157) |
| `PYC_SPLITHOMO` | 157 tuple homogeneity / slot-signature key | DELETE — too coarse (1) / not a fixed point of its own decision (2) |
| `PYC_SETTERMIN` | 157 coarsest setter partition | DELETE — it broke the dict split; its "exposed bug" was its own damage (157 Correction 2) |
| `PYC_SETTERGATE` | lifts the SETTER stage's gates | DELETE — lifting a gate is permission without a reason; `=2` is +31% CSs, non-monotone |
| `PYC_CSELEM` | 101 mint-time element canon | DELETE — mint-time keying is a dead frame (closed/128) |
| `PYC_CSSITELESS`, `PYC_CSRESPLIT` | 129 step 4 | DELETE — worse on 4 of 5 / inert |
| `PYC_CSDEFREUSE` | 074 join-by-element | DELETE — regresses sudoku5, sunfish |
| `PYC_CSCALLSITE` | 129 third clause | DELETE mode 1/3 (fans); mode 2 measured a corpus-wide wash |
| `PYC_ESPATH` | 148 split the whole path in one pass | DELETE the selector (fans; −3 programs). The scheduling idea stays in 157. |
| `PYC_ESRECV` | 146 E receiver split on BOXING | MEASURE — costed plcfrs/sudoku5 standalone before ESBLOCK was default |
| `PYC_CANON`, `PYC_TYPEKEY`, `PYC_ROUTEGATE`, `PYC_GSIGRET=0` | 074 routing controls | DELETE — controls for a closed investigation |
| `PYC_CPAMARK` | 074 mark/CPA naming | DELETE with MARK_SETTER |
| `PYC_VIOLCS`, `PYC_CSMEMBER` | 143 violation → route 4, member key | MEASURE — they fixed bh pre-flip; bh compiles without them now |
| `PYC_ELEMSETTER`, `PYC_CSSLOTDEMAND`, `PYC_CONFDEMAND`, `PYC_ESDEFS1`, `PYC_SIZEOF_VIOL`, `PYC_CONFNIL` | 129/133 candidate-set variants | MEASURE, and DELETE any that are inert |
| `PYC_SLOTARITY` | 132 cross-CS arity demotion | PENDING [132](132-arity-is-representation-not-provenance.md) — correct, does not pay yet |
| `PYC_RETDEMAND` | 157 ES demand → route 4 link | PENDING [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) |
| `PYC_TYPEMOVE` | 148 terminate on a fixed point | PENDING 157 — 2 suite tests |
| `PYC_SPLITEDGES2`, `PYC_FILTEREQ` | 148 stage-5 bound / exact filter reuse | land SPLITEDGES2 (above); FILTEREQ neutral, DELETE |
| `PYC_SOSDEMAND` | 074 | decide (above) |

Audited and **not** splitters (keep): `PYC_SELFPROD` (6), `PYC_HARDREUSE`
(5; mode 4 costs chaos, dijkstra2, plcfrs, sudoku5, webserver), `PYC_CSKEY`
(3), `PYC_ROUTECYCLE` (3), `PYC_ROUTESTABLE` (1). The demand-driven stages
`SETTER` and `split_css` are keyed on setter equivalence classes.

## Removal procedure

Six CI gates (`make test`), then a corpus `./corpus_sweep.sh -m check` A/B
from ONE binary with the env toggled
([147](147-analysis-result-depends-on-the-binary-not-the-inputs.md): a
1–2-program delta between two builds is noise). Report per-program
`compile_rc` / `run_rc` / `stdout_match` and container CS / shapes. A
removal that loses a working program is not automatically wrong, but the
trade is recorded, and the lost program is root-caused rather than used as
a reason to keep the lever: *"if some random arbitrary split happens to
cause a program to compile then it was hiding another bug"* (author,
2026-09-16).

## Dead ends — do not rebuild

- **A compatibility rule for receiver identity** (`PYC_RECVEXACT`): 48
  suite failures. A predicate can only reject an edge from an existing
  contour. It cannot break up a receiver that arrives already unioned.
- **Receiver-keyed method contours as identity** (shedskin's `dcpa`
  dimension, `func.cp[dcpa][c]`): structural. It multiplies contours
  whether or not anything demands it. pyc's argument-type identity
  already subsumes dispatch filtering wherever the receiver is an
  argument: `self` is a formal, and a genuinely polymorphic `o.go()`
  splits per class (`FUNES go es=57 [A]`, `es=58 [B]`).
- **Unconditional receiver filtering**: fixes its fixture, costs 23 suite
  tests.
- **Demand-gated receiver filtering at repair time**: never fires. By the
  time the stage runs, the receivers' element types have converged to the
  same union, so the evidence is gone. The shared WRITER must split
  instead, which is what `PYC_ESBLOCK=1` (default) does.
- **Peeling two creation points at N == 2** (`PYC_CSPEEL2`): passes the
  test on paper and is non-monotone in practice. Peeling the reader does
  not unshare the writer.
- **Wholesale fan as a last rung** (shedskin's rung 4, `1 < csites < 10`):
  a bounded fan is still a fan. It is recorded in 157 as a fact about the
  reference implementation. It is not a plan.

## Related

- [129](129-plan-demand-driven-creation-set-splitting.md) — the plan.
- [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) — when demand
  is asked, and why lower stages starve.
- [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) —
  decisions taken on transient types, and the missing re-merge.
- [156](156-FA-split-int-from-float-coerce-last.md) — the numeric demand
  that coercion currently swallows.
