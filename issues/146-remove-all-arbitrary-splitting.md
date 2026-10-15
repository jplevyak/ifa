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
| `MARK_SETTER` / `MARK_SETTER_OF_SETTER` (D) and the mark scaffolding (`build_joint_type_marks`, `collect_cs_marked_confluences`, `AKIND_MARK`, `AVar::mark_map`, `different_marked_args`, `PYC_CPAMARK`, `IFA_DBG_MARKWHY`) | the last mark splitter; it had survived because `voronoi2` needed it until the nil codegen fix. Same-binary corpus A/B (`check__default__7b644df7+0f0a43ce` vs `check__PYC_NOMARKSETTER_1__7b644df7+6900e5ef`): verdicts and warnings identical on all 77; ess 31,094 → 28,921 (−7.0%), css 120,841 → 112,539 (−6.9%), container CSs 2,735 → 2,671, compile wall 2,590 → 2,279 s (−12%). plcfrs 94 → 67 passes, voronoi2 32 → 25. `mark_distance_skew` / `mark_setter_skew` re-blessed at `ess=3` (every changed line is the removed split) and their `.known_issue` sidecars deleted. The `FAPassStage` values stay, unused | 2026-10-13 |
| stage 5's per-CreationSet fan in `split_edges` (`PYC_SPLITEDGES2` deleted) | one filtered EntrySet per receiver CreationSet; now two groups at most (the first part, the rest), re-derived and asked again. A riding `None` (ifa/178) goes in both groups' filters, which the flag's old two-group path ignored. That alone cost `richards`: its four Task receivers need three two-group attempts, and stage 5's per-Var attempt cap (`PYC_VIOLATTEMPTS`, 2) stopped it one short. The cap now counts only FUTILE attempts, ones that did not narrow the violating value; removing it outright sent softrender's genuine int/float rows to the pass limit. Same-binary `check -R` A/B against the fan (`check__default__9f6150ae+aa1a63cd` vs `check__PYC_SPLITEDGESFAN_1__…`): verdicts and warnings identical on all 77, contours equal (ess 28,919 → 28,921, css 112,539 → 112,538) | 2026-10-13 |
| route 4's partition of a confluence-only candidate | `split_css_by_defs` partitioned EVERY candidate with two or more creation points, including those a type confluence alone nominated (`tc_cs_dropped`): a fact. Its `demanded` set gated only EntrySet splits (`ESBLOCK`). It now declines a candidate not in `demanded`. Same-binary corpus A/B (`check__default__7b644df7+14447c89` vs `check__PYC_CSDEMANDONLY_1__…`): verdicts and warnings identical on all 77; contours a wash in both directions (ess 31,081 → 31,094, css 120,849 → 120,841). Found because it re-split every pair ifa/170 step 3 merged | 2026-10-13 |
| `SETTER_OF_SETTER`'s ungated confluence test (`PYC_SOSDEMAND` deleted) | a member was a candidate whenever its setters differed from a consumer's: a fact. Found in [074](closed/074-FA-cross-pass-oscillation-plan.md), where its main target was `__list_iter__.position`, always `int64`. A member is now nominated only on a demand backtracked to it: its union is irrepresentable, it lies on the backward flow reach of a type violation, it holds two or more receivers of a send that dispatches to two or more functions, or a CreationSet it holds carries one of these to any depth through its content (`sos_type_conflict`). Two weaker tests, "the member is a union" and "the member is irrepresentable", each lost dijkstra2, plcfrs, pylife and sunfish (`check__PYC_SOSDEMAND_{1,2}__185a9936+927e6d63`). Without the dispatch clause, `splitter_setter_of_setter` went from `dynamic=0` to `dynamic=2`, so author's directive: zero run-time dispatch is the goal, and an unresolved dispatch is a demand. Corpus `check -R` (`check__default__185a9936+aac86e9b` → `check__default__185a9936+3981dab9`): verdicts and warnings identical on all 77; ess 28,921 → 28,893, css 112,538 → 112,518, container CSs 2,671 → 2,669. Without the dispatch clause (`check__PYC_SOSDEMAND_3__185a9936+aac86e9b`), ess was 28,542. That clause's 351 EntrySets change dynamic calls on one program of the 12 it touches: genetic2 22 → 15. The other 11 stay as they were (chull 15, bh 8, loop 8), so the dispatch they observe is not one a setter split resolves | 2026-10-13 |
| the DELETE batch of the off-by-default inventory: `PYC_CSM` (the `CSM_ELEMENT_CS` stage; its `FAPassStage` slot stays, unused), `PYC_SPLITHOMO`, `PYC_SETTERMIN`, `PYC_SETTERGATE`, `PYC_CSELEM` with `PYC_CSSITELESS` / `PYC_CSRESPLIT` / `PYC_CSREJOIN` (the whole shape-key machinery, `CreationSet::elem_key`, `FA::var_elem_key`, routes `kR_cselem` / `kR_csshape`), `PYC_CSDEFREUSE`, `PYC_CSCALLSITE`'s third clause, `PYC_ESPATH`, `PYC_CANON` (`EntrySet::canon_key`), `PYC_TYPEKEY`'s compatibility use, `PYC_ROUTEGATE`, `PYC_GSIGRET=0`, `PYC_FILTEREQ` | each was already measured dead, non-monotone, or arbitrary (inventory at `3f36072b`, where this table's verdicts were set). Off-default code only, about 1,400 lines; the default path is kept in every case. The `IFA_DBG_DEMAND` line lost its cselem/canon/rejoin fields, so the sweep parses only `ess css container_cs shapes pshapes`. `make test` green; corpus `check -R` `check__default__e254f017+3d4e01a4` is identical to `check__default__4f0de62f+913fcfe1` on every column of all 77 programs, contour counts included | 2026-10-15 |
| the MEASURE batch: `PYC_ESRECV`, `PYC_VIOLCS`, `PYC_CSMEMBER`, `PYC_ELEMSETTER`, `PYC_CSSLOTDEMAND`, `PYC_CONFDEMAND`, `PYC_ESDEFS1`, `PYC_SIZEOF_VIOL`, `PYC_CONFNIL` (and `split_es_by_arg_types`, `cs_member_signature`, `cs_slot_signature`, which only they called) | every mode measured by a same-binary `check -R` against `check__default__e254f017+3d4e01a4` (sweeps `check__PYC_<FLAG>_<mode>__e254f017+dcb2b1b0`). **No arm makes a program compile, run or match CPython that does not already.** Inert on every column of all 77: `CONFNIL=1`, `ELEMSETTER=1`, `ESDEFS1=1`, `VIOLCS=1`. Contours only, no verdict change: `VIOLCS=2` +5 ess/+5 css; `VIOLCS=3` +369/+1,038 and +4 warnings; `CSMEMBER=1` and `=2` (identical) +828/+2,899, +444 container CSs, +5 warnings. Lost programs: `ESRECV=1,2` fysphun (+339 ess); `CSSLOTDEMAND=1` plcfrs times out (+575/+2,804 on the rest, +4 warnings on softrender); `SIZEOF_VIOL=1` ant, rdb, rsync, sha, sudoku3, sunfish; `SIZEOF_VIOL=2` 48; `CONFDEMAND=1,2,3` all 75 that compile, because stage 1's confluences also carry the dispatch and precision splits the probe dropped (its own comment said it was a probe). Each loss reproduces on the pre-deletion binary (`e254f017`), so the batch above did not cause them. bh, which `VIOLCS` / `CSMEMBER` were built for, compiles at the default. After deletion `make test` is green and `check__default__e254f017+34ca222e` is identical to `check__default__e254f017+3d4e01a4` on every column of all 77 | 2026-10-15 |

## Still in the tree

### Off-by-default levers — inventory at `3f36072b`

Every one below is a `getenv` in `fa_flags.cc` or `fa.cc` that alters
splitting or routing. **DELETE** means measured dead, measured
non-monotone, or arbitrary by the test above. **MEASURE** means untested
since `PYC_CSDCPA1=2` became the default, so decide by a same-binary corpus
`check` A/B. **PENDING** means it is a correct mechanism waiting on a named
issue.

**As of 2026-10-15 every DELETE and MEASURE row is gone** (see the last two
rows of Removed). What is left is PENDING, each waiting on its own issue.

| lever | origin | verdict |
| --- | --- | --- |
| `PYC_SLOTARITY` | 132 cross-CS arity demotion | PENDING [132](132-arity-is-representation-not-provenance.md) — correct, does not pay yet |
| `PYC_RETDEMAND` | 157 ES demand → route 4 link | PENDING [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) |
| `PYC_TYPEMOVE` | 148 terminate on a fixed point | PENDING 157 — 2 suite tests |
| `PYC_SOSDEMAND` | 074 | decided: deleted, its demand test is the default (see Removed) |

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
- [156](closed/156-FA-split-int-from-float-coerce-last.md) — the numeric demand
  that coercion currently swallows.
