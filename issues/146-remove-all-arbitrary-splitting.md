# 146 — remove every arbitrary split; only demand splitting

**Status: open. Umbrella issue.**

**Author's imperative, restated 2026-09-08:** *"no arbitrary splitting, only
demand splitting."* This issue tracks the audit to completion, because the
rule has been applied case-by-case and each application found another one.

## The test

A splitting mechanism is **arbitrary** when its partition size is a COUNT
OF THINGS rather than the number of distinct things the demand
distinguishes. Concretely, ask:

1. **Would this split happen if the demand were absent?** If yes, it is
   arbitrary.
2. **Does the demand alone decide WHETHER, with the handle deciding only
   WHICH parts?** If yes, it is a mechanism, and allowed.

(CLAUDE.md, "Provenance is never the answer" — the REASON/MECHANISM
refinement.)

**The diagnostic, learned the hard way:** an arbitrary lever is
non-monotone. More splitting makes results WORSE. On `bh`, `PYC_RECVFAN=2`
left 1 warning and `=3` left 5; that inversion is the tell, and it is what
finally identified the lever after it had been read as progress.

**Corollary the author drew:** such a lever is DELETED, not defaulted off.
An off-by-default arbitrary lever still gets reached the moment a program
resists, and it reads as sanctioned because it is in the tree.

## Removed so far

| mechanism | what it did | commit |
| --- | --- | --- |
| `PYC_RECVFAN` | fanned a method ES per receiver CreationSet whenever the receiver's CSs were all containers — no demand test in the path, partition size = receiver count. Modes ≥2 also lifted PER_CS_RECEIVER's quiescence gate. | `ede0210f` |
| route 4's fan | `split_css_by_defs` gave EVERY creation point its own contour once any demand reached it. Now partitions by assign-set signature. −37% container contours. | `3e8dcb10` |
| the ripeness wait | `kCsDefSplitRipe` made the coarsest rung wait on a CLOCK (3 passes) rather than on the finer rungs actually declining; a finer rung firing reset the count, so on `bh` it never acted. | `ede0210f` |
| `split_es_by_call_site`'s fan | one group per in-edge, partition size = caller count. Reachable as `PYC_CSCALLSITE=1` AND as the fallback whenever the demand-driven branch had no usable flow graph. | `e4edc2c3` |
| two caller-count caps | `kCsDefSplitMax` refused on the number of callers/creation points, not on the partition the demand asks for. 152 of 182 refusals on `bh` were the second one. | `20f76f27`, `e4edc2c3` |

## Still present — the work this issue tracks

### A. `PYC_CSSPLIT=1` — DONE 2026-09-08

**Removed.** The flag and both of its behaviours are gone; a split EntrySet
now always inherits its parent's instance CreationSet.

Measured on removal, corpus default arm, clean sweep:

| | before | after |
| --- | --- | --- |
| container CreationSets | 3713 | **2762 (−26%)** |
| `linalg` ess / css / container | 1593 / 4433 / 210 | **617 / 1551 / 49** |
| `chess` ess / css / container | 1591 / 6433 / 169 | **686 / 2301 / 62** |
| `sudoku5` | COMPILE-FAIL | **runs** |
| `rdb` | run:1 | COMPILE-FAIL *(the one regression)* |
| `chull` | run:1 | run:139 *(broken either way)* |
| pyc suite | 313 / 0 | 313 / 0 |

Three `fa-converge` goldens gained `total-passes 2 → 3`, `events 2 → 3`
and **`splits[setter]: 1`** — which is the change working: the SETTER stage
now does on demand, one pass later, what the structural pre-split was doing
for free. Re-blessed, every changed line belonging to this change.

`rdb` is the honest cost and is recorded rather than explained away; it was
already failing at run time, so nothing that worked was lost.

A methodology note, because it nearly produced a fabricated result: an
earlier run of this measurement showed **20+ programs flipping abort →
success** and a −951 contour delta. That was an artifact — I had recompiled
corpus programs directly while a sweep was running in the same directories,
and we overwrote each other's binaries. Re-measured alone, `linalg`,
`plcfrs` and `sudoku3` abort at BOTH settings. Only the contour numbers
survived, because they came from a compile phase that finished before the
interference. Never touch a corpus directory while a sweep is live.

### A (original statement, kept for the record). `PYC_CSSPLIT=1` — a CreationSet follows an EntrySet split, BY DEFAULT

The most significant one, and CLAUDE.md already names it: *"`PYC_CSSPLIT=1`
makes a CreationSet follow an EntrySet split by construction — the inverted
dependency as a mechanism."*

Read the guard carefully, because it is easy to invert:

```c
if (es && es->split && !cssplit) {   // reuse the SPLIT PARENT's CreationSet
```

At the default `cssplit = 1` this block is SKIPPED, so a split EntrySet
falls through and MINTS A FRESH CreationSet. `=0` restores parent
inheritance. So the default multiplies CreationSets with every ES split —
exactly "a contour split because a surrounding contour was split", which
the project's opening rule forbids.

It is not gratuitous: the comment records that it fixed a real
non-convergence (ifa/055's repro, 52 passes at the cap → 28 converged) by
letting `set`/`dict` instances separate by element type where
`clone_methods_per_cs` could not reach them. **So the demand it serves is
real and the mechanism is wrong.** Replacing it means finding what
legitimately separates those instances — element type — and asking for it
directly.

### B. `creation_point`'s `(allocation site × contour)` identity

ifa/128. The allocation site is the REASON here, and it splits with no
demand at all — test 1 above fails outright. `PYC_CSDCPA1` is the
experiment that removes it (start merged, one CreationSet per sym); the
whole 129/133/144 line of work is what it takes to make that flippable.

### C. `PYC_CSDEFPART=2`'s fan fallback — DONE 2026-09-08

**Removed**, along with the flag. The partition is now the only behaviour.

The fan existed because the partition had no grouping key for a
NON-container CreationSet: `build_cs_flow_graph` read only the ELEMENT
channel, so it returned null for every plain class. Declining instead
(mode 1) was measured and cost five corpus programs, which is why the fan
was kept — treating the symptom.

Fixed at the cause. `cs_content_avars` gives the graph the right content
channel for each shape — ifa/104's two channels — using `cs->vars` when
there is no element:

```c
static void cs_content_avars(CreationSet *cs, Vec<AVar *> &out) {
  if (cs->sym->element && cs->sym->element->var && cs->added_element_var) {
    if (AVar *e = unique_AVar(cs->sym->element->var, cs)) out.add(e);
    return;                       // containers unchanged
  }
  for (AVar *v : cs->vars) if (v && v->out) out.add(v);
}
```

Containers are untouched by construction, so no container result moves.

Measured on `bh`: route-4 mints **24 → 7 with ZERO fan splits**, and `Vec3`
lands on the **same 6 contours the fan produced** — the same precision, by a
demand-driven partition instead of an arbitrary one. That is the outcome
this issue wants: not a trade, a replacement.

Corpus, default arm: **0 verdict changes**, container CreationSets
2762 → 2736. Flag arm at this tree: 2153, −22% against the default. All six
gates pass.

### C (original statement, kept for the record). `PYC_CSDEFPART=2`'s fan fallback — mine, shipped 2026-09-07

`split_css_by_defs` partitions by assign-set signature, but when
`build_cs_flow_graph` returns null — which it does for EVERY non-container
CreationSet, since it needs an element channel — mode 2 falls back to the
per-creation-point FAN:

```c
if (defpart >= 2 && !g) goto Lfan;
```

This was a deliberate, measured choice: mode 1 (decline instead of fan)
cost five corpus programs. But it is the same defect as the levers above,
shipped as the default, and it is where `bh`'s 16 `Vec3` mints come from.
Retiring it needs a grouping key for non-container CreationSets — the
CSFlowGraph generalised from the element channel to `cs->vars`.

### D. `MARK_TYPE` and the marks-based setter splitter — DONE 2026-09-08

**Removed**, 171 lines of `fa.cc`, together with `PYC_NOMARK`.

**The finding that mattered: mark splitting was never actually off.** The
flag had three states and two consumers with DIFFERENT thresholds:

```c
analyze_again = nomark_enabled() >= 1 ? 0 : split_ess_for_mark_type(...);   // 1 >= 1 -> off
if (nomark_enabled() < 2 && split_ess_setters_marks(...)) {                 // 1 <  2 -> RUNS
```

At the default `PYC_NOMARK=1`, MARK_TYPE was off but
**`split_ess_setters_marks` was still live on the default path** — so
provenance-based splitting was running, while the flag's name and
CLAUDE.md's "mark-based splitting was retired (`PYC_NOMARK` defaults to 1)"
both read as though it were not. A flag with two thresholds hid it.

Measured before removal (`PYC_NOMARK=2`, i.e. marks fully off):

- corpus verdicts **byte-identical on all 77 programs**;
- container CreationSets unchanged at 2736;
- pyc suite 313 passed / 0 failed on both backends;
- it fired on exactly one program, `plcfrs`, where it COST contours
  (ess 1102 → 1094 with it off).

Deleting the two entry points left a cascade of dead helpers
(`split_with_setter_marks`, `split_marked_es_confluences`,
`build_setter_marks`), removed by following the compiler's
unused-function warnings to a fixed point.

`tests/splitter_mark_type.py` is KEPT and repurposed rather than deleted —
the shape it builds is the valuable part and ifa/142 cites its header. It
now pins that the shape costs nothing to resolve without marks:
`CALLS: direct=69 dynamic=0`, byte-identical to what MARK_TYPE produced.
The STAGES line lost `MARK_TYPE` and gained nothing.

### E. `PYC_CPA` — NOT arbitrary; this entry was wrong

**Audited 2026-09-08 and cleared.** The original entry here called it a fan
on the strength of its test header's phrase *"fanning the contour into one
per single CreationSet"*. Reading the implementation, that is not the
arbitrary shape:

> when a positional formal's live type is a union of >= 2 CreationSets, fan
> the contour into one filtered contour per single CS and re-dispatch every
> edge across them.

The **union is the demand** and its **members are the parts**, so the
partition size equals the number of distinct things the demand
distinguishes. Both questions pass: it cannot fire without a union, and the
union alone decides both whether and which. It is the same legitimate shape
as `split_es_by_call_site`'s surviving demand-driven branch.

It stays. Two notes for whoever revisits it: its `PYC_CPA=N` cap is
count-based, which is the shape this issue distrusts elsewhere, though here
it bounds a resource rather than deciding a partition; and it is default
0, so it is dead weight rather than a live violation.

### D (original statement, kept for the record). `MARK_TYPE` — provenance by construction

Mark distance is depth-from-a-generating-AVar, so no type tuple can name
what it separates. `PYC_NOMARK` defaults to 1, so it is OFF — but the code,
the stage, and two `splitter_*` tests remain, and `tests/splitter_mark_type.py`
still asserts it is *"the only stage that can break that symmetry"*. That
claim is now false: `CS_DEF_PART` fires on the same fixture. Under the
delete-don't-default rule this should go, and the test's claim be updated.

### E (original statement, kept for the record). `PYC_CPA` — cartesian-product fan, default 0

`tests/splitter_cartesian_product.py`'s own header describes it as *"fanning
the contour into one per single CreationSet"*. That is a fan by the
definition above. Off by default; audit and delete or justify.

### F. AUDITED 2026-09-08 — no further arbitrary splitting found

Each lever put to the two questions. **None is a fan; three are not
splitting mechanisms at all**, which is why they read as suspicious from
their names alone.

| lever | what it actually does | verdict |
| --- | --- | --- |
| `PYC_SELFPROD` (6) | validity/eviction test for an ALREADY-RECORDED split decision — "evict only the disjoint complement". Decides whether a past split is still valid; creates no partition. | not a splitter |
| `PYC_HARDREUSE` (5) | detach-route contour REUSE. Reuses an existing contour instead of minting one, so it REDUCES contours. The opposite of a fan. | not a splitter |
| `PYC_SETTERGATE` (0) | lifts the SETTER stage's quiescence gate. Its comment draws the analogy to `PYC_RECVFAN=2`, but the cases differ in the way that matters: RECVFAN lifted a gate so a FAN could run earlier, this lifts one so a DEMAND-DRIVEN stage can. | not arbitrary (dead lever) |
| `SETTER` / `SETTER_OF_SETTER` | `split_css` groups starters by `same_eq_classes(v->setters, av->setters)`. Demand = the setter confluence; parts = the distinct setter equivalence classes. | demand-driven |

**Whole-file sweep for the fan shape**, to catch what the lever-by-lever
pass might miss. Every surviving `new CreationSet(...)` inside a
per-item loop, and every `dec->groups.add`, is keyed:

| site | key |
| --- | --- |
| `split_css` | setter equivalence class (+ ledger route for cross-pass stability) |
| `cs_peel_group` (ladder routes 1/3) | the assign-set analysis; declines unless a PROPER subset moves |
| `cselem_shape_canon` | element SHAPE, with canonical reuse |
| `decide_entry_set_split` | edge type-compatibility groups |
| `split_es_by_call_site` | assign-set signature |

**So the audit is complete for everything except B.** Every arbitrary
mechanism found has been removed; what remains partitions on something the
demand names.

### D, second pass: the audit found a mark path D had missed

`split_with_type_marks` ran as the VIOLATION stage's fallback when type
splitting found nothing. It is mark-based — provenance — and D's first pass
missed it because, unlike the other two, **it was never gated by
`PYC_NOMARK`**, so deleting that flag did not touch it. Found by asking
which callers still pass `SPLIT_MARK`.

Removed (86 more lines, with `collect_es_marked_confluences` and
`build_type_marks` following it). Measured first: pyc suite 313 / 0, corpus
verdicts identical on all 77, container CreationSets unchanged at 2736. Like
the other two it fired on exactly one program, `plcfrs`, where turning it
off REMOVED contours (ess 1094 → 1088).

`SPLIT_MARK` is now passed nowhere, so `fmark` is always 0 and
`cur_split_type_only = (!fsetters && !fmark)` simplifies to `!fsetters`.
The `#define` is gone.

**The lesson:** "off by default" was never the right thing to check. Three
mark splitters existed; one was gated and off, one was gated and *on* (the
`< 2` threshold), and one was not gated at all. Only auditing the call sites
found all three.

### The HARDREUSE follow-up — hypothesis REFUTED, mode 5 stays

The suspicion was that mode 5's extra condition had gone stale, since its
rationale cites *"a setter- or MARK-driven split"* and marks are now gone.
Half of it is indeed dead — that is what the `!fmark` simplification above
records. **The other half is load-bearing, and measurably so.**

Corpus, mode 5 (default) vs mode 4:

| | mode 5 | mode 4 |
| --- | --- | --- |
| compile failures | 2 | **7** |
| total ess | 27952 | 27689 (−263) |
| total css | 98064 | 97685 (−379) |

Mode 4 loses **`chaos`, `dijkstra2`, `plcfrs`, `sudoku5`, `webserver`** to
compile failure — and `chaos` and `sudoku5` currently WORK — to save about
1% of contours. Per-program it is a genuine trade rather than a uniform
one: mode 5 is better on `plcfrs` (ess 1088 vs 1123) and mode 4 better on
`go` (454 vs 622), with `chess`, `linalg` and `sunfish` identical.

So mode 5 keeps its default. The follow-up is CLOSED, not deferred: the
question was asked, measured, and answered against the hypothesis.

### The original follow-up note

`PYC_HARDREUSE`'s mode 5 exists because *"a setter- or MARK-driven split
can produce two contours with identical argument types on purpose"*. Marks
are gone as of D, so half that rationale is stale and mode 5's extra
condition may now be over-conservative — i.e. it may be refusing reuse that
is no longer ambiguous. Worth re-measuring modes 4 and 5 against each other
now. It is a REUSE question (possibly too FEW merges), not an arbitrary-
splitting one, so it does not block this issue.

### F (original list, kept for the record). Not yet audited

`PYC_SELFPROD` (default 6), `PYC_HARDREUSE` (default 5), `PYC_SETTERGATE`,
and the `SETTER` / `SETTER_OF_SETTER` stages' partitioning. Each needs the
two-question test applied and the answer recorded here.

## Order of work

B is the goal (it is what `PYC_CSDCPA1` exists to retire) but depends on
143/144/145. A is the largest live violation on the DEFAULT path and is
independent of the flag. C is the smallest and is a regression I introduced.
D and E are deletions of dead-but-sanctioned code.

**A, C and D are DONE**; **E** and **F** audited and cleared. Every
arbitrary mechanism found has been removed. **B** is all that remains, and
it lands as the flag flip.

## Verification

Each removal: the six CI gates, plus a corpus `check` sweep on BOTH arms
(default and `PYC_CSDCPA1=2 PYC_CSLADDER=3`), reported as
programs-differing-from-default and container CreationSets. A removal that
loses corpus programs is not automatically wrong — see C, where declining
cost five — but the trade must be measured and recorded, not assumed.
