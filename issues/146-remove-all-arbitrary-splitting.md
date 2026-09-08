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

### B. `creation_point`'s `(allocation site × contour)` identity — STATUS 2026-09-08

Measured at this tree, with A/C/D/E all landed:

| | default | flag (`PYC_CSDCPA1=2 PYC_CSLADDER=3`) |
| --- | --- | --- |
| compile failures | 2 | 7 |
| container CreationSets | 2736 | **2151 (−22%)** |
| programs differing | — | **8** |

The 8 split two ways, and only the first group blocks a flip:

- **regressions from working**: `bh` (ran-ok → 134), `kanoodle` (→ 139),
  `richards` (→ 139), `sudoku5` (→ COMPILE-FAIL);
- **already broken, failure mode changes**: `chull`, `plcfrs`, `quameon`,
  `sudoku3` (all `run:134/139` → COMPILE-FAIL).

**The known blocker is intra-class receiver sharing** (ifa/143): with every
list on one CreationSet, ONE `append` contour writes into all of them, and
no ES-side mechanism separates them — established exhaustively in the
receiver-filtering entry above. What is needed is shedskin's `dcpa`:
method contour identity that includes the receiver's data contour.

**Attempted as a compatibility rule, 2026-09-08, and reverted.**
`PYC_RECVEXACT` made `edge_type_compatible_with_entry_set` hard-reject an
edge whose receiver type differs from a method contour's, so receiver
identity would be decided at contour SELECTION rather than repaired later.
Measured: **48 suite failures**, `chess` segfaults, `plcfrs` times out, `bh`
stops compiling, and even on the acceptance fixture it makes call
resolution WORSE (65 direct against 69).

Why it fails is instructive and worth not repeating: a compatibility
predicate can only reject an edge from an EXISTING contour, so it forces
new contours without ever breaking up a receiver that arrives already
unioned. shedskin does not do it this way — `create_template` MAKES the
contour for a `(dcpa, cartesian product)` pair when a call with that pair
occurs, so the union never forms. Retrofitting the same identity onto
selection is not the same change and does not work.

**CORRECTED, on the author's objection: "why does B need contour creation
keyed on receiver data contour? shouldn't it be demand driven?"** It should,
and the conclusion above was wrong in exactly the way this issue exists to
catch.

Keying contour creation on the receiver's data contour is STRUCTURAL: it
multiplies contours whether or not anything demands it. The three-way rule
already covers the temptation — identity may be as fine as it likes, but
*"turning a finer identity directly into more contours is the same error as
splitting on structure, wearing different clothes."* shedskin doing it that
way is not an argument for pyc doing it; shedskin has no demand-splitting
rule to keep.

**Why I was pushed there, and what is actually wrong.** Every demand-driven
avenue I measured died on the same wall: by the time the splitting stages
run, the union has formed, every writer carries it, and the demand can no
longer name its parts. I read that as "demand cannot reach this" and reached
for structure. But it is a statement about **WHEN I looked**, not about
whether the information exists:

- On the acceptance fixture the writers ARE distinguishable — `append`'s
  contours carry `[A]` and `[B]` as their VALUE types (`es=77`, `es=78`).
  The union is on the RECEIVER, not the value. So an element-side flow graph
  has two assign sets and the partition is nameable.
- `Vec3` in `bh` shows the same: `CSFLOW ... sets=28 csites=4 (in_defs=4)` —
  28 assign sets over 4 creation points, all of them in `defs`.

**So the demand is available; what fails is applying it.** The measured
mechanical gap is `in_defs=0` on the list CreationSets: the CSFlowGraph's
creation points and `cs->defs` are DISJOINT AVar sets, so a partition the
demand names cannot be applied to the defs that need re-pointing. That is
the bug ifa/144's "informative" check detects and then declines on — a
concrete, fixable problem in the plumbing between the demand and the
mechanism, not a reason to abandon demand splitting.

**B's next step is therefore to close that gap** — make the demand's
partition applicable to the creation points it is about — not to key
contour creation on the receiver. Nothing shipped.

### B (original statement). `creation_point`'s `(allocation site × contour)` identity

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

### E. `PYC_CPA` — ARBITRARY after all. My "cleared" verdict was wrong.

**Corrected 2026-09-08 on the author's objection:** *"CPA isn't pure demand
imo. pure demand and dispatch aware filtering e.g. of the receiver for
single dispatch oop should subsume blind CPA, right?"* Yes, on both halves.

I cleared it by reasoning "the union is the demand and its members are the
parts". **That is the error.** A union's EXISTENCE is not a demand. A demand
is an observed distinction that REQUIRES separation — a type violation, an
irrepresentable union, a dispatch that cannot resolve. CPA asks for none of
them:

```c
for (MPosition *p : es->fun->positional_arg_positions) {
  AVar *av = es->args.get(p);
  int n = av->out->type->sorted.n;
  if (n < 2 || n > limit) continue;      // ANY formal with 2..N CreationSets
```

The whole function contains **zero** references to `violation`,
`irrepresentable`, `mixed_basics`, `dispatch` or `unresolved` — it fans
every positional formal whose type has 2..N CreationSets and has reached a
fixpoint, whether or not anything downstream is harmed by the union. So
question 1 answers itself: *would this split happen if the demand were
absent?* Yes, always.

**What should subsume it.** CPA is groping at dispatch precision. In
single-dispatch OOP the position that determines dispatch is the RECEIVER,
and it only needs separating where dispatch actually fails to resolve. So
demand (an unresolved dispatch, or an irrepresentable union) plus
dispatch-aware filtering of the receiver reaches every case CPA reaches,
and:

- fires only where resolution is actually blocked, not on every union;
- acts on the ONE position that determines dispatch, not on all positional
  formals;
- therefore produces a strictly smaller partition for the same resolution.

The cases CPA "fixes" that this would not touch are the ones nothing
needed fixed.

**REMOVED 2026-09-08** — the stage, `split_ess_cartesian_product` and
`cpa_enabled`, 102 lines. `decide_csm_split` / `apply_csm_split` are shared
with the CSM stage and stay.

Corpus-neutral by construction and by measurement: `PYC_CPA` defaulted to 0
and was set by exactly one `.env` in the tree, and a confirming sweep is
verdict-identical on all 77 with container CreationSets unchanged at 2736.
All six gates pass.

`tests/splitter_cartesian_product.py` is kept and converted into the
ACCEPTANCE TEST for the replacement. On that fixture CPA was doing real
work, and the trade is not one-sided:

| | with CPA | without |
| --- | --- | --- |
| calls | direct=68 dynamic=1 | **direct=69 dynamic=0** |
| element separation | held | lost — `.ay()` on a `B` warns |

Call resolution is BETTER without it; what is lost is the element
separation. So the `.check` records the state with **no** warning — what the
replacement must reach — and a `.known_issue` explains the gap, per
CLAUDE.md's rule against baking a regression into a golden. The test reports
KNOWN today and flips to PASS when demand-driven receiver filtering lands.

**Original status line:** arbitrary, to be removed. It is `PYC_CPA=0` by default, so it
is dead weight rather than a live violation, and by the
delete-don't-default rule it should go. Removing it is cheap; the
replacement — receiver filtering keyed on unresolved dispatch — is the real
work and is what should land first if anything currently depends on CPA
being reachable. `tests/splitter_cartesian_product.py` pins it and would
need the same treatment `splitter_mark_type.py` got.

### Receiver filtering: the MECHANISM is right, and it cannot be retrofitted as a repair

**Author's clarification:** *"the filter is for oop dispatch so that the
correct class values flow to the methods for that class."* That is the
right frame, and it located the problem exactly. On the fixture, where
`self.aas` holds only `A` and `self.bbs` only `B`:

```
es=60 [list#1077 list#1111]           [A B]
es=77 [list#1070 list#1077 list#1111] [A]   <- writes A into THREE lists
es=78 [list#1071 list#1077 list#1111] [B]   <- writes B into THREE lists
```

`append` is contoured by its VALUE but its RECEIVER is a union, so A and B
both land in `list#1077` and `list#1111`. The element type then unions two
classes the program never mixes, and `self.aas[-1].ay()` draws a spurious
`illegal: B`. **The warning is pyc's imprecision, not the program's error.**

**Filtering the receiver fixes it, exactly.** Restricted to the receiver
position — note position 0 is the SELECTOR symbol and position 1 is the
receiver, which cost one wrong iteration — every `append` contour gets a
single receiver, the warning goes, and the result matches what CPA achieved
(`direct=68 dynamic=1`, no warning) by a principled mechanism instead of a
blind fan.

**But it cannot be applied as a demand-driven repair.** Three gatings, all
measured:

| gating | result |
| --- | --- |
| none — filter every method receiver | **fixes the fixture**, and breaks everything else: 23 suite failures, `chess` and `plcfrs` stop compiling, plcfrs ess 1088 → 1277 |
| receiver CreationSets' element types differ | **never fires** — by the time the stage runs they have converged to the same `{A, B}` union |
| a `SEND_ARGUMENT` violation on the formal | never fires; the violation sits on a CS-contoured AVar, not a formal |

The middle row is the finding. **The demand is unobservable at repair time
because the merge has already destroyed the evidence** — ifa/142's fixed
point for the third time, now at the receiver. Once `list#1077` holds
`{A,B}`, nothing can see that it was ever meant to hold only one.

**So receiver-keyed method contours have to be IDENTITY, not repair.** That
is precisely what shedskin does — `func.cp[dcpa][c]` indexes every function
contour by the receiver's data contour from the start, so the union never
forms and there is nothing to detect later. Under CLAUDE.md's three-way
rule that is legitimate: identity may be as fine as it likes, and it is not
a split.

This makes it **B-shaped work**, not a separate task: it belongs with
`creation_point`'s identity change rather than as another splitting stage.
No code shipped — the mechanism is proven on the fixture but has no
correct trigger as an after-the-fact repair.

### Dispatch-gated, group-wise filtering — ALREADY IMPLEMENTED (2026-09-08)

**Author's refinement:** *"it should only be used if the call requires a
dispatch. that shouldn't cause a problem because the filtered values can't
pass through to the dispatch target in any real execution trace. not a
single receiver, just all compatible receivers."*

The soundness argument is right, and the design is right — filter to the
receivers COMPATIBLE with each target rather than to one receiver, so the
partition size is the number of dispatch targets rather than the number of
CreationSets. That is what makes it safe where my unconditional version
(one contour per receiver CS) broke 23 suite tests.

**And pyc already does it.** Measured on a genuine polymorphic call:

```python
class A:
    def go(self): return 1
class B:
    def go(self): return 2
def call(o): return o.go()
```

```
FUNES fun=go    es=57 [go#936] [A#1075]      <- receiver narrowed to A
                es=58 [go#936] [B#1077]      <- receiver narrowed to B
FUNES fun=call  es=55 [call#940] [A#1075]
                es=66 [call#940] [B#1077]
```

Each target's `self` holds exactly the class that selects it. This is not a
special dispatch filter — it falls out of ordinary contour identity,
because **`self` is an argument** and contour compatibility already keys on
argument types. Which is the author's own earlier point about static
dispatch: pyc's `c` subsumes shedskin's `dcpa` wherever the receiver
appears as an argument.

**So the fixture is NOT a dispatch case, and that is why nothing here
reaches it.** Its three receivers are all `list` — one class, one `append`
body, no target to choose between:

```
es=77 [list#1070 list#1077 list#1111] [A]
```

The receiver is a UNION WITHIN ONE CLASS. There is no dispatch to gate on,
no set of compatible-per-target groups to filter into, and the union itself
IS the contour's argument type, so argument-type identity cannot break it
up either.

**Which pins the remaining gap precisely.** What the fixture needs is per-
CreationSet method contours *within* a single class — several data contours
of `list` not sharing one `append`. That is not dispatch and not a demand-
driven split; it is data-contour identity, shedskin's `dcpa` dimension, and
it belongs to **B**. Every ES-side avenue is now measured and closed:
dispatch filtering already exists, violation-gated filtering never fires,
element-difference gating is hidden by the fixed point, and unconditional
filtering breaks the suite.

### Superseded first attempt, kept for the record

Built as specified: CPA's own mechanism (`decide_csm_split` /
`apply_csm_split` — filter a formal into one contour per single
CreationSet), with CPA's arbitrary trigger replaced by a real demand (a
`SEND_ARGUMENT` or `DISPATCH_AMBIGUITY` violation), receiver position first
(`fun->sym->self` marks a method). Two demand scopings, both measured, both
reverted:

| demand scoping | result |
| --- | --- |
| a violation anywhere in the contour | fires (plcfrs 13 splits, rdb 1) and **makes things worse**: `plcfrs` compile 0 → **1**, ess 1088 → 1118, css 2549 → 2597 |
| the violation is ON the formal being split | **never fires** — 0 splits on every program tried |

**Why neither can work, and it is not a tuning problem.** The two ends do
not meet:

- the DEMAND is recorded on the AVar where the call FAILS — a call
  argument, or, as on this very fixture, a **CreationSet-contoured** AVar.
  Probed: the fixture's only violation is `kind=SEND_ARGUMENT av=3294
  contour_is_es=0` — the list ELEMENT, not any formal;
- the MECHANISM can only filter an EntrySet **formal** —
  `decide_csm_split` searches `es->args` for the AVar and returns null
  otherwise.

Those are rarely the same AVar, and joining them means walking backward
from the failing use to the formal whose union produced it — which is
`backflow_path` / the CSFlowGraph, i.e. the CS-side machinery, not
something an ES-side filter has.

**So the fixture's demand is CS-side, and CPA was not answering it.** CPA
fanned ES formals blindly and the separation propagated downstream by
accident; that is why removing it costs the element separation while
IMPROVING call resolution (direct 69/0 against 68/1). The right home for
this fixture is the CreationSet machinery — ifa/143's problem, element
writers separated — not a receiver filter.

**What the receiver filter would still be right for** is the case it was
named for: a genuine single-dispatch receiver whose union blocks
resolution, where the violation IS on a formal. Nothing in the corpus
exercised that shape in this measurement, so it has no evidence behind it
yet and no code was shipped. `tests/splitter_cartesian_product.py`'s
`.known_issue` stays as the acceptance test, but should now be read as
pointing at the CS-side fix rather than at receiver filtering.

### The refinement this correction forces on the test itself

Question 1 in the test above — *would this split happen if the demand were
absent?* — is only as good as one's willingness to ask what the demand IS.
Twice now I have accepted a fact about the program as though it were a
demand:

- here, "the formal's type is a union" (a fact) read as "the union must be
  separated" (a demand);
- earlier, in ifa/144, "the CreationSet has several creation points" (a
  fact) read as licence to give each its own contour.

**A demand is something OBSERVING a distinction and being unable to
proceed.** A union, a multiplicity, a difference in provenance — these are
all facts about the program. None of them is a demand until something reads
one and fails.

### E (superseded reading, kept for the record). `PYC_CPA` — NOT arbitrary; this entry was wrong

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

### D is NOT complete — corrected 2026-09-08

**"D done" was claimed twice and was wrong both times.** The accounting:

| mark splitter | gated by `PYC_NOMARK`? | status |
| --- | --- | --- |
| `split_ess_for_mark_type` (MARK_TYPE) | yes, and off at the default | **removed** |
| `split_ess_setters_marks` | yes, but its threshold was `< 2` so it was **ON** | **removed** |
| `split_with_type_marks` (VIOLATION fallback) | **not gated at all** | **removed** |
| **MARK_SETTER / MARK_SETTER_OF_SETTER** | **not gated at all** | **STILL LIVE** |

Nine functions and 260 lines went; what remains is the fourth splitter,
plus scaffolding.

**Why the fourth stays.** `collect_cs_marked_confluences` finds confluences
by comparing `mark_map`s and `compute_setters(..., AKIND_MARK)` keys the
setters on marks, so it is provenance-driven and by the rule it should go.
Measured with its marked confluences suppressed: pyc suite 313 / 0, corpus
verdicts identical **except `voronoi2`, which stops compiling** (`no
matching function for call to '_CG_f_...'`) — verified alone, not a sweep
artifact. It also fires on `plcfrs`, where suppressing it REMOVES contours
(ess 1088 → 1075).

So it is load-bearing on exactly one program, and retiring it means
supplying `voronoi2`'s separation some other way. That is real work, not a
deletion, and it is left in place with a note at the site rather than
removed on a hope.

**Also still there, and merely dead rather than live:**

- the MARK_TYPE stage block, now a hollow shell that sets `cur_split_stage`,
  does `analyze_again = 0` and records timing;
- `different_marked_args` and `cpa_mark_enabled` (`PYC_CPAMARK`), reachable
  only under `fmark`, which is now always 0 — except for the one live
  caller inside `collect_cs_marked_confluences`;
- `AVar::mark_map`, `MarkMap`/`MarkElem`, and the `MARK_*` stage enum
  entries.

**The lesson, and it is the same one twice:** "is it off by default" is the
wrong question. Four mark splitters existed — one gated and off, one gated
and on, two not gated at all. Only auditing CALL SITES found them, and I
declared victory after the first two because the flag's name implied it
covered everything.

### D, first and second passes: what was removed

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

**A and C are DONE**; **F** audited and cleared. **E is arbitrary** (my
first verdict on it was wrong) and awaits removal plus its replacement. **D is PARTIAL**
— three of four mark splitters removed, the fourth (MARK_SETTER) load-bearing
for `voronoi2` and left in place with a measurement. Remaining: finish D,
then **B** as the flag flip.

## Verification

Each removal: the six CI gates, plus a corpus `check` sweep on BOTH arms
(default and `PYC_CSDCPA1=2 PYC_CSLADDER=3`), reported as
programs-differing-from-default and container CreationSets. A removal that
loses corpus programs is not automatically wrong — see C, where declining
cost five — but the trade must be measured and recorded, not assumed.
