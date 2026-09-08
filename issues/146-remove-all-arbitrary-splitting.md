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

### A. `PYC_CSSPLIT=1` — a CreationSet follows an EntrySet split, BY DEFAULT

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

### C. `PYC_CSDEFPART=2`'s fan fallback — mine, shipped 2026-09-07

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

### D. `MARK_TYPE` — provenance by construction

Mark distance is depth-from-a-generating-AVar, so no type tuple can name
what it separates. `PYC_NOMARK` defaults to 1, so it is OFF — but the code,
the stage, and two `splitter_*` tests remain, and `tests/splitter_mark_type.py`
still asserts it is *"the only stage that can break that symmetry"*. That
claim is now false: `CS_DEF_PART` fires on the same fixture. Under the
delete-don't-default rule this should go, and the test's claim be updated.

### E. `PYC_CPA` — cartesian-product fan, default 0

`tests/splitter_cartesian_product.py`'s own header describes it as *"fanning
the contour into one per single CreationSet"*. That is a fan by the
definition above. Off by default; audit and delete or justify.

### F. Not yet audited

`PYC_SELFPROD` (default 6), `PYC_HARDREUSE` (default 5), `PYC_SETTERGATE`,
and the `SETTER` / `SETTER_OF_SETTER` stages' partitioning. Each needs the
two-question test applied and the answer recorded here.

## Order of work

B is the goal (it is what `PYC_CSDCPA1` exists to retire) but depends on
143/144/145. A is the largest live violation on the DEFAULT path and is
independent of the flag. C is the smallest and is a regression I introduced.
D and E are deletions of dead-but-sanctioned code.

Suggested: **A**, then **C**, then **D**/**E**, with **B** landing as the
flag flip.

## Verification

Each removal: the six CI gates, plus a corpus `check` sweep on BOTH arms
(default and `PYC_CSDCPA1=2 PYC_CSLADDER=3`), reported as
programs-differing-from-default and container CreationSets. A removal that
loses corpus programs is not automatically wrong — see C, where declining
cost five — but the trade must be measured and recorded, not assumed.
