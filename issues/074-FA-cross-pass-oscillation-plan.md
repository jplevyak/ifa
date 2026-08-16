# 074 — Plan: solve the FA cross-pass splitter oscillation (033/063/065/066 master plan)

**Status:** plan, 2026-07-30. **OVERTAKEN IN PART 2026-08-13 by
[100](100-FA-display-removed-from-contour-identity.md)**: the lexical
display has been removed from contour identity by design decision, which
retires this plan's Stage 0 and Stage 4 (both prototyped exactly that and
concluded it could not be done) and invalidates the measurements Stage 2
was ruled out on — the contour growth Stage 2 was competing against is
largely gone (ess down 40-80% corpus-wide; yopyra converges). Re-derive
Stage 2 against the new baseline before building anything here.
Previously: **re-measured and RE-BASED 2026-08-12
after [098](098-FA-per-pass-reset-scoped-to-reachable-set.md) landed.
See the two sections immediately below: the headline `pass_limit_hit`
metric was partly measuring the stall guard rather than the analysis, and
re-basing with the guards disabled cuts the target set from 17 programs
to **8** — while exposing a separate, smaller and independently
actionable defect: the stall guard's re-arm heuristic is **causing
miscompiles** (sudoku5 and msp_ss compile to crashing binaries today and
to correct ones when their descent is allowed to finish). One of Stage
1's premises is also corrected.** Synthesizes and *sequences* the existing
diagnostic work in [033](closed/033-splitter-non-idempotent-divergence.md) /
[063](closed/063-no-type-bucket-triage.md) /
[064](closed/064-method-phantom-display-blocks-es-split-routing.md) /
[065](closed/065-mark-stage-es-split-routing-and-growing-product.md) /
[066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md) into
one actionable build order, **re-grounded on current measurements** and
updated for the one thing that changed since that work (2026-07-23): the
[073](closed/073-teach-splitter-productive-vs-inert-context.md) `check_split`
type-identity fix (2026-07-30), which ties recursion knots by *type*
identity and may have dissolved the central obstacle those issues hit.
**Affects:** `ifa/analysis/fa.cc` — `run_split_stages` (the 8-stage
sequence), `split_for_per_cs_method_receivers`, `split_ess_setters`/
`split_css`/`creation_point`, the issue-033 ledger (`ledger_*`,
`cs_group_signature`, `setter_site_signature`); `python_ifa_build_syms.cc`
`def_fun_pyda` (method `nesting_depth`).

## Re-measured 2026-08-12, post-[098](098-FA-per-pass-reset-scoped-to-reachable-set.md) — and the headline metric is partly measuring the stall guard

098 (FA's per-pass reset was scoped to the *previous* pass's reachable
set, so 19-24% of edges per pass carried an older pass's
`args`/`rets`/`formal_filters`) is now fixed. Three consequences for this
plan, in order of how much they change it.

**1. It does not dissolve the oscillation.** Re-running this doc's own
`PYC_DBG_OSC` measurement on the fixed tree: **17 of the 73 examples that
reach FA** still end with `pass_limit_hit=1`. Set membership has moved a
lot since 2026-07-30 (dijkstra2, sudoku3 and pygmy now converge; msp_ss,
plcfrs and rdb are new), but the phenomenon is intact — as this doc
predicted, it is a distinct mechanism.

| program | final_pass | violations | ess | | program | final_pass | violations | ess |
|---|---|---|---|---|---|---|---|
| plcfrs | 47 | 5517 | 2724 | | pylife | 41 | 90 | 393 |
| softrender | 57 | 1029 | 847 | | linalg | 16 | 79 | 1089 |
| msp_ss | 22 | 947 | 1097 | | timsort | 17 | 66 | 367 |
| amaze | 17 | 632 | 723 | | chess | 39 | 63 | 3035 |
| rdb | 26 | 602 | 1714 | | go | 25 | 59 | 730 |
| sudoku5 | 23 | 381 | 789 | | genetic2 | 50 | 3 | 639 |
| rubik | 19 | 165 | 754 | | bh | 52 | 2 | 526 |
| sudoku4 | 27 | 160 | 1317 | | loop | 38 | 2 | 1150 |
| **yopyra** | **102** | **0** | **2086** | | | | | |

**2. `pass_limit_hit` is not a precision fact — it is substantially a
stall-guard artifact, and this doc's tables inherit that.** `softrender`
and `sudoku4` are "new" oscillators only in that sense. Relax
`IFA_STALL_LIMIT` / `IFA_NONIMPROVE_LIMIT` and both converge on the fixed
tree to **exactly the pre-098 final violation counts**:

| program | pre-098 | post-098, guards as shipped | post-098, guards relaxed |
|---|---|---|---|
| sudoku4 | 53 passes, 26 viol, `plh=0` | 27 passes, 160 viol, `plh=1` | 54 passes, **26** viol, `plh=0` |
| softrender | 77 passes, 13 viol, `plh=0` | 57 passes, 1029 viol, `plh=1` | 90 passes, **13** viol, `plh=0` |

No precision was lost; the descent simply got longer than the guard
tolerates. The reason is the shape of these descents — a long flat
plateau followed by a sudden collapse. softrender's tail, post-098,
guards relaxed:

```
429 429 429 429 429 429 429 427 443 443 443 1015 207 24 24 24 24 24 24 13
```

The guard is **sticky** and re-arms only on a *strictly* improving pass
(`v < best_violations`), so a program whose collapse lands one pass after
the guard fires is recorded as oscillating, while the same program with a
two-pass-shorter plateau is recorded as converging. Pre-098 sudoku4 hit
117 violations at p26 (below its best of 129 → re-armed → ran on to 26);
post-098 the same pass lands at 160 (not below 129 → no re-arm → stop).
**One pass's arithmetic decides set membership.** So the 17-program table
above, the 2026-07-30 one it replaces, and the dup-category tables derived
from that set are all measuring guard calibration mixed with analysis
behavior. **The re-base is done — see the next section; the genuine
target set is 8 programs, not 17.**

## THE TARGET (2026-08-16): what FA should produce for the minimal reproducer

The 13-line reproducer below is the concrete objective for this plan.
Stating what *correct* looks like, because "make it converge" is not a
specification and a fix that converges by losing precision would satisfy
it.

### The program is monomorphic. Every function has exactly one type.

```python
def shrink(M):  M1 = copy.deepcopy(M); del M1[0]; return M1
def total(M):   return 0.0 if len(M) == 0 else M[0][0] + total(shrink(M))
total([[1.0], [2.0], [3.0]])
```

`shrink: list[list[float]] -> list[list[float]]` and
`total: list[list[float]] -> float`, at every recursion depth — the
recursion is **type-invariant**, which is the whole point. Exactly two
structural list types exist in the program: the outer `list[list[float]]`
and the inner `list[float]`.

### Target contour counts

| | optimal | why |
|---|---|---|
| `total` | **1** | one argument type, all depths |
| `shrink` | **1** | same |
| `deepcopy` (module wrapper) | **1** | same |
| `list.__deepcopy__` | **2** | outer list, inner list |
| `list.__getitem__` / `__len__` / `len` | **2** each | one per receiver element type |
| `list.append` / `__setitem__` | **2** each | same |
| iterator methods | **2** each | same |
| `float.__deepcopy__`, scalar ops | **1** each | |

**Order of 20 contours in total, and — the property that actually
matters — a count that does not depend on recursion depth.**

### Measured today

| | now | target |
|---|---|---|
| total contours | **236, growing +2.8/pass without bound** | ~20, constant |
| distinct type keys | **189, also growing** | ~20 |
| `total` alone | **ess 11, setkey 11** | 1 |

### The diagnosis this forces, and it is not the splitter

`total` has 11 contours and **11 distinct type keys**. The contours are
each individually *justified* by a distinct type — so the splitter is
behaving. What is wrong is that FA has invented 11 distinct **types** for
what is semantically one.

The loop: `list.__deepcopy__` is written `r = []` / append / return
(`__pyc__/04_sequence.py`). Every contour of it therefore owns a distinct
allocation site, so it mints a **fresh CreationSet**. That CS is
structurally identical to its predecessor but has its own identity, so
the caller sees a new type key, so a new contour of `total`/`shrink` is
minted, which calls `__deepcopy__` with a new receiver type, which mints
another CS — for ever, one level per pass.

That explains the trigger table exactly: `deepcopy` supplies the fresh
allocation, recursion supplies the unbounded depth, and nesting supplies
the second level that compounds it.

**So the work is CreationSet identity, not contour splitting** — two
structurally identical lists minted at the same allocation site should
share a CS, or at minimum share a type key. That is
[066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md)'s
creation-site keying, which this plan's Stage 1 (ii) named and never
built. The reproducer makes it testable in 1.2 s.

## MINIMAL GROWTH REPRODUCER (2026-08-16) — 13 lines

Until now this plan had no small test case: everything was measured on
corpus programs, and `issues/README`'s own note said convergence issues
"want a different kind of test" because "no small program reproduces 102
passes". That turns out to be wrong — one does.

`tests/deepcopy_recursive_nested_growth.py`:

```python
import copy

def shrink(M):
    M1 = copy.deepcopy(M)
    del M1[0]
    return M1

def total(M):
    if len(M) == 0:
        return 0.0
    return M[0][0] + total(shrink(M))

print(total([[1.0], [2.0], [3.0]]))
```

With the guards off it never reaches a fixed point, and the growth is
**dead linear**:

| pass | 20 | 40 | 60 | 80 | 101 |
|---|---|---|---|---|---|
| ess | 136 | 193 | 249 | 305 | 364 |
| css | 628 | 688 | 748 | 808 | 875 |

+2.8 contours and +3 CreationSets per pass, for as long as it is allowed
to run. It demands only `TYPE_CONFL` and `SETTER` — the same
cascade-serialization signature as `go`, `linalg` and `plcfrs`. Distilled
from `linalg.py`'s `determinant`/`Minor` pair, which is why it shares
their profile.

**The trigger needs all three of `deepcopy`, recursion, and a NESTED
container.** Remove any one and it converges:

| variant | |
|---|---|
| deepcopy + recursion + nested | **no** — p102, growing |
| manual element copy instead of `deepcopy` | yes — p13 |
| `deepcopy`, no recursion | yes — p23 |
| `deepcopy` + recursion, flat list | yes — p10 |

That is a much sharper handle than "go/linalg/plcfrs are non-convergent",
and it runs in 1.2 s.

New probe `PYC_DBG_CONVERGED=1` prints exactly `CONVERGED=0` or
`CONVERGED=1` — the one bit a convergence test can assert without pinning
pass counts or contour totals, which move with every FA change. The test's
`.check` holds `CONVERGED=1`; pyc prints `CONVERGED=0` today, which is
what its `.known_issue` tag records.

## STATUS 2026-08-16 — the current numbers

Re-measured on HEAD. Supersedes the 08-14 section below, which was taken
before `PYC_NOMARK` became the default.

**Shipped config: 4 of 84 programs end `pass_limit_hit=1`** — `go`,
`linalg`, `plcfrs`, `sudoku5` — against **20** at the start of the
08-13 session.

**Guards off (only the 100-pass cap): 3 of the 73 programs that produce
an FA result fail to converge** — `go`, `linalg`, `plcfrs`. `sudoku5`
converges once the guards stop firing, so it is a guard cutoff, not
non-convergence.

The six that left the list since 08-14 (`bh`, `genetic2`, `hq2x`,
`pylife`, `rubik`, `sudoku4`) did so because **`PYC_NOMARK=1` is now the
default**, not because of anything later. Verified directly: `hq2x` at
`PYC_NOMARK=0` with guards off is still p102 / ess 1664, and at the
default is p14 / ess 620. (The "9 of 73" figure in the 08-14 section was
measured against a marks-on binary and should be read as the marks-on
number.)

**The remaining three are one group — cascade serialization.** All three
demand only `TYPE_CONFL` + `SETTER` (+ `SETTER_OF_SETTER` for `go` and
`plcfrs`); `MARK_TYPE` is out of the picture entirely. Growth over the
last ten passes before the cap:

| | ess p92 → p101 | |
|---|---|---|
| linalg | 867 → 943 | **+76**, still growing |
| go | 641 → 669 | **+28**, still growing |
| plcfrs | 1340 → 1342 | +2, plateaued |

`linalg` is worth a note: at 08-14 it was *flat* (586 → 585) and is now
the fastest grower. Its trajectory changed with marks-off plus the
element numeric coercion — it converges toward a larger contour set
(ess 944 vs 586) and is still climbing at the cap. So the group did not
just shrink; its composition moved, and `linalg` is now the sharpest
subject rather than `go`.

## STATUS 2026-08-14 — where oscillation and growth stand after this session

Re-measured on current HEAD (self-product fix default-on), **not** carried
over from the 08-12 re-base, which predates it.

**Shipped config:** 18 of 84 programs end `pass_limit_hit=1`, down from 20.
Whole-corpus deltas over the session: analysis time **−54%**, total ess
**+1.0%**, generated C **−0.9%**, and two exit-code improvements
(`sunfish` 124→0, `tictactoe` 1→0) with **no regressions**.

**With the divergence guards off** (`IFA_STALL_LIMIT` /
`IFA_NONIMPROVE_LIMIT` = 1000, so only the hard 100-pass cap bounds the
loop), **9 of the 73 programs that produce an FA result fail to
converge**: bh, genetic2, go, hq2x, linalg, plcfrs, pylife, rubik,
sudoku4.

### The 9 split cleanly by cause

`PYC_NOMARK=1`, guards off — **6 of them are non-convergent solely
because of `MARK_TYPE`**:

| program | marks on | marks off | |
|---|---|---|---|
| hq2x | p102, 0 viol, ess 1664 | **p14**, 0 viol, ess 620 | converges |
| pylife | p102, 54 viol, ess 539 | **p27**, **0** viol, ess 389 | converges |
| rubik | p102, 1729 viol, ess 1448 | **p26**, 104 viol, ess 724 | converges |
| sudoku4 | p102, 70 viol, ess 675 | **p31**, 26 viol, ess 470 | converges |
| genetic2 | p102, 3 viol, ess 371 | **p43**, 3 viol, ess 396 | converges |
| bh | p102, 2 viol, ess 651 | **p53**, **0** viol, ess 381 | converges |
| go | p102, 103 viol, ess 686 | p102, 107 viol, ess 674 | **resists** |
| linalg | p102, 78 viol, ess 586 | p102, 43 viol, ess 944 | **resists** |

`plcfrs` resists too — p102 either way — but it improves sharply all the
same: violations **1319 → 85** and it stops exhausting its time budget
(rc=124 at a 1800 s timeout → rc=1, analysis completes).

Note pylife and bh reach **zero** violations with marks off: for them
`MARK_TYPE` is not merely non-terminating, it is actively *losing*
precision.

### Growth vs. pure oscillation

ess over the last ten passes before the cap (p92 → p101), guards off:

| still growing | | plateaued, still churning | |
|---|---|---|---|
| bh | 611 → 639 (+28) | rubik | 1443 → 1443 |
| go | 667 → 686 (+19) | sudoku4 | 675 → 675 |
| hq2x | 1639 → 1657 (+18) | genetic2 | 371 → 371 |
| pylife | 519 → 537 (+18) | linalg | 586 → 585 |
| | | plcfrs | 1299 → 1300 |

So the two diseases are now separable per program: **four still grow
without bound; five have stopped growing but keep re-deciding**. Three of
the four growers (bh, hq2x, pylife) are in the `MARK_TYPE` group, so
`MARK_TYPE` accounts for most of the remaining *growth* as well as most
of the remaining non-convergence.

### What that leaves

1. ~~**`MARK_TYPE` (6 programs).**~~ **Resolved 2026-08-14 — `PYC_NOMARK`
   is now the default.** Holding it back on the five precision
   regressions was applying the wrong standard: this project's recorded
   rule for the structurally identical case (the lexical display, issue
   100) is the author's *"remove any other uses **even if it causes
   regressions**"*, accepted there at a steeper price — precision fell on
   several programs **and** oscillators went 16 → 20. Mark distance is
   provenance (depth from a generating AVar), exactly the category 100
   ruled out of contour identity; and this is not the widening
   [057](closed/057-sorted-tolist-fa-nonconvergence.md) prohibits, since
   it merges no type-distinct contours — it refuses the redundant split
   057 itself names. See the sweep below.
2. **Cascade serialization (go, linalg, plcfrs).** CS-minting stages and
   `TYPE_CONFLUENCE` are forced to alternate by the first-stage-wins
   gate. The self-product mint — which used to be the growth term here —
   is fixed, and it shows: **linalg and plcfrs have both stopped growing**
   (586→585 and 1299→1300 over ten passes) and now only re-decide. `go`
   is the one member of this group still adding contours at the cap, and
   is therefore the group's sharpest subject.
3. The stall guard still counts ledger ROUTE recoveries as divergence,
   which is why the shipped config stops 18 programs, several of them
   converging.

### `PYC_NOMARK` default-on, 2026-08-14 — measured against the previous default

| | before | after |
|---|---|---|
| programs ending `pass_limit_hit=1` | 18 | **10** |
| total analysis time | 738 s | **334 s** (−55%) |
| total EntrySets | 30675 | **26898** (−12.3%) |
| total generated C | 16.42 MB | **15.31 MB** (−6.8%) |
| exit codes | — | **mastermind2 1 → 0**, no regressions |

Violations better on 12 (chess 331→**0**, pylife 54→**0**, bh 2→**0**,
mastermind2 554→60, timsort 66→40, sudoku3 143→111, neural1 53→39,
dijkstra2 10→6, go 161→146, plcfrs 2442→2412, sudoku4 30→26, rubik
120→118), worse on 5 (softrender 1137→1380, sat 364→377, webserver 0→8,
kmeanspp 2→8, chull 0→2). `test_pyc.py` 265/14/0/4 both backends;
`ifa --test` 58/0. `PYC_NOMARK=0` restores the old behaviour.

### Where the lost separation comes from: SET-NAMING, not provenance

Traced on `chull` (0 → 2 violations), the smallest regressor. Both new
violations are the same shape:

```
chull.py:281: warning: illegal call argument type 'f0' illegal: Edge
              self.edges.extend(f0.InitEdges())
```

`f0` comes from `self.faces[-1]` yet carries `Edge`. Source: lines 424
and 432 are two structurally identical list comprehensions,
`self.edges = [e for e in self.edges if not e.delete]` and
`self.faces = [f for f in self.faces if not f.visible]`.

**Reduced to 33 lines** — `tests/listcomp_element_separation.py`. Two
list comprehensions over lists of different element types; reading the
first list's element afterwards yields the union. Narrowed by variants:

| variant | marks on | marks off |
|---|---|---|
| two comprehensions | 0 viol | **1 viol** |
| …without the `if` filter | 0 | **1** |
| …in two different methods | 0 | **1** |
| …via a local before the store | 0 | **1** |
| equivalent explicit `for`/`append` loops | 0 | 0 |
| one comprehension + one loop | 0 | 0 |
| two loops sharing one `[]` from a helper | 0 | 0 |

So it is not the filter, not scope, not the store, and **not
creation-site sharing** — the helper control shares one literal `[]`
across both uses and still separates cleanly.

`IFA_DBG_KEYSPACE` on `list.append` gives the mechanism outright:

| pass | marks ON | marks OFF |
|---|---|---|
| 7 | ess=3 setkey=3 **cpakey=6** | ess=3 setkey=3 **cpakey=6** |
| 8 | `MARK_TYPE` fires → ess=4 setkey=4 cpakey=6 | *unchanged, forever* |
| 9 | **cpakey 6 → 4** — the union breaks | |
| 10 | ess=4 setkey=4 cpakey=4, monomorphic | ess=3 setkey=3 cpakey=6 |

**Three contours are covering six distinct argument-type combinations,
and no type-based test can see it.** The two `append` call edges *do*
carry different types, but once `{A, B}` forms at the value formal it is
a **fixed point under set-naming**: every edge then carries `{A, B}`, so
`etype == stype` and `collect_type_confluence` finds no confluence at
all. `MARK_TYPE` is the only stage that can see through it, because
marks separate contributors by provenance even when their type *sets* are
identical — it mints one contour, that breaks the symmetry, and the
types re-derive monomorphically (cpakey 6 → 4).

**This is the set-naming cost, and it is the other half of the `hq2x`
finding.** Both are consequences of naming contours by tuples of type
*sets* rather than by the cartesian product of single types:

- `hq2x`: marks split what CPA would **not** (ess 287 vs cpakey 17) —
  gratuitous, unnameable over-splitting.
- `chull`: marks split what CPA **would** (ess 3 vs cpakey 6) — the only
  available symmetry-breaker for a union fixed point.

Marks are too aggressive *and* load-bearing because they are a provenance
proxy for a naming problem.

#### Why "proxy" is the literal description, not a metaphor

`AVar::mark_map` is `Map<CreationSet *, int>`, and `different_marked_args`
builds two `Vec<void *>` of `x->key` — CreationSets — returning "different"
iff those two **CS sets** are disjoint. That is exactly the question a
cartesian-product contour name asks: *which single CreationSet is at this
position?* Marks are already a CPA-style comparison.

The proxy is the **distance filter** wrapped around it: only CSs
satisfying `m - offset == x->value` are admitted — "this contributor is
exactly `offset` steps closer to a generator of that CS than the basis
is". So instead of comparing CS identity, it compares CS identity *as
sampled at one depth*. The filter exists because comparing raw CS sets at
a confluence is precisely what `TYPE_CONFLUENCE` already does, and that
goes blind on a union (`etype == stype`); depth re-introduces an ordering
the set-join destroyed. `build_type_mark`'s own comment says so: *"To
handle recursion, mark value*AVar distances from the nearest AVar
generating the value. Dataflow is considered to be only from lower to
higher distances for the purpose of splitting."*

`IFA_DBG_MARKWHY=1` counts, for every verdict of "different", whether the
**unfiltered** CS sets differ too (`cs_differ` — a CPA name would separate
these as well) or are identical (`cs_same` — separated purely by
depth-from-generator, a split no type-tuple naming would ever make):

| program | cs_differ | cs_same | pure-distance |
|---|---|---|---|
| the 33-line repro (marks load-bearing) | 245 | 6 | **2%** |
| hq2x (marks gratuitous) | 3989 | 205440 | **98%** |

The two populations separate almost perfectly, and in the direction the
theory predicts. Where marks earn their keep, 98% of their verdicts are
backed by a genuine CreationSet difference — they are doing the CPA job
that set-naming cannot. Where they are pathological, 98% of their
verdicts separate contributors carrying **identical** CreationSets, purely
because those contributors sit at different depths — which is how a
monomorphic one-line helper like `PIXEL00_20` (setkey=1, cpakey=1) ends
up with one contour per call site.

#### Swapping the CPA name in for the mark: REFUTED (`PYC_CPAMARK`, 2026-08-14)

The obvious move from the above is to drop the distance filter and let
`different_marked_args` compare the CS sets directly — "which
CreationSet is here", no depth term. `PYC_CPAMARK=1` does exactly that.
**It makes `MARK_TYPE` contribute nothing at all:**

| | marks ON | marks OFF | marks ON + `PYC_CPAMARK=1` |
|---|---|---|---|
| hq2x | p102, 0 viol, ess 1664 | p14, 0 viol, ess 620 | **p14, 0 viol, ess 620** |
| listcomp repro | 0 viol | 1 viol | **1 viol** |
| chull | 0 viol | 2 viol | **2 viol** |

`PYC_CPAMARK` ≡ `PYC_NOMARK`, on the pathological case and both precision
cases alike. **100% of `MARK_TYPE`'s distinct contribution is the distance
term**; the CS-set comparison it performs is entirely redundant with
`TYPE_CONFLUENCE`, which already compares types — and CS sets *are* types.

This also corrects an over-reading of the `IFA_DBG_MARKWHY` table above.
That the repro's verdicts are 98% `cs_differ` does **not** mean CPA naming
would recover its precision: those verdicts fire elsewhere in the program
and are redundant. The *decisive* verdict — the one that breaks the
`{A,B}` union at pass 8 — is necessarily in the 2% `cs_same` bucket,
because **inside a union the CS sets are equal by construction**, so depth
is the only thing left that can tell the two contributors apart. An
aggregate percentage hid the single verdict that mattered.

So the split rule that *should* apply is CS identity — the CPA name.
Marks approximate it with (CS identity ∧ depth). The `∧ depth` term is
the approximation: needed only because set-naming lost the per-CS view,
and harmful because depth is not a semantic property of the program — it
is a property of the contour graph, which splitting itself changes. That
is also why marks self-fuel: each split perturbs the distances that
produced it.

**But the CPA fix has to be applied to the NAME, not to the comparison.**
`PYC_CPAMARK` changed how a confluence is *tested*, which cannot help: by
the time a union exists, every comparison inside it sees equal CS sets.
Real cartesian-product naming acts earlier — it fans a multi-CS actual
out into one contour per single CS at the call site, so the union never
becomes a contour name and there is nothing to break. The hook for that
already exists in shape: `split_container_methods_per_element_cs` scans
`es->args` for positions with `av->out->type->sorted.n >= 2` and fans via
`set_or_copy_AEdge`; general CPA is that same scan without the
`all_same_container` narrowing. Built 2026-08-14 as `PYC_CPA` — see below. So the fix for the five regressors is **not**
to restore them: it is cartesian-product naming. Note `PYC_CANON` does
not supply it — `edge_canon_key` builds one `AType *` per position, i.e.
set-naming again; a CPA variant must key on single CreationSets. That is
the same conclusion the shedskin comparison pointed at from the start,
now with a 33-line repro and a stage-by-stage trace behind it.

(`PYC_CSM=2` was tried on `chull` first and is not the answer: violations
2 → 12, and both original violations survive.)

### `PYC_CPA` — cartesian-product naming, built and measured: the callee-side half is not enough

`PYC_CPA=N` adds a `CARTESIAN_PRODUCT` stage right after
`TYPE_CONFLUENCE` (same `!analyze_again` gate — the slot where
`MARK_TYPE` used to act, which is the point). Demand signal: a positional
formal whose live type is a union of 2..N CreationSets **and** which is a
*fixed point* — every incoming edge carries the whole union, so
`etype == stype` and stage 1 has nothing to see. It then fans the contour
into one filtered contour per single CS and re-dispatches. The
decide/apply pair is 075's `decide_csm_split` / `apply_csm_split`
unchanged: those were already generic, and only CSM's demand signal was
container-specific.

**The mechanism works.** On the listcomp repro, `list.append` goes
`ess=3 setkey=3 cpakey=6` → `cpakey=4`: the union breaks, exactly as
`MARK_TYPE` achieved, and **the target violation disappears** (`'a'
illegal: B` is gone). So a union *can* be prevented from being a contour
name, and marks are not the only way to do it.

**But callee-side fanning alone is a clear net loss.** 84-program sweep at
`PYC_CPA=4` vs the current default:

| | |
|---|---|
| newly failing to compile | **10** — ant, dijkstra, fysphun, genetic2, kanoodle, kmeanspp, pisang, sudoku2, sudoku4, tictactoe |
| newly compiling | 2 — plcfrs, sat |
| analysis time | **+78%** |
| `chull` | 2 → **121** violations, ess 570 → 422 |

The failure mode is uniform and diagnostic: `_CG_void_type` assigned to a
concrete type (`ant`: *"incompatible pointer to integer conversion
assigning to '_CG_int64' … from '_CG_void_type'"*; `dijkstra` the same
shape ×13). `_CG_void` is the no-type representation, and `chull`'s ess
*falling* while violations explode is the same story: **fanned contours
that never receive an edge.** Their bodies analyse with untyped vars, and
codegen emits `_CG_void`.

**Why, and what it means for a next attempt.** Fanning the callee makes
each product's filter admit exactly one CS. A call site whose actual at
that position is genuinely a *union* then matches none of them — so the
edge cannot dispatch, some products go unreached, and the imprecision is
replaced by a dispatch failure. Real CPA does the product **at the call
site**: the caller is split so that every call carries a single CS per
position, and only then is a per-CS callee well-defined. This build has
only the callee half.

So the remaining work is not a refinement of this stage — it is the
caller-side fan, which is the part that actually multiplies (and which
shedskin bounds with its `dcpa` limit). The `!fixpoint` restriction was
added during this build and made **no difference** (identical numbers on
the repro), which is itself informative: every union it fanned already
was a fixed point, so over-fanning is not coming from firing too widely.
It is coming from fanning only one side.

Flag off by default; suite 266/14/0/4, `ifa --test` 58/0.

#### Should the full call-site version be built? Measured answer: not now

The case for it rests entirely on the five programs that lose precision
with marks off, so the first question is what that precision loss
actually costs. Compiled and **run** both ways:

| program | marks ON | marks OFF | difference in behaviour |
|---|---|---|---|
| kmeanspp | compiles, **hangs** (>400 s, 0 bytes; CPython finishes in 114 s) | compiles, **aborts** on an assertion | both broken, different failure |
| chull | compiles, hangs (0 bytes) | compiles, hangs — identical | **none** |
| webserver | compiles, runs (it is a server) | same | **none** |
| sat | rc=1, does not compile | rc=1 | **none** |
| softrender | rc=134 | rc=134 | **none** |

**Not one of the five goes from working to broken.** `kmeanspp` changes
how it fails; the other four are indistinguishable at the program level.
The entire measurable cost of marks-off is the violation counter.

Against building it now:

1. **The benefit is undemonstrated.** Those five are the whole case, and
   none of them changes behaviour.
2. **The remaining non-convergence is not set-naming.** `go`, `linalg`
   and `plcfrs` resist `PYC_NOMARK` and are cascade serialization
   (measured above); CPA does not address them.
3. **The cost signal is already bad.** The callee half alone costs +78%
   analysis time and 10 newly-failing programs. The caller half is the
   one that multiplies.
4. **pyc has more splitting axes than shedskin** — `clone_for_constants`,
   per-CS receivers, setters, element CSs. CPA multiplies with all of
   them, and shedskin's `dcpa` bound exists precisely because unbounded
   CPA is not viable; pyc would need an equivalent bound designed against
   those interactions, which is a research task, not a port.
5. It would want to reach its own fixed point ahead of type partitioning
   — which is exactly the cascade-serialization pathology that causes the
   non-convergence still on the board.

Better next steps, in order:

1. ~~**The stall guard's `dup_split_attempts` accounting.**~~ **DONE
   2026-08-15 — see below.**
2. **`go`** — the only program still adding contours at the cap, and the
   representative of the cascade-serialization group.
3. If the five ever matter, attack them with the narrow tool (the
   comprehension / element-CS separation of
   [075](075-FA-element-cs-method-split-idempotent-plan.md)), not general
   CPA — the `tests/listcomp_element_separation.py` repro is a specific
   structural merge, not a general polymorphism problem.

The groundwork is in the tree either way: the stage, the flag, the
`_CG_void`-from-unreached-contours diagnosis, and the finding that the
fan must happen caller-side.

### The divergence guard now counts churn, not recovery (2026-08-15)

Every one of the four sites that bumped `dup_split_attempts` /
`cs_dup_split_attempts` has the same two-branch shape: the ledger finds
its key and either **routes** the group/CS into the product it recorded
on an earlier pass — the anti-oscillation machinery *working*, the graph
reconverging on the same answer — or **mints a fresh one anyway**, which
is the real non-idempotent churn. The guard was fed the sum. Re-measured
on current HEAD, routes outnumber mints by 85-99%:

| | rdb | rubik | sudoku5 | linalg | tictactoe | amaze | msp_ss | plcfrs | sat | softrender | chull |
|---|---|---|---|---|---|---|---|---|---|---|---|
| ROUTE (recovery) | 121 | 35 | 39 | 72 | 38 | 91 | 69 | 44 | 45 | 32 | 11 |
| GROUP/CS (churn) | 29 | 7 | 5 | 1 | 11 | 8 | 14 | 8 | 4 | 8 | 6 |
| FILTER | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |

New `FA::rederive_churn` counts only the mint-anyway half, and the guard
reads it. `dup_split_attempts` is unchanged for the `-v` line.

**Result — programs ending `pass_limit_hit=1`: 10 → 4.** Four newly
compile, with no exit-code regressions, and violations fall sharply:
amaze 689→15, rdb 3183→146, sat 377→9, softrender 1380→35, linalg 73→27,
msp_ss 617→452, sudoku5 390→303, rubik 118→104. Worse: plcfrs 2412→5237
(it now emits C where it previously emitted none), go 146→164. Cost:
+10.5% analysis time, +2.4% ess, +10.4% generated C. Suite 266/14/0/4;
`ifa --test` 58/0.

**The four newly-compiling programs, actually run** — because "compiles"
is not the claim that matters here, and 074's own `rdb`/`amaze` traps are
exactly this:

| program | run | vs CPython |
|---|---|---|
| **msp_ss** | rc=0 | **byte-identical** ✅ |
| **rdb** | rc=1 (its own "no iPod attached" path) | **byte-identical** ✅ |
| amaze | **segfault** | differs — compiles now, crashes |
| sat | rc=1, "Unhandled exception" | differs |

So two are genuine correctness wins — and they are precisely the two this
plan documented as *guard-caused miscompiles* (`msp_ss` "crash → correct,
prints its banner"; `rdb` now emits 674 KB of C where the pruned version
emitted 170 KB, the opposite of the trap). The other two move from
compile-failure to runtime-failure, which is not a regression — neither
worked before — but must not be counted as a win either.

**Both runtime failures root-caused (2026-08-15), and neither is an FA
convergence problem:**

- **`sat`** dies on an unhandled `AssertionError`. Its
  `enqueue(self, lit, reason=None, reason_txt=None)` has call sites that
  pass `reason=` at some points and `reason_txt=` at others, and
  `cause = var_info.reason` comes out typed `Clause ∪ str`. Reduced to a
  29-line **silent wrong-answer miscompile** — CPython prints 0, pyc
  prints 7 — filed as pyc
  [issues/046](../../issues/046-default-arg-omitted-differently-silently-wrong.md)
  with `tests/default_arg_omitted_differently.py`. **Trigger: two call
  sites of the same function that omit *different* defaulted parameters**
  (narrowed: not `__slots__`, not `None`-as-default, not keyword syntax —
  passing both explicitly, or always omitting the same one, is clean).
  Independent of, and older than, the guard change; it only surfaced
  because `sat` began compiling.

- **`amaze`** segfaults. Backtrace: `list::__contains__` →
  `tuple::__eq__(a1=0x100000001, …)` → `tuple::__len__` → `_CG_prim_len`
  on `0x100000001`, i.e. an **int used as a container**. The receiver is
  `_CG_any` — a boxed element read out of a list whose element type is a
  union, with dispatch choosing `tuple::__eq__` for an element that is
  actually an int. That is the container-element-union family
  ([035](../../issues/035-list-element-cast-salvage-guard-and-set-item-union.md),
  [075](075-FA-element-cs-method-split-idempotent-plan.md)) — the same
  root cause as `chull`, `tictactoe` and
  `tests/listcomp_element_separation.py`, reached from `while point in
  points2` at amaze.py:321. Not a new defect and not guard-related.

A third bug fell out of the narrowing: `write_c_prim` **aborts the
compiler** on a nameless primitive destination (`cg.cc:389`), filed as
[issues/047](../../issues/047-cg-nameless-lvalue-assert-on-prim.md).

### Per-splitter demonstration tests, and three stages nothing triggers (2026-08-15)

`PYC_DBG_STAGES=1` prints one line at the end of analysis naming the
stages that made progress — a *set*, not per-pass counts, because the
counts move with every FA change while "did this program need the setter
splitter at all" is a stable property of the program. `tests/splitter_*.py`
pin one stage each; the harness gained a `.env` sidecar so a test can
request the probe and the off-by-default flags.

| test | `.env` | STAGES |
|---|---|---|
| `splitter_type_confluence.py` | — | `TYPE_CONFL` |
| `splitter_setter.py` | — | `TYPE_CONFL SETTER` |
| `splitter_setter_of_setter.py` | — | `TYPE_CONFL SETTER SETTER_OF_SETTER` |
| `splitter_mark_type.py` | `PYC_NOMARK=0` | `TYPE_CONFL MARK_TYPE` |
| `splitter_cartesian_product.py` | `PYC_CPA=4` | `TYPE_CONFL VIOLATION PER_CS_RECV CPA` |

**Three of the ten stages could not be triggered at all**:
`MARK_SETTER`, `MARK_SETTER_OF_SETTER` and `CSM_ELEMENT_CS`. That is not
for want of trying small programs, and it is corroborated by the corpus:
across the twelve `IFA_DBG_STAGE` runs of the oscillating set, the union
of every stage that ever fires at the default settings is just

```
TYPE_CONFL (230)   SETTER (23)   SETTER_OF_SETTER (16)   VIOLATION (2, softrender only)
```

So under the shipped configuration **four stages carry all the work and
three never run**. `MARK_SETTER` / `MARK_SETTER_OF_SETTER` are enabled by
default (`PYC_NOMARK=1` only disables `MARK_TYPE`) and still never fire;
`CSM_ELEMENT_CS` is off by default and additionally sits behind stage 7's
full-quiescence gate.

The `PYC_CPA=4` row is the sharpest evidence for the starvation reading
of that: the *same program* that demands only `TYPE_CONFL` at default
settings demands `VIOLATION` and `PER_CS_RECEIVER` as well once CPA
perturbs it — those stages were never unreachable in principle, they were
simply never reached, because `run_split_stages` is first-stage-wins and
stage 1 is essentially never quiet. Any future attempt to justify a stage
by "it contributes nothing" has to clear this bar first.

## THE RE-BASED TARGET SET (2026-08-12)

Same corpus, same `PYC_DBG_OSC` probe, with `IFA_STALL_LIMIT` and
`IFA_NONIMPROVE_LIMIT` raised to 1000 (temporary; reverted) so **only the
hard `IFA_PASS_LIMIT` (100) bounds the outer loop**. That splits the 17
cleanly: a program that still ends `pass_limit_hit=1` is genuinely
non-convergent within 100 passes; one that ends `pass_limit_hit=0` was
only ever being cut off.

### Group C — genuinely non-convergent (8). This is the real target set.

| program | shipped guards | guards off | note |
|---|---|---|---|
| rubik | p19, 165 viol | p102, **1730** viol, ess 1406 | guard *helps*: diverges without it |
| plcfrs | p47, 5517 viol | p102, 135 viol, ess 3074 | far better, still caps |
| pylife | p41, 90 viol | p102, 90 viol, ess 393 | **stable residual** |
| linalg | p16, 79 viol | p102, 79 viol, ess 1089 | **stable residual** |
| go | p25, 59 viol | p102, 51 viol, ess 979 | |
| yopyra | p102, 0 viol | p102, 36 viol, ess 820 | guard *helps*: see below |
| bh | p52, 2 viol | p102, 2 viol, ess 526 | **stable residual** |
| loop | p38, 2 viol | p102, 2 viol, ess 1150 | **stable residual** |

The four **stable-residual** members (pylife, linalg, bh, loop) are the
cleanest specimens this plan has ever had: identical violation counts with
and without the guards, `.c` byte sizes within 0.02%, and — for the three
that produce a binary (pylife, bh, loop) — byte-identical program output
either way. So the guard costs them nothing, there is no descent to
confound the signal, and they are pure "splitter churns forever against a
fixed residual". Prefer these over dijkstra2/sudoku5 as the next
investigation's subjects.

### Group B — guard cutoff, not non-convergence (9)

These reach `pass_limit_hit=0` once the guards stop firing. **The
violation counts alone are a trap** — a program can "converge" by losing
the program, so every row below was checked by generated-C size and, where
both configs produced a binary, by *running* it:

| program | shipped | guards off | .c size | runtime |
|---|---|---|---|---|
| **sudoku5** | p23, 381 viol | p33, 30 viol | 0.83× | **crash → correct (`TIME 1.00`)** |
| **msp_ss** | p22, 947 viol | p67, 537 viol | 0.95× | **crash → correct (prints its banner)** |
| genetic2 | p50, 3 viol | p56, 3 viol | 1.00× | identical |
| timsort | p17, 66 viol | p18, 66 viol | 1.00× | identical |
| sudoku4 | p27, 160 viol | p53, 26 viol | 0.92× | crashes both ways |
| rubik* | p19, 165 viol | p102, 1730 viol | 2.36× | crashes both ways |
| chess | p39, 63 viol | p80, **0** viol | 1.00× | fails in codegen both ways |
| softrender | p57, 1029 viol | p90, 13 viol | 0.91× | `rc=134` both ways |
| ~~rdb~~ | p26, 602 viol | p53, 1 viol | **0.01×** | **false win — program pruned** |
| ~~amaze~~ | p17, 632 viol | p64, 8 viol | 0.92× | **false win — compiles, then crashes** |

(*rubik is in Group C; listed here for the comparison.)

Two rows are real, verified wins: **sudoku5 and msp_ss go from a runtime
assertion failure (`matching function not found` / `list element type
mismatch` — i.e. FA's residual violations surviving into codegen) to
running correctly.** So the guard is not only distorting this doc's
measurements; on those two it is **causing a miscompile**.

Two rows are traps, and they are why "just raise the constant" is not the
fix: **rdb** "converges" to 1 violation by emitting a 3.2 KB `.c` from a
19 KB source — the `if __name__=="__main__":` guard loses its type, the
whole main body is pruned, and the binary dies on `getter not resolved`;
**amaze** compiles where it previously didn't, and then core-dumps.

Group A (converged both ways) is 56 programs, of which exactly one
(tictactoe) changes at all (p34 → p30, 0 violations either way). 11
directories produce no FA result (data/script dirs, or they fail earlier).

## CENSUS 2026-08-13 — what is actually churning: new edges, or edges losing their mapping? new EntrySets, or new CreationSets?

Per-pass census over Group C plus slow-but-converging controls, counting
edge births, re-bindings, contour mints, CS mints, `copy_AEdge` fan-outs,
and splitter detaches (probe removed). Four findings, all corpus-wide.

**1. CreationSets are never the driver.** `split_css`-side CS mints are
**0** on every program measured (max 2 over a whole run). Every CS mint
comes from `creation_point` and tracks new contours: a fresh EntrySet
gives fresh AVars, and each fresh AVar mints its own CS. **CS growth is
downstream of ES growth, not a cause of it.** This retires the "is it CS
or ES?" question in favour of ES.

**2. `copy_AEdge` (the split fan-out path) fires zero times** anywhere in
the corpus, so the edge population is not growing through split copies.

**3. New edges are a *consequence* of new contours, not an independent
source.** Of the edges born each pass, the fraction whose `from` contour
was itself minted in the same or the previous pass:

| program | edges born/pass | from a contour ≤1 pass old | ES mints/pass |
|---|---|---|---|
| yopyra | 112 | 112 (100%) | 40 |
| loop | 30 | 30 (100%) | 22 |
| rubik | 518 | 469 (91%) | 110 |
| plcfrs | 1189 | 873 (73%) | 131 |
| go | 35 | 22 (63%) | 11 |

So the chain is **new EntrySet → its body's call sites need fresh AEdges
→ each dispatches → possibly more EntrySets**, amplified by the number of
call sites per body (yopyra ≈2.8 edges per new contour, plcfrs ≈9).

**4. Almost every re-binding in the corpus comes from the splitter's
detach path, and that path *cannot* reuse an existing contour.** Route
mix over a whole run (`split-*` = reached from `apply_entry_set_split`,
`flow-*` = ordinary dispatch):

| program | split-fresh | split-pref | pend | lineage-reuse | best | flow-fresh |
|---|---|---|---|---|---|---|
| yopyra | 723 | 1718 | 122 | 6 | 4 | 1 |
| plcfrs | 763 | 563 | – | 4 | 4 | 0 |
| loop | 399 | 112 | – | 8 | 1 | 1 |
| go | 341 | 121 | – | 7 | 13 | 0 |
| bh | 258 | 290 | 245 | 30 | 4 | 0 |
| chull *(converges)* | 444 | 316 | – | 8 | 19 | 0 |
| sat *(converges)* | 408 | 310 | – | 7 | 16 | 1 |

`flow-fresh` is ~0 — ordinary flow-time dispatch essentially never mints
a contour at this stage. The mints are all splitter-driven, and the
reason they are *mints* rather than reuse is structural:

```cpp
static void make_entry_set(AEdge *e, ..., EntrySet *split, EntrySet *preference) {
  if (e->to) { edges.add(e); return; }
  if (check_split(e, edges, split)) return;
  EntrySet *es = nullptr;
  if (!split) { if (find_best_entry_sets(e, edges)) return; }   // SKIPPED during a split
  if (!es) es = preference;
  set_entry_set(e, es);   // fresh contour, or the group's preference
```

**When `split` is non-null, `find_best_entry_sets` is never consulted**,
so a detached edge has no "re-bind me to an existing compatible contour"
option at all: it takes the pending/lineage route, or it gets a brand-new
contour (the first of a group) or that contour as `preference` (the
rest). Each split cycle therefore manufactures contours *by
construction*. Combined with a splitter that re-decides every pass, that
is the growth.

**Slow convergence is the same machinery, not a different one.** chull
and sat show the identical route mix and the identical
mint-edges-follow-contours chain; they differ only in that their bursts
are small (1-11 functions) and the work runs out. Contour minting is
bursty everywhere: many passes mint nothing, then one mints 85-170.
Split-lineage chains stay **shallow** (max depth 1-2), so this is *not*
the unbounded call-context chain
[closed/073](closed/073-teach-splitter-productive-vs-inert-context.md)
closed.

**One program repeats verbatim; the rest do not.** Only `yopyra` re-mints
the *same* burst signature over and over (`__getitem__#2878=6
pEscalar#10435=4 __mul__#1928=4 __mul__#10604=6`, identical at passes 82,
83, 84, 97, 99, 101 — 36% of all its contour mints land in a repeated
burst). Every other program, oscillating or converging, is at 0% by that
measure. So "re-splitting the same thing forever" is a yopyra-specific
finding, **not** the general shape — worth stating explicitly because the
opposite is easy to assume from 033's framing.

**The stable-residual members are a different disease entirely** and are
now filed separately as
[099](099-FA-pending-backedge-avoid-veto-forces-period-2.md): bh, pylife
and linalg have *no* growth at all (0 new edges, 0 new ES, 0 new CS per
pass) — a fixed set of edges swapping between a fixed pair of contours,
forced by `check_split`'s pending-backedge route interacting with the
`avoid` veto. pylife's entire non-convergence is **one edge**. **099 is now partially
fixed (2026-08-13)**: the flip-flop is gone, which converges `loop`
(Group C 8 → 7) and improves pylife (90 → 54 violations) and sudoku4
(160 → 142) — but bh/pylife/linalg still do not converge, because
removing the flip-flop moved them out of the stable-residual shape and
into *this* plan's growth shape (bh now grows ~3 contours/pass where it
used to swap in place). The churn relocated rather than stopped, matching
this plan's own Stage-1 experience ("suppression is not eviction").

## GROWTH MECHANISM 2026-08-13 — yes, ES splits beget ES splits, via `check_split`'s lineage-mint

The census above established *that* growth is EntrySet-driven (new edges
and CSs follow contours). This measures *why* contours keep being minted,
by recording the routing decision of every **newly born** edge — the
earlier route census only covered re-bindings of existing edges, which is
the wrong population for a growth question.

The "map back to the contour you split from, and put a new edge where its
pre-split counterpart went" idea is **already implemented**:
`EntrySet::split` records the parent and `check_split`'s second branch
looks up `e->from->split->out_edge_map.get(e->pnode)` and reuses that
edge's `to`. Where it fires, it works exactly as intended — `rubik`
passes 15/16 route **1155 and 1624** new edges by `lineage-reuse` with
essentially no new contours. It fails two ways:

| program | pass | new edges | lineage_reuse | knot | **lineage_mint** | best | fresh | miss: no_split | miss: nest |
|---|---|---|---|---|---|---|---|---|---|
| yopyra | 93 | 146 | 4 | 74 | **68** | 0 | 0 | 1 | **142** |
| yopyra | 92/94 | 73 | 2 | 37 | **34** | 0 | 0 | 1 | **71** |
| plcfrs | 41 | 5710 | 659 | 0 | 30 | 4235 | **740** | **4961** | 30 |
| plcfrs | 43 | 4662 | 46 | 0 | 0 | 4283 | **333** | **4616** | 0 |
| rubik | 14 | 2356 | 0 | 0 | 0 | 2275 | 81 | **2356** | 0 |
| go | 22 | 172 | 89 | 0 | 1 | 74 | 8 | 107 | 1 |

1. **`no_split` — the parent isn't recorded.** `e->from->split` is null,
   so there is nothing to map back to. Dominant on plcfrs (≈4600-5000 per
   pass), rubik and go. These edges fall through to
   `find_best_entry_sets`, which mostly binds to an existing contour
   (`best`) — but the residue mints (`fresh` 333-740/pass on plcfrs).
   A contour minted at *flow* time (`make_entry_set` with `split == null`)
   never gets a parent, by construction.
2. **`nest` — the parent's target is rejected by display compatibility.**
   Dominant on yopyra: 71-142 candidates per pass enter the
   `split_unique || !edge_nest_compatible_with_entry_set` branch. 073's
   type-identity knot catches about half; **the other half mints a fresh
   contour and sets `e->to->split = ee->to`, extending the lineage
   chain** — 073's own comment calls this "the sole unbounded EntrySet
   generator", and on yopyra it is 34-68 new contours per pass, forever.
   This is 064's phantom method display doing exactly what Stage 0
   predicted it would.

So **yes: on yopyra (the pure growth case) ES splits beget ES splits**,
through the lineage-mint, and the blocker on reusing the pre-split
mapping is the display check, not a missing map.

### Measured dead end — routing the lineage-mint to a bounded display variant does NOT work

The natural repair for (2): instead of minting and chaining, route to
`find_or_make_display_variant(e, ee->to)` — a per-display sibling of the
*same* product, which reuses an existing sibling and, when it mints,
inherits `tes->split` rather than chaining, so the fan-out is bounded by
(product × display), which 073 proved finite. Prototyped behind a flag
and measured (reverted):

| program | before | with display-variant routing |
|---|---|---|
| **loop** | `plh=0` p58, **0** viol | `plh=1` p34, 3 viol — **undoes 099's win** |
| **linalg** | `plh=1` p16, 79 viol | `plh=1` p15, **283** viol, ess 1089→2243 |
| plcfrs | 5494 viol | 6204 viol |
| yopyra | `plh=1` p102, 0 viol | `plh=1` p56, 3 viol, ess 2151→3683 |
| chess | 63 viol | 70 viol |
| sudoku4 | `plh=1` p27, 142 viol | **`plh=0` p33, 26 viol** |

One real win (sudoku4 converges) against several regressions including
undoing `loop`'s convergence. **Net negative — not landed.** Consistent
with this plan's history on this surface: the display is load-bearing in
ways a uniform substitution does not capture, and any fix for (2) has to
distinguish the cases rather than treat them alike.

## GROWTH RE-CENSUSED 2026-08-13, post-[100](100-FA-display-removed-from-contour-identity.md)

The display is out of contour identity, so the mechanism the previous
census found (`check_split`'s lineage-mint, blocked from reuse by
`edge_nest_compatible_with_entry_set`) is **gone**: across the whole
oscillating set, `lineage-mint` and `split-fresh` no longer appear among
*newly born* edges at all. New edges now route almost entirely through
`lineage-reuse` and `best` — both of which reuse an existing contour —
with a small `flow-fresh` residue:

| program | newborn edges (8 passes) | routes |
|---|---|---|
| rubik | 5092 | lineage-reuse 2728, best 2284, flow-fresh 80 |
| chess | 2348 | lineage-reuse 2340, best 5 |
| plcfrs | 3129 | best 2841, lineage-reuse 178, flow-fresh 83 |
| sudoku4 | 1088 | lineage-reuse 1072, pend 16 |

**The minting moved to RE-BOUND edges.** Splitting the route census by
newborn vs re-bound shows where the contours now come from:

```
sudoku4    routes: lineage-reuse=134 pend=2  || REBOUND: split-fresh=2
genetic2   routes: lineage-reuse=42  pend=2  || REBOUND: split-fresh=2
hq2x       routes: (almost nothing)          || REBOUND: split-fresh=2 split-pref=248..261
rubik      routes: lineage-reuse=1626        || REBOUND: split-fresh=201 split-pref=211 best=15
```

`sudoku4` and `genetic2` are the clean specimens: that line is **byte-identical
every pass** — 2 edges detached and given 2 fresh contours, then ~134/42
new edges minted for those contours' bodies, forever, to the pass cap (93
and 53 passes). `hq2x` is the pure-churn extreme: ~250 edges detached and
re-parked every pass with essentially no new edges, for 102 passes.

**The cause is structural and now dominant.** In `make_entry_set`:

```cpp
if (check_split(e, edges, split)) return;
EntrySet *es = nullptr;
if (!split) { if (find_best_entry_sets(e, edges)) return; }   // SKIPPED on the detach route
if (!es) es = preference;
set_entry_set(e, es);                                          // fresh contour, or the group's preference
```

A detached edge is never offered an existing contour: it takes the
pending/lineage route, or it gets a brand-new one. The display machinery
used to mask this; with the display gone it *is* the growth.

Also unchanged from the previous census, and worth restating: **`csSplit`
is ~0 and `copy_AEdge` is 0 everywhere** — CreationSet splitting and split
fan-out still play no part.

### The detach-route reuse experiment — LANDED AS A FLAG (`PYC_HARDREUSE`), and it overturns the premise

Available for further work: `PYC_HARDREUSE=1|2|3` (off by default,
`make_entry_set`; `IFA_DBG_HARDREUSE=1` logs each reuse). Each mode
offers a detached edge an existing contour instead of a fresh one, under
a progressively stricter test:

- **1** — `entry_set_compatibility == INT_MAX` (no penalty of any kind).
- **2** — that **and** a *positive* type match: `edge_type_identical_to_
  entry_set`, requiring every positional argument to be typed on BOTH
  sides and identical. Mode 1 is not that: `edge_type_compatible_with_
  entry_set` only rejects when both sides are non-empty
  (`etype->n && es_arg->out->type->n && ...`), so a candidate whose
  formal-parameter AVar is *bottom-typed at the compared position* is
  vacuously "compatible" — issue 097's mechanism. (Note this is a
  different thing from a **bare** ES, which has no arg AVars at all;
  that one trips the assert, see below.)
- **3** — the same at CreationSet granularity rather than the type-level
  (CS-stripped) view.

Suite: off 265/0, mode 1 **260/6**, modes 2 and 3 **261/5**. Mode 2
recovers `tuple_compare`; CS granularity buys nothing further.

**Why the strictness doesn't help — and a correction.** An earlier draft
of this section blamed mode 1's vacuous-compatibility hole above for the
`defaultdict(int)`/`defaultdict(list)` merge. **That is not supported**:
mode 2 rejects exactly that case and `builtin_type_factory` still fails,
with the same symptom. Logging the reuses (`IFA_DBG_HARDREUSE=1`) shows
what is really happening, and it is the same in three of the four
failures:

```
builtin_type_factory  p40 __str__#1880 es50  -> es193
                      p41 __str__#1880 es193 -> es50
                      p42 __str__#1880 es50  -> es193      ... to the cap
dict_iter_cross_...   p31/32/33  __str__#1880 es66  <-> es99
match_map_star        p28..p33   __str__#1880 es229 <-> es89
```

**Hard reuse manufactures a period-2 flip-flop of its own**, structurally
identical to [099](099-FA-pending-backedge-avoid-veto-forces-period-2.md)'s
and for the same reason: the splitter detaches the edge from contour A,
the `x != split` veto makes the reuse pick sibling B, and next pass it
detaches from B and picks A. It is *inevitable* — if A and B are
type-identical enough for hard reuse to accept, then whichever contour
the edge currently occupies, the other is an equally valid target, and
the veto forces the swap. The `x != split` veto is thus the same design
error 099 found in `check_split`'s pending-backedge route, reproduced in
a second place. It also explains the corpus numbers below far better than
the empty-contour story did: hard reuse mints 099-shaped flip-flops
wherever a function has two mutually compatible contours.

`recursive_polymorphic` is the genuinely *different* failure of the four,
and the informative one for the timing question. The whole run makes just
*three* reuses — `len`, `__getitem__`, `flatten_sum`, all in passes 1 and
4, with no flip-flop — and the test fails on `deep_copy_list`, which is
never reused. The damage is a knock-on: `deep_copy_list`'s two
recursion levels are **both `list`** (list-of-list vs list-of-int) and
both `defaultdict`s are `defaultdict`, so they are type-identical *and*
CS-identical at the compared positions **at the moment the detach route
runs** — the evidence that separates them (element types, per-level CSs)
only arrives later in the pass. Reusing one `__getitem__` contour across
the two levels then re-fuses the element types, exactly the mechanism
Stage 0 documented for the display. So this is issue 097's timing hazard
one level deeper: *no* predicate over the current snapshot can decide
this, because the distinguishing information does not exist yet.

**The corpus result overturns the premise this stage was built on.**
Mode 3 across all 84 examples, against the current baseline:

| | oscillators | total ess | total violations |
|---|---|---|---|
| off | 20 | 30362 | 11384 |
| `PYC_HARDREUSE=3` | **49** | **24471** | **18389** |

It *does* attack the growth — contour count drops 19% — but **oscillators
more than double and violations rise 62%**, and ten programs that
converged (`ant`, `circle`, `collatz`, `fysphun`, `path_tracing`,
`rubik2`, `sha`, `sieve`, `stereo`, `tonyjpegdecoder`) now run to the
hard 102-pass cap with **zero violations** — the pure churn shape.

Two lessons, then. **(a) The `x != split` veto cannot be combined with a
symmetric reuse test** — that combination is a flip-flop generator, in
`check_split`'s pending route (099) and here alike; any reuse rule needs
an asymmetric tie-break (lowest id *including* `split`, a recorded home,
anything that is stable under "which one am I in right now"). **(b) The
fresh mint on the detach route is not waste, it is what makes the split
stick.** Reusing an existing contour
makes the split a no-op, so the splitter re-derives the same decision
every pass — "suppression is not eviction" (this plan's own Stage-1
finding) now measured at corpus scale. The growth this census identified
is therefore the *price of the split taking effect*, not a leak to be
plugged, and the lever is not "reuse instead of mint" but "stop the
splitter needing to re-decide" (Stage 1) or "make the split productive
enough that it does not".

One real bug fell out and is fixed on the flag path: the detach route
sees BARE products (filters + lineage only, no args/rets) that the
flow-time caller never does, and `edge_type_compatible_with_entry_set`'s
`assert(e->args.n && es->args.n)` aborts on them (`kanoodle`).

### Durable type keys (`PYC_TYPEKEY`) — shedskin's model, tried; necessary but NOT sufficient

Premise (author's, 2026-08-13): shedskin does not have this problem
because its contours are bound to particular *types* and those bindings
persist between passes. Can pyc store the compatible types on the
EntrySet and match against that?

**Two enablers were already true**, which makes it cheap: `clear_es` does
**not** clear `EntrySet::filters` (durable per-contour type restrictions
already exist), and `clear_results` clears only `cannonical_setters`, so
`cannonical_atypes` persists and canonical `AType *` pointers stay
comparable by identity across passes. `find_or_make_filtered_entry_set`
is already "find the ES with this type key, else mint one" — just scoped
to the CS-partition path.

**Landed as `PYC_TYPEKEY=1`** (off by default): `EntrySet::type_key`, a
per-position map captured at `complete_pass` from each reached contour's
*converged* formal types and never cleared;
`edge_type_compatible_with_entry_set` matches against it in preference to
the momentary mid-pass `es->args[p]->out->type`. That directly removes
issue 097's "scored against a snapshot taken before this contour's own
callers re-flowed" hazard — the value is now whole-pass invariant.

**Suite: 265/0 — the only experiment in this whole investigation that
does not break anything.** So the substrate is sound.

**But on its own it does not fix the oscillation.** Corpus, against the
current baseline:

| | oscillators | total ess | total violations |
|---|---|---|---|
| off | 20 | 30362 | 11384 |
| `PYC_TYPEKEY=1` | 21 | 30237 | 12423 |

Real wins (`rdb` 3181 → 2297, `sudoku5` 488 → 237, `plcfrs` 2442 → 2189)
and real losses (`sunfish` 199 → 1741, `msp_ss` 514 → 873, `rubik`
166 → 368). Roughly a wash.

**And key-based *routing* still flip-flops** — `PYC_HARDREUSE=4` routes a
detached edge to the contour whose durable key EQUALS its filtered
actuals (a lookup, not a score). `builtin_type_factory`:

```
p42 __setitem__#2330 es88  -> es189
p43 __setitem__#2330 es189 -> es88
p44 __setitem__#2330 es88  -> es189      ... to the pass cap
```

Mode 4 requires exact key equality, so matching `es189` while in `es88`
and `es88` while in `es189` proves **both contours carry the same key**.

**That is the missing half, and it is the real difference from shedskin.**
A durable key makes matching stable *in time*; it does not make it
unique *in space*. pyc happily holds several contours of one function
with identical keys, so a key match is still symmetric, and the
`x != split` veto then forces the same alternation. Shedskin cannot get
here because its contour lookup is **total and canonical**: the type
tuple *names* the contour (find-by-key, create on miss), so two contours
can never share a key and there is no "detach from this one" step to veto
in the first place.

**Concrete next step**, now well-posed: canonicalize contour creation on
the durable key — route every mint through a find-by-key-else-create, the
way `find_or_make_filtered_entry_set` already does for CS partitions —
so duplicate-keyed contours cannot exist. Then the `x != split` veto
becomes unnecessary rather than needing a smarter tie-break, and both
this route's flip-flop and 099's have nothing to alternate between.

### Canonicalization on the durable key (`PYC_CANON`) — built, and the conflict log is the interesting output

Follows directly from the type-key result above: if duplicate-keyed
contours are what let the veto alternate, remove the duplicates. Contour
creation becomes find-by-key-else-create — the type tuple *names* the
contour, as in shedskin.

`PYC_CANON=1|2`, off by default (`IFA_DBG_CANON=1` logs each conflict and
per-pass stats). `EntrySet::canon_key` is stamped at creation and is
immutable, unlike `type_key` which is refreshed each pass.

- **1** — canonicalize, but never hand an edge back to the contour the
  splitter is detaching it FROM. Conflicts are counted and honored (the
  split wins). Suite **259/7**.
- **2** — full canonicalization: reuse even then, so a split that
  disagrees with the canonical key becomes a no-op. Suite **237/32**.

**Scope limit, stated up front:** only `make_entry_set`'s tail mint is
canonicalized. `check_split`'s lineage-mint and
`apply_entry_set_split`'s self-product eviction still create contours
without a key, so duplicates can still arise there. The numbers below are
a lower bound.

**The conflict measurement** — how often the splitter wants to separate
edges that the argument type tuple says belong together (mode 1, so
splits are honored):

| program | osc | dedup rate | conflicts/pass | top conflicting funs |
|---|---|---|---|---|
| hq2x | C | **58%** | **8.1** | `__setitem__` 92, `PIXEL11_20` 35 |
| chess | C | 10% | 6.1 | `__add__` 16, `append` 14, `len` 11 |
| rdb | C | 5% | 4.0 | `__ge__` 7, `len` 7, `__lt__` 7 |
| adatron | – | 11% | 3.0 | `__getitem__` 23/15, `__lt__` 13 |
| msp_ss | C | 7% | 2.8 | `len` 9, `__eq__` 8, `__lt__` 6 |
| bh | C | 10% | 2.5 | `__getitem__` 8, `__sub__` 7 |
| softrender | C | 9% | 2.1 | `__getitem__` 43, `__iadd__` 11 |
| rubik | C | 2% | 1.8 | `__pyc_to_bool__` 6, `__lt__` 5 |
| chull | – | 4% | 1.1 | `__getitem__` 17, `__add__` 6 |

Three things fall out of it:

1. **The conflicting functions are almost entirely container and
   comparison builtins** — `__setitem__`, `__getitem__`, `len`,
   `__lt__`, `__eq__`, `__ge__`, `append`, `__pyc_to_bool__`, `__add__`.
   So **the splitter's separations are mostly not argument-type-driven**:
   it is separating on setters, marks and element CSs, which the argument
   type tuple does not capture at all. Canonicalizing on argument types
   is therefore structurally at odds with what the splitter is doing —
   which is why mode 2 costs 32 tests rather than a handful.
2. **Conflicts are not a property of the oscillators.** `chull` (1.1/pass)
   and `adatron` (3.0/pass) converge; `go` (0.6/pass) does not. The rate
   alone does not predict convergence, so "the splitter disagrees with
   the canonical key" is normal behavior, not the pathology.
3. **`hq2x` is the exception that fits.** It is the pure-churn oscillator
   (102 passes, ~250 edges detached and re-parked per pass, ~1 new edge)
   and it has both the highest conflict rate (8.1/pass) *and* by far the
   highest dedup rate (**58%** — canonicalization finds an existing
   same-keyed contour for well over half its keyed mints, versus 2-12%
   everywhere else). That is a strong, specific signal that hq2x's churn
   really is duplicate-contour manufacturing, and it is the right first
   subject for the canonicalization work.

**Where that leaves the idea.** Canonicalization by *argument type tuple*
cannot be the whole naming scheme, because most of what the splitter
separates on is not in that tuple. Making it total would need the
contour's name to include the other split dimensions (setter classes,
marks, element CSs) — i.e. the name has to be whatever the splitter would
have split on, which is the same "stable per-group identity" Stage 1 (ii)
has always been about, now with a concrete measurement of what has to go
into it and a program (`hq2x`) where the current key already explains
most of the churn.

### WHICH SPLIT STAGES CAUSE THE OSCILLATION (2026-08-13)

Per-stage attribution of the steady-state churn: every edge detach
(`x->to = 0`) and every contour mint is tagged with `cur_split_stage`,
summed over the last 10 passes (`IFA_DBG_STAGE=1`, landed).

**Only two of the nine stages produce it: `TYPE_CONFLUENCE`
(`split_ess_for_type`) and `MARK_TYPE` (`split_ess_for_mark_type`).**

| program | osc | detaches / 10 passes | attribution |
|---|---|---|---|
| hq2x | C | 2755 | **MARK_TYPE 100%** (2755 det, only 24 mint) |
| rdb | C | 2022 | **TYPE_CONFL 100%** (2022 det, 712 mint) |
| rubik | C | 1763 | TYPE_CONFL 98%, MARK_TYPE 1% |
| amaze | C | 1295 | TYPE_CONFL 100% |
| msp_ss | C | 1119 | TYPE_CONFL 100% |
| sudoku5 | C | 702 | TYPE_CONFL 100% |
| plcfrs | C | 631 | TYPE_CONFL 100% |
| linalg | C | 487 | TYPE_CONFL 100% |
| chess | C | 419 | **MARK_TYPE 100%** (419 det, 301 mint) |
| tictactoe | C | 358 | TYPE_CONFL 100% |
| softrender | C | 248 | TYPE_CONFL 100% |
| timsort | C | 97 | MARK_TYPE 57%, TYPE_CONFL 42% |
| mastermind2 | C | 88 | MARK_TYPE 51%, TYPE_CONFL 48% |
| sat | C | 88 | TYPE_CONFL 82%, MARK_TYPE 12%, SETTER 4% |
| bh | C | 50 | MARK_TYPE 80%, TYPE_CONFL 20% |
| pylife | C | 30 | MARK_TYPE 100% |
| sunfish | C | 29 | TYPE_CONFL 79%, SETTER_OF_SETTER 20% |
| genetic2 / sudoku4 | C | 20 | MARK_TYPE 100% |

Converging programs show the *same two stages* at lower volume (`chull`
15 det/10p TYPE_CONFL; `adatron` 34 TYPE_CONFL; `yopyra` 61 MARK_TYPE
80%; `loop` 57 MARK_TYPE 87%) — more evidence this is one mechanism at
different amplitudes, not a distinct pathology.

**Essentially nothing comes from the other seven stages**: `SETTER` 4
(sat), `SETTER_OF_SETTER` 6 (sunfish) / 2 (loop), and **zero** from
`MARK_SETTER`, `MARK_SETTER_OF_SETTER`, `VIOLATION`, `PER_CS_RECEIVER`
and `CSM_ELEMENT_CS`.

> **Caveat, and it matters:** `run_split_stages` is a first-stage-wins
> cascade — every stage is gated on `if (!analyze_again)`. So stages 3-9
> only run at all on a pass where TYPE_CONFLUENCE *and* MARK_TYPE both
> find nothing, which in an oscillating program is never. Their ~0
> contribution is therefore partly **starvation**, not evidence that they
> are well behaved. What the table does establish is that the churn which
> keeps these programs from converging is entirely stages 1-2's, and that
> fixing those two is both necessary and sufficient to let the rest even
> run.

**The two stages fail differently**, which is a useful split of the
problem:

- **`MARK_TYPE` tends to pure re-assignment.** `hq2x` detaches 2755 edges
  per 10 passes and mints **24** contours — it is moving the same edges
  around, not growing. `pylife`, `genetic2` and `sudoku4` are 1:1
  det:mint at low volume. This is the assignment-churn disease.
- **`TYPE_CONFLUENCE` tends to detach *and* mint.** `rdb` 2022:712,
  `rubik` 1737:996, `amaze` 1295:497 — roughly a third to a half of its
  detaches manufacture a contour. This is the growth disease.

`chess` is the exception to that split (MARK_TYPE at 419:301, mint-heavy),
so the correlation is a tendency, not a rule.

**Consequence for this plan.** Both remaining shapes now have a named
owner. `hq2x` — already singled out by the canonicalization census for
its 58% duplicate-key rate — is a *pure `MARK_TYPE`* problem, which makes
it the sharpest available subject: one stage, one disease, no growth to
confound it, and a measurement showing over half its mints are duplicates
of contours that already exist.

### hq2x, and why type marks and canonicalization are mutually exclusive (2026-08-13)

The question that opened this: *why do we need type marks at all, if we
are canonicalizing the function contours?*

**They answer contradictory questions, so you cannot have both.**
Canonicalization says a contour's name IS its argument type tuple — two
edges with the same key go to the same contour. `MARK_TYPE` exists
specifically to separate two edges that *have* the same key. From
`build_type_mark`'s own comment and IFA.md §6.2: *"two contributors that
share a type but differ in their origin (different `mark_map` entries)
still split… this is how IFA handles the recursion-meets-polymorphism
case without falling back to k-CFA."* A split on origin-at-equal-type is
by construction unnameable in a type-tuple scheme. That is exactly what
`PYC_CANON`'s `CANON-CONFLICT` counter has been counting.

**Why the original design needed them.** pyc names contours by tuples of
argument type **sets** — `edge_canon_key` builds one hash-consed `AType*`
per position, and `collect_type_confluence` fires on
`type_diff(av->in->type, x->out->type) != bottom`. Inside a dataflow
cycle every contributor carries the same union, so that test goes blind
and plain type splitting cannot separate anything. Marks (min distance
from a generating AVar) restore an ordering the union destroyed.
Shedskin never needs the mechanism because CPA names contours by
*singletons*: there is no union to hide in, so recursion is separated
structurally by the product at the call site.

**But that is not what marks are doing here.** New probe
`IFA_DBG_KEYSPACE=1` reports, per function per pass, `ess` (contours IFA
built) vs `setkey` (distinct tuples of argument type sets — what
canonicalization would name by) vs `cpakey` (distinct tuples of single
CreationSets over all its call edges — what shedskin's dcpa would build).

hq2x, `__setitem__#2330` — the function the canon log fingered:

| pass | ess | setkey | cpakey |
|---|---|---|---|
| 7 | 20 | 8 | 17 |
| 12 | 22 | 8 | 17 |
| 30 | 94 | 8 | 17 |
| 60 | 190 | 8 | 17 |
| 100 | 285 | 8 | 17 |

**The type keyspace is stationary from pass 7 onward while the contour
count grows 14×.** Every one of those contours past the 17th is a
distinction no type-tuple naming — set-based *or* cartesian-product —
can express.

Sharper still, the `PIXELxx_yy` family (~24 one-line functions,
`def PIXEL00_20(rgb_out, pOut, BpL, c): rgb_out[pOut] = Interp2(...)`):

| fun | ess | setkey | cpakey | call sites in source |
|---|---|---|---|---|
| PIXEL00_20 | 36 | **1** | **1** | 38 |
| PIXEL01_20 | 36 | **1** | **1** | 38 |
| PIXEL00_0 | 28 | **1** | **1** | 29 |

These are **monomorphic** — one argument type tuple, no polymorphism, no
recursion — and `MARK_TYPE` builds **exactly one contour per call site,
adding exactly one per pass** from pass 12 until it saturates at the call
count. That is 1-CFA by accretion, discovered one call site at a time,
paid for at 36× code duplication, on a function with a single type. The
mark distance is a proxy for depth-from-generator, so splitting changes
the distances, which manufactures new mark differences, which drives more
splitting: this is the concrete form of "splits beget splits" for
`MARK_TYPE`.

Whole-program at pass 101: **ess=1579, setkey=188, cpakey=521** — 1065
contours beyond anything CPA would name.

### `PYC_NOMARK` — landed as a flag, and marks are a net loss on this corpus

`PYC_NOMARK=1` skips `MARK_TYPE`; `=2` also skips
`MARK_SETTER`/`MARK_SETTER_OF_SETTER`. The `VIOLATION` stage's
`split_with_type_marks(av, SPLIT_DYNAMIC)` is deliberately left armed, so
marks survive as *demand-driven repair* when a type violation actually
appears — just not as a prophylactic.

hq2x with `PYC_NOMARK=1`: **102 passes → 14** (and it stops hitting the
stall guard: `pass_limit_hit` 1 → 0), ess 1664 → 620, analysis 374s → 35s,
generated C 1192 KB → 583 KB, **0 violations both ways**, and the compiled
binary's output is byte-identical to baseline except the benchmark's own
`TIME` line. `PYC_NOMARK=2` is identical to `=1` here — setter-marks were
contributing nothing.

84-program corpus sweep, marks-off vs marks-on:

| metric | marks on | marks off | |
|---|---|---|---|
| total analysis time | 1600s | 1183s | **−26%** |
| total EntrySets | 30362 | 26741 | **−12%** |
| total generated C | 16.57 MB | 15.63 MB | **−5.6%** |
| exit-code changes | — | **mastermind2 1 → 0** | one program starts compiling |

Passes drop nearly everywhere: hq2x 102→14, sudoku4 93→31, webserver
49→22, yopyra 46→23, chess 39→15, pygasus 27→16, neural1 46→21.

Violations, 12 better / 5 worse:

- **better**: chess 331→**0**, mastermind2 554→60, pylife 54→**0**,
  bh 2→**0**, sudoku3 143→111, timsort 61→40, neural1 53→39,
  dijkstra2 10→6, go 161→146, sudoku4 30→26, rubik 166→164, plcfrs 2442→2412
- **worse**: softrender 1043→1272, webserver 0→8, kmeanspp 2→8,
  sat 401→417, chull 0→2

`test_pyc.py` with `PYC_NOMARK=1`: **265 passed / 14 expected fails / 0
failed / 4 skipped** — identical to baseline, both backends.
`ifa --test` 58/0.

**Reading.** Marks do buy real precision on five programs, so they are not
dead code — but on this corpus they cost more than they earn, and the
programs where they cost the most are exactly the non-convergent ones. The
five regressions are the actual specification of what a replacement has to
recover; they are the next thing to look at, not the flag's default.

> Do NOT read "violations down" as "precision up" on its own — see the
> `rdb` trap recorded above. The corroboration here is that the exit-code
> change goes the right way (mastermind2 starts compiling, and its C grows
> 46 KB → 286 KB), generated C shrinks without any program losing output
> fidelity, and the full test suite is unchanged.

### THE TYPE_CONFLUENCE PROGRAMS: it is not self-fuelling, it is the responder (2026-08-13)

`MARK_TYPE` turned out to build contours no type-tuple can name. The
`TYPE_CONFLUENCE` half is the opposite in every respect, and the cause is
architectural rather than algorithmic.

**First, its contours ARE nameable.** `IFA_DBG_KEYSPACE` at the last pass:

| program | ess | setkey | cpakey |
|---|---|---|---|
| rdb | 1200 | 638 | 12873 |
| plcfrs | 742 | 515 | 35597 |
| msp_ss | 1073 | 450 | 765 |
| linalg | 468 | 288 | 410 |
| tictactoe | 389 | 271 | 337 |

`ess ≈ cpakey`, and on rdb/plcfrs *far below* it — IFA builds fewer
contours than cartesian-product naming would. Nothing like `hq2x`'s
287-contours-for-8-keys. And `IFA_DBG_INCOMPAT` shows **every single
incompatibility verdict comes from the `arg` clause**: `ret` and `retn`
are exactly 0 on all 11 programs, tens of thousands of verdicts each. So
this stage separates edges on precisely the thing a canonical naming
scheme keys on. (An earlier reading of this table that had the `rets`
clause dominating was a column-offset mistake in the aggregation; the
"contour identity depends on its own result" hypothesis it suggested is
dead.)

**Second, most of them were never oscillating.** Of 11, eight reach
`dup_es=0` with no edge churn before they stop — but those trailing quiet
passes are the stall guard's doing, not convergence: once
`pass_limit_hit` is set, `extend_analysis` never calls `run_split_stages`
again. The splitter is gagged, not finished. This matches the Group B/C
re-base above; the genuinely non-convergent `TYPE_CONFLUENCE` members are
**rubik, plcfrs, linalg, go**.

**And `PYC_NOMARK` moves rubik out of Group C entirely** (guards off):

| program | guards off | guards off + `PYC_NOMARK=1` |
|---|---|---|
| rubik | p102, 1729 viol, ess 1456 | **p27, `pass_limit_hit=0`**, 104 viol, ess 728 |
| plcfrs | p102, 1353 viol | p102, **62** viol |
| linalg | p102, 78 viol | p102, **31** viol |
| go | p102, 103 viol | p102, 107 viol |

**Third, the stall guard's oscillation test is measuring the wrong thing.**
The guard advances `stall_passes` only on non-improving passes that
"re-derived" a split (`dup_split_attempts > 0`). New `REDERIVE` logging
breaks that counter down by kind:

| | rdb | rubik | linalg | amaze | msp_ss | plcfrs | sat | softrender | chull | sudoku5 | tictactoe |
|---|---|---|---|---|---|---|---|---|---|---|---|
| ROUTE | 119 | 35 | 70 | 94 | 67 | 43 | 43 | 34 | 12 | 26 | 19 |
| FILTER | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |

**100% ROUTE, zero FILTER, everywhere.** Every "re-derivation" is the
ledger finding the product it recorded on an earlier pass and routing the
edge back into it — the anti-oscillation machinery *succeeding*. The guard
reads its own recovery mechanism as evidence of divergence.

#### The actual mechanism: cascade serialization

`run_split_stages` gates every stage on `if (!analyze_again)`, so a later
stage runs only on a pass where every earlier stage found nothing. The
later stages mint **CreationSets**, which widen types wherever they flow,
which re-opens type confluences, which restarts `TYPE_CONFLUENCE` — and
`TYPE_CONFLUENCE` must go fully quiet again before the later stage gets
another turn. The two can never make progress on the same pass; they are
forced to alternate.

`linalg` (guards off, `PYC_NOMARK=1`) is the specimen — an **exact
period-10 limit cycle**, repeating verbatim from pass 64 to the cap:

```
p=64  SETTER      det=0  mint=0  reuse=0  csmint=2   ess=739 css=1832
p=65  TYPE_CONFL  det=4  mint=4  reuse=0  csmint=0   ess=739 css=1834
p=66  TYPE_CONFL  det=14 mint=8  reuse=6  csmint=0   ess=743 css=1853
p=67  TYPE_CONFL  det=15 mint=10 reuse=5  csmint=0   ess=751 css=1862
p=68  TYPE_CONFL  det=8  mint=2  reuse=6  csmint=0   ess=761 css=1864
p=69  TYPE_CONFL  det=4  mint=3  reuse=1  csmint=0   ess=763 css=1869
p=70  TYPE_CONFL  det=4  mint=0  reuse=4  csmint=0   ess=766 css=1875
p=71  TYPE_CONFL  det=3  mint=3  reuse=0  csmint=0   ess=766 css=1875
p=72  TYPE_CONFL  det=3  mint=2  reuse=1  csmint=0   ess=769 css=1877
p=73  TYPE_CONFL  det=2  mint=2  reuse=0  csmint=0   ess=771 css=1878
p=74  SETTER      det=0  mint=0  reuse=0  csmint=2   ess=773 css=1878   <- repeats
```

`SETTER` mints **2 CreationSets and moves no edges**; `TYPE_CONFLUENCE`
then spends nine passes re-partitioning around them, minting 34 contours
and 46 CreationSets; then `SETTER` fires again. Identical numbers every
cycle. `KEYDRIFT` repeats too (`p=64` and `p=74` both `grew=13 shrank=29`).
Net per cycle: **+34 ess, +46 css, zero progress on the 31 residual
violations.** That is the growth and the oscillation in one object.

`plcfrs` and `go` have the same alternation without the exact periodicity
(`plcfrs`: `SETTER csmint=6` → 4-pass storm → `SETTER csmint=2` → storm →
`SETTER_OF_SETTER`; `go`: `CSM_ELEM_CS csmint=48` → `TYPE_CONFL det=82` →
7-pass storm → `SETTER csmint=6` → …). The Group B programs are the
one-shot version of the same thing: a single `CSM_ELEM_CS` event costing
3-8 passes of storm, then done (`rubik` p15 `csmint`→ `det=983 mint=661`
at p16; `seen` jumps 180 → 1701 in one pass).

#### This corrects the stage-attribution table above

That table metered only **edge** detach/mint, so stages whose entire
effect is to mint CreationSets scored ~0 and were read as innocent. Adding
a `csmint` column (`IFA_DBG_STAGE`) shows `SETTER`, `SETTER_OF_SETTER` and
`CSM_ELEMENT_CS` minting CreationSets with *zero* edge churn — and those
mints are what drives `TYPE_CONFLUENCE`. **`TYPE_CONFLUENCE` mints no
CreationSets at all.** It is the downstream responder, not the cause.

Also fixed while measuring: `cur_split_stage` was never reset after
`run_split_stages`, so every contour and CreationSet the *next pass's
flow* created was attributed to whichever stage ran last. That is what
made the `reuse` column read in the thousands for stages that re-bind
nothing (`CSM_ELEMENT_CS reuse=2280` on rubik was flow). With the reset,
linalg's storm passes drop from `reuse=39/18/11` to `0/6/5`.

#### The re-derivation is entirely the `v>0` SELF-PRODUCT case

Correcting the ROUTE/FILTER table above: it covered only two of the three
sites that bump `dup_split_attempts`. With the third (the "DUP group"
record site in `apply_entry_set_split`) instrumented too:

| | rdb | rubik | sudoku5 | linalg | tictactoe | amaze | msp_ss | plcfrs | sat | softrender | chull | sunfish |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| ROUTE | 119 | 35 | 26 | 70 | 19 | 94 | 67 | 43 | 43 | 34 | 12 | 55 |
| GROUP | 43 | 14 | 12 | 17 | 11 | 22 | 26 | 10 | 12 | 23 | 11 | 6 |
| FILTER | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |

So it is ~75% ROUTE (recovery) / ~25% GROUP, not 100% recovery. And the
GROUP quarter is sharply characterised — on **every** program:

- **100% of GROUP re-derivations produce a DIFFERENT contour** than the
  ledger recorded (`recorded != now`, zero exceptions), and
- **100% of them have `recorded == es`** — the recorded product for the
  key *is* the contour being split.

That holds on all **12** programs measured, `sunfish` included — which
matters, because `sunfish` is the subject named below. That is exactly the
self-product case increment 1b handles, gated on
`nviol_this_pass == 0`. Since every program here carries residual
violations, the gate is closed and the fallthrough mints a fresh contour
every pass, forever (`__ge__#1851`: es 352→550, 353→551, 354→552, … one
new contour per source contour per pass). **The `v>0` self-product is not
a corner case of the `TYPE_CONFLUENCE` re-derivation — it is all of it.**

#### `PYC_SELFPROD` — two discriminators tried, both break the cycle, neither is sound

Landed off by default, as a recorded negative result.

- **`=1`** — extend the eviction to `v>0`, but evict only the `stay_edges`
  whose type at the split position is **disjoint** from the group's
  partition, on the theory that a disjoint edge can never belong to the
  recorded group.
- **`=2`** — keep the group in `es` and evict **nothing**, dropping the
  mint without re-homing anything.

On `linalg` (guards off, `PYC_NOMARK=1`), both **break the period-10 limit
cycle**:

| | passes | `pass_limit_hit` | violations | ess | css |
|---|---|---|---|---|---|
| base | 102 | 1 | 31 | 871 | 2015 |
| `=1` | **55** | **0** | **3** | 570 | 1623 |
| `=2` | 79 | **0** | 495 | 724 | 1877 |

But neither survives the corpus (`=1`, shipped guards, vs baseline):
exit codes **sunfish 124→0** (a 900 s timeout becomes a 47 s compile),
mastermind2 1→0, tictactoe 1→0 — against **adatron 0→1, dijkstra 0→1,
genetic2 0→1, sudoku2 0→139** (segfault; its `.c` drops 252 KB → 57 KB,
i.e. the program is pruned). Violations rise on ~25 programs, several from
0 (sudoku2 0→205, rubik2 0→101, pygmy 0→90, genetic 0→75). Total ess
+5.3%, generated C +8.6%. `=2` is worse still: on linalg alone, violations
31 → 495 — leaving the group in `es` without evicting the complement keeps
`es` polymorphic, which is 065's original "keep the group in es"
regression (dijkstra2 37→605) reproduced.

**Why `=1` fails, stated precisely so it is not retried:** type sets only
grow across passes, so "disjoint from `part` on this pass" does *not*
imply "disjoint from `part` at the fixed point". An edge evicted while
temporarily disjoint acquires one of `part`'s CreationSets a pass later
and is then stranded in the complement contour. That is the same
mis-homing the 2026-07-30 attempt hit (amaze 884→915, linalg 170→187),
reached by a different route — the disjointness test is not a
stale-vs-valid discriminator, it is just a narrower version of the same
unsound eviction.

What the pair does establish: **the self-product mint is the growth**, and
removing it is sufficient to break the cycle in both directions. The open
problem is unchanged and now much better bounded — a *stable* criterion
for whether a recorded `home == es` is still valid while types widen. The
durable type key (`PYC_TYPEKEY`, landed since 1b was written) is the
obvious substrate: it is captured post-convergence, so it is whole-pass
invariant, unlike the mid-pass `->out->type` both attempts tested against.
That comparison has not been built.

`sunfish` is the sharpest subject for it: its 900 s timeout is entirely
this mechanism, and it is the one program where removing the mint is an
unambiguous win.

#### The durable key as the discriminator — and the property it has to test is LOCAL convergence

Tried on `sunfish`, two readings of "is the recorded `home == es` still
valid", both using `EntrySet::type_key` (captured in `complete_pass`,
after the flow fixpoint, hence whole-pass invariant — unlike the
`->out->type` modes 1/2 and the 2026-07-30 attempt tested against):

- **`=3`/`=4` — is `es` still exactly the recorded partition's home?**
  `es->type_key.get(avpos) == part`. **Refuted.** It fires almost never
  (linalg: 2 VALID / 14 stale), and on `sunfish` both modes are
  *byte-identical to baseline* — same rc=124 timeout, same
  `final_pass=22 violations=199 ess=540 css=1603`. The test is
  near-vacuous because `es` is being split precisely when its union has
  grown past any single group's partition, so equality with `part` is a
  coincidence, not the invariant.

- **`=5` — has `es` LOCALLY converged?** `es->key_hash[0] ==
  es->key_hash[1]`: the contour's durable type key is identical on two
  consecutive passes. **This is the one.** It fires 14/15 on linalg, the
  near-inverse of `=3`.

The insight `=5` encodes: **1b's `nviol_this_pass == 0` gate is a
whole-program proxy for a per-CONTOUR property.** A contour whose key has
stopped moving has settled, so its self-product is a stable flip-flop
(pygmy's case, where eviction is safe) rather than a union still widening
(amaze/linalg's case, where it mis-homes) — regardless of whether the rest
of the program still has violations. Global convergence was never the
requirement; it was just the only stable signal available before the
durable key existed.

`sunfish`, the subject:

| | rc | secs | passes | `pass_limit_hit` | violations | ess |
|---|---|---|---|---|---|---|
| baseline | **124** (timeout) | 1200 | 22 | 1 | 199 | 540 |
| `=1` | 0 | 44 | 36 | **0** | 11 | 583 |
| `=3`, `=4` | 124 (timeout) | 1200 | 22 | 1 | 199 | 540 |
| **`=5`** | **0** | **43** | 36 | **0** | **11** | 582 |

`=5` reaches `=1`'s result (their generated C differs by 2 lines — two
`len` contour ids swapped) **without `=1`'s corpus damage**. 84-program
sweep, shipped guards, vs baseline:

| | mode 1 | **mode 5** |
|---|---|---|
| exit-code regressions | adatron, dijkstra, genetic2, sudoku2 (**4**) | **none** |
| exit-code improvements | sunfish, mastermind2, tictactoe | **sunfish 124→0, tictactoe 1→0** |
| programs with more violations | ~25, several from 0 | 4 (msp_ss 514→617, softrender 1043→1137, timsort 61→66, rdb 3181→3183) |
| programs with fewer | 5 | 6 (**tictactoe 137→0**, sunfish 199→11, sudoku5 488→390, rubik 166→120, sat 401→364, amaze 693→689) |
| total ess | +5.3% | **+1.0%** |
| total generated C | +8.6% | **−0.9%** |
| total analysis time | −52% | **−54%** |

Two programs newly converge *naturally* (`pass_limit_hit` 1→0):
**tictactoe** (48 passes, **0 violations**, `.c` 253 KB→252 KB — the
program is not lost) and **sunfish**. `test_pyc.py` **265/14/0/4** on both
backends under `PYC_SELFPROD=5`, identical to baseline; `ifa --test` 58/0.

**Landed default-on 2026-08-14** (`PYC_SELFPROD` still overrides;
`PYC_SELFPROD=0` restores the pre-074 shape exactly — verified on
tictactoe: rc=1, 137 violations, `final_pass=25`, byte-identical to
baseline). The default sweep reproduces the `=5` sweep above; suite
265/14/0/4 both backends, `ifa --test` 58/0.

Two things remain open, neither a blocker for the default:

1. **`msp_ss` regresses**: violations 514→617, and it now fails *earlier*
   and differently — pyc aborts in `sizeof_element of non-container type
   'int64' (in __add__)`, i.e. pyc [issues/018](../../issues/018-dict-mixed-key-types-boxing-failure.md)'s
   container-method-against-a-scalar, where baseline got as far as
   emitting C that clang then rejected. (Its `.c` dropping 843 KB→116 KB
   is that earlier abort's partial output, **not** a pruned program — the
   two configs fail at different stages, so the sizes are not comparable.
   Worth stating explicitly because a `.c` collapse is normally the
   losing-the-program trap.) `softrender` +94 violations is the other one
   to explain.
2. **Neither newly-compiling program actually runs.** `sunfish` aborts on
   `matching function not found` (its 11 residual violations reaching
   codegen). `tictactoe` is diagnosed in full below.

##### tictactoe: what the newly-exposed failure actually is

It compiles at **0 violations** and aborts on `list element type
mismatch`. This is *not* a regression — it is pyc
[issues/035](../../issues/035-list-element-cast-salvage-guard-and-set-item-union.md)'s
pre-existing int/float list gap, and the change only moved which site is
reached first.

**Source.** `tictactoe.py:103` builds `scores = [self.edge * [0] for i in
range(self.edge)]` — inner lists of `int64`. Then two kinds of in-place
update land in the same slots:

```python
 97:  scores[rown][coln] += 15 * sig(...)   # sig() returns float
100:  scores[rown][coln] += 15 * fields.count(...) / float(self.edge)
118:  scores[rown][coln] += 1               # int
126:  scores[rown][coln] += 1
```

So the element type becomes `int64 ∪ float64`. Two scalars of different
`num_kind` have no common scalar C representation, so the element
degrades to pointer-representable (`_CG_void`), and every store of a
scalar into it trips `cg.cc`'s issue-035 guard
(`(e->num_kind != 0) != (val->type->num_kind != 0)`), which emits the
established runtime-assert rather than invalid C.

**Where.** 11 asserts in the generated C: 8 in `rectBoard::doRow` (lines
97/100), 2 in `rectBoard::makeAImove` (lines 118/126) — and 1 in
`list::append`, reached from `set::add` → `self._items.append(v)`. That
last one is the "set was just the first victim" observation from issue
035 seen directly: FA emits **19** distinct `list::append` contours here,
and **10 of them take a boxed `_CG_any` receiver** while being
specialized on a *scalar* value type. `_CG_f_2533_204(_CG_any,
_CG_int64)` is one of those — value specialized to `int64`, receiver
still boxed, so the element slot is pointer-representable. That is the
container-method-vs-element-type gap this cluster tracks
([075](075-FA-element-cs-method-split-idempotent-plan.md)), not anything
the self-product change introduced.

**What changed vs baseline.** Baseline reached the *read* side first —
`t54 = (_CG_any)((_CG_ps12800)t62)->e1;`, casting a `_CG_float64` field
to a pointer — which has **no** guard, so clang rejected it and pyc
exited rc=1 with 137 violations. With the self-product fix, FA converges
to 0 violations and the first site reached is the *write* side, which
does have the guard, so the program compiles and asserts at run time. Same
underlying defect, different face: **the convergence fix removed 137
violations and exposed a codegen gap that was always there**.

*Read-path guard added 2026-08-14* — see pyc
[issues/035](../../issues/035-list-element-cast-salvage-guard-and-set-item-union.md)'s
"The READ side" section. `P_prim_index_object`'s constant-index record
branch now applies the writer's `num_kind` test, so the unguarded cast
becomes the established runtime assert: `tictactoe` under
`PYC_SELFPROD=0` goes rc=1 → rc=0. The sibling non-record read branch
deliberately keeps no guard — it casts storage to the *destination* type,
which is a legitimate reinterpretation; guarding it cost 39 suite
failures.

What is settled: **the durable type key is the right substrate, and
per-contour key stability is the discriminator the `v>0` self-product
needed.** That was the open problem at the top of this section.

#### Where this points

The target is no longer "make `TYPE_CONFLUENCE` idempotent". It is
**let CS-minting and type-partitioning converge together instead of
alternating** — either by running the CS-minting stages to their own fixed
point before type partitioning starts, or by dropping the first-stage-wins
gate for stages that move no edges (they cannot violate the unflowed-
contour hazard the gate exists to prevent, since they detach nothing).
The second is much the smaller change and is directly testable: on
`linalg` it should collapse a 10-pass cycle to one pass.

Secondary, and independent: **the stall guard should not count ledger
ROUTE recoveries as re-derivation.** `dup_split_attempts` conflates
"re-split into a new contour" (real churn, and it never happens here) with
"re-routed to the recorded product" (the fix working). Splitting the
counter would stop the guard firing on programs that are converging, which
is what causes the `sudoku5`/`msp_ss` miscompiles recorded above.

### Two earlier dead ends on that cause (both reverted)

1. **Let `find_best_entry_sets` run on the detach route, vetoing only
   `split`.** Semantically the split's claim is "not this contour", not
   "no contour at all". **59 test failures.** The soft score
   (`val -= 4` for a type-incompatible candidate) re-merges precisely
   what the split just separated — the same hazard
   [closed/073](closed/073-teach-splitter-productive-vs-inert-context.md)
   hit on `match_seq`.
2. **Reuse only on a HARD type match** (`entry_set_compatibility ==
   INT_MAX`, i.e. no penalties at all), still vetoing `split`. Much
   closer — **6 failures** — but they include `recursive_polymorphic` and
   `match_map_star`, exactly this plan's Stage 0 canaries, plus a hard
   compile failure (`builtin_type_factory`). So even exact type identity
   is not sufficient to show a contour is not what the split is
   separating: the recursion levels this plan has always tripped over are
   type-identical and must still be kept apart by something else.

**What that leaves.** The detach route needs a *positive* reason to reuse
a contour, not merely the absence of a type conflict — i.e. evidence that
this edge belongs with that contour's group. That is the stable
per-group/creation-site identity Stage 1 (ii) was always about, now with a
much sharper target than when it was written: it only has to serve the
detach route in `make_entry_set`, and the thing it must not merge is
recursion levels.

### What the re-base changes about this plan

1. **The target set is 8, not 17**, and half of it is a *stable residual*
   rather than a descent — a much sharper signal for the dup-category
   and self-product work than the mixed set the 2026-07-30 tables used.
   Those tables (`es_self`/`es_route`/`es_othermint`/`cs` per program)
   should be re-taken over Group C only.
2. **A separate lever exists, and it is currently causing miscompiles:
   the guard's own termination/re-arm heuristic.** `sudoku5` and `msp_ss`
   compile to *crashing* binaries today and to *correct* ones when the
   descent is allowed to finish — no splitter change required. That makes
   this a correctness issue, not just a precision one, and it is a
   smaller, better-isolated problem than the splitter core.

   It is **not** a constant to bump. Raising the limits regresses four
   programs, in three distinct ways: `rdb` and `amaze` "converge" by
   losing the program (rdb emits 1% of its former C and crashes; amaze
   compiles and core-dumps), `yopyra` goes `rc=0 → rc=1`, and `rubik`'s
   violations grow 10× (165 → 1730) with a 2.4× larger `.c`. What is
   needed is a better *progress* signal than "strictly improved on best in
   the last 8 re-deriving passes" — one that recognizes a plateau which
   still precedes a collapse (softrender sat at 429 for ten passes before
   dropping to 13) without licensing the runaway cases.

   Ready-made test set for any such change: **must improve** {sudoku5,
   msp_ss}; **must not regress** {rdb, amaze, yopyra, rubik}; **must stay
   byte-identical** {chess, timsort, genetic2, pylife, bh, loop, and the
   56 Group-A programs}.
3. **The guard is load-bearing for escaping a flip-flop.** yopyra with
   guards off freezes in exactly the `ess=820 css=1590↔1591 viol=36`
   period-2 state documented below and never leaves; with the shipped
   guard, firing suppresses the splitter, the flip-flop stops being
   driven, and the re-arm lets it escape to 0 violations. Any redesign of
   the guard has to preserve that escape, and it is a hint about the
   flip-flop's own mechanism: it is *driven* by the splitter re-deciding,
   and stops when the splitter stops.

Wall time with guards off stayed modest (most Group C members 8-15 s;
rubik 103 s, chess 61 s, plcfrs 51 s at 6-way parallelism), so the hard
cap, not cost, is what bounds these runs.

**3. One premise in Stage 1 was false when written, and is true now.**
The "Lifecycle facts" paragraph under Stage 1 argues that the ES side
needs no new keying map because "`clear_edge` clears an edge's flow
(args/rets/filters) ... so the ledger's `group_signature` (arg/ret types)
is *already* a stable key when types have converged". `clear_edge` did
not in fact run on every edge — that is exactly 098 — so
`group_signature` was being computed partly from *other passes'* argument
types on the edges that escaped the reset. The conclusion may well still
hold, but it was not established by that argument; it is worth re-deriving
now that the premise is actually true.

**New canary: `yopyra`** replaces pygmy as the pure case (pygmy converges
at 43 passes, `plh=0`). yopyra hits the *hard* cap at 102 passes with **0
violations**, so there is no union confound at all, and its trajectory
shows the textbook signature twice:

```
p37 ess=820  css=1591 viol=36 dup=1     p57 ess=1296 css=1900 viol=25 dup=2
p38 ess=820  css=1590 viol=36 dup=2     p58 ess=1296 css=1901 viol=25 dup=2
p39 ess=820  css=1591 viol=36 dup=2     p59 ess=1296 css=1900 viol=25 dup=2
p40 ess=820  css=1590 viol=36 dup=2     ... through p64
```

`ess` and violations frozen, `css` alternating by **exactly ±1**,
`dup=2` every pass — a period-2 flip-flop of a single CreationSet, the
065-gap-2 shape at its smallest possible size. Both flip-flops break on
their own (p45, p64), after which yopyra grows `ess` 1015 → 2529 across
~50 passes at **zero violations**, punctuated by `cs_dups` bursts (9, 12,
6, 9, 9). Two notes for the plan: (a) that late stretch is bounded only
by the hard pass cap — increment 1a's zero-violation stall requires
`!grew`, and yopyra grows; (b) yopyra is the one member with real CS-side
dup activity, which the 7-program dup-category scoping ("`cs ≈ 0` for the
ENTIRE set") did not cover — worth including before Stage-1 (ii) is
written off entirely.

## The problem, measured (2026-07-30, post-`check_split`-fix)

`check_split` (073) fixed the *intra*-pass unbounded generator. A distinct
*cross*-pass oscillation remains: FA runs to the pass cap / stall guard
(`pass_limit_hit=1`) with (usually) residual violations, never converging.
Measured across the whole corpus (full FA, `PYC_DBG_OSC` probe on
`FA::analyze`'s final pass): **17 of ~77 programs oscillate**
(`pass_limit_hit=1`):

| program | final_pass | violations | ess.n | | program | final_pass | violations | ess.n |
|---|---|---|---|---|---|---|---|---|
| softrender | 30 | 881 | 775 | | loop | 62 | 64 | 1566 |
| sudoku5 | 21 | 511 | 926 | | dijkstra2 | 43 | 170 | 946 |
| rubik | 33 | 417 | 875 | | linalg | 16 | 170 | 1025 |
| amaze | 16 | 884 | 688 | | sudoku3 | 16 | 105 | 472 |
| pylife | 41 | 90 | 399 | | yopyra | 55 | 86 | 822 |
| timsort | 17 | 66 | 376 | | chess | 39 | 63 | 3132 |
| go | 28 | 56 | 604 | | sudoku4 | 40 | 38 | 1964 |
| genetic2 | 50 | 3 | 650 | | bh | 53 | 2 | 530 |
| **pygmy** | **102** | **0** | **466** | | | | | |

Two shapes are present. Most have **residual violations** (the growing
union prevents a clean type fixpoint). **pygmy** is the pure case: it hits
the *hard* pass cap (100) with **0 violations** — types have converged but
the splitter keeps re-deriving/re-minting the same partition forever
(pure 033 non-idempotence, no union confound). (Calibration: slow-but-
*converging* programs — chull 45, chaos 35, adatron 28 — sit at
`pass_limit_hit=0, violations=0`; high pass count alone ≠ oscillation.)
**dijkstra2 — the canonical 063/065/066 canary — still oscillates**,
confirming this is a live, distinct issue the `check_split` fix
(intra-pass) did not touch. genetic2/adatron (this session's compile
wins) *compile* but genetic2 is in the oscillating set — it compiles
**despite** hitting the cap, with 3 residual violations.

## Root cause (established by 065/066)

Two coupled mechanisms; both must be fixed:

1. **A genuinely growing container-element union** (043 shape B).
   Shared container methods (`list`/`dict` `__getitem__`/`__eq__`/`__len__`
   /`__setitem__`/…) run over a heterogeneous element union; as more of the
   program resolves, the union widens, so the split products keep widening
   and never settle (065 "gap 2 / the deeper reason").
2. **Unstable cross-pass split-decision identity** (066). The durable
   split decision is keyed on the *re-created per-pass* CreationSet / the
   per-(Var,EntrySet) setter AVars — which shift as splitting proceeds — not
   on the stable source creation site. So re-flow re-derives a *different*
   partition each pass and the ledger has nothing stable to route to
   (why the issue-033 routing excludes the setter/mark stages).
3. **Circularity in the current architecture.** `run_split_stages` runs 8
   gated stages, one per pass; the per-element-CS method split
   (`split_for_per_cs_method_receivers`, `PER_CS_RECEIVER`) runs **only on
   quiescence of stages 1-5** — which an oscillating program never reaches
   — so the split that would stop the union never fires (065's final
   update). And `PER_CS_RECEIVER`/`clone_methods_per_cs` separate CSs *at
   creation*, so they cannot fan out a *union receiver* arriving on a
   recursive edge anyway.

## What changed since the 2026-07-23 analysis — the pivotal unknown

Every 064/065 attempt to let the CS partition (not the display) carry
per-recursion-level separation was blocked by the same wall: **zeroing
method `nesting_depth` re-fused `recursive_polymorphic`'s recursion
levels** (`len`/`__getitem__` over `list[list[int]]`→`list[int]`→`int`
collapsed to a union). But that re-fusion was **soft type-merging** — a
type-incompatible contour reused with a `val−4` penalty — which is exactly
the mechanism 073's `check_split` fix now **hard-gates** (it ties a
recursive knot only on `edge_type_compatible == 1`, and the same soft
merge was what regressed `match_seq` before I required the hard match).

> **RESOLVED 2026-07-30 (see "Stage 0 result" below): the second branch
> holds.** Dropping the display still re-fuses `recursive_polymorphic`
> (suite 231/4), because `check_split`'s hard-gate covers only the
> recursion-*routing* branch, while the container methods `len`/
> `__getitem__` that actually re-fuse are *normally dispatched* and still
> re-merge through `find_best_entry_sets`'s soft type match. The CS
> partition (Stages 1-2) is required; the cheaper "hard method-dispatch
> type gate" substitute was tested and **ruled out** (it breaks
> convergence — see Stage 0 result). So Stage 2 is unavoidable.

**So the single most decisive experiment for this plan is to re-run
064's `nesting_depth 0` prototype *with* the `check_split` fix in place**
and measure `recursive_polymorphic`. Two outcomes, each collapsing the
plan differently:

- **If it no longer regresses** (type identity now supplies the per-level
  separation the display was faking): the method display is *finally*
  droppable, 064 dissolves, and much of the oscillation may resolve with a
  far smaller change than the full CS-fan-out — possibly just "methods
  `nd 0` + the existing setter-site routing." Re-measure the oscillating
  set immediately.
- **If it still regresses**: the CS partition genuinely must carry the
  separation, and the full build below (Stages 1-2) is required.

Do this experiment first; it decides how much of the rest is needed.

## The build (in order; each step gated on full verification)

### Stage 0 — the pivotal experiment — DONE 2026-07-30: display still load-bearing

Ran it (a clean FA-side proxy for "methods `nd 0`" that avoids 064's
make_AVar resolution desync: behind an env flag, `edge_nest_compatible_
with_entry_set` / `group_display_ok` / `edge_display_compatible` all
ignore the display and `update_display` tolerates differences, so the
display is dropped from contour *identity* but still built for
resolution; reverted after). **Result: suite 231/4** —
`recursive_polymorphic` (level-refusion), `match_none`, `match_map_star`,
`match_seq`. **So the `check_split` fix did NOT make the display
droppable; the second branch holds — the CS partition must carry the
separation (Stages 1-2 required).**

Mechanism (confirmed from the errors): `recursive_polymorphic`'s
`flatten_sum` is `nd 1` (display `[module]`, already always-compatible —
so it was never the display's job). The separation the display provided
was for the *normally-dispatched container methods* it calls — `len` /
`__getitem__` (`nd 2`), whose `display[1]` = the per-level `flatten_sum`
contour kept `len(list[list])` and `len(list[int])` in distinct
contours. `check_split`'s type-identity knot-tying only covers the
*recursion-routing* (`e->from->split`) branch, **not** normal method
dispatch, so it does not replace this. With the display gone, the
re-fusion happens through `find_best_entry_sets`'s **soft** type match
(`val−4` reuse of a type-incompatible contour), so `x[i]` unions
`list ∪ int` and feeds back into the recursive formal `x` → the illegal
`'x' illegal: int64`/`list`.

**Hard-type-gate sub-experiment — DONE 2026-07-30: DEAD END.** Tested
"drop the display **and** make method-dispatch type matching hard"
(`entry_set_compatibility`'s `case 0: val−=4` → `return 0` behind a
`PYC_HARDTYPE` flag, so `find_best_entry_sets` never soft-reuses a
type-incompatible contour). Result: **`recursive_polymorphic` times out
(non-convergence) — and it does so with `HARDTYPE` alone, display kept**
(a currently-*passing* program). So a *global* hard type gate is strictly
worse than the status quo. The reason is the deep one 040/033 gestured at
and this makes explicit: **soft type matching is load-bearing for
convergence, not just laziness.** `val−4` lets a contour *absorb* type
widening as it flows (an EntrySet is a widening point); forbidding that
mints a fresh contour per intermediate type state, so the contour set
churns as types converge and the flow fixpoint never settles. Making the
type gate hard is exactly the eager fan-out `#if 0 // eager splitting
doesn't help` already disables one line above the edited case.

**Consequence:** the display's per-level separation *cannot* be replaced
by a cheap global type-gate change; the separation must be **scoped and
demand-driven** — created only where a same-type receiver's element types
actually diverge, and only for the affected container methods. That is
precisely Stage 2 (the CS-directed fan-out). The shortcut is ruled out;
Stage 2 is required.

### Stage 1 — stable, creation-site-keyed durable CS/setter identity (066)
Independently of Stage 0, key the durable split decision on the **stable
source site** (066's `gx.alloc_info` analog): a persistent
`creation-site → CS-duplicate` map, populated when a split is *decided* and
re-applied verbatim by `creation_point`/`clear_cs` on re-flow instead of
re-derived. This is the generalization of the already-landed
`setter_site_signature`/`cs_group_signature` ROUTE (066 part 1) from "a
per-pass routing hint" to "the durable identity of the split." Makes the
partition reproduce deterministically across passes → the ledger has a
stable target → the setter/mark stages can route (065 gap 1) without the
wrong-merge hazard.

**Started 2026-07-30 — measurement corrected the target; a churn-bound
landed as increment 1a.** Instrumenting per-pass stage/dup counts
(probe removed) showed **pygmy is NOT a CS-side case** (`cs_dup=0`): it is
the **ES type-split re-deriving the *same* 3 decisions every pass**
(`win=type_confluence dup=3`), with a completely frozen state
(`ess=466 css=1599 viol=0` identical from pass ~40 to the cap). So Stage 1
has two independent targets: (i) the **ES type-split** re-derivation
(pygmy, and the machinery behind the `dup_split_attempts` ledger at the
`TYPE_CONFLUENCE` stage), and (ii) the **CS creation-site keying** (066,
for the container-union-growth `v>0` oscillators, which show `cs_dup`
activity). The determinism root — *why* `collect_type_confluences` +
`decide_entry_set_split` re-detect and re-split the identical 3 groups
every pass after `clear_results`, and why the ES ledger ROUTE stops the
`ess` growth but not the `analyze_again=1` signal — is the real (ii)/(i)
fix and is not yet built.

- **Increment 1a — LANDED (churn bound, not the root):** the stall guard
  was gated `if (v > 0)` (`fa.cc`), so a *zero-violation* pass that
  re-derives a split (`dup>0`) yet adds no new contours (`ess`/`css`
  unchanged) — pure issue-033 churn — was never bounded and ran to the
  hard pass cap. Added a symmetric zero-violation branch (same
  `stall_limit`, reset on `ess`/`css` growth): pygmy 102 → 48 passes,
  same result. **Suite 235/0 both backends; full corpus sweep identical
  (zero category changes).** This is a *bound* on the pure-churn case,
  not the determinism fix — pygmy still sets `pass_limit_hit`; the root
  (i)/(ii) work is what makes such passes not re-derive in the first
  place.

- **Root of pygmy's re-derivation — FOUND 2026-07-30: issue 065 gap 2
  (self-product ES re-mint).** Instrumented the two ES-type ledger dup
  sites (probe removed). In the frozen state pygmy's 3 dups/pass are all
  `shade`/`getreflected` (a softrender's `Shaderinfo` shading path) at a
  **monomorphic** partition `part=[Shaderinfo]` — i.e. there is *no real
  type union* to separate; the split is a pure non-idempotence artifact.
  The signature: `shade` es=430 logs `DUP-MINT product=430`, i.e.
  `d->product == es` — the ledger recorded `es` as its *own* product, so
  the ROUTE guard `d->product != es` (`fa.cc:4659`) fails, there is
  nothing to route to, and it re-mints; its sibling es=431 `DUP-ROUTE ->
  430`; and the group's edge count `ngrp` alternates 1↔2 pass-to-pass — a
  **period-2 flip-flop** of the edge partition between the two sibling
  ESs. This is exactly the **065 "gap 2 — self-product re-minting"**
  case, and the one 066 part 1 left **NOT enforced** (its ROUTE only
  fires when `d->cs_product != cs`). pygmy is the *stable* variant of it
  (frozen, 0 violations) rather than 065's growing-union variant.
  **Fix direction (065/066's deferred half):** on the self-product case
  (`d->product == es`), do not re-mint — recognize the group's home *is*
  `es` and instead **evict the complement** (the flip-flopping sibling
  edges) to *their* recorded home, so `es` re-monomorphises to its
  recorded group and the partition stops oscillating. 065 measured that
  the naive "just keep the group in `es`" makes things far worse (37→605
  on dijkstra2 / 227→226 crash on `pyc_declare`), so the eviction must be
  paired with the stable creation/site keying (this Stage's (ii)); that
  pairing is the concrete next build. Note this is the **ES** self-product
  (`d->product`), sibling to the **CS** self-product (`d->cs_product ==
  cs`) 066 part 1 deferred — the same disease on both axes.

- **Naive complement-eviction attempt — DONE 2026-07-30: does NOT work
  (needs the stable keying).** Behind a `PYC_SELFPROD` flag, on a
  self-product re-derivation (`d->product == es`) the group's re-split was
  *suppressed* (its edges left durably where they are, via `continue` in
  `apply_entry_set_split` after `gsig`). Measured (probe removed):
  - **pygmy 49 → 102 passes (worse)**, still 0 violations. Suppressing the
    group removes its `dup` signal, which *disables* increment 1a's
    zero-viol stall (it keys on `dup>0`), so pygmy runs to the hard cap —
    and the confluence is simply re-detected every pass without being
    resolved. The skip *relocates* the churn; it does not stop it.
  - **dijkstra2 identical** (43 passes, 170 violations) — no help, but
    (unlike 065's reverted "keep the group in `es`", 37→605) **no
    regression**: `continue` does not *move* edges into `es`, so it never
    makes `es` polymorphic. It just leaves the confluence unresolved.

  Conclusion: suppression is not eviction. Correct eviction has to move
  the *widening complement* off `es` so `es` re-monomorphises to its
  recorded group — which requires knowing *which* current edges are that
  recorded group, i.e. **stable per-edge/creation-site identity ((ii))**.
  That identity is the prerequisite; the self-product handling cannot be
  done first. So the build order is fixed: **(ii) stable creation-site
  keying → then complement eviction on both the ES and CS self-product**.

- **Scoping + working prototype — 2026-07-30. The eviction DIRECTION is
  correct; the crude form regresses; the refinement is identified.**

  *Lifecycle facts (grounding the "stable key" question).* `clear_edge`
  (`fa.cc`) clears an edge's *flow* (args/rets/filters) but **not**
  `e->to` (its product ES), `e->from`, `e->pnode`, or `e->match`; and the
  `split_ledger` is **not** in `clear_results`. So the durable substrate
  already exists — edge structure + `e->to` + the ledger survive passes;
  only AVar types and `cs->defs` re-derive. Consequence for the ES side:
  the ledger's `group_signature` (arg/ret types) is *already* a stable
  key when types have converged (pygmy's do — `[Shaderinfo]`), so the ES
  self-product does **not** need a new keying map; it needs the eviction.
  (The CS side / 066 is separate: `creation_point` re-derives `cs->defs`
  each pass and *does* need creation-site keying — that is Stage-1 (ii)
  proper, still unbuilt.)

  *Prototype (PYC_SELFPROD2, reverted).* On a self-product group
  (`d->product == es`), keep the group in `es` and evict the **complement**
  (all other edges currently in `es`) to a fresh product, so `es`
  re-monomorphises. Measured:
  - **pygmy converges NATURALLY** — `pass_limit_hit` 1 → **0** (43 passes,
    0 violations); the flip-flop is gone. **Suite 235/0** (C).
  - **dijkstra2 identical** (no regression) — unlike 065's reverted
    "keep the group in `es`" (37→605); this evicts rather than accreting.
  - **But amaze (v884→915) and linalg (170→187) REGRESS** — more
    violations. Cause: evicting the *entire* complement into **one**
    product merges genuinely-different-typed complement edges into a
    polymorphic contour (065's own hazard, moved off `es` onto the comp).

  *Refinement — LANDED 2026-07-30 as increment 1b.* The refinement first
  tried was "evict `stay_edges` to their own product, separate from the
  other groups" — but it **still** regressed amaze/linalg (884→915,
  170→187). That disproved the merge hypothesis and revealed the real
  discriminator: the self-product ledger decision is only valid when types
  have **converged**. pygmy oscillates at **0 violations** (a spurious
  precision flip-flop — eviction is safe); amaze/linalg carry many
  violations (union still widening — the recorded `home == es` is stale,
  so eviction mis-homes real content). **The fix that landed gates the
  eviction on `nviol_this_pass == 0`.** With that gate:
  - **pygmy converges naturally** — `pass_limit_hit` 1 → **0** (first
    oscillation *resolved*, not merely bounded).
  - **amaze / linalg / dijkstra2 and the rest of the set: unchanged**
    (regression gone).
  - **Suite 235/0 on BOTH backends; full corpus sweep identical to
    baseline (zero per-example changes).**

  Landed unconditional in `apply_entry_set_split` (`fa.cc`): on a
  type-stage (`avpos && gsig`), zero-violation, self-product
  (`d->product == es`) group, keep the group in `es` and evict the
  compatible `stay_edges` (now carried on `ESSplitDecision`) to a fresh
  product, once. Soundness: at 0 violations all types are consistent, so
  which contour holds the flip-flopping edges is a pure precision choice —
  resolving it cannot change codegen correctness. The **`v>0`
  self-product** (amaze/linalg/dijkstra2 — the majority of the oscillating
  set) still needs the genuine stale-vs-valid discrimination, i.e. Stage-1
  (ii)'s stable keying; that remains the next build.

- **Dup-category scoping — 2026-07-30. REDIRECTS the target: the
  oscillators are ES-side, NOT CS-side.** Before building the CS
  creation-site keying (Stage-1 (ii) / 066), categorized every cross-pass
  dup (probe removed) across the 17-program set into: `es_self`
  (self-product, 1b's case), `es_route` (ES route to a *different*
  product), `es_othermint` (ES group that *has* a recorded product but
  re-mints anyway), `filt`, `cs`. Cumulative over all passes:

  | prog | viol | es_self | es_route | es_othermint | cs |
  |---|---|---|---|---|---|
  | dijkstra2 | 170 | 3 | 27 | **83** | **0** |
  | sudoku5 | 511 | 12 | 112 | **154** | 0 |
  | rubik | 417 | 13 | **147** | 1 | 0 |
  | chess | 63 | 0 | 19 | 31 | 0 |
  | amaze | 884 | 21 | 68 | 2 | 0 |
  | linalg | 170 | 4 | 37 | 17 | 0 |
  | loop | 64 | 1 | 18 | 40 | 2 |

  **`cs ≈ 0` for the ENTIRE set** (max 6, on pygmy). So **066's CS
  creation-site keying is the wrong lever for the oscillation** — none of
  these programs churn on the CS side. The dominant churn is two ES-side
  categories:
  - **`es_othermint`** (dijkstra2 83, sudoku5 154, chess 31, loop 40 …):
    a group that *has* a recorded product but re-mints because the ES
    ROUTE (`fa.cc:4664`) is blocked by **`group_display_ok`** — i.e.
    **issue 064's phantom method display** (065 gap 1). This is the same
    load-bearing display Stage 0 proved cannot simply be dropped (it
    supplies container-method per-recursion-level separation), so
    unblocking the route needs that separation moved onto the CS/type axis
    first — **Stage 2**, not a keying map.
  - **`es_route`** (rubik 147, sudoku5 112, dijkstra2 27 …): a group that
    routes to its recorded product every pass yet still signals
    `analyze_again=1`. **Idempotent-route lever investigated 2026-07-30 —
    it does NOT exist.** Instrumenting the route application (probe
    removed): every routed edge is a **genuine move** (`es → product`);
    rubik measured `noop=0, moved=200`. So there are no "already-applied"
    re-routes to skip. The churn is a genuine **flow↔split oscillation**:
    the split routes the edge to `product`, then next pass *flow*
    re-dispatches the same call to `es` (the caller's dispatch target was
    never redirected to `product`), so the split moves it out again. The
    split *outcome* is identical every pass, but the edge really bounces,
    so it can't be recognized as a no-op. It is already **stall-bounded**
    (the `v>0` guard fires after `stall_limit` re-deriving passes) — so
    there is no unbounded growth here; the residual is that the split
    can't *resolve* its violations because its product never becomes the
    dispatch target. The real fix is **dispatch coherence** (redirect the
    caller's dispatch to the split product so flow stops undoing the
    split) — the `check_split` / `pending_es_backedge` / `out_edge_map`
    machinery — which is the deep 033 core, not a small idempotence tweak.

  **Consequence for the plan:** Stage-1 (ii) as "066 CS creation-site
  keying" is **deprioritized for the oscillation** (the CS side is quiet);
  it stays relevant only for genuine CS re-derivation (pyc_declare/pygmy's
  CS ROUTE, 066's own repros). Both cheap ES-side levers are now ruled
  out — `es_othermint` is 064's load-bearing phantom display (needs Stage
  2 first), and `es_route` is a genuine flow↔split dispatch-coherence
  oscillation (already bounded; needs the `check_split` dispatch-coherence
  fix, not a signal tweak). **So the oscillation's residual (`v>0`) has no
  small remaining lever: it reduces to (a) Stage 2 — move container-method
  separation onto the CS/type axis so 064's display becomes inert and the
  `es_othermint` routes unblock — and/or (b) making the ES-split product
  the caller's dispatch target so `es_route` splits stick.** Both are
  large. The session's tractable wins (1a, 1b) are landed; the remainder
  is the genuine 033/064 core.

### Stage 2 — main-loop CS-directed ES fan-out (065's linchpin) — MEASURED, RULED OUT 2026-07-30
A new split stage in `run_split_stages`, running **every pass** (not on
quiescence — that is the circularity break), on a **demand signal** (so no
explosion): when a method ES's receiver arg is a union of **same-TYPE CSs
with divergent element types**, create one product contour per receiver CS
and route each CS's edges/flow to it. Key it on Stage 1's stable
CS-creation-site signature for issue-033 idempotence. This is what
`PER_CS_RECEIVER` cannot do (it separates at creation, not a union
receiver) and what stops the union from growing at its source.

> **Measure-first result 2026-07-30: Stage 2 does NOT address the
> oscillation; do NOT build it.** Before prototyping, a temporary probe
> (`PYC_DBG_STAGE2`, in `apply_entry_set_split`, removed after)
> characterized every re-minting group (`othermint`/`route`/`self`) across
> 7 oscillators by (a) its cross-pass category and (b) the CS-union shape
> at the **split position** `avpos` (n CSs, all-same-container-type?,
> element-type union, elements-divergent?). Three findings, each fatal to
> the Stage-2 premise:
>
> 1. **The Stage-2 demand signal is a small minority of the churn.**
>    Re-mints whose split position is a *same-type container union with
>    divergent elements* (the exact fan-out target), as a fraction of all
>    re-mints: dijkstra2 **1/113**, sudoku5 **3/278**, rubik **5/161**,
>    amaze 9/91, loop 7/59, softrender 8/65 — only **linalg 19/58 (33%)**
>    is non-trivial (and those are mostly `len`/`deepcopy` over a container
>    arg, not a *receiver* fan-out). The dominant split shapes are instead
>    **unions of NON-container objects** (`cont=0`: sudoku5 200/278, rubik
>    120/161, amaze 49/91 — polymorphic object dispatch) and **pure-display
>    monomorphic re-mints** (`nCS≤1`: dijkstra2 62/113, softrender 45%,
>    loop 44%).
> 2. **There is no growing union for Stage 2 to stop.** Tracking the split
>    union's size across passes: sudoku5 `__eq__` *shrinks* 22→18→8→2 as
>    separation proceeds; rubik's 112 `__getitem__` re-routes are a
>    **single-pass burst** (pass 17) at a stable `nCS 2-3`. The 043-shape-B
>    "genuinely growing container-element union" 065 gap 2 posited is **not
>    present** in the measured oscillators — the unions are small and
>    static-or-shrinking; the re-derivation is pure cross-pass
>    non-idempotence, not growth.
> 3. **The churn reduces to the two mechanisms already named in the
>    Stage-1 dup-category scoping, both display/dispatch — neither a
>    container fan-out.** (a) **`es_othermint`** (dijkstra2 55% of re-mints
>    are `mono`, all **stage-0/TYPE_CONFLUENCE**, 60 across 32 contours;
>    sudoku5 `__getitem__` 66 + `__eq__` 46): the ES-split ROUTE is blocked
>    by **`group_display_ok`** on type-identical or type-partition-matching
>    groups whose edges span different caller displays = **064's phantom
>    display / 073**. (b) **`es_route`** (rubik `__getitem__` **×112**,
>    sudoku5/dijkstra2 `len`): the split routes to its recorded product but
>    *flow* re-dispatches the call to the original ES next pass = the
>    **flow↔split dispatch-coherence** oscillation.
>
> **Consequence.** The plan's premise chain for Stage 2 — "the display is
> load-bearing (Stage 0) *because* it supplies container-method
> per-recursion-level separation, so move that job to the CS axis (Stage 2)
> and the display goes inert" — **breaks at the middle link for the
> oscillators**: in the oscillators the display is NOT doing container
> separation (their re-mints are non-container/pure-display), so a
> receiver-CS fan-out cannot make their displays inert. Stage 0's
> load-bearing evidence (`recursive_polymorphic`) is a *separate, passing*
> program whose display genuinely does element separation; that program is
> a Stage-4 *regression guard*, not a member of the oscillating set. So the
> two are decoupled: **the oscillation's real levers are (a) the display in
> the ES-split ROUTE gate `group_display_ok` — extend 073's landed
> type-identity knot-tying (which fixed the `check_split` recursion branch)
> to the ROUTE, co-modifying `update_display`, so a type-partition-matching
> group routes across inert displays — and (b) dispatch coherence for the
> `route` population (the deep 033 core: redirect the caller's dispatch to
> the split product so flow stops undoing the split).** Stage 2 is retired
> from this plan.

### Stage 3 — compose with the existing stages / phase ordering (066)
Reach the ES-split fixpoint, then the CS split, and do not let a CS split
re-open an already-decided ES split. The `e->to` durability already nearly
gives this for ESs; the missing half is not re-deriving a decided ES split
from a CS change.

### Stage 4 — demote the display from the ES-split ROUTE gate — RETIRED 2026-08-13 by [100](100-FA-display-removed-from-contour-identity.md)
> **Superseded, and more than this stage asked for.** 100 removed the
> display from contour identity outright, so `group_display_ok`,
> `fun_max_live_display_slot`, `stage4_enabled` and `PYC_STAGE4` no
> longer exist and the ROUTE has no display gate to demote. The
> `es_othermint` population this stage targeted is gone with them (see
> the 2026-08-13 growth re-census above); the growth that remains is on
> the *detach* route, which this stage never addressed. Kept for the
> measurement trail only.

With Stage 2 retired (see its measure-first result above), this is the
**primary remaining lever** for the oscillation's dominant `es_othermint`
population, not a Stage-1/2-blocked cleanup. The measurement decoupled the
two display roles the plan had conflated:
- **In the oscillators**, `group_display_ok` blocks the ES-split ROUTE for
  groups whose *type partition already matches* a recorded product but
  whose edges span different caller displays (dijkstra2's 60 stage-0
  `mono` re-mints; sudoku5's `__getitem__`/`__eq__`). The display here is
  **inert** — it separates nothing the type partition doesn't already.
- **In `recursive_polymorphic`** (a passing program, NOT an oscillator —
  it is the Stage-4 *regression guard*), the display separates genuinely
  different element types through normally-dispatched `len`/`__getitem__`
  (Stage 0). That separation must survive.

073's landed `check_split` fix already resolved the analogous split in the
**recursion-routing** branch by reusing a contour only on a **hard type
match** (`edge_type_compatible == 1`), which preserves
`recursive_polymorphic` *by type*. The concrete Stage-4 build is the same
move one level up, in `apply_entry_set_split`'s ROUTE: when a group's
`(avpos, part, gsig)` matches a prior-pass product (already a type-partition
match), **route to it even when `group_display_ok` fails**, co-modifying
`update_display` (fa.cc:958 assert) to rebuild the display from the routed
edge rather than assert equality (064 item 2 / 073's "hard constraint on
the fix"). Because the ROUTE already keys on the type partition, relaxing
*only* the display gate there cannot merge type-different groups — the
exact safety `recursive_polymorphic` needs, and stronger than Stage 0's
blanket display-drop (which regressed via the *soft* `find_best_entry_sets`
match, a different site the ROUTE does not use). Verify by re-running the
Stage-0 nodisp probe and the full regressor list; a later cleanup can then
set genuine methods' `nesting_depth` to 0 (distinguishing the issue-001
synthesized closure carriers, which keep it). The `es_route` /
dispatch-coherence population (rubik) is *not* fixed by this and remains
the separate 033-core lever (b).
(The cheaper "hard method-dispatch type gate" alternative was tested and
ruled out — it breaks convergence; see the Stage 0 result.)

> **Prototype built + measured 2026-07-30 (behind `PYC_STAGE4`,
> flag-gated, not landed).** Implemented the display-liveness demotion:
> a per-`Fun` cached `max_live_display_slot` (`fun.h` +
> `fun_max_live_display_slot` in `fa.cc`) = the highest display slot the
> fun's body actually consumes in `make_AVar` (a referenced Var at
> `nesting_depth` k+1 owned by a proper ancestor scope); slots above it
> are inert. `group_display_ok` (type-stage ROUTE only, `!fsetters &&
> !fmark`) enforces only the live slots, and `update_display` asserts
> only the live slots, so a type-partition-matching group ROUTEs across
> inert (caller-context) display slots instead of re-minting. For the
> Python frontend the mask is empty above the module singleton (captures
> are lowered to explicit closure classes), so a genuine method's caller
> slot is inert; genuine V closures / issue-001 carriers reference an
> ancestor free var and keep a live slot, so their enforcement is
> unchanged.
>
> **Correctness/determinism gate — CLEAN:** suite **235/0 on BOTH
> backends** with the flag on; corpus sweep **identical 53-compiled set**
> (zero COMPILED→FAIL, zero swaps, diffed); `recursive_polymorphic`
> compiles clean (the type-partition `gsig` gate protects it — its
> contours differ by type, not just display). Flag-off is byte-identical
> to baseline (all logic gated on `stage4_enabled()`).
>
> **Oscillation gate — NET-POSITIVE BUT NOT PRECISION-NEUTRAL (so not
> yet landable):** violations at the cap — rubik **417 → 128** (−289;
> passes 33 → 21), sudoku5 **511 → 434**, dijkstra2 **170 → 140** (passes
> 43 → 36), loop **64 → 53**; amaze/linalg neutral. **Regressions:**
> softrender **881 → 895 (+14)** and pygmy compile-time **43 → 77 passes**
> (still converges, `pass_limit_hit=0`, 0 violations). **No oscillator
> reaches `pass_limit_hit=0`** — Stage 4 removes the `es_othermint`
> re-mint churn but the `es_route`/dispatch-coherence residual (lever b)
> still caps every program. The softrender/pygmy regressions are the
> issue-033 mint→route trajectory sensitivity, NOT lost data separation
> (`group_signature` keys on `->type`, which preserves non-constant CS
> identity, so the ROUTE cannot merge CS-different groups): routing a
> group into a *durable accumulated* product vs. minting a *fresh* one
> reaches a different frozen state — program-specifically better (rubik)
> or worse (softrender). So Stage 4 alone meets correctness but violates
> the "none regress" precision gate for two programs; landing it wants
> either lever (b) composed in (so the routed splits actually stick and
> converge, likely dissolving the trajectory noise) or a
> trajectory-stabilizing refinement. Kept behind the flag as a verified,
> documented partial.

### Lever (b) — dispatch coherence — MEASURED, PREMISE OVERTURNED 2026-07-31
The plan (and 073's es_route note) framed lever (b) as a **flow↔split
bounce**: the split routes an edge es→product, then flow re-dispatches the
*same* call back to es, so the split "moves it out again" every pass —
fixable by redirecting the caller's dispatch to the product. A temporary
per-edge probe (`PYC_DBG_ROUTE` in the ES-split ROUTE, removed after)
tracking whether the **same edge** is routed on consecutive passes
**overturns that premise on rubik** (the canonical es_route case):

- **216 of 217 route events are DISTINCT edge ids** (only edge 362 ever
  routed twice; **zero** consecutive-pass bounces). So the same edge is
  *not* bouncing es↔product — essentially every route is a *fresh* edge.
- The routes are a **tapering transient**, not steady: a burst at pass 17
  (138), decaying 38 → 3 → 2 as the run proceeds — not the "~200/pass
  forever" a bounce implies. (The earlier "moved=200, noop=0" is fully
  consistent with fresh distinct edges each genuinely moved *once*; it was
  mis-read as one edge bouncing.)
- The recurring target `es 59 → product 213` (35 routes) is fed by **35
  distinct edges from ~11 distinct caller contours** (from-ids 98, 200,
  355, 357, 361, 371, 392–397). So the driver is **caller-contour
  MULTIPLICATION**: a shared method's contour `es` receives one fresh edge
  from each newly-minted caller contour, and each is routed once to the
  recorded product. As caller multiplication settles, the routes stop.
- **Stage 4 barely changes it** (217 → 204 routes), and rubik's violations
  still dropped 417 → 128 — so the route transient is *not* what drives
  rubik's residual violations.

**Consequence — lever (b) as "dispatch coherence to stop the bounce" is
attacking a bounce that does not exist.** There is nothing to make "stick":
the routed edges already stay on their product (`make_entry_set`
early-returns on a set `e->to`; `clear_edge` preserves it). The es_route
"churn" is a bounded, converging transient of caller multiplication. So the
residual violations that keep rubik/dijkstra2/sudoku5 at the pass cap
(after Stage 4 removes the es_othermint re-mints) are **not oscillation
churn at all** — they are either the caller-multiplication transient not
yet quiesced when the stall guard fires, or **genuine unresolved type
violations** (the 063 "no type" bucket: shared dispatch methods over
heterogeneous object unions that never monomorphise). Both point away from
a splitter-idempotence fix:

1. **Caller multiplication** is the *display/type caller-context* axis —
   the same root Stage 4 attacks at the *identity* level. The remaining
   per-fresh-caller route is bounded and self-limiting; reducing the
   multiplication itself (fewer caller contours) is the lever, i.e. *more*
   display demotion / a caller-side analog of Stage 4, not dispatch
   redirection.
2. **Genuine residual violations** are not an oscillation problem — they
   are the 063 "no type" limitation, tracked there; a converging FA would
   still report them. Re-measure each capped program to split its residual
   into (transient vs genuine) before treating it as an oscillation.

**Net for the plan:** both levers the plan named for the `v>0` residual are
now measured away — Stage 2 (no growing container union) and lever (b) (no
dispatch bounce). Stage 4 (display-identity demotion) is the one mechanism
that measurably reduces the churn, and the remaining residual is caller
multiplication + genuine "no type" violations, not a fixable non-idempotence
oscillation. pygmy (the pure 0-violation cap-hitter) remains the one clean
idempotence case, resolved by increment 1b except for the Stage-4
interaction that slows it (43 → 77 passes).

## Verification plan (per step — issue-033 fragility demands it)

1. **Oscillation gate:** the measured oscillating set (all 17
   `pass_limit_hit=1` programs above) moves toward `pass_limit_hit=0` with
   strictly fewer violations; none regress. **pygmy is the cleanest unit
   test** — 0 violations, pure re-derivation, so a Stage-1 determinism fix
   alone should drop it off the cap without any precision change. Re-use
   the `PYC_DBG_OSC` probe (temporary, on `FA::analyze`;
   `final_pass`/`pass_limit_hit`/`violations`/`ess.n`).
2. **Suite:** `test_pyc.py` and `PYC_FLAGS=-b test_pyc.py` at **235/0**
   both backends (current baseline), zero regressions.
3. **Determinism gate + full corpus sweep:** no COMPILED→FAIL; watch the
   historically fragile casualties — 064's `recursive_polymorphic`/
   `exception_propagation`; 065's mark-routing losses `chess`/`mastermind2`
   /`sat`; 066's `pygmy`/`pyc_declare` — all must stay green.
4. **No new `check_split` regressions:** `match_seq`/`match_none` stay
   green (Stage 0/4 touch the same display machinery 073 relies on).

## Risks

This is the most-reverted surface in the tree (033 M2/M3, 064, 065's two
reverts, 066's self-product deferral). Every "small" change here has
needed a full land-verify-revert cycle against a *many-pass* corpus member
(the standard short-running suite is insufficient — see the fysphun
stage-2 segfault note in `run_split_stages`). Gate each step on the full
oscillation + determinism + corpus sweep, and prefer landing Stage 1
(pure determinism, no new precision) before Stage 2 (new fan-out).

## What this unblocks

Convergence of the whole "no type" / oscillation bucket (063): dijkstra2,
amaze, chess, go, linalg, loop, bh, genetic2 stop hitting the pass cap and
resolve their residual violations; the container-element-union family
(043 shape B) is closed at its source; and — via Stage 4 — the method
phantom display (064) is retired, simplifying the splitter long-term. The
corpus evidence that the *decide-then-durable-with-stable-keys* shape
converges is shedskin, which compiles the whole corpus this bucket is
drawn from.
