# 128 — CreationSet identity over-discriminates 16× against the element type it stands for

**Status:** open as a MEASUREMENT and a set of negative results. Its
*measurement* motivates the start-merged posture; its *fix direction*
(CreationSet reuse at mint time) is dead — see "Where reuse fits". **Not the
active direction** — see the closing section: reducing the container
CreationSet count pulls against what the flag flip actually needs
([146](146-remove-all-arbitrary-splitting.md) E), so this is not a
dependency of [133](133-split-a-container-on-its-element-type.md).

**Area:** `ifa/analysis/fa.cc`, CreationSet identity and the ES splitting it
drives. **Severity:** performance and code size, not correctness — no wrong
answers; ~7× analysis time and ~5× emitted functions.

**The rule it violates:** IFA starts from the MINIMUM contours and splits
only on demand, and a contour is never split because a surrounding contour
split. `creation_point` keys CS identity on *(allocation site × contour)*,
so a CreationSet exists because the surrounding EntrySet split — structure,
not demand — and pyc starts maximally split rather than minimal. Both halves
are inverted.

*Compacted 2026-09-12; superseded text is in this file's git history.*

## The measurement

`IFA_DBG_ELEMTYPE=1` on `shedskin_examples/chess`, final pass:

```
ELEMTYPE p=31 | list: 95 CS / 6 elemtypes / 6 shapes
                tuple: 69 CS / 2 elemtypes / 2 shapes
```

**95 CreationSets for `list`, standing for 6 distinct element types**;
`tuple` is 34×. The excess multiplies into function contours, because a
method contour is taken per receiver CS:

| chess | pyc | shedskin |
|---|---|---|
| `list.append` | **110** clones | **3** contours |
| `list.__iter__` | 48 | — |
| function contours | ess **1591** | **135** |
| class contours | css **6433** | **248** |

shedskin gets 3 because `list<T>` is parameterised by `T` and chess has ~3
distinct `T`. This is also the root of
[111](111-FA-selective-invalidation-per-pass.md)'s 7.4× wall-clock gap:
`ess` drives the per-pass edge count, which climbs 30,823 → 1,484,115 across
one compile while shedskin's per-iteration work stays flat.

**What it is not:** not oscillation or re-minting. [066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md)
and [101](101-FA-first-time-forever-splitting.md) cover CSs being minted
*repeatedly*. This is how many distinct CSs are *justified at all* once
minting is stable — 95 for 6 is the steady state, not churn.

## Root cause: the memo cannot merge

`creation_point` memoizes on `v->cs_map` where `v` is an AVar — a
(variable × contour) pair — so the memo yields **exactly one CS per
allocation site per contour** and never asks whether two could be the same.
`IFA_DBG_CSROUTE=list` on chess:

```
calls: 26728    cs_map 26581    MINT 147    (every other route: 0)
```

### The routes, classified — three are the dead frame, one is the rule

**Corrected 2026-09-12.** This section used to call all of them "reuse
routes" and name a `creators` bug as the one actionable item. Both were
wrong; the accurate split is:

| route | what it is |
| --- | --- |
| `cs_map` | the memo. Not reuse — "this site in this contour already has its CS". |
| `dcpa1` | **coarse identity by construction** (`PYC_CSDCPA1`). The design's answer to this issue's measurement. Placed FIRST on purpose. |
| `split_parent` | **the ENFORCEMENT of "an ES split must not MULTIPLY CreationSets"** — a contour split off a parent inherits the parent's creation-point→CS mapping, so the site keeps its data contour instead of minting one per *(site × contour)*. Live and default-on (`PYC_ESLINEAGE=1`). It had a real bug: it read `es->split`, which `clear_splits()` zeroes every pass, so the inheritance held only on the pass of the split; it now reads `split_origin` durably and walks the ancestor chain. **Not a dead frame — this is the rule working.** |
| `cselem` / `csshape` / `csmold` | mint-time content/shape keying. **This is the dead frame** (see "Where reuse fits"): 0 hits at BOTH arms, and mint-time keying cannot work in principle. |
| `creators` | **already DELETED**, and deletion was right. It was dead since IFA 0.6 (`if (nvars != -1 \|\| x->vars.n != nvars) continue;` continues on every iteration), so it never selected a CreationSet in the history of the code. Both repairs are whole-program merges: `&&` takes the first creator of the sym unconditionally — one CS per class program-wide — and `==` fuses every record CS of one sym and arity, destroying the per-position precision ifa/104 depends on. `nvars` had no other consumer and went with it. |

So there is **no dead-code bug left here**, and "make the inert routes fire"
is not the work — it is the frame the next section retires.

## Where "reuse" fits in the design: it does not, and `creation_point` says so

The inert routes invite the wrong conclusion — *make them fire*. Three
different things get called reuse here and only two are live.

**1. Disambiguating among a site's multiple contours** — `split_parent`,
`cselem`, `csshape`, `csmold`. These are this issue's reuse routes, and
`creation_point`'s own comment on the route ordering states why they are a
dead frame:

> *"ifa/128 start-merged route. Deliberately BEFORE the split-parent,
> cselem, and mold routes: those all answer 'which of this site's contours
> should this be', a question that does not arise when a sym has one
> contour."*

That question exists **only because identity is *(site × contour)***. Under
the start-merged posture a sym has ONE contour, so it is void — which is
exactly why all four measure 0 hits even at the flag arm. Nor can they be
rescued as a better key: `PYC_CSELEM=3`'s key is evaluated at MINT time,
when the element is unfilled by construction (402 of 795 such mints never
acquire a shape). **Keying cannot get ahead of a decision taken before the
evidence exists**, and that argument applies to every mint-time route, not
just that one.

**2. Coarse identity by construction** — the `dcpa1` route,
`PYC_CSDCPA1`. This IS the answer to this issue's measurement, and it is
not reuse: it is one contour per sym, needs no evidence at all, and is
placed first precisely so the question above never arises. `split_css` then
moves a def off the root when a demand asks. The comment's own words:
*"start merged, separate on evidence."*

**3. Re-attaching a re-derived decision across passes** — the split ledger
(`FA::ledger_find_cs` / `ledger_add_cs` over `cs_group_signature`) and its
EntrySet analogue `find_or_make_filtered_entry_set`. Live, necessary, and a
different kind of thing: it reuses a DECISION, not a merge of unrelated
creation points. Every pass re-derives from bottom, so a split that is
re-derived must re-attach to the contour it first made rather than minting
a fresh one, or the analysis churns instead of converging (the issue 033
stability rule). **The start-merged posture needs MORE of this, not less** —
the coarser you start, the more splits are re-derived every pass.

So the design's shape is **coarse identity → demand-driven separation →
ledger so the separation survives the next pass.** There is no reuse step in
it, and the four routes in (1) are not a missing feature but a consequence
of the identity this issue is complaining about.

## Settled: a merge CAN be taken back

The original filing claimed sharing was unreachable because "derived types
are permanent, so a wrong merge is irreversible", and concluded 128 and 111
were one change. **Both are wrong.** `analyze_to_convergence` resets
*before* each pass, so all flow state is already re-derived from bottom
every pass. What persists is the **decision** (`av->cs_map`), not the types.
Demonstrated twice in code: `cselem_rejoin_unknown_mints` takes back 36
site→CS decisions corpus-wide with every verdict on all 77 programs
byte-identical, and `PYC_CSDCPA1` starts merged for **−32% container
CreationSets** with `ess` going DOWN. 111 is a performance lever for the
extra passes, not a precondition.

## Why neither lever can be the default

| chess | list CS | ess | css | passes | compile |
|---|---|---|---|---|---|
| default | 95 | 1591 | 6433 | 32 | 48.2 s |
| `PYC_CSELEM=3` | 36 | 985 | 4138 | 13 | 14.9 s |
| `PYC_CSMOLD=1` | **19** | **666** | **2789** | **10** | **8.1 s** |

`PYC_CSMOLD=1` is shedskin's model and reaches parity with it (8.1 s vs
7.1 s), but it merges with no mechanism to separate afterwards and costs
`deepcopy_copy_of_copy_chain`.

`PYC_CSELEM=3` is suite-clean but **regresses `rdb`** on the corpus, and the
reason generalises: **its key is evaluated at MINT time, when the receiver's
element is unfilled by construction** — 402 of 795 such mints never acquire
a shape at all. Keying cannot get ahead of a decision taken before the
evidence exists, which is why the work moved to the start-merged posture
(129) rather than to a better key.

## 2026-09-11: this is NOT the flip's blocker, and it pulls the other way

129 recorded "the blocker is 128 — with CreationSet reuse landed, an
EntrySet split stops multiplying data contours". **Both halves are wrong;
the measurements are here so the claim is not made a third time.**

**An ES split is not multiplying contours through `creation_point`.**
`PYC_ESBLOCK=1` on `sudoku5` costs two programs, and that was attributed to
one CS per *(site × contour)*. The route histogram says otherwise:

```
                 MINT   dcpa1   cselem  csshape  csmold  split_parent
ESBLOCK off      2342    1077        0        0       0             6
ESBLOCK on       2134     931        0        0       0             6
```

`creation_point` mints **fewer** with the split, not more, and the splitter
stages also mint fewer. Contour totals do grow at matched passes, but not
through the route this issue is about. The three reuse routes this issue
exists to land are inert (0 hits) even at the flag arm; `dcpa1` is the only
live one.

**And fewer CreationSets makes the failure WORSE.** The obvious extension is
to drop `dcpa1`'s `tuple` exclusion, whose stated reason looked obsolete
after ifa/132/139/141's arity guard. It is not:

| arm (one binary, env toggled) | cfail | warns | container CS |
| --- | --- | --- | --- |
| flag | 2 | 45 | 2406 |
| flag + tuple merge | **12** | 35 | 1806 |
| flag + ESBLOCK + tuple merge | **12** | 34 | 1848 |

−25% contours for **ten** programs: dijkstra2, kmeanspp, msp_ss, othello,
pygmy, quameon, sudoku4, sudoku5, plcfrs, voronoi2. Position is real — two
arity-2 tuples `(int, str)` and `(str, int)` merge their positional slots
and nothing separates them again. It does not even help its target: on
`sudoku5` the worst `tuple.__eq__`/`__lt__` contour goes from 22 shapes to
**32** with tuples merged, because fewer CreationSets means fewer comparison
contours each serving more shapes.

**The pyc suite is NEUTRAL across the tuple-merge change** — identical
failures with and without it, on both arms. Anyone re-testing this sees a
green suite over a corpus that has lost ten programs. **That coverage gap is
owed a fixture.**

### What the blocker actually is

146 E. `sudoku5` and `plcfrs` fail under `PYC_ESBLOCK` because ONE shared,
UNROLLED `tuple.__eq__`/`__lt__` contour serves many tuple shapes of
differing arity, and its single `x` per slot position merges their slot
types into `{int64, str}` — a BOXING violation at every use. Separating a
formal that holds N tuple CreationSets is what the removed
CARTESIAN_PRODUCT splitter did, and `tests/splitter_cartesian_product.py`
specifies the replacement.

Reducing the number of tuple CreationSets — this issue — pushes the WRONG
way for that: it concentrates more shapes onto each comparison contour.
**The two are not a chain; they pull against each other, and 146 E is the
one the flip needs.**

## The plan, following the design

The design is **coarse identity → demand-driven separation → ledger so the
separation survives the next pass.** This issue's *measurement* is the
motivation for the first step; it owns no mechanism of its own beyond that.
So the plan is not "land reuse" — it is "make the start-merged posture the
default", and the steps are the things that must be true first.

**Step 1 — coarse identity. Built (`dcpa1`), keep its one exclusion.**
`PYC_CSDCPA1=2` gives −32% container CreationSets with `ess` going DOWN. Its
`tuple` exclusion stays: dropping it was measured on one binary, env
toggled, and costs **ten** corpus programs, because two arity-2 tuples
`(int, str)` and `(str, int)` merge their positional slots and no demand
test can separate them again. Arity and position are part of a tuple's TYPE,
not provenance.

**Step 2 — separation strong enough to give the precision back on demand.**
This is the whole bill, and it is not in this issue:

| mechanism | issue | state |
| --- | --- | --- |
| arity in identity | [132](132-arity-is-representation-not-provenance.md) | landed |
| `CS_DEF_PARTITION` — partition a CreationSet's own creation points | [133](133-split-a-container-on-its-element-type.md) | landed, default 1 |
| ES split as a MEANS to a CS split (`PYC_ESBLOCK`) | 133 | built, opt-in |
| receiver separation for a formal holding N tuple CSs | [146](146-remove-all-arbitrary-splitting.md) E | **open — the actual blocker** |
| constants separated on demand, not by annotation | [151](151-split-an-entryset-on-a-constant-argument-on-demand.md) | open |
| more representation properties in the literal separator set | 133 | open |

`PYC_ESBLOCK` is the honest test of whether step 2 is finished: it is
correct by its own design and still costs `sudoku5` and `plcfrs`, because
one shared unrolled `tuple.__eq__`/`__lt__` contour merges slot types across
shapes. That is 146 E, and **reducing the CreationSet count pushes the wrong
way on it** — fewer contours means more shapes per comparison contour. Which
is why this issue is not on the critical path and 146 E is.

**Step 3 — the ledger, so a re-derived split re-attaches.** Exists
(`ledger_find_cs`/`ledger_add_cs` over `cs_group_signature`, and
`find_or_make_filtered_entry_set` on the ES side). The coarser the start,
the more splits are re-derived every pass, so this matters MORE under
start-merged, not less. Known gap: the existing signature is deliberately
constant-stripped, so a constant split needs a new key (151 step 4).

**Step 4 — the dead frame: mark it, and let the author settle deletion.**
`cselem`, `csshape` and `csmold` are 0 hits at both arms and answer a
question start-merged makes void. Note `csmold` is **default 3, i.e. ON**,
and still never fires (mode 3 excludes split children) — a default-on route
contributing nothing.

Deleting them is arguable both ways and this issue should not decide it
alone:

- **For:** 146's corollary — *an off-by-default lever still gets reached the
  moment a program resists, and it reads as sanctioned because it is in the
  tree.* `creators` was removed on exactly this reasoning.
- **Against:** that corollary targets *splitting* levers, whose failure mode
  is being reached for precision when a program resists. A *merge* lever has
  no such pull — nobody turns on `PYC_CSELEM` to make a program compile.
  And [129](129-plan-demand-driven-creation-set-splitting.md) recorded a
  deliberate decision to keep these: *"both flags stay, default off, with
  this result recorded at their definitions. They are the reproduction of
  the experiment; deleting them would lose the ability to re-run it."*
  This issue's own `PYC_CSELEM=3` and `PYC_CSMOLD=1` numbers are exactly
  that experiment.

The change that is right either way, and costs nothing: **say at each
route's definition that it answers "which of this site's contours should
this be", which start-merged makes void** — so the next reader does not
read 0 hits as a bug to fix. Making them fire is not the work.

### Verification, when it is picked up

- `IFA_DBG_ELEMTYPE`'s gap closing on chess (95 → nearer 6) and
  `list.append`'s clone count falling toward the element-type count.
- **`ess`/`css` must not GROW at the default.** This exists so contours can
  merge safely; if it adds contours where nothing merged it is doing the
  opposite of its purpose.
- `./corpus_sweep.sh -m check`, compared per program: `compile_rc`,
  `run_rc`, `cpy_rc` and `stdout_match` unchanged. The headline totals are
  not enough — two programs can swap and net to zero.
- Watch for [123](123-CGEN-union-receiver-field-access-has-no-discrimination.md)'s
  failure mode — merging CSs that codegen then blind-casts between — and for
  ifa/147's rule that a 1–2 program movement is never attributable.
