# 128 — CreationSet identity over-discriminates 16× against the element type it stands for

**Status:** open as a MEASUREMENT and a set of negative results. **Not the
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

## Root cause: the memo cannot merge, and every reuse route is inert

`creation_point` memoizes on `v->cs_map` where `v` is an AVar — a
(variable × contour) pair — so the memo yields **exactly one CS per
allocation site per contour** and never asks whether two could be the same.
`IFA_DBG_CSROUTE=list` on chess:

```
calls: 26728    cs_map 26581    MINT 147    (every other route: 0)
```

**Not one of the five reuse routes fires**, each for its own reason:

| route | why |
| --- | --- |
| `creators` | **dead code.** `if (nvars != -1 \|\| x->vars.n != nvars) continue;` — if `nvars != -1` it continues; if `nvars == -1` then `x->vars.n != -1` is always true, so it continues. Always. Almost certainly a `\|\|` that should be `&&`. |
| `split_parent` | needs `PYC_CSSPLIT=0`; that flag has since been removed entirely (146 A) |
| `cselem` / `csshape` | need `PYC_CSELEM != 0`; default 0 |
| `csmold` | mode 3 excludes split children |

The `creators` bug is the one concrete, actionable item here.

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

## If it is picked up again

1. Fix the `creators` dead code (the `||`/`&&` above) — independently
   correct regardless of direction.
2. `IFA_DBG_ELEMTYPE`'s gap closing on chess (95 → nearer 6) and
   `list.append`'s clone count falling toward the element-type count.
3. `./corpus_sweep.sh -m check` for exit-code and stdout neutrality: this
   must be behaviour-preserving.
4. Watch for [123](123-CGEN-union-receiver-field-access-has-no-discrimination.md)'s
   failure mode — merging CSs that codegen then blind-casts between.
