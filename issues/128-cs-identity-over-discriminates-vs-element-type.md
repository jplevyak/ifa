# 128 — CreationSet identity over-discriminates 16× against the element type it stands for

**Status:** open, filed 2026-09-04 while measuring why pyc's contour
count exceeds shedskin's on the same program ([111](111-FA-selective-invalidation-per-pass.md)).
**Area:** `ifa/analysis/fa.cc` — CreationSet identity and the ES splitting it drives.
**Severity:** performance and code size, not correctness. No wrong answers; ~7× analysis time and ~5× emitted functions.

**The rule this violates**, restated by the author 2026-09-06 because it
keeps being forgotten:

> IFA is a simultaneous data and control flow analysis based on abstract
> interpretation against a type-value lattice. It starts with the minimum
> function and data contours and proceeds in passes by splitting the
> function and data contours to increase precision, by types, setters,
> etc. **Contours should never be split simply because the surrounding
> contour is split; they should only be split on demand.**

That is this issue in one sentence. `creation_point` keys CS identity on
*(allocation site × contour)*, so a CreationSet is created because the
surrounding EntrySet split — structure, not demand — and pyc starts
maximally split rather than minimal. Both halves of the rule are inverted.
See CLAUDE.md and [IFA.md §1.1](../IFA.md).

## Measurement

`IFA_DBG_ELEMTYPE=1` on `shedskin_examples/chess`, final pass:

```
ELEMTYPE p=31 | list: 95 CS / 6 elemtypes / 6 shapes
                tuple: 69 CS / 2 elemtypes / 2 shapes
```

**95 CreationSets for `list`, standing for 6 distinct element types.**
The probe's own comment predicted exactly this reading: a large gap
means CS identity is "keyed on allocation site × contour rather than on
the parameter it is supposed to capture". `tuple` is worse — 69 CS for
2 element types, 34×.

## What it costs

CPA gives a method contour per receiver CS, so the excess multiplies
straight through into function contours. Emitted C for chess, one
definition per surviving clone:

| | pyc | shedskin |
|---|---|---|
| `list.append` | **110** clones | **3** contours |
| `len` | 69 | — |
| `list.__iter__` | 48 | — |
| function contours | ess **1591** (715 after cloning) | **135** (19 user + 116 builtin) |
| class contours | css **6433** | **248** total `cl.dcpa` |

shedskin gets 3 `append`s because `list<T>` is parameterised by `T` and
chess has ~3 distinct `T`. pyc gets 110 because it has 95 list CSs.

And this is the same root as [111](111-FA-selective-invalidation-per-pass.md)'s
7.4× wall-clock gap: `ess` drives the per-pass edge count, which climbs
30,823 → 1,484,115 across one compile while shedskin's per-iteration
work stays flat.

## What this is NOT

Not the oscillation/keying problem. [066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md)
(fixed, `PYC_CSKEY=3`) made the split DECISION durable per creation
site, and [101](101-FA-first-time-forever-splitting.md) covers
first-time-forever splitting. Both are about CSs being minted
*repeatedly*. This is about how many distinct CSs are *justified at all*
once minting is stable: 95 for 6 element types is not churn, it is the
steady state.

## Fix direction

A container CS whose only distinguishing feature is its allocation site
— identical element type, identical shape — should not force a separate
method contour. That is shedskin's `list<T>`: identity by the type
parameter, not by the site.

The aggressive form is to key container CS identity on the element type
directly. The reason not to reach for a weaker rule first: CS identity
is load-bearing for ES splitting, so the honest question is *which
consumers actually need site identity* — element-type inference does,
method contour selection appears not to. Answer that before narrowing.

## Verification plan

1. `IFA_DBG_ELEMTYPE` gap closes: list CS count approaches its
   elemtype count on chess (95 → nearer 6).
2. `list.append` clone count in the emitted C drops toward the element
   type count.
3. `ess`/`css` and analysis time on the corpus; `./corpus_sweep.sh -m check`
   for exit-code and stdout neutrality — this must be behaviour-preserving.
4. Watch for the [123](123-CGEN-union-receiver-field-access-has-no-discrimination.md)
   failure mode: merging CSs that codegen then blind-casts between.

## What this unblocks

The 7.4× analysis-time gap in [111](111-FA-selective-invalidation-per-pass.md),
and the emitted code size — chess is 2.1 MB / 108k lines of C against
shedskin's 904 lines of templated C++.


## Root cause (2026-09-04)

### 1. CS creation is derived from ES splitting, never from demand

`creation_point` memoizes on `v->cs_map`, and `v` is an AVar — a
(variable × contour) pair. So the memo gives **exactly one CS per
allocation site per contour**, which is the comment at fa.cc:9988's
"mints one CS per (site × contour)". Nothing ever asks whether two of
them could be the same.

Measured with the existing `IFA_DBG_CSROUTE=list` probe on chess:

```
calls: 26728        cs_map 26581        MINT 147        (every other route: 0)
```

**Not one of the five reuse routes fires.** Each is inert for its own
reason:

| route | why it never fires |
|---|---|
| `creators` | **dead code.** `if (nvars != -1 \|\| x->vars.n != nvars) continue;` — if `nvars != -1` it continues; if `nvars == -1` then `x->vars.n != -1` is always true (`Vec::n` is a non-negative int) so it continues. Always. Almost certainly a `\|\|` that should be `&&`. Predates the 2026-02-27 subdirectory move. |
| `split_parent` | needs `PYC_CSSPLIT=0`; default is 1 |
| `cselem` / `csshape` | need `PYC_CSELEM != 0`; **default is 0** |
| `csmold` | mode 3 (default) excludes split children — and under `PYC_CSSPLIT=1` essentially every container site in a split contour IS one |

So the only active mechanism is the per-AVar memo, and it cannot merge
anything by construction. There is no demand test anywhere.

### 2. What sharing would buy — measured

chess, same source, changing only the lever:

| | list CS | ess | css | passes | compile |
|---|---|---|---|---|---|
| default | 95 | 1591 | 6433 | 32 | 48.2 s |
| `PYC_CSELEM=3` | 36 | 985 | 4138 | 13 | **14.9 s** |
| `PYC_CSMOLD=1` | **19** | **666** | **2789** | **10** | **8.1 s** |

`PYC_CSMOLD=1` is shedskin's model (share one instance per allocation
site, split later) and lands at **8.1 s against shedskin's 7.1 s —
parity**. chess's output stays byte-identical to CPython.

### 3. ~~Why sharing cannot simply be turned on: merges are irreversible~~

> **CORRECTED 2026-09-05. The mechanism claimed below is wrong.** The
> measurement in it stands; the explanation does not, and the conclusion
> drawn from it ("128 and 111 are one change") is retracted. Read this
> block before the section it heads.
>
> **Derived types are NOT permanent.** `analyze_to_convergence` resets
> *before* each pass, not after: at the default (`ifa_selective = 0`) it
> calls `clear_results()` unconditionally for every pass but the first, so
> the analysis already re-derives all flow state from bottom every pass.
> pyc does not need shedskin's `restore_network`; its pass structure gives
> the same power for free.
>
> **What is permanent is the DECISION, not the types.** Of the five things
> `clear_results`'s header documents as surviving a pass, exactly one is in
> the way: `av->cs_map`. The others are structural parenthood,
> post-convergence clone state, a cache that misses rather than lies, and a
> coercion target. And the reason given for pinning `cs_map` is a *feeding*
> invariant (issue 030: a CS's positional `vars[i]` must keep being fed),
> which forbids a CS losing a feeder — not re-pointing a site onto a CS
> that is fed.
>
> **Demonstrated twice, in code:**
>
> - `cselem_rejoin_unknown_mints` takes back 36 site→CreationSet decisions
>   across the corpus and the analysis re-converges, with every verdict on
>   all 77 programs byte-identical.
> - `PYC_CSDCPA1` starts merged — one CreationSet per sym — and gives
>   **−32% container CreationSets corpus-wide** (3748 → 2540 → 2716 with
>   arity in identity), with `ess` going **down** rather than up.
>
> So sharing *can* be turned on, and 128 and 111 are **not** one change.
> ifa/111's selective invalidation is a performance lever for the extra
> passes this costs, not a precondition.
>
> **The `PYC_CSMOLD=1` measurement below is still real** — the deepcopy
> chain does fail. Its cause is not monotonicity. It is that the mold
> merges with no mechanism to separate afterwards, and this session found
> the two that were missing:
> [132](132-arity-is-representation-not-provenance.md) (arity is part of a
> record-able container's type — landed, and it removed every compiler
> crash and hang under the flag) and
> [133](133-split-a-container-on-its-element-type.md) (an element union
> with no representation must force a split — open).
> `deepcopy_copy_of_copy_chain` is still failing under `PYC_CSDCPA1`, so
> the re-diagnosis is not finished; what is settled is that "the types
> cannot be unlearned" is not the reason.
>
> The plan and every measurement live in
> [129](129-plan-demand-driven-creation-set-splitting.md); the frontend
> annotations that force splits today are
> [134](134-remove-the-frontend-forced-split-opt-in.md).

### 3 (original). Why sharing cannot simply be turned on: merges are irreversible

`PYC_CSMOLD=1` costs exactly one test, `deepcopy_copy_of_copy_chain`
(ifa/105's case). The mechanism, measured:

```
mold=3   ELEMTYPE list: 11 CS / 9 elemtypes  mixed=0   12 passes, 0 violations
mold=1   ELEMTYPE list:  8 CS / 6 elemtypes  mixed=2   31 passes, pass_limit_hit=1
```

Sharing gives `r` a container/scalar-mixed, self-referential element
type, and **the splitter then runs to the pass cap without separating
it**. It is not that the demand split is missing — ess climbs 117 → 148,
so it is trying. It cannot succeed: pyc's ATypes are monotone, so once
`r`'s element includes `list<itself>` the cycle is in the type and no
later split recovers the acyclic structure.

**That is the root cause.** Demand-driven splitting means starting
merged and separating on evidence. That requires being able to UNLEARN a
merge. pyc's analysis only accumulates, so a wrong merge is permanent —
which leaves it no choice but to split eagerly and structurally, one CS
per (site × contour), which is the 95-for-6 excess this issue opened on.

shedskin can share aggressively for exactly the reason
[111](111-FA-selective-invalidation-per-pass.md) documents:
`restore_network` discards the derived types every iteration and
re-derives them from `alloc_info`, so a merge that turns out wrong in
one iteration is simply not present in the next. Its `ifa()` split is
demand-driven because its network is disposable.

**So 128 and 111 are one change, not two.** Cheap CS sharing is
unreachable while the analysis is monotone and irreversible; it becomes
straightforward once the derived structure can be discarded and rebuilt.

### 4. Available now without that change

`PYC_CSELEM=3` (element-shape-keyed CS identity, ifa/074) is
**suite-clean**: `./test_pyc.py` 0 failed, zero new failures against the
default, and it takes chess 48.2 s → 14.9 s with `ess` 1591 → 985. It
does not need the reset because it keys identity on the element shape up
front rather than merging and hoping to split later. Corpus sweep needed
before flipping the default — `./corpus_sweep.sh -m check -e "PYC_CSELEM=3"`.

> **The sweep was run, 2026-09-04, and the answer is no.** `PYC_CSELEM=3`
> regresses `rdb` (`compile_rc` 0 → 1) and **cannot be the default**. It
> does reduce container CreationSets 3748 → 3081 (ratio 5.99 → 4.92), and
> everything else on the corpus is unchanged. The cause is that its key is
> evaluated at MINT time, when the receiver's element is unfilled by
> construction — measured, 402 of 795 such mints never acquire a shape at
> all. See [129](129-plan-demand-driven-creation-set-splitting.md) step 2c.
> This is why the work moved to the start-merged posture above rather than
> to a better key: keying cannot get ahead of a decision taken before the
> evidence exists.
