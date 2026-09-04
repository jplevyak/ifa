# 128 — CreationSet identity over-discriminates 16× against the element type it stands for

**Status:** open, filed 2026-09-04 while measuring why pyc's contour
count exceeds shedskin's on the same program ([111](111-FA-selective-invalidation-per-pass.md)).
**Area:** `ifa/analysis/fa.cc` — CreationSet identity and the ES splitting it drives.
**Severity:** performance and code size, not correctness. No wrong answers; ~7× analysis time and ~5× emitted functions.

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
