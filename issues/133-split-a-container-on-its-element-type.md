# 133 — a merged container leaks elements between unrelated lists

**Status:** open. The CreationSet-side separator landed and works; what is
left is a representation question, not another splitter.

**Where it sits.** [128](128-cs-identity-over-discriminates-vs-element-type.md)
asks how many container contours should exist; this issue is the separator
that makes starting merged safe. It is the largest single item in
[129](129-plan-demand-driven-creation-set-splitting.md)'s `PYC_CSDCPA1=2`
bill, and it must satisfy [146](146-remove-all-arbitrary-splitting.md): the
partition size must be what the demand distinguishes, never a count of
things.

*Compacted 2026-09-12. Superseded diagnoses, dated debugging narrative and
the levers that were built and reverted are in this file's git history.
What follows is what still holds.*

## Reproducer — five lines

```python
a = []
a.append(1)
s = []
s.append("x")
print(a[0], s[0])
```

Under `PYC_CSDCPA1=2`: `error: expression has mixed basic types:( int64 str )`
with `STAGES: TYPE_CONFL` — the splitter notices and gives up. Clean at the
default. No `__pyc__` internals are involved, which is what makes it the
right reproducer: two user creation points, both arity 0, sharing one
CreationSet, with elements that cannot both be represented.

`IFA_DBG_MIXELEM`:

```
MIXELEM cs=983 sym=list defs=6  elem= int64#6 str#8
  writer es=47 fun=__setitem__ type= int64
  writer es=51 fun=__setitem__ type= str
```

**The value path is already fully split** — one `__setitem__` contour per
type — and they both write into one element because the *receiver*
CreationSet is one. Nothing remains to split on the EntrySet side. **The
CreationSet must partition its own `defs`.**

## Root cause of the leak

`__pyc__/04_sequence.py`'s `__delitem__` passed an empty list LITERAL to
`__pyc_setslice__`, whose `merge_in` primitive merges the source into the
receiver. Under one CreationSet per sym that internal `[]` is the same
CreationSet as every empty list the user writes, so **any element any user
puts in an empty list leaks into every list that has an element deleted.**
Fixed (below).

**A trap worth keeping: absence of the `mixed basic types` diagnostic is
not evidence the merge did not happen.** `del a[0]` discards the result, so
nothing reads the union as a basic value and FA is silent — but the merge
still happens and the element is still laid out as `void*`, and the failure
reappears in the C backend as `incompatible integer to pointer conversion`
assigning a literal into a pointer-typed backing store.

## What has landed

- **`__delitem__`'s `merge_in` was a false constraint.** Removed; the empty
  literal no longer merges into the receiver. `tests/list_pop_insert.py` is
  the regression test. Also −35 CreationSets at the default, from removing
  a false element edge.
- **`CS_DEF_PARTITION` (`split_css_by_defs`), `PYC_CSDEFSPLIT` default 1.**
  The CreationSet-side separator. `split_ess_for_type`'s `tc_skip_cs`
  branch stashes the CreationSet whose contour carried a confluence stage 1
  cannot act on (it only knows how to split an EntrySet); `split_css_by_defs`
  drains that list as the pass's **last rung**, gated on quiescence of every
  stage above, so anything a finer route can separate is separated first.
  It was a small change because `cs->defs` is *exactly* the set of AVars
  whose `cs_map` names `cs` (`fa.cc:868-869`), so `split_css`'s existing
  re-point applies unchanged — no new state, no new invariant, no
  attribution. Default-safe by measurement, not assumption: at the default
  no CreationSet has more than one creation point (`multidef=0` over
  127 522 CreationSets), so the `defs > 1` test declines everything.
  Took ifa/129's suite bill 16 → 11.
- **`compute_setters` counted a `None` store as no store** and
  `P_prim_merge` was opaque to the container graph. Fixed, `PYC_NILSTORE=0`
  restores the old behaviour for attribution. Default arm: all 77 corpus
  rows byte-identical. Flag arm: exactly 3 programs move, established by
  byte-identical `DEMAND` lines on the other 74 — `richards` fixed
  (SIGSEGV → runs), `pygasus` 53 → 3 warnings, `chull` from a silent
  runtime segfault to a compile-time layout refusal.

## The central obstacle: the demand is unobservable at the merge

This is the sharpest statement of why "split only on demand" cannot by
itself be the whole story for container literals.

The identity rule is a **joiner** and a **separator**, and they exist for
different reasons. The joiner is one CreationSet per sym (128) — deliberate,
per CLAUDE.md's premise of starting from minimum contours. The separator is
**arity** ([132](132-arity-is-representation-not-provenance.md)) — carved
back out because merging different arities is UNREPRESENTABLE, which is
CLAUDE.md's legitimate third category (*what the target language can
represent*), not provenance.

**Arity is currently the only working separator for list literals.** The
natural sibling would be the literal's slot TYPE — but that does not raise a
demand at the merge: at `["seed"]` vs `[None]`, slot 0 is `{str, None}`,
`elem_irrepresentable` skips nil and finds a single basic, so the merge is
representable and nothing objects. The union only becomes irrepresentable
much later and elsewhere: `list.__mul__` pours slot 0 into the result's
element, `__setitem__` adds a class, and `{str, Body}` is finally a demand —
on a different CreationSet, several contours downstream, by which time the
literals are long merged and every creation point carries the whole union.

> **The demand is unobservable at the moment the merge happens, and the
> merge is unrecoverable at the moment the demand appears.**

That is the same lag that defeated every violation-gated experiment here.
It is **not** an argument for splitting literals eagerly — that is the fan
146 exists to remove. It is an argument that the **separator set is too
small**: arity is one representation property doing all the work alone. The
question worth asking is which OTHER representation properties of a literal
are knowable at construction and would keep these apart — the slot's
basic-type kind being the obvious candidate, since `str` and nil differ
there even though their union is representable. That is a question about
REPRESENTATION, so it sidesteps the lag entirely.

## Do not build a sixth splitter

The residual family is a container with ONE creation point whose element
receives two types through paths that agree on argument types and on call
site: `plcfrs`, `rdb` (flag-only), `linalg`, `voronoi2`, `sudoku3`,
`quameon`. Neither CS-side partitioning (nothing to partition) nor ES-side
splitting (nothing to split on) can separate that — **merging destroyed the
attribution**, which is the phrase other issues cite this one for
([143](143-shared-container-method-contours-refuse-cs-splits.md)): once the
element channel holds both types there is nothing left recording which
creation point contributed which. Five mechanisms were
built and measured against it and **none reaches it**:

| | result |
| --- | --- |
| `CS_DEF_PARTITION` (route 4) | splits, but these are `defs=1` |
| ladder routes 1 and 3 | −45 contours corpus-wide, no verdicts |
| contour reuse | fires on 2 of 9 programs, no totals change |
| `PYC_ESFORCS` / type-side third clause | 427 of 428 decline `no_groups` |
| `PYC_CSCALLSITE` demand partition | fixed 1 program, 0 net |

**What has actually moved this family is finding the specific upstream merge
and fixing it** — `__delitem__`'s false `merge_in`, `heapq`'s
`[n-1:n] = []`, ifa/139's arity hole. Each was a single wrong edge, found
with a probe, not a new mechanism.

## The EntrySet split as a MEANS — `PYC_ESBLOCK=1`, opt-in

CLAUDE.md: *an EntrySet is split SO THAT a CreationSet split becomes
possible, when a demand test has asked for one; never as a reason to create
data contours.*

Route 4 declines `"1 group: every creation point on the same assign sets"`
for two very different reasons and could not tell them apart: *these
creation points are alike*, or *a contour they all pass through is SHARED,
so the walk cannot tell them apart*. On
`tests/listcomp_element_separation.py` it is the second — both
comprehensions' `append` share `es=60` because both accumulators ARE the
`list#1060` being split. The `FUNES` dump also settles why every earlier
attempt failed: the two in-edges come from the SAME caller contour with
IDENTICAL actual types at every position, so there is nothing type-shaped
to split on and any key over call sites degenerates to one group per edge —
ifa/144's fan.

Three design points, each answering a specific past failure:

1. **Find the blocker BY TEST**, not by climbing an ancestor chain.
   Candidates are AVars on the walk that are FORMALS of a contour with more
   than one in-edge; hold each one's formals terminal and recompute the
   per-def signatures — if the creation points then separate, that contour
   is the blocker.
2. **Partition its in-edges by WHICH CREATION POINTS each reaches**, with
   the formals held terminal so the walk cannot re-enter through another
   caller. The parts are the demand's own objects.
3. **Take exactly TWO groups.** Bounding by `defs.n` does not fix the fan —
   six creation points license six groups and the grouping hands back one
   per edge (`plcfrs`: `es=59 append edges=7 -> 6 groups`). Two is the
   minimum that makes route 4's partition possible, so the first signature
   keeps the contour and everything else peels onto one product; if more
   separation is needed the demand survives re-derivation and the next pass
   splits again. Partition size is 2 by construction and can never track
   the caller count.

It works, and route 4 then does the CreationSet split itself on the next
pass — every `append` contour ends with a single-type value formal and the
`{A, B}` union is gone. `tests/listcomp_element_separation_startmerged.py`
pins it and is load-bearing: 0 warnings with the flag, 1 without.

Corpus, one binary with the env toggled (a real A/B, not the cross-binary
comparison [147](147-analysis-result-depends-on-the-binary-not-the-inputs.md)
invalidates):

| arm | cfail | warns | container CS |
| --- | --- | --- | --- |
| default | 2 | 43 | 2740 |
| default + ESBLOCK | 2 | **42** | 2748 |
| flag | 2 | 45 | 2406 |
| flag + ESBLOCK | **4** (+plcfrs, sudoku5) | **42** | 2392 |

Fewer warnings on both arms. The two lost programs are traced, not guessed:
splitting a contour multiplies CreationSets (128's complaint), `sudoku5`'s
worst `tuple.__eq__`/`__lt__` contour goes from 8 tuple shapes to 22, and
that ONE shared contour's unrolled body has a single `x` per slot position,
so slot types across shapes merge into `{int64, str}` and every use is a
BOXING violation.

**That merge is [146](146-remove-all-arbitrary-splitting.md) E's debt, and
146 E is the blocker — not 128.** Separating a formal that holds N tuple
CreationSets is exactly what the removed CARTESIAN_PRODUCT splitter did, and
`tests/splitter_cartesian_product.py` already specifies the replacement.
This file previously said landing 128's reuse would unblock it; 128's own
measurements retract that — reducing the number of tuple CreationSets
concentrates MORE shapes onto each comparison contour and pushes the wrong
way. See 128's closing section.

## Do not retry

- **`PYC_CSCALLSITE=3`** — one group per edge, partition size = caller
  count, ifa/144's signature verbatim. Its two dampers were scaffolding:
  `elem_representable_classes` returned 5 where the demand was 2, and "the
  demand must survive a re-derivation" is a timing knob true of every union
  after the first pass. The tell needed no sweep: with the fan removed the
  climb that fed it splits **nothing** (`climbs=4 hops=4 split=0`).
- **`PYC_WALKCTX`** (matched call/return on the backflow walk) — answers the
  wrong question. Route 4 asks **may-reach**; when a contour is SHARED its
  formal genuinely holds every caller's container, so the unrestricted walk
  is correct and matched call/return computes a strictly smaller, wrong
  answer. It "fixed" `listcomp_element_separation` only because the smaller
  answer happened to separate the defs. All three implementation defects
  found along the way made it under-approximate FURTHER, which is the
  signature of a wrong premise.
- **Never attribute a 1–2 program corpus movement to a change.** Adding
  provably dead, `getenv`-gated code to `fa.cc` moves `linalg` and `sudoku5`
  at the default arm — see 147. Only same-binary, env-toggled A/B counts.

## Open

1. The representation question above: which other properties of a literal,
   knowable at construction, belong in the separator set beside arity.
2. 146 E's receiver separation, which unblocks `PYC_ESBLOCK` and with it the
   flag flip.
3. `builtins` under the flag — the offending `list` CreationSet never
   reaches `CS_DEF_PARTITION` because an earlier stage claims progress every
   pass, so the last rung is never gated in. A scheduling question, not a
   partitioning one.
