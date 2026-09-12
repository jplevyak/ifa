# 142 — linalg: the empty-list collapse reaches a self-referential fixed point

**Status:** root-caused 2026-09-07, not fixed. `linalg` under
`PYC_CSDCPA1=2`. The cleanest instance of
[133](133-split-a-container-on-its-element-type.md)'s family, and the one
that shows why every mechanism built for it declines.

## Symptom

```
linalg.py:148:2498: warning: illegal call argument type 'n' illegal: list
        m=n//2
fail: a variable holding {int64, list, list, list, list, list, list, list, list, list}
      has no representation: '__add__' resolved to the CONTAINER method
```

`binary(n)` is a plain recursive int function with two call sites —
`binary(t)` where `t` iterates `reversed(range(1025))`, and `binary(m)`
where `m = n//2`. `n` should be `int64` and holds `{int64, list x9}`.

## The chain

`reversed` (`__pyc__/05_builtins.py:306`) is

```python
def reversed(seq):
    r = []
    ...
    r.append(seq[i])
    return r
```

so its result carries whatever `r`'s CreationSet holds. Under one
CreationSet per sym, that `[]` is one of **26** arity-0 creation points
sharing `cs=1011`:

```
CSVARS cs=1011 sym=list vars=0 defs=26 arity=0 elem= int64 list x9
  ELEMWRITER es=89  fun=__setitem__      type= int64 list#1011 list#1505 ...
  ELEMWRITER es=356 fun=__add__          type= int64 list#1011 list#1505 ...
  ELEMWRITER es=207 fun=append           type= int64 list#1011 list#1505 ...
  ...
```

**The element contains the CreationSet itself** (`list#1011`) — linalg
builds lists of lists, and the inner `[]` and the outer `[]` are both
arity 0, so they are the same contour. And **every writer carries the
identical full union**.

## Why nothing separates it

`IFA_DBG_CSFLOW`:

```
CSFLOW cs=1011 defs=26 sets=6 csites=20 (in_defs=19) empty=7
  set[0] type= int64 list x9   targets=17  path=497  cps=20
  set[1] type= list int64 list targets=1   path=29   cps=2
  set[2] type= int64           targets=1   path=48   cps=3
```

- **Route 4 (wholesale) declined on the cap**, from pass 0 onward:
  `[csdefsplit] p=0 cs=1011 defs=17 DECLINED (over cap)`. It is already
  17 defs before the first split stage runs, and grows to 32. (The cap
  was removed 2026-09-07 — see below; this trace is the pre-fix state.)
- **Routes 1 and 3 cannot separate it.** `set[0]` carries the FULL union
  and covers 17 of the 20 targets and all 20 creation points. A
  no-confusion split needs sites on exactly one assign set; a path
  partition groups by the union of types on a site's paths — and
  `set[0]`'s union subsumes every other set, so nearly every site shares
  one signature.

**This is the same fixed point `tests/splitter_mark_type.py`'s header
describes, one level down.** There it is a formal: *"once {A,B} forms at
append's value formal it is a fixed point — every edge carries {A,B}, so
etype == stype and TYPE_CONFLUENCE has nothing left to see."* Here it is
a CreationSet's element channel: once the union forms, every writer
carries it, so no type-based partition can tell the contributors apart.

## What fixes it: route 4, uncapped (landed 2026-09-07)

Only separation by creation point can break a fixed point that every
type-based test sees as uniform — and that is exactly route 4, which the
cap refused. **The cap has been removed** and `linalg` compiles.

### Correcting what this section said first

It said: *"`linalg` compiles with the cap removed, and then aborts at run
time (`rc=134`)... raising the cap buys compile status, not working
programs. `linalg` needs the collapse PREVENTED rather than partitioned
afterwards — the 26 arity-0 `[]` literals should not have become one
contour in the first place."*

Every clause of that is wrong, and the second one is wrong about the
design rather than about a measurement:

- **The run-time abort is not a regression.** `linalg` aborts with
  `run_rc=134` **at the default too**, as do `quameon` and `sudoku3`;
  `voronoi2` prints the wrong answer at the default too. Uncapping brings
  all four to *parity* with the baseline the flag has to match. The
  comparison had been against a standard the default does not meet.
- **`+15%` was flag-vs-flag.** `2835 → 3273` measures the uncapped arm
  against the *capped* arm. Against the DEFAULT's `3713` — the only
  baseline that means anything — the uncapped arm is **−11.9%**.
- **"the 26 literals should not have become one contour" inverts the
  design.** Preventing the collapse means keeping per-creation-point
  identity, which is exactly what `PYC_CSDCPA1` exists to remove. Twenty-six
  arity-0 `[]` literals starting as ONE CreationSet is the CORRECT initial
  state; the goal is minimal contours **subject to demand**. The demand
  here is real — those 26 literals hold genuinely different element types —
  so 26 contours is the right answer and route 4 is the mechanism that
  reaches it. The defect was never the merge. It was that the cap
  forbade the only mechanism able to undo it.

That last point is what makes this issue's fixed-point finding matter
rather than damning: a fixed point that no type-based test can see is
precisely the case where separation by creation point is not a fallback
but the *only* correct instrument. Capping it at 10 disabled it on the
programs whose merges were widest — i.e. wherever it was most needed.

Corpus effect of the removal: programs whose verdict differs from the
default drop from **11 to 4** (`plcfrs`, `rdb`, `bh`, `kanoodle`), with
`bh`/`kanoodle` unrelated to the cap. See
[146](146-remove-all-arbitrary-splitting.md)'s removal table for the lever
itself (133's blow-by-blow of the cap was compacted away 2026-09-12).

## What is NOT the cause

- Not `reversed` — it has ONE contour here, called only with `range`.
  Its `r = []` is a victim of the shared contour, not the source.
- Not `binary` — one contour, two call sites, both legitimately `int`.
- Not an arity bug: `cs=1011` is arity 0 throughout, and
  [141](141-unrecorded-arity-is-not-varying-arity.md)'s tightening does
  not touch it.
