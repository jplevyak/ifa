# 144 — route 4 fans one contour per creation point, and nothing re-joins the duplicates

**Status: open.** Root-caused 2026-09-07 on `bh`, measured against shedskin
on the same program.

## Symptom: contours that differ by nothing

`bh` under `PYC_CSDCPA1=2 PYC_CSLADDER=3` ends with **20 `Vec3`
CreationSets carrying 3 distinct member signatures** — 18 of the 20 are
byte-identical across all 29 members:

```
CSVARS cs=1244 sym=Vec3 vars=29 defs=1 arity=-1 no_arity=0 elem=(none)
CSVARS cs=1553 sym=Vec3 vars=29 defs=1 arity=-1 no_arity=0 elem=(none)
CSVARS cs=1643 sym=Vec3 vars=29 defs=1 arity=-1 no_arity=0 elem=(none)
...  18 of these
```

Diffing two of them, the ONLY line that differs is which creation point
made it:

```
-  DEF av=8626  es=153 fun=__pyc_copy__
+  DEF av=17934 es=398 fun=__new__
```

**That is provenance and nothing else** — the thing CLAUDE.md forbids as a
reason to split. `Vec3` holds three floats; every one of the 18 agrees on
all 29 member types.

Lists are the same shape. 12 container contours across 6 element shapes:

```
(empty)                    x3
Body                       x1
str                        x1
str Body                   x4     <- four identical
str Body Cell Cell Cell    x2     <- two identical
tuple                      x1
```

shedskin compiles the same program with **3** list instantiations
(ifa/143). pyc's `container_cs` for `bh` is 20 under the flag, 25 at the
default.

## Root cause 1: the rung answers a demand by separating MAXIMALLY

`split_css_by_defs` (shedskin's route 4) is reached only on a demand — a
type confluence on the contour, or, since 2026-09-07, an irrepresentable
element union. That gate is correct: something did observe a distinction.

What is wrong is the ACTION. The demand says *"these must be separated."*
Route 4 answers *"all N are mutually distinct"* and gives every creation
point its own contour ("the first creation point keeps the CreationSet;
every other one gets its own"). It never asks which creation points would
converge to the same content, so a demand to separate two groups is
answered with N contours.

Measured on `bh` — 25 route-4 mints, from just two parents:

| parent | sym | contours minted |
| --- | --- | --- |
| `cs=1244` | `Vec3` | 16 |
| `cs=1180` | `list` | 9 |

And it re-fires as new creation points accumulate on the residual parent:
`Vec3` fans at p=0, p=3, p=6 (x2), p=8, then eight more at p=9.

Routes 1 and 3 do partition into GROUPS — by assign set, by path
signature. Route 4 is the wholesale fallback, and shedskin bounds it at
`1 < len(csites) < 10` (`infer.py:1576`) precisely to limit this damage.

**This is the same defect class as the deleted `PYC_RECVFAN`** — partition
size determined by a count of things rather than by the distinction
demanded — except that here it sits inside a rung whose gate is
legitimate. Removing `kCsDefSplitMax` (ifa/133, 2026-09-07) removed the
only brake on the fan.

**Restoring the cap is NOT the fix.** `linalg` needs a genuine 44-way
separation and the cap is what refused it; that measurement stands. The
fix is for route 4 to partition into the groups the demand actually
distinguishes, rather than to fan.

## Root cause 2: nothing re-joins contours that converge

Even a maximal fan would be harmless if contours that turn out identical
could re-merge. They cannot.

The one re-join in the tree, `cselem_rejoin_unknown_mints`, is:

- **opt-in** — gated on `cselem_enabled() != 3`, i.e. `PYC_CSELEM=3`, not
  the default;
- **narrow** — it only re-points sites in `cselem_unknown_mints`, those
  whose element shape was unknown at MINT time and became known later;
- **explicitly forbidden from touching split products** — *"split_css may
  have moved it since, and re-pointing then would undo that split -- a
  demand-driven separation, which is the one thing this must never quietly
  reverse."*

That last guard is right about a split the demand still justifies, and
wrong as a blanket rule: once two split products converge to identical
content, nothing observes a distinction between them any more, and the
demand that justified separating them no longer distinguishes them.

Under the three-way rule (CLAUDE.md): identity may be as fine as it likes,
but **compatibility is decided by demand** — and two contours agreeing on
every member type have no demand keeping them apart. CLAUDE.md already
records that a re-join reversing 36 such decisions corpus-wide left every
verdict unchanged.

## What the fix has to be

Two candidates, not exclusive:

1. **Partition, don't fan.** Group `cs->defs` by the content they
   contribute and mint one contour per GROUP. The demand names the groups
   — for an irrepresentable element union, the parts of that union.
2. **A general compatibility re-join.** Two CreationSets of the same sym
   whose member types and element type agree are compatible; collapse
   them. This is the split-back direction ifa/129 step 4 describes, applied
   to split products rather than only to unknown mints.

(2) is the safer of the two to measure first: it is observable
after-the-fact, needs no change to how the rung decides, and its
correctness argument is exactly the compatibility leg of the three-way
rule. (1) is the better end state, because it never creates the waste.

## Reproducer

```sh
cd shedskin_examples/bh
PYC_CSDCPA1=2 PYC_CSLADDER=3 IFA_DBG_CSDEFSPLIT=1 \
  ../../pyc -D ../.. bh.py 2>&1 | grep -c "def av="          # 25 mints
PYC_CSDCPA1=2 PYC_CSLADDER=3 IFA_DBG_CSVARS=Vec3 \
  ../../pyc -D ../.. bh.py 2>&1 | grep -c "sym=Vec3 vars=29" # identical shapes
```

Note `IFA_DBG_ELEMTYPE_DUMP` needs `IFA_DBG_ELEMTYPE` set as well; alone it
prints nothing, which reads as "no contours".
