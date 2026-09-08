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

## Fix 1 landed 2026-09-07: partition where the demand names groups

`split_css_by_defs` now groups `cs->defs` by **assign-set signature** —
which of the `CSFlowGraph`'s assign sets a creation point's backflow path
lies on — and mints one contour per GROUP rather than one per creation
point. Two creation points on exactly the same assign sets cannot be told
apart by any element-type test that reaches this rung, so separating them
is unjustified: demand deciding compatibility, not provenance deciding
identity.

`PYC_CSDEFPART`: `0` = the old fan, `1` = partition and decline when
nothing names a partition, `2` = partition with the fan as fallback
(**default**).

**Mode 1 was measured first and is a RETREAT.** With no flow graph there
is no signature, every def lands in one group, and the rung declines — and
`build_cs_flow_graph` returns null for every NON-CONTAINER CreationSet,
because it needs an element channel. On `bh`, 15 of 17 declines were "no
flow graph". Declining there turns the fan into nothing at all, and it
cost five programs.

Corpus, flag arm (`PYC_CSDCPA1=2 PYC_CSLADDER=3`), against the default:

| | programs differing from default | container CS |
| --- | --- | --- |
| fan (old) | 4 — `bh`, `kanoodle`, `quameon`, `rdb` | 3281 (−12%) |
| partition everywhere (`=1`) | 8 — adds `chull`, `richards`, `sudoku2`, `sudoku3`, `sudoku4` | 2375 (−37%) |
| **partition + fan fallback (`=2`)** | **5** — `bh`, `chull`, `kanoodle`, `rdb`, `sudoku5` | **2328 (−38%)** |
| default | 0 | 3713 |

Mode 2 carries the SAME number of regressions as the old fan (four, with
`chull` replacing `quameon`), and its fifth difference is an IMPROVEMENT:

| program | default | flag + fan | flag + partition |
| --- | --- | --- | --- |
| `quameon` | `run:134` | COMPILE-FAIL | `run:134` — back to parity |
| `sudoku5` | **COMPILE-FAIL** | COMPILE-FAIL | **WRONG-OUT** — compiles and runs where the default cannot compile it |
| `chull` | `run:1` | `run:1` | COMPILE-FAIL — the one new regression |

Contours drop from −12% to **−38%** against the default, the largest
reduction measured in this effort. Default arm byte-identical on all 77
programs; all six CI gates pass.

On `bh` specifically, route-4 mints go 25 → 8 and container contours 20 →
17, at a cost of 33 → 45 passes.

### Still open after fix 1

- **`chull`** — new regression, `run:1` → COMPILE-FAIL. Not yet
  investigated.
- **Fix 2, the compatibility re-join, is still needed.** The partition only
  avoids creating duplicates where a flow graph names the groups; it cannot
  collapse duplicates that converge later, and it does nothing for the
  non-container fan that mode 2 retains. `Vec3` on `bh` still ends at 21
  contours.
- The grouping key is the coarsest one the flow graph offers. A finer key —
  the actual element types a site contributes, rather than which assign
  sets it lies on — would partition more precisely and might recover
  `chull`.

## ROOT CAUSE of the excess, traced 2026-09-08

The question "what is splitting them?" has a stage answer and a real
answer, and the stage answer is a decoy.

### The stage answer, and why it is a decoy

`IFA_DBG_STAGE` on `bh`, every CreationSet mint attributed:

```
CS_DEF_PART        csmint=23
SETTER_OF_SETTER   csmint=3
MARK_SETTER        csmint=1
```

24 of route 4's mints break down as **16 `Vec3`, all through the FAN
fallback** (a plain class has no element channel, `build_cs_flow_graph`
returns null, so fix 1's partition has no signature to group on) and 8
`list`, all through the partition. `Vec3` ends at 20 contours with **3
distinct member signatures, 18 of them byte-identical**.

That looks like route 4 is the culprit. It is not. Suppressing the
confluence that feeds it moves the work rather than removing it:

```
                     CS_DEF_PART   SETTER
  as shipped            23           0
  numeric mix withheld   7          15
```

`Vec3` then ends at **21** contours, one MORE, still 3 signatures. **The
SETTER stage answers the same demand the same way.** Route 4 is not "the"
splitter; it is whichever stage reaches the demand first, and they all
fan. A guard on any one of them is a retreat that relocates the split —
it was written, measured, and reverted for exactly that reason.

### The real cause: a permanent numeric confluence that splitting cannot resolve

`IFA_DBG_TCDROP` on `bh`: **150 of `Vec3`'s 156 confluence deferrals carry
`type= int64 float64`.** The source is in the program:

```python
class Vec3:
    def __init__(self):
        self.d0 = 0.0            # float
    def __setitem__(self, i, v):
        self.d0 = v              # whatever the caller passes
...
    xp[0] = floor(Node.IMAX * xsc)   # int
```

So the members genuinely hold `{int64, float64}`. `coerce_annotate` exists
for exactly this and **does fire** on `d0`, `d1` and `d2` — but it only
reaches *constants*. Its own comment says so:

> Runtime (non-constant) narrow members are left alone -- they would need
> an inserted conversion -- so their violations persist and are reported
> honestly.

`floor(...)` is a runtime value. So the mix never coerces, the confluence
is **permanent**, and it re-fires every pass for the life of the analysis.
Every splitting stage treats a permanent confluence as a permanent demand
and answers it by separating creation points — and every one of the 18
resulting contours converges to `d0 d1 d2 = float64` regardless. **The
splits answer a demand that splitting cannot resolve.**

### What follows

- The excess is not a defect of route 4's action alone. Fix 1 (partition
  instead of fan) is still right and still needed, but it cannot reach
  this: there is no flow graph for a plain class, and no grouping key
  short of one.
- The demand itself is the thing to fix. A `{int64, float64}` member mix
  needs an INSERTED CONVERSION at the narrow write, which is the work
  `coerce_annotate` explicitly declines for runtime values. Until that
  exists, no contour partition can retire the confluence, because both
  creation points really do see both types.
- Until then, any stage-level guard just hands the fan to the next stage.
  Measured; do not re-attempt without fixing the demand.

**This supersedes the earlier reading in this issue that "route 4 answers
a demand by separating maximally" fully explains the excess.** That is true
of the `list` half and of the mechanism, and it is NOT the whole story for
the `Vec3` half, which is 16 of the 24 mints.

## How shedskin handles it, and what the fix must respect (2026-09-08)

**In `bh`, shedskin never creates the mix — because its `floor` is wrong.**

```cpp
inline __ss_float floor(__ss_float x) {   // shedskin/lib/math/__init__.hpp:36
    return std::floor(x);
}
```

shedskin's `math.floor` returns a **double**; CPython 3's returns an
**int**, and pyc matches CPython (`pyc_lib/math.py:32`). So shedskin's
`xp[0] = floor(...)` writes a float into a float member and the confluence
never arises. **pyc is correct here and shedskin is not** — which qualifies
ifa/143's comparison: shedskin's clean typing of `bh` is bought partly by a
semantic deviation, and is not available to pyc without breaking
`math.floor`.

**When a mix genuinely occurs, shedskin widens and inserts a conversion.**
On a runtime int-into-float member write it emits one member type, one
method, and an explicit cast — no contour splitting anywhere:

```cpp
__ss_float d;
void *_set(__ss_float v);
b->_set( ((__ss_float)(len(__sys__::argv))) );
```

That is exactly the work `coerce_annotate` declines for runtime values.

### Confirmed by inserting the conversion in the source

Six `float(floor(...))` edits to a scratch copy of `bh.py`, flag arm:

| | as-is | with `float()` |
| --- | --- | --- |
| `Vec3` `{int64,float64}` confluences | 150 | **0** |
| route-4 mints | 24 | **7** |
| `Vec3` contours | 20 (18 identical) | **6** (4 identical) |
| `ess` / `css` | 412 / 1164 | 366 / 1134 |

The whole `Vec3` waste, gone. The `str` warnings and `run_rc=134` are
UNCHANGED by it, which is the cleanest demonstration that this issue and
ifa/143 are independent; fixing the `__slots__` merge as well gives a clean
compile and a correct run.

This is a diagnosis, not a patch: `float(floor(x))` is not what the program
says, and the corpus is the benchmark. The fix belongs in pyc.

### The constraint on that fix

**Author's directive: any automatic coercion must be PERMISSIVE ONLY.**
pyc's `--strict` promises *"hard compile errors on type violations, no
permissive-Python fallbacks"*, and silently widening an int member to
float is such a fallback.

The existing coercion already violates this — it consults no mode flag and
fires in `--strict`, printing `1.5 1.0` where CPython prints `1.5 1`. Filed
as [145](145-numeric-coercion-is-not-gated-on-permissive-mode.md). Whatever
retires this issue's contour waste has to be gated the same way, or it
compounds 145 rather than fixing it: in strict mode the mix must be
REPORTED, not widened.

## Attempted fix 2, and why it does not work as stated (2026-09-08)

The obvious reading of the root cause is: *a pure-numeric mix is not a
demand for a contour split, so make the splitters skip it and let the
coercion phase resolve it.* **Implemented, measured, reverted.**

The guard was `av_pure_numeric_mix(av)` applied at both confluence
collectors — `collect_type_confluence` (stage 1) and
`collect_setter_confluences` (SETTER) — permissive-gated on
`fruntime_errors` per the directive in
[145](145-numeric-coercion-is-not-gated-on-permissive-mode.md), since the
guard asserts that coercion WILL resolve the mix and that is only true
where coercion runs.

Guarding one collector alone was already known to be useless: with
`CS_DEF_PART` blocked, `SETTER` picked the same demand up and minted 15
CreationSets for it (`bh`: `CS_DEF_PART 23 -> 7`, `SETTER 0 -> 15`).
Guarding both does stop the relocation, and it is still wrong:

| | baseline | both collectors guarded | source-level `float()` |
| --- | --- | --- | --- |
| `bh` flag-arm compile | 0 | **1 — FAILS** | 0 |
| route-4 mints | 24 | 15 | **7** |
| `css` | 1164 | 1157 | **1134** |

```
bh.py.c:3724:9: error: invalid operands to binary expression
                       ('int' and '_CG_float64' (aka 'double'))
```

**Diagnosed properly 2026-09-08 by reading the emitted C**, and the first
reading of it here ("splitting and coercion do not cover the same AVars")
was WRONG. Coercion does reach this value. It types it, and it types it
INCORRECTLY:

```c
t74 = _CG_prim_add(t70, "+", 1);      // k + 1   -- t74 is a double
t73 = _CG_prim_rsh(8, ">>", t74);     // Cell.NSUB >> (k + 1)
```

from `Node.old_sub_index`:

```python
for k in range(Vec3.NDIM):
    if (int(ic[k]) & l) != 0:
        i += Cell.NSUB >> (k + 1)
```

With the demand withdrawn the contours merge, coercion widens the shift
operand to `float64`, and `>>` on a double is not C. **The split was doing
real work: it separated a contour where the value is int-only from one
where it is float, and coercion is the wrong answer for the int-only one.**

So the premise behind this whole line of attack is false. A pure-numeric
mix is NOT uniformly "resolvable by coercion" — it is resolvable by
coercion only where nothing requires the narrow type. Where the value feeds
an integer-only operation (`>>`, `&`, an index), widening is invalid and
SPLITTING is the correct response. `bh` contains both cases at once:
`Vec3`'s members (float-only uses, 18 identical contours of pure waste) and
`old_sub_index`'s `k` (int-only use, a legitimate split).

The discriminator is therefore not "is this mix pure-numeric" but **"does
any use of this value forbid widening"** — a question about the uses, which
neither the confluence collectors nor `coerce_annotate` currently ask.
That is a real and principled criterion, and a substantially larger change
than either piece attempted here.

So this issue's remaining waste is blocked on a use-sensitive criterion,
not on ifa/145 piece 2 as that was scoped — and 145 piece 2 was itself
misscoped: `type_coerce_numeric_constants` ALREADY handles runtime
(non-constant) values, replacing the narrow CreationSet with the wide one
in its `else` branch. Nothing needs extending there. What is missing is the
question "may this value be widened at all?".

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
