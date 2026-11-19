# ifa/152 — backtrack the demand to the CreationSet that actually merged

**Status:** landed. `PYC_CSBACKTRACK` is **ON BY DEFAULT** since 2026-09-15
(`=0` disables) — see
[154](154-FA-a-container-has-two-content-channels.md), which found the filter
bug that was blinding it to a container's positional slots and measured the
pair to a default. Measured on `chull`. Part of
[129](129-plan-demand-driven-creation-set-splitting.md)'s ladder.

## The defect, in one sentence

**A demand is observed where the union is USED, and that is almost never
where the merge HAPPENED** — so the CreationSet that must split is never
a candidate, and route 4 declines forever on the one that cannot.

Route 4's candidate set is built from two sources: CreationSets a
`TYPE_CONFLUENCE` marked, and (ifa/143) every live CreationSet with
`cs_elem_irrepresentable(cs)`. Both ask the same question — *is THIS
CreationSet's content irrepresentable?* The merged CreationSet is
upstream and is usually **perfectly representable on its own**, so it
answers no, is never nominated, and nothing in the ladder can reach it.

## Measured on `chull`

`chull` failed to compile with five copies of

```
error: object layout: 'Edge' is blind-cast to 'Vertex' and read at e23,
       but member width differs at e22 (_CG_bool vs _CG_void)
```

The source chain, fully traced:

```python
class Edge:
    def __init__(self, adjface=[None,None], endpts=[None,None], ...):
        self.endpts = []                    # <-- arity-0 literal
        self.endpts.extend(endpts)          # <-- fills it with Vertex

class Face:
    def InitEdges(self, fold=None):
        newedges = []                       # <-- THE SAME CreationSet
        if fold is None:
            newedges = [e0,e1,e2]           # arity 3, separate contour
        ...
        return newedges                     # returns the arity-0 one when fold is not None

class Hull:
    def DoubleTriangle(self):
        self.edges.extend(f1.InitEdges(f0))  # Vertex lands in Hull.edges
```

`newedges = []` and `Edge.__init__`'s `self.endpts = []` are **one
CreationSet**, `cs=1112`, along with seven more creation points:

```
CSVARS cs=1112 sym=list vars=0 defs=9 arity=0 elem= Vertex
  DEF es=658 fun=InitEdges          <- newedges = []
  DEF es=138 fun=InitEdges
  DEF es=695 fun=__init__           <- Edge.__init__: self.endpts = []
  DEF es=696 fun=__init__
  DEF es=11/12/41 fun=___init___    <- list.___init___'s own []
  DEF es=198 fun=__pyc_delslice__
```

`extend` fills it with `Vertex`; `InitEdges` returns it unwritten; so
`Hull.edges` inherits a Vertex and its element becomes `{Vertex, Edge}`.

### Why the ladder could not see it

`cs=1112`'s element is `{Vertex}` — **one class, perfectly
representable**. It raises no demand and is not a candidate. Meanwhile
the five CreationSets that DO carry the irrepresentable union are all
single-creation-point and decline on every pass:

```
[csdefsplit] p=49 cs=1848 sym=list defs=1 DECLINED (single creation point)
[csdefsplit] p=49 cs=1876 sym=list defs=1 DECLINED (single creation point)
[csdefsplit] p=49 cs=1887 sym=list defs=1 DECLINED (single creation point)
[csdefsplit] p=49 cs=1892 sym=list defs=1 DECLINED (single creation point)
[csdefsplit] p=49 cs=1920 sym=list defs=1 DECLINED (single creation point)
```

Fifty passes, five demands, zero progress — and the answer sitting one
backward walk away.

### The walk names it uniquely

A read-only probe at the decline site, walking `AVar::backward` from the
demanded content and reporting every CreationSet contour on the path with
`defs >= 2`, returns **exactly one** for all five:

```
BACKTRACK p=49 cs=1848 elem= Vertex#1191 Edge#1276 Edge#1896 Edge#1971 Edge#1972 -> upstream=18
    up cs=1112 sym=list defs=9 elem= Vertex#1191
BACKTRACK p=49 cs=1876 ... -> upstream=25
    up cs=1112 sym=list defs=9 elem= Vertex#1191
BACKTRACK p=49 cs=1887 ... -> upstream=18
    up cs=1112 sym=list defs=9 elem= Vertex#1191
```

Not a shortlist to guess among — one CreationSet, named by the demand's
own backward flow. That is the measurement this issue rests on.

## The mechanism

`PYC_CSBACKTRACK=1`, in route 4's candidate gathering
(`ifa/analysis/fa.cc`, `split_css_by_defs`): for every candidate whose
live def count is `< 2`, walk the value flow backward from its content
AVars and nominate any CreationSet with `>= 2` live creation points
**that supplies one of the offending types**.

Two filters, and both matter:

- **`cs_live_defs(cs) < 2`** on the source. The walk only ever runs from
  a CreationSet that has already declined, so it is the decline itself
  that triggers it.
- **"supplies one of the offending types"** on the target. Without it the
  walk nominates whatever the value flow passes through — measured at 49
  nominations on `chull`, including `sym=Vector` and
  `sym=__tuple_iter__`, which have no bearing on `{Vertex, Edge}`. With
  it, **14**. A walk that ignores the demand's own types is splitting by
  reach, which is arbitrary however well it converges.

### Why this is a mechanism and not provenance

[146](146-remove-all-arbitrary-splitting.md)'s two-question test:

- **Would this split happen if the demand were absent?** No. The walk
  runs only from a candidate that was nominated by a demand and then
  declined for want of anything to partition. Take the demand away and
  nothing is nominated.
- **Does the demand alone decide WHETHER, with the handle only deciding
  WHICH?** Yes. The demand decides whether; the handle is "the offending
  element flows from here", which is a statement about **value flow and
  deduced types** — not about where a value was born. No allocation site,
  no display, no mark, no call site enters the key.

This is the direct implementation of CLAUDE.md's directive *"Find the
confluence, backtrack the demand, split. Always."* The confluence is
`cs=1112`; the demand is at `cs=1848`; the walk is the backtrack.

## Result on `chull`

| | flag off | flag on |
| --- | --- | --- |
| compile | **fails** — 5 blind casts | rc=0, **0 errors, 0 warnings** |
| element channels | five lists hold `{Vertex, Edge}` | every list holds `Vertex` **or** `Edge`, never both |
| nominations | — | 14 |
| class contours | Vertex 3, Vector 5, Edge 6, Face 5 | Vertex 3, Vector **6**, Edge 6, Face 5 |

The union is genuinely gone, not silenced: the element channels are
clean, and class contours barely move (Vector 5 → 6), so this is not a
contour explosion buying the result.

Suite: **316 passed / 0 failed on both backends with the flag ON**, and
all six CI gates green with it off.

## The second half: the stall guard truncates the repair

The first corpus A/B fixed `chull` and broke **`quameon`** — 6 C errors of
the shape `assigning to '_CG_void' from incompatible type 'double'`, on
`self.coeff = [[1.0]]`'s inner arity-1 literal, whose slot 0 came out
untyped.

It is **not a bad split.** Tracing the same CreationSet in both arms:

| | flag off | flag on |
| --- | --- | --- |
| final pass | 80 | **40** |
| `pass_limit_hit` | 0 | **1** |
| violations | **0** | 34 |
| `cs=1561` (the arity-1 literal) | `defs=1`, slot `float64` | `defs=10`, slot `{float64, list, list, atomic_STO}` |

Route 4 declines on `cs=1561` with `sets=0` ("flow graph covers none of the
defs") on **every pass in both arms** — so its separation is not route 4's
work at all. It is incidental: `defs` grinds 22 → 16 → … → 3 → 1 across 78
passes, and only completes because the analysis happens to keep running.
The backtrack reaches quiescence 42 passes earlier and truncates it.

**Proof, by raising the limit rather than by argument:**

```
bt=0                                       -> pass 80, pass_limit_hit=0, violations=0, rc=0
bt=1                                       -> pass 40, pass_limit_hit=1, violations=34, rc=1
bt=1 IFA_STALL_LIMIT=40 IFA_NONIMPROVE_LIMIT=40
                                           -> pass 80, pass_limit_hit=0, violations=0, rc=0
```

With headroom the flag arm reaches **zero violations** at the same pass
count as the baseline. Nothing is mis-split; the guard stops the analysis
while the repair is still progressing.

### The fix, and its precedent

A nominated split re-derives types from bottom, so the violation count
**rises before it falls** and the next pass looks non-improving. The code
already names this exact shape, for frontend-requested passes:

> *"a pass the FRONTEND asked for is expected to look worse — field
> promotion exposes fields, which exposes type flow, and the violation
> count rises before it falls (measured 44 → 325 → 52 on the plcfrs
> repro). Counting that as non-improvement stops the analysis while the
> repair is still progressing."* — `PYC_STALL_REANALYZE`'s comment

So a pass FOLLOWING a backtrack nomination gets the same treatment: it does
not advance `stall_passes` / `nonimprove_passes`. Two file-static counters
(`bt_noms_this_pass`, `bt_noms_last_pass`) rolled over once per pass at the
end of `extend_analysis()`, which is the one place that sees each pass
boundary. The excuse is **inert at the default** — no nominations means no
excuse — verified by `quameon` at `bt=0` being unchanged (pass 80,
violations 0) after the change.

After it: `quameon` `bt=1` → pass 82, `pass_limit_hit=0`, **violations=0**,
rc=0. `chull` still compiles with 0 errors and 0 warnings.

**A note for whoever reads `cs=1561` later.** That an arity-1 list literal's
correctness depends on the analysis running 78 passes, with route 4
declining on it every one of them, is its own latent fragility — the
[147](147-analysis-result-depends-on-the-binary-not-the-inputs.md) family.
This issue does not fix it; it stops tripping over it. Whatever is grinding
`cs=1561` down one def per pass is not identified.

## Corpus result — `-m check` A/B, one binary, env-toggled

| | flag off | flag on |
| --- | --- | --- |
| compile failures | 7 | **5** |
| **total warnings** | 1974 | **1636 (−338, −17%)** |
| programs whose stdout differs | 24 | 23 |
| run failures | 33 | 36 |
| container CS / shapes | 2138 / 629 = 3.40 | 2196 / 624 = 3.52 |
| `pratio` | 2.31 | 2.39 |

Per program, every row that moved:

| program | off | on | |
| --- | --- | --- | --- |
| `chull` | compile fail | compiles, `run 139` | **compile fixed**; the segfault is pre-existing (below) |
| `plcfrs` | compile fail, 379 warnings | compiles, **122** warnings, `run 134` | **compile fixed, −257 warnings** |
| `linalg` | 108 warnings | **33** warnings | **−75 warnings**, behaviour unchanged (`run 134` both) |
| `sudoku5` | compile fail, 216 warnings | compile fail, **210** warnings | −6 |
| `quameon` | runs, **wrong stdout** | `run 134` | **the one regression** — root-caused below |

`with_warnings` reads 33 → 34 only because `plcfrs` joins the compiled
population; no program gained warnings.

Cost side, stated plainly: **+58 container CreationSets (+2.7%)** and
`pratio` 2.31 → 2.39. The first A/B, before the stall fix, had 2130 — the
extra contours are the passes the stall guard used to cut off.

### The one regression: `quameon`, and it is a pre-existing representation gap

`quameon` compiles with **zero warnings** in both arms and then aborts:

```
quameon.py.c:29453: coulomb_pot::compute_en_value: Assertion
  `!"runtime error: matching function not found"' failed.
```

The abort follows `t49 = (_CG_any)((_CG_ps26965)t32)->e19; /* charges */` —
an index on an **untyped member slot**. `coulomb.py`:

```python
def __init__(self, npos=[], charges=None):
    if charges == None:
      self.charges = []                 # element-channel list
      for ...: self.charges.append(1.0)
    else:
      self.charges = charges            # quameon.py:61 -- [atom[1][0]], an ARITY-1 RECORD
```

One member holding an arity-1 record and an element-channel list is two
different `list` **representations**, so the slot has no type. Measured
identically in both arms:

```
bt=0   _CG_void e19; /* charges */     9 reads as (_CG_any)
bt=1   _CG_void e19; /* charges */     9 reads as (_CG_any)
CSVARS charges (both arms): one cs vars=1 arity=1 + two cs arity=0 elem=float64
```

So **the untyped slot is not caused by this change** — it is
[132](132-arity-is-representation-not-provenance.md)'s defect, latent and
pre-existing. pyc compiles it anyway and resolves each `charges[j]` from
the FA type rather than the C type; at `bt=0` all nine reads resolve, and
under the finer contours one of them has no single target, so codegen emits
the runtime abort. `quameon` was **already printing the wrong answer** at
`bt=0`, so the change trades a silent wrong answer for a loud abort.

Two separate follow-ups fall out, neither belonging to this issue:

- **[132](132-arity-is-representation-not-provenance.md)**: when an
  arity-recorded literal and an appended list meet at one member, the
  static arity must be dropped so the slot has a representation. `cs=2912`
  keeps `no_static_arity=0` through that confluence.
- **A reporting gap.** An emitted
  `assert(!"runtime error: matching function not found")` — 11 sites — with
  **zero warnings** is [149](149-the-largest-diagnostic-class-reports-nothing.md)'s
  family: codegen knows the dispatch is unresolved and nothing says so.

**This is why the flag stays default 0.** The mechanism is justified and
measured; `quameon` names a specific pre-existing defect that must land
first, and the flag is what keeps the two decisions separate.

## The next link: `chull` then segfaults, and it is not this change

With the union fixed `chull` compiles clean and **segfaults**:

```
#0  _CG_f_12108_84 (Vertex::Collinear) at chull.py.c:3087
      t87 = (_CG_float64)((_CG_ps16967)t94)->e18; /* z */
#1  _CG_f_12858_103 (Hull::DoubleTriangle)
```

All three `Collinear` arguments are genuine `Vertex` objects
(`__pyc_tag = _CG_type_Vertex`), contiguous in memory 64 bytes apart,
with **every data field zero** — `e19 /* v */ = 0x0`,
`e20 /* vnum */ = 0` — while the method slots (`__eq__`, `__str__`) are
filled. So they were allocated from a proper Vertex template and
`__init__`'s writes never landed.

**This is pre-existing and independent**, on three separate pieces of
evidence:

1. Same binary, env-toggled A/B (the only valid comparison — ifa/147):
   `PYC_CSBACKTRACK=0 PYC_NILSTORE=0` also gets `chull` past the compile
   check and produces the **identical** crash — same function, same call
   chain, same argument addresses.
2. `sweeps/check__PYC_NILSTORE_0__ae16c44e+0e9deefa.tsv` already records
   `chull compile_rc=0 warns=0 run_rc=139`.
3. [135](135-empty-sibling-contour-wins-the-clone-merge.md) independently
   notes it: "`chull` compiles there and then segfaults (`run 139`)".

### Where the trail stops, for whoever picks it up

`ReadVertices` lowers `self.vertices = [Vertex(vc,i) for i,vc in
enumerate(v)]` to

```c
t17 = (_CG_ps16967)((_CG_ps16950)t18)->e1;   /* x of enumerate's (i, x) */
/* vc 12592 */ g42 = t17;
t15 = _CG_f_16558_112(t16);                  /* -> __new__ -> __init__ */
```

`_CG_f_16558_112` → `__new__` → `Vertex::__init__`, which **does** emit
`((_CG_ps16953)t1)->e19 = (_CG_ps16967)t4;`. So `v` is written, from
`a2` — and `a2` must therefore have been NULL, i.e. `t17` was NULL. Since
`enumerate` is
[`__pyc__/05_builtins.py:219`](../../__pyc__/05_builtins.py) returning a
list of `(i, x)` tuples, `->e1` is the correct field, so the suspect is the
tuple contour: which `(i, x)` CreationSet the read is typed against versus
which one `r.append((i, x))` actually filled. `enumerate` is one function
shared by every caller in the program, and under the start-merged default
its tuple literal is one CreationSet unless something splits it.

Two things that look like bugs and are not, ruled out so they are not
re-chased:

- **`vnum` absent from `Vertex::__init__`'s signature**, with no
  `self.vnum = vnum` emitted though `e20 /* vnum */` exists in the struct.
  Legitimate: `vnum` is read only in `debug()` (chull.py:43), which is
  never called, so the store is dead and correctly elided.
- **`Vertex(vc, i)` lowered to a one-argument call.** Same cause — the
  dropped argument is the elided `vnum`.

Filed as the next link rather than folded in here: the union and the
uninitialized-`Vertex` bug are separate defects with separate causes, and
this issue's change is verified not to cause the second.

## Verification plan

- [x] probe proves the walk names `cs=1112` uniquely from all five demands
- [x] `chull` compiles clean, element channels separated
- [x] six CI gates green (flag off)
- [x] suite 316/0 both backends (flag ON)
- [x] corpus `-m check` A/B, first round: `chull` fixed, `quameon` broken
      by the stall guard (above), container CS 2138 → 2130 — **no contour
      growth**, so 146's non-monotone diagnostic is satisfied
- [x] corpus `-m check` A/B, second round after the stall fix — above
- [ ] **to default this on:** land
      [132](132-arity-is-representation-not-provenance.md)'s arity drop at a
      member confluence, then re-measure `quameon`
- [ ] the `Vertex` uninitialized-field crash (below), as its own issue

## What this unblocks

The `defs == 1` decline is the ladder's terminal state for any union
whose merge is upstream — which, on the evidence of `chull`, is the
common case rather than a corner. Nothing else in route 4 can reach it:
`esdefs1_enabled()` exists for exactly this shape but is gated on
`viol_named`, hence on `PYC_VIOLCS >= 3` (default **0**) and on
`fa->type_violations` — and `chull` raises **no FA type violation at
all**, because its failure surfaces in codegen's layout contract. So the
one existing backtrack is both off and keyed on a signal this class of
failure does not produce.

Direct dependents: `chull` (this issue), and the `defs=1 DECLINED` lines
that dominate route 4's late passes corpus-wide.
