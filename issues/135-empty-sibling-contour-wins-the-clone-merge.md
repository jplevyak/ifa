# 135 — an empty sibling CreationSet wins the clone merge and blanks a field

**Status:** open, root-caused 2026-09-06. Group B of
[129](129-plan-demand-driven-creation-set-splitting.md)'s bill — **6 cases
from one mechanism**, 4 corpus (`chull`, `kanoodle`, `path_tracing`,
`pygmy`) and 2 suite (`poly_dispatch_shared_method_extra_args`,
`sibling_subclass_field_layout`) under `PYC_CSDCPA1=2`.

This is the question CLAUDE.md's "be aggressive" section says was never
asked: *why do two clones of one class have divergent member types, and
is the blind cast between them legitimate at all?* Here is the answer for
this family.

## Reproducer — 9 lines, no constants, no subclass methods

```python
class Base:
    def __init__(self, a):
        self.a = a
class S1(Base):
    pass
class S2(Base):
    pass
xs = [S1(11), S2(21)]
for x in xs:
    print(x.a)
```

`PYC_CSDCPA1=2`: `error: object layout: 'S2' is blind-cast to 'S1' and
read at e13, but member width differs at e13 (_CG_int64 vs
<placeholder>)`. Clean at the default. Reproduces with runtime arguments
too, so constant cloning is not involved. The union receiver IS required —
reading `.a` through `xs` is what makes the blind cast happen. Calling a
subclass method instead of reading the field is clean.

## What is NOT the cause

- **Not slot ordering** ([121](../../issues/121)'s bug). `IFA_DBG_LAYOUT`
  gives `PROMOTE S1.a -> 13` and `PROMOTE S2.a -> 13` — identical.
- **Not flow analysis.** `IFA_DBG_FUNES=__init__` shows `Base.__init__`
  with **2 contours in BOTH arms**, receivers `S1#1038` and `S2#1039`
  under the flag. FA splits correctly and writes each receiver's field.
- **Not clone equivalence across classes.** `equivalent()` requires
  `a->sym == b->sym`, and S1 and S2 are different syms.
- **Not the ifa/133 ladder or CS_DEF_PARTITION.** Reproduces with
  `PYC_CSLADDER=0 PYC_CSDEFSPLIT=0`.

## Root cause

`IFA_DBG_CSVARS` on each class, under the flag:

```
cs=1013 sym=S1 vars=14 defs=1 || var=a type=            <- promoted, never written
cs=1038 sym=S1 vars=14 defs=1 || var=a type= int64#6
cs=1014 sym=S2 vars=14 defs=1 || var=a type=            <- promoted, never written
cs=1039 sym=S2 vars=14 defs=1 || var=a type= int64#6
```

and at the default:

```
cs=1098 sym=S1 vars=14 defs=1 || var=a type= int64#6    <- ONE CS per class
cs=1103 sym=S2 vars=14 defs=1 || var=a type= int64#6
```

**Under the flag each class has TWO CreationSets, one of which has `a`
promoted but never written.** `promote_field` adds the var on the evidence
that a write landed in `cs->unknown_vars`; the var is created and counted
whether or not a value ever flows into it.

`determine_basic_clones` then marks two CSs of one sym not-equiv when
their `vars.n` differ — and `vars.n` is **14 for both**, because the
promoted-but-empty var counts. So the typed CS and the empty one are
EQUIVALENT, merge into one clone, and the merged clone takes ONE of the
two member types. S1 happened to get the typed one; S2 got the empty one,
which emits `char e13[0]` and trips the blind-cast width check.

Confirmed downstream: the emitted C has **1 field store under the flag and
2 at the default** (`->e13 = ...`), casting to one struct type instead of
two.

**And the irony is the finding.** `PYC_CSDCPA1` exists to give ONE
CreationSet per sym. On this program it produces **two per class where the
default produces one**, and the extra one is empty. Why a second CS is
minted at all — `creation_point`'s dcpa1 route should return
`s->creators`' first non-abstract entry — is **not yet established** and is
the first thing to chase. `IFA_DBG_CSROUTE=S1` shows both reached "via
cs_map", so the decision was taken earlier than the route.

## Fix directions, none yet attempted

1. **Make equivalence compare member TYPES, not just `vars.n`.** Narrow
   and local, but it splits clones rather than fixing the empty CS, so it
   trades a wrong layout for more contours.
2. **Do not let a promoted-but-unwritten var count toward `vars.n`**, or
   union the member types when merging equivalent CSs. A merge that picks
   one of two disagreeing types is wrong however the CSs arose.
3. **Stop minting the second CreationSet** — the upstream fix, and the one
   the flag's own premise asks for. Needs the "why two" question answered
   first.

3 is the real one; 1 is the retreat CLAUDE.md names, since it makes the
symptom go away by adding contours.

## Probes added for this

| flag | answers |
| --- | --- |
| `IFA_DBG_FUNES=<fun name>` | how many contours a function has, and each contour's argument types |
| `IFA_DBG_CSVARS=<class name>` | every CreationSet of that class, its member AVars and their types, as FA leaves them |

Both default-off. `IFA_DBG_CSVARS` is the one that answers "did the
analysis or the cloning lose this field", which was the question two
earlier probes could not settle.
