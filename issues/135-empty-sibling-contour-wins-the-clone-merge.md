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

## Why the second CreationSet exists — the class PROTOTYPE

Chased 2026-09-06 with `IFA_DBG_CSROUTE=S1`, and the answer is upstream of
everything above.

```
DEFAULT                                   FLAG
p=0 es=1  -> cs=1013 via MINT             p=0 es=1  -> cs=1013 via MINT
p=0 es=63 -> cs=1098 via MINT             p=0 es=63 -> cs=1013 via dcpa1
                                          p=3 es=63 -> cs=1038 via cs_map
```

`es=1` is module scope; `es=63` is `S1.___init___`. **They are not two
allocation sites of one object — they are the class PROTOTYPE and the
INSTANCE.** `S1(11)` lowers to the `__new__`-synthesized
`clone(proto, t)`, whose source operand is `cls->self`, the class's own
prototype object (issue 078); `P_prim_clone` then calls
`creation_point(result, cs->sym)` for the instance.

**`PYC_CSDCPA1`'s route cannot tell them apart, and the code already says
why.** The route is `for (CreationSet *x : s->creators)`, keyed on the
class Sym — and `structural_assignment`'s own comment
(`fa.cc:2834-2836`) states the trap exactly:

> *checking `cs->sym` here instead would be wrong, since **`cs->sym` is
> the CLASS Sym, identical for the prototype and for every other instance
> of the same class***

So the flag hands instances the PROTOTYPE's CreationSet. The route already
excludes `s->abstract_type->v[0]` for a comparable reason; the prototype
is the same kind of case and is not excluded.

Then, at p=3, `split_css` separates them again on setter equivalence —
the prototype receives no field writes and the instance does:

```
[scss] cs 1013 (sym S1) starter_set=2 defs=2
SPLIT CS 1013 S1 -> 1038
```

leaving `cs=1013` as a LIVE prototype contour holding a def and a
promoted-but-unwritten `a`. **At the default that contour simply dies** —
`cs=1013` is minted at p=0 and is absent from `fa->css` at convergence,
which is why the default's `IFA_DBG_CSVARS` lists only the instance CS.

### The chain, end to end

1. `S1(11)` → `clone(cls->self, t)`; prototype and instance both need a
   CreationSet, and both carry the class Sym.
2. The dcpa1 route, keyed on that Sym, gives the instance the
   **prototype's** CreationSet.
3. `split_css` splits them back apart on setter equivalence.
4. The prototype contour survives with `a` promoted-but-unwritten, where
   at the default it would have died.
5. `determine_basic_clones` sees same sym, same `vars.n` (the empty var
   counts) → equivalent → merges → the merged clone takes ONE of two
   disagreeing member types.
6. S2 draws the empty one → `char e13[0]` → blind-cast width violation.

**The fix is at step 2, and it is not provenance.** A prototype and an
instance are different objects — the prototype is never reachable from
Python source, per the comment above — so refusing to share a contour
between them is a distinction about what the values ARE, not about where
they came from. Steps 3-5 are then unremarkable: they only bite because a
contour that should not exist does.

## Fixed 2026-09-06 — the prototype is excluded from the dcpa1 route

```c
const bool making_proto = v->var && s->self && v->var->sym == s->self;
for (CreationSet *x : s->creators) if (x && !(abstract)) {
    const bool x_is_proto = x->creation_var && s->self && x->creation_var->sym == s->self;
    if (x_is_proto != making_proto) continue;
```

Identified through `CreationSet::creation_var`, which already records the
Var that minted a CreationSet — no struct change, and **not by name**.

**Result.** Group B is fully fixed: both suite tests
(`sibling_subclass_field_layout`, `poly_dispatch_shared_method_extra_args`)
and all the corpus programs (`chull`, `kanoodle`, `path_tracing`,
`pygmy`). Two `splitter_*` goldens stopped failing as well.

| | before | after |
| --- | --- | --- |
| suite under `PYC_CSDCPA1=2 PYC_CSLADDER=3` | 9 | **7** |
| corpus container CS / shapes | 2910 / 599 = 4.86 | **2825 / 602 = 4.69** |
| corpus `pratio` | 3.17 | **3.03** |
| corpus `with_warnings` | 37 | **35** |
| corpus compile_fail | 12 | 12 — **3 fixed, 3 new** |

Default path untouched: 311/0 on both backends, all six CI gates green.

### The cost: ONE regression mechanism, five instances

`'X' has no type`, on `builtin_type_factory` and `empty_list_print` in the
suite and `sudoku2`, `sudoku3`, `sudoku5` on the corpus. All five carry
the same diagnostic family, so this is one defect rather than five.

Separating the prototype removes it from a value flow that something was
relying on — the prototype's own fields ARE seeded normally and are read
by `ClassName.attr` and by the inherited-field copy loop for subclasses
(`python_ifa_build_syms.cc:2755-2760`), so a contour it no longer shares
is a contour those reads no longer see. **Which class's prototype is not
yet identified.**

*Measured and rejected:* scoping the exclusion to classes with no element
channel, on the theory that a container's instances come from `make_kind`
rather than `clone(cls->self)` and its prototype is therefore not in this
relationship. **Inert** — identical 7 failures — so it was reverted rather
than kept as dead complexity. The cause is a record class's prototype, not
a container's.

## Superseded fix directions

1. **Exclude the class prototype from the dcpa1 route** — the root fix,
   at step 2. Needs a structural test for "this CreationSet is a
   prototype": the clone-source Sym is available at `P_prim_clone`
   (`p->rvals[o]->sym`, issue 078's `clone_source_sym`) and
   `clone_elides_fields` is documented as non-empty only for it, so the
   handle exists. **Not by name.**
2. ~~**Union the member types when merging equivalent CreationSets.**~~
   **Already done — the earlier description of this bug was wrong.**
   `compute_member_types` (`clone.cc`) already unions across an
   equivalence class: it walks every CS in `eqcss`, collects
   `av->out->type` for each, and calls `concrete_type_set_to_type`. The
   typed and empty CreationSets were never merged in the first place —
   `determine_basic_clones` compares
   `basic_type(fa, av1->out, (Sym *)-1) != basic_type(fa, av2->out, (Sym *)-2)`,
   and the two distinct sentinels make an EMPTY var compare unequal to
   anything, so it correctly separated them into two clones. The claim
   that "the merge picks one of two disagreeing member types" was wrong;
   what actually happens is that the prototype's own clone reaches the
   union at the read site.
3. ~~Make equivalence compare member TYPES, not just `vars.n`.~~ The
   retreat CLAUDE.md names: it makes the symptom go away by adding
   contours, and leaves a contour that should not exist.

## Probes added for this

| flag | answers |
| --- | --- |
| `IFA_DBG_FUNES=<fun name>` | how many contours a function has, and each contour's argument types |
| `IFA_DBG_CSVARS=<class name>` | every CreationSet of that class, its member AVars and their types, as FA leaves them |

Both default-off. `IFA_DBG_CSVARS` is the one that answers "did the
analysis or the cloning lose this field", which was the question two
earlier probes could not settle.
