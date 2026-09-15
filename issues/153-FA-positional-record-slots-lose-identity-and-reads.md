# ifa/153 — a positional record's slots lose their identity, their type, and their reads

**Status:** fixed 2026-09-15. Three defects, one chain, all on the DEFAULT
path. Found by asking why `chull` segfaults after
[152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md) let it
compile.

## The symptom

`chull` compiled with zero warnings and segfaulted in `Vertex::Collinear`
dereferencing `a3->v`. Every `Vertex` in `Hull.vertices` had **every data
field zero** — `e19 /* v */ = 0x0`, `e20 /* vnum */ = 0` — while its method
slots were filled, and the three `Collinear` arguments sat 64 bytes apart in
allocation order. So they were real `Vertex` objects, correctly allocated,
whose `__init__` writes never landed.

`ReadVertices` lowers

```python
self.vertices = [ Vertex(vc,i) for i,vc in enumerate(v) ]
```

and `enumerate` (`__pyc__/05_builtins.py:219`) is a plain Python loop
appending `(i, x)`. The emitted tuple construction was:

```c
t19 = __next__(t6);
t17 = _CG_prim_tuple_list(_CG_ps16950, 2);
t17->e0 = t9;          /* i */
/* NO STORE FOR THE ELEMENT */
```

`x` was never stored, so every `Vertex` was built from a null `v`.

## Defect 1 — two contours with different member types share one layout

`determine_basic_clones` (`clone.cc`) compares two CreationSets' members
with `basic_type`, which **maps every non-basic type to `nullptr`**. That is
already recorded in the file: *"`Edge` vs `Site`, `Halfedge` vs `tuple`,
`SiteList` vs `list`, and record-vs-None all compare EQUAL here."*
ifa/126 fixed it for ENTRYSETS (`PYC_CLASSEQ`, class-aware,
default on). The CreationSet half was never done.

So `enumerate`'s two contours —

```
cs=1177 vars[0]=i:int64 vars[1]=x:Vector#1805   (enumerate es=292)
cs=1887 vars[0]=i:int64 vars[1]=x:Vertex#1191   (enumerate es=86)
```

— merged into one layout class. `compute_member_types` then has to give
slot 1 ONE type and cannot: `concrete_type_set_to_type({Vector, Vertex})`
builds a nameless `Type_SUM` (`make_LUB_type` is a default no-op that pyc
does not override), codegen renders it `_CG_void`, and the store is dropped.

**FA had it right throughout.** This is entirely a clone-side merge.

`PYC_CSCLASSEQ=1` (default): split when the two members' concrete class sets
differ and their union holds more than one class — i.e. **a merge must not
ENLARGE any member's concrete type set.** A member that was already a union
is not made worse by the merge and is not this rule's business.

Two synthetic fixtures moved, and both are the rule working:
`iterator_missing_field` is *"V holds records of types A or B with DISJOINT
fields"* and `stored_fn_dispatch_2` holds two different stored functions.
Each went `class[V] members=2` → two `members=1` with `fa_rc`, `css`, `ess`,
`es-equiv` and the call graph all unchanged.

## Defect 2 — a positional slot inherits a foreign field's Var

With defect 1 fixed the member TYPES were right (`_CG_ps16987 e1`,
`_CG_ps16960 e1`) and the store was **still dropped**.

`compute_member_types` builds `has` by cloning the existing `sym->has[i]`.
For a CLASS that is correct — measured on `chull`, `Vertex`'s `has[k]->var`
is the same `Var` as `cs->vars[k]->var` at all 24 slots. For a POSITIONAL
record it is not, because `sym_tuple->has` carries whatever cross-class field
promotion ([issues/128](../../issues/128-cross-class-field-promotion.md)) put
on `tuple`:

```
tuple#16957   has = [mark, delete, duplicate, newface, onhull, visible]   (promoted)
              vars = [i, x, mark, delete, duplicate, newface, onhull, visible]
  has[0] name=mark   hasvar=0x…c2d0  csvar=0x…82d0  csname=i
  has[1] name=delete hasvar=0x…0690  csvar=0x…8000  csname=x
```

Misaligned by two, so slot 1 took the name **and the Var** of `Edge.delete`.
That Var is dead, `cg_field_live` reported the slot dead, and cg.cc's
record-construction loop skipped the store — **for every 2-tuple in the
program, silently.**

Fixed by a structural test on Var pointer identity (never names): when the
inherited member Sym does not describe this slot, mint a fresh one and give
it the slot's own Var and name. A no-op for classes, where they already
agree.

## Defect 3 — a record slot read by INDEX is never counted as read

With 1 and 2 fixed, `chull` ran and raised its own
`DoubleTriangle: All points are coplanar!`. The main loop is

```python
x,y,z = 2*random()-1, 2*random()-1, 2*random()-1
if x*x+y*y+z*z < 1.0: sphere.append(Vector(x,y,z))
```

and the **third slot of that 3-float tuple was being elided**, so every
sphere point had `z = 0` and every point really was coplanar.

`cg_compute_slot_reads` claims in its own comment that *"both sides are
computed structurally above"*. Only NAMED access was: the getter's
`resolve_union_receiver` needs a symbol, and a tuple has none. `x,y,z = …`
lowers to a `P_prim_index_object` per element with a CONSTANT index, and none
was ever counted as a read.

That is not a missed optimisation, it is a wrong answer, because elision
groups on `cs->sym` — **`sym_tuple` for every tuple in the program**. One
verdict per slot index therefore covers all of them: a 2-tuple's leftover
`has[2]` is bottom, nothing recorded a read, and slot 2 was elided on every
tuple including the 3-float one.

Fixed by recording the read. A non-constant index can reach any slot, so it
marks them all.

## Defect 4 — a record's elements are indexed THROUGH its classtag

With 1-3 fixed, `chull` ran much further and still crashed, now on
`e.adjface[1]` being null in `AddOne`. Traced to `MakeConeFace`:

```python
for i in (0,1):
    for j in (0,1):
        if new_edge[i].adjface[j] is None:
            new_edge[i].adjface[j] = new_face
            break
```

gdb showed **`i=0 j=0` on every iteration**, so only
`new_edge[0].adjface[0]` was ever written and every cone edge kept one
adjacent face instead of two. `__tuple_iter__::__next__` was emitted as

```c
t1 = ((_CG_int64*)(t2))[_CG_norm_idx(t3,(int32)6)-0];
```

— indexing the record pointer DIRECTLY. But that record is

```c
struct _CG_s16977 { _CG_TypeObject *__pyc_tag; _CG_int64 e0; _CG_int64 e1; ... };
```

so element 0 is the **classtag pointer** and every element is shifted by
one: `(0,1)` iterated as `(<tag>, 0)`.

`cg.cc` has this base expression at two sites, the dynamic-index READ and
the dynamic-index WRITE, both `if (t->type_kind == Type_RECORD)
fprintf(fp, ")(%s))", rec)`. Both now go through
`cg_emit_record_index_base`, which adds `sizeof(void *)` when
`cg_has_classtag(t)`. A record with no tag is unchanged, which is why this
survived: the same expression is correct for every untagged tuple, and a
CONSTANT index takes a different path (`->eN`) that was always right.

## Result

**`chull`'s reported segfault is fixed, and the compiled code is verified
correct against CPython.** On a deterministic point set (`cube_internal`
instead of the random `sphere`) pyc now matches CPython **byte for byte** —
720 of 721 lines, the only difference being the wall-clock `TIME` line.
Before these fixes every `Vertex` in `Hull.vertices` was built from a null
`v` and it died immediately in `Collinear`.

### What still fails, and it is a different bug

With the corpus program's own random input `chull` still segfaults. That is
**not** the RNG: pyc's `random()` does not match CPython's Mersenne Twister
(measured: entirely different sequences from the same seed), but feeding
BOTH runtimes an identical LCG-generated 2000-point set still diverges.

Instrumenting both at each `AddOne` — printing `len(self.edges)`,
`len(self.faces)`, and the number of edges with a null `adjface` — the two
agree for **126 consecutive calls** and then split:

```
       pyc                 CPython
  ADDONE 120 80        ADDONE 120 80     <- last agreeing
  ADDONE 118 80        ADDONE 120 80     <- pyc lost 2 edges
```

`CleanEdges` drops edges marked `delete`, which `AddOne` sets when BOTH
adjacent faces are visible — so pyc marked two extra edges interior, i.e.
its `visible` set differs, i.e. `VolumeSign` disagreed for some face. From
there the cone fan no longer closes and the last appended edge keeps a null
`adjface[1]`; CPython reports **zero** such edges over the whole run and pyc
reports one on every subsequent `AddOne`.

Ruled out by direct test, each matching CPython exactly: nested `break`,
`break` under an `if` guard writing through two index levels, the
`(f.edge[1], f.edge[2]) = (f.edge[2], f.edge[1])` tuple swap, and object
identity comparison across two lists (`MakeCcw`'s orientation loop). The
remaining suspect is the floating-point path in `VolumeSign` or the vertex
ordering `MakeCcw` produces. **That is the next link and is not this
issue.**

`chull` no longer segfaults in `Collinear`, and its `z` values are real. It
still fails later, for a DIFFERENT and unrelated reason: `AddOne` reads
`e.adjface[1].visible` and `adjface[1]` is null, i.e. the edge/face state has
diverged by then. That is the next link and is not this issue.

All six gates green; suite 316/0 on both backends.

## Verification plan

- [x] six CI gates, including the two clone goldens re-blessed and their
      `dce` cascade (dead-type count only, `live=0` unchanged)
- [x] suite 316/0 both backends
- [x] `chull` byte-identical to CPython on a deterministic point set
- [ ] corpus `-m check` A/B on one binary — these are DEFAULT-path changes,
      so `check` is the mode that matters
- [ ] the remaining `chull` divergence at the 127th `AddOne` (a `visible`
      disagreement, so `VolumeSign` or `MakeCcw`'s vertex ordering)

## What this unblocks

Defect 3 is the one to reach for first when a record's data is silently zero:
every positional record slot in the program shared one elision verdict per
index, so any program mixing tuple arities could lose a field with no
diagnostic at all. Defect 1 removes a whole class of `_CG_void` members —
[132](132-arity-is-representation-not-provenance.md)'s census counts 976
untyped-slot conflicts in 13 corpus programs, and a merge that manufactures
a union is one of the ways they arise.
