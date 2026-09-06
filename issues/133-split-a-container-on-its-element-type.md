# 133 — A merged container leaks elements between unrelated lists

**Status:** open, root-caused, **unblocked 2026-09-06** — the design
choice is settled by the author's directive that *provenance is never the
answer*, which retires both of the options this was waiting on and leaves
wholesale-split-by-creation-point. One of the five failures is already
fixed. Blocks
[128](128-cs-identity-over-discriminates-vs-element-type.md)'s
start-merged posture (`PYC_CSDCPA1`), and is the largest single item in
[129](129-plan-demand-driven-creation-set-splitting.md)'s bill — now **4**
of the 16 suite failures under `PYC_CSDCPA1=2`, down from 5.

*Compacted 2026-09-05. This issue accumulated seven superseded diagnoses
before the reproducer was reduced; they are in the git history of this
file and are not repeated here. What follows is only what still holds.*

## Reproducer — four lines

```python
a = [1, 2]
a.pop()
b = []
b.insert(0, "x")
```

Under `PYC_CSDCPA1=2`: `error: 'x' has mixed basic types:( int64 str )`.
The bisection is sharp about what is required:

| case | result |
| --- | --- |
| `a=[1]; b=["x"]` | clean |
| `a=[1]; b=[]; b.insert(0,"x")` | clean |
| `a=[1,2]; a.pop(); b=["x"]` | clean |
| `a=[1,2]; a.pop(); b=[]; b.insert(0,"x")` | **reproduces** |
| `a=[1,2]; del a[0]; b=[]; b.insert(0,"x")` | **fails later — see below** |
| `a=[1,2]; a.pop(); b=[]; b.append("x")` | reproduces |
| `a=[1,2]; a.pop(); b=[]; b.insert(0,9)` | clean |

## Root cause

`__pyc__/04_sequence.py`:

```python
def __delitem__(self, key):
    return self.__pyc_setslice__(key, key + 1, 1, [])   # an empty list LITERAL
```

`__pyc_setslice__` begins with
`__pyc_primitive__(__pyc_symbol__("merge_in"), self, v)` — it merges the
source sequence into the receiver. Under one CreationSet per sym, that
internal `[]` **is the same CreationSet as every empty list the user
writes**.

The contours of the reproducer show it directly:

```
pop    es=44 args=[pop#580 list#981]                      ret=int64#6|str#939
insert es=45 args=[insert#828 list#983 int64#933 str#939] ret=None
```

`a` is `cs=981`, `b` is `cs=983` — already separate, so
[132](132-arity-is-representation-not-provenance.md)'s arity keying did
its job. `cs=983` is the shared empty-list contour, carrying **6 creation
points**: the user's `b = []` plus every `[]` inside `__pyc__`. Measured,
all writers into its element are `str`.

The leak:

1. `b.insert(0, "x")` writes `str` into `cs=983`'s element.
2. `a.pop()` → `__delitem__(a, …)` → `__pyc_setslice__(self=a, v=cs983)`.
3. `merge_in` merges `cs=983`'s element into `a`, so `cs=981` becomes
   `{int64, str}`.
4. `pop` returns an element of `a`; that return has no representation
   (issues/018 — boxing is a project decision against).

**Any element any user puts in an empty list leaks into every list that
has an element deleted.**

This explains the whole bisection. `b = ["z"]` is clean because arity 1 is
a different CreationSet from arity 0.

**Correction 2026-09-05: `del a[0]` is not clean.** It was recorded as
clean on the strength of the FA diagnostic alone. The reasoning given for
it is right as far as it goes — `del` discards the result, so nothing
reads the union *as a basic value*, and FA is silent — but the merge still
happens, `a`'s element is still `{int64, str}`, and it is still laid out
as `void*`. The failure just moves one stage later, into the C backend:

```
del.py.c:192:40: error: incompatible integer to pointer conversion
                        assigning to '_CG_void' (aka 'void *') from 'int'
  192 |   ((_CG_void*)(_CG_list_ptr(t3)))[0] = 1;
```

The `1` and `2` literals are stored raw into a pointer-typed backing
store (issues/018 — boxing is a project decision against). Worth keeping
in view generally: **absence of the `mixed basic types` diagnostic is not
evidence the merge did not happen**, only that nothing read it as a
scalar.

## Why the obvious fixes do not work

**Splitting `cs=983` by element contribution is not computable.** That was
this issue's original plan. The element channel records the union and
nothing else — which creation point contributed which type is exactly
what merging discards. Attribution was attempted two ways:

- `writer->container ∈ cs->defs` — wrong by construction. Creation points
  are allocation sites; a set operation *writes into* an object and its
  `container` is the CreationSet-typed receiver.
- back flow from `writer->container` to the creation points — the correct
  formulation. Reaches **0 of 6** creation points in 10-22 steps
  (`IFA_DBG_ATTRIB`). Whether that is a defect in the walk or a property
  of the graph is **not established**; the probe has not had the
  known-answer validation `IFA_DBG_FWDALL` received, and should get it
  before anything is built on it.

**Making `merge_in` notice the source is statically empty does not work
either.** The source's CreationSet *is* `cs=983`, whose `static_arity` is
0 **and** whose element is `{str}`, because the user's `b = []` shares it.
Per-call-site emptiness is not representable while the contour is shared,
so the over-approximation is not local to `merge_in`.

**The EntrySet splitter is not the answer and is not at fault.** The value
path is already split — `append` has six contours and the `str` one takes
a different receiver CreationSet from the five `int64` ones. Asking those
contours to split again reports `single_caller` or `no_groups` because
they are already monomorphic. A confluence *is* detected on the merged
element (`collect_type_confluences` covers `cs->vars` and the element
AVar) and is then discarded by `split_ess_for_type`, which accepts only a
formal or a return value as a target and has no `else` for a
CreationSet-contour AVar — but routing it to the writers' EntrySets finds
no legal target either, since those writers are non-formal temporaries.

**A container element cannot become a setter confluence on its own.**
`collect_cs_setter_confluences` collects it only when its setter classes
differ from a forward neighbour's, and an element that has never been
through `compute_setters` has none — `same_eq_classes(null, null)` is
true, so it is skipped and never acquires any. Seeding it from the
condition that does hold (backward sources disagree on type) breaks the
cycle, and it still gains no setters: for `AKIND_SETTER`,
`compute_setters` walks `av->forward`, not backward, and assigns to
`x->container`, never to `av->setters`.

## `__delitem__`'s `merge_in` was a FALSE CONSTRAINT — fixed 2026-09-05

Before the design choice: part of what this issue was measuring was not
contour sharing at all. It was a wrong constraint in the library, and it
is wrong on its own terms whatever CreationSet identity does.

`merge_in(thing1, thing2)` does `structural_assignment(cs, cs2, …)` for
every same-sym CreationSet pair (`fa.cc:3392`) — it asserts *the elements
of `thing2` flow into `thing1`*. `__pyc_setslice__` needs that, because a
slice assignment really does insert `v`'s elements into `self`. But
`__delitem__` passes `v = []` and **deletes**. Deletion cannot add an
element type to a list. The constraint was false regardless of which
contour the literal landed on; per-site CS identity was merely hiding it,
because the literal's own contour was empty and the false edge carried
nothing.

`append` already had the right form for a size-changing-but-not-inserting
operation — `merge_in(self, self)`, a self-merge. `__delitem__` now routes
through a `__pyc_delslice__` that uses it:

```python
  def __pyc_delslice__(self, i, j, s):
    return __pyc_c_call__(__pyc_primitive__(__pyc_symbol__("merge_in"), self, self),
                          "_CG_list_setslice",
                          list, self,
                          int, __pyc_primitive__(__pyc_symbol__("sizeof_element"), self),
                          int, i, int, j, list, [])
  def __delitem__(self, key):
    return self.__pyc_delslice__(key, key + 1, 1)
```

**This is not the retreat CLAUDE.md warns about.** It does not weaken a
rule to make a symptom go away — it deletes an assertion that was never
true. The empty literal is still passed to `_CG_list_setslice` as a
runtime value; only the false *type* edge is gone.

*Result.* The whole delete family clears under `PYC_CSDCPA1=2` — `pop()`,
`pop(i)`, `remove()`, `del a[i]` — and `tests/list_pop_insert.py` passes,
taking this issue's share of 129's bill from 5 to 4. Behaviour verified
byte-identical to CPython on `pop`/`pop(i)`/`remove`/`del`/slice-assign.
All six CI gates green, 311/0 on both backends; `tests/minmax_3arg.py.check`
re-blessed for the `called from __pyc__.py:1794` → `:1809` line shift (the
only golden in the suite that pins a `__pyc__` line number).

**It does not fix the class, and the boundary is exact.** A user writing
the slice assignment by hand still reproduces, because there the
`merge_in` is real:

```python
a = [1, 2]
a[0:1] = []      # genuine setslice -- merge_in(self, v) is correct here
b = []
b.append("x")    # still: 'x' has mixed basic types:( int64 str )
```

## The design choice — now with a third option

The remaining three failures (`builtins`,
`list_append_is_amortized`, `plcfrs_grammar_tables_nonconvergence`) are
the general case, and measuring one of them changes what the choice is
between. `list_append_is_amortized` under `IFA_DBG_MIXELEM`:

```
MIXELEM cs=995 sym=list defs=7  elem= int64#6 str#8
  def es=11 fun=___init___   def es=12 fun=___init___
  def es=41 fun=___init___   def es=2  fun=__main__    (+3 more)
```

**Seven creation points in one contour**, all the user's own — no library
literal involved. Compare the four-line reproducer, whose `cs=981` had
`defs=1` and was polluted from outside. These are different situations
and only the second is this issue's title.

1. ~~**Record provenance on element writes**~~ — attribute each write to
   the creation point whose container it passed through, so a merged
   container can be partitioned afterwards. **DEAD — the author's
   directive, 2026-09-06: *provenance is never the answer.*** See
   CLAUDE.md's "Provenance is never the answer". A per-write tag naming
   the container a value passed through is provenance in its purest form;
   the fact that it would make the original plan computable is not a
   defence, it is the temptation the rule exists to refuse.
2. ~~**Do not let library and user creation points share a contour.**~~
   Per-site identity knew `__delitem__`'s `[]` from the user's for free,
   which is exactly what `PYC_CSDCPA1` gives up. **Dead twice over:** it
   is provenance by another name (which module a creation point is in),
   *and* it no longer addresses the remaining failures at all — `cs=995`'s
   seven defs are all user code. A `__pyc__`-vs-user rule is also the
   frontend-driven splitting
   [134](134-remove-the-frontend-forced-split-opt-in.md) exists to remove.
3. **Split wholesale by creation point, and let the analysis re-derive** —
   shedskin's ladder route 4 (`infer.py:1576`: `len(paths) > 1 and 1 <
   len(csites) < 10` → give every site its own contour, then return).
   `defs=7` is inside that cap. **This is the answer**, by elimination and
   on its own merits.

**Option 3 needs no attribution, and that is why it was missed.** The
"not computable" finding above is correct about what it measured: which
def contributed which *type* is genuinely destroyed by the merge, so a
*minimal* two-way split cannot be computed. Wholesale does not ask that
question. It splits every def onto its own contour unconditionally and
lets the next pass re-derive each element from the writers that actually
reach it — which is monotone-safe, since every pass already re-derives
from bottom (`analyze_to_convergence` resets *before* each pass), and
bounded, since `cs->defs` is finite and capped at 10.

It is also demand-driven in the sense the goal statement means: the
trigger is an observed irrepresentable merge on a contour with more than
one creation point, not a structural property. And the primitive exists —
`split_css` already rewrites `v->cs_map->put(cs->sym, new_cs)` across a
group (`fa.cc:7775-7778`); what is missing is a caller that partitions by
`cs->defs` rather than by setter equivalence.

The known risk is precision given back: wholesale is coarser than
necessary, so `pshapes` is the meter, and route 4 is deliberately the LAST
rung of shedskin's ladder — try the finer routes first and fall through.

**There is no choice left: option 3 is the answer.** 1 and 2 are both
provenance, and provenance is never the answer (CLAUDE.md). That also
settles what to do if the precision cost turns out to be real — the
answer is a FINER demand test (shedskin's ladder tries no-confusion,
confluence partition and path partition before wholesale, and all three
key on deduced types), never a record of where a value came from.

**This issue is therefore unblocked.** The "do not work this further until
the choice is made" gate above is lifted: build the `cs->defs` partition
caller for `split_css`, triggered by an irrepresentable element union on a
contour with more than one creation point.

## Probes available

All default-off, all added while diagnosing this, all still in `fa.cc`:

| flag | answers |
| --- | --- |
| `IFA_DBG_MIXELEM` | which container's element has no representation, its creation point's EntrySet, and every writer |
| `IFA_DBG_CPATH` | the container path back from each writer, receiver spans, def reachability |
| `IFA_DBG_ATTRIB` | back flow from a writer's container to the creation points *(unvalidated)* |
| `IFA_DBG_FWDALL` | forward closure vs. type membership per container CS *(validated on a known-answer case)* |
| `IFA_DBG_TCDROP` | type confluences discarded because they sit on a CreationSet contour |
| `IFA_DBG_STAGE5` | whether the VIOLATION stage ran or was gated out that pass |
| `IFA_DBG_SESWHY` | why `split_entry_set` declined — `es_split`, `single_caller`, `no_groups` |
| `IFA_DBG_STARTERS` | whether `split_css` ran, with how many starters, and its result |

Two traps worth keeping in mind when using them:

- **Type membership is not evidence of flow.** `unreached > 0` from
  `IFA_DBG_FWDALL` occurs on the DEFAULT arm, which compiles and runs
  correctly (326, 314, 156 on `list_pop_insert`), so it is not a defect
  signal. Why it is large there is unexplained.
- **`qsort_by_id` on a set-`Vec` segfaults.** `set_add` builds the sparse
  representation, whose backing store holds nulls; call `set_to_vec()`
  first, as `split_css` does.
