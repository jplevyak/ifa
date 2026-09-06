# 133 — A merged container leaks elements between unrelated lists

**Status:** open, root-caused, blocked on a design choice (see the end).
Blocks [128](128-cs-identity-over-discriminates-vs-element-type.md)'s
start-merged posture (`PYC_CSDCPA1`), and is the largest single item in
[129](129-plan-demand-driven-creation-set-splitting.md)'s bill — 5 of the
16 suite failures under `PYC_CSDCPA1=2`.

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
| `a=[1,2]; del a[0]; b=[]; b.insert(0,"x")` | clean |
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

This explains the whole bisection, including the case that looks
contradictory: `del a[0]` is clean not because the merge does not happen
but because `del` discards the result, so nothing reads the union as a
basic value. `b = ["z"]` is clean because arity 1 is a different
CreationSet from arity 0.

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

## The design choice this is blocked on

Two options remain, and both are architectural:

1. **Record provenance on element writes** — attribute each write to the
   creation point whose container it passed through, so a merged container
   can be partitioned afterwards. Costs a per-write field and its
   maintenance across passes, and would make the original plan computable.
2. **Do not let library and user creation points share a contour.**
   Per-site identity knew `__delitem__`'s `[]` from the user's for free,
   which is exactly what `PYC_CSDCPA1` gives up. Any rule that restores it
   is provenance by another name, and a `__pyc__`-vs-user rule is the
   frontend-driven splitting
   [134](134-remove-the-frontend-forced-split-opt-in.md) exists to remove.

There is no narrow fix. **Do not work this issue further until that choice
is made** — the reproducer above makes it cheap to re-enter.

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
