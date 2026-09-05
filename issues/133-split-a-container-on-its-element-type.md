# 133 — Split a container CreationSet on its element type

**Status:** open, scoped. Blocks
[128](128-cs-identity-over-discriminates-vs-element-type.md)'s
start-merged posture (`PYC_CSDCPA1`), and is the last large item in
[129](129-plan-demand-driven-creation-set-splitting.md) step 4.

## Symptom

Under `PYC_CSDCPA1=2`, `tests/list_pop_insert.py` and
`tests/list_append_is_amortized.py`:

```
error: 'x' has mixed basic types:( int64 str )
```

The program builds three lists — `[1,2,3,4,5]`, `[10,20,30]`, and an
empty one later given `"x"` — mutates all three, and two of them end up
sharing a CreationSet whose element becomes `{int64, str}`. That union has
no representation (boxing is a project decision against, issues/018), so
compilation refuses.

`tests/splitter_cartesian_product.py` is the same cause in its soft form:
it compiles and runs correctly but cannot collapse `list.append`'s clones
on the shared contour, so three calls that were direct become dynamic
(`CALLS: 68/1 → 65/4`; see
[129](129-plan-demand-driven-creation-set-splitting.md)).

## Why it appears only now, and why nothing catches it

At the default this cannot happen: per-site CreationSet identity gives
every literal its own contour, so two lists never share one. The merge is
possible only under the start-merged posture — and specifically through
[132](132-arity-is-representation-not-provenance.md)'s rule that a
CreationSet which has lost its static arity **absorbs any arity**. That
clause is right (a dynamic list reads its length at runtime, so arity is
not its distinction) but it means the *element type* is then the only
thing left to distinguish two dynamic lists, and nothing is looking at it.

Measured, and the contour counts are otherwise exactly what the goal
statement wants — `list_pop_insert`, list CreationSets against distinct
element types:

| | list CS | element types | ratio |
| --- | --- | --- | --- |
| default | 10 | 4 | 2.5 |
| `PYC_CSDCPA1=2` | 3 | 3 | **1.0** |

So this is not over-merging in general. It is one specific pair that must
not have met.

**None of the existing routes catch it**, and three were measured:

- `PYC_CSELEM=1` (durable `elem_key`) and `PYC_CSELEM=3` (receiver shape)
  both still produce `mixed basic types` here. They key identity at
  **mint** time, when the element is unfilled by construction — the whole
  finding of [129](129-plan-demand-driven-creation-set-splitting.md)
  step 2c.
- `split_css` partitions a CreationSet by **setter equivalence**, which
  does not look at element types.
- `TYPE_CONFL` fires (it is in the `STAGES:` line) but acts on EntrySets
  and their AVars, not on a container's element channel.

## The work

**Trigger — and it is the strongest demand signal in the issue tracker.**
A container CreationSet whose element type is a union with *no
representation*: the mixed-basic-types condition. Codegen already detects
it, but at emission, far too late to act on. FA has the same information
at convergence in the element AVar's type. Nothing about this is a
heuristic — the distinction is not merely observable, it is one the
backend outright refuses to compile.

**Action.** Partition the CreationSet's creation points by the element
type each contributes and split. `split_css` is the primitive: it already
re-points `cs_map` across a partition and carries a ledger
(`cs_group_signature` → `ledger_find_cs`) so a re-derived split re-attaches
to the contour it first made instead of minting a fresh one each pass.
What is new is the partition function — by element contribution rather
than by setter equivalence.

### The blocker, found 2026-09-05 — and it is not the trigger

The obvious first move is ifa/109's pattern: raise the violation and let
the existing machinery act, exactly as its comment promises —

> *"Recording a violation here is what makes the EXISTING backward
> machinery do the work: `split_for_violations` (stage 5) splits the
> offending AVar... No annotation, no special case in codegen — FA simply
> has to know the constraint."*

There is even a real gap to close there. `collect_var_type_violations`
walks `cs->vars`, the **positional fields**, and checks each for
`mixed_basics` — but never the container's **generic element** AVar. So a
list whose element is `{int64, str}` raises nothing at that site.

**Adding it is inert, and it was measured: 0 / 16 / 19 failures at the
default, `PYC_CSDCPA1=2` and `=1`, identical with and without.** Reverted
rather than kept, but it is six lines and belongs in whatever lands here.

The reason it is inert is the actual work item.
`collect_violation_imprecisions` turns a violation into something
refinable by exactly two routes: `v->av->container` (null for a
CreationSet-contour AVar — nothing sets it for `cs->vars[i]` or the
element AVar), and `is_call_result(v->av)`, which wants a call PNode and
casts `v->av->contour` to an `EntrySet *`. **A violation whose AVar lives
on a CreationSet yields no imprecision at all, so stage 5 cannot see it.**

> **CORRECTED.** "No route into the splitter" is wrong, and the author
> named the reason: **that is what the setter flows do.** `av->setters` on
> a CS-contour AVar *are* the writers, and each is an AVar in an EntrySet,
> so the route exists and `collect_cs_setter_confluences` already collects
> the element AVar. What follows is what the route actually does when
> taken, measured with a new `IFA_DBG_SESWHY` probe.

### Taking the setter route, and what declines

`split_entry_set` returns 0 for two very different reasons — the ES was
already split this pass, or `decide_entry_set_split` found no partition —
and they had never been distinguished. `IFA_DBG_SESWHY=1` now reports
which, and `dec_why` names the decline path. Walking a CS-contour
confluence's setters and asking each to split, on `list_pop_insert` under
`PYC_CSDCPA1=2`:

| route | result |
| --- | --- |
| `SPLIT_SETTER` | **45 of 45 `single_caller`** |
| `SPLIT_TYPE` | 40 `no_groups`, 5 `single_caller` |

`single_caller` is the short-circuit `fsetters && non_rec_edges == 1`,
whose own comment says it "only applies to the setter path" — so those
EntrySets have exactly ONE non-recursive caller and a setter split has
nothing to partition. Switching to the type path clears that for 40 of
them and they then decline with `no_groups`: **the callers do not differ
in type at the confluence position either.**

**So the setters' EntrySets are not where the distinction lives.** The
writer contours are not shared — the CONTAINER is. Splitting a writer's
ES cannot separate two containers that both flow into it, however the
split is keyed.

That is the argument for the original Action above rather than a way
around it: the thing that must come apart is the CreationSet, by
partitioning `cs->defs`, and `split_css` is the primitive for exactly
that. The ES route was worth taking to the bottom because it is the one
the existing machinery offers, and now it is measured rather than
assumed.

*Kept from the experiment:* `IFA_DBG_SESWHY` and `dec_why`, which answer
"why did the splitter decline" in one line. This session needed that
question answered three times and had to re-derive it each time. The
`PYC_ESFORCS` route itself was inert both ways and is not kept.

### Why the setter split declines: it already succeeded

The author's question — *the setters' EntrySets get split by the value
being set, why isn't that happening?* — has a measurable answer: **it is
happening, and correctly.** `PYC_CSDCPA1=2` on `list_pop_insert`:

```
append es=109..112,114   args=[... list#1003  int64…]      ret=list#1003
append es=113            args=[... list#1016  str#953]     ret=list#1016
insert es=86             args=[... list#1016  … str#953]
```

`append` has six contours, split per value type, and the `str` one takes a
**different receiver CreationSet** from the five `int64` ones. The value
split is doing exactly its job. That is also why asking those EntrySets to
split again reports `single_caller` and `no_groups` — they are already
monomorphic; there is nothing left to partition.

**The merge is upstream of every write.** The CreationSets and their
creation points:

| | list CreationSets | defs |
| --- | --- | --- |
| default | 10 | one creation point each |
| `PYC_CSDCPA1=2` | 3 live | `cs=995` 1, `cs=1003` 1, **`cs=1016` 8** |

Eight creation points share `cs=1016`. They arrive there because, once a
list is mutated it loses its static arity, and
[132](132-arity-is-representation-not-provenance.md)'s rule — correctly —
lets a CreationSet with no static arity absorb any arity. So the
container is merged *before* any write happens; each write is then routed
through a correctly-split writer contour into the *same* container, and
the element unions.

**No amount of EntrySet splitting can fix that**, because the writer
contours are not what is shared. The eight defs of `cs=1016` are, and
partitioning them is the action this issue opened with.

### And `split_css` already runs — it partitions by the wrong setters

The author's follow-up — *the setters are independent, so they can be
used to split the CreationSet* — is right, and the machinery is already
there and already running. Measured with a new `IFA_DBG_STARTERS` probe on
`list_pop_insert` under `PYC_CSDCPA1=2`:

```
[sfs] split_css REACHED starters=25 -> 1
[sfs] split_css REACHED starters=24 -> 0
[sfs] split_css REACHED starters=5  -> 0   (x2)
[sfs] split_css REACHED starters=4  -> 0
```

So `split_css` is not starved by the two EntrySet stages ahead of it in
`split_for_setters`; it runs every pass with a live starter set and even
splits once. It simply does not separate `cs=1016`.

**Because of which setters it reads.** `split_css` builds its starter set
from AVars whose `cs_map` names the CreationSet, and partitions with
`same_eq_classes(v->setters, av->setters)` — the setters of the **def
AVars**, i.e. of whoever assigned the list *variable*. Those do not
discriminate: all eight creation points were assigned by their own
literal and agree.

The discriminator is one level in: the setters of the CreationSet's
**element** — who wrote *into* the container. Those are exactly the
writers the value split has already separated (`append es=113` carries
`str`, `es=109-112,114` carry `int64`), so the partition is sitting there
fully computed and is being read from the wrong AVar.

**That makes the action concrete and small.** Partition `cs->defs` by
element-setter class rather than by def-setter class, and hand the
partition to `split_css`'s existing machinery, which already re-points
`cs_map` across a group and ledgers the result for cross-pass identity.
No new splitting primitive, no new demand signal — the signal is the one
the setter flows already produce.

### It was built, and it cannot fire — the merged CS is not the broken one

Implemented as `split_css_by_element`, triggered on `mixed_basics` of the
element and partitioning `cs->defs` by what each def's own writers
contribute. **Inert: 16 failures under `PYC_CSDCPA1=2` with and without.**
Reverted. Three things it established, each correcting a step above.

1. **The element AVar of the merged container has no setters at all.**
   `cs=1016 defs=8 elemvar=1` → `no_elem_setters`. The setter machinery
   computes setters only for AVars inside a confluence closure, and a
   merged container's element is not in one. So "partition by
   element-setter class" has no data to read; `elem->backward` carries the
   same flows without that dependency and is what any implementation
   should use.

2. **The merged CreationSet's element is not the mixed one.**
   `cs=1016`, the one with eight creation points, reports
   `elem_not_mixed`. Its element is representable.

3. **The CreationSet that holds `{int64, str}` has ONE creation point.**
   `cs=995 defs=1`. So there is no `cs->defs` partition to make — the
   action this issue opened with cannot fire, because nothing is merged
   at the container that is actually broken.

**Where that leaves it.** A single creation point whose element holds both
types means the *contour that creates it* is shared: the creation site
sits inside a `__pyc__` helper, one CreationSet per sym gives it one CS,
and every caller's container flows through it. Separating it requires
splitting **that EntrySet** per caller, so the creation point becomes two
AVars and two CreationSets follow.

That is the goal statement's third clause — *"ESs are split as necessary
to separate the creation points so the CS can split"*.

### Located: `__pyc_setslice__`, and TWO defects on the path

`IFA_DBG_MIXELEM` names the container, its creation point's EntrySet, and
every writer of its element. On `list_pop_insert` under `PYC_CSDCPA1=2`:

```
MIXELEM cs=995 sym=list defs=1 elem= int64#6 str#8
  def    es=2   fun=__main__
  writer es=60  fun=__pyc_setslice__  type= str
  writer es=60  fun=__pyc_setslice__  type= int64   (x5)
  writer es=138 fun=__pyc_setslice__  type= str
  writer es=138 fun=__pyc_setslice__  type= int64   (x5)
```

The container is created in `__main__` and its element is written by
`__pyc_setslice__`, which has two contours — and **each carries both
`str` and `int64` at the writing position.** Unlike `append` and `insert`,
which the value split separates cleanly, `setslice` is not split at all.
So the shared helper is where the two element types meet, and it is the
EntrySet that has to come apart.

**Defect A — stage 5 is delayed, not starved, and only when FA
converges.** Stage 5 is gated on `!analyze_again`: it runs only in a pass
where no earlier stage progressed. The obvious reading is that it never
runs — but FA does not progress forever. `IFA_DBG_STAGE5` on
`list_pop_insert` under `PYC_CSDCPA1=2`:

```
p=0..4   analyze_again=1  -> starved   (violations 26,47,63,25,27)
p=5      analyze_again=0  -> RUNS      (violations=27)
p=6      analyze_again=0  -> RUNS      (violations=27)
```

FA converges at pass 7 (`pass_limit_hit=0`), so the quiescent pass
arrives and **stage 5 gets two turns** — and makes no progress in either,
which is why `VIOLATION` is absent from the `STAGES:` line. The gate is a
delay, not a permanent block.

It is a *real* delay: `PYC_SIZEOF_VIOL=2` lifts it, stage 5 then runs
from pass 0 and **does** progress (`STAGES:` gains `VIOLATION`). So the
gate genuinely suppresses splits that would otherwise happen — they just
are not the splits this case needs, and the `mixed basic types` error
survives either way.

The permanent starvation the gate's own comment describes (issue 109,
measured on sunfish) is the **non-convergent** case: if the pass limit is
hit, quiescence never arrives and stage 5 never runs at all. That is a
different program shape from this one, and worth keeping separate.

**Defect B — the violation produces no imprecision, and this is the
terminal blocker.** With the gate lifted, stage 5 runs and
`split_entry_set` is called seven times — **none of them on `es=60` or
`es=138`.** `collect_violation_imprecisions` turns a violation into
refinable AVars by exactly two routes, `v->av->container` (needing
`->out->n > 1`) and `is_call_result`, and a mixed-basic violation on a
writer inside a shared helper satisfies neither. The violation is
recorded and correct; nothing maps it onto the contour that must split.

So the demand signal exists, is correct, is recorded, and reaches a stage
that cannot address it.

### B cannot be routed: there is nothing imprecise left to split

Building the route establishes that it does not exist to be built. Two
attempts, both measured and reverted.

*Offer the violating AVar's own EntrySet formals.* Never fires. The
BOXING violations land on locals in `pop`, `__getitem__`, `__main__` and
`__str__` — never in `__pyc_setslice__` — and those contours' formals are
already monomorphic. There is nothing there to separate.

*Offer the formals of the EntrySets that WRITE the mixed element.* This
is the chain `IFA_DBG_MIXELEM` traced by hand, and stage 5 does see the
container:

```
[violform-cs] p=5 cs=995 back=31 on_es=12 formals=72 imprecise=0
[violform-cs] p=6 cs=995 back=31 on_es=12 formals=72 imprecise=0
```

31 backward writers, 12 of them on EntrySet contours, **72 formals
between them, and not one is imprecise** — every writer contour has
exactly one CreationSet at every formal.

**So every contour on the path is already monomorphic, and the container
has a single creation point.** `split_ess_for_type` has no imprecise
formal to accept; `split_css` has no second def to partition. The union
is formed by many individually-precise writers all flowing into one
CreationSet's element.

**That reframes this issue.** It is not a splitting problem — there is no
contour left to split. It is a FLOW problem: element writes from distinct,
monomorphic contours are reaching a container they should not reach. The
question to answer next is which flow edge carries `str` into `cs=995`'s
element when every writer contour that could do so is monomorphic in a
different receiver — most likely an element-channel trampoline that
unions across CreationSets rather than per-CS
(see [ifa-fa-snapshot-vs-durable-edge](111-FA-selective-invalidation-per-pass.md)'s
`vector_elems` discussion). Splitting cannot fix an over-wide edge.

A is a smaller, separate question — whether a stage that only runs once
everything else is quiet is the right design, given that lifting the gate
demonstrably produces splits it otherwise suppresses. But A is measured
insufficient for this case, and on a converging program it is a delay
rather than a block.

**Bounding, and the stop condition.** If the program genuinely builds a
heterogeneous list, no split helps: the union is real and today's refusal
is the right answer. So the split is *attempted*, and on failure control
falls through to the existing diagnostic. It must not loop, and the fan-out
must be capped. **If capping is what makes the numbers work, the trigger
is wrong** — the trigger is supposed to fire only on unions that have no
representation, which is a small and well-defined set.

**What it is NOT.** Not element-keyed identity at mint time — measured
above, twice, and it cannot work because the element is unfilled when the
decision is taken. This is a split *after* evidence arrives, which is what
the whole of [129](129-plan-demand-driven-creation-set-splitting.md)
step 4 established the analysis can do: every pass re-derives from bottom,
and `cs_map` is the only thing pinned.

*Verify:* `list_pop_insert` and `list_append_is_amortized` compile and run
under `PYC_CSDCPA1=1` and `=2`; `splitter_cartesian_product`'s `CALLS:`
returns to `68/1`; the default is bit-identical (this can only fire where
a CreationSet has several creation points, which `multidef=0` says never
happens at the default); corpus `check` under the flag improves on
`check__PYC_CSDCPA1_2__5e012d78+8450a439`'s 19 compile failures; and a
genuinely heterogeneous list still gets ONE contour and the existing
refusal, asserted by a new test.

## Pattern worth naming

This is the third mechanism in this area that exists but is gated on a
**static, frontend-supplied opt-in** where a demand signal is what is
needed:

| mechanism | opt-in today | wants |
| --- | --- | --- |
| per-constant contours | `__pyc_clone_constants__` → `clone_for_constants` | [131](131-demand-driven-constant-splitting.md) |
| method contours per receiver CS | `clone_methods_per_cs` (issue 045) | a demand signal |
| element-type separation | per-site CS identity, incidentally | this issue |

Each was invisible while per-site CreationSet identity was separating
things structurally. Removing that separation is what makes them ask to be
demand-driven — which is
[128](128-cs-identity-over-discriminates-vs-element-type.md)'s point. The
gating itself is tracked as
[134](134-remove-the-frontend-forced-split-opt-in.md).
