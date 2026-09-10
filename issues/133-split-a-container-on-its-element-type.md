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

## Reproducer — five lines

```python
a = []
a.append(1)
s = []
s.append("x")
print(a[0], s[0])
```

Under `PYC_CSDCPA1=2`: `error: expression has mixed basic types:( int64 str )`,
with `STAGES: TYPE_CONFL` — the splitter notices and gives up. Clean at the
default. **No `__pyc__` internals are involved**, which is what makes this
the right reproducer: two user creation points, both arity 0, sharing one
CreationSet, with elements that cannot both be represented.

`IFA_DBG_MIXELEM` gives the whole picture in four lines:

```
MIXELEM cs=983 sym=list defs=6  elem= int64#6 str#8
  writer es=47 fun=__setitem__ type= int64
  writer es=51 fun=__setitem__ type= str
```

**The value path is already fully split** — two `__setitem__` contours,
one per type — and they both write into one element because the
*receiver* CreationSet is one. Nothing remains to split on the EntrySet
side. The CreationSet must partition its own `defs`.

*Superseded reproducer, kept because it is now a passing regression test.*
This issue was originally reduced to `a=[1,2]; a.pop(); b=[]; b.insert(0,"x")`,
which is FIXED (see "`__delitem__`'s `merge_in` was a false constraint"
below) and lives on as `tests/list_pop_insert.py`. It was never the pure
form: it needed a `__pyc__` internal `[]` to carry the leak, and the
mechanism turned out to be a wrong constraint rather than contour sharing.

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

## Debugged 2026-09-06 — the confluence is found, then dropped on the floor

Traced end to end on the five-line reproducer under `PYC_CSDCPA1=2`. The
demand signal exists and carries everything needed; nothing consumes it.

**1. The confluence IS collected.** `IFA_DBG_TCDROP`:

```
[tcdrop] p=2 confluence on CS cs=983 sym=list defs=6 type= int64 str
[tcdrop] p=3 confluence on CS cs=983 sym=list defs=6 type= int64 str
```

`collect_type_confluences` covers the element AVar and finds it, with the
CreationSet, its six defs, and the offending union all in hand.

**2. `split_ess_for_type` drops it.** The AVar's contour is a
CreationSet, so it falls to the `else` at `fa.cc:8204-8207`:

```c
} else {
  ++tc_skip_cs;
  log(LOG_SPLITTING, "[stage1] av %d CS-contour skipped (passes to stage2)\n", av->id);
}
```

It is counted and logged and **not added to anything**. `confluences` is a
local `Vec` in `analyze_to_convergence` (`fa.cc:8931`), passed to this
function and never used again; every later stage re-collects its own
population. **"passes to stage2" is aspirational** — there is no handoff.

**3. Stage 2 runs, on a disjoint population, and finds nothing.**
`IFA_DBG_STARTERS`:

```
[starters] p=2 avs=94 with_setters=94 with_csmap=24 -> confluences=5 starters=24
[sfs] p=2 split_css REACHED starters=24 -> 0
```

`split_css` is fed `setter_starters` from `collect_setter_confluences` — a
*setter*-derived set. `cs=983` is not in it (its element has no setters at
all: `elem_setters=-1`), so `split_css` never considers it. Its 36 `[scss]`
lines are `range`, `Exception`, `StopIteration` and friends, **every one
`starter_set=1 defs=1`**, so the `while (starter_set.n > 1)` loop never
executes and the return is 0. `cs=983` never appears.

The probe comment already in the source says exactly this
(`fa.cc:7700`): *"split_css can only partition CreationSets that some
starter's cs_map names, so an empty starter set means it never runs."*

**So the failure is a plumbing gap, not a policy one.** A type confluence
on a CreationSet contour is detected and then discarded, and the one
mechanism that could act on it is never told.

### Fixed 2026-09-06 — `split_css_by_defs`, a new last-rung stage

`FAPassStage::CS_DEF_PARTITION` (`split_css_by_defs`, `fa.cc`), gated by
`PYC_CSDEFSPLIT`, **default 1**. Three parts:

1. The `tc_skip_cs` branch of `split_ess_for_type` now stashes
   `(CreationSet *)av->contour` in `tc_cs_dropped` instead of only
   counting it. Its log line said "passes to stage2" and nothing did;
   it now says `deferred to CS_DEF_PARTITION` and something does.
2. `split_css_by_defs` drains that list. For each live CreationSet with
   `1 < defs < 10` it gives every creation point after the first its own
   CreationSet, via the re-point `split_css` already uses.
3. It runs as the pass's **last rung**, gated on quiescence of every
   stage above — shedskin's route-4 placement — so anything a finer route
   can separate is separated first.

Default-on is safe *and verified*, not assumed: at the default no
CreationSet has more than one creation point (`multidef=0` over 127 522
CreationSets corpus-wide, ifa/129 step 3), so the `defs > 1` test declines
everything. Measured: default suite 311/0 unchanged, both backends.

**Result on the suite** — ifa/129's `PYC_CSDCPA1=2` bill, **16 → 11**:

| now passing | was |
| --- | --- |
| `list_append_is_amortized` | this issue's own group |
| `listcomp_element_separation` | "illegal call argument type" |
| `itertools_count_forloop` | undiagnosed |
| `iterator_protocol_bridge` | undiagnosed |

Three of the four "undiagnosed" failures were this bug, which is why they
produced no diagnostic of their own. On the reproducer, `cs=983`'s six
creation points become six contours and `STAGES` reads
`TYPE_CONFL CS_DEF_PART`; output matches CPython.

Cost is nil where it does not fire: `sieve` under `PYC_CSDCPA1=2` is
identical with and without it (16/17 passes, `ess` 245/250, 1.50 s vs
1.53 s).

**Corpus `check`, default arm** (`check__default__ece3a980+a471d532`
against the recorded baseline `check__default__a935532b+adf4abe8`):

| | baseline | this tree |
| --- | --- | --- |
| compile_fail / run_fail / stdout_differs / with_warnings | 2 / 39 / 24 / 44 | **2 / 39 / 24 / 44** |
| container CS / shapes | 3748 / 626 = 5.99 | 3713 / 626 = **5.93** |
| `pratio` | 3.92 | **3.89** |

Every verdict column identical. **The −35 CreationSets are NOT this
stage** — measured, not assumed: `IFA_DBG_CSDEFSPLIT` records **zero**
splits at the default on `chess`, `rubik2`, `sieve` and `go`, confirming
the `defs > 1` test declines everything there. The −35 is the
`__delitem__` fix above, which removes a false element edge and with it
some downstream contour pressure. A small, free win on the default path,
recorded here so it is not mis-attributed later.

**Still failing, and they are NOT this mechanism declining wrongly:**

- `plcfrs_grammar_tables_nonconvergence` — its only CS-contour
  confluences are `dict` with `defs=1`. Correctly declined: one creation
  point, nothing to partition. A different cause.
- `builtins` — the offending `list` CreationSet never reaches the stage
  at all, so an earlier stage claims progress on every pass and the last
  rung is never gated in. Worth a look; it is a scheduling question, not
  a partitioning one.

### Why the fix was smaller than this issue's history suggests

`cs->defs` is **exactly** the set of AVars whose `cs_map` names `cs` —
they are populated from the same variable, back to back:

```c
fa.cc:868   v->cs_map->put(s, cs);
fa.cc:869   cs->defs.set_add(v);
```

So `split_css`'s re-point, `v->cs_map->put(cs->sym, new_cs)`
(`fa.cc:7982`), applies to defs unchanged. No new state, no new
invariant, no attribution.

What is missing is a **caller**: in the `tc_skip_cs` branch, take
`(CreationSet *)av->contour` and, when `1 < cs->defs.set_count() < 10`
(shedskin's route-4 cap), partition its defs and re-point each group.
`defs=6` on this reproducer is inside the cap.

Order matters and shedskin already fixed it: wholesale is the LAST rung.
Try the finer routes first and fall through to it, so precision is given
back only where nothing finer separates the conflict.

## Why the obvious fixes do not work

**~~Splitting `cs=983` by element contribution is not computable.~~
OVERSTATED — corrected 2026-09-06.** shedskin computes exactly this, and
pyc has not implemented what it does. The accurate claim is the narrow
one: *the two attribution attempts below failed, and the second was never
validated.* Read the correction at the end of this section before citing
this paragraph. The element channel does record only the union — that part
stands — but the attribution is recoverable from the FLOW, which is where
shedskin gets it. Attribution was attempted two ways:

- `writer->container ∈ cs->defs` — wrong by construction. Creation points
  are allocation sites; a set operation *writes into* an object and its
  `container` is the CreationSet-typed receiver.
- back flow from `writer->container` to the creation points — the correct
  formulation. Reaches **0 of 6** creation points in 10-22 steps
  (`IFA_DBG_ATTRIB`). Whether that is a defect in the walk or a property
  of the graph is **not established**; the probe has not had the
  known-answer validation `IFA_DBG_FWDALL` received, and should get it
  before anything is built on it.

**Correction 2026-09-06 — what shedskin actually computes, and how pyc's
probe differs.** `ifa_flow_graph` (`infer.py:1715`) does three things
`IFA_DBG_ATTRIB` (`fa.cc:11662`) does not:

1. **Groups the incoming edges by assigned type first** —
   `assignsets.setdefault(merge_simple_types(types), []).append(target)`,
   giving `{int: [targets…], str: [targets…]}`. The question is asked per
   TYPE, not per writer. Ladder routes 1 and 3 then use `n.paths` (which
   assign-sets a node lies on), which exists only because of this
   grouping.
2. **Walks back from the ASSIGN TARGET**,
   `gx.cnode[gx.assign_target[a.thing], …]` — the node for the container
   being assigned into. pyc's probe starts at `b->container` for each
   writer, and this issue already established (`ca11b67f`) that a set
   operation's container is not a creation point. Different node.
3. **Filters every hop**: `backflow_path` (`infer.py:2031`) follows
   `node.in_` only `if t in gx.types[incoming]` — only through nodes
   carrying this `(class, contour)`. It is a walk through the CONTAINER's
   own flow. pyc's probe follows every `a->backward` edge unconditionally.

Then `creation_points[assign_set] = [n for n in path if not n.in_]` — the
roots of that filtered walk. That IS the attribution.

The missing filter is the diagnostic detail: it makes pyc's walk strictly
MORE permissive than shedskin's, so it cannot explain reaching FEWER
nodes. `0 of 6` is not a walk that was too narrow; it is a walk that
started somewhere else and grouped nothing.

**So routes 1-3 are unattempted, not ruled out.** That matters for the
cost recorded in ifa/129: wholesale route 4 buys compile fixes with +191
CreationSets corpus-wide precisely because the finer routes that would
separate the same conflicts with fewer contours have never been built.
Implementing `backflow_path` properly — group the element's backward edges
by merged type, walk back from each group through AVars carrying this
CreationSet, take the roots — is the reconciliation, and nothing measured
so far says it cannot work.

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

## The quiescence gate is the wrong gate (root-caused 2026-09-06)

`CS_DEF_PARTITION` runs `if (!analyze_again)` — only on a pass where
*nothing anywhere in the program* found work. On `sha` under
`PYC_CSDCPA1=2` that never happens, so the stage never runs at all.

**Measured, after an earlier version of this claim was wrong.** Absence of
`IFA_DBG_CSDEFSPLIT` output has three causes — never reached, reached with
an empty candidate list, or every candidate filtered as dead — and they
are not the same finding. An `ENTER` line now distinguishes them, and it
was needed: on `builtins` the stage IS reached (2 entries) and the
"starvation" reading there was simply false; its 48 candidates are all
`defs=1`, which is a different defect. Do not infer a gate problem from
silence.

With the instrument in place:

| | `ENTER` passes | candidate available | outcome |
| --- | --- | --- | --- |
| `sha` | **0** of 28 | `cs=1054` dropped on **26** of 28 passes | never partitioned |
| `builtins` | 2 | 48 candidates, all `defs=1` | nothing to partition; different cause |

**26 offers, 0 openings.** And no finer stage claimed it in between — it
is the *same* CreationSet recurring, so the protection the gate exists to
provide had 26 chances to fire and did not.

**The defect is that the gate asks a GLOBAL question to answer a LOCAL
one.** Its purpose, per the `CSM_ELEMENT_CS` placement comment, is
per-candidate: do not preempt a finer route that could separate *this*
conflict with more precision. It is implemented per-pass and
program-wide, so whether a needed decision is taken depends on unrelated
activity in unrelated functions. `sha` and `builtins` differ only in how
noisy the rest of the program is.

The local form of the same protection: partition a CreationSet that has
carried the same irrepresentable confluence for N consecutive passes with
no finer stage claiming it. That preserves "finer routes first",
terminates, and does not couple to unrelated work. It is not implemented.

### The experiment: forcing the gate and the cap

`PYC_CSDEFSPLIT=2` (default 1; **experiment arm, not a shipping mode**)
ignores both the quiescence gate and `kCsDefSplitMax`. On ifa/129's
group A — the nine corpus programs failing with a merged-container
`mixed basic types` or `{scalar, list}`:

| | |
| --- | --- |
| **compile fixed, rc 1 → 0** | `sha`, `pisang`, `sudoku3`, `msp_ss` — **4 of 9** |
| still failing | `othello2`, `plcfrs`, `rdb`, `sudoku5`, `linalg` |

So the mechanism can reach the cases it was built for, and splitting the
CreationSet really does resolve them — 132's element-flow does **not**
re-pollute the split contours, which was the open question.

**Do not read the 4 as 4 programs made correct.** `sha` already printed
the wrong answer at the DEFAULT (`compile_rc=0 run_rc=0 stdout_match=NO`),
so fixing its compile returns it to the default's wrong state, not to a
right one. The result establishes reachability, not correctness.

The 5 that remain need more than the gate: their unions carry `list`,
`tuple` and `Char` alongside the scalars, so more than one merge is in
play per program.

### The per-candidate gate — implemented 2026-09-06

`CreationSet::defsplit_offers` / `defsplit_last_pass` (durable, like
`elem_key_pass` beside them) count the CONSECUTIVE passes a CreationSet
has been offered to this stage without being acted on.
`split_css_by_defs` is now called on every pass and takes `quiescent`:

- a quiescent pass behaves exactly as before;
- on a non-quiescent pass, only a candidate with
  `defsplit_offers >= kCsDefSplitRipe` (3) may be partitioned.

That is the local form of the protection the global gate was reaching
for, and it is self-enforcing: **a conflict a finer route can separate
stops recurring, so it never ripens.**

| | before | after |
| --- | --- | --- |
| default suite, both backends | 311/0 | **311/0** |
| `PYC_CSDCPA1=2` suite | 11 failed | **9 failed** |
| ifa/129 group A (9 corpus programs) | 0 | **5 compile** |

`sha`, `pisang`, `sudoku3`, `msp_ss`, `sudoku5` compile;
`deepcopy_copy_of_copy_chain` and `plcfrs_grammar_tables_nonconvergence`
join the suite. This issue's group is down to `splitter_cartesian_product`
alone.

Default corpus `check` (`check__default__af7176ef+ac5f4016`) is
**byte-identical on all 77 programs** to the previous default arm —
`compile_fail=2 run_fail=39 stdout_differs=24 with_warnings=44`,
`3713/626 = 5.93`.

**The ripeness gate beats the forced experiment, 5 of 9 against 4, and
the reason matters.** Forcing also discarded `kCsDefSplitMax`. `sha`'s
`cs=1054` carries `defs=18` at pass 1 — over the cap — but only `defs=6`
by the time it ripens. Waiting does not merely find a safe moment; it
lets the def count settle into the cap's range. The cap and the wait are
complementary, so the earlier reading that the cap needed raising was
wrong.

*One bug introduced and caught by the goldens.* Removing the call-site
gate made that block run every pass, so `if (analyze_again)` — which for
every OTHER stage means "this stage found work", since they are all gated
on `!analyze_again` — began attributing stage 1's progress to
CS_DEF_PARTITION. `make test` failed with 15 `fa-converge` failures
showing a phantom `pass 1 ? splits=1` event. The stage's own return value
is now kept in a separate local. Worth recording that `fa-converge`
caught two separate mistakes in this issue's work, and that the first
instinct both times was that the goldens were stale. They were not.

## Plan — port shedskin's ladder routes 1-3

**Feasibility measured 2026-09-06 before writing this**, with
`IFA_DBG_ATTRIB2` (default off, beside the old `IFA_DBG_ATTRIB`), which
adds the two differences the old probe was missing: group the element's
backward edges by canonical `AType`, and filter every hop on
`x->out->type->set_in(cs)`. On this issue's five-line reproducer:

```
ATTRIB2 cs=983 sym=list defs=6 assignsets=3
  set[0] type= int64 str  targets=2 walked=12 roots=2 reached_defs=0/6
  set[1] type= int64      targets=1 walked=12 roots=1 reached_defs=0/6
  set[2] type= str        targets=1 walked=12 roots=1 reached_defs=0/6
```

**The attribution works.** Three assign sets, and the pure ones separate:
`{int64}` walks back to root `av=785`, `{str}` to root `av=794`, and they
are different nodes. That is exactly the per-type partition routes 1 and 3
key on, and the old probe's `0 of 6` was an artifact of not grouping and
not filtering.

**One gap, and it is specific.** The roots are not `cs->defs` members, not
their `lvalue`s, not forward neighbours of a def, and — measured —
**carry no `cs_map` at all**. Both existing re-point mechanisms
(`split_css`, `split_css_by_defs`) act by `v->cs_map->put(sym, new_cs)`,
so a root cannot be re-pointed. shedskin does not have this problem
because its creation points ARE its handles: `ifa_split_class(cl, dcpa,
things, ...)` takes the nodes the walk found. pyc's handle is the def
AVar, and the walk lands elsewhere.

### Is it localized and compatible?

**Localized: yes.** Every input already exists — `elem->backward`,
`b->container`, `av->backward`, `av->forward`, `av->out->type`,
`cs->defs`. `paths` and `csites` are per-invocation locals, so there is no
new persistent state, no struct-layout change, no IR change, no frontend
change. It is one function beside `split_css_by_defs`.

**Compatible: yes except the handle gap**, and one deferral:

- The **root → def bridge** is the only piece with no analogue. Use
  forward closure from each def and assign a root to the def whose closure
  contains it. That is `IFA_DBG_FWDALL`'s direction, and it is the one
  probe this issue records as *validated on a known-answer case* — the
  backward probe never was, which is what produced the retracted "not
  computable" claim.
- **Route 1 needs contour REUSE** — move sites onto an existing contour
  keyed by deduced element types (`ifa_class_types` / `classes_nr`,
  `infer.py:1632`). pyc has the machinery in `split_css`'s ledger route
  (`cs_group_signature` → `ledger_find_cs`) but has never run it in the
  joining direction. Deferrable: mint instead of joining, at a precision
  cost, and revisit.

### Steps

1. ~~**Bridge roots to defs.**~~ **NOT NEEDED — the roots ARE the defs.**
   Measured inside a function, which is the general case:

   ```
   root av=2694 fun=f  in_defs=1  csmap=1  backward_all=0  carries_cs=1
   set[0] type= str  targets=2  roots=1  reached_defs=1/5
   ```

   The root is a creation point and it carries a `cs_map`, so it is
   directly re-pointable — exactly shedskin's arrangement, where the
   creation points found by the walk are the handles
   `ifa_split_class` acts on. No bridge, no forward closure. **Skip to
   step 2.**

   `reached_defs=1/5` is also right rather than low: one assign set has
   one creation point, and the other four defs are library `[]`s that are
   not on its path. That is shedskin's `csites` / `emptycsites`
   distinction appearing on its own.
2. **Build `ifa_flow_graph`'s outputs** as locals: `assignsets`, `paths`,
   `creation_points` per assign set, `csites`, `emptycsites`
   (`cs->defs - csites`), and `n.paths` per node. Nothing splits yet.
   *Verify:* the census is stable across passes and the counts are sane
   corpus-wide.
3. **Route 1, `ifa_split_no_confusion`** (`infer.py:1585`): unconfused
   sites (`len(n.paths) == 1`) plus empty csites, grouped by the
   attribute-type tuple each would produce; split each group off. Mint
   rather than reuse for now.
4. **Route 3, partition csites across paths** (`infer.py:1571`): group
   sites by the set of types on their paths; if that yields more than one
   group, split the first off.
5. **Demote route 4.** `split_css_by_defs` becomes the last rung it was
   always meant to be — reached only when 1 and 3 decline.
   *Verify:* the +191 corpus contours from route 4 fall, the six compile
   fixes hold, `pratio` improves.
6. **Route 1's contour reuse**, if step 3's precision cost shows up.

### ~~The blocking finding~~ — RETRACTED, it was module scope

An earlier revision claimed the CreationSet was being seeded into the
graph by something other than `creation_point`, on the evidence that the
reproducer's roots had `backward_all=0`, `carries_cs=1`, no `cs_map`, and
were not in `cs->defs`. **Wrong, and wrong in a way worth recording.**

`backward_all=0 && carries_cs=1` is not the signature of a mystery
seeder — **it is exactly what a creation point looks like.**
`creation_point` ends with `update_gen(v, make_AType(cs))` on an AVar
with no incoming edge. The measurement was the thing being looked for,
read as evidence against finding it.

The `!in_defs && !csmap` half is specific to **module scope**: at the top
level the reproducer's roots have neither, while the same program inside
a function gives `in_defs=1 csmap=1`. What module scope does differently
is **not** established — the obvious guess, that these are
`GLOBAL_CONTOUR` AVars, is measured FALSE (`global=0` on both). It does
not block routes 1-3, which act on ordinary function contours; it is a
loose end to characterize, not a blocker.

### The stop condition

If step 1 cannot bridge roots to defs, routes 1 and 3 are not reachable
this way and **that is the answer** — record it, do not invent a handle by
matching on names or positions. The retracted claim in this issue came
from treating one failed probe as proof; the same mistake is available
here in the other direction.

## Steps 2-5 implemented 2026-09-06 — `PYC_CSLADDER`, default 0

`CSFlowGraph` / `build_cs_flow_graph` is shedskin's `ifa_flow_graph`:
assign sets (the element's backward edges grouped by canonical `AType` —
pyc's hash-consing gives for free what `merge_simple_types` computes
there), filtered backflow paths, per-set creation points, `csites`,
`emptycsites`, and `site_set_count` for `n.paths`. Then:

- **route 1**, `cs_ladder_no_confusion` — sites on exactly one assign set,
  plus `emptycsites`, grouped by element type; peel one group;
- **route 3**, `cs_ladder_path_partition` — group sites by the union of
  the types across the assign sets they lie on;
- **route 4 demoted** — `split_css_by_defs`'s wholesale partition now runs
  only when 1 and 3 decline, which is the position shedskin gives it;
- **the demand test** (`infer.py:1526`), `csites + emptycsites == 1 →
  decline`, gating all of it.

`cs_peel_group` re-points only members whose `cs_map` names the
CreationSet, and declines a group that is ALL the defs (a rename, not a
split). Module-scope roots have no `cs_map` and are skipped rather than
forced.

**Corpus result** (`check__PYC_CSDCPA1_2_PYC_CSLADDER_3__27ffb6e5+a58dbd70`
against the same-tree ladder-off arm):

| | off | on |
| --- | --- | --- |
| compile_fail / run_fail / stdout_differs | 12 / 35 / 23 | **identical** |
| container CS / shapes | 2955 / 604 | **2910 / 599** |
| ratio / pratio | 4.89 / 3.20 | **4.86 / 3.17** |

**Every verdict on all 77 programs is unchanged** — `compile_rc`,
`run_rc`, `stdout_match` — for −45 container CreationSets. Suite: default
311/0 both backends, flag arm 9, both unchanged.

**−45 against route 4's +191, and the reason is step 6.** Routes 1 and 3
as built only ever MINT a new contour for a peeled group; they never JOIN
one. shedskin's route 1 consults `classes_nr` first
(`infer.py:1617-1624`) and *moves* the group onto an existing contour when
the element types already match. Without that, the ladder can only split
less than wholesale would — it cannot take a contour away. **That is where
the rest of the +191 is, and it is step 6.**

### Two bugs, recorded because of how they were caught

**Route 3 was implemented from this issue's summary, not from the
source, and it segfaulted the compiler.** The summary says "group sites by
the set of types found on their paths"; shedskin does
`for p in c.paths: tspaths.update(p)`, where each `p` is an assign-set KEY
that is itself a set of types — so the signature is the UNION OF THE
TYPES, not the set of assign sets by identity. Identity is strictly finer,
so sites shedskin keeps together were separated; `sudoku3` came out with
`self.squares[row][col]` typed `int64` instead of a list and codegen died
on the untyped rval (`c_type(s=0x0)`, `cg.cc:1083`). Making the two rungs
bit-selectable (`PYC_CSLADDER` 1 / 2 / 3) isolated it in one step.

**The demand test was omitted entirely** — the one line the whole ladder
exists for. Added.

**And a contour measurement was taken from a crashed run.** The first A/B
reported `sudoku3` 58 → 54 as an improvement; that run segfaulted, and
`sudoku5`'s "+4" came from a failed compile. `container_cs` was read off
the `DEMAND` line without checking the exit status. With `rc` checked
alongside, `sudoku3` is unchanged at 58 and the "improvement" was entirely
the bug. **Never report a contour delta without the exit status beside
it** — a broken analysis produces small numbers.

## Step 6 — contour reuse: built, measured, does NOT pay

`cs_reuse_contour` is shedskin's `classes_nr` lookup
(`infer.py:1617-1624`): before minting for a peeled group, scan the live
CreationSets of the same sym for one whose element AType already equals
what the group wants, and JOIN it instead. Rebuilt from the current
contours on every call, as `ifa_class_types` does — there is no index to
go stale, which is ifa/129 step 2b item 1's complaint about
`cselem_shape_canon`. Read-only: only contours that already have an
element AVar are considered, so the accessor that CREATES one is never
called.

`PYC_CSLADDER` became a bitmask so the pieces could be attributed
separately: `1` route 1, `2` route 3, `4` reuse, `8` key the emptycsites
group on the bottom AType, `16` peel every group in route 1 as shedskin
does.

**Result: 3 is the measured-good configuration and 6 does not improve
it.**

| `PYC_CSLADDER` | suite under `PYC_CSDCPA1=2` |
| --- | --- |
| **3** — routes only, first group per pass | **9** |
| 27 — + all groups + empties keyed bottom | 10 |
| 31 — + reuse | 10 |

The regression at 27/31 is **`deepcopy_copy_of_copy_chain`**, which is
ifa/105's acceptance test and the exact case ifa/129 names as the thing
that must not come back. It fails with `a variable holding 'int64' has no
representation: '__add__' resolved to the CONTAINER method` — the same
call-resolution-wearing-a-representation-message as `pystone`.

**And the reuse itself is nearly inert.** Across nine programs it fires on
two — `pisang` and `chull`, two joins each — and changes no contour total.
The lookup is not blocked; it is instrumented and reports honestly. On
`sha`: `cand=29 dead=0 no_elem_var=0 arity=7 elem_mismatch=22`. **No
contour of that sym has the element type the group wants**, because under
`PYC_CSDCPA1` the contours that exist hold merged element types and the
group wants a pure one. shedskin's index finds hosts because its contours
are re-derived to convergence every iteration; pyc's are not.

**Why 16 (all groups) regresses, named rather than tuned away.** shedskin
collects every group's decision into `split` and applies the whole list at
the end of the iteration. This implementation peels as it goes, so the
second group's `cs_peel_group` sees a `cs->defs` the first peel already
shrank. That is exactly the discipline `split_ess_for_type`'s own M2b
comment describes — *"DECIDE every confluence's split against the same
unmutated, converged state, then APPLY"* — and it was not applied here.
Fixing 16 means decide-then-apply, not a smaller cap.

*Kept, default off, with the bits intact*, because they are the
reproduction of this measurement. The landed behaviour is `3`.

**What step 6 was supposed to buy and did not:** the ~146 CreationSets of
route 4's +191 that routes 1 and 3 do not recover. Joining is still the
only operation that can take a contour away, and it is still unavailable
in practice — not for want of the mechanism, which now exists and runs,
but because no host contour has the wanted element type. That is a
statement about pyc's contour population, not about the reuse rule, and
it is the thing to attack next.

## The ripeness counter was on the wrong object (fixed 2026-09-06)

`CS_DEF_PARTITION`'s ripeness gate counted consecutive offers on the
**CreationSet object**. Under `PYC_CSDCPA1` the CS population churns every
pass, so a candidate never survives long enough as the same CS and the
counter resets before reaching the threshold. Measured on
`tests/splitter_mark_type.py`:

```
p=2  cs=1015 defs=4  WAIT (offers=1 < 3, pass not quiescent)
p=3  cs=1049 defs=1  DECLINED
p=3  cs=1051 defs=2  WAIT (offers=1 < 3, pass not quiescent)
```

A different CreationSet each pass. **That is the same failure as the
quiescence gate this replaced, reached from the other direction** — a gate
that structurally never opens.

Fixed by counting on the LINEAGE ROOT: `cs->split_origin` is durable
(ifa/066) and already collapsed to the root at construction, so the count
survives the churn.

*Result:* `splitter_mark_type` and `splitter_cartesian_product` go from
hard failure to **rc=0** — they compile — and drop to one warning each.
Both still fail their goldens, but on a warning rather than a refusal.
Default 311/0, flag suite still 4.

**And the two `splitter_*` tests are NOT this issue's family.** `MIXELEM`
reports nothing for them: `{A, B}` is a union of two *pointers*, which is
perfectly representable. They compile and run; they fail because
`self.aas` holds `{A, B}` and `.ay()` exists only on `A`, so the call
takes an `illegal call argument type` warning. That is a **precision**
regression, not an irrepresentable-union one, and it needs `aas` and `bbs`
separated for precision rather than for correctness. Every remaining list
CreationSet there already has `defs=1`, so partitioning by creation point
cannot do it.

## The predicted case turned up in `pyc_lib/heapq.py` (fixed 2026-09-06)

When `__delitem__`'s false `merge_in` was fixed above, this issue recorded
the exact boundary the fix would NOT cover:

> **It does not fix the class, and the boundary is exact.** A user writing
> the slice assignment by hand still reproduces, because there the
> `merge_in` is real:
>
> ```python
> a = [1, 2]
> a[0:1] = []      # genuine setslice
> ```

`pyc_lib/heapq.py:40` was that line, verbatim:

```python
def heappop(heap):
    n = len(heap)
    lastelt = heap[n - 1]
    heap[n - 1:n] = []      # <-- shrink by slice-assigning an empty list
```

and it is what made `tests/test_heapq.py` fail under `PYC_CSDCPA1=2`.

**Found by probe, not by guessing.** `IFA_DBG_CSVARS` (extended here to
print the element channel and its writers) on the offending CreationSet:

```
CSVARS cs=1014 sym=list vars=10 defs=1 arity=10 elem= int64 tuple tuple tuple
  ELEMWRITER es=207 fun=__pyc_setslice__ type= tuple#1057 tuple#1069 tuple#1072
```

`data = [9, 4, 7, 1, 6, 2, 8, 3, 5, 0]` — ten `int64` positional vars —
with `tuple` in its element channel, written by `__pyc_setslice__`. Under
one CreationSet per sym the `[]` in `heappop` shares a contour with every
other empty list in the program, including the tuple-holding heaps in
`fringe`, and `merge_in(self, v)` carries their elements into `data`.
Codegen then emitted `((_CG_void*)(_CG_list_ptr(t10)))[6] = 8` — an int
stored through a pointer-typed element.

**The file's own header explained the workaround and was stale:** *"list
shrinking uses slice-assignment to an empty list since pyc's list has no
pop()."* `list.pop()` was added by issues/025 R1. Replaced with
`lastelt = heap.pop()`, which is what CPython's `heapq` does.

*Result:* `test_heapq` compiles on both arms, runs, and its stdout
**matches CPython**. Flag-arm suite **3 → 2**. Default 311/0 on both
backends, all six gates green.

**Worth keeping in view:** this was a latent correctness bug in a shim,
sitting behind a comment that justified it. The start-merged flag did not
create it — it removed the per-site contour that had been hiding it. That
is the second such case (after `__delitem__`), and both were found only
because the flag exposed them.

## `kCsDefSplitMax` was the blocker for four corpus programs — REMOVED 2026-09-07

Root-causing `voronoi2` and `linalg` (2026-09-07) put both in THIS issue's
family rather than
[137](137-scalar-receiver-resolves-to-container-method.md)'s, where
ifa/129 had classified them. The `'__add__' resolved to the CONTAINER
method` diagnostic is downstream in both:

- **`voronoi2`** — `cs=1702`, ONE creation point inside `list.__mul__`,
  element takes `str` from `__mul__` and `Halfedge` from `__setitem__`.
  Two different `[x] * n` results sharing a CreationSet.
- **`linalg`** — `cs=1011`, **`defs=44`**, `elem= int64 list x8`.
  Forty-four `[]` literals in one CreationSet.

`linalg`'s 44 defs are far past `kCsDefSplitMax` (10), so
`CS_DEF_PARTITION` declined on the cap and the program did not compile.

### The recommendation recorded here on 2026-09-07 was WRONG

It said *"uncapping yields zero additional programs that produce correct
output... raising it buys compile-status cosmetics at a real cost in the
goal metric"*, and **recommended keeping the cap**. Both halves fail.

**The correctness half compared the uncapped arm against a standard the
DEFAULT does not meet.** It is true that `linalg`, `quameon` and
`sudoku3` abort at run time after uncapping — but they abort at run time
*at the default too* (`run_rc=134` for all three), and `voronoi2` prints
the wrong answer at the default too. Uncapping does not convert working
programs into [102](102-corpus-programs-compile-then-abort-at-runtime.md)
cases; it brings four programs to **exact parity with the default**,
which is what the flip needs. Nothing was made worse than the baseline
the flag has to match.

**The contour half quoted a flag-vs-flag number as if it were the cost.**
`2835 → 3273` is `+15%` measured against the capped flag arm — but the
baseline that matters is the DEFAULT's `3713`, and against that the
uncapped arm is **−11.9%**. Uncapping gives back about half the contour
win; it does not spend a real budget.

**And the design half was backwards.** The old text said `linalg` "needs
the collapse prevented rather than partitioned afterwards — the 44
literals should not have become one contour in the first place." That
argues for per-creation-point identity, which is precisely what this flag
exists to remove. Under demand splitting, 44 arity-0 `[]` literals
starting as ONE CreationSet is the CORRECT initial state — it is what
"one CreationSet per sym" means — and the goal is minimal contours
**subject to demand**, not minimal contours. Forty-four literals whose
element types genuinely differ *demand* separation, and those contours
are not structural waste. The defect was never the merge; it was that no
mechanism was allowed to undo it.

Route 4 is the only mechanism that can. ifa/142 measured why: the element
union reaches a **fixed point where every writer carries the whole
union**, so `etype == stype` on every edge and no type-based partition
(routes 1 and 3, TYPE_CONFLUENCE, the element-CS stages) can tell the
contributors apart. Only separation BY CREATION POINT breaks it. The cap
refused that one mechanism on exactly the programs that most needed it,
because 10 was copied from shedskin, where contours start at one per
CLASS and a 44-way merge never arises.

### Measured, capped vs uncapped, both at HEAD

Verdict parity against the default, per program (77 programs):

| | capped | uncapped |
| --- | --- | --- |
| programs whose verdict DIFFERS from the default | 11 | **4** |
| ... of those, regressed to `COMPILE-FAIL` | 8 | **2** (`plcfrs`, `rdb`) |
| compile_fail | 9 | **4** |
| container CS (default = 3713) | 2835 (−23.6%) | **3273 (−11.9%)** |
| suite failures | 2 | 2 |

The 11 → 4 is the headline. The capped arm diverged from the default on
`linalg`, `plcfrs`, `quameon`, `rdb`, `richards`, `softrender`,
`sudoku3`, `voronoi2`, `sudoku5`, `bh`, `kanoodle`; the uncapped arm
diverges on **`plcfrs`, `rdb`, `bh`, `kanoodle`** only. `sudoku5` is the
single program the cap helped (it compiled capped, and is `COMPILE-FAIL`
both uncapped and at the default — so uncapping returns it to parity too,
having been the one place the cap accidentally did better than baseline).

`bh` and `kanoodle` are NOT cap-related: both arms regress them
identically, so they are separate flag defects that the cap discussion
had been masking. Isolated for the first time 2026-09-07:

| program | default | flag (both arms) | diagnostic |
| --- | --- | --- | --- |
| `bh` | `run_rc=0` | `run_rc=134` | `bh.py.c:7520 ... Assertion '!"runtime error: getter not resolved"'` |
| `kanoodle` | `run_rc=0`, stdout `NO` | `run_rc=139` (SIGSEGV) | — |

`bh` is the more serious of the two: it is the only program in the corpus
that RAN cleanly at the default and aborts under the flag. **Root-caused
2026-09-07 — see [143](143-shared-container-method-contours-refuse-cs-splits.md).**
The `getter not resolved` assert is downstream of three `__slots__` string
literals merging into the node lists; what makes it unfixable by THIS
issue's mechanisms is that `list.append` and `list.__setitem__` each have
ONE EntrySet program-wide, and they write the whole element union back
into every receiver CreationSet. Forcing route 4 to partition the merged
CS into one contour per creation point leaves all of them irrepresentable.
Separating containers is pointless while a shared method contour re-fuses
their element channels.

**So the flip blocker list is now exactly four**, in priority order:
`bh` (ran → abort), `rdb` and `plcfrs` (compile failures), `kanoodle`
(wrong output → segfault).

**Status: cap removed** from `split_css_by_defs`. `PYC_CSDEFSPLIT=2` now
means only "ignore the ripeness wait". All six CI gates pass at the
default (16/16 `test-ir` phases, 0 failed), and the default arm is
unaffected in principle — at `PYC_CSDCPA1=0` each site already has its
own CreationSet, so `defs.n > 1` rarely holds.

`kCsDefSplitMax` survives as a symbol because `split_es_by_call_site`
still uses it — and there it is wrong for a DIFFERENT reason worth its
own measurement: it caps on the CALLER count, while mode 2's partition
size is the number of distinct element types, so a 12-caller function
contributing 2 element types is declined for a fan-out it would never
have produced.

## `rdb` — as far as it goes (2026-09-07)

The union surfaces at `rdb.py:244`:

```python
iTunesSD.write(entry[:555] + bytes([int(props['shuffle']), int(props['bookmark']), entry[557]]))
```

as `'v' has mixed basic types:( int64 str )`, `v` being the loop variable
in `__pyc__`'s `__pyc_tobytes__`.

Traced with `IFA_DBG_CSVARS`:

```
CSVARS cs=1260 sym=list vars=3 defs=1 arity=3 elem= str
  ELEMWRITER es=68 fun=__pyc_getslice__ type= str#8
  DEF        av=4254 es=68 fun=__pyc_getslice__
```

An **arity-3 list whose element channel holds `str`** — the same
positional/element disagreement ifa/139 fixed for `builtins`, but by a
different route: here both the creation point and the writer are
`list.__pyc_getslice__`'s `merge(self, self)`.

**What is NOT the cause:** `__pyc_getslice__` has exactly **one contour in
both arms**, so this is not a contour-count difference and not ifa/136's
shape.

**What differs:** the default has **218** list CreationSets and no arity-3
list with a `str` element at all; the flag has **114** and does. So the
merge that puts `str` into a three-int list is UPSTREAM of `getslice` —
`self` already carries it — and the flag's coarser CreationSet population
is what lets it.

**Not root-caused further.** The remaining step is to find which list
merge gives `getslice`'s `self` a `str` element when the caller's `entry`
should be `bytes`. That is a fifth distinct instance of the family and it
has resisted the same machinery as the rest.

## Status of the element-union family

`plcfrs`, `rdb` (flag-only), and `linalg`, `voronoi2`, `sudoku3`,
`quameon` (which uncapping would convert into runtime aborts, see above).

**Five mechanisms have been built and measured against this family this
session, and none reaches it:**

| | result |
| --- | --- |
| `CS_DEF_PARTITION` (route 4) | splits, but these are `defs=1` or over-cap |
| ladder routes 1 and 3 | −45 contours corpus-wide, no verdicts |
| contour reuse (step 6) | fires on 2 of 9 programs, no totals change |
| `PYC_ESFORCS` / type-side third clause | 427 of 428 decline `no_groups` |
| `PYC_CSCALLSITE` demand partition | fixed 1 program, 0 net |

The common shape is a container with ONE creation point whose element
receives two types through paths that agree on argument types and on call
site. Neither CS-side partitioning (nothing to partition) nor ES-side
splitting (nothing to split on) can separate that, and this issue's
conclusion stands: **the next attempt should not be a sixth splitter.**

What has actually moved this family, all session, is finding the specific
upstream merge and fixing it — `__delitem__`'s false `merge_in`,
`heapq`'s `[n-1:n] = []`, ifa/139's arity hole. Each was a single wrong
edge, found with a probe, not a new mechanism. `rdb`'s remaining step is
of that kind.
## The ripeness wait went with the cap (2026-09-07)

`kCsDefSplitRipe = 3` — "wait three consecutive passes before partitioning
on a non-quiescent pass" — was removed, along with `defsplit_offers` /
`defsplit_last_pass` on `CreationSet` and the `PYC_CSDEFSPLIT=2` arm.

**Its recorded justification was explicitly paired to the cap.** From this
issue, above: *"`sha`'s `cs=1054` carries `defs=18` at pass 1 — over the
cap — but only `defs=6` by the time it ripens. Waiting does not merely
find a safe moment; it lets the def count settle into the cap's range. The
cap and the wait are complementary."* With no cap there is no range to
settle into, so the stated purpose is void by construction.

**It was also actively harmful, because a finer rung firing RESET the
count.** On `bh`, `cs=1180` (defs=8, element union `{Body, str}`) reached
`offers=1` then `2`, the ladder split it, and the counter restarted —
twice, at p=0–2 and again at p=27–29 — so the rung never acted on the
CreationSet that needed it. On `sudoku4` the wait is the entire difference
between compiling and failing with `'str' is blind-cast to 'set'`
(ifa/123): that program compiled in the capped arm, the force arm and at
the default, and failed ONLY with cap-removed-plus-wait.

**Finer-rungs-first is not lost.** The ladder (routes 1 and 3) runs inside
`split_css_by_defs` *before* the wholesale partition, so every candidate
still gets finer refusal on every pass. What is gone is only the
three-pass DEFERRAL, which asked the coarsest rung to wait on a clock
rather than on the finer rungs actually declining.

| flag arm | differs from default | container CS |
| --- | --- | --- |
| wait present | 5 (`bh`, `kanoodle`, `plcfrs`, `rdb`, `sudoku4`) | 3269 |
| wait REMOVED | 4 (`bh`, `kanoodle`, `quameon`, `rdb`) | 3281 |

### A retracted claim, and the measurement error behind it

An earlier version of this section reported a THIRD row — "wait skipped via
`PYC_CSDEFSPLIT=2`", 4 divergences at 3260 CSs — and concluded from its
disagreement with the row below it that **the flag arm was
layout-sensitive**, since the two builds were supposedly semantically
identical and differed only by two `int`s on `CreationSet`. It advised
treating A/B on this arm as carrying ±1 program of noise.

**That is withdrawn. There is no layout sensitivity, and the third row was
measured on the wrong binary.**

The sweep was launched immediately after a `git stash pop` that restored
uncommitted `fa.cc` changes, **without an intervening rebuild**. The `pyc`
binary was still the one built from stashed HEAD (20f76f27) during the
bisection minutes earlier, so the run measured 20f76f27's code — cap
removed, wait present, `PYC_RECVFAN` still in, no demand-driven candidate
set — while `corpus_sweep.sh` filed it under the *source tree's* key.

Root-caused two ways, both conclusive:

- **Padding.** Adding two dummy `int`s back to `CreationSet`, rebuilding,
  and re-running gave results IDENTICAL to the unpadded build
  (`quameon` fail, `plcfrs` and `sudoku4` clean). Layout does not move
  this needle.
- **Reconstruction.** Rebuilding 20f76f27 and running with
  `PYC_CSDEFSPLIT=2` reproduces the retracted row exactly — `plcfrs`
  `compile_rc=1`, `quameon` `compile_rc=0`. Rebuilding HEAD reproduces the
  "wait REMOVED" row exactly — `plcfrs` `compile_rc=0`, `quameon`
  `compile_rc=1`. The two rows are two different compilers, not two
  layouts.

The `plcfrs` contour gap that made the claim look substantial —
`css=3839` vs `2553` — was the tell and I read it backwards: 1286
CreationSets is far too large for a layout perturbation and should have
been treated as proof of a code difference from the start, not as
evidence of one.

**The conclusion this section reaches is unaffected.** Both surviving rows
were measured on correctly-built binaries, and 5 → 4 (plus `sudoku4`
recovering) is the finding. Only the retracted row and the noise caveat
were wrong; the mislabelled `.tsv` has been deleted rather than kept,
because a sweep filed under the wrong tree is worse than no sweep.

**The trap, stated generally:** `corpus_sweep.sh` keys its filename on the
SOURCE tree but can only measure the BINARY. Any path that changes sources
without rebuilding — `git stash`/`pop`, `git checkout <commit> -- <file>`,
a failed compile leaving the old binary in place — silently produces a
sweep whose label and content disagree. `make` before every sweep, always.

## `sudoku5` is this issue, and only this issue (2026-09-09)

`sudoku5` was on the flag arm's divergence list as an independent item,
and it is not: it is 133 with one separable compiler bug sitting on top.

**The compiler bug, fixed.** `pyc` did not fail to type `sudoku5` on the
flag arm, it SIGSEGV'd (exit 139) while printing the diagnostic. Root
cause was unrelated to inference: `show_violations` reached for the call
tree via `cs->defs.first()`, but `defs` is a set-Vec, and past
SET_LINEAR_SIZE (4) a set-Vec is an open hash table whose `n` is the table
CAPACITY and whose empty slots are NULL. `.first()` is a raw `v[0]`, so it
read a hole. `sudoku5`'s merged `list` CS carries 13 creation points.
Fixed (`first_in_set()` plus guards); the crash is latent in ANY arm --
merging only supplies the 4+ defs that expose it.

**Attribution, once it stopped crashing.** The two flag-arm flags are not
jointly responsible; one of them is the whole story:

| arm | exit | warnings | errors |
| --- | --- | --- | --- |
| default | 0 | 24 | 0 |
| `PYC_CSLADDER=3` | 0 | 24 | 0 |
| `PYC_CSDCPA1=2` | 1 | 249 | 364 |
| both | 1 | 249 | 364 |

`PYC_CSLADDER=3` alone is byte-identical to the default. `PYC_CSDCPA1=2`
alone reproduces the full flag arm exactly. So `sudoku5` needs no separate
tracking: fix 133 and it goes.

**It is the LIST merge, not the tuples.** The reported union reads `( list
tuple int64 str )`, which invites the theory that arity-2 tuples
`("rc", (r, c))` and `(len(X[c]), c)` merged and unioned slot 0 to
`{int64, str}`. They did not. `PYC_CSDCPA1=2` deliberately EXCLUDES
`sym_tuple` from the merge (fa.cc:619, and the reason is written there:
arity and position are part of a tuple's type, not provenance), so tuple
contours are identical on both arms. Every `DEMAND-ADDED` candidate in the
`IFA_DBG_CSDEFSPLIT` trace is a `list`; the tuples only appear inside the
merged list's element union.

**Where it stops, in this issue's own terms.** Of 1271 declines in the
trace, the two that matter are `DECLINED (single creation point)` (1092)
and **`DECLINED (1 group: every creation point on the same assign sets)`
(81)**. The second is this issue's wall stated as a measurement: the flow
graph key puts every creation point in ONE group, because once the element
union has formed every writer carries all of it. That is the same fixed
point as [142](142-linalg-empty-list-collapse-is-a-fixed-point.md), and
the same thing "merging destroys the attribution" (ca11b67f, 9af32e64)
already concluded from the 5-line reproducer.

## Negative result: stage 5 starvation is NOT the lever (2026-09-09)

Worth recording so it is not retried. `fa.cc` says the VIOLATION stage is
starved on `plcfrs`/`rdb`/`sudoku5` -- TYPE_CONFLUENCE fires every pass so
`!analyze_again` is never true -- and the splitting log confirms it
exactly: **22749 `[stage1]` lines and 0 `[stage5]` lines**. Since stage 5
is the only DEMAND-driven stage in the cascade, lifting its gate looks
like it should follow directly from "splitting is only ever on demand".

Measured, via the existing `PYC_SIZEOF_VIOL=2` lever, on `sudoku5` at the
flag arm:

| | passes | ess | css | mixed | warnings | errors |
| --- | --- | --- | --- | --- | --- | --- |
| stage 5 starved (as shipped) | 37 | 1104 | 2634 | 10 | 249 | 364 |
| stage 5 RUNS (16 times) | 17 | 1461 | 2581 | 29 | 299 | **660** |

Errors nearly double and `mixed` triples. This is the non-monotone
diagnostic again -- more splitting, worse result -- so the starvation is
real but it is not what is holding `sudoku5` back, and un-starving stage 5
is not a fix waiting to be applied. The demand stage cannot help while the
demand it would act on names a partition that no longer exists in the
graph; attribution has to be recoverable FIRST.

## Why SETTER splitting does not solve this (root-caused 2026-09-09)

The author's position is that this is setter splitting's job. It is, and
the machinery is not broken -- it is looking at the wrong channel, and the
CreationSet that actually needs splitting is not the one anybody has been
looking at. Measured on `tests/list_mul_scalar_object_separation.py`
(`[0] * 4` and `[None] * 4`) with a temporary probe in `split_css`.

**The stage fires and does real work.** `PYC_DBG_STAGEDELTA` shows SETTER
returning 1 at p=1 (`d_ess=1`) and p=2 (`d_css=1`), and the CS it splits is
the right one by its own lights: the two `__mul__` result lists, which
arrive with 2 starters and separate cleanly into cs 1024 / cs 1035.

**And it buys nothing, because both halves are already poisoned:**

```
[scss] cs=1024 sym=list starters=1 defs=1 elem= int64#6 __pyc_None_type__#13 Task#1019
[scss] cs=1035 sym=list starters=1 defs=1 elem= int64#6 __pyc_None_type__#13
```

Each `__mul__` contour is separate and each still carries `{int64, None}`.
So the pollution is UPSTREAM of `__mul__`, and splitting its result was
never going to help.

**The real culprit is the LITERAL CreationSet:**

```
[scss] cs=1021 sym=list starters=1 defs=2  |vars|=1  var[0]= int64#6 __pyc_None_type__#13
    STARTER var=id11251 es=51 fun=__init__  nsetters=1
    DEF     var=id11251 es=51 fun=__init__  is_starter=1  nsetters=1
    DEF     var=id11287 es=52 fun=__init__  is_starter=0  nsetters=-1   <- setters is NULL
```

`[0]` and `[None]` are both `list` of arity 1, and CreationSet identity is
*(sym x arity)*, so under `PYC_CSDCPA1` they are ONE CreationSet whose
single positional slot unions `{int64, __pyc_None_type__}`. `__mul__` then
copies that slot into the element channel of every list it builds. That is
the whole defect; everything downstream is a consequence.

Confirmed by making the arities differ -- `[0] * 4` against
`[None, None] * 2`. The merged arity-1 literal CS never forms, and the
program compiles **clean** under the flag.

**Why setter splitting cannot touch it.** `split_css` is:

```c
while (starter_set.n > 1) {
  ... same_eq_classes(v->setters, av->setters) ...
```

cs 1021 has **2 creation points but 1 starter**, so `starter_set.n > 1` is
false on entry and the loop body never executes once. A creation point
becomes a *starter* only via `collect_setter_confluences`, which requires
`av->setters` to be non-empty -- i.e. requires something to have WRITTEN
THROUGH that container. The `[None]` literal is never written through: it
is consumed as `__mul__`'s operand and discarded, so it accumulates no
setters, is not a starter, and is invisible to the partition.

**That the starter count is the operative constraint, not the union, is
directly measured.** Replace both multiplications with plain literals and
the SAME merged CreationSet appears with the SAME union -- but with two
starters:

```
[scss] cs=1020 sym=list starters=2 defs=2  var[0]= int64#6 __pyc_None_type__#13 Task#1019
    STARTER var=id11249 es=51 nsetters=2
    STARTER var=id11283 es=52 nsetters=1
```

Two starters, the loop runs, the CS splits, and the program is clean. Same
defect, same union, different starter count, opposite outcome. (The
starter is not created by the element writes either: deleting
`b.data[0] = 7` or changing `a.taskTab[0]` leaves the count at 1.)

**The one-line statement.** The distinction between `[0]` and `[None]` is
made at CONSTRUCTION -- it lives in `cs->vars[0]`, written by `make_kind`'s
`flow_vars(atv, iv)`. `split_css` partitions by equivalence over
`av->setters`, which records only writes made THROUGH the container after
it exists. A container that is built with distinguishing contents and then
only READ has all of its evidence in the channel the partition does not
consult.

So setter splitting is the right SHAPE of mechanism -- demand-driven,
partitioning creation points by what is observably different about them.

**The "key on construction arguments" fix proposed here is WITHDRAWN**
(author, 2026-09-09: *"the constructor argument solution doesn't
generalize"*). It does not: a container built empty and filled later has no
distinguishing construction arguments at all, which is the majority case
and is exactly what `two_list_element_separation.py` covers. The split has
to come from the SETTERS. The section below re-does the root cause on that
basis and finds the real break.

Note what this does NOT need: no provenance, no call-site key, no fan. The
two creation points differ in the deduced TYPE of their construction
arguments -- `int64` against `__pyc_None_type__` -- which is exactly what
contour identity is allowed to key on.

## Why the setters do not reach the creation point (2026-09-09)

Re-done on the author's direction that the split must come from setters.
Measured with a temporary container-graph probe in `split_css`, on
`[0] * 4` against `[None] * 4`.

**The split, when it works, is driven by the literals' OWN construction
setters.** `make_kind` does `set_container(atv, container)` for each
positional argument, so a literal's construction store is itself a setter
whose container is that literal. Two literals then carry two DIFFERENT
setter AVars, `same_eq_classes` is false, and `split_css` separates them.
Measured on `[0] * 4` against `["s"] * 4`:

```
[chain] cs=1020 starters=2 defs=2 vars=1
   DEF av=2980 fun=__init__  setters=1   setter av=2982 container_av=2980
   DEF av=2994 fun=__init__  setters=1   setter av=2995 container_av=2994
```

Two starters, it splits at p=2, the program is clean. The failing case has
one side missing that setter entirely:

```
[chain] cs=1021 starters=1 defs=2 vars=1
   DEF av=3020 fun=__init__  setters=1   setter av=3022 container_av=3020
   DEF av=3033 fun=__init__  setters=0        <- no construction setter
```

**Three-cell matrix. The failure needs BOTH nil and the multiply:**

| `Buf.data` | `Area.taskTab` | literal CS | outcome |
| --- | --- | --- | --- |
| `[0]` | `[None]` | starters=2 | splits, clean |
| `[0] * 4` | `["s"] * 4` | starters=2 | splits, clean |
| `[0] * 4` | `[None] * 4` | **starters=1** | never splits, FAILS |

So the multiply alone is survivable and nil alone is survivable. What the
multiply does is remove the SECOND route to the evidence, so that when nil
independently costs the construction setter there is nothing left.

**What the multiply does, exactly.** `list.__mul__` is a
`__pyc_c_call__` to native `_CG_list_mult` whose FA model is
`__pyc_primitive__("merge", self, self)`, i.e. `P_prim_merge`:

```c
case P_prim_merge: {
  ...
  CreationSet *new_cs = creation_point(result, cs->sym);   // a FRESH CreationSet
  structural_assignment(new_cs, cs, p, es, true);          // copies CONTENTS only
  break;                                                   // no flow_vars(thing1, result)
}
case P_prim_merge_in: {
  ...
  flow_vars(thing1, result);                               // the sibling DOES connect it
}
```

`structural_assignment` wires the operand to the result by contents --
`flow_vars(cs->vars[i], tval)`, `set_container(tval, result)`,
`flow_vars(tval, get_element_avar(new_cs))` -- and `tval`'s container is
the NEW list. Nothing makes the result an alias of `self`, and nothing
should: `l * 4` genuinely is not `l`.

But `update_setter` propagates strictly along `av->backward` starting from
`x->container`:

```c
for (AVar *x : *dir) if (x && x->setter_class) update_setter(x->container, x, avs);
...
Ldone:
  av->setters = new_setters;
  for (AVar *x : av->backward) if (x) (void)update_setter(x, s, avs);
```

That is a walk over the CONTAINER graph, and `P_prim_merge` leaves no
backward container edge across itself. Measured: every `__mul__` result def
has `back_closure=1` -- an empty backward set. So the `__setitem__` on the
multiplied list DOES register on the result (`setter av=3113 in
fun=__setitem__ container_av=3102`), walks backward, reaches the merge's
creation point, and stops. The literal is connected only along the VALUE
chain, which `update_setter` never traverses.

**Stated in one line:** *types flow forward through the merge; setter
attribution does not flow backward through it.* The merge is transparent to
the value graph and opaque to the container graph, so the pollution reaches
the result while the evidence that would separate the operands stays
trapped behind it.

**The fix follows the shape of [146](146-remove-all-arbitrary-splitting.md)
B**, which taught `build_cs_flow_graph`'s backward walk to CROSS a folded
global load rather than changing the type flow. The same applies here: do
not add `flow_vars(thing1, result)` to `P_prim_merge` -- that would be
wrong, the result is a new list -- but teach the setter backward walk to
cross a merge, using the operand relationship `structural_assignment`
already knows. A setter on `new_cs` is attributable to the creation points
of `cs`, because `structural_assignment` recorded that `cs`'s contents flow
into `new_cs`'s.

**The nil half is now traced -- see the next section.** It is a second,
independent defect in the same transfer function, and it is the one that
actually decides this repro.

## Yes -- the transfer function is wrong, in TWO places (2026-09-09)

Author's question: *"so the problem is the transfer function isn't
correct?"* Yes, and tracing it that way found the second half. There are
two independent defects in how the constraint functions build the SETTER
relation, and the failure needs both.

### Defect 1 -- `P_prim_merge` is opaque to the container graph

`structural_assignment` wires the operand into the result by CONTENTS
(`flow_vars(cs->vars[i], tval)`, then `flow_vars(tval, elem(new_cs))`) but
`set_container(tval, result)` points the container relation only at the NEW
list. `update_setter` walks `av->backward` starting from `x->container` --
the CONTAINER graph -- so it terminates at the merge. Measured:
`back_closure=1` on every `__mul__` result def. A `__setitem__` on the
multiplied list can therefore never be attributed to the literal it was
built from.

### Defect 2 -- a `None` store is not counted as a store

`compute_setters`:

```c
if (akind == AKIND_TYPE && !x->out->type->n) continue;
```

An empty-typed AVar never receives a `setter_class`, and `update_setter` is
gated on `setter_class`, so it never becomes a setter. **The store of
`None` into a fresh list presents an EMPTY AType**, and is dropped by this
gate. Measured directly:

```
[skip] EMPTY-TYPE gate drops x=3034 (container=3033) as setter of cs=1021 slot av=3021
       out=0x... n=0 var=(anon)   x->backward=1 x->forward=1
       slot av=3021 type: int64#6 __pyc_None_type__#13
```

`container=3033` is exactly the `[None]` creation point that measured
`setters=0`. Note the contradiction the two lines make: the store's own
AType is EMPTY, while the slot it writes into carries
`__pyc_None_type__#13`. The nil reaches the slot; the store that put it
there does not exist as far as the setter lattice is concerned.

### Why both are needed

| | route to attribution | present? |
| --- | --- | --- |
| plain `[None]` | the program writes into the literal directly (`a.taskTab[0] = Task(3)`, non-nil, survives the gate) | yes -> splits |
| `["s"] * 4` | the literal's own construction store, non-nil, survives the gate | yes -> splits |
| `[None] * 4` | construction store dropped by defect 2; later write walled off by defect 1 | **none** -> no split |

Objects behave like strings: `[0] * 4` against `[Task(0)] * 4` is clean.
It is nil specifically.

### Measured candidate fix

Letting an empty-typed AVar that HAS a container through the gate:

```c
if (akind == AKIND_TYPE && !x->out->type->n && !x->container) continue;
```

(prototyped behind `PYC_NILSETTER`, then reverted -- not landed)

| | as shipped | gate lifted |
| --- | --- | --- |
| the 25-line repro under `PYC_CSDCPA1=2` | 1 diagnostic | **0** |
| `richards` under `PYC_CSDCPA1=2` | compile w=7, **run=139 (SIGSEGV)** | compile w=4, **run=0**, stdout identical to CPython but the `TIME` line |
| pyc suite at the default | 313 passed / 0 failed | **313 passed / 0 failed** (identical) |
| `bh`, `sudoku5` under the flag | unchanged | unchanged |

So defect 2 alone is the whole of `richards`, is suite-neutral at the
default, and takes the flag arm's blockers from three to two. `bh`
(`__slots__` string lists) and `sudoku5` (comprehensions/append) reach the
shared CreationSet by other routes and are untouched by it.

**Not landed** -- a corpus `check` sweep on both arms is owed first, per
this repo's rule for any splitter change. But the "why is it empty"
question is answered below, and it replaces the `!x->container` guard above
with a better one.

## Why a `None` store has an empty AType -- and the right fix (2026-09-09)

It is not an accident and not a bug in isolation: `->type` is a
PROJECTION, documented at fa.h:105 as *"not including values
(constants)"*, and `make_AType` computes it as

```c
  // compute "type" (without constants)
  if (nonconsts.n) { ... } else
    tt->type = fa->type_world.bottom_type;      // <- pure-nil lands here
```

`nil` is a `is_unique_type` unique OBJECT, so it is a VALUE, not a
non-constant type. A lone `{None}` therefore has `nonconsts.n == 0` and its
projection is bottom -- `->type->n == 0` -- while the raw `out` still holds
the nil CreationSet.

**This also explains the contradiction the probe showed.** issue/060 put a
carve-out in `make_AType`:

```c
  if (nil_cs) {
    bool has_scalar = false;
    for (CreationSet *c : nonconsts)
      if (c && c->sym->type && c->sym->type->num_kind) { has_scalar = true; break; }
    if (has_scalar) nonconsts.set_add(nil_cs);   // KEEP nil in ->type
    else            nulls = 1;                   // strip it
  }
```

So nil survives the projection only when the same AType also carries a
numeric scalar. In the repro the *slot* holds `{int64, nil}` -- has a
scalar, nil KEPT, which is why `av=3021` printed
`int64#6 __pyc_None_type__#13`. The *store* holds a lone `{nil}` -- no
scalar, nil STRIPPED, `n=0`. Same value, two ATypes, opposite answers.

**And this exact trap is already known here.** ifa/124 hit it in the ES
splitter and its note sits a few lines above `split_type_view`:

> `->type` strips a pure-nil AType to bottom (make_AType's `is_unique_type`
> branch; the 060 carve-out that KEEPS nil only fires when the same AType
> also carries a num_kind scalar, which a lone `{None}` does not). The
> partitioner below guards every comparison with `->n &&`, so an edge
> passing only None reads as **"nothing known yet" and is compatible with
> everything**.

That is the same sentence as defect 2, in a different consumer. ifa/124's
remedy was `split_type_view`, which gives the SPLITTER a view separating
*not analyzed* (raw empty too) from *carries only nil* (raw non-empty),
deliberately without touching the `->type` projection every other consumer
reads -- narrowing, defaulted params and the recursion-separability gate
all need nil transparent there, and `is_not_none_narrow` / `minmax_3arg` /
`expr_evaluator` regress if `->type` itself is changed.

**So defect 2 is ifa/124's bug, at a site ifa/124 did not fix**, and the
right patch is its remedy applied here rather than the `!x->container`
special case:

```c
// compute_setters
if (akind == AKIND_TYPE && !x->out->type->n && !x->out->n) continue;
```

Skip only when the RAW type is also empty -- i.e. genuinely not analyzed.
A store carrying only `None` is a real store and keeps its `setter_class`.

Measured, identical outcome to the `!x->container` prototype and better
grounded:

| | as shipped | `!x->out->n` |
| --- | --- | --- |
| the 25-line repro under `PYC_CSDCPA1=2` | 1 diagnostic | **0** |
| `richards` under `PYC_CSDCPA1=2` | w=7, **run=139 (SIGSEGV)** | w=4, **run=0** |
| pyc suite at the default | 313 passed / 0 failed | **313 passed / 0 failed** |
| `bh`, `sudoku5` under the flag | unchanged | unchanged |

Both prototypes reverted; `fa.cc` is unchanged in the tree.

The audit of the other `->type->n` guards is below.

## Audit: every other bare `->type->n` guard (2026-09-09)

`split_type_view` exists because a bare `->type->n` test conflates *not
analyzed* with *carries only None*, and it was applied at exactly one site.
This is the sweep for siblings. 34 textual `->type->n` matches; most are
`Sym::type` (a different field). Nine are the AType projection on an
`AVar::out`:

| site | function | role | verdict |
| --- | --- | --- | --- |
| fa.cc:7758 | `compute_setters` | setter attribution | **LOAD-BEARING** — this is defect 2 |
| fa.cc:5552, 5559 | `collect_type_confluence` | the stage-1 TYPE_CONFLUENCE collector | same conflation, **measured inert** |
| fa.cc:1498 / 1552 | `edge_type_compatible_with_edge` / `_with_entry_set`, RETURN comparison | ES compatibility | same conflation, **measured inert** |
| fa.cc:7898 | `cs_group_signature` | ledger identity | nil setter zeroes the signature, disabling ledger routing → re-mint churn rather than a missed split. Untested. |
| fa.cc:9334 | `result_is_different` | violation-imprecision collection | conflates unanalyzed with None-only (both project to bottom, compare equal). Low. |
| clone.cc:832 | clone donor selection | layout | skips a None-only member as a type donor. Low. |
| fa.cc:6684, 6709 | `IFA_DBG_TUPARITY` / `IFA_DBG_TUPHOMO` | diagnostics only | not load-bearing |

**The most interesting entry is 1498/1552, for what it says about how this
bug spreads.** Those are the RETURN comparisons in the two functions whose
ARGUMENT comparison ifa/124 already fixed — line 1544 reads
`if (etype->n && stype->n && etype != stype)` on `split_type_view` output,
and eight lines later the return comparison reads the same shape on raw
`->out->type`. The fix was applied to one half of a function and not the
other.

**Measured, all three prototyped behind env flags and then reverted:**

| | pyc suite (default) | contours, 6 programs (default) | `richards` under `PYC_CSDCPA1=2` |
| --- | --- | --- | --- |
| shipped | 313 / 0 failed | baseline | w=7, **run=139 (SIGSEGV)** |
| `compute_setters` fix | 313 / 0 | identical | w=4, **run=0** |
| `collect_type_confluence` fix | 313 / 0 | identical | unchanged |
| return-comparison fix | 313 / 0 | identical | unchanged |
| all three | 313 / 0 | — | w=4, run=0 |

Contours checked on `richards`, `sudoku1`, `sieve`, `nbody`, `chess`,
`dijkstra`: `ess`, `css` and `container_cs` are byte-identical for all
three fixes. None of them moves `bh` or `sudoku5`.

**Conclusion.** The conflation is systemic -- six live sites, one of them a
half-fixed function pair -- but only ONE is currently load-bearing, and it
is the one already identified. The other two high-suspicion sites are real
by construction and inert by measurement, so they are cleanup to land WITH
a corpus sweep, not fixes to claim a win for. The honest summary is that
this audit narrowed the work rather than expanding it: `compute_setters` is
the fix, and it is worth landing on its own.

Nothing from this audit is in the tree; `fa.cc` is unchanged.

## LANDED 2026-09-09 -- the `compute_setters` fix, with its corpus sweeps

```c
// compute_setters
if (akind == AKIND_TYPE && !x->out->type->n && !(nilstore_enabled() && x->out->n)) continue;
```

Default ON; `PYC_NILSTORE=0` restores the previous behaviour so a corpus
change can be attributed to this clause rather than guessed at.

**Six gates green** on the build that was swept: `make test` 313 passed /
0 failed, `test_pyc.py -b` 313 / 0, `ifa test_llvm`, `test_dparse`,
`test_links` (1774 links, 0 dangling).

**Default arm: all 77 rows byte-identical.** Two full `check` sweeps from
ONE binary, the arms separated only by `PYC_NILSTORE`:

| | compile_fail | run_fail | stdout_differs | warnings | container CS / shapes |
| --- | --- | --- | --- | --- | --- |
| `PYC_NILSTORE=0` | 2 | 38 | 24 | 43 | 2740 / 625 = 4.38 |
| fix ON | 2 | 38 | 24 | 43 | 2740 / 625 = 4.38 |

`diff` of the two `.tsv` bodies is empty. So the shipped compiler is
unchanged by this, which is what makes it safe to land ahead of the flag.

**Flag arm (`PYC_CSDCPA1=2 PYC_CSLADDER=3`): exactly 3 of 77 programs are
affected**, and that is established rather than assumed -- the per-program
`DEMAND` line (`ess css container_cs shapes pshapes`) is byte-identical for
the other 74, so their analysis, generated code and run status cannot move.

| program | baseline | with the fix | reading |
| --- | --- | --- | --- |
| `richards` | compile 0, w=7, **run 139 (SIGSEGV)** | compile 0, w=4, **run 0**, stdout identical to CPython but the `TIME` line | **fixed** |
| `pygasus` | compile 0, **w=53**, run 134 | compile 0, **w=3**, run 134 | diagnostics much better, verdict unchanged |
| `chull` | compile 0, w=0, **run 139 (SIGSEGV)** | **compile 1**, `object layout: 'Edge' is blind-cast to 'Vertex' ... member width differs (_CG_bool vs _CG_void)` | failure MODE improved |

`chull` is the one that looks like a regression and is not. It segfaults at
runtime on EVERY arm today -- default included -- while compiling with ZERO
warnings, which is precisely the case
[ifa/102](102-corpus-programs-compile-then-abort-at-runtime.md) exists to
name. The fix does not break it; it makes ifa/123's layout contract SEE it,
turning a silently-shipped crashing binary into a compile-time diagnostic
that names the two classes and the member. Nothing that worked is lost, and
there is now a real latent layout bug on the record.

Contours, flag arm: container CS 2080 -> 2091 (+11, +0.5%), all of it in
those three programs.

**Measurement caveat, recorded because it shaped the method.** This machine
is a shared host and its other services keep it near the free-memory floor;
the sweeps were repeatedly killed mid-run. `-j 4 -J 1` is what completes.
Three of the four sweeps ran to a `.tsv`; the fourth (flag arm, fix ON) had
its compile phase complete 77/77 four separate times but never finished the
run phase, so the flag-arm run column above comes from the completed
compile phase plus the `DEMAND` equality argument plus running the three
changed programs by hand. That is weaker than a `.tsv` and is flagged as
such -- but the 74-program equality is exact, not a sample.

Committed `.tsv`s: `check__PYC_NILSTORE_0__ae16c44e+0e9deefa`,
`check__default__ae16c44e+0e9deefa`,
`check__PYC_CSDCPA1_2_PYC_CSLADDER_3_PYC_NILSTORE_0__ae16c44e+0e9deefa`.

**Flag-arm blockers: three -> two.** `bh` (ifa/143, `__slots__` string
lists) and `sudoku5` (comprehensions/append) are untouched by this and
remain.

## `sudoku5` dug into: it is NOT `bh`'s problem (2026-09-09)

The two remaining flag-arm blockers were being carried as one family. They
are not. Measured on the post-`nilstore` build under `PYC_CSDCPA1=2`.

**Only two stages ever run.** `PYC_DBG_STAGEDELTA` shows `TYPE_CONFL`
returning 1 on EVERY pass, with 445-1136 confluences, plus
`CS_DEF_PARTITION` (which runs unconditionally). Everything gated on
`!analyze_again` -- SETTER, MARK_SETTER, VIOLATION, PER_CS_RECEIVER,
CSM_ELEMENT_CS -- is starved. `split_css` never executes even once:
`IFA_DBG_STARTERS` prints **zero** `[sfs]` lines for the whole run, so the
setter-side CreationSet partition that fixed `richards` is not merely
declining here, it is unreachable.

**The analysis oscillates violently rather than converging:**

```
p=0  viol=644   p=14 viol=1591   p=22 viol=413   p=28 viol=2042
p=8  viol=488   p=18 viol=478    p=26 viol=438   p=32 viol=1364
                                                 p=35 viol=1404  STOP
```

The spikes track large EntrySet batches (`p=28 ess+74`, `p=32 ess+83`).

**It stops on CHURN, and on a spike.**

```
STALL LIMIT reached at pass 35, 1404 violations (best 398):
  8 re-deriving (limit 8), 8 non-improving (limit 32); stopping
```

722 re-derivations (`IFA_DBG_INCOMPAT`), dominated by SHARED CONTAINER
METHODS -- `__getitem__` 186, `__eq__` 165, `len` 87, `__lt__` 52,
`__pyc_to_bool__` 47, `__ge__` 36, `__len__` 28, `__pyc_more__` 22 -- which
is [143](143-shared-container-method-contours-refuse-cs-splits.md)'s
subject arriving as a stability problem rather than a precision one.

**`bh` is a different animal.** Same probes, same arm:

| | re-derivations | stalls on | final violations | best |
| --- | --- | --- | --- | --- |
| `sudoku5` | **722** | re-deriving (8 >= 8) | **1404** | **398** |
| `bh` | 10 | non-improving (32 >= 32) | 10 | 9 |

`bh` converges to ~9 violations and cannot resolve the last few: a
precision wall, and 143 already has it root-caused. `sudoku5` never
converges at all. Two blockers, two mechanisms.

**A third of `sudoku5`'s errors are manufactured after its best pass.**
The analysis tracks `best_violations` and never uses it to choose the final
state -- it reports whatever the LAST pass produced. Stopping earlier is
measurably better:

| `IFA_STALL_LIMIT` | passes | ess | warnings | errors |
| --- | --- | --- | --- | --- |
| 8 (default) | 37 | 1104 | 249 | **364** |
| 4 | 19 | 708 | 85 | **180** |
| 2 | 5 | 407 | 65 | 289 |
| 1 | 4 | 370 | 65 | 287 |

`IFA_STALL_LIMIT=4` halves the errors and cuts contours 36%, on the same
binary and the same program. So the passes that run after the good region
actively destroy the result.

**Two separable pieces of work, in priority order.**

1. ~~*The analysis should not end on a spike.*~~ **WITHDRAWN** (author,
   2026-09-09: *"'end at best pass' is the wrong direction. analysis should
   quiesce when it cannot make progress."*) Ending at a best-so-far
   snapshot treats a symptom and enshrines the oscillation as acceptable.
   The analysis has to actually converge. See "The decision table" below
   for the direction that replaces this.

2. *The oscillation itself* -- big TYPE_CONFLUENCE batches on shared
   container methods, re-derived every pass. This is 143's mechanism, and
   it is why `sudoku5` should be worked WITH 143 rather than as its own
   item -- but note that fixing `bh`'s precision wall will not by itself
   stop `sudoku5` churning, and vice versa.

Nothing landed from this; the probes were temporary and are reverted.

## How shedskin avoids the oscillation (read from source, 2026-09-09)

Author's question, on `sudoku5`'s 722 re-derivations: *these programs are
typed by shedskin, how does it prevent the oscillation?* Answer: **it makes
the oscillation structurally impossible, by REBUILDING the contour set
every iteration instead of patching it.** Read from
`shedskin/infer.py:1800` (`iterative_dataflow_analysis`).

```python
backup = backup_network(gx)            # snapshot the network ONCE, before iteration 1
while True:
    propagate(gx)                      # CPA to fixpoint
    split = ifa(gx)                    # detect conflicts, decide splits
    if not split: return               # done -- the ONLY normal exit
    for cl, dcpa, nodes, newnr in split:
        gx.alloc_info[parent.ident, cart, n.thing] = (cl, newnr)   # record the DECISION
    restore_network(gx, backup)        # put the graph back to pristine
```

`restore_network` (`infer.py:2090`) is far more than a type reset. It
restores `gx.types`, `gx.constraints` (the constraint SET), `gx.cnode` (the
node table) and every node's `in_`/`out` EDGES -- and then:

```python
    for func in gx.allfuncs:
        func.cp = {}                   # EVERY function contour destroyed
```

So iteration N runs against the ORIGINAL program plus the accumulated
decision table `gx.alloc_info`, keyed on
`(function identity, cartesian argument tuple, allocation node)`. Contours
are a PURE FUNCTION of that decision set. Re-deriving the same decision
reproduces the identical contour by construction; there is no accumulated
graph for a split to drift against, so "re-derivation churn" is not a
failure mode that can exist.

Two supporting details. Allocation nodes inside functions have their types
cleared on restore (`beforetypes[node] = set()` for `List`/`Dict`/`Tuple`/
`ListComp`/`Call` under a function), so creation points are re-seeded per
contour from `alloc_info` rather than carried over. And when shedskin runs
out of budget it RESTARTS cleanly rather than stopping mid-flight: on a
no-split iteration that was `cpa_limited`, it doubles `cpa_limit` and sets
`iterations = 0`; `MAXITERS` is a hard cap that it will hit three times
(`maxhits == 3`) before giving up.

**pyc does the opposite, deliberately.** `analyze_to_convergence` resets
derived VALUES but not STRUCTURE -- `clear_results()` iterates
`fa->all_entry_sets` / `fa->all_creation_sets` and clears each one, so the
contour OBJECTS survive every pass and their identity is carried forward.
Splits are applied as edits to that persistent graph. To make a re-derived
split land back on the same contour, pyc runs a whole compensating
subsystem: `SplitDecision`, `ledger_find` / `ledger_add`,
`cs_group_signature`, `fa_pass_retargeted`, `dup_split_attempts` -- and
`rederive_churn`, which is **literally the count of times that compensation
failed**.

That is the connection to the measurement above. `sudoku5`'s 722
re-derivations are 722 occasions on which the ledger could not re-find a
contour and minted a duplicate instead; each duplicate changes the graph,
which changes the next pass's confluences, which is the oscillation. On
`bh` the same counter reads 10, which is why `bh` sits still at 9-10
violations while `sudoku5` swings between 413 and 2042.

**The honest framing of the trade.** pyc's persistence is a performance
choice -- rebuilding every pass is what
[111](111-FA-selective-invalidation-per-pass.md) exists to AVOID, and
CLAUDE.md already records that 111 is "a performance lever for the extra
passes, not a precondition". shedskin pays the rebuild unconditionally and
buys determinism with it. So this is not "shedskin has an algorithm pyc
lacks"; it is pyc having optimised away the property that makes
re-derivation trivially stable, and then trying to recover that property
with bookkeeping. `sudoku5` is where the bookkeeping loses.

**What this implies for the two open items above.** Ending at the best pass
rather than the last (item 1) is a patch on the symptom -- worth having,
since `IFA_STALL_LIMIT=4` halves `sudoku5`'s errors, but it does not touch
the cause. The cause-level options are to make re-derivation actually
stable (finish what the ledger was for) or to adopt shedskin's posture and
rebuild contours from the decision table each pass. The second is a large
change and would collide with 111; it should not be started without
measuring what fraction of `rederive_churn` the ledger could close on its
own.

## The decision table -- what `sudoku5` actually needs (author, 2026-09-09)

> *"fa should record the decisions like shedskin. that is what the call
> site to es mapping should be along with the creation point to CS mapping.
> on a split we should prepopulate or store in a side table the es an es
> was split from so we can keep the same mappings for the new call points"*

Measured against the code, this is right, and the measurements below say
why the present design cannot converge on `sudoku5`.

### First, a correction to the section above

I wrote that `sudoku5`'s 722 re-derivations were "722 minted duplicates".
That is **wrong**. Splitting `REDERIVE` by kind:

| | count | what it is |
| --- | --- | --- |
| `ROUTE` | **596** | the ledger successfully routed a re-derived group to its recorded product |
| `GROUP` | **126** | minted a product the ledger already named -- the real miss |

82% of it is the ledger WORKING. And per-edge, call sites are not
flip-flopping either -- of 2080 edges rebound across the run, 1398 rebind
once, 35 always to the same target, **627 monotonically to ever-new
targets**, and only **20 edges / 24 events** ever return to a target they
previously had. So the failure is not ping-pong. It is **unbounded
subdivision**.

### Why it subdivides for ever: the ledger key is derived from types

`apply_entry_set_split` keys its decision on
`(fun, stage, avpos, part, gsig)`, where `part` is the group's type
partition and `gsig` is a signature over it -- both computed from THIS
pass's mid-pass types. Measured on `sudoku5`:

| | |
| --- | --- |
| distinct `(fun, es)` contours that mint groups | 415 |
| **of those, seeing more than one distinct `gsig`** | **221** |
| most distinct signatures on a single contour | **22** |
| distinct `(fun, es, gsig)` triples | 1080 |

and the worst offenders are exactly the shared container methods:
`append` 22, `__setitem__` 22, `__getitem__` 16, `__eq__` 12, `__lt__` 10,
`len` 10.

That is the loop, stated plainly: a shared container method's callers are
still moving, so its type partition differs every pass, so every pass's
group is a NEW ledger key, so the lookup misses and a fresh contour is
minted, which changes the types, which changes the partition. **The
decision is keyed on the very thing the decision is supposed to stabilise.**

### The gap the author names, confirmed in the code

| mapping | pyc today |
| --- | --- |
| creation point -> CreationSet | `av->cs_map`, persists across passes -- **exists** |
| CreationSet split lineage | `CreationSet::split_origin`, durable -- **exists** |
| call site -> EntrySet | `AEdge::to`, and `clear_edge` does NOT clear it -- **exists, but destroyed at every split** |
| EntrySet split lineage | `EntrySet::split` -- **transient**, `clear_splits()` zeroes it every pass |

So the CS side already has the durable pair and the ES side does not. At a
split, `apply_entry_set_split` does

```c
for (AEdge *x : these_edges) { x->to = 0; ... }        // destroy the binding
for (AEdge *x : these_edges) { make_entry_set(x, ...); } // re-bind BY SEARCH
```

and `make_entry_set` resolves by SCORING `fun->ess`
(`find_best_entry_sets`, `entry_set_compatibility`,
`edge_type_identical_to_entry_set`, five `HARDREUSE` modes). shedskin does
a dictionary lookup, `func.cp[dcpa][cart]`. `EntrySet::canon_key`'s own
comment already states the target -- *"the type tuple NAMES the contour --
shedskin's model -- and a routing decision becomes a lookup with no
symmetric choice to alternate between"* -- and `canon` reads 0 on
`sudoku5`, so it is not doing that today.

### The change

1. Give `EntrySet` a durable `split_origin`, the exact analogue of
   `CreationSet::split_origin`. `product->split = es` already records the
   parent; it is simply thrown away every pass by `clear_splits()`.
2. Make the decision table **call-site keyed**, not type-keyed: record, per
   call site (the `AEdge`, i.e. `(from ES, pnode)`), which product it was
   routed to. A call site is a fixed source-level identity and cannot
   drift, so the 221 contours that currently present up to 22 signatures
   each present exactly one key.
3. At a split, prepopulate the new product's call-site entries from the
   parent's, rather than detaching every edge and re-searching. Re-deriving
   the same split then re-finds the same product by construction --
   shedskin's property -- and `make_entry_set`'s scoring becomes the
   fallback for genuinely new call sites only.

This subsumes the ledger rather than adding to it: `SplitDecision`,
`cs_group_signature`, the `HARDREUSE` modes, `route_adj` / `route_last` and
the cycle-pinning at `routecycle_enabled() >= 3` all exist to make a
type-derived key behave like a stable one. A call-site key is stable to
begin with.

**Sequencing note.** Do not start this by deleting the scoring path. The
measurement to take first is how many of the 3128 mint-path groups on
`sudoku5` have a call-site set identical to one already recorded -- that is
the fraction a call-site key would collapse, and it is cheap to instrument
now that `[churn-mint]` carries the edge id.

## Implementing the decision table: what the measurements said (2026-09-09)

Went straight at the three steps. The result is mostly a NEGATIVE finding,
and it relocates the work rather than completing it.

### Step 2 (call-site keyed ES table) -- built, measured, REMOVED

Added `Map<AEdge *, EntrySet *> edge_home`, recorded on both the ledger
route and the mint, consulted before the search in
`apply_entry_set_split`. On `sudoku5` it looked like a win -- errors
364 -> 179, `ess` 1104 -> 745, passes 37 -> 22, suite unchanged.

**It is not a win, and the number is not the mechanism.** The consult fired
**3 times** in the whole run (`[churn-decide]`). Three routing decisions out
of ~3000 moved the error count by 51%. So the improvement is a chaotic
perturbation of an unstable analysis, not the table working, and shipping
it would have banked luck as if it were a fix. Removed.

**Why it cannot work as designed:** a call-site key answers "where did this
edge go last time", and after a split the edge LEAVES `es` and never comes
back -- measured earlier, only 20 edges / 24 events ever return to a prior
target, against 627 that move monotonically to ever-new contours. The
failure is subdivision, not re-binding, so a per-edge memo has almost
nothing to answer.

### Step 1/3 (durable ES split lineage) -- built, measured INERT

`EntrySet::split_origin` added as the durable analogue of
`CreationSet::split_origin`, and the split-parent route in
`creation_point` now walks it instead of reading only the transient
`es->split`. This is exactly "keep the same mappings for the new call
points", and the diagnosis behind it was right -- `clear_splits()` does
wipe `es->split` every pass.

Measured on `sudoku5`: **byte-identical**, `PYC_ESLINEAGE=0` vs `1`
(364 errors, ess 1104, css 2634, 37 passes, both). Kept, because it is
correct and costs nothing, but it buys nothing today and is documented as
such rather than claimed.

### Why it is inert: the CS-side decision table ALREADY EXISTS

New probe `IFA_DBG_CSROUTES` counts every `creation_point` route.
`sudoku5`, flag arm:

```
cs_map=734269  dcpa1=1226  split_parent=6  cselem=0  csshape=0  csmold=0  MINT=2328
```

`v->cs_map` -- the creation point -> CreationSet mapping -- serves **734k**
lookups, and **nothing in the file ever clears it** (`grep` for
`cs_map = 0` / `cs_map->clear`: no matches). It is already durable, already
authoritative, and already what the author asked the CS half to be. The
split-parent route fires **6 times**, so making it survive the pass changes
nothing: by the time a later pass revisits a product's creation point, its
own `cs_map` already answers.

**So on the CS side pyc already has shedskin's property.** The half that
does not is the ES side.

### Where the ES-side decision table actually is: `PYC_CANON`, and it is OFF

`EntrySet::canon_key` already states the goal in its own comment -- *"the
type tuple NAMES the contour -- shedskin's model -- and a routing decision
becomes a lookup with no symmetric choice to alternate between"* -- which is
`func.cp[dcpa][cart]`. `canon_enabled()` defaults to **0**.

Turned on, on `sudoku5` (flag arm):

| | passes | ess | css | warnings | errors |
| --- | --- | --- | --- | --- | --- |
| `PYC_CANON=0` (default) | 37 | 1104 | 2634 | 249 | **364** |
| `PYC_CANON=1` | 21 | 690 | 1792 | 82 | **177** |
| `PYC_CANON=2` | 24 | 697 | 2018 | 118 | 297 |

and it is measurably firing, not coincidence: `CANON p=19 hit=3 miss=6
conflict=3 conflict_honored=3`, with 52 `CANON-CONFLICT` lines over the
run. A conflict is the canonical home for an edge's types being the very
contour the splitter is detaching it from -- "the split is asking for a
separation the type tuple says does not exist". Mode 1 honours the split
anyway and beats mode 2, which refuses it.

pyc suite is 313 passed / 0 failed with `PYC_CANON=1`.

### What this means for the direction

The author's design is right and is **half-built already**: the creation
point -> CS decision is durable and doing the work; the call site -> ES
decision exists as `canon_key` and is switched off. The missing piece is
not a new table but making the ES-side one authoritative -- which is the
same statement as "the ledger is a hack that failed", since `SplitDecision`
/ `cs_group_signature` / `HARDREUSE` / `route_adj` all exist to make a
type-partition key behave like the canonical key would.

**Do not read `sudoku5`'s 364 -> 177 as the size of the win.** That program
is chaotically sensitive -- three decisions moved it 51% -- so the only
honest evaluation of `PYC_CANON=1` is a corpus `check` sweep on both arms.
That is the next step, and it is a knob flip plus a sweep rather than new
machinery.

## Prepopulation: NOT tried yet, and it IS the plan (2026-09-09)

Author: *"what about the idea: on a split we should prepopulate or store in
a side table the es an es was split from so we can keep the same mappings
for the new call points? did you try that?"*

**No.** What I built was the LAZY form -- consult the parent at lookup time
-- and the two are not equivalent, because of route order in
`creation_point`:

```c
cs = v->cs_map->get(s);  if (cs) goto Lfound;   // checked FIRST (734k hits)
if (csdcpa1_enabled() ...) { dcpa1 }            // second
if (es && es->split)      { split_parent }      // THIRD  <- the lazy form
... MINT
```

Prepopulation writes the parent's binding **into `cs_map`**, which is
consulted first, so it preempts `dcpa1` and `MINT`. The lazy form runs
third and only gets asked after another route has already answered. That
distinction is the whole point of the author's phrasing and I collapsed it.

### There is a large, measured target

`IFA_DBG_CSROUTES`, DEFAULT arm (where `dcpa1` is off, and which is the arm
CLAUDE.md's "an ES split MULTIPLIES CreationSets" complaint is about):

| program | `cs_map` | `split_parent` | `MINT` | mints inside a SPLIT-CHILD contour | of those, parent had NO binding | of those, `clone_methods_per_cs` |
| --- | --- | --- | --- | --- | --- | --- |
| chess | 545063 | 896 | 3469 | **2302** | **0** | 14 |
| sudoku1 | 30820 | 142 | 676 | 152 | **0** | 0 |
| richards | 14615 | 51 | 393 | 32 | **0** | 0 |

So on `chess`, **2288 CreationSets are minted inside a contour whose parent
already holds a binding for that very sym**, with issue/045's
`clone_methods_per_cs` exclusion accounting for only 14. That is the
multiplication, quantified, and it is what prepopulation would collapse.

### Why the lazy form did not collect them -- open

Instrumented: `esl_reached=896`, `esl_decline=0` on `chess`. The route is
reached exactly as often as it hits and **never declines**. So those 2302
mints never reach it: `es->split` and `es->split_origin` are both null when
the route runs, and non-null a few lines later when the mint probe fires,
inside the SAME `creation_point` call. Something between the two sets the
parent. Not yet found, and it has to be found first -- prepopulating at
split time is pointless if the contour does not yet know its parent at the
moment its creation points are resolved.

### Landed on the way: a real type error

`creation_point` did `EntrySet *es = (EntrySet *)v->contour;` with no
guard, while `Lfound` twelve lines below carefully tests
`v->contour_is_entry_set` before the same cast. So on a CS-contoured AVar
every `es->...` read below was a `CreationSet` read through an `EntrySet`
pointer -- latent, and made reachable by the split-parent route now reading
a second field. Guarded. Corpus route counts are byte-identical, so nothing
depended on the old behaviour; six gates green.

### The plan, restated

1. Find why a split product's creation points are resolved before
   `split_origin` is set on it. **This is the blocker.**
2. Then prepopulate at split time: seed the product's `cs_map` from the
   parent's, so the FIRST route answers and `dcpa1`/`MINT` never see it.
   Note `Lfound` already writes every resolution into `cs_map`, so the
   caching half exists -- what is missing is doing it eagerly, at the split,
   for the whole contour.
3. `clone_methods_per_cs` must stay excluded (issue 045), which the
   measurement says costs 14 of 2302 on `chess`.

## Both halves of the invariant already hold -- measured (2026-09-09)

Author's spec: *"the first pass should be one es per function and after a
split there should be zero mints. feel free to just add an assert on any
mint."* Did that. Both hold, so the CreationSet multiplication is not where
we were looking.

### "After a split, zero mints" -- true, modulo two deliberate policies

`PYC_ASSERT_MINT=1` reports every `creation_point` MINT taken inside a
contour that has a parent, with the sym. Broken down:

| program | mints in a split-child contour | `closure` | `clone_methods_per_cs` (`__list_iter__`, `range`) | anything else |
| --- | --- | --- | --- | --- |
| richards | 32 | 32 | 0 | **0** |
| sudoku1 | 152 | 152 | 0 | **0** |
| chess | 2302 | 2288 | 14 | **0** |

Both exclusions are by design and correct. Closures take
`if (s == sym_closure) goto Lunique;` at the very top of `creation_point`,
bypassing every reuse route -- and they must, because a closure CS binds
that contour's captured AVars, so a product's closure genuinely is not the
parent's. `clone_methods_per_cs` is issue/045's requirement.

**So there is not a single illegitimate mint after a split.** The
inheritance the author described is already complete for everything it
should cover, which is why making it durable (`split_origin`, below)
measured as a no-op: `es->split` is still set during the pass the split
happens, the route fires then, and `Lfound` writes the answer into
`v->cs_map` -- which is never cleared, so later passes hit the fast path
and never need the lineage.

This also retires my own earlier framing. I read `mint_in_child=2302` on
chess as "2288 CreationSets minted where inheritance was available". It is
not: those are closures, and inheritance was not available to them by
design.

### "First pass, one ES per function" -- true

New probe `IFA_DBG_ESPERFUN`, reported at the top of every pass, before
splitting:

| | pass 0 | funs with >1 ES | worst |
| --- | --- | --- | --- |
| richards | ess=162, funs=158 | **2** | 3 (`__new__`) |
| sudoku5 (flag arm) | ess=215, funs=203 | **5** | 4 (`__init__`) |

Essentially one contour per function, as intended.

### Where it actually goes wrong: the ES side, after pass 0

Same probe, following `sudoku5` on the flag arm:

```
pass=0  ess=215  funs=203  funs_with_multiple=5   max=4   worst=__init__
pass=1  ess=303  funs=188  funs_with_multiple=40  max=16  worst=append
pass=2  ess=339  funs=188  funs_with_multiple=44  max=17  worst=__getitem__
pass=3  ess=370  funs=188  funs_with_multiple=49  max=20  worst=__getitem__
                                    ... to ess=1104
```

One pass takes `append` from 1 contour to 16 and `__getitem__` to 20. That
is the whole of the growth, it is EntrySet-side, and it lands on exactly
the shared container methods whose group signatures were measured drifting
(221 of 415 minting contours present more than one `gsig`, one presents
22).

### Consolidated: the decision table is needed in exactly one place

- creation point -> CreationSet: `v->cs_map`, 734k hits, never cleared.
  **Done.**
- split lineage -> inherited bindings: no illegitimate mints. **Done.**
- first pass one ES per function. **Done.**
- call site -> EntrySet: resolved by SCORING `fun->ess` on every detach
  (`find_best_entry_sets`, `entry_set_compatibility`, five `HARDREUSE`
  modes), keyed on a type partition that drifts. **This is the gap**, and
  the lookup that would close it already exists as `EntrySet::canon_key`
  -- *"the type tuple NAMES the contour -- shedskin's model -- and a
  routing decision becomes a lookup"*, i.e. `func.cp[dcpa][cart]` -- with
  `canon_enabled()` defaulting to **0**.

`PYC_CANON=1` on `sudoku5`: 21 passes / ess 690 / 82 warnings / 177 errors,
against 37 / 1104 / 249 / 364. Suite 313/0. Needs a corpus sweep on both
arms before it means anything, but it is a knob flip, not new machinery.

`EntrySet::split_origin` is kept, and honestly labelled: it is the durable
form the author asked for and it is correct, but it never decides anything
today because the transient `es->split` already covers the only pass in
which the question is asked.

## "When do we consult canon_key?" -- it should not be the decision (2026-09-09)

Author, on the recommendation to flip `PYC_CANON`: *"we are demand
splitting from a single es per function now, so when do we consult the
canon_key?"* The question is the answer, and my recommendation was wrong.

### Where it is consulted

`make_entry_set`, one place, and only after everything else has declined:
`check_split`, then `find_best_entry_sets` (non-split path) or the five
`HARDREUSE` modes (split path), then `preference`, and only then
`if (have_key && !es) find_canonical_entry_set(e, ckey)`. Per split group
that is effectively the group's FIRST edge. Measured on `sudoku5`:
`hit=3 miss=6 conflict=3` per pass.

### What it keys on

`edge_canon_key` is the tuple of the edge's ARGUMENT TYPES at each
positional formal, intersected with the formal filters, and
`find_canonical_entry_set` scans `fun->ess` for a contour with the same
tuple. That is CPA's key -- `func.cp[cart]` -- i.e. contour identity keyed
on a FACT about types.

**Which is what [146](146-remove-all-arbitrary-splitting.md)'s refinement
forbids as a REASON**: *"a FACT about the program is not a demand... A
demand is something OBSERVING a distinction and being unable to proceed."*
So flipping `PYC_CANON` on would re-introduce the thing 146 has been
removing, and its `sudoku5` win (364 -> 177) is it CAPPING a runaway by
merging contours the demand never distinguished, not it being the right
authority. Its own conflict case says so: `CANON-CONFLICT` is the type
tuple disagreeing with a demanded split, 52 times on `sudoku5`, and mode 2
(obey canon, drop the split) measures WORSE than mode 1 (honour the split).
A mechanism that is only safe when overridden is not the decision table.

**Recommendation retracted.** `PYC_CANON` is a join heuristic. Do not flip
it as "the ES-side decision table".

### The split machinery is behaving -- three things checked

- **The demanded group stays together.** `IFA_DBG_GROUPSPLIT`: of 1154
  groups on `sudoku5`, **9** scatter across more than one contour. So the
  detach-and-research shape does reconstruct the partition (via
  `preference`), 99.2% of the time. My reading that it discards the group
  was wrong.
- **Creation-point inheritance is complete** -- zero illegitimate mints
  after a split (previous section).
- **Pass 0 is one ES per function** -- `richards` 162 ess / 158 funs.

### So where does `ess` 215 -> 1104 come from? The NUMBER of groups

Not from scattering and not from minting; from TYPE_CONFLUENCE forming
445-1136 confluences per pass, each partitioning an ES. And
`collect_type_confluence` is:

```c
if (x->out->type->n && type_diff(av->in->type, x->out->type) != bottom) {
  confluences.set_add(av);
```

A writer contributes a type the accumulated value does not already have.
No violation, no irrepresentable union, no unresolved dispatch -- **"this
variable's type is a union", which is 146's own example of a FACT that is
not a demand.**

### The tension this exposes, for the author to settle

CLAUDE.md says both of these:

> "...proceeds in passes, splitting them to increase precision -- **by
> types**, by setters, and so on."

> "A contour is NEVER split because a surrounding contour was split.
> **Splitting is only ever on demand.**"

TYPE_CONFLUENCE is the first sentence and, under 146's refinement, is
disallowed by the second. It is stage 1, it fires every pass on `sudoku5`,
it is the entire source of the contour growth, and because the cascade is
first-stage-wins it starves VIOLATION -- the only stage that acts on an
actual demand.

That is the real item behind `sudoku5`, and it is a much larger question
than a knob: does stage 1 stay, gated on a demand, or go? Not something to
decide from one program's numbers.

## CORRECTION: stage 1 is NOT the arbitrary splitter I called it (2026-09-09)

Author: *"but int + str is a boxing violation so it is demand, right?"*
Correct, and my example was wrong -- `{int64, str}` is `mixed_basics`, it
raises a BOXING violation, and a contour split to separate it is
demand-driven by any reading. Worse, the implication I drew from the bad
example does not survive measurement either.

**What stage 1 actually fires on** (`IFA_DBG_CONFKIND`, classifying each
collected confluence by its union):

| | irrepresentable (BOXING demand) | representable union | several CSs of ONE sym | union still forming |
| --- | --- | --- | --- | --- |
| `richards` (default) | 22 (1.7%) | 882 | 9 | 382 |
| `chess` (default) | 1999 (22%) | 3775 | 396 | 2975 |
| `sudoku5` (flag arm) | 5575 (26%) | 7699 | 1409 | 6615 |

So a real minority are irrepresentable. That is the half of my claim that
held, and on its own it would suggest the rest are splitting on a fact.

**They are not.** `PYC_CONFDEMAND=1` drops every confluence whose union is
representable, keeping only the ones something could not proceed on:

| | errors | warnings | ess | css | pyc suite |
| --- | --- | --- | --- | --- | --- |
| stage 1 as shipped | 364 | 249 | 1104 | 2634 | **313 / 0 failed** |
| irrepresentable-only | 348 | 106 | 619 | 1611 | **214 / 106 FAILED** |

Contours fall 44%, `sudoku5` barely moves (364 -> 348), and **106 of 313
tests break**. The representable-union confluences are load-bearing: they
are what resolves dispatch and what feeds the precision later stages
depend on. A `{Dog, Cat}` union is perfectly representable AND is exactly
what a receiver split has to separate for a method call to resolve.

**So the "conflict" is a wording tension in CLAUDE.md, not a defect in
stage 1.** "Splitting is only ever on demand" and "splitting to increase
precision -- by types" do read as pulling against each other, and 146's
fact-vs-demand refinement does not cleanly classify a type confluence. But
the measurement says stage 1 is not doing arbitrary work that could simply
be removed -- the cost of removing the non-BOXING half is 106 tests, and
the benefit on the program that motivated the question is 16 errors.

What remains true from the earlier section: stage 1 fires on every pass of
`sudoku5`, that IS what starves VIOLATION under first-stage-wins, and
`ess` 215 -> 1104 is stage 1's growth. Those are facts about scheduling and
volume, and they stand. What does not stand is the inference that the
splits themselves are unjustified. The starvation is the thing to fix, not
the stage.

`PYC_CONFDEMAND` kept, default 0 (inert), as the probe that produced this.

## "Violations and dynamic dispatch as the demand" -- measured: no, and why

Author: *"there should be no conflict if violations and dynamic dispatch
are the demand. that is the precision which matters because it affects
codegen. does that work?"* Implemented as a gate on stage 1 in three
increasingly generous forms. It does not work, and the reason is
interesting rather than incidental.

| `PYC_CONFDEMAND` | gate on a type confluence | pyc suite |
| --- | --- | --- |
| 0 | none (as shipped) | **313 / 0 failed** |
| 1 | union is irrepresentable | 214 / **106 failed** |
| 2 | + AVar is an arg of a send with >1 target Fun (dynamic dispatch) | 215 / **105 failed** |
| 3 | + BACKWARD CLOSURE of both, so a confluence upstream of a demand counts | 217 / **103 failed** |

Mode 3 is the fair version -- the demand is observed at the USE, so the
split has to be allowed at the confluence upstream of it, which is the
same shape as stage 5's `collect_violation_imprecisions` / `back_reaching`.
It recovers 3 tests out of 106.

### Why: violations are a LAGGING indicator

The first failure inspected says it exactly. `tests/list_pop_insert.py` --
**this issue's own former reproducer**, fixed by the `__delitem__`
`merge_in` change and kept as the regression test -- fails under the gate
with

```
error: 'value' has mixed basic types:( int64 str )
error: 'x' has mixed basic types:( int64 str )
```

That violation **does not exist in the ungated build**. The split is what
prevents it. Gating the split on the violation means the split never
happens, so the violation appears -- and then the gate would allow the
split, one pass too late and against a union that has already merged.

So the criterion is circular for two of its three signals. A violation is
evidence that precision was ALREADY LOST; a dispatch that fails to resolve
is evidence that precision was already lost. Neither can authorise the
split that would have prevented the loss. Only the third form of
codegen-visible precision -- irrepresentability of a union that has ALREADY
formed -- is observable in time, and that is the 26% bucket which on its
own is not enough (mode 1).

### What is right about the criterion

The half that holds is dynamic dispatch, and it is not the expensive half:
adding it recovered 1 test locally and 3 with closure, so it is nearly
free but also nearly inert as a GATE. As a positive REASON to split -- the
thing ifa/146 wants stages to have -- it is exactly right, and it is what
the receiver-filtering work (146 E's replacement) is already aimed at.

### The conclusion for the wording conflict

There is no formulation of "demand" here that both (a) authorises the
splits stage 1 currently makes and (b) excludes the ones it makes on
representable unions -- because the justification for a stage-1 split is
usually a violation that will NOT happen if the split is made. That is a
counterfactual, and no local predicate on the current state can test it.

Which means CLAUDE.md's two sentences are not reconcilable by finding a
better demand test at stage 1. Either splitting by types stays as a
first-class mechanism (and "only ever on demand" is understood to govern
the *other* stages -- the ones that key on setters, marks, call sites,
creation points), or stage 1 goes and something has to replace the
precision it creates speculatively. The measurements say the second option
costs a third of the suite as things stand.

`PYC_CONFDEMAND` kept at default 0 (inert) with all three modes, since it
is the apparatus for re-running this argument.

## "int + str is a violation so it would be split -- why isn't it?" (2026-09-09)

It IS. The `{int64, str}` confluence is kept by the gate and split. The one
the gate drops is upstream, and it is a third kind of thing that the
criterion does not name.

**First, a correction to my own correction.** I thought
`collect_type_confluence` fires AS the union forms, so the gate should test
the prospective union `type_union(av->in->type, writer->out->type)` rather
than the accumulated `av->in->type`. Implemented it and measured
`prospective_differs=0` -- the writer's type is ALWAYS already in
`av->in->type`. The test is `type_diff(av->in->type, x->out->type)`, which
is non-bottom when a writer supplies only PART of the accumulated union, so
the union has already formed and the original test was correct. The suite
numbers did not move, which is the tell I should have led with.

**What the gate actually drops**, on `tests/list_pop_insert.py`
(`IFA_DBG_CONFDROP`):

```
[confdrop] fun=pop         var=self  type= list#997 list#1018
[confdrop] fun=insert      var=self  type= list#1005 list#1018
[confdrop] fun=__getitem__ var=self  type= list#997 list#1005 list#1018
[confdrop] fun=len         var=x     type= list#997 list#1005 list#1018
```

Several CreationSets of ONE sym. Every property the criterion tests says
"no demand":

- **representable** -- they are all `list`, one C layout, so no BOXING
  violation on the receiver itself;
- **not a dynamic dispatch** -- `list.pop`, `list.insert`,
  `list.__getitem__` each resolve to a single target Fun;
- **no violation recorded** on those AVars, in the ungated build, ever --
  because the split prevents the only violation that would arise.

And that is the split that keeps `l1`/`l2` (int lists) apart from `l3` (a
str list) as they pass through the SHARED container method. Merge the
receiver contours and `pop`'s element channel unions `{int64, str}`; the
violation then surfaces DOWNSTREAM, on `value` in `insert` and `x` in
`pop`, which is exactly the two errors the gated build reports.

### The missing category

So the author's criterion needs a third member, and it is not an
afterthought -- it is [143](143-shared-container-method-contours-refuse-cs-splits.md)'s
whole subject:

> **a receiver whose type spans several CreationSets of one container
> class, entering a method that reads or writes its element channel.**

That is codegen-visible for the same reason the other two are -- merging
those contours unions their elements, and an element union decides the
container's layout. But unlike a violation or a stuck dispatch it is
observable BEFORE the damage, because it does not require the union to have
formed: it is a property of the receiver's contour set at the call, not of
the element type that would result.

This also explains why mode 3's backward closure did not rescue it. The
closure seeds from violations present in the CURRENT pass, and in a
correctly-split build there are none -- the splits are why. Seeding from a
consequence that the seeding is supposed to prevent is empty by
construction. Seeding from the receiver-contour property above is not.

### Status of the question

"Violations and dynamic dispatch" is not sufficient, and the missing piece
is exactly what 143 is already about and what 146 E's receiver-filtering
replacement is aimed at. The conflict is narrower than I said two sections
ago: it is not that stage 1 splits on facts, it is that ONE of its
justifications -- keeping container receiver contours apart inside shared
methods -- has never been written down as a demand, so stage 1 carries it
implicitly by splitting on all type disagreement.

## The demand transfer has TWO targets, not one (2026-09-09)

Author: *"the demand comes from the instance variable or elements having a
demand which is then transferred to a demand to split the cs via setter
splitting."* That is CLAUDE.md's stated dependency -- *"an EntrySet is
split SO THAT a CreationSet split becomes possible"* -- and it is right,
but `tests/list_pop_insert.py` shows it is only half the story, because on
that program **there is no CreationSet to split**.

With stage 1 gated off, route 4 reports:

```
[csdefsplit] cs=997  sym=list defs=1 DECLINED (single creation point)
[csdefsplit] cs=1005 sym=list defs=1 DECLINED (single creation point)
[csdefsplit] cs=1018 sym=list defs=1 DECLINED (single creation point)
```

The three lists are ALREADY on three separate CreationSets, one creation
point each. The data contours are as fine as they can be, and setter
splitting has nothing to partition -- `split_css` is reached with
`starters=0`, `1`, `2` and returns 0 every time.

The merge is on the CONTROL side: `list.pop`, `list.insert` and
`list.__getitem__` each have ONE EntrySet whose `self` sees all three CSs,
so reading `self[i]` inside that one body unions the three element types
and the violation lands on the method's own local (`value` in `insert`,
`x` in `pop`) -- an ES-contoured AVar, not any CreationSet's.

**So the demand transfers to whichever side is merged:**

| where the merge is | the split the demand asks for | machinery |
| --- | --- | --- |
| one CreationSet, several creation points | split the CS by its setters | `split_css` / route 4 -- `richards`, `sudoku5` |
| several CreationSets, one shared method contour | split the ES by its receiver | stage 5 VIOLATION -- `list_pop_insert`, and [143](143-shared-container-method-contours-refuse-cs-splits.md) |

Both are "a demand observed on an element or instance variable, transferred
to a contour split". The second is not a different KIND of demand -- which
is what I called it in the previous section, and that was wrong -- it is
the same demand with the other contour as its target.

### And the second path already exists, fires, and is throttled

Measured on `list_pop_insert` with stage 1 gated:

| pass | stage 5 (VIOLATION) | violations |
| --- | --- | --- |
| 0-2 | **starved** (`analyze_again=1` from the confluences stage 1 still keeps) | 26, 31, 36 |
| 3 | **RUNS**, `d_ess=3` | 33 -> **23** |
| 4, 5 | RUNS, splits nothing | 23, 23 |

It does exactly the right thing -- three EntrySet splits, violations cut by
a third -- and then stops, because `split_for_violations` excludes a Var
after **two** stage-5 attempts (issue 033 D6, "not refinable by contour
splitting"). Three passes of stage 5 is where that bites.

So the honest position on the whole thread: the demand-driven path for this
case is not missing, it is (a) starved for the first three passes and (b)
capped at two attempts per Var. Stage 1 hides both by splitting
speculatively before any violation exists. That is a much more tractable
statement than "stage 1 is arbitrary" or "a third demand category is
missing", and it is what the earlier `sudoku5` starvation finding was
pointing at from the other end.

## Tried removing the stage-5 cap: it is load-bearing, and it was never the throttle

Author: *"try removing the cap."* Made it tunable (`PYC_VIOLATTEMPTS`,
default 2, `0` = no cap) and measured. Two results, and the first corrects
me.

### The cap was NOT what stops stage 5 on `list_pop_insert`

I said stage 5 "stops because `split_for_violations` excludes a Var after
two attempts". Wrong, and I had already measured the disproof earlier in
this session and not connected it. With stage 1 gated:

```
cap fired (nonrefinable): 0
stage5 attempts:          3
[stage5] 61 violations -> 1 imprecisions
```

**The cap never fires.** Removing it changes nothing -- suite with stage 1
gated is 214/106 with the cap and 214/106 without; mode 3 is 217/103 both
ways; `sudoku5`, `bh`, `richards`, `chess` and `sudoku1` are byte-identical
on both arms (same rc, warnings, `ess`, `css`).

**The actual throttle is `collect_violation_imprecisions`: 61 violations
yield ONE splittable AVar.** Its filter is

```c
if (v->av->container && v->av->container->out->n > 1) imprecisions.set_add(v->av->container);
```

so a violation only becomes a split candidate when the violating AVar has a
CONTAINER whose type spans several CreationSets. Everything else is
dropped. That, not the cap, is why the demand stage cannot finish the job
stage 1 was doing speculatively -- and it is a much more specific target.

### And the cap must stay

It is not dead code. It fires **68 times on `fysphun`**, the program
issue 033 D6 cites, and removing it breaks that program on both arms:

| | default arm | flag arm |
| --- | --- | --- |
| `PYC_VIOLATTEMPTS=2` | rc=0, 13 passes, ess=238 css=821 | rc=0, 18 passes, ess=247 css=700 |
| `PYC_VIOLATTEMPTS=0` | **rc=1** | **rc=1** |

```
fail: FA flow analysis made no EntrySet progress for 120s
      (180000 edges processed) -- non-convergent input
```

So without the cap `fysphun` does not converge at all -- exactly the
"manufacturing contours every pass" that D6 describes. Note the pyc suite
stays 313/0 throughout, because `fysphun` is a corpus program and not a
suite test; the suite alone would have said "safe to remove".

**`PYC_VIOLATTEMPTS` kept, default 2 (unchanged behaviour), as the
apparatus.** The next thing to look at is
`collect_violation_imprecisions`'s container filter, not the cap.

## The demand is UNOBSERVABLE at the moment of the merge (2026-09-10)

This is the sharpest statement of why "split only on demand" cannot, on
its own, be the whole story for container literals. It came out of asking
why `bh`'s two literals share a contour at all.

**The identity rule is a joiner and a separator, and they exist for
different reasons.**

- **Joiner** -- one CreationSet per sym (`PYC_CSDCPA1`,
  [128](128-cs-identity-over-discriminates-vs-element-type.md)). Deliberate:
  CLAUDE.md's opening premise is to start from the MINIMUM data contours
  and split only on demand.
- **Separator** -- arity
  ([132](132-arity-is-representation-not-provenance.md)). Carved back out
  because merging different arities is UNREPRESENTABLE: `b = [2,3]` and
  `k = []` sharing a CS made codegen emit a record, `len()` fold to the
  static field count, and `print(k)` produce `[0, 0]`.

Arity is in the key not to distinguish provenance but because the target
language cannot represent the merge -- CLAUDE.md's explicitly legitimate
third category, *"what the target language can REPRESENT"*.

### Which reframes the `bh` probe

Changing `Random.__slots__ = ["seed"]` to arity 2 did not expose a bug in
the rule. It made the SEPARATOR FIRE. `["seed"]` and `[None]` are both
arity 1, so arity correctly says nothing about them -- and nothing else
separates them either. **Arity is currently the only working separator for
list literals.**

### And here is the trap

The natural sibling of arity would be the literal's SLOT TYPE: `["seed"]`
holds `str`, `[None]` holds nil. But at the literal, slot 0 is
`{str, None}`, and that IS representable -- `elem_irrepresentable` skips
nil and finds a single basic, so **no demand is raised at the merge**.

The union only becomes irrepresentable much later and somewhere else:
`list.__mul__` (`P_prim_merge`) pours slot 0 into the RESULT's element,
`__setitem__` adds `Body`, and `{str, Body}` is finally a demand -- on a
different CreationSet, several contours downstream, by which time the two
literals are long since merged and every creation point carries the whole
union.

**So the demand is unobservable at the moment the merge happens, and the
merge is unrecoverable at the moment the demand appears.** That is the same
lag that defeated every violation-gated experiment earlier in this issue
(the `PYC_CONFDEMAND` modes, and the "end at best pass" idea before them),
arriving here from the literal side rather than the splitter side.

### What follows

It is not an argument for splitting literals eagerly -- that is the fan
[146](146-remove-all-arbitrary-splitting.md) exists to remove. It is an
argument that the *separator* set is too small: arity is one
representation property, and it is doing all the work alone. The question
worth asking is which OTHER representation properties of a literal are
knowable at construction and would have kept these apart -- the slot's
basic-type kind being the obvious candidate, since `str` and nil differ
there even though their union is representable.

That is a question about REPRESENTATION, not about demand, which is why it
sidesteps the lag entirely.
