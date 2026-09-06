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
