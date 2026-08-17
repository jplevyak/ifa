# 104 — unify `list` and `tuple` in analysis, specialize at implementation

> **ATTEMPTED 2026-08-17 and the naive form does not work.** `PYC_UNIFY_SEQ=1`
> (build tuple literals as lists, let `tuple_able` pick the layout) is
> implemented and off by default. It breaks **13 tests**, and the reason
> is one my assessment below missed: **`repr`**. See "What the experiment
> found" at the end — the design needs a provenance bit, not a plain
> merge.

**Status:** open design proposal, assessed 2026-08-17. **The
specialize-at-implementation half already exists and works**; what is
proposed is removing the a-priori split in the analysis. Measurements
below.

## What already exists

`ifa/analysis/clone.cc` has `tuple_able`, live behind
`#define CONVERT_LISTS_TO_TUPLES 1` (line 16):

```cpp
static bool tuple_able(CreationSet *cs) {
  AVar *elem = get_element_avar(cs);
  return elem && elem->out == fa->type_world.bottom_type;
}
```

A container is tuple-able when its **generic element AVar is bottom** —
nothing ever flowed through the element, because every access went to a
constant index. `define_concrete_types` then lays such a CreationSet out
as a record.

Verified working. A *heterogeneous list* under constant indices:

```python
def mk(n): return [n, "two", float(n) * 1.5]
x = mk(len(sys.argv)); print(x[0]); print(x[1]); print(x[2])
```

emits a struct with individually-typed fields, not a list:

```c
/* list */ struct _CG_s10981; typedef struct _CG_s10981 *_CG_ps10981;
t1 = _CG_prim_tuple_list(_CG_ps10981, 3);
t1->e0 = t2;                            /* int64  */
t1->e1 = _CG_String_n("two",3);         /* string */
t1->e2 = t3;                            /* float64 */
t6 = (_CG_int64)((_CG_ps10981)t7)->e0;  /* typed constant-index read */
```

So the representation choice is already made *from the analysis result*,
per CreationSet, after FA.

**Note the discriminator is exactly the right one.** "Element AVar is
bottom" means no operation treated this container as a variable-length
sequence — append, iterate and dynamic indexing all flow through the
element var. So the condition is self-guarding: anything that would
invalidate a record layout also populates the element type and disables
it.

## The semantic cost of unifying is ~zero — measured

pyc **already** fails to enforce both distinctions that separate the two
types:

| program | CPython | pyc |
|---|---|---|
| `t = (n, n+1); t[0] = 99` | `TypeError: 'tuple' object does not support item assignment` | **prints 99** |
| `L = [n, n+1]; d[L] = …` | `TypeError: unhashable type: 'list'` | **works** |
| `d[(n, n+1)] = …` | works | works |

So the analysis-level distinction is not protecting immutability or
hashability today. Unification would not lose a diagnostic that exists.
(It would foreclose *adding* those diagnostics later at the type level —
they would have to become representation- or provenance-based checks.)

## The benefit is real but program-dependent — measured

`IFA_DBG_SPLITSYM` counts split partitions whose CreationSets include
**both** a `list` and a `tuple`:

| program | splits | partitions mixing list+tuple |
|---|---|---|
| `plcfrs` | 665 | **125 (19 %)** |
| `rdb` | 1920 | **273 (14 %)** |
| `sudoku5` | 723 | **74 (10 %)** |
| `go` | 344 | 1 (0.3 %) |
| `linalg` | 821 | **0** |

This is an *upper bound* on what unification could remove — a mixed
partition may have other distinguishing content — but for the tuple-heavy
programs it is a substantial fraction, and `plcfrs` is one of the three
that still do not converge
([101](101-FA-first-time-forever-splitting.md)).

Related, from 101's element-type survey: **~50 % of container
CreationSets already have no generic element type** (569 with a bottom
element AVar, 427 with none created at all, of 1994). That is the
population `tuple_able` keys on, so the specialization path is broadly
applicable rather than a corner case.

## What unification would involve

The pieces are mostly present:

- A container CreationSet **already carries both representations'
  information** — a generic element AVar *and* per-index vars
  (`cs->vars`). That duality is precisely the unified model.
- The representation decision already exists (`tuple_able` /
  `define_concrete_types`).
- What is a-priori is the *Sym*: `[...]` builds `sym_list` and `(...)`
  builds `sym_tuple` at lowering, and they carry separate method sets.

So the change is to stop distinguishing them at the Sym level and let the
existing per-CS decision pick the layout — rather than to build new
machinery.

## Risks to check before attempting

1. **`get_sym_tup` special-cases `sym_tuple`** (`sym != sym_tuple &&
   sym != sym_closure && !tup`) in `define_concrete_types`. That
   interaction needs untangling first.
2. **Hashing and equality.** `tuple.__hash__` exists and is used for dict
   keys; a record-laid-out container must hash and compare by value.
   pyc already permits list keys, so behaviour would not regress, but the
   unified type needs one coherent `__hash__`/`__eq__` story.
3. **Method-set merge.** `list` has `append`/`pop`/`sort`; `tuple` does
   not. Unified, those become available on everything — matching pyc's
   current laxity but widening it further.
4. **Do not assume it helps 101.** `__getitem__`'s 236 contours on
   `linalg` come from splitting per *receiver CreationSet*, and `linalg`
   shows **0** mixed partitions. Unification reduces the number of
   distinct container *types*, not the number of CreationSets, so its
   effect on the contour explosion is likely small — the measured win is
   on `plcfrs`/`rdb`/`sudoku5`, not on the CS-count problem.

## Verification plan

- The three semantic tests above keep their current behaviour (no
  regression), ideally becoming `.known_issue` tests for the diagnostics
  pyc does not emit.
- `plcfrs`, `rdb`, `sudoku5` show a measurable drop in split count.
- Full corpus: no exit-code changes **and** no run-status changes
  (`ifa/issues/runstatus.sh` — compile status alone is not evidence, see
  [102](102-corpus-programs-compile-then-abort-at-runtime.md)).


## What the experiment found (2026-08-17)

`PYC_UNIFY_SEQ` makes `PY_tuple` emit `sym_list` instead of `sym_tuple`.
The two literal cases in `python_ifa_build_if1.cc` are otherwise
character-for-character identical, so this is the whole change.

**It breaks 13 tests** (263 passed / 13 failed, against 273 / 0):

```
builtin_type_factory  colorsys_module  dict_items_keys_values
genexpr_basic  kwarg_out_of_order  itertools_module  match_seq
minmax_3arg  tuple_compare  test_heapq  tuple_arity_union
tuple_unpack_target_arity_union  tuple_eq_str
```

### The blocker my assessment missed: `repr`

Most failures are exactly this:

| | expected | got |
|---|---|---|
| `tuple_eq_str` | `(1, 2, 3)` / `(1,)` | `[1, 2, 3]` / `[1]` |
| `minmax_3arg` | `(0.9, 0.5, 1.0)` | `[0.9, 0.5, 1.0]` |
| `dict_items_keys_values` | `[('a','x'), …]` | `[['a','x'], …]` |

The assessment above checked the two distinctions pyc *fails* to enforce
— tuple immutability and list unhashability — and concluded the semantic
cost was ~zero. **It never checked `repr`, which pyc does implement
correctly**, and which is an observable difference on every tuple that
reaches output. That was the wrong conclusion drawn from an incomplete
check.

### And at least one genuine dispatch regression

`genexpr_basic` is not a printing difference:

```
Assertion `!"runtime error: matching function not found"' failed.
```

`match_seq`, `test_heapq` and `tuple_compare` also fail at compile-out
rather than on output, so the merge disturbs dispatch beyond `repr`.
Those were not characterised further.

## Revised design

Unification has to keep a **provenance bit on the CreationSet** —
`built_as_tuple` — that codegen consults for `repr`/`str`, while the
*type* is unified for analysis and dispatch. That fits the
"specialize at implementation" framing exactly: the bit is an
implementation detail, not part of the type, and so must not enter
contour identity (see
[100](100-FA-display-removed-from-contour-identity.md)'s rule).

Two consequences to design around:

1. A union of a tuple-built and a list-built CreationSet would need a
   runtime tag to print correctly. That is not a new cost — such a union
   *already* requires polymorphic dispatch today — but it does mean the
   bit cannot always be resolved statically.
2. The dispatch regressions (`genexpr_basic` et al.) must be understood
   before assuming the bit is sufficient. `repr` is the *majority* of the
   breakage, not all of it.

Until that is built, `PYC_UNIFY_SEQ` stays off. It is kept because it is
the cheapest way to re-measure the idea, and because the 13-test failure
list is a precise specification of what a correct version must preserve.

## The benefit measurement, run at last (2026-08-17)

The "partitions mixing list+tuple" table above is a **proxy**, and it was
never checked against the thing it was standing in for. Running
`PYC_UNIFY_SEQ` and measuring the actual effect:

| program | splits | violations | ess | css | passes |
|---|---|---|---|---|---|
| `plcfrs` | 665 → **1053** ✗ | 2232 → 1769 ✓ | 850 → 1060 ✗ | 2709 → 2581 ✓ | 30 → 38 ✗ |
| `rdb` | 1920 → 1539 ✓ | 136 → 146 ✗ | 1113 → 1149 ✗ | 2667 → 2698 ✗ | 39 → 38 |
| `sudoku5` | 723 → **468** ✓ | 180 → 187 | 492 → **421** ✓ | 1596 → **1135** ✓ | 40 → **28** ✓ |

**The proxy was misleading.** `plcfrs` had the highest mix (19 %) and
gets *more* splits, more contours and eight more passes. `rdb` is roughly
neutral. Only `sudoku5` is a clear win — and a good one (`css` −29 %,
passes 40 → 28).

So the payoff is inconsistent and program-dependent, which changes the
cost/benefit: it does **not** currently justify the `repr` redesign
described above. The right order of work is to understand why `plcfrs`
gets worse before building the provenance machinery.

## On avoiding the runtime tag

The observation that a runtime tag is unnecessary is **correct**, with
one caveat about which form of unification it applies to.

A tuple literal and a list literal are different creation sites, so
`creation_point` mints them different CreationSets and nothing merges
across sites (`PYC_CSMOLD`'s reuse is keyed on `creation_var`). A
`{tuple-built, list-built}` union is therefore a union of two *distinct
CSs*, and dispatch already fans per CS — each branch statically knows its
own layout. **No runtime tag is needed to pick a representation.**

The caveat is `repr` specifically. It is not a codegen decision — it is
an ordinary Python-level method, `tuple.__str__` in
`__pyc__/04_sequence.py`, looked up **by sym**:

```python
class tuple:
  def __str__(self):
    x = "("
    ...
```

So the two options are not equivalent:

- **Two symbols** preserves `repr` for free, because dispatch still finds
  `tuple.__str__`. But then the unification has to happen somewhere other
  than the type — at the *representation* level, so that a `{list,
  tuple}` union is representable without splitting. That is a different
  (and probably smaller) change than merging the syms.
- **CS separation with a unified sym** keeps the layouts distinguishable
  but **not** `repr`: with one sym there is one `__str__`, and the
  per-CS bit has no way to reach method lookup. To recover it, the
  concrete type `clone.cc` already mints per CS group would have to carry
  the tuple method set — which is close to re-introducing two symbols,
  late.

Given the measurement above, the "two symbols, unify the representation"
direction is the one worth pricing first: it keeps `repr` working by
construction, and the 13-test failure list stops being a cost to pay
down.

## Pricing "two symbols, unify the representation" (2026-08-17)

### Feasibility gate: are the tuples homogeneous?

Representation unification can only give a tuple a list layout when the
tuple is **homogeneous** — a heterogeneous tuple is a record and has no
list form. `IFA_DBG_TUPHOMO` counts, among split partitions that mix list
and tuple, how many contain a heterogeneous tuple:

| program | mixed partitions | containing a heterogeneous tuple |
|---|---|---|
| `plcfrs` | 109 | **102 (94 %)** |
| `sudoku5` | 70 | **66 (94 %)** |
| `rdb` | 267 | 126 (**47 %**) |

So layout unification could address **6 %** of the mixed partitions on
`plcfrs` and `sudoku5`, and **53 %** on `rdb`.

**The dominant case is a heterogeneous tuple unioned with a list.** That
is not a layout problem at all — it is the
[018](../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md) boxing problem,
and no amount of unifying `list` with `tuple` touches it.

### Implementation cost, if pursued anyway

Small and contained, because keeping two symbols means `repr` and the
method sets keep working by ordinary sym dispatch — the 13-test failure
list from `PYC_UNIFY_SEQ` simply does not arise.

The change is one condition in `clone.cc`'s `define_concrete_types`,
which today reads

```cpp
s->type_kind = (sym == sym_tuple || tup) ? Type_RECORD : Type_FUN;
```

and would become "`sym_tuple` **and heterogeneous** → `Type_RECORD`;
homogeneous → list layout". Plus verification that `tuple.__str__`'s
`len(self)` and `self[k]` still work against a list-laid-out receiver
(they go through `index_object`, so probably yes), and untangling
`get_sym_tup`'s `sym != sym_tuple` special case.

Estimate: tens of lines, one afternoon, low risk — **but** aimed at 6 % of
the cases on two of the three programs that motivated it.

> **That estimate is wrong. Attempted 2026-08-17 — see below.**

### Verdict

Cheap to build, and it keeps `repr` correct by construction, which is the
right shape. But the measurement says it is aimed at the minority of the
target: on `plcfrs` and `sudoku5`, 94 % of the mixed partitions need
boxing, not layout unification. `rdb` is the only program where it would
reach half the cases, and `rdb` was already neutral-to-worse under full
unification.

**Recommendation: do not build it yet.** — but see the correction below;
the reason given here ("spend the effort on boxing instead") was wrong.

## CORRECTION: boxing is not the answer, and was never checked

The paragraph above asserted that the 94 % heterogeneous mixes "need
boxing". That was asserted, not measured, and **shedskin disproves it**:

| | generated lines | `pyobj*` (boxed) | `pyseq<T>` (common base) | `tuple2<>` instantiations |
|---|---|---|---|---|
| `plcfrs` | 1830 | **2** | **0** | **110** |
| `go` | 1184 | **2** | **0** | 1 |
| `linalg` | 520 | **0** | **0** | 0 |

shedskin compiles all three with **essentially no boxing** and **zero
uses of its `pyseq<T>` common base** — even though that base exists
(`template <class T> class pyseq : public pyiter<T>`, with `list<T>` and
`tuple2<A,B>` under it). It neither boxes nor falls back to a shared
representation.

**It simply never has a variable holding both.** `list<T>::__getitem__`
and `tuple2<A,B>::__getitem__` are separate template instantiations, so
the C++ compiler produces one per receiver type and no single
`__getitem__` ever sees a union.

### Where pyc's union actually comes from

The functions carrying the `{list, tuple}` mix in `plcfrs` are precisely
the shared generic accessors:

```
61 __getitem__   10 __eq__   9 len   8 __len__   8 __iter__   5 __lt__
```

pyc clones these and relies on **contour splitting** to give each
receiver type its own copy. The union appearing inside them means the
splitting has not separated them — which is
[101](101-FA-first-time-forever-splitting.md), not a representation
problem.

So the union is a **pyc contour-separation artifact**, not an inherent
property of these programs. There is nothing here that requires boxing,
a fat pointer, or a unified layout.

### The option that was missing from this issue

**Third option: make the analysis precise enough that the union never
arises.** That is what shedskin does, it is what the earlier shedskin
comparison in 101 already concluded ("the remaining work is a splitting
rule — do not split a container method per receiver CS when the receivers
agree on element type; the template architecture is the real lesson"),
and this issue then contradicted it by recommending boxing.

Ranking, corrected:

1. **Contour separation for shared container accessors** (101). What
   shedskin gets for free from template instantiation. Addresses the
   union at its source, on all three programs.
2. **Two symbols, unify the representation** (priced above): cheap, keeps
   `repr` correct by construction, but reaches only the homogeneous
   minority — 6 % on `plcfrs`/`sudoku5`.
3. **Boxing (018/030)**: still needed for genuinely dynamic unions like
   `{None, int}`, but **not** for this — and it should not have been
   offered as the answer here.

## Attempting option 2 (2026-08-17): the estimate was wrong

Implemented `PYC_TUPLE_AS_LIST`: compute `homogeneous_tuple(cs)` (every
field the same `AType`), and for an equivalence class of homogeneous
tuples route it past the record branch in `define_concrete_types` so it
takes the ordinary sequence path.

**The compiler segfaults** on the simplest homogeneous tuple
(`t = (n, n+1)`).

### Why — and why it is not a condition flip

The non-record path ends in

```cpp
name = cs->sym->type->name;
```

and `sym_tuple` is an **ifa-core primitive type**
(`new_builtin_primitive_type(sym_tuple, "tuple")`), not a `__pyc__` class
like `list`, so `->type` is not a class sym. That is the crash.

Guarding it would not help, because the branch would then produce **a
clone of `sym_tuple`** with an inherited `type_kind` — an unformed tuple
type, not a list. "Give the tuple list layout" actually requires:

1. obtaining (or synthesizing) the **`list` concrete type for element
   type `T`**, which is derived per-CreationSet elsewhere and is not
   simply `sym_list`;
2. making codegen emit sequence operations against it — `_CG_list_ptr`
   and friends rather than `->e0`;
3. checking `tuple.__str__`'s body (`len(self)`, `self[k]`) against that
   layout — method resolution has already happened by `clone.cc`, so the
   body is fixed and must work as written.

So the correct price is **not** "one condition in `define_concrete_types`".
It is a new path that mints list concrete types for tuple CreationSets,
in a subsystem (`define_concrete_types`) whose two-pass clone/no-clone
structure and `sym_tuple`/`sym_closure` special cases are load-bearing.

**Revised estimate: days, not an afternoon, and medium risk** — against a
payoff already measured at 6 % of mixed partitions on `plcfrs` and
`sudoku5`.

The attempt was reverted rather than left behind a flag, since a
segfaulting flag is worse than none. The `homogeneous_tuple` predicate is
recorded here rather than in code:

```cpp
static bool homogeneous_tuple(CreationSet *cs) {
  if (!cs || cs->sym != sym_tuple) return false;
  AType *first = nullptr; int n = 0;
  for (AVar *fv : cs->vars) if (fv) {
    ++n;
    if (!first) first = fv->out->type;
    else if (fv->out->type != first) return false;
  }
  return n > 0;
}
```

### This strengthens the corrected ranking

Option 2 is now *more* expensive than option 1 and still reaches only the
minority of cases. **Contour separation for the shared container
accessors (101) is the recommendation** — it is what shedskin gets for
free from template instantiation, and it addresses the union at its
source on all three programs.

## Attempting option 1 (2026-08-17): the hypothesis was wrong, but the
## experiment found something better

Option 1 was "separate the shared container accessors by receiver type,
the way template instantiation does". `PYC_RECVFAN` was built to test it.

### Where the union actually comes from

`edge_type_compatible_with_edge` already separates two edges whose
receiver types differ (`etype != eetype` → incompatible). So the
`{list, tuple}` mix is **not** two edges being grouped — it is a *single*
edge whose receiver type is already the union, i.e. one call site passing
a variable that holds both. Grouping cannot fix that; the edge has to be
**fanned per receiver CreationSet**.

pyc has that: `split_for_per_cs_method_receivers` (`PER_CS_RECEIVER`,
issue 045). It bails on a mixed-class receiver — `cls != t →
all_flagged = false`, *"one class per split"* — which is exactly the
`{list, tuple}` case.

### `PYC_RECVFAN=1` and `=2`: the list/tuple fan is inert

Relaxing that rule for mixed **container** receivers changed nothing
(`=1`). Nor did lifting the stage's quiescence gate *for the
mixed-container case only* (`=2`): byte-identical on `plcfrs`, `rdb`,
`sudoku5`, `go`, `linalg`. **The `{list, tuple}` fan never fires.** So
this issue's premise — that separating list from tuple at the accessors
is what these programs need — is **not supported**.

### What did work: the stage is starved

`PER_CS_RECEIVER` runs only `if (!analyze_again)` — on full quiescence of
stages 1–5, deliberately, "so it cannot perturb their trajectories". On
these programs `TYPE_CONFLUENCE` fires every pass, so **the stage never
runs at all**.

`PYC_RECVFAN=3` lifts that gate for everything the stage already handles:

| | gate on | **gate lifted** |
|---|---|---|
| `plcfrs` | pass 30, **limit hit**, viol **2232**, ess 850, css 2709 | pass **22**, **CONVERGES**, viol **66**, ess **328**, css **1153** |
| `go` | pass 47, limit hit, viol 164, ess 488 | pass 46, limit hit, viol **94**, ess **390**, css 1246 |
| `linalg` | pass 51, limit hit, viol **40**, ess 668 | pass 32, limit hit, viol **222** ✗, ess 577, css 1618 |

**`plcfrs` converges** — one of the three programs from
[101](101-FA-first-time-forever-splitting.md) — with violations down 97 %
and contours down 61 %. `go` improves. `linalg` gets much worse on
violations.

### The cost

**14 real test failures** (plus 5 `splitter_*` characterization tests
that merely gain `PER_CS_RECV` in their pinned `STAGES:` line):

```
builtins  builtin_type_factory  bool_ordering  deepcopy_list
dict_from_iterable  genetic2_idioms  genexpr_basic
list_index_type_mismatch_salvage  itertools_module  minmax_3arg
match_seq  recursive_polymorphic  scope_read_before_write
set_from_iterable
```

Most are COMPILE-OUT against `tests/empty` — the early fan emits warnings
where none were expected, i.e. it costs precision elsewhere. That is
exactly what the quiescence gate was put there to prevent.

### Conclusion

The list/tuple framing of this issue is a dead end: the fan for it never
fires. But the experiment localises something more valuable — **the
first-stage-wins cascade is starving `PER_CS_RECEIVER`, and that
starvation is what keeps `plcfrs` from converging.** Running it earlier
converges `plcfrs` outright and helps `go`, at the price of 14 tests and
a large `linalg` regression.

That is a much better lead than anything in this issue, and it belongs in
101 rather than here. The next step is not "lift the gate" but "let
`PER_CS_RECEIVER` run early *only where it pays*" — the same
earn-your-contours question 101 already frames, now with a concrete
stage and a program that flips.

`PYC_RECVFAN` is kept off by default: 1 and 2 are the (inert) list/tuple
fan, 3 is the gate lift.

## The actual goal, sharpened (2026-08-17)

> *Allow tuples of monomorphic elements but different sizes to convert to
> the list runtime representation.*

That is a much better target than "unify list and tuple", and it is the
one shedskin actually implements: `tuple2<A,B>` for heterogeneous fixed
pairs, and a **variable-length homogeneous `tuple<T>`** for the rest
(`plcfrs`: 110 `tuple2<>` instantiations against 86 `tuple<>`).

In pyc every tuple is a fixed-arity record, so `(a,b)` and `(a,b,c)` with
the same element type are **different types**, and any variable holding
both is a union. Under a list representation they are one type.

### The right metric — measured

`IFA_DBG_TUPARITY` counts split partitions holding tuples with the **same
element type but different arity**:

| program | such partitions | of total splits |
|---|---|---|
| `plcfrs` | **109** | 665 (16 %) |
| `rdb` | **172** | 1920 (9 %) |
| `sudoku5` | **117** | 723 (16 %) |
| `go` | 0 | — |
| `linalg` | 0 | — |

This is the population a tuple→list representation collapses, and it is
a different (and better-motivated) population than the "mixed list+tuple"
one this issue started from.

### The blocker, found

```cpp
sym_list->element = new_sym();     // python_ifa_sym.cc:108
sym_vector->element = new_sym();
                                   // sym_tuple->element is NEVER SET
```

A tuple CreationSet therefore has **no generic element AVar at all**:
`get_element_avar()` returns 0, `tuple_able()` is unconditionally false
for tuples, and nothing can even ask "is this tuple monomorphic?". Lists
carry both views — per-index vars *and* an element type; tuples carry
only the first. That is the prerequisite for everything above.

### `PYC_TUPELEM`: prerequisite implemented, and where it stands

Giving `sym_tuple` an element sym and flowing each field into it in
`make_kind` works mechanically — `plcfrs`'s tuples now report
`68 CS / 37 elemtypes / 19 shapes`, so monomorphicity is now a question
the analysis can answer.

It costs **11 tests**, and they are all one thing:

```
tuple_mixed_types.py:4: warning: expression has mixed basic types:( int64 float64 str )
    print(a[1])
```

A **heterogeneous** tuple's generic element is the union of its fields,
and that union leaks into **constant-index reads** that were previously
precise per-field. The other failures are the same shape
(`tuple_compare`, `tuple_list_mix`, `destructuring_targets`,
`dict_*`, `test_heapq`).

### What remains

The element must be an **analysis-only query** — "do all fields agree?" —
and must not participate in indexing or in violation collection. Concretely:

1. Keep the field→element flow, but ensure `prim_index_object` with a
   **constant** index still resolves through `cs->vars[i]`, never the
   element. The per-index precision is the thing that must not regress.
2. Exclude the tuple element AVar from BOXING/mixed-basics collection —
   a union there is expected and means only "not monomorphic", not "needs
   a boxed representation".
3. Then, in `define_concrete_types`, a group of tuple CSs whose element
   type is monomorphic and identical may share **one list-represented
   type regardless of arity** — which is the goal, and which the earlier
   `PYC_TUPLE_AS_LIST` attempt could not even express because there was
   no element to test.

Steps 1–2 are the real work and are bounded; step 3 is the payoff.
`PYC_TUPELEM` is kept off by default with the prerequisite in place.

## Building it (2026-08-17) — three of four pieces done

Correcting the previous section: **heterogeneous short lists have no
problem with constant-index reads, so tuples do not need one either.**
The reason lists keep per-field precision is that `make_kind` **never
flows fields into the generic element** — the element stays bottom, which
is exactly what `tuple_able()` tests for; it is populated on *use*
(dynamic index, iteration, append), not on construction. The first
`PYC_TUPELEM` added a construction-time flow lists do not have, and that
alone caused all 11 failures.

With that flow removed, **`PYC_TUPELEM=1` is clean: 273 passed, 0
failed.** Monomorphicity is asked of `cs->vars`, never of the element.

### Piece 1 — element sym for `tuple` ✅

`sym_tuple->element = new_sym()`. Free.

### Piece 2 — representation choice ✅

`clone.cc`: `monomorphic_tuple(cs)` (all per-index vars one type) and
`group_monomorphic_tuple(eqcss)`, used to **exclude** the group from the
record branch so it takes the arity-independent path.

One trap worth recording: this must be part of the record branch's
*condition*. Written as a branch of its own it exits the `else if` chain
and leaves `cs->type` null, which segfaults later in
`resolve_concrete_types` — that was the earlier `PYC_TUPLE_AS_LIST`
crash, misdiagnosed at the time as "the path needs a list concrete type".

### Piece 3 — construction codegen ✅

`cg.cc`'s `P_prim_make` already had the *opposite* mirror: a **list**
whose CreationSet came out record-shaped does `goto Ltuple`. The missing
symmetric case — a **tuple** that took list representation — is now
`goto Llist`. (The locals had to be hoisted above both branches for the
jump to be legal.)

### Piece 4 — resolved by mirroring how `list` degrades ✅

The emitted list has a `void*` element:

```
sem.py.c:582: error: incompatible integer to pointer conversion assigning
to '_CG_void_type' from '_CG_int64'
```

Because the element AVar is deliberately bottom, `concretize_avar` derives
`void` for it. Seeding `cs->type->element->type` in `define_concrete_types`
does **not** survive — `resolve_concrete_types` runs afterwards and
re-derives from the AVar.

**Seeding was the wrong idea — `list` never needs it.** A container whose
generic element is bottom was only ever touched by constant-index reads,
and `tuple_able` gives such a list a **record** layout, which is both more
precise and needs no element type. So the void-element case *cannot arise*
for lists. Tuples must degrade the same way: a bottom-element tuple stays
a record. List representation is only for containers actually used
generically (iteration, dynamic index, `len`) — and those already have a
resolved element type, from the use that populated it.

Adding `if (!elem || elem->out == bottom) return false;` to
`group_monomorphic_tuple` fixes it with no seeding at all. All four pieces
now compile and run, and the **full suite is clean with both flags on:
273 passed, 0 failed.**

## But it does not pay off, and the prerequisite is not free

| program | flags off | `PYC_TUPELEM=1` alone | both flags |
|---|---|---|---|
| `plcfrs` | viol **2232**, ess 850, css 2709 | viol **4353**, ess 1213, css 3949 | identical to TUPELEM-only |
| `rdb` | viol 136, ess 1113 | unchanged | unchanged |
| `sudoku5` | viol 180, ess 492 | unchanged | unchanged |

Two things, both negative:

1. **`tuplist_groups=0` on all three** — the representation choice never
   fires. The 109/172/117 same-element-different-arity partitions are
   counted over *split partitions*, but `group_monomorphic_tuple` is asked
   of `clone.cc` **equivalence classes**, which group differently; no class
   comes out wholly monomorphic with a populated, agreeing element.
2. **`PYC_TUPELEM` alone costs `plcfrs`** — violations 2232 → 4353, `ess`
   +43 %. Giving `sym_tuple` an element sym makes tuples answer
   `cs->sym->element`, so every path keyed on "is this a container"
   (`sizeof_element`, iteration, the element numeric coercion gated on
   `added_element_var`) now includes them. The suite does not see this;
   the corpus does.

So the prerequisite is clean on the test suite and **expensive on the
program this was aimed at**, and the payoff it unlocks never fires. Both
flags stay off.

The measured target (109/172/117 partitions) is real, but reaching it
needs the monomorphic-group test applied to the grouping the splitter
actually uses, not to `clone.cc`'s equivalence classes — and it needs the
element sym without its current side effects on container-keyed paths.