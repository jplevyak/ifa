# 104 — unify `list` and `tuple` in analysis, specialize at implementation

**Status: CLOSED 2026-08-18.** The representation rule landed and is
correct; the problem it was built for turned out not to exist.

**What landed** (off by default, `PYC_TUPELEM` + `PYC_TUPLE_AS_LIST`):
tuples now follow `list`'s existing `get_sym_tup` rule — differing arity
or a populated generic element means list layout, all-same-arity with a
bottom element means record. Tuples had been excluded from it by nothing
but a hardcoded `sym == sym_tuple`. Removing that exclusion deleted 115
lines and works better than the machinery it replaced (−2.9 % generated C
on `plcfrs`, corpus-neutral, suite clean).

**Why it is closed rather than defaulted:** pyc already handles
mixed-arity homogeneous tuples correctly — six reproducer shapes all
compile and run matching CPython. The motivating measurement (2083 of
`plcfrs`'s 2232 violations "involve mixed-arity tuples") was a
**correlation artifact**: those violations are BOXING violations on
fully degenerated types containing bool, int, str, float, list, dict and
half a dozen user classes, with tuples of every arity as incidental
passengers. And the prerequisite `PYC_TUPELEM` costs `plcfrs` 2232 → 4353
violations by pulling tuples into every container-keyed path.

**Successor:** the real problem is type degeneration —
[105](105-type-degeneration-repro.md).

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

## Why the merge never fired — and the stage mismatch behind it (2026-08-17)

Two real blockers, both found and both fixed:

**1. Different arity was made non-equivalent unconditionally.**
`determine_basic_clones`:

```cpp
// if different number of instance variables
if (cs1->vars.n != cs2->vars.n) { make_not_equiv(cs1, cs2); continue; }
```

Two CreationSets of differing arity are separated **before element type is
ever consulted**, so no equivalence class could ever hold a multi-arity
monomorphic group. Relaxed via `list_form_compatible(cs1, cs2)` — both
monomorphic tuples, elements populated and agreeing — in which case arity
is not part of their type.

**2. `monomorphic_tuple` compared CreationSet identity.** It required
`sorted.n == 1` per field, which no real tuple satisfies: numeric
constants each get their own CreationSet, so a field holding `1` and `2`
has two CSs and one basic type. Changed to compare `basic_type(...)`,
the same comparison `determine_basic_clones` itself uses.

With both fixed the merge **fires** — `tuplist_groups=5` on `plcfrs`.

### But it cannot deliver the measured benefit, for a structural reason

`plcfrs`'s FA metrics are **byte-identical** with the merge firing
(violations 4353, `ess` 1213, `css` 3949 — exactly the `PYC_TUPELEM`-only
numbers). That is not a bug: **`clone.cc` runs after FA.** Violations,
`ess`, `css` and split counts are all FA-time; the representation choice
is post-FA and cannot move any of them.

So the target metric and the mechanism are at **different stages**. The
109/172/117 same-element-different-arity split partitions are an
FA-level count, and no post-FA representation choice can reduce them.
Choosing that metric for a `clone.cc` change was a mistake — it should
have been caught when the metric was picked, not after building all four
pieces.

What a post-FA merge *can* buy is fewer distinct generated types and
smaller output. Measured on `plcfrs`: it does not get that far —
`PYC_TUPELEM=1` alone emits 157 424 bytes of C and fails on the
[018](../issues/018-dict-mixed-key-types-boxing-failure.md) union, while
adding `PYC_TUPLE_AS_LIST=1` makes the **compiler abort (rc=134) with no
C at all**. The merge fires and then something downstream cannot handle
the merged type.

### Where this leaves it

Suite is clean in every configuration (273 / 0, default and both flags),
so nothing is broken by default. All four pieces are implemented and the
two real blockers are removed, which is genuine progress on the
mechanism. But:

- to reduce **splits**, the arity-independent type must exist **during
  FA**, not in `clone.cc` — a much larger change than this one;
- the post-FA merge, which is what was built, currently crashes the
  compiler on the one corpus program where it fires.

Both flags stay off. The next step is to debug the `plcfrs` abort under
`PYC_TUPLE_AS_LIST` (the merge fires 5 times, so the failing group is
findable via `IFA_DBG_TUPLIST`), and separately to decide whether an
FA-level arity-independent tuple type is worth pricing.

## The crash, root-caused and fixed (2026-08-17)

```
analysis/clone.cc:644: compute_member_types: Assertion `!n || n == cs->vars.n' failed.
```

`compute_member_types` builds `sym->has[i]` from `cs->vars[i]` across
every member of an equivalence class, so it **requires all members to
share one arity** — which the arity merge violates by construction.

The fix is the same principle that resolved piece 4: **a list-form group
has no indexed members.** Its type is described by its element alone,
exactly as a list's is.

```cpp
if (group_monomorphic_tuple(eqcss)) {
  sym->has.clear();
  // element = union of every member's fields, plus whatever use populated
  ...
  return 0;
}
```

No guard bolted onto the assert, no arity special-case — the group simply
takes the description a list-represented container should have.

### Result

| | before fix | after |
|---|---|---|
| `plcfrs` | **rc=134**, 0 bytes of C | rc=1, **155 078 bytes**, failing only on the pre-existing 018 union |
| suite, both flags | — | **273 passed / 0 failed** |
| corpus, 77 programs | — | **zero exit-code changes** |
| `plcfrs` C size | 157 424 (`TUPELEM` alone) | **155 078 (−1.5 %)** |
| `sudoku2` | 235 886 | 235 770 |
| `rdb`, `sudoku5`, `tictactoe` | — | byte-identical |

So the mechanism is complete and correct end to end, and the merge does
real work — just modest work.

## Final status: complete, correct, and not worth defaulting

All four pieces are implemented, both blockers to the merge firing are
removed, and the crash is fixed. What the flags buy, measured:

- **Payoff:** −1.5 % generated C on `plcfrs`, −116 bytes on `sudoku2`,
  nothing on the other 75 programs.
- **Cost:** `PYC_TUPELEM` — the prerequisite, not the merge — takes
  `plcfrs` from 2232 to 4353 violations and `ess` 850 → 1213, because
  giving `sym_tuple` an element makes every container-keyed path
  (`sizeof_element`, iteration, the `added_element_var` numeric coercion)
  include tuples.

**Both flags stay off.** The honest summary is that this line of work
produced a correct mechanism aimed at the wrong stage: the benefit that
motivated it (109/172/117 split partitions) is FA-level, and everything
built here runs *after* FA in `clone.cc`, so it can only ever affect
generated code. Reducing those splits requires an arity-independent tuple
type **during** FA, which is a substantially larger change and has not
been priced.

What is worth keeping from it: the mechanism itself (should an FA-level
version ever be built, the representation half is done and tested), the
three structural facts it uncovered — `sym_tuple` had no element sym;
differing arity was made non-equivalent before element type was
consulted; `monomorphic_tuple` must compare basic types, not CreationSet
identity — and the `IFA_DBG_TUPARITY` probe.
## How shedskin handles it: the choice is SYNTACTIC, by arity, before inference

Answering "does it start with all tuples in the same contour?" — not
quite, but the effect is stronger than that. **shedskin picks the
representation at graph-build time from the literal's arity alone**, so
the fixed-arity types this issue tries to merge are mostly never created.

`shedskin/graph.py`:

```python
def visit_Tuple(self, node, func=None):
    if len(node.elts) == 2:
        self.constructor(node, "tuple2", func)
    else:
        self.constructor(node, "tuple", func)
```

and `shedskin/python.py`'s `tvar_names`:

| class | type variables |
|---|---|
| `list`, `set`, `deque`, `array`, **`tuple`** | `["unit"]` |
| `dict` | `["unit", "value"]` |
| **`tuple2`** | `["first", "second"]` |

So **`tuple` has exactly one type variable, `unit` — the same shape as
`list`.** Every tuple of length ≠ 2 is variable-length homogeneous from
the moment it is built, before any inference runs. Two such tuples of
different arity and the same element type are therefore *literally the
same type from the start*; there is nothing to merge, ever.

Pairs are the special case, and they carry **both views at once**
(`graph.py`'s `constructor`):

```python
self.add_dynamic_constraint(node, elem0, "unit", func)    # union view
self.add_dynamic_constraint(node, elem1, "unit", func)
self.add_dynamic_constraint(node, elem0, "first", func)   # per-index view
self.add_dynamic_constraint(node, elem1, "second", func)
```

### The price: heterogeneous tuples longer than 2 are unsupported

That is an explicit, named limitation — `typestr.py:387`:

```
"tuple with length > 2 and different types of elements"
```

Verified directly:

| | shedskin |
|---|---|
| `(n, "two")` — heterogeneous **pair** | clean, no warnings |
| `(n, "two", 3.5)` — heterogeneous **triple** | `*WARNING* Variable 't' has dynamic (sub)type: {float, int, str}` + `*WARNING* tuple with length > 2 and different types of elements`, falls back to `pyobj *` |

### What this means for pyc

pyc is **more capable here**: it represents a heterogeneous N-tuple as a
record with per-field types, which shedskin simply cannot do
(`tests/tuple_mixed_types.py` pins this). That capability is precisely
what creates the problem this issue chased — pyc makes one fixed-arity
record type per arity, and then has to merge them back.

So the two viable directions are now clear, and neither is what was
built:

1. **shedskin's trade, adopted knowingly** — lower tuple literals of
   arity ≠ 2 to the list shape at `build_if1` time (arity is syntactic,
   so no inference is needed), accepting the same loss: heterogeneous
   N-tuples stop working. This makes different-arity tuples one type *by
   construction*, at the stage where it actually reduces splits.
2. **Keep the capability and decide later** — which is what
   `PYC_TUPLE_AS_LIST` does, and it is post-FA in `clone.cc`, so it can
   only shrink generated code, never split counts.

The post-FA merge already built is option 2 and is measured at −1.5 % C
on one program. Option 1 is the one that would move FA-level numbers, and
its cost is a capability regression, not an implementation risk.


## The list rule already IS the unified rule — tuples were just excluded

pyc's `list` already handles **both** cases this issue was trying to
build:

- *heterogeneous, fixed arity* → record layout with per-field types
  (`tuple_able`: generic element stayed bottom);
- *single type, mixed arity* → ordinary list layout.

And the rule that decides is `get_sym_tup`, which has been there all
along:

```cpp
if (n < 0) n = cs->vars.n;
else if (n != cs->vars.n) tup = false;   // differing arity -> NOT a record
tup = tup && cs->tuple_able;             // populated element -> NOT a record
```

That is exactly the wanted semantics, stated once. **Tuples were excluded
from it by nothing but the hardcoded `sym == sym_tuple`** in the two
branch conditions of `define_concrete_types`:

```cpp
if (sym != sym_tuple && sym != sym_closure && !tup) { /* no clone, share base sym */ }
...
if (sym == sym_tuple || sym == sym_closure || tup) { /* record */ }
```

So the whole apparatus built above — `monomorphic_tuple`,
`group_monomorphic_tuple`, `list_form_compatible`, the
`determine_basic_clones` arity relaxation, the `compute_member_types`
special case — was **reimplementing `tup`**. Replacing all of it with
"stop excluding tuples" is:

```
ifa/analysis/clone.cc | 15 insertions(+), 115 deletions(-)
```

and it works *better*: the list layout is chosen **25** times on `plcfrs`
against 5 before, and generated C drops to **152 921** bytes — against
155 078 for the complex version and 157 424 for `PYC_TUPELEM` alone
(−2.9 %).

Note also why `determine_basic_clones`'s arity guard never needed
relaxing: mixed-arity **lists** do not share an equivalence class either.
Both classes independently take the no-clone path and land on the same
base sym, so they are one type without ever being merged. That is why
mixed-arity lists have always worked, and it is the same reason tuples
now do.

### What the flags actually buy, restated

Mixed-arity tuples **already work by default** — `(n,n+1)` / `(n,n+1,n+2)`
through one variable, iterated and `len`-ed, matches CPython with no
flags. What `PYC_TUPLE_AS_LIST` changes is only the *representation*, and
therefore only generated code size (−2.9 % on `plcfrs`, nothing on
`rdb`/`sudoku5`). FA metrics are untouched, as they must be: `clone.cc`
runs after FA.

The cost remains `PYC_TUPELEM`, the prerequisite: giving `sym_tuple` an
element sym takes `plcfrs` from 2232 to 4353 violations by pulling tuples
into every container-keyed path. That, not the representation rule, is
what keeps both flags off by default.
## Do mixed-arity tuples cause real failures in the corpus? Yes — one program, decisively

`IFA_DBG_ARITYVIOL` asks the direct question: of the violations FA
actually records, how many are on an AVar whose type holds tuples of
**differing arity**?

| program | violations | with ≥2 tuples | **with MIXED ARITY** | of those, homogeneous |
|---|---|---|---|---|
| **`plcfrs`** | 2232 | 2083 | **2083 (93 %)** | **2016 (97 %)** |
| `sudoku5` | 180 | 53 | **0** | — |
| `rdb`, `go`, `linalg`, `chess`, `othello2`, `life` | — | 0 | **0** | — |

So mixed-arity tuples are **not** a corpus-wide problem. They are
`plcfrs`'s problem, and they are essentially *all* of it: **90 % of
`plcfrs`'s violations (2016 of 2232) are on unions of homogeneous tuples
that differ only in length** — precisely the case a variable-length
`tuple<T>` collapses to one type.

None of the corpus's other failures are arity-related. The compile
failures are all other families:

```
chess    mismatched field sizes: closure field mixes 1- and 8-byte ('__pyc_None_type__')
go       assigning to '_CG_int64' from incompatible type 'void'
linalg   no matching function for call to '_CG_list_mult_internal'
plcfrs   mismatched field sizes: closure field 'x' mixes 8- and 1-byte ('bool')
othello3 FA made no EntrySet progress for 120s
```

### The source, concretely

`plcfrs.py` is a grammar parser, and its rules are tuples of nonterminals
of *varying length*:

```python
(("S", "VP2", "VMFIN"), ((0, 1, 0),)),      # 3 strings
(("VP2", "VP2", "VAINF"), ((0,), (0, 1))),  # 3 strings
(("VP2", "VP2"),         ((0,), (0,))),     # 2 strings  <-- same element type, different arity
(("PROAV", "Epsilon"),   "Darueber"),       # 2 strings
```

Every rule LHS is a homogeneous `tuple<str>` of length 2 or 3, and the
inner `((0,1,0),)` / `((0,),(0,1))` are homogeneous tuples of int-tuples,
again of differing length. shedskin compiles this as `tuple<str *>` — one
type — which is why it has 86 `tuple<>` instantiations alongside its 110
`tuple2<>`.

### What this settles

The target is **real, measurable and concentrated**, which the earlier
proxy metrics never established. But it also confirms the stage
conclusion: these are **FA-time violations**, so the post-FA
representation work in `clone.cc` cannot touch them — and indeed the
flags produce zero exit-code changes across all 77 programs.

Fixing `plcfrs` needs the arity-independent tuple type to exist **during
FA** (option 1 above). The measured prize for that is now known: ~90 % of
one program's 2232 violations, in a program that is also one of the three
non-convergent ones. Nothing else in the corpus would benefit.

## CORRECTION: the mixed-arity finding above is a correlation artifact

Trying to build a minimal reproducer disproved the previous section. Six
shapes were tried, **all of which pyc compiles and runs correctly**,
matching CPython, with **zero** violations:

| shape | result |
|---|---|
| bare union `("S","VP2","VMFIN")` / `("VP2","VP2")`, iterated + `len` | ✅ |
| list of mixed-arity tuples, iterated and indexed | ✅ |
| plcfrs's nested shape `(tuple-of-str, tuple-of-int-tuples)` | ✅ |
| `tuple(a[: len(a)-1])` — arity not statically known | ✅ |
| mixed-arity tuples as **dict keys** + across a function boundary | ✅ |
| nested, dict-keyed, four distinct arities | ✅ |

**pyc handles mixed-arity homogeneous tuples.** That is why no repro
could be built: there is no bug to reproduce.

Printing the *rest* of each violating type shows what those violations
actually are:

```
[aritywhere] kind=6(BOXING) arities=4,4,2,3,3,4,2,3,... others=
   bool,int64,str,float64,list,list,list,dict,list,...,ChartItem,Edge,Rule,Entry,...
```

The type is not "tuples of differing arity" — it is a **fully degenerated
union of everything in the program**: bool, int, str, float, list, dict
and half a dozen user classes, *plus* 25 tuple CreationSets that happen
to span arities 2–4. The BOXING violation is caused by mixing scalars
with pointers — [018](../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md) — and the tuples
are incidental passengers.

`with_multi_tuple=2083` equalling `WITH_MIXED_ARITY=2083` should have been
the tell: it is not that mixed arity accompanies every tuple violation
because arity matters, but that plcfrs has a handful of catastrophically
degenerate types which appear in thousands of violations and contain
*everything*, tuples of every arity included.

So the corrected answer to "do mixed-arity tuples cause failures in the
corpus": **no.** Not in plcfrs, not anywhere. The measurement was real;
the causal reading of it was wrong.

### What a minimal repro should target instead

Not arity. plcfrs's real problem is **type degeneration** — how a
variable comes to hold `{bool, int64, str, float64, list, dict,
ChartItem, Edge, Rule, Entry, …}` at all. That is the 018/030 boxing
family compounded by [101](101-FA-first-time-forever-splitting.md)'s
contour explosion, and a reproducer for *that* would be worth having.

### Status of this issue

Closeable as a line of work. The representation rule landed (tuples now
follow `list`'s `get_sym_tup` rule when `PYC_TUPLE_AS_LIST` is on, 115
lines deleted), it is correct and corpus-neutral, and it buys −2.9 %
generated C on one program. The motivating problem it was built for does
not exist.
