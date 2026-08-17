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