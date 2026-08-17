# 104 — unify `list` and `tuple` in analysis, specialize at implementation

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
