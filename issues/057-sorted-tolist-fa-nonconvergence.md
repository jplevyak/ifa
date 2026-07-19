# 057 — Generic `sorted()` across differing element types + `list()`-materialization causes FA non-convergence

**Status:** open, found 2026-07-19 while testing the same-day
`dict.keys()`/`.values()`/`.items()` fix ([../../issues/025](../../issues/025-shedskin-examples-coverage.md)).
Not `dict`-specific and not caused by that fix — it's a pre-existing,
general FA convergence bug that a natural "exercise every code path"
test for the new dict methods happened to trip. Same *class* of bug
as [055](055-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
(FA's fixed-point loop churns worklists without bound while the
EntrySet count stays flat), but this repro is dramatically smaller —
4 lines, no `plcfrs.py`-scale program needed — making this the better
issue to use if someone picks up root-causing the underlying FA bug.
**Affects:** FA's fixed-point convergence (`ifa/analysis/fa.cc`'s
`analyze_to_convergence`) — same symptom location as 055, root cause
not yet isolated past the bisection below.

## Symptom

```python
d = {"a": "x", "b": "y", "c": "z"}
ks = sorted(["p", "q"])
items = list(d.items())
si = sorted(items)
```

`./pyc -D . repro.py` hangs (confirmed past 30s; not yet confirmed
whether it eventually crashes like 055's trivial-`__sub__` case did,
or spins forever — not run past 30s in this triage). Instrumented via
the same `PYC_DBG_PHASE` printf-bisection technique used for 055
(temporarily patched into `analyze_to_convergence`, removed after)
confirms the identical signature: `ess.n` (distinct EntrySets) flat
at 97 while edge/send/es-worklist pop counts climb linearly and
unboundedly within FA's first convergence pass:

```
PHASE:   pass=1 ess.n=97
PHASE:     edges=20000 ... ess.n=97
PHASE:     edges=40000 ... ess.n=97
PHASE:     edges=60000 ... ess.n=97
PHASE:     edges=80000 ... ess.n=97
PHASE:     edges=100000 ... ess.n=97
```

## What's known (isolated by bisection)

- **Needs three things together**: (1) a `sorted()` call on a plain
  `list[str]` (a literal like `sorted(["p", "q"])` — `sorted(d.keys())`
  also works, nothing dict-specific about *this* half), (2) a
  `list(d.items())` call (materializing a dict's items via
  `__pyc_tolist__`, i.e. touching `__dict_items_iter__`), and (3) a
  `sorted()` call on that materialized items list (`sorted(items)`)
  or directly on `d.items()` (`sorted(d.items())` — both trigger it
  equally; doesn't need two separate `.items()` call sites, a single
  `items = list(d.items())` reused for both `list()` and `sorted()`
  is sufficient).
- **Removing any one of the three makes it compile instantly
  (<1s)**: `sorted(str-list)` alone with `list(d.items())` (no
  `sorted()` on the items) — fast. `list(d.items())` +
  `sorted(d.items())` alone (no unrelated `sorted(str-list)`
  elsewhere) — fast. `sorted(d.keys())` + `list(d.keys())` (no
  `.items()`/tuples at all) — fast, confirms it's specifically about
  `sorted()` seeing *both* a `list[str]` shape and a `list[tuple]`
  shape (from `.items()`) in the same compiled program, not just
  "two `sorted()` calls" or "`.items()` used at all."
- **Not specific to `dict`** in the sense that matters: the `str`-typed
  half doesn't need to come from a dict at all (a bare list literal
  works identically to `sorted(d.keys())`) — this smells like a
  general "`sorted()`'s (or list-polymorphic-function-in-general's)
  per-element-type specialization doesn't converge when the same
  generic function sees sufficiently different element type shapes
  across call sites that also involve a `__pyc_tolist__`-driven
  list materialization" bug, not a dict-keys/values/items-specific
  one. Not confirmed whether `__pyc_tolist__`/`list()` is essential
  or just one way to reach the same shape (e.g. would `sorted()`
  directly on two different NON-dict-derived types, without any
  `list()` call, also hang? — not tested; time-boxed to the repro
  above once it reproduced cleanly).

## Why not fixed now

Root-causing *why* specifically requires deeper instrumentation
(dumping which AType/CreationSet is oscillating pass-over-pass) than
this triage pass had budget for — same limitation noted in 055. This
issue exists mainly to (a) record a much cheaper, cleaner repro of
the same underlying bug class than 055's `plcfrs.py`-scale one, and
(b) document a real landmine: **`sorted()` on both string and
dict-items()-derived data in the same program is a plausible, fairly
ordinary thing to write**, unlike 055's `set.__sub__`-on-a-500-line-
program trigger — so this is more likely to bite a real corpus
example than 055 was. Worth prioritizing over 055 if only one gets
picked up.

## Impact so far

None of the real shedskin corpus examples that motivated the
dict-methods fix (`loop.py`, `mastermind2.py`, `plcfrs.py`,
`sunfish.py`) hit this — each only does a single, simple, unsorted
`.keys()`/`.values()`/`.items()` access, not this specific
multi-type-`sorted()` combination. The committed test
(`tests/dict_items_keys_values.py`) was deliberately written to avoid
tripping this (see that file's own comment) rather than exercise it,
so it stays green. But any *future* corpus example (or user program)
combining `sorted()` on strings with `sorted()`/`list()` on
`dict.items()` will hit this exact hang.

## What this unblocks

Whatever program next combines these two ordinary patterns — likely
to recur. Also, since this is the same bug class as 055, whoever
fixes this fixes (or makes major progress toward) 055's `plcfrs.py`
blocker (`set.__sub__`) too, and vice versa.
