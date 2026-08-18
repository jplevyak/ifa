# 105 — type degeneration in shared generic container methods

**Status:** open, opened 2026-08-18 as the successor to
[104](closed/104-unify-list-and-tuple-in-analysis.md), whose motivating
problem turned out to be this one. Repro:
`tests/generic_method_type_degeneration.py` (passing — see below).

## Symptom

A variable in `plcfrs` holds, simultaneously:

```
bool, int64, str, float64, list, list, list, dict, list, ..., ChartItem,
Edge, Rule, Entry, list, ... + 25 tuple CreationSets of arities 2-4
```

Every one of `plcfrs`'s 2232 violations is a BOXING violation on a type
of roughly that shape. It is not a union any program wrote; it is the
analysis losing the distinctions.

## The engine: a local accumulator in a shared method

`IFA_DBG_DEGEN` (new) reports AVars whose type spans ≥ N distinct syms,
with the defining function and source line. On `plcfrs`, 2779 sites,
dominated by `__lt__` (360), `__eq__` (284), and `__add__` /
`__getitem__` at specific `__pyc__` lines.

`__pyc__/04_sequence.py`'s `list.__add__`, at the reported line:

```python
if isinstance(l, tuple):
  r = []
  for k in range(len(self)): r.append(self[k])
  for k in range(len(l)):    r.append(l[k])
  return r
```

**`r` merges the element types of both operands.** Unless FA gives
`__add__` a separate contour per element type, `r` becomes the union of
every list ever concatenated anywhere in the program — and that union
then flows back out through the return value into the callers' types.

The same shape applies to `__lt__`/`__eq__`/`__getitem__`: one generic
body, many receiver types, locals that merge them.

## Reproducer, and what it does *not* show

`tests/generic_method_type_degeneration.py` — 17 lines, six element types
(int, str, float, two user classes, nested list) through `+` and `sort`:

| program | degenerate AVars (`IFA_DBG_DEGEN=3`) |
|---|---|
| `print("hi")` | **0** |
| the same repro with **two** element types | **0** |
| **six element types** | **74** |
| six types behind a shared `def merge(a, b): return a + b` | **89** |

The degeneration is real, reproducible in 17 lines, and appears at
*exactly* the `__pyc__` lines `plcfrs` degenerates at.

**But it is not a failure.** The reproducer compiles, converges in 7
passes with **0 violations**, and matches CPython. So degeneration alone
is necessary-but-not-sufficient: something else in `plcfrs` turns 74
degenerate sites into 2232 violations and a compile failure.

That gap is the open question, and it is the right one to chase next —
candidates being scale (plcfrs has far more element types), recursion
through the degenerate methods, or the contour-count interaction from
[101](101-FA-first-time-forever-splitting.md).

## Why this matters

- It is the actual cause of `plcfrs`'s failure, after
  [104](closed/104-unify-list-and-tuple-in-analysis.md)'s mixed-arity
  theory was disproved.
- It is a *precision* metric that is invisible in test output, so nothing
  in the suite currently guards it. `IFA_DBG_DEGEN` plus the reproducer's
  74/89 give a number a fix can be measured against.
- It connects [018](../issues/018-dict-mixed-key-types-boxing-failure.md)
  (no representation for `{scalar, pointer}`) to
  [101](101-FA-first-time-forever-splitting.md) (contours not separating
  shared container methods): 018 is the *symptom*, 101 is the *mechanism*,
  and this is where they meet.

## Fix direction

Not boxing. If `__add__`'s contours separated per element type — which is
what shedskin gets free from `list<T>::__add__` being a template
instantiation — `r` would never hold more than one element type and no
boxing would be required. That is 101's splitting rule, and this issue
gives it a direct, cheap measurement.

## Verification plan

- `tests/generic_method_type_degeneration.py` keeps passing (it is an
  anchor, not a bug), and its `IFA_DBG_DEGEN=3` count drops from 74.
- `plcfrs`'s 2779 degenerate sites and 2232 violations drop together; if
  they move independently, the causal story here is wrong.
- Corpus: no exit-code **or run-status** changes
  (`ifa/issues/runstatus.sh` — see
  [102](102-corpus-programs-compile-then-abort-at-runtime.md)).
