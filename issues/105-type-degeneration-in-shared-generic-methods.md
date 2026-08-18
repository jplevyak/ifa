# 105 — type degeneration in shared generic container methods

**Status:** open, opened 2026-08-18 as the successor to
[104](closed/104-unify-list-and-tuple-in-analysis.md).

**Two reproducers, and only the second one matters:**

| test | what it does |
|---|---|
| `tests/container_scalar_union_add.py` | **reproduces the failure** — three lines, `plcfrs`'s exact compile error. `.known_issue`. |
| `tests/generic_method_type_degeneration.py` | reproduces *degeneration metrics* but **passes cleanly** — see the correction below |

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


## CORRECTION: the degeneration reproducer is not a failure reproducer

A program that type-checks cleanly has, by definition, no type problem —
so `tests/generic_method_type_degeneration.py` (74 degenerate AVars, 0
violations, correct output) does **not** reproduce what breaks `plcfrs`.
Its 74 "degenerate sites" are unions FA went on to resolve. The metric
measures something real but not, on its own, harmful.

### The actual failure, in three lines

```python
import sys
x = [1] if len(sys.argv) > 1 else 3.5
print(x + x)
```

```
internal: sizeof_element of non-container type '...' (in __add__)
-- FA specialized a container method against a scalar
```

That is `plcfrs`'s compile failure exactly, in a program with no grammar,
no tuples, no recursion and no scale. CPython prints `7.0`.

### The essential ingredient: the union must form in ONE VARIABLE

The same two types reaching `__add__` through **separate call sites**
compile and run correctly:

```python
def add(a, b): return a + b
add([1, 2], [3])    # fine
add(3.5, 1.5)       # fine
```

So this is not "pyc cannot mix `list` and `float`". FA separates those
into distinct contours without difficulty. What defeats it is the
**branch merge**: once one variable holds `{list, float}`, every use of
it is a use of the union, and `+` resolves to a container method whose
receiver may be a scalar.

### What that means for this issue's theory

The "shared generic method with a local accumulator" story is a real
mechanism — `__add__`'s `r` does merge both operands' element types, and
the degenerate sites are at exactly the `__pyc__` lines `plcfrs` shows.
But it is **not** what makes `plcfrs` fail. The failure needs no
accumulator and no sharing at all; it needs a single variable holding a
container and a scalar.

So `plcfrs`'s 2779 degenerate sites are, most likely, *downstream* of
branch-merged container/scalar unions rather than their cause — the same
correlation trap that [104](closed/104-unify-list-and-tuple-in-analysis.md)
fell into. **Do not assume the degeneration metric points at the fix
until that direction is established**, e.g. by finding the branch merges
in `plcfrs` and checking whether the degenerate sites disappear when they
are typed apart.

### Revised fix direction

This is [018](../issues/018-dict-mixed-key-types-boxing-failure.md) — a
`{scalar, pointer}` union with no representation — reached through a
control-flow merge rather than a container. Either:

1. **Represent it** (018/030: boxing or a fat pointer), which is the
   general answer and is unsolved; or
2. **Split the merge**: give the variable a per-branch contour so the
   union never forms. That is [101](101-FA-first-time-forever-splitting.md)
   again, and it is what makes the separate-call-site version work
   already.

Option 2 is attractive precisely because the working `add()` version
proves FA can already do it — the question is why the branch merge is
treated differently from the call-site split.


## CORRECTION 2: the three-line repro is not plcfrs's shape either

The decisive test — *does shedskin handle it?* — says no:

```
$ shedskin translate f3.py
*WARNING* Variable 'x' has dynamic (sub)type: {float, list}
$ make
f3.cpp:18:51: error: expected type-specifier before '__ss_floa'
```

**shedskin rejects the three-liner too** — its generated C++ does not even
compile. And shedskin compiles `plcfrs`. Therefore **`plcfrs` does not
contain a branch-merged container/scalar union**, and the three-line test
reproduces the *message* but not the *situation*. It has been re-labelled
as a plain [018](../issues/018-dict-mixed-key-types-boxing-failure.md)
instance.

That is the second time in this issue, and the third across
[104](closed/104-unify-list-and-tuple-in-analysis.md) + 105, that a
constructed reproducer matched a symptom without matching the cause.

## What plcfrs's failure actually is

```
mismatched field members: list(8) list(8) tuple(8) bool(1) tuple(8) ...
  def: <anon> __pyc__.py:1446           <-- `def len(x): return x.__len__()`
fail: mismatched field sizes: class 'closure' field 'x' mixes 8- and 1-byte members ('bool')
```

and the warnings point at:

```
plcfrs.py:605  batch(argv[1], argv[2], argv[3])
plcfrs.py:637  % argv[0]
```

with type `( list tuple bool int64 float64 str dict ChartItem Edge Rule
Entry )` — i.e. **`sys.argv`'s elements have acquired the type of
everything in the program**, and the closure field for `len`'s parameter
`x` then cannot be laid out because `bool` is 1 byte and the rest are 8.

`pyc_lib/sys.py` builds `argv` by appending only `str`, so this union is
**manufactured by the analysis**, not written by the program. That is the
real defect and it is pyc-specific — exactly as the question implied.

### Constructed reproducers that do NOT trigger it

All of these compile and run correctly, so none is the mechanism:

| hypothesis | result |
|---|---|
| `sys.argv` contaminated by other list types in the program | ✅ clean |
| `len()` called on 7 different types including `bool` | ✅ clean |
| `len` passed as a **value** to `map` (plcfrs.py:134) *and* called directly | ✅ clean |
| six element types through `+`/`sort` (74 degenerate AVars) | ✅ clean, 0 violations |
| branch-merged `{list, float}` | ✗ fails — but shedskin fails it too |

## Revised approach: reduce, do not construct

Five constructed hypotheses have now failed to reproduce it. The failure
is emergent at `plcfrs`'s scale, so the productive move is **delta
reduction from `plcfrs` itself** — cut the program down while the
`mismatched field sizes` failure persists — rather than more guessing
from the symptom.

Concretely: `plcfrs.py` is 640 lines; bisect it against the invariant
"still fails with `closure field 'x' mixes 8- and 1-byte members`". The
`argv` trail suggests starting by cutting `main`/`batch`/argument
handling, since that is where the degenerate type surfaces.

That reduction is the next task, and it should be done before any further
theory.
