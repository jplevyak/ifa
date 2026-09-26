# 169 — constants leak into ES split grouping through the dispatch filter: every split fans per literal

**Status: CLOSED 2026-09-26** with
[151](../151-split-an-entryset-on-a-constant-argument-on-demand.md)'s demand
stage. `split_type_view` keeps constants only at a formal that wants them
(annotated, or demanded). The two regressions below were fixed at their
causes, not by keeping the leak: `hq2x` by making ESBLOCK's split durable
and demand-gated, `pylife` by the demand walk climbing to the `__new__`
wrapper where its literals meet. The TypeError lowering described for
`pylife` is still the better answer (2 contours, as shedskin) and is not
built. Root-caused 2026-09-25. Found by counting unnecessary
contours on small programs against a hand-derived minimum and against
shedskin, as a speed metric: every contour is analysis time.

## Symptom — five lines, measured against shedskin

```python
def g(a, k):
    return a * k

print(g(1, 5), g(2, 6), g(3, 7), g(4, 8))
print(g(1.5, 5), g(2.5, 6))
```

| | contours of `g` |
| --- | --- |
| minimum (`a` is `int` or `float`; `k` is `int`) | **2** |
| shedskin (`func.cp`) | **2** |
| pyc default | **6** -- one per call site |

`IFA_DBG_FUNES=g`: six contours, every one with one in-edge, four of them
typed `[int64][int64]` and two `[float64][int64]`. The analysis needed
`a` split in two; it split `g` per CALLER, because every caller passes a
different literal. With the fix probe below, `g` has 2 contours and the
output is unchanged.

## Root cause

`split_type_view` (`fa.cc:1260`) is the type the splitter compares edges
by. Its own comment says constants must NOT partition -- *"partitioning on
that is clone-per-constant (survey B5)"* -- and `a->out->type` does strip
them: `make_AType` (`fa_lattice.cc:186-199`) projects `{const 5}` to
`{int64}`.

The constant comes back one line later. The view is
`type_intersection(a->out->type, e->match->formal_filters.get(p))`, and the
formal filter is the Matcher's dispatch filter (`pattern.cc:571`
`set_filters`), built from the RAW argument CreationSets -- `{const 5}`.
`type_intersection` keeps the subtype (`fa_lattice.cc:345-362`), so
`{int64} ∩ {const 5} = {const 5}`. Two effects:

1. **Grouping** (`edge_type_compatible_with_edge`, `fa.cc:1268`) compares
   EVERY positional argument, not just the confluence being split. Two
   edges that agree at the confluence but pass different literals elsewhere
   are incompatible, so any split of an EntrySet fans it per literal
   combination. Nothing asked for that -- constant differences do not START
   a split (`collect_type_confluence` strips them for unannotated formals,
   which is why ifa/151's `g(False)`/`g(True)` never separate), but they
   SHAPE every split that starts for another reason.
2. **ES compatibility** (`edge_type_compatible_with_entry_set`, `fa.cc:1300`)
   compares the edge's filtered view (constant kept) against
   `split_type_view(es_arg, nullptr)` (constant stripped), so `{const 5}`
   vs `{int64}` reads as incompatible and every literal-passing edge is
   `compat=0` against its own contour (`IFA_DBG_DECIDE`).

This is provenance by the back door: which literal a caller wrote is where
a value came from, not what type it is. It is 1-CFA for any function whose
callers pass literals, and it fires only when some other demand splits the
function, so it looks demand-driven.

## Census: where the excess on a small program comes from

```python
def f(p):
    return p[0] + p[1]
a = (1, 2); b = (3, 4)
print(f(a), f(b))
pts = [(i, i * 2) for i in range(3)]
s = 0
for q in pts:
    s += q[0] * q[1]
print(s)
```

pyc reaches 69 functions with **84 contours -- 15 over one per function**.
Every tuple is `(int, int)`; shedskin gives `f` **1** contour and `tuple2`
**2** live data contours. All 15 are accounted for by three mechanisms:

| mechanism | excess here | tracked |
| --- | --- | --- |
| A. `__pyc_clone_constants__` forced per-constant contours on `int.__add__/__mul__/__iadd__/__ge__/__str__`, `__pyc_to_bool__`, and `tuple.__getitem__`'s key | 9, + half of `__getitem__`'s 5 | [134](../134-remove-the-frontend-forced-split-opt-in.md) |
| B. **this issue** -- the leak above | masked by A here; 4 of 6 in `g` | 169 |
| C. one tuple CreationSet per literal site (the mode-2 tuple exemption, `fa.cc:570-611`) | `f` +1, `__getitem__` ×3 | [128](../128-cs-identity-over-discriminates-vs-element-type.md) |

Under `PYC_NO_FORCED_SPLIT=1` (A off) the probe for B takes
`tuple.__getitem__` 6 -> 3; the remaining 3 are C.

## Measurement -- probe `PYC_XP_STRIPC=1`

Take `->type` of the filtered view (strip constants again), except at a
formal whose sym is `clone_for_constants` so A still works:

```cpp
static AType *split_type_view(AVar *a, AType *filter, bool keep_consts = true) {
  AType *t = type_intersection(a->out->type, filter);
  if (xp_stripc() && !keep_consts) t = t->type;
  ...
// callers: keep_consts = es->args.get(p)->var->sym->clone_for_constants
```

`./test_pyc.py`: 322 / 0 / 23 known, unchanged. Corpus `-m check`, one
binary, both arms (`sweeps/check__default__63888fd6+7e402769.tsv` vs
`sweeps/check__PYC_XP_STRIPC_1__63888fd6+7e402769.tsv`):

| | default | `PYC_XP_STRIPC=1` |
| --- | --- | --- |
| ess, 76 programs (excl. `hq2x`) | 31896 | **30476 (-4.5%)** |
| programs with fewer / more ess | | **65 / 1** (`plcfrs` +61) |
| compile seconds, same 76 | 1673 | **1612 (-3.6%)** |
| largest drops | | `rdb` -128, `linalg` -124, `msp_ss` -116, `softrender` -78 |

Three verdict changes -- two regressions, both root-caused below, neither a
reason to keep the leak:

- `quameon`: compile-fail -> compiles (then aborts 134; it failed before).
- **`pylife`**: compiles -> `expression has no type` at `pylife.py:37`.
- **`hq2x`**: compiles in 61 s and matches CPython -> compiles in **383 s**
  (re-taken alone, rc=0), hitting the 101-pass cap with 0 violations.

### `pylife` -- the leak was hiding a fatal bottom on a path CPython raises on

```python
def __init__(self, board, id, children):
    if id <= 1: ...                                   # leaves
    else: nw, ne, sw, se = children                   # line 34
E = LifeNode(self, 0, None); X = LifeNode(self, 1, None)
```

At the default, `0` and `1` leak into separate contours (pyc has **4**
contours of `LifeNode.__init__`), so `id <= 1` folds and the `else` arm is
dead where `children` is `None`. With the leak closed the two `None` calls
correctly share a contour, `id` is `{0, 1}`, the one-constant cap widens it
to `int64` (`PYC_CONSTCAP=2` does not help -- the fold needs a single
constant CreationSet), and the unpack of `None` is reported as a fatal
`expression has no type`.

**shedskin shows the constant split is NOT the answer** (measured
2026-09-25, `func.cp` of `LifeNode.__init__`):

```
CART [('LifeBoard', 1), ('int_', 0), ('none', 0)]
CART [('LifeBoard', 1), ('int_', 0), ('list', 5)]
```

**2 contours, split on `children` alone; `id` is plain `int` and
`id <= 1` never folds.** It emits one C++ function,
`__init__(LifeBoard*, __ss_int id, list<LifeNode*>* children)`, with
`None` as a null `list*`; the `else` arm is compiled for both, and the
`None` contour's unpack contributes no types, which is harmless because the
variables are typed from the union over contours. The arm is dead at run
time.

So the minimum is 2 and pyc's gap is not a missing split. In the merged
`None` contour, unpacking `None` is a guaranteed CPython `TypeError`: the
faithful lowering is a raise, after which the path is dead and a bottom
type on it is not an error. pyc instead makes that bottom fatal. This is
the mirror image of issues/165 (a `None` reaching an operation is silently
read as zero instead of raising). An earlier draft of this section said
`pylife` needed [151](../151-split-an-entryset-on-a-constant-argument-on-demand.md)'s
per-constant split; that would buy 4 contours where 2 suffice.

### `hq2x` -- an ESBLOCK split the binding test cannot see

From pass 9 `CS_DEF_PART` reports `det=1 reuse=1` every pass with ess/css
frozen (601/2116) for 92 passes. `IFA_DBG_CSDEFSPLIT`:

```
[esblock] p=3 cs=2635 BLOCKER es=558 fun=append ... SPLIT edges=2 -> 2 group(s) by creation point
[esblock] p=4 cs=2635 BLOCKER es=612 fun=append ... SPLIT edges=2 -> 2 group(s) by creation point
[esblock] p=5 cs=2635 BLOCKER es=558 ...   (alternates to the cap)
```

`split_blocking_es` separates `append`'s two edges by CREATION POINT and
reuses the other contour; next pass `entry_set_compatibility` sees two
edges with identical types and re-binds them together, so the blocker
flips between es=558 and es=612 forever. At the default the edges carried
different constants, which kept them apart and hid it. The split is keyed
on something the compatibility test does not record -- 136's identity vs
compatibility, and 074's "a re-derived separation must re-attach". A
split stage that returns progress while nothing changes also starves
every stage behind it (ifa/055, 157).

## Second census: `voronoi` (2026-09-25)

`shedskin_examples/voronoi`: 57 lines, compiles, matches CPython, no
`None` mixed with scalars. Counted with a per-Fun histogram of `fa->ess`
that also counts each Fun's DISTINCT argument signatures at three levels:
exact (CreationSet ids, constants stripped), by class, and by SHAPE
(container = class + element types, tuple = per-position types). The
shape count is the minimum -- slightly low for objects with fields, such
as list iterators over different element types.

| | contours |
| --- | --- |
| pyc default | **213** (104 Funs) |
| distinct exact signatures | 152 |
| **minimum (by shape)** | **120** |

shedskin agrees where comparable: 3 live `list` contours (`list[str]`,
`list[tuple]`, `list[int]`), `list.append` 2, `float.__mul__` 1,
`int.__mul__` 1 (pyc: 5 element-bearing list CreationSets, `append` 6,
`float.__mul__` 4, `int.__mul__` 6). Its `random` and `int` operators are
native C++, so its total (45) is not comparable.

The 93 excess, by mechanism:

| mechanism | contours | example |
| --- | --- | --- |
| A, forced constants ([134](../134-remove-the-frontend-forced-split-opt-in.md)) | ~37 | `int.__rshift__` 7 (one per shift amount in `random`), `int.__mul__` 6 (one per multiplier); each `range(10/40/80/500)` its own CreationSet, so the range iterator methods have 5 contours each |
| **B, this issue** | **17** | `append(s)` vs `append("\n")` are separate contours, so the SETTER stage gives `screen` its own `list[str]` apart from `line` -- a DATA contour caused by the leak |
| C, one tuple CreationSet per literal site | several | `tuple.__getitem__` 8 = 4 tuple CSs x 2 index constants |
| D, pass-0 contours never re-merged ([170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md)) | 16 | `int.__add__` es=124 and es=125 both `[int64][int64]` |

| arm | contours | diagnostics |
| --- | --- | --- |
| default | 213 | 0 |
| constant-strip probe (B fixed) | **196** | 0 -- output matches CPython, run time unchanged (100 s) |
| `PYC_NO_FORCED_SPLIT=1` (A off) | 176 | 15 |
| both | 156 | 9 |

With B fixed, `line` and `screen` share one `list[str]` CreationSet (2
creation points).

## Fix, in order

1. Make ESBLOCK's separation durable -- the binding must see what the split
   decided -- or stop reporting progress on a reuse that re-merges
   (`hq2x`).
2. Lower an operation whose operand is exactly `None` (here: unpacking
   it) as the `TypeError` CPython raises, so the bottom that follows is dead
   code rather than a fatal "no type" (`pylife`; minimum 2 contours, as
   shedskin).
3. Strip constants in the split view at unannotated formals (the probe).

## Verification plan

- `g` above: 2 contours, same output. Count with a per-Fun histogram of
  `fa->ess` (the probe used was `IFA_DBG_ESHIST`, printing
  `count name file:line` per Fun from `report_fun_entry_sets`).
- `./test_pyc.py` unchanged on both backends.
- Corpus `-m check`: ess and compile time down as above; `pylife` compiles with 2 contours of `LifeNode.__init__`, and `hq2x`
  keeps its default verdict and 61 s compile.

## What this unblocks

About 4.5% of all contours corpus-wide, and the 1-CFA-by-literal fan on
every function called with constants. It also removes a precision source
that masks real defects -- the two regressions above are both bugs the
leak was hiding.
