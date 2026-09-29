# 168 — the recursive-edge overlap gate refuses the split that would remove its own overlap

**Status: closed 2026-09-28.** The targeted gate landed as `PYC_RECGATE`
(default 1). See *RESOLVED* at the end, and
[074](074-FA-cross-pass-oscillation-plan.md)'s resolution for the numbers.
The 4-line self-cycle residual moved to
[176](176-FA-self-feeding-deepcopy-fusion-has-one-assign-set.md).
Root-caused 2026-09-25. Owns the TYPING half of
`tests/deepcopy_recursive_nested_growth.py`; the non-convergence half
stays with [074](074-FA-cross-pass-oscillation-plan.md).

## Symptom

```python
import copy
M = [[1.0], [2.0]]
M = copy.deepcopy(M)
print(M[0][0])
```

```
nr3.py:4:49: error: illegal call argument type expression illegal: float64
fail: program does not type
```

The program is monomorphic: `list[list[float]]` throughout. The same
failure is what `tests/deepcopy_recursive_nested_growth.py` reports as
`illegal primitive argument type 'x' illegal: list` at `M[0][0]`, and a
non-recursive `while len(M) > 0: s += M[0][0]; M = shrink(M)` fails
identically. **Recursion in the user program is not needed; ifa/074's
2026-09-25 note should be read with this in mind.** What is needed is
for a deep copy to flow back into `deepcopy`'s argument (the global `M`
reassigned, `M = shrink(M)`, or `total(shrink(M))`). `M1 =
copy.deepcopy(M)` with `M` left alone types correctly.

Not the merged start: identical under `PYC_CSDCPA1=0`, `1` and `2`.

## The chain

Measured with `IFA_DBG_FUNES=__deepcopy__`, `IFA_DBG_DECIDE=__deepcopy__`,
`IFA_DBG_ELEMCONF`, `IFA_DBG_CSDEFSPLIT` on the 4-line program.

1. **The merged contour.** `list.__deepcopy__`
   (`__pyc__/04_sequence.py`, `r = []` / `r.append(self[k].__deepcopy__())`)
   has ONE list EntrySet, es=46, with `self = {list#1009 inner,
   list#1010 outer, list#1045}`. cs=1045 is its `r = []`, so the copy of
   the outer list and the copies of the inner lists are one contour:

   ```
   ELEMCONF FUSED cs=1045 sym=list defs=1 classes=2: float64 list
   ```

2. **The split that would separate them is visible in the types.**
   es=46's two in-edges disagree at `self`:

   ```
   DECIDE p=3 es=46 nedges=2 es_type: list#1009 list#1010 list#1045
      edge from_es=44 ety: list#1010 list#1045     (deepcopy)
      edge from_es=46 ety: list#1009 list#1045     (the recursive call)
   DECIDE-OUT p=3 es=46 do=0 stay=1 groups=0
   ```

3. **The gate drops the recursive edge.** `decide_entry_set_split`
   (`fa.cc:5447-5530` at `63888fd6`) admits a recursive edge into the
   grouping only if its type at the position is IDENTICAL TO or DISJOINT
   FROM every other edge's. `{1009, 1045}` vs `{1010, 1045}` partially
   overlap, so the recursive edge is skipped, the lone non-recursive edge
   is "single group exhausted", and `groups=0` -- on every pass.

4. **The overlap is the merge's own product.** 1045 is on both edges only
   because es=46 is shared: the outer copy flows back into `deepcopy`
   (edge 45), and because outer and inner copies are one CreationSet,
   1045's element is 1045, so the recursive call carries it too (edge 72).
   The overlap exists from pass 0 (as `list#1014`, the one-per-sym start;
   under `PYC_CSDCPA1=0` as the `r = []` site's single contour), so the
   analysis never leaves the fused state.

5. **Nothing downstream can act.** CS_DEF_PART declines cs=1045 as
   `DECLINED (single creation point)`. The ifa/152 backtrack finds no
   upstream CreationSet with >=2 defs -- the merge feeds itself. The
   `defs==1` rungs that split the owning EntrySet (`PYC_CSCALLSITE=1`,
   `PYC_ESDEFS1=1 PYC_VIOLCS=3`) are off by default and change nothing
   when turned on. `IFA_DBG_THIRD` reports es=46 `splittable=1` -- the
   type split exists, only the gate stands in front of it.

This is [142](142-linalg-empty-list-collapse-is-a-fixed-point.md)'s shape
(a CreationSet whose element contains itself) reached through an
EntrySet instead of through many creation points.

## Why the gate exists, and whether that still holds

Added in `251c4175` (2026-07-17, issues/025 R1 item 5) together with
recursive edges joining type grouping at all. Its recorded reason is
`tests/expr_evaluator.py`: same-shape recursion over one union
(`{Expr#1, Expr#2, None}` vs a caller's `{Expr#2}`) re-derived forever
and fanned single call sites across contours runtime dispatch could not
discriminate, regressing that test "both ways".

**That regression no longer reproduces.** With the gate relaxed,
`tests/expr_evaluator.py` compiles with 0 diagnostics and its output
matches `.exec.check`.

## Measurement (probe: `PYC_RECOVERLAP=1` admits the overlapping edge)

| program | as shipped | `PYC_RECOVERLAP=1` |
|---|---|---|
| 4-line reassign | violation | types; `__deepcopy__` = float + 2 list contours (074's target); prints `1.0`, converges p4 |
| `while` + `shrink` | violation | types; prints `6.0`, converges p4 |
| `tests/deepcopy_recursive_nested_growth.py` (guards off) | 24 violations at p101 | **types**, prints `6.0`; still runs to the p101 cap (074) |
| `tests/expr_evaluator.py` | clean | clean, output matches |
| `./test_pyc.py` (C) | 322 / 0 / 23 known | 322 / 0 / 23 known |

### Corpus `-m check`, one binary, both arms (2026-09-25)

`sweeps/check__default__63888fd6+25996bae.tsv` vs
`sweeps/check__PYC_RECOVERLAP_1__63888fd6+25996bae.tsv` -- same build,
the probe patched in and off by default, so the default arm is HEAD's
analysis.

| | default | `PYC_RECOVERLAP=1` |
|---|---|---|
| compile_fail | 32 | 32 (same set) |
| run_fail / stdout_differs | 13 / 6 | 12 / 7 |
| cs/shapes | 2287/642 = 3.56 | 2208/642 = 3.44 |

**No program that matched CPython stops matching** (`othello` stays
`yes`). Nothing starts compiling. The two verdict flips are not the
change:

- `tonyjpegdecoder` run_fail -> stdout_differs: ess/css identical in both
  arms; re-run alone, both binaries take ~120 s (the cap) and print
  byte-identical output apart from `TIME`, both differing from CPython in
  `converted <instance>` (a `repr` gap, not this).
- `softrender` compile rc 1 -> 124: **real**, re-taken alone -- compile
  time **31 s -> 693 s**, failing the same way (301 vs 302 errors).

Where the analysis moves (compile-fail programs, by error count):

| program | default | `PYC_RECOVERLAP=1` |
|---|---|---|
| `linalg` (this reproducer's origin) | ess 1257, container CSs 143, **44 errors** | ess **732**, container CSs **54**, **23 errors** |
| `sudoku5` | ess 674, 24 errors, 22 s | ess **1045**, **328 errors**, 115 s |
| `softrender` | 31 s | **693 s** |
| `pylife` (run_fail both) | ess 508, css 1402 | ess 649, css 1904 |

**So deleting the gate is NOT the fix.** `sudoku5`'s new errors are
`'x' has mixed basic types:( tuple int64 str )` centred on `solve`
(`sudoku5.py:43`), a recursive generator -- the fan-out the gate was
written against (`expr_evaluator`'s "re-derives forever, fans single call
sites"), now reproduced on a corpus program. `softrender` and `pylife`
are the same growth without a verdict change. `linalg` is the case this
issue is about, and it improves sharply.

The probe was one line, reverted after measuring:

```cpp
// decide_entry_set_split, the recursive-edge separability loop
if (type_intersection(ety, oty) != fa->type_world.bottom_type && !getenv("PYC_RECOVERLAP")) {
  separable = false;
```

## What the fix has to decide

Dropping the identical-or-disjoint test outright fixes this reproducer
and `linalg`'s share of it, and is measured wrong for `sudoku5` /
`softrender` (above). "No partial overlap" is a proxy for "this
recursion is not level-descending", and the replacement has to say what
it actually tests: it must admit es=46's recursive edge and still refuse
`sudoku5`'s `solve`. The candidate this
issue points at: the overlapping members here are CreationSets minted
INSIDE the contour being decided (es=46 owns 1045's only creation
point), so the overlap is a consequence of not splitting, not evidence
against it. Whether that is a type-shaped statement and not provenance
(ifa/146's two questions) must be argued before building on it -- the
demand is the irrepresentable element of cs=1045, and the edge types
alone decide which parts. A second shape worth measuring: gate the
admission on that demand -- admit an overlapping recursive edge only for
an EntrySet that owns the single creation point of a CreationSet
CS_DEF_PART declined -- which by construction cannot fire on a
recursion whose contours carry no such demand. **Stop condition, written
before measuring:** if either version still regresses `sudoku5` or
`softrender` by the amounts above, the premise that `solve`'s overlap
differs in kind from es=46's is wrong, and the next step is to find what
`solve`'s overlapping members are, not to tune the gate.

## Verification plan

- The 4-line program and the `while` variant above type, run, and print
  CPython's output.
- `tests/deepcopy_recursive_nested_growth.py` reports no violations
  (its `.known_issue` then stands only on 074's `CONVERGED=0`).
- `tests/expr_evaluator.py` stays clean; `make test` green.
- Corpus `-m check`: no program that matched CPython stops matching,
  AND `sudoku5` (24 errors, 22 s) / `softrender` (31 s compile) do not
  regress -- the blunt probe passes the first test and fails the second.

## What this unblocks

The typing half of 074's minimal reproducer, and with it the ability to
measure 074's contour growth on a program that types; any corpus program
that deep-copies (or rebuilds through a recursive container method) a
nested container and feeds the copy back in -- `linalg`'s
`determinant`/`Minor` pair is where the reproducer was distilled from.

## Re-measured 2026-09-27 (tree `c0327b84`): the blockers moved, and 074 is now the wall

`shedskin_examples/linalg`'s whole failure is this issue: replacing
`Minor`'s `copy.deepcopy(M)` with `[[x for x in v] for v in M]` takes it
from 46 errors to **0**. (It had 186 before list ordering landed,
issues/122.)

**The blunt probe no longer regresses sudoku5 or softrender.** Admitting
every partially overlapping recursive edge (`PYC_RECOVERLAP=1`), with the
programs that ruled it out re-taken on today's tree:

| program | as shipped | probe |
|---|---|---|
| 4-line reproducer | 1 error | types, prints `1.0` |
| `sudoku5` | 0 errors, 9.9 s | 0 errors, 10.3 s |
| `softrender` | 8 errors, 13.6 s | 8 errors, 13.4 s |
| `pylife` | compiles, 9.8 s | compiles, 19.6 s |
| `linalg` | **46** errors | **106** errors |

sudoku5's regression came from its recursive generator binding its own
coroutine body; that was fixed at `752544ed`. But the probe now splits
linalg's other recursions (`binary`, `determinant`, `sign`), the fan-out
the gate exists for.

**The targeted version was built and measured:** admit the overlap only
when the contour being decided owns the single creation point of a
CreationSet whose element is irrepresentable. It fires for
`__deepcopy__` and nothing else. The reproducer types; sudoku5 (0 errors,
10.2 s) and softrender (8 errors, 13.6 s) are unchanged, and pylife
compiles in 12.8 s. **linalg goes 46 -> 88 errors, and the cause is
[074](074-FA-cross-pass-oscillation-plan.md)'s growth, not a mis-split:**
`list.__deepcopy__` reaches **109 contours**, still splitting at pass 74 of
76. Each `__deepcopy__` contour mints its own `r = []`; `Minor` returns
that copy, `determinant` passes it back into `Minor`, `Minor` deep-copies
it again, and the copy is a new CreationSet with the SAME shape
(`list[list[float]]`) that type splitting, keyed on CreationSet identity,
separates from the original. Every pass adds a level.

The right fixed point is two contours separated by SHAPE (outer
list-of-lists vs inner list-of-floats), and nothing in the edge types as
compared today can express it. **So this issue's fix is not landable on
its own:** it types the reproducer and exposes 074's unbounded growth on
the corpus program. The next step is 074's: a compatibility notion under
which a copy minted inside the contour and the value it was copied from
are the same contour when their shapes agree -- the identity-vs-shape
question 146 frames -- measured on linalg with this gate change applied.
The gate change is reverted; the one-liner is recorded above.

## RESOLVED 2026-09-28

The targeted version, specified in the 2026-09-27 section, is now the
default. `decide_entry_set_split` admits a partially overlapping recursive
edge only when the contour being decided owns the single live creation
point of a CreationSet whose element is irrepresentable. It never fires on
a recursion whose contours carry no such demand. On today's tree it types
the reproducer and linalg, where on `c0327b84` it hit 074's growth.
Parent-first routing and the owner lift removed that growth.

**Measured against this issue's own verification plan:**

- `tests/deepcopy_recursive_nested_growth.py`: passes, and its
  `.known_issue` is removed.
- `tests/expr_evaluator.py` and `make test`: green.
- Corpus: no program that matched CPython stops matching. sudoku5 and
  softrender do not regress.

**The 4-line program still fails, by a different mechanism.** Under
parent-first, the outer and inner `r = []` share one CreationSet with TWO
creation points, so the "single creation point" test does not hold. The
CreationSet cannot be partitioned either, because both levels' writes
carry the same fused type: one assign-set key. That is
[176](176-FA-self-feeding-deepcopy-fusion-has-one-assign-set.md), not
this gate.

