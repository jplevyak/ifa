# 171 — a numeric union closed through a loop sustains itself in one shared contour, and coercion hides it

> **CLOSED 2026-09-28 (merged into 156).** The mechanism and its fixture (`tests/numeric_union_self_sustaining.py`) are the concrete instance of [156](../156-FA-split-int-from-float-coerce-last.md), and the plan is carried there.
>
> *Archived during the 2026-09-28 issue consolidation. The text below is the historical record and is not maintained.*

**Status: open.** Found 2026-09-26 while checking a `-m check` sweep
regression on `shedskin_examples/ac_encode`. That regression turned out to
be this pre-existing bug, exposed by an unrelated change.

## Symptom

`tests/numeric_union_self_sustaining.py` (known issue), reduced from
`ac_encode`'s `decode`:

```python
u = 0 ; v = 1 << 30
for c in s:
    halfway = u + (v - u) / 2          # u, v become float (true division)
    if c == '1':
        u = halfway
    a = QUARTER + 1 ; b = THREEQU - 1  # a, b are int, always
    while (a > QUARTER) and (b < THREEQU):
        a = 2*a - HALF;  b = 2*b - HALF ; u = 2*u - HALF ;  v = 2*v - HALF
```

pyc compiles it with **no diagnostics** and prints `(2.0, 1073741822.0, …)`
where CPython prints `(2, 1073741822, …)`. `a` and `b` never hold a float in
CPython.

In `ac_encode` itself the same shape does not get as far as printing. Once
coercion widens `a` and `b` to float, the int-only paths around them fail
(`unresolved call '__sub__'`, `illegal call argument type 'a' illegal:
float64`, `'w' has no type`), and the program does not compile.

## Mechanism

`PYC_DBG_BIND=__mul__`, plus a per-pass listing of `int.__mul__`'s
contours:

```
BIND pass=0 __mul__ e=120 MINT  es=88 ... actuals=[__mul__ int64 int64]
BIND pass=0 __mul__ e=121 REUSE es=88 ... actuals=[__mul__ int64 int64]
BIND pass=0 __mul__ e=122 REUSE es=88 ... actuals=[__mul__ int64 int64|float64]
BIND pass=0 __mul__ e=123 REUSE es=88 ... actuals=[__mul__ int64 int64]
p=0..4  es=88 edges: e120[2 | int64,float64] e121[..same..] e122[..same..] e123[..same..]
```

1. IFA's minimal start puts all four `2*x` sites into one `int.__mul__`
   contour. This is correct, and it is where the analysis should start.
2. `2*u` brings the float in, and the shared contour returns
   `{int64, float64}`.
3. That result flows back into `a` and `b` through `a = 2*a - HALF`, and
   `a` and `b` feed the same edges.
4. From pass 0 onwards **every edge carries identical actuals**
   `{int64, float64}`. TYPE_CONFLUENCE partitions a contour by its edges'
   argument types, and here there is nothing to partition, so `es=88` is
   never split and none of its edges is ever re-bound.
5. At quiescence the reanalyze step's numeric coercion
   (`fa_coerce_numeric_confluences`, `PYC_DBG_NUMC`) sees `a`/`b` as a pure
   `{int64, float64}` mix and widens them to float64. That answers a union
   the analysis made up; the program itself never produces it.

The union is a fixed point of the analysis, but not the least one. If the
`2*u` edge had its own contour, re-deriving from bottom (which every pass
already does) would give `a`/`b` int and `u`/`v` float, and that solution is
also self-consistent. The typing rules say nothing wrong. The problem is
that no demand ever reaches the merge. The union is representable (coercion
handles it), so nothing is violated, and the edges are type-identical, so no
type-keyed split can take them apart.

Whether a program falls into this depends on edge-binding order in pass 0,
which is why it looks layout-sensitive. `ac_encode` compiled at `3f1d6f2a`.
The same binary fails on `ac_encode` with only its `hardertest()` call
deleted, and on every other trivial perturbation tried. The failure is
latent at HEAD, and any change that renumbers symbols can expose it. The
change that exposed it here was generating tuple-compare guards as inline
primitives, and those methods are not even reached in `ac_encode`.

## Relation to other issues

- [170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md) has
  the same root: a decision taken on pass 0's transient types persists. In
  170 it is a split that should be undone. Here it is a merge that nothing
  can undo, because the union has closed on itself.
- [145](145-numeric-coercion-is-not-gated-on-permissive-mode.md): coercion
  is a permissive-only device. This issue shows it also runs **ahead of**
  the precision ladder and hides a merge the ladder could have fixed.
- AGENTS.md "Find the confluence, backtrack the demand, split": the
  confluence is a CONTOUR (`es=88`), not a program point, and the edges are
  indistinguishable by type. The per-call-site handle is the allowed
  *mechanism* under the 2026-09-06 refinement, provided a demand decides
  WHETHER to split.

## Proposed direction

Treat a pure-numeric mix that coercion is about to resolve as a demand
first:

1. Before annotating an AVar in `coerce_annotate`, backtrack its union to
   the contour where int and float meet (here `es=88`'s return, reached
   from `a`'s writer `2*a - HALF`).
2. If that contour's in-edges are type-identical, partition them by edge
   (call site). The union being coerced is the demand. The call site only
   names the parts.
3. Re-run. Coerce only what is still mixed at the next quiescence.

Stop condition, written before building: if the split `2*u` edge ends up
back in the int contour on the next pass (the partition oscillates), then
the union has a second, real source that this model missed. In that case,
find it rather than adding hysteresis.

## Verification

- `tests/numeric_union_self_sustaining.py` flips KNOWN -> PASS (prints
  ints).
- `ac_encode` compiles under any perturbation of the program, e.g. with
  `hardertest()` removed.
- Corpus `-m check`: count the `[numc] annotate` lines before and after.
  Every annotation that disappears is a CPython deviation removed.
