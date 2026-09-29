# ifa/156 — split int from float on demand; coerce only as a last resort

**Status:** open. Rewritten 2026-09-28. It now carries
[145](closed/145-numeric-coercion-is-not-gated-on-permissive-mode.md)
(coercion is permissive-only; the use-sensitive residual) and
[171](closed/171-FA-numeric-union-sustains-itself-through-a-shared-contour.md)
(the concrete mechanism and its fixture).

## The directive

**Author, 2026-09-15:** *split `int` from `float` where possible, and
coerce only as a last resort.* This is a CORRECTNESS argument. Coercion
widens the int, so pyc prints `1.0` where CPython prints `1`. Splitting
keeps the int an int. Coercion is a permissive-only device (145, landed:
`coerce_annotate` returns 0 unless `fruntime_errors`, and `--strict`
reports the mix; `tests/strict_rejects_numeric_boxing.py`).

## Fixture

`tests/numeric_union_self_sustaining.py` (`.known_issue`), reduced from
`ac_encode`'s `decode`:

```python
u = 0 ; v = 1 << 30
for c in s:
    halfway = u + (v - u) / 2          # u, v become float
    if c == '1': u = halfway
    a = QUARTER + 1 ; b = THREEQU - 1  # a, b are int, always
    while (a > QUARTER) and (b < THREEQU):
        a = 2*a - HALF;  b = 2*b - HALF ; u = 2*u - HALF ;  v = 2*v - HALF
```

pyc compiles it with no diagnostics and prints `(2.0, 1073741822.0, …)`.
CPython prints `(2, 1073741822, …)`.

## Mechanism (measured, 171)

1. IFA's minimal start puts all four `2*x` sites into one `int.__mul__`
   contour (`es=88`). That is correct.
2. `2*u` brings a float in, and the shared contour returns `{int64,
   float64}`. That flows back into `a` and `b` through `a = 2*a - HALF`,
   and they feed the same edges.
3. From pass 0 every edge carries identical actuals `{int64, float64}`.
   TYPE_CONFLUENCE has nothing to partition (self-blinding: `etype ==
   stype`), so `es=88` never splits.
4. At the end, `coerce_annotate` widens `a`/`b`: it answers a union the
   analysis made up.

The union is a fixed point, but not the least one. Split the `2*u` edge
off and re-derive from bottom, and `a`/`b` come out int and `u`/`v` float,
which is also self-consistent.

**Why no rung sees it.** A pure-numeric mix is excluded from every demand
test by design (`elem_irrepresentable` ends `return nb > 1 && !all_num`:
"coercion fixes it"). ~96% of coercion's targets are EntrySet-contoured
(fysphun 283 ES vs 15 CS, softrender 513 vs 15), while the demand ladder
is CreationSet-side. So simply dropping the exclusion (`PYC_NUMSPLIT`)
reached ~4% and measured a no-op. It was deleted.

## The plan: coercion's failure-to-be-exact is the demand

Coercion knowing it is about to change a value's representation IS
"something observing a distinction and being unable to proceed" (without
deviating from CPython). The partition key is type-shaped (which numeric
kind a path carries), so it is not provenance.

1. **Demand.** Before `coerce_annotate` annotates an AVar, raise a demand
   for its `{int64, float64}` union. It is ES-side, so it needs the
   ES → actuator link (157's `PYC_RETDEMAND` shape), not the CS ladder.
2. **Backtrack to the pure/mixed boundary.** Where the mix is observed,
   every in-edge already carries the union. It must be walked back to the
   nearest contour whose in-edges carry DIFFERENT numeric kinds, or whose
   writers include both a pure-int and a pure-float source. The boundary
   exists in quantity: on `softrender` ~641 pure-int and ~5663 pure-float
   AVars sit above ~639 mixed ones (`PYC_DBG_BOXPURE`).
3. **Split there**, bounded by the demand (two numeric kinds), never by a
   count of contributors. In 171's shape the contour is `es=88` and its
   edges are type-identical, so the call site is the HANDLE that names the
   parts (allowed under the reason/mechanism rule, since the coercion
   demand alone decides whether to split).
4. **Coerce last**, in reanalyze phase 2, only what is still mixed at the
   next quiescence: a genuine temporal mix like `bh`'s `Vec3` (AGENTS.md's
   counterexample), where one object holds a float and then an int.

### The use-sensitive half (145/144)

A pure-numeric mix is resolvable by widening only where no USE requires
the narrow type. `bh`'s `Cell.NSUB >> (k + 1)` widened to double is not C
(`>>` on a double). There, splitting is the only correct answer, and
`Vec3`'s float-only members are pure waste. So the boundary split must win
over coercion wherever any use of the value is integer-only (`>>`, `&`,
an index). Ordering alone cannot decide it: deferring the numeric demand
to "after the other splitters quiesce" releases it at exactly the instant
coercion runs, and coercion wins (144, attempt 3).

### The interaction to watch: split relocates the mix past the constants

Coercion rewrites CONSTANTS only (`type_coerce_numeric_constants`). A split
can move the int/float meeting point downstream of arithmetic, where the
int is already a runtime value, and then nothing can repair it. That is
how `PYC_ESBLOCK` once cost `softrender` 60 → 483 violations (fixed by
scoping ESBLOCK to non-numeric container element writers). A boundary
split that meets a mix it cannot discharge must fall back to coercion, not
leave the program uncompilable.

## Stop conditions

- If the split `2*u` edge lands back in the int contour on the next pass
  (the partition oscillates), the union has a second, real source: find
  it, and do not add hysteresis.
- If a boundary split increases `mixed basic types` errors anywhere, it is
  relocating the mix past the constants. Fall back to coercion for that
  AVar.

## Verification

- `tests/numeric_union_self_sustaining.py` flips KNOWN → PASS (prints ints).
- `ac_encode` compiles under any perturbation (e.g. with `hardertest()`
  removed).
- A fixture with two call paths, one int-only and one float-only, meeting
  at a shared formal, prints CPython's `1`, not `1.0`.
- Corpus `-m check`: `[numc] annotate` count (`PYC_DBG_NUMC`) falls;
  `stdout_match` rises; nothing that matched regresses.
