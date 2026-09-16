# ifa/156 — split int from float where possible; coerce only as a last resort

**Status:** open. Directive + a measured account of why the current ladder
cannot carry it. No lever landed: the obvious one was tried and is a no-op.

## The directive

**Author, 2026-09-15: split `int` from `float` where possible, and coerce only
as a last resort.**

This is a CORRECTNESS argument, not a precision one, and it inverts today's
priority. Numeric coercion widens the int, so pyc prints `1.0` where CPython
prints `1` — CLAUDE.md names that deviation explicitly, and
[145](145-numeric-coercion-is-not-gated-on-permissive-mode.md) makes coercion
a permissive-only device that `--strict` must refuse. **Splitting keeps the
int an int**, so it is the answer that matches CPython; coercion is the lossy
fallback taken when no split can separate the paths.

## What is already right

**The pass ordering.** `promote_first=2` (the default) runs
structural repair → **the splitter** → reanalyze phase 2, and coercion lives
in phase 2 (`do_coerce = ifa_reanalyze_phase != 1`). So the splitter already
gets first refusal on every pass; coercion only annotates what is left. The
ordering does not need changing.

## What is wrong — the mix is never OFFERED to the splitter

`elem_irrepresentable` ends:

```c
  return nb > 1 && !all_num;
```

`!all_num` excludes a pure numeric mix from being a demand at all, and the
comment says why: *"a pure-numeric mix — `{int64, float64}` is resolved by
`coerce_annotate`, so flagging it would split what coercion fixes."* So
`{int64, float64}` is not a demand anywhere in the ladder, **by design** — the
system delegates it wholesale.

## Dropping that exclusion is NOT enough — measured

Tried: offer a pure numeric mix as a demand (a `PYC_NUMSPLIT` lever).
**It changes nothing**, on `fysphun`, `softrender`, `sudoku4` or `bh` — with
`PYC_CSSLOTDEMAND=1` as well, so both content channels were open.

The reason is structural, and it is the useful result here:

| program | coercion targets that are **ES**-contoured | **CS**-contoured |
| --- | --- | --- |
| `fysphun` | 283 | 15 |
| `softrender` | 513 | 15 |

**~96% of numeric coercion's targets are EntrySet-contoured — locals and
formals.** `coerce_annotate`'s own first loop says so: it scans
`for (EntrySet *es : fa->ess) for (Var *v : es->fun->fa_all_Vars)`, and only
then the closure/record CS vars and container elements.

And the ENTIRE demand ladder is about CreationSets: `elem_irrepresentable`,
`cs_elem_irrepresentable`, route 4's candidate set,
[152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md)'s backtrack.
**So opening the numeric case reaches roughly 4% of it**, which is why the
lever measures as a no-op. It was removed rather than left off
(CLAUDE.md: delete an inert lever, do not default it away).

## What the directive actually needs

An **EntrySet-side demand rung**, which does not exist today:

1. **The demand.** `coerce_annotate` already finds every pure numeric mix. The
   case to raise is the one it currently abandons — a narrow member that is
   not an immediate, where the constant rewrite cannot help (see
   [146](146-remove-all-arbitrary-splitting.md)'s account of `softrender`).
   Coercion knowing it cannot proceed IS the demand; nothing watches it today.
2. **The backtrack.** The mix cannot be keyed where it is observed: measured,
   every in-edge of every mixed contour already carries `{int64, float64}`,
   so "can this contributor carry int64" answers yes for all of them. It must
   be walked back to the pure/mixed boundary — and that boundary exists in
   quantity: ~641 pure-int and ~5663 pure-float AVars sit above ~639 mixed
   ones on `softrender` (`PYC_DBG_BOXPURE`).
3. **The split.** At the boundary the key IS type-shaped — which numeric basic
   this path carries — so it is a legitimate partition and not provenance.
   Bounded by the demand (two numeric kinds), never by a count of
   contributors (ifa/144).
4. **Coercion last.** Only for what survives: a genuine temporal mix like
   `bh`'s `Vec3` (CLAUDE.md's counterexample), where one object holds a float
   then an int over its lifetime and no contour split can separate it.

## Verification plan

- [ ] a fixture: two call paths, one int-only and one float-only, meeting at a
      shared formal — the analysis should split, not coerce, and the program
      should print CPython's `1` rather than `1.0`
- [ ] corpus `-m check`: `stdout_differs` should FALL, since each program that
      currently prints a coerced `1.0` is a program whose output disagrees
      with CPython
- [ ] coercion annotations should fall on the same programs (fysphun 298,
      softrender 528 today) without the violations rising

## What this unblocks

Every program whose output differs from CPython because an int was widened.
It is also the mechanism [146](146-remove-all-arbitrary-splitting.md) needs
for `softrender`: with the int and float paths split, `PYC_ESBLOCK` would no
longer relocate a mix past the point where coercion could repair it, and the
`sudoku4`/`softrender` trade would come apart.
