# 185 — a merged tuple receiver makes the whole arity unroll live (sunfish: 537 s)

**Status:** open. Diagnosed 2026-10-08. Nothing is fixed yet.

## Symptom

sunfish compiles correctly and matches CPython on both backends. It takes
**537 s and 3 GB**, so `corpus_sweep.sh`'s 400 s cap records it as a
compile timeout. Flow analysis accounts for ~460 s of that, over 47
passes. The passes are badly uneven:

| passes | time | ess (max) | violations (max) |
| --- | --- | --- | --- |
| 1–13 | ~375 s (pass 10 alone: 111 s) | 4,692 | 40,627 |
| 14–47 | ~85 s (1–5 s each) | 1,113 | 419, then 0 from pass 28 |

At pass 14 the analysis collapses (ess 4,690 → 1,053, violations
14,302 → 410). It is not a stall or pass-limit trip: no `STALL` or `PASS
LIMIT` line is logged, and `reanalyze()` promotes nothing after pass 0.

## Mechanism

1. **The tuple methods are unrolled to the program's largest tuple
   literal.** `inject_tuple_methods_over` (`python_ifa_main.cc`)
   generates `__eq__`, `__lt__`, `__str__`, `__hash__`, `__contains__`,
   `count` and `index` as one constant-index step per position, each
   guarded by `n >= k` where `n = len(self)` (issues/119, issue 069).
   sunfish's `pst` piece-square tables are 64-element tuple literals, so
   every method has 64 guarded steps.

2. **The guards fold only when `n` is a constant**, that is, when the
   contour's `self` is record tuples of a single arity. Then the steps
   past the arity are dead and cost nothing, which is the case the design
   relies on.

3. **In the early passes `self` is every tuple in the program.**
   `tuple.__getitem__` starts as one contour per constant key
   (`__pyc_clone_constants__(key)`) with ONE merged receiver. So `t[k]`
   returns element k of every tuple that is ever indexed. That union then
   flows back into the 2-tuple moves (`i, j = move`), the transposition
   keys `(pos, (depth, root))`, the `(score, move)` pairs and the
   `directions` rays. Measured with the probe below: at pass 1 the
   formals of `__eq__`, `__lt__`, `__hash__` and `__pyc_to_bool__` carry a
   **49–52-CreationSet** union, which reaches plain ints too (`move:i`,
   `move:j`, `bound:gamma`, `bound:best`).

4. **With `self` merged, `n` is a union of arities**
   ({0, 2, 4, 8, 20, 64, runtime}), so no guard folds, and all 64 steps of
   every tuple method are live in every such contour. The steps' sends
   then fail, each with its own violation: tuple `__getitem__` BOXING and
   PRIMITIVE_ARGUMENT on `key`, and `__eq__` NOTYPE. Demand splitting
   separates the receivers a few at a time. `tuple.__getitem__` alone
   reaches **3,066 of the 4,125 EntrySets** at pass 10: each one pairs a
   22–33-CS receiver union with one constant key 0..63. Many of those
   (receiver, key) signatures occur 5–6 times, as distinct EntrySets with
   identical positional types (not yet explained).

So the number of passes comes from the demand cascade untangling a
receiver merged from the start (the ifa/157 shape), and the cost per pass
comes from the unroll depth multiplying every contour that cascade
creates.

## The measurement that separates the two

A temporary cap on the unroll count (not committed; it is an arbitrary
lever, and a heterogeneous record tuple past the cap would fall back to
the runtime-index tail that issues/119 removed) gives:

| unroll | compile | passes | output |
| --- | --- | --- | --- |
| 64 (as shipped) | 537 s | 47 | matches CPython |
| 8 | **48 s** | 51 (hits the cap; zero violations from pass 32) | matches CPython (126 lines, identical) |

The pass count barely moves; the cost per pass falls about 10×.

## Directions (none taken)

- **Fold the guard per receiver, not per contour.** The unroll is
  correct only when the receiver's arity is known. A guard that cannot
  fold is a demand: the method's contour holds receivers of different
  arities. Arity is already part of tuple type identity (ifa/132), so
  splitting the method's ES by receiver arity is a type split, not
  provenance. The open question is why that split takes ~13 passes to
  happen instead of one.
- **Find what merges the receiver of `tuple.__getitem__`.** Before the
  unroll is involved at all, the pass-1 union of 49 CSs is a confluence:
  either the single starting contour of `__getitem__` (correct by the
  minimal-start rule, but it should then split at the first quiescence),
  or something upstream that carries all tuples in one AVar. Backtrack it
  (AGENTS.md, "Find the confluence").
- **The duplicate EntrySets.** Identical (receiver, key) signatures
  appearing 5–6 times say a split is happening on something other than
  the positional types. Find out what.
- **The tail.** With the cap, passes 32–51 have zero violations and still
  grow ess (1,033 → 1,113); that is the zero-violation churn case of the
  stall guard and is separate.

## Probe

`IFA_DBG_VIOLHIST=1` (`fa.cc`, at the end of each pass) prints, per pass:

- violation totals by kind, and the top (kind, function, variable) groups (`VIOLHIST`);
- EntrySets per function (`ESHIST`);
- for the top function, its contours by positional-formal signature (`ESSIG`);
- every caller handing a formal a union of more than 10 CreationSets (`FEED`).

## Verification

sunfish compiles under the sweep's 400 s cap with output unchanged. The
`compile` column of a check sweep has no new failures, and programs that
already compile take no longer.
