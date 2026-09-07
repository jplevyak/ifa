# 138 — an argless EntrySet trips a compatibility assert

**Status:** fixed 2026-09-06. Found while triaging
[129](129-plan-demand-driven-creation-set-splitting.md)'s flag arm, where
`tests/test_heapq.py` **aborted the compiler** under `PYC_CSDCPA1=2`.

```
pyc: analysis/fa.cc:1465: int edge_type_compatible_with_entry_set(AEdge *, EntrySet *, int):
     Assertion `e->args.n && es->args.n' failed.
```

Reproduces with `PYC_CSDCPA1=2` alone — none of ifa/133's or ifa/136's
machinery. Clean at the default.

## The invariant does not hold

`set_entry_set` (`fa.cc:1658`) registers a new EntrySet in `fun->ess`
**and only then** fills `es->args`, iterating
`e->match->fun->positional_arg_positions`. An EntrySet created by an edge
whose *match* has no positional arguments therefore stays permanently
argless — while remaining a candidate in `find_best_entry_sets`, which
walks every `fun->ess`.

A later edge to the same `Fun` **with** arguments then asks
`entry_set_compatibility` about it, and the assert fires. Measured:

```
[argless] e->args.n=3 es->args.n=0 es=380 fun=__lt__ efun=__lt__ posargs=3
```

Three positional arguments on the edge, zero on the contour, same
function. Nothing on the path — `find_best_entry_sets`,
`entry_set_compatibility`, `check_edge` — guards for it.

## Fix

Replace the assert with an explicit answer for each shape:

| | |
| --- | --- |
| both argless | `return 1` — a genuine zero-argument shape, nothing to compare |
| exactly one argless | `return -1` — different shapes, reject the candidate |

**`-1` and not `0`, deliberately.** `entry_set_compatibility` treats `0`
as merely *less* compatible (`val -= 4`) and would still be free to pick
the contour on score — binding a three-argument edge to a contour that
never bound its formals. `-1` is its only hard reject.

## Result

`test_heapq` no longer aborts. It now fails with a C-backend diagnostic
(`incompatible integer to pointer conversion`, the same family as
`richards`), so the program still does not compile under the flag — but a
compiler that asserts on valid input is a strictly worse failure than one
that reports, and the two are separate defects.

Default path unchanged: `make test` 311/0, LLVM backend 311/0, all gates
green.

## Worth noting

The argless contour is itself suspicious. An EntrySet for a
three-argument function that never bound its formals is probably a defect
in its own right, not merely something to guard against — the guard here
stops the crash and makes the state observable, it does not explain why
`__lt__` acquires such a contour under the start-merged flag. Left open
deliberately rather than presented as understood.
