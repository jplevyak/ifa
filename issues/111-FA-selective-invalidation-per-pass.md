# 111 — FA pass cost tracks ACCUMULATED contours: every pass re-walks the whole graph

**Status:** open. Performance only, and not a precondition for any
precision work. Rewritten 2026-09-28; re-measured 2026-10-17. It now also carries
[113](closed/113-FA-setter-equivalence-is-a-global-batch-partition.md), the
setter-classing blocker. The full record (M1–M3, seven attempts, the
shedskin instrumentation) is in git:
`git show 3f36072b:ifa/issues/111-FA-selective-invalidation-per-pass.md`.

**Re-measured 2026-10-17** (below): the cost is much smaller on chess and
now sits in a few long-running programs, plcfrs above all. The September
numbers further down predate start-merged identity (`PYC_CSDCPA1=2`),
parent-first routing and the owner lift, which cut contours 3-4× (chess ess
1591 → 426); they are kept as the record of the shape.

## Re-measured 2026-10-17

`PYC_DBG_WORK=1` (edge visits per pass, `census.work_edges`), at `f1ca0e2e`,
each program compiled alone from a scratch copy. "FA pass time" is the sum
of the per-pass `pass_time` the same line reports; "wall" is the whole
compile.

| program | passes | per-pass work, first → last | total edge visits | FA pass time / wall | ess / css |
| --- | --- | --- | --- | --- | --- |
| chess | 16 (31 in Sept.) | 18 272 → 179 143 (×9.8; ×48 in Sept.) | 1.69 M (22.5 M in Sept.) | 3.0 s / 8.0 s | 433 / 1506 |
| voronoi2 | 25 | 37 811 → 222 305 (×5.9) | 3.28 M | 3.1 s / 6.6 s | 417 / 1605 |
| plcfrs | 67 | 226 878 → 2 139 573 (×9.4) | 89.5 M | 93.1 s / 118.3 s | 1374 / 3921 |
| sunfish | 31 | 1 239 322 → 4 359 061 (×3.5) | 111.9 M | 91.9 s / 204.3 s | 789 / 3434 |

- **chess**, the September showcase, does 13× less work: half the passes and a
  fifth of the per-pass growth. For programs of that size this issue no
  longer matters much.
- **plcfrs** is where the shape still bites: 67 passes, work growing ×9.4 per
  pass, and FA 79% of the compile. It is the program to measure a fix on.
- **sunfish** spends only 92 of its 204 s in FA passes; the rest is post-FA,
  [185](185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md)'s
  territory, not this one. othello3, which never finishes pass 1, is
  [186](186-FA-pass-one-confluence-through-shared-builtins.md).
- Neither route below has been attempted since September, and the cheap
  "preserve only settled monomorphic contours" measurement is still untaken.

## The shape of the cost

`analyze_to_convergence` calls `clear_results()` before every pass. That
clears values AND the constraint graph (`forward`/`backward`), and
`add_es_constraints` rebuilds it from `top_edge`. So pass N re-walks every
contour ever minted.

Measured 2026-09-04 on chess, pyc vs shedskin, same source:

| | pyc | shedskin |
| --- | --- | --- |
| passes / iterations | 31 | 38 |
| work units, whole compile | 22.5 M edge visits | 0.33 M node visits |
| per unit | 1.21 µs | 11.2 µs |
| per-pass work, first → last | 30 823 → 1 484 115 (48×) | 9 871 → 10 239 (flat) |

**pyc is ~9× cheaper per unit and does ~68× more units.** Type violations
reach zero at pass 6. The remaining 25 passes are pure splitting, and each
re-walks the whole accumulated network to service a frontier of a few
dozen contours.

**Why shedskin stays flat:** `restore_network` resets the graph to a
FIXED-size snapshot every iteration (20 543 nodes on all 38). What
persists is knowledge in side tables: `alloc_info` (allocation decisions,
content-addressed by `(func, cart, site)`), the class-split table, and
edits to the snapshot. pyc keeps the structure and re-walks it; shedskin
keeps the decisions and rebuilds the structure. Contour identity is by
content in both.

**Not the fix:** a pass cap. Truncation makes `amaze` and `msp_ss` fail
to compile, and the author's direction is *"no artificial pass limits; the
analysis should recognise it is not making progress"*. See
[157](closed/157-FA-all-demand-must-be-evaluated-at-quiescence.md) item 4
(terminate on a fixed point).

## Two routes

### Route 1 — selective invalidation (M3), blocked

The invariant: a split REFINES, so a split contour's AVars must reset to
bottom, and so must everything forward-reachable from them. Everything
else still holds its fixed point. M1 measured that closure at median 0%,
p90 3%, max 16% of AVars. The upside is real. The seed set already exists
(`{es : es->split}` right after `run_split_stages`), and so do the flow
edges (`forward`, intact at the end of the pass). M2's differential harness
exists (`ifa/tests/selective_diff.sh`, `IFA_SELECTIVE`, default 0).

**Blocked by setter classing (113).** `AVar::setter_class` is a global
partition, computed in one batch over a pass's confluences
(`recompute_eq_classes`), and `same_eq_classes` asserts that every member
of every live `Setters` set carries one. It cannot be scoped (the preserve
decision precedes membership) or deferred (local classing diverges from
the global one), and widening coverage does not help. Seven attempts, one
cause: see the table in closed/113. The options, in increasing ambition:

1. a lazy class that provably agrees with the batch result (needs the
   batch's inputs kept alive);
2. incremental classing (union-find with merging; `split_eq_class` already
   refines);
3. per-contour classing, so a `Setters` set cannot span a preserve/clear
   boundary. This changes what classes mean, so measure precision.

Untried and cheap: **preserve only settled monomorphic contours.**
`compute_setters` only visits confluences, and a settled monomorphic
contour has none. Measure first: at a converged pass, how many AVars in
live `Setters` sets belong to a Fun with exactly one ES? If ~0, preserving
those contours sidesteps the assert.

### Route 2 — keep decisions, rebuild structure (shedskin's shape)

Move the persisted state from the contour graph to side tables (the split
ledger already is one), and rebuild contours from those tables against a
bounded base each pass. Larger change. It sidesteps 113 entirely, because
nothing derived survives a pass. This is the route the 48× growth points
at.

## Verification

`ifa/tests/selective_diff.sh --corpus` reports zero divergence (route 1).
Suites unchanged. Per-program compile time reported, not only the total.
`IFA_SELECTIVE=1` reaches convergence in the same pass count as `=0` on
`collatz`.
