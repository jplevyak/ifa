# 111 — FA pass cost: the "accumulated contours" premise was a measurement artifact

**Status:** CLOSED 2026-10-18. **The premise was false.** Per-pass FA work
does not grow with accumulated contours; it falls after the first passes and
then stays flat. The September numbers that said otherwise were read off a
counter that is a running total (below). The FA cost that does exist is call
MATCHING in the first few passes, which is
[185](../185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md) /
[186](../186-FA-pass-one-confluence-through-shared-builtins.md)'s territory;
whether the 12-15% type-duplicate EntrySets are still demanded is
[170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md)'s.
[113](113-FA-setter-equivalence-is-a-global-batch-partition.md), the
setter-classing blocker of route 1, stays closed with it. The earlier record
(M1–M3, seven attempts, the shedskin instrumentation) is in git:
`git show 3f36072b:ifa/issues/111-FA-selective-invalidation-per-pass.md`.

## The measurement artifact

`PYC_DBG_WORK`'s counters (`census.work_edges` and friends) are reset once
per analysis, in `initialize()`, and have been since the probe was added
(`46a9aab1`, 2026-09-04). Its `edges=` was therefore the CUMULATIVE count
since pass 0, printed under a per-pass name. So:

- September's "per-pass work, first → last 30 823 → 1 484 115 (48×)" was
  pass 0's work against the running total at the last pass;
- "22.5 M edge visits, whole compile" was the SUM of running totals; the
  true whole-compile count is the last value, 1.48 M;
- so "pyc is ~9× cheaper per unit and does ~68× more units" is wrong too.
  With the true count (and the September FA time of 1.21 µs × 22.5 M ≈
  27 s): ~18 µs per edge visit against shedskin's 11.2 µs per node visit,
  and ~4.5× shedskin's 0.33 M units -- different units, so only roughly
  comparable;
- and the 2026-10-17 re-measurement's first table repeated both errors (it
  summed the running totals), and its times were taken with four compiles in
  parallel, inflating them (plcfrs 118 s against 66 s alone).

The probe now prints the per-pass difference as `edges=` and the running
total as `cum_edges=` (fixed 2026-10-17).

## Re-measured 2026-10-17, corrected

Each program compiled ALONE from a scratch copy, at `1651f50e`.
`PYC_DBG_WORK` per-pass edge visits (true per-pass differences); `-v`'s
`PASS N COMPLETE` time split into `match` (call matching / dispatch),
`extend` (the split stages) and flow (the rest, the propagation this issue
is about); `PYC_DBG_PASSCONT` contour counts.

| program | passes | edge visits per pass: first, 2nd, 3rd ... last 3 | true total | time in passes | match | extend | flow | wall |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| chess | 16 | 18 272, 14 866, 14 036 ... 9 376, 9 375, 9 375 | 179 143 | 2.8 s | 1.3 s | 0.2 s | 1.4 s | 7.4 s |
| voronoi2 | 25 | 37 811, 10 652, 11 452 ... 7 807 ×3 | 222 305 | 3.0 s | 1.0 s | 0.5 s | 1.5 s | 6.2 s |
| plcfrs | 67 | 226 878, 56 881, 86 287 ... 24 864 ×3 | 2 139 573 | 46.8 s | **30.5 s** | 4.6 s | 11.7 s | 65.7 s |
| sunfish | 31 | 1 239 322, 247 427, 260 251 ... 25 252 ×3 | 4 359 061 | 73.8 s | **67.5 s** | 4.9 s | 1.3 s | 139 s |

- **Per-pass work falls, then is flat.** chess settles at ~9.4 K edge visits
  per pass, the same order as shedskin's flat ~10 K node visits per
  iteration in September. ifa does NOT re-walk a growing graph: chess's
  REACHED EntrySets are 433-444 from pass 9 to the end (`PYC_DBG_PASSCONT`).
- **The cost is matching, early.** sunfish spends 67.5 of its 73.8 s of
  passes in `match`, almost all in passes 0-4 (8-11 s each) -- when it has
  as few as 55 EntrySets. plcfrs's expensive passes are 1-7 (2.2-4.8 s, 75-89%
  match); passes 9-66 take ~0.3 s each, mostly flow. So pass cost tracks the
  UNIONS early passes carry through matching, not the number of contours.
  That is the territory of
  [185](../185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md) and
  [186](../186-FA-pass-one-confluence-through-shared-builtins.md) (a merged
  union reaching shared builtins), not of this issue.
- **Contours walked are not "every contour ever minted".** plcfrs has
  allocated 2 191 EntrySets and reaches 1 374 at the end; sunfish 1 601 and
  789.
- **Type-duplicate EntrySets at convergence** -- reached contours whose
  converged formal types equal a sibling's, i.e. ones a types-only
  derivation would fold: chess 55 of 433, voronoi2 50 of 417, plcfrs 209 of
  1 374, sunfish 114 of 789 (12-15%). Whether they are still demanded is
  [170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md)'s
  question; their per-pass cost is part of the flat ~0.3 s.

## What persists, and how it compares with shedskin

`analyze_to_convergence` calls `clear_results()` before every pass. That
clears every DERIVED thing -- AVar values (`in`/`out`/`gen`), the
constraint graph (`forward`/`backward`), setter sets and classes, each
EntrySet's `live_pnodes` / `out_edges` / `creates`, each CreationSet's
`defs` / `ess`, each edge's `args` / `rets` / formal filters -- and
`add_es_constraints` rebuilds them from `top_edge`.

**What survives the reset is the DECISIONS** (verified 2026-10-17 in
`clear_avar` / `clear_es` / `clear_cs` / `clear_edge`, `fa.cc`):

- **data contours:** the CreationSets themselves (`fa->all_creation_sets`)
  and each creation point's assignment to one (`AVar::cs_map`);
- **function contours:** the EntrySets (`fa->all_entry_sets`), their
  `filters` and `type_key`, and each call edge's binding to one --
  `AEdge::to`, which `make_entry_set` reuses on every later pass (`if
  (e->to) { edges.add(e); return; }`); the edge objects persist in
  `es->out_edge_map`, which the reset does not clear;
- **the split ledger** (`fa->split_ledger`), and the other between-pass
  decision tables: numeric coercion (`AVar::num_coerce`),
  `apply_unbound_fills`' IF1 rewrites (039), the frontend's field
  promotions, the 191 overwrite ledger.

So ifa, like shedskin, keeps decisions in side tables and re-analyses the
program against them each pass. Both tables only grow (shedskin's class
splits, `cl.dcpa`, are never taken back), and in both a decision for code a
pass does not reach costs nothing (shedskin copies an `alloc_info` entry
into `new_alloc_info` only when it reaches that site again; ifa walks only
the EntrySets reached through the bindings).

**The structural difference is what the tables hold** (`shedskin/infer.py`).
`iterative_dataflow_analysis` takes `backup_network` once, before the first
iteration, and `restore_network` resets to it after every splitting
iteration and sets `func.cp = {}`: shedskin's FUNCTION contours are not
decisions at all, but the cartesian product of the current argument types,
re-derived by eager CPA every iteration. Its decisions are data-contour
decisions only (`alloc_info`, class copies, the split classes written into
the snapshot). ifa's EntrySets start at one per function and split on
demand, so they ARE decisions, made pass by pass -- the counterpart of a
shedskin class split. Contour identity is by content in both.

**Not the fix for anything:** a pass cap. Truncation makes `amaze` and
`msp_ss` fail to compile, and the author's direction is *"no artificial pass
limits; the analysis should recognise it is not making progress"*. See
[157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) item 4
(terminate on a fixed point).

## Two routes (written against the false premise)

Both routes below reduce the per-pass FLOW re-walk, which the corrected
measurement shows is a small share of FA time (sunfish 1.3 of 73.8 s; plcfrs
11.7 of 46.8 s, spread over 67 passes). They are kept as the record; neither
is worth pursuing for performance unless a program is found whose late passes
dominate.


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

### Route 2 — stop persisting function contours that nothing still demands (shedskin's shape)

ifa already keeps decisions and rebuilds the structure (above), so this
route is not "move state to side tables". It is the difference in WHICH
decisions persist: shedskin re-derives every function contour each
iteration and keeps only data-contour decisions. The ifa version is either

- re-derive EntrySet bindings each pass from the converged types (persist
  the data-contour decisions and the ledger, drop `AEdge::to` between
  passes), or
- retire an EntrySet split whose demand has gone at convergence, which is
  [170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md).

Either bounds the walked set by what the current types need instead of by
the split history -- worth at most the 12-15% type-duplicate EntrySets
measured above, inside passes that already cost ~0.3 s. Neither touches setter classing, so 113 does not block it. The
risk is the one ifa/124 measured on 2026-10-17: some demands are visible
only BEFORE the merge they prevent (a demand-gated SETTER lost 7 tests), so
a contour re-derived from converged types may not be re-derived at all.

## Verification

`ifa/tests/selective_diff.sh --corpus` reports zero divergence (route 1).
Suites unchanged. Per-program compile time reported, not only the total.
`IFA_SELECTIVE=1` reaches convergence in the same pass count as `=0` on
`collatz`.
