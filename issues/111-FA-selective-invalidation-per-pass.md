# 111 — FA pass cost tracks ACCUMULATED contours: every pass re-walks every EntrySet its persisted routing still reaches

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

So ifa, like shedskin, keeps decisions and rebuilds the structure. Pass N
re-walks every EntrySet still reached from `top_edge` through those
persisted bindings -- not every contour ever minted (one no route reaches
is not walked), but in practice nearly every one, because a binding is never
retired ([170](170-FA-contours-minted-on-transient-types-are-never-remerged.md)).

Measured 2026-09-04 on chess, pyc vs shedskin, same source:

| | pyc | shedskin |
| --- | --- | --- |
| passes / iterations | 31 | 38 |
| work units, whole compile | 22.5 M edge visits | 0.33 M node visits |
| per unit | 1.21 µs | 11.2 µs |
| per-pass work, first → last | 30 823 → 1 484 115 (48×) | 9 871 → 10 239 (flat) |

**pyc is ~9× cheaper per unit and does ~68× more units.** Type violations
reach zero at pass 6. The remaining 25 passes are pure splitting, and each
re-walks every EntrySet the persisted routing reaches -- a set that grows
with every split -- to service a frontier of a few dozen contours.

**Why shedskin stays flat -- the real difference is WHICH decisions
persist** (verified 2026-10-17 in `shedskin/infer.py`). Both keep decisions
and rebuild the structure. `iterative_dataflow_analysis` takes
`backup_network` once, before the first iteration, and `restore_network`
resets to it after every splitting iteration (20 543 nodes on all 38) --
and sets `func.cp = {}` for every function, discarding every
cartesian-product FUNCTION contour. What persists is DATA-contour decisions
only: `alloc_info` (`(func, cart, alloc_node) -> (class, dcpa)`), the class
copies (`class_copy`, `cl.dcpa`), and the split classes written into the
snapshot (`beforetypes[n] = {(cl, newnr)}`). Each iteration's `propagate`
then re-derives the function contours by CPA from the current types.

ifa persists its FUNCTION-contour decisions too -- EntrySet splits and edge
bindings -- and never retires them (170), so the set a pass walks only
grows: chess 30 823 -> 1 484 115 edge visits per pass in September,
18 272 -> 179 143 today. shedskin's walked set is re-derived, so it stays
flat. Contour identity is by content in both.

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

### Route 2 — stop persisting function contours that nothing still demands (shedskin's shape)

ifa already keeps decisions and rebuilds the structure (above), so this
route is not "move state to side tables". It is the difference in WHICH
decisions persist: shedskin re-derives every function contour each
iteration and keeps only data-contour decisions. The ifa version is either

- re-derive EntrySet bindings each pass from the converged types (persist
  the data-contour decisions and the ledger, drop `AEdge::to` between
  passes), or
- retire an EntrySet split whose demand has gone at convergence, which is
  [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md).

Either bounds the walked set by what the current types need instead of by
history. Neither touches setter classing, so 113 does not block it. The
risk is the one ifa/124 measured on 2026-10-17: some demands are visible
only BEFORE the merge they prevent (a demand-gated SETTER lost 7 tests), so
a contour re-derived from converged types may not be re-derived at all.

## Verification

`ifa/tests/selective_diff.sh --corpus` reports zero divergence (route 1).
Suites unchanged. Per-program compile time reported, not only the total.
`IFA_SELECTIVE=1` reaches convergence in the same pass count as `=0` on
`collatz`.
