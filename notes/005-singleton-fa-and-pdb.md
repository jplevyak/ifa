# 005 — The singleton `FA *fa` and `PDB *pdb` globals

After tier-3 reentrancy steps 1-4 landed (June 2026), `class FA`
owns all of its per-instance analysis state: worklists, hash-cons
caches, canonical types, and id counters. **What still doesn't
exist is *signature-level* independence from the global singleton
pointer.** The free functions in `fa.cc` and most external callers
still reach the active analysis context via the global `FA *fa`
(set at `FA::analyze` entry) rather than receiving it as a
parameter. This note captures where we stopped, why, and what
moving the rest of the way would entail.

## Where we are after steps 1-4

`class FA` is a real per-instance container:

- `FA::type_world` owns the 4 hash-cons tables and the 17 canonical
  AType pointers (steps 2-3).
- `FA::edge_worklist`, `send_worklist`, `es_worklist`,
  `entry_set_done`, `type_violations` (step 1).
- `FA::avar_id`, `aedge_id`, `creation_set_id`, `entry_set_id` (step 4).
- `FA::fa_events_storage`, `fa_events_enabled` (step 1's harness
  sidecar).
- Plus the older `FA::pdb`, `funs`, `top_edge`, `ess`, `ess_set`,
  `css`, `css_set`, `global_avars`, `print_call_depth`,
  `permit_boxing`, `no_unused_instance_variables`,
  `tuple_index_base`, `num_constants_per_variable`, `pass_limit`,
  `pass_limit_hit`.

The practical property this gives you: **two FA instances running
back-to-back in the same process don't leak state into each other**.
That's what `fa_reset()` used to enforce by zeroing every static;
now it's enforced structurally because each FA owns its state.

## Where we stopped

The two top-level globals remain:

- `FA *fa` (`fa.cc:49`) — assigned by `FA::analyze` at entry as
  `::fa = this;` (`fa.cc` line ~3874).
- `PDB *pdb` (`pdb.cc:8`) — assigned by `PDB::PDB(IF1*)` at
  construction.

And the ~50 free functions in `fa.cc` plus the IFA-using callers in
`analysis/clone.cc`, `optimize/*.cc`, `codegen/*.cc`,
`if1/pattern.cc`, `analysis/graph.cc`, and (top-level)
`llvm.cc` still reach the current FA via the global. There's no
signature passing it through.

## Why we stopped

Three reasons:

1. **No concrete multi-FA use case.** The test harness runs one
   FA at a time and resets between programs. pyc compiles one
   program per invocation. There's no caller wanting to construct
   two FAs concurrently.

2. **The cost is real and the diff is large.** ~430
   `fa`/`pdb` references across ~10 files. ~380 of them are
   implicit global access (not already
   `FA*`-parameter-aware). Steps 1-4 each touched 10-100
   references in 1-2 files; this would touch 400 references
   across 10 files. The AUDIT explicitly warned: "Don't try to
   do this in one PR."

3. **The structural property is already there.** Each FA owning
   its state was the point. Removing the singleton ergonomic is
   nice but not load-bearing.

## What step 5 would require

There are two plausible shapes, each with its own tradeoff.

### Shape A — Thread `FA *fa` through every call chain

Every free function that today reads `fa->X` gains an explicit
`FA *fa` parameter. Every call site passes its `fa` along.

- Pro: explicit; matches modern C++ style; makes data flow
  obvious; enables true concurrent analyses.
- Con: ~50+ function signature changes in `fa.cc`; every caller
  updated; some cascade depth (helper → helper → helper, each
  needs the parameter).
- Surface: roughly half of the ~380 implicit-global sites become
  parameter accesses; the other half are already inside member
  functions where `this->X` works.

### Shape B — Back-pointers on the IR objects

Add `FA *fa` to `EntrySet`, `CreationSet`, `AEdge`, and
`AVar` (set in their constructors via `fa = ::fa` — yes, that's
circular, but the constructors only run when `::fa` is set).
Inside fa.cc free functions, replace `fa->X` with `es->fa->X` /
`av->fa->X` / etc., using whatever local IR pointer the function
already has.

- Pro: smaller diff (no signature changes); IR is "self-aware";
  most free functions already have an EntrySet/AVar/CreationSet
  in scope.
- Con: pollutes the IR (every AVar, AEdge, etc. carries a
  pointer just for this); doesn't actually enable concurrency
  unless the constructors stop reading the global; you've moved
  the singleton dependency from "free function reads
  global" to "constructor reads global."

### Shape C — Hybrid

Pass `FA *fa` to top-level free functions in fa.cc, let nested
helpers either receive it as a parameter or reach it via
`es->fa` if back-pointers exist on EntrySet/CreationSet only
(not all IR objects). Maybe the best of both.

## What this unblocks

- **True concurrent analyses.** Two `FA::analyze` calls on
  different threads sharing no state. Today the global `::fa = this;`
  assignment in `analyze` would race.
- **Embedded IFA usage.** A host process running its own
  pipeline alongside an embedded IFA. Today there's only one
  process-wide active FA.
- **`graph.cc` globals cleanup.** The CLEANUP item "Move
  `graph.cc` globals into a config object" was tagged as
  piggybacking on step 5 — the graph subsystem reaches the
  current FA via the global. Threading FA through `graph()`
  naturally drags the graph-specific globals (`graph_fun[80]`,
  `graph_var[80]`, `graph_type`, `fgraph_frequencies`,
  `fgraph_constants`) into a config struct.

## Surface area inventory (June 2026)

For an investigator scoping the work later:

| File                          | `fa` refs | `pdb` refs | Already takes `FA*` |
|-------------------------------|-----------|------------|---------------------|
| `analysis/fa.cc`              | 205       | 10         | 4 funs              |
| `optimize/dead.cc`            | 40        | 0          | 11 funs             |
| `analysis/clone.cc`           | 40        | 0          | 2 funs              |
| `codegen/llvm.cc`             | 30        | 0          | 7 funs              |
| `codegen/cg.cc`               | 28        | 0          | 6 funs              |
| `analysis/graph.cc`           | 26        | 0          | 6 funs              |
| `optimize/inline.cc`          | 16        | 0          | 4 funs              |
| `if1/pattern.cc`              | 16        | 0          | 6 funs              |
| `optimize/loop.cc`            | 12        | 0          | 3 funs              |
| `llvm.cc` (top-level pyc)     | 8         | 0          | 1 fun               |

Numbers will drift over time. The point: ~50 functions across 10
files already accept `FA*`; ~380 references remain implicit.

## Related cleanup items

- `ifa/analysis/CLEANUP.md` "Move `graph.cc` globals into a config
  object" — naturally piggybacks on this, per AUDIT §2.4.
- `ifa/analysis/AUDIT.md` §2.3 "The `fa_reset()` band-aid" — fa_reset
  still exists post step 1-4 to clear `analysis_pass`, the timers,
  and the type_violation_hash (the last is still file-static
  because it's accessed before FA exists in some test paths).
  Eliminating `fa_reset()` entirely needs step 5 + a rethink of
  the `ifa_init`/`ifa_reset` lifecycle.

## See also

- [../analysis/AUDIT.md §2.1](../analysis/AUDIT.md) — the
  original non-reentrant inventory.
- [../analysis/AUDIT.md §2.2](../analysis/AUDIT.md) — the
  refactor sequence (steps 1-5).
- [../analysis/AUDIT.md §2.3](../analysis/AUDIT.md) — the
  `fa_reset()` band-aid.
- [../analysis/AUDIT.md §2.4](../analysis/AUDIT.md) — `clone.cc`
  and `graph.cc` reaching into the globals.
- [../analysis/CLEANUP.md](../analysis/CLEANUP.md) — the tier-3
  reentrancy entries.

## History

Filed June 2026 after tier-3 reentrancy steps 1-4 landed. No code
change. Promote to `ifa/issues/NNN-...md` if and when a concrete
use case justifies the work.
