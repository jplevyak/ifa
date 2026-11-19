# 209 — the compiler's own memory: arenas by lifetime instead of the collector

**Status: open, a design question with one measurement.** Filed 2026-11-12.
Nothing is built. This is about the memory of `pyc`/`ifa` themselves, not of
the programs they generate (see "Not this issue" at the end).

The question: what would it buy, and what would it cost, to stop depending
on the Boehm collector inside the compiler. It matters for two reasons.
[208](208-whole-compile-time-where-it-goes.md) item 4 ranks the collector as
othello3's largest single cost. And any move off bdwgc, or to Rust, has to
go through this.

## What removing the collector buys: measured upper bound

`GC_DONT_GC=1` turns collection off and leaves allocation alone, so the
difference is everything the collector costs. Tree `328f0b12`, C backend,
each program alone, default and no-collection back to back.

| program | wall, default | wall, no collection | FA, default | FA, no collection | peak RSS |
| --- | --- | --- | --- | --- | --- |
| othello3 | 152.3 s | 107.1 s | 112.4 s | 79.5 s | 4.4 → 20.3 GiB |
| plcfrs | 35.2 s | 30.5 s | 28.6 s | 24.0 s | → 6.1 GiB |
| sunfish | 28.1 s | 26.5 s | 21.9 s | 20.2 s | → 8.1 GiB |
| rubik | 16.1 s | 16.6 s | | | unchanged |
| chess | 3.2 s | 3.5 s | | | unchanged |

Run-to-run noise on this machine is 15 to 20% (othello3's FA reads 100 to
130 s on one binary), so the plcfrs and sunfish rows are inside it and only
othello3's 30% is a firm number. On othello3 the gain is not only in FA:
clone went 16.7 → 10.3 s and the C compiler wait 14.1 → 9.5 s in that pair
of runs.

What the table says:

- **Most programs gain nothing.** 72 of the 77 corpus programs never collect:
  they allocate 0.3 to 2.5 GiB inside the 4 GiB starting heap, so bdwgc is
  already a bump allocator for them. rubik and chess are that case.
- **The large ones gain 6 to 30%**, and that is the ceiling. othello3
  allocates 15.5 GiB in 10 collections to hold 2 to 4 GiB live. An arena
  scheme should recover most of the 30% without the 20 GiB footprint,
  because it frees a pass's garbage rather than keeping it.
- **Allocation itself is a further 2 to 13% of samples** (rubik 10%, chess
  13%, from 208's profiles). A bump allocator would shrink it. Not measured.
- **The larger potential is representation, and it is unmeasured.** Arenas
  make 32-bit indices natural where there are 64-bit pointers now. Six
  compiles at once slow plcfrs from 37 s to 65 s (208), which suggests the
  compiler is bound by memory traffic. That is an expectation, not a
  measurement. A ten-line program has a 361 MiB RSS.

The cheaper alternative for the same 30% is parallel marking
(`GC_start_mark_threads`, 208 item 4), still unmeasured against the rebuilt
`libgc`. Measure that first: if it recovers most of othello3's collector
time, this issue is about leaving bdwgc, not about compile time.

## Why not `free`

About 85,000 lines: 77 classes deriving `gc`, roughly 370 `new` expressions,
184 `MALLOC`/`GC_MALLOC` sites, about 2,300 `Vec<` and 300 `Map<` uses,
against 53 `delete`/`free`. The graph is cyclic and aliased throughout
(AVar forward and backward edges, hash-consed ATypes, `Vec`s that share a
buffer by shallow copy). Nothing has one owner, so per-object freeing has no
place to stand. What the objects do have is a lifetime, in three tiers.

| tier | what lives there | freed |
| --- | --- | --- |
| whole compile | IF1, Syms, hash-consed ATypes, and the decisions that persist: CreationSets and `av->cs_map`, EntrySets and `AEdge::to`, the split ledger | never |
| one pass | everything `analyze_to_convergence` resets and re-derives from bottom | at the reset |
| one call | `Matcher` scratch, split-stage scratch | on return |

The second tier is where othello3's 15 GiB goes. The third is where 208's
samples show collections being triggered (`Vec::set_expand` under
`prune_uncoverable`, `type_minus_partial_applications`, `set_filters`,
`update_match_map`).

## What it costs

The plumbing is small: `Vec` and `Map` allocate through one policy class
(`DefaultAlloc`, `ifa/common/defalloc.h`), and `gc` is one base class. The
work is assigning each class and each container to a tier, and it is audit
work. The hazards:

1. **A pointer from a longer tier into a shorter one.** The collector hides
   these; an arena makes them dangle. The suspects are whatever survives a
   pass and points at per-pass objects: `match_cache`, `AEdge::match`, the
   standing-dispatch records (207). Finding them is mechanical: poison an
   arena on reset under AddressSanitizer and run the suite and the corpus.
   (ASan on this tree has its own history, closed/094 and
   [202](202-health-docs-levers-probes-tests-and-ci.md).)
2. **Container growth.** A `Vec` that grows in the whole-compile arena
   leaks every buffer it outgrew, so long-lived containers need a real free
   on growth, and every place two `Vec`s share one buffer has to be found
   first.
3. **Mixing with bdwgc during the transition.** Arena memory the collector
   does not scan can hold the only pointer to a collected object. Either
   arena chunks are allocated scanned-but-uncollectable
   (`GC_MALLOC_UNCOLLECTABLE`), or a whole tier moves at once.
4. **Finalizers and atomic allocations.** 16 uses of the collector's
   finalizer or atomic-type calls; each needs a decision.

Estimate: weeks in C++, most of it audit and debugging rather than code.

## Rust

A line-by-line port is not available: the aliased cyclic graph cannot be
written as references. The usual answer is arenas, typed indices and
interning, which is the three tiers above. So arenas with indices in C++ is
the incremental route to a port, and the only one where each step can be
checked against byte-identical C output on the corpus. The port after it is
still a rewrite of the 85,000 lines, months rather than weeks.

## Order

1. Measure `GC_start_mark_threads` with the rebuilt `libgc` (208 item 4).
   **Stop condition:** if it recovers most of othello3's 30%, nothing below
   is justified by compile time, only by the wish to leave bdwgc.
2. A scratch arena for one `Matcher` call. It is the smallest tier, it has
   the clearest lifetime, and it is where collections are triggered.
   **Stop condition:** if the ASan poisoning run shows per-call sets
   escaping into `match_cache` in more than a handful of places, the tiers
   are not what this issue says they are; write down what escapes before
   going further.
3. The per-pass tier, behind the same poisoning check.
4. Only then indices for pointers, one class at a time, starting with the
   most numerous (AVar, AEdge), and measuring the cache effect that is
   claimed above without evidence.

## Verification

Each step: C output byte-identical on all 77 corpus programs (as for
208's items 1 and 2), `make test`, a `check` sweep equal to the one before,
and peak RSS and wall time for othello3, sunfish, plcfrs, rubik and chess
against the table above.

## Not this issue

The generated programs also use bdwgc, through `pyc_c_runtime.h` (about 30
allocation sites; [RUNTIME.md](../../RUNTIME.md)). Replacing it there is a
different and harder problem: ownership, regions or reference counts
inferred by the analysis, with CPython's finalization order as the contract.
It shares nothing with the above but the library, and should be filed
separately if it is wanted.
