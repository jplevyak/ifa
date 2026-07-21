# Issue 035: clone/codegen output is heap-layout-dependent; some layouts miscompile (int read as float)

**Status: FIXED 2026-07-10** (same day; see §Fixes landed below for
the twelve individual defects). The full tests/ determinism sweep —
compile every test twice, byte-diff the generated `.c` — went from
**36 nondeterministic tests to ZERO**, expr_evaluator compiles to a
byte-identical `.c` eight runs in a row, the ASLR-dependent
crashes (pylife FA, expr_evaluator clone) no longer reproduce, and
the corpus member set is unchanged (22/55, same members). Remaining
follow-ups, tracked here until landed: (a) DONE 2026-07-10 — the
harness (test_pyc.py) now compiles every test twice and byte-diffs
the generated .c/.ll (opt out with SKIP_DET_CHECK=1); (b) DONE
2026-07-10 — branch merged with main and revalidated: the trio
converges stall-free on MAIN alone (stage C no longer needed for
that; see 033's resolution update), and the branch's residual
builtins_batch miscompile is now deterministic and documented in
033 — branch parked; (c) DONE
2026-07-10, closed — [036](closed/036-llvm-phy-lowering-wrong-value.md) — the
one suite delta: the now-COMPLETE liveness places more phys, which
the C backend lowers correctly everywhere but the v2 LLVM path
miscompiles on expr_evaluator (deterministically — previously this
failure class was dice).

## Fixes landed (chronological by discovery; each was found by
byte-diffing logs/outputs of identical runs to their FIRST
divergence)

1. `clone.cc sets_by_f / sets_by_f_transitive`: greedy equivalence
   partitioners now canonicalize their input to id order (seeds and
   Lskip checks were iteration-order sensitive; callers hand these
   hash-set Vecs).
2. `clone.cc initialize`: `fa->ess` rebuilt compact + id-sorted
   (was a raw hash-table image via `append(fa->ess_set)`).
3. `clone.cc initialize`: `fa->global_avars` id-sorted (built from
   the contour-pointer-keyed `v->avars` Map).
4. `clone.cc build_concrete_types`: `css_sets` sorted by min member
   id — `define_concrete_types` is order-SENSITIVE (eqcss groups
   sharing a base sym mutate `sym->creators` as processed; order
   decided which group kept the sym vs got a clone — the actual
   int-as-float miscompile arm).
5. `fa.cc collect_types_and_globals`: `typesyms`/`globals`
   id-sorted (codegen numbers `g%d` and emits type decls in
   iteration order).
6. `fa.cc analyze_edge`: positional args iterated in canonical
   position-path order, not `form_MPositionAVar` bucket order —
   this loop CREATES the formal/filtered AVars, so bucket order set
   the AVar id-assignment order that every downstream `qsort_by_id`
   keys on.
7. `fa.cc get_AEdges` + `check_split`: edge lists sorted before
   routing/enqueue (worklist schedule followed hash order).
8. `map.h`: `PointerHash<MapElem<K,C>>` delegates to the KEY's
   PointerHash — Map<K*,C> bucketed by raw key pointer even for
   id-hashed key types, so every form_Map iteration followed heap
   layout.
9. `pattern.h MPosition` and `dom.h Dom`: stable serial ids +
   id-based PointerHash (arg/filter maps; dominance frontiers →
   phi placement order).
10. **`ssu.cc merge_live` — the correctness root**: fixpoint
    progress flag accumulated with `=` instead of `|=`, so only the
    LAST var iterated out of the (raw-pointer-hashed) live set
    decided convergence; under some layouts liveness was
    INCOMPLETE, phy placement varied (25 vs 21 on one fun), and
    every subsequent PNode/Var id shifted. Also `live_vars` now
    id-hashed (`VarIdHashFns`).
11. `fa.cc record_backedges`: pending-map iterated in canonical
    key order (it CREATES AEdges per element, so edge ids followed
    bucket layout); `PendingMapHash` now hashes key CONTENT ids.
12. **`map.h map_set_add`/`fa.cc map_union` on HashMap — the
    routing root**: `HashMap : Map`, and `map_set_add`/`map_union`
    bound to the BASE class insert path (MapElem/pointer-equality
    keys) while `HashMap::get` probes with `AHashFns` content
    hashing/equality — inserts and lookups used DIFFERENT hash
    functions and equality on the same table, so
    `check_split`'s pending-backedge lookups hit or missed by heap
    layout (edge routing flipped run to run). Added HashMap-aware
    `map_set_add` overloads; the `split_entry_set` union now merges
    per-element through the content-correct path (the old
    `map_union` also REPLACED the value vec on key hit, dropping
    accumulated entry sets).

Also fixed as fallout: `clone.cc determine_clones` seeded/advanced
its changed-sets as compacted VECTORS but probed them via
`some_intersection`→`set_in` (hash probing) — membership answers
were garbage beyond SET_LINEAR_SIZE and the equivalence gates
sometimes never fired. The old code only worked because `fa->ess`
was a raw table image (fix 2 exposed it). Now kept in set mode
end-to-end. One synthetic fixture's clone/dce/freq goldens
(`nested_iterator.synth`) were re-blessed: the old expectations
encoded the dark-gates behavior (`equiv-classes=0` literally means
the partitioner never ran).

Original filing follows.

**Original status:** open. **Priority: blocker** — this gates all FA/clone
work (any code change perturbs allocation patterns and re-rolls
compile outcomes), and it can produce silently WRONG code, not just
unstable output. Found 2026-07-10 while validating issue 033 stage
C (whose branch `issue033-stage-c` is blocked on this).
**Affects:** `ifa/analysis/clone.cc` / concretization / C codegen —
the assignment and binding of concrete type specializations.
**Related:** [033](033-splitter-non-idempotent-divergence.md) (the
D7 ordering audit fixed this bug-family inside fa.cc's collectors;
clone/codegen were NOT audited), the `build_setter_mark` null-deref
(fixed, `9da766ac`) — same ASLR-dependence family; the sweep's
"crash: signal N" bucket and the oliva2/stereo drift noted in 033
D7 are plausibly members too.

## Symptom / evidence chain (all reproduced at HEAD `9da766ac`)

1. **Back-to-back identical compiles differ.** Two consecutive
   `pyc -D .. builtins_batch.py` runs, same binary, same flags,
   produce **different `builtins_batch.py.c`** (143 diff chunks).
   Both happen to execute correctly at HEAD.
2. **What varies is type-specialization identity.** The generated
   headers swap concrete struct syms between list/tuple
   specializations run-to-run, e.g. one run has
   `sum(_CG_ps5270)` / `min(_CG_ps5273)`, the other
   `sum(_CG_ps5278)` / `min(_CG_ps5267)`, with the `/* list */` /
   `/* tuple */` comments swapping across those ids. The concrete
   Sym numbering (creation order) AND the callers' binding to
   specializations depend on iteration order over pointer-hashed
   structures.
3. **Some layouts miscompile.** Under the issue-033 stage-C branch
   (which only changes mid-FA allocation counts — see next point),
   `builtins_batch` deterministically prints
   `6.9169190417774516e-323` instead of `14`: the bit pattern of
   int64 14 **read as a float64**, i.e. a caller bound to the wrong
   (float) list specialization of a builtin over an int list.
4. **FA is NOT the diverging phase.** With `-l s`, the FAILING
   stage-C compile's splitting log is **byte-identical** (361,313
   lines, `diff` = empty) to a PASSING HEAD compile's log: every
   split decision matches. The divergence is entirely downstream
   of extend_analysis — clone/concretize/codegen.
5. **Perturbation sensitivity confirmed, with a subtlety.** N
   dummy `new AType()` at `initialize()` (env-gated) did NOT flip
   outcomes — unreferenced allocations are GC'd and leave no
   lasting layout shift. RETAINED allocations (the stage-C
   branch's `type_union` results cached in `union_map`) do shift
   layout and do flip outcomes. Even `-l s` alone (log-file FILE
   buffers) changes which `.c` is generated (point 1's mechanism).

## Root cause (hypothesis, strongly constrained)

Clone/concretization iterates pointer-hashed sets/maps when
creating concrete type Syms and when partitioning EntrySets into
equivalence classes (`sets_by_f`'s greedy seeds, `v->avars` Map
walks in `clone.cc initialize()`, `Sym::creators`, CS `equiv`
propagation, ...). Pointer hashes vary with heap layout, so both
the NUMBERING of specializations (cosmetic) and — the actual bug —
some first-match/greedy CHOICE of which specialization a
caller/clone binds to varies. Issue 033 D7 fixed exactly this
family inside fa.cc's collectors with `qsort_by_id`; clone.cc and
the codegen were never audited.

Note the D7 "three byte-identical runs" verification did not catch
this because it compared `-v` PASS lines (FA-level, id-ordered),
not the generated C.

## Repro

```
cd tests
../pyc -D .. builtins_batch.py && cp builtins_batch.py.c /tmp/1.c
../pyc -D .. builtins_batch.py && diff /tmp/1.c builtins_batch.py.c
# differs at HEAD; for the miscompile, build branch issue033-stage-c
# and run the same compile: ./builtins_batch prints 6.9e-323, not 14
```

## Proposed fix

Ordering audit of clone.cc + concretize + C codegen, in the D7
style: every iteration feeding (a) Sym creation order, (b)
equivalence-class seeding (`sets_by_f`), or (c) caller-to-clone
binding must run over id-sorted copies (`qsort_by_id`) or otherwise
canonically ordered data. Then the stronger invariant becomes
testable and should be locked in the harness:

- **Determinism gate:** compile every test twice and byte-diff the
  generated `.c` — cheap, catches the whole family forever.

## Verification plan

- `builtins_batch.py.c` byte-identical across repeated compiles at
  HEAD, under `-l s`, and under the issue033-stage-c branch; the
  branch's compile then produces `14` (or reveals a genuine stage-C
  defect to fix there).
- Full suites (C + LLVM + ifa-test) with the double-compile diff
  gate enabled; corpus sweep member set unchanged or better
  (watch the "crash: signal N" bucket — some members may be this
  bug).

## What it unblocks

- Issue 033 stages B/C (branch `issue033-stage-c`, currently
  blocked): on that branch the three diverging examples reach a
  genuine fixed point with the stall guard never firing (fysphun
  20 passes/0 violations, kmeanspp 21/6, pylife 13/52 vs 60), so
  landing it is valuable as soon as its validation can be trusted.
- Trustworthy suite verdicts for ANY analysis-side change.
