# Issue 035: clone/codegen output is heap-layout-dependent; some layouts miscompile (int read as float)

**Status:** open. **Priority: blocker** — this gates all FA/clone
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
