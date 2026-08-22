# 112 — two identical pyc invocations emit different C

**Status:** open, found 2026-08-22 while building ifa/issues/111's
differential harness.
**Affects:** codegen emission order (`ifa/codegen/cg.cc` and/or the
clone ordering feeding it). NOT flow analysis — FA's converged state is
reproducible on the affected program.

## Symptom

`shedskin_examples/msp_ss`, same binary, same source, no environment
differences, three consecutive runs:

```
$ for i in 1 2 3; do pyc -D . msp_ss.py >/dev/null 2>&1; md5sum msp_ss.py.c; done
16fead12298f5fda...
b7b08b00c7bdcb63...
23222b9d512127c6...
```

Three different C files. The compiler is not reproducible.

## What actually differs

Both runs emit **40211 lines** — the difference is not size.

Raw diff: **414 changed lines**, almost all local temporary
declarations reordered and renumbered:

```
<   _CG_void_type t82;        >   _CG_void_type t81;
<   _CG_void_type t51;        >   _CG_void_type t35;
```

Normalising every `t<N>` to `tN` collapses that to **4 lines** — so the
bulk is pure renaming. But the residue is NOT cosmetic: a declaration
and its statement MOVE BETWEEN FUNCTIONS.

```
7632  <   _CG_void_type tN;
7805  <   tN = (_CG_void_type)((_CG_ps21330)tN)->e23; /* comTxRx */
39014 >   _CG_void_type tN;
39116 >   tN = (_CG_void_type)((_CG_ps21330)tN)->e23; /* comTxRx */
```

The same getter on `comTxRx` is emitted around line 7805 in one run and
line 39116 in another — i.e. attributed to a different clone. That is
an ordering difference in what gets emitted where, not just in how
temporaries are numbered.

FA's own converged state is IDENTICAL across runs
(`final_pass=35 pass_limit_hit=0 violations=454 ess=891 css=2726`), so
whatever is unordered lives downstream of the fixed point.

## Why it matters

1. **Reproducible builds.** Two builds of the same source give
   different objects.
2. **It breaks differential testing.** ifa/issues/111's harness
   compares emitted C between two compiler configurations; on an
   unstable program that comparison is not evidence about the change
   under test. The harness now runs a determinism control (the same
   configuration twice) and reports such programs UNSTABLE rather than
   as divergences — without that, every one of them would read as a
   regression caused by whatever change is being evaluated, pointing
   the investigation at the wrong place.
3. It hides real regressions in exactly the same way: a genuine
   codegen change to an unstable program cannot be distinguished from
   its own noise.

## Suspected cause

Iteration over a pointer-keyed hash container somewhere between clone
and emission — the classic shape for this, and the one
[closed/035](closed/035-nondeterministic-codegen-clone-order.md) fixed
before for clone order. `Vec::set_add`-populated vectors iterate in
heap-layout order unless explicitly sorted (`vec.h` says so), and
`fa.cc` already sorts in several places for exactly this reason
(`qsort_by_id`, and issue 035's note in `propagate_out_change` about
`forward` being an open-hash set).

Not yet traced to the specific container. The `comTxRx` getter above is
a concrete probe point.

## Verification plan

- The three-run md5 check above yields one hash.
- `ifa/tests/selective_diff.sh` reports `unstable: 0` corpus-wide.
- A corpus-wide N-run reproducibility sweep, since msp_ss was found by
  accident and the true population is unmeasured.

## What this unblocks

Trustworthy differential testing, which ifa/issues/111 M2/M3 depends
on, and reproducible builds generally.

## Related

- [closed/035](closed/035-nondeterministic-codegen-clone-order.md) —
  nondeterministic clone order, same family, fixed. Worth re-reading
  first: this may be a surviving path of the same defect.
- [closed/009](closed/009-fa-violations-nondeterminism.md) — FA
  violation-order nondeterminism, also fixed. FA state is stable here,
  so this is not that.
- [111](111-FA-selective-invalidation-per-pass.md) — found by its
  harness; that harness now works around this rather than waiting on it.
