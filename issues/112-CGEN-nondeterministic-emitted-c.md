# 112 — two identical pyc invocations emit different C

**Status:** open, **root-caused** 2026-08-22 (see below); not yet
fixed. Found while building ifa/issues/111's differential harness.
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

## Root cause (traced 2026-08-22)

`ifa/optimize/cfg.cc:106` builds the reverse CFG edges with **`set_add`**:

```c
for (PNode *p : pn->cfg_succ) p->cfg_pred.set_add(pn);
```

A `set_add`-populated `Vec<T*>` iterates in **heap-layout order**
(`vec.h` says so explicitly), and pointer values move run to run — so
`cfg_pred` iteration order is nondeterministic. That is the same family
as issue 035's `forward` open-hash set, which `propagate_out_change`
already sorts around for exactly this reason.

It reaches the emitted C through `Fun::collect_Vars` (`if1/fun.cc:111`),
whose worklist walks `cfg_pred`:

```c
for (PNode *p : nodes->v[i]->cfg_pred) if (sv.set_add(p)) nodes->add(p);
```

so `vars` comes back in a run-dependent order. `cg.cc:2374` then numbers
temporaries in exactly that order —

```c
snprintf(s, sizeof(s), "t%d", index++);
```

— which explains the renumbering. And the declarations are sorted
afterwards by `defs.qsort(lt_type_id)`, a **type-id** comparison: for
the many vars sharing a type id the sort is not a total order, so their
relative order still falls back to the (nondeterministic) input order.
That explains the reordering, and plausibly the relocated getter.

## Fix direction

Two candidates, deliberately not chosen yet — this was diagnosed while
deciding whether to fix it before ifa/issues/111 M3, and the answer was
no (see below).

1. **Sort `cfg_pred` once after CFG construction.** Most fundamental:
   any consumer of reverse-CFG order becomes deterministic. Also the
   riskier one — dominators, liveness and inlining all read `cfg_pred`,
   so this can shift emitted C broadly and needs its own full
   re-verification.
2. **Sort `vars` by id in `collect_Vars` before numbering.** Narrower,
   fixes the naming and (with a tie-break added to `lt_type_id`) the
   ordering, without touching analysis order.

Option 2 is the smaller blast radius; option 1 is the honest fix. Decide
with a measurement of how much emitted C option 1 moves.

### Sequencing (2026-08-22)

**Not fixed before ifa/issues/111 M3, on purpose.** Only 1 of 41 corpus
programs measured so far is UNSTABLE, so 111's harness still covers
~97% of the corpus, and for unstable programs it falls back to
comparing FA state — which is precisely what M3 changes. Fixing this
first means changing CFG or var ordering, which can move emitted C
across many programs and would force re-baselining M1's measurements
immediately before M3 perturbs FA. One change at a time.

## Superseded suspicion (kept: it was right in family, wrong in place)

First guess was clone/emission ordering — the shape
[closed/035](closed/035-nondeterministic-codegen-clone-order.md) fixed
before. Right family (a `set_add`-populated `Vec` iterating in
heap-layout order), wrong stage: it is the reverse-CFG edge set, built
during CFG construction, well upstream of clone.

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
