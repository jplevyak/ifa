# 112 — two identical pyc invocations emit different C

**Status:** open, **partially fixed** 2026-08-30 — `timsort` and
`deepcopy_objects` are now stable, `msp_ss` is not (4 distinct outputs
of 8 runs, was 3 of 3). Three ordering sources fixed; see "Fixed
2026-08-30" below. Originally **root-caused** 2026-08-22. Found while building ifa/issues/111's differential harness.
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
- A corpus-wide N-run reproducibility sweep. **Done 2026-08-22** — see
  "Population" below; re-run it to confirm a fix.

## Population (measured 2026-08-22)

Every corpus program compiled THREE times at default settings, emitted
C compared. Three rather than two because this is intermittent — two
runs can coincidentally agree, which is how the first version of
ifa/111's harness mis-reported msp_ss as a flag divergence.

    77 programs:  66 stable,  2 NONDETERMINISTIC,  9 skipped (do not compile)

    msp_ss    3 distinct outputs of 3 runs   (always differs)
    timsort   2 distinct outputs of 3 runs   (intermittent)

So the blast radius is **2 of 68 compiling programs, ~3%** — small, and
that is what justified sequencing this AFTER ifa/111 M3 rather than
before it (see "Sequencing" above). `timsort` is the more informative
of the two: at 2-of-3 it would pass a two-run determinism check about a
third of the time, so any check for this must use three runs or more.

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

## Fixed 2026-08-30 — three sources, and the one that was not cosmetic

Prompted by issues/121, whose fix made two CreationSets genuinely
equivalent and so handed this issue's tiebreaks something to decide.
All three are the same family the root-cause section names — a
`Vec`-as-set iterating in pointer-hash order — in three different
places:

**1. `fa->type_violations` (`fa.cc:2483`, `set_add`).** The one that
was NOT cosmetic. `PycCompiler::reanalyze` promotes fields in this
order, and promotion order assigns struct slots (issues/121), so this
reached *layout*, not just temporary numbering. `show_violations`
already sorted around it for stable diagnostics; that ordering is now
available as `fa_sorted_type_violations` and `reanalyze` uses it.
Fixes `deepcopy_objects`.

**2. `cfg_pred` (`cfg.cc`, this issue's original root cause).**
`finalize_cfg` already called `set_to_vec()`, which compacts the hash
slots but *preserves their order* — so the documented fix was in the
right place and simply absent. Now `qsort_by_id` right after, which is
option 1 ("the honest fix") from the Fix direction section: one sort at
the source rather than one per consumer. Necessary for `timsort` but
not sufficient on its own.

**3. Type_SUM component order (`clone.cc`, `concretize_avar` and its
Var twin).** A union's `has` is built with `set_add`; codegen's
`resolve_union_receiver` (`cg.cc:221`) returns the FIRST component
carrying the field, so the emitted cast named a different struct each
run. Both sites now sort with `compar_syms`, matching a third site that
already did. This is what finished `timsort`.

One golden moved and was re-blessed: `container_scalar_union_add.py`
now reports `{float64, list}` where it said `{list, float64}` — the
same union, printed in the newly canonical order, stable across runs.

### Still open: msp_ss — narrowed to ONE statement

4 distinct outputs of 8 runs (was 3 of 3, "always differs"). Two
consecutive runs now compare byte-identical, which never happened
before.

**There is exactly one structural difference.** Raw diff between two
runs is ~700 lines; normalising every `t<N>` to `tN` collapses it to
**4**:

```
39053 <   _CG_void_type tN;
39276 <   tN = (_CG_void_type)((_CG_ps21729)tN)->e25; /* comTxRx */
39418 >   _CG_void_type tN;
39509 >   tN = (_CG_void_type)((_CG_ps21729)tN)->e25; /* comTxRx */
```

One `comTxRx` getter is attributed to clone `_CG_f_13323_447` of
`LowLevel::bslTxRx` in one run and to clone `_CG_f_13323_448` in the
other. The ~700 lines of renumbering are a CONSEQUENCE of that single
move — it adds a temporary to one clone and removes one from the other,
shifting every later `t<N>` in both — not an independent source. The
2026-08-22 reading ("almost all local temporary declarations reordered")
had the causality backwards.

**Ruled out by measurement, not by argument:**

| candidate | how checked | result |
|---|---|---|
| FA convergence | `PYC_DBG_OSC` over 6 runs | identical: `final_pass`, `pass_limit_hit`, `violations`, `ess`, `css`. (`PYC_DBG_OSC` prints TWO OSC lines per run, 12 and 13 — reading them as one line is what made FA briefly look unstable here.) |
| EntrySet ids | `IFA_DBG_PARTITION` (added, below) | identical |
| clone partition | same probe, classes as sorted id lists | identical over 4 runs |
| clone partition ORDER | same probe, classes UNSORTED — the order decides clone numbering, since `clone_functions` reuses the original `Fun` for the LAST set | identical over 4 runs |
| liveness / DCE | `mark_live_code` is a `do {...} while (mark_live_again)` fixed point over a transitive closure | order-independent by construction |
| the getter being dropped | `grep -c comTxRx` per run | **4 in every run** — the statement is not lost or duplicated, only re-homed |

So the partition, its order, and the ES identities are all stable, and
the difference is inside clone BODY construction or the per-clone
attribution of that one PNode. That is where the next session should
start. `IFA_DBG_PARTITION=1` dumps the settled partition (per Fun, each
equivalence class as an id list, in order).

The population figure in the section below predates all of this and
needs re-measuring: it was 2 of 68 unstable, and `timsort` has moved
out of it.
