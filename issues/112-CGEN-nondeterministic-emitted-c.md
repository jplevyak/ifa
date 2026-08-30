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

**There is exactly one structural difference**, and the raw diff hides
it: normalising every `t<N>` to `tN` reduces ~700 changed lines to 4,
but those 4 are a *deletion and an insertion of the same string* —

```
<   tN = (_CG_void_type)((_CG_ps21729)tN)->e25; /* comTxRx */
>   tN = (_CG_void_type)((_CG_ps21729)tN)->e25; /* comTxRx */
```

— so the diff on its own shows nothing. What differs is **which
function the statement is in**. Counting occurrences per enclosing
function definition:

| run | function containing the `comTxRx` getter |
|---|---|
| A | `_CG_f_13323_447` — `bslTxRx(ps21729, int64, int64, int64, nil_type, int64)` |
| B | `_CG_f_13323_448` — `bslTxRx(ps21729, bytes, int64)` |

Exactly one occurrence in the file either way. Both clones exist in
both runs and their SIGNATURES are byte-identical across runs, so the
clone numbering is stable and this is a genuine re-homing of one
statement between two differently-shaped clones of the same function —
6 parameters versus 3. The ~700 lines of renumbering are a CONSEQUENCE of that single
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

So the partition, its order, and the ES identities are all stable.

### CORRECTION (later the same day): it is NOT a codegen issue

**The section below is wrong and is kept only to show how.** It
concluded "the IR reaching codegen is deterministic" from a probe that
hashed per-Fun PNode MEMBERSHIP and phi/phy counts. `simple_inlining`
rewrites a call site's ARGUMENT LIST in place (`inline.cc`: `rvals.move`
then rebuild from the callee's send) — which changes `rvals` **without**
changing which PNodes belong to which Fun. A membership-only hash cannot
see that, so the probe reported "stable" for a stage that is not.

Adding `rvals` to the same probe settles it:

| across 6 runs | result |
|---|---|
| `rvals` after `clone` | 1 distinct — **stable** |
| `rvals` after `ifa_optimize` | 4 distinct — **UNSTABLE** |

So the nondeterminism is introduced by **`ifa_optimize` /
`simple_inlining`**, upstream of codegen. Drilling into one diverging
call (`IFA_DBG_CV_NODES=<node-set-hash>` dumps every node's rvals), the
same SEND node comes out with a different ARGUMENT COUNT:

```
pnode=31752 kind=2 rvals=20521,66251,66249,66244,66247,   (5)
pnode=31752 kind=2 rvals=66275,66249,66244,66247,          (4)
```

i.e. `inline_single_pnode` fires on that call site in some runs and not
others.

**Ruled out as the trigger:** a class's `has` order (78 record Syms
hashed after clone: 1 distinct over 6 runs). That matters because
`inline_single_sends` guards on `f->sym->has.index(fs)` and bails when
the index runs past the call site's rvals, so a varying `has` would flip
exactly this decision — and issues/121 only canonicalised promotion
order WITHIN a reanalyze round. It holds here.

### CORRECTION: interning is a real gap, but NOT the root

The section below concluded the root was "union types are never
interned". **Implemented and measured: it does not fix msp_ss.**

The claim rested on a type-identity relation hashed by *Sym ordinal*,
which cannot tell "same union, two Syms" (an interning failure) from
"genuinely different unions". Distinguishing them needs a STRUCTURAL
signature — and it has to be renaming-invariant all the way down, i.e.
built from component NAMES, not ids, since an id-based "structural"
hash walks straight back into the same trap one level lower.

With that hash, after `clone`, over 6 runs:

| | distinct |
|---|---|
| type identity relation (by Sym ordinal) | 5 |
| type STRUCTURAL signature (by component names) | **5 — also unstable** |

Both move together, so the types Vars receive after `clone` **genuinely
differ in structure** between runs — a Var's union contains a different
set of classes, not the same set behind two Sym objects. Interning
cannot help with that, and measurement agrees: with interning in place
the structural signature was still 5-of-6 distinct, and the emitted C
6-of-8.

The interning patch was therefore **reverted** rather than shipped: no
demonstrated benefit, and it changes type identity globally. It remains
worth doing once the real root is fixed (`make_LUB_type` is still a
default no-op, so structurally equal unions really are distinct Syms) —
key it on the sorted component list, which `compar_syms` already
provides. **Filed separately as
[120](120-union-types-are-never-interned.md)**, with the measurement:
1323 SUM Syms for 27 distinct unions on msp_ss.

**So the search moves upstream again:** what makes `clone` assign
structurally different types to the same Var across runs?

### Staged first-divergence trace (2026-08-30) — it is `clone_functions`

Built the trace the id-contamination argument implies: **id hashes are
useless as an aggregate verdict but exactly right as a chronological
tripwire**, because up to the first divergence every counter has handed
out the same numbers to the same objects. So the first differing record
IS the divergence rather than its wake.

**Stage 1 — FA (`IFA_DBG_FATRACE`).** One record per pass, hashing every
AVar's complete `out` CreationSet set (160k AVars, 13 passes):

    6 runs -> 1 distinct trace

**FA is deterministic** — not merely its five counters, which is all the
2026-08-22 claim rested on, but its full converged per-AVar type state.
That claim is now properly established rather than assumed.

**Stage 2 — clone stages (`IFA_DBG_CLONE`).** One record per stage,
hashing the type ASSIGNMENT (per-CS `cs->type`, per-Var `v->type`,
projected through first-encounter ordinals):

| stage | distinct over 6 runs |
|---|---|
| `initialize` / `layouts` / `clones` / `concrete-types` | 1 — stable |
| **`clone-functions`** | **5** — matches the 5 distinct `.c` |

**Stage 3 — inside `clone_functions`.** The fun processing order is
stable (both the `fa->funs` input and the `compar_fun_nesting` result).
A per-fun record puts the first divergence at record 652 of 711 — one
specific clone, same fun id and var count, different types.

**Stage 4 — per Var.** Dumping that clone's Vars, sorted (an unsorted
dump reports a difference on every line, since the map's own order is
hash order — the probe has to be canonicalised too, or it becomes the
noisy thing):

    var=11232  navars=127   (run 1)
    var=11232  navars=251   (run 3)

A genuinely different NUMBER of AVars survives clone fixup for the same
Var in the same clone — a different set, not a different order. That set
is what `concretize_var_type` reads to compute the Var's type.

### Two more hash-order sources canonicalised (landed)

Both are walks that *decide types*, and both were in hash order:

1. **`concretize_var_type` / `concretize_var_list_type`** walked
   `v->avars` — an `AVarMap` keyed by EntrySet POINTERS — by raw slot
   index. The first CreationSet seen becomes the Var's type, and the
   union is accumulated in visit order, while `new_Sym()` is called as
   it goes. Now walked in AVar-id order (`canonical_avars`).
2. **`fixup_clone_vars`** walked `f->ess`, a Vec-as-set, and its body
   REPLACES `v->avars` each iteration (`move`), so each pass re-filters
   the previous result and the LAST EntrySet decides what survives. Now
   walked in id order.

**Neither fixes msp_ss on its own** (still 7 distinct of 8), and that is
expected rather than discouraging: timsort took two independent fixes
before it fell, with no visible improvement after the first. Both are
kept because they are the same class as the three already landed —
canonicalising a hash-ordered walk that feeds a decision — and all five
gates stay green with timsort still stable.

Not yet confirmed: which of `fixup_clone_vars`' two branches produced
the 127-vs-251 split (`fixup_var` for `nesting_depth == f+1`, or the
`f->ess` loop for the rest). `fixup_var` filters by `ess->set_in(...)`,
a membership test that should be order-free, so the `f->ess` loop is the
better suspect — but the attribution is inference, not measurement.

**Next step:** instrument `fixup_clone_vars` itself — record, per (Var,
clone), the branch taken and the surviving AVar count, and diff. That
names the branch, and from there whether the input set or the filter is
what moves.

### (superseded) ROOT CAUSE: union types are never interned

Measured after `clone`, over 6 runs:

| | distinct |
|---|---|
| Var ids in rvals/lvals | 1 — stable |
| Var TYPE Sym **ids** | 5 |
| Var type **identity RELATION** (renaming-invariant) | **3 — unstable** |

The third row is the one that matters, and it needs the
renaming-invariant hash (number each distinct type Sym by first
encounter, hash the ordinals) — raw ids differ whenever `new_Sym()` is
called in a different order, which is mere renaming. The relation
itself — *which Vars share a type Sym* — genuinely varies.

`IFACallbacks::make_LUB_type` is the **default no-op** (`ifa.h`:
`virtual Sym *make_LUB_type(Sym *s) { return s; }`), and nothing
overrides it. So `concretize_avar`/`concretize_var` (`clone.cc`) mint a
**fresh Sym for every union** they build: two structurally identical
unions are the same Sym only when they happened to be concretized
together. Type identity is therefore a function of grouping order, not
of structure.

Both consumers compare types **by pointer**:

- `inline_single_sends` guards with `p->rvals[i]->type == v->type` and
  with `type_kind == Type_SUM` bail-outs, and
- codegen selects C types and casts from the same Syms.

Measured consequence: the second `simple_inlining` call makes **exactly
2109 decisions in every run** but with a different hash of
`(kind, caller, pnode, callee)` in 5 of 6 runs — the same NUMBER of
inlines landing on different call sites.

### The full chain, root to symptom

1. `clone` concretizes union types without interning → which Vars share
   a type Sym varies.
2. `inline_single_sends`' `==` guards flip → same count, different call
   sites inlined.
3. Those rewrite different call sites' argument lists → `rvals` after
   `ifa_optimize` differs (stable after `clone`, 4 distinct after).
4. `Fun::collect_Vars` returns a different var SEQUENCE.
5. `cg.cc` numbers temporaries in that order, and its `!cg_get_string(v)`
   first-claimant rule picks which clone declares a Var shared between
   clones; the getter emission skips a statement whose destination has
   no C string.
6. One `comTxRx` getter is emitted in a different clone, and every
   later `t<N>` in both clones renumbers — ~700 changed lines from one
   root.

### Proposed fix

**Intern union Syms by their component list.** The components are
already sorted (`compar_syms`, above), so the sorted id list is a
canonical key: build it, look it up, and return the existing Sym instead
of minting a new one. Structurally equal unions then compare equal by
pointer, and every `==` guard downstream becomes order-independent.

Scope warning: this changes type identity globally, so it needs the full
gate set plus a corpus `check` sweep, not just msp_ss.

### Methodology note — id renaming fooled this investigation twice

Two intermediate conclusions here were wrong because a hash over
`->id` cannot distinguish "different order/content" from "same content,
ids assigned in a different order":

- "FA is nondeterministic" (it was two OSC lines misread as one), and
- "collect_Vars returns different var COUNTS" (the counts and multiset
  match at the FIRST divergence; the later differences are cascade).

Any probe here must either compare a renaming-invariant projection, or
be pinned to the FIRST diverging record rather than an aggregate.

### (superseded) It looked like a CODEGEN issue — the IR reaching codegen is stable

`IFA_DBG_BODIES=1` (added) fingerprints, at named pipeline points, both
the per-Fun BODY MEMBERSHIP (which PNodes belong to which Fun) and
`fa->funs` order — the latter matters on its own because `cg.cc:2761`
emits bodies with `for (Fun *f : fa->funs)`, so it *is* emission order.

| measured over 4 runs | result |
|---|---|
| body membership after `clone` | 1 hash — stable |
| body membership after `ifa_optimize` (incl. `simple_inlining`) | 1 hash — stable |
| `fa->funs` order at both points | 1 hash each — stable |
| emitted C | 3-4 distinct |

So inlining is not the cause and the IR handed to codegen is
deterministic. The instability is **`Fun::collect_Vars`**
(`if1/fun.cc`), whose var sequence codegen consumes twice over:

- `cg.cc:2448` numbers temporaries `t0, t1, ...` in that order; and
- the same loop's `!cg_get_string(v)` guard means the FIRST function to
  reach a Var shared between clones is the one that declares it — and
  the getter emission then SKIPS a statement whose destination has no
  C string (`if (!cg_get_string(n->lvals[0])) goto Lgetter_found;`).

That second point is how one statement changes clones: it is emitted
wherever its destination Var was named first.

Splitting `collect_Vars` into its two inputs (probe prints both):
**the CFG walk (`nodes`) is stable** — the `cfg_pred` sort above works —
while the var sequence is not. `rvals`/`lvals` are ordered Vecs and this
caller passes `NO_TVALS`, which leaves `phi`/`phy` content and Var
identity as the remaining candidates.

**Tried and did NOT fix it:** sorting the dominance frontier
(`dom.cc`, `x->front`) by `Dom::id` after its `set_to_vec()`. `place_phi`
(`ssu.cc`) walks `front`, and issue 035 had already added `Dom::id`
noting that frontier order drives "phi placement order, hence Var
creation order and every downstream id" — so this looked exactly right
and is kept (it is a real hash-order source in the same family), but
msp_ss is unchanged by it.

### Measurement caveat, learned the hard way

**Three runs is not enough to call this stable.** One 3-run sample came
back with a single hash; the very next 8-run sample of the same binary
gave **7 distinct outputs**. Use 8+ runs before concluding anything is
fixed here.

`IFA_DBG_PARTITION=1` dumps the settled clone partition (per Fun, each
equivalence class as an id list, in order). `IFA_DBG_BODIES=1` dumps the
body/emission-order fingerprints and the `collect_Vars` split.

The population figure in the section below predates all of this and
needs re-measuring: it was 2 of 68 unstable, and `timsort` has moved
out of it.
