# ifa/151 — split an EntrySet on a constant argument, on demand

> **CLOSED 2026-09-28 (landed).** The CONST_DEMAND stage (EntrySet side) landed 2026-09-26. What is left is removing the remaining annotations, tracked in [134](../134-remove-the-frontend-forced-split-opt-in.md).
>
> *Archived during the 2026-09-28 issue consolidation. The text below is the historical record and is not maintained.*

**Status:** BUILT 2026-09-26 (the EntrySet half). `int`'s arithmetic,
bitwise, in-place and formatting methods no longer carry
`__pyc_clone_constants__`; their constants are split only on demand. The
rest of the annotations remain -- see "What is left" below. Previously:
open, root-caused, not built. Sequenced as
[129](../129-plan-demand-driven-creation-set-splitting.md) step 4 — 129 is the
single integrated plan. Filed 2026-09-12 out of
[150](150-is-not-none-never-folds.md), whose fix needed a per-constant
contour and could only get one by hand-annotating `__pyc__`.

The counterpart to [131](131-demand-driven-constant-splitting.md), which is
about the **CreationSet** side (a container's arity: `[]` against `[2, 3]`).
This is the **EntrySet** side: two call sites pass different constants to
one function, the contours merge, and the constant is destroyed for both.
131's own step 1 falsified its premise — the constant cap-strip measured
`0/0/0` on the program it was designed for — so the demand signal below is
not a restatement of that one, and the two issues do not overlap in
mechanism.

## LANDED 2026-09-26 -- what was built, and what it measured

### The mechanism

- **The per-contour bit.** `EntrySet::const_positions` (fa.h): formal
  positions at which a contour keeps constants apart. Set only by the new
  **CONST_DEMAND** stage, inherited by every product through the durable
  `split_origin` lineage, and consulted -- through `es_wants_constants` --
  everywhere `Sym::clone_for_constants` used to be: confluence collection
  (unstripped comparison), edge grouping (`split_type_view`'s
  `keep_consts`), ES compatibility (a demanded mismatch is a HARD
  incompatibility, an annotated one stays the old soft preference), and the
  clone phase's contour equivalence.
- **The demand.** `split_for_constant_demand` runs as the LAST rung, only
  when every other stage found nothing this pass. From each violation it
  walks backward through the value it complains about and through the
  conditions of the branches the violating statement is CONTROL-DEPENDENT
  on (`controlling_ifs`: a branch one of whose arms cannot reach the
  statement). It goes down into callees through call results and up into
  callers through formals -- but a callee entered from a call result may
  only be left back into that caller (`__pyc_to_bool__` has hundreds of
  callers). A formal whose in-edges disagree on a constant is nominated.
- **ifa/169's leak is closed.** `split_type_view` strips constants unless the
  formal wants them, so a split no longer fans per literal.

Each of those rules was forced by a measurement, recorded at the code:
running while other stages act split `g(a, k)` per literal on a transient
pass-0 BOXING violation; not climbing missed `pylife`'s `LifeNode(self, 0,
None)` / `(self, 1, None)`, which meet at the `__new__` wrapper; climbing
freely walked every `if` in the program; seeding every unfolded condition
nominated `msp_ss`'s `bslTxRx(cmd, addr, length)` (errors 213 -> 263).

### What it fixes on its own

| | HEAD | now |
| --- | --- | --- |
| `tests/match_seq_star.py` with `int.__sub__` unannotated | -- | passes: demand splits `__sub__` (`len(()) - 0`) and a boolean `__or__` |
| `tests/match_none.py`, `tests/match_seq.py` (known issues since ifa/158) | 4 / 33 errors | **pass**; `.known_issue` retired. `PYC_CONSTDEMAND=0` fails them again |
| `ifa/tests/synthetic/polymorphic_formal_3types_2each` | 6 contours of `f` | **3**, one per type (goldens re-blessed: 5 phases, this fixture only) |

### Small programs (contours; minimum by shape, ifa/169's census)

| | HEAD | now | minimum |
| --- | --- | --- | --- |
| `g(a, k)` x6 | 53 | **49** | 49 |
| `(int, int)` tuples | 84 | 78 | 69 (+ tuple sites, C) |
| `voronoi` | 213 | **171** | 120 |

### Corpus `-m check`

`sweeps/check__default__63888fd6+7e402769.tsv` (HEAD's analysis) vs
`sweeps/check__default__e70bcffa+eb5931f5.tsv` (this change):

| | HEAD | now |
| --- | --- | --- |
| contours, 45 programs compiling in both | 16688 | **15416 (-7.6%)** -- 41 down, 2 up (`plcfrs` +34, `pygmy` +74; pygmy runs to the pass cap in both) |
| compile seconds, same 45 | 544 | **494 (-9.2%)** |
| contours / compile seconds, all 77 | 32503 / 1734 | **29983 / 1650** (-7.8% / -4.8%) |
| compile_fail / run_fail / stdout_differs | 32 / 13 / 6 | 31 / 14 / 6 -- `quameon` now compiles (and aborts at run, as it did under `PYC_ESBLOCK=0` before) |
| stdout matching CPython | 14 | 14 |

The metric was speed with unnecessary contours as its measure, and the two
move together here. On the way there the change was briefly SLOWER (+12-19%
compile time) with fewer contours: a violation-reach walk run once per
violation (hq2x: 4434 transient violations on pass 1, split stages 0.015 s
-> 1-4 s a pass) and ESBLOCK's self-undoing split (softrender to the cap).
Both are fixed above; neither was a cost of splitting on demand.

### Latent defects the change exposed, all fixed with it

Removing the accidental per-literal separation surfaced three bugs that
the extra contours had been hiding. None is a reason to keep the
annotations:

1. **`pygmy` rendered one shader class's objects with the base method --
   at HEAD too.** `cg_build_new_to_val_map` (codegen_common.cc) fills an
   object's method slot with "one winner per (constructor, slot)", and on a
   specificity tie kept whichever registered first. `everythingshader.shade`
   calls `shader.shade(self, ...)`, so a `shader.shade` clone has `self =
   everythingshader` and ties with the override. HEAD gave spotshader's slot
   to `shader.shade`; after this change the order flipped and
   everythingshader lost instead. Now an override (a declared owner that
   `specializes` the other's) always wins -- and pygmy's image is
   **byte-identical to CPython's**, which it was not at HEAD. The corpus
   sweep marks pygmy "unverifiable", which is why this went unseen.
2. **ESBLOCK's EntrySet split undid itself every pass.** It separates edges
   by CREATION POINT, but passed the type-split flag, so HARDREUSE's
   route-by-type-key bound the peeled group into its type-identical sibling
   -- the one holding last pass's complement. At HEAD the edges also
   differed in literals, which kept the type keys apart by accident.
   `ESSplitDecision::type_only = false` for ESBLOCK. `softrender`: 101 passes
   to the cap and 301 errors at HEAD -> **50 passes, 8 errors**.
3. **ESBLOCK fired on facts.** A CS_DEF_PART candidate that only had a type
   confluence (or was backtracked from one) could split an EntrySet; `hq2x`
   ran to the cap on it with no violations at all. ESBLOCK now requires a
   demand: an irrepresentable element or a violation's backward reach.

Two smaller ones: `_CG_mod_impl` is one template (a bare `20000 % 2` was an
ambiguous overload once it stopped folding in FA), and the C backend casts a
record argument to a layout-compatible sibling formal (ifa/126's
`PYC_CLASSEQ=2` merge), recorded with the blind-cast contract.

### What is left

- *Updated same day:* [134](../134-remove-the-frontend-forced-split-opt-in.md)
  took the annotated lines from 32 to **9**, each kept for a named reason
  (a fold no demand can ask for, the CreationSet side, or a codegen gap).
  It also added an `index_object`-key step to this walk and a violation
  for a heterogeneous tuple indexed by a merged key.
- A demand the split answers without resolving still costs passes: on
  `msp_ss` (fails to compile either way) the stage folds `bslTxRx`'s
  `if cmd == ...` branches and the violations stay. Nothing that compiled is
  slowed by it.
- `pylife` compiles again but with 4 contours of `LifeNode.__init__`;
  shedskin's 2 needs the TypeError lowering in
  [169](169-FA-constants-leak-into-split-grouping-through-the-dispatch-filter.md).

## Symptom — five lines

```python
def g(b):
    if b:
        return 1
    return "s"
print(g(False))
print(g(True))
```

```
n1.py:1: error: expression has mixed basic types:( int64 str )
    def g(b):
  called from n1.py:5
  called from n1.py:6
```

`g` gets ONE EntrySet in which `b` is `{True, False}`, so neither branch
folds away and the return unions `{int64, str}` — which has no
representation, so it is an ERROR, not a warning. Each call site on its own
compiles clean. **The diagnostic already names both call sites**; the
information needed to separate them is in hand at the moment of the
complaint.

This is not a boxing gap. The program is statically typeable: `g(False)`
returns `int` and `g(True)` returns `str`, and CPython agrees. Per the
project directive, a union pyc invented is pyc's bug.

## Root cause — two gates, both keyed on a frontend annotation

**1. Edge/contour compatibility never looks at constants unless told to**
(`fa.cc:1848`):

```c
static bool edge_constant_compatible_with_entry_set(AEdge *e, EntrySet *es) {
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *av = es->args.get(p);
    if (av->var->sym->clone_for_constants) {        // <-- the annotation
      ...  return false;  // differing constants: incompatible
    }
  }
  return true;                                      // <-- everyone else
}
```

For an unannotated formal the body never runs and the function returns
`true` unconditionally. Differing constants are not a weak reason to keep
two edges apart — they are **not a reason at all**.

**2. A constant-only difference produces no confluence**
(`collect_type_confluence`, `fa.cc:5871`):

```c
if (av->var->sym->clone_for_constants) {
  if (type_diff(av->in, x->out) != bottom)            // UNSTRIPPED
    confluences.set_add(av);
} else {
  if (x->out->type->n &&
      type_diff(av->in->type, x->out->type) != bottom) // ->type: constants STRIPPED
    confluences.set_add(av);
}
```

`AType::type` drops constants when the base type is present, so `{True}`
and `{False}` both project to `{bool}` and `type_diff` is bottom. Stage 1
sees no distinction, so **nothing ever asks for the split** — the ladder
below it (`TYPE_CONFL` → `SETTER` → `MARK_SETTER`) is never handed the
confluence it already knows how to act on.

**3. The fan-out fear is already written down** (`fa.cc:2040`), and it is
the reason (1) is scoped rather than general:

> *"Scoped to the new opt-in flag: making this hard for ALL
> `clone_for_constants` functions (`list.__getitem__` keys etc.) would
> eagerly fan out contours that today only split on violation evidence."*

That sentence is the whole issue. Turning constants on everywhere is a fan
— partition size = the number of distinct constants, which is
[144](144-route-4-fans-per-creation-point-instead-of-partitioning.md)'s
signature. Leaving them off costs the separations above. **The missing
third option is to consult constants only where a demand is blocked on
them.**

## The workaround, and its scale

Today the only way to get a per-constant contour is for a human to write
`__pyc_clone_constants__` in `__pyc__`. That is **93 call sites across 8
files**:

```
02_numeric.py 39   04_sequence.py 8   05_builtins.py 6   07_dict.py 3
01_str.py      2   00_runtime.py  2   06_bytearray.py 1  08_set.py   1
```

([134](../134-remove-the-frontend-forced-split-opt-in.md) says "the whole
program-wide list of annotated sites is four" — that is stale by two orders
of magnitude and should be corrected there.)

It is load-bearing, not decorative: with `PYC_NO_FORCED_SPLIT=1`, the
single line `print(min(2, 9))` — clean at the default — produces **10
warnings**.

And it is unavailable to user code. `g` above cannot be fixed by its
author; [150](150-is-not-none-never-folds.md) could only be fixed because
`bool.__not__` happens to live in `__pyc__` where the annotation can be
written. A user writing the identical method gets the identical bug and no
remedy.

## What "on demand" has to mean here

Per [146](../146-remove-all-arbitrary-splitting.md)'s two-question test:

- **Would this split happen if the demand were absent?** It must not.
  `__pyc_clone_constants__` fails this today — it splits wherever a
  constant reaches the formal, asked for or not. So does any scheme that
  makes constants generally incompatible.
- **Does the demand alone decide WHETHER, with the constants only deciding
  WHICH parts?** That is the shape to build.

A constant union is a FACT. The demand is something **observing** it and
being unable to proceed. Three are already available at the point of
failure, and all three name the contour:

| demand | example |
| --- | --- |
| irrepresentable union (`BOXING`) | `g` above: `{int64, str}` |
| unresolved dispatch | [150](150-is-not-none-never-folds.md): `__lt__` on `{nil, int64}` |
| a violation whose type is a constant union | route 4's existing population |

So the rule to build is the inverse of today's: **when a violation is
raised, ask whether the contour it names has a formal whose constants
differ across in-edges. If so, that is the demand — partition the in-edges
by the constant and re-derive. If not, nothing changes.**

Note this needs no new identity component and no provenance: the constant
IS a deduced type-lattice value, which is the legitimate first column of
[136](136-creation-point-identity-is-es-x-call-site.md)'s table. The
handle naming the parts is the constant itself, not the call site.

### A second corpus instance: `life`

`shedskin_examples/life` constructs `collections.defaultdict` at two sites
with different factories — `defaultdict(int, board)` and
`defaultdict(None, board)` — so the shared contour's `self.factory` unions
`{None, int}`. `__getitem__` guards it (`if self.factory:`) and the guard
cannot fold across the merge, so the None arm is type-checked and
`self.factory()` fails. Same shape as the `min`/`max` case in
[150](150-is-not-none-never-folds.md), reached through truthiness rather
than `is None`. Surfaced by [125](../../../issues/125-in-has-no-iterable-fallback.md)'s
`list()` fallback, which let the analysis get that far.

## Plan

1. **Measure the ceiling first.** Probe, at each violation, whether the
   named EntrySet has a positional formal whose in-edges disagree on a
   constant. Report corpus-wide on the `DEMAND` line: how many violations
   are constant-separable, and what the partition size would be. If the
   separable population is ~0, this issue is not worth building and should
   say so. *This is the step 131 skipped analogues of and paid for.*
2. **Bound it by inspection, not by a cap.** If the measured partition size
   is routinely large, the detector is wrong — a cap that makes the numbers
   work is the retreat this repo names. The expected shape is 2 (a guard, a
   flag, a sentinel), and `bool` can never exceed 2.
3. **Route it through the existing machinery.** Set a per-AVar
   `wants_constants` bit (131's step 2 proposes the same bit for the CS
   side) and have `edge_constant_compatible_with_entry_set` and
   `collect_type_confluence` consult `flag || bit`. The bit is per-AVar and
   therefore per-contour, which is what makes it demand-driven where
   `Sym::clone_for_constants` is not.
4. **Ledger the split** so a re-derived constant partition re-attaches to
   the contour it first made rather than minting a fresh one each pass —
   the issue 033 stability rule that `split_css` already follows via
   `cs_group_signature` → `ledger_find_cs`. The existing signature is
   deliberately constant-STRIPPED, so this needs a new key.

## Verification

- `n1.py` above compiles clean and prints `1` then `s`.
- [150](150-is-not-none-never-folds.md)'s remaining case — a nested guard
  with a defaulted parameter — is fixed without the annotation.
- `bool.__not__`'s `__pyc_clone_constants__` can be REMOVED and 150's
  corpus result holds (adatron 0, chaos 0, yopyra 0, unresolved calls 146).
  That is the real acceptance test: this issue is done when 150's fix is
  unnecessary.
- `PYC_NO_FORCED_SPLIT=1` stops costing anything, which closes
  [134](../134-remove-the-frontend-forced-split-opt-in.md).
- `ess`/`css` do not grow at the default. This mechanism exists so contours
  can MERGE safely; if it adds contours where nothing merged, it is doing
  the opposite of its purpose.
- Corpus `check` neutral: `compile_fail`, `run_fail`, `stdout_differs`
  unchanged per program, as in 150's verification.

## What it unblocks

- [134](../134-remove-the-frontend-forced-split-opt-in.md) — 93 annotations
  and the 20 `fa.cc` sites they gate can go.
- [150](150-is-not-none-never-folds.md) — its fix stops being a
  bounded-at-2 mechanism defended on the grounds that `bool` only has two
  values.
- [128](128-cs-identity-over-discriminates-vs-element-type.md)'s
  start-merged posture, jointly with
  [131](131-demand-driven-constant-splitting.md): constants are the largest
  identified share of `PYC_CSDCPA1=2`'s remaining bill, and merging is
  exactly what destroys them.
