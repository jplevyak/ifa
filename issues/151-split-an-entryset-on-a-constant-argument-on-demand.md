# ifa/151 — split an EntrySet on a constant argument, on demand

**Status:** open, root-caused, not built. Filed 2026-09-12 out of
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

([134](134-remove-the-frontend-forced-split-opt-in.md) says "the whole
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

Per [146](146-remove-all-arbitrary-splitting.md)'s two-question test:

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
  [134](134-remove-the-frontend-forced-split-opt-in.md).
- `ess`/`css` do not grow at the default. This mechanism exists so contours
  can MERGE safely; if it adds contours where nothing merged, it is doing
  the opposite of its purpose.
- Corpus `check` neutral: `compile_fail`, `run_fail`, `stdout_differs`
  unchanged per program, as in 150's verification.

## What it unblocks

- [134](134-remove-the-frontend-forced-split-opt-in.md) — 93 annotations
  and the 20 `fa.cc` sites they gate can go.
- [150](150-is-not-none-never-folds.md) — its fix stops being a
  bounded-at-2 mechanism defended on the grounds that `bool` only has two
  values.
- [128](128-cs-identity-over-discriminates-vs-element-type.md)'s
  start-merged posture, jointly with
  [131](131-demand-driven-constant-splitting.md): constants are the largest
  identified share of `PYC_CSDCPA1=2`'s remaining bill, and merging is
  exactly what destroys them.
