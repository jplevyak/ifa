# 131 — Demand-driven constant splitting

**Status:** open, planned. Blocks
[128](128-cs-identity-over-discriminates-vs-element-type.md)'s
start-merged posture (`PYC_CSDCPA1`) and therefore
[129](129-plan-demand-driven-creation-set-splitting.md) step 4.

## Symptom

Under `PYC_CSDCPA1=2` (one CreationSet per sym,
[129](129-plan-demand-driven-creation-set-splitting.md)),
`tests/empty_list_print.py` miscompiles:

```python
b = [2, 3];  print(b)     # [2, 3]
k = [];      print(k)     # expected []   got [0, 0]
```

Both literals are `list<int>`, so there is **no type distinction to
observe** — the two differ only in a *constant*, the length 2 against 0.
At the default this is separated by `TYPE_CONFL`, but only because
per-site CreationSet identity had already put the two constants in
different contours; nothing asked for the separation by demand. With one
contour per sym only `SETTER` fires and it does not separate them.

## Step 1 was run, and it FALSIFIED the root cause below

*2026-09-05. The plan below is kept because its shape and its stop
conditions are still right, but its premise — that the constant cap-strip
is the demand signal — is measured wrong. Do not build steps 2-4 on it.*

Two probes were added (`cstrip=strip/multi/same` and `capstrip`, both on
the `DEMAND` line, both probe-only):

| | `empty_list_print` default | `PYC_CSDCPA1=2` |
| --- | --- | --- |
| `cstrip` (CS fields losing a constant) | 0/0/0 | 0/0/0 |
| `capstrip` (times the cap-strip fires) | 6 | **5** |

No CreationSet field loses a constant to the cap, on the very program the
mechanism was designed for. And the cap-strip fires **fewer** times under
the flag than at the default, so it cannot be what the flag breaks.

**What actually differs is in the emitted C.** At the default there are
two `list::__str__` clones and the empty list's takes *no arguments at
all* — it is fully folded:

```c
_CG_string _CG_f_3088_3 /*list::__str__*/ ();               // the [] clone
_CG_string _CG_f_3088_11/*list::__str__*/(_CG_ps11690 a1);  // the [2,3] clone
```

Under `PYC_CSDCPA1=2` there is one clone, and the two literals are built
as:

```c
t5 = _CG_prim_tuple_list(_CG_ps11690, 2);        // b = [2, 3]
t1 = (_CG_ps11690)_CG_prim_tuple_list(int*, 0);  // k = []
```

The empty literal is constructed with element type `int*` and **blind-cast**
to the merged list type. That is the
[123](123-CGEN-union-receiver-field-access-has-no-discrimination.md)
family — a representation mismatch between two things codegen was told
are one type — and it is why
`k` prints `[0, 0]`: the merged `__str__` reads a length through a value
that was never laid out as that type.

**So the question changes.** Nothing in FA observes a *type* distinction
between `[]` and `[2, 3]`; both are `list<int>`. Their separation at the
default is a side effect of per-site CreationSet identity, and clone then
specialises per CS because they happen to be separate. Under a strict
demand discipline the two SHOULD merge, `__str__` should read the length
at runtime, and `k` should print `[]`. That it prints `[0, 0]` is a **bug
on the merged path**, not evidence that a split was owed.

Which makes the next step root-causing the blind cast, not building a
splitter — and only then asking whether any split is owed at all. Adding
constant splitting to make this test pass would be the retreat this repo
names: the numbers would improve and the real defect would stay.

## Root cause (as originally filed — see above, this is NOT confirmed)

`num_constants_per_variable` is **1** (`fa.h:829`). When an AVar's type
holds more constants than that, `type_cannonicalize` **strips every
constant** and rebuilds the type from their base types (`fa.cc:1022`).
So `{0, 2}` becomes `{int64}` and the length is gone. The strip is
silent: no violation, no warning, no stage.

pyc's existing answer is a **static, frontend-driven opt-in**. A ctor
param annotated `__pyc_clone_constants__` sets `Sym::clone_for_constants`,
which marks the class `clone_methods_per_cs`
(`python_ifa_build_syms.cc:2855`) and turns on three behaviours:

| site | what it does |
| --- | --- |
| `edge_constant_compatible_with_entry_set`, `fa.cc:1490` | an edge whose constant differs from the EntrySet's is incompatible |
| `entry_set_compatibility`, `fa.cc:1672` | that incompatibility is **soft** (`val -= 1`) — **hard** (`return 0`) only for `clone_methods_per_cs` |
| `collect_type_confluence`, `fa.cc:5350` | compares the unstripped `av->in` / `x->out`, not the constant-stripped `->type` |

**And the code already records why it cannot simply be turned on for
everything** (`fa.cc:1682`):

> *"Scoped to the new opt-in flag: making this hard for ALL
> `clone_for_constants` functions (`list.__getitem__` keys etc.) would
> eagerly fan out contours that today only split on violation evidence."*

That is the whole problem in one sentence. A per-class decision cannot
work, because **one class covers both cases**: some `list` and `tuple`
values have a statically known arity and some do not. `[2, 3]` wants its
constant; a list built by `append` in a loop has no constant to want, and
demanding one fans out a contour per iteration count. The decision has to
be per *CreationSet*, and it has to be driven by evidence.

## The plan

### The demand signal is the cap-strip itself

`fa.cc:1022` is the exact instant a merge destroys a constant, and it
already computes `consts`. Today it strips unconditionally. The signal:

> **When the cap-strip is about to fire on an AVar, ask whether those
> constants are attributable to DISTINGUISHABLE creation points. If they
> are, that is demand: split instead of stripping. If they are not, strip
> exactly as today.**

Unknown arity self-identifies under this rule and needs no special case.
A loop-built list accumulates `0, 1, 2, 3…` from **one** creation point:
there is nothing to split, so the strip fires and the length becomes a
plain `int64` — today's behaviour, reached deliberately rather than by
accident. `[2, 3]` and `[]` accumulate two constants from **two**
creation points: separable, so split.

The same rule states the tuple case without a second mechanism. A tuple's
arity is its slot count rather than a field constant, so the reading
differs, but the question does not: arity is an observable integer
property of the creation point, split when two creation points disagree
and it is separable, degrade to a widened tuple when it is not.

### Step 1 — measure before building anything

Instrument the cap-strip (`fa.cc:1022`) and report on the `DEMAND` line:

- `strip` — how many times the cap-strip fires,
- `stripmulti` — of those, how many have constants reaching the AVar from
  more than one creation point (the separable population),
- `stripsame` — from exactly one (the unknown-arity population).

Take it at the **default** and under `PYC_CSDCPA1=2`, corpus-wide. This
is the ceiling on the whole issue, and it decides the shape of what
follows.

*Stop condition:* if `stripmulti` is ~0 at the default, this mechanism is
worth building only for the start-merged posture and must not be turned
on for the default — say so and scope it to the flag.

### Step 2 — turn `clone_for_constants` from a static flag into a demand bit

The three sites above all key on `av->var->sym->clone_for_constants`, a
property of the *frontend's annotation*. Add a per-AVar
`wants_constants` bit meaning *"a strip on this AVar would lose a
constant that distinct creation points disagree on"*, set by step 1's
detector, and make each of the three sites consult `flag || bit`.

`collect_type_confluence` (`fa.cc:5350`) is the load-bearing one: it is
why a constant-only difference produces **no confluence** today for an
unannotated var, and therefore why nothing ever asks for the split. With
the bit set, the constant difference becomes an ordinary confluence and
the existing ladder — `TYPE_CONFL`, then `SETTER`, then `MARK_SETTER` —
already knows what to do with a confluence. See
[129](129-plan-demand-driven-creation-set-splitting.md): the third clause
("split ESs so the creation points separate") is **already implemented**
and fires; it simply never sees this distinction.

The bit is per-AVar and therefore per-contour, which is what makes it
demand-driven where `Sym::clone_for_constants` is not.

### Step 3 — bound the fan-out, and make it survive a pass

`fa.cc:1682` names the failure mode, so it must be answered before the
bit goes anywhere near the default:

- **Cap the fan-out.** A hard limit on the number of constant-split
  contours per creation point (shedskin uses 10). Beyond it, clear the
  bit and strip — permanently for that AVar, so the decision does not
  oscillate.
- **Ledger the split**, the way `split_css` already does through
  `cs_group_signature` → `ledger_find_cs` (`fa.cc:7603`, `5231`), so a
  re-derived constant split re-attaches to the contour it first made
  instead of minting a fresh one every pass. The existing signature
  deliberately uses a *constant-stripped* value type; a constant split
  needs a signature that does not, which is a new key and not a tweak to
  that one.
- **Raise `num_constants_per_variable` only if measured.** It is 1
  (`fa.h:829`). Splitting rather than stripping may make a higher cap
  affordable, but that is a separate measurement and must not be bundled.

### Step 4 — verify

- `tests/empty_list_print.py` prints `[]` under `PYC_CSDCPA1=2`.
- `tests/list_element_type_union.py`, `tests/listcomp_element_separation.py`
  — the other constant/element separation cases in the flag's 30.
- The `__pyc_clone_constants__` classes still behave: `range(0, 0)` vs
  `range(0, 2)` stay separate (issue 040's chain), and
  `tests/deepcopy_copy_of_copy_chain.py` does not regress.
- A loop-built list still reaches a plain `int64` length and does **not**
  fan out — the unknown-arity case, asserted explicitly with a new test.
- Corpus `check` neutral at the default; under `PYC_CSDCPA1=2` the 20
  compile failures fall.
- `ess`/`css` do not grow at the default. This mechanism exists to let
  contours MERGE safely; if it adds contours where nothing merged, it is
  doing the opposite of its purpose.

*Stop condition:* if bounding the fan-out is what makes the numbers work,
that is a retreat — the cap exists for genuinely unbounded cases
(unknown arity), not to paper over eager splitting. If the cap is doing
the work, the detector in step 1 is wrong and should be fixed instead.

## What this unblocks

[128](128-cs-identity-over-discriminates-vs-element-type.md)'s
start-merged posture. `PYC_CSDCPA1=2` already gives **−32% container
CreationSets** corpus-wide (3748 → 2540, ratio 5.99 → 4.46) and, unlike
every key-side experiment in
[129](129-plan-demand-driven-creation-set-splitting.md), takes `ess`
*down* rather than up. Its bill is 18 new compile failures and 30 suite
failures, and constants are the largest identified share of them.
