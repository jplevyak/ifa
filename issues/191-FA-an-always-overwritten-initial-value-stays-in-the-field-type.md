# 191 — an initial value that is always overwritten before any read stays in the field's type

**Status:** open. Filed 2026-10-17. Not attempted.

## Symptom

A field (or list element) is given a placeholder, usually `None`, and every
path overwrites it before anything reads it. CPython never observes the
placeholder. pyc puts it in the slot's type anyway, so the slot is
`{None, int}`, and since [issues/183](../../issues/closed/183-none-or-int-in-arithmetic-reads-none-as-zero.md)
(2026-10-16) the first operator on it is refused in every mode:

```
error: expression has mixed basic types:( __pyc_None_type__ int64 )
```

Measured 2026-10-17 on `3917d829`, permissive and `--strict` alike:

| shape | pyc | CPython | test |
|---|---|---|---|
| class-body `x = None`, `__init__` sets `self.x = 5` | 6 | 6 | ([closed/078](closed/078-class-body-default-plus-init-override-permanently-unions.md)) |
| `self.x = None` then `self.x = 5`, both in `__init__` | refused | 6 | `field_none_init_overwritten_in_init.py` |
| `self.x = None` in `__init__`, overwritten by `self.setup()` | refused | 6 | `field_none_init_overwritten_by_method.py` |
| `self.x = None` in `__init__`, overwritten by the caller (`a.x = 5`) | refused | 6 | `field_none_init_overwritten_after_construction.py` |
| `xs = [None] * n`, every element overwritten with an int | refused | 3 | `list_none_prealloc_scalar_overwritten.py` |
| `self.p = None`, later an object, then `l.p.v` | 4 | 4 | (pointer: None is null, [closed/164](closed/164-nil-union-at-a-primitive-argument-is-a-nullable-pointer.md)) |

The four refused rows are `tests/*.known_issue` against CPython's output;
they flip to PASS when this is fixed. Before 183 they compiled and printed
the right answer, but only because the `None`, which pyc reads as `0`, is
never read; a program that DID read it before the overwrite got `0` where
CPython raises `TypeError` (183's own symptom). 183 traded that silent wrong answer for this refusal, which is
the right trade, and this issue is the precision that should follow.
`shedskin_examples/pisang` had this shape in an unreachable fall-off and was
edited (PYC_CHANGES.md); that edit was legitimate because its `None` was
genuinely returned on a path the types could see. These four are not.

## Mechanism

A field's AVar is keyed by `(CreationSet, field)`, with no program point.
Every reachable writer, anywhere in the program, flows into it
(`flow_vars(value, field_avar)`, and the same in `structural_assignment`),
and it only grows across the fixed point. That is what makes the whole-
program analysis tractable, and it means a write that is dominated by a
later write to the same object is indistinguishable from one that is not.
closed/078 states this in full ("Scoping Option B") and is required reading.

## Why the obvious fix is wrong

**Retracting a dominated write inside the fixed point is non-monotone.**
Early in the analysis, before `__init__`'s or `setup`'s call edges resolve,
the overwrite cannot be proved, so the `None` is included; retracting it
later breaks the monotonicity the convergence argument rests on. That is the
ifa/057 lesson (widening / `CPA_LIMIT` broke convergence) and 078's reason
for not attempting Option B.

**Not a split.** One object holds `None` then `int`: the union is temporal,
not per-object, so no contour split separates it (AGENTS.md's `bh`; 129 and
178 classify this shape the same way). Demand splitting is not the missing
mechanism, and a fix must not be dressed up as one.

**Not a representation.** issues/048 item 2's sentinel (`None` as
`INT64_MIN`) would compile these, but changes what a program that stores
`INT64_MIN` does: an accommodation, permissive-only. This issue asks for the
CPython-exact answer: the placeholder is never observed, so it is not in the
type.

## Directions (no recommendation yet)

Each is sound only if it decides BEFORE the fixed point or AFTER
convergence, never by retraction during it.

1. **Strong update in the allocating function (row 2).** Within one
   function, a store to a field of an object that function allocated (or of
   `self` in `__init__`, which `__new__` just allocated), followed on every
   path by another store to the same field with no intervening read or
   escape, is dead. That is a local, pre-FA fact about the IF1 (dominance +
   no use of the object in between, where any call taking the object counts
   as a use). It generalises 078's pre-pass from class-body defaults to
   `__init__`'s own sequential stores.
2. **After convergence, check no read can observe the placeholder (rows 3,
   4).** At convergence, for a field whose only `None` writer is the
   initializing store, ask whether any READ of the field is reachable before
   the overwriting store. That is a may-happen-before question over the call
   graph FA already has. If none is, drop the writer and re-run the pass from
   bottom (AGENTS.md: every pass re-derives from bottom and only DECISIONS
   persist, so this is a decision, recorded like the split ledger, not a
   retraction). Interprocedural, and the hard row.
3. **Definite overwrite of a preallocated list (row 5).** 164's argument for
   pointer elements, extended to scalars: `[None] * n` followed by a loop
   that stores every index in `range(n)` before any read. A loop-coverage
   fact, much narrower than 2; check whether 164's machinery already has the
   coverage half.

Order of attack: 1 (cheapest, local, likely covers most real code), then 3,
then 2.

## Stop conditions

- For 1: if killing the dominated store changes any corpus `check` verdict
  other than adding a program that now compiles and matches CPython, the
  dominance/escape test is wrong. Stop and find the case.
- For 2: if the re-run from bottom does not reach the same fixed point twice
  (the decision oscillates), it is being taken on transient types (ifa/170).
  Stop; do not add a guard.

## Verification

The four `.known_issue` tests flip to PASS on both backends, and a variant
that READS the field before the overwrite is still refused (the placeholder
is observed, so the union is real). `make test`, and a corpus `check` A/B
from one binary: every verdict unchanged or a program newly matching CPython.

## Related

- [closed/078](closed/078-class-body-default-plus-init-override-permanently-unions.md):
  the class-body-default case, fixed by a frontend pre-pass; its "Scoping
  Option B" is the design constraint here.
- [closed/164](closed/164-nil-union-at-a-primitive-argument-is-a-nullable-pointer.md):
  the `[None] * n` idiom for pointer elements.
- [issues/048](../../issues/048-none-int-field-pair-runtime-abort.md): genuine
  `{None, scalar}` unions; its sentinel is the permissive fallback, and its
  item 3 (state the initial value in a corpus edit) is what pisang got.
- [165](165-none-reaching-an-operation-is-silently-accepted.md): the opposite
  error, a `None` that does arrive.
- [issues/183](../../issues/closed/183-none-or-int-in-arithmetic-reads-none-as-zero.md):
  the operator check that turned these into refusals.
