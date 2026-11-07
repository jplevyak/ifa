# 206 — a field read through a `{R(int), R(float)}` receiver reinterprets the int's bits

**Status: open.** Filed 2026-11-06, found while fixing
[200](200-llvm-backend-parity.md) A5. A silent wrong answer on the default
path, on **both** backends.

## Symptom

```python
import sys
class R:
    def __init__(self, v): self.v = v
r = R(3) if len(sys.argv) > 1 else R(4.5)
print(r.v)
```

Run with one argument. CPython prints `3`; pyc prints `1.5e-323` on the C
backend and under `-b`, with only the permissive int/float warning.

## Mechanism

`R` splits correctly into an int-`v` contour and a float-`v` contour. `r`
holds the union, and the read is emitted through ONE member's layout
(`resolve_union_receiver` picks the first that has the field):

```c
t1 = (_CG_float64)((_CG_ps21206)t10)->e18;   /* v, through the float R */
```

The int `R`'s `e18` is an `int64` 3, read as a double. The layout contract
(closed/122, `cg_layout_mismatch`, now in `codegen_common.cc` and run by
both backends since 200 A1) records exactly this obligation and accepts
it, because it compares member WIDTHS only, and `int64` and `float64` are
both 8 bytes. Width is what the members before the accessed slot need (it
keeps offsets equal); the accessed slot's bits are then interpreted as the
cast-to type, which needs the representation to agree too.

## Dead end: tighten the contract at the read slot alone

Refusing a float-vs-non-float accessed slot (measured 2026-11-06) turns the
repro into a refusal on both backends, but also refuses
`tests/method_slot_registered_on_concrete_type.py`, which is correct as
compiled. There `V::add`'s clone `_61` types `self` as the union
`{V(float), V(int)}` and reads it through the float layout, but `_61` is
installed only in the float `V`'s method slot; the int `V` gets clone
`_39`. So only float `V`s ever reach it, and the cast is sound. A variant
with `V(7, 0, 0, 0)` prints CPython's answer on both backends.

The obligation records every union member that HAS the field
(`cg_note_blind_cast`'s filter), not the members that can REACH the access.
Tightening the check without fixing that refuses a sound program to catch
an unsound one.

## Plan

1. The obligation should name what reaches. For a slot-dispatched clone,
   that is the classes whose slot holds this clone; that answer lives in
   `cg_build_new_to_val_map`'s registrations. With that, the stricter
   read-slot rule refuses the repro and accepts the method-slot test.
   Stop condition: the suite and the corpus `check` sweep are unchanged
   apart from programs that print a reinterpreted value today.
2. The refusal is not the end state. The union at the read is a demand (a
   use that cannot proceed on `{R(int), R(float)}`), and the answer is the
   class-tag dispatch the method call already gets, or a split of the
   contour that reads `r.v`. Rule 10 of the README: refuse first, then
   satisfy the demand.

## Verification

The repro prints `3` on both backends, or is refused with a layout message;
`method_slot_registered_on_concrete_type` passes unchanged; `make test`
green on both backends.
