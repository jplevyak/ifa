# 206 — a field read through a `{R(int), R(float)}` receiver reinterprets the int's bits

**Status: closed 2026-11-07.** Step 1 turned the wrong answer into a
refusal; step 2 satisfies the demand with a class-tag test at the read.
Filed 2026-11-06, found while fixing
[200](../200-llvm-backend-parity.md) A5. It was a silent wrong answer on the
default path, on **both** backends.

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

## Step 1, landed 2026-11-07: the obligation names what reaches

`cg_note_field_access` (`codegen_common.cc`) records each field read and
write, on both backends, as an obligation with the stricter rule: at the
accessed slot the members must agree on representation (float vs
non-float), not only on width. If the accessed object is a formal of the
function, or a copy of one through SSU moves, the obligation is kept
unexpanded until every call site has been emitted. Then it is checked only
against the types that can REACH that formal (`cg_build_reach`):

- every call edge (`Fun::calls`) passes the operand at the formal's
  position, so its type is a superset of what arrives;
- a formal passed straight on to a callee's formal contributes what reaches
  IT, solved as a fixpoint over the call graph (`lerp(self, ..)` calls
  `dest.sub(self)`, so what reaches `sub`'s `r` is what reaches `lerp`'s
  `self`);
- at a site emitted as slot dispatch (`cg_note_dispatch_site`, recorded by
  both backends' dispatch emitters with the candidates they call DIRECTLY),
  the receiver of a clone called through the slot is one of the classes
  whose constructor stored that clone (`PolymorphicSlot::classes`, filled
  in `cg_build_new_to_val_map`);
- unknown, so the formal's whole type, when the function escapes as a value
  (a prototype store is what an unregistered constructor's objects inherit,
  and an indirect call passes anything), when an edge has no operand at the
  position, or when nothing reaches the formal; unknown propagates along
  the same edges.

The evidence is positive throughout: a call path the reach does not know
about falls back to the whole type, which can only refuse more. It can
never accept more.

`IFA_DBG_LAYOUT` prints each formal obligation's reach (`LAYOUT reach`) and
each call edge it was built from (`LAYOUT edge`). The error now names the
function, and its line when it has one.

**Results.**

- **The repro** is refused on both backends
  (`tests/union_receiver_field_float_vs_int.py`).
- **`tests/method_slot_registered_on_concrete_type.py`** is still accepted:
  clone `_61`'s `self` is reached only through the float `V`'s slot.
- **A second shape**, interprocedural, is
  `tests/union_argument_field_float_vs_int.py`: `P.t` holds an int-`z` and a
  float-`z` `V`, and `V.lerp`'s `dest.sub(self)` dispatches on DEST's slot.
  So the `sub` clone an int-`z` dest installed is called with `r` = lerp's
  `self`, which may be either, and it read `r.z` through the float layout.
  With the check temporarily disabled, both backends printed
  `2.0 / 1.5 / 2.5 / 2.0` where CPython prints `0.5 / 1.5 / 2.5 / -1.5`. It
  carries the ifa/121 permissive warning ("the object's method slot
  decides, which is right only if that argument has the type its version
  expects"), and the contract now proves that it does not.
- **Corpus `check` sweep** `10c5f2f6+3024dcfb` against `eb635d2a+23d143af`
  (HEAD's tree): identical in every column except **`softrender`**, which
  went from compiles-and-matches to refused. The `-b` compile sweep
  `10c5f2f6+3024dcfb` shows the same single change.

**`softrender` is the shape above, and it was not correct.** Its
`Vertex.lerp` calls `self.texCoords.lerp(other.texCoords, ..)`, and
`texCoords` holds both `Vector4(tu, tv, 0, 0)` (int `z`, `w`) and the
float-`z` vectors `lerp` builds. `Vector4.sub` (the clone with an int-field
`self`) read `r.z` and `r.w` through the float layout. It printed
CPython's answer only because those ints are all `0`, and int64 0 read as a
double is `0.0`. So the stop condition's exception covers it: it does not
PRINT a reinterpreted value, but it executes one. The refusal is
correct under the contract and costs a corpus program until step 2.

Two narrowings were measured on the way and are needed, not optional:

- receiver-only narrowing still refused `softrender` at `add`, which a
  direct call reaches (the call-site operand is the narrower answer);
- and at `sub`'s second formal, whose operand is lerp's own formal, the
  fixpoint was needed.

The result above is after both.

## Step 2, landed 2026-11-07: the read tests the tag

The union at the read is a demand: a use that cannot proceed through one
layout. The two mechanisms the step-1 record named were a split of the
contour that holds the union, and a class-tag test. The first is not
available here: the union is real at run time. `P.t` and softrender's
`Vertex.texCoords` hold an int-field object and a float-field object in
ONE list, so a single read meets both. Each object, though, has exactly one
layout. The tag test was blocked only because every clone of a class
shared one tag (`cg_tag_name`, closed/130 A3). So the fix is in
representation, and it is placed by the demand:

- **`cg_plan_concrete_tags`** (`codegen_common.cc`, run by both backends
  before any tag is named) walks every live field READ whose receiver is a
  union, and keeps the members that have the field. When two of them
  disagree (`cg_layout_mismatch` up to the slot, or float vs non-float at
  it) AND share a family, one tag cannot tell them apart. That family's
  concrete types then each get their own tag,
  `_CG_type_<name>_<family>_<type id>`. Members of different families
  already have distinct tags, and a family with no such read keeps its one
  tag, so nothing else in the emitted code changes.
- **The read** (`cg_union_read_arms`; `P_prim_period` in `cg.cc` and in
  `cg_emit_llvm.cc`) tests the tag once per arm. Each arm reads through the
  object's own layout and converts to the destination's type. Two members
  that share a tag are compatible (or their family would have been split),
  so one arm serves both, and that cast is still recorded as a layout
  obligation.
- **Stamping.** Instances are copies of one prototype per class, so the
  prototype's stamp gave every clone the same tag. A type with its own tag
  is re-stamped after `P_prim_clone` and `P_prim_copy`.
- **"Is this class"** stays a class question. `isinstance` and every
  dispatch arm test all of a split family's tags
  (`cg_tag_family_members`), so `isinstance(v, V)` and slot dispatch
  behave as before.

This is CPython's behaviour, not an accommodation: each object is read as
what it is. The int-to-float widening of the RESULT (`r.v` printing `3.0`)
is the separate, already-flagged permissive coercion (issues/171).
Setters through such a union are not tag-dispatched. Storing a widened
float into an int field would change the value, so the contract still
refuses that case, and no program in the suite or corpus reaches it.

**Results.**

- **Both fixtures now print CPython's answer** on both backends, and their
  `.check_fail` sidecars are gone. `union_receiver_field_float_vs_int` reads
  `r.v + 0.5` over both `R`s, so the int path runs, and the widening stays
  out of what is printed.
- **`softrender`** compiles again with no layout refusal (6 `Vector4` tags,
  25 tagged reads). Its corpus run prints nothing, because it renders into a
  0×0 target, so the corpus stdout comparison is vacuous for it. A scratch
  driver (monkey0.obj, a 4×4 texture, 96×72, three distances including
  near-plane clipping, 9 frames) prints the same per-frame component sum
  and hash as CPython on the C backend. That driver cannot catch the old
  defect, since its integer `z`, `w` are 0 there too; the fixtures do.
  Under `-b` the driver renders almost nothing, and so does `10c5f2f6`
  (HEAD before this issue's changes): a separate, older LLVM wrong answer,
  filed as [200](../200-llvm-backend-parity.md) A6.
- **Corpus `check` sweep** `10c5f2f6+d9550405` is identical to
  `eb635d2a+23d143af` (HEAD's tree, before step 1) in every column of every
  program, contour counts included. The `-b` compile sweep
  `10c5f2f6+fcd91a5f` is identical to `eb635d2a+4653f5e1`. So step 1's one
  corpus cost is gone, and step 2 changed nothing else.

## Verification

Step 1: the repro is refused with a layout message on both backends, and
`method_slot_registered_on_concrete_type` passes unchanged. Step 2: both
fixtures run and match CPython on both backends, through `test_pyc.py`'s
CPython stage. `make test` is green (491/0 on both backends, every IR
phase 0 failed). Both corpus sweeps are identical to the pre-206 tree,
`softrender` included.
