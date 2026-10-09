# 187 — LLVM: a constructor clone omits a method slot that a dispatch reads

**Status:** CLOSED 2026-10-09. Found and fixed the same day on
`softrender`, which compiled and matched CPython on the C backend but
segfaulted under `-b`.

## Symptom

`pyc -b softrender.py` builds; the binary jumps to address 0 inside
`Mesh.__init__`. The faulting call is a classtag dispatch on `Vector4`
through method slot 15 (`add`; slot `e14` in the C backend, after the
classtag header):

```python
if key in pos_normal:
    havenormal = pos_normal[key]          # a Vector4 made by an earlier .add()
else:
    havenormal = Vector4(0, 0, 0, 0)
pos_normal[key] = havenormal.add(normal)  # C: if tag == Vector4: ->e14(...)
```

## Mechanism

The two backends populate method slots differently:

- **C:** the class's prototype initializer stores the slot once into the
  prototype global (`g37->e14 = Vector4::add`), and every instance copies
  the prototype, so every `Vector4` carries `add`.
- **LLVM:** each `Vector4.__new__` clone memcpy's the prototype and then
  stores the slots that clone's instances are expected to need. Some clones
  store `add` (`_CG_f_22961_364`, `_372`); others store nothing
  (`_CG_f_22961_138`). An instance from one of those reaches the dispatch
  above through the dict, with slot 15 NULL.

So the LLVM backend's per-constructor slot set is narrower than the set of
slots read on the receiver's CreationSets.

## Root cause

`cg_build_new_to_val_map` (`codegen_common.cc`) registers a method against
each CreationSet that reaches the method's `self`, at that CreationSet's
slot for it, and every constructor of the CreationSet then stores the
method into new instances. It looked the slot up in `cs->sym`. After
`clone()` the struct a constructor actually fills is the concrete type,
`cs->type`. `cs->sym` can be the unspecialized class, whose method members
are never emitted and so are never live. Measured on softrender: five
Vector4 CreationSets had `cs->sym` = Vector4 (19601, `add` not live) and
`cs->type` = the clone `_CG_ps27851` or `_CG_ps27933` (`add` live at e14).
They registered nothing, so their constructors stored no `add`.

The C backend hid it: it also stores each method into the class prototype
(the class body's `self.add = ...`), and `clone` copies the prototype. The
LLVM backend deliberately skips those prototype stores and installs slots
only from this map (`emit_send_unhandled`, `cg_emit_llvm.cc`).

## Fix

Resolve the slot in `cs->type ? cs->type : cs->sym`. The ancestry filter
just above stays on `cs->sym`, because it asks about the class, not its
layout. Test: `tests/method_slot_registered_on_concrete_type.py`, which
segfaulted under `-b` without the fix (the int/float mix in its
constructor calls is what makes the constructor clones differ; an
all-float version does not reproduce).

## Verification

`pyc -b` on `softrender` runs to rc 0 with CPython's (empty) output; the
C backend still does too. `make test` green (431/0 both backends). Check
sweep `2adb3817+f853f7b5` (C backend): identical to `282fd45f+2b457a52`
in every verdict, warning count, ESS and CSS over 77 programs.
