# 187 — LLVM: a constructor clone omits a method slot that a dispatch reads

**Status:** open. Found 2026-10-09 on `softrender`, which now compiles and
matches CPython on the C backend but segfaults under `-b`.

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
slots read on the receiver's CreationSets. Unlocated: whether the
prototype global in LLVM never gets the store at all (the C one does), or
gets it but is a different global from the one `__new__` copies.

## Verification

`pyc -b` on `softrender` runs to rc 0 with CPython's (empty) output, and
a reduced program (instances of one class from two constructor contours,
mixed through a dict, method called through the dict) passes on both
backends.
