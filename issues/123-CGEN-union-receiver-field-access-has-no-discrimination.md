# 123 — a method whose receiver is a UNION of unrelated classes gets a `void*` receiver and blind-casts to ONE member's layout; field access has no runtime discrimination

**Status:** open, **root-caused 2026-09-01** with the crash line pinned.
Found by [122](122-CGEN-layout-families.md) Phase 0's layout-contract
check, which reported it at 20 sites in `go` and 1 in `bh` **at compile
time** — both programs previously only crashed.

**Affects:** `ifa/codegen/cg.cc` — the getter (~531) and setter (~578)
via `resolve_union_receiver` (~221); the union's C representation
(`_CG_any`). Contrast `poly_dispatch_classtag_targets`
(`codegen_common.cc`), which solves the same problem for method
DISPATCH and has no counterpart for FIELD ACCESS.

## Symptom

`shedskin_examples/go` segfaults (`run_rc=139`); `shedskin_examples/bh`
dies inside the collector (`GC_clear_fl_marks`), the signature of a
wrong-offset write. Both are
[102](102-corpus-programs-compile-then-abort-at-runtime.md) members.

## Root cause, with the crash line

`Square` and `UCTNode` are **unrelated** classes in `go.py` — no
inheritance. They have different numbers of method slots (17 vs 20), so
**no field index agrees between them**:

| slot | `Square` (31 members) | `UCTNode` (29 members) |
|---|---|---|
| `e24` | `used` (`_CG_bool`, 1 byte) | `pos` (`_CG_int64`, 8 bytes) |
| `e26` | `losses` (`_CG_void`) | `unexplored` (`_CG_any`, a list) |

`UCTNode::select` is emitted with a **generic receiver**, and casts it to
one class's layout:

```c
_CG_int64 _CG_f_12664_136/*UCTNode::select*/(_CG_any a1, _CG_ps17220 a2) {
  ...
  t56 = (_CG_any)((_CG_ps17236)t18)->e26;   /* unexplored */
  t54 = ...(_CG_list_ptr(t56))[...];        /* <-- SIGSEGV */
```

`a1` is `_CG_any` — a `void *`. The body blind-casts to `_CG_ps17236`
(UCTNode) and reads `e26` expecting a list. When the object is a
`Square`, `e26` is `losses`, an integer, and `_CG_list_ptr` on it
faults. gdb confirms the crash at exactly that line.

**The union has no runtime discrimination.** FA typed the receiver as
`{Square, UCTNode}`; the C representation of that union is a bare
`void *`, and the field access picks one member's layout statically.
Method *dispatch* on a union receiver already solves this — 
`poly_dispatch_classtag_targets` emits a classtag switch and uses each
concrete class's own slot index. **Field access has no equivalent.**

Note also that each class's `has` list has been polluted with the
other's field names (`Square` carries `losses`/`wins`/`bestchild`,
`UCTNode` carries `color`/`used`), which is what lets
`resolve_union_receiver` find the field on the "wrong" component in the
first place.

**Ruled out: this is NOT clone's record-type blindness.**
[121](121-CGEN-dead-clones-emitted.md) root-caused `equivalent_es_vars`
as unable to tell one record type from another, which merges contours of
different classes. That is a real hole, and it is not this one:
`IFA_DBG_VAREQ` reports **zero** `Square vs UCTNode` pairs on `go`. The
two classes are not merged by clone; the union is FA's own typing of a
variable that genuinely holds either.

## Fix directions

1. **Classtag-discriminated field access.** Mirror
   `poly_dispatch_classtag_targets` for the getter/setter: when the
   receiver is a union with more than one member carrying the field,
   emit a tag switch and use each class's own slot. Correct, and the
   most direct; costs a branch per polymorphic field access.
2. **Refuse instead of miscompiling.** 122 Phase 0 already reports the
   violation; making it an error under the default (permissive) mode
   turns two silent crashes into two compile errors. Cheap, honest, and
   a regression in "programs that build" — `go` and `bh` currently do.
3. **Give unions a representation.** [118](118-union-field-representation-and-polymorphic-field-offset.md)
   is the same family; a union with a tag would make both dispatch and
   field access uniform.
4. **Stop FA producing the union.** The receiver is only a union because
   some call site is shared; more precision there
   ([074](074-FA-cross-pass-oscillation-plan.md)/
   [030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md)) removes the
   need — but cannot be relied on in general.

Options 1 and 2 are not exclusive: 2 is a day's work and stops the
miscompile now; 1 is the real fix.

## Verification plan

1. `go` must run to completion, and `bh` must stop dying in the
   collector. Neither has a CPython stdout oracle in `check` mode
   (`go` cpy_rc=0 — it does, actually; `bh` times out), so `go`'s output
   should be compared against CPython.
2. 122 Phase 0's check must report **zero** violations on both.
3. Full gate on both backends plus a `check` sweep — this touches the
   getter/setter, which every program uses.

## What this unblocks

Two of [102](102-corpus-programs-compile-then-abort-at-runtime.md)'s
compiles-then-crashes programs, with a named cause rather than a
bisection. More broadly it is the first crash in that bucket traced end
to end from a compile-time diagnostic to the faulting line — which is
the case for keeping 122 Phase 0's check and making it louder.
