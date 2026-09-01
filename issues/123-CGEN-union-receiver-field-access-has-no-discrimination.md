# 123 — a method whose receiver is a UNION of unrelated classes gets a `void*` receiver and blind-casts to ONE member's layout; field access has no runtime discrimination

**Status:** open (fix outstanding); **root-caused end to end
2026-09-01**, and the miscompile is a compile error rather than a
segfault as of the same day (fix 2). `go`'s crash is fully explained:
`node.losses += 1` on a UCTNode is emitted through Square's layout and
increments the `unexplored` POINTER instead.
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
(UCTNode) and reads `e26` expecting a list; gdb confirms the crash at
exactly that line, and that `e26` reads back **`0x7ffff7d609d2` — not
null, and not 8-aligned**. No valid GC pointer looks like that, so the
object is not shaped like `_CG_s17236`: it is a wrong-layout read.

## ROOT CAUSE, complete (2026-09-01)

`go.py:378` is `node.losses += 1`, inside `UCTNode::update_path`. It is
emitted as:

```c
t89 = (_CG_int64)((_CG_ps17224)t86)->e26;   /* losses -- SQUARE's layout */
t87 = _CG_prim_add(t89, _CG_Symbol(7441, "+"), 1);
((_CG_ps17224)t86)->e26 = (_CG_int64)t87;
```

`t86` is a **UCTNode** (`__pyc_tag` confirms it at run time). The access
uses `_CG_ps17224`, a **Square** clone, where `e26` is `losses`. On a
UCTNode `e26` is `unexplored`. So `node.losses += 1` **reads the
`unexplored` POINTER as an integer, adds 1, and writes it back.**

Proven by watching the value, not inferred:

```
store   (computer_move:9696)  e26 <- 0x7ffff7d609d0   (a valid 8-aligned list)
crash   (select:8438)         e26 == 0x7ffff7d609d2   (same object)
```

Two `losses += 1` updates, pointer +2, `_CG_list_ptr` on a misaligned
pointer, SIGSEGV. The object at the fault is the ROOT `tree` node
(`pos=-1`, `parent=NULL`, `wins=5`), and its neighbouring `pos_child`
(`e25`) is still properly aligned — only the field Square's layout aims
at is damaged.

**Why Square's layout is used at all: field-name pollution.** `Square`'s
member list contains `losses`, `wins` and `bestchild` — UCTNode's fields
— and UCTNode's contains `color` and `used`. So when the receiver is a
union, `resolve_union_receiver(union, "losses")` finds `losses` on
`Square` and picks it. Either half of the fix stops this: do not merge
the field-name sets, or discriminate the layout at run time.

**What the check does and does not name.** This corrupting write IS one
of the 20 reported violations ("`UCTNode` is blind-cast to `Square` and
read at e26"). The check therefore names the CAUSE. It does not name the
faulting line — the fault is one hop downstream in `select`, reading the
field that was corrupted. That is the right behaviour for such a check,
and worth stating because a first reading of the report looks like it
missed the crash.

### Two claims retracted along the way

Recorded rather than quietly fixed; each was a plausible inference from
the emitted C that a re-check disproved.

**(a) "The crash LINE is the violation."** Wrong: all 20 obligations are casts TO `Square` and the
faulting line casts to `UCTNode`. But the opposite over-correction —
that the check did not predict the crash — is also wrong: it predicts
the corrupting write, which is the cause.

**(b) "`unexplored` is read seven times and never written."** Wrong. It is
written — `((_CG_ps17243)t73)->e26 = (_CG_any)t71;` — and the claim was
an artifact of grepping for a `/* unexplored */` comment that the SETTER
does not emit while the getter does. (A third dead end, also
disproved: the setter's dead-field elision. A `PYC_DBG_DROPSTORE` probe
showed it never fires for `UCTNode.unexplored`.)

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
2. **Refuse instead of miscompiling. DONE 2026-09-01.** 122 Phase 0's
   check is now fatal in every mode, so these two silent crashes are two
   compile errors naming the class pair and the slot. Corpus:
   `compile_fail` 3 → 5, `run_fail` 44 → 42, nothing else affected. This
   does not fix anything — it stops pyc emitting a program that lies,
   and it is why the remaining options are worth doing.
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
