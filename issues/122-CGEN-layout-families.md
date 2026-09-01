# 122 — name the LAYOUT FAMILY, so unused struct fields can be elided safely (and the blind-cast invariant becomes checkable)

**Status:** **Phase 0 LANDED 2026-09-01** (the contract is now recorded and checked; see "Phase 0 as built"). Phases 1-3 open. Written 2026-09-01 after
[121](121-CGEN-dead-clones-emitted.md)'s category 2 measured the prize
(**269 of pygmy's 275 candidate slots are provably dead**), built a
working analysis for it, and then failed at run time on
`tests/method_override_field_offset.py`.

**Affects:** `ifa/codegen/cg.cc` — the struct-definition emitter
(~2660), the getter (~531) and setter (~578) via
`resolve_union_receiver` (~221), the classtag dispatch call (~2153),
`build_type_strings`; `ifa/codegen/codegen_common.{h,cc}`.

## The obstacle, stated precisely

Field access in the emitted C is **by member name**, `->eN` at the
`has` index — so a correctly typed access does not care about offsets.
But a method shared between a base and its subclasses is emitted **once**,
takes a generic receiver, and **blind-casts to one class's layout**:

```c
t5 = (_CG_list)((_CG_ps2457)t1)->e5;      /* cast, then read by name */
```

C computes `e5`'s offset from the **cast-to** struct. So if the actual
object is a different class whose `e5` sits elsewhere, the read hits the
wrong bytes. `tests/method_override_field_offset.py`'s own header states
the invariant this depends on: such a cast is sound "only while the
subclass's layout is a prefix-compatible extension of the base's."

Today that holds **by construction and by accident**: every class emits a
member for every `has` index, densely, placeholders included, and the
same member Sym has the same type everywhere — so shared prefixes match.
**Any per-class elision is a layout change and breaks it.** Measured:
eliding per class made both subclass loops in that test print `0`
instead of `4`. Restricting elision to slots unused across
`s->specializers` and `s->specializes` did **not** fix it, so the set of
classes that share a layout through blind casts is **wider than the
class hierarchy**, and pyc has no name for it.

That is the whole blocker. The analysis half is done and exact — see
121: a used-set recorded by the emitters agreed with a sound clang
oracle to the slot, 269 of 275.

## What we need

Two things, and the first is worth landing on its own.

### Phase 0 — make the invariant EXPLICIT and CHECKED (no behaviour change)

Nothing today states the prefix-compatibility rule, let alone checks it.
It is enforced only by the accident of dense layout, which is exactly why
[110](closed/110-override-duplicates-member-slot.md) was able to happen
(an override appended a second slot and shifted every later field).

- **Record every blind cast the emitter performs**: at each site that
  emits `((<cast-to>)<expr>)->eN`, record the triple
  `(cast_to_sym, static_sym_of_expr, slot)`. The sites are few and
  enumerable: the getter and setter (whose cast-to comes from
  `resolve_union_receiver`, i.e. deliberately not the static type), the
  classtag dispatch call (`classes[ci]`), and the `cg_new_to_val_map`
  install (same type both sides — no constraint). Same discipline as
  121's `cg_note_field_use`: record what the emitters DO, never
  re-derive it.
- **Check prefix agreement, not offsets.** Absolute offsets would need a
  C-type size table; the sufficient and purely syntactic condition is:
  for a recorded `(A, B, N)`, the emitted member sequence of `A` and `B`
  must agree in `(index → c_type)` for every index `0..N`. String
  comparison, no size model.
- Report violations under `IFA_DBG_LAYOUT` first, then make it a hard
  error. **This would have caught 121's category-2 regression at compile
  time instead of at run time**, and it guards ifa/110's bug shape
  permanently.

Deliverable on its own: the first executable statement of pyc's
object-layout contract.

### Phase 0 as built (2026-09-01)

`cg_note_blind_cast` is called from the three sites that emit one — the
getter and setter (whose cast-to comes from `resolve_union_receiver`,
deliberately not the static type) and each classtag dispatch branch —
and `cg_check_layout_contract` runs at the end of `c_codegen_print_c`.

**Corpus census: 74 programs, 12348 blind-cast obligations, 46
violations in 3 programs; 71 programs completely clean.** Two
calibrations were needed and both were false-positive directions, worth
recording because the obvious formulation is wrong twice over:

- **Compare WIDTH, not the typedef's spelling.** `plane.e12` is
  `_CG_pf60` and `sphere.e12` is `_CG_pf64` -- two function-pointer
  typedefs; `_CG_float64` and a `_CG_psN` are a double and a pointer.
  All 8 bytes, so nothing after them moves. Spelling comparison reported
  **81 phantom violations on pygmy alone**. `cg_ctype_width` mirrors
  `pyc_c_runtime.h:353-384`; an unrecognised spelling falls back to
  comparing names, which over-reports rather than under-reports.
- **A union member that does not HAVE the field is not an obligation.**
  `webserver` reaches a `{str, dict}` receiver where
  `resolve_union_receiver` picks `dict`; `str` has no such field, so the
  access never happens on one (and would be a type violation reported
  elsewhere if it did). That was 23 of the 46 -- all of webserver's.

What survives is specific, and it is in the programs that crash:

```
go:  'UCTNode' blind-cast to 'Square', read at e24-e28,
     but member width differs at e24 (_CG_bool vs _CG_int64)   -- 20 sites
bh:  'Cell' blind-cast to 'Body', read at e28,
     but member absent at e27 (_CG_float64 vs <absent>)        --  1 site
```

`go` reads a **1-byte `_CG_bool` where `Square` has an 8-byte
`_CG_int64`** -- every later field shifts by 7 -- and segfaults
(`run_rc=139`). `bh` casts to a struct one member short, and aborts
(`run_rc=139`). Both are
[102](102-corpus-programs-compile-then-abort-at-runtime.md) members that
now have a named, located, compile-time cause instead of only a crash.

**Fatal in every mode as of 2026-09-01.** It first landed as a warning
under the default (permissive) mode, on the reasoning that seeing the
problem is a separate decision from failing builds that work today. That
reasoning does not survive
[123](123-CGEN-union-receiver-field-access-has-no-discrimination.md):
unlike a type violation, a layout violation has **no permissive
meaning**. `--permissive` accepts a type violation and inserts a runtime
check; there is no runtime check for reading one class's field through
another's layout, only a wrong value or a segfault with no diagnostic.
Corpus cost, measured: `compile_fail` **3 → 5** (`bh` and `go` join
`chess`, `othello3`, `sudoku5`), `run_fail` **44 → 42**, nothing else
affected. Neither program worked -- `go` segfaulted, `bh` corrupted the
heap.
Corpus `check` sweep vs the pre-Phase-0 tree is a two-line diff, both of
them warning counts (bh 1 -> 2, go 30 -> 40); `compile_rc`, `run_rc`,
`cpy_rc` and `stdout_match` are identical on all 77 programs. Gate green
on all five steps.

### Phase 1 — name the family

Union-find over the Phase 0 pairs: `A ~ B` whenever any cast relates
them. The equivalence classes are the **layout families**. Report their
sizes and how far they exceed the class hierarchy — 121's measurement
says they must, and this is what says by how much.

### Phase 2 — family-consistent elision

- Reuse 121's used-set (`cg_note_field_use` from the ten `->eN`
  emission sites), which is proven exact against the clang oracle.
- A slot index `i` is elidable iff **no class in its layout family uses
  it**, and every class in the family then elides it **identically**.
- Emit the elided member as `char eN[0];` — keeps the name and the `eN`
  numbering (which several sites compute independently, see 121
  category 1) at zero storage.
- Phase 0's check now guards the result rather than a test happening to
  notice.

This needs the struct-definition **deferral** already prototyped in 121
and measured behaviour-neutral: `build_type_strings` leaves a marker,
everything that can touch a field is emitted first, and
`c_codegen_print_c` splices the generated structs back over the marker.

### Phase 3 — measure

`pygmy` is the calibration point (269 slots, 2152 bytes). The
corpus-wide number is **unmeasured** and obtainable cheaply with the
oracle from 121: emit candidates as `struct { } eN;`, compile with
`-ferror-limit=0`, and anything clang does not reject is untouched. Then
the usual gate plus a `check` sweep — a wrongly elided READ is a SILENT
wrong answer, so the sweep's stdout comparison is load-bearing, not a
formality.

## The alternative that dissolves the problem

Phases 1-2 exist only because method pointers live **in every instance**.
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md) is about moving
polymorphic dispatch off that model. With a per-class vtable, the dead
method slots do not need eliding — **they do not exist**, `vec` stops
carrying 23 method pointers next to its three doubles, and the
blind-cast prefix problem shrinks to real data fields.

So: **Phase 0 is worth doing regardless** — it is the missing contract
and a permanent guard. Phases 1-2 are worth doing only if 030 is not
going to happen soon; if it is, they are throwaway work on a
representation that is about to change.

## Verification plan

1. Phase 0 alone: `make test` and the corpus `check` sweep must be
   unchanged (it emits nothing). Its own evidence is that it reports
   zero violations on the corpus as shipped, and that re-applying 121's
   per-class elision makes it report them.
2. Phase 2: `tests/method_override_field_offset.py` is the canary —
   it is the test that caught the naive version. Then the full gate on
   both backends, and a `check` sweep diffed against the pre-change tree.
3. Struct-size deltas per program, so the win is stated in bytes rather
   than in slot counts.

## What this unblocks

Directly: 121's category 2, the last measured source of emitted-but-dead
output. More usefully, Phase 0 gives pyc a checked statement of the
object-layout contract that three separate issues (110, 121, and this
one) have each had to rediscover by debugging a miscompile.
