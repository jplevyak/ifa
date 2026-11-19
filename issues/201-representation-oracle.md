# 201 — representation has three owners; give it one oracle

**Status: open, design.** Filed 2026-11-03 from
[closed/194](closed/194-review-codegen-2026-11.md) A4, C1, C2, C3,
[closed/124](closed/124-FA-refuse-imprecise-inference.md) residual 3,
[closed/054](closed/054-CGEN-remove-unconditional-tuple-list-header.md)
and [102](closed/102-codegen-detected-conditions-are-refusals-or-demands.md)'s
inventory. AGENTS.md names representation as the legitimate third
category of contour identity and says it belongs behind `IFACallbacks`.

## Where representation is decided today

| question | decided in |
| --- | --- |
| class layout, prefix layout, clone equivalence, `{bool, None}` refusal | clone (`determine_layouts`, `PYC_PREFIX_LAYOUT`, `PYC_CLASSEQ`) |
| what is tagged | codegen (`cg_has_classtag`) |
| which slots exist | codegen (`cg_field_live`, `cg_elide_unread_slots`, C backend only) |
| which blind casts are sound | codegen (the layout contract, C backend only) |
| what has a C name, and its width | codegen (`assign_type_cg_strings`, `cg_ctype_width`: a 23-entry `strcmp` table plus `strchr(t, '*')`) |
| whether candidates can be told apart at run time | codegen (the dispatch plans) |
| whether a union is representable | FA, four ways (`atype_irrepresentable`, `mixed_basics`, `scalar_with_pointer`, `nil_member_is_representable`) |
| arity | FA, in CreationSet identity (closed/132) |

The string-keyed rows are how closed/124 residual 1 happened (a list
clone with no C name read as `_CG_any` and `xs.append(3)` was refused),
and the LLVM backend runs `cg_scan_imprecise` over strings it never
emits. closed/102's inventory found four of the dispatch plan's inputs are
post-FA and stopped; its core is not.

## The oracle

One function per question, in `codegen_common`, keyed on the Sym or the
AType and nothing else:

- width and pointer-ness;
- tagged or untagged;
- nil-able as a null (`{None, T*}`) or not (`{None, int}`), and the
  sentinel encodings [118](118-union-field-representation-and-polymorphic-field-offset.md)
  asks for (`{None, bool}`);
- fixed arity or header-bearing (closed/054's flag);
- discriminability of a candidate set over the representation routes
  only: tag, null test, arity header, layout bits.

Consumers: the layout contract reads widths from it (and the LLVM backend
can then run the contract, [200](200-llvm-backend-parity.md) A1);
`cg_scan_imprecise` asks it instead of `_CG_any`; the coercion emitter's
`(uintptr_t)` rule and `simple_move`'s nil tests ask it; FA's four
predicates ask it, which is the demand channel closed/102 looked for.

## Slot elision moves to liveness and clone

`cg_elide_unread_slots` walks the live call graph after clone laid the
class out, nulls member types in place, on one backend. Computed in
`mark_live_code` (which walks those PNodes already) and consumed by
`determine_layouts`, the member would not exist, both backends would
agree, and the contract would never see a `<placeholder>`. closed/123's
93–98 % holds either way. ([203](203-shrink-the-graph-fa-walks.md) C5 is
the same direction from the analysis side.)

## The emission contract becomes true

`ifa/codegen/IF1_EMISSION_CONTRACT.md` says codegen never does dataflow;
it reads AVars and EntrySets in 20 places (slot specificity in
`cg_build_new_to_val_map`, `cg_scan_imprecise`, `cg_report_slot_use`).
Each is a representation question answered from analysis state because
nothing flattened it. They move into the oracle.

## Stop condition

From closed/102: if no suite or corpus refusal changes outcome, this is a
cleanup, and still worth landing for the LLVM layout contract alone. If
one does, it is the demand channel, and the program it changes is the
test.

## Verification

Both backends emit the same member set per class; the layout contract
reports zero placeholders; corpus `check` identical per program; `make
test` green on both backends.
