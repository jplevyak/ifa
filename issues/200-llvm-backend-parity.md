# 200 — LLVM backend parity: the safety nets only the C backend has

**Status: open.** Filed 2026-11-03 from
[closed/194](closed/194-review-codegen-2026-11.md) A1, A3, A5, D2, D3 and
[closed/093](closed/093-CGEN-int-float-union-move-not-coerced.md). Both
backends pass the full e2e suite; the gaps are in what each refuses and
checks, not in what they print.

## A1. The LLVM backend has no layout contract (defect)

`cg_note_blind_cast`, its dedup set and `cg_check_layout_contract` are
static in `cg.cc`, called from the C field read, store and dispatch arms
and checked in `c_codegen_print_c`'s epilogue. `cg_emit_llvm.cc` records
no obligation and runs no check, although its `resolve_union_receiver`
performs the same blind casts. The bug class closed/122 and closed/123 made
fatal on the C side (reading one class's field through another's layout,
go's crash, bh's heap corruption) is unguarded under `-b`.

**Do:** move the three functions and their state to `codegen_common`;
record at the LLVM field read, store and dispatch arms; check at the end
of `cg_emit_llvm`. The width table they use is C-string keyed; that is
[201](201-representation-oracle.md)'s row, and this can land first with
the table as is. Stop condition: `make test` green on both backends and
the corpus `-b` compile set unchanged; a program the LLVM check refuses
and the C check accepts names a divergence, which is the finding.

## A5. An int MOVE into a float64 slot stores raw bits (closed/093)

`emit_move` coerces int↔int (`SExtOrTrunc`) and treats pointer↔pointer as
a no-op; there is no `sitofp` path, so a `{int, float}` MOVE prints
`5e-324` where the C backend prints `1.0`. **Do:** at a scalar MOVE whose
source `num_kind` is integer and destination is float, `CreateSIToFP`
(closed/062 added the same in `emit_send_binop`). Fixture with a
runtime-varying condition; LLVM prints what C prints.

## A3. Four decisions are implemented twice, one on purpose

`resolve_union_receiver` (cg.cc:232, cg_emit_llvm.cc:731),
`is_unresolved_condition` (the LLVM copy's comment says "deliberately
kept independent"), `const_if_successor`, and the class-tag eligibility
filter (`cg_has_classtag(rt) && cg_get_string(rt)` against `rt->name &&
!rt->is_system_type && cg_has_classtag(rt)`). closed/030 records the
last two as divergences with no repro; unnamed lambda candidates make
LLVM bail a whole site. **Do:** one implementation each in
`codegen_common`, and delete the comment that calls duplication a
convention. Stop condition: the two divergences gone, a fixture each.

## D3. No unit fixture and no determinism oracle for `-b`

The `codegen-llvm` IR phase was retired with v1 and not re-created;
`print_codegen.cc` still has the LLVM normaliser. closed/112 and
closed/147's byte-identical checks were done on C output; the emitter
holds eight `std::unordered_map` / `std::set` containers keyed on
pointers. **Do:** the six `codegen-c` programs through a `codegen-llvm`
phase; two runs of `-b` on the suite diffed. Stop condition:
byte-identical `.ll` across two runs and two builds.

## D2. The emitter seam is half used

`VirtualCGEmitter` has twenty hooks; the C backend routes everything
through `emit_send_any_prim` → `write_c_prim`'s 958-line switch, so the
per-operation hooks are dead on that side and the two backends cannot be
compared hook by hook. **Do:** split `write_c_prim` along the hook
boundaries (the cases are grouped by primitive already), or state in the
header that the seam is LLVM's structure only. After A3 and A1, this is
what keeps them from drifting again.

## Also

`CODEGEN_LLVM.md` §14.9: every emitted function is `ExternalLinkage`,
which blocks whole-module DCE. With the runtime now LTO-linked, measure
`InternalLinkage` for non-`main` functions under
[199](199-emitted-code-performance.md)'s benchmark.

## Verification

Both backends: `make test` green, corpus compile sets identical, and the
`check` sweep's stdout verdicts identical per program.
