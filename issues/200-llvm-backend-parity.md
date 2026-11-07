# 200 — LLVM backend parity: the safety nets only the C backend has

**Status: open; A1 and A5 done 2026-11-06.** Filed 2026-11-03 from
[closed/194](closed/194-review-codegen-2026-11.md) A1, A3, A5, D2, D3 and
[closed/093](closed/093-CGEN-int-float-union-move-not-coerced.md). Both
backends pass the full e2e suite; the gaps are in what each refuses and
checks, not in what they print.

## A1. The LLVM backend has no layout contract (done 2026-11-06)

`cg_note_blind_cast`, its dedup set, `cg_layout_mismatch`,
`cg_member_ctype`, `cg_ctype_width` and `cg_check_layout_contract` moved
from `cg.cc` to `codegen_common.cc`. `cg_emit_llvm.cc` records the same
obligation at its field read (`emit_send_getter`), field store
(`emit_send_setter`) and class-tag dispatch arm (the slot GEP, receiver
from `class_recv_ridx`), and calls the check at the end of `cg_emit_llvm`.
The width table is still C-string keyed ([201](201-representation-oracle.md)).
`make test` is green on both backends, and the LLVM check refuses nothing
the C check accepts in the suite.

The contract itself has a hole that both backends now share: it compares
widths only, so an `int64` read through a `float64` slot passes. That is
[206](206-a-field-read-through-a-union-receiver-reinterprets-int-as-float.md).

## A5. An int MOVE into a float64 slot stores raw bits (done 2026-11-06)

`put_result`'s alloca and global stores, and `emit_move`'s global store,
converted int↔int and ptr↔int but not int↔float, so a `{int, float}` MOVE
stored the i64's bits into the double slot (`5` printed as `2.5e-323`).
One `slot_store_cast` now does every scalar conversion C's assignment does
(`sitofp`, `uitofp` for `i1`, `fptosi`, `fpcast`, ptr↔int), and
`coerce_slot_value` uses it for int↔float instead of a `BitCast`.
Fixture: `tests/int_into_float_slot.py` (a local and a global, with a
runtime-varying condition, printed through `==` and `+ 0.5` so the output
is CPython's). It fails under `-b` without the fix.

A field read through a union receiver of an int and a float contour is a
different bug and is on both backends:
[206](206-a-field-read-through-a-union-receiver-reinterprets-int-as-float.md).

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

**Sweep, 2026-11-06** (A5, A1, 204 rows 1–2, 039 item 1, 197 item 1 together,
against `eb635d2a`): `check` mode identical per program in every column,
contour counts included (`sweeps/check__default__eb635d2a.tsv` vs
`check__default__eb635d2a+23d143af.tsv`); `-b` compile mode identical,
with no program refused by the LLVM layout check
(`compile__PYC_LLVM_1__eb635d2a.tsv` vs `…+4653f5e1.tsv`).

## Verification

Both backends: `make test` green, corpus compile sets identical, and the
`check` sweep's stdout verdicts identical per program.
