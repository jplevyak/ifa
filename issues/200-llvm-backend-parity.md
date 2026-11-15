# 200 — LLVM backend parity: the safety nets only the C backend has

**Status: open; A1 and A5 done 2026-11-06, A6 2026-11-07, A3, D3 and D2 2026-11-14.** Left: the linkage measurement (internalization itself landed with the
in-process object, 2026-11-14). Filed 2026-11-03 from
[closed/194](closed/194-review-codegen-2026-11.md) A1, A3, A5, D2, D3 and
[closed/093](closed/093-CGEN-int-float-union-move-not-coerced.md). Both
backends pass the full e2e suite. The gaps are in what each refuses and
checks. A6 was a gap in output too, which the suite and the compile-only
`-b` sweep could not see.

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
[206](closed/206-a-field-read-through-a-union-receiver-reinterprets-int-as-float.md).

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
[206](closed/206-a-field-read-through-a-union-receiver-reinterprets-int-as-float.md).

## A6. `softrender` rendered nothing under `-b` (done 2026-11-07)

A silent wrong answer on the LLVM backend, older than 206 (measured on
`10c5f2f6`). `softrender`'s own `__main__` renders into a 0×0 target and
prints nothing, and the `-b` corpus sweeps had been compile-only, so
neither the sweep nor the suite saw it. The repro was a driver:
`softrender.py` with its `__main__` replaced by one that renders
`monkey0.obj` into `RenderContext(96, 72)` with a 4×4 `bytearray` texture,
three distances, three frames each, printing `sum(components)` and a
hash. CPython and the C backend agreed; under `-b` every frame printed
`108`.

There were three defects, all in `emit_send_index_load` /
`emit_send_index_store`, and each now has a fixture:

- **An int stored into a float list element kept its bits.** The list
  store GEP'd and stored at the VALUE's LLVM type, so `m[3][2] = 1` wrote
  i64 1 into a double slot, which reads back as 5e-324. Every
  `Matrix4.init_*` writes int constants, so the MVP matrix was all zeros
  and every vertex was `nan`. The fix: store and load at the list's
  element type (`t->element->type`, as cg.cc does), converting by value
  with A5's `slot_store_cast`. Fixture: `tests/int_into_float_list_element.py`.
- **A `@vector` class's elements overlaid its own fields.** C declares the
  elements as a trailing `v[0]` member, and `_CG_prim_clone_vector`
  allocates `sizeof(struct) + n`. The LLVM index paths GEP'd from the
  object's first byte, so a bytearray's `length` showed up as its bytes
  88–90, and that `108` is 27648 = 0x6C00's high byte. A 27648-byte
  framebuffer also overran its allocation, which segfaulted once the next
  defect was fixed. The fix: `vector_data_ptr`, at the struct's alloc size.
- **A uint8 element was sign-extended.** `bytearray.__getitem__` is
  `coerce(int, index_object(..))`, and `emit_send_coerce` widened every
  integer except i1 with SExt, so byte 188 read as -68. LLVM integer types
  carry no sign, so the fix reads it from the source Sym (`num_kind ==
  IF1_NUM_KIND_UINT`: ZExt, and UIToFP to a float).
  Fixture: `tests/bytearray_elements_after_fields.py` (this and the
  previous defect).

After all three, the driver prints CPython's output on both backends.

**Sweep.** `check__PYC_LLVM_1__966472ad+e55772bf` is the first `check`
sweep under `-b`. Its compile columns are identical to the `-b` compile
sweep `compile__PYC_LLVM_1__10c5f2f6+fcd91a5f`. Against the C backend's
`check__default__10c5f2f6+d9550405`, the output verdicts agree on every
program both could verify. Four programs are listed as `-b` run failures:
`bh`, `chaos`, `path_tracing`, `rubik2`. All four are rc=124 at the 120 s
cap, programs CPython also times out on. Run alone they finish: 69, 110,
107 and 104 s. HEAD's `-b` takes 76 s for `bh` and 114 s for `chaos`.
That is contention near the cap, not a regression. `doom` is refused
under `-b`, as before.

## A3. Decisions implemented twice (done 2026-11-14)

Each is now one function in `codegen_common`, called by both backends:

| decision | was | now |
| --- | --- | --- |
| a union receiver's member for a field access | `resolve_union_receiver` in each | `resolve_union_receiver` |
| a constant IF's one successor | `cg.cc` inline, `cg_emit_llvm.cc` `const_if_successor` | `cg_const_if_successor` |
| an unresolved IF condition | `is_unresolved_condition` in each ("deliberately kept independent") | `cg_is_unresolved_condition` |
| whether an unresolved IF traps | C: the IF and a successor live; LLVM: a successor has a block | `cg_if_traps_unresolved` (C's gate) |
| which found class gets a tag arm | `cg_has_classtag && cg_get_string` vs `name && !is_system_type && cg_has_classtag` | `poly_dispatch_tag_arm` |
| is a candidate a closure carrier | the same loop in each | `poly_dispatch_closure_carrier` |

**The two divergences closed/030 recorded:**

- **The class-tag filter was not a divergence.** `cg_has_classtag` already
  requires a name and a non-system type, and both backends name every
  type through `assign_type_cg_strings_pass{1,2}`. The two tests agreed;
  they are now one. No fixture can tell them apart.
- **Unnamed candidates were a silent wrong answer, not a bail.** LLVM
  rejected a candidate with no `sym->name` (every lambda) and fell to its
  bare-callable pass. That pass branched to the merge when no address
  matched, so the call read the result slot's zero: a capturing lambda
  dispatched against a plain function or a nested def printed `0` for
  `13`, and `0` for `50`. C never required a name. Fixed by accepting the
  candidate, and the pass now traps where C asserts: on no match
  (`poly_no_branch_matched`) and on a candidate whose formals do not fit
  the operands (it used to skip the call). Fixture:
  `tests/dispatch_lambda_with_function.py`; `-b` printed `0 11 0 0`
  before. A capturing lambda mixed with a `__call__` instance printed `0`
  under `-b`; both backends now refuse it with ifa/102's coverage
  diagnostic.

**The dead unresolved-ifs (from closed/102) are gone.** With C's gate,
LLVM's `unresolved_if_condition` traps over the suite went 21 → 0, C's
count (`IFA_DBG_TRAPS=1`, every suite program on both backends). Those IFs
now end in LLVM's dead-text `unreachable`; the one read
(`bytes_join`'s `__ne__`) is the false arm of a `br i1 true`. Every suite
program's exit status is unchanged on both backends, and C's trap census
is unchanged apart from line numbers.

**Found on the way, filed:** a list display of function constants
(`[lambda x: .., lambda x: ..]`) loses its elements on both backends:
[210](210-a-list-display-of-function-constants-loses-its-elements.md).

**Still by name:** `poly_dispatch_closure_carrier` recognises a carrier by
its class name `__closure__`, the only mark the frontend gives it. It is
now in one place; a structural mark (set by the frontend, read through
`IFACallbacks`) would retire the string.

**Sweep, 2026-11-14.** `check__default__328f0b12+ccf15f37` is identical
per program in every column to `check__default__328f0b12+d7246212`, the
same tree before A3. `check__PYC_LLVM_1__328f0b12+ccf15f37` (the first
`-b` check sweep since A6) has the C sweep's verdict on every program it
builds: `othello2` differs from CPython on both backends, as before;
`doom` is refused under `-b`, as before. `make test` is green.

## D3. A unit fixture and a determinism oracle for `-b` (done 2026-11-14)

**The `codegen-llvm` phase is back.** `ifa-test --phase codegen-llvm` runs
the `codegen-c` pipeline into `llvm_codegen_print_ir` (the `cg_emit_llvm`
emitter). The six `codegen-c` programs are its fixtures:
`tests/ir/codegen-llvm/` holds symlinks, so the two phases cannot drift.
The normaliser drops the module ID, `source_filename`, the target lines,
every `!N` metadata line and `!dbg` attachment, the `#dbg_` records, and
intrinsic declarations with their `attributes`. That last group is
LLVM's own text and changes between versions: 20 prints `nocapture`
where 22 prints `captures(none)`. The goldens pass unchanged on an
`ifa-test` built against LLVM 20 (CI's) and against 22.

**The fixtures had been testing nothing.** The synthesized `__main__`
does not read top's return, so DCE deleted every body. All six
`codegen-c` goldens were the same empty `__main__`, 341 bytes apiece. The
codegen phases now register `observe`, a visible primitive, which is never
dead (`pre_parse_codegen`), and each fixture ends by observing its
result. The goldens now hold a field GEP, store and load (`04_getter`),
`GC_malloc` with the class-tag store (`06_sum_type`), and a clone's
`memcpy` (`05_clone`). `02_call`'s `%add` never replied, so its result
was untyped (`fa_rc=-1` as soon as anything read it); it replies now.
The 16 synthetic fixtures' `codegen-c` goldens are still empty `main`s;
they did not get `codegen-llvm` twins.

**Determinism.** Every suite program (554; 516 emit `.ll`) compiled with `-b`
twice, from the same work directory: every `.ll` byte-identical and every
exit status the same. Then once more with a second, clean build
(`make clean`, after D2 below): byte-identical to the first run again.
The emitter's pointer-keyed containers are not reaching output order.
The stop condition (two runs, two builds) is met.

**Found on the way:** `parse_ir` retried a top-level declaration that
errored without consuming a token, forever. A stray `=>` wrote 5 GB of
`expected '(' to open declaration` before the shell killed `ifa-test`. It
stops at the first failed declaration now.

## D2. The emitter seam is used by both backends (done 2026-11-14)

`write_c_prim`, the 980-line switch C routed every primitive through
(`emit_send_any_prim`), is gone. Its cases moved whole, with no edits,
into one `c_prim_*` function per hook, and each primitive now goes to
the hook that owns it on LLVM:

| hook | primitives | C was |
| --- | --- | --- |
| `any_prim` | `await`, `yield`, `id` | the switch |
| `unaryop` | `cg_unaryop_prim`: `lnot`, `minus`, `not` | `default_prim` |
| `binop` | `cg_binop_family`, `pow` | `default_prim` |
| `period`, `setter`, `new`, `len` | themselves | the switch |
| `clone` | `copy`, `clone`, `clone_vector` | the switch |
| `strcat` | `strcat` | `default_prim` |
| `make` | `make`, `make_seq` | the switch |
| `index_load` / `index_store` | `index_object` / `set_index_object` | the switch |
| `sizeof` | `sizeof`, `sizeof_element` | the switch |
| `primitive` | a registered primitive, by name | the switch |

`is`, `coerce` and `default_prim` were already separate on both sides.
The operator sets were LLVM's (`is_binop_family`, an inline unary test).
They are now `cg_binop_family` and `cg_unaryop_prim` in
`codegen_common`, read by both. The table is in `codegen_common.h`,
above the hooks.

**What only one side has:** C's `any_prim` also takes `apply`, `assign`
and `destruct`, which only the V frontend emits; LLVM has no handler for
them. `land` and `lor` reach `default_prim` on both, and LLVM's default
would call an undefined `_CG_land`. pyc never emits any of the five.

**Verified:** every suite program compiled to C by the pre-D2 and
post-D2 compilers (cg.cc restored in a scratch copy of this tree):
all 516 `.c` files byte-identical, every exit status the same. The `-b` output is byte-identical too (D3's second build).
`make test` is green: `test_pyc` 517 passed, 0 failed on both backends; `ifa --test` 58/0; all 17 `test-ir` phases 0 failed.

## Also

`CODEGEN_LLVM.md` §14.9: every emitted function is `ExternalLinkage`.
The lld LTO link already internalized them, which is why this never
cost what it seemed to. Since 2026-11-14 the object is written in process
(`llvm_emit_object`, CODEGEN_LLVM.md §11), and it internalizes every
definition but `main` itself, whenever the runtime bitcode is linked in.
On sha that took the program from 88 calls into the runtime to the 51
the lld link made. Left: a measurement under
[199](199-emitted-code-performance.md)'s benchmark, once it exists.

**Sweep, 2026-11-06** (A5, A1, 204 rows 1–2, 039 item 1, 197 item 1 together,
against `eb635d2a`): `check` mode identical per program in every column,
contour counts included (`sweeps/check__default__eb635d2a.tsv` vs
`check__default__eb635d2a+23d143af.tsv`); `-b` compile mode identical,
with no program refused by the LLVM layout check
(`compile__PYC_LLVM_1__eb635d2a.tsv` vs `…+4653f5e1.tsv`).

## Verification

Both backends: `make test` green, corpus compile sets identical, and the
`check` sweep's stdout verdicts identical per program.
