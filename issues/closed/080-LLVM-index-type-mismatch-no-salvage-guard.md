# 080 — LLVM backend's index/element type mismatch has no salvage guard (`emit_send_index_load`/`emit_send_index_store`), unlike the C backend since issue 056

**Status: CLOSED** — fixed 2026-08-07.

### Resolution Summary
Added `emit_salvage_trap` helper in `ifa/codegen/cg_emit_llvm.cc` and invoked it from `emit_send_index_load`, `emit_send_index_store`, and `emit_send_binop`. When flow analysis degrades an index Var or binary op operand to a non-scalar/mismatched LLVM type, the LLVM backend now emits a `@llvm.trap()` call and binds a typed null constant to the destination Var instead of calling `codegen_fail(...)` or triggering an LLVM IRBuilder assertion failure during compilation.

Confirmed `tests/list_index_type_mismatch_salvage.py` now compiles and executes cleanly with `pyc -b`, removed its `.expect_fail` sidecar, and verified that both backends pass the full test suite with 0 regressions.

**Original filing follows.**

**Status:** open, found 2026-08-07 while independently verifying commit
`fa683610` ("issue 056: add index type mismatch salvage guard to
P_prim_index_object and P_prim_set_index_object"). That commit fixed
the C backend only; the LLVM backend has the same class of gap,
confirmed unaddressed.

**Affects:** `ifa/codegen/cg_emit_llvm.cc`'s `emit_send_index_load`
(~line 1315) and `emit_send_index_store` (~line 1395) — the LLVM
backend's independent emission for list/string/tuple-list indexing.
Neither function has anything resembling `cg.cc`'s new
`scalar_ct(c_type(...))` check.

**Related:** [056](closed/056-CGEN-degraded-index-type-raw-c-compile-error.md)
— the C-backend fix this issue is the LLVM-parity follow-up to. That
issue's own "What a fix would look like" section explicitly named
this as required scope ("should be checked against the LLVM
backend's independent emission for parity... the two backends drift
on exactly this kind of guard if not deliberately kept in sync") —
not done when 056 landed, and its closure doesn't mention the gap.
`tests/list_index_type_mismatch_salvage.py` (the regression test
added for 056, a real vendored program — Apache-2.0, Google Inc.
2011, `shedskin_examples/loop/loop.py`) is marked with an
`.expect_fail` sidecar specifically because of this issue; that
sidecar should be removed once this is fixed.

## Symptom

Where the C backend now degrades a salvage-degraded (non-scalar)
index Var to a runtime `assert(!"runtime error: list index type
mismatch");`, the LLVM backend hits a hard **compile-time** failure
instead — the compiler itself aborts, producing no `.ll`/`.o`/binary
at all:

```
/home/jplevyak/projects/pyc/__pyc__.py:707: codegen: primitive operand type mismatch (prim index 15)
```

Confirmed via `tests/list_index_type_mismatch_salvage.py` (the same
file that exercises 056's fix cleanly on the C backend):

```
$ pyc -b -D . list_index_type_mismatch_salvage.py
... (same two pre-existing, unrelated Basic_block warnings as the C backend) ...
/path/to/__pyc__.py:707: codegen: primitive operand type mismatch (prim index 15)
$ echo $?
1
```

No `.ll` file is produced (an empty one is left behind), no `.o`,
no binary — a total compile failure for a program that the C backend
already handles gracefully (compiles clean, runs, with the
guard emitted around the two salvage-degraded index sites internally
even though they're never reached at runtime by this particular
program's own execution).

## Root cause (not yet traced in the LLVM emitter itself)

Not root-caused to the same level of detail as 056 was for the C
backend — the `codegen: primitive operand type mismatch (prim index
15)` message comes from a different, more generic internal check
(likely in shared codegen/prim-lowering plumbing, not
`emit_send_index_load`/`emit_send_index_store` specifically; "prim
index 15" needs mapping to whichever `P_prim_*` it refers to). What
*is* confirmed: the underlying trigger is the same FA-level condition
056 already diagnosed (an index or stored-element Var salvaged to a
non-scalar `_CG_any`/void-equivalent LLVM type reaching an emission
site that assumes a concrete, castable scalar type) — the two
backends just fail differently once that condition is reached: C
backend emits invalid text that `clang` later rejects (056's original
symptom); LLVM backend's own internal type-checking during IR
construction rejects the mismatch immediately and aborts the whole
compile.

## Proposed fix

Mirror 056's fix shape in `emit_send_index_load`/
`emit_send_index_store`: before emitting the LLVM IR that indexes
into the list/string/tuple-list (or stores an element), check
whether the index Var's (and, for store, the value Var's) resolved
LLVM type is a scalar (int/float/bool — the LLVM-side equivalent of
`cg.cc`'s new `scalar_ct` helper; `cg_emit_llvm.cc` will need its own
version since LLVM types aren't C type-name strings, e.g. check
`llvm::Type::isIntegerTy()`/`isFloatingPointTy()` or similar rather
than string-prefix matching). If not, emit a trap instead of the
indexing/store IR — this codebase's existing LLVM-side convention for
a salvage-guard trap should be reused rather than invented fresh (grep
`cg_emit_llvm.cc` for how other salvage sites there emit a runtime
abort, e.g. via a trap intrinsic or a call to the same assert-style
runtime helper the C backend uses) — rather than let LLVM's own
type-checked IR builder calls reject the mismatch at compile time.

Needs the "prim index 15" message's actual origin traced first (not
done here) to confirm the fix site is genuinely
`emit_send_index_load`/`emit_send_index_store` and not one level
removed (e.g. a shared helper both call into, or a check that fires
before either function is even reached).

## Verification plan

- `tests/list_index_type_mismatch_salvage.py` compiles clean on `-b`
  (matching the C backend's already-clean compile) and its
  `.expect_fail` sidecar is removed.
- Full `test_pyc.py` both backends, `ifa --test`, clean before/after
  `shedskin_sweep.sh` is not meaningful for LLVM (it's hardcoded to
  the C backend — confirmed while investigating 056) — use an
  LLVM-flavored sweep variant (`-b` flag, check for `.py.ll` instead
  of `.py.c`) for a real corpus-level regression check, matching what
  was used to verify 056's LLVM gap didn't already silently exist
  elsewhere in the corpus.

## What this unblocks

LLVM/C backend parity for this whole class of salvage-degraded
index/element sites — right now a program that the C backend
compiles successfully (degrading gracefully at runtime if the
salvage-guarded code path is ever actually reached) simply cannot be
compiled with `-b` at all. Also closes a real gap in 056's own
closure, which claimed the fix without disclosing this asymmetry.
