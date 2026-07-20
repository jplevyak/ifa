# 058 — Polymorphic classtag method dispatch silently dropped every argument beyond `self`

**Status:** FIXED 2026-07-19. Found while digging into the shedskin
corpus's raw-crash bucket (5 examples with no runtime-error assertion
at all: `block`, `kanoodle`, `mastermind2`, `neural1`, `rsync` — see
[../../issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)'s
2026-07-19 triage entry for the other four's independent, unrelated
root causes). `kanoodle.py` was the one with a genuine, previously-
unknown pyc codegen bug.
**Affects:** `ifa/codegen/cg.cc`'s `write_c_prim` polymorphic-dispatch
emission (the `classes[]` branch) and its LLVM-backend sibling in
`ifa/codegen/cg_emit_llvm.cc` (same function/branch, independently
implemented) — both fixed, same day, same root cause.

## Symptom

`kanoodle.py` (a pentomino-tiling solver) segfaulted with no
diagnostic at all. `valgrind` isolated it precisely:

```
Invalid read of size 8
   at 0x10DD1D: _CG_f_8695_516(_CG_s12128*, _CG_s12111*) (Omino::translate)
   by 0x1233BD: _CG_f_107_83()
   by 0x1239DF: main
 Address 0x1 is not stack'd, malloc'd or (recently) free'd
```

`Omino.translate(self, v)` is inherited (not overridden) by ~11
concrete tile subclasses (`A`, `B`, `C`, ...). Called on a receiver
whose static type is a genuine union of those subclasses (`tile` in a
loop over a heterogeneous tile list), the generated C dispatch code
was:

```c
t120 = _CG_prim_tuple_list(_CG_ps12111, 2);  // v = [col, row], correctly built
t120->e0 = t131;
t120->e1 = t132;
if ((*(_CG_TypeObject**)(void*)t119) == &_CG_type_B) {
  t115 = ((_CG_any(*)(void*))((_CG_ps12129)(void*)t119)->e8)((void*)t119);
  //                    ^^^^^^                              only self passed --
  //                                                         t120 (`v`) computed, never used
}
```

`t120` — the second argument, `v` — is fully computed right before the
call and then never passed. The callee reads whatever happens to be
in the ABI slot its second parameter expects, undefined behavior that
manifested here as dereferencing `0x1`.

## Root cause

`cg.cc`'s `write_c_prim`, in the polymorphic classtag-dispatch branch
(one `if (tag == &_CG_type_X) { ... }` arm per candidate receiver
class sharing one call site), hardcoded the emitted function pointer
cast to a single `void*` parameter and the call to a single `(void*)
recv` argument, **unconditionally**, regardless of how many live
formal arguments the method actually has:

```cpp
fprintf(fp, "((%s(*)(void*))((%s)(void*)%s)->e%d)((void*)%s);\n", ret_type_str,
        cg_get_string(classes[ci]), recv_str, slots[ci], recv_str);
```

The three sibling branches in the *same* function — `nil_fn` (a
None-receiver candidate), `plains` (bare callable values), and
`directs` (untagged single-candidate receivers) — all correctly loop
over the candidate's `positional_arg_positions` and pass every live
argument, cast appropriately. Only this one branch, the most common
one (ordinary polymorphic method calls through a receiver classtag),
never got that treatment — looks like a straightforward omission
when this branch was first written, not a deliberate simplification
(nothing else in the surrounding code suggests self-only calls were
intentional).

The LLVM backend (`cg_emit_llvm.cc`) has its own, independently
written, parallel implementation of the identical dispatch shape (per
its own comment, "mirrors cg.cc's emit_send_call polymorphic
branch") — and had the exact same bug, expressed as LLVM IR instead
of a C-source printf template:

```cpp
llvm::FunctionType *fty =
    llvm::FunctionType::get(res_ty ? res_ty : llvm::Type::getVoidTy(*TheContext), {ptr_ty}, false);
llvm::Value *callv = Builder->CreateCall(fty, fnptr, {recv});
```

Same story: the `nil_fn` branch immediately above it in the same
function already has the correct argument-routing loop; this branch
never got it.

## Fix

Both backends: track each `classes[]`/`slots[]` entry's originating
candidate `Fun*` (added a parallel `Vec<Fun *> class_funs`, populated
alongside `classes.add()`/`slots.add()`), then at emission, loop over
that `Fun`'s `positional_arg_positions` — mirroring the already-
correct `nil_fn` branch in each file — building the full parameter
type list and argument list (skipping the position that's `self`,
already passed separately since the vtable-slot load has no concrete
receiver type of its own to derive a full signature from). C backend:
`std::string`-built function pointer cast and argument list. LLVM
backend: `std::vector<llvm::Type*>`/`std::vector<llvm::Value*>` fed
directly to `llvm::FunctionType::get`/`CreateCall`, with the same
int/pointer coercion the `nil_fn` branch already does.

## Verification

- `kanoodle.py` directly: `RUN_FAIL rc=139` (raw segfault) →
  `RUN_FAIL rc=134` (clean, salvage-driven `assert(!"runtime error:
  getter not resolved")` abort on a *separate*, pre-existing,
  already-documented "has no type" violation elsewhere in the
  program — genuine progress, not a new failure).
- Full `test_pyc.py` + `PYC_FLAGS=-b test_pyc.py`: 216/216 both (215
  prior + the new regression test), `ifa`'s `make test` all phases
  clean. One transient, non-reproducible single-test flake
  (`exception_assert.py`) observed once during this round and never
  again across 3 immediate reruns — matches the general flakiness
  already on record elsewhere in this codebase (issue 033's M3
  section, a `pygasus`/`fa_dump_types` segfault that also never
  reproduced); not attributable to this change.
- Full shedskin corpus sweep: zero new diffs beyond `kanoodle`'s own
  `rc=139` → `rc=134` improvement (every other delta already
  accounted for by earlier same-day fixes).
- `adatron`'s compiler-crash flakiness (`rc=139` vs `rc=124` across
  different runs) was checked directly and confirmed pre-existing,
  unrelated, non-deterministic — not attributable to this fix either
  way.
- New test: `tests/poly_dispatch_shared_method_extra_args.py` (a
  method inherited, not overridden, by 11 subclasses, dispatched
  through a genuinely polymorphic receiver list — the same shape as
  `Omino.translate`, distinct from the pre-existing
  `tests/multi_candidate_dispatch.py`, where each class defines its
  own override). Verified against CPython on both backends. **Does
  not reliably discriminate the bug via crash/wrong-output** — tried
  at both default (`-O0`-equivalent) and `OPTIMIZE=1` (`-O3`)
  compile settings; the missing argument happens to read back the
  right value by ABI/register-allocation luck at this scale either
  way. `kanoodle.py` itself, in the shedskin corpus (verified via the
  sweep above), remains the reliable, real-world repro. The test is
  still valuable positive coverage for the exact code shape.

## What this unblocks

Any polymorphic method call — inherited (not overridden) by multiple
concrete classes, dispatched through a receiver whose static type is
a genuine union, taking any argument beyond `self` — was silently
broken (real, if not always visibly manifesting, undefined behavior)
on both backends. This is a common, ordinary OOP shape, not an edge
case; likely to have been an unreported source of intermittent,
hard-to-reproduce crashes or silently-wrong output in other corpus
programs beyond `kanoodle.py` that happened not to hit visibly wrong
ABI luck.
