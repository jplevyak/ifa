# 181 — codegen: a prim no emitter claims produces no code, and its result reads as 0

**Status:** open. Filed 2026-10-02, found fixing
[180](closed/180-LLVM-pow-operator-emitted-nothing.md).

## Symptom

`virtual_cg_emit_send` (codegen_common.cc) tries each emitter in turn and
ends with a bare `return` when none claims the prim. The node then
produces no code. If its result is live, a read of that Var comes back as
a zero constant, and the program compiles, runs, and prints a wrong
answer with no diagnostic. That is how `**` printed 0 on the LLVM backend
(180).

## Census (`IFA_DBG_NOEMIT=1`, whole test suite, 2026-10-02)

The C backend reaches the fall-through ZERO times. The LLVM backend
reaches it in four ways:

| prim | count | result | what it is |
| --- | --- | --- | --- |
| `prim_setter` | 2240 | dead | method-slot stores (`obj.__len__ = <fun>`): the stored value is a function symbol, which has no LLVM value, so `emit_send_setter` declines at `if (!obj \|\| !val)` |
| `prim_period` | 3 | LIVE | genetic2_idioms (2), none_receiver_dead_path (1) |
| `prim_id` | 1 | LIVE | `hash(f)` of a function, hash_of_object_and_function |
| `prim_primitive` | 1 | dead | |

- **The setters are probably harmless.** The LLVM backend installs method
  pointers at clone time from `cg_new_to_val_map`, not from these stores.
  Unverified: nothing checks that the registry covers every slot one of
  these setters would have written.
- **`prim_id` is a real miscompile.** On `-b`, `hash(f) == hash(g)` is
  `True` for two different functions, and `hash(f) != 0` is `False`.
  CPython prints False and True. The test passes by accident, because it
  only compares `hash(f) == hash(f)`, which is 0 == 0. Same cause as the
  setters: a function used as a VALUE has no LLVM value.
- **`prim_period`** in none_receiver_dead_path is an attribute read on
  `None` in a path that never runs. The C backend emits a None check
  there. genetic2_idioms is not yet classified.

## Fix

1. Give a function symbol an LLVM value: the `llvm::Function*`, as cg.cc
   emits `(void*)_CG_f_N`. This fixes `prim_id`, and lets
   `emit_send_setter` claim method-slot stores, or skip them explicitly
   with a comment saying the registry covers them.
2. Classify the two genetic2_idioms `prim_period` cases.
3. Then make the fall-through `fail()` on a prim whose result is live, so
   the next missing emitter is a compile error instead of a silent 0.
   Re-run the census first: it must be empty of live results.

## Verification

`IFA_DBG_NOEMIT` reports nothing with a live result across the suite. A
test asserts `hash(f) != hash(g)` on both backends.
