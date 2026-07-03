# 030 — Polymorphic dispatch via fat-pointer backward flow analysis

**Status:** open / partially implemented (issue 029 struct-slot dispatch done).

Supersedes the simpler "indirect call through struct slot" sketch in
issue 029.

## Current state (issue 029 implemented)

`tests/poly_dispatch_low.py` passes: vtable slot dispatch works for
`Branch.val()` where `Branch.l` and `Branch.r` can be Leaf or Branch.
At creation time, `__new__` stores the concrete val function pointer in
the struct's `e2` slot.  At dispatch sites where `fns->n > 1`, the
emitter reads `obj->e2` via the common struct cast and calls it.

`tests/poly_dispatch_high.py` is a compile-only test with expected
warnings.  The 6-way `Inner.val()` case (N1–N5 and Inner as `l`/`r`)
exposes a FA fixpoint issue: the clones for `Inner(N1,N2)` and
`Inner(N2,N1)` end up with void/dead result vars because `convert_NOTYPE_to_void()`
fires during the FA pass.  Fixing this requires either:
- Ensuring the FA fixpoint converges for these leaf-leaf cases before
  converting NOTYPE to void, OR
- Implementing the full fat-pointer / classtag design below so the
  codegen no longer depends on FA-cloned result types being non-void.

`tests/poly_dispatch_high.py.exec.check.TODO` holds the expected
execution output (3 3 6 7 11 18) for when this is fixed.

Supersedes the simpler "indirect call through struct slot" sketch in
issue 029.  That sketch assumed method pointers are already in instance
structs; this design makes them explicit and handles the full dispatch
spectrum.

## Core idea

A **fat pointer** is a `(data*, vtable*)` pair (or equivalently a
tagged union pointer).  Instead of emitting an unconditional direct
call when `get_target_fun_core` finds multiple candidate Funs, we:

1. **Backward-flow from dispatch points** to find where the
   union-typed value was created (allocation sites / join points).
2. At each creation site, materialize the fat pointer: tag the
   allocation with the concrete type, or store a pointer to the
   appropriate dispatch row.
3. At the dispatch site, emit code chosen by fan-out:

   - **Low fan-out (≤ N, say 4):** emit a conditional tree
     (if/elif/else) that checks the tag and calls the concrete Fun
     directly.  Gives the compiler full visibility of each branch,
     enabling inlining and type specialization.
   - **High fan-out (> N):** emit a table lookup or indirect call
     through the vtable pointer.  O(1) dispatch cost, no code-size
     explosion.

## Why backward flow rather than forward

FA already propagates types forward and splits ESes.  The multi-target
problem arises when two ESes merge at a call site (e.g. `lhs` that is
`Const | BinOp` at the `eval()` call).  Rather than trying to split
ESes further (the "FA per-receiver-CS splitter" in issue 029, which can
explode specialization), backward analysis from the dispatch point
identifies the exact AVar chain that caused the union and knows the
full set of concrete types.

## Relationship to existing FA machinery

- `AVar::out` at the dispatch receiver already enumerates the concrete
  `CreationSet`s — that IS the set of candidate Funs.
- Backward flow means walking `AVar::in` edges from the receiver AVar
  back to the allocation sites.  The path is already computed by FA;
  we just need to tag each allocation with a class discriminant.
- The tag can be a small integer written at allocation time
  (`GC_malloc` + `->classtag = K`), which also satisfies
  `isinstance(x, Class)` (see issue 029's related-issue section).

## Dispatch emission in codegen

In `write_send` (`ifa/codegen/cg.cc`) / `emit_send` (LLVM path),
when `get_target_fun_core` returns null:

```
candidates = f->calls.get(n)   // Vec<Fun*>, n > 1
fan_out = candidates->n

if fan_out <= DISPATCH_THRESHOLD:
    emit if/elif chain on receiver->classtag
    each branch: direct call to candidates[k]
else:
    emit indirect call through vtable slot
    (vtable indexed by classtag, slot by method index)
```

`DISPATCH_THRESHOLD` is a tunable (suggest 4 as default).

## isinstance support

Once `classtag` is materialized at allocation sites, `isinstance(x, C)`
lowers to `x->classtag == C_tag || is_subclass(x->classtag, C_tag)`.
For the common single-concrete-type check this is a single comparison.
The subclass walk can be a small precomputed table (class hierarchy is
known at compile time).

## What needs to change

| Layer | Change |
|---|---|
| Runtime (`pyc_c_runtime.h`) | Add `classtag` field to `_CG_object` base struct |
| IF1 / codegen_common | `get_target_fun_core` → `get_target_funs` returning `Vec<Fun*>*` |
| `cg.cc` `write_send` | Conditional-tree or vtable-indirect emit for fan-out > 1 |
| LLVM `cg_emit_llvm.cc` | Same, in the LLVM IR emit path |
| `build_syms` / `gen_class_pyda` | Assign a unique `classtag` integer per concrete class |
| `pyc_runtime.c` | `_CG_prim_isinstance` using classtag comparison |

## Fan-out threshold rationale

- Conditional trees ≤ 4: branch predictor handles well; each call
  site remains monomorphic after branch prediction warms up; compiler
  can inline the bodies.
- Table / indirect > 4: avoids O(N) code size; matches what a
  traditional vtable dispatch emits; acceptable for highly polymorphic
  sites.

## Relation to issue 029

Issue 029 recorded the symptom and a simpler "struct slot indirect
call" sketch.  This issue records the full design agreed in conversation
(backward flow + fat pointer materialization + fan-out-adaptive
dispatch).  029 can be closed in favour of this issue once implementation
begins.

## Tests to add

- `tests/poly_dispatch_small.py` — 2-way dispatch (fan-out ≤ threshold):
  `Const | BinOp` receiver, `eval()` call, assert correct values.
- `tests/poly_dispatch_large.py` — fan-out > threshold: 5+ subclasses,
  same method name, table path exercised.
- `tests/isinstance_class.py` — `isinstance(x, Const)` for non-None
  class, both true and false branches.
- Existing `tests/expr_evaluator.py` should continue to pass (workaround
  path); eventually replace with the OOP version as a regression guard.
