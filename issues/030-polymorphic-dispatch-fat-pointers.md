# 030 — Polymorphic dispatch via fat-pointer backward flow analysis

**Status:** core implemented (2026-07-04) — classtag dispatch works
end-to-end on BOTH backends; `tests/poly_dispatch_high.py` executes
its full expected output (3 3 6 7 11 18) and `poly_dispatch_low.py`
passes strictly (its `check_fail` sidecar removed). See "What
landed (classtag dispatch)" below. Remaining open scope: the
receiver-less bare-callable extension (issue 007's decorator shape)
and table-based emission for very high fan-out (the if/else chain
is O(n_classes) per site today).

Supersedes the simpler "indirect call through struct slot" sketch in
issue 029.

## Current state (issue 029 implemented; FA fixpoint sub-bug fixed 2026-07-04)

`tests/poly_dispatch_low.py` passes: vtable slot dispatch works for
`Branch.val()` where `Branch.l` and `Branch.r` can be Leaf or Branch.
At creation time, `__new__` stores the concrete val function pointer in
the struct's `e2` slot.  At dispatch sites where `fns->n > 1`, the
emitter reads `obj->e2` via the common struct cast and calls it.

**The FA fixpoint sub-bug is fixed** (2026-07-04). The old symptom:
the clones for `Inner(N1,N2)` and `Inner(N2,N1)` ended up with
void/dead result vars ("expression has no type" violations, then
`convert_NOTYPE_to_void()` papering them over). Root cause, traced
via env-gated tracing of the period-closure lifecycle: bound-method
closure CreationSets persist across analysis passes (cached in the
result AVar's `cs_map`), but every pass clears all AVar state
(`clear_results`, including `closure_used`) and re-derives it.
`make_closure_var` flowed each field's value into
`unique_AVar(av->var, cs)` — keyed by whichever Var carries the
value *this* pass — while consumers (`partial_application`'s
`fun = cs->vars[0]`) read the *positional* slots created by the
pass that built the CS. After the receiver CS split between passes,
the method value arrived via a different Var, the re-derived flow
landed in an orphan AVar, `cs->vars[0]` stayed bottom, the closure's
call site returned "incomplete" forever, `closure_used` was never
re-set on the final pass, and `remove_unused_closures()` stripped
the closure — hence the void results. Fix in
`fa.cc:make_closure_var`: when the CS's positional slots already
exist and `iv != cs->vars[i]`, also flow into `cs->vars[i]`, keeping
the positional slot fed regardless of which Var carries the value.

After the fix: the minimal two-creation-site swapped-children case
compiles clean and executes correctly on both backends — new
regression test `tests/poly_dispatch_swapped.py`. The full
`poly_dispatch_high.py` now compiles with **zero** FA warnings
(its `.check` updated to empty); at runtime the leaf-leaf and
mixed clones behave, and what remains is the genuine high-fan-out
dispatch gap: the `Inner`-receiver clone with polymorphic children
still hits the `matching function not found` runtime assert — that
is this issue's actual fat-pointer/classtag work, not a fixpoint
problem.

`tests/poly_dispatch_high.py.exec.check.TODO` held the expected
execution output (3 3 6 7 11 18); it is now the live
`.exec.check`, passing on both backends.

## What landed (classtag dispatch, 2026-07-04)

The failing piece after the fixpoint fix was isolated to three
independent gaps, each fixed:

1. **Liveness** (`ifa/optimize/dead.cc` `mark_live_avars`): a
   polymorphic call site (calls->n > 1) now keeps its dispatch
   operand (rvals[0]) live even when no candidate's body reads its
   formals — with `l: N2|N4` and both leaf `val()`s ignoring self,
   the *choice* of callee still depends on the operand's runtime
   type, but the whole period-load chain used to go dead and
   codegen had nothing to dispatch on.
2. **Classtag** (both backends): 029's fixed-slot indirect call was
   unsound across a union of classes with different layouts (leaf
   structs carried `val` at e1, Inner at e2 — the "inherited from
   the base ⇒ same slot" assumption does not survive per-class
   dead-field elision). Every class-like record now carries a
   `__pyc_tag` header at offset 0 (C: struct member; LLVM: leading
   ptr field with `llvm_fld()` shifting all has-index GEPs),
   stamped once into the class prototype at `prim_new` (instances
   inherit it through clone's memcpy), pointing at the existing
   per-class `_CG_type_<name>` object (LLVM: an internal global
   with matching name; only address identity matters). Dispatch
   emits an if/else chain on the tag; each class branch casts to
   THAT class's layout and calls through THAT class's own stored
   method slot — so the per-creation-site clone selection keeps
   riding the 029 stored-pointer mechanism, and same-class clones
   need no extra branches. Excluded from tagging (raw-layout
   types, identified structurally by having only unnamed fields
   plus the tuple/list/vector predicates): tuples, list-backed
   tuples, vectors — `_CG_TUPLE_TO_LIST_FUN`'s memcpy and
   `_CG_list_ptr` indexing treat those as bare element arrays
   (found the hard way: 15-test regression, then a 2-test tuple
   regression from tuple clones missing from
   `sym_tuple->specializers`).
3. **Constant returns through indirect calls** (C backend
   `c_rhs` + the dead-reply emission): a constant-folded result
   deadens the reply under the "consumers inline the literal"
   protocol — but a caller reaching the clone through a stored
   method pointer cannot inline a per-clone constant. Dead replies
   now return the formatted literal (`sprint_imm`/string/nil
   handling) instead of a bare `0`. (The LLVM path already
   materialized constants in `value_for_var`.)

Shared infrastructure moved to `codegen_common.{h,cc}`:
`cg_has_classtag`, `cg_field_live`, `PolymorphicSlot`,
`cg_new_to_val_map`, `cg_build_new_to_val_map` (the LLVM backend
previously had NO slot-store emission at all — that, plus the
dispatch chain, is why `poly_dispatch_low` was `check_fail` on
LLVM; it now passes strictly on both backends and the sidecar is
removed).

Verification: `./test_pyc` 132/0, `PYC_FLAGS=-b` 132/0,
`ifa/ifa-test` 14/14; poly_dispatch_{low,high,swapped} pass
strictly on both backends.

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

## Related: bare callable-value dispatch (no receiver)

`../../issues/007-decorators-not-applied.md`'s re-check (2026-07)
found the same `get_target_fun_core`-returns-null gap for a call site
with no receiver at all — a plain function-typed variable (e.g.
reassigned by a decorator to one of two different closures) called
directly (`g(5)`), rather than a method called through an instance
(`obj.method()`). The fat-pointer/classtag design above is framed
entirely around receiver objects (`obj->classtag`, `obj->e<N>` method
slots); extending it to bare callable values would need first-class
function values to carry the same kind of taggable/dispatchable
representation, even when they were never wrapped in a user-visible
class. Not scoped or designed here — flagging as a needed extension
to this issue's design before it can unblock issue 007.

## Tests to add

- `tests/poly_dispatch_small.py` — 2-way dispatch (fan-out ≤ threshold):
  `Const | BinOp` receiver, `eval()` call, assert correct values.
- `tests/poly_dispatch_large.py` — fan-out > threshold: 5+ subclasses,
  same method name, table path exercised.
- `tests/isinstance_class.py` — `isinstance(x, Const)` for non-None
  class, both true and false branches.
- Existing `tests/expr_evaluator.py` should continue to pass (workaround
  path); eventually replace with the OOP version as a regression guard.
