# 029 — Polymorphic method dispatch unsupported

**Status:** open.  Workaround is tag-based dispatch in
user code (see `tests/expr_evaluator.py`).
**Related:** `../../issues/007-decorators-not-applied.md` hits the
same `get_target_fun_core` gap for a shape this issue doesn't cover —
calling a bare polymorphic function-typed *value* directly (e.g. a
variable reassigned to one of two different closures), with no
receiver object to dispatch a vtable slot through at all. Confirmed
via `def double(f): ...; @double def add_one(x): ...` becoming, after
a prerequisite fix, a call site with 2 candidate `Fun`s and zero
receiver — same `assert(!"runtime error: matching function not
found")` fallback, but the struct-slot lookup in `cg.cc:801-802`
can't apply since there's no struct to look a slot up in.

## Symptom

A method call on a union-typed receiver, where the
union contains multiple class types each defining the
method, fails to compile (or runtime-asserts).  Minimal
repro:

```python
class Expr:
  def eval(self): return 0
class Const(Expr):
  def __init__(self, v): self.v = v
  def eval(self): return self.v
class BinOp(Expr):
  def __init__(self, op, lhs, rhs):
    self.op = op; self.lhs = lhs; self.rhs = rhs
  def eval(self):
    return self.lhs.eval() + self.rhs.eval()

# Uniform child types — works:
print(BinOp(0, Const(1), Const(2)).eval())     # 3

# Mixed child types — broken:
print(BinOp(0, BinOp(0, Const(3), Const(4)), Const(5)).eval())
# runtime: Assertion `!"matching function not found"' failed
```

Inside the outer `BinOp::eval`, `self.lhs` has type
`Const | BinOp`.  The dispatch on `self.lhs.eval()` has
TWO candidate Funs (Const::eval, BinOp::eval).  The C
codegen emits a runtime assert.

## Root cause

`ifa/codegen/codegen_common.cc:97`
(`get_target_fun_core`):

```cpp
Fun *get_target_fun_core(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || fns->n != 1) return nullptr;
  return fns->v[0];
}
```

When the call site resolves to multiple candidate Funs,
the helper returns null and `write_send` falls through
to `fputs("assert(!\"runtime error: matching function not found\");\n", fp);`.

pyc was designed around monomorphic-after-FA dispatch:
the analysis splits ESes until each call site resolves
to one Fun, then codegen emits a direct call.  Multi-
target sites are an unrecovered "FA didn't split far
enough" condition.

## What's there to dispatch through

Instance structs DO carry method pointers consistently
across the class hierarchy.  For the example above,
both `Const` and `BinOp` instance structs put `eval` at
the same slot index (`e2`).  An indirect dispatch
`((_CG_pfX)recv->e2)(recv)` would work — but
`write_send`'s current shape doesn't emit it.

## Fix sketch

Two layers, escalating cost:

1. **Codegen indirect dispatch (smaller).**  When
   `get_target_fun_core` returns `null` AND the call site
   has candidate Funs that all share a method-slot
   index in their receiver classes, emit
   `((_CG_pfX)recv->e<N>)(recv, args)`.  Function-pointer
   type needs a common cast (`_CG_pfX` from any
   candidate Fun — they share a calling convention if
   they implement the same method signature).
   The closure-create SEND that precedes the invoke
   currently gets elided; for polymorphic, it would
   need to be preserved OR the receiver-and-method
   would need to be passed through the invoke
   directly.

2. **FA per-receiver-CS splitter (bigger, more
   precise).**  Extend `split_for_setters_of_setters` /
   `split_ess_for_type` to split ESes when a SEND
   resolves to multiple Funs.  Each split sees one Fun.
   Avoids indirect-call overhead but explodes
   specialization for highly polymorphic code.
   Approach used by CHA-style compilers (Common Higher-
   order Analysis).

(1) is the right starting point.  Adds runtime
polymorphism for the cost of an indirect call, which
matches what user code expects from OOP dispatch.

## Related issue: isinstance for non-None classes

Independent but adjacent.  `isinstance(x, ConcreteClass)`
lowers to `_CG_prim_isinstance(x, T)` which the C
runtime doesn't define.  Only the
`isinstance(x, __pyc_None_type__)` path is wired (in
cg.cc:write_send around line 642, emits NULL check).

To make user-level `isinstance` dispatch work,
codegen would need to:
- emit a class-tag check against `x->classtag` (which
  doesn't currently exist), OR
- emit a runtime helper that walks the class
  hierarchy.

Without this, `if isinstance(x, Const): ...` patterns
(the textbook workaround for polymorphism) also fail
to compile.

## Workaround that works today

Tag-based dispatch in user code: a single struct with
a `kind` discriminator field, and a free function with
`if e.kind == 0: ... elif e.kind == 1: ...`.  See
`tests/expr_evaluator.py` for the full pattern.

This is the C-style discriminated-union shape and pyc
handles it correctly.  The downside is it forces
users to manually write what virtual dispatch / type
narrowing would do automatically.

## Surfaced while

Implementing issue 028 step 5's Tier 3 stress test —
a small expression evaluator with class hierarchy.
Polymorphic OOP dispatch is one of the natural fits
for Python; finding pyc doesn't support it places a
real constraint on what Python idioms compile.

The fib-heap work didn't hit this because every
method (insert, extract_min, etc.) was called on a
single concrete receiver type (Heap).  Subclass-driven
polymorphism is a new axis.

## Side fix in this commit

The simpler "voidish-arg cast" workaround in
`cg.cc:write_send_arg` was restored — when a callsite
arg is `_CG_any`/`_CG_void`/`_CG_nil_type` but the
formal expects a typed pointer, emit
`(formal_t)arg_cg`.  This handles the tag-based
pattern's recursive call (`evaluate(e.lhs)` where
`e.lhs` is `Expr | None`) by casting the union-typed
arg to the typed formal.  Sound because FA proved the
runtime value is `Expr` (the union arose from `None`
being the field's default-init value, not a real
runtime possibility on the recursive path).

The cast was originally added in step 5 then removed
when the `concretize_var_list_type` FA fix subsumed it
for lists.  Restored here because it handles general
union-typed function args, which the FA fix doesn't.
