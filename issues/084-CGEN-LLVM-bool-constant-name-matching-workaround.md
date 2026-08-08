# 084 — LLVM backend materializes `bool` constants by matching the Sym's *name* string, not its immediate value

**Status:** open, found 2026-08-07 during the same audit that produced
[082](closed/082-narrowing-wrapper-names-hardcoded-in-fa.md) and
[083](closed/083-CGEN-print-println-name-collision-risk.md) (closed —
turned out not to be a real cross-frontend risk, see its resolution).

**Affects:** `ifa/codegen/cg_emit_llvm.cc`'s `value_for_var` (the
generic "materialize an LLVM value for this ifa `Var`" function — not
pyc-specific in name or purpose), ~line 506-527.

## What's there

`value_for_var`, given a constant `Sym`, has a generic path that
switches on `s->imm.const_kind` (`IF1_NUM_KIND_INT`/`UINT`/`FLOAT`,
`IF1_CONST_KIND_STRING`/`BYTES`/`SYMBOL`) to build the right kind of
LLVM constant. *Before* that generic switch, there's a special case
that instead matches the Sym's **name** string:

```cpp
if (s->name && (!strcmp(s->name, "True") || !strcmp(s->name, "true"))) {
  if (t->isIntegerTy()) return llvm::ConstantInt::get(t, 1);
  if (t->isPointerTy()) { ... ConstantExpr::getIntToPtr(...) ... }
  if (t->isFloatingPointTy()) return llvm::ConstantFP::get(t, 1.0);
}
if (s->name && (!strcmp(s->name, "False") || !strcmp(s->name, "false"))) {
  ... same, 0 instead of 1 ...
}
```

This is a bake-in on two levels: (1) it's keyed on the display *name*
of pyc's specific `sym_true`/`sym_false` Syms ("True"/"False", Python's
capitalization) rather than on the constant's actual `imm` value, so a
different frontend's own bool-true/false constant Syms — even ifa's own
generic ones, if named differently (e.g. lowercase-only, or something
else entirely) — wouldn't get this treatment at all; (2) architecturally
it duplicates, ahead of, and diverges from the generic `switch
(s->imm.const_kind)` a few lines below.

## Why this might be masking a broader, non-bool-specific gap (not confirmed)

Traced (not reproduced with a live program) why this exists at all,
since `sym_bool`'s `num_kind`/`num_index` — and therefore
`sym_true`/`sym_false`'s `imm.const_kind` (set via `if1_const`, which
reads `type->num_kind`/`num_index` off the passed-in type Sym) — appear
to already be correctly populated by the time `sym_true`/`sym_false`
are constructed in `init_default_builtin_types` (`if1_set_primitive_types`
runs first). So the generic `case IF1_NUM_KIND_INT:`/`UINT:` switch arm
*should* already produce the right value for the common case
(`t->isIntegerTy()`).

The one thing that switch arm does **not** handle, for *any* numeric
constant (not just bool), is `t->isPointerTy()` — i.e. materializing a
literal number into a boxed/`any`-typed (pointer-represented) slot. Only
the name-matched True/False special case has the
`ConstantExpr::getIntToPtr(...)` fallback for that. If that's the real
reason this special case exists, it suggests boxing a literal *int* (not
bool) constant into a generic/`any`-typed slot may hit the same
unhandled-pointer-type gap the bool special case was patched around for
just these two Syms — worth checking with a repro like:

```python
def f(x):
    print(x)
f(True)   # works today (name-matched)
f(5)      # does a literal int box into a pointer-typed formal correctly?
```

**Not verified either way — flagging as a hypothesis for whoever picks
this up, not a confirmed second bug.**

## Why this wasn't fixed live in this session

Every `bool` constant materialized by the LLVM backend goes through
this exact function — it's about as load-bearing as code gets. A
correct fix (make the generic `switch` handle the pointer-type case for
every numeric kind, then delete the name-matched special case entirely)
touches the same function as the possible broader gap above, so it
deserves its own focused investigation + full regression sweep rather
than a same-session patch alongside 082/083.

## Verification plan

- Confirm or rule out the "literal int boxed to pointer type" gap with
  a real repro on `-b` before designing the fix.
- If confirmed: extend the generic `switch`'s int/uint/float cases with
  the same `isPointerTy()` → `getIntToPtr` handling the True/False
  special case already has, then delete that special case (no more
  name matching needed at all — the generic path would legitimately
  subsume it).
- If not confirmed (bool really is special somehow): a smaller fix
  suffices — replace the name match with a value/Sym-identity check
  (e.g. compare `s == sym_true || s == sym_false`, or check
  `s->imm.const_kind` bool-ness by width, not name) so at least an
  ifa frontend with differently-*named* True/False constants still
  gets correct behavior.
- Full `test_pyc.py` both backends either way — bool constants are
  pervasive.
