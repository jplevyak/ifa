# 003 — `prim_cast` and `prim_meta_apply` transfer functions

Two IFA primitives — `prim_cast` (index 31) and `prim_meta_apply`
(index 45) — are registered in the IR vocabulary but their FA
transfer functions are stubbed. Neither is currently emitted by any
live frontend. We convert the stubs from `assert(!"implemented")`
(silent abort) to structured `fail()` calls (diagnostic + exit) and
document here both the original sketches and what a real
implementation would need.

## Where the idea came from

`prim_cast` — a C-style `(T) expr` cast. Reuses the `"("` slot in
`prim_map[1][0]` in `ifa/if1/prim_data.cc`, so an old V grammar
(or a future extension) parsing a parenthesized type prefix as a
cast would lower to a SEND targeting this prim. The semantics are
straightforward: regardless of what type the value had, the result
is the named target type — the user is asserting the conversion is
valid.

`prim_meta_apply` — parameterized type application. Given two
meta-types (the meta-type of a generic `List` and the meta-type of
`int`), produce the meta-type of `List<int>`. The sketch below
walks every (CreationSet × CreationSet) cross product in the two
operand abstract types, looks up `meta_apply(metaT1, metaT2)`, and
unions the results.

## What was there

### `prim_cast` (pre-removal at `fa.cc:1961`)

```cpp
case P_prim_cast: {
  assert(!"implemented");
  break;
}
```

Signature (from `prim_data.cc:259-264` and `prim_data.dat:50`):

```
prim_cast "(" 3 0 {PRIM_TYPE_SYMBOL, PRIM_TYPE_ANY} {PRIM_TYPE_ANY}
```

That is: takes a symbol (the target type) plus the value being
cast; returns one value.

The clone-equivalence helper at `clone.cc:234` is implemented and
stays — it requires two clones to agree on the source-value type
of the cast. (Harmless if the prim never fires.)

### `prim_meta_apply` (pre-removal at `fa.cc:1639`)

```cpp
case P_prim_meta_apply: {
#if 0
  AVar *a1 = make_AVar(p->rvals[1], es);
  AVar *a2 = make_AVar(p->rvals[2], es);
  Sym *s;
  for (CreationSet *cs1 : a1->out->sorted)
    for (CreationSet *cs2 : a2->out->sorted)
      if (cs1->sym->is_meta_type && cs2->sym->is_meta_type &&
          (s = meta_apply(cs1->sym->meta_type, cs2->sym->meta_type)))
        update_gen(result, make_abstract_type(s));
      else
        type_violation(ATypeViolation_kind::SEND_ARGUMENT, a1, a1->out, result);
#else
  assert(!"implemented");
#endif
  break;
}
```

Signature (from `prim_data.cc:351-357` and `prim_data.dat:66`):

```
prim_meta_apply "meta_apply" 3 0 {PRIM_TYPE_ANY, PRIM_TYPE_ANY} {PRIM_TYPE_ANY}
```

That is: takes two ANY values (the meta-types) and returns one.

## Why the sketches are dormant

`prim_cast`: no live frontend emits it. The V grammar
(`ifa/frontend/v.g`) parses `"("` as `paren_block` (statement
grouping), never as a cast. The pyc Python frontend never
references `prim_cast` or `sym_cast`. Every test fixture is clean.
The slot is wired but unused.

`prim_meta_apply` as a primitive: same — no `(send @primitive
prim_meta_apply …)` form is ever constructed. The
symbol-named `@meta_apply` send seen in `for1.v.code` is a
*different code path*: it dispatches through the builtin symbol
`sym_meta_apply` (`ast.cc:101`), which is matched by pattern
dispatch to user-or-runtime-supplied functions tagged
`:in @anytype`. That path is live and well-tested. The
`prim_meta_apply` transfer function is only reachable by a SEND
explicitly targeting the primitive.

The `#if 0` sketch above also depends on three things that don't
exist in the current tree:

- `Sym::is_meta_type` — a bit indicating "this Sym is a meta-type."
  No such field is declared in `sym.h`.
- `Sym::meta_type` — exists, but as a pointer to the meta-type of
  the Sym (the upward link). The sketch reads it as if it were the
  *operand* of the application, which is the inverse direction.
- `meta_apply(Sym *metaT1, Sym *metaT2) → Sym *` — a helper that
  would compute the result meta-type from two operands. Not
  declared anywhere.

So the sketch isn't a "almost works, just turn it on" implementation
— it's pseudocode against an API that was planned but never
shipped. Treat it as a design fragment, not running code.

## What the AUDIT / CLEANUP claimed

The AUDIT and the cleanup item both said: "The Python frontend can
hit `P_prim_cast` in edge cases." That doesn't match the current
tree — `grep -rn "prim_cast" python_ifa_*.cc` finds no hits, nor
does any pyc test fixture. The claim was likely true historically
(pre-DParser migration, when V-style cast syntax may have leaked
into Python parsing) and went stale during the frontend rewrite.
Recording this here so a future reader doesn't chase a path that
no longer exists.

## What's there now

Both transfer functions are now structured `fail()` calls:

```cpp
fail("P_prim_cast transfer function not implemented at %s:%d; "
     "no live frontend emits this prim — see ifa/notes/003 for context",
     filename, line);
```

`fail()` prints the message and `exit(1)`s, so the *crash semantics*
are unchanged from the assert — but the message names the
primitive, gives the source position when available, and points the
next reader at this note.

## What reviving them would require

### `prim_cast`

1. Pick a frontend that wants C-style cast syntax and lower
   `(T) value` to `(send @primitive prim_cast T value)`.
2. The transfer function is short: `result`'s abstract type is
   `make_abstract_type(target_sym)`. The argument value's type is
   ignored at the cast site (the user is asserting the conversion);
   downstream uses of `result` constrain the type instead.
3. Decide whether to emit a `type_violation` when the asserted
   target type isn't subsumable from the source type's abstract
   type. (Probably no — a cast is exactly the place where the user
   is overriding the analyzer.)
4. Wire LLVM codegen: today `llvm.cc:233` lists `P_prim_cast` as a
   fall-through "no LLVM lowering" case. Decide whether to emit a
   pointer bitcast, an int-to-int truncation/extension, or a
   call into a runtime coercion helper, depending on the prim
   arguments.

### `prim_meta_apply`

Bigger. Needs:

1. A design for what "meta-type application" means — likely a
   parameterized-type system on top of IFA's current monomorphic
   one. The V paper sketches this in the context of generic
   classes.
2. The `Sym::is_meta_type` bit and the directional `Sym::meta_type`
   field (or some replacement of the symmetric `meta_type` link).
3. The `meta_apply(Sym *, Sym *) → Sym *` helper — probably backed
   by a cache keyed on the operand pair.
4. A frontend lowering rule that recognizes `T[U]` (or whatever the
   surface syntax is) and lowers to
   `(send @primitive prim_meta_apply T U)`.
5. Clone-equivalence and codegen treatment paralleling
   `prim_cast`'s.

If the design is settled, the `#if 0` sketch is a starting point
for the body of the transfer function — but only after the missing
pieces above land.

## See also

- [../IFA.md](../IFA.md) §11.7 — short inline note.
- [../analysis/CLEANUP.md](../analysis/CLEANUP.md) tier-2 item 2 —
  the cleanup record.
- The V paper (Plevyak), §5 — discussion of meta-type application
  in the generic dispatch framework.

## History

Asserts converted to structured `fail()` calls and the `#if 0`
sketch removed from `fa.cc`, June 2026. Git history before that
point preserves the asserts and the sketch verbatim.
