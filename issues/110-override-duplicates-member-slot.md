# 110 — a method override gets a SECOND member slot, shifting every field after it

**Status:** open, found 2026-08-22 while verifying issues/116's
inheritance cases. **Pre-existing** — reproduced unchanged on
`888cb499`, the commit before that work.

## Symptom

A subclass that overrides one method and inherits another that touches
instance fields reads the wrong field. 19 lines, no iterator protocol,
no exceptions, no generators:

```python
class B:
    def __init__(self, n):
        self.i = 0
        self.n = n
    def more(self):
        return self.i < self.n
    def step(self):
        self.i = self.i + 1

class S(B):
    def step(self):
        self.i = self.i + 2

a = B(3)
while a.more():
    a.step()
print(a.i)

b = S(3)
while b.more():
    b.step()
print(b.i)
```

    CPython:  3 / 4          pyc:  prints nothing, hangs forever

Compiles clean, exit code never arrives. Either loop **alone** is
correct (`3`, or `4`) — it takes both, so that `more` is shared.

## Cause

`S`'s instance layout carries `step` **twice**:

```c
struct /* B */ {                      struct /* S */ {
  _CG_TypeObject *__pyc_tag;            _CG_TypeObject *__pyc_tag;
  _CG_pf2  e10; /* more */;             _CG_pf2  e10; /* more */;
  _CG_pf3  e11; /* step */;             _CG_void e11; /* step */;
  _CG_int64 e13; /* i */;               _CG_void e21; /* step */;   <-- the override
  _CG_int64 e14; /* n */;               _CG_int64 e23; /* i */;
};                                      _CG_int64 e24; /* n */;
                                      };
```

`i` is the 3rd slot on `B` and the 4th on `S`. `more` is emitted **once**
for both receivers, with an `_CG_any` parameter and a blind cast to the
base's layout:

```c
_CG_bool B__more(_CG_any a1) {
  t3 = a1;
  t2 = (_CG_int64)((_CG_ps11799)t3)->e13;  /* "i" -- but on an S this is
                                              the second `step` pointer */
  return _CG_prim_less(t2, "<", 3);
}
```

On an `S` instance that reads a function pointer, compares it against
`n`, and answers True forever.

Neither half is wrong on its own — a shared method over several
receivers is the whole point of the `_CG_any` receiver, and it is sound
**provided the layouts agree on the prefix**. The duplicate slot breaks
that invariant silently.

`ifa/if1/ast.cc`'s `collect_include_vars` is where the duplicate is
introduced:

```c
static void collect_include_vars(Sym *s, Sym *in = 0) {
  Vec<Sym *> saved;
  if (!in) saved.move(s->has);
  else     in->has.append(s->has);          // <-- no name dedup
  for (Sym *ss : s->includes)
    if (ss->type_kind == Type_RECORD) collect_include_vars(ss, in ? in : s);
  if (!in) s->has.append(saved);
}
```

Every base's `has` is appended wholesale, then the class's own members
are appended after it. A name defined in both simply appears twice.

The recursion compounds it whenever a base has **already been
flattened**: `include_instance_variables` walks `collect_includes`'
dependency order, so a base's `has` normally contains its own bases'
members by the time a subclass is processed — and the subclass then
descends into that base's `includes` and appends them a second time.
issues/116's bridged classes show this plainly: `Doubling(Counter)`,
where `Counter` includes `__pyc_iterator__`, gets the bridge's **entire**
member set twice, putting `i` at `e55` where `Counter` has it at `e29`.
The recursion is not simply removable — a base whose `id <
finalized_types` is skipped by that same loop and really has not been
flattened.

## Why the suite never caught it

The failure needs all three of: an override, a *sibling* inherited
method that touches fields, and enough receiver variety that the
inherited method is shared rather than monomorphized per receiver. Drop
any one and it passes. `tests/` has overrides and it has field access,
but no test combined them with two live receiver types through one
method.

That third condition is also why issues/116's `Doubling(Counter)` case
passes today despite the identical layout damage — `__pyc_more__`
happened to be cloned per receiver there. It is one monomorphization
decision away from the same hang.

## Fix

Dedup `has` by member name in `collect_include_vars`, most-derived
winning, so a subclass's layout is a strict prefix-compatible extension
of each base's.

Not a one-liner in practice, which is why this is filed rather than
fixed inline:

- **Which duplicate survives changes codegen.** The emitted prototype
  setters for `S` write *both* `B::step` and `S::step` to `e11`
  (last-store-wins on the base's slot), while `e21` is written nowhere —
  so today's dispatch reads the base's slot and the override's slot is
  dead weight. Dropping the wrong one of the pair silently changes which
  function a call reaches.
- **Layout order is load-bearing beyond this function.** `has` order
  drives struct emission, prototype setter emission, and the field
  indices baked into every already-cast accessor.
- **The blast radius is every record in every program**, so it needs a
  full before/after sweep of both suites plus the corpus, not a
  suite-green check.

## Verification plan

- The repro above prints `3` then `4` and terminates.
- Each loop in isolation still prints the same as it does today.
- `S`'s emitted struct has exactly one `step` slot, and `i`/`n` at the
  same indices as `B`'s.
- `Doubling(Counter)` from `tests/iterator_protocol_bridge.py` carries
  one copy of the `__pyc_iterator__` members, not two.
- Both suites unchanged, and the corpus shows no new compile failures or
  new runtime divergence (see [[corpus-compiles-but-crashes]] — rc=0 is
  not evidence on its own).

## What this blocks

Any ported library or user program with an ordinary override. This is
the plainest shape in object-oriented Python there is, and it hangs.

`tests/iterator_protocol_bridge.py` had to route around it: its
"a base that speaks pyc's protocol natively" case uses a `FastBase`
that deliberately does **not** define `__next__`, because a subclass
overriding it would hit this bug rather than anything about issues/116.
