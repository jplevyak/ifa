# 079 — Method dispatch's "single candidate" fast path emits an unchecked cast when the receiver's OTHER union members don't implement the method at all

**Status:** open, root-caused, not fixed (user asked to document only,
not attempt the fix — it touches a hot, pervasive codegen path used by
every single-implementation method call in every program).
**Affects:** `ifa/codegen/cg.cc`, the polymorphic-dispatch emitter
around line 1725 (the `directs.n == 1` branch quoted below). Almost
certainly also `ifa/codegen/cg_emit_llvm.cc`'s equivalent path (not
checked — this issue was found and confirmed on the C backend only).
**Related:** [030](030-polymorphic-dispatch-fat-pointers.md) — the
classtag/fat-pointer dispatch design this call site is part of; that
issue's own "core implemented" status covers the `fns->n > 1,
multiple implementations` case (tag-compare branches, verified
working) and documents two *other* known-open gaps (closure-carrier
mixed dispatch, high-fan-out table emission) — **neither of those is
this bug**; this is a third, previously-undocumented gap in the same
subsystem, found via [issues/039](../../issues/039-list-mul-shared-element-type-cross-contamination.md)'s
`bh.py` investigation.
[077](closed/077-primitive-equality-codegen-missing-salvage-guard.md)/[../../issues/closed/034](../../issues/closed/034-iadd-fallback-and-mixed-numeric-regression.md)/[../../issues/035](../../issues/035-list-element-cast-salvage-guard-and-set-item-union.md)/[../../issues/037](../../issues/037-sudoku2-str-ne-void-cast-and-str-index.md)
— the established "degrade a genuinely-incompatible-type situation to
a runtime assert instead of emitting unsafe C" convention this call
site should follow but doesn't.

## Symptom

`shedskin_examples/bh/bh.py` segfaults at runtime with no output.
`b.hack_gravity(self.rsize, self.root)` (`bh.py:555`), where `b`'s
static type is `Body | Cell` ([issues/039](../../issues/039-list-mul-shared-element-type-cross-contamination.md)
explains why `Cell` ends up in that union at all — a separate,
independent bug) and `hack_gravity` is defined *only* on `Body`
(`Cell` has no implementation whatsoever), compiles to:

```c
t36 = _CG_f_11052_101/*Body::hack_gravity*/((_CG_ps14952)t33, t34, t35);
```

`t33` is `_CG_any` (boxed/generic — the value came out of
`__list_iter__::__next__`, whose element type is a union). This is a
**raw, unchecked C cast** straight to `Body::hack_gravity`'s exact
parameter struct type, with no runtime discrimination of any kind.
When the actual receiver object is smaller/differently-laid-out than
`Body` expects (which a genuine `Cell` instance would be — different
field count, different offsets), this reads/writes memory at the
wrong offsets: undefined behavior, observed as a segfault.

## Root cause

`cg.cc`'s polymorphic dispatch emitter partitions a call site's
resolved candidate functions into groups and emits, in order: nil-test
branches, per-class tag-compare branches for classes with distinct
implementations (`plains`), and then a fallback for whatever's left
over:

```cpp
if (directs.n == 1) {
  // The single untagged candidate is everything the nil test / tag
  // compares above didn't claim -- direct call, no discrimination
  // needed (or possible).
  Fun *fv = directs[0];
  fputs(nb ? "  else {\n" : "  {\n", fp);
  ...
  fprintf(fp, "%s(", cg_get_string(fv));
  ...
} else {
  fputs("  else { assert(!\"runtime error: polymorphic dispatch: no branch matched\"); }\n", fp);
}
```

The comment's own reasoning — "everything the nil test / tag compares
above didn't claim" is safe to call directly — is correct **only**
when every other member of the receiver's static type union either
(a) got its own tag-compare branch above (because it has its own
distinct implementation), or (b) is nil (handled by the nil test). It
is **not** correct when the union has a member with **no
implementation of the method at all** — that member was never a
"candidate" to begin with, so it never shows up in `plains` or gets a
tag-compare branch, but it's still a live possibility for what `b`
(the cast's operand) might concretely be at runtime. The `directs.n
== 1` branch has no way to tell "this really is the only possible
class" from "this is the only class that HAS an implementation, but
others without one could still show up here" — and silently assumes
the former.

Contrast with the `fns->n > 1` (multiple implementations) case, which
already has a graceful degrade: `else { assert(!"runtime error:
polymorphic dispatch: no branch matched"); }`. The exact same
graceful degrade is what's missing for the "one implementation, but
the union has other, non-implementing members" case — the bug is that
this shape gets routed into `directs.n == 1`'s unconditional-call
branch instead of being detected and routed to that same assert (or
a tag-compare branch that also falls through to the assert for
anything untagged).

## Why this wasn't caught by the existing salvage-guard fixes

Issues 034/035/037/077 each found and fixed a *different* call site
(list-index writes, `str` comparison primitives, ...) missing the same
"genuinely incompatible type reaching a call site that assumes
compatibility, degrade to `assert` instead of unsafe C" convention.
This is a new instance of that same pattern class, in a call site none
of those touched: polymorphic method dispatch's single-implementation
fast path.

## Proposed fix (not attempted)

At the point `directs.n == 1` is decided, also check whether the
receiver's full static type union (`pn`'s receiver AVar's resolved
`AType`/`CreationSet`s — whatever `cg.cc` already has on hand from
computing `plains`/`directs` in the first place) contains any member
class that is **not** `directs[0]`'s own class and is **not** already
covered by a nil test or a `plains` tag-compare branch. If so, this
is not really a "single candidate" situation — either:
1. Add a tag-compare branch for `directs[0]`'s own class too (turning
   it into one more `plains`-style guarded branch), with the final
   `else` falling through to the existing
   `assert(!"runtime error: polymorphic dispatch: no branch matched")`,
   or
2. More simply, whenever the union has any uncovered member at all,
   skip the unconditional-call fast path entirely and fall through to
   that same assert.

Needs care: this is one of the hottest paths in codegen (used for
every method call with a genuinely monomorphic — single-implementation
— receiver, the overwhelming common case), so (a) false positives
(routing an actually-safe single-implementation call through an
unnecessary tag-compare or assert) would be a real regression risk,
and (b) correctly enumerating "the receiver's full union, minus what's
already covered" needs to reuse whatever `plains`/`directs`
construction already does rather than re-deriving it — not scoped or
prototyped here.

## Verification plan (for whoever attempts this)

- `bh.py`'s segfault becomes a controlled
  `assert(!"runtime error: polymorphic dispatch: no branch matched")`
  (or, if [issues/039](../../issues/039-list-mul-shared-element-type-cross-contamination.md)
  is fixed first, the union no longer contains `Cell` at all and
  `bh.py` may compile and run correctly with no assert needed).
- Full `test_pyc.py`, both backends, both `PYC_CSM` settings — this
  touches the single-implementation dispatch fast path, i.e. most
  method calls in most programs, so a regression here would likely
  show up broadly; treat any new failure as a signal to narrow the fix
  condition, not to special-case around it.
- A clean before/after `shedskin_sweep.sh` (stash/sweep/pop/rebuild/
  sweep from the same commit) given the blast radius.
- New regression test: two sibling classes of a common base, one
  implementing a method the other doesn't, dispatched through a list
  whose static element type is (genuinely or spuriously) the union of
  both — confirm a call that resolves to the "wrong" class member logs
  the assert rather than corrupting memory.

## What this unblocks

- `bh.py` failing safely (assert) instead of segfaulting — doesn't by
  itself make it compile/run cleanly (that needs
  [issues/039](../../issues/039-list-mul-shared-element-type-cross-contamination.md)
  too), but converts a memory-safety bug into a diagnosed one, matching
  every other salvage-reachable call site in this codebase.
- Any other program (not just `bh.py`) where a receiver's static type
  union includes a class that doesn't implement the called method at
  all, reached via the `directs.n == 1` fast path — a general
  correctness gap in the dispatch emitter, not corpus-specific.
