# 079 — the single-candidate dispatch fast path casts without checking the union's other members

**Status:** open, CLEANUP, not attempted. **Shrunk 2026-10-16** to the
codegen safeguard alone: the crash it was filed for can no longer reach
codegen (below), and the correlated-branch half belongs to
[025](025-FA-intra-function-union-narrowing.md). History in git:
`git show 3f36072b:ifa/issues/079-DISPATCH-single-candidate-dispatch-unchecked-cast.md`
(the full original), and `git show e3b44e2c:` this path (the 2026-09-28
rewrite).

## What changed

Filed on `bh`: `b.hack_gravity(...)` with `b : Body | Cell`, where `Cell`
has no `hack_gravity`, compiled to a direct call to `Body.hack_gravity`
with `b` cast to `Body *`, and segfaulted on a `Cell`.

That shape is now refused in every mode, `--strict` included, with
`illegal call argument type expression illegal: B`. A union member with no
candidate for the method is a type violation, and since
[closed/158](closed/158-FA-every-type-violation-is-fatal.md) every violation is fatal.
That was the original issue's "in FA" half. Re-verified 2026-10-16 on
`c7b54d6a`, both with a reachable `B` (`x = A() if c else B(); x.m()`)
and with the call guarded by a correlated `if c:`.

The correlated case is a valid CPython program (it prints `done`) that pyc
now REFUSES. That is not this issue: it is
[025](025-FA-intra-function-union-narrowing.md), branch correlation over a
class union. The original "permissive raises `AttributeError`" remedy needs
025's narrowing first, and moves there with it.

## What remains: the fast path itself

`cg.cc:2912` (`directs.n == 1`), after the nil test and the classtag
compares, still calls the one remaining candidate with `(ft)(void*)arg`.
Its comment assumes *"the single untagged candidate is everything the nil
test / tag compares above didn't claim"*. FA now guarantees that, but
codegen does not check it, and two things the neighbouring paths do are
missing here:

- **The blind-cast layout contract.** The classtag branches above it record
  their casts with `cg_note_blind_cast` (`cg.cc:295`); this one does not, so
  a cast between layouts that differ is not reported.
- **A no-match guard.** The multi-candidate path ends in
  `assert(!"runtime error: polymorphic dispatch: no branch matched")`
  (`cg.cc:2938`). This one has no equivalent.

## The fix

At codegen time, compute the receiver's union minus what the nil test and
the tag branches above covered, from the `plains`/`directs` construction
codegen already has (do not re-derive it). Every member left must be one
`directs[0]` accepts. Record the cast with `cg_note_blind_cast`, and if a
member is left that it does not accept, fail the compile naming it: FA should
have refused, so reaching this is an FA bug, not a runtime case.

This is the hottest dispatch path. The check runs in the compiler only: a
monomorphic receiver (the common case) must stay a direct call with no
emitted test.

## Verification

The corpus's emitted C is byte-identical before and after (ifa/147's oracle:
`pyc` each program, diff the `.py.c`). The two probes above are still refused
in every mode. `make test`.
