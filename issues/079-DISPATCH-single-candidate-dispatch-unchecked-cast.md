# 079 — the single-candidate dispatch fast path casts without checking the union's other members

**Status:** open, root-caused, not attempted. Rewritten 2026-09-28;
history in git:
`git show 3f36072b:ifa/issues/079-DISPATCH-single-candidate-dispatch-unchecked-cast.md`.

## The defect

`cg.cc:2690` (`directs.n == 1`), after the nil test and the classtag
compares, calls the one remaining candidate directly with
`(ft)(void*)arg`. Its comment reasons that *"the single untagged candidate
is everything the nil test / tag compares above didn't claim"*. That is
true only if every other member of the receiver's union either got a tag
branch or is nil. **A member with no implementation of the method at all
was never a candidate**, so it gets no branch, and the cast silently
reinterprets it. Found on `bh` (`b.hack_gravity(...)` with `b : Body |
Cell`, where `Cell` has no `hack_gravity`): a segfault. `bh` no longer
forms that union, but the fast path is unchanged. The multi-candidate
path already ends in `assert(!"…no branch matched")`, and this one has
no equivalent. The cast is also not recorded with the blind-cast layout
contract (`cg_note_blind_cast`), unlike the classtag branches above it
(`cg.cc:2659`).

## Principle

A union member with no candidate for the method is a dispatch that
CANNOT resolve for that member. That is a demand FA should observe
([102](102-corpus-programs-compile-then-abort-at-runtime.md)'s rule), not
something codegen should paper over:

- **In FA:** report it as a violation. Either the member is a union pyc
  invented (then the demand drives the split that removes it; AGENTS.md
  says such unions are pyc's bug), or it is genuinely reachable (then
  see below).
- **In codegen, permissive mode:** if the member really can arrive
  (branch correlation, [025](025-FA-intra-function-union-narrowing.md)),
  emit a tag test for `directs[0]`'s own class, with the final `else`
  raising `AttributeError` (CPython's behaviour), instead of the
  unconditional call. `--strict` refuses.

Compute "the receiver's union minus what the branches above covered"
from the same `plains`/`directs` construction codegen already has.
Do not re-derive it. This is the hottest dispatch path, so a monomorphic
receiver (the common case) must stay a direct call with no test.

## Verification

A fixture with `x : A | B`, `A.m` only, reached through a correlated
branch: permissive raises `AttributeError` only if the `B` path runs, and
strict refuses. Monomorphic calls emit byte-identical C (diff the corpus's
emitted C before and after). Six gates, plus a corpus `check` A/B.
