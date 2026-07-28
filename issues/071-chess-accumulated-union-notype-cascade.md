# 071 — chess.py: `squares` NOTYPE is a downstream salvage of accumulated union churn, not a setter-stage root

**Status:** open (chess still FAILs). Deeply root-caused 2026-07-28 via
delta-debugging the actual chess source; **three** contributing root
bugs found and **fixed** (`e544f6aa`, `4cfe9609`), one left open. With
the three fixed, chess's `squares` NOTYPE is gone and the failure has
advanced to a single, unrelated blocker (the issue-043 empty-list
element gap at chess.py:314). The overall shape is the issue-033
splitter-churn family, tipped over by an accumulation of small union
sources — no single "the bug".

**History correction:** the first draft of this file (same day, earlier)
hypothesised the root was a *setter-stage FA gap* — `range`'s
`__pyc_more__` method-pointer prototype-init slot resolving to NOTYPE
for `clone_methods_per_cs` classes, with codegen then emitting an
uninitialised-C-local field store. **That was wrong: the range collapse
is a downstream EFFECT, not the root.** The generated C that looked like
a fieldless, argless `range` (`__new__()` with no args, struct with only
`e5`) is what codegen emits *after* FA salvages the whole `squares`
subtree to void — because `squares` went NOTYPE for an entirely
different, upstream reason. The evidence below supersedes that draft.

**Affects (real roots):** `__pyc__/00_runtime.py` (bool ordering — now
fixed), `ifa/codegen/cg_emit_llvm.cc` (int(bool) sign-extension — now
fixed), `python_ifa_build_if1.cc` raise lowering (`raise <str>` — open),
the empty-container element-inference family (issue 043 — open), all
amplified by the issue-033 non-idempotent splitter.
**Surfaced while:** digging into `shedskin_examples/chess/chess.py`
(user request, following the
[chaos.py `== None` dig](../../issues/closed/031-eq-none-dispatch-crash.md)).

## Symptom

`chess.py` compiles with exactly one warning and exit 0:

```
shedskin_examples/chess/chess.py:26: warning: expression has no type
    squares = tuple([i for i in range(128) if not i & 8])
```

then the binary aborts on the first line of `main`
(`Assertion !"runtime error: matching function not found"`). `squares`
is a module-level global and the only surviving runtime use of `range`'s
class-based iterator protocol (every other `range(...)` in chess has
literal args and is const-folded/unrolled away).

## Why the generated C is a red herring

The crash site's C shows `range` with an argless `__new__()` and a
`struct {__pyc_tag; e5;}` (no `i/j/s` fields, no `__next__`), and
`___init___` storing an *uninitialised* local into the `__pyc_more__`
slot. This looks like a constructor/method-slot bug, but it is FA's
salvage of a NOTYPE subtree: once `squares` is NOTYPE,
`convert_NOTYPE_to_void` voids the whole list-comp-over-`range(128)`
expression, so the `range(128)` construction, its fields, and its
iterator methods all become dead and codegen emits the degenerate
shell. **`range` is the victim, not the cause.** Confirmed: the
identical `squares` line compiles and runs correctly in isolation and
in every small/medium reduction — `range` only collapses inside the
full program.

## Real root cause: accumulated union sources tip the issue-033 splitter

Delta-debugging the *chess source* (not the consumer of `squares`)
pinned three independent contributors, each of which alone is survivable
but which together push FA into the issue-033 dup-split churn (24 passes,
stuck ~38 violations across passes 7–17, then 18 dup_splits at pass 19)
that eventually salvages `squares`:

### (1) bool had no ordering dunders — FIXED (`e544f6aa`)

`nonpawnAttacks` does `max([board[ix+i] == color*2 for i in ...])` —
`max()` over a list of **bool**. `max`'s body does `x > m`, i.e.
`bool.__gt__`, which did not exist (bool defined only `__eq__`/`__ne__`).
The unresolved `__gt__` union is a churn seed. Minimal repro:
`max([v == 0 for v in [1,2,3]])` → `unresolved call '__gt__'`. Fixed by
adding `__lt__/__le__/__gt__/__ge__` to `bool` (int-subtype 0/1 ordering,
branch-on-self form, no numeric primitive). `tests/bool_ordering.py`.

### (2) int(bool) sign-extended to -1 on LLVM — FIXED (`e544f6aa`)

An early version of the bool dunders routed through `int(self) < int(x)`
and exposed this: the LLVM backend widened a bool with `SExtOrTrunc`, so
`int(True)` == **-1** (i1 `1` sign-extends to all-ones). Fixed by
zero-extending an i1 source in `emit_send_coerce`. `tests/bool_to_int.py`.
(The dunders were then rewritten to avoid `int()` entirely, but the
sign-extension bug is real and independently affects any `int(bool)`.)

### (3) `raise <str>` pollutes the `__pyc_exc__` slot — FIXED (`4cfe9609`)

The single decisive experiment: with (1) fixed, changing chess's
`raise "no move found"` / `raise "faulty castling"` (Python-2-style
string raises) to `raise ValueError(...)` **clears the `squares`
NOTYPE**. `PY_raise_stmt` moved the raised value straight into the
`__pyc_exc__` global slot; a `str` literal is not `Type_RECORD`, so it
was stored as-is, making `__pyc_exc__` a `{None, str, <exceptions...>}`
union that the program-wide `if __pyc_exc__ is not None` can-raise
checks then thread everywhere. In Python 3 `raise "str"` is itself a
TypeError; the fix wraps a raise operand whose static type is
string/bytes in `Exception(...)`, keeping the slot exception-typed
(`str(Exception("m")) == "m"` preserves the message, and the result is
catchable). `tests/raise_string.py`. Isolated `raise "str"` does *not*
repro the cascade — it needs the accumulation, consistent with the
churn model, which is why this was invisible until (1) was fixed and
the reduction narrowed to it.

### (4) empty-list element inference — OPEN (issue 043) — chess's remaining blocker

With (1) and (3) fixed, `squares` types cleanly and chess's *sole*
remaining failure is `legalMoves`'s
`[i for i in pseudoLegalCaptures(board2) if board2[i&0xff] == kingVal]`
(chess.py:314) — the classic `retval = []` filled-later element-type
gap ([043](closed/043-empty-container-inference-options.md) /
[063](063-no-type-bucket-triage.md)). This is the deep, known FA root
and is now the only thing between chess and a clean compile.

## What actually lands vs. what remains

- **Landed (`e544f6aa`):** bool ordering + LLVM int(bool) — both are
  genuine correctness bugs on their own (fix `True > False`,
  `max/min/sorted` over bools, `int(True)`), independent of chess.
- **Landed (`4cfe9609`):** `raise <str>` wrapping — chess's `squares`
  NOTYPE is now gone; the failure advanced to the single issue-043
  blocker at chess.py:314.
- **Open — chess's last blocker:** (4) the issue-043 empty-container
  element typing (`retval = []` filled later). Behind it, structurally,
  is the issue-033 splitter non-idempotency that turns any residual
  union into a program-wide NOTYPE cascade rather than a local,
  attributable error — the reason each of (1)/(3) individually looked
  scale-dependent and invisible until reduced.
- Suite 233/0 both backends after all three fixes; corpus sweep buckets
  within parallel-timeout noise (the three fixes touch only bool
  ordering, `int(bool)`, and `raise <str/bytes>` — no other example's
  behavior changes).

## Verification plan

1. `chess.py` compiles warning-free and
   `./shedskin_examples/chess/chess` runs to completion.
2. Regression for (3): a `clone_methods_per_cs`-adjacent global
   (`squares`-style range comprehension) plus a helper that does
   `raise "some string"`, in a non-`__main__` function — the shape that
   tips it (`raise <str>` in isolation does not).
3. Keep suite green both backends and re-check dijkstra2/fysphun per
   issue 063's checklist (the issue-033 canaries).

## What this unblocks

- `shedskin_examples/chess/chess.py` (a corpus benchmark).
- Correct `bool` ordering and `int(bool)` everywhere (already landed).
- More generally, making an accumulated-union NOTYPE degrade to a
  *local, attributable* diagnostic instead of salvaging an unrelated
  global's construction into an uninitialised-memory crash — the real
  robustness win hiding behind chess's confusing generated C.

## Related

- [063-no-type-bucket-triage.md](063-no-type-bucket-triage.md) — the
  multi-rooted NOTYPE bucket and setter/mark-stage churn.
- [033-splitter-non-idempotent-divergence.md](033-splitter-non-idempotent-divergence.md)
  — the non-idempotent splitting that amplifies these unions.
- [closed/043-empty-container-inference-options.md](closed/043-empty-container-inference-options.md)
  — chess's residual (4).
- [closed/045-receiver-cs-method-cloning.md](closed/045-receiver-cs-method-cloning.md)
  — `clone_methods_per_cs` (the earlier, wrong hypothesis's subject).
- [issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)
  — corpus tracker; chess entry updated alongside this.
