# 052 — Adding a branch to a shared clone_methods_per_cs method reopens issue 040's empty-list fragility

**Status:** open, found 2026-07-19 while fixing negative-index
support for `list.__getitem__` (see
[../../issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)'s
"Plain negative indexing fixed" entry). Not fixed here — worked
around at the codegen level instead (see that entry) once this was
found to be the same underlying class of issue as 040/043, not a new
one with its own fix.
**Affects:** whatever produces issue 040's fix (`ifa/analysis/fa.cc`'s
`clone_methods_per_cs`/per-constant-CS/`PER_CS_RECEIVER` machinery,
per [045](045-receiver-cs-method-cloning.md)) — this issue is a
*regression finder* for that machinery's actual scope, not a new
subsystem.
**Related:** [040](040-empty-list-shared-clone-type-inference.md)
(marked FIXED — this issue shows that fix's own verification repro
reopens with an unrelated, trivial change to the method it's about);
[043](043-empty-container-inference-options.md) (same family, "every
candidate repro checked... works today" — this is a candidate that
doesn't).

## Symptom

`tests/empty_list_print.py` (040's own committed regression test) is
exactly:

```python
b = [2, 3]
print(b)
k = []
print(k)
```

This compiles and runs clean today. Add **only** a no-op branch to
`list.__getitem__` (`__pyc__/04_sequence.py`) — nothing else in the
program changes:

```python
def __getitem__(self, key):
    if key < 0:
      pass
    return __pyc_primitive__(__pyc_symbol__("index_object"), self, __pyc_clone_constants__(key))
```

and the exact same program stops compiling:

```
__pyc__:662: expression has no type
  called from __pyc__:852
__pyc__:665: expression has no type
  called from __pyc__:852
__pyc__:852: expression has no type
  called from empty_list_print.py:11
  (x4)
fail: program does not type
```

(line 852 is `list.__str__`'s `x += self[k].__repr__()`; 662/665 are
inside the added `if`/`return`.) `empty_list_print.py:11` is
`print(k)` — the *empty* list's print, exactly 040's original shape.

## What's known

- Isolated by bisection, not guesswork: a plain `if key < 0: key =
  key + self.__len__()` fails the same way; a ternary form (`key =
  key + self.__len__() if key < 0 else key`) fails identically; even
  the no-op `if key < 0: pass` above — which changes nothing about
  what value `__getitem__` returns — is enough. It's specifically
  the **comparison** `key < 0` that breaks it, not the assignment,
  not the added call to `self.__len__()` alone (confirmed
  separately: `n = self.__len__()` with no comparison at all, added
  to the same method, compiles fine).
- `list` is `clone_methods_per_cs`-flagged (`__list_iter__.__init__`
  wraps its ctor param in `__pyc_clone_constants__`, per 040's own
  writeup and the comment at `__pyc__/04_sequence.py`'s
  `__list_iter__.__init__`) specifically *because* of 040's fix.
  This bug reproduces with that machinery active, on the exact
  program 040's fix was verified against — so whatever `key < 0`
  does to FA's handling of `__getitem__`'s empty-list-CS clone isn't
  covered by 040's fix, even though 040's fix is specifically about
  `list`'s per-CS cloning.
- Not investigated further than the bisection above — no fa.cc-level
  trace (union computation, CS splitting, EntrySet merging) was done
  for this specific shape. 040's own mechanism trace (its "Complete
  mechanism" section) is the closest existing map of this territory
  and is probably where to start.

## Repro

Add the no-op branch above to `list.__getitem__` in
`__pyc__/04_sequence.py` (don't commit it), then:

```
./pyc tests/empty_list_print.py
```

fails with the diagnostics shown above. Remove the branch and it
passes again. (Verified on 2026-07-19 against the code at commit
`c914ed12`.)

## Why this matters

`list.__getitem__` isn't an edge case — it's one of the most
frequently-modified shared methods in `__pyc__/`, and this session
alone hit it while adding an ordinary bounds-check. **Any future
change that adds a branch/comparison to a `clone_methods_per_cs`
class's shared method is at risk of silently breaking every program
that has both an empty and a non-empty instance of that class** —
which is an extremely common shape (e.g. `results = []` initialized
before a loop that might not run, alongside any other non-empty list
literal anywhere in the same program). The workaround used this
session (push the logic to codegen, where there's no FA-visible
branch at all) isn't available for logic that's inherently
Python-level (e.g. anything that needs to call another method
conditionally, not just compute an arithmetic expression).

## Verification plan

Whoever picks this up: `tests/empty_list_print.py` plus the no-op
`if key < 0: pass` repro above is the fastest reproduction — no
need to touch real negative-indexing logic to study this. A fix
should make `list.__getitem__` (and ideally any
`clone_methods_per_cs` class's shared method) tolerate an added
comparison on its own parameter without reopening 040, verified by
re-running that exact repro plus 040's own original (more complex)
shared-clone scenarios to confirm no new regression there.

## What this unblocks

Confidence that ordinary, everyday changes to `__pyc__/`'s
shared-container methods (bounds checks, added branches, anything
past pure arithmetic) won't silently reintroduce 040-class failures.
Without this, every future PR touching a `clone_methods_per_cs`
class's methods needs to be manually tested against an
empty+non-empty-instance program, which nothing currently prompts
anyone to remember to do.
