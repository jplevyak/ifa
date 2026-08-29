# 052 — Adding a branch to a shared clone_methods_per_cs method reopens issue 040's empty-list fragility

**Status:** **largely resolved 2026-08-29 — recommend closing as
superseded by [072](072-FA-empty-container-notype-current-mechanism-and-plan.md)**;
see the re-measurement at the end. The failure this issue is about — a
no-op branch in a shared method breaking an unrelated program — no longer
happens; what is left is the ordinary empty-container NOTYPE that 072
already owns and pins.

Previously: open (re-verified 2026-08-26), found 2026-07-19 while fixing negative-index
support for `list.__getitem__` (see
[../../issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)'s
"Plain negative indexing fixed" entry). Not fixed here — worked
around at the codegen level instead (see that entry) once this was
found to be the same underlying class of issue as 040/043, not a new
one with its own fix.
**Affects:** whatever produces issue 040's fix (`ifa/analysis/fa.cc`'s
`clone_methods_per_cs`/per-constant-CS/`PER_CS_RECEIVER` machinery,
per [045](closed/045-receiver-cs-method-cloning.md)) — this issue is a
*regression finder* for that machinery's actual scope, not a new
subsystem.
**Related:** [040](closed/040-empty-list-shared-clone-type-inference.md)
(marked FIXED — this issue shows that fix's own verification repro
reopens with an unrelated, trivial change to the method it's about);
[043](closed/043-empty-container-inference-options.md) (same family, "every
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


## Re-verified 2026-08-26 — still open, and the diagnosis above is wrong

Still reproduces, but its severity has dropped and **three of the
claims in this issue are measurably false**. Anyone picking this up
from the text above would study 040's per-CS machinery, which this
program never even runs.

### Severity today

    default (permissive)   compiles rc=0, 2 NOTYPE *warnings*,
                           and the program prints the RIGHT answer:
                           [2, 3] then []
    --strict               fail: program does not type, rc=1

When filed it was a hard `fail: program does not type` in both. The
permissive salvage now carries it, so this is a strict-mode-only defect.

### Wrong claim 1: it is not "adding a branch"

There is no branch. This alone reproduces it:

```python
def __getitem__(self, key):
    x = key < 0                     # no `if`, result unused
    return __pyc_primitive__(...)
```

### Wrong claim 2: it is not comparisons, it is ONE expression

Every one of these is CLEAN. Only `key < 0` fails:

| fails | clean |
|---|---|
| `key < 0` | `key < 1`, `key < 100`, `key < -1`, `key < -5` |
| `z = 0; key < z` | `key <= 0`, `key <= -1`, `key > 0`, `key > -1` |
| | `key >= 0`, `key == 0`, `key == 999`, `key != 999` |
| | `key > 1000`, `0 < key`, `key + 0`, `if key:` |
| | `if True:`, `n = self.__len__()`, `if self.__len__() < 0:` |

So it is not "a comparison", not "a comparison against 0" (`key == 0`,
`key > 0`, `key <= 0` are all fine), not "a uniformly-false comparison"
(`key < -1`, `key > 1000`, `key == 999` are all uniformly false for the
actual index set {0,1} and all fine), and not the negation
(`key >= 0` is fine). It is exactly the pair (`<`, `0`).

Note this rules out constant-folding of the comparison as the mechanism:
FA types a comparison as the abstract `bool_type`
(`ret_types[i] == PRIM_TYPE_BOOL`, fa.cc), never as a folded True/False,
so `key < 0` and `key < -1` are indistinguishable at that level.

### Wrong claim 3: 040's fix stage is not involved

`PYC_DBG_STAGES=1` reports `STAGES: TYPE_CONFL` for **both** the passing
baseline and the failing variant. `PER_CS_RECEIVER` -- the stage
[045](closed/045-receiver-cs-method-cloning.md) added as 040's fix, and
which this issue names as the affected machinery -- **never fires on
this program at all**, in either direction. Lifting its quiescence gate
(`PYC_RECVFAN=2` and `=3`) does not change the outcome either.

Whatever makes the baseline pass, it is not 040's fix.

### What else was measured

- **Both lists are required**, order irrelevant: `k = []` alone is
  clean, `b = [2,3]` alone is clean, either order of both fails.
- **Deterministic**, 3 runs each way -- not ifa/issues/112.
- **The NOTYPE events are the same in the PASSING baseline.** With
  `PYC_DBG_NOTYPE=1` the clean baseline emits 36 NOTYPE events in the
  same two functions (`list.__str__`, `list.__getitem__`) that the
  failing variant emits 48 in. So nothing new goes wrong; the same
  NOTYPEs simply stop being *resolved* by later splitting.
- **Not contour sharing.** The empty list has its own `__getitem__`
  contour: at the violation the receiver dumps as one CreationSet,
  `sym=list vars.n=0` (the element-less one), `out.sorted.n=1`.
- **One extra EntrySet.** ess = 70 baseline, 71 for `key < -1` (clean),
  72 for `key < 0` (fails). Both add contours; only one fails.

### Is it a straightforward fix?

**No.** There is no localized special case to correct: the trigger is a
single (operator, constant) pair that produces one extra EntrySet, and
the failure is that TYPE_CONFLUENCE's splitting trajectory stops
resolving NOTYPEs it resolves in the baseline. That is the same splitter
machinery tracked by [074](074-FA-cross-pass-oscillation-plan.md),
[111](111-FA-selective-invalidation-per-pass.md) and
[113](113-FA-setter-equivalence-is-a-global-batch-partition.md).

Next step for whoever takes it: a per-pass trace of *which* NOTYPE
violations `split_for_violations` clears and which it does not, baseline
vs `key < 0`, rather than any further source-level bisection -- the
source-level bisection is exhausted and is recorded in full above.


## Re-measured 2026-08-29: the fragility is gone

Re-ran this issue's own repro verbatim — `if key < 0: pass` added to
`list.__getitem__` in `__pyc__/04_sequence.py`, then
`./pyc tests/empty_list_print.py`:

| | 2026-08-26 | 2026-08-29 |
|---|---|---|
| exit | `fail: program does not type` | **rc=0** |
| binary | none | produced |
| output | — | `[2, 3]` / `[]`, correct |
| diagnostics | 3 sites (`__pyc__` 662, 665, 852) + fail | **2 warnings**, both at `print(k)` |

So the thing that made this worth filing — *"any future change that adds a
branch/comparison to a `clone_methods_per_cs` class's shared method is at
risk of silently breaking every program that has both an empty and a
non-empty instance"* — does not reproduce. The internal `__pyc__` sites
are gone entirely; the shared method can carry a branch.

**What remains is 072, not this.** The two surviving warnings are
`expression has no type` on `print(k)` where `k = []`: `list.__str__`
reaches `self[k].__repr__()` on an element of a provably-empty container,
which is exactly the residual
[072](072-FA-empty-container-notype-current-mechanism-and-plan.md)
describes and pins with `tests/empty_container_elem.py`. Nothing about it
is specific to shared methods, `clone_methods_per_cs`, or the added
branch — the same two warnings appear with `__getitem__` untouched.

**Not attributed to a specific change**, and the obvious candidates were
tested and are not it: `PYC_CSMOLD` (0/1/3), `PYC_PROMOTE_FIRST` (0/1/2)
and the 050-stage-1 global-load fold all give the identical result, and
breaking that fold's precondition (a second store to `k` from another
function) does not bring the failure back. It resolved somewhere in the
2026-08-26..29 FA/codegen work without a flag to bisect on.

**Recommendation: close as superseded by 072.** The regression-finder role
this issue was filed for is served by re-running the repro above; if a
shared-method branch ever breaks an unrelated program again, reopen from
`closed/`.
