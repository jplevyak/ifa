# 072 — Empty-container "no type" (the 043 family): current mechanism + tiered fix plan

**Status:** planning (2026-07-28). Re-diagnoses the
[043](closed/043-empty-container-inference-options.md) family against
the *current* tree (post-[045](closed/045-receiver-cs-method-cloning.md))
and lays out a concrete, tiered implementation plan. 043 is closed with
a partly-stale characterization (its option-4 prototype "had nothing to
do"; 045 landed afterward) — this issue supersedes its **plan**, not its
option survey.
**Affects:** `__pyc__/00_runtime.py` / `04_sequence.py` (container
truthiness methods) for Tier 0; `ifa/analysis/fa.cc`'s
`clone_methods_per_cs` / `PER_CS_RECEIVER` machinery for Tier 1; the
type-world (`type_union`, clone, codegen) for Tier 2.
**Related:** [040](closed/040-empty-list-shared-clone-type-inference.md),
[052](052-shared-method-branch-reopens-empty-list-fragility.md) (the
shared-method-branch fragility — this issue's core mechanism),
[043](closed/043-empty-container-inference-options.md) (option survey),
[063](063-no-type-bucket-triage.md) (the corpus "no type" bucket),
[018](../../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-polymorphic-dispatch-fat-pointers.md) (heterogeneous boxing —
Tier 2's co-blocker), [071](071-chess-accumulated-union-notype-cascade.md)
(chess, whose line-314 warning is this family's cheap end).

## The mechanism, re-traced 2026-07-28 (grounded, current)

The 040/052 root, restated precisely from a fresh bisection:

> **A branch/comparison inside a *shared* method, dispatched over a
> container CreationSet that may be empty (bottom element type in some
> contour), fails to type** — the empty and non-empty CSs merge in the
> shared method's receiver formal and the branch body is analyzed
> against the merged/bottom state, producing NOTYPE.

045's `clone_methods_per_cs` gives per-receiver-CS contours to methods
defined **on** the flagged container (list / range / `__list_iter__`),
so the empty-CS clone's branch folds independently — that is why
`if some_list:` (→ `list.__pyc_to_bool__`, list's own, which list
overrides) and `some_list == other` (→ `list.__eq__`, list's own) both
compile fine over a possibly-empty list today.

**The gap:** *inherited* methods on `object` / `__pyc_any_type__` are NOT
covered — `object` is not `clone_methods_per_cs`, so an inherited method
dispatched over a container is a single shared clone across every
container CS (empty and non-empty merged). If that inherited method
contains a branch, it reopens 040.

### Witness (chess.py:314, minimal)

`not <list>` lowers to `list.__not__()`; `list` has no `__not__`, so it
dispatches the inherited `object.__not__`:

```python
def __not__(self):          # __pyc__/00_runtime.py, on `object`
  if self.__pyc_to_bool__():  #   <-- a branch, in a shared non-per-CS method
    return False
  return True
```

Over a possibly-empty list (a comprehension result whose element is
bottom in the empty contour) this shared `if` NOTYPEs. Minimal repro
(`/tmp` during the dig, reproducible):

```python
def caps(b):
    r = []
    if b: r.append(1)
    return r
def outer(b):
    c = [i for i in caps(b)]
    if not c:        # NOTYPE here; `if c:` and `len(c)==0` are both clean
        return 0
    return 1
```

Adding a container-owned `__not__` (`return self.__len__() == 0`) makes
`not c` dispatch list's own (per-CS) method and **fixes it** (verified,
both this repro and the fuller chess-line-314 shape). Confirmed: none of
`list/tuple/dict/set/str/bytes` define `__not__` today — all six inherit
the fragile shared `object.__not__`; and `if c:` works only because they
each override `__pyc_to_bool__`.

## Difficulty spectrum — this family is not one bug

| Manifestation | Example | Tier |
|---|---|---|
| `not <container>` (inherited branchy `object.__not__`) | chess.py:314 | 0 |
| any inherited branchy shared method over an empty container | 052's `list.__getitem__` `key<0` no-op-branch repro | 1 |
| empty-field element back-propagation (`self.x=[]` filled later, then `self.x[i]`) | chull, rubik `struc[x][y]` | 2 |
| heterogeneous-union element (boxing) | amaze `(tuple None int64 float64 str)`, dijkstra2 | 2 (+ issue 018/030) |

Tier-0/1 fixes do **not** touch amaze/rubik/dijkstra2 — those are the
deep element-typing + boxing end and need Tier 2 (and issue 018/030).

## Plan

### Tier 0 — container-owned `__not__` (cheap, targeted, semantically exact)

Add `__not__` (`return self.__len__() == 0`) to `list`, `tuple`, `dict`,
`set`, `str`, `bytes`. Each is (or is on the same footing as) a per-CS
class, so its own method's branch folds per receiver CS. Audit the other
inherited `object`/`__pyc_any_type__` methods a container can dispatch —
they already override `__pyc_to_bool__`, `__str__`, `__eq__`, `__len__`;
`__repr__`/`__format__` route to `__str__`, `__deepcopy__` has a
fallback — so `__not__` is the one live gap, but confirm by grepping for
branchy `object` methods without a container override.

- **Effort:** ~1–2 h. **Risk:** low, but must run the full suite (both
  backends) *and* the determinism gate + corpus sweep — adding methods
  to per-CS container classes can perturb splitter trajectories (025
  round-3 fragility; issue 033).
- **Unblocks:** chess.py:314 (chess still needs its *other* blocker, the
  bool|None representation — [071](071-chess-accumulated-union-notype-cascade.md));
  any `if not <container>:` over a possibly-empty container in the
  corpus.
- **Verify:** the `not c` repro above; `b=[2,3];print(b);k=[];print(k)`
  (040); the 052 no-op-`if key<0:pass` repro; suite 233/0 both backends;
  determinism + sweep buckets stable.

### Tier 1 — general 052 fix: per-CS contours for *inherited* methods over flagged containers (the real root fix)

Extend the `PER_CS_RECEIVER` machinery so that a `clone_methods_per_cs`
container's dispatch of an **inherited** shared method (`object.__not__`,
`__pyc_any_type__.*`) also gets per-receiver-CS contours — today the
stage splits only the container's *own* methods. Then the empty-CS clone
of *any* branchy shared method folds independently, and Tier 0's hand
overrides become unnecessary. Prerequisite: the FA trace 052 explicitly
left open — *why* a branch over an empty-element CS NOTYPEs inside a
merged contour (union computation / CS-split / EntrySet-merge for that
exact shape) — needed to target the split precisely.

- **Effort:** multi-day (FA splitter; the 045/`PER_CS_RECEIVER`
  extension). **Risk:** medium-high — issue-033 splitter churn; re-check
  the dijkstra2/fysphun canaries per 063's checklist.
- **Unblocks:** the 052 fragility class program-wide (every future
  branchy change to a shared/`__pyc__` container method), and removes the
  Tier-0 maintenance burden.

### Tier 2 — empty-container absorption + heterogeneous boxing (deep, corpus-wide)

043's **option 2**: an `empty-<class>` lattice element that ABSORBS on
union (`empty_list ∪ list[int] = list[int]`) with a post-convergence
default (`empty → <class>[nil]`); monotone, so stable inside the
fixpoint. Fixes field-element back-propagation (chull, rubik). **Plus**
[018](../../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-polymorphic-dispatch-fat-pointers.md) heterogeneous boxing for
amaze's `(tuple None int64 float64 str)` element — a co-occurring, distinct
blocker (tagged dispatch / fat pointers), not solved by absorption alone.

- **Effort:** weeks; a type-world change (`type_union`/canonicalization,
  clone equivalence, codegen must know the new CS kind). **Risk:** high.
  043's numeric analog needed the annotate-and-re-run architecture after
  three shortcut attempts — budget accordingly; share the absorption
  machinery with the untyped-numeric-constant work.
- **Unblocks:** amaze, rubik, othello2, dijkstra2 — the bulk of 063's
  "no type" bucket.

### Orthogonal, do-regardless — option 1's codegen half

Independent of all tiers: make a residual no-type / dead branch emit a
runtime **trap** (assert) instead of raw un-compilable C (undeclared
label, `(null)*` element, `expected expression`). Turns every remaining
no-type branch in the corpus from a *build* failure into a *running*
program with a precise trap (the 056/063 convention). Highest-leverage
honest mitigation for the whole family; already partly landed in 063.

## Recommendation

Tier 0 now — bounded, correct, unblocks chess's 043 manifestation and a
class of `if not <container>:` corpus sites. Tier 1 as the durable 052
fix when someone is inside the FA splitter. Tier 2 as the long-term
corpus lever, filed jointly with issue 018/030 (they must land together
for amaze/dijkstra2). Option 1's codegen trap is worth doing on its own
schedule.

## Verification targets (cumulative)

1. Tier 0: the `not c` repro, 040's `empty_list_print.py`, 052's no-op
   branch repro — all clean; suite 233/0 both backends; determinism +
   sweep stable.
2. Tier 1: 052's repro tolerant of an *arbitrary* added branch in any
   shared container method; dijkstra2/fysphun unchanged.
3. Tier 2: amaze / rubik / othello2 past their element/boxing walls;
   dijkstra2 past its dict/heap wall; no suite regressions.
