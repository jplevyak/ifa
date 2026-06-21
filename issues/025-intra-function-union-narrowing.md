# Issue 025: IFA doesn't narrow runtime union types intra-function on conditional branches

**Status:** open.
**Affects:** `ifa/analysis/fa.cc` (splitter), the FA-level
EntrySet/AVar specialization machinery.
**Related:** [024-is-comparison-narrowing.md](024-is-comparison-narrowing.md)
(specific to `is`/`is not`) — this issue is the broader
underlying problem.

## Symptom

Pyc IFA narrows union types in exactly one situation: when
the union arises from cross-function polymorphism and the
narrowing predicate is `isinstance`.  In that case, the
"narrowing" is actually clone-time specialization: pyc
produces one Fun per type signature at the call boundary,
and inside each clone the formal has a single concrete
type — no real narrowing happens.

For any union that's genuinely intra-function (created by
phi-merging two branches, or by reading a polymorphic
field), pyc fails to narrow:

### Case 1 — phi-merged union, then re-discriminated

```python
def f(flag):
  if flag:
    x = 5
  else:
    x = "hi"
  # x is int|str via phi merge
  if flag:
    return x + 10           # narrowing needed: x is int here
  else:
    return x + " world"     # narrowing needed: x is str here
```

Fails:
```
warning: 'x' has mixed basic types:( int64 str )
```

The second `if flag:` correlates perfectly with the first
(SSU preserves `flag`'s identity through the merge), so
IFA *could* deduce x's branch-narrowed type.  It doesn't.

### Case 2 — isinstance on a runtime union

```python
def maybe(b):
  if b: return 5
  return "hi"

def use(b):
  v = maybe(b)   # v is int|str at runtime
  if isinstance(v, int):
    return v + 10
  return v + " world"
```

Fails identically.  The isinstance predicate is exactly the
type-discriminator IFA's splitter knows how to use — but it
only works when the union is at the function boundary (so
cloning can split it), not when the union is internal.

### Case 3 — `==` against a discriminating value

```python
def f(flag):
  if flag == 0:
    return "zero"
  return flag + 10
```

`flag` has a single type (int) but pyc still emits "mixed
basic types" because the function's return type is
int|str.  Narrowing the return type per branch would let
the call site work, but pyc's per-branch return-type
inference doesn't kick in here.

## Root cause

Pyc's "narrowing" is really clone-time specialization.  At
the FA level, an EntrySet is keyed by the call-site's
argument types (see [CLONE.md](../CLONE.md)).  When a fn is
called with arg type A, pyc clones for A; with B, clones
for B.  Each clone sees a non-union formal.

This works for the *cross-function* case but produces no
narrowing for *intra-function* unions, because:

1. **Phi-merged unions** exist inside a single EntrySet —
   no cloning can split them.
2. **isinstance on a runtime union** would need IFA to
   create two synthetic per-branch sub-contexts based on
   the predicate outcome.  pyc's splitter doesn't do this
   for intra-function code.
3. **`==`/`is` against a constant** would need similar
   per-branch sub-context creation, plus correlation
   tracking (the second `if flag:` test correlates with
   the first).

The mechanism for branch-conditional type refinement —
common in flow-sensitive type analyses — isn't part of
pyc's IFA today.

## What SSU does and doesn't help with

The user's observation: "SSU form produces different
values on different conditional branches even in read-only
situations."

Partially true: SSU's phi/phy mechanism creates new Var
identities at *merge points* and at *write sites*.  But in
read-only branches (no rewrite of the variable), SSU does
NOT introduce new Vars per branch — the same Var flows
into both branches.  So SSU alone doesn't drive
per-branch narrowing.

The narrowing would need to happen at the FA level:
"in the True branch of `isinstance(v, int)`, treat v's
EntrySet-AVar as restricted to int."  This is per-branch
type refinement, parallel to but distinct from SSU's
per-write Var creation.

## Proposed fix sketches

### Option A — per-branch AVar refinement

At each conditional branch entry, if the predicate is
a recognized type-discriminator (isinstance, is None,
`type(x) == T`, `x is constant`, etc.), introduce a
refined AVar for the discriminated operand on each branch.
The refined AVar has the narrowed type filter; uses inside
the branch see the refined AVar instead of the original.

Implementation surface:
- `fa.cc` branch handling — recognize discriminator prims
  on the condition.
- AVar refinement at branch entry (similar to phi/phy
  insertion but conditional on predicate truth).
- Merge points: the original AVar resumes (or a phi between
  the two refined AVars produces the merged type).

This is non-trivial — pyc IFA didn't ship with flow-
sensitive refinement and adding it touches the splitter,
the AEdge worklist, and the dispatch resolution.

### Option B — frontend rewrite to clone-eligible shapes

Before FA runs, hoist the narrowing to a function boundary:

```python
# Original
def f(flag):
  if flag: x = 5
  else: x = "hi"
  if flag: return x + 10
  return x + " world"

# Rewritten by frontend:
def f_true(): x = 5; return x + 10
def f_false(): x = "hi"; return x + " world"
def f(flag):
  if flag: return f_true()
  return f_false()
```

Now FA's clone-time specialization works.  But this is a
substantial frontend transformation that doesn't compose
well with arbitrary control flow.

### Option C — accept the limit and document

Document that pyc's IFA requires "cross-function or
isinstance-at-call-site" type discrimination.  Users
restructure their code to put type-distinct operations
in separate functions.  This is what pyc effectively
forces today; making the limit explicit at least gives
users a path forward.

## Verification plan

When fixing:

1. The phi-merged flag pattern (Case 1) compiles and
   returns the right values for both `f(True)` and `f(False)`.
2. The isinstance-on-runtime-union pattern (Case 2)
   compiles and returns the right values.
3. The recursive-linked-list pattern from
   [issues/004](../../issues/004-is-operator-unimplemented.md#whats-still-broken)
   compiles (once issue 024 also lands).
4. Pyc suite stays at 85/0 on both backends.
5. Add a `tests/conditional_narrowing.py` exercising each
   of the three cases.

## What this unblocks

- Real recursive data structures (linked lists, trees,
  fib heaps) via `is None` narrowing (composes with 024).
- Python ports that use `isinstance` to discriminate
  inside function bodies.
- More idiomatic Python in pyc — discriminator patterns
  inside functions are very common.

## Related

- [`ifa/IFA.md`](../IFA.md) §6 — setter-splitting (the
  existing per-EntrySet machinery; narrowing would extend
  this with per-branch refinement).
- [`ifa/issues/024-is-comparison-narrowing.md`](024-is-comparison-narrowing.md)
  — narrower scope (is/is_not).
- [`ifa/CLONE.md`](../CLONE.md) — current clone-time
  specialization.
- `prim_isinstance` (`if1/prim_data.h:41`) — the
  discriminator the splitter already recognizes but only
  at call boundaries.
