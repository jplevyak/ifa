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

## Investigation notes (June 2026)

A scoping investigation surfaced the right infrastructure
without committing a fix:

**Infrastructure that already exists** (`ifa/analysis/fa.cc`):
- `AVar::restrict` — an upper-bound type filter.
  `update_in(v, t)` re-computes `v->out = v->in ∩ v->restrict`
  every time `v->in` changes (line 243).  This is the
  knob narrowing would turn.
- `flow_var_type_permit(v, t)` (line 965) — the API to
  add a type to `v->restrict`.  Already used for:
  - prim_isinstance result narrowing the result AVar (the
    bool result, not the input!).
  - Constant folding.
  - prim_apply.
  - Splitter filtering at function boundaries.
- `Code_IF` (line 1987) — already detects when only one
  branch is reachable (condition AType doesn't include
  true/false).  Walks `p->phy` nodes that wire SSU-
  renamed Vars per branch (lvals[0]=True, lvals[1]=False).

**What's missing**:
- Logic at `Code_IF` that walks back from the condition
  Var to find its def PNode, recognizes narrowing
  predicates (isinstance, is None, ==, etc.), and calls
  `flow_var_type_permit` on the operand's per-branch view.
- The "per-branch view" itself.  In read-only branches
  (operand isn't reassigned), there is no per-branch SSU
  Var — the same AVar serves both branches.  Narrowing it
  would corrupt the False branch.

**Scope estimate (REVISED June 2026)**:

First scoping pass mis-claimed "in read-only branches, no
SSU rename exists for the operand."  That claim was
**wrong** — pyc's SSU DOES rename Vars per-branch even
when the operand isn't reassigned.  Verified with a
fixture (`ifa/tests/ir/ssu/14_isinstance_narrow.ir`):

```
(phy
  %p0:
    [0] lvals=[%v_v1 %v_v2] rvals=[%v]
)
(rename
  %v -> %v(def ?) %v_v1(def p0) %v_v2(def p0)
)
```

The IF node's phy creates `%v_v1` (True-branch view) and
`%v_v2` (False-branch view).  Each has its own `(Var,
contour)` AVar, so narrowing one doesn't corrupt the
other.

This makes the implementation much smaller than the first
scope estimate:

- Predicate recognition: ~50 LOC.  Walk back from the
  condition Var's def PNode, recognize prim_isinstance /
  prim_equal patterns, extract (operand, narrowing type).
- Per-branch narrowing application: ~30 LOC.  At the
  Code_IF True/False branch processing in fa.cc:1987, find
  the phy node whose rval is the operand and call
  `flow_var_type_permit` on the appropriate lval's AVar
  with the narrowing type.
- No new AVar mechanism needed.  No IF1 transformation
  needed.

Estimated 1-2 days, not weeks.  Tracked separately as the
implementation work for this issue.

## Implementation attempt (June 2026)

Landed: predicate-recognition + per-branch narrowing
infrastructure in `add_pnode_constraints` Code_IF
(commit follows).  Recognizes `prim_isinstance` direct
calls AND the Python `isinstance` wrapper SEND (by callee
sym name), computes per-branch narrowed AType, and applies
`flow_var_type_permit` on the per-branch SSU AVars.

**Found at implementation time**: a deeper design
constraint blocks observable narrowing on real Python
patterns:

- pyc emits BOXING violations on any AVar whose `out` has
  mixed basic types (`collect_var_type_violations` at
  fa.cc:2806).  This check runs on the **original Var** (e.g.
  `v`), not on the per-branch SSU views (`v_v1` / `v_v2`).
- Even with per-branch narrowing fully applied, the
  original `v`'s AVar still holds the union type
  {int, str}.  The BOXING check fires → compilation fails
  BEFORE downstream code can benefit from the narrowed
  per-branch types.

The narrowing CODE is correct; the structural problem is
that pyc's "no boxing by default" invariant rejects the
union before narrowing can route around it.

Two complementary fixes to make narrowing observable:

1. **Liveness-aware BOXING**: only emit BOXING violations
   on AVars actually USED downstream in a context that
   requires a concrete type.  If all uses of `v` are
   routed through phy nodes (v_v1, v_v2), `v` itself can
   carry the union type without violating anything.
2. **SSU rewrite-and-prune**: after FA converges, replace
   every use of `v` after the IF with the appropriate
   v_v1 / v_v2, then mark `v` as un-used.  This is a real
   IR transformation, not just a code-gen change.

Both are substantial follow-on work.  The narrowing code
landed here is the prerequisite for either approach — it
correctly computes and applies per-branch type filters; it
just doesn't fully unblock practical patterns until one of
the above lands.

Test impact: pyc suite stays at 85/0.  No new tests pass
because the BOXING gate still triggers.  No regressions.
