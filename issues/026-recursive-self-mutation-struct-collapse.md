# Issue 026: recursive types with >1 self-typed field lose value field in C struct

**Status:** **two fixes landed June 2026, one bug
diagnosed.**
(1) Prototype-vs-instance allocation size mismatch fixed
via `_CG_prim_clone_dst`.  (2) Struct field-index holes
fixed by elision + indexing (live fields keep their
has-index, dead-field setters are elided).  DLL works.
Manual tree builds work on the C backend (v2 LLVM
blocked by issue 027).  BST `insert` still hits a
*third* bug — when a Node is created inside a function
and assigned to another Node's field, the inside-function
Node's CS doesn't get its own field-iv tracking
established.  Diagnosed but not fixed (needs deeper IFA
investigation — see "Third bug" below).
**Affects:** pyc IFA cloning / struct synthesis when a
class has two or more fields of its own type alongside
non-recursive fields.
**Surfaced while:** writing graduated tests after the
`is None` fix (issue 024).  Linked list patterns work
(one Node-typed field); BST / tree patterns don't (two
Node-typed fields).

## Symptom

A BST `insert` function:

```python
class Node:
  def __init__(self, v):
    self.value = v
    self.left = None
    self.right = None

def insert(root, v):
  if root is None:
    return Node(v)
  if v < root.value:
    root.left = insert(root.left, v)
  else:
    root.right = insert(root.right, v)
  return root

def sum_tree(root):
  if root is None: return 0
  return root.value + sum_tree(root.left) + sum_tree(root.right)

t = Node(5)
t = insert(t, 3)
t = insert(t, 7)
print(sum_tree(t))   # expected 15
```

Pyc compiles cleanly and runs but returns **30** instead
of **15** (or 29 for a larger 6-node tree expecting 29).
The reason: the generated C struct for Node is:

```c
struct _CG_s3548 {
  _CG_ps3548 e2; /* left */
  _CG_ps3548 e3; /* right */
};
```

The `value` field is **missing**.  All reads of `root.value`
return garbage from the next byte after the two pointers,
producing wrong sums.

The body of `insert` also shows IFA constant-folded
`v < root.value` to `v < 5` — using the literal value
from the first Node's construction.  So even if the struct
had `value`, the comparison wouldn't have read it.

## Why this seems to be happening

A guess from one trace:

1. IFA clones `insert` per (root_type, v_type).  In the
   primary clone for the entry call `insert(t, 3)`, `t`
   has CS with `value=5` (constant).  Inside the clone,
   `root.value` is constant-folded to 5; `v < root.value`
   becomes `v < 5`.
2. The recursive call `insert(root.left, v)` passes a
   `root.left` whose CS the analyzer treats as a *new*
   creation with a fresh `value`.  But the cloning
   collapses these into the same Fun because the
   call-graph signature ends up identical.
3. Once the clone has only one CS for Node and the
   `value` access was constant-folded, the field can be
   dropped from the struct layout — codegen sees no
   *non-constant* read.

The pure-build linked-list case (`recursive_list_is_none.py`)
doesn't trigger this because there's no recursive
mutation of the field — `head.next = next_node` is a
straight assignment from an outer scope, never inside a
function that takes the type as both arg and return.

## Reproducers

Four variants, all of which differ in how they exercise
the recursive type:

1. **BST insert** (the original — above).  Compiles wrong;
   returns 30 not 29.

2. **Single-node BST sum_tree**:
   ```python
   t = Node(5)   # left/right stay None
   print(sum_tree(t))
   ```
   Compile error: `invalid conversion from 'nil_type' to
   'Node*'`.  The `.left` field's type collapses to
   nil_type-only (because no Node was ever assigned), but
   sum_tree's signature expects Node*.

3. **Two-node BST built via one insert**:
   Same compile error as (2) — likely the .right field
   stays purely None in the produced clone.

4. **Manually-built tree (no recursive insert)**:
   ```python
   root = Node(5)
   root.left = Node(3)
   root.right = Node(7)
   ...
   print(sum_tree(root))   # returns garbage
   ```
   Compiles cleanly but returns nonsense.  The generated
   struct is:
   ```c
   struct _CG_s3536 {
     _CG_ps3536 e2; /* left */;
     _CG_ps3536 e3; /* right */;
   };
   ```
   — no `value` field.  But `Node::__init__` writes
   `((_CG_ps3536)t0)->e1 = t1;` (the value).  That write
   goes into undefined memory and corrupts the heap.

   This is the most damning variant: the recursive
   function never mutates the type's fields; the structure
   is built entirely in the outer scope.  The bug fires
   anyway.  The trigger appears to be **a class with two
   or more fields of its own type plus a non-recursive
   field**, not the recursive-mutation pattern originally
   suspected.

The linked-list case
(`recursive_list_is_none.py`, `linked_list_ops.py`) works
because Node has only ONE field of its own type (`next`).
A Node with `next: Node|None` keeps the value field.  Add
a second self-typed field (`left` and `right`) and the
value field disappears.

## Workarounds

- Build the structure entirely via attribute assignment in
  the outer scope (never inside a function that takes the
  recursive type as arg and return).  This is what
  `recursive_list_is_none.py` does and why it works.
- Use only iterative tree building (not recursive insert).
- Don't read the value field in a way that requires a
  consistent layout across clones.

## What's needed for a real fix

Two pieces are likely involved:

1. **Cloning policy**: when a recursive function reads a
   field of the recursive type, the field must be present
   in the struct for ALL clones that read it.  Pyc's
   per-EntrySet field-pruning is too aggressive when one
   clone's value is a constant.

2. **Struct layout synthesis**: when multiple
   CreationSets are merged into one struct (one Node-class
   type), the merged struct must include every field that
   any contributing CS uses.  Currently it appears to
   prune based on a single contributing context.

Both touch the FA → clone → codegen pipeline at structural
points.  Estimated several days of focused work; not
session-scale.

## What this blocks

- Binary search trees with recursive insert.
- Doubly-linked lists where the recursive function mutates
  prev/next pointers.
- Fibonacci heaps and similar self-mutating recursive
  structures (the original motivating use case for the
  graduated test series).

The linked-list test (`tests/recursive_list_is_none.py`)
still works because it builds the structure outside any
recursive function.  Patterns where the field is only
written outside the recursion compile cleanly.

## Fix landed (June 2026) — prototype-vs-instance size

Root cause traced.  Pyc creates two CreationSets for the
same class:

1. **The prototype CS** (from `prim_new(cls)` in
   `gen_class_pyda`).  Used as the source for `prim_clone`
   when constructing new instances.  This CS has **no
   field writes** because nothing assigns to the proto's
   fields (the class-level `___init___` only sets defaults
   when the class has class-scope attributes like
   `x = 2` — which our test classes don't).
2. **The instance CS** (from `prim_clone(proto)` in
   `__new__`).  This CS gets field writes from `__init__`
   and any caller-side assignments.

Both CSs have the same `sym` (the class's instance type),
but pyc's per-CS struct synthesis produces **different
struct layouts** for them:

- Proto CS → 0-byte struct (no fields).
- Instance CS → struct with all written fields.

The runtime macro `_CG_prim_clone(src)` used to allocate
`sizeof(*src)` bytes, copying from the source.  With
`src` being the proto (0 bytes), only 0 bytes were
allocated.  Subsequent `__init__` writes to
`obj->value`, `obj->left`, `obj->right` corrupted the
heap.

The fix (`pyc_c_runtime.h` + `cg.cc:write_prim_send`):

- New runtime helper `_CG_prim_primitive_clone_dst(src,
  dst_size, src_size)` that allocates by `dst_size` and
  memcpy's only `min(src_size, dst_size)` bytes.
- New macro `_CG_prim_clone_dst(_dt, _src)` that passes
  both source and destination types so the macro can
  compute both sizes.
- Codegen emits `_CG_prim_clone_dst(DST_TYPE, src)`
  instead of `_CG_prim_clone(src)` for `P_prim_clone`.
- `__init__`'s writes now have room.  GC_MALLOC zeros
  the unused tail, so fields not in the source are still
  initialized cleanly.

**Result**: `tests/tree_manual.py` (manually-built tree
with recursive sum_tree and depth) compiles and runs
correctly on the C backend.  v2 LLVM still has issue 027.

## Second fix landed (June 2026) — struct-field index holes

Root-caused the DLL and basic-BST struct-collapse issue:

The struct codegen in `cg.cc:write_record` was skipping
fields whose `Var::live` was false, but it kept the
original `eN` index numbering (`fprintf(fp, " e%d;", i)`
where `i` is the index in `s->has`).  When a dead field
sat at index 2 between two live fields (indices 1 and 3),
the struct went `... e1; e3; ...` — with **no e2**.

The setter / getter codegen elsewhere (`cg.cc:write_send`
prim_setter case, prim_period case) matched field names
to `obj->has[i]` and emitted `obj->eN` using `i` directly.
So a write to `n2.prev` (where prev was at has[2]) emitted
`obj->e2 = head;` — referencing the hole in the struct.

Dead-field elision happens because pyc's liveness
analysis only marks a field's Var live if it's *read*
downstream.  Pure-write fields stay dead.  In `n2.prev =
head`, the prev field is written but never read, so it
stays dead → elided from the struct → codegen for the
write breaks.

**Fix** (final form): centralize the live/dead decision
in a shared `cg_field_live(s, i)` helper.  Struct emission
now uses it to keep the has-index `i` as the `eN` suffix
*for live fields only* (dead fields are skipped — the
struct can have gaps in eN like `e1; e3;`).  Setter
codegen also uses it to **elide writes to dead fields
entirely** — no `obj->eN = ...` line is emitted when has[i]
is dead.  The struct and the field-access codegen stay
consistent: every `eN` that the codegen references exists
in the struct.

The getter (`prim_period`) doesn't need an explicit live
check because dead-field reads can't occur — a read of
field X makes X's Var live (via the dataflow propagation),
so getters always see live fields by construction.

Verified:
- `tests/doubly_linked_list.py` (new): `head`/`n2`/`n3`
  with prev+next, recursive sum_forward — works on both
  backends, output `6\n5\n3`.  Generated DLL struct shows
  `e1; e3;` (no e2 for prev), and the `n2.prev = head`
  setter is fully elided from the C output.
- Manual tree (`tests/tree_manual.py` not added because
  v2 LLVM has a separate GEP-on-ptr issue tracked in
  [issue 027](027-v2-llvm-narrowed-loop-loses-struct-type.md))
  works on the C backend.
- Linked-list tests, recursive_alloc tests, suite
  remained 88/0.

## Third bug — Node-in-function loses iv tracking

A *separate* bug still blocks BST insert (and any pattern
that creates Nodes inside a function and reads their
fields through dispatch later).

Minimal reproducer:

```python
class Node:
  def __init__(self, v):
    self.value = v
    self.next = None

def set_next(node, v):
  node.next = Node(v)         # ← Node created inside function

def sum_list(n):
  if n is None: return 0
  return n.value + sum_list(n.next)

t = Node(5)
set_next(t, 3)
print(sum_list(t))   # expected 8, returns 10
```

Generated `sum_list` body:

```c
_CG_int64 sum_list(_CG_any a1) {
  ...
  t3 = sum_list(t->e2);      // recurse on .next
  t2 = _CG_prim_add(5, "+", t3);   // ← literal 5, not n.value
  return t2;
}
```

The `n.value` field read is fully *elided* and replaced
with the constant `5`.  There's no read of `e1` from
memory.  Pyc IFA decided that the value field across all
contexts reaching this function is `{5}` only — it never
saw the value=3 of the Node created inside set_next.

The C output also shows two Node struct types:
- `ps3449` (Node created outer-scope): `e2; /* next */`
  pointing to ps3452.
- `ps3452` (Node created inside set_next): **empty**
  struct — no fields.

So the inside-function Node CS exists at the type level
(distinct struct) but doesn't track field reads/writes.
When sum_list reads `n.value` for a receiver typed as
{ps3449, ps3452}, only ps3449's value iv (constant {5})
contributes — ps3452's value field isn't in its
var_map.

This is a different root cause than the first two bugs in
this issue.  It's not about struct field elision (now
fixed); it's about the iv set on a CS that doesn't get
populated when the Node is created inside a function and
stored into another Node's field.

### Confirmed root cause

The bug has TWO layers:

**Layer 1: iv promotion is deferred to NOTYPE
violations.**  pyc's `reanalyze` callback only promotes
`unknown_vars` to `var_map` when a NOTYPE violation
references the missing field.  When a read through a
union receiver visits multiple CSs, NOTYPE fires only if
EVERY CS lacks the field; if at least one CS contributes
a type, no violation.  The inside-function Node's CS
receives writes (its unknown_vars has value/next) but
never gets a NOTYPE — it stays unpromoted.  The dispatch
over CSs at the read site silently skips this CS.

**Layer 2: when promotion happens, the existing
prim_period PNodes don't get re-processed.**  Even with
eager promotion via my updated `reanalyze` (walks ALL
CSs, not just NOTYPE-driven ones), the field's `Var->live`
stays 0.  The reason: after a `reanalyze` pass returns
true, FA does another iteration, but the worklist
mechanism only re-fires PNodes whose input AVars
changed.  Adding new ivs to a CS's `var_map` doesn't
modify the receiver AVar's `out`, so the period PNode
isn't re-enqueued.  `flow_vars(iv, result)` doesn't run
again → `result.backward` lacks the new iv →
`mark_live_avar` never propagates to it →
`field_sym->var->live` stays 0 → struct codegen elides
the field.

Verified with instrumentation: `mark_live_avar` is never
called on the value field's Var even after eager iv
promotion.

### Fix path

To make the user's natural model work — "every Node CS
should have a value-iv and the union should be int" —
both layers need to fix:

**Layer 1 fix (landed)**: `reanalyze` walks every CS
with pending `unknown_vars`, not just NOTYPE-triggered
ones.  Promotes uniformly.  Also reuses field syms via a
`promote_field_one` helper so two CSs of the same class
don't create duplicate `has` entries.  Sibling
propagation ensures all CSs of a class get the same
field set, keeping `determine_basic_clones` happy.

**Layer 2 fix (NOT landed)**: after promotion, force
the affected `prim_period` PNodes to be re-processed.
Options:
- Explicitly enqueue receiver AVars' `arg_of_send` PNodes
  for re-analysis after each promotion.
- Mark obj AVars dirty so the worklist re-fires.
- Walk forward from each newly-promoted iv to find period
  consumers and re-flow.

Each interacts with FA's worklist invariants and the
splitter.  This is the substantial remaining work; not
session-scale.

### Which option is most general

Layer 1 fix (eager iv promotion at write site, with
sibling propagation) is the cleanest and most general.
It removes a deferred-via-violation mechanism that's hard
to coordinate, and the invariant becomes: "every CS that
received a write has the corresponding iv."

Layer 2 (worklist re-fire) is the mechanical glue
needed to make layer 1's data structures actually
participate in the analysis.  Without layer 2, layer 1's
promotion is invisible to downstream consumers.

The constant-folding aspect (literal `5` in the body) is
a SYMPTOM of layer 2's gap.  Constant folding is correct
given pyc's analysis sees value={5} only.  With both
layers fixed, the union of `{3, 5}` would widen to int64
(num_constants_per_variable=1) and folding wouldn't
fire.

## Related

- [`issues/004-is-operator-unimplemented.md`](../../issues/004-is-operator-unimplemented.md)
  — the `is None` fix that unblocked recursive type
  narrowing.  This issue is the next layer down.
- [`ifa/issues/025-intra-function-union-narrowing.md`](025-intra-function-union-narrowing.md)
  — the broader narrowing infrastructure that
  prim_isinstance rewrites use.
- [`ifa/issues/027-v2-llvm-narrowed-loop-loses-struct-type.md`](027-v2-llvm-narrowed-loop-loses-struct-type.md)
  — v2 LLVM has a related struct-tracking issue.
- `ifa/CLONE.md` — pyc's clone-time specialization where
  per-CS layouts are determined.
- `ifa/codegen/cg.cc:write_prim_send` (P_prim_clone case)
  — codegen emission.
- `pyc_c_runtime.h` — runtime helpers and macros.
