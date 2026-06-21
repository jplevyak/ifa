# Issue 026: recursive types with >1 self-typed field lose value field in C struct

**Status:** **partial fix landed June 2026** — root-cause
diagnosed; the prototype-vs-instance size mismatch is fixed
(manual tree builds now work).  A second related bug
(per-CS field tracking dropping writes from outside
`__init__`) still blocks BST insert and doubly-linked list.
See "Fix landed" section below.
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

## What's still broken

A *second* related bug blocks BST insert and DLL:

```python
def insert(root, v):
  if root is None:
    return Node(v)
  ...

t = Node(5)
t = insert(t, 3)
t = insert(t, 7)
print(sum_tree(t))   # returns 30 instead of 29
```

The generated struct for Node is missing the `value`
field even though `__init__` does `self.value = v`.  Both
clones of `Node::__init__` in the C output write only
`e2=NULL; e3=NULL;` — the value write is **dropped**.

Also `__init__` is generated as `(_CG_ps3548 a1)` — no v
parameter.  Pyc constant-folded v across clones.

This is a different root cause — **per-CS field tracking
is missing writes**.  In the DLL test:

```python
head = Node(1)
n2 = Node(2)
n2.prev = head   # write of head into n2.prev
```

The `n2.prev = head` write writes a Node into the prev
field.  IFA tracks this write on n2's CS, but the
generated struct for ps3449 (the Node CS) has only e1
(value) and e3 (next) — no e2 (prev).

Hypothesis: each `Node(v)` call site creates a SEPARATE
CreationSet.  Field writes are tracked per-CS.  The CS for
n2 receives writes to `prev` from main but the CS that
`__init__` writes via gets only value and next.  The
struct synthesis picks ONE CS's layout but the codegen
emits casts to a different CS's type.  Or the CSs are
merged for cast purposes but their field sets aren't
unioned.

This second bug needs further investigation.  It seems to
require either:

- Merging field sets across all CSs of the same instance
  type before struct synthesis.
- Or making field writes propagate to all CSs of the
  same sym at IFA time.

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
