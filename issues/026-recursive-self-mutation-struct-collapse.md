# Issue 026: recursive types with >1 self-typed field lose value field in C struct

**Status:** open.
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

## Related

- [`issues/004-is-operator-unimplemented.md`](../../issues/004-is-operator-unimplemented.md)
  — the `is None` fix that unblocked recursive type
  narrowing.  This issue is the next layer down — even
  with narrowing working, the struct layout for
  self-mutating recursive types is wrong.
- [`ifa/issues/025-intra-function-union-narrowing.md`](025-intra-function-union-narrowing.md)
  — the broader narrowing infrastructure that prim_isinstance
  rewrites use.
- `ifa/CLONE.md` — pyc's clone-time specialization, where
  the struct field pruning likely happens.
