# Issue 027: v2 LLVM loses struct type through `while x is not None` narrowing

**Status:** closed (fixed).
**Affects:** v2 LLVM backend (decommissioned).
Fixed by the replacement of `cg_normalize_v2` with `VirtualCGEmitter` and `LLVMEmitter`. The new `resolve_union_receiver` properly extracts the struct type (`Node`) from the narrowed `[Node | None]` union type passing through the `while` back-edge.
**Surfaced while:** writing iterative linked list tests
after the `is None` fix.

## Symptom

```python
class Node:
  def __init__(self, v):
    self.value = v
    self.next = None

def sum_all(head):
  total = 0
  node = head
  while node is not None:
    total = total + node.value
    node = node.next
  return total
```

C backend: works.
v2 LLVM backend: `LLVM module verification failed`:
```
Invalid indices for GEP pointer type!
  %0 = getelementptr inbounds nuw ptr, ptr %node15, i32 0, i32 2
```

The GEP target is `ptr` (untyped), not the expected
`%Node.NNNN` struct pointer.  The struct type is lost on
the loop-body's view of `node`.

## What works

Same logical operation as a tail-recursive function works
on v2 LLVM:

```python
def sum_all(node, total):
  if node is None:
    return total
  return sum_all(node.next, total + node.value)
```

This is `recursive_list_is_none.py` shape — the recursive
form passes through the prim_isinstance narrowing
infrastructure cleanly.  The iterative form's
SSU-via-while loop produces a different IR shape that the
v2 LLVM emit doesn't handle.

## Probable cause

The frontend rewrite (issue 024) turns
`node is not None` into
`not isinstance(node, sym_nil_type)`.  In a `while`
condition this rewrite + the SSU phi-renaming + the loop's
back-edge probably leaves the v2 LLVM emit pointing at a
phi/phy slot whose type was inferred as `ptr` rather than
`Node *`.

A quick LLVM IR inspection would confirm whether:
- The body's `node` variable is allocated as `ptr` instead
  of `Node *`, OR
- The phi merging at the loop header collapses to `ptr`
  because the back-edge brings in a `Node *` and the
  initial value brings in a `Node *` but the loop-end
  brings in `nil_type`-typed merged AVar.

## Workaround

Use the recursive form.  `recursive_list_is_none.py` and
`linked_list_ops.py` (which uses recursive `length`,
`find_max`, `find`) work on both backends.

## Related

- [`ifa/issues/024-is-comparison-narrowing.md`](closed/024-is-comparison-narrowing.md)
  — the frontend rewrite this issue is downstream of.
- [`ifa/issues/025-intra-function-union-narrowing.md`](025-intra-function-union-narrowing.md)
  — the Code_IF per-branch narrowing infrastructure.
- `ifa/codegen/cg_normalize_v2.cc` — v2 LLVM's per-SEND
  lowering where the type would need to flow through.
