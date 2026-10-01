# 177 — an `isinstance` / `is None` guard does not narrow a module-level variable

**Status:** open. Filed 2026-09-30 from
[issues/128](../../issues/128-cross-class-field-promotion.md) step 3, whose
converged MIXED writes this is one source of.

## Symptom

```python
class A:
    def __init__(self): self.f = 0
class B:
    def __init__(self): self.g = 0
xs = [A(), B()]
x = xs[0]
if isinstance(x, A):
    x.f = 4          # the receiver is still {A, B}
print(xs[0].f, xs[1].g)
```

CPython and pyc both print `4 0`, but the write lands on `{A, B}`, so
`B` is given a field `f` it never has (`IFA_DBG_LAYOUT`: `PROMOTE B.f`;
`IFA_DBG_FIELDSPLIT=2` shows the final-pass write on `A B`). The same
program with `x` a function local narrows correctly: the write lands on
`A` only and `B` has no `f`.

A read has the same gap, but it hides: a member read on a union where only
SOME classes have the member is typed anyway (issues/170's rule), so
nothing reports it.

## Cause

Narrowing (`Code_IF` in `add_pnode_constraints`) restricts the
per-branch SSU copy of the OPERAND of the test. A function local has one
SSU name, so every use in the branch sees the restriction. A module-level
variable is a memory cell (closed ifa/031 step 2): EACH read is a fresh
load into its own temp. The guard narrows the temp it tested. The later
load that the write uses is a different temp, loaded from the unnarrowed
cell.

## Fix

Either:

1. **In the frontend:** inside a branch guarded by a test on a
   module-level name, reuse the tested load for later reads of that name
   in the branch, until an intervening store to it or a call that could
   store it. This is sound only with that kill set, because the cell can
   be written between the test and the use.
2. **In FA:** apply the branch's restriction to every load of the same
   cell dominated by the branch and not separated from it by a store to
   the cell. This needs the same kill set, expressed over IF1.

## Verification

The repro gives `B` no `f` (`IFA_DBG_LAYOUT`), and
`IFA_DBG_FIELDSPLIT=2` shows the write on `A` alone. A version with a
store to `x` between the guard and the write must still see `{A, B}`.
