# 177 — an `isinstance` / `is None` guard does not narrow a module-level variable

**Status: CLOSED 2026-10-12** (fix 1, in the frontend). Filed 2026-09-30 from
[issues/128](../../../issues/closed/128-cross-class-field-promotion.md) step 3, whose
converged MIXED writes this is one source of.

## Closure (2026-10-12)

`build_if_pyda` (`python_ifa_build_if1.cc`, `guard_reuse_begin`) reuses
the guard's own load of a module variable for the reads in its true branch
(`if` and each `elif`), so they read the SSU copy FA narrows. It is plain
redundant-load elimination, applied only where nothing can store the cell
in between:

- **The load runs whenever the condition does.** It is not under the right
  operand of `and` / `or`, a ternary, or a later link of a chained
  comparison (`unconditional_module_loads`).
- **No function declares the variable `global`.** build_syms records those
  (`PycCompiler::globals_declared_in_functions`), since a call could then
  store it and nearly anything is a call. A module-attribute store
  (`m.x = v`) can reach a module's own top-level code only through a
  circular import.
- **Nothing in the condition or the branch stores or deletes it.** Any node
  mentioning the name other than a plain read of this variable kills the
  reuse. That over-approximates, so it is safe, and it covers a store late
  in a loop body that reaches an earlier read on the next iteration.
- **Same scope only.** A function or class body defined inside the branch
  keeps its own loads.

`PYC_GUARDREUSE=0` turns it off. Measured: the repro no longer gives `B` a
field `f` (`IFA_DBG_LAYOUT`: `PROMOTE A.f`, `PROMOTE B.g` only, as for a
function local). Two tests cover it:

- `tests/guard_narrows_module_global.py`: B's `f` holds a str. It prints
  CPython's answer with the reuse. Without it, it fails to compile
  (`'x' illegal: int64`, `mixed basic types ( int64 str )`).
- `tests/guard_reuse_kills.py`: each kill (a store, a loop that rebinds, a
  called function that declares the variable `global`, a load under `or`,
  `del`) and the `elif` path print what CPython prints.

Sweep `check__default__82e9b98f+ad98a906` against `a941b8ba+84ddb2b5`:
byte-identical on every column for all 77 programs. `make test`: 444/0 on
both backends.

The false branch is not narrowed this way: `build_if_pyda` builds the else
chain before the condition. Writing the first test also found
[188](188-FA-field-and-method-of-one-name-on-two-classes.md), which is
unrelated.

The original report is kept below.

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
