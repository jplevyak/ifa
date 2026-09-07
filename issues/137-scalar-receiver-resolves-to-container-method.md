# 137 — a scalar receiver resolves to the CONTAINER method

**Status:** open, found 2026-09-06 while triaging
[129](129-plan-demand-driven-creation-set-splitting.md)'s flag arm.
Affects `pystone` under `PYC_CSDCPA1=2`.

## The diagnostic said the wrong thing

```
fail: a variable holding 'int64' has no representation: '__add__' resolved
to the CONTAINER method, whose receiver may be a scalar. pyc does not box,
so a {container, scalar} union cannot be represented (issues/018)
```

**A variable holding ONE type always has a representation.** The message
asserts a `{container, scalar}` union that does not exist here — `outer`
is a bare `int64`.

The condition it fires on (`cg.cc:1432`) is `!t->element`: a receiver with
no element channel reached a container method's `sizeof_element`. That is
a real problem, but it has two quite different causes, and the message
conflated them:

| receiver | meaning |
| --- | --- |
| a union of container and scalar | genuinely unrepresentable — issues/018 |
| a single non-container | **a RESOLUTION defect** — this receiver should never have reached this method |

Split at `codegen_common.cc:115`. `pystone` now reports the second form;
`linalg`, whose receiver really is `{int64, list}`, keeps the first.

## What is actually wrong

`__add__` on an `int64` receiver resolved to `list.__add__`. Per CLAUDE.md
("Boxing is never the answer for a corpus program") this is a pyc
inference/dispatch defect, not a representation gap — shedskin compiles
`pystone` without boxing.

The site is `pystone.py:41`:

```python
Array2Glob = [x[:] for x in [Array1Glob]*51]
```

with three preceding warnings — `expression has no type`, and twice
`illegal call argument type expression illegal: slice`. So the slice
`x[:]` is being applied to something untyped, and the `int64` receiver
reaching `__add__` is downstream of that.

**Not reducible in isolation**: `[A]*n` with a slice, in and out of a
comprehension, compiles clean on both arms in four-line form. It needs
pystone's surrounding context, so the next step is bisecting pystone
itself rather than guessing at a repro.

## Flag-specific

`pystone` compiles at the default. Only the `PYC_CSDCPA1=2` arm fails, so
whatever merges the receiver's type is downstream of the start-merged
posture — but the resolution defect it exposes is not obviously
flag-specific and may be latent at the default.

## What is needed

1. Bisect `pystone` to the smallest failing form (the four-line attempts
   did not reproduce; it needs the `Record` class and the globals).
2. Establish whether `__add__`'s candidate set legitimately contains
   `list.__add__` for this receiver, or whether the receiver's type is
   already wrong by then. `PYC_DBG_CALLS` and
   `IFA_DBG_DISPATCHFAIL` are the probes; `get_target_fun_core` is the
   resolver to ask rather than reimplement.
3. The three `slice` warnings at the same site are almost certainly the
   same defect one step earlier — start there, not at the `fail`.
