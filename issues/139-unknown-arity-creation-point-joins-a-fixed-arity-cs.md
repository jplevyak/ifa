# 139 — an unknown-arity creation point joins a fixed-arity CreationSet

**Status:** fixed 2026-09-06. Root cause of `tests/builtins.py` under
`PYC_CSDCPA1=2`, the last element-union failure in
[129](129-plan-demand-driven-creation-set-splitting.md)'s suite bill.

## Symptom

```
builtins.py:8:67: error: 'element' has mixed basic types:( int64 str )
    print(all([1, 2, 3]))
  called from __pyc__.py:1573
```

`element` is **`all`'s loop variable**, not a container's element channel —
which is why `IFA_DBG_MIXELEM` reported nothing and the first three
attempts to place this in ifa/133's family were wrong.

## Root cause

`IFA_DBG_CSVARS=list`, with the element channel and defs added for this:

```
CSVARS cs=996 sym=list vars=3 defs=5 arity=3 no_arity=0 elem= str
  DEF es=2  fun=__main__   x4      <- [1,2,3] [1,0,3] [0,2,0] [0,0,0]
  DEF es=68 fun=__mul__    x1      <- [' '] * len("foobar")
  var=? type= int64#6
```

**One CreationSet with three `int64` positional vars and a `str` element
channel.** `list.__mul__`'s result — a runtime-length `str` list — joined
the arity-3 CreationSet of four `int` literals. `all(iterable)` then reads
the container and its loop variable unions `int64` and `str`.

The guard in `creation_point`'s dcpa1 route was:

```c
if (arity >= 0 && x->static_arity >= 0 && x->static_arity != arity && !x->no_static_arity) continue;
```

**It only fires when the INCOMING arity is known.** `__mul__`'s result has
`arity == -1`, so the check was skipped entirely and it joined a
fixed-arity CreationSet unchecked.

[132](132-arity-is-representation-not-provenance.md) established that
arity is a representation property CreationSet identity must respect, and
its comment covers one direction — a CS that has *already* lost its static
arity is on list layout, reads its length at run time, and can absorb any
arity. The opposite direction was never covered: merging an unknown-arity
container INTO a fixed-arity CS is exactly as unrepresentable, because the
CS keeps its record layout (`vars.n` fixed) while now holding a container
whose length is unknown until run time.

## Fix

```c
if (x->static_arity >= 0 && !x->no_static_arity && (arity < 0 || x->static_arity != arity)) continue;
```

Decline when the candidate has a fixed arity it has not lost, and the
incoming arity is either unknown or different. The `no_static_arity`
absorption ifa/132 documents is unchanged.

## Result

`builtins` compiles clean — **0 errors, 0 warnings**. The flag arm's suite
goes **4 → 3**. Default path untouched: `make test` 311/0, LLVM backend
311/0.

## Note on how it was found

Three probes were needed and the first two misled:

- `IFA_DBG_MIXELEM` reports nothing, because the union is on a local
  variable rather than a container element. Absence there is not evidence
  the containers are fine.
- `IFA_DBG_CSVARS` printed only the positional channel. A list-layout
  container keeps its content in the ELEMENT channel, so the CS that was
  visibly wrong looked ordinary. The element channel and the def list were
  added to that probe here, and the answer was immediate.

The general lesson matches [104](closed/104-unify-list-and-tuple-in-analysis.md):
a container has TWO content channels, and a probe that reads one of them
can report a clean CreationSet that is not clean.

## Corpus ledger — net +1, and the two it breaks are latent defects

Measured with `PYC_ARITYSTRICT` (default 1; `=0` restores the pre-139
form, kept so this attribution can be re-run):

| | |
| --- | --- |
| **fixed** | `builtins` (suite), `pystone`, `othello2` (corpus) |
| **broken** | `softrender`, `voronoi2` |

Corpus `compile_fail` 10 → 9. Both new failures are for reasons the merge
was masking, not for arity:

- `voronoi2` — `receiver 'str' is not a container but '__add__' resolved
  to the CONTAINER method`, i.e.
  [137](137-scalar-receiver-resolves-to-container-method.md)'s resolution
  defect on a second program. Notably this guard FIXED 137's first
  instance (`pystone`) and exposed another.
- `softrender` — `cast from pointer to smaller type '_CG_bool' loses
  information`, a layout/representation defect.

Neither is an argument against the guard: merging an unknown-arity
container into a fixed-arity CreationSet is unrepresentable regardless of
what the merge happened to be hiding. Recorded so the two are triaged as
their own defects rather than read as a reason to revert.