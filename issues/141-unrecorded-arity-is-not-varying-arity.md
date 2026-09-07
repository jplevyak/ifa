# 141 — `static_arity == -1` means "not known yet", not "known to vary"

**Status:** fixed 2026-09-07. Found by hunting the specific bad edge in
`rdb`, per [133](133-split-a-container-on-its-element-type.md)'s
conclusion that this family yields to individual wrong edges rather than
to another splitter.

## The edge

`IFA_DBG_CSROUTE=list` on `rdb` under `PYC_CSDCPA1=2`:

```
p=0 es=68  -> cs=1260 via MINT     <- list.__pyc_getslice__'s merge(self,self), arity -1
p=0 es=87  -> cs=1260 via dcpa1    <- an arity-3 literal JOINS it
p=0 es=226 -> cs=1260 via dcpa1
```

leaving

```
CSVARS cs=1260 sym=list vars=3 defs=1 arity=3 no_arity=0 elem= str
```

a three-element list literal carrying a `str` element — which surfaces as
`'v' has mixed basic types:( int64 str )` at `rdb.py:244`.

## Root cause

[139](139-unknown-arity-creation-point-joins-a-fixed-arity-cs.md) closed
one direction of the arity guard: an unknown-arity creation point could
join a CreationSet whose arity was already fixed. It declined only when
`x->static_arity >= 0`.

**A CreationSet minted with `static_arity == -1` therefore still accepted
anything**, and `make_kind` then fixed its arity from whichever joiner
arrived first — so `cs=1260` was minted at arity `-1` by `getslice`, took
an arity-3 literal, and ended up `arity=3` with the slice's `str` element.

The guard conflated two different states:

| | meaning | may absorb any arity? |
| --- | --- | --- |
| `no_static_arity` | arity is known to VARY; the CS is on list layout and reads its length at run time | **yes** (ifa/132) |
| `static_arity == -1` | arity has not been RECORDED yet | **no** |

## Fix

```c
if (!x->no_static_arity && x->static_arity != arity) continue;
```

"Arities agree, with `no_static_arity` the one exemption." Strictly
stronger than 139 and simpler: it subsumes 139's clause and adds the
`-1`-candidate case.

## Result

`voronoi2` compiles (it was one of the two programs ifa/139's tightening
had broken). `sudoku5`, `softrender` and `richards` stay compiling. Suite
unchanged: default 311/0 both backends, flag arm 2 (the two goldens).

`rdb` itself does **not** compile after this — the specific edge is gone
(`cs=1260` is now `arity=-1` with a clean `str` element) but another
merge feeds the same union. That is the honest outcome: one edge found and
fixed, the program not yet clean.

## Measured and reverted on the way

`list.__add__` uses `merge_in(self, l)`, which merges `l`'s element INTO
`self`'s CreationSet and aliases the result to it — so `a + b` mutates
`a`'s contour even though `+` does not modify `a`. `__mul__` two methods
below uses `merge`, which mints a fresh CreationSet holding both.
Switching `__add__` to `merge` looked like the same false-constraint shape
as `__delitem__`'s and `heapq`'s.

**It is not.** The change was inert on all five corpus programs and broke
one suite test, so it was reverted. Recorded because the reasoning is
sound and someone will have it again: the result of `_CG_list_add`
genuinely aliases `self`'s storage, so the two contours are not
independent, and the merge is not obviously false.
