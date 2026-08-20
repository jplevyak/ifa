# 090 — a variable whose tuple arity (or None-vs-tuple shape) changes across loop iterations can't resolve a call site

**Status:** repro 1 **FIXED** 2026-08-20 (`370c8806`); repro 2 still
open. The "arity" framing was wrong for repro 1 — see "What repro 1
actually was" below. Symptoms re-measured 2026-08-20; see "Re-measured" below. Found 2026-08-08 while diagnosing
[issues/025](../../issues/025-shedskin-examples-coverage.md)'s TODO
list item 4 (sunfish's blocker — the doc's own "`sizeof_element of
non-container type` internal fail in `__add__`" claim is **stale**;
re-verified today, that specific fail no longer appears in sunfish's
compile output at all, superseded by this one and by
[ifa/issues/089](089-DISPATCH-closure-pyc-to-bool-no-candidate.md)-adjacent
gaps found along the way, see issues/025 for the full trace).

**Affects:** `ifa/codegen/cg.cc`'s `get_target_fun` (~line 953),
which hard-`fail`s the whole compile when a call site's receiver type
has no single resolvable target function. Root cause is upstream —
tuples are fixed-arity `Type_RECORD`s (one concrete type per arity,
per [ifa/issues/closed/069](closed/069-per-arity-tuple-types-scope.md)) — so
a single `Var` that needs to hold *different arities* (or `None` vs.
a tuple) across a loop's iterations has no single concrete type for
FA to settle on, and codegen has nothing to call through.

## Repro 1 — accumulating tuples via `+` in a loop

```python
t = ()
for i in range(3):
    t = t + (i, i+1)
print(t)
```
- CPython: `(0, 1, 1, 2, 2, 3)`.
- pyc: `fail: unable to resolve to a single function at call site` —
  a clean compile-time rejection, not a crash. Each iteration's `t +
  (i, i+1)` needs a *different* concrete tuple-arity type for `t`
  (0-tuple, then 2-tuple, then 4-tuple, then 6-tuple...) — impossible
  to give the loop-carried `t` one static type.

This is the mechanism behind sunfish.py:75's
`padrow = lambda row: (0,) + tuple(x+piece[k] for x in row) + (0,)`
combined with `sum((padrow(...) for i in range(8)), ())` (line 76) —
even after fixing `sum()`'s missing `start` parameter (this session,
see [issues/025](../../issues/025-shedskin-examples-coverage.md)'s
item 4), the accumulator inside `sum()`'s shared loop body still needs
to hold a growing tuple across iterations, hitting this exact wall
(confirmed: re-running sunfish.py post-`sum()`-fix, this is now the
*first* error reported, at line 75).

## Repro 2 — a loop-carried `None`-or-tuple variable

```python
def gen_moves():
    return [(1, 2), (3, 4)]

move = None
while move not in gen_moves():
    move = (1, 2)
print(move)
```
- CPython: `(1, 2)`.
- pyc: identical `fail: unable to resolve to a single function at
  call site`.

This is sunfish.py:448's `while move not in hist[-1].gen_moves():`
(`move = None` initially, reassigned to an actual move tuple inside
the loop) — a second, independent trigger of the same underlying
"receiver type can't settle on one concrete shape across the loop"
mechanism, this time via `None`/tuple rather than two different tuple
arities. Not confirmed whether this is `__contains__`/`__eq__`
dispatch specifically or something more general about `move`'s type;
not traced further.

## Why not root-caused/fixed further here

Both repros hit the exact same `fail()` site
(`get_target_fun`/`cg.cc:956`) via what looks like the same
underlying cause (a `Var` needing more than one concrete tuple-family
type across loop iterations), but this session's digging only
confirmed the *symptom* and its call site, not why FA lets these
programs reach codegen with an unresolved type instead of catching
it earlier (or whether catching it earlier is even possible without
a deeper representational change — [ifa/issues/closed/069](closed/069-per-arity-tuple-types-scope.md)'s
own history suggests per-arity tuple types are foundational and
unlikely to change lightly). This may be a genuine architectural
limit (Python's dynamic tuple-arity-changing-in-a-loop pattern is
fundamentally in tension with a compiler that gives every loop-carried
variable one static type) rather than a bug with a real fix — worth
someone with FA context making that call explicitly rather than this
session guessing.

## Verification plan once addressed (or ruled permanently out of scope)

- Both repros above should either compile and match CPython, or (if
  ruled architecturally infeasible) get a clean, documented,
  compile-time diagnostic explaining why, rather than the generic
  "unable to resolve to a single function at call site" message.
- `shedskin_examples/sunfish/sunfish.py` — re-run `pyc -r` and confirm
  whether this was the last blocker or whether (per this session's
  repeated pattern) another layer is underneath.

## What this unblocks

sunfish.py's `pst` (piece-square table) construction and its move-input
loop, both idiomatic, common shapes (flatten-via-`sum`, "no selection
yet" sentinel-then-loop). More generally, any program that grows a
tuple's arity across loop iterations or lets a loop-carried variable's
type span `None`/tuple.


## Re-measured 2026-08-20 — the clean rejection is gone

The underlying limitation is unchanged: a loop-carried variable that
needs more than one concrete tuple-family type still cannot resolve.
What changed is what pyc *does* about it. Both repros used to be a
clean compile-time `fail`. Neither is now.

| repro | filed 2026-08-08 | C backend now | LLVM backend now | CPython |
|---|---|---|---|---|
| 1, arity grows in a loop | compile-time `fail` | compiles, **runtime abort** | compiles, **prints nothing** | `(0, 1, 1, 2, 2, 3)` |
| 2, `None`-or-tuple | compile-time `fail` | compiles, **prints `None`** | compiles, **prints `None`** | `(1, 2)` |

Repro 2 is now a **silent wrong answer on both backends** — the worst
of the three possible outcomes, and the one this issue's own
verification plan explicitly rules out:

> Both repros above should either compile and match CPython, or (if
> ruled architecturally infeasible) get a clean, documented,
> compile-time diagnostic explaining why, rather than the generic
> "unable to resolve to a single function at call site" message.

### Why repro 2 prints `None`

The dispatch does not fail loudly any more, it degrades. pyc reports

    sunfish.py:448: warning: unresolved call '__not__'

and carries on. `move not in gen_moves()` therefore evaluates to
something the loop body never runs on, `move` keeps its initial
`None`, and `print(move)` faithfully prints it. The wrong answer is a
direct consequence of a warning where there used to be an error.

### sunfish

Also changed: sunfish now **compiles** (rc=0, 6 warnings) where it
used to fail, and aborts at runtime instead —

    sunfish: assert(!"runtime error: matching function not found")

so this is still its blocker, just relocated from compile time to run
time. Line 448 is still the first site reported.

### What to do about it

The architectural question this issue raised — whether per-arity tuple
types can accommodate a loop-carried variable at all — is untouched and
still needs someone with FA context to make the call. But that question
is now *separable* from a defect that is worth fixing regardless:
**an unresolved call must not be downgraded to a warning that yields a
silently wrong program.** Restoring the compile-time error for this
shape would put both repros back on the "clean documented rejection"
branch of the verification plan without needing the representational
change at all.

Not bisected: no attempt was made here to find which commit turned the
`fail` into a warning.


## Why loop-carried variables are "special" — they are not (2026-08-20)

pyc CAN represent a tuple of runtime arity: a CreationSet whose
`tuple_able()` is false takes LIST layout, the same "unknown arity,
known element type" representation a list has (issues/110). So why does
a loop-carried variable not get it?

**Because the decision is made one phase too early, and at the wrong
granularity.** Measured on repro 1:

    [tupgrp] group=449 size=1 cs=948  arity=0 elem=bottom able=1
    [tupgrp] group=458 size=1 cs=1009 arity=2 elem=bottom able=1

Two CreationSet groups, each of size **1**. The escape hatch lives in
`get_sym_tup`:

```cpp
if (n < 0) n = cs->vars.n;
else if (n != cs->vars.n) tup = false;   // differing arity -> not a record
```

— and it only fires when the differing arities are in the **same
group**. `()` and `(i, i+1)` are two creation sites, so two groups,
each internally uniform (one arity, unpopulated element), each
independently electing RECORD.

### The two arities never meet until it is too late

A cross-group pass was written to catch exactly this: scan for any type
holding two tuple CreationSets of differing arity and mark them
`no_static_arity`. It marked **nothing** — first over every Fun's
`fa_all_Vars` (71 funs), then over every CreationSet's own `atype`
(587 CreationSets). There is no such type at that point.

The reason is `concretize_var_type`, which runs AFTER
`define_concrete_types`:

```cpp
for (int i = 0; i < v->avars.n; i++)          // across CONTOURS
  for (CreationSet *cs : *v->avars[i].value->out)
    if (sym != cs->type) { type = new_Sym(); type->type_kind = Type_SUM; ... }
```

FA keeps the arities apart per contour; the `Type_SUM` that forces one
static type on `t` is **manufactured here**, after layout is already
fixed. That is the `_:?` receiver the dispatch probe reports, with one
`__add__` clone per arity and nothing to discriminate on.

### So the ordering is the defect, not the representation

1. FA — the two arities live in separate contours, never in one AType.
2. `define_concrete_types` — record-vs-list chosen **per CreationSet
   group**; each group sees one arity and picks RECORD.
3. `concretize_var_type` — merges the per-contour types into a
   `Type_SUM`. First moment anyone knows `t` needs both.

A fix does not need the representational change this issue originally
assumed. It needs the arity spread of a **Var across its contours** to
be known before step 2 — the information exists (`v->avars`), it is
simply consulted a phase later. Note the scan must reach module-level
globals: `t` is one, and globals are not in any Fun's `fa_all_Vars`,
which is why the per-Fun version of the pass found nothing.


## Fix attempted in three stages — two work, the third does not (2026-08-20)

Acting on the analysis above, the fix was built and measured stage by
stage against repro 1 under `PYC_MAKESEQ=1 PYC_TUPLE_AS_LIST=1`. All of
it is REVERTED; this records exactly how far it got.

### Stage 1 — mark the Var, not the group ✅

A pass before `set_tuple_able` walking each Var **across its contours**
(the same nesting `concretize_var_type` uses) and marking every tuple
CreationSet `no_static_arity` when their arities differ:

    [marity] var 'self': tuple cs=948  arity=0 -> list layout
    [marity] var 'self': tuple cs=1009 arity=2 -> list layout

Both CreationSets take LIST layout. **The loop nesting is the whole
trick**: a first version scanned *within* one AVar's type and matched
nothing at all, because the arities live in different AVars of one Var,
never in one type. (A variant scanning all 587 CreationSets' `atype`
also matched nothing, for the same reason.)

### Stage 2 — derive the element from the per-index vars ✅

Stage 1 alone produces `error: incompatible integer to pointer
conversion assigning to '_CG_void_type' from '_CG_int64'`. A tuple
forced to list layout has real per-index vars but a **bottom generic
element** — `make` fills the vars and leaves the element alone, which is
the tuple_able design. `compute_member_types` derives the element from
that bottom, so the list gets a `void*` element.

Taking the union of `cs->vars` instead when the CreationSet is
`no_static_arity` fixes it; the C compiles.

### Stage 3 — make the groups share one type ❌

Still `runtime error: matching function not found`. Each group is
cloned into its OWN concrete Sym, so the Var's type is *still* a
`Type_SUM` — now of two list-layout tuples instead of two records. No
improvement at the call site.

Merging the groups before type assignment was then implemented:
`merge_multi_arity_tuple_groups` folds every group holding a member of
one recorded set into a single group. This needs two supporting
changes, both of which were made and both of which are correct:

- every per-group loop must tolerate an **empty** group, since merging
  empties rather than compacts (otherwise `get_sym_tup` returns a null
  sym and `clone()` segfaults the compiler);
- `compute_member_types`' `assert(!n || n == cs->vars.n)` must allow a
  mixed-arity group when it is `no_static_arity`, filling only the
  element and no `has` slots — and neither lookup (`get_element_avar`,
  `cs->vars[i]`) is guaranteed for every member any more.

With all of that, the compiler no longer crashes and repro 1 still
fails identically. Something downstream of the merge continues to see
two candidates; that was not chased further.

### Where the next attempt should start

Stages 1 and 2 are believed right and are cheap to re-create from the
description above. Stage 3 is the open question: **why merging the
CreationSet groups does not collapse the call site's candidate set.**
Look at what `f->calls` holds for the `__add__` pnode after the merge,
and whether the per-arity `__add__` clones (which FA made, per contour,
before any of this runs) are what actually need collapsing — in which
case the group merge is treating a symptom and the real lever is
upstream in cloning, not in layout.


## What repro 1 actually was — not arity (2026-08-20)

```python
t = ()
for i in range(3):
    t = t + (i, i+1)
```

`tuple.__add__` built a **list** and returned it (the old compromise:
a fixed-arity struct cannot concatenate at runtime). So `t` was a
`{tuple, list}` UNION — two different classes at one call site — and
that is what could not resolve. The varying arity was incidental.

Measured directly, before any fix: the two tuple CreationSets
**already shared one concrete type** (identical Sym pointer). The
receiver `_:?` in the dispatch trace was the tuple/list union, not a
union of two arities.

`make_seq` removes the constraint the compromise was built around — it
yields a tuple whose arity IS a runtime value (issues/110) — so
`tuple.__add__` now returns a tuple. Repro 1 prints
`(0, 1, 1, 2, 2, 3)`, matching CPython, **with no analysis change at
all**.

### The three-stage layout work above was chasing the wrong thing

Stages 1 and 2 (mark a Var's tuple CreationSets across contours; derive
the element from the per-index vars) are real and do what they say, but
they were **not needed**: reverting all of them and keeping only the
one-line `__add__` change fixes repro 1 on its own. Stage 3 failed
because there was nothing to merge — the types were already identical.

Kept in this file as a record of a wrong turn, not as work to resume.
The lesson is narrow and worth stating: **the dispatch probe printed
`r1=_:?` and I read "union of two arities" into it without checking
what was in the union.**

### What landed

- `tuple.__add__` returns a tuple via `make_seq`.
- The LLVM backend gained a `make_seq` case; it had none, so the shared
  dispatcher fell through to a generic `_CG_prim_<name>` external and
  the link failed with an undefined `_CG_prim_make_seq`. That was
  invisible while make_seq was reachable only behind `PYC_MAKESEQ`.

Two known issues resolved at DEFAULT settings, both being
`(0,) + tuple(...) + (0,)` — sunfish's `padrow` shape:
`tests/tuple_from_iterable_is_list` (issues/110) and
`tests/tuple_arity_union_slice` (ifa/issues/109).

    C backend      285 passed / 0 failed   (was 283)
    LLVM backend   285 passed / 0 failed
    corpus         70 compiled / 7 failed of 77, per-program identical

## Repro 2 is still open and is a DIFFERENT mechanism

```python
move = None
while move not in gen_moves():
    move = (1, 2)
```

still prints `None` on both backends where CPython prints `(1, 2)`,
with `warning: unresolved call '__not__'`. That is a `{None, tuple}`
receiver reaching `__contains__`/`__not__`, not a tuple/list union and
not an arity problem. It remains sunfish's blocker (line 448).
