# 126 — assess the residual method-slot reads

**Status:** open, filed 2026-09-03 out of
[123](123-CGEN-union-receiver-field-access-has-no-discrimination.md)'s
slot-usage measurement.
**Affects:** nothing yet — this is an assessment, not a defect.
**Depends on:** `IFA_DBG_SLOTUSE=1` (123), which enumerates them.

## Why this exists

93-98% of method slots are written into the vtable and never read
(123). The residue is tiny — 4 to 9 per program — and short enough to
enumerate and judge one at a time, which is worth doing before any work
on making dispatch cheaper: if some of these are not irreducible, the
vtable could go away almost entirely rather than almost.

```
bh (9)        Exception / SystemExit / StopIteration  .__str__
              Body.e20 / Cell.e18   .load_tree
              Body.e22 / Cell.e20   .walk_sub_tree
              Body.e17 / Cell.e17   .hack_cofm
go (4)        StopIteration / AssertionError  .__str__
              UCTNode  .__not__ , .__pyc_to_bool__
richards (7)  Exception / StopIteration / AssertionError  .__str__
              DeviceTask / HandlerTask / IdleTask / WorkTask  .fn
```

## Three groups, and what to ask of each

**1. `__str__` on the exception hierarchy — every program has it.**
`str()` of an exception whose class is not statically known. Worth
asking whether it is *program* polymorphism or an artifact of the
builtin library: the raise/except machinery may funnel every exception
through one generic formatting site, in which case the union is
manufactured by `__pyc__` rather than by the user's code. If so, the
whole group could go away for every program at once — the highest-value
question here.

**2. Genuine container polymorphism.** bh's six (`load_tree`,
`walk_sub_tree`, `hack_cofm` on `{Cell, Body}`) are the octree walk over
`Cell.subp`, which really does hold both. These look irreducible without
runtime type information, and they are the same union that
`collect_prefix_groups` flags for layout alignment — note `load_tree`
sits at `Cell[18]` and `Body[20]`, the exact conflict 123 reports.
Expect these to stay.

**3. Uncertain, and the most interesting.** richards' `.fn` on four
Task subclasses: is `fn` a *method* being overridden, or a DATA
attribute holding a function? The measurement's filter is "the member's
name matches some function in the program", which does not distinguish
the two. If it is a data attribute, it is not a vtable slot at all and
the classification is wrong. Same question for go's `UCTNode.__not__`
and `.__pyc_to_bool__` — truthiness on a possibly-`None` `UCTNode`,
which may be an `Optional` narrowing gap rather than real dispatch.

## Verification plan

1. For each entry, name the source construct that produces it — the
   call site and why FA could not resolve it to one target.
2. Classify: irreducible OOP dispatch / library artifact / imprecision.
3. For anything in the last two classes, file or link the underlying
   issue.
4. Re-run `IFA_DBG_SLOTUSE=1` across more of the corpus than the three
   programs above, to check the pattern holds and the residue really is
   this small everywhere.

## What this unblocks

If group 1 is a library artifact and group 3 is misclassified, the
irreducible set is bh's six and little else — i.e. the classtag vtable
would exist for a handful of genuinely polymorphic containers, and
"eliminate unused slots" (123) becomes close to "eliminate the vtable".
That changes the cost/benefit of every other dispatch idea in
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md).
