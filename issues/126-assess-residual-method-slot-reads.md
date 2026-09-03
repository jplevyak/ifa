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

## Assessed: `Basic_block | Union_find_node` — shedskin says imprecision

The union blocking slot elision in `tests/list_index_type_mismatch_salvage`
(havlak loop finder) is **not in the program**. shedskin translates the
same file cleanly and types everything concretely:

```cpp
class Union_find_node { Basic_block *bb_; Simple_loop *loop_;
                        Union_find_node *parent_; __ss_int dfs_number_; };
class Basic_block     { __ss_int name_;
                        list<Basic_block *> *in_edges_, *out_edges_; };
```

There is exactly **one** `pyobj *` in the whole generated `.cpp` — i.e.
essentially no unions anywhere, and certainly not between these two
classes, which share no base but `object`.

pyc's union appears at **`__eq__`**: `IFA_DBG_SLOTUSE=1` reports
`READ-METHOD Basic_block.e1 /* __eq__ */` and
`READ-METHOD Union_find_node.e1 /* __eq__ */`, plus
`Basic_block.e6 /* __not__ */`. `__eq__` is declared on `object` and
inherited by both, so one shared `object.__eq__` clone is receiving both
callers — the shared-generic-method degeneration of
[105](105-type-degeneration-in-shared-generic-methods.md), and the same
family as [issues/039](../../issues/039-list-mul-shared-element-type-cross-contamination.md).
shedskin does not have it because `list<Basic_block *>` and
`list<Union_find_node *>` are separate template instantiations, so the
`__eq__` each calls is a separate function.

**Consequence for 123.** The one test blocking slot elision is blocked
by an imprecision, not by real polymorphism — so the answer is not to
teach elision about dispatch groups (three attempts, all measured worse
than no constraint). It is either 105/039, or the classtag dispatch
should not be generated for a union that FA should never have formed.

That also sharpens this issue's group 1: the `__str__`-on-exceptions
reads every program shows are the same shape — one shared `object`
method receiving every caller. If 105 is fixed, group 1 and this case
both disappear, and the irreducible residue is bh's octree and
richards' Task `.fn`.
