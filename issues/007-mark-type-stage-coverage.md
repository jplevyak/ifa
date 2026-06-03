# Issue 007: post-type splitter stages not triggered by any shape

**Status:** partial — setter stage NOW reachable via the
`vector_iterator` synthetic shape (Phase 09 C 7.7). Remaining
gaps: `mark-type`, `setter-of-setter`, `mark-setter`,
`mark-setter-of-setter`, `violation`.
**Affects:** `ifa/analysis/fa.cc:split_ess_for_mark_type`,
`split_for_setters_of_setters`, and the mark-based variants;
`IFA.md` splitter section.
**Related:** [issues/003-fa-converge-determinism.md](003-fa-converge-determinism.md)
(empirical stage recon), [testing/phases/09c_splitter_triggers.md](../testing/phases/09c_splitter_triggers.md)
(predicted trigger preconditions), [testing/phases/09d_generator_design.md](../testing/phases/09d_generator_design.md)
(eager-absorption caveat).

## Symptom

Four synthetic shapes designed to trigger the post-type splitter
stages all fall to the `type` stage (or, in the simplest cases,
to no stage at all because FA converges in one pass). Per the
FAPassEvent sidecar:

```
same_type_dispatch_2:    type=1 (mark-type=0)
stored_fn_dispatch_2:    type=1 (mark-type=0)
setter_chain_2types:     type=1 (setter-of-setter=0)
                         (a flat inline variant produced
                          0 events — no stage fires)
```

Combined with the issue-003 recon (no pyc test triggers
mark-type or setter-of-setter; no V test triggers them either),
this means **no current shape — synthetic or real — triggers
any of `mark-type`, `setter-of-setter`, `mark-setter`,
`mark-setter-of-setter` in practice**.

## Root cause analysis

The splitter cascade in `extend_analysis` runs stage N+1 only if
stage N returned 0. Per the splitter code:

```c
analyze_again = split_ess_for_type(confluences, SPLIT_EDGES);
if (!analyze_again) analyze_again = split_ess_for_mark_type(confluences);
```

Both stages iterate the **same** `confluences` vector (collected
by `collect_type_confluences`). For mark-type to fire:

1. There must be a type confluence (so `confluences` is non-empty).
2. Stage 1 (`split_ess_for_type`) must return 0 — i.e., it found
   confluences but none qualified for its split rules (ES-formal
   or return-value).

The second condition is the hard one. Stage 1's qualifying check
(`fa.cc:3650-3656`):

```c
if (av->contour_is_entry_set) {
  if (!av->is_lvalue) {
    if (av->var->is_formal) analyze_again |= split_entry_set(...);
  } else {
    if (is_return_value(aav)) analyze_again |= split_entry_set(...);
  }
}
```

So stage 1 should SKIP confluences on CS-contour AVars (instance
variables) or on local lvals that aren't return values. CS-contour
confluences SHOULD pass through to mark-type.

But empirically, every shape with a polymorphic flow generates at
least one stage-1-qualifying confluence somewhere (a formal, a
return value), which fires stage 1 and pre-empts stage 2.

Iteration attempts so far:

1. **`same_type_dispatch`** (same shape as `13_setter_split.ir`):
   two T allocations with distinct-typed values in the same field,
   read via a polymorphic `peek(t)`. Type stage fires.

2. **`stored_fn_dispatch`**: stored function pointers — record T
   holds a `fn` field; allocate two T's with different closures;
   dispatcher `call_via(t)` does `t.fn()`. Type stage still fires.

3. **`setter_chain`** (inline): per type variant, allocate
   r1+r2 inline in main, do `r1.a = val; v = r1.a; r2.b = v`. No
   wrapper function. Result: zero events — FA converges in one
   pass. Each iteration's CSes are distinct and the chain's
   carried type is statically resolvable.

4. **`setter_chain`** (with wrapper): wrap the chain in a
   `chain(r1, r2)` function called twice with allocations of
   different value types. Type stage fires (chain's formals see a
   uniform R1/R2 type but the polymorphism in r1.a feeds a derived
   AVar that stage 1 catches).

5. **`missing_field_dispatch`** (targeting violation): two record
   types A, B with DISJOINT fields. Polymorphic reader reads field
   `fa` (exists on A, missing on B). Main calls reader with both
   instances. FA records the violation (violations=2 in the
   golden) but the type stage fires first because the reader's
   formal sees a confluence — the violation stage never runs.

6. **`vector_element_polymorphism`** (targeting setter via
   split_css). Two iterations:
   a. Multiple V instances each with a different-typed value
      written, polymorphic reader. Type-absorbed.
   b. Single V with multiple different-typed writes (within-CS
      element polymorphism). Zero events — FA converges in
      one pass.

7. **`vector_iterator`** (Phase C 7.7 follow-up — full iterator
   pattern). 🎯 **Setter stage fires.** The shape: vector type V,
   iterator record `It {vec, pos}`, method `next(self: It)` that
   reads `self.vec[self.pos]`, and consumer `consume(v)` that
   builds an It bound to v and calls next(it). Main allocates two
   V's with distinct-typed elements, calls consume on each.
   Result: 3-pass converge with `type=2, setter=1`. Pattern
   matches pyc's list-runtime behavior (alternating type+setter
   splits).
   This confirms split_css is reachable in synthetic IR — needed
   the full iterator-pattern stack (method dispatch with
   must_specialize on receiver, instance fields capturing the
   container, polymorphic indexing through self-stored state).

## Hypotheses (untested)

a. **The post-type stages genuinely require a shape where
   stage 1 has no qualifying confluence anywhere in the program.**
   That may be hard to construct in any IFA-source language;
   could be why no frontend triggers them.

b. **The post-type stages are reachable only via specific
   mark/setter propagation paths** that need particular runtime
   metadata not produced by current frontends. E.g., `mark_map`
   may need to be populated by something nothing currently calls.

c. **Some or all of the post-type stages are dead code** —
   transitional passes left from previous splitter designs that
   are now superseded. The fact that no currently-active workload
   hits any of them would support this.

d. **The violation stage specifically requires a shape that
   produces a violation AND has stage-1-unsplittable polymorphism
   simultaneously.** Hard to construct because a polymorphic-
   receiver shape almost always gives stage 1 a confluence to
   bite on. Splitting via SPLIT_DYNAMIC (the violation stage's
   path) might apply where SPLIT_EDGES (stage 1's path) can't —
   but engineering a shape that lands in the gap requires reading
   `split_edges` vs `split_entry_set` in detail.

## Verification plan

1. **Trace what stage 1's split actually does** on the
   `stored_fn_dispatch_2` shape. Instrument `split_entry_set` to
   log which AVar caused the split — that tells us where stage 1
   is finding its qualifying confluence and lets us design around it.

2. **Try a control-flow-merge shape**: `if (cond) t = t1 else t =
   t2; m = t.fn; m()` — pushes the polymorphism into a phi at the
   local level, which may avoid stage 1's formal/return-value
   conditions.

3. **Try a no-formal-no-reader shape**: all polymorphism inline in
   main, only on CS-contour AVars. If stage 1 still fires, the
   abstract-machinery hypothesis (a) is supported.

4. **Read `split_with_type_marks` directly** and figure out what
   inputs would actually cause it to make progress. If it requires
   marks that no current frontend or generator produces, that's
   evidence for (c).

## What this unblocks

- A working mark-type fixture would close the last `fa-converge`
  coverage gap for the stage (along with the four others currently
  uncovered: setter-of-setter, mark-setter, mark-setter-of-setter,
  violation).
- A finding of "mark-type is dead code" would justify removing
  ~50 LOC from `fa.cc` and simplify the splitter cascade.

## Why deferred

- Two iteration attempts (per 09d's "if 2-3 iterations don't fire,
  file as dead-code candidate") burned the time budget for this
  stage during Phase C.
- Phase C's other stages (setter, setter-of-setter, violation) are
  more likely to have real triggers and should be tried first.
- Returning to mark-type after exhausting other stages will give
  more empirical data about which shape characteristics matter for
  end-of-pass stages overall.

## Current shape fixtures (lock the absorbed behavior)

- `ifa/tests/synthetic/same_type_dispatch_2.synth` — `splits[type]=1`.
- `ifa/tests/synthetic/stored_fn_dispatch_2.synth` — `splits[type]=1`.
- `ifa/tests/synthetic/setter_chain_2types.synth` — `splits[type]=1`.
- `ifa/tests/synthetic/missing_field_dispatch.synth` —
  `splits[type]=1`, with `violations=2` recorded but the
  violation stage never runs.
- `ifa/tests/synthetic/vector_polymorphic_writes_2.synth` —
  0 events; FA converges in one pass. Locks the "primitive
  vec_set + vec_get without iterator pattern doesn't fire any
  splitter stage" finding.
- `ifa/tests/synthetic/vector_iterator.synth` — **type=2,
  setter=1** across 3 passes. The first synthetic shape to fire
  any post-type stage. Confirms split_css is reachable
  synthetically given the iterator pattern.

If someone fixes any post-type stage (e.g., by changing the
splitter cascade or by writing a shape that actually triggers
them), one of these goldens will shift and become the regression
marker.
