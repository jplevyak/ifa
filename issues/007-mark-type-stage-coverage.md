# Issue 007: post-type splitter stages not triggered by any shape

**Status:** partial — `setter` stage now reachable via synthetic
shapes (Phase 09 C 7.7). `violation` was briefly reachable via
the `nested_iterator` shape but that fixture exposed an FA-level
crash and was dropped; see [008-fa-crash-on-nested-iterator-shape.md](008-fa-crash-on-nested-iterator-shape.md).
Remaining gaps: `mark-type`, `setter-of-setter`, `mark-setter`,
`mark-setter-of-setter`, and (until 008 is fixed) `violation`.
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
   Result: 3-pass converge with `type=2, setter=1`.

8. **`iterator_copy`** (variant): adds a destination vector;
   `copy(src, dst)` reads via iterator and writes via
   set_index_object. Same outcome: `type=2, setter=1`. Targeted
   setter-of-setter but the chain didn't extend deep enough.

9. **`iterator_missing_field`** (variant): iterator yields
   elements that may or may not have a field. `consume(v)` reads
   `e.fa` — exists on A, missing on B; main allocates V's holding
   A and B respectively. Same outcome: `type=2, setter=1`. FA
   records `violations=2` but the violation stage never runs
   because setter makes progress first.

10. **`nested_iterator`** (attempted, first version): two-level
    vector iteration (single V type holding V's). Hit a clone-
    phase assertion ("mismatched field sizes") AND used raw
    primitive indexing. Removed.

11. **Documentation pass** (PRIMITIVES.md §13.12): identified
    that raw `prim_index_object` doesn't get per-call ES
    specialization, while method dispatch through
    `__getitem__` does. The "primitives don't split" finding
    explains why the iterator shapes worked at all (the method
    dispatch on `next` was the splitter handle) and why the
    inline-primitive shapes (`vector_polymorphic_writes`) didn't.

12. **`vector_polymorphic_writes`** (re-attempted with method
    dispatch via `install_subscript_methods` helper): 1 event
    (`splits[type]=1`), up from 0 — fix confirmed.

13. **`nested_iterator`** (re-attempted with method dispatch +
    distinct outer/inner vector types): 🎯 **Violation stage
    fires.** Three-pass converge with `type=2, violation=1`.
    Second post-type stage to be reached synthetically. Clone
    phase no longer trips. BUT: exposed an FA-level crash —
    intermittent segfault during fa-converge when the fixture
    runs alongside others. Filed as issue 008. The fixture is
    DROPPED for now; restore once issue 008 is fixed.

14. **`mark_dispatch_uniform_element`** (attempted, dropped):
    V with uniform int element type, two allocations, polymorphic
    `consume(v)`. Targeted mark-type. Fires only `type=2`. Also
    triggers an intermittent FA crash. Dropped — no unique
    coverage AND unstable.

15. **`triple_nested`** (attempted, dropped): three-level vector
    nesting. Same outcome as nested_iterator (`type=2,
    violation=1`) but duplicates its coverage and the deeper
    structure makes the crash more reproducible. Dropped.

**Final round summary**: pushed hard on mark-type / setter-of-
setter / mark-setter / mark-setter-of-setter / violation using
the method-dispatch pattern. Of those five, only violation was
ever reached (briefly, before issue 008 forced dropping its
fixture). The four other stages remain unreached after ~15
shape iterations across multiple sessions.

The retry round demonstrates: **setter is reachable via several
iterator-pattern variants, but the remaining post-type stages
(mark-type, setter-of-setter, mark-setter, mark-setter-of-setter,
violation) all stay blocked.** The iterator pattern absorbs
polymorphism via setter; deeper cascades would require structural
patterns that the analysis hasn't yet been engineered to expose.

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
  **type=1** (was 0 events before the method-dispatch fix).
- ~~`ifa/tests/synthetic/nested_iterator.synth`~~ — re-introduced
  with violation stage firing, then DROPPED due to FA crash
  (issue 008). Restore once 008 is fixed.
- `ifa/tests/synthetic/vector_iterator.synth` — **type=2,
  setter=1** across 3 passes. The first synthetic shape to fire
  any post-type stage.
- `ifa/tests/synthetic/iterator_copy.synth` — **type=2,
  setter=1**. Iterator + dst write; targeted setter-of-setter,
  absorbed by setter.
- `ifa/tests/synthetic/iterator_missing_field.synth` —
  **type=2, setter=1**, with violations=2 recorded. Iterator
  yielding records of disjoint types; targeted violation,
  absorbed by setter.

If someone fixes any post-type stage (e.g., by changing the
splitter cascade or by writing a shape that actually triggers
them), one of these goldens will shift and become the regression
marker.
