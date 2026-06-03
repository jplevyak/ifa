# Issue 007: post-type splitter stages not triggered by any shape

**Status:** open (empirical gap surfaced during Phase 09 C 7.4
and C 7.5).
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

## Hypotheses (untested)

a. **The mark machinery genuinely requires a shape where stage 1
   has no qualifying confluence anywhere in the program.** That
   may be hard to construct in any IFA-source language; could be
   why no frontend triggers it.

b. **Mark-type is reachable only after `propagate_type_marks` /
   `build_type_marks` populates `mark_map`s.** Maybe the empty
   mark_map path in `split_with_type_marks` (line ~3470) returns 0
   even for legitimate cases, suppressing the stage. Worth tracing
   what `split_with_type_marks` actually does when called on the
   confluences our shapes produce.

c. **Mark-type is dead code** — a transitional pass left from a
   previous splitter design that's now superseded by the
   subsequent setter / violation stages. The fact that no
   currently-active workload hits it would support this.

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

If someone fixes any post-type stage (e.g., by changing the
splitter cascade or by writing a shape that actually triggers
them), one of these goldens will shift and become the regression
marker.
