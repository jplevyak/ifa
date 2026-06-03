# Issue 007: mark-type splitter stage not triggered by any shape attempted

**Status:** open (empirical gap surfaced during Phase 09 C 7.4).
**Affects:** `ifa/analysis/fa.cc:split_ess_for_mark_type` and
related `split_with_type_marks` machinery; `IFA.md` splitter section.
**Related:** [issues/003-fa-converge-determinism.md](003-fa-converge-determinism.md)
(empirical stage recon), [testing/phases/09c_splitter_triggers.md](../testing/phases/09c_splitter_triggers.md)
(predicted trigger preconditions), [testing/phases/09d_generator_design.md](../testing/phases/09d_generator_design.md)
(eager-absorption caveat).

## Symptom

Two synthetic shapes (`same_type_dispatch`, `stored_fn_dispatch`)
designed to trigger the `mark-type` splitter stage both fall to
the `type` stage instead. Per the FAPassEvent sidecar:

```
same_type_dispatch_2:  type=1 (mark-type=0)
stored_fn_dispatch_2:  type=1 (mark-type=0)
```

Combined with the issue-003 recon (no pyc test triggers mark-type
either; no V test triggers it), this means **no current shape —
synthetic or real — triggers the mark-type stage in practice**.

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

Two iteration attempts (Phase 09 C 7.4):

1. **`same_type_dispatch`** (same shape as `13_setter_split.ir`):
   two T allocations with distinct-typed values in the same field,
   read via a polymorphic `peek(t)`. Type stage fires (probably on
   peek's formal or some derived AVar).

2. **`stored_fn_dispatch`**: stored function pointers — record T
   holds a `fn` field; allocate two T's with different closures;
   dispatcher `call_via(t)` does `t.fn()`. Type stage still fires.

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

## Current shape fixtures (lock the type-absorbs-it behavior)

- `ifa/tests/synthetic/same_type_dispatch_2.synth`
- `ifa/tests/synthetic/stored_fn_dispatch_2.synth`

Both have `.fa-converge.expected` goldens showing `splits[type]=1`.
If someone fixes mark-type (e.g., by changing the splitter
cascade or by writing a shape that actually triggers it), the
goldens will shift and become the regression markers.
