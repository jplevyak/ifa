# Issue 007: post-type splitter stages not triggered by any shape

**Status:** partial — `setter` reachable via iterator shapes
(Phase 09 C 7.7); `violation` reachable via `nested_iterator`
(restored June 2026 after issue 008 closed as "could not
reproduce"). Remaining gaps: `mark-type`, `setter-of-setter`,
`mark-setter`, `mark-setter-of-setter` (4 of 7 splitter stages
uncovered, down from 5 of 7 when filed).
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

## Follow-up — June 2026

With [issue 008](008-fa-crash-on-nested-iterator-shape.md)
closed as "could not reproduce," the `nested_iterator.synth`
fixture is back in the suite and the **violation** stage is
covered again. Current stage tally across all 17 fa-converge
fixtures:

| Stage                 | Reached? | Fixtures                                                                      |
|-----------------------|----------|-------------------------------------------------------------------------------|
| type                  | ✓        | 13 fixtures, broadly                                                          |
| **mark-type**         | ✗        | none — 0/17                                                                   |
| setter                | ✓        | 3 (`iterator_copy`, `iterator_missing_field`, `vector_iterator`)              |
| **setter-of-setter**  | ✗        | none — 0/17                                                                   |
| **mark-setter**       | ✗        | none — 0/17                                                                   |
| **mark-setter-of-setter** | ✗    | none — 0/17                                                                   |
| violation             | ✓        | 1 (`nested_iterator`)                                                         |

3 of 7 splitter stages reached; 4 remain uncovered.

### Verification plan step 4: read `split_with_type_marks` directly

Per the issue's verification plan #4, read the four uncovered-
stage splitters and figure out what inputs would actually cause
them to make progress. Structural finding follows.

#### `split_ess_for_mark_type` (the mark-type stage)

```cpp
// fa.cc ~3535
static int split_with_type_marks(AVar *av, int fdynamic) {
  Accum<AVar *> acc;
  build_type_marks(av, acc);
  Vec<AVar *> confluences;
  collect_es_marked_confluences(confluences, acc, SPLIT_TYPE);
  // ... same formal/return-value qualifier as stage 1 ...
  if (av->contour_is_entry_set) {
    if (!av->is_lvalue) {
      if (av->var->is_formal) ...split with SPLIT_MARK...
    } else {
      if (is_return_value(aav)) ...split with SPLIT_MARK...
    }
  }
}
```

Structure mirrors stage 1 (`split_ess_for_type`) but uses
mark-collected confluences instead of raw confluences. The two
confluence sets differ when an AVar receives the *same base
type* from multiple sources but those sources carry different
`mark_map` entries — i.e., the polymorphism is at the "same type
from distinct origins" level, not the "different types" level.

Per IFA.md §6.2, this is the **"recursion-meets-polymorphism"**
case: a recursive function called from two distinct contexts
with the same argument type, where the analysis would benefit
from splitting per-call-site even though the call-site types
match. None of the 15 prior iteration attempts tried a
recursive-polymorphic shape, because the natural shape ("call
the same fn twice with different types") gives stage 1 a
qualifying confluence on the formal.

#### `split_for_setters_of_setters`

Cascade order: setter fires first; setter-of-setter only runs
if setter returned 0. Iterator shapes generate setter splits at
the first hop (the iterator record's `vec`/`pos` write graph),
so setter absorbs the polymorphism before setter-of-setter sees
it. To trigger setter-of-setter, we'd need a write graph where
the first hop is uniform but the *second* hop is polymorphic —
e.g., a setter chain where the intermediate's setter list is
uniform but its setters' setters diverge.

#### `mark-setter` and `mark-setter-of-setter`

Same relationship as mark-type vs type: they're the
mark-augmented versions of setter and setter-of-setter, only
firing when the unmarked versions found no progress. To reach
them, the setter graph would need a same-base-type-distinct-
origin polymorphism (the recursion analog at the setter level).

### Hypothesis (c) "dead code" — supporting evidence

- **Zero fa-converge fixtures trigger any of the 4 stages.**
- **No pyc test program triggers them** (issue 003 §empirical recon).
- **No V test program triggers them** (issue 003 §empirical recon).
- **15+ iteration attempts** using polymorphic-receiver shapes,
  iterator shapes, missing-field shapes, setter chains, control-
  flow merges, and nested vectors all failed to reach any of
  the 4 stages.
- The required pattern for `mark-type` (recursion-meets-
  polymorphism with same-type-distinct-origin confluences) is
  structurally rare; if it ever fires, the splitter cascade
  already provides simpler stages (type, setter, violation) that
  would usually catch it first.

This is *suggestive* but not definitive — absence of evidence
isn't evidence of absence. A patient targeted attempt at a
recursive-polymorphic shape would either confirm hypothesis (c)
by failing OR refute it by triggering mark-type.

### Recommended next move

Two options, in increasing scope:

1. **One targeted recursive-polymorphic shape attempt.** Build
   a shape like:
   ```
   def recurse(it):
     if (cond): return it
     else: return recurse(advance(it))
   main:
     recurse(it_int); recurse(it_float)
   ```
   Same instrumentation as Phase C: drop it into
   `ifa/testing/ir_shapes.cc`, bless the fa-converge golden,
   check whether `splits[mark-type]` appears.

   - If yes → close mark-type's gap; same approach probably
     applies to mark-setter / mark-setter-of-setter.
   - If no → strong evidence for hypothesis (c). Move to option 2.

2. **Code-archeology pass for dead-code candidate removal.**
   Confirm hypothesis (c) by reading the splitter call chain
   in depth, comparing to the paper §5/§6 prescriptions, and
   filing a `[ ]` CLEANUP item to remove the unreached stages
   if they truly correspond to abandoned design directions.
   Removing ~50 LOC per stage = ~200 LOC simplification.

Either option is bounded work. The 15+ failed attempts to date
were all on the polymorphic-call axis; option 1 explores the
recursion axis, which is the one IFA.md §6.2 attributes to
mark-type. If it also fails, option 2 is the principled next
step.

## Follow-up — June 2026: diagnostics + traversal-cap fix

Verification-plan step 1 (per-pass splitter diagnostics) and a
fix to `build_type_marks`'s closure traversal landed June 2026.
Empirical findings against all 17 fa-converge fixtures:

### What `--log-flags s` reveals

Added `LOG_SPLITTING` enrichments at every splitter decision
point in `fa.cc` (`[confluence]`, `[stage1]`, `[ses]`,
`[stage2-marks]`, `[btm-seed]`, `[btm]`) and a `--log-flags`
option on `ifa-test`. Output lands at `log/log.s` and is the
single source of truth for which splitter conditions short-
circuit on which AVar in which pass.

### The 2-hop traversal cap

`build_type_marks` had `for (AVar *x : acc.asvec)` walking
backward then forward of the starting AVar. The range-for
captures `end()` at loop entry, so `acc.add(y)` calls during
iteration appended elements never visited. Effective reach:
1 hop back from `av`, then 1 hop forward from each of those.
The comment "collect all contributing nodes" was aspirational;
the loop never did transitive closure.

Concrete impact: for `same_type_dispatch_2`'s `av33` (the
polymorphic `data` field), the closure had 5 AVars and zero of
them had `gen` set — the literal-constant AVars (the only ones
with non-null `gen`) were further upstream and never reached.
`build_type_marks` produced zero marks across every fixture.

Fix (one line): index-based loop reading `acc.asvec.n` per
iteration. With the fix, `av33`'s closure is now 13 AVars, the
int32 and float64 generators are reached, and both keys mark
through to `av33` at dist=5. All 17 fa-converge goldens
remained byte-identical after the change — the analysis result
didn't shift because the resulting marked-confluences land
where stage 2's qualifier rejects them (next finding).

### The qualifier mismatch (next blocker)

With marks now propagating, `collect_es_marked_confluences`
finds 1 marked-confluence per stage-2 invocation in most
fixtures. But the qualifier in `split_with_type_marks`
requires the marked-confluence to be ES-contour AND
(`is_formal` OR `is_return_value`):

```cpp
if (cav->contour_is_entry_set) {
  if (!cav->is_lvalue) {
    if (cav->var->is_formal) { split_entry_set(...); }
  } else {
    if (is_return_value(aav)) { split_entry_set(...); }
  }
}
```

Across all 17 fixtures, the marked-confluences land on either:

- **CS-contour fields** (e.g., `same_type_dispatch_2`'s
  `av33 = T.data`) — the polymorphic merge happens at the
  record field, not at a function boundary.
- **ES-contour non-formal locals** (e.g., `vector_iterator`'s
  `av63 = elt` returned from `vec_get`).

Neither qualifies for stage 2's split action. Mark-type
returns 0 on every call.

### Why current shapes don't produce ES-formal marked-confluences

`collect_es_marked_confluences` adds `av` to the result when
`av->backward` includes some `x` such that
`different_marked_args(x, av, 1)` returns 1. With basis=0
(the call-site default), this fires when `av` has a mark key
that `x` doesn't continue at the expected distance — i.e.,
`av` aggregates marks from multiple sources where no single
source covers all keys.

That pattern arises naturally at fields and merge points
(av33 receives both int32 and float64 writes from distinct
setter sites). It does NOT arise at formals AFTER stage 1's
split — because each split copy's formal sees a single
incoming type from its single split call-edge group. Stage 1
has already absorbed the formal-level polymorphism by the
time stage 2 runs.

For mark-type to fire, we'd need a shape where:
- Stage 1's `split_entry_set` returns 0 (it short-circuits on,
  e.g., `non_rec_edges == 1 && nedges != do_edges.n`), AND
- The unsplit formal accumulates marks from multiple sources
  whose key-sets are disjoint, AND
- Those sources are visible in the formal's `backward` at the
  time stage 2 runs.

The combination is specifically: a recursive function called
from a SINGLE polymorphic site where stage 1's short-circuit
prevents the per-call split. The recursive edges (compatible
with the unsplit ES) coexist with one new-type non-recursive
edge → stage 1 short-circuits → stage 2 takes over with mark
distances distinguishing the recursion paths.

This is the precise shape to try as the next experiment.

### Recursive-shape experiment (June 2026)

Added `mark_recursive_single_site` to `ifa/testing/ir_shapes.cc`:
record T with polymorphic `data` field, recursive method
`walk(self)` that reads `self.data` and recursively dispatches
`walk` on self, returning the read value. Two call sites from
main with distinct-typed t1/t2 to force stage 1 to fire.

Result: 1 pass, `splits[type]: 1`. Stage 1 splits walk per-CS
at the formal. **Mark-type still doesn't fire.** Stage 2's
closure on the polymorphic field is now 15 AVars (with the
traversal fix); 15 are marked; one marked-confluence is found
— but it's again CS-contour (the anonymous merge), skipped by
the qualifier.

The closure includes `v` (the local read of `self.data`, an
ES-contour return value) and the recursive ES's locals. None
of them are marked-confluences because each has only ONE
backward source carrying both mark keys — no disjunction, no
confluence.

### Why ES-formal/return marked-confluences don't arise

`collect_es_marked_confluences` adds `av` when some backward
predecessor `x` has a mark-key set that DIFFERS from `av`'s
(via `different_marked_args(x, av, 1)`). This fires at MERGE
points where multiple sources contribute DISJOINT subsets of
keys.

In current shapes the merge points are CS-contour fields:
multiple sequential writes (`set_field(t, "data", int_const)`
then `set_field(t, "data", float_const)`) create distinct
backward AVars at the field — one per write — each carrying a
disjoint key set. Hence the CS-contour field becomes a marked-
confluence.

ES-contour formals/returns DON'T naturally merge with disjoint
keys:
- After stage 1's split, each split copy's formal has ONE
  backward edge (or recursive edges carrying the same CS).
- A return value receives from `move(v, ret)`; its backward is
  a single AVar (v), which carries all the keys it inherited
  from the merged field.
- Polymorphism enters the function via the formal as a single
  CS; flows through field reads (which carry all keys); exits
  via the return (single key set).

For an ES-formal to be a marked-confluence, we'd need stage 1
to NOT split a polymorphic call (so multiple backward edges
with disjoint key sets remain). Stage 1's short-circuit
condition `non_rec_edges == 1 && nedges != do_edges.n`
requires the non-recursive edge to be incompatible with the
ES — which only happens if the ES's current type-expectation
differs from the non-rec arg, and that gap evolves DURING the
analysis (via transformation in the recursion path). No source-
level primitive in `ir_shapes` constructs this transformation.

### Final structural finding

**Two compounding structural blockers prevent mark-type from
firing on any current shape:**

1. The 2-hop traversal cap in `build_type_marks` — FIXED
   (one-line index-based loop); marks now propagate properly.

2. The qualifier in `split_with_type_marks` requires the
   marked-confluence to be at an ES-contour formal/return.
   But mark-confluences in shape-language-expressible programs
   land at CS-contour merge points (record fields). To produce
   an ES-formal marked-confluence requires either:
   - A stage-1 short-circuit on a polymorphic call (which
     requires within-recursion type transformation that
     `ir_shapes` can't currently express), OR
   - A frontend primitive that injects abstract-type CSes
     into `out->type` flow (e.g., reflective `type(x)` or
     class-as-value patterns).

The same logic applies symmetrically to mark-setter and
mark-setter-of-setter — they have the same structural
position relative to setter / setter-of-setter that mark-type
has to type.

### Status

3 of 7 splitter stages reached (type / setter / violation).
4 remain uncovered (mark-type / setter-of-setter /
mark-setter / mark-setter-of-setter). The mark-* uncovered
stages now have a precise structural explanation: the
qualifier rejects every marked-confluence that current
shape-language patterns produce. The traversal-cap fix is
landed as a standalone correctness improvement (commit
`d37fec5`). `mark_recursive_single_site.synth` stays in the
suite as the structural-evidence fixture — if anyone ever
adjusts the qualifier or adds a frontend primitive that
produces abstract-type CSes in flow, this fixture's golden
should shift.
