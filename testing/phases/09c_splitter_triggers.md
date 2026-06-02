# Phase 09 — Phase A.3: Splitter-stage trigger preconditions

For each of the 7 splitter stages (one already triggered by pyc
tests, six gaps), what IF1 shape causes it to fire. Sources:
`ifa/analysis/fa.cc:extend_analysis` (the cascade) +
`split_*` / `collect_*` / `compute_setters` functions.

This is the input to Phase B: it lists what each `IRShape::*`
needs to construct in order to exercise its target stage.

---

## The cascade

`extend_analysis()` runs stages in this order. Stage N+1 fires
only if all of stages 1..N returned 0 this pass:

1. **TYPE** — `split_ess_for_type` on type confluences.
2. **MARK-TYPE** — `split_ess_for_mark_type` on type confluences
   that survived stage 1.
3. **SETTER** — `compute_setters(AKIND_TYPE)` + `split_for_setters`
   which has three inner paths: `split_ess_setters`,
   `split_ess_setters_marks`, `split_css`.
4. **SETTER-OF-SETTER** — `split_for_setters_of_setters` (loops
   `compute_setters(AKIND_SETTER)` + `split_for_setters` to
   fixed point).
5. **MARK-SETTER** — wrapper around `compute_setters(AKIND_MARK)`
   + `split_for_setters`. Different `AKIND` from stage 3.
6. **MARK-SETTER-OF-SETTER** — same as stage 4 but inside the
   mark-based loop.
7. **VIOLATION** — `split_for_violations` on collected
   `type_violations` from this pass.

## Stage 1 — TYPE (already covered)

### Precondition

`collect_type_confluences` finds AVars where some backward
(predecessor) AVar's `out->type` differs from this AVar's
`in->type`. I.e., **the same Var receives different types from
different incoming flows**.

`split_ess_for_type` then splits IF:
- `av->contour_is_entry_set` (the confluence is in an ES, not a CS), AND
- if `!av->is_lvalue`: `av->var->is_formal` (a function formal), OR
- if `av->is_lvalue`: `is_return_value(aav)` (the formal's return-slot).

### IF1 shape that triggers it

A function called from ≥2 sites with different argument types:

```text
(fun foo (x) :rets (r) :body (move x r))
;; called from main:
(send foo 1)         ; int
(send foo "hello")   ; string
```

`foo`'s `x` formal AVar has two backward flows (int, string) →
type confluence → `is_formal` → split.

Already exercised by pyc tests and `02_splitter.ir`.

## Stage 2 — MARK-TYPE

### Precondition

Type confluence exists (stage 1's collector) **but stage 1 didn't
split it**. Then `split_with_type_marks` looks at the AVar's
`mark_map` (per-CreationSet "mark" annotations) and splits based
on those marks.

The "mark" mechanism (`build_type_marks` etc.) distinguishes
multiple CreationSets of the *same type* by propagating which
allocation site they came from. Type stage can't tell them apart
(same type); mark-type can.

### IF1 shape that triggers it

- Two allocations of the same record type.
- Both flow into the same field (a CS-level instance var, not an
  ES formal).
- Then the value is read; the read site can't be split by stage
  1 because the *types* match.

```text
(type Pair :kind RECORD :has (val))
(send @primitive @new Pair => a)      ; CS a
(send @primitive @new Pair => b)      ; CS b
(send @operator a @setter #val 1 => _)  ; same type int
(send @operator b @setter #val 2 => _)  ; same type int
;; somewhere the dispatch wants to distinguish a-elements from b-elements
;; using mark info
```

**Likely trigger:** any pattern where the type is uniform but CS
identity matters for downstream resolution. Concrete example:
double-dispatch over two same-typed-but-distinct objects.

## Stage 3 — SETTER

### Precondition

Type confluences exist but stages 1 + 2 didn't split. Then
`compute_setters(AKIND_TYPE)` walks back from each confluence
AVar through `av->backward` to find setters (sites that wrote to
this AVar's location). `split_for_setters` then has THREE inner
paths:

**3a. `split_ess_setters`**: split the ES containing the confluence
if either:
- `av->contour_is_entry_set && !av->is_lvalue`, OR
- `is_lvalue && unique_AVar(av->var, av->contour)->var->is_formal`.

**3b. `split_ess_setters_marks`**: same but using `setter_marks`.

**3c. `split_css`** (the pyc-triggered path): walks `cs_map` —
AVars representing per-CS instance variables. Splits the CSes
when setters' eq-classes differ.

### IF1 shape that triggers it

Per the recon, pyc programs hit path **3c (split_css)** via list
runtime patterns:

- Multiple list allocations.
- Each list's element vector (a CS-relative AVar with `cs_map`
  populated) gets values of distinct types written via setter SENDs.
- The setters' `container->out` overlaps with the AVar's `cs_map`.

This is what `list_multiply.py` (4 lines!) produces. Reproducing
it in hand-written `.ir` requires:
- `@vector` primitives for the element vector.
- The container/cs_map plumbing the runtime sets up.

Easier (paths 3a/3b) shape: a polymorphic field that's read via a
non-formal path so type stage doesn't pre-empt:

```text
;; setter not on ES-formals, but on something stage 1 considers
;; ineligible (e.g. lvalue formal but constraint blocks split)
```

The exact shape that hits 3a/3b without first hitting 1/2 is
narrow — needs constraints on `is_lvalue` and the formal/return-
value disjunction. Empirically: pyc doesn't hit 3a/3b, just 3c.

## Stage 4 — SETTER-OF-SETTER

### Precondition

`split_for_setters_of_setters` loops `collect_cs_setter_confluences`
+ `compute_setters(AKIND_SETTER)` + `split_for_setters`. The
trigger is **cascading setter chains**: field A's setter writes a
value that becomes field B's setter input, and so on.

Reads `av->forward` (not `backward` like AKIND_TYPE) — looks
forward through the setter chain.

### IF1 shape that triggers it

```text
;; Two record types, field-to-field chain:
(type R1 :kind RECORD :has (a))
(type R2 :kind RECORD :has (b))
;; main:
(send @primitive @new R1 => r1)
(send @primitive @new R2 => r2)
(send @operator r1 @setter #a 1   => _)
(send @operator r1 @period #a     => v1)     ; read a
(send @operator r2 @setter #b v1  => _)      ; setter chain: r1.a → r2.b
;; Two distinct-typed flows through r1.a, then through r2.b, etc.
```

The chain has to produce a setter-of-setter confluence that the
preceding stages couldn't resolve.

## Stage 5 — MARK-SETTER

### Precondition

Same as stage 3 but using `AKIND_MARK` to compute setters via
mark-based identity rather than type-based. Looks at AVar's
`mark_map` (set up by `build_type_marks`).

### IF1 shape that triggers it

Combination of stage 2's mark-driven distinction with stage 3's
setter-driven dispatch:
- Two allocations of the same type (so type stage skips).
- Field values written from setters whose marks differ.
- Downstream dispatch depends on the mark-distinguished field.

Hard to construct cleanly without working through the mark
propagation by hand.

## Stage 6 — MARK-SETTER-OF-SETTER

### Precondition

The setter-of-setter loop's mark variant: chains of setters where
identity is tracked via marks rather than types.

### IF1 shape

Even more compound: stage 4's chain + stage 5's mark distinction.
A 2-deep setter chain with same-type-different-mark values.

## Stage 7 — VIOLATION

### Precondition

`split_for_violations` looks at `type_violations` accumulated
during the pass. For each violation `v`, `collect_violation_imprecisions`
collects:
- `v->av->container` if its `out->n > 1` (the receiver of a
  failing call has multiple possible types).
- If `is_call_result(v->av)`: any AVars from dispatched edges that
  match the violation type.

Then `split_ess_for_type(imprecisions, SPLIT_DYNAMIC)` tries to
split. The `SPLIT_DYNAMIC` flag is the differentiator vs stage 1.

### IF1 shape that triggers it

A polymorphic call where *some* receiver types lack the method,
producing a runtime-dispatch violation, but splitting the caller
makes each context monomorphic.

```text
(type A :kind RECORD :has (only_in_A))
(type B :kind RECORD :has (only_in_B))
;; A polymorphic receiver — some flows have only_in_A, others only_in_B.
;; A site calls receiver.only_in_A which type-errors for B.
;; Splitting the caller so each context sees only A or only B
;; eliminates the violation.
```

Concrete: function `foo(x): x.method()` called with both A
(with method) and B (without method).

## Per-stage hand-craftability ranking

| Stage | Trigger complexity | Hand-craftable in .ir? |
|-------|---------------------|--------------------------|
| 1 type | trivial | yes — `02_splitter.ir` |
| 2 mark-type | medium | doable — same-type CSes + dispatch |
| 3a/3b setter (ES path) | medium-hard | narrow; ES-formal disjunction is tight |
| 3c setter (CS path / split_css) | hard | needs vector/element plumbing |
| 4 setter-of-setter | medium | setter chain across two records |
| 5 mark-setter | hard | mark + setter compound |
| 6 mark-setter-of-setter | very hard | mark + chain + setter compound |
| 7 violation | medium-hard | polymorphic call with one type missing method |

## Coverage targets for the IR generator

Based on the rankings:

**Easy wins (start here, layer 3 first cuts):**
- Stage 1 (already done).
- Stage 2 (mark-type): two same-typed allocs + double-dispatch.
- Stage 4 (setter-of-setter): two-record setter chain.
- Stage 7 (violation): one-of-two-receivers-missing-method.

**Hard but valuable (after layer 2 is stable):**
- Stage 3c (split_css): vector/element pattern. The pyc list
  runtime does this naturally; generator needs to construct it
  explicitly.
- Stage 5 (mark-setter): mark + setter compound.

**Dead-stage candidates (no current trigger, deeply compound):**
- Stage 3a/3b (ES-path setter): empirically unused; the
  conditions are narrow. Verify by attempt before declaring dead.
- Stage 6 (mark-setter-of-setter): compound of compound. May be
  dead code that exists only because the structure mirrored stage 4.

If after the generator is built any of these stages remains
unreachable despite construction targeting it, that's strong
evidence for dead-code removal. File a separate issue.

## Recommendations for Phase B

1. **The generator API needs primitives for**:
   - `@new` allocations (multiple, distinct CSes).
   - `@setter`/`@period` on RECORD fields.
   - `@vector` (for stage 3c).
   - `@operator` method calls (for dispatch).
   - Reply / return primitive (for fun bodies).

2. **Shapes can be composed**: a "double-dispatch on same-type"
   shape (stage 2) reuses "two allocations of same type" + "call
   into shared formal." A "setter chain" (stage 4) reuses
   "@setter into field A" + "@period from field A" + "@setter
   into field B."

3. **Layer 2's API should expose CS identity explicitly** —
   stages 2, 5 care about mark/CS distinctions, not just type.
   The generator must let a fixture say "give me two CSes of the
   same type at distinct allocation sites" without forcing
   stage 1 to absorb the distinction first.

4. **Phase C step 8** (generator-driven splitter coverage) should
   target stages in this order: 4 → 2 → 7 → 5 → 3c → 6 → 3a/3b.
   Each subsequent stage builds on patterns from earlier ones.

Next: Phase B — design the generator API with these constraints
in hand.
