# 133 — Split a container CreationSet on its element type

**Status:** open, scoped. Blocks
[128](128-cs-identity-over-discriminates-vs-element-type.md)'s
start-merged posture (`PYC_CSDCPA1`), and is the last large item in
[129](129-plan-demand-driven-creation-set-splitting.md) step 4.

## Symptom

Under `PYC_CSDCPA1=2`, `tests/list_pop_insert.py` and
`tests/list_append_is_amortized.py`:

```
error: 'x' has mixed basic types:( int64 str )
```

The program builds three lists — `[1,2,3,4,5]`, `[10,20,30]`, and an
empty one later given `"x"` — mutates all three, and two of them end up
sharing a CreationSet whose element becomes `{int64, str}`. That union has
no representation (boxing is a project decision against, issues/018), so
compilation refuses.

`tests/splitter_cartesian_product.py` is the same cause in its soft form:
it compiles and runs correctly but cannot collapse `list.append`'s clones
on the shared contour, so three calls that were direct become dynamic
(`CALLS: 68/1 → 65/4`; see
[129](129-plan-demand-driven-creation-set-splitting.md)).

## Why it appears only now, and why nothing catches it

At the default this cannot happen: per-site CreationSet identity gives
every literal its own contour, so two lists never share one. The merge is
possible only under the start-merged posture — and specifically through
[132](132-arity-is-representation-not-provenance.md)'s rule that a
CreationSet which has lost its static arity **absorbs any arity**. That
clause is right (a dynamic list reads its length at runtime, so arity is
not its distinction) but it means the *element type* is then the only
thing left to distinguish two dynamic lists, and nothing is looking at it.

Measured, and the contour counts are otherwise exactly what the goal
statement wants — `list_pop_insert`, list CreationSets against distinct
element types:

| | list CS | element types | ratio |
| --- | --- | --- | --- |
| default | 10 | 4 | 2.5 |
| `PYC_CSDCPA1=2` | 3 | 3 | **1.0** |

So this is not over-merging in general. It is one specific pair that must
not have met.

**None of the existing routes catch it**, and three were measured:

- `PYC_CSELEM=1` (durable `elem_key`) and `PYC_CSELEM=3` (receiver shape)
  both still produce `mixed basic types` here. They key identity at
  **mint** time, when the element is unfilled by construction — the whole
  finding of [129](129-plan-demand-driven-creation-set-splitting.md)
  step 2c.
- `split_css` partitions a CreationSet by **setter equivalence**, which
  does not look at element types.
- `TYPE_CONFL` fires (it is in the `STAGES:` line) but acts on EntrySets
  and their AVars, not on a container's element channel.

## The work

**Trigger — and it is the strongest demand signal in the issue tracker.**
A container CreationSet whose element type is a union with *no
representation*: the mixed-basic-types condition. Codegen already detects
it, but at emission, far too late to act on. FA has the same information
at convergence in the element AVar's type. Nothing about this is a
heuristic — the distinction is not merely observable, it is one the
backend outright refuses to compile.

**Action.** Partition the CreationSet's creation points by the element
type each contributes and split. `split_css` is the primitive: it already
re-points `cs_map` across a partition and carries a ledger
(`cs_group_signature` → `ledger_find_cs`) so a re-derived split re-attaches
to the contour it first made instead of minting a fresh one each pass.
What is new is the partition function — by element contribution rather
than by setter equivalence.

### The blocker, found 2026-09-05 — and it is not the trigger

The obvious first move is ifa/109's pattern: raise the violation and let
the existing machinery act, exactly as its comment promises —

> *"Recording a violation here is what makes the EXISTING backward
> machinery do the work: `split_for_violations` (stage 5) splits the
> offending AVar... No annotation, no special case in codegen — FA simply
> has to know the constraint."*

There is even a real gap to close there. `collect_var_type_violations`
walks `cs->vars`, the **positional fields**, and checks each for
`mixed_basics` — but never the container's **generic element** AVar. So a
list whose element is `{int64, str}` raises nothing at that site.

**Adding it is inert, and it was measured: 0 / 16 / 19 failures at the
default, `PYC_CSDCPA1=2` and `=1`, identical with and without.** Reverted
rather than kept, but it is six lines and belongs in whatever lands here.

The reason it is inert is the actual work item.
`collect_violation_imprecisions` turns a violation into something
refinable by exactly two routes: `v->av->container` (null for a
CreationSet-contour AVar — nothing sets it for `cs->vars[i]` or the
element AVar), and `is_call_result(v->av)`, which wants a call PNode and
casts `v->av->contour` to an `EntrySet *`. **A violation whose AVar lives
on a CreationSet yields no imprecision at all, so stage 5 cannot see it.**

That is the same shape as
[129](129-plan-demand-driven-creation-set-splitting.md)'s finding about
`split_ess_setters`, which likewise acts only when the confluence sits on
an EntrySet — and it is why the `PYC_ESFORCS` experiment there was inert
too. **Three separate mechanisms now stop at the same boundary: an AVar
on a CreationSet contour has no route into the splitter.** That boundary,
not the trigger, is what this issue has to cross.

**Bounding, and the stop condition.** If the program genuinely builds a
heterogeneous list, no split helps: the union is real and today's refusal
is the right answer. So the split is *attempted*, and on failure control
falls through to the existing diagnostic. It must not loop, and the fan-out
must be capped. **If capping is what makes the numbers work, the trigger
is wrong** — the trigger is supposed to fire only on unions that have no
representation, which is a small and well-defined set.

**What it is NOT.** Not element-keyed identity at mint time — measured
above, twice, and it cannot work because the element is unfilled when the
decision is taken. This is a split *after* evidence arrives, which is what
the whole of [129](129-plan-demand-driven-creation-set-splitting.md)
step 4 established the analysis can do: every pass re-derives from bottom,
and `cs_map` is the only thing pinned.

*Verify:* `list_pop_insert` and `list_append_is_amortized` compile and run
under `PYC_CSDCPA1=1` and `=2`; `splitter_cartesian_product`'s `CALLS:`
returns to `68/1`; the default is bit-identical (this can only fire where
a CreationSet has several creation points, which `multidef=0` says never
happens at the default); corpus `check` under the flag improves on
`check__PYC_CSDCPA1_2__5e012d78+8450a439`'s 19 compile failures; and a
genuinely heterogeneous list still gets ONE contour and the existing
refusal, asserted by a new test.

## Pattern worth naming

This is the third mechanism in this area that exists but is gated on a
**static, frontend-supplied opt-in** where a demand signal is what is
needed:

| mechanism | opt-in today | wants |
| --- | --- | --- |
| per-constant contours | `__pyc_clone_constants__` → `clone_for_constants` | [131](131-demand-driven-constant-splitting.md) |
| method contours per receiver CS | `clone_methods_per_cs` (issue 045) | a demand signal |
| element-type separation | per-site CS identity, incidentally | this issue |

Each was invisible while per-site CreationSet identity was separating
things structurally. Removing that separation is what makes them ask to be
demand-driven — which is
[128](128-cs-identity-over-discriminates-vs-element-type.md)'s point. The
gating itself is tracked as
[134](134-remove-the-frontend-forced-split-opt-in.md).
