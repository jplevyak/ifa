# 040 — An empty list's never-taken loop body gets forced into type-checking (and fails) only when another list also exists in the program

**Status: CLOSED** — FIXED 2026-07-15 (`65e1628f`) by
[045-receiver-cs-method-cloning.md](045-receiver-cs-method-cloning.md)
(hard per-constant contours for clone_methods_per_cs classes +
per-constant instance CSs + the PER_CS_RECEIVER precision split
stage). The repro below compiles clean and runs correctly; committed
as `tests/empty_list_print.py`. The mechanism trace that led there
follows.

**Caveat found 2026-07-19**: this fix's scope is narrower than it
looks — adding so much as a no-op comparison (`if key < 0: pass`) to
`list.__getitem__` reopens the *exact* failure on this issue's own
`tests/empty_list_print.py` repro, with no other change to the
program. See
[052](../052-shared-method-branch-reopens-empty-list-fragility.md).

## Complete mechanism (2026-07-15)

The scheduling "leak" is a chain through `list.__str__`'s loop
machinery, each link verified empirically:

1. `len(self)` over a tuple-list CS IS a per-CS constant
   (`P_prim_len` → `make_size_constant_type(cs->vars.n)`): 0 for
   `k`, 2 for `b`. Per-clone constants exist.
2. The `range(0, len(self))` construction merges both callers'
   constants in `range.__new__`'s single contour (j = {0, 2} →
   constant cap → generic int64). Verified fixable: flagging the
   ctor params `__pyc_clone_constants__` (plus a REAL LATENT GAP
   fixed along the way — `gen_class_pyda` never propagated
   `clone_for_constants` from `__init__` params to the synthesized
   `__new__` wrapper formals, so constant-cloning ctor params
   silently didn't work through a constructor) produces
   per-constant `__new__` contours and per-constant range CSs.
3. **But that is still not enough**: `__pyc_more__`/`__next__`
   merge into ONE EntrySet over the union of receiver range-CSs
   (both are type `range`; the matcher/splitter has no reason to
   separate them — no violation arises *there*), and `__next__`'s
   `self.i += self.s` writes through the shared ES into EVERY
   receiver CS's `i` field — widening the empty clone's `i` to
   generic int64 too. `i < j` then cannot fold even with perfectly
   separated per-constant CSs.
4. The standalone case (`k=[]` alone) works only as a
   self-consistent least fixpoint: the loop body is never
   scheduled, so `__next__` is never analyzed, so `i` stays {0} and
   `0 < 0` folds false. Adding ANY other list breaks the
   equilibrium via link 3.

Also ruled out this round: `creation_point`'s split-parent CS reuse
(fa.cc ~434, the one explicit cross-contour CS-sharing rule) —
disabling it entirely changes nothing for this repro.

**Consequence:** the fix is receiver-CS-directed method cloning as a
PRECISION move (split `__pyc_more__`/`__next__` per receiver CS even
though no type violation occurs in them) — i.e. the deferred
issue-033 splitter redesign, not a local patch. Constant-flag
band-aids were tried and reverted (clone pressure, zero corpus
effect — see `__pyc__/05_builtins.py`'s range note). Until then, the
honest mitigation is codegen-side: emit a trap for no-type branches
instead of undeclared-label C errors (issue 043's surviving
option 1).

The original filing and the 2026-07-13 narrowing follow.

**Status (2026-07-13):** open, root cause substantially narrowed —
not fixed. The title/hypothesis from the original filing ("shared
clone") is **superseded**: traced further, confirmed the empty
list's method calls run in their own, properly-monomorphic
EntrySet, not one merged with the non-empty list's. See "Root cause
(narrowed)" below for the corrected mechanism.
**Affects:** FA's per-EntrySet reachability/liveness during
analysis (`ifa/analysis/fa.cc`, `collect_var_type_violations` and
whatever schedules `PNode`s for analysis — not further traced).
**Related:** checked against and ruled out as the same bug:
[018-dict-mixed-key-types-boxing-failure.md](../../../issues/018-dict-mixed-key-types-boxing-failure.md)
(pyc-frontend issue 018 — a genuinely different `ATypeViolation_kind`,
`BOXING` not `NOTYPE`: that's two concrete, incompatible types
unifying into one shared `dict`/`set` method body; this is one
receiver's element type never being computed at all, not two types
colliding); the CLOSED, same-numbered-by-coincidence
`closed/018-v2-loop-after-undef.md` in *this* directory (a
retired-codegen-backend bug, `cg_normalize_v2.cc`/
`cg_ir_v2_emit_llvm.cc` no longer exist in the tree — coincidental
overlap in which runtime method it happened to exercise,
`list.__str__`'s `self[k].__repr__()`, nothing else in common).
Plausibly the SAME underlying class of problem as
[025-intra-function-union-narrowing.md](../025-intra-function-union-narrowing.md)
(FA's specialization/narrowing being imprecise for cases outside its
one well-handled shape, clone-per-call-boundary-type) and
[033-splitter-non-idempotent-divergence.md](../033-splitter-non-idempotent-divergence.md)
/ [032-fa-survey-findings.md](../032-fa-survey-findings.md) (the
broader "FA's iterative splitting/cloning has precision gaps"
family) — not confirmed to share a root cause with either, but the
shape (a per-receiver CFG path that's reachable for one clone
affecting analysis of a DIFFERENT clone of the same function) fits
that family better than any single-bug explanation.

## Symptom (unchanged from original filing)

```python
b = [2, 3]
print(b)

k = []
print(k)
```

Compiles with `expression has no type` (`ATypeViolation_kind::NOTYPE`)
pointing into `__pyc__/04_sequence.py`'s `list.__getitem__`
(`index_object`) and `list.__str__` (`self[k].__repr__()`, `k` here
being that method's OWN loop variable name — coincidentally the
same name as this repro's global `k`, unrelated), then fails to
build the generated C. Neither list alone reproduces it.

## Root cause (narrowed, not fully confirmed)

Traced with a debug dump added at the `NOTYPE`-violation collection
site (`PYC_DBG_NOTYPE=1`, kept in `collect_var_type_violations` —
see its comment) plus one-off tracing inside `P_prim_index_object`'s
FA transfer function (`fa.cc`, not kept — it's in the analysis hot
loop, ran on every pass; removed after use to avoid a standing
`getenv()` per `index_object` evaluation).

**Ruled out: CreationSet-equivalence merging.** The leading
hypothesis when this was filed was that `b`'s and `k`'s `list`
CreationSets get merged into one shared clone somewhere in
`clone.cc`'s `determine_basic_clones` (`equivalent(CreationSet*,
CreationSet*)`, keyed loosely on `a->sym == b->sym` — both are just
"list", nothing else compared at that level). Checked directly:
`determine_basic_clones` (`clone.cc:370`) explicitly guards against
exactly this — `if (cs1->vars.n != cs2->vars.n) { make_not_equiv(...);
continue; }` (line 386) — `b` has 2 element vars, `k` has 0, so
they're marked NOT equivalent before any merge decision. Confirmed
via the debug dump: the violating EntrySet's `self` formal has
`out.sorted.n == 1` (one CreationSet, not a union of `b`'s and
`k`'s) with `vars.n == 0` printed directly in the trace, i.e. this
IS `k`'s own, properly-separate, properly-monomorphic clone of
`list.__getitem__`/`list.__str__` — not one shared with `b`.

**What actually seems to happen:** `list.__str__`'s loop
(`for k_loopvar in range(0, len(self)): ... self[k_loopvar].__repr__() ...`)
has a condition that's statically `0 < 0` (false) for `k`'s own
receiver — the loop body should never need a type for THIS specific
clone. Confirmed empirically: run `k = []; print(k)` completely
alone (no other list anywhere in the program) and
`P_prim_index_object`'s FA handler is **never invoked at all** — no
`NOTYPE` violation, nothing to trace, `list.__getitem__` simply
isn't analyzed for `k`'s clone. This is correct, sound pruning.

Combined with `b = [2, 3]` (which DOES need the loop body analyzed,
since its length is nonzero), `k`'s own dedicated clone SUDDENLY has
its loop body analyzed too — landing in the exact same
`P_prim_index_object` transfer function, for `k`'s own,
properly-isolated CreationSet (`vars.n == 0`), which then correctly
computes: nothing to flow (`cs->vars` is empty, and
`get_element_avar(cs)` — the per-CreationSet "ever had an element
written" slot — was never written to either, since `k = []` never
executes an append/store). Result: bottom, `NOTYPE`.

So the actual gap looks like: **whether a given loop body gets
scheduled for analysis under one EntrySet is somehow influenced by
whether ANOTHER EntrySet of the SAME underlying `Fun` needs it
analyzed**, even though each EntrySet's own formal (`self`) is
correctly, separately monomorphic and each clone's own runtime data
would never reach that path. `Var::live` (per-`Var`, not per-`AVar`/
per-EntrySet — confirmed by its field declaration in `if1/var.h`)
is one plausible mechanism for how this could leak across clones,
but `collect_var_type_violations` runs (`fa.cc:5139`) BEFORE
`mark_live_funs` (`fa.cc:5188`), so `Var::live` reads as `0`
(unset, not yet computed) for every violation in this repro's
trace regardless of clone — not a usable signal at this point in
the pipeline, and not confirmed as the actual leak path. The
ACTUAL scheduling/reachability mechanism that ties one EntrySet's
CFG-reachability determination to another EntrySet's was not
identified — this needs someone to trace FA's PNode/send worklist
(`fa->send_worklist` or equivalent) or step through with a debugger
to find where "is this PNode reachable" gets decided and confirm
whether it's genuinely per-EntrySet or has a per-Fun component.

## How this was found

Landing pyc's issue 024 (extended iterable unpacking, `a, *b = [1, 2, 3]`)
built each star target's value via a runtime loop appending into a
freshly-constructed empty list. A test combining a star-unpack that
happens to consume zero elements (`j, *k = [1]`, where the star's
range is empty) with an earlier star-unpack that consumes a
non-empty, int-element range hit this exact failure. Isolating
further showed it's unrelated to unpacking at all — a plain
empty-list literal reproduces it identically, with zero star syntax
involved.

## Reproducer

```python
b = [2, 3]
print(b)

k = []
print(k)
```

Expected (CPython): `[2, 3]` then `[]`. Order doesn't matter (swapping
which list is constructed/printed first still fails, still attributed
to whichever `print()` runs second in the source).

## Verification plan

1. Find the actual PNode-reachability/scheduling mechanism that lets
   one EntrySet's "loop body is reachable" conclusion affect a
   DIFFERENT EntrySet of the same `Fun` (see "Root cause" above for
   what's already ruled out — CreationSet merging isn't it).
2. Land a fix — likely making that determination properly
   per-EntrySet, or (if that's infeasible / too invasive) special-
   casing "a receiver whose relevant length/count is a compile-time
   constant zero doesn't need its indexing operations typed at all."
3. Add the reproducer above as a test.
4. Un-skip issue 024's always-empty-star case
   (`j, *k = [1]`, `a, *b, c = [1, 2]`) as a real committed test once
   this no longer poisons a shared analysis.

## What this unblocks

- Any program mixing an empty list literal with a non-empty list of
  some concrete element type reaching the same method — plausibly
  common (e.g. an accumulator initialized `[]` then later compared/
  printed alongside a literal list of the same nominal element type
  elsewhere in the program).
- Issue 024's always-empty extended-unpacking case
  (`j, *k = [1]`, `a, *b, c = [1, 2]`) as a fully-committed,
  unconditionally-safe test rather than one that only works in
  program-level isolation.
