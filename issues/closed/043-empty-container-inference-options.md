# 043 — Empty-container inference: current behavior and fix options

**Status: CLOSED** — RESOLVED-AS-INVALID for the container-element premise (`f9624574`),
2026-07-15: option 4 was prototyped end-to-end (CreationSet
element-seed annotation + post-convergence confluence scan + re-run,
mirroring the num_coerce architecture) and **disproved its own
premise** — see "Prototype findings" below. The option survey is
kept because options 1/5 remain relevant to what the failures
actually are.

## Prototype findings (2026-07-15) — read this first

The seeding machinery worked as designed but had nothing to do:
**FA's existing union flow already populates an empty container's
element types whenever the empty CS reaches any shared method clone
or confluence.** Every candidate repro checked:

- `ls = [[], [1, 2]]` + len/iterate/print — compiles clean and runs
  correctly TODAY, no seed needed (the presumed "shape B").
- `total([]); total([1,2])` (empty passed to a read-only fn) — works
  today, no seed needed.
- The dict-in-list repro ("shape B" in the analysis below) — its
  NOTYPE is NOT a bottom element at all: the never-written dict's
  `_keys`/`_vals` element types flow in fine through the shared
  method clones. The real failure is a **union cross-product dead
  combination**: `==` over unions dispatches `list.__eq__(self=
  [v]-list, l=V)` — a pairing that never co-occurs at runtime — and
  an unresolved `V[i]` inside that dead body is what NOTYPEs. That
  is the issue 025-union / 040-family imprecision, not an
  empty-container gap.
- Issue 040's own repro (`b=[2,3]; print(b); k=[]; print(k)`) has NO
  confluence between the two lists (separate print sends), so
  confluence seeding cannot apply by construction; its mechanism was
  traced further this session: the `range` CS created inside
  `list.__str__` is per-CREATION-SITE, shared by both clones, so its
  `i < j` loop condition unions `{0} ∪ {0,2}` and stops folding
  false — the dead loop body gets analyzed for the empty clone.
  That is a CS-per-contour splitting question, not element typing.

The prototype was withdrawn (no dead machinery left in tree). What
remains actionable from the option survey:

- **Option 1's codegen half** (no-type branch → trap instead of
  undeclared-label C errors) — still the highest-leverage honest
  mitigation for the whole family. STILL OPEN.
- **Option 5** (per-contour reachability / CS-per-contour for
  creation sites like `__str__`'s range) — **LANDED 2026-07-15 as
  [045-receiver-cs-method-cloning.md](045-receiver-cs-method-cloning.md)**
  (clone_methods_per_cs: hard per-constant contours, per-constant
  instance CSs, PER_CS_RECEIVER precision split stage). Issue 040 is
  fixed by it; see 045 for the three chain repairs it actually took.
- The union cross-product dead-combination shape deserves its own
  measurement: how much of the remaining "has no type" bucket is
  exactly `op(A, B)` pairings where A and B never co-occur. STILL
  OPEN (the dict-in-list repro in this file remains its live
  witness — 045 does not address it: dict is not a
  clone_methods_per_cs class, and the failing pairing is between
  same-field VALUES, not constant-keyed instances).

**Bonus:** the prototype's first honest repro exposed a REAL,
silent wrong-output bug unrelated to empty containers — mixed-length
list literals in one container print phantom elements
(`[[3], [1, 2]]` → `[[3, 0], [1, 2, 0]]`). Filed as
[044-mixed-length-tuple-list-len-miscompile.md](044-mixed-length-tuple-list-len-miscompile.md).

---

**Original analysis follows** (its "shape B" characterization is
superseded by the findings above). Written while digging issue
025's "has no type" bucket (2026-07-15); extends
[040-empty-list-shared-clone-type-inference.md](040-empty-list-shared-clone-type-inference.md)
with a second live failure shape and a survey of fix options.
**Related:** 040 (the cross-EntrySet scheduling leak), 025 (union
narrowing), 032/033 (splitting precision family), pyc issue 025's
numeric-unification section (the `num_coerce` architecture that
option 4 reuses).

## What works today (verified 2026-07-15)

Element types flow through writes and merges fine. All of these
compile and run correctly:

```python
k = []; k.append(3); print(k)          # accumulator
def add(l, x): l.append(x)
k = []; add(k, 42)                     # append through a callee
dists = [{}, {}]; dists[0][5] = 1.5    # write through subscript into
print(dists[0][5])                     # a container-in-container
k = []; print(k)                       # standalone never-written
                                       # empty (alone in the program)
```

The machinery: each container CreationSet has a per-CS element AVar
(`get_element_avar`) plus positional `vars` for statically-known
slots; `append`/slicing route through the `merge`/`merge_in` prims
(`structural_assignment`) which flow element AVars between CSs. A
standalone never-written empty container works because FA never
schedules its method bodies at all (sound pruning).

## What fails: one root, two surface shapes

A container CS that never receives an element has a bottom element
(or bottom field-element) type. That is harmless until FA is forced
to type-check method bodies against it, which happens whenever a
*written* container of the same class exists in the program:

**Shape A (issue 040):** the empty CS keeps its own properly-separate
EntrySet, but the sibling's existence causes the empty clone's
method-body PNodes to be scheduled anyway (mechanism not yet found —
040's open item). `b=[2,3]; print(b); k=[]; print(k)` still warns
NOTYPE into `list.__str__`/`__getitem__` today (currently benign for
this exact repro — output is right — but the same shape produces
undeclared-label C errors elsewhere).

**Shape B (new, dijkstra2-family):** the empty CS reaches reads
*unioned with* its written sibling through a shared element slot:

```python
class V: ...
paths = [{}, {}]        # ONE list-element AVar holds BOTH dict CSs
v = V(1)
paths[0][v] = [v]       # only cs1 ever written
print(len(paths[1]))    # read yields {cs1, cs2}; cs2's method
                        # clones type-check with bottom _keys/_vals
                        # element types -> NOTYPE + runtime
                        # "matching function not found"
```

Here the read is genuinely polymorphic — no scheduling fix can help;
cs2's methods are reachable for the union-typed receiver. The
failing expressions are `self._keys[i]`/`self._vals[i]` in
`__pyc__/07_dict.py` — bottom-element list fields of the
never-written dict.

**Shape C (2026-07-16, recursive-ES splitting follow-on): shared
iterator CS re-fuses level-separated recursion contours.** After the
recursive-ES splitting fix (pyc issues/025 R1 item 5 resolution), a
level-descending recursive function (deepcopy over `[[1,2],[3,4]]`)
gets one monomorphic EntrySet per recursion level — but only when it
indexes (`obj[i]`, per-receiver `__getitem__` contours). With
`for x in obj:`, ALL levels' loops share ONE `__list_iter__` CS
whose `thelist` field unions every level's lists ({outer, inner,
inner}), so `x` = the union of every level's elements in EVERY
contour, and the freshly-separated ESs re-fuse (verified by
per-edge actual dumps: both recursion edges' actuals carried the
identical 3-CS union). The iterator CS can't split because CS
splitting is setter-driven (stages 3/4) and the per-level `__init__`
setters live in ESs whose separation depends on the CS split —
the CS<->ES contour circularity, with the setter path's recursion
exclusion (deliberately retained) on top.

**Shape C FIXED (2026-07-16), two pieces:**

1. **`__list_iter__` on the issue-045 `clone_methods_per_cs` track**
   (`__pyc_clone_constants__` on its `__init__` param,
   `__pyc__/04_sequence.py`) — the existing circularity-breaking
   lever, same as `range`: `creation_point` skips split-parent CS
   reuse (one iterator CS per creating contour) and the
   PER_CS_RECEIVER stage splits `__pyc_more__`/`__next__` per
   receiver CS. The lever exists precisely because setter-driven CS
   splitting can't bootstrap itself out of this circularity.
   Scoped to `__list_iter__` only: `__tuple_iter__`/str iteration
   can receive CONSTANTS, where `clone_for_constants` would engage
   the per-constant contour machinery for real — a separate
   decision with its own cost measurement if ever needed.

2. **Dup-aware stall guard** (`fa.h` IFA_STALL_LIMIT /
   IFA_NONIMPROVE_LIMIT, `fa.cc` extend_analysis): the iterator
   method chain (`__new__` -> `__init__` -> `__iter__` ->
   `__pyc_more__` -> `__next__`) splits ONE link per pass (each
   link's confluence only appears after the previous pass's
   re-flow), so a recursive iterator user needs ~14 non-improving
   passes while its single boxing violation waits on the last link
   — the old unconditional 8-pass stall counter killed splitting
   mid-chain (and starved the PER_CS_RECEIVER stage, which runs
   only on full quiescence of stages 1-5, so the per-CS iterator
   contours never materialized when a second recursive function
   shared the file). Non-improving passes now advance the stall
   counter only when they RE-DERIVED split decisions (the per-pass
   ledger dup counters — the oscillation signature this issue's 033
   sibling measured); dup-free structural descent gets a looser
   consecutive-non-improving cap (IFA_NONIMPROVE_LIMIT 32) on top
   of the hard IFA_PASS_LIMIT. Measured: the descent shape is
   dup-free (2 dups in 15 passes); the statically-unbounded
   recursion probe runs at 0 violations, where the stall guard
   never governed (pass_limit does, unchanged).

With both, `for x in obj:` recursion fully monomorphises:
`pyc_lib/copy.py`'s deepcopy uses the natural loop form, and
`tests/recursive_polymorphic.py` covers iterator descent next to a
second recursive function (the combination that exposed the guard
starvation). Corpus: kanoodle newly compiles (22 -> 23); no other
example changed; compile times baseline-identical (pygasus 64s).

## Options

### Option 1 — prune empty-element CSs at read/violation time

Treat "read an element from a CS that provably never had one
written" as unreachable: skip such CSs in the read transfer
functions' contributions and don't demand types for results fed only
by them; codegen emits a trap for the dead branch. Sound (a value
cannot be produced at runtime from a container that never had one
stored). Cheap. BUT: pruning during convergence is non-monotonic
(the CS might get a write in a later pass — must only prune at
violation-collection/codegen time), and it fixes the *diagnostics*,
not the programs: shape B's dispatch still sees the union, and the
undeclared-label codegen family needs separate handling. This is
hygiene, not inference.

### Option 2 — an "empty container" lattice element with union absorption

The principled design, directly analogous to the documented
untyped-numeric-constant plan (pyc issue 025's numeric section): an
empty container literal contributes an `empty-<class>` CS that
ABSORBS on union — `empty_list ∪ list[int] = list[int]` — with a
post-convergence default (`empty → <class>[nil]`) for containers
that never meet a concrete sibling. Monotone by construction
(absorption only widens), so it is stable inside FA's fixpoint.
Fixes both shapes at the root: shape B's union collapses to the
written sibling; shape A's clone types against the absorbed element
type. Cost: a type-world change — `type_union`/canonicalization,
clone/concretize equivalence, and codegen all must know the new CS
kind. The numeric analog's history is instructive: three shortcut
implementations each failed for understood reasons before the
architecture (annotate + re-run) landed; budget accordingly. This is
the long-term shape.

### Option 3 — backward/bidirectional seeding from use sites

`k = []` used where `list[int]` flows → push `int` back into `k`'s
element (what shedskin's iterative inference effectively does). Runs
against FA's forward-monotone worklist design at its core; a rewrite,
not a fix. Not realistic for this codebase — listed for completeness.

### Option 4 — seed empty CSs from confluence siblings between passes
### (recommended near-term)

Reuse the exact architecture that already works for numerics
(`fa_coerce_numeric_confluences` / `AVar::num_coerce` /
`PycCompiler::reanalyze`): after a convergence pass, find confluences
where an empty-element CS meets same-class CSs with concrete element
types; seed the empty CS's element AVar (and, for record-backed
containers like dict, its field-elements) with the union of the
siblings' element types; re-run. Monotone across the re-run (only
widens); runtime-invisible (no elements exist, the seeded type only
affects what the clone type-checks and concretizes as). Fixes shape
B (cs2's `_keys`/`_vals` elements seeded from cs1 → its clones type
and dispatch), and shape A when the empty container meets its
sibling at ANY confluence; the standalone case stays correctly
pruned. Precision cost: the empty container concretizes as
`list[int]` instead of `list[nothing]` — harmless (struct layout of
a never-used buffer). Open design point: which CSs count as
"siblings" — same-class CSs in the same observed union (per-AVar,
contour-sensitive, like num_coerce) is the conservative choice.

### Option 5 — find and fix the cross-ES scheduling leak (040 item 1)

Make "is this PNode analyzed under this EntrySet" properly
per-EntrySet. Fixes shape A without touching typing, and would also
illuminate the 033/040 splitting mysteries (likely shared
machinery). Does NOT fix shape B (genuinely reachable union). The
mechanism was never located — needs a worklist trace; unbounded
diagnosis budget. Worth doing opportunistically, insufficient alone.

## Recommendation

Option 4 now (bounded, reuses proven architecture, fixes both live
shapes), option 2 as the eventual principled replacement (file it
with the untyped-numeric-constant work — they are the same design
pattern and should share the absorption machinery), option 5
opportunistically when someone is inside the FA scheduler anyway.
Option 1's codegen-side "dead branch → trap instead of undeclared
label" is worth doing regardless — it turns every residual
no-type branch in the corpus from a C compile error into a running
program with a precise trap.

## Verification targets

1. `b=[2,3]; print(b); k=[]; print(k)` — clean compile, `[2, 3]` /
   `[]` (issue 040's repro).
2. The shape-B repro above — `1` / `0`.
3. Issue 024's always-empty star-unpack tests un-skipped.
4. dijkstra2 (shedskin corpus) past its dict/heap inference wall.
5. No suite regressions (the seeding must not perturb split
   trajectories — see issue 025 round-3's fragility notes; measure
   with the determinism gate and full sweep).
