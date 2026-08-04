# 075 — Escaping the local maximum: idempotent element-CS container-method separation (the shedskin `dcpa` model)

**Status:** Piece 1 built and validated 2026-08-04; Piece 2 built and
found INSUFFICIENT the same day; a SCOPED Piece 3 (durable display-
variant sibling reuse) built the same day, then made O(1)-per-lookup
via an index the day after (2026-08-05) -- both steps confirmed
genuine fixes for what they targeted (unbounded EntrySet growth, then
per-call cost). Investigated what was left the same day (2026-08-05):
root cause is NOT `copy_AEdge` itself but this stage's placement --
it runs first, every pass, and (like every stage here) short-circuits
the rest of the pipeline whenever it applies anything, which on this
program means it starves stages 1-6 almost completely (traced: stage 1
reached 4 times in an 8s window vs. dozens of this stage's own
decisions) and re-processes a same-divergence, ever-regrowing edge set
every turn it gets, which `copy_AEdge`'s per-CS fan-out (correctly)
duplicates every time -- hence the geometric edge growth (2 → 4 → ...
→ 3240 → 19440 in one traced sequence, ES count flat at 435
throughout). See "Update 2026-08-04" and its "Indexed lookup attempt"
/ "Edge-explosion investigation" subsections at the end for the full
trail. All landed in the tree behind `PYC_CSM` (0 default/byte-
identical, 2 = Piece 1+2+3 capped to one apply per pass -- still the
only combination confirmed both safe and practically fast), not
corpus-positive on their own. Three separate, now-identified gaps
remain for whoever continues this: the general allocation-site/
creation_point-keyed identity this section originally specifies
(unbuilt), progress detection for this stage's own splits (found
2026-08-05, unbuilt), and/or a placement change so it stops
unconditionally preempting the rest of the pipeline (found 2026-08-05,
unbuilt). Concrete build plan for the genuine "no type" root
([063](063-no-type-bucket-triage.md)), synthesizing
[066](066-cs-split-decision-keyed-per-pass-not-per-creation-site.md)
(durable keying), [073](073-teach-splitter-productive-vs-inert-context.md)
(the display-is-bounded theorem), and [074](074-fa-cross-pass-oscillation-plan.md)
(this session's measurements), and grounded on a **validated prototype**
(dijkstra2 FAIL→COMPILED — see 063's 2026-07-31 update).
**Affects:** `ifa/analysis/fa.cc` — `split_edges` (4199),
`find_or_make_filtered_entry_set` (4166), `redispatch` lambda (4237),
`split_for_per_cs_method_receivers` (5895) / a new sibling stage,
`run_split_stages` (5936), `decide_entry_set_split`/`apply_entry_set_split`
(4499/4663), `creation_point` (403), the ledger (`cs_group_signature`
5346, `ledger_*`). *(Line anchors are for commit `74adab9c`; re-grep after
edits.)*

> **Audience note.** This is written to be executed by an engineer who has
> NOT done the prior investigation. Read [063](063-no-type-bucket-triage.md)'s
> "Update 2026-07-31" section first (the shedskin comparison + the
> prototype result); everything here builds on it. The one-paragraph
> orientation to the codebase is [ifa/AGENTS.md](../AGENTS.md) and
> [ARCHITECTURE](../INDEX.md).

## 1. The situation — a local maximum

Baseline is `53` corpus compiles / `235` suite tests (both backends), and
it is a **local maximum**: every single-lever change we have tried either
does nothing to the compiled count or backslides it. Measured this session:

- **Stage 4** (display-out-of-identity): suite 235/0, corpus 53 —
  byte-for-byte identical to baseline. Neutral on the compiled count; only
  internal violation-count effects, with two internal regressions
  (softrender +14 violations, pygmy 43→77 passes). Not worth landing.
- **CSM** (element-CS container-method split), naive: segfaults alone;
  with Stage 4, gains dijkstra2 + pylife but **loses 33** other corpus
  programs (53→22) and 18 suite tests. Reverted.

So no *incremental* move climbs. The escape requires a **coupled set** of
changes that is net-positive only *together* — and, critically, the
losses we saw are **non-idempotence corruption**, not genuine precision
loss (§2), so the set must also fix the *application*, not just add
features.

## 2. Why the increments backslide (the two failure classes)

The −33 (and the softrender/pygmy noise) are NOT "feature A broke what
feature B needs." They are issue-033 non-idempotence artifacts:

1. **Orphan bare-EntrySet display deref (the CSM-alone segfault).**
   `split_edges` (4199) fans a receiver out per CS by creating a
   *filtered product* ES (`find_or_make_filtered_entry_set`, 4166) and
   redispatching edges into it. Its `redispatch` lambda (4237) **skips**
   any edge that is `!edge_display_compatible` (the issue-034/sudoku5
   guard against mis-stamping a product's display). For a shared container
   method the edges span many caller displays, so most are skipped → the
   per-CS product it just created gets **no edge** → an orphan ES whose
   `display.n == 0`. A later `make_AVar(formal, orphan_es)` (204) indexes
   `orphan_es->display[depth-1]` out of bounds → SIGSEGV (the
   pystone/tictactoe/amaze family the code comment at 4226 warns about).

2. **Unflowed-contour corruption (the −33 and the suite fails).** Driving
   the *dynamic* `split_edges` **every pass, for many ESs**, leaves
   rerouted edges un-re-flowed within the pass; a later split (or the
   re-flow) then reads a half-populated contour → `"polymorphic dispatch:
   no branch matched"` / `"matching function not found"`. This is exactly
   the M2b "unflowed-contour" hazard that `split_ess_for_type`'s
   *decide-then-apply* path (4499/4663) and the fysphun stage-2 revert
   (comment in `run_split_stages`, ~6033) exist to avoid.

Both are **application** bugs, not mechanism bugs. The mechanism (clone
the container method per element-CS) is correct and validated.

## 3. The correction that shrinks the set — the display stays in identity

The earlier plan put "display-out-of-identity" (Stage 4) in the necessary
set. **It is not necessary.** [073](073-teach-splitter-productive-vs-inert-context.md)
proves `(type × display)` has a *finite* fixpoint for a finite type domain
— the display is a **bounded multiplier**, never a source of divergence.
Adding the element-CS partition keeps it bounded (`type × display × dcpa`,
all finite). Demoting the display was only ever a *shortcut* to make
`split_edges`' `redispatch` stop skipping display-incompatible edges (and
so stop orphaning products). That shortcut also dragged in Stage 4's
softrender/pygmy regressions and changed contour identity globally.

**The right fix for failure-class 1 is to fan out per `(CS × display)`**
(§4, Piece 1): give each display-distinct edge its *own* `(CS, display)`
product instead of skipping it. Then every product gets an edge (no
orphans, no crash), the element stays monomorphic per product (the
`(list Vertex)` merge still resolves), and the display **stays in
identity** — at the cost of a *bounded* constant-factor increase in
contour count, and with **no Stage 4**.

**Consequence:** the necessary set is THREE coupled changes, not four:
CSM (fan per `(CS × display)`), decide-then-apply, durable alloc-site
keying. Stage 4 is demoted to an *optional* later cost optimization —
reach for it only if the `display × dcpa` contour count proves to be a
*performance* problem, and even then weigh its trajectory regressions.

## 4. The build — three coupled changes + a fitness function

Build order is load-bearing: the harness first (you cannot judge a
combination without it), then the application-safety pieces, then confirm.

### Piece 0 — the combination-sweep harness (the fitness function) — build FIRST

A reusable script `ifa/tools/combo_sweep.sh` (or under `shedskin_examples/`)
that, given a flag set, produces the objective we optimize:

- Input: an env-flag string, e.g. `PYC_CSM=2 PYC_CSKEY=1`.
- Runs, capturing to a per-combo dir: `test_pyc.py` (C backend) and
  `PYC_FLAGS=-b test_pyc.py` (LLVM backend) → pass/fail counts; and
  `shedskin_sweep.sh` with `OUTDIR` set → `results.tsv`.
- Emits one line per combo: `flags | suiteC pass/fail | suiteLLVM
  pass/fail | corpus compiled | GAINED{…} LOST{…}` where GAINED/LOST are
  `comm -13 / -23` of the compiled-program sets vs a saved baseline
  `results.tsv`.
- **Race note:** the two suite backends both compile into `tests/`, so run
  them **serially** (concurrent runs hit `Text file busy`). Corpus sweeps
  use isolated `OUTDIR` build dirs and can run alongside a suite.

This is the fitness function for §5's combination sweep. Every later step
is gated on it; a "win" is *strictly more corpus compiles with zero
COMPILED→FAIL and 235/0 both suites*.

### Piece 1 — CSM: element-CS container-method separation, fanned per `(CS × display)`

**What it does.** pyc's analog of shedskin's `func_copy`-per-`dcpa`
(063): separate a shared `list`/`dict` method contour per receiver
element-CS so `self[i]` / element reads are monomorphic.

**The demand signal (already validated — 99 hits on dijkstra2).** For an
EntrySet `es` with live edges, and each `p ∈ es->fun->positional_arg_positions`,
let `av = es->args.get(p)`. Fire when `av->out->type->sorted` is a union of
**≥2 CreationSets** that are **all the same container type** (each has
`cs->sym->element`; `cs->sym->type ? unalias_type(cs->sym->type) : cs->sym`
equal across them) with **divergent element types** (`get_element_avar(cs)
->out->type` not all equal). Reference implementation of the predicate is
the reverted prototype `split_container_methods_per_element_cs` (see 063's
2026-07-31 update / git history around `74adab9c`); reuse it verbatim as
the starting point.

**The core change — fan per `(CS × display)`, do NOT skip, do NOT orphan.**
Today `split_edges` (4199) builds `cs_es_map: CS → one product` and its
`redispatch` (4237) skips `!edge_display_compatible` edges. Change the
container-method fan-out so each edge is routed to a product keyed on
**both its receiver CS and its `e->from` display**:

- Option A (contained, preferred): in the redispatch, when
  `edge_display_compatible(ee, tes)` is false, **mint a fresh product**
  for that edge (`set_entry_set(ee, new_es_with_same_filter)`) instead of
  returning — so the display-distinct edge gets its own `(CS, display)`
  contour. Never leave a `find_or_make_filtered_entry_set` product with no
  edge.
- Option B: make `find_or_make_filtered_entry_set` (4166) display-aware —
  its reuse test (`!es->filters.some_disjunction(filters)`, 4169) is
  display-blind; extend the key/reuse to also require display match so
  display-distinct edges land in distinct products. Heavier (touches a
  shared helper); prefer A first.
- **Orphan guard (belt-and-suspenders, not the real fix):** assert/skip so
  no filtered product with `display.n == 0` is ever handed to `make_AVar`;
  the real fix is A/B never creating one.

**Placement + safety.** Add as a new stage in `run_split_stages` (5936).
Until Piece 2 lands, run it **one split per pass** (return `1` after the
first *real* split) to sidestep the M2b hazard — the prototype's
`return 1` after a successful `split_edges` (see 063 update). Hook it
**before** the type-confluence stage (so it separates receivers before the
element union forms), but keep the existing `if (!analyze_again)` gating on
stage 1 so flag-off is byte-identical.

**Flag:** `PYC_CSM` (`0` off, `1` dump-only demand-signal probe, `2`
split). Dump mode must be side-effect-free: read a CS's element only when
`cs->added_element_var` is already set (calling `get_element_avar`
*creates* the element AVar and sets that flag — a real perturbation of
`collect_type_confluences`).

**Gate for Piece 1 alone:** dijkstra2's `( list Vertex )` resolves with
**`PYC_CSM=2` and NO Stage 4** and **no crash** (this is the proof the
display was never necessary). Expect residual churn / some backslide until
Piece 2 — that is fine at this step; the gate is *no new segfault* and
*dijkstra2's union gone*.

### Piece 2 — decide-then-apply application (removes failure-class 2, the −33)

The every-pass dynamic `split_edges` is the corruption source. Convert CSM
to the **decide-then-apply** discipline that `split_ess_for_type` uses
(4499/4663):

1. **Decide** all CSM splits for the pass against the *unmutated,
   converged* snapshot: collect a list of decisions `(es, position,
   CS-partition, per-(CS,display) product plan)` without mutating the
   graph.
2. **Apply** them after, with a **per-ES dedup** (`Vec<EntrySet*>
   applied; if (applied.set_in(es)) defer;` — 4499's `applied` guard), so
   no split reads another split's mid-pass mutation. Deferred ESs are
   re-decided next pass against settled types.

Because CSM partitions by **CS identity** (not type), you cannot reuse
`decide_entry_set_split` verbatim (it groups by `edge_type_compatible`).
Build a CSM-specific decide step that groups the ES's edges by
`(receiver-CS, e->from-display)` and records the plan; the apply step
mirrors `apply_entry_set_split`'s mechanics (null `e->to`,
`es->edges.del`, `set_entry_set`, `record_backedges`, ledger record).

**Gate:** the −33 recovers. Target: `PYC_CSM=2` alone (no Stage 4) is
suite-neutral (235/0) and corpus **≥ 53 + {dijkstra2, pylife}** with zero
COMPILED→FAIL.

### Piece 3 — durable alloc-site keying (066 / shedskin `alloc_info`)

**Problem.** `clear_cs` wipes `cs->defs`/`cs->ess`/`cs->creates` every
re-flow, so a CreationSet's membership — and thus the CSM element-partition
— is **re-derived from scratch** each pass (066). When it re-derives
differently, the partition churns (issue-033 oscillation).

**Fix.** A persistent map keyed on the **stable creation site**, the
shedskin `gx.alloc_info` analog:

- Key: the allocation `Var`'s def `PNode` (stable IR) plus the entry
  context — i.e. the site where `creation_point` (403) mints the CS,
  invariant to ES/CS re-derivation. (`creation_point(AVar *v, Sym *s, …)`:
  the site is `(v->var->def, contour-context)`.)
- Value: the decided element-CS partition (which reaching-write element
  types map to which CS-duplicate).
- On re-flow, `creation_point` **re-applies** the stored decision verbatim
  instead of re-deriving it. This generalizes `cs_group_signature` (5346)
  / `setter_site_signature` from a *per-pass routing hint* to the *durable
  identity* of the split (066 part 1).

**Gate:** cross-pass `dup_split_attempts` for the CSM/CS stage drops to ~0
on dijkstra2 (deterministic re-derivation); no new oscillation
(`PYC_DBG_OSC`: `pass_limit_hit` not newly set on any program).

## 5. Combination sweep — the actual escape

With Pieces 1–3 behind flags (`PYC_CSM`, `PYC_CSKEY`, and — only if you
must — `PYC_STAGE4`), run Piece 0's harness over the combinations and pick
the flag set that **strictly beats** `53 / 235`:

| combo | expectation |
|---|---|
| `CSM=2` (P1+P2) | +dijkstra2 +pylife, no COMPILED→FAIL, 235/0 |
| `CSM=2 CSKEY=1` (+P3) | above + deterministic (no oscillation regressions) |
| `+ STAGE4=1` | only if contour-count/compile-time forces it; expect softrender/pygmy internal regressions — likely NOT worth it |

The winning default is the smallest flag set with the largest compiled
count at zero regression. Bake that set in as the default (flip the
`getenv` default or make it unconditional) **only after** the full gate
(§6) is green on it.

## 6. Verification plan (per step — issue-033 fragility demands it)

1. **Suite:** `test_pyc.py` and `PYC_FLAGS=-b test_pyc.py` at **235/0**
   both backends, zero regressions, for every flag combo tested.
2. **Corpus compiled-set diff:** no COMPILED→FAIL; target `+dijkstra2
   +pylife` and, as the element-separation generalizes, more of the 063
   "no type" bucket (amaze/rubik/othello2/sudoku* are the shape-B family).
3. **Determinism gate:** re-run twice; identical output (issue-009/035
   ordering).
4. **fysphun canary:** MUST compile fysphun (a many-pass numeric member) —
   the standard short suite never exercises the stage-2 segfault. Run it
   explicitly at every step.
5. **Historic revert casualties — all must stay green:**
   `recursive_polymorphic`, `match_seq`, `match_none`,
   `exception_propagation` (suite); `chess`, `mastermind2`, `sat`,
   `pygmy`, `pyc_declare` (corpus).

## 7. Known pitfalls (each cost a land-verify-revert cycle this session)

- **Do NOT reach for Stage 4 / display demotion.** 073 proves the display
  is bounded; it is an *optional* cost optimization with its own
  softrender/pygmy regressions. The `(CS × display)` fan-out (Piece 1) is
  the correct fix for the orphan crash.
- **Orphan bare-ES → SIGSEGV.** Never hand `make_AVar` a filtered product
  with `display.n == 0`. Fan per `(CS × display)` so every product is
  populated.
- **M2b unflowed contour → "no branch matched"/−33.** Never apply many
  dynamic `split_edges` per pass; use one-per-pass (interim) then
  decide-then-apply (Piece 2).
- **fysphun stage-2 segfault.** Any split stage running unconditionally
  every pass can crash a many-pass numeric program deep in. One-per-pass /
  decide-then-apply avoids it; verify fysphun explicitly.
- **`get_element_avar` side effect.** It sets `cs->added_element_var = 1`,
  which `collect_type_confluences` (4080) keys on. In dump/measurement
  mode, only read elements already created (`cs->added_element_var`).
- **Ledger keys must be cross-pass-stable.** The existing type-only
  `group_signature` (4373) and per-(Var,ES) setter AVars shift as
  splitting proceeds (066); Piece 3 must key on the *source PNode*, not
  the re-created CS/AVar.

## 8. What this unblocks

The genuine "no type" bucket (063) at its root: dijkstra2 (validated),
pylife (validated), and the shape-B container-element family
(amaze/rubik/othello2/sudoku*, [043](closed/043-empty-container-inference-options.md)).
It is the reference model — shedskin's `(thing, dcpa, cpa)` with
`func_copy`-per-`dcpa` and `alloc_info` determinism — which compiles this
whole corpus, applied to pyc *without* removing the display (073's
theorem makes that unnecessary).

## Appendix — measured facts (2026-07-31, commit `74adab9c`)

- **Prototype validated (CSM=2 + Stage4):** dijkstra2 `( list Vertex )`
  gone, `pass_limit_hit` 1→0 (converges), violations 170→44, `ess`
  946→111, **FAIL→COMPILED**; pylife also gained.
- **Naive application (not landable):** suite 235→217 (19 fail), corpus
  53→22 (+2 / −33, 1 segv). Cause = §2 failure classes, not the mechanism.
- **CSM alone segfaults** (§2 failure-class 1); needs Piece 1's
  `(CS × display)` fan-out (NOT Stage 4).
- **Stage 4 alone:** suite 235/0 both backends, corpus 53 *identical set*
  (25 clean / 28 warn / 24 fail) — user-visibly neutral; internal only
  (rubik 417→128 viol …) with softrender +14 / pygmy 43→77 regressions.
- **Demand signal on dijkstra2:** 99 hits, all `list` methods
  (`len`/`__len__`/`__getitem__`/`__eq__`/`__setitem__`/`__str__`/…),
  receivers = unions of 3–12 `list` CSs with divergent element types.
- **074's negative results (do not re-litigate):** the *oscillation* is
  display caller-multiplication (Stage 2 container-fan-out ≤3% of churn;
  no dispatch bounce — 216/217 rubik routes distinct). This plan targets
  the *genuine no-type violations*, a separate lever from the oscillation.

## Update 2026-08-04: Piece 1 built and validated; Piece 2 built and found insufficient — Piece 3 is load-bearing, not optional

Built fresh (the 2026-07-31 prototype was never committed — "doc-only,
code reverted" per `74adab9c`). `ifa/analysis/fa.h`/`fa.cc`, behind
`PYC_CSM` (0 default, 2 = split), flag-off verified byte-identical
(`ifa --test` 58/58, `test_pyc.py` 239/0 both backends).

**Piece 1 (element-CS fan per `(CS × display)`), built as specified:**
`split_edges`'s `redispatch` now mints a fresh per-display sibling
product instead of skipping a display-incompatible edge
(`pick_display_variant`, reusing an existing sibling when one already
matches the edge's display); a new stage
`split_container_methods_per_element_cs` fires it on the documented
demand signal (receiver = union of same-container-type CSs with
divergent element types), before type confluence, one split per pass.

Gate result: **passes exactly as specified.** dijkstra2's `(list
Vertex)` violation count 30 → 0, no crash, no Stage 4 needed (073's
"display is bounded" holds — the `(CS × display)` fan is what the
2026-07-31 prototype used Stage 4 to work around). dijkstra2 itself
still doesn't finish compiling (new "mixed basic types" violations
appear elsewhere, same eventual internal fail as baseline) — expected,
matches the plan's own "Gate for Piece 1 alone" (§4).

Full-scale result: **reproduces the 2026-07-31 prototype's "not
landable" finding almost exactly**, from an independent
implementation. Suite 239 → 222 (18 fails, vs. their 235 → 217/19).
Corpus 54 → 22 compiled (+1 pylife, −32, 1 crash — vs. their 53 → 22,
+2/−33). This independent reproduction is itself useful confirmation
that the plan's diagnosis (M2b unflowed-contour corruption from driving
dynamic `split_edges` on unsettled state) is correct, not an artifact
of the original prototype's specific implementation.

**Piece 2 (decide-then-apply), built as a CS-identity analog of
`decide_entry_set_split`/`apply_entry_set_split`
(`CSMSplitDecision`/`decide_csm_split`/`apply_csm_split`):** DECIDE
snapshots each qualifying receiver's union type and edge set before any
mutation; APPLY runs Piece 1's cs_es_map + `(CS × display)` fan-out
against that frozen snapshot, with a per-ES dedup mirroring
`split_ess_for_type`'s stage-1 discipline (first decision touching an
ES wins the pass; later ones defer to the next pass's re-decision).

Two bugs found and fixed along the way, both instructive:

1. **A real, separate correctness bug**, not part of the plan's own
   scope: batching means the demand-signal scan visits *every*
   qualifying receiver each pass, not just the first (Piece 1's shape)
   — and the divergence check called `get_element_avar` unconditionally,
   which *creates* the element AVar as a side effect. Doing that across
   every candidate on every pass perturbed `collect_type_confluences`
   broadly enough to drop the C-backend suite to 9/241 — not a CSM-
   specific regression, just badly-behaved almost everywhere. Fixed by
   making the scan read-only (gated on `cs->added_element_var`,
   matching the dump-mode discipline §4 already specifies for the probe
   flag value this plan never got to building).

2. **The load-bearing finding.** With that fixed, applying *more than
   one* decided split per pass is separately, actively unsafe:
   `pick_display_variant`'s freshly-minted siblings have no cross-pass
   identity (unlike `find_or_make_filtered_entry_set`'s CS-level
   products, already reused via `!filters.some_disjunction(...)`), so a
   recurring incompatibility re-mints a *new* sibling every pass instead
   of reusing the last one. Batching many decisions per pass compounds
   this into unbounded growth: confirmed empirically, `fa->ess.n` went
   123 → 2747 in under 8 seconds on dijkstra2 (killed; did not
   terminate on its own). Capping the apply side back to one per pass
   (Piece 1's shape, just now decided against a cleaner snapshot) is
   what makes it terminate again — and at that point the result is
   **numerically identical** to Piece 1 alone (222/18 suite, +1/−32
   corpus, same programs both ways).

**Conclusion, sharper than the plan's original framing:** §4 Piece 3
("durable alloc-site keying") is described mainly as a determinism /
cross-pass-churn improvement layered on top of a working Piece 1+2.
That undersells it for CSM specifically — **without Piece 3, Piece 2
cannot safely apply more than one decision per pass, which makes it
provide zero additional value over Piece 1 alone.** Piece 3 is not the
third item in a sequence you could stop before; it's a prerequisite for
Piece 2 to do anything Piece 1 didn't already do. Piece 3 is unbuilt.
Whoever attempts it should budget for that dependency being real, not
optional, and should specifically stress-test the batched-apply path
(not just the single-apply path this session's `PYC_CSM=2` capped
itself back down to) once it's in place.

### Piece 3 attempt, same day: fixes the hang, exposes a performance wall instead

Given the load-bearing finding above, built a SCOPED Piece 3 --
narrower than this section's original "persistent map keyed on the
stable allocation site, hooked into `creation_point`" -- targeting
specifically the mechanism that was actually observed growing
unboundedly: `apply_csm_split`'s per-display sibling minting.

**The fix.** A new shared function `find_or_make_display_variant(ee,
tes)` (`fa.cc`, right after `find_or_make_filtered_entry_set`, which it
mirrors): before minting a fresh sibling product for a display-
incompatible edge, search `tes->fun->ess` -- the SAME persistent list
`find_or_make_filtered_entry_set` already reuses products from across
passes -- for an existing one with matching filters
(`!filters.some_disjunction(...)`, the identical reuse test) that's
ALSO display-compatible with this edge. Both `split_edges`' `redispatch`
(Piece 1) and `apply_csm_split` (Piece 2) now call this instead of each
keeping its own call-scoped (and therefore cross-pass-blind) sibling
list.

**Result: real, confirmed, but insufficient.** With the cap removed
(batching every decision per pass again), dijkstra2's `fa->ess.n` no
longer grows unboundedly -- it climbs 123 → 136 → 139 → 140 → 359 → 419
→ 435 and then PLATEAUS at 435 for as many passes as observed. That is
a genuine fix of the specific bug: siblings ARE now being reused
instead of re-minted every pass. But the run still doesn't finish:
once `fa->ess.n` stabilizes around 435, each pass takes on the order of
a minute (10 CSM invocations traced in the first 10 seconds; only 1
more in the following 50). `find_or_make_display_variant`'s linear scan
over `tes->fun->ess`, called from every edge of every decision, every
pass, is O(n) against a list that's now ~3.5x its original size and
concentrated on a handful of heavily-shared container methods
(`list.__getitem__` etc.) -- cheap per call at n≈123, ruinous at n≈435
called this many times. A program needing dozens of such passes to
converge would need many minutes, which is ~1000x too slow to be
practically usable (every compile-time test harness in this repo times
out at 60s).

**What this changes about the diagnosis.** The "unbounded growth"
finding in the update above was two coupled problems, not one:
identity instability (fixed here) AND an O(n) lookup that was masked by
the identity bug making n unbounded in the first place. Fixing identity
without also indexing the lookup just moves the wall from "never
terminates" to "terminates too slowly to matter." Whoever picks this up
next needs BOTH: an indexed structure (e.g. a map keyed on `(tes,
display)`, not a linear `Vec` scan) AND should re-check whether the
general, `creation_point`-keyed version of Piece 3 this section
originally specifies (which would also address CreationSet identity
instability across `clear_cs` re-derivation, not just the sibling-reuse
layer this session's narrower fix covers) is needed on top, or whether
the indexed sibling-reuse fix alone is sufficient once it's fast.

**Correction:** an earlier version of this update claimed the sibling-
reuse fix alone moved the corpus number to 54 → 23 (+2 pylife /
tonyjpegdecoder). That was a bad comparison -- two flag-on sweeps
against each other, not against the true flag-off baseline.
`tonyjpegdecoder` already compiles WITH warnings at baseline; it was
never gained. Rechecked directly against the baseline sweep: corpus is
54 → 23 compiled (+1 pylife, −32, 1 crash), i.e. `25 + 29` down to
`9 + 14`, identical to the number recorded for Piece 1/2 before this
fix. The sibling-reuse fix does not change which programs compile
under the one-apply-per-pass cap -- it only changes the identity/cost
of the ESs minted while getting there, which the cap already made
irrelevant to outcomes at this scale.

### Indexed lookup attempt, same day: fixes identity AND its own cost, uncovers a THIRD, deeper problem

Per request, replaced `find_or_make_display_variant`'s linear scan over
`tes->fun->ess` with an index: a new field
`EntrySet::display_variants` (`fa.h`) holds just THIS cs_es_map entry's
own siblings, so the lookup is O(siblings of one CS partition) instead
of O(every ES the function has ever split into). Both confirmed
independently, with the one-apply-per-pass cap removed again:

- **Identity/growth: still fixed**, as expected (the index doesn't
  change WHAT gets reused, only how fast). `fa->ess.n` still plateaus
  at 435 on dijkstra2 rather than climbing unboundedly.
- **Per-call cost: fixed**, confirmed by direct timing.
  `apply_csm_split`'s `cs_es_map` construction (which still calls the
  pre-existing `find_or_make_filtered_entry_set`, itself a linear scan,
  once per CreationSet) now runs in single-digit microseconds per call,
  even once `fa->ess.n` has stabilized at 435. The indexed lookup is
  not the bottleneck anymore.

**But the run still doesn't finish -- a third, previously-masked
problem.** With both of the above fixed, dijkstra2 still times out at
60s. Tracing `dec->all_edges.n` per decision shows why: 2, 4, 6, ...,
216, 540, 1296, 3240, **19440** -- geometric growth in EDGE count, not
EntrySet count. Root cause: when a receiver's union spans many
CreationSets (`ety->sorted.n > 1`, seen up to 11 on dijkstra2), every
edge whose own type touches more than one of them gets duplicated via
`copy_AEdge`, once per extra CS -- and those copies become part of the
NEXT pass's `all_edges` snapshot for whatever they land on, compounding
pass over pass. This is unrelated to identity or lookup cost; durable
keying (however implemented) does not touch it. It needs its own
investigation -- e.g. whether `copy_AEdge`'s one-copy-per-CS strategy
is even right once a union gets this wide, or whether same-outcome
copies should consolidate instead of accumulating.

**What's landed, current state:** `PYC_CSM=2` on `main` runs Piece 1+2
capped to one apply per pass -- the only combination confirmed both
safe and practically fast. `find_or_make_display_variant`'s indexed
form is live even in the capped path (a strict improvement over the
linear-scan version it replaced: correct and fast, at every scale
measured), but the cap itself is still what keeps this usable -- not
the indexed lookup by itself, and not one bug away from removable
either, per the edge-explosion finding above. Suite: 239/0 flag-off
both backends (byte-identical), 222/18 flag-on (both backends,
unchanged from every measurement in this doc). Corpus: 54 → 23
compiled flag-on (+1 pylife, −32, 1 crash), unchanged from the
corrected number above. Not a corpus win; landed as validated, safe,
well-documented groundwork, not as a fix. Three coupled problems are
now identified for whoever continues this: CreationSet/allocation-site
identity instability (the plan's original Piece 3 scope, unbuilt),
display-variant sibling identity (fixed, this session), and edge-count
explosion via `copy_AEdge` on wide unions (found this session, unbuilt).

### Edge-explosion investigation, 2026-08-05: root cause is pipeline starvation, not `copy_AEdge` per se

Per request, investigated the third problem (edge-count explosion)
found the day before. Traced `apply_csm_split` on dijkstra2 with the
one-apply-per-pass cap removed (same setup as before), logging
`es->id`, `es->fun`, `ety->sorted.n`, `dec->all_edges.n`,
`es->split`, `es->display.n`, and per-edge `is_es_recursive`/self-loop
checks on every call.

**The six parallel siblings are NOT this stage's own products.**
`list.__getitem__`'s edge count grows in lockstep across six DIFFERENT
EntrySets (traced as `es=400..405`), all with `es->split == null` --
i.e. none of them are cs_es_map products or display variants of
anything; they are six independent, pre-existing clone contours from
ordinary (pre-CSM) FA specialization. `is_es_recursive` was false and
no edge's `from` traced back to its own `es` for every edge checked --
this is not direct self-recursion in the sense
`decide_entry_set_split`'s recursion-separability logic already guards
against.

**The actual mechanism: this stage starves the rest of the pipeline,
and the rare turns it cedes feed it more work.** `run_split_stages`
short-circuits stage 1 (type confluence) through stage 6 whenever an
earlier stage's `analyze_again` is 1 -- exactly the discipline every
stage already follows, and this stage now follows it too (§4's own
spec: hook it *before* type confluence). But because this stage runs
FIRST, every pass, and reliably finds SOME qualifying receiver on this
program, `analyze_again` is 1 almost every pass -- traced directly:
stage 1 was reached only 4 times in an 8-second window during which
this stage applied dozens of decisions. The rest of the pipeline
(including the type-confluence machinery's own careful recursion-
separability and self-product-eviction logic -- see
`decide_entry_set_split`'s comments -- none of which this stage has an
analog of) gets almost no chance to run. On the RARE passes stage 1
*does* get a turn, its own splitting of `__getitem__`'s CALLERS
apparently creates new work for this stage (new clone contours / edges
feeding into `__getitem__`) -- consistent with the edge count only
ever growing between this stage's dominant stretches, never
shrinking. Every time this stage does get a turn, `copy_AEdge`'s
one-copy-per-extra-CS fan-out (needed for the base mechanism's
correctness -- it is how a polymorphic call site gets represented in
every specialized target it can reach) re-duplicates whatever has
accumulated across the ~11-member union since the last turn. That is
what produces the geometric sequence (2, 4, 6, ..., 3240, 19440) with
the EntrySet count staying flat at 435 throughout: it is not
`copy_AEdge` being wrong, and not an identity or lookup-cost problem
(Piece 3 as built, and its indexed form, don't touch this) -- it is
this stage's total lack of a notion of "am I actually making progress
on this (es, position), or just re-processing a regrowing set that
never resolves."

**Why the divergence never resolves here.** This is the same "genuine
no-type" case 063 already diagnosed: dijkstra2's containers are
genuinely nested (`list[dict[Vertex,X]]`), so separating a receiver by
its OWN CreationSet does not separate the divergence in what THAT
CreationSet's element itself contains -- confirmed independently in
the Piece 1 gate-check earlier in this doc (the target `(list Vertex)`
violation went 30 -> 0, but new "mixed basic types" violations appeared
elsewhere; the underlying heterogeneity didn't go away, it moved).

**Two independent, complementary directions for whoever continues
this** (not attempted -- this was scoped as investigation):

1. **Progress detection.** Don't keep re-deciding an (es, position)
   whose element-type divergence hasn't measurably improved since the
   last time this stage split it -- the demand signal (`av->out->
   type->sorted` divergent) is necessary but not sufficient; it needs
   a "did the LAST split actually monomorphize anything" check, the
   CSM analog of what self-product eviction and recursion-
   separability already give the type-confluence stage.
2. **Placement / priority.** This stage does not have to run
   unconditionally first, every pass, forever. It could instead run
   only when the rest of the pipeline is ALSO quiescent (mirroring
   PER_CS_RECEIVER's existing "only on quiescence of every stage
   above" gating, ifa/issues/045) -- trading "separate receivers before
   the union forms" (this doc's original rationale for running it
   first) for "don't starve the stages whose own progress this stage's
   candidates apparently depend on."

Neither was built or measured this session; both are plausible and
would need their own combination-sweep verification (§5/§6) before
landing, same as everything else in this doc.
