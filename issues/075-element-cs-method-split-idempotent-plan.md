# 075 — Escaping the local maximum: idempotent element-CS container-method separation (the shedskin `dcpa` model)

**Status:** plan, 2026-07-31. Concrete build plan for the genuine "no
type" root ([063](063-no-type-bucket-triage.md)), synthesizing
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
