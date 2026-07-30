# 073 — Teaching the splitter the productive-vs-inert context distinction (contour identity = type/data, not caller display)

**Status:** **IMPLEMENTED + verified 2026-07-30** (see "### Landed"
under the Conclusion). Framed 2026-07-29, substantially revised
2026-07-30; the "## Conclusion" below SUPERSEDES the "inert mask /
two-stage" plan in the rest of this file. The inert-mask
framing (most of this doc) turned out to be the wrong lever: a proof
below shows caller display/nesting *cannot* generate unbounded
EntrySets, so masking it is at best a bounded-factor patch. The real
generator is `check_split`'s split-lineage routing, and the real fix is
to route recursive edges through the ordinary `(type × data)` dedup.
The rest of the file is kept as the derivation trail (measured prereq
traces remain valid).
**Affects:** `ifa/analysis/fa.cc` — `check_split` (the `e->from->split`
branch + `pending_es_backedge_map`), `entry_set_compatibility`,
`find_best_entry_sets`, `edge_nest_compatible_with_entry_set`,
`group_display_ok`, `update_display`; `ifa/analysis/clone.cc`
`ES_FN::equivalent`; `python_ifa_build_syms.cc` `def_fun_pyda` /
`maybe_synthesize_closure_pyda`.

## Conclusion (2026-07-30): the fix and why — AUTHORITATIVE

### Landed (2026-07-30): `check_split` type-identity knot-tying

The fix is a single-site change in `check_split`'s `e->from->split`
branch (`ifa/analysis/fa.cc`). On the normal (flow-time, `avoid == null`,
non-`split_unique`) call path, instead of minting a fresh split-lineage
link when `edge_nest_compatible` fails, it ties the recursive knot by
**exact type identity**: it reuses the lowest-id existing contour of the
fun that is nest-compatible AND a *hard* type match
(`edge_type_compatible_with_entry_set(e, x) == 1`); only if none exists
does it fall back to the original lineage mint. A genuinely new
monomorphic arg-type tuple still mints its own contour (bounded by the
finite type domain); the `split_unique` and split-detach (`avoid`) paths
are unchanged.

The hard type match is load-bearing: an earlier version let the edge
fall through to `find_best_entry_sets`, whose type matching is *soft*
(reuses a type-incompatible contour with a `val−4` penalty) — that
merged contours it shouldn't and **regressed `match_seq`**. Requiring
`== 1` (exact type equality) never merges type-different contours, so
064's level-descending precision is preserved *by type*, not by display.

**Measured results:**
- adatron FA: **120s+ stall / 1.7 GB → 0.54 s / 149 MB** (converges).
  The 4-line 057 repro now compiles fully (exit 0). plcfrs FA also
  converges (`-x 1`: 1.3 s, then its *pre-existing* type violation — no
  longer a hang). All three divergent programs fixed.
- Suite: **235 passed / 0 failed on both the C and LLVM backends**
  (identical to the pre-change baseline).
- Corpus sweep (84 examples): **+1 (`genetic2`: FAIL → COMPILED),
  zero regressions** — exactly one example changed category. The whole
  064 regressor list (`match_seq`/`match_none`/`recursive_polymorphic`/
  `exception_propagation` in the suite; `chaos`/`chess`/`mastermind2`/
  `pisang`/`sat` in the corpus) is untouched.

**Newly exposed downstream blockers (separate issues, not FA):** now
that adatron reaches codegen it fails on `t2 = NULL; t0 =
(_CG_float64)t2;`. Investigated 2026-07-30 — this is **issue 071 Source A**
(implicit fall-off-the-end `None`): `calculate_error`'s `return 1.0 *
error / len(kernel_table)` sits **inside** the `for col_counter in
range(len(kernel_table))` loop, so an empty `kernel_table` falls off the
end → injected `return None` → return type `float64 | None`. codegen's
`simple_move` (`ifa/codegen/cg.cc:918`) has a guard for a nil *lhs*
(`:910`) but none for a nil *rhs* into a scalar *lhs*, so it emits the
illegal `(_CG_float64)NULL`.
- **`--no_implicit_none 1`** (the opt-in issue-071 mechanism, a
  deliberate CPython divergence) makes adatron **compile clean AND run
  correctly** — verified end-to-end on a reduced dataset: error `0.025`,
  identical to CPython modulo float-repr formatting. Same resolution
  chess uses. This remains the way to actually *run* adatron.
- **`simple_move` illegal-cast trap (LANDED 2026-07-30, `cg.cc`):** the
  emitted `(_CG_float64)(void *)NULL` is ill-formed C++, so `simple_move`
  now degrades a `void *`→floating/complex move to the issue-056 salvage
  trap (`assert(!"runtime error: None coerced to a floating-point
  value")`) instead of emitting uncompilable code — keyed on the emitted
  C types (the union sym has num_kind NONE; `c_type` collapses `None | T`
  to T). This makes adatron (and the class) **compile under the default**
  (FAIL→COMPILED in the sweep; suite 235/0 both backends; corpus +1, zero
  regressions). It can only turn a currently-uncompilable program into a
  compilable one (`void *`→floating never compiles today), never change a
  compiling one. **But adatron still does not *run* under the default:**
  the `float64 | None` return then reaches `print`'s repr dispatch, which
  can't resolve the union → a *separate*, pre-existing runtime trap
  (`"matching function not found"`, the 018/030 heterogeneous-union
  boxing family). My cast-trap is on a genuinely dead path here (never
  reached — adatron aborts earlier on that dispatch). So the cast trap is
  a real codegen robustness fix, but making adatron *work* under the
  default still needs union boxing (018/030) or `--no_implicit_none`.

plcfrs hits its pre-existing 055/053 type violation. Neither is this
FA fix's concern.

### Theorem: caller display/nesting cannot generate unbounded EntrySets

Under `(type × display)` identity the contour set is finite for a finite
type domain, by induction on lexical depth:

- **nd 0** (module): exactly 1 contour.
- **nd 1** (top-level fn): `display = [module]`, a *constant* — so its
  contours are keyed purely on argument types, bounded by the finite
  type domain. This holds *even for a recursive* top-level function: its
  display never varies, so recursion ties its knot by type identity.
- **nd 2** (method): `display[0] = module`; `display[1]` is the caller
  contour, which is either an nd-1 contour (bounded, above) or — within
  a same-depth cluster — *inherited* by `update_display` (`fa.cc:950`,
  slot `i < caller.nd` copies the caller's slot). Either way it ranges
  over a finite set. Bounded.

So `(type × display)` has a finite fixpoint regardless of recursion.
**The display is not, and cannot be, the source of the explosion.**
Corollary: the unbounded generator must mint contours *without* going
through the `(type × display)` dedup — i.e. *not* through
`find_best_entry_sets` (`fa.cc:1080`), which is that dedup.

### The sole unbounded generator: `check_split`'s split-lineage routing

`check_split` (`fa.cc:1123`) runs *before* `find_best_entry_sets` in
`make_entry_set` (`1185` vs `1188`) and routes by the **split lineage**,
bypassing the dedup:

- It searches the caller's split-*parent* `e->from->split->out_edge_map`
  (`1152`) — or `pending_es_backedge_map` (`1125`) — for a prior call at
  this PNode. The candidate `ee->to` passes `check_edge` (the type
  filter, `1160`), so it is **type-admissible**.
- Gate at `1162`: `!edge_nest_compatible_with_entry_set(e, ee->to)`.
  `ee->to`'s display was stamped when the split-*parent* made the call,
  so it records the *parent's* context; `e->from` is the *child*.
  Parent ≠ child **by construction**, so the nest check fails *every*
  level → mint fresh at `1163` and set `e->to->split = ee->to`, extending
  the lineage one link. Next recursion level repeats against a new
  parent → a fresh contour per level, unbounded.

So "type-identical contour considered incompatible" = `check_edge`
passes but `edge_nest_compatible` fails on a parent-vs-child display
mismatch that can never reconcile. What is actually unbounded is not the
display (bounded, above) but the **`->split` lineage chain** — a
call-string context that skips the dedup. The display is merely the gate
this router consults. Measured on adatron (probe removed): `check_split`
mint at `1163` = 19,002 → 37,487 → 59,699 and climbing across a single
pass, vs a *flat 658* for the `find_best_entry_sets` fall-through.

### Every non-type dimension that makes a type-compatible contour "incompatible"

`entry_set_compatibility` (`fa.cc:1029`) — the reuse decision:

| dim | site | strength | bounded? |
|---|---|---|---|
| `split_unique` flag | `1031` (also `check_split:1162`) | hard `return 0` | bounded — set only on `sym_new_object` (`ast_to_if1.cc:1874`), per-call-site allocator contexts |
| display / nesting | `1032` `edge_nest_compatible` | hard `return 0` | **bounded** (theorem) |
| setter / sset classes | `1047` `edge_sset_compatible` | soft `val−=2` | finite domain, but **cross-pass-unstable keying** (066) |
| constants | `1048` `edge_constant_compatible` | hard for `clone_methods_per_cs`, else soft `−1` | bounded (constant cap) |

`check_split` (`fa.cc:1123`) — runs *before* the above:

| dim | site | bounded? |
|---|---|---|
| split-lineage `e->from->split`→`out_edge_map`, display-gated | `1151-1168` | **UNBOUNDED — the `->split` chain grows per recursion level** |
| `pending_es_backedge_map` | `1125` | same lineage family |

`split_edges` — the split *decision* / product routing:

| dim | site | bounded? |
|---|---|---|
| display | `group_display_ok` `~4355`, `edge_display_compatible` `~974` | bounded |
| setter *site* signature | `setter_site_signature` `~4299` | finite sites (066: unstable) |
| marks (SPLIT_MARK) | `different_marked_args` `~803`, `mark_map` | finite domain, but mark *distance* may grow with recursion depth — **plausibly a second unbounded channel, UNVERIFIED** |
| type (SPLIT_TYPE) | `group_signature` | (the axis we keep) |

`ifa/analysis/clone.cc` `ES_FN::equivalent` (`221`) — the *post-FA*
merge that collapses over-splits (display-parent equiv, constants,
escape, ivars, per-Var out types, per-PNode + creation-point CS equiv,
prim period/cast). This is a safety net that *would* re-merge redundant
display-distinct contours — **but it only runs if FA terminates**, so it
never rescues a divergent FA.

**Verdict:** display (bounded), `split_unique` (bounded), constants
(bounded) are all harmless to termination. The setter axis is a finite
domain with unstable cross-pass keying (066's growing product; not
adatron's problem — adatron's cross-pass state is stable at 490). Marks
are an open question. **The only *genuinely* unbounded generator is
`check_split`'s split-lineage/backedge routing.**

### The general fix

Route recursive/backedge calls through the **same `(type × data)` dedup
as every other call** instead of the split lineage:

1. In `check_split`'s `e->from->split` branch, when a type-admissible
   candidate exists, do not mint on a parent-vs-child display mismatch —
   fall through to (or defer to) `find_best_entry_sets`, so a recursive
   edge whose argument types match an existing contour **reuses it**
   (knot tied by type identity). A genuinely new monomorphic arg-type
   tuple still gets its own contour — bounded by the finite type domain.
2. Retire the split-lineage/backedge context (`e->from->split`
   out-edge routing, `pending_es_backedge_map`) and the display from
   *identity*; keep the display only for `make_AVar` variable resolution
   (`fa.cc:204`), which for the Python frontend is the module singleton
   only (closures are lowered to explicit classes —
   `maybe_synthesize_closure_pyda`).
3. Keep the finite **data** axes (setter/CS = shedskin's `dcpa`,
   constants) but with stable *source-site* keying (066) so they are
   productive across passes — needed by the genuinely cross-pass
   oscillators (dijkstra2/065/066), though *not* by adatron.

This is shedskin's model: identity = `cpa` (arg-type cartesian product)
× `dcpa` (data/allocation context), **no call-context/display
dimension**. Termination follows from type-domain finiteness. A widening
/ CPA_LIMIT deferral is a backstop *only* for a genuinely infinite type
domain (unbounded recursive type construction), never for 055/057.

**Why this supersedes the inert-mask plan (below).** The inert mask only
neutralizes the display *gate* (b), and there are ≤ `nesting_depth` such
slots — a bounded factor. It "fixes" adatron only because adatron's
recursion happens to funnel entirely through that gate (making
`check_split` take its reuse branch); it does nothing about the lineage
*generator* and cannot bound a recursion routed through a non-display
dimension. Type-identity routing addresses the generator directly and is
precision-preserving: 064's level-descending `len`/`__getitem__` stay
separated because their levels have *different argument types*, not
because of the display.

### Verification (2026-07-30): both probes RESOLVED

Instrumented `set_entry_set` (total fresh mints), `check_split:1163`,
`make_entry_set` fall-through, and `build_type_mark`/`build_setter_mark`
(max mark distance); ran to the stall on every known-divergent program
(instrumentation removed after). `OTHER = total − check_split −
fall-through`:

| program | pass | total mint | check_split | fall-through | OTHER | max_mark |
|---|---|---|---|---|---|---|
| adatron | 6 | 60,357 | 59,699 | 658 | **0** | **18** |
| 057 4-line repro | 1 | 50,502 | 50,399 | 103 | **0** | **0** |
| plcfrs (055, `set`-based) | 1 | 50,627 | 49,849 | 778 | **0** | **0** |

1. **Marks — NOT a second channel (confirmed).** `max_mark` is bounded
   and *constant* (18 for adatron, 0 for the others) while contours grow
   by tens of thousands. Structural reason: `build_type_mark`
   (`fa.cc:4708`) is a min-keeping shortest-distance relaxation
   (`mark+1` per hop, stop-at-already-marked) over the pass's *finite*
   AVar graph, so mark distances are bounded by graph size per pass — a
   bounded function of the contour set, never an independent generator.
2. **Sole generator — confirmed corpus-wide (both classes).** `OTHER = 0`
   exactly on all three: every fresh mint is either `check_split`(`1163`)
   or the flat fall-through. `check_split` is 97–99.6% and the only path
   that grows; the fall-through is flat (≤778). Holds on the non-sorted
   `set`/`plcfrs` divergence (055), not just the sorted/tuple class —
   `check_split`'s split-lineage is the single unbounded generator.
   (dijkstra2 does *not* diverge — it fails fast with an unrelated
   `sizeof_element` codegen error — so it is not a test case here.)

### Still open (sequencing, not blocking the fix)

- **Data-axis stability:** the cross-pass oscillators (065/066) still
  need 066's source-site CS keying even after the `check_split` fix;
  adatron/057/plcfrs do not (their cross-pass state is stable). Sequence
  the two independently. **The build plan for that distinct cross-pass
  oscillation is [074](074-fa-cross-pass-oscillation-plan.md)** (measured
  2026-07-30: 17 of ~77 corpus programs still hit the pass cap).

## The question

`sorted()` over both `list[str]` (`.keys()`) and `list[tuple]`
(`.items()`) drives `tuple.__lt__`-style speculative recursion, and the
splitter mints unbounded contours for the comparison/bool cluster
(`__pyc_to_bool__`, `__ge__`, `__getitem__`, `__lt__`). Measured on
adatron: those contours are **type-identical and differ only in nesting
*display*** — pure caller-context multiplication with zero type
refinement (057's "Impact" evidence). The splitter must learn to *not*
create such a contour. But naively "ignore the display" regresses
(064). So the real task: teach the splitter **which context distinctions
are productive (keep) and which are inert (refuse)**.

## The display does two unrelated jobs — separate them

pyc's per-EntrySet `display[]` (built by `update_display`,
`fa.cc:950`) is consulted in two very different places:

- **(A) Variable resolution — correctness.** `make_AVar` (`fa.cc:204`)
  resolves a reference to a variable `v` with
  `0 < v.nesting_depth ≤ fun.nesting_depth` (a free variable owned by a
  proper ancestor *function* scope) via `es->display[v.nesting_depth-1]`.
  Getting this wrong is a miscompile. This is the display's *only*
  semantic obligation.
- **(B) Contour separation — a precision heuristic.**
  `edge_nest_compatible_with_entry_set` (`fa.cc:792`),
  `group_display_ok` (`fa.cc:4355`), `edge_display_compatible`,
  `ES_FN::equivalent` use display equality to decide whether two calls
  of the same function *share* a contour. This is context-sensitivity;
  it is **not** a correctness requirement — it only affects precision.

The bug lives entirely in (B). The confusion is that (B) currently keys
on the *same* array that (A) needs, so it inherits a per-caller
granularity that (A) never asked for.

### Measured: for the Python frontend, (A) is empty above the module singleton

Instrumenting `make_AVar`'s display branch (`fa.cc:206`) across adatron
+ a spread of corpus programs (mao, mandelbrot, life, othello, pystone,
sieve, bh, genetic, neural1 — including ones with nested defs and
generators): **every** display consumption resolves a *module-level*
name (`nesting_depth 1`) through `display[0]`, which is always the one
module contour (a singleton). **No function ever consumes `display[k]`
for `k ≥ 1`.** That is the direct consequence of
`maybe_synthesize_closure_pyda` (`python_ifa_build_syms.cc:510`): a
nested def/lambda that captures an enclosing *function* scope's variable
is lowered to an explicit closure-carrier class (captures threaded
through a heap `self`), "sidestepping nesting_depth/display entirely."
So for the Python frontend the display's correctness job (A) is
satisfied by a single constant slot; every finer distinction it draws is
job (B) only.

(ifa is also the V-language backend, where genuine on-stack nested
functions DO use (A). The fix below must therefore be *conditional* on a
statically-computed per-function/per-slot property, not a blanket change
— see Stage 2.)

### Why "just relax (B)" regresses — and what that teaches

[064](064-method-phantom-display-blocks-es-split-routing.md) prototyped
exactly this (zero real methods' `nesting_depth`) and **reverted it**:
suite 227→223, corpus 51→47. Its 2026-07-23 correction pins down why:
the per-caller display separation of container methods (`len`,
`__getitem__`) was silently supplying **per-recursion-level monomorphic
contours** in level-descending recursion (`list[list[int]]` → `list[int]`
→ `int`); collapsing it re-fused the levels and the recursive formal
unioned all depths. So (B) is *sometimes genuinely productive*.

The lesson is not "keep the display." It is: **that productive
separation is data polymorphism and belongs on the type/data axis, not
the caller-display axis.** 064's correction and
[066](066-cs-split-decision-keyed-per-pass-not-per-creation-site.md) both
reach this: once the container's element partition (CreationSet split) is
pinned deterministically, "level-descending recursion's per-level
separation comes from the CS partition (not the method display), so 064
dissolves and methods can be `nesting_depth 0`."

## The critical distinction (stated precisely)

> A context distinction is **productive** iff it yields a **monomorphic
> type specialization that does not already exist**. Productive
> distinctions come from the **type/data axis** — the cartesian product
> of argument types (Agesen CPA → pyc EntrySets) and the
> element/allocation-site partition (Plevyak IFA → pyc CreationSets).
> Those are productive *by construction*: a new contour there ⟺ a new
> monomorphic type. The **caller-display axis** is productive only by
> accident (when it happens to line up with a type/data difference) and
> non-productive in general (adatron: identical types, different
> displays). Therefore the caller display must be removed from contour
> **identity** and demoted to its correctness-only job (A); all
> separation the analysis needs must be carried by the type/data axis.

This is exactly shedskin's model, and why shedskin compiles this corpus:
its constraint node identity is `(thing, dcpa, cpa)` —
`dcpa` = data/allocation context, `cpa` = cartesian product of argument
types — and there is **no caller-display dimension at all**
(`shedskin/infer.py`, `CNode`). The lexical `parent` exists only for
name resolution (job A), never for contour identity. `create_template`
mints a new template **only** for an unseen `(dcpa, c)` — the
productivity invariant, enforced structurally. Its `CPA_LIMIT` is a
*deferral-and-escalate* backstop for genuine product explosion, not the
primary mechanism (`infer.py:1262`).

The determinism half is 066's finding: shedskin's durable data-split is
keyed on the **stable source allocation site** (`gx.alloc_info`), so
re-flow reproduces the identical partition — productive (each round
strictly adds decisions) instead of oscillating. pyc currently keys the
CS split on the *re-created per-pass CS*, which is why it churns.

## Prereq trace — adatron's EXACT mechanism, measured 2026-07-29 (corrects the Stage-1 framing below)

Before implementing, adatron was traced (temporary getenv-gated probes
in `set_entry_set`, `split_css`, and `make_entry_set`, all removed). The
result **refines and partly corrects** the "Stage 1 = CS creation-site
split" framing below — the CS side is not adatron's direct actor:

- **adatron reaches pass 6** (it is not an intra-pass-1 hang). It
  progresses through several split passes, then a *later* pass diverges.
- **`split_css` performs ZERO splits** on adatron (CS/data-polymorphism
  side is quiet). So 066-part-2's CS-creation-site keying, taken
  literally, is **not** what adatron trips.
- **The divergence is the [065](065-mark-stage-es-split-routing-and-growing-product.md)
  "growing product" on the ES side.** `Fun::ess` for the comparison/bool
  cluster grows into the thousands and keeps climbing
  (`__pyc_to_bool__` 6866, `__ge__` 3634, `__lt__`/`__getitem__` ~1000+
  and rising) — the same ES contour re-minted every pass.
- **The direct actor is `group_display_ok` blocking the ES-split ROUTE**
  (`fa.cc:4646-4647`). ROUTE is the idempotence mechanism: a re-derived
  split group is supposed to route back to its recorded product instead
  of minting a fresh one. For these `nesting_depth==2` methods the group
  spans multiple caller displays, `group_display_ok` returns false,
  ROUTE is skipped, and a fresh product is minted **every pass** → the
  growing product. **This is exactly [064](064-method-phantom-display-blocks-es-split-routing.md)'s
  mechanism**, now shown to be the concrete cause of an actual corpus
  non-convergence, not just a dijkstra2 routing statistic.
- **Productivity lens (measured):** at the fresh-mint site, the count of
  mints that are type-compatible-but-only-nest-incompatible with an
  existing ES is **flat (~244)** while total mints climb past 24000 — so
  the growth is **not** a type/display redundancy; the products are
  distinguished by **setter class** (data), and the churn is ROUTE being
  display-blocked, not a type over-split.
- **Hard constraint on the fix:** the ROUTE path calls
  `set_entry_set → update_display`, which **asserts** display consistency
  (`fa.cc:958-962`). That assert is *why* `group_display_ok` gates ROUTE.
  So relaxing the gate for inert display slots REQUIRES co-modifying
  `update_display` (Stage 2 item 3 / 064 item 2) — they cannot be
  separated.

**Consequence for the plan.** For adatron the operative lever is Stage 2
(demote the inert display from the ES-split ROUTE gate + co-modify
`update_display`), NOT split_css. Stage 1's role is unchanged but its
*purpose* is sharpened: it must supply, on the deterministic type/data
axis, whatever per-caller precision `group_display_ok` currently
provides for the shared container methods (064's `len`/`__getitem__`
level-descending case) so that demoting the display in the ROUTE gate
does not reproduce 064's regression. The open measurement that decides
how much Stage 1 adatron itself needs: are adatron's comparison-method
products distinguished by **genuinely different, stable setter classes**
(productive → Stage 1 must absorb them first) or **redundant/unstable**
ones (non-productive → relaxing the ROUTE gate alone converges adatron)?
That is the first thing to settle when implementation resumes.

### Correction (2026-07-29, second probe): it is INTRA-pass `check_split`, not cross-pass ROUTE

The "operative lever is the ES-split ROUTE gate (`group_display_ok`)"
claim just above was an inference from the growing `Fun::ess`; a direct
per-pass/intra-pass EntrySet-count probe **corrects it**:

- **Cross-pass durable state PLATEAUS.** `total_ess` at each pass start:
  597 (p1) → 461 (p2) → 492 (p3) → **490, 490, 490** (p4,5,6), and the
  leaf-method counts are *identical* across p3–p6 (`__lt__`=20,
  `__ge__`=30, `__pyc_to_bool__`=18, `__getitem__`=96). So the
  between-pass splitter is **convergent/idempotent** here — this is NOT
  the 065/066 cross-pass growing-product oscillation, and **adatron does
  NOT need Stage 1** (the data axis is already stable).
- **The explosion is INTRA-pass.** Within pass 6, `total_ess` climbs
  ~linearly (15.9k → 31k → 48k over successive 100k-edge windows),
  concentrated in the leaf comparison methods (`__pyc_to_bool__`,
  `__getitem__`, `__ge__`, `__lt__`). The **root callers stay bounded**
  (`sorted`/`max`/`min`/`list` = 0–2, flat) — so it is emphatically NOT
  a growing caller set.
- **The mint path is `check_split`'s `e->from->split` branch
  (`fa.cc:1163`), ~exclusively.** Counting the two intra-pass mint
  sites: `check_split`(1163) = 19,002 → 37,487 → **59,699** and climbing,
  vs `make_entry_set` find_best_entry_sets-fall-through = **658 (flat)**.
  So ~99% of the unbounded creation is `check_split` minting a fresh
  orphaned EntrySet per recursive invocation, gated by
  `!edge_nest_compatible_with_entry_set(e, ee->to)` — **exactly issue
  057's original rounds-5–7 root cause, now quantified**, and happening
  *during flow*, not between passes.

**Revised consequence.** For adatron the fix site is precisely
`check_split`'s `e->from->split` branch (`fa.cc:1162-1168`): when the
candidate `ee->to` is type-admissible (`check_edge` already passed at
1160) and the nest-incompatibility is only on **inert** display slots,
the branch must **reuse `ee->to`** (the existing `else` →
`set_or_copy_AEdge`) instead of minting at 1163 — tying the recursive
knot. The `update_display` assert (`fa.cc:958`) still couples in: the
reuse path calls `set_entry_set → update_display`, so inert slots must be
skipped there too. 064's level-descending `len`/`__getitem__` case is
guarded here by the `check_edge` type filter at 1160 (different-type
levels are skipped, not reused) — **but that filter is loose
(empty-intersection only), so this must be verified against 064's
regressor list, not assumed.** Stage 1 (cross-pass CS determinism) is
orthogonal to adatron and deferred; it remains the fix for the
*genuinely* cross-pass-oscillating programs (dijkstra2 / 065 / 066).

## Plan (two stages — order is load-bearing)

Stage 1 must land before Stage 2. Stage 2 alone is 064's measured
regression; Stage 1 supplies the precision Stage 2 removes from the
display, so together they are precision-neutral and convergent.

### Stage 1 — move productive separation onto a deterministic type/data axis (prerequisite)

Make the container-element partition (the union that actually drives
adatron's blow-up) **monomorphic and deterministic**. This is
[066](066-cs-split-decision-keyed-per-pass-not-per-creation-site.md)
part 2 + [072](072-empty-container-notype-current-mechanism-and-plan.md)
steps 1–3 (the *write-attribution split*, NOT 072's disproved step-4
seeding):

1. **Durably key each CS split on the stable creation site**, not the
   per-pass CS identity (066's `alloc_info` analog / generalize
   `setter_site_signature` from a per-pass hint to the stored identity).
   `creation_point` re-applies the stored decision verbatim on re-flow
   instead of re-deriving it. This is what makes the split *productive*
   in the user's sense: a re-derived group routes back to its first CS
   (066's `cs_group_signature` ledger, ROUTE branch already landed) and
   never re-mints.
2. **Phase-order ES-split then CS-split**, and do not let a CS split
   re-open an already-decided ES split (066's part 2). This stops the
   ES↔CS ping-pong that keeps re-opening decided splits.
3. **Result for adatron:** `sorted`'s internal `r = []` receives `str`
   from the `.keys()` path and `tuple` from the `.items()` path; the
   backward write-attribution partitions its element CS by reaching
   write-type, so `x = r[i]` is monomorphic per partition. The
   `str ∪ tuple` union never forms → `x < …` never speculatively
   dispatches into `tuple.__lt__` on a "maybe tuple-of-tuples" value →
   the comparison/bool cluster settles into a finite, productive set of
   contours. (066 predicts exactly this for the sibling dijkstra2 union.)

Stage 1 is expected to fix adatron/057 on its own; verify that before
starting Stage 2.

### Stage 2 — demote the caller display from contour identity where job (A) is inert

Now that Stage 1 carries the productive separation, remove the
non-productive display over-splitting:

1. **Compute a static per-function display-liveness mask.** For each
   `Fun` F and each display slot `k ∈ [0, F.nesting_depth)`, slot `k` is
   **correctness-live** iff some function in F's lexical subtree (F or a
   descendant) contains a reference to a `Var v` with `v.nesting_depth ==
   k+1` and `v.nesting_depth ∉ {0, ownfun.nesting_depth+1}` — i.e. job
   (A) actually consults `display[k]`. Computable once from IF1 before FA
   (`Fun::collect_Vars` gives referenced Vars with their depths;
   `Fun::nested`/`nested_in` give the subtree). For the Python frontend
   this mask is empty for all `k ≥ 1` and a constant singleton for `k =
   0`; for genuine V closures / the issue-001 synthesized closure
   carriers it is non-empty and must be respected.
2. **Distinguish real methods from closure carriers** (064's item 1):
   the issue-001 `maybe_synthesize_closure_pyda` carriers ARE real
   closures (their mask is non-empty — keep their display); genuine
   user/builtin methods have an empty mask above the module slot and can
   be treated as `nesting_depth`-0-equivalent for identity purposes.
   Tag this at build time so FA doesn't re-derive it.
3. **Gate on the mask, not on raw depth**, in every (B) site —
   `edge_nest_compatible_with_entry_set`, `entry_set_compatibility`,
   `check_split`'s `e->from->split` branch, `group_display_ok`,
   `edge_display_compatible`, `ES_FN::equivalent`: compare only
   correctness-live slots; ignore inert ones. `update_display` must
   co-modify (064's item 2): build/assert only the live slots so the
   runtime `display[]` and the identity check stay consistent (the
   desync 064 warned about).
4. **Effect:** two contours of a comparison method that differ only in an
   inert display slot are recognized as the same contour and merged;
   `find_best_entry_sets`/`check_split` reuse the existing type-compatible
   contour instead of minting a fresh one → productive, bounded,
   convergent — with no loss of the precision Stage 1 now supplies from
   the CS partition.

### Backstop (only if a genuinely infinite type domain appears)

A CPA_LIMIT-style *deferral* valve (033 §D/§M6, shedskin's
`CPA_LIMIT`) remains justified ONLY for unbounded recursive **type**
construction (an infinite family of productive monomorphic types, e.g.
`tuple[tuple[tuple[…]]]` without bound). It is a type-domain **depth**
cap with escalation-on-quiescence, never a contour-count cap, and never
the fix for 055/057. Do not implement it as part of this plan.

## Verification plan

- **Primary:** `./pyc shedskin_examples/adatron/adatron.py` compiles
  (today: 120s stall-guard failure) and runs correctly vs CPython; add
  it as a corpus/regression witness.
- **Stage-1 gate (before Stage 2):** adatron converges with Stage 1
  alone; dijkstra2/fysphun ("no type" canaries, 063) pass-count unchanged
  or improved; no new `cs_dup_split` oscillation.
- **Stage-2 must-not-regress list (064's measured casualties):**
  `recursive_polymorphic`, `exception_propagation`, `match_none`,
  `match_seq` in the suite; `chaos`, `chess`, `mastermind2`, `pisang`,
  `sat` in the corpus — all green (they broke when the display precision
  was removed *without* Stage 1; with Stage 1 they must not).
- **Suite:** `test_pyc.py` and `PYC_FLAGS=-b test_pyc.py` both at the
  current baseline (234/0), zero regressions.
- **Corpus:** full shedskin sweep net-positive, determinism gate clean.
- **Instrumentation to re-run when validating:** the `make_AVar:206`
  per-Fun display-consumption probe (confirms the liveness mask matches
  reality) and the `set_entry_set` fresh-creation per-Fun histogram
  (confirms the cluster's contour count goes from thousands to O(types)).

## What this unblocks

adatron and the whole 055/057 `sorted()`/comparison non-convergence
class; the 063–066 "no type" / oscillation bucket (dijkstra2, the
object-comparison default); the 043/072 container-element family; and it
retires the "widening is the fix" framing (033 §D/§M6) in favor of the
model that actually compiles this corpus in the reference implementation
(shedskin). It also lets real methods drop to `nesting_depth 0` safely
(064), simplifying the display machinery long-term.
