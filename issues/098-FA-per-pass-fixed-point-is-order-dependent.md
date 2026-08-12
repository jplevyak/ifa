# 098 — FA's per-pass value-flow fixed point is order-dependent, not verified before a pass is declared converged

**Status:** open, found 2026-08-11 while root-causing why
[097](097-CGEN-callsite-vs-clone-formal-type-mismatch.md)'s attempted
fix (defer an edge's routing decision until sibling edges settle,
detailed in that issue) regressed 3 existing tests. Not a duplicate of
that regression — the regression was a *symptom*; this issue is the
*mechanism* it exposed. Not fixed; documented with a concrete, traced
repro and open design options, per this directory's "file it" convention
for something requiring real design work.

**Affects:** `ifa/analysis/fa.cc`'s `analyze_to_convergence` (the
per-pass worklist loop, ~7118) and the whole reactive value-flow
machinery it drives — `clear_avar` (~5235, the per-pass "re-derive
from scratch" reset), `propagate_out_change`/`send_worklist`
(~271-304), `add_pnode_constraints`'s `Code_SEND` case (~2739, the
synchronous CFG-walk-triggered dispatch attempt), `add_send_edges_pnode`
(~2002), `function_dispatch`/`pattern_match` (~1765, `if1/pattern.cc`)
— and, per `clear_avar`'s own documentation, possibly `AVar::match_cache`
(persists across passes as a performance cache; not directly
implicated here but a plausible related suspect, see "Not fully
traced" below).

**Related:**
[097](097-CGEN-callsite-vs-clone-formal-type-mismatch.md) — the
investigation this fell out of; that issue's own compatibility-timing
bug is unaffected by this one (still open, still root-caused, just not
what regressed the tests). [033](closed/033-splitter-non-idempotent-divergence.md)/
[074](074-FA-cross-pass-oscillation-plan.md) — the existing, *closest*
prior art on FA convergence not being as solid as assumed, but a
**different mechanism**: 033/074 are about *splitting decisions*
(which `CreationSet`/`EntrySet` partition to route an edge to)
oscillating *across* passes. This issue is one level more basic — it's
about whether a single pass's own ordinary value-flow/dispatch
resolution reaches a well-defined fixed point *at all*, independent of
splitting. If this issue's mechanism is real and general, it would be
upstream of and could confound 033/074's own diagnosis (an oscillating
split decision could be a downstream symptom of a dispatch attempt
that failed on bad timing, not a genuine type-precision issue).
[closed/076](closed/076-mutation-driven-receiver-divergence-not-cloned.md)
— same family in spirit ("a reactive decision made against momentarily
incomplete data, with no guaranteed revisit"), different concrete
mechanism (076 is about container-element `CreationSet` accumulation,
this is about dispatch/`pattern_match` resolution timing).
[closed/057](closed/057-sorted-tolist-fa-nonconvergence.md)/
[closed/073](closed/073-teach-splitter-productive-vs-inert-context.md) —
**the closest real precedent**, on `entry_set_compatibility` itself
(the same function 097 root-caused); see "Prior art" below, this is
worth reading before designing a fix. [055](055-FA-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
— still **open**, same worklist-churn-without-true-convergence
signature as 057 but a *different* trigger that 057's fix didn't
close; evidence this general family has more than one live member.
[closed/035](closed/035-nondeterministic-codegen-clone-order.md) —
prior, *confirmed* instance of iteration/scheduling order changing
`analyze_edge`'s own output (not just this issue's dispatch-adjacent
machinery), including a real miscompile, not just a hang or a
diagnostic gap. [closed/009](closed/009-fa-violations-nondeterminism.md)
— a **cautionary counter-example**: a symptom that looked exactly like
order-dependence turned out to be a measurement bug, not a real one;
see "Not fully traced" above and "Prior art" below for why this matters
here specifically. [closed/037](closed/037-matcher-cartesian-cs-product.md)
— confirms `AVar::match_cache` (the "Not fully traced" suspect) is
real, keyed on exact per-position `AType`s, and already documented as
missing "throughout convergence" by design — useful grounding for
whoever checks that hypothesis first.

## Prior art in this codebase (surveyed 2026-08-11)

None of these close this issue — each is either a different mechanism
in the same convergence loop, or a methodological precedent for how to
investigate/verify a fix here. Listed in order of relevance.

**[closed/073](closed/073-teach-splitter-productive-vs-inert-context.md)
— `entry_set_compatibility`'s *soft* scoring is already a known,
fixed-once correctness hazard, on the exact function 097 root-caused.**
`check_split`'s `e->from->split` branch used to fall through to
`find_best_entry_sets` when its own nest-compatibility check failed —
and that "regressed `match_seq`" because `find_best_entry_sets`'s
matching is soft (a type-incompatible candidate is merely penalized,
`val -= 4`, not rejected — the exact scoring behavior 097 found
routing `loadTIText`'s edge into `strip`'s wrong `EntrySet`). The
landed fix requires *hard* type equality (`edge_type_compatible_with_
entry_set(e, x) == 1`) for that specific call path instead of trusting
the soft score. This is direct, already-in-the-codebase precedent that
loosening `entry_set_compatibility`'s matching is dangerous in ways
that have bitten before — worth reading in full before touching that
function again for either 097 or 098.

**[closed/057](closed/057-sorted-tolist-fa-nonconvergence.md) — same
loop, same "soft match over-splits or over-merges" family, and the
*authoritative fix philosophy* for it.** 057's own root cause (before
073 landed the fix) was `entry_set_compatibility`'s hard
`edge_nest_compatible_with_entry_set` gate causing *thousands* of
non-productive, type-redundant `EntrySet`s to be minted because every
live call arrived with a distinct nesting display. 057's "authoritative"
resolution (quoted directly, since it generalizes well beyond that
issue): *"Every newly created contour MUST be productive — i.e. it
must realize a monomorphic type specialization that does not already
exist... What the splitter is actually doing... is minting contours
that are type-identical to an existing contour... and differ only in
nesting display — pure call-context multiplication with zero type
refinement."* This issue's own mechanism is arguably the mirror image:
not *too many* non-productive contours, but a single dispatch decision
made too early, against a receiver whose type union hadn't finished
growing — the same underlying theme (a routing/matching decision made
without enough information, with no guaranteed revisit) from the
opposite direction. 057 also confirms `fa->ess.n` is **not a live,
in-pass progress signal** — only refreshed by `complete_pass()` — the
same category of "this counter looks like it tells you about progress
mid-pass; it doesn't" trap this issue's own investigation had to work
around by instrumenting the actual dispatch call directly rather than
trusting aggregate counters.

**[055](055-FA-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
— still open, same family, different trigger.** `set.__sub__` causes
`analyze_to_convergence`'s worklists to churn without bound (`ess.n`
flat while edge/send pops climb linearly) on `plcfrs.py` — the
identical *signature* 057 documented, but explicitly confirmed *not*
fixed by 057's landed change (`073`'s check_split fix does not touch
055's trigger path). Live evidence this is a family with more than one
member, not a one-off — worth checking whether 055's repro also
involves a dispatch site with the same "succeeds reliably under normal
scheduling, might not under different scheduling" shape this issue
documents, next time someone picks either up.

**[closed/035](closed/035-nondeterministic-codegen-clone-order.md) —
confirmed precedent that scheduling/iteration order in this exact
neighborhood can silently miscompile, not just hang or misdiagnose.**
Twelve distinct order-dependence bugs, found by byte-diffing repeated
compiles of the same source. Two are directly on code this issue also
touches: (a) `fa.cc analyze_edge`'s positional-argument loop used to
iterate `form_MPositionAVar` bucket order instead of canonical
position-path order — since that loop *creates* the formal/filtered
`AVar`s, bucket order (heap-layout-dependent) decided `AVar` id
assignment order, which every downstream `qsort_by_id` then keyed on;
(b) `map.h`'s `map_set_add`/`map_union` on `HashMap` used the *base*
`Map` class's pointer-equality insert path while `HashMap::get` probed
with content hashing — inserts and lookups used different hash
functions on the same table, so **`check_split`'s pending-backedge
edge-routing lookups hit or missed by heap layout, i.e. edge routing
itself flipped run to run**. (b) in particular is exactly this issue's
symptom shape (a routing decision whose outcome depends on incidental
ordering) already having been real and already having been fixed once,
in a sibling mechanism. Both were fixed by canonicalizing iteration
order (`qsort_by_id` / id-sorted copies) or making hashing/equality
consistent — the same toolkit this issue's own fix options reach for.

**[closed/009](closed/009-fa-violations-nondeterminism.md) — the
cautionary counter-example: verify before concluding "order-dependent."**
A violation count that alternated between two values across identical
runs looked exactly like an iteration-order bug (matching this issue's
own AUDIT-style hypothesis-first instinct) — and *wasn't*. The real
cause was `Vec::n` (open-addressed table capacity) being read where
`Vec::set_count()` (live element count) was meant; the underlying
analysis was byte-for-byte deterministic the whole time. Directly
relevant to this issue's own "Not fully traced" section: before
committing to "the per-pass fixed point is genuinely order-dependent"
as the final diagnosis, rule out that `pattern_match`'s observed
failure is a `match_cache`/counter-reading artifact of this same
shape, not real order-sensitivity in the underlying computation. This
issue's own repro (a `pm_ok` boolean read directly, not an aggregate
counter) is less exposed to this trap than 009's was, but the general
caution — and 009's own methodology (instrument the exact call,
diff two runs' full traces, don't trust an aggregate) — still applies
directly and is worth following before finalizing a fix design.

**[closed/037](closed/037-matcher-cartesian-cs-product.md) — grounds
the `match_cache` hypothesis.** Confirms `AVar::match_cache` is real
production machinery in the exact `pattern_match` path this issue's
repro exercises, and documents (as an accepted, deliberate tradeoff,
not a bug) that it "misses throughout convergence" because it keys on
*exact* per-position canonical `AType` pointers — i.e. it's *designed*
to only ever help within a stable pass, never to paper over a
still-changing type. That's reassuring context (a well-behaved cache
shouldn't itself cause a false negative) but doesn't rule out an
edge case; still the first thing to check per "Not fully traced" above.

**[closed/003](closed/003-fa-converge-determinism.md) — reusable test
infrastructure.** The `FAPassEvent` sidecar / `fa-converge` test phase
(`ifa/testing/print_fa_converge.cc`) already exists for locking
per-pass splitter behavior (stage, split counts, ess/css deltas) in a
golden-file test. It doesn't currently instrument dispatch/
`pattern_match` attempts or their success/failure, but the pattern —
an enable-flag-gated event sidecar, zero cost when disabled — is the
established way in this codebase to turn a one-off `getenv`-gated debug
print (what this issue's own investigation used) into a permanent,
low-cost regression test. Worth reusing for whichever fix option is
chosen, rather than inventing a new instrumentation convention.

## Symptom

Not a compiler bug visible to ordinary users today — the *only* known
trigger is [097](097-CGEN-callsite-vs-clone-formal-type-mismatch.md)'s
experimental, reverted defer/force scheduling change. But the
underlying gap it exposed is general: **nothing in `analyze_to_convergence`
verifies that a pass's worklist emptying out actually means "every
dispatch attempt made during this pass has a value stable enough that
retrying it would give the same answer."** It only means "every
currently-queued item has been popped."

Concrete repro (requires re-applying 097's reverted defer/force patch —
`git diff` on `fa.cc`/`fa.h` is clean on main, this doesn't reproduce
without it): `tests/generator_basic.py`'s first loop, `for v in gen():`,
desugars to (at least) four call sites at that source line: `gen()`
itself, `__iter__()`, and two separate `__pyc_more__`-dispatched calls
(the loop's entry test and its continuation test). Traced
`function_dispatch`'s `pattern_match` result for all four across every
pass:

- Under **unmodified** scheduling: the continuation-test site's *first*
  `pattern_match` attempt succeeds, every single pass, every time
  sampled — it never even needs its own reactive retry.
- Under **097's reordering**: in the final pass, that same site's
  *only* attempt fails (`pattern_match` finds zero candidates) — and
  because that happens to be the last event before `edge_worklist` and
  `send_worklist` both empty out, nothing ever retries it.
  `analyze_to_convergence`'s outer `do`/`while` then sees no further
  progress and stops. The `AVar` that call's result should have
  flowed into is left at its per-pass-reset empty value, reported as
  an `ATypeViolation_kind::NOTYPE` ("expression has no type").

## Root cause (traced this far; see "Not fully traced" for the remaining gap)

1. **Every pass re-derives everything from scratch.** `clear_avar`
   (`fa.cc:5235`) wipes `AVar::in`/`out`/`backward`/`forward`/
   `arg_of_send` at the top of every pass (its own comment: "the
   analysis re-derives flow state from scratch each pass"). Structural
   state (`AEdge::to`, `EntrySet::out_edge_map`, `Fun::calls`)
   survives; value state doesn't.
2. **Re-derivation is driven by a CFG walk that synchronously attempts
   dispatch.** `add_pnode_constraints`'s `Code_SEND` case (`fa.cc:2739`)
   calls `add_send_edges_pnode(p, es)` **inline**, during the walk —
   not queued. For an ordinary call, this immediately calls
   `all_applications`/`function_dispatch`/`pattern_match` against
   whatever type the receiver *currently* has (possibly still empty,
   this early in the pass).
3. **A failed attempt is only *fixable* reactively, and only if
   something else changes.** `add_send_edges_pnode` unconditionally
   registers `arg_of_send` on the receiver (and other args) before
   attempting dispatch — so if the receiver's type value changes
   *later* (via `propagate_out_change`, `fa.cc:271`), this exact send
   gets re-enqueued via `send_worklist` and retried. This is the
   mechanism that makes the "cold, walk-triggered, likely-to-fail"
   first attempt self-correct in the common case (confirmed: the
   *other* `__pyc_more__` site in the same repro fails on its own
   first attempt, every pass, and reliably self-corrects via exactly
   this reactive path).
4. **But that self-correction has no deadline, and nothing checks
   whether it's still owed when the pass ends.** `analyze_to_convergence`'s
   inner loop (`fa.cc:7126`, `while (fa->edge_worklist.head ||
   fa->send_worklist.head)`) stops the instant both queues are empty.
   Nothing distinguishes "empty because every value has genuinely
   settled" from "empty because the one thing that would have
   retriggered a failed dispatch happens not to have changed by this
   point, this time." The two are observationally identical to this
   loop.
5. **This is normally invisible because the *order* dispatch attempts
   happen in reliably delivers dependencies just in time.** The traced
   repro's continuation-test site succeeds on its first attempt in
   *every* pass under today's scheduling — meaning, empirically, it
   has **zero timing margin**: it works because something it depends
   on is *always* already resolved by the moment the CFG walk reaches
   it, not because the algorithm guarantees this. 097's reordering
   changed *only* the relative timing of when routing decisions commit
   for *unrelated* edges elsewhere in the program (nothing about *this*
   site's own dependencies) — and that alone was enough to flip this
   site from "always succeeds" to "fails once, permanently, at the
   worst possible moment."

**Stated the way the evidence points**: the per-pass fixed point this
algorithm computes is only a *true* fixed point (i.e., independent of
the order `edge_worklist`/`send_worklist`/the CFG walk happen to
interleave) if every dependency a dispatch attempt could ever need is
guaranteed to already be resolved, or guaranteed to trigger a retry
before the pass is allowed to end. Neither is actually guaranteed
today — the second one only works when it works.

## Not fully traced

Whether the failed `pattern_match` call was querying **against a
receiver type that was itself still incomplete** (the "waiting for a
value that would come, but the notification didn't fire before the
pass ended" story above) or hitting a **stale `AVar::match_cache`
entry** was not distinguished — I stopped at `pattern_match`'s boolean
result, not inside it (`if1/pattern.cc`). `clear_avar`'s own comment
documents `match_cache` as deliberately surviving across passes for
performance, with a specific claim that "a stale entry ... misses,
never lies." If 097's reordering can produce a query that the cache
mechanism treats as identical to a genuinely-different earlier query
(rather than a clean cache miss), that claim would be the actual bug,
and would be a narrower, more surgical fix than anything in "What a
fix would look like" below. Worth checking first if picking this up —
it's a much smaller investigation than the general order-independence
question.

## What a fix would look like (design options, no recommendation made)

**Option A — verify the fixed point before declaring a pass done.**
When `edge_worklist`/`send_worklist` both empty, don't immediately
stop: track whether any dispatch attempt *this pass* returned "no
match" against a receiver whose type could plausibly still be
incomplete (vs. a receiver that's a genuine, permanent type error),
and if so, don't consider the pass converged — force at least one more
sweep. Requires a way to distinguish "permanently no match" (a real
type error, should become a violation) from "no match yet" (retry) —
today `pattern_match`'s boolean result doesn't carry that distinction.

**Option B — make retry unconditional on failure, not conditional on
an unrelated value changing.** Explicitly track "this send's most
recent dispatch attempt failed" and always give it one more attempt
per pass regardless of whether `propagate_out_change` happens to fire
for its receiver — closer to a textbook worklist fixed point (every
node that could be affected gets revisited every round) than today's
"only if something reactively notifies you" scheme.

**Option C — restructure for genuine order-independence.** The
deepest fix: make the per-pass computation a true, order-independent
fixed point in the dataflow-analysis textbook sense — processing order
of `edge_worklist`/`send_worklist`/the CFG walk should provably never
affect the pass's outcome, only its cost. This is the same class of
architectural work 076 concluded its own mechanism needed ("a
different default... not an extension of the existing reactive-
splitting architecture") and did not attempt inline — likely
comparable in scope here.

**Confirm the "Not fully traced" hypothesis first** — if it's a
`match_cache` soundness bug specifically, that's a targeted fix, not
an architecture change, and should be ruled out before committing to
A/B/C.

## Verification plan

1. Re-apply 097's reverted defer/force patch (or write an equivalent
   minimal repro that doesn't require it) as a reliable trigger.
2. Instrument `pattern_match` itself (not just its boolean result) for
   the failing `__pyc_more__` call site to resolve "Not fully traced"
   above — does it consult `match_cache` and get a hit, or run fresh
   and genuinely find nothing?
3. Whichever fix direction is chosen: `generator_basic.py` must compile
   clean under 097's defer/force scheduling (proves the fix actually
   restores the missing guarantee, not just this one symptom).
4. Full `ifa --test` + `test_pyc.py` both backends + `shedskin_sweep.sh`
   — this touches the single most core, always-on loop in FA; treat
   with at least 076/097's own level of caution.
5. Once fixed, 097's own defer/force change (or some version of it)
   becomes safe to re-attempt, since the timing assumption it broke
   would no longer be silently load-bearing.

## What this unblocks

Directly: 097's own fix (extending the entry-set-compatibility timing
fix to the call/dispatch case) becomes implementable without regressing
unrelated tests. More broadly, if this mechanism is real and general
(not confined to the one repro found so far), it calls into question
how many other silent, zero-margin timing dependencies exist elsewhere
in FA's convergence — any of them could be masking as "just works"
today the same way this one did, and could break the same way under
any future change that alters scheduling order, not just 097's
specific one. Worth being aware of as a systemic risk when reasoning
about the safety of *any* future change to `analyze_to_convergence`'s
loop structure, `add_send_edges_pnode`, or related scheduling code.
