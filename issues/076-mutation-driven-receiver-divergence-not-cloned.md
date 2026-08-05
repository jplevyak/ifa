# 076 — Monotonic type growth lets a shared/prototype CreationSet permanently contaminate a container read, even after `split_css` correctly separates the instances that share it

**Status:** open, found 2026-08-04, **substantially corrected the same day**
after deeper tracing. The first version of this issue claimed `dict`
(and any no-arg-constructor class) "never gets per-instance method
specialization" and blamed issue
[045](closed/045-receiver-cs-method-cloning.md)'s `clone_methods_per_cs`
gating entirely. **That framing is wrong** — verified by actually
reading `./log/log.s` (`-l s`, `LOG_SPLITTING`) instead of only
stdout/stderr (an earlier analysis mistake, corrected in-session): the
ordinary setter-confluence machinery **does** separate `squares`'s and
`words`'s dict/list identities, with no `clone_methods_per_cs` flag
needed at all. The real mechanism, root-caused by instrumenting
`P_prim_index_object` directly, is deeper and more general: pyc's flow
analysis is **monotonic** (`AType` unions only grow, never shrink or
get re-attributed, pass over pass — a foundational invariant of the
whole framework), and `split_css` has no way to retract a
now-superseded creation-set membership from an `AVar` that observed it
in an earlier, less-refined pass. See "What actually happens" below
for the full, log-verified trace, and "Root cause" for the
instrumented proof. **A follow-up round the same day (also below, "Why
CSM specifically can't help here") confirmed and then ruled out the
most natural-looking fix**: extending CSM's own divergence-detection
to also catch this "uniformly, identically pre-contaminated" shape.
Implemented, verified safe, and empirically tested — it does not fix
`1.py` (and adds new violations), because CSM's decision runs *after*
the poisoning write has already happened, and the poisoning is
permanent under monotonic growth. Reverted (see that section for the
full trace); a genuine, small, unrelated logging bug found along the
way (`DISPATCH`'s log line silently dropped its actual target and
printed the pre-redispatch one instead) **is fixed and kept**. The
real fix now has a narrower, better-understood shape: it must touch
`P_prim_index_object`/`P_prim_set_index_object`'s poisoning mechanism
directly (Option B) or the scheduling that lets a receiver be written
through while still multi-CS (Option C) — not any reactive decision
logic, which is structurally unable to see the problem by the time it
runs. **A third round the same day ("Why Options B and C both collapse
to the same wall") traced both remaining options one level further and
found they aren't independent — both require dispatch to route a
container-mutating method's very first call by creation-site identity,
immediately and unconditionally, rather than the reactive
mint-then-split-on-divergence approach every splitting mechanism in
this codebase currently uses. That's a standalone design effort
comparable in scope to issue 075's own multi-session build, not a
follow-on patch — stopping here, fully traced, for a dedicated future
effort rather than attempting it blind this session.**

This is very likely **the same fundamental mechanism already
documented in [063](063-no-type-bucket-triage.md)** ("pyc separates
the data but keeps the code operating on the data shared, and the
element types merge straight back") and directly relevant to
[075](075-element-cs-method-split-idempotent-plan.md) (CSM), whose own
filtered products are built through the same `split_css`/`split_edges`
infrastructure and may be susceptible to the identical staleness. This
issue adds a precise, code-verified explanation for *why* the merge
persists even when both CreationSet-level and EntrySet-level splitting
demonstrably succeed — 063's "the method isn't cloned per element-CS"
framing turns out to be necessary but not sufficient; **even a cleanly
isolated per-call-site clone still routes its internal reads through a
separately-dispatched sub-method clone that can be independently
contaminated**, as traced below.

**Affects:** `ifa/analysis/fa.cc` — `split_css` (~5544, esp. the
`CreationSet` copy-constructor at ~183 and its `added_element_var(0)`
reset), `get_element_avar` (~1395), `P_prim_index_object` (~2139, the
`for (CreationSet *cs : vec->out->sorted) ... flow_vars(get_element_avar(cs), result)`
loop at ~2143-2156 — and, very likely, every other "container[i]"-style
transfer function with the same shape, not yet surveyed),
`collect_cs_setter_confluences`/`split_for_setters`/
`split_for_setters_of_setters` (~5633-5752, the mechanism that *does*
successfully separate the dict/list identities), `split_for_violations`/
`collect_violation_imprecisions` (~5963-6021, the mechanism that
detects but cannot resolve the residual violation).
**Related:** [063](063-no-type-bucket-triage.md) (same family, prior
framing), [075](075-element-cs-method-split-idempotent-plan.md) (CSM —
built on the same split infrastructure, likely shares this staleness
risk in its own filtered products, not yet checked),
[077](077-primitive-equality-codegen-missing-salvage-guard.md) (the
separate codegen-level symptom this same repro also exposes).

## Symptom

```python
squares = {1: 1, 2: 4, 3: 9}
words = {"a": 1, "b": 2}
```

fails with `expression has mixed basic types:( int64 str )` on
`self._keys[i] == key` inside `dict.__setitem__`
(`__pyc__/07_dict.py:89`), producing a hard C compile error (see 077).

## What actually happens (log-verified, corrected from the first draft)

Tracing `./log/log.s` (`-l s`) for this compile:

1. **Setter-confluence detection splits the dict's own CreationSet**:
   `SPLIT CS 965 dict 3893 -> 1026` — `squares` and `words` get
   genuinely separate dict-object identities. No `clone_methods_per_cs`
   flag involved.
2. **The backing list's CreationSet splits too**, more than once:
   `SPLIT CS 970 list 61 -> 1027` (and, per the instrumented trace
   below, earlier generations `928 -> 970/929` also occurred).
3. **Dozens of per-call-site `EntrySet` splits** happen for
   `__setitem__`, `__getitem__`, `__eq__`, `append` (e.g. `SPLIT ES 45
   __setitem__ ... -> 55/56/57/58`, one product per individual literal
   key-value insertion).
4. By the pass where stage 5 (`split_for_violations`) finally runs,
   `self`'s dict-CS ambiguity is **already fully resolved**
   (`[scss] cs 965 starter_set=1`, `cs 1026 starter_set=1` — each a
   single definition) and `self`'s `EntrySet` splits cleanly two more
   times (`[stage1] av 2768/3691 ES/formal split_entry_set -> 1`).
5. **And yet the violation count never moves** — flat at 13 from
   stage 5's first check straight through to convergence (pass 11, per
   the `-v` stage-timing summary: `type_confluence` progress on 5
   passes, `setter` on 3, `violation` on 1, **`per_cs_receiver` on 0,
   `csm_element_cs` on 0** — confirming both 045's and 075's mechanisms
   genuinely contribute nothing here, but also confirming they are not
   what's missing).

So the receiver-splitting the first draft of this issue claimed never
happens, demonstrably does happen — repeatedly, at both the CS and ES
level — and it *still* doesn't clear the violation.

## Root cause (instrumented, code-verified)

Adding temporary instrumentation to `P_prim_index_object` (the
primitive behind `self._keys[i]`) to print `vec`'s (i.e.
`self._keys`'s) resolved `CreationSet` union at each of its call
contours gave a direct answer:

```
TMPDBG-idx es:51 vec_av:2768 vec_css:[928(list) 1027(list)]
TMPDBG-idx es:51 vec_av:2768 vec_css:[928(list) 929(list) 970(list) 971(list)]
TMPDBG-idx es:51 vec_av:2768 vec_css:[928(list) 970(list) 1027(list)]
TMPDBG-idx es:51 vec_av:2768 vec_css:[928(list) 970(list)]
TMPDBG-idx es:80 vec_av:3691 vec_css:[928(list) 970(list)]
TMPDBG-idx es:82 vec_av:3708 vec_css:[1027(list)]   <- clean, single-CS
TMPDBG-idx es:83 vec_av:3712 vec_css:[970(list)]    <- clean, single-CS
```

`list.__getitem__`'s clone at `es:51` (reached, per the dispatch log,
by *all three* of squares's separately-isolated `__setitem__` clones —
`DISPATCH ES 45/55/56:8051, __getitem__ -> 51`) has a `vec` argument
whose resolved type spans **five different list `CreationSet`s
simultaneously** — `928` (the shared prototype, predating any split at
all), `970`/`1027` (the first split generation), `929`/`971` (a
second). Some *other* `list.__getitem__` clones (`es:82`, `es:83`) DO
end up cleanly isolated to exactly one CS each — proof the mechanism
*can* work — but `es:51`/`es:80`/`es:81` never do, because they are
long-lived clones, reused across multiple calls spanning many analysis
passes, and each pass's `AType` union only ever **adds** to what came
before.

**Why this happens, mechanically:**

1. `split_css` (fa.cc:5544) reassigns which `CreationSet` a *going
   forward* field lookup resolves to (`v->cs_map->put(cs->sym,
   new_cs)`), and mints the new CS via `CreationSet`'s copy constructor
   (fa.cc:183), which explicitly resets `added_element_var(0)` — a
   deliberately fresh start for the new CS's own element tracking.
2. **But nothing migrates or invalidates data an `AVar` had *already*
   flowed from the OLD, shared CS in an earlier pass.** `flow_vars`
   edges, once established, are permanent — this is not a bug in
   isolation, it's the monotonic fixed-point model every stage in this
   file relies on for convergence (the same invariant issue 033/074's
   stall-guard machinery is built around).
3. `P_prim_index_object`'s transfer function (fa.cc:2143) iterates
   `vec->out->sorted` — literally every CS `vec` has *ever* resolved to
   across the whole pass history — and calls
   `flow_vars(get_element_avar(cs), result)` **for each one**. A read
   that was first evaluated against the shared prototype CS in an
   early pass keeps drawing from that prototype's (permanently mixed,
   int+str) element data forever, even once later passes correctly
   refine `self._keys` down to a single, per-instance CS.

**Why per-call-site `EntrySet` isolation alone doesn't save it:**
`self._keys[i]` is not evaluated inline inside `__setitem__`'s own
contour — it's a dynamically-dispatched sub-call to
`list.__getitem__` (confirmed via the dispatch log: `DISPATCH ES
45:8051, __getitem__ 2226 -> 51`), which gets its *own*, separately
managed set of clones. Even though `__setitem__`'s own clone (`es:45`)
is perfectly isolated (exactly one caller, squares's first insertion),
the `list.__getitem__` clone it dispatches to (`es:51`) is a **shared,
long-lived** contour reused across squares's later insertions too (and
was first touched before the list's CS had finished splitting) — so
the caller-side isolation this session's earlier `find_fanout_entry_sets`/
filter-inheritance work (issue 075, commit `5fff3e12`) achieves doesn't
reach into this callee-side contamination at all; they're separate
mechanisms operating on separate contours.

## Relationship to 063 and 075

063 already establishes that pyc keeps shared container methods
operating on merged element data even after `split_css` separates the
data — but frames the fix as "clone the methods per element-CS" (CSM,
built as issue 075). This issue's trace shows that framing is
necessary but **not sufficient on its own**: CSM's own filtered
products are minted through the same `split_css`/`split_edges`/
`find_or_make_filtered_entry_set` machinery this issue just showed can
leave a *reused* clone permanently contaminated by a CS it observed
before an upstream split refined things further. Whether CSM's
specific products are actually vulnerable to this (as opposed to being
protected by some property of `decide_csm_split`'s divergence check)
has not been checked — worth a follow-up trace on `dijkstra2`/`pylife`
(075's still-unresolved targets) instrumented the same way this issue
was.

## Why CSM specifically can't help here (confirmed, tested, 2026-08-04)

Per request ("fix the underlying monotonic issue"), pursued this
further: instrumented `split_container_methods_per_element_cs`
(CSM's own decision loop) directly to see why it reports zero progress
on this repro despite the receiver shape (`list.__getitem__`'s `self`
spanning multiple same-container CSs) looking exactly like what CSM's
own trigger targets.

**Confirmed:** every candidate CSM considers here passes the
`all_added` gate (`cs->added_element_var` is true for every CS in the
union) — the earlier hypothesis that a freshly-split, never-touched CS
was blocking things was wrong. What actually blocks it is the
`divergent` check immediately after: CSM only decides to split when
member CSs' element types *differ*. Direct instrumentation showed the
element types are not just equal-looking but the **identical
canonicalized `AType*` pointer** across every member CS — e.g. CS `928`
(the shared prototype) and CS `1027` (the split product) both point at
the exact same `(int64, str)` union object. There is nothing to
diverge: `P_prim_set_index_object`'s `for cs : vec->out->sorted:
flow_vars(val, get_element_avar(cs))` loop (the same mechanism
identified in "Root cause" above, now confirmed for the *write* side
too, not just the read side traced there) writes every value into
*every* CS a write's receiver has ever accumulated — so once two
sibling CSs have both been written through the same shared clone,
their element AVars converge to the identical polluted union, and
CSM's pairwise-divergence check can never see a difference again. This
is a chicken-and-egg the existing "not decidable YET" comment
(`split_container_methods_per_element_cs`, the `all_added` rationale)
doesn't cover — it's not that an element AVar is *missing*, it's that
it's already wrong, identically, everywhere the receiver's union
reaches.

**Attempted, tested, and reverted**: added a narrowly-scoped
additional trigger — when `all_added` holds and the (uniform,
non-divergent) shared element type is itself `mixed_basics`-positive,
treat that as a decision-worthy signal too, alongside the existing
`divergent` check. Deliberately conservative: reuses `AVar`s the
existing loop already forces into existence (via `get_element_avar`),
adds no new forced creation, so it does not reproduce the "forcing
`get_element_avar` broadly" regression the surrounding comment already
documents (222/18 → 9/241 fails) from an earlier, unrelated experiment.

**Verified safe** (`ifa --test` 58/58; `test_pyc.py` 239/0 and
229/11/10/4, both backends, unchanged) **but empirically ineffective**:
with the extra trigger active, `1.py` still fails to compile — and
picks up *two new* "mixed basic types" violations it didn't have
before, rather than fewer. Root cause: CSM's stage runs last, on
quiescence — by the time it can act, every write that will ever
execute for this program has already happened through the shared,
unsplit clone, and `get_element_avar`'s accumulated type is permanent
under monotonic growth. Splitting the receiver at this point only
changes routing for writes that haven't happened yet (there are none
left, for this program) — it cannot retroactively clean already-mixed
element data. Reverted; `git diff` on this specific change is empty.

**What this establishes:** a real fix cannot live in CSM's (or any
similar reactive, quiescence-gated) *decision* logic at all — the
signal CSM needs is destroyed by the same write that would have
justified acting on it. It has to either (a) prevent the poisoning
write from ever reaching a shared, multi-CS receiver in the first
place (Option C below, structurally the same class of fix as this
session's earlier CSM placement work, but would need to apply far
earlier/more broadly than a single quiescence-gated stage can reach),
or (b) stop flowing a write's value into every CS a receiver has
*ever* accumulated and instead flow only into the CS(s) actually live
for that specific edge (Option B below) — which requires
restructuring `P_prim_index_object`/`P_prim_set_index_object` to
reason per-edge rather than on the aggregate, cross-call-site `AVar`
these primitives currently read (`vec = make_AVar(p->rvals[o], es)`,
one shared value per `(Var, EntrySet)` pair, not per calling edge) —
a materially larger change than anything landed this session, touching
every list/dict/tuple/str element access and write in the corpus.

**A small, genuinely-fixed bug found along the way**: the `DISPATCH`
log line in `split_edges` (fa.cc, ~4459) passed 6 arguments for 5
format placeholders (`"DISPATCH ES %d:%d, %s %d -> %d\n"`) —
`vfprintf` silently drops the trailing argument, so the printed
`"-> N"` was always `old->id` (the *pre*-redispatch target), never the
actual new target (`ee->to->id`, the argument that was being dropped).
Every trace read against this line in this investigation (and
presumably in every earlier one) had the redispatch direction
backwards. Fixed (now prints `old -> new` correctly) — pure logging,
verified safe (`ifa --test` 58/58; `test_pyc.py` 239/0 and 229/11/10/4,
both backends, byte-identical, as expected for a diagnostics-only
change).

## Proposed fix directions (design decision needed — no recommendation made)

**Option A — retroactively invalidate/re-flow on split.** When
`split_css` mints a new CS from an old one, explicitly find and
re-derive every `AVar` that had already flowed data from the old CS's
element tracking, rather than leaving those edges permanent. Directly
attacks the mechanism, but works against the monotonic-growth
invariant the whole fixed-point convergence design depends on —
substantial architectural risk, needs a termination argument as
careful as issue 033's.

**Option B — prune stale CS membership at the read/write site.** Teach
`P_prim_index_object` **and `P_prim_set_index_object`** (confirmed this
round: the write side has the identical `for cs : vec->out->sorted`
shape and is the actual poisoning mechanism, not just the read side
originally traced) to filter `vec->out->sorted` down to CS identities
still reachable from `vec`'s *current* resolved field identity, rather
than flowing from the full historical accumulation. Needs a notion of
"still live" vs. "was true in some earlier pass" that doesn't currently
exist on `AType`/`CreationSet`. Would need to operate per-edge rather
than on the aggregate `AVar` these primitives currently read — a
materially larger restructuring than it first looked (see "Why CSM
specifically can't help here" above).

**Option C — accept it, fix the scheduling instead.** Treat this as
the same class of conclusion issue 033/074 reached about other
oscillation families: monotonicity is load-bearing, so instead of
un-doing accumulated unions, prevent a consumer from ever observing the
shared prototype CS before its first split — i.e. run the relevant
`split_css` pass(es) to quiescence *before* any container-read transfer
function gets a chance to run against the still-shared identity. Similar
in spirit to this session's CSM placement fix (issue 075, "run only on
quiescence of stages 1-6"), but would need to apply far more broadly
(to ordinary `split_css`, not just CSM's own stage).

**Ruled out (tested empirically this round): a CSM-style reactive
decision fix.** Any fix that *decides* to split based on observing
already-mixed element data cannot work, regardless of how that
decision is triggered — see "Why CSM specifically can't help here"
above. This rules out the most natural-looking "quick" fix (extend an
existing decision loop's trigger condition) entirely; both remaining
options (A, B) require touching the mechanism that *produces* the
poisoning, not the mechanism that *detects* it.

## Why Options B and C both collapse to the same wall (2026-08-05, reasoned through, not yet attempted)

Per request ("follow the most promising bigger fix"), traced both
remaining options one level further before writing any code, to find
out which one is actually more tractable. They turn out not to be
independent — both bottom out in the identical requirement.

**Option B, scoped down.** Checked whether `AEdge::filtered_args` (a
genuine, existing per-edge argument mechanism, fa.h:127 —
`get_filtered`, fa.cc:2965) could let a scoped version of Option B
avoid a full primitive rewrite. It doesn't: `filtered_args` flows a
per-edge-*constrained* view **into** the shared ES-level formal
(`analyze_edge`, fa.cc:3000: `flow_vars(actual, filtered);
flow_vars(filtered, formal)`) — it narrows what's contributed, but the
shared `formal` (what `P_prim_index_object`/`P_prim_set_index_object`
actually read via `make_AVar(p->rvals[o], es)`) still ends up as the
union of everything every edge ever contributed. Nothing in the
current architecture gives a transfer function access to "just this
edge's" argument — transfer functions are called per `(PNode,
EntrySet)`, not per edge, full stop. A real per-edge fix means
changing that calling convention everywhere, not adding a lookup.

**Option C, re-examined.** The obvious objection to "isolate the
receiver before any write reaches it" is: isolate it *when*? Traced
through what happens on `1.py`'s very first pass: `dict()`'s two
literal calls have nothing yet to distinguish them from dispatch's
point of view — `find_best_entry_sets`/`entry_set_compatibility`
(the general-purpose "route this edge to the best existing `EntrySet`,
mint fresh only if none match" logic used everywhere, including by
045's own `PER_CS_RECEIVER`) has no signal to route them differently
until *something* about their arguments has already diverged — and
divergence can't be observed until the callee's body has already been
evaluated at least once. `1.py` is straight-line code with no
recursion, so `__setitem__`'s internal loop, and its writes, run to
completion within that same first pass — before setter-confluence
splitting, `PER_CS_RECEIVER`, or CSM have had a single opportunity to
run. Marking `dict`/`list` unconditionally `clone_methods_per_cs`
would not change this: `PER_CS_RECEIVER` only runs once stages 1-5
report no progress (`run_split_stages` stage 6, quiescence-gated,
mirroring CSM's own placement) — strictly after the poisoning write.

**The wall both hit:** for a receiver to be isolated before it's ever
written through, dispatch has to route a container-mutating method's
very first call by creation-site identity, immediately and
unconditionally — not "mint one shared contour, split reactively once
divergence is observed," which is what every splitting mechanism in
this codebase (setter-confluence, `PER_CS_RECEIVER`, CSM) currently
does, uniformly, by design. That's not an extension of the existing
reactive-splitting architecture — it's a different default for at
least this one class of method, and it would need its own termination
argument (an unconditional per-creation-site contour for every
`dict`/`list`/`set` mutation call, with no reactive-splitting
safety net to fall back on) before it could even be prototyped safely.

**Decision (2026-08-05): stopping here rather than attempting it.**
Both a full per-edge primitive rewrite (B) and an unconditional,
immediate per-creation-site dispatch default for container mutators
(C) are substantial, standalone design efforts — comparable in scope
to 075's own multi-session build, not a follow-on patch to it — and
neither has a scoped, low-risk starting point the way the (ultimately
ineffective) CSM-trigger attempt did. Left here, fully traced, for a
future dedicated design effort rather than attempted blind. The two
small, genuinely-safe fixes found along the way (the `DISPATCH`
logging bug, this file's corrected root-cause analysis) are kept; no
further code changes attempted this round.

## Verification plan

1. `1.py`'s repro — after a fix, `list.__getitem__`'s clones should
   show single-CS `vec` types (verifiable by re-adding the temporary
   `P_prim_index_object` instrumentation used for this trace), and the
   compile should be clean.
2. Re-run the instrumented trace on `dijkstra2`/`pylife` (075's
   still-open targets) to check whether the same staleness shows up in
   CSM's own filtered products.
3. Full `ifa --test` + `test_pyc.py` both backends both `PYC_CSM`
   settings + full corpus sweep — any fix here touches core, always-on
   FA machinery (`split_css`, `P_prim_index_object`), not something
   flag-gated.

## What this unblocks

If confirmed to generalize (not yet checked beyond this minimal
repro), this could be the single most foundational open gap in pyc's
"no type" family (063/072/073/074/075) — a fix here might improve or
resolve `dijkstra2`/`pylife`/`sha` (075's targets) and any shedskin
corpus program with genuinely heterogeneous same-type containers, not
just this dict/list corner case.
