# 057 — Generic `sorted()` across differing element types + `list()`-materialization causes FA non-convergence

**Status:** root cause FOUND 2026-07-19 (see "Root cause" below, and
especially the "Dedicated instrumentation on the precise flow path"
subsection — the first pass at root-causing named the wrong function;
corrected the same day via direct instrumentation of the actual
path). Not fixed — the real fix is narrower than first thought: a
recursion-convergence bug in `check_split`'s `e->from->split` branch
(`ifa/analysis/fa.cc`), not a generic "CPA-style cap" across all of
`find_best_entry_sets`, though the latter (issue
[033](033-splitter-non-idempotent-divergence.md)'s S4-D section) is
still relevant as the general-purpose fix shape. The *symptom*
(unbounded hang + unbounded memory growth) is separately MITIGATED as
of the same day: FA's flow-to-fixpoint inner loop now fails cleanly
with a diagnostic after a bounded stall instead of hanging/OOMing
forever (see "Mitigation landed" below) — this mitigation predates
the root cause finding and its own write-up contains one inaccuracy
about *why* it works, corrected in the root-cause section. Found
2026-07-19 while testing the same-day `dict.keys()`/`.values()`/
`.items()` fix ([../../issues/025](../../issues/025-shedskin-examples-coverage.md)).
Not `dict`-specific and not caused by that fix — it's a pre-existing,
general FA architecture gap that a natural "exercise every code path"
test for the new dict methods happened to trip. Same *class* of bug
as [055](055-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
(FA's fixed-point loop churns worklists without bound), but this
repro is dramatically smaller — 4 lines, no `plcfrs.py`-scale program
needed — making this the better issue to use for the real fix.
**Affects:** `ifa/analysis/fa.cc`'s `check_split` (specifically its
`if (e->from->split)` branch) and `edge_nest_compatible_with_entry_set`
— the EntrySet reuse-vs-create decision for a *recursive* call edge,
tried before (and able to bypass) `find_best_entry_sets`'s general
search. See "Root cause" below.

## Symptom

```python
d = {"a": "x", "b": "y", "c": "z"}
ks = sorted(["p", "q"])
items = list(d.items())
si = sorted(items)
```

`./pyc -D . repro.py` hangs (confirmed past 30s; not yet confirmed
whether it eventually crashes like 055's trivial-`__sub__` case did,
or spins forever — not run past 30s in this triage). Instrumented via
the same `PYC_DBG_PHASE` printf-bisection technique used for 055
(temporarily patched into `analyze_to_convergence`, removed after)
confirms the identical signature: `ess.n` (distinct EntrySets) flat
at 97 while edge/send/es-worklist pop counts climb linearly and
unboundedly within FA's first convergence pass:

```
PHASE:   pass=1 ess.n=97
PHASE:     edges=20000 ... ess.n=97
PHASE:     edges=40000 ... ess.n=97
PHASE:     edges=60000 ... ess.n=97
PHASE:     edges=80000 ... ess.n=97
PHASE:     edges=100000 ... ess.n=97
```

**Correction (see "Root cause" below): this `ess.n` reading is
misleading, not evidence of "no new contours."** `fa->ess.n` is a
snapshot only refreshed once a pass fully completes — since this bug
never lets a pass complete, the number above is frozen, not live.
The actual live, unboundedly-growing counter is per-function
(`Fun::ess`), found later via more targeted instrumentation.

## What's known (isolated by bisection)

- **Needs three things together**: (1) a `sorted()` call on a plain
  `list[str]` (a literal like `sorted(["p", "q"])` — `sorted(d.keys())`
  also works, nothing dict-specific about *this* half), (2) a
  `list(d.items())` call (materializing a dict's items via
  `__pyc_tolist__`, i.e. touching `__dict_items_iter__`), and (3) a
  `sorted()` call on that materialized items list (`sorted(items)`)
  or directly on `d.items()` (`sorted(d.items())` — both trigger it
  equally; doesn't need two separate `.items()` call sites, a single
  `items = list(d.items())` reused for both `list()` and `sorted()`
  is sufficient).
- **Removing any one of the three makes it compile instantly
  (<1s)**: `sorted(str-list)` alone with `list(d.items())` (no
  `sorted()` on the items) — fast. `list(d.items())` +
  `sorted(d.items())` alone (no unrelated `sorted(str-list)`
  elsewhere) — fast. `sorted(d.keys())` + `list(d.keys())` (no
  `.items()`/tuples at all) — fast, confirms it's specifically about
  `sorted()` seeing *both* a `list[str]` shape and a `list[tuple]`
  shape (from `.items()`) in the same compiled program, not just
  "two `sorted()` calls" or "`.items()` used at all."
- **Not specific to `dict`** in the sense that matters: the `str`-typed
  half doesn't need to come from a dict at all (a bare list literal
  works identically to `sorted(d.keys())`) — this smells like a
  general "`sorted()`'s (or list-polymorphic-function-in-general's)
  per-element-type specialization doesn't converge when the same
  generic function sees sufficiently different element type shapes
  across call sites that also involve a `__pyc_tolist__`-driven
  list materialization" bug, not a dict-keys/values/items-specific
  one. Not confirmed whether `__pyc_tolist__`/`list()` is essential
  or just one way to reach the same shape (e.g. would `sorted()`
  directly on two different NON-dict-derived types, without any
  `list()` call, also hang? — not tested; time-boxed to the repro
  above once it reproduced cleanly).

## Root cause (found 2026-07-19, after the mitigation below had already landed)

Picked this back up with two rounds of printf-bisection instrumentation
(same convention as 055 — temporarily patched into `fa.cc`, removed
after each round).

**Round 1 — which Sym dominates new-CreationSet creation.**
Instrumented `creation_point`'s `Lunique:` path (the "give up
searching, mint a brand new CreationSet" branch) with a histogram
keyed by the Sym being created. Result on this issue's 4-line repro:
`closure` (86% of all new CreationSets in the first 20K) and `range`
(13%) — together over 99% of all growth. Every other Sym (`list`,
`dict`, `__list_iter__`, ...) was noise (single digits).

**Round 2 — which specific call site, and is the EntrySet context
actually stable.** `sym_closure`-tagged CreationSets come from exactly
two functions, `make_closure` and `make_period_closure` (bound-method
closure construction for `.`-dispatch). Instrumented both with a
histogram keyed by `(PNode id, EntrySet id)`. Finding: only **42
distinct PNodes** ever call in (a small, fixed set — not growing),
but the **EntrySet id count *per PNode* grows without bound**: the
hottest PNode (`__pyc__:943`, `a < b` inside `tuple.__lt__` — see
`__pyc__/04_sequence.py`'s `__lt__`, which does `a = self[i]; b =
t[i]; if a < b: ...`) had already seen 2609 *distinct EntrySet ids*
by the time 60,000 total closures had been created, climbing to 3547
by 80,000 and still growing linearly. The second-hottest PNode is
`__pyc__:1039`, `range.__pyc_more__`'s `self.i < self.j` — `range(n)`
is what `tuple.__lt__`'s `for i in range(n):` loop uses to walk the
tuple's elements.

**This directly contradicts what looked, from the outside, like a
flat EntrySet count.** `fa->ess.n` (what both this issue's original
bisection and the mitigation below were reading) is **not a live
counter** — it's a snapshot rebuilt by `collect_results()`, called
only from `complete_pass()`, which only runs once the inner
flow-to-fixpoint loop *fully drains*. Since this bug never drains
that loop, `fa->ess.n` is frozen at whatever it was before the stuck
pass began, and *looks* flat regardless of how many EntrySets are
actually being created live during the stuck pass. The real, live,
growing collection is **`Fun::ess`** (per-function, updated
immediately by `set_entry_set`), not `fa->ess`. This is the
inaccuracy flagged in the mitigation's status line above — corrected
here; the mitigation below still *works* (see its own note), just
not for the reason originally written.

**Correction — the paragraph that originally lived here named the
wrong function.** It claimed `find_best_entry_sets` scans `Fun::ess`
for a compatible contour via `entry_set_compatibility`'s
exact-type-equality check, and that check's failure to tolerate
"close enough" was the cause. Dedicated instrumentation the same day
(next section) found `entry_set_compatibility` is barely even
*called* for `tuple.__lt__` — nowhere near enough to explain the
growth. The real routing happens one step earlier, in `check_split`,
which `make_entry_set` (`fa.cc`) tries **before** `find_best_entry_sets`
and which can return early without ever reaching it. See below for
what's actually happening.

### Dedicated instrumentation on the precise flow path (2026-07-19, in response to a direct follow-up question: "why are the entry sets growing without bound?")

The previous write-up's "type never exactly matches" theory turned
out to be a plausible-sounding wrong turn. Three more rounds of
targeted instrumentation (each temporarily patched into `fa.cc`,
removed after) found the real mechanism, and it isn't about type
drift at all.

**Round 3 — which function actually mints the new `EntrySet`s.**
Instrumented `set_entry_set`'s fresh-creation branch directly (the
one common choke point for every caller, sidestepping any risk of
missing a bypass route) with a histogram keyed by `Fun` name. Result:
growth is spread across a *cluster* of interdependent functions —
`bool.__pyc_to_bool__`, `int64.__lt__`, `tuple.__getitem__`,
`tuple.__lt__`, `str.__lt__`, `range.__new__`, `range.__pyc_more__`,
`range.__iter__` — not just `tuple.__lt__` alone. Makes sense in
hindsight: `range(n)`'s own comparison (`self.i < self.j`) and
`tuple.__getitem__`'s index access are both on `tuple.__lt__`'s own
call path (`for i in range(n): a = self[i]; b = t[i]; ...`).

**Round 4 — checking whether `entry_set_compatibility` (the function
the previous write-up blamed) is even being reached.** Instrumented
`entry_set_compatibility` itself, filtered to `tuple.__lt__`. Result:
it's called only a handful of times in 40 seconds of a hung compile —
far too few to explain thousands of new `EntrySet`s. Whatever's
creating them isn't going through `find_best_entry_sets`.

**Round 5 — `check_split`, the actual culprit.** `make_entry_set`
calls `check_split(e, edges, split)` *before* `find_best_entry_sets`,
and `check_split` can handle (and return early for) an edge entirely
on its own via two routes — one for recorded recursion backedges,
and one guarded by `if (e->from->split)`, which looks for a matching
call in the *split-parent's* own `out_edge_map` for the same PNode.
Instrumented that second route directly: **every single time**, it
finds a candidate (`m.n=2` matches in the parent's edge map,
consistently) but rejects it because `edge_nest_compatible_with_entry_set`
returns false — and the fallback for that specific case
(`fa.cc`) is unconditional:

```cpp
if (e->match->fun->split_unique || !edge_nest_compatible_with_entry_set(e, ee->to)) {
  set_entry_set(e);       // mint a brand new EntrySet, no cap
  e->to->split = ee->to;  // ...marked as a "split" of the rejected candidate
  ees.add(e);
  return 1;               // handled -- find_best_entry_sets never runs
}
```

**Round 6 — why the nest check always fails.** Instrumented
`edge_nest_compatible_with_entry_set` itself. The mismatch is at
display-chain position 1 (`ef_nd=2`, `es_nd=2` — both sides nested
two levels deep), and **the two colliding values are permanently
fixed, not drifting**: every sample shows the exact same pair of
pointers on each side (`e->from->display[1]` always equals one value;
`es->display[1]` always equals a *different* value) — this is a
structural, static incompatibility between two nesting lineages, not
a type union slowly growing.

**Round 7 — what `e->from` actually is: `tuple.__lt__` calling
itself.** Logged `e->from->fun` and `es->fun` for these edges — both
are `tuple.__lt__`. **This is genuine self-recursion.** Inside
`tuple.__lt__`'s own body (`__pyc__/04_sequence.py`), `if a < b:`
compares two tuple *elements* — but because FA can't statically rule
out that a tuple's elements are themselves tuples (the union typing
here is imprecise, bleeding in from `sorted()`'s shared, polymorphic
`str`/`tuple(str,str)` state), it speculatively explores the branch
where `a`/`b` are ALSO tuples, dispatching `a < b` back into
`tuple.__lt__` — a recursive call, from `tuple.__lt__` to itself.

**The full picture**: `tuple.__lt__` recursing into itself is exactly
what `check_split`'s split/backedge-routing machinery exists to
handle — its own code comments describe it as "recursion follows its
split-off caller contour," and separately warn (in a different
section of the same file) that "when the splitter has concrete type
evidence that a recursive edge does NOT belong with its enclosing
contour, the default must yield or the split silently no-ops and the
same decision re-derives every pass." That warning was written about
the *outer* extend_analysis() loop re-deriving a bad decision once
per pass. What's happening here is the identical disease one level
down: **within a single pass**, every recursive call edge from
`tuple.__lt__` to itself hits a split-parent binding whose nesting
display permanently disagrees with the current call's, and instead
of converging on one shared recursive contour (or falling through to
`find_best_entry_sets`'s general, type-aware search, which never
gets a chance to run), `check_split` mints a fresh, still-orphaned
`EntrySet` for every single recursive invocation, forever. `e->from`
itself — the calling contour — is a *fresh* `tuple.__lt__` `EntrySet`
every time too, for the exact same reason one level further up the
recursion: it's the identical bug reproducing itself at every
recursive depth.

**Confidence level**: this is now directly evidenced end-to-end, not
a theory — every link was confirmed with dedicated instrumentation
(rounds 3-7 above), including the specific pointer values showing the
display-chain mismatch is static rather than growing, and the
specific function identities (`e->from->fun == es->fun ==
tuple.__lt__`) confirming genuine self-recursion. The one thing not
chased further: *why* the two nesting lineages permanently disagree
in the first place (i.e., why the split-parent's recorded display and
the live recursive call's display were never reconciled to begin
with) — that's the next natural question for whoever fixes this, and
is likely close to answerable directly from `check_split`'s and
`set_entry_set_split`'s (or equivalent) existing code, now that the
exact call shape (self-recursive `__lt__` via speculative
tuple-of-tuples dispatch) is known.

**Relation to issue 033's S4-D ("CPA_LIMIT-style deferral valve")**:
still relevant as the general-purpose fix shape (any admission cap
needs a widen-on-quiescence escape hatch, which this specific
recursive case also needs), but the immediate, better-targeted fix
is narrower than a full CPA_LIMIT valve: `check_split`'s
`e->from->split` branch needs either (a) a cap on how many times it
will mint a fresh split-child for the *same* recursive PNode before
falling through to `find_best_entry_sets`'s general search instead of
returning 1 unconditionally, or (b) a real fix to why the two nesting
displays never reconcile. Issue 033's own comments already flagged
this general class of risk for the outer loop; this is the first
concrete repro showing it inside the inner loop too, at the level of
a single recursive call.

**Why not fixed now**: `check_split`'s recursion/split-routing logic
is exactly the kind of surface issue 033's own M2/M3 milestones show
is deceptively risky to touch — every "small" fix attempted there
needed a full multi-day land-verify-revert cycle against the whole
corpus, and this is architecturally adjacent code. Out of scope for
landing in this session; the mitigation below already converts the
failure mode from "hangs / OOMs forever" to "fails cleanly in ~2
minutes with a diagnostic," which is the practically important half
for anyone hitting this today. `check_split`'s `e->from->split`
branch (`fa.cc`) is now a confirmed, precise implementation point for
whoever picks up the real fix — not a guess, and considerably
narrower than "the FA splitter in general."

## Mitigation landed (2026-07-19): bounded stagnation timeout in the flow-to-fixpoint inner loop

Landed *before* the root cause above was found; corrected in light of
it below. A real, scoped, low-risk improvement was worth landing
regardless of root cause: **the inner flow-to-fixpoint loop
(`analyze_to_convergence`'s edge/send/es worklist drain) had no
bound at all**, unlike the *outer* `extend_analysis()` splitting loop
(which already has `pass_limit` and the issue-033 stall guard). A
non-convergent input churns this inner loop forever — confirmed via
this issue's repro: still running past 280s, RSS past 1GB and
climbing, no diagnostic ever printed.

**What was tried and rejected first: a raw edge-count cap.**
Instrumentation showed the PER-EDGE cost itself grows over time as
the stuck AVar's type union keeps accumulating without stabilizing
(measured: the first ~140K edges took ~15s; the next 200K took over
120s) — so any fixed edge-count threshold is unreliable, either too
slow to trip if set high enough to tolerate legitimate large
programs, or a false-positive risk on a slow-but-finite one.

**What landed instead: a wall-clock stagnation timeout, gated on
`fa->ess.n` growth.** Every 20,000 edges, check whether `fa->ess.n`
has grown since the last check; if it has, reset a stall clock. If
120 real seconds pass with **zero** `fa->ess.n` growth, `fail()`
cleanly with a diagnostic pointing at this issue, instead of
continuing to churn.

**Correction (added after root-causing, see above): `fa->ess.n`
does not actually reflect live progress within a pass** — it's a
snapshot only refreshed by `complete_pass()`, which can't run until
the very loop this guard is bounding finishes. So in practice this
guard is a **per-outer-pass wall-clock timeout** (120s, full stop),
not an EntrySet-growth-aware stall detector as originally described.
It still does the intended job — no single pass in the whole corpus
runs anywhere close to 120s (see calibration below) — but not
because slow-and-progressing passes get more time via a resetting
clock; a pass simply cannot exceed 120s at all under this guard,
whether it's making progress or not. A more precise version would
watch `Fun::ess` sizes (the true live counters, per this issue's
root-cause section) instead of the stale `fa->ess`, but a plain
per-pass timeout is simpler, already-verified safe, and sufficient
as a circuit breaker — not revised further here.

**Calibration**: measured the largest known-converging corpus
example, `pygasus` (issue 033's own historical worst case). Its full
run (all ~19-20 passes) completes in ~80s total — comfortably under
120s *per pass*, let alone in total — before reaching its own
separate, pre-existing, unrelated C-compile-error (`_CG_strcat`
given an `int64` argument — not investigated, not this issue's
concern), identically with and without this change, across repeated
runs. (One run produced a segfault at ~10s that did not reproduce
across 5 subsequent attempts on the same input/build — matches a
documented, never-explained, non-reproducible flake already on
record in issue 033's M3 section; not attributable to this change.)

**Verification**: full `test_pyc.py` + `PYC_FLAGS=-b test_pyc.py`
(215/215 both), `ifa`'s `make test` (all phases clean), and a full
shedskin corpus sweep — zero new diffs versus the pre-this-change
baseline, and zero occurrences of the new failure message across the
entire sweep (i.e. nothing in the routine corpus trips the guard).
This issue's own repro now fails cleanly in ~2 minutes
(`fail: FA flow analysis made no EntrySet progress for 120s (380000
edges processed) -- non-convergent input ...`) instead of an
unbounded hang. **Not added as an automated regression test** — the
~2-minute cost to actually trigger the guard is impractical for the
routine (~30s) test suite; this issue's own verification steps above
are the record instead.

This is a symptom mitigation, not a fix: `sorted()` + `dict.items()`
combined still cannot compile (it just fails fast with a clear
message now, rather than hanging). See "Root cause" above for what
actually landed later the same day, via dedicated instrumentation:
`tuple.__lt__` recurses into itself (a speculative "what if these
tuple elements are also tuples" dispatch, forced by imprecise union
typing bleeding in from `sorted()`'s shared state), and
`check_split`'s recursion-routing logic (`e->from->split` branch)
mints a fresh, orphaned `EntrySet` for every single recursive
invocation because the nesting/closure-display check it relies on
permanently disagrees between the recursive call's contour and its
recorded split-parent — never converging on one shared recursive
contour. This issue exists to (a) record a much cheaper, cleaner
repro of the same underlying bug class than 055's `plcfrs.py`-scale
one, now with a confirmed, narrow, concrete implementation point
(`check_split`'s `e->from->split` branch and
`edge_nest_compatible_with_entry_set`) for the real fix, and (b)
document a real landmine: **`sorted()` on both string and
dict-items()-derived data in the same program is a plausible, fairly
ordinary thing to write**, unlike 055's `set.__sub__`-on-a-500-line-
program trigger — so this is more likely to bite a real corpus
example than 055 was. Worth prioritizing over 055 if only one gets
picked up for the actual fix.

## Impact so far

None of the real shedskin corpus examples that motivated the
dict-methods fix (`loop.py`, `mastermind2.py`, `plcfrs.py`,
`sunfish.py`) hit this — each only does a single, simple, unsorted
`.keys()`/`.values()`/`.items()` access, not this specific
multi-type-`sorted()` combination. The committed test
(`tests/dict_items_keys_values.py`) was deliberately written to avoid
tripping this (see that file's own comment) rather than exercise it,
so it stays green. But any *future* corpus example (or user program)
combining `sorted()` on strings with `sorted()`/`list()` on
`dict.items()` will hit this exact hang.

## What this unblocks

Whatever program next combines these two ordinary patterns — likely
to recur. Also, since this is the same bug class as 055, whoever
fixes this fixes (or makes major progress toward) 055's `plcfrs.py`
blocker (`set.__sub__`) too, and vice versa.
