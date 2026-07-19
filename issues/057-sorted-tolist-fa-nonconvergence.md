# 057 — Generic `sorted()` across differing element types + `list()`-materialization causes FA non-convergence

**Status:** root cause FOUND 2026-07-19 (see "Root cause" below);
not fixed — the real fix is a genuine architecture feature (a
CPA-style cap/widening valve) that issue
[033](033-splitter-non-idempotent-divergence.md) already anticipated
needing (its S4-D section) but never built. The *symptom* (unbounded
hang + unbounded memory growth) is separately MITIGATED as of the
same day: FA's flow-to-fixpoint inner loop now fails cleanly with a
diagnostic after a bounded stall instead of hanging/OOMing forever
(see "Mitigation landed" below) — this mitigation predates the root
cause finding and its own write-up contains one inaccuracy about
*why* it works, corrected in the root-cause section. Found 2026-07-19
while testing the same-day `dict.keys()`/`.values()`/`.items()` fix
([../../issues/025](../../issues/025-shedskin-examples-coverage.md)).
Not `dict`-specific and not caused by that fix — it's a pre-existing,
general FA architecture gap that a natural "exercise every code path"
test for the new dict methods happened to trip. Same *class* of bug
as [055](055-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
(FA's fixed-point loop churns worklists without bound), but this
repro is dramatically smaller — 4 lines, no `plcfrs.py`-scale program
needed — making this the better issue to use for the real fix.
**Affects:** `ifa/analysis/fa.cc`'s `find_best_entry_sets` /
`entry_set_compatibility` / `set_entry_set` (the EntrySet reuse-vs-
create decision for a call edge) — see "Root cause" below.

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

**The actual mechanism**: `find_best_entry_sets` (`fa.cc`), given a
call edge, scans every existing `EntrySet` in the callee's
`Fun::ess` and asks `entry_set_compatibility` whether the edge's
argument types are compatible with reusing that contour. If **none**
score positively, `find_best_entry_sets` returns 0 and the caller
falls through to `set_entry_set(e)` with no `es` argument, which
unconditionally mints a **brand new** `EntrySet` (`new
EntrySet(e->match->fun); e->match->fun->ess.add(new_es);`) — with no
cap, and no fallback to *widen* an existing contour to accept the new
argument type combination instead of forking a fresh one. For
`tuple.__lt__`, called with elements drawn from `sorted()`'s shared
(and itself never-stabilizing) union of `str` and `tuple(str,str)`
values, the incoming argument types apparently never exactly match
any previously-created contour — so every single call mints a new
one, which itself becomes one more (always-incompatible) candidate
for the next call to scan past, which is *also* why the per-edge cost
grows over time (documented in the mitigation section): each new
edge does a linear scan of an ever-growing, always-failing candidate
list before minting yet another EntrySet.

**This is precisely the gap issue 033's own S4-D section
anticipated but never built**: *"D. CPA_LIMIT-style deferral valve
(safety net for sketch (d); small). In pattern_match, before
find_best_matches: compute `candidates x prod(per-position class
counts)`; if above a limit, return 0 through the `incomplete_call`
path... Escalate at quiescence: when extend_analysis finds no work
AND capped sends exist, double the FA-level limit and re-enqueue
them."* Shedskin's own architecture (033's own comparison table)
has exactly this valve (`CPA_LIMIT`, lazy doubling) precisely because
unbounded per-call-site specialization is a known failure mode of
this style of analysis, not a `pyc`-specific bug — `find_best_entry_sets`
is the `pyc` analog of shedskin's `cpa()`, and it's missing the
admission/widening control shedskin's has.

**Why not fixed now**: implementing a real CPA-style cap +
widen-on-quiescence valve is a genuine architecture feature (033's
own S4-D calls it "small," but every other S4/S5 milestone in that
issue that looked small at the time needed a full multi-day
land-verify-revert cycle against the whole corpus — M2 alone was
attempted and reverted twice). Out of scope for landing in this
session; the mitigation below already converts the failure mode from
"hangs / OOMs forever" to "fails cleanly in ~2 minutes with a
diagnostic," which is the practically important half for anyone
hitting this today. The architecture fix is the natural next step
for whoever picks this up — S4-D's own sketch is a reasonable
starting design, and `find_best_entry_sets`/`entry_set_compatibility`
are now a confirmed, concrete implementation point (not a guess).

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
actually landed later the same day: `find_best_entry_sets` mints an
unbounded number of `EntrySet`s for `tuple.__lt__` because no
existing contour is ever judged "compatible enough" to reuse, with
no cap or widening fallback — the missing valve issue 033's own
S4-D section already predicted. This issue exists to (a) record a
much cheaper, cleaner repro of the same underlying bug class than
055's `plcfrs.py`-scale one, now with a confirmed root cause and
concrete implementation point (`find_best_entry_sets`/
`entry_set_compatibility`) for the real fix, and (b) document a real
landmine: **`sorted()` on both string and dict-items()-derived data
in the same program is a plausible, fairly ordinary thing to write**,
unlike 055's `set.__sub__`-on-a-500-line-program trigger — so this is
more likely to bite a real corpus example than 055 was. Worth
prioritizing over 055 if only one gets picked up for the actual fix.

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
