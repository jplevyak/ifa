# 057 — Generic `sorted()` across differing element types + `list()`-materialization causes FA non-convergence

**Status:** root cause still open, but the *symptom* (unbounded hang
+ unbounded memory growth) is MITIGATED as of 2026-07-19: FA's
flow-to-fixpoint inner loop now fails cleanly with a diagnostic
after a bounded stall instead of hanging/OOMing forever (see
"Mitigation landed" below). Found 2026-07-19 while testing the
same-day `dict.keys()`/`.values()`/`.items()` fix
([../../issues/025](../../issues/025-shedskin-examples-coverage.md)).
Not `dict`-specific and not caused by that fix — it's a pre-existing,
general FA convergence bug that a natural "exercise every code path"
test for the new dict methods happened to trip. Same *class* of bug
as [055](055-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
(FA's fixed-point loop churns worklists without bound while the
EntrySet count stays flat), but this repro is dramatically smaller —
4 lines, no `plcfrs.py`-scale program needed — making this the better
issue to use if someone picks up root-causing the underlying FA bug.
**Affects:** FA's fixed-point convergence (`ifa/analysis/fa.cc`'s
`analyze_to_convergence`) — same symptom location as 055, root cause
not yet isolated past the bisection below.

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

## Mitigation landed (2026-07-19): bounded stagnation timeout in the flow-to-fixpoint inner loop

Root-causing *why* the type union never stabilizes would need
deeper instrumentation (dumping which specific AType/CreationSet is
growing pass-over-pass) than this round had budget for — matching
the scale of investigation [033](033-splitter-non-idempotent-divergence.md)
required for the *outer* splitting loop's analogous disease (that
issue alone runs to ~2000 lines and spanned weeks). But a real,
scoped, low-risk improvement was still worth landing: **the inner
flow-to-fixpoint loop (`analyze_to_convergence`'s edge/send/es
worklist drain) had no bound at all**, unlike the *outer*
`extend_analysis()` splitting loop (which already has `pass_limit`
and the issue-033 stall guard). A non-convergent input churns this
inner loop forever — confirmed via this issue's repro: still running
past 280s, RSS past 1GB and climbing, `fa->ess.n` (distinct
EntrySets) completely flat at 97 the entire time, no diagnostic ever
printed.

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
continuing to churn. This is robust to the per-edge cost problem
above (a slow but still-progressing pass just keeps resetting the
clock) and ties the trigger to the exact signature this bug class
shows (workqueue churn with a flat EntrySet count), not to raw
volume.

**Calibration**: measured the largest known-converging corpus
example, `pygasus` (issue 033's own historical worst case) with the
same instrumentation. Its busiest single pass processes ~65K edges
while `fa->ess.n` grows by hundreds *within that same pass* (973 →
4832 across the run) — nowhere near 120s of zero growth. Verified
directly: `pygasus` still reaches its own separate, pre-existing,
unrelated C-compile-error (`_CG_strcat` given an `int64` argument —
not investigated, not this issue's concern) in ~80s, identically
with and without this change, across repeated runs. (One run
produced a segfault at ~10s that did not reproduce across 5
subsequent attempts on the same input/build — matches a documented,
never-explained, non-reproducible flake already on record in issue
033's M3 section; not attributable to this change.)

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
message now, rather than hanging). The root cause — why FA's type
union for the shared `sorted()`/`tuple.__lt__` contours never
stabilizes across the two very different element-type call sites —
remains exactly as open as before. This issue exists to (a) record a
much cheaper, cleaner repro of the same underlying bug class than
055's `plcfrs.py`-scale one, and (b) document a real landmine:
**`sorted()` on both string and dict-items()-derived data in the
same program is a plausible, fairly ordinary thing to write**, unlike
055's `set.__sub__`-on-a-500-line-program trigger — so this is more
likely to bite a real corpus example than 055 was. Worth
prioritizing over 055 if only one gets picked up for the actual root
cause.

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
