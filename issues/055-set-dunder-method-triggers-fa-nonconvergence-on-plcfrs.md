# 055 — Adding `set.__sub__` triggers FA non-convergence / compiler crash on plcfrs.py

**Status:** open, found 2026-07-19 while attempting a followup to
[053](053-tuple-unpack-target-heterogeneous-arity-segfault.md):
`plcfrs.py` (line 300-301) calls `set(...) - set([...])`, and
`__pyc__/08_set.py`'s `class set` had no `__sub__`/difference operator
at all (a plain missing-feature gap, unrelated to 053's tuple-arity
bug). Added `__sub__` (plus `__isub__`/`intersection`/`__and__`/
`union`/`__or__` for completeness) — this made the *compiler itself*
segfault (or, with a trivial no-op body, hang past a 30s timeout
instead) compiling the full `plcfrs.py`. Reverted (`git checkout --
__pyc__/08_set.py`); NOT shipped. `set.__sub__` is still missing.
**Affects:** FA's fixed-point convergence (`ifa/analysis/fa.cc`'s
`analyze_to_convergence`), specifically whatever drives the
edge/send/EntrySet worklists — root cause not yet isolated past the
bisection below.
**Related:** [053](053-tuple-unpack-target-heterogeneous-arity-segfault.md)
(this was found while following up on that issue — `plcfrs.py` is the
same corpus program; 053's fix itself is unaffected and was verified
clean once `__pyc__/08_set.py` was reverted). See also
[057](057-sorted-tolist-fa-nonconvergence.md), found later the same
day: the identical FA non-convergence signature (worklist churn
without bound, EntrySet count flat), but with a dramatically smaller
(4-line, no 500-line real program needed) and dict/`sorted()`-based
repro — likely the better starting point for whoever roots-causes
this, since fixing one likely fixes (or substantially informs) the
other. 057 also landed a wall-clock stagnation *mitigation* (bounds
FA's inner flow loop, converting a hang into a clean failure) — but
re-tested directly against this issue's own `plcfrs.py` repro
(`__sub__` returning `self` unconditionally) and it does **not**
help here: that repro still segfaults in ~7s, the same fast crash
this issue originally documented, not the slow-hang pattern 057's
guard targets. The two issues' triggers share a root-cause *family*
but apparently not the exact same failure mode — 057's mitigation
does not close this issue.

## Symptom

`./pyc -D . plcfrs.py` (the real shedskin example, not a synthetic
repro) segfaults the compiler process itself — not the generated
program, not a reported type violation, the `pyc` binary crashes with
no diagnostic output — after `__sub__` is added to `class set` in
`__pyc__/08_set.py`. Confirmed via `PYC_DBG_PHASE`-gated instrumentation
(temporarily added to `pyc.cc`, `ifa/ifa.cc`, `ifa/analysis/fa.cc`,
removed once isolated — see repo convention of printf-bisection over
gdb, which reliably hangs in this sandbox even on trivial binaries
like `/bin/echo`) that the crash is inside `FA::analyze`'s first
`analyze_to_convergence()` pass (pass 1, after a clean pass 0):

```
PHASE:   pass=1 ess.n=501
PHASE:     edges=20000 sends=12452 ess=1034 ess.n=501
PHASE:     edges=40000 sends=22029 ess=2825 ess.n=501
PHASE:     edges=60000 sends=30417 ess=4867 ess.n=501
PHASE:     edges=80000 sends=43809 ess=6811 ess.n=501
[segfault]
```

`ess.n` (the number of distinct EntrySets, `fa->ess.n`) stays flat at
501 while `edges`/`sends`/`ess` (the count of edge_worklist /
send_worklist / es_worklist *pops*, i.e. work items processed, which
can revisit the same ES many times) keeps climbing linearly with no
sign of leveling off — the same ~501 EntrySets are being
re-constrained over and over. That shape (worklist churn growing
without bound while the underlying ES count stays fixed) is the
signature of a lattice that isn't monotonically converging — some
AType/CreationSet is oscillating rather than settling to a fixed
point — not just "a lot of legitimate work."

## What's known (isolated by bisection)

- **Baseline (no `__sub__`) is clean.** With `__pyc__/08_set.py`
  reverted to its pre-this-investigation state, the exact same
  instrumented build compiles `plcfrs.py` to completion (still
  reporting the pre-existing line-591 `illegal call argument
  type`/`expression has no type` diagnostics and failing with
  `PYC_FAIL` under default `runtime_errors=true` semantics — that's
  the known, separate, still-open gap, not this bug) — confirms
  `__sub__`'s addition is the trigger, not an artifact of the
  instrumentation itself.
- **Not about the method body.** Reducing `__sub__` to a trivial
  `def __sub__(self, other): return self` (no iteration, no
  `set()` construction, no `__contains__` call) still triggers the
  same runaway worklist growth — it degrades from a segfault to a
  30-second timeout (`edges` still climbing past 140000 with no
  convergence in sight) rather than disappearing. This rules out the
  specific recursive-construction body (`r = set(); for item in
  self: ...`) as the cause; **merely giving `set` a `__sub__` method
  at all** is sufficient.
- **Isolated minimal repros do NOT reproduce this.** Neither
  `set(nt for rule, weight in grammar for nt in rule) - set([...])`
  nor the doubly-nested-unpack variant (`set(nt for (rule, yf),
  weight in grammar for nt in rule) - set([...])`, mirroring
  `plcfrs.py`'s actual line 300) trigger any slowdown or crash in
  isolation — both compile cleanly and instantly. This is a
  scale/interaction effect specific to `plcfrs.py`'s full complexity
  (multiple hundred-plus EntrySets already in play from the rest of
  the program), not a shape a small synthetic file can reproduce.
- **Leading hypothesis (not confirmed):** Python's binary `-`
  operator is dispatched generically — every `a - b` call site in the
  whole program must consider every type defining `__sub__` as a
  candidate callee. Adding `set.__sub__` means every one of
  `plcfrs.py`'s many *unrelated* integer/float subtractions (e.g.
  `rule.lengths[x] - 1`, bit arithmetic in `nextset`/`nextunset`,
  `len(a) - 2`, etc. — plcfrs does arithmetic subtraction throughout)
  now has one more polymorphic candidate to resolve per call site,
  which could combinatorially interact with per-contour cloning
  (`clone_methods_per_cs`) or CreationSet specialization in a way
  that doesn't converge for this program's size/shape. Not verified —
  would need instrumentation inside `analyze_edge`/`add_es_constraints`
  to confirm which specific ES/AType is oscillating, which wasn't
  attempted (out of scope for a same-day investigation on top of
  053).

## Why not fixed now

Root-causing *why* FA fails to converge (as opposed to observing
*that* it doesn't) needs deeper instrumentation than the phase-level
bisection already done — likely dumping the specific AType/CreationSet
that's flip-flopping pass-over-pass, a nontrivial follow-on
investigation in its own right. `set.__sub__` is a real missing
feature (not just for `plcfrs.py` — any program wanting `-`/difference
on sets), so this is worth fixing properly rather than working around
with something narrower (e.g. a differently-named method that isn't
`__sub__`, avoiding generic operator dispatch — untested, but would
dodge the actual bug rather than fix it, and wouldn't give real
`set() - set()` syntax).

## What this unblocks

Real `set() - set()` (and `-=`, `&`, `|`, `.intersection()`,
`.union()`) support — currently `__pyc__/08_set.py` has none of these
(only `.discard()`/`.remove()`/`.update()`). `plcfrs.py` specifically
needs `__sub__` to get past its next blocker after 053's fix (though
per 053's own note, `plcfrs.py` has at least one more distinct
"heterogeneous tuple arity" gap beyond this before it fully compiles
regardless).
