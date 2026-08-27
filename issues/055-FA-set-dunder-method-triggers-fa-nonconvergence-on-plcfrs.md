# 055 — Adding `set.__sub__` triggers FA non-convergence / compiler crash on plcfrs.py

**Status:** open — the ROOT CAUSE IS FIXED (`PYC_CSSPLIT`, default on, 2026-08-26) and the minimal repro converges; plcfrs itself still does not, so this stays open on that. Found 2026-07-19 while attempting a followup to
[053](closed/053-tuple-unpack-target-heterogeneous-arity-segfault.md):
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
**Related:** [053](closed/053-tuple-unpack-target-heterogeneous-arity-segfault.md)
(this was found while following up on that issue — `plcfrs.py` is the
same corpus program; 053's fix itself is unaffected and was verified
clean once `__pyc__/08_set.py` was reverted). See also
[057](closed/057-sorted-tolist-fa-nonconvergence.md), found later the same
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

## Fix direction (2026-07-29)

See [057](closed/057-sorted-tolist-fa-nonconvergence.md)'s "Fix direction —
AUTHORITATIVE" section: this same non-convergence class is fixed by
(1) monomorphizing the affected functions so no polluting type union
forms and (2) enforcing that every new contour is *productive* (must
realize a monomorphic specialization that does not already exist) —
NOT by widening / a CPA_LIMIT valve. The signature this issue records
(`ess.n` flat at 501 while worklist pops climb without bound) is the
same non-productive-contour-creation disease: the same contours are
re-constrained / re-derived rather than the analysis refining toward a
finite monomorphic fixed point. Whoever fixes 057 should re-test this
`plcfrs.py` + `set.__sub__` repro; the fix is expected to help here
too (or reveal the one extra dimension `plcfrs.py`'s scale adds).

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


## Re-verified 2026-08-26 — trigger unchanged, everything else stale

### `set.__sub__` shipped

The issue says "Reverted; NOT shipped. `set.__sub__` is still missing."
That is no longer true: `__pyc__/08_set.py` now defines `difference`,
`__sub__`, `intersection`, `__and__`, `union`, `__or__`, and they work
in ordinary programs —

```python
a = set(); a.add(1); a.add(2); a.add(3)
b = set(); b.add(2)
print(len(a - b), len(a & b), len(a | b))   # 2 1 3, correct
```

So the feature landed and plcfrs was left as the casualty.

### The symptom is no longer a crash

`./pyc -D . plcfrs.py` does not segfault and does not hang. It finishes
in ~13 s and reports. What it does *not* do is converge:

| | final_pass | pass_limit_hit | CONVERGED | violations | ess | css |
|---|---|---|---|---|---|---|
| with `set.__sub__` | 39 | **1** | **0** | 4378 | 1246 | 4075 |
| with it removed | 13 | 0 | **1** | 104 | 258 | 1100 |

So the non-convergence this issue is about is intact; only its
*presentation* improved (bounded failure instead of a crash), presumably
from the pass cap and 073's splitter work.

### Removing `__sub__` makes plcfrs COMPILE

Without it: **rc=0**, warnings only. With it: 774 `has mixed basic types`
errors and `fail: program does not type`. plcfrs still contains the
`set(...) - set(["Epsilon", "ROOT"])` at line 301 either way — without
`__sub__` that call has no candidate and is salvaged as one of the 34
`illegal call argument type` warnings.

Shipping the *correct* method is what turned plcfrs from compiling into
failing. (Its failure is now dominated by BOXING, fatal in every mode
since [../../issues/018](../../issues/closed), so even a convergence fix
would not by itself make plcfrs compile.)

### The leading hypothesis is REFUTED

The issue proposes that adding `set.__sub__` gives every one of plcfrs's
many unrelated arithmetic `a - b` sites one more polymorphic candidate.
Tested directly, and it is wrong:

- **`__sub__` on a class plcfrs never uses costs nothing.** With
  `set.__sub__` removed and a trivial `bytearray.__sub__` added in its
  place — the exact same "one more candidate at every `-` site" — plcfrs
  converges identically to baseline: `final_pass=13`,
  `pass_limit_hit=0`, `CONVERGED=1`, `violations=104`, `ess=258`.
- **`set.__and__` is already defined and is harmless**, though plcfrs
  has 6 `&` sites against 9 `-` sites — comparable fan-out, no effect.
- **`set` is not in the degenerate union.** The 774 boxing errors all
  name `( list tuple int64 float64 str dict ChartItem Edge Rule Entry )`
  — no `set`. Whatever degenerates, it is not set polluting the
  arithmetic sites.

What survives is much narrower: it is `set.__sub__` **being reached from
plcfrs's own `set - set`**, so that the set `difference` builds
(`r = set()`, `r.add(self._items[i])`) flows onward — into `sorted(...)`
and then `+` with a list, at line 299-302.

### Where to look instead

That is the shape of [057](closed/057-sorted-tolist-fa-nonconvergence.md)
(generic `sorted()` across differing element types plus `list()`
materialization) and [105](105-type-degeneration-in-shared-generic-methods.md),
not generic operator dispatch. 057 is closed and its fix explicitly did
not resolve this, but the *call shape* it describes is the one plcfrs
hits here, and it is now reachable through a freshly-constructed set
rather than through the operator table.

Next step: trace what element type `difference`'s `r` acquires across
passes on plcfrs, and what `sorted()` does with it — the operator-dispatch
line of inquiry is closed off by the bytearray control above.


## Minimal repro FOUND, 6 lines (2026-08-26)

`tests/set_ops_chained_mixed_elem_types.py`, with a `.known_issue`
sidecar; the `.exec.check` holds CPython's `2 / 1 / 2`, so it flips to
PASS by itself when this is fixed. 0.72 s to compile.

```python
ai = set([1, 2, 3]); bi = set([2]); ci = ai - bi
print(len(ci))
as_ = set(["x", "y"]); bs = set(["x"]); cs = as_ - bs
print(len(cs))
di = ci - bi
print(len(di))
```

    final_pass=52  pass_limit_hit=1  CONVERGED=0  violations=56

This supersedes "Isolated minimal repros do NOT reproduce this" and
"a scale/interaction effect specific to plcfrs.py's full complexity".
It is not scale. The earlier attempts missed it because they were built
from the operator-dispatch hypothesis (refuted above), so they varied
the *number of `-` call sites* and never chained a result.

### Two ingredients, both necessary

| | passes | converged |
|---|---|---|
| 1 element type, chained | 13 | yes |
| 2 element types, no chain | 32 | yes |
| 4 element types, no chain | 35 | yes |
| **2 element types, chained** | **52** | **no** |

1. **Two or more element types** across the call sites, and
2. **a chain** -- a difference *result* fed back into difference.

### It has nothing to do with `__sub__`

`&`, `|`, and a plain `.difference()` method call all reproduce the same
6-line failure:

    a - b     final_pass=52  CONVERGED=0
    a & b     final_pass=52  CONVERGED=0
    a | b     final_pass=56  CONVERGED=0
    a.difference(b)  final_pass=53  CONVERGED=0

So this issue's title is wrong: it is not a *dunder* and not operator
dispatch. It is any set method that BUILDS AND RETURNS A FRESH SET.
`difference`, `intersection` and `union` share one shape:

```python
def difference(self, other):
    r = set()          # <-- ONE creation site, shared by every caller
    ...
    r.add(self._items[i])
    return r
```

### Mechanism

`r = set()` is a single creation site shared by every call site in the
program, so with two element types flowing through `self`, `r`'s element
type is their union. Chaining then makes that shared site's element type
an input to itself: `r`'s elements come from `self`, and `self` at the
second call site *is* a previous `r`. Each pass re-derives the union
through the same shared CS, contours re-split, and no fixed point is
reached.

That is the empty/shared-container-CS family --
[072](072-FA-empty-container-notype-current-mechanism-and-plan.md),
[105](105-type-degeneration-in-shared-generic-methods.md) -- and the
non-productive contour creation named in
[057](closed/057-sorted-tolist-fa-nonconvergence.md)'s fix direction, now
reachable in six lines instead of five hundred.

### Do we need the repro? Yes

plcfrs: 39 passes, 4378 violations, ess=1246, ~13 s, and a 500-line
program to read. This: 52 passes, 56 violations, ess=149, 0.72 s, six
lines. The plcfrs repro should stay as the integration check, but no
root-cause work should start from it.


## Instrumented: it is a period-2 limit cycle, and the splitter is chasing the symptom

`PYC_DBG_CONTOURS=<fun>` (analysis/fa.cc, env-gated) prints, at the end
of every pass, one line per EntrySet of that function with its formals'
and return types; `PYC_DBG_CONTOURS='*'` prints the per-function contour
counts and the ess/css totals. Run against
`tests/set_ops_chained_mixed_elem_types.py`.

### It does not diverge — it oscillates

    pass 0..23   total_ess climbs 61 -> 148, css 532 -> 656
    pass 24      total_ess=146  css=656
    pass 25      total_ess=149  css=656
    pass 26      total_ess=146  css=656
    ...          146, 149, 146, 149, ... to the pass cap

`total_css` is pinned at 656 from pass 20 on. Three EntrySets are
created and destroyed on alternate passes, forever. This is a **period-2
limit cycle**, not unbounded growth — the shape
[099](099-FA-pending-backedge-avoid-veto-forces-period-2.md) and
[074](074-FA-cross-pass-oscillation-plan.md) describe.

### What flips

The ideal contours here are obvious and small: `difference`,
`add`, `__contains__`, `__eq__` each specialized per element type,
`int64` and `str`. FA *derives* them — and then loses them again:

| | pass 44 (good) | pass 45 (bad) |
|---|---|---|
| `add` es=93 | `[set, int64]` | `[set\|set, int64\|str]` |
| `add` es=94 | `[set, str]` | `[set\|set, int64\|str]` |
| `__contains__` es=98 | `[set, int64]` | `[set\|set, int64\|str]` |
| `__contains__` es=99 | `[set, str]` | `[set\|set, int64\|str]` |
| `__eq__` es=140 | `[int64, int64]` | `[int64, int64\|str]` |
| `__eq__` es=145 | `[str, str]` | `[str, int64\|str]` |
| `__eq__` es=148 | `[int64, int64]` | `[int64, int64\|str]` |

On the good pass these are exactly the monomorphic contours we want. On
the next pass the **receiver widens from ONE `set` CreationSet to two**
(`set|set`), and the element argument unions to `int64|str` behind it.
The splitter responds by splitting again — `__eq__` goes 8 contours ->
11 — which restores the good state, and the cycle repeats.

`difference` itself settles at 3 contours and `set_CSs` at 7 (ai, bi,
as_, bs, plus one fresh `r` per difference contour), so the fresh-set
creation sites ARE separated. The instability is downstream of that: an
already-split contour is re-admitted a second receiver CS on the next
pass.

### Where the analysis fails, in one sentence

**The split contours are not stable across the pass boundary** — a
contour that was monomorphic on pass N accepts both receiver
CreationSets on pass N+1, so every split is undone and re-made forever.
The splitter keeps paying for `__eq__`/`__contains__`/`add` splits that
cannot stick, which is the non-productive contour creation named in
[057](closed/057-sorted-tolist-fa-nonconvergence.md)'s fix direction:
a new contour must realize a monomorphic specialization that does not
already exist, *and an existing one must not be re-widened*. The second
half is what is missing here.

That points at contour reuse/compatibility being scored against a
per-pass snapshot — [097](097-CGEN-callsite-vs-clone-formal-type-mismatch.md)'s
hazard and [098](098-FA-per-pass-reset-scoped-to-reachable-set.md)'s
per-pass reset — rather than at anything about sets.

### Why shedskin does not have this

shedskin monomorphizes `difference` as a C++ template instantiated per
element type, so there is no shared `r` and no union for a splitter to
chase — see [[shedskin-template-monomorphization]] in the corpus notes:
it has no union representation either, which is why its diagnostics, not
its typing, are the part worth copying. The contours pyc needs here are
the same ones a template instantiation would produce; pyc derives them
and then throws them away.

### Next step

Find what re-admits the second receiver CS into an already-monomorphic
contour on the following pass. `PYC_DBG_CONTOURS=add` narrows it to
es=93/94 between passes 44 and 45, which is a two-contour, two-type
window — small enough to trace edge by edge.


## Traced to the field: `_items` cross-contaminates between instances

`PYC_DBG_BIND=<fun>` (fa.cc, on `set_entry_set` -- the one chokepoint
both the mint and reuse routes pass through) logs every edge->EntrySet
binding with the edge's ACTUAL argument types. That moved the diagnosis
upstream twice.

### It is not the splitter, and not ES selection

    pass 44   e=171 REUSE es=93  actuals=[add set int64]
              e=226 REUSE es=94  actuals=[add set str]
    pass 45   e=171 REUSE es=61  actuals=[add set int64|str]
              e=226 REUSE es=61  actuals=[add set int64|str]

Both edges' **actuals are already unioned** on the bad pass, at the call
site `r.add(self._items[i])` inside `difference`. So the polymorphic
contour is not chosen badly -- the value arriving is already wrong. And
`append`'s contours widen with **no BIND line at all** on those passes:

    pass 44   append es=115 [list, int64] -> list
              append es=116 [list, str]   -> list
    pass 45   append es=115 [list|list|list, int64|str] -> list|list|list
              append es=116 [list|list|list, int64|str] -> list|list|list

No rebinding: the widening arrives purely through the flow graph.

### The field is what flips

Dumping each `set` CreationSet's data fields with CreationSet ids:

| set CS | pass 44 | pass 45 |
|---|---|---|
| 1032 | `_items=list#1034` | `_items=list#1034\|1171\|1172` |
| 1138 | `_items=list#1171` | `_items=list#1034\|1171\|1172` |
| 1140 | `_items=list#1172` | `_items=list#1034\|1171\|1172` |
| 1139 | `_items=list#1171` | `_items=list#1171` (stays clean) |
| 1141 | `_items=list#1173` | `_items=list#1173` (stays clean) |

On the good pass every set instance owns its own backing list. On the
bad pass three distinct instances all hold the *same three* lists. That
union is where `int64|str` comes from, and everything above is
downstream of it.

So the causal chain, measured end to end:

    _items cross-contaminates across set CSs
      -> self._items[i] is int64|str
      -> add / __contains__ / __eq__ actuals are unioned before dispatch
      -> splitter splits them into monomorphic contours (the good pass)
      -> field contamination re-forms next pass
      -> period-2 limit cycle

The splitter is chasing a symptom three levels downstream of the defect.

### Not set-specific

`dict` has the same write-back shape (`self._keys = self._keys.append(key)`,
07_dict.py:150-151) and a dict analogue of the repro reproduces exactly:
`final_pass=52 pass_limit_hit=1 CONVERGED=0`.

The write-back is not a style choice and cannot simply be deleted:
`list.append` calls `_CG_list_resize`, which may reallocate, so the
field must be reassigned.

### What did NOT reproduce (so the recipe is still incomplete)

Hand-written user classes with the same shape all CONVERGE:

| | passes | converged |
|---|---|---|
| `self.items = self.items + [x]`, chained copies | 20 | yes |
| ... plus an element-dispatched `==` scan, as set has | 27 | yes |
| ... using `self.items = self.items.append(x)` instead | 22 | yes |

So "write-back + element compare + chain" is not sufficient on its own.
The builtin containers carry some further ingredient this reconstruction
does not, and identifying it would give a repro with no `__pyc__/`
involvement at all.

### ifa/113 checked, INCONCLUSIVE

The obvious suspect for two instances' fields merging is
[113](113-FA-setter-equivalence-is-a-global-batch-partition.md)'s global
setter partition. Dumping `AVar::setter_class` for every `_items` AVar
gives `nil` on both passes, with 0 distinct classes -- but that dump runs
after `complete_pass()`, so it cannot distinguish "these AVars never
participate in setter classing" from "the class was already cleared".
**No link to 113 is claimed.** Re-check by sampling inside the pass.

### Next step

Find the assignment that writes the three-list union into `_items`:
instrument the incoming flow edges of the `_items` AVar of set CS 1032
and report, per pass, which source AVar contributes each of list#1034,
#1171, #1172. That is one field, one CreationSet, one pass boundary.


## ROOT CAUSE: one CreationSet for `r = set()` across all contours of `difference`

### Correction to the previous section

It said "difference itself settles at 3 contours and set_CSs at 7 (ai,
bi, as_, bs, plus one fresh `r` per difference contour), so the
fresh-set creation sites ARE separated". **That is wrong.** It was
inferred from the count 7 without checking which CreationSet was which.
Printing the ids says otherwise:

    pass 44/45, identical:
      difference es=49   [set#1138, set#1139]  ->  set#1032
      difference es=137  [set#1140, set#1141]  ->  set#1032
      difference es=138  [set#1032, set#1139]  ->  set#1032

All three contours return **the same CreationSet, set#1032**. The seven
set CSs are the four literals (ai/bi/as_/bs = #1138/#1139/#1140/#1141),
the single shared `r` (#1032), and two field-less class/meta CSs
(#689, #973). There is exactly ONE `r`, not one per contour.

### The defect

`r = set()` inside `difference` is a single creation site, and
**contour splitting duplicates the EntrySet but not the CreationSet the
site allocates.** So every specialization of `difference` -- the int one,
the str one, and the chained one -- allocates the same abstract set.
Two consequences, and the second is what makes it unfixable by
splitting:

1. set#1032's element type is necessarily `int64|str`: it is the union
   of what every contour puts in it.
2. es=138 takes set#1032 as its receiver AND returns set#1032. The
   chain makes the shared creation site **its own input** -- which is
   exactly why a chain is one of the two required ingredients, and why
   one element type alone is harmless.

### How that produces the period-2 cycle, end to end

    r = set() shares ONE CreationSet across difference's contours
      -> set#1032 carries int64|str and (via the chain) feeds itself
      -> set#1032 leaks into the monomorphic `add` contours:
             pass 44  add es=93 [set#1138]              es=94 [set#1140]
             pass 45  add es=93 [set#1032|set#1138]     es=94 [set#1032|set#1140]
      -> `self._items = self._items.append(item)` stores through a
         TWO-CS receiver, so it writes BOTH sets' fields, merging their
         backing lists:
             pass 44  cs1032 _items=list#1034   cs1138 _items=list#1171
             pass 45  cs1032 = cs1138 = cs1140 = list#1034|1171|1172
      -> self._items[i] is int64|str, so add/__contains__/__eq__ take
         unioned actuals
      -> the splitter splits them back to monomorphic contours (pass 44)
      -> set#1032 leaks in again next pass
      -> 146 <-> 149 EntrySets, forever

The splitter is working correctly at every step. It cannot win, because
the thing that needs separating is a CreationSet at an allocation site,
and it only ever separates EntrySets.

### Why this is not what 040/045 fixed

`clone_methods_per_cs` / PER_CS_RECEIVER (issue 045, 040's fix) key on
the **receiver** CreationSet of a method. Here the receiver is fine --
es=49 and es=137 have cleanly separated receivers on every pass. What
is shared is the CreationSet the method **creates**. That is issue
[072](072-FA-empty-container-notype-current-mechanism-and-plan.md)'s
territory (a container CS shared across the call sites that allocate
it), reached from a different direction.

`creation_point()` (fa.cc) keys a CS on `(AVar, Sym)` with a split
lookup through `es->split`; the question for a fix is why that lookup
does not give the three `difference` contours three distinct `r`s.

### Why shedskin does not have it

A C++ template instantiation of `difference` per element type gives each
instantiation its own `r` by construction -- the creation site is
duplicated along with the code. pyc duplicates the contour but shares
the allocation.

### Next step

Instrument `creation_point()` for the `r = set()` site: log the AVar,
the contour, `es->split`, and the CS returned, per pass, and find why
all three `difference` EntrySets resolve to CS #1032.


## Why the CS is not split with the ES, and what happens when it is

### The answer

`creation_point()` (fa.cc) has six routes to a CreationSet. In order:

1. `v->cs_map` memo -- per AVar, so per (Var, contour). Fine.
2. **split-parent inheritance** -- `if (es && es->split)`, look up the
   parent contour's CS and reuse it. **This is the one that fires.**
3. the `s->creators` loop -- **dead code**:
   `if (nvars != -1 || x->vars.n != nvars) continue;` always continues
   (when `nvars == -1` the second test is `x->vars.n != -1`, true for
   any `vars.n >= 0`), so it can never reach `cs = x`.
4. cselem (ifa/101, `PYC_CSELEM`, default 0) -- needs `s->element`,
   null for a Python-level class like `set`.
5. csmold (ifa/101, `PYC_CSMOLD`, default 1) -- also needs `s->element`
   at level 1, and excludes `clone_methods_per_cs` classes.
6. mint.

So route 2 hands every split child of `difference` its parent's
CreationSet. The other splitter stages DO run and DO split the contour
containing the creation point -- `PYC_DBG_STAGES` reports
`TYPE_CONFL SETTER SETTER_OF_SETTER`, and `difference` ends with three
EntrySets. The ES split happens; the CS just does not follow it.

### The exemption existed but was unreachable

Route 2's own comment says instances of `clone_methods_per_cs` classes
must not take it, citing 040's `range(0,0)` vs `range(0,2)` merge. But
that flag is set in exactly one place -- `python_ifa_build_syms.cc`,
when a class's `__init__` has a `__pyc_clone_constants__` parameter --
and `IFA_DBG_CSMINT` reports `cmc=0` for `set`. `set.__init__(self)`
and `dict.__init__(self)` take no arguments at all, so they can never
qualify, even though their instances need separating by ELEMENT type
rather than by constant. The comment at the flag's assignment predicts
the consequence exactly: *"otherwise shared method contours write
through the union and widen every sibling's fields"*.

### `PYC_CSSPLIT=1`: correct, and not yet shippable

Added, **default 0**. At 1, route 2 is skipped so a split child mints
its own instance CS.

| | default | PYC_CSSPLIT=1 |
|---|---|---|
| the 6-line repro | 52 passes, CONVERGED=0, 56 violations | **28 passes, CONVERGED=1, 0 violations** |
| its output | does not compile | **`2 1 2`** (= CPython) |
| dict analogue | CONVERGED=0 | **CONVERGED=1** |
| plcfrs | 4378 violations, ess 1246 | 2451 violations, ess 850 (still CONVERGED=0) |
| pyc suite | 296 passed / 14 known | **297 passed / 0 failed / 13 known** |
| corpus (77 programs) | 67 compile | **64 compile** |

Bounded, not a new growth source: split products are found durably
across passes (`find_or_make_filtered_entry_set` searches `fun->ess`)
and `cs_map` survives `clear_avar`, so a split child mints once and
memoizes. Cost on the repro is +16% CreationSets (656 -> 760).

### The three corpus losses are latent bugs in the phases AFTER FA

    pylife      rc=0 -> rc=1     new type errors
    softrender  rc=0 -> rc=1     new type errors
    pystone     rc=0 -> crash
    sunfish     rc=124 -> rc=1   IMPROVED (was timing out)

pystone crashes in three different places in succession, each a missing
null guard reached only because there are now more Funs:

1. `optimize/dom.cc` `build_call_dominators`: `f->dom->succ.add(ff->dom)`
   with no guard, while the CFG loop 14 lines above already does
   `if (pp->dom)`. Only `fa->funs` are given a Dom, but `calls_funs`
   can name a Fun outside that set. **Fixed here** -- independently
   correct.
2. `optimize/inline.cc` `global_frequency_estimation`:
   `f->loop_node->dfs_ancestor(...)` where `dfs_order` walks the call
   graph from top but `loop_node` is only assigned to the funs
   `find_all_loops` covers. **Fixed here** -- frequency estimation only
   feeds inlining heuristics, so skipping an unknown ancestry relation
   costs estimate precision, never correctness.
3. `codegen/cg.cc` `emit_send_call`: `fputs()` of a null name for a Var
   that reached codegen without one. **Not fixed** -- this is where
   pystone stands.

Both fixed guards are kept regardless of the flag: they are guarding
against something that was already possible.

### Where this leaves it

The mechanism the issue needs now exists and is one flag away. What
stands between `PYC_CSSPLIT=1` and default-on is not FA -- it is that
the post-FA phases have never run at this contour count. Next: the
cg.cc null name for pystone, then re-examine pylife's and softrender's
new type errors, which may be real imprecision the extra CSs expose or
may be the same class of latent gap one phase further on.


## FIXED at the root: the CreationSet now follows the EntrySet split

`PYC_CSSPLIT` **defaults to 1** as of 2026-08-26. Route 2 of
`creation_point()` (split-parent inheritance) is skipped, so a split
child mints its own instance CS instead of adopting its parent's.

    6-line repro   52 passes, pass_limit_hit, CONVERGED=0, 56 violations
                -> 28 passes, CONVERGED=1, 0 violations, prints 2 1 2
    dict analogue  CONVERGED=0 -> CONVERGED=1
    pyc suite      296 passed / 14 known -> 297 passed / 0 failed / 13
                   known, both backends
    corpus         67 of 77 compile, program for program, unchanged --
                   and sunfish improves (400 s timeout -> clean failure)
    plcfrs         STILL does not converge: 45 passes, pass_limit_hit,
                   but 4378 -> 2451 violations and ess 1246 -> 850

`tests/set_ops_chained_mixed_elem_types.py` passes and its
`.known_issue` sidecar is removed; it stays as a regression test.

Three `fa-converge` goldens were re-blessed (`iterator_copy`,
`iterator_missing_field`, `vector_iterator`). All three changed the same
way and all three are improvements: **3 passes -> 2**, the setter split
stage no longer needed, with identical `rc`, final `css` and violation
counts. Fewer passes to the same answer is exactly what this change is
about, which is the "correct behaviour, stale fixture" case.

### Three latent defects it exposed, all fixed, all on the default path

Splitting more contours reached code paths the later phases had never
run at. None of these were caused by the change; each was reachable
before and simply had not been reached.

1. **`optimize/dead.cc`, `mark_live_funs`.** Liveness propagates through
   `f->calls` and marks callees live, including callees not in
   `fa->funs` (which holds only the functions the LAST pass reached).
   The rebuild then filtered the INCOMING list, so such a function
   stayed live and UNLISTED. **One cause behind three crashes in three
   phases**: no `Dom` (dom.cc), no `loop_node` (inline.cc), no name
   (cg.cc's `fputs(NULL)` on a live call). Confirmed at the codegen
   site: `target->live == 1`, name null, `Proc1` from `Proc0`.
2. **`analysis/clone.cc`, `determine_layouts`.** Field sizes were
   resolved per CreationSet, so a field bottom in one contour
   contributed 0 bytes and shifted every later field -- two CSs of the
   SAME class with different layouts. Measured on pylife: 13 `LifeNode`
   CSs, field `id` at offset 32 in eleven and 24 in two, after which
   `prim_period_offset` rejects any union receiver mixing them. Resolved
   once per field Sym, which is shared across a class's CSs.
   (`fail("missmatched offsets")` now names the class, field and each
   CS's offset; it previously said nothing.)
3. **`codegen/cg.cc`, both subscript prims.** A container subscript was
   never cast. FA can give an index formal a float type -- softrender's
   `(srcX + srcY * src.width) * 4` where FA cannot see through `int()`.
   With one shared contour the float ACTUAL was cast to the int64 formal
   at the call boundary; split per contour, the FORMAL is float64 and
   the body emits `t0->v[t_float]`, which C rejects. Casting at the read
   site is the same coercion the call boundary already performed.

### What is still open

plcfrs. It converges no better than before in kind -- still
`pass_limit_hit` at 45 passes -- though with 44 % fewer violations and
32 % fewer contours. Whatever remains there is not this defect, and the
6-line repro no longer reproduces it, so plcfrs needs its own
`PYC_DBG_CONTOURS` trace from scratch.


## plcfrs reduced to 9 lines — and it is a DIFFERENT defect

`tests/dict_pair_swap_setdiff_nonconvergence.py`, with a `.known_issue`
sidecar; `.exec.check` holds CPython's `2`.

```python
def f(grammar):
    nts = list(set(nt for rule, weight in grammar for nt in rule) - set(["a", "b"]))
    pairs = list(enumerate(nts))
    toid = dict((lhs, n) for n, lhs in pairs)
    tolabel = dict((n, lhs) for n, lhs in pairs)
    return toid

rules = [(("S", "VP2"), 1.0)]
print(len(f(rules)))
```

    final_pass=51  pass_limit_hit=1  CONVERGED=0  58 violations
    deterministic across 3 runs

Reduced from plcfrs.py by delta-debugging, 638 lines -> 9. The parser is
not involved at all: everything reachable from `parse()` was cut, then
`splitgrammar()` was cut to its first three statements.

### Four ingredients, each verified necessary

Removing any ONE of these converges:

| ablation | result |
|---|---|
| `for rule in grammar` instead of `for rule, weight in grammar` | converges |
| drop the set difference | converges |
| drop `enumerate()` | converges |
| drop the second dict | converges |
| **both dicts the SAME orientation** | **converges** |

That last row is the sharp one: it is not "two dicts", it is two dicts
built from the SAME pairs with **swapped key/value** -- `dict[str,int]`
and `dict[int,str]`. Not needed: `sorted()`, and the
`["Epsilon","ROOT"] + ...` list concatenation plcfrs wraps around it.

### It is not the defect this issue's other repro had

`PYC_CSSPLIT` fixed the shared-`r = set()` CreationSet, and
`tests/set_ops_chained_mixed_elem_types.py` passes. This one still
fails with that fix in place, and shares only the set difference --
which here is one of four necessary ingredients rather than the
mechanism. Whatever drives it, splitting the CS with the ES does not
address it.

The plcfrs-scale numbers moved with the fix (violations 4378 -> 2451,
ess 1246 -> 850) but the shape did not: still `pass_limit_hit` at 45
passes. Its trajectory is a period-2 cycle at ess 545 <-> 575 for passes
27-39, then a jump to 811 at pass 40 and growth to the cap -- so there
may be more than one thing left in plcfrs itself. The 9-line repro is
the place to start.


## The expected contours, why we do not get them, and what shedskin does

### Expected

For the 9-line repro, by hand:

    __pyc_dict_from_iterable__  x2   pairs=list[tuple[str,int]]
                                     pairs=list[tuple[int,str]]
    dict::__new__               x2   one per caller contour
    => two dict CSs: dict[str,int] and dict[int,str]

### What we get

    CONTOUR __pyc_dict_from_iterable__ es=113 args=[list#1053] ret=dict#1182
    CONTOUR __pyc_dict_from_iterable__ es=157 args=[list#1178] ret=dict#1182
    CONTOUR __new__                    es=115 args=[dict#663]  ret=dict#1182

The CALLER splits correctly -- two contours, distinct argument lists.
`dict::__new__` does **not**: one contour for the whole program, so one
instance CS, and its fields conflate key with value --

    dictCS cs=1182 _keys=list#1059|list#1184|list#1185
                   _vals=list#1059|list#1184|list#1185

identical unions in both slots.

### Why

Contour splitting is **type-directed on the callee's formals**, and at
`dict::__new__` there is nothing to direct it. The bind trace:

    BIND __new__ e=172 MINT  es=115 from_es=113 line=2152 actuals=[dict#663]
    BIND __new__ e=277 REUSE es=115 from_es=157 line=2152 actuals=[dict#663]

One call site (`d = dict()`, line 2152), two caller contours, **identical
actuals**, differing only in `from_es`. `dict()` takes no arguments, so
the two edges are indistinguishable to a type-based splitter. The
information that would separate them -- what will later be stored into
the instance -- does not exist at the constructor; it appears only after
the instance flows back to the caller.

Contrast `__list_iter__::__new__`, which gets **eight** contours in the
same program: its argument is the list being iterated, so it differs per
call and type-directed splitting separates it for free.

`PYC_CSSPLIT` cannot help here. It makes a split CHILD mint its own CS,
and there is no split: `IFA_DBG_CSROUTE=dict` reports 2 MINT and 91
`cs_map`, and **zero** `split_parent`.

### shedskin, for reference — it has no such contour to split

Same file, `shedskin translate m7.py`, **1.62 s** (pyc hits the 51-pass
cap):

```cpp
dict<str *, __ss_int> *toid;      // dict[str,int]
dict<__ss_int, str *> *tolabel;   // dict[int,str]
toid    = (new dict<str *, __ss_int>(new list_comp_1(pairs)));
tolabel = (new dict<__ss_int, str *>(new list_comp_2(pairs)));
```

`dict<K,V>` is a template, so the allocation is emitted **at the use
site with (K,V) already resolved from the value flow**. There is no
shared constructor contour to split, because there is no shared
constructor -- `new dict<str*,__ss_int>` and `new dict<__ss_int,str*>`
are different types by construction. The comprehensions are likewise
separate functions (`list_comp_1`, `list_comp_2`), monomorphic for the
same reason.

That is the difference in one line: **shedskin derives container
identity from the element types; ifa derives it from the allocation
context.** Where the two disagree -- an allocation whose context is
identical but whose contents differ -- ifa has nothing to split on.

### The two available policies pull in opposite directions

|  | set repro | this repro | plcfrs |
|---|---|---|---|
| `PYC_CSSPLIT=1` (default; mint per split child) | **fixed** | not fixed | 2451 viol |
| `PYC_CSMOLD=2` (share one CS per allocation SITE) | **breaks** | **fixed** | 4664 viol |

`PYC_CSMOLD` is ifa's port of shedskin's *mold fallback* (its comment
says so), but only the half that shares; at its default level 1 it is
gated on `s->element`, which a Python-level class like `dict` or `set`
does not have -- their element types live in the `_keys`/`_vals`/
`_items` list FIELDS. Raising it to 2 lifts that gate and converges this
repro, at the cost of the set one.

Neither "always split" nor "always share" is right. The shedskin answer
is neither: identity from the CONTENTS. ifa's nearest equivalent is
`cselem` (ifa/issues/101), which keys a CS on its converged element
type -- and it does not fire here for exactly the same reason
(`PYC_CSELEM=1` and `=2` both still hit the cap), because `dict` has no
`s->element`.

### Fix direction

Extend element-keyed CS identity (`cselem`) to class-based containers by
keying on the FIELD types rather than on `s->element`, so a `dict` whose
`_keys` is `list[str]` and `_vals` is `list[int]` is a different
CreationSet from one with those swapped -- which is precisely
`dict<str*,int>` vs `dict<int,str*>`. That derives identity from the
value flow, as shedskin does, instead of from the allocation context,
and would not need the constructor to be split at all.


## Why the demand-driven setter back-flow does not fire here

The mechanism exists and is the right one. `update_setter` (fa.cc)
propagates a setter **backward** along flow edges:

```c
av->setters = new_setters;
for (AVar *x : av->backward) if (x) (void)update_setter(x, s, avs);
```

and `split_for_setters` then does exactly the two steps in order --
split the entry sets along the path, then split the CreationSet:

```c
collect_setter_confluences(avs, setter_confluences, setter_starters);
if (split_ess_setters(setter_confluences)) return 1;   // split the PATH
...
if (split_css(setter_starters)) return 1;              // split the CS
```

It does not fire on this repro, for three reasons that stack.

### 1. Zero setters exist. The machinery never starts.

Measured at convergence, same instrumentation both runs:

| | avars with setters | with setter_class | CSs with >1 def |
|---|---|---|---|
| this repro (fails) | **0** | **0** | 1 |
| the set repro (fixed) | 142 | 100 | 2 |

Not one AVar in the failing program carries a setter.

### 2. Why no setters: the field is not a type CONFLUENCE

`compute_setters` is seeded only from the type-confluence list (the
`ifa_selective` branch widens it to every CS-contoured AVar, but
`selective=0` by default):

```c
if (ifa_selective) { ... every CS-contoured AVar ... }
else
  for (AVar *av : confluences) (void)compute_setters(av, avs, AKIND_TYPE);
```

and `collect_type_confluence` calls an AVar a confluence only when some
*single* backward edge carries strictly less than the AVar's own
incoming type:

```c
if (type_diff(av->in->type, x->out->type) != bottom) confluences.set_add(av);
```

dict#1182's `_keys` holds `list#1059|list#1184|list#1185` -- but the
union is formed UPSTREAM and arrives whole on its edges, so no single
edge is strictly smaller, and the field is not a confluence. No
confluence, no `compute_setters`, no setter, no demand.

### 3. Even with setters, `split_css` could not act: one def

```c
for (CreationSet *cs : css) {
  Vec<AVar *> starter_set ...          // the CS's own defs
  while (starter_set.n > 1) { ... }    // partition by setter class
```

`split_css` **partitions a CreationSet's existing creators**; it cannot
manufacture one. dict#1182 has `defs=1` -- the single
`dict::__new__` contour's result AVar -- so the loop body never runs
however the setters class out. (In the fixed set repro the separation
likewise came from having several set CSs each with `defs=1`, not from
splitting one.)

### The circularity

    to split the CS   you need >= 2 defs
    to get >= 2 defs  you need dict::__new__ split into >= 2 contours
    to split __new__  the type-directed splitter needs differing formal
                      types -- and `dict()` takes no arguments

`split_ess_setters` is precisely the step that would break this by
splitting the path, and it is ordered before `split_css` for exactly
that reason. It is starved by (1): with no setters there are no setter
confluences to split at.

### Two things worth trying, in order

1. **Seed setters more widely.** The `ifa_selective` branch already
   computes setters for EVERY CS-contoured AVar rather than this pass's
   confluences, and its comment says the wider coverage is affordable
   ("M1 measured the whole `extend` phase at ~0.4% of FA time"). Running
   that unconditionally would give the dict's fields setters and let the
   back-flow start. Cheap to test.
2. If the back-flow then reaches the creation point and still cannot
   split it, the missing capability is duplicating a creation point
   under caller-contour demand -- which is the same thing as splitting
   `dict::__new__` per caller, and is what the field-typed CS identity
   in the previous section achieves without any splitting at all.


## Yes, it can be demand-driven — the demand is raised and then starved

### The upstream AVar IS split-worthy, and IS collected

Walking backward from the dict's `_keys` field (`PYC_DBG_BACKWALK`):

```
BACK av=6702 _keys@cs1182:dict nback=3 out=list#1059|list#1184|list#1185
  BACK av=4274 ?@es116:__init__      out=list#1184                      <- strictly smaller
  BACK av=6728 ?@es120:__setitem__   out=list#1059|list#1184|list#1185
  BACK av=6776 ?@es203:__setitem__   out=list#1059|list#1184|list#1185
```

The `__init__` edge carries ONE list where the field holds three, so
`type_diff(av->in->type, x->out->type) != bottom` and `_keys` is a
genuine type confluence. The log confirms it -- av 6702 appears as
`[confluence] av 6702 _keys [CS/other] list list list` **30 times**.

So the earlier reading ("the union is formed upstream, so the field is
not a confluence") was wrong. It is one. The demand IS raised.

### What starves it: the quiescence gate

`extend_analysis` runs its stages behind a cascade of
`if (!analyze_again)`:

```c
if (!analyze_again) { collect_type_confluences(confluences);
                      analyze_again = split_ess_for_type(confluences, ...); }
if (!analyze_again) { CARTESIAN_PRODUCT }
if (!analyze_again) { MARK_TYPE }
if (!analyze_again) { ... compute_setters over confluences ...   // SETTER
                      split_for_setters(...) }
```

The SETTER stage runs **only on a pass where the type splitter found
nothing**. On this program the type splitter finds work on every pass
that matters, so `compute_setters` is never called on av 6702 -- probe
`PYC_DBG_SETTERSEED=_keys` counts **0** invocations across the whole
run. Hence zero setters program-wide, hence no setter confluence, hence
`split_ess_setters` splits no path and `split_css` gets no starters.

This is the same starvation the PER_CS_RECEIVER comment already
documents for stage 6 ("TYPE_CONFLUENCE fires every pass on
plcfrs/rdb/sudoku5, so `!analyze_again` is never true and the receiver
fan never runs at all") -- one stage earlier, and load-bearing.

### Proof: lift the gate and the mechanism works

`PYC_SETTERGATE=1` (added, **default 0**) lifts it exactly as
`PYC_RECVFAN=2` lifts stage 6's:

| | default | PYC_SETTERGATE=1 |
|---|---|---|
| compute_setters on `_keys` | 0 calls | **10 calls** |
| this repro | 51 passes, CONVERGED=0, 58 viol | **26 passes, CONVERGED=1, 0 viol** |
| set repro | converges | converges |
| plcfrs | 2451 viol, CONVERGED=0 | 2455 viol, CONVERGED=0 (unchanged) |
| pyc suite | 297 / 0 failed | 295 / **3 failed** |

So the answer to "can we demand-drive it" is **yes** -- the back-flow
does the right thing the moment it is allowed to run.

### Why this is not the fix as it stands

Two reasons, both real:

1. **It does not fix plcfrs.** Whatever else is left there is not this.
2. **The three failures are semantic, not stale fixtures.**
   `tests/splitter_*.py` pin *which splitter stages a program demanded*
   (ifa/074's "one stable bit a convergence test can assert").
   `splitter_type_confluence.py` expects `STAGES: TYPE_CONFL` and gets
   `TYPE_CONFL SETTER` -- because with the gate lifted the setter stage
   runs unconditionally, so the recorded set stops meaning "demanded"
   and starts meaning "ran". Re-blessing would destroy the property the
   tests exist to measure.

### The targeted version

Do not lift the gate globally. Escalate **per confluence**: when
`split_ess_for_type` makes progress but a given CS-contoured confluence
is still un-split after N passes, run `compute_setters` on that one and
let the back-flow split its path. That keeps the stage demand-driven --
which is what the splitter_*.py tests assert and what the gate is there
to protect -- while letting a confluence the type splitter provably
cannot resolve reach the machinery that can.


## The real defect: a stage that claims progress and creates nothing

The previous section's conclusion -- "move the setter stage earlier" --
was treating a symptom. The right question was why the EARLIER stages
never quiesce, and the answer is a two-line bug.

### Measured

From pass 24 to the cap at 50 on the 9-line repro:

    total_ess = 189   total_css = 806    FLAT, 27 passes
    STAGEDELTA TYPE_CONFL returned=1 confluences=104 d_ess=0 d_css=0

`split_ess_for_type` reports "analyze again" on **every** pass while
creating nothing at all. That keeps `analyze_again` true, and since
every later stage sits behind `if (!analyze_again)`, all of them --
SETTER, PER_CS_RECEIVER, CSM -- are starved. The zero-setters finding
above is a CONSEQUENCE of this, not an independent problem.

### Why it claims progress

All three `split = 1` sites in `apply_entry_set_split` fire on an edge
being RE-POINTED (`set_entry_set(x, scomp)`, `set_entry_set(x,
product)`, `x->to != es`). None requires a contour to be created. So
re-routing an edge between two EXISTING EntrySets counts as progress.

### What is actually re-routing

    [churn-ledger] p=45 e=400 es=147 -> 223
    [churn-ledger] p=46 e=400 es=223 -> 147
    [churn-ledger] p=47 e=400 es=147 -> 223

One edge, `__contains__`, ping-ponging between two EntrySets forever.
The `[churn-look]` lines give the cause: the SAME group signature
`gsig=4103446528` is recorded in the split ledger **twice**, with each
EntrySet as the other's product --

    p=45  es=147  gsig=4103446528  found=1 pass_made=20  product=223
    p=46  es=223  gsig=4103446528  found=1 pass_made=18  product=147

so "route this group to its durable home" says 147 -> 223 AND
223 -> 147. A cycle by construction, and the ledger is the thing that
was supposed to make re-derived groups land somewhere stable.

### Already had a fix, defaulted off

`PYC_ROUTECYCLE` (ifa/issues/101, *"detect and break 2-cycles in the ES
split ledger"*) is exactly this cycle-breaker. It was default 0. **Now
default 1.**

| | before | after |
|---|---|---|
| this repro | 51 passes, CONVERGED=0, 58 viol | **32 passes, CONVERGED=1, 0 viol, prints 2** |
| set repro | converges | converges |
| pyc suite | 297 passed / 14 known | **298 passed / 0 failed / 13 known**, both backends |
| corpus | 67 of 77 | **67 of 77**, program for program; sunfish improves (400 s timeout -> clean failure) |
| ifa-test goldens | -- | unchanged |
| plcfrs | 45 passes, 2451 viol, ess 850 | 36 passes, **5353 viol, ess 1524**, still CONVERGED=0 |

`tests/dict_pair_swap_setdiff_nonconvergence.py` passes and its
`.known_issue` sidecar is removed.

### The one thing that got worse

plcfrs's violation count nearly doubles (2451 -> 5353) and its contour
count rises, though it needs fewer passes (45 -> 36) and does not
compile either way. Breaking the 2-cycle lets the analysis proceed
further into territory it had never reached, and it finds more to
complain about there. That is not evidence the change is wrong -- the
corpus is at parity and the suite improved -- but plcfrs's remaining
non-convergence is now a different, larger problem than before, and
should be re-traced from scratch rather than compared to the old
numbers.

### What this retires

`PYC_SETTERGATE` (added in the previous commit) is kept as a probe but
is no longer a candidate fix: it worked by routing AROUND the churn
rather than fixing it, and it broke the `splitter_*.py` stage-demand
assertions in doing so. With the cycle broken, the setter stage is
reachable on its own.


## Generalizing the cycle breaker: acyclic routing, not "never revisit"

Breaking a 2-cycle by pattern is not general -- `A->B->C->A` is the same
disease with three signatures, and `route_last`'s one-step memory cannot
see it. Two generalizations were implemented and measured.

### Mode 3 (now the default): keep the route relation ACYCLIC

`FA::route_adj` records the whole route relation; before routing
`es -> product`, ask whether `product` already reaches `es`. If it does,
the route would close a cycle of any length -- pin to the cycle's
lowest-id member, the same policy mode 2 applies to the 2-cycle case.
Mode 1's check is exactly the depth-1 special case of this.

| | mode 1 | **mode 3** |
|---|---|---|
| the 9-line repro | 32 passes, CONVERGED=1 | **31 passes**, CONVERGED=1 |
| pyc suite | 298 passed / 0 failed / 13 known | **identical** |
| corpus | 67 of 77 | **67 of 77**, program for program |
| plcfrs | 5353 viol, ess 1524 | **4993 viol, ess 1420** |
| ifa-test goldens | -- | unchanged |

Strictly >= mode 1 everywhere measured, so it is the default.

**Honest limit: on every program measured, only 2-cycles actually
occur.** m7 finds one, plcfrs finds two, both length 2. The extra reach
is insurance against a shape not yet observed, not a demonstrated win.

### Mode 4: "prevent any reassignment to a previous EntrySet" is WRONG

The stronger and more obvious rule -- never route a source to an
EntrySet it has been routed to before -- was implemented and it fails:

    6 suite failures, including two HARD compile failures
    (test_heapq.py, tuple_compare.py), plus
    iterator_protocol_bridge, match_seq, match_map_star,
    recursive_polymorphic

The reason is worth keeping: **repeating the SAME route every pass is
the ledger working**, not churning. In the steady state a re-derived
group is routed to its established home on every pass; that is exactly
what the ledger exists to do. Only a return to a home the group has
ABANDONED is pathological -- and "would close a cycle" is precisely that
distinction. "Never revisit" conflates the two and refuses the stable
case along with the cyclic one.

Mode 4 is kept behind the flag so the difference stays measurable rather
than argued.


## Second plcfrs repro, 36 lines — a different shape from the first

`tests/plcfrs_grammar_tables_nonconvergence.py`, `.known_issue` sidecar,
`.exec.check` holds CPython's `3`. Delta-debugged from plcfrs.py again,
638 lines -> 36, AFTER `PYC_CSSPLIT` and `PYC_ROUTECYCLE` landed.

    final_pass=41  pass_limit_hit=1  CONVERGED=0  152 violations
    deterministic across 3 runs, 1.4 s

The first repro (`dict_pair_swap_setdiff_nonconvergence.py`) now passes,
and this one **does not need the set difference at all** -- so it is a
genuinely different shape, not the same defect resurfacing.

The reduction path also moved: the old ladder's 53-line rung now
converges, while the 200-line rung still fails, so the surviving failure
lives in the part of `splitgrammar()` the old reduction had cut -- the
grammar loop that builds the tables, not the nonterminal prologue.

### Seven ingredients, each verified necessary

Removing any ONE converges:

| | |
|---|---|
| nested tuple unpack `for (rule, yf), weight in grammar` | flattening it converges |
| `enumerate()` over the nonterminal set | |
| two dicts over the same pairs, **swapped** key/value | `dict[str,int]` + `dict[int,str]` |
| `array("B", ...)` arity vector indexed by a Rule field | |
| `Rule` as a **class** | a plain tuple in its place converges |
| the round-trip comparison `yf == arraytoyf(args, lengths)` | |
| the `zip()` generator inside `arraytoyf` | |

Only the swapped-dict pair carries over from the first repro.

### What is NOT needed, though plcfrs has it

`sorted()`, the `["Epsilon","ROOT"] + ...` concatenation, **the set
difference**, the lexicon loop, `Terminal`, the four per-nonterminal
rule lists, the conditional third `Rule` argument, and -- notably --
**distinct array typecodes**: making both arrays `"I"` instead of
`"I"`/`"H"` still fails, so this is not the two-parameter-instantiation
shape it first looked like.

### Where the mechanism stands

plcfrs still shows the same top-level signature as before the fixes:

    STAGEDELTA TYPE_CONFL returned=1 confluences=301 d_ess=0 d_css=0

`split_ess_for_type` again reports progress while creating nothing, so
every later stage is starved -- but the ledger route is now acyclic, so
the 2-cycle explanation no longer applies. The churn probe shows the
ledger route still firing heavily in late passes (248 fires at p>=30)
along with 2006 mint lookups. Whatever re-points those edges is the next
thing to find, and the 36-line repro is where to find it.


## What re-points the edges: NOTHING in the splitter — it is field promotion

Traced on the 36-line repro (1.4 s), and it overturns the previous
section's framing.

### 1. It is not a hang, and not the pass limit

    STALL LIMIT reached at pass 38, 56 violations (best 44):
      3 re-deriving (limit 8), 32 non-improving (limit 32); stopping

`IFA_PASS_LIMIT` is 100. The analysis stops at 38 because the **stall
guard** fires: 32 consecutive passes that never improved on the best
violation count. `pass_limit_hit` is the guard's flag, not a pass cap.

### 2. The violation trajectory: it settles, then is wrecked, twice

    p=0   78
    p=4-6 44     <- settled
    p=7   325    <- 7x worse
    p=12  152 ... p=30 52   (20+ passes clawing back)
    p=31  419    <- 8x worse again
    p=34-38 57..56   -> stall guard stops

### 3. At the moment it is wrecked, every splitter is quiescent

    PASSEND p=6  extend=1  reanalyze=0  viol=44
    REANALYZE again=1 notype_promote=2 eager_promote=0 coerce=0
    PASSEND p=7  extend=0  reanalyze=1  viol=44     <- extend_analysis() = 0
    PASSEND p=8  extend=1               viol=325

`extend_analysis()` returns **0** at pass 7 -- TYPE_CONFLUENCE, SETTER
and every other stage are settled at 44 violations. The pass happens
because `IFACallbacks::reanalyze()` asks for it, and the flow that
follows produces 325.

So the ledger routing, the scomp path and the mint churn measured
earlier are all DOWNSTREAM NOISE: the splitter is re-pointing edges
because promotion keeps handing it a changed program, not because it is
stuck.

### 4. Proof

`PYC_NOPROMOTE=1` (probe, skips both `promote_field` paths in
`PycCompiler::reanalyze`, keeping the numeric coercion):

| | default | NOPROMOTE=1 |
|---|---|---|
| the 36-line repro | 41 passes, stall, 152 viol, ess 415 | **8 passes, CONVERGED=1, 44 viol, ess 88** |
| **plcfrs itself** | 37 passes, stall, 4993 viol, ess 1420 | **11 passes, CONVERGED=1, 138 viol, ess 257** |

**plcfrs converges** without field promotion. This is the whole of its
remaining non-convergence.

### 5. But promotion cannot simply be removed

    pyc suite, PYC_NOPROMOTE=1:  192 passed, 108 FAILED

Promotion is load-bearing (issues/026's third bug: writes that land in
`cs->unknown_vars` must reach `var_map` or dispatch over union receivers
constant-folds). Turning it off trades non-convergence for 108 broken
programs.

### 6. Why it does not settle

Promotion is individually monotone -- each field moves from
`unknown_vars` to `var_map` once -- but each promotion re-runs the
analysis, and the re-run exposes MORE fields to promote:
`notype_promote=2`, then `7`, then `5`, ... Each round multiplies the
violation count before it comes back down, and the stall guard's
"non-improving" metric counts that as failure. Raising the budget does
not help: `IFA_STALL_LIMIT=200` stops at the same pass with the same
numbers, so the non-improving counter is not the only limiter.

### Fix directions, in order of appeal

1. **Promote to a fixed point BEFORE splitting matters.** The
   promotions are discoverable from `cs->unknown_vars` without a full
   re-analysis between each round; doing them all up front (or looping
   promotion alone until quiescent) would remove the interleaving that
   makes violations oscillate.
2. **Make the stall guard aware of reanalyze.** A pass whose only
   change came from a frontend repair is expected to look worse; it
   should not advance the non-improving counter. This is a smaller
   change but treats the symptom.
3. Promotion currently fires from NOTYPE violations *and* eagerly over
   every CS. If the eager path is complete, the NOTYPE-driven path is
   redundant and is what ties promotion to the violation set -- worth
   measuring whether path (1) alone suffices.
