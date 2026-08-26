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
