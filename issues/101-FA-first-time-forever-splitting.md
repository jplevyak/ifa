# 101 — the residual non-convergence is *first-time-forever* splitting, not re-derivation

> **PARTLY FIXED 2026-08-16** — `PYC_HARDREUSE=5` (durable-type-key reuse
> on the detach route, restricted to type-driven splits) is now the
> **default**. Corpus violations **7435 → 6399 (-13.9 %)**, `ess` lower on
> 41 of 77 programs, zero exit-code changes, zero `pass_limit_hit`
> changes, +1.5 % analysis time. `go`, `linalg` and `plcfrs` still do not
> converge, so the issue stays open — see "What the fix does not do".

**Status:** open, characterized 2026-08-16 after
[074](074-FA-cross-pass-oscillation-plan.md)'s two fixes
([066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md)'s
durable setter type and `PYC_SELFPROD=6`) cleared the reproducer. This
issue names what is left on the corpus, and why neither of those fixes
touched it.

## The set is three programs, not four

Of 77 shedskin programs, 8 do not produce a converged result. Five of
those are not FA problems at all:

| program | what it actually is |
|---|---|
| `minilight`, `quameon`, `tarsalzp` | multi-module programs pyc cannot load — `cannot find module 'ml'` / `'wave_func'` / `'com.github.tarsa…'`, failing in 0.3 s. Never reach FA. |
| `othello3` | 380 s and no `OSC` line — a separate mode, not yet characterized. |
| **`sudoku5`** | **converges at pass 47** once `IFA_STALL_LIMIT` / `IFA_NONIMPROVE_LIMIT` are raised, ending at its own minimum of 26 violations. Its apparent non-convergence is purely the *stall guard* cutting it off at pass 40. |

So the genuine set is **`go`, `linalg`, `plcfrs`** — which is exactly the
trio the 2026-08-13 census found, independently reproduced here.

## Shape: contours grow steeply, violations do not fall

Measured with the guards raised so the analysis runs to the pass cap:

| | ess growth | violations: first → min → last |
|---|---|---|
| `go` | 114 → 675 (**×5.9**) | 135 → **46** @p9 → 107 |
| `linalg` | 124 → 1195 (**×9.6**) | 182 → **21** @p26 → 100 |
| `plcfrs` | 144 → 1346 (**×9.3**) | 231 → **74** @p89 → 85 |
| `sudoku5` (converges) | 130 → 486 (×3.7) | 186 → **26** @p44 → 26 |

Two things stand out:

1. **`go` and `linalg` end far worse than their own minimum** — `go`
   bottoms at 46 violations on pass 9 and finishes at 107; `linalg`
   bottoms at 21 on pass 26 and finishes at 100. Ninety more passes and
   five to ten times the contours buy a *worse* answer. `plcfrs` is the
   benign one: it ends at 85 against a minimum of 74, still improving.
2. **Violations are wildly non-monotone.** `go`: 48 @p30 → 163 @p40 →
   240 @p50. `plcfrs`: 200 @p30 → **1311** @p40 → 481 @p50. `linalg`
   spikes to 465 at p60 from 27 at p50. This is not a fixpoint being
   approached slowly; it is thrashing.

## Why the 074 fixes do not apply: the splits are nearly all first-time

The decisive measurement. `REDERIVE-GROUP` counts a split whose group
signature the ledger had already recorded — i.e. work being redone —
against the total number of split decisions:

| program | split decisions | re-derived | **first-time** |
|---|---|---|---|
| `linalg` | 2128 | 101 | **95.3 %** |
| `go` | 655 | 21 | **96.8 %** |
| `plcfrs` | 4205 | 82 | **98.0 %** |
| `sudoku5` | 1285 | 10 | **99.2 %** |

The reproducer that 066 and `PYC_SELFPROD=6` fixed was the *opposite*
case: one signature, recorded once at pass 11, re-derived every pass for
ninety passes. The ledger could recognise it, and teaching it to do so
ended the oscillation.

Here there is nothing to recognise. **The splitter is inventing a
genuinely new partition almost every time it acts.** No amount of
ledger/routing/canonicalization work helps, because those mechanisms all
key on a repeat that never happens. That is why `PYC_SELFPROD=6` measured
as *exactly* inert across all 77 programs: it fires only on self-product
contours, and those are a rounding error here (`linalg`: 101 stale + 106
valid against 2128 decisions).

Stage attribution over the last 20 passes confirms where it comes from —
`TYPE_CONFLUENCE` dominates everywhere, with a high mint fraction:

```
go       TYPE_CONFL det=152 mint= 37 reuse=115   SETTER_OF_SETTER det=21 mint=20 reuse=1
linalg   TYPE_CONFL det=639 mint=276 reuse=363   SETTER csmint=39
plcfrs   TYPE_CONFL det=297 mint=120 reuse=177
sudoku5  TYPE_CONFL det=855 mint=101 reuse=754      <-- converges; 12% mint
```

The mint:reuse ratio tracks the outcome. `sudoku5` mints 12 % of its
redispatches and converges; `linalg` mints 43 % and grows ×9.6. Note also
`go`'s `SETTER_OF_SETTER`: 20 mints against 1 reuse — a stage that
essentially never reuses a contour.

## Fix direction

This is the shape [057](057-FA-nonconvergence-monomorphization.md)
names, and the author's recorded correction there applies: the answer is
**monomorphization plus a productivity invariant**, not widening and not
a bigger `CPA_LIMIT`. Concretely, what is missing is a rule that makes
splitting *earn* its contours — a split that does not reduce violations
(or does not reduce some other measure of imprecision) should not be
retained. Today nothing connects the two: `go` and `linalg` are allowed
to quintuple and decuple their contour counts while their violation
counts get worse.

The per-Var `violation_split_attempts` cap (issue 033 D6) is the existing
instance of that idea, but it is scoped to stage 5
(`split_for_violations`). `TYPE_CONFLUENCE` — which is where essentially
all of this growth comes from — has no equivalent.

## Verification plan

- `go`, `linalg`, `plcfrs` reach a fixed point with guards raised
  (`pass_limit_hit=0`), or terminate with a violation count at or below
  their measured minimum (46 / 21 / 74).
- `sudoku5` must keep converging, and its 47 passes should not grow.
- Full corpus A/B: no exit-code changes across the 77 programs
  (68 rc=0, 8 rc=1, 1 rc=134 as of 2026-08-16).

## Measurement note — a trap that cost a cycle here

`fa->type_violations` is cleared in `initialize_pass()` and populated by
`collect_var_type_violations()` **inside `extend_analysis()`**, which
runs *after* `complete_pass()`. A probe that samples it from
`complete_pass` therefore reports 0 or a stale tally, not that pass's
violations — the first version of the per-pass trace here read `viol=0`
for `go` on every pass while the `OSC` line for the same run said 104.
The `VIOL` line is now emitted at the collection site, which is the only
point in the pass where the count means anything.

**A second trap, same session:** the corpus sweep script captured
`rc=$?` after a pipeline —

```sh
osc=$(pyc ... | grep '^OSC' | tail -1); rc=$?   # WRONG: this is tail's status
```

— so `rc` was always 0 and the "zero exit-code changes" reported for the
066 and `PYC_SELFPROD` sweeps was measuring nothing. The metric columns
(`violations`/`ess`/`css`/`final_pass`/`pass_limit_hit`) come from the
`OSC` line and were unaffected, so those A/B conclusions stand. Re-run
with the exit code captured before the pipe, comparing the old defaults
(`PYC_CSKEY=0 PYC_SELFPROD=5`) against the new (`=3` / `=6`):

- **77 programs, zero exit-code changes**, distribution 68 `rc=0`,
  8 `rc=1`, 1 `rc=134`;
- zero changes to violations / ess / css / final_pass / pass_limit_hit;
- +0.1 % analysis time.

So the claim was correct, but it had not actually been measured until
this re-run. Sweep scripts must capture `$?` directly from the command,
never through a pipe.

The same off-by-one already applies by design to the `STAGE` line's
stage counters (they are incremented by `extend_analysis`, so the line
printed at pass N reports pass N-1's splitting); that one is documented
at `initialize_pass`.

## What this unblocks

The last three non-convergent corpus programs, and with them the ability
to treat `pass_limit_hit` as a genuine red flag rather than a mixed
signal. It also gates raising the stall guard: `sudoku5` shows the guard
is currently *masking* convergence that would happen a few passes later.


## Dug in 2026-08-16: half the contours are not justified by types

`IFA_DBG_KEYSPACE` compares each function's contour count (`ess`) against
its distinct argument-type tuples (`setkey`) and against a full
cartesian-product specialization (`cpakey`). On `linalg`:

| pass | ess | setkey | cpakey | redundant |
|---|---|---|---|---|
| 1 | 154 | 106 | — | 31 % |
| 40 | 676 | 370 | 536 | 45 % |
| 101 | 1290 | 648 | 957 | **50 %** |

Two conclusions, and the second is the sharp one:

1. Redundancy **grows** with pass count — by pass 101 half of `linalg`'s
   contours share an argument-type tuple with some other contour.
2. **`ess` (1290) exceeds full CPA (957) by 35 %.** CPA is the maximally
   precise per-callsite scheme, so a contour count above it cannot be
   justified by argument types at all. This is a hard upper bound being
   overshot.

The worst offenders are unambiguous — `__pyc_to_bool__#425` has **14
contours sharing one type tuple**, `__add__#1407` has 17. Dumping them:

```
[key] es=53   edges=188 filters=0 split=-1 | __pyc_to_bool__#56 | bool#3
[key] es=164  edges=170 filters=0 split=1128 | __pyc_to_bool__#56 | bool#3
[key] es=725  edges=1   filters=0 split=-1 | __pyc_to_bool__#56 | bool#3
...
```

All fourteen are **live** (1–188 edges), all `filters=0`, all
byte-identical types. Not dead leftovers, not filter-distinguished — just
callers scattered across interchangeable contours.

### Mechanism: the detach route

`make_entry_set` does `if (!split) find_best_entry_sets(...)`, so an edge
detached by a split is **never offered an existing contour**. Every
caller split therefore cascades into fresh callee contours, and nothing
ever merges them back.

### Why the previous repair failed, and what fixes it

`PYC_HARDREUSE=4` — reuse a contour whose durable type key matches — was
measured and rejected before. Re-measured under the current defaults it
breaks 1 test rather than 6, but on the corpus it is still not safe:

| | baseline | HR=4 | **HR=5** |
|---|---|---|---|
| `linalg` | ess 1195, viol 100 | 489, 78 | **722, 48** |
| `go` | 675, 107 | 534, **201** | 645, 107 |
| `plcfrs` | 1346, 85 | 1335, **269** | 1291, **74** |
| `sudoku5` | converges p47, 26 | **pass-limited, 273** | converges **p44**, 26 |

(guards raised, so every column runs to the same pass cap)

Mode 4 destroys `sudoku5`'s convergence outright. The reason is the one
already recorded in 074: *exact type identity is not evidence that a
contour is not what the split separated*. A **setter**- or **mark**-driven
split deliberately produces contours with identical argument types.

**`PYC_HARDREUSE=5` adds exactly that condition** — reuse only when the
split's own discriminator was argument types (`!fsetters && !fmark`, via
`cur_split_type_only`). Every mode-4 regression disappears, and three of
the four programs come out ahead of baseline.

Full corpus, default guards, 77 programs:

- **zero exit-code changes**, zero `pass_limit_hit` changes
- **violations 7435 → 6399 (-13.9 %)** — `plcfrs` -882, `sudoku5` -134,
  `sudoku3` -32, `kmeanspp` -2
- `ess` lower on **41** programs, higher on 1 (`ac_encode` +1); -2.3 %
- +1.5 % analysis time

The two apparent violation regressions are both benign. `softrender` is
+1 with *fewer* contours and an unchanged exit code. `linalg` reads 27 →
40 only because its stall guard fires nine passes earlier (pass 51 vs
60) — on equal footing, guards raised, it is **100 → 48**.

### What the fix does not do

`go`, `linalg` and `plcfrs` still hit the pass limit. Hard reuse removes
redundant contours; it does not stop the splitter inventing new
partitions, which is this issue's actual subject and is still
95–98 % first-time. The productivity invariant described above remains
the open work.

One test changed and was deliberately re-recorded rather than papered
over: `tests/splitter_cartesian_product.py` pinned
`STAGES: TYPE_CONFL VIOLATION PER_CS_RECV CPA` and now reads
`TYPE_CONFL CPA`. `VIOLATION` and `PER_CS_RECEIVER` are desperation
stages that run when earlier ones failed to resolve a violation; with the
redundant contours gone there is no longer a violation for them to chase.
Fewer stages is the improvement. The test's assertion is now "CPA fires,
reached through TYPE_CONFLUENCE alone", and a reappearance of the
desperation stages means something upstream regressed.


## Dug into the new-partitions problem, 2026-08-16

Four measurements, two of them negative results worth keeping.

### 1. Closure CreationSets dominate the population — and are irrelevant

`IFA_DBG_CSPOP` (new) reports the per-pass CreationSet population grouped
by allocation-site sym. On `linalg`:

```
CSPOP p=5   css=1262 syms=226 | closure:560  continuation:124 (anon):51 list:36
CSPOP p=60  css=1825 syms=224 | closure:1112 continuation:115 list:57 (anon):47
CSPOP p=101 css=1843 syms=224 | closure:1129 continuation:115 list:57 (anon):47
```

`closure` is **61 %** of all CreationSets and the only sym that grows —
everything else is flat, and the *number of allocation sites* never moves.
That is `creation_point`'s `if (s == sym_closure) goto Lunique`: closures
bypass CS reuse and get a unique CS per site × contour.

It looked like the engine. **It is not.** `IFA_DBG_SPLITSYM` reports the
CreationSet syms in each split's partition:

| program | splits | partitions containing a closure |
|---|---|---|
| `linalg` | 1207 | **0** |
| `go` | 474 | **0** |
| `plcfrs` | 2565 | **2** |

The splitter never partitions on closures. Their population is a
red herring for this issue.

### 2. What it does partition on: single container CreationSets

| program | size-1 partitions | top syms | top funs split |
|---|---|---|---|
| `linalg` | 903 / 1207 (**75 %**) | list 2067, int64 333 | `__getitem__` 292, `len` 155 |
| `go` | 417 / 474 (**88 %**) | list 203, int64 96 | `__getitem__` 60, `__eq__` 38 |
| `plcfrs` | 1452 / 2565 (**57 %**) | tuple 10103, list 3964 | `__getitem__` 625, `__eq__` 596 |

So the overwhelmingly common split is *"separate the edges whose argument
is exactly this one container CreationSet"*, at the generic accessors
`__getitem__` / `len` / `__eq__`. This is receiver monomorphization: one
contour per container instance per accessor.

### 3. Hard reuse moved `linalg` out of this class entirely

With `PYC_HARDREUSE=5` (previous section) `linalg` now **plateaus**:

```
pass:      1     10     20     40     60     80    100
ess :    235    469    499    598    700    722    722
css :   1106   1327   1544   1666   1833   1843   1843
viol:    193     69     90     62     42     48     48
```

Flat from pass 80. `go` and `plcfrs` still grow (565→633 and 1079→1274
over the same span), so they remain growth cases, but `linalg` is now a
**churn** case — and its stage trace is byte-identical every pass,
`TYPE_CONFL(det=10 mint=0 reuse=10)`, exactly the 074 reproducer's
signature at ten edges instead of one.

### 4. The ES ledger can hold a CYCLE

`IFA_DBG_CHURN` on `linalg`'s `__deepcopy__`:

```
[churn-look] p=98  es=792 gsig=16821760 found=1 pass_made=40 product=692
[churn-look] p=99  es=692 gsig=33861632 found=1 pass_made=58 product=792
[churn-look] p=100 es=792 gsig=16821760 found=1 pass_made=40 product=692
[churn-look] p=101 es=692 gsig=33861632 found=1 pass_made=58 product=792
```

Two signatures, each recording the **other** contour as its home. Both
records are individually honest; together they alternate to the pass cap.
Nothing detects or breaks this.

### Two repairs tried, both rejected

**`PYC_ROUTECYCLE=1`** — remember the last route target per contour and
break an `A→B` whose reverse `B→A` is already recorded, pinning to the
lower id. It detects the cycles, and it makes `linalg` **worse**: `ess`
722 → 901, violations 48 → 55, with `go`/`plcfrs`/`sudoku5` unmoved. Same
lesson as `PYC_ROUTEGATE` earlier in 074: **declining a route only means
minting instead** — keeping the group in place is not a no-op, because
the splitter still wants it split and takes the mint path.

**`PYC_GSIGRET=0`** — drop the return-type term from `group_signature`.
The term reads `x->rets[r]->lvalue->out->type`, which comes from whichever
callee contour the group currently points at, so it makes a group's
identity depend on the routing decision already made about it. That is a
real design smell and the obvious cause of two signatures for one group.
It measured **exactly inert** — byte-identical `final_pass`,
`violations`, `ess` and `css` on all four programs.

That negative result is informative: the two cycling signatures do **not**
differ in their return term, so they differ in the *argument* term — which
means `792`/`692` is most likely **two distinct groups swapping contours**
each pass, not one group cycling. Both flags default off/historical.

### Where that leaves it

At default guards all four programs terminate (passes 40–51), so the
practical cost of this issue is not the pass count but the **result
quality**: `go`, `linalg` and `plcfrs` exit 1 — they fail to compile.
That, not `pass_limit_hit`, is what closing this issue has to fix.

The next thing to test is the swapping-groups hypothesis directly: log the
edge sets behind the two signatures and confirm they are disjoint. **Done
below — it is refuted.**

## The swapping hypothesis is wrong; the churn is benign

Logging the edge identities behind both signatures:

```
[churn-look] p=100 es=792 gsig=567572480  product=692 edges=1 [ 2633 ] from=[ 797 ]
[churn-look] p=101 es=692 gsig=3499186688 product=792 edges=1 [ 2633 ] from=[ 797 ]
```

**The same single edge** (2633, from contour 797) in both. So it is one
group cycling, not two swapping. Combined with the inert `PYC_GSIGRET=0`
result, the alternating term must be the *argument* type — and
`__deepcopy__` is **recursive**, so its result flows back into the
caller's variable and feeds the next call's argument. The group therefore
has two argument-type states that each name the other's contour. **No
signature computed from current types can be stable through that.**

`PYC_ROUTECYCLE=2` accordingly tries to *pin* rather than decline: on
detecting `A↔B`, rewrite the ledger to the canonical (lower-id) contour
so both signatures agree, and leave the group alone when it is already
there. It is **worse than mode 1**:

| | baseline | mode 1 (decline) | mode 2 (pin) |
|---|---|---|---|
| `linalg` ess | 722 | 901 | **1002** |
| `linalg` violations | 48 | 55 | **96** |

`go`, `plcfrs` and `sudoku5` are unmoved by either. Two independent ways
of breaking the cycle both degrade `linalg`, which is consistent evidence
that **the ten redispatches per pass are load-bearing, not waste**. The
state they cycle between is flat (ess/css/violations identical from pass
80), and at default settings the stall guard terminates it. The churn is
benign.

## Scope correction: 018, not convergence, is what blocks these programs

At default guards all four programs terminate. What they actually do is
**fail to compile**, and all three of the genuine set fail in the *same*
family — [018](../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md), a union with no
codegen representation:

| program | failure |
|---|---|
| `go` | `member reference type '_CG_int64' is not a pointer` (×10) |
| `linalg` | `no matching function for call to '_CG_list_mult_internal'` (×5) |
| `plcfrs` | `sizeof_element of non-container type 'float64' (in __add__)` |

`linalg`'s failing contour is
`_CG_f_2524_16 /*list::__mul__*/(_CG_any a1, _CG_any a2)` — **both**
parameters unresolved, one of the 75 contours of `__mul__#2524`. Two
things ruled out about it:

- It is **not** a dead contour. Dumping all 193 `__mul__` contours found
  **zero** with `edges=0`, so this is not the "callee-side fan leaves
  unreachable contours" failure that `PYC_CPA` has.
- It is **not** purely a precision problem that more passes would fix.
  With the guards raised, `go`'s C errors halve (11 → 5) but do not reach
  zero, and `linalg`'s do not improve at all (5 → 6).

So convergence work alone cannot close this issue. **The remaining
blocker for `go`, `linalg` and `plcfrs` is the 018/030 representation
gap** — a `{scalar, container}` union reaching codegen — and it would
still block them if FA converged perfectly. 101's splitting behaviour and
these programs' failures are two separate problems that happened to
co-occur on the same three programs.

### Revised fix direction

Split the work:

1. **To make `go`/`linalg`/`plcfrs` compile** — 018/030. That is a
   representation change (tagged or boxed unions), not an FA change.
2. **To close 101 as stated** (unbounded first-time splitting on `go` and
   `plcfrs`, which still grow) — the productivity invariant from 057.
   `linalg` has already left this class thanks to `PYC_HARDREUSE=5`.


## How shedskin handles the same three programs (2026-08-16)

They are shedskin's own examples, so the comparison is direct. Recipe:

```sh
PYTHONPATH=/home/jplevyak/projects/shedskin python3 -m shedskin translate go.py && make
```

**All three translate and build cleanly.** And the reason is
architectural, not a better flow analysis:

| linalg | shedskin | pyc |
|---|---|---|
| generated lines | **520** | 14 469 (**28×**) |
| container-method definitions emitted | **0** | 26 `list::__mul__` clones |
| distinct list types needed | **3** | 75 `__mul__` contours, 236 `__getitem__` |

shedskin emits the failing site as

```cpp
list<__ss_int> *types;
types = ((new list<__ss_int>(1,__ss_int(0))))->__mul__(__ss_int(1025));
```

`__mul__` appears **once** in the whole file, because the body lives in
the runtime's `list<T>` template and the C++ compiler instantiates it.
`linalg` needs exactly three list types — `list<__ss_int>`,
`list<list<__ss_int>>`, `list<list<list<__ss_int>>>`. `go` needs five,
`plcfrs` fourteen.

**shedskin delegates monomorphization to the C++ type system.** Its
analysis only has to answer *"what is the element type of this
container?"* — three answers for `linalg`. pyc's analysis is answering
*"which clone of each container method does each call site reach?"* —
hundreds. That is what the splitter is grinding on in this issue, and it
is a job shedskin never has to do.

### But shedskin does NOT solve the union problem

Run against pyc's own 018 reproducers, shedskin **detects the unions more
precisely and then fails at essentially the same place**:

| test | shedskin analysis | shedskin build |
|---|---|---|
| `branch_merged_scalar_union` | `*WARNING* Variable 'x' has dynamic (sub)type: {int, str}` | **fails**: `invalid conversion from '__ss_int' to 'pyobj*'` |
| `none_int_field_pair` | `*WARNING* … dynamic (sub)type: {None, int}` | **fails**, same conversion error |
| `dict_mixed_key_types` | no warning | **fails** inside shedskin's own `compare.hpp` |
| `list_mul_heterogeneous_element` | no warning | builds; prints `[1.5, 0.0, 0.0]` where CPython prints `[1.5, 0, 0]` — **pyc diverges identically**, already recorded on that test's `.known_issue` tag |

shedskin's representation for a union is `pyobj*`, and it does not box the
scalar — so `{None, int}` hits *the same wall pyc does*, a scalar where a
pointer is required. This is the 048/052 shape exactly.

### What to take and what not to

- **Take the diagnostics.** `*WARNING* Variable (Class V, 'a') has
  dynamic (sub)type: {None, int}` names the variable, the class and the
  union. pyc currently emits `sizeof_element of non-container type
  'float64'` from inside `__pyc__.py`. That is a cheap, self-contained
  improvement to [018](../issues/018-dict-mixed-key-types-boxing-failure.md).
- **Do not go looking for shedskin's union representation.** It does not
  have one. Boxing/tagging (030) remains genuinely unsolved work, not
  something to copy.
- **The template architecture is the real lesson for this issue**, and it
  is a large change: it would mean emitting container methods as C++/C
  templates parameterised on element type instead of cloning one C
  function per contour. It would dissolve the `__getitem__`/`len`/`__mul__`
  splitting pressure that is 75-88 % of the splits measured above, rather
  than trying to make that splitting converge.
