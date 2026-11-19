# ifa/156 — split int from float on demand; coerce only as a last resort

**Status: CLOSED 2026-10-12.** The split side is complete for the corpus:
no remaining int/float mix originates where a contour split could separate
it. What is still coerced is a genuine flow or temporal mix, which is the
last resort this issue's directive allows. The warning noise that remains is
[189](189-FA-coercion-warns-per-expression-not-per-origin.md).
Rewritten 2026-09-28; it carries
[145](145-numeric-coercion-is-not-gated-on-permissive-mode.md) and
[171](171-FA-numeric-union-sustains-itself-through-a-shared-contour.md).

## Closure (2026-10-12)

What landed, in order:
- **EntrySet split** (2026-10-03, `fa_split_numeric_confluences`). A formal
  holding a pure mix, whose in-edges' seeds differ, splits by seed set.
- **CreationSet rung** (2026-10-12, `a941b8ba`, `9e3fb17f`). A member or
  element written only ints and floats, on a contour with one creation
  point, climbs and splits by caller. It uses a call-, receiver- and
  object-matched walk. tictactoe now matches CPython.
- **Contour merge** ([170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md),
  `455fd925`). It never merges a coerced contour, so it cannot undo a split.

**Measured at closure** with two probes in `fa.cc`, off unless set:
`IFA_DBG_NUMCLASS` says where a mix enters its contour, and
`IFA_DBG_NUMORIGIN` follows each mix back across contours to where it first
forms.

| program | mixed AVars | origins |
| --- | --- | --- |
| doom | 763 | 11 local |
| ac_encode | 963 | 12 local |
| softrender | 800 | 17 local, 6 field |
| fysphun | 310 | 1 local, 8 field |

No origin is at a formal, so every caller-side confluence is already split.
The origins read as genuine:
- **doom:** `textureX0` is `seg.offset + offset_x` (int), then
  `textureX1 + p * (...)` (float) on the `za <= 0.1` path. The `scrY*`,
  `backFloor` and `*TextureY` locals in `draw_seg` merge the same way.
- **ac_encode:** `decode`'s `u` and `v` are `0` and then `halfway`. This is
  the fixture's own shape, where `u` genuinely holds 0 and then a float.
- **softrender:** the projection matrix's rows hold `0` beside
  `1.0 / tanHalfFOV` in one list, so everything `transform` computes from
  them inherits the mix. `xStep` is `0` on one branch and `xDist / yDist`
  on the other.
- **fysphun:** the `x` / `y` / `dist` fields are written 0 and then floats.

None of these can be separated by contours. They need either path
duplication after the branch, or nothing at all: most uses are float
arithmetic, which CPython would produce anyway.

Verification, as listed below: `tests/numeric_union_self_sustaining.py`
prints CPython's `(2, 1073741822, ...)`. ac_encode compiles. The two-path
fixture shapes (`tests/numeric_member_split_by_caller.py`,
`numeric_rows_split_by_caller.py`, `numeric_member_split_enumerate.py`)
print CPython's ints. No corpus program's output differs from CPython
because of coercion: mastermind2 and circle differ by timing and
`sys.version`. The use-sensitive case (`bh`'s `>>`) no longer coerces at
all; bh's one warning is the C compiler's.

The record below is historical.

## The directive

**Author, 2026-09-15:** *split `int` from `float` where possible, and
coerce only as a last resort.* This is a CORRECTNESS argument. Coercion
widens the int, so pyc prints `1.0` where CPython prints `1`. Splitting
keeps the int an int. Coercion is a permissive-only device (145, landed:
`coerce_annotate` returns 0 unless `fruntime_errors`, and `--strict`
reports the mix; `tests/strict_rejects_numeric_boxing.py`).

## Fixture

`tests/numeric_union_self_sustaining.py` (`.known_issue`), reduced from
`ac_encode`'s `decode`:

```python
u = 0 ; v = 1 << 30
for c in s:
    halfway = u + (v - u) / 2          # u, v become float
    if c == '1': u = halfway
    a = QUARTER + 1 ; b = THREEQU - 1  # a, b are int, always
    while (a > QUARTER) and (b < THREEQU):
        a = 2*a - HALF;  b = 2*b - HALF ; u = 2*u - HALF ;  v = 2*v - HALF
```

pyc compiles it with no diagnostics and prints `(2.0, 1073741822.0, …)`.
CPython prints `(2, 1073741822, …)`.

## Mechanism (measured, 171)

1. IFA's minimal start puts all four `2*x` sites into one `int.__mul__`
   contour (`es=88`). That is correct.
2. `2*u` brings a float in, and the shared contour returns `{int64,
   float64}`. That flows back into `a` and `b` through `a = 2*a - HALF`,
   and they feed the same edges.
3. From pass 0 every edge carries identical actuals `{int64, float64}`.
   TYPE_CONFLUENCE has nothing to partition (self-blinding: `etype ==
   stype`), so `es=88` never splits.
4. At the end, `coerce_annotate` widens `a`/`b`: it answers a union the
   analysis made up.

The union is a fixed point, but not the least one. Split the `2*u` edge
off and re-derive from bottom, and `a`/`b` come out int and `u`/`v` float,
which is also self-consistent.

**Why no rung sees it.** A pure-numeric mix is excluded from every demand
test by design (`elem_irrepresentable` ends `return nb > 1 && !all_num`:
"coercion fixes it"). ~96% of coercion's targets are EntrySet-contoured
(fysphun 283 ES vs 15 CS, softrender 513 vs 15), while the demand ladder
is CreationSet-side. So simply dropping the exclusion (`PYC_NUMSPLIT`)
reached ~4% and measured a no-op. It was deleted.

## Landed (2026-10-03): `fa_split_numeric_confluences`

Steps 1-3 of the plan below, for EntrySets. It runs at quiescence in
reanalyze phase 2, BEFORE coercion, and coercion runs only on a quiescence
where nothing more splits (step 4).

- **Demand:** an EntrySet formal holds a pure numeric mix and the contour
  has 2+ in-edges.
- **Backtrack:** each in-edge's actual is walked back to its PURE sources
  (single-numeric-kind AVars): through moves/phis by `backward`, through a
  primitive by its operands, and through a call by THAT call's actuals, not
  the shared callee. The contour being split is neutral, which is what
  breaks the self-blinding loop. One rule took a measurement to find: a
  call's result is a seed of kind K only if EVERY callee it dispatches to
  returns pure K. `2*a - HALF` fans out to int.__sub__ AND float.__sub__
  only because its receiver is the union being broken, and crediting each
  callee's return separately read the loop's float straight back in.
- **Split:** in-edges are grouped by seed set (`{int64}`, `{float64,
  int64}`, ...) and every group but one moves to a fresh contour. The demand
  decides whether; the call edges only name the parts.

Results: `tests/numeric_union_self_sustaining.py` matches CPython (a, b, w
are ints; only `u`, which genuinely holds `0` then a float, is still
coerced, and its warnings are its `.check`). `ac_encode` compiles under
the perturbation this issue named, and under the one that exposed it
(open() gaining a raise path shifted pass 1 by one contour and lost the
accidental split). Corpus check `f7f51e05+80a05d11` against `27b04a0c`:
coercion warnings 678 -> 572 (ac_encode 206 -> 101), contours +2.5%, no
verdict regressed. Probe: `IFA_DBG_NUMSPLIT` (1 = splits and candidates,
2 = seeds, 3 = the full walk).

Still open: the CS-side mix (record fields and container elements, which
coercion also annotates), and the use-sensitive half below.

**A corpus case of the CS-side mix (2026-10-12): tictactoe.** Its board
rows (`self.__board = [edge * [0] ...]`, written with player ints) and its
`scores` rows (`[self.edge * [0] ...]` in `makeAImove`, then `+= 15 *
sig(...)`, floats) are both `[0]` literals repeated by `list.__mul__`,
which returns a list merged from its receiver's CreationSet. Under
start-merged identity the two literals share that CreationSet, so the
board's element is `{int64, float64}` and coercion widens it: the board
prints `2.0 | 1.0 | ...` where CPython prints `2 | 1 | ...`
(`tictactoe_check.py`). The scores cells genuinely hold `0` then a float,
so they must still be coerced; the board must not. Dropping
`elem_irrepresentable`'s `!all_num` exclusion alone was tried here too and
changed nothing (51 warnings, same output), so the CS-side demand does not
reach this CreationSet through that predicate either.

**Why route 4 misses it (measured 2026-10-12, `IFA_DBG_CSDEFSPLIT` +
`IFA_DBG_CSVARS=list` on `tictactoe_check.py`).** Three contours, and the
blocker is four layers deep:

| cs | what | defs | element |
| --- | --- | --- | --- |
| 1320 | the `[0]` literal (+ `set([0])` in doRow) | 6 | `int64` |
| 1327 | the rows: `edge * [0]`'s result | **1** (`_CG_list_mult` in `list.__mul__` es=78) | `float64` (coerced; writers: `__setitem__` es=246 int, es=244/365 float) |
| 1812 | the outer lists, board's and `scores` | 2 (`__init__`, `makeAImove`) | `list#1327` |

1. **cs=1327 has one creation point**, because both rows come out of one
   `list.__mul__` contour. Route 4 declines `single creation point` every
   pass. The cure is ifa/129's third clause (`split_es_by_call_site`), and
   three things stop it:
2. **The demand is excluded twice.** `PYC_CSCALLSITE` is 0 by default, and
   `elem_irrepresentable`'s `!all_num` rejects `{int64, float64}`. Once
   `fa_coerce_numeric_confluences` annotates the element (`num_coerce`), it
   reads `{float64}` and the mix is not visible to any later pass.
3. **The site is three calls deep with one in-edge each:** `edge * [0]` →
   `int.__mul__` (es=74, the only contour with two callers) →
   `list.__rmul__` → `list.__mul__` es=78. With both gates forced open
   (`PYC_CSCALLSITE=1` and the exclusion dropped), the result is
   `es=78 fun=__mul__ DECLINED edges=1`. A prototype that climbs with
   `es_climb_to_choice` reaches es=74 and then fails with
   `DECLINED demand: all 2 edge(s) one signature`.
4. **The signatures are the same because every assign set's backflow reaches
   both callers**, and the owner rung (`split_owner_of_demanded`, which
   would separate cs=1812 first) is blind for the same reason
   (`owner cs=1812 defs=2 groups=1 | 1011 1011`). A path trace of the owner
   walk shows the leak:

   ```
   board int write: __setitem__ es=246 self <- assign:31 `self.__board[row]`
     <- list.__getitem__ es=276: LOAD of its `self` formal's element (-> track 1812)
     <- es=276 `self` formal <- EVERY caller of es=276, including
        makeAImove's `scores` (av7482..) -> def av7415, scores' outer list
   ```

   `list.__getitem__` has **one contour for both receivers**, because
   board and `scores` are both `list#1812`. The walk goes into it through
   the board call's return and comes out of its formal to every caller.
   That path cannot happen at run time: the walk is not call-matched. The
   same walk in `build_cs_flow_graph` then crosses cs=1812's shared
   element (two `append` writers) on the way to `int.__mul__`'s two
   results, which is why the call-site signature cannot tell the edges
   apart either.

So **the first fix is not a demand predicate**. Turning on the demand
(steps 2–3) is necessary but buys nothing until the owner walk is
call-matched: a walk that enters a shared callee through edge E's return
must leave through E's actual. With that fix, the board writer reaches only
`__init__`'s outer list. The owner rung then splits cs=1812, which is a
CS-side partition with a type-shaped key (int writes vs float writes). After
that, the call-site split climbs `int.__mul__` → `__rmul__` → `__mul__` one
level per pass, and cs=1327 gets two creation points that route 4 can
partition. `scores`' rows keep `{int, float}` (`0` then floats, a real
change over time) and are coerced as before. shedskin reaches the same
answer through `ifa_split_no_confusion`; see its `list_comp_0` /
`list_comp_2` in `examples/tictactoe/tictactoe.cpp`.

**The call-matched walk landed (2026-10-12): `PYC_CALLMATCH`, default 1.**
`callctx_step` in `fa.cc` tracks a stack of the calls the walk has
entered:
- stepping from a caller's result back to the callee's return pushes that
  `AEdge`;
- stepping from a formal back to an edge's `filtered` argument must match
  the top of the stack;
- a heap, global or unrecognized cross-contour step forgets the stack.

Results:

- **Bit 1 (owner walk), on: the outer lists separate.** tictactoe's
  `cs=1812` becomes board-only (`__init__`), scores' outer list becomes
  `cs=1917` (`makeAImove`), and the coercion warnings go from 51 to 48.
  `make test` is 439/0 on both backends.
- **Bit 2 (`build_cs_flow_graph`'s walk), off.** It breaks
  `tests/listcomp_element_separation{,_startmerged}.py` and
  `splitter_mark_type.py`, each with `illegal call argument type ... B`.
  ESBLOCK (`find_blocking_es`) finds the shared contour that the
  context-insensitive walk cannot see through, and splits it. Once the walk
  is matched, route 4 partitions the CreationSet first (p=3 rather than
  p=4), `append` is never split, and its one value formal writes `{A, B}`
  into both halves. Bit 2 can be turned on once the shared contour is split
  by its own demand rather than as a side effect of a blind walk. On
  tictactoe, bit 2 alone changes nothing.

**What still blocks tictactoe's rows (cs=1327).** Steps 2–3 above, measured
on top of bit 1 with the prototypes (`PYC_CSCALLSITE=1`, the `!all_num`
exclusion dropped, a climb with `es_climb_to_choice`): the climb reaches
`int.__mul__` es=74 and still declines with `all 2 edge(s) one signature`.
The leak is now on the HEAP side. Both outer lists are filled by one
`list.append` contour (es=233, receiver `{1812, 1917}`). A flow-graph walk
leaving 1812's element goes into that shared store, which forgets the
context, and back to every `append` caller, including scores' list
comprehension. Separating them needs the exit from a heap crossing
RECEIVER-matched: the edge whose `self` actual holds the CreationSet the
walk came out of. That is a type-shaped check (CreationSet membership), not
provenance. It is the next step, together with bit 2.

**Sweep, bit 1 (`check__default__fab67ca1+e1f1f856`, vs `d3499fb2+8fe3c3d2`).**
No compile, run or stdout regression. hq2x's `NO` is its `TIME` line alone,
and its CPython run only finished this time. tictactoe goes from 50 to 46
warnings, ESS 390→371 and CSS 1340→1301. plcfrs drops 39 CSS and dijkstra2
22. sudoku3 rises to 431 ESS and 2148 CSS (from 413 and 1861), and still
matches. softrender gains 1 warning. **kmeanspp goes from 8 to 40 warnings**
(its CPython run times out, so its stdout is unverifiable). That is a real
precision loss, and it is root-caused as follows:

- Mode 0 ends with three Point contours. The `generate_points` and `lloyd`
  Points separate when a SETTERS split of `__new__` (`[ses] av 4439 es 84
  nedges=2 do=1`, then `SPLIT CS 1371 -> 1656` by starter set) follows a p=3
  owner split of the outer lists. Mode 1 owner-splits the lists at p=2 on
  different signatures, and that setter confluence never appears. All
  Points stay in one contour, so `cc.x = 0` (a genuine, temporal int in a
  cluster centre) widens every point's `x`.
- Every step bit 1 refuses was traced (`IFA_DBG_CALLMATCH`, 1600 refusals),
  and every one is a correct refusal. Each exits a shared `append`,
  `__getitem__` or `__iter__` contour through a call other than the one it
  entered by; even the `__next__ -> __getitem__` pairs are two different
  `__next__` contours (es321 vs es340/es341). So mode 0's separation rested
  on paths no execution takes. It also came through the setter splitter,
  whose starters carry no demand (`fa.cc`, `split_for_setters`).
- The demand-driven route does not exist yet. Point `cs=1371` has one
  creation point (`__new__` es=84). Its demand is a MEMBER `{int, float}`.
  The third clause only looks at an ELEMENT, behind `elem_irrepresentable`'s
  `!all_num`, so it never reaches the call-site rung:
  `PYC_CSCALLSITE=1` on bit 1 still gives 40 warnings and never names
  Point. This is tictactoe's step 2–3 again: a numeric union on a
  single-creation-point contour has to climb to the contour whose callers
  separate it. (That paragraph also guessed that no heap leak stood in
  kmeanspp's way. Wrong: see below.)

## The numeric-mix rung landed (2026-10-12): tictactoe matches CPython

`split_css_by_defs`, in the `defs.n < 2` branch, after the owner split. It is
`PYC_CSNUMSPLIT` (default 1) and has four parts. Each one was found by the
measurement that the previous part failed.

1. **Demand: `cs_numeric_mix_content`.** A content channel (member or
   element) whose WRITERS carry both an int and a float. It reads the
   writers, not the channel, because by the time the split stages run,
   coercion has already widened the channel itself to float64. It is a level
   question on converged types (ifa/157).
2. **Parts: a flow graph over the mixed channels ONLY**
   (`build_cs_flow_graph_over`). The full graph keys every write into every
   member by type, so an int into `group` beside an int/float `x` puts both
   callers on the same key. The rung climbs to the nearest contour with a
   choice (`es_climb_to_choice`) and splits it with `split_es_by_call_site`.
   That is the third clause's machinery, with a numeric demand instead of an
   irrepresentable element.
3. **The walk is call-matched and receiver-matched** (`PYC_CALLMATCH` bit 4,
   default on; the default is now 5). Three defects in the matcher came out
   of tracing kmeanspp:
   - **Receiver frames.** A step from a container's content into the value
     written there pushes {L, receiver}. The walk may leave the writer's
     contour only through a call whose argument at the receiver's formal
     position holds L, and the receiver moves to that actual. Without this,
     a walk out of one list's element came back through the shared
     `append -> __setitem__` to every list's comprehension.
   - **Filtered → actual kept the context.** That step changes contour, so it
     used to fall into "unrecognized, reset", and every match after the first
     exit was lost.
   - **Stale return links are refused.** When an EntrySet is split, its
     in-edge is re-pointed to the new contour, but the caller result's flow
     link to the OLD contour's return stays in `backward`. Measured: e641
     es198→es249 (split from es85), and av9502 still linked es85's return.
     No execution returns through such a link. The index now also covers
     `out_edges`, so a re-pointed edge is still found.
4. **When every caller lands on the same keys, backtrack the receiver union**
   (`split_es_at_union_origin`). The walk records each read through a
   receiver holding several containers of one class. kmeanspp: `lloyd`
   indexes `cluster_centers`, whose type was {points list, centres list},
   because a comprehension's accumulator is `append`'s return, and one
   `append` contour served both comprehensions. The union feeds itself, so
   both callers' actuals already hold both lists. The backtrack therefore
   signs each caller by the creation points its actual reaches WITH THE
   CONTOUR HELD TERMINAL, as `split_blocking_es` does, not by its actual's
   type. Then `append` splits (`[csunion] ... SPLIT es=88 fun=append`), and on
   the next passes the call-site split separates the constructor and
   `__new__`.

The parts are type-shaped throughout: numeric kind per key, CreationSet
membership of arguments, creation points as the objects being separated.
None of these splits fires without the mix.

**Two gates, both found by the first sweep (`fab67ca1+b0d9c0ba`).**
- **A PURE mix only**: the writers are int or float and nothing else. That
  sweep failed to compile `go`: at pass 0, Board's `color` had writers
  {None, tuple, UCTNode, int, float, str, Square}, which is start-merged
  imprecision rather than a numeric mix. Splitting Board's `__new__` on it
  left int/float unions in EntrySet formals, which coercion cannot repair
  (`'self' has mixed basic types`). plcfrs's ESS 1483→1739 came from the
  same cause.
- **Only when no higher stage acted this pass** (`quiescent`), as the
  field-mix demand already is, and for the same reason. On sunfish at pass
  0, `__range_iter__.i`, `__dict_items_iter__._len` and a `_ReNode` contour
  carrying other classes' members all read as int/float mixes. The rung
  split `_ReNode`'s `__new__` for +21 ESS / +83 CSS and no verdict change.
  kmeanspp, tictactoe and both tests are unchanged under the gate.

**Sweep with both gates (`check__default__fab67ca1+3bb8922d`, vs
`fab67ca1+e1f1f856`).** No compile, run or stdout regression. tictactoe's
stdout now MATCHES CPython (warnings 46→8). kmeanspp is back to 8 warnings.
softrender loses 1 warning. Contour changes are small and mixed: plcfrs
+22 ESS, sunfish +122 CSS, ant +23 ESS, linalg −25 ESS / −36 CSS, msp_ss
−63 CSS, go −20 ESS.

**Measured:**

| | before (bit 1 only) | after |
|---|---|---|
| tictactoe warnings | 46 | 9 |
| tictactoe stdout | `2.0` for `2` (NO) | **matches CPython** |
| kmeanspp warnings | 40 (8 in mode 0) | 8 |

kmeanspp's remaining 8 are genuine: a centre's `x` is reset to the int `0`
and then accumulates floats, so one object holds both over time (`bh`'s
case, AGENTS.md). Its points are now float-only, by demand, where mode 0 got
there through the setter splitter.

**Tests.** Both print CPython's answer with the rung, and the widened
answer with `PYC_CSNUMSPLIT=0`.
- `tests/numeric_member_split_by_caller.py`: kmeanspp's shape. Without the
  rung it prints `0.0 2.0 4.0` for `0 2 4`.
- `tests/numeric_rows_split_by_caller.py`: tictactoe's shape, `n * [0]`
  rows. Without the rung it prints `2.0` for `2`. Its one `.check` warning
  is the scores rows, which genuinely hold 0 and then floats.

`make test`: 441/0 on both backends, all gates green.

**Two lists in ONE CreationSet: the enumerate shape (landed 2026-10-12).**
If the test iterates with `for i, p in enumerate(points)` instead of
indexing, both lists stay in one list contour (`cs=1288`, a creation point in
each function, element `{Point}`). The owner walk reaches no owner, because
Point is read out of the `enumerate` tuple and the tuple's def ends the walk.
A receiver frame knows only the CreationSet, so the numeric walk read
`cs=1288`'s element and left through both functions' `append`s.

The fix is to ask the OBJECT question on the heap (`heap_pts`, in the walk's
context code):
- **A read** (load result → content of L) asks which creation points of L the
  receiver can hold, each with its allocation context. It runs the same
  matched walk backward from the receiver, and the answer rides on a CONTENT
  frame.
- **Content → writer** either RESUMES THE ALLOCATION CONTEXT, when the
  writer's receiver is an object allocated in that contour, or carries the
  object set on its receiver frame. The first case is enumerate's `(n, x)`:
  the tuple is built in a shared `__next__`, and the set records which call
  built this one.
- **An exit under a receiver frame** is allowed only into a caller whose
  receiver can hold one of those objects. A primitive read of the content
  inside a frame on the same L (`append`'s `merge_in(self, self)`) keeps
  the frame's objects; that read was the last `ANY` leak.
- **Bounds:** three nested levels, 4000 states and 64 objects per query,
  memoized per pass. Past a bound the answer is "any object", which only
  over-approximates.

The creation point is the object's identity here, which the identity /
assignment / compatibility table allows. The reason to split remains the
numeric demand.

With that, the int path's read of `cs=1288` yields only `make_centres`' def
and the float path's only `make_points`'. The rung splits `__new__` by
caller. The next pass then has two Point creation points, which the
ordinary content key still puts in one group, so route 4's multi-def branch
gained a **NUMERIC KEY**. When the content key names one group and a channel
is a pure int/float mix, the defs are signed by the numeric flow graph's
paths (`NUMERIC-KEY -> 2 groups`). `tests/numeric_member_split_enumerate.py`
now prints CPython's `0 2 4` (was `0.0 2.0 4.0`), with no warnings. Sweep `check__default__a941b8ba+84ddb2b5` against `fab67ca1+3bb8922d`:
verdicts are identical on all 77 programs, and the only change is softrender
ESS/CSS +1/+1. The corpus has no instance of this shape. `make test`: 442/0
on both backends.

## The plan: coercion's failure-to-be-exact is the demand

Coercion knowing it is about to change a value's representation IS
"something observing a distinction and being unable to proceed" (without
deviating from CPython). The partition key is type-shaped (which numeric
kind a path carries), so it is not provenance.

1. **Demand.** Before `coerce_annotate` annotates an AVar, raise a demand
   for its `{int64, float64}` union. It is ES-side, so it needs the
   ES → actuator link (157's `PYC_RETDEMAND` shape), not the CS ladder.
2. **Backtrack to the pure/mixed boundary.** Where the mix is observed,
   every in-edge already carries the union. It must be walked back to the
   nearest contour whose in-edges carry DIFFERENT numeric kinds, or whose
   writers include both a pure-int and a pure-float source. The boundary
   exists in quantity: on `softrender` ~641 pure-int and ~5663 pure-float
   AVars sit above ~639 mixed ones (`PYC_DBG_BOXPURE`).
3. **Split there**, bounded by the demand (two numeric kinds), never by a
   count of contributors. In 171's shape the contour is `es=88` and its
   edges are type-identical, so the call site is the HANDLE that names the
   parts (allowed under the reason/mechanism rule, since the coercion
   demand alone decides whether to split).
4. **Coerce last**, in reanalyze phase 2, only what is still mixed at the
   next quiescence: a genuine temporal mix like `bh`'s `Vec3` (AGENTS.md's
   counterexample), where one object holds a float and then an int.

### The use-sensitive half (145/144)

A pure-numeric mix is resolvable by widening only where no USE requires
the narrow type. `bh`'s `Cell.NSUB >> (k + 1)` widened to double is not C
(`>>` on a double). There, splitting is the only correct answer, and
`Vec3`'s float-only members are pure waste. So the boundary split must win
over coercion wherever any use of the value is integer-only (`>>`, `&`,
an index). Ordering alone cannot decide it: deferring the numeric demand
to "after the other splitters quiesce" releases it at exactly the instant
coercion runs, and coercion wins (144, attempt 3).

### The interaction to watch: split relocates the mix past the constants

Coercion rewrites CONSTANTS only (`type_coerce_numeric_constants`). A split
can move the int/float meeting point downstream of arithmetic, where the
int is already a runtime value, and then nothing can repair it. That is
how `PYC_ESBLOCK` once cost `softrender` 60 → 483 violations (fixed by
scoping ESBLOCK to non-numeric container element writers). A boundary
split that meets a mix it cannot discharge must fall back to coercion, not
leave the program uncompilable.

## Stop conditions

- If the split `2*u` edge lands back in the int contour on the next pass
  (the partition oscillates), the union has a second, real source: find
  it, and do not add hysteresis.
- If a boundary split increases `mixed basic types` errors anywhere, it is
  relocating the mix past the constants. Fall back to coercion for that
  AVar.

## Verification

- `tests/numeric_union_self_sustaining.py` flips KNOWN → PASS (prints ints).
- `ac_encode` compiles under any perturbation (e.g. with `hardertest()`
  removed).
- A fixture with two call paths, one int-only and one float-only, meeting
  at a shared formal, prints CPython's `1`, not `1.0`.
- Corpus `-m check`: `[numc] annotate` count (`PYC_DBG_NUMC`) falls;
  `stdout_match` rises; nothing that matched regresses.
