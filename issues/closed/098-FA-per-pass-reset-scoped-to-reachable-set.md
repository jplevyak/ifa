# 098 — FA's per-pass reset is scoped to the PREVIOUS pass's reachable set, so bound call edges carry stale per-pass state into later passes

**Status:** root-caused and **fixed** — the reset itself 2026-08-12,
the `out_edge_map` / silent-dispatch-failure follow-on 2026-08-31 (see
"The second defect's fix" below). **CLOSED.** Reproduced on
**unmodified main** — the original
2026-08-11 filing believed the only trigger was
[097](../097-CGEN-callsite-vs-clone-formal-type-mismatch.md)'s reverted
defer/force patch, and diagnosed the mechanism as *per-pass scheduling
order-dependence*. Both of those are wrong and are corrected below; the
old diagnosis is kept at the end for the record, since the prior-art
survey it collected is still useful.

**Affects:** `ifa/analysis/fa.cc` — `clear_results` (~5296) and its
callees `clear_es` (~5268) / `clear_edge` (~5259) / `clear_cs` (~5277) /
`foreach_var` (~5287); `collect_results` (~3747, which decides what
`clear_results` will cover); `extend_analysis` (~6633, which decides
*whether* it runs at all); `analyze_to_convergence`'s loop condition
(~7142); `record_args_rets` (~1669); `analyze_edge`'s filter gate
(~3069-3077); `collect_argument_type_violations` (~3809).
`EntrySet::out_edge_map` (fa.h ~185) is never reset at all.

## The invariant, and the measurement that shows it broken

The premise is sound and worth stating first, because it is what makes
this a bug rather than a curiosity: **if a call is reachable, then some
execution schedule reaches it, and in that schedule every actual
argument already holds a value.** So when a pass quiesces (both
worklists empty), every call edge that the pass actually derived must
have a non-bottom `out` at every positional actual. There is no
admissible ordering in which a reachable call's arguments are all
empty.

Measured on unmodified main, by walking every bound AEdge at the moment
`analyze_to_convergence`'s inner `while` exits and reporting positional
actuals whose `out` is `bottom_type` (`shedskin_examples/`, 84
directories, 300 s cap; 74 of them reach FA and report a final pass —
the rest are data/script dirs or bail earlier):

| example | bad args at final pass |
|---|---|
| `rdb` | 234 |
| `sudoku5` | 20 |
| `mastermind2` | 18 |
| `go` | 3 |
| `msp_ss` | 3 |

The other 69 analyzed examples and all 265 `test_pyc.py` fixtures are
clean, so this is not a universal condition — it is a specific,
reproducible failure on larger programs.

**The premise is not violated. What is violated is the assumption that a
bound edge belongs to the current pass at all.** Every one of these
edges has `from->live_pnodes.set_in(e->pnode) == false` — its call site
was not walked this pass. Nothing marks the edge as such, and every
consumer that iterates `EntrySet::edges` / `EntrySet::out_edge_map`
reads it as current.

## Root cause

**FA re-derives flow state from scratch each pass over the subgraph the
pass reaches, but resets that state over the subgraph the *previous*
pass reached. The two sets are not the same, and the difference is
exactly the set of objects that carry an older pass's values into the
current one.**

The chain, in the order the code executes it:

1. `complete_pass` → `collect_results` (fa.cc:3749-3754) sets
   `fa->ess` to exactly `fa->entry_set_done` — the contours some edge
   *reached* during the pass that just ended. `fa->funs` and `fa->css`
   are derived from it the same way.
2. `extend_analysis` then runs `run_split_stages()` (~6669) **first**,
   which re-homes edges into freshly minted EntrySets — contours that
   by construction are not in `fa->ess`, because `fa->ess` was
   snapshotted before they existed.
3. `extend_analysis` calls `clear_results()` **last** (~6756), and
   `clear_results` resets per-edge state only via
   `for (EntrySet *es : fa->ess) clear_es(es)` → `clear_edge(e)` for
   `e ∈ es->edges`. An edge is therefore reset **only if its `to`
   contour was reached in the pass that just ended**. Same scoping
   applies to `clear_cs` (only `fa->css`) and to `foreach_var`'s
   `for (Fun *f : fa->funs)` arm.
4. `clear_results()` is called **only `if (analyze_again)`**. When a
   pass is triggered instead by the other arm of
   `analyze_to_convergence`'s loop condition (fa.cc:7142,
   `extend_analysis() || if1->callback->reanalyze(...)`), no reset
   happens at all. pyc's `PycCompiler::reanalyze`
   (`python_ifa_sym.cc:403`) returns true after `promote_field`
   without clearing; only its third arm (numeric coercion, via
   `fa_coerce_numeric_confluences`) clears.
5. An unreset edge keeps `e->args`, so `record_args_rets`'s
   `if (!e->args.n)` guard (fa.cc:1670) suppresses re-recording even
   when the send *does* re-dispatch — and it keeps
   `e->match->formal_filters`, which `pattern_match` rebuilds from the
   **current** argument CreationSets (`Matcher::set_filters`,
   `if1/pattern.cc:565`) and `clear_edge` is the only thing that
   resets.
6. `analyze_edge`'s gate (fa.cc:3077) then drops the edge for good:
   `if (filter && type_intersection(actual->out, filter) == bottom_type)
   goto LskipEdge;`. Once bound, nothing re-routes it either —
   `make_entry_set` (fa.cc:1304) early-returns on a non-null `e->to`,
   so the flow loop never revisits the binding; only the splitter does,
   between passes.

### Measured, on `mastermind2`

Per-pass escapees from the reset, counted by clearing every edge and
recording how many were not already covered:

```
FIXCLEAR pass=13 extra=441 of 1827 (to_null=0 to_not_in_ess=441)
FIXCLEAR pass=14 extra=389 of 1967 (to_null=0 to_not_in_ess=389)
FIXCLEAR pass=15 extra=350 of 1967 (to_null=0 to_not_in_ess=350)
```

19-24% of all edges escape the per-pass reset, and every single one of
them has a **non-null `to` that is simply not in `fa->ess`** — exactly
mechanism 2/3 above, not a detached (`to == 0`) edge. Separately, passes
13 and 17 emit no `clear_results` at all (mechanism 4); on `go` the
unreset passes are 3, 10, 23 and 24, where 3244 of 3439 per-edge
filtered AVars start the pass still holding the previous pass's `out`
and `forward` links.

The concrete failure, traced end to end on `mastermind2`'s
`<tuple_cmp>` contour (ES 710):

```
GATE e=1603 pos=2 actual=av17849 out={list#1094}
                  formal_filter={?#963}  es_filter={}
     args_pass=13  cleared=-1
```

`e->match->formal_filters` was computed in pass **13**, when the actual
at that position was CreationSet #963. By pass 17 the actual is
`list#1094`. The edge was never cleared (`cleared=-1`), so the pass-13
filter is still installed, the intersection is empty, and `analyze_edge`
skips the edge permanently. Downstream:

- the callee's formals never receive the actuals →
  `ES 1155.rets[0]->out` stays bottom;
- the caller's result stays bottom → the `if` that consumes it has a
  bottom condition, so `add_pnode_constraints`'s `Code_IF` case
  (fa.cc:2742) `return`s without walking either branch;
- every send after that `if` is never re-walked, so *their* bound edges
  finish the pass with bottom-valued arguments — the 18 reported
  entries.

An independent probe confirms the `is_if_arg` resume hook is **not**
implicated: across the whole sweep, zero live `if`s with a *resolved*
condition had an unwalked taken branch. Every stopped walk was stopped
on a genuinely bottom condition.

### Second defect, same family: `out_edge_map` is never reset

`EntrySet::out_edge_map` is cleared nowhere in `fa.cc` (only rebuilt
post-FA in `clone.cc:45`), so once a send has dispatched successfully in
*any* pass, the map entry persists forever. In
`collect_argument_type_violations` (fa.cc:3817) that entry decides which
branch runs:

```cpp
Vec<AEdge *> *m = from->out_edge_map.get(p);
if (!m) { /* Partial_NEVER -> SEND_ARGUMENT violation */ }
else    { /* only inspects edges in from->out_edges */ }
```

A pass in which dispatch fails *completely* still finds `m` non-null,
takes the `else` branch, finds no analyzed edges, and reports nothing.
On `mastermind2` this is live: six `if`s in live contours end the final
pass with bottom conditions because `all_applications` returns -1
(`pattern_match` finds zero candidates for `list < list` — `__pyc__` has
no `list.__lt__`), and the program still compiles `rc=0` with no
diagnostic.

## The fix (landed 2026-08-12)

Three changes in `ifa/analysis/fa.cc` / `fa.h`, plus one latent
null-deref they exposed.

**1. Reset over authoritative registries, not over the last pass's
reachable set.** `FA` gains `all_aedges` / `all_entry_sets` /
`all_creation_sets`, appended by the respective constructors, and
`clear_results()` iterates those instead of `fa->ess` / `fa->css`. Edge
resetting moves out of `clear_es` (which only ever saw the edges of
reached contours) into its own loop over `all_aedges`. `foreach_var`
switches from `fa->funs` to `fa->pdb->funs` for the same reason — a
function that drops out of the call graph for a pass and comes back kept
its AVars' values across the gap. All three registries are append-only
and add one pointer per object, not lifetime: FA already retains every
one of these for the whole run (`Fun::ess`, `out_edge_map`,
`Sym::creators`).

**2. Reset before every pass, unconditionally.** `clear_results()` moves
out of `extend_analysis`'s `if (analyze_again)` and into
`analyze_to_convergence`'s loop, guarded only by "not the first pass".
That is also the only correct place for it: the converged state must
survive the loop for clone/codegen, so the reset has to happen *before*
a pass, never after one. `fa_coerce_numeric_confluences`'s own
`clear_results()` call is dropped as redundant.

**3. `clear_edge` tolerates a null `Match`.** `get_AEdges` mints edges
for a (pnode, fun) pair before dispatch binds them; those are reachable
now that the walk is over every edge rather than over bound ones.

**4. `check_split` skips unbound candidates.** Its split-lineage loop
walked `out_edge_map` and dereferenced `ee->to->filters` (inside
`check_edge`) and `ee->match->fun` without checking either.
`out_edge_map` is not a set of live bound edges: besides never-dispatched
edges, it holds the whole group `apply_entry_set_split` has just
detached (`x->to = 0` for the group, then re-bind one at a time), which
are visible there for the duration of that second loop. Latent since the
split-lineage path was written; `dijkstra`'s contour trajectory shifted
onto it and segfaulted. Fixed with `if (!ee->to || !ee->match) continue;`.

**Deferred at the time: `out_edge_map`'s own reset.** Clearing it per
pass is *not* viable — `get_AEdges` reads it to reuse the same `AEdge`
across passes, so clearing it would mint fresh edges every pass and
destroy every `e->to` binding the splitter's cross-pass routing depends
on. The "second defect" above therefore needs the other approach: teach
`collect_argument_type_violations` to treat "map entry exists but no
edge in it was analyzed this pass" the same as "no map entry".
**Landed 2026-08-31 — see "The second defect's fix" below.**

**The invariant check is now permanent**, gated on `IFA_DBG_EDGEARGS`
(`audit_edge_arg_values`, next to `complete_pass`; same zero-cost-when-off
shape as the `PYC_DBG_*` probes in the same file):

```
IFA_DBG_EDGEARGS=1 ./pyc prog.py 2>&1 | grep 'EDGEARG SUMMARY'
```

It reports, per pass, edges whose `args` this pass recorded that still
have a bottom-typed actual. Non-empty `e->args` is exactly the "recorded
this pass" test, since `clear_results` empties it before every pass and
only `record_args_rets` refills it — so the check is self-maintaining:
anything that re-narrows the reset's domain or reintroduces a pass that
skips it shows up immediately.

## Results

Measured with the landed audit, A/B against the old reset scheme via a
temporary switch (removed):

| example | worst pass, old reset | worst pass, fixed |
|---|---|---|
| `chess` | 1207 | 0 |
| `rdb` | 599 | 0 |
| `sudoku5` | 382 | 0 |
| `msp_ss` | 252 | 0 |
| `mastermind2` | 235 | 0 |
| `go` | 42 | 0 |

Across the corpus the invariant now holds on **every pass of all 74
analyzed shedskin examples** (0 everywhere).

Regression testing, all against a baseline binary built from the
pre-fix tree:

- `ifa --test`: 58 passed / 0 failed.
- `test_pyc.py`: 265 passed / 14 expected fails / 0 failed / 4 skipped
  — identical on the C backend and under `PYC_FLAGS=-b`.
- `shedskin_examples/` sweep, 84 directories: **zero exit-code changes**.
  Notably the two worries raised by the crude "clear every edge"
  experiment did **not** materialize with the real fix — `msp_ss` stays
  `rc=0` and `softrender` stays `rc=134`; the unconditional reset and
  the widened `foreach_var` land the analysis on a different trajectory
  than edge-clearing alone.
- Compile time unchanged within noise on the largest examples
  (`plcfrs` 56.5s → 54.3s, `chess` 15.38s → 15.41s, `yopyra` 17.1s →
  16.9s, `rdb` 7.8s → 8.1s).
- One golden file updated: `tests/list_index_type_mismatch_salvage.py.check`
  gains one `called from __pyc__.py:1852` frame. Same two violations at
  the same source locations; `show_call_tree` prints one line per in-edge
  of the violating contour, so the extra line is one more caller edge
  bound into it. The generated C is unchanged — after normalizing the
  two swapped struct ids and the clone indices, the 1321 emitted
  function signatures are byte-identical.

**Handed to 097, not tracked here:** re-testing 097's reverted
defer/force patch. Its regression should be gone if this was the
load-bearing assumption it broke, but the retest is that issue's own
fix, and 097 has changed underneath in the meantime
([100](../100-FA-display-removed-from-contour-identity.md) removed
`entry_set_compatibility`'s nest gate, so its trace wants re-taking
first). Recorded in 097's status; nothing in 098 depends on it.

## The second defect's fix (landed 2026-08-31)

`collect_argument_type_violations` read "`out_edge_map` has an entry for
this send" as "this send dispatched", and the map is never reset, so the
entry survives from the FIRST pass in which dispatch succeeded. A pass
in which dispatch fails *completely* still found `m` non-null, took the
`else` arm, found no analyzed edge to inspect, and reported nothing.

The map cannot be cleared (see "Deferred at the time" above), so the
per-pass fact has to come from somewhere else. It already exists:
**`EntrySet::out_edges` is emptied by `clear_es` every pass and re-added
by `analyze_edge` only after the edge's filter gate passes.** So the
test is one helper:

```cpp
static bool dispatched_this_pass(EntrySet *from, Vec<AEdge *> *m) {
  if (!m) return false;
  for (AEdge *me : *m) if (me && from->out_edges.set_in(me)) return true;
  return false;
}
```

and the collector's `if (!m)` becomes `if (!dispatched_this_pass(from, m))`.
The `from->live_pnodes.set_in(p)` guard at the top of the loop already
establishes that the send *was* walked this pass, so "walked, and not one
of its edges survived dispatch" is exactly a dispatch failure — the same
condition the `!m` arm reports, reached a different way.

A probe reports it: `IFA_DBG_DISPATCHFAIL=1` prints, per pass,
`total=` sends with a map entry, `sites=` those none of whose edges was
analyzed this pass, and `reported=` the `Partial_NEVER` subset that
therefore yields `SEND_ARGUMENT` violations.

```
$ cd shedskin_examples/<x> && IFA_DBG_DISPATCHFAIL=1 ../../pyc -D ../.. <x>.py
```

| example | total | sites (final pass) |
|---|---|---|
| `mastermind2` | 1064 | 7 |
| `msp_ss` | 2367 | 10 |
| `rdb` | 2905 | 8 |
| `sudoku5` | 1950 | 8 |
| `go` | 1519 | 0 |

Every one of them is `Partial_NEVER`, so `sites == reported` throughout:
these are all real, previously-invisible failures. On `mastermind2` the
new diagnostics are precisely the ones predicted when the defect was
filed —

```
mastermind2.py:19:220:  warning: unresolved call '__lt__'
mastermind2.py:127:2384: warning: unresolved call '__lt__'
```

— `max([(utility(play, possibles), play) for play in plays])` compares
`(float, list)` tuples, which falls through to comparing the `list`
halves on a tie, and `__pyc__` has no `list.__lt__`. That missing
builtin is the other half of this example's symptom and is now filed
separately as [issues/122](../../../issues/122-list-ordering-comparisons-missing.md),
with `tests/list_ordering.py` as its `.known_issue` repro. Warnings on
the example go 45 → 54; it still compiles `rc=0`, because pyc is permissive
by default and a `SEND_ARGUMENT` violation is a warning plus a runtime
check there (it is a hard error under `--strict` and in the `ifa`
binary, which defaults strict).

### Regression testing

All five CI steps green on the fixed tree: `ifa --test` 58/0; `test-ir`
0 failed across all 16 phases with the 2 known `mark_*_skew` fixtures;
`test_pyc.py` **308 passed / 0 failed / 14 expected fails / 15 known**,
identical on the C backend and under `PYC_FLAGS=-b`; `make -C ifa
test_llvm` and `make test_dparse` pass.

**One golden re-blessed: `tests/match_seq.py.check`.** It gains 10
warnings, every one of the *same class it already records*, at
additional call sites — `case [[a, b], c]` reached from
`classify([2, 50])` / `([5, 3])` / `([3, 5])` (element 0 is an `int`,
so destructuring it as a sequence cannot dispatch), and
`case [a, b] if a > b` reached from `classify([[1, 2], 3])`
(`a` is a `list`, `b` an `int`; `list.__gt__` does not exist). All are
genuine unresolvable dispatches that the analysis cannot rule out —
proving the guard unreachable because an earlier `case` matched is
[050](../050-FA-general-constant-propagation-unreachable-code.md)'s job,
not this one. The runtime output is unchanged and still matches
`match_seq.py.exec.check` exactly, and the test now passes on the LLVM
backend too (307 → 308).

### The corpus

`sweeps/check__default__c8fbb054+2b9aa817.tsv`, A/B against
`check__default__de4ea252+36eaaedb` (the cached sweep of HEAD's content):
```
programs=77 compile_fail=5 run_fail=41 stdout_differs=23 with_warnings=42   (both arms)
```

**Every difference across all 77 programs is in the `warns` column.**
`compile_rc`, `run_rc`, `cpy_rc` and `stdout_match` are byte-identical
program for program — no program newly fails or newly passes, and no
program's output changes. So the splitter's `nviol_this_pass` gate did
see more violations and the trajectory did not move anywhere that the
corpus can observe.

Warnings rise on 20 programs, 1615 → 2040 corpus-wide:

| program | before | after | | program | before | after |
|---|---|---|---|---|---|---|
| `rubik` | 67 | 176 | | `sudoku5` | 68 | 82 |
| `doom` | 92 | 210 | | `softrender` | 54 | 76 |
| `plcfrs` | 82 | 123 | | `sunfish` | 33 | 55 |
| `rdb` | 114 | 130 | | `lz2` | 27 | 39 |
| `msp_ss` | 221 | 237 | | `mastermind2` | 45 | 54 |

Spot-checked `rubik`, the largest jump: 46 dispatch-failure sites at the
final pass, spread over **75 distinct source locations** — not one site
reported repeatedly. Its three `unresolved call` messages are
`key[0] == self.UP`, `DOWN[FRONT, 1] == upCol` and
`FRONT[FRONT, 1] != FRONT[FRONT, 4]` — element reads whose type has
degraded far enough that `__eq__`/`__ne__` no longer dispatch. `rubik`
already aborted at run time in both arms (`run_rc=134`), so these are
exactly the signal [102](../102-corpus-programs-compile-then-abort-at-runtime.md)
is short of: a compile-then-crash program that was reporting nothing
about the reason.

### Note on blast radius

This is not purely a diagnostic change: `fa->type_violations.set_count()`
gates the splitter's self-product complement eviction
(`nviol_this_pass`, issue 074) and is read by `clone.cc`, so surfacing
more violations can move the FA trajectory. That is why it wanted the
full corpus A/B above rather than the test suite alone.

## What this unblocks

Directly: [097](../097-CGEN-callsite-vs-clone-formal-type-mismatch.md)'s
own fix, which was blocked on this. More broadly, every analysis that
reads `EntrySet::edges`, `AEdge::args` or `AEdge::match->formal_filters`
was reading a mixture of the current pass and older ones — the
splitter's compatibility tests
(`edge_type_compatible_with_edge`/`_entry_set`), `check_split`'s routing,
and the violation collectors all do — and now reads only this pass's.
That puts this upstream of
[closed/033](033-splitter-non-idempotent-divergence.md)/[074](../074-FA-cross-pass-oscillation-plan.md)'s
cross-pass split oscillation: a split decision keyed on a stale edge's
types was not evidence about the pass making it, so those
investigations' measurements are worth re-taking on top of this fix
before more design work goes into them.

---

## Superseded: the 2026-08-11 "order-dependent fixed point" diagnosis

The original filing concluded that a pass's worklists emptying does not
mean values have settled, and that the per-pass fixed point is
order-dependent — reached by tracing one `__pyc_more__` dispatch under
097's reverted patch and finding that its single `pattern_match` attempt
failed with no retry. That framing is not supported by the measurements
above:

- The failure reproduces without 097's patch, on 5 of 86 shedskin
  examples, so it is not a property of that scheduling change.
- The `is_if_arg`/`arg_of_send` reactive-retry machinery it suspected
  measures clean: zero unresumed CFG walks with a resolved condition,
  and zero cases of `flow_var_to_var` early-returning on a link whose
  target had not already received the value.
- The remaining `match_cache` hypothesis in the old "Not fully traced"
  section is moot for this failure: dispatch never runs, because
  `all_applications` returns before calling `pattern_match` (`a0->out`
  is bottom) or the edge is gated off before `analyze_edge` flows
  anything.

What 097's reordering most likely did was change *which contours a pass
reaches*, and therefore which edges `clear_results` covers — a
state-hygiene consequence, not a scheduling one.

The prior-art survey collected in that filing
([closed/073](073-teach-splitter-productive-vs-inert-context.md),
[closed/057](057-sorted-tolist-fa-nonconvergence.md),
[055](055-FA-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md),
[closed/035](035-nondeterministic-codegen-clone-order.md),
[closed/009](009-fa-violations-nondeterminism.md),
[closed/037](037-matcher-cartesian-cs-product.md),
[closed/003](003-fa-converge-determinism.md)) remains accurate as
background on this loop and is worth reading before touching it —
`closed/009`'s "verify before concluding order-dependence" caution in
particular applied to the old diagnosis itself.
