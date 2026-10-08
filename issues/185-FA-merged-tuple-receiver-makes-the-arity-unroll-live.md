# 185 — a merged tuple receiver makes the whole arity unroll live (sunfish: 537 s)

**Status:** open. Diagnosed 2026-10-08. Nothing is fixed yet.

## Symptom

sunfish compiles correctly and matches CPython on both backends. It takes
**537 s and 3 GB**, so `corpus_sweep.sh`'s 400 s cap records it as a
compile timeout. Flow analysis accounts for ~460 s of that, over 47
passes. The passes are badly uneven:

| passes | time | ess (max) | violations (max) |
| --- | --- | --- | --- |
| 1–13 | ~375 s (pass 10 alone: 111 s) | 4,692 | 40,627 |
| 14–47 | ~85 s (1–5 s each) | 1,113 | 419, then 0 from pass 28 |

At pass 14 the analysis collapses (ess 4,690 → 1,053, violations
14,302 → 410). It is not a stall or pass-limit trip: no `STALL` or `PASS
LIMIT` line is logged, and `reanalyze()` promotes nothing after pass 0.

## Mechanism

1. **The tuple methods are unrolled to the program's largest tuple
   literal.** `inject_tuple_methods_over` (`python_ifa_main.cc`)
   generates `__eq__`, `__lt__`, `__str__`, `__hash__`, `__contains__`,
   `count` and `index` as one constant-index step per position, each
   guarded by `n >= k` where `n = len(self)` (issues/119, issue 069).
   sunfish's `pst` piece-square tables are 64-element tuple literals, so
   every method has 64 guarded steps.

2. **The guards fold only when `n` is a constant**, that is, when the
   contour's `self` is record tuples of a single arity. Then the steps
   past the arity are dead and cost nothing, which is the case the design
   relies on.

3. **In the early passes `self` is every tuple in the program.**
   `tuple.__getitem__` starts as one contour per constant key
   (`__pyc_clone_constants__(key)`) with ONE merged receiver. So `t[k]`
   returns element k of every tuple that is ever indexed. That union then
   flows back into the 2-tuple moves (`i, j = move`), the transposition
   keys `(pos, (depth, root))`, the `(score, move)` pairs and the
   `directions` rays. Measured with the probe below: at pass 1 the
   formals of `__eq__`, `__lt__`, `__hash__` and `__pyc_to_bool__` carry a
   **49–52-CreationSet** union, which reaches plain ints too (`move:i`,
   `move:j`, `bound:gamma`, `bound:best`).

4. **With `self` merged, `n` is a union of arities**
   ({0, 2, 4, 8, 20, 64, runtime}), so no guard folds, and all 64 steps of
   every tuple method are live in every such contour. The steps' sends
   then fail, each with its own violation: tuple `__getitem__` BOXING and
   PRIMITIVE_ARGUMENT on `key`, and `__eq__` NOTYPE. Demand splitting
   separates the receivers a few at a time. `tuple.__getitem__` alone
   reaches **3,066 of the 4,125 EntrySets** at pass 10: each one pairs a
   22–33-CS receiver union with one constant key 0..63. Many of those
   (receiver, key) signatures occur 5–6 times, as distinct EntrySets with
   identical positional types (not yet explained).

So the number of passes comes from the demand cascade untangling a
receiver merged from the start (the ifa/157 shape), and the cost per pass
comes from the unroll depth multiplying every contour that cascade
creates.

## The measurement that separates the two

A temporary cap on the unroll count (not committed; it is an arbitrary
lever, and a heterogeneous record tuple past the cap would fall back to
the runtime-index tail that issues/119 removed) gives:

| unroll | compile | passes | output |
| --- | --- | --- | --- |
| 64 (as shipped) | 537 s | 47 | matches CPython |
| 8 | **48 s** | 51 (hits the cap; zero violations from pass 32) | matches CPython (126 lines, identical) |

The pass count barely moves; the cost per pass falls about 10×.

## Step 1: where the merge is (measured 2026-10-08)

Three probes (`PYC_DBG_STAGEDELTA`, `IFA_DBG_CSDEFSPLIT`, and the
`EDGEPART` / `WIDEELEM` lines of `IFA_DBG_VIOLHIST`) over passes 0–14.

**Not starvation.** `TYPE_CONFLUENCE` acts on every pass (220–1,686 new
EntrySets each), and `CS_DEF_PARTITION` acts right after it on passes
0–12. The ifa/157 cascade is not what holds this back.

**The tuple methods are not the merge point.** Their contours are fed
almost entirely by their own recursion: at pass 1, 130 of `__lt__`'s 134
in-edges, 65 of `__hash__`'s 66 and 65 of `__eq__`'s 147. The unrolled
`self[k] < t[k]` calls the same method on element tuples. The few
non-recursive edges carry only 1–3 distinct types, all of them large
unions. They amplify a union made upstream. Splitting them by call site
(this issue's first draft of a plan) would act downstream of the merge,
so that plan is withdrawn.

**Phase A, passes 0–8: the start-merged list.** `cs=1489` is the one
`list` CreationSet, shared by 61 creation points, and its element channel
holds 49 CSs at pass 1. That is the union that reaches everything else
(`move = movecache[i][j]`, then `i, j = move`, and so on). Route 4
partitions it, but only a group or two per pass:

| pass | 0 | 1 | 2 | 3 | 4 | 5 | 6–10 |
| --- | --- | --- | --- | --- | --- | --- | --- |
| defs | 50 | 32 | 21 | 17 | 11 | 11 | 7 |
| KEY sets | 1 | 2 | 20 | 20 | 5 | 4 | 1–2 |
| groups | 2 | 3 | 4 | 5 | 1 | 3 | 1 |

The key is a bit per assign set of the CSFlowGraph. Its assign sets are
few because the WRITERS (`list.append`, `__setitem__`) are themselves one
contour each at the start, so every write looks like one assignment. The
writers split only as `TYPE_CONFLUENCE` reaches them, and their formals
carry the very union `cs=1489` is spreading. The key therefore sharpens
only as fast as the feedback loop unwinds.

**Phase B, passes 9–12: tuples built inside a builtin.** Once `cs=1489`
is gone, the widest channels are tuples with ONE creation point:
`cs=1802` and `cs=7881` (both `make_seq` inside `tuple.__add__`) and
`cs=1710`. Their elements hold 11–27 CSs until pass 12. Each carries its
own demand (`DEMAND-ADDED`, irrepresentable element) on every pass, and
route 4 declines each time (`DECLINED (single creation point)`). The
mechanism meant for this case is the ES split as a means (`PYC_ESBLOCK`):
split `tuple.__add__`'s contour so that its one creation point becomes
several. It reports `candidates=0` for `cs=1802`. Every `a + b` on tuples
in the program shares one result contour until `TYPE_CONFLUENCE`
separates `__add__`'s callers, which happens a few per pass (it adds
4–14 EntrySets per pass on passes 11–14).

The collapse at pass 13 (violations 14,302 → 410) is the end of phase B.

## Phase B is phase A's wake (measured 2026-10-08)

Phase B was the first target: why does no rung split the contour that
owns a single-creation-point tuple whose demand fires on every pass? The
answer is that no type partition exists to split on, because the
callers carry phase A's union.

- The defs==1 rungs are both off by default: `PYC_ESDEFS1` (verdict
  "MEASURE" in ifa/146) and `PYC_CSCALLSITE`. Turned on, `esdefs1` never
  reaches these tuples, because its gate wants a violation, field or
  dispatch that NAMES the CreationSet (`viol_named=0` on every one).
  Their demand is their own irrepresentable element.
- With the gate widened to accept that demand (an experiment, reverted),
  the rung reaches them and declines every time, correctly:
  - `cs=1802` climbs to `tuple.__add__` (es=113): "all 37 edge(s) pass
    the same types" at pass 0, 5 at pass 3.
  - `cs=3860` (`list.__mul__`): the same, over 2 edges.
  - Most of the others climb to `__main__` (the module-level `pst`
    padding loop), which has one in-edge: "1 arg-carrying in-edge(s)".
- Why the edges agree: sunfish has a handful of real tuple
  concatenations, all over int tuples. Most of `tuple.__add__`'s 37 pass-0
  edges are int arithmetic sites such as `i + ...` in `Position.move`.
  Their `i` is the polluted union, which includes tuples, so `+`
  dispatches to `tuple.__add__` as well. And `tuple.__add__` builds its
  result through `r = []`, which is the start-merged `cs=1489`. Either
  way, the single-creation-point tuples carry phase A's element union.

So there is no phase-B fix that does not first remove phase A's union.
The type-keyed split declines because the types really are the same
there. Phase A is the work.

## Phase A: the merge is circular at pass 0's fixed point (measured 2026-10-08)

Two route-4 changes were tried behind flags and reverted. Both rest on a
misreading, and the measurements correct it.

**Edge-keyed assign sets (`PYC_CSEDGEKEY`).** Read the assigned type at
a shared writer contour's in-edges instead of at its merged formal. Result:
byte-identical to the default (`cs=1489` still `KEY sets=1 groups=2` at
pass 0). At pass 0 only seven writes reach `cs=1489`'s element, all
carrying the 49-CS type. Their writer contours' callers already pass that
same union. `append` (es=78) is called by `set.add`, `list.extend`,
`print_pos`, and twice by `dict.__setitem__` (once for keys, once for
values). The callers of the writers are themselves shared builtins, so
there is no edge where the types differ.

**Joint ES-block (`PYC_ESJOINT`).** Hold every shared contour on the walk
terminal at once, instead of one at a time. It acts elsewhere (it splits
`__pyc_rehash__` for `cs=3861` and `append` for `cs=11281`), but never on
the dict, `cs=1488`. At pass 0 that CS has one shared-contour candidate
(`clear`), and nothing on any assign set's walk is a merge node (an
`IFA_DBG_CSHUB`-style probe: no path node has two dict-carrying
predecessors). Every assign set reaches all six dict creation points
directly.

**What that means.** The six dicts (`piece`, `pst`, `directions`,
`tp_score`, `tp_move`, and `print_pos`'s) are indistinguishable by type at
pass 0, honestly:

- every dict's `__init__` writes the same types into its members
  (`_len = 0`, `_mask = 0`, `_keys = []`, `_vals = []`);
- those lists are one CreationSet, `cs=1489`, because each is a single
  creation point inside the shared `dict.__init__`.

What distinguishes the dicts (`pst` holds tuples, `tp_score` holds
`Entry`, `piece` holds ints) is only in the lists' contents, and they
are merged. The list needs the dicts split, and the dicts need the list
split. Every write into either passes through `dict.__setitem__` (es=55,
one contour), whose callers see the polluted fixed point: reading
`pst[k]` returns every dict's values, and those flow on into the other
dicts. At quiescence the union sustains itself, so no type-keyed
partition exists anywhere for a demand to act on. The current route 4
untangles it only through its 1-bit early key (on a path or not), a group
or two per pass. That is the "does real work though its key carries no
element information" behaviour issues/128 recorded.

**The handle that remains.** The only place the dicts differ is
`dict.__setitem__`'s call sites, before the fixed point pollutes them.
A demand-driven answer therefore has to split a writer contour by call
site (the demand on `cs=1489` decides whether, the call site decides
which) and then re-merge at convergence the contours whose re-derived
types agree (ifa/170). That stays within AGENTS.md's 2026-09-06
refinement only with the re-merge: without it, it is 1-CFA by the back
door. `PYC_CSCALLSITE` mode 2, the existing call-site rung, was measured
a corpus-wide wash (ifa/146) without a re-merge. This is a design
decision, not a measurement.

## Containing the unroll: levers A and B (measured 2026-10-08)

Two off-by-default levers attack the unroll's cost directly instead of
the merge. Both were interim. Step 6 deleted A and made B the
generator's own lowering, with no flag (see "Open after step 2", item 1).

- **A, `PYC_OOBIDX`** (`fa_prims.cc` `index_object` / `set_index_object`,
  `const_index_past_end` in `fa.cc`). A constant index past a fixed-arity
  tuple receiver's end contributes nothing, instead of falling into the
  non-constant branch that flows every field.
- **B, `PYC_TUPIX`** (`python_ifa_main.cc`). Inline `index_object` for
  `self[k]` / `t[k]` in the generated tuple methods, so a step does not
  mint a `tuple.__getitem__` contour per constant key.

| variant | sunfish compile | `make test` |
| --- | --- | --- |
| baseline | 537 s | green |
| A, drops every past-end receiver | 443 s, output same | 5 failures |
| A + B | 365 s, output same, no warnings | 5 failures |
| B alone | ifa/057 stall guard in pass 7 | green |
| A narrowed + B | rc=124 at 900 s (749 s in passes 1–12) | green (committed form) |

A's five failures (`tuple_arity_union`, `nested_tuple_repr`,
`star_args`, `itertools_module`, `itertools_product_repeat_tuple`) are
steps past EVERY receiver's arity. The step is dead, but its `n >= k`
guard cannot fold: `len` over `{1-tuple, 2-tuple}` is the abstract `int`,
because the constant cap is 1. So the step's comparison runs on bottom
and is an unresolved call. The narrowed form, which is the one committed,
keeps the old all-fields union when no receiver has the field. That is
test-clean, but on sunfish those unions are what B's inlining amplifies.

So the defect is that liveness of a step comes from `n` rather than from
the receivers' types. Arity is type (ifa/132), and dispatch already
filters per receiver CreationSet (`set_filters`, `ifa/if1/pattern.cc`).
**The plan is to dispatch on arity.** It has three parts:

- add an arity constraint on a formal, checked in `find_best_cs_match`
  and ranked by subsumption above an unconstrained formal;
- generate one straight-line body per arity the program uses, with no
  guards;
- keep a loop fallback for tuples that have no fixed arity.

A merged receiver then reaches one target per arity without being split,
and A and B are deleted.

**Step 1 landed: the arity constraint.** `Sym::dispatch_arity` on a
formal is enforced in `update_match_map` and ranked in `subsumes_arg`
([DISPATCH.md](../DISPATCH.md) §12). The match cache keys a lost static
arity through `dispatch_arity_epoch`. In `tests/ir/dispatch/05_arity_merged.ir`,
`%g`'s receiver holds a 2-tuple and a 3-tuple, and its send reaches
`%m2` and `%m3` with one arity each (rc=0, ess=5). Without the
constraints the same program is an ambiguous call (rc=-1, ess=7).

**Step 2 landed: `tuple.__eq__` per arity.** `emit_tuple_arity_eq`
(`python_ifa_main.cc`) emits one straight-line overload per arity, plus
four general ones:

| `self` | `t` | body |
| --- | --- | --- |
| k | tuple k | element by element |
| k | tuple, no fixed arity | length check, then the same |
| no fixed arity | any tuple | `t == self` |
| no fixed arity | tuple, no fixed arity | runtime loop |
| any | any | `False` |

"No fixed arity" is `DISPATCH_ARITY_DYNAMIC` (-2). Python annotations are
dropped by the grammar, so the constraints travel in a side table keyed on
the parameter's PyDAST (`pyc_formal_dispatch`), applied in `gen_fun_pyda`.
`(1, 2) == [1, 2]` is now False, as in CPython. The mirror,
`[1, 2] == (1, 2)`, is `list.__eq__`, and is still True: an
`isinstance(l, list)` guard there untyped `l` in two suite tests.

A call FA resolves to several arities needs a RUNTIME dispatch, and
tuples have no classtag. `poly_dispatch_arity_plan` (`codegen_common.cc`)
builds a chain on both backends:

- a null test for a None method;
- the length header;
- the new **fixed-arity bit**: the high bit of a tuple header's
  `total_len`, set where a tuple whose CreationSets all have a static
  arity is built.

The bit is needed because a record 2-tuple and `tuple([a, b])` are
otherwise identical at run time (`itertools_product_repeat_tuple`).
Without it, two attempts were measured dead:

- refusing the dispatch aborted;
- taking the general body for both re-dispatched on values FA never typed
  it for.

`total_len` is otherwise a list's capacity. Only the two resize paths read
it, and they mask the bit. Test: `tests/tuple_eq_arity_dispatch.py`.

## Open after step 2 (2026-10-08)

1. ~~**The other generated tuple operations are still the max-arity
   unroll.**~~ **Done in step 4.** `emit_tuple_arity_methods` gives each
   of them one straight-line body per arity a fixed-arity tuple can have,
   plus an unconstrained runtime-loop body. The methods are `__lt__`,
   `__str__`, `__hash__`, `__contains__`, `count`, `index`,
   `__deepcopy__`, `__pyc_bytes_fmtargs__` and `__pyc_toints__`.
   - The arity set is every literal's (builtin modules included), every
     arity a `*args` call can build, and the REPL floor; sunfish's is
     `{0, 1, 2, 4, 8, 64}` before the builtins add theirs. A missed arity
     falls to the loop body, so the set bounds precision, not correctness.
   - `__lt__` dispatches on both operands: a `(k, m)` body for each pair
     in the set, `(k, dyn)` and `(dyn, k)`, and the loop.
   - `__eq__` keeps `0..max_arity`, because its unconstrained body answers
     False.

   **Measured on sunfish: compile 537 s -> 168 s, no warnings, output
   identical to CPython** (except its own `TIME` line).

   **Step 6: levers A and B.**
   - **A (`PYC_OOBIDX`, `const_index_past_end`) is deleted.** With
     straight-line per-arity bodies, no constant index is ever out of
     range, so it has nothing left to do.
   - **B is kept, without its flag.** Re-measured under arity dispatch, it
     cut sunfish from 168 s to 134 s. The generator now emits the
     `index_object` primitive with a literal index directly, and the
     `PYC_TUPIX` text rewrite is gone.
   - Result: sunfish compiles in **136 s** (537 s at the start), with no
     warnings and output identical to CPython.
2. **`[1, 2] == (1, 2)` is True** (CPython: False). `list.__eq__` never
   checks that its argument is a list. An `isinstance(l, list)` guard
   untyped `l` in `builtin_zero_arg_ctor` and `list_tuple_eq_ne`
   (`'l' has no type`), so it was reverted. Why the guard untypes the
   operand is unexplained.
3. **Same-arity tuples with different element types in one list**
   (`[(1, "a"), ("a", 1)]`) do not compile, at `2f61802c` and after.
   The arity body's single contour holds both CreationSets, `self[0]` is
   `{int, str}`, and no stage splits the contour by element type.
4. **A `no_static_arity` flip is not re-dispatched within its pass.** It
   happens in `make_kind`, mid-pass, and changes no AType, so the sends it
   affects keep their old dispatch until the next pass. A flip in the last
   pass would leave the dispatch stale, and nothing checks for it. The
   fixed-arity header bit is decided from the converged CreationSets, so
   such a stale dispatch would show up at run time as "arity dispatch: no
   branch matched".
5. **Duplicate method definitions in a user class are an ambiguous call**
   (`class A: def f..; def f..`); CPython uses the last one. The
   generated arity overloads rely on every def registering, so a fix must
   leave arity-constrained defs alone.

## Directions (none taken)

The step-1 findings narrow these. The first two below target the tuple
methods and are superseded:

- **Phase B (done, negative).** See "Phase B is phase A's wake" above.
- **Phase A (two approaches measured dead).** See "Phase A: the merge is
  circular" above. The open option is a call-site split of the shared
  writer, with a re-merge at convergence.


- **Fold the guard per receiver, not per contour.** The unroll is
  correct only when the receiver's arity is known. A guard that cannot
  fold is a demand: the method's contour holds receivers of different
  arities. Arity is already part of tuple type identity (ifa/132), so
  splitting the method's ES by receiver arity is a type split, not
  provenance. The open question is why that split takes ~13 passes to
  happen instead of one.
- **Find what merges the receiver of `tuple.__getitem__`.** Before the
  unroll is involved at all, the pass-1 union of 49 CSs is a confluence:
  either the single starting contour of `__getitem__` (correct by the
  minimal-start rule, but it should then split at the first quiescence),
  or something upstream that carries all tuples in one AVar. Backtrack it
  (AGENTS.md, "Find the confluence").
- **The duplicate EntrySets.** Identical (receiver, key) signatures
  appearing 5–6 times say a split is happening on something other than
  the positional types. Find out what.
- **The tail.** With the cap, passes 32–51 have zero violations and still
  grow ess (1,033 → 1,113); that is the zero-violation churn case of the
  stall guard and is separate.

## Probe

`IFA_DBG_VIOLHIST=1` (`fa.cc`, at the end of each pass) prints, per pass:

- violation totals by kind, and the top (kind, function, variable) groups (`VIOLHIST`);
- EntrySets per function (`ESHIST`);
- for the top function, its contours by positional-formal signature (`ESSIG`);
- every caller handing a formal a union of more than 10 CreationSets (`FEED`).

## Verification

sunfish compiles under the sweep's 400 s cap with output unchanged. The
`compile` column of a check sweep has no new failures, and programs that
already compile take no longer.
