# 186 — pass 1 pushes a whole-program union through the shared builtins (othello3: FA is ~850 s against a 400 s cap)

**Status: open; the verification below was first met on 2026-11-10.**
Sweep `check__default__0b55bb75+ae4581cf`: othello3 compiles in 352 s at
`-j32` (cap 400 s) and its stdout matches CPython, after
[207](207-FA-call-matching-re-matches-a-whole-union-per-member.md)'s
matching changes. **Later the same day the time after FA went from 172 s
to 25 s** ([below](#after-flow-analysis-2026-11-10)): alone it is 163 s,
and 180 s in sweep `check__default__61d11baa+62bfc8eb`. FA is 136 s of
that, so what is left of this issue is FA. **FA was then profiled**
([below](#inside-flow-analysis-2026-11-10)): matching is about 39 s of it,
the collector about 30 s, and the rest is fifteen passes with no hot spot;
three small fixes took it to about 126 s (157 s alone, 170 s in sweep
`check__default__6209a4c6+5b838f16`). Before that:
othello3 compiles and matches CPython since 2026-10-09,
in 994 s, almost all of it FA. Rewritten 2026-11-03; the fix-by-fix
measurements, the stall-guard deletion and the post-FA profile are in git
(`git show 123ac81:ifa/issues/186-FA-pass-one-confluence-through-shared-builtins.md`).
It also carries [closed/192](closed/192-review-analysis-and-issues-2026-11.md)
C1 and C2.

## Mechanism

Start-minimal means pass 1 has one EntrySet per function, so every shared
builtin contour (`__list_iter__.__next__`, `len`, `join`, `__eq__`,
`append`) carries every caller's values. othello3 (830 `Flip` subclasses,
64 `Put`, 1,848 table stores) forms a ~2,039-CreationSet union there, and
it spreads to tens of thousands of AVars one member at a time. The demand
that would separate the callers is evaluated at quiescence, after the
pass, so the pass must finish first. **Not the cause:** start-merged list
CreationSets (`PYC_CSDCPA1=0` stalls the same way).

## Landed (2026-10-08/09)

Four constant-factor fixes: dispatch was cubic in the receiver union
(`pattern.cc`); the flow worklist defers pushes so a union settles before
readers see it (105 M → 1.85 M out-changes on pass 1); a subset fast path
in `type_union`; one-pass `type_diff_all` in violation collection;
per-index match-cache lookup. Then clone's `cs_member_merge_enlarges`
made linear, the 830-arm dispatch chain collapsed to one slot call when
every arm agrees, and `-g` dropped from the C compile (23 GB → 3.4 GB).
Result: pass 1 184 s, 13 passes, FA ~850 s. The within-pass stall guard
was wrong (it counted minting as progress and was wall-clock) and was
deleted; only the cross-pass guards remain.

What shedskin does: eager CPA (identity, forbidden here), a deferred
connection limit (`CPA_LIMIT`: a call whose candidates × argument
combinations exceed it is not connected until the limit doubles), and
incremental connection. The last two are scheduling, not identity, and
carry over.

## Directions, in order

**2026-11-08:** the mechanism behind these is general, not othello3's.
[207](207-FA-call-matching-re-matches-a-whole-union-per-member.md)
measured it on sunfish and plcfrs. Every growth of an argument union is a
match-cache miss and a full re-match, so the cost is quadratic in union
width. Directions 1 and 2 are now 207's directions 2 and 3, alongside
incremental matching. othello3 stays this issue's witness.

1. **Deferred connection.** Do not connect a call whose receiver union is
   wider than a limit until the pass has otherwise settled, then connect
   it once with the settled union. In pyc the union still pours into one
   minimal contour; the gain is that it arrives once rather than ~2,000
   times. Stop condition: pass 1 under 400 s with the union's holders
   unchanged, else re-profile.
2. **A wide-union representation.** `AType` is a sorted `Vec` with
   hash-consing and four per-instance memo maps never cleared; a wide
   union allocates a new canonical AType per added member and memoises
   every pair; `type_diff`'s `subsumed_by` loop over abstract members is
   O(|a|·|b|). A bitset over CreationSet ids above a width threshold makes
   union, diff and subset O(width/64).
3. **Shrink what each pass walks**: [203](203-shrink-the-graph-fa-walks.md).
4. **Do not walk it again next compile**: [198](198-decision-snapshot-across-compiles.md).

## Verification

othello3 compiles under the sweep's 400 s cap with output matching
CPython; `make test` green; a `check` sweep with no outcome regressions.

## After flow analysis, 2026-11-10

**Measured** with `PYC_DBG_PHASES=1` (new, in the driver: wall seconds per
pipeline stage) and a gdb sampler, since `perf` is blocked on this
machine. othello3 compiled alone, tree `61d11baa`:

| stage | before | after |
| --- | --- | --- |
| flow analysis | 136.7 s | 136.2 s |
| clone | 10.2 s | 10.2 s |
| mark-live-code | 30.3 s | 0.7 s |
| inlining | 66.6 s | under 0.5 s |
| write C | 55.3 s | 3.6 s |
| C compiler | 9.7 s | 9.7 s |
| **wall** | **311 s** | **163 s** |

Three causes, one per stage. Each was quadratic or worse in something
othello3 has a lot of.

- **Inlining: `check_invariants` ran before and after every splice.** It
  walks the whole function and allocates two node sets, so a function
  with *n* inlined sites paid O(*n*) per site, and most of the samples
  were in the collector. It now runs once per function, after its sites
  are done (`inline_check`, `inline.cc`). `IFA_DBG_INLINE_CHECK=1`
  restores the per-splice check, which is what says which splice broke
  the CFG.
- **Write C: `cg_note_blind_cast` expanded the receiver union at every
  slot call.** A `.go()` site notes the 894-class union once per class,
  and each note walked every member's fields. What it records is a
  function of (cast type, actual type, slot), so a triple is now expanded
  once (`codegen_common.cc`).
- **Mark-live-code: `forward_live`** walks three forward steps from a dead
  setter, across every contour's edges, and every dead setter started
  from scratch. AVar liveness does not change inside `mark_live_pnodes`,
  so the answer per (Var, distance) is remembered for the pass
  (`dead.cc`).

**Exact.** othello3's generated C is byte-identical. `make test` is green.
`check__default__61d11baa+62bfc8eb` equals
`check__default__0b55bb75+ae4581cf` in every column on all 77 programs.
In the sweep othello3's compile went 352 s → 180 s and the whole `check`
sweep 593 s → 405 s.

**Not done.**
- Byte-identical C was checked on othello3 only; the other 76 rest on the
  sweep's columns.
- `cg_cast_members` decides whether a union member has the field by
  comparing field NAMES. It was there before and is untouched, but it is
  the kind of test AGENTS.md's "never decide by name" rules out.
- clone's 10 s (`determine_basic_clones`, `sets_by_f`) was sampled only 8
  times and not looked at.

## Inside flow analysis, 2026-11-10

**Measured** on tree `6209a4c6`, othello3 alone, with a gdb sampler (280
stacks at 0.5 s over FA), `-v`, `PYC_DBG_STAGEDELTA=1` and
`GC_PRINT_STATS=1`. `PYC_DBG_PHASES=1` now also prints bytes allocated and
collections so far on each line.

| where (of 137 s, by samples) | time | |
| --- | --- | --- |
| call matching | 39 s | [207](207-FA-call-matching-re-matches-a-whole-union-per-member.md). `-v` says 46 s, 28 s of it in the first pass; its timer includes the collections that land in a match |
| the collector | 30 s | eight collections; their marks are 1.5 to 4.6 s each, 27 s together |
| flow (`analyze_edge`, `flow_vars`, `push_forward`) | 16 s | |
| `complete_pass` (violations, `collect_results`) | 15 s | about 1 s a pass |
| split stages | 13 s | `split_css_by_defs` 2/3 of it |
| primitive sends | 12 s | |
| dispatch outside matching | 8 s | `dispatch_unchanged`, closures |

**There are fifteen passes and none is spurious.** Passes 1-4 are 42, 14,
12 and 7 s; from pass 5 a pass is 3.8 s (2.8 flow, 0.3 match, 0.7 extend)
plus a collection when one lands in it. `TYPE_CONFLUENCE` splits 104 and 39
EntrySets on the first two passes and then 3 to 23 on every pass through
12, out of about 1650 confluences; `CS_DEF_PARTITION` acts on every pass
through 11, a handful of CreationSets each time. Pass 13 is the first
where no split stage acts; `merge_equivalent_entry_sets` then retires 9
contours (4892 to 4883), which costs pass 14, and pass 15 is the
re-convergence after mid-FA inlining. So passes 5-15 are 45 s of
re-deriving 930 000 AVars to place about a hundred splits. That is
[closed/111](closed/111-FA-selective-invalidation-per-pass.md)'s subject and
it is the largest thing left here that is not matching.

**The collector: the mark stack overflows, on every collection.** othello3
holds 2 to 3 GiB live in the 4 GiB heap and collects eight times in FA.
bdwgc starts its mark stack at 4096 entries and doubles it once per
collection, at the start of the one after it ran out. A mark that runs out
drops entries and re-scans the heap for marked objects. The log shows 250
to 400 `Mark stack overflow` lines per collection, at 4096, 8192, ... 131072
entries, one doubling each; the collections after FA, with the stack at
over a million entries, mark in 0.4 s.

With the stack grown before the heap fills (a chain built to need it, ten
collections at start-up), every mark of FA is 0.12 to 0.36 s and FA is
137 s to 112 s. **Nothing was landed, because no place to do it pays:**

| when the stack is grown | othello3 | others |
| --- | --- | --- |
| at start-up | FA 137 to 112 s | +0.8 s on every compile to reach a million entries and +2.9 s for the four million used here (a compile of a ten-line program allocates 0.3 GiB and takes 0.6 s) |
| once, at 3/4 of the heap allocated | FA 137 to 122 s (the ten collections cost 10 s at 2.3 GiB live) | the four other programs that collect all lose: ac_encode 51.6 to 57.8 s, doom 28.3 to 32.1, sunfish 39.6 to 44.2, plcfrs 38.5 to 42.1 |
| marker threads only (`GC_start_mark_threads`) | FA 137 to 154 s | not measured |
| `GC_FREE_SPACE_DIVISOR=1` | no change: still eight collections, heap still 4.2 GiB | |

The other four overflow too (86 to 728 lines) but their marks are 0.1 to
1.6 s, so growing the stack costs more than it returns. The corpus is
bimodal: five programs allocate more than the heap (ac_encode 54 GiB, doom
17.5, othello3 14, sunfish 12, plcfrs 9) and collect; the other 72
allocate 0.27 to 2.5 GiB and never do. The collector has no call or
environment variable for the mark stack's size (`INITIAL_MARK_STACK_SIZE`
is a build-time constant), so the fix that costs nothing is a libgc built
with a larger one. That is outside this tree and is the open decision.
The marker threads are also not running: where bdwgc handles fork it
starts them at the first `pthread_create`, and the compilers create no
thread.

**Landed, three per-pass costs** (`6209a4c6` + this change):

- **`qsort_by_id` was quadratic on sorted input.** It took the first
  element as its pivot, and `collect_results` sorts `fa->css`, 11 500
  CreationSets already in id order from the pass before: 0.2 s a pass.
  Now `std::sort` (`fa.h`).
- **The call index was two `std::map`s rebuilt every pass**, an entry per
  edge argument, 0.4 s a pass. Both are only looked up, so they are hash
  maps, and the one range query ("does any edge return into this AVar")
  is a set (`callindex`, `fa.cc`).
- **`type_diff_all` asked every filter about every member**: |a| x |bs|,
  and a send on the 894-class union has a filter per class. The filters'
  members go into one set first (`fa_lattice.cc`).

FA 136 s to about 126 s, a steady pass 3.8 s to 3.2 s, alone 163 s to
157 s. Generated C byte-identical, `make test` green,
`check__default__6209a4c6+5b838f16` equals
`check__default__61d11baa+62bfc8eb` in every column on all 77 programs;
othello3 in the sweep 180 s to 170 s.

**Not done.**
- The mark stack, above.
- sunfish's frontend allocates 2.05 GiB before FA starts, and msp_ss, doom,
  sha and minpng 1.3 to 1.4 GiB, against 0.3 GiB for a small program. Seen
  in the survey, not looked at.
- What is left inside a steady pass (flow 2.4 s, extend 0.5 s) has no
  single cost above about 3% of FA: `unique_AVar` under
  `make_closure_var`, `dispatch_unchanged`, `split_css_by_defs`'s
  allocation, `sos_member_demanded`.
- The sampler keeps 16 frames, so inclusive counts above the leaf are low.

