# 208 — whole-compile time: where it goes, stage by stage, and what to do about it

**Status: open.** Filed 2026-11-11 from a profile of complete compiles:
every corpus program by pipeline stage, and the heavy ones by stack
sampling. [207](207-FA-call-matching-re-matches-a-whole-union-per-member.md)
and [186](186-FA-pass-one-confluence-through-shared-builtins.md) looked
inside flow analysis on othello3; this issue is the whole compile, on the
whole corpus, including the stages that are not analysis. Items 1 and 2
below are done and in the tree. The rest is a ranked list, each with its
measurement.

## How it was measured

Tree `a5860b47`, `./pyc` built `OPTIMIZE=1`, the C backend unless stated,
against the rebuilt `libgc` (README, "a collector with a larger mark
stack").

- **By stage:** `PYC_DBG_PHASES=1` on all 77 programs. The 76 other than
  othello3 ran six at a time, which inflates the long ones (plcfrs 65 s
  there, 37 s alone), so the heavy programs were each re-run alone.
- **By function:** a gdb stack sampler, since `perf` is blocked on this
  machine and gdb cannot attach.
  [`repro/208-sampler.py`](repro/208-sampler.py) spawns the compiler and
  interrupts it on a timer; [`repro/208-attr.py`](repro/208-attr.py) charges
  each sample to the nearest frame that is not a container, allocator or
  collector; [`repro/208-tree.py`](repro/208-tree.py) prints the inclusive
  tree. 100 to 670 samples per program. The sampler slows a short compile
  (hello world 0.66 s to 1.7 s), so use its proportions, not its seconds.
- **By pass:** `-v`'s `PASS n COMPLETE` lines on the 76.

**Run-to-run noise is large.** othello3 alone was 178 s and then 154 s on
the same binary an hour apart (flow analysis 152 s and 127 s). Compare
arms back to back, and trust a difference under 15% only after repeats.

## Where the time goes

Alone, in seconds:

| program | wall | frontend | flow analysis | clone | optimize | write | C compiler |
| --- | --- | --- | --- | --- | --- | --- | --- |
| othello3 | 154.0 | 1.8 | 127.5 | 10.3 | 1.1 | 3.7 | 9.4 |
| ac_encode | 54.7 | 0.3 | 53.3 | 0.0 | 0.0 | 0.0 | 0.7 |
| sunfish | 42.9 | 1.8 | 36.6 | 0.8 | 0.2 | 0.1 | 3.1 |
| plcfrs | 37.0 | 0.5 | 30.8 | 1.0 | 0.3 | 0.1 | 4.0 |
| doom | 26.5 | 1.2 | 22.9 | 0.2 | 0.1 | 0.0 | 1.8 |
| rubik | 16.0 | 0.4 | 4.3 | 6.7 | 0.6 | 0.1 | 3.5 |
| softrender | 10.9 | 0.3 | 8.1 | 0.4 | 0.1 | 0.1 | 1.7 |

The corpus is two populations, and they want different work:

| | programs | wall | frontend | flow analysis | clone | C compiler |
| --- | --- | --- | --- | --- | --- | --- |
| over 10 s (six at a time; othello3 alone) | 13 | 509 s | 3% | 79% | 7% | 9% |
| 10 s and under (six at a time) | 64 | 212 s | 14% | 42% | 3% | 37% |

The median program compiles in 3.2 s: 0.4 s frontend, 1.2 s flow analysis,
1.2 s C compiler. A ten-line program is 0.66 s: 0.26 s frontend, 0.38 s C
compiler, 0.02 s analysis. That floor is what the test suite pays 502
times on each backend.

Optimize and write-C are 1% each everywhere since 186's fixes. They are
not on the list.

## The list, by what it buys

### 1. `controlling_ifs` walked the function once per branch arm. DONE

`split_for_constant_demand` asks which `if`s a violating statement is
control-dependent on. `controlling_ifs` (`fa.cc`) collected every node
that reaches the site, then for each `if` among them ran a **fresh forward
walk from each successor** to see whether it reaches the site, with a new
node set per walk. In a function with many branches that is (branches ×
nodes) per violation, per pass. The sampler put it at 78% of ac_encode's
compile and 45% of doom's, plus most of their collections: ac_encode
allocated 54 GiB.

The backward walk already holds the answer. A successor reaches the site
exactly when it is in the backward-reachable set, so the test is a
membership probe. The forward walk remains only for a function whose
backward walk hit its 20 000-node cap.

| | before | after |
| --- | --- | --- |
| ac_encode | 54.7 s, 54.1 GiB allocated, 23 collections | 4.2 s, 1.9 GiB, 1 |
| doom | 26.5 s, 17.5 GiB, 9 | 9.2 s, 2.7 GiB, 1 |

Generated C byte-identical on ac_encode, doom, msp_ss and pylife. `make
test` green (502 passed, 15 known, 0 failed on each backend).
`check__default__a5860b47+13c0efd6` equals
`check__default__6209a4c6+5b838f16` in every column on all 77 programs.

### 2. Call matching (207): done

othello3, 168 s sampled: `pattern_match` is 50 s inclusive, 30% of the
whole compile. `Matcher::pattern_match_sym` owns 29.5 s of it, and 22 s of
that is `Vec::set_in` probes. sunfish: 22 s of its 35 s of passes is
matching. plcfrs: 17.5 of 39.7 s.

**Measured 2026-11-11.** The first reading of that profile, 894 set
intersections a dispatch, each over a set about that size, was wrong. A temporary
probe (removed) recorded every `find_arg_matches` call: the argument's
width, the candidates left when it runs, how many candidates each member
is accepted by, and the call's time. The probe's own bookkeeping is in the
seconds, so read the shape.

othello3, the rows that cost anything:

| width | candidates | calls | time | accepted per member |
| --- | --- | --- | --- | --- |
| 952 | 1 | 235 640 | 35.1 s | 0.01 |
| 936 | 6 | 35 573 | 6.6 s | 0.01 |
| 890 | 894 | 2 202 | 10.4 s | 0.95 |

sunfish:

| width | candidates | calls | time | accepted per member |
| --- | --- | --- | --- | --- |
| 41 | 143 | 7 140 | 6.2 s | 96 |
| 42 | 69 | 12 608 | 3.4 s | 46 |
| 23 | 142 | 7 485 | 3.1 s | 100 |
| 25 | 69 | 8 045 | 0.9 s | 46 |

**Two different costs, again.**

- **othello3 asks one method about 950 classes, 235 640 times, and about
  ten say yes.** This is the member loop of `obj.attr`
  ([207](207-FA-call-matching-re-matches-a-whole-union-per-member.md)): one
  `go` against the whole receiver union. For each member the matcher
  fetched that member's own visible-function set (two map lookups, a cache
  miss each) and probed it for the one candidate.
- **sunfish's output is the cost.** 143 `__eq__` candidates, and each of 41
  members is accepted by about 96 of them, because the formal is untyped.
  `update_match_map` then records every (member, candidate) pair, with a
  `Map` get and put for each: 4 000 pairs a call. Nothing is being
  searched for; the answer itself is width times candidates.

**Tried and dead: sharing the intersection between members.** Members with
one Sym get the same answer, and the visible set is a union over
ancestors, so the candidates can be intersected with each ancestor's set
once per call. Exact (0 mismatches in 2.4 M comparisons on five programs,
generated C byte-identical on four) and worthless: sunfish 32.8 s to 33.0 s
of flow analysis, plcfrs 31.3 to 30.9, and othello3 130.6 s to **303.9 s**,
96 GiB allocated, because 900 members are 900 Syms and the memo is pure
allocation. The intersection is not the cost on sunfish and is not
shareable on othello3.

**Done 2026-11-11, three changes, all exact, all on by default.**

1. **The members are not walked** (`find_arg_matches`, `IFA_ARGINVERT`).
   A candidate admits a member only if the member's Sym is a specializer of
   the candidate's formal type, and a Sym knows its CreationSets
   (`creators`, plus its abstract one). So the members any candidate could
   admit are found from the candidates, by probing the argument's type. A
   member with at most one candidate is confirmed by one probe of its
   visible-function set; one with several goes to `pattern_match_sym` as
   before. The work is capped at four probes per member, after which the
   members are walked: a candidate with an untyped formal has every Sym as
   a specializer, which is sunfish's case and is left alone.
2. **Standing dispatch is on** (`IFA_STANDING`,
   [207](207-FA-call-matching-re-matches-a-whole-union-per-member.md) step
   2). It was exact and off. Two things made it cost othello3 12 s: its
   state was rebuilt at every real dispatch of a send whether or not a
   firing ever used it (514 648 records for 22 102 uses), and two
   membership tests on the argument type were linear scans. A send recorded
   eight times and used for under an eighth of them is no longer recorded,
   and the tests are probes. Its counters now print only in check mode or
   under `IFA_DBG_STANDING`; with it on they went to stderr of every compile.
3. **A stale cache entry, found on the way.** `sym_match_cache` keeps the
   visible functions per (Sym, position). A type created during flow
   analysis can be asked about before `build_type_hierarchy` has given it
   its dispatch types; the answer then holds only its own functions, and it
   was cached for the rest of the compile. So what a later dispatch saw
   depended on whether an earlier one had asked. timsort has one such Sym,
   and its generated C differs by two temporaries and two `goto`s depending
   on it. It surfaced because the inversion skips the early question. An
   answer is not cached until the type's dispatch types are built
   (`visible_at`). 207's unexplained kmeanspp difference under
   `IFA_STANDING` was probably this: it is gone.

Each compiled alone, flow analysis / wall:

| | both off | inversion | inversion and standing |
| --- | --- | --- | --- |
| othello3 | 124.7 s / 153.6 s | 100.8 s / 129.5 s | 103.5 s / 132.6 s |
| sunfish | 33.2 s / 39.1 s | 33.6 s / 39.6 s | 21.3 s / 27.2 s |
| plcfrs | 31.2 s / 37.3 s | 31.2 s / 37.3 s | 24.3 s / 30.5 s |

In the sweep at `-j32` the compile phase is 182 s to 149 s and othello3
176 s to 143 s.

**Exact.**
- Generated C is byte-identical with both off and both on, on all 77
  programs (`compile__IFA_ARGINVERT_0_IFA_STANDING_0__a5860b47+69628639`
  against the default sweep's output).
- `IFA_ARGINVERT=2` walks the members as well and compares each one's
  answer: 243 184 388 members over 280 597 inverted calls, 0 mismatches
  (`compile__IFA_ARGINVERT_2__a5860b47+69628639`). The check can fail: the
  first version missed a Sym's abstract CreationSet, which is not among
  its creators, and it reported 890 mismatches on three programs.
- `IFA_STANDING=2`: 65 682 predictions, 0 mismatches.
- `check__default__a5860b47+69628639` equals
  `check__default__a5860b47+13c0efd6` in every column on all 77.
- `make test` green.

**Tried first and replaced: a pre-filter for eight or fewer candidates**
(test each member against the candidates' formal types before the general
path). 131.6 s to 117.5 s on othello3. The inversion does the same thing
without visiting the members.

**What matching costs now** (sampled, all three changes on): sunfish about
5 s of 29 s, plcfrs 4.5 s of 33 s, othello3 about 20 s of 140 s, and a
third of othello3's is collections its allocations trigger.

**Not done, and why.**

- **"Admits every member" as a shared filter.** It was the plan for
  sunfish and plcfrs. With standing dispatch on, what it would remove
  (`update_match_map`, the per-member loop) is about 3 s on each. Not worth
  a change to what a filter is.
- **Keeping routes across passes.** 207 measured 92% of sunfish's miss
  time as re-derivation. That was 58 s of misses; the whole of matching is
  now 5 s there. On othello3 it is still worth looking at, after the
  collector (item 4), which is now larger than matching on that program:
  30 s of 140 s.

### 3. Most programs spend most of their passes placing a few splits

A pass re-derives every type from bottom. Across the 76 programs the
passes sum to 306 s, and the first four passes of each are 74 s of that.

| | passes | flow analysis | in the first 4 passes | EntrySets at 95% of final by pass |
| --- | --- | --- | --- | --- |
| plcfrs | 67 | 39.7 s | 13.9 s | 7 |
| softrender | 62 | 9.6 s | 1.2 s | 35 |
| quameon | 42 | 11.0 s | 1.9 s | 6 |
| loop | 44 | 3.2 s | 0.4 s | 35 |
| genetic2 | 46 | 2.3 s | 0.3 s | 29 |
| go | 27 | 5.7 s | 0.8 s | 6 |

The median is 16.5 passes, 29 programs take 20 or more. quameon is the
plain case: from pass 5 every pass costs 0.18 s and adds 0 to 12
EntrySets, 37 times over. 186 measured the same shape on othello3 (passes
5 to 15: 45 s for about a hundred splits).

Two different levers, to be told apart by measurement:

- **Fewer passes.** Count splits per pass per stage with
  `PYC_DBG_CASCADE`. If the tail is one stage acting on a handful of
  contours per pass while the cascade gate holds the stages below it, the
  question is whether those stages could act in the same pass on contours
  the higher stage did not touch. The gate cannot lose a demand (AGENTS.md)
  but nothing has measured what it costs in passes.
- **Cheaper passes.** Re-derive only what a split can reach
  ([closed/111](closed/111-FA-selective-invalidation-per-pass.md) found
  per-pass work flat and FA time in the early passes' matching; with
  matching reduced, the flat tail is now the larger share on most
  programs). [198](198-decision-snapshot-across-compiles.md) is the
  same idea across compiles.

**Stop condition:** if no stage's tail splits are independent of the
stage above (each pass's splits are caused by the previous pass's), fewer
passes is not available and only cheaper passes is.

### 4. The collector

| | collector, of the sampled compile |
| --- | --- |
| othello3 | 28.5 s of 168 s |
| sunfish | 12.4 s of 65 s |
| doom | 6.5 s of 29 s (gone with item 1) |
| plcfrs | 1.6 s of 40 s |

Marking is single-threaded: bdwgc starts its marker threads at the first
`pthread_create` and the compilers never create a thread. 186 measured
`GC_start_mark_threads` as a loss with the packaged library, where the
mark stack overflowed, and estimated about 10 s for othello3 with the
rebuilt one. Not measured in that combination; measure it.

The other half is allocation volume. othello3 allocates 14.8 GiB and
sunfish 11.9 GiB to hold 2 to 4 GiB live. In the samples the collections
are triggered from `Vec::set_expand` under `Matcher::prune_uncoverable`,
`type_minus_partial_applications`, `set_filters` and `update_match_map`:
short-lived sets built per dispatch. Item 2 removes some of that. A
per-`Matcher` scratch arena would remove more.

### 5. Clone is quadratic in CreationSets per Sym

rubik: clone is 6.7 s of 16.0 s, more than its flow analysis. othello3:
10.3 s. `determine_basic_clones` (`clone.cc`) compares every pair of
CreationSets of one Sym, member by member, and records each difference in
both CreationSets' `not_equiv` sets (`make_not_equiv`, 2.7 s of rubik's
6.7 s in hash-set inserts). Equivalence classes are then derived from the
pairs.

Direction: partition, don't compare pairs. Give each CreationSet a
signature (element or not, member count, each member's basic type and
constant set) and bucket by it. That decides all the tests but
`cs_member_merge_enlarges`, which is pairwise by nature and only needs to
run inside a bucket. **Stop condition:** if `cs_member_merge_enlarges` is
not transitive within a bucket, the classes still need the pair set, and
the gain is limited to skipping the cross-bucket pairs.

### 6. The C compiler: 37% of a small compile

Measured on chess (`chess.py.c`, 450 KB) with the flags `Makefile.cg`
uses:

| step | time |
| --- | --- |
| parse only (`-fsyntax-only`) | 0.43 s |
| `-O0` | 0.54 s |
| `-O2` (the default) | 1.29 s |
| link | 0.08 s |
| a ten-line program, `-O2` | 0.29 s, of which 0.28 s is `pyc_c_runtime.h` |

Three independent things:

- **A precompiled header for `pyc_c_runtime.h`.** It pulls in
  `<coroutine>`, `<type_traits>`, the socket and locale headers: 34 000
  preprocessed lines before the program's first. With a PCH the ten-line
  program compiles in 0.07 s instead of 0.29 s, and chess in 1.09 s
  instead of 1.29 s. That is about 0.2 s off every compile, a third of the
  floor. It has to be built once per (compiler, flags) and invalidated
  when the header changes; `-x`'s cache directory already keys on the pyc
  binary.
- **One translation unit, one core.** The generated C is a single file, so
  the optimizer is serial: 9.4 s for othello3, 3.5 s for rubik, 4.0 s for
  plcfrs, on a machine with 32 cores. Emitting N files (prototypes and
  types in a shared generated header) and running `make -j` divides that.
  Cross-file inlining is the cost; measure the emitted code's speed with
  [199](199-emitted-code-performance.md)'s benchmark before choosing N.
- **`--no-optimize` for the edit loop** already exists and is worth 0.5 to
  2.8 s (plcfrs 4.0 s to 1.2 s). The test suite compiles at `-O2`; most
  tests would be as good a test at `-O0`, but that is a change to what the
  suite covers and belongs to [202](202-health-docs-levers-probes-tests-and-ci.md).

**The LLVM backend does not scale.** It is faster than C on small programs
(chess 0.7 s against 1.2 s, rubik 2.2 against 3.4) and far slower on
othello3: writing the IR takes 29.8 s (3.7 s for C) and compiling it
87.8 s (9.4 s), 257 s in all against 154 s. Not sampled; sample it before
anything else. 186's three write-C fixes were each a per-site walk over a
whole union, and the LLVM emitter (`cg_emit_llvm.cc`) has its own loops.

### 7. The frontend's fixed cost: the builtin library is re-parsed every compile

A ten-line program spends 0.26 s and allocates 0.24 GiB before flow
analysis starts. By samples: parsing `__pyc__/` is 24% of that compile (170
KB of Python through the GLR parser, `exhaustive_parse`), lowering it to
IF1 is 15%, and building SSU form for its functions (`Fun::Fun`,
`build_ssu`) is 10%. None of it depends on the program.

Direction, cheapest first: profile the parse itself (most of its samples
are parse-node allocation under `reduce_one`, which suggests ambiguity the
grammar could resolve earlier); then build functions lazily, only for
what the program reaches, which also shrinks what every later stage
walks. A serialized image of the parsed builtins is the larger change and
ties into 198.

A few programs have their own frontend cost on top: sunfish 1.8 s and
msp_ss 1.3 s, with over 1.3 GiB allocated before analysis (186 noted it).
Not sampled here.

### 8. `getenv` inside hot loops

`fa.cc` alone has 151 `getenv` calls. Most are cached in a static. Some
are not and sit in per-AVar or per-pair code, and they show as leaf
samples in every program: `seed_probe` (reads
`IFA_DBG_SEED` first thing), `collect_type_confluence`,
`cs_numeric_mix_content`, `Matcher::find_all_matches`,
`pattern_match_full_`, and `determine_basic_clones`, whose
`getenv("IFA_DBG_CSCLASSEQ")` runs per differing pair and is 1.2 s of
rubik's 16 s. About 1% elsewhere. Mechanical: read each once. Part of
[202](202-health-docs-levers-probes-tests-and-ci.md)'s probe cleanup, and
the cheapest item here after item 1.

### 9. What is flat

After the above, the mid-sized programs (quameon, softrender, pygasus,
rdb) have no hot spot: `make_AVar` 3 to 9% self time (the hash lookup that
finds an AVar for a (Var, contour)), `unique_AVar`, `flow_var_to_var`,
`collect_var_type_violations`, `add_pnode_constraints`. That is the cost
of a pass, and item 3 is the lever for it. hq2x is the one exception:
15.1 s of its 15.7 s is flow, 1.4 s a pass for 11 passes, 18% of it in
`add_pnode_constraints` itself. It has one very large function; not
looked at.

## Three architectural measurements, 2026-11-11

Taken to decide between larger changes; each is written up where it
belongs.

| question | answer | where |
| --- | --- | --- |
| How much of flow analysis runs library code? | 40% of the corpus's worklist time, 53% for the median program, the same in the first four passes as overall | [198](198-decision-snapshot-across-compiles.md) |
| How much of the flow graph is redundant? | 74% of typed AVars are copies of their one predecessor; 25% are left after collapsing copies and cycles. Propagation itself is 4% of worklist time; the cost is building them per contour per pass | [203](203-shrink-the-graph-fa-walks.md) |
| Are wide unions cones? | Yes, and only othello3 and pygasus have one: 901 classes are 9 cones at pass 0, a 90× reduction in mass | [186](186-FA-pass-one-confluence-through-shared-builtins.md) |

What they change in the list above: item 3 (passes) has a concrete
candidate in 203's copies, since a pass's cost is per AVar and three
quarters of them are copies; and a library-only decision snapshot (198)
is bounded by half of a typical compile, not by all of it.

## Order

1. Item 8 (`getenv`), an afternoon, exact.
2. Item 6's precompiled header: 0.2 s off every compile and the suite.
3. Item 4's marker threads, a one-line experiment against the rebuilt
   collector. The collector is now othello3's largest single cost.
4. Item 5 (clone), rubik and othello3.
5. Item 3's measurement (splits per pass per stage), before any design.
6. Items 6 (parallel C) and 7 (frontend), which are larger.

## Verification

For every item: generated C byte-identical on the witnesses where the
change claims to be exact, `make test` green, and a `check` sweep equal in
every column to the one before. The yardstick is this issue's tables,
re-measured the same way: heavy programs alone, back to back with the
previous binary.
