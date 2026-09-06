# 129 — plan: make CreationSet splitting demand-driven

**Status:** open, filed 2026-09-04. Root cause is
[128](128-cs-identity-over-discriminates-vs-element-type.md); the
architectural dependency is
[111](111-FA-selective-invalidation-per-pass.md). **Goal:** make
CLAUDE.md's first line true — *the primary purpose of IFA is the demand
splitting of Creation Sets.*

## START HERE — status as of 2026-09-05

The rest of this file is a chronological record and is long. It has four
sections titled "Step 4" because the step was re-scoped three times; read
them in file order or not at all. This block is what a cold start needs.

**The lever is `PYC_CSDCPA1` — start merged, one CreationSet per sym.**
Default off. It is the only thing measured that moves the goal number, and
it takes `ess` DOWN rather than up, unlike every key-side experiment here:

*Numbers re-measured 2026-09-06 on `88c773c9`; both arms are current.*

| | default | `PYC_CSDCPA1=2` |
| --- | --- | --- |
| container CS / shapes (corpus) | 3713 / 626 = **5.93** | 2764 / 591 = **4.68** |
| `pratio` | 3.89 | **3.04** |
| `pyc` segfaults / compile timeouts | 0 / 0 | **0 / 0** (was 2 / 3) |
| corpus compile failures | 2 | **16** (was 20) |
| corpus run_fail / stdout_differs | 39 / 24 | 34 / 20 |
| suite failures | 0 | **11** at `=2`, **14** at `=1` (was 16 / 19) |

**What has landed:** [132](132-arity-is-representation-not-provenance.md)
— arity participates in CreationSet identity, which removed every compiler
crash and hang under the flag. Then, 2026-09-06,
[133](133-split-a-container-on-its-element-type.md)'s two fixes:
`__delitem__`'s false `merge_in` (default path, worth −35 container
CreationSets corpus-wide, 5.99 → 5.93, every verdict unchanged) and
`FAPassStage::CS_DEF_PARTITION` / `split_css_by_defs` — the pass's last
rung, which partitions a CreationSet by its creation points when a type
confluence lands on its contour. Default on and measured inert at the
default (zero splits on chess/rubik2/sieve/go); under the flag it takes
the bill **16 → 11**.

**The bill, 16 → 11 as of 2026-09-06** (full table at *Step 4 — the bill*):

| group | n | state |
| --- | --- | --- |
| [133](133-split-a-container-on-its-element-type.md) element leak | 5 → **1** | **fixed** — `__delitem__`'s false `merge_in`, then `CS_DEF_PARTITION` (`split_css_by_defs`, default on). Only `splitter_cartesian_product` remains |
| `splitter_*` goldens | 3 | decided — route changed, `CALLS` verified equal, re-blessable at flip |
| layout / member width | 2 | untouched; a sibling of 132, likely the same shape |
| `builtins`, `plcfrs_*` | 2 | still `mixed basic types`, but NOT CS_DEF_PARTITION declining wrongly — see 133 |
| undiagnosed | 4 → **2** | `test_heapq`, `match_seq_star`. The other two were 133, and produced no diagnostic *because* they were |
| illegal call argument type | 2 → **1** | `listcomp_element_separation` fixed; `deepcopy_copy_of_copy_chain` remains |

1 + 3 + 2 + 2 + 2 + 1 = 11.

**Next actions, in order:**

1. **Build [133](133-split-a-container-on-its-element-type.md)** — its
   question is **answered, not open.** *Re-scoped 2026-09-05, unblocked
   2026-09-06.* Its four-line reproducer turned out to be a FALSE
   CONSTRAINT in `__pyc__` — `__delitem__` asserted, via
   `merge_in(self, [])`, that a deletion adds an element type — and fixing
   that clears the whole `pop`/`del`/`remove` family and
   `tests/list_pop_insert.py`. The remaining three are the real issue, and
   measuring one (`list_append_is_amortized`: `cs=995`, **`defs=7`**, all
   user code) rules out the library-vs-user option on the evidence. The
   author's directive then rules out the other: **provenance is never the
   answer** (CLAUDE.md), and "record which creation point wrote this
   element" is provenance. What is left, and what to build, is
   **wholesale split by creation point** — shedskin's ladder route 4,
   which needs no attribution at all, only a caller that partitions
   `cs->defs` and hands the groups to `split_css`.
2. **The layout pair.** 132 found arity is a representation property CS
   identity must respect; member width is another. Probably small.
3. **The 4 undiagnosed**, three of which produce no diagnostic at all.
4. **Corpus triage** — 19 compile failures against the default's 2. The
   corpus catches what the 311-test suite does not.
5. **Convergence cost** — passes up 40-200%.
   [111](111-FA-selective-invalidation-per-pass.md)'s `IFA_SELECTIVE` is
   the untouched lever and this is its first reason to pay.

**Independent of all of the above:**
[134](134-remove-the-frontend-forced-split-opt-in.md) — the frontend's
`__pyc_clone_constants__` forced splits, worth 69 suite tests and
**measured not to be subsumed** by start-merged.

**Superseded, do not restart from these:** `PYC_CSELEM=3` (−18%, regresses
`rdb`, keyed at mint time when the element is unfilled — dcpa1 supersedes
it); `PYC_CSSITELESS` (made contours *worse*, 4 of 5 programs);
`PYC_CSRESPLIT`, `PYC_CSREJOIN` (built, measured, inert or superseded).
[131](131-demand-driven-constant-splitting.md)'s cap-strip premise is
falsified.

## Part 1 — audit: how shedskin does it

All line numbers are `shedskin/infer.py` at `~/projects/shedskin` **7aef6e02**
(2294 lines), verified against the source 2026-09-04. The demand splitter is
`ifa()` (1469), called once per iteration of `iterative_dataflow_analysis`
(1800), immediately after `propagate()`.

### The demand test

`ifa()` first builds `allcsites`: every constraint node with **no incoming
edge** (`not n.in_`), grouped by the `(class, contour)` it can hold — that is
shedskin's definition of an allocation site. Then, per candidate class
(`ifa_classes_to_split`, 1678) and per contour `dcpa in range(1, cl.dcpa)`
(1485), `ifa_split_vars` (1498) walks the class's **type variables**
(`cl.tvar_names()` — for `list` that is its element) and calls
`ifa_flow_graph` (1715) to build:

| | meaning |
|---|---|
| `assignsets` | incoming edges grouped by their merged type — *which distinct types are assigned into this type variable* |
| `paths` | `backflow_path` (2031) from each assign set's targets, following only edges that carry `(cl, dcpa)` |
| `creation_points` | per assign set, the nodes on its path with no incoming edge |
| `csites` | the allocation sites appearing on ANY of those paths — sites that actually reach an assignment |
| `emptycsites` | `allcsites[(cl,dcpa)] - csites` — sites of this contour reaching no assignment at all |
| `n.paths` | per node, which assign sets it lies on; drives ladder routes 1 and 3 |

Then the gate (1526):

```python
if len(csites) + len(emptycsites) == 1:
    continue          # one creation site -> nothing to split, ever
```

**That is the whole idea.** A data contour is a candidate for splitting only
when two or more allocation sites have MERGED into it. Splitting is a
response to an observed merge; it is never a consequence of structure.

### The escalation ladder

When there IS more than one site, shedskin tries progressively coarser splits
and stops at the first that works:

1. **`ifa_split_no_confusion`** (1585) — entered only when the tvar holds >1
   simple type AND there is >1 assign set, or when there are both assign sets
   and empty csites. The *unconfused* sites (`len(n.paths) == 1`) plus the
   empty ones are grouped by the attribute-type tuple each would produce, and
   each group is split off. This route can **reuse an existing contour** —
   see below.
2. **Confluence-point split** (1546-1560) — for a node with >1 incoming edge
   where some creation site appears in more than one assign set's
   creation-point set (`ifa_confluence_point`, 1703), partition the sites by
   incoming edge (`ifa_determine_split`, 1660, which subtracts overlaps so the
   partition is disjoint). Taken only if `2 <= len(remaining) < 10` (1552);
   returns immediately.
3. **Partition csites across paths** (1571) — group sites by the set of types
   found on their paths; if that yields >1 group, split the FIRST group off.
   Uncapped, and does not return.
4. **Wholesale** (1576) — `len(paths) > 1 and 1 < len(csites) < 10`: give every
   site its own contour. Returns immediately.

**pyc's default is shedskin's step 4, applied unconditionally and with no
cap.** That one sentence is the gap.

### Contour reuse — the half of it that is not splitting

`ifa_class_types` (1632) builds, for every existing contour of the class, the
tuple of its tvars' merged simple types, and indexes it both ways:
`nr_classes[dcpa] -> types` and `classes_nr[types] -> dcpa`. When
`ifa_split_no_confusion` has grouped sites by the attribute-type tuple they
want, it consults that index first (1618):

```python
if subtype in classes_nr:      # reuse contour
    nr = classes_nr[subtype]
    split.append((cl, dcpa, csites, nr))
```

— the sites are **moved onto an existing contour** rather than a new one being
minted. Contour identity is therefore *the deduced element types*: two
allocation sites that agree on them land in one contour whatever their
provenance. This is the operation `creation_point` does not have — it memoizes
per AVar and never moves a site that already has a CS. It is also the same key
`PYC_CSELEM=3` computes structurally, which is why step 2 below is a down
payment on step 3 rather than a detour.

### Three further constraints worth copying

- **Scope.** `ifa_classes_to_split` (1678) considers ONLY parametric builtin
  containers — `list, tuple, tuple2, dict, frozendict, defaultdict, set,
  frozenset, deque, __iter, array` — and only where `cl.mv.module.builtin`.
  User classes are never data-contour split. (shedskin matches these by
  identifier; pyc's structural equivalent already exists and is already used
  in `creation_point`: `s->element`.)
- **A fan-out cap of 10 — on two of the four routes.** Confluence refuses
  outside `2 <= remaining < 10`, wholesale outside `1 < csites < 10`.
  No-confusion and path-partition are uncapped, but each splits off one group
  per iteration. `CPA_LIMIT` (175) is a separate 10 on the function side, and
  doubles (`gx.cpa_limit *= 2`) only after a whole round that hit it.
- **Decisions per iteration — *not* one, as this issue first claimed.** Only
  routes 2 and 4 return `split` and end the pass. Routes 1 and 3 append to the
  shared `split` list and fall through; `if split: break` (1543) then ends
  that contour's scan and `ifa_split_vars` returns `None`, so `ifa()` keeps
  walking the remaining contours and classes and can record several
  no-confusion / path-partition splits in a single iteration. Once anything
  has been recorded, each later contour gets a no-confusion attempt and
  nothing more.

(`ifa_classes_to_split` ends with `random.shuffle(classes)`, 1699 — the source
of the 38/38/39 iteration non-determinism measured in
[111](111-FA-selective-invalidation-per-pass.md).)

### What makes it safe there and not here

shedskin can start merged and separate later, and can move a site onto an
existing contour, because the derived network is thrown away every iteration.
`backup_network` (2072) is taken ONCE, before the loop; every iteration ends
with `restore_network` (2090). Exactly two things survive a round:

- **`gx.alloc_info`** — the decision table, keyed `(enclosing function ident,
  cartesian-product tuple, alloc AST node) -> (class, contour)` (1904). It is
  replaced by `gx.new_alloc_info` on every propagate (1831), so an entry that
  is no longer re-derived is DROPPED. The table forgets, and that is precisely
  what makes a merge reversible.
- **seeded types on module-level constructor nodes** (1927). Constructor nodes
  *inside functions* are explicitly cleared first (1916) and re-seeded from
  the table by `ifa_seed_template` (1936).

`ifa_seed_template` also carries a decision into contexts that did not exist
when it was made: for an unknown `alloc_id` it searches for a "mother" entry
differing only where a component contour is a split CHILD of the mother's
(`a[0].splits[a[1]] == b[1]`, 1994) and inherits its contour, falling back to
`gx.orig_types[node]`. `cl.splits` is that lineage — new contour -> parent
contour. pyc records no lineage.

pyc's ATypes are monotone instead: an over-merge that produces a mixed or
self-referential element type is permanent, and the splitter cannot walk it
back — measured in
[128](128-cs-identity-over-discriminates-vs-element-type.md) as `PYC_CSMOLD=1`
giving `mixed=2` and `pass_limit_hit=1` on
`tests/deepcopy_copy_of_copy_chain.py`.

## Part 2 — what pyc already has

More than expected. `creation_point` already contains four of the pieces,
all inert by default:

| pyc | shedskin analogue | state |
|---|---|---|
| `fa->var_elem_key` / `var_elem_ambig` | `gx.alloc_info` — a cross-pass, content-keyed decision table | built, only read by `cselem` |
| `cselem` route (`PYC_CSELEM=1/2`) | keying `list<T>` on T | **default 0** |
| `cselem_shape_reuse` (`PYC_CSELEM=3`) | ditto, structural shape key | **default 0** |
| `csmold` (`PYC_CSMOLD`) | the mold fallback — one instance per site | default 3, which excludes split children, i.e. almost everything |
| `creators` route | — | **dead code**: `if (nvars != -1 \|\| x->vars.n != nvars) continue;` always continues |

What is missing is the demand test itself. Nothing in pyc asks *how many
creation sites feed this contour's element*, so nothing can decline to
split. `PYC_CSSPLIT=1` makes the CS follow the ES split — structure, not
demand.

## Part 3 — the plan

Ordered so each step is independently verifiable and independently
revertable. Steps 1-3 are reachable without the architecture change;
step 4 is the architecture change.

### Step 1 — make the demand ratio a reported number — **DONE 2026-09-04**

`IFA_DBG_DEMAND` emits one line per converged analysis; `corpus_sweep.sh`
records its columns per program (`ess css container_cs shapes pshapes`)
and aggregates `cs/shapes=N/K=R` onto the summary row in
`sweeps/INDEX.md`. It is set inside the worker rather than through `-e`,
so it does not fork the cache key and every future sweep carries it for
free. **Nothing below should be judged on `ess` alone — this ratio is the
thing the goal statement is about.**

**The plan's element-only definition was wrong, and the code does not use
it.** `make_kind` fills `cs->vars` per position and deliberately does NOT
flow it into the generic element — ifa/issues/104: a heterogeneous tuple
read by constant indices keeps precise per-field types only because its
element stays bottom, which is exactly what `tuple_able()` tests for. So
`[1,2,3]` and `["x","y"]` BOTH present an empty element AVar (measured),
and an element-only census calls them one shape and reports a 2x
over-discrimination that is not there. A container's content lives in two
channels and the census reads both, giving two denominators:

| | identity it stands for |
|---|---|
| `shapes` | content classes MERGED across the element and every position — shedskin's one-tvar contour, where two int lists of different lengths are ONE |
| `pshapes` | the element set plus the ORDERED per-position class-sets — record identity, where arity and field order count |

A demand-driven splitter lands between them, so `cs/shapes` bounds the
over-discrimination from above and `cs/pshapes` from below. `cs/pshapes >
1` is splitting that no identity justifies.

The probe is READ-ONLY and must stay so: `get_element_avar()` is not an
accessor — it calls `unique_AVar` (which CREATES the AVar) and sets
`cs->added_element_var`, which gates element numeric coercion in
`fa_coerce_numeric_confluences`. A CS with no element AVar is counted
with a bottom element channel (its content, if any, is in `vars`) and
tallied as `novar`.

**Baseline, `check__default__5cf5baf7+1a013d49`, 76 of 77 programs:**

```
cs/shapes = 3748/626 = 5.99      pratio = 3.92
```

Higher than the 4.6x in `creation_point`'s own comment (1994 CS / 341
shapes) because that count reads only the element channel and skips every
CS without an element AVar. The corpus verdict is unchanged from the
previous default sweep — compile_fail=2 run_fail=39 stdout_differs=24
with_warnings=44, identical — confirming the change is diagnostics-only.

The worst offenders are not the slowest programs:

| program | container CS | shapes | pshapes | ratio | ess |
|---|---|---|---|---|---|
| linalg | 245 | 5 | 15 | **49.0** | 1981 |
| stereo | 192 | 6 | 9 | **32.0** | 178 |
| kanoodle | 152 | 8 | 23 | 19.0 | 550 |
| rubik | 194 | 11 | 26 | 17.6 | 852 |
| chess | 169 | 12 | 19 | 14.1 | 1591 |
| go | 60 | 12 | 17 | 5.0 | 657 |
| sunfish | 20 | 4 | 7 | 5.0 | 118 |

`stereo` is the case to reason from: 192 container CreationSets for 6
content shapes and only 178 EntrySets. The CS excess is not downstream of
contour growth there — it is minted directly by `creation_point`, one per
allocation site, which is the mechanism 128 names.

*Verify:* the number exists, on the corpus, before and after each step. ✓

### Step 2 — land the keying that already works

`PYC_CSELEM=3` is suite-clean (0 failed, no new failures vs default) and
takes chess 48.2 s → 14.9 s, `ess` 1591 → 985, list CS 95 → 36. It
sidesteps monotonicity by keying identity on the converged element shape
up front rather than merging and hoping to split later.

- `./corpus_sweep.sh -m check -e "PYC_CSELEM=3"`, per-program diff.
- If neutral on exit codes and stdout: flip the default, raise
  `LLVM_BASELINE_PASS` if it moves.
- Separately, resolve the dead `creators` branch. Do **not** blindly
  change `||` to `&&` — with `&&` and `nvars == -1` it reuses the first
  creator of that sym unconditionally, which is a whole-program merge.
  Establish what it was for, then either delete it or gate it.

*Verify:* corpus `check` neutral; step 1's ratio improves.

**Sweep result, `check__PYC_CSELEM_3__40c21ff9+adf4abe8` vs
`check__default__a935532b+adf4abe8`** (2026-09-04, `-j 12` inside a
`MemoryMax=100G` scope, 702 s, **zero OOM kills**):

```
default    programs=77 compile_fail=2 run_fail=39 stdout_differs=24 with_warnings=44  cs/shapes=3748/626=5.99 pratio=3.92
CSELEM=3   programs=77 compile_fail=3 run_fail=38 stdout_differs=24 with_warnings=43  cs/shapes=3081/626=4.92 pratio=3.25
```

**The ratio is the result: 5.99 → 4.92**, 3748 → 3081 container
CreationSets, −667 (−17.8%), and `pratio` 3.92 → 3.25. That is step 1's
number moving for the first time.

Four rows differ across all 77 programs, and only one is a behaviour
change:

```
linalg     warns 34 -> 28
rdb        compile_rc 0 -> 1        warns 130 -> 120
sudoku5    warns 82 -> 212          (already compile_fail in both)
tarsalzp   warns 213 -> 231
```

Every exit code and every stdout verdict is otherwise identical. **`rdb`
is the whole regression**: `'v' has mixed basic types:( int64 str )` — the
mode merging two containers that must stay apart. Read the summary
carefully: `run_fail` 39 → 38 and `with_warnings` 44 → 43 are **not**
improvements, they are `rdb` leaving those denominators by failing earlier.

So the gate on flipping the default is one program, and it is a real
over-merge rather than a fabricated failure — worth root-causing before
step 3 rather than capping around, since a demand-driven splitter has to
get exactly this case right.

#### `PYC_CSELEM=3` could not be swept at all until 2026-09-04 — `atype_shape` was exponential

The sweep this step asks for OOM-killed the machine every time it was
attempted. Nine kernel kills in one afternoon (08:44, 09:10, 10:36,
10:58, 11:50, 12:10, 12:28, 12:40, 13:05), victim `pyc` every time, at
39–47 GB RSS:

```
kernel: Out of memory: Killed process 937322 (pyc)
        total-vm:63568240kB  anon-rss:47494512kB
        constraint=CONSTRAINT_NONE  task_memcg=.../tmux-spawn-....scope
```

Global OOMs, not cgroup-local — at 12:40 the allocation that tripped it
came from an unrelated Java process, and each kill took the sweep's whole
`tmux` scope with it. The four attempts died at the same place with the
same four programs in flight: **`kanoodle`, `plcfrs`, `quameon`, `rdb`** —
the same set `sweeps/INDEX.md` rows 45-46 recorded as CSELEM=3 "timeouts"
in the old compile-only sweeps. They were never timeouts. `compile` mode
cannot tell a slow program from one that needs 47 GB, which is the same
blind spot [102](102-corpus-programs-compile-then-abort-at-runtime.md) names from the other
direction.

**It was not contour explosion.** `ess=187 css=1154` stayed frozen for the
whole run and pass 0 never completed; the process died asking Boehm for
7.5 GiB in one step. The backtrace (`gdb` must SPAWN the process —
`ptrace_scope` blocks attaching) is one `std::string`:

```
creation_point            fa.cc:679
 → cselem_shape_reuse     fa.cc:9826
 → cselem_shape_key       fa.cc:9803
 → atype_shape_cached     fa.cc:9751
 → atype_shape × 7        depth 0..6
 → std::string::append("#")  _M_create(capacity=4026531840, old=2013265920)
```

**Root cause.** `atype_shape_cached` refused a union wider than
`kShapeMaxMembers` (4) — but only at depth 0. The recursion had no width
guard at all, and `seen` is a PATH stack, so it caught cycles and
re-expanded every shared subtree. With `kShapeDepth = 6` the output is
O(width^depth). Instrumented on `kanoodle`:

```
[csshape-wide] depth=1 width=93 out=8       [csshape-wide] depth=4 width=93 out=176
[csshape-wide] depth=2 width=93 out=53      [csshape-wide] depth=5 width=93 out=254
[csshape-wide] depth=3 width=93 out=109     [csshape-wide] depth=6 width=93 out=343
[csshape-walk] calls=1000k out=1131001202 depth=6/6 width=93/93
```

**1.13 GB of string at 10^6 calls**, 1131 bytes per call, on a walk needing
93^5 ≈ 7×10^9 calls — roughly 8 TB. It does not finish slowly; it does not
finish.

**The width is [128](128-cs-identity-over-discriminates-vs-element-type.md)
arriving here as a number.** The 93 members are:

```
list x77   A B C D E F G H I J K L Column x1 each   int64 x1  str x1  __pyc_None_type__ x1
```

**77 of 93 are CreationSets of one class, `list`** — kanoodle's row in step
1's table is 152 container CS for 8 shapes. So the mechanism that exists to
*remove* over-discrimination was being defeated by it, on exactly the
programs where it is worst, and nowhere else. That is worth keeping in view
for step 3: any identity scheme that has to serialise a type structurally
will meet the same width.

**Fix (landed).** A sub-shape is emitted as an **id**, not as its text
(`fa.cc:9686-9694`, `atype_shape_id` at `fa.cc:9721`). Ids are handed out by
CONTENT, so two structurally equal sub-shapes always get the same id and two
different ones never do — the key compares exactly as the unfolded string
did, same merges and same splits, at O(width) per level. Content-keyed and
not pointer-keyed on purpose: the shape deliberately erases CreationSet
identity (it emits the CLASS, `name#id`), so two distinct `list` CSs with
equal element shapes must land on one id, which is the whole point of the
mode.

`cselem_shape_ids` is global and monotone like the `cselem_shape_canon` it
feeds — clearing it per pass would let id 7 name one structure in pass 3 and
another in pass 5, and since the canon map never revisits an entry that is a
permanent mis-merge, the same hazard [130](130-FA-identity-keyed-on-sym-name.md)
A2 fixed for `Sym::name`. The `(AType, depth)` memo is the opposite and is
cleared per pass, because `elem_key` moves.

Two further notes on the shape of the fix:

- The `<@>` cycle marker is **gone**. It is path-dependent, so a subtree
  that used one is not a property of `(AType, depth)` and cannot be
  memoized — keeping it fixed the memory and left the same exponential in
  TIME (kanoodle: 4.0 GB peak, zero GC warnings, and still burning the full
  600 s wall). `kShapeDepth` alone bounds the walk, so the marker was doing
  no work the cap was not; dropping it makes the shape a pure function of
  the type, which is what a shape should be.
- `kShapeMaxNestedMembers = 256` (`fa.cc:9716`) is the structural net,
  applied at every depth. Deliberately **not** `kShapeMaxMembers`: with the
  id encoding a wide union costs O(width), so reusing 4 below depth 0 would
  decline kanoodle's width-93 union and give up the entire win on the four
  programs this is for. 4 stays at depth 0, where the question "is
  canonicalizing this site meaningful?" is actually being asked.

**After**, all four complete under a 12 GB cap, zero GC warnings:

| program | before | after | container CS (default → CSELEM=3) |
|---|---|---|---|
| kanoodle | OOM 47 GB | rc=0, 3.5 s, 214 MB | 152 → 149 |
| quameon | OOM 47 GB | rc=0, 18.5 s, 373 MB | 186 → **133** |
| plcfrs | OOM 47 GB | rc=0, 85 s, 842 MB | 187 → 173 |
| rdb | OOM 47 GB | rc=1, 16 s, 408 MB | 167 → 141 |

`rdb` now fails with a real diagnostic instead of an OOM — `'v' has mixed
basic types:( int64 str )` — which is CSELEM=3 over-merging two containers,
and is a step-2 finding in its own right. Kanoodle barely moves (152 → 149):
the 93-wide union that broke the walk is over-discrimination the shape key
still cannot collapse.

*Verify:* default path is unchanged, structurally (every call site is behind
`cselem_enabled() == 3`) and empirically — `kanoodle`, `quameon` and `rdb`
reproduce their `check__default__a935532b+adf4abe8` rows exactly
(`550/1844/152/8/23`, `1267/4024/186/17/32`, `1118/3669/167/15/35`). All six
CI gates green; `PYC_FLAGS=-b ./test_pyc.py` 311/0/14/18/4, unchanged. The
`-e "PYC_CSELEM=3"` sweep is now runnable, and step 2 above records it.

### Step 2b — three things worth taking from shedskin without step 4

Part 1's audit is mostly a list of things that need step 4's reversibility.
These three do not. Each is independently landable, and the first is
arguably a defect in what step 2 already shipped.

**1. Rebuild the canon index from the CURRENT contours; stop accumulating
it forever.**

`ifa_class_types` (`infer.py:1632`) rebuilds `classes_nr` from scratch on
every call — `for dcpa in range(1, cl.dcpa)`, reading each contour's
*current* `gx.cnode[var, dcpa, 0].types()`. It is called per class per
`ifa()` iteration, so a stale entry cannot exist.

pyc's `cselem_shape_canon` (`fa.cc:9799`) is never cleared.
`cselem_shape_reuse` (`fa.cc:9887`) guards only on `it->second->sym != s`
— it never re-checks that the CS still *has* the shape its key names — and
`cselem_shape_claim` (`fa.cc:9895`) fills only absent keys, so nothing
corrects an entry once it drifts. A CreationSet that claimed key K in pass
3 keeps answering for K in pass 20 after its element type has moved.

The code defends this: *"shapes converge, so entries only stabilize, and
merging must be monotone or the canonicalization itself becomes a source of
churn."* Convergence is exactly the assumption
[074](074-FA-cross-pass-oscillation-plan.md) and the adatron oscillation
say is false, on precisely the programs where the canon matters most.

**This does not need step 4.** Rebuilding the INDEX is not unlearning a
merge: a site already routed to CS X stays routed, and only *future*
routing decisions change. The monotonicity the comment is protecting is a
property of the CS graph, not of a lookup table over it.

Unmeasured so far — instrument before changing anything: count reuses whose
CS's current shape no longer equals the key it was found under, per pass,
across the corpus. If that count is zero the comment is right and this
closes; the four programs step 2's sweep can now reach
(`kanoodle`, `plcfrs`, `quameon`, `rdb`) are where to look first.

*Verify:* the stale-hit counter is zero after the change; step 1's ratio
does not regress; `-e "PYC_CSELEM=3"` corpus `check` neutral or better.

**2. Record CreationSet lineage.**

`cl.splits` maps a new contour to the one it split from, and
`ifa_seed_template`'s "mother" search (`infer.py:1994`) uses it to carry a
decision into a context that did not exist when the decision was made,
inheriting from an entry that differs only where a component contour is a
split CHILD of the mother's. Part 1's flat statement stands: **pyc records
no lineage.**

Cheap, monotone-safe, and step 4 /
[111](111-FA-selective-invalidation-per-pass.md) needs it regardless — so
it is better landed as groundwork now than discovered inside the
architecture change.

*Verify:* lineage is recorded and dumpable; no behaviour change (it is
data, nothing reads it yet).

**3. Normalize `None` out of the key where `None` is free.**

`merge_simple_types` (`infer.py:2116-2122`) drops `none` from a
multi-member set unless the set also holds `int_`/`float_`/`bool_`. So
`{None, str}` keys identically to `{str}`, while `{None, int}` keeps its
`None`. pyc emits `__pyc_None_type__#63` as a full member of the shape, so
those two get different shapes and therefore different contours despite
having one representation — over-discrimination in the key itself, which
is what step 1's ratio measures.

The guard transfers intact, and it coincides with the representability
boundary [048](../../issues/048-none-int-field-pair-runtime-abort.md)
already documents: `None` is free next to a pointer and needs a tag next to
a scalar.

*Verify:* step 1's ratio improves on programs whose unions carry `None`;
no new `mixed`; suite and corpus `check` neutral.

#### …and 3 is not IFA's decision to make

Whether `None` is free in a union is a question about what the TARGET
language can represent. It is not a fact about flow analysis, and it does
not belong in `fa.cc`.

[`IFACallbacks`](../ifa.h) is the established seam for exactly this, and
`bool_is_numeric()` is the precedent to copy: whether `bool` is a subtype
of `int` is a language question, so ifa defaults to its own generic answer
(`false`) and `PycCallbacks` overrides it because Python's
`isinstance(True, int)` is true. The `narrowing_*_name()` hooks are the
same shape, added for
[082](closed/082-narrowing-wrapper-names-hardcoded-in-fa.md) so that FA does not
hardcode one frontend's identifiers.

So item 3 lands as a hook — an element-key canonicalizer, defaulting to no
normalization, with `PycCallbacks` supplying Python's rule *and* its
`int_`/`float_`/`bool_` guard. Baking the rule into `fa.cc` would
reproduce the defect 082 files, one level down.

The test to apply to anything else taken from shedskin: **does the rule
encode what the deduced types ARE, or what the target can REPRESENT?**
Deduction is core and belongs in `fa.cc`; representation is a callback.
Items 1 and 2 pass it — "is this index entry still accurate?" and "what did
this contour split from?" are questions about the analysis itself.
Boxing, mixed-width unions (048), and `None`-in-a-union are all the other
kind.

### Step 2c — what `rdb` says about the mode-3 key

Step 2's sweep left exactly one behaviour regression, and it turned out to
be worth more than its fix: `rdb` compile_rc 0 → 1, `'v' has mixed basic
types:( int64 str )`.

**The mechanism.** `atype_shape_id` recurses into a member's content only
`if (cs->sym->element)`, and `Sym::element` is created for **`list`,
`vector` and `tuple` only** (`python_ifa_sym.cc:109-110,129` — `tuple`'s
added by [104](closed/104-unify-list-and-tuple-in-analysis.md)). `dict` never gets
one. So a dict receiver serialises as the bare token `dict#4799`, carrying
nothing: **75 of 75 occurrences in `rdb`, never once with a nested part.**

Two split children of `dict.__init__` — one for an int-keyed dict, one for
a str-keyed dict — therefore present the identical key:

```
[csshape] p=14 v10757|list#61|3 -> reuse cs=1266  shape=dict#4799 split=58 cmc=0 fun=__init__
[csshape] p=14 v10759|list#61|3 -> reuse cs=1267  shape=dict#4799 split=58 cmc=0 fun=__init__
```

Their list contours are fused, the shared element becomes `{int64, str}`,
and `__pyc__/04_sequence.py:115-116` — `for v in self` / `chunk = chunk +
chr(v)`, inside `__pyc_tobytes__` — reads it, surfacing at the
`bytes([...])` on `rdb.py:244`.

pyc already knows this failure. `07_dict.py`'s own comment describes the
same union from the other direction — *"self.mapSocks.keys() (int-keyed)
and headers.keys() (str-keyed) share one CreationSet whose `_keys` unions
int64 and str across BOTH, unrelated dicts"* — found on `webserver.py` and
fixed there with `__pyc_clone_constants__` / `clone_methods_per_cs`. Mode 3
re-creates it by a different route.

**The obvious guard is the wrong fix, and the measurement says so.**
`creation_point`'s mold route carries two guards mode 3 lacks
(`fa.cc:714-716`): the `clone_methods_per_cs` exclusion (issue 045) and
`!(mold == 3 && split_child)` — [105](105-type-degeneration-in-shared-generic-methods.md)'s
*"the mold must not undo a SPLIT"*, whose recorded symptom is the same
family as rdb's. Every rdb reuse is a split child, so porting it fixes rdb.

Measured before recommending it:

| | reuses | split children | cmc |
|---|---|---|---|
| rdb | 22 | **22** | 0 |
| kanoodle | 3 | **3** | 0 |
| quameon | 56 | **56** | 0 |

**100% of shape-key reuses are split children.** Undoing splits IS the
mechanism, not a side effect — so that guard would make `PYC_CSELEM=3` a
corpus-wide no-op and take the whole −667 CS win with it. It is the retreat
CLAUDE.md describes: numbers collapse, suite goes green, real question
unasked. `cmc=0` throughout, so the 045 guard is irrelevant here.

**What shedskin keys on instead — and it is not the receiver.** The direct
analogue of `cselem_shape_canon` is `classes_nr`, consulted in
`ifa_split_no_confusion` (`infer.py:1617-1624`):

```python
for subtype, csites in subtype_csites.items():
    if subtype in classes_nr:          # reuse contour
        nr = classes_nr[subtype]
        split.append((cl, dcpa, csites, nr))
        cl.splits[nr] = dcpa           # lineage, recorded right here
    else:
        classes_nr[subtype] = cl.newdcpa
```

`subtype` is the tvar types of **`cl` itself** — the container being
allocated. The receiver of whatever method allocated it never enters the
key. Where shedskin does mention `self` it is as ONE component of the
enclosing contour's cartesian product, never instead of it: `cart =
((parent.parent, n.dcpa),) + cart` before `gx.alloc_info[parent.ident,
cart, n.thing]` (`infer.py:1904`).

Two consequences for pyc's key, which is `v<site>|<class>|<receiver
shape>`:

- It is **blind** — `dict` has no content channel the shape can read, while
  shedskin's `Class.tvar_names()` (`python.py:217`) gives
  `dict/frozendict/defaultdict` → `["unit", "value"]`. Worth noting what
  that unifies: `["unit"]` is pyc's `elem_key` and `tuple2`'s `["first",
  "second"]` is pyc's per-position `cs->vars`. shedskin has ONE notion of
  a class's type variables where pyc has two channels — and the shape key
  reads only one. That is step 1's `shapes`/`pshapes` asymmetry appearing
  as a defect rather than as a metric.
- It is a **proxy**, and the wrong one. Even a perfectly content-aware
  receiver shape does not determine what a list built inside
  `dict.__init__` will hold. Fixing the blindness removes rdb's collision;
  it does not make the key mean the right thing.

**Why pyc uses the proxy at all**, and why that is the real finding: keying
on the allocated container's own content requires knowing that content,
which does not exist when the CS is first minted — hence "keyed on the
durable, converged element type", the `%`-unfilled decline, and the
per-pass dance. shedskin can key on deduced content because it MOVES a site
onto an existing contour after the fact, and can do that because the round
is discardable. That is step 4 /
[111](111-FA-selective-invalidation-per-pass.md) again.

**So `rdb` is evidence for step 4, not a bug with a local fix.** Two things
follow, in order:

1. *Stopgap, no reversibility needed:* remove the blindness so the mode is
   safe to run — give the shape key dict's content, either as the second
   channel step 2b item 3's sibling would add. Reachability and cost are
   settled below.
2. *The actual fix:* key the canonical contour on the allocated container's
   own converged content, which needs the ability to re-route a site after
   that content is known. Step 4.

Do **not** flip the default on `PYC_CSELEM=3` while the key is a blind
proxy: rdb is the one program that happens to notice, and 22/22 says the
mechanism producing the −667 is the same one producing the miscompile.

**Is the content reachable? Yes — and the blindness is not about `dict`.**

pyc's `dict` is an ordinary Python class in `__pyc__/07_dict.py`, whose
`__init__` is exactly `self._keys = []`, `self._vals = []`, `self._len =
0`. So a dict CS's `vars` holds `_keys` and `_vals`, and those are `list`
CSs with real `elem_key`s: the key and value types are reachable through
the SECOND channel without giving `dict` an element sym at all. No repeat
of 104 is needed.

That also closes the loop on which lists were fused. `v10757` and `v10759`
are the two `[]` literals in `dict.__init__` — **`_keys` and `_vals`
themselves.** Fusing their contours unions the key types and the value
types of two unrelated dicts, which is the bug the comment block right
above those two lines documents: `{1:1}` and `{"a":1}` merging int/str and
hard-failing the C build, fixed by
[076](closed/076-mutation-driven-receiver-divergence-not-cloned.md) by removing the
class-body defaults.

**Mode 3 re-creates 076 by a different route**, and 076's own title is the
sentence: *"Monotonic type growth lets a shared/prototype CreationSet
permanently contaminate a container read, even after `split_css` correctly
separates the instances that share it."* There the sharing came from a
class-body default acting as a permanent setter; here it comes from the
canon map handing two correctly-separated contours one CS. Same monotone
trap, same contaminated container read, one layer up.

But the blindness is much wider than `dict`. Every receiver shape observed
across three programs:

| program | reuses | receiver shapes |
|---|---|---|
| rdb | 22 | `dict#4799` ×18, `str#90` ×2, `int64#75` ×2 |
| kanoodle | 3 | `list#61<23>` ×3 |
| quameon | 56 | `box_nopbc#11509` ×12, `fermion#15937` ×11, `atomic_STO#14910` ×11, `LCAO#15172` ×10, `wave_func_single_det#16376` ×4, 6 more ×1-2 |

quameon's are all **user classes**. They have no element sym either, so
they shape as a bare `name#id` exactly like `dict` — content-blind in the
same way, and for the same reason. Of the 81 reuse events measured, **74
(91%) are decided on a receiver whose shape is nothing but its class
name** while that receiver does have content the key cannot see. Four are
scalars (`str`, `int64`), where bare is correct. Three carry real content.

So for anything but `list`/`tuple`/`vector`, the mode-3 key degenerates to
**(allocation site, receiver CLASS)**. That is what produces the 100%
split-child figure above: a key that coarse fuses nearly every contour of a
site that shares a receiver class. **Read step 2's −667 accordingly** — it
is not evidence that a structural shape key works, it is what a very coarse
key merges, and `rdb` is the one program that noticed the merge was wrong.

**And the stopgap is not free.** Every content-carrying receiver shape in
the `rdb` trace is already declined `(unfilled)` — `5:list#61<4>` ×38,
`22:list#61<21>` ×20, and so on, where id 4 is `%`. A dict or user-class
receiver read through `cs->vars` would meet the same fate at mint time,
when `_keys`/`_vals` are empty and the fields have not arrived. The honest
expectation is that making the key content-aware converts most of those 74
reuses into declines rather than into finer merges — removing the bug by
removing the reuse, and taking much of the −667 with it. Measure it before
believing either number; do not present the result as a win or a
regression until the split between "declined" and "merged more precisely"
is counted.

**Retracted: the stopgap above is wrong, and it is wrong in the direction
that matters.**

Today a `dict` or user-class receiver produces a shape that can never
contain `%` — there is no content to be missing — so it always succeeds
and always merges. That is the 74-of-81. Give those receivers content and
they become `unfilled` at mint time, decline, and **mint**. It "fixes"
`rdb` by splitting more, on less knowledge. Numbers get worse, suite goes
green: the retreat CLAUDE.md names.

The reason is that **declining is not the neutral branch — it mints, and
the mint is permanent.**

- `creation_point` is memo-first (`fa.cc:512-516`, route `"cs_map"`), and
  `av->cs_map` deliberately SURVIVES every pass (`fa.cc:6937`: *"the
  analysis re-derives flow state from scratch each pass, but
  identity-carrying caches persist"*). So the decision is taken once, on
  the pass the contour is created — precisely the pass when the receiver's
  elements are empty by construction — and is never revisited.
- Which makes the guard's own justification false as written. *"Decline
  rather than guess; the next pass will have it"*: the next pass does have
  the shape, but nothing re-asks for that site. The memo answers first, and
  the shape only ever helps LATER contours.
- Under monotone identity both answers are permanent — a wrong merge cannot
  be unmade because the canon never revisits, and a wrong mint cannot be
  unmade because nothing re-merges CreationSets. The guard is choosing
  between two irreversible commitments and picks the one taken on **no
  evidence**.

**Minting on an unknown is a split caused by absence of evidence, which is
the exact inversion of demand splitting**, and it is not confined to the
`%` guard. `Class.dcpa = 1` (`python.py:159`): every shedskin class starts
with ONE data contour program-wide, and `dcpa` grows only through
`ifa_split_class`. pyc starts at one CS per (site × contour) — maximally
split — and the reuse routes claw back. So an unknown resolves toward
STAYING MERGED in shedskin and toward MORE SPLITTING in pyc. That is 128's
inversion showing up as a systematic bias rather than a local wart, and it
is the yardstick any change here has to be judged against.

**Correction to Part 1's claim that pyc cannot move a site onto an existing
contour.** It can, and does. `split_css` rewrites the memo across a whole
group:

```c
for (AVar *v : compatible_set) if (v) {
  assert(cs == v->cs_map->get(cs->sym));
  v->cs_map->put(cs->sym, new_cs);
}
```

and the [033](closed/033-splitter-non-idempotent-divergence.md) ledger
routes into an ALREADY-RECORDED CreationSet rather than minting
(`new_cs = d->cs_product`, counted as `cs_dup_split_attempts`). The
primitive exists and is exercised every pass. What does not exist is any
trigger that runs it in the MERGING direction — it only ever moves AVars
onto a split-off child. The gap is a policy gap, not a missing mechanism,
which makes step 4 cheaper than Part 1 implies.

**So the bounded form of step 4 that this needs** is not "re-derive
everything". It is: *invalidate exactly the `cs_map` entries whose decision
was taken under `unfilled`* — a set the code already identifies, since it
prints `[csshape-no]` for each. Record those AVars, clear those entries at
the start of the next pass, let `creation_point` re-decide against a filled
shape, and move the AVars with the machinery `split_css` already uses.
Bounded, targeted, per-pass invalidation — literally
[111](111-FA-selective-invalidation-per-pass.md)'s title.

The known hazard is stated in the same comment that makes `cs_map`
persistent: consumers hold positional slots into a CS (the issue-030
fixpoint in `make_closure_var` — *"a CS's positional vars[i] must be fed by
every pass that feeds the CS"*). Re-routing must preserve that, which is
the problem `split_css` already solves for the split direction.

**Measured: how much of the over-discrimination is ignorance.**

`IFA_DBG_DEMAND` now carries four more columns, recorded under
`PYC_CSELEM=3`. `unkmint` counts CreationSets minted while
`cselem_shape_key` declined with `unfilled` — splits taken on absence of
evidence. The other three are the counterfactual the guard itself cannot
ask, recomputed AFTER convergence when the shapes are known: `unkstill` is
how many are STILL unfilled at the end, `unkres` how many resolved to a
known shape, and `unkjoin` how many of those would now hit an existing
canon entry — i.e. would join an existing contour if the decision were
re-taken at convergence. `unkjoin` is an upper bound on "would have joined
at mint time": the entry it now matches may have been claimed after the
mint, so it answers "could this CS be retired by re-deciding?" rather than
"was a home already available?".

| program | container CS | unkmint | unkstill | unkres | unkjoin |
|---|---|---|---|---|---|
| kanoodle | 149 | 12 | 11 | 1 | 0 |
| quameon | 133 | 29 | 21 | 3 | 0 |
| rdb | 141 | 57 | 25 | 15 | 2 |
| chess | 87 | 69 | 19 | 48 | **20** |
| linalg | 128 | 90 | 44 | 28 | 6 |
| stereo | 192 | 4 | 3 | 0 | 0 |
| rubik | 126 | 6 | 2 | 2 | 0 |
| go | 38 | 3 | 0 | 1 | 1 |
| **total** | | **270** | **125** | **98** | **29** |

(`unkstill + unkres < unkmint`: the remaining 47 decline post-convergence
for a different reason — no `self` position, no receiver type, or the
depth-0 width cap.)

**Corpus-wide, and it cuts the other way.** The eight programs above
over-represent `chess`. Re-run as a full sweep
(`check__PYC_CSELEM_3__de9fca7d+adf4abe8`, verdicts byte-identical to the
uninstrumented run, so the census is behaviour-neutral):

| | total over 77 programs |
|---|---|
| container CreationSets | 3081 |
| `unkmint` — minted while the shape was unknown | 793 |
| `unkstill` — still unfilled at convergence | 402 (**51%** of unkmint) |
| `unkres` — resolved to a known shape | 304 |
| `unkjoin` — would join if re-decided at convergence | **36** (12% of resolved) |

64 of 77 programs mint at least one CS on an unknown, so the bias is
everywhere. But **`unkjoin` is 36 of 3081 container CreationSets — 1.2%**,
and seven programs account for all of it (`chess` 20, `linalg` 6, `rubik2`
5, `rdb` 2, `msp_ss` 1, `bh` 1, `go` 1). `chess` alone is 56% of the total;
its 23% figure is an outlier, not a preview.

**So the targeted `cs_map` invalidation proposed above is not worth
building.** Its entire reachable win is that 1.2%, it carries the issue-030
positional-slot hazard, and it leaves the 51% untouched by construction.
Recorded here so the next person does not re-derive the idea and spend a
week on it: the measurement was run specifically to size it, and the answer
is that re-deciding later is not where the over-discrimination lives.

What the numbers do support is the opposite reading. 793 CreationSets are
minted on no evidence; only 304 ever acquire the evidence, and of those
only 36 would have joined anything. The other 757 are contours that exist
because pyc's default is to split when it does not know — and for the 402
that never resolve, no evidence for a split ever arrives at all. Nothing
scheduled later fixes that. It is the default direction, which is step 4.

Two things follow, and the second is the more important.

**"Defer one pass and you would know" is false for most of them.** 125 of
270 — 46% — have a receiver whose shape is STILL unfilled when the analysis
has converged. The ignorance is not a timing artifact; for those sites the
element type never arrives at all. Re-deciding later cannot help them,
which bounds what the targeted `cs_map` invalidation above can recover.

**Where the shape does resolve, roughly a third would have joined.** 29 of
the 98 resolved cases hit an existing canon entry — CreationSets that exist
*solely* because the decision was forced before the evidence existed. It is
concentrated rather than spread: `chess` alone contributes 20, which is
**23% of its 87 container CreationSets**.

So the ledger on minting-on-unknown is: real, measurable, worth fixing, and
NOT mostly fixable by waiting. The 46% is the sharper form of the point —
for those receivers the analysis never learns anything, so no evidence for
a split ever arrives, and a demand-driven identity must keep them merged
rather than mint per contour. That is not a scheduling change; it is the
default-direction change, and it is step 4.

*Verify:* a reduced repro — two dicts with different key types in one
program, a container allocated inside a shared `dict` method — fails under
`PYC_CSELEM=3` and passes after the stopgap; rdb compiles; step 1's ratio
holds up; corpus `check` at `-e "PYC_CSELEM=3"` regains rdb without losing
the other three.

### Step 3 — implement the demand test and the ladder

> **Read 2c first: this step presupposes step 4, and items 1-5 are inert
> without it.**
>
> shedskin's demand test gates **splitting**: `len(csites) +
> len(emptycsites) == 1: continue` refuses to split a contour that only one
> site created. That is a test on an OBSERVED MERGE, and it presupposes
> contours that start merged — `Class.dcpa = 1`, one per class program-wide
> — so that "two sites in one contour" is an event there is something to
> notice.
>
> pyc has no such event. `creation_point` mints one CreationSet per
> (allocation site × contour) and stamps `cs->creation_var` with the single
> site that made it; at the default every one of the five reuse routes is
> inert (CLAUDE.md; [128](128-cs-identity-over-discriminates-vs-element-type.md)).
> So "one site feeds it" is true by construction almost everywhere, the gate
> fires everywhere, and it declines to split things that were never merged.
> Measured 2026-09-04: not "almost" — `multidef=0` over 127 522
> CreationSets on the whole corpus **at the default** (see this step's
> *Verify*). Under `PYC_CSELEM=3` it is 229, because that mode's csshape
> route does reuse; do not read the default's zero as a mode-3 baseline.
> **Implementing items 1-5 would not move step 1's ratio at all**, because
> the ratio is already fixed before any splitter runs — which is what step
> 1's reading of `stereo` says in as many words: *"The CS excess is not
> downstream of contour growth there — it is minted directly by
> `creation_point`, one per allocation site."*
>
> That leaves **item 6, the reuse index, as the only part of this step that
> can move the ratio** — and 2c measured its ceiling on the corpus. Of 793
> CreationSets minted while the receiver shape was unknown, 402 (51%) still
> have no shape when the analysis has converged, and only 36 would join an
> existing contour if the decision were re-taken there — **1.2% of the 3081
> container CreationSets.** An index keyed on deduced content cannot key on
> content that never arrives.
>
> So the honest sequencing is step 4 first: make contours start merged and
> merges revisable. Step 4's own entry is now unblocked — see the
> amendment there, which retracts its "wait for step 3" gate and finds
> both the re-pointing primitive and its invalidation closure already
> built. Only then does a demand test have merges to observe and
> a reuse index have content to key on. Built in the current order, items
> 1-5 will pass their verification and change nothing — which is the failure
> mode worth naming in advance, because a green suite would read as success.

The real work, and the step that makes the statement true. In FA, for
each container CreationSet and its element AVar:

1. Compute the creation sites feeding it — pyc has the backward edges
   (`av->backward`) that `backflow_path` corresponds to.
2. **If one site feeds it, never split.** This is the gate; it is the
   whole point.
3. If more than one, walk shedskin's ladder in order — no-confusion,
   confluence partition, path partition, wholesale — and stop at the
   first that separates the conflicting types.
4. Cap fan-out at 10 and refuse beyond it, as shedskin does.
5. Scope to `s->element` (the structural test, not names), so
   non-parametric classes stop being CS-split at all.
6. Build the **reuse index** the ladder's first route needs: contours keyed
   on their deduced element shape (`ifa_class_types` / `classes_nr`), so a
   site can be MOVED onto an existing CS instead of a new one being minted.
   Without it the ladder can only ever add contours and step 1's ratio
   cannot approach 1. Step 2's `PYC_CSELEM=3` key is the static
   approximation of exactly this.

Two things in shedskin's version are NOT worth copying: the
`random.shuffle`, and the accident by which routes 1 and 3 batch several
splits into one iteration while routes 2 and 4 return at once. Decide pyc's
batching deliberately.

Then `PYC_CSSPLIT` becomes the fallback rather than the driver: a CS
follows an ES split only where the demand test also asks for it.

*Verify, before writing any of it:* **done, 2026-09-04, and the answer is
zero.** `DEMAND` now carries `multidef` (container CreationSets whose
`cs->defs` holds more than one creation site) and `multidefall` (the same
over every CreationSet); `creation_point` does `cs->defs.set_add(v)` for
every site routed to a CS (`fa.cc:783`) and `clear_cs` empties it each
pass, so at convergence `defs` is exactly this pass's site set. Sweep
`compile__default__8cceaa08+2e452100`, 76 programs:

```
container_cs=3748  all_cs=127522  multidef=0  multidefall=0
```

Not one CreationSet in 127 522 has a second creation site. So item 2 —
"if one site feeds it, never split", the gate and *the whole point* —
would decline every split in the corpus, and items 1-5 cannot pay for
themselves. This is no longer an argument from `cs->creation_var` being a
single field; it is measured. **Do step 4 first.** Then, for the ladder
itself:
`tests/deepcopy_copy_of_copy_chain.py` and
`tests/set_difference*` (the `PYC_CSSPLIT` motivating cases) still pass;
corpus `check` neutral; ratio approaches 1; chess `ess` and compile time
move toward the `PYC_CSMOLD=1` numbers (666 / 8.1 s) **without** its
`mixed=2` failure.

*Stop condition:* if the ladder cannot separate a conflict that
structural splitting could, that is the monotonicity wall — do not add a
conservative "split anyway" rule. Record which case hit it and go to
step 4.

### Step 4 — re-decidable CS identity, i.e. [111](111-FA-selective-invalidation-per-pass.md)

> **Amended 2026-09-04 (read this before the plan below it).** The
> original text said pyc needs shedskin's `backup_network` /
> `restore_network` because it "has no snapshot", and told you not to
> start until step 3 had produced the list of cases needing it. Both are
> wrong, and correcting them makes this step both smaller and earlier
> than it was filed as. What follows is a code read, not a measurement —
> nothing new was run for it.

**pyc does not need a snapshot: every pass already re-derives from
bottom.** `analyze_to_convergence` resets before each pass, not after
(`fa.cc:10904-10909`) — at the default (`ifa_selective = 0`, `fa.cc:51`,
opt-in via `IFA_SELECTIVE`) that is an unconditional `clear_results()`
for every pass but the first. shedskin needs `backup_network` /
`restore_network` because it splits *speculatively inside* one iteration
and rolls back; pyc's pass structure hands it the same power for free.
Derived ATypes are not permanent here. **The decision is.**

**What actually survives a pass is documented in one place** —
`clear_results`'s header, `fa.cc:6953-6985` — and it lists five things.
Four are not obstacles:

| survivor | why it is not in the way |
| --- | --- |
| `av->container` | structural parenthood, not a decision |
| `av->type` / `av->ivar_offset` | written post-convergence, by clone |
| `av->match_cache` | keys on canonical AType pointers, so "a stale entry misses, never lies" |
| `av->num_coerce` | a coercion target set between passes (issue 025); orthogonal |

The fifth is the whole of step 4: **`av->cs_map`**. `creation_point` is
memo-first (`fa.cc:526-532`) and `cs_map` is never cleared, so the
site→CreationSet decision is taken once, on the first pass that reaches
the site, and never revisited — which is precisely the state
[2c](#step-2c--what-rdb-says-about-the-mode-3-key) measured: 793
CreationSets minted while the receiver shape was unknown, 402 of them
(51%) still unknown at convergence.

**And the reason given for pinning it is a *feeding* invariant, not a
*decision* invariant.** The comment says consumers hold positional slots
into these CSs — "a CS's positional `vars[i]` must be fed by every pass
that feeds the CS, regardless of which Var carries the value" (the
issue-030 fixpoint fix in `make_closure_var`). That forbids a CS quietly
losing a feeder. It does not forbid re-pointing a site at a different CS,
provided the CS it lands on is fed.

**The primitive that does exactly that is already built and shipping.**
`split_css` (`fa.cc:7691`) rewrites `v->cs_map->put(cs->sym, new_cs)`
across a `compatible_set` (`fa.cc:7775-7778`), and its `route` path
(`fa.cc:7762-7766`) moves the group onto an **existing** CreationSet
named by the durable ledger — `cs_group_signature` (`fa.cc:7603`) →
`FA::ledger_find_cs` (`fa.cc:5231`). So "move a site onto a contour that
already exists, across passes, keyed on stable IR" is not missing. What
is missing is a *caller that runs it in the joining direction*: today
`split_css` is the only caller of the re-point, and it only ever peels
groups off.

**The bounding mechanism is also already built** — this is ifa/111 M3.
`probe_invalidation_closure` (`fa.cc:9037`) computes, at end of pass, the
forward closure of every contour that pass changed;
`clear_results_selective` (`fa.cc:7156`) clears exactly that closure and
falls back to the full reset when it declines. A re-pointed CS is the
same *kind* of event as a split, so the same closure bounds it. Note the
direction: the closure was built for **splitting**, which narrows and is
the harder direction for a growing fixed point. Joining widens, which the
fixed point absorbs by construction.

So step 4 is not "the only step that is a genuine architecture change".
It is: **make the `cs_map` decision re-takable between passes, and route
the re-decision through machinery that exists.** 128 and 111 still
collapse into it.

*Entry criterion, replacing "wait for step 3":* step 2c already produced
the case list, and step 3's own preamble concludes the reverse ordering —
items 1-5 there are inert until this lands. The cases are (a) the 402
CreationSets that are still shape-unknown at convergence, and (b) `rdb`,
which no static key can fix, because the key is needed at mint time and
the content arrives passes later.

*First increment — **DONE 2026-09-05**.* Landed as
`cselem_rejoin_unknown_mints` (`fa.cc`, beside `capture_elem_keys`), gated
by `PYC_CSREJOIN` (default 1, but reachable only under `PYC_CSELEM=3`,
which is itself opt-in — so the pyc default is bit-identical, verified on
`chess`). `creation_point` is untouched.

It walks `cselem_unknown_mints` — the CreationSets minted while the
receiver shape was `unfilled` — recomputes each shape key now that the
shape has arrived, and where `cselem_shape_canon` already names a
different live CreationSet of the same class, does
`u.v->cs_map->put(u.s, other)`. **That is the first place in pyc where a
site→CreationSet decision is taken back.** Four guards, each borrowed
rather than invented: skip a site `split_css` has since moved (never
silently reverse a demand-driven split), require the canon target live in
`fa->css_set` (`split_css`'s own test), require `other->sym == u.s`, and
apply the `abstract_type` guard the csshape reuse route applies.

It runs **last** in the pass epilogue, only when `extend_analysis`,
`reanalyze` and `apply_unbound_fills` have all declined. The question
being answered is *"would this decision have been reusable had it
waited"*, so waiting is the point rather than a concession — and it is
also ifa/055's lesson about interleaving a decision change with the
splitter. Termination is structural: after the re-point `cs_map` names
the joined CS, so the `!= u.cs` test skips that entry forever, and the
whole batch is done in one call.

*Result.* Same-binary A/B over the corpus, `check` mode,
`check__PYC_CSELEM_3_PYC_CSREJOIN_0__7e05207f+037f2f8f` against
`check__PYC_CSELEM_3__7e05207f+037f2f8f`:

| | ess | css | container_cs | multidef | rejoin |
| --- | --- | --- | --- | --- | --- |
| rejoin off | 29878 | 120250 | 3081 | 229 | 0 |
| rejoin on | 29878 | 120229 | **3060** | 232 | **36** |

Corpus `ratio` 4.92 → 4.89, `pratio` 3.25 → 3.23. **Every verdict column
on all 77 rows is byte-identical** — `compile_rc`, `warns`, `run_rc`,
`cpy_rc`, `stdout_match` — and `ess` is unchanged on every program, so
nothing was paid for in EntrySet growth. Seven programs re-joined at all:
chess 20, linalg 6, rubik2 5, rdb 2, and one each on `bh`, `go`, `msp_ss`.

**`rejoin=36` is exactly the number 2c's counterfactual predicted.** That
is the most load-bearing thing here: the census was measuring the right
population, and the mechanism acted on all of it.

*Three things this does NOT show, stated so they are not read into it:*

1. **The `multidef` meter proposed above does not work, and the claim it
   rested on was wrong.** `multidef` was already 229 corpus-wide with the
   rejoin off — mode 3's own csshape reuse route produces those merges.
   The measured `multidef=0` was taken at the **default**, where mode 3 is
   off, and does not carry over to a mode-3 arm. The rejoin moved it 229 →
   232, because `multidef` counts *CreationSets with >1 site*, and most
   joins landed on a CS that already had two. The mechanism's evidence is
   `rejoin=36` with `container_cs` −21, `ess` flat and verdicts identical
   — not the +3.
2. **36 re-points removed only 21 CreationSets.** The other 15 abandoned
   CSs are still alive on another feeder: nothing removes them from
   `s->creators`, so the mold route (`PYC_CSMOLD` defaults to 3) can hand
   one to a sibling contour of the same Var. Not wrong — a sibling reusing
   an existing CS is the goal — but it means the join's effect on the
   count is partly given back, and `s->creators` hygiene is the first
   thing to look at next.
3. **`rdb` is unchanged** (`compile_rc=1`). Expected, and worth keeping on
   the record: rdb's fusion happens at mint time on a bare `dict#4799`
   receiver, which was never a minted-on-unknown case, so this increment
   could not reach it. `PYC_CSELEM=3` still cannot be the default.

*Where the precision went.* `linalg` lost one positional shape
(`pshapes` 19 → 18) with `container_cs` flat — a join widened one CS's
content until it coincided with another's. Corpus `pratio` still improved,
so this is a local cost, but a merge is a merge and it did show up.

*Stop condition — standing, and NOT hit by the first increment.* If a
re-pointed CS cannot converge — the join is undone by the next pass's
split and the two oscillate — that is the ledger's oscillation signal
(`cs_dup_split_attempts`, `rederive_churn`, `fa.cc:7794-7797`) and it is a
real answer, not a failure. Record the case. Do **not** answer it by
pinning the decision again. Measured on the first increment: no
oscillation, `compile_fail` stayed at 3, no new `rc=124`, and every
program that converged before still converged.

*Next increment — the mint side, and what it is actually made of.* The
first increment could only re-decide a mint AFTER the shape arrived, and
2c measured that 390 of 795 such mints never get a shape at all. Those are
unreachable that way; the only remedy is to stop minting on absence of
evidence. But `creation_point` cannot defer — it must return a
CreationSet — so "do not mint on an unknown" can only mean "return an
existing one", and the first question is whether an existing one was even
available.

Measured before touching anything (`mintwhy` on the `DEMAND` line, sweep
`compile__PYC_CSELEM_3__0ec1632e+c4905f52`, 76 programs, 795 unknown
mints):

| why it had to mint | count | share |
| --- | --- | --- |
| `nosite` — no CreationSet of this class exists for this site yet | 415 | 52% |
| `splitchild` — one exists; ifa/105's `mold == 3 && split_child` refused it | 380 | 48% |
| `cmc` — one exists; issue 045's `clone_methods_per_cs` refused it | 0 | — |
| `ineligible` — one exists; the mold is off or ineligible otherwise | 0 | — |

Two populations, and they need different answers. The zeroes are worth as
much as the counts: **ifa/105's split-child guard is the only thing that
ever blocks a same-site join here.** Issue 045's exclusion never fires on
this population, so it is not in the way.

**The 380 (48%): ifa/105's guard, and why relaxing it is the wrong move.**
The guard is not arbitrary — it measured a real failure: handing a split
CHILD the mold its parent owns puts both contours back on one container
instance, and on a chain of four nested `deepcopy`s the shared `r` came
back with element type `{list<itself>, int64, int64, list, int64}`, both
self-referential and container/scalar mixed. So this is exactly the case
CLAUDE.md warns about — the tempting move is to weaken the guard, the
numbers get better, and the deepcopy chain breaks.

But notice WHAT the guard protects: `PYC_CSSPLIT=1` (ifa/055), the rule
that *the CreationSet follows the EntrySet split*. That is splitting
driven by structure, which is the defect in the goal statement, not a
demand signal. ifa/105 is a conservative guard defending a structural
rule, and the fix is not to relax the guard but to make the rule it
defends demand-gated — which is step 3's own closing sentence: "`PYC_CSSPLIT`
becomes the fallback rather than the driver: a CS follows an ES split only
where the demand test also asks for it."

So the increment is: **give the split child its parent's CreationSet
(start merged) and split it back when the shapes diverge.** That is the
goal statement verbatim, and the first increment establishes that the
re-decision half is mechanically available. What it needs that the first
increment did not is the SPLITTING direction — `split_css` is the
primitive and ifa/111 M3's closure is the bound, but unlike joining it
narrows, so it is the direction the fixed point does not absorb for free.
`tests/deepcopy_copy_of_copy_chain.py` is the acceptance test, and
ifa/105's element type is the exact thing that must not come back.

**The 415 (52%): nothing exists at that site, and that is not ignorance.**
A fresh allocation site genuinely needs a CreationSet, so minting is the
right answer — *given the per-site key*. `cselem_shape_key` composes
`v%d|name#id|shape`, keyed on `v->var->id` by deliberate design ("this
canonicalizes the contours of ONE allocation site, it does not fuse
unrelated sites"). Drop the `v%d` and a same-shape CS at a DIFFERENT site
becomes reusable — which is shedskin's posture exactly (`Class.dcpa = 1`,
one contour per class program-wide, split on demand) and the only route
that can move ifa/128's complaint, since that complaint is about sites:
95 `list` CSs standing for 6 element types are 95 different sites. It is
also the larger semantic change of the two, and it should not be attempted
before the split-back direction above works, for the same reason ifa/105
exists.

> **RETRACTED 2026-09-05 by the measurement in "Both paths were built and
> both were measured" below.** The recommendation in this subsection was
> made from a STATIC counterfactual on the canon's key space, and the
> dynamic result is the opposite: dropping the site component makes the
> CreationSet count WORSE on 4 of 5 programs. The numbers below are still
> correct about what they measure — they just do not predict the outcome,
> and inferring one from the other was the error. Left in place because
> the 660 ≈ 626 agreement remains the best evidence in this issue for what
> the demand-driven contour count actually is.

**Which of the two is the straighter path to the goal: the 415 (drop the
site component), and it is not close.** Measured with a counterfactual on
the canon itself — strip the leading `v<id>|` from every claimed key and
count what is left (sweep `compile__PYC_CSELEM_3__fc25f947+ca955581`, 76
programs):

```
container_cs   3060
canon          1670   distinct site|class|shape triples claimed
canonsiteless   660   distinct class|shape pairs   -> 2.53x site fragmentation
shapes          626   (the independent content census, element_census)
```

Three things follow.

1. **The site component fragments contour identity 2.53×**, and that is a
   floor, not a symptom: with `v<id>` in the key the fewest contours the
   canon can ever name is the number of distinct (site, class, shape)
   triples. Minimal creation sets is unreachable while it is there. The
   380 are 12% of 3060 — realized in full they move `ratio` 4.89 → ~4.3,
   and cannot go further.
2. **`canonsiteless` (660) lands within 5% of `shapes` (626).** Those are
   two independent computations of "how many distinct contents does this
   program have" — one from the canon's own keys, one from
   `element_census` walking every CS's channels. Their agreement is the
   strongest evidence in this issue that ~626-660 is the demand-driven
   contour count and 3060 is the over-discrimination
   ([128](128-cs-identity-over-discriminates-vs-element-type.md)'s claim,
   arrived at from a different direction).
3. **`v<id>` is provenance.** Contour identity must key on types and CS
   partitioning, never on where a value came from — the same rule that
   keeps display state and marks out of it. `v->var->id` is exactly
   where-it-came-from, so the per-site key is that defect, not a tuning
   knob.

The 380 also stop being their own project once this lands: mode 3's
csshape route has **no split-child guard** (ifa/105's guard is on the
*mold* route, `fa.cc:726`), so a split child whose shape is known and
matching already reuses today. The 380 are blocked by ignorance, not by
that guard being right, and a wider canon makes more of them findable
later — exactly the way the first increment's 36 became findable.

What does not change: the split-back direction is still required, and
more so, because cross-site merging is a bigger merge than joining a
split child to its parent. `tests/deepcopy_copy_of_copy_chain.py` and
ifa/105's `{list<itself>, int64, int64, list, int64}` remain the
acceptance test, and `pshapes` (948 today) is the meter for precision
given back.

*Also outstanding from the first increment:* `s->creators` hygiene — 36
re-points removed only 21 CreationSets because the abandoned CS stays in
`s->creators` and the mold route can hand it to a sibling contour.

*Verify:* `PYC_CSMOLD=1` becomes safe — `deepcopy_copy_of_copy_chain`
converges with `mixed=0` — `PYC_CSELEM=3` stops regressing `rdb` and can
become the default, and chess reaches shedskin's ~7 s.


### Step 4 — both paths were built, and both were measured

Two flags, both **default off**, so mode 3's recorded baseline is unchanged:

- `PYC_CSRESPLIT=1` — `cselem_resplit_diverged`, the split-back. Partitions
  the defs of a container CreationSet by their current shape key and moves
  every non-home group off, preferring to JOIN the canon's existing owner
  for that shape over minting a new CS. A def whose key is unknown does not
  move and does not vote — separating on an unknown is the mint-side error
  running the other way.
- `PYC_CSSITELESS=1` — drops the `v<id>|` prefix from `cselem_shape_key`,
  making the canon key `class#id|shape`.

They are armed together on purpose: the site-free key merges contours of
unrelated allocation sites, and that merge is only defensible if it can be
taken back.

**Result: the site-free key makes the CreationSet count worse, on 4 of 5
programs.**

| program | mode 3 `container_cs` / `ess` / passes | `+CSSITELESS` |
| --- | --- | --- |
| chess | 75 / 985 / 14 | **103** / 1168 / 36 |
| quameon | 133 / 1125 / 55 | **148** / 1223 / 64 |
| rdb | 139 / 1083 / 22 | **143** / — / 35 |
| rubik2 | 43 / 412 / 12 | **44** / 446 / 20 |
| sieve | 20 / 249 / 7 | 14 / 237 / 10 |

`sieve`, the only program that improved, is also the smallest.

**Root cause, and it is not subtle.** chess under `PYC_DBG_OSC`:

```
mode 3                 final_pass=14  violations=0    ess=985   css=4126
mode 3 + CSSITELESS    final_pass=36  violations=183  ess=1168  css=4785
```

Merging containers from unrelated sites widens their element types until
use sites see a type violation. The splitter then splits EntrySets to
recover the precision — and because CreationSet identity is
*(allocation site × contour)*, **every EntrySet split mints fresh
CreationSets**. The merge is paid for twice: once in the contour it saved,
and again in the contours the induced ES splitting created. Net worse.

**What this says about the static counterfactual.** `canon` 1670 against
`canonsiteless` 660 is a true measurement of the identity space the key can
*name*. It is not a prediction of the CS count, because widening the key
CAUSES ES splitting that mints CSs the key never names. Reading a dynamic
outcome off a static property was the error, and it is worth naming: the
2.53× is real and the conclusion drawn from it was not.

**And the split-back never fires** — `resplit=0` on every program but
`quameon`, which moved one def. That is the second finding, and it is the
more useful one: the distinction that gets observed here does **not**
appear as *"the defs of this CreationSet have divergent shape keys"*. It
appears as a type violation at a use site, which the existing splitter
already acts on. So shape-key divergence among defs is the wrong demand
signal; the right one is the signal the splitter already has.

**Where that leaves the plan.** Both mint-side paths run into the same
wall, and it is [128](128-cs-identity-over-discriminates-vs-element-type.md)'s,
not either path's: while CS identity is per *(site × contour)*, any
mechanism that merges contours pays for itself in ES splits, and any
mechanism that splits EntrySets multiplies CreationSets. Widening the CS
key cannot get ahead of that, because widening is what provokes the
splitting. The 660 ≈ 626 agreement still says ~626-660 contours is the
demand-driven answer; it just is not reachable from the key.

So the next thing to attack is the *(site × contour)* product itself —
`creation_point` minting one CS per contour of a site — rather than the
key it is looked up by. That is ifa/128 as filed, and it is upstream of
everything in this step.

*Kept, not reverted:* both flags stay, default off, with this result
recorded at their definitions. They are the reproduction of the
experiment; deleting them would lose the ability to re-run it, and the
split-back is still the mechanism a future merge will need — with a
different demand signal.


### Step 4 — start merged: one CreationSet per sym (`PYC_CSDCPA1`)

The architecture the goal statement describes, stated by the author
2026-09-05 and now implemented behind a flag:

> CreationSets should only be demand split. Pass 1 should have 1 creation
> set per class/sym. Then they split by setter value (type). ESs are split
> as necessary to separate the creation points so the CS can split. Pure
> demand CS splitting. Minimal final creation sets.

This inverts today's dependency, and that inversion is the point. Today
`creation_point` mints one CS per *(allocation site × contour)*, so an
EntrySet split MULTIPLIES CreationSets as a side effect — CS identity is
decided by structure before any demand test runs. Under the flag nothing
structural makes a contour: an ES split exists to separate creation points
so that a CS split *becomes possible*.

`creation_point` gains one route, placed before the split-parent, cselem
and mold routes — those all answer "which of this site's contours should
this be", a question that does not arise when a sym has one contour. It
takes the first creator, the unsplit root (`new CreationSet(cs)` appends,
so a split never displaces it); a site that belongs with a split child
arrives on the root and is moved off by `split_css`.

`PYC_CSDCPA1=1` is every sym; `=2` excludes `tuple`; default 0.

**It works, on the numbers, and by a lot.** Against the DEFAULT (not mode
3):

| | `css` | `ess` | `container_cs` | `ratio` |
| --- | --- | --- | --- | --- |
| chess | 6433 → **3077** | 1591 → **967** | 169 → **66** | 14.08 → **5.50** |
| rubik2 | 1511 → **1037** | 474 → 455 | 61 → **45** | 7.62 → **6.43** |
| sieve | 1003 → **746** | 249 → 236 | 20 → **11** | 4.00 → **2.75** |

chess: container CreationSets −61%, EntrySets −39%, total CreationSets
−52%. **`ess` goes DOWN**, which is the signature of the inversion working
— every key-side experiment in this issue drove it up (see the retracted
`PYC_CSSITELESS` result, where chess went 985 → 1168).

**What it costs, and how it decomposes.** `./test_pyc.py`, 311 tests:

| | failed |
| --- | --- |
| default | 0 |
| `PYC_CSDCPA1=1` | 49 |
| `PYC_CSDCPA1=2` (tuples excluded) | 30 |

*19 of the 49 are tuple arity and position*, and excluding `tuple` is not
a concession to make a suite pass. A tuple's arity is observable (`len`,
unpacking, indexing) and its content is per-INDEX in `cs->vars` with the
element channel deliberately left bottom
([104](closed/104-unify-list-and-tuple-in-analysis.md)), so arity and
position are part of the tuple's TYPE. Setter-equivalence splitting cannot
express either, so one contour per `tuple` sym merges positional slots that
no demand test can separate again. Tuples need arity in CS identity, or a
splitter that splits on it; they do not need a per-site contour.

The remaining 30 are **14 EXEC, 9 COMPILE-OUT, 7 COMPILE** — and the kinds
matter more than the count. All four `splitter_*` tests are in the
COMPILE-OUT group, and `splitter_setter` **compiles clean and prints the
right answer**: it fails only because its `.check` asserts
`STAGES: TYPE_CONFL SETTER`, a mechanism assertion. Under one contour per
sym the `{A, B}` receiver resolved by poly dispatch and no split was
needed. So part of that group is "the mechanism did not have to fire",
not "the answer is wrong". The 14 EXEC failures are the ones that are
genuinely wrong output, and they are the real bill.

~~**What is missing is the third clause.**~~ **RETRACTED — measured
2026-09-06, and the third clause is already working.** The claim below was
that "ESs are split as necessary to separate the creation points so the CS
can split" is not implemented and is what the EXEC failures need. Two
independent measurements say otherwise; see *Step 4 — the third clause is
not what is missing* at the end of this file. It was already half-retracted
by item 1 of "What has to land" (the `PYC_ESFORCS` route measured inert);
what is new is WHY, which makes the retraction complete.

Original text, kept for the record: *today ES splitting runs on its own
criteria and `split_css` splits only what the setter confluences already
separate. Nothing asks "this CreationSet needs to split, but its creation
points share a contour — split that contour so it can". That is the
mechanism the 14 EXEC failures are missing, and it is the next thing to
build.*

**Corpus `check`, and it is nowhere near flippable yet.**
`check__PYC_CSDCPA1_2__3c388f22+adf4abe8` against the default baseline
`check__default__a935532b+adf4abe8`:

| | default | `PYC_CSDCPA1=2` |
| --- | --- | --- |
| container CS / shapes | 3748 / 626 = **5.99** | 2540 / 570 = **4.46** |
| `pratio` | 3.92 | **2.89** |
| compile_fail | 2 | **20** |
| run_fail | 39 | 33 |
| stdout_differs | 24 | 17 |

The contour win is real and corpus-wide: **−32% container CreationSets**,
ratio 5.99 → 4.46, `pratio` 3.92 → 2.89. Everything else is a
regression, and `run_fail` / `stdout_differs` falling is NOT an
improvement — those columns shrank because 18 fewer programs got far
enough to run.

The 18 new compile failures are not one kind:

- 13 ordinary `rc=1` diagnostics — chess, chull, dijkstra, kanoodle,
  life, linalg, path_tracing, pygmy, rdb, richards, solitaire, sudoku2,
  sudoku3, webserver.
- **2 segfaults in `pyc` itself** — `plcfrs` and `softrender`, `rc=139`.
  A crash under a flag is still a latent bug and has to be root-caused
  whatever happens to this architecture.
- **2 new timeouts** — `pygasus` and `quameon` at the 400 s cap, plus
  `othello3` going `rc=1` → `rc=124`. Passes rise sharply everywhere
  (chess 32 → 45, rubik2 12 → 39, sieve 7 → 17): with one contour per sym
  the splitter has more to re-derive and takes more passes to settle.

**What has to land before this can be turned on**, in the order the
evidence puts them:

1. **~~The third clause~~ — CONSTANTS in the demand test.** *(Revised
   2026-09-05, after building the third clause and measuring it inert.)*

   The author's observation — *"if the setters have a confluence it is
   precisely the answer"* — is correct **and already implemented.**
   `collect_cs_setter_confluences` collects confluences that sit on a
   CreationSet field (`!av->contour_is_entry_set` is its guard), and the
   ladder acts on them through the mark-setter rung. Under
   `PYC_CSDCPA1=2`, `tests/splitter_setter.py` reports
   `STAGES: TYPE_CONFL SETTER MARK_SETTER` against the default's
   `TYPE_CONFL SETTER` — the splitter fires and escalates exactly one
   rung, and the program prints the right answer. That test fails only
   because its golden pins the stage list.

   A `PYC_ESFORCS` route that split the setters' EntrySets directly from a
   CS-field confluence was built and measured: **283/30 with it and
   without it**, and all 64 targets it found were declined by
   `split_entry_set`. Reverted rather than kept — it is dead code, and
   the rung that does the work already exists.

   The real EXEC bill is a different mechanism, root-caused on
   `tests/empty_list_print.py`. `b = [2, 3]` and `k = []` share one `list`
   CreationSet under the flag, and `k` prints `[0, 0]` instead of `[]`.
   Both are `list<int>` — there is **no type distinction to observe**. The
   distinction is a *constant*: length 2 against length 0. At the default
   `TYPE_CONFL` separates them, but only because per-site CS identity had
   already put the constants in different contours; under the flag only
   `SETTER` fires and it does not separate them.

   The reason is that **constants are stripped from contour identity** —
   the ES split key uses the constant-stripped view of argument types
   (`fa.cc:5675`) — *except* for classes explicitly marked
   `clone_methods_per_cs`, which pyc sets only when a ctor param carries
   `__pyc_clone_constants__` (`python_ifa_build_syms.cc:2855`, issues
   040/045). `list` is not marked.

   So the item is: **a constant difference at a creation point is an
   observed distinction and must drive a split**, generally, rather than
   only for classes opted in by the frontend. Per-site CS identity has
   been hiding this: it separated constants structurally, so nothing ever
   had to ask for it by demand.

   It cannot be answered per CLASS, because one class covers both cases —
   some `list` and `tuple` values have a statically known arity and some
   do not, and demanding a constant from a loop-built list fans out a
   contour per iteration count. Planned as
   [131](131-demand-driven-constant-splitting.md), whose demand signal is
   the constant cap-strip itself (`fa.cc:1022`): when a strip is about to
   lose constants that distinct creation points disagree on, split
   instead; when they come from one creation point, strip as today, which
   is exactly the unknown-arity case identifying itself.
2. **The two `pyc` segfaults.** Independent of the rest.
3. **Tuple arity in CreationSet identity**, so `=1` stops costing 19
   tests and the flag stops needing a tuple exception.
4. **Convergence cost.** Two new timeouts and passes up 40-200%.
   [111](111-FA-selective-invalidation-per-pass.md)'s selective
   invalidation (`IFA_SELECTIVE`, still default 0) is the existing lever
   and has never had a reason to pay for itself; this is that reason.
5. **The `splitter_*` goldens — MEASURED 2026-09-05, and they cost
   nothing.** The author's requirement is *"we should get direct calls if
   at all possible"*, so the question is not which stages fired but
   whether any call became dynamic. Measured on the emitted C, default
   against `PYC_CSDCPA1=2`:

   | test | lines | direct calls | tag compares |
   | --- | --- | --- | --- |
   | `splitter_setter` | 52 → 52 | 3 → 3 | 0 → 0 |
   | `splitter_setter_of_setter` | 655 → **589** | 69 → **71** | 4 → 4 |
   | `splitter_mark_type` | 561 → **517** | 67 → 67 | 0 → 0 |
   | `splitter_cartesian_product` | 561 → **517** | 67 → 67 | 0 → 0 |

   > **CORRECTION.** The table above counts *tag compares* in the emitted
   > C, and that proxy is incomplete: pyc also emits polymorphic calls
   > through a **method-pointer slot**, which carries no tag compare. The
   > conclusion drawn from it — "no call became dynamic" — is wrong for
   > `splitter_cartesian_product`. See the proper observable below.

   The goldens pin a **route**, and the outcome needed measuring
   separately.

   **The tests now assert the outcome, and it is not a proxy.**
   `report_call_resolution` (`codegen_common.cc`, `PYC_DBG_CALLS=1`)
   emits `CALLS: direct=N dynamic=M` over every live call site with
   candidates: `direct` is one resolved target, `dynamic` is a call
   through a method-pointer slot. All four `splitter_*` tests assert it
   alongside their `STAGES:` line.

   It is counted from `get_target_fun_core` and **not** in either emitter,
   because the two backends decide with different predicates — `cg.cc`
   asks `get_target_fun`, `cg_emit_llvm.cc` asks `callees->n > 1` — and a
   golden is shared by both. Verified: the C and LLVM backends now report
   identical numbers, and both suites are 311/0.

   What that shows under `PYC_CSDCPA1=2`:

   | test | `STAGES` | `CALLS` |
   | --- | --- | --- |
   | `splitter_setter` | changed | **54/0 → 54/0**, unchanged |
   | `splitter_mark_type` | changed | **69/0 → 69/0**, unchanged |
   | `splitter_setter_of_setter` | changed | 58/2 → 56/2 — two sites gone, none dynamic |
   | `splitter_cartesian_product` | changed | 68/1 → **65/4** — three calls became DYNAMIC |

   So three of the four are pure route changes and re-blessable when the
   flag flips. **`splitter_cartesian_product` is a real regression** and
   must not be re-blessed — it is a `.known_issue` until the splitter
   keeps those three calls direct.

   *Deferred deliberately, because it is a symptom.* Every dynamic call in
   both configurations is `list.append` — at the default one site with
   three candidate clones, under the flag four sites with two each — i.e.
   `append` clones that will not collapse on a shared `list`
   CreationSet. That is the same cause as `list_append_is_amortized` and
   `list_pop_insert`, which fail with `mixed basic types: (int64 str)`:
   two lists of different element type sharing one contour. Those are the
   HARD form (no representation exists); this is the SOFT form (it
   compiles and runs, it just cannot pick one clone). Scoped as
   [133](133-split-a-container-on-its-element-type.md) — fix that and
   re-measure; only if this survives is it a defect of its own.

   *Two traps worth recording.* `IFA_DBG_DISPATCHFAIL`'s `sites=` is not
   this metric either: it reads 0 on `list_tuple_union_method` and
   `poly_dispatch_shared_method_extra_args`, both of which genuinely have
   union receivers, because it counts dispatch *failures*. And counting
   tag compares in the emitted C misses slot-dispatch entirely, which is
   how the regression above was missed the first time.

   So when the flag flips these four are re-blessable, but **not with a
   blanket `--rebless`**: `splitter_mark_type` loses its `MARK_TYPE`
   firing and `splitter_cartesian_product` loses `CPA`, which retires the
   only coverage those two stages have. That is the
   `mark_distance_skew` situation CLAUDE.md names, with one difference —
   here the outcome is verified equal, so it is a coverage loss rather
   than a baked-in regression. Each needs a replacement assertion before
   its golden moves.

   The stage list stays as secondary coverage — it is still the only
   thing pinning that `MARK_TYPE` and `CPA` fire at all — but it is no
   longer the assertion that matters.

*Not built, deliberately:* nothing here weakens the flag to make the suite
pass. The flag is off by default, the default path is untouched
(`make test` rc=0), and 30 failures is the honest state of an
architecture with one of its three clauses missing.


### Step 4 — the bill under `PYC_CSDCPA1=2`, triaged 2026-09-05

16 suite failures, and they are not 16 problems:

| group | n | tests |
| --- | --- | --- |
| **[133](133-split-a-container-on-its-element-type.md)** — element union with no representation | **5 → 4** | `builtins`, `plcfrs_grammar_tables_nonconvergence`, `list_append_is_amortized` (`mixed basic types: (int64 str)`), and `splitter_cartesian_product` (the soft form — compiles, loses 3 direct calls). **`list_pop_insert` FIXED 2026-09-05** — `__delitem__`'s `merge_in(self, [])` was a false constraint, not contour sharing |
| **goldens, already decided** — route changed, `CALLS` verified equal | 3 | `splitter_setter`, `splitter_setter_of_setter`, `splitter_mark_type` |
| **layout / member width** — `blind-cast … but member width differs` | 2 | `poly_dispatch_shared_method_extra_args`, `sibling_subclass_field_layout` |
| **illegal call argument type** | 2 | `deepcopy_copy_of_copy_chain`, `listcomp_element_separation` |
| **undiagnosed** | 4 | `match_seq_star` (`expression has no type`), `test_heapq`, `iterator_protocol_bridge`, `itertools_count_forloop` (the last three produce no diagnostic — they fail later) |

**133 is the single biggest lever at 5 of 16**, and it is the core demand
question besides. Do it first.

The **layout pair is a sibling of
[132](132-arity-is-representation-not-provenance.md)** and should be read
that way: 132 found that *arity* is a representation property CreationSet
identity must respect; member width is another one. It is also the exact
failure CLAUDE.md's "be aggressive" section warns about — two clones of a
class disagreeing on a slot — so the answer is to name the mechanism
producing the disagreement, not to re-add a per-site contour.

`PYC_CSDCPA1=1` (tuples included) is 19 against `=2`'s 16, so the tuple
exception is three tests from being deletable.

**On `PYC_CSELEM=3`: it is superseded, not a second default to flip.**
It gives −18% container CreationSets and regresses `rdb`; `PYC_CSDCPA1=2`
gives −27.5% and needs no key evaluated at mint time. There is no reason
to carry both to the default.

## What this unblocks

The goal statement, and with it: [128](128-cs-identity-over-discriminates-vs-element-type.md)'s
16× CS excess, [111](111-FA-selective-invalidation-per-pass.md)'s 7.4×
analysis-time gap and its 48×-per-compile work growth, and the emitted
code size (chess: 2.1 MB / 108k lines of C against shedskin's 904 lines).

### Step 4 — the third clause is not what is missing (measured 2026-09-06)

An attempt to construct an example that *requires* "ESs are split as
necessary to separate the creation points so the CS can split" failed:
every shape built for it is already handled. The creation point is a
single `[]` inside a function in all four, and the two calls disagree on
element type; `PYC_CSDCPA1=2`, C backend:

| | one creation point, reached via | result | `STAGES` |
| --- | --- | --- | --- |
| A | `return []` from a **no-argument** function | clean | `TYPE_CONFL SETTER` |
| B | `self.items = []` in `__init__` | clean | `+ SETTER_OF_SETTER` |
| C | `b.items = []` — a field of a **parameter** | clean | `+ SETTER_OF_SETTER` |
| D | C, and filled inside the callee | clean | `+ SETTER_OF_SETTER` |

A is the pure form of the clause: no argument, so CPA has nothing to split
on, and the split happens anyway because the return value is a legal
`split_ess_for_type` target. C and D are the long chain — the distinction
travels `p`/`q` → `Box`'s CreationSet → `attach`'s receiver contour → the
`[]` inside it — and it arrives. **The ladder already splits the EntrySet
so that the creation point duplicates and the CreationSet can split.** It
just does not announce itself under that name; it is `SETTER` and
`SETTER_OF_SETTER`.

**This explains the `PYC_ESFORCS` result** recorded in item 1 above — 64
targets found, *all* declined by `split_entry_set`, 283/30 with and
without. That read as the route being wrong. The reading now is that the
ES splits it was asking for had already been made, so the declines were
correct. Two measurements, taken for different reasons, agreeing.

**What the failures actually need is the opposite shape.** Reduced from
`tests/list_append_is_amortized.py`, and now
[133](133-split-a-container-on-its-element-type.md)'s reproducer:

```python
a = []
a.append(1)
s = []
s.append("x")
print(a[0], s[0])
```

Two creation points that are **already distinct AVars in one EntrySet**.
`IFA_DBG_MIXELEM` shows the value path fully split — `__setitem__` es=47
carrying `int64`, es=51 carrying `str` — both writing into one element
because the receiver CreationSet is one. There is nothing left to split on
the EntrySet side; the CreationSet must partition its own `defs`. That is
133 option 3, already the plan.

*One shape genuinely cannot be split, and it is out of scope.* A single
`inner = []` inside a loop whose iterations disagree fails on **both**
arms (default `int64 str`, flag `list int64 str`) because loop iterations
are not contours, so no ES split exists to make. Pre-existing limitation,
not a flag regression; related to
[131](131-demand-driven-constant-splitting.md)'s unknown-arity case.

*Caveat, stated so this is not over-read:* four constructed cases are not
proof the clause is never needed. A longer chain, or one the setter ladder
cannot reach, may exist. But the burden is now on producing such a case
before building the mechanism, rather than on justifying not building it.

### Step 4 — the flag arm re-measured on `88c773c9` (2026-09-06)

`check__PYC_CSDCPA1_2__88c773c9+f3cb2d55` against the previously recorded
`check__PYC_CSDCPA1_2__3c388f22+adf4abe8`:

| | recorded | current |
| --- | --- | --- |
| compile_fail | 20 | **16** |
| `pyc` segfaults (`rc=139`) | 2 — `plcfrs`, `softrender` | **0** |
| compile timeouts (`rc=124`) | 3 — `pygasus`, `quameon`, `othello3` | **0** |
| run_fail / stdout_differs | 33 / 17 | 34 / 20 |
| container CS / ratio | 2540 / 4.46 | 2764 / 4.68 |

**Every one of the 16 compile failures is now a plain `rc=1` diagnostic.**
Two of the four things "What has to land" listed are therefore done:
item 2 (the two `pyc` segfaults) and the compile half of item 4
(convergence cost — no program now exceeds the 400 s cap). Eight programs
newly compile: `chess`, `dijkstra`, `life`, `pygasus`, `softrender`,
`solitaire`, `sudoku2`, `webserver`.

**Read the run columns with ifa/102 in mind.** Several of those eight now
abort at runtime (`rc=134`: `chess`, `life`, `sudoku2`, `pygasus`,
`softrender`; `bh` goes 0 → 134). `run_fail` 33 → 34 and
`stdout_differs` 17 → 20 are largely programs that got far enough to fail
later. Compiling is not evidence.

**Four programs newly FAIL to compile** — `msp_ss`, `othello2`, `pisang`,
`sha` — and this is the open regression in the window. **Not caused by
either 2026-09-06 change**, established two ways rather than assumed:
`sha` fails identically with the pre-fix `__pyc__/04_sequence.py`, and all
four fail with `PYC_CSDEFSPLIT=0` (the A/B below). Note also that the old
arm's key is `3c388f22+adf4abe8` — a DIRTY tree — so that comparison was
never purely commit-to-commit, and the uncommitted work it carried is
gone. Root-cause before flipping.

#### What CS_DEF_PARTITION itself costs and buys, isolated

Same tree, same flag, `PYC_CSDEFSPLIT=0` vs `1`
(`check__PYC_CSDCPA1_2_PYC_CSDEFSPLIT_0__88c773c9+f3cb2d55`):

| | off | on |
| --- | --- | --- |
| compile_fail | 16 | 16 |
| run_fail / stdout_differs | 35 / 20 | **34** / 20 |
| with_warnings | 39 | **38** |
| container CS / shapes | 2708 / 591 | 2764 / 591 |

Exactly two programs move:

```
fysphun   run 134 -> 0    stdout -> yes    (aborted; now runs, output CORRECT)
rubik2    run 134 -> 124  (abort became a timeout -- ambiguous, not a win)
```

So on the corpus the stage is worth one program made correct, for
**+56 container CreationSets (+2.1%)**. That is the precision-for-contours
trade stated plainly: this stage SPLITS, so it moves the goal ratio the
wrong way (4.58 → 4.68) and is worth it only because the contours it adds
are ones a demand test asked for. The suite is where it pays — 16 → 11 —
and the corpus says it does not pay for itself twice.

*Correction to the headline table:* the previously recorded
`2716 / 595 = 4.56` for the flag arm is superseded; the current tree is
`2764 / 591 = 4.68`, and only +56 of the difference is this stage.

### Step 4 — the 4 new compile failures, bisected (2026-09-06)

`msp_ss`, `othello2`, `pisang`, `sha` fail to compile under
`PYC_CSDCPA1=2` on `88c773c9` and did not on the recorded
`3c388f22+adf4abe8` arm. Chased, because that arm was measured on a DIRTY
tree and the regression might not have been against a real commit.

**It is real.** All three of `sha`, `othello2`, `pisang` compile cleanly
at a CLEAN `3c388f22`. Bisected on `sha` (4 s to fail), clean build at
every step:

| commit | `sha` under the flag |
| --- | --- |
| `2978653c` ifa/131 step 1 | **rc=0** |
| `5e012d78` ifa/132 first half — `no_static_arity` in `make_kind` | **rc=134**, `pyc` aborts |
| `6e8ede1d` ifa/132 second half — arity in CreationSet identity | **rc=1**, `'x' has mixed basic types:( int64 str )` |
| … through `88c773c9` | rc=1, same diagnostic |

**So the cause is [132](132-arity-is-representation-not-provenance.md),
and it was recorded as such when it landed.** `5e012d78`'s own message
says *"47 compile failures under the flag is a PRECISION problem. Tuning
the guard would be the retreat… Recorded, not done."* The second half took
47 → 16; these three are what it did not recover. `'x'` is `list.pop`'s
local (`x = self[i]`), and the mechanism is 132's own second half —
`no_static_arity` forces list layout, which reads through the generic
element, so each per-index var is flowed into it and a container merging
an int creation point with a str one gets `{int64, str}`.

`msp_ss` is a different failure and not this: *"a variable holding
{str, list} has no representation: `__add__` resolved to the CONTAINER
method"* (issues/018).

**`CS_DEF_PARTITION` cannot rescue them, for two independent reasons —
both measured, neither a reason to weaken it.**

1. **It is starved.** On `sha`, `TYPE_CONFL` returns progress on 19 of 27
   passes, so the `!analyze_again` gate that makes this the last rung
   never opens. `IFA_DBG_CSDEFSPLIT` prints **nothing at all** — the
   stage never sees the CreationSet. This is the hazard the
   `CSM_ELEMENT_CS` placement comment documents, running the other way:
   the gate protects finer stages from preemption, and on a program where
   stage 1 keeps finding real work it also makes the last rung
   unreachable.
2. **The cap.** The offending `cs=1054` carries `defs=18` at pass 1, over
   `kCsDefSplitMax` (10, shedskin's route-4 cap).

Worth stating what the first one is NOT: `TYPE_CONFL` is doing **real
work** on `sha` (`d_ess=66, 32, 27, 13…`), not making hollow claims of
progress. That distinction was only visible after fixing the meter —
see below.

*Options, not implemented, because the choice is a design one.* An
irrepresentable element union is a correctness condition, not a precision
one: deferring to a finer stage is right when the alternative is a
coarser split, but wrong when the alternative is refusing to compile the
program. Letting `CS_DEF_PARTITION` run when the confluence has no
representation — rather than only on quiescence — is the principled
version. Raising the cap is the other half. Neither should be done as
tuning; both change what "last rung" means.

#### `PYC_DBG_STAGEDELTA`'s `d_ess`/`d_css` were structurally zero

Found while doing the above, and it inverted a conclusion mid-analysis, so
it is worth its own note.

`ess0`/`css0` were snapshotted from `fa->ess` / `fa->css`, which are
rebuilt only by `collect_results` (`fa.cc:4830`) — once per pass, BEFORE
splitting. Within `run_split_stages` they are frozen, so before == after
always. **Measured, not reasoned:** `CS_DEF_PARTITION` minted five
CreationSets on 133's reproducer and the probe still printed `d_css=0`.

Fixed to compute the deltas from `all_entry_sets` / `all_creation_sets`,
which the EntrySet and CreationSet constructors append to, so they move
the moment a stage creates a contour. The probe now reads
`CS_DEF_PARTITION returned=1 d_ess=0 d_css=5` on that reproducer.

**Any conclusion drawn from a pre-2026-09-06 `d_ess`/`d_css` reading is
void.** In this session it read as "TYPE_CONFL claims progress 19 times
and moves nothing", which is exactly ifa/055's signature and exactly
wrong: with the meter working, `d_ess=66` on pass 0.

*The sidecar was deliberately NOT changed.* `all_*` never shrinks, so it
is an allocation count, not a live population — 12 live vs 115 allocated
on one `fa-converge` fixture. Reporting it in `record_fa_event` would
change what all 15 `fa-converge` goldens MEAN rather than correct them,
which is a separate design decision. Those goldens' `ess=A→B` is a
within-pass tautology and worth revisiting on its own terms.

### Step 4 — flag arm after ifa/133's per-candidate gate (2026-09-06)

`check__PYC_CSDCPA1_2__c81a7b1a+f3cb2d55` against
`check__PYC_CSDCPA1_2__88c773c9+f3cb2d55` (same flag, one change apart):

| | before | after |
| --- | --- | --- |
| compile_fail | 16 | **12** |
| run_fail / stdout_differs | 34 / 20 | 35 / 23 |
| with_warnings | 38 | 37 |
| container CS / shapes | 2764 / 591 = 4.68 | 2955 / 604 = **4.89** |
| `pratio` | 3.04 | 3.20 |

**Six compile failures fixed** — `chull`, `msp_ss`, `pisang`, `sha`,
`sudoku3`, `sudoku5` — and one of them, **`msp_ss`, is a complete win**:
it compiles, runs, and its stdout MATCHES CPython. Two more programs stop
aborting at runtime (`ant`, `sudoku2` go `rc=134` → `rc=0`), though both
still print the wrong answer.

**Two new compile failures, and they are this change's:**

- **`chaos` 0 → `rc=124`**, a 400 s compile timeout at `passes=37`. A
  convergence blowup: more splits, more passes. This is
  [111](111-FA-selective-invalidation-per-pass.md)'s `IFA_SELECTIVE`
  earning its keep, and the first corpus program to time out purely from
  splitting the analysis asked for.
- **`pystone` 0 → `rc=1`**, and its diagnostic is self-contradicting:

  ```
  fail: a variable holding 'int64' has no representation: '__add__'
  resolved to the CONTAINER method, whose receiver may be a scalar.
  ```

  A variable holding ONE type cannot lack a representation. This is not a
  `{container, scalar}` union at all — it is `__add__` resolving to the
  container method for a receiver that is only `int64`, i.e. a
  CALL-RESOLUTION defect wearing a representation message. It is the same
  diagnostic family as `linalg` and `msp_ss`, and `msp_ss` was FIXED by
  this change, which is direct evidence for CLAUDE.md's rule that boxing
  is never the answer for a corpus program: the message blamed
  representation and the actual defect was inference. **The message itself
  should be fixed** — it names a single type while asserting a union.

**And the contour count moved the wrong way: +191 (+6.9%), ratio 4.68 →
4.89.** That is the honest cost. This stage SPLITS, so every case it fixes
is bought with contours, and the goal number (626 shapes) recedes. Six
compile failures for 191 CreationSets is a defensible trade only because
the alternative was refusing to compile the programs at all — but it is
not progress toward minimal creation sets, and should not be reported as
such.

**And the reconciliation is known: build shedskin's finer routes.**
Wholesale is route 4, the LAST rung, and it is expensive by design —
route 4 gives every creation point its own contour where routes 1-3 would
have separated the same conflict into two. Those routes need per-type
attribution, which this issue and
[133](133-split-a-container-on-its-element-type.md) had recorded as "not
computable". **That was overstated, corrected 2026-09-06:** shedskin
computes it in `backflow_path` (`infer.py:2031`), and pyc's
`IFA_DBG_ATTRIB` is a different algorithm — different start node, no
`(class, contour)` filter on the hops, and no grouping of the incoming
edges by assigned type, which is what routes 1 and 3 key on. Its `0 of 6`
result says nothing about whether the real walk works. See 133's
correction. **The +191 is the price of skipping rungs that were never
built, not the price of demand splitting.**

*Harness note:* the first attempt at this sweep was OOM-killed at the
default `-j 32`. Re-run at `-j 8 -J 4` with a memory monitor: **no memory
warnings at all**, so the cause was concurrency, not a per-process
blowup. Nothing partial was cached — checked before re-running, since a
killed sweep can leave a result that looks real.
