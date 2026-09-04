# 129 — plan: make CreationSet splitting demand-driven

**Status:** open, filed 2026-09-04. A plan, with an audit of how shedskin
does it. Root cause is [128](128-cs-identity-over-discriminates-vs-element-type.md);
the architectural dependency is [111](111-FA-selective-invalidation-per-pass.md).
**Goal:** make CLAUDE.md's first line true — *the primary purpose of IFA
is the demand splitting of Creation Sets.*

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
`-e "PYC_CSELEM=3"` sweep is now runnable and is still owed.

### Step 3 — implement the demand test and the ladder

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

*Verify:* `tests/deepcopy_copy_of_copy_chain.py` and
`tests/set_difference*` (the `PYC_CSSPLIT` motivating cases) still pass;
corpus `check` neutral; ratio approaches 1; chess `ess` and compile time
move toward the `PYC_CSMOLD=1` numbers (666 / 8.1 s) **without** its
`mixed=2` failure.

*Stop condition:* if the ladder cannot separate a conflict that
structural splitting could, that is the monotonicity wall — do not add a
conservative "split anyway" rule. Record which case hit it and go to
step 4.

### Step 4 — reversibility, i.e. [111](111-FA-selective-invalidation-per-pass.md)

Aggressive sharing (`PYC_CSMOLD=1`, shedskin's actual default posture)
is unreachable while merges are permanent. Making it reachable means
being able to discard derived ATypes and re-derive them against a
decision table — `backup_network`/`restore_network` plus `alloc_info`.
pyc already has the table shape (`var_elem_key`); it has no snapshot.

This is the only step that is a genuine architecture change, and 128 and
111 collapse into it. Do not start it before step 3 has produced the
list of cases that actually need it.

*Verify:* `PYC_CSMOLD=1` becomes safe — `deepcopy_copy_of_copy_chain`
converges with `mixed=0` — and chess reaches shedskin's ~7 s.

## What this unblocks

The goal statement, and with it: [128](128-cs-identity-over-discriminates-vs-element-type.md)'s
16× CS excess, [111](111-FA-selective-invalidation-per-pass.md)'s 7.4×
analysis-time gap and its 48×-per-compile work growth, and the emitted
code size (chess: 2.1 MB / 108k lines of C against shedskin's 904 lines).
