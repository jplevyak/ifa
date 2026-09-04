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

### Step 1 — make the demand ratio a reported number

`IFA_DBG_ELEMTYPE` already prints `N CS / M elemtypes / K shapes` per
container sym. Promote it: report `sum(CS)` vs `sum(shapes)` corpus-wide
as a single ratio, and record it in `sweeps/INDEX.md` alongside
`ess`/`css`. Today chess is 95/6 for `list`, 69/2 for `tuple`; the
corpus figure in `creation_point`'s own comment is 1994 CS for 341
shapes. **Nothing below should be judged on `ess` alone — this ratio is
the thing the goal statement is about.**

*Verify:* the number exists, on the corpus, before and after each step.

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
