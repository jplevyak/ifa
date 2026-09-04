# 129 — plan: make CreationSet splitting demand-driven

**Status:** open, filed 2026-09-04. A plan, with an audit of how shedskin
does it. Root cause is [128](128-cs-identity-over-discriminates-vs-element-type.md);
the architectural dependency is [111](111-FA-selective-invalidation-per-pass.md).
**Goal:** make CLAUDE.md's first line true — *the primary purpose of IFA
is the demand splitting of Creation Sets.*

## Part 1 — audit: how shedskin does it

All in `shedskin/infer.py`. The demand splitter is `ifa()` (line 1469),
called once per iteration after `propagate()`.

### The demand test

`ifa_split_vars` (1498) walks, for each candidate class contour `dcpa`,
the class's **type variables** (`cl.tvar_names()` — for `list` that is
its element). For each, `ifa_flow_graph` builds:

| | meaning |
|---|---|
| `assignsets` | incoming edges grouped by their merged type — *which distinct types are assigned into this type variable* |
| `paths` | `backflow_path` from each assign set — the nodes that contribute to it |
| `creation_points` | per assign set, the allocation sites (`[n for n in path if not n.in_]`) |
| `csites` | every allocation site reaching this contour |
| `emptycsites` | sites for this `(cl, dcpa)` that reach no assignment |

Then, the gate:

```python
if len(csites) + len(emptycsites) == 1:
    continue          # one creation site -> nothing to split, ever
```

**That is the whole idea.** A data contour is a candidate for splitting
only when two or more allocation sites have merged into it. Splitting is
a response to an observed merge, never a consequence of structure.

### The escalation ladder

When there IS more than one site, shedskin tries progressively coarser
splits and stops at the first that works:

1. **`ifa_split_no_confusion`** — when the variable holds >1 simple type
   and there are >1 assign sets, split so no contour is left with a
   confused (mixed) type.
2. **Confluence-point split** — for a node with >1 incoming edge where
   some creation site appears in more than one creation-point set
   (`ifa_confluence_point`), partition the sites by incoming edge
   (`ifa_determine_split`, which also subtracts overlaps so the
   partition is disjoint). Take it only if `2 <= len(remaining) < 10`.
3. **Partition csites across paths** — group sites by the set of types
   on their paths; if that yields >1 group, split one group off.
4. **Wholesale** — `len(paths) > 1 and 1 < len(csites) < 10`: give every
   site its own contour.

**pyc's default is shedskin's step 4, applied unconditionally and with
no cap.** That one sentence is the gap.

### Three further constraints worth copying

- **Scope.** `ifa_classes_to_split` considers ONLY parametric builtin
  containers — `list, tuple, tuple2, dict, frozendict, defaultdict, set,
  frozenset, deque, __iter, array`. User classes are never data-contour
  split. (shedskin matches these by identifier; pyc's structural
  equivalent already exists and is already used in `creation_point`:
  `s->element`.)
- **Fan-out cap.** Both the confluence and wholesale routes refuse at
  10. Splitting is bounded by construction.
- **One decision per iteration.** `ifa()` returns on the FIRST class
  that yields a split; the network is then reset and re-derived. Splits
  are never batched.

(`ifa_classes_to_split` ends with `random.shuffle(classes)` — the source
of the 38/38/39 iteration non-determinism measured in
[111](111-FA-selective-invalidation-per-pass.md).)

### What makes it safe there and not here

shedskin can start merged and separate later because `restore_network`
discards derived types every iteration. pyc's ATypes are monotone: an
over-merge that produces a mixed or self-referential element type is
permanent, and the splitter cannot walk it back — measured in
[128](128-cs-identity-over-discriminates-vs-element-type.md) as
`PYC_CSMOLD=1` giving `mixed=2` and `pass_limit_hit=1` on
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
