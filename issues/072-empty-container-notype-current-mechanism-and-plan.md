# 072 — Empty/imprecise-container element inference (the 043 family): shedskin comparison + backward-pass design

**Status:** design + **negative implementation result** (2026-07-28).
Re-diagnoses the [043](closed/043-empty-container-inference-options.md)
family, compares pyc to shedskin (same base algorithm), designs a
backward pass, and **implements its seeding half behind
`--empty_elem_split` (default OFF)** — which measured as a net negative
(see "Implementation result" below), re-confirming 043's option-4
finding with concrete numbers. The flag stays off; the design section is
kept for the real fix (backward flow from use sites, not a fixed seed).

## Implementation result (2026-07-28) — read this before extending

The seeding half of the design landed behind `--empty_elem_split`
(default off; a true no-op when off — suite 234/0 both backends).
`fa_seed_empty_container_elements()` (fa.cc), called from the frontend
`reanalyze()` hook, seeds `nil` into the element AVar of every live
container CreationSet whose generic element is bottom AND whose
positional slots carry no value (the `[]`/`{}`/never-appended shape —
the positional-slot check is required to avoid poisoning a `[1,2,3]`
literal, whose generic element is *also* bottom at quiescence because
its elements sit in `cs->vars`, not yet flowed to the generic element).

**It is a net NEGATIVE when on:**
- Suite: regresses `str_join`, `set_*`, `logical_operators`, … — empty
  containers flowing into `str.join` / concat / set-ops. Those ops
  already handle a **bottom** element correctly (join of `[]` → `""`);
  seeding `nil` makes the element a real `None` that the unboxed typed
  primitive (`_CG_strcat`, set-add) then rejects.
- Corpus sweep: **COMPILED 23 → 19, FAIL 25 → 31** (net −4 to −6).

**Root cause of the negative result:** pyc is **unboxed**. shedskin's
identical `empty → <class>[nil]` seed is harmless only because shedskin
boxes everything (a `nil` object joins to `""` like any other). In pyc a
fixed default element poisons every typed op the empty container flows
into. A fixed seed is therefore the *wrong lever* — the element type
must come **backward from the use site** (str for `join`, int for
arithmetic; nil only when *nothing* constrains it), which this forward
seed cannot do. `void` as the default fared worse (doesn't resolve the
read and still poisons). So the entangling blocker is pyc's unboxed
representation, not the seeding logic — the same wall issue 018/030
names. `git grep fa_seed_empty_container_elements` for the code; extend
it by adding use-site backward constraint collection (below), or delete
it if that path is abandoned.

**Affects:** `ifa/analysis/fa.cc` — the CreationSet split machinery
(`split_css`, `creation_point`, `get_element_avar`, `run_split_stages`,
the `AVar::backward`/`setters` edges).
**Related:** [040](closed/040-empty-list-shared-clone-type-inference.md),
[052](052-shared-method-branch-reopens-empty-list-fragility.md),
[045](closed/045-receiver-cs-method-cloning.md) (the existing per-CS
lever), [063](063-no-type-bucket-triage.md) (the corpus "no type"
bucket), [018](../../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-polymorphic-dispatch-fat-pointers.md) (heterogeneous boxing —
the co-blocker for amaze/dijkstra2), [061](061-c-backend-multi-tuple-list-null-element-type.md)
(the `(null)*` C-backend sibling).

## Correction: chess.py:314 was NOT this family (mis-attributed here earlier)

An earlier draft of this file used chess.py:314's
`if not [i for i in pseudoLegalCaptures(...) ...]:` as this family's
"cheap witness," with a mechanism story about a shared `object.__not__`
branch NOTYPE-ing over an empty list. **That was wrong.** The real cause
was a plain dispatch gap: containers don't derive from `object` (builtin
classes are exempt from the implicit `object` base), and
`__pyc_any_type__` — their actual root — had **no `__not__` at all**, so
`not <list/tuple/dict/set/str>` dispatched to nothing for *every*
container, **empty or not** (`not [1,2,3]` failed identically to
`not []`). Fixed by adding `__not__` to `__pyc_any_type__` (`8644be59`,
`tests/not_container.py`). This has nothing to do with element inference;
it is deleted from this issue's scope. The lesson (again, per 043's own
history) is that "no type near a possibly-empty container" is easy to
mis-attribute — verify the failure reproduces with a **non-empty**
container before blaming element inference.

## What this family actually is (the real corpus cases)

A container's element type is not inferred at its allocation site, so
element reads elsewhere are NOTYPE. Grounded manifestations:

- **The `retval = []` filled-later shape** — `pseudoLegalCaptures`-style
  functions that allocate `[]`, `append` in a loop, and return it; a
  *polymorphic or multi-site* read of the result reads a bottom element.
- **rubik** (`rubik.py:86` `self.struc[x][y]`) — nested-container element
  bottom: `struc` is a list-of-lists whose inner element type isn't
  attributed to the inner allocation.
- **amaze** (`(tuple __pyc_None_type__ int64 float64 str)`) — element is a
  genuinely **heterogeneous** union; even with element typing solved, the
  representation needs boxing ([018](../../issues/018-dict-mixed-key-types-boxing-failure.md) /
  [030](030-polymorphic-dispatch-fat-pointers.md)) — a *separate*
  co-blocker, not solved by element inference alone.
- **dijkstra2** — dict/heap element cross-product (063's canary).

## How shedskin solves it (the design north star)

shedskin's inference is literally **Plevyak's IFA** (its own docstring)
— the same algorithm pyc is built on — plus Agesen's CPA. Its
`shedskin/infer.py` runs, after each forward convergence, a **backward
phase** `ifa()` → `ifa_flow_graph()`:

1. **`backflow_path()`** (infer.py:2031) — the backward trace. From each
   *assignment target* (a site that writes a concrete element type into
   the container), walk incoming (`in_`) edges **backward**, following
   only edges where the container type flows, collecting the path back to
   allocation points (`alloc = [n for n in path if not n.in_]`). This
   attributes "element type X is written into containers allocated at
   sites S."
2. **`emptycsites = allcsites - csites`** (infer.py:1766) — the key move:
   allocation sites that flow to **no** assignment are the never-written
   containers, identified as a first-class set.
3. **`ifa_split_no_confusion` / `ifa_split_class`** — partition allocation
   sites by the assignment-set (element type) reaching them, giving each
   partition its own class-duplicate (`dcpa`). Empty sites are grouped in
   the "no confusion" set and **split off into their own contour** — an
   empty `[]` never shares a contour with a written `[int]`.
4. **`ifa_seed_template`** distributes the deduced element types across the
   newly-split allocation points and re-runs the forward phase.

## pyc vs shedskin (architecture map)

| Concept | shedskin | pyc |
|---|---|---|
| allocation contour | `dcpa` (class duplicate) | **CreationSet** (`creation_point`) |
| function duplicate | `cpa` (CPA) | **EntrySet** |
| container element var | class type-var `var` | `get_element_avar(cs)` |
| backward edge | `node.in_` | **`AVar::backward`** (`fa.cc:376`) |
| assignment set | `assignsets` (writes into a slot) | **`AVar::setters`** (append/setitem/merge) |
| split a contour | `ifa_split_class` | **`split_css`** |
| CS-split trigger | **proactive** backward pass every round; empty sites separated as `emptycsites` | **reactive**, violation-driven; empty sites are invisible (no setter → not a "starter") |
| empty container | first-class, split + seeded `→ <class>[nil]` | no concept; empty element stays **bottom** |
| representation | everything boxed (object ptrs) — unions free | scalars unboxed → heterogeneous unions need boxing (018/030) |

**pyc already has every building block** — `CreationSet`, `AVar::backward`,
`AVar::setters`, `get_element_avar`, `split_css`. The two missing pieces
are exactly shedskin's `emptycsites` step and its default seeding.

## Design: a backward element-split pass for pyc

Add a new stage to `run_split_stages`, run **on quiescence** of the
violation-driven stages (same slot as `PER_CS_RECEIVER`, so it never
perturbs their trajectories). Call it `split_element_imprecise_css`.
It is a focused extension of `split_css`, NOT a from-scratch tracer.

**Step 1 — collect element-imprecise container CSs.** Scan `fa->css` for
`cs` where `cs->sym->element` (it is a container) and either:
(a) `get_element_avar(cs)->out->type` is bottom (never-written), or
(b) `cs->defs` mixes allocation sites some of which are element-written
    and some not (an empty and a written `[]` sharing one CS).

**Step 2 — backward-partition `cs->defs` by reaching element-writes.**
For each container def AVar in `cs->defs`, determine which element-write
setters reach it, by walking `AVar::backward` from the element AVar's
`setters` (the shedskin `backflow_path`, but pyc already has the reverse
edges, so it is a bounded BFS over `backward`). Partition:
  - **written sites**, keyed by the element type(s) that reach them
    (data polymorphism — same as `split_css`'s setter-equivalence key);
  - **empty sites** = defs reached by **no** element write (shedskin's
    `emptycsites`).

**Step 3 — split.** Mint a new CS per partition via the existing
`split_css` machinery (reusing the issue-033/066 `cs_group_signature`
ledger for cross-pass identity — mandatory, or this oscillates). Empty
sites get their own CS, distinct from any written sibling.

**Step 4 — seed the empty CS's element with a DEFAULT** (`nil`) rather
than leaving it bottom — the analog of shedskin's `empty → <class>[nil]`.
This is the crux: a concrete default lets every downstream read and
shared-method branch over the empty container type-check and concretize
(as `list[nil]`, a never-used buffer — runtime-invisible). Seed via
`update_gen(get_element_avar(empty_cs), nil_type)`.

**Step 5 — return `analyze_again = 1`** to drive another forward round;
converge as usual.

### Why this is monotone / fixpoint-safe (the hard part)

shedskin restarts the whole analysis (`restore_network` + re-run) each
ifa round; pyc's fixpoint is incremental, so the pass must only **widen**:
- Seeding `nil` into an empty element only adds a type (never removes) →
  monotone.
- The split partitions existing defs; it mints no new element types, so
  re-running finds the same partition (idempotent) — provided the ledger
  routes a re-derived group back to its first CS (exactly `split_css`'s
  existing issue-066 discipline; reuse it verbatim).
- Running only on quiescence keeps it out of the violation stages'
  trajectories (the PER_CS_RECEIVER precedent, 045).

### Relationship to prior attempts (why this differs)

- 043's **option 4** (confluence seeding) was prototyped and "had nothing
  to do" because it seeded where empty *meets* non-empty at a flow-
  connected confluence — the case union flow already handles. This design
  seeds at the **allocation site** after a **backward** attribution and an
  explicit **empty-site split**, so it covers the empty site that is read
  *without* meeting a sibling (through a shared method, a never-written
  field, a polymorphic multi-site read) — 043's actual failing shapes.
- It generalizes **045** (`clone_methods_per_cs`, which hard-splits per
  *constant* for list/range) to the empty-vs-written distinction for
  *every* container class (dict/set/tuple/str), driven by writes rather
  than constants.

### What it does NOT solve

- **Heterogeneous element representation** (amaze's `tuple|None|int|float|
  str`) — element *typing* is only half; the unboxed-scalar union still
  needs boxing (018/030). This pass makes the element *typed*; a separate
  effort makes it *representable*.
- The **`(null)*` C-backend codegen** for a nil/bottom element
  ([061](061-c-backend-multi-tuple-list-null-element-type.md)) — seeding
  `nil` should sidestep most of it, but the C emitter's null-element path
  should still be hardened to trap rather than emit `(null)*`
  (043 option 1 / the 056/063 convention).

## Implementation order

1. **Prereq trace** — instrument `split_css` on rubik / the `retval=[]`
   multi-site-read repro: confirm the empty/imprecise CS is (a) reachable
   as a split candidate and (b) currently left with a bottom element.
   This validates Steps 1–2 against real data before writing the split.
2. **Step 1–4 behind a flag** (`--empty-elem-split`), measured on the
   determinism gate + full corpus sweep + `test_pyc.py` both backends.
3. **Ledger integration** (Step 3) — reuse `cs_group_signature`; verify
   no new `cs_dup_split` oscillation on dijkstra2/fysphun (063 canaries).
4. Flip default on once the sweep shows net-positive with zero suite
   regressions.

## Verification targets

1. The `retval = []` multi-site-read repro (a `[]`-filled function return
   read polymorphically) — clean, both backends.
2. `b=[2,3];print(b);k=[];print(k)` (040) and the 052 no-op-branch repro
   — still clean.
3. rubik past `struc[x][y]`; dijkstra2 past its dict/heap wall (element
   half — boxing may still block until 018/030).
4. `test_pyc.py` 234/0 both backends; determinism gate + sweep buckets
   net-positive, no regressions; dijkstra2/fysphun pass-count unchanged.
