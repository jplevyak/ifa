# 002 — Eager entry-set splitting and setter equivalence classes

Three `#if 0` blocks in `fa.cc` and one supporting class
(`SettersClasses`) sketched an alternative analysis mode in which
entry sets are split eagerly on initial argument types and setter
equivalence classes are tracked as first-class objects. The author
left the breadcrumbs but the lazy/on-demand splitter we ship today
was judged a better tradeoff for the benchmarks of the time.

## Where the idea came from

The default splitter widens an entry set lazily: an edge enters an
existing set, gets union'd into the type lattice, and the partition
is refined only when a precision violation is detected downstream.
This minimizes the number of entry sets but can mask cases where an
earlier split would have given a tighter result.

The eager variant proposes: when an edge first enters an entry set,
*if the initial argument types differ from every existing member*,
make a new entry set. Pair that with a canonical representation of
"which setter AVars are equivalent under the current partition" so
the same eager test can be applied to setter classes.

## What was there

Pre-removal sketch (deleted June 2026):

### Block 1 — `initial_compatibility` (`fa.cc` ~870)

```cpp
#if 0
static int
initial_compatibility(AEdge *e, EntrySet *es) {
  for (AEdge *ee : es->edges) if (ee)
    for (MPosition *p : e->match->fun->positional_arg_positions)
      if (e->initial_types.get(p) != ee->initial_types.get(p))
        return 0;
  return 1;
}
#endif
```

A pairwise predicate: "do `e` and every existing edge in `es` agree
on initial types at every positional argument?". Intended as an
additional gate inside `entry_set_compatibility` (or as a
replacement for the type lattice check there). Hard equality on
initial types is strictly tighter than the lattice union test.

### Block 2 — `setters_classes_cannonicalize` (`fa.cc` ~3348)

```cpp
#if 0
// Eventual Optimization
static SettersClasses *
setters_classes_cannonicalize(SettersClasses *s) {
  assert(!s->sorted.n);
  for (Setters *x : *s) if (x) s->sorted.add(x);
  if (s->sorted.n > 1)
    qsort_pointers((void**)&s->sorted[0], (void**)s->sorted.end());
  uint h = 0;
  for (int i = 0; i < s->sorted.n; i++)
    h = (uint)s->sorted[i] * open_hash_primes[i % 256];
  s->hash = h ? h : h + 1;
  SettersClasses *ss = cannonical_setters_classes.put(s);
  if (!ss) ss = s;
  return ss;
}
#endif
```

Parallel to `setters_cannonicalize`: hash-cons a `SettersClasses`
(a `Vec<Setters *>` — a *set of setter sets*) into a global table so
equality between equivalence classes becomes pointer-equality. The
supporting machinery — the `SettersClasses` class, the
`cannonical_setters_classes` ChainHash, the `SettersClassesHashFns`,
and the `Setters::eq_classes` back-pointer — was all built and
maintained even though only the lazy code path consumed it. All of
that infrastructure was removed alongside the `#if 0` block.

The pre-removal shape of the supporting infrastructure, for
reference if reviving:

```cpp
// fa.h (forward decls)
class SettersClasses;
class SettersHashFns;

// fa.h — the class itself
class SettersClasses : public Vec<Setters *> {
 public:
  uint hash;
  Vec<Setters *> sorted;
};

// fa.h — Setters carried a back-pointer to its containing class
class Setters : public Vec<AVar *> {
 public:
  uint hash;
  Vec<AVar *> sorted;
  SettersClasses *eq_classes;   // <-- removed
  Map<AVar *, Setters *> add_map;

  Setters() : hash(0), eq_classes(nullptr) {}   // eq_classes init removed
};

// fa.h — hash funs paralleling SettersHashFns
class SettersClassesHashFns {
 public:
  static uint hash(SettersClasses *a) { return a->hash; }
  static int equal(SettersClasses *a, SettersClasses *b) {
    if (a->sorted.n != b->sorted.n) return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i]) return 0;
    return 1;
  }
};

// fa.cc — the canonical table + its reset hook
static ChainHash<SettersClasses *, SettersClassesHashFns>
    cannonical_setters_classes;
// inside fa_reset():
cannonical_setters_classes.clear();
```

The `Setters::eq_classes` field was never read by the live code
path — it was the slot the dormant `setters_classes_cannonicalize`
would have written into so that `same_eq_classes` could short-cut
to pointer equality instead of the current set-disjunction walk
(`fa.cc same_eq_classes`).

#### Why a "set of setter sets" in the first place

The terminology is dense; the structure is layered:

| Object              | Is a                          | Meaning                                                            |
|---------------------|-------------------------------|--------------------------------------------------------------------|
| Setter site         | AVar                          | A program point that writes to some variable.                      |
| `Setters`           | `Vec<AVar *>` (set of AVars)  | The set of setter sites that may write to some variable, **or** the equivalence class an AVar has been grouped into. |
| `AVar::setter_class` | `Setters *`                   | The equivalence class (a `Setters`) this AVar currently belongs to. |
| `SettersClasses`    | `Vec<Setters *>` (set of those equivalence classes) | The whole partition — for one analysis context, the set of all equivalence classes in play. |
| `Setters::eq_classes` | `SettersClasses *`           | Back-pointer from an equivalence class to the partition it belongs to. |

The live analysis already constructs the partition: `compute_setters`
builds a `Vec<Setters *>` and `recompute_eq_classes` assigns
`AVar::setter_class` so each AVar knows its class. What it does
*not* do today is name or cache the partition itself; the
partition exists only transiently as a `Vec<Setters *>` on the
stack.

`SettersClasses` was meant to be that name. Once the partition is
hash-consed, two analysis contexts (entry sets) with the same
partition share the same `SettersClasses *`, and each class in the
partition has an `eq_classes` back-pointer to it.

#### What the splitter would have used it for

The splitter compares setter behavior across call sites and AVars
to decide whether two pieces of analysis state can stay merged or
must be split. Today every such check goes through
`same_eq_classes(Setters *s, Setters *ss)`:

```cpp
// fa.cc — current live code (paraphrased)
static bool same_eq_classes(Setters *s, Setters *ss) {
  if (s == ss) return true;
  if (!s || !ss) return false;
  Vec<Setters *> sc1, sc2;
  for (AVar *av : *s)  if (av) sc1.set_add(av->setter_class);
  for (AVar *av : *ss) if (av) sc2.set_add(av->setter_class);
  return !sc1.some_disjunction(sc2);
}
```

That is, "do these two Setters lie in the same partition?" is
answered by walking both, collecting the `setter_class` of every
member, and testing the collections for disjunction. O(n·m) on the
size of the input Setters and called from at least four splitter
hot paths:

- `entry_set_compatibility` (via the AVar-level checks at the
  call sites in `fa.cc` around the historical lines 760, 762).
- `compute_setters` itself, in the SETTER branch — recursively
  bootstrapping the partition from the previous pass's classes.
- The "collect setter confluences" pass that follows
  (`fa.cc collect_setter_confluences`), which decides where the
  partition has to refine.
- The two split-decision sites later in the file (around the
  historical 3561 / 3626 / 3637 lines) that compare setter
  behavior of two AVars or two setter representatives.

With `SettersClasses` in place, every one of those calls would
collapse to a single pointer comparison
(`s->eq_classes == ss->eq_classes`), at the cost of maintaining
the back-pointers in `compute_setters` /
`recompute_eq_classes` / `split_eq_class`.

#### How it connects to eager splitting

The other two `#if 0` blocks (initial-type compatibility and the
"group" pass) make sense only if you can compare partitions
cheaply:

- The "group" block (Block 3) wants to *merge* a candidate AVar
  into an existing `Setters` group when their setter behavior
  matches. That match test is `same_eq_classes` — fast only if
  partitions are pointer-comparable.
- The eager-splitting mode the audit alludes to wants to split an
  entry set the moment a new edge's setter-class partition differs
  from the existing members'. Same need: partition equality has to
  be O(1) or the splitter walks dominate the analysis.

So the three `#if 0` blocks plus the `SettersClasses` machinery are
one coherent feature, not three independent sketches: hash-cons the
setter partition, give each class a back-pointer to it, then use
pointer-equality to drive eager merge/split decisions in
`compute_setters` and `entry_set_compatibility`.

### Block 3 — "group" pass in `compute_setters` (`fa.cc` ~3450)

```cpp
#if 0
// group
for (int i = 0; i < ss.n; i++) {
  for (AVar *a : *ss[i]) if (a) {
    if ((akind == AKIND_TYPE &&
         (!a->out->type->n || !x->out->type->n || a->out->type == x->out->type)) ||
        (akind == AKIND_SETTER && same_eq_classes(a->setters, x->setters)) ||
        (akind == AKIND_MARK && !different_marked_args(x, a, 1)))
    {
      ss[i]->set_add(x);
      goto Ldone;
    }
  }
}
#endif
ss.add(new Setters);
ss[ss.n - 1]->set_add(x);
```

Inside `compute_setters`, this would have merged a candidate AVar
into an existing `Setters` group instead of starting a fresh one,
whenever the group's representative was compatible under the
current `akind` (TYPE / SETTER / MARK). With the block disabled,
every AVar gets its own singleton group — fine for correctness, but
forces more downstream merging via `recompute_eq_classes`.

## Why it was dormant

The original comment ("Eventual Optimization") and the
[IFA.md §11.6](../IFA.md) note explain it: this was a planned
eager-splitting mode that "was deemed not worth it for the
benchmarks at the time". Lazy splitting hits a smaller working set
in the canonical-type tables; the eager path doubles the entry-set
count on programs where the per-call initial types vary widely but
the eventual lattice union doesn't gain precision from splitting.

## What reviving it would require

1. Decide which variant to enable — eager initial-type splitting
   (block 1), grouped setter classes (blocks 2+3), or both.
2. Re-introduce the `SettersClasses` infrastructure (the class, the
   hash funs, the canonical table, the `Setters::eq_classes`
   back-pointer, the clear during `reset_analysis`).
3. Add a CLI/env switch so the two modes can be benchmarked
   side-by-side; the original author's "not worth it" judgment was
   workload-specific and should be re-tested before becoming
   permanent.
4. Plumb the eager check into `entry_set_compatibility` (block 1)
   and replace the singleton-group path in `compute_setters`
   (blocks 2+3).
5. Re-establish the per-pass goldens; the splitter is the most
   golden-sensitive part of the pipeline, so this depends on
   [issue 009](../issues/009-fa-violations-nondeterminism.md) being
   closed first.

## What this would unblock

Tighter dispatch on programs where call sites consistently see
distinct argument types from the very first call. The current lazy
splitter eventually splits these too, but only after a precision
violation triggers it — which means at least one pass is wasted on
the union'd result.

## See also

- [../IFA.md](../IFA.md) §11.6 "`set_eq_classes` exists but is
  unused (`#if 0` blocks)" — original inline mention; line numbers
  there refer to a previous revision.
- [../IFA.md](../IFA.md) §11.4 "`IFA_PASS_LIMIT = 100`" — the pass
  limit interacts with how aggressive splitting can be before
  divergence.
- `fa.cc` `entry_set_compatibility` and `compute_setters` — the two
  call sites where the dormant blocks would have plugged in.

## History

Removed from the tree as part of the ifa/analysis tier-1 cleanup,
June 2026. Git history before that point preserves the three
`#if 0` blocks and the `SettersClasses` infrastructure verbatim.
