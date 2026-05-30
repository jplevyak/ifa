# CLONE — Type-Directed Cloning

A working reference for LLMs (and humans) maintaining `ifa/analysis/clone.cc`.
Pairs Plevyak & Chien's LCPC '95 paper *Type Directed Cloning for
Object-Oriented Programs* (`clone-lcpc95.pdf`) with the implementation in
this tree.

Sister doc: [IFA.md](IFA.md) (the analysis whose output cloning consumes).

---

## 1. What cloning does, in one paragraph

The IFA flow analysis produces a fine-grained, contour-sensitive view of
the program: every allocation site (`CreationSet`) and every function
specialization (`EntrySet`) is tracked with its own per-context type
information. To get back to *executable* code, the world has to collapse
back into a finite set of concrete `Sym`s (types) and `Fun`s (functions).
Cloning is that collapse — but a smart collapse: equivalent contours
merge, while contours that look different (different dispatch targets,
different boxing, different ivar offsets, different constant arguments)
stay separate and produce specialized clones. The result is C++-style
template-like specialization driven by analysis, not by source syntax.
Empirically (paper §4) cloning eliminates 80-100% of remaining dynamic
dispatch sites and removes 30-60% of all calls — at a typical code-size
cost of ≤2× the baseline, and sometimes ≤1× because static-binding +
inlining + dead-code together shrink more than cloning duplicates.

---

## 2. Vocabulary (paper ↔ code)

| Paper term | Code | Notes |
|---|---|---|
| contour | `EntrySet *` (for functions) / `CreationSet *` (for objects) | Same definition as in IFA |
| selection criteria | `make_not_equiv` calls in `determine_basic_clones` and `determine_clones` | "These contours *cannot* merge" |
| optimization criteria | `ES_FN::equivalent`, `CS_EQ_FN::equivalent`, `CS_SYM_FN::equivalent` | "These contours *would benefit* from staying merged" |
| equivalence partition | `EntrySet::equiv`, `CreationSet::equiv` (each a `Vec<...>*`) | One pointer per contour, all members of a class share the same Vec object |
| contour-indexed dispatch table | bypassed: cloning eliminates dispatch entirely where possible | Paper §3.2 proposes a runtime mechanism; implementation prefers to specialize the call site away |
| concrete type | `Sym *` produced by `define_concrete_types` / `resolve_concrete_types` | `cs->type` after cloning is the concrete Sym; codegen uses this |
| concrete function | `Fun *` produced by `clone_functions` | `Fun::copy()` for each non-trivial equivalence class |

---

## 3. The algorithm at a glance (paper §3, code `clone.cc:1024`)

```
clone(fa)                            // clone.cc:1024
  initialize()                       // §3.1: global_avars, called_ess/css maps,
                                     //       seed equiv classes (one per contour)
  determine_layouts()                // ivar_offset for each CS->vars[i]
  determine_clones()                 // §3.3: fixed-point partition refinement
  build_concrete_types()             // §3.4: synthesize Type_SUM / RECORD /
                                     //       TAGGED / FUN Syms for each CS class
  clone_functions()                  // §3.4: duplicate Funs, rebuild call graph
```

Step ordering matters:
1. `initialize()` *must* run first — it copies `s->creators` to drop CSes
   that the flow loop never instantiated and sets `cs->equiv = {cs}` /
   `es->equiv = {es}` as the starting partition (each contour is alone).
2. `determine_layouts()` decides ivar byte offsets. Done early because
   later equivalence checks compare offsets (`prim_period_offset`) — two
   ESes can't merge if the `.sel` calls inside them land at different
   offsets.
3. `determine_clones()` is the iterative core. It tightens the
   partition (never coarsens) until no class changes.
4. `build_concrete_types()` turns each CS equivalence class into a single
   concrete `Sym`. After this point `cs->type` is the live type for
   codegen; `cs->sym` is preserved only as historical.
5. `clone_functions()` materializes the partition for `Fun`s. Most
   `Fun`s have exactly one equivalence class and need no duplication;
   the rest get one `Fun::copy()` per extra class.

---

## 4. Core data (used by clone.cc)

These are fields on `EntrySet`, `CreationSet`, `AVar`, and `Fun` that
exist *for* cloning even though they are declared in `fa.h`/`fun.h`:

### On `CreationSet`
- `equiv : Vec<CreationSet*>*` — shared equivalence class pointer; two
  CSes are equivalent iff `a->equiv == b->equiv`.
- `not_equiv : Vec<CreationSet*>` — pairwise "must NOT merge" set.
  Populated by `make_not_equiv` (`clone.cc:319`). Used by `CS_EQ_FN`.
- `type : Sym*` — assigned by `define_concrete_types` to the concrete
  type Sym for this CS's class. After cloning this is what matters.
- `tuple_able : 1` — set if all members of the class look like tuples
  (uniform shape, no element variance). Triggers `Type_RECORD` collapse.

### On `EntrySet`
- `equiv : Vec<EntrySet*>*` — shared equivalence class pointer.
- `display[]` — lexical parent chain. The parent ES's `equiv` is part of
  this ES's equivalence key (nested functions follow their parent).

### On `Fun`
- `equiv_sets : Vec<Vec<EntrySet*>*>` — partition of `Fun::ess` by ES
  equivalence; computed in `determine_clones`. The size of this vector
  equals the number of clones this `Fun` will produce.
- `ess : Vec<EntrySet*>` — used ESes, restricted to `fa->ess_set` in
  `initialize()`.
- `called_ess : Vec<EntrySet*>` — ESes called from this Fun's ESes
  (forward call set).
- `called_by_ess : Vec<EntrySet*>` — inverse.
- `called_css : Vec<CreationSet*>` — CSes passed as positional args from
  this Fun.
- `calls : Map<PNode*, Vec<Fun*>*>` — post-cloning call graph (built at
  the very end of `clone_functions`).
- `called : Vec<CallPoint*>` — inverse call graph.
- `nmap : Map<PNode*, PNode*>*` — when a Fun is cloned, `nmap` translates
  PNodes from the original to the clone.
- `fmap : Map<Fun*, Fun*>*` — translates nested-function pointers from
  original to clone for the closure of nested clones.

### On `AVar`
- `cs_map : CSMap*` — per-Sym CS map; used by `ES_FN::equivalent` to
  check that the *set of creation symbols at this AVar* matches between
  two ESes.
- `type : Sym*` — concretized type after `concretize_avar`.
- `ivar_offset : int` — byte offset within the parent CS, assigned by
  `determine_layouts`.

---

## 5. `initialize()` (`clone.cc:21`)

What it actually does, beyond "set up state":

1. **Pin global AVars.** Walk every `Var` in every `Fun`; any AVar whose
   contour is `GLOBAL_CONTOUR` goes into `fa->global_avars`. These are
   conceptually outside the partition — globals are never cloned.
2. **Seed `cs->equiv = {cs}`** for every CS in `fa->css`. Initial
   partition: every CS alone.
3. **Restrict each `Fun::ess` to `fa->ess_set`.** ESes that the analysis
   built but the flow loop never reached are dead and must not influence
   the partition.
4. **Build `Fun::called_ess`, `Fun::called_by_ess`, `Fun::called_css`.**
   These caches drive the *change propagation* in `determine_clones`'s
   outer loop — only Funs touching a changed ES/CS need re-partitioning.
5. **Seed `es->equiv = {es}`** for every live ES.
6. **Rebuild `EntrySet::out_edge_map`** filtering to live edges only.
7. **Drop dead Funs.** Any Fun in `pdb->funs` but not in `fa->funs`
   has its `ess` cleared. Cleanup, not optimization.
8. **Trim `s->creators`.** A class's `creators` list may contain CSes
   that turned out to be dead; restrict to live CSes.

---

## 6. `determine_layouts()` (`clone.cc:379`)

Trivial but load-bearing. For each `CreationSet`, walk `cs->vars` (the
ivar AVars). For each ivar:
- Take `iv->out->type` (the inferred type-without-constants).
- All CSes in that type must agree on `sym->size` and `sym->alignment`,
  or `fail("mismatched field sizes" / "mismatched field alignments")`.
- Align `offset` up to `alignment`, set `iv->ivar_offset = offset`,
  advance `offset += size`.

This is **not** a true cross-CS layout — each CS lays out its own ivars
independently. The cross-CS check happens later via `prim_period_offset`
in `ES_FN::equivalent`: two ESes that call `obj.sel` cannot merge if
their possible receivers put `sel` at different offsets.

---

## 7. `determine_clones()` (`clone.cc:398`)

The fixed-point partition refiner. Paper §3.3 figure: "Contour
Equivalence Functions".

### 7.1 `determine_basic_clones` — seeding CS partition (`clone.cc:324`)

1. Group CSes by `sym` (using `sets_by_f_transitive<CreationSet, CS_SYM_FN>`).
2. For each CS pair within a `sym` group, apply **disqualifiers** that
   force `make_not_equiv`:
   - One has `element`, the other doesn't.
   - Different `vars.n` (different number of ivars).
   - For each ivar position: different `basic_type` (different boxing
     class — `int8` vs `int32`, etc.).
   - For `clone_for_constants` variables: different constant set
     (`av->out->constants() != ...`).
   - Special case `MERGE_UNIONS` (compile-time constant `1`): for union
     types, skip the comparison if one side's `out` is empty.
3. Then `sets_by_f<CreationSet, CS_EQ_FN>` partitions into equivalence
   classes respecting `not_equiv`. Each CS's `equiv` is set to its class.

### 7.2 Outer loop (`clone.cc:407`-`478`)

Process changed sets until they stop changing:

```
while (changed_css.n) {
  last_changed_css = changed_css; changed_css.clear()
  last_changed_css_ess = collect_ess_from_css(last_changed_css)

  // (a) re-partition ESes for every Fun touched by a change
  while (changed_ess.n || last_changed_css.n) {
    last_changed_ess = changed_ess; changed_ess.clear()
    for f in fa->funs:
      if f's ess intersect any changed set:
        recompute f->equiv_sets via sets_by_f<EntrySet, ES_FN>
        for each ES in f->ess whose class changed:
          es->equiv = newclass
          changed_ess += newclass
    last_changed_css.clear()  // first pass only
  }

  // (b) "non-equivalent CS" inference from dispatch divergence
  for f in fa->funs:
    for ess_class in f->equiv_sets:
      group out_edges by target PNode (AEDGE_FN)
      for each pair (e1, e2) in same group, same called Fun,
        but to-ESes in different equiv classes:
          for each positional p:
            d1 = a1->out \ a2->out
            d2 = a2->out \ a1->out
            for c1 in d1, c2 in d2 with c1->equiv == c2->equiv:
              make_not_equiv(c1, c2)

  // (c) re-split CS classes that gained not_equiv constraints
  for each css_set in css_sets_by_sym:
    sets_by_f<CreationSet, CS_EQ_FN>  // CS_EQ_FN respects not_equiv
    if any class changed, mark CSes as changed
}
```

The loop monotonically tightens the partition (never coarsens), so it
must terminate: there are finitely many contours, and each refinement
strictly splits at least one class.

### 7.3 The two equivalence functions (paper §3.3, Fig. 3)

**`ES_FN::equivalent(a, b)`** — `clone.cc:185`. Two ESes for the same
Fun are equivalent if **all** of:
- Same parent equivalence class via `display[nesting_depth-1]->equiv`.
- If `clone_for_constants`: same constant set on each marked argument.
- Same boxing (`basic_type`) on every `Var` in `fa_all_Vars`
  (`equivalent_es_vars`).
- For every PNode: same call target equivalence classes for outgoing
  AEdges (`equivalent_es_pnode` — checks `out_edge_map`).
- For every result PNode with `cs_map`: both must have a `cs_map` with
  the same set of CS-equivalence pointers (this catches "this call site
  creates the same shape of object").
- `P_prim_period`: same selector `out` *and* same offset for the
  resolved ivar (`prim_period_offset`).
- `P_prim_cast`: same operand `out`.

A subtle pitfall on `clone.cc:220`: the cs_map check has a `return 0`
at the end of its branch unconditionally — which is wrong-looking but
intentional (any AVar with cs_map disqualifies merging unless both
agree on all CS classes; the function returns 0 to force the conserva-
tive split). Read it carefully if you touch this.

**`CS_EQ_FN::equivalent(a, b)`** — `clone.cc:259`. Two CSes are
equivalent iff they share a `sym` AND aren't in each other's
`not_equiv` set. (Cross-sym CSes are never equivalent.)

**`CS_SYM_FN::equivalent(a, b)`** — `clone.cc:254`. Same `sym`. Used
to bucket CSes for `determine_basic_clones`.

**`AEDGE_FN::equivalent(a, b)`** — `clone.cc:249`. Same PNode (i.e.,
edges from the same call site, regardless of target).

### 7.4 `sets_by_f` vs `sets_by_f_transitive`

Both partition a Vec by an equivalence predicate, but:
- `sets_by_f` (`clone.cc:264`) requires the predicate to hold against
  *every* member already in a candidate class (stricter; produces
  smaller classes). Used when the predicate is not provably transitive.
- `sets_by_f_transitive` (`clone.cc:285`) only checks the first member
  (cheaper; correct when the predicate is transitive). Used for
  `CS_SYM_FN` and `AEDGE_FN`, which trivially are.

Don't swap these — `ES_FN` and `CS_EQ_FN` are explicitly *not*
transitive, so `sets_by_f` is required there.

---

## 8. `build_concrete_types()` (`clone.cc:836`)

After `determine_clones`, every CS has its final `equiv`. Now we mint
concrete `Sym`s. Two sub-phases:

### 8.1 `define_concrete_types(css_sets)` (`clone.cc:567`)

For each CS-equivalence class `eqcss`:

1. `set_tuple_able(css_sets)` — propagate `tuple_able` to the whole
   class (only true if *every* member is tuple-able).
2. `get_sym_tup(eqcss, &sym, &tup)` — extract a representative `sym`
   (or `(Sym*)-1` if members disagree) and `tup` flag.
3. **No-clone path:** if all members share a `sym`, and `sym` is not
   `sym_tuple` / `sym_closure`, not tuple-able, and the class doesn't
   strictly subset `sym->creators` — just assign `cs->type = sym`.
   The original sym is reused as-is.
4. **Clone path:** for classes that don't qualify for no-clone:
   - **Tuples / closures / `tuple_able`:** `s = sym->clone()`,
     `s->type_kind = Type_RECORD` (or `Type_FUN` for closures),
     transfer creators, deal with `name` and `ast` collisions (mark
     `BAD_NAME` / `BAD_AST` if members disagree).
   - **Primitive / tagged / fun:** preserve `type_kind`; only clone
     when there's a meaningful element AVar (`get_element_avar` on the
     class representative).
   - **Other class:** generic clone with `s->type_kind = sym->type_kind`,
     transfer creators.
5. **Mixed-sym fallback:** if `sym == (Sym*)-1` (members disagree on
   `sym`), build a fresh `Type_SUM` Sym with all members as creators.

The "abstract" branch (`abstract = eqcss->n == 1 && eqcss->v[0]->defs.n
== 0`) uses `sym->copy()` (a shallow copy) instead of
`sym->clone()` (a deep clone) because there's no concrete instantiation
to attribute the new type to.

### 8.2 `resolve_concrete_types(css_sets)` (`clone.cc:800`)

For each CS class:
- `concretize_avar` on every ivar AVar and the element AVar. This sets
  `av->type` to either a single concrete Sym, or a `Type_SUM`/LUB if
  the AVar's `out` is mixed.
- Then, depending on the class's representative `type_kind`:
  - `Type_SUM` — populate `sym->has` with all member CS types, call
    `if1->callback->make_LUB_type(sym)` to get a unique LUB sym, and
    propagate the (possibly different) result back to all members.
  - `Type_PRIMITIVE` / `Type_RECORD` / `Type_REF` / `Type_FUN` /
    `Type_TAGGED` — `compute_member_types(eqcss)` builds `sym->has[i]`
    for each ivar position by union-ing all members' ivar `out` types
    and asking the callback to LUB-collapse.
  - `Type_NONE` / `Type_UNKNOWN` / `Type_ALIAS` / `Type_APPLICATION` —
    nothing to do.

`concretize_var_list_type` (`clone.cc:716`) is a special-case fast path:
if all of a `Var`'s contours produce only `sym_list` (possibly with
different element types), build a single cloned `sym_list` whose
`element` is the union of element types. Avoids producing N redundant
list types.

---

## 9. `clone_functions()` (`clone.cc:936`)

Now that types are concrete, materialize the function partition.

### 9.1 Per-Fun cloning

```
fs = sort(fa->funs, by nesting_depth ascending)
for f in fs:
  if f->equiv_sets.n == 1:
    fixup_clone(f, f->equiv_sets[0])
    concretize_types(f)
  else:
    for i in 0..equiv_sets.n - 1:
      if i != last:
        ff = f->copy()           // duplicate Fun + PNodes + Vars
        fixup_clone_tree(ff, equiv_sets[i], fs)
        concretize_types(ff)
      else:
        fixup_clone(f, equiv_sets[i])   // keep original for last class
        concretize_types(f)
```

`Fun::copy()` does the heavy lifting: it produces `fmap` (nested-Fun
translation) and `nmap` (PNode translation) so the rest of the cloner
can map old → new pointers as needed.

### 9.2 `fixup_clone_ess(f, ess)`

For the cloned Fun:
- `f->ess = ess` (this clone owns exactly these ESes).
- Mark all `live_pnodes` (translated via `nmap`) as `fa_live`.
- For each ES: reassign every outgoing AEdge's `fun` to `f`, and
  reassign the ES's own `fun` to `f`.
- Rebuild `f->equiv_sets`.

### 9.3 `fixup_clone_vars(f, ess)`

Filter each `Var::avars` map to keep only AVars whose contour is one of
this clone's ESes. For nested-function vars, the filter uses the
display: `es->display[v->sym->nesting_depth - 1] == x->key`. Same Var
object survives in both clones; only the `avars` map differs.

### 9.4 `fixup_clone_tree(f, ess, fs)`

When a Fun gets cloned, its *nested* Funs follow. For each nested Fun
`ff`, partition its ESes by whether the display anchor at this Fun's
depth is in the new ESes (`new_ess`) or stays with the original
(`old_ess`). Assign each side to its respective clone (`new_ff` via
`fmap`, `ff` keeps `old_ess`). Insert `new_ff` into the iteration list
`fs` so the outer loop processes it.

### 9.5 Rebuilding the call graph

After all clones exist:
```
for es in fa->ess:
  f = es->fun
  for each (old_pnode, edges) in es->out_edge_map:
    pnode = f->nmap ? f->nmap[old_pnode] : old_pnode
    f->calls[pnode] = set of ee->to->fun for ee in edges if used
    new_out_edge_map[pnode] = edges
  es->out_edge_map = new_out_edge_map
for f in fa->funs:
  for (pnode, funs) in f->calls:
    for ff in funs:
      ff->called += CallPoint(f, pnode)
```

Two notes:
- *Only* `EntrySet::fun` has been updated to the cloned Fun at this
  point; the AEdge's `fun` field was updated in `fixup_clone_ess` but
  this code re-reads from `es->out_edges` so order matters.
- Both `Fun::calls` and the inverse `Fun::called` are what later
  passes (inlining, dead code, codegen) consume.

---

## 10. Configuration knobs

In `clone.cc`:
- `MERGE_UNIONS = 1` (top of file) — empty-union side skipped in
  `determine_basic_clones`. Disabling produces more aggressive splitting
  of union types.
- `CONVERT_LISTS_TO_TUPLES = 1` — controls `tuple_able`. When a list's
  element AVar is bottom (the list is never appended to), treat it as
  a tuple of known size for layout. Disabling forces all lists to stay
  list-typed.

There are no runtime flags for cloning — its behaviour is wired to the
analysis and IR. Tuning happens through IFA (e.g.
`fa->num_constants_per_variable` increases constant-class granularity
which then propagates into cloning).

---

## 11. The compilation-database angle (paper §3.3, code: dormant)

The paper notes that cloning decisions are stable across runs and
could be cached. The intent in `cdb.h`/`cdb.cc` is to write the
"ES_id → CS_ids + outgoing edges" mapping to disk after a successful
compile, then on the next compile pre-seed `equiv` classes from that
file. Currently `write_cdb` returns -1 and `check_es_db` (in `fa.cc`)
returns 0 — the feature is inert. Reviving it would mostly mean
filling those two routines; the on-disk format is already specified in
`cdb.h`'s `CDB_EntrySet` / `CDB_CreationSet`.

---

## 12. Commentary, gotchas, suggestions

### 12.1 Equivalence functions are NOT transitive
`ES_FN::equivalent` and `CS_EQ_FN::equivalent` can return 1 for (a,b)
and (b,c) but 0 for (a,c) — they encode pairwise compatibility, not a
true equivalence relation. That's why `sets_by_f` checks against every
member before admitting a candidate, and why `not_equiv` is a pairwise
*set* rather than a class-level flag. If you refactor and accidentally
switch to `sets_by_f_transitive`, the partition will silently become
too coarse, codegen will fail late and cryptically (mismatched ivar
offsets, wrong call targets), and the bug will be very hard to bisect.

### 12.2 Monotonicity is the termination argument
The outer loop in `determine_clones` only *refines* classes — never
merges. Termination follows from finite partition lattice + monotonic
refinement. If you add a new equivalence check that *merges* classes,
the loop will infinite-loop on adversarial inputs. Don't.

### 12.3 `Fun::copy()` clones a lot
A single `Fun::copy()` produces fresh `PNode`s, fresh `Var`s for
internals, fresh `Code` objects (via `IF1::clone`), and `nmap` / `fmap`
mappings. For a Fun with N PNodes the cost is O(N). On programs with
heavy polymorphism in nested closures, multiple equivalence classes
can produce a multiplicative blowup. The paper measured ≤ 2× code-size
on benchmarks but that doesn't generalize.

### 12.4 `concretize_avar` returns -1 on failure
If `make_LUB_type` can't form a LUB (because the frontend hasn't
defined one for that combination of types), `concretize_avar` returns
-1, which bubbles up to `clone(fa)` returning -1. The caller in
`pdb.cc`'s `clone` wrapper aborts compilation. If you see this, the
frontend (`pyc` for Python, or the V frontend) needs a richer
`make_LUB_type` callback.

### 12.5 `prim_period_offset` is hot
Computed inside `ES_FN::equivalent`, which itself is called O(|ES|²)
times per outer loop iteration. Memoization would be straightforward
(key on PNode + ES) and would meaningfully speed up large programs.

### 12.6 The Fun copy / nested-Fun fix-up is fragile
`fixup_clone_tree` partitions a nested Fun's ESes by the *current
clone's display anchor*. If the display indices are off by one — easy
to do when refactoring nesting-depth code — the wrong ESes end up in
the wrong clone, with no immediate symptom. The defensive fix would be
an assertion that every ES in `new_ess` actually has the right display
parent. Currently we trust the invariant.

### 12.7 `equivalent_es_ivars` is a stub
`clone.cc:86` returns 1 unconditionally with a `// object inlining
placeholder` comment. The paper mentions object inlining as a future
optimization (§6). The hook exists; the body doesn't.

### 12.8 The pairwise `not_equiv` set can blow up
Worst case O(|CSes|²) per `sym`. For programs with thousands of
allocation sites of one class this is a real cost. The paper-style
algorithm assumes "few classes" — real programs in this codebase tend
to validate that assumption, but a pathological generator would
expose it.

### 12.9 No unit tests for cloning
Like `fa.cc`, `clone.cc` is exercised only via the end-to-end V
tests. The most useful targeted tests would cover:
- ES equivalence with constants on/off,
- CS equivalence with different ivar boxing,
- Nested-function cloning with shared display,
- LUB failures from `make_LUB_type`.

### 12.10 The `clone.cc` style is dense and templated
Lots of `sets_by_f<T, FN>`, lots of `Vec` arithmetic
(`set_union`/`set_difference`/`some_disjunction`/`some_intersection`),
and `if`-chains without braces. The semantics are clearer when read
alongside the paper's Fig. 3 ("Contour Equivalence Functions") — that
figure is essentially `ES_FN::equivalent` and the body of `determine_clones`'s
outer loop in pseudocode.

### 12.11 Code-style observations (suggestions, not edicts)
- `BAD_NAME = (char *)-1` / `BAD_AST = (IFAAST *)-1` (`clone.cc:18-19`)
  — sentinel via cast to invalid pointer. Functional but startling. A
  small wrapper struct would be clearer.
- The "C++'s answer to higher-order functions: YUCK" comment at
  `clone.cc:262` accurately describes the template approach. C++20
  concepts or `std::function` could clean this up but would slow it
  down measurably.
- `determine_clones` would benefit from being split into named helper
  functions per inner loop stage; right now the staged refinement is
  implicit in the control flow.

---

## 13. Where to read first when something's wrong

| Symptom | Start here |
|---|---|
| "wrong concrete type assigned to ivar" | `compute_member_types`, `concretize_avar` |
| "two distinct call sites collapsed to same Fun" | `ES_FN::equivalent` — likely missing disqualifier |
| "Fun cloned more than expected" | `ES_FN::equivalent` — over-strict; check constants / boxing checks |
| "LUB failure in build_concrete_types" | frontend `if1->callback->make_LUB_type` |
| "ivar offset mismatch panic" | `determine_layouts` — CSes disagree on size/alignment of an ivar's type |
| "infinite loop in determine_clones" | added a *merging* check by accident; partition is no longer monotonic |
| "nested Fun's ESes split wrong way" | `fixup_clone_tree`, display anchor indexing |
| "tuple appears as list" / "list appears as tuple" | `tuple_able`, `CONVERT_LISTS_TO_TUPLES` |

---

## 14. References

- Plevyak, J. & Chien, A. "Type Directed Cloning for Object-Oriented
  Programs", LCPC '95. PDF in this tree: `clone-lcpc95.pdf`.
- Chambers, C. & Ungar, D. "Customization: Optimizing Compiler
  Technology for SELF", PLDI '89 — the customization baseline IFA's
  cloning generalises (the paper's `baseline` configuration is
  essentially Chambers/Ungar customization).
- Cooper, K. et al. "Procedure Cloning", ICCL '92 — earlier
  context-sensitive cloning for procedural languages.
- Plevyak, J. & Chien, A. "Iterative Flow Analysis", OOPSLA '94 — the
  upstream analysis. See [IFA.md](IFA.md).
