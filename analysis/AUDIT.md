# ifa/analysis — Pre-modification Audit

A snapshot of the state of the code in `ifa/analysis/` (≈6,455
lines across `fa.cc`, `fa.h`, `clone.cc`, `clone.h`, `graph.cc`,
`graph.h`, `cdb.cc`, `cdb.h`, `pdb.cc`, `pdb.h`, `ifalog.cc`,
`ifalog.h`), written as a working brief for the next person
(human or LLM) about to touch this directory — likely starting
with [issue 009](../issues/009-fa-violations-nondeterminism.md)
and its sibling [issue 008](../issues/008-fa-crash-on-nested-iterator-shape.md).

This is paired with [IFA.md](../IFA.md), which describes *what
the analysis does*. Read IFA.md first if you need orientation.
This document is about *the shape of the code* — what to fix,
what not to fix, and what to be aware of when refactoring.

> Reading order recommendation: skim §1 (the headline problems),
> then jump to whichever later section is closest to the task
> you're starting.

---

## 1. Headline issues — in order of likely impact

| # | Issue | Where | Why it matters now |
|---|-------|-------|---|
| 1 | Pointer-keyed hash iteration is non-deterministic | `Vec::set_add_internal` (plib) keyed on `(uintptr_t)c % n`; reached from `Vec::set_add`, every pointer-set, every `ChainHash`, every `Map<Ptr,*>` | **Root cause of [issue 009](../issues/009-fa-violations-nondeterminism.md) and almost certainly [issue 008](../issues/008-fa-crash-on-nested-iterator-shape.md).** Until you tame this you cannot reliably bless any FA golden. |
| 2 | Module-level `static` worklists + globals make `FA` non-reentrant | `fa.cc:54-66, 102-108`; `fa.h:435-453` | Two analyses in one process corrupt each other. Blocks: threaded compilation, in-process test harness reuse, embedding pyc as a library. The `fa_reset()` shim (`fa.cc:74-96`) is a band-aid — easy to add a new static and forget to reset it. |
| 3 | `int` used as `bool` throughout, often with three-valued logic hidden in the type | `entry_set_compatibility` (`fa.cc:881-903`), `edge_type_compatible_with_entry_set` (`fa.cc:726`), `application` returns `{-1, 0, 1}` (`fa.cc:1382`), `split_entry_set` flags `fsetters/fmark/fdynamic` (`fa.cc:3130`), `all_applications` returns `-2/-1/0/1` (`fa.cc:1325`) | Hard to read; the meaning of `0` vs `-1` is undocumented at the call site. Every refactor here risks inverting a polarity. |
| 4 | Hand-rolled hash functions with magic primes scattered across `fa.h` | `PendingMapHash` (`fa.h:75`), `ATypeViolationHashFuns` (`fa.h:252`), `ATypeFoldChainHashFns` (`fa.h:272`); primes `13`, `1009`, `100003`, `open_hash_primes[i % 256]` | Each new hashable type adds a fresh, slightly different, untested combiner. There's no `combine_hash(a, b)` helper. |
| 5 | `DEBUG_PRINT` macro talks to stdout, gated on `ifa_debug`, not on `log_tag` | `fa.cc:21`; called from `extend_analysis` (`fa.cc:3775-3827`) | Bypasses the rest of the logging system (`ifa/analysis/ifalog.h`'s LOG_SPLITTING etc.). Two parallel logging conventions in one file. |
| 6 | `IFA_PASS_LIMIT = 100` silently accepts remaining violations on overflow | `fa.h:11`, `fa.cc:3829` | A real program that hits the cap is misreported as "passed" — see IFA.md §11.4. |
| 7 | `check_es_db` is a `return 0` stub; `write_cdb` is `return -1` | `fa.cc:977`, `cdb.cc:104` | The compilation-database feature is half-built; `cdb.cc:13-74` reads what `write_cdb` never writes. Either revive it or delete it (commit it to git history first). |
| 8 | Two `assert(!"implemented")` aborts on live code paths | `fa.cc:1672` (`P_prim_meta_apply`), `fa.cc:1927` (`P_prim_cast`) | The Python frontend (`pyc`) can emit `P_prim_cast` in edge cases. See IFA.md §11.7. |
| 9 | `mkdir(log_dir, 0xFFF)` — wrong mode | `ifalog.cc:10, 13` | `0xFFF == 04777` (sticky + suid). Almost certainly meant `0755` or `0775`. Latent permissions surprise. |
| 10 | Buffer overflows possible in `graph.cc` via `strcat`/`strcpy` on fixed-size stack buffers | `graph.cc:36-49`, `153-183` (`title[256]`), `246-260` (`id[80]`), `302-323` (`label[80]`), `442-481` (`name[256]`) | All inputs are internal so this is low likelihood, but a symbol name long enough will silently corrupt the stack. `snprintf` is mixed in inconsistently — use it everywhere. |

---

## 2. Reentrancy & state — the single biggest design debt

### 2.1 The non-reentrant set

These are all process-wide globals in `fa.cc` that survive across
analyses but get reset by `fa_reset()`:

```c
// fa.cc:28
int analysis_pass;

// fa.cc:30-46
AType *bottom_type, *nil_type, *unknown_type, *void_type, *top_type,
      *any_type, *bool_type, *true_type, *false_type, *size_type,
      *anyint_type, *anynum_kind, *symbol_type, *string_type,
      *tuple_type, *anytype_type, *function_type;

// fa.cc:48-51 — id counters
static int avar_id, aedge_id, creation_set_id, entry_set_id;

// fa.cc:53-54 — main pointer + timers
FA *fa;
static Timer pass_timer, match_timer, extend_timer;

// fa.cc:56-60 — hash-cons tables, type-fold cache, violation dedup
static ChainHash<AType*,ATypeChainHashFns>          cannonical_atypes;
static ChainHash<Setters*,SettersHashFns>           cannonical_setters;
static ChainHash<SettersClasses*,SettersClassesHashFns>
                                                    cannonical_setters_classes;
static ChainHash<ATypeFold*,ATypeFoldChainHashFns>  type_fold_cache;
static ChainHash<ATypeViolation*,ATypeViolationHashFuns>
                                                    type_violation_hash;

// fa.cc:62-66 — worklists, completion set, violation list
static Que(AEdge,…) edge_worklist;
static Que(AVar,…)  send_worklist;
static Que(EntrySet,…) es_worklist;
static Vec<EntrySet*>      entry_set_done;
static Vec<ATypeViolation*> type_violations;

// fa.cc:102-108 — event sidecar (issue 003)
static bool fa_events_enabled;
static Vec<FAPassEvent*> fa_events_storage;
```

Plus `pdb.cc:8`:
```c
PDB *pdb = 0;
```

…and the `extern`-published ones in `fa.h:435-453`.

### 2.2 Refactor plan (when ready)

The natural endpoint is **everything in §2.1 becomes a member of
`class FA`** (or a `FAContext` it owns). The cleanest sequence
that minimizes diff churn:

1. **First, sink the worklists** (`edge_worklist`, `send_worklist`,
   `es_worklist`, `entry_set_done`, `type_violations`) into `FA`.
   These are touched in maybe 30 spots; each becomes
   `fa->edge_worklist…`.

2. **Then the caches** (`cannonical_atypes`, `cannonical_setters`,
   `cannonical_setters_classes`, `type_fold_cache`,
   `type_violation_hash`). Trickier because the hash-cons identity
   is shared across all `AType` uses; you almost certainly want a
   `class TypeWorld` that owns these, owned by `FA`.

3. **Then the canonical types** (`bottom_type`, `void_type`, …).
   These hang off `TypeWorld`.

4. **Then the id counters** (`avar_id`, `aedge_id`,
   `creation_set_id`, `entry_set_id`). Sit on `FA` directly.

5. **Last, the global `fa`, `pdb` pointers.** Replace
   `if (ifa_verbose) …` with `if (fa->verbose)`; thread the `FA*`
   through call chains that currently rely on the global. Many
   functions already take `EntrySet*` or `AVar*` from which
   `(EntrySet*)av->contour` reaches an `FA*` indirectly via
   `pdb` — adjust by giving `EntrySet` / `CreationSet` a back
   pointer to `FA`, or pass it explicitly.

Each step is independently mergeable; the test suite continues
to use one `FA` per process during transition. Don't try to do
this in one PR.

### 2.3 The `fa_reset()` band-aid

`fa.cc:74-96` exists because the ifa-test runner reuses the
process to run many programs. It zeros every global enumerated
above. **The recurring failure mode: someone adds a new static
and forgets to add it to `fa_reset()`.** Until §2.2 is done,
ensure new statics are added to `fa_reset()` in the same commit
that introduces them, and consider linting for `static\s+\w+\s+\w+;`
at module scope.

### 2.4 `clone.cc` and `graph.cc` reach into the same globals

`clone.cc` uses `fa->css`, `fa->ess`, `fa->funs`, calls
`get_element_avar`, `make_AVar`, `bottom_type`, `type_union`,
`if1->callback->make_LUB_type` — all rely on the §2.1 globals.
Plan for §2.2 to ripple through these files too.

`graph.cc` is read-only with respect to global state but uses
`bottom_type`, `if1`, and (importantly) iterates `fa->css`,
`fa->ess` for diagnostic output. Easy to update once `FA` is
the holder.

---

## 3. Determinism — the cause of issue 009

### 3.1 Where non-determinism leaks in

The plib `Vec` doubles as a pointer-set; `Vec::set_add_internal`
(`ifa/common/vec.h:380`) hashes by `(uintptr_t)c % n`. **Every
`set_add(ptr)` distributes by GC heap address.** Iteration in
storage order is therefore non-deterministic across runs.

Audit signal: every `for (X *y : someVec)` where `someVec` is
populated via `set_add` and *not* subsequently sorted by id is
a candidate non-deterministic iteration. Many spots in `fa.cc`
end with `qsort_by_id(v)` *exactly* for this reason
(see `qsort_by_id` template, `fa.h:393`). Equally many do not.

Likely culprits feeding `type_violation()` (the function that
records the violation count alternating between 13 and 31 in
issue 009):

- **`collect_argument_type_violations`** (`fa.cc:2677-2720`)
  builds a local `Vec<AVar *> actuals` via `set_add`, then
  iterates it. Each iteration may call `type_violation()`. Order
  is pointer-hash-bucket order.

- **`add_send_edges_pnode` `P_prim_set_index_object` branch**
  (`fa.cc:1706-1733`) iterates `vec->out->sorted` (sorted, good)
  but calls `flow_vars` on `cs->vars`, which can later trigger
  `type_violation` via the propagation chain.

- **`add_send_edges_pnode` `P_prim_period` branch**
  (`fa.cc:1746-1789`) iterates `selector->out->sorted` and
  `obj->out->sorted` — both sorted, **but** the `methods` Vec
  it builds during the loop is `Vec<AVar *>` via `add`, then
  iterated; method-resolution order via `for (AVar *x : methods)`
  is insertion order, which is deterministic *given* sorted
  upstream iteration. Should be safe if upstream is sorted.

- **`function_dispatch`** (`fa.cc:1360-1380`) iterates `matches`
  in vector order (set by `pattern_match`); whether that's
  deterministic depends on the pattern library, which is *outside*
  this directory. Worth checking once.

- **`type_diff`** / **`type_intersection`** (`fa.cc:550-616`)
  iterate `a->sorted` × `b->sorted` — sorted. Good.

### 3.2 The dedup race

`type_violation()` (`fa.cc:1389-1403`):

```c
v = type_violation_hash.put(v);   // dedupes on (kind, av, send)
if (!v->type) v->type = type;
else          v->type = type_union(v->type, type);
…
type_violations.set_add(v);
```

The hash key is `(kind, av, send)`. Two iteration orders that
record the same set of `(kind, av, send)` triples produce the
same hash table contents *and the same `type_violations` Vec
contents*, because `set_add` is idempotent.

So the count differing means iteration orders actually produce
**different sets of triples**. That's the signal to investigate:
something upstream is conditionally adding violations based on
state that depends on iteration order. Plausible mechanism: a
loop iterates CSes in non-deterministic order; one ordering
triggers a `set_container` / `flow_vars` / `update_in` cascade
that creates an extra AVar whose `out` then fails a later
`type_diff` check; the other ordering doesn't get there. The
resulting violation against the never-allocated AVar is the
"missing" one.

### 3.3 Quick wins (low-risk, high-value)

- **`Map::form_Map` iteration order is pointer-bucket too.** Of
  the five `form_Map` sites in `fa.cc` (`668`, `679`, `3059`,
  `3486`, `3550`), the first two are within `different_marked_args`
  computing set membership (order-insensitive due to final
  `some_disjunction`); the others build sets that are sorted
  immediately afterward (`qsort_by_id`). Probably safe; verify
  before touching.

- **`actuals` and `from->out_edges.set_in` checks in
  `collect_argument_type_violations`** — sort `actuals` and `*m`
  (`Vec<AEdge *>`) by id before iterating. One-line fix; verify
  it stabilizes 009.

- **Adopt a convention**: any `Vec<Ptr*>` constructed by
  `set_add`/`set_union` and then iterated must call
  `qsort_by_id` (or `qsort_pointers`) before iteration unless
  the loop body is pointer-order-independent. Make this a code
  review checklist item.

### 3.4 The deeper fix

Replace `Vec::set_add` pointer-bucket hashing with content-based
hashing (or chain by id where ids are available). That's a plib
change that affects more than this directory — file it as a
follow-on. Stable id-ordering at the use sites is the pragmatic
fix for §3.3.

### 3.5 Issue 008 (the crash) — what to look for next

The crash is intermittent, only fires in the test harness when
multiple shapes run in the same process, and the count
alternates between two values for the affected shape. Possible
mechanism: `clear_results()` / `clear_avar()` (`fa.cc:3271-3289`)
zeros AVar state but doesn't remove the AVar from the pointer-set
caches it lives in. Next pass, an iteration in pointer order
visits a zeroed AVar; if the visit chases `av->container` or
`av->cs_map`, it can dereference a stale pointer. Look at:

- `clear_avar` (`fa.cc:3271`) — note it doesn't touch
  `cs_map`, `arg_of_send.asset` invariants.
- `clear_cs` / `clear_es` (`fa.cc:3300-3317`) — also don't
  evict from the caches.
- `update_in` (`fa.cc:254`) — recursive through `v->forward`;
  if a forward AVar was cleared and another path adds it back
  with stale `mark_map` or `setters`, the recursion can dive into
  an invalid object.

Plausible repro path: add an `assert(!av->was_cleared_this_pass)`
to `update_in`, mark AVars on `clear_avar`, run the nested-iterator
shape in a loop, observe which AVar trips it first.

---

## 4. C++ usage — modernization candidates

C++23 is enabled (`Makefile: CFLAGS += -std=c++23`) but the code
is firmly C-with-classes. No `<vector>`, `<map>`, `<memory>`,
`<optional>`, `<string_view>`, `<span>`, `<concepts>`,
`<ranges>`, `<format>` — only `<sys/types.h>` and `<sys/stat.h>`
across the whole directory.

The custom plib containers (`Vec`, `Map`, `ChainHash`, `Accum`,
`Que`) are intertwined with the GC discipline — *don't* rip them
out wholesale, they're load-bearing. But:

### 4.1 What is worth modernizing

- **`bool` instead of `int`** for predicates (§1 item 3). One-PR
  change with code review, no functional risk. Skip the
  three-valued `entry_set_compatibility` and `application` which
  genuinely return more than two states; rename and document
  those instead.

- **`nullptr` instead of `0` / `NULL`**. Cosmetic, but reduces
  noise and removes ambiguity in template substitution.

- **`enum class` for `ATypeViolation_kind` and `FAPassStage`**.
  Currently bare `enum`, leaking names. `FAPassStage` is already
  prefixed `FA_STAGE_*` — moving to `enum class` is a 30-line
  rename.

- **`[[nodiscard]]`** on `extend_analysis`, `split_entry_set`,
  `split_for_violations`, and the other `split_*` functions —
  forgetting their return value silently fails to re-iterate.

- **`std::string_view`** for `cchar *` pass-through (very common:
  `show_sym_name`, `show_fun`, `show_type`, `graph_*` family).
  But `cchar*` is `if1_cannonicalize_string`'d throughout —
  pointer equality matters. Use `string_view` *only* in display
  paths.

- **Trailing return types and `auto`** for the heavily nested
  helper return types (`Vec<EntrySet *> *`, etc.). Readability
  win.

### 4.2 What to NOT modernize

- **`Vec` / `Map` / `ChainHash` / `Que` containers themselves.**
  They are GC-aware; replacing with STL would either leak GC
  metadata or require a `gc_allocator<T>` rewrite. Both are
  out of scope for "tackle issue 009".

- **The `LINK(...)` intrusive-list macros** used by the
  worklists (`AEdge::edge_worklist_link`,
  `AVar::send_worklist_link`, `EntrySet::es_worklist_link`).
  They're correct and fast; intrusive linkage doesn't translate
  naturally to STL.

- **`goto Lfound` / `goto Lagain` / `goto Lerror` patterns.**
  Clear once read, idiomatic for the surrounding style. IFA.md
  §11.14 already notes this. Leave them.

- **`assert(!"…")` aborts** that mark unimplemented branches
  (§1 item 8). Either implement them or convert to a structured
  `fail()`. Don't just delete.

### 4.3 Bit-field flags everywhere

`EntrySet`, `CreationSet`, `AVar`, `AEdge`, `ATypeViolation`
all use packed bit-fields:

```c
uint dfs_color : 2;
uint in_es_worklist : 1;
uint clone_for_constants : 1;
uint contour_is_entry_set : 1;
uint is_lvalue : 1;
uint live : 1;
uint live_arg : 1;
uint is_if_arg : 1;
uint in_send_worklist : 1;
…
```

These are correct but easy to misread when grepping. Two things
to remember:

- **`AVar::contour_is_entry_set` is the discriminator** between
  `(EntrySet*)contour` vs `(CreationSet*)contour` vs
  `GLOBAL_CONTOUR` ((void*)1 sentinel). IFA.md §11.2 calls this
  out. The bit being wrong silently corrupts the flow graph.
  When refactoring, consider whether this can become a `union`
  + a `kind` enum.

- **`dfs_color : 2`** is reset between DFS passes
  (`compute_recursive_entry_sets`, `compute_recursive_entry_creation_sets`)
  but lives in the EntrySet/CreationSet permanently. Touching
  it between DFS phases is a real-bug hazard.

---

## 5. Recursion depths — stack-overflow surface area

Several functions are direct or indirect recursive over potentially
deep structures with no depth bound:

| Function | Recurses on | Depth bound |
|---|---|---|
| `update_in` (`fa.cc:254`) | `v->forward` | program flow graph diameter |
| `add_pnode_constraints` (`fa.cc:1989`) | `p->cfg_succ` | CFG depth (function) |
| `mark_es_backedges` (`fa.cc:2891`) | `e->to->out_edges` | call graph DFS |
| `mark_es_cs_backedges` (`fa.cc:2916`, `:2928`) | `cs->ess` / `es->out_edges` / `es->creates` | call+contour DFS |
| `qsort_by_id` (`fa.h:393`) | tail-call style on partition | O(log n) avg, O(n) worst |
| `qsort_pointers` (`fa.cc:434`) | same | as above |
| `update_setter` (`fa.cc:3365`) | `av->backward` | data-flow graph diameter |
| `build_type_mark` (`fa.cc:3213`) | `av->forward` | data-flow graph diameter |
| `build_setter_mark` (`fa.cc:3243`) | `av->backward` | data-flow graph diameter |
| `back_reaching` (`fa.cc:3672`) | iterative via `seen.asvec` | OK (iterative) |
| `destruct` (`fa.cc:1416`) | recursive pattern unpack | nesting depth of `t->has` |
| `vector_elems` (`fa.cc:1091`) | recursive on `rank` | declared rank |
| `record_arg` (`fa.cc:1259`) | recursive on `s->is_pattern` | nesting depth of pattern |

For pyc inputs the depths are typically modest; for adversarial
inputs they're not. Two of these (`update_in`,
`add_pnode_constraints`) run on user code and have no obvious
upper bound. If you ever see a stack overflow report, those are
the first places to look. **Don't preemptively iterativize** —
the recursion is the code's clearest expression — but be aware
when you change them.

---

## 6. Sentinel-value patterns — magic pointer values

The code uses several sentinel pointer values to mean "absent" or
"invalid":

| Sentinel | Type | Used as | Defined at |
|---|---|---|---|
| `GLOBAL_CONTOUR == (void*)1` | `void*` | `AVar::contour` for globals | `fa.h:13` |
| `GLOBAL_DEF`, `NONLOCAL_DEF`, `GLOBAL_USE`, `NONLOCAL_USE` | `PycSymbol*` (in pyc, but conceptually like §6) | scope-stack sentinels | pyc, but pattern recurs |
| `BAD_NAME == (char*)-1` | `char*` | "names disagreed" in concrete-type naming | `clone.cc:18` |
| `BAD_AST == (IFAAST*)-1` | `IFAAST*` | "ASTs disagreed" | `clone.cc:19` |
| `(Sym *)-1`, `(Sym *)-2` | `Sym*` | `basic_type` fail tokens | `clone.cc:133, 358, 538, 559, 578, 600` |
| `(AVar *)-1` | `AVar*` | "two distinct defs" marker | `clone.cc:607` |

These work but they're fragile under sanitizers (ASan / UBSan
will flag `(Sym*)-1` dereferences in some configurations) and
they confuse `std::optional`-style refactors.

When refactoring, prefer a small tagged-union (`std::variant`,
or a `Sym*` + `bool valid`) at the *interface* boundary. Don't
try to remove all sentinels at once — the `BAD_NAME` /
`BAD_AST` pair in particular survives unchanged for clarity in
`define_concrete_types` (`clone.cc:567-685`).

---

## 7. Code that's marked dead but kept on purpose

Don't delete these without thinking. They're breadcrumbs for
planned-but-deferred work.

- **`fa.cc:870-879` (`initial_compatibility` `#if 0`)** — eager
  splitting on first analysis pass. IFA.md §11.6.
- **`fa.cc:3347-3363` (`setters_classes_cannonicalize` `#if 0`)**
  — eager setters-class canonicalization.
- **`fa.cc:3449-3463` (compute_setters grouping `#if 0`)** — the
  alternative SettersClasses grouping algorithm.
- **`fa.cc:1660-1673` (`P_prim_meta_apply` body `#if 0`)** —
  meta-application; falls through to `assert(!"implemented")`.
  See §1 item 8.
- **`fa.cc:977` `check_es_db` stub** — see §1 item 7.
- **`cdb.cc:104` `write_cdb` stub** — same.

The `// removed for X" / // see paper §Y" comments above each
block are load-bearing. Preserve them through refactors.

---

## 8. Things to verify before merging an FA change

Recommended pre-merge checklist for any patch in this directory:

1. **`make` in `ifa/`** — should be a clean build.
2. **`./ifa --test`** — runs the built-in UnitTest harness.
3. **`make test_llvm`** — exercises the LLVM backend round-trip.
4. **`./ifa tests/<x>.v` for each `tests/*.v`** — the V regression
   suite. The pyc test suite (`make test_pyc` in the parent
   repo) exercises this via the Python frontend.
5. **`fa-converge` goldens** — `ifa-test --phase fa-converge`
   over the synthetic shapes. Per [issue 009](../issues/009-fa-violations-nondeterminism.md),
   the `violations=X→Y` field has been dropped from the printer
   for now; the rest of the per-pass line is checked.
6. **Run each fa-converge shape ≥3 times.** If your change
   stabilizes the violation count, restore the
   `violations=X→Y` field in `testing/print_fa_converge.cc` and
   re-bless. If it doesn't, leave it dropped.

There is no unit-test layer for `fa.cc` itself (IFA.md §11.13).
Building one for the lattice ops (`type_union`,
`type_intersection`, `type_diff`, `type_cannonicalize`) is the
single highest-leverage tooling work in this directory — those
are pure functions with no `FA*` dependency once you pass in the
canonical types.

---

## 9. Quick-reference: file-by-file summary

### `fa.cc` (4,112 lines)

The heart. Concerns: §2 (reentrancy), §3 (determinism), §5
(recursion). Refactor order: see §2.2. Don't `make_AVar(v, e)`
when you meant `unique_AVar(v, e)` — they differ on
`contour_is_entry_set`. Don't iterate `EntrySet::edges`
(an `EdgeHash`) expecting deterministic order — sort by id first.

### `fa.h` (459 lines)

Data definitions. Public surface area is large; everything in
the `extern FA*` and `extern AType*` lists in §2.1 ought to
disappear into `FA` over time. New hash-function classes belong
here for now (`*HashFns` convention); centralize them into a
`combine_hash` helper when you touch §1 item 4.

### `clone.cc` (1,031 lines)

Post-analysis cloning. **Single-pass mutation of the world**
established by `fa.cc`: writes `cs->type`, `av->type`, `Fun::calls`,
`Fun::called`, allocates new `Fun`s and `Sym`s. Heavily templated
in places (`sets_by_f<C,FN>`) — the comment `// C++'s answer to
higher order functions: YUCK` (`clone.cc:262`) is a tongue-in-cheek
signal that this is the most "C++"-feeling file in the directory.
Determinism in `determine_clones` (`clone.cc:398-479`) relies
on stable iteration over `fa->ess` / `fa->css` (sorted by id in
`collect_results`) — verify if you touch §3.

### `clone.h` (16 lines)

Tiny interface header. Fine as-is.

### `graph.cc` (507 lines)

Diagnostic output (VCG / GraphViz). **Pure output**, no
analysis state mutated. §1 item 10 (buffer overflows). Also see
`strcat_pattern` (`graph.cc:389`), `strcat_sym_node`
(`graph.cc:143`), `graph_fun_node` (`graph.cc:398`),
`graph_abstract_types` (`graph.cc:442-486`) — all
`strcat`/`strcpy` users.

### `graph.h` (19 lines)

Globals `graph_fun[80]`, `graph_var[80]`, `graph_type`,
`fgraph_frequencies`, `fgraph_constants` — non-reentrant config
state. Migrate to `FA` (or an `IFAConfig`) alongside §2.2.

### `cdb.cc` / `cdb.h` (108 + 43 lines)

Compilation Database. `read_cdb` is implemented, `write_cdb`
returns -1, `check_es_db` (which would use the read result)
returns 0. Three options: (a) finish the writer, (b) delete the
reader and stub (preserve in git history), (c) leave as-is with
a clear comment explaining it's a planned feature. Currently
(c). See §1 item 7.

`read_cdb` uses `fscanf("%1023s …")` — bounded, safe.

### `ifalog.cc` / `ifalog.h` (58 + 54 lines)

A 128-FILE-array log system keyed on letter tags
(`'a' = LOG_AST`, `'i' = LOG_IF1`, …). `mkdir(log_dir, 0xFFF)`
bug — §1 item 9. The static `log_FILE[128]` is reset only on
process exit; another reentrancy hole. Resetting on `fa_reset()`
would entail closing files; weigh against existing behavior.

### `pdb.cc` / `pdb.h` (21 + 27 lines)

Trivial program-database container. The `extern PDB *pdb`
singleton is consumed by `clone.cc` and elsewhere. Migrate
alongside §2.2.

---

## 10. Suggested first steps for issue 009

The minimal, low-risk path to closing 009 (and probably 008):

1. **Instrument `type_violation()` (`fa.cc:1389`)** to log each
   call's `(kind, av->id, send ? send->id : 0, type_hash)`. Run
   `nested_iterator` 10× under the test harness. Diff the logs.

2. **Find the divergence point** — the first call whose
   parameters differ between two runs. That call is downstream
   of a non-deterministic iteration; walk up the stack.

3. **Sort the offending iteration by id** before its loop, with
   `qsort_by_id`. (Likely candidates are listed in §3.1.)

4. **Re-run the 10× test.** If 009 is gone, restore the
   `violations=X→Y` field in
   `testing/print_fa_converge.cc` and re-bless the goldens.

5. **Re-introduce `nested_iterator.synth`.** Run `make test-ir`
   20×. If 008 is also gone (very likely, per the shared
   root-cause hypothesis), close both issues.

6. **Write a regression test.** The pure-function lattice ops
   (§8) are the natural home for "given these inputs in
   different orders, the output is invariant." Add one for
   `type_violation` dedup.

The work above does *not* require any of the larger refactors
in §2 or §4. Save those for after 009 is closed and the
test-harness is reliable enough to validate them.

---

## 11. Things to leave alone (for now)

- **The `Vec`/`Map`/`Que`/`Accum`/`ChainHash` containers.** They
  are tightly coupled to the GC discipline. Don't replace with
  STL — see §4.2.

- **The `LINK(...)` intrusive worklists.** Correct and fast.

- **`qsort_by_id` template in `fa.h:393`.** Recursive
  Hoare-partition style; works fine. Don't switch to `std::sort`
  unless you also switch the containers (§4.2).

- **The five-stage splitter ordering in `extend_analysis`**
  (`fa.cc:3762-3862`). The paper's §5 prescribes the order;
  changing it changes which programs converge in how many passes.
  Don't reorder casually.

- **Pattern-library calls (`pattern_match`,
  `cannonicalize_mposition`, `build_arg_positions`,
  `Match::merge`)**. They're outside this directory.

- **The `IFA_PASS_LIMIT = 100` cap.** Raise it if you must, but
  don't remove it; pathological inputs can loop forever.

---

## 12. Index of related documentation

- [IFA.md](../IFA.md) — what the analysis does (paper ↔ code).
- [CLONE.md](../CLONE.md) — the post-analysis cloning pass.
- [ARCHITECTURE.md](../ARCHITECTURE.md) — top-level IFA structure.
- [issue 003](../issues/003-fa-converge-determinism.md) — the
  earlier convergence-determinism note (FA pass events).
- [issue 007](../issues/007-mark-type-stage-coverage.md) — the
  splitter-stage coverage gap.
- [issue 008](../issues/008-fa-crash-on-nested-iterator-shape.md) — the
  intermittent crash (likely same root cause as 009).
- [issue 009](../issues/009-fa-violations-nondeterminism.md) — the
  non-deterministic violation count.
