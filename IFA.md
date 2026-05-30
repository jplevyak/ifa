# IFA — Iterative Flow Analysis

A working reference for LLMs (and humans) maintaining the IFA implementation
in this tree. Pairs the 1994 OOPSLA paper (`ifa-submit.pdf`,
Plevyak & Chien) with the actual code in `ifa/analysis/`.

The reader is expected to know SSA, points-to / type-inference style flow
analysis, and ML/Self-style polymorphic OO. This document is a map, not a
re-derivation of the paper.

---

## 1. What IFA is, in one paragraph

IFA performs whole-program **control + data flow analysis** for high-level
object-oriented and functional languages where data flow and control flow
are mutually dependent (object types decide dynamic dispatch targets, which
decide which code runs, which produces the next round of object types).
Classical k-CFA and OPS pre-commit to a fixed abstract representation
(a fixed number of "contours" per program point) before the analysis runs.
IFA starts with **one contour per program point (≈0-CFA)** and, when the
result is too imprecise, **iteratively refines the abstract domain** —
splitting functions, contours and data — and re-runs analysis until a fixed
point is reached. The empirical result reported in the paper is much better
precision than 0-CFA/OPS at competitive cost (Tables/Figures 12-17), with
80-100% of dynamic dispatch sites statically bound on the benchmark set.

---

## 2. Vocabulary cheat-sheet (paper ↔ code)

| Paper term | Code type | Notes |
|---|---|---|
| program text variable `x` | `Sym *` / `Var *` | `Sym` is the symbol, `Var` is the usage in IF1 |
| flow value | `CreationSet *` (CS) | A specific allocation site / concrete shape |
| abstract type (set of flow values) | `AType *` (`Vec<CreationSet*>`) | Canonicalized & hash-consed |
| contour | `EntrySet *` (ES) for functions; `CreationSet *` for objects | What gets "split" |
| flow edge | `AVar` forward/backward links | `flow_vars`, `flow_var_to_var` |
| call edge | `AEdge *` | Connects an `EntrySet` to another `EntrySet` |
| function (callee specialization) | `EntrySet *` | One ES = one specialization of one `Fun` |
| object/instance (CreationSet) | `CreationSet *` | A canonical creation/allocation contour |
| `Func(c)` (function contribution) | union over edges in `EntrySet::edges` | drives `analyze_edge` and dispatch |
| `Conf(c)` (call contribution) | `add_send_edges_pnode` | maps caller contours to callee contour |
| `Object(c)` (object contour) | `CreationSet` with its `var_map` / `vars` | object inspection |
| `AssignSet(v,Val)` / `Block(v)` / `Path(v)` | `Setters` + `mark_map` + reach‐ability over `forward`/`backward` | see §6 splitting |
| Split (function, contour, data) | `split_entry_set`, `find_or_make_filtered_entry_set`, `creation_point` | see §6 |
| Concretize / clone | `clone.cc` (`determine_clones`, `clone_functions`) | post-pass that maps the abstract analysis back into concrete types/functions for codegen |

`AVar` = (`Var`, contour) pair. `make_AVar(Var, EntrySet*)` chooses the right
contour automatically: globals → `GLOBAL_CONTOUR`, nested-function vars →
the appropriate display entry, internal vars → the entry set itself.

---

## 3. The algorithm at a glance

The implementation in `ifa/analysis/` directly mirrors the paper:

```
FA::analyze(top)                      // fa.cc:3822
  initialize()                        // basic types, primitives, patterns, top edge
  loop:
    initialize_pass()                 // clear violations + refresh top edge
    refresh_top_edge(top_edge)
    while (edge_worklist || send_worklist):
        analyze_edge()                // (re)bind ES, propagate args/rets, see §5
        add_send_edges_pnode()        // make calls happen for new types
        add_es_constraints()          // (re)scan EntrySet's PNodes
    complete_pass()                   // collect violations
  while extend_analysis() == true     // §6 — split & request another pass
  set_void_lub_types_to_void()
  remove_unused_closures()
  report violations / errors
```

Then (separately):

```
clone(fa)                              // clone.cc:1024
  initialize()                         // build called_ess, called_css maps
  determine_layouts()                  // ivar offsets
  determine_clones()                   // ES/CS equivalence fixed point
  build_concrete_types()               // SUM / RECORD / TAGGED / Type_FUN, etc.
  clone_functions()                    // duplicate Fun for each equiv class, rebuild calls
```

The flow loop is essentially 0-CFA per iteration. The novelty lives in
`extend_analysis()` — the splitter that refines the abstract domain and
re-runs the flow loop.

---

## 4. Core data structures (header: `ifa/analysis/fa.h`)

### `AType : public Vec<CreationSet*>`
The lattice element: a *set* of concrete creation sites (≈ "this value
could be one of these objects/constants"). Constants are summarized into
their base type once the cap `num_constants_per_variable` (default 1) is
exceeded — see `type_cannonicalize`. Hash-consed via `cannonical_atypes`,
so pointer equality on `AType*` means semantic equality. Three memoized
binary ops: `union_map`, `intersection_map`, `diff_map`.

Empty `AType` = `bottom_type`. There is no single `top_type` element; the
named `top_type = union(any_type, void_type)`. `unknown_type`, `any_type`,
`void_type`, `bool_type`, `true_type`, `false_type`, `size_type`,
`anyint_type`, `anynum_kind`, `symbol_type`, `string_type`, `tuple_type`,
`function_type` are all module-level (`fa.cc:29-45`) and built once in
`initialize()` (`fa.cc:2792`).

### `CreationSet` (CS)
One per allocation contour. Holds:
- `sym` (the kind, e.g. `sym_closure`, `sym_tuple`, user class) and `type`
  (the concrete type assigned after cloning).
- `vars` — instance variables as `AVar`s (each `AVar` bound to **this CS**
  as its contour).
- `var_map` — name → ivar AVar (used by `P_prim_period` / `P_prim_setter`).
- `defs` — every `AVar` that creates this CS (`creation_point` populates).
- `ess` — entry sets restricted by this CS, `es_backedges` — ES backedges.
- `equiv` (clone.cc) — equivalence class for cloning.
- `split` — parent CS this one was split from.
- `closure_used`, `tuple_able`, `clone_for_constants` — feature flags set
  during analysis.

CSes are *shared per (Sym, creation context)* — multiple AVars can point
at the same CS. Creation goes through `creation_point(AVar v, Sym s, int n)`
(`fa.cc:267`).

### `EntrySet` (ES)
One per function specialization. Holds:
- `fun` — the underlying `Fun*`.
- `args`, `rets`, `filters` (per-position `AType*` filter for arg dispatch).
- `edges` — incoming `AEdge`s.
- `out_edges`, `out_edge_map` — outgoing calls; map is `PNode → vec<AEdge>`.
- `creates` — CSes allocated in this ES.
- `display` — lexical-parent chain for nested functions (one per nesting
  level). Critical for closure semantics.
- `live_pnodes` — PNodes actually exercised.
- `backedges`, `es_cs_backedges`, `cs_backedges`,
  `pending_es_backedge_map` — recursion bookkeeping used by splitting.
- `split` — parent ES this one was split from.
- `equiv` (clone.cc) — equivalence class for cloning.

### `AEdge`
A call edge: `from` ES → `to` ES, via a specific `pnode`, applying a
particular `match : Match*`. `args` is the positional arg map; `rets` is
the per-result `AVar`. `filtered_args` are post-filter AVars used to gate
flow when an ES has type filters on its arguments.

### `AVar`
The flow node for `(Var, contour)`. The flow graph lives here:
- `forward` / `backward` — pointer-set of `AVar`s data flows to/from.
- `gen` — types this AVar generates locally (constants, creation points).
- `in` — accumulated types arriving via `forward` edges + `gen`.
- `out` = `in ∩ restrict` (filtered).
- `restrict` — upper-bound filter (declared/inferred type bound).
- `lvalue` — paired lvalue AVar for assignment.
- `container` — for elements of compound objects (`set_container`).
- `arg_of_send` — AVar dependencies for send (call) re-evaluation.
- `setters` / `setter_class` — for the AssignSet splitting machinery (§6).
- `mark_map` — distance marks for marked-confluence splitting.
- `cs_map` — per-Sym CS map for `creation_point`.

### `ATypeViolation`
Recorded when something doesn't type-check: kind ∈ {PRIMITIVE_ARGUMENT,
SEND_ARGUMENT, DISPATCH_AMBIGUITY, MEMBER, MATCH, NOTYPE, BOXING,
CLOSURE_RECURSION}. Splitting tries to resolve these; survivors are
reported via `if1->callback->report_analysis_errors` and `show_violations`.

### `FA`
Owns: `funs`, `ess` / `ess_set`, `css` / `css_set`, `top_edge`, the
`PDB *pdb` and (post-pass) `CDB *cdb`. Tunables: `num_constants_per_variable`,
`tuple_index_base`, `permit_boxing`, `no_unused_instance_variables`.

---

## 5. The flow loop (`fa.cc`)

### 5.1 Top edge
`make_top_edge(top)` (`fa.cc:2099`) creates a synthetic `AEdge` whose `to`
ES is the entry contour for `main`. `refresh_top_edge` plumbs
`sym___main__` so the iteration has a starting position. The top edge is
re-enqueued at the start of every pass — `initialize_pass()` doesn't clear
ES/CS state, only violation state, so cached structure carries over.

### 5.2 Worklists
Three queues live as `static` globals in `fa.cc:61-63`:
- `edge_worklist : Que<AEdge>` — new/changed call edges.
- `send_worklist : Que<AVar>` — sends whose receiver/arg `out` changed.
- `es_worklist  : Que<EntrySet>` — ES needing re-scan (e.g. an `if`'s
  predicate produced a new value, so dead-code analysis must redo).

Each item carries an `in_*_worklist` bit so it isn't double-enqueued; the
bit is cleared on pop. The order in `FA::analyze`'s inner loop is: drain
edges → drain sends → drain ES, repeat until all empty.

### 5.3 Per-edge analysis (`analyze_edge`, `fa.cc:2033`)
1. `make_entry_set(e, edges)` — pick (or build) the target ES. May
   produce more than one edge (one per CS when splitting on dispatch).
2. For each chosen edge `ee`:
   - **Filter check**: skip if `type_intersection(actual, formal_filter)
     == bottom_type` — this argument can't take the filter.
   - For each positional position `p`: flow `actual → filtered → formal`,
     where `filtered` is an extra AVar that materializes the filter so
     splitting can blame causality. Hook `container` for nested args (§4
     `set_container`). Closures point `filtered` back at the closure CS.
   - Materialize a continuation AVar if needed.
   - Flow the ES's `rets` back into the call site `e->rets`.
   - First time this ES is touched: `add_es_constraints(ee->to)` — walk
     every PNode, build per-Code constraints (`add_pnode_constraints`).

### 5.4 PNode constraints (`add_pnode_constraints`, `fa.cc:1934`)
Dispatch on `code->kind`:
- `MOVE` — direct `flow_vars`.
- `SEND` — `add_send_constraints` (cheap, type-independent) + queue
  `add_send_edges_pnode`.
- `IF` — mark predicate AVar `is_if_arg`; the predicate's `update_in`
  enqueues the ES so liveness/branch-folding can refine.
- `LABEL` / `GOTO` — nothing to do here; CFG is in `PNode`.
- `phi` / `phy` — pre-collected lists are walked at constraint time.

### 5.5 Send semantics (`add_send_edges_pnode`, `fa.cc:1499`)
Non-primitive sends go through pattern matching (Match library):
`all_applications` enumerates the receiver's possible CSes and for each
calls `application`, which:
- If the CS is `sym_closure` (i.e. a partial application capturing args
  and a callable) → `partial_application`: tack the captured args on and
  recurse into the captured function.
- Otherwise → `function_dispatch`: run `pattern_match`, then
  `make_AEdges` per matching `Match` (and possibly synthesize a
  `make_closure` if the match is partial).

Primitive sends inline a large switch over `Prim::index`. Highlights:
- `P_prim_period` (`obj.sel`) — name lookup via `cs->var_map`. Methods
  are detected (intersection with `function_type`) and dispatched via
  `make_period_closure`.
- `P_prim_index_object` / `P_prim_set_index_object` — index into
  CSes; if the CS is `is_vector` flow goes through the element AVar
  (`get_element_avar`), else through the individual ivar AVars when the
  index is a constant.
- `P_prim_clone`, `P_prim_clone_vector` — `structural_assignment` copies
  ivars between a source CS and a fresh `creation_point(result, sym)`.
- `P_prim_isinstance`, `P_prim_issubclass`, `P_prim_typeof`,
  `P_prim_len`, `P_prim_sizeof[_element]`, `P_prim_coerce` — produce
  constant `true_type`/`false_type`/`make_size_constant_type` /
  `make_abstract_type` results when the answer is statically decidable;
  otherwise widen.
- Numeric primitives — type fold via `type_num_fold`, with constant
  folding via `fold_constant` when both arg AVars are singleton constants
  (single-element `AType`s with `imm.const_kind`).
- `P_prim_destruct` — recursive `destruct` against `t->has` produces
  MATCH violations on shape mismatch.

### 5.6 Inter-procedural propagation
`update_in(v, t)` (`fa.cc:199`) re-computes `out = in ∩ restrict`. If `out`
changed it:
1. wakes `arg_of_send` listeners (the send PNodes that depend on this
   AVar's value),
2. wakes the owning ES if `is_if_arg`,
3. propagates to every `forward` AVar.

This is the inner cycle of the abstract interpreter.

---

## 6. The iterative part (`extend_analysis`, `fa.cc:3707`)

Each completed pass runs `extend_analysis`. Returning `true` causes the
outer loop in `FA::analyze` to `clear_results()` and re-execute.

The order is significant — earlier strategies are cheap and most often
sufficient, later ones target rarer imprecision sources:

1. **Type-based ES splitting (function splitting)** — `split_ess_for_type`
   → `split_entry_set` → `find_or_make_filtered_entry_set`. Detect
   confluences (`collect_type_confluences`) where two contributors deliver
   different *base types* into an AVar; build a new ES per type filter on
   that argument. Existing edges are re-targeted via `split_edges`.
   See paper §5.3 "Function Splitting".

2. **Mark-based ES splitting** — `split_ess_for_mark_type`. Uses
   `build_type_marks` to compute distance-from-source marks on AVars; two
   contributors that share a type but differ in their *origin* (different
   `mark_map` entries) still split. This is how IFA handles the
   recursion-meets-polymorphism case without falling back to k-CFA.

3. **Setter-based splitting (data splitting on AssignSets)** —
   `compute_setters` builds `Setters` (canonical sets of "AVars that wrote
   me") and `setter_class` equivalence; `split_for_setters` and
   `split_for_setters_of_setters` propagate splits backwards through the
   write graph until *paths* (`Path(v)` in paper §5.4) are unambiguous.
   Paper §5.4 "Data Splitting" — implemented as a backward problem over
   `AVar::backward`, with `update_setter` propagating set growth.

4. **Setter+marks splitting** — same as (3) but combined with mark-based
   confluence detection.

5. **Violation-driven splitting** — `split_for_violations`. Any
   `type_violations` recorded during the pass become drivers:
   `collect_violation_imprecisions` traces back from the failure site to
   the responsible AVars, then re-runs (1) and (2) with `SPLIT_DYNAMIC`
   on those. This is the part that resolves real dispatch ambiguities.

If none of the five steps produced a split (`analyze_again == 0`), the
analysis is at a fixed point: `clear_results()` is NOT called, the outer
loop exits, and we move to cloning.

A hard stop exists at `IFA_PASS_LIMIT = 100` (in `fa.h:11`) to keep
pathological inputs bounded. When tripped, the analysis just stops and
reports whatever violations remain.

### Splitting bookkeeping
`record_backedges` propagates `pending_es_backedge_map` so a child ES
inherits its parent's recursion bookkeeping. `is_es_recursive`,
`is_es_cs_recursive` (`fa.cc:2905`-`2927`) cache "this ES participates in a
recursion SCC" so the splitter doesn't try to split inside a cycle (paper
§6.1 — recursion limits splitting on recursive structures).

`update_display` (`fa.cc:755`) keeps the lexical display correct as ESes
get cloned for nested functions.

---

## 7. Constants, primitives, and numeric folding

- Primitives are declared in `if1->primitives`, initialized in
  `initialize_primitives()`. Each `Prim::args[]` is an `AType*` filter
  (constructed from `PRIM_TYPE_*` codes — `ANY`, `STRING`, `SIZE`,
  `TUPLE`, `CONT`, `REF`, `ANY_NUM_A/B`, `ANY_INT_A/B`, `ALL`).
- Constant folding for binary numeric primitives: only triggers when both
  arg AVars have `out->n == 1` (singleton AType) and that single CS has
  `sym->imm.const_kind` set. See `fa.cc:1555`-`1566`.
- `coerce_num` (`fa.cc:314`) is the C-style usual-arithmetic-conversion;
  it never narrows.
- `type_cannonicalize` enforces `num_constants_per_variable` (default 1):
  if a variable would hold more than the limit of distinct constants, the
  type is widened to the base type. This is a precision/cost knob.
- `make_size_constant_type(n)` / `make_constant(imm, sym)` — used to
  feed the constant lattice when something is statically known
  (e.g. `len(tuple)` → exact size).

---

## 8. From flow to code: cloning (`clone.cc`)

After analysis the world is described in terms of `AVar` × contour pairs,
but the IF1 IR needs concrete `Sym`s and `Fun`s for codegen. `clone()`
collapses equivalent contours and synthesizes concrete types.

See [CLONE.md](CLONE.md) for the full algorithm + implementation walk
(paper: Plevyak & Chien, "Type Directed Cloning for Object-Oriented
Programs", LCPC '95, `clone-lcpc95.pdf`). Quick summary of how the two
pieces connect:

- `clone()` consumes `fa->ess` / `fa->css` / `fa->funs` plus all the
  per-AVar state IFA produced, and writes back `cs->type`, `av->type`,
  `Fun::calls`, `Fun::called`, plus possibly new cloned `Fun`s.
- It assumes IFA has reached a fixed point (`extend_analysis` returned
  false) and that `fa->ess_set` / `fa->css_set` are populated.
- It calls back into the frontend via `if1->callback->make_LUB_type`
  whenever it needs to collapse a mixed-type AVar into a single Sym.

CDB (`cdb.h`/`cdb.cc`) is a *compilation database* — sketched but not
fully wired (`write_cdb` returns -1). The intent (paper §6, "additional
language features") is to feed last run's profile/ES info back into the
next run for incremental analysis. Currently inert.

PDB (`pdb.h`/`pdb.cc`) is a tiny program-wide function table holding the
IF1 instance and `LoopGraph`. `pdb` is a process-wide singleton.

---

## 9. Hooks the analysis depends on

`fa.cc` reaches into:

- `if1->callback->finalize_functions()` — called once in `initialize()`
  before any types are built. Frontends must have populated `Fun`s and
  symbols by this point.
- `if1->callback->reanalyze(type_violations)` — the outer `do/while` in
  `FA::analyze` calls this; a frontend can force another pass even when
  `extend_analysis` thinks we're done (used for late constraint discovery).
- `if1->callback->make_LUB_type(sym)` — clone.cc uses this when a SUM
  type collapses into a real union/LUB sym understood by codegen.
- `if1->callback->report_analysis_errors(type_violations)` — frontend-
  formatted diagnostics.

The pattern library (`pattern.cc`) owns `pattern_match`,
`build_arg_positions`, `build_patterns`, `cannonicalize_mposition`,
`Match::merge`. IFA treats it as opaque; it produces the `Match*` objects
that drive `make_AEdges`.

---

## 10. Build, run, debug

- Build: `make` from `ifa/` (clang++, llvm-config, Boehm GC, PCRE,
  dparser). See `AGENTS.md`.
- Run analysis on a V program: `./ifa tests/<x>.v`. Add `-d`/`-v` for
  debug/verbose, `-l <flags>` for selective log channels — the splitter
  uses `LOG_SPLITTING` extensively.
- `IFA_LLVM=1 ./ifa <x>.v` switches to the LLVM backend.
- Useful pretty-printers (debugger entry points):
  `pp(AVar*)`, `pp(AType*)`, `pp(CreationSet*)` at the very end of
  `fa.cc`, and `fa_print_backward`, `fa_dump_var_types`, `fa_dump_types`,
  `show_avar_call_tree`, `show_violations`.
- `write_code_exit` (global int, `fa.cc:25`) lets you dump IF1 after
  a particular pass and stop — set it from gdb to bisect.

The verbose pass summary line printed each pass:

```
PASS N COMPLETE: T sec, flow F (%), match M (%), extend E (%)
```

If `match` dominates, pattern dispatch is the cost; if `extend`
dominates, splitting is. The paper (§6.2) notes IFA is empirically
"close to" the same complexity as 0-CFA — extend cost is usually low.

---

## 11. Commentary, gotchas, open issues

These are notes for whoever maintains this code next.

### 11.1 Pointer-equality everywhere
AType / Setters / SettersClasses are hash-consed. **All comparisons assume
canonicalized inputs.** Constructing an `AType` and forgetting to call
`type_cannonicalize` before storing it in a map will silently produce
duplicates that compare unequal. The public constructors (`make_AType`,
`make_abstract_type`) canonicalize for you; raw `new AType(...)` requires
manual `type_cannonicalize`.

### 11.2 Hidden invariants that aren't asserted
- `AVar::contour` is `EntrySet*` iff `contour_is_entry_set`; otherwise
  it's `CreationSet*` or `GLOBAL_CONTOUR` (sentinel value `(void*)1`).
  Lots of code casts based on this bit — getting it wrong corrupts the
  flow graph silently.
- `EntrySet::edges` is indexed by `AEdge*` pointer hashing (`EdgeHash`)
  but `EntrySet::out_edges` is a plain `Vec`. They overlap but aren't
  symmetric: `out_edges` gates which edges count for violation
  collection, while `edges` gates which edges contribute to splitting.
  Mixing them up is a recurring source of bugs.
- `make_AVar(Var, EntrySet)` for a non-internal global routes through
  `GLOBAL_CONTOUR`, which makes the global a true singleton.  Code that
  passes a real ES expecting a per-ES global will not get one.

### 11.3 The `DEBUG_PRINT` macro is on
`fa.cc:19` enables `DEBUG_PRINT(...)` to stdout. The "off" definition
(`((void)0)`) is right below, commented out. Production builds currently
print extension summaries every pass; flip the macros for quiet runs.

### 11.4 `IFA_PASS_LIMIT = 100`
Hard cap on outer iterations. If a real program ever hits it the result
is silent acceptance of remaining `type_violations`. Two paths:
(a) detect the cap-trip and fail loudly, (b) raise the limit. Production
benchmarks reportedly converge in single-digit passes (paper Table 2),
so 100 is generous.

### 11.5 `check_es_db` is a stub
`fa.cc:922` — the database hook for "use cached entry set decisions from
last compile" is just `return 0`. CDB read/write is half-implemented
(`write_cdb` returns -1). Reviving this is the biggest latent
optimization opportunity — and what the paper called the "compilation
database" feature.

### 11.6 `set_eq_classes` exists but is unused (`#if 0` blocks)
`setters_classes_cannonicalize` and the "Eventual Optimization" block at
`fa.cc:3294` and the `initial_compatibility` check at `fa.cc:816` are
dead code preserved as breadcrumbs for future work. Don't delete them
without understanding the intent — they hint at a planned eager-splitting
mode that was deemed not worth it for the benchmarks at the time.

### 11.7 `P_prim_meta_apply` and `P_prim_cast` are unimplemented
Both `assert(!"implemented")` (`fa.cc:1617`, `fa.cc:1927`). Any program
that exercises them through the V frontend will abort. The Python
frontend (`pyc`, sibling of this tree) emits these primitives in some
edge cases — check there if you see runtime asserts during `pyc`
compilation.

### 11.8 Two `make_closure_var` overloads
`fa.cc:1090` and `fa.cc:1105`. Both legitimately needed (AVar vs Var
input). The pattern recurs: many helpers come in AVar and Var pairs.

### 11.9 `qsort_by_id`
Frequently called on Vecs of AVar/AEdge/EntrySet/CreationSet — gives
deterministic iteration order despite GC allocation jitter. Don't remove
"unnecessary" sorts; some downstream logic assumes ID order (notably the
splitter, where edge order affects which class wins a split).

### 11.10 GC discipline
Everything inherits `gc` (Boehm-GC tracked). No manual `delete`. But
`Vec<>` members own pointers and `clear()`s them lazily — be careful
when reusing a Vec across passes that you haven't kept a pointer-set
that contains stale entries. `clear_results()` in particular wipes a lot
of state; anything cached outside the FA must be recomputed.

### 11.11 "Boxing" handling
`fa->permit_boxing` defaults to false; mixed-basic-type AVars
(`mixed_basics`) become `ATypeViolation_BOXING`. Setting permit_boxing
true silences the violation but leaves codegen on its own to materialize
a union representation. Currently only used by frontends that opt in
explicitly.

### 11.12 NOTYPE → void promotion
`convert_NOTYPE_to_void()` runs at the very end if `fruntime_errors` is
set. It silently turns "no type inferred" into `void`, which can mask
real bugs. Treat this as a debugging escape hatch; don't lean on it.

### 11.13 Tests
`tests/` in `ifa/` are the V-language regression suite. There is no
unit test layer for `fa.cc` itself; the only way to exercise specific
splitting paths is via crafted V programs. Worth building lower-level
fuzz tests for `type_union/intersection/diff` and `type_cannonicalize`
if you start refactoring those.

### 11.14 Code-style observations (suggestions, not edicts)
- `static` global worklists in `fa.cc` make `FA` non-reentrant. Two
  concurrent analyses on the same process would corrupt each other.
  Refactoring into per-FA worklists would enable threaded compilation
  but is invasive.
- `DEBUG_PRINT` should be wired to `log_level` like the rest of the
  codebase; right now it's a separate gate.
- A few `goto Lfound` / `goto Lagain` patterns (e.g. `creation_point`,
  `qsort_pointers`, `qsort_by_id`) are clear once you read them, but a
  light refactor into early-return style would help new readers.
- `EdgeHash`, `PendingMapHash`, `ATypeChainHashFns`,
  `SettersHashFns`, `ATypeViolationHashFuns` follow a hand-rolled pattern
  using `100003` / `13` / `1009` magic primes. A central
  `combine_hash(a, b)` utility would dedupe these.

---

## 12. Where to read first when something's wrong

| Symptom | Start here |
|---|---|
| "type X bleeds where it shouldn't" | `update_in`, `flow_var_to_var`, `flow_vars` |
| "spurious dispatch ambiguity" | `function_dispatch` → `pattern_match`; check `Match::formal_filters` |
| "infinite analysis loop" | watch `analysis_pass`, set `write_code_exit`, inspect `extend_analysis`'s 5 stages |
| "wrong call target after cloning" | see [CLONE.md](CLONE.md) §13 |
| "constant not folded" | `type_num_fold`, check `out->n == 1` and `imm.const_kind` |
| "method lookup misses" | `P_prim_period` in `add_send_edges_pnode`, check `cs->var_map` |
| "closure variable not captured" | `make_closure`, `make_closure_var`, `update_display` |
| "nested function sees wrong outer scope" | `EntrySet::display`, `update_display`, `make_AVar` for nested vars |
| "violation reported but program is fine" | `collect_var_type_violations`, `mixed_basics`, NOTYPE/BOXING |
| "splitter never resolves the violation" | log `LOG_SPLITTING`; check which of the 5 stages declines, in order |

---

## 13. References

- Plevyak, J. & Chien, A. "Iterative Flow Analysis", OOPSLA '94 (the
  PDF in this tree: `ifa-submit.pdf`). The code is a direct descendant.
- Shivers, O. "Control Flow Analysis in Scheme" — k-CFA / 0-CFA, the
  baseline IFA improves on.
- Oxhøj, Palsberg, Schwartzbach — type inference for OO via constraints
  (closure-analysis style, cited as related work in §8 of the paper).
- Agesen et al., Chambers & Ungar — Self-style customization, the
  cloning approach IFA's `clone.cc` mirrors.
