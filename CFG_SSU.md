# CFG_SSU — Control Flow, SSU Form, Dominators, Loops

A working reference for `ifa/optimize/{cfg,ssu,dom,loop}.cc`. These four
files share enough state and timing that one doc covers them better than
four. They run as a block when `new Fun(closure)` is invoked, plus
loop discovery is re-run later via `frequency_estimation`.

Sister docs: [IR.md](IR.md) (PNode/Var/Code foundations),
[IFA.md](IFA.md) (consumes the SSU form),
[OPTIMIZE.md](OPTIMIZE.md) when written (consumes the loop+dom info).

---

## 1. The four passes in one diagram

```
new Fun(closure)                            fun.cc:54
  Fun::build_cfg()                          cfg.cc:15
    ├─ resolve_labels  (Label::code ← LABEL Code)
    ├─ build_pn_cfg    (Code → PNode + cfg_succ/cfg_pred)
    ├─ entry = code->pn
    ├─ exit  = last sub Code's pn
    └─ finalize_cfg    (remove_unreachable + set_to_vec preds)
  Fun::build_ssu()                          ssu.cc:174
    ├─ build_cfg_dominators(this)           dom.cc:149
    │   ├─ per PNode: new Dom (forward + reverse)
    │   ├─ wire dom->pred / ->succ from cfg_pred / cfg_succ
    │   ├─ build_dominators(entry->dom)     (Tarjan + Cytron frontier)
    │   └─ build_dominators(exit->rdom)
    ├─ approximate_liveness                 (PNode::live_vars iteration)
    ├─ place_phi  /  place_phy              (insert MOVE nodes)
    ├─ rename_vars                          (SSU rename via cfg_succ walk)
    └─ set Var::def from lvals/phi/phy
  build_uses(this)                          fun.cc:48  (Var::uses)
  setup_ast()                               (PycAST::pnodes back-refs)
  check_invariants(this)                    (release-mode CFG sanity)
```

Later, separately:

```
find_all_loops(fa)                          loop.cc:168
  ├─ for each Fun: find_local_loops         (per-PNode LoopGraph)
  └─ find_recursive_loops                   (call-graph LoopGraph)

build_call_dominators(fa)                   dom.cc:170
  (used by find_recursive_loops)
```

`frequency_estimation` (in `optimize/inline.cc`) reads the per-Fun
`loops` to assign `PNode::execution_frequency`.

---

## 2. CFG construction (`cfg.cc`, 102 lines)

### 2.1 `Fun::build_cfg` (`cfg.cc:15`)

```c
void Fun::build_cfg() {
  if (ifa_verbose > 2) if1_dump(stdout, sym->code);
  if (!sym || !sym->code || (sym->code->is_group() && !sym->code->sub.n)) return;
  resolve_labels(sym->code);
  build_pn_cfg(pdb->if1, sym->code, NULL, NULL);
  entry = sym->code->pn;
  exit  = sym->code->sub[sym->code->sub.n - 1]->pn;
  finalize_cfg(this);
}
```

Three sub-steps:
1. `resolve_labels(code)` (`cfg.cc:48`) — walk the Code tree, setting
   `Label::code` to point back at each `Code_LABEL` Code.
2. `build_pn_cfg(...)` (`cfg.cc:64`) — convert Code tree to PNode graph.
3. `finalize_cfg(this)` (`cfg.cc:35`) — drop unreachable PNodes.

### 2.2 `build_pn_cfg` (`cfg.cc:64`)

The core converter. Takes a Code, its successor Code (`cont`), and a
"concurrent continuation" (`conc_cont`, unused unless
`CONC_IMPLEMENTED`). Behavior depends on whether the Code is a group:

**Non-group Codes (MOVE / SEND / IF / GOTO / LABEL):**
- Get-or-create the PNode (`code->pn = new PNode(code)`).
- Compute CFG successors by `Code_kind`:
  - `Code_IF`: two successors — `label[0]->code` (then) and
    `label[1]->code` (else). Both must be `Code_LABEL`s whose `code`
    back-pointers were set by `resolve_labels`.
  - `Code_GOTO`: one successor — `label[0]->code`.
  - Default (MOVE, SEND, LABEL): falls through to `cont` if `cont != NULL`.
- For each successor `s`, `s->cfg_pred.set_add(this_pnode)`.

**Group Codes (SUB / SEQ / CONC):**
- Iterate `sub` left-to-right. For child `i`, the `cont` is child `i+1`
  (via `get_cont(child[i+1], cont)`) or the outer `cont` for the last
  child.
- `get_cont(code, cont)` (`cfg.cc:58`) memoizes — for a non-group, the
  cont is the Code itself; for a group, recurse into `sub[0]`.
- After processing all children, set `code->pn = code->sub[0]->pn` so
  the group's "PNode" is its first child's. This makes the group
  transparent to outer iteration.

The recursion threads `cont` down so each PNode knows its fallthrough
successor without keeping a parallel stack.

### 2.3 `finalize_cfg` (`cfg.cc:35`)

After building, BFS from `entry` collects reachable PNodes; anything
not reached is unreachable. For each unreachable PNode, remove it from
its successors' `cfg_pred` lists. Then `set_to_vec` every PNode's
`cfg_pred` so they're proper Vecs (not just sets).

The walk uses `Fun::collect_PNodes(nodes)` which BFSes *backward* from
`exit` via `cfg_pred`, so anything not reachable from `exit` won't be in
`nodes` either — symmetric with `remove_unreachable`'s forward BFS from
`entry`.

### 2.4 `entry` / `exit` invariants

- `entry = sym->code->pn` — the first PNode in the function body.
- `exit = sym->code->sub[sym->code->sub.n - 1]->pn` — the last
  top-level statement's PNode. Always exists for a non-empty function.
- After `finalize_cfg`, the set of PNodes reachable forward from
  `entry` equals the set reachable backward from `exit`
  (`check_invariants` asserts this in DEBUG builds — under
  `#ifndef DEBUG`, see [IR.md](IR.md) §6.2 note).

### 2.5 `CONC_IMPLEMENTED`

The `Code_CONC` group kind and `conc_succ`/`conc_pred` PNode fields
exist for the V language's concurrency feature, but the `#define
CONC_IMPLEMENTED 1` line in `pnode.h:12` is commented out. The code
that fills `conc_succ`/`conc_pred` is `#ifdef`-guarded. If you turn it
on, expect to fix things — it's stale.

---

## 3. Dominators (`dom.cc`, 180 lines)

### 3.1 The data structure

```c
struct Dom : public gc {
  void *node;                     // back to PNode (or Fun for call-dom)
  Vec<Dom *> pred, succ;          // dom graph edges = CFG edges (rev for rdom)
  int dfs, semi, size;
  Dom *label, *parent, *child, *ancestor, *idom;
  Vec<Dom *> children;            // dom tree children (filled by make_dominator_tree)
  Vec<Dom *> bucket;              // Tarjan scratch
  Vec<Dom *> front;               // dominance frontier (filled by find_dominator_frontier)
  Intervals intervals;            // interval-encoded dominator path
};
```

Each `PNode` gets two `Dom`s:
- `PNode::dom` — forward dominators (over `cfg_succ`).
- `PNode::rdom` — reverse dominators (post-dominators, over
  `cfg_pred`).

### 3.2 Algorithm

Tarjan's "Fast Dominator Algorithm" (mentioned in `dom.cc:10`) with the
Cytron-et-al dominance frontier algorithm.

`build_dominators(d)` (`dom.cc:135`):
1. `df_traversal(d, 0, vertex)` — depth-first numbering. Each Dom gets
   `semi`, `label`, `parent` set.
2. `find_dominators(vertex)` — Tarjan core. Backward iteration to
   compute semidominators, then forward iteration to compute immediate
   dominators.
3. `make_dominator_tree(vertex)` — link each Dom into its idom's
   `children`.
4. `find_dominator_frontier(d, vertex)` — post-order walk computing
   `front` per Dom.
5. `make_dom_intervals(d)` — DFS-numbering plus interval-encoding
   ancestor sets in `Dom::intervals` for fast "is X dominated by Y?"
   queries via `is_dominated_by(n)`.

`Dom::is_dominated_by(n)` checks `intervals.in(n->dfs)` — O(1) after
build.

### 3.3 The `VNULL` sentinel

Tarjan's algorithm uses a virtual root. `vertex[0]` is `VNULL`. Before
running it, `dom_replace(x, NULL, VNULL)` substitutes the sentinel for
NULL pointers; after, `dom_replace(x, VNULL, NULL)` reverses it. This
lets the algorithm treat "no parent" as a real Dom with `semi = 0`
without checking for NULL on every step.

### 3.4 The two entry points

**`build_cfg_dominators(Fun *f)`** (`dom.cc:149`) — called by
`Fun::build_ssu` (so it runs as part of the implicit setup when a Fun
is constructed). Wires `Dom::pred`/`succ` from `cfg_pred`/`cfg_succ` for
both `dom` and `rdom`, then runs `build_dominators` on
`entry->dom` and `exit->rdom`.

After this, every PNode's `dom` and `rdom` are populated; SSU's `place_phi`
and `place_phy` then use `dom->front` and `rdom->front`.

**`build_call_dominators(FA *fa)`** (`dom.cc:170`) — called by
`find_recursive_loops`. Wires `Dom`s for whole *functions* in the call
graph (one Dom per Fun), then runs `build_dominators` from
`if1->top->fun->dom`. This drives the inter-procedural loop detection.

---

## 4. SSU form (`ssu.cc`, 205 lines)

### 4.1 What SSU is

Standard SSA gives each variable a single static *definition*. SSU
("single static use") extends the idea by giving each variable a single
static *use* as well. That requires *two* kinds of merge node:
- `phi` — at confluence points (CFG joins) — same as SSA.
- `phy` — at divergence points (CFG splits, in the reverse graph).

The paper that backs this is implied by the file name; SSU is used in
the IFA analysis to give symmetric treatment to flow forward (def → use)
and flow backward (use → def). The data-splitting in `IFA.md` §6.3
relies on the backward flow.

### 4.2 `phi` and `phy` storage

On each `PNode`:
- `phi : Vec<PNode*>` — MOVE PNodes that *logically follow* this PNode
  at a join point. Each phi PNode has one lval (the joined variable)
  and N rvals (one per `cfg_pred` of the join PNode).
- `phy : Vec<PNode*>` — MOVE PNodes that *logically precede* this
  PNode at a split point. Each phy PNode has one rval (the split
  variable) and N lvals (one per `cfg_succ` of the split PNode).

A phi/phy PNode is *not* in the main CFG — `cfg_pred`/`cfg_succ` are
unused on them. They're attached to a real PNode and walked
explicitly by code that needs them (e.g.,
`collect_Vars_PNode` in `fun.cc:100`).

### 4.3 `Fun::build_ssu` (`ssu.cc:174`)

```c
void Fun::build_ssu() {
  if (!entry) return;
  build_cfg_dominators(this);
  collect_Vars(vars, &pnodes);
  approximate_liveness(this, pnodes);
  for (Var *v : vars) if (v->sym->is_local) { v->ssu = new SSUVar; vrs.add(v); }
  for (PNode *p : pnodes) {
    for (Var *v : p->lvals) if (v->sym->is_local) v->ssu->defs.add(p);
    for (Var *v : p->rvals) if (v->sym->is_local) v->ssu->uses.add(p);
  }
  int phi = place_phi(vrs);
  int phy = place_phy(vrs);
  while (phi && phy) { phi = place_phi(vrs); phy = phi && place_phy(vrs); }
  rename_vars(this, pnodes);
  for (PNode *n : pnodes) {
    for (PNode *p : n->phi) for (Var *v : p->lvals) v->def = n;
    for (Var *v : n->lvals) v->def = n;
    for (PNode *p : n->phy) for (Var *v : p->lvals) v->def = n;
  }
  for (Var *v : vrs) v->ssu = 0;          // free SSU temp
}
```

`SSUVar` (`ssu.h:6`) is per-Var scratch:

```c
class SSUVar : public gc {
  Vec<PNode *> defs, uses;
  Vec<PNode *> phis, phys;
};
```

Only `is_local` Syms get SSU treatment. Globals, arguments,
constants — all pass through unchanged.

### 4.4 Liveness (`approximate_liveness`, `ssu.cc:73`)

Standard worklist: walk every PNode, union each successor's `live_vars`
into our own (minus what we kill via `lvals`), then add our `rvals`.
Iterate until no `live_vars` changes.

`live_vars` is overlaid in `PNode`'s union:

```c
union {
  LoopNode *loop_node;
  BlockHash<Var *, PointerHashFns> *live_vars;
};
```

So you can't have both at once. SSU sets `live_vars`; later passes
overwrite with `loop_node`. After SSU is done with it, `live_vars`
isn't cleared — it's just overwritten next time someone uses the union.
This is a memory-saving overlay; don't rely on `live_vars` after SSU.

### 4.5 `place_phi` (`ssu.cc:124`)

Classic SSA phi-placement (Cytron iterative algorithm). For each local
Var `v`:
- Worklist = `v->ssu->defs`.
- For each PNode `n` in the worklist:
  - For each `n->dom->front` Dom `d` (= dominance frontier PNode `y`):
    - If `y` doesn't already have a phi for `v` AND `v` is `maybe_live`
      at `y`:
      - Create phi PNode, add to `y->phi`.
      - Mark in `v->ssu->phis`.
      - Add `y->cfg_pred` to `v->ssu->uses` (each predecessor now uses
        `v` as an argument to the phi).
      - If `y` isn't already in `v->ssu->defs`, add it (the phi defines
        `v`); add to worklist.

### 4.6 `place_phy` (`ssu.cc:149`)

Symmetric phi for the reverse graph. For each local Var `v`:
- Worklist = `v->ssu->uses`.
- For each PNode `n` in the worklist:
  - For each `n->rdom->front` Dom `d` (= post-dominance frontier
    PNode `y`):
    - If `y` doesn't already have a phy for `v` AND `v` is `maybe_live`
      at `y`:
      - Create phy PNode, add to `y->phy`.
      - Mark in `v->ssu->phys`.
      - Add `y->cfg_succ` to `v->ssu->defs`.
      - If `y` isn't already in `v->ssu->uses`, add it; add to worklist.

### 4.7 The placement loop

```c
int phi = place_phi(vrs);
int phy = place_phy(vrs);
while (phi && phy) {
  phi = place_phi(vrs);
  phy = phi && place_phy(vrs);
}
```

Each placement may create new "definitions" or "uses" that trigger more
placements. Iterate until both report no changes. The condition `phi &&
phy` requires *both* to be progressing — if either stalls, the loop ends
(but the very next single-direction check below handles a final
imbalance).

### 4.8 `rename_vars` (`ssu.cc:115`)

After placement, each Var has multiple defs (the original + any phi).
SSA-style renaming assigns each def a fresh `Var(sym)` so each def is
unique:

1. Build `cfg_pred_index` per PNode (maps pred → its index in
   `cfg_pred`, needed for phi argument slot lookup).
2. `rename_edge(f, entry, env, nset)` — DFS over `cfg_succ` with an
   environment-stack `VarEnv` mapping `Var → renamed Var`.
3. At each PNode:
   - For each phi, allocate a fresh Var for its lval.
   - Rename rvals via `get_Var(sym->var, env)` — look up the current
     binding.
   - Rename lvals via `new_Var(sym->var, env)` — allocate fresh, push
     binding.
   - For each phy, rename its rval via `get_Var`.
   - For each successor, allocate a fresh Var for each phy lval at this
     successor's slot, plus install in successor's phi rvals at the
     right slot.
4. Push/pop the environment only at points with both multiple
   successors AND multiple predecessors (i.e., at true branches with
   alternative renaming on either side).

After this, each `Var` is the unique definition of its Sym at one
program point.

### 4.9 `Var::def` finalisation

After rename:

```c
for (PNode *n : pnodes) {
  for (PNode *p : n->phi) for (Var *v : p->lvals) v->def = n;
  for (Var *v : n->lvals)                       v->def = n;
  for (PNode *p : n->phy) for (Var *v : p->lvals) v->def = n;
}
```

Note `v->def` is the PNode that *contains* the definition — for phi/phy
lvals, it's the host PNode, not the phi/phy PNode itself. Code that
walks "where was `v` defined?" via `def` then needs to check `phi`/`phy`
to see whether the def is in a normal lval, a phi lval, or a phy lval.

`v->uses` is filled later by `build_uses(this)` (`fun.cc:48`), which
walks PNodes adding `n` to `v->uses` for every rval `v` and every phi/phy
rval `v`.

---

## 5. Loop detection (`loop.cc`, 171 lines)

### 5.1 The algorithm

Ramalingam's "Identifying Loops in Almost Linear Time" with the
Sreedhar-Gao-Lee modifications. The implementation in `loop.cc`
further adds:
1. Build a *tree of loops* (`LoopNode::children`, `parent`).
2. Handle single-node loops (`self_loop`).

### 5.2 The data structures

```c
struct LoopNode : public gc {
  int index;
  void *node;                  // back to PNode or Fun
  LoopNode *parent;
  Vec<LoopNode *> children;    // loop tree
  Vec<LoopNode *> loops;       // contained sub-loops?
  Vec<LoopNode *> pred, succ;  // graph edges
  Vec<LoopNode *> dom_children;
  int pre_dfs, post_dfs;
  int pre_dom, post_dom;
  uint processed : 1, in_worklist : 1;
};

struct LoopGraph : public gc {
  LoopNode *loops;             // root of loop tree
  LoopNode *entry;
  Vec<LoopNode *> nodes;
  Vec<Vec<LoopNode *> *> levels;
  UnionFind uf;
  // ...
};
```

UnionFind is shared across the graph; `unify(n, m)` merges loop members
into a single representative; `find(n)` retrieves it.

### 5.3 `find_loops` (`loop.cc:101`)

For each level (bottom-up):
1. For each node `x` at this level:
   - Worklist = back-edges `y → x` where `y` is both a DFS ancestor AND
     a dom ancestor.
   - If `x` is its own pred (self-loop), `self_loop(g, x)`.
   - Else `find_loop(g, x, worklist)`.
2. Same again for irreducible loops (DFS ancestor but NOT dom
   ancestor) on unprocessed nodes.

`find_loop` (`loop.cc:57`):
- Create a new `rep` LoopNode for this loop.
- Worklist-BFS from the back-edge source, collecting body nodes.
- For each body node, traverse `pred`; if a pred is *not* a DFS
  ancestor of the header, it's outside this loop — record that the
  outside loop contains this one (via `zr->loops.add(rep)`).
- `collapse(g, b, rep)` unifies all body nodes into `rep` and sets
  `parent`.

The loop tree comes from the second-to-last line:
```c
for (LoopNode *n : g->nodes) if (n->parent) n->parent->children.add(n);
```

### 5.4 Two flavors

**`find_local_loops(fa, f)`** (`loop.cc:132`) — per-Fun loop tree over
PNodes. Each PNode gets a LoopNode; edges from `cfg_pred`/`cfg_succ`;
dom-children from the per-PNode `Dom::children`. Sets `f->loops`.

**`find_recursive_loops(fa)`** (`loop.cc:149`) — global loop tree over
the call graph. One LoopNode per Fun; edges from
`Fun::calls_funs`/`called_by_funs`; needs `build_call_dominators(fa)`
first. Sets `fa->pdb->loops`.

`find_all_loops(fa)` (`loop.cc:168`) does both.

### 5.5 Who calls this

`frequency_estimation(fa)` (in `optimize/inline.cc`) calls
`find_all_loops(fa)` before doing per-loop frequency assignment.
That's the only call site as of this writing. The loop info is
consumed by:
- `simple_inlining` (uses `Fun::execution_frequency`).
- `local_loop_frequency_estimation` / `global_loop_frequency_estimation`
  in `inline.cc` (multiplies frequency by `LOOP_FREQUENCY = 10.0` per
  loop nesting level).

---

## 6. When does each pass run?

| Pass | Trigger | Notes |
|---|---|---|
| `Fun::build_cfg` | `Fun(Sym *)` ctor (`fun.cc:65`) | One-time per Fun. |
| `Fun::build_ssu` | `Fun(Sym *)` ctor (`fun.cc:66`) | Calls `build_cfg_dominators` internally. |
| `build_cfg_dominators(f)` | `Fun::build_ssu`; also `ifa_analyze` loop (`ifa.cc:36`) after cloning | Recomputed after cloning because clones get fresh PNodes. |
| `build_call_dominators(fa)` | `find_recursive_loops` | Once per analysis run. |
| `find_all_loops(fa)` | `frequency_estimation` (in `inline.cc`) | Called from `ifa_analyze` after cloning. |
| Re-run after CFG modification | `rebuild_cfg_pred_index(f)` (`fun.cc:256`) | Only refreshes the `cfg_pred_index` map; doesn't redo CFG. |

After cloning (in `clone.cc`), new Fun copies have fresh PNodes via
`Fun::copy → copy_pnode`, but `Fun::copy` does *not* rebuild the CFG —
it copies `cfg_succ/cfg_pred` and remaps via `f->nmap`. Then
`ifa_analyze` re-runs `build_cfg_dominators(f)` per Fun, which rebuilds
just the Dom trees over the new PNode set.

---

## 7. Gotchas

### 7.1 `entry`/`exit` requires non-empty body
`build_cfg` early-returns on an empty `is_group` Code with no `sub`.
Such a Fun has `entry = exit = NULL`. Code that walks PNodes must
check (`if (!entry) return;` is the standard idiom — see
`Fun::collect_PNodes`, `Fun::build_ssu`).

### 7.2 The `live_vars` / `loop_node` overlay
`PNode` has a union `{ LoopNode *loop_node; BlockHash *live_vars; }`.
SSU sets `live_vars`; loop detection sets `loop_node`. Reading the
wrong one after the other is set is undefined. If you need both
simultaneously, change the union to separate fields (and pay the
extra word per PNode).

### 7.3 `Code::pn` is a back-pointer set during CFG build
`Code::pn` is `0` initially and set to `new PNode(code)` by
`build_pn_cfg`. `Code::cont` is similarly cached by `get_cont`. Both
are scratch fields; if you re-run `build_cfg` (which you shouldn't,
but `Fun::copy` calls `copy_pnode` which doesn't touch these), you may
see stale values. Reset them yourself if you need to rebuild.

### 7.4 `cfg_pred` may have stale entries from unreachable preds
`finalize_cfg` calls `remove_unreachable` which removes unreachable
PNodes from their successors' `cfg_pred`. But unreachable nodes
themselves still exist in the PNode pool (the `Var::def` may point to
one). Code that walks `cfg_pred` from a reachable node is safe; code
that holds a raw PNode pointer needs to check.

### 7.5 phi/phy PNodes have empty `cfg_pred`/`cfg_succ`
They're attached to a host PNode and walked via `host->phi` /
`host->phy`. Don't put them in `cfg_succ`/`cfg_pred` lists.

### 7.6 `Var::def` semantics for phi/phy
For a Var whose definition is a phi or phy lval, `Var::def` is the
*host* PNode (the one with `host->phi` or `host->phy` containing the
defining MOVE), not the phi/phy PNode itself. Code that finds "the
definition" must check the host's phi/phy lists.

### 7.7 SSU rename allocates many Vars
Each definition (including phi/phy) becomes a fresh `Var`. A function
with 10 locals and 5 join points can easily produce 50+ Vars. They
all live in `Fun::fa_all_Vars` after analysis. Don't try to identify
"the same variable" by `Var::sym` post-rename; lots of Vars share a
Sym.

### 7.8 Tarjan dominator algorithm depends on DFS order
`build_dominators` assumes the input has a well-defined entry (`d`) and
that `df_traversal` from `d` reaches every node. If your graph has
unreachable nodes (which shouldn't happen after `finalize_cfg`), they
won't get `Dom` info — `dom`/`rdom` will be partially initialized but
`pred`/`succ` and `idom` will be NULL for unreached nodes.

### 7.9 `Intervals` requires a contiguous integer key
`Dom::intervals` is filled by `dom_build_intervals` which uses
`Dom::dfs` numbers. Reusing a `Dom` after the DFS numbering has changed
will give wrong `is_dominated_by` answers.

### 7.10 Loop detection assumes reducible plus irreducible separately
`find_loops` runs *two* passes per level — the first picks up
reducible loops (DFS+dom ancestor); the second picks up irreducible
loops (DFS ancestor only). The Sreedhar-Gao-Lee algorithm handles both
correctly only with both passes. If you optimize one away thinking it's
redundant, you'll miss irreducible loops.

### 7.11 `LoopNode::loops` vs `children`
`children` is the loop *tree* (parent contains child). `loops` is
the list of inner loops a node *contributes back-edges to* — used by
the algorithm during construction. Don't confuse them in client code;
`children` is what you usually want.

### 7.12 `g->loops` may be NULL
After `find_loops`, `g->loops = g->nodes.last()` — but if that node's
`node` field is non-NULL (i.e., a real PNode/Fun, not a synthetic loop
rep), then there are no loops and `g->loops = 0`. Code that walks
`g->loops` must check.

### 7.13 `find_recursive_loops` needs `Fun::calls` populated
`Fun::calls` is filled by `clone.cc:clone_functions` at the end of
cloning. `find_recursive_loops` reads `Fun::calls_funs(calls)`; if you
call it before cloning, you'll get an empty graph. The current call
chain (`ifa_analyze` → `frequency_estimation` → `find_all_loops`) is
post-clone.

---

## 8. Symptom → start-here

| Symptom | Start here |
|---|---|
| "CFG looks wrong" | `cfg.cc:build_pn_cfg` — verify `cont` threading for the parent group |
| "unreachable PNode crashing analysis" | `finalize_cfg`'s `remove_unreachable` — was the PNode added after CFG build? |
| "phi/phy missing for a join" | `place_phi`/`place_phy` and `approximate_liveness`; check the Var is `is_local` |
| "wrong rename of Var across branch" | `rename_edge` — env push/pop predicate is `cfg_succ.n != 1 && cfg_pred.n != 1` |
| "dominator wrong" | `build_dominators` — likely `df_traversal` didn't reach the node (check it's in the reachable forward set from entry) |
| "is_dominated_by wrong" | `dom_build_intervals` — DFS numbering may be stale |
| "loop not detected" | `find_loops` — verify back-edge is both DFS+dom ancestor (reducible) or DFS-only (irreducible second pass) |
| "self-loop not in tree" | `self_loop` is called *only* if a node is its own pred; check `cfg_pred` |
| "frequency_estimation crash" | `find_all_loops` needs post-clone Fun::calls; the loop_node/live_vars overlay collision |
| "Fun::copy lost CFG" | `copy_pnode` copies `cfg_succ/cfg_pred` but the targets need remapping via `nmap` (done by `Fun::copy` after `copy_pnode`) |
| "lvals[i] in phy stale" | rename pass updates these; check `rename_edge` iterated all successors |

---

## 9. References

- `ifa/optimize/cfg.cc` — CFG construction.
- `ifa/optimize/ssu.cc` + `ssu.h` — SSU form.
- `ifa/optimize/dom.cc` + `dom.h` — dominators.
- `ifa/optimize/loop.cc` + `loop.h` — loop detection.
- `ifa/if1/fun.cc` `Fun::Fun(Sym*)` — the implicit driver.
- Ramalingam, G. "Identifying Loops in Almost Linear Time" (1999).
- Cytron, Ferrante, Rosen, Wegman, Zadeck. "Efficiently Computing
  Static Single Assignment Form and the Control Dependence Graph"
  (TOPLAS 1991) — the dominance-frontier algorithm.
- Lengauer, T. & Tarjan, R. E. "A Fast Algorithm for Finding
  Dominators in a Flowgraph" (TOPLAS 1979) — the dom algorithm.
- Sreedhar, G., Gao, G., & Lee, Y. "Identifying Loops Using DJ
  Graphs" (TOPLAS 1996) — the loop-detection algorithm.
- Sister docs: [IR.md](IR.md), [IFA.md](IFA.md),
  [ARCHITECTURE.md](ARCHITECTURE.md).
