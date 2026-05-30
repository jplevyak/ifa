# OPTIMIZE — Dead-Code Elimination, Inlining, Frequency Estimation

A working reference for the post-analysis transformation passes in
`ifa/optimize/{dead,inline}.cc`. These run between IFA + cloning and
code generation.

Sister docs: [IFA.md](IFA.md) (produces the AVar/AType state DCE walks
backward through), [CLONE.md](CLONE.md) (concretises types so DCE has
concrete `Var::live` to operate on), [CFG_SSU.md](CFG_SSU.md)
(provides dominators + loop trees used here), [PIPELINE.md](PIPELINE.md)
(orchestration).

---

## 1. In one paragraph

After IFA + cloning, every `Var` has a concrete type and every call
site knows its targets. `mark_live_code` then propagates a "live"
bit backward through the IFA flow graph from observable
side-effects, marking PNodes/Vars/Funs reachable from any side effect
as live. `frequency_estimation` builds per-Fun and per-program loop
trees and assigns execution-frequency weights (10× per loop level)
that drive `simple_inlining`'s decisions. `simple_inlining` then
finds single-PNode functions and inlines them at their call sites,
plus collapses simple closure-create+closure-call pairs and identity
functions. `mark_live_funs` finally prunes the call graph by
transitive call reachability from `__main__`. The order is encoded in
`ifa_analyze` (after `clone()` → `build_cfg_dominators` →
`mark_live_code` → `frequency_estimation`) and `ifa_optimize`
(`mark_live_funs` → `simple_inlining` → `mark_live_types` →
`mark_live_funs` again).

---

## 2. The passes in order

```
                                                    Lives in
ifa_analyze (ifa/ifa.cc:23):
  ...
  build_cfg_dominators(f) for each f                optimize/dom.cc
  mark_live_code(fa)              ← DCE             optimize/dead.cc
  frequency_estimation(fa)        ← freq + loops    optimize/inline.cc
ifa_optimize (ifa/ifa.cc:53):
  mark_live_funs(fa)              ← transitive      optimize/dead.cc
  simple_inlining(fa)             ← inline pass     optimize/inline.cc
  mark_live_types(fa)             ← type DCE        optimize/dead.cc
  mark_live_funs(fa)              ← again           optimize/dead.cc
```

`build_cfg_dominators` and the per-Fun `Fun::loops` setup are
prerequisites — DCE uses `dom`/`rdom` for IF/GOTO liveness, and
frequency estimation needs the loop tree. See
[CFG_SSU.md](CFG_SSU.md).

---

## 3. Dead-code elimination (`dead.cc`, 318 lines)

The comment at the top of the file flags that the file is intended to
host *two* implementations (AVar-based and Var-based). Only the
AVar-based one is implemented; the Var-based version exists as a
half-finished `#if 0` block at the end (`dead.cc:236`).

### 3.1 `mark_live_code(fa)` (`dead.cc:198`)

The entry point. Three phases:

```c
int mark_live_code(FA *fa) {
  mark_initial_dead_and_alive(fa);    // seed
  do {
    mark_live_again = 0;
    mark_live_avars(fa);              // forward / backward propagation
    mark_live_pnodes(fa);             // PNode-level liveness
  } while (mark_live_again);
  mark_live_types(fa);                // CreationSet::type→type_live
  return 0;
}
```

`mark_live_again` is a module-level static. Any propagation step that
flips a bit sets it to 1; the loop continues while progress is being
made.

### 3.2 `mark_initial_dead_and_alive(fa, init = 0)` (`dead.cc:166`)

Seed pass. For each Fun:
- `f->live = init` (default 0).
- For each `Var`: `v->constant = get_constant(v)` (post-analysis
  constant value if known); reset `v->live = init`; reset all
  `AVar::live` to false; for each instance-variable AVar of `v`'s
  type's creators, also reset.
- For each PNode: `p->live = init`. **Then** check if it's a
  `(sym_primitive, name, ...)` SEND whose registered prim has
  `is_visible = 1` — if so, set `p->live = 1` (visible primitives
  like `__pyc_c_call__` are side effects and seed liveness directly).

This is the *only* place visible registered primitives become live
seeds. Frontends that emit user-callable primitives must mark them
`is_visible = 1` or they'll be DCE'd.

### 3.3 `mark_live_avars(fa)` (`dead.cc:63`)

For each live PNode that isn't a call (`f->calls.get(p) == nullptr`):
- For each non-constant `rval` Var, mark each of its AVars live via
  `mark_live_avar`.

Additionally, for each non-constant Var: if any of its type's
CreationSets has a live instance variable, mark the Var live (the
containing object must exist if its ivar matters).

### 3.4 `mark_live_avar(av)` (`dead.cc:52`)

```c
static void mark_live_avar(AVar *av) {
  if (av->var->sym->is_fake) return;
  av->live = 1;
  av->var->live = 1;
  mark_live_again = 1;
  if (av->var->def) av->var->def->live = 1;
  for (AVar *aav : av->backward)
    if (aav && !aav->live && !get_constant(aav))
      mark_live_avar(aav);
}
```

Recursive backward propagation along the IFA flow graph. Marking an
AVar live transitively marks its *defining* PNode live and every AVar
it reads from. Constants don't propagate (they're free; their defs
don't matter).

`is_fake` Syms (e.g., pyc's `sym_declare`) are skipped — they're
declaration markers, not real runtime values.

### 3.5 `mark_live_pnodes(fa)` (`dead.cc:109`)

The PNode-level liveness driver. For each non-live PNode in each Fun:

- **If any lval is live** → mark live.
- **`Code_LABEL`** with `cfg_pred.n != 1` → live if any pred is live
  (multi-pred LABELs are join points; can't DCE).
- **`Code_GOTO`** whose successor has `cfg_pred.n != 1` → live if any
  PNode it dominates is live or is exit (potentially expensive O(n²)
  walk).
- **`Code_SEND`** with a primitive:
  - `P_prim_reply` → live if any of `f->rets` is live.
  - `P_prim_setter` / `P_prim_set_index_object` → live if
    `forward_live(p->tvals[0])` (within distance 2).
- **`Code_SEND`** without primitive (user call) → live if any callee
  Fun is live.
- **`Code_IF`** → live if any live PNode `x` dominated by `p` is NOT
  dominated by any of `p`'s successors (i.e., the IF actually
  branches between live regions).

After the switch, set `p->live = 1` and `f->live = 1`. If the PNode
was already live but `f->live` wasn't, set it.

### 3.6 `forward_live(v, dist = 2)` (`dead.cc:100`)

Recursive forward walk through AVar `forward` edges. Used to check if
a setter's stored value reaches a live use within 2 hops. Bounded to
avoid quadratic blowup on long flow chains.

### 3.7 `mark_live_types(fa)` (`dead.cc:189`)

For each CreationSet `cs`:
- Clear `cs->type->type_live = 0`.
- For each creator in `cs->type->creators`:
  - For each AVar def of that creator, if `av->var->live`, set
    `type_live = 1`.

So `Sym::type_live` is "this type has a live constructing AVar
somewhere." Used by codegen to skip emitting struct definitions for
types no live code constructs.

### 3.8 `mark_live_funs(fa)` (`dead.cc:210`)

Transitive call-reachability from `__main__`:

```c
void mark_live_funs(FA *fa) {
  for (Fun *f : fa->funs) f->live = 0;
  if1->top->fun->live = 1;
  while (changed) {
    for (Fun *f : fa->funs) if (f->live)
      for (PNode *p : f->fa_all_PNodes) if (p->live)
        for (Fun *x : *f->calls.get(p))
          if (!x->live) { x->live = 1; changed = 1; }
  }
  // Compact fa->funs to keep only live Funs
  Vec<Fun *> funs(fa->funs, MOVE);
  for (Fun *f : funs) if (f->live) fa->funs.add(f);
}
```

Reseeds `__main__` (`if1->top->fun`) as live; iterates until no new
Fun gets marked live by an existing live Fun's live PNode's `calls`.
Final loop compacts `fa->funs` to drop dead Funs.

`mark_live_funs` is called *after* `simple_inlining` plus a second
time at end of `ifa_optimize` — the inliner can make a Fun
unreachable by inlining its only call site, and the second pass
removes such Funs from `fa->funs`.

### 3.9 `print_dead(fa)` (`dead.cc:24`)

When `ifa_verbose > 2`, write `<fn>.dead_log` listing every dead PNode
and dead Var with source location, plus totals. Useful for debugging
why something got DCE'd or kept.

---

## 4. Frequency estimation (`inline.cc:12-73`)

Statically estimate per-PNode and per-Fun execution counts. The
estimate is crude (loops are 10× per level) but sufficient for
inlining decisions.

### 4.1 `frequency_estimation(fa)` (`inline.cc:68`)

```c
int frequency_estimation(FA *fa) {
  find_all_loops(fa);                                 // optimize/loop.cc
  for (Fun *f : fa->funs) local_frequency_estimation(f);
  global_frequency_estimation(fa);
  return 0;
}
```

Three sub-passes:
1. **`find_all_loops(fa)`** — builds the per-Fun `LoopGraph` and the
   global call-graph `LoopGraph`. See
   [CFG_SSU.md](CFG_SSU.md) §5.
2. **`local_frequency_estimation(f)`** — per-Fun PNode frequencies.
3. **`global_frequency_estimation(fa)`** — propagate down call tree.

### 4.2 `local_frequency_estimation(f)` (`inline.cc:31`)

Walks the per-Fun loop tree, multiplying frequency by
`LOOP_FREQUENCY = 10.0` per nesting level. PNodes outside any loop
default to `1.0`.

`local_loop_frequency_estimation(l, f)` (`inline.cc:22`) recurses
through the LoopNode tree; leaf LoopNodes' `node` is the PNode (set
its `execution_frequency = f`); internal LoopNodes have `node == 0`
and recurse with `f * LOOP_FREQUENCY`.

### 4.3 `global_frequency_estimation(fa)` (`inline.cc:48`)

Similar but at call-graph granularity:

1. Walk the global loop tree, assign each `Fun::execution_frequency`
   by 10× per call-graph loop level.
2. DFS-order all Funs from `__main__` via `dfs_order` (so callers
   come before callees).
3. For each Fun in DFS order:
   - Multiply every PNode's frequency by the Fun's frequency.
   - Reset Fun's frequency to 0; then sum up: `freq * pnode->execution_frequency`
     for each call site that calls *this* Fun, **only** counting
     call sites whose Fun is a DFS ancestor in the loop graph (i.e.,
     the loop relationship is forward, not back-edge).
   - Clamp to ≥ 1.0.

The DFS-ancestor filter prevents recursive calls from inflating
frequency without bound. The result: each Fun's `execution_frequency`
is "how many times this Fun is expected to run in one invocation of
`__main__`," and each PNode's is "how many times this exact
operation executes."

These numbers are *static estimates*. There's no profile-guided
backing — see the CDB notes in [IFA.md](IFA.md) §11.5 for the
intended (but inert) compilation-database feature that would replace
them with real profile data.

---

## 5. Simple inlining (`inline.cc:75-330`)

Inlines:
- Single-primitive-SEND functions at their call sites.
- Identity functions (return one arg unchanged).
- Closure-create immediately followed by closure-call (collapses to
  a direct call).

### 5.1 `simple_inlining(fa)` (`inline.cc:327`)

```c
int simple_inlining(FA *fa) {
  inline_single_sends(fa);
  return 0;
}
```

Single-entry, single-exit. Just calls `inline_single_sends`.

### 5.2 `inline_single_sends(fa)` (`inline.cc:230`)

Two phases.

**Phase 1: catalog inlinable Funs.**

For each Fun `f`, walk its PNodes looking for exactly one
significant SEND (`MOVE` and closure-creates don't count, and a
`P_prim_reply` is allowed as a separate "reply" PNode):
- If we find more than one significant SEND, bail (mark `p =
  f->exit`).
- If we find zero significant SENDs *but* have a `reply`, check if
  the reply's value reaches back to any formal argument — if so, it's
  an *identity function* on that argument. Record in `identity_send[f]
  = i+1` (offset by 1 to distinguish from "not identity").
- If we find exactly one SEND, check its rvals are all either formals,
  constants, or symbols. If so, record `single_send[f] = the_pnode`.

The "single send" must be a primitive (`p->prim` set) and not itself
a call. The reply must reach the SEND's result (`reaching_def`
check).

**Phase 2: inline at call sites.**

For each Fun `f`, walk PNodes looking for inlinable calls:
- If `p` is a non-closure-call SEND with exactly one callee `fn`:
  - If `fn` is in `single_send` → `inline_single_pnode(f, p, fn, s)`.
  - If `fn` is in `identity_send` → `convert_to_move(p, i-1)` (turn
    the SEND into a MOVE of the i-th argument).
- Otherwise, check for `simple_closure_call(p)` — a closure-call
  preceded by a simple closure-create. If matched:
  - Deactivate the closure-create PNode (`c->live = 0`).
  - Splice the create's rvals into the call's rvals.
  - If the closure-create was a period operator, reorder so the
    method receiver is at the right slot.
  - Then apply the single-send / identity inlining as above.

After both phases, re-collect Vars/PNodes per Fun (`f->collect_Vars`)
to refresh `fa_all_Vars` / `fa_all_PNodes`.

**Phase 3: constant substitution.**

For each live PNode, replace each rval with its constant value if
known (`sub_constants`). This rewrites Vars that DCE marked constant
into literal constant Vars.

### 5.3 `inline_single_pnode(f, p, fn, s)` (`inline.cc:187`)

Replace the call `p` with the body PNode `s` from `fn`:
- `p->prim = s->prim` (take over the primitive identity).
- `f->calls.put(p, fn->calls.get(s))` (inherit nested call targets).
- For each rval `v` of `s`:
  - If constant, push the constant Var.
  - Else find which formal it corresponds to (via `first_var(v)`),
    look up the matching actual arg from the original call's rvals.
  - If actuals' types match, push directly.
  - Otherwise create a typed wrapper Var, `insert_move_before` to
    inject a MOVE that retypes, then push the wrapper.

After this, `p` *is* what `s` was, but with the caller's args.

### 5.4 `simple_closure_call(p)` (`inline.cc:88`)

Pattern-matches "the most recent CFG predecessor (skipping MOVEs of
the closure Var) is a closure-create whose lval is what we're now
calling." Returns the create PNode if so; null otherwise. The check
`is_simple_closure_create(p, false)` verifies the create's lval is
used exactly once and only by this MOVE chain.

### 5.5 Helpers

- `first_var(v)` (`inline.cc:114`) — walk `v->def` through MOVEs to
  the original source Var (does NOT cross phi/phy).
- `reaching_def(v, p)` / `reaching_var(v, vv)` (`inline.cc:139,153`) —
  worklist walk back through MOVE defs to check if `p` (or `vv`)
  is reachable.
- `insert_move_before(f, p, rhs, lhs)` (`inline.cc:167`) — splice a
  new MOVE PNode into the CFG immediately before `p`, rewiring
  `cfg_pred`/`cfg_succ`.
- `convert_to_move(p, i)` (`inline.cc:223`) — in-place replace `p`'s
  Code with a MOVE of `p->rvals[i]`.

---

## 6. Order matters

`ifa_optimize` (`ifa.cc:53`) does:

```c
int ifa_optimize() {
  mark_live_funs(fa);
  if (simple_inlining(pdb->fa) < 0) return -1;
  mark_live_types(pdb->fa);
  mark_live_funs(pdb->fa);
  return 0;
}
```

This is the *only* place the pass order is set. Don't reorder
without understanding:

1. **`mark_live_funs` first** — prunes `fa->funs` to call-graph-
   reachable Funs. Inliner only operates on live Funs.
2. **`simple_inlining`** — inlines single-SEND functions. May make
   some Funs unreachable (their only call site was inlined).
3. **`mark_live_types`** — recompute `Sym::type_live` after
   inlining changed which AVars are live.
4. **`mark_live_funs` again** — re-prune `fa->funs` after inlining.

The first `mark_live_code` happens earlier, in `ifa_analyze`, before
`ifa_optimize` runs. It establishes the initial `Var::live` /
`PNode::live` bits that the inliner consults.

---

## 7. The other (`#if 0`) implementation

`dead.cc:236-318` contains a Var-based DCE implementation under
`#if 0`. The intent (per the top-of-file comment) is to run *post*
`concretize_program()` — using `Var::live` directly without going
through AVars. Currently dead code; turn it on at your peril (the
internal helpers reference `pdb->fa->primitive_transfer_functions`
which doesn't exist as a field).

---

## 8. Gotchas

### 8.1 `is_fake` Syms skipped from liveness
`mark_live_avar` returns early on `av->var->sym->is_fake`. pyc uses
this for `sym_declare` (the `__pyc_declare__` marker). If you add a
new pseudo-symbol that shouldn't propagate liveness, set `is_fake`.
If you forget, the marker will pull in random things.

### 8.2 `is_visible` registered prims are liveness seeds
`mark_initial_dead_and_alive` marks `(sym_primitive, name, ...)`
SENDs live if the registered prim's `is_visible` is set. This means
forgetting `is_visible = 1` causes your prim's SEND to be DCE'd.
Conversely, marking too many registered prims visible prevents
useful DCE.

### 8.3 The IF-liveness check is O(n²)
`mark_live_pnodes`'s `Code_IF` case walks every live PNode in the Fun
to find one dominated by `p` but not by any successor. For Funs
with thousands of PNodes this is slow; in practice the loop runs
once per pass and most Funs are small.

### 8.4 `forward_live` distance limit
`forward_live(v, dist = 2)` caps the walk at distance 2 from the
starting Var. This is the only place a setter's "is this store
observable?" check lives. Stores reaching a live observer beyond 2
hops will be incorrectly DCE'd. The bound is a precision/cost knob;
raising it is safe but slow.

### 8.5 `simple_inlining` can leave dangling Vars
After phase 2 splices new rvals into PNodes, `Var::uses` may
reference now-dead PNodes. `f->collect_Vars` is called per Fun to
re-scan, but downstream passes that walk `Var::uses` directly need
to check `use->live`. Most callers do this; new code should too.

### 8.6 Identity function detection is conservative
`reaching_var(reply_val, formal_var)` walks only through MOVEs (not
phi/phy). So a function that returns its argument *through a
join point* won't be recognised as identity. Usually fine in
practice; the SSU rename produces single-MOVE chains for simple
cases.

### 8.7 `sub_constants` runs after inlining
The post-inlining constant-substitution pass produces Vars whose
`sym` is a constant Sym (`is_constant`). Downstream passes (codegen)
should emit the constant value directly via `cg_string` rather than
expecting a runtime computation.

### 8.8 `LOOP_FREQUENCY = 10.0` is a constant magic number
The 10× per loop level is a heuristic. It's defined in `inline.cc:12`.
If profile data ever feeds in (via CDB or similar), this constant
becomes irrelevant.

### 8.9 `Fun::execution_frequency` is reset mid-pass
`global_frequency_estimation` reuses the field — first as loop-level
input, then computed as outgoing frequency. Don't read it before the
pass completes if you're concurrent with it.

### 8.10 `mark_live_funs` mutates `fa->funs`
The end of `mark_live_funs` rebuilds `fa->funs` by moving the old
vector out and re-adding only live ones. If you hold a `Vec<Fun*>` or
iterator into `fa->funs` across this call, it's invalidated.

### 8.11 `frequency_estimation`'s prerequisites
Needs:
- `Fun::calls` (post-clone, populated by `clone_functions`).
- `Fun::called` (populated alongside `calls`).
- `Fun::loop_node` / `f->loops` (populated by `find_all_loops`).
- `fa->pdb->loops` (the global loop graph).

If any of these are missing (e.g., calling pre-clone), the pass
silently produces wrong frequencies. The current `ifa_analyze` ordering
satisfies all prerequisites.

### 8.12 `inline_single_sends` requires single-callee call sites
Inlining only triggers when `f->calls.get(p)` returns exactly one
Fun. Polymorphic call sites (multiple targets) are skipped — the
analysis-then-clone pipeline tends to monomorphise most calls, so
this misses fewer opportunities than it sounds like.

---

## 9. Symptom → start-here

| Symptom | Start here |
|---|---|
| "side-effecting call DCE'd" | `is_visible` on registered prim, or `nonfunctional` flag on table prim |
| "dead branch still in output" | `mark_live_pnodes` `Code_IF` case — may have a spurious live PNode in the dominated region |
| "constant not folded into rval" | `sub_constants` runs only after inlining; check `Var::constant` was set by `mark_initial_dead_and_alive` |
| "function inlined when it shouldn't be" | `inline_single_sends` `single_send` catalog — verify the body has more than one significant SEND |
| "function NOT inlined when it should be" | `inline_single_sends` skip conditions: check the call has exactly one target, the body's rvals are formals/constants/symbols only |
| "wrong frequency in inlining decision" | `global_frequency_estimation` — DFS ancestor filter prevents back-edge counting |
| "Fun in `fa->funs` after clearly unreachable" | `mark_live_funs` not run after the change; or `__main__` was reset and lost the seed |
| "type missing from codegen" | `mark_live_types` — no CreationSet of that type had a live AVar def |
| "closure not collapsed" | `simple_closure_call` / `is_simple_closure_create` — the lval must be used exactly once via a MOVE chain |
| "inlined call type mismatch crash" | `inline_single_pnode` — `actuals[i]->type != formals[i]->type` branch; the typed wrapper insertion may be wrong |

---

## 10. References

- `ifa/optimize/dead.cc` — DCE.
- `ifa/optimize/inline.cc` — inlining + frequency estimation.
- `ifa/optimize/dead.h` / `inline.h` — public API.
- `ifa/ifa.cc:ifa_analyze` and `ifa_optimize` — the orchestration.
- Sister docs: [IFA.md](IFA.md) (AVar liveness graph),
  [CLONE.md](CLONE.md) (post-clone state required),
  [CFG_SSU.md](CFG_SSU.md) (dominators + loop trees),
  [PRIMITIVES.md](PRIMITIVES.md) (`is_visible` / `nonfunctional` flags).
