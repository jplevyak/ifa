# IFA Analysis Notes and Architecture

## Part 1: Architecture and Non-Obvious Aspects

### 1. Analysis Lifecycle and Convergence Protocol

IFA (*Iterative Flow Analysis*) is a simultaneous data and control flow analysis based on abstract interpretation against a type-value lattice.

#### 1.1 Outer Passes and State Persistence
An analysis run consists of alternating phases of constraint propagation to fixed point (`analyze_to_convergence`) and demand-driven contour splitting (`run_split_stages`):
- **Within a pass**: `analyze_to_convergence` drains worklists (`edge_worklist`, `send_worklist`, `es_worklist`) until fixed point.
- **Between passes**: `clear_results` and `clear_avar` wipe derived AVar state (`in`, `out`, `restrict`, `forward`, `backward`, `arg_of_send`) so values are re-derived from bottom on the next pass.
- **What persists across passes**:
  - `AVar::cs_map`: Tracks the decision of which CreationSet an allocation site maps to.
  - `AVar::container`: Container parent link.
  - `AVar::match_cache`: Method resolution cache, validated against canonical ATypes.
  - `CreationSet::vars`: Positional slots for container elements and closures. **Invariant (issue 030):** A CreationSet's positional `vars[i]` must be fed by every pass that feeds the CS, regardless of which Var carries the value in that pass.
  - Split ledger and contour identities (`EntrySet`, `CreationSet`).
  - Canonical hash-cons tables and types in `TypeWorld`.

#### 1.2 Cross-Pass Type Movement (`typemove`)
A pass does not necessarily reach a final whole-program fixed point just because no splitter stage split a contour. Decisions from prior splits (`av->cs_map`, split ledger) persist across passes, so successive passes re-derive different types even when contour counts remain unchanged (issue 148).
- `typemove_enabled()` hashes the positional formal types across all `EntrySet`s at the end of each pass.
- If derived types moved (`h != prev_state_hash`), the analysis runs another pass (`analyze_again = 1`) even if no contour was split. This carries the analysis across plateaus to subsequent splitting work.

#### 1.3 Demand Evaluation at Quiescence
- **All demand is evaluated at quiescence**: A demand is a property of converged types ("this AVar holds a union that blocks operation X"), evaluated at fixed point, never on the transient formation of a union during worklist draining (issue 157).
- **Demand-driven, not structural**: A contour is never split because a surrounding contour split. EntrySets start minimal (one per function). CreationSets start merged (one per `Sym`, `PYC_CSDCPA1=2` by default; see issues 128, 129, 146). Splits occur strictly on demand (e.g. unresolvable dispatch, irrepresentable element union).

---

### 2. Constraints, Primitives, and Return Paths

#### 2.1 `prim_reply` Lifecycle at Setup vs. Fixed Point
`add_send_constraints` runs once during constraint setup when an `EntrySet` is initialized, not per fixed-point iteration.
- Inside `P_prim_reply`, inspecting `r->out` (`fn->ret`'s AVar) reflects state **at setup time** (often bottom or partially populated), not at fixed point.
- To inspect converged return types, check post-convergence structures (e.g. `fa->funs` or `-v -v -v` verbose dumps), or inspect after `set_void_lub_types_to_void()`.

#### 2.2 Shared Python Return Paths
`PY_return_stmt` lowers in IF1 as:
```text
if1_move(val, fn->ret);
goto label[0];
...
label[0]:
prim_reply(fn->cont, fn->ret);
```
All Python `return` statements in a function funnel into the single `fn->ret` Var (one AVar per `EntrySet`). Consequently:
- `fn->ret`'s backward chain is the union of all return-source AVars.
- At setup time, `backward.n` reflects the number of return sites; at fixed point, SSU and phi-merges may consolidate them.

#### 2.3 Monotonicity and Re-trigger Registration
Transfer functions for primitives (e.g. `P_prim_period`, `P_prim_isinstance`, `P_prim_index_object`) iterate `operand->out->sorted` at execution time.
- Monotonicity holds because `add_send_edges_pnode` registers every rval as `arg_of_send.add(result)`.
- When an operand's `out` set receives new CreationSets later in the fixed point, the send's result is re-enqueued on `send_worklist` and re-evaluated.

#### 2.4 Constant Folding in Emitted C Codegen
When an AVar resolves to a single CreationSet holding an immediate constant, codegen inlines the literal directly at the call site rather than emitting a runtime struct read. For example, `_CG_prim_add(5, "+", t4)` in generated C indicates that `n.value` constant-folded to literal `5` during flow analysis.

---

### 3. Discriminator-Based Narrowing (`Code_IF`)

Narrowing refines types on conditional branches (e.g., `if x is not None:` or `if isinstance(x, Cls):`).

#### 3.1 Predicate-Based Narrowing vs. Stale Snapshots
- **Historical pitfall (issue 025/026):** Narrowing originally partitioned `operand->out->sorted` into two static `AType` snapshots at constraint-setup time and stamped them into branch-local `restrict` sets. Any CreationSet arriving at `operand->in` later during fixed-point iteration was permanently dropped by the stale snapshot.
- **Current architecture:** `AVar` holds `restrict_pred` (`AVarRestrictPred` enum: `RP_None`, `RP_NotNone`, `RP_IsInstance`, `RP_NotIsInstance`) and `restrict_pred_cls`.
- `update_in` dynamically evaluates `apply_restrict_pred` on `v->in ∩ restrict`. As new CreationSets arrive at the operand, the predicate continuously re-evaluates.
- **Adding new narrowing predicates:**
  1. Add new `RP_*` enum in `fa.h`.
  2. Handle predicate in `restrict_pred_keeps()` in `fa.cc`.
  3. Install via `flow_var_permit_pred(lv, RP_*, cls)` in `Code_IF` handler in `fa.cc`.

#### 3.2 SSU Branch Variables and Wrapper Peeling
- Condition branches in IF1 use SSU-renamed variables (`v_v1` on True branch, `v_v2` on False branch). Narrowing applies to these branch-local AVars.
- `flow_var_type_permit` growth: If called again with additional CreationSets, `restrict` grows by union (`type_union`).
- **Wrapper peeling (`peel_wrapper_def`):** Python lowers `if cond:` as:
  ```text
  SEND1: t = cond_op(...)              ; discriminator
  SEND2: m = operator cond_op . __pyc_to_bool__
  SEND3: bool_cond = m()
  IF bool_cond
  ```
  `peel_wrapper_def` traverses MOVE and SEND chains to identify the underlying discriminator PNode. It is depth-bounded to 6 hops to protect against cyclic or excessively long chains.

#### 3.3 Narrowing and Inlining Knobs
Two flags isolate the precision sources that interact at `Code_IF`:
| Flag | Environment Variable | Default | Purpose |
|---|---|---|---|
| `--narrow N` | `IFA_NARROW` | 1 | Controls discriminator recognition and per-branch narrowing in `Code_IF`. |
| `--fa_inline N` | `IFA_FA_INLINE` | 0 | Runs `mark_live_funs` + `simple_inlining` between FA convergence passes. |

---

### 4. Contour Identity and Discrimination

#### 4.1 Contour Discriminator Bit
`AVar::contour_is_entry_set` (1 bit) distinguishes the type of `av->contour`:
- `1`: `(EntrySet *)av->contour` (function contour).
- `0`: `(CreationSet *)av->contour` (data contour) or `GLOBAL_CONTOUR`.

#### 4.2 `GLOBAL_CONTOUR` Sentinel
`GLOBAL_CONTOUR` is defined as `((void *)::fa->global_es)`:
- Backed by `FA::global_es`, a real distinguished EntrySet created during `FA::analyze`.
- It is never registered in `fa->ess`, has no pnodes, and has `in_es_worklist = 1` so standard worklist enqueue checks safely ignore it.
- Dereferencing `(EntrySet *)av->contour` on a global AVar is safe; pointer equality `av->contour == GLOBAL_CONTOUR` tests for global scope.

#### 4.3 Class Instantiation Contours (`Node(v)`)
Instantiation `Node(v)` dispatches through `__new__` and `__init__`:
- Call sites with differing argument shapes may create multiple `__new__` EntrySets (visible in `[clone]` traces).
- This does **not** imply that the callee function itself splits into multiple EntrySets. Recursive functions (e.g. `insert` in a BST) called from multiple sites with compatible argument types remain in a single EntrySet.

#### 4.4 Lexical Display Decoupled from Contour Identity
`EntrySet::display` is strictly for resolving enclosing-scope variables in closures (`make_AVar` resolving `v->sym->nesting_depth`).
- Lexical displays were completely removed from contour compatibility checks (`entry_set_compatibility`, `split_edges`, etc.) in issue 100.
- Contours are partitioned solely on types and demand, never lexical displays or allocation provenance.

#### 4.5 Sentinel Pointers in Cloning (`clone.cc`)
`clone.cc` uses specific sentinel pointer values to represent failure states or multiple definitions:
- `BAD_NAME = (char *)-1`: Name conflict during concrete type naming.
- `BAD_AST = (IFAAST *)-1`: AST conflict across clones.
- `(Sym *)-1`, `(Sym *)-2`: Basic type failure tokens.
- `(AVar *)-1`: Multiple distinct definitions encountered.

---

### 5. Cloning Pipeline (`clone.cc`)

`clone.cc` executes after flow analysis converges. It performs monomorphisation:
- **Single-pass mutation**: Traverses the converged EntrySets and CreationSets, instantiating concrete `Fun` and `Sym` clones.
- Writes `cs->type`, `av->type`, `Fun::calls`, `Fun::called`.
- Relies on stable iteration over `fa->ess` and `fa->css` (sorted by id in `collect_results`).

---

### 6. State Ownership and Reentrancy Model

Historical module-level static variables have been refactored into structured member ownership:
- **`class FA`**: Owns per-analysis worklists (`edge_worklist`, `send_worklist`, `es_worklist`), completion vectors (`entry_set_done`), `type_violations`, and id counters (`avar_id`, `aedge_id`, `creation_set_id`, `entry_set_id`).
- **`class TypeWorld`** (owned by `FA`): Owns the canonical hash-cons tables (`cannonical_atypes`, `cannonical_setters`, `type_fold_cache`, `type_violation_hash`) and standard canonical types (`bottom_type`, `void_type`, `bool_type`, etc.).
- **Process Singletons**: Global pointers `FA *fa` and `PDB *pdb` remain singletons. Multi-instance concurrency or embedding without global state is deferred (see [ifa/notes/005-singleton-fa-and-pdb.md](../notes/005-singleton-fa-and-pdb.md)).

---

### 7. Recursion Surface Area

Several core algorithms in `fa.cc` are implemented recursively over the program graph:
| Function | Traversal | Depth Bound / Termination |
|---|---|---|
| `update_in` | `v->forward` (data-flow edges) | Data-flow graph diameter |
| `add_pnode_constraints` | `p->cfg_succ` (CFG successors) | Function CFG depth |
| `update_setter` | `av->backward` (data-flow edges) | Data-flow graph diameter |
| `build_type_mark` | `av->forward` | Flow graph diameter |
| `build_setter_mark` | `av->backward` | Flow graph diameter |
| `destruct` | Pattern unpack nesting | `t->has` nesting depth |
| `peel_wrapper_def` | MOVE / SEND def chain | Explicit bound (`max_depth = 6`) |

*Note:* For standard and adversarial inputs, graph diameters are normally within stack limits. Deeply linear, un-optimized machine-generated code could challenge stack limits; recursion is retained as the clearest expression of these algorithms.

---

## Part 2: Issues Which Might Need to Be Addressed

This section focuses strictly on open issues and active capability gaps for review. Closed/resolved bugs and historical cleanup items from earlier audits have been removed.

### 1. Active Issues in `ifa/issues/` Affecting Analysis

- **Vec Set API Cleanup ([ifa/issues/010](../issues/010-CLEANUP-vec-set-api-cleanup.md))**:
  Migrating the remaining ~17 `qsort_by_id` sites in `fa.cc` to non-mutating `sorted_view`, renaming `Vec::n` to `capacity()` and adding `size()`.
- **Intra-Function Union Narrowing & SSU Rewrites ([ifa/issues/025](../issues/025-FA-intra-function-union-narrowing.md))**:
  Per-branch SSU AVars (`v_v1`, `v_v2`) are successfully narrowed by `Code_IF`, but pyc's strict no-boxing default can emit `BOXING` violations on the original Var `v` before the narrowed views can gate downstream uses. Needs liveness-aware box-checking or SSU rewrite-and-prune.
- **Permissive-Mode Voiding of NOTYPE ([ifa/issues/049](../issues/049-FA-raise-only-contour-notype.md), [ifa/issues/124](../issues/124-FA-refuse-imprecise-inference.md), [ifa/issues/145](../issues/145-numeric-coercion-is-not-gated-on-permissive-mode.md))**:
  `convert_NOTYPE_to_void()` silently salvages bottom-typed AVars under `fruntime_errors` (pyc default) by converting them to `void`, producing `return 0` clones rather than failing loudly. IFA should refuse imprecise inference or gate salvage strictly behind permissive runtime error flags.
- **Splitter Stage Cascade and Quiescence Gating ([ifa/issues/146](../issues/146-remove-all-arbitrary-splitting.md), [ifa/issues/157](../issues/157-FA-all-demand-must-be-evaluated-at-quiescence.md))**:
  Auditing remaining splitters to eliminate any residual arbitrary/structural splitting, and ensuring all demand tests are evaluated once types have converged at quiescence.

### 2. Open Capability Gaps and Design Observations

The following items from the audit and survey remain open or describe architectural capabilities that may warrant future design decisions:

#### 2.1 `type_num_fold` Discards the Operator (`p = 0`)
*Reference:* `fa.cc:1291`, Survey D2, and [ifa/issues/146](../issues/146-remove-all-arbitrary-splitting.md#coercion-as-a-demand-source--feasibility-measured-2026-09-15).
- **Current Behavior:**
  ```cpp
  AType *type_num_fold(Prim *p, AType *a, AType *b) {
    (void)p;
    p = 0;  // for now
    ...
    r->set_add(coerce_num(atype, btype)->abstract_type->v[0]);
  ```
  `type_num_fold` discards the primitive operator and computes solely the coerced type kind across the cross-product of operand types.
- **Consequence:** Constant folding for numeric operations occurs in `fold_constant` (`num.cc`) on single values, but FA cannot fold multiple constant values (e.g. folding `{0, 1} + {0, 1}` to `{0, 1, 2}`). Retaining the operator would enable value-level arithmetic set folding in the abstract interpreter.

#### 2.2 Async/Await Type System Transparency
*Reference:* `fa.cc:3521`, Survey S6, and [issues/closed/022-async-await-syntax.md](../../issues/closed/022-async-await-syntax.md).
- **Current Behavior:**
  `P_prim_await` is typed as identity: `flow_vars(operand, result)`.
- **Consequence:**
  FA models `await coro()` as though the async function returns its awaited value directly; there is no first-class `coroutine`, `task`, or `awaitable` type representation in the lattice.
  - Calling an async function and immediately awaiting it works cleanly.
  - Passing coroutine objects into containers, passing them across threads, or driving them through custom Python event loops is not modeled in IFA and will produce typing errors or unexpected representations.

#### 2.3 Closure Use Verification Assertion
*Reference:* Survey S3 (assertion half), [ifa/issues/closed/032](../issues/closed/032-fa-survey-findings.md).
- **Status:**
  The invariants comment explaining what survives a pass landed in `clear_avar`. However, the planned debug assertion verifying that every closure CreationSet consumed by a live call site has `closure_used` set remains unimplemented.
- **Blocker:**
  The IR data model does not currently maintain a direct consumed-by backlink from call sites to closure CreationSets. Implementing this assertion requires either adding that backlink or executing a full post-convergence graph reachability sweep.

#### 2.4 Composed Narrowing Predicates on a Single Variable
*Reference:* `fa.cc:1025`, Survey S1 note.
- **Current Behavior:**
  `flow_var_permit_pred` installs a single predicate (`restrict_pred`). If a second, distinct predicate arrives for the same AVar (for instance, chained checks such as `if x is not None and isinstance(x, Foo):`), the implementation drops the second predicate with `"composition not implemented"`.
- **Consequence:**
  Downstream code only receives the narrowing benefits of the first predicate in the chain. Supporting composed predicates would require extending `AVar` to hold a predicate list or combined predicate mask.

#### 2.5 Unbounded Recursion in Data/Control Flow Traversals
*Reference:* Audit §5, Survey P4.
- **Current Behavior:**
  `update_in`, `update_setter`, `build_type_mark`, and `back_reaching` recurse across graph diameter without an explicit depth bound (unlike `peel_wrapper_def`, which is capped at 6 hops).
- **Consideration:**
  While recursion matches the mathematical formulation of data-flow propagation, deeply linear or un-optimized machine-generated code could risk stack exhaustion. If recursion depth becomes an issue on large inputs, these traversals can be converted to explicit worklist stacks.
