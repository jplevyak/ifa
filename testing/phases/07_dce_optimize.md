# Phase 07 — DCE, Inlining, Frequency Estimation

Tests for the post-clone optimization passes:
`mark_live_code`, `mark_live_types`, `mark_live_funs`,
`frequency_estimation`, `simple_inlining`.

Reference: [OPTIMIZE.md](../../OPTIMIZE.md). Implementation:
`ifa/optimize/dead.cc`, `ifa/optimize/inline.cc`.

---

## 1. What runs

Per the ordering in `ifa.cc:ifa_analyze` + `ifa_optimize`:

```
mark_live_code(fa)           ; AVar/PNode liveness backward from side-effects
frequency_estimation(fa)     ; build loop trees, assign exec_frequency
mark_live_funs(fa)           ; transitive reachability from __main__
simple_inlining(fa)          ; inline single-send + identity + closure-collapse
mark_live_types(fa)          ; Sym::type_live
mark_live_funs(fa)           ; final reachability prune
```

Tests split these into three sub-phases for diff clarity:
- `dce`         — `mark_live_code` + `mark_live_types` + `mark_live_funs`
                  (first invocation)
- `freq`        — `frequency_estimation`
- `inline`      — `simple_inlining` + final `mark_live_funs`

## 2. Input state expected

Post-clone: concrete types, populated `Fun::calls`/`called`.

## 3. Output format

### 3.1 `<test>.dce.expected`

```
;; phase: dce
;; live-bit state after mark_live_code + mark_live_types + mark_live_funs

(live-pnodes
  fun %main:
    live: %p0 %p1 %p2 %p3 %p4
    dead: %p5 %p6
  fun %unused_helper:
    (entire fun dead)
)

(live-vars
  fun %main: %a %b %r          ; live
              %tmp_unused        ; dead
)

(live-types
  ; sym → type_live bool
  @int32:  live
  %my_record:  live
  %unused_record:  dead
)

(live-funs
  ; after mark_live_funs prune of fa->funs
  alive: %main %add %factorial
  dropped: %unused_helper %another_dead
)
```

### 3.2 `<test>.freq.expected`

```
;; phase: freq
;; execution_frequency per PNode and per Fun (after frequency_estimation)

(loop-trees
  ; from find_local_loops + find_recursive_loops
  fun %factorial:
    %l0_root
      %l1_outer  level=1
  fun %main: (no loops)
  global call graph:
    rec_a: %factorial %factorial__rec
)

(freq-pnodes
  fun %main:
    %p0 1.0    %p1 1.0    %p2 10.0    %p3 10.0    %p4 1.0
  fun %factorial:
    %p0 1.0    %p1 10.0   %p2 10.0    %p3 10.0
)

(freq-funs
  %main:        1.0
  %factorial:  10.0
  %add:         1.0
)
```

### 3.3 `<test>.inline.expected`

```
;; phase: inline
;; effect of simple_inlining

(inlined
  ; per-call-site result
  fun %main, pnode %p3 → inlined %identity_add
  fun %main, pnode %p4 → inlined %const_42 (single-send)
  fun %main, pnode %p5 → unchanged (multi-target)
)

(closure-collapsed
  fun %main, create-pnode %p2 + call-pnode %p3 → direct call
)

(post-inline-live-funs
  alive: %main
  dropped (now unreachable): %identity_add %const_42
)
```

## 4. Printer

```c
// ifa/testing/printers/print_dce.{cc,h}
void print_dce_state(FILE *fp, FA *fa);
void print_freq_state(FILE *fp, FA *fa);
void print_inline_effects(FILE *fp, FA *fa);
```

`print_inline_effects` requires the inliner to record what it did
— see [REFACTORING.md](../REFACTORING.md) §7. Without instrumentation
the printer can only diff before/after, which is harder.

## 5. Test cases

### DCE

| # | Test | Exercises |
|---|---|---|
| 01 | `unused_var` | A local Var assigned but never read → dead. |
| 02 | `unused_send` | A SEND whose result is unused → dead (if `nonfunctional` false). |
| 03 | `side_effecting_send_kept` | A SEND with `nonfunctional=true` → kept. |
| 04 | `is_visible_prim_kept` | Registered prim with `is_visible=1` → kept. |
| 05 | `dead_branch` | IF with constant true → false branch dead. |
| 06 | `unused_class` | A class never instantiated → not in `mark_live_types`. |
| 07 | `unused_fun_pruned` | A Fun no one calls → dropped from `fa->funs`. |
| 08 | `instance_var_liveness` | Live ivar → containing object Var live. |

### Frequency

| # | Test | Exercises |
|---|---|---|
| 10 | `no_loop_baseline` | Linear program → all PNodes freq 1.0. |
| 11 | `simple_loop` | Single loop → body PNodes freq 10.0. |
| 12 | `nested_loops_100x` | Double-nested → inner body 100.0. |
| 13 | `recursive_loop` | Recursive Fun in call-graph loop → Fun freq 10.0. |
| 14 | `dfs_ancestor_filter` | Back-edge call doesn't inflate freq. |

### Inline

| # | Test | Exercises |
|---|---|---|
| 20 | `single_send_inline` | Helper with one SEND → inlined at call site. |
| 21 | `identity_inline` | Helper returns its arg → call converted to MOVE. |
| 22 | `closure_collapse` | Closure create + call → direct call. |
| 23 | `period_closure_reorder` | `obj.m()` collapse reorders receiver to right slot. |
| 24 | `multi_target_no_inline` | Polymorphic site → skipped. |
| 25 | `inline_typed_wrapper` | Caller/callee type mismatch → wrapper MOVE inserted. |
| 26 | `sub_constants_after_inline` | Constants substituted into rvals post-inline. |
| 27 | `fun_dropped_after_inline` | Inlined Fun's only call site gone → dropped by second `mark_live_funs`. |

## 6. Refactoring dependencies

- [REFACTORING.md](../REFACTORING.md) §1-§3 (same).
- §7 (printers).
- For `inline` tests, the inliner should record its actions (which
  call site was inlined, which closure was collapsed). Currently
  this info is destroyed in-place. Add a sidecar Vec of
  `InlineEvent` records that the printer reads. Light refactor;
  goes alongside this phase.

## 7. Acceptance

- [ ] DCE printer + 8 tests pass.
- [ ] Freq printer + 5 tests pass.
- [ ] Inline printer + 8 tests pass.
- [ ] Re-running DCE on the same FA is idempotent.
- [ ] Frequency estimates respect `LOOP_FREQUENCY = 10.0` (any
      change to that constant changes the goldens).
