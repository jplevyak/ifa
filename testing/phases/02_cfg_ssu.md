# Phase 02 — CFG + SSU

Tests for `Fun::build_cfg` (Code tree → PNode graph) and
`Fun::build_ssu` (phi/phy insertion + rename).

Reference: [CFG_SSU.md](../../CFG_SSU.md) §2 (CFG) and §4 (SSU).
Implementation: `ifa/optimize/cfg.cc`, `ifa/optimize/ssu.cc`.

---

## 1. What runs

Triggered by `new Fun(closure_sym)`. Order:
1. `Fun::build_cfg`:
   - `resolve_labels(code)` — `Label::code` back-pointers.
   - `build_pn_cfg(if1, code, NULL, NULL)` — Code → PNode + CFG.
   - `entry = sym->code->pn`, `exit = sym->code->sub.last()->pn`.
   - `finalize_cfg(this)` — drop unreachable, set_to_vec preds.
2. `Fun::build_ssu`:
   - `build_cfg_dominators(this)` — see phase 03.
   - `approximate_liveness(this, pnodes)` — `PNode::live_vars`.
   - `place_phi(vrs)` / `place_phy(vrs)` — phi/phy insertion.
   - `rename_vars(this, pnodes)` — SSU rename.

The test harness uses [REFACTORING.md](../REFACTORING.md) §4
(`FUN_BUILD_CFG_ONLY` flag) to invoke just `build_cfg`. SSU tests
use the default (CFG + SSU).

## 2. Input state expected

Post-finalize IF1: all SENDs have `code->prim` set, code is
flattened, nesting fixed. One closure registered as `if1->top` (or
specified by `(entry %name)` in the `.ir`).

## 3. Output format

Two phase names: `cfg` and `ssu`. Each has its own `.expected`.

### 3.1 `<test>.cfg.expected`

```
;; phase: cfg
;; Fun %add

(fun %add
  entry: %p0
  exit:  %p7
  pnodes: 8
)

(pnodes %add
  ; name = stable PN-name; KIND code; succ→[...]; pred←[...]
  %p0  LABEL  L_entry      succ:[%p1]
  %p1  MOVE   %a → %t0     succ:[%p2]
  %p2  SEND   prim_add %t0 %b → %t1  succ:[%p3]
  %p3  IF     %t1          succ:[%p4 %p5]
  %p4  LABEL  L_true       succ:[%p6]   pred:[%p3]
  %p5  LABEL  L_false      succ:[%p6]   pred:[%p3]
  %p6  LABEL  L_join       succ:[%p7]   pred:[%p4 %p5]
  %p7  SEND   prim_reply %cont %t1      pred:[%p6]
)
```

Names: `%pN` per PNode where N is the deterministic order (DFS from
entry). The PNode order is stable: DFS pre-order with successors
sorted (true branch before false for IF, sub[0] first for groups).

### 3.2 `<test>.ssu.expected`

Extends `.cfg.expected` with phi/phy sub-lists per PNode.

```
;; phase: ssu
;; Fun %add — phi/phy nodes inserted at join/split points

(phi %add
  ; per host PNode: list of phi MOVE descriptions
  %p6: %x_v2 = phi(%x_v0 from %p4, %x_v1 from %p5)
)

(phy %add
  %p3: (no phys)
  %p4: %t1_true = phy(%t1)
  %p5: %t1_false = phy(%t1)
)

(rename %add
  ; Var → Var renames produced by the rename pass
  %x   → %x_v0 (def %p1) %x_v1 (def %p2) %x_v2 (def %p6 phi)
  %t1  → %t1   (def %p2) %t1_true (def %p4 phy) %t1_false (def %p5 phy)
)

(live-vars %add
  ; post-liveness: per-PNode live set (sorted, by name)
  %p1: %a %b
  %p2: %a %b
  ...
)
```

The `(rename ...)` block makes SSU renames visible by symbol family.
The `(live-vars ...)` block validates `approximate_liveness`.

## 4. Printer

```c
// ifa/testing/printers/print_cfg.{cc,h}
void print_cfg_normalized(FILE *fp, Fun *f);

// ifa/testing/printers/print_ssu.{cc,h}
void print_ssu_normalized(FILE *fp, Fun *f);
```

PNode-naming utility shared in `ifa/testing/printers/util.cc`:
`assign_pnode_names(Fun *f, Map<PNode*, cchar*> &names)` — DFS
from entry with deterministic successor ordering.

## 5. Test cases

### CFG

| # | Test | Exercises |
|---|---|---|
| 01 | `linear` | Three MOVEs in sequence → 3 PNodes, linear succ. |
| 02 | `if_else` | `(if-then-else)` → entry/true/false/join shape. |
| 03 | `if_no_else` | IF without else → join falls through. |
| 04 | `loop_while` | `(loop :cond ...)` → back-edge. |
| 05 | `loop_do_while` | `before == body` form of `if1_loop`. |
| 06 | `unreachable_after_goto` | Code after unconditional GOTO → removed by `remove_unreachable`. |
| 07 | `nested_groups` | SEQ inside SUB inside CONC — flattening before CFG. |
| 08 | `multi_pred_label` | A LABEL with multiple incoming edges → `cfg_pred.n > 1`. |

### SSU

| # | Test | Exercises |
|---|---|---|
| 10 | `simple_local` | Local Var written then read → no phi needed. |
| 11 | `if_join_phi` | Local written in both branches → phi at join. |
| 12 | `if_one_branch_phi` | Local written in true branch only → phi(undef, defined). |
| 13 | `loop_phi` | Local updated in loop body → phi at loop header. |
| 14 | `nested_if_phi` | Nested IFs → phi propagates correctly. |
| 15 | `rename_chain` | Long MOVE chain → each def gets fresh Var. |
| 16 | `phy_split` | Split point → phy nodes for each branch. |
| 17 | `non_local_no_ssu` | Global / argument / constant Vars → no SSU treatment. |

## 6. Refactoring dependencies

- [REFACTORING.md](../REFACTORING.md) §1, §2, §3 (per-FA worklists,
  singleton reset, deterministic IDs) — needed.
- §4 (`FUN_BUILD_CFG_ONLY` flag) — needed to test CFG without SSU
  interference.
- §7 (phase printers) — this phase's printers are the first non-
  trivial ones; their utilities (PNode naming) become shared.

## 7. Acceptance

- [ ] CFG printer compiles and runs.
- [ ] SSU printer compiles and runs.
- [ ] All 8 CFG tests pass.
- [ ] All 8 SSU tests pass.
- [ ] PNode names are stable across runs.
- [ ] An additional test of "CFG without SSU" (test #01 with
      `FUN_BUILD_CFG_ONLY`) shows no phi/phy nodes.
