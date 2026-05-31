# Phase 05 — Flow Analysis (`FA::analyze`)

Tests for the IFA flow loop, the splitter (`extend_analysis`), and
all the AType / EntrySet / CreationSet machinery.

Reference: [IFA.md](../../IFA.md). Implementation:
`ifa/analysis/fa.cc`.

This is the **biggest** phase. Split into two sub-phases to make
golden files manageable:

- `fa-init` — one analysis pass with no splitting.
- `fa-converge` — full loop with splitter to fixed point.

---

## 1. What runs

`FA::analyze(top)`:
1. `initialize()` — basic types, primitives, patterns, top edge.
2. Loop while `extend_analysis()` returns true:
   - `initialize_pass()` — clear violations, refresh top edge.
   - Drain `edge_worklist`, `send_worklist`, `es_worklist`.
   - `complete_pass()` — `collect_results`,
     `collect_*_violations`.
   - `extend_analysis()` — splitter (5 stages).

Sub-phase `fa-init` runs only the first iteration of the loop and
skips `extend_analysis`. Sub-phase `fa-converge` runs to fixed
point.

## 2. Input state expected

Post-finalize IF1 with Funs registered (`pdb->add(f)` already
called for every closure). The harness invokes
`FA::analyze(if1->top->fun)`.

For complex tests, the harness can pre-populate constraints
(e.g., `add_var_constraint(av, sym_int32)`) before kicking
analysis — see [REFACTORING.md](../REFACTORING.md) §9.

## 3. Output format

Two phase names. Both extend earlier phases' output (CFG + SSU + dom
already there).

### 3.1 `<test>.fa-init.expected`

State after one analysis pass. Lists every AVar / AType / EntrySet
/ CreationSet / AEdge produced.

```
;; phase: fa-init
;; one analysis pass; no splitting

(types
  ; named ATypes, sorted by name
  bottom_type      ()
  void_type        (@void_type)
  any_type         (@any)
  int32_type       (@int32)
  ...
)

(entry-sets
  ; one block per EntrySet
  ES_add_0
    fun: %add
    args:    pos[1]=AV_a_0  pos[2]=AV_b_0
    rets:    [AV_r_0]
    filters: pos[1]:int32_type pos[2]:int32_type
    edges:   1 incoming (E_top_add_0)
    out:     [E_add_reply_0]
    creates: ()
  ES___main___0
    ...
)

(avars
  ; sorted by (Var-name, contour-name)
  AV_a_0     var=%a contour=ES_add_0   out=int32_type  setters=∅
  AV_b_0     var=%b contour=ES_add_0   out=int32_type  setters=∅
  AV_r_0     var=%r contour=ES_add_0   out=int32_type  setters={AV_a_0,AV_b_0}
  AV_top_0   var=%top contour=GLOBAL    out=any_type
)

(creation-sets
  ; sorted by sym
  CS_int32_const_42  sym=@int32 vars=()  defs=[AV_lit_0]
)

(edges
  ; sorted by (from, to, pnode)
  E_top_add_0  from=ES_top_0 to=ES_add_0 pnode=%p3
    args: pos[1]=AV_lit_0 pos[2]=AV_lit_1
    initial_types: pos[1]:int32_type pos[2]:int32_type
)

(violations
  ; sorted
  (none)
)
```

Naming:
- `ES_<funname>_<n>` per EntrySet (n = creation order within Fun).
- `AV_<varname>_<n>` per AVar (n = order within contour).
- `CS_<symname>_<discriminator>` per CreationSet.
- `E_<fromES>_<toES>_<n>` per AEdge.

### 3.2 `<test>.fa-converge.expected`

Same shape, but post-convergence. Adds:

```
(pass-counts
  total-passes: 3
  splits: 2 type, 0 setter, 1 violation-driven
  time: <suppressed>
)

(history
  pass 1: 4 type confluences; split ES_add → ES_add_int, ES_add_float
  pass 2: no progress
  pass 3: no progress
)
```

Time is suppressed because tests need determinism. The runner
filters it out in diffs.

## 4. Printer

```c
// ifa/testing/printers/print_fa.{cc,h}
void print_fa_state(FILE *fp, FA *fa);     // common body for both phases
void print_fa_history(FILE *fp, FA *fa);   // converge-only
```

`print_fa_state` walks `fa->ess`, `fa->css`, `fa->global_avars`, and
every Fun's `fa_all_Vars` to produce the four major blocks.

## 5. Test cases

Listed in order of complexity. Many overlap; aim for minimal IR
per test.

### Init-only

| # | Test | Exercises |
|---|---|---|
| 01 | `monomorphic_add` | Pure int32+int32 → one ES, no splitting needed. |
| 02 | `single_class_call` | Method on one class → one CS, one ES per method. |
| 03 | `primitive_const_fold` | `1 + 2` → folded to const Sym; result AType is the const. |
| 04 | `union_type_arg` | Arg with `int32 | float32` → result AType has both. |
| 05 | `local_var_flow` | `x = a; y = x + 1` → AVar flow chain. |
| 06 | `if_branch_flow` | `if c: x = a else: x = b` → x has union. |

### Splitter

| # | Test | Exercises |
|---|---|---|
| 10 | `dispatch_split` | Method `m` called with two distinct receivers → ES splits. |
| 11 | `mark_split` | Two args with same base type but different origin → mark-based split. |
| 12 | `setter_split` | Two writers to the same field → setter-based split. |
| 13 | `violation_split` | Dispatch ambiguity → violation-driven split resolves it. |
| 14 | `recursive_no_split` | A recursive Fun stays one ES (recursion limits splitting). |
| 15 | `nested_closure_display` | Closures over outer vars → display chain stable. |

### Edge cases

| # | Test | Exercises |
|---|---|---|
| 20 | `permit_boxing` | `fa->permit_boxing = true` → mixed-basic ATypes don't violate. |
| 21 | `notype_violation` | An unused arg → NOTYPE violation. |
| 22 | `reanalyze_callback` | Frontend `reanalyze` returns true → extra pass. |
| 23 | `pass_limit` | Pathological input runs past `IFA_PASS_LIMIT` → loop exits without crash. |
| 24 | `partial_application` | `f(1)` where `f` takes 2 args → closure creation. |
| 25 | `pyc_period_dispatch` | `obj.attr` resolves field via `cs->var_map`. |

## 6. Refactoring dependencies

- [REFACTORING.md](../REFACTORING.md) §1 (per-FA worklists) —
  **hard blocker**. Without this every test contaminates the next.
- §2 (singleton reset) — same.
- §3 (deterministic IDs) — for stable AVar/ES naming.
- §7 (printers) — major work here.
- §5 (finalize sub-phases) — minor; helps isolate test setup.

## 7. Acceptance

- [ ] Init-only printer + 6 tests pass.
- [ ] Converge printer + 6 splitter tests pass.
- [ ] All 6 edge-case tests pass.
- [ ] Run-twice determinism: the same fixture run twice produces
      byte-identical output.
- [ ] `pass-counts` and `history` blocks are stable (with time
      stripped).
- [ ] Coverage report: the splitter's 5 stages each have at least
      one test that exercises them in `extend_analysis`.
