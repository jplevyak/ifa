# Phase 06 — Clone (Type-Directed Monomorphisation)

Tests for `clone(fa)` — partition refinement, concrete-type
synthesis, function cloning, call-graph rebuild.

Reference: [CLONE.md](../../CLONE.md). Implementation:
`ifa/analysis/clone.cc`.

---

## 1. What runs

`clone(fa)`:
1. `initialize()` — global AVars, called_ess/css, seed
   equiv classes.
2. `determine_layouts()` — ivar offsets.
3. `determine_clones()` — fixed-point partition refinement.
4. `build_concrete_types()` — concrete `Sym`s per CS equivalence
   class.
5. `clone_functions()` — duplicate Funs, rebuild call graph.

After clone: `cs->type`, `av->type`, `Var::type` are set;
`Fun::calls` / `Fun::called` are populated; possibly new
`Fun`s exist (one per equivalence class for Funs that needed
splitting).

## 2. Input state expected

Post-FA-converge: every AVar has `out`, every ES has `args`/`rets`,
all `type_violations` resolved (or accepted).

## 3. Output format

Phase: `clone`. Extends phase 05's `fa-converge.expected`.

```
;; phase: clone
;; partition + concrete-type synthesis + Fun cloning

(cs-equiv
  ; CS equivalence classes (sorted by representative name)
  class[CS_int32_const_0]
    members: CS_int32_const_0 CS_int32_const_1 CS_int32_const_2
    type:    @int32           ; no clone needed (sym shared)
  class[CS_list_a]
    members: CS_list_a
    type:    %list_int32      ; cloned from @list; ivar element @int32
  class[CS_list_b]
    members: CS_list_b
    type:    %list_str        ; second clone with element @str
)

(es-equiv
  ; ES equivalence classes per Fun
  fun: %my_method
    class[ES_my_method_0]: members=[ES_my_method_0]      → clone %my_method (unchanged)
    class[ES_my_method_1]: members=[ES_my_method_1 ES_my_method_2]
                                                          → clone %my_method__1
)

(layouts
  ; ivar offsets per CS, with size + alignment
  CS_point_0: ivars
    %x  offset=0 size=4 align=4
    %y  offset=4 size=4 align=4
  CS_string_const: no ivars
)

(call-graph
  ; post-clone Fun::calls and Fun::called
  fun %main:
    calls:
      pnode=%p3 → [%add_int32]
      pnode=%p5 → [%my_method, %my_method__1]
    called_by:
      (none — entry point)
  fun %add_int32:
    called_by:
      [(fun=%main, pnode=%p3)]
)

(new-funs
  ; Funs created by cloning (i.e., not in fa->funs before)
  %my_method__1 cloned-from=%my_method ess=[ES_my_method_1 ES_my_method_2]
)
```

Naming:
- Concrete types: `%<basename>_<discriminator>` (e.g.
  `%list_int32`).
- Cloned Funs: `%<original>__<n>` where n is the equiv-class index.

## 4. Printer

```c
// ifa/testing/printers/print_clone.{cc,h}
void print_clone_normalized(FILE *fp, FA *fa);
```

Reads the post-clone state: walks `fa->funs` for `equiv_sets`,
`fa->css` for `equiv`, `cs->type`/`av->type` for concrete types.

## 5. Test cases

| # | Test | Exercises |
|---|---|---|
| 01 | `no_clone_monomorphic` | A monomorphic program → no Funs cloned, no CSes split. |
| 02 | `unbox_int_clone` | Two CS for int32 with different boxing → two equiv classes. |
| 03 | `polymorphic_method_clone` | A method called with two distinct receivers → Fun cloned. |
| 04 | `constant_clone` | Fn with `clone_for_constants` arg → one clone per constant. |
| 05 | `nested_closure_clone` | A nested closure cloned alongside its parent. |
| 06 | `mixed_sym_sum` | CS equiv class with multiple syms → Type_SUM result. |
| 07 | `tuple_record_collapse` | Tuple-able CS → Type_RECORD result. |
| 08 | `list_to_tuple` | All-element list with bottom element → tuple_able, becomes RECORD. |
| 09 | `cs_not_equiv_dispatch` | Two CSes of same sym must dispatch differently → `make_not_equiv`. |
| 10 | `ivar_offset_layout` | Verify `ivar_offset` is sum-aligned. |
| 11 | `call_graph_rebuild` | Post-clone `Fun::calls` reflects the cloned targets. |
| 12 | `lub_callback` | `make_LUB_type` callback called when needed. |
| 13 | `equivalent_es_pnode` | Two ESes with different call targets → don't collapse. |

## 6. Refactoring dependencies

- [REFACTORING.md](../REFACTORING.md) §1-§3 (same as phase 05).
- §7 (printers) — clone has lots of state to print.
- §8 (CloneState struct) — helpful but not blocking. Without it,
  re-running clone on the same FA breaks (state is on
  CS/ES/Fun pointers).

## 7. Acceptance

- [x] Clone printer compiles and runs end-to-end via
      `testing/print_clone.{cc,h}`, registered as the `clone` phase.
      Reuses `fa_setup_environment` + `FA::analyze` + `clone(fa)`,
      then dumps:
      - `(cs-equiv …)` — CreationSet equivalence classes (member
        count, concrete-type Sym).
      - `(es-equiv …)` — EntrySet equivalence classes per Fun.
      - `(call-graph …)` — post-clone `Fun::calls`.
      - `(new-funs …)` — Funs created by cloning (i.e. in
        `pdb->funs` but not in the original closure list).
- [~] 3 of 13 fixtures land:
      - `01_monomorphic` — single concrete call, no equiv-class
        merges.
      - `02_polymorphic_clone` — `%id` called int32 + float64, FA
        splits it into two Funs (visible in `es-equiv`).
      - `03_class_clone` — two %Point allocations with different
        field types + single reader; reader specialized per
        allocation site.
      Remaining 10 fixtures (`unbox_int`, `constant_clone`,
      `nested_closure`, `mixed_sym_sum`, `tuple_record_collapse`,
      `list_to_tuple`, `cs_not_equiv_dispatch`, `ivar_offset_layout`,
      `lub_callback`, `equivalent_es_pnode`) need richer fixtures or
      callbacks not yet exposed in the test harness.
- [x] Per-test: cs-equiv membership reflects `cs->equiv` (the
      printer iterates `cs->equiv` directly).
- [x] Per-test: call-graph reflects `Fun::calls` (likewise).
- [x] Re-running clone on a fresh `FA` produces byte-identical
      output — verified by the runner's run-twice determinism.
