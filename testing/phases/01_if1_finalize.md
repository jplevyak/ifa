# Phase 01 — `if1_finalize`

Tests for IF1 finalization: dead-code marking, primitive binding,
code-tree flattening, nesting-depth fixup.

Reference: [IR.md](../../IR.md) §4.2; implementation
`ifa/if1/if1.cc:if1_finalize` (line ~755).

---

## 1. What runs

`if1_finalize(IF1 *p)`:

1. `find_primitives(p)` — walks every closure body, calls
   `Primitives::find(c)` on every `Code_SEND`, stores result in
   `code->prim`.
2. `if1_simple_dead_code_elimination(p)` (if `fdce_if1`) — three
   iterative sub-passes that set `Sym::live`, `Code::live`,
   `Label::live`.
3. For each closure: `if1_flatten_code(f->code, Code_CONC, NULL)`
   then `if1_fixup_nesting(f->code, f)`.

After finalize: every SEND has `code->prim` set if it's a primitive
(or `prim_primitive` for registered prims, or NULL); dead nodes
are marked; nested SEQ/CONC are flattened; LOCALLY_NESTED Syms are
bumped to `f->nesting_depth + 1`.

## 2. Input state expected

A populated `IF1` with at least one closure registered via
`if1_closure`. Sym types (`int32`, etc.) registered as builtins.
No CFG / SSU / FA state.

## 3. Output format

`<test>.finalize.expected` is a sorted-by-name dump of the post-
finalize IF1 state. Format:

```
;; phase: finalize
;; <ID-stripped comment-only banner; rest is parseable .ir>

(prim-bound
  ; SEND name → (prim name OR "none")
  %add_call   prim_add
  %make_tup   prim_make
  %user_call  none
)

(live
  ; sym → bool, in declaration order
  syms: %a=1 %b=1 %dead=0 %result=1
  codes: 8 live, 2 dead
  labels: 4 live, 0 dead
)

(flatten
  ; nesting-depth fixups
  %inner_local: LOCALLY_NESTED → 2
)

(if1
  ;; the full post-finalize IF1, as round-trippable .ir text
  ;; (uses the writer from IF1_TEXT_FORMAT.md §7)
  ...
)
```

The format is layered:
- The first three sections (`prim-bound`, `live`, `flatten`) are
  the *delta* this phase produced — easy to eyeball.
- The last `(if1 ...)` section is the full state, for round-trip
  validation and as input to the next phase.

## 4. Printer

```c
// ifa/testing/printers/print_finalize.{cc,h}
void print_finalize_normalized(FILE *fp, IF1 *p);
```

Reuses `write_ir` for the `(if1 ...)` block; implements the three
summary blocks specifically.

## 5. Test cases

Numbered for ordering. Each is `<n>_<short_name>.ir` plus
`<n>_<short_name>.finalize.expected`.

| # | Test | Exercises |
|---|---|---|
| 01 | `simple_add` | Binary primitive `+` resolves to `prim_add`. |
| 02 | `nested_send` | A SEND whose rval is itself a SEND result. Both bind. |
| 03 | `unknown_primitive` | `(send :prim @sym_primitive #unknown_name ...)` resolves to `prim_primitive`, not NULL. |
| 04 | `dead_send` | A SEND whose lvals are unused → marked dead. |
| 05 | `live_chain` | MOVE chain reaching exit → all live. |
| 06 | `dead_label_branch` | An IF where one branch is unreachable → label marked dead. |
| 07 | `flatten_nested_seq` | `(seq (seq (move ...) (move ...)) ...)` → flattened. |
| 08 | `nesting_fixup` | A LOCALLY_NESTED Sym in a nested fun gets the parent's depth + 1. |
| 09 | `multi_closure` | Two closures, both processed; one has live code, other doesn't. |
| 10 | `prim_visibility` | A registered prim with `is_visible = 1` stays live even if lvals unused (seeded by `mark_initial_dead_and_alive`). |

## 6. Refactoring dependencies

- [REFACTORING.md](../REFACTORING.md) §3 (deterministic IDs) — must
  land before goldens can be stable.
- [REFACTORING.md](../REFACTORING.md) §5 (finalize sub-phases) —
  needed for test #04 (`dead_send`) which wants to inspect pre-DCE
  state.
- §6 (DEBUG_PRINT audit) — not blocking for this phase; finalize
  doesn't currently print noise.

## 7. Acceptance

- [ ] Printer compiles and runs.
- [ ] All 10 tests pass.
- [ ] Round-trip: `.finalize.expected`'s `(if1 ...)` block re-parses
      into a state that re-prints identically.
- [ ] A new test added by following the per-test template takes <
      10 lines of `.ir` for simple cases.
