# Phase 04 — Patterns + Dispatch Resolution

Tests for `build_arg_positions`, `build_patterns`, and the core
`pattern_match` function.

Reference: [DISPATCH.md](../../DISPATCH.md). Implementation:
`ifa/if1/pattern.cc`.

---

## 1. What runs

`build_patterns(fa)` and `build_arg_positions(fa)` both walk every
Fun in `pdb->funs` and populate per-Sym `match_type` tables plus
per-Fun positional / named MPosition maps.

`pattern_match(args, names, send, ...)` is the per-call-site
matcher. It depends on:
- `Patterns *fa->patterns` (built by `build_patterns`).
- Each Sym's `dispatch_types` (built by `build_type_hierarchy`).
- The argument AVars' `out` types (in real use, from FA; in tests,
  hand-specified).

## 2. Input state expected

For `build_*`: post-finalize IF1 with Funs. No FA needed.

For `pattern_match`: trickier. The function requires `AVar`s with
populated `out` ATypes — which only exist post-FA-init. Two options:

**(a) Mock AVars.** Build a stub `AVar` with a hand-set `out` (a
shorthand in the `.ir` text format: `(avar %v :out (@int32 @float32))`)
and call `pattern_match` directly. Needs a small helper in
`ifa/testing/`.

**(b) Drive through FA.** Run partial FA on a fixture, capture the
pattern_match traces. More realistic but harder to author.

**Recommended:** start with (a). It exercises the matcher in
isolation. Use (b) for end-to-end matching coverage as part of
phase 05.

## 3. Output format

Three sub-phases: `arg-positions`, `patterns`, `dispatch`.

### 3.1 `<test>.arg-positions.expected`

Per Fun, its MPosition layout:

```
;; phase: arg-positions
;; Fun %my_method — positional and named arg map

(positions %my_method
  ;; canonical MPositions, with their formals
  pos[1]      → %self   :type @MyClass
  pos[2]      → %x      :type @int32
  pos[2,1]    → %x.lo   :type @int8   ; pattern sub-position
  pos[2,2]    → %x.hi   :type @int8
  pos[3]      → %y      :type @int32  :default %y_default
  pos["named_only"] → %z :type @bool
)
```

### 3.2 `<test>.patterns.expected`

Per type Sym, the reverse dispatch index:

```
;; phase: patterns
;; reverse dispatch index (MType::funs per type Sym)

(mtype @int32
  pos[2]: %add %sub %mul
)

(mtype @MyClass
  pos[1]: %my_method %my_other_method
)

(mtype #my_selector
  pos[2]: %my_method
)
```

### 3.3 `<test>.dispatch.expected`

For each (hand-built or fixture-derived) call site:

```
;; phase: dispatch
;; pattern_match traces

(call site_1
  args:    [@MyClass, @int32]
  names:   [_, _]
  partial: NEVER
  matches:
    %my_method  formal_filters: pos[1]:{@MyClass} pos[2]:{@int32}
  partial:
    none
)

(call site_2
  args:    [@A | @B, @int32]
  names:   [_, _]
  partial: NEVER
  matches:
    %method_in_A  formal_filters: pos[1]:{@A} pos[2]:{@int32}
    %method_in_B  formal_filters: pos[1]:{@B} pos[2]:{@int32}
)
```

## 4. Printer

```c
// ifa/testing/printers/print_patterns.{cc,h}
void print_arg_positions(FILE *fp, FA *fa);
void print_patterns_index(FILE *fp, FA *fa);
void print_dispatch_trace(FILE *fp, Vec<DispatchSiteSpec> &sites, FA *fa);
```

`DispatchSiteSpec` is a test-only struct (`ifa/testing/dispatch_spec.h`)
that wraps the args/names/etc. passed to `pattern_match`.

## 5. Test cases

### Arg positions

| # | Test | Exercises |
|---|---|---|
| 01 | `single_positional` | One-arg function → pos[1]. |
| 02 | `multi_positional` | Three args → pos[1..3]. |
| 03 | `named_arg` | A `:name` parameter → named MPosition. |
| 04 | `pattern_destructure` | Pattern arg `(x, y)` → sub-positions. |
| 05 | `defaults` | Default-valued parameter → `:default` in output. |
| 06 | `rest_param` | `*args`-style rest → `is_rest` reflected. |
| 07 | `out_param` | `Sym_OUT` formal → in `out_positions`. |

### Patterns index

| # | Test | Exercises |
|---|---|---|
| 10 | `single_method` | One method on one class → one MType entry. |
| 11 | `overloaded` | Two methods with same name, different types → both in index. |
| 12 | `selector_dispatch` | Method bound by selector symbol → entry under `#name`. |
| 13 | `inherited_method` | Subclass inherits → still resolves via dispatch_types MRO. |
| 14 | `must_specialize` | Sym with `must_specialize = T` → dispatches on T. |

### Dispatch

| # | Test | Exercises |
|---|---|---|
| 20 | `exact_single_match` | Args match exactly one method. |
| 21 | `multi_match_polymorphic` | Receiver is `A | B` → two matches, one per branch. |
| 22 | `subsumption_filter` | Two candidates, one strictly more specific → only specific survives. |
| 23 | `partial_required` | `Partial_NEVER` + missing args → no match. |
| 24 | `partial_allowed` | `Partial_OK` + missing args → partial match. |
| 25 | `incomplete_call_shortcut` | Empty `out` on any arg → early return 0. |
| 26 | `match_cache_hit` | Same call twice → second hit is cached. |
| 27 | `match_cache_miss_after_invalidation` | New Fun added (`add_patterns`) → cache cleared. |

## 6. Refactoring dependencies

- [REFACTORING.md](../REFACTORING.md) §3 (deterministic IDs) for
  positional names.
- §9 (testable dispatch helpers) — needed to call `pattern_match`
  without a full FA setup. Adds explicit-args overloads.

## 7. Acceptance

- [ ] arg-positions printer + 7 tests pass.
- [ ] patterns printer + 5 tests pass.
- [ ] dispatch printer + 8 tests pass.
- [ ] Adding a new Fun via `add_patterns(fa, f)` clears
      `sym_match_cache` as expected.
- [ ] Dispatch traces are stable across runs of the same fixture.
