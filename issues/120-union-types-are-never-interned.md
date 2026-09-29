# 120 — union types are never interned: ~1300 Syms for 27 distinct unions

**Status:** open. Naive interning SEGFAULTS the compiler. An ownership
audit comes first. Rewritten 2026-09-28; history in git:
`git show 3f36072b:ifa/issues/120-union-types-are-never-interned.md`.

## The gap

`IFACallbacks::make_LUB_type` is a no-op that nothing overrides, and the
five `Type_SUM` construction sites in `analysis/clone.cc`
(`concrete_type_set_to_type`, `concretize_avar`, `concretize_var`, the
SUM case in `resolve_concrete_types`, the list-element SUM) each mint a
fresh Sym. `IFA_DBG_BODIES=1`'s `SUMDUP` line counts them: msp_ss has
1323 SUM Syms for 27 distinct unions (49×), richards 807 for 22, timsort
160 for 13, sudoku1 87 for 13.

## Why it matters

Type identity is compared by POINTER in decisions that change the output.
`inline_single_sends` guards on `p->rvals[i]->type == v->type`, and
codegen picks C types and casts from these Syms. With 49 copies of a
union, "same type" depends on construction history rather than on the
types. (It is not closed/112's nondeterminism: that one is structural.)

## Why the obvious fix fails

Interning keyed on the sorted component ids (sorted `has` already landed
with 112) was applied at all five sites. Result: 5 suite failures, and
`tests/nested_tuple_repr.py` segfaults `pyc`. Restricting it to the two
sites that only set a type REFERENCE still segfaults. **SUM Syms are
mutated after construction** (`has`, `element`, `name`, `creators`, …), so
sharing one corrupts unrelated state.

## Fix

1. **Ownership audit:** find every write to a `Type_SUM` Sym after it is
   handed out. Eliminate it, or make it produce a new Sym. A cheap way to
   find them: after construction, mark the Sym frozen and assert on write.
2. Then intern at all five sites, keyed on the sorted component-id list.
   The list-element site builds `has` with `set_add`, so it needs
   `set_to_vec()` plus the sort before the key is taken.

## Verification

`SUMDUP` reports `sum_syms == distinct_unions` on the four programs. Six
gates. A corpus `check` A/B: more `==` comparisons succeed, so more
inlining fires, and that must be measured, not assumed neutral.
