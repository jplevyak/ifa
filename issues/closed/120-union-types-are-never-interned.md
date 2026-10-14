# 120 — union types are never interned: ~1300 Syms for 27 distinct unions

**Closed 2026-10-14.** Value unions are interned (`intern_sum`,
`analysis/clone.cc`); see "Resolution" at the end. The ownership audit
found nothing written to a union Sym after it is handed out except
codegen's C name, and the recorded segfault no longer reproduces.

**Status at the time (kept as the record):** open. Naive interning
SEGFAULTS the compiler. An ownership audit comes first. Rewritten
2026-09-28; history in git:
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

## Resolution (2026-10-14)

**The ownership audit.** `IFA_DBG_SUMAUDIT` (`analysis/clone.cc`): each
construction site registers its Sym at handout (the `make_LUB_type` call)
with a fingerprint of every field a type user can observe -- `has`,
`element`, `name`, `creators`, `type`, `type_kind`, `isa`,
`specializes`, `specializers`, `implementors`, `dispatch_types`,
`size`, `meta_type`, `temp`, `cg_string`, `abstract_type`, `alias`,
`llvm_type`, the flags -- and `sum_audit_check` reports every field that
moved, by phase: after `build_concrete_types`, after `clone_functions`,
after clone, after optimize, and after codegen (both backends).

Over the whole pyc suite on both backends, and on msp_ss, richards,
timsort and sudoku1, **the only field written after handout is
`cg_string`**, in codegen, from all four value-union sites. Codegen's
C name for a union is a function of its components (`_CG_any` unless
`has` narrows it), so a shared Sym gets the same name from every user.
Nothing writes `has`, `element`, `name` or `creators` after handout.

**The segfault is gone.** With interning at all five sites, including
the CreationSet-group union, `tests/nested_tuple_repr.py` compiles and
matches and the C suite passes (457). Whatever the 2026-09 attempt hit
was fixed since; ifa/182's self-typing of clone-made unions (`t->type ==
t`) is the likeliest candidate, but it was not bisected.

**What landed.** `intern_sum` at the four VALUE-union sites
(`concrete_type_set_to_type`, `concretize_avar`, `concretize_var`, the
list element, whose `has` is `set_to_vec`'d first), keyed on the sorted
component ids. The CreationSet-group union (`define_concrete_types` /
`resolve_concrete_types`) is excluded, and anything with `creators` is
never returned for a value union: that Sym is the type of its own
CreationSets, and interning it would leave `cs->type` pointing at a Sym
whose `creators` omit them. `PYC_INTERN_SUMS=0` turns it off.

**Verification.** `SUMDUP` now also counts the union Syms IN USE (a live
Var's type or a list element's), keyed on component ids, since
`sum_syms` counts every Sym ever minted including the discarded
duplicates:

| program | in use before | after | distinct |
| --- | --- | --- | --- |
| msp_ss | 1002 | 30 | 30 |
| richards | 613 | 24 | 24 |
| timsort | 206 | 8 | 8 |
| sudoku1 | 11 | 3 | 3 |

`make test` green (457 on each backend). Two goldens moved, each by one
more inlined call (`tests/exception_hierarchy_base.py`,
`tests/splitter_setter_of_setter.py`: `direct` 73 -> 72, 64 -> 63;
`PYC_INTERN_SUMS=0` restores both). Corpus check -R
`check__default__b908709e+a279682e` against `b908709e+a70a805b`: every
column identical on all 77. Measured separately, corpus-wide direct calls
37,245 -> 37,154 (91 more inlined by `inline_single_sends`' type-identity
guard), dynamic 87 and null-tested 181 unchanged.
