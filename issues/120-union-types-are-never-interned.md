# 120 — union types are never interned: ~1300 Syms for 27 distinct unions

**Status:** open — **naive interning is measured to SEGFAULT the
compiler** (see "Attempted" below); the gap is real but the obvious fix
is not safe without an ownership audit first. Filed 2026-08-30 out of
[112](112-CGEN-nondeterministic-emitted-c.md), which tried interning as
a fix, measured that it does not fix 112, and reverted it. The gap it
found is real on its own and had no issue.
**Affects:** `ifa/ifa.h` (`IFACallbacks::make_LUB_type`),
`ifa/analysis/clone.cc` (`concrete_type_set_to_type`,
`concretize_avar`, `concretize_var`, the `Type_SUM` case in
`resolve_concrete_types`, and the list-element SUM at the
`v->type->element->type` site).

## The gap

```c
// ifa/ifa.h
virtual Sym *make_LUB_type(Sym *s) { return s; }
```

It is the default no-op and **nothing overrides it** — pyc does not,
and neither does the V frontend. Every place that builds a union does
`new_Sym(); type_kind = Type_SUM; has = <components>;` and keeps
whatever it just minted. So two structurally identical unions are the
same `Sym` only when they happen to be produced by the same call.

## Measured

Counting `Type_SUM` Syms against distinct component-name signatures
(`IFA_DBG_BODIES=1`, `SUMDUP` line, added with this issue):

| program | SUM Syms | distinct unions | ratio |
|---|---|---|---|
| `msp_ss` | 1323 | 27 | 49× |
| `richards` | 807 | 22 | 37× |
| `timsort` | 160 | 13 | 12× |
| `sudoku1` | 87 | 13 | 6.7× |

Stable between `after-clone` and `after-optimize`, so nothing
deduplicates them later.

## Why it matters

**Type identity is compared by POINTER in decisions that change the
emitted program.** Two examples in tree today:

- `inline_single_sends` (`optimize/inline.cc`) guards on
  `p->rvals[i]->type == v->type` and bails on `Type_SUM` operands, so
  whether a call is inlined depends on whether two Vars happen to share
  a union Sym rather than on whether their types are equal.
- codegen selects C types and casts from these Syms.

With 49 copies of the average union, "same type" is a question about
construction history, not about types. Any equality-driven optimisation
is therefore weaker than it should be, and unpredictably so.

There is also a plain size cost: 1323 Syms where 27 would do, each with
its own `has` vector, carried through clone and codegen.

## What this is NOT

Not [112](112-CGEN-nondeterministic-emitted-c.md)'s root cause —
measured. With interning in place msp_ss's structural type signature was
still 5-of-6 distinct across runs and its emitted C 6-of-8, because the
types Vars receive after `clone` differ **structurally** between runs,
not merely in Sym identity. Interning canonicalises identity; it cannot
canonicalise a genuinely different component set.

Not [025](../../issues/025-shedskin-examples-coverage.md)'s
`make_LUB_type` discussion either. That one is about *reducing* an
all-numeric union to a single numeric type (a lattice/coercion
question, and 025 records why the type-only version was unsound). This
issue is only about giving structurally equal unions one identity.

## Proposed fix

Intern by the component list. The components are already sorted with
`compar_syms` at the concretize sites (112), so the sorted id list is a
canonical key: build it, look it up in a map, return the existing Sym
instead of minting. Prototyped in 112 and reverted only for lack of
benefit *there*; the patch is small and applies at the five
construction sites listed above.

Two cautions from that prototype:

- One site (the list-element SUM) builds `has` with `set_add` and never
  sorts or compacts it — it needs `set_to_vec()` plus the sort before
  any key is taken, or the key is heap-ordered.
- This changes type identity globally, which is exactly the point but
  also the risk: more `==` comparisons start succeeding, so more
  inlining fires. It needs the full gate set **plus** a corpus `check`
  sweep, not just the test suite.

## ATTEMPTED 2026-08-30: naive interning SEGFAULTS the compiler

Implemented exactly as proposed above — `canonical_sum_has()`
(`set_to_vec()` + `compar_syms`) then a lookup keyed on the sorted
component ids, applied at all five construction sites. Result:

- `make test`: **303 passed, 5 failed** (from 308/0)
- `tests/nested_tuple_repr.py`: **pyc segfaults**, rc=139, core dumped,
  with no diagnostic output at all

Narrowing: reverting the three sites that assign a STRUCTURAL role
(`cs->type` in `resolve_concrete_types`, the member type from
`concrete_type_set_to_type`, and the list-element SUM) and keeping
interning **only** at the two `concretize_avar`/`concretize_var` sites —
which merely set `av->type`/`v->type`, a type REFERENCE — still
segfaults.

So the blocker is not one bad site. **These Syms are owned and mutated
after construction**, so returning a shared one corrupts unrelated
state. Sharing a union Sym is only safe once every post-construction
mutation of a `Type_SUM` Sym is gone.

The crash does not reproduce under `gdb` (the run exits normally), which
fits the rest of this investigation: the corruption is heap-layout
dependent.

**No corpus sweep was run.** The gates already answer "positive,
neutral, or negative?" with negative — a compiler segfault is not a
neutral change, and 80 minutes of A/B sweeping on a build that crashes
would measure nothing.

### What a safe version needs

An ownership audit of `Type_SUM` Syms: find every write to a SUM's
`has`, `element`, `name`, `ast`, `creators` and so on that happens after
the Sym is handed out, and either eliminate it or make it produce a new
Sym. Only then can identity be shared. Until that is done, the sorted
`has` (already landed, see 112) is the part of this that is safe on its
own — it makes each union's component list canonical without making two
unions the same object.

## Verification plan

- `SUMDUP` reports `sum_syms == distinct_unions` on the four programs
  above.
- All five gates green; corpus `check` sweep shows no regression in
  compile/run/stdout status.
- Worth measuring as a secondary effect: inlining decisions per program
  (`IFA_DBG_BODIES=1`, `INLEVENTS ... decisions n=`), which should rise.
