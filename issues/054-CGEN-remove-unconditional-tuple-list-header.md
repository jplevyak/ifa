# 054 — every tuple carries a 16-byte list header it rarely needs

**Status:** open, deliberately deferred (performance, no correctness
impact). Rewritten 2026-09-28; history in git:
`git show 3f36072b:ifa/issues/054-CGEN-remove-unconditional-tuple-list-header.md`.

## What and why

Since `e9b6d136` (plcfrs, 2026-07-19), every tuple is allocated with a
`_CG_list_struct` header (`total_len`/`len`/`ptr`) on both backends
(`cg.cc` `P_prim_make` → `_CG_prim_tuple_list`; `cg_emit_llvm.cc`
`emit_send_make`). The header is only READ when a tuple's arity is not
resolvable at compile time, that is, when tuples of different arity are
unioned and `len()` or non-constant indexing falls back to the runtime
path. For a uniform-arity tuple the header is written and never read.

## A precise version, if it is ever wanted

The decision belongs to FA's representation layer, not to either
backend: a tuple CreationSet needs a header iff it can reach an AVar whose
type unions it with a tuple CS of a different arity. That is [132](132-arity-is-representation-not-provenance.md)'s
cross-CreationSet arity question, and `IFA_DBG_SLOTREP` already
enumerates those AVars. Mark the contributing CreationSets (per CS, not per
`Sym`, or a monomorphic site pays for a heterogeneous one). Both backends
must read the one flag identically. Backend drift on exactly this decision
caused closed/053, 076 and 080.

## When to do it

Only if profiling shows the 16 bytes per tuple matter (allocation or GC
time on a tuple-heavy program), or when 132's cross-CS arity work lands
and the flag falls out of it for free. The universal header is sound
today.
