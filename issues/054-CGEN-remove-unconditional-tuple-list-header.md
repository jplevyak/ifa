# 054 — Narrow (or remove) the unconditional tuple list-header added for plcfrs

**Status:** open, filed 2026-07-19 as a deliberate follow-up to the
fix in
[../../issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)'s
"plcfrs's remaining violation" entry (commit `e9b6d136`). Not a bug
report — the fix landed is known to be broader than necessary; this
tracks narrowing or removing that breadth once there's a real reason
to (performance complaint, or someone picking up
[053](closed/053-tuple-unpack-target-heterogeneous-arity-segfault.md) and
wanting a cleaner base to build on).
**Affects:** `ifa/codegen/cg.cc`'s `P_prim_make` (the `_CG_prim_tuple`
→ `_CG_prim_tuple_list` change) and
`ifa/codegen/cg_emit_llvm.cc`'s `emit_send_make` (the widened
`is_list_or_vec || is_tuple` Stage-2 condition).

## What changed and why

Every tuple construction (`(a, b)`, `(1, 2, 3)`, ...) now allocates a
16-byte list-header (`total_len`/`len`/`ptr`, `_CG_list_struct` in
`pyc_c_runtime.h`) unconditionally, on both backends. Before this
fix, only list literals promoted to record shape
(`_CG_prim_tuple_list`) got one; real tuples used a bare
`GC_MALLOC(sizeof(struct))` with no header at all
(`_CG_prim_tuple`, "the true-tuple macro").

The header is only ever *read* when a tuple's arity isn't resolvable
at compile time — which only happens when tuples of *differing*
arity get unioned into one CreationSet (`len()`/non-constant
indexing over that union falls back to the generic runtime path,
`_CG_prim_len`, which needs a real header to read). For the
overwhelmingly common case — a tuple whose arity is uniform across
every contour that creates it — the header is pure overhead: never
read, always written, 16 bytes per instance.

## Why it was done unconditionally rather than precisely

Doing this precisely means codegen (`P_prim_make`, at the
*construction* site) needs to know whether the tuple's *eventual*
CreationSet — which the construction site alone can't see; it
depends on every other place that value flows to and unions with —
will ever end up heterogeneous-arity. That's an FA-level decision
(does this Sym's `element` type, or the union it may later become
part of, have members with differing `has.n`?) that would need to be
computed and threaded back to the construction site, plausibly as a
flag on the type Sym analogous to how other per-Sym precision
decisions already get threaded through this codebase (e.g.
`clone_methods_per_cs`, `is_vector`). That's real, non-trivial FA
work with its own regression surface — out of scope for landing a
fix to an active corpus blocker (plcfrs) same-day. The unconditional
version is provably safe (field access is offset-based and doesn't
care what's behind the pointer) and was the fastest way to unblock
the actual bug with reasonable confidence.

## What a narrower fix would look like

1. At the point FA finalizes a Type_SUM whose members are all
   `Type_RECORD` (the tuple-union case), check whether `has.n`
   differs across members. If so, mark the union type (or each
   member Sym) as needing a header.
2. `P_prim_make`'s codegen reads that flag instead of always taking
   the list-header path — `_CG_prim_tuple` (headerless) stays the
   default; `_CG_prim_tuple_list` only for tuples flagged as
   eventually joining a heterogeneous-arity union.
3. Both backends need the same flag consulted the same way (the
   original bug here was two backends independently drifting on this
   exact decision — a shared flag read symmetrically avoids
   reintroducing that).

## Why this hasn't been prioritized

No profiling evidence yet that the extra 16 bytes/tuple matters in
practice for any current corpus example — this is a "known
technically-unnecessary cost, revisit if it's ever shown to matter"
entry, not an active complaint. Landing the narrow version also isn't
free: it adds a new per-Sym FA flag and a new codegen branch, i.e.
real surface area, for a benefit that's currently only theoretical.

## Complexity and Research Findings (2026-08-07 Audit)

A deep-dive audit of the codebase confirms that this is **not a simple fix** and involves several non-trivial architectural trade-offs across FA and both codegen backends:

1. **Granularity Dilemma (`CreationSet` vs `Sym`):**
   - Placing a `needs_header` flag on a shared tuple type `Sym*` is over-conservative: if `(int, int)` is instantiated at Site A (monomorphic use) and Site B (unions with `(int, int, int)`), Site A would also be forced to allocate a header.
   - Placing the flag on the `CreationSet` (`cs`) requires `P_prim_make` in both `cg.cc` and `cg_emit_llvm.cc` to map the creation PNode contour back to its specific `CreationSet`, requiring additional lookups in codegen.

2. **Backward Propagation in FA:**
   - Heterogeneous-arity unions (`Type_SUM` of `Type_RECORD`s with differing `has.n`) are formed dynamically during FA convergence when an `AVar` receives multiple `CreationSet`s.
   - FA would need a post-pass or incremental propagation phase to mark all `CreationSet`s contributing to such a union with `needs_header = true`.

3. **Backend Parity Risk:**
   - Both `cg.cc` (`P_prim_make`) and `cg_emit_llvm.cc` (`emit_send_make`) as well as index/length primitives (`emit_norm_idx`, `_CG_prim_len`) must consume the exact same flag identically to prevent backend divergence (the exact root cause of Issues 053, 076, and 080).

4. **Conclusion:**
   - The current universal 16-byte header strategy is 100% sound, provably safe, and introduces zero failures across the entire 254-test suite and `shedskin` benchmark corpus.
   - Unless profiling demonstrates measurable memory/GC overhead in a tuple-allocation-heavy program, the complexity and risk of adding backward FA propagation outweigh the theoretical 16-byte saving.

## What this unblocks

Nothing currently blocked on this. Revisit if: (a) a tuple-heavy hot
path shows measurable regression from the extra header, or (b)
someone working on [053](closed/053-tuple-unpack-target-heterogeneous-arity-segfault.md)
finds it easier to reason about the header-vs-headerless distinction
being precise rather than universal.
