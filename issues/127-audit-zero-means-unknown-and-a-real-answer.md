# 127 — audit: `0` used for both "nothing known" and a real answer

**Status:** open, filed 2026-09-04. An audit, not a defect report — the
two instances below are already fixed, and the question is how many more
there are.
**Affects:** size/width computation in `ifa/codegen/cg.cc`,
`ifa/analysis/clone.cc`, and the `->type` projection in
`ifa/analysis/fa.cc`.

## The pattern

A quantity that can be *unknown* is represented by `0` (or by an empty
set), and the same `0` is also a legitimate value. Every consumer then
has to guess which it meant, and the ones that guess wrong fail
**silently** — the value is plausible, so nothing asserts.

Two instances have bitten this repo, found weeks apart and by completely
different routes:

**1. `AType::type` maps a pure-nil type to bottom**
([124](124-FA-refuse-imprecise-inference.md)). `make_AType` strips
`nil_type` from the `->type` projection, so an AType of exactly
`{None}` projects to `bottom` — the same value that means "nothing
inferred yet". Every splitter comparison is guarded by `->n &&`, so an
edge carrying only `None` read as "no information" and was compatible
with everything. The confluence was DETECTED and could never be split;
`list.append` stayed a shared contour forever, which is what untyped
`go`'s list elements and produced ifa/123's blind casts.

**2. A pointer-shaped element type has `size == 0`**
([118](118-union-field-representation-and-polymorphic-field-offset.md),
fixed 2026-09-04). `void` and `Type_SUM` element types are emitted as
`void *` but carry `size == 0`, and `_CG_list_mult_internal` computes
`size * s1 * l + SIZEOF_LIST_HEADER`. A zero size therefore allocated
*only the header* and produced a list with ZERO capacity, into which the
next `__setitem__` wrote. `[None] * 0x80` emitted
`_CG_list_mult(t2, 128, 0)` — a 16-byte block, then an 8-byte write past
it. Under Boehm this only ever surfaced as a corrupted free list inside
`GC_clear_fl_marks`, naming nobody; it took `PYC_NO_GC` + valgrind to
see.

Both are the same mistake: `0`/`bottom` overloaded to mean "unknown"
*and* "genuinely zero", with no way for a consumer to tell.

## What to audit

Every place a size, width, offset, count or type-set is computed and a
zero/empty result is then USED rather than rejected. Starting points:

- `determine_layouts` (clone.cc) — `size`, `alignment`, and the
  `field_size` fallback that exists precisely because "this contour
  observed nothing" and "this field is zero-sized" were confused
  (issues/055 already documents that one).
- `cg_ctype_width` (cg.cc) — already distinguishes `-1` unknown from
  `-2` absent, which is the RIGHT pattern and worth copying.
- `prim_period_offset` — uses `-1` for "no offset" and `kOffsetAmbiguous`
  (`-2`) for "more than one", again the right pattern.
- `basic_type` (clone.cc) — returns `nullptr` for BOTH "all basic, no
  disagreement" and "non-basic", which ifa/126 found makes every record
  class interchangeable for cloning.
- `concrete_type_set_to_type` — an empty set returns `sym_void`, which
  is then indistinguishable from a genuine `void`.
- `sizeof_element` / `_CG_prim_new`'s `sizeof(*((_c)0))` on a
  zero-field record — issues/055 already records that an incomplete type
  will not compile there.

## The fix shape

Where the distinction matters, use a sentinel that cannot be a real
answer — `cg_ctype_width`'s `-1`/`-2` and `prim_period_offset`'s
`kOffsetAmbiguous` are both in-tree precedents. Where a sentinel is
impractical, the consumer must REFUSE rather than proceed: a zero-size
allocation is never a correct answer for a container that is about to be
written.

## Verification plan

1. Enumerate the sites; for each, say whether a zero/empty result is
   possible and what the consumer does with it.
2. For any that proceed on it, add either a sentinel or a refusal, and a
   test that reaches it.
3. The two known instances have tests already —
   `tests/comprehension_index_untypes_list.py` (124) and chess's
   `[None] * 0x80` (118, corpus-only). A reduced repro for the second
   would be worth having in `tests/`.

## What this unblocks

Nothing directly; it is a class of silent wrong answers rather than a
blocker. But both known instances were expensive to find — 124 took a
delta-reduction and a five-stage measurement, 118 needed a new
debugging mode — and both were invisible to `make test`. Finding the
rest by audit is much cheaper than finding them one crash at a time.
