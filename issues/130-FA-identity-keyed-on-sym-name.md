# 130 — identity is keyed on `Sym::name` in load-bearing places, and names are not unique

**Status:** open, audit with a fix order. Nothing is fixed yet (verified
2026-09-28: `CreationSet::var_map` is still `Map<cchar *, AVar *>`,
`fa.h:289`). Rewritten 2026-09-28; the full site census is in git:
`git show 3f36072b:ifa/issues/130-FA-identity-keyed-on-sym-name.md`.

**The rule** (AGENTS.md, "Never analyse or decide by NAME"): interning
makes a name a fast key, not an identity. Two classes in different
modules, a clone and its original, and an override and the member it
overrides all share a string. A name key MERGES what is distinct
(collision), and a lapse in interning MISSES what is there.

## A1 — `CreationSet::var_map`: member resolution through a name-keyed map

Filled at `fa.cc` while walking `s->has` (`put(h->name, iv)`) and read
by the `.field` selector path, record destructuring, and `clone.cc`.

- **Collision:** two members of one class with one name (closed/110's
  override duplicating a slot) overwrite each other, and one becomes
  unreachable.
- **Miss:** `clone.cc` carries the scar. It resolves by `strcmp` instead
  of the map because an equal-but-distinct string missed, and `bh`'s `EPS`
  lost its `float64` type to padding. The O(n·m) `strcmp` scans in
  `clone.cc` are the workaround, not independent defects.

**Fix:** the selector NAME is the query, which is correct for Python
attribute access. It must not be the KEY of the answer. Resolve name →
member `Sym *` once, through the hierarchy (`Sym::has` /
`Sym::specializes`), key `var_map` on `Sym *`, and delete the `strcmp`
scans. Where one name resolves to two members on one class, the
hierarchy decides (closed/110). Struct layout change, so `make clean`.

## A2 — the `PYC_CSELEM=3` shape key is built out of names

`atype_shape` emits `cs->sym->name` into a monotone, never-cleared canon,
so a collision is a permanent merge. **Moot if `PYC_CSELEM` is deleted**,
which [146](146-remove-all-arbitrary-splitting.md)'s inventory
recommends. If it is kept, use `name#id`.

## A3 — the classtag is keyed on the class NAME

`_CG_type_<name>` is emitted once per name (`cg.cc`, the
`emitted_types` set; LLVM `get_classtag_global(name)`), so the tag
equivalence class IS name equality. Both dispatch-table sites that
`strcmp` names are then correct consequences (dedup: one tag, one
branch; collide-bail: two implementations under one tag cannot be told
apart). **The defect is the tag:** it was meant to be shared by CLONES of
a class, and it also fuses unrelated same-named classes. pyc records no
clone lineage (`Sym::clone()` copies every field), so the fix is to add
one where clones are minted in `determine_types`
(`s->tag_family = sym->tag_family ?: sym->id`) and key the tag and both
compares on it. Latent: no observed failure, silent when it bites. Do NOT
replace either `strcmp` alone, since that creates a dead second branch
on one tag.

## C — one probe keys on names

`report_cs_population` buckets by `cs->sym->name`, merging distinct syms
in a measurement people reason from. Key it on `Sym *` and print
`name#id` (the model: `element_census`, and the `ELEMTYPE` sort by name
then id).

(B, pyc builtin names baked into generic ifa such as `__pyc_exc__`,
`print` and `__closure__`, belongs to the `IFACallbacks` line of work,
closed/082-084.)

## Order

A1, then C (one line), then A3. A2 only if `PYC_CSELEM` survives.

## Verification

A1: `bh`'s `EPS` stays `_CG_float64` with the `strcmp` scans deleted. A
fixture with an override duplicating a member name reaches both members.
Corpus `check`: watch `run_rc`, since a wrong member resolution shows up
as a crash. A3: a two-module program with same-named, unrelated classes
reaching one dispatch site dispatches correctly.
