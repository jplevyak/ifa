# 130 — identity is keyed on `Sym::name` in load-bearing places, and names are not unique

**Status:** open. A3 fixed 2026-10-14 (see its section). Re-verified the
same day that A1, A2 and C are still in the tree, and that A1 is not
latent: see "A1, measured" below. Next: A1, by one attribute-resolution
table per class. Rewritten 2026-09-28; the full site census is in git:
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

### A1, measured (2026-10-14)

Not latent either. A subclass redefining an inherited attribute is
refused: `class A: f = 5` / `class B(A): def f(self)` ("illegal call
argument type"), `class A: x = 1` / `class B(A): x = "s"` ("mixed basic
types"), `class A: def f(self)` / `class B(A): f = 5` ("a number and an
object"). CPython runs all three. `gen_class_pyda` copies every base
member into the subclass's prototype by name and the subclass's body then
writes the same slot, so one slot holds the inherited value and then the
override -- a union over time CPython never has, since the override
SHADOWS. A probe skipping the copy of a member the class redefines fixed
the first two; the third also needs method lookup (`x.f`) to see that a
data member shadows an inherited method.

The principled fix is one attribute-resolution table per class, from the
C3 linearization pyc already computes: `(class, name) -> defining member
Sym *`. The name is the query (Python lookup is by name along the MRO);
the Sym is the answer and the identity. Prototype construction copies
only what the table resolves to a base, method lookup consults it,
`var_map` keys on the resolved Sym, and `clone.cc`'s `strcmp` scans go.

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

**Fixed 2026-10-14, and it was not latent.** Two modules each defining
`class Node`, reaching one dispatch site and one `isinstance`: both
backends printed `True True` for `isinstance(x, a.Node), isinstance(x,
b.Node)` on every object (`tests/same_name_classes_two_modules.py`, which
fails on the old tree). No lineage field was needed: a concrete class type
already names the class it was cloned from, its CreationSets' `sym`, so
`cg_tag_family(t)` is `t->creators[0]->sym` (or `t`), and `cg_tag_name(t)`
is `_CG_type_<name>_<family id>`. Every tag site in both backends uses it
-- the definitions (one per family), the prototype stamp, `isinstance`,
and the dispatch arms -- and the two `strcmp` sites (arm dedup and the
carrier collide-bail), as well as the dispatch-arm plans (ifa/121), now
compare families. The type object keeps the bare name as its printable
string. `make test` green (464 on each backend); corpus check -R
`check__default__3e5d6ac1+3e52bf31` identical on all 77.

## C — one probe keys on names

`report_cs_population` buckets by `cs->sym->name`, merging distinct syms
in a measurement people reason from. Key it on `Sym *` and print
`name#id` (the model: `element_census`, and the `ELEMTYPE` sort by name
then id).

(B, pyc builtin names baked into generic ifa such as `__pyc_exc__`,
`print` and `__closure__`, belongs to the `IFACallbacks` line of work,
closed/082-084.)

## Order

A3 (done), then A1, then C (one line). A2 only if `PYC_CSELEM` survives.

## Verification

A1: `bh`'s `EPS` stays `_CG_float64` with the `strcmp` scans deleted. A
fixture with an override duplicating a member name reaches both members.
Corpus `check`: watch `run_rc`, since a wrong member resolution shows up
as a crash. A3: a two-module program with same-named, unrelated classes
reaching one dispatch site dispatches correctly.
