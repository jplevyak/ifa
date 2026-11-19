# 130 — identity is keyed on `Sym::name` in load-bearing places, and names are not unique

**Status:** closed 2026-10-15. A3 and A1 are fixed (see "A3" and "A1,
fixed" below), A1's `strcmp` scans are gone, C is fixed, and A2 was
already fixed in the tree (both keys of the shape canon spell `name#id`).
Deleting `PYC_CSELEM` itself is [146](../146-remove-all-arbitrary-splitting.md)'s
verdict, not this issue's. Rewritten 2026-09-28; the full site census is in git:
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

### A1, fixed (2026-10-14): resolution by the MRO, in both places a class answers a name

Two places answer "what does `x.name` mean on this class", and both
disagreed with CPython:

1. **Prototype construction** (`gen_class_pyda`). It now copies each
   inherited attribute ONCE, from the first class in the class's C3 MRO
   that defines it, and not at all if the class defines it itself. A
   class's OWN members are those whose `in` is the class (its `has` can
   already hold inherited ones, which must not count as shadowing). The
   MRO comes from `c3_mro` (`if1/ast.cc`), the linearization ifa's
   dispatch already computes, split out of `c3_linearization` so it can
   run before the type hierarchy is final without freezing its cache.
2. **Selector dispatch** (`P_prim_period`, `fa_prims.cc`). A receiver whose
   member of the name holds only DATA is answered by that member and left
   out of the selector dispatch: data on a class shadows an inherited
   method. A receiver whose member is a method keeps the selector dispatch
   (`super()` reaches the base method that way; leaving method receivers
   out broke ten suite tests). And where two candidates' formals are
   unrelated types -- both bases of the argument's class -- `subsumes_arg`
   (`if1/pattern.cc`) now prefers the one earlier in the ARGUMENT's C3
   linearization (`dispatch_types`), which is what C3 is for; it was an
   ambiguous call.

`tests/inheritance_resolution_mro.py`: the three override rows, a data
attribute under two bases and through a diamond, a method under two
bases and through a diamond, and an inherited method reading an
overridden attribute -- all match CPython on both backends. Before:
three refusals, one silent wrong value (`B(C, D)` read D's `x`), and two
ambiguous calls.

The name stays the query throughout (Python's lookup is by name along the
MRO); what answers it is a member `Sym *` and a class order. `make test` green (465 on each backend); corpus check -R
`check__default__5fbbab3f+50288b47` identical on all 77.

**What is left of A1** is representation, not behaviour. `var_map` is
still keyed on the member's name and `clone.cc` still resolves by
`strcmp`. Since `collect_include_vars` replaces an override in place, a
class's members have unique names, so name -> member is one-to-one per
class and re-keying would change no answer. It is worth doing for the
`strcmp` scans (the "miss" half: a distinct-but-equal string), not as a
fix.

**Done 2026-10-15, and re-keying on `Sym *` was the wrong half.** A
lookup arrives as a selector STRING (`P_prim_period`'s symbol), and Python
resolves it by name along the MRO, so a `Sym *`-keyed map would still need
a name-to-member step per class, which is the map we have. What the
`strcmp` scans guarded against was a lapse in interning, so that was
measured: a probe at every `var_map` put (the CS constructor,
`creation_point`, the tuple fill, `promote_field_one`) and at the
`P_prim_period` query reported **zero** un-interned names over all 459
suite tests plus bh, chull, richards and pygmy. Member names are interned
at Sym creation (`if1_cannonicalize_string`), so pointer identity on the
name is exact. `apply_prefix_layout` and the conflict scan in
`clone.cc` now compare name pointers and use `cs->var_map.get`. bh's
`EPS` stays `_CG_float64` (this section's verification criterion). The
four `strcmp`s left in `clone.cc` are sort comparators, which order
the emitted layout and decide no identity. `make test` green (465 on each backend); corpus check -R
`check__default__4f0de62f+913fcfe1` identical to `5fbbab3f+50288b47`
on every column for all 77 programs, ess and css included.

## A2 — the `PYC_CSELEM=3` shape key is built out of names

`atype_shape` emits `cs->sym->name` into a monotone, never-cleared canon,
so a collision is a permanent merge. **Moot if `PYC_CSELEM` is deleted**,
which [146](../146-remove-all-arbitrary-splitting.md)'s inventory
recommends. If it is kept, use `name#id`.

**Already fixed (found 2026-10-15).** `atype_shape_id` emits each
member class as `name#id`, and `cselem_shape_key` keys the allocated
class the same way. Both carry comments citing this section, so no
collision can merge two classes in the canon.

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

**Fixed 2026-10-15.** CSPOP buckets on `Sym *`, prints `name#id`, and
breaks ties by id. (richards: `closure#59:160 list#62:6 ...`.)

(B, pyc builtin names baked into generic ifa such as `__pyc_exc__`,
`print` and `__closure__`, belongs to the `IFACallbacks` line of work,
closed/082-084.)

## Order

A3, A1 (behaviour, then the `strcmp` scans), and C are all done. A2 was
already done.

## Verification

A1: `bh`'s `EPS` stays `_CG_float64` with the `strcmp` scans deleted. A
fixture with an override duplicating a member name reaches both members.
Corpus `check`: watch `run_rc`, since a wrong member resolution shows up
as a crash. A3: a two-module program with same-named, unrelated classes
reaching one dispatch site dispatches correctly.
