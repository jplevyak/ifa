# 069 — Scope: per-arity tuple types, so the class-side derive dispatches tuple comparison per shape

> ## LANDED (2026-07-24): option 2' shipped — plain-Python tuple `__eq__`/`__lt__`, 067 layer 3 solved
>
> `__pyc__/04_sequence.py`'s tuple `__eq__`/`__lt__` are now the len-guarded,
> constant-index, unrolled (max arity 16) plain-Python folds — no primitive.
> Results:
> - **067 layer 3 solved**: `h1` (heap of `(float, V)`) compiles clean (no
>   "unresolved tuple comparison") and matches CPython; `V.__lt__` is
>   instantiated from the ordinary `self[i] < t[i]` send — **no codegen
>   hacks** (067 part B is now dead). dijkstra2's heap-tuple assert is gone
>   (its remaining 12 no-types are layers 1/2/4).
> - **Zero regressions**: full suite **229/0**; corpus **51 compiled,
>   identical per-example status to baseline** (stereo's 16-tuples →
>   COMPILED_C). Edge cases verified vs CPython: nested tuples (recursion
>   via dispatch), cross-arity shorter-is-less, `sort` of tuples.
> - **Caveat**: fixed max arity 16 (covers the corpus's largest literal).
>   A tuple compared beyond 16 slots would silently ignore the tail; the
>   robust removal of the bound is a per-program unroll count (frontend
>   generation) — follow-on. The `tuple_lt`/`tuple_eq` primitives + their
>   FA transfer + `emit_tuple_lt_expr`/067-part-B codegen are now **dead**;
>   deleting them is a separate cleanup.
>
> Option 1 below is **not needed** — kept as the documented alternative.
>
> ## RESULT (2026-07-24): scoping option 1 found a far cheaper fix — do "option 2'" instead
>
> The viability probes at the bottom of this doc (the "smaller alternative")
> came back **strongly positive**, and led to an even simpler variant that
> needs **no representational change at all** — just a rewrite of
> `tuple.__lt__`/`__eq__` in `__pyc__/04_sequence.py`:
>
> **Option 2' — a single generic `tuple.__lt__` with `len`-guards, unrolled
> to a fixed max arity, in plain Python:**
> ```python
> def __lt__(self, t):
>     n = len(self)
>     if n >= 1:
>         if self[0] < t[0]: return True
>         if t[0] < self[0]: return False
>     if n >= 2:
>         if self[1] < t[1]: return True
>         if t[1] < self[1]: return False
>     ...  # up to a fixed max arity
>     return False
> ```
> Verified end-to-end (`scratchpad/opt2p/p.py`): compiles clean, runs
> correct vs CPython for arity 2/3/4 **including a user-class `V` element**,
> and **`V.__lt__` is instantiated (8×)** — i.e. it solves 067 outright.
> Why it works, all already-established facts:
> - constant-index tuple access is precise (`h6`);
> - `len(self)` **constant-folds per contour** to the tuple's arity, so
>   `n >= K` prunes the over-arity branches — per-arity specialization for
>   free, no generation;
> - a constant **out-of-range** index (and comparisons on it) in a
>   surviving sibling branch is **benign** (probes A/B, `opt2/t.py`), and
>   per-contour cloning keeps wrong-arity analyses separate — no collapse;
> - each `self[i] < t[i]` is an **ordinary send**, so element method clones
>   instantiate through the normal FA (067 part A dissolves) and codegen
>   emits them normally (067 part B / the `tuple_lt` primitive retire).
>
> **Cost:** editing `__pyc__/04_sequence.py`'s tuple `__lt__`/`__eq__`
> (+`__hash__`?) and deleting the `tuple_lt`/`tuple_eq` primitives + 067
> part B. **No** per-arity types, **no** shared V/pyc infra change, **no**
> clone/transfer-function surgery.
>
> **Only caveat:** the fixed max arity — tuples larger than the unrolled
> bound compare only the first N slots. Mitigate with a generous bound
> (measure the corpus's max literal arity; it's small), or have the
> frontend set the unroll count to the program's max literal arity
> (per-program, still tiny). This retires **option 1 below** unless the
> fixed bound proves unacceptable.

**Status:** scoping, 2026-07-24. This is "option 1" from the
[067](067-dijkstra2-heap-tuple-precision-and-use-before-def.md) /
[068](068-derive-structural-ops-record-field-fold.md) discussion: give each
tuple **arity** its own dispatchable record type so the class-side derive
(landed in 068) applies to tuples verbatim — retiring the `tuple_lt`/
`tuple_eq` primitives and 067's whole layer-3 problem. **Affects (shared V
+ pyc infrastructure):** `python_ifa_build_if1.cc` (tuple make sites),
`python_ifa_build_syms.cc` (derive attachment), `__pyc__/04_sequence.py`
(`class tuple` restructure), `ifa/analysis/fa.cc` (make/dispatch, drop the
tuple_lt/eq transfer), `ifa/codegen/cg.cc` (drop `emit_tuple_lt_expr` +
067 part B), `ifa/frontend/ast_to_if1.cc` (V's tuple make), and the vestigial
`PycCompiler::tuple_types`.

## Why recursion is out (settled)

The pre-FA recursive `tuple.__lt__` (compare `self[0]`, recurse on
`self[1:]`) is dead: a heterogeneous tuple **tail slice collapses** —
`(1,"a",2.0)[1:]` fails to compile in pyc (`illegal ... ( int64 float64 )`),
because pyc has no fixed-arity slice (dynamic slices must return a list,
which is homogeneous). So there is no precise decreasing-arity recursion
for the splitter's recursive-ES machinery to unroll. The non-recursive,
per-shape, constant-index shape the class derive already uses is the way.

## What already exists (this shrinks the change)

- **Per-shape tuple type Syms already exist** as `sym_tuple->specializers`
  — `Type_RECORD`, positional `->eN` fields, made via
  `must_implement_and_specialize(sym_tuple)` + `has` (see
  `ast_to_if1.cc:714` for V's tuple-pattern types). The FA/codegen "is a
  tuple" checks key on `sym_tuple->specializers.set_in(...)`
  (`fa.cc:1709`, `cg.cc:259`), so a new per-arity specializer is *already*
  recognized as a tuple by those sites.
- **Method dispatch keys on `cs->sym`** (`cs->var_map`, `function_dispatch`
  on the receiver CS). So if a tuple CS's `sym` were a per-arity
  `sym_tupleN` instead of abstract `sym_tuple`, dispatch would route to
  `sym_tupleN`'s methods — including a per-arity derived `__lt__`.
- **`creation_point(container, kind, l)`** already takes the type `kind`
  and builds `l` slots; passing `sym_tupleN` instead of `sym_tuple` needs
  no FA-make change.
- **The class derive** (`synthesize_derived_compare`, 068) is the method
  generator; **`tuple_types`** (`Map<int,Sym*>`, currently dead) is the
  ready-made per-arity cache.
- **Constant-index element access is precise** (repro `h6`), so the derived
  tuple `__lt__` can compare `self[i]`/`other[i]` (constant `i`) directly —
  no need to name slots.

## The core change

At each tuple **literal** (arity N known syntactically — dynamic-length
`tuple(iterable)` already lowers to a *list*), create/reuse a per-arity
type `sym_tupleN` and `make sym_tupleN` instead of `make sym_tuple`:

- `sym_tupleN`: `Type_RECORD`, `must_implement_and_specialize(sym_tuple)`
  (so all existing tuple checks + the shared `__getitem__`/`__len__`/
  `__iter__`/`__add__`/slice/`__str__` methods are inherited), N slots, and
  a **derived** `__eq__`/`__lt__`/`__ne__`/`__gt__`/`__le__`/`__ge__`
  attached via `synthesize_derived_compare` (using constant-index field
  access `self[i]`).
- Dispatch of `a < b` then routes to `sym_tupleN.__lt__`, whose element
  steps are ordinary sends → the element method clones instantiate through
  the normal FA (067 part A dissolves), and codegen emits the derived
  method normally (067 part B / the `tuple_lt` primitive retire). Nested
  tuples recurse **through dispatch** (a tuple field's `<` hits *its*
  arity's derived `__lt__`) — no slicing.

## Blast radius, categorized

1. **pyc make sites** (`build_if1.cc` ~5: PY_tuple literal 3428, the
   `sym_make, sym_tuple` sends, empty-tuple 726): select `sym_tupleN`.
   Small, mechanical.
2. **Per-arity type factory** (new): `get_or_make_tuple_type(N)` — cache in
   `tuple_types`, create the record + specialize + attach the shared-method
   linkage + run the derive. The one genuinely new piece.
3. **Derive attachment to a synthetic type**: `synthesize_derived_compare`
   currently installs the method's prototype via a setter on the
   *class-body init fn* (`fn->self`). A synthetic tuple type has no class
   body, so the prototype-install path needs a synthetic-type variant.
   Medium; the rest of the derive is reused as-is.
4. **`__pyc__/04_sequence.py class tuple`**: drop `__eq__`/`__lt__` (and the
   reflected `__ne__`/…); keep it as the base carrying the shared ops that
   `sym_tupleN` inherits.
5. **Drop the `tuple_lt`/`tuple_eq` primitives**: `fa.cc` transfer
   (2406-2417), `cg.cc` `emit_tuple_lt_expr`/`emit_elem_lt`'s tuple arm +
   the 067 part-B clone-matching, `cg_emit_llvm.cc` mirrors, `prim_data.cc`
   registration. Net *deletion*, once the derive covers every case.
6. **Shared FA/codegen tuple checks**: `fa.cc:1352` (`cs->sym != sym_tuple`),
   `fa.cc:1709-1710`, `cg.cc:259`/`cg_is_tuple_record` — audit each to
   accept `sym_tupleN` (mostly already do, via the specializer set).
7. **V frontend** (`ast_to_if1.cc`): V's tuple make also uses abstract
   `sym_tuple`. Either give V per-arity types too, or keep V on the
   `tuple_lt` primitive (dual path) during transition. **The main
   shared-infra risk** — the change is in ifa code both frontends use.
8. **isinstance / patterns / destructuring**: `isinstance(x, tuple)`,
   tuple match patterns, and destructuring targets (`build_syms:1246`
   sets `sym=sym_tuple`) must treat `sym_tupleN` as a tuple — again mostly
   via the specializer relationship, but each needs a check.

## Reused vs. new

- **Reused:** the class derive (068), `must_implement_and_specialize`,
  `creation_point`, the `specializers`-based tuple checks, `tuple_types`.
- **New:** the per-arity type factory (item 2) and the synthetic-type
  prototype-install variant of the derive (item 3).
- **Deleted:** the `tuple_lt`/`tuple_eq` primitives and their codegen +
  067 part B (item 5).

## Risks / open questions

- **Shared V+pyc infra** (item 7) is the dominant risk: this is not a
  pyc-only change unless V is kept on a dual path.
- **Empty/1-tuples**: `sym_tuple0` (cf. `sym_empty_tuple`) and `sym_tuple1`
  need the derive to handle 0/1 fields (the fold already degenerates
  correctly: empty ⇒ `__eq__` True / `__lt__` False).
- **Type-lattice churn**: many small `sym_tupleN` specializers instead of
  one `sym_tuple`; check the pattern-match / dispatch cost and codegen
  determinism (issue 035) don't regress.
- **Cross-arity comparison** (`(1,2) == (1,2,3)`): today the primitive
  returns False across arities; with distinct types, `a == b` dispatches on
  `a`'s arity and must still yield False when `b`'s arity differs — the
  derived `__eq__` must arity-check (or dispatch must handle the mismatch).

## Suggested incremental path (each step independently gated on 227/0 + corpus)

1. **Transparent per-arity types.** Introduce `sym_tupleN` + the factory;
   make pyc literals create them; keep the `tuple_lt`/`tuple_eq` primitives.
   Goal: *zero behavior change* — prove every existing tuple site treats
   `sym_tupleN` exactly like `sym_tuple` (via the specializer set). This is
   the make-or-break representational step; if it isn't transparent, stop.
2. **Attach + route the derive.** Run `synthesize_derived_compare` on each
   `sym_tupleN`; route `<`/`==` to the derived methods; keep the primitive
   as fallback. Validate on `h1`/heap-of-`(float,V)` (067) + the suite.
3. **Delete the primitive + 067 part B**, restructure `class tuple`, and
   resolve the V path.

## Smaller alternative worth a viability check first

Before committing to item 7's shared-infra change, it is worth *measuring*
whether "option 2" (one `sym_tuple`, `tuple.__lt__` branches by `len(self)`
into per-arity helper calls) is even viable — its blocker is whether a
**constant out-of-range** tuple index (`self[2]` on an arity-2 tuple,
analyzed in a sibling branch) is benign or a hard error. If benign, option
2 gets the same element-as-ordinary-send instantiation with **no
representational change**. One quick experiment (a 3-line probe) settles
it and could save the bulk of this scope.
