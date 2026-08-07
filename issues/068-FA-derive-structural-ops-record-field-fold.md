# 068 — Derive structural ops (comparison/eq/hash/repr/copy) as a record field-fold: a `derive`-style macro expansion unifying classes and tuples

**Status:** design, 2026-07-24. Proposes the generic mechanism that
subsumes [067](closed/067-dijkstra2-heap-tuple-precision-and-use-before-def.md)
layer 3 (tuple/user-class-element comparison) and retires the
`tuple_lt`/`tuple_eq` primitives and their custom codegen. Grew out of
tracing why shedskin compiles these programs and pyc does not (see the
"Why shedskin works" section). **Affects:** `ifa/analysis/clone.cc`
(the tuple side / clone-phase body specialization),
`python_ifa_build_syms.cc`/`python_ifa_build_if1.cc` (the class side /
existing per-class synthesis, e.g. `__deepcopy__`),
`__pyc__/04_sequence.py` (retires the `tuple_lt`/`tuple_eq` primitive
methods), `ifa/codegen/cg.cc` (retires `emit_tuple_lt_expr` /
`emit_elem_lt` / the 067 part-B clone matching).

## Why shedskin works and pyc doesn't (the framing this fixes)

pyc lowers `tuple < tuple` to an **opaque `tuple_lt` primitive**,
specifically to dodge the union collapse a naive Python element loop
would hit (`self[i]` with a *variable* `i` merges a heterogeneous
fixed-arity tuple to the union of all slot types — see the comment at
`__pyc__/04_sequence.py:306`). The primitive dodges the collapse in
*codegen* (field-by-field on concrete types) but the price is that the FA
never sees the element comparisons, so the element method (`Vertex.__lt__`)
is never instantiated. A pure-tuple comparison then aborts at runtime with
`"unresolved tuple comparison"` (067 layer 3, repro `h1`).

shedskin never hits this because its analysis works one layer up:

- Its "transfer functions" are **model Python** (`lib/heapq.py`'s
  `__cmp(item, item)`, `builtin.py`'s `__cmp(a,b): a.__lt__(b); …`) that
  the same inference engine analyzes — so "compare the elements" is a
  **real call node** CPA instantiates like any call.
- Constant-index precision that avoids the collapse is a **call rewrite**:
  `tuple2.__getitem__(0) → __getfirst__` / `(1) → __getsecond__`
  (`infer.py:1196`), keeping the two positions distinct *in the call
  graph*.

pyc's transfer functions are **AType→AType** functions *below* the
dispatch/cloning layer; they can compute "result is `bool`" but cannot
originate a call/clone (those come only from IR `PNode`s + selector
`Var`s). So the same idea — instantiate the element comparison from the
`tuple_lt` transfer function — fights the layering (067 part A: had to
manufacture a `__lt__` selector AVar and fake a send; reverted).

**The fix is to stop fighting the layer: put the element comparisons back
at the ordinary-send level, generated structurally per type.** That is a
`derive`.

## The mechanism: records + field-folds

Treat **classes and tuples as one thing — a record**: an ordered list of
fields, each reachable by a **constant** key (named for classes,
positional for tuples). Every structural op is a **fold over the field
list**, each step an ordinary op on one field:

```
derive(Op, RecordType T)  ==>  a method that folds Op over T's fields,
                               each field-step being a NORMAL send.
```

"A normal send" is the whole point: it makes pyc's demand-driven engine
instantiate that field's own `Op` clone — the thing the opaque primitive
threw away, and exactly what shedskin's `__cmp(elem,elem)` model calls buy.

### The derivable ops (each a fold template)

| Op | Combiner over fields `f0..fn` | Field-step (a real send) |
|---|---|---|
| `__eq__` | AND, short-circuit on first `False`; different arity ⇒ `False` | `a.fi == b.fi` |
| `__lt__`/`__cmp__` | lexicographic; first differing field decides, else shorter-is-less | `a.fi < b.fi`, `b.fi < a.fi` |
| `__hash__` | `hash_combine` fold | `hash(a.fi)` |
| `__repr__` | string concat with separators | `repr(a.fi)` |
| `__copy__`/`__deepcopy__` | reconstruct with each field copied | `a.fi.__deepcopy__()` |

pyc **already synthesizes `__deepcopy__` per record** — so this is
generalizing an existing per-class synthesis to cover comparison/eq/hash,
plus extending it to tuples, not new machinery.

### The one adapter that unifies class vs tuple

```
fields(T):                       # ordered, constant accessor per field
  class  -> [ (name, self.<name>) for name in T.fields ]   # constant attr
  tuple  -> [ (i,    self[i])     for i in 0..arity(T) ]    # CONSTANT index
```

The **constant** accessor is load-bearing: constant-index tuple access is
precise per slot (pyc's `e0/e1` are already structurally distinct; the
collapse only ever came from *variable* `self[i]`). The derive never emits
a variable index.

## Expansion & timing (the real hinge)

- **Classes — build time.** Field set is known after `build_syms`; each
  `self.<name>` step is a constant attr send. Pure frontend synthesis,
  exactly like `__deepcopy__` today. No FA involvement, no timing problem.

- **Tuples — per shape.** Arity is a *type* fact, not syntactic at the
  generic call site, so the fold must be per-shape. Options, increasing
  cleanliness:
  1. **Eager per-arity from literals** (most Rust-like): scan tuple
     literals for distinct arities, generate `__tuple_cmp_N` etc., dispatch
     `<`/`==` by arity. Frontend-only; misses non-literal tuples.
  2. **Clone-phase body specialization**: pyc already clones `tuple.__lt__`
     per concrete tuple type and fully duplicates the body — so a clone
     *can* mechanically carry an arity-N constant-index body. **But
     verified unviable alone** (see findings): the clone phase is post-FA
     and never re-dispatches, so element sends minted there are never
     instantiated. Only works paired with option 3's re-analysis.
  3. **Post-FA generate-then-reanalyze**: emit the per-shape functions once
     tuple types are known, re-run FA to instantiate the field clones.
     Clean extra phase (shedskin's iterate shape), biggest change.
  4. **Pre-FA generic recursive body** (added after the clone-phase
     finding — the recommended home): a single `tuple.__lt__` that compares
     `self[0]`/`t[0]` (constant, already precise per `h6`) then recurses on
     `self[1:]`/`t[1:]`; the FA unrolls per arity via the recursion, each
     step an ordinary element send. No clone/transfer-function/re-analysis.
     Prerequisites to verify: precise tuple-tail slicing + FA recursion
     termination.

## Why it solves 067

- Each field-step is an **ordinary send** ⇒ `V.__lt__` (or any field op) is
  instantiated by **normal demand-driven cloning** — shedskin parity.
  **067 part A dissolves** (no transfer-function synthesis / selector
  manufacturing).
- The generated body is normal IR/codegen ⇒ the element call is emitted the
  ordinary way. **067 part B (per-call-site clone matching) is retired.**
- **Constant** field/slot access ⇒ no union collapse ⇒ the
  `tuple_lt`/`tuple_eq` **primitives and their custom codegen go away.**
- **Recursive & uniform**: a field that is itself a tuple/record hits the
  same derive; `(float, (int, V))` just works.

## Semantics guards (stay Python, not Rust)

- **Tuples**: always structural comparison — that *is* Python tuple
  semantics.
- **Classes**: Python default is **identity `__eq__`, no `__lt__`**. A class
  with no user override gets identity `__eq__` (the trivial derive) and NOT
  a field-wise one; `<` on it stays an error. Field-wise value comparison
  for classes is **opt-in** (dataclass-like), never automatic. For the
  dijkstra2 case the object already has its own `__lt__`, so the tuple's
  field-step just calls it — only the tuple derive is needed.
- Cross-arity / common-prefix rules live in the fold combiner (already
  implemented in the current primitive's codegen — porting that logic up
  into the generated fold).

## Class-side derive — LANDED (2026-07-24)

`@pyc_compare` derives the whole record **comparison/ordering family** as
field-folds of ordinary sends, standing up the fold-template framework.

- **Trigger:** `@pyc_compare`, recognized by bare name in the PY_classdef
  decorator scan (`python_ifa_build_if1.cc`), mirroring `@pyc_struct`;
  passed to `gen_class_pyda` as `derive_compare`. CPython shim
  `pyc_compat.pyc_compare` gives matching semantics (field-dict `__eq__`,
  lexicographic `__lt__`, `functools.total_ordering` for the rest =
  `dataclass(order=True)` for a totally-ordered record) so cross-verify
  agrees.
- **Synthesis:** `synthesize_derived_compare` (`python_ifa_build_syms.cc`),
  the binary generalization of the `__deepcopy__` synthesis, generates six
  methods, all from **ordinary sends** (period-gets + the field's own
  comparison + bool combinators) — no primitive, no inline codegen:
  - `__eq__`: AND fold `r = True; r = r & (self.f == other.f)`.
  - `__lt__`: **lexicographic, straight-line** (no branches) —
    `r = False; r = (self.f < other.f) | ((self.f == other.f) & r)` with
    fields folded in reverse (`bool.__and__`/`__or__`).
  - `__ne__`/`__gt__`/`__le__`/`__ge__`: delegate to derived
    `__eq__`/`__lt__` (mirrors tuple's own reflected ops).
  Each op is skipped when the class defines its own.
- **Verified:** `tests/derive_eq.py` (a `Point` with int fields and a `Rec`
  with int **and** str fields — proves each field dispatches to its own
  `__eq__`: `int.__eq__` / `str.__eq__`) and `tests/derive_order.py` (the
  full `< > <= >= != ==` family, incl. lexicographic first-field-decides)
  both compile clean and match CPython. Full suite **229/0** (227 + two
  tests) — zero regressions, since the derive is opt-in.

This confirms the field-fold framework end-to-end for the comparison
family, and the key structural shape the tuple side needs: **element
comparison as an ordinary send** dispatching to the element's own method.
Follow-ons: `__hash__` (a combine fold — deferred because hash *values*
differ pyc-vs-CPython, so it can't be validated by output cross-verify the
way comparison can) and — the actual 067 target — porting the fold to the
**tuple** side via the pre-FA recursive head/tail body (see findings
above).

## De-risking order

1. **Class-side derive first** — frontend-only, immediately useful
   (dataclass-style value types), stands up the fold-template framework
   with zero FA risk.
2. **Clone-phase feasibility (option 2)** — settle whether a clone can carry
   a shape-specialized body; that decides whether tuples get the elegant
   path or need a new phase. (Verification appended below.)

## Clone-phase feasibility (option 2) — findings (2026-07-24, verified against `clone.cc`)

**Verdict: option 2(b) as literally "specialize the body at the clone
phase" does NOT work — the timing is fatal. The body-duplication
machinery is real (the mechanical intuition was right), but the clone
phase runs POST-FA, and nothing after the FA can instantiate a new
method clone.**

What was confirmed:

1. **Bodies ARE fully duplicated per clone.** `Fun::copy()`
   (`ifa/if1/fun.cc:160`) rebuilds a fresh Fun: `copy_pnode` for *every*
   PNode, a fresh `nmap` (old→new PNode) and `vmap`, remapped CFG, args,
   rets, vars. `clone_functions()` (`clone.cc:1081`) calls `f->copy()` per
   equiv_set. So each clone genuinely owns its whole body — swapping in a
   shape-specialized body is mechanically possible.

2. **But the clone phase is strictly POST-FA and never re-dispatches.**
   `ifa_analyze` (`ifa/ifa.cc`) runs `fa->analyze(...)` (the FA fixpoint,
   :47) and only *then* `clone(fa)` (:56) — one FA, then clone, no
   re-analysis. Inside clone: `clone_functions` (1103-1135) merely
   *rebuilds* `Fun::calls` from **existing** edges, and `concretize_types`
   only turns already-inferred AVar types into concrete Syms — neither
   makes an `EntrySet`, an `AEdge`, or runs dispatch.

3. **Therefore a body specialized/generated at clone time introduces
   element sends the FA never saw.** A method clone (`V.__lt__`) exists
   only if the FA created a `V.__lt__` EntrySet, which happens only from a
   real send **during** the FA. New sends minted at clone time get no
   EntrySet ⇒ no clone ⇒ `V.__lt__`'s body is never even type-concretized
   ⇒ codegen has nothing to call. (This is the same wall as 067 part A,
   reached from the other side: instantiation must happen during the FA.)

**So option 2(b) is only viable as part of option 3** — specialize/generate
the per-shape bodies, then **re-run the FA** so the element sends get
EntrySets. That is a real added phase (the shedskin "iterate" shape).

**The better news — the elegant home is PRE-FA, and its one prerequisite
already holds.** The whole reason to want per-clone specialization was
constant-index precision; but **pyc already infers a constant tuple index
precisely** (repro `h6`: `t[1] < u[1]` on a `(float,V)` tuple instantiates
`V.__lt__` and runs correctly). And every real pyc tuple has a statically
known arity (a dynamic-length `tuple(iterable)` lowers to a **list**, not a
tuple — `python_ifa_build_if1.cc:610`). So the collapse-free element
comparisons can be written **before** the FA, in a generic `tuple.__lt__`
body that the normal FA then instantiates — no clone-phase surgery, no
transfer-function synthesis, no re-analysis. Two pre-FA shapes:

- **Recursive head/tail** (one generic body, no arity enumeration):
  `__lt(self[0], t[0])` then recurse on `self[1:] < t[1:]`; the FA unrolls
  it per arity through the recursion, each step a real constant-index
  element send. Remaining prerequisites to verify: precise tuple-**tail**
  slicing (`self[1:]` → a smaller precise tuple) and FA **termination** on
  the decreasing-arity recursion.
- **Per-arity generated bodies** from the (statically known) literal
  arities: `__tuple_lt_N` with `self[0..N-1]` constant compares. Needs the
  generic `tuple.__lt__` to route each concrete tuple type to its
  arity body **with arity narrowing** — per-EntrySet the self type is
  already a single tuple type, so within a clone `self[k]` is precise;
  the open question is whether the dispatch to `__tuple_lt_N` narrows
  cleanly (a `len(self)`-branch does NOT narrow the tuple arity type in
  pyc, so this needs a real arity dispatch, not an `if`).

**Revised plan.** Drop clone-phase specialization (option 2b) as the home;
it can't instantiate without a re-analysis. Pursue the **pre-FA recursive
head/tail `tuple.__lt__`/`__eq__`** (constant-index precision is already
proven), and verify the two remaining prerequisites (precise tail slice +
recursion termination). The class-side derive (build-time, from the field
set) is unaffected and still the zero-risk first step.
