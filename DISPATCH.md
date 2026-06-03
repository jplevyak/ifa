# DISPATCH — Pattern Matching & Dispatch Resolution

A working reference for `ifa/if1/pattern.cc` + `pattern.h` — the heart
of call-site dispatch. 1568 lines of dense code; this doc is the map
that lets you read it without re-deriving the design.

Sister docs: [IR.md](IR.md) (Sym/Fun/MPosition foundation),
[IFA.md](IFA.md) §5.5 (who calls `pattern_match`),
[CLONE.md](CLONE.md) §7.3 (how cloning consumes the matches).

---

## 1. What this module does

When the IFA analysis encounters a send `f(a, b, c)`, it must answer:
**"which `Fun`s could this call?"** The pattern matcher gets the receiver
plus the argument AVars and returns a vector of `Match*` — one per
candidate Fun, with per-argument *filters* recording which CreationSets
of each argument would dispatch to that Fun.

The match step is the bridge between data-flow (which produced the
arguments' types) and control-flow (which functions the call edge
should target). All overload resolution, generic instantiation,
coercion, promotion, default-argument supply, and argument-reordering
happens here.

**Why method dispatch matters for splitter precision.** Sends
resolved through this module create per-call EntrySets that the
splitter can fork via type-stage and mark machinery, giving each
receiver-CS its own specialized callee resolution. Primitive sends
([PRIMITIVES.md](PRIMITIVES.md) §13.12) bypass this entirely —
a primitive's transfer function unions all incoming-CS contributions
into one result AVar in the caller's ES, with no per-CS specialization
opportunity. Frontend authors who need polymorphic indexing,
subscripting, or container access should lower to method dispatch
(`obj.__getitem__(i)`) rather than raw primitives
(`prim_index_object`) when CS identity carries real type information
downstream. The simple inliner ([OPTIMIZE.md](OPTIMIZE.md)) removes
the method-call overhead in monomorphic cases.

---

## 2. Vocabulary

| Term | Meaning |
|---|---|
| **Argument** (actual) | A value passed at a call site (an `AVar`). |
| **Parameter** (formal) | A slot in a function definition (a `Sym` in `Fun::sym->has`). |
| **Position** | A path through nested patterns to reach a slot. Encoded as `MPosition`. |
| **MPosition** | A vector of small ints (position indices) and pointers (names, dispatch keys). Globally interned. |
| **`acp` / `acpp` / `acnp`** | actual / cannonicalized / positional / position. The 1-3 letter prefix names in pattern.cc encode origin: `a`=actual, `f`=formal, `c`=cannonical, `p`=positional, `n`=named. See top-of-file comment. |
| **Pattern** | A formal parameter that itself decomposes (e.g., `f((x, y))` matching a tuple). `Sym::is_pattern`. |
| **PMatch** | Internal Match-in-progress; carries all the substitution/filter maps used during resolution. Subclass of public `Match`. |
| **Match** | Result of resolution: `(Fun*, formal_filters, visibility_points, is_partial)`. Consumed by `fa.cc:make_AEdges`. |
| **MType** | Per-Sym (`s->match_type`) lookup table: `MPosition → Vec<Fun*>`. The reverse index from "type of an argument at position p" to "functions whose dispatch could fire." |
| **Dispatch type** | The type used for matching (`dispatch_type(a)` in pattern.cc:130 — symbols, `must_specialize`, or `sym_any`). |

---

## 3. `MPosition` (`pattern.h:21`)

The single most important type in this file. An `MPosition` identifies
where in a nested argument list a value goes.

```c
class MPosition : public gc {
  Vec<const void *> pos;       // path: small ints + pointers
  MPosition *cp, *up, *next, *down;
};
```

The `pos` vector mixes:
- **Integer indices** (positional arguments) stored as `int2Position(i)`
  — bit-trick `(void*)(intptr_t)-i`. `is_intPosition(p)` checks if a
  slot is an integer index.
- **Pointers** to interned strings (named arguments) — `set_top(name)`
  stores the name in place.

Helper methods (the small DSL for building positions):
- `push(int i)` — descend into a sub-pattern; pushes `int2Position(i)`.
- `push(const void *p)` — push a pointer (e.g., a name).
- `pop()` — back up one level.
- `inc()` — increment the last index (move to next argument at this
  level).
- `set_top(p)` — replace last entry (positional → named).
- `last()` / `last_is_positional()` / `is_positional()` — predicates.

### 3.1 Canonicalization

`cannonical_mposition` (`pattern.cc:106`) is a `ChainHash` interning
table. Every distinct position has exactly one canonical `MPosition*`.
The pointer is the identity; equality is pointer equality.

```c
MPosition *cannonicalize_mposition(MPosition &p);
```

Always call this before storing an `MPosition` in a map key. The
`MPositionHashFuns` class hashes by `pos` content; equality is by `pos`
equality.

The `cp` field on a non-canonical MPosition caches its canonical form
(`CANNONICAL_MPOSITION` is the sentinel for "this IS the canonical
copy"). The `up`/`down`/`next` fields form a tree of canonical
positions — `cp->up` is the parent position, `cp->down` is the
first-child position, `cp->next` is the next-sibling. This lets the
matcher walk neighbours in O(1).

### 3.2 The shape of a typical MPosition

For `f(a, b, c)`:
- `f` itself → `[1]`
- `a` → `[1, 1]` — first arg of position 1 (the function position)... wait

Actually, formal arg `1` is `f` (the receiver/function), and args
start at position 2 from the analysis perspective. So:
- formal[0] (the function) → `[1]`
- formal[1] (`a`) → `[2]`
- formal[2] (`b`) → `[3]`
- formal[3] (`c`) → `[4]`

For `f((x, y), z)` where the first formal is a tuple pattern:
- formal[1] (the pattern) → `[2]`
- formal[1].x → `[2, 1]`
- formal[1].y → `[2, 2]`
- formal[2] (`z`) → `[3]`

The naming convention (top of `pattern.cc`):

```
ABCD
  A = a | f, actual or formal (MANDITORY)
  B = c, cannonical (OPTIONAL)
  C = p, positional (OPTIONAL)
  D = p, position  (MANDITORY)

acpp = actual, cannonical, positional position
acnp = actual, cannonical, named position
ap   = actual non-cannonical, positional or named position
```

When reading pattern.cc, parse variable names as this code.

---

## 4. `Match` and `PMatch`

### 4.1 Public `Match` (`pattern.h:101`)

```c
class Match : public gc {
  Fun *fun;
  Map<MPosition *, AType *> formal_filters;
  Vec<PNode *> visibility_points;
  unsigned int is_partial : 1;
};
```

- `fun` — the matched Fun.
- `formal_filters[p]` — for positional position `p`, the AType that the
  receiving formal would accept. The analysis uses this to filter the
  argument's AVar into an EntrySet's typed parameter (see
  `analyze_edge` in `fa.cc`).
- `visibility_points` — PNodes from which this Fun is visible (used by
  frontends that scope visibility, e.g., V's module system).
- `is_partial` — true if this is a partial application (some arguments
  missing).

`Match::merge(m)` (`pattern.cc:117`) unions two Matches' filters and
visibility points. Used when multiple call edges land at the same
`(from, pnode, fun)` triple — they may need to share a Match.

`Match::cache_copy()` returns a fresh `Match` (not PMatch) with the
publically-visible fields copied. Used by the match cache.

### 4.2 Internal `PMatch` (`pattern.cc:27`)

`PMatch` extends `Match` with all the intermediate machinery the
resolver needs:

```c
class PMatch : public Match {
  Map<MPosition *, MPosition *> actual_to_formal_position;
  Map<MPosition *, AVar *>      actuals;
  Map<MPosition *, Sym *>       formal_dispatch_types;
  Map<MPosition *, AType *>     actual_filters;
  Map<MPosition *, MPosition *> actual_named_to_positional;
  Map<MPosition *, MPosition *> formal_to_actual_position;
  Map<MPosition *, MPosition *> order_substitutions;
  Vec<MPosition *>              default_args;
  Map<Sym *, Sym *>             generic_substitutions;
  Map<MPosition *, Sym *>       coercion_substitutions;
  Map<MPosition *, Sym *>       promotion_substitutions;
};
```

The five `MPosition → MPosition` maps record argument reorderings (keyword
args, default-arg supply, generic instantiation). `Matcher::build`
(`pattern.cc:698`) collapses these into a fresh wrapper `Fun` via the
frontend callbacks (`default_wrapper`, `coercion_wrapper`, etc.).

PMatches are alive only during one `pattern_match` call. The public
`Match` produced via `cache_copy` is what survives.

---

## 5. The dispatch tables (`Patterns`, `MType`, `match_type`)

Before any matching can happen, the analysis builds reverse indices
that say "if argument at position `p` has type `T`, here are the Funs
whose formal-`p` could dispatch on `T`."

### 5.1 Build (`build_patterns`, `pattern.cc:1451`)

Called once by `fa.cc:initialize` via
`if1->callback->finalize_functions` ordering:

```c
void build_patterns(FA *fa) {
  fa->patterns = new Patterns;
  for (Fun *f : fa->pdb->funs) build_patterns(fa, f, 0);
}
```

For each Fun, walks `f->sym->has` and for each formal calls
`build_arg(fa, f, a, p)`, which:
1. Calls `insert_fun(fa, f, a, dispatch_type(a), p)` — adds `f` to the
   per-type `MType::funs[p]` index.
2. If `a->is_pattern`, recurses into `a->has`.

`dispatch_type(a)` (`pattern.cc:130`) returns the type that the formal
dispatches on:
- `is_Sym_OUT(a)` → `sym_unknown_type`.
- `a->is_symbol` → `a` itself (constant-symbol dispatch).
- `a->type && a->type->is_symbol` → `a->type`.
- `a->must_specialize` (non-VARIABLE) → that constraint.
- Else → `sym_any` (matches everything).

The same Fun is inserted under both the positional MPosition (`cpnum`)
and a named MPosition (`cpname`) if the formal has a name.

After build:
- `fa->patterns->types` — every Sym that appears as a dispatch type.
- `fa->patterns->mtypes` — every `MType` (one per type Sym that any
  function dispatches on).
- `s->match_type->funs[p]` — for type `s` and position `p`, the Funs
  that dispatch on it.

### 5.2 Incremental `add_patterns` (`pattern.cc:1449`)

When a new Fun is installed (e.g., a default-arg wrapper from
`install_new_fun`), `add_patterns` is called and clears
`sym_match_cache`. The match cache is per-type; new Funs invalidate
existing cached match lists for the types they dispatch on.

### 5.3 `build_arg_positions` (`pattern.cc:1501`)

Computes per-Fun `MPosition` metadata:
- `f->arg_positions` — every canonical MPosition for any formal
  (positional + named variants).
- `f->positional_arg_positions` — just the positional ones.
- `f->named_to_positional` — name MPosition → canonical positional
  MPosition.
- `f->arg_syms[p]` — formal Sym at position `p`.
- `f->args[p]` — formal Var.
- `f->out_positions` — positions marked `Sym_OUT`.
- `f->rets` — `f->sym->ret->var` (one element).

The `up/down/next` chains on canonical MPositions are wired up here so
the matcher can navigate sibling/parent positions in O(1).

---

## 6. The main entry point: `pattern_match` (`pattern.cc:1376`)

```c
int pattern_match(Vec<AVar*> &args, Vec<cchar*> &names, AVar *send,
                  int is_closure, Partial_kind partial,
                  PNode *visibility_point, Vec<Match*> &matches);
```

Called by `fa.cc:function_dispatch`. Returns the number of matches
written into `matches`.

Step-by-step:

1. **Incomplete-call shortcut.** `incomplete_call(args, send)` returns
   true if any argument's `out` type is empty (`bottom_type`). When
   true, return 0 — try again later when more types have flowed in.
2. **Cache lookup.** `match_cache_hit(args, send, ...)` walks
   `send->match_cache` (a per-send Vec of `MatchCacheEntry`s). A hit
   means "for these arg types at this visibility point, we already
   computed the matches" — copy them out and return. Cache entries
   record the full `all_args` map (every position's expected AType)
   for byte-exact comparison.
3. **Construct `Matcher`.** Wraps the send, `args[0]` (the function-
   position arg), `is_closure`, `partial`.
4. **Visible functions.**
   `find_visible_functions(args, visibility_point, &partial_matches,
   function_values)`:
   - Asks the frontend via `IFAAST::visible_functions(arg0_sym)` for the
     list of visible Funs (NULL = all). pyc returns the single
     `arg0->fun` if any; V's frontend can scope by module.
   - `function_values` holds Funs passed as values (not selectors).
5. **`find_all_matches`** — the heart. Recursively walks the
   `Patterns` index to collect candidate Funs.
6. **Filter to `find_best_matches`** — eliminate subsumed candidates.
7. **`cannonicalize_matches`** — convert each surviving PMatch into a
   public `Match` and append to the output vector. Also handles
   partial-application matches.

`pattern_matches`, `pattern_match_complete`, `pattern_match_hits` are
process-wide counters logged in the per-pass summary in `fa.cc`.

---

## 7. `Matcher` (`pattern.cc:63`)

The state object that carries everything around during one
`pattern_match` call:

```c
class Matcher {
  AVar *send;                              // the call site result
  AVar *arg0;                              // the function-position arg
  IFAAST *ast;
  int is_closure;
  int is_error;
  Partial_kind partial;
  PMatchMap match_map;                     // Fun → in-progress PMatch
  Vec<Fun *> function_values;              // direct Fun values + varargs
  Map<MPosition *, int> mapped_positions;
  MapMPositionAType all_args;              // every position → AType seen
};
```

The matcher's job is to incrementally build `match_map[fun]` for every
candidate Fun.

### 7.1 `find_all_matches` (`pattern.cc:404`)

Recursive walker. For each argument position:
1. Look up dispatch candidates from the `Patterns` index for each
   `CreationSet` in the argument's AType.
2. Intersect with the running `partial_matches` set.
3. Recurse into pattern sub-positions for `is_pattern` formals.

When the recursion bottoms out, `update_match_map` (`pattern.cc:254`)
records the per-Fun filter contribution.

### 7.2 `find_arg_matches` (`pattern.cc:292`)

For one argument position, computes which Funs are still alive after
considering its possible CreationSets. Updates `partial_matches`
in place.

### 7.3 `pattern_match_sym` / `pattern_match_sym_internal`
(`pattern.cc:214`, `:186`)

Walks `Sym::dispatch_types` (the C3-linearised MRO from
`ast.cc:c3_linearization`) to find every Fun whose formal at this
position could accept this type. Uses `done` to avoid revisiting.

### 7.4 `subsumes` (`pattern.cc:520`)

Returns true if Match `x` strictly subsumes Match `y` at the given
position — i.e. `x` accepts every type `y` does. Used in
`find_best_matches` to eliminate dominated candidates.

`subsumes_arg` (`pattern.cc:440`) is the per-argument version. Together
they implement "most specific applicable method" in the Dylan/Common
Lisp sense.

### 7.5 `covers_formals` (`pattern.cc:802`)

Returns true if the actual arguments cover every required formal of
`f` (excluding ones with defaults). Used to weed out partial
applications when `partial != Partial_OK`.

### 7.6 `set_filters` (`pattern.cc:546`)

After candidates are pinned, computes the final
`formal_filters[p] = AType of accepted CreationSets`. This is what the
analysis stores in `Match::formal_filters` and uses to filter actual
→ formal flow at call edges.

### 7.7 `reverify_filters` (`pattern.cc:1050`)

Re-checks filters after generic/coercion substitution. Generic
instantiation can change the accepted set; this catches that.

### 7.8 `cannonicalize_matches` (`pattern.cc:1232`)

Last step: take the alive PMatches, turn each into a public Match,
handle partial-application bookkeeping (`is_partial` flag), and
append to the output `Vec<Match*>`.

### 7.9 `find_best_cs_match` (`pattern.cc:1081`) — the big one

The main per-CS recursion. Long (140 lines) and the source of most
behaviour subtlety. Walks all combinations of arguments' CreationSets
and prunes by `subsumes`. Calls `generic_substitutions`,
`coercion_uses`, `promotion_uses` to invoke the frontend's
generic/coerce/promote callbacks when needed.

### 7.10 `instantiation_wrappers_and_partial_application` (`pattern.cc:781`)

Walks the surviving matches and, for each one that needs a wrapper
(generic instantiation, default args, coercion/promotion), asks the
frontend to build it via `IFACallbacks::default_wrapper` /
`coercion_wrapper` / `promotion_wrapper` / `instantiate_generic` /
`order_wrapper`. The returned Fun is substituted into the Match.

`Matcher::build` (`pattern.cc:698`) is the per-PMatch wrapper builder.
Calls the right frontend callback in order: defaults → coercions →
promotions → generics → order.

---

## 8. Frontend callbacks

`pattern.cc` is the heaviest user of `IFACallbacks` (see
[ARCHITECTURE.md](ARCHITECTURE.md) §IFACallbacks). The dispatch logic
delegates to the frontend whenever a transformation needs
language-specific knowledge:

| Callback | Used by | Purpose |
|---|---|---|
| `formal_to_generic(s, &g, &bind)` | `get_generic_type`, `make_generic_substitution` | Is this formal generic? If so, what's the constraint and bind kind? |
| `instantiate(s, subs)` | `make_generic_substitution`, `instantiate_formal_types` | Instantiate a generic Sym with a substitution map. |
| `order_wrapper(f, subs)` | `Matcher::build` | Reorder args to canonical order. |
| `promote(f, a, b, c)` | `promotion_uses` | Implicit promotion (e.g. int→float). |
| `promotion_wrapper(f, subs)` | `Matcher::build` | Wrap a Fun to promote its args. |
| `coerce(actual, formal)` | `coercion_uses` | Explicit coercion (cast). |
| `coercion_wrapper(f, subs)` | `Matcher::build` | Wrap a Fun to coerce its args. |
| `default_wrapper(f, defaults)` | `Matcher::build` | Wrap a Fun to supply default args. |
| `instantiate_generic(f, subs)` | `Matcher::build` | Clone a generic Fun with type substitutions. |

The pyc frontend implements `default_wrapper` (in
`python_ifa_sym.cc:186`); the rest are inherited as no-ops (Python
doesn't need them at the IFA level). V's frontend implements more.

The caches on `Fun` (`promotion_cache`, `coercion_cache`,
`generic_cache`, `order_cache`, `default_cache`) memoize the wrapper
Funs so the same wrapper isn't built twice.

---

## 9. The match cache (`MatchCache`, `MatchCacheEntry`)

Pinned to `AVar::match_cache` (per-send-AVar). A `MatchCacheEntry` records:

```c
class MatchCacheEntry : public Vec<Match *> {
  int is_closure;
  Partial_kind partial;
  PNode *visibility_point;
  MapMPositionAType all_args;
};
```

The cache key is `(is_closure, partial, visibility_point) +
all_args[p] for each position p the matcher actually examined`. A
hit requires exact AType pointer equality for every position. Because
ATypes are hash-consed (see [IFA.md](IFA.md) §4), pointer equality is
semantic equality.

Hits are tracked in `pattern_match_hits` and reported in the per-pass
log line:

```
COMPLETE: ... match X (Y%) cached (Z%) ...
```

The cache is invalidated implicitly: `add_patterns` clears the
`sym_match_cache`, which doesn't directly clear per-AVar caches but
does mean new candidates won't appear in old entries because the
walkers consult `Patterns` fresh each time. Stale entries (in
principle) could exist if a type's `dispatch_types` change after the
fact; in practice this is fine because cloning is the only thing that
changes those, and the cache is rebuilt across passes.

---

## 10. Visibility and `visibility_points`

When a frontend wants to scope which Funs are callable from a given
call site (e.g., V's module-private functions), it overrides
`IFAAST::visible_functions(arg0_sym)`. Returning NULL means "all
candidates are visible." Returning a Vec restricts.

`PMatch::visibility_points` accumulates the PNodes contributing to a
Match; this matters when the frontend needs to know *where* a Match was
constructed (for diagnostics or for the test cache).

pyc returns `arg0->fun` (single-element) when the receiver is itself a
function Sym. Otherwise NULL.

---

## 11. Detailed example walkthrough

Suppose:

```python
class A: 
    def m(self, x): pass
class B(A):
    def m(self, x: int): pass

a = A() if cond else B()
a.m(42)
```

After build:
- `A::m` has `arg_positions = [(1,), (1,1), (1,2)]` (function position,
  self position, x position).
- `B::m` similarly, but its position `(1,2)` formal `x` has
  `must_specialize = sym_int`.
- `sym_A->match_type->funs[(1,1)] = [A::m]`,
  `sym_B->match_type->funs[(1,1)] = [B::m]`.
- `sym_A->dispatch_types = [A, ...]`,
  `sym_B->dispatch_types = [B, A, ...]` (C3 MRO).

At the call site `a.m(42)`:
- After name lookup, the SEND has rvals `[selector, a, 42]`.
- `pattern_match` is called with `args = [m_sym, a_avar, 42_avar]`,
  receiver `arg0 = m_sym`.
- `dispatch_type(a_avar)` is `A | B` (sum of CreationSets).
- `find_all_matches` walks both A and B's MRO. For A's CSes it finds
  `A::m`; for B's CSes it finds `B::m` (and also `A::m` via MRO).
- For the `x` position with `42` (an `int_constant`), `A::m` accepts
  (formal has no constraint); `B::m` accepts (formal accepts int).
- Both Matches survive `find_best_matches`.
- The result is two Matches, each with `formal_filters[1] = {A}` or
  `{B}` for the receiver.
- The analysis (`fa.cc:make_AEdges`) builds two AEdges, one to each
  EntrySet. Type splitting (see [IFA.md](IFA.md) §6) eventually puts
  each call branch into its own EntrySet.

---

## 12. The `dispatch_type` rules (`pattern.cc:130`)

Worth quoting verbatim because the lookup hinges on them:

```
if (is_Sym_OUT(a))                    return sym_unknown_type;
if (a->is_symbol)                     return a;
if (a->type && a->type->is_symbol)    return a->type;
if (a->must_specialize &&
    a->must_specialize->type_kind != Type_VARIABLE)
                                      return a->must_specialize;
return sym_any;
```

So:
- Output parameters dispatch on `unknown_type` (i.e. they don't
  dispatch — they receive).
- Symbol-typed formals dispatch on the symbol itself (this is how
  the selector / method-name dispatch works).
- `must_specialize` overrides everything except symbols.
- Default = `sym_any` (matches everything).

This is the **single function** to read when "wrong overload picked"
is the symptom. The hierarchy walker (`pattern_match_sym_internal`)
walks `Sym::dispatch_types` (C3 MRO) starting from this type.

---

## 13. Gotchas

### 13.1 The variable-naming convention
The 1-3 letter prefixes (`acpp`, `fcp`, `acnp`, etc.) are not noise —
they encode the variable's origin. Don't rename without preserving the
encoding (or commit to renaming everywhere). See the top-of-file
comment.

### 13.2 MPosition canonicalization is mandatory
Storing a non-canonical MPosition in a map will fail to retrieve.
Always call `cannonicalize_mposition(&p)` (or use the cached `p.cp`)
before storing. The maps key by pointer.

### 13.3 `int2Position(i)` sentinel encoding
`MPosition::pos` mixes integers and pointers in the same vector via
the `(void*)(intptr_t)-i` cast. The negation makes "encoded integer"
distinguishable from "real pointer" (real pointers are positive). The
limit `MAX_ARGS = 1000` in pattern.h is the largest i that can be
encoded; arguments past 1000 break the test (no one writes such
functions, but be aware).

### 13.4 `sym_match_cache` invalidation
`build_patterns(fa, f, add)`'s `add` flag clears the global
`sym_match_cache` when adding a Fun incrementally. If you forget to
pass `add = 1` (e.g., add a `build_patterns(fa, f, 0)` call after
analysis started), the cache will return stale matches.

### 13.5 `incomplete_call` shortcut hides real errors
`pattern_match` returns 0 immediately if any arg's `out` is empty. This
is correct (the matcher would have nothing to do), but if you're
debugging "why didn't this call resolve?", make sure the args have
non-bottom types. The DISPATCH log (`-l dispatch`) prints
`pattern_match_complete` vs `pattern_matches` — the gap is the
incomplete-call count.

### 13.6 Per-`AVar` `match_cache` lifetime
The cache is on the *send* AVar. When the analysis re-runs after a
split, the AVar may be replaced (different contour). The fresh AVar
won't have the old cache — that's fine, but means worst-case the same
match work is redone after every split.

### 13.7 `Match::merge` is destructive
It mutates the existing Match's `formal_filters`. The caller in
`fa.cc:make_AEdges` does this when several edges to the same Fun need
to share a Match. Don't call `merge` on Matches you've already
returned to a caller that's keeping a reference — they'll see the
mutation.

### 13.8 PMatches are throwaway
A `PMatch` lives only during one `pattern_match` invocation. The
output is *always* `Match*` (via `cache_copy`). Don't try to keep
PMatches alive across calls.

### 13.9 Visibility-point semantics differ across frontends
pyc's `visible_functions` returns the receiver's `fun` field only
(single candidate) or NULL (all). V's returns module-scope-restricted
lists. The matcher works either way, but log output and test
expectations differ.

### 13.10 Wrappers create new Funs and re-install them
`Matcher::build` may produce a fresh `Fun *` via a callback. The new
Fun is added to PDB via `install_new_fun` (in pyc's
`python_ifa_main.cc:124`) which:
- runs `if1_finalize_closure` for IF1-side init.
- calls `new Fun(f)` to build its CFG/SSU.
- calls `finalize_types` and `fixup_aspect`.
- calls `build_arg_positions(fun)` so the new Fun has its own
  positions.
- adds to `pdb`.

If a wrapper appears half-initialised (missing arg_positions,
crashing in subsumes), the wrapper-build path skipped one of these
steps.

### 13.11 The MType reverse index is global to a Sym
`Sym::match_type->funs` is keyed by MPosition; the same MPosition can
appear for many Funs. A change to one Fun's signature can ripple — call
`add_patterns(fa, f)` to refresh, which clears the cache.

### 13.12 `arg0` is the function position
`Matcher::arg0` is `args[0]`, which for a normal call is the function
or selector — *not* the receiver. The receiver (self/this) is at
`args[1]`. This matters when debugging dispatch — read positions as
1-indexed from "the function itself."

---

## 14. Symptom → start-here

| Symptom | Start here |
|---|---|
| "no match found" / `matches.n == 0` | `incomplete_call` (args not flowed yet?) → `find_visible_functions` → `Sym::match_type` populated? |
| "wrong overload picked" | `dispatch_type(a)` for the relevant arg, then `subsumes_arg` |
| "ambiguous dispatch" | `find_best_matches` and `subsumes` — verify the candidates' `must_specialize` and the type-hierarchy `dispatch_types` |
| "default arg not applied" | `Matcher::build` `default_args` branch + frontend `default_wrapper` |
| "generic not instantiated" | `unify_generic_type`, `generic_substitutions`, frontend `instantiate_generic` |
| "coercion not applied" | `coercion_uses`, frontend `coerce` and `coercion_wrapper` |
| "promotion not applied" | `promotion_uses`, frontend `promote` and `promotion_wrapper` |
| "stale match after adding Fun" | `add_patterns(fa, f)` not called? `sym_match_cache` invalidation |
| "method lookup wrong on subclass" | `Sym::dispatch_types` (C3 from `ast.cc:c3_linearization`) |
| "named arg in wrong slot" | `Fun::named_to_positional` lookup; check `build_arg_positions` |
| "wrong filter narrowed an EntrySet" | `Match::formal_filters[p]` in the matcher (computed by `set_filters`) |
| "matcher loops" | typically a missing `done.set_add(type)` in `pattern_match_sym_internal` |

---

## 15. References

- `ifa/if1/pattern.cc` — implementation.
- `ifa/if1/pattern.h` — `MPosition`, `MType`, `Patterns`, `Match`.
- `ifa/if1/ast.cc:c3_linearization` — the MRO computation that fills
  `Sym::dispatch_types` (used by the matcher's hierarchy walker).
- [IFA.md](IFA.md) §5.5 — the call site of `pattern_match` and
  `make_AEdges`.
- [CLONE.md](CLONE.md) §7.3 — how `equivalent_es_pnode` consumes the
  Matches' target equivalence classes.
- [IR.md](IR.md) §3 — `Sym` flags `is_pattern` / `is_symbol` /
  `must_specialize` that drive dispatch typing.
- [ARCHITECTURE.md](ARCHITECTURE.md) §IFACallbacks — the callback
  contract the matcher leans on.
