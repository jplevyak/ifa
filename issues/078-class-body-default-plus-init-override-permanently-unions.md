# 078 — A class-body default attribute permanently unions into a field's inferred type even when `__init__` unconditionally overwrites it first

**Status:** open, filed 2026-08-05. **Option A attempted and reverted
2026-08-05** — see "Attempt 1" below: the naive form of the rule
(elide a class-body default whenever `__init__`'s entire body is
self-independent field-literal assignments) is unsound. It missed a
whole *second* way a prototype's field is observed besides
"`__init__` runs on every real instance before user code can see it":
`gen_class_pyda`'s own inherited-field copy loop reads a **base
class's prototype fields directly**, bypassing `__init__` entirely,
to seed every subclass's prototype. Reverted before commit; no fix
currently landed. Not a hypothetical: this is the general form of the
general form of the bug fixed in
[076](closed/076-mutation-driven-receiver-divergence-not-cloned.md)
for `dict`/`set` specifically (see that issue's "RESOLVED" section for
the full original trace). Filed separately because 076's fix was a
per-class workaround (delete the two redundant lines in
`__pyc__/07_dict.py`/`08_set.py`), not a fix to the mechanism itself —
the same pattern will reproduce in any other class, `__pyc__`-builtin
or user-written, with the same shape. No fix attempted here; this
issue exists to name the general pattern, give a minimal standalone
repro independent of `dict`/`set`, and lay out fix directions for
whoever picks it up.
**Affects:** `python_ifa_build_syms.cc`'s `gen_class_pyda` (~1803-2065,
the prototype + `___init___` + `__new__`/`sym_clone` construction
described below) and, more fundamentally, `ifa/analysis/fa.cc`'s
field-write modeling — every field write (`P_prim_setter`) is treated
as a `flow_vars` contribution that unions into the field's AVar,
with no notion of "this write is provably dead, superseded by a later
one on every path." Not a single fixable line; an architectural
property of how fields are modeled.
**Related:** [076](closed/076-mutation-driven-receiver-divergence-not-cloned.md)
(the concrete `dict`/`set` instance and fix — read that issue's
"RESOLVED" section first, this issue is its generalization),
[017](../../issues/closed/017-multi-instance-mutation-corruption.md)
(project-level; the *runtime* half of the same class-body-default
footgun, already fixed — this issue is entirely about the *residual
static-analysis* consequence 017's fix didn't and couldn't address).

## Symptom (minimal, standalone repro — no `dict`/`set` involved)

```python
class MiniDict:
    keys = []              # class-body default -- ALSO a "setter" of `keys`, from FA's view
    def __init__(self):
        self.keys = []      # always overwrites the class-body default before any instance is observed
    def put(self, key):
        i = 0
        while i < len(self.keys):
            if self.keys[i] == key:
                return
            i += 1
        self.keys.append(key)

a = MiniDict()
a.put(1)
b = MiniDict()
b.put("hello")
print(a.keys)
print(b.keys)
```

This is ordinary, portable Python — verified against real `python3`,
which prints `[1]` / `['hello']`. (An earlier draft of this repro used
`self.keys = self.keys.append(key)` instead of the plain statement
above, mirroring `dict.__setitem__`'s own internal idiom — but that
relies on pyc's `list.append()` deliberately returning `self` rather
than `None` (issue 017's documented workaround for the *runtime* half
of this footgun); real Python's `list.append()` returns `None`, so
that form would silently set `b.keys` to `None` on real Python. Caught
during review and replaced with the plain, CPython-verified statement
above, which reproduces the identical failure without relying on any
pyc-specific behavior.)

Fails:

```
warning: expression has mixed basic types:( int64 str )
  if self.keys[i] == key:
...
minidict_buggy.py.c:102:8: error: comparison between pointer and integer ('_CG_int64' (aka 'long long') and 'char *')
  t1 = _CG_prim_equal(t2, _CG_Symbol(6483, "=="), _CG_String_n("hello",5));
minidict_buggy.py.c:286:8: error: no matching function for call to '_CG_str_eq'
  t1 = _CG_str_eq(t2, 1);
```

Delete the one class-body line (`keys = []`), keeping only `__init__`'s
`self.keys = []`, and the identical program compiles clean and its
output matches `python3` byte-for-byte (`[1]` / `['hello']`). **The two
versions are runtime-identical** — `__init__`
always overwrites `keys` before `a` or `b` is observable by any other
code, on every execution path, unconditionally. Confirmed by testing
two *other*, simpler shapes first that did *not* isolate this
specifically (a bare scalar field with direct external assignment,
and a method-based mutator with no internal comparison) — both failed
identically whether or not the class-body default was present, because
they hit a *different*, more basic gap (the two instances' receivers
never get CS-split from each other at all in those shapes). This
repro needed the internal `==` comparison specifically, because that's
what triggers the setter-confluence splitting that separates `a` and
`b` into distinct instances in the first place — a prerequisite for
the class-body-default leak to be the *only* remaining variable. See
[076](closed/076-mutation-driven-receiver-divergence-not-cloned.md)'s
"RESOLVED" section for why: the receiver-splitting mechanism triggers
reactively, off a detected violation, and a program with no internal
comparison never produces one for FA to react to.

## Root cause (established in 076, restated generally here)

Every class in pyc gets one compile-time singleton **prototype**
(`cls->self`, `gen_class_pyda:1960`). At program start the generated
code allocates it once (`sym_new`) and runs the class body's implicit
initializer (`___init___`, triple-underscore, synthesized from
class-body-level assignment statements) on it exactly once — for
`MiniDict` above, that means running `keys = []` once, ever, against
the prototype. Every actual `MiniDict()` call site does *not* repeat
that work: the synthesized `__new__` wrapper (`gen_class_pyda:2054`)
clones the prototype —

```cpp
if1_send(if1, &body, 3, 1, sym_primitive, sym_clone, proto, t);
...
if1_send(if1, &body, 2, 1, sym___init__, t, new_sym(ast));
```

— a shallow, memberwise struct copy, into a fresh instance `t`, then
calls the user's `__init__` on `t`.

pyc's flow analysis doesn't model "the clone happens, then gets
overwritten by `__init__`" as sequential replacement. It computes a
field's type as the **union of every setter that can reach it**, with
no notion of temporal ordering or dominance. The prototype-clone step
*is* a setter of the new instance's `keys` field (copying whatever
type the prototype's own `keys` field has — one specific, class-level-
shared CreationSet). `__init__`'s own assignment is a *second* setter
of the same field. FA unions them regardless of the fact that one
unconditionally overwrites the other at runtime. Because the prototype
is one singleton shared by *every* instance of the class, that one
CreationSet feeds into every instance's field type on every pass,
independent of how cleanly the per-instance data is otherwise
separated downstream.

## Scope

Any class with **both** a class-body-level attribute assignment
(`attr = <expr>`) **and** an `__init__` (or other always-runs-first
method) that unconditionally reassigns the same attribute is affected,
whenever that field later needs genuinely divergent per-instance
typing to type-check precisely. This is a common, unremarkable Python
idiom — declaring a class-level default for documentation/IDE purposes
and then setting the real per-instance value in `__init__` — not an
edge case. `__pyc__`'s own library had exactly this shape in `dict`
and `set` (issue 017's fix pattern, applied to both) until 076 removed
it; other `__pyc__` classes with a no-arg `__init__` overwriting bare
class-body defaults were not surveyed for the same shape (`AGENTS.md`/
issue 017's own writeup calls out `__list_iter__`, `__dict_iter__`,
and siblings in the same files as following a similar pattern — worth
a quick check, not done as part of filing this issue). User-written
classes are affected identically; nothing about this is `__pyc__`-
specific.

## Proposed fix directions (no recommendation made)

**Option A — teach `gen_class_pyda` to skip the redundant class-body
write during `___init___` synthesis when the same field is
unconditionally reassigned in `__init__`.** Effectively automates
076's by-hand fix for every class, at compile time, rather than
requiring library/user authors to notice and avoid the pattern
manually. Needs a "definitely reassigned before any use" analysis at
the point `___init___`/`__new__`/`__init__` are synthesized — probably
tractable given `__init__`'s body is available at that point, but the
analysis itself (dominance of the reassignment over every path where
the field could be observed) hasn't been designed. Closest in spirit
to how `structural_assignment`/`merge_in`'s cross product was reasoned
about and partially fixed in 076's investigation, but this would live
in the class-synthesis frontend rather than a runtime FA transfer
function.

**Option B — teach FA's field-setter modeling to recognize a
provably-dead write and exclude it from the union**, i.e. a general
"last write on every path wins" refinement for `P_prim_setter`
targets, not scoped to class-body/prototype specifically. Strictly
harder and more invasive than Option A (touches the core field-flow
model used everywhere, not just the class-construction synthesis
path) but would also catch the same *shape* of bug for any two setters
of a field where one always precedes and is superseded by the other,
not just the prototype-clone case.

**Option C — accept it as a documented style hazard.** Cheapest:
note in `__pyc__`'s own contribution guidance (or a project-level doc)
that a class-body default coexisting with an `__init__` override of
the same attribute is a latent precision hazard, and that `__pyc__`
library code should prefer one or the other. Doesn't fix user code
hitting the same pattern, and doesn't fix it automatically for future
`__pyc__` additions, but requires no FA/frontend changes and no design
work.

## Attempt 1 (Option A, naive form) — tried and reverted 2026-08-05

Implemented the rule exactly as scoped above: a pre-pass over a
class's own `__init__` (`compute_init_elidable_fields`, new static
helper) walked its top-level statements and, when EVERY one was a
`self.NAME = <expr>` assignment whose `<expr>` doesn't read `self`
(`simple_self_field_overwrite` / `pyast_references_self`), collected
`NAME` into an elidable-fields set. `gen_class_pyda`'s class-body
statement loop (the one at ~1836-1843 that builds `body`, the
`___init___` prototype-initializer) then skipped emitting any
class-body-level `NAME = ...` statement whose target name was in that
set — automating exactly what 076's by-hand fix did for `dict`/`set`,
for every class.

It compiled cleanly and the `MiniDict` repro above worked *without*
hand-deleting the class-body line — `keys = []` was auto-elided,
compiled clean, and printed `[1]` / `['hello']`, matching `python3`.
So far so good.

**Then a baseline check caught a regression on the most trivial
possible program.** `print("hello")` alone — before this change,
compiling it produces zero `expression has no type` warnings from
`__pyc__.py`. After this change, the identical `hello.py` produces
**19** such warnings, all inside `__pyc__/08_exception.py`. Traced to:

```python
class BaseException(object):
  args = ""
  def __init__(self, args=""):
    self.args = args
  ...

class SystemExit(BaseException):
  pass
```

`BaseException.__init__`'s body is exactly one statement,
`self.args = args` — `args` here is the *parameter*, not `self`, so
`pyast_references_self` correctly says the RHS doesn't read `self`,
and the rule (as designed) marks `args` elidable and drops the
class-body `args = ""` default. This is where the design's stated
safety condition — "always overwritten before any instance is
observable" — turns out to be **necessary but not sufficient**. It
was reasoned about purely in terms of *runtime instantiation*
(`__new__` clones the prototype, then calls `__init__` on the clone,
so no real instance is ever observable with the class-body default
still in place). But `gen_class_pyda`'s inherited-field copy loop
(~1819-1834, "Build base ___init___") is a **second, independent
reader of a field's value on the prototype**, and it runs at
prototype-construction time, never through `__init__` at all:

```cpp
for (int i = 0; i < cls->includes.n; i++) {
  Sym *inc = cls->includes[i];
  for (int j = 0; j < inc->has.n; j++) {
    ...
    if1_send(if1, &body, 4, 1, sym_operator, inc->self, sym_period, iv, t)->ast = ast;
    if1_send(if1, &body, 5, 1, sym_operator, fn->self, sym_setter, iv, t, ...)->ast = ast;
    ...
```

For every base class `inc` a class includes, this reads each
inherited field directly off `inc->self` — **the base class's own
prototype**, not any instance, and not anything `__init__` ever
touches — and copies it onto the subclass's own prototype (`fn->self`,
soon to become `cls->self`). `SystemExit(BaseException)` (`pass`-only,
no own `__init__`) hits exactly this path for `args`: its prototype's
`args` field is seeded by reading `BaseException`'s prototype's `args`
field. Once `BaseException`'s class-body `args = ""` is elided,
`BaseException`'s own prototype's `args` field has **no setter at
all** — `__init__` is never invoked on a prototype, only on real
clones — so `inc->self`'s `args` read here observes an unset field,
and that propagates into `SystemExit`'s (and every other subclass's)
prototype as `NOTYPE`, surfacing as "expression has no type" the
moment anything downstream reads it.

`dict`/`set` (076's actual fix) didn't hit this because neither has
subclasses in `__pyc__` — nothing ever reads their prototypes' fields
except via the ordinary `__new__`-clone-then-`__init__` path 076's fix
already accounted for. `BaseException` does, and that's the
distinguishing factor the naive rule missed entirely: the rule needs
to also prove **no other class's prototype construction reads this
field off `cls`'s own prototype** before allowing elision — i.e. `cls`
has no subclasses (or, more precisely, no subclass whose OWN
`__init__` fails to unconditionally re-overwrite the same field before
it's observable there too, recursively).

**Why this is harder to fix than it looks:** determining "does `cls`
have a subclass" cheaply and correctly, at the point `gen_class_pyda`
runs for `cls`, isn't available from local state. `cls->includes` is
the reverse relation (a class's OWN bases, populated by pass 1 before
pass 2/`gen_class_pyda` runs for ANY class — see the comment at
gen_class_pyda's `collect_match_args` call) — there's no
`included_by`/subclass list on `Sym` (checked `ifa/if1/sym.h`) to walk
the other direction. Because `gen_class_pyda` runs classes in program
order (a base class is always fully processed before any subclass
that names it, since Python requires the base to already be bound),
a subclass's own gen_class_pyda call happens strictly *after* its
base's — so even a locally-built "which classes have I seen included"
registry finds out about a subclass too late to inform the base
class's own elision decision, which has already been made and
committed by then. A correct version needs either (a) a real
whole-program subclass registry built and available *before*
`gen_class_pyda` processes the first class (nontrivial to source
correctly — no existing global class list was found; `PycModule` has
no materialized symbol table, and `ctx.saved_scopes` is a plausible
source but wasn't verified safe, in particular under the REPL
`BaselineIF1State` baseline/extend split, which builds a baseline IF1
state *once* and reuses it via fork across REPL iterations — a
naive `static` cache computed on first use would go stale the moment
a later REPL iteration introduces a new subclass), or (b) recursing
the "unconditionally overwritten before observable" check through
every transitive subclass's own `__init__`, which is substantially
more analysis than the original conservative scope intended.

**Disposition:** reverted before commit (working tree restored via
`git checkout -- python_ifa_build_syms.cc`); nothing landed. Verified
back at baseline: `hello.py` returns to 0 `expression has no type`
warnings post-revert. Recorded here so the next attempt doesn't
rediscover this the same way — Option A's design needs to fold in
"observed via a subclass's prototype-inheritance copy" as a second,
independent notion of "observable," not just "observed via a real
constructed instance," before the elision condition above is sound.

## Verification plan

1. The `MiniDict` repro above (and its `dict`/`set` analog, already
   fixed by 076) — should compile clean with either Option A or B; C
   wouldn't fix this repro at all, only prevent *new* instances of the
   pattern from landing in `__pyc__` un-noticed.
2. Survey `__pyc__/*.py` for other classes with a bare class-body
   default immediately overwritten by a no-arg `__init__` (not done
   for this filing) — confirm whether any are load-bearing for corpus
   precision the way `dict`/`set` were for `dijkstra2`.
3. Whichever of A/B is attempted: full `ifa --test`, `test_pyc.py`
   both backends both `PYC_CSM` settings, and a full corpus sweep both
   settings — this touches either class-construction synthesis (used
   by every class in every program) or core field-flow modeling (used
   by every field write in every program), the same safety bar every
   `fa.cc`-level change in 076's investigation was held to.

## What this unblocks

076's fix was necessarily narrow (two `__pyc__` files, hand-identified
by tracing one specific corpus failure). A real fix here would close
the entire bug *class* at once — for every current and future
`__pyc__` builtin, and for user-written classes with the same
ordinary, unremarkable shape, without requiring anyone to notice the
hazard and work around it by hand the way 076's investigation had to.
