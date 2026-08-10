# 091 — a non-record builtin type (`int`, `float`, `list`, `tuple`, `bool`) stored as a plain value has no real `__new__` to call indirectly

**Status:** open, found 2026-08-08 while verifying
[ifa/issues/089](closed/089-DISPATCH-closure-pyc-to-bool-no-candidate.md)'s
`__pyc_to_bool__` fix against `defaultdict(int)` (that issue's own
stated "second gap" — flagged there, root-caused and filed here).

**Affects:** `python_ifa_build_if1.cc`'s zero-arg builtin-constructor
special case (~line 837, `if (f && pos_args.n == 0) { if (f->name &&
!strcmp(f->name, "int")) {...} }`, and the equivalent blocks for
`float`/`list`/`tuple`/`bool` nearby) — a purely syntactic, direct-
call-site pattern match, confirmed by that code's own comment: "int/
float are Type_ALIAS... bool/list/tuple are ifa-core builtin primitive
types... so none of them ever get a `__new__` candidate to dispatch a
zero-arg call to."

## Repro

```python
factory = int
print(factory())
```
- CPython: `0`.
- pyc: compiles with warnings (`illegal call argument type expression
  illegal`, `expression has no type` — even the *assignment*
  `factory = int` doesn't resolve, not just the call), then aborts at
  runtime: `assert(!"runtime error: matching function not found")`.

Same for `float`, `list`, `tuple`, `bool` stored as a bare value and
called indirectly. **Not** the same for `dict`/`set`:

```python
factory = dict
print(factory())   # {}, correct, both compiles and runs clean

factory2 = set
print(factory2())  # set(), matches CPython except a separate,
                    # pre-existing, unrelated repr mismatch (pyc
                    # prints "{}" for an empty set, not chased here)
```

Also **not** the same for a user-defined class:
```python
class Thing:
    pass

factory = Thing
print(factory())   # constructs a Thing correctly (prints "<object>",
                    # matching the separate, expected __str__ default
                    # for a class with no override -- not this bug)
```

## Root cause

`python_ifa_build_if1.cc`'s builtin-constructor handling for `int`/
`float`/`list`/`tuple`/`bool` is a **direct-call-site special case**:
it matches when the callee symbol `f` is *literally* the name being
looked up at *this* send (`f->name && !strcmp(f->name, "int")`), and
on a match, synthesizes the result inline (a literal `0` for `int()`,
an empty-`make` primitive for `list()`, etc.) — there is no real `Fun`
or closure value backing "the `int` constructor" that this synthesis
routes through. `dict`/`set` (real `Type_RECORD` classes with an
explicit `__init__`, per closed
[issues/017](../../issues/closed/017-multi-instance-mutation-corruption.md))
and any user-defined class (whose `__new__` wrapper is a genuine,
ordinary `Fun`) don't have this gap — their constructors are real,
storable, callable values from the start.

So `factory = int` has nothing real to alias — the assignment itself
has no type — and `factory()` falls through to the generic closure-
call dispatch mechanism with no candidate to find, hence "matching
function not found" rather than a clean compile-time reject (the
already-fixed [ifa/085](closed/085-CGEN-dead-if-unresolved-condition-no-guard.md)
pattern doesn't apply here — this isn't an unresolved `Code_IF`
condition, it's a genuinely unresolved *call target*).

## Why not fixed here

A real fix needs `int`/`float`/`list`/`tuple`/`bool` to have a genuine
`__new__`-shaped `Fun`, the same way `dict`/`set` and every user class
already do — not another special case, but closing the actual gap
between "real `Type_RECORD` class with `__init__`" and "ifa-core
primitive/alias type with a syntax-level constructor shortcut." That
likely touches how these five types get registered in
`python_ifa_build_syms.cc` (giving each a synthesized `__new__`/
`__init__` pair mirroring `dict`/`set`'s, so the existing zero-arg/
one-arg direct-call-site special cases in `build_if1.cc` become an
*optimization* over a real fallback rather than the only path) — a
broader, more foundational change than this session's other fixes,
and risks disturbing the direct-call-site fast path every `int(x)`/
`list()`/etc. in the corpus already relies on. Not attempted here to
keep [ifa/089](closed/089-DISPATCH-closure-pyc-to-bool-no-candidate.md)'s fix
scoped to its own mechanism.

## Verification plan

- The repro above (`factory = int; factory()`) must print `0`,
  matching CPython, and the analogous cases for `float`/`list`/
  `tuple`/`bool`.
- Must not regress the existing direct-call-site fast paths
  (`int()`, `int(x)`, `int(x, base)`, `list()`, `list(x)`, etc. —
  `python_ifa_build_if1.cc`'s existing special cases) or `dict`/`set`/
  user-class indirect-construction (already working, confirmed above).
- `pyc_lib/collections.py`'s `defaultdict.__getitem__`
  (`self.d[key] = self.factory()`) is the concrete, real-world blocker
  — `defaultdict(int)`/`defaultdict(list)` should work end-to-end once
  this and [ifa/089](closed/089-DISPATCH-closure-pyc-to-bool-no-candidate.md)
  are both fixed. Extend `tests/defaultdict_keys_values.py` (or add a
  new test) for the `b[k] += 1` auto-vivify shape.
- Full `test_pyc.py`, both backends.

## Attempted fix (2026-08-10), reverted — load-site substitution is unsafe

Tried the narrowest-looking fix that avoids touching
`python_ifa_build_syms.cc`'s type registration at all: give each of
int/float/bool/list/tuple an ordinary `__pyc_<type>_new__` wrapper
function in `__pyc__/05_builtins.py` (ordinary 0-arg `def`s returning
the type's zero value — a real, storable, callable `Fun` from the
start, exactly like a bare user `def` reference already is), then in
`build_if1_pyda`'s `PY_name` case (`python_ifa_build_if1.cc`, the
final `else if (load && ast->sym)` branch before the closing
`return 0;`) substitute `ast->rval` with the matching wrapper Sym
whenever one of these five names is loaded as a plain value.

**This broke on two levels, both instructive:**

1. **First version was unconditionally slow *and* wrong**: it called
   `make_PycSymbol(ctx, name, PYC_USE)` for all 5 candidate names on
   *every* bare-name load in the entire program (gating the
   substitution only *after* the lookup, not before it) — `make_PycSymbol`
   is not read-only, so this corrupted scope resolution broadly
   (`test_pyc.py` went from 263/12/0/4 to ~35 new FAILs spanning
   totally unrelated tests: `dict_str`, `match_map`, `scope_*`, etc.).
   Fixed by gating on a cheap `strcmp(ast->sym->name, ...)` *before*
   calling `make_PycSymbol` at all.

2. **Even after that fix, ~30 unrelated tests still failed** (`dict_str`,
   `dict_eq_ne`, `match_map`, `match_map_star`, `str_index`,
   `tuple_compare`, `random_module`, etc. — none of these test files
   reference `int`/`float`/`bool`/`list`/`tuple` as a bare name
   directly). Root cause: `__pyc__/02_numeric.py:19` (`isinstance(x,
   list)`) and `__pyc__/04_sequence.py:100` (`isinstance(l, tuple)`)
   — both inside the *shared, bundled* builtin module compiled into
   *every* program — also load `list`/`tuple` as a bare name to pass
   as `isinstance`'s second (`cls`) argument
   (`build_builtin_call_pyda`'s `isinstance` special case at line 678
   uses `getAST(pos_args[1], ctx)->rval` directly as the primitive's
   `cls` operand). The substitution silently replaced the real class
   Sym with the `__pyc_list_new__`/`__pyc_tuple_new__` wrapper *Fun*
   there too, so `isinstance(x, list)` inside the builtin module's own
   dunder methods started checking against the wrong "type" — and
   because that module is bundled into every compile, the corruption
   was systemic, not confined to programs that use these names
   directly.

**Why this rules out load-site substitution as a strategy, not just
this specific bug**: a bare `int`/`float`/`bool`/`list`/`tuple`
reference is loaded as a *type descriptor* (isinstance checks, and
plausibly other class-Sym-as-value uses not yet audited) far more
often than as a *factory to store and call later*, and by the time a
name is loaded there is no way to know which use it's headed for —
that information doesn't exist yet at load time in this pass. Any fix
that changes what loading one of these names *means* will hit the
same conflict. Reverted cleanly (`git checkout --
__pyc__/05_builtins.py python_ifa_build_if1.cc`); confirmed back to
clean 263/12/0/4 baseline.

**What this narrows the real fix to**: the "why not fixed here"
section's original instinct was right — the fix has to make
`int`/`float`/`bool`/`list`/`tuple`'s *existing* class Sym itself
callable (a real `__new__`/`__init__` `Fun` attached to its
`meta_type`, `must_implement_and_specialize`-style, exactly how
`dict`/`set` already get theirs in `python_ifa_build_syms.cc`), so
the Sym loaded by `isinstance(x, list)` and the Sym loaded by
`factory = list` stay the *same* Sym and both uses keep working
through their existing, independent dispatch paths. A parallel
wrapper Sym that has to be swapped in at load time can't satisfy both
uses at once.

## What this unblocks

`collections.defaultdict(int)` / `defaultdict(list)` / `defaultdict(set)`
end-to-end (currently blocked here even with
[ifa/089](closed/089-DISPATCH-closure-pyc-to-bool-no-candidate.md) fixed —
confirmed: `pyc_lib/collections.py:34`'s `self.factory()` is exactly
this shape). More generally, any program passing `int`/`float`/`list`/
`tuple`/`bool` around as a plain callable value (a "type as a
first-class factory" idiom — the same shape `defaultdict`'s own
implementation uses, and a reasonably common pattern beyond it).
