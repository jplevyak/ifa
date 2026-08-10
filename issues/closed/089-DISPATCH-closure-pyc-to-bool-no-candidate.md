# 089 — a first-class function/closure value has no `__pyc_to_bool__` dispatch candidate: `if some_function:` fails to type

**Status:** fixed 2026-08-08. Found 2026-08-08 while diagnosing
[issues/025](../../../issues/025-shedskin-examples-coverage.md)'s TODO
list item 3 (mastermind2's blocker). The doc's own item 3 text
("int/float mixed `-=`/`*` gap... at its own line 77, not investigated
further") is **stale** — re-verified today, `mastermind2.py` no
longer reaches line 77 at all; it now fails much earlier, at
`pyc_lib/collections.py:33`'s `defaultdict.__getitem__` (`if
self.factory:`), which is what this issue tracks.

**Affects:** every Python frontend construct that puts a first-class
function/callable value in a boolean context — `if fn:`, `bool(fn)`,
`while fn:`, `not fn`, an `and`/`or` short-circuit operand, etc. — for
*any* closure, not just builtins. Root mechanism is FA/dispatch-level
(`__pyc__/00_runtime.py`'s `object.__pyc_to_bool__` / the universal
`call_method(..., sym___pyc_to_bool__, ...)` used by every boolean
coercion site in `python_ifa_build_if1.cc`), so this is filed here
rather than in `issues/`.

## Repro

```python
def make_zero():
    return 0

factory = make_zero
if factory:
    print("yes")
else:
    print("no")
```
- CPython: `yes` (any function object is truthy by default).
- pyc: fails to compile —
  ```
  repro.py:5:39: error: illegal call argument type expression illegal:
      if factory:
                 ^
  repro.py:5:39: error: expression has no type
  ```
  (In the `defaultdict(int)`/`b[5] += 1` shape that actually blocks
  mastermind2, the same error names the offending type explicitly:
  `illegal call argument type expression illegal: closure`.)

This is **not** specific to builtin types passed as callables
(`defaultdict(int)`) — a plain user `def` function in a variable
fails identically (see repro above, and `factory = int` fails the
same way too). It is specific to the *boolean-context* dispatch:
```python
def make_zero():
    return 0

class C:
    def __init__(self, factory=None):
        self.factory = factory
    def check(self):
        return self.factory()   # <-- calling it works fine

c = C(make_zero)
print(c.check())   # prints 0, matches CPython
```
compiles and runs correctly — calling a closure stored in a field
works. Only *testing its truthiness* does not.

## What's confirmed so far

- `if factory:` / `bool(factory)` (as an expression, not condition)
  — `bool(int)` fails differently, at the C-compile stage
  (`_CG_prim_coerce(_CG_bool, int64)` — literally emits the type name
  `int64` as if it were an expression into generated C — a distinct,
  narrower bug worth its own look but not chased further here).
- `str(factory)` — **compiles and runs**, printing a generic
  `<instance>` (CPython prints `<function make_zero at 0x...>` — a
  separate, cosmetic mismatch, not investigated). This shows
  `__str__` (also sent via the identical generic
  `call_method(..., sym___str__, ...)` mechanism, see
  `python_ifa_build_if1.cc`'s `print()` handling) resolves for a
  closure receiver where `__pyc_to_bool__` does not — so this isn't
  "closures have zero methods," it's specifically that
  `__pyc_to_bool__` (and by extension every boolean-context coercion
  in the frontend, all of which funnel through the same symbol) has
  no dispatch candidate for the closure Sym kind. Why `__str__`
  succeeds where `__pyc_to_bool__` doesn't was not traced further —
  worth checking first if picking this up (possibly a
  fallback/default-string-conversion path at the FA or codegen level
  that has no boolean-conversion equivalent).
- Separately, but related: calling a **builtin type constructor**
  stored in a variable (`factory = int; factory()`) fails, while
  calling a **user-defined function** stored the same way
  (`factory = make_zero; factory()`) works. This is what blocks
  `defaultdict(int)`/`defaultdict(list)` specifically (`self.factory()`
  in `pyc_lib/collections.py:34`) even *after* a `__pyc_to_bool__` fix
  — `if self.factory:` (this issue) gates it first, but the
  `self.factory()` call right after would need this second gap closed
  too. Not investigated further; flagged so whoever picks up this
  issue doesn't stop at the first error and declare victory.

## Why not root-caused further here

`__pyc_to_bool__`'s dispatch failure is a from-scratch FA/dispatch
investigation (why does the matcher find zero candidates for a
closure receiver specifically for this one symbol, when `__str__`
finds one) — a different kind of digging than the codegen-runtime
tracing this session's other item-3-adjacent work has done, and
deserves its own focused pass rather than a rushed guess.

## Verification plan once fixed

- The two repros above must both print `yes` (truthiness) and not
  regress the already-working `self.factory()` call-through-a-field
  case.
- `defaultdict(int)`/`defaultdict(list)` usage
  (`tests/defaultdict_keys_values.py` already exists per
  [issues/025](../../../issues/025-shedskin-examples-coverage.md)'s
  history — extend it, or add a new test, for the `b[k] += 1`
  auto-vivify shape specifically) should get further than today.
- `shedskin_examples/mastermind2/mastermind2.py` — re-run `pyc -r`
  and confirm the blocker moves past `collections.py:33` (it will
  very likely hit the *next* real gap, per this session's repeated
  "each fix reveals the next layer" pattern — don't expect this alone
  to make mastermind2 pass).
- Full `test_pyc.py`, both backends.

## What this unblocks

`collections.defaultdict` with any non-trivial factory
(`defaultdict(int)`, `defaultdict(list)`, `defaultdict(set)` — the
overwhelmingly common uses; `pyc_lib/collections.py`'s own
implementation is written the natural CPython-mirroring way and hits
this on its very first `__getitem__`) currently cannot compile at
all. More generally, any program passing a function/class reference
around as a plain value and testing it for truthiness (a common
`if callback:` optional-callback idiom) hits this.

## Fix (2026-08-08)

Root cause, found by comparing why `__str__` resolves for a closure
receiver but `__pyc_to_bool__` doesn't (the exact question this
issue's own "why not root-caused further" section left open):
`__pyc_any_type__` isn't an ordinary Python class at all —
`python_ifa_sym.cc:96`: `sym_any->name =
cannonicalize_string("__pyc_any_type__");`. It's pyc's Python-syntax
way of attaching methods directly to ifa's own universal top type
(`sym_any`, `ifa/if1/ast.cc`), which *every* type in the lattice
specializes, including closures/functions — they're a core ifa
concept and never go through `python_ifa_build_syms.cc`'s "a bare
`class X:` implicitly inherits `object`" rule, since that rule only
applies to user/builtin-module Python classes. `object` (the
Python-specific root regular classes actually descend from) defines
`__pyc_to_bool__`; `__pyc_any_type__`/`sym_any` did not — so a closure,
reachable only through the latter, had no candidate at all.
`__str__`, defined on *both* `object` and `__pyc_any_type__`,
resolved for closures via the latter — confirmed by the generated
C for `str(factory)`: the whole call collapses at compile time to
`__pyc_any_type__.__str__`'s body (`_CG_String("<instance>")`), not
`object.__str__`'s `"<object>"`.

**The fix**: add a default `__pyc_to_bool__` directly to
`class __pyc_any_type__:` in `__pyc__/00_runtime.py`, returning `True`
unconditionally — matching CPython's real default (any object,
including any callable, is truthy unless it overrides `__bool__`/
`__len__`, which a bare function never does). `object`'s own, more
specific `bool()`+`len()`-based `__pyc_to_bool__` still wins for
anything that reaches it (ordinary class instances); this is only the
fallback for receivers — closures, and presumably anything else that
only connects to `sym_any` — that don't.

**Verified:**
- Both this issue's repros (`if factory:`, and the `self.factory()`
  call-through-a-field case) now print `yes`/`0` correctly, matching
  CPython, on both backends.
- `defaultdict(int)`'s own `if self.factory:` (`pyc_lib/collections.py:33`)
  no longer warns — the compile progresses to the *next* gap
  (`self.factory()`, line 34), confirming this specific mechanism is
  fixed. That next gap is a different, deeper mechanism (non-record
  builtin types like `int` have no real `__new__` to call indirectly)
  — root-caused and filed separately as
  [ifa/091](091-DISPATCH-nonrecord-builtin-constructor-not-first-class.md)
  rather than folded into this fix, and **not fixed** — so
  `defaultdict(int)`/`defaultdict(list)` still don't work end-to-end
  yet.
- Editing `__pyc__/00_runtime.py` shifted line numbers in the
  concatenated `__pyc__.py` bundle; updated
  `tests/list_index_type_mismatch_salvage.py.check` accordingly (pure
  line-number drift in an unrelated pinned warning, not a behavior
  change).
- Full `test_pyc.py`, both backends: 261/11/0/4, clean.
