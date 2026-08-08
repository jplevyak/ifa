# 089 — a first-class function/closure value has no `__pyc_to_bool__` dispatch candidate: `if some_function:` fails to type

**Status:** open, found 2026-08-08 while diagnosing
[issues/025](../../issues/025-shedskin-examples-coverage.md)'s TODO
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
  [issues/025](../../issues/025-shedskin-examples-coverage.md)'s
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
