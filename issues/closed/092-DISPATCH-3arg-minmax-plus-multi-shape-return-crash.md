# 092 — 3-arg `max`/`min` inside a function with more than one differently-shaped `return` crashes the caller: "matching function not found"

**Status:** fixed 2026-08-10 (see "Fix" section at the end) — found 2026-08-08 while porting
[issues/041](../../../issues/041-stdlib-shim-stubs-silently-wrong.md)'s
`colorsys` shim — CPython's own `rgb_to_hls`/`rgb_to_hsv` use
`max(r, g, b)`/`min(r, g, b)` inside a function with two `return`
statements (one for the achromatic/gray case, one for the general
case), hitting this. Worked around in the shipped fix (local
3-value-comparison helpers instead of the builtin), not root-caused
or fixed here.

**Affects:** unclear which layer — the crash fires at the *caller*
site (`assert(!"runtime error: matching function not found")`,
`ifa/codegen/cg.cc`'s `get_target_fun`, same site
[ifa/issues/090](../090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md)
hits), not inside the function using `max`/`min` itself — so this
looks like a dispatch/clone-resolution issue triggered by something
about how a function using 3-arg `max`/`min` gets specialized, not a
`max`/`min`-internal bug per se. Not traced further.

## Repro

```python
def f(r, g, b):
    maxc = max(r, g, b)
    v = maxc
    if r == g:
        h = 1.0
        s = 2.0
        return h, s, v
    s = 4.0
    h = 5.0
    return h, s, v

a = f(0.5, 0.5, 0.9)
print(a)
```
- CPython: `(1.0, 2.0, 0.9)`.
- pyc: compiles with warnings pointing at the `max(r, g, b)` line
  itself (`expression has no type`), then aborts at runtime:
  `assert(!"runtime error: matching function not found")`.

## Narrowing (each tested standalone against the repro above)

- Removing `max(r, g, b)` entirely (keep the two-return-shape
  function) — **no crash**. Confirmed with plain variables and with
  same-named variables across both `return` branches (not about
  reusing variable names across branches, which was an early false
  lead).
- Replacing `max`/`min`'s **3-arg** call with the **2-arg** form
  (`max(r, g)`) — **no crash**. Specific to 3 (or more?) positional
  arguments; not tested with 4+.
- Replacing `max(r, g, b)` with a hand-written 3-value-comparison
  helper (no builtin `max`/`min` call at all) — **no crash**, same
  result. This is the workaround shipped in `pyc_lib/colorsys.py`.
- `min(r, g, b)` alone (no `max`) — not independently re-tested, but
  `min`/`max` share the same default-arg/narrowing implementation
  pattern per this codebase's own prior notes, so presumed to trigger
  identically; worth confirming first if picking this up.

So the precise trigger is: a function containing a 3-(or-more)-arg
`max`/`min` call, where **that same function** also has more than one
`return` statement producing differently-shaped tuples (here,
`(h, s, v)` from two branches that assign the same three names but via
different literals/expressions — narrowing shows this isn't about the
values differing, just about there being 2+ distinct `return`
statements in the function's body at all, once `max`/`min` is
involved).

## Why not root-caused further here

This needs a genuine FA/dispatch investigation (why does adding a
3-arg `max`/`min` call change how the *caller's* dispatch resolves,
when the crash site is nowhere near the `max`/`min` call itself) —
the same kind of digging
[ifa/issues/090](../090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md)
and [ifa/issues/091](091-DISPATCH-nonrecord-builtin-constructor-not-first-class.md)
were also deferred for, and this session was already several layers
deep chasing issues/041's colorsys port when this was found. A
working, correct colorsys shim was more valuable to land now than a
fully root-caused fix for this.

## Verification plan

- The repro above must print `(1.0, 2.0, 0.9)`, matching CPython.
- `pyc_lib/colorsys.py`'s `rgb_to_hls`/`rgb_to_hsv` could switch back
  to plain `max(r, g, b)`/`min(r, g, b)` once fixed (currently using
  local `_max3`/`_min3` helpers instead) — not required, just a nice
  signal the fix is complete and matches upstream CPython's own source
  more closely.
- Full `test_pyc.py`, both backends.

## What this unblocks

Any function that both uses 3-arg `max`/`min` and has more than one
`return` statement — a common combination (`max`/`min` over 3+ values
is ordinary code; multi-branch returns are everywhere). Confirmed to
affect CPython's own `colorsys.rgb_to_hls`/`rgb_to_hsv` shape
specifically, worked around rather than blocking that fix.

## Fix (2026-08-10)

The "dispatch/clone-resolution" framing above was wrong — this isn't
an FA/dispatch mystery at all, and the multi-return-shape angle was
coincidental. Root cause, found by just reading `__pyc__/05_builtins.py`'s
`min`/`max`: their signature is `def max(a, b=None, key=None)` — only
**two** plain positional values. `max(r, g, b)` passes three, so the
third value silently binds into the `key` formal (there's no other
slot for it) instead of raising an arity error. `key` then holds a
plain number instead of `None`, so the `key is None` branch is false
and the code takes `key(b) > key(a)` — calling a non-callable value.
Confirmed this reproduces with a single-`return` function, no second
`return`/differently-shaped-tuple needed at all:
```python
def f(r, g, b):
    return max(r, g, b)
print(f(0.5, 0.9, 0.3))
```
already hit the identical crash, with warnings pointing straight at
the call (`illegal call argument type 'key' illegal: float64`,
`unresolved call '__gt__'`).

Checked whether real CPython-style variadic `min`/`max` (`*args`) was
an option: pyc doesn't support `*args` in user-defined functions at
all yet — even a standalone `def f(*args): ...` aborts at runtime with
the same "matching function not found" — so that's a separate,
considerably larger gap, not pulled in here.

**Fix**: added an explicit `c=None` third positional formal to both
`min`/`max`, handling the 3-value case directly (`m = a; if b>m: m=b;
if c>m: m=c; return m`, and the mirror for `min`). Purely additive —
the 1-arg-iterable, 1-arg+`key=`, 2-arg, and 2-arg+`key=` forms are
unaffected since `c` stays `None` for all of them. Does **not**
generalize past exactly 3 positional values (a 4th would still
misbind into `key`, same as before) — matches everything the corpus
actually calls with; a fully general variadic form needs real
`*args` support first.

**One real regression caught by the full suite, fixed before landing**:
the first version checked `if c is not None:` as the very *first*
branch (before the pre-existing `if b is None:` check). That alone —
just adding a 4th formal and changing which condition FA's splitter
sees first — broke the *unrelated* 1-arg-iterable contour
(`max([False, True, False])`, `tests/bool_ordering.py`): `m` came out
typed as the whole list instead of an element, the same
"adding-a-param-disturbs-the-splitter" fragility this same file's own
`sorted()` comment already documents for an unrelated case. Fixed by
reordering so `c is not None` is checked *after* the original
`b is None` branch, keeping that branch's position (and every
pre-existing contour's entry path) byte-identical to before this fix;
the new 3-value contour only had to coexist with the branches below
it, not preempt them. This is the reason the fix isn't simply "add one
`if` block" — the ordering is load-bearing, not stylistic.

`pyc_lib/colorsys.py`'s `rgb_to_hls`/`rgb_to_hsv` switched back to
plain `max(r, g, b)`/`min(r, g, b)` (removed the local `_max3`/`_min3`
workaround helpers entirely), now matching CPython's own source
verbatim on this point.

**Verified**: the original repro prints `(1.0, 2.0, 0.9...)` matching
CPython (mod the pre-existing, already-documented float-`str`
verbosity divergence); `min`/`max` spot-checked for 3-arg int/float/
negative values plus the untouched 1-arg-iterable/2-arg/`key=` forms,
on both backends. Full `test_pyc.py`, both backends (`IFA_LLVM=1`):
clean 263/12/0/4 (one incidental `.check` sidecar line-number-drift
regenerated, same pre-existing drift class noted in earlier issues —
not a behavior change). `ifa`'s own `make test` (all phases +
`ifa-test` UnitTest): clean.
