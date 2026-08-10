# 092 — 3-arg `max`/`min` inside a function with more than one differently-shaped `return` crashes the caller: "matching function not found"

**Status:** open, found 2026-08-08 while porting
[issues/041](../../issues/041-stdlib-shim-stubs-silently-wrong.md)'s
`colorsys` shim — CPython's own `rgb_to_hls`/`rgb_to_hsv` use
`max(r, g, b)`/`min(r, g, b)` inside a function with two `return`
statements (one for the achromatic/gray case, one for the general
case), hitting this. Worked around in the shipped fix (local
3-value-comparison helpers instead of the builtin), not root-caused
or fixed here.

**Affects:** unclear which layer — the crash fires at the *caller*
site (`assert(!"runtime error: matching function not found")`,
`ifa/codegen/cg.cc`'s `get_target_fun`, same site
[ifa/issues/090](090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md)
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
[ifa/issues/090](090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md)
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
