# 087 — out-of-order keyword arguments fail to dispatch (in-order works)

**Status:** closed, fixed 2026-08-10. Originally found 2026-08-08 and
named precisely in
[issues/025](../../../issues/025-shedskin-examples-coverage.md)'s TODO
list item 16 ("`f(high=9, low=2)` still fail to compile (safely — no
miscompile)") but never turned into its own issue file until now.

**Affects:** `ifa/if1/pattern.cc`'s matcher (`pattern_match` →
`Matcher::find_all_matches`) — confirmed NOT the pyc frontend
(`python_ifa_build_if1.cc` correctly tags every keyword argument with
its parameter name via `if1_add_send_arg(..., cannonicalize_string(...))`
regardless of the order it appears at the call site; verified by
reading that code path directly).

## Repro

```python
def f(low, high):
    return (low, high)

print(f(low=2, high=9))   # matches f's declared parameter order
print(f(high=9, low=2))   # reversed
```
CPython: `(2, 9)` both times (keyword arguments are matched by name,
order at the call site is irrelevant). pyc: the first (in-declaration-
order) call compiles and runs correctly; the second (reversed) call
compiles with `illegal call argument type` warnings and, if actually
reached at runtime, aborts via a guarded salvage trap
(`assert(!"runtime error: matching function not found")`) — confirmed
**not** a silent miscompile and **not** a whole-program crash: verified
with `stdbuf`-forced unbuffered output that every statement *before*
the bad call still executes and prints correctly; the trap is
localized to the one bad call site, matching the original TODO note's
"safely" characterization exactly.

## What's confirmed vs. not

Confirmed:
- The frontend passes both calls' arguments identically in *shape*
  (each keyword argument tagged with its own name, positional
  arguments untagged) — the only difference between the two calls is
  the *order* the named arguments appear in the SEND's argument list,
  since the frontend builds them in call-site (i.e. source) order, not
  sorted or otherwise normalized.
- The actual matching entry point is `pattern_match`
  (`ifa/if1/pattern.cc:1600`), which calls `Matcher::find_all_matches`
  — a backtracking matcher over `MPosition`s (canonicalized
  position/name paths) that's substantial and general-purpose (shared
  by every call in every ifa-based frontend, not pyc-specific).

Not traced further: the exact line/branch inside
`find_all_matches`/`find_best_matches` that's order-sensitive. Given
the in-order case's SEND has kwargs in the same left-to-right order as
`f`'s formal parameter list, and the reversed case doesn't, the likely
shape of the bug is that some part of the matcher walks `names`
positionally (assuming the Nth named argument corresponds to the Nth
formal-with-a-name it hasn't matched yet) rather than doing a true
name→formal lookup via `named_to_positional` for every argument
independent of the others' order — but this is a hypothesis, not
confirmed by tracing the actual matcher logic, which is intricate
enough (recursive backtracking, `MPosition` canonicalization) to
warrant its own dedicated pass rather than guessing at a fix here.

## Verification plan

- Trace `Matcher::find_all_matches`/`find_best_matches` with the
  minimal repro above (this issue's own two-line function is a clean,
  general, non-corpus-specific case — no need for a shedskin example)
  to find the actual order-dependent step.
- `python3 repro.py` → `(2, 9)` twice is the reference.
- Full `test_pyc.py` both backends — this is core dispatch matching,
  a hot, shared path; treat any change as needing the full sweep, not
  a spot-check.
- Add a regression test once fixed (`tests/kwarg_out_of_order.py` or
  similar) — nothing in the current suite exercises reversed-order
  keyword arguments (`tests/keyword_args.py` and
  `tests/kwarg_past_default.py` both exist for related but distinct
  keyword-argument bugs, neither covers pure reordering).

## What this unblocks

Correct, order-independent keyword-argument dispatch — matches
CPython's actual semantics (keyword arguments are matched by name,
never by position). Currently a silent trap for any call written with
keywords in a different order than the callee's declaration, which is
completely ordinary, unremarkable Python style (there's no convention
requiring keyword arguments to be written in declaration order). Low
urgency per the original TODO note (safe failure, easy workaround —
reorder the call or use positional args) but a real correctness gap
in core dispatch, not corpus-specific.

## Fix

The original framing above (an order-sensitive bug somewhere inside
`Matcher::find_all_matches`/`find_best_matches`) turned out to be
wrong once actually traced: extensive instrumentation confirmed every
stage of the matcher — `find_all_matches`, `build_positional_map`,
`find_best_cs_match`, `covers_formals`, `verify_args` — correctly
remaps out-of-order named actuals to their formal positions via
`PMatch::actual_to_formal_position`/`formal_to_actual_position`,
regardless of call-site order. The match is found correctly.

The real bug is one level further out, in `Matcher::build()`
(`ifa/if1/pattern.cc:725`). After a successful match, `build()`
detects the reordering (`to_actual(p, m) != p` for some formal
position `p`) and calls `if1->callback->order_wrapper(f,
m->order_substitutions)` to synthesize a forwarding adapter — a
virtual callback declared in `IFACallbacks` (`ifa/ifa.h`) whose
base-class implementation is `{ return 0; }`. Unlike its sibling
`default_wrapper` (implemented by pyc, exercised constantly for
default arguments), `order_wrapper` was **never overridden by
`PycCompiler`** — pyc never needed it before, since this is the first
construct that actually requires reordering a matched call's
arguments. The unimplemented stub returns null, `build()` bails out,
and the match that was found correctly upstream gets silently
dropped, surfacing far downstream as the "matching function not
found" runtime trap.

Two other sibling callbacks (`coercion_wrapper`, `promotion_wrapper`)
are equally unimplemented stubs in `ifa.h`, but confirmed (by grep and
by reasoning about pyc's type system) to never actually be exercised
by pyc — numeric coercion/promotion goes through a separate mechanism
(`AVar::num_coerce`), so those two stay stubs with no observed impact.

**The fix** implements `PycCompiler::order_wrapper` in
`python_ifa_sym.cc` (declared in `python_ifa_int.h`), mirroring the
existing `default_wrapper`'s shape: one fresh formal Sym per
`has`-position, a single forwarding `SEND` to `f`, installed via
`install_new_fun`. The key difference from `default_wrapper` is
indexing: `default_wrapper`'s wrapper formals are built in `f`'s own
declared order; `order_wrapper`'s wrapper formals must instead be
built in *actual* (call-site) order, since that's what the original
call site's arguments arrive as — codegen calls whatever `Fun` the
matcher settled on with the original `SEND`'s `rvals`, unchanged, in
call-site order.

**A second, subtler bug** surfaced during verification: when a call
needs *both* a default-argument fill *and* a reorder in the same
match (`g(c=3, a=1)` against `def g(a, b=99, c=100)`), the naive
version of `order_wrapper` produced silently wrong output (`(1, 3,
100)` instead of the correct `(1, 99, 3)`) rather than the previous
safe "matching function not found" trap — a regression in severity,
since a silent miscompile is worse than a guarded crash. Root cause:
`order_wrapper`'s forwarding `SEND` is itself an ordinary,
dynamically-dispatched call — its callee slot (`rvals[0]`) carries the
*original*, unwrapped function's own value (e.g. `g`, not the
default-wrapper `f` that `order_wrapper` was actually asked to wrap),
so the matcher re-resolves this inner send from scratch against `g`'s
real (un-compacted, un-reordered) signature. Without argument names
attached, that re-resolution falls back to raw position — and once
`f`'s compacted position order (after default-skipping) no longer
lines up with `g`'s true declared order, positional-only forwarding
silently mismatches (the default-wrapper's own "b" default and the
reordered "c" actual landed in each other's slots).

Fixed by having both `default_wrapper` and `order_wrapper` carry the
wrapped formal's original *name* through onto each synthesized formal
(`a->name = orig->name`) and attach it to the forwarding `SEND`'s args
via `if1_add_send_arg`'s optional `name` parameter. Since names are
position-order-independent and the matcher's named-argument path was
already proven correct (that's the whole first half of this fix), any
re-resolution of a wrapper's inner forwarding send — no matter how
many layers of wrapping are stacked — now routes correctly by name
instead of relying on position.

### Verification

- `python3 repro.py` reference case (`f(low=2,high=9)` /
  `f(high=9,low=2)`) → `(2, 9)` both times, confirmed on both backends.
- A 10-line matrix covering: 3-argument functions reordered every way,
  reordering mixed with one leading positional, reordering in a class
  `__init__`/method context, and defaulted arguments both with and
  without reordering (`g(1)`, `g(a=1)`, `g(c=3,a=1)`, `g(1,c=3)`) — all
  10 lines match CPython exactly on both the C and LLVM backends.
- Full `test_pyc.py`, C backend: 264 passed, 14 expected fails, 0
  failed, 4 skipped.
- Full `test_pyc.py`, LLVM backend (`PYC_FLAGS="-b"`): 264 passed, 14
  expected fails, 0 failed, 4 skipped.
- `ifa`'s own `make test` (all phases, `ifa-test` UnitTest framework):
  all phases 0 failed.
- New regression test added: `tests/kwarg_out_of_order.py` (covers all
  of the above shapes in one file, with `.exec.check` verified against
  CPython's own output).
