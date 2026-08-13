# 100 — the lexical display is no longer part of contour identity (and the precision that cost)

**Status:** landed 2026-08-13, by explicit design decision: *the display
exists for nested functions; it must not be used for anything else.*
The two runtime regressions it caused were **root-caused and fixed the
same day** (see "The two crashes" below — the real cause was a
pre-existing dropped-value bug in `flow_var_to_var`, not the display
removal itself); the suite is back to its full 265/14/0/4. Supersedes
[074](074-FA-cross-pass-oscillation-plan.md)'s Stage 0 and Stage 4, which
prototyped this behind flags and concluded the display could not be
dropped; that conclusion is now overridden as a policy choice rather than
refuted as a measurement.

**Affects:** `ifa/analysis/fa.cc` — `update_display`, `set_entry_set`,
`entry_set_compatibility`, `check_split`, `split_edges`'s
`resolve_target`, `apply_entry_set_split`'s ledger ROUTE;
`ifa/analysis/fa.h` (`EntrySet::display_variants`); `ifa/if1/fun.h`
(`Fun::max_live_display_slot`).

## What the display is for, and what it was being used for

`EntrySet::display` has exactly **one** legitimate consumer: `make_AVar`
(`fa.cc:212`) resolving a `Var` that belongs to an enclosing scope —

```cpp
return unique_AVar(v, es->display[v->sym->nesting_depth - 1]);
```

— i.e. genuine nested functions, plus `clone.cc`'s post-FA equivalence,
which asks the same "which enclosing contour" question. Everything else
was using the display as **contour identity**: a test of whether an edge
may share a contour. Removed:

| removed | was used by | effect |
|---|---|---|
| `edge_nest_compatible_with_entry_set` | `entry_set_compatibility`, `check_split`'s knot search, `check_split`'s lineage branch | rejected routing candidates purely on lexical display |
| `edge_display_compatible` | `split_edges`'s `resolve_target` (×2) | same, for CS-partition products |
| `find_or_make_display_variant`, `EntrySet::display_variants` | `resolve_target` | existed *only* to work around the above |
| `group_display_ok`, `fun_max_live_display_slot`, `stage4_enabled`, `Fun::max_live_display_slot`, `PYC_STAGE4` | the ledger ROUTE gate | 074 Stage 4's partial mitigation of the same constraint |
| `update_display`'s consistency assert | — | enforced "one lexical display per contour", the identity constraint itself |

`update_display` still **builds** the display, from the first edge to
reach the contour. A later edge with a different lexical display now
shares the contour and resolves enclosing-scope Vars through that first
stamp. That is a precision loss, not unsoundness — two callers' captured
variables union instead of staying separate.

## Why: the display was a major driver of contour growth

Per [074](074-FA-cross-pass-oscillation-plan.md)'s growth census,
`check_split`'s lineage branch rejected the split parent's target on
`!edge_nest_compatible_with_entry_set` for 71-142 candidates per pass on
`yopyra`, and then minted a fresh contour linked by `e->to->split =
ee->to` — 073's "sole unbounded EntrySet generator" — at 34-68 new
contours per pass, forever. pyc gives every method a `nesting_depth` it
does not need, so that constraint was largely enforcing a *phantom*
display (issue 064).

**Removing it shrinks the contour graph corpus-wide**, typically by
40-80%:

| program | ess before | ess after | | program | ess before | ess after |
|---|---|---|---|---|---|---|
| hq2x | 5120 | 1664 | | chess | 3012 | 1190 |
| plcfrs | 2724 | 946 | | pygasus | 2700 | 1805 |
| yopyra | 2151 | **414** | | loop | 1195 | 433 |
| sudoku4 | 1188 | 707 | | linalg | 1089 | 510 |

`yopyra` — 074's pure-growth canary, which had been running to the hard
pass cap at 102 passes — now **converges** (`pass_limit_hit=0`, 46
passes).

## What it cost

**Precision, widely.** Violation counts rise on many programs as contours
that were kept apart by display now merge: `rdb` 602 → 3181, `mastermind2`
58 → 554, `sat` 2 → 401, `chess` 63 → 331, `sunfish` 11 → 199, `go` 59 →
161, `tictactoe` 0 → 137. Some fall: `sudoku4` 142 → 30, `plcfrs` 5494 →
2442, `msp_ss` 947 → 514, `dijkstra` 26 → 10.

**Net oscillator count gets worse: 16 → 20.** `yopyra` leaves the set;
`hq2x`, `mastermind2`, `sat`, `sunfish` and `tictactoe` enter it.

**Compile outcomes are unchanged**: zero exit-code differences across the
84-program shedskin sweep. `ifa --test` 58/0.

**Two pyc tests initially miscompiled** — `exception_assert.py` and
`raise_exception_qualified.py`, both dying on
`Assertion !"runtime error: getter not resolved"`. **Fixed**; see the
next section. Three further tests changed compile output only, with
runtime behavior unaffected, and their `.check` goldens were updated:
`match_none.py`, `match_seq.py`, `minmax_3arg.py` (its `.exec.check`
still matches exactly).

Suite after the follow-up fix: **265 passed / 14 expected fails / 0
failed / 4 skipped** — the full pre-change baseline.

## Follow-on work

1. **Fix the two exception-path miscompiles — diagnosed 2026-08-13,
   see below.** They are the concrete price of the merge and the first
   thing to recover.
2. **The precision losses are the same question at corpus scale.** The
   display was substituting for a type/CS-based separation that does not
   exist yet; that separation is 074's Stage 2 (CS-directed fan-out),
   which 074 ruled out on the *old* measurements. Those measurements are
   now stale — the growth it was competing against is gone — so Stage 2
   is worth re-deriving against this baseline.
3. **`Sym::nesting_depth` for methods.** The deeper fix is that pyc's
   `def_fun_pyda` gives methods a nesting depth they do not need
   (`python_ifa_build_syms.cc:1917`, `:2030`, `:2154`). With the display
   out of contour identity, giving methods `nd 0` is now a much smaller
   change than 064 found it, and would remove the phantom display at the
   source instead of ignoring it in FA.


## The two crashes — root cause and fix (2026-08-13)

**The display removal was the trigger, not the cause.** Minimizing the
failure moved the target twice, and both intermediate readings were
wrong:

- first read: "the caught exception variable `e` has no type" — refuted,
  because `print("after")` *outside* the try/except also failed;
- second read: "`__str__` dispatch on the print argument" — refuted by
  `v5` below, which has no `print` at all.

Minimal repro (10 lines, from a six-variant matrix):

```python
def f(n):
    assert n > 0, "must be positive"
    return n * 2

try:
    a = f(5)
    b = f(6)          # <-- a SECOND call is required
except AssertionError as e:
    print("c1")
print("done")         # <-- this is what goes NOTYPE
```

One call converges; `raise` instead of `assert` converges; the `as e`
binding is irrelevant (a bare `except AssertionError:` fails too).

**The chain**, traced on that repro:

1. `print("done")`'s callee is a bound-method closure whose captured
   receiver slot is **empty** — for a *string constant* receiver:
   `closure#933 vars=[av739/__str__/n1, av741/-/n0]`.
2. `application()` therefore takes `partial_application`, which
   dispatches `__str__` with an empty `self`; every overload dispatches
   on `self`, so `pattern_match` returns zero candidates, the send
   produces no edge, and its result goes NOTYPE.
3. `make_period_closure` **is** called every pass with a typed receiver,
   and `make_closure_var` runs `flow_var_to_var(cav, iv)` with `cav`
   holding a concrete type. Yet `iv` (the slot) stays empty, with
   `iv->in == 0` and no restrict — and the probe shows the link
   `cav -> iv` **already exists**.

So the actual bug is in `flow_var_to_var`:

```cpp
if (a->forward.set_in(b)) return;    // <-- drops the value
```

The invariant every consumer relies on is `b->in >= a->out` for each
link. The early return breaks it: the link is created once, at whatever
moment the constraint generator first ran, and if `a` was empty then, the
re-assert is skipped forever after — `propagate_out_change` only pushes
on a *change* to `a->out`, so a value that arrives in between is never
delivered. **Fix:** re-assert unconditionally (`update_in` is a no-op
when nothing changes, so this costs one union test on an established
edge).

This is a *pre-existing* bug — 098's investigation had probed for exactly
this condition and measured it at zero, because the display checks were
keeping the affected contours apart. Removing them exposed it.

**Result:** all six repro variants fixed; `exception_assert.py` and
`raise_exception_qualified.py` pass; suite back to **265/14/0/4**;
`ifa --test` 58/0; **zero exit-code changes** across the 84-program
sweep; and the oscillation/ess tables above are unchanged except `chaos`,
whose contour count recovers 44 -> 566 (44 was a lost program; it now
compiles and runs to completion — it is simply slow, CPython times out on
it too).

## Verification plan

1. `ifa --test` 58/0 and `test_pyc.py` at exactly 263/14/2/4 — the two
   named failures and no others.
2. Full shedskin sweep diffed for exit-code changes (currently zero).
3. Any fix for follow-on 1 must not restore contour growth: re-check
   `yopyra` stays `pass_limit_hit=0` and the ess table above does not
   regress.
