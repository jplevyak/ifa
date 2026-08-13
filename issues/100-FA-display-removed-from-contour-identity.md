# 100 — the lexical display is no longer part of contour identity (and the precision that cost)

**Status:** landed 2026-08-13, by explicit design decision: *the display
exists for nested functions; it must not be used for anything else.*
Two pyc tests regress to a **runtime crash** and are knowingly left
failing — they are tracked here, not papered over. Supersedes
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

**Two pyc tests now miscompile — knowingly left failing:**

- `exception_assert.py` and `raise_exception_qualified.py` both emit
  `warning: expression has no type` and then die at runtime on
  `Assertion !"runtime error: getter not resolved"`. Both are exception
  paths: the NOTYPE is converted to void (`convert_NOTYPE_to_void`) and
  the exception object's field getter is then unresolved.

Three further tests changed compile output only, with runtime behavior
unaffected, and their `.check` goldens were updated: `match_none.py`,
`match_seq.py`, `minmax_3arg.py` (its `.exec.check` still matches
exactly).

Suite: **263 passed / 14 expected fails / 2 failed / 4 skipped** (was
265/14/0/4).

## Follow-on work

1. **Fix the two exception-path miscompiles.** They are the concrete
   price of the merge and the first thing to recover. The shape — a
   contour merge making an exception field's type NOTYPE — suggests the
   exception carrier's contour is one where the display was doing real
   work, i.e. a genuine nested-function/closure case rather than a
   phantom method display. If so the right repair is to distinguish
   those, not to restore the blanket check.
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

## Verification plan

1. `ifa --test` 58/0 and `test_pyc.py` at exactly 263/14/2/4 — the two
   named failures and no others.
2. Full shedskin sweep diffed for exit-code changes (currently zero).
3. Any fix for follow-on 1 must not restore contour growth: re-check
   `yopyra` stays `pass_limit_hit=0` and the ess table above does not
   regress.
