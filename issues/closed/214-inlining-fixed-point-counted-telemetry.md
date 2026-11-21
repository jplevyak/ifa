# 214 — `simple_inlining` ran one pass in production, up to four with telemetry on

**Status:** closed 2026-11-21. Found while reviewing PR #6 (the compilation
explorer): `pyc --html` aborted on `shedskin_examples/chull`, which compiles
plainly.

## Symptom

```
$ pyc --html chull.py        # also --dump-json, -B, PYC_HTML=1
pyc: optimize/inline.cc:332: inline_single_pnode: Assertion `i >= 0' failed.
```

Five lines reproduce it (`tests/inline_rewritten_body.py`):

```python
def f(n):
  return float(n)
print(f(3))
print(float(7) / 2)
```

## Root cause

Two defects, the first hiding the second.

1. **The fixed-point loop counted telemetry.** issue 022 (2026-06-19) made
   `simple_inlining` repeat `inline_single_sends` until a pass inlined
   nothing, and measured "nothing" as *`inline_events_storage` did not
   grow*. That sidecar records only after `inline_events_enable()`, which
   production never calls (the gate dates from 2026-06-01), so in every
   plain compile the loop stopped after one pass. ifa-test's inline phase and
   the explorer enable the sidecar, so they ran up to four passes: tests and
   telemetry saw a different inliner from the one that shipped. 022's "zero
   delta" coverage measurement saw one pass.

2. **Rewritten bodies broke the substitution.** After a pass,
   `sub_constants()` replaces a constant rval with `new_live_Var(c)`, whose
   *sym* is the constant or symbol and whose `Var::constant` is null. The
   single-send and prim-chain matchers accept that form
   (`sym->is_constant || sym->is_symbol`), but `inline_single_pnode` and
   `inline_prim_chain`'s `subst_rvals` tested only `Var::constant`, so on the
   second pass they looked `__operator` up among the callee's formals and
   asserted. `float(int)` reaches `int.__pyc_float__` (`return self + 0.0`)
   through the synthesized `__coerce__`, which is enough.

Bisected to the allocation of `InlineEvent` before the loop was found:
skipping only that allocation made chull compile, which first read as a
heap-order dependence. It was the sidecar growing, i.e. the loop going on.

## Fix

- `inline_n_applied` counts applied inlines whether or not the sidecar is on;
  the loop tests it.
- `inline_rval_constant()` is the matchers' notion of a constant rval, used
  by the single-send pre-check and substitution, `subst_rvals` and
  `prim_chain_substitution_safe`; the pre-check bails on a non-formal
  instead of leaving it to the assert.

## Verification

- `make test` green on both backends. The second pass inlines a little more:
  `const_bytes_truthiness_or` direct calls 77 → 76 and `splitter_mark_type`
  89 → 87, `dynamic=0` unchanged (the `.check` files were updated for that
  line only).
- `test_explorer.py` (`make test-explorer`) fails if telemetry changes the
  generated C or LLVM IR; restoring the event-counted loop trips it.
- Corpus `check` sweep, main `6b762974` vs this branch `93724ae1`: every pyc
  column identical on all 77 programs (compile/run status, warnings, stdout
  verdict, ess/css/shapes); 76 match CPython, `othello2` differs on both. The
  only differing cells are CPython's own runs on main (`msp_ss` rc 1,
  `neural1` timeout), clean on the second run.
