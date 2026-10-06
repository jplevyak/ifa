# 184 — an attribute read on a `{None, T}` receiver was not checked

**Status: FIXED 2026-10-06**, in two commits: the method call
(`6db520ba`), then the attribute read itself (this file's "The attribute
read" section). Every `x.name` on a `{None, T}` receiver whose None has
no `name` now raises CPython's `AttributeError` message at the read, on
both backends. The residual is ifa/165's: the report exits, so
`except AttributeError` cannot catch it.

## Symptom

```python
import sys

class A:
    def m(self, k):
        return k + 1

x = None
if len(sys.argv) > 5:
    x = A()
print(x.m(1))
```

| | result |
| --- | --- |
| CPython | `AttributeError: 'NoneType' object has no attribute 'm'` |
| pyc, default and `--strict`, both backends | prints `2`, exit 0 |

If `m` reads `self` (say `return k + self.v`), pyc raises correctly. So
this is not the ifa/165 check missing. The check is selected and then
skipped.

## Root cause

[ifa/165](../165-none-reaching-an-operation-is-silently-accepted.md)'s check
has three users that all call `nil_receiver_rval(pn, fn)`, which maps the
callee's `self` formal to the send's rval at that position. In pass
order:

1. **DCE** (`dead.cc`, `mark_live_code`) keeps the receiver live so codegen
   has a value to test.
2. **The inliner** (`inline.cc`) declines to inline such a site.
3. **Codegen** (`cg.cc` `emit_send_call`, `cg_emit_llvm.cc`) emits the
   test and the `_CG_none_receiver` report.

A method call `x.m(k)` is lowered as a member read that builds a
bound-method closure, `period(x, m)`, then a call of that closure: rvals
`[closure, k]`. Only the inliner's `simple_closure_call` rewrites it to
the direct send `[m, x, k]`, and that happens AFTER DCE. So at DCE,
`nil_receiver_rval` mapped `self` (position 2) to `rvals[1]`, which is
`k`, an `int64`, and returned -1, and `x` was never marked live. When the
callee reads `self`, the actual `x` is live anyway, which is why the
`self.v` variant worked. When it does not, nothing reads `x`, and the
receiver load is deleted: the generated C called `A::m(1)` with no read
of `x` at all.

Codegen then saw the rewritten send, picked the check (`nri = 1`), found
no C value for the receiver, and its `if (rs)` guard dropped the check
without a word. The LLVM path had the same `if (rv && ...)` guard.

`tests/none_receiver_raises.py` did not catch this because it is an
operator (`a[1] + "y"`), whose receiver is a direct rval from the start.
It would not have caught a regression either: the harness passed a
`.check_fail` test on ANY failure or ANY output diff, so wrong output with
exit 0 passed.

## Fix (landed)

- `nil_receiver_var(pn, fn)` (fa.cc) returns the receiver `Var` in either
  shape. For the closure-call form it follows the closure through any
  MOVEs to its `period` and uses the same mapping the inliner's rewrite
  uses (selector `rvals[3]`, receiver `rvals[1]`, then the call's own
  arguments). DCE uses it.
- `cg.cc` and `cg_emit_llvm.cc` fail with an internal error when the
  selected check has no receiver value, instead of skipping it.
- `test_pyc.py`: a `.check_fail` test that has an `.exec.check` must exit
  nonzero AND match it.
- `tests/none_method_receiver_raises.py`.
- `splitter_setter` / `splitter_setter_of_setter` goldens re-blessed for
  `CALLS:` +1 / +3 only (their `STAGES:` are unchanged). `a.v` there is
  `{None, A}` (`Box.__init__` sets `self.v = None`) and `tag` ignores
  `self`, so those calls now keep their receiver load and are checked,
  which is correct.

## The attribute read (landed)

The real confluence is the ATTRIBUTE READ, not the call: CPython raises at
`x.m` and at `x.f`. After the fix above two shapes were still wrong:

1. **A stored bound method**, `f = x.m` then `f(1)`: pyc printed the
   following statement's output and `2`. That closure call is not a
   *simple* one, so the inliner never rewrites it and no call-site check
   sees the receiver.
2. **A field read**, `print(x.f)` with `A.f` always `3`: pyc printed `3`.
   The read's result folded to a constant, codegen skipped the send
   (`virtual_cg_is_const_folded_send`), and nothing read `x`.

The check now lives on `period` itself:

- `nil_period_receiver(pn, &sel)` (fa.cc): a `period` whose receiver may
  be None, is otherwise an object (not a scalar), and whose None
  CreationSet has no `sel` in its `var_map`. That is the lookup FA's
  `period` transfer function makes, so None's real members (`__str__`,
  `__eq__`, `__bool__`, ...) are never checked.
- DCE (`mark_initial_dead_and_alive`): such a read can raise, so it is a
  root; live, its receiver stays live.
- `virtual_cg_emit_send` (codegen_common.cc) emits the check BEFORE the
  constant-fold early-out, through `VirtualCGEmitter::emit_none_check`,
  which is pure virtual. Both backends implement it and reuse it for the
  ifa/165 call-site check. `cg.cc`'s closure-building branch skips a read
  kept live only for its check (the closure itself dead, e.g. when `f(1)`
  resolved to a direct call), as its field branch already did.
- `tests/none_attribute_read_raises.py`, `tests/none_bound_method_raises.py`.

Cost: the corpus binaries carry 409 checks (chull 64, quameon 53, pylife
52). chull built from its own generated C with and without them, pinned to
one core, 5 alternating runs each: 29.2 s vs 28.5 s mean, inside the
runs' 23-34 s spread.

## Verification

- The repro raises CPython's message on both backends; the not-None path
  still prints `2`.
- `make test`: 402 passed / 0 failed on both backends.
- Corpus run sweep `run__default__eb4f98c5+8e26e37a` (the method call) and
  `run__default__6db520ba+2b94c76a` (the attribute read): every compile
  rc, run rc, warning count and demand column identical to the sweep
  before each. No corpus program reaches a None receiver at run time.
- `make test`: 404 passed / 0 failed on both backends after the attribute
  read.
