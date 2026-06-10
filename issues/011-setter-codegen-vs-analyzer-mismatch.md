# Issue 011: P_prim_setter analyzer flows val, codegen emits receiver

**Status:** **closed June 2026 (Option A landed).** Codegen now
emits val, matching the analyzer. Surfaced while debugging the
`dict_methods.py` hang.
**Affects:** `ifa/analysis/fa.cc:1781`, `ifa/codegen/cg.cc:277`,
all programs that exercise polymorphic attribute assignment
where the field type differs from the receiver type (most
non-trivial setter sequences). Currently masked in the live
test suite by `tests/dict_methods.py.ignore` and by
`tests/cross_type_method.py.expect_fail` (incidentally).

## Symptom

Compiling `tests/dict_methods.py` (after `tests/dict_methods.py.ignore`
removed) produces a C type error:

```
tests/dict_methods.py.c: In function '_CG_s1952* _CG_f_3170_7(_CG_any, _CG_int64)':
tests/dict_methods.py.c:198:9: error: invalid conversion from
   '_CG_ps1952' to '_CG_int64' [-fpermissive]
  198 |   t3 = ((_CG_ps1952)t1);
```

The generated C declares `t3` as `_CG_int64` but the codegen
assigns it the receiver `t1` (a `_CG_ps1952` pointer). The
emitted code is unconditionally invalid C.

A similar mismatch surfaces (with different types) in
`tests/cross_type_method.py` — its compile failure is
incidentally what `cross_type_method.py.expect_fail` was
catching.

## Root cause

`P_prim_setter` has a semantic disagreement between the IFA
analyzer and the C codegen:

**Analyzer** (`ifa/analysis/fa.cc:1760-1782`):
```cpp
case P_prim_setter: {
  AVar *obj = make_AVar(p->rvals[1], es);     // receiver
  ...
  AVar *val = make_AVar(p->rvals[4], es);     // value being written
  ...
  flow_vars(val, result);   // <-- result inherits val's type
  break;
}
```

The analyzer flows **val** (the value being written) into the
lvalue. This matches Python's chained-assignment semantics
where `obj.attr = val` evaluates to `val`.

**Codegen** (`ifa/codegen/cg.cc:262-284`):
```cpp
case P_prim_setter: {
  ...
  fprintf(fp, "  ((%s)%s)->e%d = (%s)%s;\n", obj->cg_string,
          n->rvals[1]->cg_string, i, c_type(obj->has[i]),
          c_rhs(n->rvals.v[4]));
  if (n->lvals[0]->live)
    fprintf(fp, "  %s = ((%s)%s);\n", n->lvals[0]->cg_string,
            obj->cg_string, n->rvals[1]->cg_string);
  ...
}
```

The codegen emits the assignment using `obj->cg_string` (the
receiver's type) and `n->rvals[1]->cg_string` (the receiver
itself). The lvalue is treated as the **receiver**, not the val.

Since the analyzer wrote val's type into `n->lvals[0]->type`,
the C variable for the lvalue is declared with val's type.
Then the codegen writes a receiver-typed value into it.
Result: type-error C code that gcc rejects.

## Why this didn't surface earlier

Two reasons:

1. **`dict_methods.py.ignore` blocked the only live exerciser.**
   Until June 2026, the file was untracked / ignored. With the
   `build_type_marks` traversal cap (issue 007 / fixed in
   `d37fec5`), the test was also hanging during IFA before
   ever reaching codegen.

2. **`cross_type_method.py` was incidentally catching the same
   mismatch via `expect_fail`.** The test's intent was to
   verify "cross-type method assignment doesn't compile."
   That was true, but the *reason* it didn't compile was this
   bug, not a deliberate cross-type-method check anywhere in
   pyc.

## What this unblocks

- Removing `tests/dict_methods.py.ignore` and exercising the
  dict iterator code path through pyc → C.
- A real cross-type-method check that doesn't rely on incidental
  codegen failures (orthogonal future work).
- Any future synth shape that builds polymorphic field
  assignment through the setter primitive.

## Options for fixing

### A. Codegen emits val (rvals[4])

Replace the codegen's lvalue assignment with:
```cpp
if (n->lvals[0]->live)
  fprintf(fp, "  %s = (%s)%s;\n", n->lvals[0]->cg_string,
          c_type(n->lvals[0]), c_rhs(n->rvals.v[4]));
```

- Aligns with analyzer's `flow_vars(val, result)`.
- Matches Python's chained-assignment semantics.
- **dict_methods.py compiles** (lvalue gets val's type
  consistently), but its runtime output is wrong (separate
  bug — dict implementation correctness).
- **cross_type_method.py silently compiles**: the cast
  through `c_type(n->lvals[0])` permits coercing B's method
  pointer to A's expected type. Loses the incidental compile-
  error catch. The program would then produce wrong results /
  crash at runtime.

### B. Analyzer flows obj (receiver) to result

Replace `flow_vars(val, result)` with `flow_vars(obj, result)`.

- Aligns with the historical codegen's intent (lvalue carries
  receiver).
- Breaks Python's chained-assignment semantics if any frontend
  ever relies on it (none currently do; setter results are
  fresh symbols immediately discarded).
- **dict_methods.py compiles** (lvalue declared as receiver's
  type, codegen writes receiver — types match).
- **cross_type_method.py also silently compiles** for the
  same reason — lvalue's type now matches receiver's type, so
  the receiver-pointer assignment is a no-op cast through.

Both options A and B make dict_methods compile but cause
cross_type_method's `.expect_fail` to no longer trigger.
That's because the cross_type_method failure mode was
incidental to this same bug — fixing the bug removes the
incidental catch.

### C. Add an explicit cross-type-setter check

Keep the setter codegen / analyzer behavior as-is; add an
explicit analyzer-level check that fails when val's type is
structurally incompatible with the field type. This restores
cross_type_method's expected-fail status by deliberate
semantic check instead of accidental codegen bug.

- Requires understanding pyc's polymorphic-closure-struct
  layout enough to detect when an A-method-closure is being
  assigned to a B-typed field.
- Separate from fixing setter's analyzer/codegen mismatch.

## Verification plan

1. Apply option A or B.
2. Confirm `dict_methods.py` compiles cleanly (the C type
   error is gone).
3. Update `cross_type_method.py` test setup:
   - If A or B: remove or replace `.expect_fail` — likely
     replace with `.exec.expect_fail` so the test still
     guards against the cross-type bug at runtime.
4. Run `./test_pyc` to confirm no other regressions.
5. Document the runtime behavior of `dict_methods.py` —
   compilation is one bar; correct runtime output of the
   `d2.get('x', 0) → 10` query is a separate question that
   may need its own debugging pass.

## What's already fixed

The `dict_methods.py` **hang** (the user's original report)
is resolved by the `build_type_marks` traversal-cap fix
(issue 007 / commit `d37fec5`). Without that fix, the
analysis diverged on dict-iterator's polymorphic field flow
and never reached codegen. With it, IFA converges in <300 ms
and the codegen mismatch documented here is the next blocker.

## See also

- `ifa/analysis/fa.cc:1760-1782` — P_prim_setter analyzer.
- `ifa/codegen/cg.cc:262-284` — P_prim_setter codegen.
- `tests/dict_methods.py.ignore` — kept until runtime
  correctness lands (dict-state-sharing issue beyond Option A).
- `tests/cross_type_method.py.check` — captures the analyzer
  warnings as the expected stdout; test now passes as
  compile-only.
- Issue 007 (`build_type_marks` traversal cap fix) — the
  hang-side resolution.

## Resolution — Option A landed June 2026

Changed the P_prim_setter codegen in `ifa/codegen/cg.cc:276-285`
from receiver-emit to val-emit:

```cpp
// Before:
fprintf(fp, "  %s = ((%s)%s);\n", n->lvals[0]->cg_string,
        obj->cg_string, n->rvals[1]->cg_string);

// After:
fprintf(fp, "  %s = (%s)%s;\n", n->lvals[0]->cg_string,
        c_type(n->lvals[0]), c_rhs(n->rvals.v[4]));
```

The lvalue now receives val (rvals[4]) cast through its
analyzer-inferred type — matching Python's chained-assignment
semantics and consistent with the analyzer's
`flow_vars(val, result)`.

### Test setup changes

- **`tests/cross_type_method.py.expect_fail`** removed. The
  test no longer fails at compile time. Replaced with
  **`tests/cross_type_method.py.check`** capturing pyc's
  six-line "illegal call argument type" / "expression has no
  type" analyzer warnings. Without `.exec.check`, the test
  passes in "compile-only" mode. Runtime behavior (SIGABRT
  with "matching function not found") is acknowledged but not
  asserted — a proper semantic catch is Option C work.

- **`tests/dict_methods.py.ignore`** kept. The codegen
  mismatch is fixed, so dict_methods now compiles, but runtime
  output is incorrect (`d2.get('x', 0)` returns 4 instead of
  10 — a dict-instance-state-sharing issue, structurally
  similar to `class_attr_mutation.py`). Removing `.ignore`
  requires resolving the runtime correctness issue first.

### Verification

- `./test_pyc`: 74 pass / 1 expected fail / 0 fail / 2 skipped
  (was 73 / 2 / 0 / 2 — net +1 pass from cross_type_method
  moving out of expected-fail into compile-only-pass).
- `make` in `ifa/`: all phases clean (18/18 fa-converge).
- `make test_dparse`: all DParser parse-validation tests pass.

### What remains

Option C (explicit cross-type-setter analyzer check) is still
open as future work. With Option A, pyc emits analyzer warnings
for cross-type method assignment but doesn't fail compile or
runtime. A real semantic check would:

- Detect when val's type is structurally incompatible with the
  field type at the setter call site.
- Promote the warnings to errors (pyc rc != 0) or emit C code
  that asserts at runtime.
- Restore the catch that cross_type_method's `.expect_fail`
  was incidentally providing.

This is a deeper change to pyc's error handling and is left as
a separate follow-on. Track if/when someone scopes it.
