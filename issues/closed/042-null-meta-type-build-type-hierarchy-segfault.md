# Issue 042: `build_type_hierarchy` segfaults on a type Sym with a
# null `meta_type` (rdb.py and msp_ss.py, newly reachable via
# issues/025's os/fnmatch/io shims)

**Status: CLOSED** — RESOLVED 2026-07-14. Root-caused (not the pass-ordering
theory below — see resolution) and fixed; rdb, msp_ss, amaze,
tictactoe, and voronoi2 all cleared the segfault together.

## Resolution (2026-07-14)

Debugger inspection at the crash (tictactoe): the meta_type-less Sym
was `"Exception"` — `type_kind == Type_NONE`, not a class at all —
with implementor `SpaceNotEmpty`. Every crashing example inherits
from `Exception` somewhere (amaze: `MazeReaderException`/`MazeError`;
tictactoe: `SpaceNotEmpty`/`MultiVictory`; msp_ss: `BSLException`;
voronoi2: via `pyc_lib/getopt.py`'s `GetoptError`; rdb likewise via
shims), and **pyc's builtins never defined an `Exception` class**.
`class X(Exception)` therefore resolved the base to a plain
undefined-global Sym; `inherits_add` wired that non-type Sym into
the implements/specializes graph; `build_type_hierarchy` then pulled
it into `types.asvec` (line 421's `implement(ss, s, types)`), and —
since `set_type_and_meta_type` only makes meta types for `type_kind`
Syms — line 448's `implement(s->meta_type /* null */, ...)` crashed.
NOT an incremental-window/ordering bug: `finalized_types` covered
the Sym fine; it was skipped because it isn't a type.

Two-part fix:
1. **`__pyc__/08_exception.py`** (new): a real builtin `Exception`
   class (derives `object` explicitly; `args` string field;
   `__init__`/`__str__`), so exception classes are ordinary classes.
   pyc still has no exception *control flow* (issue 011).
2. **`python_ifa_build_syms.cc`** (`PY_classdef`): a base that is
   not a class (`!type_kind && !is_constant`) now `fail()`s with a
   clean "base 'X' of class 'Y' is not a class" error instead of
   planting a hierarchy landmine — any future instance of this shape
   becomes a readable compile error, not a SIGSEGV.

The original analysis below is kept for the record; its
"pass-ordering window" hypothesis was plausible but wrong.

## Symptom

```
Program received signal SIGSEGV, Segmentation fault.
Vec<Sym*, DefaultAlloc, 2>::set_add (this=0x2e8, a=0x...) at ifa/common/vec.h:262
#1  implement (s=0x0, ss=0x..., types=...) at if1/ast.cc:258
#2  build_type_hierarchy (compute_structural_value_hierarchy=0) at if1/ast.cc:448
#3  ast_to_if1_extend (...) at python_ifa_main.cc:289
```

`this=0x2e8` is `NULL + offsetof(Sym, implementors)` — `implement()`
was called with `s == NULL`. Line 448 is:

```cpp
for (Sym *s : types.asvec) if (!s->is_meta_type) {
  for (Sym *ss : s->implementors) if (ss && s->meta_type != ss->meta_type)
      implement(s->meta_type, ss->meta_type, meta_types);
  ...
```

`s->meta_type` is null for some `s` in `types.asvec`, and the guard
only checks `ss` for null, not `s->meta_type`. Since `s->meta_type !=
ss->meta_type` is true whenever `s->meta_type` is null (null !=
anything non-null), the branch is taken and `implement(NULL, ...)`
crashes.

## Where to look

`meta_type` is assigned by `make_meta_type` (`if1/ast.cc:558`),
called from `set_type_and_meta_type` (`if1/ast.cc:572`, guarded `if
(!s->meta_type) make_meta_type(s)`) over `allsyms[finalized_types
.. ]`. `build_type_hierarchy` walks a *different* incremental
window, `allsyms[type_hierarchy_built .. ]`. If some pass ordering
lets a type-kind Sym reach `build_type_hierarchy`'s `types.asvec`
before `set_type_and_meta_type` has run over it (or before the
`finalized_types` cursor covers it), it arrives with `meta_type ==
NULL` and this line crashes. Reproducing with a smaller input and
instrumenting `set_type_and_meta_type` vs. `build_type_hierarchy`
call order (both invoked from `ast_to_if1_extend`,
`python_ifa_main.cc`) should show the interleaving that skips it.

A one-line defensive null-check (`s->meta_type &&
s->meta_type != ss->meta_type`) would stop the crash but silently
drops a hierarchy edge instead of fixing the ordering bug — masking,
not fixing; do not apply without understanding why `meta_type` is
missing.

## Verification plan

Repro: `pyc -D <repo root> shedskin_examples/rdb/rdb.py` (crashes
100% of the time, not flaky like issue 041). Once the pass-ordering
gap is found, re-run and confirm no crash; then re-run
`./shedskin_sweep.sh` to see whether any of the other `signal 117`
entries (amaze, othello2, othello3, pystone, score4, tictactoe,
voronoi2) share the same root cause and clear at the same time.

## Impact

Blocks `rdb` from the shedskin corpus (issues/025) even after its
`os`/`os.path`/`fnmatch` module gaps are filled. Also the likely
common cause behind several other corpus crashes, so fixing it may
have a multi-example payoff.
