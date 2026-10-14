# 190 — CODEGEN: run-time dispatch census, toward zero

**Status:** open. Nullable receivers fixed (2026-10-13). What remains is
classified below.

## Goal

**Author's directive, 2026-10-13: minimal run-time dispatch, targeting
zero.** A dispatch that cannot resolve statically is a demand
([146](146-remove-all-arbitrary-splitting.md); `SETTER_OF_SETTER` asks it).
`PYC_DBG_CALLS=1` counts call sites; `=3` names each dynamic one with its
candidates and their source lines.

## Fixed: a nullable receiver is not a dispatch

chull's 15 dynamic calls were all one shape. The receiver is one class or
None: `fv.vertex[i] != e.endpts[0]`, `"%s" % self.vertex[k]`, and
`if e.newface:`. The None is real in CPython. `Face(edge=[...])` takes the
default `vertex=[None, None, None]` and fills it in later, so one object
holds None and then a Vertex over time. No contour split can separate
that, which is why `SETTER_OF_SETTER`'s dispatch demand did not reduce
these calls.

Two defects, both in codegen:

1. **A crash.** `poly_dispatch_classtag_targets` scanned every formal for one
   whose type has the method. `__pyc_None_type__.__eq__(self, x)`'s `self`
   has no `__eq__`, so the scan went on to `x : V` and took the None method
   for a V-tag candidate. No null test was emitted, and `None == v` read a
   class tag through NULL. On a method send it now asks only the receiver,
   the first formal after the selector. `tests/nil_receiver_method_send.py`
   segfaulted before the fix, on both backends.
2. **Not dynamic.** After the null test every remaining receiver calls the
   one non-None candidate. `nil_tested_target` (codegen_common.cc) names
   that pair, and both the C emitter and the census use it. The C arm is now
   a direct call instead of a tag compare and a slot call. The census reports
   `niltest=N` apart from `dynamic`, and only when N > 0. The LLVM emitter
   still tag-compares that arm; it is correct, just not shortened.

chull: `dynamic=15` → `dynamic=0 niltest=15`. Corpus `check -R`
(`check__default__4697b7c4+a77008dd` against `+3981dab9`): verdicts,
warnings and contours identical on all 77.

## Corpus census after the fix (76 programs; othello3 does not compile)

direct 37,236, niltest 130, **dynamic 142** (it was 272 before the fix).

| count | shape | example | what it is |
| --- | --- | --- | --- |
| 40 | user classes, 4+ candidates | pygasus `_exec` over its opcode classes | polymorphism over a hierarchy: hoist (shedskin's virtuals), do not split |
| 35 | user classes, 2–3 candidates | bh `load_tree` / `walk_sub_tree` (Body, Cell), path_tracing `bounce` | same; check each for a common ancestor |
| 28 | None plus one UNTAGGED builtin | amaze `__str__` over {tuple, None} | emitted as a null test plus a direct call already (the untagged-direct route), but `nil_tested_target` requires a classtag, so it is still counted dynamic. Extend the predicate to match the emitter, minding the scalar exclusion (None and 0 share a representation) |
| 20 | two builtins, no None | amaze `__str__` over two tuple shapes | builtin receivers with no tag: a real merge to find |
| 12 | runtime library (`list.remove`, `__contains__`, `index`) | `__eq__` over element types | element unions inside a builtin's contour |
| 7 | `__pyc_unhandled_exception__` | `e.__str__()` over several exception classes and None | one per program that raises two exception classes with their own `__str__` |

## Next

1. Count the 28 None-plus-untagged-builtin calls as `niltest` where the
   emitter already decides them with a null test.
2. Classify the 75 user-class calls: shared ancestor (hoist) or unrelated
   (a precision failure, so split).
