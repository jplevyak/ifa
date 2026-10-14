# 190 — CODEGEN: run-time dispatch census, toward zero

**Status:** open. Nullable receivers fixed, both tagged and untagged
(2026-10-13). What remains is classified below.

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

## Fixed: None plus an untagged builtin

28 more call sites in user code were None plus a `str`, `bytes`, `list`,
`tuple` or function value. The emitter already decided each of these with
a null test and a direct call, through its untagged-direct route or the
arity plan, but `nil_tested_target` required a classtag, so the census
counted them as dynamic. Counting with the same predicate in the runtime
library added 19 more. The predicate now accepts any receiver whose
members are all pointers: all tagged or all untagged, and none `None` or
a number. A scalar shares 0 with None, so a null test cannot decide it.

A temporary probe in the C emitter checked that every site the census
calls `niltest` is emitted as a null test plus one direct call. It found no
mismatches on 76 programs. One shape reaches that result by another
route: `{2-tuple, None}` goes through the arity plan, which keeps its
`len == 2` check after the null test. That check is a guard, not a choice
between functions, because only one candidate is left.

This changed only the census: emission is unchanged and no golden moved.

## Corpus census (76 programs; othello3 does not compile)

direct 37,236, **niltest 177, dynamic 95**. Before this issue: niltest 0,
dynamic 272.

| shape | example | what it is |
| --- | --- | --- |
| user classes, 4+ candidates (40) | pygasus `_exec` over its opcode classes | polymorphism over a hierarchy: hoist (shedskin's virtuals), do not split |
| user classes, 2–3 candidates (~35) | bh `load_tree` / `walk_sub_tree` (Body, Cell), path_tracing `bounce` | same; check each for a common ancestor |
| two builtin shapes, no None (~20) | amaze `__str__` over two tuple shapes | builtin receivers with no tag: a real merge to find |
| runtime library (`list.remove`, `__contains__`, `index`) | `__eq__` over element types | element unions inside a builtin's contour |
| `__pyc_unhandled_exception__` with several `__str__` overrides | `e.__str__()` | one per program raising two exception classes with their own `__str__` |
| None plus two or more types | amaze `__eq__` over {tuple, tuple union, None} | the non-None part is itself a dispatch |

## Next

Classify the user-class calls: shared ancestor (hoist) or unrelated (a
precision failure, so split).
