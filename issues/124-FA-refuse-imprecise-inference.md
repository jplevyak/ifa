# 124 — `--refuse-imprecise`: refuse where inference left a type untyped, instead of emitting codegen's guess

**Status:** open on one residual: 6 sites in `go`. Residual 1 is fixed
(2026-10-17, below). The option landed 2026-09-01 (off by default). The root
cause it found landed 2026-09-02, and `go` then compiled and ran. Rewritten
2026-09-28 and 2026-10-17; history in git:
`git show 3f36072b:ifa/issues/124-FA-refuse-imprecise-inference.md`.
It now also owns the remainder of
[123](closed/123-CGEN-union-receiver-field-access-has-no-discrimination.md).

## Why

shedskin compiles `go` with no untyped element list and no `void *` field
access. Where its inference does not resolve, it REFUSES. pyc used to
guess a layout: `node.losses += 1` resolved against `Square`'s layout,
incremented `UCTNode`'s `unexplored` pointer, and crashed. The layout
contract (closed/122, closed/123) now refuses that at compile time. This
option refuses one step earlier, at the cause: an untyped container
element at a constructor that HAS elements, or an untyped function
parameter.

```
PYC_DBG_IMPRECISE=1   report every site, with source location
--refuse-imprecise    make it a compile error (PYC_REFUSE_IMPRECISE=1)
```

## What it found (landed)

`make_AType` projects a lone `{None}` to bottom, and every splitter
comparison is guarded by `->n &&`, so an edge carrying only `None` read as
"no information" and was compatible with everything. The fix is
`split_type_view()`, a splitter-local view that tells "not analyzed" (raw
`out` empty) from "carries only nil". It is used consistently in decide
AND apply. Do NOT change `->type` itself: that regressed narrowing,
defaulted `None` parameters and the recursion gate. The general rule
(closed/127): a value that means both "unknown" and a real answer must
become a sentinel.

*2026-10-02:* `->type` now keeps `None`
([178](178-FA-route4-declines-records-built-through-one-constructor.md)),
after fixing what that exposed. `is_not_none_narrow`, `minmax_3arg` and
`expr_evaluator` all pass. `split_type_view` no longer changed anything
and is removed. Its constant rule (closed/169) survives as `edge_arg_type`.

## Residuals

1. **FIXED 2026-10-17: the nil-only diagnosis was wrong; it was a C naming
   gap.** `tests/comprehension_index_untypes_list.py` reported 3 sites, and
   they were NOT nil-only formals: they were `append`'s `self` and `len`'s
   `x`, typed as a clone of `list` (`list#19102`) whose C type was `_CG_any`.
   Three lines reproduce it: `xs = [1, 2]; xs.append(3); print(len(xs))` was
   refused under `--refuse-imprecise`, and compiled and printed `3` without
   it. The mechanism: `clone.cc`'s primitive branch gives a container group
   with an element its own copy of the builtin's type sym (`sym->copy()`),
   and does so before codegen names the builtins (`S(list)` ->
   `_CG_list`, `cg.cc` `build_type_strings`), so the copy had no C name and
   `assign_type_cg_strings_pass1`'s default gave it `_CG_any`. The analysis
   knew it was a list; the check reads the C type string, so it reported a
   naming artifact as imprecision.

   **Fix:** pass1 maps each named primitive's meta type to its C name, and a
   primitive with no name of its own takes the name of the named primitive of
   the same meta type (a copy keeps its builtin's meta type: same class, same
   representation). The layout contract's two tables (`cg_ptrish_ct`,
   `cg_ctype_width`) did not know `_CG_tuple` / `_CG_vector`, both `void *` in
   both runtimes; without them `genetic2_idioms` failed `'Item' ... member type
   differs (width unknown) (_CG_nil_type vs _CG_tuple)`. Added.

   Measured: the fixture flips to PASS on both backends (sidecar removed);
   `tests/list_literal_append_refuse_imprecise.py` pins the 3-line shape;
   `go` goes from 69 sites to 6; `make test` 488 passed / 14 known / 0 failed;
   corpus `check__default__4d1e5d39+4c13869d` identical to
   `check__default__d84aea97+38ad5631` in all 77 rows.
2. **`go`: 6 sites left, all one cause -- a SETTER split nothing demanded**
   (classified 2026-10-17). `go` compiles, runs and matches CPython; only
   `--refuse-imprecise` refuses it. The sites are the element type of
   `path = [node]` (`go.py:330`), the `select` and `best_child` receivers,
   and `__len__` / `__str__` / `__pyc_to_bool__`.

   - **They are one union.** `UCTNode` has two instance layouts,
     `_CG_ps30288` (the root, built by `UCTTree`'s `UCTNode()`) and
     `_CG_ps30303` (children, built at `go.py:338`). `play` takes the root,
     `score` the child, and `select` / `best_child` / `path`'s element hold
     both, which has no single C type: `_CG_any`.
   - **The two layouts differ in ONE field:** `parent` is `_CG_nil_type` in the
     root (it is only ever `self.parent = None`) and `_CG_any` in the child
     (`child.parent = node`, which is either layout). Merged, `parent` would be
     `{None, UCTNode}`, a nullable pointer: representable, and exactly what
     shedskin emits (one `UCTNode`).
   - **Who split it:** not route 4 (`IFA_DBG_CSDEFSPLIT`: `UCTNode` has one
     creation point, DECLINED every pass) and not a creation point
     (`IFA_DBG_CSMINT` mints only the prototype and cs 1994). A copy
     constructor probe shows cs 2537 copied from 1994 at pass 10 by stage 2,
     **SETTER**: `UCTNode.parent` is a type confluence (`{None}` from
     `__init__`, `{UCTNode}` from `play`), and SETTER splits the CreationSet by
     setter class.
   - **That is a fact, not a demand** (AGENTS.md: "this formal's type is a
     union ... facts"). Nothing observes `{None, UCTNode}` and fails to
     proceed. The split CREATES the unions the check reports.
   - **Probe:** `PYC_NOSETTER=1` (the whole stage off, a probe only) gives
     `go` one `UCTNode` instance layout, 1 imprecise site (a builtin `__str__`
     parameter), and output identical to CPython's.

   **Fix direction (not attempted):** nominate a SETTER confluence only on a
   demand, as SETTER_OF_SETTER already does (ifa/146: `sos_type_conflict` --
   an irrepresentable union, the backward reach of a violation, two receivers
   of a send that dispatches to two functions, or a CreationSet carrying one
   of these). The narrowest first step: a confluence whose only addition is
   `None` joining pointer-shaped types is representable and is not one. That
   is a change to IFA's core splitting trigger, so it needs a same-binary
   corpus `check` A/B; 146 records that the weaker SoS gates lost dijkstra2,
   plcfrs, pylife and sunfish, so the dispatch clause matters.
3. **Consider basing the check on FA types**, not on C type strings, so a
   codegen naming gap cannot read as imprecision again.

## Verification

`--refuse-imprecise` on the corpus: every reported site is either a real
untyped value or fixed. `go` compiles, runs, and reports no imprecise
sites. The fixture flips to PASS.
