# 124 — `--refuse-imprecise`: refuse where inference left a type untyped, instead of emitting codegen's guess

**Status:** open on two small residuals. The option landed 2026-09-01
(off by default). The root cause it found landed 2026-09-02 (nil-only edge
types in the splitter), and `go` then compiled and ran. Rewritten
2026-09-28; history in git:
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

## Residuals

1. **The check has a false positive on nil-only formals.**
   `tests/comprehension_index_untypes_list.py` (`.known_issue`) still
   reports 3 sites. They are `append`/`len` parameters that are provably
   always `None`: precise, merely lacking a C representation. Exempt
   nil-only formals from the check, and the fixture flips to PASS by
   itself.
2. **`go` does not compile at `4b61e721`, and the cause is not
   imprecision.** Its first error is `unresolved member 'rstrip' of class
   'str'` (`go.py:514`, `sys.stdin.readline().rstrip('\n')`):
   `__pyc__/01_str.py` defines `strip` but not `rstrip` / `lstrip`. That
   is a builtin-library gap for the top-level `issues/` tree, not this
   issue. Fix it, then re-measure `go`. If a `Square | UCTNode` union
   reappears, it is an inference merge. Find the merged contour with
   `IFA_DBG_ELEMCONF`. Do not build discriminated field access for a
   union of UNRELATED classes (AGENTS.md: a union pyc invented is pyc's
   bug).

## Verification

`--refuse-imprecise` on the corpus: every reported site is either a real
untyped value or fixed. `go` compiles, runs, and reports no imprecise
sites. The fixture flips to PASS.
