# 053 — Unpacking a tuple whose element has heterogeneous own-arity segfaults

**Status:** FIXED 2026-07-19. Root cause: `ifa/codegen/cg.cc`'s
constant-field-getter (inside `P_prim_index_object`) rejected a field
whose *own* type is itself a `Type_SUM` (union) with `->size == 0` —
true even when every member of that union happens to share a common
size (here, uniformly pointer-sized tuple records of differing
`has.n`) — silently skipping the getter emission and leaving the
destination `Var` declared but never assigned (read as uninitialized
stack garbage by whatever consumed it downstream; that's the
segfault). This is the exact same shape `P_prim_sizeof_element` had
already special-cased for a list's element-storage stride (added
during the sibling tuple-list-header fix, same day). Fixed by
extracting that existing uniform-size check into a shared
`resolve_uniform_size(Sym *t)` helper and using it in the getter too
— it now emits the field-access read (with an explicit cast to the
destination's own resolved C type, needed because a `Type_SUM`
field's *nominal* struct-declared type, often `_CG_void*`, can differ
from the destination's independently-resolved type even though both
represent the same pointer-sized value underneath) instead of
skipping the assignment. Verified via
`tests/tuple_unpack_target_arity_union.py` (the exact repro below,
now a committed regression test, passing on both backends) plus a
full `./test_pyc.py` / `PYC_FLAGS=-b ./test_pyc.py` / `ifa`'s
`make test` re-run (zero regressions) and a shedskin corpus sweep
(zero regressions, three examples — `dijkstra2`, `lz2`, `plcfrs` —
newly progress from `PYC_FAIL` to compiling; a fourth, `pygmy`, was
initially misattributed to this fix in an earlier draft of this note
— re-swept against the final, committed state with the issue-055
`set.py` change reverted and confirmed `pygmy` is unaffected by this
fix, still `PYC_FAIL rc=139` exactly as at baseline).
**Caveat:** this does NOT fully unblock `plcfrs.py` itself, but it
does get it further than the note above once implied. See
[../../issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)'s
2026-07-19 entries for the followup: `plcfrs.py` now *compiles*
successfully (category moves `PYC_FAIL` → `RUN_FAIL`) — same
degraded-type diagnostics at line 591 as before (via the
`runtime_errors` NOTYPE-to-void salvage), but codegen now completes
instead of the compiler crashing — and the resulting binary aborts at
runtime on `assert(!"runtime error: matching function not found")`
(a known pattern for salvage-degraded dispatch, consistent with the
still-unresolved line-591 violation). A *separate* attempt to close
that remaining gap (adding `set.__sub__` to support `set(...) -
set(...)`, which `plcfrs.py` uses) was found to independently
segfault/hang the *compiler itself* on `plcfrs.py`'s full complexity
— tracked as [055](055-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md),
reverted rather than shipped.
**Affects:** whatever `emit_assign_to_target`
(`python_ifa_build_if1.cc`)'s tuple-destructuring branch lowers to
(`call_method(..., sym___getitem__, ..., int64_constant(j))` per
target position) combined with FA's handling of a Type_SUM element
whose members are records of differing field count (`t->has.n`) —
root-caused above; fix lives in `ifa/codegen/cg.cc`'s
`resolve_uniform_size()` and the constant-field-getter branch of
`P_prim_index_object`.
**Related:** [044](044-mixed-length-tuple-list-len-miscompile.md),
[the issues/025 tuple-list-header
entry](../../issues/025-shedskin-examples-coverage.md) (this issue's
sibling — same corpus program, same general "heterogeneous tuple
arity" family, different specific code shape).

## Symptom

```python
grammar = [
    (("S", "NP", "VP"), -0.1),
    (("NP", "N"), -0.2),
]
result = [rule for rule, weight in grammar]
print(result)
```

segfaults (confirmed both `pyc` default and `-r`; `-r` reports no
diagnostics before the crash — this is a hard crash, not a rejected
violation). CPython prints `[('S', 'NP', 'VP'), ('NP', 'N')]`.

This is `plcfrs.py`'s actual, still-open line-591 blocker: `rules =
[((tuple(a[:len(a)-2]), ...), float(...)) for a in srules]` builds
`grammar` with `rule` tuples of genuinely varying arity (different
grammar rules have different RHS lengths), and `splitgrammar()`
immediately does `for (rule, yf), weight in grammar for nt in rule`
— the same tuple-unpack-of-a-heterogeneous-arity-element shape.

## What's known (isolated by bisection)

- **Needs the unpack target, not just heterogeneous-arity storage.**
  `[rule for rule in grammar]` (`grammar` a *flat* list of
  heterogeneous-arity tuples, no wrapping/unpacking) works correctly
  today — that's exactly what the issues/025 tuple-list-header fix
  landed. It's specifically `for rule, weight in grammar` — unpacking
  each *wrapped* item `(rule_tuple, weight)` (itself always a uniform
  2-tuple) so that `rule` gets bound to the inner, heterogeneous-arity
  value — that crashes.
- Not about the comprehension: a plain `for` loop with the same
  unpack target reproduces it too (`for rule, weight in grammar:
  result.append(rule)`).
- Not about the second unpacked name: same crash with `weight`
  entirely unused after unpacking.
- The unpack lowers to `item.__getitem__(0)` (`tuple.__getitem__`,
  `__pyc__/04_sequence.py`) to extract `rule` — same
  constant-index `P_prim_index_object` path already exercised
  (successfully) by plain constant tuple indexing elsewhere
  (`ts[0]`/`ts[1]` in the sibling entry's test both return the
  correct value even on file the pre-header-fix binary). So the
  *read* of `rule` itself isn't obviously the break; something after
  that — how the extracted, union-typed value flows into the
  comprehension's accumulation, or how `emit_assign_to_target`
  itself binds it — is more likely.
- Every tuple involved (the `(rule_tuple, weight)` wrapper and the
  `rule_tuple` values themselves) is pointer-represented
  (`_CG_any` = `void*` at the C level, confirmed via
  `list::__setitem__`'s generated signature) — so naively, a union of
  different-arity tuple *pointers* should be exactly as uniform
  (8 bytes) as the already-working list-of-mixed-arity-tuples case.
  Whatever's different hasn't been isolated past this point.

## Repro

```python
grammar = [(("S", "NP", "VP"), -0.1), (("NP", "N"), -0.2)]
result = [rule for rule, weight in grammar]
print(result)
```

`./pyc repro.py && ./repro` segfaults. Removing either the wrapping
tuple (`grammar = [("S","NP","VP"), ("NP","N")]`, no unpack) or the
arity mismatch (`grammar = [(("S","NP"), -0.1), (("NP","N"), -0.2)]`,
same arity) makes it pass.

## Verification plan

**C-backend only** — checked: the exact repro above runs correctly
on the LLVM backend (`pyc -b`), unlike the sibling tuple-list-header
bug (which affected both). That narrows the search to
`ifa/codegen/cg.cc` specifically, likely somewhere in how
`P_prim_index_object`'s constant-field-getter path or the
comprehension-accumulation `list::append`/`__setitem__` path handles
a union-typed extracted value — LLVM's independent, differently-shaped
emission for the same PNode apparently doesn't hit whatever this is.
Fixed when `tests/tuple_arity_union.py`-style coverage can be
extended with this exact unpack shape without crashing on the C
backend. Note this does **not** fully explain `plcfrs.py` itself:
the real program still shows the same `line 591: expression has no
type` violation and still segfaults on *both* backends (checked
2026-07-19) even though this issue's isolated repro only reproduces
on the C backend — so `plcfrs.py`'s full complexity (more grammar
rules, more nesting) hits at least one more, not-yet-isolated
manifestation of the general "heterogeneous tuple arity" family
beyond this specific unpack shape. On the C backend specifically,
`plcfrs.py` did progress from `PYC_FAIL` all the way to `RUN_TIMEOUT`
in the shedskin corpus sweep (30s budget) after the tuple-list-header
fix, i.e. it now compiles and runs without crashing under default
settings (`runtime_errors=true`'s NOTYPE-to-void salvage) — the
crash above only reproduces with `-r` or with a plain, isolated
repro; not yet determined whether that's a real fix under default
settings or just the salvage masking the same underlying gap.

## What this unblocks

`plcfrs.py` (currently the last known blocker on this specific
corpus example) and, more broadly, any program that unpacks a tuple
one of whose elements has non-constant arity — a natural shape
whenever tuple arity encodes real per-record structure (grammar
rules, variable-length records, ...), not just an artifact of a
synthetic repro.
