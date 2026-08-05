# 077 — Dunder-dispatched comparison codegen assumes both operands share the dispatch-selected type; no guard when they don't

**Status:** open, found 2026-08-04 alongside
[076](076-mutation-driven-receiver-divergence-not-cloned.md) (same
repro exposes both — 076 is the precision root cause that *produces*
a type-mismatched comparison; this issue is the separate codegen gap
that turns it into a **hard C compile error** instead of the
established "degrade to a runtime assert" convention). Not fixed —
filed rather than fixed because, like
[056](056-degraded-index-type-raw-c-compile-error.md) (the same bug
class at a different call site), the two fix sites below are shared,
hot codegen paths (every `str` comparison; every numeric-dunder-style
primitive operator, program-wide) and a correct fix needs both
located call sites covered plus the LLVM backend checked for parity,
with full regression verification — more than a quick patch.
**Affects:** `python_ifa_main.cc:58-83` (`c_call_codegen`, the
`__pyc_c_call__` emission shared by every `str` comparison dunder —
see below) and `ifa/codegen/cg.cc:857-873` (`case P_prim_primitive`'s
generic fallback, used when `__pyc_operator__` names a primitive with
no `RegisteredPrim::cgfn` — this is the `_CG_prim_equal` call site).
**Related:** [076](076-mutation-driven-receiver-divergence-not-cloned.md)
(root cause of *why* the operand types mismatch in the first place),
[056](056-degraded-index-type-raw-c-compile-error.md) (the established
precedent and convention this issue extends to a new call site — read
its "What a fix would look like" section, the same shape applies
here).

## Symptom

Reusing 076's repro (`squares = {1: 1, 2: 4, 3: 9}; words = {"a": 1,
"b": 2}`), the generated `.c` fails to compile:

```
1.py.c:114:8: error: comparison between pointer and integer ('_CG_int64' (aka 'long long') and 'char *')
  114 |   t1 = _CG_prim_equal(t2, _CG_Symbol(6488, "=="), _CG_String_n("b",1));
1.py.c:310:8: error: no matching function for call to '_CG_str_eq'
  310 |   t1 = _CG_str_eq(t2, t3);
      |        ^~~~~~~~~~
././pyc_c_runtime.h:1291:17: note: candidate function not viable: no known conversion from '_CG_int64' (aka 'long long') to 'const char *' for 2nd argument
```

This is a genuine `pyc`-produced C compile error, not a runtime crash
— exactly the bug class 056 already documents and names the
convention for: "every salvage-reachable codegen site should degrade
to a runtime `assert(!"runtime error: ...")` guard ... never a raw,
unsalvageable C compile error." This issue is a second instance of
that same gap, at the comparison-dunder call sites instead of 056's
index-object call sites.

## Root cause

`dict.__setitem__`'s `self._keys[i] == key` is dynamic dispatch:
pyc's classtag dispatch picks *which* `__eq__` body to run based on
the (possibly union/imprecise) resolved type of the **left** operand,
`self._keys[i]`, alone. Once a specific `__eq__` is picked, its body
assumes — with no further check — that the **right** operand shares
that same type:

- `str.__eq__` (`__pyc__/01_str.py:56-57`):
  `return __pyc_c_call__(bool, "_CG_str_eq", str, self, str, x)`
  — declares both `self` and `x` `str`-typed to the C-call wrapper.
  Codegen (`c_call_codegen`, `python_ifa_main.cc:58-83`) just prints
  `name(args...)` — walking `n->rvals[5, 7, 9, ...]` and emitting each
  `cg_string` verbatim (line 80) — with **no check** that the actual
  resolved C type of each argument matches the declared `const char*`
  signature of `_CG_str_eq`.

- Numeric `__eq__` (`__pyc__/02_numeric.py:85-86`, mirrored for other
  numeric-like dunders at `:87-96` and again at `:165` for the second
  numeric-like class in that file):
  `return __pyc_operator__(clone(self), __pyc_symbol__("=="), clone(x))`
  — lowers to the C `==` operator directly via the `_CG_prim_equal`
  macro (`pyc_c_runtime.h:1284`: `#define _CG_prim_equal(_a, _op, _b)
  ((_a) == (_b))`). This call is emitted by `cg.cc`'s generic
  `P_prim_primitive` fallback (~857-873: `prim_get(name)` finds no
  registered `cgfn` for `__pyc_operator__`'s target, so it falls to
  `fprintf(fp, "_CG_%s_%s(", n->prim->name, name)` and prints each
  argument's `cg_string` verbatim, again with no type check).

Both sites are, individually, perfectly reasonable when the dispatch
that selected them was precise (the common case — this is exactly why
neither has ever needed a guard before). The gap only manifests when
something upstream *has already* let the two operands' types diverge
from what the picked dunder assumes — which 076 shows happens for any
class without per-instance method specialization once its instances
genuinely diverge in type. `Box` (076's user-class repro) hits the
identical dispatch mismatch but happens to route through a *different*
codegen path (generic method dispatch, which already has a
`assert(!"runtime error: matching function not found")` fallback) —
that's the existing precedent this issue proposes extending to these
two sites.

## What a fix would look like

Mirror 056's proposed shape at both sites: before emitting the
verbatim call, check each argument's resolved C type actually matches
what the target function/macro declares (`const char *` for
`_CG_str_eq` and friends; a scalar/primitive type for `_CG_prim_equal`
and friends — the pattern already used elsewhere in `cg.cc`, e.g. the
`== sym_int64` check style at line 99, and 056's own writeup of the
`Type_RECORD`/`!t->has.n` up-front special case). If a mismatch is
detected, emit `assert(!"runtime error: ...")` instead of the raw
call. Needs:

1. Locating every `__pyc_c_call__`-based dunder this affects (056's
   writeup notes the same class of gap was independently found and
   fixed twice already at other call sites — `resolve_uniform_size`
   in issue 053, and "minpng's/plcfrs's C-compile-error bugs" in issue
   025 — so this is at least the fourth occurrence of the same
   pattern; worth checking whether a single shared guard helper could
   cover all of them instead of another one-off fix).
2. Checking the LLVM backend's independent emission
   (`cg_emit_llvm.cc:1102`'s `case P_prim_equal` — note this suggests
   the LLVM backend *does* special-case equality by enum kind, unlike
   the C backend's name-string dispatch; the two backends may already
   have drifted on this exact guard, matching 056's own observation
   that they "drift on exactly this kind of guard if not deliberately
   kept in sync").

## Verification plan

1. 076's `1.py` repro: after the fix, should either compile clean (if
   076 is fixed first / together) or degrade to a warning + runtime
   guard (matching `Box`'s current behavior) rather than a hard
   compile error.
2. Construct a minimal repro that hits *only* this issue without 076
   (any existing salvage-to-imprecise-type path that reaches a
   comparison dunder — e.g. adapt one of 056's/053's/025's existing
   repros if they compose) to verify the guard fires independent of
   076's specific mechanism.
3. Full `test_pyc.py`, both backends — this touches every `str`
   comparison and every numeric-dunder operator call in the corpus,
   so needs the same full-suite + full-corpus-sweep rigor as
   issue 056's own (not-yet-executed) verification plan.

## What this unblocks

Currently, any program where dispatch imprecision (076's mechanism,
or any other future/existing salvage path that reaches these two
call sites) lets a comparison's operand types diverge fails the BUILD
with a raw C error instead of degrading to a runtime guard like every
other salvage-reachable site in this codebase is supposed to
(established convention, per 056). Fixing this doesn't fix the
underlying imprecision (076's job) but does make the failure mode
consistent with the rest of the codebase and turns a hard build
failure into a compile-clean-but-may-runtime-assert program, matching
`runtime_errors`' documented semantics.
