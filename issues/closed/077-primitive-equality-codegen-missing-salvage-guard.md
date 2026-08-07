# 077 — Dunder-dispatched comparison codegen assumes both operands share the dispatch-selected type; no guard when they don't

**Status: FIXED 2026-08-05.** Both call sites the issue named are now
guarded: the `_CG_prim_equal`-family (numeric dunders, `cg.cc`'s
`emit_send_default_prim`, C backend, plus a parity fix for the LLVM
backend, which turned out to have a *worse* version of this bug —
crashes the compiler itself via an LLVM internal assertion, not just
a raw C error) and the `_CG_str_eq`-family (`c_call_codegen`,
`python_ifa_main.cc`) — the latter took three attempts across two
sessions before landing safely; see "RESOLVED" below for the full
trace, including why it's deliberately scoped to a small whitelist of
call targets rather than every `__pyc_c_call__` site. Originally found
2026-08-04 alongside
[076](076-mutation-driven-receiver-divergence-not-cloned.md) (same
repro exposes both — 076 is the precision root cause that *produces*
a type-mismatched comparison; this issue is the separate codegen gap
that turns it into a **hard C compile error** instead of the
established "degrade to a runtime assert" convention). **076 is now
fixed (2026-08-05)** — the specific `1.py` repro that motivated this
issue no longer reaches a type mismatch at all, so it no longer
exercises this gap either. That doesn't close this issue: the missing
guard is general (any other salvage path reaching these two call
sites hits the same hard C error), just no longer demonstrated by the
original repro. Not fixed — filed rather than fixed because, like
[056](056-CGEN-degraded-index-type-raw-c-compile-error.md) (the same bug
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
(root cause of *why* the operand types mismatch in the first place —
now fixed, see that file's "RESOLVED" section),
[056](056-CGEN-degraded-index-type-raw-c-compile-error.md) (the established
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

## RESOLVED, 2026-08-05

**What actually reaches `_CG_prim_equal`.** The original "Root cause"
section above attributes this to `cg.cc`'s `case P_prim_primitive`
generic fallback (~857-873) — that turned out to be wrong (or stale;
unclear which). Traced empirically (added a debug print at that
branch's entry, ran the repro, `name` was never `"equal"` — only
`__pyc_c_call__`/`write`/`writeln` reached it). `P_prim_equal` is its
own `prim_data.h` index (15), entirely separate from
`P_prim_primitive` (34). The real call chain: `virtual_cg_emit_send`
(`codegen_common.cc:611`) tries a fixed sequence of emitter hooks per
send (`emit_send_any_prim`, `..._unaryop`, `..._binop`, `..._period`,
...) and falls through to `emit_send_default_prim` — a **pure
virtual** every backend must implement — when none claim it. The C
backend doesn't override `emit_send_binop` (base default: `return
false`), so `P_prim_equal` and every sibling binary op falls all the
way through to `emit_send_default_prim` (`cg.cc:1207`, not 857-873),
which is where `fprintf(fp, "_CG_%s(", pn->prim->name)` actually
lives — `pn->prim->name` is already the full `"prim_equal"` etc.,
not assembled from a separate operation-name argument the way
`P_prim_primitive`'s OWN fallback works. Recorded here so the next
person doesn't re-walk this the same way.

**C backend fix.** `emit_send_default_prim` (`cg.cc:1243`, shifted
from 1207 by this fix's own added code) now checks, for the binary
arithmetic/comparison family specifically (`prim_is_binary_operator`,
a new helper listing exactly the `prim_data.h` indices with the
`(_a, _op, _b)`-shaped macros in `pyc_c_runtime.h` — deliberately
**not** every index reaching this fallback, since others like
`isinstance`/`issubclass` have their own special-cased, legitimately
heterogeneous argument shape right below), whether all non-operator-
symbol arguments (`->sym->is_symbol` excludes the operator-symbol
constant itself, e.g. `__pyc_symbol__("==")`'s value) share one
`c_type()`. On mismatch: `fail(...)` when `fruntime_errors` is off
(pyc's own default is **on** — `defs.h`: `EXTERN_INIT(true)` — so
this path is mostly theoretical for `pyc` itself, but matches the
two-tier convention used everywhere else in `cg.cc`), else emit
`assert(!"runtime error: primitive operand type mismatch")`.

**LLVM backend: found a worse bug while checking for parity, fixed
it too.** The issue's own "What a fix would look like" flagged this
as worth checking. It was worse than expected: `cg_emit_llvm.cc`'s
`emit_send_binop` (its own override, the LLVM backend's equivalent of
the C backend's fallback) already *does* coerce operand types before
building the LLVM instruction — but only for the legitimate int↔float
/ int-width-mismatch cases. A pointer-vs-scalar mismatch (this
issue's actual repro shape) hits none of those coercion branches and
falls straight into `Builder->CreateICmpEQ(lhs, rhs)` with genuinely
different LLVM types — which LLVM's own `IRBuilder` asserts on
internally. Confirmed directly: `llvm::ICmpInst::AssertOK(): Assertion
'getOperand(0)->getType() == getOperand(1)->getType() ...' failed`,
**crashing the pyc process itself** (`Aborted (core dumped)`) rather
than just producing a bad C file — a strictly worse failure mode than
the one this issue set out to fix. Fixed by adding a same-type check
before the switch that builds the LLVM instruction, calling
`codegen_fail(pn, ...)` (a clean, `noreturn`, location-aware compiler
error — already used one function up, `emit_send_unaryop`'s
"unsupported operand type" case) instead of proceeding. No LLVM-
backend runtime-trap mechanism (an LLVM IR sequence that aborts at
*program* runtime, mirroring the C backend's `assert(!"...")`) exists
anywhere in this codebase yet — building one is real, standalone
design work, not attempted here. So LLVM parity is: crash → clean
compile-time failure (a real, verified improvement), not yet crash →
runtime-degrade (full parity with the C backend's new behavior).
**First implementation of this LLVM fix had a bug**, caught by full
verification, not by inspection: the same-type check ran
unconditionally, before confirming `pn->prim->index` was even one of
the binary-op family `emit_send_binop` is responsible for — since
this function is invoked for *every* prim send (`P_prim_primitive`
included, whose "operands" aren't a same-typed pair by design), it
false-positived on ~200 corpus programs (`test_pyc.py` dropped from
239 passed to 42). Fixed by adding an explicit `is_binop_family(idx)`
membership check (mirroring the C backend's `prim_is_binary_operator`)
as an early-return before any type inspection at all.

**`_CG_str_eq`/`c_call_codegen`: three attempts (two reverted in the
first session, a third landed in a follow-up session).** This call
site's convention is different from the numeric family above —
`__pyc_c_call__(ret_type, name, type1, arg1, type2, arg2, ...)`
declares each argument's expected type *explicitly*, as a
compile-time meta-type-constant argument, rather than requiring
peer-argument agreement. Three strategies for checking "does argument
i actually match its declared type_i" were tried:

1. Compare `declared->cg_string` (the declared-type argument's own
   Sym, unwrapped via `->sym->meta_type` the same way
   `c_call_transfer_function` right above already does for the
   *return*-type position) against `actual->cg_string` directly.
   False-positived immediately (`test_pyc.py`: 239 → 238, one
   failure) — traced with a debug print to `ord(x)`
   (`__pyc__/05_builtins.py:116`:
   `__pyc_c_call__(int, "_CG_ord", str, x)`): declared `str`'s own
   `cg_string` is the generic placeholder `_CG_any` (it's the
   abstract class, never itself laid out concretely), while `x`'s
   *actual* resolved type is some concrete `str` specialization with
   `cg_string` `_CG_string` — legitimately the same type, differently
   represented, not a real mismatch.
2. Switched to `declared->specializers.set_in(actual)` — the same
   membership-check idiom already established throughout `cg.cc`
   (e.g. `sym_string->specializers.set_in(t)`) for exactly "is t a
   variant of X". **Also** false-positived, far worse (239 → 62
   passed, 188 failures) — traced to `int.__str__`
   (`__pyc__/02_numeric.py:111`:
   `__pyc_c_call__(str, "_CG_str_from_int", int, ...)`).

Both were reverted; the session ended there with the finding recorded
for whoever picked it up next. That turned out to be the same
investigation, continued: the fix for attempt 2's exact failure was
`unalias_type()` (`ifa/if1/sym.cc`) — **not previously used at this
call site**. `int` (unlike `str`) is a `Type_ALIAS` Sym (Python's
arbitrary-precision int aliased to pyc's fixed-width `int64`), not
itself a concrete specialization at all, so neither `cg_string` nor
`specializers` comparisons against the raw declared Sym could ever
have worked for it — `unalias_type(int)` resolves to a Sym that
matches a real `int64` value's `->type` *exactly* (name, `type_kind`,
`num_kind`, `num_index`, `cg_string` all identical, confirmed via
debug print). Rebuilding the check around `unalias_type()` plus a
`num_kind`-based tolerance (compatible if BOTH sides are numeric,
*regardless* of exact width/precision — two scalars are always safely
C-castable, mirroring how `cg_emit_llvm.cc`'s `emit_send_binop`
already treats int↔float and int-width differences as coercible, not
an error) got the corpus from 188 failures down to 14, uncovering two
more real false-positive categories along the way:
- **`_CG_any`** (pyc's boxed/generic placeholder, a `void*`) paired
  with any other non-numeric type — C freely, implicitly converts
  `void*` to/from any object pointer type, so this is never a real
  mismatch. Hit via `__pyc__/04_sequence.py`'s `merge`/`merge_in`
  family (`list.__add__` etc.) — a `list`-declared argument can
  resolve to a boxed `_CG_any` element type at a real call site.
- **Deliberate type erasure.** `list.__add__`'s own `__pyc_c_call__`
  to `_CG_list_add` declares `int, l` for `l` — an entire *list*
  operand labeled `int` — because the underlying macro
  (`_CG_list_add(_l1, _l2, _s1, _s2)` in `pyc_c_runtime.h`) runs both
  sides through `_CG_to_list(...)` regardless of the nominal declared
  type. This is the load-bearing finding: **a per-argument type check
  cannot be a blanket rule across every `__pyc_c_call__` site**, full
  stop — some sites declare a type that doesn't match what's actually
  passed *on purpose*, relying on the target macro's own internal
  conversion, and nothing in the IR distinguishes "this declared type
  is a real constraint" from "this declared type is a placeholder."

**Final design: an explicit whitelist, not a general rule.** Given
that finding, checking every `__pyc_c_call__` site was abandoned in
favor of only checking the specific target names issue 077 actually
named — `str`'s comparison family (`_CG_str_eq`, `_CG_str_ne`,
`_CG_str_lt`, `_CG_str_le`, `_CG_str_gt`, `_CG_str_ge`,
`__pyc__/01_str.py`), all of which declare their argument types
literally (real `const char *` C parameters, no internal-conversion
macro) and are the actual repro shape this issue documents. Every
other `__pyc_c_call__` site (`_CG_list_add`, `_CG_ord`,
`_CG_str_from_int`, `_CG_format_string`, ...) is completely unchecked
and unaffected — matches current, pre-issue-077 behavior exactly, by
construction (the check is gated on `strict_c_call`, computed once
from `name` before the loop even starts).

**Verified** (now covering both call sites — the numeric family from
the prior session's fix, plus this session's `_CG_str_eq` family):
- `ifa --test`: 58/58.
- `test_pyc.py`, C and LLVM backends, `PYC_CSM` unset: 239/11/0/4
  both — unchanged from baseline.
- `test_pyc.py`, C and LLVM backends, `PYC_CSM=2`: 235/11/4/4 both —
  unchanged from the post-issue-078 baseline (same 4 failing tests).
- `shedskin_sweep.sh`, both `PYC_CSM` settings: byte-identical
  (`diff` against the prior session's post-numeric-fix `results.tsv`)
  — the corpus doesn't happen to exercise this exact `str`-comparison
  mismatch shape, so no further corpus-level change, but critically
  zero NEW regressions either.
- This issue's own repro (the conditional-`__init__` `MiniDict`
  variant, reproducing 076's mechanism independent of both 076 and
  078 being otherwise fixed): **both** the `_CG_prim_equal` and
  `_CG_str_eq` occurrences now degrade to
  `assert(!"runtime error: ...")` instead of a raw compile error;
  compiles clean, runs, and aborts cleanly at a runtime assert.
