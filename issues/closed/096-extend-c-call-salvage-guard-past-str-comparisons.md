# 096 — Extend the `__pyc_c_call__` salvage guard past the `str`-comparison whitelist (msp_ss.py currently fails to compile)

**Status: FIXED 2026-08-11.** See "RESOLVED" below for the
implementation and full verification; the rest of this file (as
originally filed) is kept as-is for context.

**Original status:** open, found 2026-08-11 as a live compile failure
(`shedskin_examples/msp_ss/msp_ss.py`) while verifying
[issues/041](../../issues/041-stdlib-shim-stubs-silently-wrong.md)'s
`getopt` fix — real `getopt` parsing made a previously-dead-code-
eliminated branch in `msp_ss.py` live for the first time, and it hits
this. Not a new bug: this is
[closed/077](closed/077-primitive-equality-codegen-missing-salvage-guard.md)'s
own documented, deliberately-unfixed remainder, now with a concrete,
currently-failing corpus trigger instead of only a synthetic repro.
Filed as its own issue rather than reopening 077 — that issue's fix
(a narrow `str`-comparison whitelist) is complete and correct for what
it set out to do; this is the *next* scoped piece of the same
convention, not a defect in that one.

**Affects:** `python_ifa_main.cc`'s `c_call_codegen` (the
`__pyc_c_call__` emission) — currently only guards
`_CG_str_eq`/`_CG_str_ne`/`_CG_str_lt`/`_CG_str_le`/`_CG_str_gt`/
`_CG_str_ge`. Every other `__pyc_c_call__` target (`_CG_fopen`,
`_CG_chr`, `_CG_str_to_int64_base`, `_CG_ord`, `_CG_str_from_int`,
`_CG_format_string`, `_CG_list_add`, ...) is unchecked.

**Related:**
[closed/077](closed/077-primitive-equality-codegen-missing-salvage-guard.md)
— the precedent this issue extends; read its "Final design" section
first, especially the two false-positive categories it already found
and worked around (`_CG_any`/void* paired with any pointer type is
never a real mismatch; some sites, e.g. `list.__add__`'s call to
`_CG_list_add`, *deliberately* declare a type that doesn't match what's
passed because the target macro does its own internal conversion —
**a blanket per-argument check is not safe**, which is exactly why 077
shipped a whitelist instead of a general rule).
[056](closed/056-CGEN-degraded-index-type-raw-c-compile-error.md) —
the original "malformed C instead of a guarded degrade" convention
this whole family extends.
[018](../../issues/018-dict-mixed-key-types-boxing-failure.md) — a
different mechanism found the same session (`rdb.py`), not this one;
cross-referenced there for context, not a duplicate.

## Symptom

`shedskin_examples/msp_ss/msp_ss.py` fails to compile:

```
msp_ss.py.c:4763:8: error: no matching function for call to '_CG_fopen'
note: candidate function not viable: cannot convert argument of
incomplete type '_CG_any' (aka 'void *') to 'char *' for 1st argument

msp_ss.py.c:5414:9: error: no matching function for call to '_CG_strcat'

msp_ss.py.c:9386:10: error: no matching function for call to
'_CG_str_to_int64_base'
note: candidate function not viable: cannot convert argument of
incomplete type '_CG_any' ... to 'char *'

msp_ss.py.c:29692:8: error: no matching function for call to '_CG_chr'
note: candidate function not viable: cannot convert argument of
incomplete type '_CG_any' ... to 'int'

msp_ss.py.c:29719:8: error: no matching function for call to '_CG_fopen'
```

Nine errors total, all the same shape: a salvage-degraded `_CG_any`
(pyc's boxed/generic placeholder) argument reaching a `__pyc_c_call__`
target whose C signature declares a concrete type (`char *`, `int`).
A genuine `pyc`-produced C compile error, not a runtime crash — the
same "malformed C instead of guarded degrade" bug class 056/077 are
already the convention for, just at call sites those fixes didn't
cover.

## Root cause

Same as 077's own final analysis: `c_call_codegen`
(`python_ifa_main.cc`) prints `name(args...)` for a `__pyc_c_call__`
send, emitting each argument's `cg_string` verbatim with no check that
the actual resolved type matches the target function's declared C
signature — *except* for the small whitelist 077 added. Somewhere
upstream (not traced — could be the same dispatch-imprecision family
076/077/018 already document, or something specific to how `getopt`'s
newly-real return values flow through `msp_ss.py`'s option-parsing
logic into filenames/format strings), an argument that should resolve
to a concrete `str`/`int` degrades to `_CG_any` before reaching
`_CG_fopen`/`_CG_chr`/`_CG_str_to_int64_base`.

## What a fix would look like

077's own "What a fix would look like" already scoped this precisely
(§1: "Locating every `__pyc_c_call__`-based dunder this affects ...
worth checking whether a single shared guard helper could cover all of
them instead of another one-off fix"). Concretely:

1. A shared guard helper, parametrized by call target, rather than
   077's inline `strict_c_call` whitelist check — the goal is
   *expanding* which names get checked without re-deriving 077's two
   false-positive fixes (`unalias_type()` for `Type_ALIAS` declared
   types like `int`; the `_CG_any`/void* exemption; the "deliberate
   type erasure" exemption for macros like `_CG_list_add` that do
   their own internal conversion) for every newly-added name.
2. For each of `_CG_fopen`/`_CG_chr`/`_CG_str_to_int64_base` (and any
   other target hit by future corpus growth): confirm whether its
   declared argument type is a *real* constraint (like `_CG_str_eq`'s
   `const char *`) or an internal-conversion placeholder (like
   `_CG_list_add`'s `int, l`) before adding it to the checked set —
   077's own false-positive history says this can't be assumed either
   way without checking the specific macro/function.
3. On mismatch: `assert(!"runtime error: ...")` under the default
   permissive mode, `fail(...)` under `--strict` — matching 077's own
   two-tier convention exactly.
4. LLVM backend parity check (077 found the LLVM side was *worse* —
   crashed the compiler itself via an internal LLVM assertion, not
   just bad C — for the analogous binop case; worth checking whether
   `cg_emit_llvm.cc`'s own `__pyc_c_call__` emission has the same
   gap before assuming the C-backend fix alone is sufficient).

## Verification plan

1. `msp_ss.py` compiles clean (or degrades to a runtime guard, not a
   build failure) once the specific call sites this repro hits are
   covered.
2. 077's own verification battery (its repro, `ifa --test`,
   `test_pyc.py` both backends both `PYC_CSM` settings, corpus sweep)
   re-run to confirm no new false positives from the expanded checked
   set — 077's own history (three attempts, two false-positive
   categories found the hard way) says this is the actual risk here,
   not the guard logic itself.
3. Full `shedskin_sweep.sh` — confirm `msp_ss` moves from `FAIL` to
   `COMPILED_C`/`COMPILED_C_WARN` and nothing else regresses.

## What this unblocks

`msp_ss.py` compiling at all (currently a hard build failure, not
reachable for further testing/fixing until this lands). More broadly,
any program whose salvage-degraded value happens to reach one of the
many still-unchecked `__pyc_c_call__` targets — 077's own text lists
`_CG_ord`, `_CG_str_from_int`, `_CG_format_string` as known-unchecked
examples beyond the three this repro actually hits, so this is
plausibly not the last corpus program to trip over this class.

## RESOLVED (2026-08-11)

**Fix implemented, matching this doc's own "What a fix would look
like" section closely:**

1. **Shared guard helper** (design point 1): the mismatch judgment
   itself — `unalias_type()` for `Type_ALIAS` declared types,
   numeric-width/precision tolerance, exact `cg_string` agreement for
   two non-numeric types — was extracted out of `c_call_codegen`'s
   inline block into `c_call_arg_type_mismatch(Sym *declared, Sym
   *actual)` (`ifa/if1/sym.h`/`sym.cc`), so both backends share one
   implementation of 077's two hard-won false-positive fixes instead
   of each re-deriving them.
2. **Target whitelist extended** (design point 2, `python_ifa_main.cc`'s
   `strict_c_call`): added `_CG_fopen`, `_CG_chr`, `_CG_ord`,
   `_CG_str_to_int64_base`, `_CG_strcat` — each individually confirmed
   against `pyc_c_runtime.h` (and, for `_CG_strcat`, its
   `01b_bytes.py` call site) to take its declared argument types as
   real constraints, not `_CG_list_add`-style internal-conversion
   placeholders. `_CG_ord`/`_CG_str_from_int`/`_CG_format_string`'s
   other mentions in this doc turned out to be inaccurate for the
   latter: `_CG_format_string` is emitted by a wholly separate
   primitive (`format_string_codegen`), never reaches
   `c_call_codegen` at all, so there was nothing to add there.
3. **Two-tier degrade** (design point 3): unchanged, already existing
   — `assert(!"runtime error: ...")` under permissive,
   `fail(...)` under strict (`!fruntime_errors`).
4. **LLVM backend parity** (design point 4): `cg_emit_llvm.cc`'s
   `emit_send_primitive` had no equivalent guard at all — worse than
   077 originally found for the binop case, in a different way:
   opaque LLVM pointers make `_CG_any` and a real `str` both just
   `ptr` at the LLVM type level, so `get_runtime_helper`'s
   actual-value-derived `param_tys` would have silently declared a
   signature matching whatever garbage came in, producing a linkable,
   runnable, wrong call rather than a compile error. Added the same
   target whitelist (duplicated per this file's existing convention of
   keeping `__pyc_c_call__` handling in sync rather than shared across
   backends) plus a call to the shared `c_call_arg_type_mismatch`,
   trapping via the existing `emit_salvage_trap` (`llvm.trap`
   intrinsic) on mismatch — unconditional, not gated on
   `fruntime_errors`, matching this backend's other guarded-degrade
   call sites (none of which consult that flag either).
5. **Bonus, found while verifying against the actual repro**: `msp_ss.py`
   also hit `_CG_prim_strcat` (the generic `P_prim_strcat` primitive —
   str's `+` operator, prim_data.cc's uniform 2-string-argument shape)
   and `prim_is_binary_operator` (`cg.cc`, also part of 077's original
   fix, guarding the *generic*-primitive family in parallel with
   `c_call_codegen` guarding the `__pyc_c_call__` family) — this was
   simply missing `P_prim_strcat` from its switch, an oversight rather
   than a deliberate exclusion (its sibling `P_prim_add` etc. are all
   included, and strcat's arg shape is identical). Added.

**What's still open:** `msp_ss.py` does not yet compile clean.
Fixing the above took it from 9 compile errors down to 2, but the
remaining 2 are a structurally different mechanism — a *resolved
call site* whose actual argument type diverges from the *specific
callee clone's* formal parameter type (not a `__pyc_c_call__` or
generic-primitive argument at all) — outside this issue's own stated
scope (`python_ifa_main.cc`'s `c_call_codegen`). Filed separately:
[097](097-CGEN-callsite-vs-clone-formal-type-mismatch.md).

**Verified:**
- `ifa --test`: 58/58.
- `test_pyc.py`, both backends (`PYC_FLAGS=` and `PYC_FLAGS=-b`): 265
  passed / 14 expected-fail / 0 failed / 4 skipped, unchanged from
  baseline.
- `shedskin_sweep.sh`: FAIL set identical before/after (diffed
  directly against a clean pre-change build via `git stash`), zero
  regressions and zero new fails. `msp_ss` stays `FAIL` in this
  coarse classification (it still doesn't fully compile — see above)
  but its underlying diagnostic changed from the original 9-error
  `__pyc_c_call__`/`_CG_any` cascade to 097's 2-error residual.
