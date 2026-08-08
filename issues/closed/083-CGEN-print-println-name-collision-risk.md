# 083 — `print`/`println` bypass the codegen's generic primitive dispatch (C backend cleaned up; not the pyc-vs-frontend risk originally filed)

**Status: CLOSED (C backend)** — cleaned up 2026-08-07. **Original
premise corrected 2026-08-07** — see below; this was never actually a
pyc-specific bake-in.

### Correction, 2026-08-07

The original filing (preserved unedited further down) assumed `print`/
`println` were **pyc's** names, bypassing the codegen's `__pyc_`
self-namespacing convention. That premise was wrong, found while
designing the fix:

- Python's `print(...)` does **not** reach `P_prim_primitive` as a
  primitive named `"print"` at all. `python_ifa_build_if1.cc`'s
  `f == sym_print` case intercepts it entirely at the **frontend**
  and rewrites it into calls to `sym_write`/`sym_writeln` — two
  ordinary primitives already registered the fully generic way, with
  real pyc-specific codegen: `prim_reg(sym_write->name,
  return_nil_transfer_function, write_codegen)` (`python_ifa_main.cc`).
  `sym_print` itself is purely a frontend marker Sym and never appears
  in a `P_prim_primitive` SEND. So the "renaming `sym_print->name`
  would break Python source resolution via `scope_sym`" concern in the
  original filing below, while technically accurate about how
  `scope_sym`/`sym_print->name` are coupled, was moot — nothing here
  needed `sym_print` renamed in the first place.
- The literal strings `"print"`/`"println"` matched in `cg.cc`/
  `cg_emit_llvm.cc` belong to **ifa's own** `print_symbol`/
  `println_symbol` (`ifa/frontend/ast_to_if1.cc`, `prim_reg`'d with
  `cgfn=0`) — ifa's first-party generic primitives for the **V
  language** (confirmed live: `ifa/tests/for1.v`, `for2.v`,
  `literal.v` all call `print(...)`, compiled via `./ifa`, not `pyc`).
  There was never a second frontend's name colliding with pyc's here —
  ifa legitimately owns these two names for its own language, same as
  it owns `isinstance` as a *concept* before 082 let pyc opt a
  Python-specific meaning into FA's narrowing.

So this was never a pyc-into-ifa bake-in. What *was* real: ifa's own
`print`/`println` registration left `cgfn` unset, forcing both
backends to special-case the name ahead of the generic
`prim_get(name)->cgfn` dispatch every other registered primitive goes
through — an internal consistency gap in ifa's own default, not a
cross-frontend hazard.

### Resolution Summary (C backend)

Gave ifa's own `print`/`println` primitives a real `cgfn`, matching
every other `RegisteredPrim` entry, instead of leaving them as the one
exception special-cased ahead of the generic dispatch:

- Added `print_codegen`/`println_codegen` (`ifa/codegen/cg.cc`, next to
  the existing generic `cg_writeln` helper they both just call) and
  `cg_register_print_cgfns()`, which does `prim_get("print")->cgfn =
  print_codegen` (and `println` likewise) — called once at the top of
  `c_codegen_print_c`, always after `ast_to_if1.cc`'s
  `add_primitive_transfer_functions()` has already registered the
  entries (frontend build always precedes codegen in the pipeline), so
  it supplements rather than races the existing registration.
- Deleted the `strcmp("print", name)`/`strcmp("println", name)`
  special case in `cg.cc`'s `P_prim_primitive` handler — it now goes
  straight to `prim_get(name)->cgfn`, identical to every other
  registered primitive.

**LLVM backend deliberately left as-is.** `RegisteredPrim::llvm_cgfn`
exists as a field but is **dead** — grepped, nothing reads it
anywhere; the comment at its declaration site already says v2 LLVM
dispatches primitives via `lower_send_prim`/`cg_normalize_v2`, not a
per-name table. Confirmed every other `__pyc_*` primitive
(`__pyc_to_str__`, `__pyc_c_call__`, etc.) is *also* handled via its
own explicit `strcmp` branch directly in `cg_emit_llvm.cc`'s
`emit_send_primitive` — there is no generic dispatch mechanism on this
backend for `print`/`println` to fall through to. So, unlike the
original filing assumed, `print`/`println`'s LLVM handling isn't an
outlier at all; it already matches the LLVM backend's own established
(if architecturally different from the C backend's) convention.
Making it generic there would mean building that mechanism from
scratch — out of scope for what turned out to be a non-issue in the
first place.

**Verified:** full clean rebuild (`make clean` + rebuild — a stale
incremental build produces spurious, unrelated-looking crashes in this
repo, see 082's own resolution note) plus:
- Manual repro: `print(1, "two", 3.5, True, False)` /
  `print("no newline", end="")` — byte-identical to CPython, both
  before and after, on the C backend.
- `ifa/tests/for1.v`, `for2.v`, `literal.v` (the actual `print`/
  `println` consumers this issue is about) recompiled and re-run
  directly (`./ifa tests/for1.v && ./tests/for1`, etc.) — output
  unchanged, byte-identical to `.v.check` (modulo a leading test-name
  line some other, undiscovered harness script evidently prepends —
  present in `.check` but not in the raw binary's own stdout either
  before or after this change, so unrelated to it).
- Full `test_pyc.py` both backends: 255/11/0/4, unchanged baseline.
- `ifa --test`: 58/58.
- `ifa`'s own `make test_llvm` (separate from the `.v` files above —
  a dedicated LLVM smoke test): passes, confirming the untouched LLVM
  path is still fine.

---

## Original filing (2026-08-07) — premise corrected above, kept for history

**Affects:** `ifa/codegen/cg.cc` (`P_prim_primitive` case, ~line
896-910) and `ifa/codegen/cg_emit_llvm.cc` (`emit_send_primitive`,
~line 2294).

## What's there

Both backends implement a generic "call a named runtime intrinsic"
dispatch for `P_prim_primitive` SENDs: look up the callee name in the
`RegisteredPrim`/`prim_get`/`prim_reg` table (a genuinely generic,
per-name registration mechanism — any frontend can call `prim_reg(name,
...)` for its own intrinsics; confirmed pyc itself does exactly this
for `print`/`println` in `ast_to_if1.cc`'s
`add_primitive_transfer_functions`), and if no registration matches,
fall back to a generic `_CG_<primname>_<name>(...)` C call convention.
Almost every entry in this cluster self-namespaces with a `__pyc_`
prefix (`__pyc_c_call__`, `__pyc_net_wait_read__`,
`__pyc_net_wait_write__`, `__pyc_sleep__`, `__pyc_format_string__`,
`__pyc_to_str__`, `__pyc_to_bool__`) specifically so an unrelated
frontend's own intrinsic of the same bare name can't collide. Two
names break that convention with bare, plausible-for-any-language
names, hardcoded via `strcmp` ahead of the generic lookup:

```cpp
// cg.cc
if (!strcmp("print", name))
  cg_writeln(fp, n->rvals, 0);
else if (!strcmp("println", name))
  cg_writeln(fp, n->rvals, 1);
```
```cpp
// cg_emit_llvm.cc
if (strcmp(name, "print") == 0 || strcmp(name, "println") == 0) {
  bool do_nl = (strcmp(name, "println") == 0);
  ...
```

A different ifa frontend that also (plausibly) names its own
print-primitive `"print"` would get pyc's hardcoded printf-style
lowering instead of its own registration — silently wrong codegen, not
a compile error.

## Why this isn't a quick rename (unlike 082)

The obvious fix — rename to `__pyc_print__`/`__pyc_println__` and
update the two `strcmp` sites — is **not** as mechanical as it first
looked. `sym_print` (`pyc_symbols.h`'s `B(print)` entry) is created via
`if1_make_symbol(if1, "print")` in `build_builtin_symbols()`
(`python_ifa_sym.cc`), and the SAME `sym_print->name` field ("print")
is what:
1. `cg.cc`/`cg_emit_llvm.cc` match against for codegen (the thing this
   issue is about), **and**
2. gets used as the scope-table lookup key for Python source's `print`
   identifier itself — `scope_sym(ctx, sym_print)`
   (`python_ifa_main.cc`'s `P(_x)` macro expansion) defaults to
   `sym->name` when no explicit override name is given
   (`scope_sym`'s signature, `python_ifa_build_syms.cc:147`, does
   support an explicit third `name` argument for exactly this kind of
   decoupling, but the macro-generated call site doesn't use it today).

So renaming `sym_print`'s `->name` outright would also silently break
every `print(...)` call in every Python program (identifier resolution
failure), since it's the same field doing double duty. A real fix
needs to either (a) pass an explicit `scope_sym(ctx, sym_print,
"print")` override so the *Python-visible* name stays `"print"` while
the *codegen-dispatch* name becomes `"__pyc_print__"` (would need a
one-off change to how `print`/`println` are registered, since they'd
no longer follow the uniform macro-generated pattern every other
builtin symbol uses), or (b) match on something more stable than the
raw name (e.g. compare against the interned `sym_print`/`sym_println`
Sym pointers directly, which `cg.cc`/`cg_emit_llvm.cc` could get via a
new, tiny `IFACallbacks` hook — closer in shape to 082's fix). Given
`print` appears in nearly every test in the suite, whichever approach
is taken needs the full regression sweep, not a spot-check.

## Verification plan

- Whatever fix is chosen, `test_pyc.py` on both backends (255/11/0/4
  baseline) must stay green — `print` is pervasive, so this is the
  highest-blast-radius rename in the whole audit if done carelessly.
- Confirm Python source `print(...)`/`println`-equivalent calls still
  resolve and execute identically before/after.

## What this unblocks

Lets a hypothetical second ifa frontend register its own `print`
intrinsic without pyc's codegen silently intercepting it. Low current
severity (no second frontend actually exercises this collision today
— the V-language test harness doesn't define a same-named primitive),
so this is correctness-in-principle, not an active bug.
