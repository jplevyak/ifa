# Testability Refactorings

Cleanups to the IFA codebase required (or strongly enabled) by the
testing plan. Each item names: what to change, why testing wants it,
what risk to existing pyc/V end-to-end behavior.

See [TESTING.md](../TESTING.md) for context.

Order is by independence — items low in the list build on items
above. Each can be a separate PR.

---

## 1. Per-`FA` worklists (high blocker; easy fix)

**Current state.** `ifa/analysis/fa.cc` declares:

```c
static Que(AEdge,    edge_worklist_link)  edge_worklist;
static Que(AVar,     send_worklist_link)  send_worklist;
static Que(EntrySet, es_worklist_link)    es_worklist;
static Vec<EntrySet *>          entry_set_done;
static Vec<ATypeViolation *>    type_violations;
static ChainHash<AType *, ...>  cannonical_atypes;
// ... others
```

These are file-scope statics. They persist across `FA::analyze` calls.

**Why testing wants it.** The test runner instantiates a fresh `FA`
per fixture. Statics carry stale worklist items and canonicalized
ATypes from the previous test, causing nondeterminism.

**Change.** Move the worklists and other per-analysis caches into
`FA` member fields. The canonicalization tables (`cannonical_atypes`,
`cannonical_setters`, ...) move into `IF1` (since they're per-IF1
instance, conceptually).

**Risk.** Minimal. The statics are accessed only via the
analysis loop and helpers in fa.cc. A search-and-replace pass plus
a few `fa->` prefix changes. The pyc / V end-to-end runs stay
single-FA, so behavior is unchanged.

**Acceptance.** `FA *a = new FA(...); a->analyze(...); delete a;
FA *b = new FA(...); b->analyze(...);` produces deterministic output
for `b` regardless of `a`.

---

## 2. Singleton reset

**Current state.** `IF1 *if1`, `PDB *pdb`, `FA *fa` are global
singletons. `ifa_init` does `new IF1` etc. — re-init is not
supported.

**Why testing wants it.** Each fixture needs fresh state. With
singletons, the test runner has to either (a) `exec` a fresh process
per fixture (slow), or (b) wipe singletons in-place.

**Change.** Two options, pick one:

**(a) Reset helper.** Add `ifa_reset()` that:
- Discards the current `IF1`, `PDB`, `FA`.
- Clears all module-level statics (canonicalization tables,
  worklists, `analysis_pass`, `finalized_aspect`, etc.).
- Resets ID counters (`avar_id`, `aedge_id`, ...) to 1.

Conservative; pyc/V drivers unaffected (they never call reset).

**(b) Full per-context.** Replace `if1` / `pdb` / `fa` globals with
a `Compiler *` context passed everywhere. Big change; touches every
file that references the globals (~40 files).

**Recommended:** (a) for now. Revisit (b) if multi-threaded compilation
ever lands (also makes parallel IFA on multiple modules viable).

**Acceptance.** `ifa_reset()` followed by `ifa_init(...)` produces
a clean state. Two fixtures run back-to-back produce identical IDs
for identical inputs.

---

## 3. Deterministic ID assignment

**Current state.** `Sym::id`, `PNode::id`, `AVar::id`, etc. are
assigned from per-class counters in registration order. Order
depends on input parsing and frontend traversal.

**Why testing wants it.** Tests use names, not IDs. But IDs leak
through `print_syms` (the `if1_write` output) and a few error
messages. We want IDs to be a stable function of the test, not
of execution history.

**Change.** Two parts:
- Always print Syms by name (auto-naming if no user name); never
  emit raw `id` in test-facing output.
- Reset all ID counters in `ifa_reset()` (see §2).

**Risk.** Low. The pyc/V drivers don't depend on absolute ID values,
only on per-run consistency.

**Acceptance.** Same `.ir` input → same per-Sym names in output
across runs. Reordering unrelated tests doesn't change IDs.

---

## 4. Split `Fun::Fun(Sym *)` into separately-callable phases

**Current state.** `Fun::Fun(Sym *)` in `ifa/if1/fun.cc:54`
unconditionally calls `build_cfg()`, `build_ssu()`, `build_uses()`,
`setup_ast()`, `check_invariants()` in one breath.

**Why testing wants it.** The CFG test wants to construct a `Fun`
with CFG but without SSU (to diff CFG alone before phi/phy noise);
the SSU test wants CFG+SSU but maybe to inspect intermediate state.

**Change.** Keep the convenience constructor as default. Add an
opt-in: `Fun::Fun(Sym *, FunBuildFlags f)` where flags select which
phases to run. The CFG test passes
`FUN_BUILD_CFG_ONLY`; the SSU test passes `FUN_BUILD_CFG |
FUN_BUILD_SSU`. The default (no flags) is unchanged.

**Risk.** Low. Default-arg overload, no caller change.

**Acceptance.** A test can build a Fun with just CFG and print
the CFG without phi/phy nodes appearing.

---

## 5. Suppress `if1_simple_dead_code_elimination` opt-in

**Current state.** `if1_finalize` calls
`if1_simple_dead_code_elimination` unconditionally (gated only on
`fdce_if1` which defaults true).

**Why testing wants it.** The finalize-phase test should be able
to diff the pre-DCE and post-DCE state separately.

**Change.** Promote `fdce_if1` from a bool to a "phase enable bitmask"
or expose a `if1_finalize_step(IF1*, int phase)` that runs one
sub-phase. Either way, a test can:

```c
if1_finalize_step(if1, FINALIZE_PRIM_BIND);
// diff
if1_finalize_step(if1, FINALIZE_DCE);
// diff
if1_finalize_step(if1, FINALIZE_FLATTEN);
// diff
```

**Risk.** Low. Default unchanged: all steps run.

**Acceptance.** Test can introspect post-prim-bind state before
DCE wipes anything.

---

## 6. Move `DEBUG_PRINT` and friends to log channels

**Current state.** Several files (`fa.cc`, `clone.cc`, `cg.cc`)
use `fprintf(stderr, "DEBUG: ...")` or unconditional `printf` in
hot paths. We already gated `fa.cc:DEBUG_PRINT` on `ifa_debug`
(see [../analysis/fa.cc](../analysis/fa.cc) §1).

**Why testing wants it.** Tests diff stdout. Any unconditional
print is noise. Per-phase printers should be the only thing on
stdout during a test.

**Change.** Audit all `fprintf(stderr, "DEBUG:` and `printf("DEBUG`
in `ifa/`. Replace with `log(LOG_<channel>, ...)` using the
existing log framework ([COMMON.md](../COMMON.md) §9).

Specific known offenders:
- `ifa/codegen/llvm.cc`: many `fprintf(stderr, "DEBUG:...")` —
  flag with `LOG_LLVM_CODEGEN`.
- `ifa/codegen/llvm_primitives.cc`: same.
- `ifa/codegen/llvm_codegen.cc`: same.

**Risk.** Low. Default behavior unchanged unless the channel is
enabled.

**Acceptance.** `ifa-test --phase=codegen-llvm <fixture>` produces
deterministic stdout with no DEBUG noise.

---

## 7. Phase printers as first-class

**Current state.** `print_code`, `print_syms`, `if1_dump_*` are
debug printers in `if1.cc`. They were designed for human reading,
not test diffing — sometimes include line numbers, sometimes not;
field order varies; "not interesting" fields are omitted.

**Why testing wants it.** Per-phase normalizers ([TESTING.md](../TESTING.md)
§2 Layer B) need stable, complete, deterministic output. The
existing printers are close but not there.

**Change.** Each phase plan file (`phases/01...`) defines a printer.
Implement each as `void print_<phase>_normalized(FILE *, IF1 *)`
(or similar — phase-specific signature). Live under
`ifa/testing/printers/` so they're separate from the debug
printers.

A phase printer is allowed to *reuse* logic from the debug printer
(`print_code` etc.) but is responsible for sorting and formatting
its output deterministically.

**Risk.** None — these are new files alongside existing code.

**Acceptance.** Each per-phase doc lists a printer signature and
golden format; the printer exists and is exercised by at least one
test.

---

## 8. Extract clone-state struct from `clone.cc`

**Current state.** `clone.cc` uses several module-level statics
(`fa->global_avars`, `equiv` fields on CSes/ESes, ...). State is
spread across `FA`, `CreationSet`, `EntrySet`, `Fun`.

**Why testing wants it.** Clone tests want to reset clone-specific
state between runs without touching the (more expensive) IFA state.

**Change.** Introduce a `CloneState` struct (in `clone.h`) that
holds the clone-pass scratch (`equiv` chains, partition maps).
`CreationSet`/`EntrySet`/`Fun` keep pointers to it. `clone(fa)`
allocates and tears down on entry/exit.

**Risk.** Medium. Touches several files. Defer until it's
needed for a clone test (around milestone M6).

**Acceptance.** Running `clone(fa)` twice on the same FA produces
the same result both times (currently it would crash or
double-clone).

---

## 9. Make `Primitives::find` and `pattern_match` testable

**Current state.** Both functions take many parameters with
side-effects on the caller (`FA *fa`, `AVar *send`, ...).

**Why testing wants it.** Unit-test these in isolation: given a
SEND PNode, what does `Primitives::find` return? Given args + a
visible-funs list, what does `pattern_match` produce?

**Change.** Refactor to:
- `Primitives::find(...)` is already pure; nothing to do.
- `pattern_match` has implicit dependencies on `fa->patterns`,
  `sym_match_cache`. Pass those explicitly; default args preserve
  the current call-site signature.

**Risk.** Low. Additive overloads.

**Acceptance.** A unit test can call `pattern_match(...)` without
needing a full FA setup.

---

## 10. Inter-phase format = on-disk format

**Current state.** No serialization of IF1 between phases. The
deprecated stub at [LLVM.md](../LLVM.md) describes a never-completed
attempt at this.

**Why testing wants it.** With per-phase normalized formats
(Layer B in [TESTING.md](../TESTING.md) §2), each output is *also*
a valid `.ir` (or `.ir.cfg`, `.ir.fa`, ...) format. A test can
checkpoint phase N's output and start the next test at phase N+1.

**Change.** Each phase printer doubles as a serializer. A reader
for each format lets you start the pipeline at a chosen phase.

This is **substantial** — equivalent to designing 8 different file
formats. Defer to a separate sub-plan if/when needed. Realistically
not needed until incremental compilation (CDB — see
[notes/001-compilation-database.md](../notes/001-compilation-database.md))
is revived.

**Acceptance.** N/A in v1.

---

## Status checklist

- [~] §1 Per-FA worklists — **subsumed by §2** (`fa_reset` clears
      worklists; the architectural move isn't needed for test
      isolation). Revisit if multi-threaded analysis ever lands.
- [x] §2 Singleton reset — `ifa_reset()` lands in commit 1400f83
- [x] §3 Deterministic IDs — ID counters reset by `fa_reset()`
- [x] §4 Split `Fun::Fun` — `FUN_BUILD_*` flags + overload in 085c803
- [x] §5 Finalize sub-phases — `if1_finalize_*` entry points in 2aabc2f
- [x] §6 DEBUG_PRINT audit — LLVM backend gated on `ifa_debug` in 49dda85
- [ ] §7 Phase printers — needed by **each phase** (its own item)
- [ ] §8 CloneState — needed by **phase 06** (clone)
- [ ] §9 Pure dispatch helpers — *nice to have*, **phase 04**
- [ ] §10 On-disk format — deferred

The blockers (§1–§6) all landed together. Remaining items (§7–§9)
are per-phase work that travels with the phase implementation; §10
is deferred indefinitely.
