# Issue 013: pyc LLVM backend not built / not exercised by default

**Status:** **closed June 2026** — `test_pyc` learned to honor a
`PYC_FLAGS` env var (passed to every pyc invocation); the
top-level Makefile's bogus `PYC_SRCS += llvm.cc` line was
removed (the LLVM sources are compiled into `libifa_gc.a`, not
duplicated at top level); `pyc.cc`'s `llvm_codegen()` call was
renamed to `llvm_codegen_write_ir()` to match the actual
symbol; and the CI workflow now exports `USE_LLVM=1` job-wide
and runs `PYC_FLAGS=-b ./test_pyc` as a regression-detection
step floored at the baseline pass count.
**Affects:** the top-level `Makefile` (`USE_LLVM` was commented
out by default and the `USE_LLVM`-conditional `PYC_SRCS` line
referenced a non-existent top-level `llvm.cc`), `pyc.cc:45` and
`pyc.cc:99` (the `-b/--llvm` option and the `codegen_llvm`
branch are `#ifdef USE_LLVM`-guarded; the branch called a
non-existent `llvm_codegen()` symbol), `test_pyc` (no env-var
hook for backend selection), the GitHub Actions workflow at
`.github/workflows/ci.yml:78-83`.

Surfaced while collecting the phase-6.3 performance baseline
([`ifa/codegen/PERFORMANCE.md`](../../codegen/PERFORMANCE.md)).

## Symptom

The phase-6.1 CI workflow has a step labelled
`LLVM backend e2e (IFA_LLVM=1 ./test_pyc)`. That step is currently
a **no-op for the LLVM path**:

1. The default `make` does *not* set `USE_LLVM=1`, so pyc is built
   without any LLVM branch. The `-b/--llvm` CLI option and the
   `codegen_llvm` runtime branch are both compiled out.
2. The pyc CLI's env var for the LLVM backend (when it is
   compiled in) is `PYC_LLVM`, not `IFA_LLVM`. `IFA_LLVM` is the
   convention for the *V-language* `ifa` standalone executable
   (`ifa/AGENTS.md:31`).
3. `test_pyc` reads neither `PYC_LLVM` nor `IFA_LLVM`. It
   unconditionally runs whatever `pyc` binary is on `$PYC`
   (default `./pyc`). It has no flag-injection hook for the
   backend.

The combined effect: every CI run executes the C backend twice —
once in the `make test` step, once in the `IFA_LLVM=1 ./test_pyc`
step. The second step looks like it's testing the LLVM backend
but isn't.

Locally, the same applies to anyone trying
`PYC_LLVM=1 ./pyc <file.py>` against a default-built pyc — the
env var has no effect, and pyc silently runs the C backend.

## Root cause

Three layers stack to produce the no-op:

- **Build layer.** `Makefile:14` says `#USE_LLVM=1` with the
  comment "(work in progress; see ifa/CODEGEN_LLVM.md)". The
  default build leaves the LLVM path uncompiled.
- **CLI/runtime layer.** When `USE_LLVM` *is* defined,
  `pyc.cc:45` exposes `--llvm` / `-b` / `$PYC_LLVM`. None of
  these reach down through `test_pyc` to per-test invocation.
- **Test harness layer.** `test_pyc` has no
  `--backend=llvm`-style flag, no `PYC_FLAGS` injection hook,
  and doesn't translate `IFA_LLVM=1` into a `-b` arg.

The result: nothing in the default workflow ever invokes the
LLVM path, even though CODEGEN_PLAN phase 3 landed a substantial
amount of LLVM primitive implementation work and the test suite
*could* exercise it.

## Proposed fix

Three-step, each independently committable:

1. **`test_pyc`: add backend-injection hook.** Read a `PYC_FLAGS`
   env var and pass its value to each `pyc` invocation. A user
   can then do `PYC_FLAGS=-b ./test_pyc` (or whichever flag
   chooses LLVM) and the harness Just Works without per-test
   sidecar edits.
2. **CI workflow: pick the right env / flag.** Either rename
   the CI step's env var to whatever fix #1 settles on
   (e.g. `PYC_FLAGS=-b`), or — if LLVM-via-pyc still isn't
   ready — delete the step and add a comment that the LLVM
   path is currently off, with a pointer back to this issue.
3. **Top-level Makefile: decide on `USE_LLVM=1` for CI.**
   Either:
   - Default it on (`USE_LLVM=1` no longer commented out) and
     accept the LLVM library dep at build time. CI installs
     `llvm-dev` already (phase 6.1), so this is small.
   - Or keep it off by default but flip `USE_LLVM=1` for the
     CI build step that runs the LLVM e2e. This decouples
     casual builds from the LLVM toolchain.

Either build-layer choice works; the question is whether `pyc`
end-users should have LLVM in the default path.

## Verification plan

After the fix:

- A default `make` either keeps producing a C-only pyc (build
  choice b) or starts producing a LLVM-capable pyc (build choice
  a). Existing 74/1/0/2 test result holds.
- `PYC_FLAGS=-b ./test_pyc` (or whatever §1 settles on) produces
  `.ll`/`.o` artifacts in `tests/build/` and runs the LLVM path
  for every test. The pass count is allowed to differ from the C
  backend's — file a tracking sub-issue for each test that
  diverges, the LLVM backend isn't expected to match perfectly
  yet.
- The CI workflow's "LLVM backend e2e" step actually exercises
  the LLVM path (visible via `.ll` files in `tests/build/` or via
  a `pyc.cc` debug line).

## What fixing this unblocks

- Phase 6.3 (`ifa/codegen/PERFORMANCE.md`) gets honest LLVM-side
  numbers to pair with the C-side baseline already recorded.
- Phase 5 of CODEGEN_PLAN (Codegen base class wholesale
  migration) becomes testable end-to-end instead of via fixtures
  alone — the `Codegen` lifetime contract is currently observable
  only through the unit fixtures.
- The CI signal stops being misleading. A green LLVM CI step
  today does *not* mean the LLVM backend works.

## What landed (closing notes)

The fix turned out to be steps 1 and 2 from the proposed plan,
plus two bugs that surfaced only because nothing had ever
exercised the `USE_LLVM=1` build path:

- **`test_pyc` honors `PYC_FLAGS`.** A new `PYC_FLAGS` env var
  is prepended to every pyc invocation inside the harness. No
  per-test sidecar edits needed; the convention is documented
  in the script header.
- **Top-level Makefile drops the bogus `PYC_SRCS += llvm.cc`.**
  The LLVM codegen sources are compiled into
  `ifa/libifa_gc.a`, not duplicated at the top level. The old
  line tried to compile a non-existent top-level `llvm.cc` and
  broke any `USE_LLVM=1` build immediately.
- **`pyc.cc` calls the right symbol.** `llvm_codegen()` was a
  rename casualty from earlier work — the actual function is
  `llvm_codegen_write_ir()`. `defs.h` got the same fix.
- **CI exports `USE_LLVM=1` job-wide** so every step's `make`
  builds with the LLVM path compiled in. The
  `IFA_LLVM=1 ./test_pyc` step (which was a no-op) is replaced
  with `PYC_FLAGS=-b ./test_pyc`, floored at the current
  baseline pass count so future PRs that improve the LLVM
  backend ratchet the floor up but routine PRs aren't gated
  on full parity (phase 3.5 territory).
- **`USE_LLVM=1` stays default-off** for casual `make` builds
  per the issue's "option B". The Makefile comment now
  documents how to flip it on and points at CODEGEN_LLVM.md.

Deferred: full LLVM-backend parity (`PYC_FLAGS=-b ./test_pyc`
currently passes 8/74 — known method-dispatch-through-closure
gap, tracked in CODEGEN_PLAN §3.5). The PERFORMANCE.md
LLVM-side numbers item is partly unblocked (the LLVM path can
now be measured for the 8 fixtures that pass) but the
representative-program comparison the doc was set up for is
still bottlenecked on §3.5.

## Related

- `ifa/codegen/CODEGEN_PLAN.md` §9.1 — phase 6.1 CI gates (the
  workflow change lands here).
- `ifa/codegen/CODEGEN_PLAN.md` §3.5 — the parity gap
  `PYC_FLAGS=-b ./test_pyc` currently surfaces.
- `ifa/codegen/PERFORMANCE.md` §1.2 — the missing LLVM numbers
  this issue partly gates.
- `ifa/CODEGEN_LLVM.md` §12 — the `-b` / `--llvm` /
  `PYC_LLVM=1` CLI documentation.
