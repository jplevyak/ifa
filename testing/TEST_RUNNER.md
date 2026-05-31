# `ifa-test` — Test Harness

The CLI runner that loads an `.ir` file, drives the IFA pipeline up
to a chosen phase, runs that phase's normalized printer, and diffs
against a golden file.

See [TESTING.md](../TESTING.md) for the big picture and
[IF1_TEXT_FORMAT.md](IF1_TEXT_FORMAT.md) for the input format.

---

## 1. Goals

- Single binary, fast startup (parse + run + diff in < 100ms for
  small tests).
- Trivial to add a test: one `.ir` and N `.expected` files in
  `ifa/tests/ir/<phase>/`.
- Trivial to rebless: `ifa-test --rebless <pattern>` updates
  goldens.
- Trivial to debug: `ifa-test --keep --dump <name>` keeps every
  per-phase output for inspection.
- Friendly to CI: structured exit code (0/1) and concise summary.

---

## 2. Binary

Location: `ifa/ifa-test` (new), built from
`ifa/testing/ifa_test_main.cc`.

```c
int main(int argc, char *argv[]) {
  parse_args(argc, argv);
  for (auto fixture : selected_tests()) {
    run_one(fixture);
  }
  print_summary();
  return failed ? 1 : 0;
}
```

Links against `libifa_gc.a` plus the parser/writer from
[IF1_TEXT_FORMAT.md](IF1_TEXT_FORMAT.md) §6–7 and the per-phase
printers (one per phase doc).

---

## 3. CLI

```
ifa-test [options] [pattern]...

Options:
  --phase NAME            run only this phase (default: all up to codegen)
  --list-phases           print the phase names and exit
  --rebless               update .expected files instead of diffing
  --keep                  keep tests/ir/build/ outputs after run
  --dump NAME             print the full per-phase output for NAME
  --bail                  stop at first failure
  -v / --verbose          show every pass result, not just failures
  -h / --help             this
```

`pattern` is a substring match on test name (path under
`ifa/tests/ir/`). Default: all tests.

---

## 4. Phase names

These match the per-phase plan files; see
[phases/00_INDEX.md](phases/00_INDEX.md).

```
finalize         if1_finalize: DCE + prim binding + flatten
cfg              Fun::build_cfg
ssu              Fun::build_ssu (phi/phy + rename)
dom              build_cfg_dominators
loops            find_local_loops + find_recursive_loops
patterns         build_arg_positions + build_patterns
fa-init          FA::analyze through one initial pass
fa-converge      FA::analyze to fixed point (after splitter)
clone            clone(fa)
dce              mark_live_code + mark_live_types + mark_live_funs
freq             frequency_estimation
inline           simple_inlining + post-inline DCE
codegen-c        c_codegen_print_c (text C output)
codegen-llvm     llvm_codegen_print_ir (text LLVM)
```

`--phase` runs the pipeline up to and including the named phase.
Earlier phase outputs are produced as side effects (their state is
needed) but only the named phase is diffed against a golden.

---

## 5. Fixture layout

```
ifa/tests/ir/
├── README.md
├── format/
│   └── roundtrip.ir
├── finalize/
│   ├── 01_simple_dce.ir
│   ├── 01_simple_dce.finalize.expected
│   ├── 02_prim_binding.ir
│   └── 02_prim_binding.finalize.expected
├── cfg/
│   ├── 01_linear.ir
│   ├── 01_linear.cfg.expected
│   ├── 02_if_else.ir
│   ├── 02_if_else.cfg.expected
│   ├── 02_if_else.dom.expected   ; opt: tests can validate multiple phases
│   └── 03_loop.ir
├── ssu/
│   └── ...
├── fa/
│   ├── 01_constant_fold.ir
│   ├── 01_constant_fold.fa-init.expected
│   └── 01_constant_fold.fa-converge.expected
├── clone/
│   └── ...
└── codegen/
    └── ...
```

One subdirectory per phase. Test files are numbered for ordering.
Golden files are named `<test>.<phase>.expected`.

---

## 6. Per-test execution

For each `<test>.ir`:

1. `ifa_init(new TestCallbacks)` — fresh IF1 + PDB.
2. `parse_ir_file(<test>.ir)` — populate IF1.
3. For each phase up to `--phase`:
   - Run the phase function (e.g., `if1_finalize(if1)`,
     `for f in allclosures: new Fun(f); pdb->add(f)`).
   - Capture its normalized output via the per-phase printer.
   - If a `<test>.<phase>.expected` file exists, diff.
4. Report PASS/FAIL with the failing diff.
5. Tear down: free state. (Singleton reset; see
   [REFACTORING.md](REFACTORING.md) §2.)

Test isolation is crucial — each `.ir` must start from a clean
IF1/PDB/FA. Currently those are process-wide singletons; see
[REFACTORING.md](REFACTORING.md) §2 for the cleanup path.

### 6.1 `TestCallbacks`

Minimal `IFACallbacks` impl for the test harness. No frontend
features — just `new_Sym` and the required virtuals. Lives in
`ifa/testing/test_callbacks.{cc,h}`.

```c
class TestCallbacks : public IFACallbacks {
public:
  Sym *new_Sym(cchar *name) override {
    Sym *s = new Sym();
    if1_register_sym(if1, s, name);
    return s;
  }
  Sym *make_LUB_type(Sym *s) override { return s; }
  // Default no-ops for the rest.
};
```

---

## 7. Golden file format

Each `<test>.<phase>.expected` is a plain-text snapshot of the
corresponding per-phase printer's output. Format is documented per
phase ([phases/01_if1_finalize.md](phases/01_if1_finalize.md) §3,
etc.).

Common rules:
- LF line endings.
- No trailing whitespace.
- Sorted where order doesn't matter.
- Names from the input file used wherever Sym/Var has a name.

Reblessing: `ifa-test --rebless <pattern>` runs each matching test
and writes the actual output to the golden file. Always inspect
`git diff ifa/tests/ir/` before committing.

---

## 8. Makefile integration

```
make test-ir            # run ifa-test on all fixtures
make test-ir-rebless    # ifa-test --rebless on all fixtures
make ifa-test           # just build the binary
```

Added to `ifa/Makefile` (NOT the root pyc Makefile — IFA-internal
tests stay under `ifa/`).

Also wired into the root:
```
test: test-unit test-ir test-e2e
```
(See root [Makefile](../../Makefile) §Tests.) Order: unit (cheap)
→ IR (mid) → e2e (slow).

---

## 9. Running a single test by hand

```bash
cd ifa
./ifa-test --keep --phase=cfg cfg/02_if_else
# Outputs:
#   ifa/tests/ir/build/cfg/02_if_else.cfg.out
# Diff against:
#   ifa/tests/ir/cfg/02_if_else.cfg.expected
```

The `--keep` flag preserves the build dir so you can `cat` the actual
output. Without it, runs are cleaned after pass; failed runs always
keep their build dir.

---

## 10. Failure modes

| Outcome | Meaning |
|---|---|
| `PASS` | All requested phases match goldens. |
| `FAIL parse` | The `.ir` itself didn't parse. Bug in test or parser. |
| `FAIL <phase>` | Output differs from golden. Inspect diff. |
| `MISSING <phase>` | No `.expected` file for the requested phase. Test author needs to add one (rebless to generate). |
| `CRASH` | The phase aborted (uncaught assertion, segfault). Usually a real IFA bug. |

On any non-PASS, the runner prints the test name, the kind of
failure, and (for FAIL) up to 40 lines of `diff -u` against the
golden.

---

## 11. CI integration

Stage 1 (per commit): `make test-ir` must pass.
Stage 2 (per PR): full `make test` must pass.
Stage 3 (nightly): include `--phase=codegen-llvm` cases (longer).

The CI yaml goes in `.github/workflows/` once the test infra is real.
Not in scope for this plan.

---

## 12. Future extensions

Not in v1, but the harness should leave room:

- **Property-based tests.** A `--fuzz <phase>` mode could randomly
  perturb an `.ir` and check phase invariants (e.g., post-CFG has
  no unreachable PNodes). Needs a perturbation library; defer.
- **Snapshot lifecycle.** Save per-phase IF1 state to disk between
  test runs. Doubles as CDB-style cache. See
  [IFA.md](../IFA.md) §11.5.
- **Diff-aware reblessing.** `--rebless --interactive` prompts
  per-test before writing.
- **Parallel execution.** Tests are isolated (independent files), so
  parallelism is safe once singletons are per-instance ([REFACTORING.md](REFACTORING.md) §2).

---

## 13. Status checklist

- [x] `ifa/testing/parse_ir.{cc,h}` exists and round-trips a fixture
      (`run_smoke`, `run_roundtrip`, `run_rich_roundtrip` in
      `roundtrip_test.cc`; all pass via `ifa --test`)
- [x] `ifa/testing/write_ir.{cc,h}` exists
- [ ] `ifa/testing/ifa_test_main.cc` parses CLI, walks fixtures
- [ ] `ifa/testing/test_callbacks.{cc,h}` exists (currently inline
      in `roundtrip_test.cc` as `IRCallbacks`)
- [ ] First phase (finalize) prints + diffs golden
- [ ] `make test-ir` wired into Makefile
- [ ] Tests directory `ifa/tests/ir/` created with at least one
      passing test
- [ ] `--rebless` works
- [ ] Wire into root `make test`
