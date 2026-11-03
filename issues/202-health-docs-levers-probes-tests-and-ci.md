# 202 — health: docs, levers, probes, dead code, tests, CI

**Status: open, a checklist.** Filed 2026-11-03 from the hygiene rows of
[closed/192](closed/192-review-analysis-and-issues-2026-11.md),
[closed/193](closed/193-review-optimize-against-analysis-2026-11.md) and
[closed/194](closed/194-review-codegen-2026-11.md), plus
[closed/010](closed/010-CLEANUP-vec-set-api-cleanup.md) and
[closed/094](closed/094-FA-asan-heisenbug-blocks-sanitizer-diagnostics.md).
Every row is independent; strike them as they land.

## Docs that an agent reads first and that are wrong

- [ ] `ifa/INDEX.md:89–95` links `CG_IR_SEMANTICS.md` as "definitive
      semantics" of an intermediate representation deleted in June 2026.
      Say so, or drop the entry.
- [ ] `ifa/CODEGEN_LLVM_CONTRACT.md` specifies `cg_normalize_v2.cc` and
      `cg_ir_v2_emit_llvm.cc` (deleted). Rewrite to the direct emitter or
      archive.
- [ ] `ifa/CODEGEN_LLVM.md` cites `llvm_codegen.cc` / `llvm_primitives.cc`
      (deleted; the emitter is `cg_emit_llvm.cc`); §14.5 says the runtime
      is not linked (LTO-linked since 2026-10-18); §14.6 says the JIT is
      unimplemented (`jit.cc`, `-j`); §14.8 describes a name-match fallback
      that is gone.
- [ ] `ifa/CODEGEN_C.md` §11.3 says a polymorphic call aborts at run time;
      §11.4 says `Makefile.cg` uses `cc` (it is `clang++ -std=c++23`).
- [ ] `ifa/ARCHITECTURE.md:67–89`: fa.cc "4034 lines" (14,422), clone.cc
      1,031 (2,384); lists `llvm_codegen.cc`, `llvm_primitives.cc`,
      `make_prims.cc`; marks ten existing docs TODO.
- [ ] `ifa/IFA.md:590` says `GLOBAL_CONTOUR` is `(void*)1` (a real
      EntrySet since issue 031); `:696` says static worklists make FA
      non-reentrant (moved into `FA` June 2026).
- [ ] `ifa/CFG_SSU.md` and `ifa/OPTIMIZE.md` carry line counts and
      `file:line` tables from before ifa/039 and the chain inliner
      (ssu.cc 205 → 603, dead.cc 318 → 491, inline.cc sections). Keep the
      gotcha lists; drop the numbers. Neither mentions `exc_check_fold.cc`,
      `find_maybe_unbound`, `match_prim_chain`.
- [ ] `ifa/TESTING.md:146–153` and `testing/phases/00_INDEX.md` mark every
      phase TODO; 68 `.ir` fixtures across 10 phases run in CI.
- [ ] `ifa/ESCAPE_PLAN.md:3` says phases 1–6 landed; `escape.h` says phase
      2, no-op unless `ifa_escape_in_fa`, default 0 in `common/fail.h`.
      One of them is wrong.
- [ ] `fa.h:14,789` cite `ifa/analysis/CLEANUP.md` and "AUDIT §2.2";
      neither exists (`make test-links` cannot see C++ comments).
- [ ] `ifa/codegen/`: six June planning docs (`AUDIT`, `CODEGEN_PLAN`,
      `CG_IR_PARITY_PLAN`, `FFI_LIBRARY_MIGRATION`, `CG_IR_019_DESIGN`,
      `LLVM_DIRECT_EMIT_PLAN`), `PERFORMANCE.md` (2026-06-11 baseline) and
      `CG_IR_SKETCH.h` (824 lines, included by nothing) describe the
      pre-reset state. One short `CODEGEN.md` that says what is true now;
      the rest to `archive/` with a dated note.
- [ ] `ifa/analysis/NOTES.md` Part 2 still lists 010 and 025 as "active
      issues" and 146/157 as auditing in progress.

## Levers and probes

- [ ] 17 behavioural `PYC_*` levers are read inline in `fa.cc` against
      `fa_flags.h`'s own rule (`CONSTDEMAND`, `CSDEMAND`, `CSJOINSPLIT`,
      `CSMERGE`, `CSNUMSPLIT`, `CSOWNER`, `ESMERGE`, `PROMOTE_FIRST`,
      `RECGATE`, `STALL_REANALYZE`, `VIOLATTEMPTS`, …). Move them.
- [ ] Default-on levers keep modes measured unsound "as controls"
      (`SELFPROD=1,2`, `ROUTECYCLE=4`, `HARDREUSE=4`). closed/146's rule:
      delete, don't default off. A mode that is unsound is a trap.
- [ ] 26 env vars in `ifa/codegen` (9 behavioural) with no ledger. One
      `cg_flags.cc` on the `fa_flags.cc` model, measurement beside each
      default.
- [ ] 133 `IFA_DBG_*` / `PYC_DBG_*` probes in `ifa/analysis`, 47 in no
      markdown, no index. One registration macro feeding a table printed
      by `pyc --list-probes`; the dead ones become visible.

## Dead code and dead slots

- [ ] `dead.cc:395–491`: 93 lines of `#if 0` referencing a field that no
      longer exists. Notes convention: write `notes/006`, delete the code.
- [ ] `FAPassStage` keeps five dead slots of twelve (`MARK_TYPE`,
      `MARK_SETTER`, `MARK_SETTER_OF_SETTER`, `CSM_ELEMENT_CS`,
      `CARTESIAN_PRODUCT`) for numbering stability; the IR goldens that
      pin the numbers can be re-blessed once.
- [ ] `run_split_stages` carries paragraphs about deleted stages and the
      2026-07-11 revert; `fa.cc` has 270 issue citations and 294
      `fprintf(stderr)`. Move history into the issue that owns it; keep
      the invariant and the one-line why; one file per split stage behind
      `fa_internal.h` (the ifa/167 direction, stopped at diagnostics).
- [ ] `split_type_view` (closed/124) changes nothing for nil since
      closed/178 and is left in place.
- [ ] `exc_check_fold.cc` identifies the exception slot by
      `strcmp(sym->name, "__pyc_exc__")` inside the library; the
      narrowing hooks (`narrowing_isinstance_name` and two more,
      `ifa/ifa.h:103`) match callee names in `Code_IF`. Both are by-name
      decisions in generic code; a Sym flag set by the frontend makes each
      structural. ([204](204-optimize-pass-fixes.md) carries the first.)

## Tests, asserts, CI

- [ ] **The corpus `check` sweep is not in CI.** It is the real gate for
      everything in the splitter (closed/129: "the pyc suite is blind to
      this area"), takes 11 minutes, and runs by hand. A nightly job that
      diffs per-program verdicts against the last committed TSV and fails
      on any program that stops compiling, running or matching.
- [ ] 38 `assert`s in 14k lines of `fa.cc`; `IFA.md` §11.2 lists invariants
      known and not asserted (`contour_is_entry_set` against the pointer's
      type; `edges` against `out_edges`). Assert them under the IR phases.
- [ ] `check_invariants` (`fun.cc:327`) is compiled out in the default build
      (`#ifndef DEBUG` with `DEBUG=1`). [204](204-optimize-pass-fixes.md)
      row 1; listed here because it is the only CFG invariant check there
      is.
- [ ] The `ssu`, `dce` and `inline` IR phases have 2–5 fixtures each, enough
      to pin the printers and not the behaviour
      [203](203-shrink-the-graph-fa-walks.md) changes. Fixtures to add
      before 203: a narrowed branch operand, an un-narrowed one, a
      loop-carried variable narrowed inside the loop, nested branches on
      one operand, `while True:` opening a function; identity-through-join
      for the inliner; a setter kept live only by a reader for DCE.
- [ ] **ASAN build** (closed/094): intermittent SEGV in
      `StringChainHash::canonicalize` on `hello_world.py`, hypothesised to
      be a Boehm conservative-scan root miss under ASAN's layout. Steps:
      retry at HEAD (`1a240ce3` fixed a parser out-of-bounds read since);
      run with `PYC_NO_GC` / `GC_DONT_GC=1`; if it is the collector,
      register the allocation as a root or build Boehm ASAN-aware. Done
      when 100 consecutive ASAN compiles pass, then a suite soak.

## Vec / Set split (closed/010)

`Vec` is a dense array until `set_add()` turns it into an open-addressed
set: `.n` is then capacity, iteration yields null slots, and 17 in-place
`qsort_by_id` sites sort sets as a side effect. The plan, each step
byte-identical: add `Set` beside `Vec` in `vec.h`; pilot in
`ifa/optimize/` (`dom.cc`'s `front.set_to_vec()`); file by file,
`fa.cc` last; drop `set_*` from `Vec`; replace `qsort_by_id` sites with
`sorted_view`. Not a performance change (97.5 % of sets never leave the
≤4 linear path).

## Verification

Each row has its own; the whole: `make test` green on both backends,
`make test-links` clean, and no reference doc names a file that does not
exist (`grep` the deleted names).
