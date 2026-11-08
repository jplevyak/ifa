# ifa/issues

Open work items for the IFA library. Each file is one item: the symptom
with a repro, the mechanism as far as measured, the plan with a stop
condition, the dead ends with the measurement that killed each, and how to
verify. They are checked-in documents that travel with the code, so the
next investigator starts from the trail instead of re-deriving it.

*Reorganised 2026-11-03: 29 open → 22, every open file rewritten to its
current state and made an action item; sixteen closed into survivors or
distributed; ten new action issues (196–205) take the rows of the four
November reviews. Consolidated before that on 2026-09-28 (61 → 29). The
previous README, with the 2026-09-28 closure table, is in git:
`git show 123ac81:ifa/issues/README.md`.*

## Read first: how splitting work is judged here

IFA is a simultaneous data and control flow analysis over a type-value
lattice. It starts from the MINIMUM contours and splits only on demand
(AGENTS.md). Every splitting issue is held to these rules. Each one is a
lesson this directory paid for.

1. **A demand is something observing a distinction and being unable to
   proceed**: a violation, an irrepresentable union, a dispatch that
   cannot resolve. "This is a union", "this CS has several creation
   points" and "these came from different places" are facts, not demands.
   ([closed/146](closed/146-remove-all-arbitrary-splitting.md))
2. **The two-question test:** would the split happen without the demand
   (then it is arbitrary)? Does the demand decide WHETHER and the handle
   only WHICH (then it is a mechanism)? Partition size is what the demand
   distinguishes, never a count of callers, creation points or receivers.
3. **Non-monotone means arbitrary.** A lever that makes results worse as it
   splits more is not earning its keep. **Delete it, don't default it off.**
4. **Types are already converged when the split stages run.** "When" is
   not the defect. ([closed/157](closed/157-FA-all-demand-must-be-evaluated-at-quiescence.md))
5. **The confluence is a CONTOUR, and the demand is observed where the
   union is USED**, usually far from the merge. Backtrack it. Do not act at
   the symptom. (AGENTS.md; [closed/152](closed/152-FA-backtrack-the-demand-to-the-merged-creation-set.md))
6. **A merge can be taken back.** Every pass re-derives types from bottom,
   and only DECISIONS persist. A decision taken on transient types is
   revisited at convergence, not protected.
   ([170](170-FA-contours-minted-on-transient-types-are-never-remerged.md))
7. **Never identity by provenance or by name.** Types and CS partitioning
   are identity; representation properties (arity, width, nil-ability) are
   legitimate but belong behind `IFACallbacks`
   ([201](201-representation-oracle.md)); allocation site, call site and
   depth are handles at most. ([closed/130](closed/130-FA-identity-keyed-on-sym-name.md),
   [closed/132](closed/132-arity-is-representation-not-provenance.md))
8. **Measure on the corpus, same binary, env toggled.** The suite is blind
   to most of this area. ([closed/147](closed/147-analysis-result-depends-on-the-binary-not-the-inputs.md))
9. **Check for a missing builtin before blaming FA.**
   ([closed/086](closed/086-list-and-dict-have-no-copy-method.md))
10. **A condition the compiler detects is a refusal or a demand, never a
    run-time trap.** ([102](102-codegen-detected-conditions-are-refusals-or-demands.md))

## The plan

Ordered by value against effort, with what each unblocks. Items in one
row are independent of each other.

| order | do | why first | unblocks |
| --- | --- | --- | --- |
| 1 | the small defects. Done 2026-11-06: [200](200-llvm-backend-parity.md) A1 and A5, [204](204-optimize-pass-fixes.md) rows 1–2, [039](039-FA-uninitialized-local-reads-silent.md) item 1, [197](197-tuple-arity-dispatch-leftovers.md) item 1 (no fixture yet). [206](closed/206-a-field-read-through-a-union-receiver-reinterprets-int-as-float.md) (closed 2026-11-07: refused, then satisfied by a class-tag test at the read; `softrender` unchanged). [200](200-llvm-backend-parity.md) A6 (done 2026-11-07: three `-b` index-path defects behind `softrender` rendering nothing). Left: 204 row 3 (tried; not a day), 039 item 2 (needs a sentinel, not a day) | silent deviations on the default path | the contract (AGENTS.md) holds on both backends |
| 2 | [102](102-codegen-detected-conditions-are-refusals-or-demands.md): the eleven traps, one at a time. Census done 2026-11-07: nine never reached, now refused; the C unresolved-if guard moved to live IFs. LLVM no longer emits FA-dead regions (its `unreachable` fallbacks: 48 → 0). Left: None-receiver getter (with 165), the dispatch else arms (step 3), `convert_NOTYPE_to_void` | turns miscompiles into refusals and surfaces the demands FA never saw | [201](201-representation-oracle.md) has its witnesses |
| 3 | [196](196-the-setter-stage-splits-on-a-fact.md): the counterfactual setter demand | the one open design question in the splitter | [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) step 3, `go`'s one layout, voronoi's one `list[int64]` |
| 4 | [205](205-ai-centric-interface.md) D1: structured diagnostics with the backtrack attached | the walks exist; an agent or a person can then act on temporal unions by editing the program | every refusal classified; the corpus edit policy becomes mechanical |
| 5 | [203](203-shrink-the-graph-fa-walks.md): measure, then demand-placed renames and copy propagation | FA cost is Vars × contours × passes; this is the Vars | [186](186-FA-pass-one-confluence-through-shared-builtins.md) direction 3 |
| 6 | [198](198-decision-snapshot-across-compiles.md) | most of a large program's convergence, on every recompile | the edit-compile loop |
| 7 | [186](186-FA-pass-one-confluence-through-shared-builtins.md) directions 1–2: deferred connection, a wide-union AType | the structural cost of start-minimal on pass 1 | othello3 under the cap |
| 8 | [201](201-representation-oracle.md), then [199](199-emitted-code-performance.md) item 1 (a benchmark mode) | one owner for representation; a number for emitted code | 054's header, 030's tables, the LLVM contract's width table |
| 9 | [202](202-health-docs-levers-probes-tests-and-ci.md): docs, levers, probes, dead code, the corpus sweep in CI | the docs an agent reads first state deleted architecture; the real gate runs by hand | everything above is reviewable |
| 10 | the precision items that each need their own mechanism: [050](050-FA-general-constant-propagation-unreachable-code.md) 1b, [191](191-FA-an-always-overwritten-initial-value-stays-in-the-field-type.md) row 5, [072](072-FA-empty-container-notype-current-mechanism-and-plan.md), [025](025-FA-intra-function-union-narrowing.md), [119](119-sccp-as-an-outer-fixed-point-over-fa.md), [134](134-remove-the-frontend-forced-split-opt-in.md) | each closes a `.known_issue` or an annotation | strict-mode parity with CPython on the suite |

## Open issues

### CPython contract: silent wrong answers and refusals of valid programs

| issue | what |
| --- | --- |
| [039](039-FA-uninitialized-local-reads-silent.md) | possibly-unbound locals: analysis, `--safe` fill and DEFINITELY-by-default done; the runtime check is not, so `f(0)` prints `42` with no warning. Item 2 needs a sentinel on the unassigned path. |
| [165](165-none-reaching-an-operation-is-silently-accepted.md) | a `None` that arrives: the check exits instead of raising a catchable `TypeError`; primitive argument positions other than `strcat` unchecked. |
| [197](197-tuple-arity-dispatch-leftovers.md) | a `no_static_arity` flip on the final pass leaves a stale dispatch that aborts at run time; same-arity tuples with different element types in one list are refused. |
| [102](102-codegen-detected-conditions-are-refusals-or-demands.md) | eleven codegen-detected type mismatches become `assert(!"runtime error")` under the default mode; `convert_NOTYPE_to_void`; the single-candidate cast safeguard. Refuse or demand, one at a time. |
| [118](118-union-field-representation-and-polymorphic-field-offset.md) | `{bool, None}` has no representation; a tri-state sentinel, chosen through [201](201-representation-oracle.md). |
| [025](025-FA-intra-function-union-narrowing.md) | narrowing: branch correlation over a class union (tail duplication, or the permissive `AttributeError`), and composed predicates (`is not None and isinstance`). |
| [072](072-FA-empty-container-notype-current-mechanism-and-plan.md) | a never-written container's bottom element reaching live code: derive "zero-trip" and "raises" from it. |
| [191](191-FA-an-always-overwritten-initial-value-stays-in-the-field-type.md) | fields fixed by `overwrite.cc`; open: `[None] * n` with scalar elements (loop coverage). |
| [050](050-FA-general-constant-propagation-unreachable-code.md) | a global slot is flow-insensitive: write-once globals first (1b), then a mod-set and a per-ES summary. |
| [119](119-sccp-as-an-outer-fixed-point-over-fa.md) | SCCP as an outer fixed point; stage 1 lands in 203. |
| [134](134-remove-the-frontend-forced-split-opt-in.md) | 9 `__pyc_clone_constants__` annotations remain, each with a named blocker; the `PER_CS_RECEIVER` stage goes with them. |

Frontend-side items from the same work are in the top-level `issues/`:
[185](../../issues/185-list-eq-tuple-is-true.md) (`[1, 2] == (1, 2)` is True),
[186](../../issues/186-duplicate-method-definitions-are-an-ambiguous-call.md)
(duplicate defs), [172](../../issues/172-stopiteration-value-reads-the-global-exception-slot.md)
(the global exception slot).

### Splitting and precision

| issue | what |
| --- | --- |
| [196](196-the-setter-stage-splits-on-a-fact.md) | **the design question**: SETTER nominates on a fact; the demand it needs is a counterfactual over converged writers. Blocks 170 step 3. Carries the watch list and the do-not-rebuild list. |
| [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) | re-derive at convergence: EntrySet merge landed; step 2 (routing on an empty contour) small; step 3 (CreationSet merge) blocked on 196. |

### Analysis cost

| issue | what |
| --- | --- |
| [186](186-FA-pass-one-confluence-through-shared-builtins.md) | othello3: pass 1 carries a ~2,000-CS union through the shared builtins; deferred connection and a wide-union AType are the directions. |
| [203](203-shrink-the-graph-fa-walks.md) | demand-placed phy renames, copy propagation before FA, dead arms as a decision table, what mid-FA inlining is, liveness fed forward. Measure first. |
| [198](198-decision-snapshot-across-compiles.md) | warm start: clone equivalence classes and the ledger, keyed on edit-stable program-point names, read on pass 1. |

### Codegen and representation

| issue | what |
| --- | --- |
| [200](200-llvm-backend-parity.md) | A1 (layout contract), A5 (int bits in float slots) and A6 (three index-path wrong answers) done; the LLVM backend still duplicates four decisions and has no IR fixture or determinism check. |
| [201](201-representation-oracle.md) | representation has three owners (clone, codegen, FA's four predicates) and is keyed on C type strings; one oracle, read by all; slot elision into liveness and clone. |
| [199](199-emitted-code-performance.md) | no run-time baseline exists; a benchmark mode first, then dispatch tables (030), the tuple header (054), the builtin shapes (190). |

### Interface, tooling, health

| issue | what |
| --- | --- |
| [205](205-ai-centric-interface.md) | structured diagnostics with the backtrack attached, a type query by position, a time budget with a partial-result contract, server mode. |
| [204](204-optimize-pass-fixes.md) | (rows 1–2 done) the two-hop store probe, the node universe, `exc_check_fold` by name; frequency estimation with no consumer, O(n²) IF liveness, dead-object pruning. |
| [202](202-health-docs-levers-probes-tests-and-ci.md) | the checklist: stale reference docs, inline levers, unindexed probes, dead slots and `#if 0`, the corpus sweep in CI, asserts, ASAN, Vec/Set. |

## Closed on 2026-11-03, and where their residuals went

| closed | why | residual now in |
| --- | --- | --- |
| [010](closed/010-CLEANUP-vec-set-api-cleanup.md) | merged | 202 (Vec/Set) |
| [030](closed/030-DISPATCH-polymorphic-dispatch-fat-pointers.md) | merged | 199 (tables), 200 (the two divergences) |
| [054](closed/054-CGEN-remove-unconditional-tuple-list-header.md) | merged | 199 (cost), 201 (the flag) |
| [079](closed/079-DISPATCH-single-candidate-dispatch-unchecked-cast.md) | merged | 102 (safeguard), 025 (permissive remedy) |
| [093](closed/093-CGEN-int-float-union-move-not-coerced.md) | merged | 200 A5 |
| [094](closed/094-FA-asan-heisenbug-blocks-sanitizer-diagnostics.md) | merged | 202 (ASAN) |
| [124](closed/124-FA-refuse-imprecise-inference.md) | residual 1 fixed; the rest moved | 196 (go's six sites), 201 (FA types, not strings) |
| [129](closed/129-plan-demand-driven-creation-set-splitting.md) | the plan landed | 196 (watch list, do-not-rebuild list) |
| [146](closed/146-remove-all-arbitrary-splitting.md) | the audit is complete; inventory empty since 2026-10-15 | 196 (the one open violation); rules 1–3 above |
| [178](closed/178-FA-route4-declines-records-built-through-one-constructor.md) | repro passes; no witness for the rest | 196 (watch list) |
| [185](closed/185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md) | sunfish fixed (136 s, matches) | 197, issues/185, issues/186, 196 |
| [190](closed/190-CODEGEN-run-time-dispatch-census.md) | census complete; nothing separable remains | 199, issues/172 |
| [192](closed/192-review-analysis-and-issues-2026-11.md), [193](closed/193-review-optimize-against-analysis-2026-11.md), [194](closed/194-review-codegen-2026-11.md), [195](closed/195-what-the-concert-compiler-still-has-that-ifa-does-not.md) | the November reviews, distributed | 196–205, 102, 170, 186, 050, 025, 039, 119, 165; each closure note has the row-by-row map |

Each archived file starts with a dated closure note. The body below it
is the historical record and is not maintained.

## Conventions

- Filenames: `NNN-short-slug.md`, NNN zero-padded and never reused. A
  category tag (`FA`, `DISPATCH`, `CGEN`, `LLVM`, `CLEANUP`) is optional.
  A number is cited from code comments and sweeps, so a renamed slug keeps
  its number.
- One issue per file. Cross-link with relative paths. `make test-links`
  checks every link.
- Status line at the top: `open`, `partial`, or `closed` with a date.
  Closed issues move to [`closed/`](closed/) (a flat archive, never
  deleted) with a closure note saying why and where any residual went.
- **Keep an open issue current, not chronological.** When a finding
  supersedes an earlier section, rewrite the section and cite the old text
  in git (`git show <commit>:<path>`). A reader should be able to act on
  the file as it stands. Dead ends are kept only as "do not rebuild" rows
  with the measurement that killed them.
- Every issue carries: the symptom with a runnable repro, the mechanism as
  far as measured, the plan, the dead ends, a stop condition written
  before the next measurement, and verification.
- When one issue's remaining scope is covered by another, close it into
  the survivor and say so in both.
- A `.known_issue` sidecar (in `tests/` or `ifa/tests/`) names the issue
  that owns it. When an issue is closed or merged, repoint its sidecars.

## When to file an issue here vs fix it now

File an issue when the fix is more than about an hour and does not block
the current task, needs a design decision, touches a subsystem the current
task is not auditing, or is a real but rare bug with a clean workaround.
Fix it now when it blocks the current task, when it is a one-line fix whose
test you are already writing, or when the current change is its natural
home.
