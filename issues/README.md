# ifa/issues

Open work items for the IFA library. Each file documents one issue: the
symptom, the root cause as far as it has been traced, the principled fix
(or the options), the dead ends already measured, and how to verify.

These are not GitHub issues. They are checked-in documents that travel
with the code, so the next investigator starts from the trail instead of
re-deriving it.

*Consolidated 2026-09-28: 61 open → 29. Resolved issues were archived,
overlapping ones merged, and every open issue was rewritten to its
current state. The previous README, with the 2026-08 triage and
convergence-session history, is in git:
`git show 3f36072b:ifa/issues/README.md`.*

## Read first: how splitting work is judged here

IFA is a simultaneous data and control flow analysis over a type-value
lattice. It starts from the MINIMUM contours and splits only on demand
(AGENTS.md). Every splitting issue below is held to these rules. Each
one is a lesson this directory paid for.

1. **A demand is something observing a distinction and being unable to
   proceed**: a violation, an irrepresentable union, a dispatch that
   cannot resolve. "This is a union", "this CS has several creation
   points" and "these came from different places" are facts, not
   demands. ([146](146-remove-all-arbitrary-splitting.md))
2. **The two-question test:** would the split happen without the
   demand (then it is arbitrary)? Does the demand decide WHETHER and the
   handle only WHICH (then it is a mechanism)? Partition size is what the
   demand distinguishes, never a count of callers, creation points or
   receivers.
3. **Non-monotone means arbitrary.** A lever that makes results worse as
   it splits more is not earning its keep. **Delete it, don't default it
   off.**
4. **Types are already converged when the split stages run.** "When" is
   not the defect. The cascade-priority gate is (it starves every lower
   stage). ([157](157-FA-all-demand-must-be-evaluated-at-quiescence.md))
5. **The confluence is a CONTOUR, and the demand is observed where the
   union is USED**, usually far from the merge. Backtrack it. Do not act
   at the symptom. (AGENTS.md; closed/152)
6. **A merge can be taken back.** Every pass re-derives types from
   bottom, and only DECISIONS persist. So a decision taken on transient
   types should be revisited at convergence, not protected.
   ([170](170-FA-contours-minted-on-transient-types-are-never-remerged.md))
7. **Never identity by provenance or by name.** Types and CS
   partitioning are identity. Arity and other representation properties
   are legitimate (behind `IFACallbacks`). Allocation site, call site and
   depth are handles at most. ([130](130-FA-identity-keyed-on-sym-name.md),
   [132](132-arity-is-representation-not-provenance.md))
8. **Measure on the corpus, same binary, env toggled.** The suite is
   blind to most of this area, and a 1–2 program delta between two
   builds is noise. ([147](147-analysis-result-depends-on-the-binary-not-the-inputs.md))
9. **Check for a missing builtin before blaming FA.** `list.copy`,
   `dict.copy` and `str.rstrip` were each the first error of a corpus
   program whose failure was being read as a splitting problem.
   ([086](086-list-and-dict-have-no-copy-method.md),
   [124](124-FA-refuse-imprecise-inference.md))

## Open issues

### Demand-driven splitting: principles and plan

| issue | what |
| --- | --- |
| [129](129-plan-demand-driven-creation-set-splitting.md) | **the plan**: start-merged CreationSets, demand-driven separation, a ledger. What landed, what is open (the shared-writer decline, the one-creation-point family), what is settled. |
| [146](146-remove-all-arbitrary-splitting.md) | **the audit**: what was removed, what is still in the tree (MARK_SETTER is LIVE, stage 5's per-CS fan), and a verdict for every off-by-default splitting lever. |
| [157](157-FA-all-demand-must-be-evaluated-at-quiescence.md) | **when demand is asked**: the gate is a cascade priority, not quiescence. Give each stage route 4's shape; route ES-side demands to an actuator; terminate on a fixed point. |
| [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) | decisions taken on transient types (pass-0 unions, momentarily empty contours) are never revisited. Re-derive them at convergence. |
| [156](156-FA-split-int-from-float-coerce-last.md) | numeric: coercion's failure-to-be-exact is the demand. Backtrack to the pure/mixed boundary and split. Coerce last, permissive-only. |
| [134](134-remove-the-frontend-forced-split-opt-in.md) | constants: 9 `__pyc_clone_constants__` annotations remain, each with a named replacement. |
| [132](132-arity-is-representation-not-provenance.md) | arity in CS identity (landed); open: one slot holding two same-sym CSs of different arity. |

### FA: typing and precision

| issue | what |
| --- | --- |
| [049](049-FA-raise-only-contour-notype.md) | a raise-only contour's bottom return is now a hard error. The exceptional exit must not read `fn->ret`. |
| [072](072-FA-empty-container-notype-current-mechanism-and-plan.md) | a never-written container's bottom element reaching live code. Derive "zero-trip" and "raises" from it. |
| [178](178-FA-route4-declines-records-built-through-one-constructor.md) | route 4 cannot separate two creation points of a class built through one constructor; the merged member then contaminates both lists. |
| [177](177-FA-narrowing-does-not-reach-a-module-global.md) | an `isinstance` / `is None` guard does not narrow a module-level variable: each read is a fresh load of the cell. |
| [025](025-FA-intra-function-union-narrowing.md) | branch correlation over a class union: now refused. Tail duplication, or a permissive runtime check. |
| [050](050-FA-general-constant-propagation-unreachable-code.md) | a global slot's value is not call-graph precise. Stages 2-3 (mod-set, per-ES summary). |
| [119](119-sccp-as-an-outer-fixed-point-over-fa.md) | SCCP: explicit executability, dead edges as a between-pass decision, fact providers. |
| [039](039-FA-uninitialized-local-reads-silent.md) | unbound locals: analysis done, `--safe` fill done. Open: the runtime check, DEFINITELY-unbound on by default. |
| [124](124-FA-refuse-imprecise-inference.md) | `--refuse-imprecise`: a nil-only-formal false positive. `go` is blocked on a missing `str.rstrip`. |
| [165](165-none-reaching-an-operation-is-silently-accepted.md) | a `None` that does arrive at an operation reads as zero. Permissive runtime check, strict refusal. |
| [147](147-analysis-result-depends-on-the-binary-not-the-inputs.md) | determinism: the result depends on the binary layout. Canonicalise every order that reaches a decision. |
| [111](111-FA-selective-invalidation-per-pass.md) | performance: pass cost tracks accumulated contours (selective invalidation, blocked on setter classing; or rebuild from decisions). Re-measure first. |
| [086](086-list-and-dict-have-no-copy-method.md) | `list.copy()` / `dict.copy()` are missing from `__pyc__` (sudoku4's first error). |

### Codegen, dispatch, representation

| issue | what |
| --- | --- |
| [179](179-LLVM-quameon-computes-nan.md) | LLVM: quameon runs but computes `nan` (C matches CPython); latent, independent of 132. |
| [181](181-LLVM-unhandled-prim-emits-nothing.md) | codegen: a prim no emitter claims produces NO code and its result reads as 0; LLVM drops `id(function)` and method-slot setters this way. |
| [102](102-corpus-programs-compile-then-abort-at-runtime.md) | an unresolved call becomes a silent runtime stub. Make it an FA violation. Also the untyped-value work list. |
| [079](079-DISPATCH-single-candidate-dispatch-unchecked-cast.md) | the single-candidate fast path casts past union members that lack the method. |
| [118](118-union-field-representation-and-polymorphic-field-offset.md) | `{bool, None}`: a tri-state sentinel representation (bool has spare codes; int64 does not). |
| [121](121-CGEN-dead-clones-emitted.md) | LLVM still emits dead clones; call-site narrowing assumes candidate equivalence it never checks. |
| [093](093-CGEN-int-float-union-move-not-coerced.md) | LLVM: an int MOVE into a float64 slot stores raw bits (missing `sitofp`). |
| [120](120-union-types-are-never-interned.md) | union Syms are never interned; an ownership audit is needed first. |
| [130](130-FA-identity-keyed-on-sym-name.md) | `var_map` and the classtag are keyed on names. |
| [030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md) | performance: high fan-out dispatch has no table. |
| [054](054-CGEN-remove-unconditional-tuple-list-header.md) | performance: every tuple carries a 16-byte list header. |

### Tooling / cleanup

| issue | what |
| --- | --- |
| [094](094-FA-asan-heisenbug-blocks-sanitizer-diagnostics.md) | an ASAN build crashes intermittently on `hello_world.py`. |
| [010](010-CLEANUP-vec-set-api-cleanup.md) | split `Vec`'s array and set roles into two types. |

## Closed on 2026-09-28, and where their residuals went

| closed | why | residual now in |
| --- | --- | --- |
| [007](closed/007-FA-mark-type-stage-coverage.md) | mark stages being deleted | 146 D, 157 |
| [048](closed/048-FA-deepcopy-flow-divergence-genetic2.md) | genetic2 no longer diverges | 165 / issues/048 |
| [061](closed/061-CGEN-multi-tuple-list-null-element-type.md) | fixed | — |
| [066](closed/066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md) | `PYC_CSKEY=3` landed | 170 |
| [068](closed/068-FA-derive-structural-ops-record-field-fold.md) | landed via `inject_tuple_methods` | — |
| [071](closed/071-FA-chess-accumulated-union-notype-cascade.md) | chess matches CPython | — |
| [075](closed/075-FA-element-cs-method-split-idempotent-plan.md) | rejected: receiver-keyed method identity is structural | 146 (`PYC_CSM`) |
| [095](closed/095-LLVM-str-or-none-union-wrong-value.md) | fixed | — |
| [097](closed/097-CGEN-callsite-vs-clone-formal-type-mismatch.md) | merged | 170 |
| [099](closed/099-FA-pending-backedge-avoid-veto-forces-period-2.md) | fixed; relocated churn gone with 074 | — |
| [101](closed/101-FA-first-time-forever-splitting.md) | superseded by start-merged identity | 146 (`PYC_CSELEM`) |
| [105](closed/105-type-degeneration-in-shared-generic-methods.md) | plcfrs matches CPython | — |
| [113](closed/113-FA-setter-equivalence-is-a-global-batch-partition.md) | merged | 111 |
| [123](closed/123-CGEN-union-receiver-field-access-has-no-discrimination.md) | layout contract, prefix layout, elision all landed | 124 |
| [125](closed/125-sunfish-degenerate-dict-setitem-clones.md) | symptom gone | 102 |
| [126](closed/126-assess-residual-method-slot-reads.md), [127](closed/127-audit-zero-means-unknown-and-a-real-answer.md) | assessments complete | — |
| [128](closed/128-cs-identity-over-discriminates-vs-element-type.md) | `PYC_CSDCPA1=2` is the default | 129 |
| [131](closed/131-demand-driven-constant-splitting.md) | premise falsified | 134 |
| [133](closed/133-split-a-container-on-its-element-type.md) | separator landed | 129 |
| [135](closed/135-empty-sibling-contour-wins-the-clone-merge.md) | fixed 2026-09-06 | — |
| [136](closed/136-creation-point-identity-is-es-x-call-site.md) | absorbed into AGENTS.md | 146 (`PYC_CSCALLSITE`) |
| [137](closed/137-scalar-receiver-resolves-to-container-method.md) | pystone runs | — |
| [142](closed/142-linalg-empty-list-collapse-is-a-fixed-point.md) | linalg matches CPython | — |
| [143](closed/143-shared-container-method-contours-refuse-cs-splits.md) | blockers fixed at the default | 146 (`VIOLCS`, `CSMEMBER`) |
| [144](closed/144-route-4-fans-per-creation-point-instead-of-partitioning.md) | partition landed, fan removed | 170, 156 |
| [145](closed/145-numeric-coercion-is-not-gated-on-permissive-mode.md) | piece 1 landed | 156 |
| [148](closed/148-stage-5-starvation-root-caused.md) | merged | 157 |
| [149](closed/149-the-largest-diagnostic-class-reports-nothing.md) | diagnostic fixes landed | 102 |
| [150](closed/150-is-not-none-never-folds.md) | fixed | 134 |
| [151](closed/151-split-an-entryset-on-a-constant-argument-on-demand.md) | CONST_DEMAND landed | 134 |
| [171](closed/171-FA-numeric-union-sustains-itself-through-a-shared-contour.md) | merged | 156 |

Each archived file starts with a dated closure note. The body below it
is the historical record and is not maintained.

## Conventions

- Filenames: `NNN-CAT-short-slug.md`, NNN zero-padded and never reused.
  CAT is one of **FA** (flow analysis, splitter, convergence), **DISPATCH**,
  **CGEN** (C backend), **LLVM**, **CLEANUP**. Recent files often omit
  the tag; that is fine.
- One issue per file. Cross-link with relative paths. `make test-links`
  checks every link.
- Status line at the top: `open`, `partial`, or `closed` with a date or
  commit. Closed issues move to [`closed/`](closed/) (a flat archive,
  never deleted) with a closure note saying why and where any residual
  went.
- **Keep an open issue current, not chronological.** When a finding
  supersedes an earlier section, rewrite the section. Put the old text
  in git (`git show <commit>:<path>`) and cite it. Do not append
  "CORRECTION" after "CORRECTION". A reader should be able to act on the
  file as it stands.
- Every issue carries: the symptom with a runnable repro, the mechanism
  as far as measured, the principled fix, the dead ends with the
  measurement that killed each, a stop condition written before the
  next measurement, and verification.
- When one issue's remaining scope is covered by another, close it into
  the survivor and say so in both.
- A `.known_issue` sidecar (in `tests/` or `ifa/tests/`) names the issue
  that owns it. When an issue is closed or merged, repoint its sidecars.

## When to file an issue here vs fix it now

File an issue when the fix is more than about an hour and does not block
the current task, needs a design decision, touches a subsystem the
current task is not auditing, or is a real but rare bug with a clean
workaround. Fix it now when it blocks the current task, when it is a
one-line fix whose test you are already writing, or when the current
change is its natural home.
