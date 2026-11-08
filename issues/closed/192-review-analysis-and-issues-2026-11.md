# 192 — review of `ifa/analysis/` and `ifa/issues/`: gaps, errors, efficiency, and readiness for an AI-centric development loop

**CLOSED 2026-11-03: distributed.** Every row went to an action issue: A1 → [197](../197-tuple-arity-dispatch-leftovers.md); A2 → [039](../039-FA-uninitialized-local-reads-silent.md); A3 → [issues/185](../../../issues/185-list-eq-tuple-is-true.md); A4 → [issues/186](../../../issues/186-duplicate-method-definitions-are-an-ambiguous-call.md); A5 → [165](../165-none-reaching-an-operation-is-silently-accepted.md); A6 and the trap principle → [102](102-codegen-detected-conditions-are-refusals-or-demands.md); A7 → [170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md); A8, A9, E1-E5 → [202](../202-health-docs-levers-probes-tests-and-ci.md); B1 → [196](../196-the-setter-stage-splits-on-a-fact.md); B2 → [050](../050-FA-general-constant-propagation-unreachable-code.md); B3 → [025](../025-FA-intra-function-union-narrowing.md); B4 → [119](../119-sccp-as-an-outer-fixed-point-over-fa.md); B5, D1, D3, D4 → [205](../205-ai-centric-interface.md); C1, C2 → [186](../186-FA-pass-one-confluence-through-shared-builtins.md); C3 and D2 → [198](../198-decision-snapshot-across-compiles.md); C4 → [170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md); C5 → [205](../205-ai-centric-interface.md). The review itself is the record.

**Status: open. Umbrella / audit**, like [146](146-remove-all-arbitrary-splitting.md).
Written 2026-11-02 against `829f43d`. It takes no new measurements: every
number is from a checked-in sweep, an open issue, or a count over the
sources. Where this file and an issue disagree, the issue is the record.
Each item below names the issue that should own the work; an item with no
owner is new. Close this issue by filing or fixing each row and striking it.

## Verdict

The analysis core is sound on its own terms and unusually well documented.
It is tuned for one-shot batch compiles driven by a human with a debugger.
To serve an AI-centric development loop it needs three things it does not
have: structured, explainable output; warm start across recompiles; and a
bounded, re-entrant engine. Several documented semantic deviations also sit
on the default path with no runtime check and no warning.

## State of the tree

From `sweeps/check__default__9ef6d5b1+f400843f.tsv` (2026-10-07) and a
count of the sources.

| | |
| --- | --- |
| corpus compiles | 67 of 78 rows: 9 refused, othello3 at the cap |
| matches CPython | 30; 5 differ (othello2's int64 overflow, four int/float coercions); 34 unverifiable |
| open issues | 25 (12 FA precision, 6 codegen, 5 splitting policy, 2 tooling); 154 closed |
| `fa.cc` | 14,422 lines; largest function 1,108 lines |
| env levers in `ifa/analysis` | 181 distinct; 133 are `*_DBG_*` probes, 47 of those in no markdown |
| contours | 31,048 EntrySets / 114,327 CreationSets over 76 programs |

sunfish has since been fixed ([185](185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md): 136 s,
matches CPython), so the committed sweep is one tree behind the issues.

## What is strong

- **The principle is enforced by measurement.** Start-minimal contours,
  demand-only splitting, decisions persist and structure is rebuilt.
  [146](146-remove-all-arbitrary-splitting.md) emptied the off-by-default
  lever inventory on 2026-10-15 with a same-binary corpus A/B per removal.
- **Determinism is closed**
  ([147](147-analysis-result-depends-on-the-binary-not-the-inputs.md)):
  two differently-built binaries give identical sweeps and byte-identical C.
  That is the prerequisite for any cache, warm start or bisection.
- **The issues are an engineering record**: symptom with repro, measured
  mechanism, dead ends with the measurement that killed each, a stop
  condition written before the next measurement. 185, 186, 170 and 124 are
  models of a negative result.
- **The backtracking machinery exists.** `collect_violation_imprecisions`,
  `backtrack_to_own_formal`, `cs_backflow`, `find_blocking_es` and
  `report_dispatch_origin` already walk from a demand to the merged contour.
  That is the explanation an agent needs, and it is already computed.
- **The lattice is isolated and unit-tested** (`fa_lattice.cc`,
  `testing/lattice_test.cc`); diagnostics moved to `fa_debug.cc` and the
  census to `fa_census.h` (ifa/167). The remaining monolith is the splitter.
- **The sweep harness is agent-friendly**: cache keyed on the tree and on the
  binary's content, refuses a stale build, re-takes timeouts alone, commits
  its TSVs.

## A. Correctness and semantics

The contract (AGENTS.md): CPython semantics or a compile error under
`--strict`; every accommodation flagged and warned under `--permissive`.

| # | finding | severity | owner |
| --- | --- | --- | --- |
| A1 | **A mid-pass `no_static_arity` flip is never re-dispatched and nothing checks for it at convergence.** The flip happens in `make_kind` and changes no AType, so sends already dispatched keep their target until the next pass. A flip on the final pass leaves a dispatch the fixed-arity header bit contradicts at run time (`arity dispatch: no branch matched`). Fix: record flips per pass; re-enqueue the affected sends, or at least request another pass when the set is non-empty on what would be the last pass. | defect | 185 item 4 |
| A2 | **Possibly-unbound locals print the wrong answer in permissive mode.** 039's table says permissive inserts a runtime `UnboundLocalError` check; it is not built. `f(0)` prints `42`. MAYBE_UNBOUND is advisory in every mode, so the deviation is unflagged on the default path. The fact is already computed; only the backend check is missing. | contract | 039 |
| A3 | **`[1, 2] == (1, 2)` is True** (CPython: False). `list.__eq__` never checks its argument is a list; the guard untyped `l` in two tests and was reverted unexplained. Recorded only as an item in a performance issue, with no warning. File it as its own accommodation with a flag, or find why the guard untypes the operand. | contract | 185 item 2 → new `issues/` entry |
| A4 | **Duplicate method definitions in one class are an ambiguous call**; CPython takes the last. A last-wins rule in the frontend, applied before the arity overloads are generated, resolves it without touching dispatch. | semantics | 185 item 5 |
| A5 | **The None-receiver check exits instead of raising a catchable `TypeError`** (165 residual 1). Routing it through `__pyc_exc__` is the same change that would let the exception printer see only what escapes uncaught (190 item 3, issues/172). | semantics | 165, 190 |
| A6 | **`convert_NOTYPE_to_void` silently voids an untyped value** under `fruntime_errors` (`fa.cc:14071`). IFA.md §11.12 calls it an escape hatch not to lean on; it runs unconditionally in permissive mode. Delete it, or warn per site. | silent salvage | new |
| A7 | **An edge can bind to a contour whose formal is momentarily bottom, and the binding's `ret → result` flow outlives the rebinding** (170 instances 2, 2b). The codegen guard now refuses the program instead of miscompiling it. Either fix the issue names closes it; the msp_ss check against `reroute_uncovered_edges` is still owed. | routing | 170 step 2 |
| A8 | **An ASAN build is not trustworthy** (094), so every memory diagnostic on 14k lines of raw pointers into GC'd objects is blocked. Its own step 1 (retry at HEAD) has not been done since 2026-09-27. | sanitiser | 094 |
| A9 | **The narrowing recogniser decides by callee name**: `narrowing_isinstance_name` / `_is_none_name` / `_is_not_none_name` (`ifa/ifa.h:103–105`) hand FA strings to match in `Code_IF`. Contained, since the names are the frontend's own, but it is the one by-name decision in the core. A Sym flag with a kind, set by the frontend, makes it structural. | tension | new |

## B. Precision

**One design question underlies four open issues.**
[124](124-FA-refuse-imprecise-inference.md) residual 2,
[170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md) step 3,
[178](178-FA-route4-declines-records-built-through-one-constructor.md) and
[185](185-FA-merged-tuple-receiver-makes-the-arity-unroll-live.md) phase A
all end at the same wall: the `SETTER` stage nominates a confluence whenever
a member's writers disagree, a fact, not a demand. A demand-gated version was
built four ways on 2026-10-17 and each lost suite tests, because the demand
that justifies a setter split appears only *after* the merge it would
prevent. 124 names the requirement: a counterfactual, "would merging these
setters make an irrepresentable union?", asked of converged types.

Until it is answered, CreationSets that converge equal cannot be merged
(170 step 3 is blocked on it), `go` keeps two `UCTNode` layouts shedskin
does not need, and any attempted merge runs voronoi and bh to the pass cap.
The shape of an answer is in the tree: route 4's assign-set signature is a
counterfactual partition on converged content. Ask SETTER the same way:
partition the writers by the type each would contribute, and split only
when the partition has more than one irrepresentable-together class. Stop
condition, from 170: if a merge re-creates the same split next pass, a real
demand was misread; find it, add no hysteresis.

| # | finding | owner |
| --- | --- | --- |
| B1 | the counterfactual setter demand, above | 124 / 170 |
| B2 | **flow-insensitive globals** are the remaining structural merge: module cells share one `GLOBAL_CONTOUR` AVar (050 stages 2–3 not started); the pending exception slot is the same mechanism (190 item 3, issues/172). Scripts edited in a loop are exactly the programs that rebind globals. | 050 |
| B3 | **narrowing stops at one predicate**: `flow_var_permit_pred` drops a second distinct predicate ("composition not implemented"), so `if x is not None and isinstance(x, Foo):` narrows on the first test only. Sound, and a precision loss on the commonest typed-Python idiom. Fix: a predicate list on `AVar`. | new |
| B4 | **SCCP stage 1** (an explicit executable bit on `Code_IF` successors, honoured in `add_pnode_constraints`) is a size win before a precision win: unreachable arms today still mint contours carrying bottom. 119's correction is right that executability is one more between-pass decision table. | 119 |
| B5 | **temporal unions** (bh, quameon) are correctly refused; the message is `has mixed basic types` with one location. For a loop allowed to edit the program under `PYC_CHANGES.md`, the diagnostic should carry both writes and say "temporal", the one diagnosis whose fix is in the source. | see C/D |
| B6 | well-scoped and open: 072 (bottom element reaching live code), 025 (branch correlation over a class union), 191 row 5 (`[None] * n` scalar), 134 (9 forced-split annotations), 118 (`{bool, None}`), 093 (LLVM int→float64 MOVE, a one-backend miscompile) | each |

## C. Efficiency

| # | finding | owner |
| --- | --- | --- |
| C1 | **Pass 1 is maximally merged by design, so its cost is union width × program size.** Every shared builtin carries every caller's values on pass 1; othello3 builds a ~2,039-CS union and rebuilds tens of thousands of AVars once per member, ~850 s FA against a 400 s cap. 186's four fixes each cut a constant; none changes the shape. 186 itself concludes shedskin's deferred-connection limit (`CPA_LIMIT`) and incremental connection are scheduling, not identity, and allowed. Neither has been tried. | 186 |
| C2 | **`AType` representation.** A sorted `Vec` with hash-consing and four per-instance memo maps (`union_map`, `intersection_map`, `diff_map`, `coerce_map`, never cleared); a wide union allocates a new canonical AType per added member and memoises every pair. `type_diff`'s inner `subsumed_by` loop over abstract members is O(\|a\|·\|b\|). A bitset over CS ids above a width threshold makes union, diff and subset O(width/64). | 186 |
| C3 | **Nothing is carried between compiles.** Every pass re-derives from bottom (right, per 111), but a 13-pass othello3 pays 90–250 s per pass and an edit-compile loop pays it all again. The decisions that persist (`av->cs_map`, `AEdge::to`, the ledger keyed on interned `(fun, stage, pos, partition)`) are what the deleted compilation database would store ([notes/001](../../notes/001-compilation-database.md)); its prerequisite, deterministic ids, is met by 147. See D2. | new |
| C4 | **Equal CreationSets cannot be merged** (`PYC_CSMERGE` built, measured, off): voronoi has two `list[int64]` CSs to shedskin's one. The ES half bought −2.7 % contours and −7 % compile time; the CS half is worth at least as much, since every extra CS multiplies clone pairs in `determine_basic_clones`. Blocked on B1. | 170 step 3 |
| C5 | **Convergence is bounded by heuristics, not a budget**: `IFA_PASS_LIMIT`, dup-aware `IFA_STALL_LIMIT`, `IFA_NONIMPROVE_LIMIT`, a zero-violation stall counter. fa.h calls the stall guard a mitigation for order-dependent split choices. The verdict is "non-convergent" with no partial result marked as such, and `pass_limit_hit` is not surfaced to the user. See D4. | new |
| C6 | minor, profile first: 17 in-place `qsort_by_id` sites and the copying `sorted_view` (010); `tuple.__eq__` still `0..max_arity`; 150 `getenv` sites in fa.cc, the hot ones cached by hand. | 010, 185 |

## D. Readiness for an AI-centric development system

| need | today | gap |
| --- | --- | --- |
| structured diagnostics | prose on stderr from `show_violations`; `report_analysis_errors` gets `Vec<ATypeViolation *>` with kind, AVar, send | no JSON or SARIF; no stable violation id; location recovery falls back three times (`find_violation_user_loc`) |
| explanation | the backtrack walks exist and print under `IFA_DBG_*` | not attached to the violation; an agent cannot ask "why is this a union" through the API |
| type query | `type_info`, `call_info`, `constant_info` (fa.h); `fa_dump_types` under `-v` | keyed on `IFAAST *`, not source position; no "type of every expression" export |
| warm start | decisions persist across passes | never across compiles (C3) |
| bounded time | pass cap and stall heuristics | no wall-clock budget; no partial-result contract (C5) |
| re-entrancy | `FA` owns its state; `ifa_reset()` for sequential use | globals `fa`, `pdb`, `if1`, `analysis_pass`, `fruntime_errors`; one analysis per process ([notes/005](../../notes/005-singleton-fa-and-pdb.md)) |
| determinism | closed (147) | none |
| agent memory | AGENTS.md, this directory, the sweep cache | reference docs drift (E3); probes unindexed (E5) |

What to build:

- **D1. A diagnostic object with its explanation attached.** Extend
  `ATypeViolation` with the result of the walk the stages already do: the
  confluence contour (id, sym), its creation points with locations, the
  writers that supplied each member of the union, and a classification in
  the existing vocabulary: *temporal*, *polymorphism* (a common user
  ancestor), *one container*, *local merge*, *global slot*, *separable by
  ES split*, *separable by CS split*. `report_dispatch_origin` produces
  exactly this classification for dispatch; the same walk applied to
  BOXING and PRIMITIVE_ARGUMENT gives an agent the one fact it needs,
  whether the fix is in the compiler or in the program. Emit as JSON behind
  `--diagnostics=json`; keep the prose.
- **D2. A decision snapshot** (the compilation database, on today's
  ledger). Serialise after convergence: per function its EntrySets with
  type keys and the `AEdge::to` binding per (pnode, from); per creation
  point its `cs_map`; the ledger. On the next compile, after
  `if1_finalize`, pre-seed contours and bindings for functions whose IF1
  fingerprint is unchanged (`ifa_dbg_bodies` already computes one) and let
  the pass loop re-derive types. Because only decisions persist, a stale
  snapshot costs one pass of precision and never correctness.
- **D3. Server mode.** Reentrancy step 5 in notes/005's hybrid shape:
  `FA *` through the top-level free functions, back-pointers on `EntrySet`
  and `CreationSet` only. One process can then hold an analysis per open
  file, and the sweep can run in-process in parallel.
- **D4. A budget and a partial-result contract.** `--time-budget=N`: on
  expiry finish the current pass, run no split stage, report
  `converged=false` with violations marked mid-iteration. The bit exists;
  the CLI does not expose it.

## E. Code and doc health

| # | finding | owner |
| --- | --- | --- |
| E1 | **`fa.cc`'s comments are a changelog**: 270 issue citations, 294 `fprintf(stderr)`, 17 file-scope mutable statics. `run_split_stages` carries paragraphs on deleted stages (CARTESIAN_PRODUCT, MARK_TYPE, the 2026-07-11 stage-2 revert); `FAPassStage` keeps five dead slots of twelve. The splitter's heart (`split_owner_of_demanded` 1,108 lines, `decide_entry_set_split` 820) has no unit test and is exercised only through whole programs. Not a rewrite: move history into the issue that owns it, keep the invariant and one-line why in code, one file per split stage behind `fa_internal.h` (the ifa/167 direction, stopped at diagnostics). | new |
| E2 | **17 behavioural `PYC_*` levers are read inline in `fa.cc`** against `fa_flags.h`'s own rule (`CONSTDEMAND`, `CSDEMAND`, `CSJOINSPLIT`, `CSMERGE`, `CSNUMSPLIT`, `CSOWNER`, `ESMERGE`, `PROMOTE_FIRST`, `RECGATE`, `STALL_REANALYZE`, `VIOLATTEMPTS`, …). Several default-on levers keep modes measured unsound "as controls" (`SELFPROD=1,2`, `ROUTECYCLE=4`, `HARDREUSE=4`). 146's rule for splitters, delete rather than default off, applies: an unsound mode is a trap, not a control. | 146 |
| E3 | **Reference docs an agent reads first are stale.** `ARCHITECTURE.md:67–82` gives fa.cc as 4,034 lines and lists ten existing docs as TODO. `IFA.md:590` says `GLOBAL_CONTOUR` is `(void*)1` (a real EntrySet since issue 031); `IFA.md:696` says static worklists make FA non-reentrant (moved into `FA` June 2026). `fa.h:14,789` cite `ifa/analysis/CLEANUP.md` and "AUDIT §2.2", neither of which exists; `make test-links` cannot see C++ comments. `TESTING.md:146–153` and `testing/phases/00_INDEX.md` mark every phase TODO while 68 `.ir` fixtures run in CI. `ESCAPE_PLAN.md:3` says phases 1–6 landed; `escape.h` says phase 2, no-op unless `ifa_escape_in_fa`, default 0 in `common/fail.h:17`. | new |
| E4 | **The gate that matters is not in CI.** 129: "the pyc suite is blind to this area." The corpus `check` sweep is the real gate, 11 minutes, by hand. A nightly job diffing per-program verdicts against the last committed TSV catches what the issues describe catching by hand. Also: 38 asserts in 14k lines, and IFA.md §11.2 lists invariants known and not asserted (`contour_is_entry_set` vs the pointer's type; `edges` vs `out_edges`). | new |
| E5 | **133 probes, 47 in no markdown, no index.** AGENTS.md's "locate before acting" names six; the rest are found by grepping `getenv`. One registration macro feeding a table printed by `pyc --list-probes` fixes discovery and exposes the dead ones. | new |

## Plan, by value against effort

1. **Close A1, A2, A3.** Small. Stop condition: a suite test for each;
   corpus verdicts unchanged.
2. **D1, structured diagnostics with the backtrack attached.** Medium.
   Stop condition: every corpus refusal and warning is classified, and an
   agent can act on the temporal ones by editing the program under
   `PYC_CHANGES.md`.
3. **B1, the counterfactual setter demand.** Design. Unblocks C4.
4. **D2, the decision snapshot.** Medium-large. Stop condition: a second
   compile of an unchanged program converges in one pass with identical C;
   a one-function edit reconverges in fewer passes than cold and gives C
   identical to cold.
5. **C1/C2, pass-1 width.** Large. Stop condition from 186: othello3 pass 1
   under 400 s with the union's holders unchanged, else re-profile.
6. **D3, reentrancy step 5.** Medium, mechanical. Stop condition: two
   `FA::analyze` calls on two threads equal two processes.
7. **E1–E5 hygiene batch**, and retry 094 at HEAD. Small each.
8. **E4, nightly corpus sweep in CI.** Small.

## Evidence

Line numbers against `829f43d`.

| claim | source |
| --- | --- |
| loop, reset-before-pass, worklist order, reanalyze ordering | `fa.cc:13839–14006` `analyze_to_convergence`; `14008–14113` `FA::analyze` |
| stage cascade, dead-stage comments, stall guards | `fa.cc:11649` `run_split_stages`; `12107` `extend_analysis`; `fa.h:16–52` |
| propagation and deferral | `fa.cc:283–425` |
| lattice cost | `fa_lattice.cc:341–401`; `fa.h:102–119` |
| persistent decisions, ledger keys | `fa.h:543–585`, `720–785` |
| levers and defaults | `fa_flags.cc`, `fa_flags.h` |
| function sizes | `split_owner_of_demanded` 8949–10057; `decide_entry_set_split` 5551–6371; `creation_point` 466–942 |
| globals and statics | `fa.cc:29,43–45,1815,10260,13181`; 17 `static` definitions |
| frontend boundary | `ifa/ifa.h:61–178`; 14 callback sites in `fa.cc` |
| diagnostic output | `fa_debug.cc:347` `show_violations`; `fa.cc:3740–3787` |
| corpus state | `sweeps/check__default__9ef6d5b1+f400843f.tsv` |
| issues read in full | 102, 119, 124, 129, 146, 170, 185, 186, 190; heads of 010, 025, 039, 050, 072, 094, 134, 165, 178, 191 |

The same review, as a page: <https://claude.ai/artifact/PEsN5Wb6yyioJFBJNZCuhP>.
