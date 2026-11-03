# 194 — review of `ifa/codegen/`: one backend carries the safety nets, codegen traps stand in for refusals, and representation has three owners

**CLOSED 2026-11-03: distributed.** A1, A3, A5, D2, D3 → [200](../200-llvm-backend-parity.md); A2 → [102](../102-codegen-detected-conditions-are-refusals-or-demands.md); A4, C1, C2, C3 → [201](../201-representation-oracle.md); A6 → [118](../118-union-field-representation-and-polymorphic-field-offset.md) (unchanged); B1, B2, B3 → [199](../199-emitted-code-performance.md); D1, D4, D5 → [202](../202-health-docs-levers-probes-tests-and-ci.md). The review itself is the record.

**Status: open. Umbrella / audit**, like [146](146-remove-all-arbitrary-splitting.md).
Written 2026-11-03 against `8f0d334`. Static reading plus counts over the
sources, fixtures and sweep records; the compiler could not be built in
the reviewing container, so nothing here is a new measurement. Companion
to [192](192-review-analysis-and-issues-2026-11.md) (analysis) and
[193](193-review-optimize-against-analysis-2026-11.md) (optimize). Close
this issue by filing or fixing each row and striking it.

## Verdict

Two backends of unequal maturity share a thin common layer. The C
backend carries every safety net built since June: the layout contract,
slot elision, the nil-union casts, the refusal of unresolvable sends. The
LLVM backend carries almost none of them. The reference docs an agent
reads first describe an intermediate representation deleted in June.
Underneath both, eleven codegen-detected type mismatches still become
run-time traps in the default mode, the shape
[102](../102-codegen-detected-conditions-are-refusals-or-demands.md) named as the
defect and fixed for one case.

## State

| | |
| --- | --- |
| code | 12,798 lines: cg.cc 4,235 · cg_emit_llvm.cc 5,280 · codegen_common.cc 2,047 · llvm.cc 556 · jit.cc 117 · headers 563 |
| largest functions | `write_c_prim` 958 (one switch); `emit_send_call_impl` 876; the `CBackendEmitter` class body ~1,000 |
| fixtures | 6 `codegen-c` programs with goldens; the `codegen-llvm` IR phase was retired with v1 and not re-created; both backends run the full e2e suite in CI |
| run-time trap sites | 24 in cg.cc, 11 gated on `fruntime_errors` (the default), 12 distinct messages |
| corpus | C 58 / LLVM 54 of 77 compile, on different trees; on 2026-10-02 the failing sets were identical (sweeps/INDEX.md:191) |
| docs naming deleted files | 86 references to `llvm_codegen.cc`, `llvm_primitives.cc`, `cg_normalize_v2`, `cg_ir_v2*`, `make_prims.cc` |
| planning docs | 2,352 lines outside `archive/`, all dated June 2026; `CG_IR_SKETCH.h` is 824 lines included by nothing |
| last change | 2026-10-18 (LTO-linked runtime, `nsw` / `noundef`); 12 commits since September |

## What each backend runs

| pass | decides | C | LLVM |
| --- | --- | --- | --- |
| `cg_elide_unread_slots` | drops members no live read, write or dispatch touches (closed/123: 93–98 % of method slots) | yes | **no** |
| `assign_type_cg_strings_pass1/2` | every Sym's `_CG_*` name; `T \| None` collapses to `T` | yes | yes |
| `cg_build_new_to_val_map`, `cg_build_dispatch_arm_plans` | slot stores per constructor; how each class-tag arm calls (closed/121) | yes | yes |
| `cg_note_blind_cast` at field read, store and dispatch arms | the (cast-to, actual, slot) obligations | yes | **no** |
| `cg_check_layout_contract` | refuses a blind cast whose prefix layouts disagree (closed/122, 123) | yes | **no** |
| `cg_scan_imprecise` / `cg_check_imprecise` | `--refuse-imprecise`, by C type string | yes | yes, over strings it never emits |
| nil-union casts (`cg_needs_nil_union_cast`) | `{None, int}` into a voidish slot gets a cast (issues/048, closed/140) | yes | n/a |
| compile driver | `clang++ -std=c++23 -O2 -fno-strict-aliasing -fno-builtin-pow` via `Makefile.cg` | yes | `clang++ -O2`, LTO bitcode runtime when lld is present |

## What is strong

- **Dispatch resolution is shared and the rule is enforced**:
  `poly_dispatch_classtag_targets`, `poly_dispatch_directly_owned`,
  `poly_dispatch_is_nil_receiver`, `poly_dispatch_arity_plan`,
  `nil_tested_target` are one implementation both emitters call (AGENTS.md's
  "call it, don't restate it" was learned here, closed/026).
- **The layout contract is the right kind of check**: syntactic, recorded
  by the emitters that write the casts, fatal in every mode with the reason
  beside it; it found go's crash and bh's heap corruption and costs nothing
  after 186's dedup (896 obligations on othello3).
- **Refusals are explicit and shared**: `codegen_unresolved_send` (102),
  `cg_fail_unrepresentable_container_union` (issues/018),
  `emit_send_unhandled` declining as a compile error (closed/181).
- **The compile flags are reasoned**: `-fno-strict-aliasing` because the
  object model is blind casts; `-fno-builtin-pow` because nbody diverged;
  `-g` off after 23 GB on othello3.
- **Deterministic C output** (closed/112, closed/147), which is what makes
  every corpus A/B trustworthy.
- **A nullable receiver is a null test** (190); tuples dispatch by header
  bits (185, 102's layout plan).

## A. Correctness and contract

| # | finding | severity | owner |
| --- | --- | --- | --- |
| A1 | **The LLVM backend has no layout contract.** `cg_note_blind_cast`, its dedup set and `cg_check_layout_contract` are static in `cg.cc` (255–510), called from the C field read (979), store (1027) and dispatch arms (2815–2882), checked at 4175. `cg_emit_llvm.cc` records no obligation and runs no check, though its `resolve_union_receiver` performs the same blind casts. The bug class closed/122 and closed/123 made fatal on C is unguarded under `-b`. Fix: move the three functions and their state to `codegen_common`, record at the LLVM field and dispatch emitters, check at the end of `cg_emit_llvm`. | defect | new |
| A2 | **Eleven codegen-detected type mismatches become run-time traps in the default mode.** 102 stated the principle, refused the unresolved send in every mode, and left the others. `cg.cc` tests `fruntime_errors` at 11 sites and, when set, writes `assert(!"runtime error: …")` into the program: field type mismatch on read (975–977) and store (1023–1025), getter not resolved (985–988), list index type mismatch (1105–1107, 1293), list element type mismatch (1241–1244, 1291, 1329–1332), primitive operand type mismatch (2180–2181), call argument type mismatch (2292–2298), bad getter (1170–1171). Three trap unconditionally: None coerced to floating-point (1789), unresolved if condition (3119), jump to unreachable block (2999, 3078, 3087), send does not complete (3170). The LLVM backend mirrors the shape (`emit_salvage_trap`, 1177). Each is a comparison of C type strings at the emission site, a representation fact FA did not observe; by 102's argument each is a demand FA can answer or a program to refuse, never a binary that aborts later. | contract | 102 |
| A3 | **Four decisions are implemented twice, one deliberately.** `resolve_union_receiver` (cg.cc:232, cg_emit_llvm.cc:731, whose comment says "port of cg.cc:148"); `is_unresolved_condition` (3016 / 4246, the LLVM comment: "deliberately kept independent, matching how the two backends already duplicate `const_if_successor`"); `const_if_successor`; the class-tag eligibility filter (`cg_has_classtag(rt) && cg_get_string(rt)` vs `rt->name && !rt->is_system_type && cg_has_classtag(rt)`, codegen_common.h:158–162). 030 records the last two as known divergences with no repro; unnamed lambda candidates make LLVM bail a whole site. | divergence | 030 |
| A4 | **Layout widths, imprecision and coercion are decided by the C type string.** `cg_ctype_width` is a 23-entry `strcmp` table plus `strchr(t, '*')` (433–451); `cg_layout_mismatch` compares those; `cg_scan_imprecise` asks whether `c_type(av)` is `_CG_any` (672–748); the coercion emitter tests `scalar_ct` and width 8 (2235–2238); `simple_move` tests `!strcmp(rt, "_CG_nil_type")` (1787). 124 residual 1 was exactly this failure (a list clone with no C name read as `_CG_any`), and 124 residual 3 asks for FA types. The LLVM backend runs the scan over strings it never emits (llvm.cc:136). | by string | 124 |
| A5 | **LLVM stores an int's raw bits into a float64 slot** (093). Confirmed in `emit_move` (cg_emit_llvm.cc:2440–2495): int↔int `SExtOrTrunc`, pointer↔pointer no-op, no `sitofp`. | open, small | 093 |
| A6 | Of the 14 `tests/*.known_issue` sidecars, one is codegen-owned (`bool_or_none_fallthrough`, 118, refused by clone's `determine_layouts` at clone.cc:1150); the rest are analysis or frontend. 079 is a cleanup; its crash is refused by FA since closed/158. | scope | 118, 079 |

## B. Emitted code and cost

| # | finding | owner |
| --- | --- | --- |
| B1 | **No baseline for the emitted code's speed; the compile-time baseline is stale.** `PERFORMANCE.md` is pyc's own wall time on five small tests, 2026-06-11, clang 22 nightly, before `OPTIMIZE=1` was the default. The sweep records compile and run status, never time. The only emitted-performance datum in the tree is othello3's `moves/sec` in 186 (80 M vs CPython 0.77 M), seen by accident. The cost of 030's dispatch chains, 054's tuple header, `-fno-strict-aliasing` and the LTO runtime is unknown. A `corpus_sweep.sh -m bench` over the programs that match CPython, both backends, committed like the other TSVs. | new |
| B2 | 030 (dispatch chains; 186 collapsed the 830-arm case to one slot call, the rest are 190's 71 legitimate sites) and 054 (tuple header; the issue itself says the decision belongs to FA's representation layer) are both §C's subject. | 030, 054 |
| B3 | **The "C" backend emits C++23** (`Makefile.cg`: `CC = clang++`, `-std=c++23`) and pays for the object model with `-fno-strict-aliasing`; the LLVM backend emits no TBAA for the same reason. The right price today, and the measurement to take before touching the object model. CODEGEN_C.md §11.4 still says `cc`. | new |

## C. Representation belongs to one owner

Today: clone lays out (`determine_layouts`, `PYC_PREFIX_LAYOUT`,
`PYC_CLASSEQ`, the `{bool, None}` refusal); codegen decides what is tagged
(`cg_has_classtag`), which slots exist (`cg_field_live`, elision), which
casts are sound (the contract), what has a C name, and which candidates
can be told apart (the plans); FA asks its own four versions
(`atype_irrepresentable` fa.cc:4714, `mixed_basics` 4117,
`scalar_with_pointer` 4148, `nil_member_is_representable` 2517), and keeps
arity in CreationSet identity (closed/132). 102's inventory found four of
the dispatch plan's inputs post-FA and stopped; 054 says the header
decision belongs in FA; 124 says the imprecision check should read FA
types; closed/132 says representation is legitimate identity behind
`IFACallbacks`.

| # | change | owner |
| --- | --- | --- |
| C1 | **A representation oracle in `codegen_common`**, keyed on the Sym or AType alone: width and pointer-ness, tagged or untagged, nil-able as null (`{None, T*}`) or not (`{None, int}`), fixed arity or header-bearing, and discriminability over the representation routes (tag, null, arity header, layout bits). The C-string table becomes a Sym property; the contract reads widths from it; `cg_scan_imprecise` asks it; the LLVM backend stops computing strings it never emits; FA's four predicates ask the same oracle, which is the demand channel 102 looked for. Stop condition from 102: if no refusal changes outcome it is a cleanup, kept for the LLVM layout contract alone. | 102, 124, 054 |
| C2 | **Slot elision is liveness; let the analysis side own it so clone emits one layout.** `cg_elide_unread_slots` (cg.cc:3407–3822) walks the live call graph and nulls member types in place, on one backend, after clone has laid the class out. Computed in `mark_live_code` (which walks those PNodes already) and consumed by `determine_layouts`, the member would not exist, both backends would agree, and the contract would never see a `<placeholder>`. 192 §C5 is the same direction from the other end. | closed/123 → new |
| C3 | **`IF1_EMISSION_CONTRACT.md` says codegen never does dataflow; it reads analysis state in 20 places** (codegen_common.cc 17: slot specificity in `cg_build_new_to_val_map` is "fewer CreationSets in the ES", `cg_scan_imprecise`, `cg_report_slot_use`; cg.cc 3). None is wrong; each is a representation question answered from analysis state because nothing flattened it. They go into C1. | new |

## D. Code and doc health

| # | finding |
| --- | --- |
| D1 | **The reference docs describe the deleted v2 architecture, and INDEX.md calls one definitive.** `INDEX.md:89–95` lists "CG_IR_SEMANTICS.md — definitive semantics … Implementation in codegen/cg_ir_v2*.cc/.h" (deleted; the link goes into `archive/` without saying so). `CODEGEN_LLVM_CONTRACT.md:17–24` says it is "the specification" for `cg_normalize_v2.cc` and `cg_ir_v2_emit_llvm.cc` (deleted; the IF1 → CGv2Program → LLVM design was abandoned by `LLVM_DIRECT_EMIT_PLAN.md`). `CODEGEN_LLVM.md` cites `llvm_codegen.cc` / `llvm_primitives.cc` throughout and says the runtime is not linked (§14.5; LTO-linked since 2026-10-18), the JIT is unimplemented (§14.6; `jit.cc`, `-j`), and `get_target_fun` falls back to a name match (§14.8; gone). `CODEGEN_C.md` §11.3 says a polymorphic call aborts at run time (class-tag dispatch correct since July), §11.4 says `cc`. `ARCHITECTURE.md:82–89` lists `llvm_codegen.cc`, `llvm_primitives.cc`, `make_prims.cc`; `llvm_internal.h:17` names `cg_ir_v2_emit_llvm.cc` as the production path. Six June planning docs (`AUDIT`, `CODEGEN_PLAN`, `CG_IR_PARITY_PLAN`, `FFI_LIBRARY_MIGRATION`, `CG_IR_019_DESIGN`, `LLVM_DIRECT_EMIT_PLAN`) describe the pre-reset state. Fix: one short `CODEGEN.md` that says what is true now; archive the six and the sketch header with a dated note; rewrite file names and gotchas in the two reference docs and INDEX.md. |
| D2 | **The emitter seam is half used.** `VirtualCGEmitter` (codegen_common.h:358–405) has twenty hooks so both backends dispatch primitives the same way; the C backend overrides `emit_send_any_prim` and routes everything through `write_c_prim`'s 958-line switch, so the per-operation hooks are dead on that side, while LLVM implements them one by one (`emit_send_call_impl` 876, `emit_send_primitive` 524). Split `write_c_prim` along the hook boundaries, or say in the header that the seam is LLVM's structure only. |
| D3 | **No LLVM unit fixture and no determinism oracle.** `codegen-c` has six programs; `codegen-llvm` was retired with v1 (print_codegen.cc:108–140 keeps the IR normaliser). closed/112 and closed/147 were done on C output; nothing states two runs of `-b` agree, and the emitter holds eight `std::unordered_map` / `std::set` containers keyed on pointers. |
| D4 | **26 environment variables, no ledger**: nine behavioural (`PYC_ELIDE_SLOTS`, `PYC_ELIDE_LIMIT`, `PYC_DISPATCH_STUB`, `PYC_DISPATCH_ARM_PLANS`, `PYC_LLVM_LTO`, `PYC_LLVM_CXX`, `PYC_NO_GC`, `PYC_VERIFY_EACH`, `PYC_DUMP_LL`), seventeen probes, none with the measurement-beside-the-default `fa_flags.cc` requires. |
| D5 | **Thirteen by-name decisions, all at the primitive ABI** (`print`/`println`, `__pyc_to_str__`, `__pyc_c_call__`, `__pyc_format_string__`, `__pyc_sleep__`, `__pyc_net_wait_*`, `__closure__`; `emit_send_primitive` constructs `_CG_<name>`). The names are the contract, which is defensible; a flag on `RegisteredPrim` would be more so than a `strcmp` in two emitters (closed/083 was the collision). |

## Plan, by value against effort

1. **A1**: the LLVM layout contract, mechanical. Stop condition: `make test`
   green on both backends and the corpus `-b` compile set unchanged; a
   program the LLVM check refuses and the C check accepts names a
   divergence, which is the finding.
2. **D1**: rewrite the docs to the tree that exists. Stop condition: zero
   references to deleted files outside `archive/`.
3. **A2**: the eleven traps become refusals or demands, one at a time with
   a corpus `check` sweep: a trap no program reaches is a refusal; one a
   program reaches is filed against the analysis with its backtrack. Stop
   condition: no program that matched CPython stops compiling without an
   issue naming the demand.
4. **D3**: a `codegen-llvm` phase over the six programs, and two runs of
   `-b` diffed. Stop condition: byte-identical `.ll` across two runs and
   two builds.
5. **A5 + A3**: `sitofp` in `emit_move`; one `resolve_union_receiver`,
   one `is_unresolved_condition`, one `const_if_successor`, one eligibility
   filter in `codegen_common`. Stop condition: 030's divergences gone, a
   fixture each.
6. **C1**: the representation oracle. Stop condition from 102.
7. **C2**: elision into liveness and clone. Stop condition: both backends
   emit the same member set per class, zero placeholders in the contract,
   closed/123's numbers hold.
8. **B1**: `-m bench`. Stop condition: a repeat on an unchanged tree agrees
   within noise.

## Evidence

Line numbers against `8f0d334`.

| claim | source |
| --- | --- |
| C pass order and epilogue checks | `cg.cc:3960–4178`, `4188–4235`; `Makefile.cg` |
| LLVM pass order, no contract, no elision | `cg_emit_llvm.cc:394–432,5193–5280`; `llvm.cc:116–200,405–500`; `grep cg_note_blind_cast\|cg_elide ifa/codegen/cg_emit_llvm.cc` → none |
| layout contract | `cg.cc:255–510` |
| permissive traps | `cg.cc` at the lines in A2; `grep -c fruntime_errors ifa/codegen/cg.cc` → 11 |
| duplicated decisions | `cg.cc:232,3016`; `cg_emit_llvm.cc:731,4229,4246`; `codegen_common.h:140–162` |
| string-keyed representation | `cg.cc:81,92,362,433–451,672–748,1787,2235` |
| shared resolver and plans | `codegen_common.h` (all); `codegen_common.cc:259–1660` |
| emitter seam | `codegen_common.h:358–413`; `codegen_common.cc:1842–1930`; `cg.cc:760,1966`; `cg_emit_llvm.cc:3074,3353` |
| slot elision | `cg.cc:3407–3822`; `codegen_common.cc:236` |
| 093 in emit_move | `cg_emit_llvm.cc:2440–2495` |
| analysis state read by codegen | `grep -c 'AVar\|->avars\|->out->\|fa->ess\|fa->css' ifa/codegen/*.cc` → 17 + 3 |
| doc drift | `INDEX.md:89–95`; `CODEGEN_LLVM_CONTRACT.md:1–40`; `CODEGEN_LLVM.md` §§4–9, 14; `CODEGEN_C.md` §11; `ARCHITECTURE.md:82–89`; `llvm_internal.h:15–17` |
| tests and sweeps | `ifa/tests/ir/codegen-c`; `ifa/testing/print_codegen.cc`; `sweeps/compile__PYC_LLVM_1__*`; `sweeps/INDEX.md:191–198` |
| issues read | 030, 054, 079, 093, 102, 118, 190 in full; closed 112, 121, 122, 123, 147 by their README rows |

Not done: no build, so no run of either backend. Whether the LLVM output
is deterministic is unverified here; plan item 4 is the verification.

The same review, as a page: <https://claude.ai/artifact/CSm2mLCC5PXtDJz2Uc2mMV>.
