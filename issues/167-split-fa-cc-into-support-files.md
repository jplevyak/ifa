# 167 — fa.cc is 16.8k lines; split the stable parts out and leave the algorithm

**Status: steps 1 and 2 LANDED** (2026-09-25). Steps 3-5 are still open.
`fa.cc` is **14,605 lines**, down from 16,832 when this was written.

**Related:** [146](146-remove-all-arbitrary-splitting.md) (the audit whose
levers dominate the file), [129](129-plan-demand-driven-creation-set-splitting.md)
and [128](128-cs-identity-over-discriminates-vs-element-type.md) (the CS
identity work that must stay in fa.cc because it is still moving).

## The measurement

`ifa/analysis/fa.cc` is **16,788 lines** — 62% code, 34% comment — holding
**370 top-level definitions** and **115 file-scope statics**. It has 409
`fprintf(stderr, ...)` and 229 `getenv(...)` sites. For scale, the whole
rest of `ifa/analysis/` is 4,300 lines.

By concern:

| concern | funcs | lines | verdict |
| --- | ---: | ---: | --- |
| diagnostics / reporting | 45 | 2765 | **move** |
| env-lever accessors + their rationale | 45 | 1509 | **move** |
| type lattice algebra | 11 | 347 | **move** |
| constraints / primitive semantics | 8 | 1347 | **move, partly** |
| CS identity / keys | 16 | 2080 | STAY |
| split ladder | 8 | 913 | STAY |
| demand / confluence | 6 | 663 | STAY |
| violations / representation | 4 | 564 | STAY |
| pass driver | 14 | 655 | STAY |
| everything else (200 mid-sized fns) | 200 | 5603 | STAY for now |

## The rule for what moves

**Move what is finished. Keep what the remaining corpus failures will
have to change.**

32 of 77 `shedskin_examples` programs still fail to compile. Every one of
them is a question about *contour identity* — which CreationSet a value
lands in, which EntrySet a demand can split, whether a union is
representable. That machinery must stay in one file where it can be read
as a whole, because that is the thing still being rewritten. What has
stopped moving — the lattice, the diagnostics, the flag plumbing, the
per-primitive constraint semantics — is what should leave.

`escape.cc` is the precedent and it worked: 423 lines, its own header,
its own unit test (`ifa/testing/escape_test.cc`), and it has needed no
attention since. Each step below should end the same way.

## The one thing that makes this harder than it looks

**The diagnostics are not separable by a text move.** Of the 115
file-scope statics, roughly 70 are census counters — `esl_hit`,
`mint_child_cmc`, `tc_skip_lval`, `rd_declined_related`, `ed_seen` — that
are *incremented at decision points inside the algorithm* and printed
somewhere else entirely. Moving the printers without the counters breaks
the link; moving both drags the algorithm along.

So step 1 is not "move the printers". It is: give the counters one home,
a `FACensus` struct reached through `fa`, so the increment sites keep
working and the printers stop needing file-scope access. Only then does
the printer move become mechanical.

This is exactly how the dead counters in this area went unnoticed: seven
of them (`ed_split`, `mint_report`, `fs_demands`, `fs_split`, and the four
`stage2_*_time` accumulators) were incremented-but-never-read or
read-but-never-written, and nothing in a 16.8k file made that visible.

## Sequence

Each step is independently landable, each ends with `make test` green and
a `check` corpus sweep showing an unchanged status column. **No step may
change behaviour**; a step that does has gone wrong.

**1. `fa_census.{h,cc}` — the counters. DONE.** ~70 statics into one struct.
Mechanical, no logic moves, and it is what unblocks step 2. Verify: the
`IFA_DBG_*` outputs are byte-identical before and after on two corpus
programs.

**2. `fa_debug.cc` — the printers. DONE.** ~2,765 lines: `dbg_es_per_fun` (592),
`show_violations` (281), `dbg_dump_contours` (158), `report_demand_ratio`,
`report_mixed_element_owners`, `report_creation_attribution`, and the 40
smaller ones. Verify: same as step 1, plus the existing
`ifa/testing/print_*.cc` helpers keep building.

**3. `fa_flags.{h,cc}` — the levers.** All 45 `*_enabled()` accessors and,
more importantly, the measured rationale attached to each. That prose is
the most valuable documentation in the tree and it is currently scattered
through 16k lines at the point of first use. One file, alphabetical, is
strictly easier to audit against [146](146-remove-all-arbitrary-splitting.md)'s
running list of what has been removed and what is left. Verify: `git diff`
shows only moves.

**4. `fa_lattice.cc` — the type algebra.** `type_union`, `type_diff`,
`type_intersection`, `type_cannonicalize`, `type_num_fold`, `coerce_num`,
`subsumed_by`, `qsort_pointers` (347 lines). Already covered by
`ifa/testing/lattice_test.cc`, so this one has a real safety net and
should go first if a smaller proof-of-concept is wanted before step 1.

**5. `fa_prims.cc` — the primitive constraint semantics.** The
`P_prim_*` switch: what `merge_in`, `len`, `index_object`, `coerce`,
`isinstance` do to types. ~1,300 lines, stable, and a per-primitive
table is far easier to check against the runtime's own behaviour when it
sits next to nothing else. Leave `add_send_edges_pnode`'s edge and
dispatch logic behind — that is call-graph construction, not primitive
semantics, and it is still changing.

**After 1-5, fa.cc is roughly 10k lines** and holds contour identity, the
split ladder, demand/confluence, representation, and the pass driver —
which is the thing the remaining 32 programs are about.

## Not proposed

- **Splitting the split ladder itself.** The stages are read as a
  sequence and their interactions are the subject of 146 and 157. Putting
  them in separate files would hide exactly what needs to stay visible.
- **Splitting `creation_point` / CS identity.** 128 and 129 are actively
  rewriting it.
- **A header-per-concern for the internals.** The moved files should
  expose the narrowest possible surface; if a move needs ten new
  declarations in `fa.h`, that is the signal it is not finished moving.

## Already done (2026-09-25)

The survey that produced this plan also removed what it found dead. All
of it was invisible at 16.8k lines and none of it changed behaviour:

- `build_setter_mark` — a recursive function whose only caller,
  `build_setter_marks`, no longer exists; it called only itself.
- `partial_application` in `make_period_closure` (declared, never read)
  and in `make_closure` (a second name for `pn`, same expression).
- `ed_split` — declared and printed, never incremented, so `ESDEMAND`
  reported a permanent `split=0`.
- `mint_report`, `fs_demands`, `fs_split` — incremented, never read.
- `stage2_closure_time` / `_diag_` / `_collect_` / `_split_time` — summed
  and printed under `if (sum > 0)`, with nothing anywhere adding to them,
  so the whole `mark_type sub-phases` line was unreachable.
- `csm_used`, `csm_split`, `nnil`, `first_pass_full_reset` — write-only.
- A stale comment in the ifa/157 census describing the `split_edges`
  action as what the code does, directly above the note recording that it
  was MEASURED DEAD and removed.
- **Seven `STAGEDELTA` diagnostics emitted `\\n` — a literal backslash and
  `n` — instead of a newline**, so the per-stage measurements that 146 and
  157 are read from ran together on one line.

Eight compiler warnings in `fa.cc` went to zero, and the suite is
unchanged at 318 passed / 0 failed on both backends.

**Deliberately kept: every tombstone.** `fa.cc` carries a lot of "X was
tried, here is the measurement, it is dead" — ifa/157's "SPLIT COARSER
WAS TRIED HERE AND IS DEAD", 146 C's removed fan, the deleted
`CARTESIAN_PRODUCT` stage, `PYC_RECVFAN`, `PYC_CSKEYSETS`. Those are not
stale comments; they are the record that stops the next person repeating
a measured failure, and AGENTS.md is explicit that a negative result is
the deliverable. They stay, and they should move with their code.


## Steps 1-2 as landed (2026-09-25)

**Step 1 — `fa_census.{h,cc}`, 101 counters.** The classification was
mechanical rather than by eye: a counter moves only when EVERY read of it
is a printf argument or a guard on a printf. Seven statics failed that
test because they are read to DECIDE something, and stayed —
`cur_split_stage`, `ifa_selective`, `cur_split_type_only`,
`fa_selective_armed`, `cselem_shape_memo_pass`, and the two
`bt_noms_*_pass` that gate reanalysis.

**Step 2 — `fa_debug.cc`, 41 of the 42 printers, 2,196 lines.**
`dbg_es_per_fun` (591 lines) is the one that stayed, and the reason is
the useful part: it calls `clear_splits`, `build_joint_type_marks`,
`collect_type_confluences` and `clear_marks`. It does not report on the
analysis, it RE-RUNS stage machinery to report on it — an instrumented
pass wearing a diagnostic's name. Moving it would have dragged the
splitter into the diagnostics file, which is the thing this plan exists
to prevent.

**The seam is `fa_internal.h`, and its size is the number to watch.** It
is 159 lines: the 41 diagnostic entry points, plus **26 fa.cc internals**
the diagnostics read (`foreach_avar`, `element_census`, `mixed_basics`,
`atype_irrepresentable`, `CSFlowGraph`, `compar_tv`, …). `fa.h` — the
public header — did not change at all, which was the actual constraint
behind this plan's "if a move needs ten new declarations" rule; the rule
was about the PUBLIC surface, and an internal seam is the right vehicle
for the rest. If that 26 grows, the thing being reported on has probably
been put in the wrong file.

**Verification, both steps.** Every `IFA_DBG_*` stream compared
BYTE-FOR-BYTE against a binary built from the parent commit: nine
diagnostics (`CSROUTES`, `DEMAND`, `ESDEMAND`, `CSMINT`, `RETCONF`,
`KEYDRIFT`, `ELEMCONF`, `CSVARS`, `CONTOURS`) on `sieve` and `chess`,
eighteen comparisons, all identical. `make test` green and unchanged
throughout: 318 passed / 0 failed on both backends.

| file | lines |
| --- | ---: |
| `fa.cc` | 14,605 |
| `fa_debug.cc` | 2,196 |
| `fa_internal.h` | 159 |
| `fa_census.h` | 87 |
