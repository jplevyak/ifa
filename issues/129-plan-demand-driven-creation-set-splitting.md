# 129 — THE PLAN: demand-driven CreationSet splitting

**This is the single integrated plan.** [128](128-cs-identity-over-discriminates-vs-element-type.md),
[131](131-demand-driven-constant-splitting.md),
[133](133-split-a-container-on-its-element-type.md),
[134](134-remove-the-frontend-forced-split-opt-in.md),
[146](146-remove-all-arbitrary-splitting.md) and
[151](151-split-an-entryset-on-a-constant-argument-on-demand.md) are steps
or constraints inside it; none of them carries a competing plan. If one
seems to, this file wins and the other should be corrected.

*Rewritten 2026-09-12. The previous 2886-line chronological record is in
git history; its numbers were from 2026-09-05 and were badly stale.*

## The goal, and the one lever

CLAUDE.md: IFA starts from the **minimum** contours and splits only on
demand. pyc meets this on the EntrySet side and not on the CreationSet
side — `creation_point` keys identity on *(allocation site × contour)*, so
data contours start MAXIMALLY split and never merge (`multidef=0`
corpus-wide: every CreationSet has exactly one creation point).

**The lever is `PYC_CSDCPA1=2` — start merged, one CreationSet per sym.**
Making it the default is the goal of this plan. It is the only thing
measured that moves the number, and it takes `ess` DOWN rather than up.

**The design it implements, in three steps:**

1. **Coarse identity by construction** — `dcpa1`. Not "reuse": one contour
   per sym, needing no evidence. (Why mint-time reuse keying is a dead
   frame: 128, "Where reuse fits".)
2. **Demand-driven separation** — give the precision back only where
   something observed a distinction and could not proceed.
3. **A ledger** so a re-derived separation re-attaches to the contour it
   first made instead of minting a fresh one. Matters MORE the coarser you
   start (`ledger_find_cs`/`ledger_add_cs`,
   `find_or_make_filtered_entry_set`).

## Where it stands — measured 2026-09-12, not inherited

| | default | `PYC_CSDCPA1=2` |
| --- | --- | --- |
| suite | **318 / 0** | 315 / **3** |
| corpus compile failures | **2** (othello3, rdb) | **7** |
| programs with warnings | 36 | **33** |
| container CS / shapes | 2768 / 627 = **4.41** | 2138 / 629 = **3.40** |
| `pratio` | 2.98 | **2.31** |

**−23% container CreationSets, and FEWER warning programs than the
default.** This is much closer than the record suggested: the bill was 16
suite failures and ~20 corpus failures when this plan was written, and it
is now 3 and 7.

## The remaining bill, attributed

**Suite — 3, of which 1 is bookkeeping:**

| test | owner |
| --- | --- |
| `splitter_mark_type` | **benign.** Only the `STAGES` line differs (`TYPE_CONFL SETTER` vs `TYPE_CONFL`); same warning, same `CALLS: direct=69 dynamic=0`. Re-bless at the flip. |
| `listcomp_element_separation` | [133](133-split-a-container-on-its-element-type.md) — fixed by `PYC_ESBLOCK`, which is opt-in because it costs plcfrs/sudoku5 through 146 E |
| `arity1_literal_shares_contour` | 133's separator question — arity-1 literals whose slot types differ but whose union is representable, so no demand is raised at the merge |

**Corpus — 7, of which 2 (othello3, rdb) also fail at the default. The 5
flag-only failures are two groups:**

| group | programs | first error | owner |
| --- | --- | --- | --- |
| element/slot union with no representation | plcfrs, sudoku3, sudoku5 | `'x' has mixed basic types: (…)`, two of them naming `tuple` | [146](146-remove-all-arbitrary-splitting.md) E |
| layout / blind cast | chull, sudoku4 | `object layout: 'Edge' is blind-cast to 'Vertex' … member width differs` | [135](135-empty-sibling-contour-wins-the-clone-merge.md) |

So **two mechanisms stand between here and the flip**, not a long list.

## The work, in order

**1. 146 E — separate a formal that holds N tuple CreationSets.** The
critical path. One shared, UNROLLED `tuple.__eq__`/`__lt__` contour serves
many tuple shapes; its single `x` per slot position merges their slot types
into `{int64, str}`, and every use is a BOXING violation. This is what the
removed CARTESIAN_PRODUCT splitter did by fanning, and
`tests/splitter_cartesian_product.py` already specifies what the
demand-driven replacement must be: demand (an unresolved dispatch or an
irrepresentable union) plus dispatch-aware filtering of the RECEIVER — the
position that decides dispatch in single-dispatch OO — never a fan over
receivers.

Clears plcfrs, sudoku3, sudoku5, and unblocks `PYC_ESBLOCK`, which clears
`listcomp_element_separation`.

**2. 135 — the empty sibling contour wins the clone merge.** Clears chull
and sudoku4. Layout is a REPRESENTATION property, so this is legitimate
territory, not provenance.

**3. 133's separator question.** Arity is currently the ONLY working
separator for list literals, and it is doing all the work alone. The
demand is unobservable at the moment of the merge and the merge is
unrecoverable at the moment the demand appears, so the answer is not a
better demand test — it is **which other representation properties of a
literal are knowable at construction**. The slot's basic-type kind is the
obvious candidate. Clears `arity1_literal_shares_contour`.

**4. 151 — constants separated on demand.** Not on the flip's critical
path, but constants are the largest identified share of what start-merged
destroys, and it is what lets [134](134-remove-the-frontend-forced-split-opt-in.md)
close (93 `__pyc_clone_constants__` annotations across 8 files, plus the 20
`fa.cc` sites they gate). Its acceptance test is that `bool.__not__`'s
annotation can be removed with ifa/150's corpus result intact.

**5. The flip itself.** Re-bless `splitter_mark_type`, flip the default,
and keep `PYC_CSDCPA1=0` as the escape hatch for one release.

**6. [111](111-FA-selective-invalidation-per-pass.md) — convergence cost.**
A performance lever for the extra passes start-merged costs. NOT a
precondition: that claim was retracted (see below).

## What is settled, so it is not re-litigated

- **A merge CAN be taken back.** `analyze_to_convergence` resets *before*
  each pass, so flow state is re-derived from bottom every pass; what
  persists is the DECISION (`av->cs_map`), not the types. Demonstrated
  twice in code. 128 and 111 are **not** one change.
- **128 is not the blocker and pulls the other way.** An ES split mints
  FEWER CreationSets through `creation_point`, not more; and reducing the
  tuple CreationSet count costs ten corpus programs while making sudoku5's
  comparison contour worse (22 → 32 shapes). 146 E is the blocker.
- **`dcpa1`'s `tuple` exclusion stays.** Arity and position are part of a
  tuple's TYPE. Dropping it was measured on one binary, env toggled: 12
  compile failures against 2.
- **Mint-time reuse keying cannot work.** The key is evaluated when the
  element is unfilled by construction — 402 of 795 such mints never acquire
  a shape. `PYC_CSELEM=3` is suite-clean and still regresses `rdb`.

## Do not restart from these

`PYC_CSELEM=3` (superseded by dcpa1); `PYC_CSSITELESS` (made contours
worse on 4 of 5 programs); `PYC_CSRESPLIT`, `PYC_CSREJOIN` (inert or
superseded); `PYC_CSCALLSITE=3` and `PYC_WALKCTX` (reverted — a fan, and a
wrong premise; see 133 "Do not retry"); 131's cap-strip premise
(falsified by its own step 1 — `cstrip` measured 0/0/0).

## Verification at the flip

- Suite green on **both** backends with the flag on.
- `./corpus_sweep.sh -m check`, compared **per program**: `compile_rc`,
  `run_rc`, `cpy_rc`, `stdout_match`. Totals are not enough — two programs
  can swap and net to zero.
- `ess`/`css` down, not up. This exists so contours can merge safely.
- Same-binary, env-toggled A/B only. A 1–2 program movement between two
  BUILDS is not attributable ([147](147-analysis-result-depends-on-the-binary-not-the-inputs.md)).
