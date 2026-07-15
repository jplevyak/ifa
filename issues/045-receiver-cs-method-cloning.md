# 045 — Receiver-CS-directed method cloning as a precision move

**Status:** LANDED 2026-07-15. Issue 040's repro compiles clean and
runs correctly; tests `tests/empty_list_print.py` and
`tests/range_branch_union.py` (both backends); suites 196/0 e2e +
17/0 ifa-test; fysphun (the many-pass 033 canary) unchanged; full
corpus sweep shows no example-level changes (the e1 shape is not any
example's first blocker).

**What it actually took (three pieces beyond the design below, found
by instrumenting each broken link):**
1. `entry_set_compatibility`: differing constants at a
   clone_for_constants formal were only a scoring PREFERENCE
   (`val -= 1`), so with no competing EntrySet the merged one still
   matched and constants merged anyway -- constant-cloning only ever
   took effect when a downstream violation forced a split. For
   clone_methods_per_cs functions it is now a HARD incompatibility
   (scoped: making it hard for all clone_for_constants functions
   would eagerly fan out list.__getitem__-style contours program-
   wide).
2. The flag had to reach every Fun the matcher can actually match:
   the class's `__new__` wrapper and `__init__` syms
   (gen_class_pyda), AND `default_wrapper`'s Fun sym + formals
   (calls relying on defaults match the wrapper; the compat check
   reads the flag off the MATCHED fun's formal syms).
3. `creation_point`'s split-parent CS reuse funneled both
   per-constant `__new__` contours into ONE instance CS, re-merging
   the field constants the contour split had separated. Instances of
   clone_methods_per_cs classes now skip the parent-reuse.

With those three in place, the ordinary dispatch machinery separates
the method contours per receiver CS by itself for the common case;
the new PER_CS_RECEIVER stage fires only for genuine unions the
dispatcher cannot split (a branch-merged receiver:
`r = range(2) if c else range(3)` -- tests/range_branch_union.py
exercises exactly this and the stage's split makes it fold).

**Latent bug exposed and fixed:** the per-CS splits left a Fun
reachable through call edges but not from top, and
`build_call_dominators` (optimize/dom.cc) allocates Doms for all
`fa->funs` but only traverses from top -- `compute_semi` then ran
`df_eval` on a never-traversed pred (null ancestor chain, SIGSEGV).
Preds with `semi < 0` are now skipped: no semidominator path can run
through a root-unreachable vertex (same universe-mismatch family as
build_cfg_dominators' edge pruning).
**Related:** [040](040-empty-list-shared-clone-type-inference.md)
(the trace that motivates this), [033](033-splitter-non-idempotent-divergence.md)
(the splitter stability rules this must respect),
[043](043-empty-container-inference-options.md) (option survey).

## Problem

FA's splitter is violation-driven: method EntrySets whose receiver
union holds multiple CreationSets of the SAME class are never
separated when no type violation arises inside them — but the merge
still destroys per-CS precision that callers depend on. Issue 040's
verified chain: `range(0,0)` and `range(0,2)` receivers merge in
`__pyc_more__`/`__next__`'s shared ES; `__next__`'s
`self.i += self.s` writes through the shared contour into EVERY
receiver CS's field, widening the empty range's `i` too, so
`i < j` cannot fold false and the empty list's dead `__str__` loop
body gets type-checked (NOTYPE / bogus dispatch).

## Design

**Trigger (opt-in, class-gated):** a new Sym flag
`clone_methods_per_cs`, set by the pyc frontend (`gen_class_pyda`)
on any class whose `__init__` parameters use
`__pyc_clone_constants__` — the author has declared instances'
identities constant-significant, so their methods deserve
per-instance-CS contours. First user: `range` (its `__pyc_more__`
fold-ability is exactly what per-CS field constants buy).

**Mechanism (reuses the existing dispatch splitter):** a new LAST
split stage in `run_split_stages` (`PER_CS_RECEIVER`), running only
when every violation-driven stage found nothing. It scans EntrySets
whose positional args hold >= 2 same-class CSs of a flagged class
and routes their edges through the existing `split_edges` machinery
(per-CS filtered EntrySets via `find_or_make_filtered_entry_set`).

**Why this respects issue 033's stability rules:**
- `split_edges` + `find_or_make_filtered_entry_set` re-FIND existing
  filtered products across passes (no per-pass contour
  manufacturing), and the ledger records the decisions.
- Termination: the split partitions an ES by its arg's EXISTING CSs;
  no new CSs are minted by the split itself. Re-running on the
  products finds single-CS args (nothing to split). The class gate
  bounds the blast radius to explicitly-marked classes.
- Ordering: the stage runs last and only on quiescence, so it cannot
  perturb the violation-driven stages' trajectories within a pass.

**Prerequisite fixed earlier the same day:** `gen_class_pyda` now
propagates `clone_for_constants` from `__init__` params to the
synthesized `__new__` wrapper formals — without that, constructor
constants merge before the instance CS is even created and there is
nothing for per-CS method contours to preserve.

## Verification

1. Issue 040's repro (`b=[2,3]; print(b); k=[]; print(k)`) — compile
   without NOTYPE and print `[2, 3]` / `[]`.
2. Full suite both backends; ifa-test phases.
3. pygasus FA wall-time (the splitter-stress example) within noise
   of baseline; full corpus sweep for regressions.
4. The issue-033 lesson recorded in run_split_stages stage 2 applies:
   short suites are insufficient evidence for extend_analysis
   changes; fysphun (many-pass numeric program) must be explicitly
   checked.
