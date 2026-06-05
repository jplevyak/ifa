# ifa/notes

Design notes — sketches of features or directions that were
considered, prototyped, or partially built but not currently active.
Each file captures the intent, the historical state, and what would
be required to revive the work.

These are *not* open work items (those live in
[../issues/](../issues/README.md), which file concrete bugs or
deferred work tied to current behavior). They're closer to a
backlog of "ideas worth not forgetting": features the original
author sketched, abandoned approaches that may still be the right
answer someday, and breadcrumbs that explain why certain hooks
exist in the IR or the analysis.

The format is borrowed from the ADR / RFC tradition but kept
informal: one numbered markdown file per idea, no required status
field, no expectation that anyone will work on it.

## Conventions

- Filenames: `NNN-short-slug.md`, NNN zero-padded. Pick the next
  number; don't reuse.
- One idea per file. Cross-link with relative paths (use
  `[[link]]`-style links to other notes / issues / docs if helpful).
- When deleting dormant code, write the note **first** with the
  pre-deletion file/line citation and a copy of the key snippet, so
  the note stands alone after the code is gone. Then cite the
  removal commit at the bottom.
- A note may eventually graduate to an issue (if someone decides to
  do the work) or be deleted (if the idea is firmly rejected). Both
  are fine. The note itself doesn't track status.

## Current notes

- [001-compilation-database.md](001-compilation-database.md) —
  on-disk cache of clone/dispatch decisions so the next compile can
  warm-start from the previous run's analysis. The `cdb.{cc,h}`
  sources and the `Fun::prof_id` / `prof_ess` / `es_info` fields
  were the half-built scaffolding; removed June 2026.
- [002-eager-splitting.md](002-eager-splitting.md) — eager
  entry-set splitting on initial argument types and grouped setter
  equivalence classes. Three `#if 0` blocks in `fa.cc`
  (`initial_compatibility`, `setters_classes_cannonicalize`, and
  the "group" pass inside `compute_setters`) plus the supporting
  `SettersClasses` / `cannonical_setters_classes` infrastructure
  were preserved as breadcrumbs; removed June 2026.
