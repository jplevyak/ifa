# 001 — Compilation database (CDB)

Cache the analysis decisions made on the previous compile to a
file on disk, so that the next compile can pre-seed equivalence
classes and skip work that was already proved.

## Where the idea came from

The CDB feature is mentioned in the original Plevyak/Chien
dispatch-cloning paper (§3.3). The observation is that clone
decisions — "which entry set a call edge belongs to", "which
creation sets are equivalent" — are largely stable across recompiles
of the same source. If we record them, the next run can use them as
hints and avoid re-discovering the partition from scratch.

## What was there

Pre-removal layout (deleted June 2026):

- `ifa/analysis/cdb.h` — declared the on-disk record types:
  - `CDB_EntrySet`: `es_id`, `cs_ids`, `edge_pnode_id`,
    `edge_es_id`.
  - `CDB_CreationSet`: `cs_id`, `edge_pnode_id`, `edge_es_id`.
  - `CDB`: owner struct, `funid` / `classid` / `esid` / `cs_info`
    maps.
  - `int read_cdb(FA *fa);` / `int write_cdb(FA *fa);`
- `ifa/analysis/cdb.cc` — implemented `read_cdb` (working
  whitespace-delimited reader for `F` / `E` / `C` records) and
  `write_cdb` (stub: `return -1` with a comment "use a temporary
  file and link for atomicity").
- `FA::cdb` — `CDB *` field on the analysis context, default-`null`,
  never allocated by the live code paths.
- `Fun::prof_id`, `Fun::prof_ess`, `Fun::es_info` — fields populated
  only by `cdb.cc`'s readers; never read on the analysis hot path.
- `check_es_db(AEdge *e, Vec<AEdge *> &ees)` in `fa.cc` — the
  intended consumer of the cache: an `extend_analysis` hook that,
  given an incoming edge, would consult the CDB and short-circuit
  the search for a compatible entry set. The implementation was a
  one-liner `return 0;` — i.e. "always miss".

The on-disk format (sketched in `cdb.cc`'s `read_cdb` and the field
shapes in `cdb.h`) was line-oriented:

```
F <fun_id> <prof_id> <num_entry_sets> <es_id>*
E <fun_id> <es_id> <num_cs_ids> <num_edges> <cs_id>* (<pnode_id> <es_id>)*
C <class_id> <cs_id> <num_callers> (<pnode_id> <es_id>)*
```

`if1_cannonicalize_string` was used on the pnode IDs so reads
landed back in the interned-string table.

## Why it was dormant

`write_cdb` was never finished. Without a writer, the reader had
nothing to consume, so the entire pipeline was inert. Whoever
started the feature clearly intended to come back for the writer
("use a temporary file and link for atomicity" comment suggests
they had a design in mind) but didn't.

There's no test that exercised the CDB path, and no command-line
flag to enable it.

## What reviving it would require

1. Stable, deterministic IDs for funs / classes / pnodes / entry
   sets / creation sets across recompiles. Today many of these IDs
   are allocation-order-dependent — fixing
   [issue 009](../issues/009-fa-violations-nondeterminism.md) is a
   prerequisite, since iteration-order non-determinism in the
   analysis would make cache hits randomly miss.
2. Implement `write_cdb` (atomic tempfile + rename).
3. Implement the `check_es_db` consumer: given an incoming edge,
   look up `e->match->fun` in the CDB, find the recorded entry set
   for its pnode/predecessor combo, and either short-circuit the
   compatibility scan or seed the equivalence-class search.
4. Add a CLI flag (`-cdb path` or env var) and a smoke test that
   compiles a small program twice, compares the cache file, and
   confirms the second pass uses the cache (fewer fa-converge
   passes, identical output).

## What this would unblock

Incremental analysis. Today every compile re-runs full IFA from
scratch. On large programs this dominates compile time.

The estimated impact at the time was significant for pyc-style
workloads where most of the code is shared library boilerplate that
doesn't change between edits.

## See also

- [../IFA.md](../IFA.md) §11.5 "`check_es_db` is a stub" — original
  inline mention.
- [../CLONE.md](../CLONE.md) §11 "The compilation-database angle" —
  expanded explanation aligned with the paper.
- The Plevyak/Chien cloning paper, §3.3.

## History

Removed from the tree as part of the ifa/analysis tier-1 cleanup,
June 2026. Git history before that point preserves the working
reader and the dead writer.
