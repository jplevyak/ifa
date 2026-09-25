# ifa/158 — every type violation is fatal

**Status:** landed 2026-09-17, default ON. `PYC_STRICTVIOL=0` restores the old
severity.

**Author's directive:** *"Make all warnings of this sort on the default test,
regression and sweep into fatal compile errors."*

## What changed

`show_violations` used to make only `BOXING` and `DEFINITELY_UNBOUND` fatal
regardless of mode; everything else printed as `warning` under permissive mode
(`fruntime_errors`) and the analysis returned 0. Now **every** violation kind is
fatal except `MAYBE_UNBOUND`, which stays advisory for the reason already
recorded at the site: a possibly-unbound read can be perfectly correct
(a short-circuit guard), so erroring on it rejects valid programs.

## Why

A violation means the analysis could not type something. Emitting a binary
anyway is CLAUDE.md's *"the alternative to failing here is emitting a program
that lies"*.

`sudoku5` is the worked example, and it is worse than it looks:

- it compiled with **19 warnings**;
- `solution` was inferred as `{tuple, int64}` — not a list at all, where
  shedskin gives `list<tuple<__ss_int> *>` (verified by running shedskin);
- and **nothing caught it**. The program's entire output is one
  `TIME %.2f` line — `if n == 9999` inside `for n in range(100)` is dead — so
  the corpus stdout check was comparing two wall clocks and reporting a match.

A program can therefore be wrongly typed, compile, run, exit 0 and "match
CPython" while the harness verifies nothing. Refusing is the only signal that
survives that.

## What it costs

Corpus `-m check`, same tree, fresh build:

| | before | after |
| --- | --- | --- |
| compile_fail | 3 | **37** |
| run_fail | 35 | 11 |
| stdout_differs | 25 | 18 |
| programs with warnings | 35 | **1** |

**37 of 77 programs now refuse.** The falls in `run_fail` and `stdout_differs`
are not improvements — those programs no longer produce a binary, so they leave
those columns. The honest single number is: **34 programs moved from
"compiles, with warnings" to "refuses"**, and every one of them was a program
whose analysis had already failed.

Suite: **312 passed / 0 failed** on both backends. Seven fixtures moved:

- `branch_merged_scalar_union`, `container_scalar_union_add` — severity
  re-blessed in the `.check` (the only diff was `warning:` → `error:`).
- `cross_type_method`, `empty_container_elem`, `match_none`, `match_seq`,
  `splitter_mark_type` — these previously compiled and ran correctly, so they
  carry `.known_issue` markers rather than baked-in wrong output. Each flips to
  PASS by itself when the inference behind its violation lands.

One diagnostic regressed and it is inherent: `container_scalar_union_add`'s
specific message (*"a variable holding {float64, list} has no representation …
issues/018"*) comes from `codegen_common.cc`, i.e. from CODEGEN, which no longer
runs. The per-site analysis errors are still precise and better localised; the
explanation is lost. Restoring it means moving that check into the analysis.

## What this does NOT mean

It is not a claim that those 37 programs need boxing or a representation
change. shedskin compiles all of them without one. Every refusal is a pyc
inference deficiency made visible — which is the point of the change, and
CLAUDE.md's "boxing is never the answer for a corpus program" applies to every
single one.

The corpus number is now a **real** measure of inference quality rather than of
how much pyc is willing to paper over. Expect it to be the headline metric for
the splitter work: `compile_fail` should fall as demand-driven splitting
improves.
