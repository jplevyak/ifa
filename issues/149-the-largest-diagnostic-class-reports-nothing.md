# ifa/149 — the corpus's largest warning class was reporting a blank

**Found 2026-09-11** while asking which set of programs to work on next.

## The classes, by size

Grouping the 45 corpus programs that produce diagnostics by their
DOMINANT warning kind gives disjoint groups:

| dominant warning | programs |
| --- | --- |
| `illegal call argument type expression illegal: T` | **23** |
| `expression has no type` | 14 |
| `illegal call argument type 'X' illegal: T` | 5 |
| implicit conversion / illegal primitive argument | 1 each |

So the largest single class is the first, at 23 of 45 programs.

## 55% of it said nothing at all

Of the corpus's **747** `illegal call argument type expression` warnings,
**416 (55%)** rendered as

```
warning: illegal call argument type expression illegal:
```

— nothing after the colon. Across 14+ programs: tarsalzp 51, msp_ss 49,
rubik 42, doom 38, plcfrs 31, rdb 29, othello2 21, softrender 14,
sudoku3 12, mastermind2 12, quameon 10, sunfish 9, rsync 8, timsort 7.

`show_illegal_type` printed `show_type(*v->type->type)` — the PROJECTION,
which by design drops constants — so two different conditions both came
out blank:

- the offending type is BOTTOM: the analysis never typed the argument at
  all. This is the bulk of the 416.
- the offending type is constants-only (typically nil): `make_AType` sets
  `->type` to bottom when `nonconsts.n == 0`, so a genuine
  `__pyc_None_type__` printed as nothing.

Both now print. On `tarsalzp` the 51 blanks become `(no type)`; on
`tests/minmax_3arg.py` two blanks become `__pyc_None_type__`, which is the
second case and is strictly more information than before.

## Why this matters more than its size

It is diagnostic-only — no analysis behaviour changes — but the largest
warning class in the corpus was unreadable, and both conditions it was
hiding are ones this project actively works on:

- "the argument is untyped" is the same condition as the second-largest
  class (`expression has no type`, 14 programs), reported at a call site.
  Counting them together, **roughly 37 of the 45 failing programs have an
  untyped value as their dominant complaint** — far larger than any
  contour-splitting group, and it was invisible because more than half of
  its instances printed a blank.
- a nil-only type reported as blank is exactly the `{None}`-projection
  trap recorded in issue/060 and in ifa/133's nilstore work.

Five goldens pinned the blank text and were re-blessed, one line each
except `minmax_3arg` (2 lines, to `__pyc_None_type__`) and `match_seq`
(12 lines). Every changed line is the message this issue is about.

All six CI gates pass, 315/0.

## Next

The untyped-argument class is now legible and is the biggest thing in the
corpus. Reading a sample of the 416 to find out WHY those arguments are
bottom is the obvious follow-up, and it is a different question from the
contour-splitting work that ifa/128, ifa/129, ifa/133, ifa/146 and ifa/148
have been circling.
