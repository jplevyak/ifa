# 186 — pass 1 pushes a whole-program union through the shared builtins (othello3)

**Status:** open. Diagnosed 2026-10-08. Two bugs it exposed are fixed
(below); the pass-1 cost itself is not.

## Symptom

othello3 does not compile. Before 2026-10-08 the ifa/057 stall guard
failed it after ~120 s of pass 1, reporting non-convergence. With the
guard fixed, pass 1 ran for 50 minutes (7.2 GB resident) without
finishing. othello3's own README says shedskin takes about an hour on it.

## What the program is

Generated code: 830 `Flip` subclasses, 64 `Put` subclasses, 1,848 stores
into `flip_table`, 256 `.go()` dispatch sites. About 2,000 CreationSets in
all, counting ints, strs and tuples.

## Mechanism

Pass 1 starts minimal: one EntrySet per function. Every shared builtin
contour therefore merges every caller's values. Measured holders of the
same ~2,039-member union (ints, strs, tuples, every flip and put class):

- `__list_iter__.__next__`'s `x`, the single list-iterator creation point
  inside `list.__iter__`;
- `len`'s `x`, `join`'s `seq`, `__eq__` / `__ne__`'s `x`;
- `str_state`'s `digit`;
- the comprehension variable `h` in `for h in human_moves`, which only
  ever iterates strings. `__next__`'s merged return flows back into every
  loop.

The union then spreads to tens of thousands of AVars one CreationSet at a
time, so each AVar is rebuilt about 2,000 times: **~105 million AVar type
changes averaging ~900 CreationSets each in 32 minutes.** The demand that
would separate the callers is evaluated at quiescence, after the pass, and
the pass never gets there.

Not the cause: start-merged list CreationSets. With `PYC_CSDCPA1=0` the
same unions form and pass 1 still stalls.

## Fixed on the way (2026-10-08)

1. **Dispatch was cubic in the receiver union** (`ifa/if1/pattern.cc`),
   found by sampling stacks under gdb.
   - The multi-candidate votes collapse was gated on `nmat`, which stops
     counting at 2, so it ran with all 800 `go` candidates. Its vote mask
     `1ull << ci` is undefined past bit 63, so receivers with different
     candidates could be merged. It is now gated on the real candidate
     count (`ncand <= 64`), and above 8 candidates the full enumeration
     indexes candidates by receiver CreationSet.
   - `clear_matches` swept every candidate at every leaf; it now clears
     only the candidates the leaf touched.
   - `reverify_filters` ran at every leaf; it now runs once per
     `pattern_match`, with the same result.

   A synthetic othello3-shaped program with 800 classes: pass-1 match time
   76 s -> 10.5 s (whole pass 82 s -> 16 s), output equal to CPython at
   every size.
2. **The ifa/057 stall guard never saw progress within a pass**
   (`fa.cc`). It read `fa->ess.n`, which `collect_results` rebuilds only at
   the end of a pass, so it was a flat 120 s cap on any pass
   ([closed/057](closed/057-sorted-tolist-fa-nonconvergence.md) had already
   noted the reading was misleading). It now counts EntrySets as
   `set_entry_set` mints them (`fa_es_minted`).

## Directions (not chosen)

Making each update cheaper will not bring pass 1 under the sweep's 400 s
compile cap; the number of updates is the problem.

- **Remove the confluence.** The list iterator is the most visible one,
  but `len`, `join` and the comparisons show the same pattern, so a
  fix specific to `__list_iter__` is not enough. The question is why a
  minimal pass-1 contour of a builtin must carry every caller's union
  before any demand can be asked.
- **Settle a growing union before readers see it.** Order the worklist so
  an AVar's type stops growing before its consumers are re-run, so each
  AVar updates once rather than ~2,000 times.

Stop condition for either: if pass 1 on othello3 still exceeds 400 s with
the union's holders unchanged, the model (update count dominates) is
wrong, and the per-update cost needs re-profiling.

## Verification

- `./pyc shedskin_examples/othello3/othello3.py` compiles under the
  sweep's cap and its output matches CPython.
- `make test` green; a `check` sweep with no outcome regressions.
