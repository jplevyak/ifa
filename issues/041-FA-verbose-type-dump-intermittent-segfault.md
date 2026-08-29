# Issue 041: intermittent segfault inside the `-v` per-pass type dump (`fa_dump_types`)

**Status:** open, unreproduced-on-demand. Two sightings, both under
`-v`, both mid-way through `fa_dump_types`' output, on different
code versions and different inputs — so it is a longstanding latent
bug in the diagnostic dump path (or something the dump's read
pattern exposes), not a regression from any recent change. **S4**
(diagnostic-only path; no effect on non-`-v` compiles).

## Symptom

The compile segfaults partway through the per-pass verbose type
dump (`fa_dump_types`, `analysis/fa.cc` ~2850, called from
`complete_pass` under `ifa_verbose`), with the output cut mid-line.
Non-`-v` runs of the same input have never crashed.

Sightings:
- **2026-07-13, bh** (`shedskin_examples/bh/bh.py`), during the
  issue-033 stall-guard verification: crash during iteration 3's
  dump, output cut at `Vec3::__`. The identical binary then ran the
  same input cleanly 8+ consecutive times; never reproduced again
  on that build or any later one.
- **2026-07-14, pygasus** (`shedskin_examples/pygasus/pygasus.py`),
  during the issue-033 stage-2/4 joint-rework verification: crash
  mid-dump (output cut at `range::__next`), before pass 1's PASS
  line. Runs 1, 3, 4, 5 of the same binary on the same input were
  clean and byte-identical (the determinism gate holds when it
  doesn't crash).

Both sightings were under machine load (parallel sweeps/builds in
neighboring sessions), suggesting GC-timing / memory-pressure
sensitivity, consistent with ~1-in-5 to 1-in-10 flakiness that
vanishes on quiet re-runs.

## Where to look

`fa_dump_types` iterates `fa->ess`, calls `f->collect_Vars` +
`make_AVar(v, es)` per Var, and `fa_dump_var_types` →
`show_type(*av->out)` per AVar. Candidate classes, in likelihood
order given this codebase's history:
- a set-mode `Vec` iterated without the null-slot guard (`for (x :
  vec) if (x)`) somewhere in the dump path — the exact class of
  issue 033's `equivalent_es_pnode` crash;
- `make_AVar` called mid-dump creating state the surrounding
  iteration doesn't expect (the dump is supposed to be read-only
  but `make_AVar` allocates on miss);
- a GC interaction: the dump runs at pass boundaries where large
  garbage generations exist; an unrooted temporary in the print
  path would be ASLR/GC-timing dependent, matching the flakiness.

## Verification plan

ASAN soak: build with `-fsanitize=address` (the issue-033 M2
investigation already worked out the recipe, including the
`initialize_primitives` ignorelist entry) and loop `pyc -v` on bh
and pygasus under artificial memory pressure until it fires; the
first symbolized trace almost certainly identifies a one-line
guard, as it did for the two issue-033 crashes.

**Attempted 2026-08-11, blocked before reaching bh/pygasus at all** —
see [094](094-FA-asan-heisenbug-blocks-sanitizer-diagnostics.md). The
ASAN build itself hit a different, unrelated-looking intermittent
segfault on the simplest possible input (`hello_world.py`), and every
attempt to localize *that* one changed whether it reproduced (a
heisenbug on top of a heisenbug). If 094's suspicion is right — Boehm
GC's conservative stack scanner occasionally missing a root under
ASAN's altered stack layout — then this issue's own filed verification
plan may not be reliable as written; a core-dump-based approach
(no sanitizer, real memory pressure, `ulimit -c unlimited`) is the
fallback being tried next, both for 094's blocker and, opportunistically,
for this issue's own crash if it turns up first.

**Core-dump attempt, same day, also no reproduction (but see caveat).**
`/proc/sys/kernel/core_pattern` on this machine pipes to `apport`
(not usable in a sandboxed session, and not something to repoint
system-wide without cause) — used a `gdb -batch -ex run -ex "bt full"
-ex quit` live wrapper around each compile instead, functionally
equivalent (catches SIGSEGV in place with a full backtrace, no core
file needed) without touching any system-wide setting. Ran `pyc -v`
on `bh.py` and `pygasus.py` alternately, 15 iterations each (30 total
`-v` compiles) over ~23 minutes, concurrently with a second background
loop continuously re-running the full `shedskin_sweep.sh` corpus sweep
(`nproc`-wide parallel, `TIMEOUT=30`) the whole time to approximate the
"under machine load" condition both original sightings shared. **Zero
crashes.** Not strong evidence of anything either way: the original
two sightings happened across many months of general project activity
— this session's 30-compile, 23-minute sample is nowhere near large
enough to expect a hit even if the load-generation approach is exactly
right. Recorded so the next attempt doesn't have to re-derive the
`core_pattern`/apport dead end or the gdb-live-wrapper workaround —
just re-run `soak.sh`'s approach (bh/pygasus alternating under gdb,
concurrent corpus-sweep load) for hours instead of minutes.

## Impact

`-v` is used heavily for issue-033-style analysis work (per-pass
trajectories are the verification currency), so a 1-in-5 flake on
large inputs costs real re-run time and — worse — can masquerade as
a regression in whatever change is being verified (it did, twice,
costing an investigation detour each time before the pattern was
recognized).


## 2026-08-29: two of the three filed hypotheses are ruled out

Asked whether this is still an issue. It is — nothing has fixed it, and
it has not been seen since either. But a static audit of the dump path
eliminates two of the three candidates in "Where to look", which is what
an unreproduced-on-demand crash most needs.

**Ruled out: a set-mode `Vec` iterated without the null guard.** This was
the leading suspect (issue 033's crash class, and the same shape as
`cg_build_new_to_val_map`'s `Fun::ess` hole). It cannot be the top-level
loop: `FA` has **two** members, `ess` ("all used entry sets as array") and
`ess_set` ("as set"), and `fa_dump_types` iterates the ARRAY. It is built
in `collect_results()` by `fa->ess.clear()` then `.add()` from
`fa->entry_set_done` **with an `if (es)` guard**, so it holds no nulls.
The dump's other iterations (`vars` from `collect_Vars`, and `gvars`
after `set_to_vec()`) are plain vectors too.

**Ruled out for these inputs: the `es->display[depth - 1]` out-of-bounds
read.** This one is a genuine, documented crash family in this codebase —
`make_AVar` indexes the display for a Var from an enclosing scope and
`unique_AVar` dereferences whatever comes back as a contour (see the note
at `find_or_make_filtered_entry_set`, pyc issue 025). And the dump is
exactly the place it could bite, because `collect_Vars` returns every Var
the Fun mentions with no relation to this EntrySet's display. It fits the
symptom profile perfectly: `-v` only, mid-dump, and "does the garbage
pointer happen to be mapped" is ASLR/GC-timing dependent, which is the
1-in-5-under-load flakiness both sightings describe.

**It does not happen.** A bound check plus `IFA_DBG_DUMPSKIP` reports
**zero** skips on `bh` and on `pygasus` — the two programs that actually
crashed. The guard is now in `fa_dump_types` as hardening (an unchecked
index whose failure mode is a wild dereference deserves a bound test
regardless), and is explicitly documented there as NOT this issue's fix.

**Still standing: the third hypothesis.** `make_AVar` allocates on miss,
so the dump is not read-only: it creates AVars at a pass boundary, in the
middle of iterating structures the allocation can touch. That is now the
only filed candidate left, and it is also the one that would explain why
the crash is sensitive to GC timing rather than to input.

Next attempt should start there — instrument how many AVars the dump
CREATES (as opposed to finds) per pass on bh/pygasus, and if the count is
non-zero, make the dump use a non-allocating lookup and see whether the
sightings' conditions still produce anything.


## The third hypothesis was right, and it is fixed

`make_AVar`/`unique_AVar` create on miss, so the dump was never read-only.
Counted it (`IFA_DBG_DUMPALLOC`):

    bh        creates=27  finds=5443     (pass 1)
              creates=82  finds=8392     (pass 2)
              creates=8   finds=8542     (pass 3)
    pygasus   creates=134 finds=32650    (every pass)

**And it changed the answer.** Same input, same binary, `PYC_DBG_CONTOURS='*'`:

    bh, no -v    pass=26 SUMMARY total_ess=414 total_css=1577
    bh, -v       pass=26 SUMMARY total_ess=415 total_css=1578

That is worse than the crash it was filed for. `-v` is this project's
verification currency for splitter/convergence work, and it was reading a
slightly *different* analysis from the one a real compile performs — one
extra EntrySet and CreationSet on bh, from AVars the dump itself minted at
a pass boundary.

**Fix**: the dump looks up, never creates. `fa_dump_contour_for()`
replicates `make_AVar`'s contour choice, `v->avars.get()` fetches, and a
(Var, contour) pair with no AVar is skipped — it was never reached by the
analysis, so it has no type to show. On pygasus that skips 134 of ~32,800
entries (0.4%) and the dump still carries 14,429 function sections.

    bh, -v (after)   pass=26 SUMMARY total_ess=414 total_css=1577   ✓ identical

**Does this fix the segfault?** Unknown, and deliberately not claimed. It
removes allocation from a dump that runs at pass boundaries where large
garbage generations exist, which is the mechanism the "GC interaction"
hypothesis proposed, and it is consistent with every property of the two
sightings (`-v` only, mid-dump, load-sensitive, never reproducible on
demand). But neither sighting has been reproduced since 2026-07-14, so
there is nothing to re-run and confirm against. Left OPEN for that reason;
if it recurs, the remaining suspects are the print path itself
(`show_type`) and the GC.

Five CI gates green; 306 passed / 0 failed / 15 known on both backends.
