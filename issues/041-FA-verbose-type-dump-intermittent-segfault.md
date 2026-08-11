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
