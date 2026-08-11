# 094 — Intermittent `PycModule::filename` corruption under ASAN blocks the ASAN-soak diagnostic technique itself

**Status:** open, found 2026-08-11 while attempting
[041](041-FA-verbose-type-dump-intermittent-segfault.md)'s own filed
ASAN-soak verification plan. Not the bug 041 describes — a different,
apparently more fundamental one that made itself impossible to
localize with the tools tried. Filed rather than root-caused: every
diagnostic technique tried either failed to reproduce it or changed
its behavior, and the ones left (proper GC root-tracing, or ditching
ASAN for core dumps) need more setup than this session had budget for.

**Affects:** unclear — the *observed* corruption is
`PycModule::filename` (`python_ifa.h`), read from
`ast_to_if1_baseline` (`python_ifa_main.cc:417`) and crashing inside
`StringChainHash::canonicalize` (`ifa/common/map.h:700`) — but nothing
found actually points at those specific lines as the culprit; see
"Root cause" below for why the true site is still unknown. Reproduced
only under an ASAN build (`-fsanitize=address -fno-omit-frame-pointer`
added to both `Makefile`s, following
[closed/033](closed/033-splitter-non-idempotent-divergence.md)'s
recipe, in a throwaway `git worktree` — never landed on the main
tree).

## Symptom

Compiling the simplest possible input (`hello_world.py`) under an
ASAN-instrumented `pyc` intermittently crashes before any real
compilation work happens:

```
AddressSanitizer:DEADLYSIGNAL
==...==ERROR: AddressSanitizer: SEGV on unknown address 0x000000000001
    #0 StringChainHash<...>::canonicalize(char const*, char const*)  ifa/common/map.h:700
    #1 if1_cannonicalize_string(IF1*, char const*, char const*)       ifa/if1/if1.cc:671
    #2 cannonicalize_string(char const*)                              python_ifa_util.cc:49
    #3 ast_to_if1_baseline(Vec<PycModule*>&)                          python_ifa_main.cc:417
    #4 ast_to_if1(Vec<PycModule*>&)                                   python_ifa_main.cc:603
    #5 main                                                           pyc.cc:303
```

`0x1` is exactly the bit pattern of a stray C++ `bool true` landing
where a `char*` was expected — `StringChainHash::canonicalize`'s
`while (*a) ...` loop dereferences it and faults immediately.

## What was ruled out

A GDB hardware watchpoint spanning `PycModule::filename`'s full
lifetime (from construction through the crash site) shows the field is
**correctly** set at construction
(`filename(afilename)` in the member-init list — verified via `watch`
firing exactly once, for that legitimate initial write, with the
expected value) and stays correct through every intermediate read this
session added debug prints for. The corruption to `0x1` happens
*without* being caught by the watchpoint on a subsequent run — meaning
either the same memory got legitimately freed and reused for something
else before this read (a **use-after-free of GC-invalidated memory**,
consistent with "the crash trace's own line numbers aren't the real
bug" caveat above), or the bug is timing-sensitive enough that it
didn't fire on the specific run the watchpoint was active for. Checked
[closed/033](closed/033-splitter-non-idempotent-divergence.md)'s own
already-known, pre-existing, unrelated `initialize_primitives`
global-buffer-overflow (primitive index 57 / `await`'s registration,
routed around with an ignorelist in that investigation) — still present
in the current code (`ifa/if1/prim_data.cc:453`) but its call path
doesn't connect to this crash's backtrace, so probably not the same
bug, though not rigorously excluded either.

## Why this is hard: a genuine heisenbug

Reproduction rate on a freshly built ASAN binary, same input, no
changes between runs:

- With **no debug instrumentation**: crashed the first two times tried.
- Adding an `fprintf` immediately before the crash site, or inside
  `PycModule`'s constructor: **stopped reproducing** — the exact same
  crash **never fired again** across several rebuilds-and-reruns with
  instrumentation present, even though the added code only *reads* the
  already-set fields, never writes anything.
- Running under GDB (a breakpoint at construction, then a watchpoint on
  the raw field address, `continue`d to completion): **did not
  reproduce** — the program ran to the next real error
  (`hello_world.py.c:10: unknown type name '_CG_function'`, a
  *different*, likely unrelated pre-existing gap in this particular
  worktree/config, not chased) without the filename ever going bad.
- Immediately after removing the instrumentation and rebuilding
  byte-for-byte the original (working) source: 5 consecutive runs
  crashed the same way, then a 6th run **hung indefinitely** (30s
  timeout, no output at all, not even the crash banner) instead of
  crashing or succeeding.

Every technique that *looks* at the bug changes whether it happens —
textbook symptom of either a data race, or (more likely here, since
this program is single-threaded except for Boehm GC's own marker/
finalizer threads) **Boehm GC's conservative stack scanner missing a
root**. ASAN inserts redzones between stack slots and can alter
register allocation; either could shift what the conservative scanner
sees as "looks like a pointer" on the stack at GC-safepoint time,
occasionally failing to keep an object's last reference visible and
letting the collector reclaim/reuse memory that's still logically
live. This is speculative — not confirmed — but it's the only
explanation found so far consistent with every observation above,
*and* it's the same general disease class
[041](041-FA-verbose-type-dump-intermittent-segfault.md) itself already
suspects ("GC-timing / memory-pressure sensitivity... vanishes on
quiet re-runs") — just caught somewhere else in the program (module-
filename interning at process startup, not `fa_dump_types`).

## Why this matters beyond one crash

If the GC-conservative-scan-miss hypothesis is right, **ASAN is not a
reliable diagnostic tool for this codebase's intermittent-segfault
class of bug** — the exact technique
[closed/033](closed/033-splitter-non-idempotent-divergence.md) used
successfully to root-cause its own two crashes, and the technique
[041](041-FA-verbose-type-dump-intermittent-segfault.md) filed as its
own verification plan. Either ASAN got lucky in 033's case (a
different code path, less exposed to whatever's fragile here), or
something has changed since (~1 month of commits) that made this
worse, or 033's fixes for their two null-guard-shaped bugs are
unrelated to the mechanism here and both can be true at once. Worth
knowing before reaching for an ASAN soak again for any similarly
intermittent bug in this codebase — it may reproduce, may reproduce
*differently* than the real bug, or may hide the moment you add
instrumentation to look closer.

## What a real fix/investigation needs

Not attempted here — each is a real time investment on its own:

- **Proper GC root-tracing**: rebuild Boehm GC itself with
  `GC_DEBUG`/`GC_FIND_LEAK`-style diagnostics, or add
  `GC_gcollect()` calls at suspected safepoints to force collection
  early and deterministically (trading "rare" for "reliable," the
  opposite of what happened when this session added passive
  instrumentation).
- **Core dumps instead of ASAN**: run a plain (non-sanitizer) debug
  build in a loop under real memory pressure (matching 041's own
  filed plan) with `ulimit -c unlimited`, and do post-mortem analysis
  on whatever core actually drops, rather than a live sanitizer whose
  own presence perturbs the bug.
- **Rule out (or confirm) the primitive-57 overflow more rigorously**:
  the "What was ruled out" section above didn't prove it's unrelated,
  just didn't find a connecting call path.

## Verification plan

Whoever picks this up: don't trust a single clean run (with or without
instrumentation) as proof of a fix — this session got both false
negatives (bug hidden by GDB/prints) and a new failure mode (a hang)
from what should have been identical, deterministic runs. Establish an
actual reproduction rate over dozens of *unobserved* runs (a bare
shell loop piping to a log file, no debugger/print-statement changes
between attempts) before and after any change.

## What this unblocks

Confidence in ASAN as a diagnostic technique for this codebase's other
intermittent-crash issues (currently just
[041](041-FA-verbose-type-dump-intermittent-segfault.md), but the same
methodology is referenced as precedent in
[closed/033](closed/033-splitter-non-idempotent-divergence.md) and
could reasonably be reached for again). Also, if the GC-conservative-
scan-miss hypothesis holds, this is a *general* class of bug that could
be silently corrupting other GC-managed objects under real machine
load (not just under ASAN) — 041's own two sightings were both
explicitly "under machine load," which fits.
