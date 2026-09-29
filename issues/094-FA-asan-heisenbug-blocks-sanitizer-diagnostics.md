# 094 — an ASAN build of pyc intermittently crashes on `hello_world.py`

**Status:** open, unlocalised. Filed 2026-08-11 while attempting
closed/041's ASAN soak (041 is closed as not reproducible, so nothing
waits on this). Rewritten 2026-09-28; history in git:
`git show 3f36072b:ifa/issues/094-FA-asan-heisenbug-blocks-sanitizer-diagnostics.md`.
It matters on its own terms: a sanitizer build that is not trustworthy
blocks every future memory diagnostic.

## Symptom

On a freshly built ASAN `pyc` (`-fsanitize=address
-fno-omit-frame-pointer` in both Makefiles, in a throwaway worktree),
compiling `hello_world.py` sometimes dies before any real work:

```
SEGV on unknown address 0x000000000001
 #0 StringChainHash::canonicalize   ifa/common/map.h:700
 #1 if1_cannonicalize_string        ifa/if1/if1.cc:671
 #2 cannonicalize_string            python_ifa_util.cc:49
 #3 ast_to_if1_baseline             python_ifa_main.cc:417   (PycModule::filename)
```

`0x1` looks like a stored `bool true` where a `char *` was expected. A
hardware watchpoint on `PycModule::filename` saw only its legitimate
initial write. Adding any `fprintf`, or running under gdb, stopped the
crash from reproducing.

## Leading hypothesis (unconfirmed)

A Boehm-GC conservative-scan root miss under ASAN's altered stack and
heap layout. The object holding `filename` is collected and reused while
still referenced. That would explain both the watchpoint silence and the
sensitivity to any code change.

## Next steps, in order

1. **Retry at HEAD first.** `1a240ce3` (2026-09-27) fixed an
   out-of-bounds backward read in the parser's indent actions (an
   intermittent SIGSEGV under load). It is not obviously this bug, but it
   is the same "layout-dependent crash" class, and ASAN at HEAD may simply
   not reproduce.
2. Run with `PYC_NO_GC` (the valgrind recipe from issues/118), or with
   `GC_DONT_GC=1`. If the crash disappears, it is the collector.
3. If so, register the offending allocation as a GC root (or allocate it
   uncollectable), or build Boehm with ASAN awareness
   (`--enable-munmap=no`, `GC_NO_DLOPEN`, and ASAN's `detect_leaks=0`).

## Verification

100 consecutive ASAN compiles of `hello_world.py` with no crash, then an
ASAN soak of the test suite.
