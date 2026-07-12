# Issue 038: LLVM `coro-split` marks a coroutine's second suspend point's genuine-suspend path as `unreachable`, corrupting driven async execution

**Status:** open. **Found:** 2026-07-12, digging into a C-backend
vs. LLVM-backend behavioral divergence for `async`/`await`.
**Affects:** the LLVM backend's coroutine lowering
(`ifa/codegen/cg_emit_llvm.cc`, the `is_async` codegen path) for
*any* async function with two or more suspend points — which, after
the initial-suspend fix below, is every async function that awaits
anything at all.
**Related:** [022-async-await-syntax.md](../../issues/022-async-await-syntax.md)
(pyc-frontend issue tracking `async`/`await` generally),
commit `a6e58b0e` (added the missing coroutine initial suspend —
correct in isolation, but is what turns *every* driven async
function into a 2-suspend-point function, which is what exposes
this bug), commit `bbe57c70` (fixed a different, already-landed
coroutine bug: a duplicate fallthrough `llvm.coro.end`).

## Symptom

Any async function that does real work between an `await`/I/O-wait
and its return — i.e. has at least one suspend point beyond the
mandatory initial one — segfaults or infinite-loops (repeating
prints) when actually driven through the event loop
(`_CG_run_coro`). `tests/test_async_net.py` is pyc's own example:
compiles cleanly under `-b`, but running it crashes with a
"GC failed to expand heap" storm (an infinite restart loop, not a
clean segfault) or a straight SIGSEGV depending on build flags.

Minimal pyc-level repro (no networking needed):

```python
def create_socket() -> int:
    return __pyc_c_call__(int, "socket", int, 2, int, 1, int, 6)

async def do_request():
    fd = create_socket()
    print("start")
    __pyc_c_call__(int, "_CG_net_connect", int, fd, str, "example.com", int, 80)
    __pyc_c_call__(int, "__pyc_net_wait_write__", int, fd)
    print("writable")

res = __pyc_c_call__(int, "_CG_run_coro", int, do_request())
```

Compiled with `pyc -b`, then run: `"start"` repeats forever
(eventually OOMs) or the process segfaults with the fault address
equal to the instruction pointer — the classic signature of jumping
through a garbage function pointer (`_CG_resume_coro`'s manual
vtable-style dispatch, `pyc_runtime.c`, reading a coroutine frame
that's already been freed).

## Root cause

Traced through three layers before finding the actual bug — the
first two turned out to be real but insufficient explanations:

1. **Not the cause, but adjacent:** `_CG_run_coro`/
   `_CG_event_loop_run` (`pyc_runtime.c`) unconditionally spawns
   whatever handle it's given straight onto the ready queue,
   assuming it's a fresh, never-run coroutine. True once the
   initial-suspend fix (commit `a6e58b0e`) landed; not true before
   it, when a called-but-undriven async function ran eagerly and
   may have already registered itself for I/O by the time
   `_CG_run_coro` saw it. Not further investigated since it stopped
   mattering once initial suspend was added.

2. **Not the cause:** initially suspected sharing one
   `suspend_ret`/`destroy` exit block across multiple suspend points
   (`ensure_coro_suspend_destroy_bbs` in `cg_emit_llvm.cc`) confused
   `coro-split`'s per-suspend accounting. Tested by giving the
   initial suspend fully dedicated (non-shared) exit blocks — the
   bug was identical either way. Ruled out.

3. **The actual cause**, confirmed by isolating just the coroutine
   passes with `opt -passes="coro-early,cgscc(coro-split),coro-cleanup"`
   (bypassing the rest of `-O2`, which was otherwise obscuring
   things): `coro-split`'s generated `.resume` clone, for a
   coroutine with **two** suspend points, produces this pattern for
   the second suspend's dispatch —
   ```llvm
   resume.1.landing:
     %0 = phi i8 [ -1, %CoroSuspend2 ], [ 0, %resume.1 ]
     br label %AfterCoroSuspend3

   AfterCoroSuspend3:
     switch i8 %0, label %suspend_ret [
       i8 0, label %resume
       i8 1, label %destroy
     ]

   suspend_ret:                    ; default case
     unreachable                   ; <-- WRONG. -1 is a real, live PHI input.
   ```
   The `-1` PHI input (from `%CoroSuspend2`, i.e. "control fell
   through the second `coro.suspend` for the first time, genuinely
   suspending") is a completely legitimate, reachable value — it's
   what happens on *every* first pass through the function body.
   The optimizer nonetheless proves the `switch`'s default arm
   (which is what should catch `-1`) unreachable and deletes it.
   At runtime this means: the first time the coroutine reaches its
   second suspend, instead of returning control to the caller (the
   correct behavior — go dormant, wait to be resumed), execution
   falls into `unreachable`'s undefined behavior, which in practice
   free's the coroutine frame as if it had run to completion and/or
   loops back into whatever code happens to sit nearby in memory —
   matching both observed symptoms (the "prints repeat forever" loop
   and the "resume-through-freed-frame" segfault, depending on
   which side of the UB coin the specific build lands on).

   **Confirmed this is not specific to pyc's codegen shape**: the
   raw, pre-optimization IR pyc emits is structurally textbook-correct
   (verified by hand — two independent, properly wired
   save/suspend/switch/return sequences). The bug is entirely inside
   `coro-split`'s post-transform reachability analysis.

   **Confirmed this is not an unstable-LLVM-build artifact**: this
   machine's default LLVM (22, an apt.llvm.org trunk/dev snapshot)
   and a real stable release (LLVM 20, `clang-20`/`llvm-20` from
   Ubuntu 24.04's own repos, the version CI is now pinned to)
   produce byte-identical `unreachable` output for the same input.

   **Confirmed the trigger is specifically "two-or-more suspend
   points where one clone falls through from a statically-known
   dispatch into a second, genuinely dynamic suspend"** — a
   single-suspend-point coroutine compiles and runs correctly (see
   the contrast reproducer below); it's the combination of the
   initial suspend (a suspend whose "which case" is known at compile
   time within the `.resume` clone, so `coro-split` renders its own
   dispatch as `switch i8 0, ...`) immediately preceding a second,
   real suspend that trips this.

## Minimal standalone reproducer (no pyc, no runtime dependencies)

`coro_repro.ll` — two suspend points, structurally identical to
what pyc emits for any `async def` with one real `await`/I/O-wait:

```llvm
target triple = "x86_64-pc-linux-gnu"

declare token @llvm.coro.id(i32, ptr, ptr, ptr)
declare i64 @llvm.coro.size.i64()
declare ptr @llvm.coro.begin(token, ptr)
declare token @llvm.coro.save(ptr)
declare i8 @llvm.coro.suspend(token, i1)
declare ptr @llvm.coro.free(token, ptr)
declare i1 @llvm.coro.end(ptr, i1, token)

declare ptr @malloc(i64)
declare void @free(ptr)
declare void @print_a()
declare void @print_b()

define ptr @coro_two_suspends() #0 {
entry:
  %id = call token @llvm.coro.id(i32 0, ptr null, ptr null, ptr null)
  %size = call i64 @llvm.coro.size.i64()
  %mem = call ptr @malloc(i64 %size)
  %hdl = call ptr @llvm.coro.begin(token %id, ptr %mem)

  ; suspend #1: the "initial suspend" (mirrors
  ; std::suspend_always initial_suspend())
  %save0 = call token @llvm.coro.save(ptr %hdl)
  %susp0 = call i8 @llvm.coro.suspend(token %save0, i1 false)
  switch i8 %susp0, label %susp0.suspend_ret [
    i8 0, label %body
    i8 1, label %susp0.destroy
  ]

susp0.suspend_ret:
  ret ptr %hdl

susp0.destroy:
  %free0 = call ptr @llvm.coro.free(token %id, ptr %hdl)
  call void @free(ptr %free0)
  %end0 = call i1 @llvm.coro.end(ptr %hdl, i1 true, token none)
  ret ptr %hdl

body:
  call void @print_a()

  ; suspend #2: a genuine mid-body await
  %save1 = call token @llvm.coro.save(ptr %hdl)
  %susp1 = call i8 @llvm.coro.suspend(token %save1, i1 false)
  switch i8 %susp1, label %susp1.suspend_ret [
    i8 0, label %after
    i8 1, label %susp1.destroy
  ]

susp1.suspend_ret:
  ret ptr %hdl

susp1.destroy:
  %free1 = call ptr @llvm.coro.free(token %id, ptr %hdl)
  call void @free(ptr %free1)
  %end1 = call i1 @llvm.coro.end(ptr %hdl, i1 true, token none)
  ret ptr %hdl

after:
  call void @print_b()
  %end2 = call i1 @llvm.coro.end(ptr %hdl, i1 false, token none)
  ret ptr %hdl
}

attributes #0 = { presplitcoroutine }
```

Reproduce the bad optimization directly:

```
opt -S -passes="coro-early,cgscc(coro-split),coro-cleanup" coro_repro.ll -o out.ll
grep -A3 'suspend_ret:' out.ll     # the SECOND one says `unreachable`
```

Reproduce the runtime crash (driver simulates what `_CG_resume_coro`
does — read the resume-fn pointer out of frame offset 0 and call
it):

```c
#include <stdio.h>
void print_a(void) { printf("a\n"); fflush(stdout); }
void print_b(void) { printf("b\n"); fflush(stdout); }
extern void *coro_two_suspends(void);
int main(void) {
    void *hdl = coro_two_suspends();
    void (**vtable)(void *) = (void (**)(void *))hdl;
    vtable[0](hdl);   /* first resume: should suspend after "a" */
    printf("resumed once.\n");
    vtable = (void (**)(void *))hdl;
    vtable[0](hdl);   /* second resume: should print "b" and finish */
    printf("resumed twice -- completed normally.\n");
    return 0;
}
```

```
clang++ -O2 -c coro_repro.ll -o coro_repro.o
clang -O0 -c driver.c -o driver.o
clang++ coro_repro.o driver.o -o repro
./repro
```

Observed: `"a"` prints repeatedly (or the process segfaults,
platform/build-dependent) instead of the expected `a` / (suspend) /
`resumed once.` / `b` / `resumed twice -- completed normally.`
sequence. Reproduced on both LLVM 20 (Ubuntu 24.04's `clang-20`/
`llvm-20` packages) and LLVM 22 (apt.llvm.org trunk snapshot).

**Contrast (works correctly):** the identical shape with only ONE
suspend point (drop the initial suspend, `coro.begin` falls straight
into `body`) compiles and runs correctly — `coro-split` still emits
a hardcoded `switch i8 0, ...` dispatch (since there's only one
suspend, the "which one" question is trivial), but there's no
*second* suspend for that hardcoded dispatch to fall through into,
so the bug doesn't trigger. This is the strongest evidence the
trigger is specifically "resume clone's fallthrough execution
crosses two suspend points in one synchronous pass," not "any
coroutine with `presplitcoroutine`."

## What this unblocks

- A correct LLVM backend for `async`/`await` beyond the syntax
  level — right now, any driven async function that awaits real
  I/O (as opposed to the C backend's `std::coroutine`-based
  implementation, which works) is unusable.
- Confidence in `test_async_net.py` and friends as real regression
  tests rather than compile-only smoke tests (see the "process
  lesson" callout below).

## Next steps (not yet done)

1. File the minimal reproducer upstream at
   https://github.com/llvm/llvm-project/issues — this looks like a
   genuine `coro-split` bug (or at minimum an underdocumented
   invariant pyc's from-scratch, non-Clang-frontend coroutine
   lowering isn't satisfying), not something fixable from pyc's side
   without either (a) a workaround that restructures the emitted
   suspend pattern to dodge whatever triggers the bad reachability
   proof, or (b) upstream landing a fix.
2. Until then, treat the LLVM backend's `async`/`await` support as
   compile-only-verified, not execution-verified — don't build on it
   for anything that needs to actually run.
3. If a workaround is wanted before upstream responds: try
   structuring the initial suspend's dispatch to NOT use a literal
   `switch i8 0, ...` (e.g. route it through an opaque/
   `noinline`-shielded value) to see if that defeats whatever
   constant-propagation is producing the bad reachability proof —
   untested, a guess based on the pattern observed, not a confirmed
   fix.

## Process note

This is the second bug in three days found only by actually
*running* driven async code rather than trusting compile success —
matching this session's other recurring lesson (see
[033](033-splitter-non-idempotent-divergence.md)'s "process lesson"
callouts about `extend_analysis` changes needing real corpus runs,
not just compile/suite-pass checks). `test_async_net.py` and
friends have no `.exec.check` goldens; they only assert compile
success. Worth fixing once this bug (or a workaround) lands, so the
suite would have caught this on its own.
