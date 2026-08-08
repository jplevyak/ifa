# 088 — LLVM backend segfaults constructing a class two ways: once using a `None`-default arg, once with an explicit value

**Status:** open, found 2026-08-08 while verifying the fix for
[issues/044](../../issues/closed/044-list-add-mutates-receiver.md) (`list.__add__`
mutating its receiver) on the LLVM backend. Confirmed **pre-existing**
— reproduces identically on the LLVM backend before that fix too
(bisected via `git stash`), so it is unrelated to issue 044's change
and was simply never exercised on `-b` before now. Narrowed
significantly since first filed: the original repro (list `+` inside
a method returning a new instance of its own class) turned out to be
a red herring — list `+` and the loop/multi-instance shape are both
unnecessary; see "Narrowing" below.

**Affects:** `pyc -b` (LLVM backend) only. The C backend compiles and
runs every repro below correctly.

## Minimal repro

```python
class node:
    def __init__(self, route=None):
        self.route = route or []

start = node()
n2 = node([5])
print(start.route)
print(n2.route)
```
- CPython: `[]` then `[5]`.
- pyc, C backend: matches CPython exactly.
- pyc, LLVM backend (`pyc -b repro.py`): compiles cleanly (no
  warnings), then `./repro` segfaults immediately, before printing
  anything.

## Narrowing (each tested standalone against the minimal repro above)

- Removing either construction (`start = node()` alone, or `n2 =
  node([5])` alone) — **no crash**. Both call shapes must coexist in
  the same program.
- Removing `print(start.route)` (keep both constructions, drop the
  first print) — **no crash**. Reading the field back after both
  instances exist is also required, not just constructing them.
- Replacing `self.route = route or []` with plain `self.route =
  route` (so the field is `None`-or-`list` instead of always `list`)
  — **no crash**, but now the LLVM backend's output is wrong (prints
  nothing/empty instead of `None` then `[5]`) — this looks like a
  separate, pre-existing None-handling bug on `-b`, not investigated
  further here; kept out of scope for this issue.
- The original repro's list `+` (`self.route + [move]`), the loop
  building 4 children, and the `extend` method are all **not
  required** — the 8-line repro above triggers the same crash with
  none of that machinery.

This points at pyc's per-class "prototype" instantiation model
(`gen_class_pyda` in `python_ifa_build_syms.cc`; see issue 044 and the
closed issue 017 for the same mechanism traced on the C backend) —
specifically, whatever the LLVM backend does when `__init__` is
invoked through two different argument-shape call sites for the same
class (one relying on the default `None` and the `or []` fallback,
one passing an explicit value) and the resulting field type is
unified to a single concrete type (`list`) rather than a `None|list`
union. The C backend handles this combination correctly; something in
the LLVM path's equivalent handling does not.

## What's known so far

`gdb -batch -ex run -ex bt ./repro` gives an unhelpful backtrace — no
debug symbols, top frame in library address space with no symbol:
```
Program received signal SIGSEGV, Segmentation fault.
0x00007ffff7de4d00 in ?? ()
#0  0x00007ffff7de4d00 in ?? ()
#1  0x00005555555569b0 in ?? ()
#2  0x00005555555569b6 in main ()
```
`??`/no-symbol frames directly under `main` — consistent with a bad
call through a corrupted function pointer or a bad-address struct
field read, but not confirmed either way.

`ptrace_scope` in this sandbox blocks attaching to a running process
(`gdb -p`), so live inspection is limited to `gdb -batch -ex run`
against a fresh launch; core dumps are redirected through `apport`
with no local core file produced. A build with debug symbols enabled
for the LLVM path, or a session with proper ptrace permissions, would
be needed to get a real backtrace — or instrumenting
`ifa/codegen/cg_emit_llvm.cc`'s handling of multi-call-shape
`__init__`/prototype construction directly and comparing against the
C backend's `cg.cc` equivalent for this exact program.

## Why not root-caused further here

Found incidentally while confirming issue 044's fix generalizes to
the LLVM backend. Root-causing an LLVM-backend-specific codegen bug
(`ifa/codegen/llvm.cc` / `cg_emit_llvm.cc`) is a different
investigation from the C-backend/runtime-header work issue 044 did,
and wasn't pursued further to keep that fix's scope bounded — this
issue exists to carry the (now well-isolated) repro and narrowing
forward rather than lose them.

## Verification plan once fixed

- The 8-line repro above, `pyc -b repro.py && ./repro`, must match
  CPython (`[]` / `[5]`).
- The original, less-minimal repro (list `+`, loop, `extend` method —
  see issue 044) must also pass on `-b`.
- `tests/list_add_no_mutate.py` (added for issue 044) exercises a
  related but non-crashing shape (no default arg) — add a new
  `tests/` case for this default-arg-plus-explicit-arg construction
  pattern once fixed, run on both backends.

## What this unblocks

Any LLVM-backend program with a class whose `__init__` has a
default-`None` parameter that's constructed both with and without
that argument in the same program — an extremely common pattern
(optional constructor args generally) — currently crashes outright on
`-b` rather than running.
