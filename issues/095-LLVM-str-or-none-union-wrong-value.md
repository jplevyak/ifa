# 095 — LLVM backend: a `str | None` local reads back wrong when it should be `None`

**Status:** open, found 2026-08-11 implementing a real `pyc_lib/getopt.py`
(issues/041) — `optarg`'s type is `str | None` (assigned `None` on one
branch, a `str` slice on the other) and its `is not None` check
misbehaves specifically on the LLVM backend. C backend is correct.

**Affects:** `ifa/codegen/cg_emit_llvm.cc` — likely the same general
territory as [093](093-CGEN-int-float-union-move-not-coerced.md) (a
scalar/pointer union collapsed to one storage representation, MOVE not
handled correctly for one branch), but not confirmed to be the same
root cause — `093` is a numeric int64-into-float64 coercion gap;
`None` (a null pointer) into a `str` (`char*`) slot needs no numeric
conversion at all, so if it's the same *class* of bug it's a different
specific mechanism.

## Repro

```python
def f(opt):
    eq = opt.find("=")
    if eq < 0:
        optarg = None
    else:
        optarg = opt[eq+1:]
        opt = opt[:eq]
    if optarg is not None:
        print("has arg:", optarg)
    else:
        print("no arg")

f("verbose")
f("out=file.txt")
```

CPython and pyc's C backend both print:
```
no arg
has arg: file.txt
```

pyc's LLVM backend (`-b`) prints:
```
has arg: e
has arg: file.txt
```

The `None` branch is wrong: `optarg is not None` evaluates `True` when
it should be `False`, and the printed value (`"e"`) is neither `opt`
nor any expected slice of it — looks like a garbage/stale read, not a
clean off-by-one. The second call (`optarg` genuinely a `str`) is
correct on both backends.

## Not yet traced

Root cause not investigated past confirming the minimal repro and that
it's LLVM-only. Candidate starting points, in likely order given
[093](093-CGEN-int-float-union-move-not-coerced.md)'s precedent:

- Whatever MOVE-emission code path issue 093 identified for the
  int64-into-float64 case, checked for the analogous
  pointer-into-pointer (nil-into-str) case — may be a *different* gap
  even if superficially similar, since no numeric coercion is needed
  here (a null pointer is already a valid `char*` bit pattern).
- The per-branch `is None`/`is not None` narrowing mechanism
  ([025](025-FA-intra-function-union-narrowing.md)) — this repro's
  shape (`is not None` guarding a later use) is exactly what that
  issue's narrowing infrastructure targets; worth checking whether
  narrowing is *mis-firing* here rather than simply absent.

## Verification plan

- The repro above on LLVM, matching CPython's `no arg` / `has arg:
  file.txt`.
- `pyc_lib/getopt.py`'s own `f("--verbose", ...)`-shaped call sites
  (see the getopt test added this session, `tests/getopt_module.py`
  once added) on LLVM specifically — currently expected to fail on `-b`
  until this is fixed; passes on the default C backend.
- Full `test_pyc.py`, both backends, once fixed.

## What this unblocks

Correct LLVM-backend behavior for the common "optional value, `None`
on one path" idiom — likely to affect more than just `getopt`; any
function returning/assigning `T | None` where `T` is a pointer-shaped
type (`str`, `bytes`, a record) and reached through a real branch (not
constant-folded) is a candidate. Currently silently wrong output on
`-b` only; the C backend (the documented production path) is
unaffected, which is presumably why this hasn't been noticed before.
