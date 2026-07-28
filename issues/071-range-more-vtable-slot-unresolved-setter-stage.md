# 071 — `range::__pyc_more__` method-pointer slot never resolved for a sole CreationSet; setter-stage FA gap + a codegen UB gap it exposes

**Status:** open. Root-caused via generated C + FA violation trace;
not fixed (both fix directions are non-trivial, see below).

**Affects:** `ifa/analysis/fa.cc` (setter-stage splitting for
`clone_methods_per_cs` classes — see [045](closed/045-receiver-cs-method-cloning.md))
and `ifa/codegen/cg.cc`'s `P_prim_setter` case.
**Surfaced while:** digging into `shedskin_examples/chess/chess.py`'s
runtime crash (user request, following up on the
[chaos.py `== None` dig](../../issues/closed/031-eq-none-dispatch-crash.md)).

## Symptom

`chess.py` compiles with exactly **one** warning and exit 0:

```
shedskin_examples/chess/chess.py:26:265: warning: expression has no type
    squares = tuple([i for i in range(128) if not i & 8])
                                                         ^
```

The binary aborts immediately on the first line of `main`:

```
chess: chess.py.c:177: _CG_nil_type _CG_f_108_4(): Assertion `!"runtime error: matching function not found"' failed.
```

`squares` (a module-level global, `tuple(list-comp-over-range(128))`)
is the *only* runtime use of `range`'s class-based iterator protocol
in the whole program — every other `range(...)` call site in
`chess.py` (`tuple(range(17,120,17))`-style constant tuples, and the
two `for m in range(10): for n in range(20):` loops in `__main__`)
has fully-literal arguments and gets constant-folded/unrolled away by
the frontend, so it never reaches a runtime `range` object. `-v`
confirms `range::__pyc_more__` clones *do* get analyzed during FA
(ids 3390/3391 appear ~40 times in the pass log) — this is not a
"method never exists" gap, it's a "method never survives to
codegen" gap.

## Root cause, traced via the generated C

```c
struct _CG_s2300 {                    /* range */
  _CG_TypeObject *__pyc_tag;
  _CG_void e5; /* __pyc_more__ */;
};

_CG_ps2300 _CG_f_3351_2/*range::___init___*/(_CG_ps2300 a1) {
  _CG_ps2300 t0, t1, t3;
  _CG_void t2;                         // <-- never assigned
  t1 = a1;
  ((_CG_ps2300)g0)->e5 = (_CG_void)((_CG_function*)t2);   // UB: reads t2 uninitialized
  t0 = t1;
  return t0;
}
```

`___init___` is the class-prototype initializer `gen_class_pyda`
synthesizes (`python_ifa_build_syms.cc:1694` on, and the
derived-op registration at :1670-1676) — it installs each method as
a field on the class's single global prototype instance (`g0`, the
one-and-only `range` CreationSet here) via a `sym_setter` send.
`cg.cc`'s `P_prim_setter` (line 380) emits `obj->eN = (type)VALUE;`
where `VALUE` is `c_rhs(n->rvals.v[4])` — normally either a Fun
Sym's address directly (the derived-op path passes `mfn` straight
through) or, for an ordinary `def`-in-class-body method needing
per-receiver-CS dispatch resolution, a temp that FA is supposed to
have moved the resolved method pointer into beforehand. Here that
move never happened: `t2` has no writer anywhere in the function, so
the struct's `__pyc_more__` slot gets whatever garbage was on the
stack. At runtime, whatever dispatch-by-address mechanism reads that
slot back (`__main__`'s `((ps2300)t51)->e5`) matches nothing → the
"matching function not found" trap.

**Why the value is unresolved:** `range` is one of the (currently
few) classes flagged `clone_methods_per_cs` ([issue 045](closed/045-receiver-cs-method-cloning.md)) — its `__init__` params are
`__pyc_clone_constants__`, so `__pyc_more__`/`__next__` get
per-CS-precise contours instead of a shared one (that precision is
*why* `self.i < self.j` can constant-fold per instance). Filling the
prototype's method-pointer slot for such a class means FA has to
resolve, for THIS SPECIFIC CreationSet, which concrete
`__pyc_more__` clone is "the" one — i.e. it's a **setter-stage**
resolution, and [issue 063](063-no-type-bucket-triage.md)'s final
update (2026-07-23) already found and left open exactly this
category of gap: *"residual setter/mark-stage oscillation... on
`__len__`/`len`/`__getitem__`/`__setitem__` — deliberately excluded
from the issue-033 type-partition routing... Closing these needs a
setter-class-keyed routing ledger, a second step."* This is the same
mechanism, one more member of that family (`clone_methods_per_cs`'s
own method-pointer slot, not a container's setter), converging on
NOTYPE for the sole reachable use of `range`'s prototype
initializer.

**Scale sensitivity (matches the dijkstra2 precedent):** attempts to
build a small standalone repro failed to reproduce the bug — the
identical `squares = tuple([i for i in range(128) if not i & 8])`
line, plus the full preceding globals block, compiles and runs
correctly in isolation (tried with `evaluate` reachable too, both
with and without `move`/`pseudoLegalMoves` in the call graph; the
latter surfaces a *different*, unrelated true-negative in
`nonpawnAttacks`/`rowAttack` — real CPython never hits `rowAttack`'s
implicit-`None`-return path for the starting position because a
guard short-circuits first, but pyc's whole-program static analysis
sees the path is *reachable* and needs `max()` to order a
`bool|None` union, which it can't; this is a distinct, separate,
correctly-flagged gap, not a bug in `squares`). The `squares` NOTYPE
only manifests with (something close to) the *full* program's call
graph and complexity — the same "no violation forces the needed
separation at this scale" shape [063](063-no-type-bucket-triage.md)
documented for dijkstra2.

## Two distinct gaps here, not one

1. **The real fix (deep, deferred):** setter-class-keyed product
   routing for `clone_methods_per_cs` prototype-init setters, per
   issue 063's own stated follow-up. Not attempted here — it's
   FA-splitter-internals work with a history (063, 045, 033) of
   multi-day investigations and corpus-regression risk (dijkstra2
   bounced FAIL on an earlier, related attempt).
2. **A codegen-robustness gap, independently worth fixing:** when a
   `P_prim_setter`'s value operand never got a resolving write, `cg.cc`
   currently emits a plain field-store reading an **uninitialized C
   local** — undefined behavior, not the controlled
   `assert(!"runtime error: ...")` trap convention issue 056/063
   established for every other unsalvageable-value site (compare the
   `P_prim_period`/getter case immediately above `P_prim_setter` in
   `cg.cc`, which already has exactly this `fruntime_errors ?
   assert : fail()` guard for an unresolved *field*). The setter case
   has no equivalent guard for an unresolved *value*. This is small,
   isolated, and matches a fix pattern already proven safe three
   times in this codebase — but distinguishing "genuinely
   unresolved" from "a legitimately-live temp written earlier in a
   sibling branch cg hasn't looked at yet" needs care to avoid
   false-positive traps on working programs; not attempted here
   without more study of `cg.cc`'s Var-liveness machinery and a full
   corpus/suite regression pass.

## Verification plan (for whoever picks this up)

1. `chess.py` compiles with zero warnings and
   `./shedskin_examples/chess/chess` runs to completion instead of
   aborting.
2. A regression test exercising a `clone_methods_per_cs` class
   (`range`) used exactly once, at module (global) scope, through a
   filtered list comprehension — the specific shape that triggers
   this — separate from any `for x in range(...)` direct-loop
   idiom (which unrolls/const-folds away and never hits this path).
3. Full suite both backends must stay green; specifically re-check
   dijkstra2 and fysphun (the two examples prior setter/splitter work
   has regressed or stress-tested) per issue 063's own checklist.
4. If pursuing gap 2 (UB → trap) independently of gap 1: verify it
   does not change output for any currently-passing corpus member
   (it should only ever fire on programs that already crash, turning
   UB into a defined, loud trap — `./shedskin_sweep.sh` before/after
   should show identical COMPILED_C/COMPILED_C_WARN/FAIL buckets).

## What this unblocks

- `shedskin_examples/chess/chess.py` (a shedskin corpus benchmark)
  running to completion.
- Any program using a `clone_methods_per_cs` class (currently just
  `range`) exactly once at global/module scope through a non-
  unrollable iteration shape (filtered/nested comprehension) — likely
  affects other corpus members that iterate `range` inside a
  comprehension with a filter clause at global scope, not yet
  surveyed.
- Turning this whole crash class from a silent-garbage / UB hazard
  into a loud, debuggable trap (gap 2 alone), independent of whether
  gap 1 (the real FA fix) ever lands.

## Related

- [063-no-type-bucket-triage.md](063-no-type-bucket-triage.md) —
  the setter-stage/mark-stage precision gap this is a new instance
  of ("Update 2026-07-23" section specifically).
- [045-receiver-cs-method-cloning.md](closed/045-receiver-cs-method-cloning.md) —
  introduced `clone_methods_per_cs` and the per-CS method-contour
  machinery this setter is trying to install into.
- [033-splitter-non-idempotent-divergence.md](033-splitter-non-idempotent-divergence.md) —
  the stability rules any setter-stage routing fix must respect.
- [issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md) —
  corpus coverage tracker; `chess` entry updated alongside this
  filing.
